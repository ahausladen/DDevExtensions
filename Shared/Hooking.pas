{******************************************************************************}
{*                                                                            *}
{* (C) 2006-2013 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit Hooking;

// Don't ever activate them here. This unit depends on pointer arithmetic with Integer overflows.
{$RANGECHECKS OFF}

interface

{$IF CompilerVersion >= 23.0} // XE2+
uses
  WinApi.Windows, System.SysUtils;
{$ELSE}
uses
  Windows, SysUtils;
{$IFEND}

type
  {$IF not declared(SIZE_T)}
  SIZE_T = DWORD;
  {$IFEND}

  TXRedirCode = packed record
    Jump: Byte;
    Offset: Integer;
  end;

  TRedirectCode = packed record
    RealProc: Pointer;
    Count: Integer;
    case Byte of
      0: (Code: TXRedirCode);
      1: (Code2: Int64);
  end;

procedure CodeRedirect(Proc: Pointer; NewProc: Pointer; out Data: TRedirectCode); overload;
function CodeRedirect(Proc: Pointer; NewProc: Pointer): TRedirectCode; overload;
procedure CodeRestore(var Data: TRedirectCode);

procedure RehookFunction(NewProc: Pointer; var Data: TRedirectCode);
procedure HookFunction(ModuleHandle: THandle; const SymbolName: string; NewProc: Pointer; out Data: TRedirectCode); overload;
procedure HookFunction(const ModuleName, SymbolName: string; NewProc: Pointer; out Data: TRedirectCode); overload;
procedure UnhookFunction(var Data: TRedirectCode);
function InjectCode(DestProc, SourceProc: Pointer; Size: Cardinal): Boolean;

function GetActualAddr(Proc: Pointer): Pointer; //inline;
function ReplaceDllImport(Base: Pointer; const ModuleName: string; FromProc, ToProc: Pointer): Boolean;
function ReplaceVmtField(AClass: TClass; OldProc, NewProc: Pointer): Boolean;
function ReplaceDmtField(AClass: TClass; Index: Integer; NewProc: Pointer): Pointer;
function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;

function GetVirtualMethodCount(AClass: TClass): Integer;
function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);

// FindMethodPtr doesn't call GetActualAddr
function FindMethodPtr(Start: Pointer; const Bytes: array of SmallInt; MaxOffset: Integer = 0): Pointer; overload;
function FindMethodPtr(Start: THandle; const Bytes: array of SmallInt; MaxOffset: Integer = 0): Pointer; overload;
procedure EnableFindMethodPtrDllVirtualQueryCache(Enable: Boolean);

type
  POffsetTable = ^TOffsetTable;
  TOffsetTable = record
  public
    Offsets: array of PInteger;
    procedure Add(P: PInteger);
  end;

function GetStartCodeSize(CodePtr: Pointer; RequiredSize: Integer; OffsetTable: POffsetTable = nil): Integer;
function CreateOrgCallMethodPtr(Proc: Pointer): Pointer;
function RedirectOrgCall(OrgProc, NewProc: Pointer): Pointer;
procedure RestoreOrgCall(OrgProc, OrgCall: Pointer);
procedure ReRedirectOrgCall(OrgProc, NewProc, ExistingOrgCall: Pointer);
procedure RedirectOrg(OrgProc, NewProc: Pointer);

// ReplaceRelCallOffset replaces the offset in a "call/jmp rel32" to point to NewFunction. It
// returns the absolute address of the original function. CallJmpPtr must point to the $E8/$E9
function ReplaceRelCallOffset(CallJmpPtr: PByte; NewFunction: Pointer): Pointer;
// GetCallTargetAddress returns the absolute address of a rel call at CallJmpPtr
function GetCallTargetAddress(CallJmpPtr: PByte): Pointer;
// ReplaceOpCodeByRelCall replaces the 5 bytes at FiveByteStart with a rel call to CallFunction
function ReplaceOpCodeByRelCall(FiveByteStart: PByte; CallFunction: Pointer): Boolean;
// ReplaceOpCodeByRelJump replaces the 5 bytes at FiveByteStart with a rel jump to JumpTargetz
function ReplaceOpCodeByRelJump(FiveByteStart: PByte; JumpTarget: Pointer; FillWithNop: Boolean = False): Boolean;
// ReplaceInstructionByRelCall replaces the instruction at InstructionStart by a rel call to
// CallFunction and fills the remaining bytes with NOP
function ReplaceInstructionByRelCall(IntructionStart: PByte; CallFunction: Pointer): Boolean;

type
  TWinApiHookInfo = record
    WinApiProc: PByte;
    case Mode: Byte of
      1, 2: (Data: array[0..4+2] of Byte);
      3, 100: (OrgCall: Pointer);
  end;

function HookWinApiProc(AWinApiProc, ANewProc: Pointer; var AHookInfo: TWinApiHookInfo; AResolveImportAddr: Boolean = True): Pointer; overload;
function HookWinApiProc(AWinApiProc, ANewProc: Pointer; var AOrgCallProc; var AHookInfo: TWinApiHookInfo; AResolveImportAddr: Boolean = True): Boolean; overload;
function UnhookWinApiProc(const AHookInfo: TWinApiHookInfo): Boolean;

implementation

// ------- BEGIN Memory manipulation functions ----------

function WriteProtectedMemory(BaseAddress, Buffer: Pointer; Size: SIZE_T;
  var WrittenBytes: SIZE_T): Boolean;
begin
  Result := WriteProcessMemory(GetCurrentProcess, BaseAddress, Buffer, Size, WrittenBytes);
end;

function ReadProtectedMemory(BaseAddress, Buffer: Pointer; Size: SIZE_T;
  var ReadBytes: SIZE_T): Boolean;
begin
  Result := ReadProcessMemory(GetCurrentProcess, BaseAddress, Buffer, Size, ReadBytes);
end;

type
  {$IFDEF CPUX64}
  PAbsoluteIndirectJmp64 = ^TAbsoluteIndirectJmp64;
  TAbsoluteIndirectJmp64 = packed record
    OpCode: Word;   //$FF25(Jmp, FF /4)
    Rel: Integer;
  end;
  {$ELSE}
  {PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    PUSH: Byte;    // PUSH instruction opcode ($68)
    Addr: Pointer; // The actual address of the DLL routine
    JMP: Byte;     // JMP instruction opcode ($E9)
    Rel: Integer;  // Relative displacement (a Kernel32 address)
  end;}

  PAbsoluteIndirectJmp32 = ^TAbsoluteIndirectJmp32;
  TAbsoluteIndirectJmp32 = packed record
    OpCode: Word;   //$FF25(Jmp, FF /4)
    Addr: ^Pointer;
  end;
  {$ENDIF CPUX64}

function GetActualAddr(Proc: Pointer): Pointer;
begin
  Result := Proc;
  if Result <> nil then
  begin
    {$IFDEF CPUX64}
    if (PAbsoluteIndirectJmp64(Result).OpCode = $25FF) then
      Result := PPointer(PByte(@PAbsoluteIndirectJmp64(Result).OpCode) + SizeOf(TAbsoluteIndirectJmp64) + PAbsoluteIndirectJmp64(Result).Rel)^;
    {$ELSE}
    {if (Win32Platform <> VER_PLATFORM_WIN32_NT) and
       (PWin9xDebugThunk(Result).PUSH = $68) and
       (PWin9xDebugThunk(Result).JMP = $E9) then
      Result := PWin9xDebugThunk(Result).Addr;}
    if (PAbsoluteIndirectJmp32(Result).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp32(Result).Addr^;
    {$ENDIF CPUX64}
  end;
end;

function PeMapImgNtHeaders(const BaseAddress: Pointer): PImageNtHeaders;
begin
  Result := nil;
  if IsBadReadPtr(BaseAddress, SizeOf(TImageDosHeader)) then
    Exit;
  if (PImageDosHeader(BaseAddress)^.e_magic <> IMAGE_DOS_SIGNATURE) or
    (PImageDosHeader(BaseAddress)^._lfanew = 0) then
    Exit;
  Result := PImageNtHeaders(DWORD(BaseAddress) + DWORD(PImageDosHeader(BaseAddress)^._lfanew));
  if IsBadReadPtr(Result, SizeOf(TImageNtHeaders)) or
    (Result^.Signature <> IMAGE_NT_SIGNATURE) then
      Result := nil
end;

type
  TIIDUnion = record
    case Integer of
      0: (Characteristics: DWORD);
      1: (OriginalFirstThunk: DWORD);
  end;

  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = record
    Union: TIIDUnion;
    TimeDateStamp: DWORD;
    ForwarderChain: DWORD;
    Name: DWORD;
    FirstThunk: DWORD;
  end;

  PImageThunkData32 = ^TImageThunkData32;
  TImageThunkData32 = record
    case Integer of
      0: (ForwarderString: DWORD);
      1: (Function_: DWORD);
      2: (Ordinal: DWORD);
      3: (AddressOfData: DWORD);
  end;

function ReplaceDllImport(Base: Pointer; const ModuleName: string; FromProc, ToProc: Pointer): Boolean;
var
  NtHeader: PImageNtHeaders;
  ImportDir: TImageDataDirectory;
  ImportDesc: PImageImportDescriptor;
  CurrName, RefName: PAnsiChar;
  ImportEntry: PImageThunkData32;
  LastProtect, Dummy: Cardinal;
  CurProcess: DWORD;
begin
  Result := False;
  NtHeader := PeMapImgNtHeaders(Base);
  if (NtHeader = nil) or (FromProc = nil) then
    Exit;
  ImportDir := NtHeader.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  if ImportDir.VirtualAddress = 0 then
    Exit;
  CurProcess := GetCurrentProcess;
  ImportDesc := PImageImportDescriptor(DWORD(Base) + ImportDir.VirtualAddress);
  RefName := PAnsiChar({$IFDEF UNICODE}UTF8Encode{$ENDIF}(ModuleName));
  while ImportDesc^.Name <> 0 do
  begin
    CurrName := PAnsiChar(Base) + ImportDesc^.Name;

    // if lstrcmpiA(CurrName, RefName) = 0 then
    {$WARNINGS OFF}
    if StrIComp(CurrName, RefName) = 0 then
    {$WARNINGS ON}
    begin
      ImportEntry := PImageThunkData32(PAnsiChar(Base) + ImportDesc^.FirstThunk);
      while ImportEntry^.Function_ <> 0 do
      begin
        if Pointer(ImportEntry^.Function_) = FromProc then
        begin
          if VirtualProtectEx(CurProcess, @ImportEntry^.Function_, SizeOf(ToProc), PAGE_READWRITE, @LastProtect) then
          begin
            ImportEntry^.Function_ := Cardinal(ToProc);

            // According to Platform SDK documentation, the last parameter
            // has to be (point to) a valid variable
            VirtualProtectEx(CurProcess, @ImportEntry^.Function_, SizeOf(ToProc), LastProtect, Dummy);
            Result := True;
          end;
        end;
        Inc(ImportEntry);
      end;
    end;
    Inc(ImportDesc);
  end;
end;

procedure CodeRedirect(Proc: Pointer; NewProc: Pointer; out Data: TRedirectCode);
var
  OldProtect: Cardinal;
begin
  if Proc = nil then
  begin
    Data.RealProc := nil;
    Exit;
  end;
  if Data.Count = 0 then // do not overwrite an already backuped code
  begin
    Proc := GetActualAddr(Proc);
    if VirtualProtectEx(GetCurrentProcess, Proc, SizeOf(Data.Code) + 1, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      Data.RealProc := Proc;
      Data.Code2 := Int64(Proc^);
      TXRedirCode(Proc^).Jump := $E9;
      TXRedirCode(Proc^).Offset := PAnsiChar(NewProc) - PAnsiChar(Proc) - (SizeOf(Data.Code));
      VirtualProtectEx(GetCurrentProcess, Proc, SizeOf(Data.Code) + 1, OldProtect, @OldProtect);
      FlushInstructionCache(GetCurrentProcess, Proc, SizeOf(Data.Code) + 1);
    end;
  end;
  Inc(Data.Count);
end;

function CodeRedirect(Proc: Pointer; NewProc: Pointer): TRedirectCode;
begin
  Result.Count := 0;
  Result.RealProc := nil;
  CodeRedirect(Proc, NewProc, Result);
end;

procedure CodeRestore(var Data: TRedirectCode);
var
  n: SIZE_T;
begin
  if (Data.RealProc <> nil) and (Data.Count = 1) then
    WriteProtectedMemory(Data.RealProc, @Data.Code, SizeOf(Data.Code), n);
  Dec(Data.Count);
end;

procedure RehookFunction(NewProc: Pointer; var Data: TRedirectCode);
begin
  CodeRedirect(Data.RealProc, NewProc, Data);
end;

procedure HookFunction(ModuleHandle: THandle; const SymbolName: string; NewProc: Pointer; out Data: TRedirectCode);
begin
  CodeRedirect(GetProcAddress(ModuleHandle, PAnsiChar(AnsiString(SymbolName))), NewProc, Data);
end;

procedure HookFunction(const ModuleName, SymbolName: string; NewProc: Pointer; out Data: TRedirectCode);
begin
  CodeRedirect(GetProcAddress(GetModuleHandle(PChar(ModuleName)), PAnsiChar(AnsiString(SymbolName))), NewProc, Data);
end;

procedure UnhookFunction(var Data: TRedirectCode);
begin
  CodeRestore(Data);
end;

function GetVirtualMethodCount(AClass: TClass): Integer;
type
  PPAnsiChar = ^PAnsiChar;
var
  BeginVMT: PAnsiChar;
  EndVMT: PAnsiChar;
  TablePointer: PAnsiChar;
  I: Integer;
begin
  BeginVMT := PAnsiChar(AClass);

  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  // After the last virtual method there is one of these entries.

  EndVMT := PPAnsiChar(PAnsiChar(AClass) + vmtClassName)^;
  // Set iterator to first item behind VMT table pointer
  I := vmtSelfPtr + SizeOf(Pointer);
  repeat
    TablePointer := PPAnsiChar(PAnsiChar(AClass) + I)^;
    if (TablePointer <> nil) and (TablePointer >= BeginVMT) and
       (TablePointer < EndVMT) then
      EndVMT := PAnsiChar(TablePointer);
    Inc(I, SizeOf(Pointer));
  until I >= vmtClassName;

  Result := (EndVMT - BeginVMT) div SizeOf(Pointer);
end;

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
begin
  Result := PPointer(PAnsiChar(AClass) + Index * SizeOf(Pointer))^;
end;

procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);
var
  WrittenBytes: SIZE_T;
  PatchAddress: PPointer;
begin
  PatchAddress := Pointer(PAnsiChar(AClass) + Index * SizeOf(Pointer));
  WriteProtectedMemory(PatchAddress, @Method, SizeOf(Method), WrittenBytes);
end;

function ReplaceVmtField(AClass: TClass; OldProc, NewProc: Pointer): Boolean;
type
  PVmt = ^TVmt;
  TVmt = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
var
  I: Integer;
  Vmt: PVmt;
  n: SIZE_T;
  P: Pointer;
begin
  OldProc := GetActualAddr(OldProc);
  NewProc := GetActualAddr(NewProc);

  I := vmtSelfPtr div SizeOf(Pointer);
  Vmt := Pointer(AClass);
  while (I < 0) or (Vmt[I] <> nil) do
  begin
    P := Vmt[I];
    if (P <> OldProc) and (DWORD_PTR(P) > $10000) and not IsBadReadPtr(P, 6) then
      P := GetActualAddr(P);
    if P = OldProc then
    begin
      Result := WriteProtectedMemory(@Vmt[I], @NewProc, SizeOf(NewProc), n);
      Exit;
    end;
    Inc(I);
  end;
  Result := False;
end;

function InjectCode(DestProc, SourceProc: Pointer; Size: Cardinal): Boolean;
var
  n: SIZE_T;
begin
  DestProc := GetActualAddr(DestProc);
  Result := (DestProc <> nil) and (SourceProc <> nil) and
            WriteProtectedMemory(DestProc, SourceProc, Size, n) and (n = Size);
end;

function GetDmtEntryAddress(AClass: TClass; Index: Integer): Pointer;
{$IFDEF CPUX64}
type
  TDynaMethodTable = record
    Count: Word;
    Selectors: array[0..9999999] of SmallInt;
    {Addrs: array[0..0] of Pointer;}
  end;
  PDynaMethodTable = ^TDynaMethodTable;
var
  DynaTab: PDynaMethodTable;
  Parent: Pointer;
  Addrs: PPointer;
  I: Cardinal;
begin
  while True do
  begin
    DynaTab := PPointer(PByte(AClass) + vmtDynamicTable)^;
    if DynaTab <> nil then
    begin
      for I := 0 to DynaTab.Count - 1 do
        if DynaTab.Selectors[I] = Index then
        begin
          Addrs := PPointer(PByte(@DynaTab.Selectors) + DynaTab.Count * SizeOf(DynaTab.Selectors[0]));
          Result := PPointer(PByte(Addrs) + I * SizeOf(Pointer))^;
          Exit;
        end;
    end;
    Parent := PPointer(PByte(AClass) + vmtParent)^;
    if Parent = nil then Break;
    AClass := PPointer(Parent)^;
  end;
  Result := nil;
end;
{$ELSE}
asm
        { ->    EAX     vmt of class            }
        {       DX      dynamic method index    }

        PUSH    EDI
        XCHG    EAX, EDX
        JMP     @@HaveVMT
@@OuterLoop:
        MOV     EDX, [EDX]
@@HaveVMT:
        MOV     EDI, [EDX].vmtDynamicTable
        TEST    EDI, EDI
        JE      @@Parent
        MOVZX   ECX, WORD PTR [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@Found
        POP     ECX
@@Parent:
        MOV     EDX,[EDX].vmtParent
        TEST    EDX,EDX
        JNE     @@OuterLoop
        MOV     EAX, 0
        JMP     @@Exit
@@Found:
        POP     EAX
        ADD     EAX,EAX
        SUB     EAX,ECX
        LEA     EAX,[EDI+EAX*2-4]
@@Exit:
        POP     EDI
end;
{$ENDIF CPUX64}

function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;
var
  P: ^Pointer;
begin
  P := GetDmtEntryAddress(AClass, Index);
  Result := nil;
  if P <> nil then
    Result := P^;
end;

function ReplaceDmtField(AClass: TClass; Index: Integer; NewProc: Pointer): Pointer;
var
  P: ^Pointer;
  n: SIZE_T;
begin
  P := GetDmtEntryAddress(AClass, Index);
  if P <> nil then
  begin
    Result := P^;
    if not WriteProtectedMemory(P, @NewProc, SizeOf(Pointer), n) then
      Result := nil;
  end
  else
    Result := nil;
end;

function FindOtherBytesPtr(P: PByte; Bytes: PSmallInt; BytesLen: Integer): Boolean;
var
  Index: Integer;
  C: SmallInt;
begin
  Result := False;
  for Index := 1 to BytesLen - 1 do
  begin
    C := Bytes^;
    if (C <> -1) and (Byte(C) <> P^) then
      Exit;
    Inc(P);
    Inc(Bytes);
  end;
  Result := True;
end;

function FindPtr(P: PByte; RegionSize: Integer; Bytes: PSmallInt; BytesLen: Integer): Pointer;
var
  FirstByte: Byte;
begin
  FirstByte := Byte(Bytes^);
  Inc(Bytes);
  while True do
  begin
    // fast forward
    while (RegionSize > 0) and (P^ <> FirstByte) do
    begin
      Dec(RegionSize);
      Inc(P);
    end;
    if RegionSize = 0 then
      Break;

    if FindOtherBytesPtr(P + 1, Bytes, BytesLen) then
    begin
      Result := P;
      Exit;
    end;
    Inc(P);
    Dec(RegionSize);
  end;
  Result := nil;
end;

var
  AllowCachedDllVirtualQuery: Boolean = False;
  LastDllVirtualQueryInfo: TMemoryBasicInformation;

procedure EnableFindMethodPtrDllVirtualQueryCache(Enable: Boolean);
begin
  AllowCachedDllVirtualQuery := Enable;
end;

function FindMethodPtr(Start: Pointer; const Bytes: array of SmallInt; MaxOffset: Integer = 0): Pointer;
var
  P, Address: PAnsiChar;
  MemInfo: TMemoryBasicInformation;
  StartOffset: Integer;
  BytesLen: Integer;
  AllocBase: Pointer;
begin
  // Old code uses $10000 as "very far into the code segment" what it isn't. It is only 64K.
  // Because I don't know in which projects that is used, it will be extended to 16M.
  if MaxOffset = $10000 then
    MaxOffset := $1000000;

  Result := nil;
  BytesLen := Length(Bytes);
  if (Start <> nil) and (BytesLen >= 4) then
  begin
    StartOffset := 0;
    while (StartOffset < BytesLen) and (Bytes[StartOffset] = -1) do
      Inc(StartOffset);
    if StartOffset = BytesLen then
      Exit;

    try
      Address := PAnsiChar(Start);

      if AllowCachedDllVirtualQuery then
      begin
        if (LastDllVirtualQueryInfo.BaseAddress <= Address) or (Address >= PAnsiChar(LastDllVirtualQueryInfo.BaseAddress) + LastDllVirtualQueryInfo.RegionSize) then
        begin
          if not VirtualQuery(Address, MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo) then
            Exit;
          if MemInfo.Protect and $000000F0 = 0 then // not PAGE_EXECUTE | ...
          begin
            Inc(Address, MemInfo.RegionSize);
            if VirtualQuery(Address, MemInfo, SizeOf(MemInfo)) <> SizeOf(MemInfo) then
              Exit;
            //Move(MemInfo, LastDllVirtualQueryInfo, SizeOf(LastDllVirtualQueryInfo));
            LastDllVirtualQueryInfo.AllocationBase := MemInfo.AllocationBase;
            LastDllVirtualQueryInfo.RegionSize := MemInfo.RegionSize;
            LastDllVirtualQueryInfo.Protect := MemInfo.Protect;
          end;
        end
        else
        begin
          //Move(LastDllVirtualQueryInfo, MemInfo, SizeOf(MemInfo));
          MemInfo.AllocationBase := LastDllVirtualQueryInfo.AllocationBase;
          MemInfo.RegionSize := LastDllVirtualQueryInfo.RegionSize;
          MemInfo.Protect := LastDllVirtualQueryInfo.Protect;
        end;
      end
      else
      if not VirtualQuery(Address, MemInfo, SizeOf(MemInfo)) = SizeOf(MemInfo) then
        Exit;

      AllocBase := MemInfo.AllocationBase;
      repeat
        P := Address;
        if MemInfo.Protect and $000000F0 <> 0 then // PAGE_EXECUTE | ...
        begin
          if (MaxOffset > 0) and (PAnsiChar(Start) + MaxOffset < P + MemInfo.RegionSize) then
            MemInfo.RegionSize := PAnsiChar(Start) + MaxOffset - P;

          Result := FindPtr(PByte(P), MemInfo.RegionSize, @Bytes[StartOffset], BytesLen - StartOffset);
          if Result <> nil then
          begin
            Result := PAnsiChar(Result) - StartOffset;
            Exit;
          end;
        end;
        Inc(Address, MemInfo.RegionSize);
        { Cancel if we exceed the allowed offset }
        if (MaxOffset > 0) and (PAnsiChar(Address) >= PAnsiChar(Start) + MaxOffset) then
          Exit;
      until (VirtualQuery(Address, MemInfo, SizeOf(MemInfo)) <> SizeOf(MemInfo)) or
            (MemInfo.AllocationBase <> AllocBase);
    except
      on E: EAccessViolation do
        ;
      on E: EPrivilege do
        ;
    end;
  end;
end;

{function FindMethodPtr(Start: Pointer; const Bytes: array of SmallInt; MaxOffset: Integer = 0): Pointer;
begin
  Result := InternFindMethodPtr(Start, Bytes, MaxOffset);
  if Result = nil then
    Write;
end;}

function FindMethodPtr(Start: THandle; const Bytes: array of SmallInt; MaxOffset: Integer): Pointer;
begin
  Result := FindMethodPtr(Pointer(Start), Bytes, MaxOffset);
end;

{ TOffsetTable }

procedure TOffsetTable.Add(P: PInteger);
var
  Len: Integer;
begin
  Len := Length(Offsets);
  SetLength(Offsets, Len + 1);
  Offsets[Len] := P;
end;

{$IFDEF DEBUG}
  {$IF not declared(IsDebuggerPresent)}
function IsDebuggerPresent: BOOL; stdcall;
  external kernel32 name 'IsDebuggerPresent';
  {$IFEND}
{$ENDIF DEBUG}

function ModRmSize(P: PByte): Integer;
var
  ModRm: Byte;
begin
  ModRm := P[0];
  if ModRm >= $C0 then
    Result := 1 // ModRm
  else if ModRm and $07 = $04 then
  begin
    if P[1] and $07 = $05 then // SIB
    begin
      if ModRm and $C0 = $40 then
        Result := 3 // 1 + 1 + 1 // ModRm + SIB + Byte displacement
      else
        Result := 6; // 1 + 1 + 4; // ModRm + SIB + DWord displacement
    end
    else
      Result := 1;
  end
  else
  begin
    case ModRm and $C0 of
      $00:
        if ModRm and $07 = $05 then
          Result := 5 // 1 + 4;  // ModRm + DWord displacement
        else
          Result := 1; // ModRM
      $80:
        Result := 5; // 1 + 4; // ModRm + DWord offset
    else
      Result := 2; // 1 + 1; // ModRm + Byte offset
    end;
  end;
end;

function GetStartCodeSize(CodePtr: Pointer; RequiredSize: Integer; OffsetTable: POffsetTable): Integer;
// TODO: "Jcc rel": convert to Jcc dword-rel and adjust offsets
var
  RetFound: Boolean;
  {$IF CompilerVersion >= 20.0}
  Code: PByte;
  {$ELSE}
  Code: PByteArray;
  {$IFEND}

  procedure Failed;
  var
    {$IF CompilerVersion >= 20.0}
    P: PByte;
    {$ELSE}
    P: PByteArray;
    {$IFEND}
  begin
    if RetFound then
      raise Exception.Create('Function is too small')
    else
    begin
      P := Code;
      {$IFDEF DEBUG}
      if IsDebuggerPresent then
      begin
        {$IFDEF CPUX64}
        DebugBreak;
        TProcedure(P)();
        {$ELSE}
        asm
          int 3
          call [P]
        end;
      end;
        {$ENDIF CPUX64}
      {$ENDIF DEBUG}
      {$IF CompilerVersion >= 20.0}
      raise Exception.CreateFmt('Cannot handle opcode %.2x %.2x %.2x %.2x', [P[0], P[1], P[2], P[3]]);
      {$ELSE}
      raise Exception.CreateFmt('Cannot handle opcode %.2x %.2x %.2x %.2x', [Byte(PAnsiChar(P)[0]), Byte(PAnsiChar(P)[1]), Byte(PAnsiChar(P)[2]), Byte(PAnsiChar(P)[3])]);
      {$IFEND}
    end;
  end;

  procedure Int3Found;
  begin
    raise Exception.Create('Breakpoint found. Remove the breakpoint before hooking the function');
  end;

var
  Size: Integer;
begin
  Code := CodePtr;
  Result := 0;
  RetFound := False;

  while Result < RequiredSize do
  begin
    if RetFound then
      Failed;
    Size := 0;
    case Code[0] of
      $03:
        case Code[1] of
          $FF: Size := 2; // add edi,edi
        end;

      $0F:
        case Code[1] of
          $B6: Size := 2 + ModRmSize(@Code[2]); // movzx r32, r/m8
          $B7: Size := 2 + ModRmSize(@Code[2]); // movzx r32, r/m16
        end;

      $04:
        case Code[1] of
          $FD: Size := 1 + 1; // add al, ByteConst
        end;

      $24: Size := 1 + 1; // and al, ByteCOnst

      $29:
        case Code[1] of
          $C2: Size := 2; // sub edx,eax
        end;

      $2C: Size := 1 + 1; // sub al, ByteConst

      $31:
        case Code[1] of
          $C0: Size := 2; // xor eax, eax
        end;

      $33:
        case Code[1] of
          $C0: Size := 2; // xor eax, eax
          $C9: Size := 2; // xor ecx, ecx
          $D2: Size := 2; // xor edx, edx
          $DB: Size := 2; // xor ebx, ebx
        end;

      $3B:
        case Code[1] of
          $05: Size := 2 + 4; // cmp eax, [DwordConst]
        end;

      $50: Size := 1; // push eax
      $51: Size := 1; // push ecx
      $52: Size := 1; // push edx
      $53: Size := 1; // push ebx
      $54: Size := 1; // push esp
      $55: Size := 1; // push ebp
      $56: Size := 1; // push esi
      $57: Size := 1; // push edi

      $5D: Size := 1; // pop ebp
      $64:
        case Code[1] of
          $FF:
            case Code[2] of
              $15: Size := 2 + 4; // call dword ptr fs:[DwordConst]
            end;
        end;
      $68: Size := 1 + 4; // push DwordConst
      $6A: Size := 1 + 1; // push ByteConst

      $74: if OffsetTable = nil then Size := 1 + 1; // jz ByteConst  // fail if we want that code as OrgCall code
      $75: if OffsetTable = nil then Size := 1 + 1; // jnz ByteConst  // fail if we want that code as OrgCall code

      $80:
        case Code[1] of
          $38: Size := 2 + 1;     // cmp byte ptr [eax], ByteConst
          $3D: Size := 2 + 4 + 1; // cmp byte ptr [DWordConst], ByteConst
          $4B: Size := 2 + 1 + 1; // or byte ptr [ebx+ByteConst], ByteConst
          $7B: Size := 2 + 1 + 1; // cmp byte ptr [ebx+ByteConst], ByteConst
          $B8: Size := 2 + 4 + 1; // cmp byte ptr [eax+DWordConst], ByteConst
          $E1: Size := 2 + 1;     // and cl, ByteConst
          $E2: Size := 2 + 1;     // and dl, ByteConst
          $E9: Size := 2 + 1;     // sub cl, ByteConst
          $EA: Size := 2 + 1;     // sub dl, ByteConst
        end;

      $81:
        case Code[1] of
          $C4: Size := 2 + 4; // add esp, DWordConst
          $C6: Size := 2 + 4; // add esi, DWordConst
          $C7: Size := 2 + 4; // add edi, DWordConst
          $FE: Size := 2 + 4; // cmp esi, DWordConst
        end;

      $83:
        case Code[1] of
          $04:
            case Code[2] of
              $24: Size := 3 + 1; // add dword ptr [esp], ByteConst
            end;
          $3C:
            case Code[2] of
              $24: Size := 3 + 1; // cmp dword ptr [esp], ByteConst
            end;
          $3D: Size := 2 + 4 + 1; // cmp dword ptr [DWordConst], ByteConst
          $7C:
            case Code[2] of
              $24: Size := 3 + 1 + 1; // cmp dword ptr [esp+ByteConst], ByteConst
            end;
          $7D: Size := 2 + 1 + 1; // cmp dword ptr [ebp+ByteConst], ByteConst
          $BB: Size := 2 + 4 + 1; // cmp dword ptr [ebx+DWordConst], ByteConst
          $C4: Size := 2 + 1;     // add esp, ByteConst
          $CF: Size := 2 + 1;     // or edi, ByteConst
          $E4: Size := 2 + 1;     // and esp, ByteConst
          $EC: Size := 2 + 1;     // sub esp, ByteConst
        end;

      $85:
        case Code[1] of
          $35: Size := 2 + 4; // test [DWordConst], esi
          $3D: Size := 2 + 4; // test [DWordConst], edi
          $D2: Size := 2; // test edx, edx
          $C0: Size := 2; // test eax, eax
          $C9: Size := 2; // test ecx, ecx
          $DB: Size := 2; // test ebx, ebx
        end;

      $87:
        case Code[1] of
          $CA: Size := 2; // xchg edx, ecx
        end;

      $88:
        case Code[1] of
          $04:
            case Code[2] of
              $24: // mov [esp], al
            end;
          $0C:
            case Code[2] of
              $24: // mov [esp], cl
            end;
        end;


      $89:
        case Code[1] of
          $04:
            case Code[2] of
              $24: Size := 3; // mov [esp], eax
            end;
          $0C:
            case Code[2] of
              $24: Size := 3; // mov [esp], ecx
            end;
          $50: Size := 2 + 1; // mov [eax+ByteConst], edx
          $5A: Size := 2 + 1; // mov [edx+ByteConst], ebx
          $C3: Size := 2; // mov ebx, eax
          $C6: Size := 2; // mov esi, eax
          $D6: Size := 2; // mov esi, edx
          $D7: Size := 2; // mov edi, edx
        end;

      $8A:
        case Code[1] of
          $46: Size := 2 + 1; // mov al, [esi+ByteConst]
          $47: Size := 2 + 1; // mov al, [edi+ByteConst]
          $4A: Size := 2 + 1; // mov cl, [edx+ByteConst]
          $4D: Size := 2 + 1; // mov cl, [ebp+ByteConst]
          $4E: Size := 2 + 1; // mov cl, [esi+ByteConst]
          $4F: Size := 2 + 1; // mov cl, [edi+ByteConst]
          $50: Size := 2 + 1; // mov dl, [eax+ByteConst]
          $53: Size := 2 + 1; // mov dl, [ebx+ByteConst]
          $55: Size := 2 + 1; // mov dl, [ebp+ByteConst]
          $56: Size := 2 + 1; // mov dl, [esi+ByteConst]
          $57: Size := 2 + 1; // mov dl, [edi+ByteConst]
          $8A: Size := 2 + 4; // mov cl, [edx+DwordConst]
        end;

      $8B:
        case Code[1] of
          $00: Size := 2; // mov eax, [eax]
          $08: Size := 2; // mov ecx, [eax]
          $0A: Size := 2; // mov ecx, [edx]
          $0D: Size := 2 + 4; // mov ecx, [DwordConst]
          $10: Size := 2;     // mov edx, [eax]
          $15: Size := 2 + 4; // mov edx, [DwordConst]
          $1D: Size := 2 + 4; // mov ebx, [DwordConst]

          $40: Size := 2 + 1; // mov eax, [eax+ByteConst]
          $43: Size := 2 + 1; // mov eax, [ebx+ByteConst]
          $44:
            case Code[2] of
              $24: Size := 3 + 1; // mov eax,[esp+ByteConst]
            end;
          $45: Size := 2 + 1; // mov eax, [ebp+ByteConst]

          $48: Size := 2 + 1; // mov ecx, [eax+ByteConst]
          $4D: Size := 2 + 1; // mov ecx, [ebp+ByteConst]

          $50: Size := 2 + 1; // mov edx, [eax+ByteConst]
          $52: Size := 2 + 1; // mov edx, [edx+ByteConst]
          $53: Size := 2 + 1; // mov edx, [ebx+ByteConst]
          $55: Size := 2 + 1; // mov edx, [ebp+ByteConst]
          $57: Size := 2 + 1; // mov edx, [edi+ByteConst]
          $75: Size := 2 + 1; // mov esi, [ebp+ByteConst]

          $80: Size := 2 + 4; // mov eax, [eax+DwordConst]
          $86: Size := 2 + 4; // mov eax, [esi+DwordConst]

          $B3: Size := 2 + 4; // mov esi, [ebx+DwordConst]
          $B5: Size := 2 + 4; // mov esi, [ebp+DwordConst]
          $C3: Size := 2; // mov eax, ebx
          $C5: Size := 2; // mov eax, ebp
          $C6: Size := 2; // mov eax, esi
          $C7: Size := 2; // mov eax, edi
          $C8: Size := 2; // mov ecx, eax
          $CB: Size := 2; // mov ecx, ebx

          $D0: Size := 2; // mov edx, eax
          $D3: Size := 2; // mov edx, ebx
          $D4: Size := 2; // mov edx, esp
          $D6: Size := 2; // mov edx, esi
          $D8: Size := 2; // mov ebx, eax
          $D9: Size := 2; // mov ebx, ecx
          $DA: Size := 2; // mov ebx, edx

          $E8: Size := 2; // mov ebp, eax
          $E9: Size := 2; // mov ebp, ecx
          $EA: Size := 2; // mov ebp, edx
          $EC: Size := 2; // mov ebp, esp

          $F0: Size := 2; // mov esi, eax
          $F1: Size := 2; // mov esi, ecx
          $F2: Size := 2; // mov esi, edx

          $F4: Size := 2; // mov esi, esp
          $F8: Size := 2; // mov edi, eax
          $F9: Size := 2; // mov edi, ecx
          $FA: Size := 2; // mov edi, edx
          $FC: Size := 2; // mov edi, esp
          $FF: Size := 2; // mov edi, edi
        end;

      $8D:
        case Code[1] of
          $44:
            case Code[2] of
              $24: Size := 3 + 1; // lea eax, [esp+ByteConst]
            end;
          $4C:
            case Code[2] of
              $24: Size := 3 + 1; // lea ecx, [esp+ByteConst]
            end;
          $54:
            case Code[2] of
              $24: Size := 3 + 1 // lea edx, [esp+ByteConst]
            end;
          $7D: Size := 2 + 1; // lea edi, [ebp+ByteConst]
          $88: Size := 2 + 4; // lea ecx, [eax+DwordConst]
          $46: Size := 2 + 1; // lea eax, [esi+ByteConst]
        end;

      $90: Size := 1; // nop
      $91: Size := 1; // xchg eax, ecx

      $A1: Size := 1 + 4; // mov eax, [DwordConst]
      $A3: Size := 1 + 4; // mov [DwordConst], eax

      $B0: Size := 1 + 1; // mov al, ByteConst
      $B1: Size := 1 + 1; // mov cl, ByteConst

      $B8: Size := 1 + 4; // mov eax, DwordConst
      $B9: Size := 1 + 4; // mov ecx, DwordConst
      $BA: Size := 1 + 4; // mov edx, DwordConst
      $BB: Size := 1 + 4; // mov ebx, DwordConst
      $BE: Size := 1 + 4; // mov esi, DwordConst
      $BF: Size := 1 + 4; // mov edi, DwordConst

      $C2: begin Size := 1 + 2; RetFound := True; end; // retn WordConst
      $C3: begin Size := 1; RetFound := True; end; // ret

      $C6:
        case Code[1] of
          $02: Size := 2 + 1; // mov byte ptr [edx], ByteConst
          $06: Size := 2 + 1; // mov byte ptr [esi], ByteConst
          $04:
            case Code[2] of
              $24: Size := 3 + 1; // mov byte ptr [esp], ByteConst
            end;
          $80: Size := 2 + 4 + 1; // mov byte ptr [eax+DwordConst], ByteConst
        end;

      $CC: Int3Found;

      $EB: if OffsetTable = nil then Size := 1 + 1; // jmp ByteConst  // fail if we want that code as OrgCall code

      $E8, $E9: // call/jmp DwordConst
        begin
          Size := 1 + 4;
          if OffsetTable <> nil then
            OffsetTable.Add(PInteger(Code + 1));
        end;

      $F6:
        case Code[1] of
          $40: Size := 2 + 1 + 1; // test byte ptr [eax+ByteConst], ByteConst
          $46: Size := 2 + 1 + 1; // test byte ptr [esi+ByteConst], ByteConst
        end;

      $FF:
        case Code[1] of
          $25: Size := 2 + 4; // call dword ptr [DwordConst]
          $51: Size := 2 + 1; // call dword ptr [ecx+ByteConst]
          $52: Size := 2 + 1; // call dword ptr [edx+ByteConst]
          $56: Size := 2 + 1; // call dword ptr [esi+ByteConst]
          $75: Size := 2 + 1; // push dword ptr [ebp+ByteConst]
          $93: Size := 2 + 4; // call dword ptr [ebx+DwordConst]
          $96: Size := 2 + 4; // call dword ptr [esi+DwordConst]
        end;
    end;

    if Size = 0 then
      Failed;

    Inc(Result, Size);
    {$IF CompilerVersion >= 20.0}
    Inc(Code, Size);
    {$ELSE}
    Inc(PByte(Code), Size);
    {$IFEND}
  end;
end;

var
  OrgCallBlock: PByte;
  OrgCallBlockOffset: Integer;
  //OrgCallBlockCritSect: TRTLCriticalSection;

function CreateOrgCallMethodPtr(Proc: Pointer): Pointer;
const
  BlockSize = 4096;
var
  P: PByte;
  StartCodeSize, CodeSize, FullCodeSize: Integer;
  I: Integer;
  JmpRelOffset: Integer;
  OffsetTable: TOffsetTable;
  RelPos: Integer;
begin
  if Proc = nil then
    raise Exception.Create('CreateOrgCallMethodPtr called with nil');

  StartCodeSize := GetStartCodeSize(Proc, 5, @OffsetTable);
  if StartCodeSize = 0 then
    raise Exception.Create('Cannot create OrgCallMethod for the specified function');

  // space for "jmp rel"
  CodeSize := StartCodeSize;
  JmpRelOffset := StartCodeSize;
  Inc(CodeSize, 5);

  // alignment for the next OrgCallMethodPtr (filled with "INT 3")
  FullCodeSize := ((CodeSize + 1) + 3) and not $3;

  //EnterCriticalSection(OrgCallBlockCritSect);
  if (OrgCallBlock = nil) or (OrgCallBlockOffset + FullCodeSize > BlockSize) then
  begin
    // Append the next block, if possible.
    P := OrgCallBlock;
    if P <> nil then
    begin
      //VirtualProtect(P, BlockSize, PAGE_EXECUTE_READ, @Dummy);
      Inc(P, BlockSize);
    end;
    OrgCallBlock := VirtualAlloc(P, BlockSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    if OrgCallBlock = nil then
      OrgCallBlock := VirtualAlloc(nil, BlockSize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    if OrgCallBlock = nil then
      System.Error(reOutOfMemory);
    OrgCallBlockOffset := 0;
  end;

  {$IF CompilerVersion >= 20.0}
  P := OrgCallBlock + OrgCallBlockOffset;
  {$ELSE}
  P := PByte(PAnsiChar(OrgCallBlock) + OrgCallBlockOffset);
  {$IFEND}
  Inc(OrgCallBlockOffset, FullCodeSize);
  //LeaveCriticalSection(OrgCallBlockCritSect);

  // Adjust the relative address to the new code position
  Move(Proc^, P^, StartCodeSize);
  if OffsetTable.Offsets <> nil then // in 5 bytes only 1 call/jmp can be in it
  begin
    RelPos := PByte(OffsetTable.Offsets[0]) - PByte(Proc);
    PInteger(P + RelPos)^ := (PByte(Proc) - P) + OffsetTable.Offsets[0]^;
  end;

  P[JmpRelOffset] := $E9;
  PInteger(@P[JmpRelOffset + 1])^ := (PByte(Proc) + StartCodeSize) - (P + JmpRelOffset + 5);
  // Fill gab
  for I := CodeSize to FullCodeSize - 1 do
    Byte(PAnsiChar(P)[I]) := $CC; // int 3

  Result := P;
end;

procedure ReRedirectOrgCall(OrgProc, NewProc, ExistingOrgCall: Pointer);
begin
  RedirectOrg(OrgProc, NewProc);
end;

function RedirectOrgCall(OrgProc, NewProc: Pointer): Pointer;
var
  StartCodeSize: Integer;
  Buffer: array[0..63] of Byte;
  I: Integer;
  n: SIZE_T;
begin
  {$IFDEF CPUX64}
  raise Exception.Create('RedirectOrgCall is not supported in x64 mode, yet');
  {$ENDIF CPUX64}

  OrgProc := GetActualAddr(OrgProc);
  NewProc := GetActualAddr(NewProc);
  Result := CreateOrgCallMethodPtr(OrgProc);

  StartCodeSize := GetStartCodeSize(OrgProc, 5);
  Buffer[0] := $E9;
  {$IF CompilerVersion >= 20.0}
  PInteger(@Buffer[1])^ := PByte(NewProc) - (PByte(OrgProc) + 5);
  {$ELSE}
  PInteger(@Buffer[1])^ := PAnsiChar(NewProc) - (PAnsiChar(OrgProc) + 5);
  {$IFEND}
  for I := 5 to StartCodeSize - 1 do
    Buffer[I] := $90;
  if not WriteProcessMemory(GetCurrentProcess, OrgProc, @Buffer[0], StartCodeSize, n) then
    RaiseLastOSError;
end;

procedure RestoreOrgCall(OrgProc, OrgCall: Pointer);
var
  StartCodeSize: Integer;
  n: SIZE_T;
  Buffer: array[0..4 + 4] of Byte; // if Buffer[4] = $E8/$E9 we need another 4 bytes
  RelPos: Integer;
  OffsetTable: TOffsetTable;
begin
  if OrgCall = nil then
    Exit;

  OrgProc := GetActualAddr(OrgProc);

  StartCodeSize := GetStartCodeSize(OrgCall, 5, @OffsetTable);
  if OffsetTable.Offsets <> nil then // in 5 bytes only 1 call/jmp can be in it
  begin
    // Adjust the relative address to the original code position
    Move(OrgCall^, Buffer[0], StartCodeSize);
    RelPos := PByte(OffsetTable.Offsets[0]) - PByte(OrgCall);
    PInteger(@Buffer[RelPos])^ := OffsetTable.Offsets[0]^ - (PByte(OrgProc) - PByte(OrgCall));
    WriteProcessMemory(GetCurrentProcess, OrgProc, @Buffer, StartCodeSize, n);
  end
  else
    WriteProcessMemory(GetCurrentProcess, OrgProc, OrgCall, StartCodeSize, n);
end;

procedure RedirectOrg(OrgProc, NewProc: Pointer);
var
  StartCodeSize: Integer;
  Buffer: array[0..63] of Byte;
  I: Integer;
  n: SIZE_T;
begin
  OrgProc := GetActualAddr(OrgProc);
  NewProc := GetActualAddr(NewProc);

  StartCodeSize := GetStartCodeSize(OrgProc, 5);
  Buffer[0] := $E9;
  {$IF CompilerVersion >= 20.0}
  PInteger(@Buffer[1])^ := PByte(NewProc) - (PByte(OrgProc) + 5);
  {$ELSE}
  PInteger(@Buffer[1])^ := PAnsiChar(NewProc) - (PAnsiChar(OrgProc) + 5);
  {$IFEND}
  for I := 5 to StartCodeSize - 1 do
    Buffer[I] := $90;
  if not WriteProcessMemory(GetCurrentProcess, OrgProc, @Buffer[0], StartCodeSize, n) then
    RaiseLastOSError;
end;

function ReplaceRelCallOffset(CallJmpPtr: PByte; NewFunction: Pointer): Pointer;
var
  n: SIZE_T;
  Offset: Integer;
begin
  Result := GetCallTargetAddress(CallJmpPtr);
  if Result <> nil then
  begin
    if Result <> NewFunction then
    begin
      Offset := PByte(NewFunction) - (CallJmpPtr + 5);
      if not WriteProcessMemory(GetCurrentProcess, @CallJmpPtr[1], @Offset, SizeOf(Offset), n) then
        Result := nil;
    end;
  end
  else
    Result := nil;
end;

function GetCallTargetAddress(CallJmpPtr: PByte): Pointer;
begin
  if (CallJmpPtr <> nil) and (CallJmpPtr^ in [$E8, $E9]) then
    Result := Pointer(PByte(CallJmpPtr + 5) + PInteger(@CallJmpPtr[1])^)
  else
    Result := nil;
end;

function ReplaceOpCodeByRelCall(FiveByteStart: PByte; CallFunction: Pointer): Boolean;
var
  Buffer: array[0..4] of Byte;
  n: SIZE_T;
begin
  if FiveByteStart <> nil then
  begin
    Buffer[0] := $E8;
    PInteger(@Buffer[1])^ := PByte(CallFunction) - (FiveByteStart + 5);
    Result := WriteProcessMemory(GetCurrentProcess, FiveByteStart, @Buffer, SizeOf(Buffer), n);
  end
  else
    Result := False;
end;

function ReplaceOpCodeByRelJump(FiveByteStart: PByte; JumpTarget: Pointer; FillWithNop: Boolean): Boolean;
var
  Buffer: array[0..63] of Byte;
  n: SIZE_T;
  StartCodeSize, I: Integer;
begin
  if FiveByteStart <> nil then
  begin
    StartCodeSize := 5;
    Buffer[0] := $E9;
    PInteger(@Buffer[1])^ := PByte(JumpTarget) - (FiveByteStart + 5);
    if FillWithNop then
    begin
      StartCodeSize := GetStartCodeSize(FiveByteStart, 5);
      for I := 5 to StartCodeSize - 1 do
        Buffer[I] := $90;
    end;
    Result := WriteProcessMemory(GetCurrentProcess, FiveByteStart, @Buffer, StartCodeSize, n);
  end
  else
    Result := False;
end;

function ReplaceInstructionByRelCall(IntructionStart: PByte; CallFunction: Pointer): Boolean;
var
  StartCodeSize: Integer;
  Buffer: array[0..63] of Byte;
  I: Integer;
  n: SIZE_T;
begin
  if IntructionStart <> nil then
  begin
    StartCodeSize := GetStartCodeSize(IntructionStart, 5);
    Buffer[0] := $E8; // rel call
    {$IF CompilerVersion >= 20.0}
    PInteger(@Buffer[1])^ := PByte(CallFunction) - (IntructionStart + 5);
    {$ELSE}
    PInteger(@Buffer[1])^ := PAnsiChar(CallFunction) - (PAnsiChar(IntructionStart) + 5);
    {$IFEND}
    for I := 5 to StartCodeSize - 1 do
      Buffer[I] := $90;
    if not WriteProcessMemory(GetCurrentProcess, IntructionStart, @Buffer[0], StartCodeSize, n) then
      RaiseLastOSError;
    Result := True;
  end
  else
    Result := False;
end;

function HookWinApiProc(AWinApiProc, ANewProc: Pointer; var AHookInfo: TWinApiHookInfo; AResolveImportAddr: Boolean): Pointer;
begin
  if not HookWinApiProc(AWinApiProc, ANewProc, Result, AHookInfo, AResolveImportAddr) then
    Result := nil;
end;

function HookWinApiProc(AWinApiProc, ANewProc: Pointer; var AOrgCallProc; var AHookInfo: TWinApiHookInfo; AResolveImportAddr: Boolean): Boolean;
type
  PWinApiJmpCode = ^TWinApiJmpCode;
  TWinApiJmpCode = packed record
    Jump: packed record
      Jmp: Byte;
      Offset: Integer;
    end;
    JmpM5: Word;
  end;

var
  P: Pointer;
  {$IFDEF CPUX64}
  PP: PPointer;
  OldProtect: DWORD;
  {$ELSE}
  n: SIZE_T;
  JmpCode: TWinApiJmpCode;
  {$ENDIF CPUX64}
begin
  Pointer(AOrgCallProc) := nil;
  FillChar(AHookInfo, SizeOf(AHookInfo), 0);
  Result := False;
  if (AWinApiProc = nil) or (ANewProc = nil) then
    Exit;
  if AResolveImportAddr then
    AWinApiProc := GetActualAddr(AWinApiProc);

  AHookInfo.WinApiProc := AWinApiProc;

  {$IFDEF CPUX64}
  if (Word(AWinApiProc^) = $25FF) then // "FF 25 jmp [rel dword]"
  begin
    //PP^ <= GetActualAddr(AWinApiProc);
    PP := PPointer(PByte(@PAbsoluteIndirectJmp64(AWinApiProc).OpCode) + SizeOf(TAbsoluteIndirectJmp64) + PAbsoluteIndirectJmp64(AWinApiProc).Rel);
    // Set OrgCallProc before changing the code so that concurrent threads can call the hook that can call the org function
    Pointer(AOrgCallProc) := PP^;

    // Replace the target address by ANewProc.
    // WriteProcessMemory somehow fails: WriteProcessMemory(GetCurrentProcess, PP, @ANewProc, SizeOf(Pointer), n)
    if not VirtualProtect(PP, SizeOf(Pointer), PAGE_READWRITE, @OldProtect) then
      RaiseLastOSError;
    try
      PP^ := ANewProc;
    finally
      VirtualProtect(PP, SizeOf(Pointer), OldProtect, @OldProtect);
    end;
    AHookInfo.Mode := 100;
    AHookInfo.OrgCall := Pointer(AOrgCallProc);
    // Use the WinApiProc field for the import table address that we patch
    AHookInfo.WinApiProc := PByte(PP);
  end
  {$ELSE}
  if Word(AWinApiProc^) = $FF8B then // "8B FF  mov edi,edi"
  begin
    AHookInfo.Mode := 1;
    Move(Pointer(PByte(AWinApiProc) - 5)^, AHookInfo.Data[0], SizeOf(JmpCode));

    JmpCode.Jump.Jmp := $E9; // jmp dword-const
    JmpCode.Jump.Offset := PByte(ANewProc) - ((PByte(AWinApiProc) - 5) + 5);
    JmpCode.JmpM5 := $F9EB; // jmp $-5

    // Set OrgCallProc before changing the code so that concurrent threads can call the hook that can call the org function
    Pointer(AOrgCallProc) := PByte(AWinApiProc) + 2;
    if not WriteProcessMemory(GetCurrentProcess, PByte(AWinApiProc) - 5, @JmpCode, SizeOf(JmpCode), n) then
    begin
      Pointer(AOrgCallProc) := nil;
      RaiseLastOSError;
    end;
  end
  else if (Word(AWinApiProc^) = $F9EB) and // "EB F9  jmp $-5"
          (PByte(PByte(AWinApiProc) - 5)^ = $E9) then // "jmp dword-const"
  begin
    // already patched
    P := GetCallTargetAddress(PByte(AWinApiProc) - 5);
    Pointer(AOrgCallProc) := P;

    AHookInfo.Mode := 2;
    Move(Pointer(PByte(AWinApiProc) - 5)^, AHookInfo.Data[0], SizeOf(JmpCode.Jump));

    {Pointer(AOrgCallProc) :=} ReplaceRelCallOffset(PByte(AWinApiProc) - 5, ANewProc);
  end
  {$ENDIF CPUX64}
  else
  begin
    // unknown patch type
    P := RedirectOrgCall(AWinApiProc, ANewProc);
    Pointer(AOrgCallProc) := P;

    AHookInfo.Mode := 3;
    AHookInfo.OrgCall := P;

  {
    14 bytes x64 jump trampoline: must also detect such code to not destroy the QWORD after the jump if hooking a hooked function
      jmp [rel 0]
      dd 0, 0

    16 bytes x64 jump trampoline: better for other detour software, no defect disassembling but we still need to detect the "dd 0, 0" version
      jmp [rel 2]
      mov rax, qword-const
  }

  end;
  Result := True;
end;

function UnhookWinApiProc(const AHookInfo: TWinApiHookInfo): Boolean;
var
  {$IFDEF CPUX64}
  OldProtect: DWORD;
  {$ELSE}
  n: SIZE_T;
  {$ENDIF CPUX64}
begin
  Result := False;
  if (AHookInfo.WinApiProc <> nil) and (AHookInfo.Mode in [1..3, 100]) then
  begin
    case AHookInfo.Mode of
      {$IFDEF CPUX64}
      100:
        begin
          Result := VirtualProtect(AHookInfo.WinApiProc, SizeOf(Pointer), PAGE_READWRITE, @OldProtect);
          if Result then
          begin
            try
              PPointer(AHookInfo.WinApiProc)^ := AHookInfo.OrgCall;
            finally
              VirtualProtect(AHookInfo.WinApiProc, SizeOf(Pointer), OldProtect, @OldProtect);
            end;
          end;
        end;
      {$ELSE}
      1: Result := WriteProcessMemory(GetCurrentProcess, AHookInfo.WinApiProc - 5, @AHookInfo.Data[0], 5 + 2, n);
      2: Result := WriteProcessMemory(GetCurrentProcess, AHookInfo.WinApiProc - 5, @AHookInfo.Data[0], 5, n);
      {$ENDIF CPUX64}
      3:
        begin
          RestoreOrgCall(AHookInfo.WinApiProc, AHookInfo.OrgCall);
          Result := True;
        end;
    end;
  end;
end;

end.

