{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2005,2006 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit IDEUtils;

{.$I jedi\jedi.inc}
{$IFDEF VER170} {$DEFINE COMPILER9} {$ENDIF}  // Delphi 2005
{$IFDEF VER180} {$DEFINE COMPILER10} {$ENDIF} // Delphi 2006
{$IFDEF VER185} {$DEFINE COMPILER11} {$UNDEF COMPILER10} {$ENDIF} // Delphi 2007
{$IFDEF VER200} {$DEFINE COMPILER12} {$ENDIF} // Delphi 2009
{$IFDEF VER210} {$DEFINE COMPILER14} {$ENDIF} // Delphi 2010
{$IFDEF VER220} {$DEFINE COMPILER15} {$ENDIF} // Delphi XE

{$IF CompilerVersion >= 170} {$DEFINE COMPILER9_UP} {$IFEND}
{$IF CompilerVersion >= 180} {$DEFINE COMPILER10_UP} {$IFEND}
{$IF CompilerVersion >= 185} {$DEFINE COMPILER11_UP} {$IFEND}
{$IF CompilerVersion >= 200} {$DEFINE COMPILER12_UP} {$IFEND}
{$IF CompilerVersion >= 210} {$DEFINE COMPILER14_UP} {$IFEND}
{$IF CompilerVersion >= 220} {$DEFINE COMPILER15_UP} {$IFEND}

interface

uses
  Windows, ShlObj, SysUtils, Contnrs, Classes,
  Variants, StrUtils, Graphics,
  {$IF CompilerVersion >= 23.0} // XE2+
  PlatformAPI,
  {$IFEND}
  ToolsAPI,
  Forms;

{$IFNDEF COMPILER11_UP}
type
  INT_PTR = Integer;
  DWORD_PTR = DWORD;
{$ENDIF ~COMPILER11_UP}

function SupportsEx(const Instance: TObject; const IID: TGUID; out Intf): Boolean;

function FastFileExists(const Filename: string): Boolean;
function IncludeTrailingPathDelimiter(const Dir: string): string;
function ExcludeTrailingPathDelimiter(const Dir: string): string;
function FileAgeFindFile(const Filename: string): Integer;
function GetFileSize(const Filename: string): Cardinal;
function GetTempName(const Base: string): string;

function StartsText(const SubStr, S: string): Boolean;
function EndsText(const SubStr, S: string): Boolean;

function InArray(const Value: string; const Values: array of string): Boolean;
function DequoteStr(const S: string): string;

function ReadGlobalRegOption(const ValueName: string; DefaultValue: Boolean = False): Boolean;
function RegReadStringDef(RootKey: HKEY; const Key, ValueName, DefaultValue: string): string;
function RegReadBoolDef(RootKey: HKEY; const Key, ValueName: string; DefaultValue: Boolean): Boolean;
procedure RegWriteString(RootKey: HKEY; const Key, ValueName, Value: string);
procedure RegDeleteEntry(RootKey: HKEY; const Key, ValueName: string);

function FindForm(const AFormName, AFormClassName: string): TCustomForm;
function InheritsFromClassName(AObject: TObject; const AClassName: string): Boolean;

const
  MaxCacheItems = $1000;

type
  THashValue = type Cardinal;

  { The hash tables cannot be used with Key='' }
  PStringIntegerItem = ^TStringIntegerItem;
  TStringIntegerItem = record
    Key: string;
    Value: Integer;
    Next: PStringIntegerItem;
  end;

  TStringIntegerHash = class(TObject)
  private
    FItems: array[0..$1000 - 1] of PStringIntegerItem;
    FCount: Integer;
    FCacheItems: array[0..MaxCacheItems] of TStringIntegerItem;
    FCacheIndex: Integer;
  public
    destructor Destroy; override;
    procedure Clear; virtual;

    function Find(const AItem: string; out Value: Integer): Boolean; overload;
    function HasKey(const AItem: string): Boolean; overload;

    function Find(AHash: Integer; const AItem: string; out Value: Integer): Boolean; overload;
    function HasKey(AHash: Integer; const AItem: string): Boolean; overload;

    function Add(const AItem: string; AData: Integer): Integer; overload;
    function Add(AHash: Integer; const AItem: string; AData: Integer): Integer; overload;
    function Remove(const AItem: string): Integer;
    procedure SetValue(const AItem: string; AData: Integer); overload;
    procedure SetValue(AHash: Integer; const AItem: string; AData: Integer); overload;

    property Count: Integer read FCount;
  end;

type
  PStringStringItem = ^TStringStringItem;
  TStringStringItem = record
    Key: string;
    Value: string;
    Next: PStringStringItem;
  end;

  TStringStringHash = class(TObject)
  private
    FItems: array[0..$1000 - 1] of PStringStringItem;
    FCount: Integer;
    FCacheItems: array[0..MaxCacheItems] of TStringStringItem;
    FCacheIndex: Integer;
  public
    destructor Destroy; override;
    procedure Clear; virtual;

    function Find(const AItem: string; out Value: string): Boolean; overload;
    function HasKey(const AItem: string): Boolean; overload;

    function Find(AHash: Integer; const AItem: string; out Value: string): Boolean; overload;
    function HasKey(AHash: Integer; const AItem: string): Boolean; overload;

    function Add(const AItem: string; const AData: string): string; overload;
    function Add(AHash: Integer; const AItem: string; const AData: string): string; overload;
    function Remove(const AItem: string): string;

    property Count: Integer read FCount;
  end;

function HashString(const AItem: string): Integer;

const
  MaxBucketItems = $1000;

type
  { The hash tables cannot be used with Key='' }
  PBucketListItem = ^TBucketListItem;
  TBucketListItem = record
    Key: Pointer;
    Value: Pointer;
    Next: PBucketListItem;
  end;

  TBucketProc = procedure(AInfo, AItem, AData: Pointer; out AContinue: Boolean);

  TCustomBucketList = class(TObject)
  private
    FItems: array[0..MaxBucketItems - 1] of PBucketListItem;
    FCount: Integer;
    function GetData(AItem: Pointer): Pointer;
    procedure SetData(AItem: Pointer; const Value: Pointer);
  protected
    function BucketFor(AItem: Pointer): THashValue; virtual;
  public
    destructor Destroy; override;
    procedure Clear; virtual;

    function Add(AItem, AData: Pointer): Pointer;
    function Remove(AItem: Pointer): Pointer;

    function ForEach(AProc: TBucketProc; AInfo: Pointer = nil): Boolean;
    procedure Assign(AList: TCustomBucketList);

    function Exists(AItem: Pointer): Boolean;
    function Find(AItem: Pointer; out AData: Pointer): Boolean;
    property Data[AItem: Pointer]: Pointer read GetData write SetData; default;
  end;

  TBucketListSizes = (bl2, bl4, bl8, bl16, bl32, bl64, bl128, bl256);

  TBucketList = class(TCustomBucketList)
  public
    constructor Create(ABuckets: TBucketListSizes = bl16);
  end;

  TModuleBucketList = class(TBucketList)
  protected
    function BucketFor(AItem: Pointer): THashValue; override;
  end;

procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPALETTE;
  TransparentColor: TColorRef);

function ModuleFromAddr(const Addr: Pointer): HMODULE;
function Caller(Level: Integer): Pointer;
function MakeNotifyEvent(Data, Code: Pointer): TNotifyEvent;

function DelphiInterfaceToObject(const Intf: IInterface): TObject;
function GetQueryInterfaceImplFromDelphiInterface(const Intf: IInterface): Pointer;
function GetMethodImplFromDelphiInterface(const Intf: IInterface; VmtOffset: Integer): Pointer;
function FindObjectField(Obj: TObject; const AClassName: string; FindLast: Boolean = False): TObject;
function FindObjectFieldOffset(Obj: TObject; const AClassName: string; FindLast: Boolean): Cardinal; // Result=0 => not found

function VarToIntDef(const V: Variant; Default: Integer): Integer;
function VarToBoolDef(const V: Variant; Default: Boolean): Boolean;
function VarToStrDef(const V: Variant; const Default: string): string;

function QuoteFilename(const Filename: string): string;
function Dequote(const S: string): string;

procedure SplitPaths(List: TStrings; const Paths: string; DeleteDuplicates: Boolean = False); // removes quotes
function ConcatPaths(List: TStrings; const Delim: string): string; // add quotes where necessary
function SplitAndConcatPaths(const Paths, Separator: string): string;
function ConcatList(List: TStrings; const Delim: string): string;
function SplitAndConcatList(const Paths, Separator: string): string;

function GetBDSProjectsDir: string;
function ExpandDirMacros(const Path: string; Project: IOTAProject = nil): string;
function ExpandMacros(const Expression: string; const MacroNameValue: array of string): string;

type
  TIDEEvent = class(TObject)
  public
    procedure Add(AHandler: TNotifyEvent);
    procedure ForceAdd(AHandler: TNotifyEvent);
    procedure Remove(AHandler: TNotifyEvent);
  end;

function MainFormShown: TIDEEvent;
function MainFormCreated: TIDEEvent;
function MainFormDestroyed: TIDEEvent;

var
  AppDir: string;
  IsDelphi2007: Boolean = False;

implementation

uses
  Registry, IDEHooks;

function ReadGlobalRegOption(const ValueName: string; DefaultValue: Boolean): Boolean;
var
  KeyName: string;
  Reg: TRegistry;
  DataInfo: TRegDataInfo;
begin
  Result := DefaultValue;
  if BorlandIDEServices <> nil then
  begin
    KeyName := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Globals';
    if KeyName[1] = '\' then
      KeyName := Copy(KeyName, 2, MaxInt);

    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKeyReadOnly(KeyName) then
      begin
        if Reg.GetDataInfo(ValueName, DataInfo) then
        begin
          case DataInfo.RegData of
            rdString, rdExpandString:
              Result := StrToIntDef(Reg.ReadString(ValueName), Ord(DefaultValue)) <> 0;
            rdInteger:
              Result := Reg.ReadBool(ValueName);
            rdBinary:
              begin
                Result := Boolean(0);
                Reg.ReadBinaryData(ValueName, Result, SizeOf(Result));
              end;
          end;
        end;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

function RegReadStringDef(RootKey: HKEY; const Key, ValueName, DefaultValue: string): string;
var
  Reg: TRegistry;
  Info: TRegDataInfo;
begin
  Result := DefaultValue;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.KeyExists(Key) then
      if Reg.OpenKeyReadOnly(Key) then
      begin
        if Reg.GetDataInfo(ValueName, Info) and (Info.DataSize > 0) then
        begin
          case Info.RegData of
            rdString, rdExpandString:
              Result := Reg.ReadString(ValueName);
            rdInteger:
              Result := IntToStr(Reg.ReadInteger(ValueName));
            //rdBinary, rdUnknown:
          end;
        end;
      end;
  finally
    Reg.Free;
  end;
end;

function RegReadBoolDef(RootKey: HKEY; const Key, ValueName: string; DefaultValue: Boolean): Boolean;
var
  Reg: TRegistry;
  Info: TRegDataInfo;
begin
  Result := DefaultValue;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.KeyExists(Key) then
      if Reg.OpenKeyReadOnly(Key) then
      begin
        if Reg.GetDataInfo(ValueName, Info) and (Info.DataSize > 0) then
        begin
          case Info.RegData of
            rdString, rdExpandString:
              Result := Reg.ReadString(ValueName) <> '0';
            rdInteger:
              Result := Reg.ReadBool(ValueName);
            //rdBinary, rdUnknown:
          end;
        end;
      end;
  finally
    Reg.Free;
  end;
end;

procedure RegWriteString(RootKey: HKEY; const Key, ValueName, Value: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.OpenKey(Key, True) then
      Reg.WriteString(ValueName, Value);
  finally
    Reg.Free;
  end;
end;

procedure RegDeleteEntry(RootKey: HKEY; const Key, ValueName: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.KeyExists(Key) then
      if Reg.OpenKey(Key, False) then
        if Reg.ValueExists(ValueName) then
          Reg.DeleteValue(ValueName);
  finally
    Reg.Free;
  end;
end;

function FindForm(const AFormName, AFormClassName: string): TCustomForm;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
  begin
    Result := Screen.Forms[I];
    if (AFormName = '') or SameText(Result.Name, AFormName) then
      if (AFormClassName = '') or Result.ClassNameIs(AFormClassName) then
        Exit;
  end;
  Result := nil;
end;

function InheritsFromClassName(AObject: TObject; const AClassName: string): Boolean;
var
  Cls: TClass;
begin
  if AObject <> nil then
  begin
    Cls := AObject.ClassType;
    Result := True;
    while Cls <> nil do
    begin
      if Cls.ClassNameIs(AClassName) then
        Exit;
      Cls := Cls.ClassParent;
    end;
  end;
  Result := False;
end;

function GetMethodImplFromDelphiInterface(const Intf: IInterface; VmtOffset: Integer): Pointer;
type
  PEntry = ^TEntry;
  TEntry = packed record
    case Byte of
      // stdcall, safecall, cdecl, pascal:
      0: ( // add dword ptr [esp+$04], -i8
        AddArgShort: LongWord; // $83 $44 $24 $04
        ArgShortOffset: Shortint; // -i8
        ShortJmp: Byte;
        ShortJmpOffset: Integer;
      );
      1: ( // add [esp+$04], -i32
        AddArgLong: LongWord; // $81 $44 $24 $04
        ArgLongOffset: Longint; // -i32
        LongJmp: Byte;
        LongJmpOffset: Integer;
      );
      // register:
      2: ( // add eax, -i8
        RegAddArgShort: Word; // $83 $C0
        RegArgShortOffset: Shortint; // -i8
        RegShortJmp: Byte;
        RegShortJmpOffset: Integer;
      );
      3: ( // add eax, -i32
        RegAddArgLong: Byte; // $05
        RegArgLongOffset: Longint; // -i32
        RegLongJmp: Byte;
        RegLongJmpOffset: Integer;
      );
  end;

var
  P: PEntry;
begin
  Result := nil;
  if Intf <> nil then
  begin
    P := PEntry(Pointer(PByte(Pointer(Intf)^) + VmtOffset)^);
    if P <> nil then
    begin
      // stdcall, safecall, cdecl, pascal:
      if P^.AddArgShort = $04244483 then
        Result := PByte(@P^.ShortJmpOffset) + SizeOf(Integer) + P^.ShortJmpOffset
      else if P^.AddArgLong = $04244481 then
        Result := PByte(@P^.LongJmpOffset) + SizeOf(Integer) + P^.LongJmpOffset

      // register:
      else if P^.RegAddArgShort = $C083 then
        Result := PByte(@P^.RegShortJmpOffset) + SizeOf(Integer) + P^.RegShortJmpOffset
      else if P^.RegAddArgLong = $05 then
        Result := PByte(@P^.RegLongJmpOffset) + SizeOf(Integer) + P^.RegLongJmpOffset
      ;
    end;
  end;
end;

function GetQueryInterfaceImplFromDelphiInterface(const Intf: IInterface): Pointer;
begin
  Result := GetMethodImplFromDelphiInterface(Intf, 0);
end;

function DelphiInterfaceToObject(const Intf: IInterface): TObject;
//{$IFDEF COMPILER14_UP}
//begin
//  Result := Intf as TObject;    uses QueryInterface which is slow and goes through all supported interfaces before matching with ObjCastGUID
//end;
//{$ELSE}
type
  PEntry = ^TEntry;
  TEntry = packed record
    case Byte of
      // register:
      0: ( // add eax, -i8
        AddEAXShort: Word; // $83 $C0
        ShortOffset: Shortint; // -i8
      );
      1: ( // add eax, -i32
        AddEAXLong: Byte; // $05
        LongOffset: Longint; // -i32
      );
      // stdcall, safecall, cdecl, pascal:
      2: ( // add dword ptr [esp+$04], -i8
        AddArgShort: LongWord; // $83 $44 $24 $04
        ArgShortOffset: Shortint; // -i8
      );
      3: ( // add [esp+$04], -i32
        AddArgLong: LongWord; // $81 $44 $24 $04
        ArgLongOffset: Longint; // -i32
      );
      // abstract class
      4: ( // push ebp; mov ebp, esp
        PushEbp: Byte; // $55
        MovEbpEsp: Word; // $8B $EC
      );
  end;

var
  P: PEntry;
begin
  Result := nil;
  if Intf <> nil then
  begin
    P := PEntry(Pointer(Pointer(Intf)^)^);
    if P <> nil then
    begin
      // stdcall, safecall, cdecl, pascal:
      if P^.AddArgShort = $04244483 then
        Result := TObject(PAnsiChar(Intf) + P^.ArgShortOffset)
      else if P^.AddArgLong = $04244481 then
        Result := TObject(PAnsiChar(Intf) + P^.ArgLongOffset)

      // register:
      else if P^.AddEAXShort = $C083 then
        Result := TObject(PAnsiChar(Intf) + P^.ShortOffset)
      else if P^.AddEAXLong = $05 then
        Result := TObject(PAnsiChar(Intf) + P^.LongOffset)

      // abstract class (mostly a C++ class)
      else if (P^.PushEbp = $55) and (p^.MovEbpEsp = $EC8B) then
        Result := TObject(Intf);
    end;
  end;
end;
//{$ENDIF COMPILER14_UP}

function IsObject(Address: Pointer): Boolean;
asm
        MOV     EAX, [Address]
        CMP     EAX, EAX.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;

function ClassIsOfClass(AClass: TClass; const AClassName: string): Boolean;
begin
  if AClass <> nil then
    Result := AClass.ClassNameIs(AClassName) or ClassIsOfClass(AClass.ClassParent, AClassName)
  else
    Result := False;
end;

function FindObjectFieldOffset(Obj: TObject; const AClassName: string; FindLast: Boolean): Cardinal;
var
  InstSize: Cardinal;
  Field: TObject;
begin
  if Obj <> nil then
  begin
    if FindLast then
    begin
      Result := Obj.InstanceSize - SizeOf(Pointer);
      while Result > SizeOf(Pointer) do // omit VMT
      begin
        try
          Field := TObject(Pointer(Cardinal(Obj) + Result)^);
          if (Cardinal(Field) >= $00010000) and
             not IsBadReadPtr(Field, SizeOf(TClass)) and
             not IsBadReadPtr(PPointer(Field)^, {TObject.InstanceSize} SizeOf(Pointer)) and
             not IsBadReadPtr(Pointer(INT_PTR(PPointer(Field)^) + vmtSelfPtr), SizeOf(Pointer)) and
             IsObject(Field) then
            if ClassIsOfClass(Field.ClassType, AClassName) then
              Exit;
        except
        end;
        Dec(Result, SizeOf(Pointer));
      end;
    end
    else
    begin
      InstSize := Obj.InstanceSize;
      Result := SizeOf(Pointer); // start after TObject fields (VMT)
      while Result < InstSize do
      begin
        try
          Field := TObject(Pointer(Cardinal(Obj) + Result)^);
          if (Cardinal(Field) >= $00010000) and
             not IsBadReadPtr(Field, SizeOf(TClass)) and
             not IsBadReadPtr(PPointer(Field)^, {TObject.InstanceSize} SizeOf(Pointer)) and
             not IsBadReadPtr(Pointer(INT_PTR(PPointer(Field)^) + vmtSelfPtr), SizeOf(Pointer)) and
             IsObject(Field) then
            if ClassIsOfClass(Field.ClassType, AClassName) then
              Exit;
        except
        end;
        Inc(Result, SizeOf(Pointer));
      end;
    end;
  end;
  Result := 0;
end;

function FindObjectField(Obj: TObject; const AClassName: string; FindLast: Boolean): TObject;
var
  Offset: Cardinal;
begin
  Offset := FindObjectFieldOffset(Obj, AClassName, FindLast);
  if Offset = 0 then
    raise Exception.CreateFmt('%s object field not found', [AClassName])
  else
    Result := TObject(Pointer(Cardinal(Obj) + Offset)^);
end;

function MakeNotifyEvent(Data, Code: Pointer): TNotifyEvent;
begin
  TMethod(Result).Data := Data;
  TMethod(Result).Code := Code;
end;

function VarToIntDef(const V: Variant; Default: Integer): Integer;
begin
  Result := Default;
  if not VarIsEmpty(V) and not VarIsNull(V) and not (VarIsStr(V) and (VarToStr(V) = '')) then
  begin
    try
      Result := V;
    except
      on EConvertError do
        Result := Default;
      on EVariantInvalidArgError do
        Result := Default;
    end;
  end;
end;

function VarToBoolDef(const V: Variant; Default: Boolean): Boolean;
begin
  Result := Default;
  if not VarIsEmpty(V) and not VarIsNull(V) and not (VarIsStr(V) and (VarToStr(V) = '')) then
  begin
    try
      Result := V;
    except
      on EConvertError do
        Result := Default;
      on EVariantInvalidArgError do
        Result := Default;
    end;
  end;
end;

function VarToStrDef(const V: Variant; const Default: string): string;
begin
  if VarIsEmpty(V) or VarIsNull(V) then
    Result := Default
  else
  begin
    try
      Result := V;
    except
      on EConvertError do
        Result := Default;
      on EVariantInvalidArgError do
        Result := Default;
    end;
  end;
end;

function HashString(const AItem: string): Integer;
asm
  test eax, eax
  jz @@Leave

  xchg eax, edx
  mov eax, [edx-$04] // Length(AItem)
  xor ecx, ecx

@@HasStringNextChar:
  {$IFDEF UNICODE}
  mov cx, [edx]
  {ror cx, 4
  shl ecx, 1}
  add edx, 2
  add eax, ecx
  //and ecx, $0000ffff
  {$ELSE}
  mov cl, [edx]
  {ror cl, 4
  shl ecx, 1}
  add edx, 1
  add eax, ecx
  //and ecx, $000000ff
  {$ENDIF UNICODE}
  test ecx, ecx
  jnz @@HasStringNextChar

  and eax, MaxBucketItems-1
@@Leave:
end;

{ TStringIntegerHash }

destructor TStringIntegerHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStringIntegerHash.Clear;
var
  P, N: PStringIntegerItem;
  i: Integer;
begin
  if FCount > 0 then
  begin
    for i := 0 to High(FItems) do
    begin
      P := FItems[i];
      while P <> nil do
      begin
        N := P.Next;
        if (Cardinal(P) < Cardinal(@FCacheItems[0])) or (Cardinal(P) > Cardinal(@FCacheItems[High(FCacheItems)])) then
          Dispose(P);
        P := N;
        Dec(FCount);
      end;
      FItems[i] := nil;
      if FCount = 0 then
        Break;
    end;
  end;
  FCount := 0;
  FCacheIndex := 0;
end;

function TStringIntegerHash.Add(const AItem: string; AData: Integer): Integer;
begin
  Result := Add(HashString(AItem), AItem, AData);
end;

function TStringIntegerHash.Add(AHash: Integer; const AItem: string; AData: Integer): Integer;
var
  N: PStringIntegerItem;
begin
  if FCacheIndex < Length(FCacheItems) then
  begin
    N := @FCacheItems[FCacheIndex];
    Inc(FCacheIndex);
  end
  else
    New(N);
  N.Next := FItems[AHash];
  FItems[AHash] := N;
  Inc(FCount);
  N.Key := AItem;
  N.Value := AData;
  Result := AData;
end;

function TStringIntegerHash.Remove(const AItem: string): Integer;
var
  Index: Integer;
  P, N: PStringIntegerItem;
begin
  Index := HashString(AItem);
  N := FItems[Index];
  if N <> nil then
  begin
    if N.Key = AItem then
    begin
      Result := N.Value;
      P := N.Next;
      if (Cardinal(N) < Cardinal(@FCacheItems[0])) or (Cardinal(N) > Cardinal(@FCacheItems[High(FCacheItems)])) then
        Dispose(N);
      FItems[Index] := P;
      Dec(FCount);
      Exit;
    end
    else
    begin
      P := N;
      N := N.Next;
      while N <> nil do
      begin
        if N.Key = AItem then
        begin
          Result := N.Value;
          P.Next := N.Next;
          if (Cardinal(N) < Cardinal(@FCacheItems[0])) or (Cardinal(N) > Cardinal(@FCacheItems[High(FCacheItems)])) then
            Dispose(N);
          Dec(FCount);
          Exit;
        end;
        P := N;
        N := N.Next;
      end;
    end;
  end;
  Result := 0;
end;

function TStringIntegerHash.Find(const AItem: string; out Value: Integer): Boolean;
begin
  Result := Find(HashString(AItem), AItem, Value);
end;

function TStringIntegerHash.Find(AHash: Integer; const AItem: string; out Value: Integer): Boolean;
var
  N: PStringIntegerItem;
begin
  Value := 0;
  N := FItems[AHash];
  while N <> nil do
  begin
    if N.Key = AItem then
    begin
      Value := N.Value;
      Result := True;
      Exit;
    end;
    N := N.Next;
  end;
  Result := False;
end;

function TStringIntegerHash.HasKey(const AItem: string): Boolean;
var
  Value: Integer;
begin
  Result := Find(HashString(AItem), AItem, Value);
end;

function TStringIntegerHash.HasKey(AHash: Integer; const AItem: string): Boolean;
var
  N: PStringIntegerItem;
begin
  N := FItems[AHash];
  while N <> nil do
  begin
    if N.Key = AItem then
    begin
      Result := True;
      Exit;
    end;
    N := N.Next;
  end;
  Result := False;
end;

procedure TStringIntegerHash.SetValue(const AItem: string; AData: Integer);
begin
  SetValue(HashString(AItem), AItem, AData);
end;

procedure TStringIntegerHash.SetValue(AHash: Integer; const AItem: string; AData: Integer);
var
  N: PStringIntegerItem;
begin
  N := FItems[AHash];
  while N <> nil do
  begin
    if N.Key = AItem then
    begin
      N.Value := AData;
      Exit;
    end;
    N := N.Next;
  end;
  Add(AHash, AItem, AData);
end;

{ TStringStringHash }

destructor TStringStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStringStringHash.Clear;
var
  P, N: PStringStringItem;
  i: Integer;
begin
  if FCount > 0 then
  begin
    for i := 0 to High(FItems) do
    begin
      P := FItems[i];
      while P <> nil do
      begin
        N := P.Next;
        if (Cardinal(P) < Cardinal(@FCacheItems[0])) or (Cardinal(P) > Cardinal(@FCacheItems[High(FCacheItems)])) then
          Dispose(P);
        P := N;
        Dec(FCount);
      end;
      FItems[i] := nil;
      if FCount = 0 then
        Break;
    end;
  end;
  FCount := 0;
end;

function TStringStringHash.Add(const AItem: string; const AData: string): string;
begin
  Result := Add(HashString(AItem), AItem, AData);
end;

function TStringStringHash.Add(AHash: Integer; const AItem: string; const AData: string): string;
var
  N: PStringStringItem;
begin
  if FCacheIndex < Length(FCacheItems) then
  begin
    N := @FCacheItems[FCacheIndex];
    Inc(FCacheIndex);
  end
  else
    New(N);
  N.Next := FItems[AHash];
  FItems[AHash] := N;
  Inc(FCount);
  N.Key := AItem;
  N.Value := AData;
  Result := AData;
end;

function TStringStringHash.Remove(const AItem: string): string;
var
  Index: Integer;
  P, N: PStringStringItem;
begin
  Index := HashString(AItem);
  N := FItems[Index];
  if N <> nil then
  begin
    if N.Key = AItem then
    begin
      Result := N.Value;
      P := N.Next;
      if (Cardinal(N) < Cardinal(@FCacheItems[0])) or (Cardinal(N) > Cardinal(@FCacheItems[High(FCacheItems)])) then
        Dispose(N);
      FItems[Index] := P;
      Dec(FCount);
      Exit;
    end
    else
    begin
      P := N;
      N := N.Next;
      while N <> nil do
      begin
        if N.Key = AItem then
        begin
          Result := N.Value;
          P.Next := N.Next;
          if (Cardinal(N) < Cardinal(@FCacheItems[0])) or (Cardinal(N) > Cardinal(@FCacheItems[High(FCacheItems)])) then
            Dispose(N);
          Dec(FCount);
          Exit;
        end;
        P := N;
        N := N.Next;
      end;
    end;
  end;
  Result := '';
end;

function TStringStringHash.Find(const AItem: string; out Value: string): Boolean;
begin
  Result := Find(HashString(AItem), AItem, Value);
end;

function TStringStringHash.Find(AHash: Integer; const AItem: string; out Value: string): Boolean;
var
  N: PStringStringItem;
begin
  Value := '';
  N := FItems[AHash];
  while N <> nil do
  begin
    if N.Key = AItem then
    begin
      Value := N.Value;
      Result := True;
      Exit;
    end;
    N := N.Next;
  end;
  Result := False;
end;

function TStringStringHash.HasKey(const AItem: string): Boolean;
var
  Value: string;
begin
  Result := Find(HashString(AItem), AItem, Value);
end;

function TStringStringHash.HasKey(AHash: Integer; const AItem: string): Boolean;
var
  N: PStringStringItem;
begin
  N := FItems[AHash];
  while N <> nil do
  begin
    if N.Key = AItem then
    begin
      Result := True;
      Exit;
    end;
    N := N.Next;
  end;
  Result := False;
end;

{------------------------------------------------------------------------------}

function FastFileExists(const Filename: string): Boolean;
begin
  if Filename <> '' then
    Result := (GetFileAttributes(Pointer(Filename)) and FILE_ATTRIBUTE_DIRECTORY = 0)
  else
    Result := False;
end;

function IncludeTrailingPathDelimiter(const Dir: string): string;
begin
  if (Dir <> '') and not (Dir[Length(Dir)] in ['/', '\']) then
    Result := Dir + PathDelim
  else
    Result := Dir;
end;

function ExcludeTrailingPathDelimiter(const Dir: string): string;
begin
  if (Dir <> '') and (Dir[Length(Dir)] in ['/', '\']) then
    Result := Copy(Dir, 1, Length(Dir) - 1)
  else
    Result := Dir;
  if (Result <> '') and (Result[Length(Result)] = ':') then
    Result  := Result + '\';
end;

function FileAgeFindFile(const Filename: string): Integer;
var
  Handle: THandle;
  FindData: TWin32FindData;
  LocalFileTime: TFileTime;
begin
  Handle := FindFirstFile(Pointer(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
//    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi, LongRec(Result).Lo) then
        Exit;
    end;
  end;
  Result := -1;
end;

function GetFileSize(const Filename: string): Cardinal;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(Pointer(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    Result := FindData.nFileSizeLow;
  end
  else
    Result := 0;
end;

function GetTempName(const Base: string): string;
var
  BufSize: Cardinal;
  Dir: string;
  Buffer: array[0..MAX_PATH] of Char;
begin
  BufSize := GetTempPath(0, nil);
  SetLength(Dir, BufSize);
  GetTempPath(BufSize, PChar(Dir));
  Result := PChar(Dir); // truncate at #0
  if GetTempFileName(PChar(Dir), PChar(Base), 0, PChar(@Buffer[0])) <> 0 then
    Result := Buffer
  else
    RaiseLastOSError;
end;

function StartsText(const SubStr, S: string): Boolean;
begin
  Result := AnsiStartsText(SubStr, S);
end;

function EndsText(const SubStr, S: string): Boolean;
begin
  Result := AnsiEndsText(SubStr, S);
end;

procedure StretchBltTransparent(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; Palette: HPALETTE;
  TransparentColor: TColorRef);
var
  Color: TColorRef;
  bmAndBack, bmAndObject, bmAndMem, bmSave: HBITMAP;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: HBITMAP;
  MemDC, BackDC, ObjectDC, SaveDC: HDC;
  palDst, palMem, palSave, palObj: HPALETTE;
begin
  BackDC := CreateCompatibleDC(DstDC);
  ObjectDC := CreateCompatibleDC(DstDC);
  MemDC := CreateCompatibleDC(DstDC);
  SaveDC := CreateCompatibleDC(DstDC);

  bmAndObject := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndBack := CreateBitmap(SrcW, SrcH, 1, 1, nil);
  bmAndMem := CreateCompatibleBitmap(DstDC, DstW, DstH);
  bmSave := CreateCompatibleBitmap(DstDC, SrcW, SrcH);

  bmBackOld := SelectObject(BackDC, bmAndBack);
  bmObjectOld := SelectObject(ObjectDC, bmAndObject);
  bmMemOld := SelectObject(MemDC, bmAndMem);
  bmSaveOld := SelectObject(SaveDC, bmSave);

  palDst := 0;
  palMem := 0;
  palSave := 0;
  palObj := 0;
  if Palette <> 0 then
  begin
    palDst := SelectPalette(DstDC, Palette, True);
    RealizePalette(DstDC);
    palSave := SelectPalette(SaveDC, Palette, False);
    RealizePalette(SaveDC);
    palObj := SelectPalette(ObjectDC, Palette, False);
    RealizePalette(ObjectDC);
    palMem := SelectPalette(MemDC, Palette, True);
    RealizePalette(MemDC);
  end;

  SetMapMode(SrcDC, GetMapMode(DstDC));
  SetMapMode(SaveDC, GetMapMode(DstDC));

  BitBlt(SaveDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SRCCOPY);

  Color := SetBkColor(SaveDC, TransparentColor);
  BitBlt(ObjectDC, 0, 0, SrcW, SrcH, SaveDC, 0, 0, SRCCOPY);

  SetBkColor(SaveDC, Color);
  BitBlt(BackDC, 0, 0, SrcW, SrcH, ObjectDC, 0, 0, NOTSRCCOPY);
  BitBlt(MemDC, 0, 0, DstW, DstH, DstDC, DstX, DstY, SRCCOPY);
  StretchBlt(MemDC, 0, 0, DstW, DstH, ObjectDC, 0, 0, SrcW, SrcH, SRCAND);
  BitBlt(SaveDC, 0, 0, SrcW, SrcH, BackDC, 0, 0, SRCAND);
  StretchBlt(MemDC, 0, 0, DstW, DstH, SaveDC, 0, 0, SrcW, SrcH, SRCPAINT);
  BitBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SRCCOPY);

  if Palette <> 0 then
  begin
    SelectPalette(MemDC, palMem, False);
    SelectPalette(ObjectDC, palObj, False);
    SelectPalette(SaveDC, palSave, False);
    SelectPalette(DstDC, palDst, True);
  end;
  DeleteObject(SelectObject(BackDC, bmBackOld));
  DeleteObject(SelectObject(ObjectDC, bmObjectOld));
  DeleteObject(SelectObject(MemDC, bmMemOld));
  DeleteObject(SelectObject(SaveDC, bmSaveOld));

  DeleteDC(MemDC);
  DeleteDC(BackDC);
  DeleteDC(ObjectDC);
  DeleteDC(SaveDC);
end;

function ModuleFromAddr(const Addr: Pointer): HMODULE;
begin
  if Addr = nil then
    Result := 0
  else
    Result := HMODULE(FindHInstance(Addr)); // optimized by IDE Fix Pack
end;

{$STACKFRAMES ON}

type
  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallersEBP: DWORD;
    CallerAdr: DWORD;
  end;

function GetEBP: Pointer;
asm
  mov eax, ebp
end;

function GetStackTop: DWORD;
asm
  mov eax, fs:[4]
end;

function Caller(Level: Integer): Pointer;
var
  TopOfStack: Cardinal;
  BaseOfStack: Cardinal;
  StackFrame: PStackFrame;
begin
  Result := nil;
  try
    StackFrame := GetEBP;
    BaseOfStack := Cardinal(StackFrame) - 1;
    TopOfStack := GetStackTop;
    while (BaseOfStack < Cardinal(StackFrame)) and (Cardinal(StackFrame) < TopOfStack) do
    begin
      if Level = 0 then
      begin
        Result := Pointer(StackFrame^.CallerAdr - 1);
        Break;
      end;
      StackFrame := PStackFrame(StackFrame^.CallersEBP);
      Dec(Level);
    end;
  except
    Result := nil;
  end;
end;

function SupportsEx(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
begin
  Result := Supports(Instance, IID, Intf);
end;

function InArray(const Value: string; const Values: array of string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(Values) do
    if AnsiCompareText(Value, Values[i]) = 0 then
      Exit;
  Result := False;
end;

function DequoteStr(const S: string): string;
begin
  if (Length(S) > 1) and (S[1] = '"') and (S[Length(S)] = '"') then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

{ TCustomBucketList }

destructor TCustomBucketList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCustomBucketList.Add(AItem, AData: Pointer): Pointer;
var
  N: PBucketListItem;
  Hash: THashValue;
begin
  New(N);
  Hash := BucketFor(AItem);
  N.Next := FItems[Hash];
  FItems[Hash] := N;
  Inc(FCount);
  N.Key := AItem;
  N.Value := AData;
  Result := AData;
end;

procedure AssignProc(AInfo: Pointer; AItem, AData: Pointer; out AContinue: Boolean);
begin
  AContinue := True;
  TCustomBucketList(AInfo).Add(AItem, AData);
end;

procedure TCustomBucketList.Assign(AList: TCustomBucketList);
begin
  Clear;
  ForEach(AssignProc, Self);
end;

procedure TCustomBucketList.Clear;
var
  P, N: PBucketListItem;
  i: Integer;
begin
  if FCount > 0 then
  begin
    for i := 0 to High(FItems) do
    begin
      P := FItems[i];
      while P <> nil do
      begin
        N := P.Next;
        Dispose(P);
        P := N;
        Dec(FCount);
      end;
      FItems[i] := nil;
      if FCount = 0 then
        Break;
    end;
  end;
  FCount := 0;
end;

function TCustomBucketList.Exists(AItem: Pointer): Boolean;
var
  Data: Pointer;
begin
  Result := Find(AItem, Data);
end;

function TCustomBucketList.Find(AItem: Pointer; out AData: Pointer): Boolean;
var
  N: PBucketListItem;
begin
  AData := nil;
  N := FItems[BucketFor(AItem)];
  while N <> nil do
  begin
    if N.Key = AItem then
    begin
      AData := N.Value;
      Result := True;
      Exit;
    end;
    N := N.Next;
  end;
  Result := False;
end;

function TCustomBucketList.ForEach(AProc: TBucketProc; AInfo: Pointer): Boolean;
var
  P, N: PBucketListItem;
  i: Integer;
begin
  Result := False;
  if FCount > 0 then
  begin
    for i := 0 to High(FItems) do
    begin
      P := FItems[i];
      while P <> nil do
      begin
        N := P.Next;
        AProc(AInfo, P.Key, P.Value, Result);
        if not Result then
          Exit;
        P := N;
      end;
    end;
  end;
end;

function TCustomBucketList.GetData(AItem: Pointer): Pointer;
begin
  if not Find(AItem, Result) then
  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    RaiseLastOSError;
  end;
end;

function TCustomBucketList.Remove(AItem: Pointer): Pointer;
var
  Index: THashValue;
  P, N: PBucketListItem;
begin
  Index := BucketFor(AItem);
  N := FItems[Index];
  if N <> nil then
  begin
    if N.Key = AItem then
    begin
      Result := N.Value;
      P := N.Next;
      Dispose(N);
      FItems[Index] := P;
      Dec(FCount);
      Exit;
    end
    else
    begin
      P := N;
      N := N.Next;
      while N <> nil do
      begin
        if N.Key = AItem then
        begin
          Result := N.Value;
          P.Next := N.Next;
          Dispose(N);
          Dec(FCount);
          Exit;
        end;
        P := N;
        N := N.Next;
      end;
    end;
  end;
  Result := nil;
end;

procedure TCustomBucketList.SetData(AItem: Pointer; const Value: Pointer);
var
  N: PBucketListItem;
begin
  N := FItems[BucketFor(AItem)];
  while N <> nil do
  begin
    if N.Key = AItem then
    begin
      N.Value := Value;
      Exit;
    end;
    N := N.Next;
  end;
  SetLastError(ERROR_INVALID_PARAMETER);
  RaiseLastOSError;
end;

function TCustomBucketList.BucketFor(AItem: Pointer): THashValue;
begin
  Result := THashValue(Cardinal(AItem) mod MaxBucketItems);
end;

{ TBucketList }

constructor TBucketList.Create(ABuckets: TBucketListSizes);
begin
  inherited Create;
end;

{ TModuleBucketList }

function TModuleBucketList.BucketFor(AItem: Pointer): THashValue;
begin
  Result := THashValue((Cardinal(AItem) shr 16) mod MaxBucketItems);
end;

{$IFDEF COMPILER10}

// Version Info extracting
procedure VersionExtractFileInfo(const FixedInfo: TVSFixedFileInfo; var Major, Minor, Build, Revision: Word);
begin
  Major := HiWord(FixedInfo.dwFileVersionMS);
  Minor := LoWord(FixedInfo.dwFileVersionMS);
  Build := HiWord(FixedInfo.dwFileVersionLS);
  Revision := LoWord(FixedInfo.dwFileVersionLS);
end;

procedure VersionExtractProductInfo(const FixedInfo: TVSFixedFileInfo; var Major, Minor, Build, Revision: Word);
begin
  Major := HiWord(FixedInfo.dwProductVersionMS);
  Minor := LoWord(FixedInfo.dwProductVersionMS);
  Build := HiWord(FixedInfo.dwProductVersionLS);
  Revision := LoWord(FixedInfo.dwProductVersionLS);
end;

// Fixed Version Info routines
function VersionFixedFileInfo(const FileName: string; var FixedInfo: TVSFixedFileInfo): Boolean;
var
  Size, FixInfoLen: DWORD;
  Handle: THandle;
  Buffer: string;
  FixInfoBuf: PVSFixedFileInfo;
begin
  Result := False;
  Size := GetFileVersionInfoSize(PChar(FileName), Handle);
  if Size > 0 then
  begin
    SetLength(Buffer, Size);
    if GetFileVersionInfo(PChar(FileName), Handle, Size, Pointer(Buffer)) and
      VerQueryValue(Pointer(Buffer), '\', Pointer(FixInfoBuf), FixInfoLen) and
      (FixInfoLen = SizeOf(TVSFixedFileInfo)) then
    begin
      Result := True;
      FixedInfo := FixInfoBuf^;
    end;
  end;
end;

function CheckProductVersion(const FileName: string; Major, Minor: Word; ExactMatch: Boolean): Boolean;
var
  Info: TVSFixedFileInfo;
  LMajor, LMinor, LBuild, LRevision: Word;
begin
  if VersionFixedFileInfo(FileName, Info) then
  begin
    VersionExtractProductInfo(Info, LMajor, LMinor, LBuild, LRevision);
    if ExactMatch then
      Result := (LMajor = Major) and (LMinor = Minor)
    else
      Result := (LMajor >= Major) and (LMinor >= Minor);
  end
  else
    Result := False;
end;

{$ENDIF COMPILER10}

function QuoteFilename(const Filename: string): string;
begin
  if (Pos(' ', Filename) > 0) or (Pos('-', Filename) > 0) or (Pos('+', Filename) > 0) then
  begin
    if Filename[1] <> '"' then
      Result := '"' + Filename + '"'
    else
      Result := Filename;
  end
  else
    Result := Filename;
end;

function Dequote(const S: string): string;
begin
  if (Length(S) > 1) and (S[1] = '"') and (S[Length(S)] = '"') then
    Result := Copy(S, 2, Length(S) - 2)
  else
    Result := S;
end;

procedure SplitPaths(List: TStrings; const Paths: string; DeleteDuplicates: Boolean);
var
  Start, i: Integer;
  Dir: string;
begin
  Start := 1;
  for i := 1 to Length(Paths) do
  begin
    if Paths[i] = ';' then
    begin
      Dir := Copy(Paths, Start, i - Start);
      if Dir <> '' then
      begin
        if Dir[1] = '"' then
          Dir := Dequote(Dir);
        if not DeleteDuplicates or (List.IndexOf(Dir) = -1) then
          List.Add(Dir);
      end;
      Start := i + 1;
    end;
  end;
  if Start <= Length(Paths) then
  begin
    Dir := Copy(Paths, Start, MaxInt);
    if Dir <> '' then
    begin
      if Dir[1] = '"' then
        Dir := Dequote(Dir);
      if not DeleteDuplicates or (List.IndexOf(Dir) = -1) then
        List.Add(Dir);
    end;
  end;
end;

function ConcatPaths(List: TStrings; const Delim: string): string;
var
  i: Integer;
begin
  Result := '';
  if List.Count > 0 then
  begin
    Result := QuoteFilename(List[0]);
    for i := 1 to List.Count - 1 do
      Result := Result + Delim + QuoteFilename(List[i]);
  end;
end;

function SplitAndConcatPaths(const Paths, Separator: string): string;
var
  List: TStrings;
begin
  List := TStringList.Create;
  try
    SplitPaths(List, Paths, True);
    Result := ConcatPaths(List, Separator);
  finally
    List.Free;
  end;
end;

function ConcatList(List: TStrings; const Delim: string): string;
var
  i, Len, DelimLen: Integer;
  S: string;
  P: PChar;
begin
  DelimLen := Length(Delim);
  Len := 0;
  if List.Count > 0 then
  begin
    Inc(Len, Length(List[0]));
    for i := 1 to List.Count - 1 do
      Inc(Len, DelimLen + Length(List[i]));
  end;
  SetLength(Result, Len);
  P := Pointer(Result);
  for i := 0 to List.Count - 1 do
  begin
    if (i > 0) and (DelimLen > 0) then
    begin
      Move(Pointer(Delim)^, P^, DelimLen * SizeOf(Char));
      Inc(P, DelimLen);
    end;
    S := List[i];
    Len := Length(S);
    if Len > 0 then
    begin
      Move(Pointer(S)^, P^, Len * SizeOf(Char));
      Inc(P, Len);
    end;
  end;
end;

function SplitAndConcatList(const Paths, Separator: string): string;
var
  List: TStrings;
begin
  List := TStringList.Create;
  try
    SplitPaths(List, Paths, True);
    Result := ConcatList(List, Separator);
  finally
    List.Free;
  end;
end;

{$IFDEF COMPILER9_UP}
var
  GlobalBDSProjectsDir: string;

  {$IFNDEF UNICODE}
  _SHGetSpecialFolderPathA: function(hwndOwner: HWND; lpszPath: PAnsiChar;
    nFolder: Integer; fCreate: BOOL): BOOL; stdcall;
  {$ENDIF UNICODE}

function GetBDSProjectsDir: string;
{$IFDEF COMPILER9}
const
  CIV = '90';
  ProjectDirResId = 64431;
{$ENDIF COMPILER9}
{$IFDEF COMPILER10}
const
  CIV = '100';
  ProjectDirResId = 64719;
{$ENDIF COMPILER10}
var
  {$IFNDEF COMPILER11_UP}
  LocaleName: array[0..4] of Char;
  RootDir: string;
  Filename: string;
  h: HMODULE;
  {$ENDIF ~COMPILER11_UP}
  PersDir: string;
begin
  GlobalBDSProjectsDir := ExcludeTrailingPathDelimiter(GetEnvironmentVariable('BDSPROJECTSDIR'));
  if GlobalBDSProjectsDir = '' then
  begin
    GlobalBDSProjectsDir := 'Borland Studio Projects'; // do not localize
    {$IFNDEF COMPILER11_UP}
    FillChar(LocaleName, SizeOf(LocaleName[0]), 0);
    GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
    if LocaleName[0] <> #0 then
    begin
      RootDir := AppDir;
      Filename := RootDir + PathDelim + 'coreide' + CIV + '.';
      if FastFileExists(Filename + LocaleName) then
        Filename := Filename + LocaleName
      else
      begin
        LocaleName[2] := #0;
        if FastFileExists(Filename + LocaleName) then
          Filename := Filename + LocaleName
        else
          Filename := '';
      end;

      if Filename <> '' then
      begin
        h := LoadLibraryEx(PChar(Filename), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
        if h <> 0 then
        begin
          SetLength(GlobalBDSProjectsDir, 4096);
          SetLength(GlobalBDSProjectsDir, LoadString(h, ProjectDirResId, PChar(GlobalBDSProjectsDir), Length(GlobalBDSProjectsDir) - 1));
          FreeLibrary(h);
        end;
      end;
    end;
    {$ENDIF ~COMPILER11_UP}

    {$IFDEF UNICODE}
    SetLength(PersDir, MAX_PATH);
    if SHGetSpecialFolderPath(0, PChar(PersDir), CSIDL_PERSONAL, False) then
    {$ELSE}
    if not Assigned(_SHGetSpecialFolderPathA) then
      _SHGetSpecialFolderPathA := GetProcAddress(GetModuleHandle('shell32.dll'), PAnsiChar('SHGetSpecialFolderPathA'));
    SetLength(PersDir, MAX_PATH);
    if Assigned(_SHGetSpecialFolderPathA) and
       _SHGetSpecialFolderPathA(0, PChar(PersDir), CSIDL_PERSONAL, False) then
    {$ENDIF UNICODE}
    begin
      SetLength(PersDir, StrLen(PChar(PersDir)));
      GlobalBDSProjectsDir := ExcludeTrailingPathDelimiter(PersDir) + PathDelim + GlobalBDSProjectsDir;
    end
    else
      GlobalBDSProjectsDir := '';
  end;
  Result := GlobalBDSProjectsDir;
end;
{$ELSE}
function GetBDSProjectsDir: string;
begin
  Result := ExtractFilePath(AppDir) + 'Projects';
end;
{$ENDIF COMPILER9_UP}

function ExpandDirMacros(const Path: string; Project: IOTAProject): string;
var
  I: Integer;
  Start, Len: Integer;
  NewS, S: string;
begin
  if Project = nil then
    Project := GetActiveProject;

  Result := Path;
  Len := Length(Result);
  I := 1;
  while I <= Len do
  begin
    if (Result[I] = '$') and (I < Len - 1) and (Result[I + 1] = '(') then
    begin
      Start := I;
      while (I <= Len) and (Result[I] <> ')') do
        Inc(I);
      if I <= Len then
      begin
        S := AnsiUpperCase(Copy(Result, Start + 2, I - Start - 2));

        NewS := GetEnvironmentVariable(S);
        if NewS = '' then
        begin
          if (S = 'BDS') or (S = 'BCB') or (S = 'DELPHI') then
            NewS := ExtractFileDir(AppDir)
          else if S = 'BDSPROJECTSDIR' then
            NewS := GetBDSProjectsDir
          else if S = 'PLATFORM' then
          begin
           {$IF CompilerVersion >= 23.0} // XE2+
           if Project <> nil then
             NewS := Project.CurrentPlatform
           else
             NewS := cWin32Platform;
           {$IFEND}
          end
          else if S = 'CONFIG' then
          begin
            {$IF CompilerVersion >= 23.0} // XE2+
            if Project <> nil then
              NewS := Project.CurrentConfiguration
            else
              NewS := 'Debug';
            {$IFEND}
          end;
          //else if S = 'BDSCOMMONDIR' then
        end;

        Delete(Result, Start, I - Start + 1);
        Insert(NewS, Result, Start);
        Dec(I, Length(S) + 3);
        Inc(I, Length(NewS));
        Len := Length(Result);
      end;
    end;
    Inc(I);
  end;
end;

function ExpandMacros(const Expression: string; const MacroNameValue: array of string): string;
var
  I, K: Integer;
  Start, Len: Integer;
  NewS, S: string;
begin
  Result := Expression;
  Len := Length(Result);
  I := 1;
  while I <= Len do
  begin
    if (Result[I] = '$') and (I < Len - 1) and (Result[I + 1] = '(') then
    begin
      Start := I;
      while (I <= Len) and (Result[I] <> ')') do
        Inc(I);
      if I <= Len then
      begin
        S := AnsiUpperCase(Copy(Result, Start + 2, I - Start - 2));
        NewS := '';
        for K := 0 to Length(MacroNameValue) div 2 - 1 do
        begin
          if SameText(S, MacroNameValue[K * 2]) then
          begin
            NewS := MacroNameValue[K * 2 + 1];

            Delete(Result, Start, I - Start + 1);
            Insert(NewS, Result, Start);
            Dec(I, Length(S) + 3);
            Inc(I, Length(NewS));
            Len := Length(Result);
            Break;
          end;
        end;
      end;
    end;
    Inc(I);
  end;
end;

{ TIDEEvent }

procedure TIDEEvent.Add(AHandler: TNotifyEvent);
  external designide_bpl name '@Events@TEvent@Add$qqrynpqqrp14System@TObject$v';
procedure TIDEEvent.ForceAdd(AHandler: TNotifyEvent);
  external designide_bpl name '@Events@TEvent@ForceAdd$qqrynpqqrp14System@TObject$v';
procedure TIDEEvent.Remove(AHandler: TNotifyEvent);
  external designide_bpl name '@Events@TEvent@Remove$qqrynpqqrp14System@TObject$v';

function MainFormShown: TIDEEvent;
  external coreide_bpl name '@Ideintf@MainFormShown$qqrv';
function MainFormCreated: TIDEEvent;
  external coreide_bpl name '@Ideintf@MainFormCreated$qqrv';
function MainFormDestroyed: TIDEEvent;
  external coreide_bpl name '@Ideintf@MainFormDestroyed$qqrv';

procedure Init;
var
  MainModuleName: string;
begin
  MainModuleName := GetModuleName(0);
  {$IFDEF COMPILER10}
  IsDelphi2007 := CheckProductVersion(MainModuleName, 11, 0, True);
  {$ENDIF COMPILER10}
  AppDir := ExtractFileDir(MainModuleName);
end;

initialization
  Init;

end.
