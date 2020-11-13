unit DbgStepIntoSkip;

interface

// TODO: If FindSourcLine32 returns 0 the debugger uses the already transfered GetCodeRange32 information
//       to show the CPU view. That's not what this feature wants...

implementation

uses
  Windows, SysUtils, Hooking, IDEHooks;

type
  TCodePos = record
    FileIndex: LongWord;
    LineNum: LongWord;
    SymTabNum: LongWord;
  end;

  PSymbolPointer = type Pointer;
  PUnitPointer = type Pointer;

  TUserAddr = LongWord;

  TGetCodeRangesFunc = function(Addr: TUserAddr; i: Integer): Integer; cdecl;

var
  OrgGetCodeRanges32: function(SymTabIndex: Integer; PostFunc: TGetCodeRangesFunc): Integer; stdcall;
  OrgFindSourceLine32: function(Addr: TUserAddr; out StartAddr: TUserAddr; out LineSize: Integer; ReportMain: Integer): TCodePos; stdcall;
  FindUnitByLoad: function(Addr: TUserAddr; var BlockNo: Integer): PUnitPointer;
  FindProcByLoad: function(U: PUnitPointer; Addr: TUserAddr; BlockNo: Integer): PSymbolPointer;

function BrowserGetUnitSymbol(U: PUnitPointer): PSymbolPointer; stdcall;
  external dcc32_dll;
function BrowserGetSymbolTextBuff(Symbol: PSymbolPointer; Buffer: PAnsiChar; BufLen: Integer; Flags: Integer): Integer; stdcall;
  external dcc32_dll;

function StrFileName(P: PAnsiChar): PAnsiChar;
begin
  Result := P;
  if P <> nil then
    repeat
      case P^ of
        #0:
          Break;
        '\', '/':
          Result := P + 1;
      end;
      Inc(P);
    until False;
end;

function GetCodeRanges32(SymTabIndex: Integer; PostFunc: TGetCodeRangesFunc): Integer; stdcall;
begin
  Result := OrgGetCodeRanges32(SymTabIndex, PostFunc);
end;

function FindSourceLine32(Addr: TUserAddr; out StartAddr: TUserAddr; out LineSize: Integer; ReportMain: Integer): TCodePos; stdcall;
var
  BlockNum: Integer;
  U: PUnitPointer;
  Sym: PSymbolPointer;
  Buf: array[0..MAX_PATH] of AnsiChar;
begin
  if Addr <> 0 then
  begin
    U := FindUnitByLoad(Addr, BlockNum);
    if U <> nil then
    begin
      Sym := BrowserGetUnitSymbol(U);
      if Sym <> nil then
      begin
        Buf[BrowserGetSymbolTextBuff(Sym, Buf, Length(Buf) - 1, 0)] := #0;
        if StrIComp('Vcl.Forms', Buf) = 0 then
        begin
          Result.FileIndex := 0;
          Result.LineNum := 0;
          Result.SymTabNum := 0;
          StartAddr := 0;
          LineSize := 0;
          Exit;
        end;
      end;
    end;
  end;
  Result := OrgFindSourceLine32(Addr, StartAddr, LineSize, ReportMain);
end;


procedure Init;
const
  xFindSourceLineBytes: array[0..31] of SmallInt = (
    $8B, $4D, $F0,        // mov ecx,[ebp-$10]            //  0
    $8B, $D7,             // mov edx,edi                  //  3
    $8B, $45, $F4,        // mov eax,[ebp-$0c]            //  5
    $E8, -1, -1, -1, -1,  // call FindProcByLoad          //  8
    $8B, $D8,             // mov ebx,eax                  // 13
    $85, $DB,             // test ebx,ebx                 // 15
    $74, $2A,             // jz $0bfc8f5a                 // 17
    $8B, $45, $F4,        // mov eax,[ebp-$0c]            // 19
    $F6, $40, $18, $01,   // test byte ptr [eax+$18],$01  // 22
    $74, $39,             // jz $0bfc8f72                 // 26
    $83, $7D, $0C, $00    // cmp dword ptr [ebp+$0c],$00  // 28
  );

  FindSourceLineBytes: array[0..63] of SmallInt = (
    $8D, $55, $F0,            // lea edx,[ebp-$10]             //  0
    $8B, $C7,                 // mov eax,edi                   //  3
    $E8, -1, -1, -1, -1,      // call FindUnitByLoad           //  5
    $89, $45, $F4,            // mov [ebp-$0c],eax             // 10
    $8B, $55, $F0,            // mov edx,[ebp-$10]             // 13
    $89, $55, $E0,            // mov [ebp-$20],edx             // 16
    $83, $7D, $F4, $00,       // cmp dword ptr [ebp-$0c],$00   // 19
    $75, $18,                 // jnz $0bfc8f1d                 // 23
    $8B, $4D, $D8,            // mov ecx,[ebp-$28]             // 25
    $8B, $C6,                 // mov eax,esi                   // 28
    $89, $0E,                 // mov [esi],ecx                 // 30
    $8B, $4D, $DC,            // mov ecx,[ebp-$24]             // 32
    $89, $4E, $04,            // mov [esi+$04],ecx             // 35
    $8B, $4D, $E0,            // mov ecx,[ebp-$20]             // 38
    $89, $4E, $08,            // mov [esi+$08],ecx             // 41
    $E9, $2E, $01, $00, $00,  // jmp $0bfc904b                 // 44
    $8B, $4D, $F0,            // mov ecx,[ebp-$10]             // 49
    $8B, $D7,                 // mov edx,edi                   // 52
    $8B, $45, $F4,            // mov eax,[ebp-$0c]             // 54
    $E8, -1, -1, -1, -1,      // call FindProcByLoad           // 57
    $8B, $D8                  // mov ebx,eax                   // 62
  );

  RunHndlr_advanceBytes: array[0..58] of SmallInt = (
    $55,                           // push ebp                 //  0
    $8B, $EC,                      // mov ebp,esp              //  1
    $81, $C4, $74, $FE, $FF, $FF,  // add esp,$fffffe74        //  3
    $53,                           // push ebx                 //  9
    $56,                           // push esi                 // 10
    $57,                           // push edi                 // 11
    $8B, $75, $08,                 // mov esi,[ebp+$08]        // 12
    $8D, $9D, $98, $FE, $FF, $FF,  // lea ebx,[ebp-$00000168]  // 15
    $8B, $46, $08,                 // mov eax,[esi+$08]        // 21
    $8B, $50, $28,                 // mov edx,[eax+$28]        // 24
    $8B, $4A, $7C,                 // mov ecx,[edx+$7c]        // 27
    $83, $C1, $38,                 // add ecx,$38              // 30
    $89, $4D, $FC,                 // mov [ebp-$04],ecx        // 33
    $8B, $45, $FC,                 // mov eax,[ebp-$04]        // 36
    $83, $C0, $20,                 // add eax,$20              // 39
    $83, $38, $00,                 // cmp dword ptr [eax],$00  // 42
    $74, $07,                      // jz $03d8971a             // 45
    $8B, $10,                      // mov edx,[eax]            // 47
    $8B, $4A, $10,                 // mov ecx,[edx+$10]        // 49
    $EB, $02,                      // jmp $03d8971c            // 52
    $33, $C9,                      // xor ecx,ecx              // 54
    $89, $4D, $F8                  // mov [ebp-$08],ecx        // 56
  );

var
  LibDcc32: THandle;
  P: PByte;
begin
  LibDcc32 := LoadLibrary(dcc32_dll);
  @OrgFindSourceLine32 := GetProcAddress(LibDcc32, 'FindSourceLine');
  @OrgGetCodeRanges32 := GetProcAddress(LibDcc32, 'GetCodeRanges');

  P := FindMethodPtr(THandle(@OrgFindSourceLine32), FindSourceLineBytes, $1000);
  if P <> nil then
  begin
    @FindUnitByLoad := PByte(PByte(@P[5]) + 5 + PInteger(@P[5 + 1])^);
    @FindProcByLoad := PByte(PByte(@P[57]) + 5 + PInteger(@P[57 + 1])^);
  end;

  @OrgFindSourceLine32 := RedirectOrgCall(@OrgFindSourceLine32, @FindSourceLine32);
//  @OrgGetCodeRanges32 := RedirectOrgCall(@OrgGetCodeRanges32, @GetCodeRanges32);
end;

initialization
//  Init;

end.
