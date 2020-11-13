{******************************************************************************}
{*                                                                            *}
{* Delphi Lexer                                                               *}
{*                                                                            *}
{* (C) 2003-2008 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit DelphiLexer;

{$I DelphiParser.inc}

{$IFDEF COMPILER12_UP}
  {$STRINGCHECKS OFF}
{$ENDIF COMPILER12_UP}

{$IFDEF COMPILER10_UP}
  {$DEFINE SUPPORTS_INLINE}
{$ENDIF}

interface

uses
  SysUtils, Classes, Contnrs, TypInfo;

const
  WhiteChars = [#1..#32];
  NumberChars = ['0'..'9'];
  HexNumberChars = NumberChars + ['A'..'F', 'a'..'f'];
  IdentFirstChars = ['a'..'z', 'A'..'Z', '_'];
  IdentChars = IdentFirstChars + NumberChars;

type
  PUTF8Char = PAnsiChar;
  UTF8Char = AnsiChar;
  {$IFNDEF UNICODE}
  RawByteString = AnsiString;
  UnicodeString = WideString;
  {$ENDIF UNICODE}

  TTokenKind = (
    tkNone,

    tkSymbol,
      tkEqual,
      tkGreaterThan,
      tkLessThan,
      tkMinus,
      tkPlus,
      tkMultiply,
      tkDivide,

      tkLParan,
      tkRParan,
      tkLBracket,
      tkRBracket,
      tkColon,
      tkSemicolon,
      tkComma,
      tkPointer,     // '^'
      tkAddr,        // '@'
      tkAmp,         // '&' // D8/9
      tkQualifier,   // '.'

    // tkSymbolLevel2 >= tkGreaterEqualThan
      tkGreaterEqualThan, // '>='
      tkLessEqualThan,    // '<='
      tkNotEqual,         // '<>'
      tkRange,            // '..'
      tkAssign,           // ':='

    tkComment,
    tkDirective,

    tkString,
    tkInt,
    tkFloat,

    tkMacro, // macro support
    tkIdent,
      tkI_read,
      tkI_write,
      tkI_add,
      tkI_remove,
      tkI_default,
      tkI_name,
      tkI_index,
      tkI_message,
      tkI_static, // D9
      tkI_forward,
      tkI_requires,
      tkI_contains,
      tkI_platform,
      tkI_deprecated,
      tkI_experimental, // D9
      tkI_out,
      tkI_helper,

      tkI_overload,
      tkI_virtual,
      tkI_override,
      tkI_dynamic,
      tkI_abstract,
      tkI_reintroduce,

      tkI_assembler,
      tkI_register,
      tkI_stdcall,
      tkI_cdecl,
      tkI_safecall,
      tkI_pascal,
      tkI_external,

      tkI_automated,
      tkI_strict, // D9
      tkI_private,
      tkI_protected,
      tkI_public,
      tkI_published,
      tkI_local,  // D9
      tkI_export,
      tkI_resident, // D9
      tkI_far,
      tkI_near,
      tkI_readonly,
      tkI_writeonly,
      tkI_nodefault,
      tki_stored,
      tkI_implements,
      tkI_varargs,
      tkI_dispid,

      tkI_absolute,
      tkI_final, // D9
      tkI_unsafe, // D9
      tkI_sealed, // D9

      tkI_package,
      tkI_reference,

      tkI_on,
      tkI_at,

    //tkIdentStrictReserved >= tkI_if
      tkI_if,
      tkI_else,
      tkI_then,
      tkI_while,
      tkI_do,
      tkI_for,
      tkI_to,
      tkI_downto,
      tkI_repeat,
      tkI_until,
      tkI_case,
      tkI_with,
      tkI_raise,
      tkI_in,
      tkI_is,
      tkI_as,
      tkI_of,
      tkI_file,
      tkI_goto,
      tkI_except,
      tkI_finally,
      tkI_try,
      tkI_nil,

      tkI_shl,
      tkI_shr,
      tkI_mod,
      tkI_div,
      tkI_and,
      tkI_or,
      tkI_not,
      tkI_xor,

      tkI_class,
      tkI_object,
      tkI_record,
      tkI_var,
      tkI_const,
      tkI_type,
      tkI_threadvar,
      tkI_label,
      tkI_exports,
      tkI_resourcestring,
      tkI_unit,
      tkI_program,
      tkI_uses,
      tkI_interface,
      tkI_implementation,
      tkI_begin,
      tkI_end,
      tkI_procedure,
      tkI_function,
      tkI_operator,
      tkI_constructor,
      tkI_destructor,
      tkI_property,
      tkI_packed,
      tkI_dispinterface,
      tkI_string,

      tkI_array,
      tkI_set,

      tkI_asm,

      tkI_inherited,
      tkI_inline,

      tkI_library,
      tkI_initialization,
      tkI_finalization
  );

  TTokenKindSet = set of TTokenKind;

const
  tkSymbolLevel2 = tkGreaterEqualThan;
  tkIdentStrictReserved = tkI_if;
  tkLast = High(TTokenKind);
  tkDot = tkQualifier;

type
  TDelphiLexer = class;

  TToken = class(TObject)
  private
    FTokenIndex: Integer;
    FKind: TTokenKind;
    FValue: string;
    FColumn: Integer;
    FLine: Integer;
    FIndex: Integer;
    FFilename: string;
    FTokenLength: Integer;
    function GetEndIndex: Integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  protected
    procedure OffsetToken(OffsetTokenIndex, OffsetIndex, OffsetColumn, OffsetLine: Integer);
  public
    constructor Create(ATokenIndex: Integer; AKind: TTokenKind; const AValue, AFilename: string;
      ALine, AColumn, AIndex, ATokenLength: Integer);
    //class function NewInstance: TObject; override;
    //procedure FreeInstance; override;

    function IsIdent: Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function IsFreeUsableIdent: Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
    function IsTypeIdent: Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    function GetCommentBody: string;

    property Kind: TTokenKind read FKind;
    property Value: string read FValue; // WARNING: Index + Length(Value) <> EndIndex

    property Line: Integer read FLine; // 0..n-1
    property Column: Integer read FColumn; // 1..m
    property Index: Integer read FIndex; // 1..k
    property Filename: string read FFilename;
    property TokenIndex: Integer read FTokenIndex; // 0..j-1
    property TokenLength: Integer read FTokenLength;
    property EndIndex: Integer read GetEndIndex; // 1..k
  end;

  { Delphi Lexer. There must not be a BOM in the Text.
    Delphi 2009 or newer: Text must be encoded in UTF8.
    Delphi 5-2007: Text must be encoded in ANSI }
  TDelphiLexer = class(TObject)
  private
    FFilename: string;
    FText: UTF8String;
    FStartLine: Integer;
    FLine: Integer;
    FStartColumn: Integer;
    FColumn: Integer;
    FIndex: Integer;

    FTokens: TObjectList;
    FCurTokenIndex: Integer;
    FModified: Boolean;
    FSupportMacroTokens: Boolean;
    FPreprocMode: Boolean;
    function GetCount: Integer;
    function GetTokenProp(Index: Integer): TToken;
    procedure DiscardTokens(StartIndex: Integer);
    function GetCurrentToken: TToken;
    function GetPreviousToken: TToken;
    function CountSpaces(StartIndex: Integer): Integer;
  public
    constructor Create(const AFilename: string; const AText: UTF8String;
      AOwnsTokens: Boolean = True; AStartLine: Integer = 0; AStartColumn: Integer = 1);
    destructor Destroy; override;

    { Low Level }
    function GetToken: TToken; overload;
    function GetToken(out Token: TToken): Boolean; overload;

    { High Level }
    procedure RestartLexer;
    procedure RewindLastToken;
    function NextToken: TToken; overload;
    function NextToken(out Token: TToken): Boolean; overload;
    function NextTokenNoComment: TToken; overload;
    function NextTokenNoComment(out Token: TToken): Boolean; overload;
    function PreTokenOf(Token: TToken): TToken;
    function PreTokenNoCommentOf(Token: TToken): TToken;
    function NextTokenOf(Token: TToken): TToken;
    function NextTokenNoCommentOf(Token: TToken): TToken;
    function GetFullStringNext: string; // concates the current token string and all following string concatinations. Leave: next token after last string token
    function GetCombinedStringConstantNext(var CombinedString: string): Integer; // If a string is concatenated like 'a'#13#10 the function return "'a'#13#10', Leave: next token after last string token
    function LookAhead: TToken;
    function LookAheadNoComment: TToken;

    function ParseIdentifierName: string;
    function ParseIfdefToken: TToken; // allows "7ZIP_LINKONREQUEST" as Ident

    { Modification }
    procedure DeleteToken(Token: TToken; DeleteEndSpaces: Boolean = False);
    procedure DeleteTokens(StartToken, EndToken: TToken; DeleteEndSpaces: Boolean = False);
    procedure InsertTextAfter(Token: TToken; const Text: UTF8String); overload;
    procedure InsertTextAfter(Token: TToken; const Text: string); overload;
    procedure ReplaceToken(Token: TToken; const Text: UTF8String); overload;
    procedure ReplaceToken(Token: TToken; const Text: string); overload;
    { Index is 1-based }
    procedure DeleteText(StartIndex, Len: Integer);
    procedure InsertText(StartIndex: Integer; const Text: UTF8String); overload;
    procedure InsertText(StartIndex: Integer; const Text: string); overload;
    procedure ReplaceText(StartIndex, Len: Integer; const Text: UTF8String); overload;
    procedure ReplaceText(StartIndex, Len: Integer; const Text: string); overload;


    property Index: Integer read FIndex;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    property Text: UTF8String read FText;
    property Filename: string read FFilename;

      // already parsed tokens
    property Count: Integer read GetCount;
    property Tokens[Index: Integer]: TToken read GetTokenProp;
    property Modified: Boolean read FModified;
    property PreviousToken: TToken read GetPreviousToken;
    property CurrentToken: TToken read GetCurrentToken;

    property SupportMacroTokens: Boolean read FSupportMacroTokens write FSupportMacroTokens;
  end;

type
  TBOMType = (bomAnsi, bomUtf8, bomUcs2BE, bomUcs2LE, bomUcs4BE, bomUcs4LE);

  {$IFDEF CONDITIONALEXPRESSIONS}
   {$IF not declared(TBytes)}
  TBytes = array of Byte;
   {$IFEND}
  {$ELSE}
  TBytes = array of Byte;
  {$ENDIF}

  TTextFileReader = class(TObject)
  private
    FOwnStream: Boolean;
    FStream: TStream;
    FBuffer: TBytes;
    FBufStart: Integer;
    FBufCount: Integer;
    FBOMType: TBOMType;
    FMaxReadBytes: Cardinal;
    FBytesRead: Cardinal;

    procedure ReadBOM;
    procedure FillBuffer;
    function GetEof: Boolean;
    function DataToString(const Data: TBytes): string;
    function DataToUtf8String(const Data: TBytes): UTF8String;
    function InternReadAll: TBytes;
    function InternReadLine: TBytes;
  public
    constructor Create(const AFileName: string; ABufferSize: Integer = 4096); overload;
    constructor Create(AStream: TStream; ABufferSize: Integer = 4096; AOwnStream: Boolean = False); overload;
    destructor Destroy; override;

    function ReadAll: string;
    function ReadLine: string;
    function Utf8ReadAll: UTF8String;
    function Utf8ReadLine: UTF8String;

    property MaxReadBytes: Cardinal read FMaxReadBytes write FMaxReadBytes;
    property Eof: Boolean read GetEof;
    property BOMType: TBOMType read FBOMType;
  end;

function TokenKindToString(Kind: TTokenKind): string;
function LoadTextFileToString(const Filename: string; MaxReadBytes: Cardinal = 0): string;
function LoadTextFileToUtf8String(const Filename: string; MaxReadBytes: Cardinal): UTF8String; overload;
function LoadTextFileToUtf8String(const Filename: string; MaxReadBytes: Cardinal; out Encoding: TEncoding): UTF8String; overload;
function LoadTextFileToUtf8String(const Filename: string): UTF8String; overload;
function LoadTextFileToUtf8String(const Filename: string; out Encoding: TEncoding): UTF8String; overload;
function Mangle(const Name: string): string;
function IsEndIfToken(Token: TToken): Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function AsciiStartsText(const SubStr, S: string): Boolean;

implementation

{$IFDEF UNICODE}
uses
  WideStrUtils, Character;
{$ENDIF UNICODE}

resourcestring
  RsIdentifier = 'Identifier';
  RsString = 'String';
  RsInteger = 'Integer';
  RsFloat = 'Float number';

type
  TLexerTokenStringList = class(TStringList)
  protected
    function CompareStrings(const S1: string; const S2: string): Integer; override;
  end;

function TLexerTokenStringList.CompareStrings(const S1: string; const S2: string): Integer;
begin
  Result := CompareText(S1, S2);
end;

var
  TokenStrings: TLexerTokenStringList;

procedure InitTokenStrings;
var
  Kind: TTokenKind;
  Info: PTypeInfo;
begin
  TokenStrings := TLexerTokenStringList.Create;

  Info := TypeInfo(TTokenKind);
  for Kind := Succ(tkIdent) to High(TTokenKind) do
    TokenStrings.AddObject(
      Copy(GetEnumName(Info, Ord(Kind)), 5, MaxInt),
      TObject(Integer(Kind))
    );

  TokenStrings.Sorted := True;
end;

function FindTokenIdentKind(const Value: string): TTokenKind;
var
  Index: Integer;
begin
  if TokenStrings.Find(Value, Index) then
    Result := TTokenKind(Integer(TokenStrings.Objects[Index]))
  else
    Result := tkIdent;
end;

function TokenKindToString(Kind: TTokenKind): string;
begin
  if Kind > tkIdent then
    Result := AnsiUpperCase(Copy(GetEnumName(TypeInfo(TTokenKind), Integer(Kind)), 5, MaxInt))
  else
  begin
    case Kind of
      tkEqual: Result := '=';
      tkGreaterThan: Result := '>';
      tkLessThan: Result := '<';
      tkMinus: Result := '-';
      tkPlus: Result := '+';
      tkMultiply: Result := '*';
      tkDivide: Result := '/';

      tkLParan: Result := '(';
      tkRParan: Result := ')';
      tkLBracket: Result := '[';
      tkRBracket: Result := ']';
      tkColon: Result := ':';
      tkSemicolon: Result := ';';
      tkComma: Result := ',';
      tkPointer: Result := '^';
      tkAddr: Result := '@';
      tkAmp: Result := '&';
      tkQualifier: Result := '.';

      tkGreaterEqualThan: Result := '>=';
      tkLessEqualThan: Result := '<=';
      tkNotEqual: Result := '<>';
      tkRange: Result := '..';
      tkAssign: Result := ':=';

    {tkComment: Result := RsComment;
    tkDirective: Result := RsCompilerDirective}

      tkIdent: Result := RsIdentifier;
      tkString: Result := RsString;
      tkInt: Result := RsInteger;
      tkFloat: Result := RsFloat;
    end;
    Result := '' + Result + '';
  end;
end;

{$IFDEF UNICODE}
procedure SetStringFromUtf8(var S: string; P: PAnsiChar; Len: Integer);
begin
  SetLength(S, Len);
  Len := Utf8ToUnicode(PWideChar(S), Len + 1, P, Len);
  if Len <= 0 then
    S := ''
  else if Length(S) <> Len - 1 then
    SetLength(S, Len - 1);
end;
{$ENDIF UNICODE}

function LoadTextFileToString(const Filename: string; MaxReadBytes: Cardinal = 0): string;
var
  Reader: TTextFileReader;
begin
  Reader := TTextFileReader.Create(Filename);
  try
    Reader.MaxReadBytes := MaxReadBytes;
    Result := Reader.ReadAll;
  finally
    Reader.Free;
  end;
end;

function LoadTextFileToUtf8String(const Filename: string; MaxReadBytes: Cardinal): UTF8String;
var
  Encoding: TEncoding;
begin
  Result := LoadTextFileToUtf8String(FileName, MaxReadBytes, Encoding);
end;

function LoadTextFileToUtf8String(const Filename: string; MaxReadBytes: Cardinal; out Encoding: TEncoding): UTF8String; overload;
var
  Reader: TTextFileReader;
begin
  Reader := TTextFileReader.Create(Filename);
  try
    if MaxReadBytes > 0 then
      Reader.MaxReadBytes := MaxReadBytes;
    Result := Reader.Utf8ReadAll;

    Encoding := TEncoding.Default;
    case Reader.BOMType of
      //bomAnsi: Encoding := TEncoding.Default;
      bomUtf8: Encoding := TEncoding.UTF8;
      bomUcs2BE: Encoding := TEncoding.BigEndianUnicode;
      bomUcs2LE: Encoding := TEncoding.Unicode;
      bomUcs4BE: Encoding := TEncoding.BigEndianUnicode;
      bomUcs4LE: Encoding := TEncoding.Unicode;
    end;
  finally
    Reader.Free;
  end;
end;

function LoadTextFileToUtf8String(const Filename: string): UTF8String;
var
  Encoding: TEncoding;
begin
  Result := LoadTextFileToUtf8String(Filename, 0, Encoding);
end;

function LoadTextFileToUtf8String(const Filename: string; out Encoding: TEncoding): UTF8String; overload;
begin
  Result := LoadTextFileToUtf8String(FileName, 0, Encoding);
end;

function AsciiStartsText(const SubStr, S: string): Boolean;
var
  Len: Integer;
begin
  Len := Length(SubStr);
  if Len <= Length(S) then
    Result := StrLIComp(PChar(SubStr), PChar(S), Length(SubStr)) = 0
  else
    Result := False;
end;

function Mangle(const Name: string): string;
var
  P: PChar;
begin
  Result := IntToStr(Length(Name)) + Name;
  P := PChar(Result);
  while P[0] <> #0 do
  begin
    if P[0] = '.' then
      P[0] := '_';
    Inc(P);
  end;
end;

function IsEndIfToken(Token: TToken): Boolean;
begin
  Result := (Token.Kind = tkDirective) and
            (AsciiStartsText('{$ENDIF', Token.Value) or AsciiStartsText('{$IFEND', Token.Value) or
             AsciiStartsText('(*$ENDIF', Token.Value) or AsciiStartsText('(*$IFEND', Token.Value));
end;

function FindLineStart(const S: UTF8String; Index: Integer): Integer;
begin
  Result := Index;
  while (Result > 0) do
  begin
    case S[Result] of
      #10, #13:
        Break;
    end;
    Dec(Result);
  end;
  Inc(Result);
end;

{$IFDEF UNICODE}
function IsUtf8Letter(P: PUTF8Char; Index: Integer): Boolean;
var
  C: array[0..2] of WideChar;
begin
  Utf8ToUnicode(@C, Length(C), P, UTF8CharSize(P[Index]));
  {$IF CompilerVersion >= 25.0}
  Result := C[0].IsLetter;
  {$ELSE}
  Result := Character.IsLetter(C[0]);
  {$IFEND}
end;

function IsUtf8LetterOrDigit(P: PUTF8Char; Index: Integer): Boolean;
var
  C: array[0..2] of WideChar;
begin
  Utf8ToUnicode(@C, Length(C), P, UTF8CharSize(P[Index]));
  {$IF CompilerVersion >= 25.0}
  Result := C[0].IsLetterOrDigit;
  {$ELSE}
  Result := Character.IsLetterOrDigit(C[0]);
  {$IFEND}
end;
{$ENDIF UNICODE}

{ TTextFileReader }

constructor TTextFileReader.Create(const AFileName: string; ABufferSize: Integer);
begin
  Create(TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite), ABufferSize, True);
end;

constructor TTextFileReader.Create(AStream: TStream; ABufferSize: Integer; AOwnStream: Boolean);
begin
  inherited Create;
  FOwnStream := AOwnStream;
  FStream := AStream;

  if ABufferSize < 16 then
    ABufferSize := 16;
  SetLength(FBuffer, ABufferSize);

  ReadBOM;

  { Adjust buffer size to match the char size }
  case BOMType of
    bomUcs2BE, bomUcs2LE:
      if ABufferSize mod 2 <> 0 then
        SetLength(FBuffer, ABufferSize + 1);
    bomUcs4BE, bomUcs4LE:
      if ABufferSize mod 4 <> 0 then
        SetLength(FBuffer, ABufferSize + (4 - (ABufferSize mod 4)));
  end;
end;

destructor TTextFileReader.Destroy;
begin
  if FOwnStream then
    FStream.Free;
  inherited Destroy;
end;

procedure TTextFileReader.FillBuffer;
var
  BufSize: Integer;
begin
  if FBufStart >= FBufCount then
  begin
    FBufStart := 0;
    BufSize := Length(FBuffer);
    if FMaxReadBytes > 0 then
    begin
      if FBytesRead + Cardinal(BufSize) > FMaxReadBytes then
        BufSize := FMaxReadBytes - FBytesRead;
      if BufSize <= 0 then
      begin
        FBufCount := 0;
        Exit;
      end;
    end;
    FBufCount := FStream.Read(FBuffer[0], BufSize);
    Inc(FBytesRead, FBufCount);
  end;
end;

function TTextFileReader.GetEof: Boolean;
begin
  Result := FBufCount = 0;
end;

function TTextFileReader.DataToString(const Data: TBytes): string;

  procedure SwapBytes(Data: TBytes);
  var
    I: Integer;
    b: Byte;
  begin
    for I := 0 to Length(Data) div 2 - 1 do
    begin
      b := Data[I * 2];
      Data[I * 2] := Data[I * 2 + 1];
      Data[I * 2 + 1] := b;
    end;
  end;

var
  A: AnsiString;
  {$IFDEF COMPILER6_UP}
  W: UnicodeString;
  UCS4: UCS4String;
  {$ENDIF COMPILER6_UP}
begin
  Result := '';
  if Data = nil then
    Exit;
  case BOMType of
    bomAnsi:
      begin
        SetString(A, PAnsiChar(@Data[0]), Length(Data));
        Result := string(A);
      end;
    {$IFDEF COMPILER6_UP}
    bomUtf8:
      begin
        SetString(A, PAnsiChar(@Data[0]), Length(Data));
        Result := Utf8ToAnsi(A);
      end;
    bomUcs2BE:
      begin
        SwapBytes(Data);
        SetString(W, PWideChar(@Data[0]), Length(Data));
        Result := W;
      end;
    bomUcs2LE:
      begin
        SetString(W, PWideChar(@Data[0]), Length(Data));
        Result := W;
      end;
    bomUcs4BE, bomUcs4LE:
      begin
        if BOMType = bomUcs4BE then
          SwapBytes(Data);
        SetLength(UCS4, Length(Data) div SizeOf(UCS4Char));
        Move(UCS4[0], Data[0], Length(Data));
        Result := UCS4StringToWideString(UCS4);
      end;
    {$ENDIF COMPILER6_UP}
  end;
end;

function TTextFileReader.DataToUtf8String(const Data: TBytes): UTF8String;
begin
  if Data = nil then
    Exit;
  if BOMType = bomUtf8 then
    SetString(Result, PAnsiChar(@Data[0]), Length(Data))
  else
    Result := UTF8Encode(DataToString(Data));
end;

function TTextFileReader.InternReadAll: TBytes;
var
  Len: Integer;
begin
  SetLength(Result, FStream.Size - (FStream.Position - FBufCount));
  FillBuffer;
  Len := 0;
  while FBufCount > 0 do
  begin
    Move(FBuffer[FBufStart], Result[Len], FBufCount);
    Inc(Len, FBufCount);
    FBufStart := FBufCount;
    FillBuffer;
  end;
  if Len <> Length(Result) then
    SetLength(Result, Len);
end;

function TTextFileReader.InternReadLine: TBytes;
var
  Start: Integer;
  Len, AddLen: Integer;

  procedure AppendData;
  begin
    AddLen := (FBufStart - Start);
    SetLength(Result, Len + AddLen);
    Move(FBuffer[Start], Result[Len], AddLen);
    Inc(Len, AddLen);
  end;

begin
  FillBuffer;
  if FBufCount = 0 then
  begin
    Result := nil;
    Exit;
  end;

  Len := 0;
  Start := FBufStart;

  if BOMType in [bomAnsi, bomUtf8] then
  begin
    while FBufCount > 0 do
    begin
      if FBuffer[FBufStart] in [10, 13] then
      begin
        AppendData;

        { Skip line break }
        if FBuffer[FBufStart] = 13 then // => leading byte of #13#10
        begin
          Inc(FBufStart);
          if FBufStart >= FBufCount then
            FillBuffer;
        end;
        if FBuffer[FBufStart] = 10 then
        begin
          Inc(FBufStart);
          if FBufStart >= FBufCount then
            FillBuffer;
        end;

        Break;
      end;
      Inc(FBufStart);
      if FBufStart >= FBufCount then
      begin
        AppendData;
        FillBuffer;
        Start := FBufStart;
      end;
    end;
  end
  else
    raise Exception.Create('Only ANSI and UTF8 encodings are supported');
end;

function TTextFileReader.ReadAll: string;
begin
  Result := DataToString(InternReadAll);
end;

function TTextFileReader.ReadLine: string;
begin
  Result := DataToString(InternReadLine);
end;

function TTextFileReader.Utf8ReadAll: UTF8String;
begin
  Result := DataToUtf8String(InternReadAll);
end;

function TTextFileReader.Utf8ReadLine: UTF8String;
begin
  Result := DataToUtf8String(InternReadLine);
end;

procedure TTextFileReader.ReadBOM;
begin
  Assert(Length(FBuffer) >= 4);

  FBOMType := bomAnsi;
  FBufCount := FStream.Read(FBuffer[0], Length(FBuffer));
  if FBufCount >= 2 then
  begin
    if (FBuffer[0] = $EF) and (FBuffer[1] = $BB) then
    begin
      if FBuffer[2] = $BF then
      begin
        FBOMType := bomUtf8;
        FBufStart := 3;
      end;
    end
    else if (FBuffer[0] = $FF) and (FBuffer[1] = $FE) then
    begin
      if (FBuffer[2] = 0) and (FBuffer[3] = 0) then
      begin
        FBOMType := bomUcs4LE;
        FBufStart := 4;
      end
      else
      begin
        FBOMType := bomUcs2LE;
        FBufStart := 2;
      end;
    end
    else if (FBuffer[0] = $FE) and (FBuffer[1] = $FF) then
    begin
      FBOMType := bomUcs2BE;
      FBufStart := 2;
    end
    else if (FBuffer[0] = 0) and (FBuffer[1] = 0) then
    begin
      if (FBuffer[2] = $FE) and (FBuffer[3] = $FF) then
      begin
        FBOMType := bomUcs4BE;
        FBufStart := 4;
      end;
    end;
  end;

  Dec(FBufCount, FBufStart);
end;


{ TToken }

constructor TToken.Create(ATokenIndex: Integer; AKind: TTokenKind;
  const AValue, AFilename: string; ALine, AColumn, AIndex, ATokenLength: Integer);
begin
  inherited Create;
  FTokenIndex := ATokenIndex;
  FKind := AKind;
  FValue := AValue;
  FLine := ALine;
  FColumn := AColumn;
  FIndex := AIndex;
  FTokenLength := ATokenLength;
  FFilename := AFilename;
end;

{procedure TToken.FreeInstance;
begin
  inherited FreeInstance;
  //CleanupInstance;
end;

class function TToken.NewInstance: TObject;
begin
  Result := inherited NewInstance;
end;}

function TToken.IsIdent: Boolean;
begin
  Result := Kind >= tkIdent;
end;

function TToken.IsTypeIdent: Boolean;
begin
  Result := (Kind >= tkIdent) and ((Kind < tkIdentStrictReserved) or (Kind = tkI_string));
end;

function TToken.GetCommentBody: string;
begin
  if Kind = tkComment then
  begin
    if Value[1] = '{' then
      Result := Copy(Value, 2, Length(Value) - 2)
    else
    if Value[1] = '(' then
      Result := Copy(Value, 3, Length(Value) - 4)
    else if Value[1] = '/' then
      Result := Copy(Value, 3, MaxInt)
    else
      Result := '';
  end
  else
    Result := '';
end;

function TToken.GetEndIndex: Integer;
begin
  Result := Index + TokenLength - 1;
end;

function TToken.IsFreeUsableIdent: Boolean;
begin
  Result := (Kind >= tkIdent) and (Kind < tkIdentStrictReserved);
end;

procedure TToken.OffsetToken(OffsetTokenIndex, OffsetIndex, OffsetColumn, OffsetLine: Integer);
begin
  Inc(FTokenIndex, OffsetTokenIndex);
  Inc(FColumn, OffsetColumn);
  Inc(FLine, OffsetLine);
  Inc(FIndex, OffsetIndex);
end;

{ TDelphiLexer }

constructor TDelphiLexer.Create(const AFilename: string; const AText: UTF8String;
  AOwnsTokens: Boolean; AStartLine: Integer; AStartColumn: Integer);
begin
  inherited Create;
  FTokens := TObjectList.Create(AOwnsTokens);
  FFilename := AFilename;
  FText := AText;
  FStartLine := AStartLine;
  FLine := AStartLine;
  FIndex := 1;
  if AStartColumn < 1 then
    AStartColumn := 1;
  FStartColumn := AStartColumn;
  FColumn := AStartColumn;
end;

destructor TDelphiLexer.Destroy;
begin
  FTokens.Free;
  inherited Destroy;
end;

function TDelphiLexer.GetTokenProp(Index: Integer): TToken;
begin
  Result := TToken(FTokens[Index]);
end;

function TDelphiLexer.GetCount: Integer;
begin
  Result := FTokens.Count;
end;

function TDelphiLexer.GetToken(out Token: TToken): Boolean;
begin
  Token := GetToken;
  Result := Token <> nil;
end;

function TDelphiLexer.CountSpaces(StartIndex: Integer): Integer;
var
  Len: Integer;
begin
  Len := Length(FText);
  Result := 0;
  while (StartIndex <= Len) and (FText[StartIndex] in [#9, ' ']) do
  begin
    Inc(Result);
    Inc(StartIndex);
  end;
end;

procedure TDelphiLexer.DeleteToken(Token: TToken; DeleteEndSpaces: Boolean);
var
  TokenIndex, Len: Integer;
  I: Integer;
  Tk: TToken;
  Line: Integer;
begin
  if Token <> nil then
  begin
    { There is no line break change by deleting one token }
    Len := Token.TokenLength;
    if DeleteEndSpaces then
      Inc(Len, CountSpaces(Token.Index + Len));
    System.Delete(FText, Token.Index, Len);
    TokenIndex := Token.TokenIndex;
    Line := Token.Line;
    for I := TokenIndex + 1 to FTokens.Count - 1 do
    begin
      Tk := TToken(FTokens[I]);
      if Tk.Line = Line then
        Tk.OffsetToken(-1, -Len, 0, 0)
      else
        Tk.OffsetToken(-1, -Len, -Len, 0);
    end;
    FTokens.Delete(TokenIndex);
    if FCurTokenIndex > 0 then
      Dec(FCurTokenIndex);
    FModified := True;
  end;
end;

procedure TDelphiLexer.DeleteTokens(StartToken, EndToken: TToken; DeleteEndSpaces: Boolean);
var
  I: Integer;
  StartIndex, EndIndex: Integer;
  StartTokenIndex, EndTokenIndex: Integer;
  OffsetLine, Len: Integer;
  TokenDeleteCount: Integer;
  Tk: TToken;
  LineStartIndex: Integer;
begin
  if (StartToken <> nil) or (EndToken <> nil) then
  begin
    if StartToken = nil then
      DeleteToken(EndToken, DeleteEndSpaces)
    else if EndToken = nil then
      DeleteToken(StartToken, DeleteEndSpaces)
    else
    begin
      StartIndex := StartToken.Index;
      EndIndex := EndToken.Index + EndToken.TokenLength;
      if DeleteEndSpaces then
        Inc(EndIndex, CountSpaces(EndIndex));

      Delete(FText, StartIndex, EndIndex - StartIndex);
      StartTokenIndex := StartToken.TokenIndex;
      EndTokenIndex := EndToken.TokenIndex;
      TokenDeleteCount := EndTokenIndex - StartTokenIndex + 1;
      Len := EndIndex - StartIndex;

      LineStartIndex := FindLineStart(FText, EndToken.Index - 1);
      if LineStartIndex < StartIndex then
        LineStartIndex := StartIndex;

      OffsetLine := EndToken.Line - StartToken.Line;
      Dec(FLine, OffsetLine);
      for I := EndTokenIndex + 1 to FTokens.Count - 1 do
      begin
        Tk := TToken(FTokens[I]);
        if Tk.Line = Line then
          Tk.OffsetToken(-TokenDeleteCount, -Len, -(EndIndex - LineStartIndex), -OffsetLine)
        else
          Tk.OffsetToken(-TokenDeleteCount, -Len, 0, -OffsetLine);
      end;
      for I := EndTokenIndex downto StartTokenIndex do
        FTokens.Delete(I);

      if FCurTokenIndex >= TokenDeleteCount then
        Dec(FCurTokenIndex, TokenDeleteCount);
      FModified := True;
    end;
  end;
end;

procedure TDelphiLexer.InsertTextAfter(Token: TToken; const Text: UTF8String);
begin
  InsertText(Token.EndIndex + 1, Text);
end;

procedure TDelphiLexer.InsertTextAfter(Token: TToken; const Text: string);
begin
  InsertText(Token.EndIndex + 1, Text);
end;

procedure TDelphiLexer.ReplaceToken(Token: TToken; const Text: UTF8String);
var
  Index: Integer;
begin
  Index := Token.Index;
  DeleteToken(Token);
  InsertText(Index, Text);
end;

procedure TDelphiLexer.ReplaceToken(Token: TToken; const Text: string);
begin
  ReplaceToken(Token, UTF8Encode(Text));
end;

procedure TDelphiLexer.DiscardTokens(StartIndex: Integer);
var
  I: Integer;
  Tk: TToken;
begin
  if FTokens.Count > 0 then
  begin
    Tk := TToken(FTokens[0]);
    if StartIndex <= Tk.Index then
    begin
      FIndex := 1;
      FLine := FStartLine;
      FColumn := FStartColumn;
      FCurTokenIndex := 0;
      FTokens.Clear;
    end
    else
    begin
      for I := FTokens.Count - 1 downto 0 do
      begin
        Tk := TToken(FTokens[I]);
        if Tk.Index >= StartIndex then
          FTokens.Delete(I)
        else
        begin
          if Tk.Index + Tk.TokenLength > StartIndex then
          begin
            FIndex := Tk.Index;
            FLine := Tk.Line;
            FColumn := Tk.Column;
            FTokens.Delete(I);
          end
          else
          begin
            FIndex := Tk.Index + Tk.TokenLength;
            FColumn := Tk.Column + Tk.TokenLength;
            FLine := Tk.Line;
          end;
          if FCurTokenIndex > FTokens.Count then
            FCurTokenIndex := FTokens.Count;
          Break;
        end;
      end;
    end;
    FModified := True;
  end;
end;

procedure TDelphiLexer.DeleteText(StartIndex, Len: Integer);
begin
  if Len > 0 then
  begin
    Delete(FText, StartIndex, Len);
    DiscardTokens(StartIndex);
    FModified := True;
  end;
end;

procedure TDelphiLexer.InsertText(StartIndex: Integer; const Text: UTF8String);
begin
  if Text <> '' then
  begin
    Insert(Text, FText, StartIndex);
    DiscardTokens(StartIndex);
    FModified := True;
  end;
end;

procedure TDelphiLexer.InsertText(StartIndex: Integer; const Text: string);
begin
  InsertText(StartIndex, UTF8Encode(Text));
end;

procedure TDelphiLexer.ReplaceText(StartIndex, Len: Integer; const Text: UTF8String);
begin
  if (Len > 0) and (Text = '') then
    DeleteText(StartIndex, Len)
  else if (Len = 0) and (Text <> '') then
    InsertText(StartIndex, Text)
  else
  begin
    Delete(FText, StartIndex, Len);
    Insert(Text, FText, StartIndex);

    DiscardTokens(StartIndex);
    FModified := True;
  end;
end;

procedure TDelphiLexer.ReplaceText(StartIndex, Len: Integer; const Text: string);
begin
  ReplaceText(StartIndex, Len, UTF8Encode(Text));
end;

procedure TDelphiLexer.RestartLexer;
begin
  FCurTokenIndex := 0;
end;

procedure TDelphiLexer.RewindLastToken;
begin
  if FCurTokenIndex > 0 then
    Dec(FCurTokenIndex);
end;

function TDelphiLexer.LookAhead: TToken;
var
  Idx: Integer;
begin
  Idx := FCurTokenIndex;
  Result := NextToken;
  FCurTokenIndex := Idx;
end;

function TDelphiLexer.LookAheadNoComment: TToken;
var
  Idx: Integer;
begin
  Idx := FCurTokenIndex;
  Result := NextTokenNoComment;
  FCurTokenIndex := Idx;
end;

function TDelphiLexer.NextToken: TToken;
begin
  Result := nil;
  while FTokens.Count <= FCurTokenIndex do
  begin
    Result := GetToken;
    if Result = nil then
      Break;
  end;
  if FCurTokenIndex < FTokens.Count then
  begin
    Result := TToken(FTokens[FCurTokenIndex]);
    Inc(FCurTokenIndex);
  end;
end;

function TDelphiLexer.NextToken(out Token: TToken): Boolean;
begin
  Token := NextToken;
  Result := Token <> nil;
end;

function TDelphiLexer.NextTokenNoComment: TToken;
begin
  repeat
    Result := NextToken;
    if (Result <> nil) and (Result.Kind = tkDirective) then
    begin
      if AsciiStartsText(Result.Value, '{INCLUDE ') or
         AsciiStartsText(Result.Value, '{I ') or
         AsciiStartsText(Result.Value, '(*INCLUDE ') or
         AsciiStartsText(Result.Value, '(*I ') then
        Break;
    end;
  until (Result = nil) or not (Result.Kind in [tkComment, tkDirective]);
end;

function TDelphiLexer.NextTokenNoComment(out Token: TToken): Boolean;
begin
  Token := NextTokenNoComment;
  Result := Token <> nil;
end;

function TDelphiLexer.ParseIdentifierName: string;
var
  Token: TToken;
begin
  Result := '';
  Token := CurrentToken;
  if Token <> nil then
  begin
    repeat
      while (Token <> nil) and (Token.Kind in [tkComment, tkDirective]) do
        Token := NextToken;
      if not Token.IsTypeIdent then
        Exit;
      Result := Result + Token.Value;
      Token := NextToken;
      while (Token <> nil) and (Token.Kind in [tkComment, tkDirective]) do
        Token := NextToken;

      if Token.Kind <> tkQualifier then
        Break;
      Result := Result + '.';
    until not NextTokenNoComment(Token);
  end;
  // leave: token after last identifier part
end;

function TDelphiLexer.ParseIfdefToken: TToken;
begin
  FPreprocMode := True;
  try
    Result := GetToken;
  finally
    FPreprocMode := False;
  end;
end;

function TDelphiLexer.PreTokenOf(Token: TToken): TToken;
begin
  Result := nil;
  if Token.TokenIndex > 0 then
    Result := Tokens[Token.TokenIndex - 1];
end;

function TDelphiLexer.PreTokenNoCommentOf(Token: TToken): TToken;
begin
  Result := Token;
  while Result.TokenIndex > 0 do
  begin
    Result := Tokens[Result.TokenIndex - 1];
    if Result.Kind <> tkComment then
      Exit;
  end;
  Result := nil;
end;

function TDelphiLexer.NextTokenOf(Token: TToken): TToken;
begin
  Result := nil;
  if Token.TokenIndex + 1 < Count then
    Result := Tokens[Token.TokenIndex + 1];
end;

function TDelphiLexer.NextTokenNoCommentOf(Token: TToken): TToken;
begin
  Result := Token;
  while Result.TokenIndex + 1 < Count do
  begin
    Result := Tokens[Result.TokenIndex + 1];
    if Result.Kind <> tkComment then
      Exit;
  end;
  Result := nil;
end;

function TDelphiLexer.GetCurrentToken: TToken;
begin
  if FCurTokenIndex - 1 < FTokens.Count then
    Result := TToken(FTokens[FCurTokenIndex - 1])
  else
    Result := nil;
end;

function TDelphiLexer.GetFullStringNext: string;
var
  Token: TToken;
  S: string;
begin
  Token := CurrentToken;
  if (Token <> nil) and (Token.Kind = tkString) then
  begin
    if GetCombinedStringConstantNext(Result) > 0 then
      RewindLastToken;

    while NextTokenNoComment(Token) and (Token.Kind = tkPlus) do
    begin
      Result := Result + ' + ';
      if NextTokenNoComment(Token) and ((Token.Kind = tkString) or Token.IsIdent) then
      begin
        if Token.Kind = tkString then
        begin
          if GetCombinedStringConstantNext(S) > 0 then
            RewindLastToken;
          Result := Result + S;
        end
        else
          Result := Result + Token.Value;
      end
      else
        Break;
    end;
  end;
end;

function TDelphiLexer.GetCombinedStringConstantNext(var CombinedString: string): Integer; // If a string is concatenated like 'a'#13#10 the function return "'a'#13#10', Leave: next token after last string token
var
  Token: TToken;
  LastEndIndex: Integer;
begin
  Result := 0;
  Token := CurrentToken;
  if (Token <> nil) and (Token.Kind = tkString) then
  begin
    CombinedString := Token.Value;
    Result := 1;

    { Handle "'a'#13#10" but not "#13 #10"}
    LastEndIndex := Token.EndIndex;
    while True do
    begin
      Token := NextToken;
      if (Token = nil) or (Token.Kind <> tkString) or (Token.Value[1] <> '#') or
         (Token.Index <> LastEndIndex + 1) then
      begin
        Break;
      end;
      Inc(Result);
      CombinedString := CombinedString + Token.Value;
      LastEndIndex := Token.EndIndex;
    end;
  end;
end;

function TDelphiLexer.GetPreviousToken: TToken;
begin
  if (FCurTokenIndex > 0) and (FCurTokenIndex - 1 - 1 < FTokens.Count) then
    Result := TToken(FTokens[FCurTokenIndex - 1 - 1])
  else
    Result := nil;
end;

function TDelphiLexer.GetToken: TToken;
var
  Data, P, F: PUTF8Char;
  ch1, ch2: UTF8Char;
  S: UTF8String;
  TokenValue: string;

  CharSize: Integer;
  IndexAdd: Integer;
  IsDecimal: Boolean;
  IsExp: Boolean;
  IsExpSign: Boolean;
  StartLine, StartColumn, Line, Column: Integer;
  Kind: TTokenKind;
  StartIndex: Integer;
begin
  Result := nil;
  if FIndex > Length(FText) then
    Exit;
  Data := Pointer(Text);
  P := Data + FIndex - 1;

  // skip white chars
  Column := FColumn;
  Line := FLine;
  ch1 := P[0];
  while ch1 in WhiteChars do
  begin
    if ch1 = #10 then
    begin
      Inc(Line);
      Column := 0;
    end;
    Inc(P);
    ch1 := P[0];
    if ch1 <> #13 then
      Inc(Column);
  end;

  if ch1 = #0 then
  begin
    FLine := Line;
    FColumn := Column;
    Exit;
  end;

  StartLine := Line;
  StartColumn := Column;
  StartIndex := P - Data + 1;

  F := P;
  IndexAdd := 0;
  if ch1 = '''' then
  begin
    Inc(P);
    Inc(Column);
    // string
    while True do
    begin
      case P[0] of
        #0:
          Break;
        '''':
          begin
            if P[1] = '''' then
              Inc(P)
            else
              Break;
          end;
        #10, #13:
          begin
            Dec(P);
            Column := 0;
            Break; // line end is string end in pascal
          end;
      end;
      Inc(P);
      Inc(Column);
    end;
    if P[0] <> #0 then
    begin
      Inc(P); // include P[0] which is now P[-1]
      Inc(Column);
    end;
    Kind := tkString;
  end
  else if ch1 = '{' then
  begin
    // comment { ... } -> find comment end
    Inc(P);
    Inc(Column);
    if P[0] = '$' then
    begin
      Kind := tkDirective;
      Inc(P);
      Inc(Column);
    end
    else
      Kind := tkComment;

    while True do
    begin
      case P[0] of
        #0, '}':
          Break;
        #10:
          begin
            Inc(Line);
            Column := 0;
          end;
      end;
      Inc(P);
      if P[0] <> #13 then
        Inc(Column);
    end;

    if P[0] <> #0 then
    begin
      Inc(P); // include P[0] which is now P[-1]
      Inc(Column);
    end;
  end
  else if (ch1 = '(') and (P[1] = '*') then
  begin
    // comment (* ... *) -> find comment end
    Inc(P, 2);
    Inc(Column, 2);
    if P[0] = '$' then
    begin
      Kind := tkDirective;
      Inc(P);
      Inc(Column);
    end
    else
      Kind := tkComment;

    ch1 := P[0];
    while (ch1 <> #0) and not ((ch1 = '*') and (P[1] = ')')) do
    begin
      if ch1 = #10 then
      begin
        Inc(Line);
        Column := 0;
      end;
      Inc(P);
      ch1 := P[0];
      if ch1 <> #13 then
        Inc(Column);
    end;

    if ch1 <> #0 then
    begin
      Inc(P, 2); // include P[0],P[1] which is now P[-2],P[-1]
      Inc(Column, 2);
    end;
  end
  else if (ch1 = '/') and (P[1] = '/') then
  begin
    // comment "// ..." -> find comment end
    Inc(P, 2);
    Inc(Column, 2);
    while not (P[0] in [#0, #10, #13]) do
    begin
      Inc(P);
      Inc(Column);
    end;
    Kind := tkComment;
    if P[0] <> #0 then
    begin
      if P[0] = #13 then
        Inc(IndexAdd); {do not parse the #13 again}
      Inc(Line);
      Inc(IndexAdd); {do not parse the #10 again}
      Column := 1;
    end;
  end
  else if (ch1 in IdentFirstChars) {$IFDEF UNICODE}or ((P[0] >= #128) and IsUtf8Letter(P, 0)){$ENDIF}
         or (FPreprocMode and (ch1 in IdentChars)) then
  begin
    // identifier
    CharSize := UTF8CharLength(ch1);
    Inc(P, CharSize);
    Inc(Column, CharSize);

    while (P[0] in IdentChars) {$IFDEF UNICODE}or ((P[0] >= #128) and IsUtf8LetterOrDigit(P, 0)){$ENDIF} do
    begin
      if Byte(P[0]) and $80 > 0 then
      begin
        CharSize := UTF8CharLength(P[0]);
        Inc(P, CharSize);
        Inc(Column, CharSize);
      end
      else
      begin
        Inc(P);
        Inc(Column);
      end;
    end;

    Kind := tkIdent;
  end
  else if ch1 in NumberChars then
  begin
    // number
    Inc(P);
    Inc(Column);

    IsDecimal := False;
    IsExp := False;
    IsExpSign := False;
    repeat
      case P[0] of
        '0'..'9': ;

        '.':
          begin
            if P[1] = '.' then  // '..' "range symbol"
              Break;
            if IsDecimal or IsExp then
              Break
            else
              IsDecimal := True;
          end;

        '+', '-':
          if not IsExp or IsExpSign then
            Break
          else
            IsExpSign := True;

        'e', 'E':
          if IsDecimal or IsExp then
            Break
          else
            IsExp := True;

      else
        Break;
      end;
      Inc(P);
      Inc(Column);
    until False;
    if IsExp or IsDecimal then
      Kind := tkFloat
    else
      Kind := tkInt;
  end
  else if (ch1 = '$') and (P[1] in HexNumberChars) then
  begin
    // hex number
    Inc(P, 2);
    Inc(Column, 2);
    while P[0] in HexNumberChars do
    begin
      Inc(P);
      Inc(Column);
    end;
    Kind := tkInt;
  end
  else if SupportMacroTokens and (ch1 = '$') and (P[1] = '_') and (P[2] = '_') then // macro support
  begin
    // identifier
    Inc(P, 2);
    Inc(Column, 2);
    while P[0] in IdentChars do
    begin
      Inc(P);
      Inc(Column);
    end;
    SetString(S, F, P - F);
    Kind := tkMacro;
  end
  else if (ch1 = '#') and ((P[1] = '$') or (P[1] in NumberChars)) then
  begin
    // char
    Inc(P, 2);
    Inc(Column, 2);
    if P[-1] = '$' then
    begin
      while P[0] in HexNumberChars do
      begin
        Inc(P);
        Inc(Column);
      end;
    end
    else
    begin
      while P[0] in NumberChars do
      begin
        Inc(P);
        Inc(Column);
      end;
    end;
    Kind := tkString;
  end
  else {if ch1 in SymbolChars then}
  begin
    Inc(P);
    Inc(Column);
    ch2 := P[0];
    case ch1 of
      ';': Kind := tkSemicolon;
      ',': Kind := tkComma;
      '^': Kind := tkPointer;
      '@': Kind := tkAddr;
      '&': Kind := tkAmp;
      '[': Kind := tkLBracket;
      ']': Kind := tkRBracket;
      '+': Kind := tkPlus;
      '-': Kind := tkMinus;
      '*': Kind := tkMultiply;
      '/': Kind := tkDivide;
      '=': Kind := tkEqual;
      ':': if ch2 = '=' then Kind := tkAssign else Kind := tkColon;
      '<': if ch2 = '=' then
             Kind := tkLessEqualThan
           else if ch2 = '>' then
             Kind := tkNotEqual
           else
             Kind := tkLessThan;
      '>': if ch2 = '=' then Kind := tkGreaterEqualThan else Kind := tkGreaterThan;
      '.': if ch2 = '.' then
             Kind := tkRange
           else if ch2 = ')' then
           begin
             Kind := tkRBracket; // '.)' => ']'
             Inc(P);
             Inc(Column);
           end
           else
             Kind := tkQualifier;
      '(': if ch2 = '.' then
           begin
             Kind := tkLBracket; // '(.' => '['
             Inc(P);
             Inc(Column);
           end
           else
             Kind := tkLParan;
      ')': Kind := tkRParan;
    else
      Kind := tkSymbol;
    end;
    if Kind >= tkSymbolLevel2 then
    begin
      Inc(P);
      Inc(Column);
    end;
  end;
  FIndex := P - Data + 1;
  FLine := Line;
  FColumn := Column;

  {$IFDEF UNICODE}
  SetStringFromUtf8(TokenValue, F, P - F);
  {$ELSE}
  SetString(TokenValue, F, P - F);
  {$ENDIF UNICODE}
  if Kind = tkIdent then
    Kind := FindTokenIdentKind(TokenValue);

  Result := TToken.Create(FTokens.Count, Kind, TokenValue, FFilename, StartLine, StartColumn, StartIndex, P - F);
  FTokens.Add(Result);

  Inc(FIndex, IndexAdd); // skip some chars if necessary
end;

initialization
  InitTokenStrings;

finalization
  FreeAndNil(TokenStrings);

end.
