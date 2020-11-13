{******************************************************************************}
{*                                                                            *}
{* Delphi Preprocessor                                                        *}
{*                                                                            *}
{* (C) 2005 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DelphiPreproc;

interface

uses
  SysUtils, Classes, Contnrs, DelphiParserContainers, DelphiLexer, DelphiExpr;

type
  TTokenEvent = procedure(Sender: TObject; Token: TToken) of object;
  TIncludeEvent = procedure(Sender: TObject; const Name: string) of object;
  TPreprocessorErrorEvent = procedure(Sender: TObject; const Msg: string; Token: TToken) of object;
  TGetConstBoolValueEvent = procedure(Sender: TObject; const Name: string; var Value: Boolean) of object;
  TGetConstValueEvent = procedure(Sender: TObject; const Name: string; var Kind: TTokenKind; var Value: string) of object;
  TGetDeclaredEvent = procedure(Sender: TObject; const Name: string; var Value: Boolean) of object;

  TTokenParserMethod = procedure(Token: TToken; Lexer: TDelphiLexer) of object;

  EDelphiPreprocessor = class(Exception);

  TTokenParserItem = class(TObject)
  private
    FMethod: TTokenParserMethod;
  public
    constructor Create(AMethod: TTokenParserMethod);
    property Method: TTokenParserMethod read FMethod;
  end;

  TTokenParserList = class(THashtable)
  private
    function GetMethod(const Name: string): TTokenParserMethod;
  public
    procedure Add(const Name: string; Method: TTokenParserMethod);
    property Methods[const Name: string]: TTokenParserMethod read GetMethod; default;
  end;

  TIfdefStackItem = class(TObject)
  private
    FSkip: Boolean;
    FOuterSkip: Boolean;
    procedure SetSkip(const Value: Boolean);
  public
    IsIfdef: Boolean;
    ElseProceeded: Boolean;
    Processed: Boolean;

    property Skip: Boolean read FSkip write SetSkip;
    property OuterSkip: Boolean read FOuterSkip write FOuterSkip;
  end;

  TIfdefStack = class(TObject)
  private
    FStack: TStack;
    FSkip: Boolean;
    function GetCount: Integer;
    function GetInElse: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Ifdef(ASkip: Boolean);
    procedure _Else;
    function EndIf: Boolean;
    procedure ElseIf(ASkip: Boolean);
    procedure _If(ASkip: Boolean);
    function IfEnd: Boolean;

    property Skip: Boolean read FSkip;
    property Count: Integer read GetCount;
    property InElse: Boolean read GetInElse;
  end;

  TDelphiPreprocessor = class(TObject)
  private
    FLexers: TObjectList;
    FCurLexer: TDelphiLexer;
    FTokens: TObjectList;

    FIncludeFiles: TStrings;
    FOnDirective: TTokenEvent;
    FOnComment: TTokenEvent;
    FDefines: THashtable;
    FOnIncludeFile: TIncludeEvent;
    FOnError: TPreprocessorErrorEvent;
    FPreprocToken: TToken;

    FCompilerDirectives: TTokenParserList;
    FIfdefStack: TIfdefStack;
    FOnGetDeclared: TGetDeclaredEvent;
    FOnGetConstBoolValue: TGetConstBoolValueEvent;
    FOnGetConstValue: TGetConstValueEvent;

    function GetCurrentToken: TToken;
    function GetTokenCount: Integer;
    function GetTokenProp(Index: Integer): TToken;
    function GetCurrentFileName: string;
  protected
    procedure InitCompilerDirectives; virtual;
    procedure ProcessDirective(Token: TToken); virtual;
    procedure Error(const Fmt: string; const Args: array of const; Token: TToken);
    function GetConstBoolValue(const Name: string): Boolean; virtual;
    procedure GetConstValue(const Name: string; out Kind: TTokenKind; out Value: string); virtual;
    function GetDeclared(const Name: string): Boolean; virtual;
    function GetDefined(const Name: string): Boolean; virtual;

    property CompilerDirectives: TTokenParserList read FCompilerDirectives;

    procedure ParseDefine(Token: TToken; L: TDelphiLexer);
    procedure ParseUndef(Token: TToken; L: TDelphiLexer);
    procedure ParseIfdef(Token: TToken; L: TDelphiLexer);
    procedure ParseIfndef(Token: TToken; L: TDelphiLexer);
    procedure ParseIfopt(Token: TToken; L: TDelphiLexer);
    procedure ParseEndif(Token: TToken; L: TDelphiLexer);
    procedure ParseElse(Token: TToken; L: TDelphiLexer);
    procedure ParseElseif(Token: TToken; L: TDelphiLexer);
    procedure ParseIf(Token: TToken; L: TDelphiLexer);
    procedure ParseIfEnd(Token: TToken; L: TDelphiLexer);
    procedure ParseInclude(Token: TToken; L: TDelphiLexer);
  public
    constructor Create(const AFilename: string; const Text: UTF8String);
    destructor Destroy; override;

    procedure AddInclude(const Filename: string; const Text: UTF8String); overload;
    procedure Define(const Name: string);
    procedure Undefine(const Name: string);
    function GetToken(AllowComments: Boolean = False): TToken;

    property IncludeFiles: TStrings read FIncludeFiles;
    property Defines: THashtable read FDefines;

    property OnComment: TTokenEvent read FOnComment write FOnComment;
    property OnDirective: TTokenEvent read FOnDirective write FOnDirective;
    property OnIncludeFile: TIncludeEvent read FOnIncludeFile write FOnIncludeFile;
    property OnError: TPreprocessorErrorEvent read FOnError write FOnError;
    property OnGetConstBoolValue: TGetConstBoolValueEvent read FOnGetConstBoolValue write FOnGetConstBoolValue;
    property OnGetConstValue: TGetConstValueEvent read FOnGetConstValue write FOnGetConstValue;
    property OnGetDeclared: TGetDeclaredEvent read FOnGetDeclared write FOnGetDeclared;

    property CurrentFileName: string read GetCurrentFileName;
    property CurrentToken: TToken read GetCurrentToken;

    property TokenCount: Integer read GetTokenCount;
    property Tokens[Index: Integer]: TToken read GetTokenProp;
  end;

  MatchFail = (mfYes, mfNo);

  TPreprocessorIfParser = class(TExpressionParser)
  private
    FPreprocessor: TDelphiPreprocessor;
  public
    constructor Create(APreprocessor: TDelphiPreprocessor; ALexer: TDelphiLexer);
  protected
    function GetConstValue(const Name: string; out Kind: TTokenKind; out Value: string): Boolean; override;
    function GetConstBoolValue(const Name: string): Boolean; override;
    function IsBoolFunction(const Name: string): Boolean; override;
    function EvalBoolFunction(IdentToken: TToken; const Args: TDynTokenArray): Boolean; override;
    procedure ErrorMsg(const Msg: string; const Args: array of const; Token: TToken); override;
  end;

function CompareFileName(const Filename1, Filename2: string): Boolean;

implementation

resourcestring
  RsIncludeRecursion = 'Include recursion detected in file %s';
  RsUnknownCompilerDirective = 'Unknown compiler directive "%s"';
  RsIdentifierExpected = 'Identifier expected';
  RsFilenameExpected = 'Filename expected';
  RsStringEndMissing = 'String end is missing';
  RsNoIfdefOpen = 'No conditinal block open';
  RsExpected = '%s expected';
  RsMultipleElseNotAllowed = '$ELSE already used';

function CompareFileName(const Filename1, Filename2: string): Boolean;
{$IFDEF MSWINDOWS}
var
  Fn1, Fn2: string;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF LINUX}
  Result := CompareStr(Filename1, Filename2) = 0;
  {$ENDIF LINUX}
  {$IFDEF MSWINDOWS}
  if (Pos('~', Filename1) > 0) or (Pos('~', Filename2) > 0) then
  begin
    Fn1 := ExtractShortPathName(Filename1);
    Fn2 := ExtractShortPathName(Filename2);
    Result := (Fn1 <> '') and (Fn2 <> '') and (CompareText(Fn1, Fn2) = 0);
  end
  else
    Result := CompareText(Fn1, Fn2) = 0;
  {$ENDIF MSWINDOWS}
end;

{ TPreprocessorIfParser }

constructor TPreprocessorIfParser.Create(APreprocessor: TDelphiPreprocessor; ALexer: TDelphiLexer);
begin
  inherited Create(ALexer);
  FPreprocessor := APreprocessor;
end;

{function TPreprocessorIfParser.BoolIdent: IBoolNode;
var
  Node: IBoolNode;
  Macro: TToken;
  Ident: TToken;
begin
  Ident := Look;
  if SameText(Ident.Value, 'defined') then
  begin
    Next;
    Match(mfNo, [tkLParan]);
    Next;
    Macro := Look;
    if Macro = nil then
      Error('Invalid defined() syntax')
    else
      if (Macro.Kind <> tkIdent) then
        Error(Format('defined() requires an identifier: %s', [Macro.Value]));
    Node := TBoolNodeConst.Create(FPreprocessor.GetDefined(Macro.Value));
    Next;
    Match(mfYes, [tkRParan]);
    Next;
    Result := Node;
  end
  else if SameText(Ident.Value, 'declared') then
  begin
    Next;
    Match(mfNo, [tkLParan]);
    Next;
    Macro := Look;
    if Macro = nil then
      Error('Invalid declared() syntax')
    else
      if (Macro.Kind <> tkIdent) then
        Error(Format('declared() requires an identifier: %s', [Macro.Value]));
    Node := TBoolNodeConst.Create(FPreprocessor.GetDeclared(Macro.Value));
    Next;
    Match(mfYes, [tkRParan]);
    Next;
    Result := Node;
  end
  else
  begin
    Next;
    if SameText(Ident.Value, 'True') then
      Result := TBoolNodeConst.Create(True)
    else if SameText(Ident.Value, 'False') then
      Result := TBoolNodeConst.Create(False)
    else
      Result := TBoolNodeConst.Create(FPreprocessor.GetConstBoolValue(Ident.Value))
  end;
end;}

function TPreprocessorIfParser.GetConstBoolValue(const Name: string): Boolean;
begin
  if SameText(Name, 'True') then
    Result := True
  else if SameText(Name, 'False') then
    Result := False
  else
    Result := FPreprocessor.GetConstBoolValue(Name);
end;

function TPreprocessorIfParser.GetConstValue(const Name: string; out Kind: TTokenKind;
  out Value: string): Boolean;
begin
  FPreprocessor.GetConstValue(Name, Kind, Value);
  Result := True;
end;

function TPreprocessorIfParser.IsBoolFunction(const Name: string): Boolean;
begin
  Result := SameText(Name, 'defined') or
            SameText(Name, 'declared');
end;

procedure TPreprocessorIfParser.ErrorMsg(const Msg: string; const Args: array of const; Token: TToken);
begin
  if Token = nil then
    Token := FPreprocessor.FPreprocToken;
  FPreprocessor.Error(Msg, Args, Token);
end;

function TPreprocessorIfParser.EvalBoolFunction(IdentToken: TToken; const Args: TDynTokenArray): Boolean;
begin
  if SameText(IdentToken.Value, 'defined') then
  begin
    if Length(Args) <> 1 then
      Error('Invalid defined() syntax')
    else if Args[0].Kind <> tkIdent then
      Error(Format('defined() requires an identifier: %s', [Args[0].Value]));
    Result := FPreprocessor.GetDefined(Args[0].Value);
  end
  else if SameText(IdentToken.Value, 'declared') then
  begin
    if Length(Args) <> 1 then
      Error('Invalid declared() syntax')
    else if Args[0].Kind <> tkIdent then
      Error(Format('declared() requires an identifier: %s', [Args[0].Value]));
    Result := FPreprocessor.GetDeclared(Args[0].Value);
  end
  else
  begin
    ErrorMsg('Unknown $IF function "%s"', [IdentToken.Value], IdentToken);
    Result := False;
  end;
end;

{ TTokenParserList }

procedure TTokenParserList.Add(const Name: string; Method: TTokenParserMethod);
begin
  inherited Add(Name, TTokenParserItem.Create(Method));
end;

function TTokenParserList.GetMethod(const Name: string): TTokenParserMethod;
var
  Item: TTokenParserItem;
begin
  Item := TTokenParserItem(Values[Name]);
  if Item <> nil then
    Result := Item.Method
  else
    Result := nil;
end;

{ TTokenParserItem }

constructor TTokenParserItem.Create(AMethod: TTokenParserMethod);
begin
  inherited Create;
  FMethod := AMethod;
end;

{ TDelphiPreprocessor }

constructor TDelphiPreprocessor.Create(const AFilename: string; const Text: UTF8String);
begin
  inherited Create;
  FLexers := TObjectList.Create;
  FLexers.Add(TDelphiLexer.Create(AFilename, Text, {OwnsObjects:=}False));
  FCurLexer := TDelphiLexer(FLexers[0]);
  FDefines := THashtable.Create(False);
  FIncludeFiles := TStringList.Create;
  FTokens := TObjectList.Create;
  FIfdefStack := TIfdefStack.Create;

  FCompilerDirectives := TTokenParserList.Create(False);
  InitCompilerDirectives;
  Define('CONDITIONALEXPRESSIONS');
end;

destructor TDelphiPreprocessor.Destroy;
begin
  FIfdefStack.Free;
  FCompilerDirectives.Free;
  FIncludeFiles.Free;
  FDefines.Free;
  FLexers.Free;
  FTokens.Free; // from here on all tokens are invalid
  inherited Destroy;
end;

procedure TDelphiPreprocessor.AddInclude(const Filename: string; const Text: UTF8String);
var
  InclCount, i: Integer;
begin
  // detect include recursion
  InclCount := 0;
  for i := 0 to FLexers.Count - 1 do
    if CompareFileName(TDelphiLexer(FLexers[i]).Filename, Filename) then
      Inc(InclCount);
  if InclCount > 50 then
  begin
    Error(RsIncludeRecursion, [ExtractFileName(Filename)], FPreprocToken);
    Exit;
  end;

  FCurLexer := TDelphiLexer.Create(Filename, Text, {OwnsObjects:=}False);
  FCurLexer.SupportMacroTokens := TDelphiLexer(FLexers[0]).SupportMacroTokens;
  FLexers.Add(FCurLexer);

  if FIncludeFiles.IndexOf(Filename) < 0 then
    FIncludeFiles.Add(Filename);
end;

function TDelphiPreprocessor.GetCurrentFileName: string;
begin
  if FCurLexer <> nil then
    Result := FCurLexer.Filename
  else
    Result := '';
end;

function TDelphiPreprocessor.GetCurrentToken: TToken;
begin
  if FTokens.Count > 0 then
    Result := TToken(FTokens[FTokens.Count - 1])
  else
    Result := nil;
end;

procedure TDelphiPreprocessor.Define(const Name: string);
begin
  if not FDefines.Contains(Name) then
    FDefines.Add(Name, nil);
end;

procedure TDelphiPreprocessor.Undefine(const Name: string);
begin
  FDefines.Remove(Name);
end;

procedure TDelphiPreprocessor.InitCompilerDirectives;
begin
  CompilerDirectives.Add('define', ParseDefine);
  CompilerDirectives.Add('undef', ParseUndef);
  CompilerDirectives.Add('ifdef', ParseIfdef);
  CompilerDirectives.Add('ifopt', ParseIfopt);
  CompilerDirectives.Add('ifndef', ParseIfndef);
  CompilerDirectives.Add('endif', ParseEndif);
  CompilerDirectives.Add('else', ParseElse);
  CompilerDirectives.Add('if', ParseIf);
  CompilerDirectives.Add('ifend', ParseIfend);
  CompilerDirectives.Add('elseif', ParseElseif);

  CompilerDirectives.Add('i', ParseInclude);
  CompilerDirectives.Add('include', ParseInclude);
end;

function TDelphiPreprocessor.GetToken(AllowComments: Boolean): TToken;
begin
  if Assigned(FCurLexer) then
  begin
    Result := nil;
    repeat
      Result.Free;
      Result := FCurLexer.GetToken;
      if Result = nil then
      begin
        // lexer has no further tokens, return to previous lexer
        FLexers.Delete(FLexers.Count - 1);
        if FLexers.Count > 0 then
        begin
          FCurLexer := TDelphiLexer(FLexers[FLexers.Count - 1]);
          Continue; // next token
        end
        else
        begin
          FCurLexer := nil;
          // there are no more lexers available
          Break; // ignore FIfdefStack.Skip and return
        end;
      end
      else
      begin
        // evaluate preprocessor tokens and remove comments
        if Result.Kind = tkComment then
        begin
          if Assigned(FOnComment) then
            FOnComment(Self, Result);
          if not AllowComments then
            Continue; // next token
        end
        else if Result.Kind = tkDirective then
        begin
          ProcessDirective(Result);
          Continue; // next token
        end;
      end;

      if not FIfdefStack.Skip then
        Break;
    until False; // for "continue"

    if Result <> nil then
      FTokens.Add(Result);
  end
  else
    Result := nil;
end;

procedure TDelphiPreprocessor.ProcessDirective(Token: TToken);
var
  ps: Integer;
  Opt: string;
  S: string;
  L: TDelphiLexer;
  NameToken: TToken;
  TokenParser: TTokenParserMethod;
begin
  FPreprocToken := Token;
  try
    S := Token.Value;
    ps := Pos('$', S);
    Opt := Copy(S, ps + 1, Length(S) - ps * 2 + 1);

    L := TDelphiLexer.Create('', UTF8Encode(Opt), {OwnsObjects:=}True); // we do not put the tokens into the FTokens list
    try
      NameToken := L.GetToken;
      if (NameToken = nil) or (NameToken.Kind < tkIdent) then
      begin
        Error(RsUnknownCompilerDirective,[], Token);
        Exit;
      end;

      TokenParser := CompilerDirectives[NameToken.Value];
      if Assigned(TokenParser) then
        TokenParser(Token, L)
      else if not FIfdefStack.Skip and Assigned(FOnDirective) then
        FOnDirective(Self, Token);
        { TODO : reactivate when all directives are parsed };
        //Error(RsUnknownCompilerDirective, [NameToken.Value], Token);
    finally
      L.Free;
    end;

  finally
    FPreprocToken := nil;
  end;
end;

procedure TDelphiPreprocessor.Error(const Fmt: string; const Args: array of const;
  Token: TToken);
begin
  Assert(Token <> nil);
  if Assigned(FOnError) then
    FOnError(Self, Format(Fmt, Args), Token)
  else
    raise EDelphiPreprocessor.CreateFmt('%s(%d): %s',
            [Token.Filename, (Token.Line + 1), Format(Fmt, Args)]);
end;

procedure TDelphiPreprocessor.ParseInclude(Token: TToken; L: TDelphiLexer);
var
  S, Filename: string;
  i: Integer;
  Len: Integer;
begin
  if FIfdefStack.Skip then
    Exit;

  Filename := Trim(Copy(UTF8ToString(L.Text), L.Index, MaxInt));
  if Filename = '' then
    Error(RsFilenameExpected, [], Token);
  Len := Length(Filename);
  if Filename[1] = '''' then
  begin
    S := Filename;
    Filename := '';
    for i := 2 to Len do
      if S[i] = '''' then
      begin
        Filename := Copy(S, 2, i - 2);
        Break;
      end;
    if Filename = '' then
    begin
      Error(RsStringEndMissing, [], Token);
      Exit;
    end;
  end
  else
  begin
    i := 2;
    while (i <= Len) and (Filename[i] > ' ') do
      Inc(i);
    Filename := Copy(Filename, 1, i - 1);
  end;

  if Assigned(FOnIncludeFile) then
    FOnIncludeFile(Self, Filename);
end;

procedure TDelphiPreprocessor.ParseIfEnd(Token: TToken; L: TDelphiLexer);
begin
  if FIfdefStack.Count = 0 then
    Error(RsNoIfdefOpen, [], Token)
  else if not FIfdefStack.IfEnd then
    Error(RsExpected, ['$ENDIF'], Token);
end;

procedure TDelphiPreprocessor.ParseEndif(Token: TToken; L: TDelphiLexer);
begin
  if FIfdefStack.Count = 0 then
    Error(RsNoIfdefOpen, [], Token)
  else if not FIfdefStack.Endif then
    Error(RsExpected, ['$IFEND'], Token);
end;

procedure TDelphiPreprocessor.ParseIf(Token: TToken; L: TDelphiLexer);
var
  p: TPreprocessorIfParser;
begin
  if FIfdefStack.Skip then
    FIfdefStack._If(False)
  else
  begin
    p := TPreprocessorIfParser.Create(Self, L);
    try
      FIfdefStack._If(not p.Parse);
    finally
      p.Free;
    end;
  end;
end;

procedure TDelphiPreprocessor.ParseIfdef(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  t := L.ParseIfdefToken;
  if (t = nil) or (t.Kind < tkIdent) then
  begin
    Error(RsIdentifierExpected, [], Token);
    FIfdefStack.Ifdef(False); // default to undefined if no exception was raised
    Exit;
  end;
  if FIfdefStack.Skip then
    FIfdefStack.Ifdef(False)
  else
    FIfdefStack.Ifdef(not FDefines.Contains(t.Value));
end;

procedure TDelphiPreprocessor.ParseIfndef(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  t := L.ParseIfdefToken;
  if (t = nil) or (t.Kind < tkIdent) then
  begin
    Error(RsIdentifierExpected, [], Token);
    FIfdefStack.Ifdef(True); // default to undefined if no exception was raised
    Exit;
  end;

  if FIfdefStack.Skip then
    FIfdefStack.Ifdef(False)
  else
    FIfdefStack.Ifdef(FDefines.Contains(t.Value));
end;

procedure TDelphiPreprocessor.ParseIfopt(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  t := L.ParseIfdefToken;
  if (t = nil) or (t.Kind < tkIdent) then
  begin
    Error(RsIdentifierExpected, [], Token);
    FIfdefStack.Ifdef(False); // default to undefined if no exception was raised
    Exit;
  end;
  if FIfdefStack.Skip then
    FIfdefStack.Ifdef(False)
  else
    // $X+/- ???
    FIfdefStack.Ifdef(FCompilerDirectives.Contains(t.Value));
end;

procedure TDelphiPreprocessor.ParseElse(Token: TToken; L: TDelphiLexer);
begin
  if FIfdefStack.Count = 0 then
    Error(RsNoIfdefOpen, [], Token)
  else if FIfdefStack.InElse then
    Error(RsMultipleElseNotAllowed, [], Token)
  else
    FIfdefStack._Else;
end;

procedure TDelphiPreprocessor.ParseElseif(Token: TToken; L: TDelphiLexer);
var
  p: TPreprocessorIfParser;
begin
  if FIfdefStack.Count = 0 then
    Error(RsNoIfdefOpen, [], Token)
  else
  begin
    if FIfdefStack.Skip then
      FIfdefStack.ElseIf(False)
    else
    begin
      p := TPreprocessorIfParser.Create(Self, L);
      try
        FIfdefStack.ElseIf(not p.Parse);
      finally
        p.Free;
      end;
    end;
  end;
end;

procedure TDelphiPreprocessor.ParseDefine(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  if not FIfdefStack.Skip then
  begin
    t := L.ParseIfdefToken;
    if (t = nil) or (t.Kind < tkIdent) then
    begin
      Error(RsIdentifierExpected, [], Token);
      Exit;
    end;
    Define(t.Value);
  end;
end;

procedure TDelphiPreprocessor.ParseUndef(Token: TToken; L: TDelphiLexer);
var
  t: TToken;
begin
  if not FIfdefStack.Skip then
  begin
    t := L.ParseIfdefToken;
    if (t = nil) or (t.Kind < tkIdent) then
    begin
      Error(RsIdentifierExpected, [], Token);
      Exit;
    end;
    Undefine(t.Value);
  end;
end;

function TDelphiPreprocessor.GetConstBoolValue(const Name: string): Boolean;
begin
  Result := False;
  if Assigned(FOnGetConstBoolValue) then
    FOnGetConstBoolValue(Self, Name, Result);
end;

function TDelphiPreprocessor.GetDeclared(const Name: string): Boolean;
begin
  Result := False;
  if Assigned(FOnGetDeclared) then
    FOnGetDeclared(Self, Name, Result);
end;

function TDelphiPreprocessor.GetDefined(const Name: string): Boolean;
begin
  Result := Defines.Contains(Name);
end;

function TDelphiPreprocessor.GetTokenCount: Integer;
begin
  Result := FTokens.Count;
end;

function TDelphiPreprocessor.GetTokenProp(Index: Integer): TToken;
begin
  Result := TToken(FTokens[Index]);
end;

procedure TDelphiPreprocessor.GetConstValue(const Name: string;
  out Kind: TTokenKind; out Value: string);
begin
  Value := '';
  Kind := tkString;
  if Assigned(FOnGetConstValue) then
    FOnGetConstValue(Self, Name, Kind, Value);
end;

{ TIfdefStackItem }

procedure TIfdefStackItem.SetSkip(const Value: Boolean);
begin
  FSkip := Value;
  if not Value then
    Processed := True;
end;

{ TIfdefStack }

constructor TIfdefStack.Create;
begin
  inherited Create;
  FSkip := False;
  FStack := TObjectStack.Create;
end;

destructor TIfdefStack.Destroy;
begin
  while FStack.Count > 0 do // there shouldn't be anything but keep memory leaks low
    TObject(FStack.Pop).Free;
  FStack.Free;
  inherited Destroy;
end;

function TIfdefStack.GetCount: Integer;
begin
  Result := FStack.Count;
end;

procedure TIfdefStack._If(ASkip: Boolean);
var
  Item: TIfdefStackItem;
begin
  Item := TIfdefStackItem.Create;
  Item.IsIfdef := False;
  Item.Skip := ASkip;
  Item.OuterSkip := FSkip;
  FStack.Push(Item);
  FSkip := Item.OuterSkip or Item.Skip;
end;

function TIfdefStack.IfEnd: Boolean;
var
  LOuterSkip: Boolean;
begin
  with TIfdefStackItem(FStack.Pop) do
  begin
    LOuterSkip := OuterSkip;
    Result := not IsIfdef;
    Free;
  end;
  FSkip := LOuterSkip;
end;

procedure TIfdefStack.ElseIf(ASkip: Boolean);
var
  Item: TIfdefStackItem;
begin
  Item := TIfdefStackItem(FStack.Peek);
  //Assert(not Item.ElseProceeded);
  if not Item.Processed then
  begin
    Item.Skip := ASkip;
    FSkip := Item.OuterSkip or Item.Skip;
  end;
  Item.IsIfdef := False; // switch to $IF mode
end;

procedure TIfdefStack.Ifdef(ASkip: Boolean);
var
  Item: TIfdefStackItem;
begin
  Item := TIfdefStackItem.Create;
  Item.IsIfdef := True;
  Item.Skip := ASkip;
  Item.OuterSkip := FSkip;
  FStack.Push(Item);
  FSkip := Item.OuterSkip or Item.Skip;
end;

procedure TIfdefStack._Else;
var
  Item: TIfdefStackItem;
begin
  Item := TIfdefStackItem(FStack.Peek);
  Item.Skip := Item.Processed;
  FSkip := Item.OuterSkip or Item.Skip;
  Item.ElseProceeded := True;
end;

function TIfdefStack.EndIf: Boolean;
var
  LOuterSkip: Boolean;
begin
  with TIfdefStackItem(FStack.Pop) do
  begin
    LOuterSkip := OuterSkip;
    Result := IsIfdef;
    Free;
  end;
  FSkip := LOuterSkip;
end;

function TIfdefStack.GetInElse: Boolean;
begin
  if FStack.Count > 0 then
    Result := TIfdefStackItem(FStack.Peek).ElseProceeded
  else
    Result := False;
end;

end.
