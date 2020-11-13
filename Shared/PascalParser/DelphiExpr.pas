{******************************************************************************}
{*                                                                            *}
{* Delphi Expression Parser                                                   *}
{*                                                                            *}
{* (C) 2005 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DelphiExpr;

interface

uses
  SysUtils, Classes, Contnrs, DelphiLexer;

type
  TValueKind = (vkIsInt, vkIsFloat, vkIsString{, vkIsSet});

  TDynTokenArray = array of TToken;

  TExprNodeValueRec = record
    Typ: TValueKind;
    ValueInt: Int64;
    ValueFloat: Double;
    ValueStr: string;
    Error: string;
  end;

  EInvalidOp = class(Exception);

  IExprNode = interface
    function ToString: string;
    function GetValue(out Value: TExprNodeValueRec): Boolean;
  end;

  TExprNode = class(TInterfacedObject, IExprNode)
  public
    function GetValue(out Value: TExprNodeValueRec): Boolean; virtual; abstract;
  end;

  TExprNodeUnaryMinus = class(TExprNode)
  private
    FOperand: IExprNode;
  public
    constructor Create(AOperand: IExprNode);
    function ToString: string; override;
    function GetValue(out Value: TExprNodeValueRec): Boolean; override;
  end;

  TExprNodeBitOpNeg = class(TExprNode)
  private
    FOperand: IExprNode;
  public
    constructor Create(AOperand: IExprNode);
    function ToString: string; override;
    function GetValue(out Value: TExprNodeValueRec): Boolean; override;
  end;

  TExprNodeBinOp = class(TExprNode)
  private
    FOperation: TTokenKind;
    FOpName: string;
    FOperand1: IExprNode;
    FOperand2: IExprNode;
  public
    constructor Create(AOperation: TTokenKind; const AOpName: string; AOperand1, AOperand2: IExprNode);
    function ToString: string; override;
    function GetValue(out Value: TExprNodeValueRec): Boolean; override;
  end;

  TExprNodeConst = class(TExprNode)
  private
    FKind: TTokenKind;
    FValue: string;
  public
    constructor Create(AKind: TTokenKind; const AValue: string);
    function ToString: string; override;
    function GetValue(out Value: TExprNodeValueRec): Boolean; override;
  end;

  IBoolNode = interface
    function ToString: string;
    function GetValue(var Error: string): Boolean;
  end;

  TBoolNode = class(TInterfacedObject, IBoolNode)
  public
    function GetValue(var Error: string): Boolean; virtual; abstract;
  end;

  TBoolNodeOp = class(TBoolNode)
  private
    FOperation: TTokenKind;
    FOpName: string;
    FLeft: IBoolNode;
    FRight: IBoolNode;
  public
    constructor Create(AOperation: TTokenKind; const AOpName: string; ALeft, ARight: IBoolNode);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

  TBoolNodeNot = class(TBoolNode)
  private
    FNode: IBoolNode;
  public
    constructor Create(ANode: IBoolNode);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

  TBoolNodeConst = class(TBoolNode)
  private
    FValue: Boolean;
  public
    constructor Create(AValue: Boolean);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

{  TBoolNodeIdent = class(TBoolNode)
  private
    FIdent: string;
    FPreProc: TDelphiPreprocessor;
  public
    constructor Create(APreProc: TDelphiPreprocessor; const AIdent: string);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;}

  TBoolNodeExp = class(TBoolNode)
  private
    FExpr: IExprNode;
  public
    constructor Create(AExpr: IExprNode);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

  TBoolNodeRelOp = class(TBoolNode)
  private
    FRelOperation: TTokenKind;
    FRelOpName: string;
    FLeft: IExprNode;
    FRight: IExprNode;
  public
    constructor Create(ARelOperation: TTokenKind; const ARelOpName: string;
      ALeft, ARight: IExprNode);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

  TBoolNodeBoolOp = class(TBoolNode)
  private
    FOperation: TTokenKind;
    FOpName: string;
    FLeft: IBoolNode;
    FRight: IBoolNode;
  public
    constructor Create(AOperation: TTokenKind; const AOpName: string; ALeft, ARight: IBoolNode);
    function ToString: string; override;
    function GetValue(var Error: string): Boolean; override;
  end;

  MatchFail = (mfYes, mfNo);

  TExpressionParser = class(TObject)
  private
    Look: TToken;
    FLexer: TDelphiLexer;
    FRescanTokens: TObjectList;
    FRescanIndex: Integer;
  public
    constructor Create(ALexer: TDelphiLexer);
    destructor Destroy; override;
  protected
    function GetConstValue(const Name: string; out Kind: TTokenKind; out Value: string): Boolean; virtual;
    function GetConstBoolValue(const Name: string): Boolean; virtual;
    function IsBoolFunction(const Name: string): Boolean; virtual;
    function EvalBoolFunction(IdentToken: TToken; const Args: TDynTokenArray): Boolean; virtual;
    procedure ErrorMsg(const Msg: string; const Args: array of const; Token: TToken); virtual;
    procedure Error(const Msg: string);
  private
    function NextCache: TToken; {$IFDEF COMPILER10_UP} inline; {$ENDIF}
    procedure Next;
    procedure Rescan(Token: TToken);
    function Match(const Tokens: array of TTokenKind): Boolean; overload;
    function Match(Fail: MatchFail; const Tokens: array of TTokenKind): Boolean; overload;
    function Expression: IExprNode;
    function Term: IExprNode;
    function Factor: IExprNode;

    function BoolExpression: IBoolNode;
    function BoolTerm: IBoolNode;
    function BoolNotFactor: IBoolNode;
    function IsRelOp: Boolean;
    function IsBoolOp: Boolean;
    function BoolIdent: IBoolNode;
    function RelOp: IBoolNode;
    function BoolFactor: IBoolNode;
    function BoolSingleFactor: IBoolNode;
  public
    function Parse: Boolean;
  end;

implementation

{.$AUTOBOX ON}
{.$HINTS OFF}
{.$WARNINGS OFF}

constructor TExprNodeUnaryMinus.Create(AOperand: IExprNode);
begin
  inherited Create;
  FOperand := AOperand;
end;

function TExprNodeUnaryMinus.ToString: string;
begin
  Result := '-' + FOperand.ToString;
end;

function TExprNodeUnaryMinus.GetValue(out Value: TExprNodeValueRec): Boolean;
var
  Calculatable: Boolean;
begin
  Calculatable := FOperand.GetValue(Value);
  if Calculatable then
  begin
    case Value.Typ of
      vkIsInt:
        Value.ValueInt := -Value.ValueInt;
      vkIsFloat:
        Value.ValueFloat := -Value.ValueFloat;
      vkIsString:
        // TODO: invalid operation
        Value.Error := Format('Invalid operation %s', ['-']);
    end;
  end;
  Result := Calculatable;
end;

constructor TExprNodeBitOpNeg.Create(AOperand: IExprNode);
begin
  inherited Create;
  FOperand := AOperand;
end;

function TExprNodeBitOpNeg.ToString: string;
begin
  Result := '~' + FOperand.ToString;
end;

function TExprNodeBitOpNeg.GetValue(out Value: TExprNodeValueRec): Boolean;
var
  Calculatable: Boolean;
begin
  Calculatable := FOperand.GetValue(Value);
  if Calculatable then
  begin
    case Value.Typ of
      vkIsInt:
        Value.ValueInt := not Value.ValueInt;
      vkIsFloat:
        begin
          Value.Error := 'Operation ~ cannot be applied to float value';
          Result := False;
          Exit;
        end;
      vkIsString:
        // TODO: invalid operation
        Value.Error := Format('Invalid operation %s', ['not']);
    end;
  end;
  Result := Calculatable;
end;

constructor TExprNodeBinOp.Create(AOperation: TTokenKind; const AOpName: string; AOperand1, AOperand2: IExprNode);
begin
  inherited Create;
  FOperation := AOperation;
  FOpName := AOpName;
  FOperand1 := AOperand1;
  FOperand2 := AOperand2;
end;

function TExprNodeBinOp.ToString: string;
begin
  Result := '(' + FOperand1.ToString + ' ' + FOpName + ' ' + FOperand2.ToString + ')';
end;

function TExprNodeBinOp.GetValue(out Value: TExprNodeValueRec): Boolean;
var
  Calculatable2: Boolean;
  Calculatable1: Boolean;
  V2: Double;
  V1: Double;
  Value2: TExprNodeValueRec;
  Value1: TExprNodeValueRec;
begin
  Calculatable1 := FOperand1.GetValue(Value1);
  Calculatable2 := FOperand2.GetValue(Value2);
  if (Calculatable1 = False) then
    Value.Error := Value1.Error
  else
    Value.Error := Value2.Error;
  Value.ValueFloat := 0;
  Value.ValueInt := 0;
  Value.Typ := vkIsInt;
  if (Calculatable1 and Calculatable2) then
  begin
    if ((Value1.Typ = vkIsString) and (Value2.Typ = vkIsString)) then
    begin
      Value.Typ := vkIsString;
      Result := True;
      // calculate
      case FOperation of
        tkPlus:
          Value.ValueStr := Value1.ValueStr + Value2.ValueStr;
      else
        // TODO: invalid operation
        Value.Error := Format('Invalid operation %s', [FOpName]);
        Result := False;
      end;
    end
    else if ((Value1.Typ = vkIsInt) and (Value2.Typ = vkIsInt)) then
    begin
      Value.Typ := vkIsInt;
      Result := True;
      // calculate
      case FOperation of
        tkPlus:
          Value.ValueInt := Value1.ValueInt + Value2.ValueInt;
        tkMinus:
          Value.ValueInt := Value1.ValueInt - Value2.ValueInt;
        tkMultiply:
          Value.ValueInt := Value1.ValueInt * Value2.ValueInt;
        tkDivide:
          Value.ValueInt := Value1.ValueInt div Value2.ValueInt; // warning: division by 0
        tkI_Mod:
          Value.ValueInt := Value1.ValueInt mod Value2.ValueInt; // warning: division by 0
        tkI_Or:
          Value.ValueInt := Value1.ValueInt or Value2.ValueInt;
        tkI_Xor:
          Value.ValueInt := Value1.ValueInt xor Value2.ValueInt;
        tkI_And:
          Value.ValueInt := Value1.ValueInt and Value2.ValueInt;
      else
        Result := False;
      end;
    end
    else
    begin
      V1 := 0;
      V2 := 0;
      Value.Typ := vkIsFloat;
      if ((Value1.Typ = vkIsFloat) and (Value2.Typ = vkIsInt)) then
      begin
        V1 := Value1.ValueFloat;
        V2 := Value2.ValueInt;
      end
      else if ((Value1.Typ = vkIsInt) and (Value2.Typ = vkIsFloat)) then
      begin
        V1 := Value1.ValueInt;
        V2 := Value2.ValueFloat;
      end
      else if ((Value1.Typ = vkIsFloat) and (Value2.Typ = vkIsFloat)) then
      begin
        V1 := Value1.ValueFloat;
        V2 := Value2.ValueFloat;
      end;
      Result := True;
      // calculate
      case FOperation of
        tkPlus:
          Value.ValueFloat := V1 + V2;
        tkMinus:
          Value.ValueFloat := V1 - V2;
        tkMultiply:
          Value.ValueFloat := V1 * V2;
        tkDivide:
          if V2 = 0 then
          begin
            Value.Error := 'division by zero';
            Result := False;
          end
          else
            Value.ValueFloat := V1 / V2;
        tkI_Mod:
          if V2 = 0 then
          begin
            Value.Error := 'division by zero';
            Result := False;
          end
          else
            Value.ValueFloat := Frac(V1 / V2);
        tkI_Or, tkI_Xor, tkI_And:
          begin
            // TODO: invalid operation
            Value.Error := Format('Invalid operation %s', [FOpName]);
            Result := False;
          end;
      end;
    end;
  end
  else
    Result := False;
end;

constructor TExprNodeConst.Create(AKind: TTokenKind; const AValue: string);
begin
  inherited Create;
  FKind := AKind;
  FValue := AValue;
end;

function TExprNodeConst.ToString: string;
begin
  Result := FValue;
end;

function TExprNodeConst.GetValue(out Value: TExprNodeValueRec): Boolean;
var
  ErrCode: Integer;
begin
  Value.Error := '';
  Value.ValueInt := 0;
  Value.ValueFloat := 0;
  Value.ValueStr := '';
  Value.Typ := vkIsInt;
  case FKind of
    tkIdent:
      begin
        Value.Typ := vkIsInt;
        if SameText(FValue, 'true') then
          Value.ValueInt := 1
        else if SameText(FValue, 'false') then
          Value.ValueInt := 0
        else
          raise Exception.Create('Internal error: Unknown identifier found');
      end;
    tkInt:
      begin
        Value.Typ := vkIsInt;
        Value.ValueInt := StrToInt(FValue);
      end;
    tkFloat:
      begin
        Value.Typ := vkIsFloat;
        Val(FValue, Value.ValueFloat, ErrCode);
        if ErrCode > 0 then
        begin
          Value.Error := Format('%s is no valid float value', [FValue]);
          Result := False;
          Exit;
        end;
      end;
    tkString:
      begin
        Value.Typ := vkIsString;
        Value.ValueStr := FValue;
      end;
  end;
  Result := True;
end;

constructor TBoolNodeOp.Create(AOperation: TTokenKind; const AOpName: string;
  ALeft: IBoolNode; ARight: IBoolNode);
begin
  inherited Create;
  FOperation := AOperation;
  FOpName := AOpName;
  FLeft := ALeft;
  FRight := ARight;
end;

function TBoolNodeOp.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' ' + FOpName + ' ' + FRight.ToString + ')';
end;

function TBoolNodeOp.GetValue(var Error: string): Boolean;
var
  L: Boolean;
begin
  L := FLeft.GetValue(Error);
  if Error <> '' then
  begin
    Result := False;
    Exit;
  end;
  case FOperation of
    tkI_And:
      Result := L and FRight.GetValue(Error);
    tkI_Or:
      Result := L or FRight.GetValue(Error);
    tkI_Xor:
      Result := L xor FRight.GetValue(Error);
  else
    Error := Format('Unknown operation %s', [FOpName]);
    Result := False;
  end;
end;

constructor TBoolNodeNot.Create(ANode: IBoolNode);
begin
  inherited Create;
  FNode := ANode;
end;

function TBoolNodeNot.ToString: string;
begin
  Result := 'not ' + FNode.ToString;
end;

function TBoolNodeNot.GetValue(var Error: string): Boolean;
begin
  Result := not FNode.GetValue(Error);
end;

constructor TBoolNodeConst.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

function TBoolNodeConst.ToString: string;
const
  BoolToStr: array[Boolean] of string = ('False', 'True');
begin
  Result := BoolToStr[FValue];
end;

function TBoolNodeConst.GetValue(var Error: string): Boolean;
begin
  Result := FValue;
end;

{constructor TBoolNodeIdent.Create(APreProc: TDelphiPreprocessor; const AIdent: string);
begin
  inherited Create;
  FIdent := AIdent;
  FPreProc := APreProc;
end;

function TBoolNodeIdent.ToString: string;
begin
  Result := FIdent;
end;

function TBoolNodeIdent.GetValue(var Error: string): Boolean;
begin
  Result := FPreProc.Defines.Contains(FIdent);
end;}

constructor TBoolNodeExp.Create(AExpr: IExprNode);
begin
  inherited Create;
  FExpr := AExpr;
end;

function TBoolNodeExp.ToString: string;
begin
  Result := '(' + FExpr.ToString + ')';
end;

function TBoolNodeExp.GetValue(var Error: string): Boolean;
var
  Value: TExprNodeValueRec;
begin
  if FExpr.GetValue(Value) then
  begin
    if Value.Typ = vkIsFloat then
      Error := 'Float Value cannot be evaluated to true or false.'
    else if Value.Typ = vkIsString then
      Error := 'String Value cannot be evaluated to true or false.'
    else if Value.Typ = vkIsInt then
    begin
      Result := (Value.ValueInt <> 0);
      Exit;
    end
    else
      Error := 'Unknown number type.'
  end
  else
    Error := Value.Error;
  Result := False;
end;

constructor TBoolNodeRelOp.Create(ARelOperation: TTokenKind; const ARelOpName: string;
  ALeft, ARight: IExprNode);
begin
  inherited Create;
  FRelOperation := ARelOperation;
  FRelOpName := ARelOpName;
  FLeft := ALeft;
  FRight := ARight;
end;

function TBoolNodeRelOp.ToString: string;
begin
  Result := '(' + FLeft.ToString + ' ' + FRelOpName + ' ' + FRight.ToString + ')';
end;

function TBoolNodeRelOp.GetValue(var Error: string): Boolean;
var
  D1, D2: Double;
  V1, V2: TExprNodeValueRec;
  S1, S2: string;
begin
  Result := False;
  V1.Error := '';
  V2.Error := '';
  if FLeft.GetValue(V1) and FRight.GetValue(V2) then
  begin
    D1 := 0;
    D2 := 0;
    if V1.Typ = vkIsInt then D1 := V1.ValueInt
    else if V1.Typ = vkIsFloat then D1 := V1.ValueFloat
    else S1 := V1.ValueStr;

    if V2.Typ = vkIsInt then D2 := V2.ValueInt
    else if V1.Typ = vkIsFloat then D2 := V2.ValueFloat
    else S2 := V2.ValueStr;

    case FRelOperation of
      tkEqual: // ==
        if (V1.Typ = vkIsString) and (V2.Typ = vkIsString) then
          Result := S1 = S2
        else if (V1.Typ = vkIsInt) and (V2.Typ = vkIsInt) then
          Result := V1.ValueInt = V2.ValueInt
        else
          Result := D1 = D2;

      tkNotEqual: // !=
        if (V1.Typ = vkIsString) and (V2.Typ = vkIsString) then
          Result := S1 <> S2
        else if (V1.Typ = vkIsInt) and (V2.Typ = vkIsInt) then
          Result := V1.ValueInt <> V2.ValueInt
        else
          Result := D1 <> D2;

      tkLessThan: // <
        if (V1.Typ = vkIsString) and (V2.Typ = vkIsString) then
          Result := S1 < S2
        else if (V1.Typ = vkIsInt) and (V2.Typ = vkIsInt) then
          Result := V1.ValueInt < V2.ValueInt
        else
          Result := D1 < D2;

      tkGreaterThan: // >
        if (V1.Typ = vkIsString) and (V2.Typ = vkIsString) then
          Result := S1 > S2
        else if (V1.Typ = vkIsInt) and (V2.Typ = vkIsInt) then
          Result := V1.ValueInt > V2.ValueInt
        else
          Result := D1 > D2;

      tkLessEqualThan: // <=
        if (V1.Typ = vkIsString) and (V2.Typ = vkIsString) then
          Result := S1 <= S2
        else if (V1.Typ = vkIsInt) and (V2.Typ = vkIsInt) then
          Result := V1.ValueInt <= V2.ValueInt
        else
          Result := D1 <= D2;

      tkGreaterEqualThan: // >=
        if (V1.Typ = vkIsString) and (V2.Typ = vkIsString) then
          Result := S1 >= S2
        else if (V1.Typ = vkIsInt) and (V2.Typ = vkIsInt) then
          Result := V1.ValueInt >= V2.ValueInt
        else
          Result := D1 >= D2;
    else
      Error := Format('Unknown relation operation: %s', [FRelOpName]);
    end;
  end
  else
    Error := (V1.Error + '. ' + V2.Error);
end;

{ TBoolNodeBoolOp }

constructor TBoolNodeBoolOp.Create(AOperation: TTokenKind; const AOpName: string; ALeft, ARight: IBoolNode);
begin
  inherited Create;
  FOperation := AOperation;
  FOpName := AOpName;
  FLeft := ALeft;
  FRight := ARight;
end;

function TBoolNodeBoolOp.GetValue(var Error: string): Boolean;
begin
  Result := FLeft.GetValue(Error);
  if FOperation = tkEqual then
    Result := Result = FRight.GetValue(Error)
  else if FOperation = tkNotEqual then
    Result := Result <> FRight.GetValue(Error)
  else
  begin
    Error := Format('Unknown boolean operator "%s"', [FOpName]);
    Result := False;
  end;
end;

function TBoolNodeBoolOp.ToString: string;
begin
  Result := FLeft.ToString + ' ' + FOpName + ' ' + FRight.ToString;
end;

{ TExpressionParser }

constructor TExpressionParser.Create(ALexer: TDelphiLexer);
begin
  inherited Create;
  FLexer := ALexer;
  FRescanTokens := TObjectList.Create(False);
end;

destructor TExpressionParser.Destroy;
begin
  FRescanTokens.Free;
  inherited Destroy;
end;

function TExpressionParser.NextCache: TToken;
begin
  Result := nil;
  if FRescanIndex < FRescanTokens.Count then
    Result := TToken(FRescanTokens[FRescanIndex]);
  Inc(FRescanIndex);
end;

procedure TExpressionParser.Next;
begin
  Look := NextCache;
  if Look = nil then
  begin
    Look := FLexer.GetToken;
    FRescanTokens.Add(Look);
  end;
end;

procedure TExpressionParser.Rescan(Token: TToken);
begin
  FRescanIndex := FRescanTokens.IndexOf(token);
  if FRescanIndex = -1 then
    ErrorMsg('Internal error 0100: Cannot rescan token.', [], Token);
end;

procedure TExpressionParser.Error(const Msg: string);
begin
  ErrorMsg(Msg, [], nil);
end;

procedure TExpressionParser.ErrorMsg(const Msg: string; const Args: array of const; Token: TToken);
begin
  raise Exception.CreateFmt(Msg, Args);
end;

function TExpressionParser.EvalBoolFunction(IdentToken: TToken; const Args: TDynTokenArray): Boolean;
begin
  ErrorMsg('Unknown boolean function "%s"', [IdentToken.Value], IdentToken);
  Result := False;
end;

function TExpressionParser.Match(const Tokens: array of TTokenKind): Boolean;
var
  I: Integer;
begin
  if Look <> nil then
  begin
    for I := 0 to High(Tokens) do
      if Look.Kind = Tokens[I] then
      begin
        Result := True;
        Exit;
      end;
  end;
  Result := False;
end;

function TExpressionParser.Match(Fail: MatchFail; const Tokens: array of TTokenKind): Boolean;
begin
  if Fail = mfNo then
  begin
    Result := Match(Tokens);
    Exit;
  end;
  if not Match(Tokens) then
  begin
    if Look <> nil then
      Error(Format('Unexpected token: %s', [Look.Value]))
    else
      Error('Unexpected end of file');
  end;
  Result := True;
end;

(*
	<expression>  ::=  [ "+" | "-" | "not" | € ] <term> { [ "+" | "-" | "or" | "xor" ] <term> }*

  <term>  ::=  <factor> { [ "*" | "/" | "mod" | "and" ] <factor> }*

  <factor>  ::=  <number>  | <ident> | "(" <expression> ")"
*)
function TExpressionParser.Expression: IExprNode;
var
  Op: TToken;
  Exp: IExprNode;
  BitNeg: Boolean;
  Negative: Boolean;
begin
  Negative := False;
  BitNeg := False;
  case Look.Kind of
    tkPlus, tkMinus:
      begin
        Negative := True;
        Next;
      end;
    tkI_Not:
      begin
        BitNeg := True;
        Next;
      end;
  end;
  Exp := Term;
  if Negative then
    Exp := TExprNodeUnaryMinus.Create(Exp);
  if BitNeg then
    Exp := TExprNodeBitOpNeg.Create(Exp);
  while Match(mfNo, [tkPlus, tkMinus, tkI_Or, tkI_Xor]) do
  begin
    Op := Look;
    Next;
    Exp := TExprNodeBinOp.Create(Op.Kind, Op.Value, Exp, Term);
  end;
  Result := Exp;
end;

function TExpressionParser.Term: IExprNode;
var
  Op: TToken;
  t: IExprNode;
begin
  t := Factor;
  while Match(mfNo, [tkMultiply, tkDivide, tkI_Mod, tkI_And]) do
  begin
    Op := Look;
    Next;
    t := TExprNodeBinOp.Create(Op.Kind, Op.Value, t, Factor);
  end;
  Result := t;
end;

function TExpressionParser.Factor: IExprNode;
var
  Fact: IExprNode;
  Ident: TToken;
  Kind: TTokenKind;
  S: string;
begin
  Fact := nil;
  Match(mfYes, [tkInt, tkFloat, tkString, tkIdent, tkLParan]);

  case Look.Kind of
    tkInt, tkFloat, tkString:
      Fact := TExprNodeConst.Create(Look.Kind, Look.Value);
    tkIdent:
      begin
        Ident := Look;
        if SameText(Ident.Value, 'True') or SameText(Ident.Value, 'False') then
          Fact := TExprNodeConst.Create(Ident.Kind, Ident.Value)
        else
        begin
          if Match(mfNo, [tkLParan]) then
            Error(Format('Call to undefined function: %s', [Ident.Value]))
          else
          begin
            if not GetConstValue(Ident.Value, Kind, S) then
              ErrorMsg('Unknown identifier %s', [Ident.Value], Ident);
            Fact := TExprNodeConst.Create(Kind, S);
          end;
        end;
      end;
    tkLParan:
      begin
        Next();
        Fact := Expression();
        Match(mfYes, [tkRParan]);
      end;
  end;
  Next;
  Result := Fact;
end;

(*
  <B-expression>  ::=  <B-term> { [ "or" | "xor" ] <B-term> }*

  <B-term>  ::=  <B-not-factor> { "and" <B-not-factor> }*

  <B-not-factor>  ::=  [ "not" | € ] <B-factor>

  <relop>  ::=  "<" | "<=" | ">" | ">=" | "=" | "<>"

  <boolop>  ::=  "=" | "<>"

  <B-factor>  ::= <B-single-factor> [ <boolop> <B-single-factor> ]

  <B-single-factor>  ::=  <number> | <string> | true | false | "(" <B-expression> ")"
                     ::=  "defined" "(" <Ident> ")"
                     ::=  "declared" "(" <Ident> ")"
                     ::=  <Ident>
                     ::=  <expression> <relop> <expression>
*)
function TExpressionParser.BoolExpression: IBoolNode;
var
  Right: IBoolNode;
  Op: TToken;
  Left: IBoolNode;
begin
  Left := BoolTerm;
  while Match(mfNo, [tkI_Or, tkI_Xor]) do
  begin
    Op := Look;
    Next;
    Right := BoolTerm;
    Left := TBoolNodeOp.Create(Op.Kind, Op.Value, Left, Right);
  end;
  Result := Left;
end;

function TExpressionParser.BoolTerm: IBoolNode;
var
  Right: IBoolNode;
  Op: TToken;
  Left: IBoolNode;
begin
  Left := BoolNotFactor;
  while Match(mfNo, [tkI_And]) do
  begin
    Op := Look;
    Next;
    Right := BoolNotFactor;
    Left := TBoolNodeOp.Create(Op.Kind, Op.Value, Left, Right);
  end;
  if (Look <> nil) then
    Match(mfYes, [tkRParan, tkI_Or, tkI_Not, tkI_Xor]);
  Result := Left;
end;

function TExpressionParser.BoolNotFactor: IBoolNode;
begin
  if Match(mfNo, [tkI_Not]) then
  begin
    Next;
    Result := TBoolNodeNot.Create(BoolFactor);
  end
  else
    Result := BoolFactor;
end;

function TExpressionParser.IsBoolFunction(const Name: string): Boolean;
begin
  Result := False;
end;

function TExpressionParser.IsRelOp: Boolean;
begin
  Result := Match(mfNo, [tkLessThan, tkGreaterThan, tkLessEqualThan,
                         tkGreaterEqualThan, tkEqual, tkNotEqual]);
end;

function TExpressionParser.IsBoolOp: Boolean;
begin
  Result := Match(mfNo, [tkEqual, tkNotEqual]);
end;

function TExpressionParser.BoolIdent: IBoolNode;
var
  Ident: TToken;
  Args: TDynTokenArray;
begin
  Ident := Look;
  if IsBoolFunction(Ident.Value) then
  begin
    Next;
    Match(mfNo, [tkLParan]);
    Next;
    SetLength(Args, 0);
    while (Look <> nil) and (Look.Kind <> tkRParan) do
    begin
      Match(mfYes, [tkIdent, tkInt, tkFloat, tkString]);
      SetLength(Args, Length(Args) + 1);
      Args[High(Args)] := Look;
      Next;
      Match(mfYes, [tkComma, tkRParan]);
      if Look.Kind = tkComma then
      begin
        Next;
        Match(mfYes, [tkIdent, tkInt, tkFloat, tkString]); // catch "(ident,)"
      end;
    end;
    Match(mfYes, [tkRParan]);
    Next;
    Result :=  TBoolNodeConst.Create(EvalBoolFunction(Ident, Args));
  end
  else
  begin
    Next;
    Result := TBoolNodeConst.Create(GetConstBoolValue(Ident.Value));
  end;
end;

function TExpressionParser.RelOp: IBoolNode;
var
  Exp2: IExprNode;
  Op: TToken;
  Exp: IExprNode;
begin
  Exp := Expression;
  if IsRelOp then
  begin
    Op := Look;
    Next;
    Exp2 := Expression;
    Result := TBoolNodeRelOp.Create(Op.Kind, Op.Value, Exp, Exp2);
  end
  else
    Result := TBoolNodeExp.Create(Exp);
end;

function TExpressionParser.BoolFactor: IBoolNode;
var
  Op: TToken;
  Exp2: IBoolNode;
begin
  Result := BoolSingleFactor;
  if IsBoolOp then
  begin
    Op := Look;
    Next;
    Exp2 := BoolSingleFactor;
    Result := TBoolNodeBoolOp.Create(Op.Kind, Op.Value, Result, Exp2);
  end;
end;

function TExpressionParser.BoolSingleFactor: IBoolNode;
var
  Node: IBoolNode;
  LParan, Ident: TToken;
begin
  Match(mfYes, [tkInt, tkFloat, tkString, tkLParan, tkIdent]);
  case Look.Kind of
    tkLParan:
      begin
        LParan := Look;
        Next;
        Node := BoolExpression;
        Match(mfYes, [tkRParan]);
        Next;
        if IsRelOp then
        begin
          Rescan(LParan);
          Next;
          Node := RelOp;
        end;
        Result := Node;
      end;

    tkIdent:
      begin
        // it could be a simple Ident or a part of an expression
        Ident := Look;
        Next;
        if IsRelOp or Match(mfNo, [tkPlus, tkMinus, tkI_Or, tkI_Xor,
                                   tkMultiply, tkDivide, tkI_Mod, tkI_And]) then
        begin
          Rescan(Ident);
          Next;
          Node := RelOp;
        end
        else
        begin
          Rescan(Ident);
          Next;
          Node := BoolIdent;
        end;
        Result := Node;
      end;

    tkInt, tkFloat, tkString:
      Result := RelOp;
  else
    Result := nil; // never reached but the compiler does not know that Match raises an exception
  end;
end;

function TExpressionParser.Parse: Boolean;
var
  B: Boolean;
  Bn: IBoolNode;
  BoolError: string;
begin
  Next;
  BoolError := '';
  Bn := BoolExpression;
  B := Bn.GetValue(BoolError);
  if BoolError <> '' then
    Error(BoolError);
  Result := B;
end;

function TExpressionParser.GetConstBoolValue(const Name: string): Boolean;
begin
  if SameText(Name, 'True') then
    Result := True
//  else if SameText(Name, 'False') then
//    Result := False
  else
    Result := False;
end;

function TExpressionParser.GetConstValue(const Name: string; out Kind: TTokenKind; out Value: string): Boolean;
begin
  Result := False;
end;

end.
