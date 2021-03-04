{******************************************************************************}
{*                                                                            *}
{* Container classes                                                          *}
{*                                                                            *}
{* (C) 2005 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DelphiParserContainers;

{$I ..\jedi\jedi.inc}

{$IFDEF COMPILER10_UP}
 {$DEFINE D9}
{$ENDIF}

interface

uses
  SysUtils, Classes;

type
  THashtableStringList = class(TStringList)
  protected
    function CompareStrings(const S1: string; const S2: string): Integer; override;
  end;

  THashtable = class(TObject)
  private
    FItems: THashtableStringList;
    FOwnsObjects: Boolean;
    function GetCount: Integer; {$IFDEF D9}inline;{$ENDIF}
    function GetValue(const AKey: string): TObject; {$IFDEF D9}inline;{$ENDIF}
    function GetItem(Index: Integer): TObject; {$IFDEF D9}inline;{$ENDIF}
  public
    constructor Create(CaseSensitive: Boolean; AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    procedure Clear;

    procedure Remove(const AKey: string);
    procedure Add(const AKey: string; AValue: TObject); {$IFDEF D9}inline;{$ENDIF}
    function Contains(const AKey: string): Boolean; {$IFDEF D9}inline;{$ENDIF}

    property Values[const AKey: string]: TObject read GetValue; default;
    property Items[Index: Integer]: TObject read GetItem;
    property Count: Integer read GetCount;
  end;

  TIntegerList = class(TList)
  private
    function GetItem(Index: Integer): Integer; {$IFDEF D9}inline;{$ENDIF}
    procedure SetItem(Index: Integer; const Value: Integer);{$IFDEF D9}inline;{$ENDIF}
  public
    function Add(Value: Integer): Integer; {$IFDEF D9}inline;{$ENDIF}
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
  end;

  TStringDictionary = class(TObject)
  private
    FItems: THashtableStringList;
    FValues: TStringList;
    function GetCount: Integer;
    function GetValue(const AKey: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AKey: string; const AValue: string);
    function Contains(const AKey: string): Boolean;
    function Find(const AKey: string): string;

    property Values[const AKey: string]: string read GetValue; default;
    property Count: Integer read GetCount;
  end;

  TStringCollection = class(TStringList)
  public
    function Contains(const Value: string): Boolean;
    procedure RemoveAt(Index: Integer);
  end;

implementation

{ THashtable }

constructor THashtable.Create(CaseSensitive: Boolean; AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
  FItems := THashtableStringList.Create;
  FItems.Sorted := True;
  FItems.Duplicates := dupError;
  {$IFDEF COMPILER6_UP}
  FItems.CaseSensitive := CaseSensitive;
  {$ELSE}
  Assert(CaseSensitive = False, 'TStringList has no CaseSensitive property in Delphi 5');
  {$ENDIF COMPILER6_UP}
end;

destructor THashtable.Destroy;
var
  i: Integer;
begin
  if FOwnsObjects then
    for i := 0 to Count - 1 do
      FItems.Objects[i].Free;
  FItems.Free;
  inherited Destroy;
end;

function THashtable.Contains(const AKey: string): Boolean;
begin
  Result := FItems.IndexOf(AKey) >= 0;
end;

function THashtable.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure THashtable.Add(const AKey: string; AValue: TObject);
begin
  FItems.AddObject(AKey, AValue);
end;

function THashtable.GetValue(const AKey: string): TObject;
var
  Index: Integer;
begin
  if FItems.Find(AKey, Index) then
    Result := FItems.Objects[Index]
  else
    Result := nil;
end;

procedure THashtable.Remove(const AKey: string);
var
  Index: Integer;
begin
  if FItems.Find(AKey, Index) then
  begin
    if FOwnsObjects then
      FItems.Objects[Index].Free;
    FItems.Delete(Index);
  end;
end;

procedure THashtable.Clear;
begin
  FItems.Clear;
end;

function THashtable.GetItem(Index: Integer): TObject;
begin
  Result := FItems.Objects[Index];
end;

{ TIntegerList }

function TIntegerList.GetItem(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

function TIntegerList.Add(Value: Integer): Integer;
begin
  Result := inherited Add(Pointer(Value));
end;

procedure TIntegerList.SetItem(Index: Integer; const Value: Integer);
begin
  inherited Items[Index] := Pointer(Value);
end;

{ TStringCollection }

function TStringCollection.Contains(const Value: string): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

procedure TStringCollection.RemoveAt(Index: Integer);
begin
  Delete(Index);
end;

{ TStringDictionary }

constructor TStringDictionary.Create;
begin
  inherited Create;
  FItems := THashtableStringList.Create;
  FValues := TStringList.Create;
  FItems.Sorted := True;
  FItems.Duplicates := dupError;
end;

destructor TStringDictionary.Destroy;
begin
  FItems.Free;
  FValues.Free;
  inherited Destroy;
end;

function TStringDictionary.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TStringDictionary.GetValue(const AKey: string): string;
var
  Index: Integer;
begin
  Result := '';
  if FItems.Find(AKey, Index) then
    if Integer(FItems.Objects[Index]) >= 0 then
      Result := FValues[Integer(FItems.Objects[Index])];
end;

procedure TStringDictionary.Add(const AKey: string; const AValue: string);
begin
  FItems.AddObject(AKey, Pointer(FValues.Add(AValue)));
end;

function TStringDictionary.Contains(const AKey: string): Boolean;
var
  Index: Integer;
begin
  Result := FItems.Find(AKey, Index);
end;

function TStringDictionary.Find(const AKey: string): string;
begin
  Result := Values[AKey];
end;

{ THashtableStringList }

function THashtableStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);
end;

end.
