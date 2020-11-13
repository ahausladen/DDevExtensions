{******************************************************************************}
{*                                                                            *}
{* (C) 2005,2006 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit CtrlUtils;

{$I DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, ComCtrls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Hooking; 

type
  IListViewSort = interface
    ['{3EB61BB0-EAD6-4E9D-BA1C-86618B715592}']
    procedure Sort(AColumn: Integer);
    procedure Resort(NewColumn: Integer = -1);

    function GetColumn: Integer;
    function GetListView: TListView;
    function GetSortAsc: Boolean;
    procedure SetSortAsc(Value: Boolean);

    property Column: Integer read GetColumn write Sort;
    property SortAsc: Boolean read GetSortAsc write SetSortAsc;
    property ListView: TListView read GetListView;
  end;

  TListViewSort = class;

  TSortKind = (skText, skNumeric, skFloat, skDate, skPercentage, skDirectory);
  TSortKindEvent = procedure(Sender: TListViewSort; Column: Integer; var SortKind: TSortKind) of object;

  TListViewSort = class(TComponent, IListViewSort)
  private
    FSortAsc: Boolean;
    FColumn: Integer;
    FListView: TListView;
    FOnSortKind: TSortKindEvent;
  protected
    { The function calling Compare will invert the result automatically if necessary. }
    function Compare(Item1, Item2: TListItem): Integer; virtual;

    function GetColumn: Integer;
    function GetListView: TListView;
  public
    constructor Create(AListView: TListView); reintroduce;

    procedure Sort(AColumn: Integer);
    procedure Resort(NewColumn: Integer = -1);
    function GetSortAsc: Boolean;
    procedure SetSortAsc(Value: Boolean);

    property ListView: TListView read FListView;
    property Column: Integer read FColumn;
    property SortAsc: Boolean read FSortAsc write SetSortAsc;

    property OnSortKind: TSortKindEvent read FOnSortKind write FOnSortKind;
  end;

procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; DefaultItem: TListItem);
procedure ListViewClear(ListView: TListView);

procedure EnableWinCtrl(Ctrl: TWinControl; Enable: Boolean);

function ConfirmDlg2(const Text: string; DefBtn: TMsgDlgBtn = mbYes): Boolean; overload;
function ConfirmDlg2(const Fmt: string; const Args: array of const;
  DefBtn: TMsgDlgBtn = mbYes): Boolean; overload;

procedure InformDlg(const Text: string); overload;
procedure InformDlg(const Fmt: string; const Args: array of const); overload;

procedure ErrorDlg(const Text: string); overload;
procedure ErrorDlg(const Fmt: string; const Args: array of const); overload;

procedure WarningDlg(const Text: string); overload;
procedure WarningDlg(const Fmt: string; const Args: array of const); overload;

procedure FixListViewFlicker(ListView: TListView);

implementation

type
  TWinControlProtected = class(TWinControl);

procedure FixListViewFlicker(ListView: TListView);
begin
  ListView.DoubleBuffered := True;
end;

procedure EnableWinCtrl(Ctrl: TWinControl; Enable: Boolean);
begin
  Ctrl.Enabled := Enable;
  if Enable then
    TWinControlProtected(Ctrl).Color := clWindow
  else
    TWinControlProtected(Ctrl).Color := clBtnFace;
end;

function ConfirmDlg2(const Text: string; DefBtn: TMsgDlgBtn = mbYes): Boolean;
var
  Msg: TForm;
begin
  Msg := CreateMessageDialog(Text, mtConfirmation, [mbYes, mbNo]);
  try
    if DefBtn = mbNo then
      Msg.ActiveControl := Msg.FindComponent('No') as TButton;
    Result := Msg.ShowModal = mrYes;
  finally
    Msg.Free;
  end;
end;

function ConfirmDlg2(const Fmt: string; const Args: array of const;
  DefBtn: TMsgDlgBtn = mbYes): Boolean;
begin
  Result := ConfirmDlg2(Format(Fmt, Args), DefBtn);
end;

procedure InformDlg(const Text: string);
begin
  MessageDlg(Text, mtInformation, [mbOk], 0);
end;

procedure InformDlg(const Fmt: string; const Args: array of const);
begin
  InformDlg(Format(Fmt, Args));
end;

procedure ErrorDlg(const Text: string);
begin
  MessageDlg(Text, mtError, [mbOk], 0);
end;

procedure ErrorDlg(const Fmt: string; const Args: array of const);
begin
  ErrorDlg(Format(Fmt, Args));
end;

procedure WarningDlg(const Text: string);
begin
  MessageDlg(Text, mtWarning, [mbOk], 0);
end;

procedure WarningDlg(const Fmt: string; const Args: array of const);
begin
  WarningDlg(Format(Fmt, Args));
end;


procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; DefaultItem: TListItem);
var
  Item: TListItem;
  LV: TListView;
begin
  LV := TListView(Sender);
  { There must be always a selected item }
  Item := LV.GetItemAt(X, Y);
  if Item = nil then
    Item := LV.GetNearestItem(Point(X, Y), sdAbove)
  else
    Exit;
  if (Item = nil) and (DefaultItem <> nil) then
    Item := DefaultItem;
  if (Item = nil) and (LV.Items.Count > 0) then
    Item := LV.Items[LV.Items.Count - 1];
  if Item <> nil then
    if not Item.Selected then
      Item.Selected := True;
end;

procedure ListViewClear(ListView: TListView);
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
  finally
    ListView.Items.EndUpdate;
  end;
end;

{ TListViewSort }

function _SortListView(Item1, Item2: TListItem; LVSort: TListViewSort): Integer stdcall;
begin
  Result := LVSort.Compare(Item1, Item2);

  if Result < -1 then
    Result := -1
  else if Result > 1 then
    Result := 1;

  if LVSort.FSortAsc then
    Result := -1 * Result;
end;

function CompareFloat(f1, f2: Double): Integer;
begin
  if f1 < f2 then
    Result := -1
  else
  if f1 > f2 then
    Result := 1
  else
    Result := 0;
end;

function StrToFloatDef(const S: string; Default: Double): Double;
begin
  try
    Result := StrToFloat(S);
  except
    on EConvertError do
      Result := Default;
  end;
end;

function TListViewSort.Compare(Item1, Item2: TListItem): Integer;
var
  Kind: TSortKind;
  S1, S2: string;
begin
  Result := 0;
  Kind := skText;
  if Assigned(FOnSortKind) then
    FOnSortKind(Self, Column, Kind);

  if Column = 0 then
  begin
    S1 := Item1.Caption;
    S2 := Item2.Caption;
  end
  else
  begin
    S1 := '';
    if (Column > Item1.SubItems.Count) or (Column > Item2.SubItems.Count) then
      Exit;
    S1 := Item1.SubItems[Column - 1];
    S2 := Item2.SubItems[Column - 1];
  end;

  case Kind of
    skText:
      Result := AnsiCompareText(S1, S2);
    skNumeric:
      Result := StrToIntDef(S2, 0) - StrToIntDef(S1 , 0);
    skFloat:
      Result := CompareFloat(StrToFloatDef(S2, 0), StrToFloatDef(S1, 0));
    skPercentage:
      begin
        S1 := Trim(Copy(S1, 1, Pos('%', S1) - 1));
        S2 := Trim(Copy(S2, 1, Pos('%', S2) - 1));
        Result := CompareFloat(StrToFloatDef(S2, 0), StrToFloatDef(S1, 0));
      end;
    skDate:
      Result := CompareFloat(StrToFloatDef(S1, 0), StrToFloatDef(S2, 0));
    skDirectory:
      begin
        {$IFDEF MSWINDOWS}
        Result := AnsiCompareText(ExtractFileDir(S1), ExtractFileDir(S2));
        if Result = 0 then
          Result := AnsiCompareText(S1, S2);
        {$ELSE}
        Result := AnsiCompareStr(ExtractFileDir(S1), ExtractFileDir(S2));
        if Result = 0 then
          Result := AnsiCompareStr(S1, S2);
        {$ENDIF MSWINDOWS}
      end;
  end;
end;

constructor TListViewSort.Create(AListView: TListView);
begin
  inherited Create(AListView);
  FListView := AListView;
  FSortAsc := True;
end;

function TListViewSort.GetColumn: Integer;
begin
  Result := FColumn;
end;

function TListViewSort.GetListView: TListView;
begin
  Result := FListView;
end;

function TListViewSort.GetSortAsc: Boolean;
begin
  Result := FSortAsc;
end;

procedure TListViewSort.Resort(NewColumn: Integer);
begin
  if NewColumn <= -1 then
    NewColumn := FColumn;
  FColumn := -1;
  Sort(NewColumn);
end;

procedure TListViewSort.SetSortAsc(Value: Boolean);
begin
  FSortAsc := Value;
  Resort;
end;

procedure TListViewSort.Sort(AColumn: Integer);
begin
  AColumn := Abs(AColumn);
  if FColumn = AColumn then
    FSortAsc := not FSortAsc;
  FColumn := AColumn;
  if Cardinal(FColumn) >= Cardinal(ListView.Columns.Count) then
    Exit;

  ListView.CustomSort(@_SortListView, NativeInt(Self));
  if ListView.Selected <> nil then
    ListView.Selected.MakeVisible(False);
end;

end.

