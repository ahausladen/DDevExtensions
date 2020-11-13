{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmProjectSettingsEditOptions;

{$I ..\DelphiExtension.inc}

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ProjectSettingsData, StdCtrls, CheckLst, FrmBase;

type
  TFormProjectSettingsEditOptions = class(TFormBase)
    btnOk: TButton;
    btnCancel: TButton;
    clbOptions: TCheckListBox;
    lblCaption: TLabel;
    btnCheckAll: TButton;
    btnUncheckAll: TButton;
    btnDefault: TButton;
    btnToggle: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnToggleClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FOrgListBoxWndProc: TWndMethod;
    function DoExecute(ASettings: TProjectSetting): Boolean;
    procedure ListBoxWndProc(var Msg: TMessage);
  public
    { Public-Deklarationen }
    class function Execute(ASettings: TProjectSetting): Boolean;
  end;

{var
  FormProjectSettingsEditOptions: TFormProjectSettingsEditOptions;}

implementation

{$R *.dfm}

{ TFormProjectSettingsEditOptions }

class function TFormProjectSettingsEditOptions.Execute(ASettings: TProjectSetting): Boolean;
begin
  if ASettings <> nil then
  begin
    with Self.Create(nil) do
    try
      Result := DoExecute(ASettings);
    finally
      Free;
    end;
  end
  else
    Result := False;
end;

function TFormProjectSettingsEditOptions.DoExecute(ASettings: TProjectSetting): Boolean;
var
  i, Index: Integer;
begin
  for i := 0 to ASettings.Count - 1 do
  begin
    Index := clbOptions.Items.Add(ASettings.Items[i].Name);
    clbOptions.Checked[Index] := ASettings.Items[i].Active;
  end;
  Caption := ASettings.Name + ' - Option';
  Result := ShowModal = mrOk;
  if Result then
  begin
    for i := 0 to clbOptions.Items.Count - 1 do
    begin
      Index := ASettings.IndexOf(clbOptions.Items[i]);
      if Index <> -1 then
        ASettings.Items[Index].Active := clbOptions.Checked[Index];
    end;
  end;
end;

procedure TFormProjectSettingsEditOptions.FormCreate(Sender: TObject);
begin
  FOrgListBoxWndProc := clbOptions.WindowProc;
  clbOptions.WindowProc := ListBoxWndProc;

  clbOptions.Anchors := [akLeft, akTop, akRight, akBottom];
  btnOk.Anchors := [akRight, akBottom];
  btnCancel.Anchors := [akRight, akBottom];
  btnCheckAll.Anchors := [akLeft, akBottom];
  btnUncheckAll.Anchors := [akLeft, akBottom];
  btnDefault.Anchors := [akLeft, akBottom];
  btnToggle.Anchors := [akLeft, akBottom];
end;

procedure TFormProjectSettingsEditOptions.ListBoxWndProc(var Msg: TMessage);
begin
  if Msg.Msg = CN_DRAWITEM then
  begin
    { fix bug }
    with clbOptions do
      if (Items.Count = 0) or (TWMDrawItem(Msg).DrawItemStruct^.itemID >= UINT(Items.Count)) then
        Exit;
  end;

  if Assigned(FOrgListBoxWndProc) then
    FOrgListBoxWndProc(Msg);
end;

procedure TFormProjectSettingsEditOptions.btnCheckAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to clbOptions.Items.Count - 1 do
    clbOptions.Checked[i] := Sender = btnCheckAll;
end;

procedure TFormProjectSettingsEditOptions.btnDefaultClick(Sender: TObject);
var
  i, Index: Integer;
  List: TStrings;
begin
  List := TStringList.Create;
  try
    TProjectSettingList.FillOptionNames(List);
    for i := 0 to clbOptions.Items.Count - 1 do
    begin
      Index := List.IndexOf(clbOptions.Items[i]);
      clbOptions.Checked[i] := (Index = -1) or (List.Objects[Index] = nil);
    end;
  finally
    List.Free;
  end;
end;

procedure TFormProjectSettingsEditOptions.btnToggleClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to clbOptions.Items.Count - 1 do
    clbOptions.Checked[i] := not clbOptions.Checked[i];
end;

end.
