{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageComponentSelector;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComponentSelector, FrmTreePages, ComCtrls, FrmeBase, ExtCtrls;

type
  TFrameOptionPageComponentSelector = class(TFrameBase, ITreePageComponent)
    cbxSimpleSearch: TCheckBox;
    cbxSortByPalette: TCheckBox;
    cbxActive: TCheckBox;
    HotKey: THotKey;
    lblHotkey: TLabel;
    procedure cbxActiveClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FComponentSelector: TComponentSelector;
  public
    { Public-Deklarationen }
    procedure SetUserData(UserData: TObject);
    procedure LoadData;
    procedure SaveData;
    procedure Selected;
    procedure Unselected;
  end;

implementation

{$R *.dfm}

{ TFrameOptionPageComponentSelector }

procedure TFrameOptionPageComponentSelector.SetUserData(UserData: TObject);
begin
  FComponentSelector := UserData as TComponentSelector;
end;

procedure TFrameOptionPageComponentSelector.LoadData;
begin
  cbxActive.Checked := FComponentSelector.ToolBar.Visible;
  cbxSimpleSearch.Checked := FComponentSelector.Edit.CheckBoxSimpleSearch.Checked;
  cbxSortByPalette.Checked := FComponentSelector.Edit.CheckBoxPaletteSort.Checked;
  HotKey.HotKey := FComponentSelector.Hotkey;

  cbxActiveClick(cbxActive);
end;

procedure TFrameOptionPageComponentSelector.SaveData;
begin
  FComponentSelector.ToolBar.Visible := cbxActive.Checked;
  FComponentSelector.Edit.CheckBoxSimpleSearch.Checked := cbxSimpleSearch.Checked;
  FComponentSelector.Edit.CheckBoxPaletteSort.Checked := cbxSortByPalette.Checked;
  FComponentSelector.Hotkey := HotKey.HotKey;
  FComponentSelector.SaveToolbarConfig;
end;

procedure TFrameOptionPageComponentSelector.Selected;
begin
end;

procedure TFrameOptionPageComponentSelector.Unselected;
begin
end;

procedure TFrameOptionPageComponentSelector.cbxActiveClick(Sender: TObject);
begin
  cbxSimpleSearch.Enabled := cbxActive.Checked;
  cbxSortByPalette.Enabled := cbxActive.Checked;
  lblHotkey.Enabled := cbxActive.Checked;
  HotKey.Enabled := cbxActive.Checked;
end;

end.
