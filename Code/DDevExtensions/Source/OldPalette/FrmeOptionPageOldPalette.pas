{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageOldPalette;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FrmTreePages, PluginConfig, ExtCtrls, ComCtrls,
  SimpleXmlIntf, FrmeBase;

type
  TOldPaletteConfig = class(TPluginConfig)
  private
    FActive: Boolean;
    FOrgBandMove: TBandMoveEvent;
    FLoading: Boolean;
    FTop: Integer;
    FLeft: Integer;
    FMultiLine: Boolean;
    FRaggedRight: Boolean;
    FStyle: TTabStyle;
    FAlphaSortPopupMenu: Boolean;
    FSmallFonts: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetAlphaSortPopupMenu(const Value: Boolean);
  protected
    procedure DoBandMove(Sender: TObject; Control: TControl; var ARect: TRect);
    function GetOptionPages: TTreePage; override;
    procedure Init; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromXml(Node: IXmlNode); override;
  published
    property Active: Boolean read FActive write SetActive;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;

    property MultiLine: Boolean read FMultiLine write FMultiLine;
    property RaggedRight: Boolean read FRaggedRight write FRaggedRight;
    property AlphaSortPopupMenu: Boolean read FAlphaSortPopupMenu write SetAlphaSortPopupMenu;
    property SmallFonts: Boolean read FSmallFonts write FSmallFonts;
    property Style: TTabStyle read FStyle write FStyle;
  end;

  TFrameOptionPageOldPalette = class(TFrameBase, ITreePageComponent)
    cbxActive: TCheckBox;
    cbxMultiline: TCheckBox;
    cbxRaggedRight: TCheckBox;
    cbxStyle: TComboBox;
    lblStyleCaption: TLabel;
    chkAlphaSortPopupMenu: TCheckBox;
    chkSmallFonts: TCheckBox;
    procedure cbxActiveClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FOldPaletteConfig: TOldPaletteConfig;
  public
    { Public-Deklarationen }
    procedure SetUserData(UserData: TObject);
    procedure LoadData;
    procedure SaveData;
    procedure Selected;
    procedure Unselected;
  end;

procedure InitPlugin(Unload: Boolean);

implementation

uses
  OldPalette, Main, ToolsAPIHelpers;

{$R *.dfm}

var
  OldPaletteConfig: TOldPaletteConfig;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    OldPaletteConfig := TOldPaletteConfig.Create
  else
    FreeAndNil(OldPaletteConfig);
end;

{ TFrameOptionPageOldPalette }

procedure TFrameOptionPageOldPalette.SetUserData(UserData: TObject);
begin
  FOldPaletteConfig := UserData as TOldPaletteConfig;
end;

procedure TFrameOptionPageOldPalette.cbxActiveClick(Sender: TObject);
begin
  inherited;
  cbxMultiline.Enabled := cbxActive.Checked;
  cbxRaggedRight.Enabled := cbxActive.Checked;
  lblStyleCaption.Enabled := cbxActive.Checked;
  chkAlphaSortPopupMenu.Enabled := cbxActive.Checked;
  chkSmallFonts.Enabled := cbxActive.Checked;
  cbxStyle.Enabled := cbxActive.Checked;
end;

procedure TFrameOptionPageOldPalette.LoadData;
begin
  cbxActive.Checked := FOldPaletteConfig.Active;
  cbxMultiline.Checked := FOldPaletteConfig.MultiLine;
  cbxRaggedRight.Checked := FOldPaletteConfig.RaggedRight;
  chkAlphaSortPopupMenu.Checked := FOldPaletteConfig.AlphaSortPopupMenu;
  chkSmallFonts.Checked := FOldPaletteConfig.SmallFonts;
  cbxStyle.ItemIndex := Integer(FOldPaletteConfig.Style);

  cbxActiveClick(cbxActive);
end;

procedure TFrameOptionPageOldPalette.SaveData;
begin
  FOldPaletteConfig.Active := cbxActive.Checked;
  FOldPaletteConfig.MultiLine := cbxMultiline.Checked;
  FOldPaletteConfig.RaggedRight := cbxRaggedRight.Checked;
  FOldPaletteConfig.AlphaSortPopupMenu := chkAlphaSortPopupMenu.Checked;
  FOldPaletteConfig.SmallFonts := chkSmallFonts.Checked;
  if cbxStyle.ItemIndex <> -1 then
    FOldPaletteConfig.Style := TTabStyle(cbxStyle.ItemIndex);
  FOldPaletteConfig.Save;

  if FrameOldPalette <> nil then
    FrameOldPalette.InitTabControl(FOldPaletteConfig);
end;

procedure TFrameOptionPageOldPalette.Selected;
begin
end;

procedure TFrameOptionPageOldPalette.Unselected;
begin
end;

{ TOldPaletteConfig }

constructor TOldPaletteConfig.Create;
begin
  inherited Create(AppDataDirectory + '\OldPalette.xml', 'OldPalette');
end;

destructor TOldPaletteConfig.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TOldPaletteConfig.DoBandMove(Sender: TObject; Control: TControl; var ARect: TRect);
begin
  if Control = FrameOldPalette then
  begin
    ARect.Right := TControl(Sender).ClientWidth;
    if ARect.Left >= ARect.Right - 10 then
      ARect.Left := ARect.Right - 10;
  end;
  if Assigned(FOrgBandMove) then
    FOrgBandMove(Sender, Control, ARect);
end;

function TOldPaletteConfig.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Old Palette', TFrameOptionPageOldPalette, Self);
end;

procedure TOldPaletteConfig.Init;
begin
  inherited Init;
  FActive := False;
  FLeft := 0;
  FTop := -1;
  FMultiLine := False;
  FRaggedRight := True;
  FStyle := tsTabs;
  FSmallFonts := False;
end;

procedure TOldPaletteConfig.LoadFromXml(Node: IXmlNode);
begin
  FLoading := True;
  inherited LoadFromXml(Node);
  FLoading := False;
  if Active then
  begin
    FActive := False;
    SetActive(True);
  end;
end;

procedure TOldPaletteConfig.SetActive(const Value: Boolean);
var
  ControlBar: TControlBar;
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FLoading then
      Exit;
    ControlBar := TControlBar(Application.MainForm.FindComponent('ControlBar1'));
    if not Active then
    begin
      if ControlBar <> nil then
        ControlBar.OnBandMove := FOrgBandMove;
      FrameOldPalette.Free;
    end
    else
    begin
      if ControlBar <> nil then
      begin
        FOrgBandMove := ControlBar.OnBandMove;
        ControlBar.OnBandMove := DoBandMove;
        if not Assigned(FrameOldPalette) then
          FrameOldPalette := TFrameOldPalette.Create(Self);
        FrameOldPalette.Left := FLeft;
        if FTop = -1 then
          FTop := ControlBar.RowSize * 3;
        FrameOldPalette.Top := FTop;
        FrameOldPalette.Parent := ControlBar;
        FrameOldPalette.Init(Self);
      end;
    end;
  end;
end;

procedure TOldPaletteConfig.SetAlphaSortPopupMenu(const Value: Boolean);
begin
  if Value <> FAlphaSortPopupMenu then
  begin
    FAlphaSortPopupMenu := Value;
    if FrameOldPalette <> nil then
      FrameOldPalette.RebuildPaletteMenu;
  end;
end;

end.

