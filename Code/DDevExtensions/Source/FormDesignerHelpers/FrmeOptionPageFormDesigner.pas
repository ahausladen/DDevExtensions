{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2007 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageFormDesigner;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ToolsAPI, FrmTreePages, PluginConfig, StdCtrls,
  ModuleData, FrmeBase, ExtCtrls;

type
  TFormDesigner = class(TPluginConfig)
  private
    FActive: Boolean;
    FLabelMargin: Boolean;
    FRemoveExplicitProperty: Boolean;
    FRemovePixelsPerInchProperty: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetLabelMargin(const Value: Boolean);
    procedure SetRemoveExplicitProperty(const Value: Boolean);
    procedure SetRemovePixelsPerInchProperty(const Value: Boolean);
  protected
    function GetOptionPages: TTreePage; override;
    procedure Init; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateHooks;
  published
    property Active: Boolean read FActive write SetActive;
    property LabelMargin: Boolean read FLabelMargin write SetLabelMargin;
    property RemoveExplicitProperty: Boolean read FRemoveExplicitProperty write SetRemoveExplicitProperty;
    property RemovePixelsPerInchProperty : Boolean read FRemovePixelsPerInchProperty write SetRemovePixelsPerInchProperty;
  end;

  TFrameOptionPageFormDesigner = class(TFrameBase, ITreePageComponent)
    cbxActive: TCheckBox;
    cbxLabelMargin: TCheckBox;
    chkRemoveExplicitProperties: TCheckBox;
    chkRemovePixelsPerInchProperties: TCheckBox;
    procedure cbxActiveClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FFormDesigner: TFormDesigner;
  public
    { Public-Deklarationen }
    procedure SetUserData(UserData: TObject);
    procedure LoadData;
    procedure SaveData;
    procedure Selected;
    procedure Unselected;
  end;

{$IFDEF INCLUDE_FORMDESIGNER}

procedure InitPlugin(Unload: Boolean);

{$ENDIF INCLUDE_FORMDESIGNER}

implementation

uses
  Main, LabelMarginHelper,
{$IFDEF COMPILER110_UP}
  RemovePixelsPerInchProperty,
{$ENDIF COMPILER110_UP}
  RemoveExplicitProperty;

{$R *.dfm}

{$IFDEF INCLUDE_FORMDESIGNER}

var
  FormDesigner: TFormDesigner;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    FormDesigner := TFormDesigner.Create
  else
    FreeAndNil(FormDesigner);
end;

{$ENDIF INCLUDE_FORMDESIGNER}

{ TFrameOptionPageFormDesigner }

procedure TFrameOptionPageFormDesigner.cbxActiveClick(Sender: TObject);
begin
  cbxLabelMargin.Enabled := cbxActive.Checked;
  chkRemoveExplicitProperties.Enabled := cbxActive.Checked;
{$IFDEF COMPILER110_UP}
    chkRemoveExplicitProperties.Enabled := cbxActive.Checked;
{$ELSE}
    chkRemoveExplicitProperties.Enabled := False;
{$ENDIF COMPILER110_UP}
end;

procedure TFrameOptionPageFormDesigner.SetUserData(UserData: TObject);
begin
  FFormDesigner := UserData as TFormDesigner;
end;

procedure TFrameOptionPageFormDesigner.LoadData;
begin
  cbxActive.Checked := FFormDesigner.Active;
  cbxLabelMargin.Checked := FFormDesigner.LabelMargin;
  chkRemoveExplicitProperties.Checked := FFormDesigner.RemoveExplicitProperty;
  chkRemovePixelsPerInchProperties.Checked := FFormDesigner.RemovePixelsPerInchProperty;
{$IFDEF COMPILER110_UP}
    chkRemoveExplicitProperties.Enabled := cbxActive.Checked;
{$ELSE}
    chkRemoveExplicitProperties.Enabled := False;
{$ENDIF COMPILER110_UP}
  cbxActiveClick(cbxActive);
end;

procedure TFrameOptionPageFormDesigner.SaveData;
begin
  FFormDesigner.LabelMargin := cbxLabelMargin.Checked;
  FFormDesigner.RemoveExplicitProperty := chkRemoveExplicitProperties.Checked;
  FFormDesigner.RemovePixelsPerInchProperty := chkRemovePixelsPerInchProperties.Checked;

  FFormDesigner.Active := cbxActive.Checked;
  FFormDesigner.Save;
end;

procedure TFrameOptionPageFormDesigner.Selected;
begin
end;

procedure TFrameOptionPageFormDesigner.Unselected;
begin
end;

{ TFormDesigner }

constructor TFormDesigner.Create;
begin
  inherited Create(AppDataDirectory + '\FormDesigner.xml', 'FormDesigner');
end;

destructor TFormDesigner.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TFormDesigner.Init;
begin
  inherited Init;
  LabelMargin := True;
  RemoveExplicitProperty := False;
  RemovePixelsPerInchProperty := False;
  Active := True;
end;

procedure TFormDesigner.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    UpdateHooks;
  end;
end;

procedure TFormDesigner.SetLabelMargin(const Value: Boolean);
begin
  if Value <> FLabelMargin then
  begin
    FLabelMargin := Value;
    if Active then
      UpdateHooks;
  end;
end;

procedure TFormDesigner.SetRemoveExplicitProperty(const Value: Boolean);
begin
  if Value <> FRemoveExplicitProperty then
  begin
    FRemoveExplicitProperty := Value;
    if Active then
      UpdateHooks;
  end;
end;

procedure TFormDesigner.SetRemovePixelsPerInchProperty(const Value: Boolean);
begin
  if Value <> FRemovePixelsPerInchProperty then
  begin
    FRemovePixelsPerInchProperty := Value;
    if Active then
      UpdateHooks;
  end;;
end;

procedure TFormDesigner.UpdateHooks;
begin
  {$IFDEF INCLUDE_FORMDESIGNER}
  SetLabelMarginActive(Active and LabelMargin);
  SetRemoveExplicitPropertyActive(Active and RemoveExplicitProperty);
{$IFDEF COMPILER110_UP}
  SetRemovePixelsPerInchPropertyActive(Active and RemovePixelsPerInchProperty);
{$ENDIF COMPILER110_UP}
  {$ENDIF INCLUDE_FORMDESIGNER}
end;

function TFormDesigner.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Form Designer', TFrameOptionPageFormDesigner, Self);
end;

end.
