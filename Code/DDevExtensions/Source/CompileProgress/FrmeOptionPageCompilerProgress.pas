{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2011 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageCompilerProgress;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, FrmeBase, StdCtrls, ExtCtrls,
  Dialogs, FrmTreePages, CompileProgress;

type
  TFrameOptionPageCompilerProgress = class(TFrameBase, ITreePageComponent)
    cbxDisableRebuildDlg: TCheckBox;
    chkAutoSaveAfterSuccessfulCompile: TCheckBox;
    chkLastCompileVersionInfo: TCheckBox;
    edtLastCompileVersionInfoFormat: TEdit;
    chkAskBeforeCompilingFileFromDiffernetProject: TCheckBox;
    chkReleaseCompilerUnitCache: TCheckBox;
    chkReleaseCompilerUnitCacheHigh: TCheckBox;
    procedure chkLastCompileVersionInfoClick(Sender: TObject);
    procedure chkReleaseCompilerUnitCacheClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FCompileProgress: TCompileProgress;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;

    procedure LoadData;
    procedure SaveData;
    procedure Selected;
    procedure Unselected;
    procedure SetUserData(UserData: TObject);
  end;

implementation

{$R *.dfm}

{ TFrameOptionPageCompilerProgress }

procedure TFrameOptionPageCompilerProgress.SetUserData(UserData: TObject);
begin
  FCompileProgress := UserData as TCompileProgress;
end;

procedure TFrameOptionPageCompilerProgress.chkLastCompileVersionInfoClick(Sender: TObject);
begin
  inherited;
  edtLastCompileVersionInfoFormat.Enabled := chkLastCompileVersionInfo.Checked;
  if edtLastCompileVersionInfoFormat.Enabled then
    edtLastCompileVersionInfoFormat.Color := clWindow
  else
    edtLastCompileVersionInfoFormat.Color := clBtnFace;
end;

procedure TFrameOptionPageCompilerProgress.chkReleaseCompilerUnitCacheClick(Sender: TObject);
begin
  inherited;
  chkReleaseCompilerUnitCacheHigh.Enabled := chkReleaseCompilerUnitCache.Checked;
end;

constructor TFrameOptionPageCompilerProgress.Create(AOwner: TComponent);
{$IF CompilerVersion >= 22.0} // XE has its own option
var
  Diff: Integer;
{$IFEND}
begin
  inherited Create(AOwner);
  {$IF CompilerVersion >= 22.0} // XE has its own option
  Diff := chkAutoSaveAfterSuccessfulCompile.Top - cbxDisableRebuildDlg.Top;
  cbxDisableRebuildDlg.Free;

  chkAutoSaveAfterSuccessfulCompile.Top := chkAutoSaveAfterSuccessfulCompile.Top - Diff;
  chkAskBeforeCompilingFileFromDiffernetProject.Top := chkAskBeforeCompilingFileFromDiffernetProject.Top - Diff;
  chkLastCompileVersionInfo.Top := chkLastCompileVersionInfo.Top - Diff;
  edtLastCompileVersionInfoFormat.Top := edtLastCompileVersionInfoFormat.Top - Diff;
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // XE2+ changed how version info is written
  chkLastCompileVersionInfo.Visible := False;
  edtLastCompileVersionInfoFormat.Visible := False;
  {$IFEND}
end;

procedure TFrameOptionPageCompilerProgress.LoadData;
begin
  chkReleaseCompilerUnitCache.Checked := FCompileProgress.ReleaseCompilerUnitCache;
  chkReleaseCompilerUnitCacheHigh.Checked := FCompileProgress.ReleaseCompilerUnitCacheHigh;
  chkReleaseCompilerUnitCacheHigh.Enabled := chkReleaseCompilerUnitCache.Checked;

  {$IF CompilerVersion < 22.0} // XE has its own option
  cbxDisableRebuildDlg.Checked := FCompileProgress.DisableRebuildDlg;
  {$IFEND}
  chkAutoSaveAfterSuccessfulCompile.Checked := FCompileProgress.AutoSaveAfterSuccessfulCompile;
  {$IF CompilerVersion < 23.0} // XE2+ changed how version info works
  chkLastCompileVersionInfo.Checked := FCompileProgress.LastCompileVersionInfo;
  edtLastCompileVersionInfoFormat.Text := FCompileProgress.LastCompileVersionInfoFormat;
  {$IFEND}
  chkAskBeforeCompilingFileFromDiffernetProject.Checked := FCompileProgress.AskCompileFromDiffProject;

  chkLastCompileVersionInfoClick(chkLastCompileVersionInfo);
end;

procedure TFrameOptionPageCompilerProgress.SaveData;
begin
  FCompileProgress.ReleaseCompilerUnitCache := chkReleaseCompilerUnitCache.Checked;
  FCompileProgress.ReleaseCompilerUnitCacheHigh := chkReleaseCompilerUnitCacheHigh.Checked;
  {$IF CompilerVersion < 22.0} // XE has its own option
  FCompileProgress.DisableRebuildDlg := cbxDisableRebuildDlg.Checked;
  {$IFEND}
  FCompileProgress.AutoSaveAfterSuccessfulCompile := chkAutoSaveAfterSuccessfulCompile.Checked;
  {$IF CompilerVersion < 23.0} // XE2+ changed how version info works
  FCompileProgress.LastCompileVersionInfo := chkLastCompileVersionInfo.Checked;
  FCompileProgress.LastCompileVersionInfoFormat := edtLastCompileVersionInfoFormat.Text;
  {$IFEND}
  FCompileProgress.AskCompileFromDiffProject := chkAskBeforeCompilingFileFromDiffernetProject.Checked;
  FCompileProgress.Save;
end;

procedure TFrameOptionPageCompilerProgress.Selected;
begin
end;

procedure TFrameOptionPageCompilerProgress.Unselected;
begin
end;

end.
