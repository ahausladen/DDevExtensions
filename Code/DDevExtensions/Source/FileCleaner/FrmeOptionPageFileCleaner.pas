{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageFileCleaner;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ToolsAPI, FrmTreePages, PluginConfig, StdCtrls,
  ModuleData, FrmeBase, ExtCtrls;

type
  TFileCleaner = class(TPluginConfig)
  private
    FModuleNotifier: TModuleDataNotifier;
    FActive: Boolean;
    FRemoveEmptyHistory: Boolean;
    FDeleteDdp: Boolean;
    FRemoveEmptyModel: Boolean;
  protected
    function GetOptionPages: TTreePage; override;
    procedure Init; override;
    procedure DoModuleAfterSave(Data: TModuleData); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write FActive;
    property DeleteDdp: Boolean read FDeleteDdp write FDeleteDdp;
    property RemoveEmptyHistory: Boolean read FRemoveEmptyHistory write FRemoveEmptyHistory;
    property RemoveEmptyModel: Boolean read FRemoveEmptyModel write FRemoveEmptyModel;
  end;

  TFrameOptionPageFileCleaner = class(TFrameBase, ITreePageComponent)
    cbxActive: TCheckBox;
    cbxDeleteDdp: TCheckBox;
    cbxRemoveEmptyModel: TCheckBox;
    cbxRemoveEmptyHistory: TCheckBox;
    procedure cbxActiveClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FFileCleaner: TFileCleaner;
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
  Main;

{$R *.dfm}

var
  FileCleaner: TFileCleaner;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    FileCleaner := TFileCleaner.Create
  else
    FreeAndNil(FileCleaner);
end;

{ TFrameOptionPageFileCleaner }

procedure TFrameOptionPageFileCleaner.cbxActiveClick(Sender: TObject);
begin
  cbxDeleteDdp.Enabled := cbxActive.Checked;
  cbxRemoveEmptyModel.Enabled := cbxActive.Checked;
  cbxRemoveEmptyHistory.Enabled := cbxActive.Checked;
end;

procedure TFrameOptionPageFileCleaner.SetUserData(UserData: TObject);
begin
  FFileCleaner := UserData as TFileCleaner;
end;

procedure TFrameOptionPageFileCleaner.LoadData;
begin
  cbxActive.Checked := FFileCleaner.Active;
  cbxDeleteDdp.Checked := FFileCleaner.DeleteDdp;
  cbxRemoveEmptyModel.Checked := FFileCleaner.RemoveEmptyModel;
  cbxRemoveEmptyHistory.Checked := FFileCleaner.RemoveEmptyHistory;
  {$IFDEF COMPILER9_UP}
  cbxRemoveEmptyModel.Visible := True;
  {$ENDIF COMPILER9_UP}
  {$IFDEF COMPILER10_UP}
  cbxRemoveEmptyHistory.Visible := True;
  {$ENDIF COMPILER10_UP}

  cbxActiveClick(cbxActive);
end;

procedure TFrameOptionPageFileCleaner.SaveData;
begin
  FFileCleaner.DeleteDdp := cbxDeleteDdp.Checked;
  FFileCleaner.RemoveEmptyHistory := cbxRemoveEmptyHistory.Checked;
  FFileCleaner.RemoveEmptyModel := cbxRemoveEmptyModel.Checked;

  FFileCleaner.Active := cbxActive.Checked;
  FFileCleaner.Save;
end;

procedure TFrameOptionPageFileCleaner.Selected;
begin
end;

procedure TFrameOptionPageFileCleaner.Unselected;
begin
end;

{ TFileCleaner }

constructor TFileCleaner.Create;
begin
  inherited Create(AppDataDirectory + '\FileCleaner.xml', 'FileCleaner');

  FModuleNotifier := TModuleDataNotifier.Create;
  FModuleNotifier.AfterSave := DoModuleAfterSave;
end;

destructor TFileCleaner.Destroy;
begin
  FModuleNotifier.Free;
  Active := False;
  inherited Destroy;
end;

procedure TFileCleaner.Init;
begin
  inherited Init;
  DeleteDdp := False;
  RemoveEmptyHistory := True;
  RemoveEmptyModel := True;
  Active := True;
end;

function TFileCleaner.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('File Cleaner', TFrameOptionPageFileCleaner, Self);
end;

procedure TFileCleaner.DoModuleAfterSave(Data: TModuleData);
var
  Ext: string;
  Filename: string;
  IsProjectExt: Boolean;
begin
  if Active then
  begin
    Filename := Data.Module.FileName;
    Ext := AnsiLowerCase(ExtractFileExt(FileName));
    IsProjectExt := (Ext = '.dpr') or (Ext = '.dpk') or (Ext = '.bpr') or (Ext = '.bpk') or (Ext = '.bdsproj');
    if (Ext = '.pas') or (Ext = '.cpp') or (Ext = '.dfm') or (Ext = '.nfm') or (Ext = '.xfm') or (Ext = '.h') or
       IsProjectExt then
    begin
      if DeleteDdp and not IsProjectExt then
        DeleteFile(ChangeFileExt(FileName, '.ddp'));
      {$IFDEF COMPILER9_UP}
      if RemoveEmptyModel then
      begin
        RemoveDir(ExtractFilePath(FileName) + 'Modell');
        RemoveDir(ExtractFilePath(FileName) + 'Model');
      end;
      {$ENDIF COMPILER9_UP}
      {$IFDEF COMPILER10_UP}
      if RemoveEmptyHistory then
        RemoveDir(ExtractFilePath(FileName) + '__history');
      {$ENDIF COMPILER10_UP}
    end;
  end;
end;

end.
