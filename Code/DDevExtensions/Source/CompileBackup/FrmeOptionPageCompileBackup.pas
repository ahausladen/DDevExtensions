{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageCompileBackup;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FrmTreePages, PluginConfig, IDENotifiers, ModuleData,
  ToolsAPI, ExtCtrls, FrmeBase;

type
  TCompileBackupConfig = class(TPluginConfig)
  private
    FIDENofifier: TIDENotifier;
    FModuleDataNotifier: TModuleDataNotifier;
    FActive: Boolean;
    FDeleteBackupAfterClose: Boolean;
  protected
    procedure Init; override;

    procedure DeleteBackupFiles(Data: TModuleData);
    procedure BackupModifiedFiles;

    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean);
    procedure ModuleAfterSave(Data: TModuleData);
    procedure ModuleDestroying(Data: TModuleData);

    function GetOptionPages: TTreePage; override;
    function GetCompileBackupFilename(const Filename: string): string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write FActive;
    property DeleteBackupAfterClose: Boolean read FDeleteBackupAfterClose write FDeleteBackupAfterClose;
  end;

  TFrameOptionPageCompileBackup = class(TFrameBase, ITreePageComponent)
    cbxActive: TCheckBox;
    cbxDeleteBackupAfterClose: TCheckBox;
    procedure cbxActiveClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FCompileBackupConfig: TCompileBackupConfig;
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
  Main, ToolsAPIHelpers, IDEUtils;

{$R *.dfm}

var
  CompileBackupConfig: TCompileBackupConfig;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    CompileBackupConfig := TCompileBackupConfig.Create
  else
    FreeAndNil(CompileBackupConfig);
end;

{ TFrameOptionPageCompileBackup }

procedure TFrameOptionPageCompileBackup.SetUserData(UserData: TObject);
begin
  FCompileBackupConfig := UserData as TCompileBackupConfig;
end;

procedure TFrameOptionPageCompileBackup.LoadData;
begin
  cbxActive.Checked := FCompileBackupConfig.Active;
  cbxDeleteBackupAfterClose.Checked := FCompileBackupConfig.DeleteBackupAfterClose;
  cbxActiveClick(cbxActive);
end;

procedure TFrameOptionPageCompileBackup.SaveData;
begin
  FCompileBackupConfig.Active := cbxActive.Checked;
  FCompileBackupConfig.DeleteBackupAfterClose := cbxDeleteBackupAfterClose.Checked;
  FCompileBackupConfig.Save;
end;

procedure TFrameOptionPageCompileBackup.Selected;
begin
end;

procedure TFrameOptionPageCompileBackup.Unselected;
begin
end;

procedure TFrameOptionPageCompileBackup.cbxActiveClick(Sender: TObject);
begin
  cbxDeleteBackupAfterClose.Enabled := cbxActive.Checked;
end;

{ TCompileBackupConfig }

constructor TCompileBackupConfig.Create;
begin
  inherited Create(AppDataDirectory + '\CompileBackup.xml', 'CompileBackup');
  FIDENofifier := TIDENotifier.Create;
  FIDENofifier.OnBeforeCompile := BeforeCompile;
  FModuleDataNotifier := TModuleDataNotifier.Create;
  FModuleDataNotifier.AfterSave := ModuleAfterSave;
  FModuleDataNotifier.Destroying := ModuleDestroying;
end;

destructor TCompileBackupConfig.Destroy;
begin
  FModuleDataNotifier.Free;
  FIDENofifier.Free;
  inherited Destroy;
end;

procedure TCompileBackupConfig.Init;
begin
  inherited Init;
  Active := True;
  DeleteBackupAfterClose := True;
end;

procedure TCompileBackupConfig.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  if not IsCodeInsight and Active then
    BackupModifiedFiles;
end;

procedure TCompileBackupConfig.ModuleDestroying(Data: TModuleData);
begin
  if Active and DeleteBackupAfterClose then
    DeleteBackupFiles(Data);
  { Destroy the associated BackupFiles-TStringList }
  Data.Bucket[Self].Free;
end;

procedure TCompileBackupConfig.ModuleAfterSave(Data: TModuleData);
begin
  if Active then
    DeleteBackupFiles(Data);
end;

procedure TCompileBackupConfig.DeleteBackupFiles(Data: TModuleData);
var
  List: TStrings;
  i: Integer;
begin
  List := TStrings(Data.Bucket[Self]);
  if List <> nil then
  begin
    for i := 0 to List.Count - 1 do
      DeleteFile(List[i]);
    List.Free;
    Data.Bucket[Self] := nil;
  end;
end;

procedure TCompileBackupConfig.BackupModifiedFiles;
const
  Utf8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
var
  Modules: IOTAModuleServices;
  Module: IOTAModule;
  FileEditor: IOTAEditor;
  SourceEditor: IOTAEditBuffer;
  FormEditor: IOTAFormEditor;
  i, k: Integer;
  BackupFilename: string;
  BackupedFiles: TStrings;
begin
  if Supports(BorlandIDEServices, IOTAModuleServices, Modules) then
  begin
    BackupedFiles := TStringList.Create;
    try
      for i := 0 to Modules.ModuleCount - 1 do
      begin
        Module := Modules.Modules[i];
        try
          for k := 0 to Module.GetModuleFileCount - 1 do
          begin
            FileEditor := Module.GetModuleFileEditor(k);
            if Supports(FileEditor, IOTAEditBuffer, SourceEditor) then
            begin
              if SourceEditor.Modified and FastFileExists(SourceEditor.FileName) then
              begin
                BackupFilename := GetCompileBackupFilename(SourceEditor.FileName);
                SaveEditorSourceTo(SourceEditor, BackupFilename);
                BackupedFiles.Add(BackupFilename);
              end;
            end
            else
            if Supports(FileEditor, IOTAFormEditor, FormEditor) then
            begin
              if FormEditor.Modified and FastFileExists(FormEditor.FileName) then
              begin
                BackupFilename := GetCompileBackupFilename(FormEditor.FileName);
                SaveFormResourceTo(FormEditor, BackupFilename);
                BackupedFiles.Add(BackupFilename);
              end;
            end;
          end;

          if BackupedFiles.Count > 0 then
          begin
            ModuleDataList[Module].Bucket[Self].Free;
            ModuleDataList[Module].Bucket[Self] := BackupedFiles;
            BackupedFiles := TStringList.Create;
          end;
        except
          // ignore Delphi 5 exceptions
        end;
      end;
    finally
      BackupedFiles.Free;
    end;
  end;

end;

function TCompileBackupConfig.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Compile Backup', TFrameOptionPageCompileBackup, Self);
end;

function TCompileBackupConfig.GetCompileBackupFilename(const Filename: string): string;
begin
  Result := Filename + '.cbk';
end;

end.
