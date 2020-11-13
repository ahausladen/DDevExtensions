{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2008 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageStartParameterTeam;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ToolsAPI, FrmTreePages, FrmOptions, PluginConfig, StdCtrls,
  ModuleData, IDENotifiers, FrmeBase, ExtCtrls, IDEHooks, Hooking;

type
  TStartParameterTeam = class(TPluginConfig)
  private
    FModuleNotifier: TModuleDataNotifier;
    FIDENotifier: TIDENotifier;
    FActive: Boolean;
  protected
    function GetOptionPages: TTreePage; override;
    procedure Init; override;
    procedure DoModuleBeforeSave(Data: TModuleData);
    procedure DoModuleAfterSave(Data: TModuleData);
    procedure DoModuleRenamed(Data: TModuleData; const NewName: string);
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string;
      var Cancel: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveRunParams(Project: IOTAProject);
  published
    property Active: Boolean read FActive write FActive;
  end;

  TFrameOptionPageStartParameterTeam = class(TFrameBase, ITreePageComponent)
    cbxActive: TCheckBox;
  private
    { Private-Deklarationen }
    FStartParameterTeam: TStartParameterTeam;
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
  {$IF CompilerVersion >= 23.0} // XE2+
  CommonOptionStrs,
  {$IFEND}
  Variants, Main, ProjectData, IDEUtils;

{$R *.dfm}

var
  StartParameterTeam: TStartParameterTeam;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    StartParameterTeam := TStartParameterTeam.Create
  else
    FreeAndNil(StartParameterTeam);
end;

{ TFrameOptionPageStartParameterTeam }

procedure TFrameOptionPageStartParameterTeam.SetUserData(UserData: TObject);
begin
  FStartParameterTeam := UserData as TStartParameterTeam;
end;

procedure TFrameOptionPageStartParameterTeam.LoadData;
begin
  cbxActive.Checked := FStartParameterTeam.Active;
end;

procedure TFrameOptionPageStartParameterTeam.SaveData;
begin
  FStartParameterTeam.Active := cbxActive.Checked;
  FStartParameterTeam.Save;
end;

procedure TFrameOptionPageStartParameterTeam.Selected;
begin
end;

procedure TFrameOptionPageStartParameterTeam.Unselected;
begin
end;

{ TStartParameterTeam }

constructor TStartParameterTeam.Create;
begin
  inherited Create(AppDataDirectory + '\StartParameterTeam.xml', 'StartParameterTeam');

  FModuleNotifier := TModuleDataNotifier.Create;
  FModuleNotifier.BeforeSave := DoModuleBeforeSave;
  FModuleNotifier.AfterSave := DoModuleAfterSave;

  FIDENotifier := TIDENotifier.Create;
  FIDENotifier.OnFileNotification := FileNotification;
end;

destructor TStartParameterTeam.Destroy;
begin
  FIDENotifier.Free;
  FModuleNotifier.Free;
  Active := False;
  inherited Destroy;
end;

procedure TStartParameterTeam.Init;
begin
  inherited Init;
  Active := False;
end;

procedure TStartParameterTeam.RemoveRunParams(Project: IOTAProject);
var
  Ext, S: string;
  StartPos, EndPos, Len: Integer;
  BOM: TBytes;
  Encoding: TEncoding;
  Stream: TFileStream;
  Filename: string;
  Lines: TStrings;
  I: Integer;
  Modified: Boolean;
  LastWriteTime: TFileTime;
begin
  { Remove RunParams from project file }

  {
     Delphi 2005-2009:
       bdsproj, dproj: <Parameters Name="RunParams">text</Parameters>
  }

  Encoding := nil;
  Modified := False;
  Lines := TStringList.Create;
  Stream := nil;
  try
    Filename := Project.FileName;
    Ext := LowerCase(ExtractFileExt(Filename));
    if ((Ext = '.dproj') or (Ext = '.cbproj')) and FileExists(Filename) then
    begin
      Stream := TFileStream.Create(Filename, fmOpenReadWrite or fmShareDenyRead);
      SetLength(BOM, 4);
      Stream.Read(BOM[0], 4);
      TEncoding.GetBufferEncoding(BOM, Encoding);
      Stream.Position := 0;
      Lines.LoadFromStream(Stream);
      for I := 0 to Lines.Count - 1 do
      begin
        S := Lines[I];
        StartPos := Pos('<Parameters Name="RunParams">', S);
        if StartPos > 0 then
        begin
          Inc(StartPos, Length('<Parameters Name="RunParams">'));
          Len := Length(S);
          EndPos := StartPos;
          while EndPos < Len do
          begin
            if (S[EndPos] = '<') and (S[EndPos + 1] = '/') and (StrLComp('</Parameters>', PChar(S) + EndPos - 1, 13) = 0) then
              Break;
            Inc(EndPos);
          end;
          if StartPos <> EndPos then
          begin
            S := Copy(S, 1, StartPos - 1) + Copy(S, EndPos, MaxInt);
            Lines[I] := S;
            Modified := True;
          end;
          Break;
        end;

        // XE2+
        StartPos := Pos('<Debugger_RunParams>', S);
        if StartPos <> 0 then
        begin
          Inc(StartPos, Length('<Debugger_RunParams>'));
          Len := Length(S);
          EndPos := StartPos;
          while EndPos < Len do
          begin
            if (S[EndPos] = '<') and (S[EndPos + 1] = '/') and (StrLComp('</Debugger_RunParams>', PChar(S) + EndPos - 1, 13) = 0) then
              Break;
            Inc(EndPos);
          end;
          if StartPos <> EndPos then
          begin
            S := Copy(S, 1, StartPos - 1) + Copy(S, EndPos, MaxInt);
            Lines[I] := S;
            Modified := True;
          end;
        end;
      end;
    end;

    if Modified then
    begin
      GetFileTime(Stream.Handle, nil, nil, @LastWriteTime);
      Stream.Position := 0;
      Lines.SaveToStream(Stream, Encoding);
      Stream.Size := Stream.Position;
      SetFileTime(Stream.Handle, nil, nil, @LastWriteTime);
    end;
  finally
    Lines.Free;
    Stream.Free;
  end;
end;

function TStartParameterTeam.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Local Start Parameters', TFrameOptionPageStartParameterTeam, Self);
end;

procedure TStartParameterTeam.DoModuleBeforeSave(Data: TModuleData);
var
  Project: IOTAProject;
  {$IF CompilerVersion >= 23.0} // XE2+
  OptionConfig: IOTAProjectOptionsConfigurations;
  BuildConfig: IOTABuildConfiguration;
  {$IFEND}
begin
  if Active then
  begin
    if Supports(Data.Module, IOTAProject, Project) and
       not Supports(Data.Module, IOTAProjectGroup) then
    begin
      {$IF CompilerVersion >= 23.0} // XE2+
      // XE2 Update 4 requires this
      if Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, OptionConfig) then
      begin
        BuildConfig := OptionConfig.ActiveConfiguration;
        if (BuildConfig = nil) and (OptionConfig.ConfigurationCount > 0) then
          BuildConfig := OptionConfig.Configurations[0];

        if BuildConfig <> nil then
          ProjectDataList[Project].Values['RunParams'] := BuildConfig.GetValue(CommonOptionStrs.sDebugger_RunParams);
      end
      else
      {$IFEND}
        ProjectDataList[Project].Values['RunParams'] := Project.ProjectOptions.Values['RunParams'];
    end;
  end;
end;

procedure TStartParameterTeam.DoModuleAfterSave(Data: TModuleData);
var
  Project: IOTAProject;
begin
  if Active then
  begin
    if Supports(Data.Module, IOTAProject, Project) and
       not Supports(Data.Module, IOTAProjectGroup) then
    begin
      RemoveRunParams(Project);
    end;
  end;
end;

procedure TStartParameterTeam.DoModuleRenamed(Data: TModuleData; const NewName: string);
{var
  Project: IOTAProject;}
begin
{  if Active then
  begin
    if Supports(Data.Module, IOTAProject, Project) and
       not Supports(Data.Module, IOTAProjectGroup) then
    begin
      if FileExists(Project.FileName + '.localoptions') then
        RenameFile(Project.FileName + '.localoptions', NewName + '.localoptions');
    end;
  end;}
end;

procedure TStartParameterTeam.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
  Project: IOTAProject;
  {$IF CompilerVersion >= 23.0} // XE2+
  OptionConfig: IOTAProjectOptionsConfigurations;
  BuildConfig: IOTABuildConfiguration;
  {$IFEND}
  WasModified: Boolean;
  Ext: string;
begin
  if (NotifyCode = ofnFileOpened) and Active then
  begin
    Ext := AnsiLowerCase(ExtractFileExt(FileName));
    if (Ext = '.dpr') or (Ext = '.dpk') or (Ext = '.bpr') or
       (Ext = '.bdsproj') or
       (Ext = '.dproj') or (Ext = '.cbproj') then
    begin
      if Supports((BorlandIDEServices as IOTAModuleServices).FindModule(FileName), IOTAProject, Project) then
      begin
        if ProjectDataList[Project].HasValue('RunParams') then
        begin
          {$IF CompilerVersion >= 23.0} // XE2+
          // XE2 Update 4 requires this
          if Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, OptionConfig) then
          begin
            BuildConfig := OptionConfig.ActiveConfiguration;
            if (BuildConfig = nil) and (OptionConfig.ConfigurationCount > 0) then
              BuildConfig := OptionConfig.Configurations[0];

            if BuildConfig <> nil then
            begin
              if BuildConfig.GetValue(CommonOptionStrs.sDebugger_RunParams) <> ProjectDataList[Project].Values['RunParams'] then
              begin
                WasModified := Project.ProjectOptions.ModifiedState;
                BuildConfig.SetValue(CommonOptionStrs.sDebugger_RunParams, ProjectDataList[Project].Values['RunParams']);
                Project.ProjectOptions.ModifiedState := WasModified;
              end;
            end;
          end
          else
          {$IFEND}
          begin
            if VarToStr(Project.ProjectOptions.Values['RunParams']) <> VarToStr(ProjectDataList[Project].Values['RunParams']) then
            begin
              WasModified := Project.ProjectOptions.ModifiedState;
              Project.ProjectOptions.Values['RunParams'] := ProjectDataList[Project].Values['RunParams'];
              Project.ProjectOptions.ModifiedState := WasModified;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.

