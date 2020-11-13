{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit ProjectSettings;

{$I ..\DelphiExtension.inc}

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  SysUtils, Classes, Contnrs, ToolsAPI, Menus, ActnList, Forms, Controls,
  ProjectSettingsData, ProjectData, SimpleXmlImport, SimpleXmlIntf, IDENotifiers;

type
  TProjectSettingsNotifier = class(TComponent)
  private
    FGlobalSettings: TProjectSettingList;
    FMenuItemProjectSettings: TMenuItem;
    FActionProjectSettings: TAction;
    FMenuItemManageSettings: TMenuItem;
    FActionManageSettings: TAction;
    FMenuItemSetVersionInfo: TMenuItem;
    FActionSetVersionInfo: TAction;
    FProjectDataNotifier: TProjectDataNotifier;
    FActionList: TActionList;
    FIDENotifier: TIDENotifier;

    procedure InitMenu;
  protected
    procedure DoAssignSettingClick(Sender: TObject);
    procedure ProjectAdded(Data: TProjectData);
    procedure ProjectDestroying(Data: TProjectData);
    procedure SaveSettings(Data: TProjectData; Node: IXmlNode);
    procedure LoadSettings(Data: TProjectData; Node: IXmlNode);
  protected
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: String; var Cancel: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DoUpdateManageSettings(Sender: TObject = nil);
    procedure DoUpdateSetVersionInfo(Sender: TObject = nil);
    procedure DoUpdateProjectSettings(Sender: TObject = nil);
    procedure DoManageSettings(Sender: TObject = nil);
    procedure DoSetVersionInfo(Sender: TObject = nil);
  end;

const
  sProjectSettings = 'ProjectSettings';

procedure InitPlugin(Unload: Boolean);

implementation

uses
  Utils, FrmProjectSettingManageSettings, AppConsts, ProjectResource,
  FrmProjectSettingsSetVersioninfo, DtmImages, ToolsAPIHelpers, Main;

var
  ProjectSettingsNotifier: TProjectSettingsNotifier;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    ProjectSettingsNotifier := TProjectSettingsNotifier.Create(nil)
  else
    FreeAndNil(ProjectSettingsNotifier);
end;

{ ProjectSettingsNotifier }

constructor TProjectSettingsNotifier.Create(AOwner: TComponent);
var
  Filename: string;
begin
  inherited Create(AOwner);
  FGlobalSettings := TProjectSettingList.Create;

  { Add ProjectDataList-Notifier }
  FProjectDataNotifier := TProjectDataNotifier.Create;
  FProjectDataNotifier.Saving := SaveSettings;
  FProjectDataNotifier.Loading := LoadSettings;
  FProjectDataNotifier.Added := ProjectAdded;
  FProjectDataNotifier.Destroying := ProjectDestroying;

  { Add IDE-Notifier }
  FIDENotifier := TIDENotifier.Create;
  FIDENotifier.OnFileNotification := FileNotification;

  FActionList := TActionList.Create(Self);
  FActionList.Images := DataModuleImages.imlIcons;

  { Load global settings }
  Filename := AppDataDirectory + '\GlobalProjectSettings' + DelphiVersion + '.xml';
  if FileExists(Filename) then
  begin
    try
      FGlobalSettings.LoadFromFile(Filename); // refresh
    except
      Application.HandleException(Self);
    end;
  end;

  InitMenu;
end;

destructor TProjectSettingsNotifier.Destroy;
begin
  FIDENotifier.Free;
  FProjectDataNotifier.Free;

  FreeAndNil(FMenuItemProjectSettings);
  FGlobalSettings.Free;
  FinializeCachedSettingData;
  inherited Destroy;
end;

procedure TProjectSettingsNotifier.InitMenu;
var
  ProjectMenu: TMenuItem;
begin
  ProjectMenu := FindMenuItem('ProjectMenu');
  if ProjectMenu <> nil then
  begin
    FMenuItemProjectSettings := TMenuItem.Create(Application.MainForm);
    FMenuItemProjectSettings.Name := 'ProjectSettingsItem';
    FActionProjectSettings := TAction.Create(Self);
    FActionProjectSettings.Caption := sMenuItemProjectSettings;
    FActionProjectSettings.OnUpdate := DoUpdateProjectSettings;
    FActionProjectSettings.OnExecute := DoUpdateProjectSettings;
    FActionProjectSettings.ActionList := FActionList;

    FMenuItemProjectSettings.Action := FActionProjectSettings;
    FMenuItemProjectSettings.SubMenuImages := DataModuleImages.imlIcons;
    ProjectMenu.Add(FMenuItemProjectSettings);

    { "Manage configurations..." }
    FActionManageSettings := TAction.Create(Self);
    FActionManageSettings.ActionList := FActionList;
    FActionManageSettings.Caption := sMenuItemManageProjectSettings;
    FActionManageSettings.OnUpdate := DoUpdateManageSettings;
    FActionManageSettings.OnExecute := DoManageSettings;
    FActionManageSettings.ImageIndex := 6;
    {$IFDEF COMPILER9_UP}
    FActionManageSettings.Visible := False;
    {$ENDIF COMPILER9_UP}

    FMenuItemManageSettings := TMenuItem.Create(Self);
    FMenuItemManageSettings.Name := 'ProjectSettingsManageSettingsItem';
    FMenuItemManageSettings.Action := FActionManageSettings;

    { "Set Versioninfo..." }
    FActionSetVersionInfo := TAction.Create(Self);
    FActionSetVersionInfo.ActionList := FActionList;
    FActionSetVersionInfo.Caption := sMenuItemSetVersionInfo;
    FActionSetVersionInfo.OnUpdate := DoUpdateSetVersionInfo;
    FActionSetVersionInfo.OnExecute := DoSetVersionInfo;
    FActionSetVersionInfo.ImageIndex := 7;

    FMenuItemSetVersionInfo := TMenuItem.Create(Self);
    FMenuItemSetVersionInfo.Name := 'ProjectSetVersionInfoItem';
    FMenuItemSetVersionInfo.Action := FActionSetVersionInfo;

    { add menu items }
    FMenuItemProjectSettings.Add(FMenuItemSetVersionInfo);
    FMenuItemProjectSettings.Add(FMenuItemManageSettings);
    FMenuItemProjectSettings.Add(NewLine);

    DoUpdateProjectSettings;
  end;
end;

procedure TProjectSettingsNotifier.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: String;
  var Cancel: Boolean);
begin
  case NotifyCode of
    {$IFDEF COMPILER6_UP}
    ofnActiveProjectChanged:
      FActionManageSettings.Visible := (GetActiveProject <> nil) and
        {$IFDEF COMPILER9_UP}
        ((GetActiveProject.GetPersonality = sDelphiPersonality) or
        (GetActiveProject.GetPersonality = sDelphiDotNetPersonality));
        {$ELSE}
        True;
        {$ENDIF COMPILER9_UP}
    {$ELSE}
    ofnFileOpened,
    {$ENDIF COMPILER6_UP}
    ofnFileClosing:
      if InArray(ExtractFileExt(FileName), ['.bpg', '.dpr', '.dpk', '.bpr', '.bpk']) then
        FActionManageSettings.Visible := (GetActiveProject <> nil);
  end;
end;

procedure TProjectSettingsNotifier.DoManageSettings(Sender: TObject);
var
  Filename: string;
  Settings: TProjectSettingList;
begin
  Filename := AppDataDirectory + '\GlobalProjectSettings' + DelphiVersion + '.xml';
  if FileExists(Filename) then
  begin
    try
      FGlobalSettings.LoadFromFile(Filename);
    except
      Application.HandleException(Self);
    end;
  end;
  Settings := ProjectDataList[GetActiveProject].NonPersistents[sProjectSettings] as TProjectSettingList;
  if TFormManageProjectSetting.Execute(Settings, FGlobalSettings) then
  begin
    FGlobalSettings.SaveToFile(Filename);
    {$IFDEF COMPILER6_UP}
    GetActiveProject.MarkModified;
    {$ELSE}
    GetActiveProject.ProjectOptions.ModifiedState := True;
    {$ENDIF COMPILER6_UP}
  end;
end;

procedure TProjectSettingsNotifier.DoSetVersionInfo(Sender: TObject);
var
  i: Integer;
  List: TInterfaceList;
  Group: IOTAProjectGroup;
begin
  Group := GetActiveProjectGroup;
  if Assigned(Group) then
  begin
    List := TInterfaceList.Create;
    try
      for i := 0 to Group.ProjectCount - 1 do
        List.Add(Group.Projects[i]);
      TFormProjectSettingsSetVersioninfo.Execute(List);
    finally
      List.Free;
    end;
  end;
end;

procedure TProjectSettingsNotifier.DoUpdateProjectSettings(Sender: TObject);
var
  Item: TMenuItem;
  i: Integer;
  Settings: TProjectSettingList;
  Data: TProjectData;
  V: Variant;
  Options: TProjectSetting;
begin
  for i := 2 to FMenuItemProjectSettings.Count - 1 do
    FMenuItemProjectSettings.Delete(FMenuItemProjectSettings.Count - 1);

  {$IFDEF COMPILER9_UP}
  if not ((GetActiveProject <> nil) and
     ((GetActiveProject.GetPersonality = sDelphiPersonality) or
      (GetActiveProject.GetPersonality = sDelphiDotNetPersonality))) then
    Exit;
  {$ENDIF COMPILER9_UP}

  Settings := nil;
  Data := nil;
  Options := nil;
  try
    if GetActiveProject <> nil then
    begin
      Data := ProjectDataList[GetActiveProject];
      Settings := Data.NonPersistents[sProjectSettings] as TProjectSettingList;
      Options := TProjectSetting.Create;
      Options.CopyFrom(GetActiveProject);
    end;

    if Assigned(Settings) and Assigned(Data) then
    begin
      Settings.Sort;
      for i := 0 to Settings.Count - 1 do
      begin
        Item := TMenuItem.Create(FMenuItemProjectSettings);
        Item.Caption := Settings[i].Name;
        Item.Tag := Integer(Settings[i]);
        Item.RadioItem := True;
        Item.GroupIndex := 1;

        V := Data.Values[sProjectSettings];
        Item.Checked := (V <> Null) and (V = 'Local:' + Settings[i].Name) and Settings[i].Compare(Options);
        Item.OnClick := DoAssignSettingClick;
        FMenuItemProjectSettings.Add(Item);
      end;
    end;

    if Assigned(Data) then
    begin
      if FMenuItemProjectSettings.Items[FMenuItemProjectSettings.Count - 1].Caption <> '-' then
        FMenuItemProjectSettings.Add(NewLine);
      FGlobalSettings.Sort;
      for i := 0 to FGlobalSettings.Count - 1 do
      begin
        Item := TMenuItem.Create(FMenuItemProjectSettings);
        Item.Caption := FGlobalSettings[i].Name;
        Item.Tag := Integer(FGlobalSettings[i]);
        Item.RadioItem := True;
        Item.GroupIndex := 1;

        V := Data.Values[sProjectSettings];
        Item.Checked := (V <> Null) and (V = 'Global:' + FGlobalSettings[i].Name) and FGlobalSettings[i].Compare(Options);
        Item.OnClick := DoAssignSettingClick;
        FMenuItemProjectSettings.Add(Item);
      end;
    end;
  finally
    Options.Free;
  end;
end;

procedure TProjectSettingsNotifier.DoAssignSettingClick(Sender: TObject);
var
  Options: TProjectSetting;
  Project: IOTAProject;
begin
  Project := GetActiveProject;
  Options := TProjectSetting(TMenuItem(Sender).Tag);
  if (Project <> nil) and (Options <> nil) then
  begin
    Options.CopyTo(Project, scAssign);
    if FGlobalSettings.IndexOf(Options) <> -1 then
      ProjectDataList[Project].Values[sProjectSettings] := 'Global:' + Options.Name
    else
      ProjectDataList[Project].Values[sProjectSettings] := 'Local:' + Options.Name;
  end;
end;

procedure TProjectSettingsNotifier.DoUpdateManageSettings(Sender: TObject);
begin
  FActionManageSettings.Enabled := GetActiveProject <> nil;
end;

procedure TProjectSettingsNotifier.DoUpdateSetVersionInfo(Sender: TObject);
var
  Group: IOTAProjectGroup;
begin
  Group := GetActiveProjectGroup;
  FActionSetVersionInfo.Enabled := (Group <> nil) and (Group.ProjectCount > 0)
    {$IFDEF COMPILER9_UP}
    and ((GetActiveProject.GetPersonality = sDelphiPersonality) or
         (GetActiveProject.GetPersonality = sCBuilderPersonality))
    {$ENDIF COMPILER9_UP}
  ;
end;

procedure TProjectSettingsNotifier.LoadSettings(Data: TProjectData; Node: IXmlNode);
var
  Xml: IXmlNode;
begin
  Xml := Node.ChildNodes.FindNode(sProjectSettings);
  if Xml <> nil then
    TProjectSettingList(Data.NonPersistents[sProjectSettings]).LoadFromXml(Xml);
end;

procedure TProjectSettingsNotifier.SaveSettings(Data: TProjectData; Node: IXmlNode);
begin
  if TProjectSettingList(Data.NonPersistents[sProjectSettings]).Count > 0 then
    TProjectSettingList(Data.NonPersistents[sProjectSettings]).SaveToXml( Node.AddChild(sProjectSettings) );
end;

procedure TProjectSettingsNotifier.ProjectAdded(Data: TProjectData);
begin
  Data.NonPersistents[sProjectSettings] := TProjectSettingList.Create;
end;

procedure TProjectSettingsNotifier.ProjectDestroying(Data: TProjectData);
begin
  Data.NonPersistents[sProjectSettings].Free;
end;

end.
