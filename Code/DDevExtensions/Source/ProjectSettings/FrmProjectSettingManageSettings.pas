{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmProjectSettingManageSettings;

{$I ..\DelphiExtension.inc}

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolWin, ExtCtrls, ComCtrls, ImgList, ActnList,
  ProjectSettingsData, ToolsAPI, Menus, FrmBase;

type
  TFormManageProjectSetting = class(TFormBase)
    btnOk: TButton;
    btnCancel: TButton;
    pnlClient: TPanel;
    spltProjects: TSplitter;
    aclButtons: TActionList;
    actLocalNewSetting: TAction;
    actLocalEditSetting: TAction;
    actLocalDeleteSetting: TAction;
    actLocalAssignSetting: TAction;
    actGlobalNewSetting: TAction;
    actGlobalEditSetting: TAction;
    actGlobalDeleteSetting: TAction;
    actGlobalAssignSetting: TAction;
    pnlLeft: TPanel;
    pnlGlobal: TPanel;
    pnlLocal: TPanel;
    lvwLocal: TListView;
    lvwGlobal: TListView;
    spltLocalGlobal: TSplitter;
    lblLocalSettings: TLabel;
    lblGlobalSettings: TLabel;
    tbToolbarLocal: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    tbToolbarGlobal: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    pnlProjects: TPanel;
    lvwProjects: TListView;
    lblProjects: TLabel;
    ToolButton13: TToolButton;
    actLocalEditOptions: TAction;
    actGlobalEditOptions: TAction;
    ToolButton14: TToolButton;
    popLocalSettings: TPopupMenu;
    New1: TMenuItem;
    Edit1: TMenuItem;
    Setactiveflags1: TMenuItem;
    Delete1: TMenuItem;
    Assign1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    popGlobalSettings: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    tbToolbarProjects: TToolBar;
    ToolButton16: TToolButton;
    ToolButton18: TToolButton;
    actProjectEditProjectOptions: TAction;
    ToolButton15: TToolButton;
    actProjectSetVersionInfo: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure ActionExecute(Sender: TObject);
    procedure lvwProjectsCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvwProjectsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvwLocalDblClick(Sender: TObject);
    procedure lvwGlobalEdited(Sender: TObject; Item: TListItem; var S: String);
    procedure lvwGlobalKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvwGlobalEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure lvwGlobalDblClick(Sender: TObject);
    procedure ActionProjectUpdate(Sender: TObject);
    procedure ActionProjectExecute(Sender: TObject);
  private
    { Private-Deklarationen }
    FSettings: TProjectSettingList;
    FGlobalSettings: TProjectSettingList;
    FProjectAssignments: TStrings;
  protected
    procedure FillSettingsListViews(ListView: TListView; Settings: TProjectSettingList);
    procedure FillProjectListView;
    function DoExecute(ASettings, AGlobalSettings: TProjectSettingList): Boolean;
    function AddSettingListItem(ListView: TListView; Setting: TProjectSetting): TListItem;
    procedure RefreshProjectAssignment;
    procedure SetProjectAssignment(Project: IOTAProject; const Value: string);
    function GetProjectAssignment(Project: IOTAProject): string;
    procedure UpdateActions; override;
  public
    { Public-Deklarationen }
    class function Execute(ASettings, AGlobalSettings: TProjectSettingList): Boolean;
  end;

var
  FormManageProjectSetting: TFormManageProjectSetting;

implementation

uses
  Consts, Utils, ToolsAPIHelpers, FrmProjectSettingsEditOptions, ProjectData,
  ProjectSettings, FrmProjectSettingsSetVersioninfo, DtmImages, AppConsts;

{$R *.dfm}

{$IFDEF COMPILER5}
type
  TExListView = class(TListView)
    procedure SelectAll;
  end;

procedure TExListView.SelectAll;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := True;
end;
{$ENDIF COMPILER5}

class function TFormManageProjectSetting.Execute(ASettings, AGlobalSettings: TProjectSettingList): Boolean;
begin
  if GetActiveProject <> nil then
  begin
    with Self.Create(nil) do
    begin
      try
        Result := DoExecute(ASettings, AGlobalSettings);
      finally
        Free;
      end;
    end;
  end
  else
    Result := False;
end;

function TFormManageProjectSetting.DoExecute(ASettings, AGlobalSettings: TProjectSettingList): Boolean;
var
  i, Index: Integer;
begin
  // Populate ListView
  FSettings.Assign(ASettings);
  FGlobalSettings.Assign(AGlobalSettings);
  try
    lvwLocal.Tag := Integer(FSettings);
    lvwGlobal.Tag := Integer(FGlobalSettings);
    if (FSettings.Count = 0) and (GetProjectAssignment(GetActiveProject) = '') then
    begin
      // Create "Default" setting
      with FSettings.Add do
      begin
        Name := 'Default';
        CopyFrom(GetActiveProject);
        SetProjectAssignment(GetActiveProject, 'Local:Default');
      end;
    end;

    FillSettingsListViews(lvwLocal, FSettings);
    FillSettingsListViews(lvwGlobal, FGlobalSettings);
    FillProjectListView;
    lblLocalSettings.Caption := Format(_('%s from %s'), [lblLocalSettings.Caption, ExtractFileName(GetActiveProject.FileName)]);

    Result := ShowModal = mrOk;
    if Result then
    begin
      for i := 0 to lvwProjects.Items.Count - 1 do
      begin
        Index := FProjectAssignments.IndexOfObject(lvwProjects.Items[i].Data);
        if Index <> -1 then
          ProjectDataList[IOTAProject(lvwProjects.Items[i].Data)].Values[sProjectSettings] := FProjectAssignments[Index];
      end;
      // Update data
      ASettings.Assign(FSettings);
      AGlobalSettings.Assign(FGlobalSettings);
    end;
  finally
    FGlobalSettings.Clear;
    FSettings.Clear;
  end;
end;

procedure TFormManageProjectSetting.FormCreate(Sender: TObject);
begin
  FSettings := TProjectSettingList.Create;
  FGlobalSettings := TProjectSettingList.Create;
  FProjectAssignments := TStringList.Create;

  // Set anchors by code, otherwise Delphi 5 makes trouble
  pnlClient.Anchors := [akLeft, akTop, akRight, akBottom];
  btnOk.Caption := SOKButton;
  btnOk.Anchors := [akRight, akBottom];
  btnCancel.Caption := SCancelButton;
  btnCancel.Anchors := [akRight, akBottom];
  Constraints.MinWidth := 240;
  Constraints.MinHeight := 100;
  {$IFDEF COMPILER10_UP}
  tbToolbarLocal.DrawingStyle := ComCtrls.dsGradient;
  tbToolbarGlobal.DrawingStyle := ComCtrls.dsGradient;
  tbToolbarProjects.DrawingStyle := ComCtrls.dsGradient;
  {$ENDIF COMPILER10_UP}

  tbToolbarLocal.Images := DataModuleImages.imlIcons;
  tbToolbarGlobal.Images := DataModuleImages.imlIcons;
  tbToolbarProjects.Images := DataModuleImages.imlIcons;

  pnlLocal.DoubleBuffered := True;
  pnlGlobal.DoubleBuffered := True;
  pnlProjects.DoubleBuffered := True;
end;

procedure TFormManageProjectSetting.FormDestroy(Sender: TObject);
begin
  FProjectAssignments.Free;
  FGlobalSettings.Free;
  FSettings.Free;
end;

function TFormManageProjectSetting.AddSettingListItem(ListView: TListView; Setting: TProjectSetting): TListItem;
begin
  Result := ListView.Items.Add;
  Result.Caption := Setting.Name;
  Result.Data := Setting;
  Result.ImageIndex := 6;
end;

procedure TFormManageProjectSetting.FillSettingsListViews(ListView: TListView; Settings: TProjectSettingList);
var
  i: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    for i := 0 to Settings.Count - 1 do
      AddSettingListItem(ListView, Settings[i]);
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure TFormManageProjectSetting.FillProjectListView;
var
  Item: TListItem;
  i: Integer;
  ProjectGroup: IOTAProjectGroup;
  Options: TProjectSetting;
begin
  lvwProjects.Items.BeginUpdate;
  try
    lvwProjects.Items.Clear;

    Options := TProjectSetting.Create;
    try
      ProjectGroup := GetActiveProjectGroup;
      if ProjectGroup <> nil then
      begin
        for i := 0 to ProjectGroup.ProjectCount - 1 do
        begin
          Options.CopyFrom(ProjectGroup.Projects[i]);
          Item := lvwProjects.Items.Add;
          Item.Caption := ExtractFileName(ProjectGroup.Projects[i].FileName);
          Item.Data := Pointer(ProjectGroup.Projects[i]);
          Item.SubItems.AddObject('', nil);
          Item.Selected := ProjectGroup.Projects[i] = ProjectGroup.ActiveProject;
          Item.ImageIndex := 4;
        end;
      end;
    finally
      Options.Free;
    end;
    RefreshProjectAssignment;
  finally
    lvwProjects.Items.EndUpdate;
  end;
end;

procedure TFormManageProjectSetting.ActionUpdate(Sender: TObject);

  function HaveSelectedProjectsTheSetting(ListView: TListView): Boolean;
  var
    i: Integer;
    Options: TProjectSetting;
  begin
    Result := False;
    Options := ListView.Selected.Data;
    for i := 0 to lvwProjects.Items.Count - 1 do
      if lvwProjects.Items[i].Selected then
        if (lvwProjects.Items[i].SubItems.Objects[0] <> Options) or
           EndsText('*', lvwProjects.Items[i].SubItems[0]) then
          Exit;
    Result := True;
  end;

begin
  if Sender = actLocalEditSetting then
    actLocalEditSetting.Enabled := lvwLocal.Selected <> nil
  else if Sender = actLocalDeleteSetting then
    actLocalDeleteSetting.Enabled := (lvwLocal.Selected <> nil) and
      (TProjectSetting(lvwLocal.Selected.Data).Name <> 'Default')
  else if Sender = actLocalAssignSetting then
    actLocalAssignSetting.Enabled := (lvwLocal.Selected <> nil) and (lvwProjects.SelCount > 0) and
      not HaveSelectedProjectsTheSetting(lvwLocal)
  else if Sender = actLocalEditOptions then
    actLocalEditOptions.Enabled := lvwLocal.Selected <> nil

  else if Sender = actGlobalEditSetting then
    actGlobalEditSetting.Enabled := lvwGlobal.Selected <> nil
  else if Sender = actGlobalDeleteSetting then
    actGlobalDeleteSetting.Enabled := lvwGlobal.Selected <> nil
  else if Sender = actGlobalAssignSetting then
    actGlobalAssignSetting.Enabled := (lvwGlobal.Selected <> nil) and (lvwProjects.SelCount > 0) and
      not HaveSelectedProjectsTheSetting(lvwGlobal)
  else if Sender = actGlobalEditOptions then
    actGlobalEditOptions.Enabled := lvwGlobal.Selected <> nil
  ;
end;

procedure TFormManageProjectSetting.ActionExecute(Sender: TObject);
var
  List: TProjectSettingList;
  ListView: TListView;
  Backup: TProjectSetting;
  Options, OrgOptions: TProjectSetting;
  i: Integer;
  Asked: Boolean;
  Prefix: string;
begin
  if Pos('Local', TAction(Sender).Name) > 0 then
  begin
    List := FSettings;
    ListView := lvwLocal;
    Prefix := 'Local:';
  end
  else
  begin
    List := FGlobalSettings;
    ListView := lvwGlobal;
    Prefix := 'Global:';
  end;

  if (Sender = actLocalNewSetting) or (Sender = actGlobalNewSetting) then
  begin
    Options := List.Add;
    i := 1;
    while List.FindByName('Project Options ' + IntToStr(i)) <> nil do
      Inc(i);
    Options.Name := 'Project Options ' + IntToStr(i);
    with AddSettingListItem(ListView, Options) do
    begin
      Selected := True;

      Backup := TProjectSetting.Create;
      try
        Backup.CopyFrom(GetActiveProject, scCopyAll);
        GetActiveProject.ProjectOptions.EditOptions; // edit dialog
        Options.CopyFrom(GetActiveProject);
      finally
        Backup.CopyTo(GetActiveProject, scCopyAll);
        Backup.Free;
      end;

      ListView.Selected.EditCaption;
    end;
  end
  {----------------------}
  else if (Sender = actLocalEditSetting) or (Sender = actGlobalEditSetting) then
  begin
    Options := TProjectSetting(ListView.Selected.Data);
    OrgOptions := TProjectSetting.Create;
    try
      OrgOptions.Assign(Options);
      Backup := TProjectSetting.Create;
      try
        Backup.CopyFrom(GetActiveProject, scCopyAll);
        Options.CopyTo(GetActiveProject);
        GetActiveProject.ProjectOptions.EditOptions; // edit dialog
        Options.CopyFrom(GetActiveProject);
      finally
        Backup.CopyTo(GetActiveProject, scCopyAll);
        Backup.Free;
      end;

      { Copy the changes to the projects that use the Settings. But first ask
        the user. } 
      if not Options.Compare(OrgOptions) then
      begin
        Asked := False;
        for i := 0 to lvwProjects.Items.Count - 1 do
        begin
          if lvwProjects.Items[i].SubItems.Objects[0] = Options then
          begin
            if not Asked then
            begin
              if MessageDlg(_('Do you want to assign the changes to the projects?'), mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
              begin
                RefreshProjectAssignment;
                Break;
              end;
              Asked := True;
            end;
            Options.CopyTo(IOTAProject(lvwProjects.Items[i].Data));
          end;
        end;
      end;
    finally
      OrgOptions.Free;
    end;
  end
  {----------------------}
  else if (Sender = actLocalDeleteSetting) or (Sender = actGlobalDeleteSetting) then
  begin
    Options := TProjectSetting(ListView.Selected.Data);
    if MessageDlg(Format(_('Do you really want to delete the project settings %s'), [Options.Name]),
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
    List.Remove(Options);
    ListView.Selected.Delete;
    RefreshProjectAssignment;
  end
  {----------------------}
  else if (Sender = actLocalAssignSetting) or (Sender = actGlobalAssignSetting) then
  begin
    lvwProjects.Items.BeginUpdate;
    try
      Options := ListView.Selected.Data;
      for i := 0 to lvwProjects.Items.Count - 1 do
      begin
        if lvwProjects.Items[i].Selected then
        begin
          Options.CopyTo(IOTAProject(lvwProjects.Items[i].Data), scAssign);
          SetProjectAssignment(IOTAProject(lvwProjects.Items[i].Data), Prefix + Options.Name);

          lvwProjects.Items[i].SubItems.Objects[0] := Options;
          lvwProjects.Items[i].SubItems[0] := Options.Name;
        end;
      end;
    finally
      lvwProjects.Items.EndUpdate;
    end;
  end
  {----------------------}
  else if (Sender = actLocalEditOptions) or (Sender = actGlobalEditOptions) then
  begin
    TFormProjectSettingsEditOptions.Execute(ListView.Selected.Data);
  end;
end;

procedure TFormManageProjectSetting.lvwProjectsCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
  if Item.Data = Pointer(GetActiveProject) then
    Sender.Canvas.Font.Style := [fsBold]
  else
    Sender.Canvas.Font.Style := [];
end;

procedure TFormManageProjectSetting.lvwProjectsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = Ord('A')) and (Shift = [ssCtrl]) then
    {$IFDEF COMPILER6_UP}
    (Sender as TListView).SelectAll;
    {$ELSE}
    TExListView(Sender).SelectAll;
    {$ENDIF COMPILER6_UP}
end;

procedure TFormManageProjectSetting.lvwLocalDblClick(Sender: TObject);
begin
  actLocalEditSetting.Execute;
end;

procedure TFormManageProjectSetting.lvwGlobalEdited(Sender: TObject; Item: TListItem; var S: String);
var
  List: TProjectSettingList;
  i: Integer;
  Options: TProjectSetting;
  Prefix: string;
begin
  List := TProjectSettingList(TListView(Sender).Tag);
  if List = FSettings then
    Prefix := 'Local:'
  else
    Prefix := 'Global:';
  S := Trim(S);
  if (S = '') or (List.FindByName(S) <> nil) then
    S := Item.Caption
  else
  begin
    Options := Item.Data;
    Options.Name := S;
    for i := 0 to lvwProjects.Items.Count - 1 do
      if lvwProjects.Items[i].SubItems.Objects[0] = Options then
      begin
        SetProjectAssignment(IOTAProject(lvwProjects.Items[i].Data), Prefix + Options.Name);
        lvwProjects.Items[i].SubItems[0] := Options.Name;
      end;
  end;
end;

procedure TFormManageProjectSetting.lvwGlobalKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F2) and (Shift = []) then
  begin
    if TListView(Sender).Selected <> nil then
      TListView(Sender).Selected.EditCaption;
  end;
end;

procedure TFormManageProjectSetting.lvwGlobalEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  AllowEdit := (Item.ListView <> lvwLocal) or (Item.Caption <> 'Default');
end;

procedure TFormManageProjectSetting.lvwGlobalDblClick(Sender: TObject);
begin
  actGlobalEditSetting.Execute;
end;

procedure TFormManageProjectSetting.RefreshProjectAssignment;
var
  i: Integer;
  Options, EqualOpt: TProjectSetting;
  Item: TListItem;
  OptionsId: string;
begin
  Options := TProjectSetting.Create;
  try
    for i := 0 to lvwProjects.Items.Count - 1 do
    begin
      Item := lvwProjects.Items[i];
      Options.CopyFrom(IOTAProject(Item.Data));
      OptionsId := GetProjectAssignment(IOTAProject(lvwProjects.Items[i].Data));
      if Pos('Local:', OptionsId) = 1 then
        EqualOpt := FSettings.FindByName(Copy(OptionsId, 7, MaxInt))
      else
        EqualOpt := FGlobalSettings.FindByName(Copy(OptionsId, 8, MaxInt));

      if EqualOpt = nil then
        EqualOpt := FSettings.FindEqual(Options);
      if EqualOpt = nil then
        EqualOpt := FGlobalSettings.FindEqual(Options);
      if EqualOpt <> nil then
      begin
        if not EqualOpt.Compare(Options) then
          Item.SubItems[0] := EqualOpt.Name + '*'
        else
          Item.SubItems[0] := EqualOpt.Name;
        Item.SubItems.Objects[0] := EqualOpt;
      end
      else
      begin
        Item.SubItems[0] := '';
        Item.SubItems.Objects[0] := nil;
      end;
    end;
  finally
    Options.Free;
  end;
end;

procedure TFormManageProjectSetting.SetProjectAssignment(Project: IOTAProject; const Value: string);
var
  Index: Integer;
begin
  Index := FProjectAssignments.IndexOfObject(Pointer(Project));
  if Index = -1 then
    FProjectAssignments.AddObject(Value, Pointer(Project))
  else
    FProjectAssignments[Index] := Value;
end;

function TFormManageProjectSetting.GetProjectAssignment(Project: IOTAProject): string;
var
  Index: Integer;
begin
  Index := FProjectAssignments.IndexOfObject(Pointer(Project));
  if Index <> -1 then
    Result := FProjectAssignments[Index]
  else
    Result := VarToStr(ProjectDataList[Project].Values[sProjectSettings]);
end;

procedure TFormManageProjectSetting.UpdateActions;
begin
  inherited UpdateActions;
  btnOk.Default := not (lvwLocal.IsEditing or lvwGlobal.IsEditing);
  btnCancel.Cancel := btnOk.Default;
end;

procedure TFormManageProjectSetting.ActionProjectUpdate(Sender: TObject);
begin
  if Sender = actProjectEditProjectOptions then
    actProjectEditProjectOptions.Enabled := (lvwProjects.SelCount = 1) and (lvwProjects.Selected <> nil)
  else if Sender = actProjectSetVersionInfo then
    actProjectSetVersionInfo.Enabled := lvwProjects.SelCount > 0;
end;

procedure TFormManageProjectSetting.ActionProjectExecute(Sender: TObject);
var
  i: Integer;
  List: TInterfaceList;
begin
  if Sender = actProjectEditProjectOptions then
  begin
    IOTAProject(lvwProjects.Selected.Data).ProjectOptions.EditOptions;
    RefreshProjectAssignment;
  end
  else if Sender = actProjectSetVersionInfo then
  begin
    List := TInterfaceList.Create;
    try
      for i := 0 to lvwProjects.Items.Count - 1 do
        if lvwProjects.Items[i].Selected then
          List.Add(IOTAProject(lvwProjects.Items[i].Data));
      TFormProjectSettingsSetVersioninfo.Execute(List);
    finally
      List.Free;
    end;
  end;
end;

end.

