{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmProjectSettingsSetVersioninfo;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ToolsAPI, ComCtrls, StdCtrls, ExtCtrls, FrmBase, Buttons, ExtDlgs,
  ProjectResource;

type
  IOTAProjectArray = array of IOTAProject;

  TFormProjectSettingsSetVersioninfo = class(TFormBase)
    lblFileVersionMajor: TLabel;
    lblFileVersionMinor: TLabel;
    lblFileVersionRelease: TLabel;
    edtMajor: TEdit;
    udMajor: TUpDown;
    edtMinor: TEdit;
    udMinor: TUpDown;
    edtRelease: TEdit;
    udRelease: TUpDown;
    lblProductVersion: TLabel;
    edtProductVersion: TEdit;
    lblFileVersion: TLabel;
    pnlProjects: TPanel;
    lblProjects: TLabel;
    lvwProjects: TListView;
    btnApplyProductVersion: TButton;
    btnApplyFileVersion: TButton;
    lblProductName: TLabel;
    edtProductName: TEdit;
    btnApplyProductName: TButton;
    lblFileVersionBuild: TLabel;
    edtBuild: TEdit;
    udBuild: TUpDown;
    btnApplyBuild: TButton;
    dtpStartDay: TDateTimePicker;
    btnDaysbetween: TButton;
    edtDaysBetween: TEdit;
    TimerAppliedHide: TTimer;
    lblStartDay: TLabel;
    pgcPages: TPageControl;
    tsSetVersionInfo: TTabSheet;
    pnlLeft: TPanel;
    pnlBottom: TPanel;
    bvlDivider: TBevel;
    lblApplied: TLabel;
    btnClose: TButton;
    tsIncrementVersionInfo: TTabSheet;
    pnlClient: TPanel;
    cbxIncMajor: TCheckBox;
    cbxIncMinor: TCheckBox;
    cbxIncRelease: TCheckBox;
    cbxIncBuild: TCheckBox;
    cbxZeroMinor: TCheckBox;
    cbxZeroRelease: TCheckBox;
    cbxZeroRelease2: TCheckBox;
    btnExecuteIncrement: TButton;
    cbxApplyToSelectedOnly: TCheckBox;
    edtIncMajor: TEdit;
    udIncMajor: TUpDown;
    edtIncMinor: TEdit;
    udIncMinor: TUpDown;
    edtIncRelease: TEdit;
    udIncRelease: TUpDown;
    edtIncBuild: TEdit;
    udIncBuild: TUpDown;
    lblCompanyName: TLabel;
    edtCompanyName: TEdit;
    btnApplyCompanyName: TButton;
    lblLegalCopyright: TLabel;
    edtLegalCopyright: TEdit;
    btnApplyLegalCopyright: TButton;
    tsMainIcon: TTabSheet;
    lblMainIcons: TLabel;
    btnApplyMainIcon: TButton;
    btnLoadMainIcon: TBitBtn;
    dlgOpenMainIcon: TOpenPictureDialog;
    pbxMainIcon: TPaintBox;
    lbxMainIcons: TListBox;
    btnRemoveMainIcon: TBitBtn;
    lblChangeMainIcon: TLabel;
    cbxApplyToAllPlatforms: TCheckBox;
    lblLegalTrademarks: TLabel;
    edtLegalTrademarks: TEdit;
    btnApplyLegalTrademarks: TButton;
    LblFileDescription: TLabel;
    edtFileDescription: TEdit;
    btnApplyFileDescription: TButton;
    LblInternalName: TLabel;
    edtInternalName: TEdit;
    btnApplyInternalName: TButton;
    LblOriginalFilename: TLabel;
    edtOriginalFilename: TEdit;
    btnApplyOriginalFilename: TButton;
    LblComment: TLabel;
    edtComments: TEdit;
    btnApplyComments: TButton;
    btnApplyAutoSetOriginalFilename: TButton;
    procedure edtFileVersionExit(Sender: TObject);
    procedure lvwProjectsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvwProjectsCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure btnApplyStringClick(Sender: TObject);
    procedure btnApplyFileVersionClick(Sender: TObject);
    procedure btnDaysbetweenClick(Sender: TObject);
    procedure TimerAppliedHideTimer(Sender: TObject);
    procedure btnApplyBuildClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxIncMajorClick(Sender: TObject);
    procedure cbxIncMinorClick(Sender: TObject);
    procedure btnExecuteIncrementClick(Sender: TObject);
    procedure cbxApplyToSelectedOnlyClick(Sender: TObject);
    procedure cbxIncReleaseClick(Sender: TObject);
    procedure cbxIncBuildClick(Sender: TObject);
    procedure btnLoadMainIconClick(Sender: TObject);
    procedure btnApplyMainIconClick(Sender: TObject);
    procedure lbxMainIconsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbxMainIconPaint(Sender: TObject);
    procedure btnRemoveMainIconClick(Sender: TObject);
    procedure pgcPagesChange(Sender: TObject);
    procedure btnApplyAutoSetOriginalFilenameClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FIcon: TIconResource;
    function DoExecute(Projects: TInterfaceList): Boolean; virtual;
    function GetStartDateTimeOf(const Project: IOTAProject): TDateTime;
    procedure LoadSettings;
    procedure SaveSettings;
    function GetValidApplyProjects: IOTAProjectArray;
    function HasValidProjects: Boolean;
    function GetSelectedApplyProject: IOTAProject;
  protected
    procedure UpdateMainIconPreview;
    procedure UpdateActions; override;
    procedure ApplyFinished(const Text: string = '');
  public
    { Public-Deklarationen }
    class function Execute(Projects: TInterfaceList): Boolean;
  end;

procedure InitPlugin(Unload: Boolean);

implementation

uses
  IDEUtils, IDEHooks, Hooking, DtmImages,
  ActnList, Menus, AppConsts, ToolsAPIHelpers, DateUtils, Math, PluginConfig, SimpleXmlIntf;

{$R *.dfm}

type
  TVersionInfoHandler = class(TComponent)
  private
    FMenuItemSetVersionInfo: TMenuItem;
    FActionSetVersionInfo: TAction;
    FActionList: TActionList;
  public
    constructor Create(AOwner: TComponent); override;

    procedure DoUpdateSetVersionInfo(Sender: TObject = nil);
    procedure DoSetVersionInfo(Sender: TObject = nil);
  end;

var
  VersionInfoHandler: TVersionInfoHandler;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    VersionInfoHandler := TVersionInfoHandler.Create(nil)
  else
    FreeAndNil(VersionInfoHandler);
end;

{ TVersionInfoHandler }

constructor TVersionInfoHandler.Create(AOwner: TComponent);
var
  ProjectMenu: TMenuItem;
begin
  inherited Create(AOwner);
  FActionList := TActionList.Create(Self);
  FActionList.Images := DataModuleImages.imlIcons;

  ProjectMenu := FindMenuItem('ProjectMenu');
  if ProjectMenu <> nil then
  begin
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
    ProjectMenu.Add(FMenuItemSetVersionInfo);
  end;
end;

procedure TVersionInfoHandler.DoUpdateSetVersionInfo(Sender: TObject);
var
  Group: IOTAProjectGroup;
begin
  Group := GetActiveProjectGroup;
  FActionSetVersionInfo.Enabled := (Group <> nil) and (Group.ProjectCount > 0)
    and (GetActiveProject <> nil)
    and ((GetActiveProject.GetPersonality = sDelphiPersonality) or
         (GetActiveProject.GetPersonality = sCBuilderPersonality));
end;

procedure TVersionInfoHandler.DoSetVersionInfo(Sender: TObject);
var
  I: Integer;
  List: TInterfaceList;
  Group: IOTAProjectGroup;
begin
  Group := GetActiveProjectGroup;
  if Assigned(Group) then
  begin
    List := TInterfaceList.Create;
    try
      for I := 0 to Group.ProjectCount - 1 do
        List.Add(Group.Projects[I]);
      TFormProjectSettingsSetVersioninfo.Execute(List);
    finally
      List.Free;
    end;
  end;
end;


{ TFormProjectSettingsSetVersioninfo }

class function TFormProjectSettingsSetVersioninfo.Execute(Projects: TInterfaceList): Boolean;
begin
  with Self.Create(nil) do
  try
    Result := DoExecute(Projects);
  finally
    Free;
  end;
end;

function TFormProjectSettingsSetVersioninfo.DoExecute(Projects: TInterfaceList): Boolean;
var
  I: Integer;
  Version: TProjectVersion;
  Item: TListItem;
  Project: IOTAProject;
begin
  LoadSettings;

  lvwProjects.Items.BeginUpdate;
  try
    lvwProjects.Items.Clear;
    for I := 0 to Projects.Count - 1 do
    begin
      Project := IOTAProject(Projects[I]);
      Item := lvwProjects.Items.Add;
      Item.Caption := ExtractFileName(Project.FileName);
      Item.Data := Pointer(Project);
      Version := GetProjectVersion(Project);
      if Version.Valid then
      begin
        Item.SubItems.Add('');
        Item.ImageIndex := 4;
      end
      else
      begin
        Item.SubItems.Add('no resource');
        Item.ImageIndex := 8;
      end;
    end;
    if lvwProjects.Items.Count > 0 then
      lvwProjects.Selected := lvwProjects.Items[0];
  finally
    lvwProjects.Items.EndUpdate;
  end;
  Result := ShowModal = mrOk;

  SaveSettings;
end;

procedure TFormProjectSettingsSetVersioninfo.SaveSettings;
var
  Node: IXmlNode;
begin
  Node := Configuration.GetNode('ProjectSetVersionInfo');
  Node.Attributes['ApplyToSelected'] := cbxApplyToSelectedOnly.Checked;
  Node.Attributes['ApplyToAllPlatforms'] := cbxApplyToAllPlatforms.Checked;
  Configuration.Modified;
end;

procedure TFormProjectSettingsSetVersioninfo.LoadSettings;
var
  Node: IXmlNode;
begin
  Node := Configuration.FindNode('ProjectSetVersionInfo');
  if Node <> nil then
  begin
    cbxApplyToSelectedOnly.Checked := VarToBoolDef(Node.Attributes['ApplyToSelected'], False);
    cbxApplyToAllPlatforms.Checked := VarToBoolDef(Node.Attributes['ApplyToAllPlatforms'], False);
  end;

  cbxApplyToSelectedOnlyClick(cbxApplyToSelectedOnly);
end;

function TFormProjectSettingsSetVersioninfo.GetSelectedApplyProject: IOTAProject;
var
  Item: TListItem;
begin
  Result := nil;
  Item := lvwProjects.Selected;
  if (Item <> nil) then
    if (Item.SubItems.Count = 0) or (Item.SubItems[0] = '') then // Has valid version resource then
      Result := IOTAProject(Item.Data);
end;

function TFormProjectSettingsSetVersioninfo.GetValidApplyProjects: IOTAProjectArray;
var
  I, Count: Integer;
  ApplyToSelectedOnly: Boolean;
  Item: TListItem;
  SelProject: IOTAProject;
begin
  ApplyToSelectedOnly := cbxApplyToSelectedOnly.Checked;
  SetLength(Result, lvwProjects.Items.Count); // worst case

  Count := 0;
  for I := 0 to lvwProjects.Items.Count - 1 do
  begin
    Item := lvwProjects.Items[I];
    if (Item.SubItems.Count = 0) or (Item.SubItems[0] = '') then // Has valid version resource
    begin
      if not ApplyToSelectedOnly or lvwProjects.Items[I].Checked then
      begin
        Result[Count] := IOTAProject(lvwProjects.Items[I].Data);
        Inc(Count);
      end;
    end;
  end;

  if (Count = 0) and ApplyToSelectedOnly then
  begin
    SelProject := GetSelectedApplyProject;
    if SelProject <> nil then
    begin
      Result[0] := SelProject;
      Count := 1;
    end;
  end;

  SetLength(Result, Count);
end;

function TFormProjectSettingsSetVersioninfo.HasValidProjects: Boolean;
var
  I: Integer;
  ApplyToSelectedOnly: Boolean;
  Item: TListItem;
begin
  ApplyToSelectedOnly := cbxApplyToSelectedOnly.Checked;
  Result := True;
  for I := 0 to lvwProjects.Items.Count - 1 do
  begin
    Item := lvwProjects.Items[I];
    if (Item.SubItems.Count = 0) or (Item.SubItems[0] = '') then // Has valid version resource
      if not ApplyToSelectedOnly or lvwProjects.Items[I].Checked then
        Exit;
  end;

  if ApplyToSelectedOnly and (GetSelectedApplyProject <> nil) then
    Exit;

  Result := False;
end;

function TFormProjectSettingsSetVersioninfo.GetStartDateTimeOf(const Project: IOTAProject): TDateTime;
var
  sh: THandle;
  Data: TWin32FindData;
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := Date;
  if Project <> nil then
  begin
    { Get file creation time }
    sh := FindFirstFile(PChar(Project.FileName), Data);
    if sh <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(sh);
      FileTimeToLocalFileTime(Data.ftCreationTime, LocalFileTime);
      FileTimeToSystemTime(LocalFileTime, SystemTime);
      with SystemTime do
        Result := EncodeDate(wYear, wMonth, wDay) +
                  EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
    end
  end;
end;

procedure TFormProjectSettingsSetVersioninfo.lvwProjectsSelectItem(
  Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Version: TProjectVersion;
begin
  if csDestroying in ComponentState then
    Exit;

  Version.Valid := False;
  if Selected then
  begin
    Version := GetProjectVersion(IOTAProject(Item.Data));
    {if not Version.Valid then
      lvwProjects.Selected := nil;}
  end;
  if Version.Valid then
  begin
    udMajor.Position := Version.FileVersion.Major;
    udMinor.Position := Version.FileVersion.Minor;
    udRelease.Position := Version.FileVersion.Release;
    udBuild.Position := Version.FileVersion.Build;
    edtProductVersion.Text := Version.ProductVersionStr;
    edtProductName.Text := Version.ProductName;
    edtCompanyName.Text := Version.CompanyName;
    edtLegalCopyright.Text := Version.LegalCopyright;
    edtLegalTrademarks.Text := Version.LegalTrademarks;
    edtFileDescription.Text := Version.FileDescription;
    edtInternalName.Text := Version.InternalName;
    edtOriginalFilename.Text := Version.OriginalFilename;
    edtComments.Text := Version.Comments;

    dtpStartDay.Date := GetStartDateTimeOf(IOTAProject(Item.Data));
  end
  else
  begin
    udMajor.Position := 1;
    udMinor.Position := 0;
    udRelease.Position := 0;
    udBuild.Position := 0;
    edtProductVersion.Text := '';
    edtProductName.Text := '';
    edtCompanyName.Text := '';
    edtLegalCopyright.Text := '';
    edtLegalTrademarks.Text := '';
    edtFileDescription.Text := '';
    edtInternalName.Text := '';
    edtOriginalFilename.Text := '';
    edtComments.Text := '';
  end;
  FIcon.LoadFromProjectResource(IOTAProject(Item.Data));
  UpdateMainIconPreview;
end;

procedure TFormProjectSettingsSetVersioninfo.pbxMainIconPaint(Sender: TObject);
var
  Ico: HICON;
  Index: Integer;
  Item: TIconResourceItem;
  R: TRect;
begin
  Index := lbxMainIcons.ItemIndex;
  if Index = -1 then
    Index := 0;
  if Index >= FIcon.Count then
    Exit;
  Ico := FIcon.GetPaintIcon(Index);
  if Ico = 0 then
    Exit;

  Item := FIcon.Images[Index];

  R := Rect(0, 0, Max(48, Item.Width), Max(48, Item.Height));
  InflateRect(R, 4, 4);
  OffsetRect(R, -R.Left, -R.Top);

  if Ico <> 0 then
    DrawIconEx(
      pbxMainIcon.Canvas.Handle,
      R.Left + (R.Right - R.Left - Item.Width) div 2,
      R.Top + (R.Bottom - R.Top - Item.Height) div 2,
      Ico, Item.Width, Item.Height, 0, 0, DI_NORMAL
    );

  { Border }
  Inc(R.Right);
  Inc(R.Bottom);
  pbxMainIcon.Canvas.Brush.Style := bsClear;
  pbxMainIcon.Canvas.Pen.Color := $FF9933;
  pbxMainIcon.Canvas.Pen.Width := 2;
  pbxMainIcon.Canvas.RoundRect(R, 8, 8);
  pbxMainIcon.Canvas.Pen.Width := 1;
  InflateRect(R, -1, -1);
  pbxMainIcon.Canvas.RoundRect(R, 7, 7);
end;

procedure TFormProjectSettingsSetVersioninfo.pgcPagesChange(Sender: TObject);
begin
  lvwProjects.Invalidate;
end;

procedure TFormProjectSettingsSetVersioninfo.lbxMainIconsClick(Sender: TObject);
begin
  pbxMainIcon.Invalidate;
end;

procedure TFormProjectSettingsSetVersioninfo.lvwProjectsCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  DefaultDraw := True;
  if (Item.SubItems.Count > 0) and (Item.SubItems[0] <> '') and (pgcPages.ActivePage <> tsMainIcon) then
    Sender.Canvas.Font.Color := clSilver;
end;

procedure TFormProjectSettingsSetVersioninfo.btnApplyStringClick(Sender: TObject);

  function GetVersionPString(var {ref} Version: TProjectVersion): PString;
  begin
    if Sender = btnApplyProductName then
      Result := @Version.ProductName

    else if Sender = btnApplyProductVersion then
      Result := @Version.ProductVersionStr

    else if Sender = btnApplyCompanyName then
      Result := @Version.CompanyName

    else if Sender = btnApplyLegalCopyright then
      Result := @Version.LegalCopyright

    else if Sender = btnApplyLegalTrademarks then
      Result := @Version.LegalTrademarks

    else if Sender = btnApplyFileDescription then
      Result := @Version.FileDescription

    else if Sender = btnApplyInternalName then
      Result := @Version.InternalName

    else if Sender = btnApplyComments then
      Result := @Version.Comments

    else if Sender = btnApplyOriginalFilename then
      Result := @Version.OriginalFilename

    else
      raise Exception.Create('Fehlermeldung');
  end;

var
  Version: TProjectVersion;
  Value: string;
  Edit: TEdit;
  EditName: string;
  P: PString;
  Project: IOTAProject;
  Projects: IOTAProjectArray;
begin
  EditName := 'edt' + Copy((Sender as TComponent).Name, Length('btnApply') + 1, MaxInt);
  Edit := FindComponent(EditName) as TEdit;
  Assert( Edit <> nil, 'Edit "' + EditName + '" not found' );

  Value := Trim(Edit.Text);
  if Sender = btnApplyOriginalFilename then // Apply to this
  begin
    Project := GetSelectedApplyProject();
    if Project <> nil then
      Projects := IOTAProjectArray.Create(Project)
    else
      Projects := nil;
  end
  else
    Projects := GetValidApplyProjects();

  for Project in Projects do
  begin
    Version := GetProjectVersion(Project);
    if Version.Valid then
    begin
      P := GetVersionPString(Version);
      if P^ <> Value then
      begin
        P^ := Value;
        SetProjectVersion(Project, Version, cbxApplyToAllPlatforms.Checked);
      end;
    end;
  end;
  ApplyFinished;
end;

procedure TFormProjectSettingsSetVersioninfo.btnLoadMainIconClick(Sender: TObject);
begin
  if dlgOpenMainIcon.Execute then
  begin
    FIcon.LoadFromIconFile(dlgOpenMainIcon.FileName);
    UpdateMainIconPreview;
  end;
end;

procedure TFormProjectSettingsSetVersioninfo.btnRemoveMainIconClick(Sender: TObject);
begin
  FIcon.Clear;
  UpdateMainIconPreview;
end;

procedure TFormProjectSettingsSetVersioninfo.btnApplyFileVersionClick(Sender: TObject);
var
  Project: IOTAProject;
  Version: TProjectVersion;
begin
  for Project in GetValidApplyProjects() do
  begin
    Version := GetProjectVersion(Project);
    if Version.Valid and
      ((udMajor.Position <> SmallInt(Version.FileVersion.Major)) or
       (udMinor.Position <> SmallInt(Version.FileVersion.Minor)) or
       (udRelease.Position <> SmallInt(Version.FileVersion.Release))) then
    begin
      Version.FileVersion.Major := udMajor.Position;
      Version.FileVersion.Minor := udMinor.Position;
      Version.FileVersion.Release := udRelease.Position;
      with Version.FileVersion do
        Version.FileVersionStr := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
      SetProjectVersion(Project, Version, cbxApplyToAllPlatforms.Checked);
    end;
  end;
  ApplyFinished;
end;

procedure TFormProjectSettingsSetVersioninfo.btnApplyMainIconClick(Sender: TObject);
var
  Project: IOTAProject;
begin
  for Project in GetValidApplyProjects() do
    FIcon.SaveToProjectResource(Project);
  ApplyFinished('Main Icons replaced.');
end;

procedure TFormProjectSettingsSetVersioninfo.btnApplyAutoSetOriginalFilenameClick(Sender: TObject);
var
  Project: IOTAProject;
  Version: TProjectVersion;
  Value: string;
begin
  for Project in GetValidApplyProjects() do
  begin
    Version := GetProjectVersion(Project);
    if Version.Valid then
    begin
      Value := ExtractFileName(Project.ProjectOptions.TargetName);
      if Value <> Version.OriginalFilename then
      begin
        Version.OriginalFilename := Value;
        SetProjectVersion(Project, Version, cbxApplyToAllPlatforms.Checked);
      end;
    end;
  end;
  // Update selected version info
  if GetSelectedApplyProject <> nil then
  begin
    Version := GetProjectVersion(GetSelectedApplyProject);
    edtOriginalFilename.Text := Version.OriginalFilename;
  end;
  ApplyFinished;
end;

procedure TFormProjectSettingsSetVersioninfo.btnApplyBuildClick(Sender: TObject);
var
  Project: IOTAProject;
  Version: TProjectVersion;
  Value: Integer;
begin
  Value := udBuild.Position;
  for Project in GetValidApplyProjects() do
  begin
    Version := GetProjectVersion(Project);
    if Version.Valid and (SmallInt(Version.FileVersion.Build) <> Value) then
    begin
      Version.FileVersion.Build := Value;
      with Version.FileVersion do
        Version.FileVersionStr := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
      SetProjectVersion(Project, Version, cbxApplyToAllPlatforms.Checked);
    end;
  end;
  ApplyFinished;
end;

procedure TFormProjectSettingsSetVersioninfo.UpdateActions;
var
  ValidProjectsAvailable: Boolean;
begin
  inherited UpdateActions;

  { Check if there is at least one project that has a VersionInfo resource }
  ValidProjectsAvailable := HasValidProjects();

  { Enable the Apply buttons }
  btnApplyProductVersion.Enabled := (Trim(edtProductVersion.Text) <> '') and ValidProjectsAvailable;
  btnApplyProductName.Enabled := {(Trim(edtProductName.Text) <> '') and} ValidProjectsAvailable;
  btnApplyCompanyName.Enabled := {(Trim(edtCompanyName.Text) <> '') and} ValidProjectsAvailable;
  btnApplyLegalCopyright.Enabled := {(Trim(edtLegalCopyright.Text) <> '') and} ValidProjectsAvailable;
  btnApplyLegalTrademarks.Enabled := {(Trim(edtLegalTrademarks.Text) <> '') and} ValidProjectsAvailable;
  btnApplyFileDescription.Enabled := {(Trim(edtFileDescription.Text) <> '') and} ValidProjectsAvailable;
  btnApplyInternalName.Enabled := {(Trim(edtInternalName.Text) <> '') and} ValidProjectsAvailable;
  btnApplyComments.Enabled := {(Trim(edtComments.Text) <> '') and} ValidProjectsAvailable;
  btnApplyOriginalFilename.Enabled := GetSelectedApplyProject <> nil;
  btnApplyAutoSetOriginalFilename.Enabled := ValidProjectsAvailable;

  btnApplyBuild.Enabled := (Trim(edtBuild.Text) <> '') and ValidProjectsAvailable;
  btnApplyFileVersion.Enabled := (Trim(edtMajor.Text) <> '') and
                                 (Trim(edtMinor.Text) <> '') and
                                 (Trim(edtRelease.Text) <> '') and
                                 ValidProjectsAvailable;

  if pgcPages.ActivePage = tsIncrementVersionInfo then
  begin
    btnExecuteIncrement.Enabled := ValidProjectsAvailable and
                                   (cbxIncMajor.Checked or cbxIncMinor.Checked or
                                    cbxIncRelease.Checked or cbxIncBuild.Checked);

    cbxIncMinor.Enabled := not cbxIncMajor.Checked or not cbxZeroMinor.Checked;
    if not cbxIncMinor.Enabled then
      cbxIncMinor.Checked := False;
    cbxIncRelease.Enabled := (not cbxIncMajor.Checked or not cbxZeroRelease.Checked) and
                             (not cbxIncMinor.Checked or not cbxZeroRelease2.Checked);
    if not cbxIncRelease.Enabled then
      cbxIncRelease.Checked := False;

    cbxIncMajorClick(cbxIncMajor);
    cbxIncMinorClick(cbxIncMinor);
    cbxIncReleaseClick(cbxIncRelease);
    cbxIncBuildClick(cbxIncBuild);
  end;
end;

procedure TFormProjectSettingsSetVersioninfo.UpdateMainIconPreview;
var
  I: Integer;
  Item: TIconResourceItem;
begin
  lbxMainIcons.Items.BeginUpdate;
  try
    lbxMainIcons.Items.Clear;
    for I := 0 to FIcon.Count - 1 do
    begin
      Item := FIcon.Images[I];
      lbxMainIcons.AddItem(Format('%dx%d - %d', [Item.Width, Item.Height, Item.BitCount]), TObject(I));
    end;
  finally
    lbxMainIcons.Items.EndUpdate;
  end;
  if lbxMainIcons.Items.Count > 0 then
    lbxMainIcons.ItemIndex := 0;
  pbxMainIcon.Invalidate;
end;

procedure TFormProjectSettingsSetVersioninfo.ApplyFinished(const Text: string);
begin
  if Text = '' then
    lblApplied.Caption := 'Versioninfo updated.'
  else
    lblApplied.Caption := Text;
  lblApplied.Visible := True;
  TimerAppliedHide.Enabled := False; // reset timer
  TimerAppliedHide.Enabled := True;
end;

procedure TFormProjectSettingsSetVersioninfo.btnDaysbetweenClick(
  Sender: TObject);
begin
  edtDaysBetween.Text := IntToStr(Trunc(Date) - Trunc(dtpStartDay.Date));
end;

procedure TFormProjectSettingsSetVersioninfo.TimerAppliedHideTimer(Sender: TObject);
begin
  TimerAppliedHide.Enabled := False;
  lblApplied.Visible := False;
end;

procedure TFormProjectSettingsSetVersioninfo.edtFileVersionExit(Sender: TObject);
var
  Value: Integer;
begin
  if TryStrToInt((Sender as TEdit).Text, Value) then
    (FindComponent('ud' + Copy(TComponent(Sender).Name, 4, MaxInt)) as TUpDown).Position := Value;
end;

procedure TFormProjectSettingsSetVersioninfo.FormCreate(Sender: TObject);
begin
  inherited;
  FIcon := TIconResource.Create;
  dtpStartDay.Date := Date;
  pgcPages.ActivePageIndex := 0;
  btnClose.Anchors := [akTop, akRight];
  {$IF CompilerVersion >= 23.0} // Delphi XE2+
  {$ELSE}
  cbxApplyToAllPlatforms.Visible := False;
  {$IFEND}
end;

procedure TFormProjectSettingsSetVersioninfo.FormDestroy(Sender: TObject);
begin
  inherited;
  FIcon.Free;
end;

procedure TFormProjectSettingsSetVersioninfo.cbxIncMajorClick(Sender: TObject);
begin
  cbxZeroMinor.Enabled := cbxIncMajor.Checked;
  cbxZeroRelease.Enabled := cbxIncMajor.Checked;
  edtIncMajor.Enabled := cbxIncMajor.Checked;
  udIncMajor.Enabled := cbxIncMajor.Checked;
end;

procedure TFormProjectSettingsSetVersioninfo.cbxIncMinorClick(Sender: TObject);
begin
  cbxZeroRelease2.Enabled := cbxIncMinor.Checked;
  edtIncMinor.Enabled := cbxIncMinor.Checked;
  udIncMinor.Enabled := cbxIncMinor.Checked;
end;

procedure TFormProjectSettingsSetVersioninfo.btnExecuteIncrementClick(Sender: TObject);
var
  Project: IOTAProject;
  Version: TProjectVersion;
begin
  for Project in GetValidApplyProjects() do
  begin
    Version := GetProjectVersion(Project);
    if Version.Valid then
    begin
      if cbxIncBuild.Checked then
        Inc(Version.FileVersion.Build, udIncBuild.Position);
      if cbxIncRelease.Checked then
        Inc(Version.FileVersion.Release, udIncRelease.Position);
      if cbxIncMinor.Checked then
      begin
        Inc(Version.FileVersion.Minor, udIncMinor.Position);
        if cbxZeroRelease2.Checked then
          Version.FileVersion.Release := 0;
      end;
      if cbxIncMajor.Checked then
      begin
        Inc(Version.FileVersion.Major, udIncMajor.Position);
        if cbxZeroMinor.Checked then
          Version.FileVersion.Minor := 0;
        if cbxZeroRelease.Checked then
          Version.FileVersion.Release := 0;
      end;

      with Version.FileVersion do
        Version.FileVersionStr := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
      SetProjectVersion(Project, Version, cbxApplyToAllPlatforms.Checked);
    end;
  end;
  ApplyFinished;
end;

procedure TFormProjectSettingsSetVersioninfo.cbxApplyToSelectedOnlyClick(Sender: TObject);
var
  ApplyStr: string;
begin
  if cbxApplyToSelectedOnly.Checked then
    ApplyStr := 'Apply'
  else
    ApplyStr := 'Apply to all';
  btnApplyProductVersion.Caption := ApplyStr;
  btnApplyFileVersion.Caption := ApplyStr;
  btnApplyProductName.Caption := ApplyStr;
  btnApplyCompanyName.Caption := ApplyStr;
  btnApplyLegalCopyright.Caption := ApplyStr;
  btnApplyLegalTrademarks.Caption := ApplyStr;
  btnApplyFileDescription.Caption := ApplyStr;
  btnApplyInternalName.Caption := ApplyStr;
  btnApplyComments.Caption := ApplyStr;
  //btnApplyOriginalFilename.Caption := do not change;
  if cbxApplyToSelectedOnly.Checked then
    btnApplyAutoSetOriginalFilename.Caption := 'Auto set'
  else
    btnApplyAutoSetOriginalFilename.Caption := 'Auto set to all';

  btnApplyBuild.Caption := ApplyStr;
  btnApplyMainIcon.Caption := ApplyStr;

  lvwProjects.Checkboxes := cbxApplyToSelectedOnly.Checked;
end;

procedure TFormProjectSettingsSetVersioninfo.cbxIncReleaseClick(Sender: TObject);
begin
  edtIncRelease.Enabled := cbxIncRelease.Checked;
  udIncRelease.Enabled := cbxIncRelease.Checked;
end;

procedure TFormProjectSettingsSetVersioninfo.cbxIncBuildClick(Sender: TObject);
begin
  edtIncBuild.Enabled := cbxIncBuild.Checked;
  udIncBuild.Enabled := cbxIncBuild.Checked;
end;

end.
