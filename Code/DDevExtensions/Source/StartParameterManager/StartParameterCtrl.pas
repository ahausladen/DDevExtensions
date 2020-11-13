unit StartParameterCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, StdCtrls, ToolsAPI,
  StartParameterClasses, Menus;

type
  TStartParameterControl = class;

  TProjectParameters = class(TInterfacedObject, IOTANotifier, IOTAModuleNotifier)
  private
    FParameters: TStartParamList;
    FOwner: TStartParameterControl;
    FProject: IOTAProject;
    FNotifier: Integer;
    function GetLocalParameters: TStartParamList;
  protected
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    { IOTAModuleNotifier }
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);
  public
    constructor Create(AOwner: TStartParameterControl; const AProject: IOTAProject);
    destructor Destroy; override;
    class function GetParamFileName(const AFileName: string; ALocal: Boolean): string;

    procedure Load;
    procedure Save;

    property Parameters: TStartParamList read FParameters;
    property LocalParameters: TStartParamList read GetLocalParameters;
    property Project: IOTAProject read FProject;
  end;

  TStartParameterControl = class(TCustomComboBox, IOTANotifier, IOTAIDENotifier)
  private
    FNotifier: Integer;
    FProjectParams: TObjectList;
    FActiveParams: TProjectParameters;
    function FindProject(const AProject: IOTAProject): TProjectParameters;
    procedure DoEditFileClick(Sender: TObject);
  protected
    procedure CreateWnd; override;
    procedure Change; override;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    procedure WMCopy(var Msg: TWMCopy); message WM_COPY;

    procedure DoPopup(Sender: TObject);

    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    { IOTAIDENotifier }
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;

    procedure Loaded; override;
    procedure DropDown; override;
    {$IF CompilerVersion >= 33.0} // 10.3 Rio+
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    {$IFEND}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateStartParameters(const ADestroyingProject: IOTAProject);
    procedure Reload;

    class function GetActiveParams(var Params: string; AllowReload: Boolean): Boolean; static;
    class function IsDefaultParams: Boolean; static;
  published
    property Enabled;
    property Visible;
    property Action;
    property Left;
    property Top;
    property Width;
    property Height;
  end;

implementation

uses
  Forms, Clipbrd;

const
  sLocalIncludeParamFileName = '$(ParamFileName).local';

var
  GlobalStartParameterControl: TStartParameterControl;

type
  TFileMenuItem = class(TMenuItem)
  private
    FFileName: string;
  public
    property FileName: string read FFileName write FFileName;
  end;

{ TProjectParameters }

constructor TProjectParameters.Create(AOwner: TStartParameterControl; const AProject: IOTAProject);
begin
  inherited Create;
  FOwner := AOwner;
  FOwner.FProjectParams.Add(Self);
  FProject := AProject;
  FNotifier := FProject.AddNotifier(Self);
  FParameters := TStartParamList.Create(nil);
end;

destructor TProjectParameters.Destroy;
begin
  if FOwner.FProjectParams <> nil then
    FOwner.FProjectParams.Extract(Self);
  if FNotifier <> -1 then
    FProject.RemoveNotifier(FNotifier);

  if Self = FOwner.FActiveParams then
  begin
    FOwner.FActiveParams := nil;
    FOwner.UpdateStartParameters(FProject);
  end;
  FProject := nil;
  inherited Destroy;
end;

procedure TProjectParameters.AfterSave;
begin
end;

procedure TProjectParameters.BeforeSave;
begin
end;

function TProjectParameters.CheckOverwrite: Boolean;
begin
  Result := True;
end;

procedure TProjectParameters.Destroyed;
begin
  if Self = FOwner.FActiveParams then
  begin
    FOwner.FActiveParams := nil;
    FOwner.UpdateStartParameters(FProject);
  end;
  FProject := nil;
  FNotifier := -1;
end;

procedure TProjectParameters.Modified;
begin
end;

procedure TProjectParameters.ModuleRenamed(const NewName: string);
begin
  if Parameters.FindIncludeParamList(sLocalIncludeParamFileName) <> nil then
    if LocalParameters.FileName <> '' then
      MoveFile(PChar(LocalParameters.FileName), PChar(GetParamFileName(NewName, True)));
  if FParameters.FileName <> '' then
    MoveFile(PChar(Parameters.FileName), PChar(GetParamFileName(NewName, False)));
end;

procedure TProjectParameters.Save;
var
  LocalParamList: TStartParamList;
begin
  LocalParamList := LocalParameters;
  if LocalParamList.Modified then
  begin
    if (LocalParamList.Count > 0) or (LocalParamList.ActiveParamName <> '') then
      LocalParameters.SaveToFile(GetParamFileName(Project.FileName, True))
    else // delete the local file if it contains nothing
      DeleteFile(GetParamFileName(Project.FileName, True));
  end;
  // Save project parameters after the local because the local Include-tag may have been missing
  // and was added by GetLocalParameters
  if Parameters.Modified then
    FParameters.SaveToFile(GetParamFileName(Project.FileName, False));
end;

function TProjectParameters.GetLocalParameters: TStartParamList;
begin
  Result := Parameters.FindIncludeParamList(sLocalIncludeParamFileName);
  if Result = nil then
    Result := Parameters.AddInclude(sLocalIncludeParamFileName, False).StartParamList;
end;

class function TProjectParameters.GetParamFileName(const AFileName: string; ALocal: Boolean): string;
begin
  Result := ChangeFileExt(AFileName, '.params');
  if ALocal then
    Result := Result + '.local';
end;

procedure TProjectParameters.Load;
var
  ParamFileName: string;
begin
  if Project <> nil then
  begin
    ParamFileName := GetParamFileName(Project.FileName, False);
    if FileExists(ParamFileName) then
      Parameters.LoadFromFile(ParamFileName)
    else
      Parameters.Clear;
  end
  else
    Parameters.Clear;
end;

{ TStartParameterControl }

constructor TStartParameterControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GlobalStartParameterControl := Self;
  Style := csOwnerDrawFixed;
  ItemHeight := 16;
  DropDownCount := 32;
  ShowHint := True;
  Width := 180;
  Height := 21;

  FProjectParams := TObjectList.Create;
  FNotifier := (BorlandIDEServices as IOTAServices).AddNotifier(Self);

  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.OnPopup := DoPopup;
end;

destructor TStartParameterControl.Destroy;
begin
  if GlobalStartParameterControl = Self then
    GlobalStartParameterControl := nil;

  if FNotifier <> -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(FNotifier);
  FreeAndNil(FProjectParams);
  inherited Destroy;
end;

procedure TStartParameterControl.Loaded;
begin
  inherited Loaded;
  Width := 180;
end;

class function TStartParameterControl.GetActiveParams(var Params: string; AllowReload: Boolean): Boolean;
begin
  Params := '';
  Result := not IsDefaultParams;
  if Result then
  begin
    if AllowReload then
      GlobalStartParameterControl.UpdateStartParameters(nil);
    Params := (GlobalStartParameterControl.Items.Objects[GlobalStartParameterControl.ItemIndex] as TStartParam).ResolvedValue;
  end;
end;

class function TStartParameterControl.IsDefaultParams: Boolean;
begin
  Result := (GlobalStartParameterControl = nil) or not GlobalStartParameterControl.HandleAllocated or
            (GlobalStartParameterControl.ItemIndex <= 0);
end;

procedure TStartParameterControl.CreateWnd;
begin
  inherited CreateWnd;
  UpdateStartParameters(nil);
end;

procedure TStartParameterControl.Change;
var
  Param: TStartParam;
  ActiveParamName: string;
begin
  inherited Change;
  if (FActiveParams <> nil) and HandleAllocated then
  begin
    ActiveParamName := '';
    if ItemIndex > 0 then
    begin
      Param := Items.Objects[ItemIndex] as TStartParam;
      if Param <> nil then
        ActiveParamName := Param.Name;
    end;

    // Reload Parameters in order to not loose user modification from disk
    if ActiveParamName <> FActiveParams.LocalParameters.ActiveParamName then
    begin
      Reload;
      FActiveParams.LocalParameters.ActiveParamName := ActiveParamName;
      FActiveParams.Save;
      UpdateStartParameters(nil); // Reload broke Items.Objects[]
    end;
  end;
end;

procedure TStartParameterControl.Destroyed;
begin
  FNotifier := -1;
end;

procedure TStartParameterControl.DoEditFileClick(Sender: TObject);
var
  FileName: string;
  Lines: TStrings;
  LocalFile: Boolean;
begin
  FileName := (Sender as TFileMenuItem).FileName;
  if not FileExists(FileName) then
  begin
    LocalFile := SameText('.local', ExtractFileExt(FileName));
    Lines := TStringList.Create;
    try
      Lines.Add('<?xml version="1.0" encoding="utf-8"?>');
      Lines.Add('<StartParameters>');

      if not LocalFile then
      begin
        Lines.Add('<!--');
        Lines.Add('');
        Lines.Add('Predefined Macros:');
        Lines.Add('  $(ParamFileName)      Full Filename of the *.params file');
        Lines.Add('  $(ParamFilePath)      Direcory of the *.params file');
        Lines.Add('  $(Year)               Current year');
        Lines.Add('  $(Month)              Current month');
        Lines.Add('  $(Day)                Current day');
        Lines.Add('  $(MonthShortName)     Short name (3 charaters) of the current month (local settings)');
        Lines.Add('  $(MonthName)          Full name of the current month (local settings)');
        Lines.Add('  $(LastMonthShortName) Short name (3 charaters) of the last month (local settings)');
        Lines.Add('  $(LastMonthName)      Full name of the last month (local settings)');
        Lines.Add('  $(LastYear)           Last year');
        Lines.Add('');
        Lines.Add('  $(ParamFileName)      => projectpath\project.param');
        Lines.Add('  $(ParamFilePath)      => projectpath\');
        Lines.Add('  $(Year)               => 2011');
        Lines.Add('  $(Month)              => 12');
        Lines.Add('  $(Day)                => 19');
        Lines.Add('  $(MonthShortName)     => Dec');
        Lines.Add('  $(MonthName)          => December');
        Lines.Add('  $(LastMonthShortName) => Nov');
        Lines.Add('  $(LastMonthName)      => November');
        Lines.Add('  $(LastYear)           => 2010');
        Lines.Add('');
        Lines.Add('');
        Lines.Add('Include, Macro and Param can have a "Condition" attribute:');
        Lines.Add('  <Macro Name="DB" Condition=" exists(''$(ParamFileName).local'') and true ">value</Macro>');
        Lines.Add('  <Macro Name="DB" Condition=" ''$(DB)'' == '''' ">value</Macro>');
        Lines.Add('Available condition functions:');
        Lines.Add('  Exists(''filename'')   returns true if the file exists');
        Lines.Add('');
        Lines.Add('Include-Tag attributes:');
        Lines.Add('  File:        File that should be included');
        Lines.Add('  [Force]:     true : Shows an error message if the file doesn''t exist.');
        Lines.Add('               false: Ignore missing files');
        Lines.Add('  [Condition]: Include the file only if the condition evaluates to true');
        Lines.Add('');
        Lines.Add('  <Include File="$(ParamFileName).local" />');
        Lines.Add('');
        Lines.Add('Macro-Tag attributes (value):');
        Lines.Add('  Name:        Name of the macro that is used in $(name)');
        Lines.Add('  [Condition]: Set macro only if the condition evaluates to true');
        Lines.Add('');
        Lines.Add('  <Macro Name="MacroName" Condition=" ''$(MacroName)'' == '''' ">Default</Macro>');
        Lines.Add('');
        Lines.Add('Macro-Tag attributes (load from file):');
        Lines.Add('  Name:        Name of the macro that is used in $(name)');
        Lines.Add('  FromFile:    File that contains the macro value');
        Lines.Add('  [Line]:      Line number of the line that should be the macro value (1..LineCount).');
        Lines.Add('               A line number less than 1 means the whole file is the value');
        Lines.Add('  [RegEx]:     Regular expression that is applied to the content of the file or line.');
        Lines.Add('  [Condition]: Set macro only if the condition evaluates to true');
        Lines.Add('');
        Lines.Add('  <Macro Name="PASSWORD" FromFile="$(ParamFilePath)\Passwords.txt" Line="4" RegEx="&quot;[A-Z]*;quot;"/>');
        Lines.Add('');
        Lines.Add('-->');
        Lines.Add('');
        Lines.Add('');
      end;
      Lines.Add('  <Macro Name="DB">MyDB</Macro>');
      Lines.Add('  <Macro Name="PASSWORD" FromFile="C:\Somewhere\Password.txt" Line="4" RegEx="&quot;[A-Z]*;quot;"/>');
      Lines.Add('');
      Lines.Add('  <!--<Param Name="User@MyDB">-d $(DB) -u admin -p $(PASSWORD)</Param>-->');
      if not LocalFile then
      begin
        Lines.Add('');
        Lines.Add('  <Include File="$(ParamFileName).local" />');
      end;
      Lines.Add('</StartParameters>');

      Lines.SaveToFile(FileName, TEncoding.UTF8);
    finally
      Lines.Free;
    end;
  end;

  (BorlandIDEServices as IOTAActionServices).OpenFile(FileName)
end;

procedure TStartParameterControl.DoPopup(Sender: TObject);
var
  ProjectBaseDir: string;

  procedure AddFiles(const FileName: string; List: TStartParamList);
  var
    MenuItem: TFileMenuItem;
    I: Integer;
  begin
    MenuItem := TFileMenuItem.Create(PopupMenu);
    if FileExists(FileName) then
      MenuItem.Caption := Format('Edit %s', [ExtractRelativePath(ProjectBaseDir, FileName)])
    else
      MenuItem.Caption := Format('Create %s', [ExtractRelativePath(ProjectBaseDir, FileName)]);
    MenuItem.FileName := FileName;
    MenuItem.OnClick := DoEditFileClick;
    PopupMenu.Items.Add(MenuItem);

    for I := 0 to List.Count - 1 do
      if List[I] is TStartParamInclude then
        AddFiles(TStartParamInclude(List[I]).ResolvedFileName, TStartParamInclude(List[I]).StartParamList);
  end;

var
  FileName: string;
  MenuItem: TMenuItem;
begin
  PopupMenu.Items.Clear;
  Reload;
  if (FActiveParams <> nil) and (FActiveParams.Project <> nil) then
  begin
    ProjectBaseDir := FActiveParams.Project.FileName;
    FileName := TProjectParameters.GetParamFileName(GetActiveProject.FileName, False);
    AddFiles(FileName, FActiveParams.Parameters);
  end
  else
  begin
    MenuItem := TMenuItem.Create(PopupMenu);
    MenuItem.Caption := 'No active project';
    MenuItem.Enabled := False;
    PopupMenu.Items.Add(MenuItem);
  end;
end;

procedure TStartParameterControl.DropDown;
var
  MinWidth, W: Integer;
  I: Integer;
begin
  Reload;
  MinWidth := Width;
  for I := 0 to Items.Count - 1 do
  begin
    W := Canvas.TextWidth(Items[I]) + 8;
    if MinWidth < W then
      MinWidth := W;
  end;
  SendMessage(Handle, CB_SETDROPPEDWIDTH, MinWidth, 0);
  inherited DropDown;
end;

procedure TStartParameterControl.Modified;
begin
end;

procedure TStartParameterControl.Reload;
begin
  if FActiveParams <> nil then
  begin
    if FActiveParams.Parameters.FileName <> '' then
      FActiveParams.Parameters.Reload
    else
      FActiveParams.Load;
  end;
end;

procedure TStartParameterControl.AfterSave;
begin
end;

procedure TStartParameterControl.BeforeSave;
begin
end;

procedure TStartParameterControl.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TStartParameterControl.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TStartParameterControl.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  case NotifyCode of
    ofnActiveProjectChanged:
      UpdateStartParameters(nil);
  end;
end;

function TStartParameterControl.FindProject(const AProject: IOTAProject): TProjectParameters;
var
  I: Integer;
begin
  for I := 0 to FProjectParams.Count - 1 do
  begin
    Result := TProjectParameters(FProjectParams[I]);
    if Result.Project = AProject then
      Exit;
  end;
  Result := nil;
end;

procedure TStartParameterControl.UpdateStartParameters(const ADestroyingProject: IOTAProject);
var
  Project: IOTAProject;
  Index: Integer;
  Param: TStartParam;
  DropDownWidth, W: Integer;
  {$IF CompilerVersion >= 33.0} // 10.3 Rio+
  cbPlatforms: TCustomComboBox;
  {$IFEND}
begin
  FActiveParams := nil;
  if not HandleAllocated then // *.dsk files cause this
    Exit;

  {$IF CompilerVersion >= 33.0} // 10.3 Rio+
  cbPlatforms := Owner.FindComponent('cbPlatforms') as TCustomComboBox;
  if cbPlatforms <> nil then
  begin
    Font.Assign(TComboBox(cbPlatforms).Font);
    Brush.Assign(cbPlatforms.Brush);
  end;
  {$IFEND}

  Items.BeginUpdate;
  try
    Items.Clear;
    Items.AddObject('<Default Start Parameters>', nil);
    Index := 0;
    try
      Project := GetActiveProject;
      if (Project <> nil) and (Project <> ADestroyingProject) then
      begin
        FActiveParams := FindProject(Project);
        if FActiveParams <> nil then
          Reload
        else
        begin
          FActiveParams := TProjectParameters.Create(Self, Project);
          FActiveParams.Load;
        end;

        for Param in FActiveParams.Parameters.AvailableParams do
          Items.AddObject(Param.Name, Param);

        Index := Items.IndexOf(FActiveParams.LocalParameters.ActiveParamName);
        if Index = -1 then
          Index := 0;
      end;
    except
      Application.HandleException(Self);
    end;
  finally
    Items.EndUpdate;
  end;
  ItemIndex := Index;

  DropDownWidth := Width;
  Canvas.Font.Assign(Font);
  for Index := 0 to Items.Count - 1 do
  begin
    W := Canvas.TextWidth(Items[Index]) + 6;
    if W > DropDownWidth then
      DropDownWidth := W;
  end;
  SendMessage(Handle, CB_SETDROPPEDWIDTH, DropDownWidth, 0);
end;

{$IF CompilerVersion >= 33.0} // 10.3 Rio+
procedure TStartParameterControl.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  cbPlatforms: TCustomComboBox;
begin
  TControlCanvas(Canvas).UpdateTextFlags;
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
  begin
    cbPlatforms := Owner.FindComponent('cbPlatforms') as TCustomComboBox;
    if cbPlatforms <> nil then
    begin
      Font.Assign(TComboBox(cbPlatforms).Font);
      Brush.Assign(cbPlatforms.Brush);
    end;
    Canvas.FillRect(Rect);
    if Index >= 0 then
      if odComboBoxEdit in State then
        Canvas.TextOut(Rect.Left + 1, Rect.Top + 1, Items[Index]) //Visual state of the text in the edit control
      else
        Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]); //Visual state of the text(items) in the deployed list
  end;
end;
{$IFEND}

procedure TStartParameterControl.WMCopy(var Msg: TWMCopy);
var
  S: string;
begin
  if TStartParameterControl.GetActiveParams(S, False) then
    Clipboard.AsText := S
  else
    inherited;
end;

procedure TStartParameterControl.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  try
    UpdateStartParameters(nil);
  except
  end;
end;

procedure TStartParameterControl.CMHintShow(var Msg: TCMHintShow);
var
  S: string;
begin
  if TStartParameterControl.GetActiveParams(S, False) then
  begin
    Msg.HintInfo.HintStr := S;
    Msg.Result := 0
  end
  else
  begin
    Msg.HintInfo.HintStr := '';
    Msg.Result := 1;
  end;
end;

end.
