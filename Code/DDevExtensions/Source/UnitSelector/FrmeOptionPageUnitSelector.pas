{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2011 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageUnitSelector;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SimpleXmlIntf, SimpleXmlImport, FrmTreePages,
  PluginConfig, FrmeBase, ExtCtrls, ComCtrls, Menus, ToolsAPI;

type
  TUnitSelectorConfig = class(TPluginConfig)
  private
    {$IF CompilerVersion < 21.0} // Delphi 2009
    FActive: Boolean;
    {$IFEND}
    FFindUseUnitHotKey: TShortCut;
    FReplaceUseUnit: Boolean;
    {$IF CompilerVersion < 21.0} // Delphi 2009
    procedure SetActive(const Value: Boolean);
    {$IFEND}
    procedure SetFindUseUnitHotKey(const Value: TShortCut);
    procedure SetReplaceUseUnit(const Value: Boolean);
  protected
    function GetOptionPages: TTreePage; override;
    procedure Init; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    {$IF CompilerVersion < 21.0} // Delphi 2009
    property Active: Boolean read FActive write SetActive;
    {$IFEND}
    property ReplaceUseUnit: Boolean read FReplaceUseUnit write SetReplaceUseUnit;
    property FindUseUnitHotKey: TShortCut read FFindUseUnitHotKey write SetFindUseUnitHotKey;
  end;

  TFrameOptionPageUnitSelector = class(TFrameBase, ITreePageComponent)
    cbxUseUnitSelector: TCheckBox;
    hkFindUseUnit: THotKey;
    lblFindUseUnitHotKey: TLabel;
    chkReplaceUseUnit: TCheckBox;
  private
    { Private declarations }
    FUnitSelectorConfig: TUnitSelectorConfig;
  public
    procedure SetUserData(UserData: TObject);
    procedure LoadData;
    procedure SaveData;
    procedure Selected;
    procedure Unselected;
    { Public declarations }
  end;

var
  UnitSelectorConfig: TUnitSelectorConfig;

procedure InitPlugin(Unload: Boolean);

implementation

uses
  {$IF CompilerVersion < 21.0} // Delphi 2009
  FrmUnitSelector,
  {$IFEND}
  Hooking, IDEHooks, IDEUtils, Main, IDEMenuHandler,
  FrmFileSelector, ToolsAPIHelpers;

{$R *.dfm}

const
  DelphicmdsDll = delphicoreide_bpl;

var
  HookTDelphiCommands_FileUseUnitCommandExecute: TRedirectCode;
  TDelphiCommands_FileUseUnitCommandExecute: procedure(Self: TObject; Sender: TObject) = nil;

{$IF CompilerVersion < 21.0} // Delphi 2009
var
  HookTViewDialog_Execute: TRedirectCode;
//  HookTViewDialog_Create: TRedirectCode;
//  HookTViewDialog_FormCreate: TRedirectCode;
  OrgTViewDialog_Create: function(ViewDialog: TFormClass; DL: Byte; Owner: TComponent; GetFiles: TViewDialogGetFiles): TForm;

function TViewDialog_Execute(ViewDialog: TForm): Boolean;
  external coreide_bpl name '@Viewdlg@TViewDialog@Execute$qqrv';

function TViewDialog_Create(ViewDialog: TFormClass; DL: Byte; Owner: TComponent; GetFiles: TViewDialogGetFiles): TForm;
  external coreide_bpl name '@Viewdlg@TViewDialog@$bctr$qqrp18Classes@TComponentpqqrp28Collections@TStringHashTableo$v';

procedure TViewDialog_FormCreate(ViewDialog: TForm; Sender: TObject);
  external coreide_bpl name '@Viewdlg@TViewDialog@FormCreate$qqrp14System@TObject';

type
  TViewDialogGetFilesComponent = class(TComponent)
  public
    GetFiles: TViewDialogGetFiles;
  end;

  TFormAccess = class(TForm);

procedure ViewDialogDoCreate(ViewDialog: TForm);
var
  FS: ^TFormState;
begin
  FS := @ViewDialog.FormState;
  if fsActivated in FS^ then
  begin
    TFormAccess(ViewDialog).Activate;
    Exclude(FS^, fsActivated);
  end;
end;

function Hooked_TViewDialog_Create(AClass: TFormClass; DL: Byte; Owner: TComponent; GetFiles: TViewDialogGetFiles): TForm;
var
  C: TViewDialogGetFilesComponent;
begin
  // disable the OnFormCreate call as we don't need it
  ReplaceVmtField(AClass, GetActualAddr(@TFormAccess.DoCreate), @ViewDialogDoCreate);
  Result := OrgTViewDialog_Create(AClass, DL, Owner, GetFiles);
  ReplaceVmtField(AClass, @ViewDialogDoCreate, GetActualAddr(@TFormAccess.DoCreate));

  C := TViewDialogGetFilesComponent.Create(Result);
  C.GetFiles := GetFiles;
  C.Name := '__GetFiles';
end;


function Hooked_TViewDialog_Execute(ViewDialog: TForm): Boolean;
var
  Form: TFormUnitSelector;
begin
  try
    Form := TFormUnitSelector.Create(nil);
    try
      Result := Form.Execute(ViewDialog, (ViewDialog.FindComponent('__GetFiles') as TViewDialogGetFilesComponent).GetFiles);
    finally
      Form.Free;
    end;
  except
    Application.HandleException(ViewDialog);

    // failsafe code
    UnhookFunction(HookTViewDialog_Execute);
    try
      // We disabled the FormCreate code, so we must execute it now.
      TViewDialog_FormCreate(ViewDialog, ViewDialog);

      Result := TViewDialog_Execute(ViewDialog);
    finally
      CodeRedirect(@TViewDialog_Execute, @Hooked_TViewDialog_Execute, HookTViewDialog_Execute);
    end;
  end;
end;

{$IFEND}

procedure Hooked_TDelphiCommands_FileUseUnitCommandExecute(Self: TObject; Sender: TObject);
var
  Project: IOTAProject;
begin
  Project := GetActiveProject;
  if (Project <> nil) and (IsDelphiPersonality(Project) or IsDelphiNetPersonality(Project)) then
  begin
    try
      TFormFileSelector.Execute(False);
      Exit;
    except
      Application.HandleException(Self);
    end;
  end;

  // failsafe code
  UnhookFunction(HookTDelphiCommands_FileUseUnitCommandExecute);
  try
    TDelphiCommands_FileUseUnitCommandExecute(Self, Sender);
  finally
    CodeRedirect(@TDelphiCommands_FileUseUnitCommandExecute, @Hooked_TDelphiCommands_FileUseUnitCommandExecute, HookTDelphiCommands_FileUseUnitCommandExecute);
  end;
end;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    UnitSelectorConfig := TUnitSelectorConfig.Create
  else
    FreeAndNil(UnitSelectorConfig);
end;

{ TUnitSelectorConfig }

constructor TUnitSelectorConfig.Create;
begin
  inherited Create(AppDataDirectory + '\UnitSelector.xml', 'UnitSelector');
end;

destructor TUnitSelectorConfig.Destroy;
begin
  {$IF CompilerVersion < 21.0} // Delphi 2009
  Active := False;
  {$IFEND}
  ReplaceUseUnit := False;
  inherited Destroy;
end;

procedure TUnitSelectorConfig.Init;
begin
  inherited Init;
  {$IF CompilerVersion < 21.0} // Delphi 2009
  Active := True;
  {$IFEND}
  //FindUseUnitHotKey := Menus.ShortCut(Ord('U'), [ssCtrl, ssAlt]);
  FindUseUnitHotKey := scNone;
  ReplaceUseUnit := True;
end;

function TUnitSelectorConfig.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Find Unit/Use Unit', TFrameOptionPageUnitSelector, Self);
end;

{$IF CompilerVersion < 21.0} // Delphi 2009
procedure TUnitSelectorConfig.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if FActive then
    begin
      UnhookFunction(HookTViewDialog_Execute);
      RestoreOrgCall(@TViewDialog_Create, @OrgTViewDialog_Create);
    end;
    FActive := Value;
    if FActive then
    begin
      CodeRedirect(@TViewDialog_Execute, @Hooked_TViewDialog_Execute, HookTViewDialog_Execute);
      if Assigned(OrgTViewDialog_Create) then
        ReRedirectOrgCall(@TViewDialog_Create, @Hooked_TViewDialog_Create, @OrgTViewDialog_Create)
      else
        @OrgTViewDialog_Create := RedirectOrgCall(@TViewDialog_Create, @Hooked_TViewDialog_Create);
    end;
  end;
end;
{$IFEND}

procedure TUnitSelectorConfig.SetFindUseUnitHotKey(const Value: TShortCut);
begin
  if Value <> FindUseUnitHotKey then
  begin
    FFindUseUnitHotKey := Value;
    TIDEMenuHandler.SetFindUseUnitHotKey(Value);
  end;
end;

procedure TUnitSelectorConfig.SetReplaceUseUnit(const Value: Boolean);
begin
  if Value <> FReplaceUseUnit then
  begin
    if not Assigned(TDelphiCommands_FileUseUnitCommandExecute) then
      @TDelphiCommands_FileUseUnitCommandExecute := DbgStrictGetProcAddress(GetModuleHandle(PChar(DelphicmdsDll)), '@Delphicmds@TDelphiCommands@FileUseUnitCommandExecute$qqrp14System@TObject');
    if Assigned(TDelphiCommands_FileUseUnitCommandExecute) then
    begin
      if FReplaceUseUnit then
        UnhookFunction(HookTDelphiCommands_FileUseUnitCommandExecute);
      FReplaceUseUnit := Value;
      if FReplaceUseUnit then
        CodeRedirect(@TDelphiCommands_FileUseUnitCommandExecute,
          @Hooked_TDelphiCommands_FileUseUnitCommandExecute,
          HookTDelphiCommands_FileUseUnitCommandExecute);
    end;
  end;
end;

{ TFrameOptionPageUnitSelector }

procedure TFrameOptionPageUnitSelector.SetUserData(UserData: TObject);
begin
  FUnitSelectorConfig := UserData as TUnitSelectorConfig;
end;

procedure TFrameOptionPageUnitSelector.LoadData;
begin
  {$IF CompilerVersion < 21.0} // Delphi 2009
  cbxUseUnitSelector.Checked := FUnitSelectorConfig.Active;
  {$ELSE}
  cbxUseUnitSelector.Free;
  {$IFEND}
  hkFindUseUnit.HotKey := FUnitSelectorConfig.FindUseUnitHotKey;
  chkReplaceUseUnit.Checked := FUnitSelectorConfig.ReplaceUseUnit;
end;

procedure TFrameOptionPageUnitSelector.SaveData;
begin
  FUnitSelectorConfig.FindUseUnitHotKey := hkFindUseUnit.HotKey;
  FUnitSelectorConfig.ReplaceUseUnit := chkReplaceUseUnit.Checked;
  {$IF CompilerVersion < 21.0} // Delphi 2009
  FUnitSelectorConfig.Active := cbxUseUnitSelector.Checked;
  {$IFEND}
  FUnitSelectorConfig.Save;
end;

procedure TFrameOptionPageUnitSelector.Selected;
begin
end;

procedure TFrameOptionPageUnitSelector.Unselected;
begin
end;

end.

