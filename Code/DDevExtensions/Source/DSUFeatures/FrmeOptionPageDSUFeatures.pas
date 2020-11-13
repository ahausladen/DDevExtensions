{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2011 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageDSUFeatures;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ToolsAPI,
  {$IF CompilerVersion >= 23.0}
  PlatformAPI,
  {$IFEND}
  DCCStrs, FrmTreePages, PluginConfig, StdCtrls,
  ModuleData, FrmeBase, ExtCtrls, ActnList, Menus, VirtTreeHandler, ComCtrls;

type
  TEditorDblClickAction = (ecaNone, ecaZoom, ecaSuperZoom);

  TDSUFeaturesConfig = class(TPluginConfig)
  private
    FDisablePackageCache: Boolean;
    //FDisableEditorClearType: Boolean;
    FEditorDblClickAction: TEditorDblClickAction;
    FZoomModeOffset: Integer;
    {$IF CompilerVersion = 21.0} // Delphi 2010
    FIncBuildNumOnBuildOnly: Boolean;
    {$IFEND}
    {$IF CompilerVersion >= 21.0} // Delphi 2010+
    FDisableSourceFormatterHotkey: Boolean;
    FFormatSourceOrgShortCut: TShortCut;
    {$ELSE}
    FDisableCodeFolding: Boolean;
    FReplacePackageAddContain: Boolean;
    {$IFEND}
    FShowFileProjectInPrjMgr: Boolean;
    FStructureViewSearchHotKey: TShortCut;
    FReplaceOpenFileAtCursor: Boolean;
    FShowAllFrames: Boolean;
    FDontBreakOnSpawnedProcesses: Boolean;
    FKillDExplore: Boolean;
    FConfirmDlgOnDebugCtrlF1: Boolean;
    FDisableAlphaSortClassCompletion: Boolean;

    procedure SetDisablePackageCache(Value: Boolean);
    procedure SetEditorDblClickAction(Value: TEditorDblClickAction);
    {$IF CompilerVersion = 21.0} // Delphi 2010
    procedure SetIncBuildNumOnBuildOnly(const Value: Boolean);
    {$IFEND}
    {$IF CompilerVersion >= 21.0} // Delphi 2010+
    procedure SetDisableSourceFormatterHotkey(Value: Boolean);
    {$ELSE}
    procedure SetDisableCodeFolding(const Value: Boolean);
    procedure SetReplacePackageAddContain(const Value: Boolean);
    {$IFEND}
    procedure SetShowFileProjectInPrjMgr(const Value: Boolean);
    procedure SetStructureViewSearchHotKey(const Value: TShortCut);
    procedure SetReplaceOpenFileAtCursor(const Value: Boolean);
    procedure SetShowAllFrames(const Value: Boolean);
    procedure SetDontBreakOnSpawnedProcesses(const Value: Boolean);
    procedure SetConfirmDlgOnDebugCtrlF1(const Value: Boolean);
    //procedure SetDisableEditorClearType(Value: Boolean);
    procedure SetDisableAlphaSortClassCompletion(const Value: Boolean);
  protected
    FTimerStructureView: TTimer;
    FLastParsingDots: Integer;
    FParseThread: TThread;
    FPrjMgrTree: TIDEVirtualTreeHandler;
    FOrgPMGetTextEvent: TVSTGetTextEvent;

    procedure PMGetText(Sender: TObject; Node: PVirtualNode; Column: Integer;
      TextType: TVSTTextType; var CellText: WideString);

    procedure SetRegValue(var Value: Boolean; NewValue: Boolean; const ValueName: string);
    function GetOptionPages: TTreePage; override;
    procedure Init; override;
    procedure LoadFromRegistry;
    procedure Loaded; override;

    procedure StructureViewTimer(Sender: TObject);
    function IsBackgroundParsing: Boolean;

    procedure UpdateEditorDblClickAction;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property DisablePackageCache: Boolean read FDisablePackageCache write SetDisablePackageCache;
    property ReplaceOpenFileAtCursor: Boolean read FReplaceOpenFileAtCursor write SetReplaceOpenFileAtCursor;
    property ShowAllFrames: Boolean read FShowAllFrames write SetShowAllFrames;
    {$IF CompilerVersion = 21.0} // Delphi 2010
    property IncBuildNumOnBuildOnly: Boolean read FIncBuildNumOnBuildOnly write SetIncBuildNumOnBuildOnly;
    {$IFEND}
    {$IF CompilerVersion >= 21.0} // Delphi 2010+
    property DisableSourceFormatterHotkey: Boolean read FDisableSourceFormatterHotkey write SetDisableSourceFormatterHotkey;
    {$ELSE}
    property DisableCodeFolding: Boolean read FDisableCodeFolding write SetDisableCodeFolding;
    property ReplacePackageAddContain: Boolean read FReplacePackageAddContain write SetReplacePackageAddContain;
    {$IFEND}
    property ShowFileProjectInPrjMgr: Boolean read FShowFileProjectInPrjMgr write SetShowFileProjectInPrjMgr;
    property EditorDblClickAction: TEditorDblClickAction read FEditorDblClickAction write SetEditorDblClickAction;
    //property DisableEditorClearType: Boolean read FDisableEditorClearType write SetDisableEditorClearType;
    property StructureViewSearchHotKey: TShortCut read FStructureViewSearchHotKey write SetStructureViewSearchHotKey;
    property DontBreakOnSpawnedProcesses: Boolean read FDontBreakOnSpawnedProcesses write SetDontBreakOnSpawnedProcesses;
    property KillDExplore: Boolean read FKillDExplore write FKillDExplore;
    property ConfirmDlgOnDebugCtrlF1: Boolean read FConfirmDlgOnDebugCtrlF1 write SetConfirmDlgOnDebugCtrlF1;
    property DisableAlphaSortClassCompletion: Boolean read FDisableAlphaSortClassCompletion write SetDisableAlphaSortClassCompletion;
  end;

  TFrameOptionPageDSUFeatures = class(TFrameBase, ITreePageComponent)
    chkDisablePackageCache: TCheckBox;
    cbxEditorTabDblClickAction: TComboBox;
    lblEditorTabDblClickAction: TLabel;
    chkDisableSourceFormatterHotkey: TCheckBox;
    chkShowFileProjectInPrjMgr: TCheckBox;
    HotKeyStructureViewSearch: THotKey;
    LabelStructureViewSearchHotkey: TLabel;
    chkIncBuildNumOnBuildOnly: TCheckBox;
    chkDisableCodeFolding: TCheckBox;
    chkReplaceOpenFileAtCursor: TCheckBox;
    chkShowAllFrames: TCheckBox;
    chkDontBreakOnSpawnedProcesses: TCheckBox;
    chkKillDExplore: TCheckBox;
    chkConfirmDlgOnDebugCtrlF1: TCheckBox;
    chkDisableAlphaSortClassCompletion: TCheckBox;
  private
    { Private-Deklarationen }
    FDSUFeatures: TDSUFeaturesConfig;
  public
    { Public-Deklarationen }
    procedure SetUserData(UserData: TObject);
    procedure LoadData;
    procedure SaveData;
    procedure Selected;
    procedure Unselected;
  end;

procedure InitPlugin(Unload: Boolean);

var
  DSUFeaturesConfig: TDSUFeaturesConfig;

implementation

uses
  Main, DSUFeatures, StrUtils, IDEHooks, Hooking, IDEUtils, StrucViewSearch, ToolsAPIHelpers,
  AppConsts, DisableAlphaSortClassCompletion;

{$R *.dfm}

var
  GlobalBaseRegKey: string;
  {$IF CompilerVersion < 21.0} // Delphi 2009-
  DisabledGetRegionsHook: TRedirectCode;

  ProcessAddCommandHook: TRedirectCode;
  Package_AddProjectModuleHook: TRedirectCode;
  TPascalProjectUpdater_AddProjectModule: procedure(Instance: TObject);
  TPascalPackageCodeUpdater_AddProjectModule: procedure(Instance: TObject);
  TStdPackageProjectContainer_ProcessAddCommand: procedure(Instance: TObject; Command: Integer);
  TStdProjectContainer_AddToProject: procedure(Instance: TObject);
  {$IFEND}

procedure InitPlugin(Unload: Boolean);
begin
  DSUFeatures.InitPlugin(Unload);
  if not Unload then
    DSUFeaturesConfig := TDSUFeaturesConfig.Create
  else
    FreeAndNil(DSUFeaturesConfig);
end;

{ TFrameOptionPageDSUFeatures }

procedure TFrameOptionPageDSUFeatures.SetUserData(UserData: TObject);
begin
  FDSUFeatures := UserData as TDSUFeaturesConfig;
end;

procedure TFrameOptionPageDSUFeatures.LoadData;
begin
  chkDisablePackageCache.Checked := FDSUFeatures.DisablePackageCache;
  {$IF CompilerVersion = 21.0} // Delphi 2010
  chkIncBuildNumOnBuildOnly.Checked := FDSUFeatures.IncBuildNumOnBuildOnly;
  {$ELSE}
  chkIncBuildNumOnBuildOnly.Free;
  {$IFEND}

  {$IF CompilerVersion >= 21.0} // Delphi 2010+
  chkDisableSourceFormatterHotkey.Checked := FDSUFeatures.DisableSourceFormatterHotkey;

  chkDisableCodeFolding.Free;
  {$ELSE}
  chkDisableCodeFolding.Top := chkDisableSourceFormatterHotkey.Top;
  chkDisableCodeFolding.Checked := FDSUFeatures.DisableCodeFolding;

  chkDisableSourceFormatterHotkey.Free;
  chkIncBuildNumOnBuildOnly.Free;
  {$IFEND}
  chkShowFileProjectInPrjMgr.Checked := FDSUFeatures.ShowFileProjectInPrjMgr;
  cbxEditorTabDblClickAction.ItemIndex := Ord(FDSUFeatures.EditorDblClickAction);
  HotKeyStructureViewSearch.HotKey := FDSUFeatures.StructureViewSearchHotKey;
  chkReplaceOpenFileAtCursor.Checked := FDSUFeatures.ReplaceOpenFileAtCursor;
  chkShowAllFrames.Checked := FDSUFeatures.ShowAllFrames;
  chkDontBreakOnSpawnedProcesses.Checked := FDSUFeatures.DontBreakOnSpawnedProcesses;
  chkKillDExplore.Checked := FDSUFeatures.KillDExplore;
  chkConfirmDlgOnDebugCtrlF1.Checked := FDSUFeatures.ConfirmDlgOnDebugCtrlF1;
  chkDisableAlphaSortClassCompletion.Checked := FDSUFeatures.DisableAlphaSortClassCompletion;
end;

procedure TFrameOptionPageDSUFeatures.SaveData;
begin
  FDSUFeatures.DisablePackageCache := chkDisablePackageCache.Checked;
  {$IF CompilerVersion = 21.0} // Delphi 2010
  FDSUFeatures.IncBuildNumOnBuildOnly := chkIncBuildNumOnBuildOnly.Checked;
  {$IFEND}

  {$IF CompilerVersion >= 21.0} // Delphi 2010+
  FDSUFeatures.DisableSourceFormatterHotkey := chkDisableSourceFormatterHotkey.Checked;
  {$ELSE}
  FDSUFeatures.DisableCodeFolding := chkDisableCodeFolding.Checked;
  {$IFEND}
  FDSUFeatures.ShowFileProjectInPrjMgr := chkShowFileProjectInPrjMgr.Checked;
  FDSUFeatures.EditorDblClickAction := TEditorDblClickAction(cbxEditorTabDblClickAction.ItemIndex);
  FDSUFeatures.StructureViewSearchHotKey := HotKeyStructureViewSearch.HotKey;
  FDSUFeatures.ReplaceOpenFileAtCursor := chkReplaceOpenFileAtCursor.Checked;
  FDSUFeatures.ShowAllFrames := chkShowAllFrames.Checked;
  FDSUFeatures.DontBreakOnSpawnedProcesses := chkDontBreakOnSpawnedProcesses.Checked;
  FDSUFeatures.KillDExplore := chkKillDExplore.Checked;
  FDSUFeatures.ConfirmDlgOnDebugCtrlF1 := chkConfirmDlgOnDebugCtrlF1.Checked;
  FDSUFeatures.DisableAlphaSortClassCompletion := chkDisableAlphaSortClassCompletion.Checked;
  FDSUFeatures.Save;
end;

procedure TFrameOptionPageDSUFeatures.Selected;
begin
end;

procedure TFrameOptionPageDSUFeatures.Unselected;
begin
end;

{ TDSUFeaturesConfig }

type
  TOpenControl = class(TControl);

procedure OrgLoadRuntimeDesktop(Instance: TObject);
  external coreide_bpl name '@Desktop@TDesktopStates@LoadRuntimeDesktop$qqrv';

var
  HookDesktopLoadRuntimeDesktop: TRedirectCode;

procedure UndoEditorZoom;
var
  I, Index: Integer;
  EditWindow: TCustomForm;
  EditorDockPanel: TPanel;
  FoundVisible: Boolean;
begin
  EditWindow := FindForm('EditWindow_0', 'TEditWindow');
  if EditWindow <> nil then
  begin
    { When the IDE switches to the Debug Desktop and then back, the
      zoomed editor can't be restored anymore. The following code
      restores the zommed editor before the IDE switches to the Debug
      Desktop. }
    FoundVisible := False;
    for I := 0 to EditWindow.ControlCount - 1 do
    begin
      if EditWindow.Controls[I].ClassName = 'TEditorDockPanel' then
      begin
        EditorDockPanel := TPanel(EditWindow.Controls[I]);
        for Index := 0 to EditorDockPanel.DockClientCount - 1 do
        begin
          if EditorDockPanel.DockClients[Index].Visible then
          begin
            FoundVisible := True;
            Break;
          end;
        end;
      end;
      if FoundVisible then
        Break;
    end;

    if not FoundVisible then
      TOpenControl(EditWindow.FindComponent('TabControl')).DblClick;
  end;
end;

procedure DesktopLoadRuntimeDesktop(Instance: TObject);
begin
  UndoEditorZoom;
  CodeRestore(HookDesktopLoadRuntimeDesktop);
  try
    OrgLoadRuntimeDesktop(Instance);
  finally
    CodeRedirect(@OrgLoadRuntimeDesktop, @DesktopLoadRuntimeDesktop, HookDesktopLoadRuntimeDesktop);
  end;
end;


{
coreide140.@Editorform@TEditWindow@TabControlDblClick$qqrp14System@TObject:
2084FB10 55               push ebp
2084FB11 8BEC             mov ebp,esp
2084FB13 83C4F0           add esp,-$10
2084FB16 53               push ebx
2084FB17 56               push esi
2084FB18 57               push edi
2084FB19 8945FC           mov [ebp-$04],eax
2084FB1C 8B45FC           mov eax,[ebp-$04]
2084FB1F 80B85105000000   cmp byte ptr [eax+$00000551],$00  <<< we are interested in this variable
}

type
  PTabControlDblClickRec = ^TTabControlDblClickRec;
  TTabControlDblClickRec = packed record
    PushEbp: Byte;
    MovEbpEsp: Word;
    AddEsp_10: array[0..2] of Byte;
    PushEbx: Byte;
    PushEsi: Byte;
    PushEdi: Byte;
    MovMemEax: array[0..2] of Byte;
    MovEaxMem: array[0..2] of Byte;
    CmpMem0: packed record
               CmpEax: Word;
               Offset: Integer;
               Zero: Byte;
             end;
  end;

procedure TDSUFeaturesConfig.UpdateEditorDblClickAction;
const
  ZoomModes: array[TEditorDblClickAction] of Byte = (0, 2, 3);
var
  LibHandle: THandle;
  DblClick: PTabControlDblClickRec;
  EditWindow: TCustomForm;
begin
  if FZoomModeOffset = 0 then
  begin
    LibHandle := GetModuleHandle(coreide_bpl);
    if LibHandle <> 0 then
    begin
      DblClick := DbgStrictGetProcAddress(LibHandle, '@Editorform@TEditWindow@TabControlDblClick$qqrp14System@TObject');
      if DblClick <> nil then
      begin
        if (DblClick.PushEbp = $55) and
           (DblClick.MovEbpEsp = $EC8B) and
           (DblClick.AddEsp_10[0] = $83) and (DblClick.AddEsp_10[1] = $C4) and (DblClick.AddEsp_10[2] = $F0) and
           (DblClick.PushEbx = $53) and
           (DblClick.PushEsi = $56) and
           (DblClick.PushEdi = $57) and
           (DblClick.MovMemEax[0] = $89) and (DblClick.MovMemEax[1] = $45) and (DblClick.MovMemEax[2] = $FC) and
           (DblClick.MovEaxMem[0] = $8B) and (DblClick.MovEaxMem[1] = $45) and (DblClick.MovEaxMem[2] = $FC) and
           (DblClick.CmpMem0.CmpEax = $B880) and (DblClick.CmpMem0.Zero = $00) then
        begin
          FZoomModeOffset := DblClick.CmpMem0.Offset;
        end;
      end;
    end;
  end;

  { Only for the main editor window }
  if FZoomModeOffset <> 0 then
  begin
    EditWindow := FindForm('EditWindow_0', 'TEditWindow');
    if EditWindow <> nil then
      PByte(PByte(EditWindow) + FZoomModeOffset)^ := ZoomModes[FEditorDblClickAction];
  end;
end;

{$IF CompilerVersion = 21.0} // Delphi 2010
{
org
8B45F3           mov eax,[ebp-$0d]
803800           cmp byte ptr [eax],$00
740F             jz +$0F
8A45FB           mov al,[ebp-$05]
*2C02             sub al,$02
*7308             jnb +$08
8B45FC           mov eax,[ebp-$04]
E8C2FBFFFF       call $05a1e078

803E00           cmp byte ptr [esi],$00
*740C             jz $2190f0de
*80EB02           sub bl,$02
7307             jnb $2190f0de
8BC7             mov eax,edi
E86AD4FFFF       call $2190c548
33C0             xor eax,eax

patched:

8B45F3           mov eax,[ebp-$0d]
803800           cmp byte ptr [eax],$00
740F             jz +$0F
8A45FB           mov al,[ebp-$05]
*2C01             sub al,$01
*7508             jnz +$08
8B45FC           mov eax,[ebp-$04]
E8C2FBFFFF       call $05a1e078
}

const
  PascalProjectUpdaterBytesIdx = 7;
  PascalProjectUpdaterBytes: array[0..12] of SmallInt = (
    $80, $3F, $00,           // cmp byte ptr [edi],$00
    $74, $0C,                // jz +$0c
    $80, $EB, -1,            // sub bl,$02   => sub bl,1
    -1, $07,                 // jnb +$07     => jnz +$07
    $8B, $C6,                // mov eax,esi
    $E8                      // call
  );

  PascalPackageCodeUpdaterBytesIdx = 7;
  PascalPackageCodeUpdaterBytes: array[0..18] of SmallInt = (
    $80, $3E, $00,           // cmp byte ptr [esi],$00
    $74, $0C,                // jz $+0c
    $80, $EB, -1,            // sub bl,$02   => sub bl,$01
    -1, $07,                 // jnb +$07     => jnz $+07
    $8B, $C7,                // mov eax,edi
    $E8, -1, -1, -1, -1,     // call
    $33, $C0                 // xor eax,eax
  );

  CppProjectUpdaterBytesIdx = 12;
  CppProjectUpdaterBytes: array[0..18] of SmallInt = (
    $8B, $45, $F3,           // mov eax,[ebp-$0d]
    $80, $38, $00,           // cmp byte ptr [eax],$00
    $74, $0F,                // jz +$0f
    $8A, $45, $FB,           // mov al,[ebp-$05]
    $2C, -1,                 // sub al,$02    => sub al,$01
    -1, $08,                 // jnb +$08      => jnz +$08
    $8B, $45, $FC,           // mov eax,[ebp-$04]
    $E8                      // call
  );

  CppPackageProjectUpdaterBytesIdx = 12;
  CppPackageProjectUpdaterBytes: array[0..18] of SmallInt = (
    $8B, $45, $F3,           // mov eax,[ebp-$0d]
    $80, $38, $00,           // cmp byte ptr [eax],$00
    $74, $0F,                // jz +$0f
    $8A, $45, $FB,           // mov al,[ebp-$05]
    $2C, -1,                 // sub al,$02    => sub al,$01
    -1, $08,                 // jnb +$08      => jnz +$08
    $8B, $45, $FC,           // mov eax,[ebp-$04]
    $E8                      // call
  );

procedure TDSUFeaturesConfig.SetIncBuildNumOnBuildOnly(const Value: Boolean);

  procedure Patch(ModuleName: PChar; ProcName: PAnsiChar; EnablePatch: Boolean;
    const Bytes: array of SmallInt; PatchIndex: Integer);
  type
    PPatchArray = ^TPatchArray;
    TPatchArray = array[0..1] of Byte;
  const
    EnableValue: TPatchArray = ($01, $75);
    DisableValue: TPatchArray = ($02, $73);
  var
    Value: PPatchArray;
    Proc: Pointer;
    P: PByte;
    n: SIZE_T;
  begin
    if EnablePatch then
      Value := @EnableValue
    else
      Value := @Disablevalue;

    Proc := DbgStrictGetProcAddress(GetModuleHandle(ModuleName), ProcName);
    if Proc <> nil then
    begin
      P := FindMethodPtr(Cardinal(Proc), Bytes, 256);
      if P <> nil then
        WriteProcessMemory(GetCurrentProcess, @P[PatchIndex], Value, SizeOf(TPatchArray), n);

      {if P = nil then
      asm
        call Proc
      end;}
    end;
  end;

begin
  if Value <> IncBuildNumOnBuildOnly then
  begin
    FIncBuildNumOnBuildOnly := Value;

    Patch(delphicoreide_bpl, '@Pasmgr@TPascalPackageCodeUpdater@AfterCompile$qqr21Compintf@TCompileModeroo',
      FIncBuildNumOnBuildOnly, PascalPackageCodeUpdaterBytes, PascalPackageCodeUpdaterBytesIdx);
    Patch(delphicoreide_bpl, '@Pasmgr@TPascalProjectUpdater@AfterCompile$qqr21Compintf@TCompileModeroo',
      FIncBuildNumOnBuildOnly, PascalProjectUpdaterBytes, PascalProjectUpdaterBytesIdx);

    if GetModuleHandle(bcbide_bpl) <> 0 then
    begin
      Patch(bcbide_bpl, '@Cppmgr@TCppProjectUpdater@AfterCompile$qqr21Compintf@TCompileModeroo',
        FIncBuildNumOnBuildOnly, CppProjectUpdaterBytes, CppProjectUpdaterBytesIdx);
      Patch(bcbide_bpl, '@Cppmgr@TCppPackageProjectUpdater@AfterCompile$qqr21Compintf@TCompileModeroo',
        FIncBuildNumOnBuildOnly, CppPackageProjectUpdaterBytes, CppPackageProjectUpdaterBytesIdx);
    end;
  end;
end;
{$IFEND}

constructor TDSUFeaturesConfig.Create;
begin
  if BorlandIDEServices <> nil then
  begin
    GlobalBaseRegKey := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Globals';
    if GlobalBaseRegKey[1] = '\' then
      GlobalBaseRegKey := Copy(GlobalBaseRegKey, 2, MaxInt);
  end;
  FTimerStructureView := TTimer.Create(nil);
  FTimerStructureView.Interval := 800;
  FTimerStructureView.OnTimer := StructureViewTimer;
  FTimerStructureView.Enabled := True;

  inherited Create(AppDataDirectory + '\DSUFeatures.xml', 'DSUFeatures');
end;

destructor TDSUFeaturesConfig.Destroy;
begin
  {$IF CompilerVersion < 21.0} // Delphi 2009
  UnhookFunction(DisabledGetRegionsHook);
  CodeRestore(ProcessAddCommandHook);
  CodeRestore(Package_AddProjectModuleHook);
  {$IFEND}
  FTimerStructureView.Free;
  {$IF CompilerVersion <= 28.0} // XE7-, XE8 replaced DExplorer with *.chm files
  if KillDExplore then
    KillAllDExplore;
  {$IFEND}
  inherited Destroy;
end;

procedure TDSUFeaturesConfig.Init;
begin
  inherited Init;
  ShowFileProjectInPrjMgr := True;
  EditorDblClickAction := ecaZoom;

  {$IF CompilerVersion = 21.0} // Delphi 2010
  IncBuildNumOnBuildOnly := True;
  {$IFEND}

  {$IF CompilerVersion >= 21.0} // Delphi 2010+
  {$ELSE}
  FDisableCodeFolding := False;
  ReplacePackageAddContain := True;
  {$IFEND}

  FKillDExplore := True;
  ConfirmDlgOnDebugCtrlF1 := True;

  LoadFromRegistry;
end;

procedure TDSUFeaturesConfig.Loaded;
begin
  inherited Loaded;
  LoadFromRegistry; // overwrite the XML settings
end;

function TDSUFeaturesConfig.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Extended IDE Settings', TFrameOptionPageDSUFeatures, Self);
end;

procedure TDSUFeaturesConfig.LoadFromRegistry;
begin
  if GlobalBaseRegKey <> '' then
  begin
    FDisablePackageCache := RegReadBoolDef(HKEY_CURRENT_USER, GlobalBaseRegKey, 'DDevExDisablePackageCache', False);
    //FDisableEditorClearType := RegReadBoolDef(HKEY_CURRENT_USER, GlobalBaseRegKey, 'DDevExDisableEditorClearType', False);
  end;
end;

procedure TDSUFeaturesConfig.SetRegValue(var Value: Boolean; NewValue: Boolean; const ValueName: string);
begin
  if NewValue <> Value then
  begin
    Value := NewValue;
    if not Loading and (GlobalBaseRegKey <> '') then
    begin
      if Value then
        RegWriteString(HKEY_CURRENT_USER, GlobalBaseRegKey, ValueName, '1')
      else
        RegDeleteEntry(HKEY_CURRENT_USER, GlobalBaseRegKey, ValueName);
    end;
  end;
end;

procedure TDSUFeaturesConfig.SetDisablePackageCache(Value: Boolean);
begin
  SetRegValue(FDisablePackageCache, Value, 'DDevExDisablePackageCache');
end;


var
  TProcess_stopOnFirstAddrHook: TRedirectCode;

const
  {$IF CompilerVersion >= 28.0} // XE7+
  _IDbkThread_ = '41System@%DelphiInterface$14Dbk@IDbkThread%'; // XE7+
  {$ELSE}
  _IDbkThread_ = '42System@%DelphiInterface$t14Dbk@IDbkThread%'; // 2007-XE6
  {$IFEND}

function TProcess_stopOnFirstAddr(Process: TObject; Addr: Pointer; const Intf: IInterface; var ShouldStop: LongWord): HRESULT; stdcall;
  external dbkdebugide_bpl name '@Debug@TProcess@stopOnFirstAddr$qqs' + _xp_ + '17Dbk@DbkProcAddr_tx' + _IDbkThread_ + 'rui';

function DbgStopOnFirstAddr(Process: TObject; Addr: Pointer; const Intf: IInterface; var ShouldStop: LongWord): HRESULT; stdcall;
begin
  if (BorlandIDEServices as IOTADebuggerServices).ProcessCount <= 1 then
    ShouldStop := 1
  else
    ShouldStop := 0;
  Result := S_OK;
end;

procedure TDSUFeaturesConfig.SetDontBreakOnSpawnedProcesses(const Value: Boolean);
begin
  if Value <> FDontBreakOnSpawnedProcesses then
  begin
    if FDontBreakOnSpawnedProcesses then
      CodeRestore(TProcess_stopOnFirstAddrHook);
    FDontBreakOnSpawnedProcesses := Value;
    if FDontBreakOnSpawnedProcesses then
      CodeRedirect(@TProcess_stopOnFirstAddr, @DbgStopOnFirstAddr, TProcess_stopOnFirstAddrHook);
  end;
end;

{$IF CompilerVersion >= 21.0} // Delphi 2010+
procedure TDSUFeaturesConfig.SetDisableSourceFormatterHotkey(Value: Boolean);
var
  TogetherCommands: TComponent;
  actnFormatSource: TAction;
  MenuItem: TMenuItem;
begin
  if Value <> FDisableSourceFormatterHotkey then
  begin
    FDisableSourceFormatterHotkey := Value;

    TogetherCommands := Application.FindComponent('TogetherCommands');
    if TogetherCommands <> nil then
    begin
      actnFormatSource := TAction(TogetherCommands.FindComponent('actnFormatSource'));
      if actnFormatSource <> nil then
      begin
        if FFormatSourceOrgShortCut = 0 then
          FFormatSourceOrgShortCut := actnFormatSource.ShortCut;

        if FDisableSourceFormatterHotkey then
          actnFormatSource.ShortCut := 0
        else
          actnFormatSource.ShortCut := FFormatSourceOrgShortCut;

        MenuItem := TMenuItem(Application.MainForm.FindComponent('mnuFormatSource'));
        if (MenuItem <> nil) and (MenuItem.Action <> nil) then
          TAction(MenuItem.Action).ShortCut := actnFormatSource.ShortCut;
      end;
    end;
  end;
end;
{$IFEND}

{$IF CompilerVersion < 21.0} // Delphi 2009
function DisabledGetRegions(Instance: TObject): TOTARegions;
begin
  Result := nil;
end;

procedure TDSUFeaturesConfig.SetDisableCodeFolding(const Value: Boolean);
{ In Delphi 2009 Code Folding is done in the main thread and it is very
  slow (much improved in Delphi 2010). }
const
  SGetRegions = '@Pasmgr@TUnitManager@GetRegions$qqrv';
begin
  if Value <> FDisableCodeFolding then
  begin
    FDisableCodeFolding := Value;
    if FDisableCodeFolding then
      HookFunction(delphicoreide_bpl, SGetRegions, @DisabledGetRegions, DisabledGetRegionsHook)
    else
      UnhookFunction(DisabledGetRegionsHook);
  end;
end;

procedure HookedProcessAddCommand(Instance: TObject; Command: Integer);
begin
  CodeRestore(ProcessAddCommandHook);
  try
    if Command = 4 then
      TStdProjectContainer_AddToProject(Instance)
    else
      TStdPackageProjectContainer_ProcessAddCommand(Instance, Command);
  finally
    CodeRedirect(@TStdPackageProjectContainer_ProcessAddCommand, @HookedProcessAddCommand, ProcessAddCommandHook);
  end;
end;

procedure HookedPackage_AddProjectModule(Instance: TObject);
begin
  TPascalProjectUpdater_AddProjectModule(Instance);
end;

procedure TDSUFeaturesConfig.SetReplacePackageAddContain(const Value: Boolean);
const
  sAddProjectModule = '@Pasmgr@TPascalProjectUpdater@AddProjectModule$qqrv';
  sPackage_AddProjectModule = '@Pasmgr@TPascalPackageCodeUpdater@AddProjectModule$qqrv';

  sProcessAddCommand = '@Pkgcontainers@TStdPackageProjectContainer@ProcessAddCommand$qqrx27Containerintf@TLocalCommand';
  sAddToProject = '@Containers@TStdProjectContainer@AddToProject$qqrv';
var
  Lib: THandle;
begin
  if Value <> FReplacePackageAddContain then
  begin
    FReplacePackageAddContain := Value;

    if not Assigned(TStdPackageProjectContainer_ProcessAddCommand) then
    begin
      Lib := GetModuleHandle(delphicoreide_bpl);
      if Lib <> 0 then
      begin
        TStdPackageProjectContainer_ProcessAddCommand := DbgStrictGetProcAddress(Lib, PAnsiChar(sProcessAddCommand));
        TPascalProjectUpdater_AddProjectModule := DbgStrictGetProcAddress(Lib, PAnsiChar(sAddProjectModule));
        TPascalPackageCodeUpdater_AddProjectModule := DbgStrictGetProcAddress(Lib, PAnsiChar(sPackage_AddProjectModule));
      end;
    end;
    if not Assigned(TStdProjectContainer_AddToProject) then
    begin
      Lib := GetModuleHandle(coreide_bpl);
      if Lib <> 0 then
        TStdProjectContainer_AddToProject := DbgStrictGetProcAddress(Lib, PAnsiChar(sAddToProject));
    end;

    if Assigned(TStdPackageProjectContainer_ProcessAddCommand) and Assigned(TStdProjectContainer_AddToProject) and
       Assigned(TPascalPackageCodeUpdater_AddProjectModule) and Assigned(TPascalProjectUpdater_AddProjectModule) then
    begin
      if FReplacePackageAddContain then
      begin
        CodeRedirect(@TStdPackageProjectContainer_ProcessAddCommand, @HookedProcessAddCommand, ProcessAddCommandHook);
        CodeRedirect(@TPascalPackageCodeUpdater_AddProjectModule, @HookedPackage_AddProjectModule, Package_AddProjectModuleHook);
      end
      else
      begin
        CodeRestore(ProcessAddCommandHook);
        CodeRestore(Package_AddProjectModuleHook);
      end;
    end;
  end;
end;
{$IFEND}

type
  IBorlandIDE = interface
    ['{94E8CFB7-D95E-11D1-AB02-00C04FB16FB3}']
    function CreateProjectGroup(const AProjectGroup, AFileSystem: string; Existing: Boolean): Boolean;
    function OpenFile(const AFileName: string; MustExist, Show: Boolean): TObject;
    function Compile(CompileMode: {TCompileMode}Integer; Wait: Boolean): Boolean;
    procedure FileOpenDialog(const AFileName: string);
    function CloseProjectGroup: Boolean;
    function TerminateDebugging: Boolean;
    {...}
  end;

var
  OrgCallOpenModuleFile: procedure(const ModuleName, EditorFileName: string);

procedure OpenModuleFile(const ModuleName, EditorFileName: string);
  external delphicoreide_bpl name '@Commonpasreg@OpenModuleFile$qqrx20System@UnicodeStringt1';

{$IF CompilerVersion >= 22.0} // Delphi XE+
function ExpandRootMacro(const InString: string; const AdditionalVars: TObject = nil): string;
  external coreide_bpl name '@Uiutils@ExpandRootMacro$qqrx20System@UnicodeString' + _xp_ + '22Codemgr@TNameValueHash';
{$ELSE}
function ExpandRootMacro(const Name: string): string;
  external coreide_bpl name '@Uiutils@ExpandRootMacro$qqrx20System@UnicodeString';
{$IFEND}

procedure VarBorlandIDE;
  external coreide_bpl name '@Ideintf@BorlandIDE';

procedure HookedOpenModuleFile(const ModuleName, EditorFileName: string);
const
  InvalidNames: array[0..9] of string = (
    'LPT1', 'LPT2', 'LPT3', 'COM1', 'COM2', 'COM3', 'COM4', 'CON', 'AUX', 'PRN'
  );

  cWin32Platform = 'Win32';
  cWinNX32Platform = 'WinNX32';
  cOSX32Platform = 'OSX32';
  cWin64Platform = 'Win64';
  cLinux32Platform = 'Linux32';
  cAndroidPlatform = 'Android';
  ciOSSimulatorPlatform = 'iOSSimulator';
  ciOSDevicePlatform = 'iOSDevice';

  function FindEditorFileProject(const FileName: string): IOTAProject;
  var
    ProjectIndex, I: Integer;
    Group: IOTAProjectGroup;
  begin
    Result := GetActiveProject();
    if Result <> nil then
      if Result.FindModuleInfo(FileName) <> nil then
        Exit;

    Group := GetActiveProjectGroup();
    if Group <> nil then
    begin
      for ProjectIndex := 0 to Group.ProjectCount - 1 do
      begin
        Result := Group.Projects[ProjectIndex];
        if AnsiSameText(FileName, Result.FileName) or (Result.FindModuleInfo(FileName) <> nil) then
          Exit;
        for I := 0 to Result.ModuleFileCount - 1 do
          if AnsiSameText(FileName, Result.ModuleFileEditors[I].FileName) then
            Exit;
      end;
    end;
    Result := GetActiveProject();
  end;

  function FindProjectModuleInfo(const Project: IOTAProject; const ModuleFileName: string): IOTAModuleInfo;
  var
    I: Integer;
    FileName: string;
  begin
    for I := 0 to Project.GetModuleCount - 1 do
    begin
      Result := Project.GetModule(I);
      FileName := ExtractFileName(Result.FileName);
      if FileName <> '' then
      begin
        if AnsiSameText(FileName, ModuleFileName) or
           AnsiSameText(FileName, ModuleFileName + ExtractFileExt(FileName)) or
           (AnsiSameText(ChangeFileExt(FileName, '.dfm'), ModuleFileName)
            and FileExists(ChangeFileExt(Result.FileName, '.dfm'))) then
        begin
          Exit;
        end;
      end;
    end;
    for I := 0 to Project.ModuleFileCount - 1 do
    begin
      FileName := Project.ModuleFileEditors[I].FileName;
      if AnsiSameText(FileName, ModuleFileName) or
         AnsiSameText(FileName, ModuleFileName + ExtractFileExt(FileName)) then
        Exit;
    end;
    Result := nil;
  end;

  procedure AppendPath(var Path: string; const AppendPath: string);
  begin
    if Path = '' then
      Path := Trim(AppendPath)
    else
      Path := Path + ';' + Trim(AppendPath);
  end;

  procedure AppendProjectSearchDirs(var SearchPaths: string; const Project: IOTAProject);
  var
    OptionConfig: IOTAProjectOptionsConfigurations;
    BuildConfig: IOTABuildConfiguration;
    Dirs: TStrings;
    CurDir: string;
    I: Integer;
  begin
    AppendPath(SearchPaths, ExtractFileDir(Project.FileName));

    Dirs := TStringList.Create;
    try
      if Supports(Project.ProjectOptions, IOTAProjectOptionsConfigurations, OptionConfig) then
      begin
        BuildConfig := OptionConfig.ActiveConfiguration;
        if (BuildConfig = nil) and (OptionConfig.ConfigurationCount > 0) then
          BuildConfig := OptionConfig.Configurations[0];

        if BuildConfig <> nil then
          BuildConfig.GetValues(DCCStrs.sUnitSearchPath, Dirs);
      end
      else
        SplitPaths(Dirs, VarToStrDef(Project.ProjectOptions.Values['UnitDir'], ''));

      CurDir := GetCurrentDir;
      try
        SetCurrentDir(ExtractFileDir(Project.FileName));
        for I := 0 to Dirs.Count - 1 do
          AppendPath(SearchPaths, ExpandFileName(ExpandRootMacro(Dirs[I])));
      finally
        SetCurrentDir(CurDir);
      end;
    finally
      Dirs.Free;
    end;
  end;

  function FindInSearchPath(const FileName, SearchPath: string; Namespaces: TStrings): string;
  var
    I, Count: Integer;
    Name, Dir, BaseName: string;
  begin
    if Namespaces <> nil then
    begin
      Count := Namespaces.Count;
      Dir := ExtractFilePath(FileName);
      BaseName := '.' + ExtractFileName(FileName);
    end
    else
      Count := 1;

    for I := 0 to Count - 1 do
    begin
      if (I = 0) and (Namespaces = nil) then
        Name := FileName
      else
        Name := Dir + Namespaces[I] + BaseName;

      Result := FileSearch(Name, SearchPath);
      if Result <> '' then
        Break;
      Result := FileSearch(Name + '.pas', SearchPath);
      if Result <> '' then
        Break;
      Result := FileSearch(Name + '.inc', SearchPath);
      if Result <> '' then
        Break;
      Result := FileSearch(Name + '.cpp', SearchPath);
      if Result <> '' then
        Break;
      Result := FileSearch(Name + '.h', SearchPath);
      if Result <> '' then
        Break;
      Result := FileSearch(Name + '.hpp', SearchPath);
      if Result <> '' then
        Break;
    end;
  end;

var
  I, ProjectIndex: Integer;
  ModuleFileName: string;
  EditorFileProject: IOTAProject;
  Group: IOTAProjectGroup;
  ModuleInfo: IOTAModuleInfo;
  Module: IOTAModule;
  EnvOptions: IOTAEnvironmentOptions;
  SearchPath, IDESearchPath: string;
  CurDir, FoundFileName: string;
  {$IF CompilerVersion >= 23.0} // Delphi XE2+
  Namespaces: TStrings;
  PlatformName: string;
  FrameworkType: string;
  Configurations: IOTAProjectOptionsConfigurations;
  {$ELSE}
  BorlandIDE: IBorlandIDE;
  {$IFEND}
  OptionNames: TOTAOptionNameArray;
  OptionNameList: TStringList;
begin
  for I := 0 to High(InvalidNames) do
    if SameText(ChangeFileExt(ModuleName, ''), InvalidNames[I]) then
      raise Exception.Create('Cannot open device name');

  EditorFileProject := FindEditorFileProject(EditorFileName);

  ModuleFileName := ExtractFileName(ModuleName);
  if ModuleFileName = ModuleName then // No path in the ModuleName => check the projects
  begin
    // Find the module in the active project
    ModuleInfo := nil;
    if EditorFileProject <> nil then
      ModuleInfo := FindProjectModuleInfo(EditorFileProject, ModuleFileName);
    if ModuleInfo = nil then
    begin
      Group := GetActiveProjectGroup();
      if Group <> nil then
      begin
        // Find the module in the project group
        for ProjectIndex := 0 to Group.ProjectCount - 1 do
        begin
          if Group.Projects[ProjectIndex] <> EditorFileProject then
          begin
            ModuleInfo := FindProjectModuleInfo(Group.Projects[ProjectIndex], ModuleFileName);
            if ModuleInfo <> nil then
              Break;
          end;
        end;
      end;
    end;

    if ModuleInfo <> nil then
    begin
      Module := ModuleInfo.OpenModule;
      if Module <> nil then
      begin
        Module.ShowFilename(ModuleInfo.FileName);
        Exit;
      end;
    end;
  end;

  SearchPath := '';
  if EditorFileName <> '' then
    AppendPath(SearchPath, ExtractFileDir(EditorFileName));
  AppendPath(SearchPath, GetCurrentDir);

  // Try to find the file in the project's search path
  if EditorFileProject <> nil then
    AppendProjectSearchDirs(SearchPath, EditorFileProject);
  if Group <> nil then
    for ProjectIndex := 0 to Group.ProjectCount - 1 do
      if Group.Projects[ProjectIndex] <> EditorFileProject then
        AppendProjectSearchDirs(SearchPath, Group.Projects[ProjectIndex]);

  // Try to find the file in the global search paths
  EnvOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
  IDESearchPath := '';

  // Don't access EnvOptions that don't exist (in the loaded personalities). The IDE will crash.
  OptionNames := EnvOptions.GetOptionNames;
  OptionNameList := TStringList.Create;
  try
    OptionNameList.Duplicates := dupIgnore; // OptionNames are duplicated for each Platform but there is no way to access them through EnvOptions
    OptionNameList.Sorted := True;

    for I := 0 to High(OptionNames) do
      OptionNameList.Add(OptionNames[I].Name);
    OptionNames := nil; // release memory

    if OptionNameList.IndexOf('LibraryPath') <> -1 then
      AppendPath(IDESearchPath, GetProjectEnvOptionPaths(EditorFileProject, 'LibraryPath'));
    if OptionNameList.IndexOf('BrowsingPath') <> -1 then
      AppendPath(IDESearchPath, GetProjectEnvOptionPaths(EditorFileProject, 'BrowsingPath'));
    if OptionNameList.IndexOf('CppLibraryPath') <> -1 then
      AppendPath(IDESearchPath, GetProjectEnvOptionPaths(EditorFileProject, 'CppLibraryPath'));
    if OptionNameList.IndexOf('CppIncludePath') <> -1 then
      AppendPath(IDESearchPath, GetProjectEnvOptionPaths(EditorFileProject, 'CppIncludePath'));
    if OptionNameList.IndexOf('CppSearchPath') <> -1 then
      AppendPath(IDESearchPath, GetProjectEnvOptionPaths(EditorFileProject, 'CppSearchPath'));
    if OptionNameList.IndexOf('CppBrowsingPath') <> -1 then
      AppendPath(IDESearchPath, GetProjectEnvOptionPaths(EditorFileProject, 'CppBrowsingPath'));
  finally
    OptionNameList.Free;
  end;
  EnvOptions := nil;
  IDESearchPath := ExpandRootMacro(IDESearchPath);

  FoundFileName := '';
  AppendPath(SearchPath, IDESearchPath);
  SearchPath := SplitAndConcatList(SearchPath, ';'); // eliminates duplicates

  CurDir := GetCurrentDir;
  try
    if EditorFileProject <> nil then
      SetCurrentDir(ExtractFileDir(EditorFileProject.FileName));

    FoundFileName := FindInSearchPath(ModuleName, SearchPath, nil);
    if (FoundFileName = '') and (ModuleName <> ModuleFileName) then
      FoundFileName := FindInSearchPath(ModuleFileName, SearchPath, nil);

    {$IF CompilerVersion >= 23.0} // Delphi XE2
    if FoundFileName = '' then
    begin
      Namespaces := TStringList.Create;
      try
        if EditorFileProject <> nil then
        begin
          if Supports(EditorFileProject.ProjectOptions, IOTAProjectOptionsConfigurations, Configurations) then
            ExtractStrings([';'], [], PChar(Configurations.ActiveConfiguration.Value[DCCStrs.sNamespace]), Namespaces);

          PlatformName := EditorFileProject.CurrentPlatform;
          FrameworkType := EditorFileProject.FrameworkType;
        end
        else
        begin
          PlatformName := cWin32Platform;
          FrameworkType := sFrameworkTypeVCL;
        end;

        // RTL
        ExtractStrings([';'], [], 'System;Xml;Data;Datasnap;Web;Soap', Namespaces);
        // Windows
        if (PlatformName = cWin32Platform) or (PlatformName = cWin64Platform) then
          ExtractStrings([';'], [], 'Winapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win', Namespaces);
        // Win32
        if PlatformName = cWin32Platform then
          ExtractStrings([';'], [], 'Bde', Namespaces);
        // MacOS
        if (PlatformName = cOSX32Platform) or {(PlatformName = cOSX64Platform) or}
           (PlatformName = ciOSSimulatorPlatform) or (PlatformName = ciOSDevicePlatform) then
          ExtractStrings([';'], [], 'System.Mac;Macapi;Posix', Namespaces);

        // Framework
        if FrameworkType = sFrameworkTypeVCL then
          ExtractStrings([';'], [], 'Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell', Namespaces)
        else if FrameworkType = sFrameworkTypeFMX then
          ExtractStrings([';'], [], 'FMX', Namespaces)
        else if FrameworkType = sFrameworkTypeFMI then
          ExtractStrings([';'], [], 'FMI', Namespaces); // FreePascal version used in XE2, XE3 dropped it

        // remove duplicates
        for I := Namespaces.Count - 1 downto 0 do
          if Namespaces.IndexOf(Namespaces[I]) <> I then
            Namespaces.Delete(I);

        if Namespaces.Count > 0 then
        begin
          FoundFileName := FindInSearchPath(ModuleName, SearchPath, Namespaces);
          if (FoundFileName = '') and (ModuleName <> ModuleFileName) then
            FoundFileName := FindInSearchPath(ModuleFileName, SearchPath, Namespaces);
        end;
      finally
        Namespaces.Free;
      end;
    end;
    {$IFEND}
    if FoundFileName <> '' then
    begin
      // IOTAActionServices.OpenFile needs the full file name. Otherwise it would create the backup
      // file in the wrong folder as we change the current directory.
      FoundFileName := ExpandFileName(FoundFileName);
    end;
  finally
    if EditorFileProject <> nil then
      SetCurrentDir(CurDir);
  end;

  if FoundFileName <> '' then
    (BorlandIDEServices as IOTAActionServices).OpenFile(FoundFileName)
  else
  begin
    {$IF CompilerVersion >= 23.0} // Delphi XE2+
    OrgCallOpenModuleFile(ModuleName, EditorFileName);
    {$ELSE}
    BorlandIDE := IBorlandIDE(GetActualAddr(@VarBorlandIDE)^);
    BorlandIDE.FileOpenDialog(ModuleName);
    {$IFEND}
  end;
end;

procedure TDSUFeaturesConfig.SetReplaceOpenFileAtCursor(const Value: Boolean);
begin
  if Value <> FReplaceOpenFileAtCursor then
  begin
    FReplaceOpenFileAtCursor := Value;
    if FReplaceOpenFileAtCursor then
      @OrgCallOpenModuleFile := RedirectOrgCall(@OpenModuleFile, @HookedOpenModuleFile)
    else
      RestoreOrgCall(@OpenModuleFile, @OrgCallOpenModuleFile);
  end;
end;

{----------------------------------------------------------------------------------}

var
  TDelphiProjectModuleHandler_GetFormListHook: TRedirectCode;
  TDelphiProjectModuleHandler_GetFormList: procedure(Instance: TObject; List: TStrings);

procedure TPascalProjectUpdaterClass;
  external delphicoreide_bpl name '@Pasmgr@TPascalProjectUpdater@';
procedure TPascalProjectUpdater_GetFormList(Instance: TObject; List: TStrings);
  external delphicoreide_bpl name
    '@Pasmgr@TPascalProjectUpdater@GetFormList$qqrp' + System_Classes_TStrings;

procedure HookedTDelphiProjectModuleHandler_GetFormList(Instance: TObject; List: TStrings);
const
  //IID_Project: TGUID = '{0BB18D53-B8C5-4909-A98A-DFD5D3C64336}';
  IID_ProjectUpdater: TGUID = '{291DE60F-6EB6-4A03-B594-52488D52C5E5}';
var
  Group: IOTAProjectGroup;
  I: Integer;
  Prj, InstancePrj: IInterface;
  PrjObj: TObject;
  PascalProjectUpdaterClass: TClass;
begin
  CodeRestore(TDelphiProjectModuleHandler_GetFormListHook);
  try
    TDelphiProjectModuleHandler_GetFormList(Instance, List);

    Group := GetActiveProjectGroup;
    if (Group <> nil) and Supports(Instance, IID_ProjectUpdater, InstancePrj) then
    begin
      PascalProjectUpdaterClass := GetActualAddr(@TPascalProjectUpdaterClass);
      for I := 0 to Group.ProjectCount - 1 do
      begin
        if Group.Projects[I].ProjectType = sPackage then
        begin
          if Supports(Group.Projects[I], IID_ProjectUpdater, Prj) and (Prj <> InstancePrj) then
          begin
            PrjObj := DelphiInterfaceToObject(Prj);
            if PrjObj.InheritsFrom(PascalProjectUpdaterClass) then // protect against new IDE code
              TPascalProjectUpdater_GetFormList(PrjObj, List);
          end;
        end;
      end;
    end;
  finally
    CodeRedirect(@TDelphiProjectModuleHandler_GetFormList, @HookedTDelphiProjectModuleHandler_GetFormList, TDelphiProjectModuleHandler_GetFormListHook);
  end;
end;

procedure TDSUFeaturesConfig.SetShowAllFrames(const Value: Boolean);
const
  sGetFormList = '@Basedelphiproject@TDelphiProjectModuleHandler@GetFormList$qqrp' + System_Classes_TStrings;
begin
  if Value <> FShowAllFrames then
  begin
    FShowAllFrames := Value;
    if not Assigned(TDelphiProjectModuleHandler_GetFormList) then
      TDelphiProjectModuleHandler_GetFormList := DbgStrictGetProcAddress(GetModuleHandle(delphicoreide_bpl), PAnsiChar(sGetFormList));

    if Assigned(TDelphiProjectModuleHandler_GetFormList) then
    begin
      if FShowAllFrames then
        CodeRedirect(@TDelphiProjectModuleHandler_GetFormList, @HookedTDelphiProjectModuleHandler_GetFormList, TDelphiProjectModuleHandler_GetFormListHook)
      else
        CodeRestore(TDelphiProjectModuleHandler_GetFormListHook);
    end;
  end;
end;

{----------------------------------------------------------------------------------}

procedure TCustomEditControl_HelpKeyword(Editor: TControl);
  external coreide_bpl name '@Editorcontrol@TCustomEditControl@HelpKeyword$qqrv';

var
  OrgEditorHelpKeyword: procedure(Editor: TControl);

procedure EditorHelpKeyword(Editor: TControl);
var
  DebuggerServices: IOTADebuggerServices;
begin
  if Supports(BorlandIDEServices, IOTADebuggerServices, DebuggerServices) and
     (DebuggerServices.ProcessCount > 0) and (GetKeyState(VK_CONTROL) and $80 <> 0) then
  begin
    if MessageDlg(sDoYouWantToInvokeTheContextHelp, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;
  OrgEditorHelpKeyword(Editor);
end;

procedure TDSUFeaturesConfig.SetConfirmDlgOnDebugCtrlF1(const Value: Boolean);
begin
  if Value <> FConfirmDlgOnDebugCtrlF1 then
  begin
    FConfirmDlgOnDebugCtrlF1 := Value;

    if FConfirmDlgOnDebugCtrlF1 then
    begin
      if Assigned(OrgEditorHelpKeyword) then
        RedirectOrg(@TCustomEditControl_HelpKeyword, @EditorHelpKeyword)
      else
        @OrgEditorHelpKeyword := RedirectOrgCall(@TCustomEditControl_HelpKeyword, @EditorHelpKeyword);
    end
    else
      RestoreOrgCall(@TCustomEditControl_HelpKeyword, @OrgEditorHelpKeyword);
  end;
end;

{----------------------------------------------------------------------------------}

procedure TDSUFeaturesConfig.SetDisableAlphaSortClassCompletion(const Value: Boolean);
begin
  if Value <> FDisableAlphaSortClassCompletion then
  begin
    FDisableAlphaSortClassCompletion := Value;
    InstallDisableAlphaSortClassCompletion(FDisableAlphaSortClassCompletion);
  end;
end;

{----------------------------------------------------------------------------------}

{procedure TDSUFeaturesConfig.SetDisableEditorClearType(Value: Boolean);
begin
  SetRegValue(FDisableEditorClearType, Value, 'DDevExDisableEditorClearType');
end;}

procedure TDSUFeaturesConfig.PMGetText(Sender: TObject; Node: PVirtualNode; Column: Integer;
  TextType: TVSTTextType; var CellText: WideString);
type
  PNodeData = ^TNodeData;
  TNodeData = record
    Location: IInterface;
    GraphData: IInterface;
    Used: Boolean;
    OpenIndex, ClosedIndex: Integer;
  end;
var
  P: PVirtualNode;
  Data: PNodeData;
  Obj: TObject;
begin
  FOrgPMGetTextEvent(Sender, Node, Column, TextType, CellText);
  if FPrjMgrTree.Selected[Node] and (FPrjMgrTree.GetNodeLevel(Node) > 1) then
  begin
    if FPrjMgrTree.GetNextSelected(FPrjMgrTree.GetFirstSelected) = nil then // only one node is selected
    begin
      { It must be a file node }
      Data := FPrjMgrTree.GetNodeData(Node);
      {$IF CompilerVersion >= 21.0} // Delphi 2010+
      Obj := Data.Location as TObject;
      {$ELSE}
      Obj := DelphiInterfaceToObject(Data.Location);
      {$IFEND}
      if (Obj = nil) or FPrjMgrTree.IsEditing or
         not InheritsFromClassName(Obj, 'TStdFileContainer') or
         InheritsFromClassName(Obj, 'TStdDirectoryContainer') then
        Exit;
      { Do not show the module's project if there is only one project in the project group }
      if ((BorlandIDEServices as IOTAModuleServices).MainProjectGroup <> nil) and
         ((BorlandIDEServices as IOTAModuleServices).MainProjectGroup.ProjectCount <= 1) then
        Exit;

      { Find the project node }
      P := FPrjMgrTree.NodeParent[Node];
      while (P <> nil) and (FPrjMgrTree.GetNodeLevel(P) > 1) do
        P := FPrjMgrTree.NodeParent[P];

      { Append the project's name }
      CellText := CellText + '  (' + FPrjMgrTree.Text[P, 0] + ')';
    end;
  end;
end;

procedure TDSUFeaturesConfig.SetShowFileProjectInPrjMgr(const Value: Boolean);
var
  ProjectManagerForm: TCustomForm;
  TreeCtrl: TCustomControl;
begin
  if Value <> FShowFileProjectInPrjMgr then
  begin
    { Find the tree and initialize the tree handler }
    if FPrjMgrTree = nil then
    begin
      ProjectManagerForm := FindForm('ProjectManagerForm', '');
      if ProjectManagerForm <> nil then
      begin
        TreeCtrl := ProjectManagerForm.FindComponent('ProjectTree2') as TCustomControl;
        if TreeCtrl <> nil then
        begin
          FPrjMgrTree := TIDEVirtualTreeHandler.Create(TreeCtrl);
          FOrgPMGetTextEvent := FPrjMgrTree.OnGetText;
        end;
      end;
    end;


    if (FPrjMgrTree <> nil) and FShowFileProjectInPrjMgr then
      FPrjMgrTree.OnGetText := FOrgPMGetTextEvent;

    FShowFileProjectInPrjMgr := Value;

    if (FPrjMgrTree <> nil) and FShowFileProjectInPrjMgr then
      FPrjMgrTree.OnGetText := PMGetText;

    FPrjMgrTree.Invalidate;
  end;
end;

procedure TDSUFeaturesConfig.SetStructureViewSearchHotKey(const Value: TShortCut);
begin
  if Value <> FStructureViewSearchHotKey then
  begin
    FStructureViewSearchHotKey := Value;
    if StructureViewSearch <> nil then
      StructureViewSearch.SetHotkey(FStructureViewSearchHotKey);
  end;
end;

procedure TDSUFeaturesConfig.SetEditorDblClickAction(Value: TEditorDblClickAction);
begin
  if Value <> FEditorDblClickAction then
  begin
    if Value = ecaNone then
    begin
      UndoEditorZoom;
      CodeRestore(HookDesktopLoadRuntimeDesktop);
    end;

    if FEditorDblClickAction = ecaNone then
      CodeRedirect(@OrgLoadRuntimeDesktop, @DesktopLoadRuntimeDesktop, HookDesktopLoadRuntimeDesktop);

    FEditorDblClickAction := Value;
    UpdateEditorDblClickAction;
  end;
end;

function GetStructureView: TCustomForm;
begin
  Result := TCustomForm(Application.FindComponent('StructureViewForm'));
end;

function TDSUFeaturesConfig.IsBackgroundParsing: Boolean;
begin
  try
    if FParseThread = nil then
    begin
      FParseThread := DbgStrictGetProcAddress(GetModuleHandle(coreide_bpl), '@Parserthread@ParseThread');
      if FParseThread <> nil then
        FParseThread := TThread(Pointer(FParseThread)^);
    end;
    if FParseThread <> nil then
      Result := PBoolean(DWORD_PTR(FParseThread) + DWORD(TThread.InstanceSize) + $28)^
    else
      Result := False;
  except
    FTimerStructureView.Enabled := False;
    Result := False;
  end;
end;

procedure TDSUFeaturesConfig.StructureViewTimer(Sender: TObject);
var
  Index: Integer;
  S: string;
  StructView: TCustomForm;
begin
  StructView := GetStructureView;
  if (StructView <> nil) and StructView.Visible and
     (StructView.HandleAllocated and IsWindowVisible(StructView.Handle)) then
  begin
    S := StructView.Caption;
    Index := Pos('.', S);
    if Index > 0 then
      S := Copy(S, 1, Index - 1);
    if IsBackgroundParsing then
    begin
      FLastParsingDots := (FLastParsingDots + 1) mod 4;
      StructView.Caption := S + DupeString('.', FLastParsingDots);
    end
    else
    begin
      FLastParsingDots := 0;
      if Index > 0 then
        StructView.Caption := S;
    end;
  end;
end;

end.
