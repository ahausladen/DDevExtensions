{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2009 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit DSUFeatures;

interface

procedure InitPlugin(Unload: Boolean);

{$IF CompilerVersion <= 28.0} // XE7-, XE8 replaced DExplorer with *.chm files
procedure KillAllDexplore;
{$IFEND}

implementation

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Hooking, Controls, Forms, Dialogs,
  IDEHooks, ToolsAPI, IDEUtils, AppConsts, ImportHooking, PsAPI, DesignEditors, TypInfo;

{-----------------------------------------------------------------------------}
{ Close all and Kill IDE                                                      }
{-----------------------------------------------------------------------------}
var
  HookTCustomFormClose: TRedirectCode;
  APIHookList: TJclPeMapImgHooks;

type
  TOpenForm = class(TForm);

const
  // English
  sConfirmCloseAllAndKillException_Eng = 'An exception occured while quitting. Do you want to kill Delphi/BCB now?';
  // German
  sConfirmCloseAllAndKillException_Ger = 'Es trat eine Exception während des Beendens auf. Soll der Prozess Delphi/BCB trotzdem beendet werden?';
  (* PrY - French Translation by Pierre Y. / pierre@levosgien.net *)
  sConfirmCloseAllAndKillException_Fre = 'Une erreur s''est produite pendant la fermeture de Delphi/BCB. Voulez-vous terminer le processus?';
  (* /PrY *)

function sConfirmCloseAllAndKillException: string;
begin
  case GetLang of
    LANG_GERMAN: Result := sConfirmCloseAllAndKillException_Ger;
    LANG_FRENCH: Result := sConfirmCloseAllAndKillException_Fre;
  else
    Result := sConfirmCloseAllAndKillException_Eng;
  end;
end;

procedure HookedTCustomFormClose(Self: TOpenForm);

  function CloseForm(Self: TOpenForm): TCloseAction;
  begin
    Result := caNone;
    with Self do
      if fsModal in FFormState then
        ModalResult := mrCancel
      else
        if CloseQuery then
        begin
          if FormStyle = fsMDIChild then
            if biMinimize in BorderIcons then
              Result := caMinimize else
              Result := caNone
          else
            Result := caHide;
          DoClose(Result);
          if Result <> caNone then
            if Application.MainForm = Self then Application.Terminate
            else if Result = caHide then Hide
            else if Result = caMinimize then WindowState := wsMinimized
            else Release;
        end;
  end;

var
  ForceClose: Boolean;
begin
  if Self = Application.MainForm then
  begin
    ForceClose := ssCtrl in KeyboardStateToShiftState;
    try
      if (CloseForm(Self) <> caNone) and ForceClose then
      begin
        Application.ProcessMessages;
        TerminateProcess(GetCurrentProcess, 0);
      end;
    except
      on E: EAbort do
        raise;
      on E: Exception do
      begin
        Application.HandleException(E);
        { The IDE cannot be terminated because OnCloseQuery/OnClose raises an exception.
          Let the user decide if he wants to kill the IDE. }
        if MessageDlg(sConfirmCloseAllAndKillException, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          TerminateProcess(GetCurrentProcess, 0);
      end;
    end;
  end
  else
    CloseForm(Self);
end;

{------------------------------------------------------------------------------------}
{ Property Editor for Methods show "(nil)" if explicitly set to nil in derived class }
{------------------------------------------------------------------------------------}
type
  TMethodPropertyEx = class(TMethodProperty)
  public
    function GetValue: string; override;
  end;

//  IUpdateManager = interface
//    ['{7ED7BF35-E349-11D3-AB4A-00C04FB17A72}']
//    function CanRevert(Instance: TPersistent; PropInfo: PPropInfo): Boolean;
//    function GetComponent: TComponent;
//    function GetAncestor: TComponent;
//    function GetHasDescendents: Boolean;
//    function GetIsDescendent: Boolean;
//    procedure Modified;
//    function NameExists(const Name: string): Boolean;
//    procedure Notification(Component: TPersistent; Operation: TOperation);
//    procedure RenameComponent(const CurrentName, NewName: string);
//    procedure RenameDescendents(const CurrentName, NewName: string);
//    procedure Revert(Instance: TPersistent; PropInfo: PPropInfo);
//    procedure Update;
//    property Component: TComponent read GetComponent;
//    property Ancestor: TComponent read GetAncestor;
//    property HasDescendents: Boolean read GetHasDescendents;
//    property IsDescendent: Boolean read GetIsDescendent;
//  end;
//
//  IInternalRoot = interface(IRoot)
//    ['{7ED7BF2C-E349-11D3-AB4A-00C04FB17A72}']
//    function CanRevert(Instance: TPersistent; PropInfo: PPropInfo): Boolean;
//    procedure CancelModes;
//    procedure CopySelectionToStream(S: TMemoryStream;
//      UnitDependencies: TStrings);
//    procedure DesignerHide;
//    procedure DesignerShow;
//    procedure Edit(const Component: TComponent);
//    function GetCustomModuleClass: TCustomModuleClass;
//    function GetRootBaseClass: string;
//    function GetRootClass: TClass;
//    function GetCustomModule: ICustomModule;
//    function GetComponentDesigner: IInternalComponentDesigner;
//    function GetUpdateManager: IUpdateManager;
//    {...}
//  end;

function TMethodPropertyEx.GetValue: string;
var
  M: TMethod;
  Ancestor: TComponent;
  AncestorPropInfo: PPropInfo;
  Comp: TPersistent;
begin
  M := GetMethodValue;
  Result := Designer.GetMethodName(M);
  Comp := GetComponent(0);
  if (Result = '') {(M.Code = nil) and (M.Data = nil)} and (Comp is TComponent) and (TComponent(Comp).Name <> '') then
  begin
    Ancestor := nil;
    if Designer.AncestorDesigner <> nil then
      Ancestor := Designer.AncestorDesigner.GetComponent(TComponent(Comp).Name);
    if Ancestor = nil then
    begin
      // In a Frame?
//      Self.GetComponent()

// Idea: Aren't modified items painted with a "bold" font? See this code.

    end;

    if Ancestor <> nil then
    begin
      AncestorPropInfo := TypInfo.GetPropInfo(Ancestor, UTF8ToString(Self.GetPropInfo.Name), [tkMethod]);
      if AncestorPropInfo <> nil then
      begin
        if TypInfo.GetMethodProp(Ancestor, AncestorPropInfo).Code <> nil then
          Result := '(nil)';
      end;
    end;
  end;
end;

function TMethodProperty_NewInstance(AClass: TClass): TObject;
begin
  Result := TMethodPropertyEx.NewInstance;
end;

{-----------------------------------------------------------------------------}
{ Package Loading changes mouse Cursor to crAppWait                           }
{-----------------------------------------------------------------------------}

function HookedLoadPackageEx(const Name: string; AValidatePackage: TValidatePackageProc): HMODULE;
var
  LastCursor: TCursor;
  MainCaption: string;
  CursorChanged: Boolean;
begin
  LastCursor := crDefault;
  CursorChanged := False;
  if (Application <> nil) and (Application.MainForm <> nil) and
     Application.MainForm.Visible then
  begin
    MainCaption := Application.MainForm.Caption;
    Application.MainForm.Caption  := Format('%s  [Loading %s]', [MainCaption, ExtractFileName(Name)]);
    { Change mouse cursor only when mainform is visible }
    LastCursor := Screen.Cursor;
    if LastCursor = crDefault then
    begin
      CursorChanged := True;
      Screen.Cursor := crAppStart;
    end;
  end;

  try
    Result := LoadPackage(Name, AValidatePackage);
  finally
    Application.MainForm.Caption := MainCaption;
    if CursorChanged and (Screen.Cursor = crAppStart) then
      Screen.Cursor := LastCursor;
  end;
end;

{-----------------------------------------------------------------------------}
{ DisablePackageCache                                                         }
{-----------------------------------------------------------------------------}

procedure PreInit;
const
  sLoadPackageCache = '@Pascpppakmgr@TProfileData@LoadPackageCache$qqr' + _xp_ + System_Inifiles_TCustomIniFile;
  sSavePackageCache = '@Pascpppakmgr@TProfileData@SavePackageCache$qqr' + _xp_ + System_Inifiles_TCustomIniFile + 'o';

  sPaletteItemDelegateSaveData = '@Comppalmgr@TComponentPalettePageItemDelegate@SaveData$qqr' + _xp_ + System_Inifiles_TCustomIniFile;
  sPaletteItemDelegateLoadData = '@Comppalmgr@TComponentPalettePageItemDelegate@LoadData$qqr' + _xp_ + System_Inifiles_TCustomIniFile;

  RetCode: Byte = $C3;
var
  LibDelphiCoreide: THandle;
begin
  { Disable Package and Palette cache }
  if ReadGlobalRegOption('DDevExDisablePackageCache', False) then
  begin
    LibDelphiCoreide := GetModuleHandle(delphicoreide_bpl);
    InjectCode(DbgStrictGetProcAddress(LibDelphiCoreide, sLoadPackageCache), @RetCode, 1);
    InjectCode(DbgStrictGetProcAddress(LibDelphiCoreide, sSavePackageCache), @RetCode, 1);

    InjectCode(DbgStrictGetProcAddress(LibDelphiCoreide, sPaletteItemDelegateLoadData), @RetCode, 1);
    InjectCode(DbgStrictGetProcAddress(LibDelphiCoreide, sPaletteItemDelegateSaveData), @RetCode, 1);
  end;
end;

{-----------------------------------------------------------------------------}
{ InitPlugin                                                                  }
{-----------------------------------------------------------------------------}

procedure InitPlugin(Unload: Boolean);
const
  sLoadPackageEx = Unit_System_SysUtils + '@LoadPackage$qqrx20System@UnicodeStringpqqrui$o';
var
  LibCoreide, LibRtl: THandle;
begin
  if not Unload then
  begin
    CodeRedirect(@TCustomForm.Close, @HookedTCustomFormClose, HookTCustomFormClose);

    LibCoreide := GetModuleHandle(coreide_bpl);
    LibRtl := GetModuleHandle(rtl_bpl);

    APIHookList := TJclPeMapImgHooks.Create;
    APIHookList.ReplaceImport(Pointer(LibCoreide), rtl_bpl, DbgStrictGetProcAddress(LibRtl, PAnsiChar(sLoadPackageEx)), @HookedLoadPackageEx);

    //ReplaceVmtField(TMethodProperty, @TMethodProperty.NewInstance, @TMethodProperty_NewInstance);
  end
  else
  begin
    //ReplaceVmtField(TMethodProperty, @TMethodProperty_NewInstance, @TMethodProperty.NewInstance);
    CodeRestore(HookTCustomFormClose);
    APIHookList.Free;
  end;
end;

{--------------------------------------------------------------------------------------------------}

{$IF CompilerVersion <= 28.0} // XE7-, XE8 replaced DExplorer with *.chm files
function KillProcessFileName(const ProcessName: string; PID: DWORD): Boolean;
var
  Handle: THandle;
  S: string;
begin
  Result := False;
  Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_TERMINATE, False, PID);
  if Handle <> 0 then
  begin
    try
      SetLength(S, MAX_PATH);
      if GetModuleFileNameEx(Handle, 0, PChar(S), MAX_PATH) > 0 then
      begin
        SetLength(S, StrLen(PChar(S)));
        if AnsiSameText(ExtractFileName(S), ProcessName) then
        begin
          TerminateProcess(Handle, 1);
          Result := True;
        end;
      end
      else
        Result := False;
    finally
      CloseHandle(Handle);
    end;
  end;
end;

procedure KillAllDexplore;
const
  ProcessName = 'dexplore.exe';
var
  PIDs: array of DWORD;
  Needed: DWORD;
  NeededCount: Integer;
  I: Integer;
  Success: Boolean;
begin
  SetLength(PIDs, 1024);
  Success := EnumProcesses(@PIDs[0], SizeOf(DWORD) * Length(PIDs), Needed);
  NeededCount := (Needed div SizeOf(DWORD));
  if NeededCount >= Length(PIDs) then
  begin
    SetLength(PIDs, NeededCount);
    Success := EnumProcesses(@PIDs[0], SizeOf(DWORD) * Length(PIDs), Needed);
  end;

  if Success then
    for I := 0 to NeededCount - 1 do
      KillProcessFileName(ProcessName, PIDs[I]);
end;
{$IFEND}

initialization
  PreInit;

end.
