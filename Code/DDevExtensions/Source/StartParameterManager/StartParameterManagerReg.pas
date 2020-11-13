unit StartParameterManagerReg;

interface

uses
  Windows, SysUtils, Classes, ActnList, Controls, ComCtrls, ToolsAPI, StartParameterCtrl;

type
  TStartParameterManager = class(TComponent)
  private
    FActionCustomize: TAction;
  protected
    procedure StartParameterExecute(Sender: TObject);
    procedure StartParameterUpdate(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure InitPlugin(Unload: Boolean);

implementation

uses
  TypInfo, // XE
  Forms, IDEHooks, Hooking;

var
  GStartParameterManager: TStartParameterManager;
  EnabledGetRunParamsRedirect: Boolean = False;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    GStartParameterManager := TStartParameterManager.Create(nil)
  else
    FreeAndNil(GStartParameterManager);
end;

type
  TOpenControl = class(TControl);

  TDebugger = class(TObject);
  TDebugProjectOption = class(TObject);

procedure TDebugger_Run(Self: TDebugger; Mode: TOTARunMode);
  external dbkdebugide_bpl name '@Debug@TDebugger@Run$qqr20Toolsapi@TOTARunMode';

{$IF CompilerVersion <> 22.0} // not XE
function _TDebugProjectOption_GetRunParams(Self: TDebugProjectOption): string;
  {$IF CompilerVersion >= 23.0} // Delphi XE2+
  external coreide_bpl name '@Debuggerprojectoptions@TDebugProjectOption@GetRunParams$qqrv';
  {$ELSE}
  external delphicoreide_bpl name '@Basepasprojopts@TProjectOptions@GetRunParams$qqrv';
  {$IFEND}
{$ELSE}
function TDebuggerProjectOptions_GetOptionClassInfo(Instance: TObject): Pointer; // ClassInfo
  external coreide_bpl name '@Debuggerprojectoptions@TDebuggerProjectOptions@GetOptionClassInfo$qqrv';
{$IFEND}

var
  OrgTDebugger_Run: procedure(Self: TDebugger; Mode: TOTARunMode);
  TDebugProjectOption_GetRunParams, OrgTDebugProjectOption_GetRunParams: function(Self: TDebugProjectOption): string;
  {$IF CompilerVersion <= 21.0} // Delphi 2009/2010
  CppTProjectOptions_GetRunParams, OrgCppTProjectOptions_GetRunParams: function(Self: TDebugProjectOption): string;
  {$IFEND}

procedure HookedTDebugger_Run(Self: TDebugger; Mode: TOTARunMode);
begin
  EnabledGetRunParamsRedirect := True;
  try
    OrgTDebugger_Run(Self, Mode);
  finally
    EnabledGetRunParamsRedirect := False;
  end;
end;

function HookedTDebugProjectOption_GetRunParams(Self: TDebugProjectOption): string;
begin
  if EnabledGetRunParamsRedirect then
  begin
    // XE: Called 3 times, but the first returned value will overwrite the others.
    // 2009, 2010 and XE2 call it only once.
    EnabledGetRunParamsRedirect := False;
    try
      if TStartParameterControl.GetActiveParams(Result, True) then
        Exit;
    except
      Application.HandleException(Self);
    end;
  end;
  Result := OrgTDebugProjectOption_GetRunParams(Self);
end;

{$IF CompilerVersion <= 21.0} // Delphi 2009/2010
function HookedCppTProjectOptions_GetRunParams(Self: TDebugProjectOption): string;
begin
  if EnabledGetRunParamsRedirect then
  begin
    EnabledGetRunParamsRedirect := False;
    try
      if TStartParameterControl.GetActiveParams(Result, True) then
        Exit;
    except
      Application.HandleException(Self);
    end;
  end;
  Result := OrgCppTProjectOptions_GetRunParams(Self);
end;
{$IFEND}

{$IF CompilerVersion = 22.0} // Delphi XE

type
  IDebugParamOptions = interface
    ['{21CEDACD-883D-4829-8AB5-7D9106DAC581}']
    function GetRunParams: string;
    // ...
  end;

function GetTDebugProjectOption_GetRunParams: Pointer;
const
  IdxGetRunParam = 3;
var
  Cls: TClass;
  P: PByte;
begin
  Cls := GetTypeData(TDebuggerProjectOptions_GetOptionClassInfo(nil)).ClassType;
  P := PPointer(PByte(Cls.GetInterfaceEntry(IDebugParamOptions).VTable) + IdxGetRunParam * SizeOf(Pointer))^;
  Inc(P, 3); // add eax, ...
  Result := Pointer(P + 5 + PInteger(P + 1)^); // jmp offset
end;
{$IFEND}

{ TStartParameterManager }

constructor TStartParameterManager.Create(AOwner: TComponent);
var
  Services: INTAServices;
  ToolBar: TToolBar;
  Control: TStartParameterControl;
//  Button: TControl;
begin
  inherited Create(AOwner);

  RegisterClass(TStartParameterControl);

{  FActionCustomize := TAction.Create(nil);
  FActionCustomize.Name := 'DDevExtActionCustomizeStartParameters';
  FActionCustomize.Caption := '';
  FActionCustomize.OnExecute := StartParameterExecute;
  FActionCustomize.OnUpdate := StartParameterUpdate;}

  Services := BorlandIDEServices as INTAServices;
  ToolBar := Services.NewToolbar('DDevExtToolbarStartParameters', 'Start Parameters', sDesktopToolBar, True);
  ToolBar.AutoSize := True;

//  Services.AddActionMenu('', FActionCustomize, nil); // transfer Action to the IDE's ActionList
//  FActionCustomize.ImageIndex := (Application.MainForm.FindComponent('RunRunCommand') as TCustomAction).ImageIndex; // must be set after AddActionMenu()
//  Button := Services.AddToolButton('DDevExtToolbarStartParameters', 'DDevExtToolButtonCustomizeStartParameters', FActionCustomize);

  Control := TStartParameterControl.Create(nil);
  //Control.Left := Button.Left + Button.Width;
  Control.Top := 0;
  Control.Parent := ToolBar;

  {$IF CompilerVersion <> 22.0} // Delphi XE
  @TDebugProjectOption_GetRunParams := @_TDebugProjectOption_GetRunParams;
  {$ELSE}
  @TDebugProjectOption_GetRunParams := GetTDebugProjectOption_GetRunParams;
  {$IFEND}
  @OrgTDebugger_Run := RedirectOrgCall(@TDebugger_Run, @HookedTDebugger_Run);

  @OrgTDebugProjectOption_GetRunParams := RedirectOrgCall(@TDebugProjectOption_GetRunParams, @HookedTDebugProjectOption_GetRunParams);
  {$IF CompilerVersion <= 21.0} // Delphi 2009/2010
  if GetModuleHandle(bcbide_bpl) <> 0 then
  begin
    @CppTProjectOptions_GetRunParams := DbgStrictGetProcAddress(GetModuleHandle(bcbide_bpl), PAnsiChar('@Msbprojopts@TProjectOptions@GetRunParams$qqrv'));
    if Assigned(CppTProjectOptions_GetRunParams) then
      @OrgCppTProjectOptions_GetRunParams := RedirectOrgCall(@CppTProjectOptions_GetRunParams, @HookedCppTProjectOptions_GetRunParams);
  end;
  {$IFEND}
end;

destructor TStartParameterManager.Destroy;
begin
  RestoreOrgCall(@TDebugger_Run, @OrgTDebugger_Run);
  RestoreOrgCall(@TDebugProjectOption_GetRunParams, @OrgTDebugProjectOption_GetRunParams);
  {$IF CompilerVersion <= 21.0} // Delphi 2009/2010
  if Assigned(CppTProjectOptions_GetRunParams) then
    RedirectOrgCall(@CppTProjectOptions_GetRunParams, @OrgCppTProjectOptions_GetRunParams);
  {$IFEND}

  // Destroy the toolbar before the DLL is released. Otherwise the IDE would call into
  // the unloaded DLL.
  (BorlandIDEServices as INTAServices).ToolBar['DDevExtToolbarStartParameters'].Free;
  FActionCustomize.Free;
  inherited Destroy;
end;

procedure TStartParameterManager.StartParameterExecute(Sender: TObject);
begin
  // Open the drop down list
end;

procedure TStartParameterManager.StartParameterUpdate(Sender: TObject);
var
  S: string;
begin
  FActionCustomize.Enabled := GetActiveProject <> nil;
  if TStartParameterControl.GetActiveParams(S, False) then
    FActionCustomize.Hint := S
  else
    FActionCustomize.Hint := '';
end;

end.
