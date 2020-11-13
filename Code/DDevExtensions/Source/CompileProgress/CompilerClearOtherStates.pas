unit CompilerClearOtherStates;

{
   Clears the CompStates of the other Delphi projects when a project is compiled
}

interface

procedure SetClearCompilerUnitCacheOtherStates(Enabled, OnlyHighUsge: Boolean);

implementation

uses
  Windows, PsAPI, SysUtils, Hooking, IDEHooks, IDEUtils,
  {$IF CompilerVersion >= 23.0}
  PlatformAPI,
  {$IFEND}
  ToolsAPI;

var
  ClearOtherStatesEnabled: Boolean = False;
  ClearOtherStatesEnabledHigh: Boolean = True;

type
  ICustomProject = interface
  end;

  TCompState = Integer;
  TCompileMode = (cmMake, cmBuild, cmCheck, cmKibitz, cmClean, cmLink);
  TCompileResult = (crFailed, crSucceeded, crBackground);
  TCompileFinishedProc = procedure (CompileMode: TCompileMode; const Project: ICustomProject; Succeeded: Boolean) of object;

  TClearCompStateProc = procedure(CompState: TCompState); stdcall;

  TCompStateRec = record
    PlatformName: string;
    Dll: string;
    ClearCompState: TClearCompStateProc;
  end;

{$IF CompilerVersion < 23.0} // XE-
const
  cWin32Platform = 'Win32';
{$IFEND}

var
  {$WARNINGS OFF}
  cClearCompStatePlatforms: array[0..
    0
    {$IF CompilerVersion >= 23.0}+2{$IFEND} // XE2+
    {$IF CompilerVersion >= 25.0}+2{$IFEND} // XE4+
    {$IF CompilerVersion >= 26.0}+1{$IFEND} // XE5+
    {$IF CompilerVersion >= 29.0}+1{$IFEND} // XE8+
    {$IF CompilerVersion >= 33.0}+2{$IFEND} // 10.3 Rio+
    ] of TCompStateRec = (
     (PlatformName: cWin32Platform; Dll: dcc32_dll)
    {$IF CompilerVersion >= 23.0} // XE2+
    ,(PlatformName: cWin64Platform; Dll: dcc64_dll)
    ,(PlatformName: cOSX32Platform; Dll: dccosx32_dll)
    {$IFEND}
    {$IF CompilerVersion >= 25.0} // XE4+
    ,(PlatformName: ciOSSimulatorPlatform; Dll: dccios32_dll)
    ,(PlatformName: ciOSDevicePlatform; Dll: dcciosarm_dll)
    {$IFEND}
    {$IF CompilerVersion >= 26.0} // XE5+
    ,(PlatformName: cAndroidPlatform; Dll: dccaarm_dll)
    {$IFEND}
    {$IF CompilerVersion >= 29.0} // XE8+
    ,(PlatformName: ciOSDevice64Platform; Dll: dccios64_dll)
    {$IFEND}
    {$IF CompilerVersion >= 33.0} // 10.3 Rio+
    ,(PlatformName: cLinux32Platform; Dll: dcclinux32_dll)
    ,(PlatformName: cLinux64Platform; Dll: dcclinux64_dll)
    {$IFEND}
  );
  {$WARNINGS ON}

{$IF CompilerVersion >= 22.0} // XE+
type
  IProjectBuilder = interface
  end;

  TProjectBuildInfo = record
    Project: ICustomProject;
    Builder: IProjectBuilder;
    Configuration: string;
    Platform: string;
    Success: Boolean;
  end;

  TBuildControl = record
    FAllowBackground: Boolean;
    FBackground: Boolean;
    FChangeActiveOnError: Boolean;
    FCheckActive: Boolean;
    FClearMessages: Boolean;
    FMode: TCompileMode;
    FSilent: Boolean;
    FWait: Boolean;
  end;

procedure TCompiler_Compile(Compiler: TObject; var BuildInfo: TProjectBuildInfo; const BuildControl: TBuildControl; const FileName: string);
  external delphicoreide_bpl name '@Basepascomintf@TCompiler@Compile$qqrr26Compintf@TProjectBuildInforx22Compintf@TBuildControlx20System@UnicodeString';
var
  Org_TCompiler_Compile: procedure(Compiler: TObject; var BuildInfo: TProjectBuildInfo; const BuildControl: TBuildControl; const FileName: string);
{$ELSEIF CompilerVersion = 21.0} // 2010
function TCompiler_Compile(Compiler: TObject; const FileName: string; const Project: ICustomProject; CompileMode: TCompileMode; Wait, ClearPackages, ClearMessages: Boolean; Configuration: string; FinishProc: TCompileFinishedProc): TCompileResult;
  external delphicoreide_bpl name '@Basepascomintf@TCompiler@Compile$qqrx20System@UnicodeStringx50System@%DelphiInterface$t22Codemgr@ICustomProject%21Compintf@TCompileModeooo20System@UnicodeStringynpqqr21Compintf@TCompileModex50System@%DelphiInterface$t22Codemgr@ICustomProject%o$v'; // 2010
var
  Org_TCompiler_Compile: function(Compiler: TObject; const FileName: string; const Project: ICustomProject; CompileMode: TCompileMode; Wait, ClearPackages, ClearMessages: Boolean; Configuration: string; FinishProc: TCompileFinishedProc): TCompileResult;
{$ELSE}
function TCompiler_Compile(Compiler: TObject; const FileName: string; const Project: ICustomProject; CompileMode: TCompileMode; Wait, ClearPackages, ClearMessages: Boolean; FinishProc: TCompileFinishedProc): TCompileResult;
  external delphicoreide_bpl name '@Basepascomintf@TCompiler@Compile$qqrx20System@UnicodeStringx50System@%DelphiInterface$t22Codemgr@ICustomProject%21Compintf@TCompileModeoooynpqqr21Compintf@TCompileModex50System@%DelphiInterface$t22Codemgr@ICustomProject%o$v'; // 2009
var
  Org_TCompiler_Compile: function(Compiler: TObject; const FileName: string; const Project: ICustomProject; CompileMode: TCompileMode; Wait, ClearPackages, ClearMessages: Boolean; FinishProc: TCompileFinishedProc): TCompileResult;
{$IFEND}

{$IF CompilerVersion >= 23.0} // XE2+
function TCustomCodeIProject_GetCompState(Project: TObject; const PlatformName: string): Integer;
  external coreide_bpl name '@Projectmodule@TCustomCodeIProject@GetCompState$qqrx20System@UnicodeString';
{$ELSE}
function TCustomCodeIProject_GetCompState(Project: TObject): Integer;
  external coreide_bpl name '@Projectmodule@TCustomCodeIProject@GetCompState$qqrv';
{$IFEND}

procedure NopClearCompState(CompState: Integer); stdcall;
begin
end;

procedure ClearCompStatePlatform(CompState: Integer; const PlatformName: string);
var
  I: Integer;
  Lib: THandle;
begin
  for I := 0 to High(cClearCompStatePlatforms) do
  begin
    if cClearCompStatePlatforms[I].PlatformName = PlatformName then
    begin
      if not Assigned(cClearCompStatePlatforms[I].ClearCompState) then
      begin
        Lib := GetModuleHandle(PChar(cClearCompStatePlatforms[I].Dll));
        if Lib <> 0 then
          @cClearCompStatePlatforms[I].ClearCompState := GetProcAddress(Lib, 'ClearCompState');
      end;
      if Assigned(cClearCompStatePlatforms[I].ClearCompState) then
        cClearCompStatePlatforms[I].ClearCompState(CompState)
      else
        @cClearCompStatePlatforms[I].ClearCompState := @NopClearCompState;
      Break;
    end;
  end;
end;

{$IF CompilerVersion >= 22.0} // XE+
procedure TCompiler_CompileHook(Compiler: TObject; var BuildInfo: TProjectBuildInfo; const BuildControl: TBuildControl; const FileName: string);
{$ELSE}
function TCompiler_CompileHook(Compiler: TObject; const FileName: string; const Project: ICustomProject; CompileMode: TCompileMode; Wait, ClearPackages, ClearMessages: Boolean; {$IF CompilerVersion = 21.0}Configuration: string;{$IFEND} FinishProc: TCompileFinishedProc): TCompileResult;
{$IFEND}
const
  MemThreshold = LongWord($50000000);
var
  OtherProject, CompileProject: TObject;
  Group: IOTAProjectGroup;
  I: Integer;
  PlatformName: string;
  MemCounters: TProcessMemoryCounters;
  NeedReleaseMem: Boolean;
begin
  if ClearOtherStatesEnabled then
  begin
    NeedReleaseMem := True;
    {$IFNDEF CPUX64}
    if ClearOtherStatesEnabledHigh then
    begin
      MemCounters.cb := SizeOf(MemCounters);
      if GetProcessMemoryInfo(GetCurrentProcess, @MemCounters, SizeOf(MemCounters)) then
      begin
        if (MemCounters.WorkingSetSize < MemThreshold) and (MemCounters.PagefileUsage < MemThreshold) then
          NeedReleaseMem := False;
      end;
    end;
    {$ENDIF CPUX64}

    if NeedReleaseMem then
    begin
      try
        Group := (BorlandIDEServices as IOTAModuleServices).MainProjectGroup;
        if Group <> nil then
        begin
          {$IF CompilerVersion >= 22.0} // XE+
          CompileProject := DelphiInterfaceToObject(BuildInfo.Project);
          {$ELSE}
          CompileProject := DelphiInterfaceToObject(Project);
          {$IFEND}
          for I := 0 to Group.ProjectCount - 1 do
          begin
            // Packages (bpl) are referenced inside projects, so do not release their memory as it
            // will cause crashes.
            if Group.Projects[I].ProjectType <> sPackage then
            begin
              OtherProject := DelphiInterfaceToObject(Group.Projects[I]);
              if (OtherProject <> CompileProject) and InheritsFromClassName(OtherProject, 'TCustomCodeIProject') then
              begin
                {$IF CompilerVersion >= 23.0} // XE2+
                for PlatformName in Group.Projects[I].SupportedPlatforms do
                  ClearCompStatePlatform(TCustomCodeIProject_GetCompState(OtherProject, PlatformName), PlatformName);
                {$ELSE}
                PlatformName := 'Win32';
                ClearCompStatePlatform(TCustomCodeIProject_GetCompState(OtherProject), PlatformName);
                {$IFEND}
              end;
            end;
          end;
        end;
      except
      end;
    end;
  end;
  {$IF CompilerVersion >= 22.0} // XE+
  Org_TCompiler_Compile(Compiler, BuildInfo, BuildControl, FileName);
  {$ELSE}
  Result := Org_TCompiler_Compile(Compiler, FileName, Project, CompileMode,
      Wait, ClearPackages, ClearMessages, {$IF CompilerVersion = 21.0}Configuration, {$IFEND} FinishProc);
  {$IFEND}
end;

procedure SetClearCompilerUnitCacheOtherStates(Enabled, OnlyHighUsge: Boolean);
begin
  ClearOtherStatesEnabledHigh := OnlyHighUsge;
  if Enabled <> ClearOtherStatesEnabled then
  begin
    ClearOtherStatesEnabled := Enabled;
    if ClearOtherStatesEnabled then
      @Org_TCompiler_Compile := RedirectOrgCall(@TCompiler_Compile, @TCompiler_CompileHook)
    else
      RestoreOrgCall(@TCompiler_Compile, @Org_TCompiler_Compile);
  end;
end;

end.
