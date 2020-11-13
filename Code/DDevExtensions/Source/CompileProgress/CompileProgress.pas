{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2009 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit CompileProgress;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, SysUtils, Classes, Hooking, IDEHooks, ToolsAPI, Controls, Forms,
  Registry, FrmTreePages, IDENotifiers, InterceptIntf, InterceptLoader,
  {$IF CompilerVersion >= 21.0} // Delphi 2010+
  Rtti,
  {$IFEND}
  PluginConfig, Dialogs;

type
  TCompileProgress = class(TPluginConfig, ICompileInterceptor)
  private
    FCompileInterceptorId: Integer;
    FIDENotifier: TIDENotifier;
    FPasFiles: TStrings;
    FReleaseCompilerUnitCache: Boolean;
    FReleaseCompilerUnitCacheHigh: Boolean;
    FAutoSaveAfterSuccessfulCompile: Boolean;
    {$IF CompilerVersion < 23.0} // XE2+ changed how version info works
    FLastCompileVersionInfo: Boolean;
    FLastCompileVersionInfoFormat: string;
    {$IFEND}
    FAskCompileFromDiffProject: Boolean;
    FAskCompileFromDiffProjectTemporary: Boolean;
    {$IF CompilerVersion < 23.0} // XE2+ changed how version info works
    procedure UpdateLastCompileVersionInfo(const Project: IOTAProject);
    {$IFEND}
    procedure SetAskCompileFromDiffProject(const Value: Boolean);
    procedure UpdateInMainThread;
    procedure SetReleaseCompilerUnitCache(const Value: Boolean);
    procedure SetReleaseCompilerUnitCacheHigh(const Value: Boolean);
  {$IF CompilerVersion < 22.0} // XE has its own option
  private
    FDisableRebuildDlg: Boolean;
    FRebuildAddress: Pointer;
    FRebuildOrgBytes: array[0..2] of Byte;
    procedure SetDisableRebuildDlg(const Value: Boolean);
  {$IFEND}
  protected
    procedure AfterCompile(const Project: IOTAProject; Succeeded: Boolean; IsCodeInsight: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
  protected
    function GetOptionPages: TTreePage; override;
    procedure Init; override;

    { ICompileInterceptor }
    function GetOptions: TCompileInterceptOptions; stdcall;
    function GetVirtualFile(Filename: PWideChar): IVirtualStream; stdcall;
    function AlterFile(Filename: PWideChar; Content: PByte; FileDate, FileSize: Integer): IVirtualStream; stdcall;
    procedure InspectFilename(Filename: PWideChar; FileMode: TInspectFileMode); stdcall;
    function AlterMessage(IsCompilerMessage: Boolean; var MsgKind: TMsgKind;
      var Code: Integer; const Filename: IWideString; var Line, Column: Integer;
      const Msg: IWideString): Boolean; stdcall;
    procedure CompileProject(ProjectFilename: PWideChar; UnitPaths: PWideChar;
      SourcePaths: PWideChar; DcuOutputDir: PWideChar; IsCodeInsight: Boolean;
      var Cancel: Boolean); stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ReleaseCompilerUnitCache: Boolean read FReleaseCompilerUnitCache write SetReleaseCompilerUnitCache;
    property ReleaseCompilerUnitCacheHigh: Boolean read FReleaseCompilerUnitCacheHigh write SetReleaseCompilerUnitCacheHigh;
    {$IF CompilerVersion < 22.0} // XE has its own option
    property DisableRebuildDlg: Boolean read FDisableRebuildDlg write SetDisableRebuildDlg;
    {$IFEND}
    property AutoSaveAfterSuccessfulCompile: Boolean read FAutoSaveAfterSuccessfulCompile write FAutoSaveAfterSuccessfulCompile;
    {$IF CompilerVersion < 23.0} // XE2+ changed how version info works
    property LastCompileVersionInfo: Boolean read FLastCompileVersionInfo write FLastCompileVersionInfo;
    property LastCompileVersionInfoFormat: string read FLastCompileVersionInfoFormat write FLastCompileVersionInfoFormat;
    {$IFEND}
    property AskCompileFromDiffProject: Boolean read FAskCompileFromDiffProject write SetAskCompileFromDiffProject;
    property AskCompileFromDiffProjectTemporary: Boolean read FAskCompileFromDiffProjectTemporary write FAskCompileFromDiffProjectTemporary;
  end;

procedure InitPlugin(Unload: Boolean);

implementation

uses
  Main, NativeProgressForm, AppConsts,
  FrmeOptionPageCompilerProgress, ProjectResource, UnitVersionInfo, ToolsAPIHelpers,
  FrmSwitchToModuleProject, CompilerClearOtherStates;

function TimeStr(dt: TDateTime; Exact: Boolean = False): string;
var
  ms: Cardinal;
  h, m, s: Integer;
  nms: Int64;
begin
  Result := '';
  if Int(dt) > 0 then
    Result := IntToStr(Trunc(dt)) + ' d ';
  nms := Round(Frac(dt) * MSecsPerDay);

  ms := nms div 1000;
  h := ms div 3600;
  ms := ms mod 3600;
  m := ms div 60;
  s := ms mod 60;

  Result := '';
  if h > 0 then
    Result := Result + IntToStr(h) + 'h ';
  if (m > 0) or (h > 0) then
  begin
    {if (h > 0) and (m < 10) then
      Result := Result + '0';}
    Result := Result + IntToStr(m) + 'm ';
  end;
  {if (m > 0) or (h > 0) and (s < 10) then
    Result := Result + '0';}
  if Exact and (h = 0) and (m = 0) then
    Result := Result + Format('%1.2f', [s + (nms mod 1000)/1000]) + 's'
  else
    Result := Result + IntToStr(s) + 's';
end;

var
  OrgStartCompile, OrgCallStartCompile: procedure(Inst: TObject); register;
  GlobalCompileProgress: TCompileProgress;

procedure HookedStartCompile(Inst: TForm);
var
  t: TDateTime;
begin
  {$IF CompilerVersion >= 21.0} // Delphi 2010+
  // Delphi 2010+ call StartCompile once, which then calls BeforeCompile for each project.
  // Older Delphi versions call BeforeCompile, then StartCompile for the first project.
  // BeforeCompile, then StartCompile for the second project, ...
  if GlobalCompileProgress <> nil then
  begin
    TStringList(GlobalCompileProgress.FPasFiles).Sorted := False;
    GlobalCompileProgress.FPasFiles.Clear;
  end;
  {$IFEND}

  if FormNativeProgress <> nil then
    FormNativeProgress.ShowProgressBar(True);
  t := Now;
  try
    OrgCallStartCompile(Inst);
  finally
    t := Now - t;
    if GlobalCompileProgress <> nil then
      GlobalCompileProgress.FPasFiles.Clear;
  end;

  if FormNativeProgress <> nil then
  begin
    FormNativeProgress.ProjectFilesCompiled := FormNativeProgress.MaxFiles;
    if FormNativeProgress <> nil then
      FormNativeProgress.ShowProgressBar(False);

    FormNativeProgress.CurrFile := FormNativeProgress.CurrFile  + '     Time: ' + TimeStr(t);
  end;
end;

type
  TCompileMode = (cmMake, cmBuild, cmCheck, cmKibitz, cmClean, cmLink); // I really start to love the new RTTI :-)

{$IF CompilerVersion >= 21.0} // Delphi 2010+
var
  OrgProjectGroupCompileActive, OrgCallProjectGroupCompileActive: function(Instance: TObject; CompileMode: TCompileMode; Wait: Boolean): Boolean;
  OrgAppBuilderCompile: Pointer;

procedure HookedProjectGroupCompileActive;
  forward;

function CallOrgProjectGroupCompileActive(Instance: TObject; CompileMode: TCompileMode; Wait: Boolean): Boolean;
begin
  { Compile/Build/Check/... }
  Result := OrgCallProjectGroupCompileActive(Instance, CompileMode, Wait);
end;
{$ELSE}
var
  OrgCompileActiveProject, OrgCallCompileActiveProject: function(Instance: TObject; CompileMode: TCompileMode; Wait: Boolean): Boolean;

function CompileActiveProject(Instance: TObject; CompileMode: TCompileMode; Wait: Boolean): Boolean;
  forward;

function CallOrgProjectGroupCompileActive(Instance: TObject; CompileMode: TCompileMode; Wait: Boolean): Boolean;
begin
  { Compile/Build/Check/... }
  Result := OrgCallCompileActiveProject(Instance, CompileMode, Wait);
end;
{$IFEND}

function CompileActiveProject(Instance: TObject; CompileMode: TCompileMode; Wait: Boolean): Boolean;

  procedure CollectDependencies(const Dependencies: IOTAProjectGroupProjectDependencies;
    const DependentProjects: TInterfaceList; const Project: IOTAProject);
  var
    List: IOTAProjectDependenciesList;
    Prj: IOTAProject;
    I: Integer;
  begin
    DependentProjects.Add(Project);
    List := Dependencies.GetProjectDependencies(Project);
    for I := 0 to List.ProjectCount - 1 do
    begin
      Prj := List.Projects[I];
      if DependentProjects.IndexOf(Prj) = -1 then
        CollectDependencies(Dependencies, DependentProjects, Prj);
    end;
  end;

const
  sOptAutoCloseProgressDlg = 'AutoCloseProgressDlg';
var
  Module: IOTAModule;
  Project, ActiveProject: IOTAProject;
  I: Integer;
  Found: Boolean;
  Dependencies: IOTAProjectGroupProjectDependencies;
  ModuleServices: IOTAModuleServices;
  Services: IOTAServices;
  DependentProjects: TInterfaceList;
  DontShowAgain: Boolean;
  AutoClose, ConfigModified: Boolean;
begin
  if (GlobalCompileProgress <> nil) and GlobalCompileProgress.AskCompileFromDiffProject then
  begin
    ModuleServices :=  BorlandIDEServices as IOTAModuleServices;
    Services := BorlandIDEServices as IOTAServices;
    Module := ModuleServices.CurrentModule;
    if (Module <> nil) and (Module.OwnerCount > 0) then
    begin
      ActiveProject := GetActiveProject;
      Project := ActiveProject;
      if Project <> nil then
      begin
        Found := False;
        { Check if one of the module's projects is the active project }
        for I := 0 to Module.OwnerCount - 1 do
        begin
          if Module.Owners[I] = Project then
          begin
            Found := True;
            Break;
          end;
        end;

        { Check if one of the module's projects is in the active project's dependency tree }
        if not Found then
        begin
          if Supports(ModuleServices.GetMainProjectGroup, IOTAProjectGroupProjectDependencies, Dependencies) then
          begin
            DependentProjects := TInterfaceList.Create;
            try
              { Obtain all projects on which the active project depend }
              CollectDependencies(Dependencies, DependentProjects, Project);
              Found := False;
              for I := 0 to Module.OwnerCount - 1 do
              begin
                { Either the module's project is the active project or it is }
                if DependentProjects.IndexOf(Module.Owners[I]) <> -1 then
                begin
                  Found := True;
                  Break;
                end;
              end;
            finally
              DependentProjects.Free;
            end;
          end;
        end;

        { Ask the user to switch the project }
        if not Found then
        begin
          ConfigModified := False;
          try
            case TFormSwitchToModuleProject.ShowDialog(Module, Project, DontShowAgain,
                                                       GlobalCompileProgress.AskCompileFromDiffProjectTemporary) of
              mrYes:
                begin
                  ModuleServices.GetMainProjectGroup.ActiveProject := Project;
                  if GlobalCompileProgress.AskCompileFromDiffProjectTemporary then
                  begin
                    GlobalCompileProgress.AskCompileFromDiffProjectTemporary := False;
                    ConfigModified := True;
                  end;
                end;
              mrRetry:
                begin
                  ModuleServices.GetMainProjectGroup.ActiveProject := Project;

                  AutoClose := Boolean(Services.GetEnvironmentOptions.Values[sOptAutoCloseProgressDlg]);
                  { Compile/Build/Check/... the module's project }
                  {$IF CompilerVersion >= 21.0} // Delphi 2010+
                  try
                    if not AutoClose then
                      Services.GetEnvironmentOptions.Values[sOptAutoCloseProgressDlg] := True;
                    Result := OrgCallProjectGroupCompileActive(Instance, CompileMode, Wait);
                  finally
                    if not AutoClose then
                      Services.GetEnvironmentOptions.Values[sOptAutoCloseProgressDlg] := False;
                  end;
                  {$ELSE}
                  try
                    if not AutoClose then
                      Services.GetEnvironmentOptions.Values[sOptAutoCloseProgressDlg] := True;
                    Result := OrgCallCompileActiveProject(Instance, CompileMode, Wait);
                  finally
                    if not AutoClose then
                      Services.GetEnvironmentOptions.Values[sOptAutoCloseProgressDlg] := False;
                  end;
                  {$IFEND}

                  { Restore the last active project so it is also compiled }
                  ModuleServices.GetMainProjectGroup.ActiveProject := ActiveProject;

                  if not GlobalCompileProgress.AskCompileFromDiffProjectTemporary then
                  begin
                    GlobalCompileProgress.AskCompileFromDiffProjectTemporary := True;
                    ConfigModified := True;
                  end;

                  if not Result then
                    Exit;
                end;
              mrCancel:
                Exit(False);
            end;
            if DontShowAgain then
            begin
              GlobalCompileProgress.AskCompileFromDiffProject := False;
              ConfigModified := True;
            end;

          finally
            if ConfigModified then
              GlobalCompileProgress.Save;
          end;
        end;
      end;
    end;
  end;
  Result := CallOrgProjectGroupCompileActive(Instance, CompileMode, Wait);
end;

{$IF CompilerVersion >= 21.0} // Delphi 2010+
procedure HookedProjectGroupCompileActive;
asm
  // Only show the dialog if we are called by TAppBuilder.Compile()
  push eax
  mov eax, [esp+8] // ret-addr
  {$IF CompilerVersion >= 25.0}
  // Read through the CopyProtection Obfuscation (XE6 Update 1)
  cmp BYTE PTR [eax], $E9  // jmp DwordOffset
  jne @@Simple
  cmp BYTE PTR [eax - 3], $FF // call [ebx+ByteOffset]
  jne @@Simple
  add eax, DWORD PTR [eax + 1]
@@Simple:
  {$IFEND}
  sub eax, 5 // call instruction size
  sub eax, OrgAppBuilderCompile // eax = difference between TAppBuilder.Compile and the return address
  jns @@Compare                 // eax was smaller than TAppBuilder.Compile => use abs()
  neg eax
@@Compare:
  cmp eax, 30                   // We can be called from TAppBuilder.Compile() within 30 bytes
  pop eax

  jb CompileActiveProject
  jmp CallOrgProjectGroupCompileActive
end;

procedure InitPlugin(Unload: Boolean);
// We can't hook into bds.exe because the copy protection will catch us. So we need to go a different
// way than what we used to do in Delphi 2009.
const
  StartCompileSymbol = '@Comprgrs@TProgressForm@StartCompile$qqrv';
  ProjectGroupCompileActiveSymbol = '@Projectgroup@TProjectGroup@CompileActive$qqr21Compintf@TCompileModeo';
var
  Ctx: TRttiContext;
  MainType: TRttiType;
  CompileMethod: TRttiMethod;
  coreideLib: THandle;
begin
  if not Unload then
  begin
    GlobalCompileProgress := TCompileProgress.Create;
    coreideLib := GetModuleHandle(coreide_bpl);

    @OrgStartCompile := DbgStrictGetProcAddress(coreideLib, StartCompileSymbol);
    if Assigned(OrgStartCompile) then
      @OrgCallStartCompile := RedirectOrgCall(@OrgStartCompile, @HookedStartCompile);

    @OrgProjectGroupCompileActive := DbgStrictGetProcAddress(coreideLib, ProjectGroupCompileActiveSymbol);
    if Assigned(OrgProjectGroupCompileActive) then
      @OrgCallProjectGroupCompileActive := RedirectOrgCall(@OrgProjectGroupCompileActive, @HookedProjectGroupCompileActive);

    { Get "TAppBuilder.Compile" method address }
    Ctx := TRttiContext.Create;
    try
      MainType := Ctx.GetType(Application.MainForm.ClassInfo);
      if MainType <> nil then
      begin
        CompileMethod := MainType.GetMethod('Compile');
        if CompileMethod <> nil then
          OrgAppBuilderCompile := CompileMethod.CodeAddress;
      end;
    finally
      Ctx.Free;
    end;
  end
  else
  begin
    RestoreOrgCall(@OrgStartCompile, @OrgCallStartCompile);
    RestoreOrgCall(@OrgProjectGroupCompileActive, @OrgCallProjectGroupCompileActive);
    GlobalCompileProgress.Free;
  end;
end;
{$IFEND}

{$IF CompilerVersion < 21.0} // Delphi 2009
procedure InitPlugin(Unload: Boolean);
const
  ProjectMakeCode: array[0..29] of SmallInt = (
    $53,                                    // push ebx
    $8B, $D8,                               // mov ebx,eax
    $8B, $C3,                               // mov eax,ebx
    $E8, -1, -1, -1, -1,                    // call $00420f60
    $C6, $83, $74, $08, $00, $00, $0B,      // mov byte ptr [ebx+$00000874],$0b
    $B1, $01,                               // mov cl,$01
    $33, $D2,                               // xor edx,edx
    $8B, $C3,                               // mov eax,ebx
    $E8, -1, -1, -1, -1, // <<              // call $004191fc
    $5B,                                    // pop ebx
    $C3                                     // ret
  );

  StartCompileSymbol = '@Comprgrs@TProgressForm@StartCompile$qqrv';
var
  coreideLib: THandle;
  P: PByteArray;
begin
  if not Unload then
  begin
    GlobalCompileProgress := TCompileProgress.Create;
    coreideLib := GetModuleHandle(coreide_bpl);

    @OrgStartCompile := DbgStrictGetProcAddress(coreideLib, StartCompileSymbol);
    if Assigned(OrgStartCompile) then
      @OrgCallStartCompile := RedirectOrgCall(@OrgStartCompile, @HookedStartCompile);

    { Hook "TAppBuilder.Compile" method }
    P := Application.MainForm.MethodAddress('ProjectMake');
    if P <> nil then
    begin
      if FindMethodPtr(Cardinal(P), ProjectMakeCode, 1) <> nil then
      begin
        if P[23] = $E8 then // a last check for changes in ProjectMakeCode
        begin
          @OrgCompileActiveProject := Pointer(INT_PTR(@P[23 + 5]) + PInteger(@P[24])^);
          if Assigned(OrgCompileActiveProject) then
            @OrgCallCompileActiveProject := RedirectOrgCall(@OrgCompileActiveProject, @CompileActiveProject);
        end;
      end;
    end;
  end
  else
  begin
    RestoreOrgCall(@OrgStartCompile, @OrgCallStartCompile);
    RestoreOrgCall(@OrgCompileActiveProject, @OrgCallCompileActiveProject);
    GlobalCompileProgress.Free;
  end;
end;
{$IFEND}

{ TCompileProgress }

constructor TCompileProgress.Create;
begin
  inherited Create(AppDataDirectory + '\CompileProgress.xml', 'CompileProgress');
  FPasFiles := TStringList.Create;
  FormNativeProgress := TNativeProgressForm.Create;

  FIDENotifier := TIDENotifier.Create;
  FIDENotifier.OnBeforeCompile := BeforeCompile;
  FIDENotifier.OnAfterCompile := AfterCompile;

  FCompileInterceptorId := GetCompileInterceptorServices.RegisterInterceptor(Self);
end;

destructor TCompileProgress.Destroy;
begin
  GetCompileInterceptorServices.UnregisterInterceptor(FCompileInterceptorId);
  FIDENotifier.Free;
  FormNativeProgress.Free;
  FPasFiles.Free;
  inherited Destroy;
end;

procedure TCompileProgress.Init;
begin
  ReleaseCompilerUnitCache := False;
  ReleaseCompilerUnitCacheHigh := True;
  {$IF CompilerVersion < 22.0} // XE has its own option
  DisableRebuildDlg := True;
  {$IFEND}
  AutoSaveAfterSuccessfulCompile := False;
  AskCompileFromDiffProject := True;
  AskCompileFromDiffProjectTemporary := True;
  {$IF CompilerVersion < 23.0} // XE2+ changed how version info works
  LastCompileVersionInfoFormat := 'yyyy-mm-dd hh:nn';
  {$IFEND}

  SetClearCompilerUnitCacheOtherStates(FReleaseCompilerUnitCache, FReleaseCompilerUnitCacheHigh);
end;

procedure TCompileProgress.AfterCompile(const Project: IOTAProject; Succeeded, IsCodeInsight: Boolean);
begin
  if not IsCodeInsight then
  begin
    if Succeeded and AutoSaveAfterSuccessfulCompile and (Project <> nil) then
      (BorlandIDEServices as IOTAModuleServices).SaveAll;
  end;
end;

procedure TCompileProgress.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
var
  i: Integer;
  Ext: string;
  FileName: string;
begin
  if not IsCodeInsight then
  begin
    {$IF CompilerVersion < 23.0} // XE2+ changed how version info works
    if LastCompileVersionInfo then
      UpdateLastCompileVersionInfo(Project);
    {$IFEND}

    {$IF CompilerVersion <= 20.0} // Delphi 2009-
    // Delphi 2009 and older call BeforeCompile and then StartCompile for the first project,
    // BeforeCompile and then StartCompile for the second project, ...
    TStringList(FPasFiles).Sorted := False;
    FPasFiles.Clear;
    {$IFEND}
    // BeforeCompile is called multiple times for multiple projects (Delphi 2010+)
    if FPasFiles.Count = 0 then
      FormNativeProgress.ProjectFilesCompiled := 0;

    FPasFiles.Add(ExtractFileName(Project.FileName));
    for i := 0 to Project.GetModuleCount - 1 do
    begin
      FileName := Project.GetModule(i).FileName;
      Ext := AnsiLowerCase(ExtractFileExt(FileName));
      if Ext = '.pas' then
      begin
        FPasFiles.Add(ExtractFileName(FileName));
        FPasFiles.Add(ChangeFileExt(ExtractFileName(FileName), '.dcu'));
      end;
    end;
    TStringList(FPasFiles).Sorted := True;
    FormNativeProgress.MaxFiles := FPasFiles.Count div 2;
  end;
end;

procedure TCompileProgress.SetAskCompileFromDiffProject(const Value: Boolean);
begin
  if Value <> FAskCompileFromDiffProject then
  begin
    FAskCompileFromDiffProject := Value;
  end;
end;

procedure TCompileProgress.SetReleaseCompilerUnitCache(const Value: Boolean);
begin
  FReleaseCompilerUnitCache := Value;
  SetClearCompilerUnitCacheOtherStates(FReleaseCompilerUnitCache, FReleaseCompilerUnitCacheHigh);
end;

procedure TCompileProgress.SetReleaseCompilerUnitCacheHigh(const Value: Boolean);
begin
  FReleaseCompilerUnitCacheHigh := Value;
  SetClearCompilerUnitCacheOtherStates(FReleaseCompilerUnitCache, FReleaseCompilerUnitCacheHigh);
end;

{$IF CompilerVersion < 22.0} // XE has its own option
procedure TCompileProgress.SetDisableRebuildDlg(const Value: Boolean);
{
Delphi 2009:
20729AC5 8B45E4           mov eax,[ebp-$1c]
20729AC8 8B10             mov edx,[eax]
20729ACA FF521C           call dword ptr [edx+$1c]
20729ACD 50               push eax
20729ACE 8D45E0           lea eax,[ebp-$20]
20729AD1 8B55FC           mov edx,[ebp-$04]
20729AD4 B9E09D7220       mov ecx,$20729de0
20729AD9 E82A7AEEFF       call $20611508
20729ADE 8B45E0           mov eax,[ebp-$20]
20729AE1 5A               pop edx
20729AE2 8B08             mov ecx,[eax]
20729AE4 FF510C           call dword ptr [ecx+$0c]
20729AE7 84C0             test al,al
20729AE9 7508             jnz $20729af3
20729AEB 8D45FC           lea eax,[ebp-$04]
20729AEE E8057AEEFF       call $206114f8
20729AF3 837DFC00         cmp dword ptr [ebp-$04],$00
20729AF7 0F844D020000     jz $20729d4a

Delphi 2010:
208A356C 8B45F8           mov eax,[ebp-$08]
208A356F 8B10             mov edx,[eax]
208A3571 FF521C           call dword ptr [edx+$1c]  <<==
208A3574 84C0             test al,al
208A3576 0F8488000000     jz $208a3604
208A357C 8D55C8           lea edx,[ebp-$38]
208A357F B8081A8A20       mov eax,$208a1a08
208A3584 E84FE0E3FF       call $206e15d8
208A3589 8B45C8           mov eax,[ebp-$38]
208A358C 6888130000       push $00001388
208A3591 6AFF             push $ff
208A3593 6AFF             push $ff
208A3595 6A00             push $00
208A3597 0FB70DE0378A20   movzx ecx,[$208a37e0]
208A359E B202             mov dl,$02
208A35A0 E8472AE4FF       call $206e5fec
208A35A5 83E802           sub eax,$02
208A35A8 744C             jz $208a35f6
}
const
  {$IFDEF COMPILER12} // Delphi 2009:
  Bytes: array[0..37] of SmallInt = (
    $8B, $45, $E4,
    $8B, $10,
    $FF, $52, -1,
    $50,
    $8D, $45, $E0,
    $8B, $55, $FC,
    $B9, -1, -1, -1, -1,
    $E8, -1, -1, -1, -1,
    $8B, $45, $E0,
    $5A,
    $8B, 08,
    $FF, $51, $0C,
    $84, $C0,
    $75, $08
  );
  {$ELSE} // Delphi 2010:
  Bytes: array[0..60] of SmallInt = (
    $8B, $45, $F8,
    $8B, $10,
    $FF, $52, -1,
    $84, $C0,
    $0F, $84, -1, -1, -1, -1,
    $8D, $55, -1, //$C8,
    $B8, -1, -1, -1, -1,
    $E8, -1, -1, -1, -1,
    $8B, $45, -1, //$C8,
    $68, -1, -1, -1, -1,
    $6A, $FF,
    $6A, $FF,
    $6A, $00,
    $0F, $B7, $0D, -1, -1, -1, -1,
    $B2, $02,
    $E8, -1, -1, -1, -1,
    $83, $E8, $02,
    $74
  );
  {$ENDIF COMPILER12}

  PatchBytes: array[0..2] of Byte = (
    $31, $C0,         // xor eax,eax
    $90               // nop
  );

var
  P, EndP: PByte;
  I: Integer;
  Found: Boolean;
  n: DWORD;
begin
  if FRebuildAddress = nil then
  begin
    { Find the position that must be patched }
    P := DbgStrictGetProcAddress(GetModuleHandle(coreide_bpl), '@Debuggermgr@TDebuggerMgr@MakeCurrentProject$qqrv');
    EndP := DbgStrictGetProcAddress(GetModuleHandle(coreide_bpl), '@Debuggermgr@TDebuggerMgr@GetSupportedDebugCommands$qqrv');
    if (P <> nil) and (EndP > P) then
    begin
      while P < EndP do
      begin
        while (P < EndP) and (P[0] <> $8B) do
          Inc(P);
        if (P < EndP) then
        begin
          Found := True;
          for I := 0 to High(Bytes) do
          begin
            if (Bytes[I] <> -1) and (P[I] <> Byte(Bytes[I])) then
            begin
              Found := False;
              Break;
            end;
          end;
          if Found then
          begin
            FRebuildAddress := P + 5;
            Move(FRebuildAddress^, FRebuildOrgBytes[0], 3);
            Break;
          end;
        end;
        Inc(P);
      end;
    end;
  end;

  if Value <> FDisableRebuildDlg then
  begin
    if FDisableRebuildDlg and (FRebuildAddress <> nil) then
      WriteProcessMemory(GetCurrentProcess, FRebuildAddress, @FRebuildOrgBytes, SizeOf(FRebuildOrgBytes), n);

    FDisableRebuildDlg := Value;

    if FDisableRebuildDlg and (FRebuildAddress <> nil) then
      WriteProcessMemory(GetCurrentProcess, FRebuildAddress, @PatchBytes, SizeOf(PatchBytes), n);
  end;
end;
{$IFEND}

{$IF CompilerVersion < 23.0} // XE2+ changed how version info works
procedure TCompileProgress.UpdateLastCompileVersionInfo(const Project: IOTAProject);
var
  FormatSettings: TFormatSettings;
begin
  {$IF CompilerVersion >= 22.0} // Delphi XE+
  FormatSettings := TFormatSettings.Create((SUBLANG_NEUTRAL shl 10) or LANG_ENGLISH);
  {$ELSE}
  GetLocaleFormatSettings((SUBLANG_NEUTRAL shl 10) or LANG_ENGLISH, FormatSettings);
  {$IFEND}
  if Trim(LastCompileVersionInfoFormat) <> '' then
    SetVersionInfoExtraKey(Project, 'Last Compile', FormatDateTime(LastCompileVersionInfoFormat, Now, FormatSettings))
  else
    SetVersionInfoExtraKey(Project, 'Last Compile', DateTimeToStr(Now, FormatSettings));
end;
{$IFEND}

function TCompileProgress.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Compilation', TFrameOptionPageCompilerProgress, Self);
end;

function TCompileProgress.GetOptions: TCompileInterceptOptions;
begin
  Result := CIO_INSPECTFILENAMES;
end;

procedure TCompileProgress.CompileProject(ProjectFilename, UnitPaths, SourcePaths, DcuOutputDir: PWideChar;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
end;

function TCompileProgress.AlterFile(Filename: PWideChar; Content: PByte; FileDate, FileSize: Integer): IVirtualStream;
begin
  Result := nil;
end;

function TCompileProgress.AlterMessage(IsCompilerMessage: Boolean;
  var MsgKind: TMsgKind; var Code: Integer; const Filename: IWideString;
  var Line, Column: Integer; const Msg: IWideString): Boolean;
begin
  Result := False;
end;

function TCompileProgress.GetVirtualFile(Filename: PWideChar): IVirtualStream;
begin
  Result := nil;
end;

procedure TCompileProgress.UpdateInMainThread;
begin
  FormNativeProgress.ProjectFilesCompiled := FormNativeProgress.ProjectFilesCompiled + 1;
end;

procedure TCompileProgress.InspectFilename(Filename: PWideChar; FileMode: TInspectFileMode);
var
  Index: Integer;
  SFilename: string;
begin
  if (FileMode = ifmOpen) and (GetCurrentThreadId = MainThreadId) {and (FormNativeProgress.Form <> nil)} then
  begin
    SFilename := ExtractFileName(Filename);
    Index := FPasFiles.IndexOf(SFilename);
    if Index <> -1 then
    begin
      //GlobalCompileProgress.FPasFiles.Delete(Index); // prevent the file to be listed twice
      Index := FPasFiles.IndexOf(ChangeFileExt(SFilename, '.dcu'));
      if Index <> -1 then
        FPasFiles.Delete(Index);

      if FormNativeProgress.Form <> nil then
      begin
        if GetCurrentThreadId = MainThreadId then
          FormNativeProgress.ProjectFilesCompiled := FormNativeProgress.ProjectFilesCompiled + 1
        else
          TThread.Queue(nil, UpdateInMainThread);
      end;
    end;
    {if AnsiCompareText(ExtractFileExt(SFilename), '.pas') = 0 then
      FormNativeProgress.FilesCompiled := FormNativeProgress.FilesCompiled + 1;}
  end;
  //OutputDebugString(Filename);
end;

end.
