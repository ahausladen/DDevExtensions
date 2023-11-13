{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2011 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

library DDevExtensions;

{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

{$LIBSUFFIX 'D120'}

{$I ..\Source\DelphiExtension.inc}

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  Graphics,
  StdCtrls,
  ExtCtrls,
  ToolsAPI,
  Dialogs,
  Main in '..\Source\Main.pas',
  AppConsts in '..\Source\AppConsts.pas',
  Splash in '..\Source\Splash.pas',
  ComponentManager in '..\Source\ComponentManager.pas',
  NativeProgressForm in '..\Source\CompileProgress\NativeProgressForm.pas',
  CompileProgress in '..\Source\CompileProgress\CompileProgress.pas',
  FrmProjectSettingsSetVersioninfo in '..\Source\ProjectSettings\FrmProjectSettingsSetVersioninfo.pas' {FormProjectSettingsSetVersioninfo},
  IDEMenuHandler in '..\Source\IDEMenuHandler\IDEMenuHandler.pas',
  DtmImages in '..\Source\DtmImages.pas' {DataModuleImages: TDataModule},
  FrmeOptionPageCompilerProgress in '..\Source\CompileProgress\FrmeOptionPageCompilerProgress.pas' {FrameOptionPageCompilerProgress: TFrame},
  CtrlUtils in '..\Source\CtrlUtils.pas',
  FrmExcelExport in '..\Source\ExcelExport\FrmExcelExport.pas' {FormExcelExport},
  FrmeOptionPageUnitSelector in '..\Source\UnitSelector\FrmeOptionPageUnitSelector.pas' {FrameOptionPageUnitSelector: TFrame},
  FrmeOptionPageKeybindings in '..\Source\Keybindings\FrmeOptionPageKeybindings.pas' {FrameOptionPageKeybindings: TFrame},
  PluginConfig in '..\Source\PluginConfig.pas',
  FrmeOptionPageFileCleaner in '..\Source\FileCleaner\FrmeOptionPageFileCleaner.pas' {FrameOptionPageFileCleaner: TFrame},
  FrmeOptionPageCompileBackup in '..\Source\CompileBackup\FrmeOptionPageCompileBackup.pas' {FrameOptionPageCompileBackup: TFrame},
  RegisterPlugins in '..\Source\RegisterPlugins.pas',
  LabelMarginHelper in '..\Source\FormDesignerHelpers\LabelMarginHelper.pas',
  FrmeOptionPageFormDesigner in '..\Source\FormDesignerHelpers\FrmeOptionPageFormDesigner.pas' {FrameOptionPageFormDesigner: TFrame},
  FrmeBase in '..\Source\FrmeBase.pas' {FrameBase: TFrame},
  FrmFileSelector in '..\Source\FileSelector\FrmFileSelector.pas' {FormFileSelector},
  RemoveExplicitProperty in '..\Source\FormDesignerHelpers\RemoveExplicitProperty.pas',
  DSUFeatures in '..\Source\DSUFeatures\DSUFeatures.pas',
  FrmeOptionPageDSUFeatures in '..\Source\DSUFeatures\FrmeOptionPageDSUFeatures.pas' {FrameOptionPageDSUFeatures: TFrame},
  ComponentSelector in '..\Source\ComponentSelector\ComponentSelector.pas',
  FrmeOptionPageComponentSelector in '..\Source\ComponentSelector\FrmeOptionPageComponentSelector.pas' {FrameOptionPageComponentSelector: TFrame},
  InterceptIntf in '..\..\..\CompileInterceptor\Source\InterceptIntf.pas',
  InterceptLoader in '..\..\..\CompileInterceptor\Source\InterceptLoader.pas',
  DelphiDesignerParser in '..\..\..\Shared\PascalParser\DelphiDesignerParser.pas',
  DelphiExpr in '..\..\..\Shared\PascalParser\DelphiExpr.pas',
  DelphiLexer in '..\..\..\Shared\PascalParser\DelphiLexer.pas',
  DelphiParserContainers in '..\..\..\Shared\PascalParser\DelphiParserContainers.pas',
  DelphiPreproc in '..\..\..\Shared\PascalParser\DelphiPreproc.pas',
  IDENotifiers in '..\..\..\Shared\IDE\IDENotifiers.pas',
  ModuleData in '..\..\..\Shared\IDE\ModuleData.pas',
  ProjectResource in '..\..\..\Shared\IDE\ProjectResource.pas',
  IDEHooks in '..\..\..\Shared\IDE\IDEHooks.pas',
  ProjectData in '..\..\..\Shared\IDE\ProjectData.pas',
  SimpleXmlDoc in '..\..\..\Shared\Xml\SimpleXmlDoc.pas',
  SimpleXmlIntf in '..\..\..\Shared\Xml\SimpleXmlIntf.pas',
  SimpleXmlImport in '..\..\..\Shared\Xml\SimpleXmlImport.pas',
  UnitVersionInfo in '..\..\..\Shared\IDE\UnitVersionInfo.pas',
  IDEUtils in '..\..\..\Shared\IDE\IDEUtils.pas',
  Hooking in '..\..\..\Shared\Hooking.pas',
  FrmOptions in '..\..\..\Shared\IDE\Options\FrmOptions.pas' {FormOptions},
  FrmTreePages in '..\..\..\Shared\IDE\Options\FrmTreePages.pas' {FormTreePages},
  FrmBase in '..\..\..\Shared\IDE\FrmBase.pas' {FormBase},
  HtHint in '..\..\..\Shared\IDE\HtHint.pas',
  ToolsAPIHelpers in '..\..\..\Shared\IDE\ToolsAPIHelpers.pas',
  FrmDDevExtOptions in '..\Source\FrmDDevExtOptions.pas' {FormDDevExtOptions},
  FileStreams in '..\..\..\Shared\FileStreams.pas',
  FrmSwitchToModuleProject in '..\Source\CompileProgress\FrmSwitchToModuleProject.pas' {FormSwitchToModuleProject},
  FocusEditor in '..\Source\Editor\FocusEditor.pas',
  StrucViewSearch in '..\Source\DSUFeatures\StrucViewSearch.pas',
  EditPopupCtrl in '..\Source\EditPopupCtrl.pas',
  VirtTreeHandler in '..\Source\VirtTreeHandler.pas',
  ImportHooking in '..\..\..\Shared\ImportHooking.pas',
  StartParameterClasses in '..\Source\StartParameterManager\StartParameterClasses.pas',
  StartParameterCtrl in '..\Source\StartParameterManager\StartParameterCtrl.pas',
  StartParameterManagerReg in '..\Source\StartParameterManager\StartParameterManagerReg.pas',
  FrmeOptionPageStartParameterTeam in '..\Source\StartParameterTeam\FrmeOptionPageStartParameterTeam.pas',
  CompilerClearOtherStates in '..\Source\CompileProgress\CompilerClearOtherStates.pas',
  TaskbarIntf in '..\Source\TaskbarIntf.pas',
  FrmReloadFiles in '..\Source\Editor\FrmReloadFiles.pas' {FormReloadFiles},
  DocModuleHandler in '..\Source\Editor\DocModuleHandler.pas',
  CodeInsightHandling in '..\Source\Editor\CodeInsightHandling.pas',
  DisableAlphaSortClassCompletion in '..\Source\DSUFeatures\DisableAlphaSortClassCompletion.pas';

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

procedure DoneWizard;
begin
  try
    try
      UninstallHooks;
    finally
      if AboutBoxServices <> nil then
      begin
        AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
        AboutBoxServices := nil;
      end;
    end;
  except
    on E: Exception do
      MessageBox(0, PChar(E.Message), PChar('DDevExtensions - ' + string(E.ClassName)), MB_OK or MB_ICONERROR);
  end;
end;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc; var Terminate: TWizardTerminateProc): Boolean; stdcall;
begin
  Terminate := DoneWizard;
  Result := True;
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then
  begin
    AboutBoxIndex := AboutBoxServices.AddPluginInfo(
      sPluginName,
      sPluginName + sLineBreak +
      sLineBreak +
      sPluginCopyright + sLineBreak +
      'Use at your own risk.',
      0
    );
  end;

  InstallHooks;
end;

exports
  InitWizard name WizardEntryPoint;

begin
  {$R ..\Version.res}
  ShowOnSplashScreen;
end.

