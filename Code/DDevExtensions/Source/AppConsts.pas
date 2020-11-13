{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2014 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit AppConsts;

interface

const
  {$I Version.inc}

  sPluginVersion = VersionNumber;
  sPluginName = 'DDevExtensions ' + sPluginVersion;
  sPluginSmallCopyright = '(C) 2006-2020 Andreas Hausladen';
  sPluginCopyright = 'Copyright ' + sPluginSmallCopyright;

resourcestring
  RsFilterAllFields = 'All columns';
  RsNumberOfModules = '%d modules';
  RsFilterAllDirectories = 'All directories';

  RsSetActiveBuildConfiguration = 'Active Build Configuration'; 
  
const
  // English
  sSearchComponent_Eng = '(search component)';
  sFilesCompiled_Eng = '%d files compiled';
  sAutoCloseCaption_Eng = '&Automatically close on successful compile';
  sMenuItemManageProjectSettings_Eng = 'Manage Configurations...';
  sMenuItemProjectSettings_Eng = 'Project Configurations';
  sMenuItemSetVersionInfo_Eng = 'Set Versioninfo...';
  sMenuItemDDevExtensionsOptions_Eng = 'DDevExtensions options...';
  sMenuItemDDevExtensionsFileSelector_Eng = 'Find Unit-File...';
  sParseErrorUsesLocationNotFound_Eng = 'Parser Error: Failed to find the position for "uses"';

  sCapSwitchToModuleProject_Eng = 'Compile/Build - Switch Active Project';
  sLblSwitchCurrentModuleProject_Eng = 'The module is neither part of the active project nor is it in a direct or indirect dependent project.';
  sLblSwitchToModuleProjectQuestion_Eng = 'Do you want to &switch to the module''s project?';
  sLblDontShowAgain_Eng = '&Don''t show again';
  sLblTemporarySwitch_Eng = 'Switch &temporary [Shift-Key]';
  sLblActiveProject_Eng = 'Active Project:';
  sLblActiveModule_Eng = 'Active Module:';

  sDoYouWantToInvokeTheContextHelp_Eng = 'Do you want to invoke the context help?';

  sCannotUnloadModuleForm_Eng = 'Cannot unload form ''%s''';
  sLVGroup_ProjectFiles_Eng = 'Project files';
  sLVGroup_UnitFiles_Eng = 'Units/Files';
  sLVGroup_Forms_Eng = 'Forms/Frames/DataModules';
  sReloadButton_Eng = '&Reload';
  sReloadChangedFilesCaption_Eng = 'Reload changed files';
  sLVColumn_File_Eng = 'File';
  sLVColumn_Path_Eng = 'Path';

  sReloadSelectOnlyUnmodifiedField_Eng = 'Select &unmodified buffers';
  sReloadSelectOnlyModifiedFiles_Eng = 'Select &modified buffers';
  sReloadSelectAll_Eng = 'Select &all';
  sReloadDeselectAll_Eng = '&Deselect all';
  sReloadInvertSelection_Eng = '&Invert selection';
  sReloadShowInExplorer_Eng = 'Show in &Explorer';

  // German
  sSearchComponent_Ger = '(Komponente suchen)';
  sFilesCompiled_Ger = '%d Dateien compiliert';
  sAutoCloseCaption_Ger = '&Nach erfolgreicher Compilierung automatisch schließen';
  sMenuItemManageProjectSettings_Ger = 'Einstellungen...';
  sMenuItemProjectSettings_Ger = 'Projekt Konfigurationen';
  sMenuItemSetVersionInfo_Ger = 'Versionsinfo setzen...';
  sMenuItemDDevExtensionsOptions_Ger = 'DDevExtensions Optionen...';
  sMenuItemDDevExtensionsFileSelector_Ger = 'Unit-Datei suchen...';
  sParseErrorUsesLocationNotFound_Ger = 'Parser Fehler: Position zum Einfügen des "uses" konnte nicht ermittelt werden';

  sCapSwitchToModuleProject_Ger = 'Compilieren/Erzeugen - Aktives Projekt wechseln';
  sLblSwitchCurrentModuleProject_Ger = 'Die Datei gehört weder zum aktiven Projekt noch zu einem direkt oder indirekt abhängigen Projekt.';
  sLblSwitchToModuleProjectQuestion_Ger = '&Soll zum Projekt der Datei gewechselt werden?';
  sLblDontShowAgain_Ger = '&Nicht mehr anzeigen';
  sLblTemporarySwitch_Ger = '&Temporär wechseln [Umschalt-Taste]';
  sLblActiveProject_Ger = 'Aktives Projekt:';
  sLblActiveModule_Ger = 'Aktive Datei:';

  sDoYouWantToInvokeTheContextHelp_Ger = 'Soll die Kontexthilfe aufgerufen werden?';

  sCannotUnloadModuleForm_Ger = 'Formular kann nicht entladen werden: %s';
  sLVGroup_ProjectFiles_Ger = 'Projektdateien';
  sLVGroup_UnitFiles_Ger = 'Units/Dateien';
  sLVGroup_Forms_Ger = 'Formulare/Frames/Datenmodule';
  sReloadButton_Ger = '&Neuladen';
  sReloadChangedFilesCaption_Ger = 'Veränderte Dateien neuladen';
  sLVColumn_File_Ger = 'Datei';
  sLVColumn_Path_Ger = 'Pfad';

  sReloadSelectOnlyUnmodifiedField_Ger = 'Unveränderte Puffer auswählen';
  sReloadSelectOnlyModifiedFiles_Ger = '&Veränderte Puffer auswählen';
  sReloadSelectAll_Ger = '&Alle auswählen';
  sReloadDeselectAll_Ger = 'A&uswahl aufheben';
  sReloadInvertSelection_Ger = 'Auswahl um&kehren';
  sReloadShowInExplorer_Ger = 'Im &Explorer anzeigen';

  // French
  sSearchComponent_Fra = '(chercher un composant)';
  sFilesCompiled_Fra = '%d fichiers compilés';
  sAutoCloseCaption_Fra = 'Fermer &Automatiquement à la réussite de la compilation';
  sMenuItemManageProjectSettings_Fra = 'Gérer les configurations...';
  sMenuItemProjectSettings_Fra = 'Configurations du projet';
  sMenuItemSetVersionInfo_Fra = 'Affecter les infos de version...';
  sMenuItemDDevExtensionsOptions_Fra = 'Options DDevExtensions...';
  sMenuItemDDevExtensionsFileSelector_Fra = 'Trouver le fichier unité...';
  sParseErrorUsesLocationNotFound_Fra = 'Erreur du Parser : Impossible de trouver la position de "uses"';

  sCapSwitchToModuleProject_Fra = 'Compiler/Construire - Changer de projet actif';
  sLblSwitchCurrentModuleProject_Fra = 'Le module ne fait partie ni du projet actif ni d''un projet dépendant directement ou indirectement';
  sLblSwitchToModuleProjectQuestion_Fra = 'Voulez-vous pa&sser sur le projet de ce module?';
  sLblDontShowAgain_Fra = '&Ne plus afficher';
  sLblTemporarySwitch_Fra = 'Changer &temporairement [Touche Maj]';
  sLblActiveProject_Fra = 'Projet actif:';
  sLblActiveModule_Fra = 'Module actif:';

  sDoYouWantToInvokeTheContextHelp_Fra = sDoYouWantToInvokeTheContextHelp_Eng;

  sCannotUnloadModuleForm_Fra = sCannotUnloadModuleForm_Eng;
  sLVGroup_ProjectFiles_Fra = sLVGroup_ProjectFiles_Eng;
  sLVGroup_UnitFiles_Fra = sLVGroup_UnitFiles_Eng;
  sLVGroup_Forms_Fra = sLVGroup_Forms_Eng;
  sReloadButton_Fra = sReloadButton_Eng;
  sReloadChangedFilesCaption_Fra = sReloadChangedFilesCaption_Eng;
  sLVColumn_File_Fra = sLVColumn_File_Eng;
  sLVColumn_Path_Fra = sLVColumn_Path_Eng;

  sReloadSelectOnlyUnmodifiedField_Fra = sReloadSelectOnlyUnmodifiedField_Eng;
  sReloadSelectOnlyModifiedFiles_Fra = sReloadSelectOnlyModifiedFiles_Eng;
  sReloadSelectAll_Fra = sReloadSelectAll_Eng;
  sReloadDeselectAll_Fra = sReloadDeselectAll_Eng;
  sReloadInvertSelection_Fra = sReloadInvertSelection_Eng;
  sReloadShowInExplorer_Fra = sReloadShowInExplorer_Eng;


function sSearchComponent: string;
function sFilesCompiled: string;
function sAutoCloseCaption: string;
function sMenuItemManageProjectSettings: string;
function sMenuItemProjectSettings: string;
function sMenuItemSetVersionInfo: string;
function sMenuItemDDevExtensionsOptions: string;
function sMenuItemDDevExtensionsFileSelector: string;
function sParseErrorUsesLocationNotFound: string;

function sCapSwitchToModuleProject: string;
function sLblSwitchCurrentModuleProject: string;
function sLblSwitchToModuleProjectQuestion: string;
function sLblDontShowAgain: string;
function sLblTemporarySwitch: string;
function sLblActiveProject: string;
function sLblActiveModule: string;

function sDoYouWantToInvokeTheContextHelp: string;

// ReloadFiles
function sCannotUnloadModuleForm: string;
function sLVGroup_ProjectFiles: string;
function sLVGroup_UnitFiles: string;
function sLVGroup_Forms: string;
function sReloadButton: string;
function sReloadChangedFilesCaption: string;
function sLVColumn_File: string;
function sLVColumn_Path: string;

function sReloadSelectOnlyUnmodifiedField: string;
function sReloadSelectOnlyModifiedFiles: string;
function sReloadSelectAll: string;
function sReloadDeselectAll: string;
function sReloadInvertSelection: string;
function sReloadShowInExplorer: string;



function GetLang: Cardinal;
function _(const S: WideString): string;

implementation

uses
  Windows, SysUtils;

var
  Lang: Cardinal = Cardinal(-1);

function _(const S: WideString): string;
begin
  // placeholder for dxgettext
  Result := S;
end;

function GetLang: Cardinal;
var
  App: string;
begin
  if Lang = Cardinal(-1) then
  begin
    App := GetEnvironmentVariable('LANGDIR');
    if SameText(App, 'de') then
      Lang := LANG_GERMAN
    else if SameText(App, 'fr') or SameText(App, 'fra') then
      Lang := LANG_FRENCH
    else
      Lang := LANG_ENGLISH;
    {App := ChangeFileExt(ParamStr(0), '');
    case SysLocale.PriLangID of
      LANG_GERMAN:
        if GetFileAttributes(PChar(App + '.de')) and FILE_ATTRIBUTE_DIRECTORY = 0 then
          Lang := LANG_GERMAN;
      LANG_FRENCH:
        if GetFileAttributes(PChar(App + '.fra')) and FILE_ATTRIBUTE_DIRECTORY = 0 then
          Lang := LANG_FRENCH;
        else
        if GetFileAttributes(PChar(App + '.fr')) and FILE_ATTRIBUTE_DIRECTORY = 0 then
          Lang := LANG_FRENCH;
    end;}
  end;
  Result := Lang;
end;

function FromLang(const Eng, Ger, Fra: string): string;
begin
  case GetLang of
    LANG_GERMAN: Result := Ger;
    LANG_FRENCH: Result := Fra;
  else
    Result := Eng;
  end;
end;


function sSearchComponent: string;
begin
  Result := FromLang(sSearchComponent_Eng,
                     sSearchComponent_Ger,
                     sSearchComponent_Fra);
end;

function sFilesCompiled: string;
begin
  Result := FromLang(sFilesCompiled_Eng,
                     sFilesCompiled_Ger,
                     sFilesCompiled_Fra);
end;

function sAutoCloseCaption: string;
begin
  Result := FromLang(sAutoCloseCaption_Eng,
                     sAutoCloseCaption_Ger,
                     sAutoCloseCaption_Fra);
end;

function sMenuItemManageProjectSettings: string;
begin
  Result := FromLang(sMenuItemManageProjectSettings_Eng,
                     sMenuItemManageProjectSettings_Ger,
                     sMenuItemManageProjectSettings_Fra);
end;

function sMenuItemProjectSettings: string;
begin
  Result := FromLang(sMenuItemProjectSettings_Eng,
                     sMenuItemProjectSettings_Ger,
                     sMenuItemProjectSettings_Fra);
end;

function sMenuItemSetVersionInfo: string;
begin
  Result := FromLang(sMenuItemSetVersionInfo_Eng,
                     sMenuItemSetVersionInfo_Ger,
                     sMenuItemSetVersionInfo_Fra);
end;

function sMenuItemDDevExtensionsOptions: string;
begin
  Result := FromLang(sMenuItemDDevExtensionsOptions_Eng,
                     sMenuItemDDevExtensionsOptions_Ger,
                     sMenuItemDDevExtensionsOptions_Fra);
end;

function sMenuItemDDevExtensionsFileSelector: string;
begin
  Result := FromLang(sMenuItemDDevExtensionsFileSelector_Eng,
                     sMenuItemDDevExtensionsFileSelector_Ger,
                     sMenuItemDDevExtensionsFileSelector_Fra);
end;

function sParseErrorUsesLocationNotFound: string;
begin
  Result := FromLang(sParseErrorUsesLocationNotFound_Eng,
                     sParseErrorUsesLocationNotFound_Ger,
                     sParseErrorUsesLocationNotFound_Fra);
end;

function sCapSwitchToModuleProject: string;
begin
  Result := FromLang(sCapSwitchToModuleProject_Eng,
                     sCapSwitchToModuleProject_Ger,
                     sCapSwitchToModuleProject_Fra);
end;

function sLblSwitchCurrentModuleProject: string;
begin
  Result := FromLang(sLblSwitchCurrentModuleProject_Eng,
                     sLblSwitchCurrentModuleProject_Ger,
                     sLblSwitchCurrentModuleProject_Fra);
end;

function sLblSwitchToModuleProjectQuestion: string;
begin
  Result := FromLang(sLblSwitchToModuleProjectQuestion_Eng,
                     sLblSwitchToModuleProjectQuestion_Ger,
                     sLblSwitchToModuleProjectQuestion_Fra);
end;

function sLblDontShowAgain: string;
begin
  Result := FromLang(sLblDontShowAgain_Eng,
                     sLblDontShowAgain_Ger,
                     sLblDontShowAgain_Fra);
end;

function sLblActiveProject: string;
begin
  Result := FromLang(sLblActiveProject_Eng,
                     sLblActiveProject_Ger,
                     sLblActiveProject_Fra);
end;

function sLblActiveModule: string;
begin
  Result := FromLang(sLblActiveModule_Eng,
                     sLblActiveModule_Ger,
                     sLblActiveModule_Fra);
end;

function sLblTemporarySwitch: string;
begin
  Result := FromLang(sLblTemporarySwitch_Eng,
                     sLblTemporarySwitch_Ger,
                     sLblTemporarySwitch_Fra);
end;

function sDoYouWantToInvokeTheContextHelp: string;
begin
  Result := FromLang(sDoYouWantToInvokeTheContextHelp_Eng,
                     sDoYouWantToInvokeTheContextHelp_Ger,
                     sDoYouWantToInvokeTheContextHelp_Fra);
end;

function sCannotUnloadModuleForm: string;
begin
  Result := FromLang(sCannotUnloadModuleForm_Eng,
                     sCannotUnloadModuleForm_Ger,
                     sCannotUnloadModuleForm_Fra);
end;

function sLVGroup_ProjectFiles: string;
begin
  Result := FromLang(sLVGroup_ProjectFiles_Eng,
                     sLVGroup_ProjectFiles_Ger,
                     sLVGroup_ProjectFiles_Fra);
end;

function sLVGroup_UnitFiles: string;
begin
  Result := FromLang(sLVGroup_UnitFiles_Eng,
                     sLVGroup_UnitFiles_Ger,
                     sLVGroup_UnitFiles_Fra);
end;

function sLVGroup_Forms: string;
begin
  Result := FromLang(sLVGroup_Forms_Eng,
                     sLVGroup_Forms_Ger,
                     sLVGroup_Forms_Fra);
end;

function sReloadButton: string;
begin
  Result := FromLang(sReloadButton_Eng,
                     sReloadButton_Ger,
                     sReloadButton_Fra);
end;

function sReloadChangedFilesCaption: string;
begin
  Result := FromLang(sReloadChangedFilesCaption_Eng,
                     sReloadChangedFilesCaption_Ger,
                     sReloadChangedFilesCaption_Fra);
end;

function sLVColumn_File: string;
begin
  Result := FromLang(sLVColumn_File_Eng,
                     sLVColumn_File_Ger,
                     sLVColumn_File_Fra);
end;

function sLVColumn_Path: string;
begin
  Result := FromLang(sLVColumn_Path_Eng,
                     sLVColumn_Path_Ger,
                     sLVColumn_Path_Fra);
end;

function sReloadSelectOnlyUnmodifiedField: string;
begin
  Result := FromLang(sReloadSelectOnlyUnmodifiedField_Eng,
                     sReloadSelectOnlyUnmodifiedField_Ger,
                     sReloadSelectOnlyUnmodifiedField_Fra);
end;

function sReloadSelectOnlyModifiedFiles: string;
begin
  Result := FromLang(sReloadSelectOnlyModifiedFiles_Eng,
                     sReloadSelectOnlyModifiedFiles_Ger,
                     sReloadSelectOnlyModifiedFiles_Fra);
end;

function sReloadSelectAll: string;
begin
  Result := FromLang(sReloadSelectAll_Eng,
                     sReloadSelectAll_Ger,
                     sReloadSelectAll_Fra);
end;

function sReloadDeselectAll: string;
begin
  Result := FromLang(sReloadDeselectAll_Eng,
                     sReloadDeselectAll_Ger,
                     sReloadDeselectAll_Fra);
end;

function sReloadInvertSelection: string;
begin
  Result := FromLang(sReloadInvertSelection_Eng,
                     sReloadInvertSelection_Ger,
                     sReloadInvertSelection_Fra);
end;

function sReloadShowInExplorer: string;
begin
  Result := FromLang(sReloadShowInExplorer_Eng,
                     sReloadShowInExplorer_Ger,
                     sReloadShowInExplorer_Fra);
end;


end.
