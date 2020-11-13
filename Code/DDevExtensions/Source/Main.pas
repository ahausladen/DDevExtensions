{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit Main;

{$I DelphiExtension.inc}

interface

uses
  Windows, SysUtils, Classes, Hooking, IDEUtils, Splash, Registry, Forms, Menus,
  Graphics, Controls, ExtCtrls, ActnList, AppConsts, ToolsAPI, Dialogs,
  StdCtrls, ComCtrls, ActiveX, ShlObj, ImportHooking;

type
  TLateLoaderProc = procedure(Unload: Boolean);

procedure InstallHooks;
procedure UninstallHooks;

procedure ExpertLoaded;
procedure IDELoaded; // called by Splash

procedure RegisterExpertLoader(Proc: TLateLoaderProc);
procedure RegisterLateLoader(Proc: TLateLoaderProc);
procedure InitAppDataDirectory; // called by InstallHooks

var
  APIHookList: TJclPeMapImgHooks;
  AppDataDirectory: string;

implementation

uses
  SysConst, IDEHooks, ComponentManager, DtmImages, FrmDDevExtOptions, RegisterPlugins,
  ToolsAPIHelpers, PluginConfig;

var
  HookFinalizePackage: TRedirectCode;
  OrgFinalizePackage: Pointer;
  LateLoaderList: TList;
  ExpertLoaderList: TList;
  MenuItemOptions: TMenuItem;

procedure RegisterExpertLoader(Proc: TLateLoaderProc);
begin
  if not Assigned(ExpertLoaderList) then
    ExpertLoaderList := TList.Create;
  ExpertLoaderList.Add(@Proc);
end;

procedure RegisterLateLoader(Proc: TLateLoaderProc);
begin
  if not Assigned(LateLoaderList) then
    LateLoaderList := TList.Create;
  LateLoaderList.Add(@Proc);
end;

procedure ShowOptionsDialog(Data: TObject; Sender: TObject);
begin
  TFormDDevExtOptions.Execute;
end;

procedure HookedFinalizePackage(Module: HMODULE);
type
  TPackageUnload = procedure;
var
  PackageUnload: TPackageUnload;
begin
  {.$IFNDEF COMPILER9_UP}
  if (Application <> nil) and not Application.Terminated then
    try
      RegisteredComponents.DeletePackageComponents(Module);
    except
    end;
  {.$ENDIF ~COMPILER9_UP}
  @PackageUnload := GetProcAddress(Module, 'Finalize'); //Do not localize
  if Assigned(PackageUnload) then
    PackageUnload
  else
    raise EPackageError.CreateRes(@sInvalidPackageHandle);
end;

procedure InitAppDataDirectory;
var
  Malloc: IMalloc;
  pidl: PItemIDList;
  Buffer: string;
begin
  AppDataDirectory := ExtractFileDir(ExtractFileDir((ParamStr(0))));
  { Windows 95 compatible way, Win98 supports SHGetSpecialFolderPath() }
  SHGetMalloc(Malloc);
  if SHGetSpecialFolderLocation(Application.Handle, CSIDL_APPDATA, pidl) = S_OK then
  begin
    try
      SetLength(Buffer, MAX_PATH * 2); // SHGetPathFromIDList has no MaxLen parameter
      if SHGetPathFromIDList(pidl, PChar(Buffer)) then
        AppDataDirectory := ExcludeTrailingPathDelimiter(Copy(Buffer, 1, StrLen(PChar(Buffer))));
    finally
      Malloc.Free(pidl);
      Malloc := nil;
    end;
  end;
  if AppDataDirectory = '' then
    AppDataDirectory := ExtractFileDir(ExtractFileDir((ParamStr(0))));
  AppDataDirectory := AppDataDirectory + '\DDevExtensions';
  ForceDirectories(AppDataDirectory);
end;

procedure InstallHooks;
begin
  InitAppDataDirectory;
  DataModuleImages := TDataModuleImages.Create(nil);

  OrgFinalizePackage := @SysUtils.FinalizePackage;
  if Assigned(OrgFinalizePackage) then
    CodeRedirect(OrgFinalizePackage, @HookedFinalizePackage, HookFinalizePackage);

  Configuration.BeginUpdate;
  try
    InitComponentManager;
    RegisterIDEPlugins;
    ExpertLoaded;
  finally
    Configuration.EndUpdate;
  end;
end;

procedure ExpertLoaded;
var
  I: Integer;
begin
  Configuration.BeginUpdate;
  try
    if Assigned(ExpertLoaderList) then
    begin
      for I := 0 to ExpertLoaderList.Count - 1 do
      begin
        try
          TLateLoaderProc(ExpertLoaderList[I])(False);
        except
          Application.HandleException(Application);
        end;
      end;
    end;
  finally
    Configuration.EndUpdate;
  end;
end;

procedure IDELoaded;
var
  I, Index: Integer;
  Item: TMenuItem;
begin
  MenuItemOptions := TMenuItem.Create(nil);
  MenuItemOptions.OnClick := MakeNotifyEvent(nil, @ShowOptionsDialog);
  MenuItemOptions.Caption := sMenuItemDDevExtensionsOptions;
  Item := FindMenuItem('ToolsMenu');
  if Item <> nil then
  begin
    Index := Item.IndexOf(FindMenuItem('ToolsDebuggerOptionsItem'));
    if Index = -1 then
      Index := Item.IndexOf(FindMenuItem('ToolsToolsItem')) - 1;
    if Index >= 0 then
      Item.Insert(Index + 1, MenuItemOptions)
    else
      Item.Insert(1, MenuItemOptions);
  end;

  Configuration.BeginUpdate;
  try
    if Assigned(LateLoaderList) then
    begin
      for I := 0 to LateLoaderList.Count - 1 do
      begin
        try
          TLateLoaderProc(LateLoaderList[I])(False);
        except
          Application.HandleException(Application);
        end;
      end;
    end;
  finally
    Configuration.EndUpdate;
  end;
end;

procedure UninstallHooks;
var
  I: Integer;
begin
  try
    if (LateLoaderList <> nil) or (ExpertLoaderList <> nil) then
    begin
      Configuration.BeginUpdate;
      try
        // LateLoader
        if LateLoaderList <> nil then
        begin
          for I := LateLoaderList.Count - 1 downto 0 do
          begin
            try
              TLateLoaderProc(LateLoaderList[I])(True);
            except
              Application.HandleException(Application);
            end;
          end;
          FreeAndNil(LateLoaderList);
        end;
        // ExpertLoader
        if ExpertLoaderList <> nil then
        begin
          for I := ExpertLoaderList.Count - 1 downto 0 do
          begin
            try
              TLateLoaderProc(ExpertLoaderList[I])(True);
            except
              Application.HandleException(Application);
            end;
          end;
          FreeAndNil(ExpertLoaderList);
        end;
      finally
        Configuration.EndUpdate;
      end;
    end;
  finally
    DoneSplash;

    if Assigned(OrgFinalizePackage) then
      CodeRestore(HookFinalizePackage);

    FiniComponentManager;

    FreeAndNil(DataModuleImages);
  end;
end;

end.
