{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit IDEMenuHandler;

{$I ..\DelphiExtension.inc}

interface

uses
  {$IFDEF COMPILER9_UP}
  ActnMan, ActnPopup,
  {$ENDIF COMPILER9_UP}
  Windows, Messages, SysUtils, Classes, Contnrs, Menus, ActnList, Forms, Controls,
  ToolsAPI;

type
  TIDEMenuHandler = class(TObject)
  private
    FMenuProject: TMenuItem;
    FMenuItems: TObjectList;
    FActionItems: TObjectList;

    {$IFDEF COMPILER9_UP}
    FOrgUpdateSwapSourceForm, FOrgExecuteSwapSourceForm: TNotifyEvent;
    FViewSwapSourceFormItem: TMenuItem;
    {$ENDIF COMPILER9_UP}
    {$IFDEF DELPHI2007_UP}
    FActionSetActiveBuildConfiguration: TAction;
    //FBuildConfigActions: TObjectList;
    {$ENDIF DELPHI2007_UP}

    FActionFileSelector: TAction;

  protected
    function AddMenuAction(const BaseMainItemName, BaseItemName, ItemName, Caption: string;
      Execute: TNotifyEvent; InsertAfter, InsertAsChild: Boolean; ShortCut: TShortCut = scNone): TAction;

    procedure ActionUpdate(Sender: TObject);

    {$IFDEF COMPILER9_UP}
    procedure ExecuteSwapSourceForm(Sender: TObject);
    procedure UpdateSwapSourceForm(Sender: TObject);
    {$ENDIF COMPILER9_UP}

    procedure FileSelectorItemClick(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    class procedure SetFindUseUnitHotKey(Value: TShortCut);
  end;

procedure InitPlugin(Unload: Boolean);

implementation

uses
  ToolsAPIHelpers, AppConsts, FrmFileSelector;

var
  GlobalIDEMenuHandler: TIDEMenuHandler;
  GlobalFindUseUnitHotKey: TShortCut = scNone;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
  begin
    GlobalIDEMenuHandler := TIDEMenuHandler.Create;
  end
  else
  begin
    FreeAndNil(GlobalIDEMenuHandler);
  end;
end;

procedure SetActionShortCut(Action: TAction; Value: TShortCut);
begin
  if Action <> nil then
  begin
    Action.ShortCut := Value;
    {$IFDEF COMPILER6_UP}
    {$IFNDEF COMPILER9_UP}
    // Delphi 5-7 delete the shortcut without invoking the TBaseAction.Changed method
    Action.SecondaryShortCuts.Clear;
    if Value <> scNone then
      Action.SecondaryShortCuts.Add(ShortCutToText(Value));
    {$ENDIF ~COMPILER9_UP}
    {$ENDIF COMPILER6_UP}
  end;
end;

class procedure TIDEMenuHandler.SetFindUseUnitHotKey(Value: TShortCut);
begin
  GlobalFindUseUnitHotKey := Value;
  if GlobalIDEMenuHandler <> nil then
    SetActionShortCut(GlobalIDEMenuHandler.FActionFileSelector, GlobalFindUseUnitHotKey);
end;

{ TIDEMenuHandler }

constructor TIDEMenuHandler.Create;
begin
  inherited Create;
  FMenuItems := TObjectList.Create;
  FActionItems := TObjectList.Create;

  FMenuProject := FindMenuItem('ProjectMenu');

  {$IFDEF COMPILER9_UP}
  {$IFNDEF DELPHI2007_UP}
  FViewSwapSourceFormItem := FindMenuItem('ViewSwapSourceFormItem');
  if (FViewSwapSourceFormItem <> nil) and (FViewSwapSourceFormItem.Action <> nil) then
  begin
    FOrgUpdateSwapSourceForm := FViewSwapSourceFormItem.Action.OnUpdate;
    FOrgExecuteSwapSourceForm := FViewSwapSourceFormItem.Action.OnExecute;
    FViewSwapSourceFormItem.Action.OnUpdate := UpdateSwapSourceForm;
    FViewSwapSourceFormItem.Action.OnExecute := ExecuteSwapSourceForm;
  end;
  {$ENDIF ~DELPHI2007_UP}
  {$ENDIF COMPILER9_UP}

  {$IFDEF INCLUDE_FILESELECTOR}
  if FindMenuItem('SearchGotoAddressItem') <> nil then
    FActionFileSelector := AddMenuAction('SearchMenu', 'SearchGotoAddressItem',
      'DDevExtFileSelectorItem', sMenuItemDDevExtensionsFileSelector,
      FileSelectorItemClick, True, False, GlobalFindUseUnitHotKey)
  else
    FActionFileSelector := AddMenuAction('SearchMenu', 'SearchGoToItem',
      'DDevExtFileSelectorItem', sMenuItemDDevExtensionsFileSelector,
      FileSelectorItemClick, True, False, GlobalFindUseUnitHotKey);
  {$ENDIF INCLUDE_FILESELECTOR}

  ActionUpdate(nil);
end;

destructor TIDEMenuHandler.Destroy;
begin
  {$IFDEF COMPILER9_UP}
  if Assigned(FViewSwapSourceFormItem) and Assigned(FViewSwapSourceFormItem.Action) then
  begin
    FViewSwapSourceFormItem.Action.OnUpdate := FOrgUpdateSwapSourceForm;
    FViewSwapSourceFormItem.Action.OnExecute := FOrgExecuteSwapSourceForm;
  end;
  {$ENDIF COMPILER9_UP}

  FMenuItems.Free;
  FActionItems.Free;
  inherited Destroy;
end;

procedure TIDEMenuHandler.FileSelectorItemClick(Sender: TObject);
begin
  {$IFDEF INCLUDE_FILESELECTOR}
  TFormFileSelector.Execute(True);
  {$ENDIF INCLUDE_FILESELECTOR}
end;

procedure TIDEMenuHandler.ActionUpdate(Sender: TObject);
var
  {$IFDEF DELPHI2007_UP}
  //ActiveConfig, Config: IBuildConfiguration;
  MenuProjectBuildConfigsItem: TMenuItem;
  i: Integer;
  {$ENDIF DELPHI2007_UP}
  IsDelphiPers, IsDelphiNetPers: Boolean;
begin
  {$IFDEF COMPILER6_UP}
  {$IFNDEF COMPILER9_UP}
  if Sender is TAction then
  begin
    // Delphi 5-7 delete the shortcut without invoking the TBaseAction.Changed method
    if (TAction(Sender).ShortCut = 0) and (TAction(Sender).SecondaryShortCuts.Count > 0) then
    begin
      TAction(Sender).ShortCut := TAction(Sender).SecondaryShortCuts.ShortCuts[0];
      TAction(Sender).SecondaryShortCuts.Clear;
    end;
  end;
  {$ENDIF ~COMPILER9_UP}
  {$ENDIF COMPILER6_UP}

  IsDelphiPers := IsDelphiPersonality(nil);
  IsDelphiNetPers := IsDelphiNetPersonality(nil);
  if Assigned(FActionFileSelector) then
  begin
    FActionFileSelector.Visible := IsDelphiPers or IsDelphiNetPers;
    FActionFileSelector.Enabled := FActionFileSelector.Visible;
  end;

  {$IFDEF DELPHI2007_UP}
  { The ProjectBuildConfigsItem is created by the CBuilder-Personality and that
    is too late for the PluginInit code. So we create the menu item here. }
  if Assigned(FMenuProject) and not Assigned(FActionSetActiveBuildConfiguration) then
  begin
    MenuProjectBuildConfigsItem := nil;
    for i := 0 to FMenuProject.Count - 1 do
    begin
      if FMenuProject.Items[i].Name = 'ProjectBuildConfigsItem' then
      begin
        MenuProjectBuildConfigsItem := FMenuProject.Items[i];
        Break;
      end;
    end;
    if Assigned(MenuProjectBuildConfigsItem) then
    begin
      FActionSetActiveBuildConfiguration := AddMenuAction('ProjectMenu', MenuProjectBuildConfigsItem.Name,
        'MenuProjectSetActiveBuildConfigItem', RsSetActiveBuildConfiguration, nil, False, False);
      FActionSetActiveBuildConfiguration.OnUpdate := ActionUpdate;
    end;
  end;

  if Assigned(FActionSetActiveBuildConfiguration) then
    FActionSetActiveBuildConfiguration.Visible := IsDelphiPers;

  if Assigned(FActionSetActiveBuildConfiguration) and (Sender = FActionSetActiveBuildConfiguration) then
  begin
    { Easier Build Configuration selection }
(*    ProjOpts := nil;
    if (ActiveProject <> nil) and not Active then
      ProjOpts := GetProjectOptions(ActiveProject);

    FActionSetActiveBuildConfiguration.Enabled := (ProjOpts <> nil) and (ProjOpts.GetBuildConfigurationCount > 1);

    TMenuItem(FActionSetActiveBuildConfiguration.Tag).Clear;
    FBuildConfigActions.Clear;
    if (ProjOpts <> nil) and FActionSetActiveBuildConfiguration.Enabled then
    begin
      { Build sub menu }
      ActiveConfig := ProjOpts.GetActiveConfiguration;
      for i := 0 to ProjOpts.GetBuildConfigurationCount - 1 do
      begin
        Config := ProjOpts.GetBuildConfiguration(i);
        SubAction := TAction.Create(nil);
        SubAction.Caption := Config.Name;
        SubAction.Hint := Config.Name;
        SubAction.Tag := i;
        SubAction.AutoCheck := True;
        SubAction.Checked := Config = ActiveConfig;
        SubAction.OnExecute := DoExecuteSetActiveBuildConfig;

        MenuItem := TMenuItem.Create(SubAction);
        MenuItem.Name := 'bcc32pch_MenuItemBuildConfig' + IntToStr(i);
        MenuItem.Action := SubAction;
        MenuItem.RadioItem := True;

        FBuildConfigActions.Add(SubAction);
        NServices.AddActionMenu(TMenuItem(FActionSetActiveBuildConfiguration.Tag).Name, SubAction, MenuItem, True, True);
      end;
    end;*)
  end

  {$ENDIF DELPHI2007_UP}
end;

function TIDEMenuHandler.AddMenuAction(const BaseMainItemName, BaseItemName, ItemName, Caption: string;
  Execute: TNotifyEvent; InsertAfter: Boolean; InsertAsChild: Boolean; ShortCut: TShortCut): TAction;
var
  MenuItem: TMenuItem;
  {$IFNDEF COMPILER9_UP}
  Item: TMenuItem;
  i: Integer;
  {$ELSE}
  NServices: INTAServices;
  {$ENDIF ~COMPILER9_UP}
begin
  Result := TAction.Create(nil);
  Result.Name := 'Action' + ItemName;
  Result.Caption := Caption;
  SetActionShortCut(Result, ShortCut);
  Result.OnExecute := Execute;
  Result.OnUpdate := ActionUpdate;

  MenuItem := TMenuItem.Create(Result);
  MenuItem.Name := ItemName;
  MenuItem.Action := Result;
  MenuItem.ShortCut := ShortCut;
  Result.Tag := NativeInt(MenuItem);

  FActionItems.Add(Result);
  FMenuItems.Add(MenuItem);

  {$IFDEF COMPILER9_UP}
  NServices := BorlandIDEServices as INTAServices;
  NServices.AddActionMenu(BaseItemName, Result, MenuItem, InsertAfter, InsertAsChild);
  {$ELSE}
  Item := nil;
  for i := 0 to FMenuItems.Count - 1 do
  begin
    if CompareText(TMenuItem(FMenuItems[i]).Name, BaseItemName) = 0 then
    begin
      Item := TMenuItem(FMenuItems[i]);
      Break;
    end;
  end;

  if Item = nil then
  begin
    Item := TMenuItem(Application.MainForm.FindComponent(BaseItemName));
    if not (TObject(Item) is TMenuItem) then
    begin
      Item := TMenuItem(Application.MainForm.FindComponent(BaseMainItemName));
      if not (TObject(Item) is TMenuItem) then
        Item := nil;
      InsertAsChild := True;
    end;
  end;

  if Item <> nil then
  begin
    if InsertAsChild then
    begin
      if InsertAfter then
        Item.Add(MenuItem)
      else
        Item.Insert(0, MenuItem);
    end
    else
      Item.Parent.Insert(Item.Parent.IndexOf(Item) + Ord(InsertAfter), MenuItem);

    Result.ActionList := (BorlandIDEServices as INTAServices).ActionList;
    Result.ShortCut := ShortCut;
  end
  else
    raise Exception.CreateFmt('Menu item "%s" not found.', [BaseItemName]);
  {$ENDIF COMPILER9_UP}
end;


{$IFDEF COMPILER9_UP}
procedure TIDEMenuHandler.ExecuteSwapSourceForm(Sender: TObject);
const
  AltMask = $20000000;
var
  i: Integer;
  EditorLocalMenu: TPopupActionBar;
  Msg: TWMKey;
  Shift: TShiftState;
begin
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i].Visible and Screen.Forms[i].Enabled and
       Screen.Forms[i].CanFocus and
       Screen.Forms[i].ClassNameIs('TEditWindow') then
    begin
      EditorLocalMenu := TPopupActionBar(Screen.Forms[i].FindComponent('EditorLocalMenu'));
      if EditorLocalMenu <> nil then
      begin
        Msg.Msg := WM_KEYDOWN;
        ShortCutToKey(TAction(Sender).ShortCut, Msg.CharCode, Shift);
        if ssAlt in Shift then
          Msg.KeyData := AltMask;
        EditorLocalMenu.IsShortCut(Msg);
      end;
      Break;
    end;
  end;
end;

procedure TIDEMenuHandler.UpdateSwapSourceForm(Sender: TObject);
begin
  FOrgUpdateSwapSourceForm(Sender);
  TAction(Sender).Enabled := True;
end;
{$ENDIF COMPILER9_UP}


end.

