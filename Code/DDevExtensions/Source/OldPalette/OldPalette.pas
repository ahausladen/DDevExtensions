{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit OldPalette;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Contnrs, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComponentPanel,
  {$IFNDEF ALWAYS_DEFINED} // trick to get the IDE to not add those units again
  JvExExtCtrls, JvExtComponent, JvComponentPanel
  {$ENDIF}
  FrmeOptionPageOldPalette, Buttons, TypInfo, IniFiles, CategoryButtons, PaletteAPI,
  GraphUtil, StdCtrls, Menus, ToolWin, ComCtrls,
  ToolsAPI, ToolsAPIHelpers;

const
  WM_REBUILD = WM_USER + 1;

type
  TTabItem = class(TObject)
  private
    FComponents: TObjectList;
    FPalGroup: TButtonCategory;
    FCaption: string;
  public
    constructor Create(APalGroup: TButtonCategory);
    destructor Destroy; override;

    property Caption: string read FCaption;
    property PalGroup: TButtonCategory read FPalGroup;
    property Components: TObjectList read FComponents;
  end;

  TCompItem = class(TObject)
  private
    FUnitName: string;
    FName: string;
    FPalette: string;
    FPalItem: TButtonItem;
    FPalGroup: TButtonCategory;
    FComponentClass: TComponentClass;
    FhInst: HINST;
    FModuleName: string;
  public
    property Name: string read FName write FName;
    property Palette: string read FPalette write FPalette;

    property PalItem: TButtonItem read FPalItem write FPalItem;
    property PalGroup: TButtonCategory read FPalGroup write FPalGroup;

    property ComponentClass: TComponentClass read FComponentClass write FComponentClass;
    property CompUnitName: string read FUnitName write FUnitName;
    property hInst: HINST read FhInst write FhInst;
    property ModuleName: string read FModuleName write FModuleName;
  end;

  TFrameOldPalette = class(TFrame)
    Palette: TJvComponentPanel;
    PanelSpacer: TPanel;
    PopupMenuPalette: TPopupMenu;
    PanelPalette: TPanel;
    TabControl: TTabControl;
    procedure PaletteClick(Sender: TObject; Button: Integer);
    procedure PaletteDblClick(Sender: TObject; Button: Integer);
    procedure PaletteMenuItemClick(Sender: TObject);
    procedure TabBarPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PalettePaintContent(Sender: TObject; Canvas: TCanvas; R: TRect);
    procedure TabControlChange(Sender: TObject);
    procedure TabControlResize(Sender: TObject);
  private
    { Private-Deklarationen }
    FIsFormDesigner: Boolean;
    FConfig: TOldPaletteConfig;
    FDblClicked: TCompItem;
    FModified: Boolean;
    FRebuilding: Integer;
    FLockTabSelected: Boolean;
    FDesignerSelectedPaletteName: string;
    FEditorSelectedPaletteName: string;
    FOrgTabControlWndProc: TWndMethod;
    FRebuildDelay: Boolean;

    FOrgSelectedItemChange: TCatButtonEvent;
    FOrgReorderButton: TCatButtonReorderEvent;
    FOrgReorderCategory: TCategoryReorderEvent;
    procedure PaletteSelectedItemChange(Sender: TObject; const Button: TButtonItem);
    procedure PaletteReorderButton(Sender: TObject; const Button: TButtonItem;
      const SourceCategory, TargetCategory: TButtonCategory);
    procedure PaletteReorderCategory(Sender: TObject; const SourceCategory, TargetCategory: TButtonCategory);
    function GetSelectedTab: TTabItem;
    procedure UpdateIsFormDesigner(Categories: TButtonCategories);
    procedure ClearTabItemComponents;
    function GetTabItem(Index: Integer): TTabItem; inline;
    function GetTabItemCount: Integer; inline;
  protected
    procedure ReleaseToolForm;
    procedure ClearTabs;
    procedure ChangeTabIndex(NewTabIndex: Integer);
    procedure TabSelected(ComponentList: TObjectList);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMRebuild(var Msg: TMessage); message WM_REBUILD;
    procedure TabControlWndProc(var Msg: TMessage);
    procedure CreateWnd; override;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property TabItemCount: Integer read GetTabItemCount;
    property TabItems[Index: Integer]: TTabItem read GetTabItem;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(AConfig: TOldPaletteConfig);
    procedure InitTabControl(AConfig: TOldPaletteConfig);
    procedure Rebuild(ReorderOnly: Boolean = False);
    procedure RebuildDelayed;
    procedure RebuildPaletteMenu;
    procedure SelectComponent(PalGroup: TButtonCategory; PalItem: TButtonItem);
    property Config: TOldPaletteConfig read FConfig;
  end;

var
  FrameOldPalette: TFrameOldPalette;

implementation

uses
  IDEUtils, HtHint, Hooking, IDEHooks, ComponentManager, StrUtils;

{$R *.dfm}

var
  HookTToolForm_LoadPalette: TRedirectCode;

procedure TToolForm_LoadPalette(Instance: TForm);
  external coreide_bpl name '@Toolfrm@TToolForm@LoadPalette$qqrv';

procedure HookedTToolForm_LoadPalette(Instance: TForm);
begin
  CodeRestore(HookTToolForm_LoadPalette);
  Inc(FrameOldPalette.FRebuilding);
  try
    TToolForm_LoadPalette(Instance);
  finally
    Dec(FrameOldPalette.FRebuilding);
    RehookFunction(@HookedTToolForm_LoadPalette, HookTToolForm_LoadPalette);
  end;
  if Assigned(FrameOldPalette) then
    FrameOldPalette.RebuildDelayed;
end;

{ TTabItem }

constructor TTabItem.Create(APalGroup: TButtonCategory);
begin
  inherited Create;
  FPalGroup := APalGroup;
  FCaption := PalGroup.Caption;
  FComponents := TObjectList.Create;
end;

destructor TTabItem.Destroy;
begin
  FComponents.Free;
  inherited Destroy;
end;

{ TFrameOldPalette }

constructor TFrameOldPalette.Create(AOwner: TComponent);
var
  CompPalette: TCategoryButtons;
begin
  inherited Create(AOwner);
  Visible := False;
  FOrgTabControlWndProc := TabControl.WindowProc;
  TabControl.WindowProc := TabControlWndProc;

  Palette.HintWindowClass := THtHintWindow;
  ParentBackground := False;
  ControlStyle := ControlStyle + [csOpaque] - [csParentBackground];

  FrameOldPalette := Self;

  Palette.ButtonLeft.NoBorder := False;
  Palette.ButtonRight.NoBorder := False;

  CodeRedirect(@TToolForm_LoadPalette, @HookedTToolForm_LoadPalette, HookTToolForm_LoadPalette);

  CompPalette := GetComponentPalette;
  if Assigned(CompPalette) then
  begin
    FOrgSelectedItemChange := CompPalette.OnSelectedItemChange;
    CompPalette.OnSelectedItemChange := PaletteSelectedItemChange;

    FOrgReorderButton := CompPalette.OnReorderButton;
    CompPalette.OnReorderButton := PaletteReorderButton;

    FOrgReorderCategory := CompPalette.OnReorderCategory;
    CompPalette.OnReorderCategory := PaletteReorderCategory;

    CompPalette.FreeNotification(Self);
  end;
end;

procedure TFrameOldPalette.CreateWnd;
begin
  inherited CreateWnd;
  FRebuildDelay := False;
end;

destructor TFrameOldPalette.Destroy;
begin
  ReleaseToolForm;
  FrameOldPalette := nil;
  inherited Destroy;
end;

procedure TFrameOldPalette.ChangeTabIndex(NewTabIndex: Integer);
begin
  if (TabControl.TabIndex <> NewTabIndex) and
     (NewTabIndex >= 0) and (NewTabIndex < TabItemCount) then
  begin
    TabControl.TabIndex := NewTabIndex;
    TabControlChange(TabControl);
  end;
end;

procedure TFrameOldPalette.ClearTabItemComponents;
var
  i: Integer;
begin
  for i := 0 to TabItemCount - 1 do
    TabItems[i].Components.Clear;
  TabSelected(nil);
end;

procedure TFrameOldPalette.ClearTabs;
var
  i: Integer;
begin
  for i := 0 to TabItemCount - 1 do
    TabItems[i].Free;
  TabControl.Tabs.Clear;
end;

function TFrameOldPalette.GetTabItem(Index: Integer): TTabItem;
begin
  Result := TTabItem(TabControl.Tabs.Objects[Index]);
end;

function TFrameOldPalette.GetTabItemCount: Integer;
begin
  Result := TabControl.Tabs.Count;
end;

procedure TFrameOldPalette.ReleaseToolForm;
var
  CompPalette: TCategoryButtons;
begin
  CompPalette := GetComponentPalette;
  if Assigned(CompPalette) then
  begin
    CompPalette.RemoveFreeNotification(Self);

    CompPalette.OnSelectedItemChange := FOrgSelectedItemChange;
    CompPalette.OnReorderButton := FOrgReorderButton;
    CompPalette.OnReorderCategory := FOrgReorderCategory;
  end;
  if GetModuleHandle(coreide_bpl) <> 0 then
    CodeRestore(HookTToolForm_LoadPalette);

  ClearTabs;
  Palette.ButtonCount := 0;
end;

procedure TFrameOldPalette.Init(AConfig: TOldPaletteConfig);
begin
  FConfig := AConfig;
  InitTabControl(FConfig);
  Rebuild;
end;

procedure TFrameOldPalette.InitTabControl(AConfig: TOldPaletteConfig);
begin
  Inc(FRebuilding);
  try
    TabControl.Style := AConfig.Style;
    TabControl.RaggedRight := AConfig.RaggedRight;
    TabControl.MultiLine := AConfig.MultiLine;
    if AConfig.SmallFonts then
    begin
      TabControl.Font.Name := 'Small Fonts';
      TabControl.Font.Size := 7;
    end
    else
    begin
      TabControl.Font.Name := 'Tahoma';
      TabControl.Font.Size := 8;
    end;
  finally
    Dec(FRebuilding);
  end;
  if TabItemCount > 0 then
    TabControlResize(TabControl);
end;

procedure TFrameOldPalette.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent <> nil) then
  begin
    if AComponent = GetComponentPalette then
      ReleaseToolForm;
  end;
end;

procedure TFrameOldPalette.PaletteMenuItemClick(Sender: TObject);
begin
  ChangeTabIndex(TMenuItem(Sender).Tag);
end;

procedure TFrameOldPalette.PalettePaintContent(Sender: TObject; Canvas: TCanvas; R: TRect);
{$IFDEF COMPILER10_UP}
var
  ControlBar: TControlBar;
{$ENDIF COMPILER10_UP}
begin
  if TabControl.Style <> tsTabs then
  begin
    {$IFDEF COMPILER10_UP}
    ControlBar := TControlBar(Application.MainForm.FindChildControl('ControlBar1'));
    if (ControlBar <> nil) and (ControlBar.DrawingStyle = ExtCtrls.dsGradient) then
    begin
      R.Top := -PanelPalette.Top;
      GradientFillCanvas(Canvas, ControlBar.GradientStartColor, ControlBar.GradientEndColor, R, ControlBar.GradientDirection);
    end;
    {$ELSE}
    //GradientFillCanvas(Canvas, clWindow, $00B6D6DA, R, gdVertical);
    {$ENDIF COMPILER10_UP}
  end;
end;

function TFrameOldPalette.GetSelectedTab: TTabItem;
begin
  Result := nil;
  if TabControl.TabIndex <> -1 then
    Result := TabItems[TabControl.TabIndex];
end;

procedure TFrameOldPalette.PaletteClick(Sender: TObject; Button: Integer);
var
  Item: TCompItem;
begin
  if Button >= 0 then
    Item := TCompItem(Palette.Buttons[Button].Tag)
  else
    Item := nil;

  if FDblClicked = Item then
  begin
    FDblClicked := nil;
    SelectComponentPalette(nil, False);
    Palette.SetMainButton;
  end
  else
  if Item <> nil then
    SelectComponentPalette(Item.FPalItem, False)
  else
    SelectComponentPalette(nil, False);
end;

procedure TFrameOldPalette.PaletteDblClick(Sender: TObject; Button: Integer);
var
  Item: TCompItem;
begin
  if Button >= 0 then
    Item := TCompItem(Palette.Buttons[Button].Tag)
  else
    Item := nil;
  FDblClicked := Item;

  if Item <> nil then
    SelectComponentPalette(Item.PalItem, True)
  else
    SelectComponentPalette(nil, False);
end;

procedure TFrameOldPalette.PaletteSelectedItemChange(Sender: TObject; const Button: TButtonItem);
begin
  if Assigned(FOrgSelectedItemChange) then
    FOrgSelectedItemChange(Sender, Button);

  if not Application.Terminated then
    SelectComponent(nil, Button);
end;

procedure TFrameOldPalette.PaletteReorderButton(Sender: TObject; const Button: TButtonItem;
  const SourceCategory, TargetCategory: TButtonCategory);
begin
  if Assigned(FOrgReorderButton) then
    FOrgReorderButton(Sender, Button, SourceCategory, TargetCategory);
  if GetSelectedTab <> nil then
    TabSelected(GetSelectedTab.Components);
end;

procedure TFrameOldPalette.PaletteReorderCategory(Sender: TObject; const SourceCategory, TargetCategory: TButtonCategory);
begin
  if Assigned(FOrgReorderCategory) then
    FOrgReorderCategory(Sender, SourceCategory, TargetCategory);
  Rebuild(True);
end;

procedure TFrameOldPalette.UpdateIsFormDesigner(Categories: TButtonCategories);
var
  PalIndex: Integer;
  NewTabCount: Integer;
  I: Integer;

  PalGroup: TButtonCategory;
  PalGroupItems: TButtonCollection;
  PalItem: TButtonItem;
  S: string;
begin
  { Get the number of needed tabs }
  NewTabCount := 0;
  for PalIndex := 0 to Categories.Count - 1 do
    if Categories.Items[PalIndex].Items.Count > 0 then
      Inc(NewTabCount);

  if NewTabCount > 0 then // The ToolPalette meight use the LoadPalette to find out if there are components that match the filter
  begin
    FIsFormDesigner := False;
    for PalIndex := 0 to Categories.Count - 1 do
    begin
      PalGroup := Categories.Items[PalIndex];
      PalGroupItems := PalGroup.Items;
      for I := 0 to PalGroupItems.Count - 1 do
      begin
        PalItem := PalGroupItems.Items[I];
        if PalItem.InterfaceData <> nil then
        begin
          S := DelphiInterfaceToObject(PalItem.InterfaceData).ClassName;
          if (S = 'TComponentPalettePageItem') or (S = 'TFamePalettePageItem') then
          begin
            FIsFormDesigner := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

type
  TOpenControl = class(TControl);

procedure TFrameOldPalette.Rebuild(ReorderOnly: Boolean);
var
  TabUpdated: Boolean;

  procedure PreselectTab(const SelectedPaletteName: string);
  var
    Index: Integer;
  begin
    Index := TabControl.Tabs.IndexOf(SelectedPaletteName);
    if Index <> -1 then
    begin
      TabControl.TabIndex := Index;
      TabUpdated := True;
    end;
  end;

var
  i: Integer;
  List: TObjectList;
  Item: TCompItem;
  TabItem: TTabItem;
  PalIndex: Integer;

  FPalette: TCategoryButtons;
  Categories: TButtonCategories;
  PalGroup: TButtonCategory;
  PalGroupItems: TButtonCollection;
  PalItem: TButtonItem;

  PreviousRebuildingState: Integer;
begin
  if Application.Terminated or (FRebuilding > 0) then
    Exit;

  Categories := nil;
  FPalette := GetComponentPalette;
  if FPalette <> nil then
  begin
    Categories := GetPaletteCategories(FPalette);
    UpdateIsFormDesigner(Categories);
  end;

  if FIsFormDesigner then
  begin
    FModified := False;
    PreviousRebuildingState := FRebuilding;
    Inc(FRebuilding);
    try
      TabControl.Tabs.BeginUpdate;
      try
        if Categories <> nil then
        begin
          ClearTabs;
          if Categories.Count = 0 then
            TabControl.Tabs.AddObject('', nil);

          for PalIndex := 0 to Categories.Count - 1 do
          begin
            PalGroup := Categories.Items[PalIndex];
            PalGroupItems := PalGroup.Items;
            if PalGroupItems.Count = 0 then
              Continue;

            TabItem := TTabItem.Create(PalGroup);
            TabControl.Tabs.AddObject(TabItem.Caption, TabItem);
            List := TabItem.Components;
            for i := 0 to PalGroupItems.Count - 1 do
            begin
              PalItem := PalGroupItems.Items[i];
              Item := TCompItem.Create;
              Item.Name := PalItem.Caption;
              Item.Palette := PalGroup.Caption;
              Item.PalItem := PalItem;
              Item.PalGroup := PalGroup;
              List.Add(Item);
            end;
          end;
        end;
        if not ReorderOnly then
          FRebuilding := PreviousRebuildingState;

        TabUpdated := False;
        { Designer palette names }
        if FIsFormDesigner then
          PreselectTab(FDesignerSelectedPaletteName)
        else
          PreselectTab(FEditorSelectedPaletteName);
        if not TabUpdated and (TabItemCount > 0) then
          TabControl.TabIndex := 0;
        TabControlChange(TabControl);

      finally
        TabControl.Tabs.EndUpdate;
        if not ReorderOnly then
          FRebuilding := PreviousRebuildingState;
      end;

      RebuildPaletteMenu;

      Visible := True;
      if (Application.MainForm <> nil) and Application.MainForm.Visible then
        TOpenControl(Application.MainForm).AdjustSize;
    finally
      FRebuilding := PreviousRebuildingState;
    end;
  end
  else
    ClearTabItemComponents;
end;

procedure TFrameOldPalette.RebuildDelayed;
begin
  if not FRebuildDelay and HandleAllocated then
  begin
    FRebuildDelay := True;
    PostMessage(Handle, WM_REBUILD, 0, 0);
  end;
end;

procedure TFrameOldPalette.RebuildPaletteMenu;

  procedure CreateMenuItems(var Index: Integer);
  type
    TItem = record
      PalName: string;
      Index: Integer;
    end;
  var
    I: Integer;
    MenuItem: TMenuItem;
    List: TStringList;
  begin
    List := TStringList.Create;
    try
      for I := 0 to TabItemCount - 1 do
        List.AddObject(TabControl.Tabs[I], TObject(I));

      if Config.AlphaSortPopupMenu then
        List.Sort;

      for I := 0 to List.Count - 1 do
      begin
        MenuItem := TMenuItem.Create(Self);
        MenuItem.Caption := List[I];
        MenuItem.OnClick := PaletteMenuItemClick;
        MenuItem.Tag := NativeInt(List.Objects[I]);
        PopupMenuPalette.Items.Add(MenuItem);
        if (Index > 0) and (Index mod 26 = 0) then
          MenuItem.Break := mbBarBreak;
        Inc(Index);
      end;
    finally
      List.Free;
    end;
  end;

var
  Index: Integer;
begin
  PopupMenuPalette.AutoHotkeys := maManual;
  PopupMenuPalette.AutoLineReduction := maManual;
  PopupMenuPalette.Items.Clear;

  Index := 0;
  CreateMenuItems(Index);

  PopupMenuPalette.AutoHotkeys := maAutomatic;
  PopupMenuPalette.AutoLineReduction := maAutomatic;
end;

procedure TFrameOldPalette.SelectComponent(PalGroup: TButtonCategory; PalItem: TButtonItem);

  function SelectComponentIn: Boolean;
  var
    I: Integer;
    k: Integer;
    CompName: string;
    TabItem: TTabItem;
  begin
    if (PalGroup = nil) and (PalItem <> nil) then
      PalGroup := PalItem.Category
    else
    if PalGroup = nil then
    begin
      Result := False;
      Exit;
    end;

    for I := 0 to TabItemCount - 1 do
    begin
      TabItem := TabItems[I];
      if (TabItem <> nil) and (TabItem.PalGroup = PalGroup) then
      begin
        ChangeTabIndex(I);
        if (PalItem <> nil) and (TabItem.Components.Count > 0) then
        begin
          CompName := PalItem.Caption;
          for k := 0 to Palette.ButtonCount - 1 do
          begin
            // do not use TabBar.Tabs[i].AutoDeleteData[0] as TList because ButtonCount meight be out of sync here
            if AnsiCompareText(Palette.Buttons[k].HelpKeyword, CompName) = 0 then
            begin
              Palette.SelectedButton := k;
              if Palette.FirstVisible + Palette.VisibleCount <= k then
                Palette.FirstVisible := k - Palette.VisibleCount + 1;
              if k < Palette.FirstVisible then
                Palette.FirstVisible := k;
              Result := True;
              Exit;
            end;
          end;
        end;
        Break;
      end;
    end;
    Result := False;
  end;

begin
  if not SelectComponentIn() then
    Palette.SetMainButton;
end;

procedure TFrameOldPalette.TabSelected(ComponentList: TObjectList);
var
  i: Integer;
  Hint: string;
  Item: TCompItem;
  R: TRect;
  SelectedPaletteName, LeftPaletteName: string;
  CompPal: TCategoryButtons;
  SelectedPalItem: TBaseItem;
  TextOffset: Integer;
  IsDefaultImage: Boolean;
  CurPersonality: string;
  IsWin32Personality: Boolean;
begin
  if FRebuilding > 0 then
    Exit;
  if ComponentList = nil then
  begin
    Palette.ButtonCount := 0;
    Exit;
  end;
  CompPal := GetComponentPalette;

  { save/restore selected item and left item for the designer }
  LeftPaletteName := '';
  SelectedPaletteName := '';
  { Selected }
  if (TabControl.TabIndex <> -1) and (GetSelectedTab <> nil) then
    SelectedPaletteName := GetSelectedTab.Caption;

  if FIsFormDesigner then
    FDesignerSelectedPaletteName := SelectedPaletteName
  else
    FEditorSelectedPaletteName := SelectedPaletteName;

  SelectedPalItem := nil;
  if CompPal.SelectedItem <> nil then
  begin
    if CompPal.SelectedItem.ClassNameIs('TButtonCategory') then
      SelectedPalItem := CompPal.SelectedItem;
  end;

  { Initialize the component buttons }
  CurPersonality := PersonalityServices.CurrentPersonality;
  IsWin32Personality := (CurPersonality = sDelphiPersonality) or
                        (CurPersonality = sCBuilderPersonality);

  { fill button list }
  Palette.FirstVisible := 0;
  Palette.ButtonCount := ComponentList.Count;
  for i := 0 to ComponentList.Count - 1 do
  begin
    Item := TCompItem(ComponentList[i]);

    Hint := Item.Name;
    Palette.Buttons[i].Hint := Hint;

    IsDefaultImage := False;
    if IsWin32Personality and FIsFormDesigner then
      Palette.Buttons[i].Glyph.Handle := LoadComponentBitmap(TComponentClass(GetClass(Item.Name)), @IsDefaultImage);
    if IsDefaultImage then
    begin
      { we do not have a package for this item }
      Palette.Buttons[i].Glyph.Width := 0;
      if Assigned(CompPal) and Assigned(CompPal.OnDrawIcon) and (Item.PalItem <> nil) then
      begin
        Palette.Buttons[i].Glyph.Canvas.Brush.Color := Palette.Brush.Color;
        Palette.Buttons[i].Glyph.Width := 26;
        Palette.Buttons[i].Glyph.Height := 26;

        R := Bounds(0, 0, 26, 26);
        TextOffset := 0;
        CompPal.OnDrawIcon(CompPal, TButtonItem(Item.PalItem), Palette.Buttons[i].Glyph.Canvas, R, [], TextOffset);
      end;
    end;
    Palette.Buttons[i].ShowHint := True;
    Palette.Buttons[i].HelpKeyword := Item.Name;
    Palette.Buttons[i].Tag := NativeInt(Item);

    { Select button if the ToolForm has this component selected. }
    if Item.PalItem = SelectedPalItem then
      Palette.SelectedButton := i;
  end;
end;

procedure TFrameOldPalette.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TFrameOldPalette.WMRebuild(var Msg: TMessage);
begin
  FRebuildDelay := False;
  Rebuild;
end;

procedure TFrameOldPalette.TabBarPaletteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Project: IOTAProject;
begin
  { For .NET personalities to get the mouse wheel at least working after the
    user clicked on a tab }
  Project := GetActiveProject;
  if (Project <> nil) and
     ((Project.Personality = sDelphiDotNetPersonality) or
      (Project.Personality = sCSharpPersonality) or
      (Project.Personality = sVBPersonality)) then
    SetFocus;
end;

procedure TFrameOldPalette.TabControlChange(Sender: TObject);
begin
  if not FLockTabSelected then
  begin
    if (TabControl.TabIndex <> -1) and (GetSelectedTab <> nil) then
      TabSelected(GetSelectedTab.Components)
    else
      TabSelected(nil);
  end;
end;

procedure TFrameOldPalette.TabControlResize(Sender: TObject);
var
  AHeight: Integer;
begin
  if FRebuilding <= 0 then
  begin
    Inc(FRebuilding);
    try
      AHeight := TabControl.Height - (TabControl.DisplayRect.Bottom - TabControl.DisplayRect.Top) + 29;
      TabControl.Constraints.MinHeight := AHeight;
      TabControl.Parent.Constraints.MaxHeight := AHeight;
      TabControl.Parent.Height := TabControl.Height;
    finally
      Dec(FRebuilding);
    end;
  end;
end;

procedure TFrameOldPalette.TabControlWndProc(var Msg: TMessage);
var
  Pt: TPoint;
begin
  FOrgTabControlWndProc(Msg);
  case Msg.Msg of
    CM_MOUSEWHEEL:
      begin
        with TWMMouseWheel(Msg) do
        begin
          Pt := Palette.ClientToScreen(Point(0, 0));
          if YPos < Pt.Y then
          begin
            ChangeTabIndex(TabControl.TabIndex - WheelDelta div WHEEL_DELTA);
            Result := 1;
          end;
        end;
      end;
  end;
end;

procedure TFrameOldPalette.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if Assigned(Config) and ((Config.Left <> ALeft) or (Config.Top <> ATop)) then
  begin
    Config.Left := ALeft;
    Config.Top := ATop;
    Config.Save;
  end;
end;

end.

