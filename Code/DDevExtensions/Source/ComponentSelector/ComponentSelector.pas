{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit ComponentSelector;

{$I ..\DelphiExtension.inc}

interface

uses
  CategoryButtons, PaletteAPI,
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls, ToolsAPI, ActnList, Registry, FrmTreePages,
  MultiMon, Menus, ImgList, ToolsAPIHelpers, EditPopupCtrl;

type
  TToolbarInfo = packed record
    Left, Top: Integer;
    Visible: Boolean;
  end;

  TCompItem = class(TObject)
  private
    FModule: HMODULE;
    FPalette: string;
    FCompName: string;
    FData: TObject;
    FComponentClass: TComponentClass;
  public
    constructor Create(AModule: HMODULE; const APalette, ACompName: string;
      AComponentClass: TComponentClass; AData: TObject);
    destructor Destroy; override;
    function LoadBitmap: HBitmap;

    property Palette: string read FPalette;
    property CompName: string read FCompName;
    property Data: TObject read FData;
    property ComponentClass: TComponentClass read FComponentClass;
    property Module: HMODULE read FModule;
  end;

  TDropDownEdit = class(TDropDownEditBase)
  private
    FPanelBottom: TPanel;
    FCheckBoxSimpleSearch: TCheckBox;
    FOnOptionsChanged: TNotifyEvent;
    FCheckBoxPaletteSort: TCheckBox;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  protected
    procedure LooseFocus(Sender: TObject);
    procedure DoOptionChangeClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property PanelBottom: TPanel read FPanelBottom;
    property CheckBoxSimpleSearch: TCheckBox read FCheckBoxSimpleSearch;
    property CheckBoxPaletteSort: TCheckBox read FCheckBoxPaletteSort;
    procedure UpdateDropDownBounds; override;

    property OnOptionsChanged: TNotifyEvent read FOnOptionsChanged write FOnOptionsChanged;
  end;

  TComponentSelector = class(TComponent)
  private
    FEdit: TDropDownEdit;
    FToolBar: TToolBar;
    FTimerFilterUpdate: TTimer;
    FPalette: TCategoryButtons;
    FCompObjects: TObjectList;
    FHotkeyAction: TAction;

    procedure TimerFilterUpdateTimer(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);

    procedure DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ClickItem(Sender: TObject); overload;
    procedure ClickItem(Sender: TObject; ExecuteItem: Boolean); overload;
    procedure BeforeDropDown(Sender: TObject);
    procedure OptionsChanged(Sender: TObject);
    procedure SetHotkey(const Value: TShortCut);
    function GetHotkey: TShortCut;
  protected
    function Filter(const AClassName: string): Boolean;
    procedure ExecuteHotkeyAction(Sender: TObject);
    procedure UpdateComponentList;
//    procedure DoTimer(Sender: TObject); //testing
    function GetOptionPages: TTreePage; virtual;
    procedure LoadToolbarConfig;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveToolbarConfig;

    property Hotkey: TShortCut read GetHotkey write SetHotkey;
    property Edit: TDropDownEdit read FEdit;
    property ToolBar: TToolBar read FToolBar;
  end;

  TPaletteItemHolder = class(TObject)
  private
    FItem: TButtonItem;
  public
    constructor Create(AItem: TButtonItem);
    property Item: TButtonItem read FItem;
  end;

function ComponentSelectorCtrl: TComponentSelector;
procedure InitPlugin(Unload: Boolean);

implementation

uses
  ComponentManager, IDEUtils, IDEHooks, Math, AppConsts, TypInfo,
  FrmeOptionPageComponentSelector, FrmOptions, DtmImages;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    TComponentSelector.Create(Application.MainForm)
  else
    ComponentSelectorCtrl.Free;
end;

var
  GlobalComponentSelectorCtrl: TComponentSelector;

function ComponentSelectorCtrl: TComponentSelector;
begin
  Result := GlobalComponentSelectorCtrl;
end;

{ TPaletteItemHolder }

constructor TPaletteItemHolder.Create(AItem: TButtonItem);
begin
  inherited Create;
  FItem := AItem;
end;

{ TCompItem }

constructor TCompItem.Create(AModule: HMODULE; const APalette, ACompName: string;
  AComponentClass: TComponentClass; AData: TObject);
begin
  inherited Create;
  FModule := AModule;
  FPalette := APalette;
  FCompName := ACompName;
  FData := AData;
  FComponentClass := AComponentClass;
end;

destructor TCompItem.Destroy;
begin
  if FData is TPaletteItemHolder then
    FData.Free;
  inherited Destroy;
end;

function TCompItem.LoadBitmap: HBitmap;
begin
  Result := LoadComponentBitmap(ComponentClass);
end;

function PaletteSortCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  c1, c2: TCompItem;
begin
  c1 := TCompItem(List.Objects[Index1]);
  c2 := TCompItem(List.Objects[Index2]);
  Result := AnsiCompareText(c1.Palette, c2.Palette);
  if Result = 0 then
    Result := AnsiCompareText(c1.CompName, c2.CompName);
end;

{ TComponentSelector }

constructor TComponentSelector.Create(AOwner: TComponent);
var
  ControlBar: TControlBar;
  Services: INTAServices;
begin
  inherited Create(AOwner);

  GlobalComponentSelectorCtrl := Self;
  TFormOptions.RegisterPages(GetOptionPages);
  FHotkeyAction := TAction.Create(nil);
  FHotkeyAction.OnExecute := ExecuteHotkeyAction;
  FCompObjects := TObjectList.Create;

  ControlBar := TControlBar(Application.MainForm.FindComponent('ControlBar1'));
  if ControlBar <> nil then
  begin
    Supports(BorlandIDEServices, INTAServices, Services);
    FHotkeyAction.ActionList := Services.ActionList;
    FToolBar := NewToolBar(Self, 'ToolBarDDevExtensionsComponentSelector', 'ComponentSelector', False);

    FEdit := TDropDownEdit.Create(Self);
    FEdit.Name := 'EditDDevExtensionsComponentSelector';
    FEdit.Text := '';
    FEdit.Width := 150;
    FToolBar.Top := Application.MainForm.Height;
    FToolBar.Left := ControlBar.ClientWidth - FToolBar.Width;
    FEdit.Parent := FToolBar;
    FToolBar.Left := ControlBar.ClientWidth - FToolBar.Width;
    FEdit.OnChange := EditChange;
    FEdit.OnKeyPress := EditKeyPress;
    FEdit.ListBox.Color := clBtnFace;
    FEdit.ListBox.Style := lbOwnerDrawFixed;
    FEdit.ListBox.OnDrawItem := DrawItem;
    FEdit.ListBox.ItemHeight := 26;
    FEdit.DropDownWidth := 250;
    FEdit.DropDownHeight := FEdit.ListBox.ItemHeight * 25;
    FEdit.CheckBoxPaletteSort.Checked := True;
    FEdit.ListBox.OnClick := ClickItem;
    FEdit.OnBeforeDropDown := BeforeDropDown;
    FEdit.OnOptionsChanged := OptionsChanged;

    FTimerFilterUpdate := TTimer.Create(Self);
    FTimerFilterUpdate.Enabled := False;
    FTimerFilterUpdate.Interval := 170;
    FTimerFilterUpdate.OnTimer := TimerFilterUpdateTimer;

    LoadToolbarConfig;
  end;
end;

destructor TComponentSelector.Destroy;
begin
  if Assigned(FToolBar) then
    SaveToolbarConfig;
  GlobalComponentSelectorCtrl := nil;
  FHotkeyAction.Free; // must be destroyed after SaveToolbarConfig
  FCompObjects.Free;
  inherited Destroy;
end;

procedure TComponentSelector.UpdateComponentList;
var
  i: Integer;
  List: TStrings;
  PaletteName, CompName: string;
  PalIndex: Integer;
  Categories: TButtonCategories;
  PalGroup: TButtonCategory;
  PalGroupItems: TButtonCollection;
  PalItem: TButtonItem;
  Holder: TPaletteItemHolder;
  SelText: string;
  EditTextLen: Integer;
  Len: Integer;
  MinLenDiff: Integer;
  MinLenDiffIndex: Integer;
  Valid: Boolean;
begin
  FEdit.ListBox.AllowMouseExecute := False;
  if FEdit.ListBox.ItemIndex <> -1 then
    SelText := FEdit.ListBox.Items[FEdit.ListBox.ItemIndex];

  List := TStringList.Create;
  try
    FCompObjects.Clear;
    FPalette := GetComponentPalette;
    if FPalette <> nil then
    begin
      Categories := GetPaletteCategories(FPalette);
      for PalIndex := 0 to Categories.Count - 1 do
      begin
        PalGroup := Categories.Items[PalIndex];
        PalGroupItems := PalGroup.Items;
        PaletteName := PalGroup.Caption;
        for i := 0 to PalGroupItems.Count - 1 do
        begin
          PalItem := PalGroupItems[i];
          CompName := PalItem.Caption;
          if Filter(CompName) then
          begin
            Holder := TPaletteItemHolder.Create(PalItem);
            FCompObjects.Add(Holder);
            List.AddObject(CompName, TCompItem.Create(0, PaletteName, CompName, nil, Holder));
          end;
        end;
      end;
    end;
    if FEdit.CheckBoxPaletteSort.Checked then
      TStringList(List).CustomSort(PaletteSortCompare)
    else
      TStringList(List).Sort;

    if List.Count = 1 then
      FEdit.PanelBottom.Caption := '1 component  '
    else
      FEdit.PanelBottom.Caption := Format('%d components  ', [List.Count]);

    // transfer list to ListBox
    FEdit.ListBox.Items.BeginUpdate;
    try
      FEdit.ListBox.ClearList;
      FEdit.ListBox.Items.Assign(List);

      if SelText <> '' then
        FEdit.ListBox.ItemIndex := List.IndexOf(SelText);
      if (FEdit.ListBox.ItemIndex = -1) and (List.Count > 0) then
      begin
        { Is the SelText already in the list }
        Valid := False;
        for i := 0 to List.Count - 1 do
        begin
          if Pos(AnsiLowerCase(SelText), AnsiLowerCase(List[i])) <> 0 then
          begin
            Valid := True;
            Break;
          end;
        end;
        if not Valid then
          SelText := '';

        { Select the best fitting item }
        MinLenDiffIndex := 0;
        if FEdit.Text <> '' then
        begin
          EditTextLen := Length(FEdit.Text);
          MinLenDiff := Length(List[0]) - EditTextLen;
          for i := 1 to List.Count - 1 do
          begin
            Len := Length(List[i]) - EditTextLen;
            if Len < MinLenDiff then
            begin
              MinLenDiff := Len;
              MinLenDiffIndex := i;
            end;
          end;
        end;
        FEdit.ListBox.ItemIndex := MinLenDiffIndex;
      end;
    finally
      FEdit.UpdateDropDownBounds;
      FEdit.ListBox.Items.EndUpdate;
    end;
  finally
    List.Free;
  end;
end;

procedure TComponentSelector.EditChange(Sender: TObject);
begin
  FTimerFilterUpdate.Enabled := False;
  FTimerFilterUpdate.Enabled := True;
end;

procedure TComponentSelector.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    if FEdit.ListBox.ItemIndex <> -1 then
      ClickItem(FEdit.ListBox, True)
    else
      TimerFilterUpdateTimer(FTimerFilterUpdate);
  end;
end;

procedure TComponentSelector.TimerFilterUpdateTimer(Sender: TObject);
begin
  FTimerFilterUpdate.Enabled := False;
  UpdateComponentList;
end;

function TComponentSelector.Filter(const AClassName: string): Boolean;
begin
  if FEdit.CheckBoxSimpleSearch.Checked and (FEdit.Text <> '') then
  begin
    Result := AnsiLowerCase(Copy(AClassName, 1, Length(FEdit.Text))) = AnsiLowerCase(FEdit.Text); // AnsiStartsText
    if not Result and (UpCase(FEdit.Text[1]) <> 'T') then // try it with a leading "T"
      Result := AnsiLowerCase(Copy(AClassName, 1, 1 + Length(FEdit.Text))) = 't' + AnsiLowerCase(FEdit.Text); // AnsiStartsText
  end
  else
    Result := (FEdit.Text = '') or (Pos(AnsiLowerCase(FEdit.Text), AnsiLowerCase(AClassName)) <> 0);
end;

procedure TComponentSelector.DrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Item: TCompItem;
  TextOffset: Integer;
  //PaintIcon: INTAPalettePaintIcon;
  r: TColorRef;
  LUnitName: string;
begin
  with TListBox(Control) do
  begin
    Item := TCompItem(Items.Objects[Index]);
    if not (odSelected in State) then
    begin
      if Index mod 2 = 0 then
      begin
        r := ColorToRGB(Color);
        Canvas.Brush.Color := RGB(GetRValue(r) + 10, GetGValue(r) + 10, GetBValue(r) + 10);
      end
      else
        Canvas.Brush.Color := Color;
    end;
    Canvas.FillRect(Rect);

    // draw palette name text
    Canvas.Brush.Style := bsClear;
    LUnitName := '';
    {if Assigned(Item.FComponentClass) and (Item.FComponentClass.ClassInfo <> nil) then
      LUnitName := ', ' + GetTypeData(Item.FComponentClass.ClassInfo).LUnitName + '.pas';}
    Canvas.Font.Name := 'Arial';
    Canvas.Font.Size := 7;
    Canvas.Font.Color := clGrayText;
    Canvas.TextRect(Rect, Rect.Right - Canvas.TextWidth(Item.Palette + LUnitName), Rect.Top + 1 + Canvas.TextHeight('Ag') + 1, Item.Palette + LUnitName);

    TextOffset := 24;
    if (Item.Data <> nil) and Assigned(FPalette) and Assigned(FPalette.OnDrawIcon) then
    begin
      TextOffset := 0;
      {if Supports(TPaletteItemHolder(Item.Data).Item, INTAPalettePaintIcon, PaintIcon) then
        PaintIcon.Paint(Canvas, Rect.Left, Rect.Top, pi16x16);
      Inc(TextOffset, 16);}
      FPalette.OnDrawIcon(FPalette, TButtonItem(TPaletteItemHolder(Item.Data).Item), Canvas, Rect, [], TextOffset);
      Inc(TextOffset, 2);
    end;

    // draw component name text
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Assign(Font);
    if odSelected in State then
      Canvas.Font.Color := clHighlightText;
    Canvas.TextRect(Rect, Rect.Left + TextOffset + 3, Rect.Top + (ItemHeight - Canvas.TextHeight('Ag')) div 2, Item.CompName);

    if FEdit.CheckBoxPaletteSort.Checked then
    begin
      if (Index + 1 < Items.Count) and (AnsiCompareText(TCompItem(Items.Objects[Index + 1]).Palette, Item.Palette) <> 0) then
      begin
        Canvas.Pen.Color := clBlack;
        Canvas.MoveTo(Rect.Left, Rect.Bottom - 1);
        Canvas.LineTo(Rect.Right, Rect.Bottom - 1);
      end;
    end;
  end;
end;

procedure TComponentSelector.ClickItem(Sender: TObject);
begin
  ClickItem(Sender, False);
end;

procedure TComponentSelector.ClickItem(Sender: TObject; ExecuteItem: Boolean);
var
  Item: TCompItem;
begin
  with TListBox(Sender) do
  begin
    if ItemIndex <> -1 then
    begin
      Item := TCompItem(Items.Objects[ItemIndex]);
      SelectComponentPalette(TPaletteItemHolder(Item.Data).Item, ExecuteItem);
    end;
  end;
end;

procedure TComponentSelector.BeforeDropDown(Sender: TObject);
begin
  if (Application <> nil) and not Application.Terminated then
  begin
    UpdateComponentList;
  end;
end;

procedure TComponentSelector.LoadToolbarConfig;
var
  Reg: TRegistry;
  ToolbarInfo: TToolbarInfo;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    {if Reg.OpenKeyReadOnly('\Software\DelphiTools\DelphiSpeedUp\' + DelphiVersion) then
    begin
      if Reg.ValueExists('ComponentSelector') and (Reg.GetDataSize('ComponentSelector') = SizeOf(TToolbarInfo)) then
      begin
        Reg.ReadBinaryData('ComponentSelector', ToolbarInfo, SizeOf(TToolbarInfo));
        FToolBar.Left := ToolbarInfo.Left;
        FToolBar.Top := ToolbarInfo.Top;
        FToolBar.Visible := ToolbarInfo.Visible;
      end;
    end;}

    if Reg.OpenKeyReadOnly('\Software\DelphiTools\DDevExtensions\' + DelphiVersion + '\ComponentSelector') then
    begin
      if Reg.ValueExists('Toolbar') and (Reg.GetDataSize('Toolbar') = SizeOf(TToolbarInfo)) then
      begin
        Reg.ReadBinaryData('Toolbar', ToolbarInfo, SizeOf(TToolbarInfo));
        FToolBar.Left := ToolbarInfo.Left;
        FToolBar.Top := ToolbarInfo.Top;
        FToolBar.Visible := ToolbarInfo.Visible;
      end;
      if Reg.ValueExists('SimpleSearch') then
        FEdit.CheckBoxSimpleSearch.Checked := Reg.ReadInteger('SimpleSearch') <> 0;
      if Reg.ValueExists('SortByPalette') then
        FEdit.CheckBoxPaletteSort.Checked := Reg.ReadInteger('SortByPalette') <> 0;
      if Reg.ValueExists('Hotkey') then
        SetHotkey( Reg.ReadInteger('Hotkey') );
    end;
  finally
    Reg.Free;
  end;
end;

procedure TComponentSelector.SaveToolbarConfig;
var
  Reg: TRegistry;
  ToolbarInfo: TToolbarInfo;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\DelphiTools\DDevExtensions\' + DelphiVersion + '\ComponentSelector', True) then
    begin
      ToolbarInfo.Left := FToolBar.Left;
      ToolbarInfo.Top := FToolBar.Top;
      ToolbarInfo.Visible := FToolBar.Visible;
      Reg.WriteBinaryData('Toolbar', ToolbarInfo, SizeOf(TToolbarInfo));

      Reg.WriteBool('SimpleSearch', FEdit.CheckBoxSimpleSearch.Checked);
      Reg.WriteBool('SortByPalette', FEdit.CheckBoxPaletteSort.Checked);
      Reg.WriteInteger('Hotkey', GetHotkey);
    end;
    {if Reg.OpenKey('\Software\DelphiTools\DelphiSpeedUp\' + DelphiVersion, False) then
    begin
      // clean up old DelphiSpeedup integration
      if Reg.ValueExists('ComponentSelector') then
        Reg.DeleteValue('ComponentSelector');
    end;}
  finally
    Reg.Free;
  end;
end;

procedure TComponentSelector.OptionsChanged(Sender: TObject);
begin
  SaveToolbarConfig;
  UpdateComponentList;
end;

function TComponentSelector.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Component Selector', TFrameOptionPageComponentSelector, Self);
end;

procedure TComponentSelector.SetHotkey(const Value: TShortCut);
begin
  FHotkeyAction.ShortCut := Value;
end;

procedure TComponentSelector.ExecuteHotkeyAction(Sender: TObject);
begin
  if FToolBar.Visible and not (FEdit.Focused or FEdit.ListVisible) then
  begin
    FEdit.SetFocus;
    FEdit.DropDown;
  end;
end;

function TComponentSelector.GetHotkey: TShortCut;
begin
  Result := FHotkeyAction.ShortCut;
end;

{ TDropDownEdit }

constructor TDropDownEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPanelBottom := TPanel.Create(Self);
  FPanelBottom.ParentBackground := False;
  FPanelBottom.Height := 18 * 2;
  FPanelBottom.Alignment := taRightJustify;
  FPanelBottom.Align := alBottom;
  FPanelBottom.Parent := Panel;
  FPanelBottom.ParentColor := True;

  FCheckBoxSimpleSearch := TCheckBox.Create(Self);
  FCheckBoxSimpleSearch.Left := 2;
  FCheckBoxSimpleSearch.Top := 1;
  FCheckBoxSimpleSearch.Caption := 'Simple search';
  FCheckBoxSimpleSearch.OnClick := DoOptionChangeClick;
  FCheckBoxSimpleSearch.Parent := FPanelBottom;
  FCheckBoxSimpleSearch.Width := 100;

  FCheckBoxPaletteSort := TCheckBox.Create(Self);
  FCheckBoxPaletteSort.Left := 2;
  FCheckBoxPaletteSort.Top := FCheckBoxPaletteSort.BoundsRect.Bottom + 1;
  FCheckBoxPaletteSort.Caption := 'Sort by palette';
  FCheckBoxPaletteSort.OnClick := DoOptionChangeClick;
  FCheckBoxPaletteSort.Parent := FPanelBottom;
  FCheckBoxPaletteSort.Width := 120;
end;

procedure TDropDownEdit.UpdateDropDownBounds;
var
  Pt, HPt: TPoint;
  h: Integer;
  HM: HMONITOR;
  MonitorInfo: TMonitorInfo;
begin
  Pt := ClientToScreen(Point(Width, Height));
  Pt.X := Pt.X - Panel.Width;
  if Pt.X < Screen.DesktopLeft then
    Pt.X := Screen.DesktopLeft;

  HM := MonitorFromWindow(Handle, MONITOR_DEFAULTTONEAREST);
  MonitorInfo.cbSize := SizeOf(MonitorInfo);
  if (HM <> 0) and GetMonitorInfo(HM, @MonitorInfo) then
    h := MonitorInfo.rcWork.Bottom
  else
    h := Screen.Height;

  HPt := ClientToScreen(Point(0, Height + 2));
  HPt.Y := Min(h - (HPt.Y + PanelBottom.Height), 2 + ListBox.Items.Count * ListBox.ItemHeight + 2 + PanelBottom.Height);
  SetWindowPos(Panel.Handle, HWND_TOPMOST, Pt.X, Pt.Y, Panel.Width, HPt.Y, SWP_NOACTIVATE);
end;

procedure TDropDownEdit.WMPaint(var Msg: TWMPaint);
var
  ps: TPaintStruct;
  dc: HDC;
  hFnt: HFONT;
  bmp, orgBmp: HBITMAP;
  w, h: Integer;
  DibSect: TDIBSection;
  S: string;
  R: TRect;
begin
  if not Focused then
  begin
    bmp := LoadBitmap(GetModuleHandle(delphicoreide_bpl), 'DEFAULT');
    if bmp <> 0 then
    begin
      GetObject(bmp, SizeOf(DibSect), @DibSect);
      w := DibSect.dsBm.bmWidth;
      h := DibSect.dsBm.bmHeight;

      Msg.DC := BeginPaint(Handle, ps);
      dc := CreateCompatibleDC(Msg.DC);
      try
        orgBmp := SelectObject(dc, bmp);
        FillRect(Msg.DC, Rect(0, 0, w, h), Brush.Handle);

        StretchBltTransparent(Msg.DC, 0, -2, w, h,
                              dc, 0, 0, w, h, SRCCOPY, GetPixel(dc, 0, h - 1));
        SelectObject(dc, orgBmp);
      finally
        DeleteObject(bmp);
        DeleteDC(dc);
      end;
      S := sSearchComponent;
      R := ClientRect;
      Inc(R.Left, w);
      Inc(R.Top, 2);
      hFnt := SelectObject(Msg.DC, Font.Handle);
      SetBkMode(Msg.DC, TRANSPARENT);
      DrawText(Msg.DC, PChar(S), Length(S), R, DT_LEFT or DT_TOP or DT_NOPREFIX);
      SetBkMode(Msg.DC, OPAQUE);
      SelectObject(Msg.DC, hFnt);
      EndPaint(Handle, ps);
      Exit;
    end;
  end;
  inherited;
end;

procedure TDropDownEdit.DoOptionChangeClick(Sender: TObject);
begin
  LooseFocus(Sender);
  if Assigned(FOnOptionsChanged) then
    FOnOptionsChanged(Self);
end;

procedure TDropDownEdit.LooseFocus(Sender: TObject);
begin
  if ListVisible then
    Windows.SetFocus(Panel.Handle);
end;

end.
