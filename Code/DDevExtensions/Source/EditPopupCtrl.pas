unit EditPopupCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls, ToolsAPI, ActnList, MultiMon, Menus, ImgList;

const
  WM_DROPDOWN = WM_USER + 101;

type
  TDropDownEditBase = class;

  TPopupPanel = class(TPanel)
  private
    FEdit: TDropDownEditBase;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Edit: TDropDownEditBase read FEdit write FEdit;
  end;

  TPopupListBox = class(TListBox)
  private
    FEdit: TDropDownEditBase;
    FAllowMouseExecute: Boolean;
    procedure WMMouseActivate(var Message: TMessage); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    destructor Destroy; override;
    procedure ClearList;
    property Edit: TDropDownEditBase read FEdit write FEdit;
    property AllowMouseExecute: Boolean read FAllowMouseExecute write FAllowMouseExecute;
  end;

  TDropDownEditBase = class(TEdit)
  private
    FListBox: TPopupListBox;
    FPanel: TPopupPanel;
    FListVisible: Boolean;
    FOnBeforeDropDown: TNotifyEvent;
    FListAlignment: TAlignment;
    FAllowEmptyList: Boolean;
    procedure CMCancelMode(var Msg: TCMCancelMode); message CM_CANCELMODE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMDropDown(var Msg: TMessage); message WM_DROPDOWN;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    function GetDropDownHeight: Integer;
    function GetDropDownWidth: Integer;
    procedure SetDropDownHeight(const Value: Integer);
    procedure SetDropDownWidth(const Value: Integer);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CloseUp;
    procedure DropDown;
    property DropDownWidth: Integer read GetDropDownWidth write SetDropDownWidth;
    property DropDownHeight: Integer read GetDropDownHeight write SetDropDownHeight;
    property ListBox: TPopupListBox read FListBox;
    property Panel: TPopupPanel read FPanel;
    property ListVisible: Boolean read FListVisible;
    property ListAlignment: TAlignment read FListAlignment write FListAlignment;

    procedure UpdateDropDownBounds; virtual;

    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown write FOnBeforeDropDown;
  end;

  TDropDownEditSearchBase = class(TDropDownEditBase)
  private
    FImages: TCustomImageList;
    FImageIndex: Integer;
    procedure SetImages(const Value: TCustomImageList);
    function UpdateEditMargins: Boolean;
  protected
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateWnd; override;
    procedure WndProc(var Msg: TMessage); override;
  public
    destructor Destroy; override;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
  end;

implementation

uses
  CommCtrl, Themes;

{ TPopupPanel }

constructor TPopupPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible, csReplicatable];
end;

procedure TPopupPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TPopupPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FEdit.CloseUp;
end;

procedure TPopupPanel.WMActivate(var Msg: TWMActivate);
begin
  if Msg.Active = WA_INACTIVE then
    FEdit.CloseUp;
end;

procedure TPopupPanel.WMMouseActivate(var Message: TMessage);
begin
//  Message.Result := MA_NOACTIVATEANDEAT;
  Message.Result := MA_NOACTIVATE;
end;

{ TPopupListBox }

destructor TPopupListBox.Destroy;
begin
  // ClearList; can't be called here because the hWnd is already destroyed and accessing Items[] recreates the control => exception
  inherited Destroy;
end;

procedure TPopupListBox.ClearList;
begin
  Items.Clear;
end;

procedure TPopupListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FEdit.CloseUp;
end;

procedure TPopupListBox.WMActivate(var Msg: TWMActivate);
begin
  if Msg.Active = WA_INACTIVE then
    FEdit.CloseUp;
end;

procedure TPopupListBox.WMMouseActivate(var Message: TMessage);
begin
//  Message.Result := MA_NOACTIVATEANDEAT;
  Message.Result := MA_NOACTIVATE;
end;

{ TDropDownEditBase }

constructor TDropDownEditBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  FListAlignment := taRightJustify;
  FAllowEmptyList := True;

  FPanel := TPopupPanel.Create(Self);
  FPanel.ParentBackground := False;
  FPanel.Color := clBtnFace;
  FPanel.Visible := False;
  FPanel.Edit := Self;
  FPanel.Parent := Self;

  FListBox := TPopupListBox.Create(Self);
  FListBox.Edit := Self;
  FListBox.BorderStyle := bsNone;
  FListBox.Align := alClient;
  FListBox.Parent := FPanel;
  Text := '';
end;

procedure TDropDownEditBase.CMCancelMode(var Msg: TCMCancelMode);
begin
  if (Msg.Sender <> Self) and (Msg.Sender <> FPanel) and (Msg.Sender <> FListBox) and
     (Panel <> nil) and not Panel.ContainsControl(Msg.Sender) then
    CloseUp;
end;

procedure TDropDownEditBase.CloseUp;
begin
  if FListVisible then
  begin
    if (GetCapture <> 0) then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);

    SetWindowPos(FPanel.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    FPanel.Visible := False;
    //SetFocus; steals the focus from the code editor and hides the caret of the edit control
  end;
end;

procedure TDropDownEditBase.UpdateDropDownBounds;
var
  Pt: TPoint;
begin
  if ListAlignment = taRightJustify then
  begin
    Pt := ClientToScreen(Point(Width, Height));
    Pt.X := Pt.X - FPanel.Width;
    if Pt.X < Screen.DesktopLeft then
      Pt.X := Screen.DesktopLeft;
  end
  else
  begin
    Pt := ClientToScreen(Point(0, Height));
  end;

  SetWindowPos(Panel.Handle, HWND_TOPMOST, Pt.X, Pt.Y, Panel.Width + 2, Panel.Height + 2, SWP_NOACTIVATE);
end;

procedure TDropDownEditBase.DropDown;
begin
  if not FListVisible then
  begin
    if Assigned(FOnBeforeDropDown) then
      FOnBeforeDropDown(Self);
    {if (FListBox.Items.Count = 0) then  This doesn't work because the listbox will popup and close all the time
      Exit; // nothing to show}
    UpdateDropDownBounds;

    FListVisible := True;
    SetWindowPos(FPanel.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    FPanel.Visible := True;
  end;
end;

procedure TDropDownEditBase.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) or ((Key = VK_RETURN) and (ListBox.ItemIndex <> -1))then
  begin
    CloseUp;
    Key := 0;
  end
  else
  begin
    case Key of
      VK_SHIFT, VK_CONTROL, VK_MENU, VK_TAB, VK_PRINT: ;
    else
      DropDown;
    end;
  end;
  inherited KeyDown(Key, Shift);
  if not (((Key = VK_F4) and (ssAlt in Shift)) or
    (Key in [VK_DELETE, VK_LEFT, VK_RIGHT]) or
    ((Key in [VK_HOME, VK_END]) and not (ssCtrl in Shift)) or
    ((Key in [VK_INSERT]) and ((ssShift in Shift) or (ssCtrl in Shift)))) then
  begin
    SendMessage(FListBox.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TDropDownEditBase.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
  PostMessage(Handle, EM_SETSEL, 0, -1);
end;

procedure TDropDownEditBase.WMKillFocus(var Message: TWMKillFocus);
begin
  if FListVisible and not FPanel.ContainsControl(FindControl(Message.FocusedWnd)) then
    CloseUp;
  inherited;
  Invalidate;
end;

procedure TDropDownEditBase.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not FListVisible then
    PostMessage(Handle, WM_DROPDOWN, 0, 0);
end;

function TDropDownEditBase.GetDropDownHeight: Integer;
begin
  Result := FPanel.Height;
end;

function TDropDownEditBase.GetDropDownWidth: Integer;
begin
  Result := FPanel.Width;
end;

procedure TDropDownEditBase.SetDropDownHeight(const Value: Integer);
begin
  FPanel.Height := Value;
  if ListVisible then
    UpdateDropDownBounds;
end;

procedure TDropDownEditBase.SetDropDownWidth(const Value: Integer);
begin
  FPanel.Width := Value;
  if ListVisible then
    UpdateDropDownBounds;
end;

procedure TDropDownEditBase.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #27 then
    Key := #0;
end;

procedure TDropDownEditBase.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  { Delphi 2009 doesn't paint the background anymore }
  FillRect(Message.DC, ClientRect, Brush.Handle);
  Message.Result := 1;
end;

procedure TDropDownEditBase.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TDropDownEditBase.WMDropDown(var Msg: TMessage);
begin
  if Focused and not FListVisible then
    DropDown;
  Invalidate;
end;

{ TDropDownEditSearchBase }

procedure TDropDownEditSearchBase.CreateWnd;
begin
  inherited CreateWnd;
  UpdateEditMargins;
end;

destructor TDropDownEditSearchBase.Destroy;
begin
  SetImages(nil);
  inherited Destroy;
end;

procedure TDropDownEditSearchBase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    SetImages(nil);
end;

procedure TDropDownEditSearchBase.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if FImages <> nil then
      FImages.RemoveFreeNotification(Self);
    FImages := Value;
    if FImages <> nil then
      FImages.FreeNotification(Self);
  end;
end;

function TDropDownEditSearchBase.UpdateEditMargins: Boolean;
var
  Margins: Integer;
  LeftMargin: Integer;
begin
  Result := False;
  if HandleAllocated then
  begin
    LeftMargin := 0;
    if Images <> nil then
      LeftMargin := Images.Width + 2;

    Margins := SendMessage(Handle, EM_GETMARGINS, 0, 0);
    if (Margins and $FFFF) <> LeftMargin then
    begin
      SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN, MakeLong(LeftMargin, 0));
      Invalidate;
      Result := True;
    end;
  end;
end;

procedure TDropDownEditSearchBase.WMPaint(var Msg: TWMPaint);
var
  MyDC: Boolean;
  ps: TPaintStruct;
{  hFnt: HFONT;
  S: string;
  R: TRect;}
begin
  if Images <> nil then
  begin
    if UpdateEditMargins then
      Exit; // Invalidate() was triggered

    MyDC := Msg.DC = 0;
    if MyDC then
      Msg.DC := BeginPaint(Handle, ps);

    inherited;

    FillRect(Msg.DC, Rect(0, 0, Images.Width, Images.Height), Brush.Handle);
    if ImageIndex <> -1 then
      ImageList_Draw(Images.Handle, ImageIndex, Msg.DC, 0, 0, ILS_NORMAL);

{      if Text = '' then
    begin
      S := sSearchComponent;
      R := ClientRect;
      Inc(R.Left, Images.Width);
      Inc(R.Top, 2);
      hFnt := SelectObject(Msg.DC, Font.Handle);
      SetBkMode(Msg.DC, TRANSPARENT);
      DrawText(Msg.DC, PChar(S), Length(S), R, DT_LEFT or DT_TOP or DT_NOPREFIX);
      SetBkMode(Msg.DC, OPAQUE);
      SelectObject(Msg.DC, hFnt);
    end;}

    if MyDC then
      EndPaint(Handle, ps);
  end
  else
    inherited;
end;

procedure TDropDownEditSearchBase.WndProc(var Msg: TMessage);
var
  LLeft, LTop: Integer;
begin
  case Msg.Msg of
    CN_CTLCOLORSTATIC,
    CN_CTLCOLOREDIT:
      begin
        if Images <> nil then
        begin
          LLeft := 0;
          LTop := 0;
          {$IF CompilerVersion >= 23.0}
          if StyleServices.Enabled and Ctl3D then
          {$ELSE}
          if ThemeServices.ThemesEnabled and Ctl3D then
          {$IFEND}
          begin
            Inc(LLeft);
            Inc(LTop);
          end;
          ExcludeClipRect(Msg.WParam, LLeft + 1, LTop + 1, Images.Width, Images.Height);
        end;
      end;
  end;

  inherited;

  case Msg.Msg of
    WM_SIZE, WM_SETFONT, WM_FONTCHANGE, WM_WINDOWPOSCHANGED,
    CM_FONTCHANGED, CM_BORDERCHANGED, CM_CTL3DCHANGED:
      if not (csLoading in ComponentState) then
        UpdateEditMargins;
  end;
end;

end.

