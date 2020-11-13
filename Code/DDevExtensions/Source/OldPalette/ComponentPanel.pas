{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvComponentPanel.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
  Andreas Hausladen

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

components : TJvComponentPanel
description: Component panel for GUI developers

Known Issues:
-----------------------------------------------------------------------------}
// $Id: JvComponentPanel.pas 12461 2009-08-14 17:21:33Z obones $

unit ComponentPanel;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages,
  Classes, Controls, Buttons, Forms, ExtCtrls, Graphics;

type
  TButtonClick = procedure(Sender: TObject; Button: Integer) of object;

  TJvExSpeedButton = class(TSpeedButton)
  private
    FHintWindowClass: THintWindowClass;
    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
  public
    property HintWindowClass: THintWindowClass read FHintWindowClass write FHintWindowClass;
  end;


  { VCL Buttons unit does not publish TJvButtonGlyph class,
    so we do it for other programers (Delphi 3 version) }
  TJvButtonGlyph = class(TObject)
  private
    FGlyphList: TImageList;
    FIndexs: array [TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    FColor: TColor;
    FBiDiMode: TBiDiMode; {o}
    FParentBiDiMode: Boolean;
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetColor(Value: TColor);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState); virtual;
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect);
  protected
    FOriginal: TBitmap;
    procedure CalcTextRect(Canvas: TCanvas; var TextRect: TRect; const Caption: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean): TRect;
    { DrawExternal draws any glyph (not glyph property) -
      if you don't needed to save previous glyph set IgnoreOld to True -
      this increases performance }
    function DrawExternal(AGlyph: TBitmap; ANumGlyphs: TNumGlyphs; AColor: TColor; IgnoreOld: Boolean;
      Canvas: TCanvas; const Client: TRect; const Offset: TPoint; const Caption: string;
      Layout: TButtonLayout; Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean): TRect;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property Color: TColor read FColor write SetColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TPaintButtonEvent = procedure(Sender: TObject; IsDown, IsDefault: Boolean; State: TButtonState) of object;
  
  TJvNoFrameButton = class(TJvExSpeedButton)
  private
    FGlyphDrawer: TJvButtonGlyph;
    FNoBorder: Boolean;
    FOnPaint: TPaintButtonEvent;
    FRepeatedClick: Boolean;
    FRepeatTimer: TTimer;
    FInitRepeatPause: Integer;
    FRepeatPause: Integer;
    FClicked: Boolean;
    procedure SetNoBorder(Value: Boolean);
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefaultDrawing(const IsDown: Boolean; const State: TButtonState);
    property Canvas;
  published
    property Color;
    property ParentColor;
    property NoBorder: Boolean read FNoBorder write SetNoBorder default True;
    property RepeatedClick: Boolean read FRepeatedClick write FRepeatedClick default False;
    property InitRepeatPause: Integer read FInitRepeatPause write FInitRepeatPause default 400;
    property RepeatPause: Integer read FRepeatPause write FRepeatPause default 100;
    property OnPaint: TPaintButtonEvent read FOnPaint write FOnPaint;
  end;

  TJvPaintPanelContentEvent = procedure(Sender: TObject; Canvas: TCanvas; R: TRect) of object;

  TJvComponentPanel = class(TCustomPanel)
  private
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FButtons: TList;
    FOnClick: TButtonClick;
    FOnDblClick: TButtonClick;
    FButtonPointer: TJvExSpeedButton;
    FButtonLeft: TJvNoFrameButton;
    FButtonRight: TJvNoFrameButton;
    FFirstVisible: Integer;
    FLockUpdate: Integer;
    FSelectButton: TJvExSpeedButton;
    FHintWindowClass: THintWindowClass;
    FOnPaintContent: TJvPaintPanelContentEvent;
    function GetButton(Index: Integer): TJvExSpeedButton;
    function GetButtonCount: Integer;
    procedure SetButtonCount(AButtonCount: Integer);
    procedure SetButtonWidth(AButtonWidth: Integer);
    procedure SetButtonHeight(AButtonHeight: Integer);
    procedure SetFirstVisible(AButton: Integer);
    procedure BtnClick(Sender: TObject);
    procedure BtnDblClick(Sender: TObject);
    procedure MoveClick(Sender: TObject);
    function GetVisibleCount: Integer;
    procedure SetSelectedButton(Value: Integer);
    function GetSelectedButton: Integer;
    procedure WMSetText(var Msg: TWMSetText); message WM_SETTEXT;
    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
  protected
    procedure Resize; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure Paint; override;
    procedure PaintContent(const R: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecreateButtons;
    procedure SetMainButton;
    procedure Invalidate; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Buttons[Index: Integer]: TJvExSpeedButton read GetButton; default;
    property FirstVisible: Integer read FFirstVisible write SetFirstVisible;
    property ButtonLeft: TJvNoFrameButton read FButtonLeft;
    property ButtonRight: TJvNoFrameButton read FButtonRight;
    property VisibleCount: Integer read GetVisibleCount;
    property SelectedButton: Integer read GetSelectedButton write SetSelectedButton;
    property HintWindowClass: THintWindowClass read FHintWindowClass write FHintWindowClass;
  published
    property Align;
    property OnClick: TButtonClick read FOnClick write FOnClick;
    property OnDblClick: TButtonClick read FOnDblClick write FOnDblClick;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 28;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 28;
    property ButtonCount: Integer read GetButtonCount write SetButtonCount default 0;
    property Anchors;
    property Constraints;
    property AutoSize;
    property BiDiMode;
    property UseDockManager default True;
    property DockSite;
    property ParentBiDiMode;
    property DragKind;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
    property OnCanResize;
    property OnConstrainedResize;
    property OnPaintContent: TJvPaintPanelContentEvent read FOnPaintContent write FOnPaintContent;
    property PopupMenu;
  end;

implementation

uses
  CommCtrl, SysUtils;

{$R ComponentPanel.res}

const
  ROP_DSPDxax = $00E20746;

type
  TJvGlyphList = class(TImageList)
  private
    FUsed: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TJvGlyphCache = class(TObject)
  private
    FGlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TJvGlyphList;
    procedure ReturnList(List: TJvGlyphList);
    function Empty: Boolean;
  end;

//=== { TJvGlyphList } =======================================================

constructor TJvGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  FUsed := TBits.Create;
end;

destructor TJvGlyphList.Destroy;
begin
  FUsed.Free;
  inherited Destroy;
end;

function TJvGlyphList.AllocateIndex: Integer;
begin
  Result := FUsed.OpenBit;
  if Result >= FUsed.Size then
  begin
    Result := inherited Add(nil, nil);
    FUsed.Size := Result + 1;
  end;
  FUsed[Result] := True;
end;

function TJvGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TJvGlyphList.Delete(Index: Integer);
begin
  if FUsed[Index] then
  begin
    Dec(FCount);
    FUsed[Index] := False;
  end;
end;

//=== { TJvGlyphCache } ======================================================

constructor TJvGlyphCache.Create;
begin
  inherited Create;
  FGlyphLists := TList.Create;
end;

destructor TJvGlyphCache.Destroy;
begin
  FGlyphLists.Free;
  inherited Destroy;
end;

function TJvGlyphCache.GetList(AWidth, AHeight: Integer): TJvGlyphList;
var
  I: Integer;
begin
  for I := FGlyphLists.Count - 1 downto 0 do
  begin
    Result := FGlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then
        Exit;
  end;
  Result := TJvGlyphList.CreateSize(AWidth, AHeight);
  FGlyphLists.Add(Result);
end;

procedure TJvGlyphCache.ReturnList(List: TJvGlyphList);
begin
  if List = nil then
    Exit;
  if List.Count = 0 then
  begin
    FGlyphLists.Remove(List);
    List.Free;
  end;
end;

function TJvGlyphCache.Empty: Boolean;
begin
  Result := FGlyphLists.Count = 0;
end;

//=== { TJvButtonGlyph } =====================================================

var
  GlyphCache: TJvGlyphCache = nil;
  Pattern: TBitmap = nil;

procedure CreateBrushPattern(FaceColor, HighLightColor: TColor);
var
  X, Y: Integer;
begin
  Pattern := TBitmap.Create;
  Pattern.Width := 8;
  Pattern.Height := 8;
  with Pattern.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := FaceColor; // clBtnFace
    FillRect(Rect(0, 0, Pattern.Width, Pattern.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then { toggles between even/odd pixels }
          Pixels[X, Y] := HighLightColor; {clBtnHighlight}; { on even/odd rows }
  end;
end;

constructor TJvButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then
    GlyphCache := TJvGlyphCache.Create;
end;

destructor TJvButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TJvButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then
      TJvGlyphList(FGlyphList).Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(TJvGlyphList(FGlyphList));
  FGlyphList := nil;
end;

procedure TJvButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TJvButtonGlyph.SetBiDiMode(Value: TBiDiMode);
begin
  if FBiDiMode <> Value then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    Invalidate;
  end;
end;

procedure TJvButtonGlyph.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    Invalidate;
  end;
end;

procedure TJvButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then
        Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TJvButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;

procedure TJvButtonGlyph.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    GlyphChanged(Glyph);
  end;
end;

function TJvButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then
    State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then
    Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then
    Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TJvGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := Color {clBtnFace};
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then
      I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown,
        bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
            DDB.HandleType := bmDDB;
            if NumGlyphs > 1 then
              with TmpImage.Canvas do
              begin { Change white & gray to clBtnHighlight and clBtnShadow }
                CopyRect(IRect, DDB.Canvas, ORect);
                MonoBmp.Monochrome := True;
                MonoBmp.Width := IWidth;
                MonoBmp.Height := IHeight;

                { Convert white to clBtnHighlight }
                DDB.Canvas.Brush.Color := clWhite;
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnHighlight;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

                { Convert gray to clBtnShadow }
                DDB.Canvas.Brush.Color := clGray;
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnShadow;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

                { Convert transparent color to clBtnFace }
                DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := Color {clBtnFace};
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := Color {clBtnFace};
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexs[State] := TJvGlyphList(FGlyphList).AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TJvButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
begin
  if FOriginal = nil then
    Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then
    Exit;
  Index := CreateButtonGlyph(State);
  with GlyphPos do
    if Transparent or (State = bsExclusive) then
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        clNone, clNone, ILD_Transparent)
    else
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        ColorToRGB(Color {clBtnFace}), clNone, ILD_Normal);
end;

procedure TJvButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState);
var
  Flags: Longint;
begin
  Flags := 0;
  if FBiDiMode <> bdLeftToRight then
    Flags := DT_RTLREADING;
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, Flags);
    end
    else
      DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE or Flags);
  end;
end;

procedure TJvButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom - Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height)
  else
    GlyphSize := Point(0, 0);

  if Caption <> '' then
  begin
    CalcTextRect(Canvas, TextBounds, Caption);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom - TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0, 0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y + Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left + Offset.X);
  Inc(GlyphPos.Y, Client.Top + Offset.Y);
  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X,
    TextPos.Y + Client.Top + Offset.Y);
end;

function TJvButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent);
  DrawButtonText(Canvas, Caption, Result, State);
end;

function TJvButtonGlyph.DrawExternal(AGlyph: TBitmap; ANumGlyphs: TNumGlyphs; AColor: TColor; IgnoreOld: Boolean;
  Canvas: TCanvas; const Client: TRect; const Offset: TPoint; const Caption: string;
  Layout: TButtonLayout; Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean): TRect;
var
  OldGlyph: TBitmap;
  OldNumGlyphs: TNumGlyphs;
  OldColor: TColor;
begin
  OldGlyph := FOriginal;
  OldNumGlyphs := NumGlyphs;
  OldColor := FColor;
  try
    FOriginal := AGlyph;
    NumGlyphs := ANumGlyphs;
    FColor := AColor;
    GlyphChanged(FOriginal);
    Result := Draw(Canvas, Client, Offset, Caption, Layout, Margin,
      Spacing, State, Transparent);
  finally
    FOriginal := OldGlyph;
    NumGlyphs := OldNumGlyphs;
    FColor := OldColor;
    if not IgnoreOld then
      GlyphChanged(FOriginal);
  end;
end;

procedure TJvButtonGlyph.CalcTextRect(Canvas: TCanvas; var TextRect: TRect; const Caption: string);
begin
  TextRect := Rect(0, 0, TextRect.Right - TextRect.Left, 0);
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextRect, DT_CALCRECT);
end;

{ TJvExSpeedButton }

procedure TJvExSpeedButton.CMHintShow(var Msg: TCMHintShow);
begin
  if FHintWindowClass <> nil then
    Msg.HintInfo.HintWindowClass := FHintWindowClass;
  inherited;
end;

//=== { TJvNoFrameButton } ===================================================

constructor TJvNoFrameButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphDrawer := TJvButtonGlyph.Create;
  FNoBorder := True;
  FInitRepeatPause := 400;
  FRepeatPause := 100;
end;

destructor TJvNoFrameButton.Destroy;
begin
  FRepeatTimer.Free;
  FGlyphDrawer.Free;
  FGlyphDrawer := nil;
  inherited Destroy;
end;

procedure TJvNoFrameButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled and RepeatedClick then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);
    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled := True;
    FClicked := False;
  end;
end;

procedure TJvNoFrameButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  OrgMouseUp: TMouseEvent;
begin
  if FClicked then
  begin
    // prevent the OnClick event to trigger again
    if Assigned(OnMouseUp) then
      OnMouseUp(Self, Button, Shift, X, Y);
    OrgMouseUp := OnMouseUp;
    try
      OnMouseUp := nil;
      inherited MouseUp(Button, Shift, -1, -1)
    finally
      OnMouseUp := OrgMouseUp;
    end;
  end
  else
    inherited MouseUp(Button, Shift, X, Y);
  FreeAndNil(FRepeatTimer);
end;

procedure TJvNoFrameButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and Enabled and MouseCapture then
  begin
    try
      FClicked := True;
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end
  else
    FreeAndNil(FRepeatTimer);
end;

procedure TJvNoFrameButton.Paint;
begin
  if not Enabled then
  begin
    FState := bsDisabled;
    // FDragging := False;
  end
  else
  if FState = bsDisabled then
    if Down and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  if Assigned(FOnPaint) then
    FOnPaint(Self, Down, False, FState)
  else
    DefaultDrawing(Down, FState);
end;

procedure TJvNoFrameButton.DefaultDrawing(const IsDown: Boolean; const State: TButtonState);
const
  DownStyles: array [Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array [Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  Offset: TPoint;
begin
  if Flat and not NoBorder then
    inherited Paint
  else
  begin
    Canvas.Font := Self.Font;
    PaintRect := Rect(0, 0, Width, Height);
    if not NoBorder then
    begin
      DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
        FillStyles[Transparent] or BF_RECT);
      InflateRect(PaintRect, -1, -1);
    end;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(PaintRect);
    //if NoBorder and (csDesigning in ComponentState) then
    //  DrawDesignFrame(Canvas, PaintRect);
    InflateRect(PaintRect, -1, -1);

    if FState in [bsDown, bsExclusive] then
    begin
      if (FState = bsExclusive) then
      begin
        if Pattern = nil then
          CreateBrushPattern(clBtnFace, clBtnHighlight);
        Canvas.Brush.Bitmap := Pattern;
        Canvas.FillRect(PaintRect);
      end;
      Offset.X := 1;
      Offset.Y := 1;
    end
    else
    begin
      Offset.X := 0;
      Offset.Y := 0;
    end;
    {O}
    FGlyphDrawer.BiDiMode := BiDiMode;
    FGlyphDrawer.DrawExternal(Glyph, NumGlyphs, Color, True, Canvas, PaintRect, Offset, Caption, Layout, Margin,
      Spacing, FState, False {True});
  end;
end;

procedure TJvNoFrameButton.SetNoBorder(Value: Boolean);
begin
  if FNoBorder <> Value then
  begin
    FNoBorder := Value;
    Refresh;
  end;
end;

{ TJvComponentPanel }

constructor TJvComponentPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  BevelOuter := bvNone;
  FButtons := TList.Create;
  FFirstVisible := 0;
  FButtonWidth := 28;
  FButtonHeight := 28;
  FButtonLeft := TJvNoFrameButton.Create(Self);
  FButtonLeft.RepeatedClick := True;
  FButtonRight := TJvNoFrameButton.Create(Self);
  FButtonRight.RepeatedClick := True;
  FButtonPointer := TJvExSpeedButton.Create(Self);
  with FButtonLeft do
  begin
    Parent := Self;
    Tag := 0;
    Width := 12;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'JvComponentPanelLEFT');
    NumGlyphs := 2;
    OnClick := MoveClick;
  end;
  with FButtonRight do
  begin
    Parent := Self;
    Tag := 1;
    Width := 12;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'JvComponentPanelRIGHT');
    NumGlyphs := 2;
    OnClick := MoveClick;
  end;
  with FButtonPointer do
  begin
    Flat := True;
    Parent := Self;
    Top := 0;
    Glyph.LoadFromResourceName(HInstance, 'JvComponentPanelPOINTER');
    GroupIndex := 1;
    OnClick := BtnClick;
  end;
  SetMainButton;
end;

destructor TJvComponentPanel.Destroy;
var
  I: Integer;
begin
  for I := 0 to FButtons.Count - 1 do
    TJvExSpeedButton(FButtons[I]).Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TJvComponentPanel.CMHintShow(var Msg: TCMHintShow);
begin
  if FHintWindowClass <> nil then
    Msg.HintInfo.HintWindowClass := FHintWindowClass;
  inherited;
end;

procedure TJvComponentPanel.Invalidate;
begin
  if FLockUpdate = 0 then
    inherited Invalidate;
end;

procedure TJvComponentPanel.RecreateButtons;
var
  I: Integer;
  TmpNum: Integer;
begin
  TmpNum := FButtons.Count;
  for I := 0 to FButtons.Count - 1 do
    TJvExSpeedButton(FButtons[I]).Free;
  FButtons.Clear;
  FFirstVisible := 0;
  ButtonCount := TmpNum;
end;

procedure TJvComponentPanel.SetMainButton;
begin
  FButtonPointer.Down := True;
  FSelectButton := FButtonPointer;
end;

procedure TJvComponentPanel.SetSelectedButton(Value: Integer);
begin
  if (Value <> GetSelectedButton) and (Value >= -1) and (Value < ButtonCount) then
  begin
    if Value = -1 then
      SetMainButton
    else
    begin
      FSelectButton := Buttons[Value];
      FSelectButton.Down := True;
    end;
  end;
end;

function TJvComponentPanel.GetSelectedButton: Integer;
begin
  if FSelectButton <> nil then
  begin
    for Result := 0 to ButtonCount - 1 do
      if Buttons[Result] = FSelectButton then
        Exit;
  end;
  Result := -1;
end;

function TJvComponentPanel.GetButton(Index: Integer): TJvExSpeedButton;
begin
  if (Index < 0) or (Index > FButtons.Count - 1) then
    Result := nil
  else
    Result := TJvExSpeedButton(FButtons[Index]);
end;

function TJvComponentPanel.GetButtonCount: Integer;
begin
  Result := FButtons.Count;
end;

function TJvComponentPanel.GetVisibleCount: Integer;
begin
  Result := (Width - (12 + 12 + FButtonWidth)) div FButtonWidth;
end;

procedure TJvComponentPanel.SetButtonCount(AButtonCount: Integer);
var
  TmpButton: TJvExSpeedButton;
begin
  if AButtonCount < 0 then
    Exit;
  BeginUpdate;
  try
    SetMainButton;
    while FButtons.Count > AButtonCount do
    begin
      TJvExSpeedButton(FButtons[FButtons.Count - 1]).Free;
      FButtons.Delete(FButtons.Count - 1);
    end;
    while FButtons.Count < AButtonCount do
    begin
      TmpButton := TJvExSpeedButton.Create(Self);
      with TmpButton do
      begin
        Flat := True;
        Top := 0;
        GroupIndex := 1;
        HintWindowClass := Self.HintWindowClass;
        Parent := Self;
        OnClick := BtnClick;
        OnDblClick := BtnDblClick;
      end;
      FButtons.Add(TmpButton);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvComponentPanel.SetButtonWidth(AButtonWidth: Integer);
begin
  if FButtonWidth <> AButtonWidth then
  begin
    FButtonWidth := AButtonWidth;
    Resize;
  end;
end;

procedure TJvComponentPanel.SetButtonHeight(AButtonHeight: Integer);
begin
  if FButtonHeight <> AButtonHeight then
  begin
    FButtonHeight := AButtonHeight;
    Resize;
  end;
end;

procedure TJvComponentPanel.MoveClick(Sender: TObject);
begin
  case TJvExSpeedButton(Sender).Tag of
    0:
      if FFirstVisible > 0 then
        Dec(FFirstVisible);
    1:
      if FButtons.Count > FFirstVisible + VisibleCount then
        Inc(FFirstVisible);
  end;
  Resize;
end;

procedure TJvComponentPanel.Paint;
begin
  inherited Paint;
  PaintContent(ClientRect);
end;

procedure TJvComponentPanel.PaintContent(const R: TRect);
begin
  if Assigned(FOnPaintContent) then
    FOnPaintContent(Self, Canvas, R);
end;

procedure TJvComponentPanel.BtnClick(Sender: TObject);
begin
  if FSelectButton <> Sender then
  begin
    FSelectButton := TJvExSpeedButton(Sender);
    if Assigned(FOnClick) then
      FOnClick(Sender, FButtons.IndexOf(FSelectButton));
  end;
end;

procedure TJvComponentPanel.BtnDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Sender, FButtons.IndexOf(Sender));
end;

procedure TJvComponentPanel.WMSetText(var Msg: TWMSetText);
begin
  inherited;
  Caption := '';
end;

procedure TJvComponentPanel.Resize;
var
  I: Integer;
begin
  Height := FButtonHeight;
  if FButtonPointer = nil then
    Exit; // asn: for visualclx
  DisableAlign;
  try
    FButtonPointer.Height := FButtonHeight;
    FButtonPointer.Width := FButtonWidth;
    FButtonLeft.Height := FButtonHeight;
    FButtonRight.Height := FButtonHeight;
    FButtonPointer.Left := 0;
    FButtonLeft.Left := FButtonWidth + 6;
    FButtonRight.Left := (FButtonWidth + 12 + 6) + VisibleCount * FButtonWidth;
    FButtonLeft.Enabled := FFirstVisible > 0;
    FButtonRight.Enabled := FButtons.Count > FFirstVisible + VisibleCount;
    for I := 0 to FButtons.Count - 1 do
    begin
      if (I >= FFirstVisible) and (I < FFirstVisible + VisibleCount) then
        TJvExSpeedButton(FButtons[I]).SetBounds((FButtonWidth + 12 + 6) + (I - FFirstVisible) * FButtonWidth, 0, FButtonWidth, FButtonHeight)
      else
        TJvExSpeedButton(FButtons[I]).SetBounds(-100, 0, FButtonWidth, FButtonHeight);
    end;
  finally
    ControlState := ControlState - [csAlignmentNeeded];
    EnableAlign;
  end;
end;

function TJvComponentPanel.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
  begin
    Result := True;

    WheelDelta := WheelDelta div WHEEL_DELTA;
    while WheelDelta <> 0 do
    begin
      if WheelDelta < 0 then
      begin
        if ButtonRight.Enabled then
          ButtonRight.Click
        else
          Break;
      end
      else
      begin
        if ButtonLeft.Enabled then
          ButtonLeft.Click
        else
          Break;
      end;

      if WheelDelta < 0 then
        Inc(WheelDelta)
      else
        Dec(WheelDelta);
    end;
  end;
end;

procedure TJvComponentPanel.SetFirstVisible(AButton: Integer);
begin
  if AButton >= ButtonCount then
    AButton := ButtonCount - 1;
  if AButton < 0 then
    AButton := 0;
  if FFirstVisible <> AButton then
  begin
    FFirstVisible := AButton;
    Resize;
  end;
end;

procedure TJvComponentPanel.BeginUpdate;
begin
  Inc(FLockUpdate);
  DisableAlign;
end;

procedure TJvComponentPanel.EndUpdate;
begin
  Dec(FLockUpdate);
  if FLockUpdate = 0 then
  begin
    Resize;
    ControlState := ControlState - [csAlignmentNeeded];
    EnableAlign;
  end;
end;

end.
