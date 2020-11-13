object FrameOldPalette: TFrameOldPalette
  Left = 0
  Top = 0
  Width = 451
  Height = 56
  Align = alTop
  AutoSize = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  Visible = False
  object TabControl: TTabControl
    Left = 0
    Top = 0
    Width = 451
    Height = 56
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Style = []
    MultiLine = True
    ParentFont = False
    PopupMenu = PopupMenuPalette
    RaggedRight = True
    TabOrder = 0
    OnChange = TabControlChange
    OnResize = TabControlResize
    object PanelPalette: TPanel
      Left = 4
      Top = 6
      Width = 443
      Height = 29
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 0
      object PanelSpacer: TPanel
        Left = 0
        Top = 28
        Width = 443
        Height = 1
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
      end
      object Palette: TJvComponentPanel
        Left = 0
        Top = 0
        Width = 443
        Height = 28
        Align = alTop
        OnClick = PaletteClick
        OnDblClick = PaletteDblClick
        OnPaintContent = PalettePaintContent
        PopupMenu = PopupMenuPalette
      end
    end
  end
  object PopupMenuPalette: TPopupMenu
    Left = 192
    Top = 24
  end
end
