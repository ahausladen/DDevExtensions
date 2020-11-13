inherited FrameOptionPageComponentSelector: TFrameOptionPageComponentSelector
  Width = 324
  Height = 211
  TabStop = True
  inherited pnlClient: TPanel
    Width = 324
    Height = 162
    object lblHotkey: TLabel
      Left = 24
      Top = 77
      Width = 37
      Height = 13
      Caption = '&Hotkey:'
      FocusControl = HotKey
    end
    object cbxActive: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = '&Active'
      TabOrder = 0
      OnClick = cbxActiveClick
    end
    object cbxSimpleSearch: TCheckBox
      Left = 24
      Top = 31
      Width = 97
      Height = 17
      Caption = '&Simple search'
      TabOrder = 1
    end
    object cbxSortByPalette: TCheckBox
      Left = 24
      Top = 54
      Width = 97
      Height = 17
      Caption = 'Sort by &palette'
      TabOrder = 2
    end
    object HotKey: THotKey
      Left = 24
      Top = 93
      Width = 121
      Height = 19
      HotKey = 0
      Modifiers = []
      TabOrder = 3
    end
  end
  inherited pnlDescription: TPanel
    Width = 324
    inherited bvlSplitter: TBevel
      Width = 324
    end
    inherited lblDescription: TLabel
      Width = 205
      Caption = 'Configures the ComponentSelected toolbar.'
    end
  end
end
