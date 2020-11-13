inherited FrameOptionPageOldPalette: TFrameOptionPageOldPalette
  Width = 540
  Height = 226
  TabStop = True
  ExplicitWidth = 540
  ExplicitHeight = 226
  inherited pnlClient: TPanel
    Width = 540
    Height = 177
    ExplicitWidth = 540
    ExplicitHeight = 177
    object lblStyleCaption: TLabel
      Left = 24
      Top = 127
      Width = 27
      Height = 14
      Caption = 'Style:'
      FocusControl = cbxStyle
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Style = []
      ParentFont = False
    end
    object cbxActive: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Hint = 'Activates the <b>Old Component Palette</b>.'
      Caption = '&Active'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = cbxActiveClick
    end
    object cbxMultiline: TCheckBox
      Left = 24
      Top = 31
      Width = 172
      Height = 17
      Caption = 'Multiline'
      TabOrder = 1
    end
    object cbxRaggedRight: TCheckBox
      Left = 24
      Top = 54
      Width = 172
      Height = 17
      Caption = 'Ragged Right'
      TabOrder = 2
    end
    object cbxStyle: TComboBox
      Left = 24
      Top = 143
      Width = 121
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      Items.Strings = (
        'Tabs'
        'Buttons'
        'Flat Buttons')
    end
    object chkAlphaSortPopupMenu: TCheckBox
      Left = 24
      Top = 77
      Width = 172
      Height = 17
      Caption = 'AlphaSort palette popup menu'
      TabOrder = 3
    end
    object chkSmallFonts: TCheckBox
      Left = 24
      Top = 100
      Width = 172
      Height = 17
      Caption = 'Use "Small Fonts 7"'
      TabOrder = 4
    end
  end
  inherited pnlDescription: TPanel
    Width = 540
    ExplicitWidth = 540
    inherited bvlSplitter: TBevel
      Width = 540
      ExplicitWidth = 540
    end
    inherited lblDescription: TLabel
      Width = 180
      Caption = 'Configure the old component palette.'
      ExplicitWidth = 180
    end
  end
end
