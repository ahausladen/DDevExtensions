inherited FrameOptionPageUnitSelector: TFrameOptionPageUnitSelector
  Height = 187
  TabStop = True
  inherited pnlClient: TPanel
    Height = 138
    TabOrder = 1
    object lblFindUseUnitHotKey: TLabel
      Left = 8
      Top = 31
      Width = 82
      Height = 13
      Caption = '&Find Unit hotkey:'
      FocusControl = hkFindUseUnit
    end
    object cbxUseUnitSelector: TCheckBox
      Left = 8
      Top = 75
      Width = 153
      Height = 17
      Caption = '&Use Unit Selector'
      TabOrder = 2
    end
    object hkFindUseUnit: THotKey
      Left = 8
      Top = 50
      Width = 161
      Height = 19
      HotKey = 0
      Modifiers = []
      TabOrder = 0
    end
    object chkReplaceUseUnit: TCheckBox
      Left = 8
      Top = 8
      Width = 249
      Height = 17
      Caption = 'Replace "Use Unit..." by Find/Use Unit dialog'
      TabOrder = 1
    end
  end
  inherited pnlDescription: TPanel
    TabOrder = 0
    inherited lblDescription: TLabel
      Width = 253
      Caption = 'Configure the Unit Selector and Find/Use Unit dialog.'
    end
  end
end
