object FrameBase: TFrameBase
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object pnlClient: TPanel
    Left = 0
    Top = 49
    Width = 320
    Height = 191
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlDescription: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 49
    Align = alTop
    BevelOuter = bvNone
    Color = clWindow
    ParentBackground = False
    TabOrder = 1
    object bvlSplitter: TBevel
      Left = 0
      Top = 47
      Width = 320
      Height = 2
      Align = alBottom
    end
    object lblDescription: TLabel
      Left = 16
      Top = 24
      Width = 73
      Height = 13
      Caption = 'lblDescription'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object lblCaption: TLabel
      Left = 16
      Top = 8
      Width = 54
      Height = 13
      Caption = 'lblCaption'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
