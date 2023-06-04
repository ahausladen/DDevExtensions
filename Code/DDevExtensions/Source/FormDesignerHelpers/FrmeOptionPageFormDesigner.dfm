inherited FrameOptionPageFormDesigner: TFrameOptionPageFormDesigner
  Width = 372
  Height = 175
  ParentFont = False
  ExplicitWidth = 372
  ExplicitHeight = 175
  inherited pnlClient: TPanel
    Width = 372
    Height = 126
    ExplicitWidth = 372
    ExplicitHeight = 126
    object cbxActive: TCheckBox
      Left = 8
      Top = 8
      Width = 113
      Height = 17
      Caption = '&Active'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = cbxActiveClick
    end
    object cbxLabelMargin: TCheckBox
      Left = 24
      Top = 31
      Width = 313
      Height = 17
      Caption = 'Set TLabel.Margins.Bottom to zero'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object chkRemoveExplicitProperties: TCheckBox
      Left = 24
      Top = 54
      Width = 313
      Height = 17
      Caption = 'Do not store the Explicit* properties into the DFM'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  inherited pnlDescription: TPanel
    Width = 372
    ParentFont = False
    ExplicitWidth = 372
    inherited bvlSplitter: TBevel
      Width = 372
      ExplicitWidth = 372
    end
    inherited lblDescription: TLabel
      Width = 348
      Height = 25
      Caption = 'Configure the form designer enhancements.'
      ParentFont = False
      ExplicitWidth = 348
      ExplicitHeight = 25
    end
  end
end
