inherited FrameOptionPageFormDesigner: TFrameOptionPageFormDesigner
  Width = 372
  Height = 175
  inherited pnlClient: TPanel
    Width = 372
    Height = 126
    object cbxActive: TCheckBox
      Left = 8
      Top = 8
      Width = 113
      Height = 17
      Caption = '&Active'
      TabOrder = 0
      OnClick = cbxActiveClick
    end
    object cbxLabelMargin: TCheckBox
      Left = 24
      Top = 31
      Width = 313
      Height = 17
      Caption = 'Set TLabel.Margins.Bottom to zero'
      TabOrder = 1
    end
    object chkRemoveExplicitProperties: TCheckBox
      Left = 24
      Top = 54
      Width = 313
      Height = 17
      Caption = 'Do not store the Explicit* properties into the DFM'
      TabOrder = 2
    end
  end
  inherited pnlDescription: TPanel
    Width = 372
    inherited bvlSplitter: TBevel
      Width = 372
    end
    inherited lblDescription: TLabel
      Width = 212
      Caption = 'Configure the form designer enhancements.'
    end
  end
end
