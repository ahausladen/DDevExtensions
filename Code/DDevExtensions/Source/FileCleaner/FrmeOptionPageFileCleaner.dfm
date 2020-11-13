inherited FrameOptionPageFileCleaner: TFrameOptionPageFileCleaner
  Width = 385
  Height = 200
  TabStop = True
  inherited pnlClient: TPanel
    Width = 385
    Height = 151
    object cbxActive: TCheckBox
      Left = 8
      Top = 8
      Width = 113
      Height = 17
      Caption = '&Active'
      TabOrder = 0
      OnClick = cbxActiveClick
    end
    object cbxDeleteDdp: TCheckBox
      Left = 24
      Top = 31
      Width = 177
      Height = 17
      Caption = 'Automatically delete .ddp files'
      TabOrder = 1
    end
    object cbxRemoveEmptyHistory: TCheckBox
      Left = 24
      Top = 77
      Width = 257
      Height = 17
      Caption = 'Automatically delete empty "__history" directories'
      TabOrder = 2
      Visible = False
    end
    object cbxRemoveEmptyModel: TCheckBox
      Left = 24
      Top = 54
      Width = 257
      Height = 17
      Caption = 'Automatically delete empty "Model" directories'
      TabOrder = 3
      Visible = False
    end
  end
  inherited pnlDescription: TPanel
    Width = 385
    inherited bvlSplitter: TBevel
      Width = 385
    end
    inherited lblDescription: TLabel
      Width = 174
      Caption = 'Configure the automatic file deleter.'
    end
  end
end
