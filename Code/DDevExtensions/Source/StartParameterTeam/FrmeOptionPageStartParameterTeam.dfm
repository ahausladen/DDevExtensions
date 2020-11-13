inherited FrameOptionPageStartParameterTeam: TFrameOptionPageStartParameterTeam
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
    end
  end
  inherited pnlDescription: TPanel
    Width = 385
    inherited bvlSplitter: TBevel
      Width = 385
    end
    inherited lblDescription: TLabel
      Width = 276
      Caption = 'The start parameters aren'#39't stored in the dproj/cbproj file'
    end
  end
end
