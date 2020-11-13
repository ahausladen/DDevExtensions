inherited FrameOptionPageCompilerEnhancements: TFrameOptionPageCompilerEnhancements
  Width = 385
  Height = 235
  TabStop = True
  ExplicitWidth = 385
  ExplicitHeight = 235
  inherited pnlClient: TPanel
    Width = 385
    Height = 186
    ExplicitWidth = 385
    ExplicitHeight = 186
    object lblExceptWarningsCaption: TLabel
      Left = 40
      Top = 54
      Width = 243
      Height = 13
      Caption = 'Except these warnings: (example: W1000,W1054)'
      FocusControl = mnoExceptWarnings
    end
    object cbxActive: TCheckBox
      Left = 8
      Top = 8
      Width = 113
      Height = 17
      Caption = '&Active'
      TabOrder = 0
      OnClick = cbxActiveClick
    end
    object cbxTreatWarningsAsErrors: TCheckBox
      Left = 24
      Top = 31
      Width = 241
      Height = 17
      Caption = 'Treat &warnings as errors'
      TabOrder = 1
      OnClick = cbxTreatWarningsAsErrorsClick
    end
    object mnoExceptWarnings: TMemo
      Left = 40
      Top = 70
      Width = 337
      Height = 89
      ScrollBars = ssVertical
      TabOrder = 2
    end
  end
  inherited pnlDescription: TPanel
    Width = 385
    ExplicitWidth = 385
    inherited bvlSplitter: TBevel
      Width = 385
      ExplicitWidth = 385
    end
    inherited lblDescription: TLabel
      Width = 164
      Caption = 'Treat compiler warnings as errors.'
      ExplicitWidth = 164
    end
  end
end
