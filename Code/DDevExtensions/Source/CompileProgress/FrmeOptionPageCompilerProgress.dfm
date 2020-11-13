inherited FrameOptionPageCompilerProgress: TFrameOptionPageCompilerProgress
  Width = 403
  Height = 222
  TabStop = True
  inherited pnlClient: TPanel
    Width = 403
    Height = 173
    object cbxDisableRebuildDlg: TCheckBox
      Left = 8
      Top = 52
      Width = 241
      Height = 19
      Caption = '&Disable "Source modified. Rebuild?" dialog'
      TabOrder = 2
    end
    object chkAutoSaveAfterSuccessfulCompile: TCheckBox
      Left = 8
      Top = 75
      Width = 241
      Height = 19
      Caption = 'AutoSave &editor files after successful compile'
      TabOrder = 3
    end
    object chkLastCompileVersionInfo: TCheckBox
      Left = 8
      Top = 121
      Width = 241
      Height = 19
      Caption = 'Put "Last Compile" time into Version Info'
      TabOrder = 5
      OnClick = chkLastCompileVersionInfoClick
    end
    object edtLastCompileVersionInfoFormat: TEdit
      Left = 32
      Top = 141
      Width = 217
      Height = 21
      TabOrder = 6
    end
    object chkAskBeforeCompilingFileFromDiffernetProject: TCheckBox
      Left = 8
      Top = 98
      Width = 361
      Height = 19
      Caption = 'Ask before compiling if current file is from a different project'
      TabOrder = 4
    end
    object chkReleaseCompilerUnitCache: TCheckBox
      Left = 8
      Top = 7
      Width = 361
      Height = 19
      Caption = 'Release compiler unit cache of other projects before compiling'
      TabOrder = 0
      OnClick = chkReleaseCompilerUnitCacheClick
    end
    object chkReleaseCompilerUnitCacheHigh: TCheckBox
      Left = 24
      Top = 29
      Width = 361
      Height = 19
      Caption = 'Release only if memory usage is high'
      TabOrder = 1
    end
  end
  inherited pnlDescription: TPanel
    Width = 403
    inherited bvlSplitter: TBevel
      Width = 403
    end
    inherited lblDescription: TLabel
      Width = 191
      Caption = 'Configures the IDE'#39's compile behaviour.'
      ExplicitWidth = 191
    end
  end
end
