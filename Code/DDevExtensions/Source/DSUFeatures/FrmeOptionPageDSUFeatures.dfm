inherited FrameOptionPageDSUFeatures: TFrameOptionPageDSUFeatures
  Width = 385
  Height = 366
  TabStop = True
  ExplicitWidth = 385
  ExplicitHeight = 366
  inherited pnlClient: TPanel
    Width = 385
    Height = 317
    ExplicitWidth = 385
    ExplicitHeight = 317
    object lblEditorTabDblClickAction: TLabel
      Left = 8
      Top = 216
      Width = 140
      Height = 13
      Caption = 'Editor tab double click action:'
      FocusControl = cbxEditorTabDblClickAction
    end
    object LabelStructureViewSearchHotkey: TLabel
      Left = 8
      Top = 259
      Width = 142
      Height = 13
      Caption = 'Structure View Search hotkey'
      FocusControl = HotKeyStructureViewSearch
    end
    object chkDisablePackageCache: TCheckBox
      Left = 8
      Top = 8
      Width = 337
      Height = 17
      Caption = 'Disable Package Cache (requires IDE restart)'
      TabOrder = 0
    end
    object cbxEditorTabDblClickAction: TComboBox
      Left = 8
      Top = 232
      Width = 140
      Height = 21
      Style = csDropDownList
      TabOrder = 11
      Items.Strings = (
        'Disabled'
        'Zoom'
        'Super-Zoom')
    end
    object chkDisableSourceFormatterHotkey: TCheckBox
      Left = 8
      Top = 31
      Width = 337
      Height = 17
      Caption = 'Disable Source Formatter hotkey (Ctrl+D)'
      TabOrder = 2
    end
    object chkShowFileProjectInPrjMgr: TCheckBox
      Left = 8
      Top = 54
      Width = 337
      Height = 17
      Caption = 'Show project for active file in Project Manager'
      TabOrder = 3
    end
    object HotKeyStructureViewSearch: THotKey
      Left = 8
      Top = 274
      Width = 121
      Height = 19
      HotKey = 0
      Modifiers = []
      TabOrder = 12
    end
    object chkIncBuildNumOnBuildOnly: TCheckBox
      Left = 8
      Top = 299
      Width = 337
      Height = 17
      Caption = 'Increment Build Number only when building the project'
      TabOrder = 8
    end
    object chkDisableCodeFolding: TCheckBox
      Left = 8
      Top = 39
      Width = 337
      Height = 17
      Caption = 'Disable Code Folding (better performance for large units)'
      TabOrder = 1
    end
    object chkReplaceOpenFileAtCursor: TCheckBox
      Left = 8
      Top = 77
      Width = 337
      Height = 17
      Caption = 'Replace Open File At Cursor (Projectgroup support)'
      TabOrder = 4
    end
    object chkShowAllFrames: TCheckBox
      Left = 8
      Top = 100
      Width = 337
      Height = 17
      Caption = 'Show all inheritable modules (Projectgroup support)'
      TabOrder = 5
    end
    object chkDontBreakOnSpawnedProcesses: TCheckBox
      Left = 8
      Top = 123
      Width = 337
      Height = 17
      Hint = 
        'The debugger breaks the application when a spawned process is st' +
        'arted.'#13#10'With this option you can disable this "feature", so you ' +
        'do not have to'#13#10'resume the process after a CreateProcess call.'
      Caption = 'Don'#39't break when starting spawned processes'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
    end
    object chkKillDExplore: TCheckBox
      Left = 8
      Top = 146
      Width = 337
      Height = 17
      Hint = 
        'Document Explorer (dexplore.exe) that is used to show the contex' +
        't help,'#13#10'sometimes isn'#39't terminated what prevents Windows from s' +
        'hutting down.'#13#10'If this option is active, all running dexplore.ex' +
        'e will be terminated when'#13#10'the IDE is closed.'
      Caption = 'Kill all dexplore.exe when closing the IDE'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
    end
    object chkConfirmDlgOnDebugCtrlF1: TCheckBox
      Left = 8
      Top = 169
      Width = 337
      Height = 17
      Hint = 
        'During debugging sometimes it happens that you press Ctrl+F1 (He' +
        'lp) instead of'#13#10'Ctrl+F2 (Stop) and then you have to wait for dex' +
        'plore.exe to start.'#13#10'If this option is active, Ctrl+F1 will show' +
        ' a confirmation dialog before invoking the help.'
      Caption = 'Show confirmation dialog for Ctrl+F1 while debugging'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 9
    end
    object chkDisableAlphaSortClassCompletion: TCheckBox
      Left = 8
      Top = 192
      Width = 337
      Height = 17
      Caption = 'Disable alpha-sort class completion'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
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
      Width = 157
      Caption = 'Configure extended IDE settings'
      ExplicitWidth = 157
    end
  end
end
