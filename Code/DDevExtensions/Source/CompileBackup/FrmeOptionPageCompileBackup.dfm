inherited FrameOptionPageCompileBackup: TFrameOptionPageCompileBackup
  Width = 507
  Height = 241
  TabStop = True
  inherited pnlClient: TPanel
    Width = 507
    Height = 192
    object cbxActive: TCheckBox
      Left = 8
      Top = 8
      Width = 129
      Height = 17
      Caption = '&Active'
      TabOrder = 0
      OnClick = cbxActiveClick
    end
    object cbxDeleteBackupAfterClose: TCheckBox
      Left = 24
      Top = 31
      Width = 273
      Height = 17
      Caption = 'Delete backup files after the file is closed'
      TabOrder = 1
    end
  end
  inherited pnlDescription: TPanel
    Width = 507
    inherited bvlSplitter: TBevel
      Width = 507
    end
    inherited lblDescription: TLabel
      Width = 321
      Caption = 
        'Creates a .cbk file for every unsaved file when compiling a proj' +
        'ect.'
    end
  end
end
