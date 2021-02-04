inherited FormTreePages: TFormTreePages
  Left = 267
  Top = 292
  BorderStyle = bsDialog
  Caption = 'TreePages'
  ClientHeight = 440
  ClientWidth = 701
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanelButtons: TPanel
    Left = 0
    Top = 403
    Width = 701
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object bvlDivider: TBevel
      Left = 0
      Top = 0
      Width = 701
      Height = 2
      Align = alTop
    end
    object btnOk: TButton
      Left = 539
      Top = 6
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 620
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object PanelWorkingArea: TPanel
    Left = 0
    Top = 0
    Width = 701
    Height = 403
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    object TreeView: TTreeView
      Left = 5
      Top = 5
      Width = 177
      Height = 393
      Align = alLeft
      HideSelection = False
      Indent = 19
      ReadOnly = True
      RightClickSelect = True
      RowSelect = True
      TabOrder = 0
      OnChange = TreeViewChange
    end
    object PanelClient: TPanel
      Left = 182
      Top = 5
      Width = 514
      Height = 393
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object SplitterTree: TSplitter
        Left = 0
        Top = 0
        Height = 393
        AutoSnap = False
      end
    end
  end
end
