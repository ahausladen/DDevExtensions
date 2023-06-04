inherited FormFileSelector: TFormFileSelector
  Left = 236
  Top = 135
  ActiveControl = edtFilter
  BorderIcons = [biSystemMenu]
  Caption = 'Find/Use Unit'
  ClientHeight = 517
  ClientWidth = 760
  Constraints.MinHeight = 300
  Constraints.MinWidth = 500
  PopupMode = pmAuto
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnResize = FormResize
  ExplicitWidth = 782
  ExplicitHeight = 573
  TextHeight = 13
  object ListView: TListView
    Left = 0
    Top = 25
    Width = 760
    Height = 387
    Align = alClient
    Columns = <
      item
        Caption = 'Unit'
        Width = 220
      end
      item
        Caption = 'Form'
        Width = 180
      end
      item
        Caption = 'Filename'
        Tag = 1
        Width = 250
      end>
    DragMode = dmAutomatic
    HideSelection = False
    MultiSelect = True
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    ParentShowHint = False
    PopupMenu = popListView
    ShowHint = True
    TabOrder = 1
    ViewStyle = vsReport
    OnColumnClick = ListViewColumnClick
    OnCustomDrawItem = ListViewCustomDrawItem
    OnData = ListViewData
    OnDblClick = ListViewDblClick
    OnDragDrop = ListViewDragDrop
    OnDragOver = ListViewDragOver
    OnInfoTip = ListViewInfoTip
    OnKeyDown = ListViewKeyDown
    OnKeyUp = ListViewKeyUp
    OnMouseDown = ListViewMouseDown
    OnSelectItem = ListViewSelectItem
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 498
    Width = 760
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 120
      end
      item
        Width = 50
      end>
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 760
    Height = 25
    BorderWidth = 1
    ButtonHeight = 21
    Caption = 'ToolBar'
    Indent = 4
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Transparent = True
    Wrapable = False
    object tbExportToExcel: TToolButton
      Left = 4
      Top = 0
      Action = ActionExportToExcel
    end
    object tbExportSeparator: TToolButton
      Left = 27
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object edtFilter: TEdit
      Left = 35
      Top = 0
      Width = 178
      Height = 21
      Hint = 'Filter'
      TabOrder = 0
      OnChange = edtFilterChange
      OnEnter = edtFilterEnter
      OnExit = edtFilterExit
      OnKeyDown = edtFilterKeyDown
      OnKeyPress = edtFilterKeyPress
      OnKeyUp = edtFilterKeyUp
    end
    object ToolButton5: TToolButton
      Left = 213
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 4
      Style = tbsSeparator
    end
    object cbxFilterField: TComboBox
      Left = 221
      Top = 0
      Width = 152
      Height = 22
      Style = csOwnerDrawFixed
      Ctl3D = True
      DropDownCount = 10
      ParentCtl3D = False
      TabOrder = 1
      OnChange = cbxFilterFieldChange
      OnDrawItem = cbxFilterFieldDrawItem
    end
    object tsepDir: TToolButton
      Left = 373
      Top = 0
      Width = 8
      Caption = 'tsepDir'
      ImageIndex = 5
      Style = tbsSeparator
    end
    object cbxFilterDirectory: TComboBox
      Left = 381
      Top = 0
      Width = 302
      Height = 21
      Style = csOwnerDrawFixed
      DropDownCount = 25
      TabOrder = 2
      OnChange = cbxFilterDirectoryChange
      OnDrawItem = cbxFilterDirectoryDrawItem
    end
  end
  object PanelButtons: TPanel
    Left = 0
    Top = 467
    Width = 760
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      760
      31)
    object btnUseUnit: TButton
      Left = 599
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Use Unit'
      Default = True
      ModalResult = 6
      TabOrder = 3
    end
    object btnCancel: TButton
      Left = 680
      Top = 3
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object chkUseUnitsImplementation: TCheckBox
      Left = 4
      Top = 7
      Width = 237
      Height = 17
      Caption = 'Add to &implementation uses list  (Shift)'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 0
    end
    object btnOpen: TButton
      Left = 470
      Top = 3
      Width = 108
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Open (Ctrl+Enter)'
      ModalResult = 1
      ParentShowHint = False
      ShowHint = False
      TabOrder = 2
    end
    object btnOptions: TButton
      Left = 232
      Top = 3
      Width = 108
      Height = 25
      Caption = '&Options...'
      TabOrder = 1
      OnClick = btnOptionsClick
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 412
    Width = 760
    Height = 55
    Align = alBottom
    BevelOuter = bvNone
    Padding.Top = 4
    TabOrder = 4
    object ListViewInsertUnits: TListView
      Left = 0
      Top = 4
      Width = 656
      Height = 51
      Align = alClient
      Columns = <>
      DragMode = dmAutomatic
      MultiSelect = True
      ReadOnly = True
      ParentShowHint = False
      PopupMenu = popListViewInsertUnits
      ShowHint = True
      SortType = stText
      TabOrder = 0
      ViewStyle = vsList
      OnDragDrop = ListViewInsertUnitsDragDrop
      OnDragOver = ListViewInsertUnitsDragOver
      OnInfoTip = ListViewInfoTip
      OnKeyDown = ListViewInsertUnitsKeyDown
      ExplicitWidth = 646
    end
    object Panel1: TPanel
      Left = 656
      Top = 4
      Width = 114
      Height = 51
      Align = alRight
      BevelOuter = bvNone
      Padding.Left = 6
      Padding.Top = 4
      Padding.Right = 6
      Padding.Bottom = 6
      TabOrder = 1
      ExplicitLeft = 646
      object Label1: TLabel
        Left = 6
        Top = 4
        Width = 102
        Height = 41
        Align = alClient
        AutoSize = False
        Caption = 'Use the [Insert] key or Drag&&Drop to add units to the list.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
    end
  end
  object ActionList: TActionList
    Left = 112
    Top = 64
    object ActionExportToExcel: TAction
      Caption = '&Excel Export'
      Hint = 'Export to Excel'
      ImageIndex = 0
      OnExecute = ActionExportToExcelExecute
    end
    object ActionAddToInsertList: TAction
      Caption = '&Add to Insert List'
      ShortCut = 45
      OnExecute = ActionAddToInsertListExecute
      OnUpdate = ActionAddToInsertListUpdate
    end
    object ActionRemoveFromInsertList: TAction
      Caption = '&Remove from Insert List'
      OnExecute = ActionRemoveFromInsertListExecute
      OnUpdate = ActionRemoveFromInsertListUpdate
    end
    object ActionClearInsertList: TAction
      Caption = '&Clear Insert List'
      OnExecute = ActionClearInsertListExecute
      OnUpdate = ActionClearInsertListUpdate
    end
  end
  object TimerFilterUpdate: TTimer
    Enabled = False
    Interval = 60
    OnTimer = TimerFilterUpdateTimer
    Left = 40
    Top = 64
  end
  object popListView: TPopupMenu
    Left = 160
    Top = 64
    object mniOpenFile: TMenuItem
      Caption = '&Open in editor'
      ShortCut = 16397
      OnClick = mniOpenFileClick
    end
    object mniAddtoInsertList: TMenuItem
      Action = ActionAddToInsertList
    end
  end
  object popListViewInsertUnits: TPopupMenu
    Left = 88
    Top = 424
    object mniRemovefromInsertList: TMenuItem
      Action = ActionRemoveFromInsertList
    end
    object ClearInsertList1: TMenuItem
      Action = ActionClearInsertList
    end
  end
  object popOptions: TPopupMenu
    Left = 40
    Top = 144
    object mniAllowMoveFromInterfaceToImpl: TMenuItem
      AutoCheck = True
      Caption = 'Allow move from interface to implementation'
    end
    object mniEveryUnitOnSingleLine: TMenuItem
      AutoCheck = True
      Caption = 'Every unit on a single line'
    end
  end
end
