inherited FormManageProjectSetting: TFormManageProjectSetting
  Left = 380
  Top = 259
  Width = 570
  Height = 393
  BorderIcons = [biSystemMenu]
  Caption = 'Manage Configurations'
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 394
    Top = 328
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 475
    Top = 328
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object pnlClient: TPanel
    Left = 8
    Top = 8
    Width = 542
    Height = 314
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 0
    object spltProjects: TSplitter
      Left = 291
      Top = 0
      Width = 6
      Height = 310
      Align = alRight
      AutoSnap = False
      Beveled = True
      ResizeStyle = rsUpdate
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 291
      Height = 310
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object spltLocalGlobal: TSplitter
        Left = 0
        Top = 105
        Width = 291
        Height = 5
        Cursor = crVSplit
        Align = alTop
        AutoSnap = False
        Beveled = True
        ResizeStyle = rsUpdate
      end
      object pnlGlobal: TPanel
        Left = 0
        Top = 110
        Width = 291
        Height = 200
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object lblGlobalSettings: TLabel
          Left = 0
          Top = 0
          Width = 291
          Height = 13
          Align = alTop
          AutoSize = False
          Caption = ' Global Configurations'
          Color = clNavy
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object lvwGlobal: TListView
          Left = 0
          Top = 35
          Width = 291
          Height = 165
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              AutoSize = True
            end>
          ColumnClick = False
          HideSelection = False
          RowSelect = True
          PopupMenu = popGlobalSettings
          ShowColumnHeaders = False
          SmallImages = DataModuleImages.imlIcons
          SortType = stText
          TabOrder = 1
          ViewStyle = vsReport
          OnDblClick = lvwGlobalDblClick
          OnEdited = lvwGlobalEdited
          OnEditing = lvwGlobalEditing
          OnKeyDown = lvwGlobalKeyDown
        end
        object tbToolbarGlobal: TToolBar
          Left = 0
          Top = 13
          Width = 291
          Height = 22
          EdgeBorders = [ebBottom]
          EdgeOuter = esNone
          Images = DataModuleImages.imlIcons
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Transparent = False
          Wrapable = False
          object ToolButton7: TToolButton
            Left = 0
            Top = 2
            Action = actGlobalNewSetting
          end
          object ToolButton8: TToolButton
            Left = 23
            Top = 2
            Action = actGlobalEditSetting
          end
          object ToolButton13: TToolButton
            Left = 46
            Top = 2
            Action = actGlobalEditOptions
          end
          object ToolButton9: TToolButton
            Left = 69
            Top = 2
            Width = 8
            Caption = 'ToolButton3'
            ImageIndex = 2
            Style = tbsSeparator
          end
          object ToolButton10: TToolButton
            Left = 77
            Top = 2
            Action = actGlobalDeleteSetting
          end
          object ToolButton11: TToolButton
            Left = 100
            Top = 2
            Width = 8
            Caption = 'ToolButton5'
            ImageIndex = 0
            Style = tbsSeparator
          end
          object ToolButton12: TToolButton
            Left = 108
            Top = 2
            Action = actGlobalAssignSetting
          end
        end
      end
      object pnlLocal: TPanel
        Left = 0
        Top = 0
        Width = 291
        Height = 105
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lblLocalSettings: TLabel
          Left = 0
          Top = 0
          Width = 291
          Height = 13
          Align = alTop
          AutoSize = False
          Caption = ' Configurations'
          Color = clNavy
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWhite
          Font.Height = -11
          Font.Style = []
          ParentColor = False
          ParentFont = False
          Transparent = False
        end
        object lvwLocal: TListView
          Left = 0
          Top = 35
          Width = 291
          Height = 70
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              AutoSize = True
            end>
          ColumnClick = False
          HideSelection = False
          RowSelect = True
          PopupMenu = popLocalSettings
          ShowColumnHeaders = False
          SmallImages = DataModuleImages.imlIcons
          SortType = stText
          TabOrder = 1
          ViewStyle = vsReport
          OnDblClick = lvwLocalDblClick
          OnEdited = lvwGlobalEdited
          OnEditing = lvwGlobalEditing
          OnKeyDown = lvwGlobalKeyDown
        end
        object tbToolbarLocal: TToolBar
          Left = 0
          Top = 13
          Width = 291
          Height = 22
          EdgeBorders = [ebBottom]
          EdgeOuter = esNone
          Images = DataModuleImages.imlIcons
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Transparent = False
          Wrapable = False
          object ToolButton1: TToolButton
            Left = 0
            Top = 2
            Action = actLocalNewSetting
          end
          object ToolButton2: TToolButton
            Left = 23
            Top = 2
            Action = actLocalEditSetting
          end
          object ToolButton14: TToolButton
            Left = 46
            Top = 2
            Action = actLocalEditOptions
          end
          object ToolButton3: TToolButton
            Left = 69
            Top = 2
            Width = 8
            Caption = 'ToolButton3'
            ImageIndex = 2
            Style = tbsSeparator
          end
          object ToolButton4: TToolButton
            Left = 77
            Top = 2
            Action = actLocalDeleteSetting
          end
          object ToolButton5: TToolButton
            Left = 100
            Top = 2
            Width = 8
            Caption = 'ToolButton5'
            ImageIndex = 0
            Style = tbsSeparator
          end
          object ToolButton6: TToolButton
            Left = 108
            Top = 2
            Action = actLocalAssignSetting
          end
        end
      end
    end
    object pnlProjects: TPanel
      Left = 297
      Top = 0
      Width = 241
      Height = 310
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object lblProjects: TLabel
        Left = 0
        Top = 0
        Width = 241
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = ' Projects'
        Color = clNavy
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = False
      end
      object lvwProjects: TListView
        Left = 0
        Top = 35
        Width = 241
        Height = 275
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            AutoSize = True
          end
          item
            Alignment = taRightJustify
            Width = 70
          end>
        ColumnClick = False
        HideSelection = False
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        SmallImages = DataModuleImages.imlIcons
        TabOrder = 1
        ViewStyle = vsReport
        OnCustomDrawItem = lvwProjectsCustomDrawItem
        OnKeyDown = lvwProjectsKeyDown
      end
      object tbToolbarProjects: TToolBar
        Left = 0
        Top = 13
        Width = 241
        Height = 22
        EdgeBorders = [ebBottom]
        EdgeOuter = esNone
        Images = DataModuleImages.imlIcons
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Transparent = False
        Wrapable = False
        object ToolButton16: TToolButton
          Left = 0
          Top = 2
          Action = actProjectEditProjectOptions
        end
        object ToolButton18: TToolButton
          Left = 23
          Top = 2
          Width = 8
          Caption = 'ToolButton3'
          ImageIndex = 2
          Style = tbsSeparator
        end
        object ToolButton15: TToolButton
          Left = 31
          Top = 2
          Action = actProjectSetVersionInfo
        end
      end
    end
  end
  object aclButtons: TActionList
    Left = 136
    Top = 64
    object actLocalNewSetting: TAction
      Caption = '&New...'
      Hint = 'Create a new configuration from the current configuration'
      ImageIndex = 0
      OnExecute = ActionExecute
    end
    object actLocalEditSetting: TAction
      Caption = 'Edit...'
      Hint = 'Edit the selected configuration'
      ImageIndex = 2
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object actLocalDeleteSetting: TAction
      Caption = 'Delete'
      Hint = 'Delete the selected configuration'
      ImageIndex = 1
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object actLocalAssignSetting: TAction
      Caption = 'Assign'
      Hint = 'Assign the selected configuration to the selected projects'
      ImageIndex = 3
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object actLocalEditOptions: TAction
      Caption = 'Edit options...'
      Hint = 'Specify which options should be applied to the project'
      ImageIndex = 5
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object actGlobalNewSetting: TAction
      Caption = '&New...'
      Hint = 'Create a new configuration from the current configuration'
      ImageIndex = 0
      OnExecute = ActionExecute
    end
    object actGlobalEditSetting: TAction
      Caption = 'Edit...'
      Hint = 'Edit the selected configuration'
      ImageIndex = 2
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object actGlobalDeleteSetting: TAction
      Caption = 'Delete'
      Hint = 'Delete the selected configuration'
      ImageIndex = 1
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object actGlobalAssignSetting: TAction
      Caption = 'Assign'
      Hint = 'Assign the selected configuration to the selected projects'
      ImageIndex = 3
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object actGlobalEditOptions: TAction
      Caption = 'Edit options...'
      Hint = 'Specify which options should be applied to the project.'
      ImageIndex = 5
      OnExecute = ActionExecute
      OnUpdate = ActionUpdate
    end
    object actProjectEditProjectOptions: TAction
      Caption = 'Edit project options...'
      Hint = 'Edit project options'
      ImageIndex = 2
      OnExecute = ActionProjectExecute
      OnUpdate = ActionProjectUpdate
    end
    object actProjectSetVersionInfo: TAction
      Caption = 'Set versioninfo'
      Hint = 'Set version information for the selected projects'
      ImageIndex = 7
      OnExecute = ActionProjectExecute
      OnUpdate = ActionProjectUpdate
    end
  end
  object popLocalSettings: TPopupMenu
    Left = 72
    Top = 64
    object New1: TMenuItem
      Action = actLocalNewSetting
    end
    object Edit1: TMenuItem
      Action = actLocalEditSetting
    end
    object Setactiveflags1: TMenuItem
      Action = actLocalEditOptions
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Delete1: TMenuItem
      Action = actLocalDeleteSetting
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Assign1: TMenuItem
      Action = actLocalAssignSetting
    end
  end
  object popGlobalSettings: TPopupMenu
    Left = 72
    Top = 168
    object MenuItem1: TMenuItem
      Action = actGlobalNewSetting
    end
    object MenuItem2: TMenuItem
      Action = actGlobalEditSetting
    end
    object MenuItem3: TMenuItem
      Action = actGlobalEditOptions
    end
    object MenuItem4: TMenuItem
      Caption = '-'
    end
    object MenuItem5: TMenuItem
      Action = actGlobalDeleteSetting
    end
    object MenuItem6: TMenuItem
      Caption = '-'
    end
    object MenuItem7: TMenuItem
      Action = actGlobalAssignSetting
    end
  end
end
