inherited FormSwitchToModuleProject: TFormSwitchToModuleProject
  ActiveControl = ButtonNo
  BorderStyle = bsDialog
  Caption = 'Compile/Build - Switch Active Project'
  ClientHeight = 209
  ClientWidth = 438
  Color = clWindow
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  ExplicitWidth = 444
  ExplicitHeight = 235
  PixelsPerInch = 96
  TextHeight = 13
  object LabelModuleCaption: TLabel
    Left = 8
    Top = 24
    Width = 71
    Height = 13
    Caption = 'Active Module:'
  end
  object LabelFileName: TLabel
    Left = 85
    Top = 24
    Width = 84
    Height = 13
    Caption = 'LabelFileName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowAccelChar = False
    ShowHint = True
  end
  object LabelActiveProjectCaption: TLabel
    Left = 8
    Top = 8
    Width = 69
    Height = 13
    Caption = 'Active Project:'
  end
  object LabelActiveProject: TLabel
    Left = 85
    Top = 8
    Width = 108
    Height = 13
    Caption = 'LabelActiveProject'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowAccelChar = False
    ShowHint = True
  end
  object LabelQuestion: TLabel
    Left = 8
    Top = 108
    Width = 267
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Do you want to &switch to the module'#39's project?'
    FocusControl = ComboBoxProjects
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelText: TLabel
    Left = 8
    Top = 56
    Width = 411
    Height = 40
    AutoSize = False
    Caption = 
      'The module is not part of the active project and is not in a dir' +
      'ect or indirect dependent project.'
    WordWrap = True
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 174
    Width = 438
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 2
    ExplicitTop = 151
    ExplicitWidth = 425
    DesignSize = (
      438
      35)
    object BevelBottom: TBevel
      Left = 0
      Top = 0
      Width = 438
      Height = 2
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 414
    end
    object ButtonYes: TButton
      Left = 183
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'ButtonYes'
      ModalResult = 6
      TabOrder = 0
      ExplicitLeft = 170
    end
    object ButtonNo: TButton
      Left = 264
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'ButtonNo'
      Default = True
      ModalResult = 7
      TabOrder = 1
      ExplicitLeft = 251
    end
    object ButtonCancel: TButton
      Left = 357
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'ButtonCancel'
      ModalResult = 2
      TabOrder = 2
      ExplicitLeft = 344
    end
    object CheckBoxDontShowAgain: TCheckBox
      Left = 8
      Top = 10
      Width = 145
      Height = 17
      Caption = '&Don'#39't show again'
      TabOrder = 3
    end
  end
  object ComboBoxProjects: TComboBox
    Left = 8
    Top = 124
    Width = 422
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    Sorted = True
    TabOrder = 0
  end
  object CheckBoxTempSwitch: TCheckBox
    Left = 8
    Top = 151
    Width = 217
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Switch &temporary [Shift-Key]'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
end
