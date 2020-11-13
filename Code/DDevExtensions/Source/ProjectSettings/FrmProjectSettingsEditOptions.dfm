inherited FormProjectSettingsEditOptions: TFormProjectSettingsEditOptions
  Left = 391
  Top = 151
  Width = 545
  Height = 664
  BorderIcons = [biSystemMenu]
  Caption = 'Options'
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblCaption: TLabel
    Left = 8
    Top = 5
    Width = 465
    Height = 17
    AutoSize = False
    Caption = 
      'Here you can specify which options should be applied if you assi' +
      'gn the project settings.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Style = []
    ParentFont = False
  end
  object btnOk: TButton
    Left = 369
    Top = 582
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 450
    Top = 582
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object clbOptions: TCheckListBox
    Left = 8
    Top = 24
    Width = 516
    Height = 553
    Columns = 3
    ItemHeight = 13
    TabOrder = 2
  end
  object btnCheckAll: TButton
    Left = 8
    Top = 582
    Width = 73
    Height = 25
    Caption = 'Check all'
    TabOrder = 3
    OnClick = btnCheckAllClick
  end
  object btnUncheckAll: TButton
    Left = 88
    Top = 582
    Width = 73
    Height = 25
    Caption = 'Uncheck all'
    TabOrder = 4
    OnClick = btnCheckAllClick
  end
  object btnDefault: TButton
    Left = 256
    Top = 582
    Width = 75
    Height = 25
    Caption = 'Default'
    TabOrder = 6
    OnClick = btnDefaultClick
  end
  object btnToggle: TButton
    Left = 168
    Top = 582
    Width = 73
    Height = 25
    Caption = 'Toggle'
    TabOrder = 5
    OnClick = btnToggleClick
  end
end
