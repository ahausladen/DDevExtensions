object FormMain: TFormMain
  Left = 362
  Top = 178
  ActiveControl = btnInstall
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Install'
  ClientHeight = 190
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 121
    Height = 13
    Caption = 'Install to / Uninstall from:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnInstall: TButton
    Left = 8
    Top = 157
    Width = 75
    Height = 25
    Caption = '&Install'
    Default = True
    TabOrder = 0
    OnClick = btnInstallClick
  end
  object btnQuit: TButton
    Left = 294
    Top = 157
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Quit'
    TabOrder = 2
    OnClick = btnQuitClick
  end
  object btnUninstall: TButton
    Left = 89
    Top = 157
    Width = 75
    Height = 25
    Caption = '&Uninstall'
    TabOrder = 1
    OnClick = btnUninstallClick
  end
  object cbxEnvs: TCheckListBox
    Left = 8
    Top = 24
    Width = 361
    Height = 110
    ItemHeight = 13
    TabOrder = 3
  end
  object pbProgress: TProgressBar
    Left = 8
    Top = 140
    Width = 361
    Height = 11
    TabOrder = 4
    Visible = False
  end
end
