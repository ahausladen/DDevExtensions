inherited FormOptions: TFormOptions
  Left = 243
  Top = 179
  Caption = 'Application - Options'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelButtons: TPanel
    object lblURL: TLabel [1]
      Left = 6
      Top = 21
      Width = 166
      Height = 13
      Cursor = crHandPoint
      Hint = 'mailto:Andreas.Hausladen@gmx.de?subject='
      Caption = '(C) 2005-2009 Andreas Hausladen'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = lblURLClick
    end
    object Label1: TLabel [2]
      Left = 6
      Top = 4
      Width = 51
      Height = 13
      Cursor = crHandPoint
      Hint = 'https://www.idefixpack.de/blog'
      Caption = 'Homepage'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnClick = lblURLClick
    end
    object lblVersion: TLabel [3]
      Left = 182
      Top = 21
      Width = 54
      Height = 13
      Caption = 'Version x.y'
    end
  end
end
