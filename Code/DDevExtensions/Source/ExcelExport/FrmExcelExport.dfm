object FormExcelExport: TFormExcelExport
  Left = 407
  Top = 365
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Excel Export'
  ClientHeight = 41
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnPaint = FormPaint
  TextHeight = 13
  object LblExportText: TLabel
    Left = 8
    Top = 8
    Width = 101
    Height = 13
    Caption = 'Exporting to Excel...'
    Transparent = False
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 27
    Width = 412
    Height = 17
    TabOrder = 0
  end
end
