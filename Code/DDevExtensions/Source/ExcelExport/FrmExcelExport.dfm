object FormExcelExport: TFormExcelExport
  Left = 407
  Top = 365
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Excel Export'
  ClientHeight = 59
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object LblExportText: TLabel
    Left = 8
    Top = 8
    Width = 99
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
