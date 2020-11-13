{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmExcelExport;

{$I ..\DelphiExtension.inc}

interface

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ComObj;

type
  TFormExcelExport = class(TForm)
    LblExportText: TLabel;
    ProgressBar: TProgressBar;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormPaint(Sender: TObject);
  private
    FName: string;
    FListView: TListView;
    FFilename: string;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    class procedure ExportListView(const AName: string; AListView: TListView;
      const AFilename: string = '');
  end;

procedure ExportListViewToExcel(const Name: string; ListView: TListView;
  const Filename: string = ''; ProgressBar: TProgressBar = nil);

implementation

{$R *.dfm}

procedure ExportListViewToExcel(const Name: string; ListView: TListView;
  const Filename: string = ''; ProgressBar: TProgressBar = nil);
var
  ExcelApp: Variant;
  Workbook: Variant;
  Sheet: Variant;
  i: Integer;
  Cell: string;
  k: Integer;
begin
  if Assigned(ProgressBar) then
  begin
    ProgressBar.Position := 0;
    ProgressBar.Max := 100;
  end;

  if Filename <> '' then
    ExcelApp := CreateOleObject('Excel.Application')
  else
  begin
    ExcelApp := CreateOleObject('Excel.Application');
{    try
      ExcelApp := GetActiveOleObject('Excel.Application');
    except
      ExcelApp := CreateOleObject('Excel.Application');
    end;}
  end;
  ExcelApp.ScreenUpdating := False;
  try
    Workbook := ExcelApp.Workbooks.Add;
    Sheet := Workbook.Sheets.Add;
    Sheet.Name := Name;

    with ListView do
    begin
      for i := 0 to Columns.Count - 1 do
      begin
        Cell := Char(Ord('A') + i) + '1';
        Sheet.Range[Cell + ':' + Cell].Formula := Columns[i].Caption;
        Sheet.Range[Cell + ':' + Cell].Font.Bold := True;
        Sheet.Range[Cell + ':' + Cell].ColumnWidth := Columns[i].Width / 6;
      end;
      for i := 0 to Items.Count - 1 do
      begin
        for k := 0 to Columns.Count - 1 do
        begin
          Cell := Char(Ord('A') + k) + IntToStr(1 + i + 1);
          if k = 0 then
            Sheet.Range[Cell + ':' + Cell].Formula := Items[i].Caption
          else
            Sheet.Range[Cell + ':' + Cell].Formula := Items[i].SubItems[k - 1];
        end;
        if Assigned(ProgressBar) and (i mod 25 = 0) then
        begin
          ProgressBar.Position := i * 100 div Items.Count;
          Application.ProcessMessages;
        end;
      end;
    end;
    if Assigned(ProgressBar) then
      ProgressBar.Position := ProgressBar.Max;
  finally
    ExcelApp.ScreenUpdating := True;
    if Filename <> '' then
    begin
      Workbook.SaveAs(Filename);
      ExcelApp.Quit;
    end
    else
      ExcelApp.Visible := True;

    Sheet := Unassigned;
    Workbook := Unassigned;
    ExcelApp := Unassigned;
  end;
end;


class procedure TFormExcelExport.ExportListView(const AName: string;
  AListView: TListView; const AFilename: string);
begin
  with TFormExcelExport.Create(Application) do
  try
    FName := AName;
    FListView := AListView;
    FFilename := AFilename;
    ProgressBar.Position := 0;
    ProgressBar.Max := 100;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormExcelExport.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := ModalResult <> mrCancel;
end;

procedure TFormExcelExport.FormPaint(Sender: TObject);
begin
  OnPaint := nil;
  Repaint;
  try
    ExportListViewToExcel(FName, FListView, FFilename, ProgressBar);
  finally
    ModalResult := mrOk;
  end;
end;

end.
