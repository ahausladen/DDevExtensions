{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit NativeProgressForm;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, TaskbarIntf;

type
  TNativeProgressForm = class(TComponent)
  private
    FMaxFiles: Integer;
    FProjectFilesCompiled: Integer;
    FCachedAutoClose: Boolean;
    FLastPercentage: Integer;
    FProgressBar: TProgressBar;
    FTaskbarList: ITaskbarList;
    FTaskbarList3: ITaskbarList3;
    function GetCurrFile: string;
    function GetCurrLines: LongWord;
    function GetHintCount: Integer;
    function GetMainFile: string;
    function GetStatus: string;
    function GetTotalLines: LongWord;
    function GetErrorCount: Integer;
    function GetWarningCount: Integer;
    procedure SetCurrFile(const Value: string);
    procedure SetCurrLines(const Value: LongWord);
    procedure SetErrorCount(const Value: Integer);
    procedure SetHintCount(const Value: Integer);
    procedure SetMainFile(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetTotalLines(const Value: LongWord);
    procedure SetWarningCount(const Value: Integer);
    procedure SetStatusOverwrite(const Value: string);
    function GetFilesCompiled: Integer;
    procedure SetFilesCompiled(const Value: Integer);
    procedure SetMaxFiles(const Value: Integer);
    procedure SetProjectFilesCompiled(const Value: Integer);
  protected
    function GetTaskbarFormHandle: HWND;
    procedure UpdateTaskbarProgress;

    function GetForm: TCustomForm;
    function GetLabel(const Name: string): TLabel;
    function GetCheckBox(const Name: string): TCheckBox;
    procedure SetString(const Name, Value: string);
    procedure SetInteger(const Name: string; Value: Integer);
    procedure SetLongWord(const Name: string; Value: LongWord);

    function GetString(const Name: string): string;
    function GetInteger(const Name: string): Integer;
    function GetLongWord(const Name: string): LongWord;

    procedure DoAutoCloseClick(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure UpdateForm;
    procedure Cancel;
    procedure ShowProgressBar(AShow: Boolean);

    property Form: TCustomForm read GetForm;
    property StatusOverwrite: string write SetStatusOverwrite;
    property FilesCompiled: Integer read GetFilesCompiled write SetFilesCompiled;
    property ProjectFilesCompiled: Integer read FProjectFilesCompiled write SetProjectFilesCompiled;
    property MaxFiles: Integer read FMaxFiles write SetMaxFiles;

    property Status: string read GetStatus write SetStatus;
    property MainFile: string read GetMainFile write SetMainFile;
    property CurrFile: string read GetCurrFile write SetCurrFile;
    property CurrLines: LongWord read GetCurrLines write SetCurrLines;
    property TotalLines: LongWord read GetTotalLines write SetTotalLines;
    property HintCount: Integer read GetHintCount write SetHintCount;
    property WarningCount: Integer read GetWarningCount write SetWarningCount;
    property ErrorCount: Integer read GetErrorCount write SetErrorCount;
  end;

var
  FormNativeProgress: TNativeProgressForm;

implementation

uses
  Themes, AppConsts, Hooking, IDEHooks;

const
  {$IF CompilerVersion >= 21.0} // Delphi 2010+
  sCurrFileLabelName = 'FileName';
  {$ELSE}
  sCurrFileLabelName = 'CurrFile';
  {$IFEND}

procedure ProgressFormPtr;
  external coreide_bpl name '@Comprgrs@ProgressForm';

var
  ProgressFormP: ^TForm;

{ TNativeProgressForm }

constructor TNativeProgressForm.Create;
begin
  inherited Create(nil);
  FProjectFilesCompiled := -1;

  if CheckWin32Version(6, 1) then
  begin
    try
      FTaskbarList := CreateTaskbarList;
      if not Supports(FTaskbarList, IID_ITaskbarList3, FTaskbarList3) then
        FTaskbarList3 := nil;
    except
      FTaskbarList := nil;
      FTaskbarList3 := nil;
    end;
  end;
end;

destructor TNativeProgressForm.Destroy;
begin
  StatusOverwrite := '';
  FilesCompiled := 0;
  MaxFiles := 0;
  FreeAndNil(FProgressBar);
  UpdateTaskbarProgress;
  FTaskbarList3 := nil;
  FTaskbarList := nil;
  inherited Destroy;
end;

function TNativeProgressForm.GetForm: TCustomForm;
begin
  if ProgressFormP = nil then
    ProgressFormP := GetActualAddr(@ProgressFormPtr);
  Result := ProgressFormP^;
end;

procedure TNativeProgressForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FProgressBar then
    begin
      FProgressBar := nil;
      UpdateTaskbarProgress;
    end;
  end;
end;

procedure TNativeProgressForm.Cancel;
var
  Form: TCustomForm;
  Btn: TButton;
begin
  Form := GetForm;
  if Form <> nil then
  begin
    Btn := TButton(Form.FindComponent('CancelButton'));
    if TComponent(Btn) is TButton then
      Btn.Click;
  end;
end;

function TNativeProgressForm.GetLabel(const Name: string): TLabel;
var
  Form: TCustomForm;
begin
  Result := nil;
  Form := GetForm;
  if Form <> nil then
  begin
    Result := TLabel(Form.FindComponent(Name));
    if not (TComponent(Result) is TLabel) then
      Result := nil;
  end;
end;

function TNativeProgressForm.GetCheckBox(const Name: string): TCheckBox;
var
  Form: TCustomForm;
begin
  Result := nil;
  Form := GetForm;
  if Form <> nil then
  begin
    Result := TCheckBox(Form.FindComponent(Name));
    if not (TComponent(Result) is TCheckBox) then
      Result := nil;
  end;
end;

procedure TNativeProgressForm.SetString(const Name, Value: string);
var
  Lbl: TLabel;
begin
  Lbl := GetLabel(Name);
  if Assigned(Lbl) then
    Lbl.Caption := Value;
end;

procedure TNativeProgressForm.SetInteger(const Name: string; Value: Integer);
begin
  SetString(Name, IntToStr(Value));
end;

procedure TNativeProgressForm.SetLongWord(const Name: string; Value: LongWord);
begin
  SetString(Name, IntToStr(Value));
end;

function TNativeProgressForm.GetString(const Name: string): string;
var
  Lbl: TLabel;
begin
  Lbl := GetLabel(Name);
  if Assigned(Lbl) then
    Result := Lbl.Caption
  else
    Result := '';
end;

function TNativeProgressForm.GetInteger(const Name: string): Integer;
var
  Code: Integer;
begin
  Val(GetString(Name), Result, Code);
  if Code <> 0 then
    Result := 0;
end;

function TNativeProgressForm.GetLongWord(const Name: string): LongWord;
var
  Code: Integer;
begin
  Val(GetString(Name), Result, Code);
  if Code <> 0 then
    Result := 0;
end;

function TNativeProgressForm.GetCurrFile: string;
begin
  Result := GetString(sCurrFileLabelName);
end;

function TNativeProgressForm.GetCurrLines: LongWord;
begin
  Result := GetLongWord('CurrLines');
end;

function TNativeProgressForm.GetErrorCount: Integer;
begin
  Result := GetInteger('ErrorCount');
end;

function TNativeProgressForm.GetHintCount: Integer;
begin
  Result := GetInteger('HintCount');
end;

function TNativeProgressForm.GetMainFile: string;
begin
  Result := GetString('MainFile');
end;

function TNativeProgressForm.GetStatus: string;
begin
  Result := GetString('Status');
end;

function TNativeProgressForm.GetTotalLines: LongWord;
begin
  Result := GetLongWord('TotalLines');
end;

function TNativeProgressForm.GetWarningCount: Integer;
begin
  Result := GetInteger('WarningCount');
end;

procedure TNativeProgressForm.SetCurrFile(const Value: string);
begin
  SetString(sCurrFileLabelName, Value);
end;

procedure TNativeProgressForm.SetCurrLines(const Value: LongWord);
begin
  SetLongWord('CurrLines', Value);
end;

procedure TNativeProgressForm.SetErrorCount(const Value: Integer);
begin
  SetInteger('ErrorCount', Value);
end;

procedure TNativeProgressForm.SetHintCount(const Value: Integer);
begin
  SetInteger('HintCount', Value);
end;

procedure TNativeProgressForm.SetMainFile(const Value: string);
begin
  SetString('MainFile', Value);
end;

procedure TNativeProgressForm.SetStatus(const Value: string);
begin
  SetString('Status', Value);
end;

procedure TNativeProgressForm.SetTotalLines(const Value: LongWord);
begin
  SetLongWord('TotalLines', Value);
end;

procedure TNativeProgressForm.SetWarningCount(const Value: Integer);
begin
  SetInteger('WarningCount', Value);
end;

procedure TNativeProgressForm.ShowProgressBar(AShow: Boolean);
begin
  if FProgressBar <> nil then
  begin
    if AShow <> FProgressBar.Visible then
      FProgressBar.Visible := AShow;
    UpdateTaskbarProgress;
  end;
end;

procedure TNativeProgressForm.UpdateForm;
begin
  Application.ProcessMessages;
end;

procedure TNativeProgressForm.SetStatusOverwrite(const Value: string);
var
  Form: TCustomForm;
  Panel: TPanel;
  LblCurrFile, LblStatus: TLabel;
begin
  Form := GetForm;
  if Form <> nil then
  begin
    Panel := Form.FindComponent('bcc32pch_StatusOverwrite') as TPanel;
    if (Value = '') then
    begin
      if Assigned(Panel) then
        Panel.Free;
    end
    else
    begin
      if not Assigned(Panel) then
      begin
        LblCurrFile := GetLabel(sCurrFileLabelName);
        LblStatus := GetLabel('Status');
        if not Assigned(LblCurrFile) or not Assigned(LblStatus) then
          Exit;
        Panel := TPanel.Create(Form);
        Panel.Name := 'bcc32pch_StatusOverwrite';
        Panel.BevelInner := bvNone;
        Panel.BevelOuter := bvNOne;

        Panel.BoundsRect := LblCurrFile.BoundsRect;
        Panel.Alignment := taLeftJustify;

        //Panel.BoundsRect := Rect(LblStatus.Left, LblStatus.Top, LblCurrFile.BoundsRect.Right, LblCurrFile.BoundsRect.Bottom);

        Panel.Font.Style := [fsBold];
        Panel.Caption := Value;
        Panel.Parent := Form;
      end
      else
        Panel.Caption := Value;
    end;
  end;
end;

function TNativeProgressForm.GetFilesCompiled: Integer;
var
  Lbl: TLabel;
begin
  Lbl := GetLabel('bcc32pch_FilesCompiled');
  if Assigned(Lbl) then
    Result := Lbl.Tag
  else
    Result := 0;
end;

procedure TNativeProgressForm.SetFilesCompiled(const Value: Integer);
var
  Lbl: TLabel;
  Form: TCustomForm;
begin
  Form := GetForm;
  if Form = nil then
    Exit;

  Lbl := GetLabel('bcc32pch_FilesCompiled');
  if Value <= 0 then
  begin
    if Assigned(Lbl) then
      Lbl.Free;
    Exit;
  end;

  if not Assigned(Lbl) then
  begin
    Lbl := TLabel.Create(Form);
    Lbl.Name := 'bcc32pch_FilesCompiled';
    Lbl.Alignment := taRightJustify;
    Lbl.Caption := '';
    Lbl.Left := Form.ClientWidth - 8 - Lbl.Width;
    SetMaxFiles(FMaxFiles);
    Lbl.Top := FProgressBar.BoundsRect.Bottom + 4;
    Lbl.Parent := Form;
  end;

  if Assigned(Lbl) then
  begin
    Lbl.Tag := Value;
    Lbl.Caption := Format(sFilesCompiled, [Value]);
    SetMaxFiles(FMaxFiles); // update progress bar
  end;
end;

procedure TNativeProgressForm.SetProjectFilesCompiled(const Value: Integer);
begin
  FProjectFilesCompiled := Value;
  SetMaxFiles(FMaxFiles); // update progress bar
end;

procedure TNativeProgressForm.SetMaxFiles(const Value: Integer);
var
  Form: TCustomForm;
  NewPercentage: Integer;
  {$IF CompilerVersion >= 33.0}
  //I: Integer;
  pnErrors: TControl;
  TotalLines: TLabel;
  X, Y: Integer;
  {$IFEND}
begin
  FMaxFiles := Value;
  Form := GetForm;
  if Form = nil then
    Exit;

  if Value <= 0 then
  begin
    FreeAndNil(FProgressBar);
    Exit;
  end;

  if FMaxFiles = 0 then
    NewPercentage := 0
  else
  begin
    if FProjectFilesCompiled <> -1 then
      NewPercentage := FProjectFilesCompiled * 100 div FMaxFiles
    else
      NewPercentage := FilesCompiled * 100 div FMaxFiles;
  end;

  if FProgressBar = nil then
  begin
    FLastPercentage := NewPercentage;
    FProgressBar := TProgressBar.Create(Form);
    FProgressBar.FreeNotification(Self);
    FProgressBar.Name := 'DDevExtensions_ProgressBar';
    FProgressBar.Max := 100;
    FProgressBar.Position := NewPercentage;
    {$IF CompilerVersion >= 33.0}
    // New Progress-Dialog

//    AllocConsole;
//    for I := 0 to Form.ComponentCount - 1 do
//      WriteLn(Form.Components[I].Name + ': ' + Form.Components[I].ClassName);

    pnErrors := Form.FindComponent('pnErrors') as TControl;
    // In Delphi 11.1 the ProgressBar must be placed below the "Hints" panel:
    {$IF declared(RTLVersion111)}{$IF RTLVersion111}
    pnErrors := Form.FindComponent('pnHints') as TControl;
    {$IFEND}{$IFEND}
    TotalLines := GetLabel('TotalLines');
    if (pnErrors is TPanel) and (TotalLines <> nil) then
    begin
      X := Form.ScreenToClient(pnErrors.ClientToScreen(Point(0, 0))).X;
      Y := Form.ScreenToClient(TotalLines.ClientToScreen(Point(TotalLines.Top, 0))).Y;
      FProgressBar.ScaleForPPI(Form.CurrentPPI);
      FProgressBar.SetBounds(X, Y + 2, pnErrors.Width, TotalLines.Height div 2);
    end
    else // Fallback
      FProgressBar.SetBounds(384, 187 + 2, 162, 7);
    {$ELSE}
    FProgressBar.Width := {$IFDEF IDE50_UP}120{$ELSE}80{$ENDIF};
    FProgressBar.Height := 7;
    FProgressBar.Left := Form.ClientWidth - FProgressBar.Width - 8;
    FProgressBar.Top := Form.ClientHeight - 4 - 7 - 25 {$IFDEF COMPILER10_UP}- 20{$ENDIF};
    {$IFEND}
    {$IF CompilerVersion >= 23.0}
    if StyleServices.Available and StyleServices.Enabled then
    {$ELSE}
    if ThemeServices.ThemesAvailable and ThemeServices.ThemesEnabled then
    {$IFEND}
      FProgressBar.Height := 12;
    FProgressBar.Parent := Form;
    UpdateTaskbarProgress;
  end
  else
  begin
    if NewPercentage <> FLastPercentage then
    begin
      FLastPercentage := NewPercentage;
      if FProgressBar <> nil then
        FProgressBar.Position := NewPercentage;
      //Progress.Max := 100;
      UpdateTaskbarProgress;
    end;
  end;
end;

function TNativeProgressForm.GetTaskbarFormHandle: HWND;
begin
  if Application.MainFormOnTaskBar then
    Result := Application.MainFormHandle
  else
    Result := Application.Handle;
end;

procedure TNativeProgressForm.UpdateTaskbarProgress;
var
  State: Integer;
  TaskbarFormHandle: HWND;
begin
  if CheckWin32Version(6, 1) and (FTaskbarList3 <> nil) then
  begin
    TaskbarFormHandle := GetTaskbarFormHandle;
    if TaskbarFormHandle <> 0 then
    begin
      if FProgressBar <> nil then
      begin
        FTaskbarList3.SetProgressValue(TaskbarFormHandle, FProgressBar.Position, FProgressBar.Max);
        State := TBPF_NORMAL;
        if ErrorCount > 0 then
          State := TBPF_ERROR
        else if WarningCount > 0 then
          State := TBPF_PAUSED
        else if FProgressBar.Position = 0 then
          State := TBPF_NOPROGRESS;
      end
      else
        State := TBPF_NOPROGRESS;
      FTaskbarList3.SetProgressState(TaskbarFormHandle, State);
    end;
  end;
end;

procedure TNativeProgressForm.DoAutoCloseClick(Sender: TObject);
begin
  FCachedAutoClose := TCheckBox(Sender).Checked;
end;


end.
