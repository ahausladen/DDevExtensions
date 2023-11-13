{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2008 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit FrmFileSelector;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, ExtCtrls, ActnList, ImgList, StdCtrls, ToolWin,
  CtrlUtils, CommCtrl, ToolsAPI, IDEUtils, SimpleXmlImport, SimpleXmlIntf,
  ToolsAPIHelpers, FrmBase, DelphiDesignerParser;

type
  { Fix Clear() in OwnerData Mode. We don't need to get every item (OnData) if we delete them all. }
  TListView = class(ComCtrls.TListView)
  protected
    procedure CNNotify(var Message: TWMNotifyLV); message CN_NOTIFY;
  end;

  TFilterListViewDataEvent = function(Sender: TObject; const S: string): Boolean of object;

  TInfo = class(TObject)
  public
    ImageIndex: Integer;
    Opened: Boolean;
    Name: string;
    FileName: string;
    FormName: string;
    FormCaption: string;
    RelFileName: string;
    FormTypeDetected: Boolean;
  end;

  TInfoList = class(TObjectList)
  private
    function GetItem(Index: Integer): TInfo;
  public
    property Items[Index: Integer]: TInfo read GetItem; default;
  end;

  TFormFileSelector = class;

  TResourceTypeThread = class(TThread)
  private
    FForm: TFormFileSelector;
  protected
    procedure UpdateImageIndex;
    procedure Execute; override;
  public
    constructor Create(AForm: TFormFileSelector);
  end;

  TFormFileSelector = class(TFormBase)
    ListView: TListView;
    ActionList: TActionList;
    ActionExportToExcel: TAction;
    TimerFilterUpdate: TTimer;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    tbExportToExcel: TToolButton;
    tbExportSeparator: TToolButton;
    edtFilter: TEdit;
    ToolButton5: TToolButton;
    cbxFilterField: TComboBox;
    PanelButtons: TPanel;
    btnUseUnit: TButton;
    btnCancel: TButton;
    tsepDir: TToolButton;
    cbxFilterDirectory: TComboBox;
    chkUseUnitsImplementation: TCheckBox;
    btnOpen: TButton;
    popListView: TPopupMenu;
    mniOpenFile: TMenuItem;
    mniAddtoInsertList: TMenuItem;
    ActionAddToInsertList: TAction;
    popListViewInsertUnits: TPopupMenu;
    ActionRemoveFromInsertList: TAction;
    mniRemovefromInsertList: TMenuItem;
    PanelBottom: TPanel;
    ListViewInsertUnits: TListView;
    Panel1: TPanel;
    Label1: TLabel;
    ActionClearInsertList: TAction;
    ClearInsertList1: TMenuItem;
    btnOptions: TButton;
    popOptions: TPopupMenu;
    mniAllowMoveFromInterfaceToImpl: TMenuItem;
    mniEveryUnitOnSingleLine: TMenuItem;
    procedure cbxFilterFieldDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbxFilterFieldChange(Sender: TObject);
    procedure edtFilterKeyPress(Sender: TObject; var Key: Char);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtFilterChange(Sender: TObject);
    procedure TimerFilterUpdateTimer(Sender: TObject);
    procedure ActionExportToExcelExecute(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure FormCreate(Sender: TObject);
    procedure edtFilterEnter(Sender: TObject);
    procedure edtFilterExit(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbxFilterDirectoryDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbxFilterDirectoryChange(Sender: TObject);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListViewInfoTip(Sender: TObject; Item: TListItem; var InfoTip: string);
    procedure ListViewData(Sender: TObject; Item: TListItem);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormResize(Sender: TObject);
    procedure mniOpenFileClick(Sender: TObject);
    procedure ListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtFilterKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewInsertUnitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ActionRemoveFromInsertListUpdate(Sender: TObject);
    procedure ActionRemoveFromInsertListExecute(Sender: TObject);
    procedure ActionAddToInsertListUpdate(Sender: TObject);
    procedure ActionAddToInsertListExecute(Sender: TObject);
    procedure ActionClearInsertListUpdate(Sender: TObject);
    procedure ActionClearInsertListExecute(Sender: TObject);
    procedure ListViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure ListViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListViewInsertUnitsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure ListViewInsertUnitsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure btnOptionsClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FFilterField: Integer;
    FProjectFilename: string;
    FFilterTexts: TStrings;
    FDirectoryFiltered: Boolean;
    FDirectoryFilter: string;

    FSortColumn: Integer;
    FSortAsc: Boolean;

    FAllData: TInfoList;
    FCurrentData: TInfoList;
    FResourceTypeThread: TResourceTypeThread;
    FEditor: IOTASourceEditor;
    FParser: TDesignerParser;

    procedure FilerEditWndProc(var Msg: TMessage);
    procedure RemoveUsesUnits(Writer: IOTAEditWriter; StopIndex: Integer; DeleteUsesList: TList;
      UsesList: TUsesList);
  protected
    function FilterInfo(Info: TInfo; var NewSelLen: Integer): Boolean;
    function FilterListViewData(S: string): Boolean;

    procedure GetFilenames;
    procedure UpdateListViewData;
    procedure Sort;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure UpdateData;
    procedure UseSelectedUnits(Files: TList);
    function GetCurrentSourceEditor: IOTASourceEditor;
    function IsBinaryFile(const Filename: string): Boolean;

    procedure RefreshButtons;
    function InternExecute(OpenMode: Boolean): Boolean;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Execute(OpenMode: Boolean): Boolean;
  end;

function ContainsAsterix(const S: string): Boolean;
function MatchStr(const S: string; const MatchString: string): Boolean;
procedure SplitFilterText(const Text: string; List: TStrings);

implementation

uses
  AppConsts, FrmExcelExport, Main, DtmImages, PluginConfig, Variants;

{$R *.dfm}

var
  FormFileSelector: TFormFileSelector; // needed for the Sort() callback
  CachedCurDir: string;
  CachedDirs: string;
  CachedFiles: TStrings;
  CachedSources: TStrings;

type
  TOpenEdit = class(TEdit);

{ TListView }

procedure TListView.CNNotify(var Message: TWMNotifyLV);
begin
  case Message.NMHdr.code of
    LVN_DELETEALLITEMS:
      begin
        if OwnerData then
        begin
          Message.Result := 1;
          Exit;
        end;
      end;
  end;
  inherited;
end;

{ TInfoList }

function TInfoList.GetItem(Index: Integer): TInfo;
begin
  Result := TInfo(inherited Items[Index]);
end;

procedure SplitFilterText(const Text: string; List: TStrings);
var
  F, P: PChar;
  S: string;
begin
  P := PChar(Text);
  while P[0] <> #0 do
  begin
    while (P[0] <> #0) and (P[0] = ' ') do
      Inc(P);
    F := P;
    while (P[0] <> #0) and (P[0] <> ' ') do
      Inc(P);
    SetString(S, F, P - F);
    List.Add(S);
  end;
end;

function ContainsAsterix(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if (S[I] = '*') or (S[I] = '?') then
      Exit(True);
  Result := False;
end;

{ MatchStr - Patter-Search
  Syntax:
  a?b   
  a??b  
  a*b   
}
function MatchStr(const S: string; const MatchString: string): Boolean;
var
  SIndex, SLen, MatchIndex, MatchLen: Integer;
  C: Char;
  LastMatchIndex, LastSIndex: Integer;
begin
  Result := False;

  SLen := Length(S);
  MatchLen := Length(MatchString);

  if (MatchLen = 0) and (SLen <> 0) then
    Exit;

  if SLen = 0 then
  begin
    if MatchLen > 0 then
    begin
      for MatchIndex := 1 to MatchLen do
        if MatchString[MatchIndex] <> '*' then
          Exit;
    end
    else
    begin
      Result := True;
      Exit;
    end;
  end;
  SIndex := 1;
  MatchIndex := 1;
  LastMatchIndex := 0;
  LastSIndex := 0;

  Result := True;
  while (SIndex <= SLen) do
  begin
    if (MatchIndex > MatchLen) then
    begin
      Result := False;
      Break;
    end;
    C := MatchString[MatchIndex];
    case C of
      '?':
        Inc(MatchIndex);
      '*':
        begin
          LastMatchIndex := MatchIndex;
          Inc(MatchIndex);
          if MatchIndex > MatchLen then
            Break;
          C := MatchString[MatchIndex];
          Inc(MatchIndex);
          while (SIndex <= SLen) and (S[SIndex] <> C) do
            Inc(SIndex);
          if SIndex > SLen then
          begin
            Result := False;
            Break;
          end
          else
          if SIndex = SLen then
          begin
            if MatchIndex < MatchLen then
            begin
              while (MatchIndex <= MatchLen) and (MatchString[MatchIndex] = '*') do
                Inc(MatchIndex);
              if MatchIndex <= MatchLen then
                Result := False;
              Break;
            end;
          end;
          LastSIndex := SIndex;
        end; // '*'
    else
      if S[SIndex] <> C then
      begin
        if (LastMatchIndex > 0) then
        begin
          MatchIndex := LastMatchIndex;
          SIndex := LastSIndex + 1;
          Continue;
        end;
        Result := False;
        Break;
      end
      else
        Inc(MatchIndex);
    end; // case
    Inc(SIndex);
  end; // while

  if (SIndex > SLen) then
  begin
    while (MatchIndex <= MatchLen) do
    begin
      if MatchString[MatchIndex] <> '*' then
      begin
        Result := False;
        Break;
      end;
      Inc(MatchIndex);
    end;
  end;
end;

{ TResourceTypeThread }

constructor TResourceTypeThread.Create(AForm: TFormFileSelector);
begin
  FForm := AForm;
  inherited Create(False);
end;

procedure TResourceTypeThread.Execute;
var
  Index: Integer;
  Info: TInfo;
begin
  Index := 0;
  while not Terminated and (Index < FForm.FAllData.Count) do
  begin
    Info := FForm.FAllData[Index];
    if not Info.FormTypeDetected and (Info.ImageIndex = imgModuleForm) then
    begin
      Info.FormTypeDetected := True;
      case GetFormResourceType(Info.FileName, Info.FormName, Info.FormCaption) of
        ftFrame:
          Info.ImageIndex := imgModuleFrame;
        ftDataModule:
          Info.ImageIndex := imgModuleDataModule;
      end;
    end;
    Inc(Index);
  end;
  if not Terminated then
    Synchronize(UpdateImageIndex);
  Terminate;
end;

procedure TResourceTypeThread.UpdateImageIndex;
begin
  FForm.ListView.Invalidate;
end;

{ TFormFileSelector }

constructor TFormFileSelector.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);
  FFilterTexts := TStringList.Create;
  ToolBar.AutoSize := True;
  edtFilter.WindowProc := FilerEditWndProc;

  cbxFilterField.Items.Clear;
  cbxFilterField.Items.Add(RsFilterAllFields);
  for i := 0 to ListView.Columns.Count - 1 do
    if ListView.Columns[i].Tag = 0 then
      cbxFilterField.Items.Add(ListView.Columns[i].Caption);
  cbxFilterField.ItemIndex := imgModuleUnit;
end;

destructor TFormFileSelector.Destroy;
begin
  FFilterTexts.Free;
  inherited Destroy;
end;

function TFormFileSelector.InternExecute(OpenMode: Boolean): Boolean;
var
  I: Integer;
  ActionServices: IOTAActionServices;
  Module: IOTAModule;
  Files: TList;
  FileName: string;
begin
  Caption := sMenuItemDDevExtensionsFileSelector;

  if GetActiveProject <> nil then
    StatusBar.Panels[1].Text := ' ' + ExtractFileName(GetActiveProject.FileName);

  { Parse current module source code }
  FEditor := GetCurrentSourceEditor;
  if FEditor <> nil then
  begin
    FParser := TDesignerParser.Create(FEditor.FileName, GetEditorSource(FEditor));
    try
      FParser.ParseUsesOnly;
      if FParser.ModuleType <> mtUnit then
        Abort;
    except
      on E: Exception do
      begin
        if not (E is EAbort) then
          Application.HandleException(Self);
        FreeAndNil(FParser);
        FEditor := nil;
      end;
    end;
  end;

  btnUseUnit.Visible := (FEditor <> nil) and not OpenMode;
  chkUseUnitsImplementation.Visible := btnUseUnit.Visible;
  btnOptions.Visible := btnUseUnit.Visible;
  if not btnUseUnit.Visible then
  begin
    btnOpen.Left := btnUseUnit.Left + btnUseUnit.Width - btnOpen.Width;
    btnOpen.Default := True;
  end;

  GetFilenames;
  LoadSettings;
  //Sort;
  UpdateData;

  FResourceTypeThread := TResourceTypeThread.Create(Self);
  try
    Result := (ShowModal in [mrOk, mrYes]) and
              ((ListView.Selected <> nil) or (ListViewInsertUnits.Items.Count > 0));
  finally
    if Assigned(FResourceTypeThread) then
    begin
      FResourceTypeThread.Terminate;
      FResourceTypeThread.WaitFor;
      FreeAndNil(FResourceTypeThread);
    end;
  end;

  if Result then
  begin
    Files := TList.Create;
    try
      if ListViewInsertUnits.Items.Count <> 0 then
      begin
        for I := 0 to ListViewInsertUnits.Items.Count - 1 do
          Files.Add(ListViewInsertUnits.Items[I].Data);
      end
      else
      begin
        for I := 0 to ListView.Items.Count - 1 do
          if ListView.Items[I].Selected then
            Files.Add(ListView.Items[I].Data);
      end;

      if (FEditor <> nil) and (ModalResult = mrYes) then
        UseSelectedUnits(Files)
      else
      { Open all selected files }
      if Supports(BorlandIDEServices, IOTAActionServices, ActionServices) then
      begin
        for I := 0 to Files.Count - 1 do
        begin
          FileName := TInfo(Files[I]).FileName;
          if (FileName <> '') and not IsBinaryFile(FileName) then
          begin
            Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
            if Module <> nil then
              Module.Show
            else
              ActionServices.OpenFile(FileName);
          end;
        end;
      end;
    finally
      Files.Free;
    end;
  end;
  SaveSettings;
end;

function TFormFileSelector.IsBinaryFile(const Filename: string): Boolean;
begin
  Result := InArray(ExtractFileExt(FileName), ['.dcu', '.dcuil']);
end;

procedure TFormFileSelector.ActionAddToInsertListExecute(Sender: TObject);
var
  I: Integer;
  ListItem: TListItem;
begin
  ListViewInsertUnits.Items.BeginUpdate;
  try
    for I := 0 to ListView.Items.Count - 1 do
    begin
      if ListView.Items[I].Selected then
      begin
        if ListViewInsertUnits.FindData(0, ListView.Items[I].Data, True, False) = nil then
        begin
          ListItem := ListViewInsertUnits.Items.Add;
          ListItem.Assign(ListView.Items[I]);
        end;
      end;
    end;
  finally
    ListViewInsertUnits.Items.EndUpdate;
  end;
  RefreshButtons;
end;

procedure TFormFileSelector.ActionAddToInsertListUpdate(Sender: TObject);
begin
  ActionAddToInsertList.Enabled := (ListView.SelCount > 1) or
    (ListView.Selected <> nil) and (ListViewInsertUnits.FindData(0, ListView.Selected.Data, True, False) = nil);
end;

procedure TFormFileSelector.ActionClearInsertListExecute(Sender: TObject);
begin
  ListViewInsertUnits.Items.Clear;
  RefreshButtons;
end;

procedure TFormFileSelector.ActionClearInsertListUpdate(Sender: TObject);
begin
  ActionClearInsertList.Enabled := ListViewInsertUnits.Items.Count > 0;
end;

procedure TFormFileSelector.ActionExportToExcelExecute(Sender: TObject);
begin
  TFormExcelExport.ExportListView('Units', ListView);
end;

procedure TFormFileSelector.ActionRemoveFromInsertListExecute(Sender: TObject);
begin
  inherited;
  ListViewInsertUnits.DeleteSelected;
  RefreshButtons;
end;

procedure TFormFileSelector.ActionRemoveFromInsertListUpdate(Sender: TObject);
begin
  ActionRemoveFromInsertList.Enabled := ListViewInsertUnits.Selected <> nil;
end;

procedure TFormFileSelector.cbxFilterDirectoryChange(Sender: TObject);
begin
  UpdateData;
end;

procedure TFormFileSelector.cbxFilterDirectoryDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  S: string;
begin
  with TComboBox(Control).Canvas do
  begin
    FillRect(Rect);
    DataModuleImages.imlFilter.Draw(TComboBox(Control).Canvas,
      Rect.Left + 2, Rect.Top, 1);
    Inc(Rect.Left, DataModuleImages.imlFilter.Width + 2);
    S := TComboBox(Control).Items[Index];
    if S = '' then
      S := '.';
    TextRect(Rect, Rect.Left + 4, Rect.Top + 1, S);
  end;
end;

procedure TFormFileSelector.cbxFilterFieldChange(Sender: TObject);
begin
  if edtFilter.Text <> '' then
    UpdateData;
end;

procedure TFormFileSelector.cbxFilterFieldDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control).Canvas do
  begin
    FillRect(Rect);
    DataModuleImages.imlFilter.Draw(TComboBox(Control).Canvas,
      Rect.Left + 2, Rect.Top, 0);
    Inc(Rect.Left, DataModuleImages.imlFilter.Width + 2);
    TextRect(Rect, Rect.Left + 4, Rect.Top + 1, TComboBox(Control).Items[Index]);
  end;
end;

procedure TFormFileSelector.edtFilterChange(Sender: TObject);
begin
  TimerFilterUpdate.Enabled := False;
  TimerFilterUpdate.Enabled := True;
end;

procedure TFormFileSelector.edtFilterEnter(Sender: TObject);
begin
  edtFilter.Invalidate;
end;

procedure TFormFileSelector.edtFilterExit(Sender: TObject);
begin
  edtFilter.Invalidate;
end;

procedure TFormFileSelector.edtFilterKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if not (((Key = VK_F4) and (ssAlt in Shift)) or
    (Key in [VK_DELETE, VK_LEFT, VK_RIGHT]) or
    ((Key in [VK_HOME, VK_END]) and not (ssCtrl in Shift)) or
    ((Key in [VK_INSERT]) and ((ssShift in Shift) or (ssCtrl in Shift)))) then
  begin
    SendMessage(ListView.Handle, WM_KEYDOWN, Key, 0);
    Key := 0;
  end
  else
    ListViewKeyDown(ListView, Key, Shift);
end;

procedure TFormFileSelector.edtFilterKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    TimerFilterUpdateTimer(TimerFilterUpdate);
  end;
end;

procedure TFormFileSelector.edtFilterKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not (((Key = VK_F4) and (ssAlt in Shift)) or
    (Key in [VK_DELETE, VK_LEFT, VK_RIGHT]) or
    ((Key in [VK_HOME, VK_END]) and not (ssCtrl in Shift)) or
    ((Key in [VK_INSERT]) and ((ssShift in Shift) or (ssCtrl in Shift)))) then
  begin
    SendMessage(ListView.Handle, WM_KEYUP, Key, 0);
    Key := 0;
  end
  else
    ListViewKeyUp(ListView, Key, Shift);
end;

class function TFormFileSelector.Execute(OpenMode: Boolean): Boolean;
begin
  with TFormFileSelector.Create(nil) do
  try
    Result := InternExecute(OpenMode);
  finally
    Free;
  end;
end;

procedure TFormFileSelector.FilerEditWndProc(var Msg: TMessage);
var
  p: procedure(Instance: TObject; var Msg: TMessage);
  ps: TPaintStruct;
  SaveIndex: Integer;
  Bmp: TBitmap;
begin
  p := @TOpenEdit.WndProc;
  if Msg.Msg = WM_ERASEBKGND then
  begin
    if not edtFilter.Focused and (edtFilter.Text = '') then
    begin
      Msg.Result := 1;
      Exit;
    end;
  end;
  if Msg.Msg = WM_PAINT then
  begin
    if not edtFilter.Focused and (edtFilter.Text = '') then
    begin
      Msg.WParam := BeginPaint(edtFilter.Handle, ps);
      SaveIndex := SaveDC(HDC(Msg.WParam));
      try
        ExcludeClipRect(HDC(Msg.WParam), 1, 1, 1 + DataModuleImages.imlFilter.Width, 1 + DataModuleImages.imlFilter.Height);
        p(edtFilter, Msg);
      finally
        RestoreDC(HDC(Msg.WParam), SaveIndex);
      end;
      Bmp := TBitmap.Create;
      try
        Bmp.Canvas.Brush.Color := clWindow;
        Bmp.Width := DataModuleImages.imlFilter.Width;
        Bmp.Height := DataModuleImages.imlFilter.Height;
        ImageList_Draw(DataModuleImages.imlFilter.Handle, 0, Bmp.Canvas.Handle, 0, 0, ILD_NORMAL or ILD_TRANSPARENT);
        BitBlt(HDC(Msg.WParam), 1, 1, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      finally
        Bmp.Free;
      end;
      EndPaint(edtFilter.Handle, ps);
      Exit;
    end;
  end;
  p(edtFilter, Msg);
end;

function TFormFileSelector.FilterListViewData(S: string): Boolean;
var
  I: Integer;
begin
  Result := FFilterTexts.Count = 0;
  if not Result and (S <> '') then
  begin
    S := AnsiLowerCase(S) + '.';
    for I := 0 to FFilterTexts.Count - 1 do
    begin
      if ContainsAsterix(FFilterTexts[I]) then
        Result := MatchStr(S, FFilterTexts[I])
      else
        Result := Pos(FFilterTexts[I], S) > 0;
      if Result then
        Exit;
    end;
  end;
end;

procedure TFormFileSelector.FormCreate(Sender: TObject);
begin
  FormFileSelector := Self;
  ToolBar.DrawingStyle := ComCtrls.dsGradient;
  ListView.DoubleBuffered := True;
  ToolBar.Flat := True; // BDS 2006's default is True => Flat is not stored in the DFM

  { Delphi 7's designer always looses these information if the dtmImages is not
    open. }
  ListView.SmallImages := DataModuleImages.imlModules;
  ListViewInsertUnits.SmallImages := DataModuleImages.imlModules;
  ToolBar.Images := DataModuleImages.imlApplications;

  FAllData := TInfoList.Create;
  FCurrentData := TInfoList.Create(False);
end;

procedure TFormFileSelector.FormDestroy(Sender: TObject);
begin
  FormFileSelector := nil;
  FCurrentData.Free;
  FAllData.Free;
end;

procedure TFormFileSelector.FormResize(Sender: TObject);
begin
  inherited;
  cbxFilterDirectory.Width := ToolBar.ClientWidth - cbxFilterDirectory.Left;
end;

function DoSortInfos(Item1, Item2: Pointer): Integer;
begin
  case FormFileSelector.FSortColumn of
    0: Result := AnsiCompareText(TInfo(Item1).Name, TInfo(Item2).Name);
    1: Result := AnsiCompareText(TInfo(Item1).FormName, TInfo(Item2).FormName);
    2: Result := AnsiCompareText(TInfo(Item1).FileName, TInfo(Item2).FileName);
  else
    Result := 0;
  end;
  if not FormFileSelector.FSortAsc then
    Result := -1 * Result;
end;

procedure TFormFileSelector.ListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Column.Index <> FSortColumn then
  begin
    FSortColumn := Column.Index;
    FSortAsc := False;
  end
  else
    FSortAsc := not FSortAsc;
  Sort;
  UpdateData;
end;

procedure TFormFileSelector.ListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Info: TInfo;
begin
  DefaultDraw := True;

  Info := Item.Data;
  if Info.Opened then
    Sender.Canvas.Font.Color := clBlue;

  if FParser <> nil then
  begin
    if (FParser.InterfaceUses.FindUses(Info.Name) <> nil) then
      Sender.Canvas.Font.Style := [fsBold]
    else if (FParser.ImplUses.FindUses(Info.Name) <> nil) then
      Sender.Canvas.Font.Style := [fsBold, fsUnderline];
  end;

  if Item.Index mod 2 = 0 then
    Sender.Canvas.Brush.Color := $FAFAFA;
end;

procedure TFormFileSelector.ListViewData(Sender: TObject; Item: TListItem);
var
  Info: TInfo;
begin
  if (Item.Index >= 0) and (Item.Index < FCurrentData.Count) then
  begin
    Info := FCurrentData[Item.Index];
    Item.Data := Info;
    Item.Caption := Info.Name;
    Item.ImageIndex := Info.ImageIndex;
    Item.SubItems.Add(Info.FormName);
    Item.SubItems.Add(Info.RelFileName);
  end;
end;

procedure TFormFileSelector.ListViewDblClick(Sender: TObject);
begin
  if btnUseUnit.Visible and btnUseUnit.Enabled then
    btnUseUnit.Click
  else
  if btnOpen.Enabled then
    btnOpen.Click;
end;

procedure TFormFileSelector.ListViewDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source = ListViewInsertUnits then
    ActionRemoveFromInsertList.Execute;
end;

procedure TFormFileSelector.ListViewDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = ListViewInsertUnits;
end;

procedure TFormFileSelector.ListViewInfoTip(Sender: TObject; Item: TListItem;
  var InfoTip: string);
var
  Info: TInfo;
  FormFileName: string;
begin
  Info := Item.Data;
  InfoTip := Format('<b>Filename:</b> %s' + sLineBreak +
                    '<b>Filesize:</b> %d Bytes',
                    [Info.FileName, GetFileSize(Info.FileName)]);
  if Info.FormName <> '' then
  begin
    FormFileName := ChangeFileExt(Info.FileName, '.dfm');
    if not FastFileExists(FormFileName) then
    begin
//      FormFileName := ChangeFileExt(Info.FileName, '.nfm');
//      if not FastFileExists(FormFileName) then
//      begin
//        FormFileName := ChangeFileExt(Info.FileName, '.xfm');
//        if not FastFileExists(FormFileName) then
          FormFileName := '';
//      end;
    end;

    if FormFileName <> '' then
    begin
      InfoTip := InfoTip + sLineBreak + sLineBreak +
                 Format('<b>Formname:</b> %s' + sLineBreak +
                        '<b>Formsize:</b> %d Bytes',
                        [Info.FormName, GetFileSize(FormFileName)]);
    end;
  end;
end;

procedure TFormFileSelector.ListViewInsertUnitsDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source = ListView then
    ActionAddToInsertList.Execute;
end;

procedure TFormFileSelector.ListViewInsertUnitsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = ListView;
end;

procedure TFormFileSelector.ListViewInsertUnitsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Shift = []) and (Key = VK_DELETE) then
  begin
    ActionRemoveFromInsertList.Execute;
    Key := 0;
  end;
end;

procedure TFormFileSelector.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('A')) then
  begin
    ListView.SelectAll;
    Key := 0;
  end
  else if (Shift = [ssCtrl]) and (Key = VK_RETURN) then
  begin
    if btnOpen.Enabled then
      btnOpen.Click;
    Key := 0;
  end
  else if (Shift = [ssShift]) and (Key = VK_RETURN) then
  begin
    if btnUseUnit.Visible and btnUseUnit.Enabled then
    begin
      chkUseUnitsImplementation.Checked := False;
      btnUseUnit.Click;
    end;
    Key := 0;
  end
  else if (Shift = [ssShift]) and (Key = VK_SHIFT) then
  begin
    chkUseUnitsImplementation.Checked := False;
  end;
end;

procedure TFormFileSelector.ListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Shift = []) and (Key = VK_SHIFT) then
  begin
    chkUseUnitsImplementation.Checked := True;
  end;
end;

procedure TFormFileSelector.ListViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    (Sender as TListView).Selected := (Sender as TListView).GetItemAt(X, Y);
end;

procedure TFormFileSelector.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  btnOpen.Enabled := True;
  btnUseUnit.Enabled := True;
  if Selected and (ListView.Selected <> nil) then
  begin
    ListView.Selected.Focused := True;
    if (ListView.SelCount = 1) and (ListViewInsertUnits.Items.Count = 0) then
    begin
      if IsBinaryFile(TInfo(ListView.Selected.Data).FileName) then
        btnOpen.Enabled := False;

      if FProjectFilename <> '' then
        if SameText(TInfo(ListView.Selected.Data).Name, ChangeFileExt(ExtractFileName(FProjectFilename), '')) then
          btnUseUnit.Enabled := False;
      if FEditor <> nil then
        if SameText(TInfo(ListView.Selected.Data).Name, ChangeFileExt(ExtractFileName(FEditor.FileName), '')) then
          btnUseUnit.Enabled := False;;
    end;
  end;
  mniOpenFile.Enabled := btnOpen.Enabled;
end;

procedure TFormFileSelector.GetFilenames;

  procedure CollectFiles({$IFNDEF COMPILER12_UP}const Project: IOTAProject;{$ENDIF}
    PrjFiles, Files: TStrings; FilesHash: TStringIntegerHash; const Dir: string;
    SourceOnly: Boolean);
  const
    FindExInfoStandard = _FINDEX_INFO_LEVELS(0);
    FindExInfoBasic = _FINDEX_INFO_LEVELS(1);
    FindExInfoMaxInfoLevel = _FINDEX_INFO_LEVELS(2);

    FIND_FIRST_EX_CASE_SENSITIVE = 1;
    FIND_FIRST_EX_LARGE_FETCH = 2;
  var
    SearchHandle: THandle;
    FindData: TWin32FindData;
    FileName: string;
    Ext, UpName: string;
    Index: Integer;
    IsSource: Boolean;
    {$IFNDEF COMPILER12_UP}
    PersStr: string;
    {$ENDIF ~COMPILER12_UP}
  begin
    // "PrjFiles" is nil if "Files" points to the "SourceFiles".

    if CheckWin32Version(6, 1) then // we don't need the "AlternativeName", but want speed
      SearchHandle := FindFirstFileEx(PChar(Dir + PathDelim + '*.*'), FindExInfoBasic, @FindData, FindExSearchNameMatch, nil, FIND_FIRST_EX_LARGE_FETCH)
    else
      SearchHandle := FindFirstFile(PChar(Dir + PathDelim + '*.*'), FindData);

    if SearchHandle <> INVALID_HANDLE_VALUE then
    try
      repeat
        if (FindData.dwFileAttributes and (FILE_ATTRIBUTE_DIRECTORY or FILE_ATTRIBUTE_DEVICE)) = 0 then
        begin
          FileName := FindData.cFileName;
          Ext := ExtractFileExt(FileName);
          IsSource := SameText(Ext, '.pas');
          if IsSource or
            (not SourceOnly and (SameText(Ext, '.dcu') {$IFNDEF COMPILER12_UP}or SameText(Ext, '.dcuil'){$ENDIF})) then
          begin
            {$IFNDEF COMPILER12_UP}
            if not IsSource and (Project <> nil) then
            begin
              PersStr := Project.Personality;
              if (PersStr = sDelphiPersonality) and SameText(Ext, '.dcuil') then
                Continue
              else
              if (PersStr = sDelphiDotNetPersonality) and SameText(Ext, '.dcu') then
                Continue;
            end;
            {$ENDIF ~COMPILER12_UP}
            UpName := AnsiUpperCase(ChangeFileExt(FileName, ''));
            { Only add files that aren't already found in a more prioritized
              directory. }
            if not FilesHash.Find(UpName, Index) then
            begin
              Index := Files.AddObject(Dir + PathDelim + FileName, TObject(IsSource));
              if PrjFiles <> nil then
                Inc(Index, PrjFiles.Count);
              FilesHash.Add(UpName, Index);
            end
            else
            begin
              { Prefer source files }
              if IsSource then
              begin
                if (PrjFiles <> nil) and (Index < PrjFiles.Count) then
                begin
                  if not Boolean(PrjFiles.Objects[Index]) then // only replace if the previous was no source file
                  begin
                    PrjFiles[Index] := Dir + PathDelim + FileName;
                    PrjFiles.Objects[Index] := TObject(IsSource);
                  end;
                end
                else
                begin
                  if PrjFiles <> nil then
                    Dec(Index, PrjFiles.Count);
                  if not Boolean(Files.Objects[Index]) then // only replace if the previous was no source file
                  begin
                    Files[Index] := Dir + PathDelim + FileName;
                    Files.Objects[Index] := TObject(IsSource);
                  end;
                end;
              end;
            end;
          end;
        end;
      until not FindNextFile(SearchHandle, FindData);
    finally
      Windows.FindClose(SearchHandle);
    end;
  end;

var
  Modules: TStrings;
  HashTable: TStringIntegerHash;

  procedure AddProjectModules(const Project: IOTAProject; Files: TStrings; FilesHash: TStringIntegerHash);
  var
    I, Index: Integer;
    ModuleInfo: IOTAModuleInfo;
    FileName: string;
  begin
    { Add all modules of the project }
    for I := 0 to Project.GetModuleCount - 1 do
    begin
      ModuleInfo := Project.GetModule(I);
      Filename := ModuleInfo.FileName;
      if not SameText(ExtractFileExt(Filename), '.dcp') then
      begin
        Index := Modules.Add(Filename);
        Modules.Add(ModuleInfo.FormName);
        HashTable.Add(AnsiUpperCase(Filename), Index);

        if Filename <> '' then
        begin
          { Project files are prioritized }
          Index := Files.Add(Filename);
          FilesHash.Add(AnsiUpperCase(ChangeFileExt(ExtractFileName(Filename), '')), Index);
        end;
      end;
    end;
  end;

  procedure AddProjectSource(const Project: IOTAProject; PrjFiles, Files: TStrings; FilesHash: TStringIntegerHash);
  var
    I, Index: Integer;
    ModuleInfo: IOTAModuleInfo;
    FileName, UpName: string;
  begin
    { Add all source files of the project }
    for I := 0 to Project.GetModuleCount - 1 do
    begin
      ModuleInfo := Project.GetModule(I);
      Filename := ModuleInfo.FileName;
      if not SameText(ExtractFileExt(Filename), '.dcp') then
      begin
        UpName := AnsiUpperCase(ChangeFileExt(ExtractFileName(Filename), ''));
        if not FilesHash.Find(UpName, Index) then
        begin
          Index := Files.AddObject(Filename, TObject(True));
          if PrjFiles <> nil then
            Inc(Index, PrjFiles.Count);
          FilesHash.Add(UpName, Index);
        end
        else
        begin
          { Prefer source files }
          if (PrjFiles <> nil) and (Index < PrjFiles.Count) then
          begin
            PrjFiles[Index] := Filename;
            PrjFiles.Objects[Index] := TObject(True);
          end
          else
          begin
            if PrjFiles <> nil then
              Dec(Index, PrjFiles.Count);
            Files[Index] := Filename;
            Files.Objects[Index] := TObject(True);
          end;
        end;
      end;
    end;
  end;

var
  I, k: Integer;
  Module: IOTAModule;
  PrjEditor: IOTAEditor;
  ModuleServices: IOTAModuleServices;
  Filename, RelDir: string;
  Info: TInfo;
  Files: TStringList;
  {HashTable, }FilesHash, SourcesHash: TStringIntegerHash;

  Project: IOTAProject;
  Group: IOTAProjectGroup;
  Options: IOTAProjectOptions;
  Dirs: string;
  DirList, Sources: TStringList;
  Index{, StartIndex}: Integer;
  CurDir: string;

  OpenModules: TStringList;
begin
  FCurrentData.Clear;
  FAllData.Clear;

  ModuleServices := BorlandIDEServices as IOTAModuleServices;

  Modules := TStringList.Create;
  DirList := TStringList.Create;
  Files := TStringList.Create;
  Sources := TStringList.Create;
  FilesHash := TStringIntegerHash.Create;
  HashTable := TStringIntegerHash.Create;
  SourcesHash := TStringIntegerHash.Create;
  OpenModules := TStringList.Create;
  try
    Options := nil;
    FProjectFilename := '';
    Dirs := '';
    Project := GetActiveProject;

    { Get as much information from the project as possible }
    if Project <> nil then
    begin
      { Add project source file }
      if Supports(Project, IOTAModule, Module) then
      begin
        for I := 0 to Module.GetModuleFileCount - 1 do
        begin
          PrjEditor := Module.GetModuleFileEditor(I);
          if Supports(PrjEditor, IOTASourceEditor) then
          begin
            Filename := PrjEditor.FileName;
            Index := Modules.Add(Filename);
            Modules.Add('');
            HashTable.Add(AnsiUpperCase(Filename), Index);
            if Filename <> '' then
            begin
              { Project files are prioritized }
              Index := Files.Add(Filename);
              FilesHash.Add(AnsiUpperCase(ChangeFileExt(ExtractFileName(Filename), '')), Index);
            end;
            Break;
          end;
        end;
      end;

      AddProjectModules(Project, Files, FilesHash);
    end;

    { --- Add all files in the search path --- }

    { Obtain the right directory order }
    if Project <> nil then
    begin
      FProjectFilename := Project.FileName;
      Dirs := Dirs + ';' + ExtractFilePath(FProjectFilename);
      Dirs := Dirs + ';' + VarToStrDef(Project.ProjectOptions.Values['UnitDir'], '');
    end;

    Dirs := Dirs + ';' + GetProjectEnvOptionPaths(Project, 'LibraryPath');
    Dirs := ExpandDirMacros(Dirs, Project);

    { Find files in relative search and browsing paths }
   { if (CachedCurDir = ExtractFileDir(FProjectFilename)) and (CachedDirs = Dirs) then
    begin
      StartIndex := Files.Count;
      Files.AddStrings(CachedFiles);
      for I := 0 to CachedFiles.Count - 1 do
        FilesHash.Add(AnsiUpperCase(ChangeFileExt(ExtractFileName(CachedFiles[I]), '')), StartIndex + I);
      Sources.Assign(CachedSources);
      for I := 0 to CachedSources.Count - 1 do
        SourcesHash.Add(AnsiUpperCase(ChangeFileExt(ExtractFileName(CachedSources[I]), '')), I);
    end
    else}
    begin
      CurDir := GetCurrentDir;
      try
        if FProjectFilename <> '' then
        begin
          CachedCurDir := ExtractFileDir(FProjectFilename);
          SetCurrentDir(CachedCurDir);
        end
        else
          CachedCurDir := CurDir;

        CachedDirs := Dirs;
        if CachedFiles <> nil then CachedFiles.Clear else
          CachedFiles := TStringList.Create;
        if CachedSources <> nil then CachedSources.Clear else
          CachedSources := TStringList.Create;

        { Collect source and DCU files }
        DirList.Clear;
        SplitPaths(DirList, Dirs, True);
        for I := 0 to DirList.Count - 1 do
          CollectFiles({$IFNDEF COMPILER12_UP}Project,{$ENDIF} Files, CachedFiles, FilesHash,
            ExpandFileName(ExcludeTrailingPathDelimiter(DirList[I])), False);
        Files.AddStrings(CachedFiles);

        { Collect source files for DCUs without source in the library path }
        Dirs := GetProjectEnvOptionPaths(Project, 'BrowsingPath');
        Dirs := ExpandDirMacros(Dirs, Project);
        DirList.Clear;
        SplitPaths(DirList, Dirs, True);
        for I := 0 to DirList.Count - 1 do
          CollectFiles({$IFNDEF COMPILER12_UP}Project,{$ENDIF} nil, Sources, SourcesHash,
            ExpandFileName(ExcludeTrailingPathDelimiter(DirList[I])), True);

        // Add the source files from other projects in the group, that the active projects can
        // access through DCU files.
        Group := GetActiveProjectGroup();
        if Group <> nil then
          for I := 0 to Group.ProjectCount - 1 do
            if Group.Projects[I] <> Project then
              if Group.Projects[I].ProjectType = sPackage then
                AddProjectSource(Group.Projects[I], nil, Sources, SourcesHash);

        CachedSources.Assign(Sources);
      finally
        SetCurrentDir(CurDir);
      end;
    end;

    { Fill the OpenModules hash table }
    for I := 0 to ModuleServices.ModuleCount - 1 do
    begin
      Module := ModuleServices.Modules[I];
      if Module.ModuleFileCount = 0 then
        OpenModules.Add(Module.FileName)
      else
        for k := 0 to Module.ModuleFileCount - 1 do
          OpenModules.Add(Module.ModuleFileEditors[k].FileName);
    end;
    OpenModules.Sorted := True;

    { Fill FAllData }
    DirList.Clear; // now used for the Directory-ComboBox
    DirList.Sorted := True;
    for I := 0 to Files.Count - 1 do
    begin
      Info := TInfo.Create;
      FAllData.Add(Info);
      Filename := Files[I];
      if not Boolean(Files.Objects[I]) then
      begin
        // Lookup for the source file
        if SourcesHash.Find(AnsiUpperCase(ChangeFileExt(ExtractFileName(Filename), '')), Index) then
          Filename := Sources[Index];
      end;

      { Lookup the file in the project's module list }
      k := -1;
      if Filename <> '' then
      begin
        if not HashTable.Find(AnsiUpperCase(Filename), k) then
          k := -1;
      end;
      if k >= 0 then
      begin
        Info.FileName := Modules[k];
        Info.FormName := Modules[k + 1];
        if Info.FormName <> '' then
          Info.ImageIndex := imgModuleForm
        else
          Info.ImageIndex := imgModuleUnit;
      end
      else
      begin
        Info.Filename := Filename;
        Info.FormName := '';
        if IsBinaryFile(Filename) then
          Info.ImageIndex := imgModuleBinary
        else
          Info.ImageIndex := imgModuleUnit;
      end;

      Info.Name := ChangeFileExt(ExtractFileName(Filename), '');
      if Info.Name = Info.FormName then
        Info.Name := ChangeFileExt(ExtractFileName(Info.Filename), '');
      {if Project <> nil then
        Info.RelFileName := ExtractRelativePath(ExtractFilePath(FProjectFilename), Info.Filename)
      else}
        Info.RelFileName := Info.Filename;
      Info.Opened := OpenModules.IndexOf(Info.FileName) <> -1;

      { fill directory list }
      RelDir := ExtractFileDir(Info.RelFileName);
      if (RelDir <> '') and (DirList.IndexOf(RelDir) = -1) then
        DirList.Add(RelDir);
    end;

    //DirList.Sort;
    DirList.Sorted := False;
    DirList.Insert(0, RsFilterAllDirectories);

    cbxFilterDirectory.Items.Assign(DirList);
    cbxFilterDirectory.ItemIndex := 0;
    cbxFilterDirectory.Enabled := DirList.Count > 2;
  finally
    FilesHash.Free;
    Files.Free;
    SourcesHash.Free;
    Sources.Free;
    HashTable.Free;
    DirList.Free;
    Modules.Free;
    OpenModules.Free;
  end;
end;

procedure TFormFileSelector.SaveSettings;
var
  Node: IXmlNode;
  I: Integer;
begin
  Node := Configuration.GetNode('FileSelector');
  Node.Attributes['AllowMoveFromInterfaceToImpl'] := mniAllowMoveFromInterfaceToImpl.Checked;
  Node.Attributes['EveryUnitOnSingleLine'] := mniEveryUnitOnSingleLine.Checked;

  if cbxFilterDirectory.ItemIndex > 0 then
    Node.Attributes['LastFilterDir'] := cbxFilterDirectory.Text
  else
    Node.Attributes['LastFilterDir'] := Null;
  Node := Configuration.GetNode(Node, 'Layout');
  with Configuration.GetNode(Node, 'Size') do
  begin
    Attributes['ClientWidth'] := ClientWidth;
    Attributes['ClientHeight'] := ClientHeight;
  end;
  with Configuration.GetNode(Node, 'Columns') do
  begin
    Attributes['SortColumn'] := FSortColumn;
    Attributes['SortAsc'] := FSortAsc;
    Attributes['ColumnFilter'] := cbxFilterField.ItemIndex;
    ChildNodes.Clear;
    for I := 0 to ListView.Columns.Count - 1 do
      AddChild('Column').Attributes['Width'] := ListView.Columns[I].Width;
  end;
  Configuration.Modified;
end;

procedure TFormFileSelector.LoadSettings;
var
  Xml, Node: IXmlNode;
  I, ColIndex: Integer;
  Dir: string;
begin
  Node := Configuration.FindNode('FileSelector');
  if Node <> nil then
  begin

    mniAllowMoveFromInterfaceToImpl.Checked := VarToBoolDef(Node.Attributes['AllowMoveFromInterfaceToImpl'], False);
    mniEveryUnitOnSingleLine.Checked := VarToBoolDef(Node.Attributes['EveryUnitOnSingleLine'], False);

    Dir := VarToStrDef(Node.Attributes['LastFilterDir'], '');
    if Dir <> '' then
    begin
      I := cbxFilterDirectory.Items.IndexOf(Dir);
      if I <> -1 then
        cbxFilterDirectory.ItemIndex := I;
    end;

    Node := Node.ChildNodes.FindNode('Layout');
    if Node <> nil then
    begin
      Xml := Node.ChildNodes.FindNode('Size');
      if Xml <> nil then
      begin
        ClientWidth := VarToIntDef(Xml.Attributes['ClientWidth'], ClientWidth);
        ClientHeight := VarToIntDef(Xml.Attributes['ClientHeight'], ClientHeight);
      end;

      Xml := Node.ChildNodes.FindNode('Columns');
      if Xml <> nil then
      begin
        FSortColumn := VarToIntDef(Xml.Attributes['SortColumn'], FSortColumn);
        FSortAsc := VarToBoolDef(Xml.Attributes['SortAsc'], FSortAsc);
        cbxFilterField.ItemIndex := VarToIntDef(Xml.Attributes['ColumnFilter'], cbxFilterField.ItemIndex);
        ColIndex := 0;
        for I := 0 to Xml.ChildNodes.Count - 1 do
        begin
          if Xml.ChildNodes[I].NodeName = 'Column' then
          begin
            ListView.Columns[ColIndex].Width := VarToIntDef(Xml.ChildNodes[I].Attributes['Width'], ListView.Columns[ColIndex].Width);
            Inc(ColIndex);
            if ColIndex >= ListView.Columns.Count then
              Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFormFileSelector.mniOpenFileClick(Sender: TObject);
begin
  inherited;
  btnOpen.Click;
end;

procedure TFormFileSelector.RefreshButtons;
begin
  ListViewSelectItem(ListView, ListView.Selected, ListView.Selected <> nil); // refresh Open/UseUnit buttons
end;

procedure TFormFileSelector.TimerFilterUpdateTimer(Sender: TObject);
begin
  TimerFilterUpdate.Enabled := False;
  UpdateData;
end;

procedure TFormFileSelector.UpdateData;
begin
  FFilterField := cbxFilterField.ItemIndex;

  ListView.Items.BeginUpdate;
  try
    ListView.Items.Clear;
    UpdateListViewData;
  finally
    ListView.Items.EndUpdate;
  end;

  StatusBar.Panels[0].Text := Format(RsNumberOfModules, [FCurrentData.Count]);
end;

function TFormFileSelector.FilterInfo(Info: TInfo; var NewSelLen: Integer): Boolean;
begin
  Result := False;

  { Filter by directory }
  if FDirectoryFiltered then
  begin
    if FDirectoryFilter <> '' then
    begin
      if not StartsText(FDirectoryFilter, Info.RelFileName) then
        Exit;
    end
    else
    if StartsText('..' + PathDelim, Info.RelFileName) or (Pos(':', Info.RelFileName) > 0) then
      Exit;
  end;

  case FFilterField of
    0:
      begin
        Result := FilterListViewData(Info.Name);
        if Result then
          NewSelLen := Length(Info.Name)
        else
        begin
          NewSelLen := Length(Info.FormName);
          Result := FilterListViewData(Info.FormName);
        end;
      end;
    1:
      begin
        NewSelLen := Length(Info.Name);
        Result := FilterListViewData(Info.Name);
      end;
    2:
      begin
        NewSelLen := Length(Info.FormName);
        Result := FilterListViewData(Info.FormName);
      end;
  end;
end;

procedure TFormFileSelector.UpdateListViewData;
var
  I, SelIndex: Integer;
  Info: TInfo;
  SelLen, NewSelLen: Integer;
begin
  FCurrentData.Clear;
  FFilterTexts.Clear;
  SplitFilterText(AnsiLowerCase(edtFilter.Text), FFilterTexts);
  for I := 0 to FFilterTexts.Count - 1 do
  begin
    if ContainsAsterix(FFilterTexts[I]) then
    begin
      if not CharInSet(FFilterTexts[I][1], ['*', '?']) then
        FFilterTexts[I] := '*' + FFilterTexts[I];
      if not CharInSet(FFilterTexts[I][Length(FFilterTexts[I])], ['*', '?']) then
        FFilterTexts[I] := FFilterTexts[I] + '*';
    end;
  end;


  FDirectoryFilter := '';
  FDirectoryFiltered := False;
  if cbxFilterDirectory.ItemIndex > 0 then
  begin
    FDirectoryFiltered := True;
    FDirectoryFilter := cbxFilterDirectory.Items[cbxFilterDirectory.ItemIndex];
    if FDirectoryFilter <> '' then
      FDirectoryFilter := FDirectoryFilter + PathDelim;
  end;

  SelLen := MaxInt;
  SelIndex := -1;
  { collect filtered info items }
  for I := 0 to FAllData.Count - 1 do
  begin
    Info := FAllData[I];
    if FilterInfo(Info, NewSelLen) then
    begin
      if (FFilterTexts.Count <> 0) and (NewSelLen > 0) and (NewSelLen < SelLen) then
      begin
        SelIndex := FCurrentData.Count;
        SelLen := NewSelLen;
      end;
      FCurrentData.Add(Info);
    end;
  end;

  ListView.Items.Count := FCurrentData.Count;
  ListView.Invalidate;

  if FCurrentData.Count > 0 then
  begin
    if SelIndex = -1 then
      SelIndex := 0;
    ListView.Items[SelIndex].Selected := True;
    ListView.Items[SelIndex].MakeVisible(False);
  end;
end;

procedure TFormFileSelector.Sort;
begin
  FAllData.Sort(DoSortInfos);
  UpdateData;
end;

procedure TFormFileSelector.RemoveUsesUnits(Writer: IOTAEditWriter; StopIndex: Integer;
  DeleteUsesList: TList; UsesList: TUsesList);

  function FindCommaLocation(out CommaBegin, CommaEnd: Integer;
    UsesList: TUsesList; Item: TUsesItem): Boolean;
  var
    Idx: Integer;
  begin
    CommaBegin := -1;
    CommaEnd := -1;
    Idx := UsesList.IndexOf(Item);
    Result := Idx <> -1;
    if Result then
    begin
      if Idx = UsesList.Count - 1 then
      begin
        if Idx = 0 then
          Write; // TODO: remove "uses" in a way that adding it would be still possible
        CommaBegin := UsesList[Idx - 1].EndLocation.Index; // includes the "," / ";"
        CommaEnd := Item.StartLocation.Index + 1;
        // last unit terminated by ";"
      end
      else
      begin
        // unit after "uses" and before last unit terminated by ","
        CommaBegin := Item.EndLocation.Index; // includes the "," / ";"
        CommaEnd := UsesList[Idx + 1].StartLocation.Index - 1;
      end;
    end;
  end;

var
  I: Integer;
  Item: TUsesItem;
  EndIndex, Len, CommaBegin, CommaEnd, CommaFindEnd: Integer;
begin
  // The items in DeleteUsesList[] are either interface-uses or implementation-uses items but
  // never both because we can only add to one uses list. The user has only one checkbox ;-)
  // That makes things a lot easier because we can delete everything without thinking about how
  // to delete with a possible add at the some location.

  if (UsesList.Count = 0) or (DeleteUsesList.Count = 0) then
    Exit;

  Len := Length(FParser.Text);
  if UsesList.Count = DeleteUsesList.Count then
  begin
    // We delete all uses items => we have to delete the whole "uses" clause.
    EndIndex := UsesList.EndLocation.Index;
    // include the line breaks
    Inc(EndIndex);
    while (EndIndex < Len) and (FParser.Text[EndIndex] in [#10, #13]) do
      Inc(EndIndex);
    Writer.CopyTo(UsesList.Location.Index - 1);
    Writer.DeleteTo(EndIndex - 1);
  end
  else
  begin
    I := 0;
    while (I < DeleteUsesList.Count) do
    begin
      Item := DeleteUsesList[I];
      if (StopIndex <> -1) and (Item.StartLocation.Index >= StopIndex) then
        Break;

      // Find the "," that we also have to remove
      FindCommaLocation({out} CommaBegin, {out} CommaEnd, UsesList, Item);
      if CommaBegin <> -1 then
      begin
        while (CommaBegin <= CommaEnd) and (CommaBegin < Len) and (FParser.Text[CommaBegin] <> ',') do
          Inc(CommaBegin);
        if CommaBegin <= CommaEnd then
        begin
          // We found the "," but we also have to remove the space after it (", ") if it exists
          CommaFindEnd := CommaBegin;
          while (CommaFindEnd + 1 <= CommaEnd) and (FParser.Text[CommaFindEnd + 1] in [#9, ' ', #10, #13]) do
            Inc(CommaFindEnd, 1);
          CommaEnd := CommaFindEnd;
        end;
      end;

      // remove ","
      if (CommaBegin <> -1) and (CommaBegin < Item.StartLocation.Index) then
      begin
        Writer.CopyTo(CommaBegin - 1);
        Writer.DeleteTo(CommaEnd);
      end;
      // remove unit name
      Writer.CopyTo(Item.StartLocation.Index - 1);
      Writer.DeleteTo(Item.EndLocation.Index - 1);
      // remove ","
      if (CommaBegin <> -1) and (CommaBegin >= Item.EndLocation.Index) then
      begin
        Writer.CopyTo(CommaBegin - 1);
        Writer.DeleteTo(CommaEnd);
      end;

      Inc(I);
    end;
  end;
end;

function SortUsesItemByLocationIndex(Item1, Item2: Pointer): Integer;
begin
  Result := TUsesItem(Item1).StartLocation.Index - TUsesItem(Item2).StartLocation.Index;
end;

procedure TFormFileSelector.UseSelectedUnits(Files: TList);
var
  I: Integer;
  UnitNames, FileNames: TStrings;
  S: string;
  UsesList: TUsesList;
  AddIndex: Integer;
  Writer: IOTAEditWriter;
  Column: Integer;
  RightMargin: Integer;
  EditOptions: IOTAEditOptions;
  PrjName: string;
  ModuleName, FileName: string;
  FoundInImpl, FoundInIntf: TUsesItem;
  UseUnitsImplementation, AllowMoveFromInterfaceToImpl, EveryUnitOnSingleLine: Boolean;
  DeleteUsesList: TList;
begin
  if (FEditor = nil) or (FParser = nil) then
    Exit;

  UseUnitsImplementation := chkUseUnitsImplementation.Checked;
  AllowMoveFromInterfaceToImpl := mniAllowMoveFromInterfaceToImpl.Checked;
  EveryUnitOnSingleLine := mniEveryUnitOnSingleLine.Checked;

  RightMargin := 80;
  EditOptions := (BorlandIDEServices as IOTAEditorServices).EditOptions;
  if EditOptions <> nil then
    RightMargin := EditOptions.BufferOptions.RightMargin;

  if FProjectFilename <> '' then
    PrjName := ChangeFileExt(ExtractFileName(FProjectFilename), '');
  if FEditor <> nil then
    ModuleName := ChangeFileExt(ExtractFileName(FEditor.FileName), '');

  UnitNames := TStringList.Create;
  FileNames := TStringList.Create;
  DeleteUsesList := TList.Create;
  try
    for I := 0 to Files.Count - 1 do
    begin
      FileName := TInfo(Files[I]).FileName;
      S := ChangeFileExt(ExtractFileName(FileName), '');
      if (PrjName <> '') and SameText(S, PrjName) then
        Continue;
      if (ModuleName <> '') and SameText(S, ModuleName) then
        Continue;

      UnitNames.Add(S);
      FileNames.Add(FileName);
    end;

    if UseUnitsImplementation then
    begin
      UsesList := FParser.ImplUses;
      AddIndex := FParser.ImplLocation.Index;
      if AddIndex > 0 then
        Inc(AddIndex, Length('implementation') - 1);
    end
    else
    begin
      UsesList := FParser.InterfaceUses;
      AddIndex := FParser.InterfaceLocation.Index;
      if AddIndex > 0 then
        Inc(AddIndex, Length('interface') - 1);
    end;

    { Remove already used units from the "add list" }
    for I := UnitNames.Count - 1 downto 0 do
    begin
      FoundInImpl := FParser.ImplUses.FindUses(UnitNames[I]);
      FoundInIntf := FParser.InterfaceUses.FindUses(UnitNames[I]);
      if ((FoundInImpl <> nil) and UseUnitsImplementation) or
         ((FoundInIntf <> nil) and (not UseUnitsImplementation or not AllowMoveFromInterfaceToImpl)) then
      begin
        UnitNames.Delete(I);
        FileNames.Delete(I);
      end
      else
      begin
        { Add the already added UsesItem to the delete list if the user wants the unit
          to be moved to the other location (implementation vs. interferface) }
        if (FoundInImpl <> nil) and not UseUnitsImplementation then
          DeleteUsesList.Add(FoundInImpl);
        if (FoundInIntf <> nil) and UseUnitsImplementation then
          DeleteUsesList.Add(FoundInIntf);

        { If the file doesn't exist yet (created in memory) we use the already available
          unit name, instead of looking into the file's unit-header. }
        if FileExists(FileNames[I]) then
          UnitNames[I] := GetUnitNameFromFile(FileNames[I])
      end;
    end;

    if UnitNames.Count > 0 then
    begin
      Writer := FEditor.CreateUndoableWriter;

      // Sort the delete list by source code index to allow us to delete them in order
      DeleteUsesList.Sort(SortUsesItemByLocationIndex);

      // Remove interface-uses items if we add to the implementation-uses.
      if UseUnitsImplementation and AllowMoveFromInterfaceToImpl then
        RemoveUsesUnits(Writer, AddIndex, DeleteUsesList, FParser.InterfaceUses);

      { Concat the unit names and insert the LineBreaks where necessary }
      S := '';
      Column := 3;
      if UsesList.Count > 0 then
      begin
        Column := UsesList.EndLocation.Column;
        if EveryUnitOnSingleLine or (Column + 2 + Length(UnitNames[0]) >= RightMargin) then
        begin
          S := sLineBreak + '  ';
          Column := 3;
        end;
      end;

      S := S + UnitNames[0] + ',';
      Inc(Column, 2 + Length(UnitNames[0]));
      for I := 1 to UnitNames.Count - 1 do
      begin
        if EveryUnitOnSingleLine or (Column >= RightMargin) then
        begin
          S := S + sLineBreak + ' ';
          Column := 3;
        end;
        S := S + ' ' + UnitNames[I] + ',';
        Inc(Column, 2 + Length(UnitNames[I]));
      end;

      // cut trailing ','
      SetLength(S, Length(S) - 1);

      { Add the added units }
      if UsesList.Count > 0 then
      begin
        AddIndex := UsesList.EndLocation.Index - 1;
        S := ', ' + S;
      end
      else
        S := sLineBreak + sLineBreak + 'uses' + sLineBreak + '  ' + S + ';';

      if AddIndex <= 0 then
        raise Exception.Create(sParseErrorUsesLocationNotFound);

      Writer.CopyTo(AddIndex);
      Writer.Insert(PAnsiChar(Utf8Encode(S)));
      // Remove implementation-uses items if we add to the interface-uses.
      if not UseUnitsImplementation then
        RemoveUsesUnits(Writer, -1, DeleteUsesList, FParser.ImplUses);
      Writer.CopyTo(MaxInt);
      Writer := nil;
    end;
  finally
    DeleteUsesList.Free;
    UnitNames.Free;
    FileNames.Free;
  end;
end;

function TFormFileSelector.GetCurrentSourceEditor: IOTASourceEditor;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  I: Integer;
  Editor: IOTAEditor;
begin
  Result := nil;
  if Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) and
     (ModuleServices.CurrentModule <> nil) and
     SameText(ExtractFileExt(ModuleServices.CurrentModule.FileName), '.pas') then
  begin
    Module := ModuleServices.CurrentModule;
    for I := 0 to Module.GetModuleFileCount - 1 do
    begin
      Editor := Module.GetModuleFileEditor(I);
      if SameText(ExtractFileExt(Editor.FileName), '.pas') and
         Supports(Editor, IOTASourceEditor, Result) then
      begin
        Break;
      end;
    end;
  end;
end;

procedure TFormFileSelector.btnOptionsClick(Sender: TObject);
var
  R: TRect;
begin
  GetWindowRect(btnOptions.Handle, R);
  popOptions.Popup(R.Left, R.Bottom);
end;

initialization

finalization
  FreeAndNil(CachedFiles);
  FreeAndNil(CachedSources);
end.

