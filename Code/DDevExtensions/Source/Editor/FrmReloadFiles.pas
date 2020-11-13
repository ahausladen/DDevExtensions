unit FrmReloadFiles;

{
  Replaces Delphi's CheckFileDates() function by a version that collects the changed files and
  shows one dialog for all files. This dialog has the following features:
    - Checkboxes to select which files should be reloaded
    - If a diff tool (Beyond Compare, TortoiseSVN (TMerge), TortoiseGIT (TMerge), TortoiseHG (kdiff3)
      is installed the context menu shows "Diff <filename>" items and at the bottom a combobox for
      the file editors and "Diff" butten become visible.
    - The context menu allows to select/deselect items
    - With the "Show in Explorer" context menu item the main file can be opened in the Windows Explorer
    - If the user switches to another application makes changes further files and switches back to
      the IDE, the already opened ReloadFiles form will be updated with the "new" modules.
//    - Can reload forms with dependencies by unloading (GoDormant) the dependencies and resurrecting
//      them after the reload.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Contnrs, Graphics, Controls, Forms, Dialogs, FrmBase,
  ComCtrls, StdCtrls, ExtCtrls, ImgList, Menus, ToolsAPI,
  DocModuleHandler;

const
  WM_LISTVIEWSELCHANGED = WM_USER + 1;

type
  TFormReloadFiles = class(TFormBase)
    PanelButtons: TPanel;
    bvlDivider: TBevel;
    btnOk: TButton;
    btnCancel: TButton;
    pnlButtons: TPanel;
    ListViewModules: TListView;
    ImageListModules: TImageList;
    PopupMenuModules: TPopupMenu;
    mniSelectOnlyModifiedFiles: TMenuItem;
    mniInvertSelection: TMenuItem;
    mniDeselectAll: TMenuItem;
    mniSelectAll: TMenuItem;
    mniSelectOnlyUnmodifiedField: TMenuItem;
    ComboBoxDiff: TComboBox;
    btnDiff: TButton;
    N1: TMenuItem;
    mniShowInExplorer: TMenuItem;
    PopupMenuDiff: TPopupMenu;
    procedure mniDeselectAllClick(Sender: TObject);
    procedure PopupMenuModulesPopup(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListViewModulesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnDiffClick(Sender: TObject);
    procedure mniShowInExplorerClick(Sender: TObject);
    procedure ListViewModulesCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Private-Deklarationen }
    FProjectGroup: TListGroup;
    FUnitGroup: TListGroup;
    FFormGroup: TListGroup;
    FModules: TList;
    FCannotReloadModules: TList;
    FDiffer: string; // support macros %base %mine %basename %minename
    FListViewWindowProc: TWndMethod;

    class procedure ReloadFiles(Modules: TList); static;
    procedure AddModuleListItem(AModule: TDocModule);
    procedure AddModules(AModules: TList);
    procedure DiffMenuItemClick(Sender: TObject);
    procedure DiffEditor(Editor: IOTAEditor);
    procedure WMListViewSelChanged(var Msg: TMessage); message WM_LISTVIEWSELCHANGED;
    procedure ListViewWndProc(var Msg: TMessage);
    procedure ListItemDblClick(Item: TListItem);
    procedure AddDiffPopupItems(ADocModule: TDocModule; APopupMenu: TPopupMenu);
  public
    { Public-Deklarationen }
    function ShowDialog(AModules: TList): Boolean;
  end;

procedure InitPlugin(Unload: Boolean);

implementation

uses
  Hooking, IDEHooks, StrUtils, ActiveX, Registry, ComObj, IDEUtils, ShellAPI, AppConsts,
  Consts, CommCtrl, ToolsAPIHelpers;

{$R *.dfm}

type
{  TPascalCodeMgrModHandlerEx = class(TPascalCodeMgrModHandler)
  public
    procedure ReloadFile;
  end;}

  TFileEditorMenuItem = class(TMenuItem)
  private
    FEditor: IOTAEditor;
  public
    property Editor: IOTAEditor read FEditor write FEditor;
  end;

  TWaitItem = class(TObject)
  private class var
    WndHandle: HWND;
    class procedure WndProc(var Msg: TMessage);
  private
    FFileName: string;
    FProcessHandle: THandle;
    FWaitHandle: THandle;
  public
    constructor Create(const AFileName: string; AProcessHandle: THandle);
    destructor Destroy; override;

    function Start: Boolean;
  end;

var
  OrgCallCheckFileDates: procedure(NoPrompt: Boolean);
  //OrgReloadFile: procedure(Instance: TPascalCodeMgrModHandler);
  //ReloadingModules: Boolean;
  ReloadFileForm: TFormReloadFiles;

{$IF CompilerVersion >= 22.0} // XE+
procedure Docmodul_CheckFileDates(NoPrompt: Boolean);
  external coreide_bpl name '@Docmodul@CheckFileDates$qqro';
{$ELSE}
procedure Docmodul_CheckFileDates;
  external coreide_bpl name '@Docmodul@CheckFileDates$qqrv';
{$IFEND}

{$IF CompilerVersion >= 23.0} // XE2+
{var
  OrgDocModuleCanFree: function(Instance: TDocModule): Boolean;

function DocModuleCanFree(Instance: TDocModule): Boolean;
begin
  Result := Instance.CanFreeOrGoDormant(ReloadingModules);
end;}
{$IFEND}

{ TPascalCodeMgrModHandlerEx }

(*
procedure TPascalCodeMgrModHandlerEx.ReloadFile;
var
  FormWasTopmost: Boolean;
  DesignerState: TDesignerState;
  FormFileName: string;
  {$IF CompilerVersion >= 21.0} // 2010+
  {$ELSE}
  NotifierIdx: Integer;
  FWasFormModifiedAtLoading: Boolean;
  {$IFEND}
begin
  if FForm <> nil then
  begin
    { Do not recreate the form if the file hasn't changed. }
    {$WARNINGS OFF}
    if FileAge(FForm.GetFileName) = FForm.GetModifyTime then
      Exit;
    {$WARNINGS ON}
  end;

  if (FForm <> nil) and FSourceModule.CanFree then
  begin
    {$IF CompilerVersion >= 22.0} // XE+
    FReloadingFile := True;
    {$IFEND}
    {$IF CompilerVersion >= 21.0} // 2010+
    FWasFormModifiedAtLoading := False;
    {$ELSE}
    NotifierIdx := FNotifierIndex;
    FNotifierIndex := 0;
    {$IFEND}
    try
      if FSourceModule.IsDormant then
        ResurrectForm;
      FormWasTopmost := FFormIsTopmost;
      DesignerState := FForm.GetState;
      FormFileName := FForm.GetFileName;
      FSourceModule.ShowEditorName(FSourceModule.FileName, False);
      FForm.Close;
      FForm := nil;
      FForm := FDesigner.CreateRoot(Self as IDesignerModule, FormFileName,
        True, '', '', FSourceModule.FileSystem.GetIDString);
      if FormWasTopmost or not IsEmbeddedDesigner then
        if dsVisible in DesignerState then
          if dsIconic in DesignerState then
            FForm.ShowAs(ssMinimized)
          else if dsZoomed in DesignerState then
            FForm.ShowAs(ssMaximized)
          else FForm.ShowAs(ssNormal);
      if not FormWasTopMost then
        FSourceModule.Activate(False);
    finally
      {$IF CompilerVersion >= 22.0} // XE+
      FReloadingFile := False;
      {$IFEND}
      {$IF CompilerVersion >= 21.0} // 2010+
      {$ELSE}
      // make IDEFixPack patch working (we hooked it)
      FWasFormModifiedAtLoading := FNotifierIndex = -1;
      FNotifierIndex := NotifierIdx;
      {$IFEND}
    end;

    FIsFormModified := False;
    if FWasFormModifiedAtLoading then
    begin
      FIsFormModified := True;
      FSourceModule.Modified;
    end;
  end;
end;
*)

{$IF CompilerVersion >= 22.0} // XE+
procedure CheckFileDates(NoPrompt: Boolean);
{$ELSE}
procedure CheckFileDates;
{$IFEND}
var
  I: Integer;
  Modules: TList;
  Module: TDocModule;
  ReloadModules: TList;
  Form: TFormReloadFiles;
begin
  Modules := ModuleListP^;
  if (Modules = nil) or (Modules.Count = 0) then
    Exit;

  try
    ReloadModules := TList.Create;
    try
      for I := 0 to Modules.Count - 1 do
      begin
        Module := Modules[I];
        if Module.CheckFileDate then
          ReloadModules.Add(Module);
      end;

      if ReloadModules.Count > 0 then
      begin
        {$IF CompilerVersion >= 22.0} // XE+
        if not NoPrompt then
        {$IFEND}
        begin
          Form := TFormReloadFiles.Create(nil);
          try
            if not Form.ShowDialog(ReloadModules) then
              Exit;
          finally
            Form.Free;
          end;
        end;
        TFormReloadFiles.ReloadFiles(ReloadModules);
      end;
    finally
      ReloadModules.Free;
    end;
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(nil)
    else
      Application.HandleException(nil);
  end;
end;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
  begin
    OrgCallCheckFileDates := nil;
    if InitDocModuleHandler then
    begin
      @OrgCallCheckFileDates := RedirectOrgCall(@Docmodul_CheckFileDates, @CheckFileDates);
      //@OrgReloadFile := RedirectOrgCall(@TPascalCodeMgrModHandler.ReloadFile, @TPascalCodeMgrModHandlerEx.ReloadFile);
      {$IF CompilerVersion >= 23.0} // XE2+
      //@OrgDocModuleCanFree := RedirectOrgCall(@TDocModule.GetCanFree, @DocModuleCanFree);
      {$IFEND}
    end;
  end
  else
  begin
    RestoreOrgCall(@Docmodul_CheckFileDates, @OrgCallCheckFileDates);
    //RestoreOrgCall(@TPascalCodeMgrModHandler.ReloadFile, @OrgReloadFile);
    {$IF CompilerVersion >= 23.0} // XE2+
    //RestoreOrgCall(@TDocModule.GetCanFree, @OrgDocModuleCanFree);
    {$IFEND}
  end;
end;

{ TFormReloadFiles }

procedure TFormReloadFiles.mniDeselectAllClick(Sender: TObject);

  function IsChecked(Module: TDocModule; Checked: Boolean): Boolean;
  begin
    if Sender = mniDeselectAll then
      Result := False
    else if Sender = mniSelectAll then
      Result := True
    else if Sender = mniInvertSelection then
      Result := not Checked
    else if Sender = mniSelectOnlyUnmodifiedField then
      Result := not Module.IsModified
    else if Sender = mniSelectOnlyModifiedFiles then
      Result := Module.IsModified
    else
      Result := False;
  end;

var
  I: Integer;
begin
  ListViewModules.Items.BeginUpdate;
  try
    for I := 0 to ListViewModules.Items.Count - 1 do
      ListViewModules.Items[I].Checked := IsChecked(ListViewModules.Items[I].Data, ListViewModules.Items[I].Checked);
  finally
    ListViewModules.Items.EndUpdate;
  end;
end;

procedure TFormReloadFiles.mniShowInExplorerClick(Sender: TObject);
var
  FileName: string;
begin
  FileName := TDocModule(ListViewModules.Selected.Data).FileName;
  ShellExecute(Screen.ActiveCustomForm.Handle, 'open', 'explorer.exe', PChar(Format('/e, /select, "%s"', [FileName])), nil, SW_SHOWNORMAL);
end;

procedure TFormReloadFiles.DiffMenuItemClick(Sender: TObject);
begin
  if Sender is TFileEditorMenuItem then
    DiffEditor(TFileEditorMenuItem(Sender).Editor);
end;

procedure TFormReloadFiles.btnDiffClick(Sender: TObject);
var
  Msg: TMsg;
  Module: IOTAModule;
  Index: Integer;
begin
  // Don't let the user click on the button if it isn't possible (anymore)
  while PeekMessage(Msg, Handle, WM_LISTVIEWSELCHANGED, WM_LISTVIEWSELCHANGED, PM_REMOVE) do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;

  if btnDiff.Enabled and (ComboBoxDiff.ItemIndex <> -1) and (ListViewModules.SelCount = 1) then
  begin
    Index := Integer(ComboBoxDiff.Items.Objects[ComboBoxDiff.ItemIndex]);
    Module := nil;
    if Supports(TDocModule(ListViewModules.Selected.Data).GetCodeIDocModule, IOTAModule, Module) then
    begin
      if (Index >= 0) and (Index < Module.ModuleFileCount) then
        DiffEditor(Module.ModuleFileEditors[Index]);
    end;
  end;
end;

procedure TFormReloadFiles.DiffEditor(Editor: IOTAEditor);
const
  UTF8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
  ShareFlags: DWORD = FILE_SHARE_READ or FILE_SHARE_DELETE or FILE_SHARE_WRITE;

  function GetFileEncoding(const FileName: string): TEncoding;
  var
    BOM: TBytes;
    DiskFileHandle: THandle;
    BytesRead: DWORD;
  begin
    Result := TEncoding.Default;

    DiskFileHandle := CreateFile(PChar(FileName), GENERIC_READ, ShareFlags, nil, OPEN_EXISTING, 0, 0);
    if DiskFileHandle = INVALID_HANDLE_VALUE then
      RaiseLastOSError;
    try
      BytesRead := 0;
      SetLength(BOM, 5);
      ReadFile(DiskFileHandle, BOM[0], Length(BOM), BytesRead, nil);
      if BytesRead > 0 then
      begin
        SetLength(BOM, BytesRead);
        Result := nil; // otherwise GetBufferEncoding will use only this encoding
        TEncoding.GetBufferEncoding(BOM, Result);
      end;
    finally
      CloseHandle(DiskFileHandle);
    end;
  end;

var
  FormEditor: IOTAFormEditor;
  SourceEditor: IOTASourceEditor;
  FileStream: THandleStream;
  FileHandle: HFILE;
  TempFileName: string;
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  CmdLine: string;
  Item: TWaitItem;
  Encoding: TEncoding;
  Data: TBytes;
  WideData: UnicodeString;
  Utf8Data: UTF8String;
  FormStream: IStream;
begin
  TempFileName := '';
  try
    TempFileName := GetTempName(ExtractFileName(Editor.FileName));
    FileHandle := CreateFile(PChar(TempFileName), GENERIC_WRITE, ShareFlags,
        nil, CREATE_ALWAYS, FILE_ATTRIBUTE_TEMPORARY {or FILE_FLAG_DELETE_ON_CLOSE}, 0);
    try
      if FileHandle = INVALID_HANDLE_VALUE then
        RaiseLastOSError;
      // Write editor content to the temp file
      FileStream := THandleStream.Create(FileHandle);
      try
        if Supports(Editor, IOTAFormEditor, FormEditor) then
        begin
          FormStream := TStreamAdapter.Create(FileStream);
          FormEditor.GetFormResource(FormStream);
          FormStream := nil;
        end
        else if Supports(Editor, IOTASourceEditor, SourceEditor) then
        begin
          Encoding := GetFileEncoding(Editor.FileName);
          Utf8Data := GetEditorSource(SourceEditor);
          if Utf8Data <> '' then
          begin
            // Write with the encoding of the file on disk to allow the differ to work better (Beyond Compare doesn't have a problem but TSVN has)
            if Encoding = TEncoding.UTF8 then
            begin
              FileStream.Write(UTF8BOM, SizeOf(UTF8BOM));
              FileStream.Write(PAnsiChar(Utf8Data)^, Length(Utf8Data));
            end
            else
            begin
              WideData := UTF8ToString(Utf8Data);
              Utf8Data := '';
              Data := Encoding.GetPreamble;
              if Data <> nil then
                FileStream.Write(Data[0], Length(Data));
              Data := Encoding.GetBytes(WideData);
              WideData := ''; // release some memory
              if Data <> nil then
                FileStream.Write(Data[0], Length(Data));
            end;
          end;
        end;
      finally
        FileStream.Free;
      end;
    finally
      CloseHandle(FileHandle);
    end;

    CmdLine := FDiffer;
    CmdLine := StringReplace(CmdLine, '%basename', 'Editor', [rfReplaceAll, rfIgnoreCase]);
    CmdLine := StringReplace(CmdLine, '%minename', AnsiQuotedStr(ExtractFileName(Editor.FileName), '"'),[rfReplaceAll, rfIgnoreCase]);
    CmdLine := StringReplace(CmdLine, '%base', AnsiQuotedStr(TempFileName, '"'),[rfReplaceAll, rfIgnoreCase]);
    CmdLine := StringReplace(CmdLine, '%mine', AnsiQuotedStr(Editor.FileName, '"'),[rfReplaceAll, rfIgnoreCase]);

    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    StartupInfo.cb := SizeOf(StartupInfo);

    if CreateProcess(nil, PChar(CmdLine), nil, nil, False, CREATE_DEFAULT_ERROR_MODE, nil, nil, StartupInfo, ProcessInfo) then
    begin
      CloseHandle(ProcessInfo.hThread);

      Item := TWaitItem.Create(TempFileName, ProcessInfo.hProcess);
      if Item.Start then
        TempFileName := ''
      else
      begin
        CloseHandle(ProcessInfo.hProcess);
        Item.Free;
      end
    end
    else
      RaiseLastOSError;
  finally
    if TempFileName <> '' then
      DeleteFile(TempFileName);
  end;
end;

function ReadRegString(RootKey: HKEY; const Key: string; Value: string): string;
var
  Reg: TRegistry;
begin
  Result := '';
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := RootKey;
      if Reg.OpenKeyReadOnly(Key) then
      begin
        if Reg.ValueExists(Value) then
          Result := Reg.ReadString(Value);
        Reg.CloseKey;
      end;

      if Result = '' then
      begin
        Reg.RootKey := RootKey;
        Reg.Access := KEY_WOW64_RES;
        if Reg.OpenKeyReadOnly(Key) then
        begin
          if Reg.ValueExists(Value) then
            Result := Reg.ReadString(Value);
          Reg.CloseKey;
        end;
      end;
    finally
      Reg.Free;
    end;
  except
    Result := '';
  end;
end;

procedure TFormReloadFiles.FormCreate(Sender: TObject);
begin
  inherited;
  Caption := sReloadChangedFilesCaption;
  btnCancel.Caption := SCancelButton;
  btnOk.Caption := sReloadButton;

  mniSelectOnlyModifiedFiles.Caption := sReloadSelectOnlyUnmodifiedField;
  mniSelectOnlyUnmodifiedField.Caption := sReloadSelectOnlyModifiedFiles;
  mniSelectAll.Caption := sReloadSelectAll;
  mniDeselectAll.Caption := sReloadDeselectAll;
  mniInvertSelection.Caption := sReloadInvertSelection;
  mniShowInExplorer.Caption := sReloadShowInExplorer;


  ListViewModules.Columns[0].Caption := sLVColumn_File;
  ListViewModules.Columns[2].Caption := sLVColumn_Path;

  FListViewWindowProc := ListViewModules.WindowProc;
  ListViewModules.WindowProc := ListViewWndProc;

  if FDiffer = '' then
  begin
    // Beyond Compare
    FDiffer := ReadRegString(HKEY_LOCAL_MACHINE, 'Software\Scooter Software\Beyond Compare', 'ExePath');
    if (FDiffer = '') or not FileExists(FDiffer) then
    begin
      FDiffer := ReadRegString(HKEY_CURRENT_USER, 'Software\Scooter Software\Beyond Compare', 'ExePath');
      if (FDiffer = '') or not FileExists(FDiffer) then
        FDiffer := '';
    end;

    if FDiffer <> '' then
      FDiffer := '"' + FDiffer + '" %base %mine /title1=%basename /title2=%minename'
  end;

  if FDiffer = '' then
  begin
    // Beyond Compare Lite (BDS Edition)
    FDiffer := ExtractFilePath(ParamStr(0)) + 'BCompareLite.exe';
    if FileExists(FDiffer) then
      FDiffer := '"' + FDiffer + '" %base %mine /title1=%basename /title2=%minename'
    else
      FDiffer := '';
  end;

  if FDiffer = '' then
  begin
    // TortoiseSVN
    FDiffer := ReadRegString(HKEY_CURRENT_USER, 'Software\TortoiseSVN', 'Diff');
    if (FDiffer = '') or StartsStr('#', FDiffer) then
    begin
      FDiffer := ReadRegString(HKEY_LOCAL_MACHINE, 'Software\TortoiseSVN', 'TMergePath');
      if not FileExists(FDiffer) then
        FDiffer := '';
      if FDiffer <> '' then
        FDiffer := '"' + FDiffer + '" /base:%base /mine:%mine /basename:%basename /minename:%minename';
    end;
  end;

  if FDiffer = '' then
  begin
    // TortoiseGIT
    FDiffer := ReadRegString(HKEY_CURRENT_USER, 'Software\TortoiseGIT', 'Diff');
    if (FDiffer = '') or StartsStr('#', FDiffer) then
    begin
      FDiffer := ReadRegString(HKEY_LOCAL_MACHINE, 'Software\TortoiseGIT', 'TMergePath');
      if not FileExists(FDiffer) then
        FDiffer := '';
      if FDiffer <> '' then
        FDiffer := '"' + FDiffer + '" /base:%base /mine:%mine /basename:%basename /minename:%minename';
    end;
  end;

  if FDiffer = '' then
  begin
    // TortoiseHG
    FDiffer := ReadRegString(HKEY_CURRENT_USER, 'Software\TortoiseHg', '');
    if (FDiffer <> '') then
    begin
      if FileExists(FDiffer + '\kdiff3.exe') then
        FDiffer := '"' + FDiffer + '\kdiff3.exe" %base %mine'
      else
        FDiffer := '';
    end;
  end;


  btnDiff.Visible := FDiffer <> '';
  ComboBoxDiff.Visible := FDiffer <> '';
end;

procedure TFormReloadFiles.WMListViewSelChanged(var Msg: TMessage);
var
  I: Integer;
  Module: IOTAModule;
begin
  if ListViewModules.SelCount <> 1 then
  begin
    btnDiff.Enabled := False;
    ComboBoxDiff.Enabled := False;
    ComboBoxDiff.Items.Clear;
  end
  else
  begin
    ComboBoxDiff.Items.BeginUpdate;
    try
      ComboBoxDiff.Items.Clear;
      Module := nil;
      if Supports(TDocModule(ListViewModules.Selected.Data).GetCodeIDocModule, IOTAModule, Module) then
      begin
        for I := 0 to Module.ModuleFileCount - 1 do
          if Supports(Module.ModuleFileEditors[I], IOTASourceEditor) or Supports(Module.ModuleFileEditors[I], IOTAFormEditor) then
            ComboBoxDiff.Items.AddObject(ExtractFileName(Module.ModuleFileEditors[I].FileName), TObject(I));
      end;
    finally
      ComboBoxDiff.Items.EndUpdate;
      if ComboBoxDiff.Items.Count > 0 then
        ComboBoxDiff.ItemIndex := 0;
    end;
    btnDiff.Enabled := True;
    ComboBoxDiff.Enabled := True;
  end;
end;

procedure TFormReloadFiles.ListViewModulesCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not (cdsSelected in State) and (FCannotReloadModules.IndexOf(Item.Data) <> -1) then
    Sender.Canvas.Font.Color := clGrayText;
  DefaultDraw := True;
end;

procedure TFormReloadFiles.ListViewModulesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Msg: TMsg;
begin
  if FDiffer <> '' then
  begin
    PeekMessage(Msg, Handle, WM_LISTVIEWSELCHANGED, WM_LISTVIEWSELCHANGED, PM_REMOVE or PM_NOYIELD);
    PostMessage(Handle, WM_LISTVIEWSELCHANGED, 0, 0);
  end;
end;

procedure TFormReloadFiles.ListViewWndProc(var Msg: TMessage);
var
  HitTestInfo: TLVHitTestInfo;
begin
  FListViewWindowProc(Msg);
  if Msg.Msg = CN_NOTIFY then
  begin
    case TWMNotify(Msg).NMHdr^.code of
      NM_DBLCLK:
        begin
          HitTestInfo.iItem := PNMListView(TWMNotify(Msg).NMHdr)^.iItem;
          if HitTestInfo.iItem = -1 then
          begin
            HitTestInfo.pt := PNMListView(TWMNotify(Msg).NMHdr)^.ptAction;
            ListView_SubItemHitTest(Handle, @HitTestInfo);
          end;
          if HitTestInfo.iItem <> -1 then
            ListItemDblClick(ListViewModules.Items[HitTestInfo.iItem]);
        end;
    end;
  end;
end;

procedure TFormReloadFiles.ListItemDblClick(Item: TListItem);
var
  Pt: TPoint;
  Module: IOTAModule;
begin
  if (FDiffer <> '') and (Item <> nil) then
  begin
    if Supports(TDocModule(Item.Data).GetCodeIDocModule, IOTAModule, Module) then
    begin
      if Module.ModuleFileCount = 1 then
        DiffEditor(Module.ModuleFileEditors[0])
      else
      begin
        if GetCursorPos(Pt) then
        begin
          AddDiffPopupItems(TDocModule(Item.Data), PopupMenuDiff);
          PopupMenuDiff.Popup(Pt.X, Pt.Y);
        end;
      end;
    end;
  end;
end;

procedure TFormReloadFiles.AddDiffPopupItems(ADocModule: TDocModule; APopupMenu: TPopupMenu);
var
  Module: IOTAModule;
  I, MenuIndex: Integer;
  MenuItem: TFileEditorMenuItem;
  WasEmpty: Boolean;
begin
  if APopupMenu <> nil then
  begin
    // clean up menu items from last call
    for I := APopupMenu.Items.Count - 1 downto 0 do
      if APopupMenu.Items[I] is TFileEditorMenuItem then
        APopupMenu.Items[I].Free;

    WasEmpty := APopupMenu.Items.Count = 0;
    if (FDiffer <> '') and (ADocModule <> nil) then
    begin
      if Supports(ADocModule.GetCodeIDocModule, IOTAModule, Module) then
      begin
        MenuIndex := 0;
        for I := 0 to Module.ModuleFileCount - 1 do
        begin
          if Supports(Module.ModuleFileEditors[I], IOTASourceEditor) or Supports(Module.ModuleFileEditors[I], IOTAFormEditor) then
          begin
            MenuItem := TFileEditorMenuItem.Create(APopupMenu);
            MenuItem.Editor := Module.ModuleFileEditors[I];
            MenuItem.Caption := 'Diff ' + ExtractFileName(Module.ModuleFileEditors[I].FileName);
            MenuItem.OnClick := DiffMenuItemClick;
            APopupMenu.Items.Insert(MenuIndex, MenuItem);
            Inc(MenuIndex);
          end;
        end;

        if (MenuIndex > 0) and not WasEmpty then
        begin
          MenuItem := TFileEditorMenuItem.Create(APopupMenu);
          MenuItem.Caption := '-';
          APopupMenu.Items.Insert(MenuIndex, MenuItem);
        end;
      end;
    end;
  end;
end;

procedure TFormReloadFiles.PopupMenuModulesPopup(Sender: TObject);
begin
  inherited;

  mniShowInExplorer.Enabled := ListViewModules.SelCount = 1;

  if (FDiffer <> '') and (ListViewModules.SelCount = 1) then
    AddDiffPopupItems(ListViewModules.Selected.Data, PopupMenuModules)
  else
    AddDiffPopupItems(nil, PopupMenuModules);
end;

{function DormantModuleForm(Module: TDocModule; out ErrorMsg: string): Boolean;
begin
  ErrorMsg := '';
  try
    Result := Module.GoDormant;
    if not Result then
      ErrorMsg := Format(sCannotUnloadModuleForm, [Module.GetModuleName]);
  except
    on E: Exception do
    begin
      ErrorMsg := E.Message;
      Result := False;
    end;
  end;
end;

function ResurrectModuleForm(Module: TDocModule; out ErrorMsg: string): Boolean;
begin
  ErrorMsg := '';
  try
    Module.ShowEditor(False);
    Result := True;
  except
    on E: Exception do
    begin
      ErrorMsg := E.Message;
      Result := False;
    end;
  end;
end;}


class procedure TFormReloadFiles.ReloadFiles(Modules: TList);
var
  I: Integer;
  //AcceptedModules, DeniedModules, SimpleModules: TList;
  FailedModules: TStrings;
  //ClosedFiles: TStrings;
  Module: TDocModule;
  E: Exception;
  ErrorMsg: string;
  //DepList: TList;
  //ModuleIntf: IOTAModule;

{  procedure CollectDependentModules(Module: TDocModule);
  var
    List: TList;
    I: Integer;
  begin
    if SimpleModules.IndexOf(Module) <> -1 then
      Exit;

    List := TList.Create;
    try
      Module.GetDependentModules(List);
      if not Module.CanFree then
      begin
        DeniedModules.Add(Module);
        for I := 0 to List.Count - 1 do
        begin
          if (AcceptedModules.IndexOf(List[I]) = -1) and (DeniedModules.IndexOf(List[I]) = -1) then
            CollectDependentModules(List[I]);
        end;
      end
      else
        AcceptedModules.Add(Module);
    finally
      List.Free;
    end;
  end;}

{var
  ModuleServices: IOTAModuleServices;}
begin
  ErrorMsg := '';

  //ModuleServices := BorlandIDEServices as IOTAModuleServices;

  //SimpleModules := TList.Create;
  FailedModules := TStringList.Create;
  //AcceptedModules := TList.Create;
  //DeniedModules := TList.Create;
  //ClosedFiles := TStringList.Create;
  //ReloadingModules := True;
  try
    // Build the inverse list of dependent modules
    for I := Modules.Count - 1 downto 0 do
    begin
      Module := Modules[I];

      //if not Module.HasForm and Module.CanFree then
      begin
        //SimpleModules.Add(Module);
        try
          //Modules.Remove(Module); // handled
          if Module.CanReloadFile then
            Module.ReloadFile;
        except
          on E: Exception do
            FailedModules.AddObject(E.Message, Module);
        end;
      end;
    end;

{    for I := 0 to Modules.Count - 1 do
      CollectDependentModules(Modules[I]);

    try
      // Reload all Form-Modules that have no dependencies
      DepList := TList.Create;
      try
        for I := AcceptedModules.Count - 1 downto 0 do
        begin
          Module := AcceptedModules[I];
          if Module.CanReloadFile then
          begin
            DepList.Clear;
            Module.GetModuleDependencies(DepList);
            if DepList.Count = 0 then
            begin
              try
                AcceptedModules.Remove(Module); // handled
                Module.ReloadFile;
              except
                on E: Exception do
                  FailedModules.AddObject(E.Message, Module);
              end;
            end;
          end;
        end;
      finally
        DepList.Free;
      end;

      I := 0;
      while I < DeniedModules.Count do
      begin
        Module := DeniedModules[I];
        if Supports(Module, IOTAModule, ModuleIntf) then
        begin
          try
            ModuleIntf.CloseModule(True);
            ClosedFiles.Add(Module.FileName);
            ModuleIntf := nil;
            DeniedModules.Delete(I);
            Dec(I);
          except
            on E: Exception do
              FailedModules.AddObject(E.Message, Module);
          end;
        end;
        Inc(I);
      end;

      for I := 0 to AcceptedModules.Count - 1 do
      begin
        Module := AcceptedModules[I];
        try
          Module.ReloadFile;
        except
          on E: Exception do
            FailedModules.AddObject(E.Message, Module);
        end;
      end;

      for I := 0 to DeniedModules.Count - 1 do
      begin
        Module := DeniedModules[I];
        try
          Module.SwapSourceFormView;
          try
            Module.ReloadFile;
          finally
            Module.SwapSourceFormView;
          end;
        except
          on E: Exception do
            FailedModules.AddObject(E.Message, Module);
        end;
      end;

    finally
//      for I := 0 to DeniedModules.Count - 1 do
//        FailedModules.AddObject(Format(sCannotUnloadModuleForm, [TDocModule(DeniedModules[I]).GetModuleName]), DeniedModules[I]);

      // reopen all closed files in reverse order
      for I := ClosedFiles.Count - 1 downto 0 do
      begin
        try
          ModuleIntf := ModuleServices.OpenModule(ClosedFiles[I]);
          if ModuleIntf <> nil then
            ModuleIntf.Show;
        except
          on E: Exception do
            FailedModules.Add(E.Message + ' (' + ClosedFiles[I] + ')');
        end;
      end;
    end;}
  finally
    //ReloadingModules := False;
    ErrorMsg := FailedModules.Text;

    //DeniedModules.Free;
    //AcceptedModules.Free;
    FailedModules.Free;
    //SimpleModules.Free;

    if ErrorMsg <> '' then
    begin
      E := Exception.Create(Trim(ErrorMsg));
      try
        ApplicationShowException(E);
      finally
        E.Free;
      end;
    end;
  end;
end;

procedure TFormReloadFiles.AddModuleListItem(AModule: TDocModule);
var
  ListItem: TListItem;
  FileName: string;
  Ext: string;
  CanReloadPossible: Boolean;
begin
  CanReloadPossible := True;
  try
    AModule.CanReloadFile;
  except
    FCannotReloadModules.Add(AModule);
    CanReloadPossible := False;
  end;

  FileName := AModule.GetFileName;

  ListItem := ListViewModules.Items.Add;
  ListItem.Data := AModule;
  ListItem.Caption := ExtractFileName(FileName);
  ListItem.SubItems.Add('');
  ListItem.SubItems.Add(ExtractFileDir(FileName));

  if AModule.HasForm then
  begin
    ListItem.SubItems[0] := AModule.GetFormName;
    if FFormGroup <> nil then
      ListItem.GroupID := FFormGroup.GroupID;
  end
  else if FUnitGroup <> nil then
  begin
    Ext := ExtractFileExt(FileName);
    if (FProjectGroup <> nil) and (AnsiIndexText(Ext, ['.groupproj', '.dproj', '.cbproj', '.dpr', '.dpk', '.bpr', '.bpk']) <> -1) then
      ListItem.GroupID := FProjectGroup.GroupID
    else
      ListItem.GroupID := FUnitGroup.GroupID;
  end;

  if AModule.IsModified then
    ListItem.ImageIndex := 0
  else
    ListItem.ImageIndex := -1;

  ListItem.Checked := CanReloadPossible;
end;

procedure TFormReloadFiles.AddModules(AModules: TList);
var
  I: Integer;
  Module: TDocModule;
begin
  ListViewModules.Items.BeginUpdate;
  try
    for I := 0 to AModules.Count - 1 do
    begin
      Module := AModules[I];
      if (AModules = FModules) or (FModules.IndexOf(Module) = -1) then
      begin
        FModules.Add(Module);
        AddModuleListItem(Module);
      end;
    end;
    ListViewModules.AlphaSort;
  finally
    ListViewModules.Items.EndUpdate;
  end;
end;

function TFormReloadFiles.ShowDialog(AModules: TList): Boolean;
var
  I: Integer;
begin
  if ReloadFileForm <> nil then
  begin
    // Reentrance: Add the additional modules to listview of the previous dialog
    ReloadFileForm.AddModules(AModules);
    Result := False;
    Exit;
  end;

  ReloadFileForm := Self;
  FCannotReloadModules := TList.Create;
  try
    FModules := AModules;
    FProjectGroup := nil;
    FUnitGroup := nil;
    FFormGroup := nil;
    if CheckWin32Version(5, 1) then
    begin
      ListViewModules.GroupView := True;
      ListViewModules.Groups.Clear;
      FProjectGroup := ListViewModules.Groups.Add;
      FProjectGroup.Header := sLVGroup_ProjectFiles;
      FUnitGroup := ListViewModules.Groups.Add;
      FUnitGroup.Header := sLVGroup_UnitFiles;
      FFormGroup := ListViewModules.Groups.Add;
      FFormGroup.Header := sLVGroup_Forms;
    end;

    ListViewModules.Items.BeginUpdate;
    try
      ListViewModules.Items.Clear;
      for I := 0 to AModules.Count - 1 do
        AddModuleListItem(AModules[I]);
      ListViewModules.AlphaSort;
    finally
      ListViewModules.Items.EndUpdate;
    end;

    SendMessage(Handle, WM_LISTVIEWSELCHANGED, 0, 0);

    Result := ShowModal = mrOk;

    if Result then
    begin
      for I := 0 to ListViewModules.Items.Count - 1 do
        if not ListViewModules.Items[I].Checked then
          AModules.Remove(ListViewModules.Items[I].Data);
    end;
  finally
    FCannotReloadModules.Free;
    ReloadFileForm := nil;
  end;
end;

{ TWaitItem }

constructor TWaitItem.Create(const AFileName: string; AProcessHandle: THandle);
begin
  inherited Create;
  FFileName := AFileName;
  FProcessHandle := AProcessHandle;

  if TWaitItem.WndHandle = 0 then
    TWaitItem.WndHandle := AllocateHWnd(TWaitItem.WndProc);
end;

destructor TWaitItem.Destroy;
begin
  CloseHandle(FProcessHandle);
  DeleteFile(FFileName);
  PostMessage(TWaitItem.WndHandle, WM_USER + 1, 0, FWaitHandle);
  inherited Destroy;
end;

procedure WaitCallback(Context: Pointer; Success: Boolean); stdcall;
begin
  TWaitItem(Context).Free;
end;

function TWaitItem.Start: Boolean;
begin
  Result := RegisterWaitForSingleObject(FWaitHandle, FProcessHandle, @WaitCallback, Self, INFINITE,
    WT_EXECUTEONLYONCE);
end;

class procedure TWaitItem.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = WM_USER + 1 then
  begin
    UnregisterWait(THandle(Msg.LParam));
    Msg.Result := 0;
  end
  else
    Msg.Result := DefWindowProc(WndHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

initialization

finalization
  DeallocateHWnd(TWaitItem.WndHandle);

end.
