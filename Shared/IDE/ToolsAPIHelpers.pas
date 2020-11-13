{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit ToolsAPIHelpers;

interface

uses
  Windows, SysUtils, Classes, Contnrs, ToolsAPI, Controls, ActnList,
  CategoryButtons, PaletteAPI,
  {$IF CompilerVersion >= 23.0} // XE2+
  PlatformAPI,
  {$IFEND}
  ExtCtrls, ComCtrls, Forms, ImgList, IDEHooks, TypInfo, Menus;

type
  TFormResourceType = (ftForm, ftFrame, ftDataModule);

function FindMenuItem(const Name: string): TMenuItem;
procedure ShowMenuItem(const Name: string);
  
function GetActiveProjectGroup: IOTAProjectGroup;
function GetActiveProject: IOTAProject;
function FindModuleInfo(Project: IOTAProject; const Filename: string): IOTAModuleInfo;

function GetEditorSource(Editor: IOTASourceEditor): UTF8String; overload;
function GetEditorSource(EditBuffer: IOTAEditBuffer): UTF8String; overload;
procedure SaveEditorSourceTo(EditBuffer: IOTAEditBuffer; const Filename: string); overload; // UTF8 BOM is written for Delphi 2005+
procedure SaveEditorSourceTo(EditBuffer: IOTAEditBuffer; OutStream: TStream); overload; // UTF8 BOM is written for Delphi 2005+
procedure SaveFormResourceTo(FormEditor: IOTAFormEditor; const Filename: string); overload;
procedure SaveFormResourceTo(FormEditor: IOTAFormEditor; OutStream: TStream); overload;

function GetFormResourceType(const Filename, FormName: string; out FormCaption: string): TFormResourceType;
function GetUnitNameFromFile(const FileName: string): string;

function NewToolBar(AOwner: TComponent; const Name, Caption: string; Visible: Boolean = True): TToolBar;

function IsCppPersonality(Project: IOTAProject): Boolean;
function IsDelphiPersonality(Project: IOTAProject): Boolean;
function IsDelphiNetPersonality(Project: IOTAProject): Boolean;


function GetComponentPalette: TCategoryButtons;
function GetPaletteCategories(Palette: TCategoryButtons): TButtonCategories;

function GetCurrentEditor: IOTAEditor;
procedure SelectComponentPalette(PalItem: TButtonItem; ExecuteItem: Boolean);

function GetProjectEnvOptionPaths(const AProject: IOTAProject; const AOptionName: string): string;
function GetPlatformEnvOptionPaths(const APlatformName, APersonality, AOptionName: string): string;

type
  TPropField = class(TComponent)
  private
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
  public
    property Value: Variant read GetValue write SetValue;
  end;

implementation

uses
  Messages, IDEUtils, DelphiLexer;

{ TPropField }

function TPropField.GetValue: Variant;
  external vclide_bpl name '@Idepropset@TPropField@GetValue$qqrv';

procedure TPropField.SetValue(const Value: Variant);
  external vclide_bpl name '@Idepropset@TPropField@SetValue$qqrrx14System@Variant';

{------------------------------------------------------------------------------------------------------------}

function FindMenuItem(const Name: string): TMenuItem;
var
  Comp: TComponent;
begin
  Comp := Application.MainForm.FindComponent(Name);
  if (Comp <> nil) and (Comp is TMenuItem) then
    Result := TMenuItem(Comp)
  else
    Result := nil;
end;

procedure ShowMenuItem(const Name: string);
var
  Item: TMenuItem;
begin
  // Make a hidden menu item visible
  Item := FindMenuItem(Name);
  if Item <> nil then
  begin
    Item.Visible := True;
    if Item.Action <> nil then
      TAction(Item.Action).Visible := True;
  end;
end;

function GetActiveProjectGroup: IOTAProjectGroup;
var
  ModuleServices: IOTAModuleServices;
  i: Integer;
begin
  Result := nil;
  if Assigned(BorlandIDEServices) then
  begin
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    for i := 0 to ModuleServices.ModuleCount - 1 do
      if SysUtils.Supports(ModuleServices.Modules[i], IOTAProjectGroup, Result) then
        Break;
  end;
end;

function GetCustomCodeIProject(Project: IOTAProject): TObject;
begin
  Result := DelphiInterfaceToObject(Project);
end;

function GetActiveProject: IOTAProject;
begin
  Result := ToolsAPI.GetActiveProject;
end;

function FindModuleInfo(Project: IOTAProject; const Filename: string): IOTAModuleInfo;
begin
  Result := Project.FindModuleInfo(Filename);
end;

function GetEditorSource(Editor: IOTASourceEditor): UTF8String;
const
  MaxBufSize = 15872; // The C++ compiler uses this number of bytes for read operations and the IDE requires this
var
  Readn, Len: Integer;
  Buf: array[0..MaxBufSize] of AnsiChar;
  Reader: IOTAEditReader;
begin
  Result := '';
  if Editor <> nil then
  begin
    Reader := Editor.CreateReader;
    repeat
      Readn := Reader.GetText(Length(Result), Buf, MaxBufSize);
      Buf[Readn] := #0;
      if Readn > 0 then
      begin
        Len := Length(Result);
        SetLength(Result, Len + Readn);
        Move(Buf[0], Result[Len + 1], Readn);
      end;
      //Result := Result + Buf;
    until Readn < MaxBufSize;
  end;
end;

function GetEditorSource(EditBuffer: IOTAEditBuffer): UTF8String;
const
  MaxBufSize = 15872; // The C++ compiler uses this number of bytes for read operations and the IDE requires this
var
  Readn, Len: Integer;
  Buf: array[0..MaxBufSize] of AnsiChar;
  Reader: IOTAEditReader;
begin
  Result := '';
  if EditBuffer <> nil then
  begin
    Reader := EditBuffer.CreateReader;
    repeat
      Readn := Reader.GetText(Length(Result), Buf, MaxBufSize);
      Buf[Readn] := #0;
      if Readn > 0 then
      begin
        Len := Length(Result);
        SetLength(Result, Len + Readn);
        Move(Buf[0], Result[Len + 1], Readn);
      end;
      //Result := Result + Buf;
    until Readn < MaxBufSize;
  end;
end;

procedure SaveEditorSourceTo(EditBuffer: IOTAEditBuffer; const Filename: string);
  // UTF8 BOM is written for Delphi 2005+
var
  FileStream: TFileStream;
begin
  if EditBuffer <> nil then
  begin
    FileStream := TFileStream.Create(Filename, fmCreate);
    try
      SaveEditorSourceTo(EditBuffer, FileStream);
    finally
      FileStream.Free;
    end;
  end;
end;

procedure SaveEditorSourceTo(EditBuffer: IOTAEditBuffer; OutStream: TStream);
  // UTF8 BOM is written for Delphi 2005+
const
  Utf8BOM: array[0..2] of Byte = ($EF, $BB, $BF);
var
  Data: UTF8String;
begin
  if OutStream <> nil then
  begin
    Data := GetEditorSource(EditBuffer);
    if Data <> '' then
    begin
      OutStream.Write(Utf8BOM, Length(Utf8BOM));
      OutStream.Write(Data[1], Length(Data));
    end;
  end;
end;

procedure SaveFormResourceTo(FormEditor: IOTAFormEditor; const Filename: string);
var
  FileStream: TFileStream;
begin
  if FormEditor <> nil then
  begin
    FileStream := TFileStream.Create(Filename, fmCreate);
    try
      SaveFormResourceTo(FormEditor, FileStream);
    finally
      FileStream.Free;
    end;
  end;
end;

procedure SaveFormResourceTo(FormEditor: IOTAFormEditor; OutStream: TStream);
begin
  if (FormEditor <> nil) and (OutStream <> nil) then
    FormEditor.GetFormResource(TStreamAdapter.Create(OutStream));
end;

function GetFormResourceType(const Filename, FormName: string; out FormCaption: string): TFormResourceType;
var
  f: TextFile;
  S: string;
  OpenFilename: string;
  SearchS: string;
  i, k, l: Integer;
  Modules: IOTAModuleServices;
  Editor: IOTAEditBuffer;
  Lines: TStrings;
  Buffer: array[0..8*1024 - 1] of Byte;

  function CheckLine(const S: string; var Typ: TFormResourceType): Boolean;
  var
    ps: Integer;
  begin
    Result := False;
    ps := Pos(SearchS + 'TDataModule', S);
    if ps > 0 then
    begin
      Typ := ftDataModule;
      Result := True;
    end;
    ps := Pos(SearchS + 'TFrame', S);
    if ps > 0 then
    begin
      Typ := ftFrame;
      Result := True;
    end;
    if CompareText(Trim(S), 'implementation') = 0 then
      Result := True;
  end;

begin
  Result := ftForm;
  OpenFilename := Filename;

  if CompareText(ExtractFileExt(Filename), '.pas') = 0 then
    SearchS := 'T' + FormName + ' = class('
  else
  if CompareText(ExtractFileExt(Filename), '.cpp') = 0 then
  begin
    OpenFilename := ChangeFileExt(Filename, '.h');
    SearchS := 'class T' + FormName + ' : public ';
  end;

  try
    if Supports(BorlandIDEServices, IOTAModuleServices, Modules) then
    begin
      for i := 0 to Modules.ModuleCount - 1 do
      begin
        if AnsiCompareFileName(Modules.Modules[i].FileName, Filename) = 0 then
        begin
          for k := 0 to Modules.Modules[i].GetModuleFileCount - 1 do
          begin
            if (AnsiCompareText(Modules.Modules[i].GetModuleFileEditor(k).FileName, OpenFilename) = 0) and
                Supports(Modules.Modules[i].GetModuleFileEditor(k), IOTAEditBuffer, Editor) then
            begin
              Lines := TStringList.Create;
              try
                Lines.Text := Utf8ToAnsi(GetEditorSource(Editor));
                for l := 0 to Lines.Count - 1 do
                  if CheckLine(Lines[l], Result) then
                    Break;
              finally
                Lines.Free;
              end;
              Exit;
            end;
          end;
          Exit;
        end;
      end;
    end;

    { The file is not opened in the editor, load it from disk. }
    AssignFile(f, Filename);
    {$I-}
    IOResult; // reset
    SetTextBuf(f, Buffer, SizeOf(Buffer));
    Reset(f);
    if IOResult = 0 then
    try
      while not Eof(f) do
      begin
        ReadLn(f, S);
        if CheckLine(S, Result) then
          Break;
      end;
    finally
      IOResult;
      CloseFile(f);
    end;
    IOResult; // reset
    {$I+}
  except
  end;
end;

function GetUnitNameFromFile(const FileName: string): string;
var
  Lexer: TDelphiLexer;

  function Next: TToken;
  begin
    while Lexer.GetToken(Result) and (Result.Kind in [tkComment, tkDirective]) do
      ;
  end;

  function NextA: TToken;
  begin
    Result := Next;
    if Result = nil then
      Abort;
  end;

  function ParseQualifiedName(Look: TToken): string;
  begin
    Result := '';
    repeat
      if Look.Kind >= tkIdent then
        Result := Result + Look.Value
      else
        Break;
      Look := NextA;
      if Look.Kind <> tkDot then
        Break;
      Result := Result + '.';
      Look := NextA;
    until False;
  end;

var
  Token: TToken;
begin
  Result := ChangeFileExt(ExtractFileName(FileName), '');
  if SameText(ExtractFileExt(FileName), '.pas') then
  begin
    try
      Lexer := TDelphiLexer.Create(FileName, LoadTextFileToUtf8String(FileName, 4096));
      try
        Token := Next;
        if (Token <> nil) and (Token.Kind = tkI_unit) then
        begin
          Token := Next;
          if Token <> nil then
            Result := ParseQualifiedName(Token);
        end;
      finally
        Lexer.Free;
      end;
    except
      on E: EAbort do
        ;
    end;
  end;
end;

function NewToolBar(AOwner: TComponent; const Name, Caption: string; Visible: Boolean): TToolBar;
type
  TToolbarClass = class of TToolBar;
var
  ControlBar: TControlBar;
  ToolBarClass: TToolbarClass;
  Services: INTAServices;
begin
  ControlBar := TControlBar(Application.MainForm.FindComponent('ControlBar1'));
  if ControlBar <> nil then
  begin
    Supports(BorlandIDEServices, INTAServices, Services);

    ToolBarClass := TToolBarClass(Services.GetToolBar(sStandardToolBar).ClassType);
    if ToolBarClass = nil then
      ToolBarClass := TToolBar;

    Result := ToolBarClass.Create(Application.MainForm); // BDS: inserts the toolbar into the popupmenu and the dialog
    Result.Name := Name;
    Result.Caption := Caption;
    Result.EdgeBorders := [];
    Result.Left := MaxInt;
    Result.Flat := True;
    Result.AutoSize := True;
    Result.Visible := Visible;
    Result.Parent := ControlBar;
    Result.Images := Services.ImageList;
  end
  else
    raise Exception.Create('No ControlBar was found');
end;

function GetComponentPalette: TCategoryButtons;
var
  ToolForm: TForm;
begin
  ToolForm := TForm(Application.FindComponent('ToolForm'));
  if ToolForm <> nil then
    Result := TCategoryButtons(ToolForm.FindComponent('Palette'))
  else
    Result := nil;
end;

function FindPaletteItemByName(const APaletteName, AItemName: string): IOTABasePaletteItem;
var
  CompPal: TCategoryButtons;
  PaletteServices: IOTAPaletteServices;
  PalGroup: IOTAPaletteGroup;
begin
  CompPal := GetComponentPalette;
  if Assigned(CompPal) and Supports(BorlandIDEServices, IOTAPaletteServices, PaletteServices) then
  begin
    PalGroup := PaletteServices.BaseGroup.FindItemByName(APaletteName, False) as IOTAPaletteGroup;
    if PalGroup <> nil then
    begin
      Result := PalGroup.FindItemByName(AItemName, False);
      Exit;
    end;
  end;
  Result := nil;
end;

function FindPaletteItemById(const APaletteId, AItemId: string): IOTABasePaletteItem;
var
  CompPal: TCategoryButtons;
  PaletteServices: IOTAPaletteServices;
  PalGroup: IOTAPaletteGroup;
begin
  CompPal := GetComponentPalette;
  if Assigned(CompPal) and Supports(BorlandIDEServices, IOTAPaletteServices, PaletteServices) then
  begin
    PalGroup := PaletteServices.BaseGroup.FindItem(APaletteId, False) as IOTAPaletteGroup;
    if PalGroup <> nil then
    begin
      Result := PalGroup.FindItem(AItemId, False);
      Exit;
    end;
  end;
  Result := nil;
end;

function GetCurrentEditor: IOTAEditor;
var
  Module: IOTAModule;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Module <> nil then
    Result := Module.CurrentEditor
  else
    Result := nil;
end;

{--------------------------------------------------------------------------------------------------}

function GetPaletteCategories(Palette: TCategoryButtons): TButtonCategories;
begin
  Result := TButtonCategories(GetObjectProp(Palette, 'Categories', TCollection));
end;

type
  TOpenCategoryButtons = class(TCategoryButtons);

procedure SelectComponentPalette(PalItem: TButtonItem; ExecuteItem: Boolean);
var
  CurrentEditor: IOTAEditor;
  Palette: TCategoryButtons;
  SelectorButton: TToolButton;
begin
  Palette := GetComponentPalette;
  if Palette <> nil then
  begin
    if PalItem <> nil then
    begin
      Palette.SelectedItem := PalItem;
      Palette.FocusedItem := PalItem;
      TOpenCategoryButtons(Palette).DoItemClicked(PalItem);
      if ExecuteItem then
      begin
        Palette.SelectedItem := nil;

        // activate the editor
        CurrentEditor := GetCurrentEditor;
        if CurrentEditor <> nil then
          CurrentEditor.Show; // does not focus the form when in docked form designer mode
      end;
    end
    else
    begin
      SelectorButton := TToolButton(Palette.Owner.FindComponent('tbSelector'));
      if SelectorButton <> nil then
        SelectorButton.Click
      else
      begin
        Palette.SelectedItem := nil;
        Palette.FocusedItem := nil;
      end;
    end;
  end;
end;

function IsCppPersonality(Project: IOTAProject): Boolean;
begin
  if Project = nil then
    Project := GetActiveProject;
  Result := (Project <> nil) and (Project.Personality = sCBuilderPersonality);
end;

function IsDelphiPersonality(Project: IOTAProject): Boolean; // Win32
begin
  if Project = nil then
    Project := GetActiveProject;
  Result := (Project <> nil) and (Project.Personality = sDelphiPersonality);
end;

function IsDelphiNetPersonality(Project: IOTAProject): Boolean;
begin
  if Project = nil then
    Project := GetActiveProject;
  Result := (Project <> nil) and (Project.Personality = sDelphiDotNetPersonality);
end;

{$IF CompilerVersion >= 23.0} // XE2+
type
  PObject = ^TObject;

  TPlatformManager = class(TObject)
  public
    function GetEnvOptions(const APlatform, APersonality: string): PObject;
  end;

function PlatformManager: TPlatformManager;
  external coreide_bpl name '@Platforms@PlatformManager$qqrv';
function TPlatformManager.GetEnvOptions(const APlatform: string; const APersonality: string): PObject;
  external coreide_bpl name '@Platforms@TPlatformManager@GetEnvOptions$qqrx20System@UnicodeStringt1';
{$IFEND}

function GetProjectEnvOptionPaths(const AProject: IOTAProject; const AOptionName: string): string;
var
  PlatformName: string;
  Personality: string;
begin
  {$IF CompilerVersion >= 23.0} // XE2+
  if AProject = nil then
  begin
    PlatformName := cWin32Platform;
    Personality := sDelphiPersonality;
  end
  else
  begin
    PlatformName := AProject.CurrentPlatform;
    Personality := AProject.Personality;
  end;
  {$ELSE}
  PlatformName := '';
  Personality := '';
  {$IFEND}
  Result := GetPlatformEnvOptionPaths(PlatformName, Personality, AOptionName);
end;

function GetPlatformEnvOptionPaths(const APlatformName, APersonality, AOptionName: string): string;
var
  EnvOptions: IOTAEnvironmentOptions;
  {$IF CompilerVersion >= 23.0} // XE2+
  PlatformEnvOptionsPtr: PObject;
  PropFieldPtr: ^TPropField;
  {$IFEND}
begin
  {$IF CompilerVersion >= 23.0} // XE2+
  if APlatformName <> '' then
  begin
    PlatformEnvOptionsPtr := PlatformManager.GetEnvOptions(APlatformName, APersonality);
    if (PlatformEnvOptionsPtr <> nil) and (PlatformEnvOptionsPtr^ <> nil) and
       InheritsFromClassName(PlatformEnvOptionsPtr^, 'TAddInOptions') then
    begin
      PropFieldPtr := PlatformEnvOptionsPtr^.FieldAddress(AOptionName);
      if (PropFieldPtr <> nil) and (PropFieldPtr^ <> nil) and InheritsFromClassName(PropFieldPtr^, 'TPropField') then
      begin
        Result := VarToStrDef(PropFieldPtr^.Value, '');
        Exit;
      end;
    end;
  end;
  {$IFEND}
  EnvOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
  Result := VarToStrDef(EnvOptions.Values[AOptionName], '');
end;

end.

