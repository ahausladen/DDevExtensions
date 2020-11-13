{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2009 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageKeybindings;

{$I ..\DelphiExtension.inc}
{.$O-}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FrmTreePages, ToolsAPI, PluginConfig, SimpleXmlIntf, Menus,
  ActnList, FrmeBase, ExtCtrls;

type
  TKeybindings = class(TPluginConfig, IOTAKeyboardBinding)
  private
    FNotifierIndex: Integer;
    FActive: Boolean;
    FTabIndent: Boolean;
    FIndentSingleLine: Boolean;
    FExtendedHome: Boolean;
    FSwitchedExtendedHome: Boolean;
    FExtendedCtrlLeftRight: Boolean;
    {$IF CompilerVersion <= 20.0}
    FShiftF3: Boolean;
    {$IFEND}
    FMoveLineBlock: Boolean;
    FFindDeclOnCaret: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure DoKeyBinding(const Context: IOTAKeyContext; KeyCode: TShortcut;
      var BindingResult: TKeyBindingResult);
    procedure InternCtrlMoveCursor(EditPosition: IOTAEditPosition; InComment: Boolean);
    procedure CtrlMoveCursor(EditBuffer: IOTAEditBuffer; View: IOTAEditView;
      EditPosition: IOTAEditPosition; ForwardMove: Boolean);
    procedure MoveLineBlockText(EditBuffer: IOTAEditBuffer; Down: Boolean);
    procedure FindDeclaration(EditBuffer: IOTAEditBuffer);
    //procedure ReturnPressed(EditBuffer: IOTAEditBuffer);
  protected
    function GetOptionPages: TTreePage; override;
    procedure Init; override;
    procedure Loaded; override;
  public
    constructor Create;
    destructor Destroy; override;

    { IOTAKeyboardBinding }
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  published
    property Active: Boolean read FActive write SetActive;
    property TabIndent: Boolean read FTabIndent write FTabIndent;
    property IndentSingleLine: Boolean read FIndentSingleLine write FIndentSingleLine;
    property ExtendedHome: Boolean read FExtendedHome write FExtendedHome;
    property SwitchedExtendedHome: Boolean read FSwitchedExtendedHome write FSwitchedExtendedHome;
    property ExtendedCtrlLeftRight: Boolean read FExtendedCtrlLeftRight write FExtendedCtrlLeftRight;
    {$IF CompilerVersion <= 20.0}
    property ShiftF3: Boolean read FShiftF3 write FShiftF3;
    {$IFEND}
    property MoveLineBlock: Boolean read FMoveLineBlock write FMoveLineBlock;
    property FindDeclOnCaret: Boolean read FFindDeclOnCaret write FFindDeclOnCaret;
  end;

  TFrameOptionPageKeybindings = class(TFrameBase, ITreePageComponent)
    cbxActive: TCheckBox;
    cbxTabIndent: TCheckBox;
    cbxExtendedHome: TCheckBox;
    cbxSwitchExtendedHome: TCheckBox;
    cbxIndentSingleLine: TCheckBox;
    cbxExtendedCtrlLeftRight: TCheckBox;
    cbxShiftF3: TCheckBox;
    chkMoveLineBlock: TCheckBox;
    chkFindDeclOnCaret: TCheckBox;
    procedure cbxActiveClick(Sender: TObject);
    procedure cbxExtendedHomeClick(Sender: TObject);
    procedure cbxTabIndentClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FKeyBindings: TKeybindings;
  public
    { Public-Deklarationen }
    procedure SetUserData(UserData: TObject);
    procedure LoadData;
    procedure SaveData;
    procedure Selected;
    procedure Unselected;
  end;

procedure InitPlugin(Unload: Boolean);

implementation

uses
  Main, IDEHooks, Hooking, ToolsAPIHelpers, IDEUtils;

{$R *.dfm}

type
  UTF8Char = AnsiChar;
  PUTF8Char = ^UTF8Char;

var
  Keybindings: TKeybindings;

{$IF CompilerVersion <= 20.0}
procedure TCustomEditControl_RepeatSearch(AEditControl: TWinControl);
  external coreide_bpl name '@Editorcontrol@TCustomEditControl@RepeatSearch$qqrv';
procedure EnvironmentOptionsAddr;
  external coreide_bpl name '@Envoptions@EnvironmentOptions';
{$IFEND}

procedure EditorActionListsPtr;
  external coreide_bpl name '@Editoractions@EditorActionLists';

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    Keybindings := TKeybindings.Create
  else
    FreeAndNil(Keybindings);
end;

function GetEndColumn(EditPosition: IOTAEditPosition): Integer;
var
  Col: Integer;
begin
  Col := EditPosition.Column;
  EditPosition.MoveEOL;
  Result := EditPosition.Column;
  EditPosition.Move(EditPosition.Row, Col);
end;

{function IsPascalFile(const Filename: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := (Ext = '.pas') or (Ext = '.pp') or (Ext = '.inc') or (Ext = '.dpr') or (Ext = '.dpk'));
end;}

{ TFrameOptionPageKeybindings }

procedure TFrameOptionPageKeybindings.SetUserData(UserData: TObject);
begin
  FKeyBindings := UserData as TKeyBindings;
end;

procedure TFrameOptionPageKeybindings.cbxActiveClick(Sender: TObject);
begin
  cbxTabIndent.Enabled := cbxActive.Checked;
  cbxTabIndentClick(cbxTabIndent);
  cbxExtendedHome.Enabled := cbxActive.Checked;
  cbxExtendedCtrlLeftRight.Enabled := cbxActive.Checked;
  cbxExtendedHomeClick(cbxExtendedHome);
end;

procedure TFrameOptionPageKeybindings.cbxExtendedHomeClick(Sender: TObject);
begin
  cbxSwitchExtendedHome.Enabled := cbxExtendedHome.Enabled and cbxExtendedHome.Checked;
end;

procedure TFrameOptionPageKeybindings.cbxTabIndentClick(Sender: TObject);
begin
  cbxIndentSingleLine.Enabled := cbxTabIndent.Enabled;
end;

procedure TFrameOptionPageKeybindings.LoadData;
begin
  cbxActive.Checked := FKeybindings.Active;
  cbxTabIndent.Checked := FKeybindings.TabIndent;
  cbxIndentSingleLine.Checked := FKeyBindings.IndentSingleLine;
  cbxExtendedHome.Checked := FKeyBindings.ExtendedHome;
  cbxSwitchExtendedHome.Checked := FKeyBindings.SwitchedExtendedHome;
  cbxExtendedCtrlLeftRight.Checked := FKeyBindings.ExtendedCtrlLeftRight;
  {$IF CompilerVersion <= 20.0}
  cbxShiftF3.Checked := FKeyBindings.ShiftF3;
  cbxShiftF3.Visible := True;
  {$IFEND}
  chkMoveLineBlock.Checked := FKeyBindings.MoveLineBlock;
  chkFindDeclOnCaret.Checked := FKeyBindings.FindDeclOnCaret;

  cbxActiveClick(cbxActive);
  cbxExtendedHomeClick(cbxExtendedHome);
end;

procedure TFrameOptionPageKeybindings.SaveData;
begin
  FKeybindings.Active := False; // => remove all key bindings
  FKeybindings.TabIndent := cbxTabIndent.Checked;
  FKeyBindings.IndentSingleLine := cbxIndentSingleLine.Checked;
  FKeyBindings.ExtendedHome := cbxExtendedHome.Checked;
  FKeyBindings.SwitchedExtendedHome := cbxSwitchExtendedHome.Checked;
  FKeyBindings.ExtendedCtrlLeftRight := cbxExtendedCtrlLeftRight.Checked;
  {$IF CompilerVersion <= 20.0}
  FKeyBindings.ShiftF3 := cbxShiftF3.Checked;
  {$IFEND}
  FKeyBindings.MoveLineBlock := chkMoveLineBlock.Checked;
  FKeyBindings.FindDeclOnCaret := chkFindDeclOnCaret.Checked;
  FKeybindings.Active := cbxActive.Checked; // => add all active key bindings
  FKeyBindings.Save;
end;

procedure TFrameOptionPageKeybindings.Selected;
begin
end;

procedure TFrameOptionPageKeybindings.Unselected;
begin
end;

{ TKeybindings }

constructor TKeybindings.Create;
begin
  inherited Create(AppDataDirectory + '\KeyBindings.xml', 'KeyBindings');
end;

destructor TKeybindings.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

function TKeybindings.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Key Bindings', TFrameOptionPageKeybindings, Self);
end;

procedure TKeybindings.Init;
begin
  inherited Init;
  TabIndent := True;
  IndentSingleLine := False;
  ExtendedHome := True;
  ExtendedCtrlLeftRight := False;
  {$IF CompilerVersion <= 20.0}
  ShiftF3 := True;
  {$IFEND}
  MoveLineBlock := True;
  FindDeclOnCaret := True;
  Active := True;
end;

procedure TKeybindings.InternCtrlMoveCursor(EditPosition: IOTAEditPosition; InComment: Boolean);
const
  TwoChars: array[0..25] of string = (
    // Common
    '<=', '>=', '//',

    // Delphi
    '<>', ':=', '..', '(*', '*)',

    // C++
    '==', '!=',
    '++', '--', '+=', '-=', '*=', '/=', '~=', '^=', '|=', '&=',
    '&&', '||', '::', '->', '/*', '*/'
  );

var
  EndCol: Integer;
  BufferPos: Integer;
  Buffer: string;
  Ch, StartCh: Char;

  procedure ReadChar;
  begin
    if BufferPos > Length(Buffer) then
    begin
      Buffer := EditPosition.Read(1024);
      BufferPos := 1;
    end;
    if BufferPos > Length(Buffer) then
      Ch := #0
    else
    begin
      Ch := Buffer[BufferPos];
      Inc(BufferPos);
    end;
    //Ch := EditPosition.Character;
  end;

  function StepNext: Boolean;
  begin
    if EditPosition.Column > EndCol then
      Result := False
    else
      Result := EditPosition.MoveRelative(0, 1);
    if Result then
      ReadChar
    else
      Ch := #0;
  end;

  function IsWhiteSpace(Ch: Char): Boolean; inline;
  begin
    Result := (Ch = ' ') or (Ch = #9);
  end;

var
  Ch1: Char;
  I: Integer;
  Handled: Boolean;
begin
  EndCol := GetEndColumn(EditPosition);
  Handled := False;

  BufferPos := 1;
  Buffer := '';
  ReadChar;

  if Ch = '#' then // string-code
  begin
    Handled := True;
    if StepNext then
    begin
      if Ch = '$' then
      begin
        while StepNext and (Ch in ['0'..'9', 'A'..'F', 'a'..'f']) do ; // hex
      end
      else
      begin
        while (Ch in ['0'..'9']) and StepNext do ;
      end;
    end;
  end
  else if Ch in ['0'..'9', '$', '.'] then // number, hex number
  begin
    Handled := True;
    StartCh := Ch;
    if StepNext then
    begin
      if (StartCh = '$') or ((StartCh = '0') and (Ch = 'x')) then // hex
      begin
        while StepNext and (Ch in ['0'..'9', 'A'..'F', 'a'..'f']) do ;
      end
      else if not ((StartCh = '.') and (Ch in ['E', 'e'])) then
      begin
        if (StartCh = '.') and (Ch = '.') then // ".."
          StepNext
        else
        begin  // 1.23e-45
          while (Ch in ['0'..'9']) and StepNext do ; // digit
          if (Ch = '.') then
            StepNext;
          while (Ch in ['0'..'9']) and StepNext do ; // digit
          if Ch in ['E', 'e'] then
          begin
            if StepNext then
            begin
              if Ch in ['-', '+'] then
                StepNext;
            end;
            while (Ch in ['0'..'9']) and StepNext do ; // digit
          end;
        end;
      end;
    end;
  end
  else if (Ch = '_') or IsCharAlphaNumeric(Ch){EditPosition.IsWordCharacter} then
  begin
    Handled := True;
    if StepNext then
    begin
      while (Ch in ['_', '0'..'9']) or IsCharAlphaNumeric(Ch){EditPosition.IsWordCharacter} do
        if not StepNext then
          Break;
    end;
  end;

  if not Handled then
  begin
    if InComment then
    begin
      { Skip whitespaces }
      while (EditPosition.Column < EndCol) and not IsCharAlphaNumeric(Ch) and not IsWhiteSpace(Ch) {EditPosition.IsSpecialCharacter} do
        if not StepNext then
          Break;
    end
    else
    begin
      Ch1 := Ch;
      if StepNext then
      begin
        for I := 0 to High(TwoChars) do
        begin
          if (Ch1 = TwoChars[I][1]) and (Ch = TwoChars[I][2]) then
          begin
            StepNext;
            Break;
          end;
        end
      end;
    end;
  end;

  { Skip whitespaces }
  while (EditPosition.Column < EndCol) and EditPosition.IsWhiteSpace do
    if not StepNext then
      Break;
end;

procedure TKeybindings.CtrlMoveCursor(EditBuffer: IOTAEditBuffer; View: IOTAEditView;
  EditPosition: IOTAEditPosition; ForwardMove: Boolean);

  function IsInComment: Boolean;
  var
    Element, LineFlag: Integer;
  begin
    View.GetAttributeAtPos(View.CursorPos, False, Element, LineFlag);
    Result := Element = atComment;
  end;

var
  CurCol, EndCol, Col, CurRow: Integer;
  OldCol, OldRow: Integer;
  TopLeft: TOTAEditPos;
begin
  EditPosition.Save;
  if EditBuffer <> nil then
    EditBuffer.EditBlock.Save;
  TopLeft := View.TopPos;

  OldCol := EditPosition.Column;
  OldRow := EditPosition.Row;

  CurCol := OldCol;
  EndCol := GetEndColumn(EditPosition); // makes the block invisible

  repeat
    if ForwardMove then
    begin
      if CurCol >= EndCol then
      begin
        if EditPosition.Row < EditPosition.LastRow then
        begin
          EditPosition.Move(EditPosition.Row + 1, 1);
          EndCol := GetEndColumn(EditPosition);
          // skip only whitespaces
          while (EditPosition.Column < EndCol) and EditPosition.IsWhiteSpace do
            if not EditPosition.MoveRelative(0, 1) then
              Break;
        end;
      end
      else
        InternCtrlMoveCursor(EditPosition, IsInComment);
    end
    else
    begin
      if CurCol = 1 then
      begin
        if EditPosition.Row > 1 then
        begin
          EditPosition.Move(EditPosition.Row - 1, 1);
          EditPosition.MoveEOL;
          CurCol := EditPosition.Column;
        end
        else
          Break; // nothing to do
      end
      else
        if CurCol > EndCol then
          CurCol := EndCol;
      { Find the column by stepping from the BOL to the CurCol }
      EditPosition.Move(EditPosition.Row, 1);
      repeat
        Col := EditPosition.Column;
        InternCtrlMoveCursor(EditPosition, IsInComment);
      until EditPosition.Column >= CurCol;
      EditPosition.Move(EditPosition.Row, Col);
    end;
  until True;

  CurRow := EditPosition.Row;
  CurCol := EditPosition.Column;
  EditPosition.Restore;
  View.TopPos := TopLeft;

  if EditBuffer = nil then
    EditPosition.Move(CurRow, CurCol)
  else
  begin
    EditBuffer.EditBlock.Restore;
    EditBuffer.EditBlock.ExtendRelative(CurRow - OldRow, CurCol - OldCol);
  end;
end;

function SkipToNextLine(P: PAnsiChar): PAnsiChar;
begin
  Result := P;
  while True do
  begin
    case Result^ of
      #0:
        Break;
      #10:
        Inc(Result);
      #13:
        begin
          Inc(Result);
          if Result^ = #10 then
            Inc(Result);
          Break;
        end;
    end;
    Inc(Result);
  end;
end;

function GetLenWithoutLastLineBreak(S: UTF8String): Integer;
begin
  Result := Length(S);
  while (Result > 0) and (S[Result] in [#10, #13]) do
    Dec(Result);
end;


const
  emKeepTrailingBlanks = $00000020;
  emOptimalFill        = $00000040;
  emGroupUndo          = $00000100;
  emPersistentBlocks   = $00000200;
  emDisableUndo        = $00000800;

type
  TCharIndex = SmallInt;
  TLineNum = LongInt;

  PCharPos = ^TCharPos;
  TCharPos = packed record
    Index: TCharIndex;
    LineNum: TLineNum;
  end;

  PEdFile = ^TEdFile;
  TEdFile = packed record
    Pos: Longint;
    CharPos: TCharPos;
    MidChar: Boolean;
  end;

  TEdModes = LongWord;
  TEdModFlags = Word;

  PEditorBuffer = ^TEditorBuffer;
  TEditorBuffer = packed record
    NLines: TLineNum;
    ModifyFlags: TEdModFlags;
    EdModes: TEdModes;

    UndoAvailable: Boolean;
    RedoAvailable: Boolean;
    // ...
  end;

  PEkView = ^TEkView;
  TEkView = record
    PrevWin: PEkView;
    NextWin: PEkView;
    ViewData: Pointer;

    Editor: PEditorBuffer;
    // ...
  end;

  TEditWriter = class(TInterfacedObject)
  protected
    EkView: PEkView;
    EdFile: TEdFile;
    FilePos: Longint;
  end;

function SetEkViewEdModes(EkView: PEkView; SetFlags, ClearFlags: TEdModes): Int64;
begin
  Result := 0;
  if SetFlags and emDisableUndo <> 0 then
  begin
    ClearFlags := ClearFlags or emOptimalFill;
    SetFlags := SetFlags or emKeepTrailingBlanks;
  end;

  // Prefer ClearFlags over SetFlags
  SetFlags := SetFlags and not ClearFlags;

  if (EkView <> nil) and (EkView.Editor <> nil) then
  begin
    ULARGE_INTEGER(Result).LowPart := SetFlags and not (EkView.Editor.EdModes and SetFlags);
    ULARGE_INTEGER(Result).HighPart := EkView.Editor.EdModes and ClearFlags;

    EkView.Editor.EdModes := (EkView.Editor.EdModes and not ClearFlags) or SetFlags;
  end;
end;

procedure RestoreEkViewEdModes(EkView: PEkView; RestoreFlags: Int64);
var
  SetFlags, ClearFlags: TEdModes;
begin
  if (EkView <> nil) and (EkView.Editor <> nil) then
  begin
    ClearFlags := ULARGE_INTEGER(RestoreFlags).LowPart;
    SetFlags := ULARGE_INTEGER(RestoreFlags).HighPart;

    // apply in revers to restore the original settings
    EkView.Editor.EdModes := (EkView.Editor.EdModes and not ClearFlags) or SetFlags;
  end;
end;

procedure TKeybindings.MoveLineBlockText(EditBuffer: IOTAEditBuffer; Down: Boolean);
type
  TBlock = record
    Row: Integer;
    Col: Integer;
    StartingRow: Integer;
    EndingRow: Integer;
    StartingColumn: Integer;
    EndingColumn: Integer;
  end;

var
  StartRow, EndRow, LastRow, Row, RowOffset: Integer;
  BlockSize: Integer;
  EditBlock: IOTAEditBlock;
  EditPosition: IOTAEditPosition;
  Source: UTF8String;
  Start, UnchangedP, BlockStart, BlockEnd, AffectedLineP: PAnsiChar;
  Writer: IOTAEditWriter;
  S: UTF8String;
  LastUnchangedRow: Integer;
  Block: TBlock;
  EkView: PEkView;
  RestoreFlags: Int64;
  EditWriter: TEditWriter;
begin
  EditPosition := EditBuffer.EditPosition;
  EditBlock := EditBuffer.EditBlock;
  StartRow := EditPosition.Row;
  BlockSize := EditBlock.Size;

  Block.Row := EditPosition.Row;
  Block.Col := EditPosition.Column;
  Block.StartingRow := EditBlock.StartingRow;
  Block.EndingRow := EditBlock.EndingRow;
  Block.StartingColumn := EditBlock.StartingColumn;
  Block.EndingColumn := EditBlock.EndingColumn;

  if (BlockSize <> 0) and (EditBlock.Style = btColumn) then
  begin
    // EditBlock.Starting*/Ending* are wrong in this mode, stick to moving one line
    Block.StartingRow := Block.Row;
    Block.EndingRow := Block.Row;
    BlockSize := 0;
  end;
  // If only the caret is in the last line and first column than ignore that line.
  if (BlockSize > 0) and (Block.EndingRow > Block.StartingRow) and (Block.EndingColumn = 1) then
  begin
    if Block.Row = Block.EndingRow then
      Dec(Block.Row);
    Dec(Block.EndingRow);
  end;

  LastRow := EditBuffer.GetLinesInBuffer;
  if BlockSize = 0 then
    EndRow := StartRow
  else
  begin
    StartRow := Block.StartingRow;
    EndRow := Block.EndingRow;
  end;

  // MoveUp on first line is not allowed. MoveUp/Down is not allowed on the last line because it doesn't work and Delphi inserts #13#10 where it wants
  if (not Down and ((StartRow <= 1) or (EndRow >= LastRow))) or (Down and (EndRow >= LastRow - 1)) then
    Exit;

  Source := GetEditorSource(EditBuffer);
  UnchangedP := PAnsiChar(Source);
  Start := UnchangedP;
  LastUnchangedRow := StartRow;
  if not Down then
    Dec(LastUnchangedRow);
  Row := 1;
  while (UnchangedP^ <> #0) and (Row < LastUnchangedRow) do
  begin
    UnchangedP := SkipToNextLine(UnchangedP);
    Inc(Row);
  end;

  if Row = LastUnchangedRow then
  begin
    BlockStart :=  UnchangedP;
    if not Down then // we skippt the unchanged line
    begin
      BlockStart := SkipToNextLine(BlockStart);
      Inc(Row);
    end;
    BlockEnd := BlockStart;
    while (BlockEnd^ <> #0) and (Row <= EndRow) do
    begin
      BlockEnd := SkipToNextLine(BlockEnd);
      Inc(Row);
    end;

    AffectedLineP := nil;
    if Down then
      AffectedLineP := SkipToNextLine(BlockEnd);

    SetString(S, BlockStart, BlockEnd - BlockStart);

    Writer := EditBuffer.CreateUndoableWriter;
    EditWriter := TEditWriter(DelphiInterfaceToObject(Writer));
    if not EditWriter.ClassNameIs('TEditWriter') then
      raise Exception.CreateFmt('EditBuffer (%s) is not of type TEditWriter', [EditWriter.ClassName]);
    EkView := EditWriter.EkView;

    // Copy unaffected part
    Writer.CopyTo(UnchangedP - Start);
    if Down then
    begin
      // Delete moved block
      Writer.DeleteTo(BlockEnd - Start);
      // Copy line that was moved up due to the moved block
      Writer.CopyTo(AffectedLineP - Start);
    end;

    // Insert moved block
    Writer.Insert(PAnsiChar(S));

    if not Down then
    begin
      // Copy line that was moved down due to the moved block
      Writer.CopyTo(BlockStart - Start);
      // Delete moved block
      Writer.DeleteTo(BlockEnd - Start);
    end;
    // Copy unaffected part
    Writer.CopyTo(Length(Source));
    Writer := nil; // end undo group

    if Down then
      RowOffset := 1
    else
      RowOffset := -1;

    RestoreFlags := SetEkViewEdModes(EkView, emDisableUndo or emPersistentBlocks, 0);
    try
      if BlockSize = 0 then
        EditPosition.Move(Block.Row + RowOffset, 0)
      else
      begin
        EditBlock.Reset;
        EditBlock.Style := btNonInclusive;

        //PersistentBlocks := EditBuffer.BufferOptions.PersistentBlocks;
        try
          //EditBuffer.BufferOptions.PersistentBlocks := True;
          EditPosition.Move(Block.StartingRow + RowOffset, 1);
          EditBlock.BeginBlock;
          try
            EditPosition.Move(Block.EndingRow + 1 + RowOffset, 1);
            //EditPosition.MoveEOL;
          finally
            EditBlock.EndBlock;
          end;
          EditPosition.Move(Block.StartingRow + RowOffset, 1);
        finally
          //EditBuffer.BufferOptions.PersistentBlocks := PersistentBlocks;
        end;
      end;
    finally
      RestoreEkViewEdModes(EkView, RestoreFlags);
    end;
  end;
end;

(*
procedure TKeybindings.ReturnPressed(EditBuffer: IOTAEditBuffer);
var
  TopLeft: TOTAEditPos;
  EditPosition: IOTAEditPosition;
  EndCol: Integer;
  NeedIndention: Boolean;
  Ch: Char;

  function StepBack: Boolean;
  begin
    Result := EditPosition.MoveRelative(0, -1);
  end;

var
  Element, LineFlag: Integer;
  Text: string;
begin
  EditPosition := EditBuffer.EditPosition;
  EditPosition.Save;
  TopLeft := EditBuffer.TopView.TopPos;
  EndCol := GetEndColumn(EditPosition);

  NeedIndention := False;
  if (EditPosition.Column >= EndCol) and (EditPosition.Column > 1) then
  begin
    while EditPosition.IsSpecialCharacter and StepBack do ; // skip whitespaces
    while EditPosition.IsWhiteSpace and StepBack do ; // skip whitespaces

    NeedIndention := True;
    //EditBuffer.TopView.GetAttributeAtPos(EditBuffer.TopView.CursorPos, False, Element, LineFlag);
    if EditPosition.Character = ';' then
    begin
      EditPosition.MoveRelative(0, -1);
      while EditPosition.IsWhiteSpace and StepBack do ; // skip whitespaces

      // If it is "end", we have to indent it correctly
      EditPosition.MoveRelative(0, -3);
      Text := EditPosition.Read(3);
      if SameText(Text, 'end') then
      begin

      end;
    end;

  end;
  EditPosition.Restore;
  EditBuffer.TopView.TopPos := TopLeft;

  EditPosition.InsertCharacter(#13);
  {if NeedIndention then
    EditBuffer.EditBlock.Indent(1);}
end;*)

procedure TKeybindings.DoKeyBinding(const Context: IOTAKeyContext; KeyCode: TShortcut;
  var BindingResult: TKeyBindingResult);

  function GetIndentSize: Integer;
  begin
    Result := (BorlandIDEServices as IOTAEditorServices).EditOptions.BlockIndent;
  end;

var
  EditPosition: IOTAEditPosition;
  EditBuffer: IOTAEditBuffer;
  EditBlock: IOTAEditBlock;
  Column: Integer;
  BindingRec: TKeyBindingRec;
  {$IF CompilerVersion <= 20.0}
  SearchForwardEnvProp: TPropField;
  OldSearchForwardValue: Boolean;
  {$IFEND}
begin
  BindingResult := krUnhandled;

  EditBuffer := Context.EditBuffer;
  if EditBuffer <> nil then
  begin
    EditPosition := EditBuffer.EditPosition;
    if Active then
    begin
      EditBlock := EditBuffer.EditBlock;
      if (EditBlock <> nil) and (EditBlock.Size > 0) then
      begin
        if TabIndent and (KeyCode = ShortCut(VK_TAB, [])) then
        begin
          if (IndentSingleLine or (EditBlock.StartingRow <> EditBlock.EndingRow)) then // un/indent only if a line break is included in the block
            EditBlock.Indent(GetIndentSize)
          else
            EditPosition.InsertCharacter(#9); // Delphi 2010 always uses "IndentSingleLine"
          BindingResult := krHandled;
        end
        else
        if TabIndent and (KeyCode = ShortCut(VK_TAB, [ssShift])) then
        begin
          if (IndentSingleLine or (EditBlock.StartingRow <> EditBlock.EndingRow)) then // un/indent only if a line break is included in the block
            EditBlock.Indent(-GetIndentSize)
          else
            EditPosition.Tab(-1);
          BindingResult := krHandled;
        end;
      end;

      if BindingResult <> krHandled then
      begin
        if ExtendedHome and (KeyCode = ShortCut(VK_HOME, [])) then
        begin
          if SwitchedExtendedHome and (EditPosition.Column > 1) then
          begin
            { First jump to the first non-whitespace and then to the BOL }
            Column := EditPosition.Column;
            EditPosition.MoveBOL;
            while (EditPosition.Character in [#9, ' ']) and EditPosition.MoveRelative(0, 1) do
              ;
            if EditPosition.Column = Column then
              EditPosition.MoveBOL;
            BindingResult := krHandled;
          end
          else
          begin
            { First jump to the BOL and then to the first non-whitespace }
            if EditPosition.Column = 1 then
            begin
              while (EditPosition.Character in [#9, ' ']) and EditPosition.MoveRelative(0, 1) do
                ;
              BindingResult := krHandled;
            end;
          end;
        end

        else if (KeyCode = ShortCut(VK_RIGHT, [ssCtrl])) or
                (KeyCode = ShortCut(VK_LEFT, [ssCtrl])) then
        begin
          CtrlMoveCursor(nil, EditBuffer.TopView, EditPosition, Byte(KeyCode) = VK_RIGHT{, IsPascalFile(EditBuffer.FileName)});
          BindingResult := krHandled;
        end
        else if (KeyCode = ShortCut(VK_RIGHT, [ssCtrl, ssShift])) or
                (KeyCode = ShortCut(VK_LEFT, [ssCtrl, ssShift])) then
        begin
          CtrlMoveCursor(EditBuffer, EditBuffer.TopView, EditPosition, Byte(KeyCode) = VK_RIGHT{, IsPascalFile(EditBuffer.FileName)});
          BindingResult := krHandled;
        end

        else if KeyCode = ShortCut(VK_UP, [ssAlt, ssShift, ssCtrl]) then
        begin
          MoveLineBlockText(EditBuffer, False);
          BindingResult := krHandled;
        end
        else if KeyCode = ShortCut(VK_DOWN, [ssAlt, ssShift, ssCtrl]) then
        begin
          MoveLineBlockText(EditBuffer, True);
          BindingResult := krHandled;
        end

        else if KeyCode = ShortCut(VK_PRIOR, [ssCtrl, ssAlt]) then
        begin
          FindDeclaration(EditBuffer);
          BindingResult := krHandled;
        end;


        {else if KeyCode = ShortCut(VK_RETURN, []) then
        begin
          ReturnPressed(EditBuffer);
          BindingResult := krHandled;
        end;}
      end;

      {$IF CompilerVersion <= 20.0} // Delphi 2009- (Delphi 2010 introduced Shift-F3)
      if BindingResult <> krHandled then
      begin
        if ShiftF3 and (KeyCode = ShortCut(VK_F3, [ssShift])) then
        begin
          if (Screen.ActiveControl <> nil) and (Screen.ActiveControl.Name = 'Editor') and
             Screen.ActiveControl.ClassNameIs('TEditControl') then
          begin
            SearchForwardEnvProp := TPropField((TObject(PPointer(GetActualAddr(@EnvironmentOptionsAddr))^) as TComponent).FindComponent('SearchForward'));
            if SearchForwardEnvProp <> nil then
            begin
              OldSearchForwardValue := SearchForwardEnvProp.Value;
              try
                SearchForwardEnvProp.Value := not OldSearchForwardValue;
                TCustomEditControl_RepeatSearch(Screen.ActiveControl);
              finally
                SearchForwardEnvProp.Value := OldSearchForwardValue;
              end;
              BindingResult := krHandled;
            end;
          end;
        end;
      end;
      {$IFEND}
    end;

    if BindingResult = krUnhandled then
    begin
      if Context.GetKeyBindingRec(BindingRec) and Context.KeyboardServices.GetNextBindingRec(BindingRec) then
      begin
        { Let the next proc struggle with the IDE }
        BindingResult := krNextProc;
        Exit;
      end;

      { The HOME key isn't processed when bound to a method }
      if KeyCode = ShortCut(VK_HOME, []) then
      begin
        EditPosition.MoveBOL;
        BindingResult := krHandled;
      end
      else
      if KeyCode = ShortCut(VK_TAB, [ssShift]) then
      begin
        { The Shift-TAB key isn't processed when bound to a method }
        EditPosition.Tab(-1);
        BindingResult := krHandled;
      end
      else
      if KeyCode = ShortCut(VK_TAB, []) then
      begin
        { The TAB key isn't processed when bound to a method }
        if not (BorlandIDEServices as IOTAEditorServices).EditOptions.BufferOptions.SmartTab then
        begin
          EditPosition.InsertCharacter(#9);
          BindingResult := krHandled;
        end
        else
        begin
          EditPosition.Align(1);
          BindingResult := krHandled;
        end;
      end;
    end;
  end;
end;

procedure TKeybindings.FindDeclaration(EditBuffer: IOTAEditBuffer);
var
  DataModule: TDataModule;
  FindDecl: TComponent;
  EditCtrl: TWinControl;
begin
  DataModule := TDataModule(GetActualAddr(@EditorActionListsPtr)^);
  if DataModule <> nil then
  begin
    FindDecl := DataModule.FindComponent('FindDeclaration');
    if FindDecl is TAction then
    begin
      EditCtrl := Screen.ActiveControl;
      if (EditCtrl <> nil) and (EditCtrl.Name = 'Editor') and EditCtrl.ClassNameIs('TEditControl') then
      begin
        TAction(FindDecl).ActionList.Tag := NativeInt(EditCtrl);
        TAction(FindDecl).Execute;
      end;
    end;
  end;
end;

procedure TKeybindings.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  if TabIndent then
  begin
    BindingServices.AddKeyBinding([ShortCut(VK_TAB, [ssShift])], DoKeyBinding, nil, 0);
    BindingServices.AddKeyBinding([ShortCut(VK_TAB, [])], DoKeyBinding, nil, 0);
  end;

  if ExtendedHome then
  begin
    BindingServices.AddKeyBinding([ShortCut(VK_HOME, [])], DoKeyBinding, nil, 0);
  end;

  if ExtendedCtrlLeftRight then
  begin
    BindingServices.AddKeyBinding([ShortCut(VK_RIGHT, [ssCtrl])], DoKeyBinding, nil, 0);
    BindingServices.AddKeyBinding([ShortCut(VK_RIGHT, [ssCtrl, ssShift])], DoKeyBinding, nil, 0);
    BindingServices.AddKeyBinding([ShortCut(VK_LEFT, [ssCtrl])], DoKeyBinding, nil, 0);
    BindingServices.AddKeyBinding([ShortCut(VK_LEFT, [ssCtrl, ssShift])], DoKeyBinding, nil, 0);
  end;

  //BindingServices.AddKeyBinding([ShortCut(VK_RETURN, [])], DoKeyBinding, nil, 0);

  {$IF CompilerVersion <= 20.0}
  if ShiftF3 then
  begin
    BindingServices.AddKeyBinding([ShortCut(VK_F3, [ssShift])], DoKeyBinding, nil, 0);
  end;
  {$IFEND}

  if MoveLineBlock then
  begin
    BindingServices.AddKeyBinding([ShortCut(VK_UP, [ssShift, ssCtrl, ssAlt])], DoKeyBinding, nil, 0);
    BindingServices.AddKeyBinding([ShortCut(VK_DOWN, [ssShift, ssCtrl, ssAlt])], DoKeyBinding, nil, 0);
  end;

  if FindDeclOnCaret then
  begin
    BindingServices.AddKeyBinding([ShortCut(VK_PRIOR, [ssCtrl, ssAlt])], DoKeyBinding, nil, 0);
  end;


end;

function TKeybindings.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TKeybindings.GetDisplayName: string;
begin
  Result := 'DDevExtensions KeyBindings';
end;

function TKeybindings.GetName: string;
begin
  Result := 'DDevExtensions.KeyBindings';
end;

procedure TKeybindings.AfterSave;
begin
end;

procedure TKeybindings.BeforeSave;
begin
end;

procedure TKeybindings.Destroyed;
begin
end;

procedure TKeybindings.Modified;
begin
end;

procedure TKeybindings.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    if FActive then
    begin
      if FNotifierIndex >= 0 then
        (BorlandIDEServices as IOTAKeyboardServices).RemoveKeyboardBinding(FNotifierIndex);
      FNotifierIndex := -1;
    end;
    FActive := Value;
    if FActive then
      FNotifierIndex := (BorlandIDEServices as IOTAKeyboardServices).AddKeyboardBinding(Self);
    if not Application.Terminated then
      (BorlandIDEServices as IOTAKeyboardServices).RestartKeyboardServices;
  end;
end;

procedure TKeybindings.Loaded;
begin
  inherited Loaded;
  if Active then
  begin
    Active := False;
    Active := True;
  end;
end;

end.
