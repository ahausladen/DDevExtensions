{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit FrmeOptionPageCompilerEnhancements;

{$I ..\DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ToolsAPI, FrmTreePages, FrmOptions, PluginConfig, StdCtrls,
  ModuleData, InterceptIntf, FrmeBase, ExtCtrls;

type
  TCompilerEnhancements = class(TPluginConfig, ICompileInterceptor)
  private
    FActive: Boolean;
    FTreatWarningsAsErrors: Boolean;
    FExceptWarnings: TStrings;
    FCompileInterceptorId: Integer;
    procedure SetExceptWarnings(const Value: TStrings);
    procedure SetActive(const Value: Boolean);
  protected
    function GetOptionPages: TTreePage; override;
    procedure Init; override;
    procedure UpdateExceptWarnings;
  public
    constructor Create;
    destructor Destroy; override;

    function AlterFile(Filename: PAnsiChar; Content: PAnsiChar;
      FileDate: Integer; FileSize: Integer): IVirtualStream; stdcall;
    function AlterMessage(IsCompilerMessage: Boolean; var MsgKind: TMsgKind;
      var Code: Integer; var Filename: string; Line: Integer; Column: Integer;
      var Msg: string): Boolean; stdcall;
    function GetOptions: TCompileInterceptOptions; stdcall;
    function GetVirtualFile(Filename: PAnsiChar): IVirtualStream; stdcall;
    procedure InspectFilename(Filename: PAnsiChar; FileMode: TInspectFileMode); stdcall;
  published
    property Active: Boolean read FActive write SetActive;
    property TreatWarningsAsErrors: Boolean read FTreatWarningsAsErrors write FTreatWarningsAsErrors;
    property ExceptWarnings: TStrings read FExceptWarnings write SetExceptWarnings;
  end;

  TFrameOptionPageCompilerEnhancements = class(TFrameBase, ITreePageComponent)
    cbxActive: TCheckBox;
    cbxTreatWarningsAsErrors: TCheckBox;
    mnoExceptWarnings: TMemo;
    lblExceptWarningsCaption: TLabel;
    procedure cbxActiveClick(Sender: TObject);
    procedure cbxTreatWarningsAsErrorsClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FCompilerEnhancements: TCompilerEnhancements;
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
  Main, Utils, InterceptLoader;

{$R *.dfm}

var
  CompilerEnhancements: TCompilerEnhancements;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    CompilerEnhancements := TCompilerEnhancements.Create
  else
    FreeAndNil(CompilerEnhancements);
end;

{ TFrameOptionPageCompilerEnhancements }

procedure TFrameOptionPageCompilerEnhancements.cbxActiveClick(Sender: TObject);
begin
  cbxTreatWarningsAsErrors.Enabled := cbxActive.Checked;
  cbxTreatWarningsAsErrors.OnClick(cbxTreatWarningsAsErrors);
end;

procedure TFrameOptionPageCompilerEnhancements.SetUserData(UserData: TObject);
begin
  FCompilerEnhancements := UserData as TCompilerEnhancements;
end;

procedure TFrameOptionPageCompilerEnhancements.cbxTreatWarningsAsErrorsClick(
  Sender: TObject);
begin
  mnoExceptWarnings.Enabled := cbxTreatWarningsAsErrors.Checked and cbxTreatWarningsAsErrors.Enabled;
  lblExceptWarningsCaption.Enabled := mnoExceptWarnings.Enabled;
  if mnoExceptWarnings.Enabled then
    mnoExceptWarnings.Color := clWindow
  else
    mnoExceptWarnings.Color := clBtnFace;
end;

procedure TFrameOptionPageCompilerEnhancements.LoadData;
begin
  cbxActive.Checked := FCompilerEnhancements.Active;
  cbxTreatWarningsAsErrors.Checked := FCompilerEnhancements.TreatWarningsAsErrors;
  mnoExceptWarnings.Lines.Text := FCompilerEnhancements.ExceptWarnings.CommaText;

  cbxActiveClick(cbxActive);
end;

procedure TFrameOptionPageCompilerEnhancements.SaveData;
begin
  FCompilerEnhancements.TreatWarningsAsErrors := cbxTreatWarningsAsErrors.Checked;
  FCompilerEnhancements.ExceptWarnings.CommaText := mnoExceptWarnings.Lines.Text;
  FCompilerEnhancements.UpdateExceptWarnings;

  FCompilerEnhancements.Active := cbxActive.Checked;
  FCompilerEnhancements.Save;
end;

procedure TFrameOptionPageCompilerEnhancements.Selected;
begin
end;

procedure TFrameOptionPageCompilerEnhancements.Unselected;
begin
end;

{ TCompilerEnhancements }

constructor TCompilerEnhancements.Create;
begin
  inherited Create(AppDataDirectory + '\CompilerEnhancements.xml', 'CompilerEnhancements');

end;

destructor TCompilerEnhancements.Destroy;
begin
  Active := False;
  FExceptWarnings.Free;
  inherited Destroy;
end;

procedure TCompilerEnhancements.Init;
begin
  inherited Init;
  TreatWarningsAsErrors := False;
  FExceptWarnings := TStringList.Create;
  TStringList(FExceptWarnings).Sorted := True;
  FExceptWarnings.Clear;
  FExceptWarnings.Add('W1000'); // deprecated
  FExceptWarnings.Add('W1054'); // $MESSAGE WARNING
  Active := False;
end;

function TCompilerEnhancements.GetOptionPages: TTreePage;
begin
  Result := TTreePage.Create('Compiler Enhancements', TFrameOptionPageCompilerEnhancements, Self);
end;

function TCompilerEnhancements.GetOptions: TCompileInterceptOptions;
begin
  Result := CIO_ALTERMESSAGES;
end;

function TCompilerEnhancements.AlterFile(Filename, Content: PAnsiChar; FileDate,
  FileSize: Integer): IVirtualStream;
begin
  Result := nil;
end;

procedure TCompilerEnhancements.InspectFilename(Filename: PAnsiChar;
  FileMode: TInspectFileMode);
begin
end;

procedure TCompilerEnhancements.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
      FCompileInterceptorId := GetCompileInterceptorServices.RegisterInterceptor(Self)
    else
      GetCompileInterceptorServices.UnregisterInterceptor(FCompileInterceptorId);
  end;
end;

procedure TCompilerEnhancements.SetExceptWarnings(const Value: TStrings);
begin
  if Value <> FExceptWarnings then
    FExceptWarnings.Assign(Value);
end;

procedure TCompilerEnhancements.UpdateExceptWarnings;
var
  i: Integer;
begin
  TStringList(ExceptWarnings).Sorted := False;
  for i := 0 to ExceptWarnings.Count - 1 do
    ExceptWarnings[i] := Trim(DequoteStr(ExceptWarnings[i]));
  TStringList(ExceptWarnings).Sorted := True;
end;

function TCompilerEnhancements.GetVirtualFile(Filename: PAnsiChar): IVirtualStream;
begin
  Result := nil;
end;

function TCompilerEnhancements.AlterMessage(IsCompilerMessage: Boolean;
  var MsgKind: TMsgKind; var Code: Integer; var Filename: string; Line,
  Column: Integer; var Msg: string): Boolean;
begin
  Result := False;
  if TreatWarningsAsErrors then
  begin
    if (MsgKind = mkWarning) and
       ((ExceptWarnings.Count = 0) or (ExceptWarnings.IndexOf('W' + IntToStr(Code)) <> -1)) then
    begin
      MsgKind := mkError;
      Result := True;
    end;
  end;
end;

end.
