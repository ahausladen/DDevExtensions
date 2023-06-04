{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2008 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}

unit FrmBase;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.Themes, Vcl.Styles;

type
  TFormBase = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    function ShowModal: Integer; override;
  end;

var
  FormBase: TFormBase;

implementation

uses
  HtHint;

{$R *.dfm}

type
  PPointer = ^Pointer;
  {$IF not declared(SIZE_T)}
  SIZE_T = DWORD;
  {$IFEND}

  TControlAccess = class(TControl);

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
begin
  Result := PPointer(PAnsiChar(AClass) + Index * SizeOf(Pointer))^;
end;

procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);
var
  WrittenBytes: SIZE_T;
  PatchAddress: PPointer;
begin
  PatchAddress := Pointer(PAnsiChar(AClass) + Index * SizeOf(Pointer));
  WriteProcessMemory(GetCurrentProcess, PatchAddress, @Method, SizeOf(Method), WrittenBytes);
end;

procedure ReaderError(Self: TObject; Reader: TReader; const Message: string; var Handled: Boolean);
begin
  Handled := True;
end;

function IgnoreReader_NewInstance(AClass: TClass): TObject;
var
  M: TMethod;
begin
  M.Code := @ReaderError;
  M.Data := nil;
  Result := TReader.NewInstance;
  TReader(Result).OnError := TReaderError(M);
end;

{ TFormBase }

constructor TFormBase.Create(AOwner: TComponent);
const
  {$WARNINGS OFF}
  Index = vmtNewInstance div SizeOf(Pointer);
  {$WARNINGS ON}
var
  NewInst: procedure;
begin
  NewInst := GetVirtualMethod(TReader, Index);
  try
    SetVirtualMethod(TReader, Index, @IgnoreReader_NewInstance);
    inherited Create(AOwner);
  finally
    SetVirtualMethod(TReader, Index, @NewInst);
  end;
  Font.Name := UTF8ToString(DefFontData.Name);
  Font.Height := DefFontData.Height;
end;

procedure TFormBase.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  if Action <> caNone then
  begin
    // Save state
  end;
end;

procedure TFormBase.DoShow;

  // Set the dialogs base font name to every control that uses "Tahoma" (all MS Sans Serif were eliminated)
  procedure SetControlFonts(ParentControl: TWinControl);
  var
    I: Integer;
    Control: TControl;
  begin
    for I := 0 to ParentControl.ControlCount - 1 do
    begin
      Control := ParentControl.Controls[I];
      if TControlAccess(Control).Font.Name = 'Tahoma' then
        TControlAccess(Control).Font.Name := Self.Font.Name;
      if Control is TWinControl then
        SetControlFonts(TWinControl(Control));
    end;
  end;

begin
  // restore state
  inherited DoShow;

  if Self.Font.Name <> 'Tahoma' then
    SetControlFonts(Self);
end;

procedure TFormBase.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  {$IF CompilerVersion >= 33.0} // 10.3 Rio+
  for StyleName in TStyleManager.StyleNames do
  begin
    if StyleName.StartsWith('Win10IDE_') then
      self.StyleName:= StyleName;
  end;
  {$ENDIF}
end;

function TFormBase.ShowModal: Integer;
var
  HintClass: THintWindowClass;
  HintHidePause: Integer;
begin
  HintHidePause := Application.HintHidePause;
  HintClass := HintWindowClass;
  try
    HintWindowClass := THtHintWindow;
    Application.HintHidePause := 30000;
    Result := inherited ShowModal;
  finally
    Application.HintHidePause := HintHidePause;
    HintWindowClass := HintClass;
  end;
end;

end.
 
