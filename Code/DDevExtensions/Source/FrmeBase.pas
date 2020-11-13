{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2008 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit FrmeBase;

{$I DelphiExtension.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, StdCtrls, FrmTreePages;


type
  TFrameBase = class(TFrame, ITreePageComponentEx)
    pnlClient: TPanel;
    pnlDescription: TPanel;
    bvlSplitter: TBevel;
    lblDescription: TLabel;
    lblCaption: TLabel;
  private
    { Private-Deklarationen }
  protected
    procedure SetTitle(const ACaption: string);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

type
  PPointer = ^Pointer;
  {$IF not declared(SIZE_T)}
  SIZE_T = DWORD;
  {$IFEND}

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

{ TFrameBase }

constructor TFrameBase.Create(AOwner: TComponent);
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

  Align := alClient;
  {$IFDEF COMPILER7_UP}
  pnlDescription.Color := clBtnFace;
  pnlDescription.ParentBackground := True;
  {$ENDIF COMPILER7_UP}
end;

procedure TFrameBase.SetTitle(const ACaption: string);
begin
  lblCaption.Caption := ACaption;
end;

end.
