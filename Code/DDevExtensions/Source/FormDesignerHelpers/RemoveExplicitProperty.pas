{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2008 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit RemoveExplicitProperty;

{$I ..\DelphiExtension.inc}

interface

{$IFDEF COMPILER10_UP}

uses
  SysUtils, Classes, Forms, Controls, IDEHooks, Hooking;

procedure SetRemoveExplicitPropertyActive(Active: Boolean);

{$ENDIF COMPILER10_UP}

implementation

{$IFDEF COMPILER10_UP}

uses
  IDEUtils;

var
  HookTControl_DefineProperties: TRedirectCode;

type
  TControlEx = class(TControl)
  protected
    procedure ReadIsControl(Reader: TReader);
    procedure WriteIsControl(Writer: TWriter);

    procedure WriteExplicitLeft(Writer: TWriter);
    procedure WriteExplicitTop(Writer: TWriter);
    procedure WriteExplicitWidth(Writer: TWriter);
    procedure WriteExplicitHeight(Writer: TWriter);

    procedure ReadExplicitLeft(Reader: TReader);
    procedure ReadExplicitTop(Reader: TReader);
    procedure ReadExplicitWidth(Reader: TReader);
    procedure ReadExplicitHeight(Reader: TReader);

    procedure IgnoreInteger(Reader: TReader);

    procedure DefineProperties(Filer: TFiler); override;
  end;

  TOpenControl = class(TControl);

procedure TControlEx.ReadIsControl(Reader: TReader);
begin
  IsControl := Reader.ReadBoolean;
end;

procedure TControlEx.WriteIsControl(Writer: TWriter);
begin
  Writer.WriteBoolean(IsControl);
end;

procedure TControlEx.WriteExplicitTop(Writer: TWriter);
begin
  Writer.WriteInteger(FExplicitTop);
end;

procedure TControlEx.WriteExplicitHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FExplicitHeight);
end;

procedure TControlEx.WriteExplicitLeft(Writer: TWriter);
begin
  Writer.WriteInteger(FExplicitLeft);
end;

procedure TControlEx.ReadExplicitWidth(Reader: TReader);
begin
  FExplicitWidth := Reader.ReadInteger;
end;

procedure TControlEx.WriteExplicitWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FExplicitWidth);
end;

procedure TControlEx.ReadExplicitTop(Reader: TReader);
begin
  FExplicitTop := Reader.ReadInteger;
end;

procedure TControlEx.ReadExplicitHeight(Reader: TReader);
begin
  FExplicitHeight := Reader.ReadInteger;
end;

procedure TControlEx.ReadExplicitLeft(Reader: TReader);
begin
  FExplicitLeft := Reader.ReadInteger;
end;

procedure TControlEx.IgnoreInteger(Reader: TReader);
begin
  Reader.ReadInteger;
end;

procedure TControlEx.DefineProperties(Filer: TFiler);
type
  TExplicitDimension = (edLeft, edTop, edWidth, edHeight);

  function DoWriteIsControl: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := TControlEx(Filer.Ancestor).IsControl <> IsControl else
      Result := IsControl;
  end;

  function DoWriteExplicit(Dim: TExplicitDimension): Boolean;
  begin
    case Dim of
      edLeft: Result := ((Filer.Ancestor <> nil) and (TControl(Filer.Ancestor).ExplicitLeft <> FExplicitLeft)) or
        ((Filer.Ancestor = nil) and ((Align <> alNone) or ((Anchors * [akLeft]) = [])) and (FExplicitLeft <> Left));
      edTop: Result := ((Filer.Ancestor <> nil) and (TControl(Filer.Ancestor).ExplicitTop <> FExplicitTop)) or
        ((Filer.Ancestor = nil) and ((Align <> alNone) or ((Anchors * [akTop]) = [])) and (FExplicitTop <> Top));
      edWidth: Result := ((Filer.Ancestor <> nil) and (TControl(Filer.Ancestor).ExplicitWidth <> FExplicitWidth)) or
        ((Filer.Ancestor = nil) and ((Align <> alNone) or ((Anchors * [akLeft, akRight]) = [akLeft, akRight])) and (FExplicitWidth <> Width));
      edHeight: Result := ((Filer.Ancestor <> nil) and (TControl(Filer.Ancestor).ExplicitHeight <> FExplicitHeight)) or
        ((Filer.Ancestor = nil) and ((Align <> alNone) or ((Anchors * [akTop, akBottom]) = [akTop, akBottom])) and (FExplicitHeight <> Height));
    else
      Result := False;
    end;
  end;

begin
  { The call to inherited DefinedProperties is omitted since the Left and
    Top special properties are redefined with real properties }
  Filer.DefineProperty('IsControl', ReadIsControl, WriteIsControl, DoWriteIsControl);

  if csDesigning in ComponentState then
  begin
    Filer.DefineProperty('ExplicitLeft', IgnoreInteger, nil, False);
    Filer.DefineProperty('ExplicitTop', IgnoreInteger, nil, False);
    Filer.DefineProperty('ExplicitWidth', IgnoreInteger, nil, False);
    Filer.DefineProperty('ExplicitHeight', IgnoreInteger, nil, False);
  end
  else
  begin
    Filer.DefineProperty('ExplicitLeft', ReadExplicitLeft, WriteExplicitLeft, not (csReading in ComponentState) and DoWriteExplicit(edLeft));
    Filer.DefineProperty('ExplicitTop', ReadExplicitTop, WriteExplicitTop, not (csReading in ComponentState) and DoWriteExplicit(edTop));
    Filer.DefineProperty('ExplicitWidth', ReadExplicitWidth, WriteExplicitWidth, not (csReading in ComponentState) and DoWriteExplicit(edWidth));
    Filer.DefineProperty('ExplicitHeight', ReadExplicitHeight, WriteExplicitHeight, not (csReading in ComponentState) and DoWriteExplicit(edHeight));
  end;
end;


var
  IsActive: Boolean;

procedure SetRemoveExplicitPropertyActive(Active: Boolean);
begin
  if Active <> IsActive then
  begin
    IsActive := Active;
    if Active then
      CodeRedirect(@TOpenControl.DefineProperties, @TControlEx.DefineProperties, HookTControl_DefineProperties)
    else
      UnhookFunction(HookTControl_DefineProperties);
  end;
end;

{$ENDIF COMPILER10_UP}

end.
