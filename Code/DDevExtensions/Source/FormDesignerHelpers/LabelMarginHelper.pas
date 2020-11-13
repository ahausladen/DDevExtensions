{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2007 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit LabelMarginHelper;

{$I ..\DelphiExtension.inc}

interface

{$IFDEF COMPILER10_UP}

uses
  SysUtils, Classes, Forms, Controls, IDEHooks, Hooking, StdCtrls;

procedure SetLabelMarginActive(Active: Boolean);

{$ENDIF COMPILER10_UP}

implementation

{$IFDEF COMPILER10_UP}

type
  TLabelMargins = class(TMargins)
  protected
    class procedure InitDefaults(Margins: TMargins); override;
  published
    property Bottom default 0;
  end;

class procedure TLabelMargins.InitDefaults(Margins: TMargins);
begin
  inherited InitDefaults(Margins);
  Margins.Bottom := 0;
end;

procedure Label_AfterConstruction(Self: TLabel);
type
  TAfterConstructionProc = procedure(Self: TLabel);
var
  P: ^TMargins;
  ChangeEvent: TNotifyEvent;
begin
  if csDesigning in Self.ComponentState then
  begin
    P := @Self.Margins;
    ChangeEvent := Self.Margins.OnChange;
    P^.Free;
    P^ := TLabelMargins.Create(Self);
    P^.OnChange := ChangeEvent;
  end;
  TAfterConstructionProc(@TLabel.AfterConstruction)(Self);
end;

var
  IsActive: Boolean;

procedure SetLabelMarginActive(Active: Boolean);
begin
  if Active <> IsActive then
  begin
    IsActive := Active;
    if Active then
      ReplaceVmtField(TLabel, @TLabel.AfterConstruction, @Label_AfterConstruction)
    else
      ReplaceVmtField(TLabel, @Label_AfterConstruction, @TLabel.AfterConstruction);
  end;
end;

{$ENDIF COMPILER10_UP}

end.

