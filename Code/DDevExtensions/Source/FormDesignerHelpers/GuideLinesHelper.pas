{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit GuideLinesHelper;

{$I ..\DelphiExtension.inc}

interface

{$IFNDEF COMPILER12_UP}
{$IFDEF COMPILER10_UP}

uses
  SysUtils, Classes, Forms, Controls, IDEHooks, Hooking;

procedure SetGuildLinesHelperActive(Active: Boolean);

{$ENDIF COMPILER10_UP}
{$ENDIF ~COMPILER12_UP}

implementation

{$IFNDEF COMPILER12_UP}
{$IFDEF COMPILER10_UP}

uses
  Utils;

var
  TDesigner_SnapToAlignGuide: TRedirectCode;
  TDesigner_SnapToMarginGuide: TRedirectCode;

type
  TGuideType = set of (gtFirst = 0, gtLast = 5);

function Hook_TDesigner_SnapToAlignGuide(Instance: TObject; var i: Integer;
  const ItemGuideLines: IInterface; List: TList; GuideType: TGuideType): Boolean;
type
  TTDesigner_SnapToAlignGuide = function(Instance: TObject; var i: Integer;
    const ItemGuideLines: IInterface; List: TList; GuideType: TGuideType): Boolean;
var
  Shift: TShiftState;
begin
  Shift := KeyboardStateToShiftState;
  if (ssAlt in Shift) and ([ssLeft, ssMiddle] * Shift <> []) then
    Result := False
  else
  begin
    UnhookFunction(TDesigner_SnapToAlignGuide);
    try
      Result := TTDesigner_SnapToAlignGuide(TDesigner_SnapToAlignGuide.RealProc)(Instance, i, ItemGuideLines, List, GuideType);
    finally
      RehookFunction(@Hook_TDesigner_SnapToAlignGuide, TDesigner_SnapToAlignGuide);
    end;
  end;
end;

function Hook_TDesigner_SnapToMarginGuide(Instance: TObject; const ItemWrapper: IInterface;
  var i: Integer; const ItemGuideLines: IInterface; List: TList; GuideType: TGuideType): Boolean;
type
  TTDesigner_SnapToMarginGuide = function(Instance: TObject; const ItemWrapper: IInterface;
    var i: Integer; const ItemGuideLines: IInterface; List: TList; GuideType: TGuideType): Boolean;
var
  Shift: TShiftState;
begin
  Shift := KeyboardStateToShiftState;
  if (ssAlt in Shift) and ([ssLeft, ssMiddle] * Shift <> []) then
    Result := False
  else
  begin
    UnhookFunction(TDesigner_SnapToMarginGuide);
    try
      Result := TTDesigner_SnapToMarginGuide(TDesigner_SnapToMarginGuide.RealProc)(Instance, ItemWrapper, i, ItemGuideLines, List, GuideType);
    finally
      RehookFunction(@Hook_TDesigner_SnapToMarginGuide, TDesigner_SnapToMarginGuide);
    end;
  end;
end;

var
  IsActive: Boolean;

procedure SetGuildLinesHelperActive(Active: Boolean);
begin
  if Active <> IsActive then
  begin
    IsActive := Active;
    if Active then
    begin
      HookFunction(designide_bpl, '@Designer@TDesigner@SnapToAlignGuide$qqrrix52System@%DelphiInterface$t24Designer@IItemGuidelines%p19Contnrs@TObjectList48System@%Set$t19Designer@TGuideType$iuc$0$iuc$12%',
        @Hook_TDesigner_SnapToAlignGuide, TDesigner_SnapToAlignGuide);
      HookFunction(designide_bpl, '@Designer@TDesigner@SnapToMarginGuide$qqrx49System@%DelphiInterface$t21Designer@IItemWrapper%rix52System@%DelphiInterface$t24Designer@IItemGuidelines%p19Contnrs@TObjectList48System@%Set$t19Designer@TGuideType$iuc$0$iuc$12%',
        @Hook_TDesigner_SnapToMarginGuide, TDesigner_SnapToMarginGuide);
    end
    else
    begin
      UnhookFunction(TDesigner_SnapToAlignGuide);
      UnhookFunction(TDesigner_SnapToMarginGuide);
    end;
  end;
end;

{$ENDIF COMPILER10_UP}
{$ENDIF ~COMPILER12_UP}

end.

