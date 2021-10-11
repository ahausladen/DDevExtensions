// * Copyright: ©2021 Fred Schetterer

unit RemovePixelsPerInchProperty;

{$I ..\DelphiExtension.inc}

interface

uses
  SysUtils, Classes, Forms, Controls, IDEHooks, Hooking;


procedure SetRemovePixelsPerInchPropertyActive(Active: Boolean);

implementation

uses
//  CodeSiteLogging,
  IDEUtils;

var
  HookTControl_DefineProperties: TRedirectCode;

type
  /// <summary>
  ///   Access to Private Parts
  /// </summary>
  TDataModuleHelper = class helper for TDataModule
    procedure DefineProperties2(Filer: TFiler);
  end;

type
  TOpenDataModule = class(TDataModule);

var
  IsActive: Boolean;

procedure SetRemovePixelsPerInchPropertyActive(Active: Boolean);
begin
  if Active <> IsActive then
  begin
    IsActive := Active;
    if Active then
      CodeRedirect(@TOpenDataModule.DefineProperties, @TDataModule.DefineProperties2, HookTControl_DefineProperties)
    else
      UnhookFunction(HookTControl_DefineProperties);
  end;
end;

procedure TDataModuleHelper.DefineProperties2(Filer: TFiler);
var
  Ancestor: TDataModule;

  function DoWriteWidth: Boolean;
  begin
    Result := True;
    if Ancestor <> nil then
      Result := DesignSize.X <> Ancestor.DesignSize.X;
  end;

  function DoWriteHorizontalOffset: Boolean;
  begin
    if Ancestor <> nil then
      Result := DesignOffset.X <> Ancestor.DesignOffset.X else
      Result := DesignOffset.X <> 0;
  end;

  function DoWriteVerticalOffset: Boolean;
  begin
    if Ancestor <> nil then
      Result := DesignOffset.Y <> Ancestor.DesignOffset.Y else
      Result := DesignOffset.Y <> 0;
  end;

  function DoWriteHeight: Boolean;
  begin
    Result := True;
    if Ancestor <> nil then Result := DesignSize.Y <> Ancestor.DesignSize.Y;
  end;

begin
{$If Declared(TCodeSiteLogger)}
  CodeSite.Send('DefineProperties');
{$IfEnd}

  Ancestor := TDataModule(Filer.Ancestor);
  with self do begin // access to private parts
    Filer.DefineProperty('Height', ReadHeight, WriteHeight, DoWriteHeight);
    Filer.DefineProperty('HorizontalOffset', ReadHorizontalOffset,
      WriteHorizontalOffset, DoWriteHorizontalOffset);
    Filer.DefineProperty('VerticalOffset', ReadVerticalOffset,
      WriteVerticalOffset, DoWriteVerticalOffset);
    Filer.DefineProperty('Width', ReadWidth, WriteWidth, DoWriteWidth);
    Filer.DefineProperty('OldCreateOrder', IgnoreIdent, nil, False);
    // We need to read if it exists else it Errors, but never write it..
    Filer.DefineProperty('PixelsPerInch', ReadPixelsPerInch, WritePixelsPerInch, (csReading in ComponentState));
  end;

{$IF CompilerVersion > 35}
  Check if anything changed in System.Classes.TDataModule.DefineProperties
{$IFEND}
end;

end.
