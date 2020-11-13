{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit FocusEditor;

{$I ..\DelphiExtension.inc}

interface

{
  Sets the focus to the editor window when the desktop settings were changed.
}

uses
  SysUtils, Classes, Forms, Controls, IDEHooks, Hooking;

procedure InitPlugin(Unload: Boolean);

implementation

uses
  IDEUtils;

var
  LoadDesktopHook: TRedirectCode;

procedure Hook_LoadDesktop(Instance: TObject; Desktop: TObject);
type
  TLoadDesktopProc = procedure(Instance: TObject; Desktop: TObject);
var
  I: Integer;
  Editor: TComponent;
begin
  UnhookFunction(LoadDesktopHook);
  try
    try
      TLoadDesktopProc(LoadDesktopHook.RealProc)(Instance, Desktop);
    finally
      for I := 0 to Screen.FormCount - 1 do
      begin
        if (Screen.Forms[I].ClassName = 'TEditWindow') and
           Screen.Forms[I].Visible and Screen.Forms[I].Enabled and
           Screen.Forms[I].CanFocus then
        begin
          Editor := Screen.Forms[I].FindComponent('Editor');
          if (Editor is TWinControl) and TWinControl(Editor).CanFocus then
            TWinControl(Editor).SetFocus;
          Break;
        end;
      end;
    end;
  finally
    RehookFunction(@Hook_LoadDesktop, LoadDesktopHook);
  end;
end;

procedure InitPlugin(Unload: Boolean);
begin
  if not Unload then
    HookFunction(coreide_bpl, '@Desktop@TDesktopStates@LoadDesktop$qqrp21Desktop@TDesktopState',
      @Hook_LoadDesktop, LoadDesktopHook)
  else
    UnhookFunction(LoadDesktopHook);
end;

end.

