{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2005-2013 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit Splash;

{$I DelphiExtension.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Graphics, StdCtrls,
  ToolsAPI,
  ExtCtrls, AppConsts;

procedure ShowOnSplashScreen;
procedure SetSplashProgress(const Text: string);
procedure DoneSplash;

implementation

uses
  Main;

{$R Splash.res}

var
  LblProgress: TLabel;

type
  TSplashScreen = class(TComponent)
  public
    destructor Destroy; override;
  end;

destructor TSplashScreen.Destroy;
begin
  LblProgress := nil;
  IDELoaded;
  if Supports(BorlandIDEServices, IOTAAboutBoxServices) then
    (BorlandIDEServices as IOTAAboutBoxServices).AddPluginInfo(sPluginName, 'DDevExtensions - Extensions for the IDE' +
       sLineBreak + sLineBreak + sPluginSmallCopyright, 0);
  inherited Destroy;
end;

procedure SetSplashProgress(const Text: string);
begin
  if Assigned(LblProgress) then
    LblProgress.Caption := Text;
end;

var
  InitTimerId: Integer = -1;

procedure IsIDELoaded(wnd: HWND; Msg: Cardinal; idEvent: Cardinal; time: LongWord); stdcall;
begin
  if (Application = nil) or Application.Terminated then
  begin
    KillTimer(0, InitTimerId);
    InitTimerId := -1;
  end
  else
  if (Application.MainForm <> nil) and Application.MainForm.Visible then
  begin
    KillTimer(0, InitTimerId);
    InitTimerId := -1;
    IDELoaded;
  end;
end;

procedure ShowOnSplashScreen;
var
  SplashScreenInit: Boolean;
begin
  SplashScreenInit := False;
  if BorlandIDEServices <> nil then
  begin
    SplashScreenServices.StatusMessage(sPluginName);
    SplashScreenServices.AddPluginBitmap(sPluginName, LoadBitmap(Hinstance, 'DDEVEXTENSIONSLOGO'));
  end;

  if InitTimerId <> -1 then
    KillTimer(0, InitTimerId);
  InitTimerId := -1;
  if not SplashScreenInit then
    InitTimerId := SetTimer(0, 0, 100, @IsIDELoaded);
end;

procedure DoneSplash;
begin
  if InitTimerId <> -1 then
    KillTimer(0, InitTimerId);
end;

end.
