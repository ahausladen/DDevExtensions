{******************************************************************************}
{*                                                                            *}
{* CompileInterceptor IDE Plugin                                              *}
{*                                                                            *}
{* (C) 2006-2013 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit IdeDllNames;

interface

uses
  Windows, SysUtils;

const
  LastSupportedIDEVersion = 350; // adjust ToolsAPIIntf.pas if necessary

var
  bcbide_bpl: PChar;
  coreide_bpl: PChar;
  rtl_bpl: PChar;
  vcl_bpl: PChar;
  delphicoreide_bpl: PChar;
  designide_bpl: PChar;
  DelphiVersion: string;
  DelphiVer: Integer;

const
  bcbide5_bpl = 'bcbide50.bpl';
  coreide5_bpl = 'coride50.bpl';
  rtl5_bpl = 'vcl50.bpl';
  vcl5_bpl = 'vcl50.bpl';
  delphicoreide5_bpl = coreide5_bpl;
  designide5_bpl = 'dsnide50.bpl';

  bcbide6_bpl = 'bcbide60.bpl';
  coreide6_bpl = 'coreide60.bpl';
  rtl6_bpl = 'rtl60.bpl';
  vcl6_bpl = 'vcl60.bpl';
  delphicoreide6_bpl = coreide6_bpl;
  designide6_bpl = 'designide60.bpl';

  bcbide7_bpl = 'bcbide70.bpl';
  coreide7_bpl = 'coreide70.bpl';
  rtl7_bpl = 'rtl70.bpl';
  vcl7_bpl = 'vcl70.bpl';
  delphicoreide7_bpl = coreide7_bpl;
  designide7_bpl = 'designide70.bpl';

  // Galileo IDE:
  bcbide_base_bpl = 'bcbide.bpl';
  coreide_base_bpl = 'coreide.bpl';
  rtl_base_bpl = 'rtl.bpl';
  vcl_base_bpl = 'vcl.bpl';
  delphicoreide_base_bpl = 'delphicoreide.bpl';
  vclide_base_bpl = 'vclide.bpl';
  designide_base_bpl = 'designide.bpl';

implementation

var
  StrMemAllocated: Boolean = False;

procedure MakeVersionDll(var Name: PChar; const DllName, VersionStr: string);
begin
  if Name <> nil then
    StrDispose(Name);
  Name := StrNew(PChar(ChangeFileExt(DllName, VersionStr + ExtractFileExt(DllName))));
end;

procedure FreeStrMem;
begin
  StrDispose(coreide_bpl);
  StrDispose(bcbide_bpl);
  StrDispose(rtl_bpl);
  StrDispose(vcl_bpl);
  StrDispose(delphicoreide_bpl);
  StrDispose(designide_bpl);
  StrMemAllocated := False;
end;

function InitVersion: Boolean;
var
  Version: Integer;
  VersionStr: string;
begin
  Version := 90;
  while Version <= LastSupportedIDEVersion do
  begin
    VersionStr := IntToStr(Version);
    MakeVersionDll(coreide_bpl, coreide_base_bpl, VersionStr);
    if GetModuleHandle(coreide_bpl) <> 0 then
    begin
      MakeVersionDll(bcbide_bpl, bcbide_base_bpl, VersionStr);
      MakeVersionDll(rtl_bpl, rtl_base_bpl, VersionStr);
      MakeVersionDll(vcl_bpl, vcl_base_bpl, VersionStr);
      MakeVersionDll(delphicoreide_bpl, delphicoreide_base_bpl, VersionStr);
      MakeVersionDll(designide_bpl, designide_base_bpl, VersionStr);

      DelphiVer := Version div 10;
      DelphiVersion := IntToStr(DelphiVer);

      StrMemAllocated := True;
      Result := True;
      Exit;
    end;
    Inc(Version, 10);
  end;
  StrDispose(coreide_bpl);
  coreide_bpl := nil;
  Result := False;
end;

initialization
  if not InitVersion then
  begin
    if GetModuleHandle(coreide7_bpl) <> 0 then
    begin
      bcbide_bpl := bcbide7_bpl;
      coreide_bpl := coreide7_bpl;
      rtl_bpl := rtl7_bpl;
      vcl_bpl := vcl7_bpl;
      delphicoreide_bpl := delphicoreide7_bpl;
      designide_bpl := designide7_bpl;
      DelphiVersion := '7';
      DelphiVer := 7
    end
    else
    if GetModuleHandle(coreide6_bpl) <> 0 then
    begin
      bcbide_bpl := bcbide6_bpl;
      coreide_bpl := coreide6_bpl;
      rtl_bpl := rtl6_bpl;
      vcl_bpl := vcl6_bpl;
      delphicoreide_bpl := delphicoreide6_bpl;
      designide_bpl := designide6_bpl;
      DelphiVersion := '6';
      DelphiVer := 6
    end
    else
    if GetModuleHandle(coreide5_bpl) <> 0 then
    begin
      bcbide_bpl := bcbide5_bpl;
      coreide_bpl := coreide5_bpl;
      rtl_bpl := rtl5_bpl;
      vcl_bpl := vcl5_bpl;
      delphicoreide_bpl := delphicoreide5_bpl;
      designide_bpl := designide5_bpl;
      DelphiVersion := '5';
      DelphiVer := 5
    end
    else
      MessageBox(0, 'No compatible Delphi version loaded', 'CompileInterceptor', MB_OK or MB_ICONERROR);
  end;

finalization
  if StrMemAllocated then
    FreeStrMem;

end.

