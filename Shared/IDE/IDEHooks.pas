{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit IDEHooks;

{$IF CompilerVersion >= 25.0} // XE4+
  {$LEGACYIFEND ON}
{$IFEND}

{.$I jedi\jedi.inc}
{$IFDEF VER130} {$DEFINE COMPILER5} {$ENDIF}  // Delphi 5
{$IFDEF VER140} {$DEFINE COMPILER6} {$ENDIF}  // Delphi 6
{$IFDEF VER150} {$DEFINE COMPILER7} {$ENDIF}  // Delphi 7
{$IFDEF VER170} {$DEFINE COMPILER9} {$ENDIF}  // Delphi 2005
{$IFDEF VER180} {$DEFINE COMPILER10} {$ENDIF} // Delphi 2006
{$IFDEF VER185} {$DEFINE COMPILER11} {$UNDEF COMPILER10} {$ENDIF} // Delphi 2007
{$IFDEF VER200} {$DEFINE COMPILER12} {$ENDIF} // Delphi 2009
// The other versions use the CompilerVersion constant because ErrorInsight can handle it

interface

uses
  Windows;

const
  {$IFDEF EMBT_DEBUG_EDITION}
  CompilerVersion = 24.0;
  {$ENDIF EMBT_DEBUG_EDITION}

{$IFNDEF CONDITIONALEXPRESSIONS}

{$IFDEF COMPILER5}
  bcbide_bpl = 'bcbide50.bpl';
  delphide_bpl = 'dphide50.bpl';
  coreide_bpl = 'coride50.bpl';
  rtl_bpl = 'vcl50.bpl';
  vcl_bpl = 'vcl50.bpl';
  delphicoreide_bpl = coreide_bpl;
  designide_bpl = 'dsgnide50.bpl';
  vclide_bpl = 'vclide50.bpl';
  DelphiVersion = '5';
  BplVersion = '50';
  dcc32_dll = 'dcc50.dll';
{$ENDIF COMPIELR5}

{$ELSE}

{$IFDEF COMPILER6}
  bcbide_bpl = 'bcbide60.bpl';
  delphide_bpl = 'delphide60.bpl';
  coreide_bpl = 'coreide60.bpl';
  rtl_bpl = 'rtl60.bpl';
  vcl_bpl = 'vcl60.bpl';
  delphicoreide_bpl = coreide_bpl;
  vclide_bpl = 'vclide60.bpl';
  designide_bpl = 'designide60.bpl';
  DelphiVersion = '6';
  BplVersion = '60';
  dcc32_dll = 'dcc60.dll';
{$ENDIF COMPIELR6}
{$IFDEF COMPILER7}
  bcbide_bpl = 'bcbide70.bpl';
  delphide_bpl = 'delphide70.bpl';
  coreide_bpl = 'coreide70.bpl';
  rtl_bpl = 'rtl70.bpl';
  vcl_bpl = 'vcl70.bpl';
  delphicoreide_bpl = coreide_bpl;
  vclide_bpl = 'vclide70.bpl';
  designide_bpl = 'designide70.bpl';
  DelphiVersion = '7';
  BplVersion = '70';
  dcc32_dll = 'dcc70.dll';
{$ENDIF COMPIELR7}
{$IFDEF COMPILER9} // Delphi 2005
  bcbide_bpl = 'bcbide90.bpl';
  delphide_bpl = 'delphide90.bpl';
  coreide_bpl = 'coreide90.bpl';
  rtl_bpl = 'rtl90.bpl';
  vcl_bpl = 'vcl90.bpl';
  delphicoreide_bpl = 'delphicoreide90.bpl';
  vclide_bpl = 'vclide90.bpl';
  designide_bpl = 'designide90.bpl';
  idectrls_bpl = 'idectrls90.bpl';
  DelphiVersion = '9';
  BplVersion = '90';
  dcc32_dll = 'dcc90.dll';
{$ENDIF COMPILER9}
{$IFDEF COMPILER10} // Delphi 2006
  bcbide_bpl = 'bcbide100.bpl';
  delphide_bpl = 'delphide100.bpl';
  coreide_bpl = 'coreide100.bpl';
  coreproide_bpl = 'coreproide100.bpl';
  rtl_bpl = 'rtl100.bpl';
  vcl_bpl = 'vcl100.bpl';
  delphicoreide_bpl = 'delphicoreide100.bpl';
  vclide_bpl = 'vclide100.bpl';
  designide_bpl = 'designide100.bpl';
  dbkdebugide_bpl = 'dbkdebugide100.bpl';
  win32debugide_bpl = 'win32debugide100.bpl';
  idectrls_bpl = 'idectrls100.bpl';
  DelphiVersion = '10';
  BplVersion = '100';
  dcc32_dll = 'dcc100.dll';
  comp32x_dll = 'comp32x.dll';
{$ENDIF COMPILER10}
{$IFDEF COMPILER11} // Delphi 2007 has the same BPL version that Delphi 2006 has
  bcbide_bpl = 'bcbide100.bpl';
  delphide_bpl = 'delphide100.bpl';
  coreide_bpl = 'coreide100.bpl';
  coreproide_bpl = 'coreproide100.bpl';
  rtl_bpl = 'rtl100.bpl';
  vcl_bpl = 'vcl100.bpl';
  delphicoreide_bpl = 'delphicoreide100.bpl';
  vclide_bpl = 'vclide100.bpl';
  designide_bpl = 'designide100.bpl';
  dbkdebugide_bpl = 'dbkdebugide100.bpl';
  win32debugide_bpl = 'win32debugide100.bpl';
  idectrls_bpl = 'idectrls100.bpl';
  bordbkN_dll = 'bordbk105N.dll';
  DelphiVersion = '11';
  BplVersion = '100'; // the "this is a non-breaking release" problem
  dcc32_dll = 'dcc100.dll'; // compiler has same version number as Delphi 2006
  comp32x_dll = 'comp32x.dll';
{$ENDIF COMPIELR11}
{$IFDEF COMPILER12} // Delphi 2009
  bcbide_bpl = 'bcbide120.bpl';
  delphide_bpl = 'delphide120.bpl';
  coreide_bpl = 'coreide120.bpl';
  coreproide_bpl = 'coreproide120.bpl';
  rtl_bpl = 'rtl120.bpl';
  vcl_bpl = 'vcl120.bpl';
  delphicoreide_bpl = 'delphicoreide120.bpl';
  vclide_bpl = 'vclide120.bpl';
  designide_bpl = 'designide120.bpl';
  dbkdebugide_bpl = 'dbkdebugide120.bpl';
  win32debugide_bpl = 'win32debugide120.bpl';
  idectrls_bpl = 'idectrls120.bpl';
  bordbkN_dll = 'bordbk120N.dll';
  DelphiVersion = '12';
  BplVersion = '120';
  dcc32_dll = 'dcc120.dll';
  comp32x_dll = 'comp32x.dll';
{$ENDIF COMPIELR12}
{$IF CompilerVersion = 21.0} // Delphi 2010
  bcbide_bpl = 'bcbide140.bpl';
  delphide_bpl = 'delphide140.bpl';
  coreide_bpl = 'coreide140.bpl';
  coreproide_bpl = 'coreproide140.bpl';
  rtl_bpl = 'rtl140.bpl';
  vcl_bpl = 'vcl140.bpl';
  delphicoreide_bpl = 'delphicoreide140.bpl';
  vclide_bpl = 'vclide140.bpl';
  designide_bpl = 'designide140.bpl';
  dbkdebugide_bpl = 'dbkdebugide140.bpl';
  win32debugide_bpl = 'win32debugide140.bpl';
  idectrls_bpl = 'idectrls140.bpl';
  bordbkN_dll = 'bordbk140N.dll';
  DelphiVersion = '14';
  BplVersion = '140';
  dcc32_dll = 'dcc140.dll';
  comp32x_dll = 'comp32x.dll';
{$IFEND}
{$IF CompilerVersion = 22.0} // Delphi XE
  bcbide_bpl = 'bcbide150.bpl';
  delphide_bpl = 'delphide150.bpl';
  coreide_bpl = 'coreide150.bpl';
  coreproide_bpl = 'coreproide150.bpl';
  rtl_bpl = 'rtl150.bpl';
  vcl_bpl = 'vcl150.bpl';
  delphicoreide_bpl = 'delphicoreide150.bpl';
  vclide_bpl = 'vclide150.bpl';
  designide_bpl = 'designide150.bpl';
  dbkdebugide_bpl = 'dbkdebugide150.bpl';
  win32debugide_bpl = 'win32debugide150.bpl';
  idectrls_bpl = 'idectrls150.bpl';
  bordbkN_dll = 'bordbk150N.dll';
  DelphiVersion = '15';
  BplVersion = '150';
  dcc32_dll = 'dcc150.dll';
  comp32x_dll = 'comp32x.dll';
{$IFEND}
{$IF CompilerVersion = 23.0} // Delphi XE2
  bcbide_bpl = 'bcbide160.bpl';
  delphide_bpl = 'delphide160.bpl';
  coreide_bpl = 'coreide160.bpl';
  coreproide_bpl = 'coreproide160.bpl';
  rtl_bpl = 'rtl160.bpl';
  vcl_bpl = 'vcl160.bpl';
  delphicoreide_bpl = 'delphicoreide160.bpl';
  vclide_bpl = 'vclide160.bpl';
  designide_bpl = 'designide160.bpl';
  dbkdebugide_bpl = 'dbkdebugide160.bpl';
  win32debugide_bpl = 'win32debugide160.bpl';
  idectrls_bpl = 'idectrls160.bpl';
  bordbkN_dll = 'bordbk160N.dll';
  DelphiVersion = '16';
  BplVersion = '160';
  dcc32_dll = 'dcc32160.dll';
  dcc64_dll = 'dcc64160.dll';
  dccosx32_dll = 'dccosx160.dll';
  comp32x_dll = 'comp32x.dll';
{$IFEND}
{$IF CompilerVersion = 24.0} // Delphi XE3
  DelphiVersion = '17';
  BplVersion = '170';
{$IFEND}
{$IF CompilerVersion = 25.0} // Delphi XE4
  DelphiVersion = '18';
  BplVersion = '180';
{$IFEND}
{$IF CompilerVersion = 26.0} // Delphi XE5
  DelphiVersion = '19';
  BplVersion = '190';
{$IFEND}
{$IF CompilerVersion = 27.0} // Delphi XE6
  DelphiVersion = '20';
  BplVersion = '200';
{$IFEND}
{$IF CompilerVersion = 28.0} // Delphi XE7
  DelphiVersion = '21';
  BplVersion = '210';
{$IFEND}
{$IF CompilerVersion = 29.0} // Delphi XE8
  DelphiVersion = '22';
  BplVersion = '220';
{$IFEND}
{$IF CompilerVersion = 30.0} // Delphi 10 Seattle
  DelphiVersion = '23';
  BplVersion = '230';
{$IFEND}
{$IF CompilerVersion = 31.0} // Delphi 10.1 Berlin
  DelphiVersion = '24';
  BplVersion = '240';
{$IFEND}
{$IF CompilerVersion = 32.0} // Delphi 10.2 Tokyo
  DelphiVersion = '25';
  BplVersion = '250';
{$IFEND}
{$IF CompilerVersion = 33.0} // Delphi 10.3 Rio
  DelphiVersion = '26';
  BplVersion = '260';
{$IFEND}
{$IF CompilerVersion = 34.0} // Delphi 10.4 Sydney
  DelphiVersion = '27';
  BplVersion = '270';
{$IFEND}
{$IF CompilerVersion = 35.0} // Delphi 11.0 Alexandria
  DelphiVersion = '28';
  BplVersion = '280';
{$IFEND}
{$IF CompilerVersion = 36.0} // Delphi 12.0 Athens
  DelphiVersion = '29';
  BplVersion = '290';
{$IFEND}

{$IF CompilerVersion >= 24.0} // Delphi XE3+
  bcbide_bpl = 'bcbide' + BplVersion + '.bpl';
  delphide_bpl = 'delphide' + BplVersion + '.bpl';
  coreide_bpl = 'coreide' + BplVersion + '.bpl';
  coreproide_bpl = 'coreproide' + BplVersion + '.bpl';
  rtl_bpl = 'rtl' + BplVersion + '.bpl';
  vcl_bpl = 'vcl' + BplVersion + '.bpl';
  delphicoreide_bpl = 'delphicoreide' + BplVersion + '.bpl';
  vclide_bpl = 'vclide' + BplVersion + '.bpl';
  designide_bpl = 'designide' + BplVersion + '.bpl';
  dbkdebugide_bpl = 'dbkdebugide' + BplVersion + '.bpl';
  win32debugide_bpl = 'win32debugide' + BplVersion + '.bpl';
  idectrls_bpl = 'idectrls' + BplVersion + '.bpl';
  bordbkN_dll = 'bordbk' + BplVersion + 'N.dll';
  dcc32_dll = 'dcc32' + BplVersion + '.dll';
  dcc64_dll = 'dcc64' + BplVersion + '.dll';
  dccosx32_dll = 'dccosx' + BplVersion + '.dll';
  comp32x_dll = 'comp32x.dll';
  compclang_dll = 'compclang.dll';

  visualizationserviceside_bpl = 'VisualizationServiceIDE' + BplVersion + '.bpl';
  dclbindcomp_bpl = 'dclbindcomp' + BplVersion + '.bpl';
{$IFEND}

  dbkdebugproide_bpl = 'dbkdebugproide' + BplVersion + '.bpl';
  vcldesigner_bpl = 'vcldesigner' + BplVersion + '.bpl';
  boreditu_dll = 'boreditu.dll';

  dccios32_dll = 'dccios32' + BplVersion + '.dll';
  dcciosarm_dll = 'dcciosarm' + BplVersion + '.dll';
{$IF CompilerVersion >= 26.0} // XE5+
  dccaarm_dll = 'dccaarm' + BplVersion + '.dll';
{$IFEND}
{$IF CompilerVersion >= 29.0} // XE8+
  dccios64_dll = 'dccios64' + BplVersion + '.dll';
{$IFEND}
{$IF CompilerVersion >= 33.0} // 10.3 Rio+
  dcclinux32_dll = 'dcclinux32' + BplVersion + '.dll';
  dcclinux64_dll = 'dcclinux64' + BplVersion + '.dll';
{$IFEND}

{$IF defined(DCC64)}
  dcc_dll = dcc64_dll;
  DccPointerSize = 8;
{$ELSEIF defined(DCCAARM)}
  dcc_dll = dccaarm_dll;
  DccPointerSize = 4;
{$ELSE}
  dcc_dll = dcc32_dll;
  DccPointerSize = 4;
{$IFEND}


{$IF CompilerVersion >= 23.0} // XE2+
  Unit_System_SysUtils = '@System@Sysutils';
  System_Classes_TStream = '22System@Classes@TStream';
  System_Classes_TStrings = '23System@Classes@TStrings';
  System_Classes_TList = '20System@Classes@TList';
  System_Classes_TListNotification = '32System@Classes@TListNotification';
  System_Inifiles_TCustomIniFile = '30System@Inifiles@TCustomIniFile';
{$ELSE}
  Unit_System_SysUtils = '@Sysutils';
  System_Classes_TStream = '15Classes@TStream';
  System_Classes_TStrings = '16Classes@TStrings';
  System_Classes_TList = '13Classes@TList';
  System_Classes_TListNotification = '25Classes@TListNotification';
  System_Inifiles_TCustomIniFile = '23Inifiles@TCustomIniFile';
{$IFEND}

{$IFDEF UNICODE}
  System_String = '20System@UnicodeString';
  System_UnicodeString = System_String;
{$ELSE}
  System_String = '17System@AnsiString';
  System_UnicodeString = '17System@WideString';
{$ENDIF UNICODE}

{$IF CompilerVersion >= 24.0} // XE3+
  _xp_ = 'xp';
{$ELSE}
  _xp_ = 'px';
{$IFEND}

{$ENDIF ~CONDITIONALEXPRESSIONS}

function GetDccHandle: HMODULE;
function DbgStrictGetProcAddress(Handle: THandle; Name: PAnsiChar): Pointer;

const
  COMPILER_VERSION_10_2_RTM = (Int64(25) shl 48) or (Int64(0) shl 32) or (26309 shl 16) or (314);
  COMPILER_VERSION_10_2_UP1 = (Int64(25) shl 48) or (Int64(0) shl 32) or (27659 shl 16) or (1188);

function SameCompilerVersion(AVersion: Int64; AIgnoreBuild: Boolean = True): Boolean;
function CheckCompilerVersion(AVersion: Int64; AIgnoreBuild: Boolean = True): Boolean; overload;
function CheckCompilerVersion(AMajor, AMinor, ARelease: Integer; ABuild: Integer = -1): Boolean; overload;

implementation

var
  LibDcc: HMODULE;

function GetDccHandle: HMODULE;
begin
  if LibDcc = 0 then
  {$IFDEF CMD_COMPILER}
    LibDcc := GetModuleHandle(nil);
  {$ELSE}
    LibDcc := LoadLibrary(dcc_dll);
  {$ENDIF CMD_COMPILER}
  Result := LibDcc;
end;

function DbgStrictGetProcAddress(Handle: THandle; Name: PAnsiChar): Pointer;
{$IFDEF DEBUG}
var
  FileName: array[0..MAX_PATH] of Char;
{$ENDIF DEBUG}
begin
  Result := GetProcAddress(Handle, Name);
  {$IFDEF DEBUG}
  {$WARNINGS OFF}
  if (Result = nil) and (DebugHook <> 0) then
  {$WARNINGS ON}
  begin
    FileName[GetModuleFileName(Handle, FileName, Length(FileName))] := #0;
    Assert(False, 'External Symbol "' + string(AnsiString(Name)) + '" not found in "' + FileName + '"');
  end;
  {$ENDIF DEBUG}
end;

type
  TDCCVersion = packed record
    case Integer of
      0: (Build, Release, Minor, Major: Word);
      1: (Value: UInt64);
      2: (LS, MS: Cardinal);
  end;

var
  DCCVersion: TDCCVersion;
  DCCVersionInitialized: Boolean = False;

procedure InitDCCVersion;
var
  FileName: array[0..MAX_PATH] of Char;
  H: DWORD;
  Size: DWORD;
  Version: PVSFixedFileInfo;
  Buffer: array of Byte;
begin
  DCCVersionInitialized := True;
  DCCVersion.Major := Trunc(CompilerVersion);
  DCCVersion.Minor := Trunc(Frac(CompilerVersion) * 10);
  DCCVersion.Release := 0;
  DCCVersion.Build := 0;

  GetModuleFileName(GetDccHandle, FileName, Length(FileName));

  Size := GetFileVersionInfoSize(FileName, H);
  if Size <> 0 then
  begin
    SetLength(Buffer, Size);
    if GetFileVersionInfo(FileName, H, Size, @Buffer[0]) then
    begin
      Size := SizeOf(Version);
      if VerQueryValue(@Buffer[0], '\', Pointer(Version), Size) then
      begin
        DCCVersion.MS := Version.dwFileVersionMS;
        DCCVersion.LS := Version.dwFileVersionLS;
      end;
    end;
  end;
end;

function SameCompilerVersion(AVersion: Int64; AIgnoreBuild: Boolean = True): Boolean;
var
  V, DCCV: TDCCVersion;
begin
  if not DCCVersionInitialized then
    InitDCCVersion;

  DCCV.Major := DCCVersion.Major;
  DCCV.Minor := DCCVersion.Minor;
  DCCV.Release := DCCVersion.Release;
  DCCV.Build := 0;

  V.Value := AVersion;
  if AIgnoreBuild then
    V.Build := 0
  else
    DCCV.Build := DCCVersion.Build;

  Result := V.Value = DCCV.Value;
end;

function CheckCompilerVersion(AVersion: Int64; AIgnoreBuild: Boolean = True): Boolean;
var
  V, DCCV: TDCCVersion;
begin
  if not DCCVersionInitialized then
    InitDCCVersion;

  DCCV.Major := DCCVersion.Major;
  DCCV.Minor := DCCVersion.Minor;
  DCCV.Release := DCCVersion.Release;
  DCCV.Build := 0;

  V.Value := AVersion;
  if AIgnoreBuild then
    V.Build := 0
  else
    DCCV.Build := DCCVersion.Build;

  Result := V.Value <= DCCV.Value;
end;

function CheckCompilerVersion(AMajor, AMinor, ARelease: Integer; ABuild: Integer = -1): Boolean;
var
  V, DCCV: TDCCVersion;
begin
  if not DCCVersionInitialized then
    InitDCCVersion;

  DCCV.Major := DCCVersion.Major;
  DCCV.Minor := DCCVersion.Minor;
  DCCV.Release := 0;
  DCCV.Build := 0;

  V.Major := AMajor;
  V.Minor := AMinor;
  V.Release := 0;
  V.Build := 0;

  if ARelease <> -1 then
  begin
    DCCV.Release := DCCVersion.Release;
    V.Release := ARelease;
    if ABuild <> -1 then
    begin
      DCCV.Build := DCCVersion.Build;
      V.Build := ABuild;
    end;
  end;

  Result := V.Value <= DCCV.Value;
end;

end.
