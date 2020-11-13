@echo off

SET BORLAND=C:\Borland
SET P=%PATH%

::copy source\AppConsts.pas Installer\AppConsts.pas >NUL

echo === Delphi 10 ==============================

SET PATH=%Borland%\BDS\4.0\Bin;C:\Windows\System32
cd source
dcc32 -B -GD CompileInterceptor.dpr
if ERRORLEVEL 1 goto Error1

::cd Installer
::dcc32 -B InstallDelphiSpeedUp10.dpr
::if ERRORLEVEL 1 goto Error1
cd ..


:: ===========================================
goto Leave
:Error1
cd ..
:Error0
pause


:Leave
SET PATH=%P%
SET P=
SET BORLAND=
