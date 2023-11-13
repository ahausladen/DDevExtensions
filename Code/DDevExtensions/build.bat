@echo off
SETLOCAL

:: *************************
:: * rebuild with Delphi 2009 (much smaller file). XE2 is required for the .res file
REM Set BuildInstallerWith="C:\CodeGear\RAD Studio\6.0\bin\rsvars.bat"
Set BuildInstallerWith="C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat"

SET curdir=%CD%
cd /d "%~dp0"

if NOT "%fileversion%#" == "#" goto HASVERSION
:: *************************
:: * Adjust version number
call version.bat
SET fileversion=%majorversion%%minorversion%
SET version=%majorversion%.%minorversion%
SET versioninfo=%version%
SET ReleaseDir=%~dp0\Release\%version%

:: *************************
:HASVERSION
echo Version    : %version%
echo FileVersion: %fileversion%
echo Major.Minor: %majorversion%.%minorversion%
echo VersionInfo: %versioninfo%
echo.

echo VersionNumber = '%versioninfo%'; >Source\version.inc
echo #define VER_PRODUCTVERSION          %majorversion%,%minorversion%,0,0 >version.h
echo #define VER_PRODUCTVERSION_STR      "%majorversion%.%minorversion%\0" >>version.h
cgrc Version.rc -foVersion.res
del version.h

:: **********************************************************************************************

SET LINKMAPFILE=..\..\Tools\LinkMapFile\linkmapfile.exe

:: Delete intermediate files
del /Q /S D_2009\lib\*.dcu >NUL
del /Q /S D_2010\lib\*.dcu >NUL
del /Q /S D_XE\lib\*.dcu >NUL
del /Q /S D_XE2\lib\*.dcu >NUL
del /Q /S D_XE3\lib\*.dcu >NUL
del /Q /S D_XE4\lib\*.dcu >NUL
del /Q /S D_XE5\lib\*.dcu >NUL
del /Q /S D_XE6\lib\*.dcu >NUL
del /Q /S D_XE7\lib\*.dcu >NUL
del /Q /S D_XE8\lib\*.dcu >NUL
del /Q /S D_D10\lib\*.dcu >NUL
del /Q /S D_D101\lib\*.dcu >NUL
del /Q /S D_D102\lib\*.dcu >NUL
del /Q /S D_D103\lib\*.dcu >NUL
del /Q /S D_D104\lib\*.dcu >NUL
del /Q /S D_D110\lib\*.dcu >NUL

if "%1-" == "clean-" goto :EOF

echo.
echo === Installer ==============================
cd Installer

:: Use XE2 to create the .res file
::call "C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\rsvars.bat"
::msbuild /nologo /t:Build /p:Config=Release DDevExtensionsReg.dproj
::if ERRORLEVEL 1 goto Error1

:: rebuild with Delphi 2009 (much smaller file). XE2 is required for the .res file
call %BuildInstallerWith%
msbuild /nologo /t:Build /p:Config=Release DDevExtensionsReg.dproj
if ERRORLEVEL 1 goto Error1

cd ..
del bin\DDevExtensionsReg.map bin\DDevExtensionsReg.drc
echo.

echo.
echo === Delphi 11.0 ==============================
call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"

cd D_D110
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsD110.dll
del bin\DDevExtensionsD110.map bin\DDevExtensions.drc
echo.

echo.
echo === Delphi 10.4 ==============================
call "C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat"

cd D_D104
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsD104.dll
del bin\DDevExtensionsD104.map bin\DDevExtensions.drc
echo.

echo.
echo === Delphi 10.3 ==============================
call "C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\rsvars.bat"

cd D_D103
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsD103.dll
del bin\DDevExtensionsD103.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi 10.2 ==============================
call "C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\rsvars.bat"

cd D_D102
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsD102.dll
del bin\DDevExtensionsD102.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi 10.1 Berlin ==============================
call "C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\rsvars.bat"

cd D_D101
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsD101.dll
del bin\DDevExtensionsD101.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi 10 Seattle ==============================
call "C:\Program Files (x86)\Embarcadero\Studio\17.0\bin\rsvars.bat"

cd D_D10
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsD10.dll
del bin\DDevExtensionsD10.map bin\DDevExtensions.drc
echo.

echo.
echo === Delphi XE8 ==============================
call "C:\Program Files (x86)\Embarcadero\Studio\16.0\bin\rsvars.bat"

cd D_XE8
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsXE8.dll
del bin\DDevExtensionsXE8.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi XE7 ==============================
call "C:\Program Files (x86)\Embarcadero\Studio\15.0\bin\rsvars.bat"

cd D_XE7
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsXE7.dll
del bin\DDevExtensionsXE7.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi XE6 ==============================
call "C:\Program Files (x86)\Embarcadero\Studio\14.0\bin\rsvars.bat"

cd D_XE6
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsXE6.dll
del bin\DDevExtensionsXE6.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi XE5 ==============================
call "C:\Program Files (x86)\Embarcadero\RAD Studio\12.0\bin\rsvars.bat"

cd D_XE5
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsXE5.dll
del bin\DDevExtensionsXE5.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi XE4 ==============================
call "C:\Program Files (x86)\Embarcadero\RAD Studio\11.0\bin\rsvars.bat"

cd D_XE4
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsXE4.dll
del bin\DDevExtensionsXE4.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi XE3 ==============================
call "C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin\rsvars.bat"

cd D_XE3
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsXE3.dll
del bin\DDevExtensionsXE3.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi XE2 ==============================
call "C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\rsvars.bat"

cd D_XE2
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsXE2.dll
del bin\DDevExtensionsXE2.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi XE ==============================
call "C:\Program Files (x86)\Embarcadero\RAD Studio\8.0\bin\rsvars.bat"

cd D_XE
msbuild /nologo /t:Build /p:Config=Release DDevExtensions.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensionsXE.dll
del bin\DDevExtensionsXE.map bin\DDevExtensions.drc
echo.


echo.
echo === Delphi 2010 ============================
call "C:\Program Files (x86)\Embarcadero\RAD Studio\7.0\bin\rsvars.bat"

cd D_2010
msbuild /nologo /t:Build /p:Config=Release DDevExtensions2010.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensions2010.dll
del bin\DDevExtensions2010.map bin\DDevExtensions2010.drc
echo.


echo.
echo === Delphi 2009 ============================
call "C:\CodeGear\RAD Studio\6.0\bin\rsvars.bat"

cd D_2009
msbuild /nologo /t:Build /p:Config=Release DDevExtensions2009.dproj
if ERRORLEVEL 1 goto Error1
cd ..
if exist "%LINKMAPFILE%" "%LINKMAPFILE%" bin\DDevExtensions2009.dll
del bin\DDevExtensions2009.map bin\DDevExtensions2009.drc
echo.


echo === Packaging ==============================

echo DDevExtensions Version %majorversion%.%minorversion%>bin\Version.txt
if "%fileversion%#" == "Dev#" echo %versioninfo%>>bin\Version.txt

:: Delete old files
del "%ReleaseDir%\DDevExtensions*.*" /Q 2>NUL
md "%ReleaseDir%" 2>NUL

cd bin
SET FILENAME=..\DDevExtensions

del "%FILENAME%.7z" 2>NUL >NUL
"C:\Program Files\7-Zip\7z.exe" a -y "%FILENAME%.7z" *.dll *.txt *.exe
if ERRORLEVEL 1 GOTO Error1

move "%FILENAME%.7z" "%ReleaseDir%\DDevExtensions%fileversion%.7z"
cd ..

if "%fileversion%#" == "Dev#"  copy /Y ChangeLog.txt "%ReleaseDir%\Changelog.txt"

:: ===========================================
goto Leave
:Error1
cd ..
:Error0
pause


:Leave
SET FILENAME=
SET LINKMAPFILE=
cd /d "%curdir%"

ENDLOCAL
