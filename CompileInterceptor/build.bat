@echo off

SETLOCAL

cd Source

call "C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\rsvars.bat"
msbuild /nologo /t:Build /p:Configuration=Release CompileInterceptorW.dproj
if ERRORLEVEL 1 goto Error1

:: rebuild with Delphi 2009 (much smaller file). XE2 is required for the .res file
call "C:\CodeGear\RAD Studio\6.0\bin\rsvars.bat"
msbuild /nologo /t:Build /p:Configuration=Release CompileInterceptorW.dproj
if ERRORLEVEL 1 goto Error1

cd ..


:: ===========================================
goto Leave
:Error1
cd ..
:Error0
pause


:Leave
ENDLOCAL