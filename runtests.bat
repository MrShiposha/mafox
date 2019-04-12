@echo off
set olddir=%CD%
cd /D "%~dp0"

IF NOT EXIST build/tests/Debug/mafox-tests.exe (
    call rebuild.bat
)

call build.bat
call "build/tests/Debug/mafox-tests"

cd /D %olddir%