@echo off
set olddir=%CD%
cd /D "%~dp0"

if EXIST build (
    rd /s /q build
)

call build.bat

cd /D %olddir%