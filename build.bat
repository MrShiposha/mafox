@echo off

set olddir=%CD%
cd /D "%~dp0"

IF NOT EXIST build (
    mkdir build
)

cd build
cmake ..
cd ..
cmake --build build

cd /D %olddir%