#!/bin/bash

olddir=$(pwd)
cd "$(dirname "$0")"

if [ ! -d "build" ]; then
    mkdir build
fi

cd build

if [[ "$OSTYPE" == "darwin"* ]]; then
    cmake -GXcode ..
else
    cmake ..
fi

cd ..
cmake --build build

cd $olddir
