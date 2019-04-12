#!/bin/bash
olddir=$(pwd)
cd "$(dirname "$0")"

if [ -d "build" ]; then
    rm -rf build
fi

./build.sh

cd $olddir
