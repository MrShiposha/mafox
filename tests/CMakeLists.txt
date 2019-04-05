cmake_minimum_required(VERSION 3.9)
project(mafox-tests)

include_directories(
    ../include
    ../lib
)

file(GLOB TEST_SRC
    "*.h"
    "*.cpp"
)

add_executable(mafox-tests ${TEST_SRC})