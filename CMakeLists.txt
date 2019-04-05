cmake_minimum_required(VERSION 3.9)
project(mafox)

if(MSVC_VERSION GREATER_EQUAL "1900")
    include(CheckCXXCompilerFlag)
    CHECK_CXX_COMPILER_FLAG("/std:c++latest" _cpp_latest_flag_supported)
    if(_cpp_latest_flag_supported)
        add_compile_options("/std:c++latest")
    else()
        message(FATAL_ERROR "C++17 is required")
    endif()
endif()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Python3 COMPONENTS Interpreter)
if(${Python3_Interpreter_FOUND})
        add_custom_target(
            make_header_only ALL
            COMMAND ${Python3_EXECUTABLE} "script/maheo.py" "include/mafox.h" "-o" "mafox.hpp"
            WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
            COMMENT "Creating release header"
        )
else()
    message(FATAL_ERROR "python3 is not found")
endif()

add_subdirectory(tests)