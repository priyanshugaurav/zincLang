# Set minimum required CMake version and project name
cmake_minimum_required(VERSION 3.18)
project(ZincLang VERSION 0.1 LANGUAGES CXX)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find LLVM package
find_package(LLVM REQUIRED CONFIG)

# If LLVM isn't found, show an error
if(NOT LLVM_FOUND)
    message(FATAL_ERROR "LLVM not found!")
endif()

# Display LLVM configuration information for debugging
message(STATUS "LLVM found at ${LLVM_DIR}")

# Include LLVM headers
include_directories(${LLVM_INCLUDE_DIRS})

# Set the necessary LLVM libraries
link_directories(${LLVM_LIBRARY_DIRS})

# Add any necessary compiler flags for LLVM
add_definitions(${LLVM_DEFINITIONS})

# Include directories
include_directories(src)

# Source files
file(GLOB_RECURSE SOURCES
    src/*.cpp
    src/*/*.cpp
)

# Library for compiler logic
add_library(zincLib
    src/lexer/lexer.cpp
    src/parser/parser.cpp
    src/codegen/codegen.cpp
    src/sema/environment.cpp
    src/sema/typechecker.cpp
    src/runtime/builtins.cpp
    src/ir/ir.cpp
)

# Main executable
add_executable(zinc_exec src/main.cpp)
target_link_libraries(zinc_exec PRIVATE zincLib LLVM)

# Optional: warnings
if (MSVC)
    target_compile_options(zinc_exec PRIVATE /W4 /WX)
else()
    target_compile_options(zinc_exec PRIVATE -Wall -Wextra -Wpedantic -Werror -Wno-unused-parameter)
endif()


# Enable tests
enable_testing()
add_subdirectory(tests)
