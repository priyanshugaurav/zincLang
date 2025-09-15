cmake_minimum_required(VERSION 3.18)
project(ZincLang VERSION 0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

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
    src/ir/ir_builder.cpp
    # Add other core files here (AST, sema, utils)
)

target_include_directories(zincLib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src)

# Main executable
add_executable(zinc_exec src/main.cpp)
target_link_libraries(zinc_exec PRIVATE zincLib)

# Optional: warnings
if (MSVC)
    target_compile_options(zinc_exec PRIVATE /W4 /WX)
else()
    target_compile_options(zinc_exec PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()

# Enable tests
enable_testing()
add_subdirectory(tests)
