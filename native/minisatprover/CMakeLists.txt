# The MiniSat prover (MiniSat with UnSAT cores) as a JNI library for Alloy 6.2.0.
# Alloy's own build of this library has no optimization, which makes it several
# times slower (https://github.com/AlloyTools/org.alloytools.alloy/issues/352).
# The solver sources are cloned into `repo` by the Makefile.
cmake_minimum_required(VERSION 3.20 FATAL_ERROR)

project(minisatprover CXX)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

# The solver uses dynamic exception specifications, which C++17 removed.
set(CMAKE_CXX_STANDARD 14)

find_package(JNI REQUIRED)

add_library(libminisatprover SHARED
    MiniSatProver.cpp
    repo/File.C
    repo/Proof.C
    repo/Solver.C)

target_include_directories(libminisatprover PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/repo
    ${CMAKE_CURRENT_SOURCE_DIR}/repo/ADTs
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${JNI_INCLUDE_DIRS})

target_compile_options(libminisatprover PRIVATE -Wno-deprecated-declarations -Wno-deprecated)

set_target_properties(libminisatprover PROPERTIES OUTPUT_NAME "minisatprover")
