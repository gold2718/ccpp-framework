cmake_minimum_required(VERSION 2.8.11)

if(POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
    project(ccpp VERSION 0.1.0)
else(POLICY CMP0048)
    project(ccpp)
    set(PROJECT_VERSION 0.1.0)
    set(PROJECT_VERSION_MAJOR 0)
    set(PROJECT_VERSION_MINOR 1)
    set(PROJECT_VERSION_PATCH 0)
endif(POLICY CMP0048)

if(POLICY CMP0042)
    cmake_policy(SET CMP0042 NEW)
endif(POLICY CMP0042)

#------------------------------------------------------------------------------
# Enable Fortran
enable_language(Fortran)

#------------------------------------------------------------------------------
# Set package definitions
set(PACKAGE "CCPP")
set(AUTHORS  "Timothy Brown" "David Gill" "Dom Heinzeller")
string(TIMESTAMP YEAR "%Y")

#------------------------------------------------------------------------------
# The PGI compiler can not find any cap routines in their library.
# This is due to how it labels subroutines within a modules.
# For example the subroutine b() in the moduel a(), gets named a_b.
# GCC and Intel do NOT do this, it is name simply as b.
if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "PGI")
    message(STATUS "WARNING: PGI compiler is not fully ISO_C compliant; working solution involves a hack pgifix.py")
endif()

#------------------------------------------------------------------------------
# CMake Modules
# Set the CMake module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

#------------------------------------------------------------------------------
# Set OpenMP flags for C/C++/Fortran
if (OPENMP)
  include(detect_openmp)
  detect_openmp()
  set (CMAKE_Fortran_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_Fortran_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
  message(STATUS "Enable OpenMP support for C/C++/Fortran compiler")
else(OPENMP)
  message (STATUS "Disable OpenMP support for C/C++/Fortran compiler")
endif()

#------------------------------------------------------------------------------
# The Fortran compiler/linker flag inserted by cmake to create shared libraries
# with the Intel compiler is deprecated (-i_dynamic), correct here.
# CMAKE_Fortran_COMPILER_ID = {"Intel", "PGI", "GNU", "Clang", "MSVC", ...}
if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Intel")
    string(REPLACE "-i_dynamic" "-shared-intel"
           CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS
           "${CMAKE_SHARED_LIBRARY_CREATE_Fortran_FLAGS}")
    string(REPLACE "-i_dynamic" "-shared-intel"
           CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS
           "${CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS}")
endif()

#------------------------------------------------------------------------------
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to 'Debug' as none was specified.")
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)

    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                 "MinSizeRel" "RelWithDebInfo")
endif()

#------------------------------------------------------------------------------
# By default we want a shared library
option(BUILD_SHARED_LIBS "Build a shared library" ON)

#------------------------------------------------------------------------------
# Enable code coverage
if(CMAKE_COMPILER_IS_GNUCC AND (CMAKE_BUILD_TYPE STREQUAL "Coverage"))
    include(code_coverage)
    list(APPEND LIBS "gcov")
endif()

#------------------------------------------------------------------------------
# Enable testing
enable_testing()

#------------------------------------------------------------------------------
# Add the sub-directories
# Source
add_subdirectory(src)
# Documentation
add_subdirectory(doc)
# All schemes
add_subdirectory(schemes)

#------------------------------------------------------------------------------
# Configure and enable packaging
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Common Community Phyics Package")
set(CPACK_PACKAGE_VENDOR "DTC NCAR")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(CPACK_PACKAGE_INSTALL_DIRECTORY
    "CMake ${CMake_VERSION_MAJOR}.${CMake_VERSION_MINOR}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME
    "${CMAKE_PROJECT_NAME}-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
set(CPACK_SOURCE_GENERATOR "TBZ2")
set(CPACK_GENERATOR "TBZ2")
#set(CPACK_GENERATOR "DEB")
#set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Timothy Brown")

include(CPack)
