CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
PROJECT(cam_kessler)
ENABLE_LANGUAGE(Fortran)

include(CMakeForceCompiler)

SET(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/cmake/modules)

# By default, no verbose output
SET(VERBOSITY 0 CACHE STRING "Verbosity level of output (default: 0)")
# By default, generated caps go in ccpp subdir
SET(CCPP_CAP_FILES "${CMAKE_BINARY_DIR}/ccpp" CACHE
  STRING "Location of CCPP-generated cap files")

#MESSAGE("The NETCDF env. variable is $ENV{NETCDF}")
SET(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} $ENV{NETCDF})

get_filename_component(TEST_ROOT "${CMAKE_SOURCE_DIR}" DIRECTORY)
get_filename_component(CCPP_ROOT "${TEST_ROOT}" DIRECTORY)
SET(CCPP_FRAMEWORK ${CCPP_ROOT}/ccpp-framework)

# Use rpaths on MacOSX
set(CMAKE_MACOSX_RPATH 1)

#------------------------------------------------------------------------------
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    #message(STATUS "Setting build type to 'Debug' as none was specified.")
    #set(CMAKE_BUILD_TYPE Debug CACHE STRING "Choose the type of build." FORCE)
    message(STATUS "Setting build type to 'Release' as none was specified.")
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)

    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                 "MinSizeRel" "RelWithDebInfo")
endif()

ADD_COMPILE_OPTIONS(-O0)

SET(temp_metadata_files temp_set.meta temp_adj.meta)

#this should call the custome FindNetCDF.cmake file in /src/cmake/modules and populate the NETCDF_INCLUDES and NETCDF_LIBRARIES variables with the appropriate values
SET(NETCDF_F90 "YES")
FIND_PACKAGE(NetCDF)

INCLUDE_DIRECTORIES(${NETCDF_INCLUDES})

if (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
#  gfortran
#  MESSAGE("gfortran being used.")
  ADD_COMPILE_OPTIONS(-fcheck=all)
  ADD_COMPILE_OPTIONS(-fbacktrace)
  ADD_COMPILE_OPTIONS(-ffpe-trap=zero)
  ADD_COMPILE_OPTIONS(-finit-real=nan)
  ADD_COMPILE_OPTIONS(-ggdb)
  ADD_COMPILE_OPTIONS(-ffree-line-length-none)
  ADD_COMPILE_OPTIONS(-cpp)
  #set(f_flags -fcheck=all -fbacktrace -ffpe-trap=zero -finit-real=nan -ggdb -ffree-line-length-none -g -cpp)
elseif (${CMAKE_Fortran_COMPILER_ID} MATCHES "Intel")
#  ifort
#  MESSAGE("ifort being used.")
  #ADD_COMPILE_OPTIONS(-check all)
  ADD_COMPILE_OPTIONS(-fpe0)
  ADD_COMPILE_OPTIONS(-warn)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-debug extended)
  ADD_COMPILE_OPTIONS(-fpp)
  #set(f_flags -g -fpe0 -warn -traceback -debug extended -fpp)
elseif (${CMAKE_Fortran_COMPILER_ID} MATCHES "PGI")
#  pgf90
#  MESSAGE("pgf90 being used.")
  ADD_COMPILE_OPTIONS(-g)
  ADD_COMPILE_OPTIONS(-Mipa=noconst)
  ADD_COMPILE_OPTIONS(-traceback)
  ADD_COMPILE_OPTIONS(-Mfree)
  ADD_COMPILE_OPTIONS(-Mfptrap)
  ADD_COMPILE_OPTIONS(-Mpreprocess)
  #set(f_flags -g -Mipa=noconst -traceback -Mfree -Mfptrap -Mpreprocess)
else (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  message (FATAL_ERROR "This program has only been compiled with gfortran, pgf90 and ifort. If another compiler is needed, the appropriate flags must be added in ${CMAKE_SOURCE_DIR}/CMakeLists.txt")
endif (${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")

#------------------------------------------------------------------------------
# CMake Modules
# Set the CMake module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../ccpp-framework/cmake")
#------------------------------------------------------------------------------
# Set OpenMP flags for C/C++/Fortran
if (OPENMP)
  include(detect_openmp)
  detect_openmp()
  set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
  set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
  set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
  message(STATUS "Enable OpenMP support for C/C++/Fortran compiler")
else(OPENMP)
  message (STATUS "Disable OpenMP support for C/C++/Fortran compiler")
endif()

add_library(library ${library_files})

# Run ccpp_capgen
set(CAPGEN_CMD "${CCPP_FRAMEWORK}/scripts/ccpp_capgen.py")
set(MODEL_FILES "${CAM_VERSTRING}_ccpp_files.txt")
list(APPEND CAPGEN_CMD "--host-files")
list(APPEND CAPGEN_CMD "${CAM_SRC}/${MODEL_FILES}")
set(SCHEME_FILES "${SIMPLE_SRC}/simple_schemes.txt,${MICM_CHEM_SRC}/MICM_chemistry_schemes.txt")
list(APPEND CAPGEN_CMD "--scheme-files")
list(APPEND CAPGEN_CMD "${SCHEME_FILES}")
set(SUITES "${SUITES}/suite_cam_kessler_test_simple1.xml,${MICM_CHEM_SUITES}/suite_micm.xml")
list(APPEND CAPGEN_CMD "--suites")
list(APPEND CAPGEN_CMD "${SUITES}")
list(APPEND CAPGEN_CMD "--output-root")
list(APPEND CAPGEN_CMD "${CCPP_CAP_FILES}")
list(APPEND CAPGEN_CMD "--generate-host-cap")
while (VERBOSITY GREATER 0)
  list(APPEND CAPGEN_CMD "--verbose")
  MATH(EXPR VERBOSITY "${VERBOSITY} - 1")
endwhile ()
MESSAGE(STATUS "capgen cmd: ${CAPGEN_CMD}")
EXECUTE_PROCESS(COMMAND ${CAPGEN_CMD} WORKING_DIRECTORY ${CPF_ROOT}
  OUTPUT_VARIABLE CAPGEN_OUT ERROR_VARIABLE CAPGEN_OUT RESULT_VARIABLE RES)
MESSAGE(STATUS "${CAPGEN_OUT}")
if (RES EQUAL 0)
  MESSAGE(STATUS "CCPP cap generation completed")
else(RES EQUAL 0)
  MESSAGE(FATAL_ERROR "CCPP cap generation FAILED: result = ${RES}")
endif(RES EQUAL 0)

# Read the list of files from capfiles.txt and set to CCPP_CAPS
file(STRINGS ${CCPP_CAP_FILES}/capfiles.txt CCPP_CAPS)
add_library(caps ${CCPP_CAPS})
INCLUDE_DIRECTORIES(${CCPP_CAP_FILES})

ADD_SUBDIRECTORY(${SIMPLE} ${CMAKE_BINARY_DIR}/simple)
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR}/simple)

ADD_SUBDIRECTORY(${MICM_CHEM} ${CMAKE_BINARY_DIR}/MICM_chemistry)
INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR}/MICM_chemistry)

ADD_DEPENDENCIES(library base)
ADD_DEPENDENCIES(simple_cam library)
ADD_DEPENDENCIES(micmchem library)
ADD_DEPENDENCIES(caps simple_cam)
ADD_DEPENDENCIES(caps micmchem)

ADD_EXECUTABLE(cam_kessler ${kessler_source_files})
TARGET_LINK_LIBRARIES(cam_kessler caps simple_cam micmchem library base ${NETCDF_LIBRARIES})
set_target_properties(cam_kessler PROPERTIES
                               COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}"
                               LINK_FLAGS "${CMAKE_Fortran_FLAGS}")
