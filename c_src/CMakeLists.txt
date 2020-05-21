##########################################################################
# Project Setup                                                          #
##########################################################################
cmake_minimum_required(VERSION 3.1)
project(ErlangH3 C)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 99)

list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_SOURCE_DIR}/cmake)

IF (APPLE)
  set(CMAKE_MODULE_LINKER_FLAGS "-flat_namespace -undefined suppress")
  set(CMAKE_MACOSX_RPATH 1)
ENDIF()

# Set a default build type if none was specified
set(default_build_type "Release")

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Choose the type of build." FORCE)
endif()


##########################################################################
# Static Analysis                                                        #
##########################################################################
find_program(CPPCHECK NAMES cppcheck)
if(CPPCHECK)
  message(STATUS "Found cppcheck: ${CPPCHECK}")
  set(CMAKE_C_CPPCHECK ${CPPCHECK}
    --enable=all
    --inline-suppr
    --inconclusive
    --quiet
    --std=c${CMAKE_C_STANDARD}
    --suppress=missingInclude
    --template=gcc
    )
endif()


##########################################################################
# NIF                                                                    #
##########################################################################
find_package(Erlang REQUIRED)
find_package(H3 REQUIRED)
add_library(h3 MODULE h3.c)
target_link_libraries(h3
  PRIVATE
  Erlang::Erlang
  H3::H3
  )

install(TARGETS h3 DESTINATION ${CMAKE_SOURCE_DIR}/../priv)
