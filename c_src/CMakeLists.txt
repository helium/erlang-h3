##########################################################################
# Project Setup                                                          #
##########################################################################
cmake_minimum_required(VERSION 3.3)
project(ErlangH3 C)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 99)

list(INSERT CMAKE_MODULE_PATH 0 ${CMAKE_SOURCE_DIR}/cmake)
include(ASan)

IF (APPLE)
  set(CMAKE_MODULE_LINKER_FLAGS "-flat_namespace -undefined suppress")
  set(CMAKE_MACOSX_RPATH 1)
ENDIF()

#
# Enable sanitization if environment variable is set
#
# Truth Table
#
# +----------------------+--------------------+------------------+
# | SANITIZE_ERLANG_NIFS | SANITIZE_ERLANG_H3 | Enable santizer? |
# +----------------------+--------------------+------------------+
# | undefined            | undefined          |                0 |
# | 1                    | undefined          |                1 |
# | don't care           | 1                  |                1 |
# | don't care           | 0                  |                0 |
# +----------------------+--------------------+------------------+
#
if(DEFINED ENV{SANITIZE_ERLANG_H3})
  if($ENV{SANITIZE_ERLANG_H3})
    set(CMAKE_BUILD_TYPE ASan CACHE STRING "Choose the type of build." FORCE)
  else()
    unset(CMAKE_BUILD_TYPE CACHE)
  endif()
elseif(DEFINED ENV{SANITIZE_ERLANG_NIFS})
  if($ENV{SANITIZE_ERLANG_NIFS})
    set(CMAKE_BUILD_TYPE ASan CACHE STRING "Choose the type of build." FORCE)
  else()
    unset(CMAKE_BUILD_TYPE CACHE)
  endif($ENV{SANITIZE_ERLANG_NIFS})
endif()

#
# Set a default build type if none was specified
#
set(default_build_type "Release")

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Choose the type of build." FORCE)
endif()


##########################################################################
# Static Analysis                                                        #
##########################################################################
if(CMAKE_VERSION VERSION_GREATER 3.9)
  find_program(CPPCHECK
    NAMES cppcheck
    DOC   "Path to cppcheck executable"
    )
  if(CPPCHECK)
    message(STATUS "Found cppcheck: ${CPPCHECK}")
    set(CMAKE_C_CPPCHECK ${CPPCHECK}
      --inline-suppr
      --inconclusive
      --quiet
      --std=c${CMAKE_C_STANDARD}
      --template=gcc
      )
  endif(CPPCHECK)
endif()

if(CMAKE_VERSION VERSION_GREATER 3.5)
  find_program(CLANG_TIDY
    NAMES clang-tidy
    DOC   "Path to clang-tidy executable"
    )
  if(CLANG_TIDY)
    message(STATUS "Found clang-tidy: ${CLANG_TIDY}")
    set(CMAKE_C_CLANG_TIDY "${CLANG_TIDY}")
  endif(CLANG_TIDY)
endif()


##########################################################################
# NIF                                                                    #
##########################################################################
find_package(Erlang REQUIRED)
find_package(H3 REQUIRED)
add_library(h3
  MODULE
  h3.c
  h3_nif_memory.c
  )
target_link_libraries(h3
  PRIVATE
  Erlang::Erlang
  H3::H3
  )

if(NOT ${CMAKE_BUILD_TYPE} MATCHES "ASan")
  target_compile_definitions(h3
    PRIVATE
    H3_NIF_USE_BEAM_ALLOC
    )
endif()

install(TARGETS h3 DESTINATION ${CMAKE_SOURCE_DIR}/../priv)
