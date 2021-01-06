# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindErlang
-------

Finds Erlang libraries.

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported targets, if found:

``Erlang::Erlang``
  Header only interface library suitible for compiling NIFs.

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``Erlang_FOUND``
  True if the system has the Erlang library.

Cache Variables
^^^^^^^^^^^^^^^

The following cache variables may also be set:

``Erlang_ERTS_INCLUDE_DIR``
  Path to Erlang Runtime System headers.

#]=======================================================================]
include(FindPackageHandleStandardArgs)

if(DEFINED ENV{ERTS_INCLUDE_DIR})
  SET(Erlang_ERTS_INCLUDE_DIR $ENV{ERTS_INCLUDE_DIR})
else()
  SET(Erlang_BIN_PATH
    $ENV{ERLANG_HOME}/bin
    /opt/bin
    /sw/bin
    /usr/bin
    /usr/local/bin
    /opt/local/bin
    )

  EXECUTE_PROCESS(
    COMMAND         erl -noshell -eval "io:format(\"~s\", [code:root_dir()])" -s erlang halt
    OUTPUT_VARIABLE Erlang_OTP_ROOT_DIR
    )

  EXECUTE_PROCESS(
    COMMAND         erl -noshell -eval "io:format(\"~s\",[filename:basename(code:lib_dir('erts'))])" -s erlang halt
    OUTPUT_VARIABLE Erlang_ERTS_DIR
    )

  SET(Erlang_ERTS_INCLUDE_DIR ${Erlang_OTP_ROOT_DIR}/${Erlang_ERTS_DIR}/include)
endif()

FIND_PACKAGE_HANDLE_STANDARD_ARGS(
  Erlang
  DEFAULT_MSG
  Erlang_ERTS_INCLUDE_DIR
  )

if(Erlang_FOUND)
  if(NOT TARGET Erlang::Erlang)
    add_library(Erlang::Erlang INTERFACE IMPORTED)
    set_target_properties(Erlang::Erlang
      PROPERTIES
      INTERFACE_INCLUDE_DIRECTORIES ${Erlang_ERTS_INCLUDE_DIR}
      )
  endif()
endif(Erlang_FOUND)
