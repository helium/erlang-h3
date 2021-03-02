include(ExternalProject)

# Hack to let us declare a not-yet-existing include path below.
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/h3)

# get an uppercase version of the build type, for extracting build_type specific flags
if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILD_TYPE_UC)
endif()

# Use this repo's semantic version as the tag to fetch from up
# upstream uber/h3
find_package(Git REQUIRED)
execute_process(COMMAND
  ${GIT_EXECUTABLE} describe --tags
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  RESULT_VARIABLE GIT_SUCCESS
  OUTPUT_VARIABLE ERLANG_H3_TAG
  )
if(NOT GIT_SUCCESS EQUAL "0")
  message(WARNING "could not get erlang-h3 tag, falling back on hard-coded version")
  set(ERLANG_H3_TAG "v3.6.4")
endif()
string(REGEX REPLACE "\n$" "" ERLANG_H3_TAG "${ERLANG_H3_TAG}")
string(REGEX MATCH "^v[0-9]+\.[0-9]+\.[0-9]+" UPSTREAM_H3_GIT_TAG "${ERLANG_H3_TAG}")
message(STATUS "Using upstream h3 tag ${UPSTREAM_H3_GIT_TAG} based on local tag ${ERLANG_H3_TAG}")

ExternalProject_Add(external-h3
  PREFIX            ${CMAKE_CURRENT_BINARY_DIR}/external-h3
  GIT_REPOSITORY    http://github.com/uber/h3.git
  GIT_TAG           ${UPSTREAM_H3_GIT_TAG}
  BUILD_IN_SOURCE   0
  CMAKE_ARGS        -DBUILD_BENCHMARKS=OFF
                    -DBUILD_FILTERS=OFF
                    -DBUILD_GENERATORS=OFF
                    -DBUILD_TESTING=OFF
                    -DENABLE_COVERAGE=OFF
                    -DENABLE_COVERAGE=OFF
                    -DENABLE_DOCS=OFF
                    -DENABLE_FORMAT=OFF
                    -DENABLE_LINTING=OFF
                    -DCMAKE_POSITION_INDEPENDENT_CODE=ON
                    -DCMAKE_INSTALL_PREFIX=${CMAKE_CURRENT_BINARY_DIR}
                    -DCMAKE_C_FLAGS=${CMAKE_C_FLAGS_${BUILD_TYPE_UC}}
                    -DCMAKE_EXE_LINKER_FLAGS=${CMAKE_EXE_LINKER_FLAGS_${BUILD_TYPE_UC}}
                    -DCMAKE_TOOLCHAIN_FILE=$ENV{CMAKE_TOOLCHAIN_FILE}
                    -DH3_ALLOC_PREFIX=libh3_
                    )

add_library(H3::H3 STATIC IMPORTED)
set_target_properties(H3::H3
  PROPERTIES
  IMPORTED_LOCATION             ${CMAKE_CURRENT_BINARY_DIR}/lib/libh3.a
  INTERFACE_INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR}/include/h3
  )
add_dependencies(H3::H3 external-h3)
