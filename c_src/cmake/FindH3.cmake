include(ExternalProject)

# Hack to let us declare a not-yet-existing include path below.
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/h3)

# get an uppercase version of the build type, for extracting build_type specific flags
if(CMAKE_BUILD_TYPE)
  string(TOUPPER ${CMAKE_BUILD_TYPE} BUILD_TYPE_UC)
endif()

ExternalProject_Add(external-h3
  PREFIX            ${CMAKE_CURRENT_BINARY_DIR}/external-h3
  GIT_REPOSITORY    http://github.com/uber/h3.git
  GIT_TAG           v3.6.3
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
  )

add_library(H3::H3 STATIC IMPORTED)
set_target_properties(H3::H3
  PROPERTIES
  IMPORTED_LOCATION             ${CMAKE_CURRENT_BINARY_DIR}/lib/libh3.a
  INTERFACE_INCLUDE_DIRECTORIES ${CMAKE_CURRENT_BINARY_DIR}/include/h3
  )
add_dependencies(H3::H3 external-h3)
