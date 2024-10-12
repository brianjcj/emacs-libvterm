cmake_minimum_required(VERSION 3.11)
include(ExternalProject)

project(emacs-libvterm C)

if(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD" OR CMAKE_SYSTEM_NAME STREQUAL "OpenBSD" OR CMAKE_SYSTEM_NAME STREQUAL "NetBSD")
   set(LIBVTERM_BUILD_COMMAND "gmake")
else()
   set(LIBVTERM_BUILD_COMMAND "make")
endif()

set(LIBVTERM_DIR libs/libvterm-mirror)
add_library(vterm-module MODULE vterm-module.c utf8.c elisp.c
  libs/log.c/src/log.c

  # ${LIBVTERM_DIR}/src/encoding.c
  # ${LIBVTERM_DIR}/src/keyboard.c
  # ${LIBVTERM_DIR}/src/mouse.c
  # ${LIBVTERM_DIR}/src/parser.c
  # ${LIBVTERM_DIR}/src/pen.c
  # ${LIBVTERM_DIR}/src/screen.c
  # ${LIBVTERM_DIR}/src/state.c
  # ${LIBVTERM_DIR}/src/unicode.c
  # ${LIBVTERM_DIR}/src/vterm.c

)
target_include_directories(vterm-module PUBLIC libs/log.c/src)
target_include_directories(vterm-module PUBLIC ${LIBVTERM_DIR}/include)
# target_include_directories(vterm-module INTERFACE ${LIBVTERM_DIR}/src)

set_target_properties(vterm-module PROPERTIES
  C_STANDARD 99
  C_VISIBILITY_PRESET "hidden"
  POSITION_INDEPENDENT_CODE ON
  PREFIX ""
  # LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}
  )

# Set RelWithDebInfo as default build type
if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "No build type selected, defaulting to RelWithDebInfo")
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Build type (default RelWithDebInfo)" FORCE)
endif()


add_library(vterm STATIC
  ${LIBVTERM_DIR}/src/encoding.c
  ${LIBVTERM_DIR}/src/keyboard.c
  ${LIBVTERM_DIR}/src/mouse.c
  ${LIBVTERM_DIR}/src/parser.c
  ${LIBVTERM_DIR}/src/pen.c
  ${LIBVTERM_DIR}/src/screen.c
  ${LIBVTERM_DIR}/src/state.c
  ${LIBVTERM_DIR}/src/unicode.c
  ${LIBVTERM_DIR}/src/vterm.c
)
target_include_directories(vterm PUBLIC libs/log.c/src)
target_include_directories(vterm PUBLIC ${LIBVTERM_DIR}/include)
target_include_directories(vterm INTERFACE ${LIBVTERM_DIR}/src)
set_property(TARGET vterm PROPERTY POSITION_INDEPENDENT_CODE ON)

# Link with libvterm
target_link_libraries(vterm-module PUBLIC vterm)

# Custom run command for testing
add_custom_target(run
  COMMAND emacs -Q -L ${CMAKE_SOURCE_DIR} -L ${CMAKE_BINARY_DIR} --eval "\\(require \\'vterm\\)" --eval "\\(vterm\\)"
  DEPENDS vterm-module
  )
