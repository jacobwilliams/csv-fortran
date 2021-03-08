# Distributed under the OSI-approved MIT License.  See accompanying
# file LICENSE for details.

#[=======================================================================[.rst:
FindNSIS
---------

Find ``makensis`` executable.

The module defines the following variables:

``NSIS_MAKE``
  path to the ``makensis`` program

``NSIS_VERSION``
  version of ``makensis``

``NSIS_FOUND``
  "True" if the program ``makensis`` was found

The minimum required version of ``NSIS`` can be specified using the
standard CMake syntax, e.g.  :command:`find_package(NSIS 2.1.3)`.

Example usage:

.. code-block:: cmake

  find_package(NSIS)
#]=======================================================================]

# Input:
# Set -D "NSIS_BINARY_DIR:PATH=/Program Files (x86)/NSIS" to specify
# directory containing makensis.exe
set(NSIS_BINARY_DIR "/Program Files (x86)/NSIS"
    CACHE PATH "Directory containing makensis.exe for NSIS packaging")

# CMake does not allow for braces in $ENV{}, so a temporary variable must be used.
set(PROGRAMFILES_X86 "ProgramFiles(x86)")

find_program(NSIS_MAKE
    NAMES makensis
    PATHS ${NSIS_BINARY_DIR} $ENV{PROGRAMFILES}/NSIS $ENV{${PROGRAMFILES_X86}}/NSIS
    DOC "Path to the makensis executable"
)

if(EXISTS "${NSIS_MAKE}")
    execute_process(COMMAND "${NSIS_MAKE}" /VERSION
        OUTPUT_VARIABLE NSIS_MAKE_OUTPUT_VARIABLE
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET)
    # Version string looks like ""
    string(REGEX REPLACE "^.*v([0-9\\.]+)" "\\1" NSIS_VERSION "${NSIS_MAKE_OUTPUT_VARIABLE}")
    unset(NSIS_MAKE_OUTPUT_VARIABLE)
endif()

include(FindPackageHandleStandardArgs)
#simple find_package_handle_standard_args(NSIS DEFAULT_MSG NSIS_MAKE)
find_package_handle_standard_args(NSIS
    REQUIRED_VARS NSIS_MAKE
    VERSION_VAR NSIS_VERSION)

mark_as_advanced(
    NSIS_MAKE
)