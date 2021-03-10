# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file LICENSE for details.

#[=======================================================================[.rst:
FindFCSV
---------

Find ``FCSV`` unit testing library for Fortran.

The module defines the following variables:

``FCSV_LIB_NAME``
  ``FCSV`` library base name ('fcsv')

``FCSV_LIBRARY``
  path to the ``FCSV`` library

``FCSV_LIBRARY_DIR``
  path to the ``FCSV`` library directory

``FCSV_MODULE_FILE``
  path to the ``FCSV`` Fortran module (.mod) file

``FCSV_MODULE_DIR``
  path to the ``FCSV`` Fortran module directory

``FCSV_FOUND``
  "True" if the ``FCSV`` library and module files were found

Example usage:

.. code-block:: cmake

  find_package(FCSV)
#]=======================================================================]

set(FCSV_LIB_NAME fcsv)
set(FCSV_FOUND OFF)

# Set FCSV_ROOT and FCSV_MODULE_PATH on the command line:
# The following are defined in the root CMakeLists.txt file
# set(FCSV_ROOT "" CACHE PATH "Installation root of FCSV library")
# set(FCSV_MODULE_PATH "" CACHE PATH "Directory containing FCSV Fortran module (.mod) files")

# BuildFCSV.cmake sets FCSV_ROOT to
# <project_root>\build\FCSV_external-prefix\src\FCSV_external-build

if(IS_DIRECTORY "${FCSV_ROOT}")
    set(SEARCH_FCSV_LIB ${FCSV_ROOT}/lib)
    set(SEARCH_FCSV_MOD ${FCSV_ROOT}/include ${FCSV_ROOT}/module
        ${FCSV_ROOT}/finclude ${FCSV_ROOT}/finclude/fcsv)
endif()

if(IS_DIRECTORY "${FCSV_MODULE_PATH}")
    list(APPEND SEARCH_FCSV_MOD "${FCSV_MODULE_PATH}")
endif()

find_library(FCSV_LIBRARY
    NAMES "${FCSV_LIB_NAME}"
    PATHS ${SEARCH_FCSV_LIB}
)

# message(STATUS "Debug: SEARCH_FCSV_MOD=${SEARCH_FCSV_MOD}")
find_file(FCSV_MODULE_FILE
    NAMES "csv_module.mod" "${FCSV_LIB_NAME}.mod"
    PATHS ${SEARCH_FCSV_MOD}
)

# Set FCSV_FOUND if both FCSV_LIBRARY and FCSV_MODULE_FILE are found
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FCSV DEFAULT_MSG
    FCSV_LIBRARY FCSV_MODULE_FILE)

if(FCSV_FOUND)
    ##### Set Output Variables #####

    # Set the following:
    # - FCSV_LIB_NAME (at top; "fcsv")
    # - FCSV_LIBRARY_DIR
    # - FCSV_MODULE_DIR
    # - FCSV_MODULE_FILE (from find_file())
    get_filename_component(FCSV_LIBRARY_DIR "${FCSV_LIBRARY}" DIRECTORY)
    get_filename_component(FCSV_MODULE_DIR "${FCSV_MODULE_FILE}" DIRECTORY)
    message(STATUS "Found FCSV library under ${FCSV_LIBRARY_DIR}")
else()
    message(STATUS "Cannot find FCSV (is FCSV_ROOT set? '${FCSV_ROOT}')")
endif()

# These variables are set to be compatible with the naming scheme used
# in original FCSV example CMake setup; see
# build/FCSV-source/examples/example1/CMakeLists.txt
# - FCSV_LIB_NAME (= "fcsv")
# - FCSV_LIBRARY_DIR
# - FCSV_MODULE_DIR

# Note: This needs to be manually added to the list of source files
# required for unit tests
# - FCSV_MODULE_FILE
