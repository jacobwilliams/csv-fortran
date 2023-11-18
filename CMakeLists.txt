# cmake_minimum_required(VERSION 3.6) #maybe ok?
cmake_minimum_required(VERSION 3.17)

# Needed since 3.19.4; not sure why...
cmake_policy(SET CMP0053 NEW)

# Append local CMake module directory
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Set package metadata
set(CPACK_PACKAGE_NAME "csv-fortran")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "3")
set(CPACK_PACKAGE_VERSION_PATCH "1")
set(CPACK_PACKAGE_VERSION_TWEAK "0")
set(CPACK_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}.${CPACK_PACKAGE_VERSION_TWEAK}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "A modern Fortran library for reading and writing CSV (comma-separated value) files.A modern Fortran library for reading and writing CSV (comma-separated value) files.")
# set(CPACK_PACKAGE_VENDOR "")
set(CPACK_PACKAGE_CONTACT "Jacob Williams")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://github.com/jacobwilliams/csv-fortran")
# See https://spdx.org/licenses/
set(SPDX_LICENSE_TAG "BSD-3-Clause")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
# Note: This should not change for the life of the product
# Generated from guidgen.exe
set(CPACK_WIX_UPGRADE_GUID "C4EE52E4-336B-4702-97EA-8F3A192365BF")

# RAA: What is this? Base installation directory for NSIS installer?
set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}")
string(TIMESTAMP BUILD_TIMESTAMP UTC)

# Show your work...
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set project name and language
project(csv-fortran
        LANGUAGES Fortran
        VERSION "${CPACK_PACKAGE_VERSION}")

###############################################################################
## Options ####################################################################
###############################################################################

# Set fortran compiler with environment variable FC or CMake option
# -D CMAKE_Fortran_COMPILER

# WiX installer search directories (root and binary directories)
# Note the quotes around the entire combination of option, type specifier, and value
# -D "WIX_BINARY_DIR:PATH=/Program Files (x86)/WiX Toolset v3.11/bin"
# Only one of these options is necessary; "/bin" is appended to WIX_ROOT_DIR for detecting WiX binaries
# If possible, just set WIX_BINARY_DIR to the directory containing candle.exe and light.exe
set(WIX_BINARY_DIR "/Program Files (x86)/WiX Toolset v3.11/bin" CACHE PATH "Path to WiX binaries")
set(WIX_ROOT_DIR "/Program Files (x86)/WiX Toolset v3.11" CACHE PATH "Path to WiX installation")

###############################################################################
## Dependencies and CMake Modules  ############################################
###############################################################################

# Defined cmake_parse_arguments(). Needed only for CMake 3.4 and earlier
include(CMakeParseArguments)

# Set default installation paths; should be invoked after setting project language(s)
include(GNUInstallDirs)

# CTest setup
# Needed for valgrind (usually only enable_testing() is needed)
include(CTest)
enable_testing()

# Manage Fortran compiler options, library linking and .mod file inclusion
include(FortranHelper)

###############################################################################
## Build ######################################################################
###############################################################################

list(APPEND FCOPTS
    ${FCOPT_WALL}
    ${FCOPT_FCHECKALL}
    ${FCOPT_DEBUG}
    ${FCOPT_BACKTRACE}
)

# Set recent language standard if available
# if(${FC_ALLOWS_STD_F2018})
#     list(APPEND FCOPTS ${FCOPT_STD_F2018})
# else
if(${FC_ALLOWS_STD_F2008})
    list(APPEND FCOPTS ${FCOPT_STD_F2008})
endif()

if(${FC_ALLOWS_NO_OPTIMIZATION})
    list(APPEND FCOPTS ${FCOPT_NO_OPTIMIZATION})
endif()

message(STATUS "Fortran compiler options for csv-fortran set to ${FCOPTS}")

# Target FCSV: csv-fortran library
set(FCSV_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src")

# Define sources for FCSV library
set(FCSV_SOURCES
    "${FCSV_SOURCE_DIR}/csv_kinds.f90"
    "${FCSV_SOURCE_DIR}/csv_parameters.f90"
    "${FCSV_SOURCE_DIR}/csv_utilities.f90"
    "${FCSV_SOURCE_DIR}/csv_module.F90"
)

# Define FCSV library artifact
set(LIBFCSV_NAME fcsv)
set(LIBFCSV_FORTRAN_MODULE_DIR "${CMAKE_CURRENT_BINARY_DIR}/${LIBFCSV_NAME}_include")
file(MAKE_DIRECTORY "${LIBFCSV_FORTRAN_MODULE_DIR}")

add_library(${LIBFCSV_NAME} STATIC ${FCSV_SOURCES})

set_target_properties(
    ${LIBFCSV_NAME}
    PROPERTIES
    Fortran_MODULE_DIRECTORY ${LIBFCSV_FORTRAN_MODULE_DIR}
)

target_compile_options(${LIBFCSV_NAME} PUBLIC ${FCOPTS})

install(TARGETS ${LIBFCSV_NAME})

### Test executables

# Target : csv-fortran library
set(TEST_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/src/tests")

# add_executable(csv_write_test "${TEST_SOURCE_DIR}/csv_write_test.f90")
# target_link_libraries(csv_write_test ${LIBFCSV_NAME})
# target_compile_options(csv_write_test PUBLIC ${FCOPTS})
# install(TARGETS csv_write_test)

# add_executable(csv_read_test "${TEST_SOURCE_DIR}/csv_read_test.f90")
# target_link_libraries(csv_read_test ${LIBFCSV_NAME})
# target_compile_options(csv_read_test PUBLIC ${FCOPTS})
# install(TARGETS csv_read_test)

# add_executable(csv_local_test "${TEST_SOURCE_DIR}/csv_local_test.f90")
# target_link_libraries(csv_local_test ${LIBFCSV_NAME})
# target_compile_options(csv_local_test PUBLIC ${FCOPTS})
# install(TARGETS csv_local_test)

add_executable(csv_test "${TEST_SOURCE_DIR}/csv_test.f90")
target_link_libraries(csv_test ${LIBFCSV_NAME})
target_compile_options(csv_test PUBLIC ${FCOPTS})
install(TARGETS csv_test)

set_target_properties(
#    csv_write_test csv_read_test csv_local_test csv_test
    csv_test
    PROPERTIES
    Fortran_MODULE_DIRECTORY ${LIBFCSV_FORTRAN_MODULE_DIR}
)


###############################################################################
## Testing ####################################################################
###############################################################################
set(TEST_DIR "${CMAKE_CURRENT_BINARY_DIR}/test")
# ***** Pre-Test File and Directory Generation *****

# Create test output as separate target folded into `make all`
add_custom_target(fcsv_test_setup
    ALL
    DEPENDS csv_test
    COMMAND ${CMAKE_COMMAND} -E echo "Copying test and reference data"
    COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_CURRENT_SOURCE_DIR}/files" "${TEST_DIR}"
    VERBATIM
    USES_TERMINAL
)

# # Create csv_write_test output as separate target folded into `make all`
# add_custom_target(run_csv_write_test
#     ALL
#     DEPENDS fcsv_test_setup
#     COMMAND ${CMAKE_COMMAND} -E echo "Running csv_write_test"
#     COMMAND $<TARGET_FILE:csv_write_test>
#     BYPRODUCTS test_write.csv
#     WORKING_DIRECTORY "${TEST_DIR}"
#     VERBATIM
#     USES_TERMINAL
# )

# # Create csv_read_test output as separate target folded into `make all`
# add_custom_target(run_csv_read_test
#     ALL
#     DEPENDS run_csv_write_test
#     COMMAND ${CMAKE_COMMAND} -E echo "Running csv_read_test"
#     COMMAND $<TARGET_FILE:csv_read_test> > csv_read_test.out
#     BYPRODUCTS csv_read_test.out
#     WORKING_DIRECTORY "${TEST_DIR}"
#     VERBATIM
#     USES_TERMINAL
# )

# # Create csv_local_test output as separate target folded into `make all`
# add_custom_target(run_csv_local_test
#     ALL
#     DEPENDS fcsv_test_setup
#     COMMAND ${CMAKE_COMMAND} -E echo "Running csv_local_test"
#     COMMAND $<TARGET_FILE:csv_local_test> > csv_test.out
#     BYPRODUCTS csv_test.out test2.csv
#     WORKING_DIRECTORY "${TEST_DIR}"
#     VERBATIM
#     USES_TERMINAL
# )

# Create csv_local_test output as separate target folded into `make all`
add_custom_target(run_csv_local_test
    ALL
    DEPENDS fcsv_test_setup
    COMMAND ${CMAKE_COMMAND} -E echo "Running csv_local_test"
    COMMAND $<TARGET_FILE:csv_test> > csv_test.out
    BYPRODUCTS csv_test.out test2.csv test_write.csv
    WORKING_DIRECTORY "${TEST_DIR}"
    VERBATIM
    USES_TERMINAL
)

# ***** Integral Tests *****

# Tests using CMake's simple built-in "compare_files" command
# Compare output with original reference data

# # Example 1: csv_read_test.out
# add_test(NAME 1_csv_read_test
#     COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol csv_read_test.out csv_read_test.out.ref
#     CONFIGURATIONS Debug Release ""
# )

# Example 2: csv_test.out
add_test(NAME 2_csv_test
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol csv_test.out csv_test.out.ref
    CONFIGURATIONS Debug Release ""
)

# Example 3: test_write.csv
add_test(NAME 3_test_write
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol test_write.csv test_write.csv.ref
    CONFIGURATIONS Debug Release ""
)

# Example 4: test2.csv
add_test(NAME 4_test2
    COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol test2.csv test2.csv.ref
    CONFIGURATIONS Debug Release ""
)

set_tests_properties(
   # 1_csv_read_test
    2_csv_test
    3_test_write
    4_test2
    PROPERTIES
    DEPENDS fcsv_test_setup
    # DEPENDS run_csv_write_test
    # DEPENDS run_csv_read_test
    DEPENDS run_csv_test
    WORKING_DIRECTORY "${TEST_DIR}"
    TIMEOUT 30
)

###############################################################################
## Analysis ###################################################################
###############################################################################

# If valgrind or equivalent is available, memory leak/access tests can be run
# with:
# ctest -T memcheck
#
# Manual memory checks:
# valgrind --leak-check=full --track-origins=yes --show-leak-kinds=all <command [opts] [args]>
# "c:\Program Files (x86)\Dr. Memory\bin64\drmemory.exe" -ignore_kernel <command [opts] [args]>

###############################################################################
## Documentation ##############################################################
###############################################################################

# TODO: Use add_custom_target() to run FORD against source tree then
#       use install() to package resulting documentation

###############################################################################
## Packaging ##################################################################
###############################################################################

# Property data files
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/files/"
    DESTINATION files)

# Documentation files
install(FILES LICENSE README.md
    DESTINATION doc)

# Test files (source files; test data is in ./files)
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/test"
    DESTINATION test
    FILES_MATCHING
    PATTERN *.f90
)

# Fortran module files
install(FILES "${LIBFCSV_FORTRAN_MODULE_DIR}/csv_module.mod"
    DESTINATION finclude)

list(APPEND CPACK_GENERATOR ZIP)

if(WIN32)
    # Set up NSIS
    find_package(NSIS)
    if(NSIS_FOUND)
        # set(CPACK_NSIS_MUI_ICON "${CMAKE_CURRENT_SOURCE_DIR}/img/csv-fortran-icon.ico")
        # set(CPACK_NSIS_MUI_UNIICON "${CMAKE_CURRENT_SOURCE_DIR}/img/csv-fortran-icon.ico")
        set(CPACK_NSIS_INSTALLED_ICON_NAME "Uninstall.exe")
        set(CPACK_NSIS_HELP_LINK "${CPACK_PACKAGE_HOMEPAGE_URL}")
        set(CPACK_NSIS_URL_INFO_ABOUT "${CPACK_PACKAGE_HOMEPAGE_URL}")
        set(CPACK_NSIS_MODIFY_PATH ON)
        set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)

        list(APPEND CPACK_GENERATOR NSIS)
    endif()

    # NuGet
    # TODO: Find a more robust means of detecting whether NuGet is available
    find_program(NUGET_EXECUTABLE nuget)
    if(NUGET_EXECUTABLE)

        install(FILES LICENSE
            RENAME LICENSE.txt
            DESTINATION doc)

        # install(FILES "${CMAKE_CURRENT_SOURCE_DIR}/img/csv-fortran-icon.png"
        #     DESTINATION img)

        # set(CPACK_NUGET_COMPONENT_INSTALL OFF)
        set(CPACK_NUGET_PACKAGE_NAME "${CPACK_PACKAGE_NAME}")
        set(CPACK_NUGET_PACKAGE_VERSION "${CPACK_PACKAGE_VERSION}")
        set(CPACK_NUGET_PACKAGE_DESCRIPTION "A CSV library for Modern Fortran")
        set(CPACK_NUGET_PACKAGE_AUTHORS jacobwilliams)
        set(CPACK_NUGET_PACKAGE_TITLE "${CPACK_PACKAGE_NAME}")
        set(CPACK_NUGET_PACKAGE_OWNERS jacobwilliams)
        set(CPACK_NUGET_PACKAGE_HOMEPAGE_URL "${CPACK_PACKAGE_HOMEPAGE_URL}")
        # LicenseURL is deprecated; how to set License with CMake/CPack?
        # set(CPACK_NUGET_PACKAGE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
        # set(CPACK_NUGET_PACKAGE_LICENSEURL "https://github.com/jacobwilliams/csv-fortran/blob/master/LICENSE")
        set(CPACK_NUGET_PACKAGE_LICENSE_EXPRESSION "${SPDX_LICENSE_TAG}")
        # set(CPACK_NUGET_PACKAGE_LICENSE_FILE_NAME "doc/LICENSE.txt")
        #    A URL for a 64x64 image with transparency background to use as the icon for the package in UI display.
        # set(CPACK_NUGET_PACKAGE_ICONURL "https://example.com/img/csv-fortran-icon.png")
        # set(CPACK_NUGET_PACKAGE_ICON "img/csv-fortran-icon.png")
        set(CPACK_NUGET_PACKAGE_DESCRIPTION_SUMMARY "${CPACK_PACKAGE_DESCRIPTION_SUMMARY}")
        set(CPACK_NUGET_PACKAGE_RELEASE_NOTES "Experimental version for testing NuGet packaging. For information only.")
        set(CPACK_NUGET_PACKAGE_COPYRIGHT "Copyright 2021, Jacob Williams")
        set(CPACK_NUGET_PACKAGE_LANGUAGE "en_US")
        #    A space-delimited list of tags and keywords that describe the package and aid discoverability of packages through search and filtering.
        set(CPACK_NUGET_PACKAGE_TAGS "CSV" "comma separated values" "Fortran")
# 	# Repository info
# 	set(CPACK_NUGET_PACKAGE_REPOSITORY_TYPE "git")
# 	set(CPACK_NUGET_PACKAGE_REPOSITORY_URL "https://github.com/jacobwilliams/csv-fortran.git")
# 	set(CPACK_NUGET_PACKAGE_REPOSITORY_BRANCH "1.2.1")
# 	set(CPACK_NUGET_PACKAGE_REPOSITORY_COMMIT "77a50e78c52<your_hash_here>7bba3bd655b4400b")
        #    A list of package dependencies.
        # set(CPACK_NUGET_PACKAGE_DEPENDENCIES TBD)
        #    A version specification for the particular dependency, where <dependency> is an item of the dependency list (see above) transformed with MAKE_C_IDENTIFIER function of string() command.
        # CPACK_NUGET_PACKAGE_DEPENDENCIES_<dependency>_VERSION
        #
        set(CPACK_NUGET_PACKAGE_DEBUG OFF)

        list(APPEND CPACK_GENERATOR NuGet)
    endif()

    # Set up WIX
    # These config variables are set at the top of CMakeLists.txt
    # set(WIX_BINARY_DIR "/Program Files (x86)/WiX Toolset v3.11/bin" CACHE PATH "Path to WiX binaries")
    # set(WIX_ROOT_DIR "/Program Files (x86)/WiX Toolset v3.11" CACHE PATH "Path to WiX installation")
    find_package(WIX)
    if(WIX_FOUND)
        message(STATUS "WIX was found: WIX_FOUND = ${WIX_FOUND}")
        # Manually tell CPack where find_package() found WIX...
        set(CPACK_WIX_ROOT "${WIX_ROOT}")
        # Manually convert LICENSE to RTF format because WIX/CPack is stupid
        set(WIX_LICENSE_FILE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.rtf")
        set(CPACK_WIX_LICENSE_RTF "${WIX_LICENSE_FILE}")
        install(FILES "${WIX_LICENSE_FILE}" DESTINATION doc)
        # set(CPACK_WIX_PRODUCT_ICON "${CMAKE_CURRENT_SOURCE_DIR}/img/csv-fortran-icon.ico")

        list(APPEND CPACK_GENERATOR WIX)
    else()
        message(STATUS "WIX was not found: WIX_FOUND = ${WIX_FOUND}")
    endif()
else()
    list(APPEND CPACK_GENERATOR TGZ TBZ2)
    if(APPLE)
        # Set up DRAGNDROP
        # Add DragNDrop properties
        set(CPACK_DMG_VOLUME_NAME "${CPACK_PACKAGE_NAME} v${CPACK_PACKAGE_VERSION}")
#        set(CPACK_DMG_FORMAT "UDZO")
        set(CPACK_DMG_FORMAT "UDBZ")
#*       CPACK_DMG_DS_STORE
#*       CPACK_DMG_DS_STORE_SETUP_SCRIPT
#*       CPACK_DMG_BACKGROUND_IMAGE
#        CPACK_DMG_DISABLE_APPLICATIONS_SYMLINK
#        CPACK_DMG_SLA_DIR
#        CPACK_DMG_SLA_LANGUAGES
#        CPACK_DMG_<component>_FILE_NAME
#        CPACK_COMMAND_HDIUTIL
#        CPACK_COMMAND_SETFILE
#        CPACK_COMMAND_REZ
        list(APPEND CPACK_GENERATOR DragNDrop)
    else()
        # Set up DEB
        # TODO: Find a more robust means of detecting whether debian packaging should be enabled
        # Note that readelf is not strictly necessary but platform is assumed
        # Debian-ish if it's present
        find_program(READELF_EXECUTABLE readelf)
        if(READELF_EXECUTABLE)
            set(CPACK_DEBIAN_PACKAGE_DESCRIPTION
"${CPACK_PACKAGE_DESCRIPTION_SUMMARY}")
            # set(CPACK_DEBIAN_PACKAGE_ARCHITECTURE "any")
            # Auto-detect dependencies
            set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)
            # Hack to find internal libraries - see https://gitlab.kitware.com/cmake/cmake/-/issues/17447
            # list(APPEND CMAKE_INSTALL_RPATH
            #     "$ORIGIN/../lib/graphviz"
            # )
            # A better solution is to set LD_LIBRARY_PATH when running CPack or `make package`
            # Exanple: LD_LIBRARY_PATH=./_CPack_Packages/Linux/DEB/Graphviz-2.45.0-Linux/usr/lib/graphviz:${LD_LIBRARY_PATH} make package
            # Exanple: LD_LIBRARY_PATH=./_CPack_Packages/Linux/DEB/Graphviz-2.45.0-Linux/usr/lib/graphviz:${LD_LIBRARY_PATH} cpack -G DEB
            # Build multiple packages
            set(CPACK_DEB_COMPONENT_INSTALL ON)
            set(CPACK_DEBIAN_ENABLE_COMPONENT_DEPENDS ON)
            # set(CPACK_DEBIAN_PACKAGE_DEBUG ON)
            # Turn off for executable-only; only needed for packaging libraries
            set(CPACK_DEBIAN_PACKAGE_GENERATE_SHLIBS OFF)

            list(APPEND CPACK_GENERATOR DEB)
        endif()

        # Set up RPM
        # TODO: Find a more robust means of detecting whether RPM generator is available
        find_program(RPMBUILD_EXECUTABLE rpmbuild)
        if(RPMBUILD_EXECUTABLE)
            # message(STATUS "rpmbuild found at ${RPMBUILD_EXECUTABLE}; needs configuration")

            # Needs additional work (maybe?)
            set(CPACK_RPM_PACKAGE_LICENSE "${SPDX_LICENSE_TAG}")
            set(CPACK_RPM_FILE_NAME RPM-DEFAULT)
            list(APPEND CPACK_GENERATOR RPM)
        endif()
    endif()
endif()

# This must be last
include(CPack)
