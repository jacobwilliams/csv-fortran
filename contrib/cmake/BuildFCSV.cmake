if(GIT_FOUND)
    # Retrieve, build, and install csv-fortran (aka FCSV)
    # distribution from GitHub
    set(FCSV_DIST_DIR "${CMAKE_CURRENT_BINARY_DIR}/FCSV-source")

    set(FCSV_LOCAL_INSTALL_DIR "${CMAKE_CURRENT_BINARY_DIR}/FCSV-artifacts")

    # Note: "<INSTALL_DIR>" is interpolated within ExternalProject_Add to
    # FCSV_LOCAL_INSTALL_DIR
    # list(APPEND FCSV_CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>")
    list(APPEND FCSV_CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX:PATH=${FCSV_LOCAL_INSTALL_DIR}")

    ExternalProject_Add(
        FCSV_external
        # Note: Use URL and URL_HASH [SHA512|SHA256|MD5]=4A54C0DE... to
        # download and checksum an archive. Note that URL may refer to a
        # local file, allowing this to work without net access.
        #    GIT_REPOSITORY         https://github.com/jacobwilliams/csv-fortran.git
        #    GIT_TAG                1.2.0
        GIT_REPOSITORY         https://github.com/jacobwilliams/csv-fortran
        GIT_TAG                1.3.1
        SOURCE_DIR             "${FCSV_DIST_DIR}"
        INSTALL_DIR            "${FCSV_LOCAL_INSTALL_DIR}"
        CMAKE_ARGS             ${FCSV_CMAKE_ARGS}
        BUILD_BYPRODUCTS       ${FCSV_LOCAL_INSTALL_DIR}/${CMAKE_STATIC_LIBRARY_PREFIX}fcsv${CMAKE_STATIC_LIBRARY_SUFFIX}
        LOG_BUILD              YES
        USES_TERMINAL_DOWNLOAD YES
        USES_TERMINAL_UPDATE   YES
    )

    # From csv-fortran/CMakeLists.txt:
    # ...
    # # Set default installation paths; should be invoked after setting project language(s)
    # include(GNUInstallDirs)
    # ...
    # # Fortran module files
    # install(FILES "${LIBFCSV_FORTRAN_MODULE_DIR}/csv_module.mod"
    #        DESTINATION finclude)
    # ...

    # Create ${FCSV_LOCAL_INSTALL_DIR}/finclude based on the module install location
    # set in csv-fortran/CMakeLists.txt. Creating this directory avoids a race
    # condition - see https://www.scivision.dev/cmake-fetchcontent-vs-external-project/
    file(MAKE_DIRECTORY ${FCSV_LOCAL_INSTALL_DIR}/finclude)

    # Make the fcsv library available to the current project as an import
    add_library(fcsv STATIC IMPORTED GLOBAL)

    # Set properties on fcsv target to point at the installed library location and
    # the module directory created above. FCSV uses `include(GNUInstallDirs)` which
    # typically installs libraries to ./lib which is why the IMPORTED_LOCATION below
    # uses the path ${FCSV_LOCAL_INSTALL_DIR}/lib
    set_target_properties(fcsv
      PROPERTIES
      IMPORTED_LOCATION ${FCSV_LOCAL_INSTALL_DIR}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}fcsv${CMAKE_STATIC_LIBRARY_SUFFIX}
      INTERFACE_INCLUDE_DIRECTORIES ${FCSV_LOCAL_INSTALL_DIR}/finclude
    )

    # To use this recipe, add one of the following fragments
    # to CMakeLists.txt after project():
    #     find_package(Git)
    #     include(BuildFCSV)
    # or
    #     find_package(Git)
    #     include(/path/to/BuildFCSV.cmake)

    # To include the csv_module.mod link the fcsv library to the target
    # MyExecutable, add the following directives after
    # add_executable(MyExecutable ...):
    # -----
    #     target_link_libraries(MyExecutable fcsv)
    #     target_include_directories(MyExecutable PUBLIC $<TARGET_PROPERTY:fcsv,Fortran_MODULE_DIRECTORY>)
    #     add_dependencies(MyExecutable FCSV_external)
    # -----

else()
    message(STATUS "git not available; using fallback CSV source files")

    set(FCSV_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/contrib/csv-fortran/src")

    # Full path to csv_kinds.f90
    set(FCSV_KINDS_SRC "${FCSV_SOURCE_DIR}/csv_kinds.f90")

    # Full path to csv_parameters.f90
    set(FCSV_PARAMETERS_SRC "${FCSV_SOURCE_DIR}/csv_parameters.f90")

    # Full path to csv_utilities.f90
    set(FCSV_UTILITIES_SRC "${FCSV_SOURCE_DIR}/csv_utilities.f90")

    # Full path to csv_module.F90
    set(FCSV_MODULE_SRC "${FCSV_SOURCE_DIR}/csv_module.F90")

    list(APPEND FCSV_SRC_FILES "${FCSV_KINDS_SRC}" "${FCSV_PARAMETERS_SRC}" "${FCSV_UTILITIES_SRC}" "${FCSV_MODULE_SRC}")
    message(STATUS "Developer Note: Append contents of FCSV_SRC_FILES to list of sources to compile")

    message(STATUS "CSV source files are in ${FCSV_SOURCE_DIR}")

endif()

set(CSV_FOUND ON)
# __END__