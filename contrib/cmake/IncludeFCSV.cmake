# Retrieve csv-fortran from external source and include into project
# See https://github.com/jacobwilliams/csv-fortran
#
#!!! Verify all these!
# The following output variables are set by the FCSV subproject
# - FCSV_LIB_NAME    --> LIBFCSV_NAME
# - FCSV_LIBRARY_DIR --> ???
# - FCSV_MODULE_DIR  --> LIBFCSV_FORTRAN_MODULE_DIR
set(FCSV_SOURCE_DIR "${CMAKE_CURRENT_BINARY_DIR}/FCSV-source")

FetchContent_Declare(
    FCSV_external
    GIT_REPOSITORY         https://github.com/jacobwilliams/csv-fortran.git
    GIT_TAG                1.3.1
    SOURCE_DIR             "${FCSV_SOURCE_DIR}"
)

FetchContent_MakeAvailable(FCSV_external)
FetchContent_GetProperties(FCSV_external)
# FetchContent_GetProperties(FCSV_external
#     POPULATED FCSV_external_POPULATED
# )

# To use this recipe, add one of the following include() lines
# to CMakeLists.txt after project():
#     include(IncludeFCSV)
# or
#     include(/path/to/IncludeFCSV.cmake)

# To include the csv_module.mod link the fcsv library to the target
# MyExecutable, add the following directives after
# add_executable(MyExecutable ...):
# -----
#     target_link_libraries(MyExecutable fcsv)
#     target_include_directories(MyExecutable PUBLIC $<TARGET_PROPERTY:fcsv,Fortran_MODULE_DIRECTORY>)
#     add_dependencies(MyExecutable fcsv)
# -----

# set(FCSV_LIB_NAME "${LIBFCSV_NAME}")
# # set(FCSV_LIBRARY_DIR --generator expression giving path of TARGET:${LIBFCSV_NAME}--
# set(FCSV_MODULE_DIR "${LIBFCSV_FORTRAN_MODULE_DIR}")

set(FCSV_FOUND "${FCSV_external_POPULATED}")
# __END__