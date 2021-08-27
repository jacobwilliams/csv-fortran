# Detect available compiler options
include(CheckFortranCompilerFlag)

# Set variable name fcopt_name to $fc_flag and fcopt_allowed to 1 (True)
# if $fc_flag is a legal, quiet option to the Fortran compiler
function(set_fcopt fcopt_allowed fcopt_name fc_flag)
    check_fortran_compiler_flag("${fc_flag}" ${fcopt_allowed})
    if(${${fcopt_allowed}})
        set(${fcopt_name} "${fc_flag}" PARENT_SCOPE)
    else()
        set(${fcopt_name} "" PARENT_SCOPE)
    endif()
endfunction()

# Set option flag visibility and values
set_fcopt(FC_ALLOWS_NO_OPTIMIZATION FCOPT_NO_OPTIMIZATION "-O0")
set_fcopt(FC_ALLOWS_DEBUG_OPTIMIZATION FCOPT_DEBUG_OPTIMIZATION "-Og")
# set_fcopt(FC_ALLOWS_STD_LEGACY FCOPT_STD_LEGACY "--std=legacy")
set_fcopt(FC_ALLOWS_WALL FCOPT_WALL "-Wall")
set_fcopt(FC_ALLOWS_BACKTRACE FCOPT_BACKTRACE "-fbacktrace")
set_fcopt(FC_ALLOWS_DEBUG FCOPT_DEBUG "-g")
set_fcopt(FC_ALLOWS_SAVE FCOPT_SAVE "-fno-automatic")
set_fcopt(FC_ALLOWS_FCHECKALL FCOPT_FCHECKALL "-fcheck=all")

set_fcopt(FC_ALLOWS_STD_F2008 FCOPT_STD_F2008 "--std=f2008")
# set_fcopt(FC_ALLOWS_STD_F2018 FCOPT_STD_F2018 "--std=f2018")

# Code coverage options - experimental
set_fcopt(FC_ALLOWS_COVERAGE FCOPT_COVERAGE "--coverage")
set_fcopt(FC_ALLOWS_PROFILE_ARCS FCOPT_PROFILE_ARCS "-fprofile-arcs")
set_fcopt(FC_ALLOWS_TEST_COVERAGE FCOPT_TEST_COVERAGE "-ftest-coverage")

# Add Fortran_MODULE_DIRECTORY for each Fortran library included in a
# target
function(link_fortran_libraries my_target)
    target_link_libraries(${my_target} ${ARGN})
    foreach(f_lib IN LISTS ARGN)
        target_include_directories(${my_target} PUBLIC $<TARGET_PROPERTY:${f_lib},Fortran_MODULE_DIRECTORY>)
    endforeach()
endfunction()