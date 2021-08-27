# Distributed under the OSI-approved MIT License.  See accompanying
# file LICENSE for details.

#[=======================================================================[.rst:
FindWIX
---------

Find components of ``WIX`` package.

The module defines the following variables:

``WIX_FOUND``
  "True" if the programs ``candle`` and ``light`` wer found

``WIX_ROOT``
  path to the directory containing the ``candle`` program

``WIX_VERSION_STRING``
  version of ``candle``

``WIX_CANDLE``
  path to the ``candle`` program

``WIX_LIGHT``
  path to the ``light`` program

``WIX_DARK``
  path to the ``dark`` program

``WIX_HEAT``
  path to the ``heat`` program

``WIX_INSIGNIA``
  path to the ``insignia`` program

``WIX_LIT``
  path to the ``lit`` program

``WIX_LUX``
  path to the ``lux`` program

``WIX_MELT``
  path to the ``melt`` program

``WIX_NIT``
  path to the ``nit`` program

``WIX_PYRO``
  path to the ``pyro`` program

``WIX_SHINE``
  path to the ``shine`` program

``WIX_SMOKE``
  path to the ``smoke`` program

``WIX_THMVIEWER``
  path to the ``ThmViewer`` program

``WIX_TORCH``
  path to the ``torch`` program

``WIX_WIXCOP``
  path to the ``WixCop`` program

The minimum required version of ``WIX`` can be specified using the
standard CMake syntax, e.g.  :command:`find_package(WIX 2.1.3)`.

Example usage:

.. code-block:: cmake

  find_package(WIX)
#]=======================================================================]

# Original coding
# 2009/02 Petr Pytelka (pyta at lightcomp.cz)
# Retrieved from https://gitlab.kitware.com/cmake/community/-/wikis/contrib/modules/FindWix
#
# WIX homepage has moved to https://wixtoolset.org/
# -- Bob Apthorpe (bob.apthorpe at gmail.com) 20200620
# Cleaned syntax and logic
# -- Bob Apthorpe (bob.apthorpe at gmail.com) 20201012
# Substantially revised and modernized; removeed legacy macros
# -- Bob Apthorpe (bob.apthorpe at gmail.com) 20201020
#
# - Try to find Windows Installer XML
# See http://wix.sourceforge.net
#
# The follwoing variables are optionally searched for defaults
#  WIX_ROOT_DIR:            Base directory of WIX2 tree to use.
#
# The following are set after configuration is done:
# WIX_FOUND
# WIX_ROOT
# WIX_VERSION_STRING
# WIX_CANDLE
# WIX_LIGHT
# WIX_DARK
# WIX_HEAT
# WIX_INSIGNIA
# WIX_LIT
# WIX_LUX
# WIX_MELT
# WIX_NIT
# WIX_PYRO
# WIX_SHINE
# WIX_SMOKE
# WIX_THMVIEWER
# WIX_TORCH
# WIX_WIXCOP

# Typical root dirs of installations, exactly one of them is used
set(WIX_POSSIBLE_BIN_DIRS
    "${WIX_BINARY_DIR}"
    "${WIX_ROOT_DIR}/bin"
    "$ENV{WIX}/bin"
    "$ENV{WIX_ROOT_DIR}/bin"
    "$ENV{ProgramFiles}/Windows Installer XML/bin"
    "$ENV{ProgramFiles}/WiX Toolset v3.11/bin"
)

# WiX functionality requires at least candle.exe and light.exe

find_program(WIX_CANDLE
    NAMES candle
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

if(EXISTS "${WIX_CANDLE}")
    execute_process(COMMAND "${WIX_CANDLE}" -help
        OUTPUT_VARIABLE WIX_CANDLE_OUTPUT_VARIABLE
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET)
    # First line of help output looks like
    # "Windows Installer XML Toolset Compiler version 3.11.2.4516"
    # Remaining lines can be dropped; regex fragment ' *\n.*' trims output
    string(REGEX REPLACE "^Windows.*version ([0-9\\.]+) *\n.*" "\\1"
        WIX_VERSION_STRING "${WIX_CANDLE_OUTPUT_VARIABLE}")
    unset(WIX_CANDLE_OUTPUT_VARIABLE)
endif()

# Lesser tools
find_program(WIX_LIGHT
    NAMES light
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_DARK
    NAMES dark
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_HEAT
    NAMES heat
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_INSIGNIA
    NAMES insignia
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_LIT
    NAMES lit
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_LUX
    NAMES lux
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_MELT
    NAMES melt
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_NIT
    NAMES nit
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_PYRO
    NAMES pyro
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_SHINE
    NAMES shine
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_SMOKE
    NAMES smoke
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_THMVIEWER
    NAMES ThmViewer
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_TORCH
    NAMES torch
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

find_program(WIX_WIXCOP
    NAMES WixCop
    PATHS ${WIX_POSSIBLE_BIN_DIRS}
)

include(FindPackageHandleStandardArgs)
# find_package_handle_standard_args(WIX DEFAULT_MSG
#     WIX_CANDLE WIX_LIGHT)
find_package_handle_standard_args(WIX
    REQUIRED_VARS WIX_CANDLE WIX_LIGHT
    VERSION_VAR WIX_VERSION_STRING)

# Set WiX root directory based on location of candle.exe
if(WIX_FOUND)
    # message(STATUS "WiX version: ${WIX_VERSION_STRING}")
    get_filename_component(WIX_BINARY_DIR_ "${WIX_CANDLE}" DIRECTORY)
    get_filename_component(WIX_ROOT "${WIX_BINARY_DIR_}/.." ABSOLUTE)
endif()

mark_as_advanced(
    WIX_ROOT
    WIX_CANDLE
    WIX_LIGHT
    WIX_DARK
    WIX_HEAT
    WIX_INSIGNIA
    WIX_LIT
    WIX_LUX
    WIX_MELT
    WIX_NIT
    WIX_PYRO
    WIX_SHINE
    WIX_SMOKE
    WIX_THMVIEWER
    WIX_TORCH
    WIX_WIXCOP
)
