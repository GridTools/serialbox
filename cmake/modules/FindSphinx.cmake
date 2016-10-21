# Try to find Sphinx.
#
# Usage of this module as follows:
#
#   find_package(Sphinx)
#
# Variables defined by this module:
#
#   SPHINX_FOUND         System has Sphinx
#   STELLA_EXECUTABLE    The location sphinx-build
#

include(FindPackageHandleStandardArgs)

find_program(SPHINX_EXECUTABLE NAMES sphinx-build 
             HINTS $ENV{SPHINX_DIR} PATH_SUFFIXES bin
             DOC "Sphinx documentation generator"
)
 
find_package_handle_standard_args(Sphinx DEFAULT_MSG
    SPHINX_EXECUTABLE
)

mark_as_advanced(SPHINX_EXECUTABLE)

