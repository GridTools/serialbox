**************************************
Using Serialbox as an external library
**************************************

This section provides a brief introduction on how to use Serialbox as an external library using the CMake modules of Serialbox.

.. _FindSerialbox:

CMake modules
-------------

There are several CMake modules located in ``install/cmake/``. To assist in the loading of the settings of the Serialbox project (e.g include and link directories), Serialbox provides a `find_package <https://cmake.org/cmake/help/v3.0/command/find_package.html>`_ module. In addition, to run the preprocessor script ``pp_ser.py`` a tooling module is provided. For a working example, see ``examples/fortran/perturbation``.

FindSerialbox
^^^^^^^^^^^^^

Try to find Serialbox headers and libraries.

Use this module by invoking find_package with the form::

  find_package(Serialbox 
    [version]                  # Minimum version e.g. 2.0
    [REQUIRED]                 # Fail with error if Serialbox is not found
    [COMPONENTS <languages>]   # Components of Serialbox i.e the languages: C++, C, Fortran. C++ is always ON.
  )

Example to find Serialbox headers and `shared` libraries for the C, C++ and Fortran interfaces:: 
   
  set(SERIALBOX_USE_SHARED_LIBS ON)
  find_package(Serialbox REQUIRED COMPONENTS C++ C Fortran)

The Serialbox module will look for the `exact` boost version used during compilation and append 
the necessary libraries to the ``SERIALBOX_[LANGUAGE]_LIBRARIES`` variable. If Serialbox was 
compiled with OpenSSL and/or NetCDF support, the necessary libraries will be appended as well. 

Variables used by this module, they can change the default behaviour and need to be set before 
calling find_package::

  SERIALBOX_ROOT                  - Set this variable to the root installation of Serialbox if the 
                                    module has problems finding the proper installation path.
  SERIALBOX_USE_SHARED_LIBS       - Use the shared libraries (.so or .dylib) of Serialbox.
  SERIALBOX_NO_EXTERNAL_LIBS      - Don't look for external libraries (Boost, NetCDF and OpenSSL)

Variables defined by this module::

  SERIALBOX_FOUND                 - True if headers and requested libraries were found
  SERIALBOX_VERSION               - Version string of Serialbox (e.g "2.0.1")
  SERIALBOX_INCLUDE_DIRS          - The location of the Serialbox headers (i.e to include the 
                                    C Interface ${SERIALBOX_INCLUDE_DIRS}/serialbox-c/Serialbox.h)
                                    and possibly the boost headers.
  SERIALBOX_LIBRARY_DIR           - The location of the Serialbox libraries and Fortran mod files.
  SERIALBOX_HAS_C                 - Serialbox was compiled with C support 
  SERIALBOX_HAS_FORTRAN           - Serialbox was compiled with Fortran support
  SERIALBOX_CXX_LIBRARIES         - The C++ libraries of Serialbox (libSerialboxCore) and 
                                    possibly the external libraries.
  SERIALBOX_C_LIBRARIES           - The C libraries of Serialbox (libSerialboxC) and 
                                    possibly the external libraries.
  SERIALBOX_FORTRAN_LIBRARIES     - The Fortran libraries of Serialbox (libSerialboxFortran) and 
                                    possibly the external libraries.
  SERIALBOX_PPSER                 - Path to the pp_ser.py script.
  SERIALBOX_BOOST_VERSION         - Boost version used during compilation.
  SERIALBOX_HAS_OPENSSL           - Serialbox was compiled with OpenSSL support.
  SERIALBOX_HAS_NETCDF            - Serialbox was compiled with NetCDF support.


SerialboxTooling
^^^^^^^^^^^^^^^^

This module contains the ``serialbox_run_pp_ser`` function which is used to preprocess Fortran
source code using the pp_ser.py script.

Function arguments::

  SOURCES       - Sources to preprocess
  OUTPUT_DIR    - Output directory of the the source files. If nothing is specified ${CMAKE_BINARY_DIR}/pp is used. 

