.. Building Serialbox Documentation

******************
Building Serialbox
******************

Serialbox relies on `CMake <http://www.cmake.org/>`_, a cross-platform build-generator tool. CMake does not build the project, it generates the files needed by your build tool (GNU make, Visual Studio, etc.) for building Serialbox.

.. _Quick start:

Quick start
===========

To build Serialbox on your favorite flavor of Linux, you need a C++11 toolchain, `CMake <http://www.cmake.org/>`_ and a fairly recent version of `Boost <http://www.boost.org/>`_. We use here the command-line, non-interactive CMake interface.

#. Install all the dependencies, especially Boost and CMake. 

  - `Dependencies - Linux`_
  - `Dependencies - Mac OSX`_ 

#. Clone the repository and create a build directory. Building Serialbox in the source
   directory is not supported. cd to this directory:

   .. code-block:: console

     $ git clone https://github.com/thfabian/serialbox2.git
     $ cd serialbox2
     $ mkdir build
     $ cd build

#. Execute CMake

   .. code-block:: console

     $ cmake ../

   CMake will detect your development environment, perform a series of tests, and generate the files required for building Serialbox. 
   CMake will use default values for all build parameters. See the `Options and variables`_ section for a list of build parameters that you can modify.

#. After CMake has finished running, proceed to use IDE project files, or start
   the build from the build directory:

   .. code-block:: console

     $ cmake --build .

   The ``--build`` option tells ``cmake`` to invoke the underlying build tool (``make``, ``ninja``, ``xcodebuild``, ``msbuild``, etc.)

   The underlying build tool can be invoked directly, of course, but  the ``--build`` option is portable.

#. After Serialbox has finished building, install it from the build directory:

   .. code-block:: console

     $ cmake --build . --target install

   The ``--target`` option with ``install`` parameter in addition to  the ``--build`` option tells ``cmake`` to build the ``install`` target (equivalent to ``make install``).

   It is possible to set a different install prefix at installation time
   by invoking the ``cmake_install.cmake`` script generated in the
   build directory:


Options and variables

 .. code-block:: console

   $ 
     
If you are using Ubuntu 16.04, the following will be sufficient:

 .. code-block:: console

   $ sudo apt-get install cmake libboost-all-dev python3-numpy python3-nose

If you are using Mac OSX with `Homebrew <http://brew.sh/>`_

.. _Options and variables:

Options and variables
---------------------

+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
|  CMake option                           | Default    | Explanation                                                                                                         |
+=========================================+============+=====================================================================================================================+
| ``SERIALBOX_BUILD_SHARED``              | ``ON``     | Build shared libraries.                                                                                             |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_ENABLE_C``                  | ``ON``     | Build C interface of Serialbox ``libSerialboxC``.                                                                   |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_ENABLE_PYTHON``             | ``ON``     | Build Python3 interface of Serialbox (requires ``SERIALBOX_ENABLE_C``).                                             |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_ENABLE_FORTRAN``            | ``OFF``    | Build Fortran interface of Serialbox ``libSerialboxFortran`` (requires ``SERIALBOX_ENABLE_C``).                     |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_LOGGING``                   | ``ON``     | Enable logging (requires Boost.Log).                                                                                |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_ASYNC_API``                 | ``ON``     | Enable the asynchronous API.                                                                                        |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_USE_OPENSSL``               | ``-``      | Use OpenSSL library for fast hash-algorithms. By default the option is ``ON`` if OpenSSL was found.                 |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_USE_NETCDF``                | ``-``      | Use [NetCDF-4] library to build the NetCDF archive backend. By default the option is ``ON`` if NetCDF-4 was found.  |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_EXAMPLES``                  | ``ON``     | Build example exectuables.                                                                                          |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_TESTING``                   | ``OFF``    | Build unittests.                                                                                                    |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_TESTING_girdtools``         | ``OFF``    | Build girdtools unitests and examples.                                                                              |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_TESTING_STELLA``            | ``OFF``    | Build STELLA unitests.                                                                                              |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_TESTING_OLD_SERIALBOX``     | ``OFF``    | Build compatiblity unitests against old [Serialbox (0.1)].                                                          |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_TESTING_DEATH_TESTS``       | ``OFF``    | Run death-tests.                                                                                                    |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_BENCHMARKING``              | ``OFF``    | Build benchmark exectuables.                                                                                        |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_DOCUMENTATION``             | ``OFF``    | Build and install the documentation (requires [doxygen] and [sphinx]).                                              |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_CODE_COVERAGE``             | ``OFF``    | Generate code coverage (requires ``lcov`` and ``gcov``).                                                            |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
| ``SERIALBOX_VERBOSE_WARNINGS``          | ``OFF``    | Enable verbose warnings (``-Wall``).                                                                                |
+-----------------------------------------+------------+---------------------------------------------------------------------------------------------------------------------+
