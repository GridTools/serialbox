.. Building Serialbox Documentation

******************
Building Serialbox
******************

Serialbox relies on `CMake <http://www.cmake.org/>`_ (>= 3.3), a cross-platform build-generator tool. CMake does not build the project, it generates the files needed by your build tool (GNU make, Visual Studio, etc.) for building Serialbox.

.. contents::
   :local:

.. _Quick start:

Quick start
===========

To build Serialbox you need a C++11 toolchain, `CMake <http://www.cmake.org/>`_ and a fairly recent version of `Boost <http://www.boost.org/>`_. We use here the command-line, non-interactive CMake interface.

#. Make sure you have installed all the tools and dependencies, especially Boost and CMake. 
   See `Dependencies`_. 

#. Clone the repository and create a build directory. Building Serialbox in the source directory is not supported:

   .. code-block:: console

     $ git clone https://github.com/thfabian/serialbox2.git
     $ cd serialbox2
     $ mkdir build
     $ cd build

#. Execute CMake:

   .. code-block:: console

     $ cmake ../

   CMake will detect your development environment, perform a series of tests, and generate the files required for building Serialbox. 
   CMake will use default values for all build parameters. See the `Options and variables`_ section for a list of build parameters that you can modify.

#. After CMake has finished running, proceed to use IDE project files, or start the build from the build directory:

   .. code-block:: console

     $ cmake --build .

   The ``--build`` option tells ``cmake`` to invoke the underlying build tool (``make``, ``ninja``, ``xcodebuild``, ``msbuild``, etc.)

   The underlying build tool can be invoked directly, of course, e.g ``make -j4``.

#. After Serialbox has finished building, install it from the build directory. The files will be installed into the top-level ``install`` directory (i.e ``serialbox2/install``).

   .. code-block:: console

     $ cmake --build . --target install

   The ``--target`` option with ``install`` parameter in addition to  the ``--build`` option tells ``cmake`` to build the ``install`` target (equivalent to ``make install``).
   
.. _Building the unittests:
   
Building the unittests
======================

Serialbox has an extensive test-suite. To build the unitests, proceed the in the same way as described in the `Quick start`_ section with the diffrence of passing ``-DSERIALBOX_TESTING=ON`` to CMake.

.. code-block:: console

   $ cmake -DSERIALBOX_TESTING=ON ../
   
Serialbox has several unittest which use the external library they are built for. For example, to build the unittests for the `gridtools <https://github.com/eth-cscs/gridtools>`_ frontend:

.. code-block:: console

   $ cmake -DSERIALBOX_TESTING=ON -DSERIALBOX_TESTING_GRIDTOOLS=ON -DSERIALBOX_GRIDTOOLS_ROOT=<path-to-gridtools> ../
   
Similarly, you can build the unittest for `STELLA <https://github.com/MeteoSwiss-APN/stella>`_ and the compatible tests with the old `Serialbox <https://github.com/MeteoSwiss-APN/serialbox>`_. If you clone those projects in ``external/``, CMake will automatically find and **build** them. The following will enable all possible unittests:

.. code-block:: console

  $ cd $(git rev-parse --show-toplevel) # Change to top-level directory
  $ git clone git@github.com:eth-cscs/gridtools.git external/gridtools
  $ git clone git@github.com:MeteoSwiss-APN/stella.git external/stella
  $ git clone https://github.com/MeteoSwiss-APN/serialbox external/serialbox
  $ cd build
  $ cmake -DSERIALBOX_TESTING=ON -DSERIALBOX_TESTING_GRIDTOOLS=ON -DSERIALBOX_TESTING_OLD_SERIALBOX=ON -DSERIALBOX_TESTING_STELLA=ON ../

To run the unittests via CTest:

.. code-block:: console

   $ cmake --build . --target test

.. _Dependencies:

Dependencies
============

Serialbox requires a C++11 compatible compiler:

============  =======
Compiler      Version
============  =======
GNU gcc       >= 4.9    
LLVM clang    >= 3.4    
Intel icc     >= 17.0  
XCode         >= 6.1   
============  =======

Serialbox depends on the `Boost <http://www.boost.org/>`_ modules: filesystem and log. Optionally, Serialbox can be compiled with `NetCDF-4 <http://www.unidata.ucar.edu/software/netcdf/>`_ support.

Ubuntu (16.04)
--------------
  
The following will install all the necessary dependencies:

.. code-block:: console

   $ sudo apt-get install cmake libboost-all-dev python3-numpy python3-nose

and the following will furhter install all the optional dependencies:

.. code-block:: console

   $ sudo apt-get install libnetcdf-dev

Mac OSX
-------

Make sure you have the `Xcode Command Line Tools <http://railsapps.github.io/xcode-command-line-tools.html>`_ installed.

.. code-block:: console

  $ xcode-select --install

If you are using `Homebrew <http://brew.sh/>`_, the following will install all the necessary dependencies:

.. code-block:: console

  $ brew update
  $ brew install cmake boost
    
and the following will further install all the optional dependencies:

.. code-block:: console
    
  $ brew tap homebrew/science
  $ brew install netcdf
  
The Python3 module of Serialbox further requires `numpy <http://www.numpy.org/>`_.
    
.. _Options and variables:

Options and variables
=====================

Variables customize how the build will be generated. Options are boolean variables, with possible values ON/OFF. Options and variables are defined on the CMake command line like this:

.. code-block:: console

  $ cmake -DVARIABLE=value ../
  
You can also edit the options and variables with `CCMake <https://cmake.org/cmake/help/v3.0/manual/ccmake.1.html>`_

.. code-block:: console

  $ ccmake .

Frequently-used CMake variables
-------------------------------

Here are some of the CMake variables that are used often, along with a brief explanation and Serialbox-specific notes. For full documentation, consult the
CMake manual, or execute ``cmake --help-variable VARIABLE_NAME``.

**CMAKE_BUILD_TYPE**:STRING
  Sets the build type for ``make``-based generators. Possible values are
  Release, Debug, RelWithDebInfo and MinSizeRel (default is Release).

**CMAKE_INSTALL_PREFIX**:PATH
  Path where Serialbox will be installed if "make install" is invoked or the "install" target is built (default is the top-level ``install`` directory)


Serialbox specific variables
----------------------------

**SERIALBOX_ENABLE_C**:BOOL
  Build the C interface of Serialbox (``libSerialboxC``). The options is ``ON`` by default.

**SERIALBOX_ENABLE_PYTHON**:BOOL
  Build Python3 interface of Serialbox (requires ``SERIALBOX_ENABLE_C=ON``). The options is ``ON`` by default. The module will be installed in ``python/serialbox``.

**SERIALBOX_ENABLE_FORTRAN**:BOOL
  Build the C interface of Serialbox (``libSerialboxFortran``). The options is ``OFF`` by default.
 
**SERIALBOX_EXAMPLES**:BOOL
  Build the example executables in ``examples/``. To build the gridtools examples, ``SERIALBOX_TESTING_GRIDTOOLS=ON`` is required.
  
**SERIALBOX_BUILD_SHARED**:BOOL
  Build shared libraries of Serialbox. This is required for the Python module. The option is ``ON`` by default.
  
**SERIALBOX_LOGGING**:BOOL
  Enable/disable the logging infrastructure. If logging is disabled, `Boost.Log <http://www.boost.org/doc/libs/1_62_0/libs/log/doc/html/index.html>`_ is not **NOT** required anymore. 
  The option is ``ON`` by default.

**SERIALBOX_ASYNC_API**:BOOL
  Enable the asynchronous API. This uses the C++11 STL multitheading infrastructure. The option is ``ON`` by default.

**SERIALBOX_USE_OPENSSL**:BOOL
  Use OpenSSL library for fast hash-algorithms. By default the option is ``ON`` if NetCDF-4 was found.

**SERIALBOX_USE_NETCDF**:BOOL
  Use `NetCDF-4 <http://www.unidata.ucar.edu/software/netcdf/>`_ library to build the NetCDF archive backend. By default the option is ``ON`` if NetCDF-4 was found.

**SERIALBOX_TESTING**:BOOL
  Build the unittests (see `Building the unittests`_)
  
**SERIALBOX_TESTING_GRIDTOOLS**:BOOL
  Build `gridtools <https://github.com/eth-cscs/gridtools>`_ unittests and examples.

**SERIALBOX_TESTING_STELLA**:BOOL
  Build `STELLA <https://github.com/MeteoSwiss-APN/stella>`_ unittests.
  
**SERIALBOX_TESTING_OLD_SERIALBOX**:BOOL
  Build the compatiblity unitests against the old `Serialbox <https://github.com/MeteoSwiss-APN/serialbox>`_.
  
**SERIALBOX_TESTING_DEATH_TESTS**:BOOL
  Compile the death-tests.

**SERIALBOX_BENCHMARKING**:BOOL
  Build the benchmark exectuables.

**SERIALBOX_DOCUMENTATION**:BOOL
  Build and install the documentation (requires `doxygen <http://www.stack.nl/~dimitri/doxygen/>`_ and `sphinx <http://www.sphinx-doc.org/en/1.4.8/>`_).  
  
**SERIALBOX_CODE_COVERAGE**:BOOL
  Generate code coverage (requires `lcov <http://ltp.sourceforge.net/coverage/lcov.php>`_ and `gcov <https://gcc.gnu.org/onlinedocs/gcc/Gcov.html>`_)  
  
**SERIALBOX_VERBOSE_WARNINGS**:BOOL
  Enable verbose warnings (``-Wall``) 

External project specific variables
-----------------------------------

**BOOST_ROOT**:PATH
  Install directory of Boost (see `here <https://cmake.org/cmake/help/v3.0/module/FindBoost.html>`_).
  
**GRIDTOOLS_ROOT**:PATH
  Main directory of `gridtools <https://github.com/eth-cscs/gridtools>`_.

**SERIALBOX_OLD_ROOT**:PATH
  Install directory of old `Serialbox <https://github.com/MeteoSwiss-APN/serialbox>`_.

**STELLA_ROOT**:PATH
  Install directory of `STELLA <https://github.com/MeteoSwiss-APN/stella>`_.

**NETCDF_ROOT**:PATH
  Install directory of `NetCDF-4 <http://www.unidata.ucar.edu/software/netcdf/>`_.


