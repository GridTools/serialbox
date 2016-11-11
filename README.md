<img src="docs/logo/logo.png" width="447" height="170" />

#### Table of Contents
* [Introduction](#introduction)
* [Continuous integration](#continuous-integration)
* [Building](#building)
     * [Building on Linux](#building-linux)
     * [Building on Mac OSX](#building-mac)


## Introduction  <a id="introduction"></a>
Serialbox is a serialization library and tools for C/C++, Python3 and Fortran. Serialbox is used in several projects for building validation frameworks against reference runs. This is useful in the scope of rewrite of large codes, or when porting codes to multiple computing architectures. As an example, porting scientific codes to graphical processing units, that require continuous validation against the existing x86 code.


## Continuous integration  <a id="continuous-integration"></a>

Serialbox builds are run on Travis CI [here](https://travis-ci.org/thfabian/serialbox2).

|  Branch |                                                     Linux                                                                 |                                                   Mac OSX                                                               |
|:-------:|:-------------------------------------------------------------------------------------------------------------------------:|:-------------------------------------------------------------------------------------------------------------------------:|
| Master   | [![Build Status](https://travis-ci.org/thfabian/serialbox2.svg?branch=master)](https://travis-ci.org/thfabian/serialbox2) | [![Build Status](https://travis-ci.org/thfabian/serialbox2.svg?branch=master)](https://travis-ci.org/thfabian/serialbox2) |
| Develop | [![Build Status](https://travis-ci.org/thfabian/serialbox2.svg?branch=develop)](https://travis-ci.org/thfabian/serialbox2) | [![Build Status](https://travis-ci.org/thfabian/serialbox2.svg?branch=develop)](https://travis-ci.org/thfabian/serialbox2) |

## Building  <a id="building"></a>

The build can be customized with the following [CMake] options:

|  CMake option                     | Default  | Explanation |
| :----                             |  :----:  | :----       |
| `SERIALBOX_BUILD_SHARED`          | `ON`     | Build shared libraries. |
| `SERIALBOX_ENABLE_C`              | `ON`     | Build C interface of Serialbox `libSerialboxC`. |
| `SERIALBOX_ENABLE_PYTHON`         | `ON`     | Build Python3 interface of Serialbox (requires `SERIALBOX_ENABLE_C`). |
| `SERIALBOX_ENABLE_FORTRAN`        | `OFF`    | Build Fortran interface of Serialbox `libSerialboxFortran` (requires `SERIALBOX_ENABLE_C`). |
| `SERIALBOX_LOGGING`               | `ON`     | Enable logging (requires Boost.Log). |
| `SERIALBOX_ASYNC_API`             | `ON`     | Enable the asynchronous API. |
| `SERIALBOX_USE_OPENSSL`           | `-`      | Use OpenSSL library for fast hash-algorithms. By default the option is `ON` if OpenSSL was found. |
| `SERIALBOX_USE_NETCDF`            | `-`      | Use [NetCDF-4] library to build the NetCDF archive backend. By default the option is `ON` if NetCDF-4 was found. |
| `SERIALBOX_EXAMPLES`              | `ON`     | Build example exectuables. |
| `SERIALBOX_TESTING`               | `OFF`    | Build unittests. |
| `SERIALBOX_TESTING_GRIDTOOLS`     | `OFF`    | Build girdtools unitests and examples. |
| `SERIALBOX_TESTING_STELLA`        | `OFF`    | Build STELLA unitests. |
| `SERIALBOX_TESTING_OLD_SERIALBOX` | `OFF`    | Build compatiblity unitests against old [Serialbox (0.1)]. |
| `SERIALBOX_TESTING_DEATH_TESTS`   | `OFF`    | Run death-tests. |
| `SERIALBOX_BENCHMARKING`          | `OFF`    | Build benchmark exectuables. |
| `SERIALBOX_DOCUMENTATION`         | `OFF`    | Build and install the documentation (requires [doxygen] and [sphinx]). |
| `SERIALBOX_CODE_COVERAGE`         | `OFF`    | Generate code coverage (requires `lcov` and `gcov`). |
| `SERIALBOX_VERBOSE_WARNINGS`      | `OFF`    | Enable verbose warnings (`-Wall`). |

The following variables can be used to hint [CMake] where to find the dependency libraries.

|  CMake option         | Explanation                                                                                                         |
| :----                 | :----                                                                                                               |
|  `BOOST_ROOT`         | Directory of the Boost header and libraries (See [here](https://cmake.org/cmake/help/v3.0/module/FindBoost.html)).  |
|  `SERIALBOX_OLD_ROOT` | Install directory of [Serialbox (0.1)].                            |
|  `STELLA_ROOT`        | Install directory of [STELLA].                                      |
|  `NETCDF_ROOT`        | Install directory of [NetCDF-4].                                      |
|  `GRIDTOOLS_ROOT`     | Main directory of [girdtools].                                      |


### Building on Linux <a id="building-linux"></a>

To build Serialbox on your favorite flavor of Linux, you need a C++11 toolchain, [CMake] and a fairly recent version of [Boost], at least 1.54. For Ubuntu (16.04), the following will install all the dependencies:

```bash
sudo apt-get install cmake libboost-all-dev python3-numpy python3-nose
```

To build and install Serialbox locally (installed in `serialbox2/install`):

```bash
git clone https://github.com/thfabian/serialbox2.git
cd serialbox2
mkdir build && cd build
cmake ../
make -j4
make install
```

The newly build and installed C++ `SerialboxCore`, C `SerialboxC` and Python3 libraries can be found in `serialbox2/install/lib` and `serialbox2/install/python` respectively. 

Serialbox has an extensive test-suite which can be enabled by passing the option `-DSERIALBOX_TESTING=ON` to CMake. To build the unittest for the specific frontend libraries, you need to provide the install locations of [girdtools], [STELLA] and [Serialbox (0.1)]. CMake will automatically pick them up and build them if you check them out in `external/`. Note that the Python3 tests require the module [nose]. To run the unitests, run `make test` in the build dirctory.

To build the documentation you need [doxygen] and [sphinx]. CMake will try to pick up the tools and, if successful, provides a target (e.g `make doc`) to build and install the documentation. To view it, open `docs/serialbox-index.html` or `docs/html/index.html`.

### Building on Mac OSX <a id="building-mac"></a>

This section will walk you through the building process on Mac OSX using [Homebrew]. 

Make sure you have the [Xcode Command Line Tools](http://railsapps.github.io/xcode-command-line-tools.html) installed.

```bash
xcode-select --install
```

Install the dependencies ([Boost] and [CMake]) via Homebrew

```bash
brew update
brew install cmake boost
``` 

Optionally, if you want to build the the NetCDF backend

```bash
brew tap homebrew/science
brew install netcdf
``` 

or the python3 library

```bash
brew install python3
pip3 install numpy nose
``` 

To build and install Serialbox locally (installed in `serialbox2/install`):

```bash
git clone https://github.com/thfabian/serialbox2.git
cd serialbox2
mkdir build && cd build
cmake ../
make -j4
make install
```

The newly build and installed C++ `SerialboxCore`, C `SerialboxC` and Python3 libraries can be found in `serialbox2/install/lib` and `serialbox2/install/python` respectively. 

Serialbox has an extensive test-suite which can be enabled by passing the option `-DSERIALBOX_TESTING=ON` to CMake. To build the unittest for the specific frontend libraries, you need to provide the install locations of [girdtools], [STELLA] and [Serialbox (0.1)]. CMake will automatically pick them up and build them if you check them out in `external/`. Note that the Python3 tests require the module [nose]. To run the unitests, run `make test` in the build dirctory.

To build the documentation you need [doxygen] and [sphinx]. CMake will try to pick up the tools and, if successful, provides a target (e.g `make doc`) to build and install the documentation. To view it, open `docs/serialbox-index.html` or `docs/html/index.html`.

[girdtools]: https://github.com/eth-cscs/girdtools
[STELLA]: https://github.com/MeteoSwiss-APN/stella
[Serialbox (0.1)]: https://github.com/MeteoSwiss-APN/serialbox
[Serialbox]: https://github.com/thfabian/serialbox
[Boost]: http://www.boost.org/
[CMake]: https://cmake.org/
[CUDA]: https://developer.nvidia.com/cuda-downloads
[Homebrew]: http://brew.sh/
[doxygen]: http://www.stack.nl/~dimitri/doxygen/ 
[sphinx]: http://www.sphinx-doc.org/en/1.4.8/
[NetCDF-4]: http://www.unidata.ucar.edu/software/netcdf/
[nose]: http://nose.readthedocs.io/en/latest/

