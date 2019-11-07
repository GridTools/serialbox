<a href="https://eth-cscs.github.io/serialbox2"><img src="docs/logo/logo.png" width="559" height="212" border="0" alt="Serialbox2 documentation" /></a>

<a target="_blank" href="http://semver.org">![Version][Version.Badge]</a> <a target="_blank" href="https://travis-ci.org/eth-cscs/serialbox2">![Travis status][TravisCI.Badge]</a> <a target="_blank" href="https://opensource.org/licenses/MIT">![License: BSD][BSD.License]</a> <a target="_blank" href="https://eth-cscs.github.io/serialbox2">![Documentation][Documentation.Badge]</a>

## Introduction

Serialbox is a serialization library and tools for C/C++, Python3 and Fortran. Serialbox is used in several projects for building validation frameworks against reference runs. This is useful in the scope of rewrite of large codes, or when porting codes to multiple computing architectures. As an example, porting scientific codes to graphical processing units, that require continuous validation against the existing x86 code.

For instruction on how to build and use Serialbox, see [here](https://eth-cscs.github.io/serialbox2).

## Continuous integration  <a id="continuous-integration"></a>

Serialbox builds are run on Travis CI [here](https://travis-ci.org/eth-cscs/serialbox2).

### Linux

|  Toolchain   | Config         |                                                     Status                                                         |
|:-------------|:---------------|:-------------------------------------------------------------------------------------------------------------------|
| GCC 5.5      | Release        |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 5.5][GCC_55_Release.Badge]</a>            |
| GCC 5.5      | RelWithDebInfo |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 5.5][GCC_55_RelWithDebInfo.Badge]</a>     |
| GCC 7.4      | Release        |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 7.4][GCC_74_Release.Badge]</a>            |
| GCC 7.4      | RelWithDebInfo |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 7.4][GCC_74_RelWithDebInfo.Badge]</a>     |
| GCC 8.1      | Release        |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 8.1][GCC_81_Release.Badge]</a>            |
| GCC 8.1      | RelWithDebInfo |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 8.1][GCC_81_RelWithDebInfo.Badge]</a>     |
| Clang 5.0    | Release        |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![Clang 5.0][Clang_50_Release.Badge]</a>        |
| Clang 5.0    | RelWithDebInfo |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![Clang 5.0][Clang_50_RelWithDebInfo.Badge]</a> |

<!-- 
### OSX
 
|  Toolchain   | Config         |                                                     Status                                                           |
|:-------------|:---------------|:---------------------------------------------------------------------------------------------------------------------|
| Xcode 7.3    | Release        |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 5.4][Xcode_73_Release.Badge]</a>            |
| Xcode 7.3    | RelWithDebInfo |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 5.4][Xcode_73_RelWithDebInfo.Badge]</a>     |
| Xcode 8.0    | Release        |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 6.3][Xcode_80_Release.Badge]</a>            |
| Xcode 8.0    | RelWithDebInfo |  <a target="_blank" href="https://travis-ci.org/eth-cscs/eth-cscs">![GCC 6.3][Xcode_80_RelWithDebInfo.Badge]</a>     |
 -->
## License

> You can check out the full license [here](LICENSE.txt).

This project is licensed under the terms of the **BSD** license.

<!-- Links -->
[TravisCI]: https://travis-ci.org/eth-cscs/serialbox2
[TravisCI.Badge]: https://travis-ci.org/eth-cscs/serialbox2.svg?branch=master
[Documentation.Badge]: https://img.shields.io/badge/documentation-link-blue.svg
[BSD.License]: https://img.shields.io/badge/License-BSD-blue.svg
[Version.Badge]: https://badge.fury.io/gh/eth-cscs%2Fserialbox2.svg
[GCC_55_Release.Badge]: https://travis-matrix-badges.herokuapp.com/repos/eth-cscs/serialbox2/branches/master/3
[GCC_55_RelWithDebInfo.Badge]: https://travis-matrix-badges.herokuapp.com/repos/eth-cscs/serialbox2/branches/master/4
[GCC_74_Release.Badge]: https://travis-matrix-badges.herokuapp.com/repos/eth-cscs/serialbox2/branches/master/5
[GCC_74_RelWithDebInfo.Badge]: https://travis-matrix-badges.herokuapp.com/repos/eth-cscs/serialbox2/branches/master/6
[GCC_81_Release.Badge]: https://travis-matrix-badges.herokuapp.com/repos/eth-cscs/serialbox2/branches/master/7
[GCC_81_RelWithDebInfo.Badge]: https://travis-matrix-badges.herokuapp.com/repos/eth-cscs/serialbox2/branches/master/8
[Clang_50_Release.Badge]: https://travis-matrix-badges.herokuapp.com/repos/eth-cscs/serialbox2/branches/master/9
[Clang_50_RelWithDebInfo.Badge]: https://travis-matrix-badges.herokuapp.com/repos/eth-cscs/serialbox2/branches/master/10
