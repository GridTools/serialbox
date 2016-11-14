Introduction
============

Serialbox is a serialization library and tools for C/C++, Python3 and Fortran. Serialbox is used in several projects for building validation frameworks against reference runs. This is useful in the scope of rewrite of large codes, or when porting codes to multiple computing architectures. As an example, porting scientific codes to graphical processing units, that require continuous validation against the existing x86 code.

.. toctree::
   :hidden:
   :maxdepth: 2

   Building
   Documentation
   Usage
   C++
   C
   Python
   Fortran

Building
========

:doc:`Building Serialbox with CMake <Building>`
  Discusses how to get up and running quickly with the Serialbox tools and libraries.
  
:doc:`Building the Documentation<Documentation>`
  Instructions on how to build and deploy the documentation to `GitHub Pages <https://pages.github.com/>`_.
  
Examples
========

:doc:`Using Serialbox as an external library with CMake <Usage>`
  Notes on using the CMake module of Serialbox.
  
API Documentation
=================

The core implementation of Serialbox is written in C++ and bindings to C, Fortran and Python3 are available.

:doc:`C++ interface <C++>`
  Information about the C++ interface of Serialbox.

:doc:`C interface <C>`
  Information about the C interface of Serialbox.

:doc:`Python3 module <Python>`
  A reference manual of the Python3 interface of Serialbox.
  
:doc:`Fortran interface <Fortran>`
  Information about the Fortran interface of Serialbox.

