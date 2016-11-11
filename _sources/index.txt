.. Serialbox Documentation

Introduction
============

Serialbox is a serialization library and tools for C/C++, Python3 and Fortran. Serialbox is used in several projects for building validation frameworks against reference runs. This is useful in the scope of rewrite of large codes, or when porting codes to multiple computing architectures. As an example, porting scientific codes to graphical processing units, that require continuous validation against the existing x86 code.

.. toctree::
   :hidden:
   :maxdepth: 2

   Building
   Documentation
   C++
   C
   Python

Building
========

:doc:`Building Serialbox with CMake <Building>`
  Discusses how to get up and running quickly with the Serialbox tools and libraries.
  
:doc:`Building documentation<Documentation>`
  Discusses how to build and deploy the documentation to `github pages <https://pages.github.com/>`_.
  
API Documentation
=================

The core implementation of Serialbox is written in C++ and bindings to C, Fortran and Python3 are available.

:doc:`C++ interface <C++>`
  C++ interface of Serialbox.

:doc:`C interface <C>`
  C interface of Serialbox.

:doc:`Python module <Python>`
  Python3 interface of Serialbox.

