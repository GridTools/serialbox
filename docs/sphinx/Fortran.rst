*****************
Fortran interface
*****************

This section deals with the Fortran interface of Serialbox. A simple example can be found in ``examples/fortran/simple`` and a fully self-contained one in ``examples/fortran/perturbation``.

.. _FortranBuilding:

Building
========

To build the Fortran interface, proceed as described in :ref:`Quick start` section with the difference of setting the CMake variable ``SERIALBOX_ENABLE_FORTRAN`` to ON. Note that you can control the Fortran compiler used by CMake with the the environment varialbe ``FC``.

**For example**: To use the GNU fortran compiler:

   .. code-block:: console

     $ export FC=gfortran
     $ cmake ../ -DSERIALBOX_ENABLE_FORTRAN=ON
     $ make
     $ make install
     
The CMake build system of Serialbox takes care of setting the correct flags for the different Fortran compilers (GNU, PGI and Cray).

.. note::
  If you want to use Serialbox in your project, it is important to use the same tool-chain (C++ and Fortran compiler) than the one used to build Serialbox.
     
Using the Fortran Interface
===========================

To use the Fortran interface in your project, you need to link against ``libSerialboxFortran`` and the C++ standard library used to compile the code (e.g ``libstdc++`` for GNU gcc) as well as the dependency libraries (possibly NetCDF and OpenSSL). As this can be tedious, Serialbox provides a CMake find_package module (:ref:`FindSerialbox`) to handle this task. 

To use the preprocessing script ``pp_ser.py``, take a look at the CMake module :ref:`SerialboxTooling`. For a self-contained example see ``examples/fortran/perturbation``.
