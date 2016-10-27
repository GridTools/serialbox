.. Serialbox documentation master file, created by
   sphinx-quickstart on Thu Oct 27 15:47:49 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. toctree::
   :maxdepth: 2

Python Serialbox - Data serialization library
=============================================

Serialbox is a serialization library and tools for C/C++, Python3 and Fortran. Serialbox is used in several projects for building validation frameworks against reference runs. This is useful in the scope of rewrite of large codes, or when porting codes to multiple computing architectures. As an example, porting scientific codes to graphical processing units, that require continuous validation against the existing x86 code.

This documentation is concerned with the Python interface of Serialbox. To get started, import the ``serialbox`` module:

  >>> import serialbox as ser
   
.. _Serializer:

Serializer
----------

.. autoclass:: serialbox.Serializer
  :members:

         
.. _Savepoint:
        
Savepoint
--------- 

.. autoclass:: serialbox.Savepoint
  :members:
  :special-members:
  :exclude-members: __weakref__
  
.. autoclass:: serialbox.SavepointCollection
  :members:

.. _MetaInfoMap:
        
MetaInfoMap
-----------

Objects of this class contain a map of meta-information in form of key = value or key = {value1, ... valueN} pair. The keys are strings and unique, while the values can be integers, booleans, floating point numbers (either single or double precision) or strings.

.. autoclass:: serialbox.MetaInfoMap
  :members:

.. _OpenModeKind:

OpenModeKind
------------

.. autoclass:: serialbox.OpenModeKind

==========  =====
Mode        Value
==========  =====
``Read``    0    
``Write``   1    
``Append``  2     
==========  =====

.. _SerialboxError:

SerialboxError
--------------

.. autoclass:: serialbox.SerialboxError



