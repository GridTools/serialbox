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


.. _MetaInfoMap:
        
MetaInfoMap
-----------

.. autoclass:: serialbox.MetaInfoMap
  :members:
  :special-members:
  :exclude-members: __weakref__
  
.. _FieldMetaInfo:
  
FieldMetaInfo
-------------

.. autoclass:: serialbox.FieldMetaInfo
  :members:
  :special-members:
  :exclude-members: __weakref__

.. _Savepoint:
        
Savepoint
--------- 

.. autoclass:: serialbox.Savepoint
  :members:
  :special-members:
  :exclude-members: __weakref__
  
.. autoclass:: serialbox.SavepointCollection
  :members:
  
.. _Logging:
        
Archive
-------

.. autoclass:: serialbox.Archive
  :members:
  
  .. _Logging:
        
Logging
-------

.. autoclass:: serialbox.Logging
  :members:
  
  
.. _Config:

Config
------

.. autoclass:: serialbox.Config
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

.. _TypeID:

TypeID
------

.. autoclass:: serialbox.TypeID

+-------------------------+-----------------------------------------------------------------+----------------------------------------+
| TypeID                  | Python Type                                                     | C++ Type                               |
+=========================+=================================================================+========================================+
| ``TypeID.Boolean``      | :class:`bool`                                                   | ``bool``                               |
+-------------------------+-----------------------------------------------------------------+----------------------------------------+
| ``TypeID.Int32``        | :class:`int`, np.int32                                          | ``int``                                |
+-------------------------+-----------------------------------------------------------------+----------------------------------------+
| ``TypeID.Int64``        | :class:`int`, np.int64                                          | ``std::int64_t``                       |
+-------------------------+-----------------------------------------------------------------+----------------------------------------+
| ``TypeID.Float32``      | :class:`float`, np.float32                                      | ``float``                              |
+-------------------------+-----------------------------------------------------------------+----------------------------------------+
| ``TypeID.Float64``      | :class:`float`, np.float64                                      | ``double``                             |
+-------------------------+-----------------------------------------------------------------+----------------------------------------+
| ``TypeID.String``       | :class:`str`                                                    | ``std::string``                        |
+-------------------------+-----------------------------------------------------------------+----------------------------------------+

.. _SerialboxError:

SerialboxError
--------------

.. autoclass:: serialbox.SerialboxError



