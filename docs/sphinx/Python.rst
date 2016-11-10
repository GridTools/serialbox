.. Python Serialbox Documentation

****************
Serialbox Python
****************

This section is concerned with the `python3 <https://www.python.org/>`_ interface of Serialbox. A set of examples can be found in ``examples/python/``.

.. contents::
   :local:

.. _Building:

Building
--------

The Python interface of Serialbox is built if the CMake variable ``SERIALBOX_ENABLE_PYTHON`` is ``ON`` (which is the default behaviour) and installed in ``install/python/serialbox``. Serialbox requires atleast Python3.4 and relies on `numpy <http://www.numpy.org/>`_.

To get started, add the path of the Serialbox module to the Python environment variable ``PYTHONPATH``

.. code::

  export PYTHONPATH=<path-to-serialbox-installation>/python:$PYTHONPATH
  
To check if everything works:

.. code::

  export PYTHONPATH=<path-to-serialbox-installation>/python:$PYTHONPATH
  python3 -c "from serialbox import __version__; print(__version__)"
  
which should print the current version string of the library.

.. _QSWriting:

Quick start - Writing
---------------------

This section will show you how to write :class:`numpy.arrays` using the Python interface of Serialbox. The written data set can be read by any language supported by Serialbox. To get started, import the ``serialbox`` module:

  >>> import serialbox as ser
  
To serialize data, you have to create a :class:`Serializer <serialbox.Serializer>` object. The Serializer is initialized with a `mode` (:class:`OpenModeKind <serialbox.OpenModeKind>`), a `directory` and a specific `prefix`, with which all the written files in the data set start. All files will be placed in `directory` and the directory will be created if necessary. For writing there are two modes: ``Write`` and ``Append``: Write will erase all files of a previous run of a serializer the with same `directory` and `prefix`; Append will import all existing information and allow you to add more data.
  
To open a :class:`Serializer <serialbox.Serializer>` for writing in the current directory and prefix all files with ``field``:

  >>> serializer = ser.Serializer(ser.OpenModeKind.Write, ".", "field")

To serialize a field, you have to register the field within the Serializer (:func:`Serializer.register_field <serialbox.Serializer.register_field>`) and specify a specifc :class:`Savepoint <serialbox.Savepoint>` at which the field will be written. Savepoints are used within the Serializer to discriminate fields at different points in time. Savepoints in the Serializer are unique and primarily identified by their :func:`name <serialbox.Savepoint.name>` and further distinguished by their :func:`metainfo <serialbox.Savepoint.metainfo>`. Savepoints can thus have the same name as long as their :func:`metainfo <serialbox.Savepoint.metainfo>` differs. As a short example, we will serialize two :class:`numpy.arrays` ``foo`` and ``bar`` at two different Savepoints (``MySavepoint`` with ``time=1`` and ``time=2``).  

We create the fields:

  >>> import numpy as np
  >>> foo = np.random.rand(10, 10)
  >>> bar = np.random.rand(25, 25, 25)

Register the fields (:class:`FieldMetainfo <serialbox.FieldMetainfo>`) within the :class:`Serializer <serialbox.Serializer>`:

  >>> foo_info = ser.FieldMetainfo(ser.TypeID.Float64, foo.shape)
  >>> bar_info = ser.FieldMetainfo(ser.TypeID.Float64, bar.shape)
  >>> serializer.register_field("foo", foo_info)
  >>> serializer.register_field("bar", bar_info)
  >>> serializer.fieldnames()
  ['foo', 'bar']

Write ``foo`` and ``bar`` at ``MySavepoint`` with ``time=1``

  >>> MySavepoint_t1 = ser.Savepoint("MySavepoint", {"time": 1})
  >>> serializer.register_savepoint(MySavepoint_t1)
  >>> serializer.write("foo", MySavepoint_t1, foo)
  >>> serializer.write("bar", MySavepoint_t1, bar)
  
Perform a silly timestep:
  
  >>> foo *= 2
  >>> bar *= 2
  
and write the updated fields at ``MySavepoint`` with ``time=2``
  
  >>> MySavepoint_t2 = ser.Savepoint("MySavepoint", {"time": 2})
  >>> serializer.register_savepoint(MySavepoint_t2)
  >>> serializer.write("foo", MySavepoint_t2, foo)
  >>> serializer.write("bar", MySavepoint_t2, bar)
  
The current directory will now contain the following files

.. code::

  .
  ├── ArchiveMetaData-field.json
  ├── MetaData-field.json
  ├── field_bar.dat
  └── field_foo.dat

With ``field_foo.dat`` and ``field_bar.dat`` containing the actual data while the `JSON` files hold the meta-information.
  
**Note:** The Python interface is actually more powerful and you can omit all the calls to  :func:`register_field <serialbox.Serializer.register_field>` and :func:`register_savepoint <serialbox.Serializer.register_savepoint>`. The example above can thus be written as:
    
  >>> import numpy as np
  >>> foo = np.random.rand(10, 10)
  >>> bar = np.random.rand(25, 25, 25)
  >>> serializer.write("foo", ser.Savepoint("MySavepoint", {"time": 1}), foo) # implicitly register savepoint and field
  >>> serializer.write("bar", ser.Savepoint("MySavepoint", {"time": 1}), bar)
  >>> foo *= 2
  >>> bar *= 2
  >>> serializer.write("foo", ser.Savepoint("MySavepoint", {"time": 2}), foo)
  >>> serializer.write("bar", ser.Savepoint("MySavepoint", {"time": 2}), bar)

.. _QSReading:

Quick start - Reading
---------------------

This section will show you how to read data from an existing Serialbox data set, written by any language supported by Serialbox. In this example, we will use the data set written in the :ref:`QSWriting` section. To get started, import the ``serialbox`` module:

  >>> import serialbox as ser
  
To access the data, you have to create a :class:`Serializer <serialbox.Serializer>` object. The Serializer is initialized with a `mode` (:class:`OpenModeKind <serialbox.OpenModeKind>`), a `directory` and a specific `prefix`, with which all the files in the data set start. The `directory` has to contain the data in question and the `prefix` has to match the one of the data set (i.e the same as used in the writing).

To open a :class:`Serializer <serialbox.Serializer>` in the current directory for reading of fields matching ``field_*.dat``:
  
  >>> serializer = ser.Serializer(ser.OpenModeKind.Read, ".", "field")
  
In case of an error, a :ref:`SerialboxError` is thrown. To access a particular `field`, you have to specify a specific :class:`Savepoint <serialbox.Savepoint>` of this `field`. Savepoints are used within the Serializer to discriminate fields at different points in time. Savepoints in the Serializer are unique and primarily identified by their :func:`name <serialbox.Savepoint.name>` and further distinguished by their :func:`metainfo <serialbox.Savepoint.metainfo>`. To get a full list of all registered savepoint (in the order they were registered):

  >>> serializer.savepoint_list()
  [Savepoint MySavepoint {"time":1}, Savepoint MySavepoint {"time":2}]

For more advanced queries, see :func:`Serializer.savepoint <serialbox.Serializer.savepoint>`. To get a list of all registered fields:

  >>> serializer.fieldnames()
  ['foo', 'bar']
  
To read ``foo`` at Savepoint with ``time=1``, you can use one of the `read` methods of the Serializer (:func:`Serializer.read <serialbox.Serializer.read>`, :func:`Serializer.read_slice <serialbox.Serializer.read_slice>`, :func:`Serializer.read_async <serialbox.Serializer.read_async>`)

  >>> savepoint = serializer.savepoint["MySavepoint"].time[1]
  >>> foo = serializer.read("foo", savepoint)

``foo`` is a newly allocated :class:`numpy.array` containing the deserialized data.

.. _Serializer:

Serializer
----------

.. autoclass:: serialbox.Serializer
  :members:
  :special-members:
  :exclude-members: __weakref__

.. _MetaInfoMap:
        
MetaInfoMap
-----------

.. autoclass:: serialbox.MetainfoMap
  :members:
  :special-members:
  :exclude-members: __weakref__
  
.. _FieldMetaInfo:
  
FieldMetaInfo
-------------

.. autoclass:: serialbox.FieldMetainfo
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
  
.. _archive:
        
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

.. _Slice:

Slice
-----

Specification of the slice indices which are used for partial loading of serialized data.
To avoid instantiation, use the global object serialbox.Slice.

    >>> Slice[:, :, 5]
    (slice(None, None, None), slice(None, None, None), 5)

See `here <https://docs.scipy.org/doc/numpy/reference/arrays.indexing.html>`_ for furhter information on indexing.

.. _SerialboxError:

SerialboxError
--------------

.. autoclass:: serialbox.SerialboxError
  :members:


