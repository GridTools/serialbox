**********************
sdb - stencil debugger
**********************

This section is concerned with the ``sdb`` -- stencil debugger of Serialbox.

.. contents::
   :local:
   
.. _sdbInstallation:

Installation
============

``sdb`` is written in ``python3`` using ``PyQt5`` and depends on the Python module of :doc:`Serialbox <Python>`. It further has optional dependencies on `matplotlib <http://matplotlib.org/>`_ and `IPython <https://ipython.org/>`_.

Ubuntu 16.04
------------

To install all dependencies on Ubuntu 16.04 using apt:

.. code-block:: console

   $ sudo apt-get install python3-pyqt5 python3-matplotlib python3-zmq ipython3-qtconsole python3-pyqt5.qtsvg
     

Mac OSX
-------

To install all dependencies on Mac OSX using `Homebrew <http://brew.sh/>`_:

.. code-block:: console

   $ brew install python3 pyqt5
   $ pip3 install qtconsole matplotlib
   
Make sure ``pip3`` points to your current ``python3`` interpreter.

.. _sdbUsageExample:

Usage Example
=============


.. _sdbSerializerFormat:

Serializer Format
=================

This section describes the format requirements by ``sdb`` on the Serializer and Savepoints. This might be useful, if you want to add ``sdb`` support for you language/library supported by Serialbox. 

    #. Requirements on the Serializer:
        - The Serializer needs to contain a global meta-information with key ``"stencils"`` which holds a vector of strings of all registrered stencils.
  
    #. Requirements on the Savepoints of the stencil:
        - Each stage of a stencil has two Savepoints, input and output, which are named ``"<stencil_name>__in"`` and ``"<stencil_name>__out"`` where ``<stencil_name>`` is the name of the stencil regsitered in the global meta-information of the Serializer.  
        - Each Savepoint needs to contain the following meta-information:
            - ``stage_name`` : Name of the current stage [str].
            - ``stage_id`` : Unique id of the stage [int].
            - ``invocation_count`` : Number of invocations of the stencil [int]

**Example:**
The following shows how to implement the above mentioned requirements in Python:

Consider the following, totally silly, stencil which operates on the two 3-dimensional `numpy <http://www.numpy.org/>`_ fields ``u`` and ``v`` and keeps track of the invocation count.

.. code-block:: python

    import numpy as np

    class MyStencil(object):

        def __init__(self):
            self.invocation_count = 0
            self.u = np.random.rand(32, 34, 80)
            self.v = np.random.rand(32, 34, 80)

        def run(self):
            self.my_fance_stage()
            self.invocation_count += 1

        def my_fance_stage(self):
            self.u += 1
            self.v += 2

    if __name__ == '__main__':
        stenncil = MyStencil()
        stenncil.run()

Now, we would like to add fine-grained serialization to ``MyStencil``. Meaning, we desire to serialize ``u`` and ``v`` *before* and *after* each stage such that we can analyze the stencil with ``sdb``. This requires us to follow the requirements mentioned above.
        
.. code-block:: python
        
    import numpy as np
    import serialbox as ser

    class MyStencil(object):

        def __init__(self):
            self.invocation_count = 0
            self.u = np.random.rand(32, 34, 80)
            self.v = np.random.rand(32, 34)
            
        def run(self, serializer):
            self.my_fance_stage(serializer)
            self.invocation_count += 1

        def my_fance_stage(self, serializer):
            #
            # At each stage we create the savepoints and serialize the input fields, ...
            #
            savepoint_in = ser.Savepoint("MyStencil__in")
            savepoint_in.metainfo.insert("stage_name", "my_fance_stage")
            savepoint_in.metainfo.insert("invocation_count", self.invocation_count)
            savepoint_in.metainfo.insert("stage_id", 0)
            
            serializer.write("u", savepoint_in, self.u)
            serializer.write("v", savepoint_in, self.v)

            #
            # .. run the stage ...
            #    
            self.u += 1
            self.v += 2
            
            #
            # .. and serialize the output fields at the output savepoint.
            # 
            savepoint_out = ser.Savepoint("MyStencil__out")
            savepoint_out.metainfo.insert("stage_name", "my_fance_stage")
            savepoint_out.metainfo.insert("invocation_count", self.invocation_count)
            savepoint_out.metainfo.insert("stage_id", 0)
            
            serializer.write("u", savepoint_out, self.u)
            serializer.write("v", savepoint_out, self.v)


    if __name__ == '__main__':
        #
        # Initialize the Serializer and register the stencil in the global meta-information
        #
        serializer = ser.Serializer(ser.OpenModeKind.Write, "./test/", "stencil")
        serializer.global_metainfo.insert("stencils", ["MyStencil"])

        stenncil = MyStencil()
        stenncil.run(serializer)

.. _sdbAPIDocumentation:

API Documentation
=================
     

