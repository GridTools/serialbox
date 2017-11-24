***********
C interface
***********

This section is concerned with the C interface of Serialbox. A set of examples can be found in ``examples/c/``.

Building
--------

The C interface of Serialbox is built if the CMake variable ``SERIALBOX_ENABLE_C`` is ON (which is the default behaviour) and installed in ``install/lib/`` and ``install/include/serialbox-c``. To get started, include the ``Serialbox.h`` header, which exposes the complete API:

.. code-block:: C

  #include <serialbox-c/Serialbox.h>

To get an overview of the full API, you can take a look at the doxygen generate documentation: `here <_doxygen/html/group__serialboxC.html>`_. 


Linking
-------

Consider the following minimalistic example, ``test.c``, which simply creates and destroys a serializer:

.. code-block:: C

  #include <serialbox-c/Serialbox.h>

  int main(int argc, char* argv[]) {
    serialboxSerializer_t* serializer = serialboxSerializerCreate(Write, "./ser-test", "field", "Binary");
    serialboxSerializerDestroy(serializer);
  }
  
To compile this sample program, you have to link against ``SerialboxC`` and the dependency libraries. The dependency libraries include the Boost libraries (filesystem, system, chrono, log, date_time, log_setup, thread, regex and atomic) as well as the NetCDF and OpenSSL libraries. However, if you choose to link dynamically (i.e against ``libSerialboxC.so``), the dependencies will be resolved automatically:

.. code-block:: console

   $ export SERIALBOX_INSTALL_PATH=<path-to-serialbox-installation>
   $ gcc test.c -o test -I$SERIALBOX_INSTALL_PATH/include -L$SERIALBOX_INSTALL_PATH/lib -lSerialboxC
   $ export LD_LIBRARY_PATH=$SERIALBOX_INSTALL_PATH/lib; ./test 

In general, it is **strongly** advised to use the CMake find_package-module of Serialbox which takes care of linking against the correct libraries (see :doc:`Usage`).

