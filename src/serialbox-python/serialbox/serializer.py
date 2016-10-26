#!/usr/bin/python3
# -*- coding: utf-8 -*-
##===-----------------------------------------------------------------------------*- Python -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##
##
## This file contains the Serializer implementation of the Python Interface.
##
##===------------------------------------------------------------------------------------------===##

from ctypes import c_char_p, c_void_p, c_int, Structure, POINTER

import numpy as np

from .archive import Archive
from .common import get_library, extract_string
from .error import invoke, SerialboxError
from .fieldmetainfo import FieldMetaInfo, FieldMetaInfoImpl
from .metainfomap import MetaInfoMap, MetaInfoImpl, ArrayOfStringImpl
from .savepoint import Savepoint, SavepointImpl
from .type import *

lib = get_library()


class SerializerImpl(Structure):
    """ Mapping of serialboxSerializer_t """
    _fields_ = [("impl", c_void_p), ("ownsData", c_int)]


def register_library(library):
    #
    # Construction & Destruction
    #
    library.serialboxSerializerCreate.argtypes = [c_int, c_char_p, c_char_p, c_char_p]
    library.serialboxSerializerCreate.restype = POINTER(SerializerImpl)

    library.serialboxSerializerDestroy.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerDestroy.restype = None

    #
    # Utility
    #
    library.serialboxSerializerGetMode.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerGetMode.restype = c_int

    library.serialboxSerializerGetDirectory.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerGetDirectory.restype = c_char_p

    library.serialboxSerializerGetPrefix.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerGetPrefix.restype = c_char_p

    library.serialboxSerializerUpdateMetaData.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerUpdateMetaData.restype = None

    library.serialboxEnableSerialization.argtypes = None
    library.serialboxEnableSerialization.restype = None

    library.serialboxDisableSerialization.argtypes = None
    library.serialboxDisableSerialization.restype = None

    library.serialboxSerializationStatus.argtypes = None
    library.serialboxSerializationStatus.restype = c_int

    #
    # Global Meta-information
    #
    library.serialboxSerializerGetGlobalMetaInfo.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerGetGlobalMetaInfo.restype = POINTER(MetaInfoImpl)

    #
    #  Register and Query Savepoints
    #
    library.serialboxSerializerAddSavepoint.argtypes = [POINTER(SerializerImpl),
                                                        POINTER(SavepointImpl)]
    library.serialboxSerializerAddSavepoint.restype = c_int

    library.serialboxSerializerHasSavepoint.argtypes = [POINTER(SerializerImpl),
                                                        POINTER(SavepointImpl)]
    library.serialboxSerializerHasSavepoint.restype = c_int

    library.serialboxSerializerGetNumSavepoints.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerGetNumSavepoints.restype = c_int

    library.serialboxSerializerGetSavepointVector.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerGetSavepointVector.restype = POINTER(POINTER(SavepointImpl))

    library.serialboxSerializerDestroySavepointVector.argtypes = [POINTER(POINTER(SavepointImpl)),
                                                                  c_int]
    library.serialboxSerializerDestroySavepointVector.restype = None

    library.serialboxSerializerGetFieldnamesAtSavepoint.argtypes = [POINTER(SerializerImpl),
                                                                    POINTER(SavepointImpl)]
    library.serialboxSerializerGetFieldnamesAtSavepoint.restype = POINTER(ArrayOfStringImpl)

    library.serialboxSavepointCreateFromSavepoint.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointCreateFromSavepoint.restype = POINTER(SavepointImpl)

    #
    #  Register and Query Fields
    #
    library.serialboxSerializerAddField.argtypes = [POINTER(SerializerImpl), c_char_p,
                                                    POINTER(FieldMetaInfoImpl)]
    library.serialboxSerializerAddField.restype = c_int

    library.serialboxSerializerHasField.argtypes = [POINTER(SerializerImpl), c_char_p]
    library.serialboxSerializerHasField.restype = c_int

    library.serialboxSerializerGetFieldMetaInfo.argtypes = [POINTER(SerializerImpl), c_char_p]
    library.serialboxSerializerGetFieldMetaInfo.restype = POINTER(FieldMetaInfoImpl)

    library.serialboxSerializerGetFieldnames.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerGetFieldnames.restype = POINTER(ArrayOfStringImpl)

    #
    # Writing & Reading
    #
    library.serialboxSerializerWrite.argtypes = [POINTER(SerializerImpl),
                                                 c_char_p,
                                                 POINTER(SavepointImpl),
                                                 c_void_p,
                                                 POINTER(c_int),
                                                 c_int]
    library.serialboxSerializerWrite.restype = None

    library.serialboxSerializerRead.argtypes = [POINTER(SerializerImpl),
                                                c_char_p,
                                                POINTER(SavepointImpl),
                                                c_void_p,
                                                POINTER(c_int),
                                                c_int]
    library.serialboxSerializerRead.restype = None

    library.serialboxSerializerReadSliced.argtypes = [POINTER(SerializerImpl),
                                                      c_char_p,
                                                      POINTER(SavepointImpl),
                                                      c_void_p,
                                                      POINTER(c_int),
                                                      c_int,
                                                      POINTER(c_int)]
    library.serialboxSerializerReadSliced.restype = None

    library.serialboxSerializerReadAsync.argtypes = [POINTER(SerializerImpl),
                                                     c_char_p,
                                                     POINTER(SavepointImpl),
                                                     c_void_p,
                                                     POINTER(c_int),
                                                     c_int]
    library.serialboxSerializerReadAsync.restype = None

    library.serialboxSerializerWaitForAll.argtypes = [POINTER(SerializerImpl)]
    library.serialboxSerializerWaitForAll.restype = None

    #
    # Stateless Serialization
    #
    library.serialboxWriteToFile.argtypes = [c_char_p,
                                             c_void_p,
                                             c_int,
                                             POINTER(c_int),
                                             c_int,
                                             POINTER(c_int),
                                             c_char_p,
                                             c_char_p]
    library.serialboxWriteToFile.restype = None

    library.serialboxReadFromFile.argtypes = [c_char_p,
                                              c_void_p,
                                              c_int,
                                              POINTER(c_int),
                                              c_int,
                                              POINTER(c_int),
                                              c_char_p,
                                              c_char_p]
    library.serialboxReadFromFile.restype = None

    #
    # Array
    #
    library.serialboxArrayOfStringDestroy.argtypes = [POINTER(ArrayOfStringImpl)]
    library.serialboxArrayOfStringDestroy.restype = None


class Serializer(object):
    """Serializer implementation of the Python Interface.
    """
    Enabled = 1
    Disabled = -1

    def __init__(self, mode, directory, prefix, archive="Binary"):
        """Initialize the Serializer and prepare it to perform input/output operations.

        The serializer is attached to a specific `directory` and to a specific `prefix`, with which
        all files read and written will start. There are three modes to open a serializer: `Read`,
        `Write` and `Append` (see :class:`OpenModeKind`). `Read` will give a read-only access to the
        serialized data; `Write` will erase all files of a previous run of a serializer with same
        `directory` and `prefix`; `Append` will import all existing information and allow the user
        to add more data.

        :param mode: Mode of the Serializer
        :type mode: OpenModeKind
        :param directory: The directory where the files will be stored (will be created if
                          necessary)
        :type directory: str
        :param prefix: The prefix of the files
        :type prefix: str
        :param archive: String used to construct the Archive
                        (see :func:`serialbox.Archive.registered_archives`)
        :type archive: str
        """
        self.__serializer = None

        #
        # Convert mode
        #
        if isinstance(mode, OpenModeKind):
            mode = mode.value
        modeint = c_int(mode)

        #
        # Validate archive
        #
        archives = Archive.registered_archives()
        if archive not in archives:
            raise SerialboxError(
                "'%s' is not a valid archive. Registered archives: %s" % (archive, archives))

        #
        # Create serializer
        #
        dirstr = extract_string(directory)[0]
        prefixstr = extract_string(prefix)[0]
        archivestr = extract_string(archive)[0]

        self.__serializer = invoke(lib.serialboxSerializerCreate, modeint, dirstr, prefixstr,
                                   archivestr)

    # ===----------------------------------------------------------------------------------------===
    #   Utility
    # ==-----------------------------------------------------------------------------------------===

    @property
    def mode(self):
        """Return mode of the Serializer.

        :return: Mode of the Serializer
        :rtype: OpenModeKind
        """
        return OpenModeKind(invoke(lib.serialboxSerializerGetMode, self.__serializer))

    @property
    def prefix(self):
        """Return the prefix of all filenames.

        :return: prefix of all filenames
        :rtype: str
        """
        return invoke(lib.serialboxSerializerGetPrefix, self.__serializer).decode()

    @property
    def directory(self):
        """Return the directory of the Serializer.

        :return: directory of the Serializer
        :rtype: str
        """
        return invoke(lib.serialboxSerializerGetDirectory, self.__serializer).decode()

    def update_meta_data(self):
        """ Write meta-data to disk.
        """
        invoke(lib.serialboxSerializerUpdateMetaData, self.__serializer)

    @staticmethod
    def enable():
        """Enable serialization.

        Serialization is enabled by default, but it can be disabled either by setting the
        environment variable `STELLA_SERIALIZATION_DISABLE` to a positive value or by calling the
        funcion :func:`Serializer.disable`. With this function you enable the serialization
        independently of the current environment.

        The serialization can be only globally enabled or disabled. There is no way to enable or
        disable only a specific serializer.
        """
        invoke(lib.serialboxEnableSerialization)

    @staticmethod
    def disable():
        """Disable serialization.

        Serialization is enabled by default, but it can be disabled either by setting the
        environment variable `STELLA_SERIALIZATION_DISABLE` to a positive value or by calling the
        funcion :func:`Serializer.disable`. With this function you disable the serialization
        independently of the current environment.

        The serialization can be only globally enabled or disabled. There is no way to enable or
        disable only a specific serializer.
        """
        invoke(lib.serialboxDisableSerialization)

    @staticmethod
    def status():
        """Get the status of serialization

        The status is represented as an integer which can take the following values:

        - 0: the variable is not yet initialized i.e the serialization is enabled if the environment
          variable `STELLA_SERIALIZATION_DISABLE` or `SERIALBOX_SERIALIZATION_DISABLE` is not set
          to a positive value. The first Serializer which is initialized has to set this value
          either to +1 or to -1 according to the environment.
        - +1: the serialization is enabled, independently of the environment
        - -1: the serialization is disabled, independently of the environment

        See :func:`Serializer.enable` and :func:`Serializer.disable`.

        :return: serialization status
        :rtype: int
        """
        return int(invoke(lib.serialboxSerializationStatus))

    # ===----------------------------------------------------------------------------------------===
    #   Global Meta-information
    # ==-----------------------------------------------------------------------------------------===

    @property
    def global_metainfo(self):
        """Global meta-information of the serializer.

        :return: Refrence to the meta-information map
        :rtype: MetaInfoMap
        """
        return MetaInfoMap(impl=invoke(lib.serialboxSerializerGetGlobalMetaInfo, self.__serializer))

    # ===----------------------------------------------------------------------------------------===
    #    Register and Query Savepoints
    # ==-----------------------------------------------------------------------------------------===

    def register_savepoint(self, savepoint):
        """Register savepoint `savepoint` within the Serializer.

        :param savepoint: Savepoint to add
        :type savepoint: Savepoint
        :raises SerialboxError: Savepoint already exists within the Serializer
        """
        if not invoke(lib.serialboxSerializerAddSavepoint, self.__serializer, savepoint.impl()):
            raise SerialboxError(
                "savepoint '%s' already exists withing the Serializer" % (savepoint.__str__()))

    def has_savepoint(self, savepoint):
        """Check if `savepoint` exists in the Serializer.

        :param savepoint: Savepoint to search for
        :type savepoint: Savepoint

        :return: True if Savepoint exists, False otherwise
        :rtype: bool
        """
        return bool(
            invoke(lib.serialboxSerializerHasSavepoint, self.__serializer, savepoint.impl()))

    def get_savepoint(self, name):
        """Get a list of Savepoint(s) identified by `name` in the order they were registered

        Savepoints can have the same `name` but their meta-data has to be different.

        :param name: Name of the savepoint(s)
        :type name: str
        :return: List of registered Savepoint(s) identified  by `name`
        :rtype: list[Savepoint]
        """
        return [sp for sp in self.savepoints() if sp.name == name]

    def fields_at_savepoint(self, savepoint):
        """Get a list of the registered fieldnames at `savepoint`.

        :param savepoint: Savepoint to use
        :type savepoint: Savepoint
        :return: list of fieldsnames
        :rtype: list[str]
        :raises SerialboxError: Savepoint does not exists
        """
        array = invoke(lib.serialboxSerializerGetFieldnamesAtSavepoint, self.__serializer,
                       savepoint.impl())
        list_array = []
        for i in range(array.contents.len):
            list_array += [array.contents.data[i].decode()]
        invoke(lib.serialboxArrayOfStringDestroy, array)
        return list_array

    def savepoints(self):
        """ Get a list of registered savepoints within the Serializer.

        The Savepoints in the list are copy-constructed from the Savepoints in the Serializer and
        inserted in the order they were registered.

        :return: List of registered savepoints
        :rtype: list[Savepoint]
        """
        vec_savepoint = lib.serialboxSerializerGetSavepointVector(self.__serializer)
        num_savepoints = lib.serialboxSerializerGetNumSavepoints(self.__serializer)

        list_savepoint = []
        for i in range(num_savepoints):
            list_savepoint += [Savepoint('',
                                         impl=invoke(lib.serialboxSavepointCreateFromSavepoint,
                                                     vec_savepoint[i].contents))]
        invoke(lib.serialboxSerializerDestroySavepointVector, vec_savepoint, num_savepoints)
        return list_savepoint

    # ===----------------------------------------------------------------------------------------===
    #    Register and Query Fields
    # ==-----------------------------------------------------------------------------------------===

    def register_field(self, name, fieldmetainfo):
        """Add field given by `name` with field meta-information `fieldmetainfo` to the Serializer.

        :param name: Name of the newly registered field
        :type name: str
        :param fieldmetainfo: Field meta-information of the newly registered field
        :type fieldmetainfo: FieldMetaInfo

        :raises SerialboxError: Field with given name already exists within the Serializer
        """
        namestr = extract_string(name)[0]
        if not invoke(lib.serialboxSerializerAddField, self.__serializer, namestr,
                      fieldmetainfo.impl()):
            raise SerialboxError("field '%s' already exists withing the Serializer" % name)

    def has_field(self, field):
        """Check if `field` is registered within the Serializer.

        :param field: Name of the field to check for
        :type field: str
        :return: True if field exists, False otherwise
        :rtype: bool
        """
        fieldstr = extract_string(field)[0]
        return bool(invoke(lib.serialboxSerializerHasField, self.__serializer, fieldstr))

    def get_field_metainfo(self, field):
        """Get the :class:`FieldMetaInfo` of `field`.

        :param field: Name of the field
        :type field: str
        :return: Copy of the field meta-information of `field`
        :rtype: FieldMetaInfo
        :raises SerialboxError: `field` does not exist within the Serializer
        """
        fieldstr = extract_string(field)[0]
        return FieldMetaInfo(None, [],
                             impl=invoke(lib.serialboxSerializerGetFieldMetaInfo, self.__serializer,
                                         fieldstr))

    def fieldnames(self):
        """Get a list of registered fieldnames within the Serializer.

        :return: list of fieldsnames
        :rtype: list[str]
        """
        array = invoke(lib.serialboxSerializerGetFieldnames, self.__serializer)
        list_array = []
        for i in range(array.contents.len):
            list_array += [array.contents.data[i].decode()]
        invoke(lib.serialboxArrayOfStringDestroy, array)
        return list_array

    # ===----------------------------------------------------------------------------------------===
    #    Writing & Reading
    # ==-----------------------------------------------------------------------------------------===

    @staticmethod
    def __extract_strides(field):
        """Extract strides from a numpy.array and convert to unit-strides, returns a C-Array and its
           size
        """
        strides = (c_int * len(field.strides))()
        for i in range(len(field.strides)):
            strides[i] = int(field.strides[i] / field.dtype.itemsize)
        num_strides = c_int(len(field.strides))
        return strides, num_strides,

    @staticmethod
    def __extract_dims(field):
        """Extract dimensions from a numpy.array, returns a C-Array and its size
        """
        dims = (c_int * len(field.shape))()
        for i in range(len(field.shape)):
            dims[i] = int(field.shape[i])
        num_dims = c_int(len(field.shape))
        return dims, num_dims,

    def __allocate_or_check_field(self, name, field):
        """Allocate or check the numpy field for consistency
        """
        if not self.has_field(name):
            raise SerialboxError("field '%s' is not registered within the Serializer" % name)

        info = self.get_field_metainfo(name)
        if field is None:
            field = np.ndarray(shape=info.dims, dtype=typeID2numpy(info.type))
        else:
            if list(field.shape) != list(info.dims):
                raise SerialboxError(
                    "registered dimensions %s do not match dimensions of field (%s) %s" % (
                        field.shape, name, info.dims))

            if numpy2TypeID(field.dtype) != info.type:
                raise SerialboxError(
                    "registered type %s does not match type of field (%s) %s" % (
                        numpy2TypeID(field.dtype), name, info.type))

        return (field, info,)

    def write(self, name, savepoint, field, register_field=True):
        """ Serialize `field` identified by `name` at `savepoint` to disk

        The `savepoint` will be registered at field `name` if not yet present. If `register_field`
        is True, the field will be registered if necessary.

        :param name: Name of the field
        :type name: str
        :param savepoint: Savepoint to at which the field will be serialized
        :type savepoint: Savepoint
        :param field: Field to serialize
        :type field: numpy.array
        :param register_field: Register the field if not present
        :type register: bool

        :raises SerialboxError: Serialization failed
        """
        if self.mode == OpenModeKind.Read:
            raise SerialboxError("write operations are not permitted in OpenModeKind.Read")

        if not self.has_field(name):
            if register_field:
                info = FieldMetaInfo(numpy2TypeID(field.dtype), list(field.shape))
                self.register_field(name, info)
            else:
                raise SerialboxError("field '%s' is not registered within the Serializer" % name)

        #
        # Extract strides and convert to unit-strides
        #
        strides, num_strides = self.__extract_strides(field)

        #
        # Write to disk
        #
        origin_ptr = c_void_p(field.ctypes.data)
        namestr = extract_string(name)[0]
        invoke(lib.serialboxSerializerWrite, self.__serializer, namestr, savepoint.impl(),
               origin_ptr, strides, num_strides)

    def read(self, name, savepoint, field=None):
        """ Deserialize `field` identified by `name` at `savepoint` from disk

        If `field` is a `numpy.array` it will be filled with data from disk. If `field` is None,
        a new `numpy.array` will be allocated with the registered dimensions and type.

        :param name: Name of the field
        :type name: str
        :param savepoint: Savepoint to at which the field will be serialized
        :type savepoint: Savepoint
        :param field: Field to serialize
        :type field: numpy.array

        :return: Newly allocated and deserialized field
        :rtype: numpy.array

        :raises SerialboxError: Deserialization failed
        """
        if self.mode != OpenModeKind.Read:
            raise SerialboxError("read operations are not permitted in OpenModeKind.%s" % self.mode)

        #
        # Allocate or check the field
        #
        field = self.__allocate_or_check_field(name, field)[0]

        #
        # Extract strides and convert to unit-strides
        #
        strides, num_strides = self.__extract_strides(field)

        #
        # Read from disk
        #
        origin_ptr = c_void_p(field.ctypes.data)
        namestr = extract_string(name)[0]
        invoke(lib.serialboxSerializerRead, self.__serializer, namestr, savepoint.impl(),
               origin_ptr, strides, num_strides)

        return field

    def read_async(self, name, savepoint, field=None):
        """ Asynchronously deserialize field `name` (given as `storageView`) at `savepoint` from
        disk using std::async.

        If `field` is a `numpy.array` it will be filled with data from disk. If `field` is None,
        a new `numpy.array` will be allocated with the registered dimensions and type.

        This method runs the `read` function asynchronously (potentially in a separate thread which
        may be part of a thread pool) meaning this function immediately returns. To synchronize all
        threads, use :func:`Serializer.wait_for_all()`.

        If the archive is not thread-safe or if the library was not configured with
        `SERIALBOX_ASYNC_API` the method falls back to synchronous execution.

        :param name: Name of the field
        :type name: str
        :param savepoint: Savepoint to at which the field will be serialized
        :type savepoint: Savepoint
        :param field: Field to serialize
        :type field: numpy.array

        :return: Newly allocated and deserialized field
        :rtype: numpy.array

        :raises SerialboxError: Deserialization failed
        """
        if self.mode != OpenModeKind.Read:
            raise SerialboxError("read operations are not permitted in OpenModeKind.%s" % self.mode)

        #
        # Allocate or check the field
        #
        field = self.__allocate_or_check_field(name, field)[0]

        #
        # Extract strides and convert to unit-strides
        #
        strides, num_strides = self.__extract_strides(field)

        #
        # Read from disk
        #
        origin_ptr = c_void_p(field.ctypes.data)
        namestr = extract_string(name)[0]
        invoke(lib.serialboxSerializerReadAsync, self.__serializer, namestr, savepoint.impl(),
               origin_ptr, strides, num_strides)

        return field

    def wait_for_all(self):
        """ Wait for all pending asynchronous read operations and reset the internal queue.
        """
        invoke(lib.serialboxSerializerWaitForAll, self.__serializer)

    def read_slice(self, name, savepoint, slice_obj, field=None):
        """ Deserialize sliced `field` identified by `name`  and `slice` at `savepoint` from disk

        The method will allocate a `numpy.array` with the registered dimensions and type and fill it
        at specified positions (given by `slice_obj`) with data from disk.

        :param name: Name of the field
        :type name: str
        :param savepoint: Savepoint to at which the field will be serialized
        :type savepoint: Savepoint
        :param slice_obj: Slice of the data to load
        :type slice_obj: serialbox.slice
        :param field: Field to deserialize
        :type field: numpy.array

        :return: Newly allocated and deserialized field
        :rtype: numpy.array

        :raises SerialboxError: Deserialization failed
        """

        if self.mode != OpenModeKind.Read:
            raise SerialboxError("read operations are not permitted in OpenModeKind.%s" % self.mode)

        #
        # Allocate or check the field
        #
        field, info = self.__allocate_or_check_field(name, field)

        #
        # Extract strides and convert to unit-strides
        #
        strides, num_strides = self.__extract_strides(field)

        #
        # Construct slice array
        #
        dims = info.dims
        num_dims = len(info.dims)

        c_slice_array = (c_int * (3 * num_dims))()

        # Convert slice_obj to list
        slice_array = []
        if isinstance(slice_obj, slice):
            slice_array += [slice_obj]
        else:
            slice_array = list(slice_obj)

        if len(slice_array) > num_dims:
            raise SerialboxError("number of slices (%i) exceeds number of dimensions (%i)" % (
                len(slice_array), num_dims))

        while len(slice_array) != num_dims:
            slice_array += [slice(None, None, None)]

        for i in range(num_dims):
            slice_triple = slice_array[i]

            if isinstance(slice_triple, int):
                # start
                c_slice_array[3 * i] = slice_triple

                # stop
                c_slice_array[3 * i + 1] = slice_triple + 1

                # end
                c_slice_array[3 * i + 2] = 1
            else:
                # start
                c_slice_array[3 * i] = 0 if slice_triple.start is None else slice_triple.start

                # stop (expand negative values)
                if slice_triple.stop is None:
                    c_slice_array[3 * i + 1] = dims[i]
                elif slice_triple.stop >= 0:
                    c_slice_array[3 * i + 1] = slice_triple.stop
                else:
                    c_slice_array[3 * i + 1] = dims[i] + slice_triple.stop

                # end
                c_slice_array[3 * i + 2] = 1 if slice_triple.step is None else slice_triple.step

        #
        # Read from disk
        #
        origin_ptr = c_void_p(field.ctypes.data)
        namestr = extract_string(name)[0]
        invoke(lib.serialboxSerializerReadSliced, self.__serializer, namestr, savepoint.impl(),
               origin_ptr, strides, num_strides, c_slice_array)

        return field

    # ===----------------------------------------------------------------------------------------===
    #    Stateless Serialization
    # ==-----------------------------------------------------------------------------------------===

    @staticmethod
    def to_file(name, field, filename, archive=None):
        """ Serialize `field` identified by `name` directly to file.

        If a file with `filename` already exists, it's contents will be discarded. If `archive` is
        None, the method will try to deduce the archive using the extensions of `filename`
        (See :func:`Archive.archive_from_extension`).

        :param name: Name of the field
        :type name: str
        :param field: Field to serialize
        :type field: numpy.array
        :param name: Name of the file
        :type name: str

        :raises SerialboxError: Archive could not be dedcuded or serialization failed
        """
        strides, num_strides = Serializer.__extract_strides(field)
        dims, num_dims = Serializer.__extract_dims(field)

        if not archive:
            archive = Archive.archive_from_extension(filename)

        origin_ptr = c_void_p(field.ctypes.data)
        typeint = c_int(numpy2TypeID(field.dtype).value)
        namestr = extract_string(name)[0]
        filestr = extract_string(filename)[0]
        archivestr = extract_string(archive)[0]

        invoke(lib.serialboxWriteToFile, filestr, origin_ptr, typeint, dims, num_dims, strides,
               namestr, archivestr)

    def from_file(name, field, filename, archive=None):
        """ Deserialize `field` identified by `name` directly from file.

        If `archive` is None, the method will try to deduce the archive using the extensions of
        `filename` (See :func:`Archive.archive_from_extension`).

        .. warning::

           This method performs no consistency checks you have to know what you are doing!

        :param name: Name of the field
        :type name: str
        :param field: Field to serialize
        :type field: numpy.array
        :param name: Name of the file
        :type name: str
        :raises SerialboxError: Archive could not be deduced or deserialization failed
        """
        strides, num_strides = Serializer.__extract_strides(field)
        dims, num_dims = Serializer.__extract_dims(field)

        if not archive:
            archive = Archive.archive_from_extension(filename)

        origin_ptr = c_void_p(field.ctypes.data)
        typeint = c_int(numpy2TypeID(field.dtype).value)
        namestr = extract_string(name)[0]
        filestr = extract_string(filename)[0]
        archivestr = extract_string(archive)[0]

        invoke(lib.serialboxReadFromFile, filestr, origin_ptr, typeint, dims, num_dims, strides,
               namestr, archivestr)

    def __del__(self):
        if self.__serializer:
            invoke(lib.serialboxSerializerDestroy, self.__serializer)


register_library(lib)
