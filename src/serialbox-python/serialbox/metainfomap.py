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
## This file contains the metainfo implementation of the Python Interface.
##
##===------------------------------------------------------------------------------------------===##

from ctypes import c_void_p, c_bool, c_int, c_int32, c_int64, c_float, c_double, c_char_p, \
    Structure, POINTER

from .common import get_library, extract_string
from .error import invoke, SerialboxError
from .type import *

lib = get_library()


class MetaInfoImpl(Structure):
    """ Mapping of serialboxMetaInfo_t """
    _fields_ = [("impl", c_void_p), ("ownsData", c_int)]


class MetaInfoElementInfoImpl(Structure):
    """ Mapping of serialboxMetaInfoElementInfo_t """
    _fields_ = [("keys", POINTER(c_char_p)),
                ("types", POINTER(c_int)),
                ("len", c_int)]


class ArrayOfBooleanImpl(Structure):
    """ Mapping of serialboxArrayOfBoolean_t """
    _fields_ = [("data", POINTER(c_int)), ("len", c_int)]


class ArrayOfInt32Impl(Structure):
    """ Mapping of serialboxArrayOfInt32_t """
    _fields_ = [("data", POINTER(c_int32)), ("len", c_int)]


class ArrayOfInt64Impl(Structure):
    """ Mapping of serialboxArrayOfInt64_t """
    _fields_ = [("data", POINTER(c_int64)), ("len", c_int)]


class ArrayOfFloat32Impl(Structure):
    """ Mapping of serialboxArrayOfFloat32_t """
    _fields_ = [("data", POINTER(c_float)), ("len", c_int)]


class ArrayOfFloat64Impl(Structure):
    """ Mapping of serialboxArrayOfFloat64_t """
    _fields_ = [("data", POINTER(c_double)), ("len", c_int)]


class ArrayOfStringImpl(Structure):
    """ Mapping of serialboxArrayOfString_t """
    _fields_ = [("data", POINTER(c_char_p)), ("len", c_int)]


def register_library(library):
    #
    # Construction & Destruction
    #
    library.serialboxMetaInfoCreate.argtypes = None
    library.serialboxMetaInfoCreate.restype = POINTER(MetaInfoImpl)

    library.serialboxMetaInfoCreateFromMetaInfo.argtypes = [POINTER(MetaInfoImpl)]
    library.serialboxMetaInfoCreateFromMetaInfo.restype = POINTER(MetaInfoImpl)

    library.serialboxMetaInfoDestroy.argtypes = [POINTER(MetaInfoImpl)]
    library.serialboxMetaInfoDestroy.restype = None

    #
    # Utility
    #
    library.serialboxMetaInfoGetSize.argtypes = [POINTER(MetaInfoImpl)]
    library.serialboxMetaInfoGetSize.restype = c_int

    library.serialboxMetaInfoIsEmpty.argtypes = [POINTER(MetaInfoImpl)]
    library.serialboxMetaInfoIsEmpty.restype = c_int

    library.serialboxMetaInfoClear.argtypes = [POINTER(MetaInfoImpl)]
    library.serialboxMetaInfoClear.restype = None

    library.serialboxMetaInfoEqual.argtypes = [POINTER(MetaInfoImpl), POINTER(MetaInfoImpl)]
    library.serialboxMetaInfoEqual.restype = c_int

    library.serialboxMetaInfoHasKey.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoHasKey.restype = c_int

    library.serialboxMetaInfoGetTypeIDOfKey.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetTypeIDOfKey.restype = c_int

    library.serialboxMetaInfoToString.argtypes = [POINTER(MetaInfoImpl)]
    library.serialboxMetaInfoToString.restype = c_char_p

    library.serialboxMetaInfoCreateElementInfo.argtypes = [POINTER(MetaInfoImpl)]
    library.serialboxMetaInfoCreateElementInfo.restype = POINTER(MetaInfoElementInfoImpl)

    library.serialboxMetaInfoDestroyElementInfo.argtypes = [POINTER(MetaInfoElementInfoImpl)]
    library.serialboxMetaInfoDestroyElementInfo.restype = None

    #
    # Arrays
    #
    library.serialboxArrayOfBooleanCreate.argtypes = [c_int]
    library.serialboxArrayOfBooleanCreate.restype = POINTER(ArrayOfBooleanImpl)

    library.serialboxArrayOfInt32Create.argtypes = [c_int]
    library.serialboxArrayOfInt32Create.restype = POINTER(ArrayOfInt32Impl)

    library.serialboxArrayOfInt64Create.argtypes = [c_int]
    library.serialboxArrayOfInt64Create.restype = POINTER(ArrayOfInt64Impl)

    library.serialboxArrayOfFloat32Create.argtypes = [c_int]
    library.serialboxArrayOfFloat32Create.restype = POINTER(ArrayOfFloat32Impl)

    library.serialboxArrayOfFloat64Create.argtypes = [c_int]
    library.serialboxArrayOfFloat64Create.restype = POINTER(ArrayOfFloat64Impl)

    library.serialboxArrayOfStringCreate.argtypes = [c_int]
    library.serialboxArrayOfStringCreate.restype = POINTER(ArrayOfStringImpl)

    library.serialboxArrayOfBooleanDestroy.argtypes = [POINTER(ArrayOfBooleanImpl)]
    library.serialboxArrayOfBooleanDestroy.restype = None

    library.serialboxArrayOfInt32Destroy.argtypes = [POINTER(ArrayOfInt32Impl)]
    library.serialboxArrayOfInt32Destroy.restype = None

    library.serialboxArrayOfInt64Destroy.argtypes = [POINTER(ArrayOfInt64Impl)]
    library.serialboxArrayOfInt64Destroy.restype = None

    library.serialboxArrayOfFloat32Destroy.argtypes = [POINTER(ArrayOfFloat32Impl)]
    library.serialboxArrayOfFloat32Destroy.restype = None

    library.serialboxArrayOfFloat64Destroy.argtypes = [POINTER(ArrayOfFloat64Impl)]
    library.serialboxArrayOfFloat64Destroy.restype = None

    library.serialboxArrayOfStringDestroy.argtypes = [POINTER(ArrayOfStringImpl)]
    library.serialboxArrayOfStringDestroy.restype = None

    #
    # Add meta-information
    #
    library.serialboxMetaInfoAddBoolean.argtypes = [POINTER(MetaInfoImpl), c_char_p, c_bool]
    library.serialboxMetaInfoAddBoolean.restype = c_int

    library.serialboxMetaInfoAddInt32.argtypes = [POINTER(MetaInfoImpl), c_char_p, c_int32]
    library.serialboxMetaInfoAddInt32.restype = c_int

    library.serialboxMetaInfoAddInt64.argtypes = [POINTER(MetaInfoImpl), c_char_p, c_int64]
    library.serialboxMetaInfoAddInt64.restype = c_int

    library.serialboxMetaInfoAddFloat32.argtypes = [POINTER(MetaInfoImpl), c_char_p, c_float]
    library.serialboxMetaInfoAddFloat32.restype = c_int

    library.serialboxMetaInfoAddFloat64.argtypes = [POINTER(MetaInfoImpl), c_char_p, c_double]
    library.serialboxMetaInfoAddFloat64.restype = c_int

    library.serialboxMetaInfoAddString.argtypes = [POINTER(MetaInfoImpl), c_char_p, c_char_p]
    library.serialboxMetaInfoAddString.restype = c_int

    library.serialboxMetaInfoAddArrayOfBoolean.argtypes = [POINTER(MetaInfoImpl), c_char_p,
                                                           POINTER(ArrayOfBooleanImpl)]
    library.serialboxMetaInfoAddArrayOfBoolean.restype = c_int

    library.serialboxMetaInfoAddArrayOfInt32.argtypes = [POINTER(MetaInfoImpl), c_char_p,
                                                         POINTER(ArrayOfInt32Impl)]
    library.serialboxMetaInfoAddArrayOfInt32.restype = c_int

    library.serialboxMetaInfoAddArrayOfInt64.argtypes = [POINTER(MetaInfoImpl), c_char_p,
                                                         POINTER(ArrayOfInt64Impl)]
    library.serialboxMetaInfoAddArrayOfInt64.restype = c_int

    library.serialboxMetaInfoAddArrayOfFloat32.argtypes = [POINTER(MetaInfoImpl), c_char_p,
                                                           POINTER(ArrayOfFloat32Impl)]
    library.serialboxMetaInfoAddArrayOfFloat32.restype = c_int

    library.serialboxMetaInfoAddArrayOfFloat64.argtypes = [POINTER(MetaInfoImpl), c_char_p,
                                                           POINTER(ArrayOfFloat64Impl)]
    library.serialboxMetaInfoAddArrayOfFloat64.restype = c_int

    library.serialboxMetaInfoAddArrayOfString.argtypes = [POINTER(MetaInfoImpl), c_char_p,
                                                          POINTER(ArrayOfStringImpl)]
    library.serialboxMetaInfoAddArrayOfString.restype = c_int

    #
    # Query meta-information
    #
    library.serialboxMetaInfoGetBoolean.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetBoolean.restype = c_int

    library.serialboxMetaInfoGetInt32.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetInt32.restype = c_int32

    library.serialboxMetaInfoGetInt64.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetInt64.restype = c_int64

    library.serialboxMetaInfoGetFloat32.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetFloat32.restype = c_float

    library.serialboxMetaInfoGetFloat64.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetFloat64.restype = c_double

    library.serialboxMetaInfoGetString.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetString.restype = c_char_p

    library.serialboxMetaInfoGetArrayOfBoolean.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetArrayOfBoolean.restype = POINTER(ArrayOfBooleanImpl)

    library.serialboxMetaInfoGetArrayOfInt32.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetArrayOfInt32.restype = POINTER(ArrayOfInt32Impl)

    library.serialboxMetaInfoGetArrayOfInt64.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetArrayOfInt64.restype = POINTER(ArrayOfInt64Impl)

    library.serialboxMetaInfoGetArrayOfFloat32.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetArrayOfFloat32.restype = POINTER(ArrayOfFloat32Impl)

    library.serialboxMetaInfoGetArrayOfFloat64.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetArrayOfFloat64.restype = POINTER(ArrayOfFloat64Impl)

    library.serialboxMetaInfoGetArrayOfString.argtypes = [POINTER(MetaInfoImpl), c_char_p]
    library.serialboxMetaInfoGetArrayOfString.restype = POINTER(ArrayOfStringImpl)


class MetaInfoMapIterator(object):
    """Iterator of the MetaInfoMap

    TODO
    """

    def __init__(self, metainfomap):
        self.__metainfomap = metainfomap
        self.__idx = 0
        self.__elements = invoke(lib.serialboxMetaInfoCreateElementInfo, metainfomap.impl())

    def __next__(self):
        if self.__idx >= self.__elements.contents.len:
            raise StopIteration
        else:
            i = self.__idx
            self.__idx += 1
            return (self.__elements.contents.keys[i].decode(),
                    self.__metainfomap.__getitem__(
                        self.__elements.contents.keys[i].decode(),
                        self.__elements.contents.types[i]))

    def __del__(self):
        invoke(lib.serialboxMetaInfoDestroyElementInfo, self.__elements)


class MetaInfoMap(object):
    """Meta-information implementation of the Python Interface.

    Objects of this class contain a map of meta-information in form of `key = value` or
    `key = {value1, ... valueN}` pair. The keys are strings and unique, while the values can be
    integers, booleans, floating point numbers (either single or double precision) or strings.
    """

    def __init__(self, metainfo=None, impl=None):
        """Initialize the MetaInfo map.

        :param metainfo: dict -- Key-value pair dictionary used for initialization
        :param impl: Directly set the implementation pointer (internal use)
        """
        if impl:
            self.__metainfomap = impl
        else:
            self.__metainfomap = invoke(lib.serialboxMetaInfoCreate)

        if metainfo:
            for key, value in metainfo.items():
                self.insert(key, value)

    def clone(self):
        """Clone the MetaInfo map by performing a deepcopy.

        :return: Clone of the savepoint
        :rtype: Savepoint
        """
        return MetaInfoMap(impl=invoke(lib.serialboxMetaInfoCreateFromMetaInfo, self.__metainfomap))

    def insert(self, key, value, typeid=None):
        """Insert a new element in the form `key = value` or `key = {value1, ... valueN}` pair.

        The element is inserted only if its key is not equivalent to the key of any other element
        already in the map (i.e keys must be unique).

        If the optional parameter `typeid` is omitted, the function will try it's best effort to
        deduce the typeid, otherwise the value will be converted to match the type of `typeid`.

        :param key: str -- Key of the new element
        :param value: Object to be copied to the value of the new element
        :param typeid: serialbox.TypeID, int -- Type-id to use (optional)
        :raises: SerialboxError -- Element with key already exists or typeid could not be deduced
        :raises: TypeError -- typeid is not a serialbox.TypeID or int
        """
        isArray = False
        keystr = extract_string(key)[0]

        if typeid and isinstance(typeid, TypeID):
            typeid = typeid.value
        elif typeid and not isinstance(typeid, int):
            raise TypeError(
                "param 'typeid' must be of type serialbox.TypeID or int (type: %s)" % type(typeid))

        #
        # Deduce typeid
        #
        if not typeid:

            # Check if type is iterable
            if isinstance(value, list):
                isArray = True
                valueToDeduce = value[0]
            else:
                valueToDeduce = value

            # Deduce primitve type
            if isinstance(valueToDeduce, BooleanTypes):
                typeid = TypeID.Boolean.value
            elif isinstance(valueToDeduce, Int32Types):
                typeid = TypeID.Int32.value
            elif isinstance(valueToDeduce, Int64Types):
                typeid = TypeID.Int64.value
            elif isinstance(valueToDeduce, Float32Types):
                typeid = TypeID.Float32.value
            elif isinstance(valueToDeduce, Float64Types):
                typeid = TypeID.Float64.value
            elif isinstance(valueToDeduce, StringTypes):
                typeid = TypeID.String.value
            else:
                raise SerialboxError(
                    "could not deduce type-id of key '%s' (python type: %s)" % (key, type(value)))

            if isArray:
                typeid |= TypeID.Array.value

        #
        # Insert into map
        #
        ret = 0
        if typeid is TypeID.Boolean.value:
            ret = invoke(lib.serialboxMetaInfoAddBoolean, self.__metainfomap, keystr, c_bool(value))

        elif typeid is TypeID.Int32.value:
            ret = invoke(lib.serialboxMetaInfoAddInt32, self.__metainfomap, keystr, c_int32(value))

        elif typeid is TypeID.Int64.value:
            ret = invoke(lib.serialboxMetaInfoAddInt64, self.__metainfomap, keystr, c_int64(value))

        elif typeid is TypeID.Float32.value:
            ret = invoke(lib.serialboxMetaInfoAddFloat32, self.__metainfomap, keystr,
                         c_float(value))

        elif typeid is TypeID.Float64.value:
            ret = invoke(lib.serialboxMetaInfoAddFloat64, self.__metainfomap, keystr,
                         c_double(value))

        elif typeid is TypeID.String.value:
            ret = invoke(lib.serialboxMetaInfoAddString, self.__metainfomap, keystr,
                         extract_string(value)[0])

        elif typeid is TypeID.ArrayOfBoolean.value:
            array = invoke(lib.serialboxArrayOfBooleanCreate, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_int(value[i])
            ret = invoke(lib.serialboxMetaInfoAddArrayOfBoolean, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfBooleanDestroy, array)

        elif typeid is TypeID.ArrayOfInt32.value:
            array = invoke(lib.serialboxArrayOfInt32Create, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_int32(value[i])
            ret = invoke(lib.serialboxMetaInfoAddArrayOfInt32, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfInt32Destroy, array)

        elif typeid is TypeID.ArrayOfInt64.value:
            array = invoke(lib.serialboxArrayOfInt64Create, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_int64(value[i])
            ret = invoke(lib.serialboxMetaInfoAddArrayOfInt64, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfInt64Destroy, array)

        elif typeid is TypeID.ArrayOfFloat32.value:
            array = invoke(lib.serialboxArrayOfFloat32Create, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_float(value[i])
            ret = invoke(lib.serialboxMetaInfoAddArrayOfFloat32, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfFloat32Destroy, array)

        elif typeid is TypeID.ArrayOfFloat64.value:
            array = invoke(lib.serialboxArrayOfFloat64Create, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_double(value[i])
            ret = invoke(lib.serialboxMetaInfoAddArrayOfFloat64, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfFloat64Destroy, array)

        elif typeid is TypeID.ArrayOfString.value:
            array = invoke(lib.serialboxArrayOfStringCreate, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = extract_string(value[i])[0]
            ret = invoke(lib.serialboxMetaInfoAddArrayOfString, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfStringDestroy, array)

        else:
            raise SerialboxError('internal error: unreachable (typeid = %s)' % (typeid))

        if not ret:
            raise SerialboxError("cannot insert key '%s': key already exists" % key)

    def size(self):
        """Get number of elements in the meta-information.

        :return: Number of elements in the meta-information
        :rtype: int
        """
        return invoke(lib.serialboxMetaInfoGetSize, self.__metainfomap)

    def empty(self):
        """Check if meta information is empty.

        :return: True if map is empty, False otherwise
        :rtype: bool
        """
        return bool(invoke(lib.serialboxMetaInfoIsEmpty, self.__metainfomap))

    def has_key(self, key):
        """Check if and element with key `key` exists.

        :param key: str -- Key of the element
        :return: True if element with `key` exists, False otherwise
        :rtype: bool
        """
        keystr = extract_string(key)[0]
        return bool(invoke(lib.serialboxMetaInfoHasKey, self.__metainfomap, keystr))

    def clear(self):
        """Clear the map.

        All the elements in the MetaInfoMap are dropped: their destructors are called, and they are
        removed from the container, leaving it with a size of 0.
        """
        invoke(lib.serialboxMetaInfoClear, self.__metainfomap)

    def to_dict(self):
        """Convert MetaInfoMap to python builtin dictionary

        The MetaInfoMap is `copied` into the dictionary

        :return: copy of the MetaInfo map as a dictionary
        :rtype: dict
        """
        elements = invoke(lib.serialboxMetaInfoCreateElementInfo, self.__metainfomap)

        dic = {}
        for i in range(elements.contents.len):
            dic[elements.contents.keys[i].decode()] = self.__getitem__(
                elements.contents.keys[i].decode(), elements.contents.types[i])

        invoke(lib.serialboxMetaInfoDestroyElementInfo, elements)
        return dic

    def __eq__(self, other):
        """Test for equality.

        :return: True if self == other, False otherwise
        :rtype: bool
        """
        return bool(invoke(lib.serialboxMetaInfoEqual, self.__metainfomap, other.__metainfomap))

    def __ne__(self, other):
        """Test for inequality.

        :return: True if self != other, False otherwise
        :rtype: bool
        """
        return not self.__eq__(other)

    def __getitem__(self, key, typeid=None):
        """Get `value` of element given by `key`.

        :param key: str -- Key of the element
        :param typeid: int -- Type-id (internal use)
        :return: Copy of the value of the element
        :raises: SerialboxError -- Element with `key` does not exist
        """
        if type(key) not in StringTypes:
            raise TypeError("parameter 'key' is not a string (type: %s)" % type(key))
        keystr = extract_string(key)[0]

        #
        # Get typeid
        #
        if not typeid:
            typeid = invoke(lib.serialboxMetaInfoGetTypeIDOfKey, self.__metainfomap, keystr)

        #
        # Conversions
        #
        if typeid is TypeID.Boolean.value:
            return bool(invoke(lib.serialboxMetaInfoGetBoolean, self.__metainfomap, keystr))

        elif typeid is TypeID.Int32.value:
            return int(invoke(lib.serialboxMetaInfoGetInt32, self.__metainfomap, keystr))

        elif typeid is TypeID.Int64.value:
            return int(invoke(lib.serialboxMetaInfoGetInt64, self.__metainfomap, keystr))

        elif typeid is TypeID.Float32.value:
            return float(invoke(lib.serialboxMetaInfoGetFloat32, self.__metainfomap, keystr))

        elif typeid is TypeID.Float64.value:
            return float(invoke(lib.serialboxMetaInfoGetFloat64, self.__metainfomap, keystr))

        elif typeid is TypeID.String.value:
            return invoke(lib.serialboxMetaInfoGetString, self.__metainfomap, keystr).decode()

        elif typeid is TypeID.ArrayOfBoolean.value:
            array = invoke(lib.serialboxMetaInfoGetArrayOfBoolean, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [bool(array.contents.data[i])]
            invoke(lib.serialboxArrayOfBooleanDestroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfInt32.value:
            array = invoke(lib.serialboxMetaInfoGetArrayOfInt32, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [int(array.contents.data[i])]
            invoke(lib.serialboxArrayOfInt32Destroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfInt64.value:
            array = invoke(lib.serialboxMetaInfoGetArrayOfInt64, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [int(array.contents.data[i])]
            invoke(lib.serialboxArrayOfInt64Destroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfFloat32.value:
            array = invoke(lib.serialboxMetaInfoGetArrayOfFloat32, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [float(array.contents.data[i])]
            invoke(lib.serialboxArrayOfFloat32Destroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfFloat64.value:
            array = invoke(lib.serialboxMetaInfoGetArrayOfFloat64, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [float(array.contents.data[i])]
            invoke(lib.serialboxArrayOfFloat64Destroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfString.value:
            array = invoke(lib.serialboxMetaInfoGetArrayOfString, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [array.contents.data[i].decode()]
            invoke(lib.serialboxArrayOfStringDestroy, array)
            return list_array

        else:
            raise SerialboxError('internal error: unreachable (typeid = %i)' % typeid)

    def __iter__(self):
        """ Construct the MetaInfoMap iterator.

        :return: Iterator of MetaInfoMap
        :rtype: MetaInfoMapIterator
        """
        return MetaInfoMapIterator(self)

    def impl(self):
        """Get implementation pointer.
        """
        return self.__metainfomap

    def __del__(self):
        invoke(lib.serialboxMetaInfoDestroy, self.__metainfomap)

    def __repr__(self):
        return "<MetaInfoMap {0}>".format(self.__str__())

    def __str__(self):
        return invoke(lib.serialboxMetaInfoToString, self.__metainfomap).decode()


register_library(lib)
