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

from ctypes import (c_void_p, c_bool, c_int, c_int32, c_int64, c_float, c_double, c_char_p,
                    Structure, POINTER)

from .common import get_library, to_c_string
from .error import invoke, SerialboxError
from .type import *

lib = get_library()


class MetainfoImpl(Structure):
    """ Mapping of serialboxMetainfo_t """
    _fields_ = [("impl", c_void_p), ("ownsData", c_int)]


class MetainfoElementInfoImpl(Structure):
    """ Mapping of serialboxMetainfoElementInfo_t """
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
    library.serialboxMetainfoCreate.argtypes = None
    library.serialboxMetainfoCreate.restype = POINTER(MetainfoImpl)

    library.serialboxMetainfoCreateFromMetainfo.argtypes = [POINTER(MetainfoImpl)]
    library.serialboxMetainfoCreateFromMetainfo.restype = POINTER(MetainfoImpl)

    library.serialboxMetainfoDestroy.argtypes = [POINTER(MetainfoImpl)]
    library.serialboxMetainfoDestroy.restype = None

    #
    # Utility
    #
    library.serialboxMetainfoGetSize.argtypes = [POINTER(MetainfoImpl)]
    library.serialboxMetainfoGetSize.restype = c_int

    library.serialboxMetainfoIsEmpty.argtypes = [POINTER(MetainfoImpl)]
    library.serialboxMetainfoIsEmpty.restype = c_int

    library.serialboxMetainfoClear.argtypes = [POINTER(MetainfoImpl)]
    library.serialboxMetainfoClear.restype = None

    library.serialboxMetainfoEqual.argtypes = [POINTER(MetainfoImpl), POINTER(MetainfoImpl)]
    library.serialboxMetainfoEqual.restype = c_int

    library.serialboxMetainfoHasKey.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoHasKey.restype = c_int

    library.serialboxMetainfoGetTypeIDOfKey.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetTypeIDOfKey.restype = c_int

    library.serialboxMetainfoToString.argtypes = [POINTER(MetainfoImpl)]
    library.serialboxMetainfoToString.restype = c_char_p

    library.serialboxMetainfoCreateElementInfo.argtypes = [POINTER(MetainfoImpl)]
    library.serialboxMetainfoCreateElementInfo.restype = POINTER(MetainfoElementInfoImpl)

    library.serialboxMetainfoDestroyElementInfo.argtypes = [POINTER(MetainfoElementInfoImpl)]
    library.serialboxMetainfoDestroyElementInfo.restype = None

    library.serialboxMetainfoDeleteKey.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoDeleteKey.restype = c_int

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
    library.serialboxMetainfoAddBoolean.argtypes = [POINTER(MetainfoImpl), c_char_p, c_bool]
    library.serialboxMetainfoAddBoolean.restype = c_int

    library.serialboxMetainfoAddInt32.argtypes = [POINTER(MetainfoImpl), c_char_p, c_int32]
    library.serialboxMetainfoAddInt32.restype = c_int

    library.serialboxMetainfoAddInt64.argtypes = [POINTER(MetainfoImpl), c_char_p, c_int64]
    library.serialboxMetainfoAddInt64.restype = c_int

    library.serialboxMetainfoAddFloat32.argtypes = [POINTER(MetainfoImpl), c_char_p, c_float]
    library.serialboxMetainfoAddFloat32.restype = c_int

    library.serialboxMetainfoAddFloat64.argtypes = [POINTER(MetainfoImpl), c_char_p, c_double]
    library.serialboxMetainfoAddFloat64.restype = c_int

    library.serialboxMetainfoAddString.argtypes = [POINTER(MetainfoImpl), c_char_p, c_char_p]
    library.serialboxMetainfoAddString.restype = c_int

    library.serialboxMetainfoAddArrayOfBoolean.argtypes = [POINTER(MetainfoImpl), c_char_p,
                                                           POINTER(ArrayOfBooleanImpl)]
    library.serialboxMetainfoAddArrayOfBoolean.restype = c_int

    library.serialboxMetainfoAddArrayOfInt32.argtypes = [POINTER(MetainfoImpl), c_char_p,
                                                         POINTER(ArrayOfInt32Impl)]
    library.serialboxMetainfoAddArrayOfInt32.restype = c_int

    library.serialboxMetainfoAddArrayOfInt64.argtypes = [POINTER(MetainfoImpl), c_char_p,
                                                         POINTER(ArrayOfInt64Impl)]
    library.serialboxMetainfoAddArrayOfInt64.restype = c_int

    library.serialboxMetainfoAddArrayOfFloat32.argtypes = [POINTER(MetainfoImpl), c_char_p,
                                                           POINTER(ArrayOfFloat32Impl)]
    library.serialboxMetainfoAddArrayOfFloat32.restype = c_int

    library.serialboxMetainfoAddArrayOfFloat64.argtypes = [POINTER(MetainfoImpl), c_char_p,
                                                           POINTER(ArrayOfFloat64Impl)]
    library.serialboxMetainfoAddArrayOfFloat64.restype = c_int

    library.serialboxMetainfoAddArrayOfString.argtypes = [POINTER(MetainfoImpl), c_char_p,
                                                          POINTER(ArrayOfStringImpl)]
    library.serialboxMetainfoAddArrayOfString.restype = c_int

    #
    # Query meta-information
    #
    library.serialboxMetainfoGetBoolean.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetBoolean.restype = c_int

    library.serialboxMetainfoGetInt32.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetInt32.restype = c_int32

    library.serialboxMetainfoGetInt64.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetInt64.restype = c_int64

    library.serialboxMetainfoGetFloat32.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetFloat32.restype = c_float

    library.serialboxMetainfoGetFloat64.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetFloat64.restype = c_double

    library.serialboxMetainfoGetString.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetString.restype = c_char_p

    library.serialboxMetainfoGetArrayOfBoolean.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetArrayOfBoolean.restype = POINTER(ArrayOfBooleanImpl)

    library.serialboxMetainfoGetArrayOfInt32.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetArrayOfInt32.restype = POINTER(ArrayOfInt32Impl)

    library.serialboxMetainfoGetArrayOfInt64.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetArrayOfInt64.restype = POINTER(ArrayOfInt64Impl)

    library.serialboxMetainfoGetArrayOfFloat32.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetArrayOfFloat32.restype = POINTER(ArrayOfFloat32Impl)

    library.serialboxMetainfoGetArrayOfFloat64.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetArrayOfFloat64.restype = POINTER(ArrayOfFloat64Impl)

    library.serialboxMetainfoGetArrayOfString.argtypes = [POINTER(MetainfoImpl), c_char_p]
    library.serialboxMetainfoGetArrayOfString.restype = POINTER(ArrayOfStringImpl)


class MetainfoMapIterator(object):
    """Iterator of the MetainfoMap
    """

    def __init__(self, metainfomap):
        self.__metainfomap = metainfomap
        self.__idx = 0
        self.__elements = invoke(lib.serialboxMetainfoCreateElementInfo, metainfomap.impl())

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
        invoke(lib.serialboxMetainfoDestroyElementInfo, self.__elements)


class MetainfoMap(object):
    """Meta-information implementation of the Python Interface.

    Objects of this class contain a map of meta-information in form of `key = value` or
    `key = {value1, ... valueN}` pair. The keys are strings and unique, while the values can be
    integers, booleans, floating point numbers (either single or double precision) or strings.

    The elements are internally stored as a hash-map and thus the order of insertion is irrelevant.
    The MetainfoMaps can be constrcuted from python dictionary :class:`dict`.

        >>> m = MetainfoMap({'key1': 1, 'key2': 'str'})
        >>> m
        <MetainfoMap {"key2": str, "key": 5}>
        >>>

    """

    def __init__(self, metainfo=None, impl=None):
        """Initialize the Metainfo map.

        If `metainfo` is None, an empty map is created. Elements can be added later with
        :func:`MetainfoMap.insert <serialbox.MetainfoMap.insert>`.

        :param metainfo: Key-value pair dictionary used for initialization
        :type metainfo: dict
        :param impl: Directly set the implementation pointer [internal use]
        """
        if impl:
            self.__metainfomap = impl
        else:
            self.__metainfomap = invoke(lib.serialboxMetainfoCreate)

        if metainfo:
            for key, value in metainfo.items():
                self.insert(key, value)

    def clone(self):
        """Clone the Metainfo map by performing a deepcopy.

            >>> m = MetainfoMap({'key1': 1, 'key2': 'str'})
            >>> m_clone = m.clone()
            >>> m.clear()
            >>> m
            <MetainfoMap {}>
            >>> m_clone
            <MetainfoMap {"key2": str, "key": 5}>

        :return: Clone of the map
        :rtype: MetainfoMap
        """
        return MetainfoMap(impl=invoke(lib.serialboxMetainfoCreateFromMetainfo, self.__metainfomap))

    def insert(self, key, value, typeid=None):
        """Insert a new element in the form `key = value` or `key = {value1, ... valueN}` pair.

        The element is inserted only if its `key` is not equivalent to the `key` of any other
        element already in the map (i.e keys must be unique). If the optional parameter `typeid` is
        omitted, the function will try it's best effort to deduce the typeid, otherwise the value
        will be converted to match the type of `typeid` (see :class:`TypeID <serialbox.TypeID>`).

            >>> m = MetainfoMap()
            >>> m.insert('key', 5)
            >>> m
            <MetainfoMap {"key": 5}>
            >>> m.insert('Array', [1, 2, 3, 4])
            >>> m
            <MetainfoMap {"Array": [1, 2, 3, 4], "key": 5}>
            >>> m.insert('Float', 5.0, TypeID.Float32)
            >>> m
            <MetainfoMap {"Array": [1, 2, 3, 4], "Float": 5.000000, "key": 5}>
            >>> type(m['Float'])
            <class 'float'>

        :param key: Key of the new element
        :type key: str
        :param value: Object to be copied to the value of the new element
        :param typeid: Type-id to use
        :type typeid: serialbox.TypeID
        :raises SerialboxError: if element with key already exists or typeid could not be deduced
        :raises TypeError: if `typeid` is not a serialbox.TypeID or int
        """
        isArray = False
        keystr = to_c_string(key)[0]

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
            ret = invoke(lib.serialboxMetainfoAddBoolean, self.__metainfomap, keystr, c_bool(value))

        elif typeid is TypeID.Int32.value:
            ret = invoke(lib.serialboxMetainfoAddInt32, self.__metainfomap, keystr, c_int32(value))

        elif typeid is TypeID.Int64.value:
            ret = invoke(lib.serialboxMetainfoAddInt64, self.__metainfomap, keystr, c_int64(value))

        elif typeid is TypeID.Float32.value:
            ret = invoke(lib.serialboxMetainfoAddFloat32, self.__metainfomap, keystr,
                         c_float(value))

        elif typeid is TypeID.Float64.value:
            ret = invoke(lib.serialboxMetainfoAddFloat64, self.__metainfomap, keystr,
                         c_double(value))

        elif typeid is TypeID.String.value:
            ret = invoke(lib.serialboxMetainfoAddString, self.__metainfomap, keystr,
                         to_c_string(value)[0])

        elif typeid is TypeID.ArrayOfBoolean.value:
            array = invoke(lib.serialboxArrayOfBooleanCreate, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_int(value[i])
            ret = invoke(lib.serialboxMetainfoAddArrayOfBoolean, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfBooleanDestroy, array)

        elif typeid is TypeID.ArrayOfInt32.value:
            array = invoke(lib.serialboxArrayOfInt32Create, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_int32(value[i])
            ret = invoke(lib.serialboxMetainfoAddArrayOfInt32, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfInt32Destroy, array)

        elif typeid is TypeID.ArrayOfInt64.value:
            array = invoke(lib.serialboxArrayOfInt64Create, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_int64(value[i])
            ret = invoke(lib.serialboxMetainfoAddArrayOfInt64, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfInt64Destroy, array)

        elif typeid is TypeID.ArrayOfFloat32.value:
            array = invoke(lib.serialboxArrayOfFloat32Create, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_float(value[i])
            ret = invoke(lib.serialboxMetainfoAddArrayOfFloat32, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfFloat32Destroy, array)

        elif typeid is TypeID.ArrayOfFloat64.value:
            array = invoke(lib.serialboxArrayOfFloat64Create, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = c_double(value[i])
            ret = invoke(lib.serialboxMetainfoAddArrayOfFloat64, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfFloat64Destroy, array)

        elif typeid is TypeID.ArrayOfString.value:
            array = invoke(lib.serialboxArrayOfStringCreate, c_int(len(value)))
            for i in range(array.contents.len):
                array.contents.data[i] = to_c_string(value[i])[0]
            ret = invoke(lib.serialboxMetainfoAddArrayOfString, self.__metainfomap, keystr, array)
            invoke(lib.serialboxArrayOfStringDestroy, array)

        else:
            raise SerialboxError('internal error: unreachable (typeid = %s)' % (typeid))

        if not ret:
            raise SerialboxError("cannot insert key '%s': key already exists" % key)

    def size(self):
        """Get number of elements in the map.

        :return: Number of elements in the map
        :rtype: int
        """
        return invoke(lib.serialboxMetainfoGetSize, self.__metainfomap)

    def empty(self):
        """Check if mao is empty.

        :return: `True` if map is empty, `False` otherwise
        :rtype: bool
        """
        return bool(invoke(lib.serialboxMetainfoIsEmpty, self.__metainfomap))

    def has_key(self, key):
        """Check if and element with key `key` exists.

        :param key: Key of the element
        :type key: str
        :return: True if element with `key` exists, False otherwise
        :rtype: bool
        """
        keystr = to_c_string(key)[0]
        return bool(invoke(lib.serialboxMetainfoHasKey, self.__metainfomap, keystr))

    def clear(self):
        """Clear the map.

        All the elements in the MetainfoMap are dropped: their destructors are called, and they are
        removed from the container, leaving it with a size of 0.
        """
        invoke(lib.serialboxMetainfoClear, self.__metainfomap)

    def to_dict(self):
        """Convert MetainfoMap to a python dictionary :class:`dict`.

        The MetainfoMap is `copied` into the dictionary.

            >>> d = {'key': 5, 'string': 'str'}
            >>> m = MetainfoMap(d)
            >>> map_as_dict = m.to_dict()
            >>> map_as_dict
            {'key': 5, 'string': 'str'}
            >>> d == map_as_dict
            True

        :return: copy of the Metainfo map as a dictionary
        :rtype: dict
        """
        elements = invoke(lib.serialboxMetainfoCreateElementInfo, self.__metainfomap)

        dic = {}
        for i in range(elements.contents.len):
            dic[elements.contents.keys[i].decode()] = self.__getitem__(
                elements.contents.keys[i].decode(), elements.contents.types[i])

        invoke(lib.serialboxMetainfoDestroyElementInfo, elements)
        return dic

    def __eq__(self, other):
        """Test for equality.

        MetainfoMaps are equal if all their elements are equal.

        :return: `True` if self == other, `False` otherwise
        :rtype: bool
        """
        return bool(invoke(lib.serialboxMetainfoEqual, self.__metainfomap, other.__metainfomap))

    def __ne__(self, other):
        """Test for inequality.

        MetainfoMaps are equal if all their elements are equal.

        :return: `True` if self != other, `False` otherwise
        :rtype: bool
        """
        return not self.__eq__(other)

    def __getitem__(self, key, typeid=None):
        """Get `value` of element given by `key`. The correct type will be inferred.

            >>> m = MetainfoMap()
            >>> m.insert('key', 5)
            >>> m['key']
            5
            >>>

        :param key: Key of the element
        :type key: str
        :param typeid: Type-id [internal use]
        :type typeid: int
        :return: Copy of the value of the element
        :raises serialbox.SerialboxError: if Element with `key` does not exist
        """
        if type(key) not in StringTypes:
            raise TypeError("parameter 'key' is not a string (type: %s)" % type(key))
        keystr = to_c_string(key)[0]

        #
        # Get typeid
        #
        if not typeid:
            typeid = invoke(lib.serialboxMetainfoGetTypeIDOfKey, self.__metainfomap, keystr)

        #
        # Conversions
        #
        if typeid is TypeID.Boolean.value:
            return bool(invoke(lib.serialboxMetainfoGetBoolean, self.__metainfomap, keystr))

        elif typeid is TypeID.Int32.value:
            return int(invoke(lib.serialboxMetainfoGetInt32, self.__metainfomap, keystr))

        elif typeid is TypeID.Int64.value:
            return int(invoke(lib.serialboxMetainfoGetInt64, self.__metainfomap, keystr))

        elif typeid is TypeID.Float32.value:
            return float(invoke(lib.serialboxMetainfoGetFloat32, self.__metainfomap, keystr))

        elif typeid is TypeID.Float64.value:
            return float(invoke(lib.serialboxMetainfoGetFloat64, self.__metainfomap, keystr))

        elif typeid is TypeID.String.value:
            return invoke(lib.serialboxMetainfoGetString, self.__metainfomap, keystr).decode()

        elif typeid is TypeID.ArrayOfBoolean.value:
            array = invoke(lib.serialboxMetainfoGetArrayOfBoolean, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [bool(array.contents.data[i])]
            invoke(lib.serialboxArrayOfBooleanDestroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfInt32.value:
            array = invoke(lib.serialboxMetainfoGetArrayOfInt32, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [int(array.contents.data[i])]
            invoke(lib.serialboxArrayOfInt32Destroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfInt64.value:
            array = invoke(lib.serialboxMetainfoGetArrayOfInt64, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [int(array.contents.data[i])]
            invoke(lib.serialboxArrayOfInt64Destroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfFloat32.value:
            array = invoke(lib.serialboxMetainfoGetArrayOfFloat32, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [float(array.contents.data[i])]
            invoke(lib.serialboxArrayOfFloat32Destroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfFloat64.value:
            array = invoke(lib.serialboxMetainfoGetArrayOfFloat64, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [float(array.contents.data[i])]
            invoke(lib.serialboxArrayOfFloat64Destroy, array)
            return list_array

        elif typeid is TypeID.ArrayOfString.value:
            array = invoke(lib.serialboxMetainfoGetArrayOfString, self.__metainfomap, keystr)
            list_array = []
            for i in range(array.contents.len):
                list_array += [array.contents.data[i].decode()]
            invoke(lib.serialboxArrayOfStringDestroy, array)
            return list_array

        else:
            raise SerialboxError('internal error: unreachable (typeid = %i)' % typeid)

    def __delitem__(self, key):
        if type(key) not in StringTypes:
            raise TypeError("parameter 'key' is not a string (type: %s)" % type(key))
        keystr = to_c_string(key)[0]
        invoke(lib.serialboxMetainfoDeleteKey, self.__metainfomap, keystr)

    def __iter__(self):
        """ Iterate the MetainfoMap.

            >>> m = MetainfoMap({'key1': 5, 'key2': 6})
            >>> for elements in m:
                ...     print(elements)
                ('key1', 5)
                ('key2', 6)
            >>>

        :return: Iterator of MetainfoMap
        :rtype: MetainfoMapIterator
        """
        return MetainfoMapIterator(self)

    def impl(self):
        return self.__metainfomap

    def __del__(self):
        invoke(lib.serialboxMetainfoDestroy, self.__metainfomap)

    def __repr__(self):
        return "<MetainfoMap {0}>".format(self.__str__())

    def __str__(self):
        return invoke(lib.serialboxMetainfoToString, self.__metainfomap).decode()


register_library(lib)
