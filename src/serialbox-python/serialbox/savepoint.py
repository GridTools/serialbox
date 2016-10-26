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
## This file contains the savepoint implementation of the Python Interface.
##
##===------------------------------------------------------------------------------------------===##

from abc import ABCMeta
from ctypes import c_char_p, c_void_p, c_int, Structure, POINTER

from .common import get_library, extract_string
from .error import invoke, SerialboxError
from .metainfomap import MetaInfoMap, MetaInfoImpl
from .type import StringTypes
from .util import levenshtein

lib = get_library()


class SavepointImpl(Structure):
    """ Mapping of serialboxSavepoint_t """
    _fields_ = [("impl", c_void_p), ("ownsData", c_int)]


def register_library(library):
    library.serialboxSavepointCreate.argtypes = [c_char_p]
    library.serialboxSavepointCreate.restype = POINTER(SavepointImpl)

    library.serialboxSavepointCreateFromSavepoint.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointCreateFromSavepoint.restype = POINTER(SavepointImpl)

    library.serialboxSavepointDestroy.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointDestroy.restype = None

    library.serialboxSavepointGetName.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointGetName.restype = c_char_p

    library.serialboxSavepointEqual.argtypes = [POINTER(SavepointImpl), POINTER(SavepointImpl)]
    library.serialboxSavepointEqual.restype = c_int

    library.serialboxSavepointToString.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointToString.restype = c_char_p

    library.serialboxSavepointGetMetaInfo.argtypes = [POINTER(SavepointImpl)]
    library.serialboxSavepointGetMetaInfo.restype = POINTER(MetaInfoImpl)


# ===--------------------------------------------------------------------------------------------===
#   Savepoint
# ==---------------------------------------------------------------------------------------------===

class Savepoint(object):
    """Savepoint implementation of the Python Interface.

    Savepoints are primarily identified by their `name` and further distinguished by their
    `meta_info`. Savepoints are used within the :class:`Serializer` to discriminate fields
    at different points in time.
    """

    def __init__(self, name, metainfo=None, impl=None):
        """Initialize the Savepoint.

        This method prepares the savepoint for usage and gives a name, which is the only required
        information for the savepoint to be usable. Meta-information can be added after the
        initialization has been performed.

        :param name: str -- Name of the savepoint
        :param metainfo: dict -- {Key:value} pair dictionary used for initializing the
                                 meta-information of the Savepont
        :param impl: Directly set the implementation pointer (internal use)
        :raises: SerialboxError -- Savepoint could not be initialized
        """
        if impl:
            self.__savepoint = impl
        else:
            namestr = extract_string(name)[0]
            self.__savepoint = invoke(lib.serialboxSavepointCreate, namestr)

        if metainfo:
            if isinstance(metainfo, MetaInfoMap):
                metainfo = metainfo.to_dict()

            metainfomap = self.metainfo
            for key, value in metainfo.items():
                metainfomap.insert(key, value)

    @property
    def name(self):
        """Name of the Savepoint.

        :return: Name of the savepoint
        :rtype: str
        """
        return invoke(lib.serialboxSavepointGetName, self.__savepoint).decode()

    @property
    def metainfo(self):
        """Meta-information of the Savepoint.

        :return: Refrence to the meta-information map
        :rtype: MetaInfoMap
        """
        return MetaInfoMap(impl=invoke(lib.serialboxSavepointGetMetaInfo, self.__savepoint))

    def clone(self):
        """Clone the Savepoint by performing a deepcopy.

        :return: Clone of the savepoint
        :rtype: Savepoint
        """
        return Savepoint('',
                         impl=invoke(lib.serialboxSavepointCreateFromSavepoint, self.__savepoint))

    def __eq__(self, other):
        """Test for equality.

        Savepoints compare equal if their names and meta-infor compare equal.

        :return: True if self == other, False otherwise
        :rtype: bool
        """
        return bool(invoke(lib.serialboxSavepointEqual, self.__savepoint, other.__savepoint))

    def __ne__(self, other):
        """Test for inequality.

        :return: True if self != other, False otherwise
        :rtype: bool
        """
        return not self.__eq__(other)

    def impl(self):
        """Get implementation pointer.
        """
        return self.__savepoint

    def __del__(self):
        invoke(lib.serialboxSavepointDestroy, self.__savepoint)

    def __repr__(self):
        return '<Savepoint {0}>'.format(self.__str__())

    def __str__(self):
        return invoke(lib.serialboxSavepointToString, self.__savepoint).decode()


# ===--------------------------------------------------------------------------------------------===
#   SavepointCollection
# ==---------------------------------------------------------------------------------------------===

class SavepointCollection(object, metaclass=ABCMeta):
    def savepoints(self):
        """ Get the list of savepoints in this collection.

        :return: List of savepoints in the collection.
        :rtype: list[Savepoint]
        """
        raise NotImplementedError()

    def as_savepoint(self):
        """ Return the unqiue savepoint in the list or raise an Error if list has more than 1
        element.

        :return: Savepoint in this collection.
        :rtype: Savepoint
        :raises SerialboxError: List has more than one savepoint
        """
        num_savepoints = len(self.savepoints())
        if num_savepoints == 1:
            return self.savepoints()[0]

        if num_savepoints > 1:
            errstr = "Savepoint is ambiguous. Candiates are:\n"
            for sp in self.savepoints():
                errstr += "  {0}\n".format(str(sp))
            raise SerialboxError(errstr)
        else:
            raise SerialboxError("SavepointCollection is empty")


def transformed_equal(name, key):
    """ Return True if `name` can be mapped to the transformed `key` such that `key` is a valid
    python identifier.

    The following transformation of `key` will be considered:
        ' '     ==>  '_'
        '-'     ==>  '_'
        '.'     ==>  '_'
        '[0-9]' ==> _[0-9]
    """
    key_transformed = key.replace(' ', '_').replace('-', '_').replace('.', '_')
    if key_transformed[0].isdigit():
        key_transformed = '_' + key_transformed
    return key_transformed == name


class SavepointTopCollection(SavepointCollection):
    """ Collection of all savepoints.
    """

    def __init__(self, savepoint_list):
        self.__savepoint_list = savepoint_list

    def savepoints(self):
        return self.__savepoint_list

    def __make_savepoint_collection(self, name, match_exact=False):
        savepoint_list = []
        for sp in self.__savepoint_list:
            sp_name = sp.name

            if name == sp_name:
                savepoint_list += [sp]
            elif not match_exact and transformed_equal(name, sp_name):
                savepoint_list += [sp]

        if not savepoint_list:
            errstr = "savepoint with name '%s' does not exist" % name

            # Make a suggestion if possible
            dist = []
            for sp in self.__savepoint_list:
                dist += [levenshtein(name, sp.name)]

            if min(dist) <= 3:
                errstr += ", did you mean '%s'?" % self.__savepoint_list[dist.index(min(dist))].name

            raise SerialboxError(errstr)

        return SavepointNamedCollection(savepoint_list, None)

    def __getattr__(self, name):
        """ Access a collection of savepoints identified by `name`

        :param name: Name of the savepoint
        :type name: str
        :return: Collection of savepoints sharing the same `name`
        :rtype: SavepointNamedCollection
        """
        return self.__make_savepoint_collection(name, False)

    def __getitem__(self, index):
        """ Access a collection of savepoints identified by `index`

        If `index` is an integer (`isinstance(index, int`), the method returns the unique Savepoint
        at poisition `index` in the savepoint list. Otherwise,

        :param index: Name or index of the savepoint
        :type index: str, int
        :return: Collection of savepoints sharing the same `name` or unique savepoint.
        :rtype: SavepointNamedCollection, Savepoint
        """
        if isinstance(index, int):
            return self.__savepoint_list[index]
        return self.__make_savepoint_collection(index, True)

    def __str__(self):
        s = "SavepointTopCollection:\n"
        for sp in self.__savepoint_list:
            s += "  {0}\n".format(str(sp))
        return s


class SavepointNamedCollection(SavepointCollection):
    """ Collection of Savepoints which all share the same `name`.
    """

    def __init__(self, savepoint_list, prev_key):
        self.__savepoint_list = savepoint_list
        self.__prev_key = prev_key

    def savepoints(self):
        return self.__savepoint_list

    def __make_named_savepoint_collection(self, key, match_exact=False):

        savepoint_list = []
        for sp in self.__savepoint_list:
            # Exact match
            if sp.metainfo.has_key(key):
                savepoint_list += [sp]

        if not savepoint_list:
            # Try a little harder ... we iterate now over all keys of the savepoints in the
            # collection.
            keys = []
            if not match_exact:
                for sp in self.__savepoint_list:
                    sp_keys = sp.metainfo.to_dict()
                    for k in sp_keys:
                        if transformed_equal(key, k):
                            keys += [k]
                            savepoint_list += [sp]

            # At his point we have to give up.. but not before we make a suggestion ;)
            if not savepoint_list:
                errstr = "no savepoint named '%s' has meta-info with key '%s'" % (
                    self.__savepoint_list[0].name, key)
                raise SerialboxError(errstr)

            # If we used match_exact=False and matched for example for key 'key_1': 'key 1' and
            # 'key-1', we just abort as there is no point to handle this case ...
            if keys.count(keys[0]) != len(keys):
                errstr = "ambiguous match for key '%s' for savepoint with name '%s'" % (
                    key, self.__savepoint_list[0].name)
                errstr += "Found matches:\n"
                for k in keys:
                    errstr += "  %s\n" % k
                raise SerialboxError(errstr)
            key = keys[0]

        return SavepointNamedCollection(savepoint_list, key)

    def __getattr__(self, key):
        return self.__make_named_savepoint_collection(key, False)

    def __getitem__(self, index):
        #
        # If `self.__prev_key` is not None, we have a query of the form
        # `serializer.savepoint.key[1]` meaning we access the meta-info key=value pair with
        # key=self.__prev_key and value=index. Otherwise, we have a query of the form
        # `serializer.savepoint['key1']`.
        #
        if self.__prev_key:
            savepoint_list = []

            # Check if key=value pair exists
            for sp in self.__savepoint_list:
                if sp.metainfo[self.__prev_key] == index:
                    savepoint_list += [sp]

            # Nothing found.. list the available savepoints and raise
            if not savepoint_list:
                errstr = "no savepoint named '%s' has meta-info: {\"%s\": %s}. Candiates are:\n" % (
                    self.__savepoint_list[0].name, self.__prev_key, index)
                for sp in self.savepoints():
                    errstr += "  {0}\n".format(str(sp))
                raise SerialboxError(errstr)

            return SavepointNamedCollection(savepoint_list, None)
        else:
            if not type(index) in StringTypes:
                raise SerialboxError("expected string in query for meta-info of Savepoint '%s'" %
                                     self.__savepoint_list[0].name)

            return self.__make_named_savepoint_collection(index, True)

    def __str__(self):
        s = "SavepointNamedCollection:\n"
        for sp in self.__savepoint_list:
            s += "  {0}\n".format(str(sp))
        return s


register_library(lib)
