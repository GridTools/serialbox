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

class SerializerDataListener(object):
    """Listen to any changes of the data of the SerializerData object.
    """

    def reload(self):
        """Serializer data changed i.e the underlying serializer have been reloaded.
        """
        raise NotImplementedError

class SerializerDataDirectoryAndPrefixListener(object):
    """Listen to any changes of the directory and prefix of the SerializerData object.
    """

    def prefix_changed(self, prefix):
        """Prefix of the SerializerData changed to `prefix`
        """
        raise NotImplementedError

    def directory_changed(self, directory):
        """Directory of the SerializerData changed to `directory`
        """
        raise NotImplementedError
