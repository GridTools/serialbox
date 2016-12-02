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

from collections import defaultdict
from json import dump, load, JSONDecodeError
from os import getcwd, path, environ, makedirs, error

from sdbcore.logger import Logger
from sdbcore.serializerdata import SerializerData
from sdbcore.version import Version


class SessionManager(object):
    """Load/store information about the current session
    """

    DefaultConfigFile = "default-config.json"

    def __init__(self):
        self.__config = dict()
        self.set_default()

    def set_default(self):
        config = defaultdict(dict)

        config["Version"] = Version().sdb_version()

        # Recently used serializers
        config["RecentlyUsedSerializerPaths"] = []

        # SerializerData
        config["SerializerData"]["Input Serializer"] = {"directory": "", "prefix": ""}
        config["SerializerData"]["Reference Serializer"] = {"directory": "", "prefix": ""}

        # StencilData
        config["StencilData"] = dict()

        self.__config = dict(config)

    # ===----------------------------------------------------------------------------------------===
    #   SerializerData
    # ==-----------------------------------------------------------------------------------------===

    def set_serializer_data(self, serializer_data):
        name = serializer_data.name
        Logger.info("Loading SerializerData of '%s' from Configuration" % name)

        if not isinstance(serializer_data, SerializerData):
            Logger.error(
                "Type of SerializerData '%s' is not valid: %s" % (name, type(serializer_data)))

        try:
            serializer_data.directory = self.__config["SerializerData"][name]["directory"]
            serializer_data.prefix = self.__config["SerializerData"][name]["prefix"]
        except IndexError as e:
            Logger.warning("Error loading SerializerData of '%s': %s" % (name, e))

    def update_serializer_data(self, serializer_data):
        name = serializer_data.name
        Logger.info("Syncing SerializerData of '%s' with Configuration" % name)
        try:
            self.__config["SerializerData"][name]["directory"] = serializer_data.directory
            self.__config["SerializerData"][name]["prefix"] = serializer_data.prefix
        except IndexError as e:
            Logger.warning("Error storing SerializerData of '%s': %s" % (name, e))

    # ===----------------------------------------------------------------------------------------===
    #   Recently used files
    # ==-----------------------------------------------------------------------------------------===

    def get_recently_used_serializer_paths(self):
        """Get a list of recently used serializer paths.

        :return: List of recently used serializer paths
        :rtype: :class:`list` [:class:`str`]
        """
        Logger.info("Query recently used Serializer paths")

        if "RecentlyUsedSerializerPaths" in self.__config:
            return self.__config["RecentlyUsedSerializerPaths"]
        else:
            return []

    def update_recently_used_serializer_paths(self, path):
        """Add a path to the recently used serializer paths

        :param path: Path to the serializer i.e directory
        :type path: str
        """
        if "RecentlyUsedSerializerPaths" in self.__config:
            if not path in self.__config["RecentlyUsedSerializerPaths"]:
                Logger.info("Adding '%s' to recently used Serializer paths" % path)
                self.__config["RecentlyUsedSerializerPaths"] += [path]
        else:
            self.__config["RecentlyUsedSerializerPaths"] = [path]

    # ===----------------------------------------------------------------------------------------===
    #   Load/Store config file
    # ==-----------------------------------------------------------------------------------------===

    def excract_config_files(self, filename):
        """If filename is None, the default configuration paths: (pwd)/.sdb/ or $HOME/.sdb/ will be
        used

        :returns: List of config filenames
        """
        if filename:
            config_files = [filename]
        else:
            config_files = [path.join(getcwd(), ".sdb", SessionManager.DefaultConfigFile)]

            home_dir = environ.get("HOME")
            if home_dir:
                config_files += [path.join(home_dir, ".sdb", SessionManager.DefaultConfigFile)]

        return config_files

    def __store_to_file_impl(self, filename):
        try:
            try:
                dir = path.dirname(filename)
                makedirs(dir)
            except error:
                pass

            with open(filename, 'w') as file:
                Logger.info("Saving config file in \"%s\"" % filename)
                dump(self.__config, file, indent=2)

        except (OSError, IOError) as e:
            Logger.warning("Unable to save config file in \"%s\": %s" % (filename, e))
            return False, str(e)
        return True, ""

    def store_to_file(self, filename=None):
        """Store configuration file to disk.

        :param filename: Path of the configuration file. If `filename` is ``None``, the default
                         locations will be searched for ``default-config.json``.
        :type filename: str
        """
        config_files = self.excract_config_files(filename)
        error_messages = []

        for config_file in config_files:
            ret, msg = self.__store_to_file_impl(config_file)
            if ret:
                return True, error_messages
            else:
                error_messages += [msg]

        Logger.warning("Failed to save config file")
        return False, error_messages

    def __load_from_file_impl(self, filename):
        try:
            with open(filename, 'r') as file:
                self.__config = load(file)
                Logger.info("Loading config file from \"%s\"" % filename)
        except (OSError, IOError, error) as e:
            Logger.warning("Unable to load config file from \"%s\": %s" % (filename, e))
            return False, str(e)
        except JSONDecodeError as e:
            return False, "JSON decoding error: " + str(e)
        return True, ""

    def load_from_file(self, filename=None):
        """Load configuration file from disk.

        :param filename: Path of the configuration file. If `filename` is ``None``, the default
                         locations will be searched for ``default-config.json``.
        :type filename: str
        """
        config_files = self.excract_config_files(filename)
        error_messages = []

        for config_file in config_files:
            ret, msg = self.__load_from_file_impl(config_file)
            if ret:
                return True, error_messages
            else:
                error_messages += [msg]

        Logger.warning("Failed to load config file")
        return False, error_messages
