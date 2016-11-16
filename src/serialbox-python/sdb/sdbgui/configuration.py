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

from collections import MutableMapping
from json import dump, load
from os import getcwd, path, environ, makedirs, error

from sdbcore.logger import Logger
from sdbcore.serializerdata import SerializerData
from sdbcore.version import Version


class Configuration(MutableMapping):
    ConfigFile = "config.json"

    def __init__(self):
        self.__config = dict()

        self.__config["Version"] = Version().sdb_version()
        self.__config["SerializerData"] = dict()
        self.__config["SerializerData"]["Input Serializer"] = {"directory": "", "prefix": ""}
        self.__config["SerializerData"]["Reference Serializer"] = {"directory": "", "prefix": ""}

    def __getitem__(self, key):
        return self.__config[self.__keytransform__(key)]

    def __setitem__(self, key, value):
        self.__config[self.__keytransform__(key)] = value

    def __delitem__(self, key):
        del self.__config[self.__keytransform__(key)]

    def __iter__(self):
        return iter(self.__config)

    def __len__(self):
        return len(self.__config)

    def __keytransform__(self, key):
        return key

    def make_serializer_data(self, name):
        serializer_data = SerializerData(name)
        try:
            serializer_data.directory = self.__config["SerializerData"][name]["directory"]
            serializer_data.prefix = self.__config["SerializerData"][name]["prefix"]
        except IndexError as e:
            pass

        return serializer_data

    def upade_serializer_data(self, serializer_data):
        name = serializer_data.name
        Logger.info("Updating serializer data of: %s" % name)
        try:
            self.__config["SerializerData"][name]["directory"] = serializer_data.directory
            self.__config["SerializerData"][name]["prefix"] = serializer_data.prefix
        except IndexError as e:
            pass

    def sotre_to_file(self):
        # Try to store config in $(pwd)/.sdb/
        stored_config = True
        config_file = path.join(getcwd(), ".sdb", Configuration.ConfigFile)
        try:
            try:
                dir = path.dirname(config_file)
                makedirs(dir)
            except error:
                pass

            with open(config_file, 'w') as file:
                dump(self.__config, file)
                Logger.info("Storing config file in: %s" % config_file)

        except (OSError, IOError) as e:
            Logger.warning("Unable to save config file in %s: %s" % (config_file, e))
            stored_config = False

        # Try to store in $HOME/.sdb/
        if not stored_config:
            home_dir = environ.get("HOME")
            if home_dir:
                config_file = path.join(home_dir, ".sdb", Configuration.ConfigFile)
                try:
                    try:
                        dir = path.dirname(config_file)
                        makedirs(dir)
                    except error:
                        pass

                    with open(config_file, 'w') as file:
                        dump(self.__config, file)
                        Logger.info("Storing config file in: %s" % config_file)

                except (OSError, IOError) as e:
                    Logger.warning("Unable to save config file in %s: %s" % (config_file, e))
                    stored_config = False

    def load_from_file(self):
        # Try to load config from $(pwd)/.sdb/
        loaded_config = False
        config_file = path.join(getcwd(), ".sdb", Configuration.ConfigFile)
        try:
            with open(config_file, 'r') as file:
                self.__config = load(file)
                Logger.info("Loading config file from: %s" % config_file)
                loaded_config = True
        except (OSError, IOError, error) as e:
            Logger.info("Unable to load config file from %s: %s" % (config_file, e))

        # Try to load config from $HOME/.sdb/
        if not loaded_config:
            home_dir = environ.get("HOME")
            if home_dir:
                config_file = path.join(home_dir, ".sdb", Configuration.ConfigFile)
                try:
                    with open(config_file, 'r') as file:
                        self.__config = load(file)
                        Logger.info("Loading config file from: %s" % config_file)

                except (OSError, IOError) as e:
                    Logger.info("Unable to load config file from %s: %s" % (config_file, e))
