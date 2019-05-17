#!/usr/bin/env python3
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
## Compares Serialbox fields in FILE_1 and FILE_2.
## FILE_1 and FILE_2 can be .dat or .json files, but need to be of the same type. When they are
## .json files, all fields are compared. When they are .dat files, the field name has to be the
## last part of the file name, separated by '_'.
##
##===------------------------------------------------------------------------------------------===##

from __future__ import print_function

from sys import exit, stderr, version_info

# Check Python version
if version_info < (3, 4):
    from platform import python_version

    print("compare: error: compare requires at least python 3.4 (detected %s)" % python_version(),
          file=stderr)
    exit(1)

from argparse import ArgumentParser
from os import path
from sys import path as sys_path
from time import time
from enum import Enum
import numpy as np
from math import isnan
import re

# Find the Serialbox python module
sys_path.insert(1, path.join(path.dirname(path.realpath(__file__)), "../"))

import serialbox as ser


class Config(object):
    """ Global configuration """

    def __init__(self):
        # Use colored output?
        self.USE_COLOR = True

        # Maximum number of errors to report per field
        self.MAX_ERRORS = 10

        # Tolerance used for field comparison
        self.TOL = 1e-12

        # Tolerance used per field for field comparison
        self.TOLS = dict()

        # Tolerance used for field comparison
        self.SAVEPOINT_REGEX = ""

        # Treat mismatches in the metainfo of fields as warnings (mismatches "in dimensions are
        # still errors)
        self.FIELD_INFO_MISMATCH_WARN = False

        # Only compare field meta-info (no data comparison)
        self.FIELD_INFO_ONLY = False


    def tol(self, field = None):
        """ Get tolerance """

        return self.TOLS.get(field, self.TOL)


g_config = Config()


def read_tolerances(filename):
    """ Read tolerance dictionary from JSON """
    import json

    with open(filename, 'r') as f:
        tols = json.load(f)

    return tols


def get_config():
    """ Access the global configuration """
    global g_config
    return g_config


def fatal_error(msg):
    """ Report an error message and exit with 1 """
    print("compare: error: {}".format(msg), file=stderr)
    exit(1)


class Alignment(Enum):
    """ Alignment options used in report """
    LEFT = '<'
    CENTER = '^'
    RIGHT = '>'


class Color(Enum):
    """ Colors used in report

    https://stackoverflow.com/questions/15580303/python-output-complex-line-with-floats-colored-by-value
    """
    GREEN = '\033[92m'
    RED = '\033[91m'
    BOLD_WHITE = '\033[1;93m'
    RESET = '\033[0m'


def report(prefix, msg, alignment=Alignment.CENTER, color=Color.GREEN):
    """ Report a msg to stdout """
    fmt_str = "[{:" + alignment.value + "10}]"
    if get_config().USE_COLOR:
        fmt_str = color.value + fmt_str + color.RESET.value
    fmt_str += " {}"
    print(fmt_str.format(prefix, msg))


def make_serializer_and_extract_field(file):
    """ Create a Serializer of the archive pointed to by file and extract the field name

        If file is set to '/home/foo/bar_SomeField.dat' we will open a Serializer in
        '/home/foo' with the prefix 'bar' and return 'SomeField' to be used for comparison. If file
        is '/home/foo/MetaData-bar.json' we will return None for field and compare every field in
        the archive.
    """
    file = path.abspath(file)
    directory = path.dirname(file)
    basename = path.basename(file)
    filename, extension = path.splitext(basename)

    if extension == ".json":
        # New Serialbox archive 'MetaData-prefix.json'
        if "MetaData-" in filename:
            prefix = filename[len("MetaData-"):]
        # Old Serialbox archive 'prefix.json'
        else:
            prefix = filename
        field = None
    else:
        first_underscore = filename.find("_")
        if first_underscore == -1:
            fatal_error(
                "cannot extract archive prefix from field file '{}': missing '_'".format(file))

        prefix = filename[:first_underscore]
        field = filename[first_underscore + 1:]

    serializer = None

    try:
        serializer = ser.Serializer(ser.OpenModeKind.Read, directory, prefix)
    except ser.SerialboxError as e:
        fatal_error(e)

    return serializer, field


def compare_infos(serializers, field):
    """ Compare the field-meta infos of field_1 to field_2 and return True on equality """
    serializer_1, serializer_2 = serializers
    info_1, info_2 = serializer_1.get_field_metainfo(field), serializer_2.get_field_metainfo(field)

    if info_1 == info_2:
        return True

    hard_error = False

    # Compare type and dimensions
    for attrname in ["type", "dims"]:
        attr_1, attr_2 = getattr(info_1, attrname), getattr(info_2, attrname)
        if attr_1 != attr_2:
            print(
                "  Field meta-info mismatch: {}\n    Expected: {}\n    Actual:   {}".format(
                    attrname,
                    attr_1,
                    attr_2))

            hard_error = True

    if hard_error:
        return False

    # Compare metainfo
    metainfo_1 = info_1.metainfo.to_dict()
    metainfo_2 = info_2.metainfo.to_dict()
    diff = dict(set(metainfo_2.items()) - set(metainfo_1.items()))

    if diff:
        hard_error = False if get_config().FIELD_INFO_MISMATCH_WARN else True
        list_1, list_2 = "", ""

        for key, value in sorted(metainfo_1.items()):
            list_1 += "{}{}: {}\n".format(6 * " ", key, value)

        for key, value in sorted(metainfo_2.items()):
            use_color = False
            if get_config().USE_COLOR:
                use_color = key in diff
            list_2 += "{}{}{}: {}{}\n".format(Color.BOLD_WHITE.value if use_color else "",
                                              6 * " ", key,
                                              value, Color.RESET.value if use_color else "")

        print(
            "  Field meta-info mismatch: meta-info\n    Expected:\n{}\n    Actual:\n{}".format(
                list_1, list_2))

    return not hard_error


def compare_fields(serializers, field, savepoint, dim_bounds):
    """ If field is not None compare the field at each savepoint if field is None compare every
        field at every savepoint (full archive comparison).
    """
    serializer_1, serializer_2 = serializers
    field_1 = serializer_1.read(field, savepoint)
    field_2 = serializer_2.read(field, savepoint)

    dims = serializer_1.get_field_metainfo(field).dims
    if len(dims) > len(dim_bounds):
        print("  Field dimension '{}' exceeds maximum of 4 dimension")
        return False

    slices = []
    for i in range(0, len(dims)):
        slices += [dim_bounds[i]]

    # Get a view of a field incorporating the user defined slices
    field_view_1 = field_1[slices]
    field_view_2 = field_2[slices]
    assert field_view_1.size == field_view_2.size

    errors = []
    num_nans = 0
    max_abs_error = 0
    max_rel_error = 0
    tol = get_config().tol(field)

    it_1 = np.nditer(field_view_1, flags=['multi_index'])
    it_2 = np.nditer(field_view_2, flags=['multi_index'])

    # Iterate the fields
    while not it_1.finished and not it_2.finished:
        value_1, value_2 = it_1[0], it_2[0]
        value_1_isnan, value_2_isnan = isnan(value_1), isnan(value_2)

        # Check for NaN values
        num_nans += value_1_isnan + value_2_isnan
        if value_1_isnan != value_2_isnan:
            errors += [
                {"index": it_1.multi_index, "value_1": value_1, "value_2": value_2,
                 "error": float('nan')}]
        # Compute error
        else:
            if(value_1.dtype == 'bool'):
                if(value_1 != value_2):
                    errors += [
                        {"index": it_1.multi_index, "value_1": value_1, "value_2": value_2,
                         "error": 1.0}]
            else:
                abs_error = abs(value_2 - value_1)
                rel_error = abs((value_2 - value_1) / value_2) if abs(value_2) > 1.0 else 0
                err = rel_error if abs(value_2) > 1.0 else abs_error

                # Check error
                if err > tol:
                    errors += [
                        {"index": it_1.multi_index, "value_1": value_1, "value_2": value_2,
                         "error": err}]
                    max_abs_error = max(max_abs_error, abs_error)
                    max_rel_error = max(max_rel_error, rel_error)

        it_1.iternext()
        it_2.iternext()

    # All good!
    if len(errors) == 0:
        return True

    # Report the errors
    num_errors = len(errors)
    num_errors_displayed = min(get_config().MAX_ERRORS, num_errors)
    if num_errors_displayed > 0:
        print("  Failed values (displayed {} of {}):".format(num_errors_displayed, num_errors))
        for idx in range(0, num_errors_displayed):
            print("    {}: value_1 = {:.10f}, value_2 = {:.10f}, error = {:.10e}".format(
                errors[idx]["index"], float(errors[idx]["value_1"]), float(errors[idx]["value_2"]),
                float(errors[idx]["error"])))

    print("  Number of errors: {}".format(num_errors))
    print("  Number of NaN: {}".format(num_nans))
    print("  Percentage of errors: {:.2f} %".format(100 * num_errors / field_view_1.size))
    print("  Maximum absolute error: {:.10e}".format(max_abs_error))
    print("  Maximum relative error: {:.10e}".format(max_rel_error))
    return False


def compare(serializers, field_to_check, dim_bounds):
    """ Compare the data and info at every savepoint of field_1 to field_2 and returns
        True on success
    """
    serializer_1, serializer_2 = serializers

    report(10 * "=", "Set-up serializers.")
    report("", "  serializer_1 = '{}' (prefix '{}')".format(serializer_1.directory,
                                                            serializer_1.prefix))
    report("", "  serializer_2 = '{}' (prefix '{}')".format(serializer_2.directory,
                                                            serializer_2.prefix))
    num_comparison = 0
    failures = []
    start_time = time()

    # Compute elapsed time in ms
    def get_time(start):
        return int(1000 * (time() - start))

    # Empty regex string will match every savepoint -> regex wil be set to None
    savepoint_regex = None if get_config().SAVEPOINT_REGEX == "" else re.compile(
        get_config().SAVEPOINT_REGEX)

    for savepoint in serializer_1.savepoint_list():
        # Savepoint not present in both serializers -> skip
        if not serializer_2.has_savepoint(savepoint):
            continue

        # Do we check this savepoint?
        if savepoint_regex is not None and not savepoint_regex.match(savepoint.name):
            continue

        # Find the intersection of the fields at this savepoint
        fields_at_savepoint_1 = serializer_1.fields_at_savepoint(savepoint)
        fields_at_savepoint_2 = serializer_2.fields_at_savepoint(savepoint)
        fields_at_savepoint = list(set(fields_at_savepoint_1).intersection(fields_at_savepoint_2))

        # If field_to_check is None, we always check all fields at the savepoint
        if field_to_check is None:
            fields_to_check = fields_at_savepoint
        else:
            fields_to_check = [field_to_check] if field_to_check in fields_at_savepoint else []

        savepoint_start_time = time()
        if fields_to_check:
            report(10 * "-", "Savepoint {}".format(savepoint))

        # Check field(s) at the savepoint
        for field in fields_to_check:
            field_start_time = time()

            num_comparison += 1
            report(" RUN", "{}".format(field), Alignment.LEFT)

            # Check the field info of the field
            if not compare_infos(serializers, field):
                failures += [{"savepoint": str(savepoint), "field": field}]
                report("FAILED", "{} ({} ms) ".format(field, get_time(field_start_time)),
                       color=Color.RED)
            else:
                # Compare the data of the fields
                if not get_config().FIELD_INFO_ONLY and not compare_fields(serializers, field,
                                                                           savepoint, dim_bounds):
                    failures += [{"savepoint": str(savepoint), "field": field}]
                    report("FAILED",
                           "{} ({} ms) ".format(field, get_time(field_start_time)),
                           color=Color.RED)

                report("OK ", "{} ({} ms) ".format(field, get_time(field_start_time)),
                       Alignment.RIGHT)

        if fields_to_check:
            report(10 * "-",
                   "Savepoint {} ({} ms total)".format(savepoint.name,
                                                       get_time(savepoint_start_time)))

    report(10 * "=",
           "{} comparisons ran. ({} ms total)".format(num_comparison, get_time(start_time)))

    print("")
    report(10 * "=", "Tear-down serializers.")

    num_failures = len(failures)
    num_success = num_comparison - num_failures

    if num_success > 0:
        report("PASSED", "{} comparisons.".format(num_success))
    if num_failures:
        report("FAILED", "{} comparisons.".format(num_failures), color=Color.RED)
        for failure in failures:
            report("FAILED", "{} at Savepoint {}".format(failure["field"], failure["savepoint"]),
                   color=Color.RED)

    return 1 if num_failures else 0


def main(arguments=None):
    parser = ArgumentParser(
        description=
        """
        Compares Serialbox fields in FILE_1 and FILE_2.
        FILE_1 and FILE_2 can be .dat or .json files, but need to be of the same type. When they are
        .json files, all fields are compared. When they are .dat files, the field name has to be the
        last part of the file name, separated by '_'.
        """
    )
    parser.add_argument('FILE_1', help="Path to a field file (.dat) or archive (.json)", nargs=1,
                        type=str)
    parser.add_argument('FILE_2', help="Path to a field file (.dat) or archive (.json)", nargs=1,
                        type=str)
    parser.add_argument("--version", action="version",
                        version="compare (Serialbox {})".format(ser.__version__),
                        help="show version information and exit")
    parser.add_argument("-v", "--verbose", dest="verbose", action="store_true",
                        help="enable verbose logging")
    parser.add_argument("--no-color", dest="no_color", action="store_true",
                        help="disabled colored output (default: {})".format(
                            not get_config().USE_COLOR))
    parser.add_argument("-m", "--max-errors", dest="max_errors", metavar="NUM",
                        default=get_config().MAX_ERRORS,
                        type=int,
                        help="report up to 'NUM' errors per field (default: {})".format(
                            get_config().MAX_ERRORS))
    parser.add_argument("-w", "--info-warn", dest="field_info_mismatch_warn", action='store_true',
                        help="treat mismatches in the metainfo of fields as warnings (mismatches "
                             "in dimensions and data type are still errors) (default: {})".format(
                            get_config().FIELD_INFO_MISMATCH_WARN))
    parser.add_argument("-s", "--savepoint-filter", dest="savepoint_regex", metavar="REGEX",
                        default=get_config().SAVEPOINT_REGEX, type=str,
                        help="only compare fields of savepoints whose name matches REGEX. An "
                             "empty string will match every savepoint (default: \"{}\")".format(
                            get_config().SAVEPOINT_REGEX))
    parser.add_argument("-t", "--tolerance", dest="tolerance", metavar="TOL",
                        default=get_config().TOL,
                        help="set the tolerance used for comparison to 'TOL' (default : {})".format(
                            get_config().TOL))
    parser.add_argument("-T", "--tolerance-json", dest="tolerance_file", metavar="TOLERANCE_FILE",
                        default=None,
                        help="set the JSON file for per field tolerance used for comparison")
    parser.add_argument("-q", "--info-only", dest="field_info_only", action="store_true",
                        help="only compare field meta-info (no data comparison) "
                             "(default: {})".format(get_config().FIELD_INFO_ONLY))
    for dim in ["i", "j", "k", "l"]:
        parser.add_argument("-{}".format(dim), dest="{}".format(dim), metavar="START[:END]",
                            help="only compare the {} dimension 'START' or if 'END' is supplied "
                                 "compare in the range ['START', 'END']".format(dim))
    args = parser.parse_args(arguments) if arguments else parser.parse_args()

    if args.verbose:
        ser.Logging.enable()

    get_config().USE_COLOR = not args.no_color
    get_config().FIELD_INFO_MISMATCH_WARN = args.field_info_mismatch_warn
    get_config().FIELD_INFO_ONLY = args.field_info_only
    get_config().MAX_ERRORS = args.max_errors
    get_config().SAVEPOINT_REGEX = args.savepoint_regex
    get_config().TOL = float(args.tolerance)
    if args.tolerance_file is not None:
        get_config().TOLS = read_tolerances(args.tolerance_file)

    path_1, path_2 = (args.FILE_1[0], args.FILE_2[0])

    # Check paths exists
    for file in [path_1, path_2]:
        if not path.exists(file):
            fatal_error("file '{}' does not exist".format(file))

    # Extract bounds and create the slices
    dim_bounds = []
    for dim in ["i", "j", "k", "l"]:
        if getattr(args, dim):
            bound = getattr(args, dim).split(":")
            dim_bounds += [slice(bound) if len(bound) == 1 else slice(bound[0], bound[1])]
        else:
            dim_bounds += [slice(None)]

    # Open archives and create read-only serializers
    serializer_1, field_1 = make_serializer_and_extract_field(path_1)
    serializer_2, field_2 = make_serializer_and_extract_field(path_2)
    if field_1 != field_2:
        fatal_error("field_1 '{}' is not equal to field_2 '{}'".format(field_1, field_2))

    # Perform comparison
    ret = 1
    try:
        ret = compare([serializer_1, serializer_2], field_1, dim_bounds)
    except ser.SerialboxError as e:
        fatal_error(e)
    return ret


if __name__ == '__main__':
    exit(main())
