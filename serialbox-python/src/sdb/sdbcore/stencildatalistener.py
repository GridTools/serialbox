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


class StencilDataDataListener(object):
    """Listen to any changes of the data of the StencilData object.
    """

    def data_changed(self):
        """Data has changed.
        """
        raise NotImplementedError


class StencilDataStencilListListener(object):
    """Listen to changes in the stencil list of the StencilData object.
    """

    def remove_all_stencils(self):
        """All stencils have been removed.
        """
        raise NotImplementedError

    def add_stencil(self, stencil):
        """Add a stencil.

        :param stencil: Stencil to add
        :type stencil: str
        """
        raise NotImplementedError

    def remove_stencil(self, stencil):
        """Remove a stencil.

        :param stencil: Stencil to remove (expexted to exist)
        :type stencil: str
        """
        raise NotImplementedError


class StencilDataFieldListListener(object):
    """Listen to changes in the field list of the StencilData object.
    """

    def remove_all_fields(self):
        """Remove all registered fields.
        """
        raise NotImplementedError

    def add_field(self, name, idx=None):
        """Append or insert field `name` at the end or `idx`.

        :param name: Name of the field
        :type name: str
        :param idx: Position to insert the field. If idx is ``None``, the field will be appended at
                    the end
        :type idx: int
        """
        raise NotImplementedError

    def remove_field(self, name):
        """Remove the field `name`.

        :param name: Name of the field
        :type name: str
        """
        raise NotImplementedError

    def set_field_enabled(self, name_or_idx, enable):
        """Enable or disable field.

        :param name_or_idx: Name or index of the field
        :type name_or_idx: str, int
        :param enable: Enable (``True``) or disable (``False``) field
        :type enable: bool
        """
        raise NotImplementedError

    def move_field(self, name_or_idx, idx):
        """Move field, given by `name` or `idx`, to position `idx` in the list field list.

        :param name_or_idx: Name or index of the field
        :type name_or_idx: str, int
        :param idx: Index of the `field` to be moved to
        :type idx: int
        """
        raise NotImplementedError

    def num_fields(self):
        """Get number of fields in the list.

        :return: Number of fields in the list
        :rtype: int
        """
        raise NotImplementedError
