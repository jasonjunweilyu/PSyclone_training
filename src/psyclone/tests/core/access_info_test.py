# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: Joerg Henrichs, Bureau of Meteorology
# Modifications: A. R. Porter, STFC Daresbury Laboratory

'''This module tests the various classes in core.access_info.'''

from __future__ import absolute_import
import pytest

from psyclone.core import AccessInfo, Signature, SingleVariableAccessInfo, \
    VariablesAccessInfo
from psyclone.core.access_type import AccessType
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Node
from psyclone.tests.utilities import create_schedule


def test_access_info():
    '''Test the AccessInfo class.
    '''
    location = 12
    access_info = AccessInfo(AccessType.READ, location, Node())
    assert access_info.access_type == AccessType.READ
    assert access_info.location == location
    assert access_info.indices is None
    assert str(access_info) == "READ(12)"
    access_info.change_read_to_write()
    assert str(access_info) == "WRITE(12)"
    assert access_info.access_type == AccessType.WRITE
    with pytest.raises(InternalError) as err:
        access_info.change_read_to_write()
    assert "Trying to change variable to 'WRITE' which does not have "\
        "'READ' access." in str(err.value)

    access_info.indices = ["i"]
    assert access_info.indices == ["i"]

    access_info = AccessInfo(AccessType.UNKNOWN, location, Node())
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.location == location
    assert access_info.indices is None

    access_info = AccessInfo(AccessType.UNKNOWN, location, Node(), ["i", "j"])
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.location == location
    assert access_info.indices == ["i", "j"]


# -----------------------------------------------------------------------------
def test_variable_access_info():
    '''Test the VariableAccesInfo class, i.e. the class that manages a list
    of VariableInfo instances for one variable
    '''

    vai = SingleVariableAccessInfo("var_name")
    assert vai.var_name == "var_name"
    assert str(vai) == "var_name:"
    assert vai.is_written() is False
    assert vai.is_read() is False
    assert vai.all_accesses == []

    vai.add_access_with_location(AccessType.READ, 2, Node())
    assert str(vai) == "var_name:READ(2)"
    assert vai.is_read()
    assert vai.is_read_only()
    vai.change_read_to_write()
    assert not vai.is_read()
    assert vai.is_written()
    assert not vai.is_read_only()

    # Now we have one write access, which we should not be able to
    # change to write again:
    with pytest.raises(InternalError) as err:
        vai.change_read_to_write()
    assert "Trying to change variable 'var_name' to 'WRITE' which "\
        "does not have 'READ' access." in str(err.value)

    assert vai.all_accesses[0] == vai[0]
    with pytest.raises(IndexError) as err:
        _ = vai[1]

    # Add a READ access - now we should not be able to
    # change read to write anymore:
    vai.add_access_with_location(AccessType.READ, 1, Node())
    with pytest.raises(InternalError) as err:
        vai.change_read_to_write()
    assert "Variable 'var_name' had 2 accesses listed, "\
           "not one in change_read_to_write." in str(err.value)

    # And make sure the variable is not read_only if a write is added
    vai.add_access_with_location(AccessType.WRITE, 3, Node())
    assert vai.is_read_only() is False


# -----------------------------------------------------------------------------
def test_variable_access_info_read_write():
    '''Test the handling of READWRITE accesses. A READWRITE indicates both
    a read and a write access, but if a variable as a READ and a WRITE
    access, this is not one READWRITE access. A READWRITE access is only
    used in subroutine calls (depending on kernel metadata)
    '''

    vai = SingleVariableAccessInfo("var_name")
    assert vai.has_read_write() is False

    # Add a READ and WRITE access at the same location, and make sure it
    # is not reported as READWRITE access
    node = Node()
    vai.add_access_with_location(AccessType.READ, 2, node)
    assert vai[0].node == node
    assert vai[0].location == 2
    vai.add_access_with_location(AccessType.WRITE, 2, Node())
    assert vai.has_read_write() is False

    vai.add_access_with_location(AccessType.READWRITE, 2, Node())
    assert vai.has_read_write()

    # Create a new instance, and add only one READWRITE access:
    vai = SingleVariableAccessInfo("var_name")
    vai.add_access_with_location(AccessType.READWRITE, 2, Node())
    assert vai.has_read_write()
    assert vai.is_read()
    assert vai.is_written()


# -----------------------------------------------------------------------------
def test_variables_access_info():
    '''Test the implementation of VariablesAccessInfo, a class that manages
    a list of variables, each with a list of accesses.
    '''
    var_accesses = VariablesAccessInfo()
    node1 = Node()
    var_accesses.add_access(Signature("read"), AccessType.READ, node1)
    node2 = Node()
    var_accesses.add_access(Signature("written"), AccessType.WRITE, node2)
    assert str(var_accesses) == "read: READ, written: WRITE"

    var_accesses.next_location()
    node = Node()
    var_accesses.add_access(Signature("written"), AccessType.WRITE, node)
    var_accesses.next_location()
    var_accesses.add_access(Signature("read_written"), AccessType.WRITE, node)
    var_accesses.add_access(Signature("read_written"), AccessType.READ, node)
    assert str(var_accesses) == "read: READ, read_written: READ+WRITE, "\
                                "written: WRITE"
    assert set(var_accesses.all_signatures) == set([Signature("read"),
                                                    Signature("written"),
                                                    Signature("read_written")])
    all_accesses = var_accesses[Signature("read")].all_accesses
    assert all_accesses[0].node == node1
    written_accesses = var_accesses[Signature("written")].all_accesses
    assert written_accesses[0].location == 0
    assert written_accesses[1].location == 1
    # Check that the location pointer is pointing to the next statement:
    assert var_accesses.location == 2

    # Create a new instance
    var_accesses2 = VariablesAccessInfo()
    var_accesses2.add_access(Signature("new_var"), AccessType.READ, node)
    var_accesses2.add_access(Signature("written"), AccessType.READ, node)

    # Now merge the new instance with the previous instance:
    var_accesses.merge(var_accesses2)
    assert str(var_accesses) == "new_var: READ, read: READ, " \
                                "read_written: READ+WRITE, written: READ+WRITE"

    with pytest.raises(KeyError):
        _ = var_accesses[Signature("does_not_exist")]
    with pytest.raises(KeyError):
        var_accesses.is_read(Signature("does_not_exist"))
    with pytest.raises(KeyError):
        var_accesses.is_written(Signature("does_not_exist"))

    assert "READWRITE" not in str(var_accesses)
    var_accesses.add_access(Signature("readwrite"), AccessType.READWRITE, node)
    assert "READWRITE" in str(var_accesses)

    with pytest.raises(InternalError) as err:
        var_accesses.add_access("no-signature", AccessType.READWRITE, node)

    assert "Got 'no-signature' of type 'str' but expected it to be of type " \
           "psyclone.core.Signature." in str(err.value)


# -----------------------------------------------------------------------------
def test_variables_access_info_merge():
    '''Tests the merge operation of VariablesAccessInfo.
    '''
    # First create one instance representing for example:
    # a=b; c=d
    var_accesses1 = VariablesAccessInfo()
    node = Node()
    var_accesses1.add_access(Signature("b"), AccessType.READ, node)
    var_accesses1.add_access(Signature("a"), AccessType.WRITE, node)
    var_accesses1.next_location()
    var_accesses1.add_access(Signature("d"), AccessType.READ, node)
    var_accesses1.add_access(Signature("c"), AccessType.WRITE, node)
    c_accesses = var_accesses1[Signature("c")]
    assert len(c_accesses.all_accesses) == 1
    assert c_accesses[0].access_type == AccessType.WRITE

    # First create one instance representing for example:
    # e=f; g=h
    var_accesses2 = VariablesAccessInfo()
    var_accesses2.add_access(Signature("f"), AccessType.READ, node)
    var_accesses2.add_access(Signature("e"), AccessType.WRITE, node)
    var_accesses2.next_location()
    var_accesses2.add_access(Signature("h"), AccessType.READ, node)
    var_accesses2.add_access(Signature("g"), AccessType.WRITE, node)

    # Now merge the second instance into the first one
    var_accesses1.merge(var_accesses2)

    # The e=f access pattern should have the same location
    # as the c=d (since there is no next_location after
    # adding the b=a access):
    c_accesses = var_accesses1[Signature("c")]
    e_accesses = var_accesses1[Signature("e")]
    assert c_accesses[0].access_type == AccessType.WRITE
    assert e_accesses[0].access_type == AccessType.WRITE
    assert c_accesses[0].location == e_accesses[0].location

    # Test that the g=h part has a higher location than the
    # c=d data. This makes sure that merge() increases the
    # location number of accesses when merging.
    c_accesses = var_accesses1[Signature("c")]
    g_accesses = var_accesses1[Signature("g")]
    h_accesses = var_accesses1[Signature("h")]
    assert c_accesses[0].location < g_accesses[0].location
    assert g_accesses[0].location == h_accesses[0].location

    # Also make sure that the access location was properly increased
    # Originally we had locations 0,1. Then we merged accesses with
    # location 0,1 in - the one at 0 is merged with the current 1,
    # and the new location 1 increases the current location from
    # 1 to 2:
    assert var_accesses1.location == 2


# -----------------------------------------------------------------------------
def test_constructor():
    '''Test the optional constructor parameter (single node and list
    of nodes).'''

    code = '''module test
        contains
        subroutine tmp()
          integer :: a,b,c
          a = b/c
          c = a*b
        end subroutine tmp
        end module test'''
    schedule = create_schedule(code, "tmp")
    node1 = schedule[0]
    node2 = schedule[1]
    vai1 = VariablesAccessInfo(node1)
    assert str(vai1) == "a: WRITE, b: READ, c: READ"
    vai2 = VariablesAccessInfo([node1, node2])
    assert str(vai2) == "a: READ+WRITE, b: READ, c: READ+WRITE"

    with pytest.raises(InternalError) as err:
        VariablesAccessInfo([node1, node2, 3])
    assert "One element in the node list is not a Node, but of type " in \
        str(err.value)
    # The error message is slightly different between python 2 and 3
    # so only test for the part that is the same in both:
    assert "'int'>" in str(err.value)

    with pytest.raises(InternalError) as err:
        VariablesAccessInfo(1)
    assert "Error in VariablesAccessInfo" in str(err.value)
    # The error message is slightly different between python 2 and 3
    # so only test for the part that is the same in both:
    assert "'int'>" in str(err.value)
