# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2023, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the ArrayMember class. '''

import pytest
from psyclone.psyir import symbols, nodes
from psyclone.errors import GenerationError


def test_am_constructor():
    ''' Test that we can construct an ArrayMember. '''
    amr = nodes.ArrayMember("sub_mesh")
    assert len(amr.children) == 0
    assert amr.name == "sub_mesh"


def test_am_create():
    ''' Test the create method of ArrayMember. '''
    amem = nodes.ArrayMember.create("subdomains",
                                    [nodes.Literal("1", symbols.INTEGER_TYPE),
                                     nodes.Literal("2", symbols.INTEGER_TYPE)])
    assert isinstance(amem, nodes.ArrayMember)
    assert len(amem.children) == 2
    assert isinstance(amem.indices[1], nodes.Literal)
    assert amem.indices[1].parent is amem

    with pytest.raises(GenerationError) as err:
        nodes.ArrayMember.create("subdomains",
                                 nodes.Literal("1", symbols.INTEGER_TYPE))
    assert ("indices argument in create method of ArrayMember class should be "
            "a list but found 'Literal'" in str(err.value))


def test_am_validate_child():
    ''' Test the _validate_child method of ArrayMember. '''
    idx = nodes.Literal("3", symbols.INTEGER_TYPE)
    amr = nodes.ArrayMember("sub_mesh")
    with pytest.raises(GenerationError) as err:
        amr.addchild("wrong")
    assert "'str' can't be child 0 of 'ArrayMember'" in str(err.value)
    amr.addchild(idx)
    assert amr.children[0] is idx


def test_am_is_lower_upper_bound():
    ''' Test the is_lower/upper_bound methods of ArrayMember. '''
    one = nodes.Literal("1", symbols.INTEGER_TYPE)
    two = nodes.Literal("2", symbols.INTEGER_TYPE)
    amem1 = nodes.ArrayMember.create(
        "subdomains",
        [one.copy(), nodes.Literal("2", symbols.INTEGER_TYPE)])
    assert amem1.is_lower_bound(0) is False
    assert amem1.is_upper_bound(0) is False
    grid_type = symbols.DataTypeSymbol("grid_type", symbols.DeferredType())
    sym = symbols.DataSymbol("grid_var", grid_type)
    ref = nodes.StructureReference.create(sym, ["data"])
    # Start and stop for the range are binary operators but not the right ones
    start = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.UBOUND,
        ref.copy(), one.copy())
    stop = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.LBOUND,
        ref.copy(), one.copy())
    my_range = nodes.Range.create(start, stop)
    sref = nodes.StructureReference.create(sym, [("data", [my_range])])
    amem2 = sref.walk(nodes.ArrayMember)[0]
    assert amem2.is_lower_bound(0) is False
    assert amem2.is_upper_bound(0) is False
    # Correct binary operators but wrong types of operand
    start = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.LBOUND,
        one.copy(), one.copy())
    stop = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.UBOUND,
        one.copy(), one.copy())
    my_range = nodes.Range.create(start, stop)
    sref = nodes.StructureReference.create(sym, [("data", [my_range])])
    amem2 = sref.walk(nodes.ArrayMember)[0]
    assert amem2.is_lower_bound(0) is False
    assert amem2.is_upper_bound(0) is False
    # Correct start and stop arguments to Range
    start = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.LBOUND,
        ref.copy(), one.copy())
    stop = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.UBOUND,
        ref.copy(), one.copy())
    my_range = nodes.Range.create(start, stop)
    sref = nodes.StructureReference.create(sym, [("data", [my_range])])
    amem2 = sref.walk(nodes.ArrayMember)[0]
    assert amem2.is_lower_bound(0) is True
    assert amem2.is_upper_bound(0) is True
    # Range in a dimension other than the first
    start = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.LBOUND,
        ref.copy(), two.copy())
    stop = nodes.operation.BinaryOperation.create(
        nodes.operation.BinaryOperation.Operator.UBOUND,
        ref.copy(), two.copy())
    my_range = nodes.Range.create(start, stop)
    sref = nodes.StructureReference.create(sym, [("data",
                                                  [one.copy(), my_range])])
    amem2 = sref.walk(nodes.ArrayMember)[0]
    assert amem2.is_lower_bound(0) is False
    assert amem2.is_upper_bound(0) is False
    assert amem2.is_lower_bound(1) is True
    assert amem2.is_upper_bound(1) is True


def test_am_lbound(fortran_writer):
    ''' Tests for the lbound() method of ArrayMember. '''
    one = nodes.Literal("1", symbols.INTEGER_TYPE)
    two = nodes.Literal("2", symbols.INTEGER_TYPE)
    # First, test when we don't have type information.
    grid_type = symbols.DataTypeSymbol("grid_type", symbols.DeferredType())
    sym = symbols.DataSymbol("grid_var", grid_type)
    ref = nodes.StructureReference.create(sym, [("data", [one.copy()])])
    lbnd = ref.member.lbound(0)
    assert isinstance(lbnd, nodes.BinaryOperation)
    out = fortran_writer(lbnd).lower()
    assert out == "lbound(grid_var%data, 1)"
    usym = symbols.DataSymbol("uvar", symbols.DeferredType())
    ref = nodes.ArrayOfStructuresReference.create(
        usym, [one.copy()],
        [("map", [one.copy(), two.copy()]),
         ("data", [one.copy()])])
    lbnd = ref.member.member.lbound(0)
    assert isinstance(lbnd, nodes.BinaryOperation)
    out = fortran_writer(lbnd).lower()
    assert out == "lbound(uvar(1)%map(1,2)%data, 1)"
    # Second, test when we do have type information.
    a2d = symbols.ArrayType(symbols.REAL_TYPE, [2, (2, 8)])
    # Structure that contains "map" which is a 2D array.
    stypedef = symbols.StructureType.create(
        [("map", a2d, symbols.Symbol.Visibility.PUBLIC)])
    stypedefsym = symbols.DataTypeSymbol("map_type", stypedef)
    # Structure containing a structure of stypedef and an array of such
    # structures.
    stypedef2 = symbols.StructureType.create(
        [("grid", stypedef, symbols.Symbol.Visibility.PUBLIC),
         ("subgrids", symbols.ArrayType(stypedefsym, [3, (2, 6)]),
          symbols.Symbol.Visibility.PUBLIC)])
    ssym = symbols.DataSymbol("var", stypedef2)
    sref = nodes.StructureReference.create(ssym,
                                           ["grid",
                                            ("map", [two.copy(), two.copy()])])
    assert sref.member.member.lbound(0) == one
    assert sref.member.member.lbound(1) == two
    sref2 = nodes.StructureReference.create(
        ssym,
        [("subgrids", [two.copy(), two.copy()]),
         ("map", [two.copy(), two.copy()])])
    assert sref2.member.lbound(1) == two
    assert sref2.member.member.lbound(1) == two


def test_am_same_array():
    ''' Test the is_same_array method of ArrayMember. '''
    one = nodes.Literal("1", symbols.INTEGER_TYPE)
    two = nodes.Literal("2", symbols.INTEGER_TYPE)
    amem1 = nodes.ArrayMember.create(
        "subdomains",
        [one.copy(), nodes.Literal("2", symbols.INTEGER_TYPE)])
    # Check when the ArrayMember has no parent Reference
    result = amem1.is_same_array(
        nodes.Reference(symbols.DataSymbol("fake",
                                           symbols.INTEGER_TYPE)))
    assert result is False
    grid_type = symbols.DataTypeSymbol("grid_type", symbols.DeferredType())
    sym1 = symbols.DataSymbol("grid_var", grid_type)
    sym2 = symbols.DataSymbol("grid_var2", grid_type)
    ref1 = nodes.StructureReference.create(sym1, ["data"])
    # Reference is to a different symbol
    ref2 = nodes.StructureReference.create(sym2, [("data", [one.copy()])])
    assert ref2.member.is_same_array(ref1) is False
    # Reference is to a different member of the same symbol
    ref2 = nodes.StructureReference.create(sym1, [("xvals", [one.copy()])])
    assert ref2.member.is_same_array(ref1) is False
    ref1 = nodes.StructureReference.create(sym1, [("data", [one.copy()]),
                                                  ("xobs", [one.copy()])])
    ref2 = nodes.StructureReference.create(sym1, [("data", [one.copy()])])
    assert ref1.member.is_same_array(ref2) is True
    assert ref2.member.is_same_array(ref1) is False
    ref2 = nodes.StructureReference.create(sym1, [("data", [one.copy()]),
                                                  ("yobs", [one.copy()])])
    amem = ref2.member.member  # "yobs"
    assert amem.is_same_array(ref1) is False
    # The same 'signature' (a%b%c) but where b is an array access in one
    # case. This may not be possible in Fortran but we need to exercise
    # all conditions.
    ref1 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  "b", "c"])
    ref2 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy()]),
                                                  ("c", [one.copy()])])
    amem = ref2.walk(nodes.ArrayMember)[0]
    assert amem.is_same_array(ref1) is False
    # Same 'signature' but with one array access having more dimensions.
    ref1 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy()]),
                                                  ("c", [one.copy()])])
    ref2 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy(),
                                                         one.copy()]),
                                                  ("c", [one.copy()])])
    amem = ref2.walk(nodes.ArrayMember)[0]
    assert amem.is_same_array(ref1) is False
    # Same 'signature' but with one array access having a different index.
    ref1 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy(),
                                                         one.copy()]),
                                                  ("c", [one.copy()])])
    ref2 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy(),
                                                         two.copy()]),
                                                  ("c", [one.copy()])])
    amem = ref2.walk(nodes.ArrayMember)[0]
    assert amem.is_same_array(ref1) is False
    # Reference to an element of the same array
    ref1 = nodes.StructureReference.create(sym1, ["data"])
    ref2 = nodes.StructureReference.create(sym1, [("data", [one.copy()])])
    assert ref2.member.is_same_array(ref1) is True
    # Reference to an ArrayOfStructures
    array_sym = symbols.DataSymbol("grids",
                                   symbols.ArrayType(grid_type, [two.copy()]))
    ref1 = nodes.ArrayOfStructuresReference.create(array_sym, [one.copy()],
                                                   ["data"])
    assert ref1.is_same_array(nodes.Reference(array_sym))
    # member being compared is not at the bottom of a derived-type access
    ref1 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy()])])
    ref2 = nodes.StructureReference.create(sym1, [("a", [one.copy()]),
                                                  ("b", [one.copy()]),
                                                  ("c", [one.copy()])])
    amem = ref2.member.member
    assert amem.name == "b"
    assert amem.is_same_array(ref1)
