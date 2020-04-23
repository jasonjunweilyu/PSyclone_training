# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Loop PSyIR node. '''

from __future__ import absolute_import
import os
import pytest
from psyclone.psyir.nodes import Loop, Literal, Schedule, Return, Assignment, \
    Reference
from psyclone.psyir.symbols import DataSymbol, REAL_SINGLE_TYPE, \
    INTEGER_SINGLE_TYPE
from psyclone.psyGen import PSyFactory
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import get_invoke, check_links
from psyclone.parse.algorithm import parse


def test_loop_navigation_properties():
    # pylint: disable=too-many-statements
    ''' Tests the start_expr, stop_expr, step_expr and loop_body
    setter and getter properties.

    '''
    loop = Loop()

    # Properties return an error if the node is incomplete
    error_str = ("Loop malformed or incomplete. It should have exactly 4 "
                 "children, but found")
    with pytest.raises(InternalError) as err:
        _ = loop.start_expr
    assert error_str in str(err.value)

    # Expressions that are not PSyIR are not accepted
    with pytest.raises(TypeError) as err:
        loop.start_expr = "start"
    assert "Only PSyIR nodes can be assigned as the Loop start expression" \
        ", but found '" in str(err.value)
    with pytest.raises(TypeError) as err:
        loop.stop_expr = "stop"
    assert "Only PSyIR nodes can be assigned as the Loop stop expression" \
        ", but found '" in str(err.value)
    with pytest.raises(TypeError) as err:
        loop.step_expr = "step"
    assert "Only PSyIR nodes can be assigned as the Loop step expression" \
        ", but found '" in str(err.value)

    loop.addchild(Literal("start", INTEGER_SINGLE_TYPE, parent=loop))
    loop.addchild(Literal("stop", INTEGER_SINGLE_TYPE, parent=loop))
    loop.addchild(Literal("step", INTEGER_SINGLE_TYPE, parent=loop))

    # If it's not fully complete, it still returns an error
    with pytest.raises(InternalError) as err:
        _ = loop.start_expr
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = loop.stop_expr
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = loop.step_expr
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = loop.loop_body
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.start_expr = Literal("invalid", INTEGER_SINGLE_TYPE, parent=loop)
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.stop_expr = Literal("invalid", INTEGER_SINGLE_TYPE, parent=loop)
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.step_expr = Literal("invalid", INTEGER_SINGLE_TYPE, parent=loop)
    assert error_str in str(err.value)

    # The fourth child has to be a Schedule
    loop.addchild(Literal("loop_body", INTEGER_SINGLE_TYPE, parent=loop))
    with pytest.raises(InternalError) as err:
        _ = loop.loop_body
    assert "Loop malformed or incomplete. Fourth child should be a " \
        "Schedule node, but found loop with " in str(err.value)

    # Fix loop and check that Getters properties work
    del loop.children[3]
    loop.addchild(Schedule(parent=loop))
    loop.loop_body.addchild(Return(parent=loop.loop_body))

    assert loop.start_expr.value == "start"
    assert loop.stop_expr.value == "stop"
    assert loop.step_expr.value == "step"
    assert isinstance(loop.loop_body[0], Return)

    # Test Setters
    loop.start_expr = Literal("newstart", INTEGER_SINGLE_TYPE, parent=loop)
    loop.stop_expr = Literal("newstop", INTEGER_SINGLE_TYPE, parent=loop)
    loop.step_expr = Literal("newstep", INTEGER_SINGLE_TYPE, parent=loop)

    assert loop.start_expr.value == "newstart"
    assert loop.stop_expr.value == "newstop"
    assert loop.step_expr.value == "newstep"


def test_loop_invalid_type():
    ''' Tests assigning an invalid type to a Loop object. '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0)
    sched = invoke.schedule
    loop = sched.children[0].loop_body[0]
    assert isinstance(loop, Loop)
    with pytest.raises(GenerationError) as err:
        loop.loop_type = "not_a_valid_type"
    assert ("loop_type value (not_a_valid_type) is invalid. Must be one of "
            "['inner', 'outer']" in str(err.value))


def test_loop_gen_code():
    ''' Check that the Loop gen_code method prints the proper loop '''
    base_path = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))), "test_files", "dynamo0p3")
    _, invoke_info = parse(os.path.join(base_path,
                                        "1.0.1_single_named_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # By default DynLoop has step = 1 and it is not printed in the Fortran DO
    gen = str(psy.gen)
    assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen

    # Change step to 2
    loop = psy.invokes.get('invoke_important_invoke').schedule[3]
    loop.step_expr = Literal("2", INTEGER_SINGLE_TYPE, parent=loop)

    # Now it is printed in the Fortran DO with the expression  ",2" at the end
    gen = str(psy.gen)
    assert "DO cell=1,mesh%get_last_halo_cell(1),2" in gen


def test_invalid_loop_annotations():
    ''' Check that the Loop constructor validates any supplied annotations. '''
    # Check that we can have 'was_where' on its own
    test_loop = Loop(annotations=['was_where'])
    assert test_loop.annotations == ['was_where']
    # Check that 'was_single_stmt' on its own raises an error
    with pytest.raises(InternalError) as err:
        Loop(annotations=['was_single_stmt'])
    assert ("Loop with the 'was_single_stmt' annotation must also have the "
            "'was_where'" in str(err.value))
    # Check that it's accepted in combination with 'was_where'
    test_loop = Loop(annotations=['was_single_stmt', 'was_where'])
    assert test_loop.annotations == ['was_single_stmt', 'was_where']


def test_loop_create():
    '''Test that the create method in the Loop class correctly
    creates a Loop instance.

    '''
    start = Literal("0", INTEGER_SINGLE_TYPE)
    stop = Literal("1", INTEGER_SINGLE_TYPE)
    step = Literal("1", INTEGER_SINGLE_TYPE)
    child_node = Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Reference(DataSymbol("i", REAL_SINGLE_TYPE)))
    loop = Loop.create("i", start, stop, step, [child_node])
    schedule = loop.children[3]
    assert isinstance(schedule, Schedule)
    check_links(loop, [start, stop, step, schedule])
    check_links(schedule, [child_node])
    result = FortranWriter().loop_node(loop)
    assert result == "do i = 0, 1, 1\n  tmp=i\nenddo\n"


def test_loop_create_invalid():
    '''Test that the create method in a Loop class raises the expected
    exception if the provided input is invalid.

    '''
    zero = Literal("0", INTEGER_SINGLE_TYPE)
    one = Literal("1", INTEGER_SINGLE_TYPE)
    children = [Assignment.create(
        Reference(DataSymbol("x", INTEGER_SINGLE_TYPE)),
        one)]

    # var_name is not a string.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(1, zero, one, one, children)
    assert ("var_name argument in create method of Loop class "
            "should be a string but found 'int'.") in str(excinfo.value)

    # start not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create("i", "invalid", one, one, children)
    assert ("start argument in create method of Loop class should "
            "be a PSyIR Node but found 'str'.") in str(excinfo.value)

    # stop not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create("i", zero, "invalid", one, children)
    assert ("stop argument in create method of Loop class should "
            "be a PSyIR Node but found 'str'.") in str(excinfo.value)

    # step not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create("i", zero, one, "invalid", children)
    assert ("step argument in create method of Loop class should "
            "be a PSyIR Node but found 'str'.") in str(excinfo.value)

    # children not a list
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create("i", zero, one, one, "invalid")
    assert ("children argument in create method of Loop class should "
            "be a list but found 'str'." in str(excinfo.value))

    # contents of children list are not Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create("i", zero, one, one, ["invalid"])
    assert (
        "child of children argument in create method of Loop class "
        "should be a PSyIR Node but found 'str'." in str(excinfo.value))
