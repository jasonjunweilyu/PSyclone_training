# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab

'''Module providing a transformation script that converts the supplied
PSyIR to the Stencil intermediate representation (SIR) and

1) modifies any PSyIR min, abs and sign intrinsics to PSyIR code
beforehand using transformations, as SIR does not support intrinsics.

2) transforms implicit loops to explicit loops as the SIR does not
have the concept of implicit loops.

Translation to the SIR is limited to the NEMO API. The NEMO API has no
algorithm layer so all of the original code is captured in the invoke
objects. Therefore by translating all of the invoke objects, all of
the original code is translated.

'''
from __future__ import print_function
from psyclone.psyir.backend.sir import SIRWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.nemo import NemoKern
from psyclone.psyir.nodes import (UnaryOperation, BinaryOperation,
                                  NaryOperation, Operation, Assignment)
from psyclone.psyir.symbols import SymbolTable
from psyclone.psyir.transformations import Abs2CodeTrans, Sign2CodeTrans, \
    Min2CodeTrans
from psyclone.domain.nemo.transformations import NemoAllArrayRange2LoopTrans, \
    NemoAllArrayAccess2LoopTrans


def trans(psy):
    '''Transformation routine for use with PSyclone. Applies the PSyIR2SIR
    transform to the supplied invokes after replacing any ABS, SIGN or
    MIN intrinsics with equivalent code. This is done because the SIR
    does not support intrinsics.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    abs_trans = Abs2CodeTrans()
    sign_trans = Sign2CodeTrans()
    min_trans = Min2CodeTrans()
    nemo_loop_trans = NemoAllArrayRange2LoopTrans()
    nemo_loop_trans2 = NemoAllArrayAccess2LoopTrans()

    sir_writer = SIRWriter()
    fortran_writer = FortranWriter()

    # For each Invoke write out the SIR representation of the
    # schedule. Note, there is no algorithm layer in the NEMO API so
    # the invokes represent all of the original code.
    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule

        for assignment in schedule.walk(Assignment):
            nemo_loop_trans2.apply(assignment)

        for assignment in schedule.walk(Assignment):
            nemo_loop_trans.apply(assignment)

        for kernel in schedule.walk(NemoKern):

            # The NEMO api currently has no symbol table so create one
            # to allow the generation of new variables. Note, this
            # does not guarantee unique names as we don't know any of
            # the existing names (so generated names could clash).
            symbol_table = SymbolTable()

            kernel_schedule = kernel.get_kernel_schedule()
            for oper in kernel_schedule.walk(Operation):
                if oper.operator == UnaryOperation.Operator.ABS:
                    # Apply ABS transformation
                    abs_trans.apply(oper, symbol_table)
                elif oper.operator == BinaryOperation.Operator.SIGN:
                    # Apply SIGN transformation
                    sign_trans.apply(oper, symbol_table)
                elif oper.operator in [BinaryOperation.Operator.MIN,
                                       NaryOperation.Operator.MIN]:
                    # Apply (2-n arg) MIN transformation
                    min_trans.apply(oper, symbol_table)
        kern = fortran_writer(schedule)
        print(kern)
        kern = sir_writer(schedule)
        print(kern)

    return psy
