# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''This module contains an abstract parent class for adjoint
transformations.

'''
from psyclone.psyGen import Transformation
from psyclone.psyir.backend.fortran import FortranWriter


class AdjointTransformation(Transformation):
    '''An abstract class for Adjoint transformations. Requires a list of
    active variables to be passed when creating an instance of the
    class.

    :param active_variables: a list of names of the active variables.
    :type active_variables: list of str
    :param writer: the writer to use when outputting error \
        information. Defaults to FortranWriter.
    :type writer: subclass of \
        :py:class:`psyclone.psyir.backend.visitor.PSyIRVisitor`

    '''
    def __init__(self, active_variables, writer=FortranWriter()):
        super(AdjointTransformation, self).__init__()
        # TODO Check the active_variables argument has valid content
        # TODO Check the writer argument is valid.
        self._active_variables = active_variables
        # The writer to use when outputting error information
        self._writer = writer

__all__ = ["AdjointTransformation"]
