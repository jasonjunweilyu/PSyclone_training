# -----------------------------------------------------------------------------
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

'''This module contains the HoistTrans transformation. HoistTrans
moves an assignment out of a parent loop if it is safe to do so. Hoist
is a name that is often used to describe this type of transformation.

'''

from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Loop, Assignment, Schedule
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class HoistTrans(Transformation):
    '''This transformation takes an assignment and moves it outside of
    its parent loop if it is valid to do so. For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import HoistTrans
    >>> code = ("program test\\n"
    ...         "  integer :: i,j,n\\n"
    ...         "  real :: a(n,n)\\n"
    ...         "  real value\\n"
    ...         "  do i=1,n\\n"
    ...         "    value = 1.0\\n"
    ...         "    do j=1,n\\n"
    ...         "      a(i,j) = value\\n"
    ...         "    end do\\n"
    ...         "  end do\\n"
    ...         "end program\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> hoist = HoistTrans()
    >>> hoist.apply(psyir.walk(Assignment)[0])
    >>> print(FortranWriter()(psyir))
    program test
      integer :: i
      integer :: j
      integer :: n
      real, dimension(n,n) :: a
      real :: value
    <BLANKLINE>
      value = 1.0
      do i = 1, n, 1
        do j = 1, n, 1
          a(i,j) = value
        enddo
      enddo
    <BLANKLINE>
    end program test
    <BLANKLINE>

    '''
    def apply(self, node, options=None):
        '''Applies the hoist transformation to the supplied assignment node
        within a loop, moving the assignment outside of the loop if it
        is valid to do so. Issue #1387 will also look to extend this
        transformation to other types of node.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(node, options)

        # Find the enclosing loop (the validate() method has already
        # verified that there is one).
        loop = node.ancestor(Loop)

        # Remove the assignment node
        node.detach()

        # Place the assignment node before the loop.
        loop.parent.children.insert(loop.position, node)

    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target for a hoist
        transformation. At this stage only an assignment statement is
        allowed to be hoisted, see #1387.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        :raises TransformationError: if the supplied node is not an \
            assignment.
        :raises TransformationError: if the assignment is not within a \
            loop.
        :raises TransformationError: if the assignment is not a direct \
            child of the the loop.

        '''
        # The node should be an assignment
        if not isinstance(node, Assignment):
            raise TransformationError(
                "The target of the HoistTrans transformation should be an "
                "assignment, but found '{0}'.".format(type(node).__name__))

        # The assignment should be within a loop.
        parent_loop = node.ancestor(Loop)
        if not parent_loop:
            raise TransformationError(
                "The supplied assignment node '{0}' should be within a loop, "
                "but no loop was found.".format(self._writer(node)))

        # The assignment should be directly within a loop i.e. no other
        # control logic in between.
        current = node.parent
        while current is not parent_loop:
            if not isinstance(current, Schedule):
                raise TransformationError(
                    "The supplied assignment node '{0}' should be directly "
                    "within a loop but found '{1}'."
                    "".format(self._writer(node), self._writer(current)))
            current = current.parent

        # Check dependency issues that might prevent hoisting:
        self.validate_dependencies(node, parent_loop)

    def validate_dependencies(self, statement, parent_loop):
        '''Checks if the variable usage allows to host the specified statement
        out of the loop, i.e. no dependency on loop variable (directly or
        indirectly) etc. This validation does not assume that the statement
        is an assignment, it should work with other types of nodes, too.

        :param statement: the statement that is to be hoisted out of \
            the loop.
        :type statement: \
            subclass of :py:class:`psyclone.psyir.nodes.Assignment`
        :param parent_loop: the loop out of which the statement is to \
            be hoisted.
        :type parent_loop: subclass of :py:class:`psyclone.psyir.nodes.Loop`

        :raises TransformationError: if a variable in the statement is read \
            and written in the statement (e.g. reduction: a=a+1).
        :raises TransformationError: if any variable that is written in the \
            statement is accessed previously.
        :raises TransformationError: if any variable that is written is \
            written more than once in the loop.
        :raises TransformationError: if the left- or right-hand-side of \
            the statement depends directly or indirectly on the loop \
            variable or loop iteration.

        '''
        # pylint: disable=too-many-locals
        # Collect all variable usages in the loop
        all_loop_vars = VariablesAccessInfo(parent_loop)

        # Collect all variables used in the statement that will be hoisted.
        all_statement_vars = VariablesAccessInfo(statement)

        # Determine the variables which are written and which are read-only:
        read_sigs = []
        write_sigs = []
        for sig in all_statement_vars.all_signatures:
            if all_statement_vars[sig].is_written():
                write_sigs.append(sig)
            else:
                read_sigs.append(sig)

        for written_sig in write_sigs:
            statement_accesses = all_statement_vars[written_sig]
            # If this written variable is also read in the statement to be
            # hoisted, we can't hoist (likely a # reduction statement: a=a+1)
            if statement_accesses.is_read():
                raise TransformationError("The variable '{0}' is read "
                                          "and written in the statement that "
                                          "is hoisted."
                                          .format(str(written_sig)))

            # Check if the variable is written or read before the first
            # access in the statement to be hoisted:
            written_node = statement_accesses[0].node
            # Get all access to that variable in the whole loop before the
            # first write access that is to be hoisted:
            loop_accesses = all_loop_vars[written_sig]
            if loop_accesses.is_accessed_before(written_node):
                raise TransformationError("The variable '{0}' is accessed "
                                          "before the statement that is "
                                          "hoisted.".format(str(written_sig)))

            # Make sure that there is no additional write statement to a
            # written variable in the loop outside of the statement to be
            # hoisted. E.g.:
            #    a = 3; b(i) = a;  a = 2;  c(i) = a
            # Hoisting any of the assignments to 'a' out is invalid.
            # This is done by counting the write accesses to the variable
            # in the loop and in the statement.
            writes_in_loop = sum(access.access_type == AccessType.WRITE
                                 for access in loop_accesses)
            writes_in_statement = sum(access.access_type == AccessType.WRITE
                                      for access in statement_accesses)
            if writes_in_loop > writes_in_statement:
                raise TransformationError("There is at least one additional "
                                          "write to the variable '{0}' in "
                                          "the loop.".format(str(written_sig)))

        # Now check if any variable read in the statement to be hoisted is
        # being written to somewhere in the loop. This especially includes
        # the loop variable (which is marked as write-read in the loop
        # statement). This will create a direct (loop variable) or indirect
        # dependency (to a value in the loop), which prohibits hoisting.

        for read_sig in read_sigs:
            # Get all access to this variable in the whole loop
            loop_accesses = all_loop_vars[read_sig]
            if loop_accesses.is_written():
                code = self._writer(statement).strip()
                raise TransformationError("The supplied statement node '{0}' "
                                          "depends on the variable '{1}' "
                                          "which is is written in the loop."
                                          .format(code, str(read_sig)))

    def __str__(self):
        return "Hoist an assignment outside of its parent loop"


# For Sphinx AutoAPI documentation generation
__all__ = ["HoistTrans"]
