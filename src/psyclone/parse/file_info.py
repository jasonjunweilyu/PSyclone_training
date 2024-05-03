# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Laboratory.

'''This module contains the FileInfo class, which TODO

'''

import os
import codecs

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import (Function_Subprogram, Interface_Block,
                                     Interface_Stmt, Procedure_Stmt,
                                     Subroutine_Subprogram, Use_Stmt)
from fparser.two.parser import ParserFactory
from fparser.two.utils import FortranSyntaxError, walk

from psyclone.configuration import Config
from psyclone.errors import InternalError, PSycloneError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Container, FileContainer
from psyclone.psyir.symbols import SymbolError


# ============================================================================
def log_decode_error_handler(err):
    '''
    A custom error handler for use when reading files. Simply skips any
    characters that cause decoding errors.

    :returns: 2-tuple containing replacement for bad chars (an empty string
              and the position from where encoding should continue).
    :rtype: tuple[str, int]

    '''
    return ("", err.end)


codecs.register_error("file-error-handler", log_decode_error_handler)


# ============================================================================
class FileInfo:
    # pylint: disable=too-many-instance-attributes
    '''This class stores mostly cached information about files: it stores
    the original filename, if requested it will read the file and then caches
    the plain text file.

    :param str filename: the name of the source file that stores this module
        (including path).

    '''

    def __init__(self, filename):
        self._filename = filename
        # A cache for the source code:
        self._source_code = None

    # ------------------------------------------------------------------------
    @property
    def filename(self):
        ''':returns: the filename that this FileInfo object represents.
        :rtype: str

        '''
        return self._filename

    # ------------------------------------------------------------------------
    def get_source(self):
        '''Returns the source code for the module. The first time, it
        will be read from the file, but the data is then cached.

        :returns: the source code.
        :rtype: str

        :raises ModuleInfoError: when the file cannot be read.

        '''
        if self._source_code is None:
            self._source_code = self.read_source(self._filename)

        return self._source_code

    # ------------------------------------------------------------------------
    @staticmethod
    def read_source(path):
        '''
        '''
        # Error handler is defined at the top of this file. It simply skips any
        # characters that result in decoding errors. (Comments in a code may
        # contain all sorts of weird things.)
        with open(path, "r", encoding='utf-8',
                  errors='file-error-handler') as file_in:
            return file_in.read()
