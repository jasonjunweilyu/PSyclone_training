# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2016.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing py.test tests for functionality related to quadrature
and evaluators in the LFRic API '''

import os
import pytest
import fparser
from fparser import api as fpapi
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")

CODE = '''
module testkern_eval
  type, extends(kernel_type) :: testkern_eval_type
    type(arg_type) :: meta_args(2) = (/                                  &
         arg_type(GH_FIELD,   GH_INC,  W0),                              &
         arg_type(GH_FIELD,   GH_READ, W1)                               &
         /)
    type(func_type) :: meta_funcs(2) = (/                                &
         func_type(W0, GH_BASIS),                                        &
         func_type(W1, GH_DIFF_BASIS)                                    &
         /)
    integer, parameter :: gh_shape = gh_evaluator
    integer, parameter :: iterates_over = cells
  contains
    procedure() :: code => testkern_eval_code
  end type testkern_eval_type
contains
  subroutine testkern_eval_code()
  end subroutine testkern_eval_code
end module testkern_eval
'''


def test_eval_mdata():
    ''' Check that we recognise "evaluator" as a valid gh_shape '''
    from psyclone.dynamo0p3 import DynKernMetadata
    fparser.logging.disable('CRITICAL')
    ast = fpapi.parse(CODE, ignore_comments=False)
    dkm = DynKernMetadata(ast, name="testkern_eval_type")
    assert dkm.get_integer_variable('gh_shape') == 'gh_evaluator'


def test_single_kern_eval():
    ''' Check that we generate correct code for a single kernel that
    requires both basis and differential basis functions for an
    evaluator '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.1_eval_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)
    print gen_code
    expected_decl = (
        "    SUBROUTINE invoke_0_testkern_eval_type(f0, f1)\n"
        "      USE testkern_eval, ONLY: testkern_eval_code\n"
        "      USE evaluate_function_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      TYPE(field_type), intent(inout) :: f0\n"
        "      TYPE(field_type), intent(in) :: f1\n"
        "      INTEGER cell\n"
        "      INTEGER df_w1, df_w0, df_nodal\n"
        "      REAL(KIND=r_def), allocatable :: basis_w0(:,:,:,:), "
        "diff_basis_w1(:,:,:,:)\n"
        "      INTEGER ndf_nodal_w0, dim_w0, diff_dim_w1\n"
        "      REAL(KIND=r_def), pointer :: nodes_w0(:,:) => null()\n"
        "      INTEGER ndf_w0, undf_w0, ndf_w1, undf_w1\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f0_proxy, f1_proxy\n"
        "      INTEGER, pointer :: map_w0(:,:) => null(), "
        "map_w1(:,:) => null()\n")
    assert expected_decl in gen_code

    expected_code = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f0_proxy = f0%get_proxy()\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f0_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w0 => f0_proxy%vspace%get_whole_dofmap()\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w0\n"
        "      !\n"
        "      ndf_w0 = f0_proxy%vspace%get_ndf()\n"
        "      undf_w0 = f0_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise evaluator-related quantities using the field(s) "
        "that are written to\n"
        "      !\n"
        "      ndf_nodal_w0  = f0_field_proxy%vspace%get_ndf()\n"
        "      nodes_w0 => f0_field_proxy%vspace%get_nodes()\n"
        "      !\n"
        "      ! Allocate basis arrays\n"
        "      !\n"
        "      dim_w0  = f0_field_proxy%vspace%get_dim_space()\n"
        "      ALLOCATE (basis_w0(dim_w0, ndf_w0, ndf_nodal_w0))\n"
        "      !\n"
        "      ! Allocate differential basis arrays\n"
        "      !\n"
        "      diff_dim_w1  = w1_field_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (diff_basis_w1(diff_dim_w1, ndf_w1, ndf_nodal_w0))\n"
        "      !\n"
        "      ! Compute basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_nodal_w0\n"
        "        DO df_w0=1,ndf_w0\n"
        "          basis_w0(:,df_w0,df_nodal) = "
        "f0_field_proxy%vspace%evaluate_function(BASIS,df_w0,"
        "nodes_w0(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      !\n"
        "      ! Compute differential basis arrays\n"
        "      !\n"
        "      DO df_nodal=1,ndf_nodal\n"
        "        DO df_w1=1,ndf_w1\n"
        "          diff_basis_w1(:,df_w1,df_nodal) = w1_field_proxy%vspace%evaluate_function(DIFF_BASIS,df_w1,nodes(:,df_nodal))\n"
        "        END DO\n"
        "      END DO\n"
        "      !\n"
        "      ! Call our kernels\n"
        "      !\n"
        "      DO cell=1,f0_proxy%vspace%get_ncell()\n"
        "        !\n"
        "        CALL testkern_eval_code(nlayers, f0_proxy%data, f1_proxy%data, ndf_w0, undf_w0, map_w0(:,cell), basis_w0, ndf_w1, undf_w1, map_w1(:,cell), diff_basis_w1)\n"
        "      END DO \n"
        "      !\n")
    assert expected_code in gen_code


def test_two_qr():
    ''' Check that we handle an invoke containing two kernels that each
    require quadrature '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.2_single_invoke_2qr.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)
    print gen_code


def test_qr_plus_eval():
    ''' Check that we handle an invoke containing two kernels, one
    requiring quadrature and one requiring an evaluator '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "6.2_qr_eval_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    gen_code = str(psy.gen)
    print gen_code

