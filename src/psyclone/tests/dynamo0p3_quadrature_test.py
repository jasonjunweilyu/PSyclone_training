# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Author R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

''' Module containing py.test tests for functionality related to
quadrature in the LFRic API '''


from __future__ import absolute_import, print_function
import os
import pytest
from fparser import api as fpapi
from psyclone.configuration import Config
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.errors import GenerationError, InternalError
from psyclone.dynamo0p3 import DynKernMetadata, DynKern, DynBasisFunctions
from psyclone.tests.lfric_build import LFRicBuild

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
API = "dynamo0.3"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"


def test_field_xyoz(tmpdir):
    ''' Tests that a call, with a set of fields requiring XYoZ
    quadrature, produces correct code. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output_decls = (
        "    SUBROUTINE invoke_0_testkern_qr_type(f1, f2, m1, a, m2, istp,"
        " qr)\n"
        "      USE testkern_qr, ONLY: testkern_qr_code\n"
        "      USE quadrature_xyoz_mod, ONLY: quadrature_xyoz_type, "
        "quadrature_xyoz_proxy_type\n"
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      INTEGER, intent(in) :: istp\n"
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      TYPE(quadrature_xyoz_type), intent(in) :: qr\n"
        "      INTEGER cell\n"
        "      REAL(KIND=r_def), allocatable :: basis_w1_qr(:,:,:,:), "
        "diff_basis_w2_qr(:,:,:,:), basis_w3_qr(:,:,:,:), "
        "diff_basis_w3_qr(:,:,:,:)\n"
        "      INTEGER dim_w1, diff_dim_w2, dim_w3, diff_dim_w3\n"
        "      REAL(KIND=r_def), pointer :: weights_xy_qr(:) => null(), "
        "weights_z_qr(:) => null()\n"
        "      INTEGER np_xy_qr, np_z_qr\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      TYPE(quadrature_xyoz_proxy_type) qr_proxy\n"
        "      INTEGER, pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n")
    assert output_decls in generated_code
    init_output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n"
        "      ! Create a mesh object\n"
        "      !\n"
        "      mesh => f1_proxy%vspace%get_mesh()\n"
        "      !\n"
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      qr_proxy = qr%get_quadrature_proxy()\n"
        "      np_xy_qr = qr_proxy%np_xy\n"
        "      np_z_qr = qr_proxy%np_z\n"
        "      weights_xy_qr => qr_proxy%weights_xy\n"
        "      weights_z_qr => qr_proxy%weights_z\n")
    assert init_output in generated_code
    compute_output = (
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w2 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      dim_w3 = m2_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w1_qr(dim_w1, ndf_w1, np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, np_xy_qr, "
        "np_z_qr))\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, np_xy_qr, np_z_qr))\n"
        "      ALLOCATE (diff_basis_w3_qr(diff_dim_w3, ndf_w3, np_xy_qr, "
        "np_z_qr))\n"
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function(BASIS, f1_proxy%vspace, dim_w1, "
        "ndf_w1, basis_w1_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, f2_proxy%vspace, "
        "diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n"
        "      CALL qr%compute_function(BASIS, m2_proxy%vspace, dim_w3, "
        "ndf_w3, basis_w3_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, m2_proxy%vspace, "
        "diff_dim_w3, ndf_w3, diff_basis_w3_qr)\n"
        "      !\n"
        "      ! Call kernels and communication routines\n"
        "      !\n"
        "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL f2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m1_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
        "        CALL m2_proxy%halo_exchange(depth=1)\n"
        "      END IF\n"
        "      !\n"
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_qr_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, a, m2_proxy%data, istp, ndf_w1, undf_w1, "
        "map_w1(:,cell), basis_w1_qr, ndf_w2, undf_w2, map_w2(:,cell), "
        "diff_basis_w2_qr, ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr, "
        "diff_basis_w3_qr, np_xy_qr, np_z_qr, weights_xy_qr, weights_z_qr)\n"
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      !\n"
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (basis_w1_qr, basis_w3_qr, diff_basis_w2_qr, "
        "diff_basis_w3_qr)\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_qr_type"
    )
    assert compute_output in generated_code


def test_edge_qr(tmpdir, dist_mem):
    ''' Check that we generate correct code when a kernel specifies
    that it requires edge quadrature. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.1.5_edge_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen_code = str(psy.gen).lower()

    assert ("use quadrature_edge_mod, only: quadrature_edge_type, "
            "quadrature_edge_proxy_type\n" in gen_code)
    assert "type(quadrature_edge_type), intent(in) :: qr\n" in gen_code
    assert (
        "      qr_proxy = qr%get_quadrature_proxy()\n"
        "      np_xyz_qr = qr_proxy%np_xyz\n"
        "      nedges_qr = qr_proxy%nedges\n"
        "      weights_xyz_qr => qr_proxy%weights_xyz\n" in gen_code)

    assert (
        "      ! compute basis/diff-basis arrays\n"
        "      !\n"
        "      call qr%compute_function(basis, f1_proxy%vspace, dim_w1, "
        "ndf_w1, basis_w1_qr)\n"
        "      call qr%compute_function(diff_basis, f2_proxy%vspace, "
        "diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n"
        "      call qr%compute_function(basis, m2_proxy%vspace, dim_w3, "
        "ndf_w3, basis_w3_qr)\n"
        "      call qr%compute_function(diff_basis, m2_proxy%vspace, "
        "diff_dim_w3, ndf_w3, diff_basis_w3_qr)\n" in gen_code)

    assert ("call testkern_qr_edges_code(nlayers, f1_proxy%data, "
            "f2_proxy%data, m1_proxy%data, a, m2_proxy%data, istp, "
            "ndf_w1, undf_w1, map_w1(:,cell), basis_w1_qr, ndf_w2, undf_w2, "
            "map_w2(:,cell), diff_basis_w2_qr, ndf_w3, undf_w3, "
            "map_w3(:,cell), basis_w3_qr, diff_basis_w3_qr, nedges_qr, "
            "np_xyz_qr, weights_xyz_qr)" in gen_code)


def test_face_qr(tmpdir, dist_mem):
    ''' Check that we generate correct code when a kernel specifies
    that it requires face quadrature. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.1.6_face_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    generated_code = str(psy.gen)
    print(generated_code)
    output_decls = (
        "      USE testkern_qr_faces, ONLY: testkern_qr_code\n"
        "      USE quadrature_face_mod, ONLY: quadrature_face_type, "
        "quadrature_face_proxy_type\n"
        "      USE function_space_mod, ONLY: BASIS, DIFF_BASIS\n")
    if dist_mem:
        output_decls += "      USE mesh_mod, ONLY: mesh_type\n"
    output_decls += (
        "      TYPE(field_type), intent(inout) :: f1\n"
        "      TYPE(field_type), intent(in) :: f2, m1, m2\n"
        "      TYPE(quadrature_face_type), intent(in) :: qr\n"
        "      INTEGER cell\n"
        "      REAL(KIND=r_def), allocatable :: basis_w1_qr(:,:,:,:), "
        "diff_basis_w2_qr(:,:,:,:), basis_w3_qr(:,:,:,:), "
        "diff_basis_w3_qr(:,:,:,:)\n"
        "      INTEGER dim_w1, diff_dim_w2, dim_w3, diff_dim_w3\n"
        "      REAL(KIND=r_def), pointer :: weights_xyz_qr(:,:) => null()\n"
        "      INTEGER np_xyz_qr, nfaces_qr\n"
        "      INTEGER nlayers\n"
        "      TYPE(field_proxy_type) f1_proxy, f2_proxy, m1_proxy, m2_proxy\n"
        "      TYPE(quadrature_face_proxy_type) qr_proxy\n"
        "      INTEGER, pointer :: map_w1(:,:) => null(), "
        "map_w2(:,:) => null(), map_w3(:,:) => null()\n"
        "      INTEGER ndf_w1, undf_w1, ndf_w2, undf_w2, ndf_w3, undf_w3\n")
    assert output_decls in generated_code
    init_output = (
        "      !\n"
        "      ! Initialise field and/or operator proxies\n"
        "      !\n"
        "      f1_proxy = f1%get_proxy()\n"
        "      f2_proxy = f2%get_proxy()\n"
        "      m1_proxy = m1%get_proxy()\n"
        "      m2_proxy = m2%get_proxy()\n"
        "      !\n"
        "      ! Initialise number of layers\n"
        "      !\n"
        "      nlayers = f1_proxy%vspace%get_nlayers()\n"
        "      !\n")
    if dist_mem:
        init_output += ("      ! Create a mesh object\n"
                        "      !\n"
                        "      mesh => f1_proxy%vspace%get_mesh()\n"
                        "      !\n")
    init_output += (
        "      ! Look-up dofmaps for each function space\n"
        "      !\n"
        "      map_w1 => f1_proxy%vspace%get_whole_dofmap()\n"
        "      map_w2 => f2_proxy%vspace%get_whole_dofmap()\n"
        "      map_w3 => m2_proxy%vspace%get_whole_dofmap()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w1\n"
        "      !\n"
        "      ndf_w1 = f1_proxy%vspace%get_ndf()\n"
        "      undf_w1 = f1_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w2\n"
        "      !\n"
        "      ndf_w2 = f2_proxy%vspace%get_ndf()\n"
        "      undf_w2 = f2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Initialise number of DoFs for w3\n"
        "      !\n"
        "      ndf_w3 = m2_proxy%vspace%get_ndf()\n"
        "      undf_w3 = m2_proxy%vspace%get_undf()\n"
        "      !\n"
        "      ! Look-up quadrature variables\n"
        "      !\n"
        "      qr_proxy = qr%get_quadrature_proxy()\n"
        "      np_xyz_qr = qr_proxy%np_xyz\n"
        "      nfaces_qr = qr_proxy%nfaces\n"
        "      weights_xyz_qr => qr_proxy%weights_xyz\n")
    assert init_output in generated_code
    init_output2 = (
        "      !\n"
        "      ! Allocate basis/diff-basis arrays\n"
        "      !\n"
        "      dim_w1 = f1_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w2 = f2_proxy%vspace%get_dim_space_diff()\n"
        "      dim_w3 = m2_proxy%vspace%get_dim_space()\n"
        "      diff_dim_w3 = m2_proxy%vspace%get_dim_space_diff()\n"
        "      ALLOCATE (basis_w1_qr(dim_w1, ndf_w1, np_xyz_qr, nfaces_qr))\n"
        "      ALLOCATE (diff_basis_w2_qr(diff_dim_w2, ndf_w2, np_xyz_qr, "
        "nfaces_qr))\n"
        "      ALLOCATE (basis_w3_qr(dim_w3, ndf_w3, np_xyz_qr, nfaces_qr))\n"
        "      ALLOCATE (diff_basis_w3_qr(diff_dim_w3, ndf_w3, np_xyz_qr, "
        "nfaces_qr))\n"
        "      !\n"
        "      ! Compute basis/diff-basis arrays\n"
        "      !\n"
        "      CALL qr%compute_function(BASIS, f1_proxy%vspace, dim_w1, "
        "ndf_w1, basis_w1_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, f2_proxy%vspace, "
        "diff_dim_w2, ndf_w2, diff_basis_w2_qr)\n"
        "      CALL qr%compute_function(BASIS, m2_proxy%vspace, dim_w3, "
        "ndf_w3, basis_w3_qr)\n"
        "      CALL qr%compute_function(DIFF_BASIS, m2_proxy%vspace, "
        "diff_dim_w3, ndf_w3, diff_basis_w3_qr)\n"
        "      !\n")
    if dist_mem:
        init_output2 += (
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      IF (f2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL f2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (m1_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL m1_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n"
            "      IF (m2_proxy%is_dirty(depth=1)) THEN\n"
            "        CALL m2_proxy%halo_exchange(depth=1)\n"
            "      END IF\n"
            "      !\n")
    else:
        init_output2 += (
            "      ! Call our kernels\n")
    assert init_output2 in generated_code
    if dist_mem:
        compute_output = (
            "      DO cell=1,mesh%get_last_halo_cell(1)\n")
    else:
        compute_output = (
            "      DO cell=1,f1_proxy%vspace%get_ncell()\n")
    compute_output += (
        "        !\n"
        "        CALL testkern_qr_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
        "map_w1(:,cell), basis_w1_qr, ndf_w2, undf_w2, map_w2(:,cell), "
        "diff_basis_w2_qr, ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr, "
        "diff_basis_w3_qr, nfaces_qr, np_xyz_qr, weights_xyz_qr)\n"
        "      END DO\n")
    if dist_mem:
        compute_output += (
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the above "
            "loop\n"
            "      !\n"
            "      CALL f1_proxy%set_dirty()\n"
            "      !\n")
    compute_output += (
        "      !\n"
        "      ! Deallocate basis arrays\n"
        "      !\n"
        "      DEALLOCATE (basis_w1_qr, basis_w3_qr, diff_basis_w2_qr, "
        "diff_basis_w3_qr)\n"
        "      !\n"
        "    END SUBROUTINE invoke_0_testkern_qr_type"
    )
    assert compute_output in generated_code


def test_face_and_edge_qr(dist_mem, tmpdir):
    ''' Check that we can handle a kernel that requires two types of
    quadrature. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.7_face_and_edge_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    gen_code = str(psy.gen)
    print(gen_code)
    assert (
        "CALL testkern_2qr_code(nlayers, f1_proxy%data, f2_proxy%data, "
        "m1_proxy%data, m2_proxy%data, "
        "ndf_w1, undf_w1, map_w1(:,cell), basis_w1_qr_face, basis_w1_qr_edge, "
        "ndf_w2, undf_w2, map_w2(:,cell), diff_basis_w2_qr_face, "
        "diff_basis_w2_qr_edge, "
        "ndf_w3, undf_w3, map_w3(:,cell), basis_w3_qr_face, basis_w3_qr_edge, "
        "diff_basis_w3_qr_face, diff_basis_w3_qr_edge, "
        "nfaces_qr_face, np_xyz_qr_face, weights_xyz_qr_face, "
        "nedges_qr_edge, np_xyz_qr_edge, weights_xyz_qr_edge)" in gen_code)


def test_field_qr_deref(tmpdir):
    ''' Tests that a call, with a set of fields requiring
    quadrature, produces correct code when the quadrature is supplied as the
    component of a derived type. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.1_single_invoke_qr_deref.f90"),
                           api="dynamo0.3")
    for dist_mem in [True, False]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)

        assert LFRicBuild(tmpdir).code_compiles(psy)
        gen = str(psy.gen)
        print(gen)
        assert (
            "    SUBROUTINE invoke_0_testkern_qr_type(f1, f2, m1, a, m2, istp,"
            " qr_data)\n" in gen)
        assert "TYPE(quadrature_xyoz_type), intent(in) :: qr_data" in gen


def test_internal_qr_err(monkeypatch):
    ''' Check that internal error for unrecognised QR type is raised
    as expected '''
    from psyclone import dynamo0p3
    # Monkeypatch the list of valid quadrature and evaluator shapes so we
    # get past some of the earlier checks
    monkeypatch.setattr(dynamo0p3, "VALID_EVALUATOR_SHAPES",
                        value=["gh_quadrature_xyz", "gh_quadrature_xyoz",
                               "gh_quadrature_xoyoz", "gh_quadrature_wrong"])
    monkeypatch.setattr(dynamo0p3, "VALID_QUADRATURE_SHAPES",
                        value=["gh_quadrature_xyz", "gh_quadrature_xyoz",
                               "gh_quadrature_xoyoz", "gh_quadrature_wrong"])
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.1.4_wrong_qr_shape.f90"),
                           api=API)
    with pytest.raises(InternalError) as excinfo:
        _ = PSyFactory(API).create(invoke_info)
    assert ("internal error: unsupported shape (gh_quadrature_wrong) "
            "found" in str(excinfo.value))


def test_dynbasisfunctions(monkeypatch):
    ''' Check that we raise internal errors as required. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    # Get hold of a DynBasisFunctions object
    evaluator = psy.invokes.invoke_list[0].evaluators

    # Test the error check in dynamo0p3.qr_basis_alloc_args() by passing in a
    # dictionary containing an invalid shape entry
    basis_dict = {"shape": "gh_wrong_shape"}
    from psyclone import dynamo0p3
    with pytest.raises(InternalError) as excinfo:
        _ = dynamo0p3.qr_basis_alloc_args("size1", basis_dict)
    assert ("Unrecognised shape (gh_wrong_shape) specified "
            in str(excinfo.value))

    # Monkey-patch it so that it doesn't have any quadrature args
    monkeypatch.setattr(evaluator, "_qr_vars", value=[])
    # Check that calling the various _initialise_... routines does nothing.
    # We pass parent=None so that if any of the routines get beyond the
    # initial check then they will fail.
    evaluator._initialise_xyz_qr(None)
    evaluator._initialise_xyoz_qr(None)
    evaluator._initialise_xoyoz_qr(None)

    # Check that the constructor raises an internal error if it encounters
    # a shape it doesn't recognise
    invoke = psy.invokes.invoke_list[0]
    sched = invoke.schedule
    call = sched.children[0].loop_body[0]
    assert isinstance(call, DynKern)
    monkeypatch.setattr(call, "_eval_shapes", ["not-a-shape"])
    with pytest.raises(InternalError) as err:
        _ = DynBasisFunctions(invoke)
    assert "Unrecognised evaluator shape: 'not-a-shape'" in str(err.value)


def test_dynbasisfns_setup(monkeypatch):
    ''' Check that DynInvokeBasisFns._setup_basis_fns_for_call() raises an
     internal error if an unrecognised evaluator shape is encountered or
    if it is passed something other than a Kernel object. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    call = sched.children[0].loop_body[0]
    assert isinstance(call, DynKern)
    dinf = DynBasisFunctions(psy.invokes.invoke_list[0])
    # Now we've created a DynBasisFunctions object, monkeypatch the call
    # to have the wrong shape and try and call setup_basis_fns_for_call()
    monkeypatch.setattr(call, "_eval_shapes", ["not-a-shape"])
    with pytest.raises(InternalError) as err:
        dinf._setup_basis_fns_for_call(call)
    assert "Unrecognised evaluator shape: 'not-a-shape'" in str(err.value)
    # Check that we get the expected error if the method is passed
    # something that is not a Kernel call
    with pytest.raises(InternalError) as err:
        dinf._setup_basis_fns_for_call("call")
    assert "Expected a DynKern object but got: " in str(err.value)


def test_dynbasisfns_initialise(monkeypatch):
    ''' Check that the DynBasisFunctions.initialise() method
    raises the expected InternalErrors. '''
    from psyclone.f2pygen import ModuleGen
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    dinf = DynBasisFunctions(psy.invokes.invoke_list[0])
    mod = ModuleGen(name="testmodule")
    # Break the shape of the first basis function
    dinf._basis_fns[0]["shape"] = "not-a-shape"
    with pytest.raises(InternalError) as err:
        dinf.initialise(mod)
    assert ("Unrecognised evaluator shape: 'not-a-shape'. Should be "
            "one of " in str(err.value))
    # Break the internal list of basis functions
    monkeypatch.setattr(dinf, "_basis_fns", [{'type': 'not-a-type'}])
    with pytest.raises(InternalError) as err:
        dinf.initialise(mod)
    assert ("Unrecognised type of basis function: 'not-a-type'. Should be "
            "either 'basis' or 'diff-basis'" in str(err.value))


def test_dynbasisfns_compute(monkeypatch):
    ''' Check that the DynBasisFunctions._compute_basis_fns() method
    raises the expected InternalErrors if an unrecognised type or shape of
    basis function is encountered. '''
    from psyclone.f2pygen import ModuleGen
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    dinf = DynBasisFunctions(psy.invokes.invoke_list[0])
    mod = ModuleGen(name="testmodule")
    # First supply an invalid shape for one of the basis functions
    dinf._basis_fns[0]["shape"] = "not-a-shape"
    with pytest.raises(InternalError) as err:
        dinf._compute_basis_fns(mod)
    assert ("Unrecognised shape 'not-a-shape' specified for basis function. "
            "Should be one of: ['gh_quadrature_xyoz', "
            in str(err.value))
    # Now supply an invalid type for one of the basis functions
    monkeypatch.setattr(dinf, "_basis_fns", [{'type': 'not-a-type'}])
    with pytest.raises(InternalError) as err:
        dinf._compute_basis_fns(mod)
    assert ("Unrecognised type of basis function: 'not-a-type'. Expected "
            "one of 'basis' or 'diff-basis'" in str(err.value))


def test_dynbasisfns_dealloc(monkeypatch):
    ''' Check that the DynBasisFunctions.deallocate() method
    raises the expected InternalError if an unrecognised type of
    basis function is encountered. '''
    from psyclone.f2pygen import ModuleGen
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    call = sched.children[0].loop_body[0]
    assert isinstance(call, DynKern)
    dinf = DynBasisFunctions(psy.invokes.invoke_list[0])
    mod = ModuleGen(name="testmodule")
    # Supply an invalid type for one of the basis functions
    monkeypatch.setattr(dinf, "_basis_fns", [{'type': 'not-a-type'}])
    with pytest.raises(InternalError) as err:
        dinf.deallocate(mod)
    assert ("Unrecognised type of basis function: 'not-a-type'. Should be "
            "one of 'basis' or 'diff-basis'" in str(err.value))


def test_dynkern_setup(monkeypatch):
    ''' Check that internal-consistency checks in DynKern._setup() work
    as expected '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.1.0_single_invoke_xyoz_qr.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)
    # Get hold of a DynKern object
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.children[3].loop_body[0]
    # Monkeypatch a couple of __init__ routines so that we can get past
    # them in the _setup() routine.
    from psyclone.psyGen import CodedKern
    monkeypatch.setattr(CodedKern, "__init__",
                        lambda me, ktype, kcall, parent, check: None)
    from psyclone.parse.algorithm import KernelCall
    monkeypatch.setattr(KernelCall, "__init__",
                        lambda me, mname, ktype, args: None)
    # Break the shape of the quadrature for this kernel
    monkeypatch.setattr(kern, "_eval_shapes", value=["gh_wrong_shape"])
    # Rather than try and mock-up a DynKernMetadata object, it's easier
    # to make one properly by parsing the kernel code.
    ast = fpapi.parse(os.path.join(BASE_PATH, "testkern_qr.F90"),
                      ignore_comments=False)
    name = "testkern_qr_type"
    dkm = DynKernMetadata(ast, name=name)
    # Finally, call the _setup() method
    with pytest.raises(InternalError) as excinfo:
        kern._setup(dkm, "my module", None, None)
    assert ("Evaluator shape(s) {'gh_wrong_shape'} is/are not "
            "recognised" in str(excinfo.value))


BASIS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(7) =                         &
          (/ arg_type(gh_field,    gh_write,     w0),       &
             arg_type(gh_operator, gh_readwrite, w1, w1),   &
             arg_type(gh_field,    gh_read,      w2),       &
             arg_type(gh_operator, gh_write,     w3, w3),   &
             arg_type(gh_field,    gh_write,     wtheta),   &
             arg_type(gh_operator, gh_readwrite, w2h, w2h), &
             arg_type(gh_field,    gh_read,      w2v)       &
           /)
     type(func_type), meta_funcs(7) =     &
          (/ func_type(w0, gh_basis),     &
             func_type(w1, gh_basis),     &
             func_type(w2, gh_basis),     &
             func_type(w3, gh_basis),     &
             func_type(wtheta, gh_basis), &
             func_type(w2h, gh_basis),    &
             func_type(w2v, gh_basis)     &
           /)
     integer, parameter :: iterates_over = cells
     integer, parameter :: gh_shape = gh_quadrature_xyoz
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_qr_basis_stub():
    ''' Test that basis functions for quadrature are handled correctly for
    kernel stubs '''
    ast = fpapi.parse(BASIS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    generated_code = str(kernel.gen_stub)
    output = (
        "  MODULE dummy_mod\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "    SUBROUTINE dummy_code(cell, nlayers, field_1_w0, op_2_ncell_3d, "
        "op_2, field_3_w2, op_4_ncell_3d, op_4, field_5_wtheta, "
        "op_6_ncell_3d, op_6, field_7_w2v, ndf_w0, undf_w0, map_w0, "
        "basis_w0_qr_xyoz, ndf_w1, basis_w1_qr_xyoz, ndf_w2, undf_w2, map_w2, "
        "basis_w2_qr_xyoz, ndf_w3, basis_w3_qr_xyoz, ndf_wtheta, undf_wtheta, "
        "map_wtheta, basis_wtheta_qr_xyoz, ndf_w2h, basis_w2h_qr_xyoz, "
        "ndf_w2v, undf_w2v, map_w2v, basis_w2v_qr_xyoz, np_xy_qr_xyoz, "
        "np_z_qr_xyoz, weights_xy_qr_xyoz, weights_z_qr_xyoz)\n"
        "      USE constants_mod, ONLY: r_def\n"
        "      IMPLICIT NONE\n"
        "      INTEGER, intent(in) :: nlayers\n"
        "      INTEGER, intent(in) :: ndf_w0\n"
        "      INTEGER, intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER, intent(in) :: ndf_w2\n"
        "      INTEGER, intent(in), dimension(ndf_w2) :: map_w2\n"
        "      INTEGER, intent(in) :: ndf_w2v\n"
        "      INTEGER, intent(in), dimension(ndf_w2v) :: map_w2v\n"
        "      INTEGER, intent(in) :: ndf_wtheta\n"
        "      INTEGER, intent(in), dimension(ndf_wtheta) :: map_wtheta\n"
        "      INTEGER, intent(in) :: undf_w0, ndf_w1, undf_w2, ndf_w3, "
        "undf_wtheta, ndf_w2h, undf_w2v\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_w0) :: "
        "field_1_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2) :: "
        "field_3_w2\n"
        "      REAL(KIND=r_def), intent(out), dimension(undf_wtheta) :: "
        "field_5_wtheta\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w2v) :: "
        "field_7_w2v\n"
        "      INTEGER, intent(in) :: cell\n"
        "      INTEGER, intent(in) :: op_2_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w1,ndf_w1,"
        "op_2_ncell_3d) :: op_2\n"
        "      INTEGER, intent(in) :: op_4_ncell_3d\n"
        "      REAL(KIND=r_def), intent(out), dimension(ndf_w3,ndf_w3,"
        "op_4_ncell_3d) :: op_4\n"
        "      INTEGER, intent(in) :: op_6_ncell_3d\n"
        "      REAL(KIND=r_def), intent(inout), dimension(ndf_w2h,ndf_w2h,"
        "op_6_ncell_3d) :: op_6\n"
        "      INTEGER, intent(in) :: np_xy_qr_xyoz, np_z_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), "
        "dimension(1,ndf_w0,np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w0_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w1,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w1_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w2_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_w3,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w3_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(1,ndf_wtheta,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_wtheta_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2h,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w2h_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(3,ndf_w2v,"
        "np_xy_qr_xyoz,np_z_qr_xyoz) :: basis_w2v_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_xy_qr_xyoz) :: "
        "weights_xy_qr_xyoz\n"
        "      REAL(KIND=r_def), intent(in), dimension(np_z_qr_xyoz) :: "
        "weights_z_qr_xyoz\n"
        "    END SUBROUTINE dummy_code\n"
        "  END MODULE dummy_mod")
    assert output in generated_code


def test_stub_basis_wrong_shape(monkeypatch):
    ''' Check that stub generation for a kernel requiring basis functions
    for quadrature raises the correct errors if the kernel meta-data is
    broken '''
    from psyclone import dynamo0p3
    ast = fpapi.parse(BASIS, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    monkeypatch.setattr(kernel, "_eval_shapes",
                        value=["gh_quadrature_wrong"])
    with pytest.raises(InternalError) as excinfo:
        _ = kernel.gen_stub
    assert ("Unrecognised evaluator shape: 'gh_quadrature_wrong'"
            in str(excinfo.value))
    monkeypatch.setattr(dynamo0p3, "VALID_QUADRATURE_SHAPES",
                        value=["gh_quadrature_xyz", "gh_quadrature_xyoz",
                               "gh_quadrature_xoyoz", "gh_quadrature_wrong"])
    # Add a fake QR rule for the invalid shape (so that we can get to the bit
    # of code we want to test)
    kernel.qr_rules["gh_quadrature_wrong"] = kernel.QRRule("arg", "arg_name",
                                                           [])
    with pytest.raises(NotImplementedError) as excinfo:
        _ = kernel.gen_stub
    assert ("unrecognised shape (gh_quadrature_wrong) specified in "
            "dynamo0p3.qr_basis_alloc_args" in str(excinfo.value))


def test_stub_dbasis_wrong_shape(monkeypatch):
    ''' Check that stub generation for a kernel requiring differential basis
    functions for quadrature raises the correct errors if the kernel meta-data
    is broken '''
    from psyclone import dynamo0p3
    # Change meta-data to specify differential basis functions
    diff_basis = BASIS.replace("gh_basis", "gh_diff_basis")
    print(diff_basis)
    ast = fpapi.parse(diff_basis, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    monkeypatch.setattr(kernel, "_eval_shapes",
                        value=["gh_quadrature_wrong"])
    with pytest.raises(InternalError) as excinfo:
        _ = kernel.gen_stub
    assert ("Unrecognised evaluator shape: 'gh_quadrature_wrong'"
            in str(excinfo.value))
    monkeypatch.setattr(dynamo0p3, "VALID_QUADRATURE_SHAPES",
                        value=["gh_quadrature_xyz", "gh_quadrature_xyoz",
                               "gh_quadrature_xoyoz", "gh_quadrature_wrong"])
    # Add a fake QR rule for the invalid shape (so that we can get to the bit
    # of code we want to test)
    kernel.qr_rules["gh_quadrature_wrong"] = kernel.QRRule("arg", "arg_name",
                                                           [])
    with pytest.raises(NotImplementedError) as excinfo:
        _ = kernel.gen_stub
    assert ("unrecognised shape (gh_quadrature_wrong) specified in "
            "dynamo0p3.qr_basis_alloc_args(). Should be" in str(excinfo.value))
