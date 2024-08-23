  MODULE psy_time_step_alg_mod
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_compute(neighbours, current, born, die)
      TYPE(r2d_field), intent(inout) :: neighbours
      TYPE(r2d_field), intent(inout) :: current
      TYPE(r2d_field), intent(inout) :: born
      TYPE(r2d_field), intent(inout) :: die
      INTEGER j
      INTEGER i

      neighbours%data_on_device = .true.
      neighbours%read_from_device_f => read_from_device
      current%data_on_device = .true.
      current%read_from_device_f => read_from_device
      born%data_on_device = .true.
      born%read_from_device_f => read_from_device
      die%data_on_device = .true.
      die%read_from_device_f => read_from_device
      !$acc enter data copyin(born,born%data,current,current%data,die,die%data,neighbours,neighbours%data)
      !$acc parallel default(present)
      !$acc loop independent collapse(2)
      DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
        DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
          CALL count_neighbours_code(i, j, neighbours%data, current%data)
          CALL compute_born_code(i, j, born%data, current%data, neighbours%data)
          CALL compute_die_code(i, j, die%data, current%data, neighbours%data)
        END DO
      END DO
      !$acc loop independent collapse(2)
      DO j = current%internal%ystart, current%internal%ystop, 1
        DO i = current%internal%xstart, current%internal%xstop, 1
          CALL combine_code(i, j, current%data, die%data, born%data)
        END DO
      END DO
      !$acc end parallel

    END SUBROUTINE invoke_compute
    SUBROUTINE count_neighbours_code(i, j, neighbours, c)
      DOUBLEPRECISION, dimension(:,:), intent(out) :: neighbours
      DOUBLEPRECISION, dimension(:,:), intent(in) :: c
      INTEGER, intent(in) :: i
      INTEGER, intent(in) :: j

      !$acc routine
      neighbours(i,j) = c(i - 1,j - 1) + c(i,j - 1) + c(i + 1,j - 1) + c(i - 1,j) + c(i + 1,j) + c(i - 1,j + 1) + c(i,j + 1) + c(i &
&+ 1,j + 1)

    END SUBROUTINE count_neighbours_code
    SUBROUTINE compute_born_code(i, j, born, current, neighbours)
      DOUBLEPRECISION, dimension(:,:), intent(out) :: born
      DOUBLEPRECISION, dimension(:,:), intent(in) :: current
      DOUBLEPRECISION, dimension(:,:), intent(in) :: neighbours
      INTEGER, intent(in) :: i
      INTEGER, intent(in) :: j

      !$acc routine
      born(i,j) = 0.0
      IF (current(i,j) == 0.0 .AND. neighbours(i,j) == 3.0) THEN
        born(i,j) = 1.0
      END IF

    END SUBROUTINE compute_born_code
    SUBROUTINE compute_die_code(i, j, die, current, neighbours)
      DOUBLEPRECISION, dimension(:,:), intent(out) :: die
      DOUBLEPRECISION, dimension(:,:), intent(in) :: current
      DOUBLEPRECISION, dimension(:,:), intent(in) :: neighbours
      INTEGER, intent(in) :: i
      INTEGER, intent(in) :: j

      !$acc routine
      die(i,j) = 0.0
      IF (current(i,j) > 0.0 .AND. (neighbours(i,j) < 2.0 .OR. neighbours(i,j) > 3.0)) THEN
        die(i,j) = 1.0
      END IF

    END SUBROUTINE compute_die_code
    SUBROUTINE combine_code(i, j, current, die, born)
      DOUBLEPRECISION, dimension(:,:), intent(out) :: current
      DOUBLEPRECISION, dimension(:,:), intent(in) :: die
      DOUBLEPRECISION, dimension(:,:), intent(in) :: born
      INTEGER, intent(in) :: i
      INTEGER, intent(in) :: j

      !$acc routine
      current(i,j) = current(i,j) - die(i,j) + born(i,j)

    END SUBROUTINE combine_code
    SUBROUTINE read_from_device(from, to, startx, starty, nx, ny, blocking)
      USE iso_c_binding, ONLY: c_ptr
      USE kind_params_mod, ONLY: go_wp
      TYPE(c_ptr), intent(in) :: from
      REAL(KIND=go_wp), DIMENSION(:, :), INTENT(INOUT), TARGET :: to
      INTEGER, intent(in) :: startx
      INTEGER, intent(in) :: starty
      INTEGER, intent(in) :: nx
      INTEGER, intent(in) :: ny
      LOGICAL, intent(in) :: blocking

      !$acc update host(to)

    END SUBROUTINE read_from_device
  END MODULE psy_time_step_alg_mod