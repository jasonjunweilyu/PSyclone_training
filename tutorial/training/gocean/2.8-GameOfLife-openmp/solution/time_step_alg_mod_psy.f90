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

      !$omp parallel do default(shared), private(i,j), schedule(static)
      DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
        DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
          CALL count_neighbours_code(i, j, neighbours%data, current%data)
        END DO
      END DO
      !$omp end parallel do
      !$omp parallel do default(shared), private(i,j), schedule(static)
      DO j = born%internal%ystart, born%internal%ystop, 1
        DO i = born%internal%xstart, born%internal%xstop, 1
          CALL compute_born_code(i, j, born%data, current%data, neighbours%data)
        END DO
      END DO
      !$omp end parallel do
      !$omp parallel do default(shared), private(i,j), schedule(static)
      DO j = die%internal%ystart, die%internal%ystop, 1
        DO i = die%internal%xstart, die%internal%xstop, 1
          CALL compute_die_code(i, j, die%data, current%data, neighbours%data)
        END DO
      END DO
      !$omp end parallel do
      !$omp parallel do default(shared), private(i,j), schedule(static)
      DO j = current%internal%ystart, current%internal%ystop, 1
        DO i = current%internal%xstart, current%internal%xstop, 1
          CALL combine_code(i, j, current%data, die%data, born%data)
        END DO
      END DO
      !$omp end parallel do

    END SUBROUTINE invoke_compute
    SUBROUTINE count_neighbours_code(i, j, neighbours, c)
      DOUBLEPRECISION, dimension(:,:), intent(out) :: neighbours
      DOUBLEPRECISION, dimension(:,:), intent(in) :: c
      INTEGER, intent(in) :: i
      INTEGER, intent(in) :: j

      neighbours(i,j) = c(i - 1,j - 1) + c(i,j - 1) + c(i + 1,j - 1) + c(i - 1,j) + c(i + 1,j) + c(i - 1,j + 1) + c(i,j + 1) + c(i &
&+ 1,j + 1)

    END SUBROUTINE count_neighbours_code
    SUBROUTINE compute_born_code(i, j, born, current, neighbours)
      DOUBLEPRECISION, dimension(:,:), intent(out) :: born
      DOUBLEPRECISION, dimension(:,:), intent(in) :: current
      DOUBLEPRECISION, dimension(:,:), intent(in) :: neighbours
      INTEGER, intent(in) :: i
      INTEGER, intent(in) :: j

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

      current(i,j) = current(i,j) - die(i,j) + born(i,j)

    END SUBROUTINE combine_code
  END MODULE psy_time_step_alg_mod