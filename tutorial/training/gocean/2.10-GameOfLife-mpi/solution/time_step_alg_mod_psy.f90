  MODULE psy_time_step_alg_mod
    USE field_mod
    USE kind_params_mod
    IMPLICIT NONE
    CONTAINS
    SUBROUTINE invoke_compute(neighbours, current, born, die)
      USE count_neighbours_mod, ONLY: count_neighbours_code
      USE compute_born_mod, ONLY: compute_born_code
      USE compute_die_mod, ONLY: compute_die_code
      USE combine_mod, ONLY: combine_code
      TYPE(r2d_field), intent(inout) :: neighbours
      TYPE(r2d_field), intent(inout) :: current
      TYPE(r2d_field), intent(inout) :: born
      TYPE(r2d_field), intent(inout) :: die
      INTEGER j
      INTEGER i

      CALL current%halo_exchange(1)
      DO j = neighbours%internal%ystart, neighbours%internal%ystop, 1
        DO i = neighbours%internal%xstart, neighbours%internal%xstop, 1
          CALL count_neighbours_code(i, j, neighbours%data, current%data)
        END DO
      END DO
      DO j = born%internal%ystart, born%internal%ystop, 1
        DO i = born%internal%xstart, born%internal%xstop, 1
          CALL compute_born_code(i, j, born%data, current%data, neighbours%data)
        END DO
      END DO
      DO j = die%internal%ystart, die%internal%ystop, 1
        DO i = die%internal%xstart, die%internal%xstop, 1
          CALL compute_die_code(i, j, die%data, current%data, neighbours%data)
        END DO
      END DO
      DO j = current%internal%ystart, current%internal%ystop, 1
        DO i = current%internal%xstart, current%internal%xstop, 1
          CALL combine_code(i, j, current%data, die%data, born%data)
        END DO
      END DO

    END SUBROUTINE invoke_compute
  END MODULE psy_time_step_alg_mod