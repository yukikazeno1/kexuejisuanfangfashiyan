        !COMPILER-GENERATED INTERFACE MODULE: Tue Nov 26 16:50:36 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CALCULATE_2_NORM__genmod
          INTERFACE 
            SUBROUTINE CALCULATE_2_NORM(INPUT_VECTOR,N,NORM2)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: INPUT_VECTOR(N)
              REAL(KIND=4), INTENT(OUT) :: NORM2
            END SUBROUTINE CALCULATE_2_NORM
          END INTERFACE 
        END MODULE CALCULATE_2_NORM__genmod
