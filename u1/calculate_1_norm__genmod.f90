        !COMPILER-GENERATED INTERFACE MODULE: Mon Nov 25 23:04:51 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CALCULATE_1_NORM__genmod
          INTERFACE 
            SUBROUTINE CALCULATE_1_NORM(INPUT_VECTOR,N,NORM1)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: INPUT_VECTOR(N)
              REAL(KIND=4), INTENT(OUT) :: NORM1
            END SUBROUTINE CALCULATE_1_NORM
          END INTERFACE 
        END MODULE CALCULATE_1_NORM__genmod