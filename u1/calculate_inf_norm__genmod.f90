        !COMPILER-GENERATED INTERFACE MODULE: Mon Nov 25 23:04:51 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CALCULATE_INF_NORM__genmod
          INTERFACE 
            SUBROUTINE CALCULATE_INF_NORM(INPUT_VECTOR,N,NORM_INF)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=4), INTENT(IN) :: INPUT_VECTOR(N)
              REAL(KIND=4), INTENT(OUT) :: NORM_INF
            END SUBROUTINE CALCULATE_INF_NORM
          END INTERFACE 
        END MODULE CALCULATE_INF_NORM__genmod
