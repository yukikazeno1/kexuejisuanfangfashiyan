        !COMPILER-GENERATED INTERFACE MODULE: Sat Nov 23 22:49:17 2024
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SOR__genmod
          INTERFACE 
            SUBROUTINE SOR(A,B,N,X,OMEGA,EPSILON,N_STEP)
              INTEGER(KIND=4), INTENT(IN) :: N
              REAL(KIND=8), INTENT(INOUT) :: A(N,N)
              REAL(KIND=8), INTENT(INOUT) :: B(N)
              REAL(KIND=8), INTENT(INOUT) :: X(N)
              REAL(KIND=8), INTENT(IN) :: OMEGA
              REAL(KIND=8), INTENT(IN) :: EPSILON
              INTEGER(KIND=4), INTENT(IN) :: N_STEP
            END SUBROUTINE SOR
          END INTERFACE 
        END MODULE SOR__genmod
