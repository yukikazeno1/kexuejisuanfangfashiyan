program sortest2
    implicit none
    real*8 A(4,4), b(4), x(4), w, eps
    integer :: i
    open (1, file='set-sor.dat')
    data A /5.0, 2.0, 1.0, -1.0, 1.0, 8.0, -2.0, 3.0, -1.0, 1.0, -4.0, 2.0, -2.0, 3.0, -1.0, 7.0/
    data b /-2.0, -6.0, 6.0, 12.0/
    eps = 1.0e-05 ! 迭代终止误差
    w = 1.0 ! 松弛因子，假设为1.0
    call sor(A, b, 4, x, w, eps)
    write(*,*) (x(i), i=1, 4)
    write(1,*) (x(i), i=1, 4)
end program sortest2

subroutine sor1(A, b, N, x, w, eps)
    implicit none
    integer :: N, i, j
    real*8 :: A(4,4), b(4), x(4), s, t(4), p, q
    do 10 i = 1, N
        t(i) = 0.0
    10  x(i) = 0.0
    20  p = 0.0
        do 50 i = 1, N
            t(i) = x(i)
            s = 0.0
            do 30 j = 1, N
                s = s + A(i, j) * x(j)
30          continue
        x(i) = x(i) + w * (b(i) - s) / A(i, i)
        q = abs(x(i) - t(i))
        if (q > p) p = q
50      continue
        if (p >= eps) goto 20
        return
    end subroutine sor1