subroutine bigtosmall(n)
    integer*8, intent(in) :: n
    integer*8::i
    real(4)::Sn
    Sn = 0.0
    do i = 2, n
        Sn = Sn + 1.0/(i**2-1)        
    end do
    print*,"bigtosmall=", Sn
    return
end subroutine bigtosmall
subroutine smalltobig(n)
    integer*8, intent(in) :: n
    integer*8::i
    real(4)::Tn
    Tn=0.0
    do i = n, 2, -1
        Tn = Tn + 1.0_8/(i**2-1)        
    end do
    print*,"smalltobig=",Tn
    return
end subroutine smalltobig
subroutine standard(n)
    integer*8, intent(in) :: n
    real(4)::Standard1
    Standard1 = 1.0/2.0*(3.0/2.0-(n+1.0)/((n+1.0)*(n)))
    print*, "标准 = ", Standard1
    return
end subroutine standard
program u1problem1
    implicit none
    integer*8::n,k
    do k=2,6,2
        n=10**k
        print*, n
        call smalltobig(n)
        call bigtosmall(n)
        call standard(n)
    end do
end program u1problem1