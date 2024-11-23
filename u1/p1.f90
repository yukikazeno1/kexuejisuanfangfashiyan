subroutine bigtosmall(n)
    integer*8, intent(in) :: n
    integer::i
    real(4)::Sn
    Sn = 0.0
    do i = 2, n
        Sn = Sn + 1.0/(i**2-1)        
    end do
    print*, Sn
    Sn=0.0
    return
end subroutine bigtosmall
subroutine smalltobig(n)
    integer*8, intent(in) :: n
    integer::i
    real(4)::Tn=0.0

    do i = n, 2, -1
        Tn = Tn + 1.0/(i**2-1)        
    end do
    print*, Tn
    Tn=0.0
end subroutine smalltobig
program u1problem1
    implicit none
    integer*8::n,k
    real(8)::Tn=0.0
    do k=2,6,2
        n=10**k
        print*, n
        call smalltobig(n)
        call bigtosmall(n)
    end do
    do k = 1000000, 2, -1
        Tn = Tn + 1.0/(k**2-1)        
    end do
    print*, Tn
    Tn=0.0
end program u1problem1