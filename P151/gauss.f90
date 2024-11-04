program gauss_update
    implicit none
    integer::n,i,j,k
    real::s
    real,allocatable::x(:)
    real,allocatable::a(:,:)
    write(*,*) '请输入矩阵的维数'
    read(*,*) n
    allocate(a(n, n+1))
    allocate(x(n))
    x=0
    write(*,*) '请输入矩阵的元素,先列后行'
    read(*,*) a
    print*,a(1,4)
    do i=1,n
        do j=i+1,n
            s=a(j,i)/a(i,i)
            do k=i,n+1
                a(j,k)=a(j,k)-s*a(k,i)
            end do
        end do
    end do
    x(n)=a(n,n+1)/a(n,n)
    do k = n-1, 1,-1
        x(k)=(a(k,n+1)-sum(a(k,k+1:n)*x(k+1:n)))/a(k,k)
    end do
    print*,x

end program gauss_update