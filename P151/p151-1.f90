program p1511
    implicit none
    integer::n,i,j,k,n_step
    real*8 s,omega,epsilon
    real,allocatable::x(:)
    real,allocatable::ab(:,:)
    write(*,*) '请输入矩阵的维数'
    read(*,*) n
    write(*,*) '请输入矩阵的元素,先列后行'
    read(*,*) ab
    write(*,*) '请输入初始x向量'
    read(*,*) x
    write(*,*) '请输入松弛因子'
    read(*,*) omega
    write(*,*) '请输入精度'
    read(*,*) epsilon
    write(*,*) '请输入最大迭代次数'
    read(*,*) n_step
    call sor(n,ab,x,omega,epsilon,n_step)
end program p1511

subroutine sor(n,ab,x,omega,epsilon,n_step)
    implicit none
    real*8 A(4,4),b(4),x(4),s,t(4),p,q
    ! integer::n,i,j,k,n_step
    ! real*8 p,q
    real,allocatable::t(:)
    ! allocate(ab(n, n+1))
    ! allocate(x(n))
    ! allocate(t(n))
    do 10 i=1,N
        t(i)=0.0
    10  x(i)=0.0
    20  p=0.0
        do 50 i=1,N
            t(i)=x(i)
            s=0.0
            do 30 j=1,N
                s=s+A(i,j)*x(j)
        30  continue
        x(i)=x(i)+w*(b(i)-s)/A(i,i)
        q=abs(x(i)-t(i))
        if(q.gt.p) p=q
    50  continue
        if(p.ge.eps) goto 20
        return
end subroutine sor