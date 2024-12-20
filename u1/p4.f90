subroutine calculate_2_norm(input_vector, n, norm2)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: input_vector(n)
    real, intent(out) :: norm2
    integer :: i

    norm2 = 0.0
    do i = 1, n
        norm2 = norm2 + input_vector(i) ** 2
    end do
    norm2 = sqrt(norm2)

end subroutine calculate_2_norm

subroutine calculate_1_norm(input_vector,n,norm1)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: input_vector(n)
    real, intent(out) :: norm1
    integer :: i

    norm1=0.0
    do i = 1, n
        norm1 = norm1 + input_vector(i)
    end do
    
end subroutine calculate_1_norm

subroutine calculate_inf_norm(input_vector,n,norm_inf)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: input_vector(n)
    real, intent(out) :: norm_inf
    integer :: i
    norm_inf = 0.0
    do i = 1, n
        if (abs(input_vector(i)) > abs(norm_inf)) then 
            norm_inf = abs(input_vector(i))
        end if
    end do
    
end subroutine calculate_inf_norm

subroutine yvector_generate(n, input_vector)
    implicit none
    integer, intent(in) :: n
    real, intent(out) :: input_vector(n)
    integer :: i

    do i = 1, n
        input_vector(i) = i
    end do

end subroutine yvector_generate

subroutine xvector_generate(n, input_vector)
    implicit none
    integer, intent(in) :: n
    real, intent(out) :: input_vector(n)
    integer :: i

    do i = n, 1, -1
        input_vector(i) = 1.0/i
    end do

end subroutine xvector_generate

subroutine vector_compute(n, input_vector)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: input_vector(n)
    real :: norm1,norm2,norm_inf
    
    call calculate_1_norm(input_vector, n, norm1)
    call calculate_2_norm(input_vector, n, norm2)
    call calculate_inf_norm(input_vector, n, norm_inf)

    print *, '向量的1-范数: ', norm1
    print *, '向量的2-范数: ', norm2
    print *, '向量的无穷范数: ', norm_inf

end subroutine vector_compute
program u1problem4
    implicit none
    integer :: n, i
    real, allocatable :: input_vectorx(:), input_vectory(:)
    real :: norm1,norm2,norm_inf
    
    do i = 1, 3
        
        n = 10**i
        allocate(input_vectorx(n))
        allocate(input_vectory(n))
        call yvector_generate(n, input_vectory)
        call xvector_generate(n, input_vectorx)
        print *, 'n = ', n
        print *, 'x向量'
        call vector_compute(n, input_vectorx)
        print *, 'y向量'
        call vector_compute(n, input_vectory)
        deallocate(input_vectorx)
        deallocate(input_vectory)
    
    end do

end program u1problem4