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
        norm1 = norm1 + input_vector(n)
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
program u1problem4
    implicit none
    integer, parameter :: n = 5
    real :: input_vector(n)
    real :: norm1,norm2,norm_inf

    ! 初始化输入向量
    input_vector = (/ 1.0, 2.0, 3.0, 4.0, 5.0 /)

    call calculate_1_norm(input_vector, n, norm1)
    call calculate_2_norm(input_vector, n, norm2)
    call calculate_inf_norm(input_vector, n, norm_inf)

    print *, '向量的1-范数: ', norm1
    print *, '向量的2-范数: ', norm2
    print *, '向量的无穷范数: ', norm_inf

end program u1problem4