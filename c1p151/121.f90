program problem121
    implicit none
    real:: x=2,x_step
    do 
        x_step = x-(x**3-x**2-x-1)/(3*x**2-2*x-1)
        if (abs(x_step-x) < 1e-5) exit    
    end do
    print*, x_step
end program problem121
