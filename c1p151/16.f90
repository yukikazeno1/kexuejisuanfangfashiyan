program problem16
    implicit none
    real*8::x=1,x_step
    do 
        x_step = x - (x**3+2*x**2+10*x-20)/(3*x**2+4*x+10)
        if (abs(x_step-x) < 1e-5) exit
        x = x_step
    end do
    print*, x_step
end program problem16