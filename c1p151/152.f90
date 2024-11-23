program problem152
    implicit none
    real:: x_forward=-2.0,x=-1.5,x_step
    do 
        x_step = x-((x**3-3*x**2-x+9)/((x**3-3*x**2-x+9)-(x_forward**3-3*x_forward**2-x_forward+9)))*(x-x_forward)
        if (abs(x_step-x) < 1e-5) exit
        x_forward = x
        x = x_step
    end do
    print*, x_step
end program problem152