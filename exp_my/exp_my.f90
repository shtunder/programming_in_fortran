program my_programm
    
    implicit none
    !запрет неявного определения типов
    
    real(8) :: my_ans, x, ans
    
    interface
        real(8) function exp_my(x)
            real(8), intent(IN) :: x
        end function exp_my
    end interface
    
    read(*,*), x
    
    my_ans = exp_my(x)
    ans = exp(x)
    print*, my_ans
    print*, ans

    read(*,*) 
    !pause 
end program my_programm
    
    
    
real(8) function exp_my(x)

    implicit none
    
    real(8), intent(IN) :: x
    real(8) :: sum 
    integer :: i 
    real(8) :: a 
    real(8) :: fact   
    real(8) :: eps
    
    sum = 1.0_8
    i = 0
    a = 1.0_8
    fact = 1.0_8
    eps = TINY(X)
    
   ! read(*,*), eps
    
    do while(a > eps)
	    i=i+1
        fact = fact*i
        a = x**i/fact
	    sum = sum + a
    end do
    
    exp_my = sum 
    
end function exp_my
    

    

    

    
