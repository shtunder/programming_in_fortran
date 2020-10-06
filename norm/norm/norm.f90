program my_program
    implicit none
     
    integer :: n
    integer :: j
    real(8), allocatable :: v(:)
    real(8) :: t1, t2
    real(8) :: dnrm2
    
    interface
        real(8) function norm(n, x)
            integer, intent(in) :: n
            real(8), intent(in) :: x(n)
        end function norm
    end interface
    
    print *, "Enter n:"
    read(*,*) n
    
    allocate(v(n))
    
    do j=1,n
        call RANDOM_NUMBER(v(j)) 
        v(j) = v(j) * 10.0_8**160
    end do
    
    call cpu_time(t1)
    print *, 'vector norm = ', norm(n, v)
    call cpu_time(t2)
    print *, t2 - t1, " sec"
    
    call cpu_time(t1)
    print *, 'vector norm by dnrm2 = ', dnrm2(n, v, 1)
    call cpu_time(t2)
    print *, t2 - t1, " sec"
    
    deallocate(v)
    
    read(*,*)
end program my_program

    
real(8) function norm(n, x)
    implicit none
    
    integer, intent(in) :: n
    real(8), intent(in) :: x(n)
    real(8) :: sum
    integer :: i
    real(8) :: cur
    real(8) :: max
    
    sum = 0.0_8
    max = 0.0_8
    
    do  i=1,n
        cur = ABS(x(i))
        if (cur > max) then
            max = cur
        end if
    end do
    if (max == 0.0_8) then
        return
    end if
    
    do i=1,n
        cur = x(i) / max
        sum = sum + cur*cur
    end do
    
    norm = max * sqrt(sum) 
    
end function norm