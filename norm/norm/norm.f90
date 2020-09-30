program my_program
    implicit none
     
    integer :: n
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
    v(:) = 8

    
    call cpu_time(t1)
    print *, 'Vector norm = ', norm(n, v)
    call cpu_time(t2)
    print *, t2 - t1, " sec"
    
    call cpu_time(t1)
    print *, 'Vector norm by dnrm2 = ', dnrm2(n, v, 1)
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
    
    sum = 0
    
    do i=1,n
        sum = sum + x(i)*x(i)
    end do
    
    norm = sqrt(sum)
    
end function norm