program my_program
!    use blas95
!    use f95_precision
    implicit none
    
    real(8) :: v(100000)
    real :: t1, t2
    
    interface
        real(8) function norm(n, x)
            integer, intent(in) :: n
            real(8), intent(in) :: x(n)
        end function norm
    end interface
    
    v(:) = 8

    
    call cpu_time(t1)
    print *, 'Vector norm = ', norm(100000, v)
    call cpu_time(t2)
    print *, t2 - t1, " sec"
    
!    call cpu_time(t1)
!    print *, 'Vector norm by dnrm2 = ', dnrm2(100, v, 1)
!    call cpu_time(t2)
!    print *, t2 - t1, " sec"
    
    read(*,*)
end program my_program

    
real(8) function norm(n, x)
    implicit none
    
    integer, intent(in) :: n
    real(8), intent(in) :: x(n)
    real(8) :: a(n)
    real(8) :: sum
    integer :: i
    
    
    sum = 0
    a = x**2
    
    do i=1,n
        sum = sum + a(i)
    end do
    
    norm = sqrt(sum)
    
end function norm