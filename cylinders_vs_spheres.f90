! build with gfortran -o cylinders_vs_spheres -O3 cylinders_vs_spheres.f90

program cylinders_vs_spheres
    implicit none
    integer, parameter :: N=2*10**4
    
    real(kind=8), parameter :: radius=1.d-2, height=3.d-2
    real(kind=8), parameter :: double_radius_squared = 2.*radius**2
    
    real(kind=8), dimension(N,3) :: pos
    
    integer :: ii,jj,ncols
    
    real :: start,finish
    
    call random_number(pos) ! positions are random numbers from 0 to 1
    
    call cpu_time(start)
    ncols = 0
    do ii=1,N-1
        do jj=ii+1,N
            if ( sum((pos(ii,:)-pos(jj,:))**2)<=double_radius_squared ) then
                ncols = ncols + 1
            endif
        end do
    end do
    call cpu_time(finish)
    
    print *,"sphere collisions:",ncols,"time=",finish-start

    call cpu_time(start)
    ncols = 0
    do ii=1,N-1
        do jj=ii+1,N
            if ( sum((pos(ii,1:2)-pos(jj,1:2))**2)<=double_radius_squared &
                 .and. abs(pos(ii,3)-pos(jj,3))<=height ) then
                ncols = ncols + 1
            endif
        end do
    end do
    call cpu_time(finish)
    
    print *,"cylinder collisions:",ncols,"time=",finish-start
    
end program