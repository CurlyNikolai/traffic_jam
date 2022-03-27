module subroutines
  use mt95
  implicit none
  type :: car
    logical :: check
    integer :: id,pos
    real :: rate
  end type car
  
  contains

  !INITIALIZATON SUBROUTINE
  subroutine init(N,L,road,seed)
    integer,intent(in) :: N,L
    type (car),intent(out) :: road(:)
    integer :: space, i, seed, cars
    !init RNG
    call genrand_init(seed)
    !check that there is enough space for cars
    if (N>L) then
      print *,
      print *, "ERROR: More cars than space on the road!"
      print *,
      print *, "STOPPING SIMULATION DUE TO ERROR"; STOP
    end if
    if (N==L) then
      print *,
      print *, "ERROR: No space on road! (number of cars = roadlength)"
      print *,
      print *, "STOPPING SIMULATION DUE TO ERROR"; STOP
    end if
    space = nint(1.0/(real(N)/L))
    cars = 0
    !put cars on road at defined spacing
    do i=1,size(road) 
      if (mod(i,space)==0 .and. cars<N) then
        road(i)=car(.true.,i,i,0.0)
        cars=cars+1
      else
        road(i)=car(.false.,0,i,0.0)
      end if
    end do
    !if the cars don't fit with spacing, add rest of cars in a queue in the beginning!
    if (cars<N) then
    i = 1
      do while (cars<N)
        if (road(i)%check.eqv..false.) then
          road(i)=car(.true.,i,i,0.0)
          cars=cars+1
        end if
        i = i+1
      end do
    end if
    !calculate rates for all cars in init
    call calcrates(road)
  end subroutine init

  !PRINT ROAD
  subroutine printroad(road,step,time)
    implicit none
    type (car),intent(in) :: road(:)
    integer :: i
    integer,intent(in) :: step
    real,intent(in) :: time
    write(*,'(3g8.2,x,i8)',advance="no") time,step
    do i=1,size(road)
      if (road(i)%check.eqv..true.) then
        write(*,'(a1)',advance="no") "X"
      else
        write(*,'(a1)',advance="no") " "
      end if
    end do
    print *,
  end subroutine printroad

  !PRINT GRID
  subroutine printgrid(N)
    implicit none
    integer,intent(in) :: N
    integer :: i
    write(*,'(a4,a12)',advance="no") "time", "step"
    do i=1,N
      write(*, '(a1)',advance="no") "|"
    end do
    print *,
  end subroutine printgrid

  !CALCULATE RATE FOR EACH CAR
  subroutine calcrates(road)
    implicit none
    type (car), intent(out) :: road(:)
    integer :: i,j,d
    do i=1,size(road)
      if (road(i)%check.eqv..true.) then
        if (i<size(road)) then
          j=1
          d=0
          do while (road(i+j)%check.eqv..false.)
            j=j+1
            d=d+1
            if (i+j>size(road)) j=j-size(road)
          end do
        else
          j=1
          d=0
          do while (road(j)%check.eqv..false.)
            j=j+1
            d=d+1
          end do
        end if
        road(i)%rate=d
      else
        road(i)%rate=0
      end if
    end do
  end subroutine calcrates

  !PRINT ALL CARS WITH INFO
  subroutine printcars(road)
    implicit none
    type (car),intent(in) :: road(:)
    integer :: i
    do i=1,size(road)
      print *, road(i)
    end do
    print *,
  end subroutine printcars

  !MOVE CAR ONE STEP FORWARD, RECALCULATES RATES AND CUMULATIVE FUCNTION!!
  subroutine move(road,id,cumu)
    implicit none
    type (car),intent(out) :: road(:), cumu(:)
    integer,intent(in) :: id
    type (car) :: help
    if (road(id)%check.eqv..false.) then
      print *, 
      print *, "ERROR: trying to move empty space from", id, "to", id+1
      call printcars(road)
      print *,
      print *, "STOPPING SIMULATION DUE TO ERROR"; STOP
    else if (road(id+1)%check.eqv..true.) then
      print *,
      print *, "ERROR: collision alert from", id, "to", id+1
      call printcars(road)
      print *,
      print *, "STOPPING SIMULATION DUE TO ERROR"; STOP
    end if
    if (id<size(road)) then
      help=road(id)
      road(id)=road(id+1)
      road(id)%pos=id
      road(id+1)=help
      road(id+1)%pos=id+1
    else
      help=road(id)
      road(id)=road(1)
      road(id)%pos=id
      road(1)=help
      road(1)%pos=1
    end if
    !calculate rates and cumulative function
    call calcrates(road)
    call cumulative(road,cumu)
  end subroutine move

  !CALCULATE CUMULATIVE FUNCTION
  subroutine cumulative(road,cumu)
    implicit none
    type (car),intent(in) :: road(:)
    type (car),intent(out) :: cumu(:)
    integer :: i,n
    n=1
    do i=1,size(road)
      if (road(i)%check.eqv..true.) then
        cumu(n)=road(i)
        if (n>1) cumu(n)%rate=cumu(n)%rate+cumu(n-1)%rate
        n=n+1
      end if
    end do
  end subroutine cumulative

  !PICK POSITION FROM WHICH A CAR IS TO BE MOVED
  integer function pick(cumu)
    implicit none
    type (car), intent(in) :: cumu(:)
    integer :: i
    real(kind=genrand_real) :: u
    call genrand_real1(u)
    u=u*cumu(size(cumu))%rate
    if (u<cumu(1)%rate) then
      pick=cumu(1)%pos
    else
      do i=1,size(cumu)-1
        if (u>cumu(i)%rate .and. u<cumu(i+1)%rate) then
          pick=cumu(i+1)%pos
          exit
        end if
      end do
    end if
  end function pick 

  !INSERT TRAFFIC CAM IN THE TRAFFIC JAM, HUEHUE
  subroutine cam(road,cumu)
    implicit none
    type (car) :: road(:),cumu(:)
    real(kind=genrand_real) :: x
    if (road(size(road)/2)%check.eqv..true. .and. road(size(road)/2+1)%check.eqv..false.) then
      call genrand_real1(x)
      x=x*3
      road(size(road)/2)%rate=x
      call cumulative(road,cumu)
    end if
  end subroutine cam

  !SET SPEED LIMIT FOR ALL CARS
  subroutine speedlimit(road)
    implicit none
    type (car),intent(out) :: road(:)
    integer :: i
    do i=1,size(road)
      if (road(i)%rate>3) road(i)%rate=3
    end do
  end subroutine speedlimit


end module subroutines
