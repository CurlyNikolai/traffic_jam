program mainjam
  use mt95
  use subroutines
  use analyze
  implicit none
  real(kind=genrand_real) :: u
  integer :: seed, N, L, step, pos, steps, printstep, camcheck, speedlimitcheck
  integer,allocatable :: queues(:,:)
  type (car),allocatable :: road(:),cumu(:)
  real :: time,R
  character(len=32) :: arg1,arg2,arg3,arg4,arg5,arg6,arg7


  !CHECK THE COMMAND LINE ARGUMENT AMOUNT
  if (command_argument_count()<7) then
    print *,
    print *, "ERROR: Less than seven comman line arguments!"
    print *,
    print *, "When running the program, give seven command line arguments (all ints):"
    print *, "1: Seed for RNG"
    print *, "2: Number of cars"
    print *, "3: Length of road"
    print *, "4: Simulation steps"
    print *, "5: Print interval"
    print *, "6: Traffic cam on/off (1/0)"
    print *, "7: Speed limit on/off (1/0)"
    print *,
    print *, "STOPPING SIMULATION DUE TO ERROR"; STOP
  end if


  !ASSIGN COMMAND LINE ARGUMENTS
  call get_command_argument(1,arg1); read(arg1,*) seed
  call get_command_argument(2,arg2); read(arg2,*) N
  call get_command_argument(3,arg3); read(arg3,*) L
  call get_command_argument(4,arg4); read(arg4,*) steps
  call get_command_argument(5,arg5); read(arg5,*) printstep
  call get_command_argument(6,arg6); read(arg6,*) camcheck
  call get_command_argument(7,arg7); read(arg7,*) speedlimitcheck
  allocate(road(L),cumu(N))


  !INITIALIZE, CALC CUMULATIVE
  step=0
  time=0
  call init(N,L,road,seed)
  call cumulative(road,cumu)
  call printgrid(L)
  call printroad(road,step,time)
  call anaque(road,queues)

  !START KMC LOOP
  open(unit=1,file="dat.out",status="replace")
  do step=1,steps
    deallocate(queues) !empty the queues array 
    call anaque(road,queues) !fill the queues array
    pos=pick(cumu) !pick the car to be moved
    R=cumu(size(cumu))%rate 
    call genrand_real1(u)
    time=time-log(u)/R !update the time of the system
    call move(road,pos,cumu) !move the picked car
    if (camcheck==1) call cam(road,cumu) !places traffic camera into the system
    if (speedlimitcheck==1) call speedlimit(road) !sets speed limit for all cars
    if (mod(step,printstep)==0) then !check print interval
      call printroad(road,step,time) !print the road to terminal
      write(1,*) time,step,calcnum(queues),calcmean(queues) !write number of queues and length mean to dat.out
    end if
  end do
  close(1)


end program mainjam
