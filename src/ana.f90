module analyze
  use subroutines
  implicit none

  contains

  !CALCULATE NUMBER OF QUEUES (NON-PERIODIC) FOR SIZE OF QUEUES ARRAY
  integer function qnumber(road)
    implicit none
    type (car),intent(in) :: road(:)
    integer :: i,j,n,qnum
    qnum=0
    do i=1,size(road)
      if (i>1 .and. road(i)%check.eqv..true. .and. road(i-1)%check.eqv..true.) then
        cycle
      else if (road(i)%check.eqv..true.) then
        j=1
        do while(i+j<size(road) .and. road(i+j)%check.eqv..true.)
          j=j+1
        end do
        qnum=qnum+1
      else
        n=0
        j=0
      end if
    end do
    qnumber = qnum
  end function qnumber

  !MAKE ARRAY WITH INFORMATION ABOUT EVERY QUEUE 
  subroutine anaque(road,queues)
    implicit none
    type (car),intent(in) :: road(:)
    integer,allocatable,intent(out) :: queues(:,:)
    integer :: i,j,n,k
    allocate(queues(2,qnumber(road)))
    k=0
    do i=1,size(road)
      if (i>1 .and. road(i)%check.eqv..true. .and. road(i-1)%check.eqv..true.) then
        cycle
      else if (road(i)%check.eqv..true.) then
        j=1
        do while(i+j<size(road) .and. road(i+j)%check.eqv..true.)
          j=j+1
        end do
        k=k+1
        queues(1,k)=i
        queues(2,k)=j
      else 
        n=0
        j=0     
      end if
    end do
  end subroutine anaque

  !PRINT QUEUE INFO
  subroutine printque(queues)
    implicit none
    integer,intent(in) :: queues(:,:)
    integer :: i,j
    do i=1,size(queues(1,:))
      print *, queues(1,i),queues(2,i)
    end do
  end subroutine printque

  !CALULATE NUMBER OF QUEUES (PERIODIC)
  integer function calcnum(queues)
    implicit none
    integer,intent(in) :: queues(:,:)
    integer :: i, num
    num=0
    do i=1,size(queues(1,:))
      if (queues(2,i)>1) num = num+1
    end do
    if (queues(1,1)==1 .and. queues(1,size(queues(1,:)))==100) num=num-1
    calcnum=num
  end function calcnum

  !CALCULATE MEAN LENGTH OF QUEUES IN STEP
  integer function calcmean(queues)
    implicit none
    integer,intent(in) :: queues(:,:)
    integer :: i, mean, tot, num
    tot=0
    num=calcnum(queues)
    do i=1,size(queues(1,:))
      if (queues(2,i)>1) tot=tot+queues(2,i)
    end do
    if (num>0) then
      mean=tot/num
    else
      mean=0
    end if
    calcmean=mean
  end function calcmean


end module analyze
 
