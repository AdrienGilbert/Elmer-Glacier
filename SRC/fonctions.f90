FUNCTION GetMaskMB(Model, n, f) RESULT(g)
USE DefUtils
IMPLICIT NONE
TYPE(Model_t) :: Model
TYPE(ValueList_t), POINTER :: Constants
INTEGER :: n,i,j,nb,k,cont,nb_line,io
REAL(KIND=dp) :: f, g, D, Depth , x, y, z, mean_MaskMB
logical :: first_time=.true.
real,dimension(:,:),allocatable :: MaskMB_l
real,dimension(190,223,3) :: MaskMB

save first_time,MaskMB

if (first_time) then
  first_time=.false.
  

  
  open(1,file='Data/MaskCalcMB.dat',status='old')
  
  nb_line = 0
	do
		read(1,*,iostat=io)
		nb_line = nb_line + 1
		if (io/=0) exit
	enddo
	close(1)
	
	nb_line=nb_line-1
	
allocate(MaskMB_l(nb_line,3))
  
  
  open(1,file='Data/MaskCalcMB.dat',status='old')
  do i=1,nb_line
    read(1,*) MaskMB_l(i,1),MaskMB_l(i,2),MaskMB_l(i,3)
  enddo
  close(1)

  cont=0
  do i=1,190
    do j=1,223
      cont=cont+1

      MaskMB(i,j,1)=MaskMB_l(cont,1)
      MaskMB(i,j,2)=MaskMB_l(cont,2)
      MaskMB(i,j,3)=MaskMB_l(cont,3)
    enddo
  enddo

endif

x = model % nodes % x (n)
y = model % nodes % y (n)


k=floor((x-MaskMB(1,1,1))/40)+1
j=floor((y-MaskMB(1,1,2))/40)+1


IF ((j<=1).or.(j>=190).or.(k<=1).or.(k>=223)) THEN
      g=0.0
ELSE

mean_MaskMB=MaskMB(j,k,3)*(MaskMB(j,k+1,1)-x)*(MaskMB(j+1,k,2)-y)+MaskMB(j,k+1,3)*(x-MaskMB(j,k,1))&
&*(MaskMB(j+1,k,2)-y)+MaskMB(j+1,k,3)*(MaskMB(j,k+1,1)-x)*(y-MaskMB(j,k,2))+MaskMB(j+1,k+1,3)*&
&(x-MaskMB(j,k,1))*(y-MaskMB(j,k,2))
mean_MaskMB=mean_MaskMB/40/40

g=mean_MaskMB

ENDIF



END FUNCTION GetMaskMB