!/*****************************************************************************/
! *
! *  Elmer/Ice, a glaciological add-on to Elmer
! *  http://elmerice.elmerfem.org
! *
! *
! *  This program is free software; you can redistribute it and/or
! *  modify it under the terms of the GNU General Public License
! *  as published by the Free Software Foundation; either version 2
! *  of the License, or (at your option) any later version.
! *
! *  This program is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! *  GNU General Public License for more details.
! *
! *  You should have received a copy of the GNU General Public License
! *  along with this program (in file fem/GPL-2); if not, write to the
! *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
! *  Boston, MA 02110-1301, USA.
! *
! *****************************************************************************/
! ******************************************************************************
! *
! *  Author: F. Gillet-Chaulet (IGE)
! *  Email:  fabien.gillet-chaulet@univ-grenoble-alpes.fr
! *  Web:    http://elmerice.elmerfem.org
! *
! *  Original Date: 03-2018, 
! *  11-2020
! *  O. Gagliardini (IGE) modified the initial version from Fabien to adpat it for Glacier
! *****************************************************************************
!!! Compute standard 1D variables:
!   1: Time
!   2: Volume
!   3: Area    
!   4: Ablation Area    
!   5: Accumulation Area    
!   6: SMB Total
!   7: SMB Ablation 
!   8: SMB Accumulation 
!   9: Front elevation
!  10: X Front
!  11: Y Front
!  12: Front distance
! *****************************************************************************     
      SUBROUTINE Scalar_OUTPUT( Model,Solver,dt,TransientSimulation )
      USE DefUtils
      IMPLICIT NONE

      TYPE(Model_t) :: Model
      TYPE(Solver_t):: Solver
      REAL(KIND=dp) :: dt
      LOGICAL :: TransientSimulation,FoundXRef,FoundYRef

      TYPE(Variable_t),POINTER :: SMBVar,HVar,BedVar,ZsVar,ZsLoads,DzDtVar,EVar,IceMask,MassBalance
      INTEGER,POINTER :: Permutation(:)
      TYPE(ValueList_t), POINTER :: SolverParams
      REAL (KIND=DP), POINTER :: AltRange(:,:)

      REAL(KIND=dp) :: Volume,Vchange
      REAL(KIND=dp) :: TotalArea,AblaArea,AccuArea
      REAL(KIND=dp) :: TotalSMB,AblaSMB,AccuSMB
      REAL(KIND=dp) :: Hfront,XFront,YFront
      REAL(KIND=dp), SAVE :: XRefFront,YRefFront
      REAL(KIND=dp) :: ZsLoadsFlux

      REAL (KIND=dp), ALLOCATABLE, DIMENSION(:),SAVE :: NodeArea
      REAL (KIND=dp), ALLOCATABLE, DIMENSION(:),SAVE :: LocalArea
      REAL (KIND=dp), ALLOCATABLE, DIMENSION(:),SAVE :: NodalH,NodalBed,MinH,TotalSMBAlt,AreaAlt,AltitudeBin
      REAL (KIND=dp), ALLOCATABLE, DIMENSION(:),SAVE :: NodalSMB,NodalZs,NodalDzDt
      REAL (KIND=dp), ALLOCATABLE, DIMENSION(:),SAVE :: Val,ParVal
      REAL(KIND=dp),ALLOCATABLE,SAVE :: Basis(:), dBasisdx(:,:)

      INTEGER :: i
      INTEGER :: ierr
      INTEGER, SAVE :: Nbin
      INTEGER,PARAMETER :: NVal=13
      INTEGER, PARAMETER :: io=12
      INTEGER,PARAMETER :: DIM=3 !dimension of the pb restricted to 3 currently

      INTEGER :: FlowDofs

      LOGICAL,SAVE :: Firsttime=.TRUE.

      CHARACTER(LEN=MAX_NAME_LEN) :: SolverName='Scalar_OUTPUT_Glacier'
      CHARACTER(LEN=MAX_NAME_LEN),SAVE :: OUTPUT_FName, str
      CHARACTER(LEN=MAX_NAME_LEN),ALLOCATABLE,SAVE :: ValueNames(:)

      CALL GET_VARIABLES()
  
      IF (Firsttime.OR.Solver%Mesh%Changed) THEN

        IF (.NOT.ASSOCIATED(Solver%Variable)) & 
         CALL FATAL(SolverName,'Solver%Variable Not associated')
        IF (.NOT.ASSOCIATED(Solver%Matrix)) &
         CALL FATAL(SolverName,'Solver%Matrix Not associated')

        IF ( CurrentCoordinateSystem() /= Cartesian )  &
          CALL FATAL(SolverName,'Only For cartesian system')

        IF ( Model % Mesh % MeshDim /= DIM ) &
          CALL FATAL(SolverName,'Only For 2D plan view')

       !## DO SOME ALLOCATION
        CALL DO_ALLOCATION(Firsttime,Nbin)

       !## Name of Saved variables
        ValueNames(1)='Volume'
        ValueNames(2)='Volume Change'

        ValueNames(3)='Area'
        ValueNames(4)='Ablation Area'
        ValueNames(5)='Accumulation Area'

        ValueNames(6)='SMB Total'
        ValueNames(7)='SMB Ablation'
        ValueNames(8)='SMB Accumulation'
        ValueNames(9)='Residual Flux'
		
        SolverParams=>GetSolverParams(Solver)
        AltRange => ListGetConstRealArray(SolverParams,'SMB altitude Range')
        AltitudeBin= (/(i, i=floor(AltRange(1,1)),floor(AltRange(2,1)), 200)/)

        DO i=1,size(AltitudeBin,1)
           write (str, *) AltitudeBin(i)
           ValueNames(10+(i-1)*3)='Altitude bin '//trim(str)
           ValueNames(11+(i-1)*3)='SMBAlt Tot '//trim(str)
           ValueNames(12+(i-1)*3)='AreaAlt '//trim(str)
        ENDDO

        ValueNames(9+3*size(AltitudeBin,1)+1)='Front Elevation'
        ValueNames(9+3*size(AltitudeBin,1)+2)='X Front'
        ValueNames(9+3*size(AltitudeBin,1)+3)='Y Front'
        ValueNames(9+3*size(AltitudeBin,1)+4)='Front Distance'
      
        
        XRefFront = GetConstReal(SolverParams,'XRefFrontPosition',FoundXRef)
        YRefFront = GetConstReal(SolverParams,'YRefFrontPosition',FoundYRef)

        IF (Firsttime) CALL INIT_OUTPUT_FILE(OUTPUT_FName)
        IF (Firsttime) CALL COMPUTE_NodeArea(NodeArea)
        Firsttime=.FALSE.          
      END IF
	  
      CALL BODY_INTEGRATION(Volume,TotalArea,AblaARea,AccuArea,TotalSMB, &
           AblaSMB,AccuSMB,Hfront,XFront,YFront,ZsLoadsFlux,Vchange,TotalSMBAlt,AreaAlt,AltitudeBin) 

      Val(1)=Volume
      Val(2)=Vchange

      Val(3)=TotalArea
      Val(4)=AblaArea
      Val(5)=AccuArea

      Val(6)=TotalSMB
      Val(7)=AblaSMB
      Val(8)=AccuSMB
      Val(9)= ZsLoadsFlux
	  
      DO i=1,size(AltitudeBin,1)
         Val(10+(i-1)*3)=AltitudeBin(i)
         Val(11+(i-1)*3)=TotalSMBAlt(i)
         Val(12+(i-1)*3)=AreaAlt(i)
      ENDDO
	
      Val(9+3*size(AltitudeBin,1)+1)= Hfront
      Val(9+3*size(AltitudeBin,1)+2)= Xfront
      Val(9+3*size(AltitudeBin,1)+3)= Yfront

      IF (ParEnv % PEs > 1 ) THEN
        DO i=1,9
           CALL MPI_ALLREDUCE(Val(i),ParVal(i),1,&
                       MPI_DOUBLE_PRECISION,MPI_SUM,ELMER_COMM_WORLD,ierr)					  					  
        END DO
					   
        DO i=10,9+3*size(AltitudeBin,1)
           IF (mod(i,3)==1) THEN
              ParVal(i)=Val(i)
           ELSE
              CALL MPI_ALLREDUCE(Val(i),ParVal(i),1,&
                       MPI_DOUBLE_PRECISION,MPI_SUM,ELMER_COMM_WORLD,ierr)
           ENDIF
        END DO			   

        CALL MPI_ALLREDUCE(Val(9+3*size(AltitudeBin,1)+1),ParVal(9+3*size(AltitudeBin,1)+1),1,&
                       MPI_DOUBLE_PRECISION,MPI_MIN,ELMER_COMM_WORLD,ierr)
					   
        IF (Val(9+3*size(AltitudeBin,1)+1).NE.ParVal(9+3*size(AltitudeBin,1)+1)) THEN
           Val(9+3*size(AltitudeBin,1)+2) = Huge(1.0)
           Val(9+3*size(AltitudeBin,1)+3) = Huge(1.0)
        END IF
		
        DO i=9+3*size(AltitudeBin,1)+2,9+3*size(AltitudeBin,1)+3
           CALL MPI_ALLREDUCE(Val(i),ParVal(i),1,&
                       MPI_DOUBLE_PRECISION,MPI_MIN,MPI_COMM_WORLD,ierr)
        END DO

        Val(1:9+3*size(AltitudeBin,1)+3)=ParVal(1:9+3*size(AltitudeBin,1)+3)
		
      END IF
	  
      IF (FoundYRef.and.FoundYRef) THEN
         Val(9+3*size(AltitudeBin,1)+4)= &
           sqrt((Val(9+3*size(AltitudeBin,1)+2)-XRefFront)**2+(Val(9+3*size(AltitudeBin,1)+3)-YRefFront)**2)
      ELSE
         Val(9+3*size(AltitudeBin,1)+4)=0.0
      ENDIF
		
      IF ((ParEnv % PEs > 1 ).AND.(ParEnv % MyPe.NE.0)) RETURN

      IF( Solver % TimesVisited > 0 ) THEN
         OPEN(io,file=TRIM(OUTPUT_FName),position='append')
      ELSE
         OPEN(io,file=TRIM(OUTPUT_FName))
      END IF

      WRITE(io,'(ES22.12E3)',advance='no') GetTime()
      Do i=1,NVal-1+3*Nbin
        WRITE(io,'(ES22.12E3)',advance='no') Val(i)
      End do
        WRITE(io,'(ES22.12E3)') Val(NVal+3*Nbin)
      CLOSE(io)

      CONTAINS

      SUBROUTINE DO_ALLOCATION(Firsttime,Nbin)
      LOGICAL,INTENT(IN) :: Firsttime
      LOGICAL :: Found
      INTEGER :: M
      INTEGER :: N
      INTEGER,intent(out) :: Nbin
      REAL (KIND=DP), POINTER :: AltRange(:,:)
      TYPE(ValueList_t), POINTER :: SolverParams
	  
      IF (.NOT.Firsttime) &
            DEALLOCATE(NodalH,NodalBed,NodalZs,MinH,NodalSMB,NodalDzDt,&
                      LocalArea, &
                      Basis,dBasisdx,&
                      Val,ParVal,ValueNames,&
                      NodeArea,TotalSMBAlt,AreaAlt,AltitudeBin)
       N=Model % Mesh % NumberOfNodes
       M=Model % MaxElementNodes
		  
       SolverParams=>GetSolverParams(Solver)
       AltRange => ListGetConstRealArray(SolverParams,'SMB altitude Range',Found)
		  
       IF (.NOT.Found) &
            CALL FATAL(SolverName,'SMB altitude Range should be provided in scalar output solver')

       Nbin = floor((AltRange(2,1)-AltRange(1,1))/200)+1
 
       ALLOCATE(Basis(M),&
                   MinH(M),&
                   dBasisdx(M,3),&
                   NodalH(M),NodalZs(M),NodalBed(M),&
                   NodalSMB(M),NodalDzDt(M),&
                   LocalArea(M),&
                   Val(NVal+3*Nbin),ParVal(NVal+3*Nbin),ValueNames(NVal+3*Nbin),&
                   NodeArea(N),TotalSMBAlt(Nbin),AreaAlt(Nbin),AltitudeBin(Nbin))
      END SUBROUTINE DO_ALLOCATION

      SUBROUTINE INIT_OUTPUT_FILE(OUTPUT_FName)
        USE GeneralUtils
        IMPLICIT NONE
        CHARACTER(LEN=MAX_NAME_LEN),INTENT(OUT) :: OUTPUT_FName

        CHARACTER(LEN=MAX_NAME_LEN) ::NamesFile,&
                   OUTPUT_FName_D='glacier_Scalar_OUTPUT.dat'

        CHARACTER(LEN=MAX_NAME_LEN) :: DateStr 
        TYPE(ValueList_t), POINTER :: SolverParams
        LOGICAL :: Found
        INTEGER :: i

         SolverParams=>GetSolverParams(Solver)
         OUTPUT_FName = ListGetString(SolverParams,'File Name',Found)
         IF (.NOT.Found) OUTPUT_FName=OUTPUT_FName_D

         NamesFile = TRIM(OUTPUT_FName) // '.' // TRIM("names")
         
         IF ((ParEnv % PEs >1).AND.(ParEnv%MyPe.NE.0)) RETURN

         DateStr = FormatDate()

         OPEN(io,file=TRIM(NamesFile))
         WRITE(io,'(A)') 'File started at: '//TRIM(DateStr)
         WRITE(io,'(A)') ' '
         WRITE(io,'(A)') 'Elmer version: '//TRIM(GetVersion())
         WRITE(io,'(A)') 'Elmer revision: '//TRIM(GetRevision())
         WRITE(io,'(A)') 'Elmer Compilation Date: '//TRIM(GetCompilationDate())
         WRITE(io,'(A)') ' '
         WRITE(io,'(A)') 'Variables in columns of matrix:'//TRIM(OUTPUT_FName)
         WRITE(io,'(I4,": ",A)') 1,'Time'
         DO i=1,NVal+3*Nbin
           WRITE(io,'(I4,": ",A)') i+1,TRIM(ValueNames(i))
         END DO
         CLOSE(io)
      END SUBROUTINE INIT_OUTPUT_FILE

      SUBROUTINE GET_VARIABLES()
       LOGICAL :: Found
       CHARACTER(LEN=MAX_NAME_LEN) :: PVarName
       TYPE(ValueList_t), POINTER :: SolverParams

       SolverParams=>GetSolverParams(Solver)

       HVar    => VariableGet(Solver%Mesh%Variables,'Thickness',UnfoundFatal=.TRUE.)

       BedVar => VariableGet(Solver%Mesh%Variables,'BedDEM',UnfoundFatal=.TRUE.)

       ZsVar => VariableGet(Solver%Mesh%Variables,'Zs Top',UnfoundFatal=.TRUE.)

       ZsLoads => VariableGet(Solver%Mesh%Variables,'Zs Top Loads',UnfoundFatal=.TRUE.)

       DzDtVar => VariableGet(Solver%Mesh%Variables,'Zs Top Velocity',UnfoundFatal=.TRUE.)
  
       SMBVar => VariableGet(Solver%Mesh%Variables,'Mass Balance',UnfoundFatal=.TRUE.)

       Permutation => Solver%Variable%Perm

       PVarName = ListGetString(SolverParams,'Passive Variable Name',Found)
   
       IceMask => VariableGet(Solver%Mesh%Variables,'IcyMask',UnfoundFatal=.TRUE.)
   
       IF (Found) THEN
          EVar => VariableGet(Solver%Mesh%Variables,TRIM(PVarName),UnFoundFatal=.TRUE.)
          IF (EVar % Type .NE. Variable_on_elements) &
             CALL FATAL(TRIM(SolverName),'Passive Variable should be on elements')
          EVar % Values = +1._dp
       ELSE
          EVar => NULL()
       END IF

      END SUBROUTINE GET_VARIABLES

      SUBROUTINE COMPUTE_NodeArea(NodeArea)
      IMPLICIT NONE
      REAL(KIND=dp),INTENT(OUT) :: NodeArea(:)

      TYPE(Element_t), POINTER :: Element
      TYPE(Nodes_t),SAVE :: ElementNodes
      TYPE(GaussIntegrationPoints_t) :: IntegStuff
      REAL(KIND=dp) :: U,V,W,SqrtElementMetric
      INTEGER,POINTER :: Indexes(:)
      INTEGER :: n
      INTEGER :: t,i
      LOGICAL :: stat

      NodeArea=0._dp

      DO t=1,GetNOFActive()

         Element => GetActiveElement(t)
         n = GetElementNOFNodes(Element)
         Indexes => Element % NodeIndexes
         CALL GetElementNodes( ElementNodes, Element )
         ElementNodes % z =0._dp

         IntegStuff = GaussPoints( Element )

         DO i=1,IntegStuff % n
            U = IntegStuff % u(i)
            V = IntegStuff % v(i)
            ! W = IntegStuff % w(i) 
            ! We need the area projected on the horizontal plane
            W = 0.0
            stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
                        Basis,dBasisdx )

            NodeArea(Permutation(Indexes(1:n)))=NodeArea(Permutation(Indexes(1:n)))+&
                SqrtElementMetric*IntegStuff % s(i) * Basis(1:n)

         END DO
      END DO
      IF (ParEnv % PEs > 1 ) CALL ParallelSumVector( Solver % Matrix, NodeArea, 0 )
  
      END SUBROUTINE COMPUTE_NodeArea

      SUBROUTINE BODY_INTEGRATION(Volume,TotalArea,AblaARea,AccuArea,TotalSMB, &
               AblaSMB,AccuSMB,Hfront,Xfront,Yfront,ZsLoadsFlux,Vchange,TotalSMBAlt,AreaAlt,AltitudeBin)
      IMPLICIT NONE
      REAL(KIND=dp),INTENT(OUT) :: Volume,TotalArea,&
                       AblaArea,AccuArea,TotalSMB,AblaSMB,AccuSMB,&
                       Hfront,Xfront,Yfront,ZsLoadsFlux,Vchange
					   
      REAL(KIND=dp),dimension(:),INTENT(OUT) :: TotalSMBAlt,AreaAlt
      REAL(KIND=dp),dimension(:),INTENT(IN) :: AltitudeBin

      REAL(KIND=dp),parameter :: Fsmall=100.0*EPSILON(1.0)

      TYPE(Mesh_t),POINTER :: Mesh
      TYPE(Element_t),POINTER :: Element
      TYPE(ValueList_t), POINTER :: Material,BodyForce
      TYPE(GaussIntegrationPoints_t) :: IntegStuff
      TYPE(Nodes_t),SAVE :: ElementNodes
      TYPE(ValueList_t), POINTER :: SolverParams

      REAL(KIND=dp) :: U,V,W,SqrtElementMetric
      REAL(KIND=dp) :: cellarea,XRefFront,YRefFront
      REAL(KIND=dp) :: HAtIP,SMBAtIP
      REAL(KIND=dp) :: s

      LOGICAL :: IceFree
      LOGICAL :: stat
      LOGICAL :: Found

      INTEGER,POINTER :: NodeIndexes(:)
      INTEGER :: t
      INTEGER :: i,k
      INTEGER :: n
      INTEGER :: ne, nf

      ne=GetNOFActive()
 
      Volume=0._dp
      
      TotalArea = 0._dp
      AblaArea = 0._dp
      AccuArea = 0._dp
      
      TotalSMB = 0._dp
      AblaSMB = 0._dp
      AccuSMB = 0._dp
	  
      TotalSMBAlt = 0._dp
      AreaAlt = 0._dp
	      
      Hfront = Huge(1.0)

      ZsLoadsFlux = 0._dp
      Vchange = 0._dp
	  
	  
      DO t = 1,ne
         Element => GetActiveElement(t)

         n = GetElementNOFNodes(Element)
         NodeIndexes => Element % NodeIndexes
         CALL GetElementNodes( ElementNodes )
         ElementNodes % z = 0._dp

         IF ( CheckPassiveElement(Element) ) THEN
                 IF (ASSOCIATED(EVar)) &
                   EVar % Values (EVar % Perm (element % elementIndex)) = -1.0_dp
                 CYCLE
         ENDIF

         Material => GetMaterial(Element,Found)
         IF (.NOT.Found) &
            CALL FATAL(SolverName,'No Material Found')
         BodyForce => GetBodyForce( Element, Found )
         IF (.NOT.Found) &
            CALL FATAL(SolverName,'No Body Force Found')

         NodalH(1:n) = HVar%Values(HVar%Perm(NodeIndexes(1:n)))
         NodalBed(1:n) = BedVar%Values(BedVar%Perm(NodeIndexes(1:n)))
         NodalZs(1:n) = ZsVar%Values(ZsVar%Perm(NodeIndexes(1:n)))

         NodalDzDt(1:n) = DzDtVar%Values(DzDtVar%Perm(NodeIndexes(1:n)))
         
         NodalSMB(1:n) = SMBVar%Values(SMBVar%Perm(NodeIndexes(1:n)))
         !NodalSMB(1:n) = ListGetReal(BodyForce, 'Zs Top Accumulation Flux 3',n,NodeIndexes,UnfoundFatal=.TRUE.)

        ! CELL IS NOT ACTIVE ALL H VALUES BELOW MinH Value
         IceFree=.FALSE.
         ! IF (ALL((NodalH(1:n)-MinH(1:n)).LE.Fsmall).AND.ALL(NodalSMB(1:n).LT.0._dp)) IceFree=.TRUE.
         IF (ALL(IceMask%Values(IceMask%Perm(NodeIndexes(1:n)))<0.1)) IceFree=.TRUE.

        ! GO TO INTEGRATION
         cellarea=0._dp
         LocalArea=0._dp

         IntegStuff = GaussPoints( Element )
         DO i=1,IntegStuff % n
            U = IntegStuff % u(i)
            V = IntegStuff % v(i)
            ! W = IntegStuff % w(i)
            ! We need the area projected on the horizontal plane
            W = 0.0

            stat = ElementInfo(Element,ElementNodes,U,V,W,SqrtElementMetric, &
                        Basis,dBasisdx )
            s=SqrtElementMetric*IntegStuff % s(i)
            ! cell area
            cellarea=cellarea+s
            ! the area seen by each node
            LocalArea(1:n)=LocalArea(1:n)+ s * Basis(1:n)

            IF (IceFree) CYCLE

            ! Integrate H
            HAtIP=SUM(NodalH(1:n)*Basis(1:n))
            Volume=Volume+HAtIP*s
            Vchange=Vchange+SUM(NodalDzDt(1:n)*Basis(1:n))*s

            SMBAtIP=SUM(NodalSMB(1:n)*Basis(1:n))
   
            DO k=1,size(AltitudeBin,1)
               IF (ALL(NodalZs(1:n)<AltitudeBin(k)+100).and.(ALL(NodalZs(1:n)>AltitudeBin(k)-100))) THEN
                  TotalSMBAlt(k) = TotalSMBAlt(k) + SMBAtIP*s
                  AreaAlt(k) = AreaAlt(k) + s
               ENDIF
            ENDDO

            IF (SMBAtIP>0.0_dp) THEN
               AccuSMB = AccuSMB + SMBAtIP*s
               AccuArea = AccuArea + s
            ELSE
               AblaSMB = AblaSMB + SMBAtIP*s
               AblaArea = AblaArea + s
            END IF
         
            TotalArea = TotalArea + s
            TotalSMB = TotalSMB + SMBAtIP*s

          END DO

          ! find the front elevation (min z for Not IceFree)
          IF (.NOT.IceFree) THEN
             DO i=1,n
                ZsLoadsFlux = ZsLoadsFlux + &
                ZsLoads%Values(ZsLoads%Perm(NodeIndexes(i)))*LocalArea(i)/NodeArea(Permutation(NodeIndexes(i)))
             END DO

             IF (ANY(NodalZs(1:n)<Hfront)) THEN
                Hfront = MINVAL(NodalZs(1:n))
                nf = MINLOC(NodalZs(1:n), DIM=1)
                Xfront = Solver % Mesh % Nodes % x(NodeIndexes(nf))
                Yfront = Solver % Mesh % Nodes % y(NodeIndexes(nf))
             END IF
          END IF
       END DO 
      END SUBROUTINE BODY_INTEGRATION

      END
