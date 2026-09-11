!PG RaFSIP and RafWBF PARAMETERS

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!This MODULE holds the subroutines which are used to initialize all  +
!built random forest regressors.                                     +
!For both RafSIP and RafWBF parameteriztions                         +
!Routines with postfix 'wbf' and vars with 'b' are related to RafWBF.+
!This MODULE CONTAINS the following routines:                        +
!  *forestbrhm                                                       +
!  *forestbr                                                         +
!  *forestall                                                        +
!  *forestbrds                                                       +
!  *forestbrwarm                                                     +
!  *forestwbf                                                        +
!Each subroutine opens, reads and stores the parameters of all 4     +
!random forest regressors. The initial .txt files are first          +
!converted into binary files so that the processing is faster.       +
!                                                                    +
!This module also includes the three subroutines that make all the   +
!random forest predictions needed in the microphysics routine.       +
!These are the following:                                            +
!  *runforest                                                        +
!  *runforestriv                                                     +
!  *runforestmulti                                                   +
!RafWBF uses *runforest routine                                      +
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module module_random_forests

   use micro_mg_utils, only: r8
   use cam_abortutils, only: endrun

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: sec_ice_readnl
   PUBLIC :: wbf_readnl
   PUBLIC :: sec_ice_init
   PUBLIC :: wbf_init

   PUBLIC :: runforest
   PUBLIC :: runforestriv
   PUBLIC :: runforestmulti

   !!MDIM DEFINES THE NUMBER OF FEATURES/INPUTS TO THE RaFSIP PARAMETERIZATION
   INTEGER, PARAMETER, PUBLIC :: MDIM5=5
   INTEGER, PARAMETER, PUBLIC :: MDIM6=6 
   INTEGER, PARAMETER, PUBLIC :: JBT=10  !!The number of trees in each random forest regressor

   !!  NUMBER OF FEATURES/INPUTS TO THE RaFWBF PARAMETERIZATION
   INTEGER, PARAMETER, PUBLIC :: MDIMB=6 ! for WBF
   INTEGER, PARAMETER, PUBLIC :: JBTB=10  !!The number of trees in each random forest regressor

   !!The maximum number of nodes across trees
   INTEGER, PARAMETER, PUBLIC :: MAX_NODES1=7705  !forestBRHM
   INTEGER, PARAMETER, PUBLIC :: MAX_NODES2=8219  !forestBR
   INTEGER, PARAMETER, PUBLIC :: MAX_NODES3=7833  !forestALL
   INTEGER, PARAMETER, PUBLIC :: MAX_NODES4=7093  !forestBRDS
   INTEGER, PARAMETER, PUBLIC :: MAX_NODES5=8593  !forestBRwarm
   INTEGER, PARAMETER, PUBLIC :: MAX_NODESB=124209  !forestWBF

   !!Thresh = threshold value at each internal node
   !!Outi = prediction for a given node
   REAL(r8), DIMENSION(JBT,MAX_NODES1), PUBLIC    :: THRESH1,OUT11,OUT12,OUT13
   REAL(r8), DIMENSION(JBT,MAX_NODES2), PUBLIC    :: THRESH2,OUT21
   REAL(r8), DIMENSION(JBT,MAX_NODES3), PUBLIC    :: THRESH3,OUT31,OUT32,OUT33,OUT34,OUT35
   REAL(r8), DIMENSION(JBT,MAX_NODES4), PUBLIC    :: THRESH4,OUT41,OUT42,OUT43
   REAL(r8), DIMENSION(JBT,MAX_NODES5), PUBLIC    :: THRESH5,OUT51
   REAL(r8), DIMENSION(JBTB,MAX_NODESB), PUBLIC   :: THRESHB,OUTB

   !!Splitfeat = feature used for splitting the node
   !!Leftchild = left child of node
   !!Rightchild = right child of node
   INTEGER, DIMENSION(JBT,MAX_NODES1), PUBLIC, PROTECTED :: SPLITFEAT1,LEFTCHILD1,RIGHTCHILD1
   INTEGER, DIMENSION(JBT,MAX_NODES2), PUBLIC, PROTECTED :: SPLITFEAT2,LEFTCHILD2,RIGHTCHILD2
   INTEGER, DIMENSION(JBT,MAX_NODES3), PUBLIC, PROTECTED :: SPLITFEAT3,LEFTCHILD3,RIGHTCHILD3
   INTEGER, DIMENSION(JBT,MAX_NODES4), PUBLIC, PROTECTED :: SPLITFEAT4,LEFTCHILD4,RIGHTCHILD4
   INTEGER, DIMENSION(JBT,MAX_NODES5), PUBLIC, PROTECTED :: SPLITFEAT5,LEFTCHILD5,RIGHTCHILD5
   INTEGER, DIMENSION(JBTB,MAX_NODESB), PUBLIC, PROTECTED :: SPLITFEATB,LEFTCHILDB,RIGHTCHILDB

   !!The exact number of nodes across in consecutive trees of the forest
   INTEGER, DIMENSION(JBT), PUBLIC, PROTECTED  :: NRNODES1,NRNODES2,NRNODES3,NRNODES4,NRNODES5
   INTEGER, DIMENSION(JBTB), PUBLIC, PROTECTED :: NRNODESB

   ! RAFWBF specific parameters
   REAL(r8), PUBLIC, PARAMETER :: TUPB = 273.15 ! Upper bound for temperature for running RF [K]
   REAL(r8), PUBLIC, PARAMETER :: TLOB = 238.15 ! Lower bound for temperature for running RF [K]

   !! Namelist variables
   logical, public, protected :: rafsip_on = .false.
   logical, public, protected :: rafwbf_on = .false.

   character(len=256) :: forestfileALL = 'NONE'
   character(len=256) :: forestfileBRDS = 'NONE'
   character(len=256) :: forestfileBRHM = 'NONE'
   character(len=256) :: forestfileBR = 'NONE'
   character(len=256) :: forestfileBRwarm = 'NONE'
   character(len=256) :: forestfileWBF = 'NONE'

   !! Make sure init is only called once
   logical :: rafsip_initialized = .false.
   logical :: rafwbf_initialized = .false.

CONTAINS


   !---------------------------------------------------------------------------------------------------------------

   subroutine sec_ice_readnl(nlfile)
      ! Read files needed for random forest tables of seconary ice formation

      use mpi,            only: mpi_character, mpi_logical
      use spmd_utils,     only: masterproc, mstrid=>masterprocid, mpicom
      use namelist_utils, only: find_group_name
      use cam_logfile,    only: iulog

      character(len=*), intent(in) :: nlfile ! path to file containing namelist input

      ! Local variables
      integer                     :: unitn, ierr
      character(len=*), parameter :: subname = 'sec_ice_readnl'

      namelist /sec_ice_nl/ rafsip_on,                                        &
           forestfileALL,                                                     &
           forestfileBRDS,                                                    &
           forestfileBRHM,                                                    &
           forestfileBR,                                                      &
           forestfileBRwarm

      ! Initialize all namelist variables
      rafsip_on = .false.
      forestfileALL = 'None'
      forestfileBRDS = 'None'
      forestfileBRHM = 'None'
      forestfileBR = 'None'
      forestfileBRwarm = 'None'

      if (masterproc) then
         open(newunit=unitn, file=trim(nlfile), status='old' )
         call find_group_name(unitn, 'sec_ice_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, sec_ice_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname//':: ERROR reading namelist')
            end if
         end if
         close(unitn)
      end if

      call MPI_Bcast(rafsip_on, 1, mpi_logical, mstrid, mpicom, ierr)

      call MPI_Bcast(forestfileALL,   len(forestfileALL),    mpi_character,   &
           mstrid, mpicom, ierr)
      call MPI_Bcast(forestfileBRDS,  len(forestfileBRDS),   mpi_character,   &
           mstrid, mpicom, ierr)
      call MPI_Bcast(forestfileBRHM,  len(forestfileBRHM),   mpi_character,   &
           mstrid, mpicom, ierr)
      call MPI_Bcast(forestfileBR,    len(forestfileBR),     mpi_character,   &
           mstrid, mpicom, ierr)
      call MPI_Bcast(forestfileBRwarm,len(forestfileBRwarm), mpi_character,   &
           mstrid, mpicom, ierr)

      if (masterproc) then
         write(iulog ,*) 'Microphysics secondary ice namelist:'
         write(iulog ,*) '  rafsip_on        = ', rafsip_on
         if (rafsip_on) then
            write(iulog, *) '  forestfileALL    = ', trim(forestfileALL)
            write(iulog, *) '  forestfileBRDS   = ', trim(forestfileBRDS)
            write(iulog, *) '  forestfileBRHM   = ', trim(forestfileBRHM)
            write(iulog, *) '  forestfileBR     = ', trim(forestfileBR)
            write(iulog, *) '  forestfileBRwarm = ', trim(forestfileBRwarm)
         end if
      end if

   end subroutine sec_ice_readnl

   !------------------------------------------------------------------------+

   subroutine wbf_readnl(nlfile, mg_ver)
      ! Read files needed for random forest tables of wbf factor

      use mpi,            only: mpi_character, mpi_logical
      use spmd_utils,     only: masterproc, mstrid=>masterprocid, mpicom
      use namelist_utils, only: find_group_name
      use cam_logfile,    only: iulog

      character(len=*), intent(in) :: nlfile ! path to file containing namelist input
      integer,          intent(in) :: mg_ver

      ! Local variables
      integer                     :: unitn, ierr
      character(len=*), parameter :: subname = 'wbf_readnl'

      namelist /wbf_nl/ rafwbf_on, forestfileWBF

      ! Initialize all namelist variables
      rafwbf_on = .false.
      forestfileWBF = 'None'
      if (mg_ver>1) then
        if (masterproc) then
          open(newunit=unitn, file=trim(nlfile), status='old' )
          call find_group_name(unitn, 'wbf_nl', status=ierr)
          if (ierr == 0) then
              read(unitn, wbf_nl, iostat=ierr)
              if (ierr /= 0) then
                call endrun(subname//':: ERROR reading namelist')
              end if
          end if
          close(unitn)
        end if
      else
         call endrun(subname//':: ERROR rafwbf_on = .true. is incompatible with micro_mg_version=1')
      endif

      call MPI_Bcast(rafwbf_on, 1, mpi_logical, mstrid, mpicom, ierr)

      call MPI_Bcast(forestfileWBF,   len(forestfileWBF),    mpi_character,   &
           mstrid, mpicom, ierr)

      if (masterproc) then
         write(iulog ,*) 'Microphysics WBF factor namelist:'
         write(iulog ,*) '  rafwbf_on        = ', rafwbf_on
         if (rafsip_on) then
            write(iulog, *) '  forestfileWBF    = ', trim(forestfileWBF)
         end if
      end if
   end subroutine wbf_readnl

   !------------------------------------------------------------------------+

   subroutine sec_ice_init()
      use mpi,        only: mpi_integer, mpi_real8
      use spmd_utils, only: masterproc, mstrid=>masterprocid, mpicom

      integer :: j_ind, n_ind
      integer :: unitn
      integer :: ierr

      if (.not. rafsip_initialized) then
         !---------------------------------------------------------------------
         ! RaFSIP: INITIALIZE THE RANDOM FOREST PARAMETERS
         !         Initialize on the root processor, then broadcast
         !---------------------------------------------------------------------

         if (masterproc) then
            ! Initialize forestBRHM parameters
            open(newunit=unitn, file=trim(forestfileBRHM), status="old",      &
                 action="read")
            do j_ind = 1, JBT
               read(unitn, *) nrnodes1(j_ind)
               read(unitn, *) (leftchild1(j_ind, n_ind),                      &
                    rightchild1(j_ind, n_ind),                                &
                    out11(j_ind, n_ind), out12(j_ind, n_ind),                 &
                    out13(j_ind, n_ind), thresh1(j_ind, n_ind),               &
                    splitfeat1(j_ind, n_ind), n_ind=1,nrnodes1(j_ind))
            end do
            close(unitn)

            ! Initialize forestBR parameters
            open(newunit=unitn, file=trim(forestfileBR), status="old",        &
                 action="read")
            do j_ind = 1, JBT
               read(unitn, *) nrnodes2(j_ind)
               read(unitn, *) (leftchild2(j_ind, n_ind),                      &
                    rightchild2(j_ind, n_ind),                                &
                    out21(j_ind, n_ind), thresh2(j_ind, n_ind),               &
                    splitfeat2(j_ind, n_ind), n_ind=1,nrnodes2(j_ind))
            end do
            close(unitn)

            ! Initialize forestALL parameters
            open(newunit=unitn, file=trim(forestfileALL), status="old",       &
                 action="read")
            do j_ind = 1, JBT
               read(unitn, *) nrnodes3(j_ind)
               read(unitn, *) (leftchild3(j_ind, n_ind),                      &
                    rightchild3(j_ind, n_ind),                                &
                    out31(j_ind, n_ind), out32(j_ind, n_ind),                 &
                    out33(j_ind, n_ind), out34(j_ind, n_ind),                 &
                    out35(j_ind, n_ind), thresh3(j_ind, n_ind),               &
                    splitfeat3(j_ind, n_ind), n_ind=1,nrnodes3(j_ind))
            end do
            close(unitn)

            ! Initialize forestBRDS parameters
            open(newunit=unitn, file=trim(forestfileBRDS), status="old",      &
                 action="read")
            do j_ind = 1, JBT
               read(unitn, *) nrnodes4(j_ind)
               read(unitn, *) (leftchild4(j_ind, n_ind),                      &
                    rightchild4(j_ind, n_ind),                                &
                    out41(j_ind, n_ind), out42(j_ind, n_ind),                 &
                    out43(j_ind, n_ind), thresh4(j_ind, n_ind),               &
                    splitfeat4(j_ind, n_ind), n_ind=1,nrnodes4(j_ind))
            end do
            close(unitn)

            ! Initialize forestBRwarm parameters
            open(newunit=unitn, file=trim(forestfileBRwarm), status="old",    &
                 action="read")
            do j_ind = 1, JBT
               read(unitn, *) nrnodes5(j_ind)
               read(unitn, *) (leftchild5(j_ind, n_ind),                      &
                    rightchild5(j_ind, n_ind),                                &
                    out51(j_ind, n_ind), thresh5(j_ind, n_ind),               &
                    splitfeat5(j_ind, n_ind), n_ind=1,nrnodes5(j_ind))
            end do
            close(unitn)
         end if ! masterproc

         ! Broadcast all the parameters
         call MPI_Bcast(nrnodes1, JBT, mpi_integer,                           &
           mstrid, mpicom, ierr)
         call MPI_Bcast(leftchild1, JBT*MAX_NODES1, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(rightchild1, JBT*MAX_NODES1, mpi_integer,             &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out11, JBT*MAX_NODES1, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out12, JBT*MAX_NODES1, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out13, JBT*MAX_NODES1, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(thresh1, JBT*MAX_NODES1, mpi_real8,                   &
              mstrid, mpicom, ierr)
         call MPI_Bcast(splitfeat1, JBT*MAX_NODES1, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(nrnodes2, JBT, mpi_integer,                           &
              mstrid, mpicom, ierr)
         call MPI_Bcast(leftchild2, JBT*MAX_NODES2, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(rightchild2, JBT*MAX_NODES2, mpi_integer,             &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out21, JBT*MAX_NODES2, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(thresh2, JBT*MAX_NODES2, mpi_real8,                   &
              mstrid, mpicom, ierr)
         call MPI_Bcast(splitfeat2, JBT*MAX_NODES2, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(nrnodes3, JBT, mpi_integer,                           &
              mstrid, mpicom, ierr)
         call MPI_Bcast(leftchild3, JBT*MAX_NODES3, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(rightchild3, JBT*MAX_NODES3, mpi_integer,             &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out31, JBT*MAX_NODES3, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out32, JBT*MAX_NODES3, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out33, JBT*MAX_NODES3, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out34, JBT*MAX_NODES3, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out35, JBT*MAX_NODES3, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(thresh3, JBT*MAX_NODES3, mpi_real8,                   &
              mstrid, mpicom, ierr)
         call MPI_Bcast(splitfeat3, JBT*MAX_NODES3, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(nrnodes4, JBT, mpi_integer,                           &
              mstrid, mpicom, ierr)
         call MPI_Bcast(leftchild4, JBT*MAX_NODES4, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(rightchild4, JBT*MAX_NODES4, mpi_integer,             &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out41, JBT*MAX_NODES4, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out42, JBT*MAX_NODES4, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out43, JBT*MAX_NODES4, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(thresh4, JBT*MAX_NODES4, mpi_real8,                   &
              mstrid, mpicom, ierr)
         call MPI_Bcast(splitfeat4, JBT*MAX_NODES4, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(nrnodes5, JBT, mpi_integer,                           &
              mstrid, mpicom, ierr)
         call MPI_Bcast(leftchild5, JBT*MAX_NODES5, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(rightchild5, JBT*MAX_NODES5, mpi_integer,             &
              mstrid, mpicom, ierr)
         call MPI_Bcast(out51, JBT*MAX_NODES5, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(thresh5, JBT*MAX_NODES5, mpi_real8,                   &
              mstrid, mpicom, ierr)
         call MPI_Bcast(splitfeat5, JBT*MAX_NODES5, mpi_integer,              &
              mstrid, mpicom, ierr)

         rafsip_initialized = .true.
      end if

   end subroutine sec_ice_init

   !------------------------------------------------------------------------+

   subroutine wbf_init()
      use mpi,        only: mpi_integer, mpi_real8
      use spmd_utils, only: masterproc, mstrid=>masterprocid, mpicom

      integer :: j_ind, n_ind
      integer :: unitn
      integer :: ierr
      character(len=*), parameter :: subname = 'wbf_init'

      if (.not. rafwbf_initialized) then
         !---------------------------------------------------------------------
         ! RaFWBF: INITIALIZE THE RANDOM FOREST PARAMETERS
         !         Initialize on the root processor, then broadcast
         !---------------------------------------------------------------------

         if (masterproc) then
            ! Initialize forestBRHM parameters
            ! Initialize forestALL parameters
            open(newunit=unitn, file=trim(forestfileWBF), status="old",       &
                 action="read")
            do j_ind = 1, JBTB
               read(unitn, *) nrnodesb(j_ind)
               read(unitn, *) (leftchildb(j_ind, n_ind),                      &
                    rightchildb(j_ind, n_ind),                                &
                    outb(j_ind, n_ind), threshb(j_ind, n_ind),               &
                    splitfeatb(j_ind, n_ind), n_ind=1,nrnodesb(j_ind))
            end do

            close(unitn)
              if (any(SPLITFEATB<-1)) then
                call endrun(subname//':: ERROR tree has split feature index invalid')
              end if
         end if ! masterproc

         ! Broadcast
         call MPI_Bcast(nrnodesb, JBTB, mpi_integer,                           &
           mstrid, mpicom, ierr)
         call MPI_Bcast(leftchildb, JBTB*MAX_NODESB, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(rightchildb, JBTB*MAX_NODESB, mpi_integer,             &
              mstrid, mpicom, ierr)
         call MPI_Bcast(outb, JBTB*MAX_NODESB, mpi_real8,                     &
              mstrid, mpicom, ierr)
         call MPI_Bcast(splitfeatb, JBTB*MAX_NODESB, mpi_integer,              &
              mstrid, mpicom, ierr)
         call MPI_Bcast(threshb, JBTB*MAX_NODESB, mpi_real8,              &
              mstrid, mpicom, ierr)

         rafwbf_initialized = .true.
      end if

   end subroutine wbf_init
   !------------------------------------------------------------------------+

   !======================================================================+
   !   THREE SUBROUTINES CALLED BY THE RaFSIP PARAMETERIZATION            !
   !======================================================================+

   !This subroutine is called only when the requirements for the
   !activation of the forestBR model are met (i.e., -25<T<-8 in
   !the absence of rainwater). In this case only the BR process
   !can contribute to the ice production, and hence the RF gives
   !only one prediction: log(IEFBR)
   SUBROUTINE runforest(mdim,max_nodes,jbt,features,ypred1,leftchild,rightchild, &
        & splitfeat,thresh,out1)

      integer,intent(in) :: jbt,mdim,max_nodes
      integer,dimension(jbt,max_nodes),intent(in) :: splitfeat,leftchild,rightchild
      real(r8),dimension(jbt,max_nodes),intent(in)    :: out1,thresh
      real(r8),dimension(mdim),intent(in) :: features
      real(r8), intent(out)   :: ypred1
      integer :: jb,inode,next_node

      ! Initialize variables
      ypred1 = 0._r8

      ! START DOWN FOREST TO CALCULATE THE PREDICTED VALUES
      ! loop over trees in forest
      DO jb=1,jbt

         ! set current node to root node
         inode = 1

         ! loop as long as we reach a leaf node
         do while (leftchild(jb,inode) .ne. rightchild(jb,inode))
            if (features(splitfeat(jb,inode)).le.thresh(jb,inode)) then
               next_node = leftchild(jb,inode)
            else
               next_node = rightchild(jb,inode)
            endif

            inode = next_node

         enddo  !do while

         YPRED1 = YPRED1 + out1(jb,inode)

      ENDDO  !tree loop


      YPRED1 = YPRED1/jbt  !YPRED1=log10(IEFBR)


   end subroutine runforest

   !------------------------------------------------------------------------+

   !This subroutine is called when the requirements for either the forestBRDS
   !or the forestBRHM are met (i.e., -25<T<-8 in the presence of raindrops or
   !-8<T<-3 without raindrops). In this case the RF gives in total 3
   !predictions: log(IEFBR), log(IEFDS) or log(IEFHM), log(QIRSIP) or log(QICSIP)
   SUBROUTINE runforestriv(mdim,max_nodes,jbt,features,ypred1,ypred2,ypred3, &
        & leftchild,rightchild,splitfeat,thresh,out1,out2,out3)

      integer,intent(in) :: jbt,mdim,max_nodes
      integer,dimension(jbt,max_nodes),intent(in) :: splitfeat,leftchild,rightchild
      real(r8),dimension(jbt,max_nodes),intent(in)    :: out1,out2,out3,thresh
      real(r8),dimension(mdim),intent(in) :: features
      real(r8), intent(out)   :: ypred1,ypred2,ypred3
      integer :: jb,inode,next_node

      ! Initialize variables
      ypred1 = 0._r8
      ypred2 = 0._r8
      ypred3 = 0._r8

      ! START DOWN FOREST TO CALCULATE THE PREDICTED VALUES
      ! loop over trees in forest
      DO jb=1,jbt

         ! set current node to root node
         inode = 1

         ! loop as long as we reach a leaf node
         do while (leftchild(jb,inode) .ne. rightchild(jb,inode))
            if (features(splitfeat(jb,inode)).le.thresh(jb,inode)) then
               next_node = leftchild(jb,inode)
            else
               next_node = rightchild(jb,inode)
            endif

            inode = next_node

         enddo  !do while

         YPRED1 = YPRED1 + out1(jb,inode)
         YPRED2 = YPRED2 + out2(jb,inode)
         YPRED3 = YPRED3 + out3(jb,inode)

      ENDDO  !tree loop


      YPRED1 = YPRED1/jbt  !YPRED1=log10(IEFBR)
      YPRED2 = YPRED2/jbt  !YPRED2=log10(IEFDS) or log10(IEFHM)
      YPRED3 = YPRED3/jbt  !YPRED3=log10(QIRSIP) or log10(QICSIP)


   end subroutine runforestriv

   !------------------------------------------------------------------------+

   !This subroutine is called when the requirements for the forestALL are met
   !(i.e., -8<T<-3 in the presence of raindrops). In this case the RF gives 5
   !predictions: log(IEFBR), log(IEFHM), log(IEFDS), log(QICSIP), log(QIRSIP)
   SUBROUTINE runforestmulti(mdim,max_nodes,jbt,features,ypred1,ypred2,ypred3,ypred4,ypred5, &
        & leftchild,rightchild,splitfeat,thresh,out1,out2,out3,out4,out5)

      integer,intent(in) :: jbt,mdim,max_nodes
      integer,dimension(jbt,max_nodes),intent(in) :: splitfeat,leftchild,rightchild
      real(r8),dimension(jbt,max_nodes),intent(in)    :: out1,out2,out3,out4,out5,thresh
      real(r8),dimension(mdim),intent(in) :: features
      real(r8), intent(out)   :: ypred1,ypred2,ypred3,ypred4,ypred5
      integer :: jb,inode,next_node

      ! Initialize variables
      ypred1 = 0._r8
      ypred2 = 0._r8
      ypred3 = 0._r8
      ypred4 = 0._r8
      ypred5 = 0._r8

      ! START DOWN FOREST TO CALCULATE THE PREDICTED VALUES
      ! loop over trees in forest
      DO jb=1,jbt

         ! set current node to root node
         inode = 1

         ! loop as long as we reach a leaf node
         do while (leftchild(jb,inode) .ne. rightchild(jb,inode))
            if (features(splitfeat(jb,inode)).le.thresh(jb,inode)) then
               next_node = leftchild(jb,inode)
            else
               next_node = rightchild(jb,inode)
            endif

            inode = next_node

         enddo  !do while

         YPRED1 = YPRED1 + out1(jb,inode)
         YPRED2 = YPRED2 + out2(jb,inode)
         YPRED3 = YPRED3 + out3(jb,inode)
         YPRED4 = YPRED4 + out4(jb,inode)
         YPRED5 = YPRED5 + out5(jb,inode)

      ENDDO  !tree loop


      YPRED1 = YPRED1/jbt  !YPRED1=log10(IEFBR)
      YPRED2 = YPRED2/jbt  !YPRED2=log10(IEFHM)
      YPRED3 = YPRED3/jbt  !YPRED3=log10(IEFDS)
      YPRED4 = YPRED4/jbt  !YPRED4=log10(QICSIP)
      YPRED5 = YPRED5/jbt  !YPRED5=log10(QIRSIP)


   end subroutine runforestmulti

end module module_random_forests
