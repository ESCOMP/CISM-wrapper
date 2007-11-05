!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_IOUnitsMod

!BOP
!
! !MODULE:  glc_IOUnitsMod
!
! !DESCRIPTION:
!  This module contains an I/O unit manager for tracking, assigning
!  and reserving I/O unit numbers.
!
! !USERDOC:
!  There are three reserved I/O units set as parameters in this
!  module.  The default units for standard input (stdin), standard 
!  output (stdout) and standard error (stderr).  These are currently 
!  set as units 5,6,6, respectively as that is the most commonly 
!  used among vendors. However, the user may change these if those 
!  default units are conflicting with other models or if the
!  vendor is using different values. 
!
!  The maximum number of I/O units per node is currently set by
!  the parameter glc\_IOMaxUnits.
!
! !REFDOC:
!
! !REVISION HISTORY:
!  SVN:$Id: POP_IOUnitsMod.F90 808 2006-04-28 17:06:38Z njn01 $
!  WHL, May 2007: Adapted from POP_IOUnitsMod.F90 in POP 2.0

! !USES:

   use glc_kinds_mod

   implicit none
   private
   save

! !PUBLIC MEMBER FUNCTIONS:

   public :: glc_IOUnitsGet,                &
             glc_IOUnitsRelease,            &
             glc_IOUnitsReserve

! !PUBLIC DATA MEMBERS:

   integer (i4), parameter, public :: &
      glc_stdin  =  5,  &! reserved unit for standard input
      glc_stdout =  6,  &! reserved unit for standard output
      glc_stderr =  6    ! reserved unit for standard error

   ! common formats for writing to stdout, stderr

   character (9), parameter, public :: &
      glc_delimFormat = "(72('-'))"

   character (5), parameter, public :: &
      glc_blankFormat = "(' ')" 

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  io unit manager variables
!
!-----------------------------------------------------------------------

   integer (i4), parameter :: &
      glc_IOUnitsMinUnits = 11,   & ! do not use unit numbers below this
      glc_IOUnitsMaxUnits = 99      ! maximum number of open units

   logical :: &
      glc_IOUnitsInitialized = .false.

   logical, dimension(glc_IOUnitsMaxUnits) :: &
      glc_IOUnitsInUse       ! flag=.true. if unit currently open

!EOC
!***********************************************************************

contains

!***********************************************************************
!BOP
! !IROUTINE: glc_IOUnitsGet
! !INTERFACE:

 subroutine glc_IOUnitsGet(iunit)

! !DESCRIPTION:
!  This routine returns the next available i/o unit and marks it as
!  in use to prevent any later use.
!  Note that {\em all} processors must call this routine even if only
!  the master task is doing the i/o.  This is necessary insure that
!  the units remains synchronized for other parallel I/O functions.
!
! !REVISION HISTORY:
!  same as module

! !OUTPUT PARAMETERS:

   integer (i4), intent(out) :: &
      iunit                     ! next free i/o unit

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (i4) :: n  ! dummy loop index

   logical  :: alreadyInUse

!-----------------------------------------------------------------------
!
!  check to see if units initialized and initialize if necessary
!
!-----------------------------------------------------------------------

   if (.not. glc_IOUnitsInitialized) then
      glc_IOUnitsInUse = .false.
      glc_IOUnitsInUse(glc_stdin) = .true.
      glc_IOUnitsInUse(glc_stdout) = .true.
      glc_IOUnitsInUse(glc_stderr) = .true.

      glc_IOUnitsInitialized = .true.
   endif

!-----------------------------------------------------------------------
!
!  find next free unit
!
!-----------------------------------------------------------------------

   srch_units: do n=glc_IOUnitsMinUnits, glc_IOUnitsMaxUnits
      if (.not. glc_IOUnitsInUse(n)) then   ! I found one, I found one

         !*** make sure not in use by library routines
         INQUIRE (unit=n,OPENED=alreadyInUse)

         if (.not. alreadyInUse) then         
            iunit = n
            glc_IOUnitsInUse(iunit) = .true.  ! mark iunit as being in use
            exit srch_units
         endif
      endif
   end do srch_units

   if (iunit > glc_IOUnitsMaxUnits) stop 'glc_IOUnitsGet: No free units'

!-----------------------------------------------------------------------
!EOC

 end subroutine glc_IOUnitsGet

!***********************************************************************
!BOP
! !IROUTINE: glc_IOUnitsRelease
! !INTERFACE:

 subroutine glc_IOUnitsRelease(iunit)

! !DESCRIPTION:
!  This routine releases an i/o unit (marks it as available).
!  Note that {\em all} processors must call this routine even if only
!  the master task is doing the i/o.  This is necessary insure that
!  the units remain synchronized for other parallel I/O functions.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETER:

   integer (i4), intent(in) :: &
      iunit                    ! i/o unit to be released

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  check for proper unit number
!
!-----------------------------------------------------------------------

   if (iunit < 1 .or. iunit > glc_IOUnitsMaxUnits) then
      stop 'glc_IOUnitsRelease: bad unit'
   endif

!-----------------------------------------------------------------------
!
!  mark the unit as not in use
!
!-----------------------------------------------------------------------

   glc_IOUnitsInUse(iunit) = .false.  !  that was easy...

!-----------------------------------------------------------------------
!EOC

 end subroutine glc_IOUnitsRelease

!***********************************************************************
!BOP
! !IROUTINE: glc_IOUnitsReserve
! !INTERFACE:

 subroutine glc_IOUnitsReserve(iunit)

! !DESCRIPTION:
!  This routine releases an i/o unit (marks it as available).
!  Note that {\em all} processors must call this routine even if only
!  the master task is doing the i/o.  This is necessary insure that
!  the units remains synchronized for other parallel I/O functions.
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETER:

   integer (i4), intent(in) :: &
      iunit                    ! i/o unit to be released

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   logical :: alreadyInUse

!-----------------------------------------------------------------------
!
!  check for proper unit number
!
!-----------------------------------------------------------------------

   if (iunit < glc_IOUnitsMinUnits .or. iunit > glc_IOUnitsMaxUnits) then
      stop 'glc_IOUnitsReserve: invalid unit'
   endif

!-----------------------------------------------------------------------
!
!  check to see if glc already using this unit
!
!-----------------------------------------------------------------------

   if (glc_IOUnitsInUse(iunit)) then
      stop 'glc_IOUnitsReserve: unit already in use by glc'
   endif

!-----------------------------------------------------------------------
!
!  check to see if others already using this unit
!
!-----------------------------------------------------------------------

   INQUIRE (unit=iunit, OPENED=alreadyInUse)
   if (alreadyInUse) then
      stop 'glc_IOUnitsReserve: unit already in use by others'
   endif

!-----------------------------------------------------------------------
!
!  mark the unit as in use
!
!-----------------------------------------------------------------------

   glc_IOUnitsInUse(iunit) = .true.  !  that was easy...

!-----------------------------------------------------------------------
!EOC

 end subroutine glc_IOUnitsReserve

!***********************************************************************

 end module glc_IOUnitsMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
