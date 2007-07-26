!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

 module glc_ErrorMod

!BOP
! !MODULE: glc_ErrorMod
! !DESCRIPTION:
!  This module contains glc error flags and facilities for logging and
!  printing error messages.  Note that error flags are local to a 
!  process and there is no synchronization of error flags across 
!  processes.  As routines trap error flags, they may add a message
!  to the error log to aid in tracking the call sequence.
!
! !USERDOC:
!  Users should not need to change any values in this module.
!
! !REFDOC:
!  All routines in glc which encounter an error should return to
!  the calling routine with the glc\_Fail error code set and a message
!  added to the error log using the glc\_ErrorSet function.  Also,
!  routines in glc should check error codes returned by called routines
!  and add a message to the error log to help users track the calling
!  sequence that generated the error.  This process
!  enables the error code to be propagated to the highest level or
!  to a coupler for a proper call to the glc finalize method.
!
! !REVISION HISTORY:
!  SVN:$Id: POP_ErrorMod.F90 808 2006-04-28 17:06:38Z njn01 $
!  glc version copied from POP_ErrorMod.F90
!
! !USES:

   use glc_KindsMod
   !use glc_CommMod
   use glc_communicate
   use glc_constants
   use glc_IOUnitsMod

   implicit none
   private
   save

! !DEFINED PARAMETERS:

   integer (glc_i4), parameter, public :: &
      glc_Success =  0,           & ! standard glc error flags
      glc_Fail    = -1

! !PUBLIC MEMBER FUNCTIONS:

   public :: glc_ErrorSet, &
             glc_ErrorPrint

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  module variables
!
!-----------------------------------------------------------------------

   integer (glc_i4), parameter :: &
      glc_ErrorLogDepth = 20   ! Max depth of call tree to properly
                               ! size the error log array

   integer (glc_i4) ::         &
      glc_ErrorMsgCount =  0   ! tracks current number of log messages

   character (glc_CharLength), dimension(glc_ErrorLogDepth) :: &
      glc_ErrorLog             ! list of error messages to be output

!EOC
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: glc_ErrorSet -- sets error code and logs error message
! !INTERFACE:

 subroutine glc_ErrorSet(ErrorCode, ErrorMsg)

! !DESCRIPTION:
!  This routine sets an error code to glc\_Fail and adds a message to 
!  the error log for later printing.
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  same as module

! !OUTPUT PARAMETERS:

   integer (glc_i4), intent(out) :: &
      ErrorCode              ! Error code to set to fail

! !INPUT PARAMETERS:

   character (*), intent(in) :: &
      ErrorMsg               ! message to add to error log for printing

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  Set error code to fail
!
!-----------------------------------------------------------------------

   ErrorCode = glc_Fail

!-----------------------------------------------------------------------
!
!  Add error message to error log
!
!-----------------------------------------------------------------------

   glc_ErrorMsgCount = glc_ErrorMsgCount + 1

   if (glc_ErrorMsgCount <= glc_ErrorLogDepth) then
      glc_ErrorLog(glc_ErrorMsgCount) = ErrorMsg
   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine glc_ErrorSet

!***********************************************************************
!BOP
! !IROUTINE: glc_ErrorPrint -- prints the error log
! !INTERFACE:

 subroutine glc_ErrorPrint(ErrorCode, PrintTask)

! !DESCRIPTION:
!  This routine prints all messages in the error log.  If a PrintTask
!  is specified, only the log on that task will be printed.
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  same as module

! !INPUT PARAMETERS:

   integer (glc_i4), intent(in) :: &
      ErrorCode              ! input error code to check success/fail

   integer (glc_i4), intent(in), optional :: &
      PrintTask              ! Task from which to print error log

!EOP
!BOC
!-----------------------------------------------------------------------
!
!  local variables
!
!-----------------------------------------------------------------------

   integer (glc_i4) :: n

!-----------------------------------------------------------------------
!
!  Print all error messages to stdout
!
!-----------------------------------------------------------------------

   if (present(PrintTask)) then

      if (my_Task == PrintTask) then
      !if (glc_myTask == PrintTask) then

         write(glc_stdout,blank_fmt)
         write(glc_stdout,'(a34)') '----------------------------------'

         if (glc_ErrorMsgCount == 0) then ! no errors

            write(glc_stdout,'(a34)') &
                                'Successful completion of glc model'

         else

            write(glc_stdout,'(a14)') 'glc Exiting...'
            do n=1,min(glc_ErrorMsgCount,glc_ErrorLogDepth)
               write(glc_stderr,'(a)') trim(glc_ErrorLog(n))
               if (glc_stdout /= glc_stderr) then
                  write(glc_stdout,'(a)') trim(glc_ErrorLog(n))
               endif
            end do
            if (glc_ErrorMsgCount > glc_ErrorLogDepth) then
               write(glc_stderr,'(a)') 'Too many error messages'
               if (glc_stdout /= glc_stderr) then
                  write(glc_stdout,'(a)') 'Too many error messages'
               endif
            endif

         endif

         write(glc_stdout,'(a34)') '----------------------------------'

      endif

   else

      write(glc_stdout,'(a34)') '----------------------------------'

      if (glc_ErrorMsgCount == 0) then ! no errors

         write(glc_stdout,'(a34)') 'Successful completion of glc model'

      else

         write(glc_stdout,'(a14)') 'glc Exiting...'
         do n=1,min(glc_ErrorMsgCount,glc_ErrorLogDepth)
            write(glc_stderr,'(a)') trim(glc_ErrorLog(n))
            if (glc_stdout /= glc_stderr) then
               write(glc_stdout,'(a)') trim(glc_ErrorLog(n))
            endif
         end do
         if (glc_ErrorMsgCount > glc_ErrorLogDepth) then
            write(glc_stderr,'(a)') 'Too many error messages'
            if (glc_stdout /= glc_stderr) then
               write(glc_stdout,'(a)') 'Too many error messages'
            endif
         endif

      endif

      write(glc_stdout,'(a34)') '----------------------------------'

   endif

!-----------------------------------------------------------------------
!EOC

 end subroutine glc_ErrorPrint

!***********************************************************************

 end module glc_ErrorMod

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
