      program p_task
      use m_task
      implicit none

      integer :: count, inp
      logical :: loop
      type(t_task), dimension(10) :: task

      loop = .true.
      count = 0

                                     
      write (*,'(/A/)') '                                     TASKS'
      write (*,'(A/)') '(c) 2023 Tasks Developers'

      


      do while (loop)

         write (*,'(/A)') '1) LIST  2) CREATE  3) DELETE  4) QUIT'
         write (*,'(A)', advance="no") '> '
         read (*,*) inp

         if (inp .eq. 1) then
            call list_tasks(task, count)
         else if (inp .eq. 2) then
            task(count + 1) = new_task(count)
            count = count + 1
         else if (inp .eq. 3) then
            write (*,'(A)') '[Not yet implemented]'
         else if (inp .eq. 4) then
            loop = .false.
         end if
         
      end do
      end program p_task

      

      subroutine list_tasks(task, count)
      use m_task
      implicit none

      type(t_task), dimension(10), intent(in) :: task
      integer, intent(in) :: count
      integer :: i

      do i = 1, count
         call task_print(task(i))
      end do
      

      end subroutine

