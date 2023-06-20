c     Task utilities
      module m_task
      public t_task, task_print
      
c     Type definition of t_task
      type :: t_task
      integer :: id
      character (len=64) :: title, comment
      end type

      contains

c     Print task instance
      subroutine task_print(task)
      implicit none
      
      type(t_task), intent(in) :: task
      character(len=14) :: fmt

      fmt = '(I4T6AT32AT32)'
      
      write (*,fmt) task%id, task%title, task%comment
      
      end subroutine


c     Input new Task
      function new_task(last_id) result(task)
      implicit none

      integer, intent(in) :: last_id
      integer :: new_id
      type(t_task) :: task
      
      character(len=64) :: title, comment

      write (*,'(A/A)') 'Create New Task', 'Title:'

      new_id = last_id + 1
      read (*,*) title
      write (*,'(A)') 'Comment:'
      read (*,*) comment

      task%id = new_id
      task%title = title
      task%comment = comment

      end function new_task

      
      end module m_task
