C     TASK UTILITIES
      MODULE M_TASK
      PUBLIC T_TASK, TASK_PRINT, TASKS_STORE, TASKS_LOAD, LIST_TASKS

C     TYPE DEFINition OF T_TASK
      TYPE :: T_TASK
          INTEGER :: ID
          CHARACTER (LEN=64) :: TITLE, COMMENT
      END TYPE
c     Type definition of t_task

      CONTAINS

      SUBROUTINE LIST_TASKS(TASK, COUNT)
      IMPLICIT NONE

      TYPE(T_TASK), INTENT(IN) :: TASK(:)
      INTEGER, INTENT(in) :: COUNT
      INTEGER :: I

      DO I = 1, COUNT
         CALL TASK_PRINT(TASK(I))
      END DO
      END SUBROUTINE

C     Print task instance
      SUBROUTINE task_print(task)
      IMPLICIT NONE
      
      type(t_task), INTENT(IN) :: task
      CHARACTER(len=14) :: fmt

      WRITE (*,10) TASK%id, TASK%title, TASK%comment
 10   FORMAT(I4T6AT32AT32)
      END SUBROUTINE


C     INPUT NEW TASK
      FUNCTION NEW_TASK(LAST_ID) result(TASK)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: last_id
      INTEGER :: NEW_ID
      type(t_task) :: TASK
      
      CHARACTER(len=64) :: title, comment

      WRITE (*,'(A/A)', advance="no") 'Create New Task', 'Title:   '

      new_id = last_id + 1
      READ (*,*) title
      WRITE (*,'(A)', advance="no") 'Comment: '
      READ (*,*) comment

      TASK%id = new_id
      TASK%title = title
      TASK%comment = comment

      END FUNCTION new_task

C     STORE TASKS
      SUBROUTINE TASKS_STORE(TASKS, COUNT)
      IMPLICIT NONE

      type(t_task), ALLOCATABLE, INTENT(IN) :: TASKS(:)
      INTEGER, INTENT(IN) :: COUNT
      INTEGER :: io, i

      OPEN(NEWUNIT=IO, FILE="tasks.csv")
 100  FORMAT(I4T6AT32AT32)
 101  FORMAT(I1)
      WRITE (io,101) COUNT
      do i = 1, COUNT
         WRITE (io,100) TASKS(i)%id, TASKS(i)%title, TASKS(i)%comment
      END do
      CLOSE(io)
      
      END SUBROUTINE TASKS_STORE

      FUNCTION TASKS_LOAD(TASKS) result(COUNT)
      IMPLICIT NONE

      TYPE(t_task), ALLOCATABLE :: TASKS(:)
      INTEGER :: IO, I, COUNT
      LOGICAL :: EXISTS

 200  FORMAT(I4T6AT32AT32)
 201  FORMAT(I1)

      INQUIRE(FILE="tasks.csv", EXIST=EXISTS)

      IF (EXISTS) THEN
         OPEN(NEWUNIT=IO, FILE="tasks.csv", ACTION="READ")
         READ (IO,201) COUNT
         ALLOCATE(TASKS(COUNT))
         DO I = 1, COUNT
            READ(IO,200) TASKS(i)%id, TASKS(i)%title, TASKS(i)%comment
         END DO
         
         CLOSE(IO)

      ELSE
         allocate(TASKS(0))
         COUNT = 0
      END IF
      
      END FUNCTION TASKS_LOAD
      
      END MODULE M_TASK
