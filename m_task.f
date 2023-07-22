C     TASK UTILITIES
      MODULE M_TASK
      PUBLIC T_TASK, TASK_PRINT, TASKS_STORE, TASKS_LOAD, LIST_TASKS

C     TYPE DEFINITION OF T_TASK
      TYPE :: T_TASK
         INTEGER :: ID
         CHARACTER (LEN=64) :: TITLE, COMMENT
      END TYPE

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

C     PRINT TASK INSTANCE
      SUBROUTINE task_print(task)
      IMPLICIT NONE
      
      type(t_task), INTENT(IN) :: task
      CHARACTER(len=14) :: fmt

      WRITE (*,10) TASK%ID, TASK%TITLE, TASK%COMMENT
 10   FORMAT(I4T6AT32AT32)
      END SUBROUTINE


C     INPUT NEW TASK
      FUNCTION NEW_TASK(LAST_ID) result(TASK)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: LAST_ID
      INTEGER :: NEW_ID
      type(t_task) :: TASK
      
      CHARACTER(len=64) :: TITLE, COMMENT

      WRITE (*,'(A/A)', ADVANCE="NO") 'CREATE NEW TASK', 'TITLE:   '

      NEW_ID = LAST_ID + 1
      READ (*,*) TITLE
      WRITE (*,'(A)', ADVANCE="NO") 'COMMENT: '
      READ (*,*) COMMENT

      TASK%ID = NEW_ID
      TASK%TITLE = TITLE
      TASK%COMMENT = COMMENT

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
      WRITE (IO,101) COUNT
      DO i = 1, COUNT
         WRITE (io,100) TASKS(i)%ID, TASKS(i)%TITLE, TASKS(i)%COMMENT
      END DO
      CLOSE(IO)
      
      END SUBROUTINE TASKS_STORE

      FUNCTION TASKS_LOAD(TASKS) RESULT(COUNT)
      IMPLICIT NONE

      TYPE(T_TASK), ALLOCATABLE :: TASKS(:)
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
            READ(IO,200) TASKS(i)%ID, TASKS(i)%TITLE, TASKS(i)%COMMENT
         END DO
         
         CLOSE(IO)

      ELSE
         ALLOCATE(TASKS(0))
         COUNT = 0
      END IF
      
      END FUNCTION TASKS_LOAD
      
      END MODULE M_TASK
