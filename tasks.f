      PROGRAM P_TASK
      USE M_TASK
      IMPLICIT NONE

      INTEGER :: COUNT, INP, I
      LOGICAL :: LOOP
      TYPE(T_TASK), ALLOCATABLE :: TASK(:)
      TYPE(T_TASK), ALLOCATABLE :: TEMP(:)

      LOOP = .TRUE.
      COUNT = 0
      COUNT = TASKS_LOAD(TASK)
      
      WRITE (*,'(/A/)') '                                     TASKS'
      WRITE (*,'(A/)') '(C) 2023 Tasks Developers'

      


      DO WHILE (LOOP)
         WRITE (*,'(/A)') '1) LIST  2) CREATE  3) DELETE  4) QUIT'
         WRITE (*,'(A)', ADVANCE="NO") '> '
         READ (*,*) INP

         IF (INP .eq. 1) THEN
            CALL LIST_TASKS(TASK, COUNT)
         ELSE IF (INP .eq. 2) THEN
C     ;;;;; REALLOCATE TASK
            ALLOCATE(TEMP(COUNT))
            DO i = 1, COUNT
               TEMP(i) = TASK(i)
            END DO
            DEALLOCATE(task)
            ALLOCATE(TASK(COUNT + 1))
            do i = 1, COUNT
               TASK(i) = TEMP(i)
            END do
            DEALLOCATE(TEMP)
            TASK(COUNT + 1) = NEW_TASK(COUNT)
            COUNT = COUNT + 1
            CALL TASKS_STORE(TASK, COUNT)
         ELSE IF (INP .eq. 3) THEN
            WRITE (*,'(A)') '[Not yet implemented]'
         ELSE IF (INP .eq. 4) THEN
            LOOP = .FALSE.
         END IF
         
      END DO
      END PROGRAM P_TASK

      


