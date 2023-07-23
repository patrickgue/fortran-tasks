      PROGRAM P_TASK
      USE M_TASK
      IMPLICIT NONE

      INTEGER :: COUNT, INP, I, J
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

         IF (INP .EQ. 1) THEN
            CALL LIST_TASKS(TASK, COUNT)
         ELSE IF (INP .eq. 2) THEN
C     ;;;;; REALLOCATE TASK
            ALLOCATE(TEMP(COUNT))
            DO i = 1, COUNT
               TEMP(i) = TASK(i)
            END DO
            DEALLOCATE(TASK)
            ALLOCATE(TASK(COUNT + 1))
            do i = 1, COUNT
               TASK(I) = TEMP(I)
            END do
            DEALLOCATE(TEMP)
            TASK(COUNT + 1) = NEW_TASK(COUNT)
            COUNT = COUNT + 1
            CALL TASKS_STORE(TASK, COUNT)
         ELSE IF (INP .eq. 3) THEN
 100        FORMAT(A)
            WRITE (*,100) 'ID of Task to be deleted (-1 to cancel)'
            WRITE (*,100,ADVANCE="NO") '# '
            READ (*,*) INP
            IF (INP .GE. 1 .AND. INP .LE. COUNT) THEN
               ALLOCATE(TEMP(COUNT))
               DO I = 1, COUNT
                  TEMP(I) = TASK(I)
               END DO
               DEALLOCATE(TASK)
               ALLOCATE(TASK(COUNT - 1))
               J = 0
               DO I = 1, COUNT
                  IF (.NOT. TEMP(I)%ID .EQ. INP) THEN
                     TASK(J) = TEMP(I)
                     J = J + 1
                  END IF
               END DO
               DEALLOCATE(TEMP)
               COUNT = COUNT - 1
               CALL TASKS_STORE(TASK, COUNT)
            ELSE
 101           FORMAT(AI2A3I3)
               WRITE(*,101) 'Input is outside range', 0, '-', COUNT
            END IF
         ELSE IF (INP .eq. 4) THEN
            LOOP = .FALSE.
         END IF
         
      END DO
      END PROGRAM P_TASK

      


