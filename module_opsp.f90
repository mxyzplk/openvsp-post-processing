MODULE MODULE_OVSPPP
    
    TYPE CONFIG
        INTEGER :: ID
        INTEGER :: MTYPE
        CHARACTER(LEN=8) :: NAME
    END TYPE

    
    TYPE MESH
        REAL*8, ALLOCATABLE :: G(:,:)
        INTEGER :: NG, NGI
        INTEGER, ALLOCATABLE :: CHECK(:)
    END TYPE
    
    
    TYPE CONNECTIVITY
        INTEGER, ALLOCATABLE :: E(:,:)
        REAL*8, ALLOCATABLE :: AREA(:,:)
        INTEGER, ALLOCATABLE :: CHECK(:)
        INTEGER :: NE, NEI
        INTEGER :: ETYPE
    END TYPE
    
    
    CONTAINS
    
    SUBROUTINE SET_CONFIG(C)
    
        TYPE(CONFIG), INTENT(INOUT) :: C
    
        OPEN(UNIT=11, FILE='config.txt', STATUS='OLD')
        
        READ(11,*) C%ID
        READ(11,*) C%MTYPE
        READ(11,'(A8)') C%NAME
        
        CLOSE(11)
    
    END SUBROUTINE
    
    
    
    SUBROUTINE READ_DATA(C, M, CN)
    
        TYPE(CONFIG),INTENT(IN) :: C
        TYPE(MESH),INTENT(INOUT) :: M
        TYPE(CONNECTIVITY),INTENT(INOUT) :: CN    
    
        INTEGER :: I, J, K, IDUMMY, ID
        REAL*8 :: DPRESS, AOA, BETA, MACH, P
        CHARACTER(LEN=20) :: FILENAME
        CHARACTER(LEN=3) :: FID
        
        OPEN(UNIT=91, FILE='input.txt', STATUS='OLD')
        OPEN(UNIT=20, FILE='connectivity.txt', STATUS='UNKNOWN')
        OPEN(UNIT=21, FILE='grids.txt', STATUS='UNKNOWN')
        OPEN(UNIT=222, FILE='dpress_report.txt', STATUS='UNKNOWN')
        
        WRITE(222,'("Mach     AoA      Beta     P        Filename")')
        
        READ(91,'(14X,I8)')M%NG
        READ(91,'(13X,I8)')CN%NE
        READ(91,*)
        READ(91,*)
        
        ALLOCATE(M%CHECK(M%NG))
        ALLOCATE(M%G(M%NG,3))
        
        M%CHECK = 0

        ALLOCATE(CN%CHECK(CN%NE))
        ALLOCATE(CN%AREA(CN%NE,4))
        
        CN%CHECK = 0
        
        IF (C%MTYPE == 0) THEN ! TRI
            CN%ETYPE = 3
            ALLOCATE(CN%E(CN%NE,3))
        ELSE IF (C%MTYPE == 1) THEN ! QUAD
            CN%ETYPE = 4
            ALLOCATE(CN%E(CN%NE,4))
        END IF
    
        ! READING GRIDS
        DO I=1, M%NG
            READ(91,*)(M%G(I,J), J=1,3)
        END DO
        
        READ(91,*)
        READ(91,*)
        
        CN%NEI = 0
        ! READING ELEMENTS
        DO I=1, CN%NE
            READ(91,*)(CN%E(I,J), J=1,CN%ETYPE), IDUMMY, ID, CN%AREA(I,4), CN%AREA(I,1), CN%AREA(I,2), CN%AREA(I,3)
            IF (ID == C%ID) THEN
                CN%CHECK(I) = 1
                CN%NEI = CN%NEI + 1
            END IF
        END DO
        
        M%NGI = 0
        WRITE(20,'(I8)')CN%NEI
        DO I = 1, CN%NE
            IF (CN%CHECK(I) == 1) THEN
                IF (CN%ETYPE == 4) THEN
                    WRITE(20,'(4I8,4F16.8)')(CN%E(I,J), J=1,CN%ETYPE), CN%AREA(I,4), CN%AREA(I,1), CN%AREA(I,2), CN%AREA(I,3)
                ELSE IF (CN%ETYPE == 3) THEN
                    WRITE(20,'(3I8,4F16.8)')(CN%E(I,J), J=1,CN%ETYPE), CN%AREA(I,4), CN%AREA(I,1), CN%AREA(I,2), CN%AREA(I,3)
                END IF
                DO J = 1,CN%ETYPE
                    IF (M%CHECK(CN%E(I,J)) == 0) THEN
                        M%NGI = M%NGI + 1
                        M%CHECK(CN%E(I,J)) = 1
                    END IF
                END DO
            END IF
        END DO
        
        WRITE(21,'(I8)')M%NGI
        
        DO I = 1, M%NG
            IF (M%CHECK(I) == 1) THEN
                WRITE(21,'(I8,3F12.6)')I, (M%G(I,J), J=1,3)
            END IF
        END DO
        
        K = 0
        DO
            K = K + 1
            READ(91,*,END=200)
            READ(91,*)
            READ(91,*)
            READ(91,*)
            READ(91,*)
            READ(91,*)
            READ(91,*)
            READ(91,*)
            READ(91,*)
            READ(91,*)
            READ(91,'(21X,F12.0)')MACH
            READ(91,'(21X,F12.0)')AOA
            READ(91,'(21X,F12.0)')BETA
            READ(91,*)
            READ(91,*)
            READ(91,'(21X,F12.0)')P
            READ(91,*)
            READ(91,*)
            READ(91,*)
            READ(91,*)
            READ(91,*)
            WRITE(FID,'(I3)')K
            FILENAME = "dpress_" // TRIM(ADJUSTL(FID)) // ".txt"
            OPEN(UNIT=22, FILE=FILENAME, STATUS='UNKNOWN')
            WRITE(222,'(4(F8.3,1X),A20)')MACH, AOA, BETA, P, ADJUSTL(FILENAME)

            DO I=1, CN%NE
                READ(91,*)IDUMMY, DPRESS
                IF (CN%CHECK(I) == 1) THEN
                    WRITE(22,'(I8,F12.6)')I, DPRESS
                END IF
            END DO
            CLOSE(22)
        END DO
        
        200     CONTINUE
        
        CLOSE(91)
        CLOSE(20)
        CLOSE(21)
        
    END SUBROUTINE
    
    
END MODULE