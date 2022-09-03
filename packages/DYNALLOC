//DYNALLOC  JOB (TSO),
//             'Install DYNALLOC',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*
//*********************************************************************
//* This jobstream assembles DYNALLOC and compiles the BREAKDSN sub-  *
//* routine it calls into a single load module that may be called by  *
//* a COBOL main program to dynamically allocate datasets at run time.*
//*********************************************************************
//*
//DYNALLOC EXEC ASMFC,PARM.ASM='LOAD,NODECK,LIST,NOXREF'
//ASM.SYSGO DD DSN=&&LOADSET,UNIT=SYSDA,SPACE=(80,(200,50)),
//             DISP=(MOD,PASS)
//ASM.SYSIN DD *
         PRINT ON,NOGEN
         SPACE 3
*        THIS PROGRAM IS A SUBROUTINE WHICH DYNAMICALLY ALLOCATES
*        THE FOLLOWING TYPES OF DATASETS TO THE DDNAME GIVEN:
*
*            .  GOOD OLE OS SEQUENTIAL DATASET
*            .  A PARTITIONED DATASET WITH OR WITHOUT MEMBER NAME
*            .  A GDG EXPRESSED WITH ABSOLUTE OR RELATIVE GEN NUMBER
*            .  A VSAM FILE
*
*        ALSO ALIASES OF THESE TYPES OF DATASETS ARE ACCEPTABLE
*
*        TO CALL THIS PROGRAM FROM COBOL, DO THE FOLLOWING:
*
*           1.  PLACE THE FOLLOWING AREA IN WORKING STORAGE:
*
*                  01  DYNALLOC-I-O-AREA.
*                      05  DDNAME                    PIC X(8).
*                      05  DSNAME                    PIC X(44).
*
*           2.  PLACE THE FOLLOWING IN THE PROCEDURE DIVISION:
*
*                  MOVE YOUR DDNAME
*                    TO DDNAME OF DYNALLOC-I-O-AREA.
*                  MOVE YOUR DATASET NAME
*                    TO DSNAME OF DYNALLOC-I-O-AREA.
*
*                 CALL 'DYNALLOC' USING DYNALLOC-I-O-AREA.
*
*           3.  AFTER THE CALL, EXAMINE RETURN-CODE
*
*                 RETURN CODE VALUE      MEANING
*                 _________________      _____________________________
*
*                       0                DYNAMIC ALLOC WAS SUCCESSFUL
*
*                       4                UNABLE TO FREE THE DDNAME
*
*                       8                UNSUCCESSFUL DYNAMIC ALLOC
*
*                      12                DATASET NOT CATALOGUED
*
*                      16                INVALID DSNAME GIVEN
*
*
*
*
         EJECT
         SPACE 3
*
**********************************************
*                                            *
*        I N I T I A L I Z A T I O N         *
*                                            *
**********************************************
*
DYNALLOC CSECT
         SAVE  (14,12)
         BALR  12,0
         USING *,12
         ST    13,SAVEAREA+4
         LA    13,SAVEAREA
         B     STARTIT
         SPACE
SAVEAREA DC    18F'0'
         SPACE 3
********************************************************************
*        EQUATE RESISTERS 0 THRU 15 TO MORE MEANINGFUL NAMES       *
********************************************************************
         SPACE 3
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         EJECT
         SPACE 3
********************************************************************
*                                                                  *
*        SAVE REGISTER 1 CONTENTS IN REGISTER 11 AND PLACE         *
*        PARAMETER AREA INTO PASSAREA                              *
*                                                                  *
********************************************************************
         SPACE 3
STARTIT  DS    0D
         LR    R11,R1                 SAVE REG 1 CONTENTS IN REG 11
         L     R9,0(R11)
         MVC   PASSAREA,0(R9)
         EJECT
         SPACE 3
********************************************************************
*                                                                  *
*        M A I N   L I N E   L O G I C                             *
*                                                                  *
********************************************************************
         SPACE 3
*
         LA    R10,0                  R10===> 0
*
         MVC   BRKINDSN,INDSNAME
         CALL  BREAKDSN,(BRKDSNIO),VL
         CLC   BRKVALID,=CL3'YES'     DID ALL GO WELL ?
         BE    LOKAYT                 IF SO, BRANCH TO LOKAYT
         LA    R10,16                 R10===> 16
         B     SHUTDOWN               BRANCH TO SHUTDOWN
LOKAYT   LOCATE CAMLIST1              SEARCH ZEE CATALOG
         C     R15,=F'0'              IS THE DSNAME IN THE CATALOG?
         BE    DOCALC                 IF SO, BRANCH TO DOCALC
         LA    R10,12                 R10===> 12
         B     SHUTDOWN               BRANCH TO SHUTDOWN
DOCALC   BAL   R5,CALCLENS            PERFORM ROUTINE TO CALCULATE
*                                       DSNAME AND DDNAME LENGTHS
         BAL   R5,DYNAMALC            PERFORM DYNAMIC ALLOCATION
         B     SHUTDOWN               BRANCH TO SHUTDOWN
         EJECT
         SPACE 3
*
********************************************************************
*                                                                  *
*  THIS ROUTINE IS RESPONSIBLE FOR GENERATING THE FOLLOWING        *
*  DATA FIELDS:                                                    *
*                                                                  *
*     DDNLEN  ===> BINARY LENGTH OF DDNAME                         *
*     DSNLEN  ===> BINARY LENGTH OF DSNAME                         *
*     MEMLEN  ===> BINARY LENGTH OF MEMBER NAME (IF PROVIDED)      *
*                                                                  *
********************************************************************
*
CALCLENS ST    R5,SAVEIT1
         LA    R6,8
         LA    R7,INDDNAME+7
LOOP1    CLI   0(R7),C' '
         BNE   MOVEIT1
         S     R6,=F'1'
         S     R7,=F'1'
         B     LOOP1
MOVEIT1  STH   R6,DDNLEN
         LA    R6,44
         LA    R7,BRKOTDSN+43
LOOP2    CLI   0(R7),C' '
         BNE   MOVEIT2
         S     R6,=F'1'
         S     R7,=F'1'
         B     LOOP2
MOVEIT2  STH   R6,DSNLEN
         CLC   BRKMEMPR,=CL3'YES'
         BNE   EXITCALC
         LA    R6,8
         LA    R7,BRKOTMEM+7
LOOP3    CLI   0(R7),C' '
         BNE   MOVEIT3
         S     R6,=F'1'
         S     R7,=F'1'
         B     LOOP3
MOVEIT3  STH   R6,MEMLEN
EXITCALC L     R5,SAVEIT1            RESTORE REGISTER 5
         BR    R5                    RETURN
         EJECT
         SPACE 3
*
********************************************************************
*                                                                  *
*               D Y N A M I C   A L L O C A T I O N                *
*               -----------------------------------                *
*                                                                  *
*        THIS ROUTINE FIRST ATTEMPTS TO FREE THE DDNAME GIVEN.     *
*        IF THE DDNAME WAS SUCCESSFULLY FREED, THEN THIS ROUTINE   *
*        BRANCHES TO THE DYNAMIC ALLOCATION MACRO DYNALLOC.        *
*        HOWEVER, IF THE DDNAME  WAS NOT SUCCESSFULLY FREED,       *
*        THEN THIS ROUTINE CHECKS FOR ERROR REASON CODE 0438       *
*        (DDNAME NOT FOUND).  IF THE ERROR REASON CODE IS 0438,    *
*        THEN THIS ROUTINE BRANCHES TO THE DYNAMIC ALLOCATION      *
*        MACRO DYNALLOC, ELSE IT LOADS R10 WITH A 4.               *
*                                                                  *
********************************************************************
*
DYNAMALC ST    R5,SAVEIT2
*
         MVC   FREEUT1A,DDNLEN
         MVC   FREEUT1B,INDDNAME
         MVC   DD1UNT1A,DDNLEN
         MVC   DD1UNT1B,INDDNAME
         MVC   DD1UNT2A,DSNLEN
         MVC   DD1UNT2B,BRKOTDSN
         MVC   DD1UNIT3,=XL2'0000'
         CLC   BRKMEMPR,=CL3'YES'
         BNE   FREEIT
         MVC   DD1UNIT3,=XL2'0003'
         MVC   DD1UNT3A,=XL2'0001'
         MVC   DD1UNT3B,MEMLEN
         MVC   DD1UNT3C,BRKOTMEM
*
FREEIT   LA    R1,FREEPARM
         DYNALLOC                      FREE DDNAME
         LTR   R15,R15                 TEST REGISTER 15 FOR ZERO
         BZ    DODYNAM                 IF ZERO, BRANCH TO DODYNAM
         CLC   ERCODE,HEX0438          COMPARE ERROR REASON CODE
*                                         AGAINST HEXADECIMAL 0438
         BE    DODYNAM                 IF EQUAL, BRANCH TO DODYNAM
         LA    R10,4                   R10===> 4
         L     R5,SAVEIT2
         BR    R5                      RETURN
DODYNAM  LA    R1,DD1PARM
         DYNALLOC                      ALLOC F(DDNAME) DA(DSN) SHR
         L     R5,SAVEIT2
         LTR   R15,R15                 TEST REGISTER 15 FOR ZERO
         BZR   R5                      IF ZERO, RETURN
         LA    R10,8                   R10===> 8
         BR    R5                      RETURN
         EJECT
         SPACE 3
********************************************************************
*                                                                  *
*        E N D   O F   J O B   P R O C E S S I N G                 *
*                                                                  *
********************************************************************
         SPACE 3
SHUTDOWN LR    R15,R10
         L     R13,4(R13)
         RETURN (14,12),RC=(15)
         EJECT
         SPACE 3
*
********************************************************************
*                                                                  *
*        DATA STRUCTURE FOR FREEING OF DDNAME                      *
*                                                                  *
********************************************************************
*
         SPACE 3
         DS    0F
FREEPARM DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(FREERB)   ADDRESS OF REQUEST BLOCK
FREERB   DC    X'14'         S99RBLN (LENGTH OF REQUEST BLOCK)
         DC    X'02'         S99VERB (REQUEST FOR UNALLOCATION)
         DC    X'2000'       S99FLAG1 (DO NOT MOUNT VOLUMES)
ERCODE   DC    X'0000'       S99ERROR (ERROR REASON CODE)
         DC    X'0000'       S99INFO (INFORMATION REASON CODE)
         DC    A(FREETUPL)   S99TXTPP (ADDR OF POINTER TO TEXT UNIT)
         DC    X'00000000'   RESERVED
         DC    X'00000000'   S99FLAG2
FREETUPL DC    A(FREEUNT1)   ADDRESS OF TEXT UNIT #1
         DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(FREEUNT2) ADDRESS OF TEXT UNIT#2
FREEUNT1 DC    X'0001'
         DC    X'0001'
FREEUT1A DS    BL2
FREEUT1B DS    CL8
FREEUNT2 DC    X'0007'
         DC    X'0000'
         EJECT
         SPACE 3
*
********************************************************************
*                                                                  *
*        DATA STRUCTURE FOR DYNAMIC ALLOCATION                     *
*                                                                  *
********************************************************************
*
         SPACE 3
         DS    0F
DD1PARM  DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(DD1RB)    ADDRESS OF REQUEST BLOCK
DD1RB    DC    X'14'         S99RBLN (LENGTH OF REQUEST BLOCK)
         DC    X'01'         S99VERB (REQUEST FOR DSNAME ALLOC)
         DC    X'2000'       S99FLAG1 (DO NOT MOUNT VOLUMES)
         DC    X'0000'       S99ERROR (ERROR REASON CODE)
         DC    X'0000'       S99INFO (INFORMATION REASON CODE)
         DC    A(DD1TUPL)    S99TXTPP (ADDR OF POINTERS TO TEXT UNITS
         DC    X'00000000'   RESERVED
         DC    X'00000000'   S99FLAG2
DD1TUPL  DC    A(DD1UNIT1)   ADDRESS OF TEXT UNIT #1
         DC    A(DD1UNIT2)   ADDRESS OF TEXT UNIT #2
         DC    A(DD1UNIT3)   ADDRESS OF TEXT UNIT #3
         DC    A(DD1UNIT4)   ADDRESS OF TEXT UNIT #4
         DC    X'80'         TURN ON HIGH ORDER BIT
         DC    AL3(DD1UNIT5) ADDRESS OF TEXT UNIT #5
DD1UNIT1 DC    X'0001'       DDNAME TEXT UNIT
         DC    X'0001'
DD1UNT1A DS    BL2
DD1UNT1B DS    CL8
DD1UNIT2 DC    X'0002'       DSNAME TEXT UNIT
         DC    X'0001'
DD1UNT2A DS    BL2
DD1UNT2B DS    CL44
DD1UNIT3 DS    XL2           MEMBER NAME TEXT UNIT
DD1UNT3A DS    XL2
DD1UNT3B DS    BL2
DD1UNT3C DS    CL8
DD1UNIT4 DC    X'0004',X'0001',X'0001',X'08'
DD1UNIT5 DC    X'0052'
         DC    X'0000'
         EJECT
         SPACE 3
********************************************************************
*        DATA AREAS                                                *
********************************************************************
         SPACE 3
         DS    0F
SAVEIT1  DS    F
SAVEIT2  DS    F
DDNLEN   DS    H
DSNLEN   DS    H
MEMLEN   DS    H
*
PASSAREA DS    0CL52
INDDNAME DS    CL8
INDSNAME DS    CL44
*
*   B R E A K D S N   P A R A M E T E R S
*
BRKDSNIO DS    0CL135
BRKINDSN DS    CL44
BRKOTDSN DS    CL44
BRKOTMEM DS    CL8
BRKMEMPR DS    CL3
BRKGDG   DS    CL3
BRKVALID DS    CL3
BRKERROR DS    CL30
*
CAMLIST1 CAMLST NAME,BRKOTDSN,,LOCAREA
LOCAREA  DS    0D
         DS    265C
HEX0438  DC    X'0438'
         END
//BREAKDSN EXEC COBUCL
//COB.SYSPRINT DD SYSOUT=*
//COB.SYSIN DD *
000010*******************************
000100 ID DIVISION.
000110*******************************
000200 PROGRAM-ID.     BREAKDSN.
000300 AUTHOR.         GARY DUFFIELD.
000310 REMARKS.
000320                 THIS PROGRAM EDITS ANY DATASET NAME PASSED TO IT
000330                 AND SPLITS OUT THE PDS MEMBER NAME IF IT EXISTS.
000340
000350*******************************
000400 ENVIRONMENT DIVISION.
000410*******************************
000500 CONFIGURATION SECTION.
000600 SOURCE-COMPUTER. IBM-370.
000700 OBJECT-COMPUTER. IBM-370.
000800
000810*******************************
000900 DATA DIVISION.
000910*******************************
001100 WORKING-STORAGE SECTION.
001300 01  TABLES.
001400     05  WORK-DSN-INPUT                  PIC X(44).
001500     05  DSN-CHARACTER REDEFINES WORK-DSN-INPUT
001600         OCCURS 44 TIMES INDEXED BY INX  PIC X.
001700
001800     05  WORK-DSN-OUTPUT                 PIC X(44).
001900     05  DSN-CHAR-OUT REDEFINES WORK-DSN-OUTPUT
002000         OCCURS 44 TIMES INDEXED BY INY  PIC X.
002100
002200     05  WORK-MEMBER                     PIC X(8).
002300     05  DSN-CHAR-MEMBER REDEFINES WORK-MEMBER
002400         OCCURS 8 TIMES INDEXED BY INZ   PIC X.
002500
002600 01  SWITCHES.
002700     05  BAD-CHAR                        PIC X(3).
002800         88  BAD-CHARACTER                           VALUE 'YES'.
002850     05  QUOTE-CHAR                      PIC 9(5) COMP-3.
002860         88  TOO-MANY-QUOTES                    VALUE 3 THRU 44.
002870         88  NOT-ENOUGH-QUOTES                  VALUE 1.
002900
003000 01  ACCUMULATORS.
003100     05  PERFORM-COUNT                   PIC 9(5) COMP-3.
003110     05  CHAR-COUNT                      PIC 9(5) COMP-3.
003200
003300     EJECT
003310*******************************
003400 LINKAGE SECTION.
003410*******************************
003500
003600 01  BREAKDSN-IO-AREA.
003700     05  INPUT-DSN                       PIC X(44).
003800     05  OUTPUT-DSN                      PIC X(44).
003900     05  OUTPUT-MEMBER-NAME              PIC X(8).
004000     05  MEMBER-NAME-PROVIDED            PIC X(3).
004010     05  GDG-TYPE-DATASET                PIC X(3).
004100     05  VALID-DSN-PASSED                PIC X(3).
004200     05  ERROR-MESSAGE                   PIC X(30).
004300     EJECT
004400********************************************
004500 PROCEDURE DIVISION USING BREAKDSN-IO-AREA.
004600********************************************
004700 0100-MAIN-MODULE.
004800********************************************
004900
004910     PERFORM 0110-INITIALIZE-MODULE.
004911     IF DSN-CHARACTER (INX) = SPACE OR '('
004912        MOVE 'YES' TO BAD-CHAR.
004920     IF DSN-CHARACTER (INX) = QUOTE
004930        COMPUTE QUOTE-CHAR = QUOTE-CHAR + 1
004940        MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY)
004950        SET INX UP BY 1
004960        SET INY UP BY 1.
005400     PERFORM 0200-EXAMINE-DSN-MODULE
005500        VARYING INX FROM INX BY 1
005600        UNTIL (DSN-CHARACTER (INX) = ' ' OR '(' OR QUOTE)
005610           OR (INX > 44)
005700           OR (BAD-CHARACTER).
005710     IF DSN-CHARACTER (INX) = QUOTE
005720        COMPUTE QUOTE-CHAR = QUOTE-CHAR + 1.
005730     SET INX DOWN BY 1.
005740     IF DSN-CHARACTER (INX) = '.'
005750        MOVE 'YES' TO BAD-CHAR.
005760     SET INX UP BY 1.
005800     IF BAD-CHARACTER
005900        MOVE 'NO ' TO VALID-DSN-PASSED
006000        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
006100     ELSE
006200        IF INX > 44
006210           IF NOT-ENOUGH-QUOTES
006220              MOVE 'NO ' TO VALID-DSN-PASSED
006230              MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
006240           ELSE
006300              MOVE WORK-DSN-OUTPUT TO OUTPUT-DSN
006400              MOVE 'NO ' TO MEMBER-NAME-PROVIDED
006410              MOVE 'NO ' TO GDG-TYPE-DATASET
006500              MOVE 'YES' TO VALID-DSN-PASSED
006700        ELSE
006800           IF DSN-CHARACTER (INX) = ' ' OR QUOTE
006900              PERFORM 0300-BASIC-DSN-MODULE
007000           ELSE
007100              PERFORM 0310-OTHER-DSN-MODULE.
007110     GOBACK.
007111
007112********************************
007113 0110-INITIALIZE-MODULE.
007114********************************
007115
007120     SET INX TO 1.
007130     SET INY TO 1.
007140     SET INZ TO 1.
007150     MOVE INPUT-DSN TO WORK-DSN-INPUT.
007160     MOVE SPACE TO WORK-DSN-OUTPUT WORK-MEMBER OUTPUT-DSN
007161                   OUTPUT-MEMBER-NAME MEMBER-NAME-PROVIDED
007162                   GDG-TYPE-DATASET VALID-DSN-PASSED
007163                   ERROR-MESSAGE.
007170     MOVE 'NO ' TO BAD-CHAR.
007180     MOVE ZERO TO QUOTE-CHAR PERFORM-COUNT CHAR-COUNT.
007200     EJECT
007300
007400********************************
007500 0200-EXAMINE-DSN-MODULE.
007600********************************
007700
007800     COMPUTE PERFORM-COUNT = PERFORM-COUNT + 1.
007900     IF (DSN-CHARACTER (INX) NOT NUMERIC)
008000        AND (DSN-CHARACTER (INX) NOT ALPHABETIC)
008100        AND (DSN-CHARACTER (INX) NOT = '@' AND '#' AND '$'
008110           AND QUOTE AND '.' AND SPACE)
008200        MOVE 'YES' TO BAD-CHAR.
008210     IF DSN-CHARACTER (INX) = '.'
008211        IF CHAR-COUNT = 0
008212           MOVE 'YES' TO BAD-CHAR
008213        ELSE
008220           COMPUTE CHAR-COUNT = 0
008230     ELSE
008231        IF DSN-CHARACTER (INX) NOT = SPACE
008240           COMPUTE CHAR-COUNT = CHAR-COUNT + 1.
008250     IF CHAR-COUNT > 8
008260        MOVE 'YES' TO BAD-CHAR.
008261     IF CHAR-COUNT = 1
008262        IF DSN-CHARACTER (INX) NUMERIC
008263           MOVE 'YES' TO BAD-CHAR.
008264     IF DSN-CHARACTER (INX) = QUOTE
008265        COMPUTE QUOTE-CHAR = QUOTE-CHAR + 1.
008266     IF TOO-MANY-QUOTES
008267        MOVE 'YES' TO BAD-CHAR.
008300     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY).
008400     SET INY UP BY 1.
008500
008600********************************
008700 0205-MOVE-MEMBER-MODULE.
008800********************************
008900
009000     COMPUTE PERFORM-COUNT = PERFORM-COUNT + 1.
009100     IF (DSN-CHARACTER (INX) NOT NUMERIC)
009200        AND (DSN-CHARACTER (INX) NOT ALPHABETIC)
009300        AND (DSN-CHARACTER (INX) NOT = '@' AND '#' AND '$')
009400        MOVE 'YES' TO BAD-CHAR.
009500     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-MEMBER (INZ).
009600     SET INZ UP BY 1.
009700     EJECT
009800********************************
009900 0300-BASIC-DSN-MODULE.
010000********************************
010100
010110     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY)
010120     SET INX UP BY 1
010200     SET INY UP BY 1
010300     PERFORM 0200-EXAMINE-DSN-MODULE
010400        VARYING INX FROM INX BY 1
010500        UNTIL (DSN-CHARACTER (INX) NOT = ' ' AND QUOTE)
010600           OR (INX > 44).
010610     IF BAD-CHARACTER
010620        MOVE 'NO ' TO VALID-DSN-PASSED
010630        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
010640     ELSE
010700        IF INX > 44
010710           IF NOT-ENOUGH-QUOTES
010720              MOVE 'NO ' TO VALID-DSN-PASSED
010730              MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
010740           ELSE
010800              MOVE WORK-DSN-OUTPUT TO OUTPUT-DSN
010900              MOVE 'NO ' TO MEMBER-NAME-PROVIDED
010910              MOVE 'NO ' TO GDG-TYPE-DATASET
011000              MOVE 'YES' TO VALID-DSN-PASSED
011100        ELSE
011200           MOVE 'NO ' TO VALID-DSN-PASSED
011300           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE.
011500
011600********************************
011700 0310-OTHER-DSN-MODULE.
011800********************************
011900
012000     SET INX UP BY 1
012100     IF INX > 44
012200        MOVE 'NO ' TO VALID-DSN-PASSED
012300        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
012400     ELSE
012500       IF (DSN-CHARACTER (INX) NUMERIC)
012600          OR (DSN-CHARACTER (INX) = '-' OR '+')
012700          PERFORM 0400-GDG-DSN-MODULE
012800       ELSE
012900          IF DSN-CHARACTER (INX) ALPHABETIC
012910             OR DSN-CHARACTER (INX) = '@' OR '#' OR '$'
013000             PERFORM 0410-PDS-DSN-MODULE
013100          ELSE
013200             MOVE 'NO ' TO VALID-DSN-PASSED
013300             MOVE 'INVALID MEMBER NAME' TO ERROR-MESSAGE.
013400     EJECT
013500********************************
013600 0400-GDG-DSN-MODULE.
013700********************************
013710
013800     SET INX DOWN BY 1.
013810     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY).
013820     SET INX UP BY 1.
013821     SET INY UP BY 1.
013830     IF DSN-CHARACTER (INX) = '+' OR '-'
013840        MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY)
013900        SET INX UP BY 1
013910        SET INY UP BY 1.
014000     COMPUTE PERFORM-COUNT = 0
014010     COMPUTE CHAR-COUNT = 1
014100     PERFORM 0200-EXAMINE-DSN-MODULE
014200        VARYING INX FROM INX BY 1
014300        UNTIL (DSN-CHARACTER (INX) NOT NUMERIC)
014400           OR (INX > 44)
014500           OR (PERFORM-COUNT > 3).
014600     IF (INX > 44) OR (PERFORM-COUNT > 3)
014700        MOVE 'NO ' TO VALID-DSN-PASSED
014800        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
014900     ELSE
015000        IF  DSN-CHARACTER (INX) = ')'
015010           IF PERFORM-COUNT = 0
015011              MOVE 'NO ' TO VALID-DSN-PASSED
015012              MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
015020           ELSE
015100              PERFORM 0500-GDG-CONT-MODULE
015200        ELSE
015300           MOVE 'NO ' TO VALID-DSN-PASSED
015400           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE.
015600
015700********************************
015800 0410-PDS-DSN-MODULE.
015900********************************
016000
016200     COMPUTE PERFORM-COUNT = 0.
016210     COMPUTE CHAR-COUNT = 0.
016300     PERFORM 0205-MOVE-MEMBER-MODULE
016400        VARYING INX FROM INX BY 1
016500        UNTIL (DSN-CHARACTER (INX) = ')' OR ' ')
016600           OR (INX > 44)
016700           OR (PERFORM-COUNT > 8)
016800           OR (BAD-CHARACTER).
016900     IF BAD-CHARACTER
017000        MOVE 'NO ' TO VALID-DSN-PASSED
017100        MOVE 'INVALID MEMBER NAME' TO ERROR-MESSAGE
017200     ELSE
017300        IF INX > 44
017400           MOVE 'NO ' TO VALID-DSN-PASSED
017500           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
017600        ELSE
017700           IF PERFORM-COUNT > 8
017800              MOVE 'NO ' TO VALID-DSN-PASSED
017900              MOVE 'MEMBER NAME TOO LONG' TO ERROR-MESSAGE
018000           ELSE
018100              IF  DSN-CHARACTER (INX) = ')'
018200                 PERFORM 0510-PDS-CONT-MODULE
018300              ELSE
018400                 MOVE 'NO ' TO VALID-DSN-PASSED
018500                 MOVE 'INVALID MEMBER NAME' TO ERROR-MESSAGE.
018600     EJECT
018700********************************
018800 0500-GDG-CONT-MODULE.
018900********************************
019000
019010     MOVE DSN-CHARACTER (INX) TO DSN-CHAR-OUT (INY)
019100     SET INX UP BY 1
019110     SET INY UP BY 1
019200     PERFORM 0200-EXAMINE-DSN-MODULE
019300        VARYING INX FROM INX BY 1
019400        UNTIL (DSN-CHARACTER (INX) NOT = ' ' AND QUOTE)
019500           OR (INX > 44).
019510     IF BAD-CHARACTER
019520        MOVE 'NO ' TO VALID-DSN-PASSED
019530        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
019540     ELSE
019600        IF INX > 44
019610           IF NOT-ENOUGH-QUOTES
019611              MOVE 'NO ' TO VALID-DSN-PASSED
019612              MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
019620           ELSE
019700              MOVE WORK-DSN-OUTPUT TO OUTPUT-DSN
019800              MOVE 'NO ' TO MEMBER-NAME-PROVIDED
019900              MOVE 'YES' TO VALID-DSN-PASSED
019910              MOVE 'YES' TO GDG-TYPE-DATASET
020000        ELSE
020100           MOVE 'NO ' TO VALID-DSN-PASSED
020200           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE.
020400
020500********************************
020600 0510-PDS-CONT-MODULE.
020700********************************
020800
020900     SET INX UP BY 1
021000     PERFORM 0200-EXAMINE-DSN-MODULE
021100        VARYING INX FROM INX BY 1
021200        UNTIL (DSN-CHARACTER (INX) NOT = ' ' AND QUOTE)
021300           OR (INX > 44).
021310     IF BAD-CHARACTER
021320        MOVE 'NO ' TO VALID-DSN-PASSED
021330        MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE
021340     ELSE
021400        IF INX > 44
021410           IF NOT-ENOUGH-QUOTES
021420              MOVE 'NO ' TO VALID-DSN-PASSED
021430              MOVE 'INVALID MEMBER NAME' TO ERROR-MESSAGE
021440           ELSE
021500              MOVE WORK-DSN-OUTPUT TO OUTPUT-DSN
021600              MOVE WORK-MEMBER TO OUTPUT-MEMBER-NAME
021700              MOVE 'YES' TO MEMBER-NAME-PROVIDED
021710              MOVE 'NO ' TO GDG-TYPE-DATASET
021800              MOVE 'YES' TO VALID-DSN-PASSED
021900        ELSE
022000           MOVE 'NO ' TO VALID-DSN-PASSED
022100           MOVE 'INVALID SYNTAX FOR DSNAME' TO ERROR-MESSAGE.
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD    <- TARGET LOAD LIBRARY
//LKED.SYSIN DD *
  NAME DYNALLOC(R)
//
