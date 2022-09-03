//DUTAPES  JOB (TSO),
//             'Install DUTAPES',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*
//* ***************************************************************** *
//* INSTALL DUTAPES COMMAND IN SYS2.CMDLIB (HELP IN SYS2.HELP)        *
//* ***************************************************************** *
//*
//DUTAPES EXEC ASMFCL,PARM.ASM='LIST,NODECK,LOAD,RENT',
//             MAC1='SYS1.AMODGEN',PARM.LKED='LIST,MAP,NOCAL,RENT'
//ASM.SYSIN DD *
*          DATA SET S051B19    AT LEVEL 001 AS OF 07/06/77
*          DATA SET S051B19    AT LEVEL 002 AS OF 03/23/77
*          DATA SET S051B19    AT LEVEL 001 AS OF 09/08/76
P051B19 CSECT
*
* THIS IS THE DTAPES COMMAND FOR TSO    OS/VS2 R3.
*
* THE COMMAND WILL DISPLAY THE STATUS AND VOLSERS OF ALL TAPE UNITS.
* THE COMMAND SYNTAX IS:
*
*   DTAPES
*
*
* ADAPTED FROM MICHIGAN MODS 'USPACE' COMMAND FROM UNITED AIR LINES
*       TSO MODS 3-75  BY KEN TRUE  MAY76
*
*  ALSO BAGGED OBTAIN CODE FROM U.C.L.A  URLISTVO COMMAND FROM THE
*       SAME MISHMODS TAPE.  KT JUNE76.
*
*  BAGGED SORT UCB ADDRESS CODE FROM MISHMODS DDASD COMMAND
*                            KT 13JULY76
*
*  INSTALLED SEARCH OF DEVNAMET FOR UNIT DESCRIPTION.. KT 26APR77
*
*  MODIFIED TO RUN AS TSO COMMAND OR OPER STARTED TASK KT  27MAY77
*
*
*
LINKREG  EQU   6                   LINK REGISTER
*
         STM   R14,R12,D12(R13)         SAVE REGISTERS
         BALR  R12,R0                   PROGRAM BASE
         USING *,R12                    PROGRAM ADDRESSABILITY
         LR    R2,R1
         GETMAIN R,LV=CORENEED          OBTAIN DYNAMIC CORE
         ST    R13,D4(R1)               CHAIN
         ST    R1,D8(R13)                SAVE
         LR    R13,R1                     AREAS
         LR    R11,R13                  CORE ADDR.
         USING CORE,R11
         LOAD  EP=DEVNAMET        LOAD DEVICE NAME TABLE
         ST    R0,DEVADDR         ...AND SAVE ITS ADDRESS
         MVC   EXTRACT1(EXTRACTL),EXTRACT MOVE IN EXTRACT LIST FORM
         LA    R1,ANSWER          LOAD ADDR OF ANSWER
         ST    R1,EXTRACT1          AND SAVE IT IN THE MACRO
         EXTRACT MF=(E,EXTRACT1)
         L     R1,ANSWER            (R1)=ADDR OF TSO FLAG FROM EXTRACT
         MVC   TSOFLAG,0(R1)        MOVE IN THE ANSWER TO OUR FLAG
         SPACE
LISTALL  MVI   SW1,X'00'                INIT PRINT HDR SWITCH
*
         L     R7,CVTPTR                CVT ADDR.
         USING CVT,R7                   CVT ADDRESSABILITY
         L     R7,CVTILK2               UCB LOOKUP TABLE ADDR.
         DROP  R7                       KILL CVT ADDRESSABILITY
         USING UCBSECT,R8               SET ADDRESSAB TO UCB.
         EJECT
         XC    LCT,LCT                  CLEAR COUNT
         MVC   LMAX(2),=H'1000'         SAY ROOM FOR 500 ENTRIES
CYCLE    LH    R8,D0(R7)                UCB ADDR.
         LTR   R8,R8                    TEST UCB STATUS
         BM    RLSE                     END OF TABLE
         BZ    INDEX                    PAD ENTRY
         CLI   UCBID,UCBSTND            IS UCB STANDARD?
         BNE   INDEX                    NO, ERROR
         CLI   UCBTBYT3,UCB3TAPE        IS IT A TAPE?
         BNE   INDEX                    NO
         SR    R4,R4
COMPLIST CH    R4,LCT                   SEE IF WEVE SEEN THIS ONE ?
         BNL   ADDLIST                  NOPE-> GO ADDIT
         CH    R8,LIST(R4)
         BE    INDEX
         LA    R4,2(R4)
         B     COMPLIST
ADDLIST  LH    R4,LCT
         CH    R4,LMAX
         BNL   FULL
         STH   R8,LIST(R4)
         LA    R4,2(R4)
         STH   R4,LCT
FULL     EQU   *
DISPCHK  TM    SW1,HEADING              HAVE WE WRITTEN THE HEADER?
         BNO   WRTFMT                   ->NO..GO WRITE IT
         SPACE
DISPLAY  MVC   BUFFER(WTOMSGL),WTOMSG   REINIT WTO MESSAGE
         MVC   MSGUNIT,UCBNAME          COPY UNIT ADDRESS
         CLI   UCBVOLI,X'00'            ANY VOLSER IN UCBVOLI ?
         BE    NOVOLSER                 NO-> SKIP MOVE OF VOLSER
         MVC   MSGVOL,UCBVOLI           COPY VOLUME SERIAL NUMBER
*
NOVOLSER TM    UCBSTAT,UCBONLI          IS DRIVE ONLINE?
         BNO   NOTON                    ->NO..MARK MSG AS OFFLINE
         MVC   MSGSTAT,ONLINE           ->YES..SAY SO.
         B     CHKVARY
NOTON    MVC   MSGSTAT,OFFLINE
*
CHKVARY  TM    UCBSTAT,UCBCHGS          IS IT CHANGING ON/OFF?
         BNO   CHKDEN                   ->NO GO CHECK DENSITY..
         MVC   MSGVARY,VARY             ->YES..SAY SO.
*
CHKDEN   TM    UCBTBYT2,UCBDUDN1        IS IT 800/1600?
         BNO   CHKDEN1                  ->NO ..KEEP LOOKING
         MVC   MSGDENS,DDEN1
         B     CHKMODL
CHKDEN1  TM    UCBTBYT2,UCBDUDN2        IS IT 1600/6250?
         BNO   CHKDEN2                  -> NO
         MVC   MSGDENS,DDEN2
         B     CHKMODL
CHKDEN2  TM    UCBTBYT1,UCBD1600         HOW ABOUT JUST 1600?
         BNO   CHKDEN3                   -> NO..ONCE MORE WITH FEELING
         MVC   MSGDENS,DEN1600
         B     CHKMODL
CHKDEN3  TM    UCBTBYT1,UCBD6250         LAST CHANCE..IS IT JUST 6250?
         BNO   CHKMODL                   -> NO..DONT KNOW DENSITY..
         MVC   MSGDENS,DEN6250
         SPACE
***
***   USE SYSTEM DEVICE NAME TABLE
***
***
CHKMODL  L     R1,DEVADDR         ADDRESS OF DEVICE NAME TABLE
         L     R10,0(R1)          NUMBER OF ENTRIES
         LA    R1,4(R1)           ADDRESS OF FIRST ENTRY
DEVLP    CLC   UCBTYP(4),8(R1)    IS THIS THE DEVICE TYPE
         BE    DEVFND             YES... BR TO SAVE DEVICE TYPE
         LA    R1,12(R1)          NO... INCR TO NEXT TABLE ENTRY
         BCT   R10,DEVLP          LOOP UNTIL END OF TABLE
         MVC   MSGMODL,=C'????'   SAY WE DONT KNOW DEVICE TYPE
         B     GETWHO
*
DEVFND   MVC   MSGMODL,0(R1)           MOVE UNIT TYPE TO PRINT LINE
*
GETWHO   XR    R10,R10
         L     R10,UCBEXTPT             GET EXTENTION TO UCB
         LTR   R10,R10                  ANYTHING THERE?
         BZ    PUTIT                    NO->SPLIT
         LA    R10,14(R10)              GET UCBASID ADDRESS
         LH    R10,0(R10)               AND GET THE ASID
         N     R10,=X'0000FFFF'          PLUS MASK OFF RESIDUE..
         LTR   R10,R10                  ANY BODY THERE?
         BZ    PUTIT                    NO->SIGH,ALL THAT FOR NAUGHT..
*
GOTASID  L     R4,CVTPTR                GET ADDR OF CVT
         USING CVT,R4                    AND ADDRESS YOURSELF TO IT..
         L     R4,CVTASVT               GET ADDR OF ASVT..
         DROP  R4
         USING ASVT,R4                  AND ESTAB ADDRESSABILITY
         SLL   R10,2                    ADJUST ASID..TO USE AS INDEX
         L     R10,ASVTFRST(R10)        R10=ADDR(ASCB) FOR ASID
         DROP  R4
         USING ASCB,R10                 GOTCHA
         L     R4,ASCBJBNI              IS IT A JOB
         LTR   R4,R4
         BZ    ITSTSO
         MVC   MSGUSER,0(R4)            MOVIN JOBNAME
         B     PUTIT
ITSTSO   L     R4,ASCBJBNS              ITS START/MOUNT/LOGON
         LTR   R4,R4
         BZ    PUTIT
         MVC   MSGUSER,0(R4)            MOVEIN USERID
PUTIT    DS    0H
         LA    R1,BUFFER                MESSAGE ADDR.
         BAL   LINKREG,PUTMSG           ISSUE MESSAGE
         OI    SW1,FOUND                SAY WE FOUND A VOL...
         SPACE
INDEX    LA    R7,D2(R7)                NEXT ENTRY ADDR.
         B     CYCLE                    CONTINUE
         EJECT
WRTFMT   LA    R1,FMTMSG1               WRITE HEADERS....
         BAL   LINKREG,PUTMSG
         OI    SW1,HEADING              SAY WE WROTE THE HEADERS
         B     DISPLAY
         SPACE 2
RLSE     LR    R1,R11                   CORE ADDR.
         L     R13,D4(R13)
THEEND   FREEMAIN R,LV=CORENEED,A=(1)   FREE DYNAMIC CORE
         XC    D16(D4,R13),D16(R13)
         LM    R14,R12,D12(R13)
         XR    R15,R15                  SET RC=0
         BR    R14                      RETURN
         EJECT
*.....................................................................*
*        LOCAL SUBROUTINE FOR IO TO OPER/USER                         *
*.....................................................................*
         SPACE
*
* ENTER WITH WTO,WTOR LIST FORM POINTED TO BY R1
*
*    BAL  LINKREG,PUTMSG    OR
*    BAL  LINKREG,PUTGET
*
*  USES R14 FOR WORK
*
*
PUTMSG   DS    0H
         TM    TSOFLAG,X'80'       IS IT TSO?
         BO    TPUTIT              YES-> GO DO TPUT
         SVC   35                  NO-> USE WTO
         BR    LINKREG
TPUTIT   DS    0H
         LH    R0,0(,R1)           GET LENGTH OF WTO MESSAGE
         S     R0,=F'4'            SUBTRACT OFF HEADER
         LA    R1,4(,R1)           BUMP MSG ADDRESS PAST HDR
         TPUT  (1),(0),R           DO THE TPUT
         BR    LINKREG
         SPACE 2
PUTGET   DS    0H
         TM    TSOFLAG,X'80'       IS IT TSO?
         BO    TSOIT               YES-> GO USE TGET
         LR    R14,R1              NO-> USE WTOR .. SAVE MSG ADDRESS
         SVC   35                  DO THE WTOR...
         L     R1,4(,R14)          GET ADDR OF ECB
         XC    0(4,R1),0(R1)       CLEAN OUT THE ECB....
         LA    R0,1
         SVC   1                   WAIT FOR WTOR TO COMPLETE
         B     PUTRET              RETURN
TSOIT    DS    0H
         LR    R14,R1              SAVE
         LH    R0,8(,R1)           PUT LENGTH
         S     R0,=F'4'
         LA    R1,12(,R1)          PAST HDR
         TPUT  (1),(0),R
         SR    R0,R0
         IC    R0,0(,R14)          GET REPLY LENGTH
         L     R1,0(,R14)          GET REPLY ADRS
         LA    R1,0(,R1)
         ICM   R1,B'1000',=X'80'   INDICATE TGET...
         TGET  (1),(0),R
PUTRET   DS    0H
         L     R1,0(,R14)          GET REPLY ADRS
         XR    R0,R0
         IC    R0,0(,R14)          GET REPLY LENGTH
UPPER    DS    0H
         OI    0(R1),C' '          UPPER CASE
         LA    R1,1(,R1)           NXT
         BCT   R0,UPPER
         BR    LINKREG
         SPACE 2
         EJECT
*.....................................................................*
*        LIST FORMS OF MACROS                                         *
*.....................................................................*
         SPACE 2
EXTRACT  EXTRACT *-*,'S',FIELDS=(TSO),MF=L
EXTRACTL EQU   *-EXTRACT
         SPACE 2
FMTMSG1  WTO   'UNIT VOLUME STATUS  USER          DENSITY   MODEL      X
                         ',ROUTCDE=(11),MF=L
         SPACE 2
WTOMSG   WTO   '                                                       X
                         ',ROUTCDE=(11),MF=L
WTOMSGL  EQU   *-WTOMSG
         EJECT
*.....................................................................*
*        PROGRAM CONSTANTS/LITERALS                                   *
*.....................................................................*
         SPACE 2
ONLINE   DC    CL7'ONLINE'
OFFLINE  DC    CL7'OFFLINE'
VARY     DC    CL4'VARY'
DDEN1    DC    CL9'800/1600'
DDEN2    DC    CL9'1600/6250'
DEN800   DC    CL9'800'
DEN1600  DC    CL9'1600'
DEN6250  DC    CL9'6250'
*
         EJECT
*
* PROGRAM  E Q U A T E S
*
R0       EQU   0              MACROS-WORK
R1       EQU   1              MACROS-WORK
R2       EQU   2
R3       EQU   3
R4       EQU   4              WORK/ASVT
R5       EQU   5
R6       EQU   6              LINKREG FOR PUTGET
R7       EQU   7              UCB LOOKUP TABLE
R8       EQU   8              UCB ADDRESSABILITY
R9       EQU   9
R10      EQU   10             WORK/ASCB
R11      EQU   11             CORE DSECT (WORK AREA) ADDRESSABILITY
R12      EQU   12             PROGRAM BASE REGISTER
R13      EQU   13             SAVE AREA POINTER
R14      EQU   14             MACROS - WORK  RETURN ADDRESS
R15      EQU   15             MACROS - WORK/ RETURN CODE
*
D0       EQU   0
D1       EQU   1
D2       EQU   2
D4       EQU   4
D8       EQU   8
D12      EQU   12
D16      EQU   16
*
HEADING  EQU   X'01'
FOUND    EQU   X'02'
NOTFOUND EQU   X'FD'
SKIP     EQU   X'80'
NOSKIP   EQU   X'7F'
CHARZERO EQU   C'0'
         EJECT
CVT      DSECT
         CVT       SYS=VMS,TSO=YES
         EJECT
UCBSECT  DSECT
         IEFUCBOB
         EJECT
         IHAASCB
         EJECT
         IHAASVT
         EJECT
*
* DYNAMIC WORK AREA FOR PROGRAM
*
CORE     DSECT
         DS    18F
DEVADDR  DS    F
BUFFER   DS    CL80              TO RECEIVE THE WTO LIST FORM
         ORG   BUFFER+4
         DS    CL1
MSGUNIT  DS    CL3
         DS    CL1
MSGVOL   DS    CL6
         DS    CL1
MSGSTAT  DS    CL7
         DS    CL1
MSGUSER  DS    CL8
         DS    CL1
MSGVARY  DS    CL4
         DS    CL1
MSGDENS  DS    CL9
         DS    CL1
MSGMODL  DS    CL8
*
         ORG
*
SW1      DS    1C
TSOFLAG  DS    X                   ='80' IF TSO. '00' IF NOT TSO
ANSWER   DS    F                   ADDRESS POINTER FOR EXTRACT
DBLW     DS    4D
         ORG   DBLW
EXTRACT1 DS    4D
         ORG
LCT      DS    H                        TO REMEMBER COUNT
LMAX     DS    H                        TO REMEMBER UCB MAX ADDR
LIST     DS    500H                     TO REMEMBER EACH UCB ADDR
CORENEED EQU   *-CORE
         END
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR    <== TARGET LOAD LIBRARY
//LKED.SYSIN DD *
  NAME DUTAPES(R)
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP         <== TARGET HELP LIBRARY
//SYSIN    DD  *
./ ADD NAME=DUTAPES
)F FUNCTION -
   THE DUTAPES COMMAND DISPLAYS ALL TAPE DEVICES
   DEFINED IN THE SYSTEM, WITH THE FOLLOWING DATA :
     -  UNIT NUMBER
     -  STATUS
        = ONLINE OR OFFLINE
     -  DENSITY
     -  MODEL
     -  VOLUME ID, IF ONLINE
     -  USERID, IF ALLOCATED
)X SYNTAX -
         DUTAPES
)O OPERANDS -
  THERE ARE NO OPERANDS ON THE DUTAPES COMMAND
./ ENDUP
//PROC    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.PROCLIB      <== TARGET PROC LIBRARY
//SYSIN    DD  DATA
./ ADD NAME=DUTAPES
//*-------------------------------------------------------------------*
//*                  DISPLAY INFORMATION FOR TAPE UNITS               *
//*-------------------------------------------------------------------*
//DUTAPES  PROC
//DUTAPES  EXEC PGM=DUTAPES
//STEPLIB   DD  DSN=SYS2.CMDLIB,DISP=SHR
./ ENDUP
//
