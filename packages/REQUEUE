//REQUEUE JOB (JOB),
//             'INSTALL REQUEUE',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//SYSIN    DD  DATA,DLM=@@
***********************************************************************
*                                                                     *
* The source was sent to me by                   (somitcw@yahoo.com)  *
*                                                                     *
* If things don't work on the Turnkey system, blame me                *
* If everything works out okay, it is his fault                       *
*                                                                     *
***********************************************************************
PASS     TITLE 'REQUEUE - - - Program to re-queue a JOB'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*   This program delays 1 minute and issues an operator command
* to re-queue the job that executed it.
*
*  Blame: somitcw@yahoo.com
*
*  Example:
*
* //IEHLIST EXEC PGM=IEHLIST
* //SY5PR1NT DD SYSOUT=*
*  LISTVTOC VOL=3350=SYSRES,FORMAT
* //VSYSRES  DD UNIT=SYSDA,VOL=SER=SYSRES,DISP=SHR
* //REQUEUE EXEC PGM=REQUEUE,COND=(0,EQ,IEHLIST)
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         PRINT ON,GEN,NODATA
REQUEUE  CSECT ,
         USING *,R15              SET TEMPORARY BASE REGISTER
         SAVE  (14,12),,'REQUEUE.&SYSDATE..&SYSTIME' SAVE CALLER'S REGS
         LA    R12,SAVEAREA       LOAD THE ADDRESS ON THE NEW SAVE AREA
         ST    R12,8(,R13)        CONNECT OLD AND NEW SAVE AREAS
         ST    R13,4(,R12)        CONNECT OLD AND NEW SAVE AREAS
         LR    R13,R12            CONNECT OLD AND NEW SAVE AREAS
         DROP  R15                DROP THE TEMPORARY BASE REGISTER
         LR    R12,R15            LOAD THE PROGRAM'S BASE REGISTER
         USING REQUEUE,R12        GIVE THE ASSEMBLER THE NEW BASE
*
*   THE FOLLOWING CODE DELAYS THIS JOB FOR 1 MINUTE
         STIMER WAIT,BINTVL=TIME  WAIT FOR 6000 HUNDREDTHS OF SECONDS
*   THE PRECEEDING CODE DELAYS THIS JOB FOR 1 MINUTE
*
*   THE FOLLOWING CODE FINDS THE JES2 JOB OR STC NUMBER
         USING PSA,R0             SET ADDRESSIBILITY TO THE TCB
         L     R1,PSATOLD         LOAD THE ADDRESS OF THE TCB
         USING TCB,R1             SET ADDRESSIBILITY TO THE TCB
         L     R1,TCBJSCB         LOAD THE ADDRESS OF THE JOBSTEP CB
         USING IEZJSCB,R1         SET ADDRESSIBILITY TO THE JSCB
         L     R1,JSCBSSIB        LOAD THE ADDR. OF THE SUB-SYS ID BLK
         USING SSIB,R1            SET ADDRESSIBILITY TO THE SSIB
         MVC   CMD+06(8),SSIBJBID PUT JOB WITH NUMBER IN $E COMMAND
         MVC   CMD+16(8),SSIBJBID PUT JOB WITH NUMBER IN $C COMMAND
         MVC   CMD+26(8),SSIBJBID PUT JOB WITH NUMBER IN $H COMMAND
         DROP  R1
*  THE PRECEEDING CODE FINDS THE JES2 JOB OR STC NUMBER
*
         MODESET MODE=SUP,KEY=ZERO SWITCH TO SUPERVISOR MODE
         LTR   R15,R15            SEE IF MODESET WORKED
         BNE   ERROR1             GO ABEND IF MODESET FAILED
*
         LA    R1,CMD             LOAD ADDRESS OF COMMAND TO BE EXCUTED
         SLR   R0,R0              CLEAR REGISTER ZERO FOR SVC 34
         SVC   34                 ISSUE OPERATOR COMMAND
*
         MODESET MODE=PROB,KEY=NZERO SWITCH BACK TO PROBLEM STATE
         LTR   R15,R15            SEE IF MODESET WORKED
         BNE   ERROR2             GO ABEND IF MODESET FAILED
*
*   THE FOLLOWING CODE DELAYS THIS JOB TO GIVE THE CANCEL TIME TO WORK
         STIMER WAIT,BINTVL=TIME  WAIT FOR 6000 HUNDREDTHS OF SECONDS
*   THE PRECEEDING CODE DELAYS THIS JOB TO GIVE THE CANCEL TIME TO WORK
*
         WTO   'Please requeue this JOB - automatic requeue failed',   C
               ROUTCDE=(1,11),DESC=2    Highlight only first message
FAILED   DS   0H
         STIMER WAIT,BINTVL=TIME2  WAIT FOR 6000 HUNDREDTHS OF SECONDS
         WTO   'Please requeue this JOB - automatic requeue failed',   C
               ROUTCDE=(1,11)
         B     FAILED            Repeat message every 10 minutes
*
*  Never end normally
*        L     R13,SAVEAREA+4     LOAD CALLER'S SAVE AREA ADDRESS
*        RETURN (14,12),RC=0      RETURN TO CALLER
*
         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*      ERROR ROUTINES
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 2
ERROR1   DS    0H
         WTO   'MODESET MODE=SUP,KEY=ZERO FAILED',      ERROR MESSAGE  C
               ROUTCDE=(1,11)     DISPLAY TO OPERATOR AND PROGRAMMER
         ABEND 1                  ABEND THE PROGRAM, NODUMP
         SPACE 2
ERROR2   DS    0H
         WTO   'MODESET MODE=PROB,KEY=NZERO FAILED',    ERROR MESSAGE  C
               ROUTCDE=(1,11)     DISPLAY TO OPERATOR AND PROGRAMMER
         ABEND 2                  ABEND THE PROGRAM, NODUMP
         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*      WORKAREA AND CONSTANTS
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 2
SAVEAREA DC    18F'0'             REGISTER SAVE AREA
*
TIME     DC    F'6000'            DELAY IN HUNDREDTHS/SECONDS IN BINARY
TIME2    DC    F'60000'           DELAY IN HUNDREDTHS/SECONDS IN BINARY
*
CMD      DC    AL2(35),AL2(0),CL31'$EXXXXXXXX;CXXXXXXXX;HXXXXXXXX '
         SPACE 3
R0       EQU   0                  REGISTER EQUATE FOR REGISTER 0
R1       EQU   1                  REGISTER EQUATE FOR REGISTER 1
R2       EQU   2                  REGISTER EQUATE FOR REGISTER 2
R3       EQU   3                  REGISTER EQUATE FOR REGISTER 3
R4       EQU   4                  REGISTER EQUATE FOR REGISTER 4
R5       EQU   5                  REGISTER EQUATE FOR REGISTER 5
R6       EQU   6                  REGISTER EQUATE FOR REGISTER 6
R7       EQU   7                  REGISTER EQUATE FOR REGISTER 7
R8       EQU   8                  REGISTER EQUATE FOR REGISTER 8
R9       EQU   9                  REGISTER EQUATE FOR REGISTER 9
R10      EQU   10                 REGISTER EQUATE FOR REGISTER 10
R11      EQU   11                 REGISTER EQUATE FOR REGISTER 11
R12      EQU   12                 REGISTER EQUATE FOR REGISTER 12
R13      EQU   13                 REGISTER EQUATE FOR REGISTER 13
R14      EQU   14                 REGISTER EQUATE FOR REGISTER 14
R15      EQU   15                 REGISTER EQUATE FOR REGISTER 15
         EJECT
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*      DSECTS FOLLOW
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE
         IEFJSSIB ,               SUBSYSTEM IDENTIFICATION BLOCK DSECT
         EJECT
         IEZJSCB  ,               JOB STEP CONTROL BLOCK DSECT
         EJECT
         IKJTCB   ,               TASK CONTROL BLOCK DSECT
         EJECT
         IHAPSA   ,               PREFIXED STORAGE AREA
         END   ,                  THE END OF THE PROGRAM
@@
//SYSLIB   DD  DISP=SHR,DSN=SYS2.MACLIB,DCB=BLKSIZE=32720
//         DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT2   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT3   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DISP=(,PASS),UNIT=VIO,SPACE=(CYL,(1,1))
//LINK    EXEC PGM=IEWL,
//             COND=(0,NE),
//             PARM='NORENT,LIST,LET,MAP,AC=1'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME REQUEUE(R)
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=REQUEUE
//REQUEUE  JOB  (SETUP),
//             'Run REQUEUE',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: REQUEUE
//*
//* Desc: Run the REQUEUE program
//*
//********************************************************************
//IEHLIST EXEC PGM=IEHLIST
//SY5PR1NT DD SYSOUT=*
 LISTVTOC VOL=3350=MVSRES,FORMAT
//VSYSRES  DD UNIT=SYSDA,VOL=SER=MVSRES,DISP=SHR
//REQUEUE EXEC PGM=REQUEUE,COND=(0,EQ,IEHLIST)
//
@@