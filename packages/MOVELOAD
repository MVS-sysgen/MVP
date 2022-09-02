//MOVELOAD JOB (JOB),
//             'INSTALL MOVELOAD',
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
 PUNCH ' ENTRY MOVELOAD'
 PUNCH ' ALIAS MOVELOD'
 PUNCH ' SETCODE AC(1)'
MOVELOAD CSECT ,
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
*
* Where did I get this code?  CBT Tape?  Share Tape?  IBM-MAIN?
*
*   MOVELOAD - Load IEHMOVE modules & link to IEHMOVE
*   MOVELOD - Load/Delete IEHMOVE modules
*
*     Entry:  As at entry to IEHMOVE
*
*     Exit:   Aa at exit to IEHMOVE
*
* When using IEHMOVE on small data sets, IEHMOVE can do as much
* I/O loading its modules as copying data.  This program
* pre-loads all RENT or REUS IEHMOVE modules to avoid the
* program load I/O.
*
* Since the normal IEHMOVE requires APF authorization to run,
* this module would have to be linked AC=1 to run it.
*
* ( Note that there are Zap's which allow IEHMOVE to run without
*   APF authorization for a major subset of functions.
*   There are also Zap's which allow VIO work files for IEHMOVE ).
*
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
         SPACE 1
MOVELOAD CSECT
         STM   R14,R12,12(R13)
         LR    R12,R15
         LR    R11,R13            SAVE @ ORIG SAVEAREA
         USING MOVELOAD,R12
         GETMAIN R,LV=72
         LR    R13,R1             @ NEW SAVEAREA
         ST    R11,4(,R13)
         ST    R13,8(,R11)
         SPACE 1
         LA    R1,PARML
         L     R15,=A(MOVELOD)
         BALR  R14,R15            LOAD THE MODULES
         SPACE 1
         L     R1,12+8+4*R1(,R11) ORIG R1 VALUE
         LINK  EPLOC=MODS          GO TO MODULE
         ST    R15,8+4+4(,R11)    RETURN RETURN TO CALLERS R15 SLOT
         SPACE 1
         LA    R1,PARMD
         L     R15,=A(MOVELOD)
         BALR  R14,R15            DELETE THE MODULES
         SPACE 1
         FREEMAIN R,LV=72,A=(R13) FREE MY SAVEAREA
         LR    R13,R11            RESTORE CALLERS R13
         LM    R14,R12,12(R13)    RESTORE CALLERS REGS
         BR    R14                 AND RETURN
         DROP  R12               MOVELOAD
         SPACE 1
PARML    DC    A(*+4),Y(1),C'L'     LOAD PARM FOR MOVELOD
PARMD    DC    A(*+4),Y(1),C'D'     DELETE PARM FOR MOVELOD
         EJECT ,
W#       DSECT
W#SAVE   DC    18F'0'
W#BXH    DC    3F'0'              @ FIRST-1, LEN, @ LAST BLDL ENTRIES
W#MOD    DC    CL8' '
W#WTO    DC    CL140' '
         DC    0D'0'
W#BLDL   DC    Y(MODS#,BLDLLEN)
W#BLDL1  EQU   *
W#L      EQU   *-W#
         SPACE 1
         ENTRY MOVELOD
MOVELOAD CSECT ,
MOVELOD  STM   R14,R12,12(R13)    SAVE CALLERS REGS
         LR    R12,R15
         L     R11,0(,R1)         GET PARM ADDRESS
         USING MOVELOD,R12
         L     R2,=A(W#L+(MODS#*BLDLLEN))
         GETMAIN R,LV=(R2)
         LA    R9,0(,R1)          @ W#
         LR    R0,R9              @ W# FOR CLEAR
         LR    R1,R2              LENGTH OF AREA
         SR    R15,R15            CLEAR TO ZERO
         MVCL  R0,R14
         USING W#,R9
         SPACE 1
         ST    R13,W#SAVE+4
         ST    R9,8(,R13)
         LR    R13,R9
         SPACE 1
         CLI   2(R11),C'D'         DELETE REQUEST?
         BE    DLT                 BIF DELETE REQUEST
         SPACE 1
         BAL   R10,MSGI           INIT WTO AREA
         LA    R3,W#BLDL1         @ FIRST BLDL ENTRY
         LA    R4,BLDLLEN         LENGTH OF EACH BLDL ENTRY
         LA    R6,MODS#           NUMBER OF BLDL ENTRIES
         STH   R6,W#BLDL           NUMBER OF BLDL ENTRIES TO BLDL PFX
         STH   R4,W#BLDL+2          AND LENGTH OF EACH ENTRY
         BCTR  R6,0               NUMBER OF ENTRIES - 1
         MH    R6,W#BLDL+2        OFFSET OF LAST ENTRY
         LA    R6,W#BLDL1(R6)     @ OF LAST ENTRY (BXH LIMIT)
         SR    R3,R4              @ FIRST ENTRY - 1 FOR BXH
         LR    R5,R3              INIT EMPTY BLDL LIST
         STM   R3,R4,W#BXH         SAVE BXH REGS
         ST    R6,W#BXH+8
         SPACE 1
         LA    R7,MODS            @ MODULE LIST
ADD1     MVC   W#MOD(8),0(R7)     NAME TO ADD TO BLDL LIST
         L     R3,W#BXH           @ FIRST BLDL ENTRY - 1 ENTRY
ADD2     BXH   R3,R4,ADD3         BIF NO MORE PREV NAMES TO CHECK
         CLC   0(8,R3),W#MOD      THIS MODULE GO HERE?
         BL    ADD2
         BE    ADD4               BIF DUPLICATE MODULE (SKIP?)
         XC    W#MOD(8),0(R3)        TO BLDL LIST
         XC    0(8,R3),W#MOD
         XC    W#MOD(8),0(R3)        TO BLDL LIST
         B     ADD2
         SPACE 1
ADD3     AR    R5,R4              BUMP TO NEW ENTRY
         CR    R5,R6              ROOM LEFT IN BLDL TABLE?
         BNH   *+8                BIF NOT OUT OF ROOM
         EX    0,*                ** LOGIC ERROR IF OUT OF ROOM **
         MVC   0(8,R5),W#MOD      NAME TO BLDL LIST
         SPACE 1
ADD4     LA    R7,8(,R7)          TO NEXT MODULE NAME
         CL    R7,=A(MODSE)
         BNH   ADD1
         SPACE 1
         BLDL  0,W#BLDL
         CH    R15,=H'4'
         BNH   *+8                BIF BLDL OK
         EX    0,*
         SPACE 1
         LM    R3,R5,W#BXH        SCAN FOR BLDL STATS
LOAD1    BXH   R3,R4,RET          BIF ALL MODULES PROCESSED
         USING PDS2,R3
         CLI   PDS2TTRP+2,0       RECORD NUMBER ZERO?
         BE    LOADER1            BIF BLDL FAILED FOR MODULE
         TM    PDS2ATR,PDS2REUS+PDS2RENT  RENT OR REUS
         BZ    LOADER2            BIF NOT REUS OR RENT
         LOAD  EPLOC=PDS2         LOAD THE MODULE
         B     LOAD1               AND CONTINUE
LOADER1  MVC   W#WTO+WTO#M+9(11),=C'BLDL FAILED'
         B     LOADERR
LOADER2  MVC   W#WTO+WTO#M+9(16),=C'NOT RENT OR REUS'
LOADERR  MVC   W#WTO+WTO#M(8),PDS2    MODULE NAME TO MSG
         BAL   R10,MSG
         B     LOAD1
         DROP  R3
         SPACE 1
RET      L     R13,4(,R13)        @ CALLERS SAVEAREA
         L     R2,=A(W#L+(MODS#*BLDLLEN))
         FREEMAIN R,LV=(R2),A=(R9) FREE W# AREA / BLDL LIST
         LM    R14,R12,12(R13)    RESTORE CALLERS REGS
         BR    R14
         SPACE 1
DLT      LA    R3,MODS            @ MODULE NAME LIST
DLT1     DELETE EPLOC=(R3)         DELETE EACH MODULE
         LA    R3,8(,R3)           @ NEXT MODULE
         CL    R3,=A(MODSE)       ALL MODULES DELETED
         BNH   DLT1               BIF MORE TO DO
         B     RET
         SPACE 1
MSG      WTO   MF=(E,W#WTO)
MSGI     MVC   W#WTO(WTOL),WTO
         BR    R10
         SPACE 1
         DROP  R12                MOVELOD
         DROP  R9                 W#
         EJECT ,
BLDLLEN  EQU   80                 LENGTH OF EACH BLDL ENTRY
WTO      WTO   'OAC9999I (MOVELOAD) -                                  X
                       ',ROUTCDE=(11),MF=L
WTOL     EQU   *-WTO
WTO#M    EQU   4+22
         SPACE 1
MODS     DC    CL8'IEHMOVE '
         DC    CL8'IEHMVERA'
         DC    CL8'IEHMVERD'
         DC    CL8'IEHMVESA'
         DC    CL8'IEHMVESC'
         DC    CL8'IEHMVESE'
         DC    CL8'IEHMVESH'
         DC    CL8'IEHMVESI'
         DC    CL8'IEHMVESJ'
         DC    CL8'IEHMVESK'
         DC    CL8'IEHMVESL'
         DC    CL8'IEHMVESM'
         DC    CL8'IEHMVESN'
         DC    CL8'IEHMVESO'
         DC    CL8'IEHMVESP'
         DC    CL8'IEHMVESQ'
         DC    CL8'IEHMVESR'
         DC    CL8'IEHMVESS'
         DC    CL8'IEHMVEST'
         DC    CL8'IEHMVESU'
         DC    CL8'IEHMVESV'
         DC    CL8'IEHMVESX'
         DC    CL8'IEHMVESY'
         DC    CL8'IEHMVESZ'
         DC    CL8'IEHMVETA'
         DC    CL8'IEHMVETG'
         DC    CL8'IEHMVETJ'
         DC    CL8'IEHMVETL'
         DC    CL8'IEHMVXSE'
MODSE    DC    CL8'IEHMVXSF'
MODS#    EQU   (*-MODS)/8
         SPACE 1
R0       EQU   0                  REGISTER EQUATES
R1       EQU   1                  REGISTER EQUATES
R2       EQU   2                  REGISTER EQUATES
R3       EQU   3                  REGISTER EQUATES
R4       EQU   4                  REGISTER EQUATES
R5       EQU   5                  REGISTER EQUATES
R6       EQU   6                  REGISTER EQUATES
R7       EQU   7                  REGISTER EQUATES
R8       EQU   8                  REGISTER EQUATES
R9       EQU   9                  REGISTER EQUATES
R10      EQU   10                 REGISTER EQUATES
R11      EQU   11                 REGISTER EQUATES
R12      EQU   12                 REGISTER EQUATES
R13      EQU   13                 REGISTER EQUATES
R14      EQU   14                 REGISTER EQUATES
R15      EQU   15                 REGISTER EQUATES
         SPACE 1
         IHAPDS ,
         END   ,
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
//             PARM='NORENT,LIST,LET,MAP'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME MOVELOAD(R)
//*
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=MOVELOAD
//MOVELOAD  JOB  (SETUP),
//             'Run MOVELOAD',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: MOVELOAD
//*
//* Desc: Run the MOVELOAD program
//*
//********************************************************************
//MOVELOD EXEC PGM=MOVELOD,PARM='POWER=9'
//SYSPRINT DD  SYSOUT=*
 MOVE TO=3350=USER50,DSNAME=IBMUSER.TEST.PDS
//VPUB001  DD  UNIT=3390,VOL=SER=PUB001,DISP=SHR
//PROTECT  DD  UNIT=3350,VOL=SER=WORK50,DISP=SHR
//SYSUT1   DD  UNIT=3380,VOL=SER=WORK80,DISP=SHR
//LOCKSTEP EXEC PGM=IEFBR14,COND=(0,LE)
//DSNLOCK  DD  DISP=OLD,DSN=IBMUSER.TEST.PDS
//