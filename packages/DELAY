//DELAY JOB (JOB),
//             'INSTALL DELAY',
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
PASS     TITLE 'DELAY - - - SET AN STIMER WITH USER SPECIFIED VALUE'
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*     This program accepts the PARM from its EXECute statement,
*
*  and delays the JOB for the requested number of seconds.
*
*  For problems or questions, contact: somitcw@erols.com
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE
DELAY    CSECT ,
* DELAY    AMODE 31               Addressing mode of 31
* DELAY    RMODE 24               Residency mode of 24 ( for VS/COBOL )
         SAVE  (14,12),,'DELAY.&SYSDATE..&SYSTIME'    Save Reg.s
         LR    R12,R15            Load the program's base register
         USING DELAY,R12          Give the assembler the base reg.
         L     R2,0(,R1)          Load/save the address of the PARM
         LA    R0,WORKSIZE        Load work area size for GETMAIN
         GETMAIN R,LV=(0)         Get storage for work space
         XC    0(WORKSIZE,R1),0(R1)  Clear DSECT to binary zeroes
         ST    R1,8(,R13)         Connect old and new save areas
         ST    R13,4(,R1)         Connect old and new save areas
         LR    R13,R1             Connect old and new save areas
         USING WORKAREA,R13       Tell assembler of save area base reg.
         LH    R3,0(,R2)          Load the length of the PARM
         LTR   R3,R3              See if PARM length is zero
         BZ    NOPARM             No PARM, go abend
         LA    R4,15              Load maximum PARM size
         CLR   R3,R4              See if PARM is over 15 bytes
         BH    LONGPARM           If over 15 bytes, go abend
         SLR   R4,R3              Find relative displacement to move to
         LA    R4,PARMEXPD(R4)    Find absolute displacement to move to
         BCTR  R3,0               Drop length by 1 for EXecute of MVC
         EX    R3,MVC             Move the PARM to the delay field
         PACK  PARMCNVT,PARMEXPD  Change display data to packed format
         CVB   R3,PARMCNVT        Change packed data to binary data
         M     R2,HUNDRED         Find number of hundredths of seconds
         ST    R3,TIME            Set time for STIMER to delay
         STIMER WAIT,BINTVL=TIME  Wait for requested number of seconds
         LA    R0,WORKSIZE        Load work area size for FREEMAIN
         LR    R1,R13             Save work area location for FREEMAIN
         L     R13,4(,R1)         Restore the caller's save area addr.
         FREEMAIN R,LV=(0),A=(1)  Free work area's storage
         RETURN (14,12),RC=0      Return to caller w/retcode of zero
         SPACE 3
MVC      MVC   0(0,R4),2(R2)      Dummy move for above EXecute
         EJECT ,
NOPARM   DS    0H
         WTO   'DELAY - PARM missing, JOB abending',ROUTCDE=(1,11)
         ABEND 1111               Abend, no dump
         SPACE 3
LONGPARM DS    0H
         WTO   'DELAY - PARM greater than 15 bytes not allowed',       C
               ROUTCDE=(1,11)
         ABEND 1112               Abend, no dump
         EJECT ,
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*     Constants, Register equates, and literals
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 2
HUNDRED  DC    F'100'             Constant for multiply above
         SPACE 2
R0       EQU   0                  Register equate for register 0
R1       EQU   1                  Register equate for register 1
R2       EQU   2                  Register equate for register 2
R3       EQU   3                  Register equate for register 3
R4       EQU   4                  Register equate for register 4
R5       EQU   5                  Register equate for register 5
R6       EQU   6                  Register equate for register 6
R7       EQU   7                  Register equate for register 7
R8       EQU   8                  Register equate for register 8
R9       EQU   9                  Register equate for register 9
R10      EQU   10                 Register equate for register 10
R11      EQU   11                 Register equate for register 11
R12      EQU   12                 Register equate for register 12
R13      EQU   13                 Register equate for register 13
R14      EQU   14                 Register equate for register 14
R15      EQU   15                 Register equate for register 15
         LTORG ,                  In case someone uses literals
         EJECT ,
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
*      DSECTs follow
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
         SPACE 2
WORKAREA DSECT ,
         SPACE 2
SAVEAREA DS    18F                Register save area
         SPACE 2
PARMCNVT DS    D                  PARM convert area
         SPACE 2
TIME     DS    F                  Delay in hundredths/seconds in binary
         SPACE 2
PARMEXPD DS    CL15               PARM expand area
         SPACE 2
WORKSIZE EQU   *-SAVEAREA
         SPACE 3
         END   ,                  The end of the program
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
//             PARM='LIST,LET,MAP'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.LINKLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 ALIAS SLEEP
 NAME DELAY(R)
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=DELAY
//DELAY  JOB  (SETUP),
//             'Run DELAY',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//********************************************************************
//*
//* Name: DELAY
//*
//* Desc: Run the DELAY program
//*
//********************************************************************
//DELAY11 EXEC PGM=DELAY,PARM=11   Delay 11 seconds
//
@@