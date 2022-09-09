//FINDSRCH JOB (JOB),
//             'INSTALL FINDSRCH',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//* -------------------------------------------------------*
//* *  FINDSRCH for MVS3.8J TSO / Hercules                 *
//* *                                                      *
//* *  JOB: $INST04                                        *
//* *       Install FINDSRCH Programs                      *
//* *                                                      *
//* *  - Install libraries marked...                       *
//* *    - Search for '<--TARGET'                          *
//* *    - Update install libraries per your               *
//* *      installation standard                           *
//* *                                                      *
//* -------------------------------------------------------*
//*
//* -------------------------------------------------------*
//* *                                                      *
//* *  PROC: ASMLKED                                       *
//* *       Assembler Link-Edit                            *
//* *                                                      *
//* -------------------------------------------------------*
//ASML     PROC 
//*
//ASM      EXEC PGM=IFOX00,
//             PARM='NODECK,LOAD,RENT,TERM,XREF'
//SYSGO    DD  DSN=&&LOADSET,DISP=(MOD,PASS),SPACE=(CYL,(1,1)),
//             UNIT=VIO,DCB=(DSORG=PS,RECFM=FB,LRECL=80,BLKSIZE=800)
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//         DD  DSN=SYS2.MACLIB,DISP=SHR          ** YREG  **
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DSN=NULLFILE
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSUT2   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSUT3   DD  UNIT=VIO,SPACE=(CYL,(6,1))
//SYSIN    DD  DUMMY
//*
//LKED     EXEC PGM=IEWL,PARM='MAP,LIST,LET,RENT,XREF',
//             COND=(0,NE,ASM)
//SYSLIN   DD  DSN=&&LOADSET,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DUMMY
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(5,2))
//SYSIN    DD  DUMMY
//*
//         PEND
//*
//* -------------------------------------------------------*
//* *  Assemble Link-Edit FINDSRCH to ISPLLIB              *
//* -------------------------------------------------------*
//FINDSRCH EXEC  ASML,
//         PARM.LKED='MAP,LIST,LET,RENT,XREF,REUS,REFR'
//ASM.SYSIN DD DATA,DLM=@@
         MACRO
         EXIT  &RC=,&R=
.*
.*--------------------------------------------------------------------
.*
.*       MACRO = EXIT
.*
.*       AUTHOR = VINH VU
.*
.*       DATE-WRITTEN = 09/02/81
.*
.*       PURPOSE = TO EXIT FROM A MODULE
.*             THIS HAS TO BE USED IN CONJUNCTION WITH MACRO
.*             SET WHICH IS THE INITIALIZATION MACRO.
.*
.*       EXAMPLES :
.*
.*         *   EXIT RC=10
.*
.*             THIS WILL GENERATE A RETURN CODE OF 10. NO FREEMAIN
.*                WILL BE DONE
.*
.*         *   EXIT R=R9
.*
.*             THIS WILL GENERATE RETURN CODE FROM CONTENTS OF REG 9.
.*                A FREEMAIN WILL BE DONE ALSO ASSUMING ADDRESS OF
.*                GETMAINED AREA IS IN REG13 (DONE IN MACRO SET).
.*
.*--------------------------------------------------------------------
.*
         GBLC  &SETEXIT
         AIF   ('&SETEXIT' EQ '').NOFREE
         LR    R1,R13              GET ADDR OF GMAREA INTO R1
         L     R13,4(,R13)         GET PREVIOUS SAVE AREA
         FREEMAIN R,LV=&SETEXIT,A=(R1)
         AGO   .FREEIT
.NOFREE  ANOP
         L     R13,4(,R13)         GET PREVIOUS SAVE AREA
.FREEIT  ANOP
         AIF   (T'&RC EQ 'O').CKR
         LA    R15,&RC             GET RETURN CODE
         AGO   .THRU
.CKR     ANOP
         AIF   (T'&R EQ 'O').NONE
         LR    R15,&R              GET RETURN CODE
         AGO   .THRU
.NONE    ANOP
         SR    R15,R15             RETURN CODE IS ZERO
.THRU    ANOP
         L     R14,12(0,R13)       GET R14
         LM    R0,R12,20(R13)      GET REG 0 - 12
         BR    R14                 RETURN TO CALLER
         MEND
         MACRO
         SET   &BASE,&LV=,&DSECT=,&REG=
.*
.*--------------------------------------------------------------------
.*
.*       MACRO = SET
.*
.*       AUTHOR = VINH VU
.*
.*       DATE-WRITTEN = 08/31/81
.*
.*       PURPOSE = TO SET UP BASE REGS FOR A SOURCE MODULE.
.*             IF LV IS OMITTED, PROGRAM IS ASSUMED NOT TO BE
.*             RE-ENTRANT. USE EXIT MACRO IN CONJUNCTION WITH THIS
.*             MACRO.
.*             FOR RE-ENTRANT MODULE, 18F AS SAVE AREA WILL
.*             BE ASSUMED TO BE FIRST. PAY ATTENTION TO THIS
.*             WHEN YOU SET UP THE DSECT.
.*
.*       EXAMPLES =
.*
.*         *   SET (R12,R11),LV=WKLENG,DSECT=WKAREA
.*
.*             R12 AND R11 WILL BE BASE REGISTERS.
.*             LV WILL ASSUME A RE-ENTRANT MODULE AND THIS IS THE
.*                LENGTH OF THE GETMAIN AREA BEING OBTAINED (WHICH
.*                ALSO INCLUDES THE SAVE AREA OF 18F THAT WILL BE
.*                AT THE BEGINNING OF THE GETMAINED AREA.)
.*                THE GETMAINED DSECT SHOULD LOOK LIKE :
.*                           WKAREA    DSECT
.*                                     DS    18F
.*                           USER1     DS    CL256
.*                                     ETC   ........
.*
.*         *   SET LV=WKLENG
.*
.*             THIS ASSUMES R12 AS BASE REGISTER, A RE-ENTRANT
.*                MODULE, BUT YOU WILL HAVE TO CODE THE USING
.*                STATEMENT FOR THE GETMAIN AREA DSECT
.*
.*         *   SET
.*
.*             THIS ASSUMES A NON-RE-ENTRANT MODULE, R12 WILL BE
.*                BASE REGISTER.
.*
.*--------------------------------------------------------------------
.*
         LCLA  &C,&TEST,&ORG,&CSLENG
         LCLC  &B,&SAVE
         GBLC  &SETEXIT
&SETEXIT SETC  ''
&ORG     SETA  0
&TEST    SETA  1
         AIF   (T'&BASE EQ 'O').DEF
&C       SETA  N'&BASE
&B       SETC  '&BASE(&TEST)'
         AGO   .EQU
.DEF     ANOP
&C       SETA  1
&B       SETC  'R12'
.EQU     ANOP
         AIF   (T'&REG EQ 'O').SETREG
         AGO   .REGDONE
.SETREG  ANOP
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
*
.REGDONE ANOP
         B     28(R15)             BRANCH AROUND
&CSLENG  SETA  K'&SYSECT
         DC    AL2(&CSLENG)        LENG OF CSECT NAME
         DC    CL8'&SYSECT'        CSECT NAME
         DC    CL8'&SYSDATE'       THIS IS DATE
         DC    CL6'&SYSTIME'       THIS IS TIME
         CNOP  0,4                 GET ALIGNMENT
         STM   R14,R12,12(R13)     SAVE PREVIOUS REGS
         LR    &B,R15              GET BASE REG SETUP
         USING &SYSECT,&B          ESTABLISH ADDRESABILITY
.LOOP    ANOP
         AIF   (&TEST EQ &C).THRU
&SAVE    SETC  '&BASE(&TEST)'
&ORG     SETA  &ORG+4096
&TEST    SETA  &TEST+1
&B       SETC  '&BASE(&TEST)'
         USING &SYSECT+&ORG,&B
         LA    &B,4095(,&SAVE)
         LA    &B,1(,&B)
         AGO   .LOOP
.THRU    ANOP
         AIF   (T'&LV EQ 'O').NORENT
&SETEXIT SETC  '&LV'
         GETMAIN R,LV=&LV
         ST    R13,4(,R1)          SAVE REG13 FROM PREVIOUS
         L     R15,24(,R13)        SAVE REG 1
         ST    R1,8(,R13)          SAVE MY REG13 IN PREV SAVE AREA
         LR    R13,R1              GET MY REG 13 READY
         LR    R1,R15              GET REG1 BACK
         AIF   (T'&DSECT EQ 'O').FINAL
         USING &DSECT,R13          ESTABLISH ADDRESSABILITY
.FINAL   ANOP
         MEXIT
.NORENT  ANOP
         CNOP  0,4                 GET ALIGNMENT
         BAL   R1,*+76             BRANCH AROUND
         DS    18F                 THIS IS MY SAVE AREA
         ST    R13,4(,R1)          SAVE REG13 FROM PREVIOUS
         L     R15,24(,R13)        SAVE REG 1
         ST    R1,8(,R13)          SAVE MY REG13 IN PREV SAVE AREA
         LR    R13,R1              GET MY REG 13 READY
         LR    R1,R15              GET REG1 BACK
         MEND
FINDSRCH TITLE 'FINDSRCH --- TO SEARCH FOR AN ARGUMENT IN A PDS'        LB
*
*-------------------------------------------------------------------
*        PROGRAM - FINDSRCH (modified version of FIND)                  LB
*        PURPOSE - TO SEARCH ARGUMENT IN A PDS.
*        AUTHOR - VINH VU
*        DATE-WRITTEN - 02/82
*        Modified by Larry Belmontes 09/26/2020                         LB
*         - Renamed to FINDSRCH as to not to interfere with             LB
*           original module name (FIND)                                 LB
*         - Added three parameters for enhanced use                     LB
*           QUIET, VOL, FILE                                            LB
*        MACROS USED -
*              SET,EXIT,READ,NOTE,POINT,TPUT,TGET,GETMAIN,
*              FREEMAIN,OPEN,CLOSE,DCB,DCBD
*        NOTE -
*              THIS IS A COMMAND PROCESSOR. ALL MISSING POSITIONAL
*              PARAMETERS WILL BE PROMPTED AT TERMINAL.
*
*        COMMAND FORMAT :
*              FINDSRCH 'XXXXX' S('YYY') LOWER GROUP('TTT') QUICK       LB
*                       QUIET FILE VOL(VVVVVV)                          LB
*        WHERE :
*          *   'XXXX' = PDS TO BE SEARCHED
*          *   S - STANDS FOR STRING, 'YYY' = ARGUMENT STRING. THIS
*              IS OPTIONAL. IF NOT ENTERED, YOU WILL BE PROMPTED FOR.
*          *   LOWER = OPTIONAL KEYWORD. TO USE SEARCH ARGUMENT AS
*              ENTERED (DO NOT TURN TO UPPERCASE). THE DEFAULT IS
*              'NOT LOWER'.
*          *   GROUP = OPTIONAL KEYWORD. TO START SEARCHING ON
*              MEMBERS WITH NAME PREFIXED BY THIS.
*          *   QUICK = OPTIONAL KEYWORD. STOP THE SEARCH IN A MEMBER
*              ONCE THE CHARACTER STRING IS FOUND.  WILL PROCEED TO
*              NEXT MEMBER.
*          *   QUIET = OPTIONAL KEYWORD. STOP DISPLAY OF SEARCH         LB
*              RESULTS ON TERMINAL.                                     LB
*          *   VOL   = OPTIONAL KEYWORD. TO SPECIFY VOLUME SERIAL       LB
*              NUMBER OF PDS TO BE SEARCHED.                            LB
*          *   FILE  = OPTIONAL KEYWORD. TO SPECIFY SEQUENTIAL DATASET  LB
*              TO STORE SEARCH RESULTS.  FILE MUST BE PRE-ALLOCATED     LB
*              WITH DCB (LRECL=80, BLKSIZE=3200, RECFM=FB, DSO=PS)      LB
*-------------------------------------------------------------------
*
FINDSRCH CSECT                                                          LB
         SET   DSECT=WKAREA,LV=WKLENG
         LR    R11,R1              GET CPPL ADDR
         USING CPPL,R11            ESTAB ADDR
         XC    WKPPL,WKPPL         CLEAR OUT PPL
         LA    R4,WKPPL            GET ADDR OF PPL
         USING PPL,R4              ESTAB ADDR
         MVC   PPLUPT,CPPLUPT      GET UPT
         MVC   PPLECT,CPPLECT      GET ECT
         XC    MYECB,MYECB         CLEAR MY ECB
         LA    R1,MYECB            GET A(ECB)
         ST    R1,PPLECB           GET ECB
         MVC   PPLPCL,APCL         GET PPLPCL
         LA    R1,ANS
         ST    R1,PPLANS           GET ANS
         MVC   PPLCBUF,CPPLCBUF    GET CBUF
         CALLTSSR EP=IKJPARS,MF=(E,WKPPL)
         LTR   R15,R15
         BNZ   ABEND020
         L     R7,ANS              GET ANS
         LA    R1,0(0,R7)          CLEAR OUT TRASH
         LTR   R1,R1               CHECK IF ANS IS THERE
         BZ    ABEND020            NOP - THEN ABEND
         USING IKJPARMD,R7         ESTAB ADDR
*
         XC    FILESW,FILESW       TURN OFF 'FILE' SW                   LB
         CLI   FFILE+1,0           IS 'FILE' ENTERED ??                 LB
         BE    CKQUIET             NOP - THEN GO THRU                   LB
         MVI   FILESW,X'FF'        ELSE - TURN ON SWITCH                LB
*                                                                       LB
CKQUIET  EQU   *                                                        LB
*                                                                       LB
         XC    QUIETSW,QUIETSW     TURN OFF 'FILE' SW                   LB
         CLI   FQUIET+1,0          IS 'QUIET' ENTERED ??                LB
         BE    CKQUIK              NOP - THEN GO THRU                   LB
         MVI   QUIETSW,X'FF'       ELSE - TURN ON SWITCH                LB
*                                                                       LB
CKQUIK   EQU   *                                                        LB
         XC    QKSW,QKSW           TURN OFF 'QK' SW
         CLI   FQUICK+1,0          IS 'QK' ENTERED ??
         BE    CKLOWER             NOP - THEN GO THRU
         MVI   QKSW,X'FF'          ELSE - TURN ON SWITCH
*
CKLOWER  EQU   *
*
         MVI   UPPER,X'FF'         TURN ON SWITCH
         CLI   FLOWER+1,0          IS 'LOWER' ENTERED ?
         BE    NOLOWER             NOP - THEN GO THRU
         MVI   UPPER,X'00'         ELSE - TURN OFF SWITCH
*
NOLOWER  EQU   *
         MVC   WKDSTEST,BLANK
         LA    R1,44               GET LENG OF DSNAME
         STH   R1,WKDST            STORE IT
         TM    FPDS+6,X'80'        SOURCE NAME THERE ?
         BZ    ABEND020            NOP - THEN ABEND
         TM    FPDS+6,X'40'        SOURCE NAME HAS QUOTES ?
         BZ    PREFIXTS            NOP - THEN PREFIX IT WITH USER-ID
         L     R1,FPDS             GET POINTER TO SOURCE NAME
         LH    R2,FPDS+4           GET LENG OF SOURCE NAME
         BCTR  R2,R0               MINUS 1 FOR EX
         EX    R2,MVCFPDS          MOVE DSN
         B     CHECK               GOTO ALLOCIT
*
PREFIXTS EQU   *
         L     R1,CPPLPSCB         GET ADDR OF PSCB
         USING PSCB,R1             ESTABLISH ADDRESSABILITY
         MVC   WKDSTEST(L'PSCBUSER),PSCBUSER
         XR    R15,R15             CLEAR REG 15
         LA    R14,WKDSTEST        GET ADDR OF WKSOURCE
         IC    R15,PSCBUSRL        GET LENG OF USER-ID
         LA    R14,0(R14,R15)      GET TO END OF IT
         MVI   0(R14),C'.'         MOVE '.' IN THERE
         DROP  R1                  DROP ADDR
         L     R1,FPDS             GET POINTER TO SOURCE NAME
         LH    R2,FPDS+4           GET LENG OF SOURCE NAME
         BCTR  R2,R0               MINUS 1 FOR EX
         EX    R2,MVCPFR           MOVE DSN
         B     CHECK               GO TO ALLOCIT
*
MVCFPDS  MVC   WKDSTEST(0),0(R1)
MVCPFR   MVC   1(0,R14),0(R1)
*
*
CHECK    EQU   *
         CLI   FKEYWD+1,0          KEY WORD ENTERED ?
         BE    ALLOCIT             NOP - THEN DO THE TGET
         L     R10,STRING          ELSE - GET POINTER INTO R10
         LH    R2,STRING+4         GET LENG
         ST    R2,ARGLENG          STORE LENG
         BCTR  R2,R0               MINUS 1 FOR EX
         MVC   ARG,BLANK           BLANK OUT THIS AREA FIRST
         EX    R2,SAVEARG          SAVE THE ARGUMENT
         B     THRU                THEN GO THRU
*
ALLOCIT  EQU   *
         LA    R9,MSG1             GET MSG
         TPUT  (R9),L'MSG1         DO TPUT
         LTR   R15,R15             GOOD RET CODE ?
         BNZ   BADPUT              NOP - THEN ABEND 10
*
LOOPGET  EQU   *
         MVC   BUFFER,BLANK        BLANK THE BUFFER
         LA    R9,BUFFER           GET ADDR OF BUFFER
         TGET  (R9),L'BUFFER       DO THE GET
         CLC   BUFFER,BLANK        IS IT STILL BLANK ?
         BE    ALLOCIT             YEAH - THEN GOBACK TO LOOP
         TRT   BUFFER,QUOTE        CHECK FOR 1ST QUOTE
         BC    8,PROMPT            IF NOT FND , GO TO PROMPT
         LA    R15,BUFFER+L'BUFFER GET TO THE END
         LA    R1,1(0,R1)          GET PAST THE QUOTE
         LR    R10,R1              SAVE BEGINNING ADDRESSS
         SR    R15,R1              GET LENG
         BZ    PROMPT              IF ZERO THEN GOTO PROMPT
         BCTR  R15,R0              MINUS 1 FOR EX
         EX    R15,TRTQUOTE        DO THE SEARCH AGAIN
         BC    8,PROMPT            IF NOT FND , GO TO PROMPT
         SR    R1,R10              GET LENG OF ARGUMENT
         ST    R1,ARGLENG          SAVE ARGUMENT LENG
         BCTR  R1,R0               MINUS 1 FOR EX
         MVC   ARG,BLANK           BLANK OUT THIS AREA FIRST
         EX    R1,SAVEARG          SAVE THE ARGUMENT
         B     THRU                THEN GO THRU
*
TRTQUOTE TRT   0(0,R10),QUOTE      DO THE TRT
SAVEARG  MVC   ARG(0),0(R10)
*
PROMPT   EQU   *
         LA    R9,MSG2             GET MSG
         TPUT  (R9),L'MSG2         DO TPUT
         LTR   R15,R15             GOOD RET CODE ?
         BNZ   BADPUT              NOP - THEN ABEND 10
         B     LOOPGET             GOBACK TO GET
*
THRU     EQU   *
         CLI   UPPER,X'FF'         SHOULD TURN ON TO UPPERCASE ?
         BNE   THRU01              NOP - THEN GO THRU
         TR    ARG,TRTABLE         ELSE - TURN ON TO UPPERCASE
*
THRU01   EQU   *
*--------------
         B     THRU02              BYPASS EVERYBODY FOR NOW
*--------------
         L     R15,CPPLPSCB        GET PSCB POINTER
         TM    PSCBATR1-PSCB(R15),PSCBCTRL     OPER ON ?
         BO    THRU02              YEAH - THEN GO THRU
         MVC   TESTUSER,BLANK      TO BLANK IT FIRST
         MVC   TESTUSER(7),PSCBUSER-PSCB(R15)    ELSE - SAVE USER-ID
         XR    R14,R14             CLEAR REG 14
         IC    R14,PSCBUSRL-PSCB(R15)   GET LENG OF USER
         LA    R1,TESTUSER         GET ADDR INTO R1
         LA    R1,0(R14,R1)
         MVI   0(R1),C'.'          PUT IN PERIOD
         EX    R14,TESTDSN         THEN DO THE TEST
         BE    THRU02              IF EQ THEN LET THRU
         LA    R9,ERR3             GET MSG
         TPUT  (R9),L'ERR3         DO TPUT
         LTR   R15,R15             GOOD RET CODE ?
         BNZ   BADPUT              NOP - THEN ABEND 10
         B     ENDIT
*
TESTDSN  CLC   TESTUSER(0),WKDSTEST
MOVE     MVC   KGROUP(0),0(R1)
COMPARE  CLC   SAVEMEM(0),KGROUP
*
THRU02   EQU   *
*
         XC    GROUPSW,GROUPSW     CLEAR GROUPSW
         TM    PGROUP+6,X'80'      IS GROUP THERE ???
         BZ    THRU03              NOP - THEN FORGET IT
         MVI   GROUPSW,X'FF'       TURN ON SWITCH
         L     R1,PGROUP           ELSE - GET ITS ADDRESS
         LH    R9,PGROUP+4         GET LENG
         BCTR  R9,R0               MINUS 1 FOR EXECUTE
         ST    R9,LGROUP           SAVE LENG FOR LATER COMPARE
         EX    R9,MOVE             NOW DO THE MOVE
*
THRU03   EQU   *
         XC    ETAB,ETAB           CLEAR TABLE
         XR    R9,R9               CLEAR REG 9
         IC    R9,ARG              GET FIRST BYTE INTO TABLE
         LA    R9,ETAB(R9)         GET TO THE POINT
         MVI   0(R9),X'FF'         TURN ON STOP INDICATOR
         XC    WKDAP08,WKDAP08     CLEAR AREA
         LA    R9,WKDAP08          GET ADDR INTO R9
         USING DAPB08,R9           ESTAB ADDR
         MVC   DA08CD,=XL2'0008'
         LA    R1,WKDST            GET ADDR OF DSNAME BUFF (TEST)
         ST    R1,DA08PDSN
         MVC   DA08DDN,BLANK       BLANK OUT DDNAME
         MVC   DA08UNIT,=CL8'SYSDA'
         TM    PVOL+6,X'80'        IS VOL   THERE ???                   LB
         BZ    VOLBLNK             NOP - Use Blanks                     LB
         L     R1,PVOL             Load address of PVOL                 LB
         MVC   DA08SER,0(R1)       Use specified VOL                    LB
         B     VOLXIT              Continue                             LB
VOLBLNK  EQU   *                                                        LB
         MVC   DA08SER,BLANK       BLANK SER NO
VOLXIT   EQU   *                                                        LB
         MVC   SAVEVOLS,DA08SER    SAVE VOLSER                          LB
         MVC   DA08PSWD,BLANK      BLANK OUT PSWD
         MVC   DA08MNM,BLANK       BLANK OUT MEMBER NAME
         OI    DA08DSP1,DA08SHR    SHR
         DROP  R9
         XC    WKDAPL,WKDAPL       CLEAR AREA
         LA    R9,WKDAPL           GET ADDR INTO R9
         USING DAPL,R9             ESTAB ADDTR
         MVC   DAPLUPT,CPPLUPT     GET UPT
         MVC   DAPLECT,CPPLECT     GET ECT
         XC    MYECB,MYECB         CLEAR MYECB
         LA    R1,MYECB            GET ITS ADDR
         ST    R1,DAPLECB          GET IT INTO LIST
         MVC   DAPLPSCB,CPPLPSCB   GET PSCB
         LA    R1,WKDAP08          GET ADDR OF DAIR
         ST    R1,DAPLDAPB         GET IT INTO LIST
         LA    R1,WKDAPL           GET ADDR OF PARM LIST
         CALLTSSR EP=IKJDAIR,MF=(E,WKDAPL)
         LTR   R15,R15             TEST RET CODE
         BNZ   DAIRERR             BAD - GOTO DAIR ERR
         LA    R9,WKDAP08          GET ADDR INTO R9
         USING DAPB08,R9           ESTAB ADDR
         MVC   SAVEDDTS,DA08DDN    SAVE DD NAME FOR TEST MASTER
         TM    DA08DSO,X'02'       PDS ???
         BZ    NOTPDS              NOP - THEN ERROR
         ZAP   COUNTER,=P'0'
         ZAP   TOT,=P'0'           ZERO OUT FINAL COUNTER
         ZAP   MEMTOT,=P'0'        ZERO OUT THE MEMBER TOTAL
         DROP  R9
*
*                                                                       LB
*********************************************************************** LB
*        OPEN  OUTFIL                                                 * LB
*********************************************************************** LB
         CLI   FILESW,X'FF'        Is 'FILE' SW ON ??                   LB
         BNE   NOFILE01            No, bypass FILE IO                   LB
*                                  Yes, open file                       LB
         MVC   WOUTFIL,OUTFIL      Move DCB constant to workarea        LB
         MVC   WOPENO,OPENO        Move Open constant to workarea       LB
         LA    R9,WOUTFIL                                               LB
         OPEN  ((R9),(OUTPUT)),MF=(E,WOPENO)                            LB
         LA    R1,WOUTFIL                                               LB
         USING IHADCB,R1                                                LB
         TM    DCBOFLGS,DCBOFOPN   IS OPEN GOOD ?                       LB
         BC    8,BADOPENO          NO _ GOTO BAD OPEN FOR OUTFIL        LB
         DROP  R1                                                       LB
NOFILE01 EQU   *                                                        LB
*********************************************************************** LB
*                                                                       LB
         MVC   WMASTER,MASTER      GET DCB INTO WK AREA
         MVC   WOPENM,OPENM        MOVE OPEN LIST TO WK AREA
         LA    R1,WMASTER          GET DCB
         USING IHADCB,R1           ESTAB ADDR
         MVC   DCBDDNAM,SAVEDDTS   GET DDNAME
         DROP  R1                  DROP ADDR
         OPEN  (WMASTER),MF=(E,WOPENM)
         LA    R1,WMASTER          GET DCB
         USING IHADCB,R1           ESTAB ADDR
         TM    DCBOFLGS,DCBOFOPN   IS OPEN GOOD ?
         BC    8,BADOPEN           NO - GOTO BAD OPEN
         MVC   SAVESIZE,DCBBLKSI   SAVE REAL BLK SIZE
         MVC   SAVERECL,DCBLRECL   SAVE REAL LRECL
         MVC   DCBBLKSI,H256       BLKSIZE IS 256
         DROP  R1                  DROP ADDR
         LA    R5,DIR              GET ADD OF DIR AREA
         BAL   R9,READNXT          DO THE READ
         BAL   R9,CHECKIT          DO THE CHECK
         NOTE  WMASTER             GET CURR TTR
         LA    R1,1(0,R1)          BUMP IT BY 1
         ST    R1,TTR              SAVE IT
         LA    R1,WMASTER          GET DCB ADDR
         USING IHADCB,R1           ESTAB ADDR
         LA    R2,LOOPPROC         GET ADDRT OF EODAD
         STCM  R2,B'0111',DCBEODA  STORE INTO DCB
         DROP  R1                  DROP ADDR
         B     LOOPCK              GOTO LOOP
*
READIR   EQU   *
*
*--------------
*        THIS IS A LOOP TO READ DIR
*--------------
*
         LA    R1,WMASTER          GET DCB ADDR
         USING IHADCB,R1           ESTAB ADDR
         MVC   DCBBLKSI,H256       BLKSIZE IS 256
         MVC   SAVEODAD,DCBEODA    SAVE EODAD
         LA    R2,ENDRPT           GET NEW EODAD
         STCM  R2,B'0111',DCBEODA
         DROP  R1                  DROP ADDR
         POINT WMASTER,TTR         GET TO NEW POS
         LA    R5,DIR              GET RECORD AREA
         BAL   R9,READNXT          THEN READ
         BAL   R9,CHECKIT          AND CHECK THE READ
         LA    R1,WMASTER          GET DCB ADDR
         USING IHADCB,R1           ESTAB ADDR
         MVC   DCBEODA(3),SAVEODAD GET OLD EODAD BACK
         DROP  R1                  DROP R1
         NOTE  WMASTER
         LA    R1,1(0,R1)          BUMP IT BY 1
         ST    R1,TTR              SAVE THE TTR FOR NEXT TIME
*
LOOPCK   EQU   *
         LA    R1,DIR              GET DIR ADDR
         LH    R2,DIR              GET THE BYTES USED
         AR    R1,R2               GET ENDING ADDR
         ST    R1,SAVEEND          SAVE IT
         LA    R5,2                GET R5 AS 1ST BASE TO MEM NAME
         ST    R5,SAVECURR         SAVE CURR POS
*
LOOPPROC EQU   *
*
*--------------
*        THIS IS A LOOP TO CHECK DIR ENTRY FOR EACH MEMBER
*--------------
*
         CP    COUNTER,=P'0'       IS COUNTER GREATER THAN 0
         BE    LOOPPR1             ZERO ? THEN GO THRU
         AP    TOT,COUNTER         ADD COUNTER TO FINAL TOTAL
         AP    MEMTOT,=P'1'        ADD 1 TO MEMBER TOTAL
         MVC   BUFFER,BLANK        CLEAR BUFFER
         MVC   BUFFER(L'MSG4),MSG4 GET MSG
         MVC   BUFFER+44(L'SAVEMEM),SAVEMEM
         MVC   BUFFER+55(L'PAT),PAT
         ED    BUFFER+55(L'PAT),COUNTER+2
*                                                                       LB
*********************************************************************** LB
*        PUT   OUTFIL - DETAILS for member totals                     * LB
*********************************************************************** LB
         CLI   FILESW,X'FF'        IS 'FILE' SW ON ??                   LB
         BNE   NOFILE02            NO, BYPASS FILE IO                   LB
         MVC   PREC,BLANK          CLEAR PRINT RCD                      LB
         MVI   PRCDTYP,C'2'        Member Details RCD                   LB
         MVC   PDSN,WKDSTEST                                            LB
         MVC   PVOLSER,SAVEVOLS                                         LB
         MVC   PDSO,=C'PO '                                             LB
         MVC   PMBR,SAVEMEM        Member Name                          LB
         MVC   PFNDCNT,BUFFER+56   Total Occurances                     LB
         LA    R9,WOUTFIL                                               LB
         PUT   (R9),PREC           Write to OUTFIL                      LB
NOFILE02 EQU   *                                                        LB
*********************************************************************** LB
*                                                                       LB
         CLI   QUIETSW,X'FF'       IS 'QUIET' SW ON ??                  LB
         BE    LOOPPR1             YES, BYPASS TERMINAL IO              LB
         LA    R9,BUFFER           GET MSG
         TPUT  (R9),L'BUFFER       DO TPUT
         LTR   R15,R15             GOOD RET CODE ?
         BNZ   BADPUT              NOP - THEN ABEND 10
*
LOOPPR1  EQU   *
         ZAP   COUNTER,=P'0'       ZERO OUT COUNTER
         LA    R1,DIR              GET DIR ADDTR
         L     R5,SAVECURR         GET CURR DISP
         AR    R1,R5               GET TO CURR POS
         C     R1,SAVEEND          CHECK AGST ENDING ADDR
         BNL   READIR              IF END - READ ANOTHER DIR BLK
         CLC   0(8,R1),=8XL1'FF'   IS THIS LAST MEM
         BE    ENDRPT              YES - GOTO ENDIT
         XR    R2,R2               CLEAR REG 2
         IC    R2,11(R1)           GET NUM
         N     R2,=F'31'           CLEAN OUT TRASH
         AR    R2,R2               DOUBLE LENGTH
         LA    R2,12(0,R2)         PLUS MEM NAME AND MISC
         A     R2,SAVECURR         ADD TO CURR DISP
         ST    R2,SAVECURR         SAVE THIS CURR DISP
*
GOON     EQU   *
         LR    R2,R1               R2 HAS GOOD ADDR
         MVC   SAVEMEM,0(R2)       SAVE MOD NAME
*
         CLI   GROUPSW,X'FF'       GROUP SW ON ???
         BNE   GOON1               NOP - THEN NO CHECK
         L     R15,LGROUP          GET LENG FOR COMPARE
         EX    R15,COMPARE         DO THE COMPARE
         BL    LOOPPR1             LOW - THEN GO BACK
         BH    ENDRPT              HIGH - THEN TERMINATE
*
GOON1    EQU   *
         XR    R1,R1               CLEAR REG1
         ICM   R1,B'1110',8(R2)    GET TTR
         ST    R1,WKTTR            SAVE MEM TTR
         LA    R1,WMASTER          GET DCB ADDR
         USING IHADCB,R1           ESTAB ADDR
         MVC   DCBBLKSI,SAVESIZE   GET REAL BLKSIZE
         MVC   DCBLRECL,SAVERECL   GET REAL LRECL
         DROP  R1                  DROP ADDR
         POINT WMASTER,WKTTR
         LA    R5,RECORD           GET REC ADDR INTO R5
         XC    SW,SW               CLEAR BLK SWITCH
         ZAP   COUNTER,=P'0'       MOVE 0 TO COUNTER
*
FIND0010 EQU   *
         BAL   R9,READNXT          DO THE READ
         BAL   R9,CHECKIT          DO THE CHECK
         LA    R8,RECORD           GET ADDR OF RECORD
         L     R1,WREADM+16        GET IOB ADDR
         LH    R15,SAVESIZE        GET SAVED SIZE
         SH    R15,14(R1)          GET RECORD SIZE HERE
         STH   R15,SAVECNT         SAVE IT
         LA    R14,RECORD(R15)     GET ENDING ADDR
         ST    R14,SAVEREC         SAVE IT
         TM    SW,NEXTBLK          SHOULD CHECK THIS BLK COMPARE ?
         BZ    FIND0020            NOP - THEN KEEP ON
         NI    SW,X'FF'-NEXTBLK    TURN OFF SW
         LH    R14,SAVECOMP        GET SAVE COMP
         LA    R7,ARG(R14)         GET ADDR OF THE REST OF CONSTANT
         XR    R1,R1               CLEAR REG 1
         L     R1,ARGLENG          GET LENG OF CONSTANT
         SR    R1,R14              R1 HAS RESIDUAL LENG
         CR    R15,R1              CHECK SAVECNT
         BL    FIND0020            IS LESS THEN KEEP ON
         BCTR  R1,R0               MINUS 1 FOR EX
         EX    R1,CKREST           DO THE COMPARE
         BNE   FIND0015            IF NOT EQ THEN GO THRU
         AP    COUNTER,=P'1'       ADD 1 TO COUNTER
         CLI   QKSW,X'FF'          IS 'QK' SW ON ??
         BE    LOOPPROC            YEAH - THEN BYE BYE
*
FIND0015 EQU   *
         LA    R8,1(0,R8)          ELSE - RESET R8
         BCTR  R15,R0              SUBTRACT SAVECNT BY 1
         STH   R15,SAVECNT         AND RESAVE IT
*
FIND0020 EQU   *
         XR    R15,R15             CLEAR R15
         ICM   R15,B'0011',SAVECNT GET SAVED COUNT
         BZ    FIND0010            IF ZERO THEN GOBACK TO READ
         CH    R15,H256            IS IT GREATER THAN 256
         BNH   FIND0030            NOP - THEN GO THRU
         SH    R15,H256            ELSE - SUBTRACT IT BY 256
         STH   R15,SAVECNT         THEN SAVE IT FOR NEXT TIME
         LH    R15,H256            LOAD 256 FOR TRT
         B     FIND0040            THEN GOTO DO TRT
*
FIND0030 EQU   *
         XC    SAVECNT,SAVECNT     ZERO OUT SAVE CNT
*
FIND0040 EQU   *
         BCTR  R15,R0              MINUS 1 FOR EX
         EX    R15,TRTEE           CHECK FOR X'EE'
         BC    8,FIND0070          NOT FND - THEN BUMP ADDR
         L     R14,SAVEREC         GET ENDING ADDR
         SR    R14,R1              SUBTRACT FOR LENG
         L     R15,ARGLENG         GET ARG LENG
         CR    R14,R15             CHECK AGSNT ARG LNG
         BNL   FIND0060            IF HIGH THEN SKIP THIS
         STH   R14,SAVECOMP        SAVE THIS LENG
         BCTR  R14,R0              MINUS 1 FOR EX
         EX    R14,CLCEE           DO THE COMPARE
         BNE   FIND0050            IS NOT EQ THEN GO ON
         OI    SW,NEXTBLK          TURN ON SW FOR NEXT BLK COMPARE
         B     FIND0010            GOBACK TO READ
*
FIND0050 EQU   *
         LA    R8,1(0,R1)          ELSE - RESET R8
         L     R15,SAVEREC         GET ENDING ADDR
         SR    R15,R8              GET RESIDUAL LENG
         STH   R15,SAVECNT         SAVE IT
         B     FIND0020            GOBACK TO LOOP
*
FIND0060 EQU   *
         L     R3,ARGLENG          GET ARG LENG
         BCTR  R3,R0               MINUS 1 FOR EX
         EX    R3,CLCEE            DO THE COMPARE
         BNE   FIND0065            IF NOT MATCH - THEN GO THU
         AP    COUNTER,=P'1'       ADD 1 TO COUNTER
         CLI   QKSW,X'FF'          IS 'QK' SW ON ??
         BE    LOOPPROC            YEAH - THEN BYE BYE
*
FIND0065 EQU   *
         LA    R8,1(0,R1)          ELSE - RESET R8
         L     R15,SAVEREC         GET ENDING ADDR
         SR    R15,R8              GET RESIDUAL LENG
         STH   R15,SAVECNT         SAVE IT
         B     FIND0020            GOBACK TO LOOP
*
FIND0070 EQU   *
         LA    R8,1(R15,R8)        GET TO NEXT ADDR
         B     FIND0020            GOBACK TO LOOP
*
TRTEE    TRT   0(0,R8),ETAB
CLCEE    CLC   0(0,R1),ARG
CKREST   CLC   0(0,R8),0(R7)
*
*
READNXT  EQU   *
*
*--------------
*        AT ENTRY TO THIS ROUTINE, R5 WILL POINT TO THE AREA
*        FOR THE RECORD TO BE READ INTO
*--------------
*
         MVC   WREADM,READM        MOVE READ LIST TO WK AREA
         READ  WREADM,SF,WMASTER,(R5),'S',MF=E
         BR    R9
         SPACE 3
*
CHECKIT  EQU   *
*
*--------------
*        THIS IS TO CHECK AFTER ANY BSAM I/O
*        WREADM WILL BE THE DECB TO BE CHECKED ON
*--------------
*
         CHECK WREADM
         BR    R9
         EJECT
*
DAIRERR  EQU   *
         ST    R15,DAIRRET         SAVE R15
         LA    R9,ERR2             GET MSG
         TPUT  (R9),L'ERR2         DO TPUT
         LTR   R15,R15             GOOD RET CODE ?
         BNZ   BADPUT              NOP - THEN ABEND 10
         LA    R9,WKDERR           GET ADDR OF DAIRFAIL PARM
         USING DFDSECTD,R9         ESTAB ADDR
         LA    R15,WKDAPL
         ST    R15,DFDAPLP
         LA    R15,DAIRRET
         ST    R15,DFRCP
         LA    R15,HEXZEROS
         ST    R15,DFJEFF02
         LA    R15,DAIRBYTE
         ST    R15,DFIDP
         LR    R1,R9               GET R1 SET UP
         MVC   WLINKIT,LINKIT      GET LINK LIST FORM
         LINK  SF=(E,WLINKIT)
         LTR   R15,R15             RETN CODE GOOD ?
         BZ    ENDIT               YEAH - THEN THRU
         ABEND 30                  ELSE ABEND 30
*
ABEND020 EQU   *
         LA    R2,ERR1             GET BUFFER ADDR
         TPUT  (R2),L'ERR1         DO TPUT
         LTR   R15,R15             TEST R15
         BNZ   BADPUT              NO GOOD - GOTO BADTPUT
         ABEND 20
*
NOTPDS   EQU   *
         LA    R2,ERR4             GET BUFFER ADDR
         TPUT  (R2),L'ERR4         DO TPUT
         LTR   R15,R15             TEST R15
         BNZ   BADPUT              NO GOOD - GOTO BADTPUT
         B     ENDIT               ELSE - GO TO EXIT
*
BADOPEN  EQU   *
         MVC   BUFFER,MSG3         GET MSG
         LA    R2,BUFFER           GET BUFFER ADDR
         TPUT  (R2),EIGHTY         DO TPUT
         LTR   R15,R15             TEST R15
         BC    7,BADPUT            NO GOOD - GOTO BADTPUT
         B     ENDIT               ELSE - GOTO EXIT
*                                                                       LB
BADOPENO EQU   *                                                        LB
         MVC   BUFFER,MSG3O        GET MSG                              LB
         LA    R2,BUFFER           GET BUFFER ADDR                      LB
         TPUT  (R2),EIGHTY         DO TPUT                              LB
         LTR   R15,R15             TEST R15                             LB
         BC    7,BADPUT            NO GOOD - GOTO BADTPUT               LB
         B     ENDIT               ELSE - GOTO EXIT                     LB
         EJECT
*
*
*
ENDRPT   EQU   *
         CP    COUNTER,=P'0'       IS COUNTER GREATER THAN 0
         BE    ENDRPT1             ZERO ? THEN GO THRU
         MVC   BUFFER,BLANK        CLEAR BUFFER
         MVC   BUFFER(L'MSG4),MSG4 GET MSG
         MVC   BUFFER+44(L'SAVEMEM),SAVEMEM
         MVC   BUFFER+55(L'PAT),PAT
         ED    BUFFER+55(L'PAT),COUNTER+2
*                                                                       LB
*********************************************************************** LB
*        PUT   OUTFIL - DETAILS for member totals                     * LB
*********************************************************************** LB
         CLI   FILESW,X'FF'        Is 'FILE' SW ON ??                   LB
         BNE   NOFILE03            No, bypass FILE IO                   LB
         MVC   PREC,BLANK          Yes, write to file                   LB
         MVI   PRCDTYP,C'2'        Member Details RCD                   LB
         MVC   PDSN,WKDSTEST       PDS Name                             LB
         MVC   PVOLSER,SAVEVOLS    Volser No                            LB
         MVC   PDSO,=C'PO '        DSO of PO                            LB
         MVC   PMBR,SAVEMEM        Member Name                          LB
         MVC   PFNDCNT,BUFFER+56   Total Occurances                     LB
         LA    R9,WOUTFIL                                               LB
         PUT   (R9),PREC           Write to OUTFIL                      LB
NOFILE03 EQU   *                                                        LB
*********************************************************************** LB
*                                                                       LB
         CLI   QUIETSW,X'FF'       IS 'QUIET' SW ON ??                  LB
         BE    ENDRPT1             YES, BYPASS TERMINAL IO              LB
         LA    R9,BUFFER           GET MSG
         TPUT  (R9),L'BUFFER       DO TPUT
         LTR   R15,R15             GOOD RET CODE ?
         BNZ   BADPUT              NOP - THEN ABEND 10
*
ENDRPT1  EQU   *
         CLI   QUIETSW,X'FF'       IS 'QUIET' SW ON ??                  LB
         BE    NOTPUT00            YES, BYPASS TERMINAL IO              LB
         MVC   BUFFER,BLANK        CLEAR BUFFER
         LA    R9,BUFFER           GET MSG
         TPUT  (R9),L'BUFFER       DO TPUT
         LTR   R15,R15             GOOD RET CODE ?
         BNZ   BADPUT              NOP - THEN ABEND 10
NOTPUT00 EQU   *                                                        LB
         MVC   BUFFER,BLANK        CLEAR BUFFER
         MVC   BUFFER(L'MSG5),MSG5 GET MSG
         MVC   BUFFER+7(L'PAT1),PAT1
         ED    BUFFER+7(L'PAT1),MEMTOT+3
         MVC   BUFFER+55(L'PAT),PAT
         ED    BUFFER+55(L'PAT),TOT+2
*                                                                       LB
*********************************************************************** LB
*        PUT   OUTFIL - TOTALS                                        * LB
*********************************************************************** LB
         CLI   FILESW,X'FF'        IS 'FILE' SW ON ??                   LB
         BNE   NOFILE04            NO, BYPASS FILE IO                   LB
         CP    MEMTOT,=P'0'        IS MEMTOT  EQUAL TO     0            LB
         BNE   NO0RCD              NO, BYPASS RECORD WITH DSN           LB
*                                  YES, WRITE RECROD WITH DSN           LB
         MVC   PREC,BLANK          CLEAR PRINT RCD                      LB
         MVI   PRCDTYP,C'2'        Member DSN RCD                       LB
         MVC   PDSN,WKDSTEST                                            LB
         MVC   PVOLSER,SAVEVOLS                                         LB
         MVC   PMBR,=C'>*None*<'   Member Name                          LB
**       MVI   PFNDCNT+6,C'0'      Total Occurances = Zero              LB
         LA    R9,WOUTFIL                                               LB
         PUT   (R9),PREC           Write to OUTFIL                      LB
         B     NOFILE04                                                 LB
NO0RCD   EQU   *                                                        LB
         MVC   PREC,BLANK          CLEAR PRINT RCD                      LB
         MVI   PRCDTYP,C'2'        Member DSN RCD                       LB
         MVC   PDSN,WKDSTEST                                            LB
         MVC   PVOLSER,SAVEVOLS    Volser No                            LB
         MVC   PMBR,=C' MBRS->>'   Member Name                          LB
         MVC   PFNDCNT+1(L'PAT1),BUFFER+7  Member Total                 LB
         LA    R9,WOUTFIL                                               LB
         PUT   (R9),PREC           Write to OUTFIL                      LB
         MVC   PMBR,=C' HITS->>'   Member Name                          LB
         MVC   PFNDCNT+0(L'PAT-1),BUFFER+56  Total Occurances           LB
         LA    R9,WOUTFIL                                               LB
         PUT   (R9),PREC           Write to OUTFIL                      LB
*        B     NOFILE04                                                 LB
                                                                        LB
         MVC   PREC,BLANK          YES, WRITE TO FILE                   LB
         MVC   PREC(36),=C'3 XXXXX Members, Total Found XXXXXXX'        LB
         MVC   PREC+1(L'PAT1),BUFFER+7  Member Total                    LB
         MVC   PREC+28(L'PAT),BUFFER+55 Total Occurances                LB
         LA    R9,WOUTFIL                                               LB
         PUT   (R9),PREC           WRITE TO OUTFIL                      LB
*        LTR   R15,R15             GOOD IO ?                            LB
*        BNZ   BADPUT11            NOP - THEN ABEND 11                  LB
NOFILE04 EQU   *                                                        LB
*********************************************************************** LB
*                                                                       LB
         CLI   QUIETSW,X'FF'       IS 'QUIET' SW ON ??                  LB
         BE    NONOTES             YES, BYPASS TERMINAL IO              LB
         LA    R9,BUFFER           GET MSG
         TPUT  (R9),L'BUFFER       DO TPUT
         LTR   R15,R15             GOOD RET CODE ?
         BNZ   BADPUT              NOP - THEN ABEND 10
         CLI   QKSW,X'FF'          'QK' SWITCH ON ??
         BNE   NONOTES             NO - THEN NO NOTES DISPLAYED
         MVC   BUFFER,BLANK        CLEAR BUFFER
         MVC   BUFFER(L'MSG5A),MSG5A GET MSG
         LA    R9,BUFFER           GET MSG
         TPUT  (R9),L'BUFFER       DO TPUT
         LTR   R15,R15             GOOD RET CODE ?
         BNZ   BADPUT              NOP - THEN ABEND 10
*
NONOTES  EQU   *
         MVC   WCLOSEM,CLOSEM      MOVE CLOSE LIST TO WK AREA
         CLOSE (WMASTER),MF=(E,WCLOSEM)
*                                                                       LB
*********************************************************************** LB
*        CLOSE OUTFIL                                                 * LB
*********************************************************************** LB
         CLI   FILESW,X'FF'        Is 'FILE' SW ON ??                   LB
         BNE   NOFILE05            No, bypass FILE IO                   LB
         LA    R9,WOUTFIL          Yes, close file                      LB
         MVC   WCLOSEO,CLOSEO                                           LB
         CLOSE ((R9),),MF=(E,WCLOSEO)                                   LB
NOFILE05 EQU   *                                                        LB
*********************************************************************** LB
*                                                                       LB
         B     ENDIT
*
BADPUT   EQU   *
         ABEND 10                  ABEND FOR TPUT
*                                                                       LB
BADPUT11 EQU   *                                                        LB
         ABEND 11                  ABEND FOR TPUT on OUTFIL             LB
*
*
ENDIT    EQU   *
         EXIT
         EJECT
*
EIGHTY   EQU   80
MSG1     DC    C'Enter search argument in quotes'
MSG2     DC    C'Missing quotes - Re-Enter'
MSG3     DC    C'Open on ''MASTER'' file has failed'
MSG3O    DC    C'Open on ''OUTFIL'' file has failed'                    LB
MSG4     DC    C'Total Number of Occurrences found in Member XXXXXXXX iX
               s XXXXXXX'
MSG5     DC    C'   From XXXXX Members, the Final Total is   ==========X
               > XXXXXXX'
MSG5A    DC    C'   Note - QUICK option invoked. Number of occurrences X
               is always 1 if found'
ERR1     DC    C'IKJPARS ERROR - COMMAND TERMINATED'
ERR2     DC    C'IKJDAIR ERROR - COMMAND TERMINATED'
ERR3     DC    C'Command is restricted to search your own Data Sets OnlX
               y'
ERR4     DC    C'Data set is not Partitioned. Command Terminated'
*
H256     DC    H'256'
PAT      DC    X'4020202020202120'
PAT1     DC    X'402020202120'
*
*
         EJECT
         DS    0F
LINKIT   LINK  EP=IKJEFF18,SF=L
LINKITL  EQU   *-LINKIT
*
*
*
*
MASTER   DCB   DSORG=PS,MACRF=(RP),EODAD=ENDRPT,RECFM=U,DDNAME=MASTER
MASTERL  EQU   *-MASTER            LENG OF THE INPUT DCB
*
*
*
*
         DS    0F                  ALIGN ON FWD BOUNDARY
OPENM    OPEN  (MASTER),MF=L
OPENML   EQU   *-OPENM
*
*
*
*
         DS    0F                  ALIGN ON FWD BOUNDARY
CLOSEM   CLOSE (MASTER,FREE),MF=L
CLOSEML  EQU   *-CLOSEM
*
*
*
*
         DS    0F
READM    READ  HDECB,SF,MASTER,,'S',MF=L
READML   EQU   *-READM
*
*
*
*                                                                       LB
         DS    0F                                                       LB
OPENO    OPEN  (OUTFIL,(OUTPUT)),MF=L                                   LB
OPENOL   EQU   *-OPENO                                                  LB
*                                                                       LB
         DS    0F                                                       LB
CLOSEO   CLOSE (OUTFIL),MF=L                                            LB
CLOSEOL  EQU   *-CLOSEO                                                 LB
*                                                                       LB
         DS    0F                                                       LB
OUTFIL   DCB   DSORG=PS,MACRF=(PM),                                    XLB
               LRECL=80,BLKSIZE=3200,RECFM=FB,                         XLB
               DDNAME=OUTFIL                                            LB
OUTFILL  EQU   *-OUTFIL                                                 LB
*                                                                       LB
*                                                                       LB
*                                                                       LB
*                                                                       LB
APCL     DC    A(PCL)
HEXZEROS DC    F'0'
DAIRBYTE DC    XL2'0001'
*
*
BLANK    DC    CL80' '
*
         EJECT
         SPACE 3
QUOTE    DS    0CL256
         DC    256X'00'
         ORG   QUOTE+C''''         STOP AT QUOTE
         DC    X'FF'
         ORG
         SPACE 3
*
TRTABLE  DC    256AL1(*-TRTABLE)
*
         ORG   TRTABLE+X'81'       THIS IS LOWER CASE 'A'
         DC    C'ABCDEFGHI'
         ORG   TRTABLE+X'91'       THIS IS LOWER CASE 'J'
         DC    C'JKLMNOPQR'
         ORG   TRTABLE+X'A2'       THIS IS LOWER CASE 'S'
         DC    C'STUVWXYZ'
         ORG
*
         EJECT
         IKJPPL
PPLLENG  EQU   *-PPL               LENG OF PPL
         EJECT
         IKJDAPL
DAPLLENG EQU   *-DAPL              LENG OF DAPL
         EJECT
         IKJDAP08
DAP08LEN EQU   *-DAPB08            LENG
         EJECT
         IKJEFFDF DFDSECT=YES
         EJECT
*
WKAREA   DSECT
         DS    18F                 SAVE AREA
WKPARM   DS    20F                 THIS IS USED FOR CALL
*
PREC     DS    0CL80               OUTFIL  RECORD FOR FOUND RESULTS     LB
PRCDTYP  DS    CL01      01-01     Record Type 2-member  3-PDS Totals   LB
PDSN     DS    CL44      02-45     PDS Name                             LB
PVOLSER  DS    CL06      46-51     PDS VOLSER                           LB
PDSO     DS    CL03      52-54     DSO PO always!                       LB
PMBR     DS    CL08      55-62     PDS Member Name                      LB
PFNDCNT  DS    CL07      63-69     Found Search Count                   LB
         DS    CL11      70-80                                          LB
*                                                                       LB
         DS    0F
WLINKIT  DS    CL(LINKITL)
*
*
*
         DS    0F
WKPPL    DS    CL(PPLLENG)
         DS    0F
WKDAPL   DS    CL(DAPLLENG)
         DS    0F
WKDAP08  DS    CL(DAP08LEN)
         DS    0F
WKDERR   DS    CL(DFLEN)
*
*
TTR      DS    F                   THIS IS SAVED TTR
WKTTR    DS    F                   THIS IS SAVED TTR
SAVEEND  DS    F
SAVECURR DS    F
SAVESIZE DS    H
SAVERECL DS    H
SAVE15   DS    F
SAVECOMP DS    H
SAVECNT  DS    H
SAVEREC  DS    F
ANS      DS    F
MYECB    DS    F
DAIRRET  DS    F
ARGLENG  DS    F                   ARGUMENT LENG
WDWORD   DS    D                   WORK DOUBLE WORD
SAVEODAD DS    CL3
         DS    0H
WKDST    DS    0CL46
         DS    CL2                 LENG
WKDSTEST DS    CL44                NAME
         DS    0H
*
SAVEDDTS DS    CL8                 SAVED DDNAME FOR TEST MASTER
SAVEMEM  DS    CL8                 SAVED MEMBER NAME
SAVEVOLS DS    CL6                 SAVE VOLSER                          LB
*
*
BUFFER   DS    CL80
ARG      DS    CL80                SAVED ARGUMENT
SW       DS    X
NEXTBLK  EQU   X'80'
*
UPPER    DS    X                   SWITCH USED FOR UPPERCASE
QKSW     DS    X                   SWITCH USED FOR 'QUICK' OPTION
FILESW   DS    X                   SWITCH USED FOR 'FILE'  OPTION       LB
QUIETSW  DS    X                   SWITCH USED FOR 'QUIET'  OPTION      LB
COUNTER  DS    PL6
TOT      DS    PL6                 THIS IS FINAL TOTAL
MEMTOT   DS    PL6                 THIS IS TOTAL OF MEMBERS
TESTUSER DS    CL8
KGROUP   DS    CL7                 SAVED GROUP PREFIX
LGROUP   DS    F                   LENG USED TO COMPARE
GROUPSW  DS    X                   SWITCH
*
*                                                                       LB
         DS    0F                                                       LB
WOUTFIL  DS    CL(OUTFILL)         DCB                                  LB
*                                                                       LB
         DS    0F                                                       LB
WOPENO   DS    CL(OPENOL)          OPEN                                 LB
*                                                                       LB
         DS    0F                                                       LB
WCLOSEO  DS    CL(CLOSEOL)         CLOSE                                LB
*                                                                       LB
*                                                                       LB
*                                                                       LB
*
         DS    0F                  ALIGN ON FWD BOUNDARY
WMASTER  DS    CL(MASTERL)         WK DCB
*
         DS    0F                  ALIGN ON FWD BOUNDARY
WOPENM   DS    CL(OPENML)
*
         DS    0F
WCLOSEM  DS    CL(CLOSEML)
*
         DS    0F
WREADM   DS    CL(READML)
*
ETAB     DS    CL256
*
         DS    0F
DIR      DS    CL256
RECORD   DS    CL32767             THIS IS A LARGEST BLK
*
WKLENG   EQU   *-WKAREA            THIS IS LENG OF GETMAINED AREA
*
         EJECT
         CVT   DSECT=YES
         EJECT
         IKJCPPL
         EJECT
         IKJPSCB
         EJECT
         DCBD  DSORG=PS
         EJECT
FIND     CSECT
         PRINT NOGEN
PCL      IKJPARM
FPDS     IKJPOSIT DSNAME,PROMPT='DSNAME OF PDS',                       X
               HELP=('DSNAME OF PDS TO BE SEARCHED')
*
FKEYWD   IKJKEYWD
         IKJNAME 'STRING',SUBFLD=FSTRING
*
FLOWER   IKJKEYWD
         IKJNAME 'LOWER'
*                                                                       LB
FFILE    IKJKEYWD                                                       LB
         IKJNAME 'FILE'                                                 LB
*                                                                       LB
FQUIET   IKJKEYWD                                                       LB
         IKJNAME 'QUIET'                                                LB
*                                                                       LB
FVOL     IKJKEYWD                                                       LB
         IKJNAME 'VOL',SUBFLD=@VOL                                      LB
*
FQUICK   IKJKEYWD
         IKJNAME 'QUICK'
*
GROUP    IKJKEYWD
         IKJNAME 'GROUP',SUBFLD=@GROUP
*
*
FSTRING  IKJSUBF
STRING   IKJIDENT 'ARGUMENT STRING',MAXLNTH=80,CHAR,ASIS,              X
               PROMPT='STRING OF ARGUMENT TO BE SEARCHED'
*
@GROUP   IKJSUBF
PGROUP   IKJIDENT 'MEMBER NAME PREFIX',MAXLNTH=7,                      X
               FIRST=ALPHA,OTHER=ALPHANUM,                             X
               PROMPT='MEMBER NAME PREFIX USED FOR SEARCH'
*                                                                       LB
@VOL     IKJSUBF                                                        LB
PVOL     IKJIDENT 'VOLUME',,MAXLNTH=6,CHAR,                            XLB
               PROMPT='VOLUME for Dataset'                              LB
*
         IKJENDP
*
         END
@@
//LKED.SYSLMOD DD  DISP=SHR,DSN=SYSGEN.ISPF.LLIB(FINDSRCH)
//*
//* -------------------------------------------------------*
//* *  Assemble Link-Edit DSNTAB  to SYS2.LINKLIB          *
//* -------------------------------------------------------*
//DSNTAB   EXEC  ASML,
//         PARM.LKED='MAP,LIST,LET,RENT,XREF,REUS,REFR'
//ASM.SYSIN DD DATA,DLM=@@
         MACRO
&LAB1    ENTERR  &SA=SAVEAREA,&WA=WORKAREA,&WL=WORKLEN,&LEVEL=,&R=,   XX
               &CLEAR=YES
         MNOTE ' CLEAR=&CLEAR,SA=&SA,WA=&WA,WL=&WL,LEVEL=&LEVEL'
&LAB1    CSECT
         SAVE  (14,12),,&LAB1-&LEVEL
         LR    R12,R15            HOPE HE KNOWS WHAT HE'S DOING
         USING &LAB1,R12
         LR    R10,R1             SAVE PARM PTR R10->PARM PTR
         L     R0,=A(&WL)         R0=GET LENGTH
         GETMAIN R,LV=(0)         R1->WORKAREA
         LR    R11,R13            R11->CALLERS SAVEAREA
         LR    R13,R1             R13->WORKAREA
         USING &WA.,R13
         AIF   ('&CLEAR' NE 'YES').NCLEAR
         L     R15,=A(&WL)        R0=GET LENGTH
         S     R15,=F'72'         SKIP REGS
         MOVE  72(13),(15),0,0
.NCLEAR  ANOP
         ST    R11,&SA.+4         SAVE HIS SAVEAREA PTR
         LA    R13,&SA            R13->SAVEAREA (MINE)
         ST    R13,8(,R11)        MINE IN HIS
         LR    R11,R1             R11->WORKAREA IN CASE NOT SAME AS R13
*                       WORKAREA ADDR IS STILL R13 FOR
*                       THE ASSEMBLER - IF DIFFERENT FROM R13
*                       THEN USE: DROP R13 AND USING &WA.,R11
         LR    R1,R10             RESTORE PARM PTR PTR
         AIF   ('&R' EQ 'NO').NRE
         REGEQU
.NRE     ANOP
         MEND
         MACRO
         REGEQU
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
         MEND
         MACRO                                                          TSO06270
&L1      MOVE  &TO,&TL,&FROM,&FL,&PAD=                                  TSO06280
         LCLA  &NL,&UL,&VL,&RP,&RP2,&RC                                 TSO06290
         LCLC  &TO$,&TL$,&FROM$,&FL$                                    TSO06290
         LCLC  &R1(2),&R2(2),&RN1,&RN2                                  TSO06290
&RC      SETA  1
&RP      SETA  2
&RP2     SETA  &RP+1
.RPLOOP  ANOP
&RN1     SETC  '(R'.'&RP'.')'
&RN2     SETC  '(R'.'&RP2'.')'
         AIF   ('&TO' EQ '&RN1').CKP2
         AIF   ('&TL' EQ '&RN1').CKP2
         AIF   ('&FROM' EQ '&RN1').CKP2
         AIF   ('&FL' EQ '&RN1').CKP2
         AIF   ('&TO' EQ '&RN2').CKP2
         AIF   ('&TL' EQ '&RN2').CKP2
         AIF   ('&FROM' EQ '&RN2').CKP2
         AIF   ('&FL' EQ '&RN2').CKP2
&R1(&RC) SETC  'R'.'&RP'
&R2(&RC) SETC  'R'.'&RP2'
&RC      SETA  &RC+1
         AIF   (&RC EQ 3).GP
.CKP2    ANOP
&RP      SETA  &RP+2
&RP2     SETA  &RP+1
         AIF   (&RP LT 10).RPLOOP
         MNOTE 8,'NO REGISTER PAIRS FREE FOR MVCL INSTRUCTION'
         MEXIT
.GP      ANOP
&TO$     SETC  '&TO'
&TL$     SETC  '&TL'
&FROM$   SETC  '&FROM'
&FL$     SETC  '&FL'
         AIF   ('&TO'(1,1) NE '(').TLC
&TO$     SETC  '0&TO'
.TLC     AIF   ('&TL'(1,1) NE '(').FC
&TL$     SETC  '0&TL'
.FC      AIF   ('&FROM'(1,1) NE '(').FLC
&FROM$   SETC  '0&FROM'
.FLC     AIF   (T'&FL EQ 'O').OO                                        TSO06340
         AIF   ('&FL'(1,1) NE '(').OO
&FL$     SETC  '0&FL'
.OO      ANOP
&L1      STM   14,12,12(13)                                             TSO06300
         LA    &R1(1),&TO$                                              TSO06310
         LA    &R2(1),&TL$                                              TSO06320
         LA    &R1(2),&FROM$                                            TSO06330
         AIF   (T'&FL EQ 'O').UTL                                       TSO06340
         LA    &R2(2),&FL$                                              TSO06350
         AGO   .PC                                                      TSO06360
.UTL     LA    &R2(2),&TL$                                              TSO06370
.PC      AIF   ('&PAD' EQ '').NPC                                       TSO06380
         ICM   &R2(2),8,=&PAD                                           TSO06390
.NPC     MVCL  &R1(1),&R1(2)                                            TSO06400
         LM    14,12,12(13)                                             TSO06410
         MEND                                                           TSO06420
         MACRO
&LAB     LEAVER &WR=R13,&WL=WORKLEN
&LAB     LR    R1,&WR             WORKAREA ADDR FOR FREEMAIN
         L     R0,=A(&WL)         WORKAREA LEN   "    "
         L     R13,4(R13)         GET CALLERS SAVEAREA ADDR
         LR    R11,R15            SAVE RETURN CODE
         FREEMAIN R,LV=(0),A=(1)
         LR    R15,R11            RESTORE RETURN CODE
         RETURN (14,12),RC=(15)
         MEND
*          DATA SET 761KKDSNTB AT LEVEL 001 AS OF 08/30/82
* WRITTEN BY KERMIT KISER - WASHINGTON STATE DP SERVICE CENTER (WDPSC)
*
DSNTAB   ENTERR LEVEL=V1M0
         LR    R11,R1             R11->PARM LIST
         SLR   R15,R15            R15=0
         EJECT
*
*        MAINLINE ROUTINE:
*
         BAL   R14,FINDDD         R9->DD ENTRY
         BXH   R15,R15,EXIT       STOP IF NOT FOUND
*
*
         SLR   R5,R5              R5=0  DSN COUNT
         L     R6,4(,R11)         R6->DSN TABLE
         L     R7,8(,R11)         R7->SIZE OF TABLE
         LH    R7,0(,R7)          R7=LENGTH OF TABLE
*
MOVELOOP BAL   R14,MOVEDSN        PUT THE DSN IN THE TABLE
         BXH   R15,R15,EXIT       STOP IF TABLE ERROR
         CLI   0(R9),X'00'        END OF TIOT?
         BE    MOVEND             YES
         CLC   4(8,R9),=CL8' '    BLANK DD NAME?
         BE    MOVELOOP           YES->MUST BE CONCATENATED
*
MOVEND   L     R1,12(,R11)        R1->ENTRY COUNT PARM
         STH   R5,0(R1)           PASS THE COUNT OF ENTRIES TO JEFF
         EJECT
*
*        CODE  TO LEAVE DSNTAB:
*
EXIT     LEAVER
         EJECT
*                                 FIND THE DDNAME IN THE TIOT:
*
FINDDD   ST    R14,L1LS
         L     R1,540             R1->TCB NOW RUNNING
         L     R2,12(,R1)         R2->TIOT
         LA    R9,24(,R2)         R9->FIRST DD ENTRY IN TIOT
         L     R10,0(,R11)        R10->DDNAME PASSED BY JEFF
*
MTCHLOOP CLC   0(8,R10),4(R9)     DDNAMES MATCH?
         BE    FINDX              YES
         BAL   R14,NEXTDD         R9->NEXT DD ENTRY
         CLI   0(R9),X'00'        END OF TIOT?
         BNE   MTCHLOOP           NO->COMPARE DDNAME
*
         LA    R15,2              R15=2 RC=4
*
FINDX    L     R14,L1LS
         BR    R14                EXIT
         EJECT
*
*              SUBRTN TO MOVE A   DSN TO THE TABLE:
*
MOVEDSN  ST    R14,L1LS
         ICM   R1,7,12(R9)        R1->JFCB
         CH    R7,=H'50'          ROOM IN TABLE FOR THIS NAME?
         BNL   MOVEIT             YES -> DO IT
         LA    R15,4              NO->SAY TABLE SIZE ERROR
         B     MX
*
MOVEIT   MVC   6(44,R6),16(R1)    MOVE THE DSN INTO THE TABLE
         MVC   0(4,R6),=X'00020001'    INIT TEXT UNIT KEY
*
*                                 FIND THE LENGTH OF THE DSN:
*
         LA    R2,6(,R6)          R2->DSN
         SLR   R3,R3              R3=0 INIT DSN LEN
DSL      IC    R4,0(R2,R3)        R4=NEXT DSN CHAR TO BE CHECKED
         CLM   R4,1,=C' '         IS IT BLANK?
         BE    DSEND              YES
         LA    R3,1(,R3)          R3+1->NEXT CHAR
         B     DSL                CHECK NEXT IF NO BLANK YET
*
DSEND    STH   R3,4(,R6)          PUT THE DSN LENGTH IN THE TABLE
         SH    R7,=H'50'          UPDATE LENGTH REMAINING IN TABLE
         LA    R5,1(,R5)          R5+1=DSN COUNT IN TABLE
         BAL   R14,NEXTDD         R9->NEXT DD ENTRY IN TIOT
         LA    R6,50(,R6)         R6->NEXT SLOT IN NAME TABLE
*
MX       L     R14,L1LS
         BR    R14                EXIT
         EJECT
*
*              SUBRTN TO MOVE R9  TO THE NEXT DD ENTRY IN THE TIOT
*
NEXTDD   SLR   R8,R8
         IC    R8,0(,R9)          R8=LENGTH OF CURRENT DD ENTRY
         LA    R9,0(R8,R9)        R9->NEXT DD ENTRY IN TIOT
         BR    R14                RETURN
         SPACE 2
         LTORG
         SPACE 2
WORKAREA DSECT
SAVEAREA DS    18F
L1LS     DS    F
L2LS     DS    F
WORKLEN  EQU   *-WORKAREA
         END
@@
//LKED.SYSLMOD DD  DISP=SHR,DSN=SYS2.LINKLIB(DSNTAB)
//*
//* -------------------------------------------------------*
//* *  Assemble Link-Edit LOCATE  to SYS2.CMDLIB           *
//* -------------------------------------------------------*
//LOCATE   EXEC  ASML,
//         PARM.LKED='MAP,LIST,LET,RENT,XREF,REUS,REFR'
//ASM.SYSIN DD DATA,DLM=@@
*
*  MODIFIED 4/3/86 TO SUPPORT CONCATENATED STEPLIBS AND
*              THE XA PARMLIB MEMBER LPALSTXX - KERMIT KISER
*
*  MODIFIED 4/22/86 TO ALSO SEARCH ISPLLIB CONCATENATION IF PRESENT- KK
*
*  MODIFIED 7/25/86 TO FIX THE PROBLEM OF LOCATE FINDING ONLY
*              A PORTION  OF THE DATASETS WHEN MULTIPLE MEMBERS
*              WERE SPECIFIED. - GORDON SCHILLINGER
*
         TITLE 'LOCATE - TSO COMMAND TO IDENTIFY THE LOCATION OF SPECIF*
               IED MEMBERS IN SYSTEM LIBRARIES'
*
*        LOCATE WILL REPORT THE NAME OF THE DATASET IN WHICH THE
*        SPECIFIED MEMBER(S) EXIST. SYSTEM LIBRARIES ARE SEARCHED IN
*        A MANNER SIMILAR TO THE PROGRAM FETCH MODULE SEARCH ORDER:
*              1) THE STEPLIB      (IF ONE EXISTS)
*              2) SYS1.LPALIB      (SEE NOTE BELOW ABOUT FLPA, MLPA)
*              3) SYS1.LINKLIB
*              4) THE SYSTEM LINK LIST, ALWAYS ASSUMED TO BE LNKLST00
*              5) SYS1.SVCLIB
*        THESE DATASETS ARE ALLOCATED AND CONCATENATED, THEN A BLDL
*        MACRO IS ISSUED AGAINST THE ENTIRE GROUP. THUS, ONLY THE FIRST
*        OCCURRENCE OF A MEMBER WILL BE REPORTED ALTHOUGH IT MAY EXIST
*        IN MORE THAN ONE LIBRARY.
*
*        LOCATED MEMBERS WILL BE IDENTIFIED BY ONE OF THE FOLLOWING:
*              MEMBER-NAME IS A MEMBER IN DATASET-NAME
*              -------------------------- STEPLIB DATASET-NAME
*              ALIAS-NAME IS AN ALIAS OF MEMBER-NAME IN DATASET-NAME
*              ---------------------------------------- STEPLIB DSNAME
*
*  NOTE ****** CONCERNING FIXED AND MODIFIED LINK PACK AREA DATASETS
*              A LOGICAL ERROR CAN OCCUR IN THE STANDARD OPERATION OF
*              LOCATE IF DATASETS OTHER THAN LINKLIB AND SVCLIB ARE
*              USED FOR LPA EXTENSIONS AND ARE APPENDED TO THE LINK
*              LIST, AS IS USUALLY THE CASE WITH SYS1.FLPALIB/MLPALIB.
*              ALTHOUGH THE SYSTEM PROPERLY SEARCHES THE VIRTUAL
*              STORAGE IMAGES OF THESE LIBRARIES, THE BLDL FUNCTION
*              DOES NOT, SO A LOCATION OTHER THAN THAT OF THE MODULE
*              ACTUALLY IN USE MAY BE REPORTED.
*       ****** FOR THIS REASON, A 'LOCATE ALL' FEATURE IS PROVIDED
*              WHICH WILL ISSUE A SEPARATE BLDL AGAINST EACH OF THE
*              DATASETS LISTED ABOVE AND REPORT ALL OCCURRENCES.
*
*        IF ALLOCATION OF ANY LIBRARY OTHER THAN STEPLIB IS REFUSED,
*        LOCATE WILL TERMINATE. IF STEPLIB IS A CONCATENATED GROUP IT
*        WILL BE IGNORED TO PREVENT UNNECESSARY CONFUSION OF THE
*        INTERNAL DATASET NAME TABLES.
*
*        ORIGINAL AUTHOR OF MVT VERSION UNKNOWN.
*        THIS VERSION WRITTEN FOR MVS (OS/VS RELEASE 3) AND USES
*        MVS-ONLY FUNCTIONS SUCH AS 'DYNALLOC' AND 'CALLTSSR'.
*              OCTOBER, 1976       SAM LEPORE       617 890-8460 X-138
*              GTE LABORATORIES, INC., 40 SYLVAN RD., WALTHAM, MASS.
*
*        ATTRIBUTES: RE-ENTRANT AND REUSABLE
*        SYS1.AMODGEN REQUIRED TO RESOLVE CVT MACRO
*
         EJECT
***********************************************************************
*        FOLLOWING AS COMMENTS IS THE HELP ENTRY FOR    LOCATE        *
*        TO USE THIS AS INPUT TO IEBUPDTE,                            *
*        MERELY REMOVE THE ASTERISKS FROM COLUMN 1.                   *
***********************************************************************
         SPACE 2
*./ ADD NAME=LOCATE
*)F FUNCTION -
*   LOCATE WILL REPORT THE NAME OF THE DATASET IN WHICH THE SPECIFIED
*   MEMBER NAME(S) EXIST. SYSTEM LIBRARIES ARE SEARCHED IN A MANNER
*   SIMILAR TO THE PROGRAM FETCH MODULE SEARCH ORDER. IF THE MEMBER
*   NAME SPECIFIED IS AN ALIAS, AND IF THE MODULE WHICH IT NAMES
*   IS REENTERABLE OR REUSABLE, LOCATE WILL ALSO IDENTIFY THE
*   REAL MEMBER NAME.
*)X SYNTAX -
*   LOCATE (MEMBER-NAMES) ALL
*
*REQUIRED - AT LEAST ONE MEMBER-NAME
*DEFAULTS - NONE
*OPTIONAL - ALL
*)O OPERANDS -
*))MEMBER-NAMES - ONE OR MORE MEMBER OR ALIAS NAMES TO BE SEARCHED FOR
*               IN THE SYSTEM LIBRARIES.
*))ALL - REQUEST TO LOOK FOR MEMBER-NAME IN EACH OF THE SYSTEM
*   LIBRARIES. IF ALL IS NOT SPECIFIED, ONLY THE FIRST OCCURRENCE
*   OF EACH MEMBER-NAME WILL BE REPORTED, JUST AS PROGRAM FETCH
*   STOPS AT THE FIRST PROGRAM COPY IT FINDS.
*   LIBRARY SEARCH ORDER IS THE SAME WHETHER ALL IS SPECIFIED OR NOT.
*     1) THE STEP LIBRARY, IF ONE EXISTS
*     2) SYS1.LPALIB
*     3) SYS1.LINKLIB
*     4) THE SYSTEM LINKLIST, AS DEFINED BY LNKLST00
*     5) SYS1.SVCLIB
*./ ENDUP
         EJECT
LOCATE   CSECT
         SAVE  (14,12),,*
         LR    R12,R15             REG 12 IS BASE AT DISPLACEMENT +0
         USING LOCATE,R12
         LR    R11,R1              SAVE CPPL ADDRESS
         USING CPPL,R11
         GETMAIN R,LV=CORENEED
         ST    R1,8(,R13)          FORWARD CHAIN
         ST    R13,4(,R1)          BACKWARD CHAIN
         LR    R13,R1              SAVEAREA/WORKAREA ADDRESSABILITY
         USING WORKAREA,R13
*
***********************************************************************
*        PARSE THE COMMAND OPERANDS                                   *
***********************************************************************
*
         LA    R10,PPLAREA         ADDRESS THE PARSE PARAMETER LIST
         USING PPL,R10
         MVC   PPLUPT,CPPLUPT      INITIALIZE
         MVC   PPLECT,CPPLECT        PARSE
         LA    R1,ECB                  PARAMETER
         ST    R1,PPLECB                 LIST
         MVC   PPLPCL,=A(PARSEPCL)         CONTROL
         LA    R1,PPLANSWR                   FIELDS
         ST    R1,PPLANS
         MVC   PPLCBUF,CPPLCBUF
         SR    R1,R1
         ST    R1,PPLUWA
         ST    R1,ECB
         CALLTSSR EP=IKJPARS,MF=(E,PPL)
         CH    R15,=H'4'           SUCCESSFUL ?
         BE    RETURN12            NO- MSG WAS ISSUED, RETURN TO TMP
         BH    PARSFAIL            NO- ISSUE MSG VIA GNRLFAIL ROUTINE
         L     R10,PPLANSWR        YES- ADDRESS PARSE DESCRIPTOR DSECT
         USING IKJPARMD,R10
*
***********************************************************************
*        INITIALIZE TSO MESSAGE ISSUER (IKJEFF02) PARAMETER LIST      *
***********************************************************************
*
         LA    R9,MTPARMS          ADDRESS MESSAGE ISSUER WORKAREA
         USING MTDSECTD,R9
         XC    MTPARMS(MTLENMT),MTPARMS CLEAR THE PARM LIST
         LA    R1,MTCSECTP
         ST    R1,MTPLPTR          POINT TO PARMS
         ST    R11,MTCPPLP         POINT TO CPPL
         LA    R1,ECB
         ST    R1,MTECBP           POINT TO PUTLINE COMMUNICATIONS ECB
         MVI   MTRESV1,X'80'       INDICATE END OF LINKAGE LIST
         MVC   MTCSECTP,=A(MESSAGES) ADDRESS OF MESSAGE TEXT CSECT
         OI    MTSW1,MTNOIDSW+MTPUTLSW ALL MSGS ARE 'PUTLINE INFO'
*
***********************************************************************
*        MOVE THE MEMBER NAMES TO THE WORKAREA                        *
***********************************************************************
*
         LA    R8,BLDAREA          ADDRESS BLDL LIST WORKAREA
         USING PDSBLDL,R8
         LA    R3,1                INITIALIZE BXLE LOOP - TOTAL
         LR    R4,R3                                    - INCREMENT
         LA    R5,MAXMEMS                               - LIMIT
         LA    R6,MEMNAME          ADDRESS FIRST SPECIFIED NAME PDE
MOVELOOP L     R2,0(,R6)           ADDRESS NAME CHARACTER STRING
         LH    R1,4(,R6)           LENGTH OF STRING
         BCTR  R1,0
         MVC   BLDMEM(8),=CL8' '   BLANK-FILL THE FIELD
         EX    R1,MOVEMEM          MOVE MEMBER NAME TO BLDL LIST
         L     R6,8(,R6)           FOLLOW CHAIN OF NAMES
         C     R6,ENDCHAIN         END OF CHAIN ?
         BE    ENDLOOP
         LA    R8,ENTRYLEN(,R8)    POINT TO NEXT ENTRY IN BLDAREA
         BXLE  R3,R4,MOVELOOP      LOOP UNTIL BLDAREA IS FULL
         MVC   MTMSGID(4),=C'OVFL' TABLE OVERFLOW IMMINENT, ISSUE MSG
         CALLTSSR EP=IKJEFF02,MF=(E,MTPARMS)
         LTR   R15,R15
         BNZ   RETURN12
         LR    R3,R5               RESET TOTAL TO MAXMEMS
ENDLOOP  STH   R3,BLDLIST          SET NUMBER OF ENTRIES FOR BLDL
         LA    R1,ENTRYLEN
         STH   R1,BLDELEN          SET LENGTH OF EACH ENTRY
         CR    R3,R4               ONLY ONE ENTRY ?
         BE    STEPLIB             YES- SKIP THE SORT
***********************************************************************
*        SORT THE MEMBERS INTO ASCENDING SEQUENCE                     *
***********************************************************************
         SR    R3,R4               SET OUTER LOOP TO TOTAL MINUS 1
         LA    R8,BLDAREA          ADDRESS FIRST MEMBER NAME
NEWLOW   LA    R1,ENTRYLEN(,R8)    ADDRESS NEXT ENTRY FOR INNER LOOP
         LR    R2,R3               SET INNER LOOP TO MAX OF OUTER VALUE
WHICHLOW CLC   0(8,R8),0(R1)       COMPARE CURRENT 'LOW' TO NEXT ENTRY
         BNH   OLDLOW              NOT HIGH- KEEP 'LOW', NEW 'NEXT'
         MVC   DBLWORD(8),0(R8)    HIGH- SWITCH ENTRIES, SAVE OLD
         MVC   0(8,R8),0(R1)            'NEXT' BECOMES 'LOW'
         MVC   0(8,R1),DBLWORD
OLDLOW   LA    R1,ENTRYLEN(,R1)    INCREMENT NEXT
         BCT   R2,WHICHLOW         COMPLETE INNER LOOP
         LA    R8,ENTRYLEN(,R8)    INCREMENT TO NEXT 'LOW'
         BCT   R3,NEWLOW           COMPLETE OUTER LOOP
*
***********************************************************************
*        BUILD THE LIBRARY SEARCH TABLE OF DATASET NAMES AND ALLOCATE *
*        EACH LIBRARY. BUILD A CORRESPONDING TABLE OF DDNAMES RETURNED*
*        BY DYNALLOC TO BE USED TO CONCATENATE OR OPEN EACH LIBRARY.  *
***********************************************************************
STEPLIB  LA    R8,S99PARMS         ADDRESS S99 REQUEST BLOCK
         USING S99RB,R8
         XC    S99PARMS(S99RBLEN),S99PARMS CLEAR THE BLOCK
         MVI   S99RBLN,S99RBLEN    SET LENGTH
         LA    R1,S99TEXTP
         ST    R1,S99TXTPP
***********************************************************************
*        FIRST SEE IF THERE IS AN ISPLLIB                             *
***********************************************************************
         CALL  DSNTAB,(=CL8'ISPLLIB',DSNTABLE,=H'1000',DSNCNT),VL,    XX
               MF=(E,PLIST)
         LTR   R15,R15                                           WDPSC
         BZ    GOTSUM                                           WDPSC
         CH    R15,=H'4'  OK?
         BNE   RETURN12   OOPS!
         LA    R3,DSNTABLE
         B     CHKSTEP
GOTSUM   LH    R1,DSNCNT                                         WDPSC
         MH    R1,=H'50'                                         WDPSC
         LA    R3,DSNTABLE(R1)                                   WDPSC
***********************************************************************
* CHECK STEPLIB AFTER ISPLLIB
***********************************************************************
CHKSTEP  CALL  DSNTAB,(=CL8'STEPLIB',0(R3),=H'1000',DSNCNT),VL,       XX
               MF=(E,PLIST)
         LTR   R15,R15                                           WDPSC
         BZ    GOTMORE                                          WDPSC
         CH    R15,=H'4'  OK?
         BNE   RETURN12   OOPS!
         B     LPALIB
GOTMORE  LH    R1,DSNCNT                                         WDPSC
         MH    R1,=H'50'                                         WDPSC
         LA    R3,0(R1,R3)                                   WDPSC
***********************************************************************
*        THEN THROW IN LPALIB AND LINKLIB                             *
***********************************************************************
LPALIB   L     R1,16              R1->CVT
         TM    116(R1),X'80'      XA?
         BNO   SKPLPAX            NOPE
         MVC   PARMNAME,LPALST00
         BAL   R14,READPARM                                      WDPSC
SKPLPAX  MVC   0(17,R3),SYS1LPA                                  WDPSC
         LA    R3,50(R3)                                         WDPSC
         MVC   0(18,R3),SYS1LINK                                 WDPSC
         LA    R3,50(R3)                                         WDPSC
         MVC   PARMNAME,LNKLST00
         BAL   R14,READPARM                                      WDPSC
         B     FINISHIT WITH SVCLIB                              WDPSC
***********************************************************************
*                FILL    TABLE WITH CONTENTS OF SYS1.PARMLIB(LNKLST00)*
***********************************************************************
READPARM ST    R14,L1LS                                          WDPSC
         MVI   S99VERB,S99VRBAL    SET DYNALLOC FUNTION TO ALLOCATE
         LA    R1,S99TEXT
         ST    R1,S99TEXTP         FIRST TEXT UNIT POINTER
         MVC   S99TEXT(18),SYS1PARM DSNAME SYS1.PARMLIB
         LA    R1,S99TEXT+18
         ST    R1,S99TEXTP+4       SECOND TEXT UNIT POINTER
         MVC   S99TEXT+18(14),PARMNAME MEMBER NAME LNKLST00
         LA    R1,S99TEXT+32
         ST    R1,S99TEXTP+8       THIRD TEXT UNIT POINTER
         MVC   S99TEXT+32(7),STATSHR STATUS SHARE
         LA    R1,S99TEXT+39
         ST    R1,S99TEXTP+12      FOURTH TEXT UNIT POINTER
         MVC   S99TEXT+39(4),FREECLOS AUTOMATICALLY FREE AT CLOSE
         LA    R1,S99TEXT+43
         ST    R1,S99TEXTP+16      FIFTH TEXT UNIT POINTER
         MVC   S99TEXT+43(14),RTDDN RETURN ALLOCATED DDNAME
         MVI   S99TEXTP+16,S99TUPLN INDICATE END OF POINTER LIST
         ST    R8,DBLWORD          POINT TO REQUEST BLOCK
         MVI   DBLWORD,S99RBPND
         LA    R1,DBLWORD          ADDRESS THE POINTER
         DYNALLOC
         LTR   R15,R15             SUCCESSFUL ?
         BNZ   S99FAIL
         MVC   DCB(96),DCBPARML    INITIALIZE DCB FOR READ PARMLIB
         MVC   DCB+40(8),S99TEXT+49 MOVE IN ALLOCATED DDNAME
         MVI   DBLWORD,X'80'       SET OPEN OPTION INPUT,END OF LIST
         OPEN  (DCB),MF=(E,DBLWORD)
         TM    DCB+48,X'10'        OPEN SUCCESSFUL ?
         BO    GETREC              YES- READ THE CONTROL CARDS
         MVC   MTMSGID(4),=C'NOPN' NO- TELL USER CANNOT OPEN
         LA    R1,ERRPARML
         ST    R1,MTINSRT          POINT TO MESSAGE INSERT
         MVI   MTLEN,L'ERRPARML    LENGTH OF INSERT
         CALLTSSR EP=IKJEFF02,MF=(E,MTPARMS)
         B     RETURN12
GETREC   GET   DCB
         MVI   71(R1),C'*'         FORCE END OF SCAN AT COLUMN 72
FINDNONB CLI   0(R1),C' '          LOOK FOR FIRST NON-BLANK
         BNE   FOUNDNB
         LA    R1,1(,R1)           INCREMENT
         B     FINDNONB
FOUNDNB  CLC   0(12,R1),SYS1LINK+6 SYS1.LINKLIB DSNAME SPECIFIED ?
         BNE   CKLPA               NO- PUT NAME INTO DSNTABLE
         CLI   12(R1),C','         YES- MAKE SURE
         BE    SKIPLINK
         CLI   12(R1),C' '
         BNE   USENAME
SKIPLINK LA    R1,12(,R1)          INCREMENT PAST LINKLIB
         B     CHECKEND            SEE IF END OF CARD
CKLPA    CLC   0(11,R1),SYS1LPA+6  SYS1.LPALIB DSNAME SPECIFIED ?
         BNE   USENAME             NO- PUT NAME INTO DSNTABLE
         CLI   11(R1),C','         YES- MAKE SURE
         BE    SKIPLPA
         CLI   11(R1),C' '
         BNE   USENAME
SKIPLPA  LA    R1,11(,R1)          INCREMENT PAST LINKLIB
         B     CHECKEND            SEE IF END OF CARD
USENAME  LR    R2,R1
FINDEND  LA    R2,1(,R2)           FIND END OF CURRENT NAME
         CLI   0(R2),C','          COMMA
         BE    FOUNDEND
         CLI   0(R2),C' '          OR BLANK
         BNE   FINDEND
FOUNDEND MVC   0(6,R3),SYS1LINK    MOVE IN S99TEXT KEY HEADER
         SR    R2,R1               LENGTH OF DSNAME
         STC   R2,5(,R3)           PUT INTO S99TEXT
         BCTR  R2,0                DECREMENT FOR EXECUTE
         EX    R2,MOVEDSN          MOVE DSNAME INTO DSNTABLE SLOT
         LA    R3,50(,R3)          INCREMENT TO NEXT AVAILABLE SLOT
         LA    R1,1(R2,R1)         POINT TO SEPARATOR CHARACTER
CHECKEND CLI   0(R1),C' '          END OF LINKLIST ?
         BE    DONEPARM            YES- FINISH DSNTABLE
         CLI   1(R1),C' '          NO, NEXT COL BLANK ? (THIS IS COMMA)
         BE    GETREC              YES- LINKLIST CONTINUES
         LA    R1,1(,R1)           POINT TO ASSUMED NEXT DSN ON RECORD
         CLI   0(R1),C'*'          COLUMN 72 ? (FORCED AT 'GETREC')
         BNE   USENAME             NO- PROCESS NEXT DSNAME
         B     GETREC              YES- COMMA IS IN 71, LIST CONTINUES
DONEPARM MVI   DBLWORD,X'80'       SET CLOSE OPTION - END OF LIST
         CLOSE (DCB),MF=(E,DBLWORD)
         L     R14,L1LS                                          WDPSC
         BR    R14                                               WDPSC
***********************************************************************
*        FINISH THE DSNTABLE WITH SVCLIB, THEN ALLOCATE ALL DATASETS  *
***********************************************************************
FINISHIT MVC   0(17,R3),SYS1SVC
*
         SR    R6,R6               ZERO COUNTER
         LR    R5,R3               ADDRESS OF LAST DSN SLOT USED (SVC)
         LA    R4,50               INCREMENTAL VALUE FOR BXLE
         LA    R3,DSNTABLE         ADDRESS FIRST (STEPLIB) DSN TEXT KEY
         LA    R2,DDNTABLE+4       ADDRESS TABLE OF RETURNED DDNAMES
         CLI   1(R3),0             ANY STEPLIB ?
         BNE   SETALLOC            YES- ALLOCATE IT
         AR    R3,R4               NO- SKIP TO NEXT DSNTABLE SLOT
SETALLOC MVI   S99VERB,S99VRBAL
         LA    R1,S99TEXT
         ST    R1,S99TEXTP
         MVC   S99TEXT(7),STATSHR  STATUS SHARE
         LA    R1,S99TEXT+7
         ST    R1,S99TEXTP+4
         MVC   S99TEXT+7(4),FREECLOS AUTOMATIC FREE AT CLOSE
         LA    R1,S99TEXT+11
         ST    R1,S99TEXTP+8
ALLOCATE MVC   S99TEXT+11(14),RTDDN RETURN ALLOCATED DDNAME
         ST    R3,S99TEXTP+12      POINT TO NAME IN DSNTABLE
         MVI   S99TEXTP+12,S99TUPLN END OF POINTERS
         ST    R8,DBLWORD          POINT TO REQUEST BLOCK
         MVI   DBLWORD,S99RBPND
         LA    R1,DBLWORD          ADDRESS THE POINTER
         DYNALLOC
         LTR   R15,R15             SUCCESSFUL ?
         BNZ   S99FAIL
         MVC   0(10,R2),S99TEXT+15 MOVE LENGTH & DDNAME TO DDNTABLE
         MVI   1(R2),8             FORCE LENGTH TO 8
         LA    R2,10(,R2)          INCREMENT TO NEXT DDNTABLE SLOT
         LA    R6,1(,R6)           KEEP COUNT OF DDNAMES ALLOCATED
         BXLE  R3,R4,ALLOCATE
*
***********************************************************************
*        OPEN THE LIBRARIES, BLDL, AND REPORT ON EACH MEMBER. IF 'ALL'*
*        WAS SPECIFIED, LOOP FOR EACH LIBRARY. IF 'ALL' NOT SPECIFIED,*
*        CONCATENATE THE LIBRARIES BEFORE OPEN AND LOOP ONLY ONCE.    *
***********************************************************************
*
         LA    R2,DSNTABLE+50      DSNAMES TO BE USED AS MESSAGE INSERT
         LA    R3,DDNTABLE+4       ADDRESS DDNAMES LENGTH-TEXT UNITS
         CLI   DSNTABLE+1,0        ASSUME STEPLIB (FIRST ENTRY) ABSENT
         BE    CHECKALL            YES- SEE IF 'ALL' SPECIFIED
         LA    R2,DSNTABLE         NO- RESET DSNAME POINTER TO STEPLIB
         CLI   5(R2),36            ROOM TO INCLUDE STEPLIB ID WITH DSN?
         B     ONLYID              GOTTA SKIP THIS CUZA ISPLLIB--KK
         BH    ONLYID              NO- JUST SAY 'STEPLIB', FORGET NAME
         MVC   S99TEXT(36),6(R2)   YES- TEMPORARILY SAVE THE DSN
         MVC   6(8,R2),=C'STEPLIB ' ADD IDENTITY
         MVC   14(36,R2),S99TEXT   RESTORE THE NAME
         LH    R1,4(,R2)           PICK UP DSN LENGTH
         LA    R1,8(,R1)           ADD ID LENGTH
         STH   R1,4(,R2)           RESET
         B     CHECKALL
ONLYID   EQU   *                   I DONT LIKE THIS PART - KERMIT
*        MVC   6(17,R2),=C'YOUR STEP LIBRARY' OVERLAY STEPLIB DSN
*        MVI   5(R2),17            SET LENGTH
CHECKALL CLI   ALL+1,1             'ALL' SPECIFIED ? (TO SEARCH EACH)
         B     BLDLLOOP    BE      YES- SKIP CONCATENATION     5-15-78
         MVI   S99VERB,S99VRBCC    SET CONCATENATION FUNCTION
         LA    R1,DDNTABLE
         ST    R1,S99TEXTP         POINT TO CONCATENATION TEXT UNIT
         ST    R6,DDNTABLE         SET COUNT OF DDNAMES
         MVI   DDNTABLE+1,1        SET TEXT UNIT KEY ID
         MVI   S99TEXTP,S99TUPLN   END OF POINTERS
         ST    R8,DBLWORD          POINT TO REQUEST BLOCK
         MVI   DBLWORD,S99RBPND
         LA    R1,DBLWORD          ADDRESS THE POINTER
         DYNALLOC
         LTR   R15,R15             SUCCESSFUL ?
         BNZ   S99FAIL
         LA    R4,ERRSYSL+1        ADDRESS INSERT FOR 'NOT FOUND' MSG
         ICM   R4,B'1000',ERRSYSL  LENGTH OF INSERT
         LA    R6,1                FORCE ONLY ONE PASS THRU BLDLLOOP
         B     BLDLOPEN
BLDLLOOP LA    R4,6(,R2)           ADDRESS INSERT FOR 'NOT FOUND' MSG
         ICM   R4,B'1000',5(R2)    LENGTH OF INSERT
***********************************************************************
*        OPEN THE DATASET(S) VIA RETURNED DDNAME                      *
***********************************************************************
BLDLOPEN MVC   DCB(96),DCBSYSL     INITIALIZE DCB FOR OPEN FOR BLDL
         MVC   DCB+40(8),2(R3)     MOVE IN ALLOCATED DDNAME
         MVI   DBLWORD,X'80'       SET OPEN OPTION INPUT, END OF LIST
         OPEN  (DCB),MF=(E,DBLWORD)
         TM    DCB+48,X'10'        OPEN SUCCESSFUL ?
         BO    BLDL                YES- ISSUE BLDL
         MVC   MTMSGID(4),=C'NOPN' NO- TELL USER CANNOT OPEN
         ST    R4,MTINSRT          USE INSERT AS IN 'NOT FOUND' MSG
         CALLTSSR EP=IKJEFF02,MF=(E,MTPARMS)
         B     RETURN12
***********************************************************************
*        AT LAST, ISSUE THE BLDL                                      *
***********************************************************************
BLDL     BLDL  DCB,BLDLIST
         CH    R15,=H'4'           SUCCESSFUL ? (AT LEAST SOME FOUND)
         BNH   REPORT              YES- REPORT ON MEMBERS
         MVC   MTMSGID(4),=C'BLDL' NO- TELL USER BLDL FAILED
         ST    R4,MTINSRT          POINT TO INSERT
         STH   R15,DBLWORD         SAVE THE RETURN CODE, USE AS INSERT
         LA    R1,DBLWORD
         ST    R1,MTINSRT+4        POINT TO RETURN CODE INSERT
         MVI   MTINSRT+4,X'02'+X'80' SET LENGTH & 'PRINT AS DEC CHAR'
         CALLTSSR EP=IKJEFF02,MF=(E,MTPARMS)
         B     BLDLCLOS
***********************************************************************
*        REPORT ON EACH MEMBER AS FOUND OR NOT FOUND BY BLDL          *
***********************************************************************
REPORT   LA    R8,BLDAREA          ADDRESS THE FILLED IN MEMBER ENTRIES
         USING PDSBLDL,R8
         LH    R7,BLDLIST          NUMBER OF MEMBER ENTRIES
MEMLOOP  ST    R8,MTINSRT          POINT TO MEMBER NAME INSERT
         MVI   MTINSRT,8           LENGTH
         CLI   BLDR,0              MEMBER FOUND BY BLDL ?
         BNE   OFFSETK             YES- OFFSET TO PROPER DSN INSERT
         B     NXTMEMBR            NO, GET NEXT MEMBER ENTRY
OFFSETK  SR    R1,R1
         IC    R1,BLDK             GET RELATIVE CONCATENATION NUMBER
         MH    R1,=H'50'           TIMES DSNTABLE ENTRY LENGTH
         AR    R1,R2               ADDRESS RELATIVE DSNTABLE ENTRY
         LA    R5,6(,R1)           ADDRESS DSN FOR MSG INSERT
         ICM   R5,B'1000',5(R1)    LENGTH OF INSERT
         TM    BLDTYPE,BLDALIAS    IS BLDMEM ACTUALLY AN ALIAS NAME ?
         BO    ALIAS               YES- PREPARE ALIAS MESSAGE
         MVC   MTMSGID(4),=C'MEMB' USE MEMBER MESSAGE
         ST    R5,MTINSRT+4        POINT TO DSN INSERT
         B     ISSUEMSG
ALIAS    MVC   MTMSGID(4),=C'ALIA' USE ALIAS MESSAGE
         ST    R5,MTINSRT+8        DSN IS THIRD INSERT IN ALIAS MSG
         MVI   MTINSRT+4,0         ASSUME NO SECOND INSERT
         TM    BLDATTR1,BLDRENT+BLDREUS REENTRANT OR REUSABLE MEMBER ?
         BZ    ISSUEMSG            NEITHER- MESSAGE COMPLETE AS IS
         MVC   BLDMEMA-3(3),=C'OF ' PREFIX REAL MEMBER NAME FOR INSERT
         LA    R1,BLDMEMA-3
         ST    R1,MTINSRT+4        POINT TO SECOND INSERT (REAL MEMBER)
         MVI   MTINSRT+4,11        LENGTH OF INSERT
ISSUEMSG EQU *
         CALLTSSR EP=IKJEFF02,MF=(E,MTPARMS)
NXTMEMBR LA    R8,ENTRYLEN(,R8)    INCREMENT TO NEXT MEMBER ENTRY
         BCT   R7,MEMLOOP          LOOP FOR SPECIFIED MEMBERS
***********************************************************************
*        CLOSE THE DATASET(S) AND LOOP IF 'ALL' REQUESTED             *
***********************************************************************
BLDLCLOS MVI   DBLWORD,X'80'       SET CLOSE OPTION - END OF LIST
         CLOSE (DCB),MF=(E,DBLWORD)
         LA    R2,50(,R2)          INCREMENT TO NEXT DSNAME
         LA    R3,10(,R3)          INCREMENT TO NEXT DDNAME
         BCT   R6,BLDLLOOP         LOOP FOR EACH DD (IF 'ALL' SPECIF)
*
***********************************************************************
*        DONE - FREE WORKAREAS (INCLUDING PARSE PDE'S)                *
***********************************************************************
*
         IKJRLSA (10)              RELEASE PARSE STORAGE
         LR    R1,R13              ADDRESS THE SAVEAREA/WORKAREA
         L     R13,4(,R13)
         FREEMAIN R,LV=CORENEED,A=(1)
         RETURN (14,12),RC=0
         EJECT
*
***********************************************************************
*        FAILURE MESSAGE ROUTINES FOR PARSE, AND DYNALLOC             *
***********************************************************************
*
PARSFAIL LA    R1,GFPARAM          ADDRESS GNRLFAIL WORKAREA
         USING GFDSECTD,R1
         XC    GFPARAM(GFLENGF),GFPARAM CLEAR THE PARM LIST
         MVI   GFCALLID,GFPARSE    INDICATE PARSE ERROR
         ST    R15,GFRCODE         ERROR RETURN CODE
         ST    R11,GFCPPLP         POINTER TO CPPL
         LA    R2,ECB
         ST    R2,GFECBP           POINTER TO PUTLINE ECB
         SR    R2,R2
         ST    R2,ECB              CLEAR THE ECB
         ST    R1,DBLWORD          POINT TO THE PARAMETER LIST
         DROP  R1
         LA    R1,DBLWORD          ADDRESS THE POINTER
         LINK  EP=IKJEFF19
         B     RETURN12
*
S99FAIL  LA    R1,DFPARAM          ADDRESS DAIRFAIL WORKAREA
         USING DFDSECTD,R1
         XC    DFPARAM(DFLEN),DFPARAM CLEAR THE PARMLIST
         ST    R8,DFS99RBP         POINT TO REQUEST BLOCK
         ST    R15,DBLWORD         SAVE THE RETURN CODE
         LA    R2,DBLWORD
         ST    R2,DFRCP            POINT TO RETURN CODE
         LA    R2,=F'0'
         ST    R2,DFJEFF02         SHOW IKJEFF02 IS NOT LOADED
         LA    R2,DFSVC99
         STH   R2,DBLWORD+4        SET CALLER ID TO INDICATE SVC99
         LA    R2,DBLWORD+4
         ST    R2,DFIDP            POINT TO CALLER ID
         ST    R11,DFCPPLP         POINTER TO CPPL
         DROP  R1
         LINK  EP=IKJEFF18
*
*        SET 'NOT IN USE' ATTRIBUTE FOR ALL RESOURCES, THEN FLUSH STACK
*
RETURN12 LA    R8,S99PARMS         ADDRESS S99 REQUEST BLOCK
         USING S99RB,R8
         XC    S99PARMS(S99RBLEN),S99PARMS CLEAR THE BLOCK
         MVI   S99RBLN,S99RBLEN    SET LENGTH
         MVI   S99VERB,S99VRBRI    SET FUNCTION TO 'REMOVE IN USE'
         LA    R1,S99TEXTP
         ST    R1,S99TXTPP         POINT TO TEXT POINTERS
         LA    R1,S99TEXT
         ST    R1,S99TEXTP         POINT TO TEXT
         MVC   S99TEXT(6),TCBAD    TEXT UNIT TO ADDRESS THIS TCB
         MVC   S99TEXT+6(4),PSATOLD SUPPLY CURRENT TCB ADDRESS FROM PSA
         MVI   S99TEXTP,S99TUPLN   INDICATE END OF TEXT UNIT POINTERS
         ST    R8,DBLWORD          POINT TO REQUEST BLOCK
         MVI   DBLWORD,S99RBPND    (HIGH BIT MUST BE ON)
         LA    R1,DBLWORD          ADDRESS THE POINTER
         DYNALLOC
         IKJRLSA PPLANSWR          RELEASE ANY STORAGE FOR PARSE
         L     R2,CPPLUPT          ADDRESS THE USER PROFILE TABLE
         L     R3,CPPLECT          ADDRESS  ENVIRONMENT CONTROL TABLE
         STACK DELETE=ALL,PARM=DBLWORD,UPT=(2),ECT=(3),ECB=ECB,        *
               MF=(E,S99TEXT)      (USE S99TEXT AS WORKAREA FOR IOPL)
         LR    R1,R13
         L     R13,4(,R13)
         FREEMAIN R,LV=CORENEED,A=(1)
         RETURN (14,12),RC=12
         EJECT
***********************************************************************
*                                  REGISTER USAGES                    *
***********************************************************************
R0       EQU   0
R1       EQU   1                   WORK
R2       EQU   2                   WORK
R3       EQU   3                   WORK
R4       EQU   4                   WORK
R5       EQU   5                   WORK
R6       EQU   6                   WORK
R7       EQU   7                   WORK
R8       EQU   8    *DUAL USE*     BLDL ARRAY BASE * S99 FUNCTIONS BASE
R9       EQU   9                   TSO MESSAGE ISSUER PARAMETER BASE
R10      EQU   10                  PARSE PPL AND PDL ADDRESSABILITY
R11      EQU   11                  CPPL ADDRESSABILITY
R12      EQU   12                  PROGRAM BASE
R13      EQU   13                  *
R14      EQU   14                  ** STANDARD LINKAGES
R15      EQU   15                  *
         SPACE 2
***********************************************************************
*                                  EQUATES                            *
***********************************************************************
MAXMEMS  EQU   16
ENTRYLEN EQU   46
MAXDSNS  EQU   40
PSATOLD  EQU   540                 ADDRESS OF CURRENT TCB, HEX LOC 21C
         SPACE 2
***********************************************************************
*        EXECUTED INSTRUCTIONS                                        *
***********************************************************************
MOVEMEM  MVC   BLDMEM-PDSBLDL(0,R8),0(R2) MOVE MEMBER NAME TO BLDL AREA
MOVEDSN  MVC   6(0,R3),0(R1)       MOVE DSNAME TO DSNTABLE SLOT
         SPACE 2
***********************************************************************
*        DYNAMIC ALLOCATION TEXT UNITS INITIAL VALUES                 *
***********************************************************************
DDSTEPL  DC    X'0001',X'0001',X'0008',CL8'STEPLIB'    DINDDNAM
RTDSN    DC    X'0005',X'0001',X'002C' PLUS CL44       DINRTDSN
RTATT    DC    X'000C',X'0001',X'0001' PLUS X          DINRTATT
SYS1LPA  DC    X'0002',X'0001',X'000B',C'SYS1.LPALIB'  DALDSNAM
SYS1LINK DC    X'0002',X'0001',X'000C',C'SYS1.LINKLIB'
SYS1SVC  DC    X'0002',X'0001',X'000B',C'SYS1.SVCLIB'
SYS1PARM DC    X'0002',X'0001',X'000C',C'SYS1.PARMLIB'
LNKLST00 DC    X'0003',X'0001',X'0008',C'LNKLST00'     DALMEMBR
LPALST00 DC    X'0003',X'0001',X'0008',C'LPALST00'     DALMEMBR
STATSHR  DC    X'0004',X'0001',X'0001',X'08'           DALSTATS
FREECLOS DC    X'001C',X'0000'                         DALCLOSE
RTDDN    DC    X'0055',X'0001',X'0008',CL8' '          DALRTDDN
TCBAD    DC    X'0001',X'0001',X'0004' PLUS XL4        DRITCBAD
         SPACE 2
***********************************************************************
*        OTHER CONSTANTS                                              *
***********************************************************************
ENDCHAIN DC    0F'0',X'FF000000'   END OF CHAIN OF PARSE PDE LIST
ERRPARML DC    C'PARMLIB LINKLIST MEMBER'
ERRSYSL  DC    AL1(34),C'CONCATENATED SYSTEM LINK LIBRARIES'
         PRINT NOGEN
         DS    0D
DCBPARML DCB   DSORG=PS,MACRF=(GL)
DCBSYSL  DCB   DSORG=PO,MACRF=(R)
         LTORG
         EJECT
***********************************************************************
*        WORKAREAS, DSECTS, AND MAPPING MACROES                       *
***********************************************************************
WORKAREA DSECT
         DS    18F                 STANDARD REGISTER SAVEAREA
DBLWORD  DS    D                   GENERAL PURPOSE DOUBLE WORD
ECB      DS    F                   TSSR COMMUNICATIONS ECB
PPLAREA  DS    7F                  PARSE PARAMETER LIST
PPLANSWR DS    F                   ADDRESS OF PDL (RETURNED BY PARSE)
GFPARAM  DS    11F                 GNRLFAIL/VSAMFAIL ROUTINE PARAMETERS
MTPARMS  DS    15F                 IKJEFF02 MESSAGE ISSUER PARM LIST
S99PARMS DS    5F                  DYNAMIC ALLOCATION REQUEST BLOCK
S99TEXTP DS    5F                  DYNALLOC TEXT UNIT POINTERS
S99TEXT  DS    CL256               DYNALLOC TEXT UNITS (AND WORKAREA)
DFPARAM  DS    5F                  DAIRFAIL ROUTINE PARAMETERS
PARMNAME DS    XL14                                              WDPSC
L1LS     DS    F                                                 WDPSC
PLIST    DS    4F                                                WDPSC
DSNCNT   DS    H                                                 WDPSC
DSNTABLE DS    0XL6                TEXT UNIT KEYS FOR DATASETS TO BE
         DS    0XL44                 ALLOCATED AND SEARCHED.
         DS    (MAXDSNS)CL50         ROOM FOR STEP,LPA,SVC, & 16 LINKS
DCB      DS    0D,CL96             DCB TO READ PARMLIB AND OPEN SYSLIBS
DDNTABLE DS    XL4                 TEXT UNIT KEY TO CONCATENATE DDNAMES
         DS    0XL2                  RETURNED BY DYNALLOC OF DSNTABLE.
         DS    0XL8                  LENGTH OF EACH DDNAME FORCED TO 8
         DS    (MAXDSNS)CL10         (TRAILING BLANKS IN VALUE ARE OK)
BLDLIST  DS    H                   NUMBER OF ENTRIES IN LIST
BLDELEN  DS    H                   LENGTH OF EACH ENTRY
BLDAREA  DS    (MAXMEMS)CL(ENTRYLEN) ARRAY OF MEMBER NAME ENTRIES
CORENEED EQU   *-WORKAREA          ***LENGTH NEEDED FOR GETMAIN***
         SPACE 3
PDSBLDL  DSECT
BLDMEM   DS    CL8                 MEMBER OR ALIAS NAME
BLDTT    DS    XL2                 TTR TRACK OF FIRST BLOCK
BLDR     DS    X                   TTR RECORD
BLDK     DS    X                   CONCATENATED DATASET NUMBER
BLDLIBRY DS    X                   LIBRARY IDENTIFIER WHERE FOUND:
BLDPRIV  EQU   0                     FOUND IN PRIVATE LIBRARY
BLDLINK  EQU   1                     FOUND IN SYSTEM LINK LIST LIBRARY
BLDJSTEP EQU   2                     FOUND IN JOB, STEP, OR TASK LIB
BLDTYPE  DS    X
BLDALIAS EQU   X'80'               BLDMEM FIELD IS AN ALIAS NAME
         DS    XL3                 TTR OF FIRST TEXT BLOCK
         DS    X                   RESERVED
         DS    XL3                 TTR OF NOTE LIST OR SCATTER TABLE
         DS    X                   NUMBER OF NOTE ENTRIES
BLDATTR1 DS    X                   ATTRIBUTE BYTE 1
BLDRENT  EQU   X'80'               REENTERABLE
BLDREUS  EQU   X'40'               REUSABLE
BLDATTR2 DS    X                   ATTRIBUTE BYTE 2
         DS    XL3                 MAIN STORAGE NEEDED FOR MODULE
         DS    XL2                 LENGTH OF FIRST TEXT BLOCK
         DS    XL3                 ENTRY-POINT ADDRESS
         DS    XL3                 FIRST TEXT BLOCK ORIGIN
         DS    XL3                 ENTRY-POINT FOR MEMBER-NAME IF ALIAS
BLDMEMA  DS    CL8                 LOAD MODULE MEMBER NAME IF ALIAS
         SPACE 3
LOCATE   CSECT
PARSEPCL IKJPARM
MEMNAME  IKJIDENT 'MEMBER NAME(S)',LIST,MAXLNTH=8,FIRST=ALPHA,         *
               OTHER=ALPHANUM,PROMPT='MEMBER NAME(S)',HELP=('NAMES OF M*
               EMBERS TO BE SEARCHED FOR IN THE SYSTEM LIBRARIES')
ALL      IKJKEYWD
         IKJNAME 'ALL'
         IKJENDP
         PRINT GEN
         IKJCPPL
         IKJPPL
         IKJEFFGF GFDSECT=YES
         IKJEFFMT MTDSECT=YES
         IEFZB4D0
S99RBLEN EQU   S99RBEND-S99RB
         IKJEFFDF DFDSECT=YES
DFPEND   EQU   *
         CVT   DSECT=YES           (DISTRIBUTED IN SYS1.AMODGEN)
         EJECT
MESSAGES CSECT
         IKJTSMSG ('TOO MANY MEMBER NAMES FOR INTERNAL TABLE -- LIST TR*
               UNCATED'),OVFL
         IKJTSMSG ('STEPLIB FOUND TO BE CONCATENATED GROUP -- NOT SUPPO*
               RTED, IGNORED'),STEP
         IKJTSMSG ('UNABLE TO OPEN ',,' -- LOCATE TERMINATED'),NOPN
         IKJTSMSG ('BLDL FAILED FOR ',,' -- BLDL RETURN CODE ',),BLDL
         IKJTSMSG (,' IS A MEMBER IN ',),MEMB
         IKJTSMSG (,' IS AN ALIAS ',,' IN ',),ALIA
         IKJTSMSG (,' NOT FOUND IN ',),NFND
         IKJTSMSG
         END
@@
//LKED.SYSLMOD DD  DISP=SHR,DSN=SYS2.CMDLIB(LOCATE)
//LKED.SYSLIB  DD  DSN=SYS2.LINKLIB,DISP=SHR
//*
//* -------------------------------------------------------*
//* *  Assemble Link-Edit FINDMEM to SYS2.CMDLIB           *
//* -------------------------------------------------------*
//FINDMEM  EXEC  ASML,
//         PARM.ASM='NODECK,LOAD,TERM,XREF',
//         PARM.LKED='MAP,LIST,LET,XREF'
//***      PARM.LKED=(XREF,LET,LIST,NCAL)
//ASM.SYSIN DD DATA,DLM=@@
         TITLE    'FINDMEM'                                             00001
*********************************************************************** 00002
* FUNCTION: TSO CP TO FIND FIRST OCCURANCE OF MEMBER IN A             * 00003
*           CONCATENATION.                                            * 00004
*                                                                     * 00004
* INPUT: PARM1 - MEMBER NAME TO FIND                                  * 00005
*        PARM2 - OPTIONAL - DDNAME TO SEARCH. IF NOT SPECIFIED,       * 00006
*                LINKLIST IS SEARCHED.                                * 00007
*                                                                     * 00008
* OUTPUT: RETURN CODE IN R15:                                         * 00009
*           0 = MEMBER FOUND                                          * 00010
*           4 = MEMBER NOT FOUND                                      * 00011
*          16 = PARM ERRROR                                           * 00012
*          20 = DDNAME NOT ALLOCATED                                  * 00012
*                                                                     * 00013
*         VARIABLES TO ISPF:                                          * 00013
*           FMCONCAT - CONCATENATION NUMBER WHERE MEMBER WAS FOUND.   * 00013
*           FMDSNAME - DSNAME WHERE MEMBER WAS FOUND                  * 00013
*           FMLIB    - LIBRARY TYPE WHERE MEMBER WAS FOUND            * 00013
*                        'PRIVATE', 'LINKLIST' OR 'STEPLIB'           * 00013
*           FMMSG    - ERROR MSG WHEN NON ZERO RC                     * 00013
*           FMDIRENT - UNFORMATTED DIRECTORY ENTRY FROM BLDL          * 00013
*           FMLKEDDT - LINK EDIT DATE                                 * 00013
*                                                                     * 00013
* WRITTEN BY: ROB WUNDERLICH                                          * 00013
*                                                                     * LB
* MODIFIED  : LARRY BELMONTES 2019-07-02                              * LB
*             For use under MVS 3.8J / Hercules and                   * LB
*             ISPF 2.1 Product Wally Mclaughlin                       * LB
*             - LABEL shorten to 8 characters due to IFOX00           * LB1
*             - LLA not supported in MVS 3.8J, branch around code,    * LB2
*               resolved CVTLLTA for assembly, but do NOT use!        * LB2
*             - Used VL=1 in  ISPLINK call parm list to set           * LB3
*               high-order bit of last address parameter to 1.        * LB3
*             - Commented lines that set LKED DATE size and move      * LB4
*               directory entry data up in the ISPF variable array.   * LB4
*               VREPLACE require lengths > 0 (e.g. LKED DATE)         * LB4
*             - Defaulted lengths of FMLKEDDT and FMDIRENT            * LB5
*                                                                     * 00013
* LIMITATIONS: THE SECTION LABELED 'GETLKDAT' WAS CODED TO READ       * 00013
*              MODULES GENERATED BY LKED. I DON'T THINK IT WILL       * 00013
*              WORK ON BINDER FORMAT LMODS.                           * 00013
*                                                                     * 00013
*********************************************************************** 00014
         MACRO
         REGISTER
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
RA       EQU   10
R10      EQU   10
RB       EQU   11
R11      EQU   11
RC       EQU   12
R12      EQU   12
RD       EQU   13
R13      EQU   13
RE       EQU   14
R14      EQU   14
RF       EQU   15
R15      EQU   15
         MEND
*
*
         MACRO                                                          00001
&N       DDFIND &DDNAME=X,&LENGTH=8,&DDLABEL=                           00002
.********************************************************************** 00003
.*                                                                      00004
.*        -- WRITTEN BY: ROB WUNDERLICH  12/86                          00005
.*                                                                      00006
.*       THE DDFIND MACRO SCANS THE TIOT FOR THE SPECIFIED DDNAME.      00007
.*       THE ADDRESS OF THE TIOT ENTRY IS RETURNED IN REG 1. R1 = 0     00008
.*       IF DDNAME NOT FOUND.                                           00009
.*                                                                      00010
.*       DDFIND  DDNAME=NAME                                            00011
.*                                                                      00012
.*       THE CONTENTS OF REGS 0 AND 1 ARE ALTERED BY THIS MACRO.        00013
.*                                                                      00014
.********************************************************************** 00015
.*                                                                      00016
.*                                                                      00017
         AIF   (T'&N EQ 'O').NOLBL     IF NO LABEL SKIP AROUND          00018
&N       EQU   *                  GENERATE LABEL                        00019
.NOLBL   ANOP                                                           00020
.*                                                                      00021
         B     *+X'C'             BRANCH AROUND CONSTANT                00023
&DDLABEL DC    CL8'&DDNAME'       CONSTANT FOR COMPARE                  00024
         L     1,16               LOAD CVT ADDRESS                      00025
         L     1,0(,1)            LOAD TCB WORDS ADDRESS                00026
         L     1,4(,1)            LOAD TCB ADDRESS                      00027
         L     1,12(,1)           LOAD ADDRESS OF TIOT                  00028
         LA    1,24(,1)           LOAD ADDRESS OF FIRST DD ENTRY        00029
         XR    0,0                CLEAN R0                              00030
         ICM   0,B'0001',0(1)     GET LEN OF THIS DD ENTRY              00031
         BZ    *+X'14'            IF ZERO, THEN END OF TIOT             00032
         CLC   4(&LENGTH,1),*-X'26' TEST FOR DDNAME                     00033
         BE    *+X'C'             BRANCH OUT IF FOUND                   00034
         AR    1,0                INCREMENT TO NEXT DD ENTRY            00035
         B     *-X'14'            GO CHECK THE NEXT ENTRY               00036
         XR    1,1                ZERO R15 - ENTRY NOT FOUND            00037
         MEND                                                           00038
         EJECT                                                          00015
FINDMEM  CSECT                                                          00016
         SAVE  (14,12),,FINDMEM*&SYSDATE&SYSTIME                        00017
         REGISTER                 EQUATE REGISTERS                      00018
         LR    R12,R15                                                  00019
         USING FINDMEM,R12                                              00020
         ST    R13,SAV+4          STORE ADDRESS OF HIS SAVEAREA         00021
         LR    R2,R13             SAVE ADDRESS OF HIS SAVEAREA          00022
         LA    13,SAV             LOAD ADDRESS OF MY SAVEAREA           00023
         ST    13,8(2)            STORE ADDRESS OF MY SAV IN HIS SAV    00024
*                                                                       00025
*                                                                       00026
GETPARM  EQU   *                                                        00027
         L     R3,0(,R1)          LOAD COMMAND BUFFER POINTER
*
*
         LH    R2,0(,R3)          LOAD LENGTH OF COMMAND BUFFER
         SH    R2,=H'4'           SUBTRACT LENGTH OF BUFFER HEADER
         LH    R1,2(,R3)          LOAD COMMAND LENGTH
         LA    R3,4(R1,R3)        POINT TO START OF PARMS
         SR    R2,R1              COMPUTE PARM LENGTH
         LTR   R2,R2              ANY PARM?
         BP    GOTPARM            YES - BRANCH AROUND
         MVC   RETCODE,=H'16'     SET RETURN CODE
         MVC   FMMSG,=CL80'NO PARM SUPPLIED'
         B     SETVARS
*
GOTPARM  EQU   *                  SCAN PARMS
         LR    R15,R3             SAVE PARM START
P1SCAN   CLI   0(R3),C' '         BLANK?
         BE    P1FND
         LA    R3,1(,R3)          INCREMENT POINTER
         BCT   R2,P1SCAN          KEEP LOOKING
P1FND    LR    R14,R3             LOAD FOUND ADDRESS
         SR    R14,R15            GET LENGTH OF PARM
         CH    R14,=H'8'          CHECK MAX LENGTH
         BNH   P1FNDA             OK
         MVC   RETCODE,=H'16'     SET RETURN CODE
         MVC   FMMSG,=CL80'MEMBER NAME EXCEEDS 8 CHARACTERS'
         B     SETVARS            GET OUT
P1FNDA   BCTR  R14,0              MAKE MACHINE LENGTH
         EX    R14,MVPARM1        MOVE THE PARM
         OC    BLMEMBER,=8X'40'   MAKE UPPERCASE
*
         LTR   R2,R2              ANY MORE PARM?
         BNP   PARMEND            NO - BRANCH AROUND
*  GET DDNAME PARM
P2SET    LA    R3,1(,R3)          POINT PAST BLANK
         BCTR  R2,0               DECREMENT LENGTH FOR BLANK
         LTR   R2,R2              ANY MORE PARM?
         BNP   PARMEND            NO - BRANCH AROUND
         CLI   0(R3),C' '         BLANK?
         BE    P2SET              YES - CHECK NEXT BYTE
         LR    R15,R3             SAVE PARM START
P2SCAN   CLI   0(R1),C' '         BLANK?
         BE    P2FND
         LA    R3,1(,R3)          INCREMENT POINTER
         BCT   R2,P2SCAN          KEEP LOOKING
P2FND    LR    R14,R3             LOAD FOUND ADDRESS
         SR    R14,R15            GET LENGTH OF PARM
         CH    R14,=H'8'          CHECK MAX LENGTH
         BNH   P2FNDA             OK
         MVC   RETCODE,=H'16'     SET RETURN CODE
         MVC   FMMSG,=CL80'DDNAME EXCEEDS 8 CHARACTERS'
         B     SETVARS            GET OUT
P2FNDA   BCTR  R14,0              MAKE MACHINE LENGTH
         EX    R14,MVPARM2        MOVE THE PARM
         OC    DDNAME,=8X'40'     MAKE UPPERCASE
         MVC   FINDLIB+(DCBDDNAM-IHADCB)(8),DDNAME   MOVE DDN TO DCB
*
PARMEND  EQU   *
*
         CLC   DDNAME,=CL8' '     DDNAME SUPPLIED?
         BE    NODDN1             NO - BRANCH AROUND
         MVC   DDFNAME,DDNAME     SET DDNAME TO LOOK FOR
         DDFIND DDLABEL=DDFNAME   CHECK TIOT FOR DDNAME
         LTR   R1,R1              DDNAME IN TIOT?
*        BZ    DD_NOT_ALLOCATED   NO -GO TAKE AN ERROR                  LB1
         BZ    DDNOALOC           NO -GO TAKE AN ERROR                  LB1
         ST    R1,TIOTPTR         SAVE PTR INTO TIOT
         OPEN  (FINDLIB,INPUT)
         BLDL  FINDLIB,BLDLLIST   SEARCH USER SUPPLIED DDNAME
         B     CHKBLDL            BRANCH AROUND
NODDN1   BLDL  0,BLDLLIST         SEARCH LINKLIB
CHKBLDL  LTR   R15,R15            FOUND?
         BZ    FOUND              YES
         MVC   FMMSG,=CL80'MEMBER NOT FOUND'
         MVC   RETCODE,=H'4'
         B     SETVARS
FOUND    CLC   DDNAME,=CL8' '     DDNAME SUPPLIED?
         BE    LLIBDSN            NO - GO CHECK FOR LINKLIB DSN
         CLOSE (FINDLIB)                                                00033
*  USER SUPPLIED A DDNAME - DETERMINE THE DSN OF THE LIB WHERE FOUND
         XR    R3,R3               CLEAR R3
         L     R2,TIOTPTR          GET SAVED PTR INTO TIOT
         USING TIOENTRY,R2         MAP IT
         XR    R1,R1               CLEAR R1
         IC    R1,BLCONCAT         GET THE CONCATENATION NUMBER
         LA    R1,1(,R1)           PLUS 1
DDINDEX1 BCT   R1,DDINDEX2         INDEX INTO THIS CONCATENATION
         XR    R1,R1               CLEAR R1
         ICM   R1,B'0111',TIOEJFCB GET JFCB ADDRESS
         LA    R1,16(,R1)          PLUS 16 - WHY DO I NEED THIS OFFSET?
         USING INFMJFCB,R1         MAP JFCB
         MVC   FMDSNAME,JFCBDSNM   BRANCH AROUND
         B     GOTDSN              BRANCH AOUND
         DROP  R1
DDINDEX2 IC    R3,TIOELNGH         GET LENGTH OF THIS ENTRY
         AR    R2,R3               POINT TO NEXT ENTRY
         B     DDINDEX1            KEEP INDEXING
         DROP  R2
*
GOTDSN   EQU   *
         B     MAKEVARS            BRANCH AROUND
*
*
LLIBDSN  EQU   *                   GET DSN FOR LINKLIST DATASET
         CLI   BLLIB,1             WAS MEMBER FOUND IN LINKLIST LIB?
         BNE   MAKEVARS            NO - BRANCH
         MVC   FMDSNAME,=CL44'LINKLIST DSN NOT KNOWN!'                  LB2
         B     MAKEVARS                                                 LB2
         L     R2,16               LOAD CVT ADDRESS
         USING CVTMAP,R2           MAP CVT
*        L     R2,CVTLLTA          GET LINKLIST TABLE ADDR              LB2
         L     R2,(1244)(R2)                                            LB2
         DROP  R2                  DROP CVT MAPPING
*  (CAN'T FIND IHALLT MAPPING MACRO)
         LA    R2,8(,R2)           POINT TO FIRST ENTRY
         XR    R1,R1               CLEAR R1
         IC    R1,BLCONCAT         GET THE CONCATENATION NUMBER
         MH    R1,=H'45'           TIMES THE LENGTH OF AN ENTRY
         LA    R2,1(R1,R2)         POINT TO DSNAME
         MVC   FMDSNAME,0(R2)      GET THE LINKLIST DSNAME
*
*
MAKEVARS EQU   *
*  CONVERT CONCATENATION NUMBER TO CHARACTER
         XR    R1,R1               CLEAR R1
         IC    R1,BLCONCAT         GET THE CONCATENATION NUMBER
         CVD   R1,DWORK           CONVERT TO DECIMAL
         UNPK  FMCONCAT,DWORK     UNPK TO CHARACTER
         OI    FMCONCAT+(L'FMCONCAT-1),X'F0'     FIX SIGN BYTE
*
*  CONVERT LIBRARY TYPE TO A WORD
         MVC   FMLIB,=CL8'PRIVATE'  ASSUME PRIVATE LIB
         CLI   BLLIB,0              IS IT?
         BE    EOLIBSET             YES - BRANCH OUT
         MVC   FMLIB,=CL8'LINKLIST' ASSUME LINKLIST LIB
         CLI   BLLIB,1              IS IT?
         BE    EOLIBSET             YES - BRANCH OUT
         MVC   FMLIB,=CL8'STEPLIB'  MUST BE STEPLIB/JOBLIB
EOLIBSET EQU   *
         MVC   FMDIRENT,BLMEMBER  MOVE DIR ENTRY TO ISPF AREA
         XR    R1,R1              CLEAR R1
         IC    R1,BLINDC          GET BLDL 'C' BYTE
         N     R1,=X'0000001F'    TURN OFF ALL BUT USERDATA LEN BITS
         SLL   R1,1               TIMES 2 TO GET ACTUAL LENGTH
         AH    R1,=H'14'          PLUS LENGTH OF FIXED HEADER
         ST    R1,VLDIRENT        SET LENGTH IN PARM LIST
*
GETLKDAT EQU   *                  GET LKED DATE FOR LOAD MODULES
*  HAVE TO BAIL OUT HERE UNTIL DYNALLOC CODE FOR LINKLIST/STEPLIB
         CLI   BLLIB,0            PRIVATE LIBRARY (ALEADY ALLOCATED)?
         BNE   EOLKEDDT           NO - BRANCH AROUND
*
         CH    R1,=H'14'          ANY USERDATA?
         BE    EOLKEDDT           NO - BRANCH AROUND
         CH    R1,=H'44'          SPF LIBRARY?
         BE    EOLKEDDT           YES - BRANCH AROUND
*  ASSUME LOAD LIBRARY DIRECTORY
         OPEN  (FINDLIB,INPUT)
         FIND  FINDLIB,BLTTR,C    POINT DCB TO MEMBER                   00106
READLOOP EQU   *                                                        00107
         READ  DECB1,SF,FINDLIB,IDRREC,20  READ 20 BYTES OF RECORD      00108
         CHECK DECB1                                                    00109
         CLI   IDRREC,X'01'       PAST THE IDR RECORDS?                 00110
         BE    NOLKEDDT           YES - BRANCH TO ERROR                 00111
         CLI   IDRREC,X'80'       IDR RECORD?                           00112
         BNE   READLOOP           NO - TRY NEXT RECORD                  00113
         TM    IDRREC+2,X'02'     LKED IDR RECORD?                      00114
         BNO   READLOOP           NO - TRY NEXT RECORD                  00115
         UNPK  FMLKEDDT(5),IDRREC+15(3)  UNPACK LKED DATE               00129
         OI    FMLKEDDT+4,X'F0'   FIX SIGN                              00130
         MVC   VLLKEDDT,=F'5'     SET LENGTH IN ARRAY
NOLKEDDT EQU   *
         CLOSE (FINDLIB)          CLOSE DS
EOLKEDDT EQU   *
*
SETVARS  LINK  EP=ISPQRY          SEE IF ISPF IS ACTIVE
         LTR   R15,R15            IS ISPF ACTIVE?
         BNZ   BYE
*        CLC   VLLKEDDT,=F'0'     DID WE SET LKED DATE VARIABLE?        LB4
*        BNE   SETVAR2            YES - BRANCH AROUND                   LB4
*        MVC   VLLKEDDT,=F'5'     SET LENGTH IN ARRAY     LB            LB4
*        MVC   FMLKEDDT(L'FMDIRENT),FMDIRENT  NO LKED DATA VAR, SO MOVE LB4
*                                             DIRENT UP IN ARRAY
*SETVAR2  LINK  EP=ISPLINK,PARAM=(VREPLACE,VARLIST,VLENGTH,VVARS),VL    LB3
SETVAR2  LINK  EP=ISPLINK,PARAM=(VREPLACE,VARLIST,VLENGTH,VVARS),VL=1   LB3
         B     BYE                EXIT
*                                                                       00029
*                                                                       00031
BYE      EQU   *                                                        00032
         LH    R15,RETCODE
         L     R13,SAV+4                                                00034
         RETURN (14,12),RC=(15)                                         00035
*
*DD_NOT_ALLOCATED   EQU  *                                              LB1
DDNOALOC EQU   *                                                        LB1
         MVC   FMMSG,=CL80'DDNAME NOT ALLOCATED'
         MVC   RETCODE,=H'20'
         B     SETVARS
*                                                                       00036
MVPARM1  MVC   BLMEMBER(0),0(R15)
MVPARM2  MVC   DDNAME(0),0(R15)
************************ DATA AREAS AND DCB'S ************************* 00037
*                                                                       00038
SAV      DS    18F                                                      00039
DWORK    DS    D
TIOTPTR  DC    F'0'               PTR TO DDNAME ENTRY IN TIOT
DDNAME   DC    CL8' '             DDNAME TO SEARCH
RETCODE  DC    H'0'               RETURN CODE
IDRREC   DS    CL20               INPUT AREA FOR LKED DATE SEARCH
*  KEEP TOGETHER *
BLDLLIST DS    0H
         DC    H'1'               NUMBER OF ENTRIES IN THE LIST
         DC    AL2(BLEND-BLMEMBER)  LENGTH OF ENTRY
BLMEMBER DC    CL8' '             MEMBER NAME
BLTTR    DS    XL3
BLCONCAT DS    X                  CONCATENATION NUMBER WHERE FOUND
BLLIB    DS    X                  FOUND IN LINKLIB, JOBLIB, ETC
BLINDC   DS    X                  ALIAS INDICATOR/USER DATA LENGTH
         DS    CL62               USER DATA
BLEND    EQU   *
* END KEEP TOGETHER *
*
VREPLACE DC    CL8'VREPLACE'      COMMAND TO ISPLINK
*  VARIABLE NAMES FOR ISPF
VARLIST  DC    C'(FMCONCAT FMDSNAME FMLIB FMMSG FMLKEDDT FMDIRENT)'
** VARIABLE ARRAY FOR VREPLACE ***
VVARS    EQU   *
FMCONCAT DC    CL3' '
FMDSNAME DC    CL44' '
FMLIB    DC    CL8' '
FMMSG    DC    CL80' '
FMLKEDDT DC    CL5'*****'
FMDIRENT DC    CL76' '
** LENGTH ARRAY FOR VREPLACE ***
VLENGTH  DS    0F
         DC    AL4(L'FMCONCAT)
         DC    AL4(L'FMDSNAME)
         DC    AL4(L'FMLIB)
         DC    AL4(L'FMMSG)
*VLLKEDDT DC    AL4(0)             LENGTH OF LKED DATE                  LB5
*VLDIRENT DC    AL4(0)             LENGTH OF DIRECTORY ENTRY            LB5
VLLKEDDT DC    AL4(L'FMLKEDDT)    LENGTH OF LKED DATE                   LB5
VLDIRENT DC    AL4(L'FMDIRENT)    LENGTH OF DIRECTORY ENTRY             LB5
**
*                                                                       00040
FINDLIB  DCB   DDNAME=X,MACRF=R,DSORG=PO,EODAD=NOLKEDDT                 00041
         LTORG
         CVT   DSECT=YES
         DCBD  DEVD=DA
         DSECT
         IEFTIOT1
         DSECT
         IEFJFCBN
         END   FINDMEM                                                  00045
@@
//LKED.SYSLMOD DD  DISP=SHR,DSN=SYS2.CMDLIB(FINDMEM)
//PLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.PLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=PFINDM
/********************************************************************/
/*                                                                  */
/*    PANEL: PFINDM                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x FIND Menu Panel                                         */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/23/2020 0.9.10   Larry Belmontes Jr.                          */
/*                     - Added options 4 and 5                      */
/*                                                                  */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
%   TYPE(TEXT)   INTENS(HIGH)
+   TYPE(TEXT)   INTENS(LOW)
_   TYPE(INPUT)  INTENS(HIGH) CAPS(ON) JUST(LEFT)
[   TYPE(TEXT)   INTENS(HIGH)                      COLOR(TURQ)
]   TYPE(TEXT)   INTENS(LOW)       HILITE(USCORE)  COLOR(PINK)
{   TYPE(TEXT)   INTENS(HIGH)                      COLOR(GREEN)
}   TYPE(TEXT)   INTENS(HIGH)                      COLOR(YELLOW)
$   TYPE(TEXT)   INTENS(LOW)       HILITE(REVERSE) COLOR(BLUE)
?   TYPE(OUTPUT) INTENS(LOW)           JUST(LEFT)
)BODY WIDTH(&ZSCREENW) EXPAND(\\)
%----------------------$        FINDSRCH Menu        %--------------------------
%COMMAND ===>_ZCMD                                                             +
+
% 1 [Find{MEMBER[in}DSL                                                ?Z
% 2 [List Datasets in}DSL                                              ?Z
% 3 [Find members in}DSL[with{SEARCHstr
%   [  optionally grouped by{MEMBER[
% 4 [Find{MEMBER[in system libraries
% 5 [Find{MEMBER[in allocated{DDName
%
%
%}DSL%(Dataset List) includes dataset names with{DSNstr%located at{DSNstrPOS%
% where {S[start%of DSN, or {E[end%of DSN, or  {C[contained%in DSN.
%
{DSNstrPOS%===>_Z+ S/E/C
{DSNstr%======>_F1DSN                                         +
{DDName%======>_F1DDN   +
{MEMBER%======>_F1MEMBER+
{SEARCHstr %==>_F1SRCH                                      +Use single quotes
{Fast Search%=>_Z+ Y/N  Stop string member search after first found occurrence
{ListAll DSN%=>_Z+ Y/N  List all datasets processed (options 1-3)
%
+
)INIT
.ZVARS = '(ZUSER ZPANELID F1LVL F1QK F1ALLDSN)'
.HELP = HFINDM
.CURSOR = ZCMD
&PRM2 = ''
&RESP0 = .RESP
&KEYPRESS = .PFKEY
&LSCREEN = &ZSCREEN
VPUT (LSCREEN) SHARED

VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE
VGET (F1LVL F1DDN) PROFILE

)REINIT
.CURSOR = ZCMD
&RESP0 = .RESP
&KEYPRESS = .PFKEY
REFRESH(*)          /* refresh all fields */

)PROC

&RESP0 = .RESP
&KEYPRESS = .PFKEY

&OPTS = TRUNC (&ZCMD,'.')              /* Option Selected */
&OPTT = .TRAIL                         /* Option Trail    */

IF (&KEYPRESS EQ PF00)        /* ENTER key  */
  IF (&OPTS EQ 1)
    VER (&F1LVL,NB,MSG=FIND001)
    VER (&F1LVL,LIST,C,S,E,MSG=FIND003)
    VER (&F1DSN,NB,MSG=FIND001)
    VER (&F1DSN,DSNAME,MSG=FIND002)
    VER (&F1MEMBER,NB,MSG=FIND001)
    VER (&F1ALLDSN,NB,MSG=FIND001)
    VER (&F1ALLDSN,LIST,Y,N,MSG=FIND003)
  IF (&OPTS EQ 2)
    VER (&F1LVL,NB,MSG=FIND001)
    VER (&F1LVL,LIST,C,S,E,MSG=FIND003)
    VER (&F1DSN,NB,MSG=FIND001)
    VER (&F1DSN,DSNAME,MSG=FIND002)
    VER (&F1ALLDSN,NB,MSG=FIND001)
    VER (&F1ALLDSN,LIST,Y,N,MSG=FIND003)
  IF (&OPTS EQ 3)
    VER (&F1LVL,NB,MSG=FIND001)
    VER (&F1LVL,LIST,C,S,E,MSG=FIND003)
    VER (&F1DSN,NB,MSG=FIND001)
    VER (&F1DSN,DSNAME,MSG=FIND002)
    /*   &F1MEMBER is optional   */
    VER (&F1SRCH,NB,MSG=FIND001)
    VER (&F1QK,NB,MSG=FIND001)
    VER (&F1QK,LIST,Y,N,MSG=FIND003)
    VER (&F1ALLDSN,NB,MSG=FIND001)
    VER (&F1ALLDSN,LIST,Y,N,MSG=FIND003)
  IF (&OPTS EQ 4)
    VER (&F1MEMBER,NB,MSG=FIND001)
  IF (&OPTS EQ 5)
    VER (&F1MEMBER,NB,MSG=FIND001)
    IF (&F1DDN > ' ')
      &PRM2 = 'DD(&F1DDN)'
    ELSE
      &PRM2 = ''

  &DEBUG = ''                          /* Default NO DEBUG*/

  VPUT (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE
  VPUT (F1LVL F1DDN) PROFILE

  IF (&OPTT = DEBUG)
    &DEBUG = 'DEBUG'

  &ZSEL = TRANS( TRUNC (&ZCMD,'.')     /* Process options */
                1,'CMD(%CMEMFIND &DEBUG) NEWAPPL(FIND)'
                2,'CMD(%CVOL &DEBUG) NEWAPPL(FIND)'
                3,'CMD(%CSRCH &DEBUG) NEWAPPL(FIND)'
                4,'CMD(%CLOC8 &DEBUG) NEWAPPL(FIND)'
                5,'CMD(%CMEMINDD &DEBUG MEM(&F1MEMBER) &PRM2) NEWAPPL(FIND)'
              ' ',' '
                *,'?' )
  &ZTRAIL = .TRAIL

)END
./ ADD NAME=HFINDM
/********************************************************************/
/*                                                                  */
/*    PANEL: HFINDM                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Help panel for PFINDM                                   */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/23/2021 0.9.10   Larry Belmontes Jr.                          */
/*                     - Update HELP content with new options       */
/*                       (4, 5)                                     */
/*                                                                  */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 [ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(TURQ)
 [ TYPE(TEXT)   INTENS(LOW)  HILITE(USCORE)
 ? TYPE(TEXT)   INTENS(LOW)  HILITE(REVERSE) COLOR(BLUE)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
)BODY EXPAND(//)
%--Tutorial------------?        FINDSRCH Menu        %---------------+v0.9.10 %-
%Command ===>_ZCMD                                                     {Z
%
% 1 [Find+MEMBER[in~DSL%searches a set of PO datasets for a specified member
%  e.g. Display DSNs[containing~HERC01.TEST%with member name~MYNAME%
% 2 [List Datasets[in~DSL%searches all online DASD volumes for a specified DSN
%  e.g. Display DSNs[starting with~HERC01.TEST%
% 3 [Find members[in~DSL[with+SEARCHstr%searches a set of PO datasets
%    for a specified+SEARCHstr.%Optionally, grouping by+MEMBER.
%  e.g. Search for~string%in PO DSNs that[contain~HERC01.TEST%and grouped by
%      ~LB%   (members names starting with 'LB')
% 4 [Find+MEMBER[in system libraries%
%  e.g. Search for~MYLOAD%in system search order (STEPLIB,LPALIB,LINKLIB,
%       LINKLIST,SVCLIB)
% 5 [Find+Member[in allocated+DDName%
%  e.g. Search for~MYLOAD%in session allocated DD dataset(s)
%
%Dataset List is created from DASD VTOCs (catalogued and not catalogued DSNs)
%that include a specified+DSNstrPOS%and+DSNstr%value.
%
%Tabular results are scrollable containing dataset name and other information
%based on requested option.  Each dataset may be conveniently Browsed or Edited.
%
%Options 4-5 present results in a browse session or terminal line displays.
)INIT
 .CURSOR = ZCMD
 .ZVARS = (ZPANELID)
)PROC

)END
./ ADD NAME=PMEMFIND
/********************************************************************/
/*                                                                  */
/*    PANEL: PMEMFIND                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Member Find panel used by CMEMFIND                      */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR DEFAULT(%@_)
%   TYPE(TEXT)   INTENS(HIGH)
@   TYPE(TEXT)   INTENS(LOW)
_   TYPE(INPUT)  INTENS(HIGH) CAPS(ON)  JUST(LEFT)
!   TYPE(OUTPUT) INTENS(LOW)  CAPS(OFF) JUST(ASIS)
$   TYPE(TEXT)   INTENS(LOW)       HILITE(REVERSE) COLOR(BLUE)
[   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(TURQ)
]   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(YELLOW)
{   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(GREEN)
}   TYPE(TEXT)   INTENS(HIGH)                      COLOR(YELLOW)
?   TYPE(OUTPUT) INTENS(LOW)            JUST(LEFT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%-------------------$  Find Member Results  %-----------------------------------
%COMMAND ===>_ZCMD                                             %SCROLL ===>_AMT
%
}DSN &DSNBY  :[Z                                            %          ?Z
}MEMBER Name :[Z       }ListAll:[Z%                                    ?Z
%
%PF3-End   PF7-Up   PF8-Down
%Dataset can be Browsed or Edited
%
%DSNs            DSNs           Not Found     Found         Total
%Extracted:!Z   %Searched:!Z   %Members:!Z   %Members:!Z   %Others:!Z   %
%S   Dataset Name-------------------------------- Member   Found Volume
)MODEL
_Z  [Z                                           {Z       % {Z  {Z     %
)INIT
.HELP = HMEMFIND
.CURSOR = ZCMD
.ZVARS = '(PDSN ZUSER PMEM ALLQDSNS ZPANELID
           TOTDSN TSRCH NULC TMBS THIT
           SEL TDSN TMEM TFND TVOL)'
&AMT=CSR
)PROC
/* ISPF 2.0  Pass ZCMD      value  to CLIST  */
 &LCMD = &ZCMD
/* ISPF 2.0  Pass TBDISPL Z values to CLIST  */
 &LTDSELS = &ZTDSELS
 &LTDROWS = &ZTDROWS
 &LTDTOP = &ZTDTOP
)END
./ ADD NAME=HMEMFIND
/********************************************************************/
/*                                                                  */
/*    PANEL: HMEMFIND                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Help panel for PMEMFIND                                 */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/23/2021 0.9.10   Larry Belmontes Jr.                          */
/*                     - Update VRM on panel                        */
/*                                                                  */
/* 09/20/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 [ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(TURQ)
 ? TYPE(TEXT)   INTENS(LOW)  HILITE(REVERSE) COLOR(BLUE)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
)BODY EXPAND(//)
%--Tutorial---------?  Find Member Results  %------------------------~v0.9.10 %-
%Command ===>_ZCMD                                                     {Z
%
[Find+MEMBER%obtains a list of PO datasets from DASD VTOCs (catalogued and
%not catalogued) that contain the specified~DSNstrPOS%and~DSNstr%value.
%For each dataset, determine if the requested~MEMBER Name%exists or not.
%
%Results are scrollable and include dataset name, member name, found indicator,
%and volume.
%
%Each dataset listed may be conveniently Browsed or Edited.
%
%
%
%
%
%
)INIT
 .CURSOR = ZCMD
 .ZVARS = (ZPANELID)
)PROC

)END
./ ADD NAME=PSRCH
/********************************************************************/
/*                                                                  */
/*    PANEL: PSRCH                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Search String panel used by CSRCH                       */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR DEFAULT(%@_)
%   TYPE(TEXT)   INTENS(HIGH)
@   TYPE(TEXT)   INTENS(LOW)
_   TYPE(INPUT)  INTENS(HIGH) CAPS(ON)  JUST(LEFT)
!   TYPE(OUTPUT) INTENS(LOW)  CAPS(OFF) JUST(ASIS)
$   TYPE(TEXT)   INTENS(LOW)       HILITE(REVERSE) COLOR(BLUE)
[   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(TURQ)
]   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(YELLOW)
{   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(GREEN)
}   TYPE(TEXT)   INTENS(HIGH)                      COLOR(YELLOW)
?   TYPE(OUTPUT) INTENS(LOW)            JUST(LEFT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%--------------------$  Search String Results  %--------------------------------
%COMMAND ===>_ZCMD                                             %SCROLL ===>_AMT
%
}Group:{Z       }Fast:[Z} ListAll:[Z%                                  ?Z
}DSN &DSNBY  :[Z                                            %          ?Z
}SEARCHstr   :[Z
%
%PF3-End   PF7-Up   PF8-Down
%Dataset can be Browsed or Edited
%
%DSNs            DSNs           DSN w/       Found      Total
%Extracted:!Z   %Searched:!Z   %NoHits:!Z   %Mbrs:!Z   %Hits:!Z   %
%S   Dataset Name----&GRPBY               ------- Member   Found Volume
)MODEL
_Z  [Z                                           [Z       [Z    [Z     %
)INIT
.HELP = HSRCH
.CURSOR = ZCMD
.ZVARS = '(PMEM PQK ALLQDSNS ZUSER PDSN ZPANELID PSRCH
           TOTDSN TSRCH NULC TMBS THIT
           SEL TDSN TMEM TFND TVOL)'
&AMT=CSR
)PROC
/* ISPF 2.0  Pass ZCMD      value  to CLIST  */
 &LCMD = &ZCMD
/* ISPF 2.0  Pass TBDISPL Z values to CLIST  */
 &LTDSELS = &ZTDSELS
 &LTDROWS = &ZTDROWS
 &LTDTOP = &ZTDTOP
)END
./ ADD NAME=HSRCH
/********************************************************************/
/*                                                                  */
/*    PANEL: HSRCH                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Help panel for PSRCH                                    */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/23/2021 0.9.10   Larry Belmontes Jr.                          */
/*                     - Updated VRM on panel                       */
/*                                                                  */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 [ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(TURQ)
 ? TYPE(TEXT)   INTENS(LOW)  HILITE(REVERSE) COLOR(BLUE)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
)BODY EXPAND(//)
%--Tutorial----------?  Search String Results  %---------------------~v0.9.10 %-
%Command ===>_ZCMD                                                     {Z
%
[Find members in+DSL[with+SEARCHstr%obtains a list of datasets from DASD
%VTOCs (catalogued and not catalogued) containing the specified~DSNstrPOS
%and~DSNstr%value.  Each dataset includes dataset name, member name, found
%(see below for designations), found count, and volume.
%
%Results are scrollable and include dataset name, member name (see below),
%found count and volume name.
%
% [>*NONE*<%designates no members met string search for DSN
% [ MBR->> %designates total member count containing search string
% [ HIT->> %designates total hits across all members with search string
% [Member  %designates member name
%
%Summary totals include DSNs Extracted, DSNs Searched, DSN w NoHits,
%Found Mbrs and Total Hits.
%
%Each dataset listed may be conveniently Browsed or Edited.
)INIT
 .CURSOR = ZCMD
 .ZVARS = (ZPANELID)

)PROC

)END
./ ADD NAME=PVOL
/********************************************************************/
/*                                                                  */
/*    PANEL: PVOL                                                   */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Volume DSN Panel used by CVOL                           */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR DEFAULT(%@_)
%   TYPE(TEXT)   INTENS(HIGH)
@   TYPE(TEXT)   INTENS(LOW)
_   TYPE(INPUT)  INTENS(HIGH) CAPS(ON)  JUST(LEFT)
!   TYPE(OUTPUT) INTENS(LOW)  CAPS(OFF) JUST(ASIS)
$   TYPE(TEXT)   INTENS(LOW)       HILITE(REVERSE) COLOR(BLUE)
[   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(TURQ)
]   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(YELLOW)
{   TYPE(OUTPUT) INTENS(HIGH) CAPS(OFF) JUST(ASIS) COLOR(GREEN)
}   TYPE(TEXT)   INTENS(HIGH)                      COLOR(YELLOW)
?   TYPE(OUTPUT) INTENS(LOW)            JUST(LEFT)
)BODY WIDTH(&ZSCREENW) EXPAND(//)
%------------------$   List Datasets Results   %--------------------------------
%COMMAND ===>_ZCMD                                             %SCROLL ===>_AMT
%
}DSN &DSNBY  :[Z                                             %         ?Z
%                                                                      ?Z
%
%PF3-End   PF7-Up   PF8-Down                      C catalogued
%Dataset can be Browsed or Edited                 N not catalogued
%                                                 W catalogued on another vol
%DSNs            DSNs                             E catalog processing error
%Extracted:!Z   %Listed:!Z   %                    ~
%S   Dataset Name-------------------------------- C Volume DSO RFM LRECL BLKSZ
)MODEL
_Z  [Z                                           [Z[Z     [Z  [Z  [Z    [Z    %
)INIT
.HELP = HVOL
.CURSOR = ZCMD
.ZVARS = '(PDSN ZUSER ZPANELID
           TOTDSN TSRCH
           SEL TDSN TCAT TVOL TDSO TRFM TLRECL TBLKSZ)'
&AMT=CSR
)PROC
/* ISPF 2.0  Pass ZCMD      value  to CLIST  */
 &LCMD = &ZCMD
/* ISPF 2.0  Pass TBDISPL Z values to CLIST  */
 &LTDSELS = &ZTDSELS
 &LTDROWS = &ZTDROWS
 &LTDTOP = &ZTDTOP
)END
./ ADD NAME=HVOL
/********************************************************************/
/*                                                                  */
/*    PANEL: HVOL                                                   */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.x Help panel for PVOL                                     */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/23/2020 0.9.10   Larry Belmontes Jr.                          */
/*                     - Updated VRM on panel                       */
/*                                                                  */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
)ATTR
 + TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(GREEN)
 % TYPE(TEXT)   INTENS(HIGH) SKIP(ON)    COLOR(WHITE)
 ~ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(YELLOW)
 [ TYPE(TEXT)   INTENS(LOW)  SKIP(ON)    COLOR(TURQ)
 ? TYPE(TEXT)   INTENS(LOW)  HILITE(REVERSE) COLOR(BLUE)
 _ TYPE(INPUT)  INTENS(LOW)  JUST(LEFT)  COLOR(TURQ)
 # TYPE(INPUT)  INTENS(LOW)  JUST(ASIS)
 ! TYPE(INPUT)  INTENS(HIGH) JUST(RIGHT)
 { TYPE(OUTPUT) INTENS(LOW)  JUST(LEFT)
 } TYPE(OUTPUT) INTENS(LOW)  JUST(RIGHT)
)BODY EXPAND(//)
%--Tutorial--------?   List Datasets Results   %---------------------~v0.9.10 %-
%Command ===>_ZCMD                                                     {Z
%
[List Datasets%obtains a list of datasets from DASD VTOCs (catalogued and
%not catalogued) that contain the specified~DSNstrPOS%and~DSNstr%value.
%
%Results are scrollable and include dataset name, catalog indicator, volume,
%dataset organization, record format, and LRECL.
%
%Each dataset listed may be conveniently Browsed or Edited.
%
%
%
%
%
%
)INIT
 .CURSOR = ZCMD
 .ZVARS = (ZPANELID)

)PROC

)END
@@
//MLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.MLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=FIND00
/********************************************************************/
/*                                                                  */
/* MESSAGES: FIND00                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 FINDSRCH messages                                       */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/23/2021 0.9.10   Larry Belmontes Jr.                          */
/*                     - Add new messages FIND008, FIND009          */
/*                                                                  */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
FIND000  '&ZERRSM                 ' .ALARM=NO
'&ZERRMSG &ZERRLM                                                              '
FIND001  '                        ' .ALARM=NO
'FIND001  Cannot be blank                                                      '
FIND002  '                        ' .ALARM=NO
'FIND002  Must conform to DSN naming standards                                 '
FIND003  '                        ' .ALARM=NO
'FIND003  Invalid value                                                        '
FIND004  '                        ' .ALARM=NO
'FIND004  Exceeded DSN limit.  DSNs Searched reflects applied limit.           '
FIND005  '                        ' .ALARM=NO
'FIND005  No datasets found containing ''&PDSN''                               '
FIND006  '                        ' .ALARM=NO
'FIND006  No Members found for this query.                                     '
FIND007  '                        ' .ALARM=NO
'FIND007  No Search Results found for this query.                              '
FIND008  '                        ' .ALARM=NO
'FIND008  DDName (pos. 1) must contain A-Z, #, @ or $.                         '
FIND009  '                        ' .ALARM=NO
'FIND009  DDName (pos. 2-8) must contain A-Z, 0-9, #, @ or $.                  '
./ ADD NAME=FIND01
/********************************************************************/
/*                                                                  */
/* MESSAGES: FIND01                                                 */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* ISPF 2.0 FINDSRCH messages                                       */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/
FIND010  '                        ' .ALARM=NO
'FIND010  No datasets found starting w ''&PDSN''                               '
FIND011  '                        ' .ALARM=NO
'FIND011  No datasets found ending w ''&PDSN''                                 '
FIND019  '                        ' .ALARM=NO
'FIND019  Invalid selection code - ''&SEL''                                    '
@@
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYSGEN.ISPF.CLIB,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=C$FNDSRH
PROC 0

/********************************************************************/
/*                                                                  */
/*    CLIST: C$FNDSRH                                               */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* IVP CLIST to validate   FINDSRCH                                 */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY VERSION  NAME / DESCRIPTION                           */
/* ---------- -------  -------------------------------------------- */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/


/* ------------------------------------------------------ */
/* Display FINDSRCH Menu                                  */
/* ------------------------------------------------------ */
ISPEXEC SELECT PANEL(PFINDM) NEWAPPL(FIND)


END

./ ADD NAME=CMEMFIND
PROC 0 DEBUG

/********************************************************************/
/*                                                                  */
/* CLIST: CMEMFIND                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* This CLIST uses ISPF services under ISPF product from Wally      */
/* Mclaughlin, version 2.1 or greater.                              */
/*                                                                  */
/* Starting variables are fetched via an ISPF VGET,                 */
/*                                                                  */
/*      VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE          */
/*                                                                  */
/* A list of datasets (PO) is assembled for DSN Containing using    */
/* the VTOC CP.                                                     */
/*                                                                  */
/* For each dataset in the list, determine if member exists or      */
/* does not exist using the CHKDSN CP.  Results are written to an   */
/* ISPF table for display and processing.                           */
/*                                                                  */
/* Panel PMEMFIND is used to display table results.                 */
/*                                                                  */
/* Result dataset can be browsed or edited for convenience.         */
/*                                                                  */
/* Command:                                                         */
/* as issued from menu panel PFINDM -                               */
/*  'CMD(%CMEMFIND) NEWAPPL(FIND)'                                  */
/*                                                                  */
/* Parameters:                                                      */
/* DEBUG    optional, used to display debug information during      */
/*          CLIST execution.  The value for this positional         */
/*          parameter is DEBUG.                                     */
/*                                                                  */
/*          i.e. CMEMFIND DEBUG                                     */
/*                                                                  */
/* PANELS:   PMEMFIND                                               */
/*                                                                  */
/* CPs:      CHKDSN                                                 */
/*           CUTIL00                                                */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/

/*********************************************************************/
/* my Initialization                                                 */
/*********************************************************************/
SET MYNAME = &STR(CMEMFIND)          /* my Proc Name                 */
SET VTOCDSNLIMIT = 200               /* EXCEED DSN LIMIT for CMEMFIND*/
SET PDSONLY =                        /* VTOC LIMIT Parameter         */
SET PDSONLY = &STR(LIMIT (DSO EQ PO))
SET NULC = 0                         /* Members NOT Found counter    */
SET THIT = 0                         /* Members Other Found counter  */
SET TMBS = 0                         /* Members Found counter        */
SET BYPRC=                           /* Bypass RC ERROR routine      */

/*---BEGIN------------------- $$start_up ----------------------------*/
IF &DEBUG = DEBUG THEN -
  CONTROL MSG NOSYMLIST NOCONLIST NOLIST NOFLUSH
ELSE -
  CONTROL NOMSG NOSYMLIST NOCONLIST NOLIST NOFLUSH

/*********************************************************************/
/* ISPF must be available                                            */
/*********************************************************************/
ISPQRY
SET RC = &LASTCC
IF &RC NE 0 THEN -
 DO
   WRITE THIS CLIST REQUIRES ISPF TO BE ACTIVE
   EXIT CODE(12)
 END

/*********************************************************************/
/* ISPF Control Handler   (we handle errors!)                        */
/*********************************************************************/
ISPEXEC CONTROL ERRORS RETURN

/*********************************************************************/
/* Get ISPF Profile Variables                                        */
/*********************************************************************/
ISPEXEC VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE
ISPEXEC VGET (F1LVL F1DDN) PROFILE

CUTIL00 TRIM F1MEMBER PMEM
CUTIL00 TRIM F1DSN PDSN
CUTIL00 TRIM F1SRCH PSRCH
CUTIL00 TRIM F1QK PQK
CUTIL00 TRIM F1ALLDSN ALLQDSNS
CUTIL00 TRIM F1LVL PLVL
CUTIL00 TRIM F1DDN PDDN

SET ROWID = 0                        /* Row sequence number for TBL  */
SET RCDID = 0                        /* OUTFIL record counter        */
SET DONE = NO                        /* EOF Ind for GETFILE ERROR    */
SET OUTID =                          /*    Time Stamp                */
CUTIL00 TDSN OUTID
SET VRCDS = 0                        /* VTOC Records counter         */
SET VTOCDSNEXCEEDED =                /* EXCEED DSN LIMIT Ind         */
SET TSRCH = 0                        /* Total DSN Searched           */

ISPEXEC VGET (LSCREEN) SHARED        /* Screen No from ISPF panel    */
                                     /* VTOC Temp DSN                */
SET VTCTMP = &STR(&SYSUID..&MYNAME..S&LSCREEN..VTOCOUT.&OUTID)
                                     /* Other Temp DSNs should       */
                                     /* follow same naming           */
                                     /* convention                   */
/*---END--------------------- $$start_up ----------------------------*/

/*********************************************************************/
/* VTOC File Allocation and Extract Qualifying DSNs                  */
/*********************************************************************/
/*---START------------------- $$vtoc_2_dsns -------------------------*/
/*********************************************************************/
/* VTOCOUT DD   Used by VTOC                                         */
/*  Free and Allocation                                              */
/*********************************************************************/
FREE ATTR(OUTATTR)
FREE FI(VTOCOUT)
ATTR OUTATTR RECFM(F B) LRECL(80) BLKSIZE(8000) DSORG(PS)
SET RC = &LASTCC
IF &RC > 0 THEN -
  DO
    WRITE ATTR error for OUTATTR, RC=&RC  **PROCESS TERMINATED**
    GOTO DONEHERE
  END
ALLOC F(VTOCOUT) DA('&VTCTMP') NEW CATALOG TR +
  SP(10,5) USING(OUTATTR) UNIT(SYSDA)
SET RC = &LASTCC
IF &RC > 0 THEN -
  DO
    WRITE ALLOC error for VTOCOUT, RC=&RC  **PROCESS TERMINATED**
    GOTO DONEHERE
  END

/*********************************************************************/
/* VTOC Command                                                      */
/*   List of DSNs                                                    */
/*********************************************************************/
SET DSNT = &STR(CONTAINING)          /* CONTAINING, default          */
SET DSNBY = &STR(CONTAINS)
IF &PLVL = S THEN -
  DO                                 /* AT START                     */
    SET DSNT = &STR(LEVEL)
    SET DSNBY = &STR(STARTS W)
  END
IF &PLVL = E THEN -
  DO                                 /* AT END                       */
    SET DSNT = &STR(ENDING)
    SET DSNBY = &STR(ENDS W)
  END
VTOC 'ALL'    +
     &DSNT ('&PDSN') +
     CAT             +
     &PDSONLY        +
     PRINT (NEW (DSO VOLUME DSNAME CAT RFM LRECL BLKSZ)) +
     NOHEADING
SET RC = &LASTCC
IF &RC > 0 THEN -
  DO
    WRITE VTOC processing error, RC=&RC  **PROCESS TERMINATED**
    GOTO DONEHERE
  END

IF &DEBUG = DEBUG THEN -
  DO
    WRITE VTOC process, RC=&RC, starting BROWSE session...
    ISPEXEC BROWSE DATASET('&VTCTMP')
  END
/*---END--------------------- $$vtoc_2_dsns -------------------------*/

/*********************************************************************/
/* TBCREATE MEMFIND0                                                 */
/*  Declare TBL columens and keys                                    */
/*  Create table                                                     */
/*********************************************************************/
SET TDSN = &STR(12345678901234567890123456789012345678901234)
SET TMEM = &STR(12345678)
SET TFND = &STR(1)
SET TVOL = &STR(123456)
SET TDSO = &STR(12)
SET TBKEYS  = &STR(ROWID)
SET TBNAMES = &STR(TDSN TMEM TFND TVOL TDSO)

/*---START------------------- $$tbcre8_00 ---------------------------*/
ISPEXEC TBCREATE MEMFIND0 KEYS(&TBKEYS) +
        NAMES(&TBNAMES)    +
        NOWRITE REPLACE
SET RC = &LASTCC
IF &DEBUG = DEBUG THEN WRITE TBCREATE RC=&RC
IF &RC NE 0 THEN -
  DO
    WRITE TBCREATE error, RC=&RC, **PROCESS TERMINATED**
    GOTO DONEHERE
  END
/*---END--------------------- $$tbcre8_00 ---------------------------*/

/*********************************************************************/
/* Open and prime VTOCOUT                                            */
/*********************************************************************/
/*---START------------------- $$vtoc_opn_n_prm ----------------------*/
/*********************************************************************/
/* Open VTOCOUT                                                      */
/*********************************************************************/
OPENFILE VTOCOUT INPUT
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT DD open error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

/*---START------------------- $$error_eof ---------------------------*/
/*********************************************************************/
/* Error Handler for VTOCOUT                                         */
/*  RC=400 (GETFILE EOF) set DONE to YES and return                  */
/*  Other, write error message and return                            */
/*                                                                   */
/* Note: If any error is raised between this ERROR routine and the   */
/*       resetting ERROR routine, a message will be displayed        */
/*       with a return code, but no statement # or statement is      */
/*       accompanied due to normal CONTROL settings (NOMSG).         */
/*********************************************************************/
ERROR +
  DO
    SET ERRXRC = &LASTCC             /* Save return code...          */
    IF &BYPRC > &STR() THEN -
      DO                             /* Bypass RC Error Routine      */
        SET BYPRC=                   /* Reset BYPRC to nulls         */
        RETURN                       /* Return                       */
      END
    ELSE -
      DO
        IF &ERRXRC = 400 THEN -
          DO                         /* GETFILE EOF                  */
            IF &DEBUG = DEBUG THEN WRITE EOF detected on GETFILE
            SET DONE=YES             /* Set FLAG and                 */
            RETURN                   /* ...Return                    */
          END
        ELSE -
          DO                         /* NON-EOF Error                */
            WRITE ** NON-EOF ERROR in &MYNAME, RC=&ERRXRC
            RETURN
          END
      END
  END
/*---END--------------------- $$error_eof ---------------------------*/

/*********************************************************************/
/* Prime VTOCOUT                                                     */
/*********************************************************************/
GETFILE VTOCOUT
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT DD getfile-PR error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

SET TOTDSN = 0
/*---END--------------------- $$vtoc_opn_n_prm ----------------------*/

/*********************************************************************/
/* Add table entries for each DSN                                    */
/*********************************************************************/
DO WHILE &DONE = NO
  IF &SUBSTR(3:10,&VTOCOUT) EQ &STR(TOTALS -) THEN -
    DO
      SET TOTDSN = &SUBSTR(11:16,&VTOCOUT)
      SET TOTDSN = &TOTDSN
      GOTO GETER
    END
  SET VRCDS = &VRCDS + 1
  IF &VRCDS > &VTOCDSNLIMIT THEN -
    DO
      IF &VTOCDSNEXCEEDED > &STR() THEN
      ELSE -
        DO
          SET TSRCH = &VTOCDSNLIMIT
          SET VTOCDSNEXCEEDED = X
          ISPEXEC SETMSG MSG(FIND004 ) /* Exceeded DSN Limit &TSRCH  */
        END
      GOTO GETER
    END
  /***************************/
  /* Process only PDS files  */
  /***************************/
  SET TDSO = &SUBSTR(2:3,&VTOCOUT)
  CUTIL00 TRIM TDSO
  IF &TDSO NE PO THEN GOTO GETER
  SET TVOL = &SUBSTR(6:11,&VTOCOUT)
  SET TDSN = &SUBSTR(13:57,&VTOCOUT)
  CUTIL00 TRIM TVOL
  CUTIL00 TRIM TDSN
  CUTIL00 TRIM PMEM TMEM
  /***************************/
  /* Checck for member in PDS*/
  /***************************/
  SET BYPRC=CHKDSN                   /* Bypass RC ERROR routine      */
  CHKDSN '&TDSN(&PMEM)' VOL(&TVOL) QUIET
  /*CHKDSN DSN VOL(volser) MBR(member) QUIET CATLG
  SET RC = &LASTCC
  IF &BYPRC =  THEN -
    SET RC = &ERRXRC                 /* Use RC captured in ERROR rtn */
  SET BYPRC =                        /* Reset for RC=0 condition     */
  /* Set FOUND indicator     */
  /* Y-Found, N-Not Found    */
  /* ?-Undefined status      */
  SET TFND = ?
  IF &RC = 0 THEN -
    SET TFND = Y
  IF &RC = 16 THEN -
    DO
      SET TFND = N
      SET TMEM = &STR( )
    END
  /***************************/
  /* Accum    FOUND items    */
  /***************************/
  IF &TFND = Y THEN -
    SET TMBS = &TMBS + 1
  ELSE -
  IF &TFND = N THEN -
    SET NULC = &NULC + 1
  ELSE -
    SET THIT = &THIS + 1
  /***************************/
  /* Show ALL or FOUND rcds  */
  /***************************/
  IF &ALLQDSNS = N THEN -
    IF &TFND = Y THEN
    ELSE -
      GOTO GETER
  SET ROWID = &ROWID + 1
  ISPEXEC TBADD MEMFIND0
  SET RC = &LASTCC
  IF &DEBUG = DEBUG THEN WRITE TBADD RC=&RC
  GETER: GETFILE VTOCOUT
  SET RC = &LASTCC
END

/*********************************************************************/
/* Close VTOCOUT and check for no DSNs found                         */
/*********************************************************************/
/*---START------------------- $$vtoc_clos_n_chk4no ------------------*/
/*---START------------------- $$error_off ---------------------------*/
/*********************************************************************/
/* Reset Error Handler to                                            */
/*  Display statement that causes error w/ error message(s)          */
/*********************************************************************/
ERROR OFF
/*---END--------------------- $$error_off ---------------------------*/

/*********************************************************************/
/* Close VTOCOUT                                                     */
/*********************************************************************/
CLOSFILE VTOCOUT
SET RC = &LASTCC
IF &TSRCH = 0 THEN -
  SET TSRCH = &TOTDSN

IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT close error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

IF &DEBUG = DEBUG THEN -
  WRITE ROWID=&ROWID RCDID=&RCDID TOTDSN=&TOTDSN

/*********************************************************************/
/* Check for NO DSNs FOUND                                           */
/*********************************************************************/
IF &TOTDSN = 0 THEN -
  DO
    SET MSGGG = &STR(FIND005 )    /* No datasets found containing    */
    IF &PLVL = L THEN -
      SET MSGGG = &STR(FIND010 )  /* No datasets found starting w/   */
    IF &PLVL = E THEN -
      SET MSGGG = &STR(FIND011 )  /* No datasets found ending w/     */
    ISPEXEC SETMSG MSG(&MSGGG)    /* No dataset found ...            */
    GOTO CLOSNGO
  END
/*---END--------------------- $$vtoc_clos_n_chk4no ------------------*/

/*********************************************************************/
/* Check for NO MEMBERS FOUND                                        */
/*********************************************************************/
IF &ROWID = 0 THEN -
  DO
    SET TDSN = &STR(*** NO MEMBER FOUND IN QUERY ***)
    SET TMEM = &STR( )
    SET TFND = &STR( )
    SET TVOL = &STR( )
    SET TDSO = &STR( )
    SET ROWID = &ROWID + 1
    ISPEXEC TBADD MEMFIND0
    SET RC = &LASTCC
    IF &DEBUG = DEBUG THEN WRITE TBADD RC=&RC
  END

IF &TMBS = 0 THEN -
    ISPEXEC SETMSG MSG(FIND006 )     /* No Search Results found      */

/*********************************************************************/
/* SORT table by FOUND DSC, DSN ASC                                  */
/*********************************************************************/
/*      NAMES(TDSN TFND TVOL TDSO)
IF &ROWID > 1 THEN -
  DO
    ISPEXEC TBSORT MEMFIND0 FIELDS(TFND,C,D,TDSN,C,A)
    SET RC = &LASTCC
    IF &DEBUG = DEBUG THEN WRITE TBSORT RC=&RC
  END

/*********************************************************************/
/* Position to TOP of table                                          */
/*********************************************************************/
ISPEXEC TBTOP MEMFIND0
SET RC = &LASTCC
IF &DEBUG = DEBUG THEN WRITE TBTOP RC=&RC

SET CSSR = 1
SET RC = 0

DO WHILE &RC < 8
  ISPEXEC TBDISPL MEMFIND0  PANEL(PMEMFIND)  +
          CURSOR(ZCMD) CSRROW(&CSSR)
  SET RC=&LASTCC
  IF &DEBUG = DEBUG THEN WRITE TBDISPL RC=&RC
  /***************************/
  /* Close up shop!! PF3/PF4 */
  /***************************/
  IF &RC = 8 THEN GOTO CLOSNGO
  SET ZCMD   =&LCMD
  SET ZTDROWS=&LTDROWS
  SET ZTDTOP =&LTDTOP
  SET ZTDSELS=&LTDSELS
  /***************************/
  /* Check for selections    */
  /***************************/
  IF &ZTDSELS > 0 THEN -
    DO
      CUTIL00 TRIM TDSN
      CUTIL00 TRIM TVOL
      CUTIL00 TRIM TMEM
      IF &DEBUG = DEBUG THEN WRITE SEL=&SEL TDSN='&TDSN'
      IF &SUBSTR(1:1,&STR(&TDSN)) = &STR(*) THEN
      ELSE
        DO
          IF &TFND = Y THEN -
            SET BDSN = &STR(&TDSN)(&TMEM)
          ELSE -
            SET BDSN = &STR(&TDSN)
/*---BEGIN------------------- $$sel_proc ----------------------------*/
          /***************************/
          /* Save Display            */
          /***************************/
          ISPEXEC CONTROL DISPLAY SAVE
          /***************************/
          /* Process SELECTION       */
          /***************************/
          IF &SEL = B THEN -
            ISPEXEC BROWSE DATASET('&BDSN') VOLUME(&TVOL)
          ELSE -
            IF &SEL = E THEN -
              ISPEXEC EDIT   DATASET('&BDSN') VOLUME(&TVOL)
            ELSE -
              ISPEXEC SETMSG MSG(FIND019 )    /* Invalid selection   */
          SET &SEL = &STR( )
          /***************************/
          /* Restore Display         */
          /***************************/
          ISPEXEC CONTROL DISPLAY RESTORE
/*---END--------------------- $$sel_proc ----------------------------*/
        END
    END
END

CLOSNGO: +
ISPEXEC TBEND   MEMFIND0
SET RC=&LASTCC
IF &DEBUG = DEBUG THEN WRITE TBEND RC=&RC

DONEHERE: +
IF &DEBUG = DEBUG THEN WRITE ENDING... DONEHERE!
FREE ATTR(OUTATTR)
FREE FI(VTOCOUT) DELETE


END
./ ADD NAME=CSRCH
PROC 0 DEBUG

/********************************************************************/
/*                                                                  */
/* CLIST: CSRCH                                                     */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* This CLIST uses ISPF services under ISPF product from Wally      */
/* Mclaughlin, version 2.1 or greater.                              */
/*                                                                  */
/* Starting variables are fetched via an ISPF VGET,                 */
/*                                                                  */
/*      VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE          */
/*                                                                  */
/* A list of datasets (PO) is assembled for DSN Containing using    */
/* the VTOC CP.                                                     */
/*                                                                  */
/* For each dataset in the list, determine if search string exists  */
/* in a member using the FINDSRCH CP.  Results are written to an    */
/* ISPF table for display and processing.                           */
/*                                                                  */
/* Panel PSRCH    is used to display table results.                 */
/*                                                                  */
/* Result dataset can be browsed or edited for convenience.         */
/*                                                                  */
/* Command:                                                         */
/* as issued from menu panel PFINDM -                               */
/*  'CMD(%CSRCH) NEWAPPL(FIND)'                                     */
/*                                                                  */
/* Parameters:                                                      */
/* DEBUG    optional, used to display debug information during      */
/*          CLIST execution.  The value for this positional         */
/*          parameter is DEBUG.                                     */
/*                                                                  */
/*          i.e. CSRCH DEBUG                                        */
/*                                                                  */
/* PANELS:   PSRCH                                                  */
/*                                                                  */
/* CPs:      CUTIL00                                                */
/*           FINDSRCH                                               */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/

/*********************************************************************/
/* my Initialization                                                 */
/*********************************************************************/
SET MYNAME = &STR(CSRCH)             /* my Proc Name                 */
SET VTOCDSNLIMIT = 20                /* EXCEED DSN LIMIT for CSRCH   */
SET PDSONLY =                        /* VTOC LIMIT Parameter         */
SET PDSONLY = &STR(LIMIT (DSO EQ PO))
SET NULC = 0                         /* DSNs NoHit counter           */
SET THIT = 0                         /* Total Hits counter           */
SET TMBS = 0                         /* Members Found counter        */
SET BYPRC=                           /* Bypass RC ERROR routine      */

/*---BEGIN------------------- $$start_up ----------------------------*/
IF &DEBUG = DEBUG THEN -
  CONTROL MSG NOSYMLIST NOCONLIST NOLIST NOFLUSH
ELSE -
  CONTROL NOMSG NOSYMLIST NOCONLIST NOLIST NOFLUSH

/*********************************************************************/
/* ISPF must be available                                            */
/*********************************************************************/
ISPQRY
SET RC = &LASTCC
IF &RC NE 0 THEN -
 DO
   WRITE THIS CLIST REQUIRES ISPF TO BE ACTIVE
   EXIT CODE(12)
 END

/*********************************************************************/
/* ISPF Control Handler   (we handle errors!)                        */
/*********************************************************************/
ISPEXEC CONTROL ERRORS RETURN

/*********************************************************************/
/* Get ISPF Profile Variables                                        */
/*********************************************************************/
ISPEXEC VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE
ISPEXEC VGET (F1LVL F1DDN) PROFILE

CUTIL00 TRIM F1MEMBER PMEM
CUTIL00 TRIM F1DSN PDSN
CUTIL00 TRIM F1SRCH PSRCH
CUTIL00 TRIM F1QK PQK
CUTIL00 TRIM F1ALLDSN ALLQDSNS
CUTIL00 TRIM F1LVL PLVL
CUTIL00 TRIM F1DDN PDDN

SET ROWID = 0                        /* Row sequence number for TBL  */
SET RCDID = 0                        /* OUTFIL record counter        */
SET DONE = NO                        /* EOF Ind for GETFILE ERROR    */
SET OUTID =                          /*    Time Stamp                */
CUTIL00 TDSN OUTID
SET VRCDS = 0                        /* VTOC Records counter         */
SET VTOCDSNEXCEEDED =                /* EXCEED DSN LIMIT Ind         */
SET TSRCH = 0                        /* Total DSN Searched           */

ISPEXEC VGET (LSCREEN) SHARED        /* Screen No from ISPF panel    */
                                     /* VTOC Temp DSN                */
SET VTCTMP = &STR(&SYSUID..&MYNAME..S&LSCREEN..VTOCOUT.&OUTID)
                                     /* Other Temp DSNs should       */
                                     /* follow same naming           */
                                     /* convention                   */
/*---END--------------------- $$start_up ----------------------------*/

/*********************************************************************/
/* OUTFIL DD    Used by FIND                                         */
/*  Free and Allocation                                              */
/*********************************************************************/
SET FINDTMP = &STR(&SYSUID..&MYNAME..S&LSCREEN..OUTFIL.&OUTID)
FREE FI(OUTFIL)
FREE ATTR(OUTFATTR)
ATTR OUTFATTR RECFM(F B) LRECL(80) BLKSIZE(3120) DSORG(PS)
ALLOC F(OUTFIL) DA('&FINDTMP') MOD +
   USING(OUTFATTR)         SP(3 1) CYL

/*********************************************************************/
/* VTOC File Allocation and Extract Qualifying DSNs                  */
/*********************************************************************/
/*---START------------------- $$vtoc_2_dsns -------------------------*/
/*********************************************************************/
/* VTOCOUT DD   Used by VTOC                                         */
/*  Free and Allocation                                              */
/*********************************************************************/
FREE ATTR(OUTATTR)
FREE FI(VTOCOUT)
ATTR OUTATTR RECFM(F B) LRECL(80) BLKSIZE(8000) DSORG(PS)
SET RC = &LASTCC
IF &RC > 0 THEN -
  DO
    WRITE ATTR error for OUTATTR, RC=&RC  **PROCESS TERMINATED**
    GOTO DONEHERE
  END
ALLOC F(VTOCOUT) DA('&VTCTMP') NEW CATALOG TR +
  SP(10,5) USING(OUTATTR) UNIT(SYSDA)
SET RC = &LASTCC
IF &RC > 0 THEN -
  DO
    WRITE ALLOC error for VTOCOUT, RC=&RC  **PROCESS TERMINATED**
    GOTO DONEHERE
  END

/*********************************************************************/
/* VTOC Command                                                      */
/*   List of DSNs                                                    */
/*********************************************************************/
SET DSNT = &STR(CONTAINING)          /* CONTAINING, default          */
SET DSNBY = &STR(CONTAINS)
IF &PLVL = S THEN -
  DO                                 /* AT START                     */
    SET DSNT = &STR(LEVEL)
    SET DSNBY = &STR(STARTS W)
  END
IF &PLVL = E THEN -
  DO                                 /* AT END                       */
    SET DSNT = &STR(ENDING)
    SET DSNBY = &STR(ENDS W)
  END
VTOC 'ALL'    +
     &DSNT ('&PDSN') +
     CAT             +
     &PDSONLY        +
     PRINT (NEW (DSO VOLUME DSNAME CAT RFM LRECL BLKSZ)) +
     NOHEADING
SET RC = &LASTCC
IF &RC > 0 THEN -
  DO
    WRITE VTOC processing error, RC=&RC  **PROCESS TERMINATED**
    GOTO DONEHERE
  END

IF &DEBUG = DEBUG THEN -
  DO
    WRITE VTOC process, RC=&RC, starting BROWSE session...
    ISPEXEC BROWSE DATASET('&VTCTMP')
  END
/*---END--------------------- $$vtoc_2_dsns -------------------------*/

/*********************************************************************/
/* TBCREATE MEMFIND0                                                 */
/*  Declare TBL columens and keys                                    */
/*  Create table                                                     */
/*********************************************************************/
SET TDSN = &STR(12345678901234567890123456789012345678901234)
SET TMEM = &STR(12345678)
SET TFND = &STR(12345)
SET TVOL = &STR(123456)
SET TDSO = &STR(12)
SET TBKEYS  = &STR(ROWID)
SET TBNAMES = &STR(TDSN TMEM TFND TVOL TDSO)

/*---START------------------- $$tbcre8_00 ---------------------------*/
ISPEXEC TBCREATE MEMFIND0 KEYS(&TBKEYS) +
        NAMES(&TBNAMES)    +
        NOWRITE REPLACE
SET RC = &LASTCC
IF &DEBUG = DEBUG THEN WRITE TBCREATE RC=&RC
IF &RC NE 0 THEN -
  DO
    WRITE TBCREATE error, RC=&RC, **PROCESS TERMINATED**
    GOTO DONEHERE
  END
/*---END--------------------- $$tbcre8_00 ---------------------------*/

/*********************************************************************/
/* Open and prime VTOCOUT                                            */
/*********************************************************************/
/*---START------------------- $$vtoc_opn_n_prm ----------------------*/
/*********************************************************************/
/* Open VTOCOUT                                                      */
/*********************************************************************/
OPENFILE VTOCOUT INPUT
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT DD open error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

/*---START------------------- $$error_eof ---------------------------*/
/*********************************************************************/
/* Error Handler for VTOCOUT                                         */
/*  RC=400 (GETFILE EOF) set DONE to YES and return                  */
/*  Other, write error message and return                            */
/*                                                                   */
/* Note: If any error is raised between this ERROR routine and the   */
/*       resetting ERROR routine, a message will be displayed        */
/*       with a return code, but no statement # or statement is      */
/*       accompanied due to normal CONTROL settings (NOMSG).         */
/*********************************************************************/
ERROR +
  DO
    SET ERRXRC = &LASTCC             /* Save return code...          */
    IF &BYPRC > &STR() THEN -
      DO                             /* Bypass RC Error Routine      */
        SET BYPRC=                   /* Reset BYPRC to nulls         */
        RETURN                       /* Return                       */
      END
    ELSE -
      DO
        IF &ERRXRC = 400 THEN -
          DO                         /* GETFILE EOF                  */
            IF &DEBUG = DEBUG THEN WRITE EOF detected on GETFILE
            SET DONE=YES             /* Set FLAG and                 */
            RETURN                   /* ...Return                    */
          END
        ELSE -
          DO                         /* NON-EOF Error                */
            WRITE ** NON-EOF ERROR in &MYNAME, RC=&ERRXRC
            RETURN
          END
      END
  END
/*---END--------------------- $$error_eof ---------------------------*/

/*********************************************************************/
/* Prime VTOCOUT                                                     */
/*********************************************************************/
GETFILE VTOCOUT
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT DD getfile-PR error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

SET TOTDSN = 0
/*---END--------------------- $$vtoc_opn_n_prm ----------------------*/

/*********************************************************************/
/* Process each DSN                                                  */
/*********************************************************************/
DO WHILE &DONE = NO
  IF &SUBSTR(3:10,&VTOCOUT) EQ &STR(TOTALS -) THEN -
    DO
      SET TOTDSN = &SUBSTR(11:16,&VTOCOUT)
      SET TOTDSN = &TOTDSN
      GOTO GETER
    END
  SET VRCDS = &VRCDS + 1
  IF &VRCDS > &VTOCDSNLIMIT THEN -
    DO
      IF &VTOCDSNEXCEEDED > &STR() THEN
      ELSE -
        DO
          SET TSRCH = &VTOCDSNLIMIT
          SET VTOCDSNEXCEEDED = X
          ISPEXEC SETMSG MSG(FIND004 ) /* Exceeded DSN Limit &TSRCH */
        END
      GOTO GETER
    END
  /***************************/
  /* Process only PDS files  */
  /***************************/
  SET TDSO = &SUBSTR(2:3,&VTOCOUT)
  CUTIL00 TRIM TDSO
  IF &TDSO NE PO THEN GOTO GETER
  SET TVOL = &SUBSTR(6:11,&VTOCOUT)
  SET TDSN = &SUBSTR(13:57,&VTOCOUT)
  CUTIL00 TRIM TVOL
  CUTIL00 TRIM TDSN
  SET GRP  = &STR()
  SET QCK  = &STR()
  /***************************/
  /* Ensure GROUP is max len */
  /* of 7 bytes              */
  /***************************/
  IF &PMEM > &STR( ) THEN -
    DO
      SET BYPRC=PMEM$CUTIL00         /* Bypass RC ERROR routine      */
      CUTIL00 LEN PMEM
      SET LRC = &ERRXRC              /* Use RC captured in ERROR rtn */
      IF &LRC > 7 THEN -
        DO
          SET MYPOKE = &STR( 8)      /* Make pos 8 BLANK and TRIM    */
          CUTIL00 PUT1V PMEM MYPOKE
          CUTIL00 TRIM  PMEM
        END
      SET GRP = &STR(GROUP)(&PMEM)
    END
  IF &PQK = Y THEN -
    SET QCK = &STR(QUICK)
  IF &DEBUG = DEBUG THEN -
    WRITE FIND '&TDSN' S('&PSRCH') FILE QUIET &GRP &QCK
  SET BYPRC=FINDSRCH                 /* Bypass RC ERROR routine      */
  FINDSRCH '&TDSN' S(&PSRCH) FILE QUIET VOL(&TVOL) &GRP &QCK
  /***************************/
  /* FIND string in PDS      */
  /***************************/
  SET RC = &LASTCC
  IF &BYPRC =  THEN -
    SET RC = &ERRXRC                 /* Use RC captured in ERROR rtn */
  SET BYPRC =                        /* Reset for RC=0 condition     */
  IF &RC NE 0 THEN DISPLAY FIND RC=&RC  VOL='&TVOL'
  GETER: GETFILE VTOCOUT
  SET RC = &LASTCC
END

/*********************************************************************/
/* Close VTOCOUT and check for no DSNs found                         */
/*********************************************************************/
/*---START------------------- $$vtoc_clos_n_chk4no ------------------*/
/*---START------------------- $$error_off ---------------------------*/
/*********************************************************************/
/* Reset Error Handler to                                            */
/*  Display statement that causes error w/ error message(s)          */
/*********************************************************************/
ERROR OFF
/*---END--------------------- $$error_off ---------------------------*/

/*********************************************************************/
/* Close VTOCOUT                                                     */
/*********************************************************************/
CLOSFILE VTOCOUT
SET RC = &LASTCC
IF &TSRCH = 0 THEN -
  SET TSRCH = &TOTDSN

IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT close error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

IF &DEBUG = DEBUG THEN -
  WRITE ROWID=&ROWID RCDID=&RCDID TOTDSN=&TOTDSN

/*********************************************************************/
/* Check for NO DSNs FOUND                                           */
/*********************************************************************/
IF &TOTDSN = 0 THEN -
  DO
    SET MSGGG = &STR(FIND005 )    /* No datasets found containing    */
    IF &PLVL = L THEN -
      SET MSGGG = &STR(FIND010 )  /* No datasets found starting w/   */
    IF &PLVL = E THEN -
      SET MSGGG = &STR(FIND011 )  /* No datasets found ending w/     */
    ISPEXEC SETMSG MSG(&MSGGG)    /* No dataset found ...            */
    GOTO CLOSNGO
  END
/*---END--------------------- $$vtoc_clos_n_chk4no ------------------*/

/*********************************************************************/
/* Open OUTFIL                                                       */
/*********************************************************************/
OPENFILE OUTFIL INPUT
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE OUTFIL DD open error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

/*---START------------------- $$error_eof ---------------------------*/
/*********************************************************************/
/* Error Handler for VTOCOUT                                         */
/*  RC=400 (GETFILE EOF) set DONE to YES and return                  */
/*  Other, write error message and return                            */
/*                                                                   */
/* Note: If any error is raised between this ERROR routine and the   */
/*       resetting ERROR routine, a message will be displayed        */
/*       with a return code, but no statement # or statement is      */
/*       accompanied due to normal CONTROL settings (NOMSG).         */
/*********************************************************************/
ERROR +
  DO
    SET ERRXRC = &LASTCC             /* Save return code...          */
    IF &BYPRC > &STR() THEN -
      DO                             /* Bypass RC Error Routine      */
        SET BYPRC=                   /* Reset BYPRC to nulls         */
        RETURN                       /* Return                       */
      END
    ELSE -
      DO
        IF &ERRXRC = 400 THEN -
          DO                         /* GETFILE EOF                  */
            IF &DEBUG = DEBUG THEN WRITE EOF detected on GETFILE
            SET DONE=YES             /* Set FLAG and                 */
            RETURN                   /* ...Return                    */
          END
        ELSE -
          DO                         /* NON-EOF Error                */
            WRITE ** NON-EOF ERROR in &MYNAME, RC=&ERRXRC
            RETURN
          END
      END
  END
/*---END--------------------- $$error_eof ---------------------------*/

/*********************************************************************/
/* Prime OUTFIL                                                      */
/*********************************************************************/
GETFILE OUTFIL
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE OUTFIL DD getfile-PR error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

SET DONE = NO
SET TDSO = &STR( )
SET TFND = &STR( )

/*********************************************************************/
/* Add table entries for results from FIND                           */
/*********************************************************************/
DO WHILE &DONE = NO
  SET RCDID = &RCDID + 1
  SET TDSN = &SUBSTR(02:45,&OUTFIL)
  SET TVOL = &SUBSTR(46:51,&OUTFIL)
  SET TDSO = &SUBSTR(52:54,&OUTFIL)
  SET TMEM = &SUBSTR(55:62,&OUTFIL)
  IF &STR(&TMEM) = &STR(>*NONE*<) THEN -
    DO
      SET NULC = &NULC + 1
    END
  IF &SUBSTR(01:01,&OUTFIL) EQ &STR(2) THEN -
    DO
      SET TFND = &SUBSTR(65:69,&OUTFIL)
    END
  ELSE -
  IF &SUBSTR(01:01,&OUTFIL) EQ &STR(3) THEN -
    DO
      SET HIT  = &SUBSTR(29:36,&OUTFIL)
      SET HIT  = &HIT
      SET THIT = &THIT + &HIT
      SET MBS  = &SUBSTR(02:07,&OUTFIL)
      SET MBS  = &MBS
      SET TMBS = &TMBS + &MBS
      GOTO XETER
    END
  ELSE -
    GOTO XETER
  /***************************/
  /* Show ALL or FOUND rcds  */
  /***************************/
  IF &ALLQDSNS = N THEN -
    IF &STR(&TMEM) = &STR(>*NONE*<) THEN -
       GOTO XETER
  SET ROWID = &ROWID + 1
  ISPEXEC TBADD MEMFIND0
  SET RC = &LASTCC
  IF &DEBUG = DEBUG THEN WRITE TBADD RC=&RC
  XETER: GETFILE OUTFIL
  SET RC = &LASTCC
END

/*---START------------------- $$error_off ---------------------------*/
/*********************************************************************/
/* Reset Error Handler to                                            */
/*  Display statement that causes error w/ error message(s)          */
/*********************************************************************/
ERROR OFF
/*---END--------------------- $$error_off ---------------------------*/

/*********************************************************************/
/* Close OUTFIL                                                      */
/*********************************************************************/
CLOSFILE OUTFIL
SET RC = &LASTCC
IF &DEBUG = DEBUG THEN WRITE ROWID=&ROWID RCDID=&RCDID

IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT close error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

/*********************************************************************/
/* Check for NO RESULTS FOUND, write TBL entry                       */
/*********************************************************************/
IF &ROWID = 0 THEN -
  DO
    SET TDSN = &STR(*** NO SEARCH RESULTS FOUND IN QUERY ***)
    SET TMEM = &STR( )
    SET TFND = &STR( )
    SET TVOL = &STR( )
    SET TDSO = &STR( )
    SET ROWID = &ROWID + 1
    ISPEXEC TBADD MEMFIND0
    SET RC = &LASTCC
    IF &DEBUG = DEBUG THEN WRITE TBADD RC=&RC
  END

IF &TMBS = 0 THEN -
    ISPEXEC SETMSG MSG(FIND007 )  /* No Search Results found         */

/*********************************************************************/
/* Set GRPBY message                                                 */
/*********************************************************************/
IF &PMEM > &STR() THEN -
  DO
    SET &GRPBY = &STR(GROUPED BY )&PMEM
    SET &GRPBY = &GRPBY&STR(---------)
  END
ELSE -
  DO
    SET &GRPBY = &STR(---------------------)
  END

/*********************************************************************/
/* Position to TOP of table                                          */
/*********************************************************************/
ISPEXEC TBTOP MEMFIND0
SET RC = &LASTCC
IF &DEBUG = DEBUG THEN WRITE TBTOP RC=&RC

SET CSSR = 1
SET RC = 0

DO WHILE &RC < 8
  ISPEXEC TBDISPL MEMFIND0  PANEL(PSRCH) +
          CURSOR(ZCMD) CSRROW(&CSSR)
  SET RC=&LASTCC
  IF &DEBUG = DEBUG THEN WRITE TBDISPL RC=&RC
  /***************************/
  /* Close up shop!! PF3/PF4 */
  /***************************/
  IF &RC = 8 THEN GOTO CLOSNGO
  SET ZCMD   =&LCMD
  SET ZTDROWS=&LTDROWS
  SET ZTDTOP =&LTDTOP
  SET ZTDSELS=&LTDSELS
  /***************************/
  /* Check for selections    */
  /***************************/
  IF &ZTDSELS > 0 THEN -
    DO
      CUTIL00 TRIM TDSN
      CUTIL00 TRIM TVOL
      CUTIL00 TRIM TMEM
      IF &DEBUG = DEBUG THEN WRITE SEL=&SEL TDSN='&TDSN'
      IF &SUBSTR(1:1,&TDSN) > &STR( ) THEN -
        DO
          IF (&STR(&TMEM) = &STR(>*NONE*<)) OR -
             (&SUBSTR(1:1,&STR(&TMEM)) = &STR( )) THEN -
            DO
              SET BDSN = &STR(&TDSN)
            END
          ELSE -
            DO
              CUTIL00 TRIM TMEM
              SET BDSN = &STR(&TDSN(&TMEM))
            END
/*---BEGIN------------------- $$sel_proc ----------------------------*/
          /***************************/
          /* Save Display            */
          /***************************/
          ISPEXEC CONTROL DISPLAY SAVE
          /***************************/
          /* Process SELECTION       */
          /***************************/
          IF &SEL = B THEN -
            ISPEXEC BROWSE DATASET('&BDSN') VOLUME(&TVOL)
          ELSE -
            IF &SEL = E THEN -
              ISPEXEC EDIT   DATASET('&BDSN') VOLUME(&TVOL)
            ELSE -
              ISPEXEC SETMSG MSG(FIND019 )    /* Invalid selection   */
          SET &SEL = &STR( )
          /***************************/
          /* Restore Display         */
          /***************************/
          ISPEXEC CONTROL DISPLAY RESTORE
/*---END--------------------- $$sel_proc ----------------------------*/
        END
    END
END

CLOSNGO: +
ISPEXEC TBEND   MEMFIND0
SET RC=&LASTCC
IF &DEBUG = DEBUG THEN WRITE TBEND RC=&RC

DONEHERE: +
IF &DEBUG = DEBUG THEN WRITE ENDING... DONEHERE!
FREE ATTR(OUTATTR)
FREE FI(VTOCOUT) DELETE

FREE ATTR(OUTFATTR)
FREE FI(OUTFIL) DELETE


END
./ ADD NAME=CVOL
PROC 0 DEBUG

/********************************************************************/
/*                                                                  */
/* CLIST: CVOL                                                      */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* This CLIST uses ISPF services under ISPF product from Wally      */
/* Mclaughlin, version 2.1 or greater.                              */
/*                                                                  */
/* Starting variables are fetched via an ISPF VGET,                 */
/*                                                                  */
/*      VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE          */
/*                                                                  */
/* A list of datasets is assembled for DSN Containing using         */
/* the VTOC CP.                                                     */
/*                                                                  */
/* For each dataset in the list, results are written to an          */
/* ISPF table for display and processing.                           */
/*                                                                  */
/* Panel PVOL     is used to display table results.                 */
/*                                                                  */
/* Result dataset can be browsed or edited for convenience.         */
/*                                                                  */
/* Command:                                                         */
/* as issued from menu panel PFINDM -                               */
/*  'CMD(%CVOL NEWAPPL(FIND)'                                       */
/*                                                                  */
/* Parameters:                                                      */
/* DEBUG    optional, used to display debug information during      */
/*          CLIST execution.  The value for this positional         */
/*          parameter is DEBUG.                                     */
/*                                                                  */
/*          i.e. CVOL DEBUG                                         */
/*                                                                  */
/* PANELS:   PVOL                                                   */
/*                                                                  */
/* CPs:      CUTIL00                                                */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 09/26/2020 0.9.00   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/

/*********************************************************************/
/* my Initialization                                                 */
/*********************************************************************/
SET MYNAME = &STR(CVOL)              /* my Proc Name                 */
SET VTOCDSNLIMIT = 250               /* EXCEED DSN LIMIT for CVOL    */
SET PDSONLY =                        /* VTOC LIMIT Parameter         */
SET BYPRC=                           /* Bypass RC ERROR routine      */

/*---BEGIN------------------- $$start_up ----------------------------*/
IF &DEBUG = DEBUG THEN -
  CONTROL MSG NOSYMLIST NOCONLIST NOLIST NOFLUSH
ELSE -
  CONTROL NOMSG NOSYMLIST NOCONLIST NOLIST NOFLUSH

/*********************************************************************/
/* ISPF must be available                                            */
/*********************************************************************/
ISPQRY
SET RC = &LASTCC
IF &RC NE 0 THEN -
 DO
   WRITE THIS CLIST REQUIRES ISPF TO BE ACTIVE
   EXIT CODE(12)
 END

/*********************************************************************/
/* ISPF Control Handler   (we handle errors!)                        */
/*********************************************************************/
ISPEXEC CONTROL ERRORS RETURN

/*********************************************************************/
/* Get ISPF Profile Variables                                        */
/*********************************************************************/
ISPEXEC VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE
ISPEXEC VGET (F1LVL F1DDN) PROFILE

CUTIL00 TRIM F1MEMBER PMEM
CUTIL00 TRIM F1DSN PDSN
CUTIL00 TRIM F1SRCH PSRCH
CUTIL00 TRIM F1QK PQK
CUTIL00 TRIM F1ALLDSN ALLQDSNS
CUTIL00 TRIM F1LVL PLVL
CUTIL00 TRIM F1DDN PDDN

SET ROWID = 0                        /* Row sequence number for TBL  */
SET RCDID = 0                        /* OUTFIL record counter        */
SET DONE = NO                        /* EOF Ind for GETFILE ERROR    */
SET OUTID =                          /*    Time Stamp                */
CUTIL00 TDSN OUTID
SET VRCDS = 0                        /* VTOC Records counter         */
SET VTOCDSNEXCEEDED =                /* EXCEED DSN LIMIT Ind         */
SET TSRCH = 0                        /* Total DSN Searched           */

ISPEXEC VGET (LSCREEN) SHARED        /* Screen No from ISPF panel    */
                                     /* VTOC Temp DSN                */
SET VTCTMP = &STR(&SYSUID..&MYNAME..S&LSCREEN..VTOCOUT.&OUTID)
                                     /* Other Temp DSNs should       */
                                     /* follow same naming           */
                                     /* convention                   */
/*---END--------------------- $$start_up ----------------------------*/

/*********************************************************************/
/* VTOC File Allocation and Extract Qualifying DSNs                  */
/*********************************************************************/
/*---START------------------- $$vtoc_2_dsns -------------------------*/
/*********************************************************************/
/* VTOCOUT DD   Used by VTOC                                         */
/*  Free and Allocation                                              */
/*********************************************************************/
FREE ATTR(OUTATTR)
FREE FI(VTOCOUT)
ATTR OUTATTR RECFM(F B) LRECL(80) BLKSIZE(8000) DSORG(PS)
SET RC = &LASTCC
IF &RC > 0 THEN -
  DO
    WRITE ATTR error for OUTATTR, RC=&RC  **PROCESS TERMINATED**
    GOTO DONEHERE
  END
ALLOC F(VTOCOUT) DA('&VTCTMP') NEW CATALOG TR +
  SP(10,5) USING(OUTATTR) UNIT(SYSDA)
SET RC = &LASTCC
IF &RC > 0 THEN -
  DO
    WRITE ALLOC error for VTOCOUT, RC=&RC  **PROCESS TERMINATED**
    GOTO DONEHERE
  END

/*********************************************************************/
/* VTOC Command                                                      */
/*   List of DSNs                                                    */
/*********************************************************************/
SET DSNT = &STR(CONTAINING)          /* CONTAINING, default          */
SET DSNBY = &STR(CONTAINS)
IF &PLVL = S THEN -
  DO                                 /* AT START                     */
    SET DSNT = &STR(LEVEL)
    SET DSNBY = &STR(STARTS W)
  END
IF &PLVL = E THEN -
  DO                                 /* AT END                       */
    SET DSNT = &STR(ENDING)
    SET DSNBY = &STR(ENDS W)
  END
VTOC 'ALL'    +
     &DSNT ('&PDSN') +
     CAT             +
     &PDSONLY        +
     PRINT (NEW (DSO VOLUME DSNAME CAT RFM LRECL BLKSZ)) +
     NOHEADING
SET RC = &LASTCC
IF &RC > 0 THEN -
  DO
    WRITE VTOC processing error, RC=&RC  **PROCESS TERMINATED**
    GOTO DONEHERE
  END

IF &DEBUG = DEBUG THEN -
  DO
    WRITE VTOC process, RC=&RC, starting BROWSE session...
    ISPEXEC BROWSE DATASET('&VTCTMP')
  END
/*---END--------------------- $$vtoc_2_dsns -------------------------*/

/*********************************************************************/
/* TBCREATE MEMFIND0                                                 */
/*  Declare TBL columns and keys                                     */
/*  Create table                                                     */
/*********************************************************************/
SET TDSN = &STR(12345678901234567890123456789012345678901234)
SET TCAT = &STR(1)
SET TVOL = &STR(123456)
SET TDSO = &STR(123)
SET TRFM = &STR(123)
SET TLRECL = &STR(12345)
SET TBLKSZ = &STR(12345)
SET TBKEYS  = &STR(ROWID)
SET TBNAMES = &STR(TDSN TCAT TVOL TDSO TRFM TLRECL TBLKSZ)

/*---START------------------- $$tbcre8_00 ---------------------------*/
ISPEXEC TBCREATE MEMFIND0 KEYS(&TBKEYS) +
        NAMES(&TBNAMES)    +
        NOWRITE REPLACE
SET RC = &LASTCC
IF &DEBUG = DEBUG THEN WRITE TBCREATE RC=&RC
IF &RC NE 0 THEN -
  DO
    WRITE TBCREATE error, RC=&RC, **PROCESS TERMINATED**
    GOTO DONEHERE
  END
/*---END--------------------- $$tbcre8_00 ---------------------------*/

/*********************************************************************/
/* Open and prime VTOCOUT                                            */
/*********************************************************************/
/*---START------------------- $$vtoc_opn_n_prm ----------------------*/
/*********************************************************************/
/* Open VTOCOUT                                                      */
/*********************************************************************/
OPENFILE VTOCOUT INPUT
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT DD open error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

/*---START------------------- $$error_eof ---------------------------*/
/*********************************************************************/
/* Error Handler for VTOCOUT                                         */
/*  RC=400 (GETFILE EOF) set DONE to YES and return                  */
/*  Other, write error message and return                            */
/*                                                                   */
/* Note: If any error is raised between this ERROR routine and the   */
/*       resetting ERROR routine, a message will be displayed        */
/*       with a return code, but no statement # or statement is      */
/*       accompanied due to normal CONTROL settings (NOMSG).         */
/*********************************************************************/
ERROR +
  DO
    SET ERRXRC = &LASTCC             /* Save return code...          */
    IF &BYPRC > &STR() THEN -
      DO                             /* Bypass RC Error Routine      */
        SET BYPRC=                   /* Reset BYPRC to nulls         */
        RETURN                       /* Return                       */
      END
    ELSE -
      DO
        IF &ERRXRC = 400 THEN -
          DO                         /* GETFILE EOF                  */
            IF &DEBUG = DEBUG THEN WRITE EOF detected on GETFILE
            SET DONE=YES             /* Set FLAG and                 */
            RETURN                   /* ...Return                    */
          END
        ELSE -
          DO                         /* NON-EOF Error                */
            WRITE ** NON-EOF ERROR in &MYNAME, RC=&ERRXRC
            RETURN
          END
      END
  END
/*---END--------------------- $$error_eof ---------------------------*/

/*********************************************************************/
/* Prime VTOCOUT                                                     */
/*********************************************************************/
GETFILE VTOCOUT
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT DD getfile-PR error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

SET TOTDSN = 0
/*---END--------------------- $$vtoc_opn_n_prm ----------------------*/

/*********************************************************************/
/* Process each DSN                                                  */
/*********************************************************************/
DO WHILE &DONE = NO
  IF &SUBSTR(3:10,&VTOCOUT) EQ &STR(TOTALS -) THEN -
    DO
      SET TOTDSN = &SUBSTR(11:16,&VTOCOUT)
      SET TOTDSN = &TOTDSN
      GOTO GETER
    END
  SET VRCDS = &VRCDS + 1
  IF &VRCDS > &VTOCDSNLIMIT THEN -
    DO
      IF &VTOCDSNEXCEEDED > &STR() THEN
      ELSE -
        DO
          SET TSRCH = &VTOCDSNLIMIT
          SET VTOCDSNEXCEEDED = X
          ISPEXEC SETMSG MSG(FIND004 ) /* Exceeded DSN Limit &TSRCH  */
        END
      GOTO GETER
    END
  SET BYPRC =                        /* Reset for RC=0 condition     */
  SET TDSO = &SUBSTR(02:03,&VTOCOUT)
  SET TVOL = &SUBSTR(06:11,&VTOCOUT)
  SET TDSN = &SUBSTR(13:57,&VTOCOUT)
  SET TCAT = &SUBSTR(59:59,&VTOCOUT)
  SET TRFM = &SUBSTR(62:64,&VTOCOUT)
  SET TLRECL = &SUBSTR(66:70,&VTOCOUT)
  SET TBLKSZ = &SUBSTR(72:76,&VTOCOUT)
  SET ROWID = &ROWID + 1
  ISPEXEC TBADD MEMFIND0
  SET RC = &LASTCC
  IF &DEBUG = DEBUG THEN WRITE TBADD RC=&RC
  GETER: GETFILE VTOCOUT
  SET RC = &LASTCC
END

/*********************************************************************/
/* Close VTOCOUT and check for no DSNs found                         */
/*********************************************************************/
/*---START------------------- $$vtoc_clos_n_chk4no ------------------*/
/*---START------------------- $$error_off ---------------------------*/
/*********************************************************************/
/* Reset Error Handler to                                            */
/*  Display statement that causes error w/ error message(s)          */
/*********************************************************************/
ERROR OFF
/*---END--------------------- $$error_off ---------------------------*/

/*********************************************************************/
/* Close VTOCOUT                                                     */
/*********************************************************************/
CLOSFILE VTOCOUT
SET RC = &LASTCC
IF &TSRCH = 0 THEN -
  SET TSRCH = &TOTDSN

IF &RC NE 0 THEN -
  DO
    WRITE VTOCOUT close error, RC=&RC, **PROCESS TERMINATED**
    GOTO CLOSNGO
  END

IF &DEBUG = DEBUG THEN -
  WRITE ROWID=&ROWID RCDID=&RCDID TOTDSN=&TOTDSN

/*********************************************************************/
/* Check for NO DSNs FOUND                                           */
/*********************************************************************/
IF &TOTDSN = 0 THEN -
  DO
    SET MSGGG = &STR(FIND005 )    /* No datasets found containing    */
    IF &PLVL = L THEN -
      SET MSGGG = &STR(FIND010 )  /* No datasets found starting w/   */
    IF &PLVL = E THEN -
      SET MSGGG = &STR(FIND011 )  /* No datasets found ending w/     */
    ISPEXEC SETMSG MSG(&MSGGG)    /* No dataset found ...            */
    GOTO CLOSNGO
  END
/*---END--------------------- $$vtoc_clos_n_chk4no ------------------*/

/*********************************************************************/
/* Position to TOP of table                                          */
/*********************************************************************/
ISPEXEC TBTOP MEMFIND0
SET RC = &LASTCC
IF &DEBUG = DEBUG THEN WRITE TBTOP RC=&RC

SET CSSR = 1
SET RC = 0

DO WHILE &RC < 8
  ISPEXEC TBDISPL MEMFIND0  PANEL(PVOL) +
          CURSOR(ZCMD) CSRROW(&CSSR)
  SET RC=&LASTCC
  IF &DEBUG = DEBUG THEN WRITE TBDISPL RC=&RC
  /***************************/
  /* Close up shop!! PF3/PF4 */
  /***************************/
  IF &RC = 8 THEN GOTO CLOSNGO
  SET ZCMD   =&LCMD
  SET ZTDROWS=&LTDROWS
  SET ZTDTOP =&LTDTOP
  SET ZTDSELS=&LTDSELS
  /***************************/
  /* Check for selections    */
  /***************************/
  IF &ZTDSELS > 0 THEN -
    DO
      CUTIL00 TRIM TDSN
      CUTIL00 TRIM TVOL
      IF &DEBUG = DEBUG THEN WRITE SEL=&SEL TDSN='&TDSN'
      IF &SUBSTR(1:1,&TDSN) > &STR( ) THEN -
        DO
          SET BDSN = &STR(&TDSN)
/*---BEGIN------------------- $$sel_proc ----------------------------*/
          /***************************/
          /* Save Display            */
          /***************************/
          ISPEXEC CONTROL DISPLAY SAVE
          /***************************/
          /* Process SELECTION       */
          /***************************/
          IF &SEL = B THEN -
            ISPEXEC BROWSE DATASET('&BDSN') VOLUME(&TVOL)
          ELSE -
            IF &SEL = E THEN -
              ISPEXEC EDIT   DATASET('&BDSN') VOLUME(&TVOL)
            ELSE -
              ISPEXEC SETMSG MSG(FIND019 )    /* Invalid selection   */
          SET &SEL = &STR( )
          /***************************/
          /* Restore Display         */
          /***************************/
          ISPEXEC CONTROL DISPLAY RESTORE
/*---END--------------------- $$sel_proc ----------------------------*/
        END
    END
END

CLOSNGO: +
ISPEXEC TBEND   MEMFIND0
SET RC=&LASTCC
IF &DEBUG = DEBUG THEN WRITE TBEND RC=&RC

DONEHERE: +
IF &DEBUG = DEBUG THEN WRITE ENDING... DONEHERE!
FREE ATTR(OUTATTR)
FREE FI(VTOCOUT) DELETE


END
./ ADD NAME=CLOC8
PROC 0 DEBUG

/********************************************************************/
/*                                                                  */
/* CLIST: CLOC8                                                     */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* This CLIST uses ISPF services under ISPF product from Wally      */
/* Mclaughlin, version 2.1 or greater.                              */
/*                                                                  */
/* Starting variables are fetched via an ISPF VGET,                 */
/*                                                                  */
/*      VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE          */
/*                                                                  */
/* A list of results are captured (via SYSOUTTRAP) using            */
/* the LOCATE CP.                                                   */
/*                                                                  */
/* Each result line is written to a TEMP DSN presented in           */
/* a BROWSE session for review.                                     */
/*                                                                  */
/* Command:                                                         */
/* as issued from menu panel PFINDM -                               */
/*  'CMD(%CLOC8 NEWAPPL(FIND)'                                      */
/*                                                                  */
/* Parameters:                                                      */
/* DEBUG    optional, used to display debug information during      */
/*          CLIST execution.  The value for this positional         */
/*          parameter is DEBUG.                                     */
/*                                                                  */
/*          i.e. CLOC8 DEBUG                                        */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/23/2021 0.9.10   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/

/*********************************************************************/
/* my Initialization                                                 */
/*********************************************************************/
SET MYNAME = &STR(CLOC8)             /* my Proc Name                 */

IF &DEBUG = DEBUG THEN -
  CONTROL MSG NOSYMLIST NOCONLIST NOLIST NOFLUSH
ELSE -
  CONTROL NOMSG NOSYMLIST NOCONLIST NOLIST NOFLUSH

/*********************************************************************/
/* ISPF must be available                                            */
/*********************************************************************/
ISPQRY
SET RC = &LASTCC
IF &RC NE 0 THEN -
 DO
   WRITE THIS CLIST REQUIRES ISPF TO BE ACTIVE
   EXIT CODE(12)
 END

/*********************************************************************/
/* ISPF Control Handler   (we handle errors!)                        */
/*********************************************************************/
ISPEXEC CONTROL ERRORS RETURN

/*********************************************************************/
/* Get ISPF Profile Variables                                        */
/*********************************************************************/
ISPEXEC VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE
ISPEXEC VGET (F1LVL F1DDN) PROFILE

CUTIL00 TRIM F1MEMBER PMEM
CUTIL00 TRIM F1DSN PDSN
CUTIL00 TRIM F1SRCH PSRCH
CUTIL00 TRIM F1QK PQK
CUTIL00 TRIM F1ALLDSN ALLQDSNS
CUTIL00 TRIM F1LVL PLVL
CUTIL00 TRIM F1DDN PDDN

SET OUTID =                          /*    Time Stamp                */
CUTIL00 TDSN OUTID

ISPEXEC VGET (LSCREEN) SHARED        /* Screen No from ISPF panel    */
                                     /* LOC8 Temp DSN                */
SET TMPDSN = &STR(&SYSUID..&MYNAME..S&LSCREEN..LOC8OUT.&OUTID)
                                     /* Other Temp DSNs should       */
                                     /* follow same naming           */
                                     /* convention                   */

IF &DEBUG = DEBUG THEN -
  WRITE TMPDSN='&TMPDSN'

/*********************************************************************/
/* LOC8OUT DD   Used by LOCATE trapped output                        */
/*  Free and Allocation                                              */
/*********************************************************************/
FREE ATTR(OUTATTR)
FREE FI(LOC8OUT)
ATTR OUTATTR RECFM(F B) LRECL(80) BLKSIZE(8000) DSORG(PS)
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE *** ATTR ERROR w/ OUTATTR, RC=&RC
    GOTO DONE
  END
ALLOC F(LOC8OUT) DA('&TMPDSN') MOD CATALOG TR +
  SP(10,5) USING(OUTATTR) UNIT(SYSDA)
SET RC = &LASTCC
IF &RC NE 0 THEN -
  DO
    WRITE *** ALLOCATION ERROR w/ LOC8OUT, RC=&RC
    GOTO DONE
  END

/*********************************************************************/
/* LOCATE Command                                                    */
/*   List DSN's where member is found                                */
/*********************************************************************/
SET LOC8CMD = &STR(LOCATE (
SET LOC8CMD = &LOC8CMD&PMEM
SET LOC8CMD = &LOC8CMD)&STR( ALL)

SET SYSOUTTRAP = 100               /* Turn ON TRAP, set LIMIT        */
&LOC8CMD
SET RC = &LASTCC
SET UPPER = &SYSOUTLINE            /* # LINES USED in TRAP           */
SET SYSOUTTRAP = 0                 /* TURN OFF TRAP                  */

IF &UPPER = 0 THEN -
 GOTO CONT01
SET CTR = 1
OPENFILE LOC8OUT OUTPUT
DO WHILE &CTR LE &UPPER
  SET EVALLINE = &&SYSOUTLINE&CTR
  SET LOC8OUT = &STR(&CTR:  &EVALLINE)
  PUTFILE LOC8OUT
  SET CTR = &CTR + 1
END
CLOSFILE LOC8OUT

IF &DEBUG = DEBUG THEN -
  WRITE LOCATE   RC=&RC

IF &RC NE 0 THEN -
  GOTO DONE

/******************************************************************/
/* Append Results DSN and LOCATE  command to end of report
/******************************************************************/
CONT01: +
OPENFILE LOC8OUT OUTPUT
IF &UPPER = 0 THEN -
  DO
    SET LOC8OUT = &STR(*** No members found ***)
    PUTFILE LOC8OUT
  END
SET LOC8OUT = &STR( )
PUTFILE LOC8OUT
SET LOC8OUT = &STR(Results: '&TMPDSN')
PUTFILE LOC8OUT
SET LOC8OUT = &STR(Command: &LOC8CMD)
PUTFILE LOC8OUT
CLOSFILE LOC8OUT

/******************************************************************/
/* BROWSE LOCATE  RESULTS
/******************************************************************/
ISPEXEC BROWSE DATASET('&TMPDSN')
SET RC=&LASTCC
IF &RC > 0 THEN -
  DO
    SET &MYRC = &RC
    SET &MYMSG = &STR(BROWSE ERROR)
    WRITE UNABLE TO BROWSE RESULTS
    GOTO DONE
   END

/******************************************************************/
/* FREE OUT DD and ATTR-NAME-LIST
/******************************************************************/
DONE: +
FREE ATTR(OUTATTR)
FREE FI(LOC8OUT) DELETE

EXITME: +
IF &DEBUG = DEBUG THEN -
  DO
    WRITE DBUG: EXITING LOCATE...
  END


END  /* PROC */
./ ADD NAME=CMEMINDD
PROC 0 DEBUG MEM() DD()

/********************************************************************/
/*                                                                  */
/* CLIST: CMEMINDD                                                  */
/*                                                                  */
/* Author: Larry Belmontes Jr.                                      */
/*         https://ShareABitofIT.net/FINDSRCH-in-MVS38J             */
/*         Copyright (C) 2020-2021 Larry Belmontes, Jr.             */
/*                                                                  */
/*                                                                  */
/* Description:                                                     */
/* ---------------------------------------------------------------  */
/*                                                                  */
/* This CLIST uses ISPF services under ISPF product from Wally      */
/* Mclaughlin, version 2.1 or greater.                              */
/*                                                                  */
/* Starting variables are fetched via an ISPF VGET,                 */
/*                                                                  */
/*      VGET (F1MEMBER F1DSN F1SRCH F1QK F1ALLDSN) PROFILE          */
/*                                                                  */
/* The FINDMEM CP will create ISPF variables for the requested      */
/* member which include:                                            */
/*                                                                  */
/*      FMCONCAT - CONCATENATION NUMBER WHERE MEMBER WAS FOUND      */
/*      FMDSNAME - DSNAME WHERE MEMBER WAS FOUND                    */
/*      FMLIB    - LIBRARY TYPE WHERE MEMBER WAS FOUND              */
/*                   'PRIVATE', 'LINKLIST' OR 'STEPLIB'             */
/*      FMMSG    - ERROR MSG WHEN NON ZERO RC                       */
/*      FMDIRENT - UNFORMATTED DIRECTORY ENTRY FROM BLDL            */
/*      FMLKEDDT - LINK EDIT DATE                                   */
/*                                                                  */
/* If a DD name is present, display the allocated DSN list          */
/* for the DD via CLIST CFLDSI which uses LISTDSJ to obtain         */
/* dataset attributes.                                              */
/*                                                                  */
/* All listings are displays to the terminal screen.                */
/*                                                                  */
/* Command:                                                         */
/* as issued from menu panel PFINDM -                               */
/*  'CMD(%CMEMINDD MEM() &PRM2 NEWAPPL(FIND)'                       */
/*                         !                                        */
/*                         !                                        */
/*                         +--&PRM2 is optional in CMEMINDD CLIST.  */
/*                            In using the variable (&PRM2),        */
/*                            set to NULL when no DD is specified   */
/*                            or set to the value of 'DD(&ddvar)'   */
/*                            before invoking the CMD.              */
/*                                                                  */
/* Parameters:                                                      */
/* DEBUG    optional, used to display debug information during      */
/*          CLIST execution.  The value for this positional         */
/*          parameter is DEBUG.                                     */
/*                                                                  */
/*          i.e. CMEMINDD DEBUG                                     */
/*                                                                  */
/*                                                                  */
/* Disclaimer:                                                      */
/* ================================================================ */
/*                                                                  */
/*    No guarantee; No warranty; Install / Use at your own risk.    */
/*                                                                  */
/*    This software is provided "AS IS" and without any expressed   */
/* or implied warranties, including, without limitation, the        */
/* implied warranties of merchantability and fitness for a          */
/* particular purpose.                                              */
/*                                                                  */
/*    The author requests keeping authors name intact in any        */
/* modified versions.                                               */
/*                                                                  */
/*    In addition, the author requests readers to submit any        */
/* code modifications / enhancements and associated comments        */
/* for consideration into a subsequent release (giving credit       */
/* to contributor(s)) thus, improving overall functionality         */
/* and further benefiting the MVS 3.8J hobbyist public domain       */
/* community.                                                       */
/*                                                                  */
/*                                                                  */
/*                                                                  */
/* Change History:                                                  */
/* ================================================================ */
/* MM/DD/CCYY Version  Name / Description                           */
/* ---------- -------  -------------------------------------------- */
/* 10/23/2021 0.9.10   Larry Belmontes Jr.                          */
/*                     - Initial version released to MVS 3.8J       */
/*                       hobbyist public domain                     */
/*                                                                  */
/********************************************************************/

IF &DEBUG = DEBUG THEN -
  CONTROL MSG NOSYMLIST NOCONLIST NOLIST NOFLUSH
ELSE -
  CONTROL NOMSG NOSYMLIST NOCONLIST NOLIST NOFLUSH

/*********************************************************************/
/* ISPF must be available                                            */
/*********************************************************************/
ISPQRY
SET RC = &LASTCC

IF &DEBUG = DEBUG THEN -
  WRITE ISPQRY, RC=&RC

IF &RC NE 0 THEN -
 DO
   WRITE THIS CLIST REQUIRES ISPF TO BE ACTIVE
   EXIT CODE(12)
 END

/*********************************************************************/
/* Display request header and invoke FINDMEM CP to create ISPF       */
/* variables                                                         */
/*********************************************************************/
IF &STR(&DD) = &STR( ) THEN -
 DO
  WRITE REQUEST: FIND MEMBER-&MEM
  FINDMEM &MEM
  SET RC = &LASTCC
 END
ELSE -
 DO
  WRITE REQUEST: FIND MEMBER-&MEM   IN DD-&DD
  FINDMEM &MEM &DD
  SET RC = &LASTCC
 END

IF &DEBUG = DEBUG THEN -
  WRITE FINDMEM, RC=&RC

WRITE ============================================

/*********************************************************************/
/* Display results from FINDMEM CP                                   */
/*********************************************************************/
IF &RC = 0 THEN -
  DO
    SET MEM      = &MEM
    SET FMLIB    = &FMLIB
    IF &FMLIB = &STR(STEPLIB) OR &FMLIB = &STR(LINKLIST) THEN -
      DO
        WRITE MEMBER &MEM FOUND IN &FMLIB+&FMCONCAT
      END
    ELSE -
      DO
        SET FMDSNAME = &FMDSNAME
   WRITE MEMBER &MEM FOUND IN &FMLIB LIB AT &DD+&FMCONCAT (&FMDSNAME)
      END
    IF &SUBSTR(1:1,&FMLKEDDT) = &STR(*) THEN
    ELSE -
       WRITE LINKEDIT DATE (YYJJJ) =&FMLKEDDT
  END
ELSE -
  DO
    SET FMMSG    = &FMMSG
    WRITE RC=&RC (&FMMSG)
  END

/*********************************************************************/
/* If DD specified, display allocated DD/DSN list via CFLDSI CLIST   */
/*********************************************************************/
WRITE .
IF &DEBUG = DEBUG THEN -
  IF &RC = 20 THEN -
    WRITE BYPASS CFLDSI...!

IF (&STR(&DD) NE &STR( )) AND (&RC NE 20) THEN -
  %CFLDSI DD(&DD)

END
@@
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CLDSI
PROC 0 DSN()

/*********************************************************************/
/* CLIST:  CLDSI                                                     */
/*                                                                   */
/* Description                                                       */
/* -----------                                                       */
/* List dataset attributes using LISTDSI                             */
/*                                                                   */
/* More information at:                                              */
/*  https://ShareABitOfIT.net/LISTDSJ-for-mvs-3-8j/                  */
/*                                                                   */
/* Syntax                                                            */
/* -------                                                           */
/* CLDSI DSN('''xxxxxxxx.xxxxxxxx.xxxxxxxx''')                       */
/*    DSN - Dataset Name, fully qualified                            */
/*         o use three single quotes in DSN                          */
/*                                                                   */
/* CLDSI DSN(TEST.CNTL)                                              */
/*    DSN - Dataset Name, prepend prefix                             */
/*         o prefix preceeds DSN if no quotes used in DSN            */
/*                                                                   */
/* Output:                                                           */
/*                                                                   */
/*-------------------------------------------------------------------*/
/*DSN:LARRY01.TEST.CNTL                                              */
/*MSG:                                                               */
/*RETURN CD: 0   SUCCESSFUL REQUEST                                  */
/*SYSREASON: 00   NORMAL COMPLETION                                  */
/*                                                                   */
/*DSORG RECFM LRECL BLKSZ KEYL  RKP   PASSWORD RACF CHGD             */
/*PO    FB    00080 19040 000   00000 NONE     NONE    Y             */
/*CREDT 2013-03-09  EXPDT 0000-00-00  REFDT 2019-01-30     CATL: Y   */
/*  13068 2013-068    00000 0000-000    19030 2019-030     CVOL: PUB000
/*PUB000                                                   VOLS: 001 */
/*ALLOCATION:     TYPE     PRI     USED     SEC        ALLOC         */
/*        PUB000  CYLINDER 00003   00002    0000001    00003         */
/*TRACKS: TOT     USED     UNUSED  EXTENTS                           */
/*        00090   00087    00003   003                               */
/*DEVICE: CYLS    TRKSCYL  TRKLEN  BLKSTRK  CAPACITY                 */
/*3350    00555   00030    19254   00001    000000320579100          */
/*PO DIR: BLKS    USED     UNUSED  MEMBERS  ALIAS                    */
/*        ?????   ?????    ?????   ?????    ?????                    */
/*-------------------------------------------------------------------*/
/*                                                                   */
/*                                                                   */
/*  Disclaimer: <DSCLAIMR>                                           */
/*  ================================================================ */
/*                                                                   */
/*  No guarantee; No warranty; Install / Use at your own risk.       */
/*                                                                   */
/*  This software is provided "AS IS" and without any expressed      */
/*  or implied warranties, including, without limitation, the        */
/*  implied warranties of merchantability and fitness for a          */
/*  particular purpose.                                              */
/*                                                                   */
/*                                                                   */
/*********************************************************************/

/*********************************************************************/
/* CLIST CONTROL statement                                           */
/*********************************************************************/
CONTROL MSG


/*********************************************************************/
/* EXECUTE LISDSI                                                    */
/*********************************************************************/
LISTDSI &DSN
SET RC = &LASTCC

/*********************************************************************/
/* Print results on terminal                                         */
/*********************************************************************/
SET WLINE = &STR(DSN: &SYSDSNAME)
WRITE &WLINE

SET WLINE = &STR(MSG: &SUBSTR(1:71,&SYSLISTDSJMSG)...)
WRITE &WLINE

SET WLINE = &STR(RETURN CD: &RC   &STR(&SYSMSGLVL1))
WRITE &WLINE

SET WLINE = &STR(SYSREASON: &SYSREASON   )
SET WLINE = &STR(&WLINE&STR(&SYSMSGLVL2))
WRITE &WLINE

WRITE

SET WLINE = &STR(DSORG RECFM LRECL BLKSZ KEYL  RKP)
SET WLINE = &STR(&WLINE   PASSWORD RACF CHGD)
WRITE &WLINE

SET WLINE = &STR(&SYSDSORG)
SET WLINE = &STR(&WLINE   &SYSRECFM)
SET WLINE = &STR(&WLINE &SYSLRECL)
SET WLINE = &STR(&WLINE &SYSBLKSIZE)
SET WLINE = &STR(&WLINE &SYSKEYLEN)
SET WLINE = &STR(&WLINE   &SYSKEYPOS)
SET WLINE = &STR(&WLINE &SYSPASSWORD)
SET WLINE = &STR(&WLINE    &SYSRACFA)
SET WLINE = &STR(&WLINE    &SYSUPDATED)
WRITE &WLINE

SET WLINE = &STR(CREDT &SYSCCREATE)
SET WLINE = &STR(&WLINE  EXPDT &SYSCEXDATE)
SET WLINE = &STR(&WLINE  REFDT &SYSCREFDATE)
SET WLINE = &STR(&WLINE     CATL: &SYSDSCAT)
WRITE &WLINE

SET WLINE = &STR(  &SYSJCREATE &SYSCREATE)
SET WLINE = &STR(&WLINE    &SYSJEXDATE &SYSEXDATE)
SET WLINE = &STR(&WLINE    &SYSJREFDATE &SYSREFDATE)
SET WLINE = &STR(&WLINE     CVOL: &SYSDSCATV)
WRITE &WLINE

SET WLINE = &STR(&SYSVOLUMES         )
SET WLINE = &STR(&WLINE             VOLS: &SYSNUMVOLS)
WRITE &WLINE

SET WLINE = &STR(ALLOCATION:     TYPE     PRI     USED)
SET WLINE = &STR(&WLINE     SEC        ALLOC)
WRITE &WLINE

SET WLINE = &STR(        &SYSVOLUME)
SET WLINE = &STR(&WLINE  &SYSUNITS)
SET WLINE = &STR(&WLINE &SYSPRIMARY)
SET WLINE = &STR(&WLINE   &SYSUSED)
SET WLINE = &STR(&WLINE    &SYSSECONDS)
SET WLINE = &STR(&WLINE    &SYSALLOC)
WRITE &WLINE

SET WLINE = &STR(TRACKS: TOT     USED     UNUSED  EXTENTS)
SET WLINE = &STR(&WLINE )
WRITE &WLINE

SET WLINE = &STR(        &SYSTRKSALLOC)
SET WLINE = &STR(&WLINE   &SYSTRKSUSED)
SET WLINE = &STR(&WLINE    &SYSTRKSUNUSED)
SET WLINE = &STR(&WLINE   &SYSEXTENTS)
WRITE &WLINE

SET WLINE = &STR(DEVICE: CYLS    TRKSCYL  TRKLEN  BLKSTRK)
SET WLINE = &STR(&WLINE  CAPACITY)
WRITE &WLINE

SET WLINE = &STR(&SYSUNIT)
SET WLINE = &STR(&WLINE &SYSCYLVOL)
SET WLINE = &STR(&WLINE   &SYSTRKSCYL)
SET WLINE = &STR(&WLINE    &SYSTRKLEN)
SET WLINE = &STR(&WLINE   &SYSBLKSTRK)
SET WLINE = &STR(&WLINE    &SYSUNITCAP)
WRITE &WLINE

IF &SYSADIRBLK NE &STR(?????) THEN -
  DO
    SET WLINE = &STR(PO DIR: BLKS    USED     UNUSED  MEMBERS)
    SET WLINE = &STR(&WLINE  ALIAS)
    WRITE &WLINE

    SET WLINE = &STR(        &SYSADIRBLK)
    SET WLINE = &STR(&WLINE   &SYSUDIRBLK)
    SET WLINE = &STR(&WLINE    &SYSNUDIRBLK)
    SET WLINE = &STR(&WLINE   &SYSMEMBERS)
    SET WLINE = &STR(&WLINE    &SYSMEMBERSALIAS)
    WRITE &WLINE
  END

/*********************************************************************/
/* Done!                                                             */
/*********************************************************************/
EXIT CODE(0)


@@
//CLIB   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=CLDSI
)F FUNCTION -
  The CLDSI CLIST displays dataset attributes using LISTDSI.

)X SYNTAX  -
         CLDSI DSN(xxxx)

  REQUIRED - DSN
  DEFAULTS - NONE
  ALIAS    -
)O OPERANDS -

))DSN      - DSN (dataset name)
             DSN in quotes is fully qualified, otherwise, prefix DSN
             with USERID


  Sample Commands

  1) CLDSI DSN('''HERC01.TEST.CNTL''')
     Displays dataset attributes for 'HERC01.TEST.CNTL'

  2) CLDSI DSN(TEST.CNTL)
     Displays dataset attributes for 'userid.TEST.CNTL'

./ ADD NAME=FINDMEM
)F FUNCTION -
  The FINDMEM command is used to find FIRST occurrence of
  member in a concatenation.  ISPF variables created are:

    FMCONCAT - CONCATENATION NUMBER WHERE MEMBER WAS FOUND.
    FMDSNAME - DSNAME WHERE MEMBER WAS FOUND
    FMLIB    - LIBRARY TYPE WHERE MEMBER WAS FOUND
                 'PRIVATE', 'LINKLIST' OR 'STEPLIB'
    FMMSG    - ERROR MSG WHEN NON ZERO RC
    FMDIRENT - UNFORMATTED DIRECTORY ENTRY FROM BLDL
    FMLKEDDT - LINK EDIT DATE

  RETURN CODES: (&LASTCC)
    00 - MEMBER FOUND
    04 - MEMBER NOT FOUND
    16 - PARM ERROR
    20 - DDNAME NOT ALLOCATED

  WRITTEN BY: ROB WUNDERLICH

)X SYNTAX  -
         FINDMEM member dd

  REQUIRED - member
  DEFAULTS - dd, defaults to LINKLIST if not specified
  ALIAS    - none

)O OPERANDS -
)) member           - The MEMBER name to find

)) dd               - optional, DDNAME to search. If not specified,
                      LINKLIST is searched.

./ ADD NAME=FINDSRCH
)F FUNCTION -

     The FINDSRCH TSO Command searches for a character string in a PDS
 (Partitioned Data Set) and displays the number of occurrences found
 in each member.

     This command can be executed under native TSO mode or SPF/Option 6.
 The command will prompt for Data Set Name and search argument.  The
 total for occurrences and members will also be displayed at the end of
 execution.


)X SYNTAX -

          FINDSRCH 'DSNAME' S('CHARACTER STRING')
                        GROUP(XX)
                        LOWER
                        QUICK
                        FILE
                        VOL
                        QUIET


  REQUIRED - 'DSNAME'  S('Character string')
  DEFAULTS -  None

)O OPERAND -

  'DSNAME'    - Name of the partitioned Data Set to be
                searched.  Omit the single quotes and the first
                data set qualifier if it is your own data set.

))S('Character string')

              - The string of characters to be searched for.  Could be a
                program name, data set name, or text data, etc..

))GROUP(XX)

              - Optional Keyword.  Used to limit the search on only
                members having name beginning with XX.
                Max length for GROUP is 7 bytes.


                E.g.  FIND  '$VNV.JCL.CNTL'  S('PGM=IDCAMS') GROUP($)


))LOWER

              - Optional Keyword.  Used to search for lowercase TEXT
                character string.  If this keyword is not used, the
                character string will be converted into uppercase before
                the search is being performed.

))QUICK

              - Optional Keyword.  Used to stop command from reading
                the entire member after the first occurrence found.
                Process continues to the next member.

))FILE

              - Optional Keyword.  Used to create a sequential file
                of results preallocated as DD OUTFIL with DCB of
                (RECFM=FB,LRECL=80,BLKSIZE=3200)

))VOL

              - Optional Keyword.  Used to specify volume serial
                number of Partitioned Data Set.

))QUIET

              - Optional Keyword.  Used to bypass display of results
                on terminal screen.


./ ADD NAME=LOCATE
 )F FUNCTION -
    LOCATE WILL REPORT THE NAME OF THE DATASET IN WHICH THE SPECIFIED
    MEMBER NAME(S) EXIST. SYSTEM LIBRARIES ARE SEARCHED IN A MANNER
    SIMILAR TO THE PROGRAM FETCH MODULE SEARCH ORDER. IF THE MEMBER
    NAME SPECIFIED IS AN ALIAS, AND IF THE MODULE WHICH IT NAMES
    IS REENTERABLE OR REUSABLE, LOCATE WILL ALSO IDENTIFY THE
    REAL MEMBER NAME.
 )X SYNTAX -
    LOCATE (MEMBER-NAMES) ALL

 REQUIRED - AT LEAST ONE MEMBER-NAME
 DEFAULTS - NONE
 OPTIONAL - ALL
 )O OPERANDS -
 ))MEMBER-NAMES - ONE OR MORE MEMBER OR ALIAS NAMES TO BE SEARCHED FOR
                IN THE SYSTEM LIBRARIES.
 ))ALL - REQUEST TO LOOK FOR MEMBER-NAME IN EACH OF THE SYSTEM
    LIBRARIES. IF ALL IS NOT SPECIFIED, ONLY THE FIRST OCCURRENCE
    OF EACH MEMBER-NAME WILL BE REPORTED, JUST AS PROGRAM FETCH
    STOPS AT THE FIRST PROGRAM COPY IT FINDS.
    LIBRARY SEARCH ORDER IS THE SAME WHETHER ALL IS SPECIFIED OR NOT.
      1) THE STEP LIBRARY, IF ONE EXISTS
      2) SYS1.LPALIB
      3) SYS1.LINKLIB
      4) THE SYSTEM LINKLIST, AS DEFINED BY LNKLST00
      5) SYS1.SVCLIB

@@
