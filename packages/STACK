//STACK  JOB (TSO),
//             'Install STACK',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//* This job installs the STACK TSO CP into SYS2.CMDLIB and the help  *
//* text into SYS2.HELP.                                              *
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//LOADMAC  EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=&&MACLIB,UNIT=SYSDA,DISP=(,PASS),
//             SPACE=(TRK,(120,30,10)),DCB=(SYS1.MACLIB)
//SYSIN    DD  *
./ ADD NAME=BPPL
         MACRO
&LAB1    BPPL   &DSECT
         LCLC  &LAB2
*
*               PARAMETER LIST PASSED TO OPERSCAN SUBROUTINE :
*
         AIF   (T'&LAB1 NE 'O').LOK
&LAB2    SETC  'BPPL'
         AGO   .DSCK
.LOK     ANOP
&LAB2    SETC  '&LAB1'
.DSCK    AIF   ('&DSECT' EQ 'DSECT').DSL
         DS    0A
&LAB2    DS    0XL44
         AGO   .ADSL
.DSL     ANOP
&LAB2    DSECT
.ADSL    ANOP
BUFFPTR  DS    F                        BUFFER PTR (CBUF)
LENPTR   DS    F                        LENGTH PTR (CBUF LEN)
STARTPTR DS    F                        START SEARCH PTR
OPERPTR  DS    F                        NEXT OPER LOC
OPLENPTR DS    F                        NEXT OPER LEN PTR
SUBPTR   DS    F                        SUBFIELD PTR (PARENS INCLUDED)
SUBLENPT DS    F                        SUBFIELD LEN PTR
WORKPTR  DS    F                        WORK PTR (OPTIONAL 350 BYTES)
OPDESCP  DS    F                        PTR TO OPERAND DESCRIPTOR
         DS    F                        RESERVED (ZERO)
*
OPLEN    DS    H                        POINTED TO BY OPLENPTR
SUBLEN   DS    H                        POINTED TO BY SUBLENPT
         MEND
./ ADD NAME=ENTERR
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
./ ADD NAME=IKJCVT
         MACRO
&N       IKJCVT
CVTPTR   EQU   16
CVTMAP   DSECT
         ORG   CVTMAP+X'9C'
CVTTVT   DS    A
         ORG   CVTMAP+480
CVTSCAN  DS    A
         ORG   CVTMAP+524
CVTPARS  DS    A
         ORG   CVTMAP+732
CVTDAIR  DS    A
CVTEHDEF DS    A
CVTEHCIR DS    A
         MEND
./ ADD NAME=KPPL
         MACRO
&LAB1    KPPL   &DSECT
         LCLC  &LAB2
*
*               PARAMETER LIST PASSED TO KMDPARS (12 FULLWORDS):
*
         AIF   (T'&LAB1 NE 'O').LOK
&LAB2    SETC  'KPPL'
         AGO   .DSCK
.LOK     ANOP
&LAB2    SETC  '&LAB1'
.DSCK    AIF   ('&DSECT' EQ 'DSECT').DSL
         DS    0A
&LAB2    DS    0XL48
         AGO   .ADSL
.DSL     ANOP
&LAB2    DSECT
.ADSL    ANOP
CBUFPTR  DS    F                        CMDBUF TO BE PARSED
OPLSTPTR DS    F                        LIST OF OPERANDS TO FLAG
FLAGPTR  DS    F                        WHERE TO FLAG THEM
UNKNEXIT DS    F                        EXIT TO CALL IF UNKNOWN OPER
EXITPARM DS    F                        PARAMETER TO PASS EXITS (ADDR)
*                                       (R1 POINTS HERE AT EXIT ENTRY)
BPPLPASS DS    F                        BPPL PTR WHEN EXIT GETS CONTROL
WORKPASS DS    F                        OPTIONAL WORKAREA (512 BYTES)
KEYLPASS DS    F                        LIST OF KEYWORD OPERANDS
KEYWPASS DS    F                        WORKAREA FOR KEYWORD PROCESSOR
REEXPASS DS    F                        ADDRESS OF REAL UNKNOWN EXIT
BUFFLEN  DS    F                        LEN OF STR BUFF
         DS    F                        RSVD- FLAGS
         MEND
./ ADD NAME=LEAVER
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
./ ADD NAME=MOVE
         MACRO
&L1      MOVE  &TO,&TL,&FROM,&FL,&PAD=
         LCLA  &NL,&UL,&VL,&RP,&RP2,&RC
         LCLC  &TO$,&TL$,&FROM$,&FL$
         LCLC  &R1(2),&R2(2),&RN1,&RN2
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
.FLC     AIF   (T'&FL EQ 'O').OO
         AIF   ('&FL'(1,1) NE '(').OO
&FL$     SETC  '0&FL'
.OO      ANOP
&L1      STM   14,12,12(13)
         LA    &R1(1),&TO$
         LA    &R2(1),&TL$
         LA    &R1(2),&FROM$
         AIF   (T'&FL EQ 'O').UTL
         LA    &R2(2),&FL$
         AGO   .PC
.UTL     LA    &R2(2),&TL$
.PC      AIF   ('&PAD' EQ '').NPC
         ICM   &R2(2),8,=&PAD
.NPC     MVCL  &R1(1),&R1(2)
         LM    14,12,12(13)
         MEND
./ ADD NAME=OPER
         MACRO
&OPLB    OPER  &ON,&F,&EXIT=,&BLANK=,&MINLEN=,&KEYFLD=,&SUBFLD=
         GBLC  &XPARM
         LCLA  &I
         LCLA  &L
&OPLB    DS    0F                       FULLWORD BNDRY
         DC    &F                       FLAG LOC (SET IF OPER FOUND):
*                                       BYTE1=FLAG BIT,BYTE2=FLAG BYTE
         AIF   ('&EXIT&SUBFLD' EQ '').S1
&I       SETA  &I+8
.S1      AIF   ('&BLANK' EQ '').S2
&I       SETA  &I+4
.S2      AIF   ('&KEYFLD' EQ '').S2B
&I       SETA  &I+2
.S2B     AIF   ('&SUBFLD' EQ '').SS
&I       SETA  &I+1
.SS      DC    AL1(&I*16)        BYTE1= OPTIONS (80 = TAKE EXIT)
*              (40 = OVERLAY WITH BLANKS,20 = KEYWORD OPERAND)
*              (10 = SUBFIELD AREA OFFSET PROVIDED FOR SUBEX )
         DC    AL1(0&MINLEN)     BYTE2= MINIMUM LENGTH OF OPER TO MATCH
         AIF   ('&EXIT' EQ '').S3A
         DC    A(&EXIT)                 EXIT ADDR
         AGO   .S4
.S3A     AIF   ('&SUBFLD' EQ '').S3
         DC    A(X&SYSNDX)              EXTERNAL EXIT TRANSFER
         AGO   .S4
.S3      AIF   ('&KEYFLD' EQ '').SN    NO EXIT
         DC    A(&KEYFLD)         ADDRESS OF KEYFLD ->
*        HFWD  FIELD LEN, HFWD DATA LEN, DATA FIELD
         AGO   .S4
.SN      DC    A(0)                     NO EXIT
.S4      ANOP
&L       SETA  K'&ON                    NAME LENGTH
         DC    H'&L'                    OPERAND LENGTH (SUBFLD EXCL)
         DC    C'&ON'                   OPERAND NAME
         SPACE
         AIF   ('&SUBFLD' EQ '').SX
X&SYSNDX DC    V(SUBEX)
         DC    A(&SUBFLD-&XPARM)
         SPACE
.SX      MEND
./ ADD NAME=REGEQU
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
./ ADD NAME=WTP
         MACRO
&LAB1    WTP   &TXT,&LEN,&MF=I
         LCLC  &GT,&T,&CL,&P
         LCLA  &TL
&P       SETC  '()'(1,1)
&T       SETC  T'&TXT
.*       MNOTE 'MESSAGE TYPE IS &T'
         AIF   (T'&TXT EQ 'U').MP
         AIF   (T'&TXT EQ 'N').MP
.* NO MESSAGE PRESENT
&GT      SETC  (125)' '
&TL      SETA  125
         AGO   .NQL
.* MESSAGE PRESENT
.MP      ANOP
&GT      SETC  '&TXT'
         AIF   ('&TXT'(1,1) NE '''').NQL
&GT      SETC  '&TXT'(2,K'&TXT-2)
.* CHECK LENGTH:
.NQL     ANOP
&T       SETC  T'&LEN
.*       MNOTE 'LENGTH TYPE IS &T'
         AIF   (T'&LEN NE 'O').NLC
.* NO LENGTH PRESENT
         AIF   (T'&TXT EQ 'O').DL
         AIF   (T'&TXT EQ 'U').UL
&TL      SETA  L'&TXT
         AGO   .LOK
.UL      ANOP
&TL      SETA  K'&GT
         AGO   .NLL
.* NO LENGTH AND NO MESSAGE: DEFAULT LEN = 125
.DL      ANOP
&TL      SETA  125
         AGO   .NLL
.* LENGTH PRESENT
.NLC     AIF   (T'&LEN EQ 'N').SDL
         AIF   (T'&LEN EQ 'U').NLL
.* LENGTH NOT SELF DEFINING
&TL      SETA  L'&LEN
         AGO   .LOK
.* LENGTH SELF DEFINING
.SDL     ANOP
         AIF   ('&LEN'(1,1) EQ '&P').NLL
&TL      SETA  &LEN
.LOK     ANOP
.* FORCE MESSAGE LENGTH:
&GT      SETC  '&GT'.(125)' '
&GT      SETC  '&GT'(1,&TL)
.NLL     ANOP
         AIF   ('&MF(1)' EQ 'L').WTPL
         AIF   ('&MF(1)' EQ 'E').WTPE
&LAB1    WTO   &TXT,ROUTCDE=(11)
         MEXIT
.WTPL    ANOP
&LAB1    WTO   '&GT',ROUTCDE=(11),MF=L
         MEXIT
.WTPE    ANOP
         AIF   (T'&TXT NE 'O').YM
         AIF   (N'&MF EQ 2).MFOK
         MNOTE 8,'WTP MF TYPE INVALID'
         MEXIT
.MFOK    ANOP
&LAB1    WTO   MF=&MF
         MEXIT
.YM      ANOP
&LAB1    DS    0H
         LH    1,&MF(2)
         SH    1,=H'6'
         MVI   &MF(2).+4,C' '
C&SYSNDX MVC   &MF(2).+5(0),&MF(2).+4   CLEAR WTO BUFF
         EX    1,C&SYSNDX
         AIF   (T'&LEN EQ 'U').UL2
         AIF   (T'&LEN EQ 'O').AIL
         AIF   ('&LEN'(1,1) NE '&P').AIL
&P       SETC  '&LEN'(2,K'&LEN-2)
&CL      SETC  '0'
         BCTR  &P,0
         EX    &P,M&SYSNDX
         LA    &P,1(&P)
         AGO   .CT
.AIL     ANOP
&CL      SETC  '&TL'
         AGO   .CT
.UL2     ANOP
&CL      SETC  '&LEN'
.CT      AIF   (T'&TXT EQ 'C').CC
M&SYSNDX MVC   &MF(2).+4(&CL),=C'&GT'
         AGO   .EWTO
.CC      ANOP
M&SYSNDX MVC   &MF(2).+4(&CL),&TXT
.EWTO    ANOP
         WTO   MF=&MF
         MEND
//*
//ASM1     EXEC ASMFC,REGION.ASM=1024K,
//             PARM.ASM='LIST,LOAD,NODECK,NOXREF,RENT'
//ASM.SYSLIB DD
//           DD DSN=&&MACLIB,DISP=OLD
//ASM.SYSPRINT DD  SYSOUT=*,DCB=BLKSIZE=1089
//ASM.SYSIN DD *
         TITLE 'STACK CP - PUT OUTPUT OF TSO CMD TO A FILE'
*
*  V1M4 - MODIFIED TO STOP ABEND 001 IN BATCH TMP RUNS - 01/31/85 KK4
*  V1M5 - MODIFIED TO ALLOW INDD/OUTDD SYNONYMS        - 11/12/86 KK5
*  V2M0 - MODIFIED TO LOOP W/LINK TO CMDS AS SPEED TEST- 09/06/88 KK6
*
STACK    ENTERR LEVEL=V2M0
         LR    R11,R1                   R11 -> CPPL
         USING CPPL,R11
         EJECT
**********************************************************************
*       MAINLINE PROCESSING CONTROL ROUTINE:                         *
**********************************************************************
*
*              FIRST STEP IS EXTRACT OUR OPERANDS FROM COMMAND:
*
B4       BAL   R14,PB                   FIX BUFFER
         LTR   R15,R15                  OK?
         BNZ   TERMWERR                 NO
*
*              NOW GET SET UP
*
         L     R6,CPPLECT               R6->ECT
         USING ECT,R6                                               KK6
         MVC   TAG,=CL8'NEWISPF'  ALLOWS ISPF XL CMD                KK6
         MVC   COPYCPPL,CPPL      "                                 KK6
         L     R1,CPPLECT         R1->ECT                           KK6
         MVC   COPYECT(56),0(R1)  SAVE FOR WDPSCXL (IOSRL NEEDED)   KK6
         LA    R1,COPYECT         R1->NEW ECT                       KK6
         ST    R1,COPYCPPL+12     CHAIN IT                          KK6
*
NXTCMD   BAL   R14,STACKME              STACK AND GET CMD
         CH    R15,=H'12'         NOWAIT READ FAILED(NO MORE CLISTS)KK6
         BE    TERM1              YEP... ALL DONE                   KK6
         CH    R15,=H'16'         EOD? (NESTED CLIST MAY BE DONE)   KK6
         BE    NXTCMD                                               KK6
         LTR   R15,R15                  OK?
         BNZ   TERMWERR                 NO
*
         BAL   R14,SCAN                 SCAN FOR A CMD NAME
         LTR   R15,R15                  OK?
         BNZ   TERMWERR                 NO
*
         CLC   EPNAME,=CL8'STACKEND' END FAST?                      KK6
         BE    TERM1              OK                                KK6
         TM    FLAG,X'08'         FAST RUN?                         KK6
         BNO   TRYATT             NO->NORMAL RUN                    KK6
*
*              DO THE CMD
*
         BAL   R14,LINKIT         LINK TO CMD IN FAST MODE          KK6
         B     CHEKFAST                                             KK6
*
TRYATT   BAL   R14,ATTACHME             CALL HIM
*
*  IF "FAST" TYPE REQUEST, AND NOT STACKEND CMD, LOOP TO NXTCMD:    KK6
*
CHEKFAST TM    FLAG,X'08'         FAST?                             KK6
         BO    NXTCMD             YES->LOOP                         KK6
         EJECT
*
*              REMOVE FROM STACK ?:
*
TERM1    BAL   R14,UNSTACKM             REMOVE TOP IF WE ADDED
*
         L     R15,AECB                 GET CMD RC IF NORMAL
         SPACE 3
TERM     LEAVER
         SPACE 3
TERMWERR WTP   'ERROR HAS OCCURED IN STACK CMD'
         B     TERM
         EJECT
*
*        PB -  PROCESS BUFFER TO REMOVE AND FLAG OUR OPERANDS
*
PB       ST    R14,L1LS
*
*       FIRST BUILD KOMAND-PROCESSING PARAMETER LIST:
*
BPI      XC    KPPL(40),KPPL            CLEAR BUFFER PROC PARMLIST
         L     R10,0(,R11)              R10 -> CBUF
         ST    R10,CBUFPTR              BUFFER PTR
         LA    R10,OPLIST               OPERAND LIST
         ST    R10,OPLSTPTR             PASS IT
         LA    R9,FLAG                  FLAG AREA
         ST    R9,FLAGPTR               PASS IT
         LA    R8,UNKNEX                EXIT FOR OPS NOT IN MY LIST
         ST    R8,UNKNEXIT              PASS IT
         LA    R6,L1LS                  WORK AREA FOR EXIT
         ST    R6,EXITPARM              PASS IT TO THE EXIT
         LA    R7,FASTAREA              R7 -> OPLEN AREA
         ST    R7,WORKPASS              LENGTH PTR RETURN AREA
         XC    OPFLAG,OPFLAG            CLEAR OPERANDS PRESENT FLAG
*
*       THEN CALL KMDPARS (PARSE SUBROUTINE):
*
         LA    R1,KPPL                  R1 -> BUFFER PROC PARMLIST
DOPO     CALL  KMDPARS                  PROCESS AN OPERAND
         LTR   R15,R15                  ANYTHING FOUND?
         BNZ   BPT                      YES
         OI    OPFLAG,X'FF'
*
*       SAY DONE AND EXIT:
*
BPT      SLR   R15,R15                  RC=0
         L     R14,L1LS
         BR    R14
*
*   THIS EXIT SETS A FLAG IF ANY OPERANDS WHICH ARE NOT KNOWN ARE HERE:
*
UNKNEX   L     R1,0(,R1)                R1 -> L1LS
         USING L1LS,R1
         OI    UNKNFLAG,X'FF'           SET IT ON
         SLR   R15,R15                  RC = 0  SAY OK TO CONTINUE
         BR    R14                      RETURN TO KMDPARS
         DROP  R1
         EJECT
*
*   THIS EXIT SAVES THE DDNAMES:
*
DDINS    LA    R15,12(,R15)
DDOUTS   LA    R15,12(,R15)
TASKSV   LA    R15,10(,R15)
DDSENT   STM   R14,R12,12(R13)          SAVE REGS
         BALR  R11,R0                   R11->DDSTART
         USING DDSTART,R11
DDSTART  LA    R11,0(,R11)              CLR HI BYTE
         SR    R15,R11                  R15 = 16,8,OR 0
         L     R10,0(,R1)               R10-> L1LS
         USING L1LS,R10
         L     R9,4(,R1)                R9->BPPL
         USING BPPL,R9
         L     R8,SUBPTR                R8->SUB FLD
         LTR   R8,R8                    ANY?
         BZ    TRUBELL                  NO
         L     R7,SUBLENPT              R7->SUBFLD LEN
         LH    R6,0(R7)                 R6=SUBLEN
         SH    R6,=H'3'                 -3 FOR PARENS AND EX
         MVC   DDSAVE(8),=CL8' '        BLANK FILL
RMVC     MVC   DDSAVE(0),1(R8)          EX ED MVC
         EX    R6,RMVC                  SAVE IT
         LA    R5,TASKDD(R15)           PT TO CORRECT AREA FOR THIS GUY
         MVC   0(8,R5),DDSAVE           MOVE HIM TO HIS SLOT
TRUBELL  LM    R14,R12,12(R13)          RESTORE REGS
         SLR   R15,R15                  RC = 0  SAY OK TO CONTINUE
         BR    R14                      RETURN TO KMDPARS
         DROP  R11
         DROP  R10
         DROP  R9
         USING CPPL,R11
         EJECT
*
*   THIS ROUTINE STACKS THE DATASETS AND GETS THE CMD:
*
STACKME  ST    R14,L1LS
*
         TM    FLAG,X'80'               DDIN PRESENT?
         BO    STCK                     YES->DOIT
GETPRE   BAL   R14,GETL                 NO-> GET THE CMD FIRST
         LTR   R15,R15            OK?
         BNZ   STEX               NO
         SPACE
STCK     TM    FLAG,X'C0'               ANY DD TO STACK?
         BZ    GETPOST                  NO
STACKIT  BAL   R14,ADDIT                YES->DOIT
         LTR   R15,R15            OK?
         BNZ   STEX               NO
         SPACE
GETPOST  TM    FLAG,X'80'               DDIN PRES?
         BNO   PUTCK                    NO->WE ALREADY DID THIS
         BAL   R14,GETL                 YES-> GET CMD
         LTR   R15,R15            OK?
         BNZ   STEX               NO
         SPACE
PUTCK    TM    FLAG,X'10'               LIST?
         BNO   STEX
         BAL   R14,PUTL                 YES-> ECHO CMD
         SPACE
STEX     L     R14,L1LS
         BR    R14
         EJECT
*
*   THIS ROUTINE STACKS THE DDNAME:
*
ADDIT    ST    R14,L2LS
*
         L     R7,CPPLUPT               R7->UPT
         TM    FLAG,X'80'               DDIN PRES?
         BZ    SE2                      NO
         SPACE 2
SE       STACK PARM=STPB,UPT=(R7),ECT=(R6),ECB=ECB,                    X
               DATASET=(OUTDD=DDOUT,CNTL,SEQ,INDD=DDIN),MF=(E,IOPL)
         SPACE
         B     ADDX
         EJECT
*
SE2      STACK PARM=STPB,UPT=(R7),ECT=(R6),ECB=ECB,                    X
               DATASET=(OUTDD=DDOUT,CNTL,SEQ),                         X
               MF=(E,IOPL)
         SPACE 3
ADDX     LTR   R15,R15            STACK OK?
         BNZ   AXT                NO
         SPACE
         MVC   SAVESWS,28(R6)  SAVE ECTSWS FIELD - KK4 01/31/85
         OI    28(R6),X'02'             PRETEND BACKGROUND
         SPACE
         L     R5,4(,R6)          R5->IOSRL
         MVC   SAVELEM(4),4(R5)   SAVE BOTTOM OF STACK PTR
         MVC   4(4,R5),0(R5)      PRETEND ONLY ONE ELEMENT IN STACK
*              (PREVENTS SUBTASK FROM TRYING TO DELETE IT)
         SPACE
AXT      L     R14,L2LS
         BR    R14
         EJECT
GETL     ST    R14,L2LS
         L     R7,CPPLUPT               R7->UPT
         TM    FLAG,X'08'         FAST?                             KK6
         BO    GETFAST                                              KK6
         SPACE 3
         GETLINE  PARM=GTPB,UPT=(R7),ECT=(R6),ECB=ECB,                 X
               INPUT=(ISTACK,LOGICAL),TERMGET=(EDIT,WAIT),MF=(E,IOPL)
         B     GX1                                                  KK6
         EJECT
*                                                                   KK6
*  ADD NOWAIT TYPE GETLINE FOR 'FAST' MODE TO DETECT CLIST ENDS:    KK6
*                                                                   KK6
GETFAST  GETLINE  PARM=GTPB,UPT=(R7),ECT=(R6),ECB=ECB,                 X
               INPUT=(ISTACK,LOGICAL),TERMGET=(EDIT,NOWAIT),MF=(E,IOPL)
         SPACE 3
GX1      C     R15,=F'4'          RC=4
         BNE   GETX               NO->LEAVE IT
         SLR   R15,R15            4 SAME AS 0
         SPACE 3
GETX     L     R14,L2LS
         BR    R14
         EJECT
PUTL     ST    R14,L2LS
         L     R5,GTPB+4
         SPACE 3
         PUTLINE  PARM=PUTLIST,UPT=(R7),ECT=(R6),ECB=ECB,              X
               OUTPUT=((R5),TERM,SINGLE,DATA),MF=(E,IOPL)
         SPACE 3
PUTX     L     R14,L2LS
         BR    R14
         EJECT
*
*        THIS  ROUTINE ATTACHES THE CMD:
*
ATTACHME ST    R14,L1LS
*
*              CALL APPROPRIATE ATTACH SUBRTN:
*
         TM    FLAG,X'20'               TASKDD PRES?
         BNO   NTL                      NO
         BAL   R14,AWTL                 YES->ATTACH WITH TASKLIB
         B     AEX                      GOON
         SPACE
NTL      BAL   R14,AWNTL                ATTACH W/NO TASKLIB
         SPACE 2
AEX      L     R14,L1LS
         BR    R14
         EJECT
SCAN     ST    R14,L2LS
         LA    R10,CSPLA                R10->CSPL
         USING CSPL,R10
         LA    R9,CSOAA                 R9->CSOA
         USING CSOA,R9
         XC    CSPLA(9),CSPLA
         MVC   CSPLUPT,CPPLUPT
         MVC   CSPLECT,CPPLECT
         LA    R8,ECB             R8->ECB
         ST    R8,CSPLECB
         LA    R7,CSOAF           R7->CSOAF
         ST    R7,CSPLFLG
         ST    R9,CSPLOA
         L     R1,GTPB+4                R1->CBUF
         ST    R1,CSPLCBUF              PASS CBUF TO SCAN
         CLI   4(R1),C'0'               FIRST CHAR < 0 ?
         BL    SCANIT                   YES->NO LINE NUMBER
         MVC   4(8,R1),=CL8' '          NO->KILL LINE NUMBER
         SPACE
SCANIT   LR    R1,R10                   R1->CSPL
         CALLTSSR EP=IKJSCAN
         LTR   R15,R15
         BNZ   SCANEX
         EJECT
         TM    CSOAFLG,X'C0'      ANY?
         BNZ   GCN                YES
SCANERR  LA    R15,33             NO->RC=33
         B     SCANEX
GCN      L     R4,CSOACNM               R4->CMD NAME
         LTR   R4,R4                    OK?
         BZ    SCANERR            YES
GCL      LH    R5,CSOALNM               R5=CMD NAME LEN
         LTR   R5,R5              R5>0?
         BNP   SCANERR            NO
         BCTR  R5,R0                    R5=R5-1 FOR EX
         MVC   EPNAME,=CL8' '           CLEAR EPNAME AREA
RM       MVC   EPNAME(0),0(R4)          EX'ED CMDNAME SAVE
         EX    R5,RM                    DOIT
         SPACE
         MVC   ECTPCMD,=CL8' '          CLEAR ECT NAME AREA
CNS      MVC   ECTPCMD(0),0(R4)         EX'ED CMDNAME SAVE
         EX    R5,CNS                   DOIT
         SPACE
         NI    ECTSWS,255-ECTNOPD SAY OPERANDS
         CLI   CSOAFLG,X'80'      PRESENT?
         BE    ISPCHK             YES
         OI    ECTSWS,ECTNOPD     SAY NO OPERANDS
         SPACE
ISPCHK   CLC   EPNAME,=CL8'ISPEXEC' ISPF SERVICE CALL ?             KK6
         BNE   TIMECHK            NO                                KK6
         MVC   EPNAME,=CL8'SPFEXEC'                                 KK6
         L     R1,GTPB+4          R1->CBUF                          KK6
         XC    2(2,R1),2(R1)      RESET OFFSET FOR SPFEXEC SCAN     KK6
         B     SCANEX                                               KK6
TIMECHK  CLC   EPNAME,=CL8'TIME'  PSUEDO TIME?                      KK6
         BNE   SCANEX             NOPE                              KK6
         MVC   EPNAME,=CL8'IKJEFT25' WE ARE THE TMP!                KK6
SCANEX   L     R14,L2LS
         BR    R14
         EJECT
LINKIT   ST    R14,L2LS
         MVC   CPPLCBUF,GTPB+4                                      KK6
         TM    CSOAFLG,X'04'      CLIST?                            KK6
         BO    CLIST                                                KK6
LINK2    EQU   *                                                    KK6
         LOAD  EPLOC=EPNAME,ERRET=CLIST                             KK6
         LA    R1,CPPL                                              KK6
         LINK  EPLOC=EPNAME,SF=(E,LLIST)                            KK6
         ST    R15,ECTRCDF        SAVE CMD RC                       KK6
         SLR   R15,R15                                              KK6
         L     R14,L2LS                                             KK6
         BR    R14                                                  KK6
CLIST    MVC   EPNAME,=CL8'EXEC'                                    KK6
         L     R1,GTPB+4          R1->CBUF                          KK6
         XC    2(2,R1),2(R1)      RESET OFFSET FOR    EXEC SCAN     KK6
         B     LINK2                                                KK6
         EJECT
AWNTL    ST    R14,L2LS
         SPACE 2
AWNTL2   MVC   CPPLCBUF,GTPB+4          PASS CBUF
         LA    R1,CPPL                  PASS CPPL
         SPACE
         XC    ATTACHL(60),ATTACHL
         ATTACH EPLOC=EPNAME,ECB=AECB,SHSPV=78,SF=(E,ATTACHL)
ACFIN    LTR   R1,R1                    ANY TCB?
         BZ    AWNTLX                   NO->FASTEXIT
         ST    R1,TCBADD
         SPACE 2
         XC    AECB,AECB                CLEAR ECB
         WAIT  ECB=AECB                 LET HIM RUN THEN
         SPACE 2
         DETACH TCBADD                  REMOVE DEAD TCB
         SPACE 2
AWNTLX   L     R14,L2LS
         BR    R14
         EJECT
AWTL     ST    R14,L2LS
         SPACE
         MVC   DCBL(96),DCBP            INIT DCB
         MVC   DCBL+40(8),TASKDD       GET DDNAME
         MVC   OPENL(4),=X'80000000'    INIT OPEN
         OPEN  (DCBL,INPUT),MF=(E,OPENL)  OPEN THE TASKLIB
         TM    DCBL+48,X'10'            DID IT OPEN?
         BZ    AWNTL2                   NO->DON'T USE IT THEN
         MVC   CPPLCBUF,GTPB+4          PASS CBUF
         LA    R1,CPPL                  PASS CPPL
         SPACE 2
         XC    ATTACHL(60),ATTACHL
         ATTACH EPLOC=EPNAME,ECB=AECB,SHSPV=78,DCB=DCBL,               X
               TASKLIB=DCBL,SF=(E,ATTACHL)
         SPACE
         B     ACFIN                    REST IS THE SAME
         EJECT
*
*        THIS  ROUTINE UNSTACKS WHATEVER WE STACKED:
*
UNSTACKM ST    R14,L1LS
*
         TM    FLAG,X'C0'               ANYTHING STACKED?
         BZR   R14                      NO->EXIT FAST
         SPACE 2
         L     R5,4(,R6)          R5->IOSRL
         MVC   4(4,R5),SAVELEM    RESTORE TRUE STACK BOTTOM FIRST
         SPACE 2
         BAL   R14,DELIT                REMOVE TOP ENTRY
         SPACE 3
USX      L     R14,L1LS
         BR    R14
         EJECT
*
*        THIS  ROUTINE REMOVES THE TOP ELEMENT ON THE STACK:
*
DELIT    ST    R14,L2LS
*
         L     R7,CPPLUPT               R7->UPT
*
SED      STACK PARM=STPB,UPT=(R7),ECT=(R6),ECB=ECB,                    X
               DELETE=TOP,MF=(E,IOPL)
         SPACE 3
* THIS RETURNS TO FOREGROUND MODE ONLY IF NOT IN BATCH - KK4 01/31/85
         MVC   28(1,R6),SAVESWS   RESTORE ECTSWS FIELD - KK4 01/31/85
*KK4     NI    28(R6),X'FD'             RETURN TO FOREGROUND
         SPACE 3
         L     R14,L2LS
         BR    R14
         EJECT
STATWORK EQU   *
*
*              STACK DDIN() DDOUT() TASKLIB() LIST
*
OPLIST   DC    F'0'                     LIST OF KNOWN OPERANDS:
         DC    A(OPER1)
         DC    A(OPER2)
         DC    A(OPER3)
         DC    A(OPER4)
         DC    A(OPER5)   KK5
         DC    A(OPER6)   KK5
         DC    A(OPER7)   KK6
         DC    F'0'
         DC    F'0'
         SPACE
OPER1    DS    0F
         DC    X'8000'                  BYTE1=FLAG MASK,BYTE2=FLAG BYTE
         DC    X'8000'                  OPTIONS  (X'80'=TAKE EXIT)
*                                       OPTIONS  (X'40'=BLANK OUT)
         DC    A(DDINS)                 EXIT ADDR
         DC    H'4'                     OPERAND LENGTH
DDI      DC    CL8'DDIN'                OPERAND NAME
         SPACE
OPER2    DS    0F
         DC    X'4000'
         DC    X'8000'                  OPTIONS  (X'80'=TAKE EXIT)
*                                       OPTIONS  (X'40'=BLANK OUT)
         DC    A(DDOUTS)                EXIT ADDR
         DC    H'5'
DDO      DC    CL8'DDOUT'
         SPACE
OPER3    DS    0F
         DC    X'2000'
         DC    X'8000'                  OPTIONS  (X'80'=TAKE EXIT)
*                                       OPTIONS  (X'40'=BLANK OUT)
         DC    A(TASKSV)                EXIT ADDR
         DC    H'7'
TASKLB   DC    CL8'TASKLIB'
         SPACE
OPER4    DS    0F
         DC    X'1000'
         DC    X'0000'                  OPTIONS  (X'80'=TAKE EXIT)
*                                       OPTIONS  (X'40'=BLANK OUT)
         DC    A(0)                     EXIT ADDR
         DC    H'4'
LIST     DC    CL8'LIST'
         SPACE
OPER5    DS    0F                                          KK5
         DC    X'8000'                  BYTE1=FLAG MASK,BYTE2=FLAG BYTE
         DC    X'8000'                  OPTIONS  (X'80'=TAKE EXIT)
*                                       OPTIONS  (X'40'=BLANK OUT)
         DC    A(DDINS)                 EXIT ADDR
         DC    H'4'                     OPERAND LENGTH
DDI2     DC    CL8'INDD'                OPERAND NAME
         SPACE
OPER6    DS    0F                                          KK5
         DC    X'4000'
         DC    X'8000'                  OPTIONS  (X'80'=TAKE EXIT)
*                                       OPTIONS  (X'40'=BLANK OUT)
         DC    A(DDOUTS)                EXIT ADDR
         DC    H'5'
DDO2     DC    CL8'OUTDD'
         SPACE
OPER7    OPER  FAST,X'0800'                                         KK6
         SPACE 3
         LTORG
         DC    80X'00'                  ZAP AREA
         EJECT
DCBP     DCB   DSORG=PO,MACRF=R         PATTERN DCB
         EJECT
WORKAREA DSECT
SAVEAREA DS    18F
TAG      DC    CL8'NEWISPF'       WDPSCXL MODULE FLAG               KK6
COPYCPPL DS    4F                 CPPL COPY FOR WDPSCXL             KK6
COPYECT  DS    0D                 CPPL ECT  FOR WDPSCXL             KK6
         DS    XL64               "                                 KK6
L1LS     DS    F
L2LS     DS    F
L3LS     DS    F
L4LS     DS    F
*
SAVESWS  DS    X                  SAVE AREA FOR ECTSWS - KK4  01/31/85
*
FLAG     DS    F
OPFLAG   DS    X
UNKNFLAG DS    X
SAVELEM  DS    F
DDSAVE   DC    CL8' '                   DDN SAVE
TASKDD   DC    CL8' '                   DDN SAVE
DDOUT    DC    CL8' '                   DDN SAVE
DDIN     DC    CL8' '                   DDN SAVE
EPNAME   DC    CL8' '                   EPN SAVE
TCBADD   DC    F'0'
LLIST    LINK  EPLOC=L1LS,SF=L                                      KK6
*
*              I/O RTN PARM AREA:
*
IOPL     DS    4F
PUTLIST  PUTLINE  MF=L
*
STPB     STACK MF=L
*
GTPB     GETLINE MF=L
*
ECB      DS    F
AECB     DS    F
*
*              CMD SCAN PARM AREA:
*
CSPLA    DS    6F
CSOAA    DS    2F
CSOAF    DS    F
*
         EJECT
OPENL    OPEN  (DCBL,INPUT),MF=L
         SPACE 3
DCBL     DCB   DSORG=PO,MACRF=R         EXECUTION DCB
         EJECT
ATTACHL  ATTACH EPLOC=EPNAME,ECB=AECB,SHSPV=78,SF=L
         EJECT
         KPPL
FASTAREA DS    XL600                    SOME AREA TO GIVE KMDPARS
*
WORKLEN  EQU   *-WORKAREA
         EJECT
X        DSECT
         BPPL
         EJECT
MCSPL    IKJCSPL
MCSOA    IKJCSOA
         IKJCVT
         IKJCPPL
         IKJECT
         END
//SYSGO    DD  DSN=&&OBJSET,UNIT=SYSDA,DISP=(,PASS),
//             SPACE=(80,(200,50))
//*
//ASM2     EXEC ASMFC,REGION.ASM=1024K,
//             PARM.ASM='LIST,LOAD,NODECK,NOXREF,RENT'
//ASM.SYSLIB DD
//           DD DSN=&&MACLIB,DISP=OLD
//ASM.SYSPRINT DD  SYSOUT=*,DCB=BLKSIZE=1089
//ASM.SYSIN DD *
*
*   @OK - ADJUSTED TO INVOKE REGISTER EQUATE MACRO SUPPLIED IN
*          FILE270 .MACLIB                       10/26/98
*
*   V1M4- MODIFIED 4/1/86 TO CHECK FOR ZERO LENGTH OPERANDS
*          DUE TO A PROBLEM WITH SMPE AND OUR NEW SUBMIT FRONTEND
*
*              DOCUMENTATION FOR KMDPARS AND OPERSCAN SUBRTNS:
*
*
*
*               PARAMETER LIST PASSED TO KMDPARS:
*
*         KPPL     DS 10F :
*         CBUFPTR  DS    F              CMDBUF TO BE PARSED
*         OPLSTPTR DS    F              LIST OF OPERANDS TO FLAG
*         FLAGPTR  DS    F              WHERE TO FLAG THEM
*         UNKNEXIT DS    F              EXIT TO CALL IF UNKNOWN OPER
*         EXITPARM DS    F              PARAMETER TO PASS EXITS (ADDR)
*                                       (R1 POINTS HERE AT EXIT ENTRY)
*         BPPLPASS DS    F              BPPL PTR WHEN EXIT GETS CONTROL
*         WORKPASS DS    F              OPTIONAL WORKAREA (512 BYTES)
*         KEYLPASS DS    F              LIST OF KEYWORD OPERANDS
*         KEYWPASS DS    F              WORKAREA FOR KEYWORD PROCESSO
*         REEXPASS DS    F              ADDRESS OF REAL UNKNOWN EXIT
*
*
*               PARAMETER LIST PASSED TO OPERSCAN SUBROUTINE:
*
*         BPPL     DS    10F :
*         BUFFPTR  DS    F              BUFFER PTR (CBUF)
*         LENPTR   DS    F              LENGTH PTR (CBUF LEN)
*         STARTPTR DS    F              START SEARCH PTR
*         OPERPTR  DS    F              NEXT OPER LOC
*         OPLENPTR DS    F              NEXT OPER LEN PTR
*         SUBPTR   DS    F              SUBFIELD PTR
*         SUBLENPT DS    F              SUBFIELD LEN PTR
*         WORKPTR  DS    F              OPTIONAL WORKAREA (350 BYTES)
*         OPDESCP  DS    F              PTR TO OPERAND DESCRIPTOR
*                  DS    F              RESERVED (ZERO)
*
*         OPLEN    DS    H              POINTED TO BY OPLENPTR
*         SUBLEN   DS    H              POINTED TO BY SUBLENPT
*
         TITLE 'KMDPARS - GENERAL PURPOSE COMMAND BUFF PARSE SUBRTN'
KMDPARS  CSECT
         SAVE  (14,12),,KMDPARS-V1M4
         LR    R12,R15
         USING KMDPARS,R12
         LR    R11,R1                   R11 -> PARM LIST
         USING KPPL,R11
         SLR   R15,R15                  R15 = 0
         LR    R10,R13                  SAVE SAVE
         L     R13,WORKPASS             SEE IF A FREE ONE THERE
         LTR   R13,R13                  ANY?
         BNZ   FASTCALL                 YES -> SMART CALLER
         LA    R0,WORKLEN               NO -> GET SOME
         GETMAIN R,LV=(0)
         LR    R13,R1                   SAVE IT
         USING WORKAREA,R13
FASTCALL ST    R10,SAVEAREA+4           SAVE HIS SAVE PTR
         LA    R13,SAVEAREA             MAKE SURE MINE IS OK
         USING SAVEAREA,R13
         ST    R13,8(,R10)              GIVE HIM MY SAVE AREA
         EJECT
**********************************************************************
*       MAINLINE PROCESSING CONTROL ROUTINE:                         *
**********************************************************************
*
*              FIRST STEP IS EXTRACT OUR OPERANDS FROM COMMAND:
*
B4       BAL   R14,PB                   FIX BUFFER
*
SETRC    SLR   R15,R15                  R15=0
         CLI   OPFLAG,X'FF'             ANY OPS FOUND?
         BE    KMDEX                    YES -> EXIT
         LA    R15,8                    SAY NONE THERE
*
**********************************************************************
         EJECT
**********************************************************************
*       KMDPARS EXIT ROUTINE:                                        *
**********************************************************************
*
KMDEX    LR    R1,R13                   R1 -> WORKAREA
         LA    R0,WORKLEN               R0 = LENGTH OF AREA
         L     R13,4(,R13)              R13 -> OLD SAVE AREA
         C     R1,WORKPASS              IS WORKAREA MINE?
         BE    FASTEX                   NO -> DONT FREE
         LR    R11,R15                  SAVE RC
         FREEMAIN R,LV=(0),A=(1)        FREE
         LR    R15,R11                  GET RC BACK
FASTEX   RETURN (14,12),RC=(15)         EXIT
         REGEQU                                                     @OK
         EJECT
**********************************************************************
*        PB -  PROCESS BUFFER TO REMOVE AND FLAG OUR OPERANDS
**********************************************************************
PB       ST    R14,L2LS
*
*       FIRST BUILD BUFFER-PROCESSING PARAMETER LIST:
*
BPI      XC    BPPL(40),BPPL            CLEAR BUFFER PROC PARMLIST
         L     R10,CBUFPTR              R10 -> CBUF
         ST    R10,BUFFPTR              BUFFER PTR
         ST    R10,LENPTR               LENGTH PTR
         LH    R9,2(R10)                R9 = OPERAND OFFSET
         LA    R8,4(R9,R10)             R8 -> OPERAND START
         ST    R8,STARTPTR              SEARCH PTR
         LA    R7,OPLEN                 R7 -> OPLEN AREA
         ST    R7,OPLENPTR              LENGTH PTR RETURN AREA
         LA    R6,SUBLEN                R6 -> SUBLEN AREA
         ST    R6,SUBLENPT              LENGTH PTR RETURN AREA
         LA    R5,OPSCNWK               GET HIM A WORKAREA
         ST    R5,WORKPTR               PASS IT TO HIM
         XC    OPFLAG,OPFLAG            CLEAR OPERANDS PRESENT FLAG
*
*       THEN CHECK EACH OPERAND IN THE INPUT BUFFER:
*
DOPO     BAL   R14,PO                   PROCESS AN OPERAND
         LTR   R15,R15                  MORE OPERANDS?
         BZ    DOPO                     YES
*
*       SAY DONE AND EXIT:
*
BPT      SLR   R15,R15                  RC=0
         L     R14,L2LS
         BR    R14
         EJECT
**********************************************************************
*              PO - PROCESS AN OPERAND:
**********************************************************************
PO       ST    R14,L3LS
*
*       FIRST WE OBTAIN AN OPERAND FROM THE BUFFER:
*
         BAL   R14,OO                   OBTAIN AN OPERAND
         LTR   R15,R15                  ANY FOUND?
         BNZ   POX                NO -> NOTHING TO PROCESS
         OI    OPFLAG,X'FF'             SAY WE HAVE OPERANDS
*
*       GET READY TO CHECK THE OPERAND AGAINST OUR LIST:
*
         L     R10,OPERPTR              R10 -> OPERAND IN BUFFER
         LH    R9,OPLEN                 R9  = OPLENGTH IN BUFFER
         L     R8,OPLSTPTR              R8 -> OPLIST (-4)
         LTR   R6,R9                    R6 = R9  = OPLEN         V1M4
         BZ    UNKN                     OPERAND MAY BE IN PARENS V1M4
         BCTR  R6,R0                    R6 = R6-1 FOR EX
         EX    R6,FOLD                  MAKE OPERAND UPPER CASE
*
*       NOW SCAN THE LIST TO SEE IF ANY MATCH BUFFER OPERAND:
*
FOOP     LA    R8,4(R8)                 R8 -> NEXT OP DESC PTR
         L     R7,0(R8)                 R7 -> OPERAND DESCRIPTOR
         LTR   R7,R7                    R7 = 0? ANY MORE TO CHECK?
         BZ    UNKN                     NO
*
         CH    R9,8(,R7)                OPLEN > DESCRIPTOR OPER LEN?
         BH    FOOP                     YES -> TOO LONG TO BE THIS ONE
*
         SLR   R1,R1                    R1=0                      WDPSC
         ICM   R1,1,3(R7)               R1=MINIMUM OPER LEN       WDPSC
         BZ    ANYLEN                   SKIP IF 0                 WDPSC
         CR    R9,R1                    OPLEN>MIN?                WDPSC
         BL    FOOP                     NO->TOO SHORT TO BE THIS  WDPSC
*
ANYLEN   EX    R6,OPCLC                 DOES OPERAND MATCH THIS ONE?
         BNE   FOOP                     NO -> NOT THIS ONE
         EJECT
**********************************************************************
*    WE HAVE AN OPERAND MATCH - SET FLAG AND DO OTHER OPTIONS:
**********************************************************************
FO       SLR   R5,R5
         IC    R5,1(,R7)
         A     R5,FLAGPTR
         OC    0(1,R5),0(R7)            SET FLAG FOR OPERAND FOUND
*
         SLR   R10,R10                  R10=0                     WDPSC
         ICM   R10,7,SUBPTR+1           R10->SUBFIELD?            WDPSC
         BZ    XCHK                     NONE PRESENT              WDPSC
         LH    R2,SUBLEN                R2=SUBFLD LEN             WDPSC
         BCTR  R2,R0                    R2=R2-1 FOR EX            WDPSC
         EX    R2,FOLD                  MAKE UPPER CASE           WDPSC
*
XCHK     TM    2(R7),X'80'              EXIT PRESENT?
         BNO   DO                       NO
         LA    R1,BPPL
         ST    R1,BPPLPASS              GIVE HIM THE BPPL
         ST    R7,OPDESCP               GIVE HIM THE OPER DESCRIPTOR
         LA    R1,EXITPARM              YES -> PASS PARMS
         L     R15,4(,R7)               R15->EXIT
         TM    2(R7),X'10'              SUBEX BEING CALLED?
         BNO   GCX                      NO
         L     R15,0(,R15)
GCX      BALR  R14,R15                  CALL IT
*
DO       TM    2(R7),X'40'              DELETE OPERAND DESIRED?
         BNO   POX                      NO -> EXIT PO
         L     R10,OPERPTR              R10->OPERAND IN BUFFER    WDPSC
         EX    R6,DOMVC                 COVER OPERAND WITH BLANKS
         ICM   R10,7,SUBPTR+1           R10->SUBFIELD             WDPSC
         BZ    POX                      NONE THERE                WDPSC
         EX    R2,DOMVC                 ERASE SUBFIELD            WDPSC
         B     POX
         SPACE 3
**********************************************************************
*    WE HAVE NO OPERAND MATCH - INVOKE EXIT IF PRESENT:
**********************************************************************
UNKN     L     R15,UNKNEXIT             UNKNOWN OPERAND EXIT ADDRESS
         LTR   R15,R15                  EXIT PRESENT?
         BZ    POX                      NO
         LA    R1,BPPL
         ST    R1,BPPLPASS              GIVE HIM THE BPPL
         LA    R1,EXITPARM              YES -> PASS PARMS
         BALR  R14,R15                  CALL IT
         SPACE 3
*
POX      L     R14,L3LS                 EXIT PO
         BR    R14
         SPACE
*
*       OPERATIONS EXECUTED AGAINST THE OPERAND IN THE BUFFER:
*
FOLD     OC    0(0,R10),=CL256' '       FOLD TO UPPER CASE
OPCLC    CLC   0(0,R10),10(R7)          BUFFOP = DESCOP?
DOMVC    MVC   0(0,R10),=CL256' '       MOVE BLANKS ON TOP OF OPERAND
         EJECT
**********************************************************************
*       THE PARSE SUBROUTINE IS IN A SEPARATE MODULE
*   IN ORDER TO MAKE IT GENERAL PURPOSE IN CASE WE NEED OTHER
*   FRONTENDS TO IBM COMMANDS.
**********************************************************************
OO       ST    R14,L4LS
*
         LA    R1,BPPL                  R1 -> BUFFER PROC PARMLIST
         CALL  OPERSCAN                 CALL PARSE ROUTINE
*
         L     R14,L4LS
         BR    R14
         EJECT
STATWORK EQU   *
*
         LTORG
         DC    40X'00'                  ZAP AREA
         TITLE 'WORKAREA FOR KMDPARS'
WORKAREA DSECT
*
SAVEAREA DS    18F
L1LS     DS    F
L2LS     DS    F
L3LS     DS    F
L4LS     DS    F
*
OPFLAG   DS    X
*
         TITLE 'PARAMETER LIST PASSED TO OPERSCAN SUBROUTINE'
*
         BPPL
*
OPSCNWK  DS    XL350                    WORK AREA TO PASS TO OPERSCAN
*
WORKLEN  EQU   *-WORKAREA
         TITLE 'PARAMETER LIST PASSED TO KMDPARS'
*
         KPPL  DSECT
*
         END
//SYSGO    DD  DSN=&&OBJSET,DISP=(MOD,PASS)
//*
//ASM3     EXEC ASMFC,REGION.ASM=1024K,
//             PARM.ASM='LIST,LOAD,NODECK,NOXREF,RENT'
//ASM.SYSLIB DD
//           DD DSN=&&MACLIB,DISP=OLD
//ASM.SYSPRINT DD SYSOUT=*,DCB=BLKSIZE=1089
//ASM.SYSIN DD *
*
*   @OK - ADJUSTED TO INVOKE REGISTER EQUATE MACRO SUPPLIED IN
*          FILE270 .MACLIB                        10/26/98
*
*          DATA SET 761KKOPERS AT LEVEL 002 AS OF 02/13/80
*
* WRITTEN BY KERMIT KISER - WASHINGTON STATE DP SERVICE CENTER (WDPSC)
*
*
*              DOCUMENTATION FOR KMDPARS AND OPERSCAN SUBRTNS:
*
*
*
*               PARAMETER LIST PASSED TO KMDPARS:
*
*         KPPL     DS 10F :
*         CBUFPTR  DS    F              CMDBUF TO BE PARSED
*         OPLSTPTR DS    F              LIST OF OPERANDS TO FLAG
*         FLAGPTR  DS    F              WHERE TO FLAG THEM
*         UNKNEXIT DS    F              EXIT TO CALL IF UNKNOWN OPER
*         EXITPARM DS    F              PARAMETER TO PASS EXITS (ADDR)
*                                       (R1 POINTS HERE AT EXIT ENTRY)
*         BPPLPASS DS    F              BPPL PTR WHEN EXIT GETS CONTROL
*         WORKPASS DS    F              OPTIONAL WORKAREA (512 BYTES)
*                  DS    3F             RESERVED
*
*
*               PARAMETER LIST PASSED TO OPERSCAN SUBROUTINE:
*
*         BPPL     DS    10F :
*         BUFFPTR  DS    F              BUFFER PTR (CBUF)
*         LENPTR   DS    F              LENGTH PTR (CBUF LEN)
*         STARTPTR DS    F              START SEARCH PTR
*         OPERPTR  DS    F              NEXT OPER LOC
*         OPLENPTR DS    F              NEXT OPER LEN PTR
*         SUBPTR   DS    F              SUBFIELD PTR
*         SUBLENPT DS    F              SUBFIELD LEN PTR
*         WORKPTR  DS    F              OPTIONAL WORKAREA (350 BYTES)
*                  DS    2F             RESERVED
*
*         OPLEN    DS    H              POINTED TO BY OPLENPTR
*         SUBLEN   DS    H              POINTED TO BY SUBLENPT
*
*          DATA SET 761KKOPSCN AT LEVEL 001 AS OF 04/27/79
         TITLE 'OPERSCAN - PARSE ROUTINE FOR INTERCEPT COMMANDS'
OPERSCAN CSECT
         SAVE  (14,12),,*
         LR    R12,R15
         USING OPERSCAN,R12
         LR    R11,R1                   R11 -> PARM LIST
         USING BPPL,R11
         SLR   R15,R15                  R15 = 0
         LR    R10,R13                  SAVE SAVE
         L     R13,WORKPTR              SEE IF A FREE ONE THERE
         LTR   R13,R13                  ANY?
         BNZ   FASTCALL                 YES -> SMART CALLER
         LA    R0,WORKLEN               NO -> GET SOME
         GETMAIN R,LV=(0)
         LR    R13,R1                   SAVE IT
         USING WORKAREA,R13
FASTCALL ST    R10,SAVEAREA+4           SAVE HIS SAVE PTR
         LA    R13,SAVEAREA             MAKE SURE MINE IS OK
         USING SAVEAREA,R13
         ST    R13,8(,R10)              GIVE HIM MY SAVE AREA
*
         EJECT
         BAL   R14,VALCHECK             CAN WE LOOK FOR AN OPERAND?
         LTR   R15,R15                  R15 STILL 0?
         BNZ   OPEREX                   NO CAN DO
*
         BAL   R14,FINDOPER             LOOK FOR OPERAND
         LTR   R15,R15                  R15 STILL 0?
         BNZ   OPEREX                   NO CAN DO
*
         BAL   R14,FINDEND              FIND THE END OF THE OPERAND
         EJECT
OPEREX   LR    R1,R13                   R1 -> WORKAREA
         LA    R0,WORKLEN               R0 = LENGTH OF AREA
         L     R13,4(,R13)              R13 -> OLD SAVE AREA
         C     R1,WORKPTR               IS WORKAREA MINE?
         BE    FASTEX                   NO -> DONT FREE
         LR    R11,R15                  SAVE RC
         FREEMAIN R,LV=(0),A=(1)        FREE
         LR    R15,R11                  GET RC BACK
FASTEX   RETURN (14,12),RC=(15)         EXIT OPERSCAN
         REGEQU                                                     @OK
         EJECT
VALCHECK ST    R14,L1LS
*
         ST    R15,OPERPTR              NEXT OPER PTR = 0
         ST    R15,SUBPTR               SUBFLD PTR = 0
*
         L     R10,BUFFPTR              R10 -> BUFFER
         L     R9,LENPTR                R9 ->  BUFF LEN
         AH    R10,0(R9)                R10 -> BUFFEND+1
         S     R10,STARTPTR             R10 = LEN TO SCAN
         BP    SAVELEN                  SAVE IF > 0
*
         LA    R15,4                    RC = 4 -> NO OPER FOUND
         BR    R14
*
SAVELEN  ST    R10,SCANLEN              SAVE LENGTH TO SCAN
         BR    R14                      EXIT
         EJECT
FINDOPER ST    R14,L1LS
*
         SLR   R1,R1                    R1=0
*
         L     R10,STARTPTR             R10 -> START OF SEARCH
         L     R9,SCANLEN               R9 = LEN TO SEARCH
         BCTR  R9,R0                    R9 = R9-1 FOR EX
         EX    R9,TRT1                  LOOK FOR AN OPERAND
         BZ    NOOPER                   NONE FOUND?
*              WE HAVE ONE! ; POINT TO  IT:
         ST    R1,OPERPTR               RETURN OPER LOCATION
         L     R8,BUFFPTR               R8 -> BUFF
         L     R7,LENPTR                R7 -> BUFF LEN
         AH    R8,0(R7)                 R8 -> BUFFEND+1
         SR    R8,R1                    R8 = NEW SCAN LENGTH
         ST    R8,SCANLEN               SAVE IT
         BR    R14
NOOPER   LA    R15,4
         L     R14,L1LS
         BR    R14
TRT1     TRT   0(0,R10),TRANTAB1        EXECUTED SCAN
         TITLE 'OPERSCAN    - FINDEND ROUTINES'
FINDEND  DS    0H
         ST    R14,L1LS           SAVE R14
*
FINDINIT L     R9,OPERPTR         R9 -> START OF OPER
         A     R9,SCANLEN         R9 -> END OF BUFF +1
         LR    R8,R9              R8 -> "
         BCTR  R8,R0              R8 = R8-1
         L     R7,OPERPTR         R7 -> OPER START
         LA    R5,NORMODE         R5 -> NORMMODE
         SLR   R4,R4              R4 = 0 = PAREN DEPTH
         L     R3,SCANLEN         R3 =  LEN TO SCAN
         BCTR  R3,R0              R3=LENGTH FOR EXECUTE
         SLR   R2,R2              R2=0
         SLR   R1,R1              R1=0
         L     R15,=F'-1'         R15 = -1
         MVC   TRTTABLE,TT
*
SCANLOOP DS    0H
         BAL   R14,SCANLINE       LINK TO PARSE SCAN
         LTR   R15,R15            R15=0?
         BM    SCANLOOP           CONTINUE IF NEGATIVE
*
FINDEXIT L     R14,L1LS           RESTORE R14
         BR    R14                EXIT
*
*
SCANLINE DS    0H
*
TRTEX    EX    R3,TRT             EXECUTE SCAN
         BZ    0(,R5)             END OF LINE - NOTHING FOUND
         BC    4,0(R2,R5)         NOT END OF LINE - SOMETHING FOUND
         BC    2,4(R2,R5)         END OF LINE - SOMETHING FOUND
         DS    0F
TRT      TRT   0(0,R7),TRTTABLE   TRANSLATE INSTRUCTION (EXECUTED)
         EJECT
         DS    0F
TT       DC    256X'0'            TRANS TABLE PATTERN
         ORG   TT+107             COMMA
         DC    X'04'
         ORG   TT+77              LEFT PAREN
         DC    X'0C'
         ORG   TT+93              RIGHT PAREN
         DC    X'14'
         ORG   TT+125             QUOTE
         DC    X'1C'
         ORG   TT+64              BLANK
         DC    X'24'
         ORG
*
*   BRANCH TABLES
*
NORMODE  DS    0H
         B     NORMEND                       END OF LINE
         B     NORMCOM            COMMA
         B     NMCOMEL            COMMA      END OF LINE
         B     NORMLP             LEFT PAREN
         B     NMLPEL             LEFT PAREN END OF LINE
         B     NORMRP             RITE PAREN
         B     NMRPEL             RITE PAREN END OF LINE
         B     NORMQ              QUOTE
         B     NORMQEL            QUOTE      END OF LINE
         B     NORMB              BLANK
         B     NORMBEL            BLANK      END OF LINE
*
PARNMODE DS    0H
         B     PMEND                       END OF LINE
         B     PMCOM              COMMA
         B     PMCOMEL            COMMA      END OF LINE
         B     PMLP               LEFT PAREN
         B     PMLPEL             LEFT PAREN END OF LINE
         B     PMRP               RITE PAREN
         B     PMRPEL             RITE PAREN END OF LINE
         B     PMQ                QUOTE
         B     PMQEL              QUOTE      END OF LINE
         B     PMB                BLANK
         B     PMBEL              BLANK      END OF LINE
*
QMODE    DS    0H
         B     QMEND                       END OF LINE
         B     QMQ                QUOTE
         B     QMQEL              QUOTE      END OF LINE
         EJECT
*
*   COMMON SCAN ROUTINES
*
*
COMSEX1  DS    0H                 COMMON SCAN EXIT
         LA    R7,1(,R1)          R7 -> NEXT SCAN POSITION
         LR    R3,R8              R3 -> END OF LINE
         SR    R3,R7              R3 =  EX LENGTH FOR SCAN
         BM    SCANERR            ERROR IF NO LEN
         BR    R14                EXIT SCANLINE
*
STORLEN1 EQU   *                  STORE OPER LENGTH
         ST    R1,STARTPTR        SAVE DELIM PTR
         S     R1,OPERPTR         R1 = OPERLEN
         L     R10,OPLENPTR       R10 -> OPER LEN FIELD
         STH   R1,0(R10)          SAVE IT
         BR    R14
*
STORLEN2 EQU   *                  STORE SUBFIELD LENGTH
         ST    R1,STARTPTR        SAVE DELIM PTR
         S     R1,SUBPTR          R1 = SUBFLD LENGTH
         L     R10,SUBLENPT       R10 -> SUBLEN FIELD
         STH   R1,0(R10)          SAVE LEN
         BR    R14
*
*
SCANERR  EQU   *
         LR    R1,R9              R1 -> BUFFEND+1
         LTR   R4,R4              IN PARENS?
         BZ    SENM               NO
         LA    R15,12             RC=12 ERROR
         B     STORLEN2
SENM     LA    R15,8              RC= 8 ERROR
         B     STORLEN1
         EJECT
NORMEND  EQU   *
         LR    R1,R9              R1 -> BUFFEND+1
         SLR   R15,R15            RC=0 -> SCAN COMPLETED
         B     STORLEN1           EXIT
*
NMCOMEL  EQU   *                  NORMAL MODE,COMMA,EOL
NORMCOM  EQU   *                  NORMAL MODE,COMMA
         SLR   R15,R15            RC=0 -> SCAN COMPLETED
         B     STORLEN1           EXIT
*
NMLPEL   B     NORMRP             NORMAL MODE,LEFT PAREN,EOL
NORMLP   DS    0H                 NORMAL MODE,LEFT PAREN
         LA    R5,PARNMODE        R5 -> PAREN MODE (MODE SWITCH)
         LA    R4,1(,R4)          R4 = R4+1 (PAREN DEPTH = 1)
         ST    R1,SUBPTR          SAVE START OF SUBFIELD
         LR    R7,R1              R7 -> LEFT PAREN
         S     R7,OPERPTR         R7 = OPERAND LENGTH
         L     R10,OPLENPTR       R10 -> OPER LEN AREA
         STH   R7,0(R10)          SAVE OPER LENGTH
         B     COMSEX1            GO TO COMMON EXIT
*
NMRPEL   EQU   *                  NORMAL MODE,RITE PAREN,EOL
NORMRP   EQU   *                  NORMAL MODE,RITE PAREN
         SLR   R15,R15            RC=0 -> SCAN COMPLETE
         B     STORLEN1           EXIT
*
NORMQEL  EQU   SCANERR            NORMAL MODE,QUOTE,EOL
NORMQ    DS    0H                 NORMAL MODE,QUOTE
         LA    R5,QMODE           CHANGE TO QUOTE MODE
         XC    TRTTABLE,TRTTABLE  CLEAR TRANSLATE TABLE
         MVI   TRTTABLE+125,X'04' QUOTE POSITION IN TABLE
         B     COMSEX1            GO TO COMMON EXIT
*
NORMBEL  EQU   NORMCOM            NORMAL MODE,BLANK,EOL
NORMB    EQU   NORMCOM            NORMAL MODE,BLANK
         EJECT
*
PMEND    EQU   *                  PAREN MODE,EOL (NO VALID DELIM)
         LR    R1,R9              R1 -> BUFFEND+1
         SLR   R15,R15            RC=0 -> SCAN COMPLETED
         B     STORLEN2           EXIT
*
PMCOMEL  EQU   PMEND              PAREN MODE,COMMA,EOL
PMCOM    EQU   COMSEX1            PAREN MODE,COMMA
*
PMLPEL   EQU   PMEND              PAREN MODE,LEFT PAREN,EOL
PMLP     DS    0H                 PAREN MODE,LEFT PAREN
         LA    R4,1(,R4)          R4=R4+1 (PAREN DEPTH)
         B     COMSEX1            GO TO COMMON EXIT
*
PMRPEL   EQU   PMEND              PAREN MODE,RITE PAREN,EOL
PMRP     EQU   *                  PAREN MODE,RITE PAREN
         BCT   R4,COMSEX1         R4=R4-1 (EXIT IF STILL NESTED)
         SLR   R15,R15            RC=0 -> SCAN COMPLETE FLAG
         LA    R1,1(R1)           R1=R1+1 POINT PAST DELIMITER
         B     STORLEN2           GO TO SAVE SUBFLD LEN
*
PMQEL    EQU   SCANERR            PAREN MODE,QUOTE,EOL
PMQ      EQU   NORMQ              PAREN MODE,QUOTE
*
PMBEL    EQU   PMEND              PAREN MODE,BLANK,EOL
PMB      EQU   COMSEX1            PAREN MODE,BLANK
*
QMEND    EQU   SCANERR            QUOTE MODE,EOL (INVALID DELIM)
*
QMQ      DS    0H                 QUOTE MODE,QUOTE
         CLI   1(R1),C''''        DOUBLE QUOTE?
         BE    IGQ                YES ->SKIP IT
         MVC   TRTTABLE,TT        NO -> RESET MODE
         LTR   R4,R4              R4=0? (MODE=NORM)
         BZ    QNS                YES -> QUOTE TO NORM SWITCH
         LA    R5,PARNMODE        NO -> PAREN MODE SWITCH
         B     COMSEX1            GO TO COMMON EXIT
QNS      LA    R5,NORMODE         NORMAL MODE SWITCH
         B     COMSEX1            GO TO COMMON EXIT
IGQ      LA    R1,1(,R1)          R1=R1+1 (IGNORE QUOTES)
         B     COMSEX1            GO TO COMMON EXIT
*
QMQEL    EQU   *                  QUOTE MODE,QUOTE,EOL
         LTR   R4,R4              IN PARENS?
         BZ    NORMEND            NO
         B     PMEND              YES
         TITLE 'OPERSCAN    - WORKAREA AND CONSTANTS'
STATWORK EQU   *
*
TRANTAB1 DC    256X'04'                 OPER SCAN TABLE
         ORG   TRANTAB1+C' '            BLANK
         DC    X'00'
         ORG   TRANTAB1+C','            COMMA
         DC    X'00'
         ORG   TRANTAB1+C')'            RIGHT PAREN
         DC    X'00'
         ORG
         LTORG
         DC    80X'00'                  ZAP AREA
         EJECT
WORKAREA DSECT
SAVEAREA DS    18F
L1LS     DS    F
L2LS     DS    F
L3LS     DS    F
L4LS     DS    F
SCANLEN  DS    F
*
TRTTABLE DS    XL256
WORKLEN  EQU   *-WORKAREA
         TITLE 'PARAMETER LIST PASSED TO OPERSCAN SUBROUTINE'
BPPL     DSECT
BUFFPTR  DS    F                        BUFFER PTR
LENPTR   DS    F                        LENGTH PTR
STARTPTR DS    F                        START SEARCH PTR
OPERPTR  DS    F                        NEXT OPER LOC
OPLENPTR DS    F                        NEXT OPER LEN PTR
SUBPTR   DS    F                        SUBFIELD PTR
SUBLENPT DS    F                        SUBFIELD LEN PTF
WORKPTR  DS    F                        WORKAREA MAY BE PASSED HERE
         DS    F
         DS    F
         END
//SYSGO    DD  DSN=&&OBJSET,DISP=(MOD,PASS)
//*
//LKED     EXEC PGM=IEWL,REGION=512K,COND=(0,NE),
//             PARM='XREF,LET,LIST,NCAL,RENT'
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  DSN=&&OBJSET,DISP=(OLD,PASS)
//         DD  *
  ORDER STACK,KMDPARS,OPERSCAN
  ENTRY STACK
  NAME STACK(R)
//SYSLMOD  DD  DSN=SYS2.CMDLIB,DISP=MOD          <= TARGET LIBRARY
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(300,20))
//*
//HELP     EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR            <= TARGET LIBRARY
//SYSIN    DD  *
./ ADD NAME=STACK
)F FUNCTION -

     STACK is a TSO COMMAND PROCESSOR which enables interception
 of PUTGET IO for the duration of the TSO command following
 STACK.  Thus the output from a TSO command may be directed to a
 dataset.  STACK imitates the TMP to obtain the next TSO command
 and attach it with the IO redirected via the STACK macro.  STACK
 can be used to execute a command from a specific library (for
 example a test version) or to call a program using dynamic
 linkage since the optional TASKDD is also used as the tasklib
 for the duration of the command.  VIO datasets may be used for
 the IO files. All operands are optional.  All file I/O must
 be done via PUTGET module to be intercepted.

)X SYNTAX  -

        STACK DDIN(INPUTDD) DDOUT(OUTDD) -
              TASKLIB(TASKDD) LIST

  REQUIRED - none
  ALIAS    - none

)O OPERANDS -
        INPUTDD - DDNAME to read command from instead of normal
                  source (TSO terminal)
        OUTDD   - DDNAME the command output should go to instead
                  of the TSO terminal
        TASKDD  - DDNAME the command should be attached from
        LIST    - causes the command to be written to the output
                  file in front of output from command
//*
//
