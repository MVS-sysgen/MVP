//DUDASD  JOB (TSO),
//             'Install DUDASD',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*-----------------------------------------------------------------*
//*       INSTALL OF THE 'DUDASD' TSO CMD                           *
//*-----------------------------------------------------------------*
//DUDASD   EXEC ASMFCL,
//         PARM.ASM='NOLIST,ALIGN,OBJECT,TERM',
//         PARM.LKED='LIST,LET,MAP,XREF,RENT',MAC1='SYS1.AMODGEN'
//ASM.SYSPRINT DD SYSOUT=*
//ASM.SYSTERM DD SYSOUT=*
//ASM.SYSIN DD *
         MACRO , JVDENTER - V1M00 - 01/28/74
&LABEL   JVDENTER                                                      +
               &BASE=(11),                                             +
               &REGS=(14,12),                                          +
               &ID=,                                                   +
               &VERSION=,                                              +
               &DSA=72,                                                +
               &ORIGIN=*,                                              +
               &CFL=YES
.*
         LCLC  &EPID,&BASR,&START
.*
.BEGIN   AIF   ('&LABEL' EQ '').IDT
.*
         SPACE
&LABEL   DS    0H
         ENTRY &LABEL
         SPACE
.*
.IDT     AIF   (T'&ID EQ 'O').IDD
&EPID    SETC  '&ID'
         AGO   .VER
.IDD     AIF   ('&LABEL' EQ '').IDC
&EPID    SETC  '&LABEL'
         AGO   .VER
.IDC     ANOP
&EPID    SETC  '&SYSECT'
.VER     AIF   (T'&VERSION EQ 'O').GEN
&EPID    SETC  '&EPID-&VERSION-&SYSDATE-&SYSTIME'
         MNOTE *,'       &EPID'
.*
.GEN     ANOP
&BASR    SETC  '&BASE(1)'
.*
&START   SETC  '&ORIGIN'
         AIF   ('&ORIGIN' NE '@').START
&START   SETC  '&SYSECT'
.START   AIF   ('&ORIGIN' EQ '*').HERE
.*
         USING &START,&BASR
         SAVE  &REGS,,&EPID
         BALR  &BASR,0
         LA    14,*-&ORIGIN
         SLR   &BASR,14
         LR    2,13
         SPACE
.*
         AGO   .GETDSA
.HERE    ANOP
.*
         USING &START,&BASR
         SAVE  &REGS,,&EPID
         LR    &BASR,15
         LR    2,13
         SPACE
.*
.GETDSA  AIF   ('&DSA' EQ '0').NOCFL
.*
         LA    3,&DSA
         GETMAIN R,LV=(3)
         ST    3,0(1)
         ST    13,4(1)
         LR    13,1
.*
         AIF   ('&CFL' EQ 'NO').NOCFL
.*
         ST    1,8(2)
.*
.NOCFL   ANOP
.*
         LM    14,04,12(2)
         SPACE
.*
         MEND
         MACRO , JVDLEAVE - V1M00 - 01/28/74
&LABEL   JVDLEAVE                                                      +
               &REGS=(14,12),                                          +
               &RC=,                                                   +
               &DSA=
.*
         LCLC  &L
.*
         AIF   ('&DSA' EQ '0').NODSA
.*
&LABEL   LH    0,2(13)
         LR    1,13
         L     13,4(13)
.*
         AIF   ('&RC' NE '(15)').FREE
.*
         LR    2,15
.*
.FREE    ANOP
.*
         FREEMAIN R,LV=(0),A=(1)
.*
.RCCHK   AIF   ('&RC' EQ '').NORC
         AIF   ('&RC' NE '(15)').RTRN
.*
         LR    15,2
.*
         AGO   .RTRNN
.RTRN    AIF   ('&RC'(1,1) NE '(').RTRNC
.*
         LR    15,&RC(1)
.*
.RTRNN   ANOP
.*
&L       RETURN &REGS,RC=(15)
         SPACE
.*
         MEXIT
.RTRNC   ANOP
.*
&L       RETURN &REGS,RC=&RC
         SPACE
.*
         MEXIT
.NORC    ANOP
.*
&L       RETURN &REGS
         SPACE
.*
         MEXIT
.NODSA   ANOP
&L       SETC  '&LABEL'
         AGO   .RCCHK
.*
         MEND
         MACRO
&L       JVDEQU &O
*
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
         SPACE
.*
         MEND
 TITLE 'D U D A S D R  ---   D A S D   V S N   D I S P L A Y'           00000100
DUDASDR  CSECT                                                          00000200
         SPACE                                                          00000300
         USING    DSAWRK,R13                                            00000400
         USING    CVTMAP,R12                                            00000500
         USING    CPPL,R1                                               00000600
         USING    IOPL,R2                                               00000700
         USING    UCBOB,R10                                             00000800
         SPACE                                                          00000900
         JVDENTER DSA=(DSAEND-DSAWRK),BASE=(R11),VERSION=&SYSPARM       00001000
         SPACE                                                          00001100
         LA       R2,IOPLIST                                            00001200
         MVC      IOPLECT,CPPLECT                                       00001300
         MVC      IOPLUPT,CPPLUPT                                       00001400
         XC       ECB,ECB                                               00001500
         XC       PREFIX(4),PREFIX                                      00001600
         LA       R15,L'BUFFER+4                                        00001700
         STH      R15,PREFIX                                            00001800
         L        R12,CVTPTR                                            00001900
         L        R7,CVTILK2                                            00002000
         XR       R10,R10                                               00002100
         EJECT                                                          00002200
DULINE   EQU   *                                                        00002300
         SPACE                                                          00002400
         MVI   BUFFER,C' '                                              00002500
         MVC   BUFFER+1(L'BUFFER-1),BUFFER                              00002600
         LA    R5,BUFFER                                                00002700
         LA    R4,L'BUFFER/L'ELEMENT                                    00002800
         USING ELEMENT,R5                                               00002900
         SPACE                                                          00003000
DUUNIT   EQU   *                                                        00003100
         CLI   0(R7),X'FF'                                              00003200
         BE    PUT                                                      00003300
         LH    R15,0(R7)                                                00003400
         LA    R7,2(R7)                                                 00003500
         CR    R15,R10                                                  00003600
         BNH   DUUNIT                                                   00003700
         LR    R10,R15                                                  00003800
         CLI   UCBTYP+2,X'20'                                           00003900
         BNE   DUUNIT                                                   00004000
         CLC   LASTCUU,UCBNAME                                          00004100
         BE    DUUNIT                                                   00004200
         MVC   ELEUNIT,UCBNAME                                          00004300
         MVC   LASTCUU,UCBNAME                                          00004400
         MVC   ELEVSN,SRTEVOLI                                          00004500
         TM    SRTESTAT,UCBONLI                                         00004600
         BO    ONLINE                                                   00004700
         MVC   ELEVSN,=CL6'(OFF)'                                       00004800
         B     ULOOP                                                    00004900
ONLINE   MVI   ELEDASH,C'-'                                             00005000
         MVI   ELESTAT,C'O'                                             00005100
         TM    SRTESTAT,UCBALOC                                         00005200
         BNO   NALLOC                                                   00005300
         MVI   ELESTAT,C'A'                                             00005400
NALLOC   TM    SRTESTAT,UCBCHGS                                         00005500
         BNO   RESERV                                                   00005600
         MVI   ELESTAT,C'P'                                             00005700
RESERV   CLI   UCBSQC,0                                                 00005800
         BE    MOUNT                                                    00005900
         MVI   ELERSRV,C'R'                                             00006000
MOUNT    TM    SRTESTAT,UCBRESV        IS IT RESERVED(MOUNT)            00006100
         BZ    RESIDENT                NO - CHECK RESIDENT              00006200
         MVC   ELEMSTAT(3),=C'RSV'     SET C'RSV' IN LINE               00006300
         B     ULOOP                   CONTINUE TO NEXT UCB             00006400
RESIDENT TM    SRTESTAT,UCBPRES        IS IT RESIDENT                   00006500
         BZ    ULOOP                   NO - CONTINUE TO NEXT UCB        00006600
         MVC   ELEMSTAT(3),=C'RSD'     SET C'RSD' IN LINE               00006700
ULOOP    LA    R5,ELENEXT                                               00006800
         BCT   R4,DUUNIT                                                00006900
         SPACE                                                          00007000
PUT      CLI   BUFFER,C' '                                              00007100
         BE    DONE                                                     00007200
         SPACE                                                          00007300
         PUTLINE                                                       +00007400
               PARM=PUTLINE,MF=(E,IOPLIST),ECB=ECB,                    +00007500
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),                     +00007600
               OUTPUT=(PREFIX,TERM,SINGLE,DATA)                         00007700
         SPACE                                                          00007800
         B     DULINE                                                   00007900
         EJECT                                                          00008000
DONE     JVDLEAVE                                                       00008100
         EJECT                                                          00008200
         LTORG                                                          00008300
         SPACE                                                          00008400
ELEWRK   DSECT                                                          00008500
ELEMENT  DS    0CL20                                                    00008600
ELEUNIT  DS    CL3                                                      00008700
         DS    CL1                                                      00008800
ELEVSN   DS    CL6                                                      00008900
ELEDASH  DS    CL1                                                      00009000
ELESTAT  DS    CL1                                                      00009100
ELERSRV  DS    CL1                                                      00009200
ELEMSTAT DS    CL3                                                      00009300
         DS    CL2                                                      00009400
ELENEXT  DS    0CL1                                                     00009500
         SPACE                                                          00009600
DSAWRK   DSECT                                                          00009700
SSA      DS    18F                                                      00009800
IOPLIST  DS    07F                                                      00009900
ECB      DS    01F                                                      00010000
PREFIX   DS    2H                                                       00010100
BUFFER   DS    CL(L'ELEMENT*4)                                          00010200
LASTCUU  DS    CL3                                                      00010300
PUTLINE  PUTLINE MF=L                                                   00010400
DSAEND   DS    0D                                                       00010500
         EJECT                                                          00010600
         IKJIOPL                                                        00010700
         EJECT                                                          00010800
         IKJCPPL                                                        00010900
         EJECT                                                          00011000
UCBWRK   DSECT                                                          00011100
         IEFUCBOB                                                       00011200
         SPACE                                                          00011300
         JVDEQU                                                         00011400
         EJECT                                                          00011500
         CVT                                                            00011600
         END   DUDASDR                                                  00011700
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR
//LKED.SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  ENTRY DUDASDR
  NAME DUDASD(R)
/*
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP
//SYSIN    DD  *
./ ADD NAME=DUDASD
)F FUNCTION -                                                           00000010
   THE DUDASD COMMAND DISPLAYS ALL DASD DEVICES                         00000020
   DEFINED IN THE SYSTEM, WITH THE FOLLOWING DATA :                     00000030
     -  VOLUME ID, IF ONLINE                                            00000040
     -  ALLOCATION STATUS, AS FOLLOWS :                                 00000050
        -  O - UNALLOCATED                                              00000060
        -  A - ALLOCATED                                                00000070
        -  P - OFFLINE PENDING                                          00000080
     -  RESERVED INDICATION, IF THE TSO SYSTEM HAS                      00000090
        SUCCESSFULLY RESERVED THE DEVICE                                00000100
     -  VOLUME STATUS                                                   00000110
        -  RSV - RESERVED ON THE SYSTEM                                 00000120
        -  RSD - SYSTEM REDIDENT                                        00000130
)X SYNTAX -                                                             00000140
         DUDASD                                                         00000150
)O OPERANDS -                                                           00000160
  THERE ARE NO OPERANDS ON THE DUDASD COMMAND                           00000170

./ ENDUP
//
