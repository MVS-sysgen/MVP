//VTOC  JOB (TSO),
//             'Install VTOC',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00000200
//* SOURCE: CBT (V129) FILE #112                                        00000300
//* TARGET: SYS2.CMDLIB   SYS2.HELP                                     00000400
//*                                                                     00000500
//********************************************************************* 00000600
//* This job installs the VTOC TSO command and help.                  * 00000700
//********************************************************************* 00000800
//*                                                                     00000900
//INSTALL PROC SOUT='*',               <=== SYSOUT CLASS                00001000
//             LIB='SYS2.CMDLIB',      <=== TARGET LOAD LIBRARY         00001100
//             HELP='SYS2.HELP',       <=== HELP LIBRARY                00001200
//             SYSTS=SYSDA,            <=== UNITNAME FOR WORK DATASETS  00001300
//             ASMBLR=IFOX00,          <=== NAME OF YOUR ASSEMBLER      00001400
//             ALIB='SYSC.LINKLIB',    <=== LOCATION OF YOUR ASSEMBLER  00001500
//             MACLIB='SYS1.MACLIB',   <=== MACLIB DATASET NAME         00001600
//             AMODGEN='SYS1.AMODGEN'  <=== AMODGEN DATASET NAME        00001700
//*                                                                     00001800
//LOADMACS EXEC PGM=IEBUPDTE,PARM=NEW                                   00001900
//SYSPRINT DD  SYSOUT=&SOUT                                             00002000
//SYSUT2   DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(,PASS),                   00002100
//             SPACE=(TRK,(120,,5),RLSE),DCB=(SYS1.MACLIB)              00002200
//*                                                                     00002300
//IEBUPDTE EXEC PGM=IEBUPDTE,PARM=NEW                                   00002400
//SYSPRINT DD  SYSOUT=&SOUT                                             00002500
//SYSUT1   DD  DSN=&HELP,DISP=SHR                                       00002600
//SYSUT2   DD  DSN=&HELP,DISP=SHR                                       00002700
//*                                                                     00002800
//ASM1    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00002900
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00003000
//SYSTERM  DD  SYSOUT=&SOUT                                             00003100
//SYSPRINT DD  SYSOUT=&SOUT                                             00003200
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00003300
//         DD  DSN=&AMODGEN,DISP=SHR                                    00003400
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00003500
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003600
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003700
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003800
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(,PASS,DELETE),            00003900
//             SPACE=(TRK,(30,15))                                      00004000
//*                                                                     00004100
//ASM2    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00004200
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00004300
//SYSTERM  DD  SYSOUT=&SOUT                                             00004400
//SYSPRINT DD  SYSOUT=&SOUT                                             00004500
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00004600
//         DD  DSN=&AMODGEN,DISP=SHR                                    00004700
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00004800
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00004900
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00005000
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00005100
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00005200
//*                                                                     00005300
//ASM3    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00005400
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00005500
//SYSTERM  DD  SYSOUT=&SOUT                                             00005600
//SYSPRINT DD  SYSOUT=&SOUT                                             00005700
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00005800
//         DD  DSN=&AMODGEN,DISP=SHR                                    00005900
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00006000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00006100
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00006200
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00006300
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00006400
//*                                                                     00006500
//ASM4    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00006600
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00006700
//SYSTERM  DD  SYSOUT=&SOUT                                             00006800
//SYSPRINT DD  SYSOUT=&SOUT                                             00006900
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00007000
//         DD  DSN=&AMODGEN,DISP=SHR                                    00007100
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00007200
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00007300
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00007400
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00007500
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00007600
//*                                                                     00007700
//ASM5    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00007800
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00007900
//SYSTERM  DD  SYSOUT=&SOUT                                             00008000
//SYSPRINT DD  SYSOUT=&SOUT                                             00008100
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00008200
//         DD  DSN=&AMODGEN,DISP=SHR                                    00008300
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00008400
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00008500
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00008600
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00008700
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00008800
//*                                                                     00008900
//ASM6    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00009000
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00009100
//SYSTERM  DD  SYSOUT=&SOUT                                             00009200
//SYSPRINT DD  SYSOUT=&SOUT                                             00009300
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00009400
//         DD  DSN=&AMODGEN,DISP=SHR                                    00009500
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00009600
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00009700
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00009800
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00009900
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00010000
//*                                                                     00010100
//ASM7    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00010200
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00010300
//SYSTERM  DD  SYSOUT=&SOUT                                             00010400
//SYSPRINT DD  SYSOUT=&SOUT                                             00010500
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00010600
//         DD  DSN=&AMODGEN,DISP=SHR                                    00010700
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00010800
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00010900
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00011000
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00011100
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00011200
//*                                                                     00011300
//LKED    EXEC PGM=IEWL,COND=(0,NE),                                    00011400
//             PARM='XREF,LET,LIST,SIZE=(600K,64K)'                     00011500
//SYSPRINT DD  SYSOUT=&SOUT                                             00011600
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,10)                               00011700
//SYSLMOD  DD  DSN=&LIB,DISP=SHR                                        00011800
//SYSLIN   DD  DSN=&&SYSLIN,DISP=(OLD,DELETE)                           00011900
//         DD  DDNAME=SYSIN                                             00012000
//        PEND                                                          00012100
//*                                                                     00012200
//        EXEC INSTALL                                                  00012300
//*                                                                     00012400
//LOADMACS.SYSIN DD *                                                   00012500
./       ADD   NAME=ALLOC                                               00012600
         MACRO                                                          00012700
&NAME    ALLOC &DUMMY,&PERM,&DSN=,&DDN=,&DDNRET=,&MEMBER=,&DISP=,      X00012800
               &VOL=,&UNIT=,&SYSOUT=,&FREE=,&COPIES=,&LABEL=,          X00012900
               &BLKSIZE=,&DEN=,&DSORG=,&KEYLEN=,&LRECL=,&RECFM=,       X00013000
               &PASWORD=,&DSNRET=,&MF=AUTO,&PREFIX=,&ERROR=,           X00013100
               &SPACE=,&F=,&FILE=,&DA=,&QNAME=,&DSORGRT=,              X00013200
               &VOLRET=,&DCBDSN=,&DCBDDN=,&SPECIAL=,&DDNTO=,           X00013300
               &FORMS=,&DEST=,&SSREQ=,&FORUSER=,&TU=,&DSNPDE=           00013400
.********************************************************************** 00013500
.*                                                                    * 00013600
.*    THIS MACRO PROVIDES A DYNAMIC ALLOCATION FUNCTION BY BUILDING   * 00013700
.*    A DYNAMIC ALLOCATION PARAMETER LIST AND INVOKING SVC 99.        * 00013800
.*    IT FIRST SETS UP A WORKAREA ENVIRONMENT FOR THE PARAMETER LIST  * 00013900
.*    AND THEN TESTS THE KEYWORDS SUPPLIED AND INVOKES INNER MACROS   * 00014000
.*    TO BUILD THE TEXT UNITS. THE INNER MACROS THEMSELVES USE INNER  * 00014100
.*    MACROS TO UPDATE GLOBAL VARIABLES, STORE TEXT UNIT POINTERS ETC * 00014200
.*    THERE ARE THREE WAYS OF SPECIFYING THE WORK AREA ADDRESS.       * 00014300
.*    A) MF=AUTO, MF=G, MF=(E,ADDRESS,LNTHSYMB).                      * 00014400
.*    IN THE FIRST FORM, AN INNER MACRO DYNSPACE IS CALLED TO NAME    * 00014500
.*    A WORK AREA, THE NAME BEING RETURNED IN THE GLOBAL SETC         * 00014600
.*    VARIABLE &DYNSP. A DSECT IS CREATED TO MAP THIS AREA.           * 00014700
.*    THE GLOBAL VARIABLES &DTUO (TEXT UNIT OFFSET COUNTER) AND       * 00014800
.*    &DTUPO (TEXT UNIT POINTER OFFSET ACCUMULATOR) ARE SET TO ZERO.  * 00014900
.*    THESE ACCUMULATORS ARE UPDATED AS EACH TEXT UNIT PROCESSOR      * 00015000
.*    AQUIRES STORAGE. AFTER ALL TEXT UNITS HAVE BEEN BUILT, THE      * 00015100
.*    AMOUNT OF SPACE USED IS CALCULATED, AND THE DYNSPACE MACRO IS   * 00015200
.*    THEN CALLED AGAIN TO LOG THE AMOUNT NEEDED. DYNSPACE SETS A     * 00015300
.*    GLOBAL VARIABLE &DYNSPQ TO THE HIGHEST AMOUNT ANY ALLOC OR      * 00015400
.*    FREE MACRO REQUESTED, AND WHEN CALLED WITH THE EXPAND OPTION,   * 00015500
.*    (NO OPERANDS OR NAME FIELD SUPPLIED), EXPANDS INTO A DS FOR     * 00015600
.*    THAT QUANTITY. (SEE DYNSPACE)                                   * 00015700
.*    MF=G SPECIFIES THAT THE ALLOC MACRO ENTER THE BEGIN MACRO       * 00015800
.*    WORKAREA TO ACQUIRE THE STORAGE NECESSARY. IT DOES THIS VIA     * 00015900
.*    THE RCPDS MACRO. (SEE RCPDS). HOWEVER, IF THE ALLOC MACRO IS    * 00016000
.*    CALLED SEVERAL TIMES WITH THIS OPTION, A LOT OF STORAGE WILL BE * 00016100
.*    USED UP, AS THE STORAGE WILL NOT BE SHARED. THUS, THIS FORM     * 00016200
.*    SHOULD ONLY BE USED IF THE ALLOC/FREE MACRO IS ONLY TO BE USED  * 00016300
.*    ONCE OR TWICE DURING AN ASSEMBLY.                               * 00016400
.*    MF=E CAUSES THE MACRO TO USE A USER SPECIFIED WORK AREA. THE    * 00016500
.*    SECOND PARAMETER GIVES THE NAME OF THE WORKAREA, AND AN         * 00016600
.*    OPTIONAL THIRD PARAMETER IS THE NAME OF A SYMBOL TO BE EQUATED  * 00016700
.*    TO THE LENGTH OF THE REQUIRED WORK AREA.                        * 00016800
.*                                                                    * 00016900
.*    DYNAMIC ALLOCATION FUNCTIONS ARE SIMILAR TO THOSE AVAILABLE    *  00017000
.*    WITH JCL, USING THE SAME KEYWORDS. HOWEVER, CERTAIN FORMATS    *  00017100
.*    ARE SLIGHTLY DIFFERENT. FOR INSTANCE, CERTAIN KEYWORDS CAN     *  00017200
.*    HAVE VARYING PARAMETERS, EG DATASET NAME, DDNAME, VOLSER ETC.  *  00017300
.*    PROVISION IS MADE FOR BOTH VARIABLE SPECIFICATION.             *  00017400
.*    IN THE ABSOLUTE FORM, THE PARAMETER IS ENTERED IN QUOTES,      *  00017500
.*    E.G.   ALLOC DSN='SYS1.LINKLIB',DISP=SHR                       *  00017600
.*    HOWEVER, THIS NAME REMAINS FIXED FOR THE ASSEMBLY.             *  00017700
.*    IN THE VARIABLE FORMAT, THE ADDRESS OF A LOCATOR IS SPECIFIED, *  00017800
.*    WHERE THE LOCATOR CONSISTS OF A SIX BYTE FIELD, THE FIRST 4    *  00017900
.*    BYTES OF WHICH POINT TO THE PARAMETER, WHILE THE NEXT TWO      *  00018000
.*    CONTAIN THE LENGTH.                                            *  00018100
.*    EG          ALLOC DSN=LOCATOR                                  *  00018200
.*       LOCATOR  DC    A(DSN),Y(12)                                 *  00018300
.*       DSN      DC    C'SYS1.LINKLIB'                              *  00018400
.*                                                                   *  00018500
.*       NUMERIC QUANTITIES E.G. COPIES= FOR SYSOUT, SHOULD EITHER   *  00018600
.*       SPECIFY A NUMERIC VALUE, COPIES=3,                          *  00018700
.*       A VALUE IN A REGISTER, COPIES=(R3),                         *  00018800
.*       OR THE NAME OFF A FULLWORD CONTAINING THE VALUE,            *  00018900
.*          COPIES=NUMCOPYS, WHERE NUMCOPYS IS THE NAME OF A         *  00019000
.*       FULLWORD FIELD.                                             *  00019100
.*                                                                   *  00019200
.*       OTHER KEYWORDS SUCH AS DISP= CAN ONLY HAVE THE ABSOLUTE     *  00019300
.*       FORM, AND VALUES SHOULD NOT BE ENTERED WITHIN QUOTES.       *  00019400
.*       ADDITIONAL FACILITIES NOT AVAILABLE WITH JCL ARE THE        *  00019500
.*       RETURN BY THE SYSTEM OF INFORMATION ON THE DATASET, EG      *  00019600
.*       DSORG. THIS IS DONE BY SPECIFYING DSORGRT=SYMBOL, WHERE     *  00019700
.*       SYMBOL IS A SYMBOL WHICH WILL BE EQUATED TO A TWO BYTE      *  00019800
.*       FIELD CONTAINING THE DSORG TYPE (SEE JOB MANAGEMENT,        *  00019900
.*       SUPERVISOR AND TSO).                                        *  00020000
.*       THE SYSTEM CAN ALSO GENERATE AND RETURN A DDNAME. THIS IS   *  00020100
.*       CARRIED OUT BY ENTERING DDNTO=(ADDR1,ADDR2,,...)            *  00020200
.*       WHERE ADDR1,ADDR2 ETC ARE THE NAMES OF 8 BYTE FIELDS WHICH  *  00020300
.*       ARE TO RECEIVE THE DDNAME.                                  *  00020400
.*       FOR FURTHER INFORMATION ON DYNAMIC ALLOCATION, SEE          *  00020500
.*       JOB MANAGEMENT, SUPERVISOR AND TSO.                         *  00020600
.*                                                                   *  00020700
.********************************************************************** 00020800
         GBLA  &RCPDYN            COUNTER FOR NO ENTRIES TO MACRO       00020900
         GBLA  &DTUO              OFFSET TO TEXT UNITS                  00021000
         GBLA  &DTUPO             OFFSET TO TEXT UNIT POINTERS          00021100
         GBLB  &RCPS99(2)         TELL RCPDSECT NEED DSECTS             00021200
         GBLC  &DYNP              PREFIX FOR LABELS FOR THIS CALL       00021300
         GBLC  &DYNSP         NAME FOR AUTOMATIC STORAGE ALLOC          00021400
         LCLA  &DDNRTO,&DSNRTO         FOR EQUATES FOR RETURNED FLDS    00021500
         LCLA  &VOLRTO,&DSRGRTO        FOR EQUATES FOR RETURNED FIELDS  00021600
         LCLA  &I                 COUNTER                               00021700
         LCLB  &DSECT             DSECT NEEDED FOR STORAGE, MF=E        00021800
         LCLC  &C,&T,&PAR                                               00021900
.*                                                                      00022000
.*   THE ALLOC MACRO PROVIDES A DYNAMIC ALLOCATION FUNCTION,            00022100
&RCPS99(1)     SETB           1                                         00022200
&RCPDYN  SETA  &RCPDYN+1          INCEREMENT COUNTER                    00022300
&DYNP    SETC  'DYN&RCPDYN' SET DEFAULT PREFIX                          00022400
&NAME    DS    0H                                                       00022500
         AIF   ('&PREFIX' EQ '').TMF                                    00022600
         AIF   (K'&PREFIX LT 4).POK                                     00022700
         MNOTE 4,'PREFIX TOO LONG, 1ST 4 CHARS USED'                    00022800
&DYNP    SETC  '&PREFIX'(1,4)                                           00022900
         AGO   .TMF                                                     00023000
.POK     ANOP                                                           00023100
&DYNP    SETC  '&PREFIX'                                                00023200
.TMF     AIF   ('&MF(1)' EQ 'G').GEN                                    00023300
         AIF   ('&MF' NE 'AUTO').TMFE                                   00023400
NAME     DYNSPACE             GET NAME FOR SPACE                        00023500
         LA    R1,&DYNSP               LOAD ADDRESS OF PARAM LIST       00023600
         USING &DYNP.DS,R1             USE GENERATED DSECT              00023700
&T       SETC  'A'                                                      00023800
&PAR     SETC  '&DYNSP+4'                                               00023900
&DSECT   SETB  1                                                        00024000
         AGO   .START                                                   00024100
.TMFE    AIF   ('&MF(2)' NE '').E2OK                                    00024200
         MNOTE 4,'PLIST ADDRESS OMITTED, MF=G USED'                     00024300
         AGO   .GEN                                                     00024400
.E2OK    ANOP                                                           00024500
&DSECT   SETB  1                                                        00024600
         AIF   ('&MF(2)' EQ '(').RMFE                                   00024700
         LA    R1,&MF(2)               LOAD PARAM LIST ADDRESS          00024800
         USING &DYNP.DS,R1             USE GENERATED DSECT              00024900
         AGO   .START                                                   00025000
.RMFE    AIF   ('&MF(2)' EQ '(R1)' OR '&MF(2)' EQ '(1)').START          00025100
         LR    R1,&PAR                 LOAD S99 PARAM LIST ADDRESS      00025200
         AGO   .START                                                   00025300
.GEN     LA    R1,&DYNP.RBP            LOAD ADDRESS OF S99 RBP          00025400
.START   LA    R15,&DYNP.RB            LOAD ADDRESS OF S99 RB           00025500
         USING S99RB,R15                                                00025600
         ST    R15,0(R1)               AND STORE IN RB POINTER          00025700
         XC    4(&DYNP.LEN-4,R1),4(R1) ZERO PARAMETER LIST              00025800
         MVI   S99RBLN,20              MOVE IN LIST LENGTH              00025900
         MVI   S99VERB,S99VRBAL        MOVE IN VERB CODE                00026000
         LA    R14,&DYNP.TUP           LOAD ADDRESS OF TU POINTERS      00026100
         ST    R14,S99TXTPP            STORE ADDRESS IN S99 RB          00026200
         LA    R15,&DYNP.TU            POINT TO SPACE FOR TEXT UNITS    00026300
         USING S99TUNIT,R15                                             00026400
&DTUO    SETA  0                                                        00026500
&DTUPO   SETA  0                                                        00026600
         AIF   ('&SSREQ' EQ 'YES').SSREQ                                00026700
.TDSN    AIF   ('&DSN&DA' NE '').DSN                                    00026800
         AIF   ('&DSNPDE' NE '').DSNPDE                                 00026900
         AIF   ('&DSNRET' NE '').DSNRT                                  00027000
         AIF   ('&SYSOUT' NE '').SYSOUT                                 00027100
         AIF   ('&DUMMY' NE '').DUMMY                                   00027200
         AIF   ('&QNAME' NE '').QNAME                                   00027300
.TDDN    AIF   ('&DDN&FILE&F' NE '').DDN                                00027400
         AIF   ('&DDNRET&DDNTO' NE '').DDNRT                            00027500
.TUNIT   AIF   ('&UNIT&VOL' NE '').UNIT                                 00027600
.TVOLRET AIF   ('&VOLRET' NE '').VOLRET                                 00027700
.TDSRGO  AIF   ('&DSORGRT' NE '').DSORGRT                               00027800
.TLABEL  AIF   ('&LABEL' NE '').LABEL                                   00027900
.TPSWD   AIF   ('&PASWORD' NE '').PASWORD                               00028000
.TFORUSE AIF   ('&FORUSER' NE '').FORUSER                               00028100
.TTU     AIF   ('&TU' NE '').TU                                         00028200
.TDISP   AIF   ('&DISP' NE '').DISP                                     00028300
.TSPACE  AIF   ('&SPACE' NE '').SPACE                                   00028400
.TLRECL  AIF   ('&LRECL' NE '').DCB                                     00028500
         AIF   ('&DEN' NE '').DCB                                       00028600
         AIF   ('&RECFM' NE '').DCB                                     00028700
         AIF   ('&BLKSIZE' NE '').DCB                                   00028800
         AIF   ('&DSORG' NE '').DCB                                     00028900
         AIF   ('&KEYLEN' NE '').DCB                                    00029000
.TDCBDSN AIF   ('&DCBDSN' NE '').DCBDSN                                 00029100
.TDCBDDN AIF   ('&DCBDDN' NE '').DCBDDN                                 00029200
.TFREE   AIF   ('&FREE' EQ 'CLOSE').FREE                         TE7343 00029300
.TPERM   AIF   ('&PERM' EQ 'PERM' OR '&PERM' EQ 'PERMANENT').PERM       00029400
         AIF   ('&DUMMY' EQ 'PERM' OR '&DUMMY' EQ 'PERMANENT').PERM     00029500
.TSPECI  AIF   ('&SPECIAL' NE '').SPECIAL                               00029600
         AGO   .SVC99                                                   00029700
.SSREQ   RCPSSREQ                                                       00029800
         AGO   .TDSN                                                    00029900
.DSN     RCPDSN &DSN&DA,&MEMBER                                         00030000
         AGO   .TDDN                                                    00030100
.DSNPDE  RCPDSNPD &DSNPDE                                               00030200
         AGO   .TDDN                                                    00030300
.DSNRT   RCPDSNRT &DSNRET                                               00030400
&DSNRTO  SETA  &DTUO-46                                                 00030500
         AGO   .TDDN                                                    00030600
.SYSOUT  RCPSYSOU &SYSOUT,COPIES=&COPIES,FREE=&FREE,DEST=&DEST,        X00030700
               FORMS=&FORMS                                             00030800
         AGO   .TDDN                                                    00030900
.DUMMY   RCPDUMMY &DUMMY                                                00031000
         AGO   .TDDN                                                    00031100
.QNAME   RCPQNAME &QNAME                                                00031200
         AGO   .TDDN                                                    00031300
.DDN     RCPDDN &DDN&F&FILE                                             00031400
         AGO   .TUNIT                                                   00031500
.DDNRT   RCPDDNRT &DDNRET                                               00031600
&DDNRTO  SETA  &DTUO-10                                                 00031700
         AGO   .TUNIT                                                   00031800
.UNIT   RCPUNIT &UNIT,&VOL                                              00031900
         AGO   .TVOLRET                                                 00032000
.VOLRET  RCPVOLRT &VOLRET                                               00032100
&VOLRTO  SETA  &DTUO-8                                                  00032200
         AGO   .TDSRGO                                                  00032300
.DSORGRT RCPDSRGR                                                       00032400
&DSRGRTO SETA  &DTUO-2                                                  00032500
         AGO   .TLABEL                                                  00032600
.LABEL   RCPLABEL &LABEL                                                00032700
         AGO   .TPSWD                                                   00032800
.PASWORD RCPPSWD &PASWORD                                               00032900
         AGO   .TFORUSE                                                 00033000
.FORUSER RCPFORUS &FORUSER                                              00033100
         AGO   .TTU                                                     00033200
.TU      RCPTU &TU                                                      00033300
         AGO   .TDISP                                                   00033400
.DISP    RCPDISP &DISP                                                  00033500
         AGO   .TSPACE                                                  00033600
.SPACE   RCPSPACE &SPACE                                                00033700
         AGO   .TLRECL                                                  00033800
.DCB     RCPDDCB LRECL=&LRECL,DEN=&DEN,RECFM=&RECFM,BLKSIZE=&BLKSIZE,  X00033900
               DSORG=&DSORG,KEYLEN=&KEYLEN                              00034000
         AGO .TDCBDSN                                                   00034100
.DCBDSN  RCPDCBDS &DCBDSN                                               00034200
         AGO .TDCBDDN                                                   00034300
.DCBDDN  RCPDCBDD &DCBDDN                                               00034400
         AGO .TFREE                                              TE7343 00034500
.FREE    RCPFREE  &FREE                                          TE7343 00034600
         AGO   .TPERM                                                   00034700
.PERM    RCPPERM                                                        00034800
         AGO   .TSPECI                                                  00034900
.SPECIAL RCPSPEC &SPECIAL                                               00035000
.SVC99   ANOP                                                           00035100
&DTUPO   SETA  &DTUPO-4                                                 00035200
         SPACE                                                          00035300
         MVI   &DYNP.TUP+&DTUPO,X'80'  SET HIGH ORDER BIT ON TEXT PTRS  00035400
         MVI   &DYNP.RBP,X'80'         SET HIGH ORDER BIT ON RB PTR     00035500
         RCPSR2 UNSAVE                                                  00035600
&DTUPO   SETA  &DTUPO+4                                                 00035700
         AIF   (NOT &DSECT).DYNA                                        00035800
         DROP  R1,R15                  DEACTIVATE ADDRESSABILITY        00035900
         LA    R14,4(R1)               POINT TO REQUEST BLOCK           00036000
.DYNA    DYNALLOC                                                       00036100
         AIF   (NOT &DSECT).LTR                                         00036200
         USING &DYNP.RB,R14            SET UP ADDRESSABILITY            00036300
**       NOTE  R14 HAS RB ADDRESS, R15 HAS SVC 99 RETURN CODE        ** 00036400
.LTR     AIF   ('&ERROR' EQ '').TDDTO                                   00036500
         LTR   R15,R15                 TEST RETURN CODE                 00036600
         BNZ   &ERROR                  BRANCH IF NON ZERO               00036700
.TDDTO   AIF   ('&DDNTO' EQ '').RESERVE                                 00036800
&I       SETA  0                                                        00036900
.DDNTOL  ANOP                                                           00037000
&I       SETA  &I+1                                                     00037100
         AIF   ('&DDNTO(&I)' EQ '').RESERVE                             00037200
         AIF   ('&DDNTO(&I)'(1,1) EQ '(').DDNTOR                        00037300
         MVC   &DDNTO(&I).(8),&DYNP.TU+&DDNRTO+2                        00037400
         AGO   .DDNTOL                                                  00037500
.DDNTOR  ANOP                                                           00037600
&C       SETC  '&DDNTO(&I)'(2,K'&DDNTO(&I)-2)                           00037700
         MVC   0(8,&C),&DYNP.TU+&DDNRTO+2                               00037800
         AGO   .DDNTOL                                                  00037900
.RESERVE AIF   (&DSECT).RESDS                                           00038000
         SPACE 1                                                        00038100
*********************************************************************** 00038200
**       RESERVE SPACE FOR DYNALLOC PARAMETER LIST                   ** 00038300
*********************************************************************** 00038400
         RCPDS                                                          00038500
.SSP     ANOP                                                           00038600
&DYNP.RBP DS   F                       SVC 99 REQ BLOCK POINTER         00038700
&DYNP.RB  DS   5F                      SVC 99 REQUEST BLOCK             00038800
&DYNP.TUP DS   CL&DTUPO                SPACE FOR TEXT POINTERS          00038900
         AIF   (&DTUO EQ 0).DTU21                                       00039000
&DYNP.TU  DS   CL&DTUO                 SPACE FOR TEXT UNITS             00039100
         AIF   (&DSNRTO EQ 0).TDDNRTO                                   00039200
&DSNRET  EQU   &DYNP.TU+&DSNRTO        OFFSET TO RETURNED DSN           00039300
.TDDNRTO AIF   ('&DDNRET' EQ '').DTU11                                  00039400
&DDNRET  EQU   &DYNP.TU+&DDNRTO        OFFSET TO RETURNED DDNAME        00039500
.DTU11   AIF   (&VOLRTO EQ 0).DTU12                                     00039600
&VOLRET  EQU   &DYNP.TU+&VOLRTO        OFFSET TO RETURNED VOLSER        00039700
.DTU12   AIF   (&DSRGRTO EQ 0).DTU10                                    00039800
&DSORGRT EQU   &DYNP.TU+&DSRGRTO       OFFSET TO RETURNED DSORG         00039900
         AGO   .DTU10                                                   00040000
.DTU21   ANOP                                                           00040100
&DYNP.TU  DS   0C                      NO SPACE NEEDED FOR TEXT UNITS   00040200
.DTU10   ANOP                                                           00040300
&DYNP.LEN EQU  *-&DYNP.RBP             LENGTH OF SPACE USED             00040400
         AIF   (&DSECT).DSP                                             00040500
         RCPDS                                                          00040600
         SPACE 3                                                        00040700
         AGO   .EXIT                                                    00040800
.RESDS   ANOP                                                           00040900
         AIF   ('&DYNSP' EQ '').SP3                                     00041000
         DYNSPACE ADD                                                   00041100
.SP3     SPACE                                                          00041200
&DYNP.DS DSECT                         DSECT TO MAP SVC 99 DATA         00041300
         AGO   .SSP                                                     00041400
.DSP     AIF   ('&MF(3)' EQ '').END1                                    00041500
&MF(3)   EQU   &DYNP.LEN               LENGTH OF AREA                   00041600
.END1    ANOP                                                           00041700
&SYSECT  CSECT                                                          00041800
         SPACE 3                                                        00041900
.EXIT    MEND                                                           00042000
./       ADD   NAME=CLEAR                                               00042100
         MACRO                                                          00042200
&NAME    CLEAR &FIELD,&CHAR,&LENGTH                                     00042300
         LCLC  &FILL,&L                                                 00042400
&L       SETC  'L'''                                                    00042500
&FILL    SETC  '&CHAR'                                                  00042600
         AIF   ('&CHAR' NE '').CHSPEC                                   00042700
&FILL    SETC  '40'                                                     00042800
.CHSPEC  ANOP                                                           00042900
&NAME    MVI   &FIELD,X'&FILL'   SET THE FIRST POSITION                 00043000
         AIF   ('&LENGTH' EQ '').NOLSPEC                                00043100
         MVC   &FIELD+1(&LENGTH),&FIELD  FILL THE ENTIRE FIELD          00043200
         MEXIT                                                          00043300
.NOLSPEC ANOP                                                           00043400
         MVC   &FIELD+1(&L&FIELD-1),&FIELD  FILL THE ENTIRE FIELD       00043500
         MEND                                                           00043600
./       ADD   NAME=CONV                                                00043700
         MACRO                                                          00043800
&LABEL   CONV  &TO,&FROM,&LEN,&EDMASK,&SCOMP                            00043900
         LCLC  &L,&FIRSTFR,&EDM,&COMP                                   00044000
         LCLA  &COUNT                                                   00044100
&L       SETC  'L'''                                                    00044200
         AIF   ('&LABEL' EQ '').NOLABEL  SKIP LABEL IF NOT PRESENT      00044300
&LABEL   DS    0H             SET THE LABEL                             00044400
.NOLABEL ANOP                                                           00044500
&EDM     SETC  'EDMASK'      DEFAULT EDIT MASK                          00044600
         AIF   ('&EDMASK' EQ '').DEFMASK  IF NOT ENTERED USE DEFAULT    00044700
&EDM     SETC  '&EDMASK'     USE THE ENTERED VALUE                      00044800
.DEFMASK ANOP                                                           00044900
&COMP    SETC  'BLANKS'      DEFAULT COMPARISON CHARS                   00045000
         AIF   ('&SCOMP' EQ '').DEFCOMP  NOT ENTERED, USE THE DEFAULT   00045100
&COMP    SETC  '&SCOMP'      GET WHAT THE GUY WANTS                     00045200
.DEFCOMP ANOP                                                           00045300
&FIRSTFR SETC  '&FROM'(1,1)   GET FIRST CHAR OF &FROM                   00045400
         AIF   ('&FIRSTFR' EQ '(').REGISTR                              00045500
         L     R1,&FROM       GET THE DATA TO CONVERT                   00045600
         CVD   R1,DOUBLE      CONVERT TO PACKED DECIMAL                 00045700
         AGO   .INDEC                                                   00045800
.REGISTR ANOP                                                           00045900
&COUNT   SETA  K'&FROM-2                                                00046000
&FIRSTFR SETC  '&FROM'(2,&COUNT)  STRIP THE PERRONS                     00046100
         CVD   &FIRSTFR,DOUBLE   CONVERT TO PACKED DECIMAL              00046200
.INDEC   ANOP                                                           00046300
         MVC   CHARS,&EDM     PUT IN THE EDIT MASK                      00046400
         ED    CHARS,DOUBLE   CONVERT TO CHARACTERS                     00046500
         AIF   ('&LEN' NE '').LENSET                                    00046600
         MVC   &TO,CHARS+16-&L&TO  MOVE IN THE NUMBER                   00046700
         CLC   CHARS(16-&L&TO),&COMP   WAS THERE AN OVERFLOW?           00046800
         BE    *+10           NO, EVERYTHING WAS OK                     00046900
         MVC   &TO,STARS      BAD NEWS, NOTE IT                         00047000
         MEXIT                                                          00047100
.LENSET  ANOP                                                           00047200
         MVC   &TO.(&LEN),CHARS+16-&LEN MOVE IN THE NUMBER              00047300
         CLC   CHARS(16-&LEN),&COMP   WAS THERE AN OVERFLOW?            00047400
         BE    *+10           NO, EVERYTHING WAS OK                     00047500
         MVC   &TO.(&LEN),STARS   BAD NEWS, NOTE IT                     00047600
         MEND                                                           00047700
./       ADD   NAME=DYNSPACE                                            00047800
         MACRO                                                          00047900
&NAME    DYNSPACE &TYPE                                                 00048000
.*                                                                      00048100
.*    THIS IS AN INNER MACRO TO ALLOC/FREE.                             00048200
.*    IT IS CALLED TO   A) NAME AN AREA FOR THE PARMLIST                00048300
.*                      B) LOG THE VARIOUS AMOUNTS NEEDED BY            00048400
.*                         EACH, REMEMBERING THE LARGEST.               00048500
.*                      C) GENERATING A DS FOR THE LARGEST AMOUNT.      00048600
.*    THE FIRST TWO FUNCTIONS ARE INVOKED BY ALLOC/FREE MACROS ONLY,    00048700
.*    AND THE THIRD IS USED BY THE PROGRAMMER, EITHER EXPLICITLY,       00048800
.*    OR BY BEGINWKA, IF THE LATTER IS USED.                            00048900
.*                                                                      00049000
.*     TO INVOKE THE NAMING FUNCTION, ALLOC/FREE GENERATE               00049100
.*     NAME DYNSPACE                                                    00049200
.*     NOTE. THE NAMING OPERATION ONLY GENERATES A NAME ON THE          00049300
.*     FIRST CALL IN THE ASSEMBLY. THE NAME REMAINS THE SAME UNTIL      00049400
.*     DYNSPACE IS CALLED TO EXPAND INTO A DS.                          00049500
.*                                                                      00049600
.*     THE SECOND FUNCTION IS INVOKED BY THE MACRO CALL                 00049700
.*          DYNSPACE ADD                                                00049800
.*     (NO NAME FIELD AND ONE OPERAND)                                  00049900
.*     IT USES THE GLOBAL VARIABLES &DTUO AND &DTUPO TO CALCULATE       00050000
.*     THE SPACE FOR THIS REQUEST, AND UPDATES &DYNSPQ ONLY IF THE      00050100
.*     CURRENT REQUEST IS FOR A GREATER AMOUNT                          00050200
.*                                                                      00050300
.*     THE THIRD FUNCTION IS INVOKED BY CALLING DYNSPACE WITH NO        00050400
.*     NAME OR OPERAND FIELD.                                           00050500
.*     THIS EXPANDS INTO A DEFINE STORAGE, CLEARS THE DYNSPACE NAME     00050600
.*     GLOBAL SETC, AND THE &DYNSPQ GLOBAL SETA.                        00050700
.*     THUS, THE MACRO IS SERIALLY REUSABLE IN ALL FUNCTIONS.           00050800
.*                                                                      00050900
         GBLA  &DYNSPQ,&DTUO,&DTUPO,&RCPDYN                             00051000
         GBLC  &DYNP,&DYNSP                                             00051100
         LCLA  &I                                                       00051200
         AIF   ('&NAME' NE '').NAME                                     00051300
         AIF   ('&TYPE' EQ '').ALLOC                                    00051400
.*   THE ACCUMULATE FUNCTION IS REQUIRED                                00051500
&I       SETA  24+&DTUO+&DTUPO         GET AMOUNT FOR THIS REQUEST      00051600
         AIF   (&I LE &DYNSPQ).EXIT    IF CURRENT < MAX, EXIT           00051700
&DYNSPQ  SETA  &I                      ELSE UPDATE CURRENT MAXIMUM      00051800
         MEXIT                                                          00051900
.NAME    AIF   ('&DYNSP' NE '').EXIT   IF NAME ALREADY EXISTS, EXIT     00052000
&DYNSP   SETC  'DYNSP&RCPDYN'           ELSE GENERATE A NAME            00052100
.EXIT    MEXIT                                                          00052200
.ALLOC   AIF   ('&DYNSP' EQ '').EXIT                                    00052300
*                                                                       00052400
**     RESERVE SPACE FOR ALLOC/FREE MACRO WORK AREA                     00052500
*                                                                       00052600
&DYNSP   DS    0F,CL&DYNSPQ            RESERVE SPACE                    00052700
&DYNSP   SETC  ''                      SET MAX QUANTITY TO 0            00052800
&DYNSPQ  SETA 0                                                         00052900
         MEND                                                           00053000
./       ADD   NAME=ENTER                                               00053100
         MACRO                                                          00053200
&SUBR    ENTER &BASES,&SAVE,&CSECT                                      00053300
.*   THIS MACRO, USED WITH THE LEAVE MACRO, WILL PERFORM                00053400
.*   STANDARD HOUSEKEEPING FOR A CSECT, INCLUDING SAVEAREA              00053500
.*   CONSTRUCTION AND CHAINING, AND GETTING SOME STORAGE,               00053600
.*   IF THAT IS DESIRED.                                                00053700
.*   THE LEAVE MACRO WILL FREE THE GOTTEN STORAGE                       00053800
.*   THE OPERANDS ARE                                                   00053900
.*       &SUBR    ENTER  &BASES,&SAVE,&CSECT                            00054000
.*    WHERE                                                             00054100
.*       &SUBR    IS THE NAME OF THE CSECT                              00054200
.*       &BASES   ARE THE BASE REGISTERS FOR THE ROUTINE                00054300
.*       &SAVE    IS THE LABEL FOR A SAVEAREA, OR A SUBPOOL             00054400
.*                AND LENGTH FOR THE GETMAIN                            00054500
.*       &CSECT   TO CONTINUE AN EXISTING CSECT WITH ENTRY              00054600
.*                POINT &SUBR                                           00054700
.*                                                                      00054800
.*    EXAMPLES -                                                        00054900
.*               ENTER 13,*                                             00055000
.*                                                                      00055100
.*       THIS WILL GENERATE NON-REENTRANT CODE, USING SAVEAREA          00055200
.*       AS THE SAVE AREA LABEL, AND REGISTER 13 FOR THE BASE           00055300
.*       REGISTER.                                                      00055400
.*                                                                      00055500
.*       RENTMOD  ENTER (12,11),(,LDSECT)                               00055600
.*                                                                      00055700
.*       THIS WILL GENERATE REENTRANT CODE WITH REGISTERS 12 AND        00055800
.*       11 FOR BASE REGISTERS.  A GETMAIN WILL BE DONE FOR THE         00055900
.*       DEFAULT SUBPOOL (0) WITH A LENGTH 'LDSECT'.                    00056000
.*                                                                      00056100
         GBLC  &LV,&SP                                                  00056200
         LCLA  &K,&N                                                    00056300
         LCLC  &AREA,&B(16),&SUBNAME,&S                                 00056400
&SUBNAME SETC  '&SUBR'                                                  00056500
         AIF   ('&SUBNAME' NE '').SUBSPEC                               00056600
&SUBNAME SETC  'MAIN'         DEFAULT CSECT NAME                        00056700
.SUBSPEC AIF   ('&CSECT' EQ '').NOTENT  IS IT AN ENTRY POINT?           00056800
&CSECT   CSECT                                                          00056900
&SUBNAME DS    0F                                                       00057000
         AGO   .CSSPEC                                                  00057100
.NOTENT  ANOP                                                           00057200
&SUBNAME CSECT                                                          00057300
.CSSPEC  ANOP                                                           00057400
         SAVE  (14,12),T,&SUBNAME   SAVE THE REGISTERS                  00057500
         AIF   ('&BASES(1)' EQ '15' OR '&BASES' EQ '').R15SET           00057600
         AIF   ('&BASES(1)' EQ '13' AND '&SAVE' NE '').R15SET           00057700
         LR    &BASES(1),15  SET FIRST BASE REG                         00057800
.R15SET  CNOP  0,4                                                      00057900
&S       SETC  '&SUBNAME'                                               00058000
         AIF   (N'&SAVE EQ 2).P4   SUBPOOL, SIZE SPEC?                  00058100
         AIF   ('&SAVE' EQ '').P3  NO SAVEAREA - DEFAULT                00058200
&AREA    SETC  '&SAVE'                                                  00058300
         AIF   ('&SAVE' NE '*').P2                                      00058400
&AREA    SETC  'SAVEAREA'                                               00058500
.P2      AIF   ('&BASES(1)' NE '13').P4                                 00058600
&S       SETC  '*'                                                      00058700
         USING &SUBNAME,15                                              00058800
         ST    14,&AREA+4                                               00058900
         LA    14,&AREA                                                 00059000
         ST    14,8(13)                                                 00059100
         L     14,&AREA+4                                               00059200
         ST    13,&AREA+4                                               00059300
         BAL   13,*+76        SKIP AROUND THE SAVEAREA                  00059400
         DROP  15                                                       00059500
         AGO   .P4                                                      00059600
.P3      AIF   ('&BASES(1)' NE '13').P4                                 00059700
         MNOTE 8,'*** CONTENTS OF REG 13 ARE LOST.  NO SAVE AREA WAS ESX00059800
               TABLISHED.'                                              00059900
.P4      AIF   ('&BASES(1)' NE '14' OR '&SAVE' EQ '').P5                00060000
         MNOTE 8,'*** MACRO RESTRICTION - REG 14 MUST NOT BE USED AS THX00060100
               E FIRST BASE REGISTER IF A SAVE AREA IS USED.'           00060200
.P5      AIF   ('&BASES' EQ '').P9                                      00060300
&N       SETA  N'&BASES                                                 00060400
.P6      ANOP                                                           00060500
&K       SETA  &K+1                                                     00060600
&B(&K)   SETC  ','.'&BASES(&K)'                                         00060700
         AIF   (N'&SAVE EQ 1).PE                                        00060800
         AIF   ('&BASES(&K)' NE '13').P7                                00060900
         MNOTE 8,'*** REG 13 MAY NOT BE USED AS A BASE REGISTER FOR REEX00061000
               NTRANT CODE.'                                            00061100
         AGO   .P7                                                      00061200
.PE      AIF   ('&BASES(&K+1)' NE '13' OR '&SAVE' EQ '').P7             00061300
         MNOTE 8,'*** WHEN USING A SAVE AREA, REG 13 MAY NOT BE USED ASX00061400
                A SECONDARY BASE REGISTER.'                             00061500
.P7      AIF   ('&BASES(&K+1)' NE '').P6                                00061600
         USING &S&B(1)&B(2)&B(3)&B(4)&B(5)&B(6)&B(7)&B(8)&B(9)&B(10)&B(X00061700
               11)&B(12)&B(13)&B(14)&B(15)&B(16)                        00061800
&K       SETA  1                                                        00061900
         AIF   ('&BASES(1)' NE '13' OR '&SAVE' EQ '').P8                00062000
&AREA    DC    18F'0'                                                   00062100
.P8      AIF   (&K GE &N).P10                                           00062200
         LA    &BASES(&K+1),X'FFF'(&BASES(&K))                          00062300
         LA    &BASES(&K+1),1(&BASES(&K+1))                             00062400
&K       SETA  &K+1                                                     00062500
         AGO   .P8                                                      00062600
.P9      USING &SUBNAME,15                                              00062700
.P10     AIF   (N'&SAVE GE 2).P13                                       00062800
         AIF   ('&SAVE' EQ '' OR '&BASES(1)' EQ '13').P12               00062900
         AIF   ('&SAVE' GE '0').P16  NUMERIC MEANS A PASSED AREA        00063000
         ST    14,&AREA+4                                               00063100
         LA    14,&AREA                                                 00063200
         ST    14,8(13)                                                 00063300
         L     14,&AREA+4                                               00063400
         ST    13,&AREA+4                                               00063500
.P11     BAL   13,*+76       SKIP AROUND THE SAVEAREA                   00063600
&AREA    DC    18F'0'                                                   00063700
.P12     MEXIT                                                          00063800
.P13     ANOP                                                           00063900
&LV      SETC  '&SAVE(2)'                                               00064000
&SP      SETC  '0'                                                      00064100
         AIF   ('&SAVE(1)' EQ '').P14                                   00064200
&SP      SETC  '&SAVE(1)'                                               00064300
.P14     CNOP  0,4          DO A GETMAIN FOR THE AREA                   00064400
         BAL   1,*+8          POINT THE SP AND LV                       00064500
ENT&SYSNDX DC  AL1(&SP)       SUBPOOL FOR THE GETMAIN                   00064600
         DC    AL3(&LV)       LENGTH OF THE GETMAIN                     00064700
         L     0,0(1)         GET THE DATA IN REG 1                     00064800
         SVC   10             ISSUE THE GETMAIN                         00064900
.*                            CHAIN THE SAVEAREAS                       00065000
         ST    13,4(1)        PRIOR SAVEAREA ADDRESS TO MINE            00065100
         ST    1,8(13)        MY SAVEAREA ADDRESS TO HIS                00065200
         LR    2,13           KEEP THE SAVEAREA ADDRESS FOR REGS        00065300
         LR    13,1           THIS IS MY SAVEAREA                       00065400
         LM    0,2,20(2)      RESTORE ORIGINAL REGS                     00065500
         MEXIT                                                          00065600
.P16     L     1,&AREA+0(1)   NUMERIC &SAVE IMPLIES A PASSED SAVEAREA   00065700
         ST    13,4(1)        PRIOR SAVEAREA ADDRESS TO MINE            00065800
         ST    1,8(13)        MY SAVEAREA ADDRESS TO HIS                00065900
         LR    2,13           KEEP THE SAVEAREA ADDRESS FOR REGS        00066000
         LR    13,1           THIS IS MY SAVEAREA                       00066100
         LM    0,2,20(2)      RESTORE ORIGINAL REGS                     00066200
         MEND                                                           00066300
./       ADD   NAME=ENTERX                                              00066400
         MACRO                                                          00066500
&SUBR    ENTERX &BASES,&SAVE,&CSECT                                     00066600
.*   THIS MACRO, USED WITH THE LEAVE MACRO, WILL PERFORM                00066700
.*   STANDARD HOUSEKEEPING FOR A CSECT, INCLUDING SAVEAREA              00066800
.*   CONSTRUCTION AND CHAINING, AND GETTING SOME STORAGE,               00066900
.*   IF THAT IS DESIRED.                                                00067000
.*   THE LEAVE MACRO WILL FREE THE GOTTEN STORAGE                       00067100
.*   THE OPERANDS ARE                                                   00067200
.*       &SUBR    ENTER  &BASES,&SAVE,&CSECT                            00067300
.*    WHERE                                                             00067400
.*       &SUBR    IS THE NAME OF THE CSECT                              00067500
.*       &BASES   ARE THE BASE REGISTERS FOR THE ROUTINE                00067600
.*       &SAVE    IS THE LABEL FOR A SAVEAREA, OR A SUBPOOL             00067700
.*                AND LENGTH FOR THE GETMAIN                            00067800
.*       &CSECT   TO CONTINUE AN EXISTING CSECT WITH ENTRY              00067900
.*                POINT &SUBR                                           00068000
.*                                                                      00068100
.*    EXAMPLES -                                                        00068200
.*               ENTER 13,*                                             00068300
.*                                                                      00068400
.*       THIS WILL GENERATE NON-REENTRANT CODE, USING SAVEAREA          00068500
.*       AS THE SAVE AREA LABEL, AND REGISTER 13 FOR THE BASE           00068600
.*       REGISTER.                                                      00068700
.*                                                                      00068800
.*       RENTMOD  ENTER (12,11),(,LDSECT)                               00068900
.*                                                                      00069000
.*       THIS WILL GENERATE REENTRANT CODE WITH REGISTERS 12 AND        00069100
.*       11 FOR BASE REGISTERS.  A GETMAIN WILL BE DONE FOR THE         00069200
.*       DEFAULT SUBPOOL (0) WITH A LENGTH 'LDSECT'.                    00069300
.*                                                                      00069400
         GBLC  &LV,&SP                                                  00069500
         LCLA  &K,&N                                                    00069600
         LCLC  &AREA,&B(16),&SUBNAME,&S                                 00069700
&SUBNAME SETC  '&SUBR'                                                  00069800
         AIF   ('&SUBNAME' NE '').SUBSPEC                               00069900
&SUBNAME SETC  'MAIN'         DEFAULT CSECT NAME                        00070000
.SUBSPEC AIF   ('&CSECT' EQ '').NOTENT  IS IT AN ENTRY POINT?           00070100
&CSECT   CSECT                                                          00070200
&SUBNAME DS    0F                                                       00070300
         AGO   .CSSPEC                                                  00070400
.NOTENT  ANOP                                                           00070500
&SUBNAME CSECT                                                          00070600
.CSSPEC  ANOP                                                           00070700
         SAVE  (14,12),T,&SUBNAME   SAVE THE REGISTERS                  00070800
         AIF   ('&BASES(1)' EQ '15' OR '&BASES' EQ '').R15SET           00070900
         AIF   ('&BASES(1)' EQ '13' AND '&SAVE' NE '').R15SET           00071000
         LR    &BASES(1),15  SET FIRST BASE REG                         00071100
.R15SET  CNOP  0,4                                                      00071200
&S       SETC  '&SUBNAME'                                               00071300
         AIF   (N'&SAVE EQ 2).P4   SUBPOOL, SIZE SPEC?                  00071400
         AIF   ('&SAVE' EQ '').P3  NO SAVEAREA - DEFAULT                00071500
&AREA    SETC  '&SAVE'                                                  00071600
         AIF   ('&SAVE' NE '*').P2                                      00071700
&AREA    SETC  'SAVEAREA'                                               00071800
.P2      AIF   ('&BASES(1)' NE '13').P4                                 00071900
&S       SETC  '*'                                                      00072000
         USING &SUBNAME,15                                              00072100
         ST    14,&AREA+4                                               00072200
         LA    14,&AREA                                                 00072300
         ST    14,8(13)                                                 00072400
         L     14,&AREA+4                                               00072500
         ST    13,&AREA+4                                               00072600
         BAL   13,*+76        SKIP AROUND THE SAVEAREA                  00072700
         DROP  15                                                       00072800
         AGO   .P4                                                      00072900
.P3      AIF   ('&BASES(1)' NE '13').P4                                 00073000
         MNOTE 8,'*** CONTENTS OF REG 13 ARE LOST.  NO SAVE AREA WAS ESX00073100
               TABLISHED.'                                              00073200
.P4      AIF   ('&BASES(1)' NE '14' OR '&SAVE' EQ '').P5                00073300
         MNOTE 8,'*** MACRO RESTRICTION - REG 14 MUST NOT BE USED AS THX00073400
               E FIRST BASE REGISTER IF A SAVE AREA IS USED.'           00073500
.P5      AIF   ('&BASES' EQ '').P9                                      00073600
&N       SETA  N'&BASES                                                 00073700
.P6      ANOP                                                           00073800
&K       SETA  &K+1                                                     00073900
&B(&K)   SETC  ','.'&BASES(&K)'                                         00074000
         AIF   (N'&SAVE EQ 1).PE                                        00074100
         AIF   ('&BASES(&K)' NE '13').P7                                00074200
         MNOTE 8,'*** REG 13 MAY NOT BE USED AS A BASE REGISTER FOR REEX00074300
               NTRANT CODE.'                                            00074400
         AGO   .P7                                                      00074500
.PE      AIF   ('&BASES(&K+1)' NE '13' OR '&SAVE' EQ '').P7             00074600
         MNOTE 8,'*** WHEN USING A SAVE AREA, REG 13 MAY NOT BE USED ASX00074700
                A SECONDARY BASE REGISTER.'                             00074800
.P7      AIF   ('&BASES(&K+1)' NE '').P6                                00074900
         USING &S&B(1)&B(2)&B(3)&B(4)&B(5)&B(6)&B(7)&B(8)&B(9)&B(10)&B(X00075000
               11)&B(12)&B(13)&B(14)&B(15)&B(16)                        00075100
&K       SETA  1                                                        00075200
         AIF   ('&BASES(1)' NE '13' OR '&SAVE' EQ '').P8                00075300
&AREA    DC    18F'0'                                                   00075400
.P8      AIF   (&K GE &N).P10                                           00075500
         LA    &BASES(&K+1),X'FFF'(&BASES(&K))                          00075600
         LA    &BASES(&K+1),1(&BASES(&K+1))                             00075700
&K       SETA  &K+1                                                     00075800
         AGO   .P8                                                      00075900
.P9      USING &SUBNAME,15                                              00076000
.P10     AIF   (N'&SAVE GE 2).P13                                       00076100
         AIF   ('&SAVE' EQ '' OR '&BASES(1)' EQ '13').P12               00076200
         AIF   ('&SAVE(1,1)' GE '0').P16  NUMERIC MEANS A PASSED AREA   00076300
         ST    14,&AREA+4                                               00076400
         LA    14,&AREA                                                 00076500
         ST    14,8(13)                                                 00076600
         L     14,&AREA+4                                               00076700
         ST    13,&AREA+4                                               00076800
.P11     BAL   13,*+76       SKIP AROUND THE SAVEAREA                   00076900
&AREA    DC    18F'0'                                                   00077000
.P12     MEXIT                                                          00077100
.P13     ANOP                                                           00077200
&LV      SETC  '&SAVE(2)'                                               00077300
&SP      SETC  '0'                                                      00077400
         AIF   ('&SAVE(1)' EQ '').P14                                   00077500
&SP      SETC  '&SAVE(1)'                                               00077600
.P14     CNOP  0,4          DO A GETMAIN FOR THE AREA                   00077700
         BAL   1,*+8          POINT THE SP AND LV                       00077800
ENT&SYSNDX DC  AL1(&SP)       SUBPOOL FOR THE GETMAIN                   00077900
         DC    AL3(&LV)       LENGTH OF THE GETMAIN                     00078000
         L     0,0(1)         GET THE DATA IN REG 1                     00078100
         SVC   10             ISSUE THE GETMAIN                         00078200
.*                            CHAIN THE SAVEAREAS                       00078300
         ST    13,4(1)        PRIOR SAVEAREA ADDRESS TO MINE            00078400
         ST    1,8(13)        MY SAVEAREA ADDRESS TO HIS                00078500
         LR    2,13           KEEP THE SAVEAREA ADDRESS FOR REGS        00078600
         LR    13,1           THIS IS MY SAVEAREA                       00078700
         LA    4,12(13)       YES, POINT PAST THE CHAIN                 00078800
         L     5,ENT&SYSNDX   GET THE SIZE                              00078900
         LA    6,12           MINUS THE CHAIN AREA (12 BYTES )          00079000
         SR    5,6            GIVES THE AMOUNT TO CLEAR                 00079100
         SR    7,7            CLEAR THE FROM COUNT AND CLEAR BYTE       00079200
         MVCL  4,6            WHEE, CLEAR IT OUT                        00079300
         LM    0,7,20(2)      RESTORE THE ORIGINAL REGISTERS            00079400
         MEXIT                                                          00079500
.P16     L     1,&AREA+0(1)   NUMERIC &SAVE IMPLIES A PASSED SAVEAREA   00079600
         ST    13,4(1)        PRIOR SAVEAREA ADDRESS TO MINE            00079700
         ST    1,8(13)        MY SAVEAREA ADDRESS TO HIS                00079800
         LR    2,13           KEEP THE SAVEAREA ADDRESS FOR REGS        00079900
         LR    13,1           THIS IS MY SAVEAREA                       00080000
         LM    0,2,20(2)      RESTORE ORIGINAL REGS                     00080100
         MEND                                                           00080200
./       ADD   NAME=FREE                                                00080300
         MACRO                                                          00080400
&NAME    FREE  &UNALC,&DSN=,&DDN=,&MEMBER=,&DISP=,&SYSOUT=,            X00080500
               &ERROR=,&MF=AUTO,&PREFIX=,&FILE=,&F=,&DA=,&HOLD=         00080600
         GBLA  &RCPDYN            COUNTER FOR NO ENTRIES TO MACRO       00080700
         GBLA  &DTUO              OFFSET TO TEXT UNITS                  00080800
         GBLA  &DTUPO             OFFSET TO TEXT UNIT POINTERS          00080900
         GBLB  &RCPS99(2)         TELL RCPDSECT NEED DSECTS             00081000
         GBLC  &DYNP              PREFIX FOR LABELS FOR THIS CALL       00081100
         GBLC  &DYNSP         NAME FOR AUTOMATIC STORAGE ALLOC          00081200
         LCLB  &DSECT             DSECT NEEDED FOR STORAGE, MF=E        00081300
         LCLC  &C,&T,&PAR                                               00081400
&RCPS99(1)     SETB           1                                         00081500
&RCPDYN  SETA  &RCPDYN+1          INCEREMENT COUNTER                    00081600
&DYNP    SETC  'DYN&RCPDYN' SET DEFAULT PREFIX                          00081700
&NAME    DS    0H                                                       00081800
         AIF   ('&PREFIX' EQ '').TMF                                    00081900
         AIF   (K'&PREFIX LT 4).POK                                     00082000
         MNOTE 4,'PREFIX TOO LONG, 1ST 4 CHARS USED'                    00082100
&DYNP    SETC  '&PREFIX'(1,4)                                           00082200
         AGO   .TMF                                                     00082300
.POK     ANOP                                                           00082400
&DYNP    SETC  '&PREFIX'                                                00082500
.TMF     AIF   ('&MF(1)' EQ 'G').GEN                                    00082600
         AIF   ('&MF' NE 'AUTO').TMFE                                   00082700
NAME     DYNSPACE             GET NAME FOR SPACE                        00082800
         LA    R1,&DYNSP               LOAD ADDRESS OF PARAM LIST       00082900
         USING &DYNP.DS,R1             USE GENERATED DSECT              00083000
&T       SETC  'A'                                                      00083100
&PAR     SETC  '&DYNSP+4'                                               00083200
&DSECT   SETB  1                                                        00083300
         AGO   .START                                                   00083400
.TMFE    AIF   ('&MF(2)' NE '').E2OK                                    00083500
         MNOTE 4,'PLIST ADDRESS OMITTED, MF=G USED'                     00083600
         AGO   .GEN                                                     00083700
.E2OK    ANOP                                                           00083800
&DSECT   SETB  1                                                        00083900
         AIF   ('&MF(2)' EQ '(').RMFE                                   00084000
         LA    R1,&MF(2)               LOAD PARAM LIST ADDRESS          00084100
&T       SETC  'A'                                                      00084200
&PAR     SETC  '&MF(2)+4'                                               00084300
         USING &DYNP.DS,R1             USE GENERATED DSECT              00084400
         AGO   .START                                                   00084500
.RMFE    AIF   ('&MF(2)' EQ '(R1)' OR '&MF(2)' EQ '(1)').START          00084600
&PAR     SETC  '&MF(2)'(2,K'&MF(2)-2)                                   00084700
&T       SETC  'R'                                                      00084800
         LR    R1,&PAR                 LOAD S99 PARAM LIST ADDRESS      00084900
&PAR     SETC  '4&MF(2)'                                                00085000
         USING &DYNP.DS,R1             USE GENERATED DSECT              00085100
         AGO   .START                                                   00085200
.GEN     LA    R1,&DYNP.RBP            LOAD ADDRESS OF S99 RBP          00085300
&T       SETC  'A'                                                      00085400
&PAR     SETC  '&DYNP.RB'                                               00085500
.START   LA    R15,&DYNP.RB            LOAD ADDRESS OF S99 RB           00085600
         USING S99RB,R15                                                00085700
         ST    R15,0(R1)               AND STORE IN RB POINTER          00085800
         XC    4(&DYNP.LEN-4,R1),4(R1) ZERO PARAMETER LIST              00085900
         MVI   S99RBLN,20              MOVE IN LIST LENGTH              00086000
         MVI   S99VERB,S99VRBUN        MOVE IN VERB CODE                00086100
         LA    R14,&DYNP.TUP           LOAD ADDRESS OF TU POINTERS      00086200
         ST    R14,S99TXTPP            STORE ADDRESS IN S99 RB          00086300
         LA    R15,&DYNP.TU            POINT TO SPACE FOR TEXT UNITS    00086400
         USING S99TUNIT,R15                                             00086500
&DTUO    SETA  0                                                        00086600
&DTUPO   SETA  0                                                        00086700
         AIF   ('&DSN&DA' NE '').DSN                                    00086800
         AIF   ('&SYSOUT' NE '').SYSOUT                                 00086900
.TDDN    AIF   ('&DDN&FILE&F' NE '').DDN                                00087000
.TDISP   AIF   ('&DISP' NE '').DISP                                     00087100
.TUNALC  AIF   ('&UNALC' NE '').PERM                                    00087200
.THOLD   AIF   ('&HOLD' NE '').HOLD                                     00087300
         AGO   .SVC99                                                   00087400
.DSN     RCPFDSN &DSN&DA,&MEMBER                                        00087500
         AGO   .TDDN                                                    00087600
.SYSOUT  RCPFSYS &SYSOUT                                                00087700
         AGO   .TDDN                                                    00087800
.DDN     RCPFDDN &DDN&F&FILE                                            00087900
         AGO   .TDISP                                                   00088000
.DISP RCPFDISP &DISP                                                    00088100
         AGO   .TUNALC                                                  00088200
.PERM    RCPUNALC                                                       00088300
         AGO   .THOLD                                                   00088400
.HOLD    RCPFHOLD &HOLD                                                 00088500
.SVC99   ANOP                                                           00088600
&DTUPO   SETA  &DTUPO-4                                                 00088700
         SPACE                                                          00088800
         MVI   &DYNP.TUP+&DTUPO,X'80'  SET HIGH ORDER BIT ON TEXT PTRS  00088900
         MVI   &DYNP.RBP,X'80'         SET HIGH ORDER BIT ON RB PTR     00089000
         RCPSR2 UNSAVE                                                  00089100
&DTUPO   SETA  &DTUPO+4                                                 00089200
         AIF   (NOT &DSECT).DYNA                                        00089300
         DROP  R1,R15                  DEACTIVATE ADDRESSABILITY        00089400
.DYNA    DYNALLOC                                                       00089500
         AIF   ('&ERROR' EQ '').RESERVE                                 00089600
         AIF   ('&PAR' EQ '').LTR                                       00089700
         L&T   R14,&PAR                 LOAD REG 14 WITH ADDRESS OF RB  00089800
         AIF   (NOT &DSECT).LTR                                         00089900
         USING &DYNP.RB,R14            SET UP ADDRESSABILITY            00090000
.LTR     LTR   R15,R15                 TEST RETURN CODE                 00090100
         BNZ   &ERROR                  BRANCH IF NON ZERO               00090200
**       NOTE.  R14 POINTS TO REQUEST BLOCK, R15 HAS RETURN CODE     ** 00090300
.RESERVE AIF   (&DSECT).RESDS                                           00090400
         SPACE                                                          00090500
*********************************************************************** 00090600
**       RESERVE SPACE FOR DYNALLOC DATA                             ** 00090700
*********************************************************************** 00090800
         RCPDS                                                          00090900
.SSP     ANOP                                                           00091000
&DYNP.RBP DS   F                       SVC 99 REQ BLOCK POINTER         00091100
&DYNP.RB  DS   5F                      SVC 99 REQUEST BLOCK             00091200
&DYNP.TUP DS   CL&DTUPO                SPACE FOR TEXT POINTERS          00091300
         AIF   (&DTUO EQ 0).DTU11                                       00091400
&DYNP.TU  DS   CL&DTUO                 SPACE FOR TEXT UNITS             00091500
         AGO   .DTU10                                                   00091600
.DTU11   ANOP                                                           00091700
&DYNP.TU  DS   0C                      NO SPACE NEEDED FOR TEXT UNITS   00091800
.DTU10   ANOP                                                           00091900
&DYNP.LEN EQU  *-&DYNP.RBP             LENGTH OF SPACE USED             00092000
         AIF   (&DSECT).DSP                                             00092100
         RCPDS                                                          00092200
         SPACE 3                                                        00092300
         AGO   .EXIT                                                    00092400
.RESDS   ANOP                                                           00092500
         AIF   ('&DYNSP' EQ '').SP3                                     00092600
         DYNSPACE ADD                                                   00092700
.SP3     SPACE                                                          00092800
&DYNP.DS DSECT                         DSECT TO MAP SVC 99 DATA         00092900
         AGO   .SSP                                                     00093000
.DSP     AIF   ('&MF(3)' EQ '').END1                                    00093100
&MF(3)   EQU   &DYNP.LEN               LENGTH OF AREA                   00093200
.END1    ANOP                                                           00093300
&SYSECT  CSECT                                                          00093400
         SPACE 3                                                        00093500
.EXIT    MEND                                                           00093600
./       ADD   NAME=LEAVE                                               00093700
         MACRO                                                          00093800
&NAME    LEAVE &EQ,&RC=                                                 00093900
         GBLC  &LV,&SP                                                  00094000
&NAME    LR    2,13                                                     00094100
         L     13,4(13)                                                 00094200
         AIF   ('&RC' EQ '').L0                                         00094300
         LA    15,&RC         LOAD THE RETURN CODE                      00094400
.L0      STM   15,1,16(13)  STORE RETURN REGS                           00094500
         AIF   ('&LV' EQ '').L1  ANYTHING TO FREE?                      00094600
         FREEMAIN R,LV=&LV,SP=&SP,A=(2)  FREE THE AREA                  00094700
.L1      RETURN (14,12),T     RETURN FROM WHENCE WE CAME                00094800
         AIF   ('&EQ' NE 'EQ').L4  REGISTERS TOO?                       00094900
         COPY  REGS                                                     00095000
.L4      MEND                                                           00095100
./       ADD   NAME=MSG                                                 00095200
         MACRO                                                          00095300
&NAME    MSG   &TEXT                                                    00095400
         LCLA  &A                                                       00095500
&A       SETA  K'&TEXT-2+4  SUBTRACT QUOTES, ADD PREFIX FOUR BYTES      00095600
&NAME    DC    H'&A',H'0',C&TEXT                                        00095700
         MEND                                                           00095800
./       ADD   NAME=PDEDSNAM                                            00095900
         MACRO                                                          00096000
         PDEDSNAM                                                       00096100
*                                                                       00096200
*        DEFINE A DSECT FOR THE DSNAME PARSE DESCRIPTION                00096300
*                                                                       00096400
PDEDSNAM DSECT                                                          00096500
PDEDSN   DS    A              POINTER TO DSNAME                         00096600
PDEDSNL  DS    H              LENGTH OF DSNAME                          00096700
PDEDFLG1 DS    X              DATA SET NAME FLAGS                       00096800
PDEDFLD1 EQU   X'80'          ONE IF THE DSNAME IS PRESENT              00096900
PDEDFLQ1 EQU   X'40'          ONE IF THE DSNAME IS WITHIN QUOTES        00097000
PDEDMEM  DS    A              POINTER TO MEMBER NAME                    00097100
PDEDMEML DS    H              LENGTH OF MEMBER NAME                     00097200
PDEDFLG2 DS    X              MEMBER   NAME FLAGS                       00097300
PDEDFLD2 EQU   X'80'          ONE IF THE MEMBER IS PRESENT              00097400
PDEDPASS DS    A              POINTER TO PASSWORD                       00097500
PDEDPASL DS    H              LENGTH OF PASSWORD                        00097600
PDEDFLG3 DS    X              PASSWORD      FLAGS                       00097700
PDEDFLD3 EQU   X'80'          ONE IF THE PASSWORD IS PRESENT            00097800
PDEDCHAN DS    0F             CHAIN ADDRESS                             00097900
PDEDCHNF DS    X              CHAIN FLAGS ( X'FF' FOR END )             00098000
PDEDCHN  DS    AL3            TRUE CHAIN POINTER                        00098100
         MEND                                                           00098200
./       ADD   NAME=RCPBFRGS                                            00098300
         MACRO                                                          00098400
         RCPBFRGS &BUFPTR,&WKREGS                                       00098500
         GBLC  &RCPBFRP,&RCPBFR1,&RCPBFR2                               00098600
         AIF   ('&BUFPTR' EQ '').TGP                                    00098700
&RCPBFRP SETC  '&BUFPTR'                                                00098800
         AGO   .TWK1                                                    00098900
.TGP     AIF   ('&RCPBFRP' NE '').TWK1                                  00099000
&RCPBFRP SETC  'R1'                                                     00099100
.TWK1    AIF   ('&WKREGS(1)' EQ '').TG1                                 00099200
&RCPBFR1 SETC  '&WKREGS(1)'                                             00099300
         AGO   .TWK2                                                    00099400
.TG1     AIF   ('&RCPBFR1' NE '').TWK2                                  00099500
&RCPBFR1 SETC  'R14'                                                    00099600
.TWK2    AIF   ('&WKREGS(2)' EQ '').TG2                                 00099700
&RCPBFR2 SETC  '&WKREGS(2)'                                             00099800
         MEXIT                                                          00099900
.TG2     AIF   ('&RCPBFR2' NE '').EXIT                                  00100000
&RCPBFR2 SETC  'R15'                                                    00100100
.EXIT    MEND                                                           00100200
./       ADD   NAME=RCPBTU                                              00100300
         MACRO                                                          00100400
         RCPBTU &KEY,&NUM,&PAR                                          00100500
         LCLA  &L                                                       00100600
.*                                                                      00100700
.*  INNER MACRO FOR ALLOC, TO GENERATE TEXT UNITS ENTERED               00100800
.*  IN QUOTES                                                           00100900
.*                                                                      00101000
&L       SETA  K'&PAR-2                GET LENGTH OF TEXT UNIT          00101100
         MVI   S99TUKEY+1,&KEY         SET TEXT UNIT KEY                00101200
         MVI   S99TUNUM+1,&NUM         SET NUMBER FIELD                 00101300
         MVI   S99TULNG+1,&L           MOVE IN LENGTH                   00101400
         MVC   S99TUPAR(&L.),=C&PAR    MOVE IN TEXT UNIT                00101500
&L       SETA  &L+6                                                     00101600
         AIF   (&L/2 EQ (&L+1)/2).LOK                                   00101700
&L       SETA  &L+1                                                     00101800
.LOK     RCPDINC &L                                                     00101900
         MEND                                                           00102000
./       ADD   NAME=RCPBTU2                                             00102100
         MACRO                                                          00102200
         RCPBTU &KEY,&NUM,&PAR                                          00102300
         GBLA  &DTUPO                                                   00102400
         GBLC  &DYNP                                                    00102500
         LCLA  &L                                                       00102600
.*                                                                      00102700
.*  INNER MACRO FOR ALLOC, TO BRANCH AROUND TEXT UNIT AND               00102800
.*  CREATE TEXT UNIT                                                    00102900
.*                                                                      00103000
&L       SETA  K'&PAR+8                GET LENGTH TO BRANCH AROUND      00103100
         AIF   (&L/2 EQ (&L+1)/2).LOK  MAKE SURE LENGTH IS EVEN         00103200
&L       SETA  &L+1                                                     00103300
.LOK     BAL   R14,*+&L                BRANCH AROUND TEXT UNIT          00103400
&L       SETA  K'&PAR-2                                                 00103500
         DC    Y(&KEY,&NUM,&L),C&PAR   TEXT UNIT                        00103600
         LA    R14,0(R14)              CLEAR HIGH ORDER BYTE            00103700
         ST    R14,&DYNP.TUP+&DTUPO    STORE TEXT UNIT ADDRESS          00103800
&DTUPO   SETA  &DTUPO+4                                                 00103900
         MEND                                                           00104000
./       ADD   NAME=RCPCKID                                             00104100
         MACRO                                                          00104200
&NAME    RCPCKID              &CHECKID                                  00104300
         GBLB  &RCPECT(2),&RCPPSCB(2)                                   00104400
         GBLC  &RCPPRE                                                  00104500
         LCLC  &CHARVAR,&P                                              00104600
         LCLA  &COUNTR,&L                                               00104700
&P       SETC  '&RCPPRE'                                                00104800
&RCPPSCB(1) SETB  1                                                     00104900
&RCPECT(1)  SETB  1                                                     00105000
         EJECT                                                          00105100
         SPACE 4                                                        00105200
*********************************************************************** 00105300
***  THE USERID OF THE USER IS CHECKED. IF IT IS NOT VALID, THE    **** 00105400
***   COMMAND PRETENDS IT DOES NOT EXIST BY LINKING TO EXEC IN     **** 00105500
***   THE SAME WAY THE TMP DOES IF IT CANNOT FIND THE COMMAND.     **** 00105600
*********************************************************************** 00105700
         SPACE 3                                                        00105800
         L     R1,CPPLPSCB             LOAD ADDRESS OF PSCB             00105900
         USING PSCB,R1                 PSCB ADDRESSABILITY              00106000
.NID     ANOP                                                           00106100
&COUNTR  SETA  &COUNTR+1                                                00106200
         AIF   ('&CHECKID(&COUNTR)' EQ '').ENDID                        00106300
&CHARVAR SETC  '&CHECKID(&COUNTR)'                                      00106400
&L       SETA  K'&CHARVAR                                               00106500
         AIF   ('&CHARVAR'(1,1) EQ '''').QCID                           00106600
         CLC   PSCBUSER(&L),=C'&CHARVAR'  IS THE USERID VALID?          00106700
         BE    &P.IDOK                     YES, BRANCH OUT              00106800
         AGO   .NID                                                     00106900
.QCID    ANOP                                                           00107000
&L       SETA  &L-2                                                     00107100
         CLC   PSCBUSER(&L),=C&CHARVAR    IS THE USERID VALID?          00107200
         BE    &P.IDOK                     YES, BRANCH OUT              00107300
         AGO   .NID                                                     00107400
.ENDID   L     R1,CPPLECT              LOAD ECT ADDRESS                 00107500
         SPACE 2                                                        00107600
         USING ECT,R1                                                   00107700
         MVC   ECTPCMD,&P.EXECN        MOVE IN COMMAND NAME             00107800
         DROP  R1                      KILL ECT ADDRESSABILITY          00107900
         L     R1,CPPLCBUF             LOAD CBUF ADDRESS                00108000
         XC    2(2,R1),2(R1)           ZERO OFFSET FIELD                00108100
         L     R1,&P.CPPL              RELOAD CPPL ADDRESS              00108200
         XCTL  EPLOC=&P.EXECN                                           00108300
&P.EXECN DC    CL8'EXEC'               NAME OF EXEC PROCESSOR           00108400
&P.IDOK  DS    0H                                                       00108500
         MEND                                                           00108600
./       ADD   NAME=RCPDDN                                              00108700
         MACRO                                                          00108800
         RCPDDN &DDN                                                    00108900
         GBLC  &DYNP                                                    00109000
         SPACE 1                                                        00109100
*********************************************************************** 00109200
**   BUILD THE DDNAME TEXT UNIT                                      ** 00109300
*********************************************************************** 00109400
         AIF   ('&DDN'(K'&DDN,1) EQ '/').BTU                            00109500
         AIF   ('&DDN'(1,1) EQ '''').Q                                  00109600
         RCPSR2                                                         00109700
         AIF   ('&DDN'(1,1) EQ '(').R                                   00109800
         L     R14,&DDN                LOAD ADDRESS OF DDNAME           00109900
         LH    R2,&DDN+4               LOAD LENGTH OF DDNAME            00110000
         AGO   .STH                                                     00110100
.R       L     R14,0&DDN               LOAD ADDRESS OF DDNAME           00110200
         LH    R2,4&DDN                LOAD LENGTH OF DDNAME            00110300
.STH     STH   R2,S99TULNG             STORE DDNAME LENGTH              00110400
         BCTR  R2,0                    DECREMENT FOR EXECUTE            00110500
         EX    R2,&DYNP.MVC            MOVE DDNAME                      00110600
         MVI   S99TUKEY+1,DALDDNAM     MOVE IN DDNAME KEY               00110700
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00110800
         RCPDINC 14                                                     00110900
         MEXIT                                                          00111000
.Q       RCPBTU DALDDNAM,1,&DDN                                         00111100
         MEXIT                                                          00111200
.BTU     RCPTUBFR DALDDNAM,14,&DDN                                      00111300
         MEND                                                           00111400
./       ADD   NAME=RCPDDNRT                                            00111500
         MACRO                                                          00111600
         RCPDDNRT                                                       00111700
         SPACE 1                                                        00111800
*********************************************************************** 00111900
**    DDNAME RETURN TEXT UNIT                                        ** 00112000
*********************************************************************** 00112100
         MVI   S99TUKEY+1,DALRTDDN     SET RETURN DDNAME KEY            00112200
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00112300
         MVI   S99TULNG+1,8            SET LENGTH FIELD                 00112400
         MVC   S99TUPAR(8),=CL8' '     INITIALIZE FIELD TO BLANKS       00112500
         RCPDINC 14                                                     00112600
         MEND                                                           00112700
./       ADD   NAME=RCPDEBUG                                            00112800
         MACRO                                                          00112900
         RCPDEBUG &ON                                                   00113000
         GBLA  &RCPBGN#,&RCPSWS(10)                                     00113100
         GBLB  &RCPDBUG                                                 00113200
         GBLC  &RCPPRE,&RCPWKDS,&RCPWKCS                                00113300
         AIF   ('&ON' EQ '').TSW                                        00113400
&RCPDBUG SETB 1                                                         00113500
.TSW     AIF   (&RCPDBUG).DEBUG                                         00113600
         MEXIT                                                          00113700
.DEBUG   MNOTE *,'RCPBGN# IS &RCPBGN#'                                  00113800
         MNOTE *,'RCPSWS(1) IS &RCPSWS(1)'                              00113900
         MNOTE *,'RCPSWS(2) IS &RCPSWS(2)'                              00114000
         MNOTE *,'RCPSWS(3) IS &RCPSWS(3)'                              00114100
         MNOTE *,'RCPSWS(4) IS &RCPSWS(4)'                              00114200
         MNOTE *,'RCPSWS(5) IS &RCPSWS(5)'                              00114300
         MNOTE *,'RCPWKCS IS ''&RCPWKCS'''                              00114400
         MNOTE *,'RCPWKDS IS ''&RCPWKDS'''                              00114500
         MNOTE *,'RCPPRE IS ''&RCPPRE'''                                00114600
         MEND                                                           00114700
./       ADD   NAME=RCPDFPL                                             00114800
         MACRO                                                          00114900
         RCPDFPL                                                        00115000
         GBLC  &RCPPRE                                                  00115100
         GBLB  &RCPDFPL(2)                                              00115200
         GBLB  &RCPDFPB(2)                                              00115300
         LCLC  &P,&L,&L1                                                00115400
&P       SETC  '&RCPPRE'                                                00115500
         EJECT                                                          00115600
         AIF   (&RCPDFPL(2)).BYPDFPL                                    00115700
&RCPDFPL(2) SETB 1                                                      00115800
         IKJDFPL                                                        00115900
L#DFPL   EQU   *-DFPL                  LENGTH OF DEFAULT PARAM LIST     00116000
         IKJDFPB                                                        00116100
L#DFPB   EQU   *-DFPB                  LENGTH OF DEFAULT PARAM BLOCK    00116200
&SYSECT  CSECT                         RESUME PROGRAM CSECT             00116300
         SPACE 3                                                        00116400
.BYPDFPL RCPDS                                                          00116500
&P.DFPL  DS    CL(L#DFPL)              RESERVE SPACE FOR DFPL           00116600
&P.DFPB  DS    CL(L#DFPB)              RESERVE SPACE FOR DFPB           00116700
&P.DSNB  DS    CL48                    RESERVE SPACE FOR DSNAME BUFFER  00116800
         RCPDS                                                          00116900
         EJECT                                                          00117000
*********************************************************************** 00117100
***   THIS CODE GENERATES AN DEFAULT SERVICE ROUTINE PARAMETER LIST *** 00117200
***       AND PARAMETER BLOCK                                       *** 00117300
*********************************************************************** 00117400
         LA    R1,&P.DFPL              LOAD DFPL ADDRESS                00117500
         USING DFPL,R1                 DFPL ADDRESSABLE                 00117600
         MVC   DFPLUPT,CPPLUPT         MOVE IN ADDRESS OF UPT           00117700
         MVC   DFPLECT,CPPLECT         MOVE IN ADDRESS OF ECT           00117800
         LA    R15,&P.ECB              LOAD ADDRESS OF ATTN ECB         00117900
         ST    R15,DFPLECB             AND STORE IN DFPL                00118000
         LA    R15,&P.DFPB             LOAD DFBP ADDRESS                00118100
         ST    R15,DFPLDFPB             AND STORE IT IN DFPB            00118200
         DROP  R1                                                       00118300
         USING DFPB,R15                ADDRESS DFPB DSECT               00118400
         XC    DFPB(L#DFPB),DFPB       CLEAR DEFAULT PARAMETER BLOCK    00118500
         MVC   DFPBPSCB,CPPLPSCB       MOVE IN ADDRESS OF PSCB          00118600
         LA    R1,&P.DSNB              LOAD DSNAME BUFFER ADDRESS       00118700
         ST    R1,DFPBDSN               AND STORE IT INTO DFPB          00118800
         MVI   DFPBCODE,DFPB04          SET ENTRY CODE                  00118900
         DROP  R15                     DFPB NO LONGER ADDRESSABLE       00119000
         EJECT                                                          00119100
         MEND                                                           00119200
./       ADD   NAME=RCPDINC                                             00119300
         MACRO                                                          00119400
         RCPDINC &L1                                                    00119500
         GBLA  &DTUO,&DTUPO                                             00119600
         GBLC  &DYNP                                                    00119700
         AIF   ('&L1' EQ '').T2                                         00119800
         ST    R15,&DYNP.TUP+&DTUPO    STORE TEXT UNIT ADDRESS          00119900
         LA    R15,&L1.(R15)           BUMP TEXT UNIT PTR TO NEXT SLOT  00120000
&DTUPO   SETA  &DTUPO+4                                                 00120100
&DTUO    SETA  &DTUO+&L1                                                00120200
         MEXIT                                                          00120300
.T2      ST    R14,&DYNP.TUP+&DTUPO    STORE TEXT UNIT ADDRESS          00120400
&DTUPO   SETA  &DTUPO+4                                                 00120500
         MEND                                                           00120600
./       ADD   NAME=RCPDISP                                             00120700
         MACRO                                                          00120800
         RCPDISP &DISP                                                  00120900
         LCLA  &I                                                       00121000
         LCLB  &B(4)                                                    00121100
         AIF   ('&DISP(1)' EQ '').TD2                                   00121200
         SPACE                                                          00121300
*********************************************************************** 00121400
**     DATA SET INITIAL STATUS                                       ** 00121500
*********************************************************************** 00121600
&B(1)    SETB  ('&DISP(1)' EQ 'SHR')                                    00121700
&B(2)    SETB  ('&DISP(1)' EQ 'NEW')                                    00121800
&B(3)    SETB  ('&DISP(1)' EQ 'MOD')                                    00121900
&B(4)    SETB  ('&DISP(1)' EQ 'OLD')                                    00122000
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK1                   00122100
         MNOTE 8,'&DISP(1) IS INVALID, DISP=SHR USED'                   00122200
&B(1)    SETB  1                                                        00122300
.OK1     ANOP                                                           00122400
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)                            00122500
         MVC   S99TUKEY(8),=Y(DALSTATS,1,1,X'0&I.00')                   00122600
         RCPDINC 8                                                      00122700
.TD2     AIF   ('&DISP(2)' EQ '').TD3                                   00122800
         SPACE                                                          00122900
*********************************************************************** 00123000
**    DATA SET NORMAL DISPOSITION                                    ** 00123100
*********************************************************************** 00123200
&B(1)    SETB  ('&DISP(2)' EQ 'KEEP')                                   00123300
&B(2)    SETB  ('&DISP(2)' EQ 'DELETE')                                 00123400
&B(3)    SETB  ('&DISP(2)' EQ 'CATLG')                                  00123500
&B(4)    SETB  ('&DISP(2)' EQ 'UNCATLG')                                00123600
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK2                   00123700
         MNOTE 8,'&DISP(2) IS INVALID, DISP=(,KEEP) USED'               00123800
&B(1)    SETB  1                                                        00123900
.OK2     ANOP                                                           00124000
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)                            00124100
         MVC   S99TUKEY(8),=Y(DALNDISP,1,1,X'0&I.00')                   00124200
         RCPDINC 8                                                      00124300
.TD3     AIF   ('&DISP(3)' EQ '').EXIT                                  00124400
         SPACE                                                          00124500
*********************************************************************** 00124600
**   DATASET CONDITIONAL DISPOSITION                                 ** 00124700
*********************************************************************** 00124800
&B(1)    SETB  ('&DISP(3)' EQ 'KEEP')                                   00124900
&B(2)    SETB  ('&DISP(3)' EQ 'DELETE')                                 00125000
&B(3)    SETB  ('&DISP(3)' EQ 'CATLG')                                  00125100
&B(4)    SETB  ('&DISP(3)' EQ 'UNCATLG')                                00125200
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK3                   00125300
         MNOTE 8,'&DISP(3) IS INVALID, DISP=(,,KEEP) USED'              00125400
&B(1)    SETB  1                                                        00125500
.OK3     ANOP                                                           00125600
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)                            00125700
         MVI   S99TUKEY(8),=Y(DALCDISP,1,1,X'0&I.00')                   00125800
         RCPDINC 8                                                      00125900
.EXIT    MEND                                                           00126000
./       ADD   NAME=RCPDS                                               00126100
         MACRO                                                          00126200
         RCPDS                                                          00126300
         GBLB  &RCPDSBR                                                 00126400
         GBLC  &RCPWKDS,&RCPWKCS,&RCPDS                                 00126500
         AIF   ('&RCPDS' NE '').RESUME                                  00126600
&RCPDS   SETC  '&SYSECT'                                                00126700
         AIF   ('&RCPWKDS' EQ '').CSECT                                 00126800
&RCPWKDS DSECT                         ENTER WORKAREA DSECT             00126900
         MEXIT                                                          00127000
.CSECT   AIF   ('&RCPWKCS' EQ '').BRANCH                                00127100
&RCPWKCS CSECT                         ENTER WORKAREA CSECT             00127200
         MEXIT                                                          00127300
.RESUME  AIF   (&RCPDSBR).BRTO                                          00127400
&RCPDS   CSECT                         RESUME PROGRAM CSECT             00127500
&RCPDS   SETC  ''                                                       00127600
         MEXIT                                                          00127700
.BRANCH  ANOP                                                           00127800
&RCPDS   SETC  'RCP&SYSNDX'                                             00127900
&RCPDSBR SETB  1                                                        00128000
         B     &RCPDS                  BRANCH AROUND CONSTANTS          00128100
         MEXIT                                                          00128200
.BRTO    ANOP                                                           00128300
&RCPDS   DS    0H                                                       00128400
&RCPDSBR SETB  0                                                        00128500
&RCPDS   SETC  ''                                                       00128600
         MEND                                                           00128700
./       ADD   NAME=RCPDSECT                                            00128800
         MACRO                                                          00128900
&NAME    RCPDSECT &LTORG=YES                                            00129000
         AIF   ('&LTORG' NE 'YES').RCPDS                                00129100
*********************************************************************** 00129200
****                  LITERALS                                     **** 00129300
*********************************************************************** 00129400
         SPACE 3                                                        00129500
         LTORG                                                          00129600
         EJECT                                                          00129700
.RCPDS   RCPDS                                                          00129800
         MEND                                                           00129900
./       ADD   NAME=RCPDSN                                              00130000
         MACRO                                                          00130100
         RCPDSN &DSN,&MEM                                               00130200
         LCLC  &MEMBER                                                  00130300
         GBLC  &DYNP                                                    00130400
         SPACE                                                          00130500
*********************************************************************** 00130600
**   BUILD THE DSNAME TEXT UNIT                                      ** 00130700
*********************************************************************** 00130800
         AIF   ('&DSN'(1,1) EQ '''').Q                                  00130900
         AIF   ('&DSN'(K'&DSN,1) EQ '/').BD                             00131000
         AIF   ('&DSN'(1,1) EQ '(').REG                                 00131100
         AIF   ('&DSN'  EQ '*').TERM                                    00131200
         RCPSR2                                                         00131300
         L     R14,&DSN                LOAD ADDRESS OF DSNAME           00131400
         LH    R2,&DSN+4               LOAD LENGTH OF DSNAME            00131500
.STH     STH   R2,S99TULNG             STORE DSNAME LENGTH              00131600
         BCTR  R2,0                    DECREMENT FOR EXECUTE            00131700
         EX    R2,&DYNP.MVC            MOVE DSNAME                      00131800
         MVI   S99TUKEY+1,DALDSNAM     MOVE IN DSNAME KEY               00131900
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00132000
         RCPDINC 50                                                     00132100
         AGO   .TMEMBER                                                 00132200
.REG     L     R14,0&DSN               LOAD ADDRESS OF DSNAME           00132300
         RCPSR2                                                         00132400
         LH    R2,4&DSN                LOAD LENGTH OF DSNAME            00132500
         AGO   .STH                                                     00132600
.TERM    MVI   S99TUKEY+1,DALTERM                                       00132700
         RCPDINC 4                                                      00132800
         MEXIT                                                          00132900
.BD      RCPTUBFR DALDSNAM,50,&DSN                                      00133000
         AGO   .TMEMBER                                                 00133100
.Q       RCPBTU DALDSNAM,1,&DSN                                         00133200
.TMEMBER AIF   ('&MEM' EQ '').EXIT                                      00133300
         SPACE                                                          00133400
*********************************************************************** 00133500
**   BUILD THE MEMBER NAME TEXT UNIT                                 ** 00133600
*********************************************************************** 00133700
&MEMBER  SETC  '&MEM'                                                   00133800
         AIF   ('&MEM' NE '*').MOK                                      00133900
         AIF   ('&DSN'(1,1) NE '''').MAST                               00134000
         MNOTE 8,'MEMBER=* INVALID WITH QUOTED DSNAME'                  00134100
         MEXIT                                                          00134200
.MAST    ANOP                                                           00134300
&MEMBER  SETC  '8+&DSN'                                                 00134400
.MOK     ANOP                                                           00134500
         AIF   ('&MEMBER'(K'&MEMBER,1) EQ '/').BM                       00134600
         RCPSR2                                                         00134700
         AIF   ('&MEMBER'(1,1) EQ '(').RM                               00134800
         LH    R2,4+&MEMBER            LOAD LENGTH OF MEMBER NAME       00134900
         LTR   R2,R2                   TEST FOR ZERO                    00135000
         BZ    *+30                    IF NO MEMBER, SKIP               00135100
         L     R14,&MEMBER             LOAD ADDRESS OF MEMBER           00135200
         AGO   .STHM                                                    00135300
.RM      LH    R2,4&MEMBER             LOAD LENGTH OF MEMBER            00135400
         LTR   R2,R2                   AND TEST FOR ZERO                00135500
         BZ    *+30                    IF NO MEMBER, SKIP               00135600
         L     R14,0&MEMBER            LOAD ADDRESS OF MEMBER           00135700
.STHM    STH   R2,S99TULNG             STORE LENGTH OF MEMBER           00135800
         BCTR  R2,0                    DECREMENT FOR EXECUTE            00135900
         EX    R2,&DYNP.MVC            MOVE IN MEMBER NAME              00136000
         MVI   S99TUKEY+1,DALMEMBR     MOVE IN MEMBER KEY               00136100
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00136200
         RCPDINC 14                                                     00136300
         MEXIT                                                          00136400
.BM      RCPTUBFR DALMEMBR,14,&MEMBER                                   00136500
         MEXIT                                                          00136600
.QM      RCPBTU DALMEMBR,1,&MEMBER                                      00136700
.EXIT    MEND                                                           00136800
./       ADD   NAME=RCPDSNPD                                            00136900
         MACRO                                                          00137000
         RCPDSNPD &PDE                                                  00137100
         AIF   ('&PDE'(1,1) EQ '(').RPDE                                00137200
         RCPDSN &PDE,8+&PDE                                             00137300
         RCPPSWD 16+&PDE                                                00137400
         MEXIT                                                          00137500
.RPDE    RCPDSN &PDE,8&PDE                                              00137600
         RCPPSWD 16(&PDE)                                               00137700
         MEND                                                           00137800
./       ADD   NAME=RCPDSNRT                                            00137900
         MACRO                                                          00138000
         RCPDSNRT                                                       00138100
         SPACE                                                          00138200
*********************************************************************** 00138300
**    DSNAME RETURN TEXT UNIT                                        ** 00138400
*********************************************************************** 00138500
         MVI   S99TUKEY+1,DALRTDSN     SET RETURN DSNAME KEY            00138600
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00138700
         MVI   S99TULNG+1,44           SET LENGTH FIELD                 00138800
         RCPDINC 50                                                     00138900
         MEND                                                           00139000
./       ADD   NAME=RCPDSRGR                                            00139100
         MACRO                                                          00139200
         RCPDSRGR                                                       00139300
         SPACE                                                          00139400
*********************************************************************** 00139500
**    DSORG RETURN TEXT UNIT                                         ** 00139600
*********************************************************************** 00139700
         MVI   S99TUKEY+1,DALRTORG     SET RETURN DSORG KEY             00139800
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00139900
         MVI   S99TULNG+1,2            SET LENGTH FIELD                 00140000
         XC    S99TUPAR(2),S99TUPAR    INITIALIZE FIELD TO ZERO         00140100
         RCPDINC 8                                                      00140200
         MEND                                                           00140300
./       ADD   NAME=RCPDUMMY                                            00140400
         MACRO                                                          00140500
         RCPDUMMY &DUMMY                                                00140600
         SPACE                                                          00140700
*********************************************************************** 00140800
**      DUMMY DATASET TEXT UNIT                                      ** 00140900
*********************************************************************** 00141000
         MVI   S99TUPAR+1,DALDUMMY     MOVE IN DUMMY DS TEXT UNIT KEY   00141100
         RCPDINC 4                                                      00141200
         MEND                                                           00141300
./       ADD   NAME=RCPENDD                                             00141400
         MACRO                                                          00141500
&NAME    RCPENDD                                                        00141600
         GBLB  &RCPECT(2),&RCPUPT(2),&RCPPSCB(2),&RCPS99(2)             00141700
         GBLC  &RCPPRE,&RCPWKDS,&RCPDS                                  00141800
         LCLC  &P,&CS                                                   00141900
&CS      SETC  '&RCPDS'                PROGRAM CSECT                    00142000
         AIF   (NOT &RCPS99(1)).TDS                                     00142100
         DYNSPACE                                                       00142200
.TDS     AIF   ('&RCPWKDS' EQ '').RCPDS                                 00142300
         DS    0D                      ALIGN TO DOUBLEWORD              00142400
&P       SETC  '&RCPPRE'                                                00142500
&P.WKLEN EQU   *-&RCPWKDS              LENGTH OF WORK AREA              00142600
.RCPDS   RCPDS                                                          00142700
         EJECT                                                          00142800
         AIF   (NOT &RCPECT(1) OR &RCPECT(2)).TRYUPT                    00142900
         IKJECT                                                         00143000
&CS      CSECT                         REENTER MAIN CSECT               00143100
         EJECT                                                          00143200
&RCPECT(2)     SETB           1                                         00143300
.TRYUPT  AIF   (NOT &RCPUPT(1) OR &RCPUPT(2)).TRYPSCB                   00143400
         IKJUPT                                                         00143500
&CS      CSECT                         REENTER MAIN CSECT               00143600
         EJECT                                                          00143700
&RCPUPT(2) SETB  1                                                      00143800
.TRYPSCB AIF   (NOT &RCPPSCB(1) OR &RCPPSCB(2)).TRYS99                  00143900
         IKJPSCB                                                        00144000
&CS      CSECT                         REENTER MAIN CSECT               00144100
         EJECT                                                          00144200
&RCPPSCB(2) SETB  1                                                     00144300
.TRYS99  AIF   (NOT &RCPS99(1) OR &RCPS99(2)).TRYREST                   00144400
         IEFZB4D0                                                       00144500
         EJECT                                                          00144600
         IEFZB4D2                                                       00144700
&CS      CSECT                         REENTER MAIN CSECT               00144800
         EJECT                                                          00144900
&RCPS99(2) SETB  1                                                      00145000
.TRYREST MEND                                                           00145100
./       ADD   NAME=RCPFDDN                                             00145200
         MACRO                                                          00145300
         RCPFDDN &DDN                                                   00145400
         GBLC &DYNP                                                     00145500
         SPACE                                                          00145600
*********************************************************************** 00145700
**        FREE DDNAME TEXT UNIT                                      ** 00145800
*********************************************************************** 00145900
         AIF   ('&DDN'(1,1) EQ '''').Q                                  00146000
         AIF   ('&DDN'(K'&DDN,1) EQ '/').B                              00146100
         RCPSR2                                                         00146200
         AIF   ('&DDN'(1,1) EQ '(').R                                   00146300
         L     R14,&DDN                LOAD ADDRESS OF DDNAME           00146400
         LH    R2,&DDN+4               LOAD LENGTH OF DDNAME            00146500
         AGO   .STH                                                     00146600
.R       L     R14,0&DDN               LOAD ADDRESS OF DDNAME           00146700
         LH    R2,4&DDN                LOAD LENGTH OF DDNAME            00146800
.STH     STH   R2,S99TULNG             STORE DDNAME LENGTH              00146900
         BCTR  R2,0                    DECREMENT FOR EXECUTE            00147000
         EX    R2,&DYNP.MVC            MOVE DDNAME                      00147100
         MVI   S99TUKEY+1,DUNDDNAM     MOVE IN DDNAME KEY               00147200
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00147300
         RCPDINC 14                                                     00147400
         MEXIT                                                          00147500
.Q       RCPBTU DUNDDNAM,1,&DDN                                         00147600
         MEXIT                                                          00147700
.B       RCPTUBFR DUNDDNAM,14,&DDN                                      00147800
         MEND                                                           00147900
./       ADD   NAME=RCPFDISP                                            00148000
         MACRO                                                          00148100
         RCPFDISP &DISP                                                 00148200
         LCLB  &B(4)                                                    00148300
         LCLA  &I                                                       00148400
         SPACE                                                          00148500
*********************************************************************** 00148600
**       OVERRIDING DISPOSITION                                      ** 00148700
*********************************************************************** 00148800
&B(1)    SETB  ('&DISP' EQ 'KEEP')                                      00148900
&B(2)    SETB  ('&DISP' EQ 'DELETE')                                    00149000
&B(3)    SETB  ('&DISP' EQ 'CATLG')                                     00149100
&B(4)    SETB  ('&DISP' EQ 'UNCATLG')                                   00149200
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK3                   00149300
         MNOTE 8,'&DISP IS INVALID, DISP=KEEP USED'                     00149400
&B(1)    SETB  1                                                        00149500
.OK3     ANOP                                                           00149600
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)                            00149700
         MVC   S99TUKEY(8),=Y(DUNOVDSP,1,1,X'0&I.00')                   00149800
         RCPDINC 8                                                      00149900
.EXIT    MEND                                                           00150000
./       ADD   NAME=RCPFDSN                                             00150100
         MACRO                                                          00150200
         RCPFDSN &DSN,&MEM                                              00150300
         LCLC  &MEMBER                                                  00150400
         GBLC  &DYNP                                                    00150500
         SPACE                                                          00150600
*********************************************************************** 00150700
**      FREE DATA SET TEXT UNIT                                      ** 00150800
*********************************************************************** 00150900
         AIF   ('&DSN'(1,1) EQ '''').Q                                  00151000
         AIF   ('&DSN'(K'&DSN,1) EQ '/').BD                             00151100
         AIF   ('&DSN'(1,1) EQ '(').REG                                 00151200
         RCPSR2                                                         00151300
         L     R14,&DSN                LOAD ADDRESS OF DSNAME           00151400
         LH    R2,&DSN+4               LOAD LENGTH OF DSNAME            00151500
.STH     STH   R2,S99TULNG             STORE DSNAME LENGTH              00151600
         BCTR  R2,0                    DECREMENT FOR EXECUTE            00151700
         EX    R2,&DYNP.MVC            MOVE DSNAME                      00151800
         MVI   S99TUKEY+1,DUNDSNAM     MOVE IN DSNAME KEY               00151900
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00152000
         RCPDINC 50                                                     00152100
         AGO   .TMEMBER                                                 00152200
.REG     L     R14,0&DSN               LOAD ADDRESS OF DSNAME           00152300
         RCPSR2                                                         00152400
         LH    R2,4&DSN                LOAD LENGTH OF DSNAME            00152500
         AGO   .STH                                                     00152600
.BD      RCPTUBFR DUNDSNAM,50,&DSN                                      00152700
         AGO   .TMEMBER                                                 00152800
.Q       RCPBTU DUNDSNAM,1,&DSN                                         00152900
.TMEMBER AIF   ('&MEM' EQ '').EXIT                                      00153000
         SPACE                                                          00153100
*********************************************************************** 00153200
**       FREE MEMBER NAME TEXT UNIT                                  ** 00153300
*********************************************************************** 00153400
&MEMBER  SETC  '&MEM'                                                   00153500
         AIF   ('&MEM' NE '*').MOK                                      00153600
         AIF   ('&DSN'(1,1) NE '''').MAST                               00153700
         MNOTE 8,'MEMBER=* INVALID WITH QUOTED DSNAME'                  00153800
         MEXIT                                                          00153900
.MAST    ANOP                                                           00154000
&MEMBER  SETC  '8+&DSN'                                                 00154100
.MOK     ANOP                                                           00154200
         AIF   ('&MEMBER'(K'&MEMBER,1) EQ '/').BM                       00154300
         RCPSR2                                                         00154400
         AIF   ('&MEMBER'(1,1) EQ '(').RM                               00154500
         LH    R2,4+&MEMBER            LOAD LENGTH OF MEMBER NAME       00154600
         LTR   R2,R2                   TEST FOR ZERO                    00154700
         BZ    *+30                    IF NO MEMBER, SKIP               00154800
         L     R14,&MEMBER             LOAD ADDRESS OF MEMBER           00154900
         AGO   .STHM                                                    00155000
.RM      LH    R2,4&MEMBER             LOAD LENGTH OF MEMBER            00155100
         LTR   R2,R2                   AND TEST FOR ZERO                00155200
         BZ    *+30                    IF NO MEMBER, SKIP               00155300
         L     R14,0&MEMBER            LOAD ADDRESS OF MEMBER           00155400
.STHM    STH   R2,S99TULNG             STORE LENGTH OF MEMBER           00155500
         BCTR  R2,0                    DECREMENT FOR EXECUTE            00155600
         EX    R2,&DYNP.MVC            MOVE IN MEMBER NAME              00155700
         MVI   S99TUKEY+1,DUNMEMBR     MOVE IN MEMBER KEY               00155800
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00155900
         RCPDINC 14                                                     00156000
         MEXIT                                                          00156100
.BM      RCPTUBFR DUNMEMBR,14,&MEMBER                                   00156200
         MEXIT                                                          00156300
.QM      RCPBTU DUNMEMBR,1,&MEMBER                                      00156400
.EXIT    MEND                                                           00156500
./       ADD   NAME=RCPFHOLD                                            00156600
         MACRO                                                          00156700
         RCPFHOLD &H                                                    00156800
         AIF   ('&H' EQ 'YES').YES                                      00156900
         AIF   ('&H' EQ 'NO').NO                                        00157000
         MNOTE 4,'HOLD PARMETER VALUE INCORRECT - IGNORED'              00157100
         MEXIT                                                          00157200
.YES     ANOP                                                           00157300
         SPACE 1                                                        00157400
*********************************************************************** 00157500
**       OVERIDING SYSOUT HOLD TEXT UNIT                             ** 00157600
*********************************************************************** 00157700
         SPACE 1                                                        00157800
         MVI   S99TUKEY+1,DUNOVSHQ MOVE IN TEXT UNIT KEY                00157900
         RCPDINC 4                                                      00158000
         MEXIT                                                          00158100
.NO      ANOP                                                           00158200
         SPACE 1                                                        00158300
*********************************************************************** 00158400
**       OVERIDING SYSOUT NO HOLD TEXT UNIT                          ** 00158500
*********************************************************************** 00158600
         SPACE 1                                                        00158700
         MVI   S99TUKEY+1,DUNOVSHQ MOVE IN TEXT UNIT KEY                00158800
         RCPDINC 4                                                      00158900
         MEND                                                           00159000
./       ADD   NAME=RCPFORUS                                            00159100
         MACRO - TO SET UP SVC 99 TEXT UNIT 'FOR USER'                  00159200
         RCPFORUS &T                                                    00159300
         SPACE 1                                                        00159400
*********************************************************************** 00159500
**       'FOR USER' TEXT UNIT                                        ** 00159600
*********************************************************************** 00159700
         RCPVCHAR 0,5,&T,N=X'7701'                                      00159800
         MEND                                                           00159900
./       ADD   NAME=RCPFREE                                             00160000
         MACRO                                                          00160100
         RCPFREE &FREE                                                  00160200
         SPACE                                                          00160300
*********************************************************************** 00160400
**      UNALLOC AT CLOSE TEXT UNIT                                   ** 00160500
*********************************************************************** 00160600
         MVI   S99TUPAR+1,DALCLOSE     MOVE IN CLOSE TEXT UNIT KEY      00160700
         RCPDINC 4                                                      00160800
         MEND                                                           00160900
./       ADD   NAME=RCPIOPL                                             00161000
         MACRO                                                          00161100
&NAME    RCPIOPL                                                        00161200
         GBLC  &RCPPRE                                                  00161300
         GBLB  &RCPIOPL(2)                                              00161400
         GBLB  &RCPSTPB(2),&RCPPTPB(2),&RCPPGPB(2),&RCPGTPB(2)          00161500
         LCLC  &P,&L,&L1                                                00161600
&P       SETC  '&RCPPRE'                                                00161700
         EJECT                                                          00161800
         AIF   (&RCPIOPL(2)).BYPIOPL                                    00161900
&RCPIOPL(2) SETB 1                                                      00162000
         IKJIOPL                                                        00162100
L#IOPL   EQU   *-IOPL                  LENGTH OF IO PARAM LIST          00162200
&SYSECT  CSECT                         RESUME PROGRAM CSECT             00162300
         SPACE 3                                                        00162400
.BYPIOPL RCPDS                                                          00162500
&P.IOPL  DS    CL(L#IOPL)              RESERVE SPACE FOR IOPL           00162600
         RCPDS                                                          00162700
         SPACE 5                                                        00162800
*********************************************************************** 00162900
***   THIS CODE GENERATES AN I/O SERVICE ROUTINE PARAMETER LIST     *** 00163000
*********************************************************************** 00163100
         LA    R1,&P.IOPL              LOAD IOPL ADDRESS                00163200
         USING IOPL,R1                 IOPL ADDRESSABLE                 00163300
         MVC   IOPLUPT,CPPLUPT         MOVE IN ADDRESS OF UPT           00163400
         MVC   IOPLECT,CPPLECT         MOVE IN ADDRESS OF ECT           00163500
         LA    R15,&P.ECB              LOAD ADDRESS OF ATTN ECB         00163600
         ST    R15,IOPLECB             AND STORE IN IOPL                00163700
         DROP  R1                                                       00163800
  AIF (&RCPSTPB(1) OR &RCPGTPB(1) OR &RCPPGPB(1) OR &RCPPTPB(1)).I      00163900
         MEXIT                                                          00164000
.I       EJECT                                                          00164100
         AIF   (NOT &RCPSTPB(1) OR &RCPSTPB(2)).TPT                     00164200
         IKJSTPB                                                        00164300
&RCPSTPB(2) SETB 1                                                      00164400
L#STPB   EQU   *-STPB         LENGTH OF STPB                            00164500
&SYSECT  CSECT                                                          00164600
.TPT     AIF   (NOT &RCPPTPB(1) OR &RCPPTPB(2)).TGT                     00164700
         IKJPTPB                                                        00164800
&RCPPTPB(2) SETB 1                                                      00164900
L#PTPB   EQU   *-PTPB         LENGTH OF PTPB                            00165000
&SYSECT  CSECT                                                          00165100
.TGT     AIF   (NOT &RCPGTPB(1) OR &RCPGTPB(2)).TPG                     00165200
         IKJGTPB                                                        00165300
&RCPGTPB(2) SETB 1                                                      00165400
L#GTPB   EQU   *-GTPB         LENGTH OF GTPB                            00165500
&SYSECT  CSECT                                                          00165600
.TPG     AIF   (NOT &RCPPGPB(1) OR &RCPPGPB(2)).STO                     00165700
         IKJPGPB                                                        00165800
&RCPPGPB(2) SETB 1                                                      00165900
L#PGPB   EQU   *-PGPB         LENGTH OF PGPB                            00166000
&SYSECT  CSECT                                                          00166100
.STO     SPACE 3                                                        00166200
&L       SETC  ''                                                       00166300
         RCPDS                                                          00166400
         AIF   (NOT &RCPSTPB(1)).XPT                                    00166500
&P.STPB  DS    CL(L#STPB)              RESERVE SPACE FOR STPB           00166600
&L       SETC  '&L.+L#STPB'                                             00166700
.XPT     AIF   (NOT &RCPPTPB(1)).XGT                                    00166800
&P.PTPB  DS    CL(L#PTPB)              RESERVE SPACE FOR PTPB           00166900
&L       SETC  '&L.+L#PTPB'                                             00167000
.XGT     AIF   (NOT &RCPGTPB(1)).XPG                                    00167100
&P.GTPB  DS    CL(L#GTPB)              RESERVE SPACE FOR GTPB           00167200
&L       SETC  '&L.+L#GTPB'                                             00167300
.XPG     AIF   (NOT &RCPPGPB(1)).XC                                     00167400
&P.PGPB  DS    CL(L#PGPB)              RESERVE SPACE FOR PGPB           00167500
&L       SETC  '&L.+L#PGPB'                                             00167600
.XC      RCPDS                                                          00167700
&L1      SETC  '&L'(2,K'&L-1)                                           00167800
&L       SETC  '&P'.'&L1'(3,4)                                          00167900
         XC    &L.(&L1.),&L            CLEAR IOPB AREA                  00168000
         MEND                                                           00168100
./       ADD   NAME=RCPLINK                                             00168200
         MACRO                                                          00168300
&NAME    RCPLINK &MODULE                                                00168400
         LCLC  &OFFSET,&C                                               00168500
         AIF   ('&MODULE' EQ '').ERROR                                  00168600
         AIF   ('&MODULE' NE 'IKJPARS').T1                              00168700
&OFFSET  SETC  '524'                                                    00168800
         AGO   .START                                                   00168900
.T1      AIF   ('&MODULE' NE 'IKJDAIR').T2                              00169000
&OFFSET  SETC  '732'                                                    00169100
         AGO   .START                                                   00169200
.T2      AIF   ('&MODULE' NE 'IKJEHDEF').T3                             00169300
&OFFSET  SETC  '736'                                                    00169400
         AGO   .START                                                   00169500
.T3      AIF   ('&MODULE' NE 'IKJEHCIR').T4                             00169600
&OFFSET  SETC  '740'                                                    00169700
         AGO   .START                                                   00169800
.T4      AIF   ('&MODULE' NE 'IKJPUTL').T5                              00169900
&OFFSET  SETC  '444'                                                    00170000
         AGO   .START                                                   00170100
.T5      AIF   ('&MODULE' NE 'IKJGETL').T6                              00170200
&OFFSET  SETC  '348'                                                    00170300
         AGO   .START                                                   00170400
.T6      AIF   ('&MODULE' NE 'IKJSCAN').T7                              00170500
&OFFSET  SETC  '480'                                                    00170600
         AGO   .START                                                   00170700
.T7      AIF   ('&MODULE' NE 'IKJPTGT').T8                              00170800
&OFFSET  SETC  '464'                                                    00170900
         AGO   .START                                                   00171000
.T8      AIF   ('&MODULE' NE 'IKJSTCK').T9                              00171100
&OFFSET  SETC  '472'                                                    00171200
         AGO   .START                                                   00171300
.T9      ANOP                                                           00171400
&NAME    DS    0H                                                       00171500
*                                                                       00171600
 MNOTE *,' EP OF &MODULE. NOT IN CVT. STANDARD LINK USED'               00171700
*                                                                       00171800
         AGO   .LINK                                                    00171900
.START   ANOP                                                           00172000
&NAME    L     R15,16                  LOAD CVT ADDRESS                 00172100
         L     R15,&OFFSET.(R15)       LOAD MODULE ADDRESS              00172200
         LTR   R15,R15                 IS MODULE ADDRESS THERE?         00172300
&C       SETC  'RCP&SYSNDX'                                             00172400
         BNM   &C.L                     IF NOT, BRANCH TO LINK          00172500
         BALR  R14,R15                  ELSE BALR TO IT                 00172600
         B     &C.B                      AND BYPASS LINK                00172700
&C.L     LINK  EP=&MODULE                                               00172800
&C.B     DS    0H                      BRANCHED TO IF LINK BYPASSED     00172900
         MEXIT                                                          00173000
.LINK    ANOP                                                           00173100
&NAME    LINK  EP=&MODULE                                               00173200
         MEXIT                                                          00173300
.ERROR   MNOTE 4,'NO MODULE NAME SPECIFIED'                             00173400
         MEND                                                           00173500
./       ADD   NAME=RCPLOAD                                             00173600
         MACRO                                                          00173700
&NAME    RCPLOAD &MOD,&EP1                                              00173800
         GBLC  &RCPPTEP,&RCPGTEP,&RCPPGEP                               00173900
         GBLC  &RCPDFEP,&RCPSTEP,&RCPPREP                               00174000
         GBLC  &RCPPRE                                                  00174100
         LCLA  &I,&J                                                    00174200
         LCLB  &EPXISTS                                                 00174300
         LCLC  &OFFSET,&C,&EP,&MODULE                                   00174400
&EP      SETC  '&EP1'                                                   00174500
&MODULE  SETC  '&MOD'                                                   00174600
         AIF   ('&MODULE' EQ '').ERROR                                  00174700
         AIF   ('&MODULE'(K'&MOD,1) NE ')').NOBR                        00174800
&I       SETA  K'&MOD                                                   00174900
.LOOP    ANOP                                                           00175000
&I       SETA  &I-1                                                     00175100
         AIF   (&I LT 2).NOLB                                           00175200
         AIF   ('&MOD'(&I,1) NE '(').LOOP                               00175300
&MODULE  SETC  '&MOD'(1,&I-1)                                           00175400
&J       SETA  K'&MOD-1-&I                                              00175500
&EP      SETC  '&MOD'(&I+1,&J)                                          00175600
         RCPDS                                                          00175700
&EP      DS    F                       TO STORE MODULE ADDRESS          00175800
         RCPDS                                                          00175900
.NOBR    ANOP                                                           00176000
&EPXISTS  SETB  ('&EP' NE '')                                           00176100
         AIF   ('&MODULE' NE 'IKJPARS').T1                              00176200
&OFFSET  SETC  '524'                                                    00176300
&RCPPREP SETC '&EP'                                                     00176400
         AIF   (&EPXISTS).START                                         00176500
         RCPDS                                                          00176600
&RCPPREP SETC '&RCPPRE.PREP'                                            00176700
&EP      SETC  '&RCPPREP'                                               00176800
&RCPPREP DS    F                       TO HOLD ADDRESS OF IKJPARS       00176900
         RCPDS                                                          00177000
         AGO   .START                                                   00177100
.T1      AIF   ('&MODULE' NE 'IKJDAIR').T2                              00177200
&OFFSET  SETC  '732'                                                    00177300
         AGO   .START                                                   00177400
.T2      AIF   ('&MODULE' NE 'IKJEHDEF').T3                             00177500
&RCPDFEP SETC  '&EP'                                                    00177600
&OFFSET  SETC  '736'                                                    00177700
         AIF   (&EPXISTS).START                                         00177800
&RCPDFEP SETC  '&RCPPRE.DFEP'                                           00177900
         RCPDS                                                          00178000
&RCPDFEP DS    F                       ADDR OF DEFAULT SERVICE ROUTINE  00178100
         RCPDS                                                          00178200
&EP      SETC  '&RCPDFEP'                                               00178300
         AGO   .START                                                   00178400
.T3      AIF   ('&MODULE' NE 'IKJEHCIR').T4                             00178500
&OFFSET  SETC  '740'                                                    00178600
         AGO   .START                                                   00178700
.T4      AIF   ('&MODULE' NE 'IKJPUTL').T5                              00178800
&RCPPTEP SETC  '&EP'                                                    00178900
&OFFSET  SETC  '444'                                                    00179000
         AIF   (&EPXISTS).START                                         00179100
&RCPPTEP SETC  '&RCPPRE.PTEP'                                           00179200
&EP      SETC  '&RCPPTEP'                                               00179300
         RCPDS                                                          00179400
&RCPPTEP DS    F                       ADDR OF PUTLINE ROUTINE          00179500
         RCPDS                                                          00179600
         AGO   .START                                                   00179700
.T5      AIF   ('&MODULE' NE 'IKJGETL').T6                              00179800
&RCPGTEP SETC  '&EP'                                                    00179900
&OFFSET  SETC  '348'                                                    00180000
         AIF   (&EPXISTS).START                                         00180100
&RCPGTEP SETC  '&RCPPRE.GTEP'                                           00180200
&EP      SETC  '&RCPGTEP'                                               00180300
         RCPDS                                                          00180400
&RCPGTEP DS    F                       ADDR OF GETLINE ROUTINE          00180500
         RCPDS                                                          00180600
         AGO   .START                                                   00180700
.T6      AIF   ('&MODULE' NE 'IKJSCAN').T7                              00180800
&OFFSET  SETC  '480'                                                    00180900
         AGO   .START                                                   00181000
.T7      AIF   ('&MODULE' NE 'IKJPTGT').T8                              00181100
&RCPPGEP SETC  '&EP'                                                    00181200
&OFFSET  SETC  '464'                                                    00181300
         AIF   (&EPXISTS).START                                         00181400
&RCPPGEP SETC  '&RCPPRE.PGEP'                                           00181500
&EP      SETC  '&RCPPGEP'                                               00181600
         RCPDS                                                          00181700
&RCPPGEP DS    F                       ADDR OF PUTGET ROUTINE           00181800
         RCPDS                                                          00181900
         AGO   .START                                                   00182000
.T8      AIF   ('&MODULE' NE 'IKJSTCK').T9                              00182100
&RCPSTEP SETC  '&EP'                                                    00182200
&OFFSET  SETC  '472'                                                    00182300
         AIF   (&EPXISTS).START                                         00182400
&RCPSTEP SETC  '&RCPPRE.STEP'                                           00182500
&EP      SETC  '&RCPSTEP'                                               00182600
         RCPDS                                                          00182700
&RCPSTEP DS    F                       ADDR OF STACK ROUTINE            00182800
         RCPDS                                                          00182900
         AGO   .START                                                   00183000
.T9      ANOP                                                           00183100
&NAME    DS    0H                                                       00183200
*                                                                       00183300
 MNOTE *,' EP OF &MODULE. NOT IN CVT. STANDARD LOAD USED'               00183400
*                                                                       00183500
         AGO   .LOAD                                                    00183600
.START   ANOP                                                           00183700
&NAME    L     R15,16                  LOAD CVT ADDRESS                 00183800
         L     R0,&OFFSET.(R15)        LOAD MODULE ADDRESS              00183900
         LTR   R0,R0                   IS MODULE LOADED?                00184000
&C       SETC  'RCP&SYSNDX'                                             00184100
         BM    &C                      IF SO, BYPASS LOAD MACRO         00184200
.LOAD    LOAD EP=&MODULE.                                               00184300
         AIF   ('&EP' EQ '').EPERR                                      00184400
&C       ST    R0,&EP                  STORE ENTRY POINT ADDRESS        00184500
         MEXIT                                                          00184600
.EPERR   MNOTE 4,'EP RETURN FIELD NOT SPECIFIED'                        00184700
         MEXIT                                                          00184800
.ERROR   MNOTE 4,'NO MODULE NAME SPECIFIED'                             00184900
         MEXIT                                                          00185000
.NOLB    MNOTE 4,'INVALID MODULE NAME ''&MOD'''                         00185100
         MEND                                                           00185200
./       ADD   NAME=RCPLOCSW                                            00185300
*23456789*12345*78921234567893123456789*                                00185400
         MACRO                                                          00185500
         RCPLOCSW &SW                                                   00185600
.********************************************************************   00185700
.*                                                                  *   00185800
.*       INNER MACRO USED BY GOIF, SET, RESET AND FLIP.             *   00185900
.*       THE PARM PASSED IS THE SWITCH OR LIST OF SWITCHES.         *   00186000
.*       RCPLOCSW SCANS THE ARRAYS SET UP BY DCLSW TO SEE IF THE    *   00186100
.*       SWITCH BIT NAMES WERE DECLARED, AND IF A LIST WAS PASSED,  *   00186200
.*       WHETHER ALL THE SWITCH BITS BELONG TO THE SAME BYTE.       *   00186300
.*       RCPLOCSW PASSES BACH THE SWITCH BYTE NAME IN GLOBAL SETC   *   00186400
.*       VARIABLE &RCPDSW1 AND THE SWITCH BIT NAME IN GLOBAL SETC   *   00186500
.*       &RCPDSW2. IF A LIST OF SWITCHES WAS PASSED, &RCPDSW2       *   00186600
.*       CONTAINS THE SWITCH NAMES SEPARATED BY PLUS SIGNS.         *   00186700
.*       IF THE FIRST OR ONLY SWITCH WAS NOT FOUND, &RCPDSW1 IS SET *   00186800
.*       TO NULL. IF A LIST OF SWITCHES IS PASSED AND ANY SWITCH IS *   00186900
.*       NOT DECLARED IN THE SAME SWITCH BYTE AS THE FIRST, AN MNOTE*   00187000
.*       IS ISSUED WARNING OF POSSIBLE ERROR, BUT &RCPDSW1 IS SET   *   00187100
.*       TO THE NAME OF THE SWITCH BYTE CONTAINING THE FIRST SWITCH *   00187200
.*       BIT IN THE LIST.                                           *   00187300
.*                                                                  *   00187400
.********************************************************************   00187500
         GBLA  &RCPDSW#,&RCPGSW#       COUNTER FOR DECLARED SWITCHES    00187600
         GBLA  &RCPDSW0                NO OF SWS FOUND BY RCPLOCSW      00187700
         GBLB  &RCPDSWD(99)            DEFER DECLARE INDICATORS         00187800
         GBLB  &RCPDSW3(20)   INVERT INDICATOR                          00187900
         GBLC  &RCPDSWN(99)            SWITCH BYTE NAMES                00188000
         GBLC  &RCPDSWB(800)           SWITCH BIT NAMES                 00188100
         GBLC  &RCPGSWN(99)            GENERIC SWITCH BYTE NAMES        00188200
         GBLC  &RCPGSWB(99)            GENERIC SWITCH BIT PREFIXES      00188300
         GBLC  &RCPDSW1(20)            SWITCH BYTE NAMES                00188400
         GBLC  &RCPDSW2(20)            SWITCH BIT NAME(S)               00188500
         LCLA  &I,&J,&K,&L,&M,&N                                        00188600
         LCLB  &NOT                                                     00188700
         LCLC  &C,&SW1,&SW2                                             00188800
&RCPDSW0 SETA  0                       INITIALIZE                       00188900
&N       SETA  N'&SW                   NO OF SWITCHES ENTERED           00189000
&J       SETA  &RCPDSW#*8+8            INDEX TO LAST DECLARED SW BIT    00189100
.LOOP1   AIF   (&M GE &N).EXIT        LOOP FOR EACH SW                  00189200
&M       SETA  &M+1                                                     00189300
&SW2     SETC  '&SW(&M)'               SWITCH TO SEARCH FOR             00189400
         AIF   ('&SW2' EQ '').LOOP1    SKIP IF NULL                     00189500
&I       SETA  8                       INDEX TO FIRST DECLARED SW - 1   00189600
&NOT     SETB  0                                                        00189700
         AIF   ('&SW2'(1,1) NE ' ' AND '&SW2'(1,1) NE '-').TNOT2        00189800
&SW2     SETC  '&SW2'(2,K'&SW2-1)       REMOVE NOT SIGN                 00189900
&NOT     SETB  1                       INDICATE INVERT FUNCTION         00190000
         AGO   .LOOP1A                 CONTINUE                         00190100
.TNOT2   AIF   (K'&SW2 LT 5).LOOP1A    CHECK LENGTH                     00190200
         AIF   ('&SW2'(1,4) NE 'NOT-').LOOP1A  WAS SWITCH INVERTED?     00190300
&SW2     SETC  '&SW2'(5,K'&SW2-4)      STRIP OFF 'NOT-'                 00190400
&NOT     SETB  1                       INDICATE INVERTED                00190500
.LOOP1A  AIF   (&I GE &J).TGEN         SEARCH NAME ARRAY                00190600
&I       SETA  &I+1                                                     00190700
         AIF   ('&RCPDSWB(&I)' NE '&SW2').LOOP1A                        00190800
.*                                                                      00190900
.*   WE FOUND IT                                                        00191000
.*                                                                      00191100
&L       SETA  (&I-1)/8                INDEX TO BYTE NAME               00191200
&SW1     SETC  '&RCPDSWN(&L)'          GET BYTE NAME                    00191300
.FOUNDSW ANOP                          HAVE WE HAD IT BEFORE?           00191400
&K       SETA  0                                                        00191500
.SWL1    AIF   (&K GE &RCPDSW0).NEWSW1                                  00191600
&K       SETA  &K+1                                                     00191700
         AIF   ('&RCPDSW1(&K)' NE '&SW1').SWL1                          00191800
         AIF   (&RCPDSW3(&K) NE &NOT).SWL1  ENSURE INVERT BIT THE SAME  00191900
.*                                                                      00192000
.* WE FOUND IT                                                          00192100
.*                                                                      00192200
&RCPDSW2(&K) SETC '&RCPDSW2(&K)+&SW2'  CONCATENATE CURRENT SW           00192300
         AGO   .LOOP1                  GO DO NEXT                       00192400
.NEWSW1  ANOP                                                           00192500
&RCPDSW0 SETA  &K+1                    NEXT SW BYTE INDEX               00192600
&RCPDSW1(&RCPDSW0) SETC '&SW1'         BYTE NAME                        00192700
&RCPDSW2(&RCPDSW0) SETC '&SW2'         BIT NAME                         00192800
&RCPDSW3(&RCPDSW0) SETB (&NOT)         SET INVERT INDICATOR             00192900
         AGO   .LOOP1                  GO DO NEXT                       00193000
.TGEN    ANOP  SEARCH GENERIC NAME ARRAY                                00193100
&I       SETA  0                                                        00193200
&L       SETA  K'&SW2                                                   00193300
.LOOP2   ANOP                                                           00193400
&I       SETA  &I+1                                                     00193500
         AIF   (&I GT &RCPGSW#).NOTFND                                  00193600
&C       SETC  '&RCPGSWB(&I)'                                           00193700
         AIF   (&L LT K'&C).LOOP2                                       00193800
         AIF   ('&SW2'(1,K'&C) NE '&C').LOOP2                           00193900
&SW1     SETC  '&RCPGSWN(&I)'                                           00194000
         AGO   .FOUNDSW                EUREKA                           00194100
.NOTFND  MNOTE 4,'SWITCH ''&SW2'' NOT DECLARED'                         00194200
         AGO   .LOOP1                                                   00194300
.EXIT    MEND                                                           00194400
./       ADD   NAME=RCPLOCS1                                            00194500
*23456789*12345*78921234567893123456789*                                00194600
         MACRO                                                          00194700
         RCPLOCSW &SW                                                   00194800
.********************************************************************   00194900
.*                                                                  *   00195000
.*       INNER MACRO USED BY GOIF, SET, RESET AND FLIP.             *   00195100
.*       THE PARM PASSED IS THE SWITCH OR LIST OF SWITCHES.         *   00195200
.*       RCPLOCSW SCANS THE ARRAYS SET UP BY DCLSW TO SEE IF THE    *   00195300
.*       SWITCH BIT NAMES WERE DECLARED, AND IF A LIST WAS PASSED,  *   00195400
.*       WHETHER ALL THE SWITCH BITS BELONG TO THE SAME BYTE.       *   00195500
.*       RCPLOCSW PASSES BACH THE SWITCH BYTE NAME IN GLOBAL SETC   *   00195600
.*       VARIABLE &RCPDSW1 AND THE SWITCH BIT NAME IN GLOBAL SETC   *   00195700
.*       &RCPDSW2. IF A LIST OF SWITCHES WAS PASSED, &RCPDSW2       *   00195800
.*       CONTAINS THE SWITCH NAMES SEPARATED BY PLUS SIGNS.         *   00195900
.*       IF THE FIRST OR ONLY SWITCH WAS NOT FOUND, &RCPDSW1 IS SET *   00196000
.*       TO NULL. IF A LIST OF SWITCHES IS PASSED AND ANY SWITCH IS *   00196100
.*       NOT DECLARED IN THE SAME SWITCH BYTE AS THE FIRST, AN MNOTE*   00196200
.*       IS ISSUED WARNING OF POSSIBLE ERROR, BUT &RCPDSW1 IS SET   *   00196300
.*       TO THE NAME OF THE SWITCH BYTE CONTAINING THE FIRST SWITCH *   00196400
.*       BIT IN THE LIST.                                           *   00196500
.*                                                                  *   00196600
.********************************************************************   00196700
         GBLA  &RCPDSW#,&RCPGSW#       COUNTER FOR DECLARED SWITCHES    00196800
         GBLA  &RCPDSW0                NO OF SWS FOUND BY RCPLOCSW      00196900
         GBLB  &RCPDSWD(99)            DEFER DECLARE INDICATORS         00197000
         GBLC  &RCPDSWN(99)            SWITCH BYTE NAMES                00197100
         GBLC  &RCPDSWB(800)           SWITCH BIT NAMES                 00197200
         GBLC  &RCPGSWN(99)            GENERIC SWITCH BYTE NAMES        00197300
         GBLC  &RCPGSWB(99)            GENERIC SWITCH BIT PREFIXES      00197400
         GBLC  &RCPDSW1(20)            SWITCH BYTE NAMES                00197500
         GBLC  &RCPDSW2(20)            SWITCH BIT NAME(S)               00197600
         LCLA  &I,&J,&K,&L,&M,&N       LOCAL COUNTERS                   00197700
         LCLC  &C,&SW1,&SW2                                             00197800
&RCPDSW0 SETA  0                       INITIALIZE                       00197900
&N       SETA  N'&SW                   NO OF SWITCHES ENTERED           00198000
&J       SETA  &RCPDSW#*8              INDEX TO LAST DECLARED SW BIT    00198100
.LOOP1   AIF   (&M GE &N).EXIT        LOOP FOR EACH SW                  00198200
&M       SETA  &M+1                                                     00198300
&SW2     SETC  '&SW(&M)'               SWITCH TO SEARCH FOR             00198400
         AIF   ('&SW2' EQ '').LOOP1    SKIP IF NULL                     00198500
&I       SETA  8                       INDEX TO FIRST DECLARED SW - 1   00198600
.LOOP1A  AIF   (&I GE &J).TGEN         SEARCH NAME ARRAY                00198700
&I       SETA  &I+1                                                     00198800
         AIF   ('&RCPDSWB(&I)' NE '&SW2').LOOP1A                        00198900
.*                                                                      00199000
.*   WE FOUND IT                                                        00199100
.*                                                                      00199200
&L       SETA  (&I-1)/8                INDEX TO BYTE NAME               00199300
&SW1     SETC  '&RCPDSWN(&L)'          GET BYTE NAME                    00199400
.FOUNDSW ANOP                          HAVE WE HAD IT BEFORE?           00199500
&K       SETA  0                                                        00199600
.SWL1    AIF   (&K GE &RCPDSW0).NEWSW1                                  00199700
&K       SETA  &K+1                                                     00199800
         AIF   ('&RCPDSW1(&K)' NE '&SW1').SWL1                          00199900
.*                                                                      00200000
.* WE FOUND IT                                                          00200100
.*                                                                      00200200
&RCPDSW2(&K) SETC '&RCPDSW2(&K)+&SW2'  CONCATENATE CURRENT SW           00200300
         AGO   .LOOP1                  GO DO NEXT                       00200400
.NENSW1  ANOP                                                           00200500
&RCPDSW0 SETA  &K+1                    NEXT SW BYTE INDEX               00200600
&RCPDSW1(&RCPDSW0) SETC '&SW1'         BYTE NAME                        00200700
&RCPDSW2(&RCPDSW0) SETC '&SW2'         BIT NAME                         00200800
         AGO   .LOOP1                  GO DO NEXT                       00200900
.TGEN    ANOP  SEARCH GENERIC NAME ARRAY                                00201000
&I       SETA  0                                                        00201100
&L       SETA  K'&SW2                                                   00201200
.LOOP2   ANOP                                                           00201300
&I       SETA  &I+1                                                     00201400
         AIF   (&I GT &RCPGSW#).NOTFND                                  00201500
&SW1     SETC  '&RCPGSWN(&I)'                                           00201600
         AIF   (&L LT K'&SW1).LOOP2                                     00201700
         AIF   ('&SW1'(1,&L) NE '&SW2').LOOP2                           00201800
         AGO   .FOUNDSW                EUREKA                           00201900
.NOTFND  MNOTE 4,'SWITCH ''&SW2'' NOT DECLARED'                         00202000
         AGO   .LOOP1                                                   00202100
.EXIT    MEND                                                           00202200
./       ADD   NAME=RCPLOCS2                                            00202300
*23456789*12345*78921234567893123456789*                                00202400
         MACRO                                                          00202500
         RCPLOCSW &SW                                                   00202600
.********************************************************************   00202700
.*                                                                  *   00202800
.*       INNER MACRO USED BY GOIF, SET, RESET AND FLIP.             *   00202900
.*       THE PARM PASSED IS THE SWITCH OR LIST OF SWITCHES.         *   00203000
.*       RCPLOCSW SCANS THE ARRAYS SET UP BY DCLSW TO SEE IF THE    *   00203100
.*       SWITCH BIT NAMES WERE DECLARED, AND IF A LIST WAS PASSED,  *   00203200
.*       WHETHER ALL THE SWITCH BITS BELONG TO THE SAME BYTE.       *   00203300
.*       RCPLOCSW PASSES BACH THE SWITCH BYTE NAME IN GLOBAL SETC   *   00203400
.*       VARIABLE &RCPDSW1 AND THE SWITCH BIT NAME IN GLOBAL SETC   *   00203500
.*       &RCPDSW2. IF A LIST OF SWITCHES WAS PASSED, &RCPDSW2       *   00203600
.*       CONTAINS THE SWITCH NAMES SEPARATED BY PLUS SIGNS.         *   00203700
.*       IF THE FIRST OR ONLY SWITCH WAS NOT FOUND, &RCPDSW1 IS SET *   00203800
.*       TO NULL. IF A LIST OF SWITCHES IS PASSED AND ANY SWITCH IS *   00203900
.*       NOT DECLARED IN THE SAME SWITCH BYTE AS THE FIRST, AN MNOTE*   00204000
.*       IS ISSUED WARNING OF POSSIBLE ERROR, BUT &RCPDSW1 IS SET   *   00204100
.*       TO THE NAME OF THE SWITCH BYTE CONTAINING THE FIRST SWITCH *   00204200
.*       BIT IN THE LIST.                                           *   00204300
.*                                                                  *   00204400
.********************************************************************   00204500
         GBLA  &RCPDSW#,&RCPGSW#       COUNTER FOR DECLARED SWITCHES    00204600
         GBLB  &RCPDSWD(99)            DEFER DECLARE INDICATORS         00204700
         GBLC  &RCPDSWN(99)            SWITCH BYTE NAMES                00204800
         GBLC  &RCPDSWB(800)           SWITCH BIT NAMES                 00204900
         GBLC  &RCPGSWN(99)            GENERIC SWITCH BYTE NAMES        00205000
         GBLC  &RCPGSWB(99)            GENERIC SWITCH BIT PREFIXES      00205100
         GBLC  &RCPDSW1                SWITCH BYTE NAME                 00205200
         GBLC  &RCPDSW2                SWITCH BIT NAME(S)               00205300
         LCLA  &I,&J,&K,&L,&M,&N       LOCAL COUNTERS                   00205400
         LCLC  &C                                                       00205500
&RCPDSW2 SETC  '&SW(1)'                EXTRACT 1ST SWITCH BIT           00205600
&J       SETA  &RCPDSW#*8+8            ARRAY POS OF LAST SW BIT         00205700
&I       SETA  8                       ARRAY POS-1 OF 1ST SW BIT        00205800
.LOOP1   AIF   (&I GE &J).TGEN         IF SW NOT FOUND IN 1ST ARRAY,    00205900
.*                                      GO SEARCH GENERIC NAME ARRAY    00206000
&I       SETA  &I+1                                                     00206100
         AIF   ('&RCPDSWB(&I)' NE '&RCPDSW2').LOOP1  LOOK FOR MATCH     00206200
.*                                                                      00206300
.*       OK, WE'VE FOUND A MATCH.                                       00206400
.*                                                                      00206500
&I       SETA  (&I-1)/8               GET POS OF SWITCH BYTE            00206600
&RCPDSW1 SETC  '&RCPDSWN(&I)'         MOVE IT TO EXIT PARM VAR          00206700
&I       SETA  &I*8+1                 POINT TO 1ST SW BIT IN IT         00206800
&J       SETA  &I+8                   POINT TO LAST SW BIT IN IT        00206900
&M       SETA  N'&SW                  GET NO OF SWITCHES                00207000
&L       SETA  1                                                        00207100
.*                                                                      00207200
.*       NOW WE PROCESS SUBSEQUENT SWITCHES IN THE LIST                 00207300
.*                                                                      00207400
.LOOP2   AIF   (&L GE &M).EXIT        EXIT WHEN FINISHED                00207500
&L       SETA  &L+1                   POINT TO NEXT SW IN LIST          00207600
&C       SETC  '&SW(&L)'               EXTRACT IT                       00207700
&RCPDSW2 SETC  '&RCPDSW2.+&C'           THEN APPEND TO PREVIOUS         00207800
.*                                                                      00207900
.*       NOW WE CHECK THAT THE SWITCH IS DECLARED IN THE SAME           00208000
.*       BYTE AS THE FIRST.                                             00208100
.*                                                                      00208200
&N       SETA  &I-1                     POINT TO 1ST BIT POS MINUS 1    00208300
.LOOP3   AIF   (&N GE &J).NM            IF SW NOT FOUND, ISSUE MNOTE    00208400
&N       SETA  &N+1                     POINT TO NEXT                   00208500
         AIF   ('&C' NE '&RCPDSWB(&N)').LOOP3  SEARCH FOR MATCH         00208600
         AGO   .LOOP2                   IF FOUND, GO PROCESS NEXT       00208700
.NM      MNOTE 4,'WARNING: SWITCH ''&C'' NOT DECLARED IN SAME BYTE AS  X00208800
               SWITCH ''&SW(1)'' - LOGIC ERROR MAY OCCUR'               00208900
         AGO   .LOOP2            CONTINUE FOR NEXT SWITCH BIT           00209000
.*                                                                      00209100
.*       IF THE SWITCH WAS NOT LOCATED IN THE EXPLICIT NAME ARRAY,      00209200
.*       THE GENERIC NAME ARRAY IS SEARCHED.                            00209300
.*                                                                      00209400
.TGEN    ANOP                                                           00209500
&I       SETA  0                                                        00209600
&RCPDSW2 SETC  '&SW(1)'                EXTRACT 1ST SWITCH               00209700
&L       SETA  K'&RCPDSW2              GET LENGTH OF 1ST SW             00209800
.LOOP4   AIF   (&I GE &RCPGSW#).ERROR  IF NOT SW NOT DECLARED, ERROR    00209900
&I       SETA  &I+1                                                     00210000
&C       SETC  '&RCPGSWB(&I)'          GET GENERIC PREFIX               00210100
&K       SETA  K'&C                    GET LENGTH OF GENERIC PREFIX     00210200
         AIF   (&L LT &K).LOOP4         AND SKIP IF LEN OF SWITCH NAME  00210300
.*                                          < LEN OF GENERIC PREFIX     00210400
         AIF   ('&RCPDSW2'(1,&K) NE '&C').LOOP4  ALSO SKIP IF NO MATCH  00210500
&RCPDSW1 SETC  '&RCPGSWN(&I)'          SAVE SWITCH BYTE NAME            00210600
&I       SETA   1                                                       00210700
&J       SETA   N'&SW                                                   00210800
.LOOP5   AIF   (&I GE &J).EXIT         EXIT WHEN FINISHED               00210900
&I       SETA   &I+1                                                    00211000
&RCPDSW2 SETC   '&RCPDSW2.+&SW(&I)'     APPEND THIS SWITCH              00211100
         AIF    ('&SW(&I)    '(1,&K) EQ '&C').LOOP5 CHECK PREFIX        00211200
         MNOTE 4,'WARNING: SWITCH ''&SW(&I)'' NOT GENERICALLY EQUAL TO X00211300
               SWITCH ''&SW(1)'''                                       00211400
         AGO   .LOOP5                                                   00211500
.ERROR   MNOTE 8,'SWITCH ''&SW(1)'' NOT DECLARED'                       00211600
&RCPDSW1 SETC  ''             INDICATE ERROR                            00211700
.EXIT    MEND                                                           00211800
./       ADD   NAME=RCPMCA                                              00211900
         MACRO                                                          00212000
         RCPMCA &DSECT=YES                                              00212100
         GBLC  &RCPPRE                                                  00212200
         GBLA  &RCPSWS(10)                                              00212300
         LCLC  &P                                                       00212400
     RCPDEBUG                                                           00212500
&P       SETC  '&RCPPRE'                                                00212600
         AIF   (&RCPSWS(2) NE 2).DSECT                                  00212700
&P.MCA   DS    0F                      MODULE COMMUNICATIONS AREA       00212800
         AGO   .MCA2                                                    00212900
.DSECT   ANOP                                                           00213000
&P.MCA   DSECT                         MODULE COMMUNICATIONS AREA       00213100
.MCA2    ANOP                                                           00213200
&P.XDS   DS    F                       ADDR OF EXTERNAL DUMMY SECTION   00213300
         AIF   (&RCPSWS(3) LT 1).EXIT                                   00213400
&P.A#GET DS    F                       ADDRESS OF LIFO GET ROUTINE      00213500
&P.A#FRE DS    F                       ADDRESS OF LIFO FREE ROUTINE     00213600
&P.#S    DS    F                       ADDRESS OF CURRENT LIFO STACK    00213700
&P.#E    DS    F                       ADDRESS OF END OF LIFO STACK     00213800
&P.#N    DS    F                       ADDRESS OF NEXT FREE AREA        00213900
&P.#C    DS    F                       ADDRESS OF NEXT LIFO STACK       00214000
&P.#L    DS    F                       LENGTH OF CURRENT LIFO STACK     00214100
.EXIT    MEND                                                           00214200
./       ADD   NAME=RCPNTU                                              00214300
         MACRO                                                          00214400
         RCPNTU &KEY,&LEN,&PAR                                          00214500
.*                                                                      00214600
.*     THIS IS AN ALLOC/FREE MACRO TEXT UNIT PROCESSOR SUBROUTINE       00214700
.*     MACRO. IT BUILDS NUMERIC TYPE TEXT UNITS.                        00214800
.*                                                                      00214900
         LCLA  &L,&R                                                    00215000
         LCLC  &C                                                       00215100
         GBLC  &RCPTYPE                                                 00215200
.*  ALLOC/FREE INNER MACRO TO SET UP NUMERIC TEXT UNITS                 00215300
&L       SETA  1                       DEFAULT LENGTH                   00215400
         AIF   ('&LEN' EQ '').NL                                        00215500
&L       SETA  &LEN                                                     00215600
.NL      MVI   S99TUKEY+1,&KEY         SET KEY FIELD                    00215700
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00215800
         MVI   S99TULNG+1,&L           SET LENGTH FIELD                 00215900
         AIF   ('&PAR'(1,1) EQ '(').REG                                 00216000
         RCPTYPE &PAR                  ANALYSE PARAMETER                00216100
         AIF   ('&RCPTYPE' EQ 'N').NUMERIC                              00216200
&R       SETA  4-&L                                                     00216300
         MVC   S99TUPAR(&L),&R+&PAR    MOVE IN QUANTITY                 00216400
         RCPDINC 10                                                     00216500
         MEXIT                                                          00216600
.NUMERIC AIF   (&L EQ 1).NL1                                            00216700
         MVC   S99TUPAR(&L.),=AL&L.(&PAR) MOVE IN QUANTITY              00216800
&R       SETA  &L+6                                                     00216900
         AIF   (&R/2 EQ (&R+1)/2).LOK ENSURE LENGTH EVEN                00217000
&R       SETA  &R+1                                                     00217100
.LOK     RCPDINC &R                                                     00217200
         MEXIT                                                          00217300
.NL1     MVI   S99TUPAR,&PAR           MOVE IN QUANTITY                 00217400
         RCPDINC 8                                                      00217500
         MEXIT                                                          00217600
.REG     ANOP                                                           00217700
&C       SETC  '&PAR'(2,K'&PAR-2)                                       00217800
         AIF   (&L EQ 3).STCM                                           00217900
         AIF   (&L EQ 2).STH                                            00218000
         AIF   (&L EQ 1).STC                                            00218100
         ST    &C,S99TUPAR             STORE TEXT UNIT QUANTITY         00218200
         AGO   .RCPDINC                                                 00218300
.STH     STH   &C,S99TUPAR             STORE TEXT UNIT QUANTITY         00218400
         AGO   .RCPDINC                                                 00218500
.STC     STC   &C,S99TUPAR             STORE TEXT UNIT QUANTITY         00218600
         AGO   .RCPDINC                                                 00218700
.STCM    STCM  &C,7,S99TUPAR           STORE TEXT UNIT QUANTITY         00218800
.RCPDINC RCPDINC 10                                                     00218900
         MEND                                                           00219000
./       ADD   NAME=RCPPERM                                             00219100
         MACRO                                                          00219200
         RCPPERM                                                        00219300
         SPACE                                                          00219400
*********************************************************************** 00219500
**     PERMANENTLY ALLOCATED ATTRIBUTE TEXT UNIT                     ** 00219600
*********************************************************************** 00219700
         MVI   S99TUKEY+1,DALPERMA     SET TEXT UNIT KEY                00219800
         RCPDINC  4                                                     00219900
         MEND                                                           00220000
./       ADD   NAME=RCPPPL                                              00220100
         MACRO                                                          00220200
&NAME    RCPPPL &PCL=,&NOPARM=,&PARSERR=,&PDLREG=R11,                  X00220300
               &PDLNAME=,&PARSEP=,&PARSWKA=                             00220400
         GBLB  &RCPPPL(2),&RCPECT(2)                                    00220500
         GBLC  &RCPPRE,&RCPPREP                                         00220600
         LCLC  &P                                                       00220700
&P       SETC  '&RCPPRE'                                                00220800
         AIF   (&RCPPPL(2)).BPPL                                        00220900
         EJECT                                                          00221000
         IKJPPL                                                         00221100
L#PPL    EQU   *-PPL                   LENGTH OF PPL                    00221200
&SYSECT  CSECT                                                          00221300
         SPACE 1                                                        00221400
&RCPPPL(2) SETB 1                                                       00221500
.BPPL    RCPDS                                                          00221600
&P.PPL   DS    CL(L#PPL)               RESERVE SPACE FOR PPL            00221700
&P.PDLP  DS    F                       POINTER TO PDL                   00221800
         RCPDS                                                          00221900
         SPACE 6                                                        00222000
*********************************************************************** 00222100
***   THIS CODE GENERATES A PARSE PARAMETER LIST                    *** 00222200
*********************************************************************** 00222300
         XC    &P.PDLP,&P.PDLP         ZERO PDL POINTER                 00222400
         AIF   ('&NOPARM(1)' EQ '' OR '&NOPARM(2)' NE '').PB2           00222500
         L     R1,CPPLECT              LOAD ECT ADDRESS                 00222600
&RCPECT(1) SETB 1                                                       00222700
         USING ECT,R1                  ECT ADDRESSABLE                  00222800
         TM    ECTSWS,ECTNOPD          WERE ANY OPERANDS SUPPLIED?      00222900
         BO    &NOPARM(1)              NO, BRANCH OUT                   00223000
         SPACE                                                          00223100
.PB2     LA    R1,&P.PPL               LOAD PPL ADDRESS                 00223200
         USING PPL,R1                                                   00223300
         MVC   PPLUPT,CPPLUPT          MOVE IN UPT ADDRESS              00223400
         MVC   PPLECT,CPPLECT          MOVE IN ECT ADDRESS              00223500
         MVC   PPLCBUF,CPPLCBUF        MOVE IN CBUF ADDRESS             00223600
         LA    R15,&P.ECB              LOAD ATTN ECB ADDRESS            00223700
         ST    R15,PPLECB              AND STORE IN PPL                 00223800
         LA    R15,&P.PDLP             LOAD PDL POINTER ADDRESS         00223900
         ST    R15,PPLANS               AND STORE IN PPL                00224000
         AIF   ('&PARSWKA' EQ '').PB3                                   00224100
         AIF   ('&PARSWKA'(1,1) EQ '').PB4                              00224200
         LA    R15,&PARSWKA            LOAD ADDRESS OF WORK AREA        00224300
         ST    R15,PPLUWA               AND STORE IN PPL                00224400
         AGO   .PB3                                                     00224500
.PB4     ST    &PARSWKA(1),PPLUWA      STORE ADDRESS OF WORKAREA        00224600
.PB3     AIF   ('&PCL' EQ '').EXIT                                      00224700
         L     R15,=V(&PCL)            LOAD PCL ADDRESS                 00224800
         ST    R15,PPLPCL              AND STORE IN PPL                 00224900
         SPACE 2                                                        00225000
         AIF   ('&NOPARM(1)' EQ '' OR '&NOPARM(2)' EQ '').PB5           00225100
         L     R1,CPPLECT              LOAD ECT ADDRESS                 00225200
&RCPECT(1) SETB 1                                                       00225300
         USING ECT,R1                                                   00225400
         TM    ECTSWS,ECTNOPD          WERE ANY OPERANDS SUPPLIED?      00225500
         BO    &NOPARM(1)               NO, BRANCH OUT                  00225600
         SPACE                                                          00225700
.PB5     AIF   ('&SYSPARM' EQ 'MVT').MVTBYP                             00225800
         AIF   ('&RCPPREP' EQ '').NOPREP                                00225900
         L     R15,&RCPPREP            LOAD EP OF IKJPARS               00226000
         BALR  R14,R15                  AND ENTER IT                    00226100
         AGO   .PRET                                                    00226200
.NOPREP  ANOP                                                           00226300
         L     R15,16                  LOAD CVT ADDRESS                 00226400
         TM    524(R15),X'80'          IS IKJPARS LOADED?               00226500
         AIF   ('&PARSEP' EQ '').PBL1                                   00226600
         BZ    &P.LOAD                  NO, BRANCH TO LOAD SVC          00226700
         L     R15,524(15)             LOAD EP OF IKJPARS               00226800
         ST    R15,&PARSEP             SAVE ITS ADDRESS                 00226900
         BALR  R14,R15                 THEN BALR TO IT                  00227000
         B     &P.PLNKB                BYPASS LOAD SVC                  00227100
&P.LOAD  LOAD  EP=IKJPARS                                               00227200
         LR    R15,R0                  LOAD EP OF IKJPARS               00227300
         ST    R15,&PARSEP             SAVE IT                          00227400
         BALR  R14,R15                 THEN BALR TO IT                  00227500
&P.PLNKB DS    0H                                                       00227600
         AGO   .PRET                                                    00227700
.PBL1    BZ    &P.PLINK                 NO, BRANCH TO LINK SVC          00227800
         L     R15,524(R15)            ELSE LOAD ITS ADDRESS            00227900
         BALR  R14,R15                  AND BALR TO IT                  00228000
         B     &P.PLNKB                BYPASS LINK SVC                  00228100
.MVTBYP  ANOP                                                           00228200
&P.PLINK LINK  EP=IKJPARS                                               00228300
&P.PLNKB DS    0H                                                       00228400
.PRET    AIF   ('&PARSERR' EQ '').EXIT                                  00228500
         SPACE                                                          00228600
         LTR   R15,R15                 TEST RETURN CODE                 00228700
         BNZ   &PARSERR                 AND BRANCH ON NON-ZERO          00228800
         SPACE                                                          00228900
         AIF   ('&PDLREG' EQ '' OR '&PDLNAME' EQ '').EXIT               00229000
         L     &PDLREG,&P.PDLP         LOAD PDL ADDRESS                 00229100
         USING &PDLNAME,&PDLREG        PDL DSECT ADDRESSABLE            00229200
.EXIT    MEND                                                           00229300
./       ADD   NAME=RCPPROC                                             00229400
         MACRO                                                          00229500
         RCPPROC &WKCSECT=,&WKDSECT=,                                  X00229600
               &REG1=,&REG0=,&ISA=,&SAVEPRE=,                          X00229700
               &SAVESUF=,&SP=                                           00229800
         GBLA  &RCPSWS(10)                                              00229900
         GBLC  &RCPPRE,&RCPWKCS,&RCPWKDS                                00230000
         GBLC  &RCPSPN                                                  00230100
         LCLC  &P,&C                                                    00230200
         RCPDEBUG                                                       00230300
&P       SETC  '&RCPPRE'                                                00230400
         AIF   ('&WKCSECT' EQ '').TDS                                   00230500
         SPACE                                                          00230600
         MNOTE 4,'WKCSECT= OPTION INVALID WITH PROC OPTION, '           00230700
         MNOTE *,'    WKDSECT=  USED INSTEAD'                           00230800
&RCPWKDS SETC  '&WKCSECT'                                               00230900
         AGO   .SETCS                                                   00231000
.TDS     AIF   ('&WKDSECT' EQ '').SYSECT                                00231100
&RCPWKDS SETC  '&WKDSECT'                                               00231200
         AGO   .SETCS                                                   00231300
.SYSECT  ANOP                                                           00231400
&RCPWKDS SETC  '&SYSECT'                                                00231500
.SET1    AIF   (K'&RCPWKDS LT 8).LOK                                    00231600
&RCPWKDS SETC  '&RCPWKDS'(1,4)'&RCPWKDS'(6,3)'1'                        00231700
         AGO   .SETCS                                                   00231800
.LOK     ANOP                                                           00231900
&RCPWKDS SETC  '&RCPWKDS.1'                                             00232000
.SETCS   ANOP                                                           00232100
&RCPWKCS SETC  ''                                                       00232200
&RCPSWS(4) SETA &RCPSWS(2)-1 SET W/A TO BE FREED OPT IF PROC(MAIN)      00232300
         AIF   ('&ISA' EQ '').NISA                                      00232400
&RCPSWS(3) SETA 1                      SET LIFO FLAG IF ISA SPEC        00232500
.NISA    ANOP                                                           00232600
         SPACE 2                                                        00232700
         RCPDS                                                          00232800
         DS    9D                      SAVE AREA                        00232900
&P.RCODE DS    F                       RETURN CODE                      00233000
         RCPMCA                                                         00233100
         RCPDS                                                          00233200
         SPACE 2                                                        00233300
         AIF   ('&REG1' EQ '').TR0                                      00233400
         LR    &REG1,R1                SAVE CONTENTS OF REG 1           00233500
.TR0     AIF   ('&REG0' EQ '').TP                                       00233600
         LR    &REG0,R0                SAVE CONTENTS OF REG 0           00233700
.TP      AIF   (&RCPSWS(2) EQ 2).PROCMN   PROCMAIN OPTION               00233800
         AIF   (&RCPSWS(3) EQ 1).PL    LIFO OPTION                      00233900
         L     R15,0(R13)              R15 -> MODULE COMMUNIC. AREA     00234000
         L     R15,&P.XDS-&P.MCA(R15)  LOAD EXTERNAL DUMMY SECT ADDR    00234100
         AL    R15,&P.QCON             GET OFFSET TO WORK AREA          00234200
         ST    R15,8(R13)              CHAIN SAVE                       00234300
         ST    R13,4(R15)               AREAS TOGETHER                  00234400
         MVC   0(4,R15),0(R13)         COPY POINTER TO COMM AREA        00234500
         LR    R13,R15                 LOAD WORK AREA ADDRESS           00234600
         USING &RCPWKDS,R13              ESTABLISH ADDRESSABLITY TO IT  00234700
         MEXIT                                                          00234800
.PL      ANOP                                                           00234900
*********************************************************************** 00235000
*        GET WORKAREA FROM LIFO STACK                                 * 00235100
*********************************************************************** 00235200
         #GET  LV=&P.WKLEN                                              00235300
         ST    R1,8(R13)               CHAIN SAVE                       00235400
         ST    R13,4(R1)                AREAS TOGETHER                  00235500
         MVC   0(4,R1),0(R13)          PROPAGATE MODULE COMM. AREA ADDR 00235600
         LR    R13,R1                  LOAD WORK AREA ADDRESS           00235700
         USING &RCPWKDS,R13             ESTABLISH ADDRESSABILITY TO IT  00235800
         MEXIT                                                          00235900
.PROCMN  L     R0,&P.CXD               LOAD WORK AREA LENGTH            00236000
         AIF   ('&SYSPARM' EQ 'MVT').MVT                                00236100
 MNOTE *,'      GETMAIN RU,LV=(0),SP=&SP,BNDRY=PAGE'                    00236200
         GETMAIN RU,LV=(0),SP=&SP,BNDRY=PAGE                            00236300
         AGO   .CONT                                                    00236400
.MVT     AIF   ('&SP' EQ '').NOSP                                       00236500
         ICM   R0,8,=AL1(&SP)          INSERT SUBPOOL NUMBER            00236600
.NOSP    ANOP                                                           00236700
*        GETMAIN R,LV=(0)              OBTAIN A WORK AREA               00236800
.CONT    ANOP                                                           00236900
&RCPSPN  SETC  '&SP'                                                    00237000
         LR    R15,R13                 SAVE CALLER'S SAVE AREA ADDR     00237100
         LR    R13,R1                  LOAD EXT DUMMY SECTION ADDR      00237200
         AL    R13,&P.QCON              ADD OFFSET TO WORK AREA         00237300
         ST    R13,8(R15)              CHAIN SAVE                       00237400
         ST    R15,4(R13)               AREAS TOGETHER                  00237500
         USING &RCPWKDS,R13            GET WORKAREA ADDRESSABILITY      00237600
         ST    R1,&P.XDS               STORE DUMMY SECTION ADDR IN     X00237700
                                         MODULE COMMUNICATIONS AREA     00237800
         LA    R15,&P.MCA              STORE COMMUNICATIONS AREA ADDR   00237900
         ST    R15,0(R13)               IN WORD 1 OF SAVE AREA          00238000
         AIF   (&RCPSWS(3) EQ 0 AND '&ISA' EQ '').EXIT                  00238100
&RCPSWS(3) SETA 1                      SET LIFO IN CASE ONLY ISA SPEC   00238200
&C       SETC  '&ISA'                                                   00238300
         AIF   ('&ISA' NE '').TK                                        00238400
&C       SETC  '8192'                                                   00238500
         AGO   .NK                                                      00238600
.TK      AIF   ('&C'(K'&C,1) NE 'K').NK                                 00238700
&C       SETC  '&C'(1,K'&C-1)'*1024'                                    00238800
.NK      EJECT                                                          00238900
*********************************************************************** 00239000
**       INITIALIZE MODULE COMMUNICATIONS AREA WITH POINTERS         ** 00239100
**       TO LIFO STACK AND LIFO GET/FREE ROUTINES                    ** 00239200
*********************************************************************** 00239300
         SPACE 1                                                        00239400
         MVC   &P.A#GET,=V(#####GET)   MOVE LIFO GET AND FREE           00239500
         MVC   &P.A#FRE,=V(####FREE)    ROUTINE ADDRESSES TO MCA        00239600
         L     R15,=Q(#####ISA)        COMPUTE LIFO STACK               00239700
         AL    R15,&P.XDS               PSEUDO REGISTER OFFSET          00239800
         ST    R15,&P.#S                 AND INITIALIZE POINTERS        00239900
         ST    R15,&P.#N                  IN MODULE COMMUNICATIONS AREA 00240000
         L     R14,=A(&C)              LOAD SIZE OF INITIAL STACK AREA  00240100
         ST    R14,&P.#L               STORE THIS IN MCA                00240200
         ALR   R15,R14                  THEN COMPUTE STACK END ADDRESS  00240300
         ST    R15,&P.#E                 AND STORE THIS INTO MCA        00240400
         EJECT                                                          00240500
*********************************************************************** 00240600
**       LIFO STACK GET/FREE ROUTINES                                ** 00240700
*********************************************************************** 00240800
         SPACE 1                                                        00240900
#####ISA DXD   CL(&C)                  DEFINE PSEUDO REGISTER FOR ISA   00241000
         SPACE 1                                                        00241100
#####GET CSECT                         LIFO GET ROUTINE                 00241200
         USING *,R15                                                    00241300
         USING &P.MCA,R1                                                00241400
         A     R0,&P.F7                ROUND LENGTH UP TO               00241500
         N     R0,&P.F8                 A MULTIPLE OF 8                 00241600
         AL    R0,&P.#N                COMPUTE NEXT FREE LIFO SLOT ADDR 00241700
         CL    R0,&P.#E                COMPARE TO STACK END ADDRESS     00241800
         BH    &P.GA                    AND IF TOO BIG, BRANCH          00241900
         LR    R15,R1                  PRESERVE MCA ADDRESS             00242000
         USING &P.MCA,R15              NEW BASE                         00242100
         L     R1,&P.#N                LOAD ADDRESS OF SLOT             00242200
         ST    R0,&P.#N                 AND STORE ADDRESS OF NEXT SLOT  00242300
         BR    R14                     RETURN TO CALLER                 00242400
         SPACE 1                                                        00242500
&P.GA    EQU   *                       IF CURRENT SLOT TOO SMALL        00242600
*        ABEND 1000,DUMP                ABEND FOR NOW                   00242700
         ABEND 1000,DUMP                                                00242800
         SPACE 2                                                        00242900
####FREE DS    0H                      LIFO FREE ROUTINE                00243000
         ENTRY ####FREE                                                 00243100
         USING *,R15                   BASE ADDRESS                     00243200
         USING &P.MCA,R1               MCA ADDRESS                      00243300
         CL    R0,&P.#S                CHECK THAT                       00243400
         BL    &P.FA                    ADDRESS TO BE                   00243500
         CL    R0,&P.#E                  FREED IS WITHIN                00243600
         BH    &P.FA                      BOUND OF CURRENT STACK        00243700
         AL    R0,&P.F7                GET UPPER DOUBLE                 00243800
         N     R0,&P.F8                 WORD BOUNDARY                   00243900
         ST    R0,&P.#N                  AND UPDATE MCA                 00244000
         BR    R14                     RETURN TO CALLER                 00244100
         SPACE 1                                                        00244200
&P.FA    EQU   *                       IF ADDRESS NOT WITHIN THIS STACK 00244300
*        ABEND 1001,DUMP               ABEND                            00244400
         ABEND 1001,DUMP                                                00244500
         SPACE 2                                                        00244600
&P.F7    DC    F'7'                    CONSTANTS                        00244700
&P.F8    DC    F'-8'                    TO ROUND UP TO DOUBLEWORD SIZE  00244800
         DROP  R1,R15                  KILL ADDRESSABILITY              00244900
&SYSECT  CSECT                         RESUME MAIN PROGRAM CSECT        00245000
.EXIT    MEND                                                           00245100
./       ADD   NAME=RCPPSWD                                             00245200
         MACRO                                                          00245300
         RCPPSWD &PASSW                                                 00245400
         GBLC  &DYNP                                                    00245500
         SPACE                                                          00245600
*********************************************************************** 00245700
**   BUILD THE PASSWORD TEXT UNIT                                    ** 00245800
*********************************************************************** 00245900
         RCPVCHAR DALPASSW,14,&PASSW                                    00246000
         MEND                                                           00246100
./       ADD   NAME=RCPQNAME                                            00246200
         MACRO                                                          00246300
         RCPQNAME &QNAME                                                00246400
         GBLC  &DYNP                                                    00246500
         SPACE                                                          00246600
*********************************************************************** 00246700
**   BUILD THE QNAME TEXT UNIT                                       ** 00246800
*********************************************************************** 00246900
         RCPVCHAR DALQNAME,14,&QNAME                                    00247000
         MEND                                                           00247100
./       ADD   NAME=RCPRNGE                                             00247200
         MACRO - BREAK A RANGE PARAMETER INTO TWO                       00247300
         RCPRNGE &P                                                     00247400
         GBLC  &RCPRNGE(2)                                              00247500
         LCLA  &I,&J,&K                                                 00247600
&K       SETA  K'&P                                                     00247700
&RCPRNGE(1) SETC ''                                                     00247800
&RCPRNGE(2) SETC ''                                                     00247900
.LOOP    ANOP                                                           00248000
&I       SETA  &I+1                                                     00248100
         AIF   (&I GT &K).NR                                            00248200
         AIF   ('&P'(&I,1) NE '-' AND '&P'(&I,1) NE ':').LOOP           00248300
&RCPRNGE(1) SETC '&P'(1,&I-1)                                           00248400
&RCPRNGE(2) SETC '&P'(&I+1,&K-&I)                                       00248500
         MEXIT                                                          00248600
.NR      ANOP                                                           00248700
&RCPRNGE(1) SETC '&P'                                                   00248800
         MEND                                                           00248900
./       ADD   NAME=RCPSPACE                                            00249000
         MACRO                                                          00249100
         RCPSPACE &SPACE                                                00249200
         GBLA  &RCPSUB#                NO OF SUBLIST ELEMENTS           00249300
         GBLC  &RCPSUBL(100)           SUBLIST ELEMENTS                 00249400
.********************************************************************** 00249500
.*    THIS IS AN ALLOC INNER MACRO TO BUILD THE ALLOCATION SPACE        00249600
.*    QUANTITY TEXT UNIT. IT SHOULD BE SPECIFIED AS:-                   00249700
.*     SPACE=(TYPE,(PRIMARY,SECONDARY,DIRECTORY),RLSE,CONTIG,ROUND)     00249800
.*   WHERE TYPE IS 'TRK', 'CYL', 'ABSTR' OR A BLOCK QUANTITY            00249900
.*     'CYL' OR 'TRK' SHOULD NOT BE ENTERED IN QUOTES. THE BLOCK        00250000
.*     QUANTITY CAN BE A NUMBER, A REGISTER (IN BRACKETS), OR THE       00250100
.*     NAME OF A FULLWORD CONTAINING THE BLOCK SIZE.                    00250200
.********************************************************************** 00250300
         AIF   ('&SPACE(1)' EQ '' OR '&SPACE(1)' EQ 'TRK').TRK          00250400
         AIF   ('&SPACE(1)' EQ 'CYL').CYL                               00250500
*********************************************************************** 00250600
**        SPACE UNIT IN BLOCKS                                       ** 00250700
*********************************************************************** 00250800
         RCPNTU DALBLKLN,3,&SPACE(1)  GENERATE BLOCK UNIT TU            00250900
         AGO   .TPRIME        GO TEST PRIME QUANTITY                    00251000
.TRK     ANOP  TRACK SPEC REQ OR DEFAULTED                              00251100
         SPACE                                                          00251200
*********************************************************************** 00251300
**       SPACE QUANTITY IN TRACKS                                    ** 00251400
*********************************************************************** 00251500
         MVI   S99TUKEY+1,DALTRK       SET TEXT UNIT KEY                00251600
         RCPDINC 4                                                      00251700
         AGO   .TPRIME                                                  00251800
.CYL     ANOP  CYL QUANTITY                                             00251900
         SPACE 1                                                        00252000
*********************************************************************** 00252100
**      SPACE UNIT IN CYLINDERS                                      ** 00252200
*********************************************************************** 00252300
         MVI   S99TUKEY+1,DALCYL       SET TEXT UNIT KEY                00252400
         RCPDINC 4                     STORE TEXT UNIT ADDR             00252500
.TPRIME  RCPSUBL &SPACE(2)             BREAK UP SUBLIST                 00252600
         AIF   (&RCPSUB# EQ 0).TCONTIG                                  00252700
         AIF   ('&RCPSUBL(1)' EQ '').TSP2                               00252800
         SPACE                                                          00252900
*********************************************************************** 00253000
**       PRIMARY SPACE QUANTITY                                      ** 00253100
*********************************************************************** 00253200
         RCPNTU DALPRIME,3,&RCPSUBL(1)                                  00253300
.TSP2    AIF   (&RCPSUB# LT 2).TCONTIG                                  00253400
         AIF   ('&RCPSUBL(2)' EQ '').TSP3                               00253500
         SPACE                                                          00253600
*********************************************************************** 00253700
**       SECONDARY SPACE QUANTITY                                    ** 00253800
*********************************************************************** 00253900
         RCPNTU DALSECND,3,&RCPSUBL(2)                                  00254000
.TSP3    AIF   (&RCPSUB# LT 3).TCONTIG                                  00254100
         AIF   ('&RCPSUBL(3)' EQ '').TCONTIG                            00254200
         SPACE                                                          00254300
*********************************************************************** 00254400
**       DIRECTORY BLOCK QUANTITY                                    ** 00254500
*********************************************************************** 00254600
         RCPNTU DALDIR,3,&RCPSUBL(3)                                    00254700
.TCONTIG AIF  ('&SPACE(3)' EQ 'CONTIG' OR '&SPACE(4)' EQ 'CONTIG').CON  00254800
         AIF   ('&SPACE(3)' EQ 'MXIG' OR '&SPACE(4)' EQ 'MXIG').MXIG    00254900
         AIF   ('&SPACE(3)' EQ 'ALX' OR '&SPACE(4)' EQ 'ALX').ALX       00255000
.TRLSE   AIF   ('&SPACE(3)' EQ 'RLSE' OR '&SPACE(4)' EQ 'RLSE').RLSE    00255100
.TROUND  AIF   ('&SPACE(4)'EQ'ROUND'OR'&SPACE(5)'EQ'ROUND').ROUND       00255200
         MEXIT                                                          00255300
.CON     ANOP                                                           00255400
*********************************************************************** 00255500
**      CONTIGUOUS SPACE TEXT UNIT                                   ** 00255600
*********************************************************************** 00255700
         RCPNTU DALSPFRM,1,8                                            00255800
         AGO   .TRLSE                                                   00255900
.MXIG    ANOP                                                           00256000
*********************************************************************** 00256100
**       MAXIMUM CONTIGUOUS SPACE TEXT UNIT                          ** 00256200
*********************************************************************** 00256300
         RCPNTU DALSPFRM,1,4                                            00256400
         AGO   .TRLSE                                                   00256500
.ALX     ANOP                                                           00256600
*********************************************************************** 00256700
**       'ALX' SPACE TEXT UNIT                                       ** 00256800
*********************************************************************** 00256900
         RCPNTU DALSPFRM,1,2                                            00257000
         AGO   .TRLSE                                                   00257100
.RLSE    ANOP                                                           00257200
*********************************************************************** 00257300
**      RELEASE UNUSED SPACE TEXT UNIT                               ** 00257400
*********************************************************************** 00257500
         MVI   S99TUKEY+1,DALRLSE      SET TEXT UNIT KEY                00257600
         RCPDINC 4                                                      00257700
         AGO   .TROUND                                                  00257800
.ROUND   ANOP                                                           00257900
*********************************************************************** 00258000
**      RELEASE UNUSED SPACE TEXT UNIT                               ** 00258100
*********************************************************************** 00258200
         MVI   S99TUKEY+1,DALROUND     MOVE IN TEXT UNIT KEY            00258300
         RCPDINC 4                                                      00258400
         MEND                                                           00258500
./       ADD   NAME=RCPSPEC                                             00258600
         MACRO - SET UP USER DEFINED TEXT UNIT                          00258700
         RCPSPEC &T                                                     00258800
         LCLA  &I,&J                                                    00258900
&I       SETA  1                                                        00259000
&J       SETA  K'&T                                                     00259100
         SPACE                                                          00259200
*********************************************************************** 00259300
**       PROCESS SPECIAL TEXT UNITS                                  ** 00259400
*********************************************************************** 00259500
.LOOP    RCPVCHAR &T(&I),&T(&I+2),&T(&I+3),N=&T(&I+1)                   00259600
&I       SETA  &I+4                                                     00259700
         AIF   (&I LE &J).LOOP                                          00259800
         MEND                                                           00259900
./       ADD   NAME=RCPSR2                                              00260000
         MACRO                                                          00260100
         RCPSR2 &A                                                      00260200
         GBLB  &RCPSR2                                                  00260300
         GBLC  &DYNP                                                    00260400
         LCLC  &C                                                       00260500
.*   TO SAVE REG 2 IN REG 0 FOR ALLOC INNER MACROS FIRST TIME ONLY      00260600
.*    IF OPERAND SUPPLIED AND SAVE DONE, RESTORES REG 2 AND             00260700
.*    GENERATES MOVE INSTRUCTION FOR EXECUTE                            00260800
         AIF   ('&A' NE '').UNSAVE                                      00260900
         AIF   (&RCPSR2).EXIT                                           00261000
&RCPSR2  SETB  1                                                        00261100
         LR    R0,R2                   SAVE CONTENTS OF REGISTER 2      00261200
         MEXIT                                                          00261300
.UNSAVE  AIF   (NOT &RCPSR2).EXIT                                       00261400
         B     *+10                    SKIP NEXT INSTRUCTION            00261500
&C       SETC  '&DYNP.MVC'                                              00261600
&C       MVC   S99TUPAR(0),0(R14)      EXECUTED MOVE                    00261700
         LR    R2,R0                   RESTORE CONTENTS OF REGISTER 2   00261800
&RCPSR2  SETB  0                                                        00261900
.EXIT    MEND                                                           00262000
./       ADD   NAME=RCPSSREQ                                            00262100
         MACRO                                                          00262200
         RCPSSREQ                                                       00262300
         SPACE 1                                                        00262400
*********************************************************************** 00262500
**       SUBSYSTEM REQUEST TEXT UNIT                                 ** 00262600
*********************************************************************** 00262700
         SPACE 1                                                        00262800
         MVI   S99TUKEY+1,DALSSREQ MOVE IN TEXT UNIT KEY                00262900
         RCPDINC                   4                                    00263000
         MEND                                                           00263100
./       ADD   NAME=RCPSUBL                                             00263200
         MACRO - BREAK DOWN A SUBLIST                                   00263300
         RCPSUBL &L                                                     00263400
         GBLA  &RCPSUB#                NO OF ELEMENTS FOUND             00263500
         GBLC  &RCPSUBL(100)           ELEMENTS                         00263600
         LCLA  &I,&J,&K                                                 00263700
&RCPSUB# SETA  0                       INITIALIZE                       00263800
         AIF   ('&L' EQ '').EXIT       EXIT IF NULL STRING              00263900
         AIF   ('&L'(1,1) NE '(').NOSUB                                 00264000
&K       SETA  K'&L-1                                                   00264100
&I       SETA  2                                                        00264200
&J       SETA  1                                                        00264300
.LOOP    ANOP                                                           00264400
&J       SETA  &J+1                                                     00264500
         AIF   (&J  GT &K).LAST                                         00264600
         AIF   ('&L'(&J,1) NE ',').LOOP                                 00264700
&RCPSUB# SETA &RCPSUB#+1                                                00264800
         AIF   (&J EQ &I).NULL                                          00264900
&RCPSUBL(&RCPSUB#) SETC '&L'(&I,&J-&I)                                  00265000
&I       SETA  &J+1                                                     00265100
         AGO   .LOOP                                                    00265200
.NULL    ANOP                                                           00265300
&RCPSUBL(&RCPSUB#) SETC ''                                              00265400
&I       SETA  &J+1                                                     00265500
         AGO   .LOOP                                                    00265600
.LAST    AIF   (&J EQ &I).LASTNUL                                       00265700
&RCPSUB# SETA  &RCPSUB#+1                                               00265800
&RCPSUBL(&RCPSUB#) SETC '&L'(&I,&J-&I)                                  00265900
         AGO   .EXIT                                                    00266000
.LASTNUL ANOP                                                           00266100
&RCPSUB# SETA  &RCPSUB#+1                                               00266200
&RCPSUBL(&RCPSUB#) SETC ''                                              00266300
         AGO   .EXIT                                                    00266400
.NOSUB   ANOP                                                           00266500
&RCPSUBL(1) SETC '&L'                                                   00266600
&RCPSUB# SETA 1                                                         00266700
.EXIT    MEND                                                           00266800
./       ADD   NAME=RCPSYSOU                                            00266900
         MACRO                                                          00267000
         RCPSYSOU &CLASS,&COPIES=,&FREE=,&DEST=,&FORMS=                 00267100
         GBLC  &DYNP                                                    00267200
         LCLC  &C                                                       00267300
         AIF   ('&CLASS(1)' EQ '').TPGN                                 00267400
&C       SETC  '&CLASS(1)'                                              00267500
         SPACE                                                          00267600
*********************************************************************** 00267700
**       SYSOUT CLASS TEXT UNIT                                      ** 00267800
*********************************************************************** 00267900
         AIF   ('&C'(1,1) EQ '''').Q                                    00268000
         AIF   ('&C'(K'&C,1) EQ '/').BS                                 00268100
         AIF   ('&C'(1,1) EQ '(').REG                                   00268200
         L     R14,&C                  LOAD ADDRESS OF SYSOUT CLASS     00268300
         MVC   S99TUPAR(1),0(R14)       AND MOVE IT TO TEXT UNIT        00268400
         AGO   .SKEY                                                    00268500
.REG     MVC   S99TUPAR(1),0&C         MOVE SYSOUT CLASS TO TEXT UNIT   00268600
.SKEY    MVI   S99TUKEY+1,DALSYSOU     SET SYSOUT KEY                   00268700
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00268800
         MVI   S99TULNG+1,1            SET LENGTH FIELD                 00268900
         RCPDINC 8                                                      00269000
         AGO   .TPGN                                                    00269100
.BS      RCPTUBFR DALSYSOU,14,&C                                        00269200
         AGO   .TPGN                                                    00269300
.Q       RCPBTU DALSYSOU,1,&C                                           00269400
.TPGN    AIF   ('&CLASS(2)' EQ '').TCOP                                 00269500
         SPACE                                                          00269600
*********************************************************************** 00269700
**   SYSOUT PROGRAM NAME TEXT UNIT                                   ** 00269800
*********************************************************************** 00269900
&C       SETC  '&CLASS(2)'                                              00270000
         RCPVCHAR DALSPGNM,14,&C                                        00270100
.TCOP    AIF   ('&COPIES' EQ '').TFREE                                  00270200
         SPACE                                                          00270300
*********************************************************************** 00270400
**    SYSOUT COPIES TEXT UNIT                                        ** 00270500
*********************************************************************** 00270600
         RCPNTU DALCOPYS,1,&COPIES                                      00270700
.TFREE   AIF   ('&FREE' EQ '').TDEST                                    00270800
         SPACE                                                          00270900
*********************************************************************** 00271000
**     FREE = CLOSE TEXT UNIT                                        ** 00271100
*********************************************************************** 00271200
         AIF   ('&FREE' EQ 'CLOSE').CLOSEOK                             00271300
         MNOTE 4,' **** FREE=&FREE INVALID, FREE=CLOSE USED'            00271400
.CLOSEOK MVI   S99TUKEY+1,DALCLOSE     MOVE IN TEXT UNIT KEY            00271500
         RCPDINC 4                                                      00271600
.TDEST   AIF   ('&DEST' EQ '').TFORMS                                   00271700
         SPACE                                                          00271800
*********************************************************************** 00271900
**       SYSOUT DESTINATION TEXT UNIT                                ** 00272000
*********************************************************************** 00272100
         RCPVCHAR DALSUSER,14,&DEST                                     00272200
.TFORMS  AIF   ('&FORMS' EQ '').EXIT                                    00272300
         SPACE                                                          00272400
*********************************************************************** 00272500
**     SYSOUT FORMS NUMBER TEXT UNIT                                 ** 00272600
*********************************************************************** 00272700
         RCPVCHAR DALSFMNO,14,&FORMS                                    00272800
.EXIT    MEND                                                           00272900
./       ADD   NAME=RCPTU                                               00273000
         MACRO                                                          00273100
         RCPTU &TU            TEXT UNIT LIST                            00273200
         GBLA  &DTUPO         TEXT UNIT POINTER OFFSET                  00273300
         GBLC  &DYNP          ALLOC SYMBOL PREFIX                       00273400
         LCLA  &I,&J                                                    00273500
         LCLC  &C                                                       00273600
         SPACE 1                                                        00273700
*********************************************************************** 00273800
**       ADD SPECIAL TEXT UNITS                                      ** 00273900
*********************************************************************** 00274000
&J       SETA  N'&SYSLIST                                               00274100
.LOOP    ANOP                                                           00274200
&I       SETA  &I+1                                                     00274300
         AIF   (&I GT &J).EXIT                                          00274400
         AIF   ('&TU(&I)'(1,1) EQ '(').R                                00274500
         LA    R15,&TU(&I)             LOAD TEXT UNIT ADDRESS           00274600
         ST    R15,&DYNP.TUP+&DTUPO     AND STORE IT IN POINTER LIST    00274700
&DTUPO   SETA  &DTUPO+4                                                 00274800
         AGO   .LOOP                                                    00274900
.R       ANOP                                                           00275000
&C       SETC  '&TU(&I)'(2,K'&TU(&I)-2)                                 00275100
         ST    &C,&DYNP.TUP+&DTUPO     STORE TEXT UNIT ADDR IN PTR LIST 00275200
&DTUPO   SETA  &DTUPO+4                                                 00275300
         AGO   .LOOP                                                    00275400
.EXIT    MEND                                                           00275500
./       ADD   NAME=RCPTUBFR                                            00275600
         MACRO  - BUILD TEXT UNIT FROM BUFFER                           00275700
         RCPTUBFR &KEY,                TEXT UNIT KEY                   X00275800
               &L,                     MAXIMUM LENGTH VALUE            X00275900
               &C,                     TEXT UNIT                       X00276000
               &N=1                    TEXT UNIT NUMBER                 00276100
         GBLC  &EXECNAM                                                 00276200
         LCLC  &C1,&C2                                                  00276300
         LCLA  &I,&K                                                    00276400
         MVI   S99TUKEY+1,&KEY         SET TEXT UNIT KEY                00276500
         AIF   ('&N' EQ '' OR '&N' EQ '1').N1                           00276600
         LA    R14,&N                  LOAD TEXT UNIT NUMBER            00276700
         STH   R14,S99TUNUM             AND STORE INTO TEXT UNIT        00276800
         AGO   .ENDN                                                    00276900
.N1      MVI   S99TUNUM+1,1            SET TEXT UNIT NUMBER             00277000
.ENDN    ANOP                                                           00277100
&K       SETA  K'&C                                                     00277200
&I       SETA  &K-1                                                     00277300
.LOOP1   ANOP                                                           00277400
&K       SETA  &K-1                                                     00277500
         AIF   (&K LE 0).STD                                            00277600
         AIF   ('&C'(&K,1) NE '/').LOOP1                                00277700
&C2      SETC  '&C'(&K+1,&I-&K)                                         00277800
&C1      SETC  '&C'(1,&K-1)                                             00277900
         AIF   ('&C1'(1,1) NE '(').TC2                                  00278000
&C1      SETC  '0&C1'                                                   00278100
.TC2     AIF   ('&C2' EQ '0000').V2B                                    00278200
         AIF   ('&C2' EQ '00').V1B                                      00278300
         AIF   ('&C2' EQ '0').V0B                                       00278400
         AIF   ('&C2'(1,1) EQ '(').RL                                   00278500
         MVI   S99TULNG+1,&C2          SET LENGTH FIELD                 00278600
         MVC   S99TUPAR(&C2.),&C1      MOVE IN TEXT UNIT                00278700
         RCPDINC &L                                                     00278800
         MEXIT                                                          00278900
.STD     ANOP                                                           00279000
&K       SETA  &L-6                                                     00279100
         MVI   S99TULNG+1,&K           SET TEXT UNIT LENGTH             00279200
&C1      SETC  '&C'(1,&I)              REMOVE TRAILING SLASH            00279300
         MVC   S99TUPAR(&K),&C1        MOVE IN TEXT UNIT                00279400
         RCPDINC &L                                                     00279500
         MEXIT                                                          00279600
.V2B     LH    R14,&C1                 LOAD TEXT UNIT LENGTH            00279700
         S     R14,=A(4)               EXCLUDE LENGTH OF HEADER         00279800
&C1      SETC  '4+&C1'                                                  00279900
         AGO   .MOVE                                                    00280000
.V1B     LH    R14,&C1                 LOAD TEXT UNIT LENGTH            00280100
&C1      SETC  '2+&C1'                                                  00280200
         AGO   .MOVE                                                    00280300
.V0B     SLR   R14,R14                 CLEAR FOR IC                     00280400
         IC    R14,&C1                 INSERT TEXT UNIT LENGTH          00280500
&C1      SETC  '1+&C1'                                                  00280600
         AGO   .MOVE                                                    00280700
.RL      ANOP                                                           00280800
&C2      SETC  '&C2'(2,K'&C2-2)                                         00280900
         LR    R14,&C2                 LOAD TEXT UNIT LENGTH            00281000
.MOVE    STH   R14,S99TULNG             AND STORE INTO LENGTH FIELD     00281100
         BCTR  R14,0                   GET MACHINE LENGTH               00281200
         EXECUTE ,MVC,S99TUPAR-S99TUNIT(0,R15),&C1                      00281300
         EX    R14,&EXECNAM            MOVE IN TEXT UNIT                00281400
         RCPDINC &L                                                     00281500
         MEND                                                           00281600
./       ADD   NAME=RCPTXTL                                             00281700
         MACRO - TO COUNT CHARACTERS IN A STRING                        00281800
         RCPTXTL &S                                                     00281900
         GBLA  &RCPTXTL                                                 00282000
         LCLA  &I,&K,&L                                                 00282100
&RCPTXTL SETA  0                                                        00282200
         AIF   (K'&S LT 3).MEND                                         00282300
&RCPTXTL SETA  K'&S-2                                                   00282400
&L       SETA  &RCPTXTL                                                 00282500
&I       SETA  1                                                        00282600
.LOOP    ANOP                                                           00282700
&I       SETA  &I+1                                                     00282800
.LOOP2   AIF   (&I GT &L).MEND                                          00282900
         AIF   ('&S'(&I,2) NE '''''' AND '&S'(&I,2) NE '&&').LOOP       00283000
&I       SETA  &I+2                                                     00283100
&RCPTXTL SETA  &RCPTXTL-1                                               00283200
         AGO   .LOOP2                                                   00283300
.MEND    MEND                                                           00283400
./       ADD   NAME=RCPTYPE                                             00283500
         MACRO                                                          00283600
         RCPTYPE &T                                                     00283700
         GBLC  &RCPTYPE                                                 00283800
         LCLA  &I,&K                                                    00283900
&K       SETA  K'&T                                                     00284000
&RCPTYPE SETC  ''                                                       00284100
         AIF   (&K EQ 0).EXIT                                           00284200
&RCPTYPE SETC  'C'                                                      00284300
.LOOP    ANOP                                                           00284400
&I       SETA  &I+1                                                     00284500
         AIF   ('&T'(&I,1) LT '0' OR '&T'(&I,1) GT '9').EXIT            00284600
         AIF   (&I LT &K).LOOP                                          00284700
&RCPTYPE SETC  'N'                                                      00284800
.EXIT    MEND                                                           00284900
./       ADD   NAME=RCPUNALC                                            00285000
         MACRO                                                          00285100
         RCPUNALC                                                       00285200
         SPACE 1                                                        00285300
*********************************************************************** 00285400
**     FREE EVEN IF PERMANENTLY ALLOCATED                            ** 00285500
*********************************************************************** 00285600
         MVI   S99TUKEY+1,DUNUNALC     SET TEXT UNIT KEY                00285700
         RCPDINC  4                                                     00285800
         MEND                                                           00285900
./       ADD   NAME=RCPUNIT                                             00286000
         MACRO                                                          00286100
         RCPUNIT &U,&V                                                  00286200
         GBLC  &DYNP                                                    00286300
         AIF   ('&U' EQ '').TVOL                                        00286400
         SPACE 1                                                        00286500
*********************************************************************** 00286600
**       UNIT NAME TEXT UNIT                                         ** 00286700
*********************************************************************** 00286800
         RCPVCHAR DALUNIT,14,&U                                         00286900
.TVOL    AIF   ('&V' EQ '').EXIT                                        00287000
         SPACE 1                                                        00287100
*********************************************************************** 00287200
**       VOLUME SERIAL TEXT UNIT                                     ** 00287300
*********************************************************************** 00287400
         RCPVCHAR DALVLSER,14,&V                                        00287500
.EXIT    MEND                                                           00287600
./       ADD   NAME=RCPVCHAR                                            00287700
         MACRO                                                          00287800
         RCPVCHAR &KEY,&LEN,&C,&N=1                                     00287900
         GBLC  &DYNP                                                    00288000
         AIF   ('&C'(K'&C,1) EQ '/').BM                                 00288100
         AIF   ('&C'(1,1) EQ '''').QM                                   00288200
         RCPSR2                                                         00288300
         AIF   ('&C'(1,1) EQ '(').RM                                    00288400
         LH    R2,&C+4                 LOAD LENGTH OF TEXT UNIT         00288500
         LTR   R2,R2                   TEST FOR ZERO                    00288600
         BZ    *+30                    IF NO TEXT UNIT, SKIP            00288700
         L     R14,&C                  LOAD ADDRESS OF TEXT UNIT        00288800
         AGO   .STHM                                                    00288900
.RM      LH    R2,4&C                  LOAD LENGTH OF TEXT UNIT         00289000
         LTR   R2,R2                   AND TEST FOR ZERO                00289100
         BZ    *+30                    IF NO TEXT UNIT, SKIP            00289200
         L     R14,0&C                 LOAD ADDRESS OF TEXT UNIT        00289300
.STHM    STH   R2,S99TULNG             STORE LENGTH OF TEXT UNIT        00289400
         BCTR  R2,0                    DECREMENT FOR EXECUTE            00289500
         EX    R2,&DYNP.MVC            MOVE IN TEXT UNIT                00289600
         MVI   S99TUKEY+1,&KEY         MOVE IN TEXT UNIT KEY            00289700
         AIF   ('&N' EQ '1' OR '&N' EQ '').N1                           00289800
         LA    R14,&N                  LOAD TEXT UNIT NUMBER            00289900
         STH   R14,S99TUNUM             AND STORE IT IN TEXT UNIT       00290000
         AGO   .ENDN                                                    00290100
.N1      MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00290200
.ENDN    RCPDINC &LEN                                                   00290300
         MEXIT                                                          00290400
.BM      RCPTUBFR &KEY,&LEN,&C                                          00290500
         MEXIT                                                          00290600
.QM      RCPBTU &KEY,&N,&C                                              00290700
         MEND                                                           00290800
./       ADD   NAME=RCPVOLRT                                            00290900
         MACRO                                                          00291000
         RCPVOLRT                                                       00291100
         SPACE 1                                                        00291200
*********************************************************************** 00291300
**    VOLUME SERIAL RETURN TEXT UNIT                                 ** 00291400
*********************************************************************** 00291500
         MVI   S99TUKEY+1,DALRTVOL     SET RETURN VOLUME SERIAL KEY     00291600
         MVI   S99TUNUM+1,1            SET NUMBER FIELD                 00291700
         MVI   S99TULNG+1,8            SET LENGTH FIELD                 00291800
         MVC   S99TUPAR(8),=CL8' '     INITIALIZE FIELD TO BLANKS       00291900
         RCPDINC 14                                                     00292000
         MEND                                                           00292100
./       ADD   NAME=REGS                                                00292200
R0       EQU   0        *USED BY O.S.                                   00292300
R1       EQU   1        *USED BY O.S. // ADDRESS OF PARAMETER LIST      00292400
R2       EQU   2                                                        00292500
R3       EQU   3                                                        00292600
R4       EQU   4                                                        00292700
R5       EQU   5                                                        00292800
R6       EQU   6                                                        00292900
R7       EQU   7                                                        00293000
R8       EQU   8                                                        00293100
R9       EQU   9                                                        00293200
R10      EQU   10                                                       00293300
R11      EQU   11                                                       00293400
R12      EQU   12                                                       00293500
R13      EQU   13       *USED BY O.S. // SAVE-AREA ADDRESS              00293600
R14      EQU   14       *USED BY O.S. // RETURN ADDRESS                 00293700
R15      EQU   15       *USED BY O.S. // ENTRY-PT ADDR, RETURN CODE     00293800
./       ADD   NAME=S99FAIL                                             00293900
         MACRO                                                          00294000
&NAME    S99FAIL &RB=(R14),&RC=(R15),&CPPL=,&MF=G,&CP=                  00294100
         GBLB  &RCPCPPL(2)             CP INDICATOR                     00294200
         GBLC  &RCPPRE                                                  00294300
         LCLB  &GEN                                                     00294400
         LCLC  &C                                                       00294500
&NAME    DS    0H                                                       00294600
         AIF   ('&MF(1)' EQ 'G').GEN                                    00294700
         AIF   ('&MF(1)' EQ 'E').EXEC                                   00294800
         MNOTE 4,'&MF(1) IS AN INVALID MF, MF=G USED'                   00294900
.GEN     LA    R1,FAIL&SYSNDX     LOAD PLIST ADDRESS                    00295000
&GEN     SETB  1                                                        00295100
         AGO   .L                                                       00295200
.EXEC    AIF   ('&MF(2)' NE '').LISTOK                                  00295300
         MNOTE 8,'LIST ADDRESS NOT SPECIFIED'                           00295400
         MEXIT                                                          00295500
.LISTOK  AIF   ('&MF(3)' EQ '').TMF2                                    00295600
&MF(3)   EQU   24                      LENGTH OF PARAMETER LIST         00295700
.TMF2    AIF   ('&MF(2)' EQ '(R1)' OR '&MF(2)' EQ '(1)').L              00295800
         AIF   ('&MF(2)'(1,1) EQ '(').REG                               00295900
         LA    R1,&MF(2)          LOAD DAIRFAIL PARAM LIST ADDRESS      00296000
         AGO   .L                                                       00296100
.REG     ANOP                                                           00296200
&C       SETC  '&MF(2)'(2,K'&MF(2)-2)                                   00296300
         LR    R1,&C              LOAD DAIRFAIL PARAM LIST ADDR         00296400
.L       AIF   ('&RB'(1,1) EQ '(').RBR                                  00296500
         AIF   ('&RB' NE '').RBA                                        00296600
         MNOTE 8,'REQ BLOCK ADDRESS NOT SPECIFIED'                      00296700
         MEXIT                                                          00296800
.RBR     ST    &RB(1),0(R1)       STORE S99 RB ADDRESS                  00296900
         AGO   .RC                                                      00297000
.RBA     LA    R14,&RB            LOAD ADDRESS OF REQ BLOCK             00297100
         ST    R14,0(R1)          AND STORE IN PLIST                    00297200
.RC      AIF   ('&RC'(1,1) EQ '(').RCR                                  00297300
         LA    R14,&RC            LOAD ADDRESS OF RET CODE              00297400
         ST    R14,4(R1)          AND STORE IN PLIST                    00297500
         AGO   .EFF02                                                   00297600
.RCR     ANOP                                                           00297700
.GRC     LA    R14,20(R1)         LOAD ADDR RET CODE FLD                00297800
         ST    &RC(1),0(R14)      STORE RET CODE                        00297900
         ST    R14,4(R1)          AND STORE ITS ADDRESS                 00298000
.EFF02   LA    R14,=A(0)          LOAD ADDR OF FULLWORD OF 0            00298100
         ST    R14,8(R1)          STORE IT.                             00298200
         AIF   ('&CP' EQ 'YES' OR &RCPCPPL(1)).CPID                     00298300
         LA    R14,=X'8032'       LOAD ADDRESS OF CALLERID              00298400
         ST    R14,12(R1)          AND STORE IT                         00298500
         XC    16(4,R1),16(R1)    CLEAR CPPL POINTER                    00298600
         AGO   .GO                                                      00298700
.CPID    LA    R14,=Y(50)         LOAD ADDRESS OF CALLERID              00298800
         ST    R14,12(R1)         AND STORE IT                          00298900
         AIF   ('&CPPL' EQ '').DCPPL                                    00299000
         AIF   ('&CPPL'(1,1) EQ '(').RCPPL                              00299100
         LA    R14,&CPPL          LOAD CPPL ADDRESS                     00299200
         ST    R14,16(R1)          AND STORE IT                         00299300
         AGO   .GO                                                      00299400
.DCPPL   MVC   16(4,R1),&RCPPRE.CPPL MOVE IN CPPL ADDRESS               00299500
         AGO   .GO                                                      00299600
.RCPPL   ST    &CPPL(1),16(R1)    STORE ADDRESS OF CPPL                 00299700
.GO      LINK  EP=IKJEFF18                                              00299800
         AIF   (NOT &GEN).EXIT                                          00299900
         SPACE 1                                                        00300000
         RCPDS                                                          00300100
&C SETC 'FAIL&SYSNDX'                                                   00300200
&C       DS    6F             RESERVE SPACE FOR PARAM LIST              00300300
         RCPDS                                                          00300400
.EXIT    MEND                                                           00300500
./       ADD   NAME=VTCALL                                              00300600
         MACRO                                                          00300700
&LAB     VTCALL &RTN,&TEST                                              00300800
&LAB     LA    R1,VTOCOM      POINT TO THE COMMON AREA                  00300900
         L     R15,VAD&RTN    POINT TO THE ROUTINE                      00301000
         AIF ('&TEST' NE 'TEST').NOTEST                                 00301100
         LTR   R15,R15       SEE IF THE ROUTINE IS PRESENT              00301200
         BZ    *+6           DON'T CALL IT IF IT'S NOT THERE            00301300
.NOTEST  ANOP                                                           00301400
         BALR  R14,R15        THEN CALL IT                              00301500
         MEND                                                           00301600
./       ADD   NAME=VTFMT                                               00301700
         MACRO                                                          00301800
         VTFMT                                                          00301900
*                                                                       00302000
*        THIS DSECT DESCRIBES THE FORMATTED DSCB                        00302100
*                                                                       00302200
VTFMT    DSECT                                                          00302300
VTFNEXT  DS    A              POINTER TO NEXT DSCB                      00302400
VTFALLOC DS    F              ALLOCATION IN UNITS AS DEFINED BY THE     00302500
*                               COMMAND.  KBYTES, MBYTES, TRACKS, OR    00302600
*                               CYLS MAY BE THE UNIT.                   00302700
VTFUSED  DS    F                AMOUNT USED, SAME UNIT                  00302800
VTFUNUSD DS    F                AMOUNT UNUSED, SAME UNIT                00302900
VTFPCT   DS    H                PERCENT USED,  0-100                    00303000
VTFVOLUM DS    CL6            VOLUME SERIAL NUMBER                      00303100
VTFCREDT DS    XL3            CREATION DATE YYDDD                       00303200
VTFEXPDT DS    XL3            EXPIRATION DATE YYDDD                     00303300
VTFLSTAC DS    XL3            LAST ACCESS DATE YYDDD                    00303400
VTFNOEPV DS    AL1            NUMBER OF EXTENTS PER VOLUME              00303500
VTFDSORG DS    CL3            DATA SET ORGANIZATION                     00303600
*                               PS, PO, DA, VS, IS, PERHAPS U           00303700
VTFRECFM DS    CL5            RECORD FORMAT                             00303800
*                               F,V, OR U, B, T, S, A, M                00303900
VTFLRECL DS    H              LOGICAL RECORD LENTGH                     00304000
VTFBLKSZ DS    H              BLOCK SIZE                                00304100
VTFROUND DS    C             R IF ROUND WAS SPECIFIED                   00304200
VTFPROT  DS    C              PASSWORD PROTECTION FLAG                  00304300
VTFCATLG DS    C              CATALOG INDICATION                        00304400
VTFSECAM DS    XL2           SECONDARY AMOUNT                           00304500
VTFSECAL DS    C              SECONDARY ALLOCATION TYPE                 00304600
*                               C FOR CYL, T FOR TRKS, B FOR BLOCKS     00304700
*                               R FOR BLOCKS WITH ROUND                 00304800
VTFDSTYP DS    C              DATA SET TYPE, USER MAY DEFINE            00304900
*                               S = SYSTEM TEMPORARY DATA SET           00305000
*                               T = TEST DATA SET                       00305100
*                               P = PRODUCTION DATA SET                 00305200
VTFACTON DS    CL8            REQUESTED ACTION OR COMMENT               00305300
VTFDSNL  DS    H              LENGTH OF DSNAME                          00305400
VTFMTL   EQU   *-VTFMT        FIXED LENGTH OF THIS DSECT                00305500
VTFDSN   DS    44C            VARIABLE LENGTH FIELD                     00305600
         MEND                                                           00305700
./       ADD   NAME=VTOCMSG                                             00305800
         MACRO                                                          00305900
&LAB     VTOCMSG &MSG1,&MSG2    FIRST LEVEL MESSAGE, OPTIONAL SECOND    00306000
&LAB     LA    R1,&MSG1       POINT TO THE FIRST MESSAGE                00306100
         AIF   ('&MSG2' EQ '').NOSEC  IF NO SECOND LEVEL MSG            00306200
         LA    R0,&MSG2       POINT TO THE SECOND MESSAGE               00306300
         AGO   .SETMSG        SET UP THE MESSAGES                       00306400
.NOSEC   SR    R0,R0          NO SECOND LEVEL MESSAGE                   00306500
.SETMSG  STM   R0,R1,MSGADDRS SAVE THE MESSAGE ADDRESSES                00306600
*        THEN JUST CALL THE MESSAGE ISSUING ROUTINE                     00306700
         VTCALL MSG           AWAY WE GO                                00306800
         MEND                                                           00306900
./       ADD   NAME=VTOCOM                                              00307000
         MACRO                                                          00307100
         VTOCOM  &TYPE                                                  00307200
*                                                                       00307300
*        THIS IS THE VTOC COMMAND COMMON AREA                           00307400
*                                                                       00307500
         AIF   ('&TYPE' EQ 'NODSECT').NODSECT                           00307600
VTOCOM   DSECT                                                          00307700
         AGO   .NODS                                                    00307800
.NODSECT ANOP                                                           00307900
VTOCOM   DS    0D                                                       00308000
.NODS    ANOP                                                           00308100
*                                                                       00308200
*        WORKING STORAGE AREAS FOR THE VARIOUS ROUTINES                 00308300
*                                                                       00308400
VTCWMSG  DS    A              WORKING STORAGE FOR THE MSG  ROUTINE      00308500
VTCWEXIT DS    A              WORKING STORAGE FOR THE EXIT ROUTINE      00308600
VTCWEXCP DS    A              WORKING STORAGE FOR THE EXCP ROUTINE      00308700
VTCWCHEK DS    A              WORKING STORAGE FOR THE CHEK ROUTINE      00308800
VTCWFORM DS    A              WORKING STORAGE FOR THE FORM ROUTINE      00308900
VTCWPRNT DS    A              WORKING STORAGE FOR THE PRNT ROUTINE      00309000
VTCWSORT DS    A              WORKING STORAGE FOR THE SORT ROUTINE      00309100
*                                                                       00309200
*        ADDRESSES OF THE ROUTINES                                      00309300
*                                                                       00309400
VADMSG   DC    V(VTOCMSG)     ADDRESS OF THE MESSAGE ROUTINE            00309500
VADEXIT  DC    V(VTOCEXIT)    ADDRESS OF THE EXIT ROUTINE               00309600
VADEXCP  DC    V(VTOCEXIT)    ADDRESS OF THE EXCP ROUTINE               00309700
VADCHEK  DC    V(VTOCEXIT)    ADDRESS OF THE CHECK ROUTINE              00309800
VADFORM  DC    V(VTOCEXIT)    ADDRESS OF THE FORMAT ROUTINE             00309900
VADPRNT  DC    V(VTOCEXIT)    ADDRESS OF THE PRINT ROUTINE              00310000
VADSORT  DC    V(VTOCEXIT)    ADDRESS OF THE SORT ROUTINE               00310100
*                                                                       00310200
*        TSO COMMAND PROCESSOR AND PARSE DATA                           00310300
*                                                                       00310400
ADDRUPT  DS    A              USER PROFILE TABLE                        00310500
ADDRECT  DS    A              ENVIRONMENT CONTROL TABLE                 00310600
ADDRPSCB DS    A              PROTECTED STEP CONTROL BLOCK              00310700
ADDRCBUF DS    A              COMMAND BUFFER                            00310800
         SPACE                                                          00310900
ADDRANSR DS    A              PARSE ANSWER OR PDL ADDRESS               00311000
         SPACE                                                          00311100
PARMLIST DS    8A             INTERNAL PARM AREA ( MSG )                00311200
ATTNECB  DS    F              ECB FOR ATTENTIONS                        00311300
DOUBLE   DS    D                                                        00311400
         SPACE                                                          00311500
MSGADDRS DS    2A             ADDRESSES OF MESSAGES FOR VTOCMSG         00311600
MSGTEXT1 DS    XL124                                                    00311700
MSGTEXT2 DS    XL124                                                    00311800
         SPACE 3                                                        00311900
PUTOLD1  DS    3F                                                       00312000
PUTOLD2  DS    3F                                                       00312100
*                                                                       00312200
*        PARAMETER LIST FOR THE EXIT ROUTINE                            00312300
*                                                                       00312400
EXITLIST DS    0F                                                       00312500
EXITAREA DS    A       WORKAREA LOCATION                                00312600
DSCBADDR DS    A       ADDRESS OF THE DSCB                              00312700
FORMATAD DS    A       ADDRESS OF THE FORMATTED DSCB                    00312800
CPPLADDR DS    A       ADDRESS OF THE CPPL                              00312900
ACTIONAD DS    A       ADDRESS OF THE RECOMMENDED OR REQUESTED ACTION   00313000
*                                                                       00313100
*        INTER ROUTINE FLAGS                                            00313200
*                                                                       00313300
VTCEFUNC DS    X              VTOCEXCP FUNCTION FLAG                    00313400
VTCFMTCK DS    X              FORMAT IS CALLED BY CHECK RTN             00313500
VTCFMTCD EQU   X'80'          FORMAT WAS CALLED BY CHECK                00313600
VTCFMTCC EQU   X'08'          FORMAT WAS CALLED BY CHECK THIS CALL      00313700
*                                                                       00313800
TABFULL  DS    X             FLAG TABLES FULL, STOP INPUT               00313900
LOCAT    DS    X              FLAG TO PERFORM CATALOG LOCATE            00314000
VTCEPRNT DS    X               PRINT END AND CLEANUP FLAG               00314100
DSNLEN   DS    H              LENGTH OF THE DSNAME (NON-BLANKS)         00314200
ATABTITL DS    A              ADDRESS OF TABLE OF TITLES, LENGTHS       00314300
*                                                                       00314400
*                                                                       00314500
*                                                                       00314600
*        WORKING STORAGE FOR VOLUME UCB SEARCH                          00314700
*                                                                       00314800
ADDR     DS    CL3     UCB ADDRESS IN CHARACTERS                        00314900
VOLSER   DS    CL6     VOLUME SERIAL NUMBER FROM PARSE                  00315000
VOLID    DS    CL6     CURRENT VOLUME SERIAL NUMBER TO PROCESS          00315100
FLAG     DS    X       UCB SEARCH FLAG                                  00315200
LASTADR  DS    F       LAST UCB ADDRESS FOUND ( NO DUP'S )              00315300
*                                                                       00315400
*                                                                       00315500
*                                                                       00315600
SORTTAB  DS    16F                                                      00315700
*                                                                       00315800
* EACH ENTRY CONTAINS A KEY OFFSET (2 BYTES) AND A KEY LENGTH (2 BYTES) 00315900
* THIS TABLE IS BUILT AT PARSE TIME ACCORDING TO THE SORT PARAMETERS    00316000
* SPECIFIED. THE 1ST PARM IS THE HIGH KEY AND SO ON.                    00316100
*                                                                       00316200
*                                                                       00316300
*                                                                       00316400
*        ADDRESSES OF GETMAIN FOR FORMATTED DATA                        00316500
*                                                                       00316600
VTCCURAD DS    A             CURRENT AVAILABLE ADDRESS                  00316700
VTCCURLN DS    A             CURRENT AVAILABLE LENGTH                   00316800
VTCGETMN DS    50A           ADDRESSES OF BLOCKS                        00316900
VTCGETMX EQU  (*-VTCGETMN)/4  NUMBER OF BLOCKS  MAXIMUM                 00317000
VTCGETMS EQU   32768          GETMAIN SIZE                              00317100
*                                                                       00317200
*        HASH SORT TABLE, POINTERS TO FIRST ENTRIES                     00317300
*                                                                       00317400
VTCSORTH DS    256A           POINT TO FORMATED ENTRIES                 00317500
VTCSORTE EQU   *              END OF LIST                               00317600
*                                                                       00317700
*        PRINT ENTRIES - PAGE AND LINE COUNTERS                         00317800
*                                                                       00317900
LINECT   DS    H              LINE COUNT                                00318000
LINEMAX  DS    H              MAXIMUM LINES PER PAGE                    00318100
PAGECT   DS    H              PAGE COUNT                                00318200
LINELEN  DS    H              LENGTH OF THE PRINT LINE                  00318300
*                                                                       00318400
*                                                                       00318500
*          VARIOUS ITEMS                                                00318600
*                                                                       00318700
FMT4     DS    XL44           SPACE FOR DSCB NAME                       00318800
         IECSDSL1 4          SAVE EACH FORMAT 4 DSCB                    00318900
         DS    0D                                                       00319000
FMT3     DS    0XL148         SPACE FOR FORMAT3 DSCB                    00319100
         IECSDSL1 3                                                     00319200
         DS    0D                                                       00319300
         MEND                                                           00319400
./       ADD   NAME=VTOCPARS                                            00319500
         PUSH  PRINT                                                    00319600
         PRINT NOGEN                                                    00319700
PCLMAIN  IKJPARM DSECT=PDL                                              00319800
         SPACE 2                                                        00319900
VOLS     IKJPOSIT DSNAME,VOLSER,LIST,                                  $00320000
               PROMPT='VOLUMES TO SEARCH AND OTHER PARAMETERS',        $00320100
               HELP=('VOLUME SERIAL NUMBERS WHICH ARE TO BE SEARCHED FO$00320200
               DATA SETS TO LIST')                                      00320300
         SPACE 2                                                        00320400
LEVKEY   IKJKEYWD                                                       00320500
         IKJNAME 'LEVEL',SUBFLD=SUBLEV                                  00320600
         SPACE 2                                                        00320700
ENDKEY   IKJKEYWD                                                       00320800
         IKJNAME 'ENDING',SUBFLD=SUBEND                                 00320900
         SPACE 2                                                        00321000
CONTAINK IKJKEYWD                                                       00321100
         IKJNAME 'CONTAINING',SUBFLD=SUBCONT                            00321200
         SPACE 2                                                        00321300
         SPACE 2                                                        00321400
SPACEK   IKJKEYWD DEFAULT='TRKS'                                        00321500
         IKJNAME 'TRKS'                                                 00321600
         SPACE 2                                                        00321700
CATK     IKJKEYWD                                                       00321800
         IKJNAME 'CAT'                                                  00321900
         SPACE 2                                                        00322000
SORTK    IKJKEYWD                                                       00322100
         IKJNAME 'SORT',SUBFLD=SUBSORTS                                 00322200
         IKJNAME 'NOSORT'                                               00322300
         SPACE 2                                                        00322400
BREAKK   IKJKEYWD                                                       00322500
         IKJNAME 'BREAK',SUBFLD=SUBBREAK                                00322600
         SPACE 2                                                        00322700
LIMITK   IKJKEYWD                                                       00322800
         IKJNAME 'LIMIT',SUBFLD=SUBLIMIT                                00322900
         SPACE 2                                                        00323000
ANDOR1K  IKJKEYWD                                                       00323100
         IKJNAME 'AND1',SUBFLD=SUBAO1,ALIAS='AND'                       00323200
         IKJNAME 'OR1',SUBFLD=SUBAO1,ALIAS='OR'                         00323300
         SPACE 2                                                        00323400
ANDOR2K  IKJKEYWD                                                       00323500
         IKJNAME 'AND2',SUBFLD=SUBAO2                                   00323600
         IKJNAME 'OR2',SUBFLD=SUBAO2                                    00323700
         SPACE 2                                                        00323800
ANDOR3K  IKJKEYWD                                                       00323900
         IKJNAME 'AND3',SUBFLD=SUBAO3                                   00324000
         IKJNAME 'OR3',SUBFLD=SUBAO3                                    00324100
         SPACE 2                                                        00324200
PRINTK   IKJKEYWD                                                       00324300
         IKJNAME 'PRINT',SUBFLD=SUBPRINT                                00324400
         IKJNAME 'NOPRINT'                                              00324500
         SPACE 2                                                        00324600
CHARSK   IKJKEYWD                                                       00324700
         IKJNAME 'CHARS',SUBFLD=SUBCHARS                                00324800
         SPACE 2                                                        00324900
LINESK   IKJKEYWD                                                       00325000
         IKJNAME 'LINES',SUBFLD=SUBLINES                                00325100
         SPACE 2                                                        00325200
HEADK    IKJKEYWD                                                       00325300
         IKJNAME 'HEADING',SUBFLD=SUBHEAD                               00325400
         IKJNAME 'NOHEADING'                                            00325500
         SPACE 2                                                        00325600
TOTALK   IKJKEYWD                                                       00325700
         IKJNAME 'TOTALS',SUBFLD=SUBTOTAL                               00325800
         SPACE 2                                                        00325900
OUTPUTK  IKJKEYWD                                                       00326000
         IKJNAME 'OUTPUT'                                               00326100
         SPACE 2                                                        00326200
FORMATK  IKJKEYWD                                                       00326300
         IKJNAME 'FORMAT',SUBFLD=SUBFORMT                               00326400
         SPACE 2                                                        00326500
DSNPLNK  IKJKEYWD                                                       00326600
         IKJNAME 'DSNLEN',SUBFLD=SUBDSNLN                               00326700
         SPACE 5                                                        00326800
SUBLEV   IKJSUBF                                                        00326900
LEVEL    IKJPOSIT DSNAME,LIST,                                         X00327000
               PROMPT='BEGINNING CHARACTERS OF DSNAMES TO PROCESS'      00327100
         SPACE 2                                                        00327200
SUBEND   IKJSUBF                                                        00327300
ENDING   IKJPOSIT DSNAME,LIST,                                         X00327400
               PROMPT='ENDING CHARACTERS OF DSNAMES TO PROCESS'         00327500
         SPACE 2                                                        00327600
SUBCONT  IKJSUBF                                                        00327700
CONTAIN  IKJPOSIT DSNAME,LIST,                                         X00327800
               PROMPT='CHARACTER STRING CONTAINED IN DSNAMES TO PROCESSX00327900
               '                                                        00328000
         SPACE 2                                                        00328100
SUBSORTS IKJSUBF                                                        00328200
SUBSORT  IKJIDENT 'SORT FIELDS',LIST,FIRST=ALPHA,MAXLNTH=6              00328300
         SPACE 2                                                        00328400
SUBBREAK IKJSUBF                                                        00328500
BREAK    IKJIDENT 'NUMBER OF CHARACTERS FOR A BREAK',FIRST=NUMERIC,    X00328600
               OTHER=NUMERIC,MAXLNTH=2,DEFAULT='3'                      00328700
         SPACE 2                                                        00328800
SUBCHARS IKJSUBF                                                        00328900
CHARSPL  IKJIDENT 'NUMBER OF CHARACTERS PER LINE   ',FIRST=NUMERIC,    X00329000
               OTHER=NUMERIC,MAXLNTH=3                                  00329100
BLKSZSET IKJIDENT 'PHYSICAL BLOCK SIZE',FIRST=NUMERIC,OTHER=NUMERIC,   X00329200
               MAXLNTH=5                                                00329300
         SPACE 2                                                        00329400
SUBLINES IKJSUBF                                                        00329500
LINESPP  IKJIDENT 'NUMBER OF LINES PER PAGE        ',FIRST=NUMERIC,    X00329600
               OTHER=NUMERIC,MAXLNTH=3                                  00329700
         SPACE 2                                                        00329800
SUBPRINT IKJSUBF                                                        00329900
SUBPRTKY IKJIDENT 'ADD, REP, NEW, OR DEL',                             X00330000
               FIRST=ALPHA,OTHER=ALPHA,MAXLNTH=3                        00330100
SUBPRTIT IKJIDENT 'ITEMS TO PRINT',LIST,FIRST=ALPHA,MAXLNTH=6           00330200
         SPACE 2                                                        00330300
SUBHEAD  IKJSUBF                                                        00330400
HEADING  IKJPOSIT QSTRING                                               00330500
         SPACE 2                                                        00330600
SUBTOTAL IKJSUBF                                                        00330700
TOTALN   IKJIDENT 'NUMBER OF CHARACTERS FOR TOTALS',FIRST=NUMERIC,     X00330800
               OTHER=NUMERIC,MAXLNTH=2,DEFAULT='0'                      00330900
         SPACE 3                                                        00331000
SUBLIMIT IKJSUBF                                                        00331100
SUBLKEY IKJIDENT 'FIELD IN DATA SET CONTROL BLOCK TO COMPARE',         X00331200
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8                     00331300
         SPACE 2                                                        00331400
SUBLOPER IKJIDENT 'OPERATOR FOR COMPARISON',FIRST=ALPHA,OTHER=ALPHA,   X00331500
               MAXLNTH=2,                                              X00331600
               PROMPT=' OPERATORS ARE EQ, NE, LT, LE, GT, AND GE'       00331700
         SPACE 2                                                        00331800
SUBLVALU IKJIDENT 'COMPARISON VALUE',FIRST=ALPHANUM,                   X00331900
               OTHER=ALPHANUM,                                         X00332000
               PROMPT='VALUE TO COMPARE FOR DATA SET KEYWORDS'          00332100
         SPACE 3                                                        00332200
SUBAO1   IKJSUBF                                                        00332300
SUB1KEY IKJIDENT 'FIELD IN DATA SET CONTROL BLOCK TO COMPARE',         X00332400
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8                     00332500
         SPACE 2                                                        00332600
SUB1OPER IKJIDENT 'OPERATOR FOR COMPARISON',FIRST=ALPHA,OTHER=ALPHA,   X00332700
               MAXLNTH=2,                                              X00332800
               PROMPT=' OPERATORS ARE EQ, NE, LT, LE, GT, AND GE'       00332900
         SPACE 2                                                        00333000
SUB1VALU IKJIDENT 'COMPARISON VALUE',FIRST=ALPHANUM,                   X00333100
               OTHER=ALPHANUM,                                         X00333200
               PROMPT='VALUE TO COMPARE FOR DATA SET KEYWORDS'          00333300
         SPACE 2                                                        00333400
SUBAO2   IKJSUBF                                                        00333500
SUB2KEY IKJIDENT 'FIELD IN DATA SET CONTROL BLOCK TO COMPARE',         X00333600
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8                     00333700
         SPACE 2                                                        00333800
SUB2OPER IKJIDENT 'OPERATOR FOR COMPARISON',FIRST=ALPHA,OTHER=ALPHA,   X00333900
               MAXLNTH=2,                                              X00334000
               PROMPT=' OPERATORS ARE EQ, NE, LT, LE, GT, AND GE'       00334100
         SPACE 2                                                        00334200
SUB2VALU IKJIDENT 'COMPARISON VALUE',FIRST=ALPHANUM,                   X00334300
               OTHER=ALPHANUM,                                         X00334400
               PROMPT='VALUE TO COMPARE FOR DATA SET KEYWORDS'          00334500
         SPACE 2                                                        00334600
SUBAO3   IKJSUBF                                                        00334700
SUB3KEY IKJIDENT 'FIELD IN DATA SET CONTROL BLOCK TO COMPARE',         X00334800
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=8                     00334900
         SPACE 2                                                        00335000
SUB3OPER IKJIDENT 'OPERATOR FOR COMPARISON',FIRST=ALPHA,OTHER=ALPHA,   X00335100
               MAXLNTH=2,                                              X00335200
               PROMPT=' OPERATORS ARE EQ, NE, LT, LE, GT, AND GE'       00335300
         SPACE 2                                                        00335400
SUB3VALU IKJIDENT 'COMPARISON VALUE',FIRST=ALPHANUM,                   X00335500
               OTHER=ALPHANUM,                                         X00335600
               PROMPT='VALUE TO COMPARE FOR DATA SET KEYWORDS'          00335700
         SPACE 2                                                        00335800
SUBFORMT IKJSUBF                                                        00335900
FORMATSP IKJIDENT 'FORMAT TYPES TO OUTPUT',LIST,FIRST=NUMERIC,         X00336000
               MAXLNTH=1,DEFAULT='1'                                    00336100
         SPACE 2                                                        00336200
SUBDSNLN IKJSUBF                                                        00336300
DSNPLN   IKJIDENT 'LENGTH OF DSNAME TO PRINT',FIRST=NUMERIC,           X00336400
               OTHER=NUMERIC,MAXLNTH=2,DEFAULT='44'                     00336500
DSNLNTYP IKJKEYWD DEFAULT='TRUNCATE'                                    00336600
         IKJNAME 'TRUNCATE'                                             00336700
         IKJNAME 'MULTILINE'                                            00336800
         IKJENDP                                                        00336900
         SPACE 2                                                        00337000
         POP   PRINT                                                    00337100
ACTION   EQU   1                                                        00337200
VOLUME   EQU   2                                                        00337300
CDATE    EQU   3                                                        00337400
LSTUS    EQU   4                                                        00337500
EXPDT    EQU   5                                                        00337600
ALLOC    EQU   6                                                        00337700
UNUSED   EQU   7                                                        00337800
PCT      EQU   8                                                        00337900
EXT      EQU   9                                                        00338000
DSORG    EQU   10                                                       00338100
RECFM    EQU   11                                                       00338200
BLKSZ    EQU   12                                                       00338300
LRECL    EQU   13                                                       00338400
PASS     EQU   14                                                       00338500
CAT      EQU   15                                                       00338600
SECT     EQU   16                                                       00338700
SECQ     EQU   17                                                       00338800
UNIT     EQU   18                                                       00338900
ROUND    EQU   19                                                       00339000
TYPE     EQU   20                                                       00339100
USED     EQU   21                                                       00339200
CCHH     EQU   22                                                       00339300
DUMMY3   EQU   23                                                       00339400
DUMMY4   EQU   24                                                       00339500
DUMMY5   EQU   25                                                       00339600
DSNAME   EQU   26                                                       00339700
./ ENDUP                                                                00339800
/*                                                                      00339900
//*----------------------------------------------------------- LOADMACS 00340000
//*                                                                     00340100
//IEBUPDTE.SYSIN DD *                                                   00340200
./       ADD   NAME=VTOC                                                00340300
)F FUNCTION -                                                           00340400
  THE VTOC COMMAND DISPLAYS SELECTED DATA SETS ON A DISK OR SET OF      00340500
  DISKS.  EACH DISK HAS A VOLUME TABLE OF CONTENTS ( VTOC ).  THIS      00340600
  TABLE IS SEARCHED FOR DATA SETS THAT MEET THE SPECIFICATIONS.         00340700
)X SYNTAX -                                                             00340800
         VTOC     'VOLUME-LIST'           LEVEL('DSNAME-START')         00340900
                                          CONTAINING('DSNAME-STRING')   00341000
                                          ENDING('DSNAME-END')          00341100
                                          BREAK('BREAK-CHARS')          00341200
                  CAT                                                   00341300
                  NOSORT/SORT('SORT-FIELDS')                            00341400
                  NOPRINT/PRINT('PRINT-OP' ('PRINT-ITEM-LIST'))         00341500
                  LIMIT('KEYWORD' 'OPER' 'VALUE')                       00341600
                  AND1/OR1('KEYWORD' 'OPER' 'VALUE')                    00341700
                  AND2/OR2('KEYWORD' 'OPER' 'VALUE')                    00341800
                  AND3/OR3('KEYWORD' 'OPER' 'VALUE')                    00341900
                  CHARS('CHARS-PER-LINE')                               00342000
                  LINES('LINES-PER-PAGE')                               00342100
                  NOHEADING/HEADING('TEXT')                             00342200
                  DSNLEN('LENGTH')                                      00342300
  REQUIRED - 'VOLUME-LIST'                                              00342400
  DEFAULTS - LIST ALL DATA SETS ON THE VOLUME(S) SELECTED.              00342500
             SORT, PRINT                                                00342600
)O OPERANDS -                                                           00342700
))'VOLUME-LIST'     - A VOLUME SERIAL NUMBER OR A LIST OF VOLUMES.      00342800
              IF THE FIRST ONE TO FIVE CHARACTERS OF A VOLUME           00342900
              SERIAL NUMBER ARE ENTERED, ALL VOLUMES THAT ARE           00343000
              MOUNTED ON THE MACHINE WHICH START WITH THOSE             00343100
              CHARACTERS WILL BE LISTED.                                00343200
              IF 'ALL' IS SPECIFIED, ALL NON-VIRTUAL VOLUMES            00343300
              WHICH ARE ONLINE AND READY WILL BE PROCESSED.             00343400
              IF 'ALV' IS SPECIFIED, ALL VIRTUAL VOLUMES                00343500
              WHICH ARE ONLINE AND READY WILL BE PROCESSED,             00343600
              IF THEY ARE MOUNTED PRIVATE/RESERVED OR                   00343700
              PRIVATE/RESIDENT.                                         00343800
))LEVEL('DSNAME-START')  - SPECIFIES THE HIGH LEVEL QUALIFIERS TO BE    00343900
         SEARCHED.  THIS WILL NOT BE PREFIXED BY YOUR USERID OR         00344000
         PREFIX.  ONLY DATA SETS STARTING WITH THESE PREFIXES WILL      00344100
         BE LISTED.                                                     00344200
))CONTAINING('DSNAME-STRING') - SPECIFIES A CHARACTER STRING CONTAINED  00344300
         IN THE DATA SET NAME.  AT LEAST ONE OF THE STRINGS MUST        00344400
         BE IN THE DSNAME FOR THE DATA SET TO BE LISTED.                00344500
         THESE STRINGS MUST CONFORM TO DSNAME STANDARDS.                00344600
         THEY CANNOT BEGIN WITH A PERIOD OR A NUMBER.                   00344700
))ENDING('DSNAME-END')  - SPECIFIES THE ENDING CHARACTERS OF THE        00344800
         DSNAME.  THE FINAL NONBLANK CHARACTERS OF THE DSNAME MUST      00344900
         BE ONE OF THESE STRINGS TO ALLOW THE DATA SET TO BE LISTED.    00345000
         THESE STRINGS MUST CONFORM TO DSNAME STANDARDS.                00345100
))CAT      -  A LOCATE IS DONE FOR EACH DSNAME ON THE VOLUMES LISTED    00345200
         AND STATUS IS INDICATED.  NOTE -  THIS OPTION USES A           00345300
         CONSIDERABLE AMOUNT OF PROCESSING TIME.                        00345400
         C -  CATALOGGED ON THIS VOLUME                                 00345500
         N -  NOT CATALOGGED                                            00345600
         W -  CATALOGGED ON ANOTHER VOLUME                              00345700
         E -  CATALOG PROCESSING ERROR                                  00345800
))NOSORT   -  THE DATA SETS ARE NOT SORTED.  THEY ARE OUTPUT AS THEY    00345900
              ARE FOUND.                                                00346000
))SORT('SORT-FIELDS') DATA SETS ARE SORTED INTO ALPHABETICAL ORDER,     00346100
              BASED UPON THE SORT FIELDS SPECIFIED.                     00346200
              DSNAME,VOLUME,ALLOC,USED,UNUSED,PCT,EX,DSO,RFM,           00346300
              LRECL,BLKSZ,CDATE,EXPDT,REFDT ARE VALID SORT FIELDS.      00346400
              'A/D'   ARE REQUIRED AFTER EACH SORT FIELD TO INDICATE    00346500
              ASCENDING/DESCENDING SEQUENCE.                            00346600
))BREAK('BREAK-CHARS') - THE LISTING WILL CONTAIN A NEW HEADER,         00346700
              ( ON A NEW PAGE IF THE VTOCOUT DD CARD OPTION IS USED ),  00346800
              WHENEVER THE SPECIFIED NUMBER OF CHARACTERS DIFFERS       00346900
              FROM THE PRECEDING DATA SET.  THIS OPTION FUNCTIONS       00347000
              ONLY WITH THE SORT OPTION.                                00347100
))CHARS('CHARS-PER-LINE') - SPECIFIES THE NUMBER OF CHARACTERS ON EACH  00347200
              LINE OF OUTPUT.  THE DEFAULT IS 150 FOR PRINT AND THE     00347300
              LINESIZE OF THE TERMINAL FOR TSO SESSIONS.                00347400
              YOU CAN GET MORE INFORMATION BY SPECIFYING A LARGER       00347500
              NUMBER OF CHARACTERS PER LINE OR YOU CAN LIMIT THE        00347600
              PRINTING BY SETTING A SMALLER NUMBER OF CHARACTERS        00347700
              PER LINE OF OUTPUT.                                       00347800
))LINES('LINES-PER-PAGE') - SPECIFIES THE NUMBER OF LINES BEFORE A NEW  00347900
              TITLE LINE IS PRODUCED.  IT DEFAULTS TO 60 FOR PRINT      00348000
              AND TO THE SCREEN SIZE FOR TSO SESSIONS.                  00348100
))NOHEADING   - DO NOT PRODUCE A HEADING.  THE HEADING WILL ONLY BE     00348200
              OUTPUT IF THE VTOCOUT DD STATEMENT IS PRESENT.            00348300
))HEADING('TEXT') - IF A DD STATEMENT WITH A DDNAME OF VTOCOUT IS       00348400
              PRESENT, THIS TEXT WILL BE USED TO BEGIN EVERY PAGE.      00348500
              CARRIAGE CONTROL SHOULD BE INCLUDED ( ASA ).  THE         00348600
              DEFAULT HEADER CONSISTS OF VTOC COMMAND VERSION 02        00348700
              AND THE COMMAND THAT WAS ENTERED.                         00348800
))DSNLEN('LENGTH') - SPECIFIES THE LENGTH OF THE DSNAME TO PRINT.       00348900
              THE REST OF THE DSNAME IS TRUNCATED.  THE CHARS PARAMETER 00349000
              WILL ALSO CAUSE THE DSNAME TO BE TRUNCATED, IF THE NAME   00349100
              AND THE PRECEDING INFORMATION EXCEEDS THE PRINT LINE.     00349200
))NOPRINT     - SPECIFIES THAT INDIVIDUAL ITEMS ARE NOT TO BE LISTED.   00349300
              THE COMMAND CAN BE USED TO CALCULATE TOTALS.              00349400
))PRINT('PRINT-OP' ('PRINT-ITEM-LIST')) -                               00349500
              SPECIFIES THE ITEMS TO PRINT.  THE 'PRINT-OP' IS THE      00349600
              OPERATION TO BE DONE.  THEY INCLUDE THE FOLLOWING.        00349700
                NEW - THE 'PRINT-ITEM-LIST' IS A COMPLETE LIST OF WHAT  00349800
                      TO PRINT.                                         00349900
                REP - THE FIRST 'PRINT-ITEM' WILL BE REPLACED WITH THE  00350000
                      REST OF THE ITEMS ON THE LIST.                    00350100
                ADD - THE REST OF THE 'PRINT-ITEM-LIST' WILL BE ADDED   00350200
                      AFTER THE FIRST ITEM ON THE LIST.                 00350300
                DEL - THE ITEMS ON THE 'PRINT-ITEM-LIST' WILL NOT BE    00350400
                      PRINTED.                                          00350500
              THE 'PRINT-ITEM-LIST' NAMES ARE THE SAME KEYWORDS USED    00350600
              IN LIM, AND, AND OR FUNCTIONS AND ARE ALSO THE TITLES     00350700
              AS PRINTED.                                               00350800
              THE ADD, DEL, AND REP PRINT OPERATIONS REFER TO THE       00350900
              DEFAULT PRINT LIST.  THE DEFAULT LIST IS ALLOC, UNUSED,   00351000
              PCT, EX, DSO, RFM, BLKSZ, LRECL, REFDT, CDATE, VOLUME,    00351100
              DSNAME, EXPDT, SECQ, SECT, ROUND, PASS, ACTION,           00351200
              AND TYPE.  AS NOTED UNDER CHARS ABOVE, ONLY THE ITEMS     00351300
              THAT WILL FIT ON THE PRINT LINE WILL BE LISTED.           00351400
))LIMIT('VALUE' 'OPER' 'KEYWORD') - SPECIFIES WHICH DATA SETS ARE TO    00351500
              BE LISTED.  ONLY DATA SETS THAT SATISFY THE RELATION      00351600
              ARE LISTED.                                               00351700
))'KEYWORD'   - IS THE NAME OF A DATA SET FIELD.  THE LIST OF CURRENTLY 00351800
                PROVIDED FIELDS FOLLOWS.  THE KEYWORDS AND THEIR VALUES 00351900
                ARE THE SAME AS IN THE VTOC OUTPUT.                     00352000
                                                                        00352100
                      ALLOC      DATA SET ALLOCATION                    00352200
                                    NUMBER OF KILOBYTES, TRACKS,        00352300
                                    CYLINDERS, OR MEGABYTES ALLOCATED.  00352400
                                    DEFAULT IS KILOBYTES.               00352500
                      UNUSED     AMOUNT OF UNUSED SPACE IN THE DATA     00352600
                                    SET.  SAME UNITS AS IN ALLOC.       00352700
                      USED       AMOUNT OF SPACE USED IN THE DATA SET.  00352800
                                    SAME UNITS AS IN ALLOC.             00352900
                      PCT        PERCENTAGE OF SPACE USED IN THE DATA   00353000
                                    USED.                               00353100
                      EX         NUMBER OF EXTENTS IN THE DATA SET.     00353200
                      DSO        DATA SET ORGANIZATION                  00353300
                                    PS = SEQUENTIAL   PO = PARTITIONED  00353400
                                    VS = VSAM         IS = ISAM         00353500
                                    DA = DIRECT ACCESS  U = UNMOVEABLE  00353600
                      RFM        RECORD FORMAT                          00353700
                                    F  = FIXED        V  = VARIABLE     00353800
                                    U  = UNDEFINED    B  = BLOCKED      00353900
                                    T  = TRACK OVERFLOW                 00354000
                                    S  = SPANNED OR STANDARD            00354100
                                    A  = ASA CARRIAGE CONTROL           00354200
                                    M  = MACHINE CARRIAGE CONTROL       00354300
                      BLKSZ      BLOCKSIZE FOR PHYSICAL BLOCKS OF       00354400
                                    DATA.                               00354500
                      LRECL      LOGICAL RECORD LENGTH IN BYTES.        00354600
                      CDATE      CREATION DATE IN THE FORM YYDDD,       00354700
                                    SOMETIMES CALLED JULIAN.            00354800
                      EXPDT      EXPIRATION DATE IN THE SAME FORM.      00354900
                                    THIS FIELD IS RARELY USED HERE.     00355000
                      REFDT      LAST USE DATE IN THE SAME FORM.        00355100
                                    THIS DATE IS WHEN THE DATA SET WAS  00355200
                                    LAST OPENED.                        00355300
                      SECT       TYPE OF ALLOCATION                     00355400
                                    A  = ABSOLUTE TRACK                 00355500
                                    B  = BLOCKS                         00355600
                                    T  = TRACKS                         00355700
                                    C  = CYLINDERS                      00355800
                      PASS       PROTECTION INDICATORS                  00355900
                                    N  = NONE                           00356000
                                    R  = READ AND WRITE PROTECTION      00356100
                                    W  = WRITE PROTECTION               00356200
                      ROUND      SPACE ROUNDED UP TO CYLINDERS          00356300
                                    R  = ROUND                          00356400
                                    N  = NO ROUND                       00356500
                      CCHH       CYLINDER AND HEAD ADDRESS, IN 4 OR 8   00356600
                                    HEXADECIMAL DIGITS.  IF 4 DIGITS    00356700
                                    ARE USED, ONLY THE CYLINDER IS USED 00356800
                                    FOR COMPARISON, OTHERWISE, THE CYL  00356900
                                    AND TRACK ARE COMPARED.             00357000
                      VOLUME     VOLUME SERIAL NUMBER OR DISK NAME      00357100
                      DSNAME     NAME OF THE DATA SET                   00357200
                      ACTION     SOME ERROR INDICATIONS                 00357300
                      TYPE       RESERVED FOR EXIT USAGE.               00357400
))'OPER'      - IS AN OPERATOR.  THE LIST OF OPERATORS FOLLOWS.         00357500
                        EQ       IS EQUAL TO                            00357600
                        NE       IS NOT EQUAL TO                        00357700
                        LE       IS LESS THAN OR EQUAL TO               00357800
                        LT       IS LESS THAN                           00357900
                        GE       IS GREATER THAN OR EQUAL TO            00358000
                        GT       IS GREATER THAN                        00358100
))'VALUE'     - GIVES THE VALUE OF THE ITEM FOR COMPARISON, SUCH AS     00358200
              FB, PS, R, OR A NUMBER.                                   00358300
))AND1('VALUE' 'OPER' 'KEYWORD') -  SPECIFIES WHICH DATA SETS ARE TO    00358400
                 BE LISTED.  BOTH THE LIMIT AND THIS CONDITION MUST     00358500
                 BE TRUE TO ALLOW THE LISTING.                          00358600
))OR1('VALUE' 'OPER' 'KEYWORD') -   SPECIFIES WHICH DATA SETS ARE TO    00358700
                 BE LISTED.  EITHER THE LIMIT OR THIS CONDITION MUST    00358800
                 BE TRUE TO ALLOW THE LISTING.                          00358900
))AND2('VALUE' 'OPER' 'KEYWORD') -  SPECIFIES WHICH DATA SETS ARE TO    00359000
                 BE LISTED.  BOTH THE PREVIOS RESULT AND THIS           00359100
                 CONDITION MUST BE TRUE TO ALLOW THE LISTING.           00359200
))OR2('VALUE' 'OPER' 'KEYWORD') -   SPECIFIES WHICH DATA SETS ARE TO    00359300
                 BE LISTED.  EITHER THE PREVIOUS RESULT OR THIS         00359400
                 CONDITION MUST BE TRUE TO ALLOW THE LISTING.           00359500
))AND3('VALUE' 'OPER' 'KEYWORD') -  SPECIFIES WHICH DATA SETS ARE TO    00359600
                 BE LISTED.  BOTH THE PREVIOS RESULT AND THIS           00359700
                 CONDITION MUST BE TRUE TO ALLOW THE LISTING.           00359800
))OR3('VALUE' 'OPER' 'KEYWORD') -   SPECIFIES WHICH DATA SETS ARE TO    00359900
                 BE LISTED.  EITHER THE PREVIOUS RESULT OR THIS         00360000
                 CONDITION MUST BE TRUE TO ALLOW THE LISTING.           00360100
))EXAMPLES -                                                            00360200
  LIST ALL DATA SETS ON VOL             VTOC VOL                        00360300
  LIST ALL DATA SETS ON ALL VOLUMES     VTOC ALL                        00360400
  LIST ALL DATA SETS THAT START                                         00360500
     WITH  XXX   ON ANY MVXXXX VOLUME   VTOC MV LEV(XXX)                00360600
  LIST ALL RECENTLY CREATED DATA SETS   VTOC VOL LIM(CDATE GT 79001)    00360700
  LIST DATA SETS WITH UNUSED SPACE      VTOC VOL LIM(PCT LT 50)  -      00360800
                                                OR1(UNUSED GT 30)       00360900
  LIST DATA SETS WITH MULTIPLE EXTENTS  VTOC VOL LIM(EX GT 1)           00361000
  LIST ALL OF USER'S DATA ON TSO001   VTOC TSO001 LEVEL(TMTCEXX)        00361100
  LIST ALL DATA SETS OVER 100 TRKS      VTOC VOL LIM(ALLOC GT 100)      00361200
  LIST DATA SETS UNDER THE FIXED HEADS  VTOC VOL LIM(CC EQ 0001) -      00361300
                                                 OR1(CC EQ 0002)        00361400
  LIST USED SPACE INSTEAD OF UNUSED,                                    00361500
       IN TRACKS.                VTOC VOL PRINT(REP (UNUSED USED)) TRK  00361600
  LIST CLIST DATA SETS           VTOC VOL END(CLIST)                    00361700
  LIST ALL OF THE INFORMATION ABOUT                                     00361800
       USER'S DATA SETS AT A 3270.   VTOC VOL CHAR(150)                 00361900
  CHECK IF DATASETS ARE CATALOGGED ON THIS VOLUME.                      00362000
                  VTOC VOL CAT PRINT(NEW (ALLOC PCT CAT DSNAME))        00362100
  LIST ALL DATA SETS ON 335XXX VOLUMES SORTED BY ALLOC IN DESCENDING    00362200
       SEQUENCE, VOLUME AND DSNAME IN ASCENDING SEQUENCE.               00362300
       VTOC 335 SORT(ALLOC,D,VOLUME,A,DSNAME,A)                         00362400
./ ENDUP                                                                00362500
/*                                                                      00362600
//*----------------------------------------------------------- IEBUPDTE 00362700
//*                                                                     00362800
//ASM1.SYSIN DD *                                                       00362900
         TITLE 'VTOC COMMAND - LIST DATA SETS AND ATTRIBUTES'           00363000
*********************************************************************** 00363100
*                                                                     * 00363200
*                                                                     * 00363300
* TITLE -      VTOC COMMAND - LIST DATA SETS AND ATTRIBUTES           * 00363400
*                                                                     * 00363500
* FUNCTION -   PROVIDE THE ABILITY FOR A TSO USER OR A BATCH JOB      * 00363600
*              TO LIST THE CONTENTS OF VARIOUS VOLUMES, WITH A        * 00363700
*              FAIR AMOUNT OF SELECTION.                              * 00363800
*                                                                     * 00363900
*                                                                     * 00364000
* OPERATION -  ACCEPT FROM THE TSO USER OR BATCH JOB A COMMAND        * 00364100
*              WITH THE FOLLOWING SYNTAX.  THEN CHECK THE COMMAND     * 00364200
*              AND LOOP THROUGH, GETTING A DSCB, FORMATTING IT,       * 00364300
*              PERFORMING THE DATA SET NAME AND LIMIT CHECKS, AND     * 00364400
*              CALLING AN EXIT ROUTINE IF DESIRED, THEN PUT THE       * 00364500
*              ENTRY IN THE CORRECT SORT SEQUENCE.                    * 00364600
*              FINALLY CALL THE PRINT ROUTINE TO PRINT THE            * 00364700
*              SPECIFIED ITEMS, HEADERS, AND BREAKS, OR JUST          * 00364800
*              THE TOTALS.                                            * 00364900
*                                                                     * 00365000
*                                                                     * 00365100
* INPUT -      STANDARD COMMAND PROCESSOR PARAMETER LIST              * 00365200
*              POINTED TO BY REGISTER 1                               * 00365300
*                                                                     * 00365400
*                                                                     * 00365500
* OUTPUT -     TO SYSOUT, A LIST OF THE REQUESTED DATA SETS AND       * 00365600
*              THEIR ATTRIBUTES.                                      * 00365700
*                                                                     * 00365800
*                                                                     * 00365900
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00366000
*                                                                     * 00366100
*                                                                     * 00366200
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00366300
*              FIREMAN'S FUND INSURANCE  CPSD 2N                      * 00366400
*              ONE LUCAS GREEN                                        * 00366500
*              SAN RAFAEL, CA  94911                                  * 00366600
*                                                                     * 00366700
*                                                                     * 00366800
*********************************************************************** 00366900
*                                                                       00367000
         MACRO                                                          00367100
&LABEL   VTOCEXCP  &FUNC                                                00367200
         AIF   ('&FUNC' NE 'EQ').CALL                                   00367300
VTCOPEN  EQU   1              DEFINE FUNCTION CODES FOR VTOCEXCP        00367400
VTCCLOSE EQU   2                                                        00367500
VTCREAD  EQU   0                                                        00367600
         MEXIT                                                          00367700
.CALL    ANOP                 CALL VTOCEXCP                             00367800
&LABEL   MVI   VTCEFUNC,VTC&FUNC   SET THE FUNCTION CODE                00367900
         VTCALL EXCP          GO GET A DSCB                             00368000
         MEND                                                           00368100
*                                                                       00368200
*        MACRO FOR INITIALIZING SUBROUTINE WORK AREA ADDRESSES          00368300
*                                                                       00368400
         MACRO                                                          00368500
&LABEL   WORKADDR &RTN,&PRMADDR                                         00368600
&LABEL   L     R1,=A(WORK&RTN-WORKAREA)  GET THE OFFSET ( OVER 4K )     00368700
         LA    R1,0(R1,R13)   RELOCATE IT                               00368800
         ST    R1,&PRMADDR   THEN STORE IT FOR THE ROUTINES             00368900
         MEND                                                           00369000
*                                                                       00369100
         EJECT                                                          00369200
VTOCCMD  ENTERX 12,(1,LENWORK,C)     DO THE HOUSEKEEPING                00369300
         LR    R2,R1          SAVE ADDR OF CPPL                         00369400
         SPACE                                                          00369500
         USING WORKAREA,WORKREG                                         00369600
         EJECT                                                          00369700
         BAL   R14,PARSINIT   PERFORM THE PARSING                       00369800
         LTR   R15,R15        TEST THE RETURN CODE                      00369900
         BNZ   RETURN         BAD NEWS, GET OUT                         00370000
         VTCALL PRNT         INITIALIZE FOR PRINTING                    00370100
         L     R9,ADDRANSR    ADDR OF PARSE DESCRIPTOR LIST             00370200
         USING PDL,R9         RETURNED BY PARSE                         00370300
*                                                                       00370400
*                                                                       00370500
*        SCAN SORT PARSE LIST AND BUILD SORT FIELD TABLE                00370600
*                                                                       00370700
*                                                                       00370800
SORTPAR  LA    R4,SUBSORT     SORT PARSE LIST                           00370900
         LA    R5,SORTTAB     SORT FIELD TABLE                          00371000
         XC    0(64,R5),0(R5) CLEAR SORT FIELD TABLE                    00371100
         MVC   0(4,R5),SORTTABX DEFAULT TO DSNAME                       00371200
         SPACE 1                                                        00371300
SORTPAR1 LA    R1,SORTTABX-12 SORT COMPARE TABLE                        00371400
         SPACE 1                                                        00371500
SORTPAR2 LA    R1,12(0,R1)    POINT TO NEXT COMPARE ENTRY               00371600
         CLC   0(4,R1),=F'0'  END OF TABLE                              00371700
         BE    SORTPAR3       ITEM NOT FOUND, IGNORE                    00371800
         L     R6,0(0,R4)     POINT TO TEXT                             00371900
         LH    R3,4(0,R4)     TEXT LENGTH                               00372000
         LTR   R3,R3          IGNORE IF ZERO                            00372100
         BZ    SORTPAR3                                                 00372200
         BCTR  R3,0                                                     00372300
         EX    R3,SORTCOMP    FIELD NAME MATCH                          00372400
         BE    SORTPAR4       YES                                       00372500
         B     SORTPAR2       NO, TRY NEXT                              00372600
         SPACE 1                                                        00372700
SORTPAR3 ICM   R4,7,9(R4)     NEXT ITEM                                 00372800
         BNZ   SORTPAR1       CONTINUE IF MORE                          00372900
         B     SORTPAR5                                                 00373000
         SPACE 1                                                        00373100
SORTPAR4 MVC   0(4,R5),0(R1)  SET UP SORT FIELD                         00373200
         ICM   R4,7,9(R4)     ASCENDING/DESCENDING INDICATOR            00373300
         BZ    PARMERR        ERROR IF MISSING                          00373400
         L     R6,0(0,R4)     INDICATOR ADDR                            00373500
         CLC   4(2,R4),=F'0'  ERROR IF MISSING                          00373600
         BE    PARMERR                                                  00373700
         MVC   0(1,R5),0(R6)  A/D INDICATOR                             00373800
         LA    R5,4(0,R5)                                               00373900
         CLI   0(R6),C'A'     ASCENDING SORT                            00374000
         BE    SORTPAR3       YES, OK                                   00374100
         CLI   0(R6),C'D'     DESCENDING SORT                           00374200
         BNE   PARMERR        NO, ERROR                                 00374300
         B     SORTPAR3       CHECK IF ANY MORE                         00374400
         SPACE 1                                                        00374500
SORTCOMP CLC   4(0,R1),0(R6)                                            00374600
         SPACE 1                                                        00374700
SORTPAR5 LA    R3,SORTKTAB-12 SORT HEADER INDEX TABLE                   00374800
SORTK1   LA    R3,12(0,R3)    NEXT ENTRY                                00374900
         CLC   0(4,R3),=F'0'  END OF TABLE                              00375000
         BE    SORTK3         YES                                       00375100
         CLC   SORTTAB+1(1),1(R3)  ENTRY MATCH                          00375200
         BNE   SORTK1         NO, CHECK NEXT                            00375300
         SR    R4,R4                                                    00375400
         LH    R5,2(0,R3)     LOAD TABLE LENGTH                         00375500
         D     R4,=F'12'      TABLE ENTRIES                             00375600
         LA    R5,1(0,R5)                                               00375700
         LA    R6,VTCSORTH                                              00375800
         L     R4,4(0,R3)     LOAD TABLE BEGIN ADDR                     00375900
         CLI   SORTTAB,C'D'   DESCENDING SORT                           00376000
         BE    SORTK2         YES                                       00376100
         L     R4,8(0,R3)     LOAD TABLE END ADDR                       00376200
SORTK2   MVC   0(12,R6),0(R4)                                           00376300
         LA    R4,12(0,R4)                                              00376400
         LA    R6,12(0,R6)                                              00376500
         CLI   SORTTAB,C'D'   DESCENDING SORT                           00376600
         BE    *+8            YES                                       00376700
         S     R4,=F'24'                                                00376800
         BCT   R5,SORTK2                                                00376900
         B     SORTK4                                                   00377000
SORTK3   MVC   VTCSORTH(12),=3F'0'                                      00377100
SORTK4   MVC   0(12,R6),=3F'0'                                          00377200
*                                                                       00377300
*        CHECK THROUGH THE UCB'S TO SELECT THE VOLUMES TO PROCESS       00377400
*                                                                       00377500
*                                                                       00377600
**  FIND A VOLUME SERIAL NUMBER                                         00377700
*                                                                       00377800
         LA    R3,VOLS        POINT TO THE PDL                          00377900
LOOP1    L     R5,0(R3)       GET THE ADDRESS OF THE TEXT               00378000
         LH    R4,4(R3)       ALSO GET ITS LENGTH                       00378100
         LTR   R4,R4          FOR EXECUTES, GET THE LENGTH              00378200
         BZ    PHASE2         NO MORE VOLUMES, CONTINUE TO NEXT PHASE   00378300
         BCTR  R4,0           MAKE IT READY FOR THE EX INSTR            00378400
         MVC   VOLSER,BLANKS   INITIALIZE FIELD                         00378500
         EX    R4,MOVVOL                                                00378600
*                                                                       00378700
**  VOLUME FOUND - VERIFY AND CHECK FOR GLOBAL OR SPECIAL REQUESTS      00378800
*                                                                       00378900
         CH    R4,H5          IS THE ENTIRE NAME THERE?                 00379000
         BE    VOLSET         YES, IT'S A SPECIFIC VOLUME               00379100
         MVI   FLAG,X'01'     IT'S A GENERIC REQUEST                    00379200
         CH    R4,H2          CHECK FOR THE ALL KEYWORD, FIRST LENGTH   00379300
         BNE   VOLSET         NOT A GLOBAL REQUEST                      00379400
         CLC   0(3,R5),CHARALV  IS THIS THE KEYWORD 'ALLV'?             00379500
         BE    VOLSETV        NO, NOT A GLOBAL REQUEST                  00379600
         CLC   0(3,R5),CHARALL  IS THIS THE KEYWORD 'ALL'?              00379700
         BNE   VOLSET         NO, NOT A GLOBAL REQUEST                  00379800
         MVI   FLAG,X'02'   GLOBAL REQUEST                              00379900
         B     VOLSET                                                   00380000
*                                                                       00380100
**  FIND THE A(UCB)                                                     00380200
*                                                                       00380300
VOLSETV  MVI   FLAG,X'82'   GLOBAL REQUEST FOR VIRTUAL                  00380400
         B     VOLSET                                                   00380500
VOLSET   XC    LASTADR,LASTADR CLEAR THE UCB COMPARE ADDRESS            00380600
         L     R5,16   A(CVT)                                           00380700
         SR    R6,R6                                                    00380800
         L     R5,40(R5)   A(UCB ADDRESSES)                             00380900
NEXTUCB  ICM   R6,3,0(R5)   A(A UCB)                                    00381000
         LTR   R6,R6   CHECK FOR VALID ENTRIES                          00381100
         BZ    INCR1   UCB HOLE                                         00381200
         C     R6,FMIN1    CHECK FOR END                                00381300
         BE    NOTMNT   END OF UCB LIST - VOLUME NOT FOUND              00381400
         CLI   18(R6),X'20'   MUST BE DIRECT ACCESS                     00381500
         BNE   INCR1                                                    00381600
         C     R6,LASTADR   UCB ADDRESSES MUST INCREASE                 00381700
         BNH   INCR1        OTHERWISE THEY REPEAT.                      00381800
         ST    R6,LASTADR   NEW ADDRESS                                 00381900
         TM    FLAG,X'02'   CHECK FOR GLOBAL                            00382000
         BO    FNDGBL   IT IS                                           00382100
         TM    FLAG,X'01'   CHECK FOR SPECIAL REQUESTS                  00382200
         BO    SPECUCB   IT IS                                          00382300
         CLC   VOLSER,28(R6)   COMPARE FULL VOLSER                      00382400
         BE    FNDUCB   FOUND IT                                        00382500
         B     INCR1                                                    00382600
SPECUCB  EX    R4,CLCVOL   COMPARE FIRST X CHARACTERS ONLY              00382700
         BE    CHKRDY                                                   00382800
INCR1    LA    R5,2(R5)                                                 00382900
         B     NEXTUCB   TRY NEXT UCB                                   00383000
*                                                                       00383100
*        VARIOUS ERRORS, LET THE PERSON KNOW                            00383200
*                                                                       00383300
NOTMNT   TM    FLAG,X'04'     WAS A VOLUME  FOUND?                      00383400
         BO    NEXTVOL        YES, LOOK FOR THE NEXT SPEC               00383500
         MVC   MSGTEXT2,MSGNOTMT  NO, GET THE ERROR MESSAGE             00383600
SETVOL   MVC   MSGTEXT2+5(6),VOLSER ADD THE VOLUME SERIAL NUMBER        00383700
         VTOCMSG MSGTEXT2     AND ISSUE THE MESSAGE                     00383800
         B     NEXTVOL       GO GET THE NEXT VOLUME FROM PARSE          00383900
PENDING  MVC   MSGTEXT2,MSGPEND   SET UP THE MESSAGE                    00384000
*                                                                       00384100
*        SEE IF THIS IS A GENERIC OR GLOBAL REQUEST                     00384200
*                                                                       00384300
         TM    FLAG,X'03'    WAS IT ALL OR A PARTIAL VOLUME SERIAL?     00384400
         BNZ   INCR1         IN EITHER CASE, SKIP THE MESSAGE           00384500
*                            THEN FIND MORE VOLUMES                     00384600
*                                                                       00384700
*        OUTPUT THE OFFLINE PENDING MESSAGE                             00384800
         B     SETVOL         THEN ADD THE VOLUME                       00384900
OFFLINE  MVC   MSGTEXT2,MSGOFFLN SET UP THE MESSAGE                     00385000
         B     SETVOL         THEN ADD THE VOLUME                       00385100
*                                                                       00385200
**  FOR GLOBAL REQUESTS JUST LIST ONLINE PACKS                          00385300
*                                                                       00385400
FNDGBL   TM    3(R6),X'80'   ONLINE BIT                                 00385500
         BZ    INCR1   NOPE                                             00385600
*                                                                       00385700
**  FOR GLOBAL AND SPECIAL REQUESTS, CHECK FOR DEVICE READY             00385800
*                                                                       00385900
CHKRDY   TM    6(R6),X'40'   TEST READY BIT                             00386000
         BO    INCR1   NOT READY                                        00386100
         TM    FLAG,X'80'   GLOBAL REQUEST FOR VIRTUAL                  00386200
         BO    CHKVIRT                                                  00386300
         TM    FLAG,X'02'   GLOBAL REQUEST                              00386400
         BZ    FNDUCB                                                   00386500
         TM    17(R6),X'08'  VIRTUAL UCB                                00386600
         BO    INCR1   YES                                              00386700
         B     FNDUCB                                                   00386800
CHKVIRT  TM    17(R6),X'08'  VIRTUAL UCB                                00386900
         BZ    INCR1   NO                                               00387000
*                                                                       00387100
**  MOVE UCB INFORMATION TO OUTPUT LINE                                 00387200
*                                                                       00387300
FNDUCB   MVC   VOLID,28(R6)   MOVE VOLID                                00387400
         MVC   ADDR,13(R6)   MOVE UNIT ADDRESS                          00387500
         OI    FLAG,X'04'      NOTE THE VOLUME AS FOUND                 00387600
*                                                                       00387700
**  IF OFFLINE, DO NOT PROCESS                                          00387800
*                                                                       00387900
         TM    3(R6),X'40'   PENDING BIT - SHOULD BE OFF                00388000
         BO    PENDING                                                  00388100
         TM    3(R6),X'80'   ONLINE BIT - SHOULD BE ON                  00388200
         BZ    OFFLINE                                                  00388300
*                                                                       00388400
*        NOW GET DSCB'S FROM THE VOLUME                                 00388500
*                                                                       00388600
*                                                                       00388700
*        SET UP THE PARM LIST FOR VTOCEXCP                              00388800
*                                                                       00388900
         VTOCEXCP OPEN        OPEN THE VTOC                             00389000
         LTR   R15,R15        DID IT OPEN OK?                           00389100
         BNE   RETURN         NO, JUST EXIT                             00389200
READDSCB CLI   TABFULL,0     CHECK FOR FULL TABLES                      00389300
         BNE   ENDVTOC       IF FULL, TRY END OF VTOC TO CLEAR          00389400
         VTOCEXCP READ        GET A DSCB                                00389500
         CH    R15,H4         CHECK THE RETURN CODE                     00389600
         BE    ENDVTOC        END OF VTOC                               00389700
         BH    RETURN         BAD ERROR, VTOCEXCP GAVE THE MESSAGE      00389800
*                                                                       00389900
*        CHECK THE DATA SET QUALIFICATIONS, LIMIT, AND, OR              00390000
*                                                                       00390100
         VTCALL CHEK          CALL THE CHECK ROUTINE                    00390200
         LTR   R15,R15        DOES THIS DATA SET GET PASSED ON?         00390300
         BNZ   READDSCB       NO, GET ANOTHER                           00390400
*                             YES, CONTINUE PROCESSING                  00390500
*                                                                       00390600
*        FORMAT THE DSCB INFORMATION                                    00390700
*                                                                       00390800
         TM    VTCFMTCK,VTCFMTCD WAS FORMAT CALLED BY CHECK?            00390900
         BO    CALLEXIT       YES, DON'T CALL IT AGAIN                  00391000
         VTCALL FORM          CALL THE FORMATTING ROUTINE               00391100
         LTR   R15,R15        DID IT FUNCTION?                          00391200
         BNZ   READDSCB       NO, GET ANOTHER DSCB                      00391300
*                                                                       00391400
*        CALL THE EXIT ROUTINE IF ONE WAS SPECIFIED                     00391500
*                                                                       00391600
CALLEXIT VTCALL EXIT,TEST     CALL THE EXIT ROUTINE                     00391700
         LTR   R15,R15        SHOULD THE DATA SET BE PASSED ON?         00391800
         BNZ   READDSCB       NO, GET ANOTHER DSCB                      00391900
*                                                                       00392000
*        SORT THE ENTRIES INTO THE NEW LIST                             00392100
*                                                                       00392200
         VTCALL SORT          CALL THE SORT ROUTINE                     00392300
         B     READDSCB       GET ANOTHER DSCB                          00392400
*                                                                       00392500
*        END OF THE VOLUME, CHECK FOR MORE                              00392600
*                                                                       00392700
ENDVTOC  VTOCEXCP CLOSE FIRST CLOSE THE VTOC                            00392800
*                                                                       00392900
ENDVOL   TM    FLAG,X'03'         IS THIS A GENERIC VOLUME SEARCH       00393000
         BNZ   INCR1              YES, SEARCH FOR MORE                  00393100
NEXTVOL  ICM   R3,B'0111',25(R3)  GET THE NEXT VOLUME FROM THE PDL      00393200
         BP    LOOP1              THERE IS ANOTHER, GET IT              00393300
*                                                                       00393400
*        PRINT THE SELECTED ITEMS FOR THE SELECTED DATA SETS            00393500
*                                                                       00393600
PHASE2   DS    0H                                                       00393700
         VTCALL PRNT          CALL THE PRINT ROUTINE                    00393800
         B     EXIT0                                                    00393900
         EJECT                                                          00394000
*                                                                       00394100
*        PROCESSING IS COMPLETE, EXEUNT                                 00394200
*                                                                       00394300
PARMERR  LA    R15,16                                                   00394400
         B     RETURN                                                   00394500
EXIT0    SR    R15,R15                                                  00394600
         SPACE 3                                                        00394700
RETURN   LTR   R2,R15         NORMAL EXIT?                              00394800
         BZ    RETURN1        YES, LEAVE EVERY THING ALONE              00394900
         SPACE 2                                                        00395000
         LA    R1,PARMLIST    AREA FOR STACK PARM LIST                  00395100
         USING IOPL,R1        AN ERROR WAS FOUND, FLUSH THE STACK       00395200
         SPACE                                                          00395300
         MVC   IOPLUPT,ADDRUPT                                          00395400
         MVC   IOPLECT,ADDRECT                                          00395500
         LA    R0,ATTNECB                                               00395600
         MVI   ATTNECB,0                                                00395700
         ST    R0,IOPLECB                                               00395800
         SPACE 2                                                        00395900
         STACK PARM=PARMLIST+16,DELETE=ALL,MF=(E,(1))                   00396000
         SPACE 3                                                        00396100
         TCLEARQ INPUT        CLEAR INPUT BUFFERS                       00396200
         SPACE 3                                                        00396300
RETURN1  DS    0H                                                       00396400
         BAL   R14,FREEPDL    FREE THE PARSE STROAGE                    00396500
         MVI   VTCEPRNT,15    TELL PRINT TO CLEAN UP HIS ACT            00396600
*                                CLOSE DATA SETS AND FREE MAIN STORAGE  00396700
         VTCALL PRNT          CALL THE PRINT ROUTINE                    00396800
         SPACE                                                          00396900
         LR    R15,R2          GET THE RETURN CODE AGAIN                00397000
         LEAVE EQ                                                       00397100
WORKREG  EQU   13                                                       00397200
*                                                                       00397300
*        PARSE INITIALIZATION                                           00397400
*                                                                       00397500
         SPACE 3                                                        00397600
PARSINIT DS    0H                                                       00397700
         ST    R2,CPPLADDR    AND THE CPPL ADDRESS                      00397800
         USING CPPL,R2        BASE FOR COMMAND PARM LIST                00397900
         MVC   ADDRUPT,CPPLUPT ADDR OF USER PROFILE TABLE               00398000
         MVC   ADDRPSCB,CPPLPSCB                                        00398100
         MVC   ADDRECT,CPPLECT ADDR OF ENVIROMENT TABLE                 00398200
         MVC   ADDRCBUF,CPPLCBUF                                        00398300
         DROP  R2                                                       00398400
         SPACE 3                                                        00398500
*                                                                       00398600
*        PUT THE WORK AREA ADDRESSES INTO THE PARM LISTS                00398700
*                                                                       00398800
         WORKADDR MSG,VTCWMSG     WORK AREA FOR VTOCMSG                 00398900
         WORKADDR EXCP,VTCWEXCP   WORK AREA FOR VTOCEXCP                00399000
         WORKADDR CHEK,VTCWCHEK   WORK AREA FOR VTOCCHEK                00399100
         WORKADDR FORM,VTCWFORM   WORK AREA FOR VTOCFORM                00399200
         WORKADDR EXIT,VTCWEXIT   WORK AREA FOR VTOCEXIT                00399300
         WORKADDR SORT,VTCWSORT   WORK AREA FOR VTOCSORT                00399400
         WORKADDR PRNT,VTCWPRNT   WORK AREA FOR VTOCPRNT                00399500
         SPACE 3                                                        00399600
*        SET UP THE ADDRESSES FOR CALLING                               00399700
*                                                                       00399800
         MVC   VADMSG(RTNADLEN),RTNADDRS  MOVE IN THE ADDRESSES         00399900
*                                                                       00400000
*                                                                       00400100
*                                                                       00400200
*        BUILD PARSE PARAMETER LIST AND INVOKE                          00400300
*        IKJPARS TO ANALYZE COMMAND OPERANDS                            00400400
*                                                                       00400500
         SPACE 3                                                        00400600
GOPARSE  DS    0H                                                       00400700
         ST    R14,R14PARSE   SAVE THE RETURN ADDRESS                   00400800
         LA    R1,PARSELST    AREA FOR PARSE PARAMETERS                 00400900
         USING PPL,R1         BASE FOR PARSE PARAMETER LIST             00401000
         SPACE 2                                                        00401100
         MVC   PPLUPT,ADDRUPT PASS UPT ADDRESS                          00401200
         MVC   PPLECT,ADDRECT AND ECT ADDRESS                           00401300
         MVC   PPLCBUF,ADDRCBUF AND COMMAND BUFFER ADDR                 00401400
         SPACE                                                          00401500
         ST    WORKREG,PPLUWA ALSO WORK AREA ADDR FOR VALIDITY EXITS    00401600
         SPACE                                                          00401700
         LA    R0,ATTNECB     ECB FOR ATTN INTERRUPTS                   00401800
         MVI   ATTNECB,0      CLEAR ECB                                 00401900
         ST    R0,PPLECB      PASSE TO PARSE                            00402000
         SPACE                                                          00402100
         LA    R0,ADDRANSR    PASS ADDR OF WORD WHERE PARSE             00402200
         ST    R0,PPLANS      RETURNS PDL ADDRESS                       00402300
         SPACE                                                          00402400
         MVC   PPLPCL,ADDRPCL STORE PCL ADDRESS                         00402500
         SPACE 3                                                        00402600
         CALLTSSR EP=IKJPARS  INVOKE PARSE                              00402700
         DROP  R1                                                       00402800
         SPACE 2                                                        00402900
         LA    R14,MAXPARSE   RETURN CODE LIMIT                         00403000
         SPACE                                                          00403100
         CR    R15,R14        VERIFY RETURN CODE WITHIN LIMITS          00403200
         BH    PARSEBAD       NO, ERROR                                 00403300
         SPACE                                                          00403400
         B     *+4(R15)       PROCESS RETURN CODE                       00403500
         SPACE                                                          00403600
PARSERET B     PARSEOK         0- SUCESSFUL                             00403700
         B     PARSEERR        4- PARSE UNABLE TO PROMPT                00403800
         B     PARSEERR        8- USER ENTERED ATTENTION                00403900
         B     PARSEBAD       12- INVALID PARAMETERS                    00404000
         B     PARSEBAD       16- PARSE INTERNAL FAILURE                00404100
         B     PARSEERR       20 - VALIDITY CHECK ERROR                 00404200
MAXPARSE EQU   *-PARSERET                                               00404300
         SPACE 5                                                        00404400
PARSEBAD DS    0H                                                       00404500
         MVC   MSGTEXT2+4(L'MSGPARSE),MSGPARSE                          00404600
         LA    R1,MSGTEXT2+4+L'MSGPARSE                                 00404700
         SPACE                                                          00404800
         CVD   R15,DOUBLE                                               00404900
         OI    DOUBLE+7,X'0F'                                           00405000
         UNPK  0(2,R1),DOUBLE                                           00405100
         SPACE                                                          00405200
         LA    R0,MSGTEXT2-2                                            00405300
         SR    R1,R0                                                    00405400
         SLL   R1,16                                                    00405500
         ST    R1,MSGTEXT2                                              00405600
         SPACE 2                                                        00405700
         VTOCMSG MSGCMDER,MSGTEXT2    PUT OUT 'COMMAND ERROR' MSG       00405800
         SPACE 3                                                        00405900
PARSEERR LA    R15,12         ERROR CODE 12 - COMMAND FAILED            00406000
         B     PARSERTN       RETURN FROM PARSE                         00406100
         SPACE                                                          00406200
PARSEOK  SR    R15,R15        CLEAR THE RETURN CODE                     00406300
PARSERTN L     R14,R14PARSE   GET THE RETURN LOCATION                   00406400
         BR    R14            AND GET OUT OF HERE                       00406500
         SPACE                                                          00406600
         EJECT                                                          00406700
*                                                                       00406800
*        PARSE CLEANUP ROUTINE                                          00406900
*                                                                       00407000
         SPACE 3                                                        00407100
FREEPDL  DS    0H                                                       00407200
         SPACE                                                          00407300
         ST    R14,R14SAVE                                              00407400
         SPACE                                                          00407500
         IKJRLSA ADDRANSR     RELEASE THE STORAGE                       00407600
         SPACE 2                                                        00407700
         XC    ADDRANSR,ADDRANSR                                        00407800
         SPACE                                                          00407900
         L     R14,R14SAVE                                              00408000
         BR    R14                                                      00408100
         EJECT                                                          00408200
*                                                                       00408300
*                                                                       00408400
*        CONSTANTS                                                      00408500
*                                                                       00408600
*                                                                       00408700
         LTORG                                                          00408800
RTNADDRS DC    V(VTOCMSG)                                               00408900
         DC    A(0)           DUMMY ENTRY FOR THE EXIT ROUTINE          00409000
         DC    V(VTOCEXCP)                                              00409100
         DC    V(VTOCCHEK)                                              00409200
         DC    V(VTOCFORM)                                              00409300
         DC    V(VTOCPRNT)                                              00409400
         DC    V(VTOCSORT)                                              00409500
RTNADLEN EQU   *-RTNADDRS                                               00409600
ADDRPCL  DC    A(PCLMAIN)     ADDR OF MAIN PARSE CONTROL LIST           00409700
FMIN1    DC    X'0000FFFF'    END OF UCB LIST                           00409800
BLANKS   DC    CL8' '         BALNKS                                    00409900
H2       DC    H'2'                                                     00410000
H4       DC    H'4'                                                     00410100
H5       DC    H'5'                                                     00410200
*                                                                       00410300
*                                                                       00410400
*                                                                       00410500
*                                                                       00410600
*                                                                       00410700
CHARALL  DC    CL3'ALL'                                                 00410800
CHARALV  DC    CL3'ALV'                                                 00410900
MOVVOL   MVC   VOLSER(0),0(R5)                                          00411000
CLCVOL   CLC   VOLSER(0),28(R6)                                         00411100
         EJECT                                                          00411200
SORTTABX DC    AL2(VTFDSN-VTFMT),AL2(43),CL8'DSNAME'                    00411300
         DC    AL2(VTFVOLUM-VTFMT),AL2(5),CL8'VOLUME'                   00411400
         DC    AL2(VTFALLOC-VTFMT),AL2(3),CL8'ALLOC'                    00411500
         DC    AL2(VTFUSED-VTFMT),AL2(3),CL8'USED'                      00411600
         DC    AL2(VTFUNUSD-VTFMT),AL2(3),CL8'UNUSED'                   00411700
         DC    AL2(VTFPCT-VTFMT),AL2(1),CL8'PCT'                        00411800
         DC    AL2(VTFNOEPV-VTFMT),AL2(0),CL8'EX'                       00411900
         DC    AL2(VTFDSORG-VTFMT),AL2(2),CL8'DSO'                      00412000
         DC    AL2(VTFRECFM-VTFMT),AL2(4),CL8'RFM'                      00412100
         DC    AL2(VTFLRECL-VTFMT),AL2(1),CL8'LRECL'                    00412200
         DC    AL2(VTFBLKSZ-VTFMT),AL2(1),CL8'BLKSZ'                    00412300
         DC    AL2(VTFCREDT-VTFMT),AL2(2),CL8'CDATE'                    00412400
         DC    AL2(VTFEXPDT-VTFMT),AL2(2),CL8'EXPDT'                    00412500
         DC    AL2(VTFLSTAC-VTFMT),AL2(2),CL8'REFDT'                    00412600
         DC    F'0'                                                     00412700
         EJECT                                                          00412800
*                                                                       00412900
*        PROGRAM MESSAGES                                               00413000
*                                                                       00413100
         SPACE 2                                                        00413200
         PRINT NOGEN                                                    00413300
         SPACE                                                          00413400
MSGPARSE MSG   ' PARSE ERROR CODE '                                     00413500
MSGCMDER MSG   ' COMMAND SYSTEM ERROR'                                  00413600
MSGNOTMT MSG   ' VVVVVV VOLUME IS NOT MOUNTED'                          00413700
MSGOFFLN MSG   ' VVVVVV VOLUME IS OFFLINE'                              00413800
MSGPEND  MSG   ' VVVVVV VOLUME IS PENDING OFFLINE'                      00413900
*                                                                       00414000
*                                                                       00414100
         EJECT                                                          00414200
         DS    0F                                                       00414300
SORTKTAB DC    AL2(VTFDSN-VTFMT),AL2(DSNSORTE-DSNSORT)                  00414400
         DC    A(DSNSORT),A(DSNSORTE)                                   00414500
         DC    AL2(VTFVOLUM-VTFMT),AL2(VOLSORTE-VOLSORT)                00414600
         DC    A(VOLSORT),A(VOLSORTE)                                   00414700
         DC    AL2(VTFUSED-VTFMT),AL2(USESORTE-USESORT)                 00414800
         DC    A(USESORT),A(USESORTE)                                   00414900
         DC    AL2(VTFALLOC-VTFMT),AL2(ALCSORTE-ALCSORT)                00415000
         DC    A(ALCSORT),A(ALCSORTE)                                   00415100
         DC    AL2(VTFUNUSD-VTFMT),AL2(UNUSORTE-UNUSORT)                00415200
         DC    A(UNUSORT),A(UNUSORTE)                                   00415300
         DC    AL2(VTFPCT-VTFMT),AL2(PCTSORTE-PCTSORT)                  00415400
         DC    A(PCTSORT),A(PCTSORTE)                                   00415500
         DC    AL2(VTFNOEPV-VTFMT),AL2(EXTSORTE-EXTSORT)                00415600
         DC    A(EXTSORT),A(EXTSORTE)                                   00415700
         DC    AL2(VTFDSORG-VTFMT),AL2(DSOSORTE-DSOSORT)                00415800
         DC    A(DSOSORT),A(DSOSORTE)                                   00415900
         DC    AL2(VTFRECFM-VTFMT),AL2(RFMSORTE-RFMSORT)                00416000
         DC    A(RFMSORT),A(RFMSORTE)                                   00416100
         DC    AL2(VTFLRECL-VTFMT),AL2(LRCSORTE-LRCSORT)                00416200
         DC    A(LRCSORT),A(LRCSORTE)                                   00416300
         DC    AL2(VTFBLKSZ-VTFMT),AL2(BLKSORTE-BLKSORT)                00416400
         DC    A(BLKSORT),A(BLKSORTE)                                   00416500
         DC    AL2(VTFCREDT-VTFMT),AL2(CDTSORTE-CDTSORT)                00416600
         DC    A(CDTSORT),A(CDTSORTE)                                   00416700
         DC    AL2(VTFLSTAC-VTFMT),AL2(RDTSORTE-RDTSORT)                00416800
         DC    A(RDTSORT),A(RDTSORTE)                                   00416900
         DC    AL2(VTFEXPDT-VTFMT),AL2(EDTSORTE-EDTSORT)                00417000
         DC    A(EDTSORT),A(EDTSORTE)                                   00417100
         DC    2F'0'                                                    00417200
         SPACE 3                                                        00417300
DSNSORT  DC    A(0),AL2(0),CL6'Z'                                       00417400
         DC    A(0),AL2(1),CL6'TV'                                      00417500
         DC    A(0),AL2(1),CL6'TM'                                      00417600
         DC    A(0),AL2(2),CL6'T.Z'                                     00417700
         DC    A(0),AL2(2),CL6'T.Y'                                     00417800
         DC    A(0),AL2(2),CL6'T.X'                                     00417900
         DC    A(0),AL2(2),CL6'T.W'                                     00418000
         DC    A(0),AL2(2),CL6'T.V'                                     00418100
         DC    A(0),AL2(2),CL6'T.U'                                     00418200
         DC    A(0),AL2(2),CL6'T.T'                                     00418300
         DC    A(0),AL2(2),CL6'T.S'                                     00418400
         DC    A(0),AL2(2),CL6'T.R'                                     00418500
         DC    A(0),AL2(2),CL6'T.Q'                                     00418600
         DC    A(0),AL2(2),CL6'T.P'                                     00418700
         DC    A(0),AL2(2),CL6'T.O'                                     00418800
         DC    A(0),AL2(2),CL6'T.N'                                     00418900
         DC    A(0),AL2(2),CL6'T.M'                                     00419000
         DC    A(0),AL2(2),CL6'T.L'                                     00419100
         DC    A(0),AL2(2),CL6'T.K'                                     00419200
         DC    A(0),AL2(2),CL6'T.J'                                     00419300
         DC    A(0),AL2(2),CL6'T.I'                                     00419400
         DC    A(0),AL2(2),CL6'T.H'                                     00419500
         DC    A(0),AL2(2),CL6'T.G'                                     00419600
         DC    A(0),AL2(2),CL6'T.F'                                     00419700
         DC    A(0),AL2(2),CL6'T.E'                                     00419800
         DC    A(0),AL2(2),CL6'T.D'                                     00419900
         DC    A(0),AL2(2),CL6'T.C'                                     00420000
         DC    A(0),AL2(2),CL6'T.B'                                     00420100
         DC    A(0),AL2(2),CL6'T.A'                                     00420200
         DC    A(0),AL2(1),CL6'SY'                                      00420300
         DC    A(0),AL2(1),CL6'SV'                                      00420400
         DC    A(0),AL2(1),CL6'PV'                                      00420500
         DC    A(0),AL2(2),CL6'P.Z'                                     00420600
         DC    A(0),AL2(2),CL6'P.Y'                                     00420700
         DC    A(0),AL2(2),CL6'P.X'                                     00420800
         DC    A(0),AL2(2),CL6'P.W'                                     00420900
         DC    A(0),AL2(2),CL6'P.V'                                     00421000
         DC    A(0),AL2(2),CL6'P.U'                                     00421100
         DC    A(0),AL2(2),CL6'P.T'                                     00421200
         DC    A(0),AL2(2),CL6'P.S'                                     00421300
         DC    A(0),AL2(2),CL6'P.R'                                     00421400
         DC    A(0),AL2(2),CL6'P.Q'                                     00421500
         DC    A(0),AL2(2),CL6'P.P'                                     00421600
         DC    A(0),AL2(2),CL6'P.O'                                     00421700
         DC    A(0),AL2(2),CL6'P.N'                                     00421800
         DC    A(0),AL2(2),CL6'P.M'                                     00421900
         DC    A(0),AL2(2),CL6'P.L'                                     00422000
         DC    A(0),AL2(2),CL6'P.K'                                     00422100
         DC    A(0),AL2(2),CL6'P.J'                                     00422200
         DC    A(0),AL2(2),CL6'P.I'                                     00422300
         DC    A(0),AL2(2),CL6'P.H'                                     00422400
         DC    A(0),AL2(2),CL6'P.G'                                     00422500
         DC    A(0),AL2(2),CL6'P.F'                                     00422600
         DC    A(0),AL2(2),CL6'P.E'                                     00422700
         DC    A(0),AL2(2),CL6'P.D'                                     00422800
         DC    A(0),AL2(2),CL6'P.C'                                     00422900
         DC    A(0),AL2(2),CL6'P.B'                                     00423000
         DC    A(0),AL2(2),CL6'P.A'                                     00423100
         DC    A(0),AL2(0),CL6'N'                                       00423200
DSNSORTE DC    A(0),AL2(0),CL6' '                                       00423300
         SPACE 3                                                        00423400
VOLSORT  DC    A(0),AL2(4),CL6'33509'                                   00423500
         DC    A(0),AL2(4),CL6'33508'                                   00423600
         DC    A(0),AL2(4),CL6'33507'                                   00423700
         DC    A(0),AL2(4),CL6'33506'                                   00423800
         DC    A(0),AL2(4),CL6'33505'                                   00423900
         DC    A(0),AL2(4),CL6'33504'                                   00424000
         DC    A(0),AL2(4),CL6'33503'                                   00424100
         DC    A(0),AL2(4),CL6'33502'                                   00424200
         DC    A(0),AL2(4),CL6'33501'                                   00424300
         DC    A(0),AL2(4),CL6'33500'                                   00424400
         DC    A(0),AL2(4),CL6'33309'                                   00424500
         DC    A(0),AL2(4),CL6'33308'                                   00424600
         DC    A(0),AL2(4),CL6'33307'                                   00424700
         DC    A(0),AL2(4),CL6'33306'                                   00424800
         DC    A(0),AL2(4),CL6'33305'                                   00424900
         DC    A(0),AL2(4),CL6'33304'                                   00425000
         DC    A(0),AL2(4),CL6'33303'                                   00425100
         DC    A(0),AL2(4),CL6'33302'                                   00425200
         DC    A(0),AL2(4),CL6'33301'                                   00425300
         DC    A(0),AL2(4),CL6'33300'                                   00425400
         DC    A(0),AL2(0),CL6'T'                                       00425500
         DC    A(0),AL2(0),CL6'R'                                       00425600
         DC    A(0),AL2(0),CL6'P'                                       00425700
         DC    A(0),AL2(0),CL6'M'                                       00425800
         DC    A(0),AL2(0),CL6'I'                                       00425900
         DC    A(0),AL2(0),CL6'H'                                       00426000
VOLSORTE DC    A(0),AL2(0),CL6' '                                       00426100
         SPACE 3                                                        00426200
USESORT  DS    0F                                                       00426300
UNUSORT  DS    0F                                                       00426400
ALCSORT  DC    A(0),AL2(3),XL4'0000F000',XL2'00'                        00426500
         DC    A(0),AL2(3),XL4'0000C000',XL2'00'                        00426600
         DC    A(0),AL2(3),XL4'0000A000',XL2'00'                        00426700
         DC    A(0),AL2(3),XL4'00008000',XL2'00'                        00426800
         DC    A(0),AL2(3),XL4'00006000',XL2'00'                        00426900
         DC    A(0),AL2(3),XL4'00005000',XL2'00'                        00427000
         DC    A(0),AL2(3),XL4'00004000',XL2'00'                        00427100
         DC    A(0),AL2(3),XL4'00003000',XL2'00'                        00427200
         DC    A(0),AL2(3),XL4'00002000',XL2'00'                        00427300
         DC    A(0),AL2(3),XL4'00001000',XL2'00'                        00427400
         DC    A(0),AL2(3),XL4'00000C00',XL2'00'                        00427500
         DC    A(0),AL2(3),XL4'00000800',XL2'00'                        00427600
         DC    A(0),AL2(3),XL4'00000400',XL2'00'                        00427700
         DC    A(0),AL2(3),XL4'00000300',XL2'00'                        00427800
         DC    A(0),AL2(3),XL4'00000200',XL2'00'                        00427900
         DC    A(0),AL2(3),XL4'00000100',XL2'00'                        00428000
         DC    A(0),AL2(3),XL4'000000C0',XL2'00'                        00428100
         DC    A(0),AL2(3),XL4'00000080',XL2'00'                        00428200
         DC    A(0),AL2(3),XL4'00000040',XL2'00'                        00428300
         DC    A(0),AL2(3),XL4'00000010',XL2'00'                        00428400
USESORTE DS    0F                                                       00428500
UNUSORTE DS    0F                                                       00428600
ALCSORTE DC    A(0),AL2(3),XL6'00'                                      00428700
         SPACE 3                                                        00428800
PCTSORT  DC    A(0),AL2(1),XL2'0064',XL4'00'                            00428900
         DC    A(0),AL2(1),XL2'005A',XL4'00'                            00429000
         DC    A(0),AL2(1),XL2'0050',XL4'00'                            00429100
         DC    A(0),AL2(1),XL2'0046',XL4'00'                            00429200
         DC    A(0),AL2(1),XL2'003C',XL4'00'                            00429300
         DC    A(0),AL2(1),XL2'0032',XL4'00'                            00429400
         DC    A(0),AL2(1),XL2'0028',XL4'00'                            00429500
         DC    A(0),AL2(1),XL2'001E',XL4'00'                            00429600
         DC    A(0),AL2(1),XL2'0014',XL4'00'                            00429700
         DC    A(0),AL2(1),XL2'000A',XL4'00'                            00429800
PCTSORTE DC    A(0),AL2(1),XL6'00'                                      00429900
         SPACE 3                                                        00430000
EXTSORT  DC    A(0),AL2(0),CL6'0'                                       00430100
EXTSORTE DC    A(0),AL2(0),CL6'0'                                       00430200
         SPACE 3                                                        00430300
DSOSORT  DC    A(0),AL2(1),CL6'VS'                                      00430400
         DC    A(0),AL2(1),CL6'PS'                                      00430500
         DC    A(0),AL2(1),CL6'PO'                                      00430600
         DC    A(0),AL2(1),CL6'DA'                                      00430700
DSOSORTE DC    A(0),AL2(1),CL6' '                                       00430800
         SPACE 3                                                        00430900
RFMSORT  DC    A(0),AL2(1),CL6'VS'                                      00431000
         DC    A(0),AL2(2),CL6'VBS'                                     00431100
         DC    A(0),AL2(1),CL6'VB'                                      00431200
         DC    A(0),AL2(0),CL6'V'                                       00431300
         DC    A(0),AL2(0),CL6'U'                                       00431400
         DC    A(0),AL2(1),CL6'FS'                                      00431500
         DC    A(0),AL2(2),CL6'FBS'                                     00431600
         DC    A(0),AL2(1),CL6'FB'                                      00431700
         DC    A(0),AL2(0),CL6'F'                                       00431800
RFMSORTE DC    A(0),AL2(0),CL6' '                                       00431900
         SPACE 3                                                        00432000
LRCSORT  DS    0F                                                       00432100
BLKSORT  DC    A(0),AL2(1),XL2'4650',XL4'00'                            00432200
         DC    A(0),AL2(1),XL2'3A98',XL4'00'                            00432300
         DC    A(0),AL2(1),XL2'2EE0',XL4'00'                            00432400
         DC    A(0),AL2(1),XL2'2328',XL4'00'                            00432500
         DC    A(0),AL2(1),XL2'1770',XL4'00'                            00432600
         DC    A(0),AL2(1),XL2'0BB8',XL4'00'                            00432700
         DC    A(0),AL2(1),XL2'07D0',XL4'00'                            00432800
         DC    A(0),AL2(1),XL2'0640',XL4'00'                            00432900
         DC    A(0),AL2(1),XL2'04B0',XL4'00'                            00433000
         DC    A(0),AL2(1),XL2'0320',XL4'00'                            00433100
         DC    A(0),AL2(1),XL2'0258',XL4'00'                            00433200
         DC    A(0),AL2(1),XL2'0190',XL4'00'                            00433300
         DC    A(0),AL2(1),XL2'00C8',XL4'00'                            00433400
         DC    A(0),AL2(1),XL2'00A0',XL4'00'                            00433500
         DC    A(0),AL2(1),XL2'0078',XL4'00'                            00433600
         DC    A(0),AL2(1),XL2'0050',XL4'00'                            00433700
         DC    A(0),AL2(1),XL2'0028',XL4'00'                            00433800
BLKSORTE DS    0F                                                       00433900
LRCSORTE DC    A(0),AL2(1),XL6'00'                                      00434000
         SPACE 3                                                        00434100
CDTSORT  DS    0F                                                       00434200
EDTSORT  DS    0F                                                       00434300
RDTSORT  DC    A(0),AL2(2),AL1(99),AL2(0),XL3'00'                       00434400
         DC    A(0),AL2(2),AL1(83),AL2(300),XL3'00'                     00434500
         DC    A(0),AL2(2),AL1(83),AL2(200),XL3'00'                     00434600
         DC    A(0),AL2(2),AL1(83),AL2(100),XL3'00'                     00434700
         DC    A(0),AL2(2),AL1(83),AL2(000),XL3'00'                     00434800
         DC    A(0),AL2(2),AL1(82),AL2(300),XL3'00'                     00434900
         DC    A(0),AL2(2),AL1(82),AL2(200),XL3'00'                     00435000
         DC    A(0),AL2(2),AL1(82),AL2(100),XL3'00'                     00435100
         DC    A(0),AL2(2),AL1(82),AL2(000),XL3'00'                     00435200
         DC    A(0),AL2(2),AL1(81),AL2(300),XL3'00'                     00435300
         DC    A(0),AL2(2),AL1(81),AL2(200),XL3'00'                     00435400
         DC    A(0),AL2(2),AL1(81),AL2(100),XL3'00'                     00435500
         DC    A(0),AL2(2),AL1(81),AL2(000),XL3'00'                     00435600
         DC    A(0),AL2(2),AL1(80),AL2(300),XL3'00'                     00435700
         DC    A(0),AL2(2),AL1(80),AL2(200),XL3'00'                     00435800
         DC    A(0),AL2(2),AL1(80),AL2(100),XL3'00'                     00435900
         DC    A(0),AL2(2),AL1(80),AL2(000),XL3'00'                     00436000
         DC    A(0),AL2(2),AL1(79),AL2(300),XL3'00'                     00436100
         DC    A(0),AL2(2),AL1(79),AL2(200),XL3'00'                     00436200
         DC    A(0),AL2(2),AL1(79),AL2(100),XL3'00'                     00436300
         DC    A(0),AL2(2),AL1(79),AL2(000),XL3'00'                     00436400
         DC    A(0),AL2(2),AL1(78),AL2(300),XL3'00'                     00436500
         DC    A(0),AL2(2),AL1(78),AL2(200),XL3'00'                     00436600
         DC    A(0),AL2(2),AL1(78),AL2(100),XL3'00'                     00436700
         DC    A(0),AL2(2),AL1(78),AL2(000),XL3'00'                     00436800
         DC    A(0),AL2(2),AL1(77),AL2(300),XL3'00'                     00436900
         DC    A(0),AL2(2),AL1(77),AL2(200),XL3'00'                     00437000
         DC    A(0),AL2(2),AL1(77),AL2(100),XL3'00'                     00437100
         DC    A(0),AL2(2),AL1(77),AL2(000),XL3'00'                     00437200
         DC    A(0),AL2(2),AL1(76),AL2(300),XL3'00'                     00437300
         DC    A(0),AL2(2),AL1(76),AL2(200),XL3'00'                     00437400
         DC    A(0),AL2(2),AL1(76),AL2(100),XL3'00'                     00437500
         DC    A(0),AL2(2),AL1(76),AL2(000),XL3'00'                     00437600
         DC    A(0),AL2(2),AL1(75),AL2(300),XL3'00'                     00437700
         DC    A(0),AL2(2),AL1(75),AL2(200),XL3'00'                     00437800
         DC    A(0),AL2(2),AL1(75),AL2(100),XL3'00'                     00437900
         DC    A(0),AL2(2),AL1(75),AL2(000),XL3'00'                     00438000
EDTSORTE DS    0F                                                       00438100
RDTSORTE DS    0F                                                       00438200
CDTSORTE DC    A(0),AL2(2),XL6'00'                                      00438300
         EJECT                                                          00438400
*                                                                       00438500
*                                                                       00438600
*        P A R S E   C O N T R O L   L I S T                            00438700
*                                                                       00438800
*                                                                       00438900
         SPACE 3                                                        00439000
         COPY  VTOCPARS                                                 00439100
         EJECT                                                          00439200
*                                                                       00439300
*        DYNAMIC WORK AREA                                              00439400
*                                                                       00439500
         SPACE 3                                                        00439600
WORKAREA DSECT                                                          00439700
MAINSAVE DS    18A                                                      00439800
         SPACE                                                          00439900
         VTOCEXCP EQ          DEFINE VTOCEXCP CODES                     00440000
         SPACE                                                          00440100
PARSELST DS    8A             AREA FOR PARSE PARAMETER LIST             00440200
         SPACE                                                          00440300
R14SAVE  DS    A                                                        00440400
R14PARSE DS    A                                                        00440500
*                                                                       00440600
*        VTOC COMMAND COMMON AREA                                       00440700
*                                                                       00440800
         PRINT GEN                                                      00440900
         VTOCOM  NODSECT                                                00441000
         PRINT NOGEN                                                    00441100
         SPACE 3                                                        00441200
*                                                                       00441300
*        WORK AREAS FOR SUBROUTINES                                     00441400
*                                                                       00441500
WORKMSG  DS    XL256                                                    00441600
WORKEXCP DS    4XL256                                                   00441700
WORKCHEK DS    XL256                                                    00441800
WORKFORM DS    2XL256                                                   00441900
WORKEXIT DS    8XL256                                                   00442000
WORKSORT DS    XL256                                                    00442100
WORKPRNT DS    10XL256                                                  00442200
         DS    0D                                                       00442300
LENWORK  EQU   *-WORKAREA                                               00442400
         SPACE 3                                                        00442500
         VTFMT                                                          00442600
         SPACE 3                                                        00442700
         PDEDSNAM                                                       00442800
         SPACE 3                                                        00442900
         IKJPPL                                                         00443000
         SPACE 3                                                        00443100
         IKJIOPL                                                        00443200
         SPACE 3                                                        00443300
         IKJPSCB                                                        00443400
         SPACE 3                                                        00443500
         IKJECT                                                         00443600
         SPACE 3                                                        00443700
         IKJCPPL                                                        00443800
         SPACE 3                                                        00443900
         IKJUPT                                                         00444000
         SPACE 3                                                        00444100
         PRINT NOGEN                                                    00444200
         CVT                                                            00444300
         END                                                            00444400
/*                                                                      00444500
//*------------------------------------------------------ ASM: VTOC     00444600
//*                                                                     00444700
//ASM2.SYSIN DD *                                                       00444800
         TITLE 'VTOC COMMAND CHECK  ROUTINE'                            00444900
*********************************************************************** 00445000
*                                                                     * 00445100
*                                                                     * 00445200
* TITLE -      VTOC COMMAND CHECK  ROUTINE                            * 00445300
*                                                                     * 00445400
* FUNCTION -   CHECK THE CONDITIONS SPECIFIED ON THE VTOC COMMAND.    * 00445500
*              SEE IF THE DATA SET PASSED SHOULD BE PROCESSED.        * 00445600
*              THE LIMIT, ENDING, CONTAINING, CCHH, LIMIT, AND,       * 00445700
*              AND OR KEYWORDS ARE PROCESSED BY THIS ROUTINE.         * 00445800
*                                                                     * 00445900
* OPERATION -  FIRST GET THE LENGTH OF THE DSNAME AND SAVE IT.        * 00446000
*              THEN GET THE FORMAT 3 DSCB, IF ONE EXISTS.  TRY        * 00446100
*              EACH KEYWORD TO SEE IF IT WILL EXCLUDE THE DATA        * 00446200
*              SET FROM FURTHER PROCESSING.                           * 00446300
*                                                                     * 00446400
* INPUT -      VTOC COMMON AREA ( VTOCOM )                            * 00446500
*              POINTED TO BY REGISTER 1                               * 00446600
*              USE PARSE DATA, FORMAT 1, 3, AND 4 DSCB'S              * 00446700
*              FOR DSORG, RECFM, ALLOC, USED, PROT, CATLG, OR SECAL   * 00446800
*              CALL VTOCFORM TO FORMAT THE PARMS.  USE FORMATTED DSCB * 00446900
*                                                                     * 00447000
* OUTPUT -     A RETURN CODE OF 0 TO CONTINUE PROCESSING OR 8 TO      * 00447100
*              EXCLUDE THIS DATA SET.                                 * 00447200
*                                                                     * 00447300
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00447400
*                                                                     * 00447500
*                                                                     * 00447600
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00447700
*                                                                     * 00447800
*                                                                     * 00447900
*********************************************************************** 00448000
         EJECT                                                          00448100
*        MACROS FOR CHECK ROUTINE                                       00448200
*                                                                       00448300
         MACRO                                                          00448400
&LABEL   VTCHL &KEY           CALL THE KEYWORD CHECK ROUTINE            00448500
&LABEL   L     R1,SUB&KEY.OPER    GET THE OPERATOR VALUE                00448600
         BAL   R8,GETOPER    TRANSLATE TO A NUMBER                      00448700
         ST    R15,REFOPER     SAVE IT TOO                              00448800
         LA    R1,SUB&KEY.VALU   GET THE VALUE PDL                      00448900
         ST    R1,REFVAL      SAVE THAT ADDRESS THREE                   00449000
         CLI   FLAGNM&KEY,0  HAS IT BEEN CONVERTED?                     00449100
         BNE   VTP&SYSNDX    YES, SKIP ALONG                            00449200
         MVI   FLAGNM&KEY,1  NOTE IT AS CONVERTED                       00449300
         BAL   R8,PDLNUM     GO CONVERT IT                              00449400
         ST    R15,NUMBER&KEY      SAVE THE VALUE                       00449500
         LA    R4,SUB&KEY.KEY  POINT TO THE IKJIDENT FOR THE KEYWORD    00449600
         BAL   R8,GETKEY     CONVERT TEXT TO A NUMERIC KEY              00449700
         STC   R15,NUMKEY&KEY  SAVE THAT NUMERIC KEY                    00449800
         LTR   R15,R15       WAS IT SUCCESSFUL?                         00449900
         BNZ   VTP&SYSNDX    YES, SKIP ALONG                            00450000
*        ISSUE A MESSAGE - A BAD LIM, AND, OR KEYWORD                   00450100
         MVC   MSGTEXT2,KEYERR  START THE ERROR MESSAGE                 00450200
         L     R1,0(R4)      POINT TO THE TEXT                          00450300
         MVC   MSGTEXT2+49(6),0(R1)  THEN ADD IT TO THE MESSAGE         00450400
         VTOCMSG MSGTEXT2    ISSUE THE ERROR MESSAGE                    00450500
VTP&SYSNDX DS  0H                                                       00450600
         SR    R1,R1         CLEAR A REGISTER                           00450700
         ICM   R1,1,NUMKEY&KEY GET THE KEYWORD VALUE                    00450800
         BZ    VTE&SYSNDX    IF NOT SET, SKIP THE EVALUATION            00450900
         ST    R1,REFKEY      SAVE THE ADDRESS                          00451000
         LA    R1,NUMBER&KEY  GET THE ADDRESS OF CONVERTED NUMBER       00451100
         ST    R1,REFNUM      SAVE IT'S ADDRESS                         00451200
         BAL   R8,LIMEVAL     GO EVALUATE THE EXPRESSION                00451300
VTE&SYSNDX DS  0H                                                       00451400
         MEND                                                           00451500
         SPACE 3                                                        00451600
         MACRO                                                          00451700
&LABEL   VTANDOR &NUM         EVALUATE, THEN DO AND OR OR FUNCTION      00451800
&LABEL   CLI   ANDOR&NUM.K+1,0  WAS THIS KEYWORD SET?                   00451900
         BE    LIMCOMP        NO, JUST CHECK THE FINAL RESULT           00452000
         VTCHL &NUM           YES, EVALUATE                             00452100
         CLI   ANDOR&NUM.K+1,1  WAS IT AN AND ?                         00452200
         BE    VTA&SYSNDX     YES, DO THE AND                           00452300
         O     R15,LIMVAL     NO, OR IT                                 00452400
         B     VTE&SYSNDX     FINISHED WITH THIS EXPRESSION             00452500
VTA&SYSNDX N   R15,LIMVAL     AND THE EXPRESSION VALUE                  00452600
VTE&SYSNDX ST  R15,LIMVAL     SAVE THE VALUE                            00452700
         MEND                                                           00452800
*                                                                       00452900
         EJECT                                                          00453000
VTOCCHEK ENTER 12,12          DO THE HOUSEKEEPING                       00453100
         LR    R11,R1         SAVE ADDR OF VTOCOM                       00453200
         USING VTOCOM,R11     SET ITS ADDRESSABILITY                    00453300
         L     R9,ADDRANSR    POINT TO THE PARSE ANSWER                 00453400
         USING PDL,R9         SET ITS ADDRESSABILITY                    00453500
         USING CHEKWORK,R13   SET ADDRESSABILITY FOR LOCAL WORK AREA    00453600
*                                                                       00453700
*        SEE WHAT THE FORMAT ID IS                                      00453800
*                                                                       00453900
CHEKFMT  L     R7,DSCBADDR    POINT TO THE DSCB                         00454000
         LA    R7,8(R7)       GET PAST THE HEADER                       00454100
         USING DSCB1,R7       SET ADDRESSABILITY                        00454200
         CLI   FORMATK+1,0    DID HE SPECIFY VARIOUS DSCB'S             00454300
         BNE   CHEKFMTI       YES, GO DO HIS CHECKS                     00454400
CHEKFMTI DS    0H             NOT YET PROGRAMMED                        00454500
*                                                                       00454600
*        STANDARD IS ONLY TO ALLOW FORMAT ONES TO GO                    00454700
*                                                                       00454800
         CLI   DS1FMTID,C'1'  IS THIS A FORMAT 1?                       00454900
         BNE   CHECKOUT       NO, EXCLUDE IT FROM FURTHER PROCESSING    00455000
*                                                                       00455100
*              FIRST SEE HOW BIG THE DSNAME IS                          00455200
*                                                                       00455300
         LA    R1,DS1FMTID    POINT PAST THE DSNAME                     00455400
         TRT   DS1DSNAM,BLKTRTAB  FIND THE FIRST BLANK                  00455500
         SR    R1,R7          SUBTRACT TO GET THE LENGTH                00455600
         STH   R1,DSNLEN      SAVE THE DSNAME LENGTH                    00455700
         LR    R3,R1          KEEP THE LENGTH FOR LATER                 00455800
*                                                                       00455900
*        GET THE FORMAT 3 DSCB, IF IT EXISTS                            00456000
*                                                                       00456100
         XC    FMT3,FMT3      CLEAR IT FIRST                            00456200
         CLC   DS1PTRDS,=XL5'0000000000'  IS THERE A FORMAT 3?          00456300
         BE    FMT3NO         NO, SKIP ALONG                            00456400
*                                                                       00456500
*        SET UP THE CAMLST                                              00456600
*                                                                       00456700
         MVC   CAMSEEK(4),CAMSCON  MOVE IN THE FIRST WORD OF CAMLST     00456800
         LA    R1,DS1PTRDS    GET THE CCHHR ADDRESS                     00456900
         ST    R1,CAMSEEK+4   SAVE IT                                   00457000
         LA    R1,VOLID       POINT TO THE VOLUME SERIAL                00457100
         ST    R1,CAMSEEK+8   SAVE IT                                   00457200
         LA    R1,FMT3        POINT TO THE AREA FOR THE DSCB3           00457300
         ST    R1,CAMSEEK+12  SAVE IT                                   00457400
         OBTAIN CAMSEEK       GET THE DSCB                              00457500
         LTR   R15,R15        TEST THE RETURN CODE                      00457600
         BNZ   OBT3ERR        BAD NEWS, ISSUE THE MESSAGE               00457700
*                                                                       00457800
*        PROCESS THE LEVEL KEYWORD                                      00457900
*                                                                       00458000
FMT3NO   CLI   LEVKEY+1,0     WAS LEVEL SPECIFIED?                      00458100
         BE    LEVEND         NO, SKIP ON                               00458200
         LA    R4,LEVEL       YES, POINT TO THE PDE                     00458300
         USING PDEDSNAM,R4    SET ADDRESSABILITY                        00458400
         LR    R2,R7          POINT TO THE START OF THE DSNAME          00458500
LEVNEXT  SR    R5,R5          CLEAR FOR INSERT                          00458600
         ICM   R5,B'0011',PDEDSNL  GET THE DSNAME LENGTH                00458700
         BZ    CHECKOUT       END OF THE LINE, EXCLUDE IT               00458800
         L     R6,PDEDSN      POINT TO THE LEVEL                        00458900
         CR    R3,R5          COMPARE LENGTHS                           00459000
         BL    LEVINC         THIS LEVEL IS LONGER THAN DSN, NO MATCH   00459100
         BCTR  R5,0           MINUS ONE FOR THE EX                      00459200
         EX    R5,COMPARE     CHECK THE LENGTHS                         00459300
         BE    LEVEND         IT MATCHES, ALLOW IT                      00459400
LEVINC   ICM   R4,B'0111',PDEDCHN GET THE NEXT LEVEL PDE POINTER        00459500
         BNZ   LEVNEXT        IF IT'S THERE, KEEP LOOKING               00459600
         B     CHECKOUT       NO MATCHES, EXCLUDE THIS DSNAME           00459700
LEVEND   DS    0H                                                       00459800
         DROP  R4             FINISHED WITH THE PDE                     00459900
*                                                                       00460000
*        PROCESS THE ENDING KEYWORD                                     00460100
*                                                                       00460200
         CLI   ENDKEY+1,0     WAS ENDING SPECIFIED?                     00460300
         BE    ENDEND         NO, SKIP ON                               00460400
         LA    R4,ENDING      YES, POINT TO THE PDE                     00460500
         USING PDEDSNAM,R4    SET ADDRESSABILITY                        00460600
ENDNEXT  SR    R5,R5          CLEAR FOR INSERT                          00460700
         ICM   R5,B'0011',PDEDSNL  GET THE DSNAME LENGTH                00460800
         BZ    CHECKOUT       END OF THE LINE, EXCLUDE IT               00460900
         L     R6,PDEDSN      POINT TO THE ENDING                       00461000
         CR    R3,R5          COMPARE LENGTHS                           00461100
         BL    ENDINC         THIS ENDING IS LONGER THAN DSN, NO MATCH  00461200
         LR    R2,R7          POINT TO THE START OF THE DSNAME          00461300
         AR    R2,R3          POINT TO THE END                          00461400
         SR    R2,R5          BACKUP TO COMPARE THIS LENGTH             00461500
         BCTR  R5,0           MINUS ONE FOR THE EX                      00461600
         EX    R5,COMPARE     CHECK THE LENGTHS                         00461700
         BE    ENDEND         IT MATCHES, ALLOW IT                      00461800
ENDINC   ICM   R4,B'0111',PDEDCHN GET THE NEXT ENDING PDE POINTER       00461900
         BNZ   ENDNEXT        IF IT'S THERE, KEEP LOOKING               00462000
         B     CHECKOUT       NO MATCHES, EXCLUDE THIS DSNAME           00462100
ENDEND   DS    0H                                                       00462200
         DROP  R4             FINISHED WITH THE PDE                     00462300
*                                                                       00462400
*        PROCESS THE CONTAINING KEYWORD                                 00462500
*                                                                       00462600
         CLI   CONTAINK+1,0   WAS CONTAINING SPECIFIED?                 00462700
         BE    CONEND         NO, SKIP ON                               00462800
         LA    R4,CONTAIN     YES, POINT TO THE PDE                     00462900
         USING PDEDSNAM,R4    SET ADDRESSABILITY                        00463000
CONNEXT  SR    R5,R5          CLEAR FOR INSERT                          00463100
         ICM   R5,B'0011',PDEDSNL  GET THE DSNAME LENGTH                00463200
         BZ    CHECKOUT       END OF THE LINE, EXCLUDE IT               00463300
         L     R6,PDEDSN      POINT TO THE CONTAINING                   00463400
         CR    R3,R5          COMPARE LENGTHS                           00463500
         BL    CONINC         THIS CONTAIN IS LONGER THAN DSN, NO MATCH 00463600
         LR    R1,R7          POINT TO THE START OF THE DSNAME          00463700
         AR    R1,R3          POINT TO THE END                          00463800
         SR    R1,R5          BACKUP TO COMPARE THIS LENGTH - LAST ONE  00463900
         LR    R2,R7          POINT TO THE START OF THE DSNAME          00464000
         BCTR  R5,0           MINUS ONE FOR THE EX                      00464100
CONCOMP  EX    R5,COMPARE     CHECK THE LENGTHS                         00464200
         BE    CONEND         IT MATCHES, ALLOW IT                      00464300
         LA    R2,1(R2)       CHECK THE WHOLE DSNAME                    00464400
         CR    R2,R1          CHECK FOR THE END OF THE REAL DSN         00464500
         BNH   CONCOMP        NOT THERE YET                             00464600
CONINC   ICM   R4,B'0111',PDEDCHN GET THE NEXT CONTAINING PDE POINTER   00464700
         BNZ   CONNEXT        IF IT'S THERE, KEEP LOOKING               00464800
         B     CHECKOUT       NO MATCHES, EXCLUDE THIS DSNAME           00464900
CONEND   DS    0H                                                       00465000
         DROP  R4             FINISHED WITH THE PDE                     00465100
*                                                                       00465200
*        NOW THE BIG MESS, CHECK FOR LIMIT, AND'S, AND OR'S             00465300
*                                                                       00465400
         CLI   LIMITK+1,0     WAS LIMIT SPECIFIED                       00465500
         BE    LIMEND         NO, THEN THERE CAN BE NO AND'S OR OR'S    00465600
         NI    VTCFMTCK,255-VTCFMTCC  TURN OFF THE ROUTINE CALL FLAG    00465700
         VTCHL L              EVALUATE THE LIMIT 1=TRUE 0=FALSE         00465800
         ST    R15,LIMVAL     SAVE THE ANSWER                           00465900
         VTANDOR 1            CHECK AND1 OR OR1                         00466000
         VTANDOR 2            CHECK AND2 OR OR2                         00466100
         VTANDOR 3            CHECK AND3 OR OR3                         00466200
LIMCOMP  L     R15,LIMVAL     GET THE RESULT OF ALL THIS                00466300
         LTR   R15,R15        TEST IT                                   00466400
         BZ    CHECKOUT       IT GETS EXCLUDED                          00466500
LIMEND   DS    0H                                                       00466600
         B     CHECKIN        ALL TESTS PASSED, INCLUDE THIS ONE        00466700
         SPACE 5                                                        00466800
*                                                                       00466900
*        EVALUATION ROUTINE FOR   KEYWORD  OPER  VALUE                  00467000
*                                                                       00467100
LIMEVAL  L     R5,REFKEY      GET THE KEYWORD VALUE                     00467200
         MH    R5,H12         MULTIPLY IT BY 12                         00467300
         A     R5,ATABTITL    THEN RELOCATE IT                          00467400
         SR    R10,R10         CLEAR THE ROUTINE POINTER                00467500
         TM    1(R5),X'80'    IS IT A FORMATTED ITEM?                   00467600
         BO    LIMFORM        YES, GO DO IT                             00467700
         SR    R6,R6         CLEAR A REGISTER                           00467800
         IC    R6,2(R5)      GET THE OFFSET INTO VTFMT                  00467900
         IC    R10,1(R5)      GET THE ROUTINE NUMBER                    00468000
         LTR   R10,R10         SEE IF IT'S A GOOD NUMBER                00468100
         BP    LIMEVAL1       IT'S ALL RIGHT                            00468200
LIMABEND ABEND 702,DUMP       CRASH AND BURN                            00468300
LIMEVAL1 B     *(R10)         AND GO TO IT                              00468400
         B     LIMDATE        CDATE, EXPDT, REFDT                       00468500
         B     LIMLUSE        *** DUMMY ENTRY ***                       00468600
         B     LIMFORM        FORM , USED SPACE                         00468700
         B     LIMFORM        UNUSED SPACE                              00468800
         B     LIMFORM        PCT USED                                  00468900
         B     LIMEXT         EXTENTS                                   00469000
         B     LIMBLREC       LRECL, BLKSZ, SEC Q                       00469100
         B     LIMCCHH       CCHH CHECKING                              00469200
         SPACE 5                                                        00469300
*                                                                       00469400
*        PERFORM THE FORMATTED ITEM CHECKING                            00469500
*                                                                       00469600
LIMFORM  DS    0H                                                       00469700
         TM    VTCFMTCK,VTCFMTCC  WAS FORMAT CALLED BEFORE FOR THIS DS  00469800
         BO    LIMFCALD       YES, DON'T CALL IT AGAIN                  00469900
         VTCALL FORM          NO, CALL IT TO GET THE ITEMS              00470000
         OI    VTCFMTCK,VTCFMTCC+VTCFMTCD  THEN SET THE SWITCHES        00470100
LIMFCALD SR    R2,R2          CLEAR A WORK REG                          00470200
         A     R6,FORMATAD   RELOCATE THE BLOCK                         00470300
         B     *+4(R10)         AND GO TO IT                            00470400
         B     LIMFORMA      FORMATTED ITEM                             00470500
         B     LIMABEND       CDATE, EXPDT                              00470600
         B     LIMABEND       LAST USE DATE                             00470700
         B     LIMALLOC       ALLOC, USED SPACE                         00470800
         B     LIMUNUSD       UNUSED SPACE                              00470900
         B     LIMPCT         PCT USED                                  00471000
LIMFORMA DS    0H                                                       00471100
         IC    R2,2(R5)       GET THE OFFSET IN VTFMT                   00471200
         A     R2,FORMATAD    THEN RELOCATE IT                          00471300
         L     R4,REFVAL      GET THE VALUE PDE                         00471400
         LH    R3,4(R4)       GET THE LENGTH OF THE STRING              00471500
         L     R1,0(R4)       AND ITS ADDRESS                           00471600
*                                                                       00471700
*        DO THE ACTUAL COMPARE                                          00471800
*                                                                       00471900
         BCTR  R3,0           DOWN ONE FOR AN EX                        00472000
         EX    R3,COMPLIM     COMPARE AS SPECIFIED                      00472100
COMPDONE DS    0H             GET THE OPERATOR ADDRESS                  00472200
         BL    COMPLOW        CHECK THE OPERATOR, VALUE LESS THAN ITEM  00472300
         BE    COMPEQ         CHECK THE OPERATOR, KEYWORD EQUALS VALUE  00472400
*                             KEYWORD IS GREATER THAN THE VALUE         00472500
COMPHI   CLI   REFOPER+3,NE       WAS OPERATOR NE                       00472600
         BE    COMPYES        HIGH SATISFIES THE EXPRESSION             00472700
         CLI   REFOPER+3,GT       ALSO FOR GT                           00472800
         BE    COMPYES        HIGH SATISFIES THE EXPRESSION             00472900
         CLI   REFOPER+3,GE       AND FOR GE                            00473000
         BE    COMPYES        HIGH SATISFIES THE EXPRESSION             00473100
         B     COMPNO         THIS ONE DOESN'T FIT                      00473200
*                             KEYWORD IS EQUAL TO THE VALUE             00473300
COMPEQ   CLI   REFOPER+3,EQ       WAS OPERATOR EQ                       00473400
         BE    COMPYES        EQ   SATISFIES THE EXPRESSION             00473500
         CLI   REFOPER+3,LE       ALSO FOR LE                           00473600
         BE    COMPYES        EQ   SATISFIES THE EXPRESSION             00473700
         CLI   REFOPER+3,GE       AND FOR GE                            00473800
         BE    COMPYES        EQ   SATISFIES THE EXPRESSION             00473900
         B     COMPNO         THIS ONE DOESN'T FIT                      00474000
*                             KEYWORD IS LESS THAN THE VALUE            00474100
COMPLOW  CLI   REFOPER+3,NE       WAS OPERATOR NE                       00474200
         BE    COMPYES        LOW  SATISFIES THE EXPRESSION             00474300
         CLI   REFOPER+3,LT       ALSO FOR LT                           00474400
         BE    COMPYES        LOW  SATISFIES THE EXPRESSION             00474500
         CLI   REFOPER+3,LE       AND FOR LE                            00474600
         BE    COMPYES        LOW  SATISFIES THE EXPRESSION             00474700
         B     COMPNO         THIS ONE DOESN'T FIT                      00474800
*                                                                       00474900
*        IT FITS OR IT DOESN'T                                          00475000
*                                                                       00475100
COMPYES  LA    R15,1          SET A TRUE VALUE                          00475200
         BR    R8             THEN RETURN                               00475300
COMPNO   SR    R15,R15        SET A FALSE VALUE                         00475400
         BR    R8             THEN RETURN                               00475500
*                                                                       00475600
*        SPECIAL ROUTINES TO CHECK NON-FORMATTED ITEMS                  00475700
*                                                                       00475800
LIMDATE  DS    0H                                                       00475900
*                                                                       00476000
*        COMPARE DATES                                                  00476100
*                                                                       00476200
         LA    R14,DS1CREDT  POINT TO CREATION DATE                     00476300
         CLI   REFKEY+3,CDATE      IS THAT IT?                          00476400
         BE    LIMDGET       YES, THIS IS IT                            00476500
         LA    R14,DS1EXPDT  POINT TO EXPIRATION DATE                   00476600
         CLI   REFKEY+3,EXPDT      IS THAT IT?                          00476700
         BE    LIMDGET       YES, THIS IS IT                            00476800
         LA    R14,DS1REFD   NO, USE REFERENCE DATE                     00476900
LIMDGET  SR    R15,R15       CLEAR A WORK REG                           00477000
         IC    R15,0(R14)    GET THE YEAR                               00477100
         MH    R15,H1000     TIMES 1000                                 00477200
         SR    R1,R1         CLEAR ANOTHER WORK REG                     00477300
         ICM   R1,3,1(R14)    GET THE DAYS                              00477400
         AR    R15,R1        PUT THE DATE TOGETHER                      00477500
         L     R1,REFNUM     GET THE ADDRESS OF THE VALUE               00477600
         L     R1,0(R1)      GET THE VALUE ITSELF                       00477700
         CR    R15,R1        COMPARE THEM                               00477800
         B     COMPDONE      GO CHECK OPERANDS                          00477900
LIMLUSE  DS    0H                                                       00478000
*                                                                       00478100
*        LAST USE DATE                                                  00478200
*                                                                       00478300
         SR    R14,R14       CLEAR THE DATE                             00478400
         CLC   ZERO,75(R7)   CHECK FOR NO DATA                          00478500
         BE    LIMLUCMP      RIGHT, SKIP ON                             00478600
         MVC   CHEKDBLW+5(3),75(R7)  MOVE IN THE LAST USE DATE          00478700
         CVB   R14,CHEKDBLW  CONVERT IT TO BINARY                       00478800
LIMLUCMP L     R1,REFNUM     GET THE ADDRESS OF THE VALUE               00478900
         L     R1,0(R1)      GET THE VALUE                              00479000
         CR    R14,R1        DO THE COMPARE                             00479100
         B     COMPDONE      THEN CHASE DOWN THE OPERANDS               00479200
LIMEXT   DS    0H                                                       00479300
*                                                                       00479400
*        EXTENTS                                                        00479500
*                                                                       00479600
         L     R1,REFNUM     GET THE COMPARE VALUE ADDRESS              00479700
         CLC   DS1NOEPV,3(R1)        COMPARE THEM                       00479800
         B     COMPDONE      GO CHECK OPERANDS                          00479900
LIMBLREC DS    0H                                                       00480000
*                                                                       00480100
*        LRECL, BLKSZ, SECQ                                             00480200
*                                                                       00480300
         L     R1,REFNUM     GET THE ADDRESS OF THE COMPARE VALUE       00480400
         L     R15,0(R1)      THEN GET THE VALUE ITSELF                 00480500
         CLI   REFKEY+3,BLKSZ      BLOCK SIZE?                          00480600
         BNE   LIMB1         NO, KEEP CHECKING                          00480700
         LH    R1,DS1BLKL    COMPARE TO THE BLOCK SIZE                  00480800
         CR    R1,R15        COMPARE THEM                               00480900
         B     COMPDONE      GO SIFT THROUGH THE OPERANDS               00481000
LIMB1    CLI   REFKEY+3,LRECL  LOGICAL RECORD LENGTH                    00481100
         BNE   LIMB2         NO, KEEP GOING                             00481200
         LH    R1,DS1LRECL   COMPARE TO THE LRECL                       00481300
         CR    R1,R15        COMPARE THEM                               00481400
         B     COMPDONE      GO CHECK THE OPERANDS                      00481500
LIMB2    MVC   HWORK,DS1SCALO+2 GET THE SECONDARY QUANTITY              00481600
         LH    R1,HWORK      DO THE COMPARE                             00481700
         CR    R1,R15        COMPARE THEM                               00481800
         B     COMPDONE      THEN CHECK THE OPERANDS                    00481900
*                                                                       00482000
*        SPACE CHECKING ROUTINES                                        00482100
*                                                                       00482200
LIMALLOC DS    0H                                                       00482300
*                                                                       00482400
*        ALLOC AND USED                                                 00482500
*                                                                       00482600
         L     R1,REFNUM     GET THE ADDRESS OF THE CONVERTED NUMBER    00482700
         L     R1,0(R1)      GET THE VALUE                              00482800
         L     R15,0(R6)     GET THE AMOUNT                             00482900
         CR    R15,R1        COMPARE THEM                               00483000
         B     COMPDONE      THEN CHECK THE OPERANDS                    00483100
LIMUNUSD DS    0H                                                       00483200
*                                                                       00483300
*        UNUSED                                                         00483400
*                                                                       00483500
         L     R6,FORMATAD   POINT TO THE FORMATTED VTOC                00483600
         USING VTFMT,R6      SET ADDRESSABILITY                         00483700
         ICM   R14,15,VTFUSED      GET THE AMOUNT USED                  00483800
         BM    LIMUNUAL      IF MINUS, WE DON'T KNOW                    00483900
         L     R14,VTFALLOC  GET ALLOC                                  00484000
         S     R14,VTFUSED   MINUS THE AMOUNT USED                      00484100
LIMUNUAL L     R1,REFNUM     GET THE ENTERED VALUE                      00484200
         L     R1,0(R1)      NOW ITS VALUE FOR REAL                     00484300
         CR    R14,R1        COMPARE THE VALUES                         00484400
         B     COMPDONE      THEN GO CHECK THE OPERANDS                 00484500
LIMPCT   DS    0H                                                       00484600
*                                                                       00484700
*        PER CENT                                                       00484800
*                                                                       00484900
         L     R6,FORMATAD   POINT TO THE FORMATTED VTOC                00485000
         USING VTFMT,R6      SET ADDRESSABILITY                         00485100
         SR    R14,R14       CLEAR A REGISTER                           00485200
         ICM   R15,15,VTFUSED      GET THE AMOUNT USED                  00485300
         BM    LIMP100       IF UNKNOWN USED, SET 100 PER CENT          00485400
         CLC   VTFALLOC,ZERO ZERO ALLOCATED SPACE?                      00485500
         BNE   LIMPCTOK      NO, CONTINUE                               00485600
         CLC   VTFUSED,ZERO  ZERO USED SPACE?                           00485700
         BE    LIMPCOMP      YES, PCT IS ZERO                           00485800
*              ZERO ALLOCATED, NONZERO USED, INCLUDE THIS ONE           00485900
         B     COMPYES                                                  00486000
LIMP100  LA    R15,100       SET UP 100 PER CENT                        00486100
         B     LIMPCOMP      GO COMPARE                                 00486200
LIMPCTOK M     R14,F100      MULTIPLY BY 100 PERCENT                    00486300
         D     R14,VTFALLOC  DIVIDE BY THE ALLOCATION                   00486400
LIMPCOMP L     R1,REFNUM     GET THE VALUE ADDRESS                      00486500
         L     R1,0(R1)      THEN THE VALUE                             00486600
         CR    R15,R1        THEN COMPARE THEM                          00486700
         B     COMPDONE      THEN GO SIFT THROUGH THE OPERANDS          00486800
*                                                                       00486900
*        CCHH CHECKING IS NOT QUITE STANDARD BECAUSE THERE MAY          00487000
*        MAY BE UP TO 16 EXTENTS TO COMPARE.  THE DATA MAY BE           00487100
*        CC OR CCHH FORMATS.  THE DATA SET MAY BE EQUAL TO,             00487200
*        LESS THAN, AND GREATER THAN ANY PARTICULAR VALUE.              00487300
*                                                                       00487400
LIMCCHH  DS    0H                                                       00487500
*                                                                       00487600
*        FIRST SEE IF THE CCHH WAS CONVERTED                            00487700
*              CONVERT IT IF NOT, SKIP IF IT'S DONE                     00487800
*                                                                       00487900
         L     R5,REFNUM     GET THE ADDRESS OF THE COMPARISON VALUE    00488000
         L     R4,0(R5)      GET THE VALUE                              00488100
         ICM   R6,3,4(R5)    GET THE COMPARE LENGTH                     00488200
         BP    LIMCSET       IF IT'S SET, THE CONVERSION IS DONE        00488300
*                      IT WASN'T SET, CONVERT FROM CHARS TO BINARY      00488400
         L     R1,REFVAL     GET THE ADDRESS OF THE IKJIDENT            00488500
         L     R2,0(R1)      POINT TO THE TEXT                          00488600
         LH    R3,4(R1)      GET THE LENGTH OF THE TEXT                 00488700
*        IT SHOULD BE 4 OR 8 CHARACTERS                                 00488800
         XC    DOUBLE,DOUBLE CLEAR OUT A PLACE TO WORK                  00488900
         CH    R3,H4         IS IT A CYLINDER ONLY?                     00489000
         BH    LIMCCON2      NO, TRY FOR A CCHH                         00489100
         BE    LIMCCON1      YES, JUST CONVERT IT                       00489200
         VTOCMSG CCHHLEN     LESS THAN FOUR CHARS, ISSUE A MSG          00489300
LIMCCON1 LA    R6,1          SET THE COMPARE LENGTH                     00489400
         B     LIMCMOVE      GO MOVE IT IN                              00489500
LIMCCON2 LA    R6,3          SET THE COMPARE LENGTH                     00489600
         CH    R3,H8         WAS IT A CCHH?                             00489700
         BE    LIMCMOVE      YES, JUST THE RIGHT LENGTH                 00489800
         VTOCMSG CCHHLEN     WARN THE PERSON                            00489900
         CH    R3,H8         CHECK AGAIN                                00490000
         BL    LIMCMOVE      IS IT OVER 8 CHARS?                        00490100
         LH    R3,H8         YES, SET IT FOR THE MAX - IGNORE RR        00490200
LIMCMOVE BCTR  R3,0          MINUS ONE FOR THE EX                       00490300
         EX    R3,MOVECCHH   MOVE IN THE CHARS                          00490400
         TR    DOUBLE,DECTABLE TRANSLATE HEX EBCDIC TO HEX BINARY       00490500
         PACK  CYLH(5),DOUBLE(9)  SQUISH OUT THE ZONES                  00490600
         L     R4,CYLH       GET THE CCHH                               00490700
         ST    R4,0(R5)      SAVE IT FOR LATER                          00490800
         STH   R6,4(R5)      SAVE THE LENGTH TOO                        00490900
LIMCSET  DS    0H            THE NUMBER IS CONVERTED                    00491000
*                                                                       00491100
*        COMPARE THE EXTENTS TO THE CCHH VALUE.                         00491200
*        ANY EXTENT MAY BE LT, EQ, AND GT A PARTICULAR                  00491300
*        VALUE, AND ALL THE EXTENTS MUST BE CHECKED.                    00491400
*                                                                       00491500
         MVI   CCHHCOMP,0    CLEAR THE FLAGS                            00491600
         SR    R2,R2         CLEAR A REG FOR AN EXTENT COUNTER          00491700
         ICM   R2,1,DS1NOEPV GET THE NUMBER OF EXTENTS                  00491800
         BZ    COMPNO        NO EXTENTS, JUST GO SEE                    00491900
*                                                                       00492000
*        GET EACH EXTENT AND PROCESS IT                                 00492100
*                                                                       00492200
         SR    R1,R1         FIRST EXTENT                               00492300
EXTNEXT  LR    R3,R1         GET THE CURRENT EXTENT                     00492400
         SLL   R3,2          TIMES 4                                    00492500
         EX    R0,GETEXT(R3) GET THE EXTENT ADDRESS INTO R3             00492600
*                                                                       00492700
*        CHECK THE BOTTOM OF THE EXTENT                                 00492800
*                                                                       00492900
         NI    CCHHCOMP,255-CCHHX TURN OFF THE STRADDLE FLAG            00493000
         EX    R6,CLCEXTLO    DO THE COMPARE                            00493100
         BH    SETH1         THE FIELD IS HIGHER THAN THE VALUE         00493200
         BE    SETEQ1        THE FIELD IS EQUAL TO THE VALUE            00493300
         OI    CCHHCOMP,CCHHLOW+CCHHX  LOWER -  POSSIBLE STRADDLE       00493400
         B     CHECKHI       GO CHECK THE TOP OF THIS EXTENT            00493500
SETH1    OI    CCHHCOMP,CCHHHIGH  SET THE FLAG                          00493600
         B     CHECKHI       GO CHECK THE TOP OF THIS EXTENT            00493700
SETEQ1   OI    CCHHCOMP,CCHHEQ   SET THE FLAG                           00493800
*                                                                       00493900
*        CHECK THE TOP OF THE EXTENT                                    00494000
*                                                                       00494100
CHECKHI  EX    R6,CLCEXTHI   DO THE COMPARE                             00494200
         BE    SETEQ2        EQUAL, GO SET IT                           00494300
         BL    EXTSET        LOW, GO SET IT                             00494400
*                                                                       00494500
*        THIS IS THE ONLY SLIGHTLY TRICKY PART, A STRADDLE              00494600
*        IF THE BOTTOM OF THE EXTENT IS LOWER THAN THE VALUE AND THE    00494700
*        TOP OF THE EXTENT IS HIGHER THAN THE VALUE, THEN THE           00494800
*        EQ FLAG SHOULD BE SET TOO.                                     00494900
*                                                                       00495000
         OI    CCHHCOMP,CCHHHIGH  SET THE HIGH FLAG                     00495100
         TM    CCHHCOMP,CCHHX     WAS THE BOTTOM LOWER THAN THE VALUE?  00495200
         BZ    EXTSET        NO, SKIP ON                                00495300
SETEQ2   OI    CCHHCOMP,CCHHEQ    SET THE EQ FLAG                       00495400
*                                                                       00495500
*        FINISHED WITH THAT EXTENT, CHECK FOR MORE                      00495600
*                                                                       00495700
EXTSET   DS    0H                                                       00495800
         LA    R1,1(R1)      INCREMENT THE EXTENT COUNTER               00495900
         CR    R1,R2         CHECK THE EXTENT COUNTER                   00496000
         BNL   LIMCOPER      THAT'S ALL FOLKS                           00496100
         TM    CCHHCOMP,CCHHHIGH+CCHHEQ+CCHHLOW  ARE THEY ALL SET?      00496200
         BNO   EXTNEXT       NO, CONTINUE LOOKING                       00496300
*                            YES, STOP NOW - ALL THE FLAGS ARE SET      00496400
LIMCOPER L     R4,REFOPER    GET THE NUMERIC VALUE OF THE KEY           00496500
         IC    R4,CCHHTAB(R4)  GET A FLAG MASK                          00496600
         EX    R4,CCHHOPER   CHECK TO SEE IF THE CONDITION IS SET       00496700
         BZ    COMPNO        NOT THERE                                  00496800
         B     COMPYES       YES                                        00496900
*                                                                       00497000
*        EXECUTED INSTRUCTIONS TO GET THE ADDRESS OF THIS EXTENT        00497100
*                                                                       00497200
GETEXT   LA    R3,DS1EXT1     1ST EXTENT                                00497300
         LA    R3,DS1EXT2     2ND EXTENT                                00497400
         LA    R3,DS1EXT3     3RD EXTENT                                00497500
         LA    R3,DS3EXTNT    4TH EXTENT                                00497600
         LA    R3,DS3EXTNT+10 5TH EXTENT                                00497700
         LA    R3,DS3EXTNT+20 6TH EXTENT                                00497800
         LA    R3,DS3EXTNT+30 7TH EXTENT                                00497900
         LA    R3,DS3ADEXT    8TH EXTENT                                00498000
         LA    R3,DS3ADEXT+10 9TH EXTENT                                00498100
         LA    R3,DS3ADEXT+20 10TH EXTENT                               00498200
         LA    R3,DS3ADEXT+30 11TH EXTENT                               00498300
         LA    R3,DS3ADEXT+40 12TH EXTENT                               00498400
         LA    R3,DS3ADEXT+50 13TH EXTENT                               00498500
         LA    R3,DS3ADEXT+60 14TH EXTENT                               00498600
         LA    R3,DS3ADEXT+70 15TH EXTENT                               00498700
         LA    R3,DS3ADEXT+80 16TH EXTENT                               00498800
*                                                                       00498900
*        ISSUE ERROR MESSAGES AND RETURN                                00499000
*                                                                       00499100
OBT3ERR  VTOCMSG OBT3ERRM   OBTAIN ERROR MESSAGE                        00499200
CHECKOUT LA    R15,8          EXCLUDE THIS DATA SET                     00499300
         B     CHEKRET        RETURN                                    00499400
*                                                                       00499500
CHECKIN  SR    R15,R15        CLEAR THE REGISTER, PROCESS THIS DATA SET 00499600
CHEKRET  LEAVE EQ                                                       00499700
*                                                                       00499800
*                                                                       00499900
         EJECT                                                          00500000
*                                                                       00500100
*        ROUTINES USED ABOVE                                            00500200
*                                                                       00500300
         EJECT                                                          00500400
*                                                                       00500500
*        PDLNUM - CONVERT FROM CHARACTERS ( EBCDIC ) TO AN INTEGER      00500600
*              BINARY FORM, PASSED BACK VIA REGISTER 15                 00500700
*              A PARSE PDE IS THE INPUT AS SHOWN IN THE SAMPLE BELOW    00500800
*                       LA    R1,PDL     POINT TO THE PARSE DECRIPTION  00500900
*                       BAL   R8,PDLNUM  GO CONVERT TO NUMERICS         00501000
*              THE ROUTINE WILL TERMINATE IF IT FINDS NON-NUMERICS      00501100
*                 ANY CHARACTERS OTHER THEN 0-9, +, -                   00501200
*              REGISTERS 1, 2, 5, 6, AND 7 ARE USED                     00501300
*                                                                       00501400
PDLNUM   STM   R1,R8,PDLNSAVE SAVE THE REGISTERS                        00501500
         LH    R2,4(R1)       GET THE STRING ADDRESS                    00501600
         L     R1,0(R1)       GET THE STRING ADDRESS                    00501700
         MVI   PDLMINUS,0     CLEAR THE NEGATIVE NUMBER FLAG            00501800
         SR    R5,R5          CLEAR THE CHARACTER COUNTER               00501900
         SR    R15,R15        CLEAR THE ANSWER                          00502000
PDLLOOP  LA    R6,0(R5,R1)    POINT TO THIS DIGIT                       00502100
         LA    R5,1(R5)       GET TO THE NEXT DIGIT                     00502200
         CR    R5,R2          IS THIS THE END OF THE STRING?            00502300
         BH    PDLFINI        YES, EXIT                                 00502400
         SR    R7,R7          CLEAR A WORK REGISTER                     00502500
         IC    R7,0(R6)       GET THE CHARACTER                         00502600
         SH    R7,PDLH240     SUBTRACT THE CHARACTER C'0'               00502700
         BM    PDLSP          IF NEGATIVE, CHECK SPECIAL CHARACTERS     00502800
         MH    R15,PDLH10     IT'S A DIGIT, MULTIPLY PRIOR NUM BY TEN   00502900
         AR    R15,R7         ADD ON THE NEW DIGIT                      00503000
         B     PDLLOOP        AND LOOP FOR MORE                         00503100
*                                                                       00503200
*        CHECK FOR SPECIAL CHARACTERS                                   00503300
*                                                                       00503400
PDLSP    CLI   0(R6),C' '     IS IT A BLANK?                            00503500
         BE    PDLLOOP        THEN IT'S OK                              00503600
         CLI   0(R6),C'+'     IS IT A PLUS?                             00503700
         BE    PDLLOOP        THAT'S ALSO OK                            00503800
         CLI   0(R6),C'-'     IS IT A MINUS?                            00503900
         BNE   PDLFINI        NO, JUST QUIT                             00504000
         MVI   PDLMINUS,1     YES, NOTE IT                              00504100
         B     PDLLOOP        AND LOOK FOR MORE                         00504200
*                                                                       00504300
*        QUIT, AFTER SETTING R15 TO NEGATIVE IF NEEDED                  00504400
*                                                                       00504500
PDLFINI  CLI   PDLMINUS,1     WAS A MINUS SIGN FOUND?                   00504600
         BNE   PDLLEAVE       NO, EXIT                                  00504700
         LNR   R15,R15        YES, MAKE IT NEGATIVE                     00504800
PDLLEAVE LM    R1,R8,PDLNSAVE RESTORE THE REGISTERS                     00504900
         BR    R8             RETURN                                    00505000
PDLH10   DC    H'10'                                                    00505100
PDLH240  DC    H'240'                                                   00505200
         EJECT                                                          00505300
*                                                                       00505400
*        ROUTINE TO CONVERT A TEXT DSCB ITEM                            00505500
*        INTO ITS KEY NUMBER                                            00505600
*        INPUT IS REG 4 - IKJIDENT PTR                                  00505700
*        OUTPUT IS REG 15 - KEY NUMBER                                  00505800
*        ENTRY VIA BAL   R8,GETKEY                                      00505900
*                                                                       00506000
GETKEY   L     R1,ATABTITL     POINT TO THE TABLE                       00506100
         LA    R1,12(R1)     POINT TO THE FIRST ENTRY                   00506200
         LA    R15,1           SET UP THE KEY NUMBER COUNTER            00506300
         L     R6,0(R4)      POINT TO THE ENTERED TEXT                  00506400
         ICM   R3,3,4(R4)    GET THE LENGTH OF THE ENTERED TEXT         00506500
         BNP   GETKNOTF      NOT FOUND IF ZERO                          00506600
         BCTR  R3,0          MINUS ONE FOR THE EX                       00506700
GETKLOOP LA    R2,4(R1)      POINT TO THE COMPARISON TEXT               00506800
         CLI   0(R2),C' '    IS IT HERE?                                00506900
         BNE   GETKSTD       YES, THIS IS IT                            00507000
         LA    R2,1(R2)      NO, MOVE OVER ONE MORE                     00507100
         CLI   0(R2),C' '    IS IT HERE?                                00507200
         BNE   GETKSTD       YES, THIS IS IT                            00507300
         LA    R2,1(R2)      NO, MOVE OVER ONE MORE                     00507400
GETKSTD  EX    R3,GETKCOMP   COMPARE THE KEY TEXT                       00507500
         BE    GETKFND       I FOUND IT                                 00507600
         LA    R1,12(R1)     GET TO THE NEXT KEY                        00507700
         LA    R15,1(R15)    INCREMENT THE KEY COUNTER                  00507800
         CH    R15,H26       CHECK FOR THE END OF THE TABLE             00507900
         BNH   GETKLOOP      NOT YET, KEEP LOOKING                      00508000
*                                                                       00508100
*        KEY WAS NOT FOUND, SEND BACK A ZERO                            00508200
*                                                                       00508300
GETKNOTF SR    R15,R15       SET UP THE ZERO AND RETURN                 00508400
GETKFND  BR    R8            JUST RETURN                                00508500
GETKCOMP CLC   0(0,R6),0(R2) EXECUTED TEXT COMPARE                      00508600
H26      DC    H'26'                                                    00508700
         EJECT                                                          00508800
*                                                                       00508900
*        ROUTINE TO CONVERT THE OPERATOR TEXT                           00509000
*        INTO A NUMERIC VALUE                                           00509100
*                                                                       00509200
GETOPER  LA    R15,1         NUMERIC VALUE COUNTER                      00509300
GETOLOOP LR    R14,R15       GET THE NUMBER                             00509400
         SLA   R14,1         MULTIPLY BY 2                              00509500
         LA    R14,OPERS(R14)      RELOCATE IT                          00509600
         CLC   0(2,R14),0(R1)      IS THIS THE TEXT?                    00509700
         BE    GETOFND       YES, RETURN THE NUMBER                     00509800
         LA    R15,1(R15)    NO, TRY THE NEXT ONE                       00509900
         CH    R15,H7        CHECK FOR THE END                          00510000
         BL    GETOLOOP      NOT THERE YET, KEEP TRYING                 00510100
         VTOCMSG OPERERR,OPERERR2  ISSUE THE MESSAGE                    00510200
         LA    R15,1         SET THE DEFAULT OPERATOR, EQ               00510300
GETOFND  BR    R8            THEN RETURN                                00510400
         EJECT                                                          00510500
*                                                                       00510600
*        PROGRAM CONSTANTS                                              00510700
*                                                                       00510800
COMPLIM  CLC   0(0,R2),0(R1)  COMPARE KEYWORD TO VALUE                  00510900
MOVECCHH MVC   DOUBLE(0),0(R2)                                          00511000
CLCEXTLO CLC   2(0,R3),0(R5)                                            00511100
CLCEXTHI CLC   6(0,R3),0(R5)                                            00511200
CCHHOPER TM    CCHHCOMP,0                                               00511300
CCHHTAB  DC    X'0040A0206080C0'  CCHHCOMP FLAGS                        00511400
OPERS    DC    C'  EQNELTLEGTGE'                                        00511500
*    FOR  EQ, NE, LT, LE, GT, GE                                        00511600
EDMASK   DC    XL16'40202020202020202020202020202120'                   00511700
BLANKS   DC    CL16'                '                                   00511800
STARS    DC    CL16'****************'                                   00511900
BLKTRTAB DC    XL64'00',X'04',XL192'00'                                 00512000
CAMSCON  CAMLST SEEK,*,*,*                                              00512100
COMPARE  CLC   0(0,R6),0(R2)  EXECUTED COMPARE                          00512200
DECTABLE EQU   *-C'A'   CONVERT EBCDIC HEX TO BINARY                    00512300
         DC    X'0A0B0C0D0E0F'                                          00512400
         DC    (C'0'-C'F'-1)X'FF'  FILLER                               00512500
         DC    X'00010203040506070809'                                  00512600
*                                                                       00512700
*                                                                       00512800
*                                                                       00512900
H3       DC    H'3'                                                     00513000
H4       DC    H'4'                                                     00513100
H7       DC    H'7'                                                     00513200
H8       DC    H'8'                                                     00513300
H10      DC    H'10'                                                    00513400
H12      DC    H'12'                                                    00513500
ZERO     DC    F'0'                                                     00513600
F100     DC    F'100'                                                   00513700
H1000    DC    H'1000'                                                  00513800
F127     DC    F'127'                                                   00513900
*                                                                       00514000
*                                                                       00514100
         PRINT NOGEN                                                    00514200
*                                                                       00514300
*        PROGRAM MESSAGES                                               00514400
*                                                                       00514500
OBT3ERRM MSG   ' VTOCCHEK - ERROR IN OBTAIN FOR FORMAT 3 DSCB '         00514600
KEYERR   MSG   ' VTOCCHEK - LIM, AND, OR OR SUBPARM ERROR - XXXXXX '    00514700
CCHHLEN  MSG   ' VTOCCHEK - CCHH SHOULD BE 4 OR 8 HEX CHARACTERS '      00514800
OPERERR  MSG   ' VTOCCHEK - OPERATOR WAS NOT EQ, NE, LT, LE, GT, OR GE' 00514900
OPERERR2 MSG   '          - WILL ASSUME EQ'                             00515000
*                                                                       00515100
         EJECT                                                          00515200
*                                                                       00515300
*                                                                       00515400
*        P A R S E   C O N T R O L   L I S T                            00515500
*                                                                       00515600
*                                                                       00515700
         PRINT OFF                                                      00515800
         COPY  VTOCPARS                                                 00515900
         PRINT ON                                                       00516000
*                                                                       00516100
*        DYNAMIC WORK AREA                                              00516200
*                                                                       00516300
         SPACE 3                                                        00516400
CHEKWORK DSECT                                                          00516500
         DS    18A            PRINT ROUTINE SAVE AREA                   00516600
CHARS    DS    CL16           CONVERSION TO CHARACTERS                  00516700
CAMSEEK  CAMLST SEEK,*,*,*                                              00516800
CAMLEN   EQU   *-CAMSEEK                                                00516900
         DS    0D                                                       00517000
CYLH     DS    F                                                        00517100
         DS    X              PAD FOR CCHH                              00517200
EQ       EQU   1              EQUATES FOR OPERATOR VALUES               00517300
NE       EQU   2                                                        00517400
LT       EQU   3                                                        00517500
LE       EQU   4                                                        00517600
GT       EQU   5                                                        00517700
GE       EQU   6                                                        00517800
HWORK    DS    H                                                        00517900
LIMVAL   DS    F                                                        00518000
NUMBERL  DS    F                                                        00518100
NUMLENL  DS    H                                                        00518200
FLAGNML  DS    X                                                        00518300
NUMKEYL  DS    X                                                        00518400
NUMBER1  DS    F                                                        00518500
NUMLEN1  DS    H                                                        00518600
FLAGNM1  DS    X                                                        00518700
NUMKEY1  DS    X                                                        00518800
NUMBER2  DS    F                                                        00518900
NUMLEN2  DS    H                                                        00519000
FLAGNM2  DS    X                                                        00519100
NUMKEY2  DS    X                                                        00519200
NUMBER3  DS    F                                                        00519300
NUMLEN3  DS    H                                                        00519400
FLAGNM3  DS    X                                                        00519500
NUMKEY3  DS    X                                                        00519600
REFKEY   DS    F                                                        00519700
REFOPER  DS    F                                                        00519800
REFVAL   DS    A                                                        00519900
REFNUM   DS    A                                                        00520000
PDLNSAVE DS    8A             REGISTER SAVE AREA FOR PDLNUM RTN         00520100
PDLMINUS DC    X'00'                                                    00520200
CHEKDBLW DS    D                                                        00520300
CCHHCOMP DS    X                                                        00520400
CCHHHIGH EQU   X'80'                                                    00520500
CCHHEQ   EQU   X'40'                                                    00520600
CCHHLOW  EQU   X'20'                                                    00520700
CCHHX    EQU   X'08'                                                    00520800
         DS    0D                                                       00520900
LENWORK  EQU   *-CHEKWORK                                               00521000
*                                                                       00521100
*        VTOC COMMAND COMMON AREA                                       00521200
*                                                                       00521300
         PRINT NOGEN                                                    00521400
         VTOCOM                                                         00521500
         SPACE 3                                                        00521600
*                                                                       00521700
*        FORMATTED DSCB                                                 00521800
*                                                                       00521900
         VTFMT                                                          00522000
         SPACE 3                                                        00522100
         PDEDSNAM                                                       00522200
         SPACE 3                                                        00522300
         SPACE 3                                                        00522400
DSCB1    DSECT                                                          00522500
         IECSDSL1 1                                                     00522600
         END                                                            00522700
/*                                                                      00522800
//*------------------------------------------------------ ASM: VTOCCHEK 00522900
//*                                                                     00523000
//ASM3.SYSIN DD *                                                       00523100
         TITLE 'VTOCEXCP- VTOC READING SUBROUTINE'                      00523200
*********************************************************************** 00523300
*        SPACE                                                          00523400
* AUTHOR;  R. F. MORSE, MIT INSTRUMENTATION LABORATORY  AUG 5,1968.     00523500
* MODIFIED;   E.BANK, FIREMAN'S FUND  MAY 15,1975.                      00523600
* MODIFIED;   R.MILLER  FIREMAN'S FUND  MAR 20,1977.                    00523700
* FUNCTION; THIS SUBROUTINE READS THE VOLUME TABLE OF CONTENTS (VTOC)   00523800
*        FROM A DIRECT-ACCESS DEVICE AND PRESENTS IT TO THE CALLER      00523900
*        ONE RECORD (DSCB) AT A TIME.                                   00524000
*                                                                       00524100
* OPERATION; THIS ROUTINE IS A SPECIALIZED SEQUENTIAL ACCESS METHOD     00524200
*        FOR VTOC'S.  ITS ADVANTAGE OVER ORDINARY BSAM IS THAT IT READS 00524300
*        AN ENTIRE TRACK IN ONE REVOLUTION, THUS SAVING CONSIDERABLE    00524400
*        TIME.  THE ROUTINE HAS THREE CALL MODES;                       00524500
*                                                                       00524600
*        0 - READ.  RETURNS WITH THE CORE ADDRESS OF A DSCB IN THE 3RD  00524700
*              PARAMETER.  THE CORE CONSISTS OF 148 CONSECUTIVE BYTES,  00524800
*              CONTAINING THE COUNT (8 BYTES), KEY (44 BYTES), AND DATA 00524900
*              (96 BYTES) FOR ONE DSCB.  RETURN CODES (REGISTER 15)     00525000
*              ARE;                                                     00525100
*                      0 - NORMAL;                                      00525200
*                      4 - END OF FILE, NO DATA PRESENTED;              00525300
*                      8 - PERMANENT I/O ERROR.  THE KEY AND DATA AREAS 00525400
*                          WILL BE SET TO ZEROS; THE COUNT AREA WILL    00525500
*                          CONTAIN THE CORRECT CCHHR.  SINCE READING    00525600
*                          IS DONE A TRACK AT A TIME, ALL THE DSCB'S    00525700
*                          FOR THAT TRACK WILL BE MARKED IN ERROR.      00525800
*                          READING MAY CONTINUE ON TO THE NEXT TRACK.   00525900
*                                                                       00526000
*        1 - OPEN.  THE SECOND PARAMETER SHOULD POINT TO                00526100
*              A  6-BYTE FIELD CONTAINING THE VOLSER TO BE USED FOR THE 00526200
*              ALLOCATION.                                              00526300
*              RETURN CODES ( REG 15 )  ARE DIRECT FROM DYNAMIC ALLOC.  00526400
*                      0 - NORMAL;                                      00526500
*                      4 - UNABLE TO OPEN (PROBABLY MISSING DD CARD);   00526600
*                      8 - DD CARD DID NOT REFER TO A DIRECT-ACCESS     00526700
*                          DEVICE, OR DEVICE TYPE UNKNOWN.              00526800
*                                                                       00526900
*        2 - CLOSE.  NO ARGUMENTS ARE REQUIRED OR RETURNED.  RETURN     00527000
*              CODE ( REG 15 ) IS FROM DYNAMIC UNALLOCATION.            00527100
         SPACE                                                          00527200
* ENTRY POINTS:  ENTRY IS ALWAYS TO 'VTOCEXCP'.                         00527300
*        ARGUMENTS ARE:                                                 00527400
*                      1 - A(FULL-WORD BINARY ENTRY TYPE);              00527500
*                      2 - A(PTR FOR DSCB);                             00527600
*                      3 - A(VOLSER).                                   00527700
* DATA SETS:  READS VOLUME TABLE OF CONTENTS FROM ANY DIRECT-ACCESS     00527800
*        DEVICE.  USES EXCP TO EXECUTE A CHAINED CHANNEL PROGRAM TO     00527900
*        READ AN ENTIRE TRACK AT A TIME.                                00528000
*                                                                       00528100
* EXTERNAL ROUTINES:  USES SUPERVISOR ROUTINE 'IECPCNVT' TO CONVERT     00528200
*        A RELATIVE TRACK NUMBER TO AN ABSOLUTE ADDRESS.                00528300
         SPACE                                                          00528400
* EXITS - NORMAL;  RETURNS TO CALLER VIA R14 WITH RETURN                00528500
*        CODE IN REGISTER 15.       (SEE ABOVE FOR RETURN CODE VALUES.) 00528600
*                                                                       00528700
* TABLES AND WORK AREAS;  USES AN AREA PROVIDED BY THE CALLER FOR       00528800
*        ITS SAVEAREA AND FOR WORKING STORAGE IMMEDIATELY FOLLOWING     00528900
*        THE PRIOR SAVEAREA.  IT USES GETMAIN TO OBTAIN AN AREA FOR     00529000
*        THE DSCB'S TO BE READ INTO.  THIS COULD BE AS LARGE AS         00529100
*        8K FOR 3350'S.  IT IS FREED BY THE FINAL CALL.                 00529200
*                                                                       00529300
* ATTRIBUTES;  REENTRANT, REFRESHABLE.                                  00529400
         EJECT                                                          00529500
* ENTER HERE AND PERFORM STANDARD REGISTER SAVE AREA HOUSEKEEPING.      00529600
         SPACE                                                          00529700
VTOCEXCP ENTER 12,8                    USE THE PROVIDED SAVEAREA        00529800
         USING VTOCWORK,R13   SET ADDRESSABILITY FOR WORK AREA          00529900
         LR    R11,R1                  SAVE PARAMETER REGISTER          00530000
         USING VTOCOM,R11              SET ADDRESSABILITY               00530100
*                                                                       00530200
*        POINT TO THE DCB FOR LATER REFERENCES                          00530300
*                                                                       00530400
         LA    RDCB,VTOCDCB   POINT TO IT                               00530500
         USING IHADCB,RDCB    SET ADDRESSABILITY                        00530600
         SPACE                                                          00530700
* SELECT MODE FROM CONTENTS AT ADDRESS IN REGISTER 1.                   00530800
         SPACE                                                          00530900
         SR    RWA,RWA                 CLEAR THE REGISTER               00531000
         IC    RWA,VTCEFUNC            GET CALL MODE                    00531100
         SLL   RWA,2                   MODE TIMES 4                     00531200
         B     *+4(RWA)                BRANCH ON MODE                   00531300
         SPACE                                                          00531400
         B     GETDSB                  MODE 0, GET A DSCB               00531500
         B     OPEN                    MODE 1, OPEN A NEW VTOC          00531600
         B     CLOSE                   MODE 2, CLOSE                    00531700
         B     RETURN0                 MODE 3 NOT DEFINED, NO OP        00531800
         SPACE 3                                                        00531900
***********                                                             00532000
* RETURNS *                                                             00532100
***********                                                             00532200
         SPACE                                                          00532300
RETURN0  SR    R15,R15                 CLEAR THE RETURN CODE            00532400
RETURN   LEAVE EQ                      EXIT WITH THE CURRENT RET CODE   00532500
         EJECT                                                          00532600
*********************                                                   00532700
* MODE 0 - GET DSCB *                                                   00532800
*********************                                                   00532900
         SPACE                                                          00533000
* IF END-OF-FILE WAS REACHED, RETURN AT ONCE.                           00533100
         SPACE                                                          00533200
GETDSB   LA    R15,4                   SET THE RETURN CODE, IN CASE     00533300
         TM    MODESW,EOFSW            TEST END-OF-FILE BIT             00533400
         BO    RETURN                  RETURN CODE 4 IF ON              00533500
         SPACE                                                          00533600
* IF CHANNEL PROGRAM HAS BEEN STARTED, GO TO CHECK IT.  OTHERWISE,      00533700
* ASSUME THERE IS AT LEAST ONE FULL BUFFER.                             00533800
         SPACE                                                          00533900
         TM    MODESW,XCPRUN           TEST IF EXCP ISSUED              00534000
         BO    XCPTEST                 BRANCH IF SO                     00534100
         SPACE                                                          00534200
* SET BUFFER ADDRESS TO NEXT DSCB AND TEST IF LAST ON TRACK.  IF NOT,   00534300
* EXIT WITH ITS ADDRESS IN R1.                                          00534400
         SPACE                                                          00534500
         L     RWA,DSCBADR             LOAD BUFFER POINTER              00534600
         LA    RWA,148(RWA)            ADVANCE TO NEXT DSCB             00534700
NDXSTORE ST    RWA,DSCBADR             STORE UPDATED POINTER            00534800
         C     RWA,DSCBLIM             TEST IF LAST DSCB IN BUFFER      00534900
         BNL   LASTDSCB                BRANCH IF SO                     00535000
         LR    R1,RWA                  PASS ADDRESS TO USER             00535100
GETOUT   ST    R1,DSCBADDR             STORE IT FOR THE CALLER          00535200
         TM    MODESW,RDERR            TEST IF ERROR ON THIS TRACK      00535300
         BZ    RETURN0                 RETURN CODE 0 IF NOT             00535400
         LA    R15,8                   SET THE RETURN CODE              00535500
         B     RETURN                  RETURN CODE 8 IF ERROR           00535600
         SPACE                                                          00535700
* IF THIS IS THE LAST DSCB, MOVE IT TO THE INTERNAL BUFFER AND START    00535800
* READING THE NEXT TRACK.                                               00535900
         SPACE                                                          00536000
LASTDSCB MVC   BUFF(148),0(RWA)        MOVE LAST DSCB                   00536100
         L     RWB,TTRN                LOAD RELATIVE TRACK NUMBER       00536200
         AL    RWB,=X'00010000'        INCREMENT TO NEXT TRACK          00536300
         ST    RWB,TTRN                                                 00536400
         BAL   RRET,EXCP               START CHANNEL PROGRAM            00536500
         LA    R1,BUFF                 LOAD DSCB ADDRESS FOR CALLER     00536600
         B     GETOUT                  TO RETURN                        00536700
         EJECT                                                          00536800
* WAIT FOR CHANNEL PROGRAM COMPLETION AND TEST THE OUTCOME.             00536900
         SPACE                                                          00537000
XCPTEST  WAIT  ECB=VTOCECB                                              00537100
         SPACE                                                          00537200
         NI    MODESW,X'FF'-XCPRUN     TURN EXCP STARTED BIT OFF        00537300
         CLI   VTOCECB,X'7F'           TEST COMPLETION CODE             00537400
         BNE   PERMERR                 BRANCH IF ERROR                  00537500
SETDSCBA L     RWA,DSCBSTRT            SET BUFFER POINTER TO 1ST DSCB   00537600
         B     NDXSTORE                                                 00537700
         SPACE                                                          00537800
* PERMANENT ERROR FOR THIS TRACK.  ZERO THE DSCB'S AND FILL IN THE      00537900
* CCHHR PORTIONS OF THE COUNT AREAS.                                    00538000
         SPACE                                                          00538100
PERMERR  OI    MODESW,RDERR            SIGNAL READ ERROR                00538200
         NI    IOBFLAG1,X'FB'          TURN OFF BIT 5 OF IOB FLAG       00538300
         NI    DCBIFLGS,X'3F'          TURN OFF BITS 0 AND 1            00538400
         L     RWA,DSCBSTRT            LOAD ADDRESS OF FIRST DSCB       00538500
         LA    RWB,1                   LOAD RECORD NUMBER               00538600
         SPACE                                                          00538700
DSCBELUP XC    0(148,RWA),0(RWA)       ZERO DSCB BUFFER                 00538800
         MVC   0(4,RWA),IOBSEEK+3      INSERT CCHH IN COUNT FIELD       00538900
         STC   RWB,4(RWA)              INSERT R IN COUNT FIELD          00539000
         LA    RWA,148(RWA)            POINT TO NEXT BUFFER             00539100
         LA    RWB,1(RWB)              INCREMENT RECORD NUMBER          00539200
         C     RWA,DSCBLIM             TEST FOR LAST BUFFER             00539300
         BNH   DSCBELUP                                                 00539400
         VTOCMSG TRACKERR       ISSUE THE ERROR MESSAGE                 00539500
         B     SETDSCBA                BRANCH TO RESET BUFFER POINTER   00539600
         EJECT                                                          00539700
*****************                                                       00539800
* MODE 1 - OPEN *                                                       00539900
*****************                                                       00540000
         SPACE                                                          00540100
* ENTER WITH A DDNAME IN SECOND PARAMETER POSITION.  PERFORM CLOSE      00540200
* SUBROUTINE FIRST TO BE SURE EVERYTHING IS INITIALIZED.                00540300
         SPACE                                                          00540400
OPEN     DS    0H                                                       00540500
         BAL   RRET,CLOSESUB           CALL CLOSE SUBROUTINE            00540600
         SPACE                                                          00540700
*                                                                       00540800
*        INITIALIZE THE DATA AREAS                                      00540900
*                                                                       00541000
*        FIRST THE DCB                                                  00541100
         MVC   VTOCDCB(DCBLEN),VTOCDCBM  SET UP THE DCB                 00541200
*                                                                       00541300
*        SET UP THE JFCB LISTS                                          00541400
*                                                                       00541500
         LA    R1,JEXLST      POINT TO THE EXIT LIST                    00541600
         STCM  R1,B'0111',DCBEXLSA  PUT IT INTO THE DCB                 00541700
         LA    R1,JFCBAREA    POINT TO THE JFCB AREA                    00541800
         ST    R1,JEXLST      AND PUT THAT INTO THE EXIT LIST           00541900
         MVI   JEXLST,X'87'   END OF LIST, JFCB EXIT                    00542000
         MVI   OPENLIST,X'80' END OF THE OPEN LIST TOO                  00542100
*        INITIALIZE THE IOB                                             00542200
         MVC   VTOCIOB(IOBCONL),IOBCONST START IT OUT                   00542300
         LA    R1,VTOCECB     GET THE ECB ADDRESS                       00542400
         ST    R1,IOBECB      AND STORE IT INTO THE IOB                 00542500
         ST    RDCB,IOBDCB    STORE THE DCB ADDRESS INTO THE IOB        00542600
*        INITIALIZE THE CAMLST                                          00542700
         MVC   DSCBFMT4(4),DSCBCON SET UP THE FIRST WORD                00542800
         LA    R1,IOBSEEK+3   SEEK ADDRESS                              00542900
         ST    R1,DSCBFMT4+4  INTO THE CAMLST                           00543000
         LA    R1,VOLID       VOLUME SERIAL NUMBER                      00543100
         ST    R1,DSCBFMT4+8  INTO THE CAMLST                           00543200
         LA    R1,FMT4        DSCB AREA                                 00543300
         ST    R1,DSCBFMT4+12 INTO THE CAMLST                           00543400
*                                                                       00543500
*        ALLOCATE THE VTOC OF THE CHOSEN PACK                           00543600
*                                                                       00543700
         LA    R1,ADDR        POINT TO THE UNIT ADDRESS                 00543800
         ST    R1,UNITADDR    SAVE THE ADDRESS                          00543900
         LA    R1,3           ALSO GET THE LENGTH                       00544000
         STH   R1,UNITLEN     AND SAVE IT FOR DYNAMIC ALLOCATION MACRO  00544100
         LA    R1,VOLID       POINT TO THE VOLUME SERIAL                00544200
         ST    R1,VOLADDR     SAVE THE ADDRESS                          00544300
         LA    R1,6           ALSO GET THE LENGTH                       00544400
         STH   R1,VOLLEN      AND SAVE IT FOR DYNAMIC ALLOCATION MACRO  00544500
         ALLOC DSN=VTOCNM,VOL=VOLADDR,UNIT=UNITADDR,DISP=SHR,          X00544600
               DDNTO=DCBDDNAM,ERROR=S99FAIL                             00544700
         OI    MODESW,ALLOCSW          SET ALLOCATE FLAG ON             00544800
         SPACE                                                          00544900
* OPEN THE VTOC.                                                        00545000
         SPACE                                                          00545100
*                                                                       00545200
*        FIRST READ THE JFCB TO SWITCH THE DSNAME TO HEX 04'S           00545300
*                                                                       00545400
         RDJFCB ((RDCB)),MF=(E,OPENLIST)  READ THE JFCB                 00545500
         LTR   R15,R15        TEST THE RETURN CODE                      00545600
         BNZ   ERRJFCB        BAD NEWS                                  00545700
         LA    R1,JFCBAREA    POINT TO THE JFCB                         00545800
         USING JFCB,R1        SET UP ADDRESSABILITY                     00545900
         MVI   JFCBDSNM,X'04' PUT IN THE FIRST ONE                      00546000
         MVC   JFCBDSNM+1(L'JFCBDSNM-1),JFCBDSNM  PROPAGATE IT          00546100
         OI    JFCBTSDM,JFCNWRIT  DON'T REWRITE IT                      00546200
         DROP  R1                                                       00546300
         OPEN  ((RDCB),(INPUT)),MF=(E,OPENLIST),TYPE=J  OPEN THE VTOC   00546400
         TM    DCBOFLGS,OPENBIT        TEST IF OPEN WORKED              00546500
         BZ    OPENERR                 ERROR IF OPEN FAILED             00546600
         SPACE                                                          00546700
* ISSUE AN OBTAIN FOR THE FIRST DSCB ON THE VTOC ( FORMAT 4 )           00546800
D3       STM   R2,R13,EXCPSAVE         SAVE OUR REGS                    00546900
         LA    R3,EXCPSAVE    POINT TO THE REGISTER SAVE AREA           00547000
         ICM   R0,B'1111',=X'00000100' FIRST DSCB                       00547100
         L     R1,DCBDEBAD             DEB ADDRESS                      00547200
         LA    R2,IOBSEEK              SAVE ADDRESS OF CCHHR            00547300
         L     R15,CVT                 GET ADDRESS OF CVT               00547400
         L     R15,CVTPCNVT(R15)       GET ADDRESS OF CONVERT ROUTINE   00547500
         BALR  R14,R15                 GO TO CONVERT ROUTINE            00547600
         LM    R2,R13,0(R3)            GET MY REGS BACK                 00547700
         OBTAIN DSCBFMT4               GET FORMAT 4 DSCB                00547800
         LTR   R15,R15                 DID WE GET IT                    00547900
         BNZ   OBTERR                  NO - THEN ERROR, KEEP R15        00548000
         CLI   DS4IDFMT,X'F4'          MAKE SURE WE HAVE FORMAT 4       00548100
         BNE   NOTFMT4                 NO - THEN ERROR                  00548200
         IC    R15,DS4DEVDT            GET NUMBER OF DSCBS PER TRACK    00548300
         ST    R15,NDSCBS              SAVE THE NUMBER OF DSCBS         00548400
         OC    NDSCBS,NDSCBS           MAKE SURE NOT ZERO               00548500
         BZ    DSCBNUM0                YES - GO TELL CALLER             00548600
*                                                                       00548700
* OBTAIN CORE FOR CHANNEL PROGRAM AND DSCB BUFFERS.                     00548800
         SPACE                                                          00548900
         LA    R0,156                  CORE FOR ONE DSCB AND ITS CCW    00549000
         MH    R0,NDSCBS+2             TIMES NUMBER PER TRACK           00549100
         AH    R0,=H'15'               PLUS 1 CCW AND ROUNDING          00549200
         N     R0,=X'FFFFFFF8'         ROUND TO DOUBLE-WORD MULTIPLE    00549300
         ST    R0,CBSIZE               SAVE SIZE OF GOTTEN CORE         00549400
         GETMAIN  R,LV=(0)             GET TRACK BUFFERS                00549500
         ST    R1,CBADDR               SAVE ADDRESS OF GOTTEN CORE      00549600
         OI    MODESW,CBGOT            INDICATE CORE GOTTEN             00549700
         SPACE                                                          00549800
* GENERATE CHANNEL PROGRAM.  IT CONSISTS OF A 'READ R0' ORDER WITH      00549900
* THE SKIP FLAG ON, FOLLOWED BY A 'READ COUNT-KEY-AND-DATA' ORDER FOR   00550000
* EACH DSCB.                                                            00550100
         SPACE                                                          00550200
         L     RWA,NDSCBS              NUMBER OF DSCB'S                 00550300
         SLL   RWA,3                   TIMES   8                        00550400
         LA    RWA,8(RWA,R1)           PLUS 8 AND BASE = 1ST BUFFER ADD 00550500
         ST    RWA,DSCBSTRT            SAVE ADDRESS OF FIRST BUFFER     00550600
         SPACE                                                          00550700
         ST    R1,IOBSTART             ADDRESS OF CHANNEL PROGRAM       00550800
         MVC   0(8,R1),INITCCW         INSERT FIRST CCW                 00550900
         LA    RWB,8(R1)               PLACE FOR NEXT CCW               00551000
         LA    RWC,1                   BUFFER COUNTER                   00551100
         SPACE                                                          00551200
CCWLOOP  MVC   0(8,RWB),READCCW        INSERT READ CCW FOR ONE DSCB     00551300
         ST    RWA,0(RWB)              SET ITS BUFFER ADDRESS           00551400
         MVI   0(RWB),READCKD          RESTORE COMMAND CODE             00551500
         C     RWC,NDSCBS              TEST BUFFER COUNTER              00551600
         BNL   LASTCCW                 BRANCH IF LAST BUFFER            00551700
         LA    RWB,8(RWB)              INCREMENT CCW ADDRESS            00551800
         LA    RWA,148(RWA)            INCREMENT BUFFER ADDRESS         00551900
         LA    RWC,1(RWC)              INCREMENT BUFFER COUNTER         00552000
         B     CCWLOOP                 DO NEXT BUFFER                   00552100
         SPACE                                                          00552200
LASTCCW  NI    4(RWB),X'FF'-CC         TURN OFF COMMAND CHAIN BIT       00552300
         ST    RWA,DSCBLIM             SAVE ADDRESS OF LAST DSCB BUFFER 00552400
         SPACE                                                          00552500
* SET OTHER THINGS AND START PROGRAM TO FILL BUFFER.                    00552600
         SPACE                                                          00552700
         SR    R0,R0                                                    00552800
         ST    R0,TTRN                 SET RELATIVE TRACK NUMBER TO 0   00552900
         NI    MODESW,X'FF'-XCPRUN-RDERR-EOFSW   SET FLAGS OFF          00553000
         BAL   RRET,EXCP               START CHANNEL PROGRAM            00553100
         B     RETURN0                 INDICATE SUCCESSFUL OPEN         00553200
         EJECT                                                          00553300
******************                                                      00553400
* MODE 2 - CLOSE *                                                      00553500
******************                                                      00553600
         SPACE                                                          00553700
CLOSE    BAL   RRET,CLOSESUB           CALL CLOSED CLOSE SUBROUTINE     00553800
         B     RETURN0                                                  00553900
         SPACE 2                                                        00554000
* IF THE CHANNEL PROGRAM IS RUNNING, WAIT FOR IT BEFORE TAKING FURTHER  00554100
* ACTION.                                                               00554200
         SPACE                                                          00554300
CLOSESUB DS    0H                                                       00554400
         TM    MODESW,XCPRUN           TEST IF CHANNEL PROGRAM RUNNING  00554500
         BZ    NOEXCP                  BRANCH IF NOT                    00554600
         WAIT  ECB=VTOCECB             WAIT UNTIL COMPLETE              00554700
         NI    MODESW,X'FF'-XCPRUN     TURN RUNNING SWITCH OFF          00554800
NOEXCP   DS    0H                                                       00554900
         SPACE                                                          00555000
* CLOSE THE DCB.                                                        00555100
         SPACE                                                          00555200
         TM    DCBOFLGS,OPENBIT        TEST IF DCB OPEN                 00555300
         BZ    NOCLOSE                 BRANCH IF NOT                    00555400
         CLOSE ((RDCB)),MF=(E,OPENLIST)   CLOSE THE VTOC                00555500
NOCLOSE  DS    0H                                                       00555600
         SPACE                                                          00555700
* FREE UP THE DDNAME AND VOLUME                                         00555800
         SPACE                                                          00555900
         TM    MODESW,ALLOCSW          DID WE ALLOCATE A DEVICE         00556000
         BNO   NOALLOC                 NO  - THEN NOTHING TO FREEUP     00556100
         LA    R1,DCBDDNAM   POINT TO THE DDNAME                        00556200
         ST    R1,DDNPDL     SAVE IT FOR FREE                           00556300
         LA    R1,8          GET THE DDNAME LENGTH                      00556400
         STH   R1,DDNPDL+4   SAVE IT FOR FREE                           00556500
         FREE  UNALC,DDN=DDNPDL,ERROR=S99FAIL  FREE THE DDNAME          00556600
         NI    MODESW,X'FF'-ALLOCSW    TURN OFF ALLOCATE SW             00556700
         SPACE                                                          00556800
NOALLOC  DS    0H                                                       00556900
         SPACE                                                          00557000
* RELEASE CORE OBTAINED FOR DSCB BUFFERS.                               00557100
         SPACE                                                          00557200
         TM    MODESW,CBGOT            TEST IF CORE GOTTEN              00557300
         BZ    NOFREE                  BRANCH IF NOT                    00557400
         LM    R0,R1,CBSIZE            LOAD SIZE AND LOCATION           00557500
         FREEMAIN  R,LV=(0),A=(1)      FREE CORE                        00557600
         NI    MODESW,X'FF'-CBGOT      SET CORE GOTTEN BIT OFF          00557700
NOFREE   DS    0H                                                       00557800
         SPACE                                                          00557900
         NI    MODESW,X'FF'-RDERR      CLEAR ERROR SWITCH               00558000
         BR    RRET                                                     00558100
         EJECT                                                          00558200
****************                                                        00558300
* EXCP ROUTINE *                                                        00558400
****************                                                        00558500
         SPACE                                                          00558600
* CONVERT RELATIVE TRACK ADDRESS IN 'TTRN' TO ABSOLUTE SEEK ADDRESS IN  00558700
* 'IOBSEEK', USING SUPERVISOR CONVERSION ROUTINE.                       00558800
         SPACE                                                          00558900
EXCP     DS    0H                                                       00559000
         STM   R2,R13,EXCPSAVE         SAVE IMPORTANT REGISTERS         00559100
         LA    R3,EXCPSAVE             SAVE REGS FOR RESTORING AFTER CL 00559200
         L     R0,TTRN                 LOAD RELATIVE TRACK NUMBER       00559300
         L     R1,DCBDEBAD             LOAD DEB ADDRESS                 00559400
         LA    R2,IOBSEEK              LOAD ADDR TO RECEIVE MBBCCHHR    00559500
         L     R15,CVT                 LOAD CVT ADDRESS                 00559600
         L     R15,CVTPCNVT(R15)       LOAD ADDR OF CONVERT ROUTINE     00559700
         BALR  R14,R15                 CONVERT TTRN TO MBBCCHHR         00559800
*                                      THAT CLOBBERED BASE REG          00559900
         LM    R2,R13,0(R3)            RESTORE REGISTERS                00560000
         LTR   R15,R15                 TEST IF EXTENT VIOLATED (RC=4)   00560100
         BNZ   SETEOF                  IF SO, MEANS END-OF-FILE         00560200
         CLC   DS4HPCHR,IOBSEEK+3      CHECK FOR THE LAST FMT1          00560300
         BL    SETEOF                  IF SO, PRETEND END-OF-FILE       00560400
         SPACE                                                          00560500
* ZERO ECB AND START CHANNEL PROGRAM.                                   00560600
         SPACE                                                          00560700
         SR    R0,R0                                                    00560800
         ST    R0,VTOCECB              CLEAR ECB                        00560900
         NI    MODESW,X'FF'-RDERR      RESET ERROR SWITCH               00561000
         EXCP  VTOCIOB                 START CHANNEL PROGRAM            00561100
         OI    MODESW,XCPRUN           SET 'RUNNING' FLAG               00561200
         BR    RRET                                                     00561300
         SPACE                                                          00561400
* WHEN EXTENT IS VIOLATED, SET END-FILE AND EXIT VIA CLOSE ROUTINE.     00561500
         SPACE                                                          00561600
SETEOF   OI    MODESW,EOFSW            SET END-OF-FILE BIT              00561700
         B     CLOSESUB                EXIT VIA CLOSE SUBROUTINE        00561800
         EJECT                                                          00561900
********************************                                        00562000
* DAIRFAIL ROUTINE             *                                        00562100
********************************                                        00562200
S99FAIL  LR    R15,RRCODE     SAVE THE RETURN CODE                      00562300
         S99FAIL MF=(E,S99FLIST,S99FLEN)  ISSUE THE APPROPRIATE MSG     00562400
         LR    R15,RRCODE     RELOAD THE RETURN CODE                    00562500
         B     RETURN         AND THEN EXIT                             00562600
         SPACE 3                                                        00562700
*                                                                       00562800
*        VARIOUS OTHER ERROR ROUTINES                                   00562900
*                                                                       00563000
OPENERR  VTOCMSG OPENERRM  ISSUE THE MESSAGE                            00563100
         B     ERRET          THEN RETURN                               00563200
OBTERR   VTOCMSG OBTERRM  ISSUE THE MESSAGE                             00563300
         B     ERRET          THEN RETURN                               00563400
NOTFMT4  VTOCMSG NOTFMT4M  ISSUE THE MESSAGE                            00563500
         B     ERRET          THEN RETURN                               00563600
DSCBNUM0 VTOCMSG DSCBNUMM  ISSUE THE MESSAGE                            00563700
         B     ERRET          THEN RETURN                               00563800
*                                                                       00563900
ERRJFCB  VTOCMSG ERRJFCBM   ERROR IN READING JFCB                       00564000
*                                                                       00564100
ERRET    LA    R15,8          SET AN ERROR RETURN CODE                  00564200
         B     RETURN         THEN EXIT                                 00564300
*                                                                       00564400
         EJECT                                                          00564500
********************************                                        00564600
* CONSTANTS, VARIABLES, ETC... *                                        00564700
********************************                                        00564800
         SPACE                                                          00564900
*        ERROR MESSAGES                                                 00565000
*                                                                       00565100
OPENERRM MSG   ' VTOCEXCP - ERROR IN OPENING VTOC '                     00565200
OBTERRM  MSG   ' VTOCEXCP - ERROR IN OBTAIN '                           00565300
NOTFMT4M MSG   ' VTOCEXCP - FORMAT 4 DSCB WAS NOT FIRST'                00565400
DSCBNUMM MSG   ' VTOCEXCP - THE FORMAT 4 DSCB HAS DSCB S/TRK = 0 '      00565500
TRACKERR MSG   ' VTOCEXCP - A READ ERROR OCCURRED ON THE VTOC '         00565600
ERRJFCBM MSG   ' VTOCEXCP - A RDJFCB ERROR OCCURRED '                   00565700
         SPACE                                                          00565800
INITCCW  CCW   READR0,0,CC+SLI+SKIP,8                                   00565900
READCCW  CCW   READCKD,0,CC,148                                         00566000
         SPACE                                                          00566100
DSCBCON  CAMLST SEEK,0,0,0   FILLED IN WITH IOBSEEK+3, VOLID, FMT4      00566200
         EJECT                                                          00566300
* DATA CONTROL BLOCK                                                    00566400
         PRINT   GEN                                                    00566500
VTOCDCBM DCB   DDNAME=VTOCDD,MACRF=(E),EXLST=1                          00566600
DCBLEN   EQU   *-VTOCDCBM                                               00566700
         SPACE                                                          00566800
* IOB FOR CHANNEL PROGRAM                                               00566900
         SPACE                                                          00567000
IOBCONST DS    0D                                                       00567100
         DC    X'42000000'     COMMAND CHAIN, NOT RELATED               00567200
         DC    A(0)            ECB ADDRESS                              00567300
         DC    2F'0'                                                    00567400
         DC    A(0)            CHANNEL PROGRAM BEGINNING                00567500
         DC    A(0)            DCB ADDRESS                              00567600
         DC    X'03000000'                                              00567700
         DC    F'0'                                                     00567800
         DC    D'0'            INITIAL SEEK ADDRESS                     00567900
IOBCONL  EQU   *-IOBCONST                                               00568000
* VTOC NAME FOR ALLOCATION                                              00568100
VTOCNM   DC    A(VTOCNAME)                                              00568200
         DC    Y(12)                                                    00568300
VTOCNAME DC    CL12'FORMAT4.DSCB'   DATA SET NAME FOR VTOC              00568400
*                                                                       00568500
*                                                                       00568600
         LTORG                                                          00568700
         EJECT                                                          00568800
* SECTION DEFINITION AND REGISTER ASSIGNMENTS;                          00568900
         SPACE 2                                                        00569000
RWA      EQU   2                                                        00569100
RWB      EQU   3                                                        00569200
RWC      EQU   4                                                        00569300
RDCB     EQU   8              DCB POINTER                               00569400
RRCODE   EQU   10              RETURN CODE REGISTER                     00569500
RRET     EQU   9               LOCAL SUBROUTINE EXIT REGISTER           00569600
         SPACE 3                                                        00569700
* TAGS FOR CHANNEL COMMANDS AND FLAG BITS:                              00569800
         SPACE                                                          00569900
READR0   EQU   X'16'           READ RECORD 0                            00570000
READCKD  EQU   X'1E'           READ COUNT, KEY, AND DATA                00570100
         SPACE                                                          00570200
CC       EQU   X'40'           COMMAND CHAIN FLAG                       00570300
SLI      EQU   X'20'           SUPPRESS LENGTH INDICATION FLAG          00570400
SKIP     EQU   X'10'           SKIP DATA TRANSFER FLAG                  00570500
         SPACE 3                                                        00570600
* COMMUNICATION VECTOR TABLE (CVT) DEFINITIONS:                         00570700
         SPACE                                                          00570800
CVT      EQU   16              LOCATION OF CVT BASE ADDRESS             00570900
CVTPCNVT EQU   28              OFFSET TO CONVERT ROUTINE ADDRESS        00571000
         EJECT                                                          00571100
*                                                                       00571200
*        AREA USED BY VTOCREAD, PASSED VIA R13                          00571300
*                                                                       00571400
VTOCWORK DSECT                                                          00571500
         DS    18F             SAVE AREA                                00571600
         SPACE                                                          00571700
EXCPSAVE DS    18F             INTERNAL SAVE AREA                       00571800
CBSIZE   DS    2F              SIZE AND LOCATION OF GOTTEN CORE         00571900
CBADDR   EQU   CBSIZE+4                                                 00572000
NDSCBS   DS    F               NUMBER OF DSCB'S PER TRACK               00572100
DSCBSTRT DS    F               ADDRESS OF 1ST DSCB BUFFER               00572200
DSCBLIM  DS    F               ADDRESS OF LAST DSCB BUFFER              00572300
DSCBADR  DS    F               ADDRESS OF CURRENT DSCB                  00572400
TTRN     DS    F               RELATIVE TRACK NUMBER                    00572500
VOLADDR  DS    A               FAKE PDL FOR ALLOC MACRO - ADDRESS       00572600
VOLLEN   DS    H                       AND LENGTH OF VOLID              00572700
UNITADDR DS    A               FAKE PDL FOR ALLOC MACRO - ADDRESS       00572800
UNITLEN  DS    H                       AND LENGTH OF UNIT ADDRESS       00572900
DDNPDL   DS    2F            SPACE FOR DDNAME PDL                       00573000
         SPACE                                                          00573100
* MODE SWITCH AND BIT DEFINITIONS                                       00573200
         SPACE                                                          00573300
MODESW   DC    X'00'                                                    00573400
CBGOT    EQU   X'80'           CORE GOTTEN FOR BUFFER                   00573500
XCPRUN   EQU   X'40'           CHANNEL PROGRAM STARTED BUT NOT CHECKED  00573600
RDERR    EQU   X'20'           PERMANENT I/O ERROR                      00573700
EOFSW    EQU   X'10'           END-OF-FILE SENSED                       00573800
ALLOCSW  EQU   X'08'           ALLOCATE VOLUME FLAG                     00573900
         SPACE                                                          00574000
VTOCDCB  DCB   DDNAME=VTOCDD,MACRF=(E),EXLST=1                          00574100
         SPACE                                                          00574200
OPENBIT  EQU   X'10'                                                    00574300
OPENLIST DS    2F                                                       00574400
         SPACE                                                          00574500
* IOB FOR CHANNEL PROGRAM                                               00574600
         SPACE                                                          00574700
VTOCIOB  DS    0D                                                       00574800
IOBFLAG1 DC    X'42000000'     COMMAND CHAIN, NOT RELATED               00574900
IOBECB   DC    A(VTOCECB)                                               00575000
         DC    2F'0'                                                    00575100
IOBSTART DC    A(0)            CHANNEL PROGRAM BEGINNING                00575200
IOBDCB   DC    A(VTOCDCB)                                               00575300
         DC    X'03000000'                                              00575400
         DC    F'0'                                                     00575500
IOBSEEK  DC    D'0'            INITIAL SEEK ADDRESS                     00575600
         SPACE                                                          00575700
* EVENT CONTROL BLOCK FOR CHANNEL PROGRAM:                              00575800
         SPACE                                                          00575900
VTOCECB  DC    F'0'            EVENT CONTROL BLOCK                      00576000
         SPACE 3                                                        00576100
* INTERNAL BUFFER FOR LAST DSCB                                         00576200
BUFF     DS    XL148                                                    00576300
         SPACE 2                                                        00576400
DSCBFMT4 CAMLST SEEK,IOBSEEK+3,VOLID,FMT4                               00576500
         SPACE                                                          00576600
*   WORK AREA FOR DYNAMIC ALLOCATION                                    00576700
         DYNSPACE                                                       00576800
S99FLIST DS    XL(S99FLEN)                                              00576900
         SPACE                                                          00577000
*                                                                       00577100
*        JFCB EXIT LIST AND AREA                                        00577200
*                                                                       00577300
JEXLST   DS    F                                                        00577400
JFCBAREA DS    XL176                                                    00577500
         DS    0D                                                       00577600
VTOCWLEN EQU   *-VTOCWORK                                               00577700
         SPACE 2                                                        00577800
         PRINT GEN                                                      00577900
         VTOCOM                                                         00578000
         PRINT NOGEN                                                    00578100
         SPACE 2                                                        00578200
         IEFZB4D0                                                       00578300
         SPACE 2                                                        00578400
         IEFZB4D2                                                       00578500
         SPACE 2                                                        00578600
         DCBD  DEVD=DA,DSORG=PS                                         00578700
         SPACE 2                                                        00578800
JFCB     DSECT                                                          00578900
         IEFJFCBN                                                       00579000
         END                                                            00579100
/*                                                                      00579200
//*------------------------------------------------------ ASM: VTOCEXCP 00579300
//*                                                                     00579400
//ASM4.SYSIN DD *                                                       00579500
         TITLE 'VTOC COMMAND FORMAT ROUTINE'                            00579600
*********************************************************************** 00579700
*                                                                     * 00579800
*                                                                     * 00579900
* TITLE -      VTOC COMMAND FORMAT ROUTINE                            * 00580000
*                                                                     * 00580100
* FUNCTION -   FORMAT THE DATA INTO THE VTFMT  DSECT FROM THE         * 00580200
*              FORMAT 1 ( AND 3 IF NEEDED ) DSCB.  THIS ROUTINE       * 00580300
*              ALSO GETS THE AREA TO CONTAIN THE FORMATTED            * 00580400
*              DSCB INFORMATION.                                      * 00580500
*                                                                     * 00580600
* OPERATION -  FIRST GET AN AREA FROM THE CURRENT BLOCK, OR GET       * 00580700
*              A BLOCK ( 32K ) OF STORAGE TO USE FOR THE FORMATTED    * 00580800
*              DSCB'S.  MOVE THE DATA OVER FROM THE FORMAT 1 DSCB.    * 00580900
*              THE SPACE CALCULATIONS MAY NEED THE FORMAT 3 DSCB.     * 00581000
*              CATALOG INFORMATION IS OBTIANED VIA LOCATE.  SOME      * 00581100
*              OF THE DSCB INFORMATION IS CONVERTED HERE.             * 00581200
*                                                                     * 00581300
* INPUT -      VTOC COMMON AREA ( VTOCOM )                            * 00581400
*              POINTED TO BY REGISTER 1                               * 00581500
*              USE PARSE DATA, CURRENT FORMATTED DSCB, LOCATE         * 00581600
*                                                                     * 00581700
* OUTPUT -     THE FORMATTED DSCB INFORMATION WITH ITS ADDRESS IN     * 00581800
*              FORMATAD.                                              * 00581900
*                                                                     * 00582000
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00582100
*                                                                     * 00582200
*                                                                     * 00582300
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00582400
*                                                                     * 00582500
*                                                                     * 00582600
*********************************************************************** 00582700
*                                                                       00582800
         EJECT                                                          00582900
         MACRO                                                          00583000
&LAB     DS1TST  &FIELD,&VALUE,&CODE                                    00583100
&LAB     TM    DS1&FIELD,X'&VALUE'  TEST IT                             00583200
         BNO   D&SYSNDX       IF NOT THERE, SKIP ALONG                  00583300
         MVC   VTF&FIELD,=CL3'&CODE'                                    00583400
D&SYSNDX DS    0H                                                       00583500
         MEND                                                           00583600
*                                                                       00583700
*                                                                       00583800
         EJECT                                                          00583900
VTOCFORM ENTER 12,16          DO THE HOUSEKEEPING                       00584000
         LR    R11,R1         SAVE ADDR OF VTOCOM                       00584100
         USING VTOCOM,R11     SET ITS ADDRESSABILITY                    00584200
         L     R9,ADDRANSR    POINT TO THE PARSE ANSWER                 00584300
         USING PDL,R9         SET ITS ADDRESSABILITY                    00584400
         USING FORMWORK,R13   SET ADDRESSABILITY FOR LOCAL WORK AREA    00584500
         SPACE 3                                                        00584600
*                                                                       00584700
*        CHECK FOR THE FIRST TIME THROUGH                               00584800
*        IF SO, PERFORM SOME INITIALIZATION                             00584900
*                                                                       00585000
         CLI   FIRSTFRM,0     IS THIS THE FIRST TIME?                   00585100
         BNE   GETAREA        NO, KEEP ON TRUCKIN'                      00585200
*                                                                       00585300
*        ROUTINE INITIALIZATION                                         00585400
*                                                                       00585500
         MVI   FIRSTFRM,255   NOTE THE INITIALIZATION AS DONE           00585600
         MVC   CAMLOC(CAMLEN),CAMCONST  SET UP THE CAMLST               00585700
*                                                                       00585800
*        FIND OR GET AN AREA FOR THE FORMATTED DSCB                     00585900
*              FIRST SEE HOW BIG IT IS                                  00586000
*                                                                       00586100
GETAREA  L     R7,DSCBADDR    POINT TO THE DSCB                         00586200
         LA    R7,8(R7)       GET PAST THE HEADER                       00586300
         USING DSCB1,R7       SET ADDRESSABILITY                        00586400
         LH    R1,DSNLEN     GET THE DSNAME LENGTH                      00586500
         LA    R4,VTFMTL(R1)  GET THE FORMATTED DSCB LENGTH             00586600
*                                                                       00586700
*        SEE IF THE CURRENT BLOCK CAN HANDLE IT                         00586800
*                                                                       00586900
FORMFIT  L     R3,VTCCURLN    GET THE CURRENT AVAILABLE                 00587000
         SR    R3,R4          SEE IF IT WILL FIT                        00587100
         BM    GOGETMN        NO, GET ANOTHER BLOCK                     00587200
*                                                                       00587300
*        NO SWEAT, GET THE SPACE FROM THIS BLOCK                        00587400
*                                                                       00587500
         ST    R3,VTCCURLN    STORE THE NEW ( REDUCED ) CURRENT LENGTH  00587600
         L     R3,VTCCURAD    POINT TO THE CURRENT ADDRESS              00587700
         LA    R5,0(R3,R4)    POINT TO THE END OF THE BLOCK             00587800
         ST    R5,VTCCURAD    AND PLACE THE NEW AVAILABLE ADDRESS       00587900
*                                                                       00588000
*        NOW FILL IN THE DATA IN THE FORMATTED DSCB                     00588100
*                                                                       00588200
         USING VTFMT,R3       SET FORMATTED DSCB ADDRESSABILITY         00588300
         ST    R3,FORMATAD    SAVE THIS BLOCK'S ADDRESS                 00588400
         XC    VTFNEXT,VTFNEXT  CLEAR THE SORT POINTER                  00588500
         MVC   VTFVOLUM,VOLID SAVE THE VOLUME SERIAL NUMBER             00588600
         LH    R1,DSNLEN     GET THE LENGTH OF THE DSNAME               00588700
         STH   R1,VTFDSNL     SAVE THE DSNAME LENGTH                    00588800
         BCTR  R1,0           SUBTRACT ONE FOR THE EX                   00588900
         EX    R1,MOVEDSN     MOVE IN THE DSNAME                        00589000
         MVC   VTFNOEPV,DS1NOEPV  NUMBER OF EXTENTS                     00589100
         MVC   VTFLRECL,DS1LRECL  LOGICAL RECORD LENGTH                 00589200
         MVC   VTFBLKSZ,DS1BLKL   BLOCK SIZE                            00589300
*                                                                       00589400
*     MOVE IN THE CREATION DATE, EXPIRATION DATE, AND LAST ACCESS DATE  00589500
*                                                                       00589600
         MVC   VTFCREDT,DS1CREDT  MOVE OVER CREATION DATE               00589700
         MVC   VTFEXPDT,DS1EXPDT  MOVE OVER EXPIRATION DATE             00589800
         MVC   VTFLSTAC,DS1REFD   MOVE OVER LAST ACCESS DATE            00589900
*                                                                       00590000
*        FORMAT THE RECORD FORMAT INTO CHARACTERS                       00590100
*                                                                       00590200
*                                                                       00590300
         MVC   VTFRECFM,BLANKS  BLANK THE FIELD TO START                00590400
         MVC   VTFACTON,BLANKS  ANOTHER BLANK FIELD                     00590500
         MVI   VTFDSTYP,C' ' AND STILL ANOTHER                          00590600
         LA    R2,VTFRECFM    POINT TO THE FIELD                        00590700
         TM    DS1RECFM,X'C0' UNKNOWN RECFM?                            00590800
         BZ    RECFM2         YES, TROUBLE                              00590900
         TM    DS1RECFM,X'40' IS IT FIXED?                              00591000
         BNZ   RECFM3         NO, KEEP TRYING                           00591100
         MVI   0(R2),C'F'     YES, SET UP THE FIRST CHAR                00591200
         LA    R2,1(R2)       AND BUMP THE POINTER                      00591300
         B     RECFM2         CHECK OTHER ATTRIBUTES                    00591400
RECFM3   TM    DS1RECFM,X'80' SEE IF IT'S V OR U                        00591500
         BZ    RECFM4         VARIABLE RECFM                            00591600
         MVI   0(R2),C'U'     RECFM = U                                 00591700
         B     RECFM4A        ADD TO THE POINTER AND KEEP LOOKING       00591800
RECFM4   MVI   0(R2),C'V'     VARIABLE                                  00591900
RECFM4A  LA    R2,1(R2)       GET PAST THIS CHAR                        00592000
RECFM2   DS    0H                                                       00592100
RECFM5   TM    DS1RECFM,X'10' IS IT BLOCKED?                            00592200
         BZ    RECFM6         NO, SKIP ON                               00592300
         MVI   0(R2),C'B'     YES, SET THE SYMBOL                       00592400
         LA    R2,1(R2)       GET PAST THE CHAR                         00592500
RECFM6   TM    DS1RECFM,X'08' IS IT SPANNED OR STANDARD?                00592600
         BZ    RECFM6A        NO                                        00592700
         MVI   0(R2),C'S'     YES, SET IT                               00592800
         LA    R2,1(R2)       GET PAST THIS CHARACTER                   00592900
RECFM6A  TM    DS1RECFM,X'20' CHECK TRACK OVERFLOW                      00593000
         BZ    RECFM7         NO DICE                                   00593100
         MVI   0(R2),C'T'     YES, SET IT                               00593200
         LA    R2,1(R2)       PUSH THE POINTER ON                       00593300
RECFM7   TM    DS1RECFM,X'04' IS IT ASA CONTROL                         00593400
         BZ    RECFM8         NO, SKIP ON                               00593500
         MVI   0(R2),C'A'     YES, SET IT                               00593600
         LA    R2,1(R2)       GET PAST THIS CHAR                        00593700
RECFM8   TM    DS1RECFM,X'02' HOW ABOUT MACHINE CARRIAGE CONTROL        00593800
         BZ    RECFM9         NO, SKIP ON                               00593900
         MVI   0(R2),C'M'     YES, SET IT                               00594000
RECFM9   DS    0H                                                       00594100
*                                                                       00594200
*        FORMAT THE DSORG                                               00594300
*                                                                       00594400
         MVC   VTFDSORG,=CL3'   '  CLEAR THE FIELD                      00594500
         DS1TST DSORG,80,IS   TRY ISAM                                  00594600
         DS1TST DSORG,40,PS   TRY SEQUENTIAL                            00594700
         DS1TST DSORG,20,DA   TRY DIRECT ACCESS                         00594800
         DS1TST DSORG,02,PO   TRY PARTITIONED                           00594900
         CLC   DS1DSORG(2),=X'0008'  IS IT VSAM?                        00595000
         BNE   DSORG05       NO, KEEP LOOKING                           00595100
         MVC   VTFDSORG,=CL3'VS ' YES, FLAG IT                          00595200
DSORG05  TM    DS1DSORG,X'01'      IS IT UNMOVEABLE?                    00595300
         BNO   DSORG06       NO, KEEP ON TRUCKIN'                       00595400
         MVI   VTFDSORG+2,C'U'     YES, NOTE IT                         00595500
DSORG06  DS    0H                                                       00595600
*                                                                       00595700
*        FORMAT THE SECONDARY ALLOCATION                                00595800
*                                                                       00595900
         SR    R1,R1          CLEAR A WORK REGISTER                     00596000
         IC    R1,DS1SCALO    GET THE ALLOCATION FLAG                   00596100
         SRL   R1,6           REMOVE THE BOTTOM 6 BITS ( 75 CENTS )     00596200
         IC    R2,SECAL(R1)   GET THE CHARACTER CODE                    00596300
         STC   R2,VTFSECAL    AND SAVE IT FOR LATER                     00596400
         MVC   VTFSECAM,DS1SCALO+2  SAVE THE SECONDARY AMOUNT TOO       00596500
         MVI   VTFROUND,C'N'  SET CODE FOR NO ROUND                     00596600
         TM    DS1SCALO,X'01' SEE IF ROUND WAS SET                      00596700
         BNO   PROTFORM       NO, THE CODE IS SET RIGHT                 00596800
         MVI   VTFROUND,C'R'  YES, RESET THE CODE                       00596900
*                                                                       00597000
*        FORMAT THE PASSWORD PROTECTION                                 00597100
*                                                                       00597200
PROTFORM TM    DS1DSIND,X'14' CHECK THE PASSWORD BITS                   00597300
         BO    PROTWRIT       WRITE PROTECT IS X'14'                    00597400
         BM    PROTREAD       READ PROTECT IS X'10'                     00597500
         MVI   VTFPROT,C'N'   NO PASSWORD PROTECTION                    00597600
         B     PROTEND        END OF PROTECTION FORMATTING              00597700
PROTWRIT MVI   VTFPROT,C'W'   SET CODE FOR WRITE PROTECT                00597800
         B     PROTEND        THEN CHECK OTHER ITEMS                    00597900
PROTREAD MVI   VTFPROT,C'R'   SET CODE FOR READ/WRITE PROTECT           00598000
PROTEND  DS    0H             END OF PROTECTION FORMATTING              00598100
*                                                                       00598200
*        FORMAT THE CATLG                                               00598300
*                                                                       00598400
         MVI   VTFCATLG,C' ' INITIALIZE IT TO BLANKS                    00598500
         CLI   CATK+1,0       SHOULD WE DO THE LOCATE?                  00598600
         BE    CATEND         NO, SKIP PAST IT                          00598700
*                                                                       00598800
*        SET UP THE CAMLST                                              00598900
*                                                                       00599000
         LA    R1,DS1DSNAM    POINT TO THE DSNAME                       00599100
         ST    R1,CAMLOC+4    SAVE IT IN THE CAMLST                     00599200
         LA    R1,LOCWORK     LOCATE WORKAREA                           00599300
         ST    R1,CAMLOC+12   SAVE IT IN THE CAMLST                     00599400
         LOCATE CAMLOC        CHECK THE CATALOG                         00599500
         LTR   R15,R15        TEST THE CATALOG RETURN CODE              00599600
         BZ    CATOK          ZERO, THERE IS AN ENTRY                   00599700
         MVI   VTFCATLG,C'N'  SET CODE FOR NOT CATALOGED                00599800
         CH    R15,H8         SEE IF THAT'S THE CASE                    00599900
         BE    CATEND         YES, LET IT STAND                         00600000
         MVI   VTFCATLG,C'E'  CATALOG ERROR, PROBLEMS                   00600100
*                                                                       00600200
*        CATALOG ENTRY IS THERE, SEE THAT THE VOLUME IS THIS ONE        00600300
*                                                                       00600400
CATOK    MVI   VTFCATLG,C'C'  SET UP AS A GOOD ENTRY                    00600500
         CLC   VOLID,LOCWORK+6  COMPARE THE VOLUME SERIAL NUMBERS       00600600
         BE    CATEND         GOOD, WE'RE DONE                          00600700
         MVI   VTFCATLG,C'W'  WRONG VOLUME, NOT CATALOGED               00600800
CATEND   DS    0H                                                       00600900
*                                                                       00601000
*        FORMAT THE ALLOCATION AND USED QUANTITIES                      00601100
*                                                                       00601200
         SPACE                                                          00601300
*        CHECK THROUGH THE EXTENTS                                      00601400
         SPACE                                                          00601500
         SR    R2,R2          CLEAR A WORK REGISTER                     00601600
         ICM   R2,B'0001',DS1NOEPV  GET THE NUMBER OF EXTENTS           00601700
         BZ    SPACEND        NO EXTENTS MEANS NO SPACE                 00601800
         SR    R4,R4          ZERO THE SPACE COUNTER FOR THE DATA SET   00601900
*                                                                       00602000
*        GET EACH EXTENT AND PROCESS IT                                 00602100
*                                                                       00602200
         SR    R6,R6          FIRST EXTENT                              00602300
EXTNEXT  LR    R5,R6          GET THE CURRENT EXTENT NUMBER             00602400
         SLL   R5,2           MULTIPLY IT BY FOUR                       00602500
         EX    R0,GETEXT(R5)  GET THE CORRECT ADDRESS                   00602600
*                                                                       00602700
*        PROCESS THIS EXTENT                                            00602800
*                                                                       00602900
         USING XTDSECT,R5     SET ADDRESSABILITY                        00603000
         CLI   XTFLAGS,XTNOEXT  IS THERE AN EXTENT                      00603100
         BE    NOEXT          NO, THE EXTENT ISN'T THERE                00603200
         CLI   XTFLAGS,XTCYLBD  IS IT ON CYLINDER BOUNDARIES            00603300
         BNE   FORMALOC       NO, DO IT FOR CYLS AND TRACKS             00603400
*                                                                       00603500
*        CYLINDER BOUNDS - BE SURE THE ALLOCATION IS CORRECT            00603600
*                                                                       00603700
         ICM   R1,B'0011',XTLOWHH GET THE LOWER TRACK                   00603800
         BZ    LOWOK          IT'S ZERO                                 00603900
         MVC   VTFACTON(6),=C'CYLERR'  NOTE THE ERROR                   00604000
         MVI   VTFACTON+6,C'L'  ON THE LOW CCHH                         00604100
LOWOK    LH    R1,XTHIHH      GET THE HIGH TRACK                        00604200
         LA    R1,1(R1)       ADD ONE FOR ZERO ADDRESSING               00604300
         CH    R1,DS4DEVSZ+2  IS THIS THE NUMBER OF TRACKS/CYL          00604400
         BE    FORMALOC       YES, GO CALCULATE                         00604500
         MVC   VTFACTON(6),=C'CYLERR'  NOTE THE ERROR                   00604600
         MVI   VTFACTON+7,C'H'  ON THE HIGH CCHH                        00604700
*                                                                       00604800
*        GET THE SPACE FOR NON-CYLINDER ALLOCATIONS                     00604900
*                                                                       00605000
FORMALOC LH    R1,XTHICC      GET THE HIGH CYLINDER                     00605100
         SH    R1,XTLOWCC     MINUS THE LOW CYLINDER                    00605200
         MH    R1,DS4DEVSZ+2  TIMES THE NUMBER OF TRACKS PER CYLINDER   00605300
         LH    R8,XTHIHH      GET THE HIGH TRACK                        00605400
         SH    R8,XTLOWHH     MINUS THE LOW TRACK                       00605500
         AR    R8,R1          TRACKS IN THIS EXTENT ( MINUS 1 )         00605600
         LA    R4,1(R4,R8)    ADD THE TRACKS TOGETHER FOR THIS DATA SET 00605700
*                                                                       00605800
*        GET THE NEXT EXTENT                                            00605900
*                                                                       00606000
NOEXT    LA    R6,1(R6)       INCREMENT THE EXTENT COUNTER              00606100
         CR    R6,R2          CHECK FOR THE END                         00606200
         BL    EXTNEXT        NOT YET, KEEP GOING                       00606300
*                                                                       00606400
*        ALL THE EXTENTS ARE SUMMED REGISTER 4 HAS THE SUM              00606500
*                                                                       00606600
         BAL   R8,SPACUNIT    CHANGE IT TO THE APPROPRIATE UNITS        00606700
         ST    R4,VTFALLOC    STORE IT FOR LATER                        00606800
SPACEND  DS    0H                                                       00606900
*                                                                       00607000
*        GET THE TRACKS USED                                            00607100
*                                                                       00607200
         SR    R4,R4          CLEAR THE TRACK ( WOO WOO )               00607300
         CLC   DS1LSTAR,ZEROES IS THE TRACK USED COUNTER SET?           00607400
         BNE   USEDOK         YES, ACCEPT IT                            00607500
*        NO, SEE IF THE ZERO IS VALID                                   00607600
         TM    DS1DSORG,X'40' IS IT SEQUENTIAL?                         00607700
         BO    USEDOK0        YES,THE ZERO IS VALID                     00607800
         CLC   DSORG(4),ZEROES  MAYBE IT WASN'T EVER OPENED             00607900
         BE    USEDOK0        THEN NO SPACE USED IS OK                  00608000
         TM    DS1DSORG,X'0C' CHECK FOR AN INVALID DSORG                00608100
         BO    USEDOK0        NO SPACE USED IS STILL OK                 00608200
         MVC   VTFUSED,FMIN1  SET A FLAG UNUSED SPACE UNKNOWN           00608300
         B     USEDEND        USED SPACE IS SET                         00608400
*                                                                       00608500
*        THE TRACKS USED COUNTER SEEMS OK                               00608600
*                                                                       00608700
USEDOK   LH    R4,DS1LSTAR    GET THE LAST TRACK USED                   00608800
         LA    R4,1(R4)       ADD ONE ( ZERO ADDRESSING )               00608900
         BAL   R8,SPACUNIT    CONVERT TO APPROPRIATE UNITS              00609000
USEDOK0  ST    R4,VTFUSED     SAVE THE AMOUNT OF SPACE USED             00609100
USEDEND  DS    0H                                                       00609200
         L     R14,VTFALLOC   ALLOCATED TRACKS                          00609300
         S     R14,VTFUSED    MINUS USED TRACKS                         00609400
         ST    R14,VTFUNUSD   EQUALS UNUSED TRACKS                      00609500
         SR    R14,R14                                                  00609600
         SR    R15,R15                                                  00609700
         CLC   VTFALLOC(4),=F'0'                                        00609800
         BE    USEDEND1                                                 00609900
         L     R15,VTFUSED    USED TRACKS                               00610000
         M     R14,=F'100'    MULT BY 100 TO GET PCT                    00610100
         D     R14,VTFALLOC   DIVIDE BY ALLOC TO GET PCT USED           00610200
USEDEND1 STH   R15,VTFPCT     SAVE PCT USED                             00610300
*                                                                       00610400
*        RETURN                                                         00610500
*                                                                       00610600
FORMRET  LEAVE EQ,RC=0                                                  00610700
*                                                                       00610800
*                                                                       00610900
         EJECT                                                          00611000
*                                                                       00611100
*        ROUTINES USED ABOVE                                            00611200
*                                                                       00611300
*                                                                       00611400
*        CONVERT FROM TRACKS TO THE APPROPRIATE UNITS                   00611500
*              KBYTES, MBYTES, TRKS, OR CYLS                            00611600
*                                                                       00611700
SPACUNIT LH    R1,SPACEK      GET THE UNIT TYPE                         00611800
         SLL   R1,2           MULTIPLY BY 4                             00611900
         B     *+4(R1)        THEN BRANCH TO THE CORRECT ROUTINE        00612000
         B     SPACTRK        R1=0  KILOBYTES                           00612100
         B     SPACTRK        R1=1  KILOBYTES                           00612200
         B     SPACTRK        R1=2  MEGABYTES                           00612300
         B     SPACTRK        R1=3  TRACKS                              00612400
         B     SPACTRK        R1=4  CYLINDERS                           00612500
*        TRACKS                                                         00612600
SPACTRK  BR    R8             WAS SET WHEN WE STARTED                   00612700
*        CYLINDERS                                                      00612800
SPACCYL  SR    R0,R0          CLEAR A REGISTER                          00612900
         LR    R1,R4          GET THE NUMBER OF TRACKS                  00613000
         LH    R4,DS4DEVSZ+2  GET THE NUMBER OF TRACKS PER CYLINDER     00613100
         SRL   R4,2           DIVIDE BY 2 FOR ROUNDING                  00613200
         AR    R1,R4          ADD IT IN                                 00613300
         LH    R4,DS4DEVSZ+2  GET THE NUMBER OF TRACKS PER CYLINDER     00613400
         DR    R0,R4          DIVIDE TO GET ROUNDED CYLINDERS           00613500
         LR    R4,R1          GET THE ANSWER BACK INTO R4               00613600
         BR    R8             THEN RETURN                               00613700
*        KILOBYTES                                                      00613800
SPACKB   MH    R4,DS4DEVTK    MULTIPLY BY BYTES PER TRACK               00613900
         SR    R0,R0          CLEAR THE TOP                             00614000
         LR    R1,R4          GET THE NUMBER TO DIVIDE                  00614100
         A     R1,F500        ADD UP TO ROUND                           00614200
         D     R0,F1000       DIVIDE TO GET KILOBYTES                   00614300
         LR    R4,R1          GET THE ANSWER BACK INTO R4               00614400
         BR    R8             THEN RETURN                               00614500
*        MEGABYTES                                                      00614600
SPACMB   MH    R4,DS4DEVTK    MULTIPLY BY BYTES PER TRACK               00614700
         SR    R0,R0          CLEAR THE TOP                             00614800
         LR    R1,R4          GET THE NUMBER TO DIVIDE                  00614900
         A     R1,F500000     ADD UP TO ROUND                           00615000
         D     R0,F1000000    DIVIDE TO GET MEGABYTES                   00615100
         LR    R4,R1          GET THE ANSWER BACK INTO R4               00615200
         BR    R8             THEN RETURN                               00615300
*                                                                       00615400
*        GET A NEW BLOCK OF MAIN STORAGE                                00615500
*                                                                       00615600
GOGETMN  GETMAIN R,LV=VTCGETMS  GET SOME                                00615700
         ST    R1,VTCCURAD    SET UP THE AVAILABLE ADDRESS              00615800
         LA    R2,VTCGETMS/1024   GET THE SIZE OF THE BLOCK IN K        00615900
         SLL   R2,10          GET IT INTO BYTES ( TIMES 1024 )          00616000
         ST    R2,VTCCURLN    SO THE FORMATTED DSCB'S CAN USE IT        00616100
*                                                                       00616200
*        SAVE THE BLOCK ADDRESS IN THE VTCGETMN TABLE                   00616300
*                                                                       00616400
         LA    R2,VTCGETMN    POINT TO THE TABLE                        00616500
         LA    R5,VTCGETMX    GET THE NUMBER OF ENTRIES IN THE TABLE    00616600
GOGETTAB ICM   R3,B'1111',0(R2) GET THIS ENTRY                          00616700
         BNZ   GOGETINC       IF NOT ZERO, KEEP LOOKING                 00616800
         ST    R1,0(R2)       SAVE THE NEW ENTRY                        00616900
         B     FORMFIT        THEN GO ALLOCATE A FORMATTED DSCB         00617000
*                                                                       00617100
*        THIS ENTRY WAS TAKEN, GET THE NEXT ONE                         00617200
*                                                                       00617300
GOGETINC LA    R2,4(R2)       POINT TO THE NEXT ENTRY                   00617400
         BCT   R5,GOGETTAB    COUNT AND LOOP                            00617500
*                                                                       00617600
*        TABLE OVERFLOW  - ISSUE ERROR MSG                              00617700
*              SET A FLAG TO STOP INPUT                                 00617800
*                                                                       00617900
         VTOCMSG TABOVFLW,TABOVSEC  ISSUE A MESSAGE                     00618000
         MVI   TABFULL,255    SET A STOP FLAG                           00618100
         B     FORMRET        RETURN FROM FORMATTING                    00618200
         EJECT                                                          00618300
*                                                                       00618400
*                                                                       00618500
*                                                                       00618600
*        PROGRAM CONSTANTS                                              00618700
*                                                                       00618800
         SPACE                                                          00618900
*        INSTRUCTIONS EXECUTED TO GET THE NEXT EXTENT                   00619000
GETEXT   LA    R5,DS1EXT1        1ST EXTENT                             00619100
         LA    R5,DS1EXT2        2ND EXTENT                             00619200
         LA    R5,DS1EXT3        3RD EXTENT                             00619300
         LA    R5,DS3EXTNT+00    4TH EXTENT                             00619400
         LA    R5,DS3EXTNT+10    5TH EXTENT                             00619500
         LA    R5,DS3EXTNT+20    6TH EXTENT                             00619600
         LA    R5,DS3EXTNT+30    7TH EXTENT                             00619700
         LA    R5,DS3ADEXT+00    8TH EXTENT                             00619800
         LA    R5,DS3ADEXT+10    9TH EXTENT                             00619900
         LA    R5,DS3ADEXT+20   10TH EXTENT                             00620000
         LA    R5,DS3ADEXT+30   11TH EXTENT                             00620100
         LA    R5,DS3ADEXT+40   12TH EXTENT                             00620200
         LA    R5,DS3ADEXT+50   13TH EXTENT                             00620300
         LA    R5,DS3ADEXT+60   14TH EXTENT                             00620400
         LA    R5,DS3ADEXT+70   15TH EXTENT                             00620500
         LA    R5,DS3ADEXT+80   16TH EXTENT                             00620600
MOVEDSN  MVC   VTFDSN(0),DS1DSNAM   EXECUTED COMPARE                    00620700
ZEROES   DC    2F'0'                                                    00620800
FMIN1    DC    F'-1'                                                    00620900
F500     DC    F'500'                                                   00621000
F1000    DC    F'1000'                                                  00621100
F500000  DC    F'500000'                                                00621200
F1000000 DC    F'1000000'                                               00621300
BLANKS   DC    CL8'                '                                    00621400
CAMCONST CAMLST NAME,*,,*                                               00621500
H8       DC    H'8'                                                     00621600
SECAL    DC    C'ABTC'        SECONDARY ALLOCATION CODES                00621700
*              ABSOLUTE TRK, BLOCKS, TRACKS, CYLINDERS                  00621800
*                                                                       00621900
*                                                                       00622000
*                                                                       00622100
         PRINT NOGEN                                                    00622200
*                                                                       00622300
*        PROGRAM MESSAGES                                               00622400
*                                                                       00622500
TABOVFLW MSG   ' THE VTOC TABLES (1.6 MEG) ARE NOT LARGE ENOUGH TO HANDX00622600
               LE THIS REQUEST'                                         00622700
TABOVSEC MSG   ' PARTIAL PROCESSING WILL CONTINUE '                     00622800
*                                                                       00622900
*                                                                       00623000
*                                                                       00623100
*                                                                       00623200
*                                                                       00623300
*                                                                       00623400
         EJECT                                                          00623500
*                                                                       00623600
*                                                                       00623700
*        P A R S E   C O N T R O L   L I S T                            00623800
*                                                                       00623900
*                                                                       00624000
         PRINT OFF                                                      00624100
         COPY  VTOCPARS                                                 00624200
         PRINT ON                                                       00624300
*                                                                       00624400
*        DYNAMIC WORK AREA                                              00624500
*                                                                       00624600
         SPACE 3                                                        00624700
FORMWORK DSECT                                                          00624800
         DS    18A            PRINT ROUTINE SAVE AREA                   00624900
FIRSTFRM DS    X              INITIALIZATION FOR THIS ROUTINE           00625000
CHARS    DS    CL16           CONVERSION TO CHARACTERS                  00625100
CAMLOC   CAMLST NAME,*,,*                                               00625200
CAMLEN   EQU   *-CAMLOC                                                 00625300
         DS    0D                                                       00625400
LOCWORK  DS    265C                                                     00625500
         SPACE                                                          00625600
         DS    0D                                                       00625700
LENWORK  EQU   *-FORMWORK                                               00625800
*                                                                       00625900
*        VTOC COMMAND COMMON AREA                                       00626000
*                                                                       00626100
         PRINT NOGEN                                                    00626200
         VTOCOM                                                         00626300
         SPACE 3                                                        00626400
*                                                                       00626500
*        FORMATTED DSCB                                                 00626600
*                                                                       00626700
         PRINT GEN                                                      00626800
         VTFMT                                                          00626900
         PRINT NOGEN                                                    00627000
         SPACE 3                                                        00627100
         PDEDSNAM                                                       00627200
         SPACE 3                                                        00627300
         SPACE 3                                                        00627400
DSCB1    DSECT                                                          00627500
         IECSDSL1 1                                                     00627600
         SPACE 3                                                        00627700
*        FORMAT 1 AND 3 EXTENT DESCRIPTION                              00627800
XTDSECT  DSECT                                                          00627900
XTFLAGS  DS    X                                                        00628000
XTNOEXT  EQU   X'00'          NO EXTENT                                 00628100
XTDATAB  EQU   X'01'          DAT BLOCKS                                00628200
XTOVFLW  EQU   X'02'          OVERFLOW AREA                             00628300
XTINDEX  EQU   X'04'          INDEX AREA                                00628400
XTUSRLAB EQU   X'40'          USER LABEL EXTENT                         00628500
XTSHRCYL EQU   X'80'          SHARING CYLINDERS                         00628600
XTCYLBD  EQU   X'81'          CYLINDER BOUNDARIES                       00628700
XTSEQ    DS    X              EXTENT SEQUENCE NUMBER                    00628800
XTLOWCC  DS    H              LOWER CYLINDER                            00628900
XTLOWHH  DS    H              LOWER TRACK                               00629000
XTHICC   DS    H              UPPER CYLINDER                            00629100
XTHIHH   DS    H              UPPER TRACK                               00629200
         END                                                            00629300
/*                                                                      00629400
//*------------------------------------------------------ ASM: VTOCFORM 00629500
//*                                                                     00629600
//ASM5.SYSIN DD *                                                       00629700
*                                                                       00629800
*   VTOC ERROR MESSAGE ROUTINE, R1 POINTS TO VTOC COMMON AT ENTRY       00629900
*                                                                       00630000
VTOCMSG  ENTER 12,0           DO THE STANDARD HOUSEKEEPING              00630100
         LR    R11,R1         GET THE PARM REGISTER                     00630200
         USING VTOCOM,R11     SET ADDRESSABILITY                        00630300
         SPACE                                                          00630400
         LM    R0,R1,MSGADDRS GET THE MESSAGE(S) TO SEND                00630500
         LTR   R0,R0          SECOND LEVEL MSG?                         00630600
         BZ    ERRORM1        NO                                        00630700
         SPACE                                                          00630800
         MVC   MSGTEXT1,0(R1) INSURE MSG IN WORK AREA                   00630900
         LA    R1,MSGTEXT1                                              00631000
         SPACE                                                          00631100
         LH    R14,0(R1)      LENGTH OF FIRST LEVEL MSG                 00631200
         LA    R15,0(R14,R1)  ADDR OF END OF MSG                        00631300
         LA    R14,1(R14)     JUMP MSG LENGTH                           00631400
         STH   R14,0(R1)                                                00631500
         MVI   0(R15),C'+'    INDICATE SECOND LEVEL MSG EXISTS          00631600
         SPACE 2                                                        00631700
         SR    R14,R14        CLEAR CHAIN FIELD                         00631800
         LA    R15,1          ONE SEGMENT IN 2ND MSG                    00631900
         STM   R14,R0,PUTOLD2 CREATE SECOND-LEVEL                       00632000
*                             OUTPUT LINE DESCRIPTOR ('OLD')            00632100
         LA    R0,PUTOLD2                                               00632200
         SPACE 3                                                        00632300
ERRORM1  LR    R14,R0         NEXT 'OLD' ADDR OR ZERO                   00632400
         LA    R15,1          ONE SEGMENT                               00632500
         LR    R0,R1          MSG ADDR                                  00632600
         STM   R14,R0,PUTOLD1 FIRST LEVEL 'OLD'                         00632700
         SPACE                                                          00632800
         LA    R1,PARMLIST                                              00632900
         USING IOPL,R1                                                  00633000
         SPACE                                                          00633100
         MVC   IOPLECT,ADDRECT                                          00633200
         MVC   IOPLUPT,ADDRUPT                                          00633300
         SPACE                                                          00633400
         LA    R0,ATTNECB                                               00633500
         ST    R0,IOPLECB                                               00633600
         MVI   ATTNECB,0                                                00633700
         SPACE 3                                                        00633800
         XC    PARMLIST+16(4),PARMLIST+16                               00633900
         PUTLINE PARM=PARMLIST+16,MF=(E,(1)),                          X00634000
               OUTPUT=(PUTOLD1,TERM,MULTLVL,INFOR)                      00634100
         SPACE 3                                                        00634200
         LEAVE EQ                                                       00634300
         SPACE 3                                                        00634400
         IKJIOPL                                                        00634500
         SPACE 3                                                        00634600
         VTOCOM                                                         00634700
         END                                                            00634800
/*                                                                      00634900
//*------------------------------------------------------ ASM: VTOCMSGX 00635000
//*                                                                     00635100
//ASM6.SYSIN DD *                                                       00635200
         TITLE 'VTOC COMMAND PRINT ROUTINE'                             00635300
*********************************************************************** 00635400
*                                                                     * 00635500
*                                                                     * 00635600
* TITLE -      VTOC COMMAND PRINT ROUTINE                             * 00635700
*                                                                     * 00635800
* FUNCTION -   PRINT THE DATA PASSED TO IT.  IT WILL USE THE DDNAME   * 00635900
*              SYSOUT IF IT IS ALLOCATED, AND WILL USE THE VTOCMSG    * 00636000
*              ROUTINE IF NOT.  IT CAN ALSO PUT OUT THE DATA TO       * 00636100
*              AN OUTPUT DATA SET.  TOTALS ARE COMPUTED HERE.         * 00636200
*                                                                     * 00636300
* OPERATION -  FOR UNSORTED DATA, GET THE CURRENT ENTRY, ADD TO THE   * 00636400
*              TOTALS, AND OUTPUT IT.  IF THE DATA IS SORTED, THE     * 00636500
*              ACTION IS MORE COMPLEX, BECAUSE ALL THE DATA SETS      * 00636600
*              ARE TO BE OUTPUT.  THE TOTALS AND BREAKS MAY BE        * 00636700
*              NEEDED AT ANY POINT.                                   * 00636800
*                                                                     * 00636900
* INPUT -      VTOC COMMON AREA ( VTOCOM )                            * 00637000
*              POINTED TO BY REGISTER 1                               * 00637100
*              USE PARSE DATA, CURRENT FORMATTED DSCB, SORTED LIST    * 00637200
*                                                                     * 00637300
* OUTPUT -     TO SYSOUT, A LIST OF THE REQUESTED DATA SETS AND       * 00637400
*              THEIR ATTRIBUTES, WITH TOTALS AND BREAKS AS NEEDED.    * 00637500
*              ALSO TO THE OUTPUT DATA SET, IF NEEDED.  IF SYSOUT     * 00637600
*              IS NOT ALLOCATED, VTOCMSG IS USED FOR OUTPUT.          * 00637700
*                                                                     * 00637800
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00637900
*                                                                     * 00638000
*                                                                     * 00638100
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00638200
*                                                                     * 00638300
*                                                                     * 00638400
*********************************************************************** 00638500
*                                                                       00638600
*        MACRO FOR DEFINING FAKE PDE FOR A DEFAULT LIST                 00638700
*                                                                       00638800
         EJECT                                                          00638900
VTOCPRNT ENTER 12,20          DO THE HOUSEKEEPING                       00639000
         LR    R11,R1         SAVE ADDR OF VTOCOM                       00639100
         USING VTOCOM,R11     SET ITS ADDRESSABILITY                    00639200
         L     R9,ADDRANSR    POINT TO THE PARSE ANSWER                 00639300
         USING PDL,R9         SET ITS ADDRESSABILITY                    00639400
         USING PRNTWORK,R13   SET ADDRESSABILITY FOR LOCAL WORK AREA    00639500
         SPACE 3                                                        00639600
*                                                                       00639700
*        CHECK FOR THE PRINT CLEAN - CLOSE AND FREEMAIN                 00639800
*                                                                       00639900
         CLI   VTCEPRNT,0     IS IT TIME                                00640000
         BNE   PRNTCLEN       YES, GO DO IT                             00640100
*                                                                       00640200
*        CHECK FOR THE FIRST TIME THROUGH                               00640300
*        IF SO, SET UP THE DCB'S AND OPEN THEM                          00640400
*                                                                       00640500
         CLI   FIRSTIM,0      IS THIS THE FIRST TIME?                   00640600
         BNE   CHKSORT        NO, KEEP ON TRUCKIN'                      00640700
         B     PRTINIT    INITIALIZE FOR PRINTING                       00640800
*                                                                       00640900
*        CHECK TO SEE IF THE DATA IS SORTED                             00641000
*                                                                       00641100
CHKSORT  CLI   SORTK+1,2      IS THIS NOSORT?                           00641200
         BNE   SORTED         NO, THE ENTRIES ARE SORTED                00641300
*                                                                       00641400
*        NOSORT WAS SPECIFIED.  ONLY THE CURRENT ENTRY IS AVAILABLE     00641500
*                                                                       00641600
         L     R3,FORMATAD    POINT TO THE ENTRY                        00641700
         USING VTFMT,R3       FORMATTED DSCB ADDRESSABILITY             00641800
         LTR   R3,R3          IS IT THERE?                              00641900
         BZ    CHEKTOT        NO, SEE IF A TOTAL HAS BEEN OUTPUT        00642000
         XC    FORMATAD,FORMATAD  CLEAR THE ADDRESS FOR LATER           00642100
*                                                                       00642200
*        ADD TO THE TOTALS                                              00642300
*                                                                       00642400
         BAL   R8,ADDTOT      GO DO IT                                  00642500
*                                                                       00642600
*        SEE IF THE OUTPUT DATA SET IS WANTED                           00642700
*                                                                       00642800
         TM    OUTDCB+48,X'10'  IS THE DCB OPEN?                        00642900
         BNO   NOOUTPT        NO, SKIP ALONG                            00643000
         BAL   R8,OUTPUT      YES, GO DO IT                             00643100
*                                                                       00643200
*        SEE IF THERE'S PRINTING TO DO                                  00643300
*                                                                       00643400
NOOUTPT  CLI   PRINTK+1,2     WAS NOPRINT SPECIFIED?                    00643500
         BE    VTRET          YES, WE'RE DONE FOR NOW                   00643600
         BAL   R8,PRINT       NO, PRINT OUT THE ENTRY                   00643700
*                                                                       00643800
*        RETURN FROM WHENCE WE CAME                                     00643900
*                                                                       00644000
VTRET    LEAVE EQ,RC=0        EXEUNT                                    00644100
*                                                                       00644200
*        NOSORT, CHECK FOR OUTPUTTING THE TOTALS                        00644300
*                                                                       00644400
CHEKTOT  TM    ENDTOTAL,ENTOTOUT  WAS THE TOTAL OUTPUT BEFORE?          00644500
         BO    VTRET          YES, JUST RETURN                          00644600
         BAL   R8,PRNTOT      NO, OUTPUT THE TOTAL                      00644700
         OI    ENDTOTAL,ENTOTOUT  REMEMBER THE TOTAL IS OUT             00644800
         B     VTRET          THEN RETURN                               00644900
         EJECT                                                          00645000
*                                                                       00645100
*        THE DATA IS SORTED, SO THE LISTS MUST BE EMPTIED.              00645200
*        CHECK EACH ITEM FOR A TOTAL AND FOR A BREAK                    00645300
*        OUTPUT TO PRINT AND/OR THE OUTPUT DATA SET                     00645400
*                                                                       00645500
*                                                                       00645600
*        FIRST GET THE TOTAL AND BREAK COUNTS FOR COMPARES              00645700
*                                                                       00645800
SORTED   LA    R1,TOTALN      POINT TO THE TOTAL PDL                    00645900
         BAL   R8,PDLNUM      CONVERT IT TO A NUMBER                    00646000
         LTR   R15,R15        IS IT ZERO?                               00646100
         BNZ   SETTNUM        NO, IT'S GOOD                             00646200
         OI    ENDTOTAL,ENDTONLY  YES, TOTALS AT THE END ONLY           00646300
SETTNUM  BCTR  R15,0          CUT IT DOWN ONE                           00646400
         STH   R15,NUMTOTAL   SET THE TOTAL COUNT                       00646500
         LA    R1,BREAK       POINT TO THE BREAK PDL                    00646600
         BAL   R8,PDLNUM      CONVERT IT TO A NUMBER                    00646700
         LTR   R15,R15        IS IT ZERO?                               00646800
         BNZ   SETBNUM        NO, IT'S GOOD                             00646900
         OI    ENDTOTAL,NOBREAK   YES, BREAK  AT THE END ONLY           00647000
SETBNUM  BCTR  R15,0          CUT IT DOWN ONE                           00647100
         STH   R15,NUMBREAK   SET THE BREAK COUNT                       00647200
*                                                                       00647300
*        START GOING THROUGH THE LISTS, PROCESS THE ENTRIES             00647400
*                                                                       00647500
         LA    R4,VTCSORTH    POINT TO THE LISTS                        00647600
         LA    R5,VTCSORTE    POINT TO THE END OF THE LISTS             00647700
         ST    R5,ADDREND     SAVE THE ADDRESS                          00647800
NEWLIST  L     R3,0(R4)       GET THE FIRST ENTRY FROM THIS LIST        00647900
         LTR   R3,R3          ANYTHING ON THIS LIST?                    00648000
         BZ    NEXTLIST       NO, GET ANOTHER LIST                      00648100
*                                                                       00648200
*        THIS IS AN ENTRY, DO THE TOTALS, PRINT, AND OUTPUT             00648300
*        CHECK FOR TOTALS AND BREAKS FIRST                              00648400
*                                                                       00648500
GOTENTRY CLI   PRINTK+1,2     WAS NOPRINT SET?                          00648600
         BE    CHKOUTPT       YES, SKIP INTERIM TOTALS                  00648700
         MVI   TOTLAST,0      SET UP FLAG FOR TOTAL AS LAST ACTION      00648800
*        NOTE - EXTENSION - OUTPUT INTERIM TOTALS WITH THE KEY          00648900
         TM    ENDTOTAL,ENDTONLY  END TOTAL ONLY?                       00649000
         BO    CHKBREAK       YES, SEE ABOUT BREAKS                     00649100
         LH    R2,NUMTOTAL    GET THE LENGTH TO COMPARE                 00649200
         L     R1,LASTKEY     GET THE LAST ENTRY                        00649300
         EX    R2,COMPKEY     SEE IF THIS IS THE SAME                   00649400
         BE    CHKBREAK       YES, KEEP COUNTING                        00649500
*                                                                       00649600
*        THIS ONE IS DIFFERENT, PRINT THE TOTALS FIRST                  00649700
*                                                                       00649800
         BAL   R8,PRNTOT      PRINT THE TOTALS                          00649900
*                                                                       00650000
*        CHECK FOR A BREAK                                              00650100
*                                                                       00650200
CHKBREAK TM    ENDTOTAL,NOBREAK   NO BREAKS THIS TIME?                  00650300
         BO    SKPBREAK       YES, SKIP PAST BREAKS                     00650400
         LH    R2,NUMBREAK    GET THE LENGTH TO COMPARE                 00650500
         L     R1,LASTKEY     GET THE LAST ENTRY                        00650600
         EX    R2,COMPKEY     SEE IF THIS IS THE SAME                   00650700
         BE    SKPBREAK       YES, KEEP COUNTING                        00650800
*                                                                       00650900
*        THIS ONE IS DIFFERENT, GET A NEW PAGE                          00651000
*                                                                       00651100
         MVC   LINECT,LINEMAX BE SURE THE NEXT ITEM GETS A NEW PAGE     00651200
         MVI   TOTLAST,0      DON'T SKIP A LINE AFTER TOTAL             00651300
         LA    R1,VTFDSN     POINT TO THE DSNAME                        00651400
         ST    R1,LASTKEY    SAVE THE ADDRESS FOR BREAK COMPARES        00651500
*                                                                       00651600
*        PRINT THE ITEM                                                 00651700
*                                                                       00651800
SKPBREAK CLI   TOTLAST,0      WAS A TOTAL NOT FOLLOWED BY A BREAK?      00651900
         BE    SKPBREA2       NO, CONTINUE NORMALLY                     00652000
         MVC   MSGBL,MSGBLC   YES, SET UP A BLANK MESSAGE               00652100
         LA    R1,MSGBL       POINT TO IT                               00652200
         BAL   R8,PRNTLINE    THEN PUTPUT IT                            00652300
SKPBREA2 BAL   R8,PRINT       FINAL FORMAT AND PRINT                    00652400
*                                                                       00652500
*        CHECK FOR DATA SET OUTPUT                                      00652600
*                                                                       00652700
CHKOUTPT TM    OUTDCB+48,X'10'     IS IT OPEN AND READY                 00652800
         BNO   GOTOT          NO, GO DO THE TOTALS                      00652900
         BAL   R8,OUTPUT      YES, PUT OUT THE DATA SET ENTRY           00653000
*                                                                       00653100
*        ADD UP THE TOTALS                                              00653200
*                                                                       00653300
GOTOT    BAL   R8,ADDTOT      SUM THEM                                  00653400
*                                                                       00653500
*        GET THE NEXT ENTRY                                             00653600
*                                                                       00653700
         ICM   R3,B'1111',VTFNEXT  FOLLOW THE CHAIN                     00653800
         BNZ   GOTENTRY       SOMETHING'S THERE, USE IT                 00653900
*                                                                       00654000
*        END OF THIS LIST, TRY THE NEXT LIST                            00654100
*                                                                       00654200
NEXTLIST LA    R4,12(0,R4)    MOVE OVER ONE                             00654300
         C     R4,ADDREND     WAS THAT THE LAST LIST?                   00654400
         BL    NEWLIST        NO, KEEP TRYING                           00654500
*                                                                       00654600
*        END OF THE LISTS, OUTPUT THE FINAL TOTAL AND RETURN            00654700
*                                                                       00654800
         BAL   R8,PRNTOT      LIST THE TOTAL                            00654900
*                                                                       00655000
*        CLEAR OUT THE PRINT LISTS                                      00655100
*                                                                       00655200
         XC    VTCSORTH+000(256),VTCSORTH  CLEAR 64 ENTRIES             00655300
         XC    VTCSORTH+256(256),VTCSORTH+256 CLEAR 64 ENTRIES          00655400
         XC    VTCSORTH+512(256),VTCSORTH+512 CLEAR 64 ENTRIES          00655500
         XC    VTCSORTH+768(256),VTCSORTH+768 CLEAR 64 ENTRIES          00655600
         B     VTRET          THEN GET OUT OF HERE                      00655700
         EJECT                                                          00655800
*                                                                       00655900
*        PRINT CLEANUP ROUTINE - CLOSE DCB'S FIRST                      00656000
*                                                                       00656100
PRNTCLEN TM    SYSOUT+48,X'10'  IS SYSOUT OPEN?                         00656200
         BNO   PRNTCLO        NO, CHECK THE OUTDCB                      00656300
         CLOSE (SYSOUT),MF=(E,OPENLIST)  DO THE CLOSE                   00656400
PRNTCLO  TM    OUTDCB+48,X'10'  IS OUTDCB OPEN?                         00656500
         BNO   PRNTFREE       NO, SKIP DOWN TO THE FREEMAINS            00656600
         CLOSE (OUTDCB),MF=(E,OPENLIST)  DO THE CLOSE                   00656700
*                                                                       00656800
*        FREE UP THE STORAGE                                            00656900
*                                                                       00657000
PRNTFREE LA    R2,VTCGETMN    POINT TO THE TABLE                        00657100
         LA    R5,VTCGETMX    GET THE NUMBER OF ENTRIES IN THE TABLE    00657200
PRNTFRL  ICM   R3,B'1111',0(R2)  GET THE STORAGE ADDRESS                00657300
         BZ    VTRET          IF ZERO, WE'RE DONE                       00657400
         FREEMAIN R,LV=VTCGETMS,A=(R3)  FREE IT                         00657500
         XC    0(4,R2),0(R2)  CLEAR THE ADDRESS                         00657600
         LA    R2,4(R2)       GET THE NEXT BLOCK ADDRESS                00657700
         BCT   R5,PRNTFRL     AND LOOP UNTIL DONE                       00657800
         B     VTRET          THEN RETURN                               00657900
*                                                                       00658000
         EJECT                                                          00658100
*                                                                       00658200
*        ROUTINES USED ABOVE                                            00658300
*              ADDTOT - ADD TO THE CURRENT TOTALS                       00658400
*              PRNTOT - PRINT OUT THE TOTALS AND CLEAR THEM             00658500
*              PRINT  - PRINT OUT AN ENTRY                              00658600
*              OUTPUT - OUTPUT THE DATA SET ENTRY                       00658700
*              PDLNUM - GET A NUMBER FROM A PDL ENTRY                   00658800
*              PRNTLINE - INTERNAL ROUTINE TO COUNT LINES, OUTPUT HEAD  00658900
*                                                                       00659000
ADDTOT   L     R1,TOTDS       NUMBER OF DATA SETS                       00659100
         LA    R1,1(R1)       ADD ONE                                   00659200
         ST    R1,TOTDS       STORE IT BACK                             00659300
         L     R1,TOTALLOC    TOTAL ALLOCATION                          00659400
         A     R1,VTFALLOC    ADD IN THIS DATA SET                      00659500
         ST    R1,TOTALLOC    STORE IT BACK                             00659600
         L     R1,TOTUSED     TOTAL USED SPACE                          00659700
         A     R1,VTFUSED     ADD IN THIS DATA SET                      00659800
         ST    R1,TOTUSED     STORE IT BACK                             00659900
         BR    R8             RETURN                                    00660000
*                                                                       00660100
         EJECT                                                          00660200
*                                                                       00660300
*        PRINT THE TOTALS                                               00660400
*              FIRST FORMAT THEM, THEN PUT THE LINE OUT TO SYSOUT       00660500
*              OR USE VTOCMSG TO LIST IT                                00660600
*                                                                       00660700
PRNTOT   ST    R8,PRNTTOT8   SAVE THE RETURN ADDRESS                    00660800
         MVC   MSGWORK(MSGTLEN),MSGTOTC  INIT THE MSG                   00660900
         CONV  MSGWORK+4+11,TOTDS,5  CONVERT NO OF DATA SETS            00661000
         CONV  MSGWORK+4+28,TOTALLOC,8  CONVERT ALLOCATION              00661100
         CONV  MSGWORK+4+51,TOTUSED,8   CONVERT USED SPACE              00661200
         MVC   MSGWORK+4+37(6),SPACTYPE MOVE IN THE UNITS               00661300
         MVC   MSGWORK+4+60(6),SPACTYPE MOVE IN THE UNITS               00661400
*                                                                       00661500
*        NOW OUTPUT THE MESSAGE                                         00661600
*                                                                       00661700
         MVI   MSGWORK+4,C'0' ADD A CARRIAGE CONTROL                    00661800
         LA    R1,MSGWORK     POINT TO THE TOTAL LINE                   00661900
         BAL   R8,PRNTLINE    PUT OUT THE TOTAL LINE                    00662000
         MVI   TOTLAST,1      NOTE THAT A TOTAL WAS THE LAST ITEM       00662100
         L     R8,PRNTTOT8    GET THE RETURN ADDRESS                    00662200
         BR    R8             THEN RETURN                               00662300
         EJECT                                                          00662400
*                                                                       00662500
*        PRINT OUT THE FORMATTED DSCB                                   00662600
*              FIRST FORMAT IT                                          00662700
*                                                                       00662800
PRINT    ST    R8,PRINTR8     SAVE REGISTER 8 FOR RETURNING             00662900
         MVI   WORKLINE+4,C' ' BLANK OUT THE LINE                       00663000
         MVC   WORKLINE+5(250),WORKLINE+4 SO INDIVIUAL FIELDS DON'T     00663100
         L     R2,VTPRNTLS   GET THE PRINT ITEM LIST                    00663200
         LA    R1,WORKLINE+4  POINT TO THE WORK  LINE                   00663300
PRTLOOP  SR    R6,R6          GET THE RESERVED WORD NUMBER              00663400
         IC    R6,0(R2)       FROM THE TOP BYTE                         00663500
         MH    R6,H12         MULTIPLY BY 12 FOR THE TABLE ENTRIES      00663600
         LA    R6,TABTITL(R6) THEN RELOCATE THE MESS                    00663700
         SR    R7,R7          CLEAR A REGISTER                          00663800
         IC    R7,0(R6)       GET THE EXECUTE LENGTH                    00663900
         LA    R5,1(R1)       SAVE A PLACE TO MOVE FIELD INTO           00664000
         LA    R1,1(R1,R7)    MOVE THE POINTER OVER                     00664100
         LA    R0,WORKLINE+4  POINT TO THE BEGINNING AGAIN              00664200
         SR    R1,R0          AND FIND THE CURRENT LENGTH               00664300
         CH    R1,LINELEN     IS IT TOO LONG?                           00664400
         BNL   PRTEND         YES, WE'RE DONE                           00664500
         AR    R1,R0          NO, KEEP GOING                            00664600
*                                                                       00664700
*        MOVE IN OR CONVERT THIS ITEM                                   00664800
*                                                                       00664900
         SR    R14,R14       CLEAR A REG FOR LENGTH OF RTN NO           00665000
         IC    R14,1(R6)     GET THE LENGTH OR ROUTINE                  00665100
         N     R14,F127      CLEAR THE TOP BIT                          00665200
         SR    R15,R15       CLEAR A REG FOR VTFMT                      00665300
         IC    R15,2(R6)       DISPLACEMENT                             00665400
         AR    R15,R3        RELOCATE IT                                00665500
*                                                                       00665600
*        DECIDE WHERE TO PUT IT                                         00665700
*                                                                       00665800
         TM    1(R6),X'80'   IS IT IN CHARS                             00665900
         BNO   PRTRTN        NO, USE THE SPECIAL ROUTINE                00666000
         SR    R7,R14        GET THE DIFFERENCE IN LENGTHS              00666100
         SRL   R7,1          GET HALF THE DIFFERENCE                    00666200
         AR    R5,R7         PUT THE DATA HERE                          00666300
         EX    R14,PRTMOVE   MOVE IN THE CHARACTERS                     00666400
PRTINC   LA    R2,1(R2)            GET THE NEXT CHAIN POINTER           00666500
         CLI   0(R2),0       ARE WE DONE?                               00666600
         BNE   PRTLOOP        GO GET MORE ITEMS                         00666700
PRTEND   LA    R1,WORKLINE   POINT TO THIS LINE                         00666800
         BAL   R8,PRNTLINE   THEN GO PRINT IT                           00666900
         L     R8,PRINTR8     GET THE RETURN ADDRESS                    00667000
         BR    R8             RETURN                                    00667100
PRTMOVE  MVC   0(0,R5),0(R15)  EXECUTED MOVE                            00667200
*                                                                       00667300
*        VARIOUS ROUTINES TO FORMAT BEFORE MOVING IN THE DATA           00667400
*                                                                       00667500
PRTRTN   LTR   R14,R14       BE SURE THE OFFSET IS OK                   00667600
         BP    PRTRTN1        IT SEEMS ALL RIGHT                        00667700
         ABEND 701,DUMP       CRASH AND BURN                            00667800
PRTRTN1  B     *(R14)        GO DO YOUR THING                           00667900
         B     PRDATES        4  CDATE, EXPDT                           00668000
         B     PRLSTUS        8  LSTUS                                  00668100
         B     PRALLOC       12  ALLOC, USED                            00668200
         B     PRUNUSED      16  ALLOC - USED                           00668300
         B     PRPCT         20  100 * USED / ALLOC                     00668400
         B     PREXT         24  EXT                                    00668500
         B     PRBLREC       28  BLKSZ LRECL                            00668600
         B     PRDSN         32  DSNAME                                 00668700
*                                                                       00668800
*                                                                       00668900
*        CDATE AND EXPDT                                                00669000
*                                                                       00669100
PRDATES  SR    R14,R14       CLEAR REG FOR YEAR                         00669200
         IC    R14,0(R15)    GET THE YEAR                               00669300
         USING DUMMD,R5      ALLOW CONV TO USE SYMBOL                   00669400
         CONV  DUMMA,(R14),2 CONVERT YEAR                               00669500
         ICM   R14,B'0011',1(R15)  GET THE DAY                          00669600
         CONV  DUMMA+2,(R14),3,EDMASK0,COMP0  CONVERT THE DAY           00669700
         B     PRTINC        GO GET MORE PRINT ITEMS                    00669800
*                                                                       00669900
*        LAST USE DATE                                                  00670000
*                                                                       00670100
PRLSTUS  MVC   CHARS,EDMASK   SET UP THE EDIT MASK                      00670200
         ED    CHARS(6),0(R15)  CONVERT TO CHARACTERS                   00670300
         MVC   0(5,R5),CHARS+1  THEN MOVE THEM IN                       00670400
         B     PRTINC        GO GET MORE PRINT ITEMS                    00670500
*                                                                       00670600
*        ALLOCATION AND USED                                            00670700
*                                                                       00670800
PRALLOC  ICM   R14,B'1111',0(R15)  GET THE AMOUNT                       00670900
         BM    PRUNKN6       IF NEGATIVE, IT'S NOT KNOWN                00671000
         CONV  DUMMA,(R14),6 CONVERT THE NUMBER                         00671100
         B     PRTINC        GO GET MORE PRINT ITEMS                    00671200
PRUNKN6  MVC   0(6,R5),BLANKS  UNKNOWN AMOUNT, LEAVE IT BLANK           00671300
         B     PRTINC        GO GET MORE PRINT ITEMS                    00671400
*                                                                       00671500
*        UNUSED SPACE                                                   00671600
*                                                                       00671700
PRUNUSED ICM   R14,B'1111',VTFUSED  GET THE   USED SPACE                00671800
         BM    PRUNKN6       IF NOT KNOWN, SKIP ALONG                   00671900
         L     R14,VTFALLOC  GET THE ALLOCATED SPACE                    00672000
         S     R14,VTFUSED   MINUS THE USED SPACE GIVES UNUSED          00672100
         BZ    PRTINC        IF NO UNUSED SPACE, LEAVE BLANK            00672200
         CONV  DUMMA,(R14),6 CONVERT FOR PRINTING                       00672300
         B     PRTINC        GO GET MORE PRINT ITEMS                    00672400
*                                                                       00672500
*        PCT - PERCENTAGE USED                                          00672600
*                                                                       00672700
PRPCT    SR    R6,R6         CLEAR THE TOP PART OF THE NUMBER           00672800
         ICM   R7,B'1111',VTFUSED   GET THE AMOUNT OF USED SPACE        00672900
         BM    PRUNKN3       IF NOT KNOWN, LEAVE BLANK                  00673000
         CLC   VTFALLOC,ZERO IS THE ALLOCATION ZERO?                    00673100
         BNE   PRPCTM        NO, DO THE STANDARD STUFF                  00673200
         CLC   VTFUSED,ZERO  IS THE USED SPACE ZERO?                    00673300
         BE    PRUNKN3       YES, JUST USE BLANKS                       00673400
PRPCTERR MVC   0(3,R5),=C'ERR'  NO, IT'S AN ERROR                       00673500
         B     PRTINC        THEN GO TRY FOR MORE                       00673600
PRPCTM   M     R6,F100       MULTIPLY BY 100 FOR PERCENT                00673700
         D     R6,VTFALLOC   DIVIDE BY ALLOC TO GET THE PERCENT         00673800
         LTR   R7,R7         IS IT LESS THAN ZERO?                      00673900
         BM    PRPCTERR      YES, FLAG THE ERROR                        00674000
         C     R7,F100       ALSO CHECK FOR OVER 100 PERCENT            00674100
         BH    PRPCTERR      THAT'S ALSO AN ERROR                       00674200
         CONV  DUMMA,(R7),3  CONVERT FOR PRINTING                       00674300
         B     PRTINC        GO GET MORE PRINT ITEMS                    00674400
PRUNKN3  MVC   0(3,R5),BLANKS BLANK THE UNKNOWN                         00674500
         B     PRTINC        GO GET MORE PRINT ITEMS                    00674600
*                                                                       00674700
*        EXTENTS                                                        00674800
*                                                                       00674900
PREXT    SR    R14,R14       CLEAR REG FOR YEAR                         00675000
         IC    R14,0(R15)    GET THE YEAR                               00675100
         CONV  DUMMA,(R14),2 CONVERT THE EXTENTS                        00675200
         B     PRTINC        GO GET MORE PRINT ITEMS                    00675300
*                                                                       00675400
*        LRECL AND BLKSZ                                                00675500
*                                                                       00675600
PRBLREC  LH    R14,0(R15)    GET THE DATA ( HALFWORD )                  00675700
         CONV  DUMMA,(R14),5  CONVERT THE DATA                          00675800
         B     PRTINC        GO GET MORE PRINT ITEMS                    00675900
*                                                                       00676000
*        DATA SET NAME                                                  00676100
*                                                                       00676200
PRDSN    LH    R14,VTFDSNL   GET THE DSNAME LENGTH                      00676300
         CH    R14,DSNLENGT  CHACK FOR MAX LENGTH                       00676400
         BL    PRDMOVE       THIS DSN IS SHORT ENOUGH                   00676500
         LH    R14,DSNLENGT  CUT IT DOWN                                00676600
PRDMOVE  BCTR  R14,0         COUNT DOWN ONE FOR THE EX                  00676700
         EX    R14,PRTMOVE   MOVE IN THE DSNAME                         00676800
         AH    R1,DSNLENOF   CORRECT THE LINE POINTER                   00676900
         B     PRTINC        GO GET MORE PRINT ITEMS                    00677000
         EJECT                                                          00677100
*                                                                       00677200
*        OUTPUT THE FORMATTED DSCB AS IT IS                             00677300
*                                                                       00677400
OUTPUT   CLEAR OUTWORK       CLEAR THE OUTPUT REC                       00677500
         LH    R2,VTFDSNL     GET THE LENGTH OF THE DSNAME              00677600
         LA    R2,VTFMTL-1(R2)  GET THE FULL LENGTH MINUS ONE           00677700
         EX    R2,OUTMOVE    MOVE IN THE ACTUAL RECORD                  00677800
         PUT   OUTDCB,OUTWORK    OUTPUT THE RECORD                      00677900
         BR    R8             RETURN                                    00678000
OUTMOVE  MVC   OUTWORK(0),0(R3)  EXECUTED MOVE                          00678100
         EJECT                                                          00678200
*                                                                       00678300
*        PDLNUM - CONVERT FROM CHARACTERS ( EBCDIC ) TO AN INTEGER      00678400
*              BINARY FORM, PASSED BACK VIA REGISTER 15                 00678500
*              A PARSE PDE IS THE INPUT AS SHOWN IN THE SAMPLE BELOW    00678600
*                       LA    R1,PDL     POINT TO THE PARSE DECRIPTION  00678700
*                       BAL   R8,PDLNUM  GO CONVERT TO NUMERICS         00678800
*              THE ROUTINE WILL TERMINATE IF IT FINDS NON-NUMERICS      00678900
*                 ANY CHARACTERS OTHER THEN 0-9, +, -                   00679000
*              REGISTERS 1, 2, 5, 6, AND 7 ARE USED                     00679100
*                                                                       00679200
PDLNUM   STM   R1,R8,PDLNSAVE SAVE THE REGISTERS                        00679300
         LH    R2,4(R1)       GET THE STRING ADDRESS                    00679400
         L     R1,0(R1)       GET THE STRING ADDRESS                    00679500
         MVI   PDLMINUS,0     CLEAR THE NEGATIVE NUMBER FLAG            00679600
         SR    R5,R5          CLEAR THE CHARACTER COUNTER               00679700
         SR    R15,R15        CLEAR THE ANSWER                          00679800
PDLLOOP  LA    R6,0(R5,R1)    POINT TO THIS DIGIT                       00679900
         LA    R5,1(R5)       GET TO THE NEXT DIGIT                     00680000
         CR    R5,R2          IS THIS THE END OF THE STRING?            00680100
         BH    PDLFINI        YES, EXIT                                 00680200
         SR    R7,R7          CLEAR A WORK REGISTER                     00680300
         IC    R7,0(R6)       GET THE CHARACTER                         00680400
         SH    R7,PDLH240     SUBTRACT THE CHARACTER C'0'               00680500
         BM    PDLSP          IF NEGATIVE, CHECK SPECIAL CHARACTERS     00680600
         MH    R15,PDLH10     IT'S A DIGIT, MULTIPLY PRIOR NUM BY TEN   00680700
         AR    R15,R7         ADD ON THE NEW DIGIT                      00680800
         B     PDLLOOP        AND LOOP FOR MORE                         00680900
*                                                                       00681000
*        CHECK FOR SPECIAL CHARACTERS                                   00681100
*                                                                       00681200
PDLSP    CLI   0(R6),C' '     IS IT A BLANK?                            00681300
         BE    PDLLOOP        THEN IT'S OK                              00681400
         CLI   0(R6),C'+'     IS IT A PLUS?                             00681500
         BE    PDLLOOP        THAT'S ALSO OK                            00681600
         CLI   0(R6),C'-'     IS IT A MINUS?                            00681700
         BNE   PDLFINI        NO, JUST QUIT                             00681800
         MVI   PDLMINUS,1     YES, NOTE IT                              00681900
         B     PDLLOOP        AND LOOK FOR MORE                         00682000
*                                                                       00682100
*        QUIT, AFTER SETTING R15 TO NEGATIVE IF NEEDED                  00682200
*                                                                       00682300
PDLFINI  CLI   PDLMINUS,1     WAS A MINUS SIGN FOUND?                   00682400
         BNE   PDLLEAVE       NO, EXIT                                  00682500
         LNR   R15,R15        YES, MAKE IT NEGATIVE                     00682600
PDLLEAVE LM    R1,R8,PDLNSAVE RESTORE THE REGISTERS                     00682700
         BR    R8             RETURN                                    00682800
PDLH10   DC    H'10'                                                    00682900
PDLH240  DC    H'240'                                                   00683000
         EJECT                                                          00683100
*                                                                       00683200
*        PRNTLINE - GET EACH LINE FOR SYSOUT, THEN COUNT THE LINES      00683300
*              OUTPUT HEADERS AND TITLE LINES AS NECESSARY              00683400
*                                                                       00683500
PRNTLINE STM   R1,R8,PRTLSAVE SAVE THE REGISTERS                        00683600
         TM    SYSOUT+48,X'10' IS SYSOUT OPEN FOR BUSINESS?             00683700
         BNO   PRNTTERM       NO, IT'S NOT OPEN, USE VTOCMSG            00683800
         CLI   HEADK+1,2     NOHEADING REQUEST?                         00683900
         BE    PRNTLIN       YES, JUST OUTPUT THE DATA LINE             00684000
*                                                                       00684100
*        DO THE LINE COUNTING                                           00684200
*                                                                       00684300
         LH    R2,LINECT      GET THE LINE COUNT                        00684400
         CLI   4(R1),C'1'     IS IT REQUESTING A NEW PAGE?              00684500
         BE    PRNTPAGE       YES, DO IT                                00684600
         CLI   0(R1),C'-'     SKIP 3 LINES                              00684700
         BE    PRNTSKP3       YES, TRY IT                               00684800
         CLI   0(R1),C'0'     SKIP 2 LINES?                             00684900
         BE    PRNTSKP2       YES, DO IT                                00685000
         B     PRNTSKP1       JUST SKIP ONE                             00685100
PRNTSKP3 LA    R2,1(R2)       ADD ONE TO THE LINE COUNT                 00685200
PRNTSKP2 LA    R2,1(R2)       ADD ONE TO THE LINE COUNT                 00685300
PRNTSKP1 LA    R2,1(R2)       ADD ONE TO THE LINE COUNT                 00685400
         STH   R2,LINECT      SAVE THE LINE COUNT                       00685500
         CH    R2,LINEMAX     DOES THIS OVERFLOW  HE PAGE?              00685600
         BL    PRNTLIN        NO, JUST PUT OUT THIS LINE                00685700
*                                                                       00685800
*        PRINT OUT THE PAGE HEADER AND ITEM TITLES                      00685900
*                                                                       00686000
PRNTPAGE LH    R6,PAGECT      GET THE PAGE COUNT                        00686100
         LA    R6,1(R6)       ADD ONE TO IT                             00686200
         STH   R6,PAGECT      THEN STORE IT BACK                        00686300
         L     R7,PAGEADDR    GET THE PLACE TO PUT THE PAGE             00686400
         USING DUMMD,R7       DUMMY DSECT                               00686500
         CONV  DUMMA,(R6),5   GET THE CHARACTERS                        00686600
         DROP  R7                                                       00686700
         ST    R1,PRNTLSAV    SAVE THE INPUT REG1                       00686800
         PUT   SYSOUT,PRNTHEAD  OUTPUT THE HEADER                       00686900
         PUT   SYSOUT,PRNTTITL  OUTPUT THE ITEM TITLES                  00687000
         L     R1,PRNTLSAV    GET THE ORIGINAL LINE                     00687100
         MVI   4(R1),C'0'     ALWAYS SKIP THE FIRST LINE AFTER NEW PAGE 00687200
         LA    R7,5           SET THE LINE COUNT                        00687300
         STH   R7,LINECT      SAVE IT FOR LATER                         00687400
PRNTLIN  CLEAR OUTWORK       CLEAR A PRINT RECORD                       00687500
         LH    R6,0(R1)      GET THE RECORD LENGTH                      00687600
         SH    R6,H5         MINUS 4 FOR PREFIX, 1 FOR EX               00687700
         LA    R3,4(R1)      POINT TO THE TEXT                          00687800
         EX    R6,OUTMOVE    THEN MOVE IN THE LINE                      00687900
         PUT   SYSOUT,OUTWORK      AND FINALLY PRINT IT                 00688000
         B     PRNTLRET       THEN RETURN                               00688100
*                                                                       00688200
*        SIMPLER CHECKING FOR VTOCMSG OUTPUT                            00688300
*                                                                       00688400
PRNTTERM LH    R2,LINECT      GET THE LINE COUNT                        00688500
         LA    R2,1(R2)       IGNORE CARRIAGE CONTROL                   00688600
         STH   R2,LINECT      SAVE IT BACK                              00688700
         LR    R6,R1          SAVE THE ORIGINAL REG 1                   00688800
         MVI   4(R1),C' '    BLANK THE CARRIAGE CONTROL                 00688900
         CLI   HEADK+1,2     NOHEADING REQUEST?                         00689000
         BE    PRNTTLIN       YES, JUST OUTPUT THE DATA LINE            00689100
         CH    R2,LINEMAX     NEED A NEW TITLE?                         00689200
         BL    PRNTTLIN       NO, KEEP GOING                            00689300
         VTOCMSG PRNTTITH     YES, PUT IT OUT                           00689400
         MVC   LINECT,H2      RESET THE LINE COUNT                      00689500
PRNTTLIN VTOCMSG 0(R6)        OUTPUT THE PASSED LINE                    00689600
PRNTLRET LM    R1,R8,PRTLSAVE RESTORE THE REGISTERS                     00689700
         BR    R8             THEN RETURN                               00689800
         EJECT                                                          00689900
PRTINIT  MVI   FIRSTIM,10    FLAG THE INITIALIZATION AS DONE            00690000
         LA    R1,TABTITL    POINT TO THE BUG TABLE                     00690100
         ST    R1,ATABTITL   LET CHECK KNOW WHERE IT IS                 00690200
*                                                                       00690300
*        SET UP THE CHARACTERS FOR TYPE OF SPACE ALLOC.                 00690400
*                                                                       00690500
         LH    R2,SPACEK      GET THE SPACE TYPE KEYWORD                00690600
         SLA   R2,3           MULTIPLY BY 8                             00690700
         LA    R2,TABSPACE(R2)  RELOCATE IT                             00690800
         MVC   SPACTYPE,0(R2) SAVE THE CHARACTERS                       00690900
         CLI   PRINTK+1,2     IS THIS NOPRINT?                          00691000
         BE    NOOPEN         YES, SKIP THE OPEN                        00691100
         MVC   SYSOUT(SYSOUTL),SYSOUTC  INITIALIZE THE DCB              00691200
         MVI   OPENLIST,X'80' TERMINATE THE LIST                        00691300
         LA    R1,JFCB       POINT TO THE JFCB                          00691400
         ST    R1,DCBEXIT    AND PUT THE ADDR IN THE DCB EXIT           00691500
         MVI   DCBEXIT,X'87' NOTE IT AS A JFCB EXIT                     00691600
         LA    R1,DCBEXIT    POINT TO THE EXIT LIST                     00691700
         LA    R2,SYSOUT     AND TO THE DCB FOR ADDRESSABILITY          00691800
         USING IHADCB,R2     TELL THE ASSEMBLER ABOUT IT                00691900
         STCM  R1,B'0111',DCBEXLSA  STUFF IT INTO THE DCB               00692000
         TM    CHARSPL+6,X'80'  CHARS PER LINE ENTERED?                 00692100
         BZ    RDJFCB        NO, CONTINUE ALONG                         00692200
         LA    R1,CHARSPL    YES, POINT TO THE PDL                      00692300
         BAL   R8,PDLNUM     CONVERT TO A NUMBER                        00692400
         STH   R15,DCBLRECL  SAVE THE NEW LRECL                         00692500
         STH   R15,LINELEN   ALSO THE LINE LENGTH                       00692600
         TM    BLKSZSET+6,X'80'  BLOCKSIZE ENTERED?                     00692700
         BZ    BLKEQREC      NO, BLOCKSIZE EQUALS LRECL                 00692800
         LA    R1,BLKSZSET      POINT TO THE PDL                        00692900
         BAL   R8,PDLNUM     GET THE NUMBER                             00693000
BLKEQREC STH   R15,DCBPRECL  STUFF IT AWAY                              00693100
         DROP  R2            FINISHED WITH THE DCB                      00693200
RDJFCB   RDJFCB ((R2)),MF=(E,OPENLIST)  SEE IF IT'S THERE               00693300
         LTR   R15,R15       WAS IT THERE?                              00693400
         BNZ   NOOPEN        NO, SKIP ALONG                             00693500
         OPEN  ((R2),OUTPUT),MF=(E,OPENLIST)  OPEN THE PRINT DCB        00693600
*                                                                       00693700
*        INITIALIZE PRINT VARIABLES                                     00693800
*                                                                       00693900
*        SET LINES/PAGE AND LINESIZE                                    00694000
*                                                                       00694100
NOOPEN   MVC   LINEMAX,DEFLMAX  SET THE DEFAULT NUMBER OF LINES/PAGE    00694200
         TM    LINESPP+6,X'80'     LINES PER PAGE ENTERED?              00694300
         BZ    DEFLINPP     NO, SKIP ON                                 00694400
         LA    R1,LINESPP    YES, POINT TO THE PDL                      00694500
         BAL   R8,PDLNUM     CONVERT TO A NUMBER                        00694600
         STH   R15,LINEMAX   AND SAVE IT                                00694700
DEFLINPP MVC   LINECT,LINEMAX SET UP TO PAGE ON THE FIRST WRITE         00694800
         TM    CHARSPL+6,X'80'     CHARS PER LINE ENTERED?              00694900
         BO    LENSET        YES, USE IT                                00695000
         MVC   LINELEN,DEFLEN SET UP A DEFAULT LENGTH                   00695100
         TM    SYSOUT+48,X'10' DO WE USE SYSOUT?                        00695200
         BO    LENSET         YES, USE WHAT WE'VE GOT                   00695300
*        GET THE TERMINAL LINE SIZE TO SEE IF IT MAKES SENSE            00695400
         GTSIZE                                                         00695500
         LTR   R1,R1          SEE IF IT'S GOOD                          00695600
         BZ    LENSET         NO, JUST A ZERO, KEEP THE DEFAULTS        00695700
         BCTR  R1,0           CUT IT DOWN ONE TO AVOID A MESS           00695800
         STH   R1,LINELEN     SAVE THIS LENGTH                          00695900
         LTR   R0,R0          FOR DISPLAYS, IT'S SCREEN SIZE            00696000
         BZ    LENSET         KEEP WHAT WE'VE GOT                       00696100
         TM    LINESPP+6,X'80'  WAS LINES PER PAGE ENTERED?             00696200
         BO    LENSET           YES, DON'T OVERRIDE IT                  00696300
         STH   R0,LINEMAX     AND SAVE THE NEW PAGE LOCATION            00696400
LENSET   DS    0H                                                       00696500
*                                                                       00696600
*        SET UP THE PAGE COUNTER                                        00696700
*                                                                       00696800
PAGEAD   LH    R1,LINELEN     GET THE LENGTH OF THE LINE                00696900
         SH    R1,H10         MINUS TEN CHARACTERS                      00697000
         LA    R1,PRNTHEAD(R1) THEN RELOCATE IT                         00697100
         CLEAR PRNTHEAD       CLEAR THE LINE FIRST                      00697200
         MVC   0(4,R1),CPAGE  MOVE IN THE CHARACTERS PAGE               00697300
         LA    R1,5(R1)       MOVE OVER 5 MORE                          00697400
         ST    R1,PAGEADDR    THIS IS THE PLACE                         00697500
*                                                                       00697600
*        BUILD THE PRINT HEADER LINE                                    00697700
*                                                                       00697800
         TM    HEADING+6,X'80' IS A USER HEADING PRESENT                00697900
         BO    USERHEAD       YES, USE IT                               00698000
         MVC   PRNTHEAD(L'DEFHEAD),DEFHEAD  NO, GET A DEFAULT           00698100
*                                                                       00698200
*        ADD THE COMMAND BUFFER TO THE HEADING                          00698300
*                                                                       00698400
         LH    R1,LINELEN    GET THE LINE LENGTHE                       00698500
         SH    R1,H32        MINUS SPACES FOR PAGE, START OF HDR        00698600
         BNP   OUTOPEN       IF IT'S SHORT, SKIP ON                     00698700
         L     R14,ADDRCBUF  POINT TO THE COMMAND BUFFER                00698800
         CH    R1,0(R14)     COMPARE LENGTHS                            00698900
         BL    CBUFBIG       THE COMMAND BUFFER TOO BIG                 00699000
         LH    R1,0(R14)     GET THE COMMAND BUFFER SIZE                00699100
CBUFBIG  SH    R1,H5         MINUS 1 FOR EX, 4 FOR CBUF PREFIX          00699200
         EX    R1,MOVEHED    MOVE THE CBUF TO THE DEFAULT HEADER        00699300
         B     OUTOPEN        GO SET UP THE PAGE COUNTER                00699400
USERHEAD LH    R1,HEADING+4   GET THE LENGTH                            00699500
         BCTR  R1,0           MINUS ONE FOR THE EX                      00699600
         L     R2,HEADING     POINT TO THE USER HEAD                    00699700
         EX    R1,MOVEHEAD    THEN MOVE IT IN                           00699800
OUTOPEN  DS    0H                                                       00699900
*                                                                       00700000
*        SET UP THE WORK LINE                                           00700100
*                                                                       00700200
         LH    R1,LINELEN    GET THE LINE LENGTH                        00700300
         LA    R1,4(R1)      ADD FOUR FOR THE PREFIX                    00700400
         STH   R1,WORKLINE   OUTPUT TEXT                                00700500
         STH   R1,PRNTTITH    ITEM TITLES                               00700600
*        GET THE PRINT SPECIFICATION                                    00700700
*                                                                       00700800
         LA    R1,DEFPRNT                                               00700900
         ST    R1,VTPRNTLS   SAVE THE PRINT ITEM LIST ADDRESS           00701000
         TM    SUBPRTKY+6,X'80'  WERE ANY ITEMS SET UP                  00701100
         BE    PRTITSET      NO, THE DEFAULT LIST IS OK                 00701200
*                                                                       00701300
*        GET THE ADD, REPLACE, DELETE, AND NEW ITEMS                    00701400
*        AND BUILD THE NEW LIST                                         00701500
*                                                                       00701600
*        FIRST CONVERT THE ENTERED TEXT INTO NUMERIC KEYS               00701700
*                                                                       00701800
         LA    R4,SUBPRTIT   POINT TO THE ITEMS                         00701900
         LA    R5,VTPRNTEN   POINT TO THE OUTPUT KEYS                   00702000
         LA    R0,VTPRNTEX   POINT TO THE END OF THE LIST               00702100
ENTKEY   BAL   R8,GETKEY     GET A KEY                                  00702200
         STC   R15,0(R5)     SAVE IT                                    00702300
         LA    R5,1(R5)      GET TO THE NEXT ONE                        00702400
         CR    R0,R5         CHECK FOR THE END                          00702500
         BNH   ENTKEND       IF THAT'S ALL                              00702600
         ICM   R4,7,9(R4)    GET THE CHAIN POINTER                      00702700
         BNZ   ENTKEY        AND KEEP GOING IF THERE'S MORE             00702800
ENTKEND  DS    0H            THE KEYS ARE ENTERED INTO THE LIST         00702900
*                                                                       00703000
*        MERGE THE ENTERED ITEMS AND THE DEFAULT LIST INTO A NEW LIST   00703100
*                                                                       00703200
         LA    R6,VTPRNTL    POINT TO THE NEW LIST                      00703300
         ST    R6,VTPRNTLS   SAVE IT'S ADDRESS                          00703400
         LA    R4,VTPRNTEN   POINT TO THE ENTERED ITEMS                 00703500
         LA    R2,DEFPRNT    POINT TO THE DEFAULT LIST                  00703600
         LR    R3,R2         POINT TO THE BEGINNING - NO DEFAULT IF NEW 00703700
         L     R1,SUBPRTKY   POINT TO THE KEYWORD                       00703800
         CLI   0(R1),C'N'    IS THIS A NEW LIST?                        00703900
         BE    PRTINEW       YES, SKIP PAST DEFAULT COPY                00704000
         LA    R3,DEFPRNTE   POINT TO THE END OF THE DEFAULT LIST       00704100
*                                                                       00704200
*        ADD, REPLACE, DELETE - COPY THE DEFAULT LIST                   00704300
*                                                                       00704400
PRTICOPY CR    R2,R3         IS THIS THE END OF THE DEFAULT LIST?       00704500
         BNL   PRTITSET      YES, END OF PROCESSING FOR PRINT ITEMS     00704600
*                                                                       00704700
*        CHECK FOR ENTERED ITEMS THAT ARE ALSO                          00704800
*              IN THE DEFAULT LIST.                                     00704900
*                                                                       00705000
PRTICDLP CLC   0(1,R2),0(R4) IS THIS THE SAME ITEM                      00705100
         BE    PRTIFND       YES, SEE WHAT TO DO                        00705200
         LA    R4,1(R4)      NO, GET TO THE NEXT ITEM                   00705300
         CLI   0(R4),0       WAS THIS THE LAST ENTERED ITEM?            00705400
         BNE   PRTICDLP      NO, KEEP LOOKING                           00705500
         LA    R4,VTPRNTEN   POINT BACK TO THE TOP OF THE LIST          00705600
*                                                                       00705700
*        ADD THIS ITEM TO THE NEW LIST                                  00705800
*                                                                       00705900
PRTICSKP MVC   0(1,R6),0(R2) MOVE IN THE NEW KEY                        00706000
         LA    R6,1(R6)      POINT PAST IT                              00706100
PRTICDEL LA    R2,1(R2)      GO DOWN THE DEFAULT LIST                   00706200
         B     PRTICOPY      THEN KEEP ON CHECKING                      00706300
*                                                                       00706400
*        AN ITEM WAS ENTERED AND WAS IN THE DEFAULT LIST                00706500
*        FOR DELETE, JUST DELETE ITEMS                                  00706600
*        FOR ADD AND REPLACE, DELETE ALL BUT THE FIRST ITEM             00706700
*              TO AVOID DUPLICATES                                      00706800
*              IF IT IS THE FIRST ITEM, INSERT THE ENTERED LIST         00706900
*                                                                       00707000
PRTIFND  LA    R0,VTPRNTEN   POINT TO THE FIRST ITEM                    00707100
         CR    R0,R4         COMPARE WITH THE ITEM FOUND                00707200
         BH    PRTICDEL      NOT THE FIRST ITEM, DELETE IT              00707300
         CLI   0(R1),C'D'    IS THIS DELETE TIME?                       00707400
         BE    PRTICDEL      THEN JUST DELETE IT                        00707500
         CLI   0(R1),C'R'    IS THIS A REPLACE?                         00707600
         BE    PRTIREPA      YES, IGNORE THIS DEFAULT ITEM              00707700
*                                                                       00707800
*        ADD THE DEFAULT ITEM FIRST                                     00707900
*                                                                       00708000
PRTIASKP MVC   0(1,R6),0(R2) MOVE IN THE NEW KEY                        00708100
         LA    R6,1(R6)      POINT PAST IT                              00708200
*                                                                       00708300
*        MOVE THE ITEMS IN FROM THE ADD OR REPLACE LIST                 00708400
*                                                                       00708500
PRTIREPA LA    R2,1(R2)      GET PAST THE DEFAULT LIST ITEM             00708600
PRTIREP  LA    R4,1(R4)      GET PAST THE FIRST ENTRY                   00708700
PRTINEXT CLI   0(R4),0       IS THIS THE LAST ITEM?                     00708800
         BE    PRTICOPY      YES, SEE ABOUT MORE DEFAULTS               00708900
*                                                                       00709000
*        ADD AN ENTERED ITEM TO THE LIST                                00709100
*                                                                       00709200
PRTINSKP MVC   0(1,R6),0(R4) MOVE IN THE NEW KEY                        00709300
         LA    R6,1(R6)      POINT PAST IT                              00709400
         B     PRTIREP       GO GET MORE ENTERED ITEMS                  00709500
*                                                                       00709600
*        NEW LIST, JUST USE IT AS ENTERED                               00709700
*                                                                       00709800
PRTINEW  LA    R1,VTPRNTEN   POINT TO THE ENTERED LIST                  00709900
         ST    R1,VTPRNTLS   THEN SAVE ITS ADDRESS FOR LATER            00710000
PRTITSET DS    0H                                                       00710100
*                                                                       00710200
*        BUILD THE TITLE LINE FOR THE DSNAME FIELDS                     00710300
*                                                                       00710400
         CLEAR PRNTTITL        BLANK OUT THE TITLE LINE                 00710500
         MVC   PRNTTITL+1(139),PRNTTITL  SO THE WHOLE THING IS GOOD     00710600
         L     R2,VTPRNTLS   GET THE PRINT ITEM LIST                    00710700
         LA    R1,PRNTTITL    POINT TO THE TITLE LINE                   00710800
TITLOOP  SR    R6,R6          GET THE RESERVED WORD NUMBER              00710900
         IC    R6,0(R2)       FROM THE TOP BYTE                         00711000
         MH    R6,H12         MULTIPLY BY 12 FOR THE TABLE ENTRIES      00711100
         LA    R6,TABTITL(R6) THEN RELOCATE THE MESS                    00711200
         SR    R7,R7          CLEAR A REGISTER                          00711300
         IC    R7,0(R6)       GET THE EXECUTE LENGTH                    00711400
         EX    R7,MOVETIT     MOVE IN THE TITLE                         00711500
         LA    R1,1(R1,R7)    MOVE THE POINTER OVER                     00711600
         CLI   0(R2),DSNAME  IS THIS THE DSNAME KEY                     00711700
         BE    TITDSN        YES, SPECIAL PROCESSING                    00711800
         LA    R0,PRNTTITL    POINT TO THE BEGINNING AGAIN              00711900
         SR    R1,R0          AND FIND THE CURRENT LENGTH               00712000
         CH    R1,LINELEN     IS IT TOO LONG?                           00712100
         BNL   TITOVER        YES, PULL BACK                            00712200
         AR    R1,R0          NO, KEEP GOING                            00712300
TITINC   LA    R2,1(R2)            GET THE NEXT CHAIN POINTER           00712400
         CLI   0(R2),0       ARE WE DONE?                               00712500
         BNE   TITLOOP        GO GET MORE TITLES                        00712600
         B     TITEND         ALL DONE                                  00712700
TITDSN   LR    R6,R1         SAVE THE ADDRESS POINTER                   00712800
         LA    R1,DSNPLN     POINT TO THE PDL FOR DSN LENGTH            00712900
         BAL   R8,PDLNUM     GO TRANSLATE IT                            00713000
         LTR   R15,R15       WAS IT THERE?                              00713100
         BP    TITDSN2       YES, USE IT                                00713200
         LA    R15,44        NO, SET THE DEFAULT                        00713300
TITDSN2  STH   R15,DSNLENGT  SAVE THE LENGTH                            00713400
         SH    R15,H9        SUBTRACT THE 9 CHARS MOVED ALREADY         00713500
*              MINUS ONE FOR EX, PLUS ONE FOR SPACE                     00713600
         MVI   0(R6),C' '    GET AN INITIAL BLANK                       00713700
         EX    R15,DSNBLMOV  MOVE IN THE BLANKS                         00713800
         LA    R1,1(R15,R6)  RESET THE POINTER ( INCLUDE A SPACE )      00713900
         LA    R15,1(R15)    ADD ON THE SPACE CHARACTER                 00714000
         STH   R15,DSNLENOF  SAVE THE OFFSET                            00714100
         B     TITINC        ALLOW DSNAME TO OVERFLOW THE LINE          00714200
*                                                                       00714300
*        TITLE RAN OFF THE END, CUT IT OFF                              00714400
*                                                                       00714500
TITOVER  SR    R1,R7          SUBTRACT PAST THIS FIELD                  00714600
         AR    R1,R0          RELOCATE IT                               00714700
         BCTR  R1,0           THEN GET THE LAST CHARACTER               00714800
         MVC   0(9,R1),BLANKS THEN BLANK IT OUT                         00714900
TITEND   TM    SYSOUT+48,X'10' IS THE DCB OPEN                          00715000
         BNO   CKOUTPT       NO, TERMINAL OUTPUT, NO CC                 00715100
         MVI   PRNTTITL,C'0'  ALWAYS SKIP A LINE FOR IT                 00715200
CKOUTPT  CLI   OUTPUTK+1,1    OUTPUT THIS RUN?                          00715300
         BNE   VTRET          YES, SKIP THE OPEN, JUST RETURN           00715400
         MVC   OUTDCB(OUTDCBL),OUTDCBC  INITIALIZE THE DCB              00715500
         MVI   OPENLIST,X'80' TERMINATE THE LIST                        00715600
         OPEN  (OUTDCB,OUTPUT),MF=(E,OPENLIST)  OPEN THE DATA SET DCB   00715700
         B     VTRET          RETURN, INITIALIZATION IS DONE            00715800
         EJECT                                                          00715900
*                                                                       00716000
*        ROUTINE TO CONVERT A TEXT DSCB ITEM                            00716100
*        INTO ITS KEY NUMBER                                            00716200
*        INPUT IS REG 4 - IKJIDENT PTR                                  00716300
*        OUTPUT IS REG 15 - KEY NUMBER                                  00716400
*        ENTRY VIA BAL   R8,GETKEY                                      00716500
*                                                                       00716600
GETKEY   L     R1,ATABTITL     POINT TO THE TABLE                       00716700
         LA    R1,12(R1)     POINT TO THE FIRST ENTRY                   00716800
         LA    R15,1           SET UP THE KEY NUMBER COUNTER            00716900
         L     R6,0(R4)      POINT TO THE ENTERED TEXT                  00717000
         ICM   R3,3,4(R4)    GET THE LENGTH OF THE ENTERED TEXT         00717100
         BNP   GETKNOTF      NOT FOUND IF ZERO                          00717200
         BCTR  R3,0          MINUS ONE FOR THE EX                       00717300
GETKLOOP LA    R2,4(R1)      POINT TO THE COMPARISON TEXT               00717400
         CLI   0(R2),C' '    IS IT HERE?                                00717500
         BNE   GETKSTD       YES, THIS IS IT                            00717600
         LA    R2,1(R2)      NO, MOVE OVER ONE MORE                     00717700
         CLI   0(R2),C' '    IS IT HERE?                                00717800
         BNE   GETKSTD       YES, THIS IS IT                            00717900
         LA    R2,1(R2)      NO, MOVE OVER ONE MORE                     00718000
GETKSTD  EX    R3,GETKCOMP   COMPARE THE KEY TEXT                       00718100
         BE    GETKFND       I FOUND IT                                 00718200
         LA    R1,12(R1)     GET TO THE NEXT KEY                        00718300
         LA    R15,1(R15)    INCREMENT THE KEY COUNTER                  00718400
         CH    R15,H26       CHECK FOR THE END OF THE TABLE             00718500
         BNH   GETKLOOP      NOT YET, KEEP LOOKING                      00718600
*                                                                       00718700
*        KEY WAS NOT FOUND, SEND BACK A ZERO                            00718800
*                                                                       00718900
GETKNOTF SR    R15,R15       SET UP THE ZERO AND RETURN                 00719000
GETKFND  BR    R8            JUST RETURN                                00719100
GETKCOMP CLC   0(0,R6),0(R2) EXECUTED TEXT COMPARE                      00719200
H26      DC    H'26'                                                    00719300
         EJECT                                                          00719400
*                                                                       00719500
*        PROGRAM CONSTANTS                                              00719600
*                                                                       00719700
ZERO     DC    F'0'                                                     00719800
F100     DC    F'100'                                                   00719900
F127     DC    F'127'                                                   00720000
DEFLMAX  DC    H'60'          DEFAULT LINES PER PAGE                    00720100
DEFLEN   DC    H'132'         DEFAULT CHARS PER LINE                    00720200
H2       DC    H'2'                                                     00720300
H5       DC    H'5'                                                     00720400
H9       DC    H'9'                                                     00720500
H10      DC    H'10'                                                    00720600
H12      DC    H'12'                                                    00720700
H18      DC    H'18'                                                    00720800
H32      DC    H'32'                                                    00720900
MOVETIT  MVC   0(0,R1),3(R6)  MOVE IN THE TITLE                         00721000
COMPKEY  CLC   0(0,R1),VTFDSN  EXECUTED COMPARE                         00721100
MOVEHEAD MVC   PRNTHEAD(0),0(R2)                                        00721200
MOVEHED  MVC   PRNTHEAD+21(0),4(R14)  MOVE CMD BUF TO DEFAULT HEADER    00721300
DSNBLMOV MVC   1(0,R6),0(R6)  BLANK OUT THE DSN SPACE IN THE TITLE      00721400
CPAGE    DC    C'PAGE'                                                  00721500
EDMASK   DC    XL16'40202020202020202020202020202120'                   00721600
EDMASK0  DC    XL16'F0202020202020202020202020202120'                   00721700
SKIP     DC    C'0'                                                     00721800
COMP0    DC    CL16'0000000000000000'                                   00721900
BLANKS   DC    CL16'                '                                   00722000
STARS    DC    CL16'****************'                                   00722100
TABSPACE DC    CL8'TRKS'                                                00722200
         DC    CL8'TRKS'                                                00722300
         DC    CL8'TRKS'                                                00722400
         DC    CL8'TRKS'                                                00722500
         DC    CL8'TRKS'                                                00722600
*                                                                       00722700
         PRINT NOGEN                                                    00722800
SYSOUTC  DCB   DSORG=PS,DDNAME=VTOCOUT,MACRF=PM,                       X00722900
               RECFM=FBA,LRECL=150,BLKSIZE=1500                         00723000
OUTDCBC  DCB   DSORG=PS,DDNAME=OUTPUT,MACRF=PM,                        X00723100
               RECFM=FB,LRECL=100,BLKSIZE=6000                          00723200
*                                                                       00723300
*        PROGRAM MESSAGES                                               00723400
*                                                                       00723500
         SPACE 2                                                        00723600
         SPACE                                                          00723700
MSGTOTC  MSG   '  TOTALS -  NNNN DATA SETS, MMMMMMMM UUUUUU ALLOC, LLLLX00723800
               LLLL UUUUUU USED '                                       00723900
MSGTLEN  EQU   *-MSGTOTC                                                00724000
MSGBLC   MSG   '                '                                       00724100
*                                                                       00724200
DEFHEAD  DC    CL20'1 VTOC COMMAND  V-77'                               00724300
*                                                                       00724400
*        DEFAULT PRINT LIST                                             00724500
*                                                                       00724600
DEFPRNT  DC    AL1(ALLOC)                                               00724700
         DC    AL1(UNUSED)                                              00724800
         DC    AL1(PCT)                                                 00724900
         DC    AL1(EXT)                                                 00725000
         DC    AL1(DSORG)                                               00725100
         DC    AL1(RECFM)                                               00725200
         DC    AL1(LRECL)                                               00725300
         DC    AL1(BLKSZ)                                               00725400
         DC    AL1(CDATE)                                               00725500
         DC    AL1(LSTUS)                                               00725600
         DC    AL1(VOLUME)                                              00725700
         DC    AL1(DSNAME)                                              00725800
         DC    AL1(EXPDT)                                               00725900
         DC    AL1(SECQ)                                                00726000
         DC    AL1(SECT)                                                00726100
         DC    AL1(ROUND)                                               00726200
         DC    AL1(PASS)                                                00726300
         DC    AL1(ACTION)                                              00726400
         DC    AL1(TYPE)                                                00726500
         DC    AL1(0)        END OF THE LIST                            00726600
DEFPRNTE EQU   *                                                        00726700
DEFPRNTL EQU   *-DEFPRNT                                                00726800
*                                                                       00726900
*        TABLE OF PRINT ITEM LENGTHS AND TITLES                         00727000
*                                                                       00727100
*        ENTRIES IN THE TABLE FOR EACH FORMATTED ITEM -                 00727200
*        FIRST BYTE IS FIELD LENGTH FOR OUTPUT ( MINUS ONE FOR EX )     00727300
*        SECOND BYTE - X'80' BIT INDICATES A CHARACTER FIELD            00727400
*                            THEN BITS 0-7 GIVE VTFMT LENGTH            00727500
*                      OTHERWISE IT'S A KEY TO WHICH ROUTINE TO USE     00727600
*        THIRD BYTE - OFFSET IN FORMATTED DSCB, VTFMT                   00727700
*        4-12 TH BYTES, THE TITLE FOR THE FIELD                         00727800
*                                                                       00727900
*                                                                       00728000
TABTITL  DC    XL12'00'  DUMMY ENTRY FOR 0 ADDRESSING                   00728100
         DC    AL1(8),AL1(128+7),AL1(VTFACTON-VTFMT),CL9' ACTION  '  1  00728200
         DC    AL1(6),AL1(128+5),AL1(VTFVOLUM-VTFMT),CL9' VOLUME  '  2  00728300
         DC    AL1(5),AL1(000+4),AL1(VTFCREDT-VTFMT),CL9' CDATE   '  3  00728400
         DC    AL1(5),AL1(000+4),AL1(VTFLSTAC-VTFMT),CL9' REFDT   '  4  00728500
         DC    AL1(5),AL1(000+4),AL1(VTFEXPDT-VTFMT),CL9' EXPDT   '  5  00728600
         DC    AL1(6),AL1(00+12),AL1(VTFALLOC-VTFMT),CL9'  ALLOC  '  6  00728700
         DC    AL1(6),AL1(00+16),AL1(VTFALLOC-VTFMT),CL9' UNUSED  '  7  00728800
         DC    AL1(3),AL1(00+20),AL1(VTFALLOC-VTFMT),CL9' PCT     '  8  00728900
         DC    AL1(2),AL1(00+24),AL1(VTFNOEPV-VTFMT),CL9' EX      '  9  00729000
         DC    AL1(3),AL1(128+2),AL1(VTFDSORG-VTFMT),CL9' DSO     ' 10  00729100
         DC    AL1(3),AL1(128+3),AL1(VTFRECFM-VTFMT),CL9' RFM     ' 11  00729200
         DC    AL1(5),AL1(00+28),AL1(VTFBLKSZ-VTFMT),CL9' BLKSZ   ' 12  00729300
         DC    AL1(5),AL1(00+28),AL1(VTFLRECL-VTFMT),CL9' LRECL   ' 13  00729400
         DC    AL1(4),AL1(128+0),AL01(VTFPROT-VTFMT),CL9' PASS    ' 14  00729500
         DC    AL1(3),AL1(128+0),AL1(VTFCATLG-VTFMT),CL9' CAT     ' 15  00729600
         DC    AL1(4),AL1(128+0),AL1(VTFSECAL-VTFMT),CL9' SECT    ' 16  00729700
         DC    AL1(5),AL1(00+28),AL1(VTFSECAM-VTFMT),CL9'  SECQ   ' 17  00729800
         DC    AL1(4),AL1(00+36),AL1(VTFVOLUM-VTFMT),CL9' UNIT    ' 18  00729900
         DC    AL1(5),AL1(128+0),AL1(VTFROUND-VTFMT),CL9' ROUND   ' 19  00730000
         DC    AL1(4),AL1(128+0),AL1(VTFDSTYP-VTFMT),CL9' TYPE    ' 20  00730100
         DC    AL1(6),AL1(00+12),AL01(VTFUSED-VTFMT),CL9'   USED  ' 21  00730200
         DC    AL1(8),AL1(00+32),AL1(VTFVOLUM-VTFMT),CL9'   CCHH  ' 22  00730300
         DC    AL1(6),AL1(128+0),AL1(VTFVOLUM-VTFMT),CL9' DUMMY3  ' 23  00730400
         DC    AL1(6),AL1(128+0),AL1(VTFVOLUM-VTFMT),CL9' DUMMY4  ' 24  00730500
         DC    AL1(6),AL1(128+0),AL1(VTFVOLUM-VTFMT),CL9' DUMMY5  ' 25  00730600
         DC    AL1(8),AL1(00+32),AL001(VTFDSN-VTFMT),CL9' DSNAME  ' 26  00730700
         EJECT                                                          00730800
*                                                                       00730900
*                                                                       00731000
*        P A R S E   C O N T R O L   L I S T                            00731100
*                                                                       00731200
*                                                                       00731300
         PRINT OFF                                                      00731400
         COPY  VTOCPARS                                                 00731500
         PRINT ON                                                       00731600
*                                                                       00731700
*        DYNAMIC WORK AREA                                              00731800
*                                                                       00731900
         SPACE 3                                                        00732000
PRNTWORK DSECT                                                          00732100
         DS    18A            PRINT ROUTINE SAVE AREA                   00732200
TOTDS    DS    F              TOTAL COUNTER                             00732300
TOTALLOC DS    F              TOTAL ALLOCATION                          00732400
TOTUSED  DS    F              TOTAL USED                                00732500
FTOTDS   DS    F              FINAL TOTAL DATA SETS                     00732600
FTOTALLC DS    F              FINAL TOTAL ALLOC                         00732700
FTOTUSED DS    F              FINAL TOTAL USED                          00732800
PRNTLSAV DS    A                                                        00732900
PRINTR8  DS    A                                                        00733000
PRNTTOT8 DS    A                                                        00733100
ADDREND  DS    A                                                        00733200
PAGEADDR DS    A                                                        00733300
OPENLIST DS    2A             PARM LIST FOR OPEN                        00733400
PDLNSAVE DS    8A             REGISTER SAVE AREA FOR PDLNUM RTN         00733500
PRTLSAVE DS    8A             REGISTER SAVE AREA FOR PRNTLINE RTN       00733600
LASTKEY  DS    A              ADDRESS OF LAST KEY FOR SUBTOTALS, BREAKS 00733700
NUMBREAK DS    H              CHARACTERS TO COMPARE FOR BREAK           00733800
NUMTOTAL DS    H              CHARACTERS TO COMPARE FOR SUBTOTALS       00733900
         PRINT NOGEN                                                    00734000
SYSOUT   DCB   DSORG=PS,DDNAME=VTOCOUT,MACRF=PM,                       X00734100
               RECFM=FBA,LRECL=150,BLKSIZE=1500                         00734200
SYSOUTL  EQU   *-SYSOUT                                                 00734300
OUTDCB   DCB   DSORG=PS,DDNAME=OUTPUT,MACRF=PM,                        X00734400
               RECFM=FB,LRECL=100,BLKSIZE=6000                          00734500
OUTDCBL  EQU   *-OUTDCB                                                 00734600
ENDTOTAL DS    X              PROGRAM SWITCHES                          00734700
ENTOTOUT EQU   X'80'          THE FINAL TOTALS HAVE BEEN OUTPUT         00734800
ENDTONLY EQU   X'10'          NO SUBTOTALS, END TOTALS ONLY             00734900
NOBREAK  EQU   X'08'          NO BREAKS                                 00735000
FIRSTIM  DS    X              INITIALIZATION FOR THIS ROUTINE           00735100
PDLMINUS DC    X'00'                                                    00735200
TOTLAST  DS    X                                                        00735300
SPACTYPE DS    CL6          CHARACTERS FOR SPACE UNITS                  00735400
DSNLENGT DS    H                                                        00735500
DSNLENOF DS    H                                                        00735600
CHARS    DS    CL16           CONVERSION TO CHARACTERS                  00735700
MSGWORK  DS    CL256          AREA FOR BUILDING MESSAGES                00735800
WORKLINE DS    CL256          AREA FOR DATA SET                         00735900
PRNTHDRH DS    F                                                        00736000
PRNTHEAD DS    CL256          AREA FOR HEADER                           00736100
PRNTTITH DS    F                                                        00736200
PRNTTITL DS    CL256          AREA FOR ITEM TITLES                      00736300
MSGBL    DS    CL20           AREA FOR BLANK LINE                       00736400
OUTWORK  DS    CL256          WORKING AREA FOR OUTPUT                   00736500
DCBEXIT  DS    F                                                        00736600
JFCB     DS    XL176                                                    00736700
         SPACE                                                          00736800
VTPRNTLS DS    A             PRINT ITEM LIST ADDRESS                    00736900
VTPRNTL  DS    40C           PRINT ITEM LIST ( IF MODIFIED )            00737000
VTPRNTEN DS    40C                                                      00737100
VTPRNTEX DS    C                                                        00737200
         SPACE                                                          00737300
         DS    0D                                                       00737400
LENWORK  EQU   *-PRNTWORK                                               00737500
*                                                                       00737600
*        VTOC COMMAND COMMON AREA                                       00737700
*                                                                       00737800
         PRINT NOGEN                                                    00737900
         VTOCOM                                                         00738000
         SPACE 3                                                        00738100
*                                                                       00738200
*        FORMATTED DSCB                                                 00738300
*                                                                       00738400
         VTFMT                                                          00738500
         SPACE 3                                                        00738600
         PDEDSNAM                                                       00738700
         SPACE 3                                                        00738800
DUMMD    DSECT                                                          00738900
DUMMA    DS    C              DUMMY ENTRY TO USE FOR CONV               00739000
         PRINT NOGEN                                                    00739100
         DCBD  DSORG=PS,DEVD=DA                                         00739200
         END                                                            00739300
/*                                                                      00739400
//*------------------------------------------------------ ASM: VTOCPRNT 00739500
//*                                                                     00739600
//ASM7.SYSIN DD *                                                       00739700
         TITLE 'VTOC COMMAND  SORT  ROUTINE'                            00739800
*********************************************************************** 00739900
*                                                                     * 00740000
*                                                                     * 00740100
* TITLE -      VTOC COMMAND  SORT  ROUTINE                            * 00740200
*                                                                     * 00740300
* FUNCTION -   PUT THIS FORMATTED DSCB INTO THE SORTED LIST.          * 00740400
*                                                                     * 00740500
* OPERATION -  IF THIS IS A NOSORT RUN, JUST CALL THE PRINT ROUTINE.  * 00740600
*              TO BUILD THE SORTED LIST, FIRST DO A SIMPLE HASH       * 00740700
*              ON THE FIRST CHARACTER.  BUILD UP TO 256 SEPARATE      * 00740800
*              LISTS TO SAVE SORT TIME.  THEN SEARCH THROUGH THESE    * 00740900
*              LISTS SEQUENTIALLY.                                    * 00741000
*                                                                     * 00741100
* INPUT -      VTOC COMMON AREA ( VTOCOM )                            * 00741200
*              POINTED TO BY REGISTER 1                               * 00741300
*              USE PARSE DATA, CURRENT FORMATTED DSCB, SORTED LIST    * 00741400
*                                                                     * 00741500
* OUTPUT -     THE FORMATTED DSCB IS PLACED INTO THE SORTED LIST.     * 00741600
*                                                                     * 00741700
* ATTRIBUTES - REENTRANT, REUSEABLE, REFRESHABLE.                     * 00741800
*                                                                     * 00741900
*                                                                     * 00742000
*         PROGRAMMED BY R. L. MILLER  (415) 485-6241                  * 00742100
*                                                                     * 00742200
*                                                                     * 00742300
*********************************************************************** 00742400
         EJECT                                                          00742500
VTOCSORT ENTER 12,24          DO THE HOUSEKEEPING                       00742600
         LR    R11,R1         SAVE ADDR OF VTOCOM                       00742700
         USING VTOCOM,R11     SET ITS ADDRESSABILITY                    00742800
         L     R9,ADDRANSR    POINT TO THE PARSE ANSWER                 00742900
         USING PDL,R9         SET ITS ADDRESSABILITY                    00743000
         USING SORTWORK,R13   SET ADDRESSABILITY FOR LOCAL WORK AREA    00743100
         SPACE 3                                                        00743200
*                                                                       00743300
*        IS THIS A NOSORT RUN ?                                         00743400
*        IF SO, JUST CALL PRINT                                         00743500
*                                                                       00743600
         CLI   SORTK+1,2      IS THIS NOSORT?                           00743700
         BNE   GOSORT         NO, KEEP ON TRUCKIN'                      00743800
         VTCALL PRNT          YES, CALL PRINT AND GET OUT               00743900
         B     SORTRET        GET OUT OF HERE                           00744000
*                                                                       00744100
*        PUT THIS ENTRY WHERE IT BELONGS                                00744200
*                                                                       00744300
GOSORT   L     R3,FORMATAD    POINT TO THE FORMATTED DSCB               00744400
         USING VTFMT,R3       SET ADDRESSABILITY                        00744500
         LA    R6,SORTTAB     POINT TO THE SORT FIELDS TABLE            00744600
         SR    R4,R4                                                    00744700
         IC    R4,1(0,R6)     LOAD HIGH KEY OFFSET                      00744800
         LA    R4,VTFMT(R4)   POINT TO HIGH KEY                         00744900
         LA    R2,VTCSORTH-12 SORT HEADER AREA                          00745000
GOSORT1  LA    R2,12(0,R2)    NEXT ENTRY                                00745100
         LH    R5,4(0,R2)     LOAD COMAPRE LENGTH                       00745200
         CLI   0(R6),C'D'     DESCENDING SORT                           00745300
         BE    GOSORT3        YES                                       00745400
         B     GOSORT4        NO                                        00745500
GOSORT2  ICM   R5,B'1111',0(R2) GET THE HEAD OF THE LIST                00745600
         BNZ   NOTFIRST       IF NON-ZERO, SEARCH THE LIST              00745700
*                                                                       00745800
*        FIRST ENTRY ON THE LIST, IT'S EASY                             00745900
*                                                                       00746000
         ST    R3,0(R2)       START UP THE LIST                         00746100
         B     SORTRET        THEN RETURN                               00746200
GOSORT3  EX    R5,GOSORTCL    COMPARE TO GET CORRECT LIST               00746300
         BL    GOSORT1                                                  00746400
         B     GOSORT2                                                  00746500
GOSORT4  EX    R5,GOSORTCL    COMPARE TO GET CORRECT LIST               00746600
         BH    GOSORT1                                                  00746700
         B     GOSORT2                                                  00746800
*                                                                       00746900
*        FIND A SLOT FOR THIS ENTRY                                     00747000
*              FIRST GET THE SHORTER DSN LENGTH                         00747100
*                                                                       00747200
NOTFIRST SR    R1,R1                                                    00747300
         IC    R1,1(0,R6)     OFFSET OF SORT FIELD                      00747400
         LA    R7,0(R1,R5)    LOAD PREV ENTRY FIELD ADDR                00747500
         LA    R8,0(R1,R3)    LOAD NEW ENTRY FIELD ADDR                 00747600
         C     R1,=A(VTFDSN-VTFMT)  DSN                                 00747700
         BNE   NOTFRST1                                                 00747800
         LH    R1,VTFDSNL-VTFMT(0,R3)                                   00747900
         CH    R1,VTFDSNL-VTFMT(0,R5)                                   00748000
         BNH   NOTFRST0                                                 00748100
         LH    R1,VTFDSNL-VTFMT(0,R5)                                   00748200
NOTFRST0 BCTR  R1,0                                                     00748300
         B     NOTFRST2                                                 00748400
NOTFRST1 LH    R1,2(0,R6)     LOAD SORT FIELD EXEC LENGTH               00748500
NOTFRST2 CLI   0(R6),C'D'     DESCENDING SORT                           00748600
         BE    NOTFRST4       YES                                       00748700
NOTFRST3 EX    R1,COMPVTF     COMPARE THE FIELDS                        00748800
         BL    NEXTENT        LIST ENTRY IS LOWER, UP THE CHAIN         00748900
         BE    CHECKNXT       IDENTICAL, CHECK NEXT FIELD               00749000
         B     INSERT                                                   00749100
NOTFRST4 EX    R1,COMPVTF     COMPARE THE FIELDS                        00749200
         BH    NEXTENT        LIST ENTRY IS LOWER, UP THE CHAIN         00749300
         BE    CHECKNXT       IDENTICAL, CHECK NEXT FIELD               00749400
*                                                                       00749500
*        THE NEW ENTRY GOES HERE                                        00749600
*                                                                       00749700
INSERT   ST    R3,0(R2)       SAVE THE NEW POINTER                      00749800
         ST    R5,VTFNEXT     JUST BEFORE THIS LIST ENTRY               00749900
         B     SORTRET        THEN EXIT                                 00750000
*                                                                       00750100
*                                                                       00750200
CHECKNXT LA    R6,4(0,R6)     NEXT SORT FIELD                           00750300
         CLC   0(4,R6),=F'0'  ANY MORE FIELDS                           00750400
         BE    INSERT         NO, PUT IT HERE                           00750500
         B     NOTFIRST       YES, CHECK IT                             00750600
*                                                                       00750700
*        GET THE NEXT ENTRY ON THIS LIST                                00750800
*                                                                       00750900
NEXTENT  LA    R2,VTFNEXT-VTFMT(R5)  POINT BACK TO THIS ENTRY           00751000
         LA    R6,SORTTAB     RELOAD SORT FIELD TABLE ADDR              00751100
         ICM   R5,B'1111',VTFNEXT-VTFMT(R5)  GET THE NEXT ENTRY         00751200
         BNZ   NOTFIRST       THERE IS ONE, CHECK IT                    00751300
         ST    R3,0(R2)       LAST ENTRY ON THE LIST, PUT IT THERE      00751400
*                                                                       00751500
*        RETURN                                                         00751600
*                                                                       00751700
SORTRET  LEAVE EQ,RC=0                                                  00751800
*                                                                       00751900
*                                                                       00752000
*                                                                       00752100
*        PROGRAM CONSTANTS                                              00752200
*                                                                       00752300
COMPVTF  CLC   0(0,R7),0(R8)     EXECUTED COMPARE                       00752400
GOSORTCL CLC   0(0,R4),6(R2)     EXECUTED COMPARE                       00752500
*                                                                       00752600
*                                                                       00752700
         PRINT NOGEN                                                    00752800
         EJECT                                                          00752900
*                                                                       00753000
*                                                                       00753100
*        P A R S E   C O N T R O L   L I S T                            00753200
*                                                                       00753300
*                                                                       00753400
         PRINT OFF                                                      00753500
         COPY  VTOCPARS                                                 00753600
         PRINT ON                                                       00753700
*                                                                       00753800
*        DYNAMIC WORK AREA                                              00753900
*                                                                       00754000
         SPACE 3                                                        00754100
SORTWORK DSECT                                                          00754200
         DS    18A            PRINT ROUTINE SAVE AREA                   00754300
         SPACE                                                          00754400
         DS    0D                                                       00754500
LENWORK  EQU   *-SORTWORK                                               00754600
*                                                                       00754700
*        VTOC COMMAND COMMON AREA                                       00754800
*                                                                       00754900
         PRINT NOGEN                                                    00755000
         VTOCOM                                                         00755100
         SPACE 3                                                        00755200
*                                                                       00755300
*        FORMATTED DSCB                                                 00755400
*                                                                       00755500
         VTFMT                                                          00755600
         SPACE 3                                                        00755700
         PDEDSNAM                                                       00755800
         SPACE 3                                                        00755900
         END                                                            00756000
/*                                                                      00756100
//*------------------------------------------------------ ASM: VTOCSORT 00756200
//*                                                                     00756300
//LKED.SYSIN DD *                                                       00756400
  ENTRY VTOCCMD                                                         00756500
  NAME VTOC(R)                                                          00756600
/*                                                                      00756700
//*------------------------------------------------------ LKED          00756800
//                                                                      00756900

