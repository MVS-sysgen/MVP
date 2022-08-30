//COLEMAC  JOB (TSO),
//             'Install ESP MACLIB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*  Installs SYS2.COLEMAC
//*
//STEP1   EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.COLEMAC,DISP=(NEW,CATLG),
//             VOL=SER=PUB000,
//             UNIT=SYSDA,SPACE=(CYL,(2,3,14)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=19040)
//SYSUT1   DD  DATA,DLM=@@
./ ADD NAME=#ALD
         MACRO
&N       #ALD  &R,&A
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED F4OM $ALD TO #ALD
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - FEBRUARY 3, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING IT
.* MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO SIMULATES A HYPOTHETICAL "ADD LOGICAL DOUBLE" MACHINE
.* INSTRUCTION. ITS FUNCTION IS SIMILAR TO THE "AL" MACHINE INSTRUCTION
.* EXCEPT THAT IT OPERATES ON 64-BIT NUMBERS. WARNING, THE RESULTING
.* CONDITION CODE IS NOT AN ANALOGOUS EXTENSION FROM THE "AL"
.* INSTRUCTION.
.*
.* INNER MACROS USED - #TEST
.*
.*
.*
         GBLA  &#TESERR
         GBLC  &#TESRET(20)
         LCLA  &A1
         LCLC  &RODD
&RODD    SETC  '&R+1'
         #TEST REGS=&R
         AIF   (&#TESERR NE 0).REGUNKN
&A1      SETA  &#TESRET(1)+1
         AIF   (&A1 LE 15).GOTA1
&A1      SETA  0
.GOTA1   #TEST PFIX=
&RODD    SETC  '&#TESRET(1)&A1'
.REGUNKN ANOP
&N       AL    &RODD,4+&A LOW-ORDER SUM; HI-ORDER AFFECTED?
         BC    12,ALD&SYSNDX NO, SKIP
         AL    &R,=F'1' YES, ADJUST HI-ORDER
ALD&SYSNDX AL  &R,&A GET HI-ORDER SUM
         MEND
./ ADD NAME=#ALDR
         MACRO
&N       #ALDR &R1,&R2
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $ALDR TO #ALDR
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - FEBRUARY 3, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING IT
.* MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO SIMULATES A HYPOTHETICAL "ADD LOGICAL DOUBLE REGISTER"
.* MACHINE INSTRUCTION. ITS FUNCTION IS SIMILAR TO THE "ALR" MACHINE
.* INSTRUCTION EXCEPT THAT IT OPERATES ON 64-BIT NUMBERS. WARNING,
.* THE RESULTING CONDITION CODE IS NOT AN ANALOGOUS EXTENSION FROM THE
.* "ALR" INSTRUCTION.
.*
.* INNER MACROS USED - #TEST
.*
.*
.*
         GBLA  &#TESERR
         GBLC  &#TESRET(20)
         LCLA  &A1
         LCLC  &R1ODD,&R2ODD
&R1ODD   SETC  '&R1+1'
         #TEST REGS=&R1
         AIF   (&#TESERR NE 0).R1UNKN
&A1      SETA  &#TESRET(1)+1
         AIF   (&A1 LE 15).GOTA1A
&A1      SETA  0
.GOTA1A  #TEST PFIX=
&R1ODD   SETC  '&#TESRET(1)&A1'
.R1UNKN  ANOP
&R2ODD   SETC  '&R2+1'
         #TEST REGS=&R2
         AIF   (&#TESERR NE 0).R2UNKN
&A1      SETA  &#TESRET(1)+1
         AIF   (&A1 LE 15).GOTA1B
&A1      SETA  0
.GOTA1B  #TEST PFIX=
&R2ODD   SETC  '&#TESRET(1)&A1'
.R2UNKN  ANOP
&N       ALR   &R1ODD,&R2ODD LOW-ORDER SUM; HI-ORDER AFFECTED?
         BC    3,ALDR&SYSNDX NO, SKIP
         BCTR  &R1,0 YES, ADJUST HI-ORDER
ALDR&SYSNDX ALR &R1,&R2 GET HI-ORDER SUM
         MEND
./ ADD NAME=ALLOC
         MACRO
&NAME    ALLOC &DUMMY,&PERM,                                           X
               &WAITDSN=NO,&WAITVOL=NO,&WAITUNI=NO,                    X
               &DSN=,&DDN=,&DDNRET=,&MEMBER=,&DISP=,                   X
               &VOL=,&UNIT=,&SYSOUT=,&FREE=,&COPIES=,&LABEL=,          X
               &BLKSIZE=,&DEN=,&DSORG=,&KEYLEN=,&LRECL=,&RECFM=,       X
               &PASWORD=,&DSNRET=,&MF=AUTO,&PREFIX=,&ERROR=,           X
               &SPACE=,&F=,&FILE=,&DA=,&QNAME=,&DSORGRT=,              X
               &VOLRET=,&DCBDSN=,&DCBDDN=,&SPECIAL=,&DDNTO=,&TERM=,    X
               &FCB=,&UCS=,                                            X
               &DEFER=,                                    *LBD 03/86* X
               &UNITCNT=,                                  *LBD 06/86* X
               &FORMS=,&DEST=,&SSREQ=,&FORUSER=,&TU=,&DSNPDE=,&MSVGP=
.**********************************************************************
.*                                                                    *
.*    THIS MACRO PROVIDES A DYNAMIC ALLOCATION FUNCTION BY BUILDING   *
.*    A DYNAMIC ALLOCATION PARAMETER LIST AND INVOKING SVC 99.        *
.*    IT FIRST SETS UP A WORKAREA ENVIRONMENT FOR THE PARAMETER LIST  *
.*    AND THEN TESTS THE KEYWORDS SUPPLIED AND INVOKES INNER MACROS   *
.*    TO BUILD THE TEXT UNITS. THE INNER MACROS THEMSELVES USE INNER  *
.*    MACROS TO UPDATE GLOBAL VARIABLES, STORE TEXT UNIT POINTERS ETC *
.*    THERE ARE THREE WAYS OF SPECIFYING THE WORK AREA ADDRESS.       *
.*    A) MF=AUTO, MF=G, MF=(E,ADDRESS,LNTHSYMB).                      *
.*    IN THE FIRST FORM, AN INNER MACRO DYNSPACE IS CALLED TO NAME    *
.*    A WORK AREA, THE NAME BEING RETURNED IN THE GLOBAL SETC         *
.*    VARIABLE &DYNSP. A DSECT IS CREATED TO MAP THIS AREA.           *
.*    THE GLOBAL VARIABLES &DTUO (TEXT UNIT OFFSET COUNTER) AND       *
.*    &DTUPO (TEXT UNIT POINTER OFFSET ACCUMULATOR) ARE SET TO ZERO.  *
.*    THESE ACCUMULATORS ARE UPDATED AS EACH TEXT UNIT PROCESSOR      *
.*    AQUIRES STORAGE. AFTER ALL TEXT UNITS HAVE BEEN BUILT, THE      *
.*    AMOUNT OF SPACE USED IS CALCULATED, AND THE DYNSPACE MACRO IS   *
.*    THEN CALLED AGAIN TO LOG THE AMOUNT NEEDED. DYNSPACE SETS A     *
.*    GLOBAL VARIABLE &DYNSPQ TO THE HIGHEST AMOUNT ANY ALLOC OR      *
.*    FREE MACRO REQUESTED, AND WHEN CALLED WITH THE EXPAND OPTION,   *
.*    (NO OPERANDS OR NAME FIELD SUPPLIED), EXPANDS INTO A DS FOR     *
.*    THAT QUANTITY. (SEE DYNSPACE)                                   *
.*    MF=G SPECIFIES THAT THE ALLOC MACRO ENTER THE BEGIN MACRO       *
.*    WORKAREA TO ACQUIRE THE STORAGE NECESSARY. IT DOES THIS VIA     *
.*    THE RCPDS MACRO. (SEE RCPDS). HOWEVER, IF THE ALLOC MACRO IS    *
.*    CALLED SEVERAL TIMES WITH THIS OPTION, A LOT OF STORAGE WILL BE *
.*    USED UP, AS THE STORAGE WILL NOT BE SHARED. THUS, THIS FORM     *
.*    SHOULD ONLY BE USED IF THE ALLOC/FREE MACRO IS ONLY TO BE USED  *
.*    ONCE OR TWICE DURING AN ASSEMBLY.                               *
.*    MF=E CAUSES THE MACRO TO USE A USER SPECIFIED WORK AREA. THE    *
.*    SECOND PARAMETER GIVES THE NAME OF THE WORKAREA, AND AN         *
.*    OPTIONAL THIRD PARAMETER IS THE NAME OF A SYMBOL TO BE EQUATED  *
.*    TO THE LENGTH OF THE REQUIRED WORK AREA.                        *
.*                                                                    *
.*    DYNAMIC ALLOCATION FUNCTIONS ARE SIMILAR TO THOSE AVAILABLE    *
.*    WITH JCL, USING THE SAME KEYWORDS. HOWEVER, CERTAIN FORMATS    *
.*    ARE SLIGHTLY DIFFERENT. FOR INSTANCE, CERTAIN KEYWORDS CAN     *
.*    HAVE VARYING PARAMETERS, EG DATASET NAME, DDNAME, VOLSER ETC.  *
.*    PROVISION IS MADE FOR BOTH VARIABLE SPECIFICATION.             *
.*    IN THE ABSOLUTE FORM, THE PARAMETER IS ENTERED IN QUOTES,      *
.*    E.G.   ALLOC DSN='SYS1.LINKLIB',DISP=SHR                       *
.*    HOWEVER, THIS NAME REMAINS FIXED FOR THE ASSEMBLY.             *
.*    IN THE VARIABLE FORMAT, THE ADDRESS OF A LOCATOR IS SPECIFIED, *
.*    WHERE THE LOCATOR CONSISTS OF A SIX BYTE FIELD, THE FIRST 4    *
.*    BYTES OF WHICH POINT TO THE PARAMETER, WHILE THE NEXT TWO      *
.*    CONTAIN THE LENGTH.                                            *
.*    EG          ALLOC DSN=LOCATOR                                  *
.*       LOCATOR  DC    A(DSN),Y(12)                                 *
.*       DSN      DC    C'SYS1.LINKLIB'                              *
.*                                                                   *
.*       NUMERIC QUANTITIES E.G. COPIES= FOR SYSOUT, SHOULD EITHER   *
.*       SPECIFY A NUMERIC VALUE, COPIES=3,                          *
.*       A VALUE IN A REGISTER, COPIES=(R3),                         *
.*       OR THE NAME OFF A FULLWORD CONTAINING THE VALUE,            *
.*          COPIES=NUMCOPYS, WHERE NUMCOPYS IS THE NAME OF A         *
.*       FULLWORD FIELD.                                             *
.*                                                                   *
.*       OTHER KEYWORDS SUCH AS DISP= CAN ONLY HAVE THE ABSOLUTE     *
.*       FORM, AND VALUES SHOULD NOT BE ENTERED WITHIN QUOTES.       *
.*       ADDITIONAL FACILITIES NOT AVAILABLE WITH JCL ARE THE        *
.*       RETURN BY THE SYSTEM OF INFORMATION ON THE DATASET, EG      *
.*       DSORG. THIS IS DONE BY SPECIFYING DSORGRT=SYMBOL, WHERE     *
.*       SYMBOL IS A SYMBOL WHICH WILL BE EQUATED TO A TWO BYTE      *
.*       FIELD CONTAINING THE DSORG TYPE (SEE JOB MANAGEMENT,        *
.*       SUPERVISOR AND TSO).                                        *
.*       THE SYSTEM CAN ALSO GENERATE AND RETURN A DDNAME. THIS IS   *
.*       CARRIED OUT BY ENTERING DDNTO=(ADDR1,ADDR2,,...)            *
.*       WHERE ADDR1,ADDR2 ETC ARE THE NAMES OF 8 BYTE FIELDS WHICH  *
.*       ARE TO RECEIVE THE DDNAME.                                  *
.*       FOR FURTHER INFORMATION ON DYNAMIC ALLOCATION, SEE          *
.*       JOB MANAGEMENT, SUPERVISOR AND TSO.                         *
.*                                                                   *
.**********************************************************************
.*  MODIFIED 08/25/82 TO ADD MSVGP SUPPORT .                         *
.*  MODIFIED 09/10/82 TO ADD TERM  SUPPORT .                         *
.*  MODIFIED 10/27/83 TO:                                            *
.*             ADD WAITVOL TO WAIT FOR VOLUME TO BE MOUNTED.         *
.*             ADD WAITDSN TO WAIT FOR DSN TO BECOME AVAILABLE.      *
.*             ADD WAITUNI TO WAIT FOR UNIT TO BECOME AVAILABLE.     *
.*             **  THESE THREE OPTIONS ARE VALID ONLY IF THE USER    *
.*             **  IS AN AUTHORIZED PROGRAM                          *
.*                   LIONEL DYCK/ROCKWELL INT'L  (213) 594-1647      *
.* MODIFIED 03/31/86 TO ADD DEFER SUPPORT.                 *LBD 03/86*
.*                   LIONEL DYCK/ROCKWELL INT'L  (213) 594-1125      *
.* MODIFIED 01/13/92 TO ADD UCS SUPPORT                      GYP 92013
.**********************************************************************
         GBLA  &RCPDYN            COUNTER FOR NO ENTRIES TO MACRO
         GBLA  &DTUO              OFFSET TO TEXT UNITS
         GBLA  &DTUPO             OFFSET TO TEXT UNIT POINTERS
         GBLB  &RCPS99(2)         TELL RCPDSECT NEED DSECTS
         GBLC  &DYNP              PREFIX FOR LABELS FOR THIS CALL
         GBLC  &DYNSP         NAME FOR AUTOMATIC STORAGE ALLOC
         LCLA  &DDNRTO,&DSNRTO         FOR EQUATES FOR RETURNED FLDS
         LCLA  &VOLRTO,&DSRGRTO        FOR EQUATES FOR RETURNED FIELDS
         LCLA  &I                 COUNTER
         LCLB  &DSECT             DSECT NEEDED FOR STORAGE, MF=E
         LCLC  &C,&T,&PAR
.*
.*   THE ALLOC MACRO PROVIDES A DYNAMIC ALLOCATION FUNCTION,
&RCPS99(1)     SETB           1
&RCPDYN  SETA  &RCPDYN+1          INCEREMENT COUNTER
&DYNP    SETC  'DYN&RCPDYN' SET DEFAULT PREFIX
&NAME    DS    0H
         AIF   ('&PREFIX' EQ '').TMF
         AIF   (K'&PREFIX LT 4).POK
         MNOTE 4,'PREFIX TOO LONG, 1ST 4 CHARS USED'
&DYNP    SETC  '&PREFIX'(1,4)
         AGO   .TMF
.POK     ANOP
&DYNP    SETC  '&PREFIX'
.TMF     AIF   ('&MF(1)' EQ 'G').GEN
         AIF   ('&MF' NE 'AUTO').TMFE
NAME     DYNSPACE             GET NAME FOR SPACE
         LA    R1,&DYNSP               LOAD ADDRESS OF PARAM LIST
         USING &DYNP.DS,R1             USE GENERATED DSECT
&T       SETC  'A'
&PAR     SETC  '&DYNSP+4'
&DSECT   SETB  1
         AGO   .START
.TMFE    AIF   ('&MF(2)' NE '').E2OK
         MNOTE 4,'PLIST ADDRESS OMITTED, MF=G USED'
         AGO   .GEN
.E2OK    ANOP
&DSECT   SETB  1
         AIF   ('&MF(2)' EQ '(').RMFE
         LA    R1,&MF(2)               LOAD PARAM LIST ADDRESS
         USING &DYNP.DS,R1             USE GENERATED DSECT
         AGO   .START
.RMFE    AIF   ('&MF(2)' EQ '(R1)' OR '&MF(2)' EQ '(1)').START
         LR    R1,&PAR                 LOAD S99 PARAM LIST ADDRESS
         AGO   .START
.GEN     LA    R1,&DYNP.RBP            LOAD ADDRESS OF S99 RBP
.START   LA    R15,&DYNP.RB            LOAD ADDRESS OF S99 RB
         USING S99RB,R15
         ST    R15,0(R1)               AND STORE IN RB POINTER
         XC    4(&DYNP.LEN-4,R1),4(R1) ZERO PARAMETER LIST
         MVI   S99RBLN,20              MOVE IN LIST LENGTH
         MVI   S99VERB,S99VRBAL        MOVE IN VERB CODE
         LA    R14,&DYNP.TUP           LOAD ADDRESS OF TU POINTERS
         ST    R14,S99TXTPP            STORE ADDRESS IN S99 RB
         AIF   ('&WAITDSN' EQ 'NO').NWD
         OI    S99FLG21,S99WTDSN       SET WAIT FOR DSN FLAG
.NWD     AIF   ('&WAITVOL' EQ 'NO').NWV
         OI    S99FLG21,S99WTVOL       SET WAIT FOR VOLUME
.NWV     AIF   ('&WAITUNI' EQ 'NO').NWU
         OI    S99FLG21,S99WTUNT       SET WAIT FOR UNIT
.NWU     ANOP
         LA    R15,&DYNP.TU            POINT TO SPACE FOR TEXT UNITS
         USING S99TUNIT,R15
&DTUO    SETA  0
&DTUPO   SETA  0
         AIF   ('&SSREQ' EQ 'YES').SSREQ
.TDSN    AIF   ('&DSN&DA' NE '').DSN
         AIF   ('&DSNPDE' NE '').DSNPDE
         AIF   ('&DSNRET' NE '').DSNRT
         AIF   ('&SYSOUT' NE '').SYSOUT
         AIF   ('&DUMMY' NE '').DUMMY
         AIF   ('&QNAME' NE '').QNAME
.TDDN    AIF   ('&DDN&FILE&F' NE '').DDN
         AIF   ('&DDNRET&DDNTO' NE '').DDNRT
.TUNIT   AIF   ('&UNIT&VOL' NE '').UNIT
.TDEFER  AIF   ('&DEFER' EQ 'YES').DEFER                   *LBD 03/86*
.TVOLRET AIF   ('&VOLRET' NE '').VOLRET
.TDSRGO  AIF   ('&DSORGRT' NE '').DSORGRT
.TLABEL  AIF   ('&LABEL' NE '').LABEL
.TPSWD   AIF   ('&PASWORD' NE '').PASWORD
.TFORUSE AIF   ('&FORUSER' NE '').FORUSER
.TTU     AIF   ('&TU' NE '').TU
.TDISP   AIF   ('&DISP' NE '').DISP
.TSPACE  AIF   ('&SPACE' NE '').SPACE
.TLRECL  AIF   ('&LRECL' NE '').DCB
         AIF   ('&DEN' NE '').DCB
         AIF   ('&RECFM' NE '').DCB
         AIF   ('&BLKSIZE' NE '').DCB
         AIF   ('&DSORG' NE '').DCB
         AIF   ('&KEYLEN' NE '').DCB
.TDCBDSN AIF   ('&DCBDSN' NE '').DCBDSN
.TDCBDDN AIF   ('&DCBDDN' NE '').DCBDDN
.TFREE   AIF   ('&FREE' EQ 'CLOSE' AND '&SYSOUT' EQ '').FREE     92013
.TPERM   AIF   ('&PERM' EQ 'PERM' OR '&PERM' EQ 'PERMANENT').PERM
         AIF   ('&DUMMY' EQ 'PERM' OR '&DUMMY' EQ 'PERMANENT').PERM
.TSPECI  AIF   ('&SPECIAL' NE '').SPECIAL
.TMSVGP  AIF   ('&MSVGP' NE '').MSVGP
.TTERM   AIF   ('&TERM' NE '').TERM
         AGO   .SVC99
.TERM    RCPTERM
         AGO   .SVC99
.SSREQ   RCPSSREQ
         AGO   .TDSN
.DSN     RCPDSN &DSN&DA,&MEMBER
         AGO   .TDDN
.DSNPDE  RCPDSNPD &DSNPDE
         AGO   .TDDN
.DSNRT   RCPDSNRT &DSNRET
&DSNRTO  SETA  &DTUO-46
         AGO   .TDDN
.SYSOUT  RCPSYSOU &SYSOUT,COPIES=&COPIES,FREE=&FREE,DEST=&DEST,        X
               FORMS=&FORMS,FCB=&FCB,UCS=&UCS                GYP 92013
         AGO   .TDDN
.DUMMY   RCPDUMMY &DUMMY
         AGO   .TDDN
.QNAME   RCPQNAME &QNAME
         AGO   .TDDN
.DDN     RCPDDN &DDN&F&FILE
         AGO   .TUNIT
.DDNRT   RCPDDNRT &DDNRET
&DDNRTO  SETA  &DTUO-10
         AGO   .TUNIT
.UNIT   RCPUNIT &UNIT,&VOL
         AIF   ('&UNITCNT' EQ '').TDEFER                   *LBD 06/86*
         RCPUNITC &UNITCNT                                 *LBD 06/86*
         AGO   .TDEFER                                     *LBD 03/86*
*        AGO   .TVOLRET
.DEFER   RCPDEFER                                          *LBD 03/86*
         AGO   .TVOLRET                                    *LBD 03/86*
.VOLRET  RCPVOLRT &VOLRET
&VOLRTO  SETA  &DTUO-8
         AGO   .TDSRGO
.DSORGRT RCPDSRGR
&DSRGRTO SETA  &DTUO-2
         AGO   .TLABEL
.LABEL   RCPLABEL &LABEL
         AGO   .TPSWD
.PASWORD RCPPSWD &PASWORD
         AGO   .TFORUSE
.FORUSER RCPFORUS &FORUSER
         AGO   .TTU
.TU      RCPTU &TU
         AGO   .TDISP
.DISP    RCPDISP &DISP
         AGO   .TSPACE
.SPACE   RCPSPACE &SPACE
         AGO   .TLRECL
.DCB     RCPDDCB LRECL=&LRECL,DEN=&DEN,RECFM=&RECFM,BLKSIZE=&BLKSIZE,  X
               DSORG=&DSORG,KEYLEN=&KEYLEN
         AGO .TDCBDSN
.DCBDSN  RCPDCBDS &DCBDSN
         AGO .TDCBDDN
.DCBDDN  RCPDCBDD &DCBDDN
         AGO .TFREE                                              TE7343
.FREE    RCPFREE  &FREE                                          TE7343
         AGO   .TPERM
.PERM    RCPPERM
         AGO   .TSPECI
.MSVGP   RCPMSVGP &MSVGP
         AGO   .SVC99
.SPECIAL RCPSPEC &SPECIAL
.SVC99   ANOP
&DTUPO   SETA  &DTUPO-4
         SPACE
         MVI   &DYNP.TUP+&DTUPO,X'80'  SET HIGH ORDER BIT ON TEXT PTRS
         MVI   &DYNP.RBP,X'80'         SET HIGH ORDER BIT ON RB PTR
         RCPSR2 UNSAVE
&DTUPO   SETA  &DTUPO+4
         AIF   (NOT &DSECT).DYNA
         DROP  R1,R15                  DEACTIVATE ADDRESSABILITY
         LA    R14,4(R1)               POINT TO REQUEST BLOCK
.DYNA    DYNALLOC
         AIF   (NOT &DSECT).LTR
         USING &DYNP.RB,R14            SET UP ADDRESSABILITY
**       NOTE  R14 HAS RB ADDRESS, R15 HAS SVC 99 RETURN CODE        **
.LTR     AIF   ('&ERROR' EQ '').TDDTO
         LTR   R15,R15                 TEST RETURN CODE
         BNZ   &ERROR                  BRANCH IF NON ZERO
.TDDTO   AIF   ('&DDNTO' EQ '').RESERVE
&I       SETA  0
.DDNTOL  ANOP
&I       SETA  &I+1
         AIF   ('&DDNTO(&I)' EQ '').RESERVE
         AIF   ('&DDNTO(&I)'(1,1) EQ '(').DDNTOR
         MVC   &DDNTO(&I).(8),&DYNP.TU+&DDNRTO+2
         AGO   .DDNTOL
.DDNTOR  ANOP
&C       SETC  '&DDNTO(&I)'(2,K'&DDNTO(&I)-2)
         MVC   0(8,&C),&DYNP.TU+&DDNRTO+2
         AGO   .DDNTOL
.RESERVE AIF   (&DSECT).RESDS
         SPACE 1
***********************************************************************
**       RESERVE SPACE FOR DYNALLOC PARAMETER LIST                   **
***********************************************************************
         RCPDS
.SSP     ANOP
&DYNP.RBP DS   F                       SVC 99 REQ BLOCK POINTER
&DYNP.RB  DS   5F                      SVC 99 REQUEST BLOCK
&DYNP.TUP DS   CL&DTUPO                SPACE FOR TEXT POINTERS
         AIF   (&DTUO EQ 0).DTU21
&DYNP.TU  DS   CL&DTUO                 SPACE FOR TEXT UNITS
         AIF   (&DSNRTO EQ 0).TDDNRTO
&DSNRET  EQU   &DYNP.TU+&DSNRTO        OFFSET TO RETURNED DSN
.TDDNRTO AIF   ('&DDNRET' EQ '').DTU11
&DDNRET  EQU   &DYNP.TU+&DDNRTO        OFFSET TO RETURNED DDNAME
.DTU11   AIF   (&VOLRTO EQ 0).DTU12
&VOLRET  EQU   &DYNP.TU+&VOLRTO        OFFSET TO RETURNED VOLSER
.DTU12   AIF   (&DSRGRTO EQ 0).DTU10
&DSORGRT EQU   &DYNP.TU+&DSRGRTO       OFFSET TO RETURNED DSORG
         AGO   .DTU10
.DTU21   ANOP
&DYNP.TU  DS   0C                      NO SPACE NEEDED FOR TEXT UNITS
.DTU10   ANOP
&DYNP.LEN EQU  *-&DYNP.RBP             LENGTH OF SPACE USED
         AIF   (&DSECT).DSP
         RCPDS
         SPACE 3
         AGO   .EXIT
.RESDS   ANOP
         AIF   ('&DYNSP' EQ '').SP3
         DYNSPACE ADD
.SP3     SPACE
&DYNP.DS DSECT                         DSECT TO MAP SVC 99 DATA
         AGO   .SSP
.DSP     AIF   ('&MF(3)' EQ '').END1
&MF(3)   EQU   &DYNP.LEN               LENGTH OF AREA
.END1    ANOP
&SYSECT  CSECT
         SPACE 3
.EXIT    MEND
./ ADD NAME=#DCBD
         MACRO
&N       #DCBD &DSORG=,&DEVD=                                       DBC
.*                                                                  DBC
.*                                                                  DBC
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $DCBD TO #DCBD
.*                                                                  DBC
.* LAST CHANGE DATE - FEBRUARY 2, 1977                              DBC
.*                  - MAILING ADDRESS CHANGE.                       DBC
.*                                                                  DBC
.* LAST CHANGE DATE - APRIL 20, 1976
.*                                                                  DBC
.* THIS MACRO IS A MODIFICATION OF IBM'S DCBD MACRO FROM R21.7 OF   DBC
.* OS. ANY QUESTIONS CONCERNING IT MAY BE ADDRESSED TO:             DBC
.*       809 WHITNEY AVE.                                           DBC
.*       NEW HAVEN, CT. 06511                                       DBC
.*                                                                  DBC
.*                                                                  DBC
.*                                                                  DBC
.*   THIS MACRO DUPLICATES THE FUNCTION OF IBM'S DCBD MACRO BUT     DBC
.* WITH TWO IMPORTANT DIFFERENCES:                                  DBC
.* - THE DSECT STATEMENT IS NOT GENERATED.                          DBC
.* - A FACILITY IS PROVIDED WHEREBY THE FIRST THREE CHARACTERS      DBC
.*   (I.E. "DCB") OF EACH FIELD NAME (BUT NOT BIT NAME) CAN BE      DBC
.*   REPLACED BY ANY ONE, TWO, OR THREE CHARACTERS OF YOUR CHOICE.  DBC
.* AS A RESULT, THE #DCBD MACRO CAN BE USED ANY NUMBER OF TIMES IN  DBC
.* AN ASSEMBLY. REFER TO THE EXAMPLES GIVEN BELOW FOR A             DBC
.* PARTICULARLY USEFUL TECHNIQUE.                                   DBC
.*                                                                  DBC
.*   THIS MACRO USES THE SAME OPERANDS AS DOES THE DCBD MACRO, SO   DBC
.* PLEASE REFER TO IBM'S DATA MANAGEMENT MACROS MANUAL FOR FURTHER  DBC
.* DETAILS CONCERNING THEM.                                         DBC
.*   IN ADDITION TO THE OPERANDS, THE #DCBD MACRO RECOGNIZES THE    DBC
.* NAME FIELD WHICH SHOULD EITHER BE OMITTED OR CONTAIN A ONE, TWO, DBC
.* OR THREE CHARACTER NAME. IF THE NAME FIELD IS OMITTED, THEN THE  DBC
.* CHARACTERS "DCB" ARE USED TO PREFIX ALL FIELD NAMES. IF THE NAME DBC
.* FIELD IS GIVEN, THEN IT RATHER THAN "DCB" IS USED TO PREFIX ALL  DBC
.* FIELD NAMES. IF THE GIVEN NAME IS LONGER THAN THREE CHARACTERS,  DBC
.* THEN ASSEMBLY ERRORS ARE LIKELY TO RESULT.                       DBC
.*                                                                  DBC
.*   AS INDICATED ABOVE, THE GIVEN NAME FIELD AFFECTS ONLY FIELD    DBC
.* NAMES (E.G. "DCBOFLGS" MIGHT BE CHANGED TO "RDROFLGS"), BUT BIT  DBC
.* NAMES (E.G. "DCBOFOPN") ARE NEVER AFFECTED, SINCE THE PREFIX     DBC
.* "DCB" WILL BE USED REGUARDLESS OF WHAT IS GIVEN IN THE NAME      DBC
.* FIELD, SO IN ORDER TO TEST, FOR EXAMPLE, IF A DCB HAS BEEN       DBC
.* OPENED, YOU MIGHT USE  " TM RDROFLGS,DCBOFOPN ".                 DBC
.*   IN ADDITION, A PARTICULAR BIT NAME WILL BE GENERATED ONLY BY   DBC
.* THE FIRST #DCBD MACRO THAT REQUIRES IT, THUS A PARTICULAR BIT    DBC
.* NAME WILL BE GENERATED ONLY ONCE PER ASSEMBLY.                   DBC
.*                                                                  DBC
.*   EXAMPLES - I HAVE FOUND THAT THE MOST VALUABLE USE OF THE      DBC
.* #DCBD MACRO HAS BEEN IN SIMPLIFYING THE PROCEDURE FOR REFERING   DBC
.* TO DCB FIELDS WITHOUT USING A SPECIAL BASE REGISTER. CONSIDER    DBC
.* THE FOLLOWING EXAMPLES:                                          DBC
.*                                                                  DBC
.*       PRINT NOGEN (IF YOU'VE SEEN ONE DCB, YOU'VE SEEN THEM ALL) DBC
.*                                                                  DBC
.*RDR    #DCBD DSORG=QS                                             DBC
.*       ORG   RDRDCB                                               DBC
.*SYSIN  DCB   DDNAME=SYSIN,DSORG=PS, ...                           DBC
.*                                                                  DBC
.*SYSPRINT DCB DDNAME=SYSPRINT,DSORG=PS, ...                        DBC
.*       ORG   SYSPRINT                                             DBC
.*PRT    #DCBD DSORG=QS                                             DBC
.*                                                                  DBC
.*       ORG   *-16                                                 DBC
.*UT1    #DCBD DSORG=IS                                             DBC
.*       ORG   UT1DCB+16                                            DBC
.*SYSUT1 DCB   DDNAME=SYSUT1,DSORG=IS, ...                          DBC
.*                                                                  DBC
.*SYSUT2 DCB   DDNAME=SYSUT2,DSORG=IS, ...                          DBC
.*       ORG   SYSUT2                                               DBC
.*UT2    #DCBD DSORG=IS                                             DBC
.*                                                                  DBC
.*       PRINT GEN                                                  DBC
.*                                                                  DBC
.*   IN THE FIRST AND THIRD EXAMPLES (RDR AND UT1), THE #DCBD MACRO DBC
.* GENERATES FIELD NAMES (E.G. "RDROFLGS" AND "UT1OFLGS"); THE ORG  DBC
.* STATEMENT RESETS THE LOCATION COUNTER TO THE START OF THE FIELD  DBC
.* NAMES; AND THE DCB MACRO GENERATES THE ACTUAL DCB'S ON TOP OF    DBC
.* THE FIELD NAMES.                                                 DBC
.*   IN THE SECOND AND FOURTH EXAMPLES (PRT AND UT2) THE DCB MACRO  DBC
.* GENERATES THE DCB CODE; THE ORG STATEMENT RELOCATES BACK TO THE  DBC
.* START OF THE DCB; AND THEN THE #DCBD MACRO GENERATES THE FIELD   DBC
.* NAMES ON TOP OF THE DCB CODE. NOTE THAT THE DCB CODE IS NOT      DBC
.* DESTROYED BECAUSE THE #DCBD MACRO GENERATES ONLY DS              DBC
.* INSTRUCTIONS.                                                    DBC
.*   BIT NAMES ARE GENERATED ONLY BY THE FIRST AND THIRD #DCBD      DBC
.* MACROS. THE SECOND MACRO HAS THE IDENTICAL EXPANSION PATH AS THE DBC
.* FIRST, SO ALL BIT NAMES THAT IT MIGHT HAVE GENERATED WOULD HAVE  DBC
.* BEEN REDUNDANT, SO NONE IS GENERATED. THE SAME CAN BE SAID FOR   DBC
.* THE FOURTH #DCBD MACRO WITH RESPECT TO THE THIRD. THE THIRD      DBC
.* #DCBD MACRO HAS A DIFFERENT EXPANSION PATH FROM EITHER OF THE    DBC
.* PRECEEDING TWO, SO SOME OF THE BIT NAMES THAT IT WOULD HAVE      DBC
.* GENERATED ARE NOT REDUNDANT. THOSE BIT NAMES THAT ARE REDUNDANT  DBC
.* ARE NOT GENERATED. THOSE BIT NAMES THAT ARE NOT REDUNDANT ARE    DBC
.* GENERATED.                                                       DBC
.*   BOTH THE THIRD AND FOURTH DCB'S ARE ISAM, SO ONLY A SHORT      DBC
.* DEVICE DEPENDANT SECTION IS GENERATED BY THE DCB MACRO. TO       DBC
.* ACCOMPLISH THIS, THE DCB MACRO RELOCATES BACK 16 BYTES,          DBC
.* GENERATES THE DCB NAME, RELOCATES FORWARD 16 BYTES, AND THEN     DBC
.* GENERATES THE DCB CODE. ON THE OTHER HAND, THE PRIMARY FUNCTION  DBC
.* OF THE DCBD MACRO AND, THEREFORE, THE #DCBD MACRO IS TO CREATE   DBC
.* DSECTS. BECAUSE OF THIS IT WOULD BE LOGICALLY INCONSISTANT FOR   DBC
.* THEM TO ATTEMPT TO RELOCATE BACKWARD PRIOR TO DEFINING THE       DBC
.* STARTING POINT. THEREFORE, IN THE CASE OF ISAM (AND BDAM, AND    DBC
.* EXCP, ETC.), THE DCBD AND #DCBD MACROS FIRST DEFINE THE STARTING DBC
.* NAME AND THEN RELOCATE FORWARD (USUALLY 16 BYTES) BEFORE         DBC
.* DEFINING FIELD NAMES. IT IS FOR THIS REASON THAT THE EXTRA ORG   DBC
.* STATEMENT APPEARS IN THE THIRD EXAMPLE ABOVE. IN THE FOURTH      DBC
.* EXAMPLE THE EXTRA ORG STATEMENT IS NOT NEEDED.                   DBC
.*                                                                  DBC
.*                                                                  DBC
.*                                                                  DBC
.* INNER MACROS USED - #DSORG                                       DBC
.*                                                                  DBC
         GBLB  &#DCBDSG                                             DBC
         GBLB  &#DCBSW(150)                                         DBC
         GBLB  &ONESW
         GBLA  &IEZBITS
         LCLA  &A0            FOR DSORG= AND DEVD= ANALYSIS LOOPS
         LCLB  &DSORGIS       SET BY DSORG=IS - ISAM
         LCLB  &DSORGBX       SET BY DSORG=BX OR CX - BTAM
         LCLB  &DSORGDA       SET BY DSORG=DA - BDAM
         LCLB  &DSORGQX       SET BY DSORG=QX OR CX - QTAM
         LCLB  &DSORGCQ       SET BY DSORG=CQ - QTAM DIRECT ACCESS
.*                            MESSAGE QUEUE
         LCLB  &DSORGMQ       SET BY DSORG=MQ - QTAM PROBLEM PROGRAM
.*                            MESSAGE QUEUE INTERFACE
         LCLB  &DSORGXA       SET BY DSORG=XA - EXCP WITH APPENDAGES
         LCLB  &DSORGQS       SET BY DSORG=QS OR PS - QSAM
         LCLB  &DSORGBS       SET BY DSORG=BS OR PS OR PO - BSAM,BPAM
         LCLB  &DSORGXE       SET BY DSORG=XE - EXCP WITH EXTENSION
         LCLB  &DSORGLR       SET BY DSORG=LR - DCBLRECL FIELD ONLY
         LCLB  &DSORGGS       SET BY DSORG=GS - GAM
         LCLB  &DSORGTX       SET BY DSORG=TX - TCAM LINE GROUP
         LCLB  &DSORGTQ       SET BY DSORG=TQ - TCAM MESSAGE QUEUE
         LCLB  &DSORGTR       SET BY DSORG=TR 3705 LINE GROUP    S22024
         LCLB  &DEVDDA        DIRECT ACCESS
         LCLB  &DEVDTA        MAGNETIC TAPE
         LCLB  &DEVDPT        PAPER TAPE
         LCLB  &DEVDRD        READER OR PUNCH, DEVD=RD OR PC
         LCLB  &DEVDPR        PRINTER
         LCLB  &DEVDBS        BINARY SYNCHRONOUS
         LCLB  &DEVDWT        WORLD TRADE TELEGRAPH
         LCLB  &DEVDMR        MAGNETIC CARD READER
         LCLB  &DEVDOR        OPTICAL READER
         LCLC  &C0            SET TO EACH VALUE OF DSORG AND DEVD
         LCLB  &LSW(150)                                            DBC
         LCLC  &P                                                   DBC
&A0      SETA  0                                                    DBC
.LPXYZ   AIF   (&A0 EQ 150).ENDXYZ                                  DBC
&A0      SETA  &A0+1                                                DBC
&#DCBSW(&A0) SETB (&#DCBSW(&A0) OR &ONESW)                          DBC
         AGO   .LPXYZ                                               DBC
.ENDXYZ  ANOP                                                       DBC
&#DCBDSG SETB  (&#DCBDSG OR &ONESW)                                 DBC
&P       SETC  'DCB'                                                DBC
         AIF   ('&N' EQ '').GOTPFIX                                 DBC
&P       SETC  '&N'                                                 DBC
.GOTPFIX ANOP                                                       DBC
         AIF   (&IEZBITS GT 0).SETBTS
&IEZBITS SETA  1
BIT0     EQU   128
BIT1     EQU   64
BIT2     EQU   32
BIT3     EQU   16
BIT4     EQU   8
BIT5     EQU   4
BIT6     EQU   2
BIT7     EQU   1
.SETBTS  ANOP
.*
.*                  ANALYZE DSORG OPERAND
.*
&A0      SETA  N'&DSORG       SET NUMBER OF DSORG FLEMENTS
.A1      AIF   (&A0 LE 0).D0  IF ZERO, LOOP FINISHED
&C0      SETC  '&DSORG(&A0)'  SET TO A DSORG ELEMENT
.*
.*                  TEST FOR VALID DSORG ELEMENT
.*
         AIF   ('&C0' EQ 'IS' OR '&C0' EQ 'PS' OR '&C0' EQ 'BS' OR     *
               '&C0' EQ 'QS' OR '&C0' EQ 'DA' OR '&C0' EQ 'CX' OR      *
               '&C0' EQ 'CQ' OR '&C0' EQ 'MQ' OR '&C0' EQ 'LR').A2
         AIF   ('&C0' EQ 'XE' OR '&C0' EQ 'XA' OR '&C0' EQ 'PO' OR     *
               '&C0' EQ 'BX' OR '&C0' EQ 'QX' OR '&C0' EQ 'GS' OR      *
               '&C0' EQ 'TX' OR '&C0' EQ 'TQ' OR '&C0' EQ '').A2
         AIF   ('&C0' EQ 'TR').A2                                S22024
         IHBERMAC 156,DSORG,&C0
         AGO   .AA
.*
.*                  SET VARIABLES FOR DSORG
.*
.A2      ANOP
&DSORGIS SETB  (&DSORGIS OR '&C0' EQ 'IS')
&DSORGBX SETB  (&DSORGBX OR '&C0' EQ 'BX' OR '&C0' EQ 'CX')
&DSORGDA SETB  (&DSORGDA OR '&C0' EQ 'DA')
&DSORGQX SETB  (&DSORGQX OR '&C0' EQ 'QX' OR '&C0' EQ 'CX')
&DSORGCQ SETB  (&DSORGCQ OR '&C0' EQ 'CQ')
&DSORGMQ SETB  (&DSORGMQ OR '&C0' EQ 'MQ')
&DSORGXA SETB  (&DSORGXA OR '&C0' EQ 'XA')
&DSORGQS SETB  (&DSORGQS OR '&C0' EQ 'QS' OR '&C0' EQ 'PS')
&DSORGBS SETB  (&DSORGBS OR '&C0' EQ 'BS' OR '&C0' EQ 'PS' OR '&C0' EQ *
               'PO')
&DSORGXE SETB  (&DSORGXE OR '&C0' EQ 'XE')
&DSORGLR SETB  (&DSORGLR OR '&C0' EQ 'LR')
&DSORGGS SETB  (&DSORGGS OR '&C0' EQ 'GS')
&DSORGTX SETB  (&DSORGTX OR '&C0' EQ 'TX')
&DSORGTQ SETB  (&DSORGTQ OR '&C0' EQ 'TQ')
&DSORGTR SETB  (&DSORGTR OR '&C0' EQ 'TR')                       S22024
.AA      ANOP
&A0      SETA  &A0-1          DECREMENT ELEMENT COUNTER
         AGO   .A1            TO DO NEW LOOP
.*
.*                  TEST FOR ANY VALID DSORG OPERAND
.*
.D0      AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *
               &DSORGCQ OR &DSORGMQ OR &DSORGXA OR &DSORGQS OR         *
               &DSORGBS OR &DSORGXE OR &DSORGLR OR &DSORGGS).D0A
         AIF   (&DSORGTX OR &DSORGTQ OR &DSORGTR).D0A            S22024
         IHBERMAC 173
.D0A     ANOP
         AIF   (T'&DEVD NE 'O').D1      BRANCH IF DEVD CODED       1318
         AIF   (&DSORGGS).B1
         AIF   (NOT (&DSORGQS OR &DSORGBS OR &DSORGXE)).B1
.*
.*                  SET DEVD DEFAULTS
.*
&DEVDDA  SETB  1
&DEVDTA  SETB  ('&DSORG' NE '(PO)')
&DEVDPT  SETB  ('&DSORG' NE '(PO)')
&DEVDPR  SETB  ('&DSORG' NE '(PO)')
&DEVDRD  SETB  ('&DSORG' NE '(PO)')
&DEVDMR  SETB  ('&DSORG' NE '(PO)')
&DEVDOR  SETB  ('&DSORG' NE '(PO)')
         AIF   ('&DSORG' EQ '(PO)').D1
         IHBERMAC 174
         AGO   .B1
.D1      ANOP
.*
.*                  ANALYZE DEVD OPERAND
.*
&A0      SETA  N'&DEVD        SET ELEMENT COUNT
.D2      ANOP
         AIF   (&A0 LE 0).B1  IF ZERO, LOOP FINISHED
&C0      SETC  '&DEVD(&A0)'   SET TO A DEVD ELEMENT
.*
.*                  TEST FOR VALID DEVD ELEMENT
.*
         AIF   ('&C0' EQ 'DA' OR '&C0' EQ 'TA' OR '&C0' EQ 'PT' OR     *
               '&C0' EQ 'PR' OR '&C0' EQ 'RD' OR '&C0' EQ 'PC' OR      *
               '&C0' EQ 'BS' OR '&C0' EQ 'WT').D3
         AIF   ('&C0' EQ 'MR' OR '&C0' EQ 'OR').D3
         AIF   ('&C0' EQ '').D4
         IHBERMAC 157,DEVD,&C0
         AGO   .D4            TO DO NEW LOOP
.*
.*                  SET VARIABLES FOR DEVD
.*
.D3      ANOP
&DSORGXE SETB  (&DSORGXE OR T'&DSORG EQ 'O') FORCE EXCP EXTENDED
&DEVDDA  SETB  (&DEVDDA OR '&C0' EQ 'DA')    DIRECT ACCESS DEVICE
&DEVDTA  SETB  (&DEVDTA OR '&C0' EQ 'TA')    MAGNETIC TAPE DEVICE
&DEVDPT  SETB  (&DEVDPT OR '&C0' EQ 'PT')    PAPER TAPE DEVICE
&DEVDPR  SETB  (&DEVDPR OR '&C0' EQ 'PR')    PRINTER
&DEVDRD  SETB  (&DEVDRD OR '&C0' EQ 'RD' OR '&C0' EQ 'PC') READER,PUNCH
&DEVDBS  SETB  (&DEVDBS OR '&C0' EQ 'BS')    BINARY SYNCHRONOUS COMM.
&DEVDWT  SETB  (&DEVDWT OR '&C0' EQ 'WT')    WORLD TRADE TELEGRAPH
&DEVDMR  SETB  (&DEVDMR OR '&C0' EQ 'MR')    MAGNETIC CHAR READER
&DEVDOR  SETB  (&DEVDOR OR '&C0' EQ 'OR')    OPTICAL READER
.D4      ANOP
&A0      SETA  &A0-1          DECREMENT ELEMENT COUNTER
         AGO   .D2            TO DO NEW LOOP
.*
.B1      ANOP
         AIF   ('&DSORG(1)' EQ 'LR').BA SKIP OVER COMMENTS
         SPACE 2
*                       DCB SYMBOLIC DEFINITION FOR
         AIF   (NOT &DSORGIS).B2
*                       INDEXED SEQUENTIAL
.B2      AIF   (NOT (&DSORGQS AND &DSORGBS)).B2A
*                       PHYSICAL SEQUENTIAL
         AGO   .B3
.B2A     AIF   (NOT &DSORGQS).B2B
*                       QSAM
.B2B     AIF   (NOT &DSORGBS).B3
*                       BSAM-BPAM
.B3      AIF   (NOT &DSORGDA).B4
*                       DIRECT ACCESS
.B4      AIF   (NOT (&DSORGBX AND &DSORGQX)).B4A
*                       COMMUNICATIONS LINE GROUP
         AGO   .B5
.B4A     AIF   (NOT &DSORGBX).B4B
*                       BTAM LINE GROUP
.B4B     AIF   (NOT &DSORGQX).B5
*                       QTAM LINE GROUP
.B5      AIF   (NOT &DSORGCQ).B6
*                       COMMUNICATIONS DIRECT ACCESS QUEUE
.B6      AIF   (NOT &DSORGMQ).B6A
*                       QTAM MESSAGE QUEUE
.B6A     AIF   (NOT &DSORGTX).B6B
*                       TCAM LINE GROUP
.B6B     AIF   (NOT &DSORGTQ).B6C                                S22024
*                       TCAM MESSAGE QUEUE
.B6C     AIF   (NOT &DSORGTR).B7                                 S22024
*                       3705 LINE GROUP
.B7      AIF   (NOT (&DSORGXA AND &DSORGXE)).B8
*                       EXCP WITH EXTENSION AND APPENDAGES
         AGO   .BA
.B8      AIF   (NOT &DSORGXE).B9
*                       EXCP WITH EXTENSION
         AGO   .BA
.B9      AIF   (NOT &DSORGXA).B0
*                       EXCP WITH APPENDAGES
         AGO   .BA
.B0      AIF   (NOT &DSORGGS).B00
*                       GRAPHICS WITH APPENDAGES
         AGO   .BA
.B00     AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *
               &DSORGCQ OR &DSORGMQ OR &DSORGQS OR &DSORGBS OR         *
               &DSORGTX OR &DSORGTQ OR &DSORGLR OR &DSORGTR).BA  S22024
*                       EXCP WITH EXTENSION
&DSORGXE SETB  1
.BA      ANOP
         SPACE 1
**********************************************************************
*   02/29/72          LEVEL=04
*
**********************************************************************
         SPACE 1
         AIF   (&DSORGGS).SLIP
&LSW(001) SETB (1)                                                  DBC
&P.DCB   DS    0A 
         SPACE 1
         AIF   (NOT (&DEVDDA OR &DEVDTA OR &DEVDPT OR &DEVDPR OR       *
               &DEVDRD OR &DEVDWT OR &DEVDOR OR &DEVDMR)).C4
&LSW(002) SETB (1)                                                  DBC
*                       DEVICE INTERFACES
         SPACE 1
         AIF   (NOT &DEVDDA).C1
&LSW(003) SETB (1)                                                  DBC
*                       DIRECT ACCESS DEVICES
         SPACE 1
&P.RELAD DS    CL4 -          PARTITIONED ORGANIZATION DATA SET -
*                             ADDRESS (IN THE FORM TTRN) OF MEMBER
*                             CURRENTLY USED.  ---
*                             SYS1.LOGREC DATA SET - IF CCH OPTION HAS
*                             BEEN SPECIFIED IN SYSGEN PROCESS, ADDRESS
*                             OF A 12-BYTE PARAMETER IN THE EXPANSION
*                             OF MACRO INSTRUCTION IGFCATAP
&P.KEYCN DS    FL1 -          KEYED BLOCK OVERHEAD CONSTANT
&P.FDAD  DS    CL8 -          FULL DISK ADDRESS IN THE FORM OF MBBCCHHR
*                             OF RECORD THAT WAS JUST READ OR WRITTEN
         SPACE 1
         ORG   &P.FDAD+7
&P.DVTBL DS    0A -           SAME AS DCBDVTBA BELOW
         DS    X -            LAST BYTE OF DCBFDAD
&P.DVTBA DS    AL3 -          ADDRESS OF ENTRY IN I/O DEVICE
*                             CHARACTERISTICS TABLE FOR DEVICE BEING
*                             USED
         DS    FL1 -          DCBKEYLE - KEY LENGTH OF DATA SET
         DS    C -            DCBDEVT - DEVICE TYPE
*   FOR MASKS FOR ISAM DIRECT ACCESS, SEE DCBOVDEV IN ISAM SECTION
         AIF   (&#DCBSW(003)).SKP003A                               DBC
DCBDV311 EQU   X'21' -        2311 DISK DRIVE
DCBDV301 EQU   X'22' -        2301 PARALLEL DRUM
DCBDV303 EQU   X'23' -        2303 SERIAL DRUM
DCBDV302 EQU   X'24' -        2302 DISK STORAGE
DCBDV321 EQU   X'25' -        2321 DATA CELL DRIVE
DCBDV314 EQU   X'28' -        2314 DISK STORAGE FACILITY
.SKP003A ANOP  ,                                                    DBC
&P.TRBAL DS    H -            TRACK BALANCE.  NUMBER OF BYTES REMAINING
*                             ON CURRENT TRACK AFTER A WRITE OPERATION
*                             (THIS QUANTITY MAY BE NEGATIVE IF THERE
*                             ARE NO BYTES REMAINING ON TRACK).
         SPACE 1
.C1      AIF   (NOT &DEVDTA).C2
&LSW(004) SETB (1)                                                  DBC
*                       MAGNETIC TAPE
         SPACE 1
         ORG   &P.DCB
         DS    CL12 -         RESERVED FOR I/O SUPERVISOR
&P.BLKCT DS    F -            BLOCK COUNT FOR EACH VOLUME
&P.TRTCH DS    C -            TAPE RECORDING TECHNIQUE FOR 7-TRACK TAPE
         AIF   (&#DCBSW(004)).SKP004A                               DBC
DCBMTE   EQU   X'23' -        E  - EVEN PARITY
DCBMTT   EQU   X'3B' -        T  - BCD/EBCDIC TRANSLATION
DCBMTC   EQU   X'13' -        C  - DATA CONVERSION
DCBMTET  EQU   X'2B' -        ET - EVEN PARITY AND TRANSLATION
.SKP004A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(004)).SKP004B                               DBC
DCBDVMT  EQU   X'81' -        2400 SERIES MAGNETIC TAPE UNIT (7-TRACK
*                             OR 9-TRACK)
DCBDVMT3 EQU   X'83' -        3400 SERIES MAGNETIC TAPE UNIT     ICB277
.SKP004B ANOP  ,                                                    DBC
&P.DEN   DS    C -            TAPE DENSITY - 2400 SERIES MAGNETIC TAPE
*                             UNITS
         AIF   (&#DCBSW(004)).SKP004C                               DBC
*                             CODE    7-TRACK     9-TRACK
DCBMTDN0 EQU   X'03' -         0       200 BPI       -
DCBMTDN1 EQU   X'43' -         1       556 BPI       -
DCBMTDN2 EQU   X'83' -         2       800 BPI     800 BPI
DCBMTDN3 EQU   X'C3' -         3         -        1600 BPI
.SKP004C ANOP  ,                                                    DBC
         DS    X -            RESERVED
         SPACE 1
.C2      AIF   (NOT &DEVDPT).C3
&LSW(005) SETB (1)                                                  DBC
*                       PAPER TAPE
         SPACE 1
         ORG   &P.DCB+8
&P.LCTBL DS    A -            ADDRESS OF TRANSLATE TABLE
         DS    XL4 -          RESERVED
&P.CODE  DS    C -            PAPER TAPE CODE BEING USED.  THE
*                             APPROPRIATE TRANSLATE TABLE IS MADE
*                             AVAILABLE
         AIF   (&#DCBSW(005)).SKP005A                               DBC
DCBPTCDN EQU   X'80' -        N - NO CONVERSION
DCBPTCDI EQU   X'40' -        I - IBM BCD
DCBPTCDF EQU   X'20' -        F - FRIDEN
DCBPTCDB EQU   X'10' -        B - BURROUGHS
DCBPTCDC EQU   X'08' -        C - NATIONAL CASH REGISTER
DCBPTCDA EQU   X'04' -        A - ASCII (8-TRACK)
DCBPTCDT EQU   X'02' -        T - TELETYPE
.SKP005A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(005)).SKP005B                               DBC
DCBDVPTP EQU   X'50' -        2671 PAPER TAPE READER
.SKP005B ANOP  ,                                                    DBC
         DS    X -            RESERVED
&P.PTFLG DS    BL1 -          PAPER TAPE FLAGS
         AIF   (&#DCBSW(005)).SKP005C                               DBC
DCBPTIC  EQU   BIT3 -         INVALID CHARACTER IN LAST RECORD READ
DCBPTECT EQU   BIT4 -         END OF RECORD CHARACTER REACHED IN
*                             TRANSLATION
DCBPTECR EQU   BIT5 -         END OF RECORD CHARACTER DETECTED DURING
*                             READ
DCBPTUCT EQU   BIT6 -         IF ONE, UPPER CASE TRANSLATE.
*                             IF ZERO, LOWER CASE TRANSLATE
DCBPTERR EQU   BIT7 -         ERROR DETECTED ON READ
.SKP005C ANOP  ,                                                    DBC
         SPACE 1
.C3      AIF   (NOT &DEVDPR).C3A
&LSW(006) SETB (1)                                                  DBC
*                       PRINTER
         SPACE 1
         ORG   &P.DCB+16
&P.PRTSP DS    C -            NUMBER INDICATING NORMAL PRINTER SPACING
         AIF   (&#DCBSW(006)).SKP006A                               DBC
DCBPRSP0 EQU   X'01' -        0 - NO SPACING
DCBPRSP1 EQU   X'09' -        1 - SPACE ONE LINE
DCBPRSP2 EQU   X'11' -        2 - SPACE TWO LINES
DCBPRSP3 EQU   X'19' -        3 - SPACE THREE LINES
.SKP006A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(006)).SKP006B                               DBC
DCBDVPR1 EQU   X'48' -        1403 PRINTER AND 1404 PRINTER (CONTINUOUS
*                             FORM SUPPORT ONLY)
DCBDVPR2 EQU   X'4A' -        1443 PRINTER
DCBDVPR3 EQU   X'49' -        3211 PRINTER
.SKP006B ANOP  ,                                                    DBC
&P.PRTOV DS    C -            TEST-FOR-PRINTER-OVERFLOW MASK
*                             (PRTOV MASK)
         AIF   (&#DCBSW(006)).SKP006C                               DBC
DCBPRC9  EQU   X'20' -        9  - TEST FOR CHANNEL 9 OVERFLOW
DCBPRC12 EQU   X'10' -        12 - TEST FOR CHANNEL 12 OVERFLOW
.SKP006C ANOP  ,                                                    DBC
         DS    X -            RESERVED
         SPACE 1
.C3A     AIF   (NOT &DEVDRD).C3B
&LSW(007) SETB (1)                                                  DBC
*                       CARD READER, CARD PUNCH
         SPACE 1
         ORG   &P.DCB+16
&P.MODE  DS    0B -           MODE OF OPERATION FOR 1442 CARD READ
*                             PUNCH (BITS 0-3)
&P.STACK DS    B -            STACKER SELECTION (BITS 4-7)
         AIF   (&#DCBSW(007)).SKP007A                               DBC
DCBMODEC EQU   BIT0 -         COLUMN BINARY MODE
DCBMODEE EQU   BIT1 -         EBCDIC MODE
DCBMODEO EQU   BIT2 -         OPTICAL MARK READ MODE
DCBMODER EQU   BIT3 -         READ COLUMN ELIMINATE MODE
DCBSTCK2 EQU   BIT6 -         STACKER 2
DCBSTCK1 EQU   BIT7 -         STACKER 1
.SKP007A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(007)).SKP007B                               DBC
DCBDVCR0 EQU   X'41' -        2540 CARD READER
DCBDVCP0 EQU   X'42' -        2540 CARD PUNCH
DCBDVCRP EQU   X'43' -        1442 CARD READ PUNCH
DCBDVCR1 EQU   X'44' -        2501 CARD READER
DCBDVCPR EQU   X'45' -        2520 CARD READ PUNCH
DCBDVCR2 EQU   X'46'          3505 CARD READER                   XM0629
DCBDVCP1 EQU   X'4C'          3525 CARD PUNCH                    XM0629
.SKP007B ANOP  ,                                                    DBC
         DS    X -            RESERVED
&P.FUNC  DS    B -            FUNCTION INDICATOR FOR THE 3525
         AIF   (&#DCBSW(007)).SKP007C                               DBC
DCBFNCBI EQU   BIT0 -         INTERPRET (PUNCH AND PRINT TWO LINES)
DCBFNCBR EQU   BIT1 -         READ
DCBFNCBP EQU   BIT2 -         PUNCH
DCBFNCBW EQU   BIT3 -         PRINT
DCBFNCBD EQU   BIT4 -         DATA PROTECTION
DCBFNCBX EQU   BIT5 -         THIS DATA SET IS TO BE PRINTED
DCBFNCBT EQU   BIT6 -         TWO-LINE PRINT SUPPORT REQUEST
.SKP007C ANOP  ,                                                    DBC
         SPACE 1
.C3B     AIF   (NOT &DEVDWT).C3C
&LSW(008) SETB (1)                                                  DBC
*                       WORLD TRADE TELEGRAPH
         SPACE 1
         ORG   &P.DCB+16
&P.BQFLG DS    BL1 -          WTTA FLAG BYTE
         AIF   (&#DCBSW(008)).SKP008A                               DBC
DCBBQWRU EQU   BIT1 -         WRU FEATURE IS TO BE USED
DCBBQIAM EQU   BIT2 -         IAM FEATURE IS TO BE USED
DCBBQWRS EQU   BIT3 -         WRU FEATURE TO BE USED IN SEND HEADER
*                             SUBGROUP
DCBBQWRE EQU   BIT4 -         WRU FEATURE TO BE USED IN END SEND
*                             SUBGROUP
.SKP008A ANOP  ,                                                    DBC
&P.WTEOM DS    C -            EOM CHARACTER
&P.WTEOT DS    C -            EOT CHARACTER
&P.WTPAD DS    FL1 -          NUMBER OF PAD (LTRS) CHARACTERS REQUIRED
*                             FOR MOTOR-ON DELAY
         SPACE 1
.C3C     AIF   (NOT (&DEVDOR OR &DEVDMR)).C4
&LSW(009) SETB (1)                                                  DBC
*                       OPTICAL READER AND MAGNETIC CHAR READER
         SPACE 1
         ORG   &P.DCB
&P.WTOID DS    0A -           SAME AS DCBWTOIA BELOW
         DS    X -            RESERVED
&P.WTOIA DS    AL3 -          A BINARY IDENTIFICATION NUMBER ASSIGNED
*                             BY COMMUNICATIONS TASK TO MESSAGE ISSUED
*                             BY WTO MACRO.  THIS NUMBER IS USED BY THE
*                             DOM MACRO WHEN MESSAGE IS NO LONGER
*                             REQUIRED (MCS SUPPORT).  ---
*                             FOR MAGNETIC CHAR READER - AFTER FIRST
*                             READ HAS BEEN ISSUED, CONTAINS ADDRESS OF
*                             MAGNETIC INTERRUPT CONTROL BLOCK (MICB)
*                             BEING USED BY THE APPENDAGES.
         SPACE 1
         AIF   (NOT &DEVDOR).C3D
&LSW(010) SETB (1)                                                  DBC
*                       OPTICAL READER DEVICES
*                       1285, 1287, 1288
         SPACE 1
         ORG   &P.WTOID+4
&P.ERRCN DS    0A -           SAME AS DCBERRCA BELOW
         DS    X -            RESERVED
&P.ERRCA DS    AL3 -          ADDRESS OF 32 BYTES OF DECLARED STORAGE
*                             SPECIFIED BY THE USER IN HIS PROGRAM.
*                             THIS STORAGE WILL BE USED BY THE
*                             PROGRAMMING SUPPORT AS EIGHT 4-BYTE
*                             COUNTERS IN WHICH TOTALS OF CERTAIN 1285,
*                             1287 AND 1288 ERROR CONDITIONS ARE
*                             ACCUMULATED.
&P.DSPLY DS    0A -           SAME AS DCBDSPLA BELOW
         DS    X -            RESERVED
&P.DSPLA DS    AL3 -          ADDRESS OF DSPLY (BSAM) ROUTINE USED FOR
*                             KEYBOARD ENTRY OF A COMPLETE FIELD
&P.RESCN DS    0A -           SAME AS DCBRESCA BELOW
&P.RDLNE DS    0A -           SAME AS DCBRDLNA BELOW
         DS    X -            RESERVED
&P.RESCA DS    0AL3 -         ADDRESS OF RESCN (BSAM) ROUTINE USED TO
*                             FORCE ON-LINE CORRECTION OF UNREADABLE
*                             CHARACTERS
&P.RDLNA DS    AL3 -          ADDRESS OF RDLNE (QSAM) ROUTINE USED TO
*                             FORCE ON-LINE CORRECTION OF UNREADABLE
*                             CHARACTERS
&P.ORBYT DS    BL1 -          OPTICAL READER BYTE USED BY BSAM/QSAM
         AIF   (&#DCBSW(010)).SKP010A                               DBC
DCBORSYN EQU   BIT0 -         SYNAD IN CONTROL
DCBOREOF EQU   BIT1 -         END OF FILE (EOF)
DCBORBFP EQU   BIT2 -         BUFFERS PRIMED (QSAM)
.SKP010A ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(010)).SKP010B                               DBC
DCBDVOR5 EQU   X'5A' -        1285 OPTICAL READER
DCBDVOR7 EQU   X'5B' -        1287 OPTICAL READER
DCBDVOR8 EQU   X'5C' -        1288 OPTICAL READER
.SKP010B ANOP  ,                                                    DBC
&P.EIB   DS    BL1 -          ERROR INDICATOR BYTE
         AIF   (&#DCBSW(010)).SKP010C                               DBC
DCBORNRM EQU   BIT1 -         THE 1287 OR 1288 SCANNER WAS UNABLE TO
*                             LOCATE THE REFERENCE MARK
DCBORREJ EQU   BIT2 -         FOR 1287, A STACKER SELECT COMMAND WAS
*                             GIVEN AFTER ALLOTTED TIME HAD ELAPSED AND
*                             THE DOCUMENT HAS BEEN PUT IN REJECT
*                             POCKET.  FOR 1288 UNFORMATTED ONLY,
*                             END-OF-PAGE HAS OCCURRED.
DCBORERR EQU   BIT3 -         A NONRECOVERABLE ERROR HAS OCCURRED.
DCBORECK EQU   BIT4 -         AN EQUIPMENT CHECK RESULTED IN AN
*                             INCOMPLETE READ
DCBORWLR EQU   BIT5 -         A WRONG-LENGTH RECORD CONDITION HAS
*                             OCCURRED
DCBORHPR EQU   BIT6 -         FOR QSAM - OPERATOR ENTERED ONE OR MORE
*                             CHARACTERS FROM THE KEYBOARD.
*                             FOR BSAM - A HOPPER EMPTY CONDITION HAS
*                             OCCURRED
DCBORDCK EQU   BIT7 -         A DATA CHECK HAS OCCURRED
.SKP010C ANOP  ,                                                    DBC
         DS    X -            RESERVED
         SPACE 1
.C3D     AIF   (NOT &DEVDMR).C4
&LSW(011) SETB (1)                                                  DBC
*                       MAGNETIC CHARACTER READER DEVICES
*                       1419 MAGNETIC CHARACTER READER
*                       1275 OPTICAL READER SORTER
         SPACE 1
         ORG   &P.DCB
&P.SSID  DS    CL8 -          BEFORE DCB IS OPENED - NAME OF USER'S
*                             STACKER SELECT ROUTINE.
         SPACE 1
         ORG   &P.SSID
         DS    A -            AFTER DCB IS OPENED - DCBWTOID
&P.SSAD  DS    0A -           ADDRESS OF USER'S STACKER SELECT ROUTINE
         DS    X -            RESERVED
&P.SSADA DS    AL3 -          ADDRESS OF USER'S STACKER SELECT ROUTINE
&P.IMAGE DS    0A -           SAME AS DCBIMAGA BELOW
&P.MRFG  DS    BL1 -          BUFFER INDICATOR
         AIF   (&#DCBSW(011)).SKP011A                               DBC
DCBMRBCT EQU   BIT0+BIT1 -    TWO-BIT BINARY COUNTER WHICH INDICATES
*                             INTO WHICH BUFFER STATUS INFORMATION IS
*                             TO BE POSTED
.SKP011A ANOP  ,                                                    DBC
&P.IMAGA DS    AL3 -          ADDRESS OF PARAMETER LIST USED TO
*                             COMMUNICATE BETWEEN USER'S PROCESSING
*                             ROUTINES AND HIS STACKER SELECT ROUTINES
&P.ECBLT DS    0A -           SAME AS DCBECBLA BELOW
&P.MRIND DS    BL1 -          INDICATOR AND COUNTER BYTE
         AIF   (&#DCBSW(011)).SKP011B                               DBC
DCBMRDCT EQU   BIT0+BIT1+BIT2 THREE-BIT BINARY COUNTER OF NUMBER OF
*                             DOCUMENTS READ AFTER DISENGAGE
DCBMRSCU EQU   BIT3 -         DCB WAS ALTERED WHEN SYNAD ROUTINE WAS
*                             ENTERED DUE TO SECONDARY CONTROL UNIT
*                             (SCU) ERROR
DCBMRPLO EQU   BIT4 -         POCKET LIGHT HAS BEEN TURNED ON
DCBMRPLS EQU   BIT5 -         POCKET LIGHT 0-6 IS BEING SET ON
DCBMRERP EQU   BIT6 -         ERROR RECOVERY PROCEDURE IS EXECUTING FOR
*                             PRIMARY CONTROL UNIT (PCU)
DCBMRERS EQU   BIT7 -         ERROR RECOVERY PROCEDURE IS EXECUTING FOR
*                             SECONDARY CONTROL UNIT (SCU)
.SKP011B ANOP  ,                                                    DBC
&P.ECBLA DS    AL3 -          ADDRESS OF ECB LIST PASSED TO WAIT MACRO
*                             BY CHECK MACRO WHEN NO 1419/1275 IS
*                             AVAILABLE FOR PROCESSING
&P.MRFLG DS    BL1 -          FLAG BYTE
         AIF   (&#DCBSW(011)).SKP011C                               DBC
DCBMRSCC EQU   BIT0 -         FIRST OR SECOND SECONDARY CONTROL UNIT
*                             COMMAND CHAIN IS BEING USED
DCBMRDBG EQU   BIT1 -         DEBUGGING MODE IN USE
DCBMRDRU EQU   BIT2 -         DISENGAGE REQUESTED BY USER
DCBMRDR  EQU   BIT3 -         DISENGAGE REQUESTED
DCBMRPCC EQU   BIT4+BIT5 -    TWO-BIT BINARY COUNTER INDICATING FIRST,
*                             SECOND OR THIRD PRIMARY CONTROL UNIT
*                             COMMAND CHAIN IS BEING USED
DCBMRDWT EQU   BIT6 -         WTO MESSAGE MUST BE DELETED
DCBMRUE  EQU   BIT7 -         UNIT EXCEPTION
.SKP011C ANOP  ,                                                    DBC
         DS    C -            DCBDEVT - DEVICE TYPE
         AIF   (&#DCBSW(011)).SKP011D                               DBC
DCBDVMR  EQU   X'5D' -        1419 MAGNETIC CHARACTER READER
DCBDVORS EQU   X'5F' -        1275 OPTICAL READER SORTER
.SKP011D ANOP  ,                                                    DBC
&P.APPIN DS    C -            AN INDICATOR USED BY THE APPENDAGES TO
*                             PASS INFORMATION ABOUT ONE CHANNEL CHAIN
*                             TO AN APPENDAGE ASSOCIATED WITH ANOTHER
*                             CHANNEL CHAIN
         DS    X -            RESERVED
         SPACE 1
.C4      AIF   (NOT &DSORGTR).C4A                                S22024
&LSW(012) SETB (1)                                                  DBC
*                       3705 LINE TERMINAL                       S22024
         ORG   &P.DCB+8                                          S22024
&P.IPLTX DS    CL8            NAME OF MODULE TO BE USED TO IPL   S22024
*                             THE 3705                           S22024
&P.BCKUP DS    0A             FULL WORD LABEL                    S22024
         DS    BL1            RESERVED                           S22024
&P.BCKUA DS    AL3            ADDRESS OF THE DCB FOR THE         S22024
*                             BACKUP 3705.                       S22024
.C4A     AIF   (NOT (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR   *
               &DSORGXE)).C5
&LSW(013) SETB (1)                                                  DBC
*                       ACCESS METHOD COMMON INTERFACE
         SPACE 1
         ORG   &P.DCB+16
&P.RELB  DS    0F -           SAME AS DCBREL BELOW
&P.KEYLE DS    FL1 -          KEY LENGTH OF DATA SET
&P.DEVT  DS    0C -           DEVICE TYPE
         AIF   (&#DCBSW(13)).SKP13A                                 DBC
DCBDVTRM EQU   X'4F' -        TERMINAL.  (DD CONTAINS TERM=TS)
.SKP13A  ANOP  ,                                                    DBC
&P.REL   DS    FL3 -          NUMBER OF RELATIVE TRACKS OR BLOCKS IN
*                             THIS DATA SET (BDAM)
&P.BUFCB DS    0A -           ADDRESS OF BUFFER POOL CONTROL BLOCK
&P.BUFNO DS    FL1 -          NUMBER OF BUFFERS REQUIRED FOR THIS DATA
*                             SET.  MAY RANGE FROM 0 TO 255.  IF
*                             UNBLOCKED SPANNED RECORDS ARE USED,
*                             NUMBER OF SEGMENT WORK AREAS REQUIRED
*                             FOR THIS DATA SET.
&P.BUFCA DS    AL3 -          ADDRESS OF BUFFER POOL CONTROL BLOCK
&P.BUFL  DS    H -            LENGTH OF BUFFER.  MAY RANGE FROM 0 TO
*                             32,767.
&P       #DSORG                                                     DBC
&P.IOBAD DS    0A -           ADDRESS OF IOB WHEN CHAINED SCHEDULING IS
*                             USED OR FOR 1419/1275
&P.ODEB  DS    0A -           ADDRESS OF OLD DEB
&P.LNP   DS    0FL1 -         3525 PRINTER LINE POSITION COUNTER
&P.QSLM  DS    BL1 -          QSAM LOCATE MODE LOGICAL RECORD INTERFACE
*                             INDICATOR BYTE FOR UPDAT PROCESSING OF
*                             SPANNED RECORDS
         AIF   (&#DCBSW(013)).SKP013A                               DBC
DCB1DVDS EQU   BIT0 -         ONLY ONE DEVICE IS ALLOCATED TO THIS
*                             DATA SET
DCBUPDCM EQU   BIT1 -         UPDATE COMPLETE, FREE OLD DEB
DCBUPDBT EQU   BIT2+BIT3 -    UPDATE BITS
DCBUPDT  EQU   BIT2 -         UPDATE TO TAKE PLACE
DCBNUPD  EQU   BIT2+BIT3 -    NO UPDATE TO TAKE PLACE
DCBSVDEB EQU   BIT3 -         OLD DEB ADDRESS MUST BE SAVED
.SKP013A ANOP  ,                                                    DBC
&P.IOBAA DS    0AL3 -         SAME AS DCBIOBAD ABOVE
&P.ODEBA DS    AL3 -          ADDRESS OF OLD DEB
         ORG   &P.DCB+28                                     ICBI DCB-4
&P.SVCXL DS    0A -           SAME AS DCBSVCXA BELOW         ICBI DCB-4
         DS    X -            RESERVED                       ICBI DCB-4
&P.SVCXA DS    AL3 -          POINTER TO EXIT LIST OF JES    ICBI DCB-4
*                             C.I. INTERFACE CONTROL SVC     ICBI DCB-4
         SPACE 1
*                       FOUNDATION EXTENSION
         SPACE 1
&P.EODAD DS    0A -           SAME AS DCBEODA BELOW
&P.HIARC DS    0BL1 -         HIERARCHY BITS
&P.BFTEK DS    0BL1 -         BUFFERING TECHNIQUE BITS
&P.BFALN DS    BL1 -          BUFFER ALIGNMENT BITS
         AIF   (&#DCBSW(13) OR &#DCBSW(16)).SKP013B                 DBC
DCBH1    EQU   BIT0 -         HIERARCHY 1 MAIN STORAGE - BIT 5 IS ZERO
DCBBFT   EQU   BIT1+BIT2+BIT3 BUFFERING TECHNIQUE
DCBBFTA  EQU   BIT1+BIT2 -    QSAM LOCATE MODE PROCESSING OF SPANNED
*                             RECORDS - OPEN IS TO CONSTRUCT A RECORD
*                             AREA IF IT AUTOMATICALLY CONSTRUCTS
*                             BUFFERS
DCBBFTR  EQU   BIT2 -         FOR BSAM CREATE BDAM PROCESSING OF
*                             UNBLOCKED SPANNED RECORDS - SOFTWARE
*                             TRACK OVERFLOW.  FOR BSAM INPUT
*                             PROCESSING OF UNBLOCKED SPANNED RECORDS
*                             WITH KEYS - RECORD OFFSET PROCESSING.
DCBBFTS  EQU   BIT1 -         SIMPLE BUFFERING - BIT 3 IS ZERO
DCBBFTKR EQU   BIT2 -         UNBLOCKED SPANNED RECORDS - SOFTWARE
*                             TRACK OVERFLOW (BDAM)
DCBBFTE  EQU   BIT3 -         EXCHANGE BUFFERING - BIT 1 IS ZERO
DCBBFTKD EQU   BIT4 -         DYNAMIC BUFFERING (BTAM)
DCBH0    EQU   BIT5 -         HIERARCHY 0 MAIN STORAGE - BIT 0 IS ZERO
DCBBFA   EQU   BIT6+BIT7 -    BUFFER ALIGNMENT
DCBBFAD  EQU   BIT6 -         DOUBLEWORD BOUNDARY
DCBBFAF1 EQU   BIT7 -         FULLWORD NOT A DOUBLEWORD BOUNDARY,
*                             CODED IN DCB MACRO INSTRUCTION
DCBBFAF2 EQU   BIT6+BIT7 -    FULLWORD NOT A DOUBLEWORD BOUNDARY,
*                             CODED IN DCB MACRO INSTRUCTION
.SKP013B ANOP  ,                                                    DBC
&P.EODA  DS    AL3 -          ADDRESS OF A USER-PROVIDED ROUTINE TO
*                             HANDLE END-OF-DATA CONDITIONS
&P.EXLST DS    0A -           ADDRESS OF USER-PROVIDED LIST OF EXITS
&P.RECFM DS    BL1 -          RECORD FORMAT
         AIF   (&#DCBSW(013)).SKP013C                               DBC
DCBRECLA EQU   BIT0+BIT1+BIT2 RECORD LENGTH INDICATOR - ASCII
DCBRECD  EQU   BIT2 -         ASCII VARIABLE RECORD LENGTH
DCBRECL  EQU   BIT0+BIT1 -    RECORD LENGTH INDICATOR
DCBRECF  EQU   BIT0 -         FIXED RECORD LENGTH
DCBRECV  EQU   BIT1 -         VARIABLE RECORD LENGTH
DCBRECU  EQU   BIT0+BIT1 -    UNDEFINED RECORD LENGTH
DCBRECTO EQU   BIT2 -         TRACK OVERFLOW
DCBRECBR EQU   BIT3 -         BLOCKED RECORDS
DCBRECSB EQU   BIT4 -         FOR FIXED LENGTH RECORD FORMAT - STANDARD
*                             BLOCKS.  FOR VARIABLE LENGTH RECORD
*                             FORMAT - SPANNED RECORDS
DCBRECCC EQU   BIT5+BIT6 -    CONTROL CHARACTER INDICATOR
DCBRECCA EQU   BIT5           ASA CONTROL CHARACTER
DCBRECCM EQU   BIT6 -         MACHINE CONTROL CHARACTER
DCBRECC  EQU   X'00' -        NO CONTROL CHARACTER
DCBRECKL EQU   BIT7 -         KEY LENGTH (KEYLEN) WAS SPECIFIED IN DCB
*                             MACRO INSTRUCTION
.SKP013C ANOP  ,                                                    DBC
&P.EXLSA DS    AL3 -          ADDRESS OF USER-PROVIDED LIST OF EXITS
         SPACE 1
.C5      AIF   (NOT &DSORGBX).C5B
&LSW(014) SETB (1)                                                  DBC
         AIF   (&DSORGQX AND (&DSORGIS OR &DSORGDA OR &DSORGQS OR      *
               &DSORGBS OR &DSORGXE)).C5A
&LSW(015) SETB (1)                                                  DBC
*                       BTAM LINE GROUP INTERFACE
         SPACE 1
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR        *
               &DSORGXE).C5A
&LSW(016) SETB (1)                                                  DBC
         ORG   &P.DCB+20
&P.BUFCB DS    0A -           ADDRESS OF BUFFER POOL CONTROL BLOCK
&P.BUFNO DS    FL1 -          NUMBER OF BUFFERS OBTAINED BY OPEN
&P.BUFCA DS    AL3 -          ADDRESS OF BUFFER POOL CONTROL BLOCK
&P.BUFL  DS    H -            BUFFER LENGTH
&P       #DSORG                                                     DBC
&P.IOBAD DS    0A -           BASE FOR ADDRESSING IOB'S (BASE = ADDRESS
*                             OF FIRST IOB MINUS LENGTH OF AN IOB)
         DS    FL1 -          DCBDEVTP - INDEX TO DEVICE ENTRY IN THE
*                             DEVICE I/O DIRECTORY
&P.IOBAA DS    AL3 -          SAME AS DCBIOBAD ABOVE
&P.HIARC DS    0BL1 -         HIERARCHY FLAG BITS
&P.BFTEK DS    BL1 -          BUFFERING TECHNIQUE FLAG BITS
         AIF   (&#DCBSW(13) OR &#DCBSW(16)).SKP016A                 DBC
DCBH1    EQU   BIT0 -         HIERARCHY 1 MAIN STORAGE - BIT 5 IS ZERO
DCBBFT   EQU   BIT1+BIT2+BIT3 BUFFERING TECHNIQUE
DCBBFTA  EQU   BIT1+BIT2 -    QSAM LOCATE MODE PROCESSING OF SPANNED
*                             RECORDS - OPEN IS TO CONSTRUCT A RECORD
*                             AREA IF IT AUTOMATICALLY CONSTRUCTS
*                             BUFFERS
DCBBFTR  EQU   BIT2 -         FOR BSAM CREATE BDAM PROCESSING OF
*                             UNBLOCKED SPANNED RECORDS - SOFTWARE
*                             TRACK OVERFLOW.  FOR BSAM INPUT
*                             PROCESSING OF UNBLOCKED SPANNED RECORDS
DCBBFTS  EQU   BIT1 -         SIMPLE BUFFERING - BIT 3 IS ZERO
DCBBFTKR EQU   BIT2 -         UNBLOCKED SPANNED RECORDS - SOFTWARE
*                             TRACK OVERFLOW (BDAM)
DCBBFTE  EQU   BIT3 -         EXCHANGE BUFFERING - BIT 1 IS ZERO
DCBBFTKD EQU   BIT4 -         DYNAMIC BUFFERING (BTAM)
DCBH0    EQU   BIT5 -         HIERARCHY 0 MAIN STORAGE - BIT 0 IS ZERO
DCBBFA   EQU   BIT6+BIT7 -    BUFFER ALIGNMENT
DCBBFAD  EQU   BIT6 -         DOUBLEWORD BOUNDARY
DCBBFAF1 EQU   BIT7 -         FULLWORD NOT A DOUBLEWORD BOUNDARY,
*                             CODED IN DCB MACRO INSTRUCTION
DCBBFAF2 EQU   BIT6+BIT7 -    FULLWORD NOT A DOUBLEWORD BOUNDARY,
*                             CODED IN DCB MACRO INSTRUCTION
.SKP016A ANOP  ,                                                    DBC
         DS    BL1 -          DCBERROP - ERROR RECOVERY PROCEDURE BITS
         DS    FL1 -          DCBBUFCT - MAX NUMBER OF READ BUFFERS
         DS    X -            RESERVED
&P.EXLST DS    0A -           ADDRESS OF USER-PROVIDED EXIT LIST
         DS    FL1 -          DCBEIOBX - SIZE OF IOB
&P.EXLSA DS    AL3 -          ADDRESS OF USER-PROVIDED EXIT LIST
         SPACE 1
.C5A     ANOP
         ORG   &P.DCB+33
&P.ERROP DS    BL1 -          ERROR RECOVERY PROCEDURE BITS
         AIF   (&#DCBSW(014)).SKP014A                               DBC
DCBERPT  EQU   BIT3 -         ON-LINE TEST FACILITIES TO BE USED
DCBERPC  EQU   BIT4 -         THRESHOLD AND CUMULATIVE ERROR COUNTS TO
*                             BE MAINTAINED
DCBERPW  EQU   BIT5 -         TEXT-WRITE ERRORS TO BE RETRIED
DCBERPR  EQU   BIT6 -         TEXT-READ ERRORS TO BE RETRIED
DCBERPN  EQU   BIT7 -         IF ZERO, BASIC ERP TO BE FOLLOWED ---
*                             IF ONE, NO ERP TO BE FOLLOWED
.SKP014A ANOP  ,                                                    DBC
&P.BUFCT DS    FL1 -          CONTAINS MAXIMUM NUMBER OF BUFFERS TO BE
*                             OBTAINED BY BTAM FOR READ OPERATION
*                             (DYNAMIC BUFFERING ONLY)
         SPACE 1
         AIF   (&DSORGQX OR &DSORGTX).C5B
&LSW(017) SETB (1)                                                  DBC
         ORG   &P.DCB+28
&P.DEVTP DS    FL1 -          INDEX TO DEVICE ENTRY IN THE DEVICE I/O
*                             DIRECTORY
         SPACE 1
         ORG   &P.DCB+36
&P.EIOBX DS    FL1 -          SIZE OF EXTENDED IOB.  SIZE OF AN IOB
*                             ASSOCIATED WITH THIS DCB
         SPACE 1
.C5B     AIF   (NOT &DSORGTX).C5B1
&LSW(018) SETB (1)                                                  DBC
*                       TCAM LINE GROUP INTERFACE
         SPACE 1
         ORG   &P.DCB+20
&P.MHA   DS    0A -           SAME AS DCBMH BELOW
&P.BUFIN DS    0BL1 -         NUMBER OF INPUT BUFFERS (BITS 0-3)
&P.BUFOU DS    BL1 -          NUMBER OF OUTPUT BUFFERS (BITS 4-7)
         AIF   (&#DCBSW(018)).SKP018A                               DBC
DCBBFIN  EQU   BIT0+BIT1+BIT2+BIT3 NUMBER OF BUFFERS ASSIGNED
*                             INITIALLY FOR RECEIVING OPERATIONS, FOR
*                             EACH LINE IN LINE GROUP
DCBBFOUT EQU   BIT4+BIT5+BIT6+BIT7 NUMBER OF BUFFERS ASSIGNED
*                             INITIALLY FOR SENDING OPERATIONS, FOR
*                             EACH LINE IN LINE GROUP
.SKP018A ANOP  ,                                                    DBC
&P.MH    DS    AL3 -          ADDRESS OF MESSAGE HANDLER FOR THIS LINE
*                             GROUP
         DS    FL1 -          DCBINTVL - NUMBER OF SECONDS OF
*                             INVITATION DELAY
&P.PCI   DS    BL1 -          PROGRAM CONTROLLED INTERRUPTION HANDLING
         AIF   (&#DCBSW(018)).SKP018B                               DBC
DCBPCIX1 EQU   BIT0 -         PCI=(X,)                       ICBI DCB-8
DCBPCIX2 EQU   BIT1 -         PCI=(,X)                       ICBI DCB-8
DCBPCIA1 EQU   BIT2 -         PCI=(A,)
DCBPCIA2 EQU   BIT3 -         PCI=(,A)
DCBPCIN1 EQU   BIT4 -         PCI=(N,)
DCBPCIN2 EQU   BIT5 -         PCI=(,N)
DCBPCIR1 EQU   BIT6 -         PCI=(R,)
DCBPCIR2 EQU   BIT7 -         PCI=(,R)
.SKP018B ANOP  ,                                                    DBC
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *
               &DSORGCQ OR &DSORGQS OR &DSORGBS OR &DSORGXE).C5B2
&LSW(019) SETB (1)                                                  DBC
&P       #DSORG                                                     DBC
         AGO   .C5B3
.C5B2    ANOP
&LSW(020) SETB (1)                                                  DBC
         DS    0BL2 -         DCBDSORG - DATA SET ORGANIZATION
         DS    BL1 -          DCBDSRG1 - FIRST BYTE OF DCBDSORG
         DS    BL1 -          DCBDSRG2 - SECOND BYTE OF DCBDSORG
.C5B3    ANOP
&P.BUFMA DS    FL1 -          MAXIMUM NUMBER OF BUFFERS TO BE USED FOR
*                             DATA TRANSFER FOR EACH LINE IN THIS GROUP
         SPACE 1
.C5B1    AIF   (NOT (&DSORGQX OR &DSORGTX)).C6
&LSW(021) SETB (1)                                                  DBC
*                       QTAM LINE GROUP INTERFACE
         SPACE 1
         ORG   &P.DCB+20
&P.CLPS  DS    0A -           ADDRESS OF LINE PROCEDURE SPECIFICATION
*                             ROUTINE
&P.BUFRQ DS    FL1 -          NUMBER OF BUFFERS REQUESTED FOR A READ
*                             OR WRITE OPERATION
&P.CLPSA DS    AL3 -          SAME AS DCBCLPS ABOVE
&P.INTVL DS    FL1 -          NUMBER OF SECONDS OF INTENTIONAL DELAY
*                             BETWEEN PASSES THROUGH A POLLING LIST
*                             FOR NONSWITCHED LINES
         DS    X -            RESERVED
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQS OR        *
               &DSORGBS OR &DSORGXE OR &DSORGTX).C5C
&LSW(022) SETB (1)                                                  DBC
&P       #DSORG                                                     DBC
&P.IOBAD DS    0A -           ADDRESS OF FIRST IOB
&P.DEVTP DS    AL1 -          DEVICE TYPE POINTER
&P.IOBAA DS    AL3 -          ADDRESS OF FIRST IOB
&P.LCBAD DS    0A -           BASE FOR ADDRESSING LCB'S (BASE = ADDRESS
*                             OF FIRST LCB MINUS LENGTH OF ONE LCB)
&P.CPRI  DS    BL1 -          COMMUNICATION PRIORITY BITS
         AIF   (&#DCBSW(22) OR &#DCBSW(23)).SKP022A                 DBC
DCBCPR   EQU   BIT5 -         RECEIVING HAS PRIORITY
DCBCPE   EQU   BIT6 -         RECEIVING AND SENDING HAVE EQUAL PRIORITY
DCBCPS   EQU   BIT7 -         SENDING HAS PRIORITY
.SKP022A ANOP  ,                                                    DBC
&P.LCBA  DS    AL3 -          SAME AS DCBLCBAD ABOVE
&P.EXLST DS    0A -           ADDRESS OF EXIT LIST
&P.EIOBX DS    FL1 -          EXTENDED IOB INDEX.  SIZE OF LCB.
&P.EXLSA DS    AL3 -          ADDRESS OF EXIT LIST
         SPACE 1
         AGO   .C6
.C5C     ANOP
&LSW(023) SETB (1)                                                  DBC
         DS    0BL2 -         DCBDSORG - DATA SET ORGANIZATION
         DS    BL1 -          DCBDSRG1 - FIRST BYTE OF DCBDSORG
         DS    BL1 -          DCBDSRG2 - SECOND BYTE OF DCBDSORG
         AIF   (NOT &DSORGTX).C5C1
&LSW(024) SETB (1)                                                  DBC
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGCQ OR        *
               &DSORGQS OR &DSORGBS OR &DSORGXE).C5C1
&LSW(025) SETB (1)                                                  DBC
&P.IOBAD DS    0A -           ADDRESS OF FIRST IOB
&P.DEVTP DS    AL1 -          DEVICE TYPE POINTER
&P.IOBAA DS    AL3 -          ADDRESS OF FIRST IOB
         SPACE 1
         AGO   .C5C2
.C5C1    ANOP
&LSW(026) SETB (1)                                                  DBC
&P.DEVTP DS    AL1 -          DEVICE TYPE POINTER
         DS    AL3 -          DCBIOBAA - ADDRESS OF FIRST IOB
         SPACE 1
.C5C2    AIF   (NOT &DSORGTX).C5D
&LSW(027) SETB (1)                                                  DBC
         ORG   &P.DCB+32
&P.TRANA DS    0A -           ADDRESS OF TRANSLATION TABLE
         DS    BL1 -          DCBCPRI - COMMUNICATION PRIORITY BITS
&P.TRANS DS    AL3 -          ADDRESS OF TRANSLATION TABLE
         SPACE 1
.C5D     ANOP
         ORG   &P.DCB+32
&P.LCBAD DS    0A -           BASE FOR ADDRESSING LCB'S (BASE = ADDRESS
*                             OF FIRST LCB MINUS LENGTH OF ONE LCB)
&P.CPRI  DS    BL1 -          COMMUNICATION PRIORITY BITS
         AIF   (&#DCBSW(22) OR &#DCBSW(23)).SKP023A                 DBC
DCBCPR   EQU   BIT5 -         RECEIVING HAS PRIORITY
DCBCPE   EQU   BIT6 -         RECEIVING AND SENDING HAVE EQUAL PRIORITY
DCBCPS   EQU   BIT7 -         SENDING HAS PRIORITY
.SKP023A ANOP  ,                                                    DBC
&P.LCBA  DS    AL3 -          SAME AS DCBLCBAD ABOVE
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQS OR        *
               &DSORGBS OR &DSORGXE).C5E
&LSW(028) SETB (1)                                                  DBC
&P.EXLST DS    0A -           ADDRESS OF EXIT LIST
&P.EIOBX DS    FL1 -          EXTENDED IOB INDEX.  SIZE OF LCB
&P.EXLSA DS    AL3 -          ADDRESS OF EXIT LIST
         SPACE 1
         AGO   .C6
.C5E     ANOP
&LSW(029) SETB (1)                                                  DBC
&P.EIOBX DS    FL1 -          EXTENDED IOB INDEX.  SIZE OF LCB.
         DS    AL3 -          DCBEXLSA - ADDRESS OF EXIT LIST
         SPACE 1
.C6      AIF   (NOT (&DSORGMQ OR &DSORGTQ)).C7
&LSW(030) SETB (1)                                                  DBC
*                       QTAM PROBLEM PROGRAM MESSAGE QUEUE INTERFACE
         SPACE 1
         ORG   &P.DCB+20
&P.TRMAD DS    0A -           ADDRESS OF USER-PROVIDED AREA IN WHICH
*                             THE TERMINAL NAME IS STORED
         AIF   (&DSORGQX OR &DSORGTX).C6A
&LSW(031) SETB (1)                                                  DBC
&P.BUFRQ DS    FL1 -          NUMBER OF BUFFERS TO BE FILLED FROM THE
*                             DIRECT ACCESS QUEUE
         AGO   .C6B
.C6A     ANOP
&LSW(032) SETB (1)                                                  DBC
         DS    FL1 -          DCBBUFRQ - NUMBER OF BUFFERS TO BE FILLED
*                             FROM THE DIRECT ACCESS QUEUE
.C6B     ANOP
&P.TRMA  DS    AL3 -          SAME AS DCBTRMAD ABOVE
&P.SOWA  DS    H -            SIZE OF USER-PROVIDED WORK AREA
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *
               &DSORGQS OR &DSORGBS OR &DSORGXE OR &DSORGTX).C6C
&LSW(033) SETB (1)                                                  DBC
&P       #DSORG                                                     DBC
&P.IOBAD DS    0A -           BEFORE OPEN - ADDRESS OF AVT  ---
*                             AFTER OPEN - BASE FOR ADDRESSING IOB'S
*                             (BASE = ADDRESS OF FIRST IOB MINUS LENGTH
*                             OF ONE IOB)
         DS    FL1 -          DCBBUFMA - MAXIMUM NUMBER OF BUFFERS TO
*                             BE USED FOR DATA TRANSFER FOR EACH LINE
*                             IN THIS GROUP
&P.IOBAA DS    AL3 -          SAME AS DCBIOBAD ABOVE
         AGO   .C6D
.C6C     ANOP
&LSW(034) SETB (1)                                                  DBC
         DS    0BL2 -         DCBDSORG - DATA SET ORGANIZATION
         DS    BL1 -          DCBDSRG1 - FIRST BYTE OF DCBDSORG
         DS    BL1 -          DCBDSRG2 - SECOND BYTE OF DCBDSORG
         DS    A -            DCBIOBAD - BASE FOR ADDRESSING IOB'S
.C6D     ANOP
         SPACE 1
         ORG   &P.DCB+28
&P.SEGAD DS    A -            ADDRESS OF CURRENT SEGMENT
         AIF   (NOT &DSORGTQ).C6D1
&LSW(035) SETB (1)                                                  DBC
&P.THRES DS    FL1 -          FOR NON-REUSABLE MESSAGE QUEUE RECORDS,
*                             PERCENTAGE OF NON-REUSABLE DISK MESSAGE
*                             QUEUE RECORDS TO BE USED BEFORE A FLUSH
*                             CLOSEDOWN OF THE SYSTEM IS INITIATED.
*                             FOR REUSABLE MESSAGE QUEUE RECORDS AND
*                             CHECKPOINT RECORDS, THIS FIELD IS
*                             RESERVED
         AGO   .C6D2
.C6D1    ANOP
&LSW(036) SETB (1)                                                  DBC
         DS    X -            RESERVED
.C6D2    ANOP
         SPACE 1
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR        *
               &DSORGXE).C7
&LSW(037) SETB (1)                                                  DBC
         ORG   &P.DCB+32
&P.EODAD DS    A -            ADDRESS OF USER-PROVIDED ROUTINE
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX).C6E
&LSW(038) SETB (1)                                                  DBC
&P.EXLST DS    0A -           ADDRESS OF EXIT LIST
         AGO   .C6F
.C6E     ANOP
&LSW(039) SETB (1)                                                  DBC
         DS    0A -           DCBEXLST - ADDRESS OF EXIT LIST
.C6F     ANOP
&P.RECFM DS    C -            RECORD FORMAT
         AIF   (&#DCBSW(037)).SKP037A                               DBC
DCBRECR  EQU   X'02' -        RECORD
DCBRECG  EQU   X'04' -        MESSAGE
DCBRECS  EQU   X'08' -        SEGMENT
.SKP037A ANOP  ,                                                    DBC
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX).C6G
&LSW(040) SETB (1)                                                  DBC
&P.EXLSA DS    AL3 -          ADDRESS OF EXIT LIST
         AGO   .C7
.C6G     ANOP
&LSW(041) SETB (1)                                                  DBC
         DS    AL3 -          DCBEXLSA - ADDRESS OF EXIT LIST
.C7      ANOP
         SPACE 1
         AIF   (NOT &DSORGCQ OR (&DSORGIS OR &DSORGBX OR &DSORGDA OR   *
               &DSORGQX OR &DSORGQS OR &DSORGBS OR &DSORGXE)).C8
&LSW(042) SETB (1)                                                  DBC
*                       QTAM DIRECT ACCESS MESSAGE QUEUE
         SPACE 1
         ORG   &P.DCB+20
&P.BUFCB DS    0A -           ADDRESS OF TERMINAL TABLE
&P.BUFNO DS    X -            RESERVED
&P.BUFCA DS    AL3 -          ADDRESS OF TERMINAL TABLE
&P.BUFL  DS    H -            SIZE OF THE DATA IN BUFFER EQUATED TO
*                             IECKBUFL
&P       #DSORG                                                     DBC
&P.IOBAD DS    A -            ADDRESS OF IOB
         SPACE 1
.C8      ANOP                                                    S22024
         AIF   (NOT &DSORGTR).C8A7                               S22024
&LSW(043) SETB (1)                                                  DBC
         ORG   &P.DCB+20                                         S22024
&P.DUMPD DS    0A             FULL WORD LABEL                    S22024
&P.UNITN DS    BL1            NUMBER OF UNITS FOR READ FOLLOWING S22024
*                             ATTENTION.                         S22024
&P.DUMPA DS    AL3            ADDRESS OF THE DCB USED FOR        S22024
*                             DUMPING THE 3705                   S22024
         DS    AL1            RESERVED                           S22024
&P.TRSTA DS    BL1            STATUS BYTE. WHEN SET TO 1,        S22024
*                             THE INDICATORS HAVE THE SPECIFIED  S22024
*                             MEANING                            S22024
         AIF   (&#DCBSW(043)).SKP043A                               DBC
DCBAUTOI EQU   BIT0           IPLAUTO=YES WAS SPECIFIED IN THE   S22024
*                             DCB                                S22024
DCBAUTOD EQU   BIT1           DMPAUTO=YES WAS SPECIFIED IN THE   S22024
*                             DCB MACRO.                         S22024
DCBINITL EQU   BIT2           BRINGUP=YES WAS SPECIFIED IN THE   S22024
*                             DCB MACRO.                         S22024
DCBRSTRT EQU   BIT3           RESTART IS IN PROCESS              S22024
DCBIPLED EQU   BIT4           3705 HAS BEEN IPL'D.               S22024
DCBBAKUP EQU   BIT5           BACKUP=YES WAS SPECIFIED IN THE    S22024
*                             DCB MACRO.                         S22024
DCBNIDLE EQU   BIT6           IDLE=NO WAS SPECIFIED IN THE OPEN  S22024
*                             MACRO OR WAS IMPLIED BY DEFAULT    S22024
DCBCHNGL EQU   BIT7           IPL TEXT HAS BEEN CHANGED          S22024
.SKP043A ANOP  ,                                                    DBC
         AIF   (NOT &DSORGTR OR (&DSORGCQ OR &DSORGIS OR &DSORGBX OR   *
               &DSORGDA OR &DSORGQX OR &DSORGQS OR &DSORGBS OR         *
               &DSORGXE OR &DSORGMQ OR &DSORGTQ OR &DSORGTX)).C8A0
&LSW(044) SETB (1)                                                  DBC
&P.DSORG DS    0BL2           DATA SET ORGANIZATION BEING USED   S22024
&P.DSRG1 DS    BL1            FIRST BYTE OF DCBDSORG             S22024
&P.DSRG2 DS    BL1            SECOND BYTE OF DCBDSORG            S22024
         AIF   (&#DCBDSG OR &#DCBSW(44)).SKP044A                    DBC
DCBDSGTR EQU   BIT5           DSORG=TR SPECIFIED                 S22024
&P.IOBAD DS    A              ADDRESS OF IOB                     S22024
.SKP044A ANOP  ,                                                    DBC
         AGO   .C8A1                                             S22024
.C8A0    ANOP                                                    S22024
&LSW(045) SETB (1)                                                  DBC
         DS    H              DCBDSORG                           S22024
         DS    A              DCBIOBAD                           S22024
.C8A1    ANOP                                                    S22024
&P.RNCKD DS    0A             FULL WORD LABEL                    S22024
         DS    BL1            RESERVED                           S22024
&P.RNCKA DS    AL3            ADDRESS OF THE DCB USED TO RETAIN  S22024
*                             INCIDENT CHECKPOINT RECORDS        S22024
*                             GENERATED BY THE 3705.             S22024
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR        *
               &DSORGXE OR &DSORGBX OR &DSORGQX OR &DSORGTX OR         *
               &DSORGMQ OR &DSORGTQ).C8A2                        S22024
&LSW(046) SETB (1)                                                  DBC
&P.EXLST DS    0A             FULL WORD LABEL FOR EXLIST         S22024
         AGO   .C8A3                                             S22024
.C8A2    ANOP                                                    S22024
&LSW(047) SETB (1)                                                  DBC
         DS    0A             DCBEXLST                           S22024
.C8A3    ANOP                                                    S22024
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX).C8A4           S22024
&LSW(048) SETB (1)                                                  DBC
&P.EIOBX DS    BL1            SIZE, IN BYTES, OF THE IOB.        S22024
         AGO   .C8A5                                             S22024
.C8A4    ANOP                                                    S22024
&LSW(049) SETB (1)                                                  DBC
         DS    BL1            DCBEIOBX                           S22024
.C8A5    ANOP                                                    S22024
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX OR &DSORGIS OR        *
               &DSORGDA OR &DSORGQS OR &DSORGBS OR &DSORGXE  OR        *
               &DSORGMQ OR &DSORGTQ).C8A6                        S22024
&LSW(050) SETB (1)                                                  DBC
&P.EXLSA DS    AL3            ADDRESS OF THE EXIT LIST.          S22024
         AGO   .C8A7                                             S22024
.C8A6    ANOP                                                    S22024
&LSW(051) SETB (1)                                                  DBC
         DS    AL3            DCBEXLSA                           S22024
.C8A7    AIF   (&DSORGLR AND NOT (&DSORGIS OR &DSORGBX OR &DSORGDA OR  *
               &DSORGQX OR &DSORGCQ OR &DSORGMQ OR &DSORGXA OR         *
               &DSORGQS OR &DSORGBS OR &DSORGXE OR &DSORGTR)).CF
&LSW(052) SETB (1)                                                  DBC
*                       FOUNDATION BEFORE OPEN
         SPACE 1
         ORG   &P.DCB+40
&P.DDNAM DS    CL8 -          NAME ON THE DD STATEMENT WHICH DEFINES
*                             THE DATA SET ASSOCIATED WITH THIS DCB
&P.OFLGS DS    BL1 -          FLAGS USED BY OPEN ROUTINE
         AIF   (&#DCBSW(052)).SKP052A                               DBC
DCBOFLWR EQU   BIT0 -         IF ZERO, LAST I/O OPERATION WAS READ OR
*                             POINT.  IF ONE, LAST I/O OPERATION WAS
*                             WRITE.
DCBOFIOD EQU   BIT0 -         DATA SET IS BEING OPENED FOR INPUT OR
*                             OUTPUT (BDAM)
DCBOFLRB EQU   BIT1 -         LAST I/O OPERATION WAS IN READ BACKWARD
*                             MODE
         AIF   (&#DCBSW(108)).SKP052A                               DBC
DCBOFEOV EQU   BIT2 -         SET TO 1 BY EOV WHEN IT CALLS CLOSE
*                             ROUTINE FOR CONCATENATION OF DATA SETS
*                             WITH UNLIKE ATTRIBUTES
DCBOFOPN EQU   BIT3 -         AN OPEN HAS BEEN SUCCESSFULLY COMPLETED
DCBOFPPC EQU   BIT4 -         SET TO 1 BY PROBLEM PROGRAM TO INDICATE A
*                             CONCATENATION OF UNLIKE ATTRIBUTES
DCBOFTM  EQU   BIT5 -         TAPE MARK HAS BEEN READ
DCBOFUEX EQU   BIT6 -         SET TO 0 BY AN I/O SUPPORT FUNCTION WHEN
*                             THAT FUNCTION TAKES A USER EXIT. SET TO 1
*                             ON RETURN FROM USER EXIT TO THE I/O
*                             SUPPORT FUNCTION WHICH TOOK THE EXIT.
DCBOFIOF EQU   BIT7 -         SET TO 1 BY AN I/O SUPPORT FUNCTION IF
*                             DCB IS TO BE PROCESSED BY THAT FUNCTION
.SKP052A ANOP  ,                                                    DBC
&P.IFLG  DS    BL1 -          FLAGS USED BY IOS IN COMMUNICATING ERROR
*                             CONDITIONS AND IN DETERMINING CORRECTIVE
*                             PROCEDURES
         AIF   (&#DCBSW(052)).SKP052B                               DBC
DCBIBEC  EQU   BIT0+BIT1 -    ERROR CORRECTION INDICATOR
DCBIFNEP EQU   X'00' -        NOT IN ERROR PROCEDURE
DCBEX    EQU   BIT1           ERROR CORRECTION OR IOS PAGE FIX IN
*                             PROCESS
DCBIFPEC EQU   BIT0+BIT1 -    PERMANENT ERROR CORRECTION
DCBIBPCT EQU   BIT2+BIT3 -    PRINTER CARRIAGE TAPE PUNCH INDICATOR
DCBIFC9  EQU   BIT2 -         CHANNEL 9 PRINTER CARRIAGE TAPE PUNCH
*                             SENSED
DCBIFC12 EQU   BIT3 -         CHANNEL 12 PRINTER CARRIAGE TAPE PUNCH
*                             SENSED
DCBIBIOE EQU   BIT4+BIT5 -    IOS ERROR ROUTINE USE INDICATOR
DCBIFER  EQU   X'00' -        ALWAYS USE I/O SUPERVISOR ERROR ROUTINE
DCBIFNE1 EQU   BIT5 -         NEVER USE I/O SUPERVISOR ERROR ROUTINE
DCBIFTIM EQU   BIT5 -         TEST IOS MASK (IMSK) FOR ERROR PROCEDURE
*                             (BTAM)
DCBIFNE2 EQU   BIT4 -         NEVER USE I/O SUPERVISOR ERROR ROUTINE
DCBIFNE3 EQU   BIT4+BIT5 -    NEVER USE I/O SUPERVISOR ERROR ROUTINE
.SKP052B ANOP  ,                                                    DBC
&P.MACR  DS    0BL2 -         MACRO INSTRUCTION REFERENCE
&P.MACR1 DS    BL1 -          FIRST BYTE OF DCBMACR
         AIF   (&#DCBSW(052)).SKP052E                               DBC
DCBMRECP EQU   BIT0 -         EXECUTE CHANNEL PROGRAM (EXCP) ---
*                             ALWAYS ZERO (BSAM, QSAM, BPAM, BISAM,
*                             QISAM, BDAM) --- RESERVED (QTAM, BTAM)
DCBMRFE  EQU   BIT1 -         FOUNDATION EXTENSION IS PRESENT (EXCP)
DCBMRGET EQU   BIT1 -         GET (QSAM, QISAM, TCAM)
DCBMRPTQ EQU   BIT1 -         PUT FOR MESSAGE GROUP (QTAM) ---
*                             ALWAYS ZERO (BSAM, BPAM, BISAM, BDAM) ---
*                             RESERVED (BTAM)
DCBMRAPG EQU   BIT2 -         APPENDAGES ARE REQUIRED (EXCP)
         AIF   (&#DCBSW(108)).SKP052C                               DBC
DCBMRRD  EQU   BIT2 -         READ (BSAM, BPAM, BISAM, BDAM, BTAM)
.SKP052C ANOP  ,                                                    DBC
DCBMRWRQ EQU   BIT2 -         WRITE FOR LINE GROUP (QTAM) ---
*                             ALWAYS ZERO (QSAM, QISAM)
DCBMRCI  EQU   BIT3 -         COMMON INTERFACE (EXCP)
DCBMRMVG EQU   BIT3 -         MOVE MODE OF GET (QSAM, QISAM)
DCBMRRDK EQU   BIT3 -         KEY SEGMENT WITH READ (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BSAM, BPAM, QTAM, BTAM)
DCBMRLCG EQU   BIT4 -         LOCATE MODE OF GET (QSAM, QISAM)
DCBMRRDI EQU   BIT4 -         ID ARGUMENT WITH READ (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (EXCP, BSAM, BPAM, QTAM, BTAM)
DCBMRABC EQU   BIT5 -         USER'S PROGRAM MAINTAINS ACCURATE BLOCK
*                             COUNT (EXCP)
DCBMRPT1 EQU   BIT5 -         POINT (WHICH IMPLIES NOTE) (BSAM, BPAM)
DCBMRSBG EQU   BIT5 -         SUBSTITUTE MODE OF GET (QSAM)
DCBMRDBF EQU   BIT5 -         DYNAMIC BUFFERING (BISAM, BDAM) ---
*                             ALWAYS ZERO (QISAM) ---
*                             RESERVED (QTAM, BTAM)
DCBPGFXA EQU   BIT6 -         PAGE FIX APPENDAGE IS SPECIFIED (EXCP)
         AIF   (&#DCBSW(108)).SKP052D                               DBC
DCBMRCRL EQU   BIT6 -         CNTRL (BSAM, QSAM)
.SKP052D ANOP  ,                                                    DBC
DCBMRCHK EQU   BIT6 -         CHECK (BISAM)
DCBMRRDX EQU   BIT6 -         READ EXCLUSIVE (BDAM) ---
*                             RESERVED (BPAM, QISAM, QTAM, BTAM)
DCBMRDMG EQU   BIT7 -         DATA MODE OF GET (QSAM)
DCBMRCK  EQU   BIT7 -         CHECK (BDAM) --- RESERVED (EXCP, BSAM,
*                             BPAM, BISAM, QISAM, QTAM, BTAM)
.SKP052E ANOP  ,                                                    DBC
&P.MACR2 DS    BL1 -          SECOND BYTE OF DCBMACR
         AIF   (&#DCBSW(052)).SKP052H                               DBC
DCBMRSTL EQU   BIT0 -         SETL (QISAM) --- ALWAYS ZERO (BSAM, QSAM,
*                             BPAM, BISAM, BDAM) ---
*                             RESERVED (EXCP, QTAM, BTAM)
DCBMRPUT EQU   BIT1 -         PUT (QSAM, TCAM) - PUT OR PUTX (QISAM)
DCBMRGTQ EQU   BIT1 -         GET FOR MESSAGE GROUP (QTAM) ---
*                             ALWAYS ZERO (BSAM, BPAM, BISAM, BDAM) ---
*                             RESERVED (EXCP, BTAM)
         AIF   (&#DCBSW(108)).SKP052F                               DBC
DCBMRWRT EQU   BIT2 -         WRITE (BSAM, BPAM, BISAM, BDAM, BTAM)
.SKP052F ANOP  ,                                                    DBC
DCBMRRDQ EQU   BIT2 -         READ FOR LINE GROUP (QTAM) ---
*                             ALWAYS ZERO (QSAM, QISAM) ---
*                             RESERVED (EXCP)
DCBMRMVP EQU   BIT3 -         MOVE MODE OF PUT (QSAM, QISAM)
DCBMRWRK EQU   BIT3 -         KEY SEGMENT WITH WRITE (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (EXCP, BSAM, BPAM, QTAM, BTAM)
DCBMR5WD EQU   BIT4 -         FIVE-WORD DEVICE INTERFACE (EXCP)
DCBMRLDM EQU   BIT4 -         LOAD MODE BSAM (CREATE BDAM DATA SET)
*                             (BSAM)
DCBMRLCP EQU   BIT4 -         LOCATE MODE OF PUT (QSAM, QISAM)
DCBMRIDW EQU   BIT4 -         ID ARGUMENT WITH WRITE (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BPAM, QTAM, BTAM)
DCBMR4WD EQU   BIT5 -         FOUR-WORD DEVICE INTERFACE (EXCP)
DCBMRPT2 EQU   BIT5 -         POINT (WHICH IMPLIES NOTE) (BSAM, BPAM)
DCBMRTMD EQU   BIT5 -         SUBSTITUTE MODE (QSAM)
DCBMRUIP EQU   BIT5 -         UPDATE IN PLACE (PUTX) (QISAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BDAM, QTAM, BTAM)
DCBMR3WD EQU   BIT6 -         THREE-WORD DEVICE INTERFACE (EXCP)
         AIF   (&#DCBSW(108)).SKP052G                               DBC
DCBMRCTL EQU   BIT6 -         CNTRL (BSAM, QSAM)
.SKP052G ANOP  ,                                                    DBC
DCBMRSTK EQU   BIT6 -         SETL BY KEY (QISAM)
DCBMRAWR EQU   BIT6 -         ADD TYPE OF WRITE (BDAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BPAM, QTAM, BTAM)
DCBMR1WD EQU   BIT7 -         ONE-WORD DEVICE INTERFACE (EXCP)
DCBMRSWA EQU   BIT7 -         USER'S PROGRAM HAS PROVIDED A SEGMENT
*                             WORK AREA POOL (BSAM CREATE BDAM, BDAM)
DCBMRDMD EQU   BIT7 -         DATA MODE (QSAM)
DCBMRSTI EQU   BIT7 -         SETL BY ID (QISAM) ---
*                             ALWAYS ZERO (BISAM) ---
*                             RESERVED (BPAM, QTAM, BTAM)
.SKP052H ANOP  ,                                                    DBC
         SPACE 1
*                       FOUNDATION AFTER OPEN
         SPACE 1
         ORG   &P.DCB+40
&P.TIOT  DS    H -            OFFSET FROM TIOT ORIGIN TO TIOELNGH FIELD
*                             IN TIOT ENTRY FOR DD STATEMENT ASSOCIATED
*                             WITH THIS DCB
&P.MACRF DS    0BL2 -         SAME AS DCBMACR BEFORE OPEN
&P.MACF1 DS    BL1 -          FIRST BYTE OF DCBMACRF
&P.MACF2 DS    BL1 -          SECOND BYTE OF DCBMACRF
&P.DEBAD DS    0A -           ADDRESS OF ASSOCIATED DEB
&P.IFLGS DS    BL1 -          SAME AS DCBIFLG BEFORE OPEN
         AIF   (&#DCBSW(052)).SKP052I                               DBC
DCBIFEC  EQU   BIT0+BIT1 -    ERROR CORRECTION INDICATOR
DCBIFPCT EQU   BIT2+BIT3 -    PRINTER CARRIAGE TAPE PUNCH INDICATOR
DCBIFIOE EQU   BIT4+BIT5 -    IOS ERROR ROUTINE USE INDICATOR
.SKP052I ANOP  ,                                                    DBC
&P.DEBA  DS    AL3 -          ADDRESS OF ASSOCIATED DEB
         SPACE 1
         AIF   (NOT (&DSORGBX OR &DSORGDA OR &DSORGQX OR &DSORGBS)).C8A
&LSW(053) SETB (1)                                                  DBC
         ORG   &P.DCB+48
&P.READ  DS    0A -           ADDRESS OF READ MODULE
&P.WRITE DS    A -            ADDRESS OF WRITE MODULE
         SPACE 1
.C8A     AIF   (NOT (&DSORGIS OR &DSORGQX OR &DSORGMQ OR &DSORGQS OR   *
               &DSORGTR)).C8B                                    S22024
&LSW(054) SETB (1)                                                  DBC
         ORG   &P.DCB+48
&P.GET   DS    0A -           ADDRESS OF GET MODULE
&P.PUT   DS    A -            ADDRESS OF PUT MODULE
         SPACE 1
.C8B     ANOP
         AIF   (NOT (&DSORGTX OR &DSORGTR)).C8B1
&LSW(055) SETB (1)                                                  DBC
*                       TCAM LINE GROUP EXTENSION
*                       3705 EXTENSION
         SPACE 1
         ORG   &P.DCB+48
&P.SCTAB DS    0A -           ADDRESS OF SPECIAL CHARACTERS TABLE (SCT)
         DS    BL1 -          DCBOFLGS - FLAGS USED BY OPEN ROUTINE
&P.SCTAD DS    AL3 -          ADDRESS OF SPECIAL CHARACTERS TABLE (SCT)
&P.ILCT  DS    FL1 -          COUNT OF INVITATION LISTS
&P.UNTCT DS    FL1 -          BEFORE OPEN - NUMERICAL VALUE OF SCT.
*                             AFTER OPEN - COUNT OF UNITS FOR 1 BUFFER.
&P.BUFSI DS    H -            SIZE OF ALL BUFFERS USED FOR THIS LINE
*                             GROUP
         AIF   (NOT &DSORGTX).C8B1                               S22024
&LSW(056) SETB (1)                                                  DBC
&P.RESER DS    0CL4 -         NUMBER OF RESERVED BYTES IN BUFFERS
&P.RESB1 DS    FL1 -          NUMBER OF BYTES RESERVED IN THE BUFFER
*                             RECEIVING FIRST INCOMING SEGMENT OF A
*                             MESSAGE
&P.RESB2 DS    FL1 -          NUMBER OF BYTES RESERVED IN ALL BUFFERS
*                             EXCEPT THE ONE CONTAINING FIRST SEGMENT
*                             OF A MESSAGE
         DS    XL2 -          RESERVED
         SPACE 1
*        THE FOLLOWING 4 BYTES MAY BE REPEATED 'N' TIMES
&P.INVLI DS    0A -           ADDRESS OF INVITATION LIST
&P.INVCI DS    BL1 -          TYPE OF COMMUNICATION INTERFACE FOR 2701
*                             DATA ADAPTER UNIT
         AIF   (&#DCBSW(056)).SKP056A                               DBC
DCBINVB1 EQU   BIT2 -         IF ZERO, UNIT (A,)
*                             IF ONE, UNIT (B,)
DCBINVB2 EQU   BIT4 -         IF ZERO, UNIT (,A)
*                             IF ONE, UNIT (,B)
.SKP056A ANOP  ,                                                    DBC
&P.INVLA DS    AL3 -          ADDRESS OF INVITATION LIST
         SPACE 1
.C8B1    ANOP
         AIF   (NOT (&DSORGXA OR &DSORGXE)).C9
&LSW(057) SETB (1)                                                  DBC
*                       EXCP WITH EXTENSION OR APPENDAGES
         SPACE 1
         ORG   &P.DCB+52
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS).C8C
&LSW(058) SETB (1)                                                  DBC
&P.OPTCD DS    BL1 -          OPTION CODES
         AGO   .C8D
.C8C     ANOP
&LSW(059) SETB (1)                                                  DBC
         DS    BL1 -          DCBOPTCD - OPTION CODES
.C8D     ANOP
         AIF   (&DSORGQS OR &DSORGBS).C8E
&LSW(060) SETB (1)                                                  DBC
         AIF   (&#DCBSW(60) OR &#DCBSW(83)).SKP060A                 DBC
DCBOPTZ  EQU   BIT5 -         MAGNETIC TAPE DEVICES - USE REDUCED ERROR
*                             RECOVERY PROCEDURE (EXCP, BSAM, BPAM,
*                             QSAM)
DCBSRCHD EQU   BIT5 -         USE SEARCH DIRECT, INSTEAD OF SEARCH
*                             PREVIOUS, ON RECORD POSITION SENSING
*                             DEVICE  (EXCP, BSAM, BPAM, QSAM)   ICB217
.SKP060A ANOP  ,                                                    DBC
.C8E     ANOP
         DS    XL7 -          RESERVED
         SPACE 1
         AIF   (NOT &DSORGXA).C9
&LSW(061) SETB (1)                                                  DBC
*                       EXCP APPENDAGE LIST
         SPACE 1
         ORG   &P.DCB+60
&P.EOEA  DS    CL2 -          END OF EXTENT APPENDAGE ID
&P.PCIA  DS    CL2 -          PROGRAM CONTROLLED INTERRUPTION
*                             APPENDAGE ID
&P.SIOA  DS    CL2 -          START I/O APPENDAGE ID
&P.CENDA DS    CL2 -          CHANNEL END APPENDAGE ID
&P.XENDA DS    CL2 -          ABNORMAL END APPENDAGE ID
         DS    XL2 -          RESERVED
         SPACE 1
.C9      AIF   (NOT &DSORGIS).CA
&LSW(062) SETB (1)                                                  DBC
*                       BISAM-QISAM INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.OPTCD DS    BL1 -          OPTION CODES
         AIF   (&DSORGQS OR &DSORGBS).C9A
&LSW(063) SETB (1)                                                  DBC
         AIF   (&#DCBSW(63) OR &#DCBSW(68) OR &#DCBSW(38)).SKP063A  DBC
DCBOPTW  EQU   BIT0 -         WRITE VALIDITY CHECK (DASD)
*                             (BSAM, BPAM, QSAM, ISAM, BDAM)
.SKP063A ANOP  ,                                                    DBC
         AGO   .C9B
.C9A     ANOP
&LSW(064) SETB (1)                                                  DBC
*        BIT0 IS DCBOPTW - SAME AS BSAM
.C9B     ANOP
         AIF   (&#DCBSW(062)).SKP062A                               DBC
DCBOPTUF EQU   BIT1 -         FULL-TRACK INDEX WRITE
DCBOPTM  EQU   BIT2 -         MASTER INDEXES
DCBOPTI  EQU   BIT3 -         INDEPENDENT OVERFLOW AREA
DCBOPTY  EQU   BIT4 -         CYLINDER OVERFLOW AREA
DCBOPTL  EQU   BIT6 -         DELETE OPTION
DCBOPTR  EQU   BIT7 -         REORGANIZATION CRITERIA
.SKP062A ANOP  ,                                                    DBC
&P.MAC   DS    BL1 -          EXTENSION OF DCBMACRF FIELD FOR ISAM
         AIF   (&#DCBSW(062)).SKP062B                               DBC
DCBMACUR EQU   BIT4 -         UPDATE FOR READ
DCBMACUW EQU   BIT5 -         UPDATE TYPE OF WRITE
DCBMACAW EQU   BIT6 -         ADD TYPE OF WRITE
DCBMACRE EQU   BIT7 -         READ EXCLUSIVE
.SKP062B ANOP  ,                                                    DBC
&P.NTM   DS    FL1 -          NUMBER OF TRACKS THAT DETERMINE THE
*                             DEVELOPMENT OF A MASTER INDEX
*                             MAXIMUM PERMISSABLE VALUE - 99
&P.CYLOF DS    FL1 -          NUMBER OF TRACKS TO BE RESERVED ON EACH
*                             PRIME DATA CYLINDER FOR RECORDS THAT
*                             OVERFLOW FROM OTHER TRACKS ON THAT
*                             CYLINDER
&P.SYNAD DS    A -            ADDRESS OF USER'S SYNAD ROUTINE
&P.RKP   DS    H -            RELATIVE POSITION OF FIRST BYTE OF KEY
*                             WITHIN EACH LOGICAL RECORD
&P.BLKSI DS    H -            BLOCK SIZE
&P.LPDT  DS    0BL8 -         FOR RESUME LOAD,THE LAST PRIME DATA
*                             TRACK ON THE LAST PRIME DATA CYLINDER
*                             IN THE FORM MBBCCHHR.          ICBI DCB-5
&P.MSWA  DS    A -            ADDRESS OF MAIN STORAGE WORK AREA FOR USE
*                             BY CONTROL PROGRAM WHEN NEW RECORDS ARE
*                             BEING ADDED TO AN EXISTING DATA SET
&P.SMSI  DS    H -            NUMBER OF BYTES IN AREA RESERVED TO HOLD
*                             HIGHEST LEVEL INDEX
&P.SMSW  DS    H -            NUMBER OF BYTES IN WORK AREA USED BY
*                             CONTROL PROGRAM WHEN NEW RECORDS ARE
*                             BEING ADDED TO DATA SET
&P.MSHI  DS    0A -           ADDRESS OF MAIN STORAGE AREA TO HOLD
*                             HIGHEST LEVEL INDEX
&P.NCP   DS    FL1 -          NUMBER OF COPIES OF READ-WRITE (TYPE K)
*                             CHANNEL PROGRAMS THAT ARE TO BE
*                             ESTABLISHED FOR THIS DCB.  (99 MAXIMUM)
&P.MSHIA DS    AL3 -          SAME AS DCBMSHI ABOVE
&P.SETL  DS    A -            ADDRESS OF SETL MODULE FOR QISAM.
*                             ADDRESS OF CHECK MODULE FOR BISAM
&P.EXCD1 DS    BL1 -          FIRST BYTE IN WHICH EXCEPTIONAL
*                             CONDITIONS DETECTED IN PROCESSING DATA
*                             RECORDS ARE REPORTED TO THE USER
         AIF   (&#DCBSW(062)).SKP062C                               DBC
DCBEXNKY EQU   BIT0 -         LOWER KEY LIMIT NOT FOUND
DCBEXIDA EQU   BIT1 -         INVALID DEVICE ADDRESS FOR LOWER LIMIT
DCBEXNSP EQU   BIT2 -         SPACE NOT FOUND
DCBEXINV EQU   BIT3 -         INVALID REQUEST
DCBEXIER EQU   BIT4 -         UNCORRECTABLE INPUT ERROR
DCBEXOER EQU   BIT5 -         UNCORRECTABLE OUTPUT ERROR
DCBEXBLI EQU   BIT6 -         BLOCK COULD NOT BE REACHED (INPUT)
DCBEXBLU EQU   BIT7 -         BLOCK COULD NOT BE REACHED (UPDATE)
.SKP062C ANOP  ,                                                    DBC
&P.EXCD2 DS    BL1 -          SECOND BYTE IN WHICH EXCEPTIONAL
*                             CONDITIONS DETECTED IN PROCESSING DATA
*                             RECORDS ARE REPORTED TO THE USER
         AIF   (&#DCBSW(062)).SKP062D                               DBC
DCBEXSEQ EQU   BIT0 -         SEQUENCE CHECK
DCBEXDUP EQU   BIT1 -         DUPLICATE RECORD
DCBEXCLD EQU   BIT2 -         DCB CLOSED WHEN ERROR WAS DETECTED
DCBEXOFL EQU   BIT3 -         OVERFLOW RECORD
DCBEXLTH EQU   BIT4 -         FOR PUT - LENGTH FIELD OF RECORD LARGER
*                             THAN LENGTH INDICATED IN DCBLRECL
DCBEXRDE EQU   BIT4 -         READ EXCLUSIVE
.SKP062D ANOP  ,                                                    DBC
&P.LRECL DS    H -            FOR FIXED-LENGTH RECORD FORMATS, LOGICAL
*                             RECORD LENGTH.  FOR VARIABLE-LENGTH
*                             RECORD FORMATS, MAXIMUM LOGICAL RECORD
*                             LENGTH OR AN ACTUAL LOGICAL RECORD LENGTH
*                             CHANGED DYNAMICALLY BY USER WHEN CREATING
*                             THE DATA SET
&P.ESETL DS    A -            ADDRESS OF ESETL ROUTINE IN GET MODULE
&P.LRAN  DS    A -            ADDRESS OF READ-WRITE K MODULE OR
*                             EXCLUSIVE MODULE
&P.LWKN  DS    A -            ADDRESS OF WRITE KN MODULE
&P.RELSE DS    A -            WORK AREA FOR TEMPORARY STORAGE OF
*                             REGISTER CONTENTS
&P.PUTX  DS    A -            WORK AREA FOR TEMPORARY STORAGE OF
*                             REGISTER CONTENTS
&P.RELEX DS    A -            ADDRESS OF READ EXCLUSIVE MODULE
&P.FREED DS    A -            ADDRESS OF DYNAMIC BUFFERING MODULE
&P.HIRTI DS    FL1 -          NUMBER OF INDEX ENTRIES THAT FIT ON A
*                             PRIME DATA TRACK
&P.FTMI2 DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF SECOND LEVEL MASTER INDEX (IN
*                             THE FORM MBBCCHH)
&P.LEMI2 DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE ENTRY IN SECOND LEVEL MASTER INDEX
*                             (IN THE FORM CCHHR)
&P.FTMI3 DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF THIRD LEVEL MASTER INDEX (IN
*                             THE FORM MBBCCHH)
&P.LEMI3 DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE ENTRY IN THIRD LEVEL MASTER INDEX
*                             (IN THE FORM CCHHR)
&P.NLEV  DS    FL1 -          NUMBER OF LEVELS OF INDEX
&P.FIRSH DS    CL3 -          HHR OF FIRST DATA RECORD ON EACH
*                             CYLINDER.  FOR VARIABLE LENGTH RECORD
*                             PROCESSING, R PORTION OF THIS FIELD IS
*                             ALWAYS X'01'.
&P.HMASK DS    CL1 -          BYTE INDICATING 2301 OR NOT
         AIF   (&#DCBSW(062)).SKP062E                               DBC
DCBHMDRM EQU   X'07' -        DEVICE IS 2301 DRUM
DCBHMNDM EQU   X'FF' -        DEVICE IS OTHER THAN 2301 DRUM
.SKP062E ANOP  ,                                                    DBC
&P.LDT   DS    CL2 -          HH IS THE LAST PRIME DATA TRACK ON EACH
*                             CYLINDER
&P.HIRCM DS    CL1 -          HIGHEST POSSIBLE R FOR TRACKS OF THE
*                             CYLINDER AND MASTER INDICES
&P.HIRPD DS    CL1 -          HIGHEST R ON ANY PRIME TRACK IN DATA SET.
*                             FOR VARIABLE-LENGTH RECORDS, THIS
*                             REPRESENTS THE GREATEST NUMBER OF
*                             PHYSICAL RECORDS ON ANY PRIME TRACK IN
*                             THE DATA SET
&P.HIROV DS    CL1 -          FOR FIXED-LENGTH RECORD FORMAT, HIGHEST
*                             POSSIBLE R FOR OVERFLOW DATA TRACKS.  FOR
*                             VARIABLE-LENGTH RECORD FORMAT, UNUSED.
&P.HIRSH DS    CL1 -          FOR FIXED-LENGTH RECORD FORMAT, R OF LAST
*                             DATA RECORD ON A SHARED TRACK, IF
*                             APPLICABLE.  FOR VARIABLE-LENGTH RECORD
*                             FORMAT, UNUSED.
&P.TDC   DS    H -            USER-SUPPLIED NUMBER OF RECORDS TAGGED
*                             FOR DELETION.
&P.NCRHI DS    H -            NUMBER OF STORAGE LOCATIONS NEEDED TO
*                             HOLD THE HIGHEST LEVEL INDEX
&P.RORG3 DS    F -            FOR EACH USE OF DATA SET, NUMBER OF READ
*                             OR WRITE ACCESSES TO AN OVERFLOW RECORD
*                             WHICH IS NOT FIRST IN A CHAIN OF SUCH
*                             RECORDS
&P.NREC  DS    F -            NUMBER OF LOGICAL RECORDS IN PRIME DATA
*                             AREA
&P.ST    DS    BL1 -          STATUS INDICATORS
         AIF   (&#DCBSW(062)).SKP062F                               DBC
DCBSTSSM EQU   BIT0 -         SINGLE SCHEDULE MODE
DCBSTKSQ EQU   BIT1 -         KEY SEQUENCE CHECKING IS TO BE PERFORMED
DCBSTLOD EQU   BIT2 -         LOADING HAS COMPLETED.  SET TO 1 BY CLOSE
*                             ROUTINE AND TO 0 BY FIRST EXECUTION OF
*                             PUT ROUTINE.
DCBSTNCY EQU   BIT3 -         EXTENSION OF DATA SET WILL BEGIN ON NEW
*                             CYLINDER
DCBSTNMC EQU   BIT5 -         FIRST MACRO INSTRUCTION NOT YET RECEIVED
DCBSTLBF EQU   BIT6 -         LAST BLOCK FULL
DCBSTLTF EQU   BIT7 -         LAST TRACK FULL
.SKP062F ANOP  ,                                                    DBC
&P.FTCI  DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF CYLINDER INDEX (IN THE FORM
*                             MBBCCHH).
&P.HIIOV DS    CL1 -          FOR FIXED LENGTH RECORD FORMAT, HIGHEST
*                             POSSIBLE R FOR INDEPENDENT OVERFLOW DATA
*                             TRACKS.  FOR VARIABLE LENGTH RECORD
*                             FORMAT, UNUSED
&P.FTMI1 DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF FIRST LEVEL MASTER INDEX (IN
*                             THE FORM MBBCCHH).
&P.NTHI  DS    FL1 -          NUMBER OF TRACKS OF HIGH-LEVEL INDEX
&P.FTHI  DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST
*                             TRACK OF HIGHEST LEVEL INDEX (IN THE
*                             FORM MBBCCHH).
&P.LPDA  DS    CL8 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             PRIME DATA RECORD IN PRIME DATA AREA
*                             (IN THE FORM MBBCCHHR).
&P.LETI  DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE NORMAL ENTRY OF TRACK INDEX ON
*                             LAST ACTIVE CYLINDER (IN THE FORM CCHHR).
&P.OVDEV DS    CL1 -          DEVICE TYPE FOR INDEPENDENT OVERFLOW
         AIF   (&#DCBSW(062)).SKP062G                               DBC
*        THESE SAME MASKS APPLY TO DCBDEVT FOR ISAM DIRECT ACCESS
DCBDVI11 EQU   X'01' -        2311 DISK DRIVE
DCBDVI01 EQU   X'02' -        2301 PARALLEL DRUM
DCBDVI03 EQU   X'03' -        2303 SERIAL DRUM
DCBDVI02 EQU   X'04' -        2302 DISK STORAGE
DCBDVI21 EQU   X'05' -        2321 DATA CELL DRIVE
DCBDVI14 EQU   X'08' -        2314 DISK STORAGE FACILITY
.SKP062G ANOP  ,                                                    DBC
&P.NBOV  DS    H -            FOR FIXED LENGTH RECORD FORMAT, RESERVED.
*                             FOR VARIABLE LENGTH RECORD FORMAT, IF THE
*                             INDEPENDENT OVERFLOW OPTION IS SELECTED,
*                             CONTAINS, IN BINARY, NUMBER OF BYTES LEFT
*                             ON CURRENT TRACK OF INDEPENDENT OVERFLOW
*                             AREA
&P.LECI  DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE ENTRY IN CYLINDER INDEX (IN THE
*                             FORM CCHHR).
         DS    X -            RESERVED
&P.RORG2 DS    H -            NUMBER OF TRACKS (PARTIALLY OR WHOLLY)
*                             REMAINING IN INDEPENDENT OVERFLOW AREA
&P.LEMI1 DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             ACTIVE ENTRY IN FIRST LEVEL MASTER INDEX
*                             (IN THE FORM CCHHR).
         DS    X -            RESERVED
&P.NOREC DS    H -            NUMBER OF LOGICAL RECORDS IN AN OVERFLOW
*                             AREA
&P.LIOV  DS    CL8 -          DIRECT ACCESS DEVICE ADDRESS OF LAST
*                             AREA (IN THE FORM MBBCCHHR).
&P.RORG1 DS    H -            NUMBER OF CYLINDER OVERFLOW AREAS THAT
*                             ARE FULL
         DS    XL2 -          RESERVED
&P.WKPT1 DS    A -            POINTER TO WORK AREA OR TO CONSTRUCTED
*                             CHANNEL PROGRAM FOR WHICH SPACE IS
*                             OBTAINED BY GETMAIN MACRO INSTRUCTIONS
*                             ISSUED BY OPEN EXECUTORS
&P.WKPT2 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
&P.WKPT3 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
&P.WKPT4 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
&P.WKPT5 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
&P.WKPT6 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1
         SPACE 1
.CA      AIF   (NOT &DSORGDA).CB
&LSW(065) SETB (1)                                                  DBC
*                       BDAM INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.CHECK DS    0A -           ADDRESS OF CHECK MODULE
         AIF   (&DSORGIS).CA1
&LSW(066) SETB (1)                                                  DBC
&P.OPTCD DS    BL1 -          OPTION CODES
         AGO   .CA2
.CA1     ANOP
&LSW(067) SETB (1)                                                  DBC
         DS    BL1 -          DCBOPTCD - OPTION CODES
.CA2     ANOP
         AIF   (&DSORGIS OR &DSORGQS OR &DSORGBS).CA3
&LSW(068) SETB (1)                                                  DBC
         AIF   (&#DCBSW(63) OR &#DCBSW(68) OR &#DCBSW(38)).SKP068A  DBC
DCBOPTW  EQU   BIT0 -         WRITE VALIDITY CHECK (DASD)
*                             (BSAM, BPAM, QSAM, ISAM, BDAM)
.SKP068A ANOP  ,                                                    DBC
         AGO   .CA4
.CA3     ANOP
&LSW(069) SETB (1)                                                  DBC
*        BIT0 IS DCBOPTW - SAME AS BSAM AND ISAM
.CA4     ANOP
         AIF   (&#DCBSW(065)).SKP065A                               DBC
DCBOPTTO EQU   BIT1 -         TRACK OVERFLOW
DCBOPTE  EQU   BIT2 -         EXTENDED SEARCH
DCBOPTF  EQU   BIT3 -         FEEDBACK
DCBOPTA  EQU   BIT4 -         ACTUAL ADDRESSING
DCBOPTDB EQU   BIT5 -         DYNAMIC BUFFERING
DCBOPTRE EQU   BIT6 -         READ EXCLUSIVE
DCBOPTRB EQU   BIT7 -         RELATIVE BLOCK ADDRESSING
.SKP065A ANOP  ,                                                    DBC
&P.CHCKA DS    AL3 -          ADDRESS OF CHECK MODULE
         AIF   (&DSORGIS).CAA
&LSW(070) SETB (1)                                                  DBC
&P.SYNAD DS    A -            ADDRESS OF SYNAD ROUTINE
         DS    XL2 -          RESERVED
&P.BLKSI DS    H -            MAXIMUM BLOCK SIZE
         AGO   .CAB
.CAA     ANOP
&LSW(071) SETB (1)                                                  DBC
         DS    A -            DCBSYNAD - ADDRESS OF SYNAD ROUTINE
         DS    XL2 -          RESERVED
         DS    H -            DCBBLKSI - MAXIMUM BLOCK SIZE
.CAB     ANOP
&P.IOBSQ DS    A -            ADDRESS OF FIRST IOB ON UNSCHEDULED QUEUE
*                             FOR EITHER A WRITE-ADD REQUEST WHEN
*                             ANOTHER WRITE-ADD IS IN PROGRESS OR A
*                             READ-EXCLUSIVE REQUEST WHEN THE
*                             READ-EXCLUSIVE LIST IS FULL
&P.SQND  DS    A -            ADDRESS OF LAST IOB ON UNSCHEDULED QUEUE
&P.IOBUQ DS    A -            ADDRESS OF FIRST IOB ON UNPOSTED QUEUE
&P.UQND  DS    A -            ADDRESS OF LAST JOB ON UNPOSTED QUEUE
*                             THAT IS MAINTAINED BY THE READ EXCLUSIVE
*                             MODULE
         DS    X -            RESERVED
&P.LIMCT DS    FL3 -          NUMBER OF TRACKS OR NUMBER OF RELATIVE
*                             BLOCKS TO BE SEARCHED (EXTENDED SEARCH
*                             OPTION)
&P.XARG  DS    0A -           ADDRESS OF READ EXCLUSIVE LIST
&P.XCNT  DS    FL1 -          NUMBER OF ENTRIES IN READ EXCLUSIVE LIST
&P.XARGA DS    AL3 -          ADDRESS OF READ EXCLUSIVE LIST
&P.DRDX  DS    0A -           ADDRESS OF READ EXCLUSIVE MODULE
&P.MVXNO DS    FL1 -          TOTAL NUMBER OF EXTENTS IN MULTIVOLUME
*                             DATA SET
&P.DRDXA DS    AL3 -          ADDRESS OF READ EXCLUSIVE MODULE
&P.DFOR  DS    A -            ADDRESS OF A FORMAT MODULE
&P.DFBK  DS    A -            ADDRESS OF A FEEDBACK MODULE
&P.DYNB  DS    A -            FOR DYNAMIC BUFFERING, ADDRESS OF DYNAMIC
*                             BUFFER MODULE.  FOR UNBLOCKED SPANNED
*                             RECORDS WITH BFTEK=R SPECIFIED AND NO
*                             DYNAMIC BUFFERING, ADDRESS OF SEGMENT
*                             WORK AREA CONTROL BLOCK
         SPACE 1
.CB      AIF   (NOT &DSORGQX).CC
&LSW(072) SETB (1)                                                  DBC
*                       QTAM INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.KSTAT DS    0CL4 -         FOUR THRESHOLD VALUES FOR ERROR COUNTS
&P.KSTA1 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF
*                             TRANSMISSIONS
&P.KSTA2 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF DATA CHECKS
&P.KSTA3 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF
*                             INTERVENTIONS REQUIRED
&P.KSTA4 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF TIMEOUTS
         SPACE 1
*                       QTAM POLLING LIST ORIGIN
         SPACE 1
&P.CPOLL DS    0A -           A 4-BYTE FIELD FOR EACH POLLING LIST
&P.PLBYT DS    BL1 -          ADAPTER TYPE
         AIF   (&#DCBSW(072)).SKP072A                               DBC
DCBCPWTT EQU   BIT4 -         WTTA
.SKP072A ANOP  ,                                                    DBC
&P.CPOLA DS    AL3 -          ADDRESS OF THE POLLING LIST
         SPACE 1
.CC      AIF   (NOT &DSORGTQ).CC1
&LSW(073) SETB (1)                                                  DBC
*                       TCAM MESSAGE QUEUE INTERFACE
         SPACE 1
         ORG   &P.DCB+52
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS).CC1A
&LSW(074) SETB (1)                                                  DBC
&P.OPTCD DS    BL1 -          OPTION CODES
         AGO   .CC1B
.CC1A    ANOP
&LSW(075) SETB (1)                                                  DBC
         DS    BL1 -          DCBOPTCD - OPTION CODES
.CC1B    ANOP
         AIF   (&#DCBSW(073)).SKP073A                               DBC
DCBOPTWP EQU   BIT0 -         SOURCE OR DESTINATION NAME PRECEDES
*                             MESSAGE (AFTER CONTROL BYTE)
*                             (TCAM PROCESS QUEUE)
DCBOPTUM EQU   BIT1 -         WORK UNIT IS A MESSAGE.  DEFAULT WORK
*                             UNIT IS A RECORD.  (TCAM PROCESS QUEUE)
DCBOPTCB EQU   BIT2 -         CONTROL BYTE PRECEDES WORK UNIT
*                             (TCAM PROCESS QUEUE)
DCBOPTCP EQU   BIT2 -         CHECKPOINT DATA SET
DCBOPTIM EQU   BIT6 -         NON-REUSABLE MESSAGE QUEUE DATA SET
DCBOPTRM EQU   BIT7 -         REUSABLE MESSAGE QUEUE DATA SET
.SKP073A ANOP  ,                                                    DBC
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS).CC1D
&LSW(076) SETB (1)                                                  DBC
*                                                            ICBI DCB-9
         DS    XL9 -           RESERVED                      ICBI DCB-9
&P.BLKSI DS    H -             BLOCK SIZE                    ICBI DCB-9
         AGO   .CC1E                                         ICBI DCB-9
.CC1D    ANOP                                                ICBI DCB-9
&LSW(077) SETB (1)                                                  DBC
         DS    XL11 -         RESERVED
.CC1E    ANOP                                                ICBI DCB-9
         SPACE 1
.CC1     ANOP
         AIF   (NOT &DSORGMQ).CD
&LSW(078) SETB (1)                                                  DBC
*                       QTAM PROBLEM PROGRAM MESSAGE QUEUE INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.RECRD DS    A -            NOT USED BY QTAM
         AIF   (&DSORGIS OR &DSORGDA).CCA
&LSW(079) SETB (1)                                                  DBC
&P.SYNAD DS    A -            ADDRESS OF USER-PROVIDED SYNAD ROUTINE TO
*                             BE ENTERED IF A WORK UNIT IS LONGER THAN
*                             THE WORK AREA PROVIDED FOR INPUT
         AGO   .CCB
.CCA     ANOP
&LSW(080) SETB (1)                                                  DBC
         DS    A -            DCBSYNAD - ADDRESS OF USER-PROVIDED SYNAD
*                             ROUTINE TO BE ENTERED IF A WORK UNIT IS
*                             LONGER THAN THE WORK AREA PROVIDED FOR
*                             INPUT
.CCB     ANOP
&P.EOBLK DS    A -            NOT USED BY QTAM
         SPACE 1
.CD      AIF   (NOT &DSORGBX).CDF
&LSW(081) SETB (1)                                                  DBC
*                       BTAM INTERFACE
         SPACE 1
         ORG   &P.DCB+52
&P.LERB  DS    A -            ADDRESS OF LINE ERROR BLOCK
         SPACE 1
         AIF   (NOT &DEVDBS).CDF
&LSW(082) SETB (1)                                                  DBC
*                       BSC INTERFACE
         SPACE 1
         ORG   &P.DCB+56
&P.XMODE DS    BL1 -          MODE OF TRANSMISSION FOR BINARY
*                             SYNCHRONOUS COMMUNICATION (BSC)
         AIF   (&#DCBSW(082)).SKP082A                               DBC
DCBXMIBC EQU   BIT1 -         INTERMEDIATE BLOCK CHECKING IS TO BE
*                             PERFORMED
DCBXMDA1 EQU   BIT2 -         TRANSMISSION IS THROUGH A 2701 DATA
*                             ADAPTER UNIT DUAL COMMUNICATION
*                             INTERFACE B
DCBXMDA2 EQU   BIT4 -         TRANSMISSION IS IN CODE B FOR A 2701
*                             DATA ADAPTER UNIT DUAL CODE FEATURE
.SKP082A ANOP  ,                                                    DBC
&P.XCODE DS    BL1 -          BSC CONTROL STATION FLAG AND
*                             TRANSMISSION CODE
         AIF   (&#DCBSW(082)).SKP082B                               DBC
DCBXCCSF EQU   BIT0 -         BSC CONTROL STATION FLAG ---
*                             IF ZERO, THIS IS THE CONTROL STATION.
*                             IF ONE, THIS IS THE REMOTE STATION.
DCBXCPTP EQU   BIT1 -         IF PTOP IS SPECIFIED IN SYSGEN PROCEDURE
*                             - SCHEDULE AN ASYNCHRONOUS EXIT TO
*                             INTERFACE RESOLUTION ROUTINE
DCBXCTR1 EQU   BIT2 -         6-BIT TRANSCODE IS BEING USED (BIT 4 IS
*                             ALSO ON)
DCBXCAS1 EQU   BIT3 -         USASCII TRANSMISSION CODE IS BEING USED
*                             (BIT 5 IS ALSO ON)
DCBXCEBC EQU   BIT4+BIT5 -    IF BOTH BITS ARE ZERO, EBCDIC
*                             TRANSMISSION CODE IS BEING USED.
DCBXCTR2 EQU   BIT4 -         6-BIT TRANSCODE IS BEING USED (BIT 2 IS
*                             ALSO ON)
DCBXCAS2 EQU   BIT5 -         USASCII TRANSMISSION CODE IS BEING USED
*                             (BIT 3 IS ALSO ON)
.SKP082B ANOP  ,                                                    DBC
&P.BSRSV DS    CL1 -          DLE CONTROL CHARACTER
&P.BSWBT DS    X -            RESERVED
&P.IRRAD DS    0A -           BEFORE OPEN - IF PTOP IS SPECIFIED IN THE
*                             SYSGEN PROCEDURE, ADDRESS OF INTERFACE
*                             RESOLUTION ROUTINE.
*                             AFTER OPEN, THE FOLLOWING 4 CHARACTERS
*                             OCCUPY THIS SPACE.
&P.BSTSX DS    CL1 -          DLE CONTROL CHARACTER
&P.BSSTX DS    CL1 -          STX CONTROL CHARACTER
&P.BSTEX DS    CL1 -          DLE CONTROL CHARACTER
&P.BSETX DS    CL1 -          ETX CONTROL CHARACTER
&P.BSAK0 DS    CL2 -          ACK-0 CONTROL CHARACTER
&P.BSAK1 DS    CL2 -          ACK-1 CONTROL CHARACTER
&P.BSENQ DS    CL1 -          ENQ CONTROL CHARACTER
&P.BSNAK DS    CL1 -          NAK CONTROL CHARACTER
&P.BSETB DS    CL1 -          ETB CONTROL CHARACTER
&P.BSDLE DS    CL1 -          DLE CONTROL CHARACTER
&P.BSEOT DS    CL1 -          EOT CONTROL CHARACTER
&P.BSSYN DS    CL3 -          SYN, SYN, SYN CONTROL CHARACTERS
&P.BSONL DS    CL2 -          SOH % CONTROL CHARACTERS
&P.BSSAK DS    CL2 -          WACK CONTROL CHARACTERS
&P.BSRVI DS    CL2 -          DLE @ CONTROL CHARACTERS
         DS    XL18 -         RESERVED
         SPACE 1
.CDF     AIF   (NOT (&DSORGQS OR &DSORGBS)).FIN
&LSW(083) SETB (1)                                                  DBC
*                       QSAM-BSAM-BPAM COMMON INTERFACE
         SPACE 1
         ORG   &P.DCB+52
         AIF   (&DSORGDA).CDA1
&LSW(084) SETB (1)                                                  DBC
&P.GERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR GET
&P.PERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT
&P.CHECK DS    0A -           ADDRESS OF CHECK MODULE
         AIF   (&DSORGIS).CDA
&LSW(085) SETB (1)                                                  DBC
&P.OPTCD DS    BL1 -          OPTION CODES
         AGO   .CD2
.CDA1    ANOP
&LSW(086) SETB (1)                                                  DBC
         DS    0A -           DCBGERR, DCBPERR OR DCBCHECK
.CDA     ANOP
&LSW(087) SETB (1)                                                  DBC
         DS    BL1 -          DCBOPTCD - OPTION CODES
.CD2     ANOP
         AIF   (&#DCBSW(083)).SKP083C                               DBC
         AIF   (&#DCBSW(63) OR &#DCBSW(68)).SKP083A                 DBC
DCBOPTW  EQU   BIT0 -         WRITE VALIDITY CHECK (DASD)
*                             (BSAM, BPAM, QSAM, ISAM, BDAM)
.SKP083A ANOP  ,                                                    DBC
DCBOPTU  EQU   BIT1 -         ALLOW DATA CHECK CAUSED BY INVALID
*                             CHARACTER (1403 PRINTER WITH UCS FEATURE)
*                             (BSAM, BPAM, QSAM)
DCBOPTC  EQU   BIT2 -         CHAINED SCHEDULING USING PCI
*                             (BSAM, BPAM, QSAM)
DCBOPTH  EQU   BIT3 -         1287/1288 OPTICAL READER - HOPPER EMPTY
*                             EXIT (BSAM, BPAM)
DCBOPTO  EQU   BIT3 -         1285/1287 OPTICAL READER - ON-LINE
*                             CORRECTION (QSAM)
DCBBCKPT EQU   BIT3 -         CHANNEL-END APPENDAGE IS TO BYPASS DOS
*                             EMBEDDED CHECKPOINT RECORDS ON TAPE
*                             (BSAM, QSAM)                       ICB226
DCBOPTQ  EQU   BIT4 -         TRANSLATION TO OR FROM ASCII
*                             (BSAM, BPAM, QSAM)
         AIF   (&#DCBSW(060)).SKP083B                               DBC
DCBOPTZ  EQU   BIT5 -         MAGNETIC TAPE DEVICES - USE REDUCED ERROR
*                             RECOVERY PROCEDURE (EXCP, BSAM, BPAM,
*                             QSAM)
DCBSRCHD EQU   BIT5 -         USE SEARCH DIRECT, INSTEAD OF SEARCH
*                             PREVIOUS, ON RECORD POSITION SENSING
*                             DEVICE  (EXCP, BSAM, BPAM, QSAM)   ICB217
.SKP083B ANOP  ,                                                    DBC
DCBOPTT  EQU   BIT6 -         USER TOTALING (BSAM, QSAM)
.SKP083C ANOP  ,                                                    DBC
         AIF   (&DSORGDA).CD1
&LSW(088) SETB (1)                                                  DBC
&P.GERRA DS    0AL3 -         ADDRESS OF SYNCHRONIZING ROUTINE FOR GET
&P.PERRA DS    0AL3 -         ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT
&P.CHCKA DS    AL3 -          ADDRESS OF CHECK MODULE
         AGO   .CD3
.CD1     ANOP
&LSW(089) SETB (1)                                                  DBC
         DS    AL3 -          DCBGERRA, DCBPERRA OR DCBCHCKA
.CD3     AIF   (&DSORGIS OR &DSORGDA OR &DSORGMQ).CDB
&LSW(090) SETB (1)                                                  DBC
&P.SYNAD DS    0A -           ADDRESS OF USER-PROVIDED SYNAD ROUTINE
&P.IOBL  DS    FL1 -          IOB LENGTH IN DOUBLE WORDS
&P.SYNA  DS    AL3 -          ADDRESS OF USER-PROVIDED SYNAD ROUTINE
         AGO   .CD4
.CDB     ANOP
&LSW(091) SETB (1)                                                  DBC
         DS    0A -           DCBSYNAD - ADDRESS OF SYNAD ROUTINE
&P.IOBL  DS    FL1 -          IOB LENGTH IN DOUBLE WORDS
         DS    AL3 -          DCBSYNA - ADDRESS OF SYNAD ROUTINE
.CD4     ANOP
&P.FLAG1 DS    0BL1           TCAM APPLICATION PROGRAM FLAGS ICBI DCB-3
*                             (BSAM, BPAM, QSAM)
&P.CIND1 DS    BL1 -          CONDITION INDICATORS
         AIF   (&#DCBSW(083)).SKP083D                               DBC
DCBCNTOV EQU   BIT0 -         DIRECT ACCESS - TRACK OVERFLOW IN USE
*                             (BSAM, BPAM, QSAM)
*                             2540 CARD PUNCH - DATA SET WAS OPENED BUT
*                             NO DATA WAS WRITTEN (QSAM)
DCBSTQCK EQU   BIT0 -         STOP EQUAL QUICK WAS SPECIFIED FOR
*                             APPLICATION PROG. DCBS (TCAM)  ICBI DCB-3
DCBSTFLS EQU   BIT1 -         STOP EQUAL FLUSH WAS SPECIFIED FOR
*                             APPLICATION PROG. DCBS (TCAM)  ICBI DCB-3
DCBCNSRD EQU   BIT1 -         SEARCH DIRECT (BSAM, BPAM, QSAM)
DCBCNEVB EQU   BIT2 -         END OF VOLUME - USED BY EOB ROUTINES
*                             (BSAM, BPAM, QSAM)
DCBCNEVA EQU   BIT3 -         END OF VOLUME - USED BY CHANNEL-END
*                             APPENDAGE ROUTINES (BSAM, BPAM, QSAM)
DCBCNBRM EQU   BIT5 -         BLOCKED RECORD BIT MODIFIED (BSAM,BPAM,
*                             QSAM)                          ICBI DCB-2
DCBCNEXB EQU   BIT7 -         EXCHANGE BUFFERING SUPPORTED (QSAM)
.SKP083D ANOP  ,                                                    DBC
&P.CIND2 DS    BL1 -          CONDITION INDICATORS
         AIF   (&#DCBSW(083)).SKP083E                               DBC
DCBCNSTO EQU   BIT0 -         PARTITIONED DATA SET - STOW HAS BEEN
*                             PERFORMED (BSAM, BPAM, QSAM)
*                             SEQUENTIAL DATA SET - UPDATE (BSAM, BPAM)
DCBCNWR0 EQU   BIT1 -         DIRECT ORGANIZATION DATA SET - LAST I/O
*                             WAS A WRITE RECORD ZERO
*                             (BSAM, BPAM, QSAM)
*                             SEQUENTIAL DATA SET - UPDATE EOF IS
*                             INDICATED (BSAM, BPAM)
DCBCNCLO EQU   BIT2 -         CLOSE IN PROCESS (QSAM)
DCBCNIOE EQU   BIT3 -         PERMANENT I/O ERROR (BSAM, BPAM, QSAM)
DCBCNBFP EQU   BIT4 -         OPEN ACQUIRED BUFFER POOL
*                             (BSAM, BPAM, QSAM)
DCBCNCHS EQU   BIT5 -         CHAINED SCHEDULING BEING SUPPORTED
*                             (BSAM, BPAM, QSAM)
DCBCNFEO EQU   BIT6 -         FEOV BIT (BSAM, BPAM, QSAM)
DCBCNQSM EQU   BIT7 -         ALWAYS ZERO (BSAM, BPAM)
*                             THIS IS A QSAM DCB (QSAM)
.SKP083E ANOP  ,                                                    DBC
         AIF   (&DSORGIS OR &DSORGDA).CDC
&LSW(092) SETB (1)                                                  DBC
&P.BLKSI DS    H -            MAXIMUM BLOCK SIZE
         AGO   .CD7
.CDC     ANOP
&LSW(093) SETB (1)                                                  DBC
         DS    H -            DCBBLKSI - MAXIMUM BLOCK SIZE
.CD7     ANOP
&P.WCPO  DS    AL1 -          OFFSET OF WRITE CHANNEL PROGRAM FROM THE
*                             START OF IOB
&P.WCPL  DS    FL1 -          LENGTH OF WRITE CHANNEL PROGRAM
&P.OFFSR DS    AL1 -          OFFSET OF READ CCW FROM BSAM/BPAM PREFIX
*                             OF IOB
&P.OFFSW DS    AL1 -          OFFSET OF WRITE CCW FROM BSAM/BPAM PREFIX
*                             OF IOB
&P.IOBA  DS    A -            FOR NORMAL SCHEDULING, ADDRESS OF QSAM OR
*                             BSAM/BPAM PREFIX OF IOB.  FOR CHAINED
*                             SCHEDULING, ADDRESS OF ICB.  FOR
*                             1419/1275, ADDRESS OF MAGNETIC INTERRUPT
*                             CONTROL BLOCK (MICB) CURRENTLY BEING
*                             PROCESSED BY READ ROUTINE.  FOR TSO
*                             TERMINAL DATA SET OPENED FOR INPUT AND
*                             FORMAT U, SIMULATED LOW-ORDER FOUR BYTES
*                             OF IOBCSW
         SPACE 1
         ORG   &P.DCB+68                                         ICB354
&P.CICB  DS    0A -           SAME AS DCBCICBA BELOW             ICB354
         DS    X -            DCBNCP  (BSAM,BPAM)                ICB354
&P.CICBA DS    AL3 -          POINTER TO JES C.I.                ICB354
*                             CONTROL BLOCK (CICB)               ICB354
         SPACE 1
         ORG   &P.DCB+80                                     ICBI DCB-4
&P.DIRCT DS    0H -           NUMBER OF BYTES USED IN LAST DIRECTORY
*                             BLOCK (RANGE 0-254)  (BSAM, BPAM)  ICB295
&P.QSWS  DS    0BL1 -         FLAG BYTE                          ICB295
&P.USASI DS    B -            FLAG BYTE FOR ASCII TAPES
         AIF   (&#DCBSW(083)).SKP083F                               DBC
DCBBLBP  EQU   BIT1 -         BLOCK PREFIX IS FOUR BYTE FIELD
*                             CONTAINING BLOCK LENGTH IN UNPACKED
*                             DECIMAL (SPECIFIED BY BUFFER=L).
DCBQADFS EQU   BIT2+BIT3+BIT4 USED TO PERFORM SEQUENCE CHECKING WITH
*                             MULTIPLE FUNCTION SUPPORT FOR 3525
*                             (BSAM, QSAM)
DCBQADF1 EQU   BIT2 -         FIRST BIT OF DCBQADFS
DCBQADF2 EQU   BIT3 -         SECOND BIT OF DCBQADFS
DCBQADF3 EQU   BIT4 -         THIRD BIT OF DCBQADFS
DCBQSTRU EQU   BIT7 -         TRUNC ENTRY POINT ENTERED (QSAM)
.SKP083F ANOP  ,                                                    DBC
&P.BUFOF DS    0FL1 -         BLOCK PREFIX LENGTH (0-99), SPECIFIED BY
*                             BUFOFF=N OR BUFOFF=L
&P.DIRCQ DS    FL1 -          NUMBER OF BYTES USED IN LAST DIRECTORY
*                             BLOCK (RANGE 0-254)  (QSAM)        ICB295
         SPACE 1
         AIF   (NOT &DSORGBS).CE
&LSW(094) SETB (1)                                                  DBC
*                       BSAM-BPAM INTERFACE
         SPACE 1
         ORG   &P.DCB+72
&P.EOBR  DS    0A -           ADDRESS OF END-OF-BLOCK MODULE FOR READ
         AIF   (&DSORGIS).CDD
&LSW(095) SETB (1)                                                  DBC
&P.NCP   DS    FL1 -          NUMBER OF CHANNEL PROGRAMS.
         AGO   .CD8
.CDD     ANOP
&LSW(096) SETB (1)                                                  DBC
         DS    FL1 -          DCBNCP - NUMBER OF CHANNEL PROGRAMS.
.CD8     ANOP
*                             NUMBER OF READ OR WRITE REQUESTS WHICH
*                             MAY BE ISSUED PRIOR TO A CHECK, NUMBER
*                             OF IOB'S GENERATED.  (99 MAXIMUM)
&P.EOBRA DS    AL3 -          ADDRESS OF END-OF-BLOCK MODULE FOR READ
&P.EOBW  DS    A -            ADDRESS OF END-OF-BLOCK MODULE FOR WRITE.
*                             FOR BSAM CREATE BDAM PROCESSING OF
*                             UNBLOCKED SPANNED RECORDS WITH BKTEK=R
*                             SPECIFIED, ADDRESS OF SEGMENT WORK AREA
*                             CONTROL BLOCK
         DS    H -            DCBDIRCT - NUMBER OF BYTES USED IN LAST
*                             DIRECTORY BLOCK  (RANGE 0-254)     ICB295
         AIF   (&DSORGIS).CDE
&LSW(097) SETB (1)                                                  DBC
&P.LRECL DS    H -            LOGICAL RECORD LENGTH
         AGO   .CD9
.CDE     ANOP
&LSW(098) SETB (1)                                                  DBC
         DS    H -            DCBLRECL - LOGICAL RECORD LENGTH
.CD9     ANOP
&P.CNTRL DS    0A -           ADDRESS OF CNTRL MODULE
&P.NOTE  DS    0A -           ADDRESS OF NOTE/POINT MODULE
&P.POINT DS    A -            ADDRESS OF NOTE/POINT MODULE
         SPACE 1
.CE      AIF   (NOT &DSORGQS).FIN
&LSW(099) SETB (1)                                                  DBC
*                       QSAM INTERFACE
         SPACE 1
         AIF   (NOT &DSORGDA).CE1
&LSW(100) SETB (1)                                                  DBC
         ORG   &P.DCB+52
&P.GERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR GET
&P.PERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT
         DS    BL1 -          DCBOPTCD - OPTION CODES
&P.GERRA DS    0AL3 -         ADDRESS OF SYNCHRONIZING ROUTINE FOR GET
&P.PERRA DS    AL3 -          ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT
         SPACE 1
.CE1     ANOP
         ORG   &P.DCB+72
&P.LCCW  DS    0A -           FOR EXCHANGE BUFFERING, ADDRESS OF LAST
*                             CCW IN LIST
&P.EOBAD DS    A -            FOR SIMPLE BUFFERING, ADDRESS OF LAST
*                             BYTE OF CURRENT BUFFER
&P.CCCW  DS    0A -           FOR EXCHANGE BUFFERING, ADDRESS OF
*                             CURRENT OR NEXT CCW
&P.RECAD DS    0A -           ADDRESS OF CURRENT OR NEXT LOGICAL RECORD
&P.RECBT DS    BL1 -          FLAG BYTE
         AIF   (&#DCBSW(099)).SKP099A                               DBC
DCBRCREL EQU   BIT0+BIT1+BIT2+BIT3 RELSE MACRO HAS BEEN ISSUED
*                             (QSAM WITH SIMPLE BUFFERING)
DCBRCTRU EQU   BIT0 -         TRUNC MACRO HAS BEEN ISSUED (QSAM LOCATE
*                             MODE)
DCBRCFGT EQU   BIT1 -         FIRST GET AFTER OPEN (QSAM LOCATE MODE)
.SKP099A ANOP  ,                                                    DBC
&P.RECA  DS    AL3 -          ADDRESS OF CURRENT OR NEXT LOGICAL RECORD
         DS    B -            DCBQSWS - FLAG BYTE                ICB295
         DS    FL1 -          DCBDIRCQ - NUMBER OF BYTES USED IN LAST
*                             DIRECTORY BLOCK (RANGE 0-254)      ICB295
         AIF   (&DSORGIS OR &DSORGBS).CEE
&LSW(101) SETB (1)                                                  DBC
&P.LRECL DS    H -            LOGICAL RECORD LENGTH
         AGO   .CEEA
.CEE     ANOP
&LSW(102) SETB (1)                                                  DBC
         DS    H -            DCBLRECL - LOGICAL RECORD LENGTH
.CEEA    AIF   (&DSORGBS).CEF
&LSW(103) SETB (1)                                                  DBC
&P.CNTRL DS    0A -           ADDRESS OF CNTRL MODULE
         AGO   .CEF1
.CEF     ANOP
&LSW(104) SETB (1)                                                  DBC
         DS    0A -           DCBCNTRL - ADDRESS OF CNTRL MODULE
.CEF1    ANOP
&P.EROPT DS    BL1 -          ERROR OPTION
         AIF   (&#DCBSW(099)).SKP099B                               DBC
DCBERACC EQU   BIT0 -         ACCEPT PERMANENT ERROR
DCBERSKP EQU   BIT1 -         SKIP PERMANENT ERROR
DCBERABE EQU   BIT2 -         ABNORMAL END OF TASK
.SKP099B ANOP  ,                                                    DBC
         AIF   (&DSORGBS).CEF2
&LSW(105) SETB (1)                                                  DBC
&P.CNTRA DS    AL3 -          ADDRESS OF CNTRL MODULE
         AGO   .CEF3
.CEF2    ANOP
&LSW(106) SETB (1)                                                  DBC
         DS    AL3 -          DCBCNTRA - ADDRESS OF CNTRL MODULE
.CEF3    ANOP
         DS    XL2 -          RESERVED
&P.PRECL DS    H -            BLOCK LENGTH, MAXIMUM BLOCK LENGTH OR
*                             DATA LENGTH
&P.EOB   DS    A -            ADDRESS OF END OF BLOCK MODULE
         SPACE 1
.CF      AIF   (&DSORGIS OR &DSORGQS OR &DSORGBS OR NOT &DSORGLR).FIN
&LSW(107) SETB (1)                                                  DBC
         ORG   &P.DCB+82
&P.LRECL DS    H -            LOGICAL RECORD LENGTH
         SPACE 1
         AGO   .FIN
.SLIP    ANOP
&LSW(108) SETB (1)                                                  DBC
*                       GRAPHIC DEVICE INTERFACE
         SPACE 1
&P.DCB   DS    0A 
         DS    XL12 -         RESERVED
&P.BRSA  DS    AL2 -          BUFFER RESTART ADDRESS.  BLANK BEFORE
*                             EXECUTION OF SECOND I/O OPERATION
&P.GTYPE DS    CL1 -          TYPE OF BUFFER MANAGEMENT AND ATTENTION
*                             HANDLING
         AIF   (&#DCBSW(108)).SKP108A                               DBC
DCBGTEXP EQU   X'00' -        EXPRESS
DCBGTBAS EQU   X'01' -        BASIC
.SKP108A ANOP  ,                                                    DBC
         DS    X -            RESERVED
&P.BFRST DS    AL2 -          BLANK BEFORE EXECUTION OF OPEN ROUTINE.
*                             STARTING ADDRESS FOR BUFFER AFTER
*                             EXECUTION OF OPEN ROUTINE
&P.BFRSZ DS    H -            BLANK BEFORE EXECUTION OF OPEN ROUTINE.
*                             SIZE OF BUFFER AFTER EXECUTION OF OPEN
*                             ROUTINE.
         SPACE 1
*                       COMMON INTERFACE
         SPACE 1
         DS    XL6 -          RESERVED
&P       #DSORG                                                     DBC
&P.IOBAD DS    A -            BLANK BEFORE EXECUTION OF OPEN ROUTINE.
*                             ADDRESS OF STANDARD FIELDS OF FIRST IOB
*                             AFTER EXECUTION OF OPEN ROUTINE
         SPACE 1
*                       FOUNDATION EXTENSION
         SPACE 1
&P.POLST DS    0A -           ADDRESS OF AREA WHERE A DCB LIST IS TO BE
*                             CONSTRUCTED FOR POLLING PURPOSES
&P.GNCP  DS    FL1 -          NUMBER OF I/O INSTRUCTIONS TO BE ISSUED
*                             BEFORE A WAIT MACRO INSTRUCTION
&P.POLSA DS    AL3 -          SAME AS DCBPOLST ABOVE
&P.EXLST DS    0A -           ADDRESS OF USER'S EXIT LIST
         DS    X -            RESERVED
&P.EXLSA DS    AL3 -          ADDRESS OF USER'S EXIT LIST
         SPACE 1
*                       FOUNDATION BEFORE OPEN
         SPACE 1
&P.DDNAM DS    CL8 -          8-BYTE NAME FROM DD STATEMENT THAT
*                             DEFINES DATA SET ASSOCIATED WITH THIS DCB
&P.OFLG  DS    BL1 -          FLAGS USED BY OPEN ROUTINE
         AIF   (&#DCBSW(108)).SKP108B                               DBC
DCBOFGRW EQU   BIT0 -         IF ZERO, LAST I/O OPERATION WAS GREAD.
*                             IF ONE, LAST I/O OPERATION WAS GWRITE.
         AIF   (&#DCBSW(052)).SKP108B                               DBC
DCBOFEOV EQU   BIT2 -         SET TO 1 BY EOV WHEN IT CALLS CLOSE
*                             ROUTINE FOR CONCATENATION OF DATA SETS
*                             WITH UNLIKE ATTRIBUTES
DCBOFOPN EQU   BIT3 -         AN OPEN HAS BEEN SUCCESSFULLY COMPLETED
DCBOFPPC EQU   BIT4 -         SET TO 1 BY PROBLEM PROGRAM TO INDICATE A
*                             CONCATENATION OF UNLIKE ATTRIBUTES
DCBOFTM  EQU   BIT5 -         TAPE MARK HAS BEEN READ
DCBOFUEX EQU   BIT6 -         SET TO 0 BY AN I/O SUPPORT FUNCTION WHEN
*                             THAT FUNCTION TAKES A USER EXIT. SET TO 1
*                             ON RETURN FROM USER EXIT TO THE I/O
*                             SUPPORT FUNCTION WHICH TOOK THE EXIT.
DCBOFIOF EQU   BIT7 -         SET TO 1 BY AN I/O SUPPORT FUNCTION IF
*                             DCB IS TO BE PROCESSED BY THAT FUNCTION
.SKP108B ANOP  ,                                                    DBC
&P.IFLG  DS    BL1 -          SET TO ZERO BY GRAPHIC ROUTINES BUT USED
*                             BY IOS IN COMMUNICATING ERROR CONDITIONS
*                             AND IN DETERMINING CORRECTIVE PROCEDURES
&P.MACR  DS    0BL2 -         MACRO INSTRUCTION REFERENCE
&P.MACR1 DS    BL1 -          FIRST BYTE OF DCBMACR
         AIF   (&#DCBSW(52) OR &#DCBSW(108)).SKP108C                DBC
DCBMRRD  EQU   BIT2 -         READ
DCBMRCRL EQU   BIT6 -         CNTRL
.SKP108C ANOP  ,                                                    DBC
&P.MACR2 DS    BL1 -          SECOND BYTE OF DCBMACR
         AIF   (&#DCBSW(52) OR &#DCBSW(108)).SKP108D                DBC
DCBMRWRT EQU   BIT2 -         WRITE
DCBMRCTL EQU   BIT6 -         CNTRL
.SKP108D ANOP  ,                                                    DBC
         SPACE 1
*                       FOUNDATION AFTER OPEN
         SPACE 1
         ORG   &P.DCB+40
&P.TIOT  DS    AL2 -          OFFSET FROM TIOT ORIGIN TO DD ENTRY
*                             ASSOCIATED WITH THIS DCB
&P.MACRF DS    0BL2 -         SAME AS DCBMACR BEFORE OPEN
&P.MACF1 DS    BL1 -          FIRST BYTE OF DCBMACRF
&P.MACF2 DS    BL1 -          SECOND BYTE OF DCBMACRF
&P.DEBAD DS    0A -           ADDRESS OF ASSOCIATED DEB
&P.IFLGS DS    BL1 -          SAME AS DCBIFLG BEFORE OPEN
&P.DEBA  DS    AL3 -          ADDRESS OF ASSOCIATED DEB
&P.GIOCR DS    0A -           ADDRESS OF GRAPHICS I/O CONTROL ROUTINE
&P.OFLGS DS    BL1 -          SAME AS DCBOFLG BEFORE OPEN
&P.GIOCA DS    AL3 -          ADDRESS OF GRAPHICS I/O CONTROL ROUTINE
         SPACE 1
.FIN     ANOP
&A0      SETA  0                                                    DBC
.LP1     AIF   (&A0 EQ 150).EXIT                                    DBC
&A0      SETA  &A0+1                                                DBC
&#DCBSW(&A0) SETB (&#DCBSW(&A0) OR &LSW(&A0))                       DBC
         AGO   .LP1                                                 DBC
.EXIT    ANOP  ,                                                    DBC
         MEND
./ ADD NAME=DCBEXIT
         MACRO
&NFS     DCBEXIT  &BLKSIZE=,&BUFNO=
.*.
.*$MACRO=DCBEXIT    DATE=04/20/78       SOURCE=LDW
.*
.*  THIS MACRO WILL GENERATE A DCB EXIT TO SUPPLY CERTAIN OMITTED DCB
.*       VALUES
.*
.*  UPDATED:  02/15/84 LDW  FIX FOR UNALIGNED MACRO CALL
.*
.*.
         LCLC  &NAME
&NAME    SETC  '&NFS'
         AIF   ('&NAME' NE '').NAMEOK
&NAME    SETC  'IHB&SYSNDX'
.NAMEOK  PUSH  USING
         DROP
         USING &NAME,R15
         AIF   ('&BLKSIZE' EQ '').B1
&NAME    CLC   62(2,R1),=F'0'           BLKSIZE GIVEN?
&NAME    SETC  ''
         AIF   ('&BUFNO' EQ '').A1
         BNE   *+10                     YES - USE IT
         AGO   .A2
.A1      BNER  R14                      YES - USE IT
.A2      MVC   62(2,R1),=Y(&BLKSIZE)    SET DEFAULT
.B1      AIF   ('&BUFNO' EQ '').C1
&NAME    CLI   20(R1),0                 BUFNO GIVEN?
         BNER  R14                      YES - USE IT
         MVI   20(R1),&BUFNO            SET DEFAULT
.C1      BR    R14                      RETURN TO OPEN
         SPACE 2
         POP   USING
         SPACE 3
         MEND
./ ADD NAME=#DIAG
         MACRO
&N       #DIAG &R1,&R3,&D2
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $DIAG TO #DIAG.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - FEBRUARY 3, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING IT
.* MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES A DIAGNOSE MACHINE INSTRUCTION (OPCODE IS
.* X'83'). IF THE THIRD POSITIONAL OPERAND IS PRESENT, THEN AN RS-TYPE
.* INSTRUCTION FORMAT IS USED. IF THE THIRD POSITIONAL OPERAND IS
.* OMITTED, THEN AN RX-TYPE INSTRUCTION FORMAT IS USED.
.*
.*
.*
.* INNER MACROS USED - NONE
.*
         AIF   (K'&D2 EQ 0).RX
&N       LM    &R1,&R3,&D2 GENERATE AN RS-TYPE INSTRUCTION
         AGO   .OPCODE
.RX      ANOP
&N       L     &R1,&R3 GENERATE AN RX-TYPE INSTRUCTION
.OPCODE  ORG   *-4 LOCATE BACK TO CHANGE THE OPCODE
         DC    X'83' GENERATE THE DIAGNOSE OPCODE
         ORG   *+3 RELOCATE FORWARD
         MEND
./ ADD NAME=#DIE
         MACRO
&NME     #DIE  &O,&A,&B,&C,&TYPE=MSG
.*
.*
.*                                                            DBC 12/84
.* LAST CHANGE DATE - DECEMBER 6, 1964                        DBC 12/84
.*                  - ADDED "0H'0'," TO INSURE THAT GENERATED DBC 12/84
.*                    "00DEAD COMMANDS" ARE HWORD ALIGNED.    DBC 12/84
.*                                                            10/84 DBC
.* LAST CHANGE DATE - OCTOBER 25, 1984                        10/84 DBC
.*                  - MINOR CHANGE TO REDUCE THE NUMBER OF    10/84 DBC
.*                    LINES PRINTED BY THE MACRO EXPANSION.   10/84 DBC
.*                    THE OBJECT CODE REMAINS UNCHANGED.      10/84 DBC
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - MARCH 16, 1983
.*                  - COMMENTARY ADJUSTMENTS
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $DIE TO #DIE.
.*
.* LAST CHANGE DATE - JAUNARY 27, 1981
.*                  - THE MACRO HAS BEEN REWRITTEN TO
.*                    GENERATE INLINE TRAPS WHENEVER
.*                    EXTENDED MNEMONICS FOR THE 'BC' AND
.*                    'BCR' INSTRUCTIONS ARE USED. THIS IS
.*                    ACCOMPLISHED BY USING THE INVERSE OF
.*                    OF THE GIVEN BRANCH CONDITION TO
.*                    SKIP AROUND THE INLINE TRAP.
.*                    IN THOSE CASES WHERE AN INLINE TRAP
.*                    IS NOT DESIRED (SUCH AS WITHIN A
.*                    BRANCH VECTOR TABLE), THE
.*                    NON-EXTENDED FORM OF THE 'BC'
.*                    INSTRUCTION CAN BE USED; E.G.:
.*                         #DIE C,8   INSTEAD OF   #DIE Z
.*                  - A UNIQUE NUMBER IS NOW APPENDED TO
.*                    GIVEN MESSAGES.
.*
.* LAST CHANGE DATE - FEBRUARY 12, 1980
.*                  - TYPE=CMD SUPPORT HAS BEEN ADDED. THIS
.*                    PROVIDES A MEANS OF COMMUNICATING
.*                    COMMANDS TO THE "DEBUGGING CONTROLLER"
.*                    PROGRAM.
.*                  - IF NO BRANCH CONDITIONALS ARE GIVEN,
.*                    THEN THE "DEAD" CODE IS NOW GENERATED
.*                    INLINE. BEFORE, AN UNCONDITIONAL BRANCH
.*                    TO A "DEAD" LITTERAL WAS MADE.
.*
.* LAST CHANGE DATE - APRIL 24, 1978
.*                  - THE MACRO HAS BEEN REWRITTED TO
.*                    GENERATE MESSAGE STRINGS (INSTEAD OF
.*                    JUST A SEQUENCE NUMBER) AT THE BRANCH
.*                    ADDRESS. SUCH MESSAGES COULD BE
.*                    REASONABLY DISPLAYED VIA AN ESTAE
.*                    ROUTINE.
.*
.* LAST CHANGE DATE - DECEMBER 16, 1977
.*                  - 2ND AND 3RD OPERANDS HAVE BEEN ADDED TO
.*                    THE MACRO PROTOTYPE SO AS TO SUPPORT
.*                    THE GENERATION OF BRANCH INSTRUCTIONS
.*                    OTHER THAN JUST CONDITIONAL-BRANCH.
.*                  - WHEN NO OPERANDS ARE GIVEN, AN
.*                    UNCONDITIONAL BRANCH IS NOW GENERATED.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - SEPTEMBER 10, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS
.* CONCERNING IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
         GBLA  &#DIECOD
         GBLC  &#EBCDIC(256)
         LCLA  &L,&M
         LCLC  &MSG,&LAB,&OP,&N,&#
         AIF   ('&TYPE' EQ 'MSG' OR '&TYPE' EQ 'CMD').TYPEOK
         MNOTE 8,'TYPE=&TYPE NOT RECOGNIZED. TYPE=CMD ASSUMED.'
.TYPEOK  ANOP
&N       SETC  '&NME'
&#DIECOD SETA  &#DIECOD+1
&#       SETC  '000&#DIECOD'
&#       SETC  '&#'(K'&#-3,4)
&M       SETA  N'&SYSLIST+1
.MLP     ANOP
&M       SETA  &M-1
         AIF   (&M EQ 0).NOMSG
         AIF   (K'&SYSLIST(&M) EQ 0).MLP
         AIF   ('&SYSLIST(&M)' EQ '''''').NOMSG
&L       SETA  K'&SYSLIST(&M)-2
         AIF   (&L LE 0).NOMSG2
         AIF   ('&SYSLIST(&M)'(1,1) NE '''').NOMSG2
&MSG     SETC  '&SYSLIST(&M)'(2,&L)
         AIF   ('&TYPE' NE 'MSG').MSGDONE
&MSG     SETC  '&#DIECOD &MSG'
&L       SETA  &L+1+K'&#DIECOD
         AGO   .COMN
.*
.NOMSG2  ANOP
&M       SETA  &M+1
.NOMSG   AIF   ('&TYPE' NE 'MSG').MSGDONE
&MSG     SETC  '&#DIECOD'
&L       SETA  K'&MSG
.*
.COMN    AIF   (&L/2*2 EQ &L).MSGDONE
&L       SETA  &L+1
&MSG     SETC  '&MSG '
.MSGDONE ANOP
.*
         AIF   (&M LE 1 OR '&O' EQ '').INLINE
         AIF   (&M EQ 2).INVERT
         AIF   (&M NE 3).GENLIT
         AIF   ('&O'(K'&O,1) NE 'R').GENLIT
.*
.INVERT  ANOP
&OP      SETC  'BN&O'
         AIF   ('&O'(1,1) NE 'N').NVRTDON
&OP      SETC  'B'.'&O'(2,K'&O-1)
.NVRTDON ANOP
.*
         AIF   (&M EQ 2).SETLAB
&N       &OP   &A                  BRANCH IF OK
&N       SETC  ''
         AGO   .INLINE
.SETLAB  ANOP
&LAB     SETC  'DIE&#.Z'
&N       &OP   &LAB                SKIP IF OK
&N       SETC  ''
.INLINE  ANOP
.*                                                            10/84 DBC
         AIF   ('&TYPE' EQ 'MSG').INLMSG
&N       DC    0H'0',X'00DEAD0100',C'&MSG',X'00'
         AGO   .TRAIL
.INLMSG  ANOP                                                 10/84 DBC
&N       DC    0H'0',X'00DEAD',AL1(&L),C'&MSG'
.*                                                            10/84 DBC
.TRAIL   AIF   ('&LAB' EQ '').MEND
&LAB     DS    0H                  RECEIVE SKIP AROUND TRAP
         AGO   .MEND
.*
.GENLIT  AIF   ('&TYPE' EQ 'MSG').LITMSG
&MSG     SETC  '
         AGO   .GOTLIT
.LITMSG  #TEST GEN=EBCDIC
&MSG     SETC  '
.GOTLIT  ANOP
.*
         AIF   (&M NE 3).OP4
&N       B&O   &A,=C'&MSG'
         AGO   .MEND
.OP4     ANOP
&N       B&O   &A,&B,=C'&MSG'
.MEND    MEND
./ ADD NAME=#DROP
         MACRO
         #DROP  &D
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $DROP TO #DROP.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - AUGUST 24, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES A DROP INSTRUCTION THAT RELEASES ALL BASES
.* DECLARED BY A PRIOR #ENTER MACRO EXPANSION.
.*
.*
.*
.* INNER MACROS USED - NONE
.*
         GBLC  &#BS(14)
         LCLA  &W1
         AIF   ('&#BS(14)' EQ '').END
&W1      SETA  14
.LP2     AIF   (&W1 EQ 1).END
&W1      SETA  &W1-1
         AIF   ('&#BS(&W1)' EQ '').LP2
         DROP  &#BS(&W1)
         AGO   .LP2
.END     MEND
./ ADD NAME=#DSA
         MACRO
&NME     #DSA  &D
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $DSA TO #DSA.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - JANUARY 27, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES A DSECT FOR A STANDARD REGISTER SAVE AREA.
.* FREQUENTLY, A REENTRANT PROGRAM NEEDS TO GETMAIN A "DATA STORAGE
.* AREA" AND USE THE BEGINNING OF IT AS A REGISTER SAVE AREA. IN THIS
.* SITUATION, THIS MACRO CAN BE USED TO MAP THE BEGINNING OF THE DSA.
.* THE REMAINDER OF THE DSA, OF COURSE, WOULD HAVE TO BE MAPPED
.* MANUALLY.
.*
.*   IF THE NAME FIELD ON THE MACRO CALLING STATEMENT IS OMITTED, THEN
.* THE GENERATED DSECT IS NAMED "DSA" AND ALL GENERATED FIELD NAMES ARE
.* PREFIXED WITH "DSA". IF THE NAME FIELD IS USED, THEN IT INSTEAD OF
.* "DSA" IS USED IN THE ABOVE MANNER.
.*
.*   INNER MACROS USED - NONE
.*
.*
.*
         LCLA  &A1
         LCLC  &N
&N       SETC  'DSA'
         AIF   (K'&NME EQ 0).GOTNME
&N       SETC  '&NME'
.GOTNME  ANOP
&N       DSECT  ,                  STANDARD REGISTER SAVE AREA
&N.WD1   DS    A -                 WORD-1 (USED BY PL/1)
&N.HSA   DS    A -                 HIGHER SAVE AREA POINTER
&N.LSA   DS    A -                 LOWER SAVE AREA POINTER
&N.R14   DS    A -                 REGISTER SAVE AREA (RETURN ADDRESS)
&N.R15   DS    A -                 REGISTER SAVE AREA (ENTRY POINT)
&A1      SETA  0-1
.LP      AIF   (&A1 EQ 12).END
&A1      SETA  &A1+1
&N.R&A1  DS    A -                 REGISTER SAVE AREA
         AGO   .LP
.END     MEND
./ ADD NAME=#DSORG
         MACRO
&N       #DSORG &D
.*
.*
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $DSORG TO #DSORG.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - APRIL 1, 1975
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       809 WHITNEY AVE.
.*       NEW HAVEN, CT. 06511
.*
.*
.*
.*   THIS MACRO WAS WRITTEN TO BE AN INNER MACRO FOR THE #DCBD MACRO.
.* ITS SOLE PURPOSE IS TO GENERATE DSORG FIELD AND BIT NAMES FOR THE
.* #DCBD MACRO.
.*
.*
.*
.* INNER MACROS USED - NONE
.*
         GBLB  &#DCBDSG
         GBLB  &#DCBSW(150)
         LCLC  &P
&P       SETC  'DCB'
         AIF   ('&N' EQ '').GOTPFIX
&P       SETC  '&N'
.GOTPFIX ANOP
&P.DSORG DS    0BL2 -         DATA SET ORGANIZATION BEING USED
&P.DSRG1 DS    BL1 -          FIRST BYTE OF DCBDSORG
         AIF   (&#DCBDSG).SKP1
DCBDSGIS EQU   BIT0 -         IS - INDEXED SEQUENTIAL ORGANIZATION
DCBDSGPS EQU   BIT1 -         PS - SHYSICAL SEQUENTIAL ORGANIZATION
DCBDSGDA EQU   BIT2 -         DA - DIRECT ORGANIZATION
DCBDSGCX EQU   BIT3 -         CX - BTAM OR QTAM LINE GROUP
DCBDSGCQ EQU   BIT4 -         CQ - QTAM DIRECT ACCESS MESSAGE QUEUE
DCBDSGMQ EQU   BIT5 -         MQ - QTAM PROBLEM PROGRAM MESSAGE QUEUE
DCBDSGPO EQU   BIT6 -         PO - PARTITIONED ORGANIZATION
DCBDSGU  EQU   BIT7 -         U  - UNMOVABLE, THE DATA CONTAINS
*                                  LOCATION DEPENDENT INFORMATION
.SKP1    ANOP  ,
&P.DSRG2 DS    BL1 -          SECOND BYTE OF DCBDSORG
         AIF   (&#DCBDSG).SKP2
DCBDSGGS EQU   BIT0 -         GS - GRAPHICS ORGANIZATION
DCBDSGTX EQU   BIT1 -         TX - TCAM LINE GROUP
DCBDSGTQ EQU   BIT2 -         TQ - TCAM MESSAGE QUEUE
DCBACBM  EQU   BIT4 -         ACCESS METHOD CONTROL BLOCK   ICBI DCB-1
         AIF   (&#DCBSW(44)).SKP2
&#DCBSW(44) SETB (1)
DCBDSGTR EQU   BIT5 -         TR - TCAM 3705                    S22024
.SKP2    ANOP
&#DCBDSG SETB  (1)
         MEND
./ ADD NAME=#ENTER
         MACRO
&N       #ENTER &NME,&ESDTYPE=DEFAULT,&BASES=1,&SAVTYPE=LOCAL,&PFIX=
.*
.*
.*                                                            08/85 DBC
.* LAST CHANGE DATE - AUGUST 12, 1985                         08/85 DBC
.*                  - THIS MACRO NO LONGER ASSUMES THAT A     08/85 DBC
.*                    SATISFIED UNCONDITIONAL GETMAIN         08/85 DBC
.*                    REQUEST SETS R15 TO ZERO. ONE CUSTOMER  08/85 DBC
.*                    ACTUALLY HAD A MODIFIED GETMAIN SVC     08/85 DBC
.*                    ROUTINE THAT INSURED THAT R15 WAS       08/85 DBC
.*                    UNCHANGED BY GETMAIN. THIS LED TO       08/85 DBC
.*                    DISASTEROUS RESULTS WHEN HE ATTEMPTED   08/85 DBC
.*                    TO USE THIS MACRO IN THAT               08/85 DBC
.*                    ENVIRONMENT. NOW, CHANGING MY MACRO     08/85 DBC
.*                    WAS A LOT SIMPLER THAN CHANGING HIS     08/85 DBC
.*                    ENVIRONMENT, AND THERE WAS A LOT OF     08/85 DBC
.*                    MONEY AT STAKE, SO ...                  08/85 DBC
.*                                                            08/84 DBC
.* LAST CHANGE DATE - AUGUST 8, 1984                          08/84 DBC
.*                  - WHEN BASES=* IS SPECIFIED, #ENTER NOW   08/84 DBC
.*                    DERIVES THE DESIRED BASE ADDRESS BY     08/84 DBC
.*                    SUBTRACTING AN OFFSET FROM THE          08/84 DBC
.*                    CURRENT ENTRY ADDRESS. PREVIOUSLY, IT   08/84 DBC
.*                    WAS JUST LOADING AN ADCON FOR THE       08/84 DBC
.*                    DESIRED BASE ADDRESS. THIS CAUSED       08/84 DBC
.*                    PROBLEMS IF THE #ENTER MACRO WAS        08/84 DBC
.*                    LOCATED WITHIN DYNAMICALLY RELOCATED    08/84 DBC
.*                    CODE.                                   08/84 DBC
.*                  - SIMILARLY, WHEN SAVTYPE=(REMOTE,NME%)   08/84 DBC
.*                    IS SPECIFIED, THE DESIRED SAVE AREA     08/84 DBC
.*                    IS LOCATED BY ADDING AN OFFSET (WHICH   08/84 DBC
.*                    MAY BE NEGATIVE) TO THE CURRENT ENTRY   08/84 DBC
.*                    ADDRESS.                                08/84 DBC
.*                                                            06/84 DBC
.* LAST CHANGE DATE - JUNE 11, 1984                           06/84 DBC
.*                  - ADDED "SAVTYPE=NONE" SUPPORT.           06/84 DBC
.*                  - WHEN A REMOTE SAVE AREA WAS USED,       06/84 DBC
.*                    #ENTER USE TO GENERATE A "USING"        06/84 DBC
.*                    STATEMENT DECLARING R13 AS A BASE FOR   06/84 DBC
.*                    THAT SAVE AREA. THAT "USING" STATEMENT  06/84 DBC
.*                    IS NO LONGER GENERATED.                 06/84 DBC
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 27, 1983
.*                  - MAILING ADDRESS CHANGE.
.*                  - USE OF IBM'S "SAVE" MACRO HAS BEEN
.*                    REPLACED BY LOCAL CODE.
.*                  - THE ASSEMBLY DATE AND TIME ARE NOW
.*                    INCLUDED IN THE MODULE IDENTIFIER
.*                    TEXT.
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $ENTER TO #ENTER.
.*
.* LAST CHANGE DATE - APRIL 15, 1981
.*                  - ADDED ENTRY LINKAGE FOR A PLI ENVIRONMENT.
.*                  - ADDED "#REGS GEN=NO" SUPPORT.
.*
.* LAST CHANGE DATE - JULY 18, 1980
.*                  - BUG FIXED: THE PRECEEDING MODIFICATION INTRODUCED
.*                    AN ERROR WHICH UNDER CERTAIN CIRCUMSTANCES
.*                    GENERATED ASSEMBLY ERRORS.
.*
.* LAST CHANGE DATE - JULY 10, 1980
.*                  - FOR GETMAINED REENTRANT SAVE AREAS, CODE HAS BEEN
.*                    ADDED TO CLEAR THE ENTIRE GETMAINED AREA TO ZEROS
.*                    BEFORE SETING THE CHAIN FIELD.
.*                  - INDIRECT ADDRESSING TO A REMOTE SAVE AREA IS NOW
.*                    SIGNALLED BY A TRAILING PERCENT SIGN RATHER THAN
.*                    A LEADING PERCENT SIGN.
.*
.* LAST CHANGE DATE - OCTOBER 3, 1979
.*                  - CODE HAS BEEN ALTERED SO THAT ADDRESSABILITY TO
.*                    A REMOTE SAVE AREA DOES NOT HAVE TO BE BASED ON
.*                    R15 (I.E., ON THE ENTRY ADDRESS).
.*
.* LAST CHANGE DATE - OCTOBER 3, 1978
.*                  - THE GETMAIN FOR THE RENTRANT SAVE AREA HAS BEEN
.*                    CHANGED SO THAT MORE THAN 4K BYTES CAN BE GOTTEN.
.*
.* LAST CHANGE DATE - FEBRUARY 28, 1978
.*                  - BUG FIXED IN REMOTE SAVE AREA HANDLING
.*
.* LAST CHANGE DATE - JANUARY 29, 1978
.*                  - IN MOST CASES IT IS LOGICALLY INCONSISTANT TO
.*                    CODE 'BASES=*' WHEN ONE OF THE OLD BASES IS R13.
.*                    THIS PROBLEM IS NOW RECOGNIZED AND FLAGGED.
.*
.*                  - A REMOTE SAVE AREA'S NAME CAN NOW BE GIVEN EITHER
.*                    WITH OR WITHOUT A PRECEEDING PERCENT (%) SIGN TO
.*                    INDICATE WHETHER THE NAMED ADDRESS MUST BE
.*                    REACHED BY INDIRECT ADDRESSING.
.*
.* LAST CHANGE DATE - NOVEMBER 4, 1977
.*                  - SUPPORT IS ADDED FOR DEFINING A LOCAL SAVE AREA
.*                    WHOSE LENGTH IS OTHER THAN 72 BYTES.
.*
.* LAST CHANGE DATE - JANUARY 13, 1977
.*                  - THE MF= AND SVID= OPERANDS ARE REPLACED BY THE
.*                    SAVTYPE= OPERAND.
.*                  - SUPPORT FOR THE HANDLING OF A REMOTELY ASSEMBLED
.*                    SAVE AREA.
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - SEPTEMBER 14, 1976
.*                  - IMPLEMENT SUPPORT FOR "BASES=*" WHICH IMPLIES
.*                    THAT BOTH THE BASE ADDRESS AND BASE REGISTERS
.*                    DEFINED BY THE PHYSICALLY PREVIOUS USE OF THE
.*                    #ENTER MACRO ARE TO BE REUSED.
.*
.* LAST CHANGE DATE - AUGUST 23, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES OS STANDARD ENTRY LINKAGE. IT WAS WRITTEN TO
.* PROVIDE A SIMPLE, FLEXIBLE, AND COMPLETE METHOD OF GENERATING SUCH
.* LINKAGE. IN ADDITION, FOR ALMOST ALL OPERAND COMBINATIONS, #ENTER
.* WILL GENERATE THE ABSOLUTE MINIMUM AMOUNT OF CODE NECESSARY.
.*
.*   THE #ENTER MACRO WILL GENERATE THE FOLLOWING:
.*     - A CSECT OR ENTRY CARD (IF DESIRED)
.*     - A MODULE IDENTIFIER WHICH WILL INCLUDE THE ASSEMBLY
.*       DATE AND TIME.
.*     - CODE TO SAVE ALL REGISTERS IN THE HIGHER SAVE AREA
.*     - CODE TO LOAD R13 WITH A POINTER TO A LOWER SAVE AREA
.*     - CODE TO CROSS LINK THE LOWER SAVE AREA WITH THE HIGHER SAVE
.*       AREA
.*     - CODE TO LOAD ANY SET OF BASE REGISTERS
.*     - A USING STATEMENT DECLARING THE SET OF BASE REGISTERS
.*     - EITHER THE LOWER SAVE AREA ITSELF OR CODE TO GETMAIN A
.*       REENTRANT SAVE AREA OF 72 OR MORE BYTES OR CODE TO LOAD THE
.*       ADDRESS OF AN ASSEMBLED SAVE AREA THAT IS REMOTE FROM THE
.*       MACRO EXPANSION.
.*     - FOR A GETMAINED REENTRANT SAVE AREA, CODE TO CLEAR THE AREA TO
.*       ZEROS.
.*
.* &N
.*       THIS IS THE ONLY FIELD REQUIRED FOR THE MACRO CALL. IT MUST
.*       SPECIFY THE DESIRED CONTROL SECTION OR ENTRY NAME.
.*
.* &NME
.*       USE THIS FIELD TO SPECIFY OPTIONAL TEXT TO BE
.*       INCLUDED INTO THE MODULE IDENTIFIER. ENCLOSING
.*       QUOTES ARE OPTIONAL.
.*
.* &ESDTYPE=
.*       THIS OPERAND CONTROLS THE TYPE OF EXTERNAL SYMBOL (IF ANY) TO
.*       BE GENERATED USING &N. VALID VALUES FOR THIS OPERAND IMPLY THE
.*       FOLLOWING:
.*              -OMITTED-    ==> ESDTYPE=ENTRY IF SAVTYPE=PLI
.*              -OMITTED-    ==> ESDTYPE=CSECT OTHERWISE
.*             ESDTYPE=CSECT ==>
.*                     &N       CSECT
.*             ESDTYPE=START ==>
.*                     &N       START
.*             ESDTYPE=ENTRY ==>
.*                              ENTRY &N
.*                     &N       DS    0H
.*             ESDTYPE=     <==>
.*             ESDTYPE=NONE  ==>
.*                     &N       DS    0H
.*
.* &BASES=
.*       USE THIS OPERAND TO SPECIFY EITHER HOW MANY OR EXACTLY WHICH
.*       BASE REGISTERS TO DECLARE AND LOAD. VALID VALUES FOR THIS
.*       OPERAND ARE:
.*             BASES= -A SINGLE SELF DEFINING NUMERIC-
.*                   THIS REQUESTS THAT A SPECIFIC NUMBER OF BASES BE
.*                   LOADED AND DECLARED. THE MACRO IS ALLOWED TO
.*                   DETERMINE FOR ITSELF PRECISELY WHICH REGISERS TO
.*                   DECLARE AS FOLLOWS. FOR SAVTYPE=LOCAL (SEE BELOW)
.*                   THE FIRST BASE REGISTER WILL BE R13; OTHERWISE
.*                   (I.E. FOR SAVTYPE=RENT OR SAVTYPE=REMOTE), THE
.*                   FIRST BASE REGISTER WILL BE R12. IN EITHER CASE,
.*                   ADDITIONAL BASES WILL BE SUCCESSIVELY LOWER
.*                   NUMBERED REGISTERS. EXAMPLES:
.*                   BASES=3,SAVTYPE=RENT  ==> R12, R11, AND R10.
.*                   BASES=2,SAVTYPE=LOCAL ==> R13, AND R12.
.*             BASES= -A SUBLIST OF ONE OR MORE REGISTER NAMES-
.*                   THE LISTED REGISTERS ARE LOADED AND DECLARED AS
.*                   BASES. THE LEFTMOST LISTED REGISTER IS LOADED WITH
.*                   THE LOWEST ADDRESS. EXAMPLE:
.*                   BASES=(R5,6,4) ==> R5, R6, AND R4 IN THAT ORDER.
.*             BASES=*
.*                   THE BASE ADDRESS AND BASE REGISTERS DEFINED BY THE
.*                   PHYSICALLY PREVIOUS #ENTER MACRO ARE REUSED.
.*          THE BASES= OPERAND MAY BE NULLIFIED BY SPECIFYING EITHER:
.*             BASES=
.*             BASES=0
.*       IN THIS CASE, NO BASE REGISTERS ARE LOADED OR DECLARED.
.*          IF THE BASES= OPERAND IS OMITTED, THEN A DEFAULT OF BASES=1
.*       WILL BE USED.
.*
.* &SAVTYPE=
.*       THIS OPERAND IDENTIFIES THE TYPE OR LOCATION OF THE SAVE AREA
.*       TO BE GENERATED OR USED. VALID VALUES ARE:
.*             -OMITTED-
.*             SAVTYPE=
.*             SAVTYPE=LOCAL
.*             SAVTYPE=(,-SAVE AREA NAME-)
.*             SAVTYPE=(LOCAL,-SAVE AREA NAME-)
.*             SAVTYPE=(LOCAL,,-SAVE AREA LENGTH-)
.*             SAVTYPE=(LOCAL,-SAVE AREA NAME-,-SAVE AREA LENGTH-)
.*                     A STANDARD SAVE AREA IS GENERATED IN THE
.*                     THE MACRO EXPANSION AND ITS ADDRESS IS LOADED
.*                     INTO R13. NOTE, DEPENDING UPON THE BASES=
.*                     OPERAND (SEE ABOVE) R13 MAY ALSO BE DECLARED AS
.*                     A PROGRAM BASE.
.*                        IF A -SAVE AREA NAME- IS GIVEN, THEN IT IS
.*                     USED TO LABEL THE SAVE AREA; OTHERWISE, AN
.*                     INTERNAL NAME IS GENERATED.
.*                              IF -SAVE AREA LENGTH- IS GIVEN, THEN IT
.*                              IS USED TO SET THE LENGTH OF THE SAVE
.*                              AREA; OTHERWISE, THE DEFAULT LENGTH OF
.*                              72 BYTES IS USED.
.*             SAVTYPE=(REMOTE,-SAVE AREA ADDRESS-)
.*                     THE ADDRESS OF THE REMOTE SAVE AREA IS
.*                     LOADED INTO R13. NOTE, IN THIS CASE -SAVE AREA
.*                     ADDRESS- IS A REQUIRED SUB-OPERAND. IT MAY BE
.*                     EITHER AN ADDRESS LABEL OR A PARENTHESIZED
.*                     REGISTER NAME OR AN ADDRESS LABEL FOLLOWED BY A
.*                     PERCENT (%) SIGN. IF THE NAME IS JUST AN ADDRESS
.*                     LABEL, THEN A 'LA' INSTRUCTION IS USED TO LOAD
.*                     THE SAVE AREA'S ADDRESS. IF A PERCENT SIGN
.*                     FOLLOWS THE NAME, THEN AN ADDRESS CONSTANT IS
.*                     GENERATED AND A 'L' INSTRUCTION IS USED. IF A
.*                     REGISTER NAME IS GIVEN, THEN A 'LR' INSTRUCTION
.*                     IS USED UNLESS THE MACRO CAN DETERMINE THAT THE
.*                     NAMED REGISTER IS ACTUALLY R13 IN WHICH CASE IT
.*                     IS ASSUMED THAT THE LOWER SAVE AREA IS ALREADY
.*                     PRESENT AND INITIALIZED, SO THE SAVING OF
.*                     REGISTERS AND THE CROSS-CHAINING OF THE SAVE
.*                     AREAS IS BYPASSED.
.*             SAVTYPE=RENT
.*             SAVTYPE=(RENT,(-LENGTH-,-SUBPOOL-),-ERROR ADDRESS-)
.*             SAVTYPE=(RENT,(-LENGTH-,-SUBPOOL-),RETURN)
.*                     THE MACRO EXPANSION IS TO BE REENTRANT. THE SAVE
.*                     AREA IS TO BE GETMAINED. THE SUB-OPERANDS HAVE
.*                     THE FOLLOWING AFFECT:
.*                     -LENGTH- IS OPTIONAL. IF OMITTED, THEN A VALUE
.*                              OF 72 IS USED. IF GIVEN, THEN IT
.*                              INDICATES THE SIZE OF THE SAVE AREA TO
.*                              BE GOTTEN. WARNING, THE VALUE OF
.*                              -LENGTH- SHOULD NEVER BE LESS THAN 72.
.*                     -SUBPOOL- IS OPTIONAL. IF OMITTED, THEN A VALUE
.*                               OF 0 IS IMPLIED. IF GIVEN, THEN IT
.*                               SPECIFIES THE SUBPOOL OUT OF WHICH THE
.*                               SAVE AREA IS TO BE GOTTEN.
.*                     -ERROR ADDRESS- IS OPTIONAL. IF OMITTED, THEN
.*                                     THE SAVE AREA GETMAIN REQUEST IS
.*                                     UNCONDITIONAL. IF GIVEN, THEN
.*                                     THE GETMAIN IS CONDITIONAL, AND
.*                                     IF IT FAILS, THEN CONTROL IS
.*                                     PASSED TO THE INDICATED ADDRESS.
.*                                     NOTE, -ERROR ADDRESS- MAY BE
.*                                     EITHER A STATEMENT LABEL OR A
.*                                     PARENTHESIZED REGISTER NAME.
.*                                     WARNING, -ERROR ADDRESS- MAY BE
.*                                     USED ONLY IN A MVS ENVIRONMENT.
.*                                     IT IS NOT SUPPORTED UNDER MVT.
.*                     RETURN IS A SPECIAL FORM OF -ERROR ADDRESS-
.*                            WHICH, IF GIVEN, CAUSES CONTROL TO BE
.*                            RETURNED IMMEDIATELY TO THE CALLER IN THE
.*                            EVENT OF A GETMAIN FAILURE. ALL REGISTERS
.*                            ARE RESTORED EXCEPT R15 WHICH CONTAINS
.*                            THE RETURN CODE FROM GETMAIN.
.*             SAVTYPE=PLI
.*             SAVTYPE=NONE                                   06/84 DBC
.*                     NO LOCAL LEVEL SAVEAREA IS DEFINED OR  06/84 DBC
.*                     USED.                                  06/84 DBC
.*             SAVTYPE=(PLI,-LENGTH-)
.*                     THE MACRO IS TO EXPAND INTO THE FORMAT OF A PLI
.*                     PROLOG. THE EXPANSION IS REENTRANT. THE SAVE
.*                     AREA WILL BE A PLI DSA. IT WILL PROBABLY BE
.*                     OBTAINED FROM PLI'S ISA. IF -LENGTH- IS GIVEN,
.*                     THEN IT SPECIFIES THE DESIRED LENGTH OF THE DSA
.*                     THAT THIS EXPANSION OBTAINS. WARNING, THE VALUE
.*                     OF -LENGTH- MUST NEVER BE LESS THAN 88.
.*
.* &PFIX=
.*       THE #ENTER MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS
.*       EXPANSION WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE
.*       OF THE REGISTERS IS INDICATED IN THE ASSEMBLER'S CROSS
.*       REFERENCE LISTING. THE PFIX= OPERAND CAN BE USED TO CONTROL
.*       THE SET OF EQUATES USED. FOR EXAMPLE, IF "PFIX=GPR" IS GIVEN,
.*       THEN "GPR1" IS USED WHENEVER THE EXPANSION REFERS FO REGISTER
.*       1.
.*          IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF REGISTER
.*       EQUATES DEFINED BY THE NEXT PRIOR #REGS MACRO IS USED. IF
.*       THERE ARE NO PRIOR #REGS MACROS, THEN SELF-DEFINING NUMERICS
.*       ARE USED.
.*
.* MISCELLANIOUS CONSIDERATIONS
.*     - SINCE THE #ENTER MACRO EXPANSION USUALLY INCLUDES A CSECT
.*       CARD, THE MACRO CALL SHOULD BE PLACED AT THE PHYSICAL
.*       BEGINNING OF A CONTROL SECTION.
.*     - FOR LOCAL SAVE AREAS IT IS BOTH POSSIBLE AND REASONABLE FOR
.*       R13 TO SERVE AS BOTH THE SAVE AREA POINTER AND A PROGRAM BASE.
.*       THERE ARE, HOWEVER, CERTAIN PRECAUTIONS THAT HAVE TO BE TAKEN
.*       FOR VARIOUS SYSTEM EXIT ROUTINES IF THEY ARE INCLUDED IN THE
.*       PROGRAM. HERE ARE SOME EXAMPLES:
.*           - IOS APPENDAGE ROUTINES: NO BASE REGISTER FOR THE MAIN
.*             PROGRAM, INCLUDING R13, IS AVAILABLE FROM WITHIN AN IOS
.*             APPENDAGE.
.*           - DCB OPEN EXITS: R13 REMAINS A VALID BASE REGISTER.
.*           - EOD ROUTINES: R13 REMAINS A VALID BASE REGISTER.
.*           - SYNAD EXITS: R13 REMAINS A VALID BASE REGISTER UNTIL A
.*             SYNADAF MACRO IS ISSUED. AFTER A SUBSEQUENT SYNADRLS
.*             MACRO, R13 IS AGAIN A VALID PROGRAM BASE.
.*
.*
.*
.* INNER MACROS USED - #USING, #TEST, SAVE, GETMAIN
.*
         GBLA  &#TESERR
         GBLB  &#ENTRNT,&#ENTPLI,&#ENTNUN                     06/84 DBC
         GBLC  &#TESRET(20),&#ENTSIZ,&#ENTSP,&#BS(14)
         LCLA  &A1,&A2,&C1,&C2,&C3,&C4,&B(13),&RMTREGA
         LCLB  &REDUN(13),&OLDBASE,&REMOTE(5),&ALIGND,&LOCAL,&LENGTH
         LCLC  &LID,&@,&#,&N1,&N2,&W2,&W3,&R,&RMTREGC,&SAVLEN,&RMTNAME
         LCLC  &SPOOL,&TB,&ESDT,&C                            06/84 DBC
&#       SETC  '&SYSNDX'
&#ENTRNT SETB  (0)
&#ENTPLI SETB  (0)
&#ENTNUN SETB  (0)
&C1      SETA  11
.*                                                            06/84 DBC
         AIF   ('&SAVTYPE' NE 'NONE').TYPNNON                 06/84 DBC
&#ENTNUN SETB  (1)                                            06/84 DBC
         AGO   .PFXTST                                        06/84 DBC
.TYPNNON ANOP                                                 06/84 DBC
.*                                                            06/84 DBC
         AIF   ('&SAVTYPE(1)' NE 'RENT').TYPNRNT
&#ENTRNT SETB  (1)
&#ENTSIZ SETC  '72'
&#ENTSP  SETC  ''
         #TEST DCODE=&SAVTYPE(2)
&A1      SETA  &#TESRET(1)
         AIF   (&A1 EQ 0).PFXTST
         AIF   ('&#TESRET(2)' EQ '').DFLTLEN
&#ENTSIZ SETC  '&#TESRET(2)'
.DFLTLEN AIF   (&A1 EQ 1).PFXTST
&#ENTSP  SETC  '&#TESRET(3)'
         AGO   .PFXTST
.TYPNRNT ANOP
.*
         AIF   ('&SAVTYPE(1)' NE 'REMOTE').TYPNRMT
&REMOTE(1) SETB (1)
         AIF   ('&SAVTYPE(2)' NE '').GOTRMT2
         MNOTE 12,'ERROR - SAVTYPE(2) (REMOTE AREA''S NAME) OMITTED.'
.GOTRMT2 AIF   ('&SAVTYPE(2)'(1,1) EQ '(').TYPLCL2
&REMOTE(2) SETB (1)
&RMTNAME SETC  '&SAVTYPE(2)'
         AIF   ('&SAVTYPE(2)'(K'&SAVTYPE(2),1) NE '%').PFXTST
&REMOTE(5) SETB (1)
&RMTNAME SETC  '&SAVTYPE(2)'(1,K'&SAVTYPE(2)-1)
         AGO   .PFXTST
.TYPLCL2 #TEST DCODE=&SAVTYPE(2)
&RMTREGC SETC  '&#TESRET(2)'
         #TEST REGS=&RMTREGC
         AIF   (&#TESERR NE 0).PFXTST
&RMTREGA SETA  &#TESRET(1)
         AIF   (&RMTREGA NE 13).PFXTST
&REMOTE(3) SETB (1)
         AGO   .PFXTST
.TYPNRMT ANOP
.*
         AIF   ('&SAVTYPE(1)' NE 'PLI').TYPNPLI
&#ENTPLI SETB  (1)
&C1      SETA  10
         AGO   .PFXTST
.TYPNPLI ANOP
.*
         AIF   ('&SAVTYPE(1)' EQ '' OR '&SAVTYPE(1)' EQ 'LOCAL').TYPLCL
         MNOTE 4,'SAVTYPE(1)=&SAVTYPE(1) IS INVALID.'
         MNOTE 4,'SAVTYPE(1)=LOCAL ASSUMED.'
.TYPLCL  ANOP
&LOCAL   SETB  (1)
&C1      SETA  12
&LID     SETC  'E&#.SVA'
         AIF   ('&SAVTYPE(2)' EQ '').GOTSLID
&LID     SETC  '&SAVTYPE(2)'
.GOTSLID ANOP
&SAVLEN  SETC  '72'
         AIF   ('&SAVTYPE(3)' EQ '').PFXTST
&SAVLEN  SETC  '&SAVTYPE(3)'
.*
.PFXTST  ANOP
&@       SETC  '&PFIX'
         AIF   (K'&PFIX NE 0).GOTPFIX
         #TEST PFIX=
&@       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.GOTPFIX #REGS &PFIX,GEN=NO
.DONPFIX ANOP
.*                                                            06/84 DBC
&A1      SETA  0
         AIF   ('&BASES' NE '*').BSCLR
.BSOLD   AIF   (&A1 EQ 13).BASEND
&A1      SETA  &A1+1
         AIF   ('&#BS(&A1)' EQ '').BSOLD
         #TEST REGS=&#BS(&A1)
&B(&A1)  SETA  16
         AIF   (&#TESERR NE 0).BSOLD
&B(&A1)  SETA  &#TESRET(1)
         AIF   (&B(&A1) NE 13).BSOLD
         AIF   (&A1 NE 13 OR '&#BS(14)' NE '&RMTNAME' OR &#ENTRNT OR &#*
               ENTPLI).BSERROR
&REMOTE(4) SETB (1)
         AGO   .BSOLD
.BSERROR ANOP
         MNOTE 4,'THE OLD BASE REGISTER &B(&A1) CANNOT ALSO FUNCTION'
         MNOTE 4,'AS A SAVE AREA POINTER IN THIS CONTEXT.'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
         AGO   .BSOLD
.BSCLR   AIF   (&A1 EQ 14).BSCLRD
&A1      SETA  &A1+1
&#BS(&A1) SETC ''
         AGO   .BSCLR
.BSCLRD  AIF   (K'&BASES EQ 0).BASEND
         AIF   ('&BASES' NE '&BASES(1)').TSTNBSE
         #TEST NUM=&BASES
         AIF   (&#TESERR EQ 0).BSEOKX
         MNOTE 4,'"BASES=&BASES" IS INVALID.'
         MNOTE 4,'"BASES=1" ASSUMED.'
&C3      SETA  1
         AGO   .BSESET
.BSEOKX  ANOP
&C3      SETA  &BASES
         AIF   (&C3 LE &C1).BSESET
         MNOTE 4,'"BASES=&BASES" IS OUTSIDE THE RANGE OF 0...&C1..'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
.BSESET  ANOP
&C1      SETA  &C1+1
&C3      SETA  &C1-&C3
&C4      SETA  13
.BOK     AIF   (&C1 LE &C3).BASEND
&#BS(&C4) SETC '&@&C1'
&B(&C4)  SETA  &C1
&C1      SETA  &C1-1
&C4      SETA  &C4-1
         AGO   .BOK
.TSTNBSE ANOP
&C3      SETA  N'&BASES
         AIF   (&C3 LE &C1).NBSOK
         MNOTE 4,'"BASES=&BASES" SPECIFIES TOO MAY REGISTERS.'
         MNOTE 4,'ONLY THE FIRST &C1 REGISTERS WILL BE USED.'
&C3      SETA  &C1
.NBSOK   ANOP
&C1      SETA  &C1+1
&C4      SETA  13
&C2      SETA  0
.GETBSE  AIF   (&C2 GE &C3).BASEND
&C2      SETA  &C2+1
         AIF   ('&BASES(&C2)' EQ '').IGNR
         #TEST REGS=&BASES(&C2)
&B(&C4)  SETA  16
         AIF   (&#TESERR EQ 16).REGUNK
         AIF   (&#TESRET(1) GE 2 AND &#TESRET(1) LE &C1).BSEOK2
         MNOTE 4,'"BASES(&C2)=&BASES(&C2)" IS OUTSIDE THE RANGE OF 2...*
               &C1..'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
         AGO   .BSEOK2
.IGNR    AIF   (&C3 GE N'&BASES).GETBSE
&C3      SETA  &C3+1
         AGO   .GETBSE
.BSEOK2  AIF   (NOT &REDUN(&#TESRET(1))).BSEOK3
         MNOTE 4,'"BASES(&C2)=&BASES(&C2) IS REDUNDANT.'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
.BSEOK3  ANOP
&REDUN(&#TESRET(1)) SETB (1)
&B(&C4)  SETA &#TESRET(1)
.REGUNK  ANOP
&C1      SETA  12
&#BS(&C4) SETC '&BASES(&C2)'
&C4      SETA  &C4-1
         AGO   .GETBSE
.BASEND  ANOP
.*
&R       SETC  '&@.1'
&C3      SETA  0
.WRLP    AIF   (&C3 GE 13).GOTWRG
&C3      SETA  &C3+1
         AIF   ('&#BS(&C3)' EQ '').WRLP
         AIF   (&B(&C3) EQ 13).GOTWRG
&R       SETC  '&#BS(&C3)'
.GOTWRG  ANOP
.*
&ESDT    SETC  '&ESDTYPE(1)'
         AIF   (&#ENTPLI).PLIGEN
.*
         AIF   ('&ESDT' NE 'DEFAULT').GOTESD
&ESDT    SETC  'CSECT'
.GOTESD  ANOP
&N2      SETC  '&N'
         AIF   ('&ESDT' EQ 'NONE' OR '&ESDT' EQ '' OR '&ESDT' EQ 'ENTRY*
               ').NCSCETC
         AIF   ('&ESDT' NE 'CSECT').ESDNCSC
&N       CSECT ,                   START CONTROL SECTION
         AGO   .ESDDONE
.ESDNCSC AIF   ('&ESDT' NE 'START').ESDNSTA
&N       START ,                   START CONTOL SECTION
         AGO   .ESDDONE
.ESDNSTA ANOP
&W2      SETC  '&ESDT'
&N       &W2   0H'0'               START
.ESDDONE ANOP
&N2      SETC  ''
.*
.NCSCETC AIF   ('&ESDT' NE 'ENTRY').NENTRY
         ENTRY &N                  MAKE NAME EXTERNALLY AVAILABLE
.NENTRY  ANOP
.*
         AIF   (K'&NME EQ 0 AND '&ESDT' NE 'CSECT' AND '&ESDT' NE 'STAR*
               T').NMODID
&N2      B     E&#.ZID-&N.(,&@.15) SKIP AROUND THE MODULE ID
&N2      SETC  'E&#.ZID'
         DC    AL1(&N2-E&#.MID)    LENGTH OF TEXT
E&#.MID  DC    C'&N '              ENTRY NAME
&W2      SETC  '&SYSDATE       '(1,8).' '
         DC    C'&W2'              ASSEMBLY DATE
&W2      SETC  '&SYSTIME    '(1,5)
         AIF   (K'&NME EQ 0).NMEZ1
&W2      SETC  '&W2 - '
.NMEZ1   DC    C'&W2'              ASSEMBLY TIME
         AIF   (K'&NME EQ 0).NMEZ2
         AIF   ('&NME'(1,1) EQ '''').QNME
         DC    C'&NME'
         AGO   .NMEZ2
.QNME    DC    C&NME
.NMEZ2   ANOP
.NMODID  ANOP
.*
         AIF   (&REMOTE(3)).RNTRNT1
&W2      SETC  '&@.14,&@.12,12(&@.13)'
&N2      STM   &W2                 SAVE CALLER'S REGISTERS
&N2      SETC  ''
.*
         AIF   (&#ENTRNT OR &#ENTNUN).RNTRNT1                 06/84 DBC
         LR    &R,&@.13            POINT TO HIGHER SA
         AIF   (&REMOTE(1)).LRMTSV1
&C       SETC  '&@.13,&LID-&N.(,&@.15)'                       06/84 DBC
         LA    &C                  POINT TO LOWER SA
         AGO   .LRMTSV2
.LRMTSV1 AIF   (&REMOTE(2)).LRMTSV3
         #TEST REGS=&R
         AIF   (&RMTREGA NE &#TESRET(1)).LRMTSV4
&A1      SETA  20+&RMTREGA*4-&RMTREGA/13*44
         L     &@.13,&A1.(,&@.13)  POINT TO LOWER SA
         AGO   .LRMTSV2
.LRMTSV4 LR    &@.13,&RMTREGC      POINT TO LOWER SA
         AGO   .LRMTSV2
.LRMTSV3 AIF   (&REMOTE(5)).LRMTSV5
         PUSH  USING               SAVE USING ENVIRONMENT
         USING &N,&@.15            DECLARE TEMP BASE
         LA    &@.13,&RMTNAME      POINT TO LOWER SA
         POP   USING               RESTORE USING ENVIRONMENT
         AGO   .LRMTSV2                                       06/84 DBC
.LRMTSV5 ANOP                                                 06/84 DBC
         LR    &@.13,&@.15         POINT TO LOWER SA          08/84 DBC
&C       SETC  '&@.13,E&#.SAP-&N.(,&@.15)'                    06/84 DBC
         AH    &C                                             08/84 DBC
.LRMTSV2 ST    &@.13,8(,&R)        FORWARD CHAIN THE SAVE AREAS
         ST    &R,4(,&@.13)        BACK CHAIN THE SAVE AREAS
         AIF   ('&R' NE '&@.1').RNTRNT1
         L     &@.1,24(,&@.1)      RESTORE REGISTER 1
.RNTRNT1 AIF   ('&#BS(13)' EQ '').SKIPUSE
&C1      SETA  13
         AIF   ('&BASES' NE '*').BSEADR
         AIF   (&REMOTE(4)).EQUATE
&OLDBASE SETB  (1)
&N2      LR    &#BS(13),&@.15      LOAD 1ST BASE REGISTER     08/84 DBC
&N2      SETC  ''                                             08/84 DBC
&C       SETC  '&#BS(13),E&#.BSE-&N.(,&@.15)'                 06/84 DBC
         SH    &C                                             08/84 DBC
         AGO   .EQUATE
.BSEADR  ANOP
&C2      SETA  15
&#BS(14) SETC  '&N'
         AIF   (&#ENTRNT OR &REMOTE(1) OR &#ENTNUN).FLDTST    06/84 DBC
&C2      SETA  13
&#BS(14) SETC  '&LID'
.FLDTST  AIF   (&B(13) EQ &C2).EQUATE
&N2      LR    &#BS(13),&@&C2      LOAD FIRST BASE REGISTER
&N2      SETC  ''
.EQUATE  ANOP
&W2      SETC  '&#BS(&C1)'
         AIF   (&C1 EQ 2).ENDLA
&C1      SETA  &C1-1
         AIF   ('&#BS(&C1)' EQ '').ENDLA
&N2      LA    &#BS(&C1),X'FFF'(,&W2) LOAD NEXT BASE
&N2      SETC  ''
         AGO   .EQUATE
.ENDLA   #USING
.SKIPUSE AIF   (NOT &#ENTRNT).DATACHK
&W2      SETC  ''
&TB      SETC  ''
         AIF   ('&#BS(13)' NE '').GETM2
&N2      LR    &@.14,&@.15         LOAD TEMPORARY BASE
&N2      SETC  ''
         PUSH  USING               SAVE BASES
         DROP  ,                   CLEAR BASES
         USING &N,&@.14            DECLARE TEMPORARY BASE
&TB      SETC  '-&N.(,&@.14)'
.GETM2   ANOP
&N2      L     &@.0,E&#.LEN        LOAD LENGTH (MAYBE SUBPOOL TOO)
&N2      SETC  ''
         AIF   ('&#ENTSP' EQ '' OR '&SAVTYPE(3)' EQ '').GETM4
&SPOOL   SETC  ''
         MNOTE '         GETMAIN RC,LV=(0),SP=&#ENTSP'
         GETMAIN RC,LV=(0),SP=&#ENTSP
         AGO   .GETM5
.GETM4   ANOP
&SPOOL   SETC  '&#ENTSP'
&W3      SETC  'R'
         AIF   ('&SAVTYPE(3)' EQ '').GETM4A
&W3      SETC  'RC'
.GETM4A  MNOTE '         GETMAIN &W3,LV=(0)'
         GETMAIN &W3,LV=(0)
.GETM5   AIF   ('&#BS(13)' NE '').GETM5A
         POP   USING               RESTURE BASES
.GETM5A  AIF   ('&SAVTYPE(3)' EQ '').GETM7
         LTR   &@.15,&@.15         GETMAIN OK?
         AIF   ('&SAVTYPE(3)' EQ 'RETURN').GETM8
         AIF   ('&SAVTYPE(3)'(1,1) EQ '(').GETM6
         BNZ   &SAVTYPE(3)         NO, TAKE ERROR EXIT
         AGO   .GETM7
.GETM6   #TEST DCODE=&SAVTYPE(3)
         BCR   7,&#TESRET(2)       NO, TAKE ERROR EXIT
         AGO   .GETM7
.GETM8   ANOP
&W2      SETC  'E&#.GO'
         BZ    &W2&TB              YES, PROCEED
         L     &@.14,12(,&@.13)    NO, RESTORE REGISTER
         LM    &@.0,&@.12,20(&@.13) RESTORE REGISTERS
         MVI   12(&@.13),X'FF'     SET RETURNED SIGNEL
         BR    &@.14               RETURN TO CALLER
.GETM7   ANOP
&W2      LR    &@.0,&@.1           POINT TO AREA TO CLEAR
         L     &@.1,E&#.LEN&TB     GET LENGTH TO CLEAR
         LR    &@.14,&@.0          SAVE AREA POINTER
         SLR   &@.15,&@.15         CLEAR SRC LEN AND PAD CHAR 08/85 DBC
         MVCL  &@.0,&@.14          CLEAR THE AREA (R15 SET BY GETMAIN)
         ST    &@.14,8(,&@.13)     FORWARD CHAIN THE SAVE AREAS
         ST    &@.13,4(,&@.14)     BACK CHAIN THE SAVE AREAS
         LM    &@.13,&@.1,8(&@.13) RESTORE REGS AND POINT TO LOWER SA
.DATACHK AIF   (NOT &OLDBASE AND NOT &#ENTRNT AND NOT &REMOTE(5) AND NO*
               T &LOCAL).ENDCHK
         AIF   ('&#BS(13)' EQ '').NOUSING
&N2      B     E&#.END             SKIP AROUND DATA AREA
&N2      SETC  ''
         AGO   .DFNDATA
.NOUSING ANOP
&N2      B     E&#.END-&N.(,&@.15) SKIP AROUND DATA AREA
&N2      SETC  ''
.DFNDATA ANOP                                                 08/84 DBC
.*                                                            08/84 DBC
         AIF   (NOT &OLDBASE).NOLDBSE                         08/84 DBC
E&#.BSE  DC    Y(&N-&#BS(14))       OLD BASE ADDRESS          08/84 DBC
.NOLDBSE ANOP                                                 08/84 DBC
.*                                                            08/84 DBC
         AIF   (NOT &#ENTRNT).NLENGTH                         08/84 DBC
         AIF   ('&SPOOL' EQ '').NSUBPOO
         AIF   (&ALIGND).ALIGND1
         DS    0F                  ALIGNMENT
&ALIGND  SETB  (1)
.ALIGND1 ANOP
E&#.LEN  DC    AL1(&SPOOL),AL3(&#ENTSIZ) SAVE AREA SUBPOOL AND LENGTH
         AGO   .NLENGTH
.NSUBPOO ANOP
E&#.LEN  DC    A(&#ENTSIZ)         SAVE AREA LENGTH
&ALIGND  SETB  (1)
.NLENGTH AIF   (&#ENTRNT OR &#ENTNUN).NSVAREA                 06/84 DBC
         AIF   (&REMOTE(1)).RMTSVPT
         AIF   (&ALIGND).ALIGND2
         DS    0F                  ALIGNMENT
&ALIGND  SETB  (1)
.ALIGND2 ANOP
&LID     DC    (&SAVLEN)X'00'      LOCAL SAVE AREA
         AGO   .NSVAREA
.RMTSVPT ANOP                                                 08/84 DBC
.*                                                            08/84 DBC
         AIF   (NOT &REMOTE(5)).NSVAREA                       08/84 DBC
E&#.SAP  DC    Y(&RMTNAME-&N)       PTR TO REMOTE SA          08/84 DBC
.NSVAREA ANOP                                                 08/84 DBC
.*                                                            08/84 DBC
         AGO   .END                                           08/84 DBC
.*
.PLIGEN  AIF   ('&ESDT' EQ 'DEFAULT').PESDSET
         AIF   (K'&N GT 0 OR '&ESDT' NE 'ENTRY').PESDOK1
         MNOTE 4,'"ESDTYPE=&ESDTYPE" IS INVALID WHEN THE NAME FIELD IS'
         MNOTE 4,'OMITTED FROM THE MACRO CALL.'
         MNOTE 4,'"ESDTYPE=NONE" WILL BE USED INSTEAD.'
&ESDT    SETC  'NONE'
.PESDOK1 ANOP
         AIF   ('&ESDT' EQ 'ENTRY' OR '&ESDT' EQ 'NONE' OR '&ESDT' EQ '*
               ').PESDOK
         MNOTE 4,'"ESDTYPE=&ESDTYPE" IS INVALID WHEN "SAVTYPE=PLI".'
.PESDSET ANOP
&ESDT    SETC  'ENTRY'
         AIF   (K'&N GT 0).PESDOK2
&ESDT    SETC  'NONE'
.PESDOK2 AIF   ('&ESDTYPE' EQ 'DEFAULT').PESDOK
         MNOTE 4,'"ESDTYPE=&ESDT" WILL BE USED INSTEAD.'
.PESDOK  ANOP
         AIF   ('&ESDT' NE 'ENTRY').PNOTENT
         ENTRY &N                  MAKE NAME EXTERNALLY AVAILABLE
.PNOTENT ANOP
.*
&N2      SETC  '&N'
&A1      SETA  K'&N
         AIF   (K'&NME EQ 0).GOTN2
&N2      SETC  '&NME'
&A1      SETA  K'&NME
.GOTN2   ANOP
&N2      SETC  ' '(1,1-(&A1-&A1/2*2)).'&N2'
         DS    0H                  ALIGNMENT
         DC    C'&N2'              ENTRY NAME
         DC    AL1(&A1)            LENGTH OF NAME
.*
&N1      SETC  '&N'
         AIF   (K'&N GT 0).PGOTN1
&N1      SETC  'E&#.ENT'
.PGOTN1  ANOP
         USING &N1,&@.15           DCL LOCAL BASE
&N1      STM   &@.14,&@.12,12(&@.13) SAVE CALLER'S REGISTERS
.*
         #TEST DCODE=&SAVTYPE(2)
&A1      SETA  &#TESRET(1)
&A2      SETA  120
         AIF   (&A1 EQ 0).DSALLA
         AIF   ('&#TESRET(2)' NE '&SAVTYPE(2)').DSALREG
         #TEST NUM=&#TESRET(2)
         AIF   (&#TESERR NE 0).DSALL
&A2      SETA  &#TESRET(2)
         AIF   (&A2 GE 4096-7).DSALL
         AIF   (&A2 GE 120-7).DSALLA
         MNOTE 4,'"SAVTYPE(2)=&SAVTYPE(2)" IS TOO SHORT A LENGTH.'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
.DSALLA  ANOP
         LA    &@.0,(&A2+7)/8*8    GET DESIRED DSA LENGTH
         AGO   .GOTDSAL
.DSALREG #TEST REGS=&#TESRET(2)
         AIF   (&#TESERR NE 0).DSALLR
         AIF   (&#TESRET(1) EQ 0).GOTLLR
.DSALLR  LR    &@.0,&#TESRET(2)    GET DESIRED DSA LENGTH
.GOTLLR  LA    &@.14,7             ROUND UP -
         AR    &@.0,&@.14           TO -
         OR    &@.0,&@.14            DOUBLE WORD -
         XR    &@.0,&@.14             LENGTH
         AGO   .GOTDSAL
.DSALL   ANOP
&LENGTH  SETB  (1)
         L     &@.0,E&#.LEN        GET DESIRED DSA LENGTH
.GOTDSAL ANOP
.*
         L     &@.1,76(,&@.13)     GET NXT AVAILABLE BLOCK POINTER
         ALR   &@.0,&@.1           --> PAST DESIRED AREA
         CL    &@.0,12(,&@.12)     WOULD THE ISA OVERFLOW?
         BNH   E&#.GOT             NO, PROCEED
         L     &@.15,116(,&@.12)   YES, --> SPECIAL HANDLER
         DROP  &@.15               RELEASE CLOBBERED BASE
         BALR  &@.14,&@.15         GO OBTAIN DESIRED DSA FROM ELSEWHERE
E&#.GOT  LR    &@.14,&@.1          SAVE PTR TO NEW DSA
         LR    &@.15,&@.0          SAVE HI-BYTE OF NAB POINTER REG
         SRL   &@.15,24            ISSOLATE IT
         SLL   &@.15,24            RESTORE ITS POSITION. SET MVCL
*                                  SOURCE LENGTH TO ZERO
         SR    &@.0,&@.1           GET LENGTH OF NEW DSA
         LR    &@.1,&@.0           COPY FOR MVCL SINK LENGTH
         LR    &@.0,&@.14          GET MVCL SINK POINTER
         MVCL  &@.0,&@.14          CLEAR THE NEW DSA
         OR    &@.0,&@.15          RESTORE HI-BYTE TO NAB POINTER REG
         LR    &@.1,&@.0           COPY NEXT AVAILABLE BLOCK POINTER
         L     &@.15,72(,&@.13)    GET LIBRARY WORKSPACE POINTER
         STM   &@.15,&@.1,72(&@.14) STORE INTO OUR NEW DSA
         ST    &@.5,88(,&@.14)     STORE PASSED PARAMETERS POINTER
         ST    &@.13,4(,&@.14)     BACK CHAIN THE DSA
         L     &@.1,24(,&@.13)     RESTORE PLIST POINTER
         LR    &@.13,&@.14         --> NEW DSA (R14 PURIFIED BY MVCL)
         MVI   0(&@.13),X'80'      SET FOR -
         MVI   1(&@.13),X'00'       PLI -
         MVI   86(&@.13),X'91'       ERROR -
         MVI   87(&@.13),X'C0'        HANDLING
.*
&N2      SETC  ''
         AIF   ('&#BS(13)' EQ '').PSKPUSE
&C1      SETA  13
         AIF   ('&BASES' NE '*').PBSEADR
&OLDBASE SETB  (1)
         BALR  &#BS(13),0          LOAD TEMP LOCAL BASE
         L     &#BS(13),E&#.BSE-*(,&#BS(13)) LOAD 1ST PROGRAM BASE
         AGO   .PEQUATE
.PBSEADR ANOP
&N2      SETC  'E&#.BSE'
&#BS(14) SETC  '&N2'
         BALR  &#BS(13),0          LOAD 1ST PROGRAM BASE
.PEQUATE ANOP
&W2      SETC  '&#BS(&C1)'
&C1      SETA  &C1-1
         AIF   (&C1 EQ 1 OR '&#BS(&C1)' EQ '').PENDLA
&N2      LA    &#BS(&C1),X'FFF'(,&W2) LOAD NEXT PROGRAM BASE
&N2      SETC  ''
         AGO   .PEQUATE
.PENDLA  #USING ,
.PSKPUSE ANOP
.*
         AIF   (NOT &LENGTH AND NOT &OLDBASE).ENDCHK
         AIF   ('&#BS(13)' NE '').PGOTBAS
&N2      BALR  &@.15,0             LOAD TEMP BASE
&N2      SETC  ''
         B     E&#.END-*(,&@.15)   SKIP DATA AREA
         AGO   .PDFNDAT
.PGOTBAS ANOP
&N2      B     E&#.END             SKIP DATA AREA
&N2      SETC  ''
.PDFNDAT AIF   (NOT &LENGTH).PNOLEN
E&#.LEN  DC    A((&SAVTYPE(2)+7)/8*8) DESIRED DSA LENGTH
.PNOLEN  AIF   (NOT &OLDBASE).PNOOBAS
E&#.BSE  DC    A(&#BS(14))         OLD BASE ADDRESS
.PNOOBAS ANOP
.*
.END     ANOP
E&#.END  DS    0H
.ENDCHK  AIF   ('&N2' EQ '').MEND
&N2      DS    0H
.MEND    MEND
./ ADD NAME=#EXIT
         MACRO
&NME     #EXIT &R,&PFIX=,&RC=,&MODE=LEAVE                     DBC 04/85
.*
.*
.*                                                            DBC 04/85
.* LAST CHANGE DATE - APRIL 29, 1985                          DBC 04/85
.*                  - ADDED SUPPORT FOR THE "MODE=" OPERAND.  DBC 04/85
.*                                                            DBC 10/84
.* LAST CHANGE DATE - OCTOBER 1, 1984                         DBC 10/84
.*                  - DELETED CODE THAT SET A X'FF' "RETURN   DBC 10/84
.*                    INDICATOR" IN THE HI-BYTE OF DSAR14.    DBC 10/84
.*                    IT WAS NOT APPROPRIATE FOR MVS/XA.      DBC 10/84
.*                                                            DBC 06/84
.* LAST CHANGE DATE - JUNE 11, 1984                           DBC 06/84
.*                  - ADDED SUPPORT FOR "SAVTYPE=NONE" ON THE DBC 06/84
.*                    #ENTER MACRO.                           DBC 06/84
.*                  - FOR REENTRANT EXIT LINKAGE, CHANGED     DBC 06/84
.*                    THE FREEMAIN SO THAT IT WOULD NO        DBC 06/84
.*                    LONGER GENERATE AN INLINE PLIST.        DBC 06/84
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - CHANGE THE MACRO NAME FROM $EXIT TO #EXIT
.*
.* LAST CHANGE DATE - APRIL 15, 1981
.*                  - ADDED EXIT LINKAGE FOR A PLI ENVIRONMENT.
.*                  - "#REGS GEN=NO" SUPPORT ADDED.
.*
.* LAST CHANGE DATE - OCTOBER 3, 1978
.*                  - FOR REENTRANT SAVE AREAS, THE FREEMAIN HAS BEEN
.*                    CHANGED SO THAT MORE THAN 4K CAN BE FREED.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - FEBRUARY 10, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES EITHER OS STANDARD OR PLI STANDARD EXIT
.* LINKAGE. IT WAS WRITTEN TO PROVIDE A SIMPLE, FLEXIBLE, AND COMPLETE
.* METHOD FOR GENERATING SUCH LINKAGE. IN ADDITION, FOR ALL POSSIBLE
.* OPERAND COMBINATIONS, #EXIT WILL GENERATE THE ABSOLUTE MINIMUM
.* AMOUNT OF CODE NECESSARY.
.*
.*   THE #EXIT MACRO WILL GENERATE THE FOLLOWING:
.* - CODE TO LOAD REGISTER 13 WITH A POINTER TO THE HIGHER OS SAVE AREA
.*   OR PLI DATA STORAGE AREA
.* - CODE TO RELEASE (VIA FREEMAIN) THE LOWER SAVE AREA IF THE
.*   PRECEEDING #ENTER MACRO CALL GENERATED A REENTRANT EXPANSION
.* - CODE TO RESTORE ANY PARTICULAR SET OF REGISTERS
.* - CODE TO LOAD REGISTER 15 WITH A RETURN CODE THAT IS EITHER AN
.*   ABSOLUTE VALUE OR A VALUE PRELOADED INTO ANY REGISTER
.* - CODE TO RETURN TO THE CALLING PROGRAM VIA REGISTER 14
.*
.*   THE NAME FIELD
.* USE THIS FIELD TO ASSIGN A STATEMENT LABEL TO THE FIRST MACHINE
.* INSTRUCTION OF THE EXPANSION. IF THE NAME FIELD IS OMITTED, THEN NO
.* STATEMENT LABEL IS ASSIGNED.
.*
.*   THE FIRST POSITIONAL OPERAND
.* THIS OPERAND MUST CONSIST OF A SUB-LIST OF ANY NUMBER OF ENTRIES.
.* EACH ENTRY MAY BE EITHER A SINGLE REGISTER NAME OR A PARENTHESIZED
.* PAIR (SEPERATED BY A COMMA) OF REGISTER NAMES - E.G.
.* " (1,11,(7,9),5) ". EACH SINGLE REGISTER NAME SPECIFIES A PARTICULAR
.* REGISTER TO BE RESTORED FROM THE HIGHER SAVE AREA. EACH
.* PARENTHESIZED PAIR OF REGISTER NAMES SPECIFIES A RANGE OF REGISTERS
.* TO BE RESTORED. THUS, THE ABOVE EXAMPLE WOULD CAUSE REGISTERS 1, 5,
.* 7, 8, 9, AND 11 TO BE RESTORED.
.*   SOME NOTES AND WARNINGS:
.* - A REQUEST TO RESTORE REGISTER 13 IS MEANINGLESS AND IS IGNORED.
.* - IF REGISTER 14 IS TO BE LOADED WITH THE RETURN ADDRESS FOUND IN
.*   THE HIGHER SAVE AREA, THEN YOU MUST SPECIFICALLY REQUEST THAT IT
.*   (REGISTER 14) BE RESTORED; OTHERWISE, WHATEVER VALUE IS FOUND IN
.*   REGISTER 14 PRIOR TO THE MACRO CALL WILL BE USED FOR THE RETURN
.*   ADDRESS.
.* - TO RESTORE ALL REGISTERS FROM 14 THROUGH 12, YOU MUST CODE
.*   " ((14,12)) ". CODING " (14,12) " WILL CAUSE ONLY REGISTERS 14 AND
.*   12 TO BE RESTORED.
.* - THE NUMERIC VALUES OF ALL REGISTER NAMES USED IN THIS OPERAND MUST
.*   BE DETERMINABLE AT MACRO PASS TIME. THUS, EACH REGISTER NAME USED
.*   MUST BE EITHER A SELF-DEFINING NUMERIC OR A NAME DEFINED VIA THE
.*   #REGS MACRO.
.* - IF ONLY A SINGLE REGISTER IS TO BE RESTORED, THEN IT NEED NOT BE
.*   ENCLOSED IN PARENTHESES.
.* - IF THE FIRST POSITIONAL OPERAND IS OMITTED, THEN NO REGISTERS ARE
.*   RESTORED.
.*
.*   THE RC= OPERAND
.* THIS OPERAND MUST CONSIST OF A SINGLE VALUE EITHER WITHIN OR NOT
.* WITHIN PARENTHESES. IF ENCLOSED WITHIN PARENTHESES, THEN THE VALUE
.* IS TREATED AS THE NAME OF A REGISTER CONTAINING A RETURN CODE. IF
.* NOT ENCLOSED WITHIN PARENTHESES, THEN THE VALUE IS TREATED AS BEING
.* THE RETURN CODE ITSELF.
.*   IF THE RC= OPERAND SPECIFIES A REGISTER NAME, THEN:
.* - THE VALUE OF THAT NAME NEED NOT BE DETERMINABLE AT MACRO PASS
.*   TIME;
.* - THE REGISTER NAME MAY IDENTIFY ANY REGISTER WHATSOEVER REGUARDLESS
.*   OF WHICH REGISTERS ARE TO BE RESTORED SINCE IN CASES OF POTENTIAL
.*   CONFLICT, THE RETURN CODE IS COPIED INTO REGISTER 15 PRIOR TO
.*   REGISTER RESTORATION;
.* - IT IS ILLOGICAL FOR THE RC= OPERAND TO SPECIFY REGISTER 13.
.* NOTE THAT IF THE RC= OPERAND IS SPECIFIED BUT THE FIRST POSITIONAL
.* OPERAND INDICATES THAT REGISTER 15 IS ALSO TO BE RESTORED, THEN
.* REGISTER 15 IS NOT RESTORED. INSTEAD, IT IS LOADED WITH THE RETURN
.* CODE VALUE.
.*   IF THE RC= OPERAND IS OMITTED, THEN NO CODE IS GENERATED TO LOAD
.* REGISTER 15 WITH A RETURN CODE.
.*                                                            DBC 04/85
.*   THE MODE= OPERAND                                        DBC 04/85
.* THIS FOR MVS/XA. IT CONTROLS WHETHER OR NOT THE #EXIT      DBC 04/85
.* MACRO IS TO GENERATE CODE TO RESTORE THE CALLER'S          DBC 04/85
.* ADDRESSING MODE. THE DEFAULT IS NOT TO DO SO.              DBC 04/85
.*                                                            DBC 04/85
.* - MODE=LEAVE                                               DBC 04/85
.*   THIS IS THE DEFAULT. NO EXPLICIT ATTEMPT IS MADE TO      DBC 04/85
.*   RESTORE THE CALLER'S ADDRESSING MODE.                    DBC 04/85
.*                                                            DBC 04/85
.* - MODE=RESTORE                                             DBC 04/85
.*   THE #EXIT MACRO ATTEMPTS TO RESTORE THE CALLER'S         DBC 04/85
.*   ADDRESSING MODE BASED ON THE HI-ORDER BIT OF THE         DBC 04/85
.*   CALLER'S R14.                                            DBC 04/85
.*
.*   THE PFIX= OPERAND
.* THE #EXIT MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS EXPANSION
.* WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE OF THE REGISTERS
.* WILL BE INDICATED IN THE CROSS REFERENCE LISTING. THE PFIX= OPERAND
.* CAN BE USED TO CONTROL THE SET OF EQUATES USED. FOR EXAMPLE, IF
.* "PFIX=GPR" IS SPECIFIED, THEN "GPR1" WILL BE USED WHENEVER THE
.* EXPANSION REFERS TO REGISTER 1.
.*   IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF EQUATES DEFINED
.* BY THE FIRST PRIOR #REGS MACRO WILL BE USED. IF THERE IS NO PRIOR
.* #REGS MACRO, THEN NO EQUATES WILL BE USED.
.*
.*   CONSIDERATIONS
.* THE #EXIT MACRO WILL GENERATE AN EXPANSION THAT WILL ACCURATELY
.* RESTORE ALL DESIRED REGISTERS AND SET THE RETURN CODE REGUARDLESS OF
.* WHETHER OR NOT A FREEMAIN SVC IS ISSUED TO RELEASE THE LOWER SAVE
.* AREA AND REGUARDLESS OF THE RELATIONSHIP BETWEEN THE RC= OPERAND AND
.* THE SET OF REGISTERS RESTORED.
.*
.*
.*
.* INNER MACROS USED - #REGS #TEST AND FREEMAIN
.*
         GBLA  &#TESERR
         GBLB  &#ENTRNT,&#ENTPLI,&#ENTNUN                     DBC 06/84
         GBLC  &#TESRET(20),&#ENTSIZ,&#ENTSP
         LCLA  &C1,&R1,&R2,&W1,&W2,&ERRCODE
         LCLB  &RCLA,&RCST,&RSW(16)
         LCLC  &LNME,&@,&RG(16),&REGNME,&RG2SAVE,&RG4SAVE,&RG5SAVE
         LCLC  &C,&#                                          DBC 06/84
&#       SETC  '&SYSNDX'                                      DBC 06/84
&LNME    SETC  '&NME'
.*
&@       SETC  '&PFIX'
         AIF   (K'&PFIX NE 0).GOTPFIX
         #TEST PFIX=
&@       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.GOTPFIX #REGS &PFIX,GEN=NO
.DONPFIX ANOP
.*
&C1      SETA  0                                              DBC 06/84
.RLP     AIF   (&C1 GE N'&R).RFIN
&C1      SETA  &C1+1
         #TEST DCODE=&R(&C1)
         AIF   (&#TESERR NE 0).END
&W1      SETA  &#TESRET(1)
         AIF   (&W1 EQ 0).RLP
&REGNME  SETC  '&#TESRET(2)'
         #TEST REGS=&REGNME
&ERRCODE SETA  1
         AIF   (&#TESERR NE 0).REGERR
.REGOK1  ANOP
&R1      SETA  &#TESRET(1)
&R2      SETA  &R1+3-&R1/14*16
&RG(&R2) SETC  '&REGNME'
&RSW(&R1+1) SETB (1)
         AIF   (&W1 EQ 1).RLP
         AIF   (&W1 EQ 2).TWOND
         MNOTE 4,'"&R(&C1)" CONTAINS EXCESS INFORMATION.'
.TWOND   ANOP
&REGNME  SETC  '&#TESRET(3)'
         #TEST REGS=&REGNME
&ERRCODE SETA  2
         AIF   (&#TESERR NE 0).REGERR
.REGOK2  ANOP
&W2      SETA  &#TESRET(1)
&RSW(&W2+1) SETB (1)
.ENTLP   AIF   (&R1 EQ &#TESRET(1)).ENTEND
&R1      SETA  &R1+1
&R2      SETA  &R2+1
         AIF   (&R1 LE 15).R1OK
&R1      SETA  0
.R1OK    AIF   (&R2 LE 16).R2OK
&R2      SETA  1
.R2OK    ANOP
&RG(&R2) SETC  '&@&R1'
         AGO   .ENTLP
.ENTEND  ANOP
&RG(&R2) SETC  '&REGNME'
         AGO   .RLP
.REGERR  AIF   (&#TESRET(1) GE 0 OR &#TESRET(1) LT 0).REGVALU
         MNOTE 0,'THE ABOVE ERROR IS NOT DUE TO A BUG IN THE MACRO.'
         MNOTE 8,'THE VALUE OF "&REGNME" IS NOT DETERMINABLE.'
         MEXIT
.REGVALU AIF   (&#TESRET(1) GE 0 AND &#TESRET(1) LE 15).REGOK
         MNOTE 8,'THE VALUE OF "&REGNME" IS OUTSIDE THE RANGE OF 0 ... *
               15'
         MEXIT
.REGOK   AIF   (&ERRCODE EQ 1).REGOK1
         AGO   .REGOK2
.RFIN    AIF   (NOT &#ENTPLI).RGOK
         AIF   (NOT &RSW(1)).RG0OK
         MNOTE 4,'&RG(3) NEEDED BY THE EXIT LINKAGE - NOT RESTORED.'
.RG0OK   AIF   (NOT &RSW(2)).RG1OK
         MNOTE 4,'&RG(4) NEEDED BY THE EXIT LINKAGE - NOT RESTORED.'
.RG1OK   ANOP
&RG(3)   SETC  ''
&RG(4)   SETC  ''
.RGOK    ANOP
.*
&RG(16)  SETC  ''
.*
         AIF   (K'&RC EQ 0).NORC
&RG2SAVE SETC  '&RG(2)'
&RG(2)   SETC  ''
         AIF   ('&RG2SAVE' NE '').RG2SOK
&RG2SAVE SETC  '&@.15'
.RG2SOK  ANOP
.*
         AIF   (NOT &RSW(16)).NOPRBLM
         MNOTE 4,'&RG2SAVE SET TO THE RETURN CODE - NOT RESTORED.'
.NOPRBLM ANOP
.*
         AIF   ('&RC' EQ '&RC(1)').RCNTRG
         #TEST REGS=&RC(1)
         AIF   (&#TESERR NE 0).LOADRC
         AIF   ('&#TESRET(1)' NE '13').RCOK
         MNOTE 4,'"RC=&RC" IS ILLOGICAL.'
.RCOK    AIF   ('&#TESRET(1)' EQ '15').NORC
.LOADRC  AIF   ('&RG(1)' EQ '' OR '&RG(3)' EQ '' OR &#ENTPLI).RCLR
&RCST    SETB  (1)
         AGO   .NORC
.RCLR    ANOP
&LNME    LR    &@.15,&RC(1)        LOAD THE RETURN CODE
&LNME    SETC  ''
         AGO   .NORC
.RCNTRG  ANOP
&RCLA    SETB  (1)
         AIF   ('&RG(3)' EQ '' OR '&RG(1)' EQ '').NORC
&RG(2)   SETC  '&RG2SAVE'
.NORC    ANOP
.*
         AIF   (NOT &#ENTPLI).NOTPLI2
&LNME    LR    &@.0,&@.13          COPY OUR DSA POINTER
&LNME    SETC  ''
.NOTPLI2 ANOP
.*
         AIF   (NOT &#ENTRNT OR '&RG(4)' EQ '').NOLRR1
&LNME    LR    &@.1,&@.13          GET SAVE AREA ADDRESS FOR FREEMAIN
&LNME    SETC  ''
.NOLRR1  ANOP
.*
         AIF   (&#ENTNUN).NOLSA                               DBC 06/84
&LNME    L     &@.13,4(,&@.13)     POINT TO THE HIGHER SAVE AREA
&LNME    SETC  ''                                             DBC 06/84
.NOLSA   ANOP                                                 DBC 06/84
         AIF   (NOT &RCST).NORCST
&LNME    ST    &RC(1),16(,&@.13)   STORE THE RC FOR LATER     DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
&RG(2)   SETC  '&RG2SAVE'
.NORCST  ANOP
.*
         AIF   (NOT &#ENTRNT).NTRENT
         AIF   (NOT &RCLA).RG2OK
&RG(2)   SETC  'X'
         AIF   ('&RG(1)&RG(3)' NE '').RG2OK
&RG(2)   SETC  ''
.RG2OK   ANOP
&RG4SAVE SETC  '&RG(4)'
&RG5SAVE SETC  '&RG(5)'
&RG(5)   SETC  'X'
&C1      SETA  0
.STMLP   AIF   (&C1 GE 4).STMEND
&C1      SETA  &C1+1
         AIF   ('&RG(&C1)' NE '').STMLP
&R1      SETA  &C1+13-(&C1+13)/16*16
&R2      SETA  &R1-1
&W1      SETA  &C1*4+8
.STMLP2  ANOP
&R2      SETA  &R2+1
         AIF   (&R2 LE 15).STMR2OK
&R2      SETA  0
.STMR2OK ANOP
&RG(&C1) SETC  '&@&R2'
&C1      SETA  &C1+1
         AIF   ('&RG(&C1)' EQ '').STMLP2
         AIF   (&R1 EQ &R2).ST
&C       SETC  '&@&R1,&@&R2,&W1.(&@.13)'                      DBC 06/84
&LNME    STM   &C                  SAVE AGAINST FREEMAIN      DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .STMLP
.ST      ANOP                                                 DBC 06/84
&LNME    ST    &@&R1,&W1.(,&@.13)  SAVE AGAINST FREEMAIN      DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .STMLP
.STMEND  ANOP
&RG(5)   SETC  '&RG5SAVE'
.GTR1M   AIF   ('&RG4SAVE' NE '').NOGTR1
&LNME    L     &@.1,8(,&@.13)      GET RSA PTR FOR FREEMAIN   DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
.NOGTR1  ANOP                                                 DBC 06/84
&LNME    L     &@.0,E&#.LEN        GET RSA LEN (AND SUBPOOL)  DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         MNOTE '         FREEMAIN R,A=(1),LV=(0)'             DBC 06/84
         FREEMAIN R,A=(1),LV=(0)                              DBC 06/84
.NTRENT  ANOP
.*
&C1      SETA  0
.LMLP    AIF   (&C1 GE 16).SETRCM
&C1      SETA  &C1+1
         AIF   ('&RG(&C1)' EQ '').LMLP
&R1      SETA  &C1
&W1      SETA  &C1*4+8
.LMLP2   ANOP
&C1      SETA  &C1+1
         AIF   ('&RG(&C1)' NE '').LMLP2
         AIF   (&R1 EQ &C1-1).L
&C       SETC  '&RG(&R1),&RG(&C1-1),&W1.(&@.13)'              DBC 06/84
&LNME    LM    &C                  RESTORE REGISTERS          DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .LMLP
.L       ANOP                                                 DBC 06/84
&C       SETC  '&RG(&R1),&W1.(,&@.13)'                        DBC 06/84
&LNME    L     &C                  RESTORE THE REGISTER       DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .LMLP
.SETRCM  ANOP
.*
         AIF   (NOT &RCLA).RETURN
         AIF   ('&RC' EQ '0').SR
&LNME    LA    &@.15,&RC           GET THE RETURN CODE        DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
         AGO   .RETURN
.SR      ANOP                                                 DBC 06/84
&LNME    SLR   &@.15,&@.15         ZERO THE RETURN CODE       DBC 06/84
&LNME    SETC  ''                                             DBC 06/84
.RETURN  ANOP
.*
         AIF   (NOT &#ENTPLI).NOTPLI3
&LNME    BALR  &@.1,&@.14          RETURN TO CALLER           DBC 06/84
         MEXIT
.NOTPLI3 ANOP
.*
         AIF   ('&MODE '(1,1) EQ 'R').RETBSM                  DBC 04/85
         AIF   ('&MODE '(1,1) EQ 'L').RETBR                   DBC 04/85
         MNOTE 8,'"MODE=&MODE" NOT RECOGNIZED.'               DBC 04/85
         MNOTE *,'"MODE=RESTORE" OR "MODE=LEAVE" WAS EXPECTED.' C 04/85
.*                                                            DBC 04/85
.RETBR   ANOP                                                 DBC 04/85
&LNME    BR    &@.14               RETURN                     DBC 10/84
&LNME    SETC  ''                                             DBC 04/85
         AGO   .MODEZ                                         DBC 04/85
.RETBSM  ANOP                                                 DBC 04/85
&LNME    BSM   0,&@.14             RESTORE AMODE AND RETURN   DBC 04/85
&LNME    SETC  ''                                             DBC 04/85
.MODEZ   ANOP                                                 DBC 04/85
.*                                                            DBC 06/84
         AIF   (NOT &#ENTRNT).END                             DBC 06/84
         AIF   ('&#ENTSP' EQ '').NOSPOOL                      DBC 06/84
         DS    0F                  ALIGN                      DBC 06/84
&C       SETC  'AL1(&#ENTSP),AL3(&#ENTSIZ)'                   DBC 06/84
E&#.LEN  DC    &C                  RSA SUBPOOL AND LENGTH     DBC 06/84
         MEXIT                                                DBC 06/84
.NOSPOOL ANOP
E&#.LEN  DC    A(&#ENTSIZ)         RSA LENGTH                 DBC 06/84
.END     MEND
./ ADD NAME=FREE
         MACRO
&NAME    FREE  &UNALC,&DSN=,&DDN=,&MEMBER=,&DISP=,&SYSOUT=,            X
               &ERROR=,&MF=AUTO,&PREFIX=,&FILE=,&F=,&DA=,&HOLD=
         GBLA  &RCPDYN            COUNTER FOR NO ENTRIES TO MACRO
         GBLA  &DTUO              OFFSET TO TEXT UNITS
         GBLA  &DTUPO             OFFSET TO TEXT UNIT POINTERS
         GBLB  &RCPS99(2)         TELL RCPDSECT NEED DSECTS
         GBLC  &DYNP              PREFIX FOR LABELS FOR THIS CALL
         GBLC  &DYNSP         NAME FOR AUTOMATIC STORAGE ALLOC
         LCLB  &DSECT             DSECT NEEDED FOR STORAGE, MF=E
         LCLC  &C,&T,&PAR
&RCPS99(1)     SETB           1
&RCPDYN  SETA  &RCPDYN+1          INCEREMENT COUNTER
&DYNP    SETC  'DYN&RCPDYN' SET DEFAULT PREFIX
&NAME    DS    0H
         AIF   ('&PREFIX' EQ '').TMF
         AIF   (K'&PREFIX LT 4).POK
         MNOTE 4,'PREFIX TOO LONG, 1ST 4 CHARS USED'
&DYNP    SETC  '&PREFIX'(1,4)
         AGO   .TMF
.POK     ANOP
&DYNP    SETC  '&PREFIX'
.TMF     AIF   ('&MF(1)' EQ 'G').GEN
         AIF   ('&MF' NE 'AUTO').TMFE
NAME     DYNSPACE             GET NAME FOR SPACE
         LA    R1,&DYNSP               LOAD ADDRESS OF PARAM LIST
         USING &DYNP.DS,R1             USE GENERATED DSECT
&T       SETC  'A'
&PAR     SETC  '&DYNSP+4'
&DSECT   SETB  1
         AGO   .START
.TMFE    AIF   ('&MF(2)' NE '').E2OK
         MNOTE 4,'PLIST ADDRESS OMITTED, MF=G USED'
         AGO   .GEN
.E2OK    ANOP
&DSECT   SETB  1
         AIF   ('&MF(2)' EQ '(').RMFE
         LA    R1,&MF(2)               LOAD PARAM LIST ADDRESS
&T       SETC  'A'
&PAR     SETC  '&MF(2)+4'
         USING &DYNP.DS,R1             USE GENERATED DSECT
         AGO   .START
.RMFE    AIF   ('&MF(2)' EQ '(R1)' OR '&MF(2)' EQ '(1)').START
&PAR     SETC  '&MF(2)'(2,K'&MF(2)-2)
&T       SETC  'R'
         LR    R1,&PAR                 LOAD S99 PARAM LIST ADDRESS
&PAR     SETC  '4&MF(2)'
         USING &DYNP.DS,R1             USE GENERATED DSECT
         AGO   .START
.GEN     LA    R1,&DYNP.RBP            LOAD ADDRESS OF S99 RBP
&T       SETC  'A'
&PAR     SETC  '&DYNP.RB'
.START   LA    R15,&DYNP.RB            LOAD ADDRESS OF S99 RB
         USING S99RB,R15
         ST    R15,0(R1)               AND STORE IN RB POINTER
         XC    4(&DYNP.LEN-4,R1),4(R1) ZERO PARAMETER LIST
         MVI   S99RBLN,20              MOVE IN LIST LENGTH
         MVI   S99VERB,S99VRBUN        MOVE IN VERB CODE
         LA    R14,&DYNP.TUP           LOAD ADDRESS OF TU POINTERS
         ST    R14,S99TXTPP            STORE ADDRESS IN S99 RB
         LA    R15,&DYNP.TU            POINT TO SPACE FOR TEXT UNITS
         USING S99TUNIT,R15
&DTUO    SETA  0
&DTUPO   SETA  0
         AIF   ('&DSN&DA' NE '').DSN
         AIF   ('&SYSOUT' NE '').SYSOUT
.TDDN    AIF   ('&DDN&FILE&F' NE '').DDN
.TDISP   AIF   ('&DISP' NE '').DISP
.TUNALC  AIF   ('&UNALC' NE '').PERM
.THOLD   AIF   ('&HOLD' NE '').HOLD
         AGO   .SVC99
.DSN     RCPFDSN &DSN&DA,&MEMBER
         AGO   .TDDN
.SYSOUT  RCPFSYS &SYSOUT
         AGO   .TDDN
.DDN     RCPFDDN &DDN&F&FILE
         AGO   .TDISP
.DISP RCPFDISP &DISP
         AGO   .TUNALC
.PERM    RCPUNALC
         AGO   .THOLD
.HOLD    RCPFHOLD &HOLD
.SVC99   ANOP
&DTUPO   SETA  &DTUPO-4
         SPACE
         MVI   &DYNP.TUP+&DTUPO,X'80'  SET HIGH ORDER BIT ON TEXT PTRS
         MVI   &DYNP.RBP,X'80'         SET HIGH ORDER BIT ON RB PTR
         RCPSR2 UNSAVE
&DTUPO   SETA  &DTUPO+4
         AIF   (NOT &DSECT).DYNA
         DROP  R1,R15                  DEACTIVATE ADDRESSABILITY
.DYNA    DYNALLOC
         AIF   ('&ERROR' EQ '').RESERVE
         AIF   ('&PAR' EQ '').LTR
         L&T   R14,&PAR                 LOAD REG 14 WITH ADDRESS OF RB
         AIF   (NOT &DSECT).LTR
         USING &DYNP.RB,R14            SET UP ADDRESSABILITY
.LTR     LTR   R15,R15                 TEST RETURN CODE
         BNZ   &ERROR                  BRANCH IF NON ZERO
**       NOTE.  R14 POINTS TO REQUEST BLOCK, R15 HAS RETURN CODE     **
.RESERVE AIF   (&DSECT).RESDS
         SPACE
***********************************************************************
**       RESERVE SPACE FOR DYNALLOC DATA                             **
***********************************************************************
         RCPDS
.SSP     ANOP
&DYNP.RBP DS   F                       SVC 99 REQ BLOCK POINTER
&DYNP.RB  DS   5F                      SVC 99 REQUEST BLOCK
&DYNP.TUP DS   CL&DTUPO                SPACE FOR TEXT POINTERS
         AIF   (&DTUO EQ 0).DTU11
&DYNP.TU  DS   CL&DTUO                 SPACE FOR TEXT UNITS
         AGO   .DTU10
.DTU11   ANOP
&DYNP.TU  DS   0C                      NO SPACE NEEDED FOR TEXT UNITS
.DTU10   ANOP
&DYNP.LEN EQU  *-&DYNP.RBP             LENGTH OF SPACE USED
         AIF   (&DSECT).DSP
         RCPDS
         SPACE 3
         AGO   .EXIT
.RESDS   ANOP
         AIF   ('&DYNSP' EQ '').SP3
         DYNSPACE ADD
.SP3     SPACE
&DYNP.DS DSECT                         DSECT TO MAP SVC 99 DATA
         AGO   .SSP
.DSP     AIF   ('&MF(3)' EQ '').END1
&MF(3)   EQU   &DYNP.LEN               LENGTH OF AREA
.END1    ANOP
&SYSECT  CSECT
         SPACE 3
.EXIT    MEND
./ ADD NAME=GSAMCALL
         MACRO
&LABEL   GSAMCALL &VERB,                                               *
               &MF=,                                                   *
               &POOL=,                                                 *
               &FILE=,                                                 *
               &OPTIONS=,                                              *
               &BUFFER=,                                               *
               &BUFFLEN=,                                              *
               &RECLEN=,                                               *
               &MSGAREA=,                                              *
               &RBA=,                                                  *
               &PASS=
.************************************************************
.*                                                          *
.* MACRO NAME = GSAMCALL                                    *
.*                                                          *
.* DESCRIPTIVE NAME = FAKE GSAM CALL MACRO.                 *
.*                                                          *
.* FUNCTION = MODIFIES A GSB AND "BALR"S TO A GSAM          *
.*            SIMULATION ROUTINE WHICH TRANSLATES THE GSAM  *
.*            CALL INTO A SIMPLE VSAM CALL.                 *
.*                                                          *
.* PROCESSOR = ASSEMBLER XF                                 *
.*                                                          *
.* DATA AREAS = GSB                                         *
.*                                                          *
.************************************************************
         LCLC  &SET
         LCLC  &RESET
         LCLA  &I,&GSAMSVC
         LCLB  &ZERO
         LCLC  &R
.************************************************************
.*       CHANGE THE FOLLOWING LINE IF A DIFFERENT SVC       *
.*       NUMBER IS ASSIGNED AT YOUR INSTALLATION            *
.************************************************************
&GSAMSVC SETA  999                 IMPLIES GSAM NOT AVAILABLE
         AIF   (K'&LABEL EQ 0).NOLAB
&LABEL   DS    0H
.NOLAB   ANOP
.*       LOCATE THE GSB
         AIF   ('&MF(1)' NE 'E').BADMF
         AIF   ('&MF(2)' EQ '').BADMF
         AIF   ('&MF(2)' EQ '(1)').GOTMF
         IHBSETR &MF(2),1
.GOTMF   ANOP
.*       PROCESS OPTIONS LOOP
&I       SETA  1
.OPTLOOP AIF   ('&OPTIONS(&I)' EQ '').ENDOPT
         AIF   ('&OPTIONS(&I)' NE 'RESET').NZERO
&ZERO    SETB  (1)
         AGO   .NEXTOPT
.NZERO   AIF   ('&OPTIONS(&I)' NE 'INITIAL').NA1
         MVC   GSBID-GSB(4,1),=CL4'GSB'
         XC    4(GSBLEN-4,1),4(1)
&ZERO    SETB  (1)
         AGO   .NEXTOPT
.NA1     AIF   ('&OPTIONS(&I)' NE 'WAIT').NA2
&SET     SETC  '&SET+GSBWAIT'
         AGO   .NEXTOPT
.NA2     AIF   ('&OPTIONS(&I)' NE 'NOWAIT').NB1
&RESET   SETC  '&RESET-GSBWAIT'
         AGO   .NEXTOPT
.NB1     AIF   ('&OPTIONS(&I)' NE 'UPDATE').NB2
&SET     SETC  '&SET+GSBUPD'
         AGO   .NEXTOPT
.NB2     AIF   ('&OPTIONS(&I)' NE 'NOUPDATE').NC1
&RESET   SETC  '&RESET-GSBUPD'
         AGO   .NEXTOPT
.NC1     AIF   ('&OPTIONS(&I)' NE 'ASYNC').NC2
&SET     SETC  '&SET+GSBASYNC'
         AGO   .NEXTOPT
.NC2     AIF   ('&OPTIONS(&I)' NE 'SYNC').ND1
&RESET   SETC  '&RESET-GSBASYNC'
         AGO   .NEXTOPT
.ND1     AIF   ('&OPTIONS(&I)' NE 'DIR').ND2
&SET     SETC  '&SET+GSBDIR'
         AGO   .NEXTOPT
.ND2     AIF   ('&OPTIONS(&I)' NE 'SEQ').NE1
&RESET   SETC  '&RESET-GSBDIR'
         AGO   .NEXTOPT
.NE1     AIF   ('&OPTIONS(&I)' NE 'BACK').NE2
&SET     SETC  '&SET+GSBBACK'
         AGO   .NEXTOPT
.NE2     AIF   ('&OPTIONS(&I)' NE 'FWD').NF1
&RESET   SETC  '&RESET-GSBBACK'
         AGO   .NEXTOPT
.NF1     AIF   ('&OPTIONS(&I)' NE 'PTY').NF2
&SET     SETC  '&SET+GSBPTY'
         AGO   .NEXTOPT
.NF2     AIF   ('&OPTIONS(&I)' NE 'NOPTY').NH1
&RESET   SETC  '&RESET-GSBPTY'
         AGO   .NEXTOPT
.NH1     AIF   ('&OPTIONS(&I)' NE 'ABTERM').NH2
&SET     SETC  '&SET+GSBABTRM'
         AGO   .NEXTOPT
.NH2     AIF   ('&OPTIONS(&I)' NE 'NOABTERM').NI1
&RESET   SETC  '&RESET-GSBABTRM'
         AGO   .NEXTOPT
.NI1     AIF   ('&OPTIONS(&I)' NE 'DEBUG').NI2
&SET     SETC  '&SET+GSBDEBUG'
         AGO   .NEXTOPT
.NI2     AIF   ('&OPTIONS(&I)' NE 'NODEBUG').NJ1
&RESET   SETC  '&RESET-GSBDEBUG'
         AGO   .NEXTOPT
.NJ1     MNOTE 8,'GSAM002E INVALID OPTION &OPTIONS(&I)'
.NEXTOPT ANOP
&I       SETA  &I+1
         AGO   .OPTLOOP
.ENDOPT  ANOP
.*       NOW EXPAND OUT OPTION BIT SETTING INSTRUCTIONS
         AIF   (&ZERO).SET2
         AIF   ('&SET' EQ '').SET1
&SET     SETC  '&SET'(2,K'&SET-1)
         OI    GSBFLGS-GSB(1),&SET
.SET1    AIF   ('&RESET' EQ '').SET3
         NI    GSBFLGS-GSB(1),X'FF'&RESET
         AGO   .SET3
.SET2    AIF   ('&SET' EQ '').SET3
&SET     SETC  '&SET'(2,K'&SET-1)
         MVI   GSBFLGS-GSB(1),&SET
.SET3    ANOP
         AIF   ('&POOL' EQ '').NOPOOL
&R       SETC  '&POOL(1)'
         AIF   ('&R' NE '&POOL').GOTPOOL
         LA    15,&POOL
&R       SETC  '15'
.GOTPOOL ANOP
         STH   &R,GSBPOOL-GSB(,1)
.NOPOOL  AIF   ('&FILE' EQ '').NOFILE
         AIF   ('&FILE'(1,1) NE '''').GETFILE
         MVC   GSBFILE-GSB(8,1),=CL8&FILE
         AGO   .NOFILE
.GETFILE ANOP
&R       SETC  '&FILE(1)'
         AIF   ('&R' NE '&FILE').GOTFILE
         LA    15,&FILE
&R       SETC  '15'
.GOTFILE ANOP
         MVC   GSBFILE-GSB(8,1),0(&R)
.NOFILE  AIF   ('&BUFFER' EQ '').NOBUF
&R       SETC  '&BUFFER(1)'
         AIF   ('&R' NE '&BUFFER').GOTBUF
         AIF   ('&BUFFER' NE '-1').GETBUF
         SR    15,15
         BCTR  15,0
         AGO   .GETBUF2
.GETBUF  ANOP
         LA    15,&BUFFER
.GETBUF2 ANOP
&R       SETC  '15'
.GOTBUF  ANOP
         ST    &R,GSBBUFA-GSB(,1)
.NOBUF   AIF   ('&BUFFLEN' EQ '').NOBUFL
&R       SETC  '&BUFFLEN(1)'
         AIF   ('&R' NE '&BUFFLEN').GOTBUFL
         LA    15,&BUFFLEN
&R       SETC  '15'
.GOTBUFL ANOP
         ST    &R,GSBBUFL-GSB(,1)
.NOBUFL  AIF   ('&RECLEN' EQ '').NORECL
&R       SETC  '&RECLEN(1)'
         AIF   ('&R' NE '&RECLEN').GOTRECL
         LA    15,&RECLEN
&R       SETC  '15'
.GOTRECL ANOP
         ST    &R,GSBRECL-GSB(,1)
.NORECL  AIF   ('&MSGAREA' EQ '').NOMSG
&R       SETC  '&MSGAREA(1)'
         AIF   ('&R' NE '&MSGAREA').GOTMSG
         LA    15,&MSGAREA
&R       SETC  '15'
.GOTMSG  ANOP
         ST    &R,GSBMSGA-GSB(,1)
.NOMSG   AIF   ('&RBA' EQ '').NORBA
&R       SETC  '&RBA(1)'
         AIF   ('&R' NE '&RBA').GOTRBA
         LA    15,&RBA
&R       SETC  '15'
.GOTRBA  ANOP
         MVC   GSBRBA-GSB(4,1),0(&R)
.NORBA   AIF   ('&PASS' EQ '').NOPASS
&R       SETC  '&PASS(1)'
         AIF   ('&R' NE '&PASS').GOTPASS
         LA    15,&PASS
&R       SETC  '15'
.GOTPASS ANOP
         ST    &R,GSBPASSW-GSB(,1)
.NOPASS  AIF   ('&VERB' EQ 'MODIFY').EXIT
         AIF   ('&VERB' NE 'READ').NREAD
         SR    0,0                 INDICATE READ
         AGO   .SVC
.NREAD   AIF   ('&VERB' NE 'WRITE').NWRITE
         LA    0,1                 INDICATE WRITE
         AGO   .SVC
.NWRITE  AIF   ('&VERB' NE 'REWRITE').NREWRIT
         LA    0,2                 INDICATE REWRITE
         AGO   .SVC
.NREWRIT AIF   ('&VERB' NE 'DELETE').NDEL
         LA    0,3                 INDICATE DELETE
         AGO   .SVC
.NDEL    AIF   ('&VERB' NE 'RELEASE').NRLSE
         LA    0,4                 INIDCATE RELEASE
         AGO   .SVC
.NRLSE   AIF   ('&VERB' NE 'MONITOR').NMON
         LA    0,5                 INDICATE MONITOR
         AGO   .SVC
.NMON    AIF   ('&VERB' NE 'MAINTAIN').NMAINT
         LA    0,6                 INDICATE MAINTAIN
         AGO   .SVC
.NMAINT  AIF   ('&VERB' NE 'FINISH').VERBERR
         LA    0,7
         AGO   .SVC
.VERBERR MNOTE 8,'GSAM001E UNRECOGNIZED VERB: &VERB'
.SVC     AIF   (&GSAMSVC GT 255).FAKEIT
         SVC   &GSAMSVC            ISSUE SVC
         MEXIT
.FAKEIT  ANOP
         L     R15,=V(FAKEGSAM)    --> SUBSTITUTE ROUTINE
         BALR  R14,R15             GO PERFORM THE VSAM I/O
         MEXIT
.BADMF   MNOTE 8,'GSAM003E IMPROPER OR OMITTED ''MF'' PARAMETER'
.EXIT    MEND
./ ADD NAME=GSB
         MACRO
         GSB   &DSECT=YES
.************************************************************
.*                                                          *
.* MACRO NAME = GSB                                         *
.*                                                          *
.* DESCRIPTIVE NAME = GSAM SIMULATION ROUTINE'S PARAMETER   *
.*                    BLOCK.                                *
.*                                                          *
.* PROCESSOR = ASSEMBLER XF                                 *
.*                                                          *
.************************************************************
*************************************************************
*                                                           *
*        GSB -- GSAM SVC PARAMETER BLOCK                    *
*                                                           *
*************************************************************
         SPACE 1
         IEZBITS ,
         AIF   ('&DSECT' NE 'YES').NODSECT
GSB      DSECT ,
         AGO   .YESDSEC
.NODSECT ANOP
GSB      DS    0F                  ALIGNMENT
.YESDSEC ANOP
GSBID    DC    CL4'GSB'            MUST BE FILLED IN
GSBFILE  DS    CL8                 FILE NAME DEFINED TO GSRTASK
GSBPOOL  DS    H                   VALUES 0-7. KEY OF GSR POOL.
GSBFLGW  DS    0H                  OPTION BITS FOR USER -
GSBFLG1  DS    X                    IN HALFWORD SO THEY CAN -
GSBFLGS  DS    X                     BE SET IN FORTRAN AND COBOL
GSBWAIT  EQU   BIT0                INDICATION THAT USER
*                                  WANTS TO WAIT FOR A
*                                  RECORD. APPLICABLE ONLY
*                                  IF UPDATE INDICATOR SET.
GSBUPD   EQU   BIT1                USER WANTS TO UPDATE THE
*                                  RECORD. GSAM WILL OBTAIN
*                                  OWNERSHIP OF THE RECORD
GSBASYNC EQU   BIT2                THE USER WANTS TO
*                                  ASYNCHRONOUSLY CONTEND FOR
*                                  RECORD OWNERSHIP.
*                                  GSBUPD MUST BE ON,
*                                  GSBWAIT MUST BE OFF
GSBDIR   EQU   BIT3                THIS IS A DIRECT REQUEST.
*                                  FOR A KSDS, A KEY IS IN
*                                  THE BUFFER. OTHERWISE, IT
*                                  IS IN GSBRBA.
GSBBACK  EQU   BIT4                THIS IS A 'BACKWARDS'
*                                  OPERATION. THE USER WANTS
*                                  TO SEQUENTIALLY READ RECORDS
*                                  IN DESCENDING ORDER OF KEY OR
*                                  ADDRESS. GSBDIR MUST BE OFF.
GSBPTY   EQU   BIT5                PRIORITY REQUEST. CALLER
*                                  MUST BE IN SYSTEM KEY OR
*                                  SUPERVISOR STATE. UPDATE BIT
*                                  MUST BE ON. CALLER WILL
*                                  STEAL RECORD OWNERSHIP FROM
*                                  ANYONE WHO HOLDS IT.
GSBABTRM EQU   BIT6                ABEND MY TASK WHEN GSAM
*                                  GOES DOWN OR THIS FILE IS
*                                  PURGED
GSBDEBUG EQU   BIT7                INVOKE DBC (REQUIRES AUTH
*                                  LEVEL 2 AND DEBUG FLAG)
GSBINDX  DS    H                   INDEX OF OUR GSAM PLACEHOLDER
*                                  SET TO ZERO BEFORE FIRST CALL
*                                  USER MUST NOT SUBSEQUENTLY
*                                  MODIFY THIS FIELD
GSBRSV1  DS    H                   RESERVED
GSBBUFL  DS    F                   TOTAL LENGTH OF BUFFER WHOSE
*                                  ADDRESS IS SPECIFIED IN GSBBUFA.
*                                  SPECIFIED BY USER.
*                                  ON A READ REQUEST, BUFFER IS
*                                  PADDED WITH BLANKS FOR A LENGTH OF
*                                  GSBBUFL.
*                                  CANNOT BE GREATER THAN INSTALLATION
*                                  SPECIFIED MAX.
GSBBUFA  DS    A                   -> USER BUFFER FOR READS, REWRITES,
*                                  DELETES.
*                                  IF F'-1' THEN THE MSG AREA & BUFFER
*                                  FOLLOW THE GSB (FOR LANGUAGES THAT
*                                  DO NOT SUPPORT THE PTR DATA TYPE)
GSBRECL  DS    A                   FOR READ, LENGTH OF BUFFER RETURNED
*                                  TO USER. FOR REWRITE, WRITE, LENGTH
*                                  OF RECORD, SUPPLIED BY USER.
*                                  MUST BE >0, <=GSBBUFL
GSBMSGA  DS    A                   ->128 BYTE AREA FOR RETURNING I/O
*                                  ERROR MESSAGES, OR NULL.
GSBVSAM  DS    F                   VSAM FEEDBACK CODE THAT CAUSED
*                                  THE SVC'S NON-0 RETURN CODE. 0 IF
*                                  NONE APPLIES.
*                                  OR, ON SUCCESSFUL COMPLETION,
*                                  THE VSAM FEEDBACK CODE FOR 0
*                                  RETURN CODE. (SEE VSAM MANUAL).
GSBVSFN  DS    F                   VSAM FUNCTION CODE ON LOGICAL
*                                  OR PHYSICAL ERROR. -1 WHEN NONE
*                                  APPLIES.
GSBECB   DS    A                   ECB WHICH WILL BE POSTED
*                                  WHEN ASYNCHRONOUS REQUEST FOR
*                                  RECORD OWNERSHIP COMPLETES
GSBRBA   DS    F                   RBA OF RECORD FOR READ, REWRITE,
*                                  WRITE (RETURNED BY SVC).
*                                  ONLY TRUE WHEN GSBPATH IS OFF
*                                  RBAS ARE NOT RETURNED BY VSAM FOR
*                                  A PATH.
GSBPASSW DS    CL8                 PASSWORD-FOR EXAMINATION BY
*                                  INSTALLATION EXITS
*        THE GSBFIL.. FIELDS ARE RETURNED TO THE USER
*        WHENEVER THE GSAM FILE SPECIFIED IS FOUND BY THE SVC.
GSBFILKL DS    F                   KEY LENGTH OF FILE. 0 FOR ESDS.
GSBFILKO DS    F                   KEY OFFSET IN RECS. 0 FOR ESDS.
*                                  0-INDEXED.
GSBFILRL DS    F                   MAXINUM RECORD LENGTH FOR THE FILE.
GSBFILCL DS    F                   CONTROL INTERVAL LENGTH OF FILE
*                                  (RETURNED W/ OTHER GSBFIL.. FIELDS)
GSBFILAC DS    C                   ACCESS CODE OF FILE
*                                  RETURNED WITH OTHER GSBFIL.. INFO
GSBACC1  EQU   C'1'                READ-ONLY ACCESS.
GSBACC2  EQU   C'2'                UPDATE ACCESS.
GSBACC3  EQU   C'3'                WRITE-ONLY,TO AN ESDS.
GSBFILFG DS    C                   FLAGS RETURNED,DESCRIBING FILE
*                                  (ALONG WITH OTHER GSBFIL.. FIELDS)
GSBRD    EQU   C'R'                READ ONLY
GSBWT    EQU   C'W'                WRITE ONLY
GSBRDWT  EQU   C'*'                FULL ACCESS
GSBNO    EQU   C'C'                NO ACCESS
GSBLEN   EQU   *-GSB
         SPACE 3
*************************************************************
* OPTIONAL FIELDS (IF GSBBUFA IS F'-1')                     *
*************************************************************
         SPACE 1
GSBMSGX  DS    CL128               CONTIGUOUS MESSAGE AREA
GSBBUFX  DS    0C                  CONTIGUOUS BUFFER.
         SPACE 3
*************************************************************
*        RETURN CODES TO CALLER OF GSAM SVC:                *
*        (NOTE THAT MEANINGS OF RETURN CODES DEPEND ON      *
*        THE VSAM RETURN CODE-GSBVSAM).                     *
*************************************************************
         SPACE 1
GSAMEKEY EQU   4                   KEY OR ENDFILE CONDITION
GSAMEREC EQU   8                   RECORD CONDITION
GSAMENAV EQU   12                  SOME GSR OBJECT UNAVAILABLE
GSAMEFIL EQU   16                  UNDEFINEDFILE CONDITION.
GSAMEPOS EQU   20                  POSITIONING ERROR.
GSAMEDS  EQU   24                  DATASET FULL OR BAD.
GSAMEAUT EQU   28                  AUTHORIZATION ERROR.
GSAMEACC EQU   32                  WRONG KIND OF ACCESS FOR THISFILE.
GSAMECOR EQU   36                  NOT ENOUGH CORE AVAILABLE.
GSAMEPRM EQU   40                  PARMS BAD OR INCONSISTANT.
GSAMEIO  EQU   44                  I/O PHYSICAL ERROR
GSAMEBUG EQU   48                  BUG IN THE SVC
GSAMEUBG EQU   52                  BUG IN INSTALLATION EXIT
         MEND
./ ADD NAME=OLDPDPTP
*    TEMPORARY ADDITION TO ALLOW MVSSUPA TO ASSEMBLE
*
 GBLC &SYS,&COMP   *****DEBUG*****
&SYS     SETC  'S370' *****DEBUG*****
&COMP    SETC  'C/370'  *****DEBUG*****
./ ADD NAME=#PARMS
         MACRO
&NME     #PARMS &P1,&P2,&P3,&P4,&P5,&P6,&P7,&P8,&P9,&P10,              *
               &P11,&P12,&P13,&P14,&P15,&P16,&P17,&P18,&P19,&P20,      *
               &P21,&P22,&P23,&P24,&P25,&P26,&P27,&P28,&P29,&P30,      *
               &P31,&P32,&P33,&P34,&P35,&P36,&P37,&P38,&P39,&P40,      *
               &SVID=,&WKID=,&SUBAD=,&PFIX=
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $PARMS TO #PARMS.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - CHARACTERS NOT WITHIN APOSTROPHIES ARE NOW
.*                    UPCASED.
.*                  - FIXED BUG: THE ERROR RETURN FROM THE PARM
.*                    SUBROUTINE GENERATED BY THE MACRO WAS NEVER
.*                    TAKEN.
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - JUNE 17, 1975
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING IT
.* MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO WAS WRITTEN TO GENERATE A GENERALIZED PARM FIELD SCAN
.* OUT SUBROUTINE. IT DOES THE FOLLOWING:
.* - IT DETERMINES THE EXISTANCE OF A PARM FIELD.
.* - IT INTERFACES WITH A MESSAGE PRINTING SUBROUTINE TO PRINT
.*   APPROPIATE MESSAGES INCLUDING AN ECHO OF THE PARM FIELD AS WELL AS
.*   ANY ERROR MESSAGES THAT MIGHT BE APPROPIATE.
.* - IT SCANS FOR AND ISSOLATES EACH ENTRY IN THE PARM FIELD,
.*   IDENTIFIES IT AGAINST A LIST OF GIVEN KEYWORDS, AND THEN BRANCHES
.*   TO A GIVEN SPECIALIZED HANDLER ROUTINE.
.* - IF BY THE END OF THE SCAN OUT ANY ERRORS HAVE BEEN DETECTED, THE
.*   ROUTINE WILL MAKE AN ERROR RETURN TO ITS CALLER.
.*
.*
.*
.* THE MACRO CALL USES THE FOLLOWING ARGUEMENTS:
.*
.* THE NAME FIELD (OPTIONAL) -
.*   THIS SPECIFIES BOTH THE NAME OF THE GENERATED SUBROUTINE AND THE
.* PREFIX TO USE FOR ALL GENERATED STATEMENT LABELS. IT SHOULD NOT BE
.* LONGER THAN 4 CHARACTERS. IF OMITTED, THEN THE DEFAULT OF "PARM" IS
.* USED.
.*
.* THE POSITIONAL PARAMETERS (AT LEAST ONE REQUIRED) -
.*   FROM ONE TO 40 POSITIONAL PARAMETERS MAY BE GIVEN. EACH OF THEM
.* MUST CONSIST OF A SUBLIST OF TWO ITEMS. THE FIRST MUST BE A PARM
.* FIELD ENTRY KEYWORD. THE SECOND MUST BE THE ADDRESS (A-TYPE) OF AN
.* ASSOCIATED HANDLING ROUTINE. THIS ROUTINE IS GIVEN CONTROL IF THE
.* ASSOCIATED KEYWORD IS FOUND IN THE PARM FIELD.
.*
.* THE SUBAD= OPERAND (OPTIONAL) -
.*   THIS SPECIFIES THE NAME OF A MESSAGE PRINTING SUBROUTINE TO BE
.* USED BY THE SCAN OUT ROUTINE. IF OMITTED, THEN THE DEFAULT IS TO USE
.* THE PRINTING SUBROUTINE USED BY THE NEXT PREVIOUS #PARMS OR $PUT
.* MACRO.
.*   THE FOLLOWING INFORMATION IS PASSED TO THE MESSAGE PRINTING
.* SUBROUTINE:
.*       R14 --> RETURN ADDRESS
.*       R1 --> A MESSAGE BLOCK. THE FIRST BYTE CONTAINS THE LENGTH OF
.*              THE MESSAGE. THE REMAINDER OF THE BLOCK CONTAINS THE
.*              MESSAGE ITSELF (INCLUDING ASA CARRAGE CONTROL).
.*   THE MESSAGE PRINTING ROUTINE SHOULD RESTORE ALL REGISTERS UPON
.* RETURN.
.*
.* THE SVID= OPERAND (OPTIONAL) -
.*   THIS MUST SPECIFY THE NAME OF AN ALIGNED 8-WORD REGISTER SAVE
.* AREA. IF OMITTED, THEN A SAVE AREA IS GENERATED BY THE MACRO.
.*
.* THE WKID= OPERAND (OPTIONAL) -
.*   THIS MUST SPECIFY THE NAME OF AN ALIGNED 1-WORD WORK AREA. IF
.* OMITTED, THEN A WORK AREA IS GENERATED BY THE MACRO.
.*
.* THE PFIX= OPERAND (OPTIONAL) -
.*   THIS MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS EXPANSION. THIS
.* IS SO THAT THE USE OF THE REGISTERS WILL BE INDICATED IN THE CROSS
.* REFERENCE LISTING. THE PFIX= OPERAND CAN BE USED TO CONTROL THE SET
.* OF EQUATES USED. FOR EXAMPLE, IF "PFIX=GPR" IS SPECIFIED, THEN
.* "GPR1" WILL BE USED WHENEVER THE EXPANSION REFERS TO REGISTER 1.
.*   IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF EQUATES DEFINED
.* BY THE FIRST PRIOR #REGS MACRO WILL BE USED. IF THERE IS NO PRIOR
.* #REGS MACRO, THEN NO EQUATES WILL BE USED.
.*
.*
.*
.* SUBROUTINE PROCESSING INFORMATION
.*
.*       INPUTS:
.*             R14 --> RETURN ADDRESS
.*             R1 --> ANY OS STANDARD PARM FIELD
.*
.*       OUTPUTS:
.*             ALL REGISTERS ARE RESTORED.
.*             THE PARM FIELD IS PROCESSED AND MAY BE DESTROYED.
.*       EXITS:
.*             BRANCH AND LINK TO SPECIFIED HANDLING SUBROUTINES UPON
.*             RECOGNITION OF VALID KEYWORDS.
.*
.*       RETURNS:
.*             NORMAL:  " BR R14 "
.*             ERROR:  " B 4(,R14) "
.*       ATTRIBUTES:
.*             NONE (NOT EVEN SERIEALLY REUSABLE)
.*
.* METHOD OF SCAN -
.*   THE PARM FIELD IS CONSIDERED TO BE A LIST OF KEYWORD ENTRIES.
.* POSITIONAL ENTRIES ARE NOT CONSIDERED.
.*   EACH ENTRY MUST START WITH ONE OF THE KEYWORDS DEFINED IN THE
.* MACRO CALL. ANY CHARACTERS THAT MIGHT BE FOUND IN ADDITION TO THE
.* KEYWORD (I.E. ADJACENT TO AND IMMEDIATELY TO THE RIGHT OF THE
.* KEYWORD) ARE CONSIDERED TO BE THE VALUE ASSIGNED TO THE KEYWORD.
.*   NOTE, THIS ROUTINE DOES NOT EXPLICITLY TEST FOR ANY SEPERATOR
.* CHARACTER BETWEEN THE KEYWORD AND ITS VALUE. SUCH A CHARACTER, IF
.* DESIRED, MUST BE DEFINED AS PART OF THE KEYWORD (E.G. "SEQ=").
.*   ITEMS OF SYNTAX RECOGNIZED BY THIS ROUTINE ARE COMMAS,
.* APOSTROPHIES, AND PARENTHESES. APOSTROPHIES MUST OCCURE IN MATCHING
.* PAIRS. COMMAS AND PARENTHESES FOULD WITHIN MATCHING APOSTROPHIES ARE
.* IGNORED. PARENTHESES NOT FOUND WITHIN APOSTROPHIES MUST BE BALANCED.
.* COMMAS FOUND WITHIN PARENTHESES ARE IGNORED.
.*   COMMAS NOT FOUND WITHING MATCHING APOSTROPHIES OR PARENTHESES ARE
.* TREATED AS PARM FIELD ENTRY DELIMITERS.
.*   EACH PARM FIELD ENTRY IS ISSOLATED AT ITS DELIMITERS AND THEN
.* CHECKED FOR A MATCH IN THE MACRO'S KEYWORD LIST. IF A MATCH IS
.* FOUND, THEN THE ASSOCIATED HANDLING ROUTINE IS CALLED WITH THE
.* FOLLOWING INPUTS:
.*       R14 --> RETURN ADDRESS
.*       R15 --> HANDLER ENTRY ADDRESS
.*       R0 = LENGTH OF THE KEYWORD'S VALUE (IF ANY). IF NO VALUE IS
.*            GIVEN, THEN R0 = 0.
.*       R1 --> FIRST BYTE PAST THE KEYWORD - I.E. AT THE VALUE (IF
.*              ANY).
.*   EACH HANDLER MUST MEET THE FOLLOWING OUTPUT REQUIREMENTS:
.*       - ALL REGISTERS MUST BE RESTORED EXCEPT THAT FOR ERROR RETURNS
.*         R1 MUST POINT TO THE LOCATION OF THE ERROR.
.*   THE FOLLOWING RETURNS ARE PERMITTED:
.*       NORMAL -  " BR R14 "
.*       ERROR -  " B 4(,R14) "
.*
.*
.*
.* INNER MACROS USED - #TEST, #PUT
.*
         GBLC  &#TESRET(20)
         LCLA  &A1,&A2
         LCLC  &N,&@,&SAVEA,&WORKA,&C1
&N       SETC  'PARM'
         AIF   (K'&NME EQ 0).GOTNME
&N       SETC  '&NME'
.GOTNME  ANOP
&@       SETC  '&PFIX'
         AIF   (K'&PFIX NE 0).GOTPFIX
         #TEST PFIX=
&@       SETC  '&#TESRET(1)'
.GOTPFIX ANOP
&SAVEA   SETC  '&SVID'
         AIF   (K'&SVID NE 0).GOTSAVE
&SAVEA   SETC  '&N.SAVE'
.GOTSAVE ANOP
&WORKA   SETC  '&WKID'
         AIF   (K'&WKID NE 0).GOTWORK
&WORKA   SETC  '&N.WORK'
.GOTWORK ANOP
* PARM FIELD SCAN-OUT ROUTINE
*        INPUTS:
*              R14 --> RETURN ADDRESS
*              R1 --> PARM FIELD
         SPACE 1
&N       STM   &@.14,&@.5,&SAVEA   SAVE REGISTERS
         LA    &@.4,0(,&@.1)       PNT TO PARM FIELD (CLEAR HI-BYTE)
         LH    &@.3,0(,&@.4)       GET ITS LENGTH
         LTR   &@.3,&@.3           PARM FIELD GIVEN?
         BZ    &N.RET              NO, GO RETURN TO CALLER
         #PUT  &N.TITL,SUBAD=&SUBAD YES, PRINT AN INTRO LINE
         LA    &@.2,1              GETA BXLE INCREMENT FOR THE SCAN
         AR    &@.3,&@.2           GET LENGTH FOR ECHO MSG
         STC   &@.3,0(,&@.4)       STORE IN THE MSG BLOCK
         MVI   1(&@.4),C'0'        SET CARRAGE CONTROL CHARACTER
         #PUT  (&@.4),SUBAD=&SUBAD ECHO THE PARM FIELD
         AR    &@.3,&@.4           GET THE SCAN BXLE LIMIT
         LA    &@.5,2(,&@.4)       GET THE SCAN BXLE INDEX
&N.LOOP  LR    &@.1,&@.5           SAVE KEYWORD STARTING ADDRESS
         LR    &@.0,&@.2           INIT APOS AND PAREN FLAG
&N.DELM  CLI   0(&@.5),C''''       APOSTROPHIE?
         BNE   &N.NAPO             NO, SKIP
         LCR   &@.0,&@.0           YES, FLIP-FLOP THE APOS FLAG
         B     &N.DEND             GO LOOP FOR NEXT CHARACTER
&N.NAPO  LTR   &@.0,&@.0           WITHIN APOSTROPHIES?
         BNP   &N.DEND             YES, SKIP THIS CHARACTER
         CLI   0(&@.5),C'('        NO, OPEN PAREN?
         BNE   &N.NOPN             NO, SKIP
         CR    &@.0,&@.2           YES, FIRST IN A NEST?
         BNE   &N.N1ST             NO, SKIP
         LR    &@.14,&@.5          YES, SAVE A POINTER TO IT ICO ERROR
&N.N1ST  AR    &@.0,&@.2           COUNT THE NEST LEVEL
         B     &N.DEND             LOOP FOR NEXT CHARACTER
&N.NOPN  CLI   0(&@.5),C')'        CLOSE PAREN?
         BNE   &N.NCPN             NO, SKIP
         SR    &@.0,&@.2           YES, DECR NEST LEVEL; IMBALANCE?
         BP    &N.DEND             NO, LOOP FOR NEXT CHARACTER
         B     &N.SNTX             YES, ERROR; GIVE UP ALTOGETHER
&N.NCPN  OI    0(&@.5),X'40'       UPCASE THE CHARACTER
         CR    &@.0,&@.2           WITHIN PARENS?
         BNE   &N.DEND             YES, SKIP THIS CHARACTER
         CLI   0(&@.5),C','        NO, DELIMITING COMMA?
         BE    &N.GOTC             YES, GOT AN ENTRY; GO PROCESS
&N.DEND  BXLE  &@.5,&@.2,&N.DELM   NO, LOOP FOR NEXT CHARACTER
         CR    &@.0,&@.2           END OF PARM; PARENS & APOS OK?
         BE    &N.GOTC             YES, GO PROCESS LAST ENTRY
         LR    &@.1,&@.14          NO, PNT TO START OF NEST
         BP    &N.ERRR             UNBALANCED PARENS; ISSUE ERROR MSG
&N.BAPO  BCTR  &@.5,0              UNMATCHED APOS; BACK-SCAN
         CLI   0(&@.5),C''''       IS THIS IT?
         BNE   &N.BAPO             NO, CONTINUE BACK-SCAN
&N.SNTX  LR    &@.1,&@.5           YES, POINT TO ERROR
         LR    &@.5,&@.3           PREVENT FURTHER PROCESSING
         B     &N.ERRR             GO FLAG THE ERROR
&N.GOTC  CR    &@.1,&@.5           NULL ENTRY?
         BE    &N.EEND             YES, SKIP
         LA    &@.15,&N.KEYS       NO, POINT TO KEYWORD LIST
         SR    &@.14,&@.14         CLEAR LENGTH REGISTER
&N.SRCH  CLI   0(&@.15),X'FF'      END OF KEYWORDS?
         BE    &N.ERRR             YES, ERROR
         IC    &@.14,3(,&@.15)     NO, GET THIS KEYWORD LENGTH - 1
         CLC   4(*-*,&@.15),0(&@.1) (EXECUTED)
&N.CLC   EX    &@.14,*-6           RIGHT KEYWORD?
         BE    &N.GOTK             YES, GO PASS TO HANDLER
         LA    &@.15,5(&@.14,&@.15) NO, ADVANCE TO NEXT
         B     &N.SRCH             LOOP TO TRY AGAIN
&N.GOTK  MVI   4(&@.15),X'FF'      PREVENT RE-USE OF THIS KEYWORD
         MVC   &WORKA+1(3),0(&@.15) ALIGN THE HANDLER ADDRESS
         L     &@.15,&WORKA        LOAD IT
         LA    &@.1,1(&@.14,&@.1)  POINT TO KEYWORD'S VALUE
         LR    &@.0,&@.5           POINT PAST THE ENTRY
         SR    &@.0,&@.1           GET THE ENTRY'S VALUE'S LENGTH
         BALR  &@.14,&@.15         LINK TO THE HANDLER
         B     &N.EEND             NORMAL RETURN; LOOP FOR NEXT ENTRY
&N.ERRR  MVI   0(&@.1),X'FF'       ERROR RETURN; FLAG THE ERROR
         MVI   1(&@.4),X'00'       SIGNEL THAT AN ERROR HAS OCCURED
&N.EEND  BXLE  &@.5,&@.2,&N.LOOP   LOOP TO PROCESS NEXT ENTRY
         CLI   1(&@.4),C'0'        ANY ERRORS OCCURE?
         BE    &N.RET              NO, GO RETURN TO CALLER
         LA    &@.1,1(,&@.4)       YES, PNT TO PARM CCC.
&N.FLAG  CLI   0(&@.1),X'FF'       ERROR HERE?
         MVI   0(&@.1),C' '        (ASSUME NO)
         BNE   &N.STAR             NO, SKIP TO NEXT CHARACTER
         MVI   0(&@.1),C'*'        YES, SET A VISUAL SIGNEL
&N.STAR  BXLE  &@.1,&@.2,&N.FLAG   LOOP FOR NEXT CHARACTER
         #PUT  (R4),SUBAD=&SUBAD   PRINT THE ERROR FLAG(S)
         #PUT  &N.ERRM,SUBAD=&SUBAD PRINT ERROR INFO
         LTR   &@.14,&@.14         SIGNEL ERROR RETURN
&N.RET   LM    &@.14,&@.5,&SAVEA   (RESTORE REGISTERS)
         BC    7,4(,&@.14)         ERROR RETURN
         BR    &@.14               NORMAL RETURN
         SPACE 3
         AIF   (K'&SVID NE 0).NOSAVE
&SAVEA   DS    8A                  REGISTER SAVE AREA
.NOSAVE  AIF   (K'&WKID NE 0).NOWORK
&WORKA   DS    A                   WORK AREA
.NOWORK  AIF   (K'&SVID NE 0 AND K'&WKID NE 0).NOSPACE
         SPACE 1
.NOSPACE ANOP
&N.KEYS  DS    0H                  KEYWORD LIST
&A1      SETA  0
.LP1     AIF   (&A1 EQ N'&SYSLIST).END1
&A1      SETA  &A1+1
&A2      SETA  K'&SYSLIST(&A1,1)-1
         DC    AL3(&SYSLIST(&A1,2)),AL1(&A2),C'&SYSLIST(&A1,1)'
         AGO   .LP1
.END1    DC    X'FF'               KEYWORD LIST TERMINATOR
         SPACE 1
&C1      SETC  'L'''
         DC    AL1(&C1&N.TITL)
&N.TITL  DC    C'-THE FOLLOWING PARM FIELD HAS BEEN GIVEN.'
         SPACE 1
         DC    AL1(&C1&N.ERRM)
&N.ERRM  DC    C'0ERRORS HAVE BEEN DETECTED AT THE ABOVE ASTERISKS.'
         MEND
./ ADD NAME=#PLIENV
         MACRO
&N       #PLIENV &ISASIZE=,&DSASIZE=,&RECOVER=LOCAL,&BASES=1,          *
               &ESDTYPE=CSECT,&PFIX=
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* THIS MACRO WAS WRITTEN - APRIL 21, 1981
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.* THIS MACRO INTERFACES TO THE 'PLIENV' PROGRAM (WRITTEN IN PLI) TO
.* CREATE A PLI ENVIRONMENT IN WHICH ASSEMBLER LANGUAGE CODE CAN RUN.
.* THE FLOW OF CONTROL IS DESCRIBED IN THE COMMENTARY GIVEN BELOW.
.*
.* THIS MACRO ACCEPTS THE FOLLOWING OPERANDS.
.*
.* THE NAME FIELD.
.*       THIS IS OPTIONAL. IT ASSIGNS A NAME TO THE FIRST SUITABLE
.*       GENERATED INSTRUCTION. NOTE, THE DEFAULT FIRST INSTRUCTION IS
.*       A 'CSECT' CARD.
.*
.* ISASIZE=
.*       THIS IS OPTIONAL. IT SPECIFIES THE DESIRED SIZE OF PLI'S
.*       "INITIAL STORAGE AREA". ITS SYNTAX IS IDENTICAL TO PLI'S
.*       ISASIZE EXECUTION TIME OPTION AS DESCRIBED IN THE PLI
.*       PROGRAMMER'S GUIDE. IF THIS OPERAND IS OMITTED, THEN THE LOCAL
.*       INSTALLATION ESTABLISHED DEFAULT ISASIZE IS OBTAINED. NOTE, A
.*       "REGISTER FORM" OF THIS OPERAND IS NOT SUPPORTED.
.*
.* DSASIZE=
.*       THIS IS OPTIONAL. IT SPECIFIES THE DESIRED DATA STORAGE AREA
.*       SIZE TO BE MADE AVAILABLE TO THE ASSEMBLER CODE FOLLOWING THIS
.*       MACRO. IF OMITTED, THEN THE MINIMUM VALUE (120 BYTES) IS
.*       OBTAINED. DSASIZE MUST SPECIFY A NON-RELOCATABLE EXPRESSION.
.*       THE SPECIFIED VALUE MUST BE EQUAL TO OR GREATER THAN 120 BYTES
.*       SINCE THE FIRST 120 BYTES ARE REQUIRED BY THE PLI SUBROUTINE
.*       LINKAGE CONVENTIONS. A "REGISTER FORM" OF THIS OPERAND IS NOT
.*       SUPPORTED.
.*
.* RECOVER={LOCAL×PLI}
.*       THIS CONTROLS WHETHER OR NOT PLI ESTABLISHES A RECOVERY
.*       ENVIRONMENT (I.E., ISSUES 'ESTAE' AND 'SPIE' MACROS).
.*
.*       RECOVER=LOCAL (THE DEFAULT)
.*       PLI WILL NOT ISSUE 'ESTAE' AND 'SPIE' MACROS. THE ASSEMBLER
.*       CODE FOLLOWING THIS MACRO EXPANSION MUST PROVIDE FOR ITS OWN
.*       RECOVERY.
.*
.*       RECOVER=PLI
.*       PLI WILL ISSUE 'ESTAE' AND 'SPIE' MACROS. ALL ABENDS MUST BE
.*       HANDLER VIA PLI "ON UNITS".
.*
.* BASES=
.*       THIS DEFINES THE REGISTERS THAT ARE TO BE USED AS PROGRAM
.*       BASES FOR THE CODE FOLLOWING THIS MACRO EXPANSION. THIS
.*       OPERAND MAY SPECIFY EITHER A SINGLE SELF-DEFINING TERM OR IT
.*       MAY GIVE A PARENTHESIZED LIST OF ONE OR MORE REGISTER NAMES.
.*
.*       BASES=<A NUMBER>   (THE DEFAULT IS "BASES=1")
.*       THIS DEFINES THE NUMBER OF BASES REGISTERS THAT ARE TO BE
.*       LOADED AND DECLARED BY THE MACRO. REGISTERS ARE SELECTED IN
.*       DESCENDING REGISTER ORDER STARTING WITH REGISTER 11. "NUMBER"
.*       MAY RANGE IN VALUE FROM 0 TO 11.
.*
.*       BASES=(<A LIST OF REGISTER NAMES>)
.*       THIS DEFINES SPECIFIC BASE REGISTERS THAT ARE TO BE LOADED AND
.*       DECLARED BY THE MACRO. ANY REGISTERS BETWEEN R1 AND R11 MAY
.*       BE CHOSEN.
.*
.* ESDTYPE={CSECT×START×ENTRY×DS×NONE×<NULL>}
.*
.*       ESDTYPE=CSECT  (THE DEFAULT)
.*       THIS CAUSES THE MACRO TO GENERATE A 'CSECT' CARD AS THE FIRST
.*       CARD OF THE EXPANSION.
.*
.*       ESDTYPE=START
.*       THIS CAUSES THE MACRO TO GENERATE A 'START' CARD AS THE FIRST
.*       CARD OF THE EXPANSION.
.*
.*       ESDTYPE=ENTRY
.*       THIS CAUSES THE MACRO TO GENERATE A 'ENTRY' CARD NAMING THE
.*       FIRST EXECUTABLE INSTRUCTION IN THE EXPANSION AS AN ENTRY
.*       POINT.
.*
.*       ESDTYPE=DS
.*       THIS CAUSES THE MACRO TO GENERATE A 'DS 0H' CARD AS THE FIRST
.*       CARD OF THE EXPANSION.
.*
.*       ESDTYPE=NONE  OR  ESDTYPE=
.*       THIS SUPPRESSES THIS FUNCTION.
.*
.* PFIX=
.*       THIS CONTROLS THE SET OF REGISTER NAMES TO BE USED BY THE
.*       MACRO EXPANSION. EXAMPLE: "PFIX=GPR" CAUSES THE MACRO TO USE
.*       "GPR1" TO REFER TO REGISTER 1. THE DEFAULT IS TO USE THE
.*       REGISTER NAME PREFIX DEFINED BY THE FIRST PRECEDING '#REGS'
.*       MACRO, IF ANY.
.*
.* INNER MACROS USED - #REGS #TEST #ENTER GETMAIN FREEMAIN LOAD DELETE
.*
         GBLC  &#TESRET(20)
         LCLC  &#,&@,&PARM,&I,&N2,&W1,&W2
&#       SETC  '&SYSNDX'
&@       SETC  '&PFIX'
         AIF   (K'&PFIX NE 0).GOTPFIX
         #TEST PFIX=
&@       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.GOTPFIX #REGS &PFIX,GEN=NO
.DONPFIX ANOP
&PARM    SETC  'NOSTAE,NOSPIE'
         AIF   ('&RECOVER' EQ '' OR '&RECOVER' EQ 'LOCAL').GOTRCVR
&PARM    SETC  ''
         AIF   ('&RECOVER'(1,1) EQ 'P').GOTRCVR
         MNOTE 4,'"&&RECOVER=&RECOVER" NOT RECOGNIZED.'
         MNOTE 4,'"&&RECOVER=PLI" ASSUMED.'
.GOTRCVR ANOP
         AIF   (K'&ISASIZE EQ 0).GOTISA
         AIF   ('&ISASIZE' NE '&ISASIZE(1)').GETPISA
&PARM    SETC  '&PARM.,ISASIZE(&ISASIZE.)'
         AGO   .GOTISA0
.GETPISA ANOP
&PARM    SETC  '&PARM.,ISASIZE&ISASIZE'
.GOTISA0 AIF   ('&PARM'(1,1) NE ',').GOTISA
&PARM    SETC  '&PARM'(2,K'&PARM-1)
.GOTISA  ANOP
&PARM    SETC  '&PARM./'
.*
         SPACE 1
*************************************************************
*        ENTRY CODE -- EXECUTED FIRST.                      *
*        - RECEIVES CONTROL FROM A CALLER.                  *
*        - SAVES REGISTERS.                                 *
*        - CONSTRUCTS A SPECIAL PARM FIELD CONTAINING:      *
*          - PLI EXECUTION TIME OPTIONS;                    *
*          - INFORMATION SPECIFIC TO THE CONTROL OF THIS    *
*            INTERFACE.                                     *
*        - LOCATES THE 'PLIENV' LOAD MODULE.                *
*        - CALLS 'PLIENV' VIA THE 'PLISTART' ENTRY POINT.   *
*        - THIS CAUSES THE CREATION OF A PLI ENVIRONMENT.   *
*        - CONTROL NEXT PASSES TO THE "INTERFACE CODE"      *
*          GENERATED BELOW.                                 *
*************************************************************
         SPACE 1
.*
&N2      SETC  '&N'
&W1      SETC  '&ESDTYPE(1)'
         AIF   ('&W1' EQ 'NONE' OR '&W1' EQ '').ESDDONE
         AIF   ('&W1' EQ 'ENTRY').ESDENT
&W2      SETC  ','
         AIF   ('&W1' EQ 'CSECT' OR '&W1' EQ 'START').ESDNCSC
&W2      SETC  '0H''0'''
.ESDNCSC ANOP
&N       &W1   &W2                 START
&N2      SETC  ''
         AGO   .ESDDONE
.ESDENT  ANOP
         ENTRY &N                  DECLARE ENTRY NAME
.ESDDONE ANOP
.*
&N2      STM   &@.14,&@.12,12(&@.13) SAVE CALLER'S REGISTERS
         BALR  &@.14,0             LOAD LOCAL BASE
         USING *,&@.14             DECLARE IT
         LA    &@.0,L'PNV&#.P+34   GET L'LCL SAVE AREA + L'PARM FIELD
         MNOTE '         GETMAIN R,LV=(0)'
         GETMAIN R,LV=(0)
         LR    &@.2,&@.1           COPY LOCAL AREA POINTER
         MVC   0(20,&@.2),12(&@.13) SAVE CALLER'S R14 THRU R2
         LA    &@.1,24(,&@.2)      --> PARM FIELD
         ST    &@.1,20(,&@.2)      BUILD 1-ENTRY PLIST
         MVI   20(&@.2),B'10000000' FLAG THE LAST ENTRY
         MVC   0(L'PNV&#.P+6,&@.1),PNV&#.P-2 COPY PARM FIELD
         MVC   L'PNV&#.P+6(4,&@.1),12(&@.2) COPY CALLER'S R1 TO PARM
         MNOTE '         LOAD  EPLOC=PNV&#.N'
         LOAD  EPLOC=PNV&#.N
         LR    &@.15,&@.0          GET PLIENV'S ENTRY ADDRESS
         LA    &@.1,20(,&@.2)      --> PARM FIELD
         BALR  &@.14,&@.15         GO ESTABLISH PLI ENVIRONMENT AND
*                                  EXECUTED THE PROGRAM STARTING AT
*                                  PNV####R.
         EJECT ,
*************************************************************
*        EXIT CODE -- EXECUTED LAST.                        *
*        - RECEIVES CONTROL BACK VIA 'PLIENV' UPON          *
*          COMPLETION OF THE ASSEMBLER CODE BELOW.          *
*        - NOTE, AT THIS POINT THE PLI ENVIRONMENT NO       *
*          LONGER EXISTS.                                   *
*        - CLEANS UP AND RESTORES REGISTERS (PRESERVING THE *
*          R15 RECEIVED FROM 'PLIENV').                     *
*        - RETURNS TO "ENTRY CODE'S" CALLER.                *
*************************************************************
         SPACE 1
         USING *,&@.14             THE PROGRAM HAS COMPLETED. DECLARE A
*                                  LOCAL BASE.
         ST    &@.15,4(,&@.2)      STORE CALLED PROGRAM'S RC
         MNOTE '         DELETE EPLOC=PNV&#.N'
         DELETE EPLOC=PNV&#.N
         MVC   12(20,&@.13),0(&@.2) RESTORE CALLER'S REGS TO SAVE AREA
         LA    &@.0,L'PNV&#.P+34   GET L'LOCAL WORK AREA
         MNOTE '         FREEMAIN R,LV=(0),A=(&@.2)'
         FREEMAIN R,LV=(0),A=(&@.2)
         LM    &@.14,&@.2,12(&@.13) RESTORE CALLER'S REGISTERS
         DROP  &@.14               RELEASE LOCAL BASE
         BR    &@.14               RETURN TO CALLER
         SPACE 3
PNV&#.N  DC    CL8'PLIENV'         ROUTINE THAT SETS UP PLI ENVIRONMENT
         DC    Y(L'PNV&#.P+8)      L'PARM FIELD
PNV&#.P  DC    C'&PARM',AL4(PNV&#.R) MOST OF PARM FIELD
         EJECT ,
*************************************************************
*        INTERFACE CODE -- EXECUTED SECOND.                 *
*        - RECEIVES CONTROL FROM 'PLIENV'.                  *
*        - NOTE, AT THIS POINT A PLI ENVIRONMENT EXISTS.    *
*        - ESTABLISHES A LOCAL DSA OF THE DESIRED SIZE.     *
*        - ESTABLISHES THE DESIRED LOCAL PROGRAM BASE       *
*          REGISTERS.                                       *
*        - RESTORES THE R1 VALUE ORIGINALLY RECEIVED BY     *
*          "ENTRY CODE" ABOVE.                              *
*        - FALLS THROUGH TO USER WRITTEN ASSEMBLER CODE.    *
*************************************************************
         SPACE 1
         AIF   ('&DSASIZE' EQ '&DSASIZE(1)').DSASOK
         MNOTE 4,'"DSASIZE=&DSASIZE" IS INVALID.'
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*
               '
.DSASOK  MNOTE 'PNV&#.R #ENTER &N,SAVTYPE=(PLI,&DSASIZE),BASES=&BASES,'
         MNOTE '               ESDTYPE=NONE,PFIX=&PFIX'
PNV&#.R  #ENTER &N,SAVTYPE=(PLI,&DSASIZE),BASES=&BASES,                *
               ESDTYPE=NONE,PFIX=&PFIX
         L     &@.1,88(,&@.13)     RESTORE INITIAL PARAMETER REGISTER
         EJECT ,
*************************************************************
*        USER CODE.                                         *
*        - IF THIS CODE WISHES TO CALL SUBROUTINES USING    *
*          PLI CONVENTIONS, THEN THE CALLED SUBROUTINES     *
*          SHOULD START WITH A "#ENTER SAVTYPE=PLI" MACRO   *
*          AND END WITH A "#EXIT ((R14,R12)),RC=..." MACRO. *
*        - WHEN THIS CODE WISHES TO RETURN TO ITS CALLER,   *
*          IT SHOULD DO SO VIA A "#EXIT ((R14,R12)),RC=..." *
*          MACRO. THIS WILL CAUSE CONTROL TO RETURN TO      *
*          'PLIENV' WHICH WILL THEN CLEAN UP THE PLI        *
*          ENVIRONMENT AND THEN RETURN CONTROL TO "EXIT     *
*          CODE" ABOVE.                                     *
*************************************************************
         SPACE 1
         MEND
./ ADD NAME=#PUT
         MACRO
&NME     #PUT  &MSG,&PFIX=,&SUBAD=,&MF=
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $PUT TO #PUT.
.*
.* LAST CHANGE DATE - APRIL 20, 1981
.*                  - "#REGS GEN=NO" SUPPORT ADDED.
.*
.* LAST CHANGE DATE - JULY 18, 1980
.*                  - INDIRECT ADDRESSING IS NOW INDICATED BY A
.*                    TRAILING PERCENT SIGN (%) RATHER THAN A LEADING
.*                    ONE.
.*
.* LAST CHANGE DATE - JANUARY 12, 1977
.*                  - HANDLING OF THE SUBAD= OPERAND IS REWRITTEN.
.*                  - MAILING ADDRESS CHANGE.
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
         GBLA  &#TESERR
         GBLC  &#PUTSUB
         GBLC  &#TESRET(20)
         LCLA  &A1
         LCLC  &@,&C1,&N
         AIF   ('&SUBAD' EQ '').SUBOK
         AIF   ('&SUBAD(1)' NE '&SUBAD').SUBOK
&#PUTSUB SETC  '&SUBAD'
.SUBOK   AIF   ('&MF(1)' EQ 'INIT').MEND
&N       SETC  '&NME'
.*
&@       SETC  '&PFIX'
         AIF   ('&PFIX' NE '').PFIXOK
         #TEST PFIX=
&@       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.PFIXOK  #REGS &PFIX,GEN=NO
.DONPFIX ANOP
.*
         AIF   ('&MSG(1)' EQ '&MSG').NOTREG
         #TEST REGS=&MSG(1)
         AIF   (&#TESERR NE 0).LR
         AIF   (&#TESRET(1) EQ 1).BAL
.LR      ANOP
&N       LR    &@.1,&MSG(1)        --> MESSAGE LENGTH FIELD
&N       SETC  ''
         AGO   .BAL
.NOTREG  AIF   ('&MSG' EQ '').BAL
         AIF   ('&MSG'(K'&MSG,1) EQ '%').MSGINDR
&N       LA    &@.1,&MSG-1         --> MESSAGE LENGTH FIELD
         AGO   .MSGOK
.MSGINDR ANOP
&C1      SETC  '&MSG'(1,K'&MSG-1)
&N       L     &@.1,=A(&C1-1)      --> MESSAGE LENGTH FIELD
.MSGOK   ANOP
&N       SETC  ''
.BAL     AIF   ('&SUBAD(1)' NE '&SUBAD').BALR
         AIF   ('&#PUTSUB'(1,1) EQ '%').BALINDR
&N       BAL   &@.14,&#PUTSUB      GO DISPLAY THE MESSAGE
         AGO   .MEND
.BALR    ANOP
&N       BALR  &@.14,&SUBAD(1)     GO DISPLAY THE MESSAGE
         AGO   .MEND
.BALINDR ANOP
&A1      SETA  0
.LP1     ANOP
&A1      SETA  &A1+1
         AIF   ('&#PUTSUB'(1,&A1) NE '&#PUTSUB').LP1
&C1      SETC  '&#PUTSUB'(2,&A1-1)
&N       L     &@.15,=A(&C1)       --> MESSAGE PRINTING ROUTINE
         BALR  &@.14,&@.15         GO DISPLAY THE MESSAGE
.MEND    MEND
./ ADD NAME=RCPBFRGS
         MACRO
         RCPBFRGS &BUFPTR,&WKREGS
         GBLC  &RCPBFRP,&RCPBFR1,&RCPBFR2
         AIF   ('&BUFPTR' EQ '').TGP
&RCPBFRP SETC  '&BUFPTR'
         AGO   .TWK1
.TGP     AIF   ('&RCPBFRP' NE '').TWK1
&RCPBFRP SETC  'R1'
.TWK1    AIF   ('&WKREGS(1)' EQ '').TG1
&RCPBFR1 SETC  '&WKREGS(1)'
         AGO   .TWK2
.TG1     AIF   ('&RCPBFR1' NE '').TWK2
&RCPBFR1 SETC  'R14'
.TWK2    AIF   ('&WKREGS(2)' EQ '').TG2
&RCPBFR2 SETC  '&WKREGS(2)'
         MEXIT
.TG2     AIF   ('&RCPBFR2' NE '').EXIT
&RCPBFR2 SETC  'R15'
.EXIT    MEND
./ ADD NAME=RCPBTU2
         MACRO
         RCPBTU &KEY,&NUM,&PAR
         GBLA  &DTUPO
         GBLC  &DYNP
         LCLA  &L
.*
.*  INNER MACRO FOR ALLOC, TO BRANCH AROUND TEXT UNIT AND
.*  CREATE TEXT UNIT
.*
&L       SETA  K'&PAR+8                GET LENGTH TO BRANCH AROUND
         AIF   (&L/2 EQ (&L+1)/2).LOK  MAKE SURE LENGTH IS EVEN
&L       SETA  &L+1
.LOK     BALS  R14,*+&L                BRANCH AROUND TEXT UNIT
&L       SETA  K'&PAR-2
         DC    Y(&KEY,&NUM,&L),C&PAR   TEXT UNIT
         LA    R14,0(,R14)             CLEAR HIGH ORDER BYTE
         ST    R14,&DYNP.TUP+&DTUPO    STORE TEXT UNIT ADDRESS
&DTUPO   SETA  &DTUPO+4
         MEND ,
./ ADD NAME=RCPBTU
         MACRO
         RCPBTU &KEY,&NUM,&PAR
         LCLA  &L
.*
.*  INNER MACRO FOR ALLOC, TO GENERATE TEXT UNITS ENTERED
.*  IN QUOTES
.*
&L       SETA  K'&PAR-2                GET LENGTH OF TEXT UNIT
         MVI   S99TUKEY+1,&KEY         SET TEXT UNIT KEY
         MVI   S99TUNUM+1,&NUM         SET NUMBER FIELD
         MVI   S99TULNG+1,&L           MOVE IN LENGTH
         MVC   S99TUPAR(&L.),=C&PAR    MOVE IN TEXT UNIT
&L       SETA  &L+6
         AIF   (&L/2 EQ (&L+1)/2).LOK
&L       SETA  &L+1
.LOK     RCPDINC &L
         MEND
./ ADD NAME=RCPCKID
         MACRO
&NAME    RCPCKID              &CHECKID
         GBLB  &RCPECT(2),&RCPPSCB(2)
         GBLC  &RCPPRE
         LCLC  &CHARVAR,&P
         LCLA  &COUNTR,&L
&P       SETC  '&RCPPRE'
&RCPPSCB(1) SETB  1
&RCPECT(1)  SETB  1
         EJECT
         SPACE 4
***********************************************************************
***  THE USERID OF THE USER IS CHECKED. IF IT IS NOT VALID, THE    ****
***   COMMAND PRETENDS IT DOES NOT EXIST BY LINKING TO EXEC IN     ****
***   THE SAME WAY THE TMP DOES IF IT CANNOT FIND THE COMMAND.     ****
***********************************************************************
         SPACE 3
         L     R1,CPPLPSCB             LOAD ADDRESS OF PSCB
         USING PSCB,R1                 PSCB ADDRESSABILITY
.NID     ANOP
&COUNTR  SETA  &COUNTR+1
         AIF   ('&CHECKID(&COUNTR)' EQ '').ENDID
&CHARVAR SETC  '&CHECKID(&COUNTR)'
&L       SETA  K'&CHARVAR
         AIF   ('&CHARVAR'(1,1) EQ '''').QCID
         CLC   PSCBUSER(&L),=C'&CHARVAR'  IS THE USERID VALID?
         BE    &P.IDOK                     YES, BRANCH OUT
         AGO   .NID
.QCID    ANOP
&L       SETA  &L-2
         CLC   PSCBUSER(&L),=C&CHARVAR    IS THE USERID VALID?
         BE    &P.IDOK                     YES, BRANCH OUT
         AGO   .NID
.ENDID   L     R1,CPPLECT              LOAD ECT ADDRESS
         SPACE 2
         USING ECT,R1
         MVC   ECTPCMD,&P.EXECN        MOVE IN COMMAND NAME
         DROP  R1                      KILL ECT ADDRESSABILITY
         L     R1,CPPLCBUF             LOAD CBUF ADDRESS
         XC    2(2,R1),2(R1)           ZERO OFFSET FIELD
         L     R1,&P.CPPL              RELOAD CPPL ADDRESS
         XCTL  EPLOC=&P.EXECN
&P.EXECN DC    CL8'EXEC'               NAME OF EXEC PROCESSOR
&P.IDOK  DS    0H
         MEND
./ ADD NAME=RCPDDCB
         MACRO
         RCPDDCB &LRECL=,&BLKSIZE=,&RECFM=,&DEN=,&DSORG=,&KEYLEN=
         GBLC  &DYNP
         AIF   ('&DEN' EQ '').DSORG
         SPACE
***********************************************************************
**   DCB DENSITY TEXT UNIT                                           **
***********************************************************************
         AIF   ('&DEN' EQ '0').DEN0
         AIF   ('&DEN' EQ '1').DEN1
         AIF   ('&DEN' EQ '2').DEN2
         AIF   ('&DEN' EQ '3').DEN3
         AIF   ('&DEN' EQ '4').DEN4
         MNOTE 8,'DENSITY INVALID'
         AGO   .DSORG
.DEN0    ANOP
         MVI   S99TUPAR,X'03'          SET PARAMETER
         AGO   .DODEN
.DEN1    ANOP
         MVI   S99TUPAR,X'43'          SET PARAMETER
         AGO   .DODEN
.DEN2    ANOP
         MVI   S99TUPAR,X'83'          SET PARAMETER
         AGO   .DODEN
.DEN3    ANOP
         MVI   S99TUPAR,X'C3'          SET PARAMETER
         AGO   .DODEN
.DEN4    ANOP
         MVI   S99TUPAR,X'D3'          SET PARAMETER
         AGO   .DODEN
.DODEN   ANOP
         MVI   S99TUKEY+1,DALDEN       SET TEXT UNIT KEY
         MVI   S99TUNUM+1,1            SET NUMBER
         MVI   S99TULNG+1,1            SET LENGTH
         RCPDINC 8
.DSORG   AIF   ('&DSORG' EQ '').BLKSIZE
         SPACE
***********************************************************************
**    DCB DSORG TEXT UNIT                                            **
***********************************************************************
         AIF   ('&DSORG' EQ 'PO').PO
         AIF   ('&DSORG' EQ 'PS').PS
         AIF   ('&DSORG' EQ 'PO').DA
         MNOTE 8,'DSORG INVALID'
         AGO   .BLKSIZE
.PO      ANOP
         MVC   S99TUPAR(2),=X'0200'    SET PARAMETER
         AGO   .DODSORG
.PS      ANOP
         MVC   S99TUPAR(2),=X'4000'    SET PARAMETER
         AGO   .DODSORG
.DA      ANOP
         MVC   S99TUPAR(2),=X'2000'    SET PARAMETER
         AGO   .DODSORG
.DODSORG ANOP
         MVI   S99TUKEY+1,DALDSORG     SET TEXT UNIT KEY
         MVI   S99TUNUM+1,1            SET NUMBER
         MVI   S99TULNG+1,2            SET LENGTH
         RCPDINC 8
.BLKSIZE ANOP
         AIF   ('&BLKSIZE' EQ '').LRECL
         SPACE
***********************************************************************
**    DCB BLKSIZE TEXT UNIT                                          **
***********************************************************************
         MVI   S99TUKEY+1,DALBLKSZ     SET TEXT UNIT KEY
         MVI   S99TUNUM+1,1            SET NUMBER
         MVI   S99TULNG+1,2            SET LENGTH
         MVC   S99TUPAR(2),=AL2(&BLKSIZE) SET PARAMETER
         RCPDINC 10
.LRECL   ANOP
         AIF   ('&LRECL' EQ '').RECFM
         SPACE
***********************************************************************
**    DCB LRECL TEXT UNIT                                            **
***********************************************************************
         MVI   S99TUKEY+1,DALLRECL     SET TEXT UNIT KEY
         MVI   S99TUNUM+1,1            SET NUMBER
         MVI   S99TULNG+1,2            SET LENGTH
         MVC   S99TUPAR(2),=AL2(&LRECL) SET PARAMETER
         RCPDINC 10
.RECFM   ANOP
         AIF   ('&RECFM' EQ '').KEYLEN
         SPACE
***********************************************************************
**    DCB RECFM TEXT UNIT                                            **
***********************************************************************
         AIF   ('&RECFM' EQ 'F').RF
         AIF   ('&RECFM' EQ 'FA').RFA
         AIF   ('&RECFM' EQ 'FM').RFM
         AIF   ('&RECFM' EQ 'FB').RFB
         AIF   ('&RECFM' EQ 'FBM').RFBM
         AIF   ('&RECFM' EQ 'FBA').RFBA
         AIF   ('&RECFM' EQ 'V').RV
         AIF   ('&RECFM' EQ 'VA').RVA
         AIF   ('&RECFM' EQ 'VM').RVM
         AIF   ('&RECFM' EQ 'VB').RVB
         AIF   ('&RECFM' EQ 'VBM').RVBM
         AIF   ('&RECFM' EQ 'VBA').RVBA
         AIF   ('&RECFM' EQ 'U').RU
         MNOTE 8,'INVALID RECFM'
         AGO   .KEYLEN
.RF      ANOP
         MVI   S99TUPAR,X'80'
         AGO   .DORECFM
.RFA     ANOP
         MVI   S99TUPAR,X'84'
         AGO   .DORECFM
.RFM     ANOP
         MVI   S99TUPAR,X'82'
         AGO   .DORECFM
.RFB     ANOP
         MVI   S99TUPAR,X'90'
         AGO   .DORECFM
.RFBA    ANOP
         MVI   S99TUPAR,X'94'
         AGO   .DORECFM
.RFBM    ANOP
         MVI   S99TUPAR,X'92'
         AGO   .DORECFM
.RFBS    ANOP
         MVI   S99TUPAR,X'9A'
         AGO   .DORECFM
.RV      ANOP
         MVI   S99TUPAR,X'40'
         AGO   .DORECFM
.RVA     ANOP
         MVI   S99TUPAR,X'44'
         AGO   .DORECFM
.RVM     ANOP
         MVI   S99TUPAR,X'42'
         AGO   .DORECFM
.RVB     ANOP
         MVI   S99TUPAR,X'50'
         AGO   .DORECFM
.RVBA    ANOP
         MVI   S99TUPAR,X'54'
         AGO   .DORECFM
.RVBM    ANOP
         MVI   S99TUPAR,X'52'
         AGO   .DORECFM
.RVBS    ANOP
         MVI   S99TUPAR,X'5A'
         AGO   .DORECFM
.RU      ANOP
         MVI   S99TUPAR,X'C0'
         AGO   .DORECFM
.DORECFM ANOP
         MVI   S99TUKEY+1,DALRECFM     SET TEXT UNIT KEY
         MVI   S99TUNUM+1,1            SET NUMBER
         MVI   S99TULNG+1,1            SET LENGTH
         RCPDINC 8
.KEYLEN  ANOP
         AIF   ('&KEYLEN' EQ '').EXIT
         SPACE
***********************************************************************
**    DCB KEYLEN TEXT UNIT                                           **
***********************************************************************
         MVI   S99TUKEY+1,DALKYLEN     SET TEXT UNIT KEY
         MVI   S99TUNUM+1,1            SET NUMBER
         MVI   S99TULNG+1,1            SET LENGTH
         MVI   S99TUPAR,AL1(&KEYLEN)   SET PARAMETER
         RCPDINC 8
.EXIT    MEND
./ ADD NAME=RCPDDN
         MACRO
         RCPDDN &DDN
         GBLC  &DYNP
         SPACE 1
***********************************************************************
**   BUILD THE DDNAME TEXT UNIT                                      **
***********************************************************************
         AIF   ('&DDN'(K'&DDN,1) EQ '/').BTU
         AIF   ('&DDN'(1,1) EQ '''').Q
         RCPSR2
         AIF   ('&DDN'(1,1) EQ '(').R
         L     R14,&DDN                LOAD ADDRESS OF DDNAME
         LH    R2,&DDN+4               LOAD LENGTH OF DDNAME
         AGO   .STH
.R       L     R14,0&DDN               LOAD ADDRESS OF DDNAME
         LH    R2,4&DDN                LOAD LENGTH OF DDNAME
.STH     STH   R2,S99TULNG             STORE DDNAME LENGTH
         BCTR  R2,0                    DECREMENT FOR EXECUTE
         EX    R2,&DYNP.MVC            MOVE DDNAME
         MVI   S99TUKEY+1,DALDDNAM     MOVE IN DDNAME KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         RCPDINC 14
         MEXIT
.Q       RCPBTU DALDDNAM,1,&DDN
         MEXIT
.BTU     RCPTUBFR DALDDNAM,14,&DDN
         MEND
./ ADD NAME=RCPDDNRT
         MACRO
         RCPDDNRT
         SPACE 1
***********************************************************************
**    DDNAME RETURN TEXT UNIT                                        **
***********************************************************************
         MVI   S99TUKEY+1,DALRTDDN     SET RETURN DDNAME KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         MVI   S99TULNG+1,8            SET LENGTH FIELD
         MVC   S99TUPAR(8),=CL8' '     INITIALIZE FIELD TO BLANKS
         RCPDINC 14
         MEND
./ ADD NAME=RCPDEBUG
         MACRO
         RCPDEBUG &ON
         GBLA  &RCPBGN#,&RCPSWS(10)
         GBLB  &RCPDBUG
         GBLC  &RCPPRE,&RCPWKDS,&RCPWKCS
         AIF   ('&ON' EQ '').TSW
&RCPDBUG SETB 1
.TSW     AIF   (&RCPDBUG).DEBUG
         MEXIT
.DEBUG   MNOTE *,'RCPBGN# IS &RCPBGN#'
         MNOTE *,'RCPSWS(1) IS &RCPSWS(1)'
         MNOTE *,'RCPSWS(2) IS &RCPSWS(2)'
         MNOTE *,'RCPSWS(3) IS &RCPSWS(3)'
         MNOTE *,'RCPSWS(4) IS &RCPSWS(4)'
         MNOTE *,'RCPSWS(5) IS &RCPSWS(5)'
         MNOTE *,'RCPWKCS IS ''&RCPWKCS'''
         MNOTE *,'RCPWKDS IS ''&RCPWKDS'''
         MNOTE *,'RCPPRE IS ''&RCPPRE'''
         MEND
./ ADD NAME=RCPDEFER
         MACRO
         RCPDEFER
         SPACE
***********************************************************************
**     DEFER VOLUME MOUNT TEXT ATTRIBUTE UNIT                        **
***********************************************************************
         MVI   S99TUKEY+1,DALDEFER     SET TEXT UNIT KEY
         RCPDINC  4
         MEND
./ ADD NAME=RCPDFPL
         MACRO
         RCPDFPL
         GBLC  &RCPPRE
         GBLB  &RCPDFPL(2)
         GBLB  &RCPDFPB(2)
         LCLC  &P,&L,&L1
&P       SETC  '&RCPPRE'
         EJECT
         AIF   (&RCPDFPL(2)).BYPDFPL
&RCPDFPL(2) SETB 1
         IKJDFPL
L#DFPL   EQU   *-DFPL                  LENGTH OF DEFAULT PARAM LIST
         IKJDFPB
L#DFPB   EQU   *-DFPB                  LENGTH OF DEFAULT PARAM BLOCK
&SYSECT  CSECT                         RESUME PROGRAM CSECT
         SPACE 3
.BYPDFPL RCPDS
&P.DFPL  DS    CL(L#DFPL)              RESERVE SPACE FOR DFPL
&P.DFPB  DS    CL(L#DFPB)              RESERVE SPACE FOR DFPB
&P.DSNB  DS    CL48                    RESERVE SPACE FOR DSNAME BUFFER
         RCPDS
         EJECT
***********************************************************************
***   THIS CODE GENERATES AN DEFAULT SERVICE ROUTINE PARAMETER LIST ***
***       AND PARAMETER BLOCK                                       ***
***********************************************************************
         LA    R1,&P.DFPL              LOAD DFPL ADDRESS
         USING DFPL,R1                 DFPL ADDRESSABLE
         MVC   DFPLUPT,CPPLUPT         MOVE IN ADDRESS OF UPT
         MVC   DFPLECT,CPPLECT         MOVE IN ADDRESS OF ECT
         LA    R15,&P.ECB              LOAD ADDRESS OF ATTN ECB
         ST    R15,DFPLECB             AND STORE IN DFPL
         LA    R15,&P.DFPB             LOAD DFBP ADDRESS
         ST    R15,DFPLDFPB             AND STORE IT IN DFPB
         DROP  R1
         USING DFPB,R15                ADDRESS DFPB DSECT
         XC    DFPB(L#DFPB),DFPB       CLEAR DEFAULT PARAMETER BLOCK
         MVC   DFPBPSCB,CPPLPSCB       MOVE IN ADDRESS OF PSCB
         LA    R1,&P.DSNB              LOAD DSNAME BUFFER ADDRESS
         ST    R1,DFPBDSN               AND STORE IT INTO DFPB
         MVI   DFPBCODE,DFPB04          SET ENTRY CODE
         DROP  R15                     DFPB NO LONGER ADDRESSABLE
         EJECT
         MEND
./ ADD NAME=RCPDINC
         MACRO
         RCPDINC &L1
         GBLA  &DTUO,&DTUPO
         GBLC  &DYNP
         AIF   ('&L1' EQ '').T2
         ST    R15,&DYNP.TUP+&DTUPO    STORE TEXT UNIT ADDRESS
         LA    R15,&L1.(R15)           BUMP TEXT UNIT PTR TO NEXT SLOT
&DTUPO   SETA  &DTUPO+4
&DTUO    SETA  &DTUO+&L1
         MEXIT
.T2      ST    R14,&DYNP.TUP+&DTUPO    STORE TEXT UNIT ADDRESS
&DTUPO   SETA  &DTUPO+4
         MEND
./ ADD NAME=RCPDISP
         MACRO
         RCPDISP &DISP
         LCLA  &I
         LCLB  &B(4)
         AIF   ('&DISP(1)' EQ '').TD2
         SPACE
***********************************************************************
**     DATA SET INITIAL STATUS                                       **
***********************************************************************
&B(1)    SETB  ('&DISP(1)' EQ 'SHR')
&B(2)    SETB  ('&DISP(1)' EQ 'NEW')
&B(3)    SETB  ('&DISP(1)' EQ 'MOD')
&B(4)    SETB  ('&DISP(1)' EQ 'OLD')
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK1
         MNOTE 8,'&DISP(1) IS INVALID, DISP=SHR USED'
&B(1)    SETB  1
.OK1     ANOP
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)
         MVC   S99TUKEY(8),=Y(DALSTATS,1,1,X'0&I.00')
         RCPDINC 8
.TD2     AIF   ('&DISP(2)' EQ '').TD3
         SPACE
***********************************************************************
**    DATA SET NORMAL DISPOSITION                                    **
***********************************************************************
&B(1)    SETB  ('&DISP(2)' EQ 'KEEP')
&B(2)    SETB  ('&DISP(2)' EQ 'DELETE')
&B(3)    SETB  ('&DISP(2)' EQ 'CATLG')
&B(4)    SETB  ('&DISP(2)' EQ 'UNCATLG')
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK2
         MNOTE 8,'&DISP(2) IS INVALID, DISP=(,KEEP) USED'
&B(1)    SETB  1
.OK2     ANOP
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)
         MVC   S99TUKEY(8),=Y(DALNDISP,1,1,X'0&I.00')
         RCPDINC 8
.TD3     AIF   ('&DISP(3)' EQ '').EXIT
         SPACE
***********************************************************************
**   DATASET CONDITIONAL DISPOSITION                                 **
***********************************************************************
&B(1)    SETB  ('&DISP(3)' EQ 'KEEP')
&B(2)    SETB  ('&DISP(3)' EQ 'DELETE')
&B(3)    SETB  ('&DISP(3)' EQ 'CATLG')
&B(4)    SETB  ('&DISP(3)' EQ 'UNCATLG')
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK3
         MNOTE 8,'&DISP(3) IS INVALID, DISP=(,,KEEP) USED'
&B(1)    SETB  1
.OK3     ANOP
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)
         MVC   S99TUKEY(8),=Y(DALCDISP,1,1,X'0&I.00')
         RCPDINC 8
.EXIT    MEND
./ ADD NAME=RCPDSECT
         MACRO
&NAME    RCPDSECT &LTORG=YES
         AIF   ('&LTORG' NE 'YES').RCPDS
***********************************************************************
****                  LITERALS                                     ****
***********************************************************************
         SPACE 3
         LTORG
         EJECT
.RCPDS   RCPDS
         MEND
./ ADD NAME=RCPDS
         MACRO
         RCPDS
         GBLB  &RCPDSBR
         GBLC  &RCPWKDS,&RCPWKCS,&RCPDS
         AIF   ('&RCPDS' NE '').RESUME
&RCPDS   SETC  '&SYSECT'
         AIF   ('&RCPWKDS' EQ '').CSECT
&RCPWKDS DSECT                         ENTER WORKAREA DSECT
         MEXIT
.CSECT   AIF   ('&RCPWKCS' EQ '').BRANCH
&RCPWKCS CSECT                         ENTER WORKAREA CSECT
         MEXIT
.RESUME  AIF   (&RCPDSBR).BRTO
&RCPDS   CSECT                         RESUME PROGRAM CSECT
&RCPDS   SETC  ''
         MEXIT
.BRANCH  ANOP
&RCPDS   SETC  'RCP&SYSNDX'
&RCPDSBR SETB  1
         B     &RCPDS                  BRANCH AROUND CONSTANTS
         MEXIT
.BRTO    ANOP
&RCPDS   DS    0H
&RCPDSBR SETB  0
&RCPDS   SETC  ''
         MEND
./ ADD NAME=RCPDSN
         MACRO
         RCPDSN &DSN,&MEM
         LCLC  &MEMBER
         GBLC  &DYNP
         SPACE
***********************************************************************
**   BUILD THE DSNAME TEXT UNIT                                      **
***********************************************************************
         AIF   ('&DSN'(1,1) EQ '''').Q
         AIF   ('&DSN'(K'&DSN,1) EQ '/').BD
         AIF   ('&DSN'(1,1) EQ '(').REG
         AIF   ('&DSN'  EQ '*').TERM
         RCPSR2
         L     R14,&DSN                LOAD ADDRESS OF DSNAME
         LH    R2,&DSN+4               LOAD LENGTH OF DSNAME
.STH     STH   R2,S99TULNG             STORE DSNAME LENGTH
         BCTR  R2,0                    DECREMENT FOR EXECUTE
         EX    R2,&DYNP.MVC            MOVE DSNAME
         MVI   S99TUKEY+1,DALDSNAM     MOVE IN DSNAME KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         RCPDINC 50
         AGO   .TMEMBER
.REG     L     R14,0&DSN               LOAD ADDRESS OF DSNAME
         RCPSR2
         LH    R2,4&DSN                LOAD LENGTH OF DSNAME
         AGO   .STH
.TERM    MVI   S99TUKEY+1,DALTERM
         RCPDINC 4
         MEXIT
.BD      RCPTUBFR DALDSNAM,50,&DSN
         AGO   .TMEMBER
.Q       RCPBTU DALDSNAM,1,&DSN
.TMEMBER AIF   ('&MEM' EQ '').EXIT
         SPACE
***********************************************************************
**   BUILD THE MEMBER NAME TEXT UNIT                                 **
***********************************************************************
&MEMBER  SETC  '&MEM'
         AIF   ('&MEM' NE '*').MOK
         AIF   ('&DSN'(1,1) NE '''').MAST
         MNOTE 8,'MEMBER=* INVALID WITH QUOTED DSNAME'
         MEXIT
.MAST    ANOP
&MEMBER  SETC  '8+&DSN'
.MOK     ANOP
         AIF   ('&MEMBER'(K'&MEMBER,1) EQ '/').BM
         RCPSR2
         AIF   ('&MEMBER'(1,1) EQ '(').RM
         LH    R2,4+&MEMBER            LOAD LENGTH OF MEMBER NAME
         LTR   R2,R2                   TEST FOR ZERO
         BZ    *+30                    IF NO MEMBER, SKIP
         L     R14,&MEMBER             LOAD ADDRESS OF MEMBER
         AGO   .STHM
.RM      LH    R2,4&MEMBER             LOAD LENGTH OF MEMBER
         LTR   R2,R2                   AND TEST FOR ZERO
         BZ    *+30                    IF NO MEMBER, SKIP
         L     R14,0&MEMBER            LOAD ADDRESS OF MEMBER
.STHM    STH   R2,S99TULNG             STORE LENGTH OF MEMBER
         BCTR  R2,0                    DECREMENT FOR EXECUTE
         EX    R2,&DYNP.MVC            MOVE IN MEMBER NAME
         MVI   S99TUKEY+1,DALMEMBR     MOVE IN MEMBER KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         RCPDINC 14
         MEXIT
.BM      RCPTUBFR DALMEMBR,14,&MEMBER
         MEXIT
.QM      RCPBTU DALMEMBR,1,&MEMBER
.EXIT    MEND
./ ADD NAME=RCPDSNPD
         MACRO
         RCPDSNPD &PDE
         AIF   ('&PDE'(1,1) EQ '(').RPDE
         RCPDSN &PDE,8+&PDE
         RCPPSWD 16+&PDE
         MEXIT
.RPDE    RCPDSN &PDE,8&PDE
         RCPPSWD 16(&PDE)
         MEND
./ ADD NAME=RCPDSNRT
         MACRO
         RCPDSNRT
         SPACE
***********************************************************************
**    DSNAME RETURN TEXT UNIT                                        **
***********************************************************************
         MVI   S99TUKEY+1,DALRTDSN     SET RETURN DSNAME KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         MVI   S99TULNG+1,44           SET LENGTH FIELD
         RCPDINC 50
         MEND
./ ADD NAME=RCPDSRGR
         MACRO
         RCPDSRGR
         SPACE
***********************************************************************
**    DSORG RETURN TEXT UNIT                                         **
***********************************************************************
         MVI   S99TUKEY+1,DALRTORG     SET RETURN DSORG KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         MVI   S99TULNG+1,2            SET LENGTH FIELD
         XC    S99TUPAR(2),S99TUPAR    INITIALIZE FIELD TO ZERO
         RCPDINC 8
         MEND
./ ADD NAME=RCPDUMMY
         MACRO
         RCPDUMMY &DUMMY
         SPACE
***********************************************************************
**      DUMMY DATASET TEXT UNIT                                      **
***********************************************************************
         MVI   S99TUPAR+1,DALDUMMY     MOVE IN DUMMY DS TEXT UNIT KEY
         RCPDINC 4
         MEND
./ ADD NAME=RCPENDD
         MACRO
&NAME    RCPENDD
         GBLB  &RCPECT(2),&RCPUPT(2),&RCPPSCB(2),&RCPS99(2)
         GBLC  &RCPPRE,&RCPWKDS,&RCPDS
         LCLC  &P,&CS
&CS      SETC  '&RCPDS'                PROGRAM CSECT
         AIF   (NOT &RCPS99(1)).TDS
         DYNSPACE
.TDS     AIF   ('&RCPWKDS' EQ '').RCPDS
         DS    0D                      ALIGN TO DOUBLEWORD
&P       SETC  '&RCPPRE'
&P.WKLEN EQU   *-&RCPWKDS              LENGTH OF WORK AREA
.RCPDS   RCPDS
         EJECT
         AIF   (NOT &RCPECT(1) OR &RCPECT(2)).TRYUPT
         IKJECT
&CS      CSECT                         REENTER MAIN CSECT
         EJECT
&RCPECT(2)     SETB           1
.TRYUPT  AIF   (NOT &RCPUPT(1) OR &RCPUPT(2)).TRYPSCB
         IKJUPT
&CS      CSECT                         REENTER MAIN CSECT
         EJECT
&RCPUPT(2) SETB  1
.TRYPSCB AIF   (NOT &RCPPSCB(1) OR &RCPPSCB(2)).TRYS99
         IKJPSCB
&CS      CSECT                         REENTER MAIN CSECT
         EJECT
&RCPPSCB(2) SETB  1
.TRYS99  AIF   (NOT &RCPS99(1) OR &RCPS99(2)).TRYREST
         IEFZB4D0
         EJECT
         IEFZB4D2
&CS      CSECT                         REENTER MAIN CSECT
         EJECT
&RCPS99(2) SETB  1
.TRYREST MEND
./ ADD NAME=RCPFDDN
         MACRO
         RCPFDDN &DDN
         GBLC &DYNP
         SPACE
***********************************************************************
**        FREE DDNAME TEXT UNIT                                      **
***********************************************************************
         AIF   ('&DDN'(1,1) EQ '''').Q
         AIF   ('&DDN'(K'&DDN,1) EQ '/').B
         RCPSR2
         AIF   ('&DDN'(1,1) EQ '(').R
         L     R14,&DDN                LOAD ADDRESS OF DDNAME
         LH    R2,&DDN+4               LOAD LENGTH OF DDNAME
         AGO   .STH
.R       L     R14,0&DDN               LOAD ADDRESS OF DDNAME
         LH    R2,4&DDN                LOAD LENGTH OF DDNAME
.STH     STH   R2,S99TULNG             STORE DDNAME LENGTH
         BCTR  R2,0                    DECREMENT FOR EXECUTE
         EX    R2,&DYNP.MVC            MOVE DDNAME
         MVI   S99TUKEY+1,DUNDDNAM     MOVE IN DDNAME KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         RCPDINC 14
         MEXIT
.Q       RCPBTU DUNDDNAM,1,&DDN
         MEXIT
.B       RCPTUBFR DUNDDNAM,14,&DDN
         MEND
./ ADD NAME=RCPFDISP
         MACRO
         RCPFDISP &DISP
         LCLB  &B(4)
         LCLA  &I
         SPACE
***********************************************************************
**       OVERRIDING DISPOSITION                                      **
***********************************************************************
&B(1)    SETB  ('&DISP' EQ 'KEEP')
&B(2)    SETB  ('&DISP' EQ 'DELETE')
&B(3)    SETB  ('&DISP' EQ 'CATLG')
&B(4)    SETB  ('&DISP' EQ 'UNCATLG')
         AIF   (&B(1) OR &B(2) OR &B(3) OR &B(4)).OK3
         MNOTE 8,'&DISP IS INVALID, DISP=KEEP USED'
&B(1)    SETB  1
.OK3     ANOP
&I       SETA  8*&B(1)+4*&B(2)+2*&B(3)+&B(4)
         MVC   S99TUKEY(8),=Y(DUNOVDSP,1,1,X'0&I.00')
         RCPDINC 8
.EXIT    MEND
./ ADD NAME=RCPFDSN
         MACRO
         RCPFDSN &DSN,&MEM
         LCLC  &MEMBER
         GBLC  &DYNP
         SPACE
***********************************************************************
**      FREE DATA SET TEXT UNIT                                      **
***********************************************************************
         AIF   ('&DSN'(1,1) EQ '''').Q
         AIF   ('&DSN'(K'&DSN,1) EQ '/').BD
         AIF   ('&DSN'(1,1) EQ '(').REG
         RCPSR2
         L     R14,&DSN                LOAD ADDRESS OF DSNAME
         LH    R2,&DSN+4               LOAD LENGTH OF DSNAME
.STH     STH   R2,S99TULNG             STORE DSNAME LENGTH
         BCTR  R2,0                    DECREMENT FOR EXECUTE
         EX    R2,&DYNP.MVC            MOVE DSNAME
         MVI   S99TUKEY+1,DUNDSNAM     MOVE IN DSNAME KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         RCPDINC 50
         AGO   .TMEMBER
.REG     L     R14,0&DSN               LOAD ADDRESS OF DSNAME
         RCPSR2
         LH    R2,4&DSN                LOAD LENGTH OF DSNAME
         AGO   .STH
.BD      RCPTUBFR DUNDSNAM,50,&DSN
         AGO   .TMEMBER
.Q       RCPBTU DUNDSNAM,1,&DSN
.TMEMBER AIF   ('&MEM' EQ '').EXIT
         SPACE
***********************************************************************
**       FREE MEMBER NAME TEXT UNIT                                  **
***********************************************************************
&MEMBER  SETC  '&MEM'
         AIF   ('&MEM' NE '*').MOK
         AIF   ('&DSN'(1,1) NE '''').MAST
         MNOTE 8,'MEMBER=* INVALID WITH QUOTED DSNAME'
         MEXIT
.MAST    ANOP
&MEMBER  SETC  '8+&DSN'
.MOK     ANOP
         AIF   ('&MEMBER'(K'&MEMBER,1) EQ '/').BM
         RCPSR2
         AIF   ('&MEMBER'(1,1) EQ '(').RM
         LH    R2,4+&MEMBER            LOAD LENGTH OF MEMBER NAME
         LTR   R2,R2                   TEST FOR ZERO
         BZ    *+30                    IF NO MEMBER, SKIP
         L     R14,&MEMBER             LOAD ADDRESS OF MEMBER
         AGO   .STHM
.RM      LH    R2,4&MEMBER             LOAD LENGTH OF MEMBER
         LTR   R2,R2                   AND TEST FOR ZERO
         BZ    *+30                    IF NO MEMBER, SKIP
         L     R14,0&MEMBER            LOAD ADDRESS OF MEMBER
.STHM    STH   R2,S99TULNG             STORE LENGTH OF MEMBER
         BCTR  R2,0                    DECREMENT FOR EXECUTE
         EX    R2,&DYNP.MVC            MOVE IN MEMBER NAME
         MVI   S99TUKEY+1,DUNMEMBR     MOVE IN MEMBER KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         RCPDINC 14
         MEXIT
.BM      RCPTUBFR DUNMEMBR,14,&MEMBER
         MEXIT
.QM      RCPBTU DUNMEMBR,1,&MEMBER
.EXIT    MEND
./ ADD NAME=RCPFHOLD
         MACRO
         RCPFHOLD &H
         AIF   ('&H' EQ 'YES').YES
         AIF   ('&H' EQ 'NO').NO
         MNOTE 4,'HOLD PARMETER VALUE INCORRECT - IGNORED'
         MEXIT
.YES     ANOP
         SPACE 1
***********************************************************************
**       OVERIDING SYSOUT HOLD TEXT UNIT                             **
***********************************************************************
         SPACE 1
         MVI   S99TUKEY+1,DUNOVSHQ MOVE IN TEXT UNIT KEY
         RCPDINC 4
         MEXIT
.NO      ANOP
         SPACE 1
***********************************************************************
**       OVERIDING SYSOUT NO HOLD TEXT UNIT                          **
***********************************************************************
         SPACE 1
         MVI   S99TUKEY+1,DUNOVSHQ MOVE IN TEXT UNIT KEY
         RCPDINC 4
         MEND
./ ADD NAME=RCPFORUS
         MACRO - TO SET UP SVC 99 TEXT UNIT 'FOR USER'
         RCPFORUS &T
         SPACE 1
***********************************************************************
**       'FOR USER' TEXT UNIT                                        **
***********************************************************************
         RCPVCHAR 0,5,&T,N=DALUSRID
         MEND
./ ADD NAME=RCPFREE
         MACRO
         RCPFREE &FREE
         SPACE
***********************************************************************
**      UNALLOC AT CLOSE TEXT UNIT                                   **
***********************************************************************
.*   WAS S99TUPAR+1 !!!!!!                                   GYP 92013
         MVI   S99TUKEY+1,DALCLOSE     MOVE IN CLOSE TEXT UNIT KEY
         RCPDINC 4
         MEND
./ ADD NAME=RCPIOPL
         MACRO
&NAME    RCPIOPL
         GBLC  &RCPPRE
         GBLB  &RCPIOPL(2)
         GBLB  &RCPSTPB(2),&RCPPTPB(2),&RCPPGPB(2),&RCPGTPB(2)
         LCLC  &P,&L,&L1
&P       SETC  '&RCPPRE'
         EJECT
         AIF   (&RCPIOPL(2)).BYPIOPL
&RCPIOPL(2) SETB 1
         IKJIOPL
L#IOPL   EQU   *-IOPL                  LENGTH OF IO PARAM LIST
&SYSECT  CSECT                         RESUME PROGRAM CSECT
         SPACE 3
.BYPIOPL RCPDS
&P.IOPL  DS    CL(L#IOPL)              RESERVE SPACE FOR IOPL
         RCPDS
         SPACE 5
***********************************************************************
***   THIS CODE GENERATES AN I/O SERVICE ROUTINE PARAMETER LIST     ***
***********************************************************************
         LA    R1,&P.IOPL              LOAD IOPL ADDRESS
         USING IOPL,R1                 IOPL ADDRESSABLE
         MVC   IOPLUPT,CPPLUPT         MOVE IN ADDRESS OF UPT
         MVC   IOPLECT,CPPLECT         MOVE IN ADDRESS OF ECT
         LA    R15,&P.ECB              LOAD ADDRESS OF ATTN ECB
         ST    R15,IOPLECB             AND STORE IN IOPL
         DROP  R1
  AIF (&RCPSTPB(1) OR &RCPGTPB(1) OR &RCPPGPB(1) OR &RCPPTPB(1)).I
         MEXIT
.I       EJECT
         AIF   (NOT &RCPSTPB(1) OR &RCPSTPB(2)).TPT
         IKJSTPB
&RCPSTPB(2) SETB 1
L#STPB   EQU   *-STPB         LENGTH OF STPB
&SYSECT  CSECT
.TPT     AIF   (NOT &RCPPTPB(1) OR &RCPPTPB(2)).TGT
         IKJPTPB
&RCPPTPB(2) SETB 1
L#PTPB   EQU   *-PTPB         LENGTH OF PTPB
&SYSECT  CSECT
.TGT     AIF   (NOT &RCPGTPB(1) OR &RCPGTPB(2)).TPG
         IKJGTPB
&RCPGTPB(2) SETB 1
L#GTPB   EQU   *-GTPB         LENGTH OF GTPB
&SYSECT  CSECT
.TPG     AIF   (NOT &RCPPGPB(1) OR &RCPPGPB(2)).STO
         IKJPGPB
&RCPPGPB(2) SETB 1
L#PGPB   EQU   *-PGPB         LENGTH OF PGPB
&SYSECT  CSECT
.STO     SPACE 3
&L       SETC  ''
         RCPDS
         AIF   (NOT &RCPSTPB(1)).XPT
&P.STPB  DS    CL(L#STPB)              RESERVE SPACE FOR STPB
&L       SETC  '&L.+L#STPB'
.XPT     AIF   (NOT &RCPPTPB(1)).XGT
&P.PTPB  DS    CL(L#PTPB)              RESERVE SPACE FOR PTPB
&L       SETC  '&L.+L#PTPB'
.XGT     AIF   (NOT &RCPGTPB(1)).XPG
&P.GTPB  DS    CL(L#GTPB)              RESERVE SPACE FOR GTPB
&L       SETC  '&L.+L#GTPB'
.XPG     AIF   (NOT &RCPPGPB(1)).XC
&P.PGPB  DS    CL(L#PGPB)              RESERVE SPACE FOR PGPB
&L       SETC  '&L.+L#PGPB'
.XC      RCPDS
&L1      SETC  '&L'(2,K'&L-1)
&L       SETC  '&P'.'&L1'(3,4)
         XC    &L.(&L1.),&L            CLEAR IOPB AREA
         MEND
./ ADD NAME=RCPLINK
         MACRO
&NAME    RCPLINK &MODULE
         LCLC  &OFFSET,&C
         AIF   ('&MODULE' EQ '').ERROR
         AIF   ('&MODULE' NE 'IKJPARS').T1
&OFFSET  SETC  '524'
         AGO   .START
.T1      AIF   ('&MODULE' NE 'IKJDAIR').T2
&OFFSET  SETC  '732'
         AGO   .START
.T2      AIF   ('&MODULE' NE 'IKJEHDEF').T3
&OFFSET  SETC  '736'
         AGO   .START
.T3      AIF   ('&MODULE' NE 'IKJEHCIR').T4
&OFFSET  SETC  '740'
         AGO   .START
.T4      AIF   ('&MODULE' NE 'IKJPUTL').T5
&OFFSET  SETC  '444'
         AGO   .START
.T5      AIF   ('&MODULE' NE 'IKJGETL').T6
&OFFSET  SETC  '348'
         AGO   .START
.T6      AIF   ('&MODULE' NE 'IKJSCAN').T7
&OFFSET  SETC  '480'
         AGO   .START
.T7      AIF   ('&MODULE' NE 'IKJPTGT').T8
&OFFSET  SETC  '464'
         AGO   .START
.T8      AIF   ('&MODULE' NE 'IKJSTCK').T9
&OFFSET  SETC  '472'
         AGO   .START
.T9      ANOP
&NAME    DS    0H
*
 MNOTE *,' EP OF &MODULE. NOT IN CVT. STANDARD LINK USED'
*
         AGO   .LINK
.START   ANOP
&NAME    L     R15,16                  LOAD CVT ADDRESS
         L     R15,&OFFSET.(R15)       LOAD MODULE ADDRESS
         LTR   R15,R15                 IS MODULE ADDRESS THERE?
&C       SETC  'RCP&SYSNDX'
         BNM   &C.L                     IF NOT, BRANCH TO LINK
         BALSR R14,R15                  ELSE BALR TO IT
         B     &C.B                      AND BYPASS LINK
&C.L     LINK  EP=&MODULE
&C.B     DS    0H                      BRANCHED TO IF LINK BYPASSED
         MEXIT
.LINK    ANOP
&NAME    LINK  EP=&MODULE
         MEXIT
.ERROR   MNOTE 4,'NO MODULE NAME SPECIFIED'
         MEND  ,
./ ADD NAME=RCPLOAD
         MACRO
&NAME    RCPLOAD &MOD,&EP1
         GBLC  &RCPPTEP,&RCPGTEP,&RCPPGEP
         GBLC  &RCPDFEP,&RCPSTEP,&RCPPREP
         GBLC  &RCPPRE
         LCLA  &I,&J
         LCLB  &EPXISTS
         LCLC  &OFFSET,&C,&EP,&MODULE
&EP      SETC  '&EP1'
&MODULE  SETC  '&MOD'
         AIF   ('&MODULE' EQ '').ERROR
         AIF   ('&MODULE'(K'&MOD,1) NE ')').NOBR
&I       SETA  K'&MOD
.LOOP    ANOP
&I       SETA  &I-1
         AIF   (&I LT 2).NOLB
         AIF   ('&MOD'(&I,1) NE '(').LOOP
&MODULE  SETC  '&MOD'(1,&I-1)
&J       SETA  K'&MOD-1-&I
&EP      SETC  '&MOD'(&I+1,&J)
         RCPDS
&EP      DS    F                       TO STORE MODULE ADDRESS
         RCPDS
.NOBR    ANOP
&EPXISTS  SETB  ('&EP' NE '')
         AIF   ('&MODULE' NE 'IKJPARS').T1
&OFFSET  SETC  '524'
&RCPPREP SETC '&EP'
         AIF   (&EPXISTS).START
         RCPDS
&RCPPREP SETC '&RCPPRE.PREP'
&EP      SETC  '&RCPPREP'
&RCPPREP DS    F                       TO HOLD ADDRESS OF IKJPARS
         RCPDS
         AGO   .START
.T1      AIF   ('&MODULE' NE 'IKJDAIR').T2
&OFFSET  SETC  '732'
         AGO   .START
.T2      AIF   ('&MODULE' NE 'IKJEHDEF').T3
&RCPDFEP SETC  '&EP'
&OFFSET  SETC  '736'
         AIF   (&EPXISTS).START
&RCPDFEP SETC  '&RCPPRE.DFEP'
         RCPDS
&RCPDFEP DS    F                       ADDR OF DEFAULT SERVICE ROUTINE
         RCPDS
&EP      SETC  '&RCPDFEP'
         AGO   .START
.T3      AIF   ('&MODULE' NE 'IKJEHCIR').T4
&OFFSET  SETC  '740'
         AGO   .START
.T4      AIF   ('&MODULE' NE 'IKJPUTL').T5
&RCPPTEP SETC  '&EP'
&OFFSET  SETC  '444'
         AIF   (&EPXISTS).START
&RCPPTEP SETC  '&RCPPRE.PTEP'
&EP      SETC  '&RCPPTEP'
         RCPDS
&RCPPTEP DS    F                       ADDR OF PUTLINE ROUTINE
         RCPDS
         AGO   .START
.T5      AIF   ('&MODULE' NE 'IKJGETL').T6
&RCPGTEP SETC  '&EP'
&OFFSET  SETC  '348'
         AIF   (&EPXISTS).START
&RCPGTEP SETC  '&RCPPRE.GTEP'
&EP      SETC  '&RCPGTEP'
         RCPDS
&RCPGTEP DS    F                       ADDR OF GETLINE ROUTINE
         RCPDS
         AGO   .START
.T6      AIF   ('&MODULE' NE 'IKJSCAN').T7
&OFFSET  SETC  '480'
         AGO   .START
.T7      AIF   ('&MODULE' NE 'IKJPTGT').T8
&RCPPGEP SETC  '&EP'
&OFFSET  SETC  '464'
         AIF   (&EPXISTS).START
&RCPPGEP SETC  '&RCPPRE.PGEP'
&EP      SETC  '&RCPPGEP'
         RCPDS
&RCPPGEP DS    F                       ADDR OF PUTGET ROUTINE
         RCPDS
         AGO   .START
.T8      AIF   ('&MODULE' NE 'IKJSTCK').T9
&RCPSTEP SETC  '&EP'
&OFFSET  SETC  '472'
         AIF   (&EPXISTS).START
&RCPSTEP SETC  '&RCPPRE.STEP'
&EP      SETC  '&RCPSTEP'
         RCPDS
&RCPSTEP DS    F                       ADDR OF STACK ROUTINE
         RCPDS
         AGO   .START
.T9      ANOP
&NAME    DS    0H
*
 MNOTE *,' EP OF &MODULE. NOT IN CVT. STANDARD LOAD USED'
*
         AGO   .LOAD
.START   ANOP
&NAME    L     R15,16                  LOAD CVT ADDRESS
         L     R0,&OFFSET.(R15)        LOAD MODULE ADDRESS
         LTR   R0,R0                   IS MODULE LOADED?
&C       SETC  'RCP&SYSNDX'
         BM    &C                      IF SO, BYPASS LOAD MACRO
.LOAD    LOAD EP=&MODULE.
         AIF   ('&EP' EQ '').EPERR
&C       ST    R0,&EP                  STORE ENTRY POINT ADDRESS
         MEXIT
.EPERR   MNOTE 4,'EP RETURN FIELD NOT SPECIFIED'
         MEXIT
.ERROR   MNOTE 4,'NO MODULE NAME SPECIFIED'
         MEXIT
.NOLB    MNOTE 4,'INVALID MODULE NAME ''&MOD'''
         MEND
./ ADD NAME=RCPLOCS1
*23456789*12345*78921234567893123456789*
         MACRO
         RCPLOCSW &SW
.********************************************************************
.*                                                                  *
.*       INNER MACRO USED BY GOIF, SET, RESET AND FLIP.             *
.*       THE PARM PASSED IS THE SWITCH OR LIST OF SWITCHES.         *
.*       RCPLOCSW SCANS THE ARRAYS SET UP BY DCLSW TO SEE IF THE    *
.*       SWITCH BIT NAMES WERE DECLARED, AND IF A LIST WAS PASSED,  *
.*       WHETHER ALL THE SWITCH BITS BELONG TO THE SAME BYTE.       *
.*       RCPLOCSW PASSES BACH THE SWITCH BYTE NAME IN GLOBAL SETC   *
.*       VARIABLE &RCPDSW1 AND THE SWITCH BIT NAME IN GLOBAL SETC   *
.*       &RCPDSW2. IF A LIST OF SWITCHES WAS PASSED, &RCPDSW2       *
.*       CONTAINS THE SWITCH NAMES SEPARATED BY PLUS SIGNS.         *
.*       IF THE FIRST OR ONLY SWITCH WAS NOT FOUND, &RCPDSW1 IS SET *
.*       TO NULL. IF A LIST OF SWITCHES IS PASSED AND ANY SWITCH IS *
.*       NOT DECLARED IN THE SAME SWITCH BYTE AS THE FIRST, AN MNOTE*
.*       IS ISSUED WARNING OF POSSIBLE ERROR, BUT &RCPDSW1 IS SET   *
.*       TO THE NAME OF THE SWITCH BYTE CONTAINING THE FIRST SWITCH *
.*       BIT IN THE LIST.                                           *
.*                                                                  *
.********************************************************************
         GBLA  &RCPDSW#,&RCPGSW#       COUNTER FOR DECLARED SWITCHES
         GBLA  &RCPDSW0                NO OF SWS FOUND BY RCPLOCSW
         GBLB  &RCPDSWD(99)            DEFER DECLARE INDICATORS
         GBLC  &RCPDSWN(99)            SWITCH BYTE NAMES
         GBLC  &RCPDSWB(800)           SWITCH BIT NAMES
         GBLC  &RCPGSWN(99)            GENERIC SWITCH BYTE NAMES
         GBLC  &RCPGSWB(99)            GENERIC SWITCH BIT PREFIXES
         GBLC  &RCPDSW1(20)            SWITCH BYTE NAMES
         GBLC  &RCPDSW2(20)            SWITCH BIT NAME(S)
         LCLA  &I,&J,&K,&L,&M,&N       LOCAL COUNTERS
         LCLC  &C,&SW1,&SW2
&RCPDSW0 SETA  0                       INITIALIZE
&N       SETA  N'&SW                   NO OF SWITCHES ENTERED
&J       SETA  &RCPDSW#*8              INDEX TO LAST DECLARED SW BIT
.LOOP1   AIF   (&M GE &N).EXIT        LOOP FOR EACH SW
&M       SETA  &M+1
&SW2     SETC  '&SW(&M)'               SWITCH TO SEARCH FOR
         AIF   ('&SW2' EQ '').LOOP1    SKIP IF NULL
&I       SETA  8                       INDEX TO FIRST DECLARED SW - 1
.LOOP1A  AIF   (&I GE &J).TGEN         SEARCH NAME ARRAY
&I       SETA  &I+1
         AIF   ('&RCPDSWB(&I)' NE '&SW2').LOOP1A
.*
.*   WE FOUND IT
.*
&L       SETA  (&I-1)/8                INDEX TO BYTE NAME
&SW1     SETC  '&RCPDSWN(&L)'          GET BYTE NAME
.FOUNDSW ANOP                          HAVE WE HAD IT BEFORE?
&K       SETA  0
.SWL1    AIF   (&K GE &RCPDSW0).NEWSW1
&K       SETA  &K+1
         AIF   ('&RCPDSW1(&K)' NE '&SW1').SWL1
.*
.* WE FOUND IT
.*
&RCPDSW2(&K) SETC '&RCPDSW2(&K)+&SW2'  CONCATENATE CURRENT SW
         AGO   .LOOP1                  GO DO NEXT
.NENSW1  ANOP
&RCPDSW0 SETA  &K+1                    NEXT SW BYTE INDEX
&RCPDSW1(&RCPDSW0) SETC '&SW1'         BYTE NAME
&RCPDSW2(&RCPDSW0) SETC '&SW2'         BIT NAME
         AGO   .LOOP1                  GO DO NEXT
.TGEN    ANOP  SEARCH GENERIC NAME ARRAY
&I       SETA  0
&L       SETA  K'&SW2
.LOOP2   ANOP
&I       SETA  &I+1
         AIF   (&I GT &RCPGSW#).NOTFND
&SW1     SETC  '&RCPGSWN(&I)'
         AIF   (&L LT K'&SW1).LOOP2
         AIF   ('&SW1'(1,&L) NE '&SW2').LOOP2
         AGO   .FOUNDSW                EUREKA
.NOTFND  MNOTE 4,'SWITCH ''&SW2'' NOT DECLARED'
         AGO   .LOOP1
.EXIT    MEND
./ ADD NAME=RCPLOCS2
*23456789*12345*78921234567893123456789*
         MACRO
         RCPLOCSW &SW
.********************************************************************
.*                                                                  *
.*       INNER MACRO USED BY GOIF, SET, RESET AND FLIP.             *
.*       THE PARM PASSED IS THE SWITCH OR LIST OF SWITCHES.         *
.*       RCPLOCSW SCANS THE ARRAYS SET UP BY DCLSW TO SEE IF THE    *
.*       SWITCH BIT NAMES WERE DECLARED, AND IF A LIST WAS PASSED,  *
.*       WHETHER ALL THE SWITCH BITS BELONG TO THE SAME BYTE.       *
.*       RCPLOCSW PASSES BACH THE SWITCH BYTE NAME IN GLOBAL SETC   *
.*       VARIABLE &RCPDSW1 AND THE SWITCH BIT NAME IN GLOBAL SETC   *
.*       &RCPDSW2. IF A LIST OF SWITCHES WAS PASSED, &RCPDSW2       *
.*       CONTAINS THE SWITCH NAMES SEPARATED BY PLUS SIGNS.         *
.*       IF THE FIRST OR ONLY SWITCH WAS NOT FOUND, &RCPDSW1 IS SET *
.*       TO NULL. IF A LIST OF SWITCHES IS PASSED AND ANY SWITCH IS *
.*       NOT DECLARED IN THE SAME SWITCH BYTE AS THE FIRST, AN MNOTE*
.*       IS ISSUED WARNING OF POSSIBLE ERROR, BUT &RCPDSW1 IS SET   *
.*       TO THE NAME OF THE SWITCH BYTE CONTAINING THE FIRST SWITCH *
.*       BIT IN THE LIST.                                           *
.*                                                                  *
.********************************************************************
         GBLA  &RCPDSW#,&RCPGSW#       COUNTER FOR DECLARED SWITCHES
         GBLB  &RCPDSWD(99)            DEFER DECLARE INDICATORS
         GBLC  &RCPDSWN(99)            SWITCH BYTE NAMES
         GBLC  &RCPDSWB(800)           SWITCH BIT NAMES
         GBLC  &RCPGSWN(99)            GENERIC SWITCH BYTE NAMES
         GBLC  &RCPGSWB(99)            GENERIC SWITCH BIT PREFIXES
         GBLC  &RCPDSW1                SWITCH BYTE NAME
         GBLC  &RCPDSW2                SWITCH BIT NAME(S)
         LCLA  &I,&J,&K,&L,&M,&N       LOCAL COUNTERS
         LCLC  &C
&RCPDSW2 SETC  '&SW(1)'                EXTRACT 1ST SWITCH BIT
&J       SETA  &RCPDSW#*8+8            ARRAY POS OF LAST SW BIT
&I       SETA  8                       ARRAY POS-1 OF 1ST SW BIT
.LOOP1   AIF   (&I GE &J).TGEN         IF SW NOT FOUND IN 1ST ARRAY,
.*                                      GO SEARCH GENERIC NAME ARRAY
&I       SETA  &I+1
         AIF   ('&RCPDSWB(&I)' NE '&RCPDSW2').LOOP1  LOOK FOR MATCH
.*
.*       OK, WE'VE FOUND A MATCH.
.*
&I       SETA  (&I-1)/8               GET POS OF SWITCH BYTE
&RCPDSW1 SETC  '&RCPDSWN(&I)'         MOVE IT TO EXIT PARM VAR
&I       SETA  &I*8+1                 POINT TO 1ST SW BIT IN IT
&J       SETA  &I+8                   POINT TO LAST SW BIT IN IT
&M       SETA  N'&SW                  GET NO OF SWITCHES
&L       SETA  1
.*
.*       NOW WE PROCESS SUBSEQUENT SWITCHES IN THE LIST
.*
.LOOP2   AIF   (&L GE &M).EXIT        EXIT WHEN FINISHED
&L       SETA  &L+1                   POINT TO NEXT SW IN LIST
&C       SETC  '&SW(&L)'               EXTRACT IT
&RCPDSW2 SETC  '&RCPDSW2.+&C'           THEN APPEND TO PREVIOUS
.*
.*       NOW WE CHECK THAT THE SWITCH IS DECLARED IN THE SAME
.*       BYTE AS THE FIRST.
.*
&N       SETA  &I-1                     POINT TO 1ST BIT POS MINUS 1
.LOOP3   AIF   (&N GE &J).NM            IF SW NOT FOUND, ISSUE MNOTE
&N       SETA  &N+1                     POINT TO NEXT
         AIF   ('&C' NE '&RCPDSWB(&N)').LOOP3  SEARCH FOR MATCH
         AGO   .LOOP2                   IF FOUND, GO PROCESS NEXT
.NM      MNOTE 4,'WARNING: SWITCH ''&C'' NOT DECLARED IN SAME BYTE AS  X
               SWITCH ''&SW(1)'' - LOGIC ERROR MAY OCCUR'
         AGO   .LOOP2            CONTINUE FOR NEXT SWITCH BIT
.*
.*       IF THE SWITCH WAS NOT LOCATED IN THE EXPLICIT NAME ARRAY,
.*       THE GENERIC NAME ARRAY IS SEARCHED.
.*
.TGEN    ANOP
&I       SETA  0
&RCPDSW2 SETC  '&SW(1)'                EXTRACT 1ST SWITCH
&L       SETA  K'&RCPDSW2              GET LENGTH OF 1ST SW
.LOOP4   AIF   (&I GE &RCPGSW#).ERROR  IF NOT SW NOT DECLARED, ERROR
&I       SETA  &I+1
&C       SETC  '&RCPGSWB(&I)'          GET GENERIC PREFIX
&K       SETA  K'&C                    GET LENGTH OF GENERIC PREFIX
         AIF   (&L LT &K).LOOP4         AND SKIP IF LEN OF SWITCH NAME
.*                                          < LEN OF GENERIC PREFIX
         AIF   ('&RCPDSW2'(1,&K) NE '&C').LOOP4  ALSO SKIP IF NO MATCH
&RCPDSW1 SETC  '&RCPGSWN(&I)'          SAVE SWITCH BYTE NAME
&I       SETA   1
&J       SETA   N'&SW
.LOOP5   AIF   (&I GE &J).EXIT         EXIT WHEN FINISHED
&I       SETA   &I+1
&RCPDSW2 SETC   '&RCPDSW2.+&SW(&I)'     APPEND THIS SWITCH
         AIF    ('&SW(&I)    '(1,&K) EQ '&C').LOOP5 CHECK PREFIX
         MNOTE 4,'WARNING: SWITCH ''&SW(&I)'' NOT GENERICALLY EQUAL TO X
               SWITCH ''&SW(1)'''
         AGO   .LOOP5
.ERROR   MNOTE 8,'SWITCH ''&SW(1)'' NOT DECLARED'
&RCPDSW1 SETC  ''             INDICATE ERROR
.EXIT    MEND
./ ADD NAME=RCPLOCSW
*23456789*12345*78921234567893123456789*
         MACRO
         RCPLOCSW &SW
.********************************************************************
.*                                                                  *
.*       INNER MACRO USED BY GOIF, SET, RESET AND FLIP.             *
.*       THE PARM PASSED IS THE SWITCH OR LIST OF SWITCHES.         *
.*       RCPLOCSW SCANS THE ARRAYS SET UP BY DCLSW TO SEE IF THE    *
.*       SWITCH BIT NAMES WERE DECLARED, AND IF A LIST WAS PASSED,  *
.*       WHETHER ALL THE SWITCH BITS BELONG TO THE SAME BYTE.       *
.*       RCPLOCSW PASSES BACH THE SWITCH BYTE NAME IN GLOBAL SETC   *
.*       VARIABLE &RCPDSW1 AND THE SWITCH BIT NAME IN GLOBAL SETC   *
.*       &RCPDSW2. IF A LIST OF SWITCHES WAS PASSED, &RCPDSW2       *
.*       CONTAINS THE SWITCH NAMES SEPARATED BY PLUS SIGNS.         *
.*       IF THE FIRST OR ONLY SWITCH WAS NOT FOUND, &RCPDSW1 IS SET *
.*       TO NULL. IF A LIST OF SWITCHES IS PASSED AND ANY SWITCH IS *
.*       NOT DECLARED IN THE SAME SWITCH BYTE AS THE FIRST, AN MNOTE*
.*       IS ISSUED WARNING OF POSSIBLE ERROR, BUT &RCPDSW1 IS SET   *
.*       TO THE NAME OF THE SWITCH BYTE CONTAINING THE FIRST SWITCH *
.*       BIT IN THE LIST.                                           *
.*                                                                  *
.********************************************************************
         GBLA  &RCPDSW#,&RCPGSW#       COUNTER FOR DECLARED SWITCHES
         GBLA  &RCPDSW0                NO OF SWS FOUND BY RCPLOCSW
         GBLB  &RCPDSWD(99)            DEFER DECLARE INDICATORS
         GBLB  &RCPDSW3(20)   INVERT INDICATOR
         GBLC  &RCPDSWN(99)            SWITCH BYTE NAMES
         GBLC  &RCPDSWB(800)           SWITCH BIT NAMES
         GBLC  &RCPGSWN(99)            GENERIC SWITCH BYTE NAMES
         GBLC  &RCPGSWB(99)            GENERIC SWITCH BIT PREFIXES
         GBLC  &RCPDSW1(20)            SWITCH BYTE NAMES
         GBLC  &RCPDSW2(20)            SWITCH BIT NAME(S)
         LCLA  &I,&J,&K,&L,&M,&N
         LCLB  &NOT
         LCLC  &C,&SW1,&SW2
&RCPDSW0 SETA  0                       INITIALIZE
&N       SETA  N'&SW                   NO OF SWITCHES ENTERED
&J       SETA  &RCPDSW#*8+8            INDEX TO LAST DECLARED SW BIT
.LOOP1   AIF   (&M GE &N).EXIT        LOOP FOR EACH SW
&M       SETA  &M+1
&SW2     SETC  '&SW(&M)'               SWITCH TO SEARCH FOR
         AIF   ('&SW2' EQ '').LOOP1    SKIP IF NULL
&I       SETA  8                       INDEX TO FIRST DECLARED SW - 1
&NOT     SETB  0
         AIF   ('&SW2'(1,1) NE '^' AND '&SW2'(1,1) NE '-').TNOT2
&SW2     SETC  '&SW2'(2,K'&SW2-1)       REMOVE NOT SIGN
&NOT     SETB  1                       INDICATE INVERT FUNCTION
         AGO   .LOOP1A                 CONTINUE
.TNOT2   AIF   (K'&SW2 LT 5).LOOP1A    CHECK LENGTH
         AIF   ('&SW2'(1,4) NE 'NOT-').LOOP1A  WAS SWITCH INVERTED?
&SW2     SETC  '&SW2'(5,K'&SW2-4)      STRIP OFF 'NOT-'
&NOT     SETB  1                       INDICATE INVERTED
.LOOP1A  AIF   (&I GE &J).TGEN         SEARCH NAME ARRAY
&I       SETA  &I+1
         AIF   ('&RCPDSWB(&I)' NE '&SW2').LOOP1A
.*
.*   WE FOUND IT
.*
&L       SETA  (&I-1)/8                INDEX TO BYTE NAME
&SW1     SETC  '&RCPDSWN(&L)'          GET BYTE NAME
.FOUNDSW ANOP                          HAVE WE HAD IT BEFORE?
&K       SETA  0
.SWL1    AIF   (&K GE &RCPDSW0).NEWSW1
&K       SETA  &K+1
         AIF   ('&RCPDSW1(&K)' NE '&SW1').SWL1
         AIF   (&RCPDSW3(&K) NE &NOT).SWL1  ENSURE INVERT BIT THE SAME
.*
.* WE FOUND IT
.*
&RCPDSW2(&K) SETC '&RCPDSW2(&K)+&SW2'  CONCATENATE CURRENT SW
         AGO   .LOOP1                  GO DO NEXT
.NEWSW1  ANOP
&RCPDSW0 SETA  &K+1                    NEXT SW BYTE INDEX
&RCPDSW1(&RCPDSW0) SETC '&SW1'         BYTE NAME
&RCPDSW2(&RCPDSW0) SETC '&SW2'         BIT NAME
&RCPDSW3(&RCPDSW0) SETB (&NOT)         SET INVERT INDICATOR
         AGO   .LOOP1                  GO DO NEXT
.TGEN    ANOP  SEARCH GENERIC NAME ARRAY
&I       SETA  0
&L       SETA  K'&SW2
.LOOP2   ANOP
&I       SETA  &I+1
         AIF   (&I GT &RCPGSW#).NOTFND
&C       SETC  '&RCPGSWB(&I)'
         AIF   (&L LT K'&C).LOOP2
         AIF   ('&SW2'(1,K'&C) NE '&C').LOOP2
&SW1     SETC  '&RCPGSWN(&I)'
         AGO   .FOUNDSW                EUREKA
.NOTFND  MNOTE 4,'SWITCH ''&SW2'' NOT DECLARED'
         AGO   .LOOP1
.EXIT    MEND
./ ADD NAME=RCPMCA
         MACRO
         RCPMCA &DSECT=YES
         GBLC  &RCPPRE
         GBLA  &RCPSWS(10)
         LCLC  &P
     RCPDEBUG
&P       SETC  '&RCPPRE'
         AIF   (&RCPSWS(2) NE 2).DSECT
&P.MCA   DS    0F                      MODULE COMMUNICATIONS AREA
         AGO   .MCA2
.DSECT   ANOP
&P.MCA   DSECT                         MODULE COMMUNICATIONS AREA
.MCA2    ANOP
&P.XDS   DS    F                       ADDR OF EXTERNAL DUMMY SECTION
         AIF   (&RCPSWS(3) LT 1).EXIT
&P.A#GET DS    F                       ADDRESS OF LIFO GET ROUTINE
&P.A#FRE DS    F                       ADDRESS OF LIFO FREE ROUTINE
&P.#S    DS    F                       ADDRESS OF CURRENT LIFO STACK
&P.#E    DS    F                       ADDRESS OF END OF LIFO STACK
&P.#N    DS    F                       ADDRESS OF NEXT FREE AREA
&P.#C    DS    F                       ADDRESS OF NEXT LIFO STACK
&P.#L    DS    F                       LENGTH OF CURRENT LIFO STACK
.EXIT    MEND
./ ADD NAME=RCPMSVGP
         MACRO
         RCPMSVGP &MSVGP
         GBLC  &DYNP
         SPACE 1
***********************************************************************
**   BUILD THE MSVGP TEXT UNIT                                       **
***********************************************************************
         AIF   ('&MSVGP'(K'&MSVGP,1) EQ '/').BTU
         AIF   ('&MSVGP'(1,1) EQ '''').Q
         RCPSR2
         AIF   ('&MSVGP'(1,1) EQ '(').R
         L     R14,&MSVGP              LOAD ADDRESS OF MSVGP
         LH    R2,&MSVGP+4             LOAD LENGTH OF MSVGP
         AGO   .STH
.R       L     R14,0&MSVGP             LOAD ADDRESS OF MSVGP
         LH    R2,4&MSVGP              LOAD LENGTH OF MSVGP
.STH     STH   R2,S99TULNG             STORE MSVGP LENGTH
         BCTR  R2,0                    DECREMENT FOR EXECUTE
         EX    R2,&DYNP.MVC            MOVE MSVGP
         MVI   S99TUKEY+1,DALMSVGP     MOVE IN MSVGP KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         RCPDINC 14
         MEXIT
.Q       RCPBTU DALMSVGP,1,&MSVGP
         MEXIT
.BTU     RCPTUBFR DALMSVGP,14,&MSVGP
         MEND
./ ADD NAME=RCPNTU
         MACRO
         RCPNTU &KEY,&LEN,&PAR
.*
.*     THIS IS AN ALLOC/FREE MACRO TEXT UNIT PROCESSOR SUBROUTINE
.*     MACRO. IT BUILDS NUMERIC TYPE TEXT UNITS.
.*
         LCLA  &L,&R
         LCLC  &C
         GBLC  &RCPTYPE
.*  ALLOC/FREE INNER MACRO TO SET UP NUMERIC TEXT UNITS
&L       SETA  1                       DEFAULT LENGTH
         AIF   ('&LEN' EQ '').NL
&L       SETA  &LEN
.NL      MVI   S99TUKEY+1,&KEY         SET KEY FIELD
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         MVI   S99TULNG+1,&L           SET LENGTH FIELD
         AIF   ('&PAR'(1,1) EQ '(').REG
         RCPTYPE &PAR                  ANALYSE PARAMETER
         AIF   ('&RCPTYPE' EQ 'N').NUMERIC
&R       SETA  4-&L
         MVC   S99TUPAR(&L),&R+&PAR    MOVE IN QUANTITY
         RCPDINC 10
         MEXIT
.NUMERIC AIF   (&L EQ 1).NL1
         MVC   S99TUPAR(&L.),=AL&L.(&PAR) MOVE IN QUANTITY
&R       SETA  &L+6
         AIF   (&R/2 EQ (&R+1)/2).LOK ENSURE LENGTH EVEN
&R       SETA  &R+1
.LOK     RCPDINC &R
         MEXIT
.NL1     MVI   S99TUPAR,&PAR           MOVE IN QUANTITY
         RCPDINC 8
         MEXIT
.REG     ANOP
&C       SETC  '&PAR'(2,K'&PAR-2)
         AIF   (&L EQ 3).STCM
         AIF   (&L EQ 2).STH
         AIF   (&L EQ 1).STC
         ST    &C,S99TUPAR             STORE TEXT UNIT QUANTITY
         AGO   .RCPDINC
.STH     STH   &C,S99TUPAR             STORE TEXT UNIT QUANTITY
         AGO   .RCPDINC
.STC     STC   &C,S99TUPAR             STORE TEXT UNIT QUANTITY
         AGO   .RCPDINC
.STCM    STCM  &C,7,S99TUPAR           STORE TEXT UNIT QUANTITY
.RCPDINC RCPDINC 10
         MEND
./ ADD NAME=RCPPERM
         MACRO
         RCPPERM
         SPACE
***********************************************************************
**     PERMANENTLY ALLOCATED ATTRIBUTE TEXT UNIT                     **
***********************************************************************
         MVI   S99TUKEY+1,DALPERMA     SET TEXT UNIT KEY
         RCPDINC  4
         MEND
./ ADD NAME=RCPPPL
         MACRO
&NAME    RCPPPL &PCL=,&NOPARM=,&PARSERR=,&PDLREG=R11,                  X
               &PDLNAME=,&PARSEP=,&PARSWKA=
         GBLB  &RCPPPL(2),&RCPECT(2)
         GBLC  &RCPPRE,&RCPPREP
         LCLC  &P
&P       SETC  '&RCPPRE'
         AIF   (&RCPPPL(2)).BPPL
         EJECT
         IKJPPL
L#PPL    EQU   *-PPL                   LENGTH OF PPL
&SYSECT  CSECT
         SPACE 1
&RCPPPL(2) SETB 1
.BPPL    RCPDS
&P.PPL   DS    CL(L#PPL)               RESERVE SPACE FOR PPL
&P.PDLP  DS    F                       POINTER TO PDL
         RCPDS
         SPACE 6
***********************************************************************
***   THIS CODE GENERATES A PARSE PARAMETER LIST                    ***
***********************************************************************
         XC    &P.PDLP,&P.PDLP         ZERO PDL POINTER
         AIF   ('&NOPARM(1)' EQ '' OR '&NOPARM(2)' NE '').PB2
         L     R1,CPPLECT              LOAD ECT ADDRESS
&RCPECT(1) SETB 1
         USING ECT,R1                  ECT ADDRESSABLE
         TM    ECTSWS,ECTNOPD          WERE ANY OPERANDS SUPPLIED?
         BO    &NOPARM(1)              NO, BRANCH OUT
         SPACE
.PB2     LA    R1,&P.PPL               LOAD PPL ADDRESS
         USING PPL,R1
         MVC   PPLUPT,CPPLUPT          MOVE IN UPT ADDRESS
         MVC   PPLECT,CPPLECT          MOVE IN ECT ADDRESS
         MVC   PPLCBUF,CPPLCBUF        MOVE IN CBUF ADDRESS
         LA    R15,&P.ECB              LOAD ATTN ECB ADDRESS
         ST    R15,PPLECB              AND STORE IN PPL
         LA    R15,&P.PDLP             LOAD PDL POINTER ADDRESS
         ST    R15,PPLANS               AND STORE IN PPL
         AIF   ('&PARSWKA' EQ '').PB3
         AIF   ('&PARSWKA'(1,1) EQ '').PB4
         LA    R15,&PARSWKA            LOAD ADDRESS OF WORK AREA
         ST    R15,PPLUWA               AND STORE IN PPL
         AGO   .PB3
.PB4     ST    &PARSWKA(1),PPLUWA      STORE ADDRESS OF WORKAREA
.PB3     AIF   ('&PCL' EQ '').EXIT
         L     R15,=V(&PCL)            LOAD PCL ADDRESS
         ST    R15,PPLPCL              AND STORE IN PPL
         SPACE 2
         AIF   ('&NOPARM(1)' EQ '' OR '&NOPARM(2)' EQ '').PB5
         L     R1,CPPLECT              LOAD ECT ADDRESS
&RCPECT(1) SETB 1
         USING ECT,R1
         TM    ECTSWS,ECTNOPD          WERE ANY OPERANDS SUPPLIED?
         BO    &NOPARM(1)               NO, BRANCH OUT
         SPACE
.PB5     AIF   ('&SYSPARM' EQ 'MVT').MVTBYP
         AIF   ('&RCPPREP' EQ '').NOPREP
         L     R15,&RCPPREP            LOAD EP OF IKJPARS
         BALSR R14,R15                  AND ENTER IT
         AGO   .PRET
.NOPREP  ANOP
         L     R15,16                  LOAD CVT ADDRESS
         TM    524(R15),X'80'          IS IKJPARS LOADED?
         AIF   ('&PARSEP' EQ '').PBL1
         BZ    &P.LOAD                  NO, BRANCH TO LOAD SVC
         L     R15,524(,R15)           LOAD EP OF IKJPARS
         ST    R15,&PARSEP             SAVE ITS ADDRESS
         BALSR R14,R15                 THEN BALR TO IT
         B     &P.PLNKB                BYPASS LOAD SVC
&P.LOAD  LOAD  EP=IKJPARS
         LR    R15,R0                  LOAD EP OF IKJPARS
         ST    R15,&PARSEP             SAVE IT
         BALSR R14,R15                 THEN BALR TO IT
&P.PLNKB DS    0H
         AGO   .PRET
.PBL1    BZ    &P.PLINK                 NO, BRANCH TO LINK SVC
         L     R15,524(,R15)           ELSE LOAD ITS ADDRESS
         BALSR R14,R15                  AND BALR TO IT
         B     &P.PLNKB                BYPASS LINK SVC
.MVTBYP  ANOP
&P.PLINK LINK  EP=IKJPARS
&P.PLNKB DS    0H
.PRET    AIF   ('&PARSERR' EQ '').EXIT
         SPACE ,
         LTR   R15,R15                 TEST RETURN CODE
         BNZ   &PARSERR                 AND BRANCH ON NON-ZERO
         SPACE ,
         AIF   ('&PDLREG' EQ '' OR '&PDLNAME' EQ '').EXIT
         L     &PDLREG,&P.PDLP         LOAD PDL ADDRESS
         USING &PDLNAME,&PDLREG        PDL DSECT ADDRESSABLE
.EXIT    MEND  ,
./ ADD NAME=RCPPROC
         MACRO
         RCPPROC &WKCSECT=,&WKDSECT=,                                  X
               &REG1=,&REG0=,&ISA=,&SAVEPRE=,                          X
               &SAVESUF=,&SP=
         GBLA  &RCPSWS(10)
         GBLC  &RCPPRE,&RCPWKCS,&RCPWKDS
         GBLC  &RCPSPN
         LCLC  &P,&C
         RCPDEBUG
&P       SETC  '&RCPPRE'
         AIF   ('&WKCSECT' EQ '').TDS
         SPACE
         MNOTE 4,'WKCSECT= OPTION INVALID WITH PROC OPTION, '
         MNOTE *,'    WKDSECT=  USED INSTEAD'
&RCPWKDS SETC  '&WKCSECT'
         AGO   .SETCS
.TDS     AIF   ('&WKDSECT' EQ '').SYSECT
&RCPWKDS SETC  '&WKDSECT'
         AGO   .SETCS
.SYSECT  ANOP
&RCPWKDS SETC  '&SYSECT'
.SET1    AIF   (K'&RCPWKDS LT 8).LOK
&RCPWKDS SETC  '&RCPWKDS'(1,4)'&RCPWKDS'(6,3)'1'
         AGO   .SETCS
.LOK     ANOP
&RCPWKDS SETC  '&RCPWKDS.1'
.SETCS   ANOP
&RCPWKCS SETC  ''
&RCPSWS(4) SETA &RCPSWS(2)-1 SET W/A TO BE FREED OPT IF PROC(MAIN)
         AIF   ('&ISA' EQ '').NISA
&RCPSWS(3) SETA 1                      SET LIFO FLAG IF ISA SPEC
.NISA    ANOP
         SPACE 2
         RCPDS
         DS    9D                      SAVE AREA
&P.RCODE DS    F                       RETURN CODE
         RCPMCA
         RCPDS
         SPACE 2
         AIF   ('&REG1' EQ '').TR0
         LR    &REG1,R1                SAVE CONTENTS OF REG 1
.TR0     AIF   ('&REG0' EQ '').TP
         LR    &REG0,R0                SAVE CONTENTS OF REG 0
.TP      AIF   (&RCPSWS(2) EQ 2).PROCMN   PROCMAIN OPTION
         AIF   (&RCPSWS(3) EQ 1).PL    LIFO OPTION
         L     R15,0(R13)              R15 -> MODULE COMMUNIC. AREA
         L     R15,&P.XDS-&P.MCA(R15)  LOAD EXTERNAL DUMMY SECT ADDR
         AL    R15,&P.QCON             GET OFFSET TO WORK AREA
         ST    R15,8(R13)              CHAIN SAVE
         ST    R13,4(R15)               AREAS TOGETHER
         MVC   0(4,R15),0(R13)         COPY POINTER TO COMM AREA
         LR    R13,R15                 LOAD WORK AREA ADDRESS
         USING &RCPWKDS,R13              ESTABLISH ADDRESSABLITY TO IT
         MEXIT
.PL      ANOP
***********************************************************************
*        GET WORKAREA FROM LIFO STACK                                 *
***********************************************************************
         #GET  LV=&P.WKLEN
         ST    R1,8(R13)               CHAIN SAVE
         ST    R13,4(R1)                AREAS TOGETHER
         MVC   0(4,R1),0(R13)          PROPAGATE MODULE COMM. AREA ADDR
         LR    R13,R1                  LOAD WORK AREA ADDRESS
         USING &RCPWKDS,R13             ESTABLISH ADDRESSABILITY TO IT
         MEXIT
.PROCMN  L     R0,&P.CXD               LOAD WORK AREA LENGTH
         AIF   ('&SYSPARM' EQ 'MVT').MVT
 MNOTE *,'      GETMAIN RU,LV=(0),SP=&SP,BNDRY=PAGE'
         GETMAIN RU,LV=(0),SP=&SP,BNDRY=PAGE
         AGO   .CONT
.MVT     AIF   ('&SP' EQ '').NOSP
         ICM   R0,8,=AL1(&SP)          INSERT SUBPOOL NUMBER
.NOSP    ANOP
*        GETMAIN R,LV=(0)              OBTAIN A WORK AREA
.CONT    ANOP
&RCPSPN  SETC  '&SP'
         LR    R15,R13                 SAVE CALLER'S SAVE AREA ADDR
         LR    R13,R1                  LOAD EXT DUMMY SECTION ADDR
         AL    R13,&P.QCON              ADD OFFSET TO WORK AREA
         ST    R13,8(R15)              CHAIN SAVE
         ST    R15,4(R13)               AREAS TOGETHER
         USING &RCPWKDS,R13            GET WORKAREA ADDRESSABILITY
         ST    R1,&P.XDS               STORE DUMMY SECTION ADDR IN     X
                                         MODULE COMMUNICATIONS AREA
         LA    R15,&P.MCA              STORE COMMUNICATIONS AREA ADDR
         ST    R15,0(R13)               IN WORD 1 OF SAVE AREA
         AIF   (&RCPSWS(3) EQ 0 AND '&ISA' EQ '').EXIT
&RCPSWS(3) SETA 1                      SET LIFO IN CASE ONLY ISA SPEC
&C       SETC  '&ISA'
         AIF   ('&ISA' NE '').TK
&C       SETC  '8192'
         AGO   .NK
.TK      AIF   ('&C'(K'&C,1) NE 'K').NK
&C       SETC  '&C'(1,K'&C-1)'*1024'
.NK      EJECT
***********************************************************************
**       INITIALIZE MODULE COMMUNICATIONS AREA WITH POINTERS         **
**       TO LIFO STACK AND LIFO GET/FREE ROUTINES                    **
***********************************************************************
         SPACE 1
         MVC   &P.A#GET,=V(#####GET)   MOVE LIFO GET AND FREE
         MVC   &P.A#FRE,=V(####FREE)    ROUTINE ADDRESSES TO MCA
         L     R15,=Q(#####ISA)        COMPUTE LIFO STACK
         AL    R15,&P.XDS               PSEUDO REGISTER OFFSET
         ST    R15,&P.#S                 AND INITIALIZE POINTERS
         ST    R15,&P.#N                  IN MODULE COMMUNICATIONS AREA
         L     R14,=A(&C)              LOAD SIZE OF INITIAL STACK AREA
         ST    R14,&P.#L               STORE THIS IN MCA
         ALR   R15,R14                  THEN COMPUTE STACK END ADDRESS
         ST    R15,&P.#E                 AND STORE THIS INTO MCA
         EJECT
***********************************************************************
**       LIFO STACK GET/FREE ROUTINES                                **
***********************************************************************
         SPACE 1
#####ISA DXD   CL(&C)                  DEFINE PSEUDO REGISTER FOR ISA
         SPACE 1
#####GET CSECT                         LIFO GET ROUTINE
         USING *,R15
         USING &P.MCA,R1
         A     R0,&P.F7                ROUND LENGTH UP TO
         N     R0,&P.F8                 A MULTIPLE OF 8
         AL    R0,&P.#N                COMPUTE NEXT FREE LIFO SLOT ADDR
         CL    R0,&P.#E                COMPARE TO STACK END ADDRESS
         BH    &P.GA                    AND IF TOO BIG, BRANCH
         LR    R15,R1                  PRESERVE MCA ADDRESS
         USING &P.MCA,R15              NEW BASE
         L     R1,&P.#N                LOAD ADDRESS OF SLOT
         ST    R0,&P.#N                 AND STORE ADDRESS OF NEXT SLOT
         BR    R14                     RETURN TO CALLER
         SPACE 1
&P.GA    EQU   *                       IF CURRENT SLOT TOO SMALL
*        ABEND 1000,DUMP                ABEND FOR NOW
         ABEND 1000,DUMP
         SPACE 2
####FREE DS    0H                      LIFO FREE ROUTINE
         ENTRY ####FREE
         USING *,R15                   BASE ADDRESS
         USING &P.MCA,R1               MCA ADDRESS
         CL    R0,&P.#S                CHECK THAT
         BL    &P.FA                    ADDRESS TO BE
         CL    R0,&P.#E                  FREED IS WITHIN
         BH    &P.FA                      BOUND OF CURRENT STACK
         AL    R0,&P.F7                GET UPPER DOUBLE
         N     R0,&P.F8                 WORD BOUNDARY
         ST    R0,&P.#N                  AND UPDATE MCA
         BR    R14                     RETURN TO CALLER
         SPACE 1
&P.FA    EQU   *                       IF ADDRESS NOT WITHIN THIS STACK
*        ABEND 1001,DUMP               ABEND
         ABEND 1001,DUMP
         SPACE 2
&P.F7    DC    F'7'                    CONSTANTS
&P.F8    DC    F'-8'                    TO ROUND UP TO DOUBLEWORD SIZE
         DROP  R1,R15                  KILL ADDRESSABILITY
&SYSECT  CSECT                         RESUME MAIN PROGRAM CSECT
.EXIT    MEND
./ ADD NAME=RCPPSWD
         MACRO
         RCPPSWD &PASSW
         GBLC  &DYNP
         SPACE
***********************************************************************
**   BUILD THE PASSWORD TEXT UNIT                                    **
***********************************************************************
         RCPVCHAR DALPASSW,14,&PASSW
         MEND
./ ADD NAME=RCPQNAME
         MACRO
         RCPQNAME &QNAME
         GBLC  &DYNP
         SPACE
***********************************************************************
**   BUILD THE QNAME TEXT UNIT                                       **
***********************************************************************
         RCPVCHAR DALQNAME,14,&QNAME
         MEND
./ ADD NAME=RCPRNGE
         MACRO - BREAK A RANGE PARAMETER INTO TWO
         RCPRNGE &P
         GBLC  &RCPRNGE(2)
         LCLA  &I,&J,&K
&K       SETA  K'&P
&RCPRNGE(1) SETC ''
&RCPRNGE(2) SETC ''
.LOOP    ANOP
&I       SETA  &I+1
         AIF   (&I GT &K).NR
         AIF   ('&P'(&I,1) NE '-' AND '&P'(&I,1) NE ':').LOOP
&RCPRNGE(1) SETC '&P'(1,&I-1)
&RCPRNGE(2) SETC '&P'(&I+1,&K-&I)
         MEXIT
.NR      ANOP
&RCPRNGE(1) SETC '&P'
         MEND
./ ADD NAME=RCPSPACE
         MACRO
         RCPSPACE &SPACE
         GBLA  &RCPSUB#                NO OF SUBLIST ELEMENTS
         GBLC  &RCPSUBL(100)           SUBLIST ELEMENTS
.**********************************************************************
.*    THIS IS AN ALLOC INNER MACRO TO BUILD THE ALLOCATION SPACE
.*    QUANTITY TEXT UNIT. IT SHOULD BE SPECIFIED AS:-
.*     SPACE=(TYPE,(PRIMARY,SECONDARY,DIRECTORY),RLSE,CONTIG,ROUND)
.*   WHERE TYPE IS 'TRK', 'CYL', 'ABSTR' OR A BLOCK QUANTITY
.*     'CYL' OR 'TRK' SHOULD NOT BE ENTERED IN QUOTES. THE BLOCK
.*     QUANTITY CAN BE A NUMBER, A REGISTER (IN BRACKETS), OR THE
.*     NAME OF A FULLWORD CONTAINING THE BLOCK SIZE.
.**********************************************************************
         AIF   ('&SPACE(1)' EQ '' OR '&SPACE(1)' EQ 'TRK').TRK
         AIF   ('&SPACE(1)' EQ 'CYL').CYL
***********************************************************************
**        SPACE UNIT IN BLOCKS                                       **
***********************************************************************
         RCPNTU DALBLKLN,3,&SPACE(1)  GENERATE BLOCK UNIT TU
         AGO   .TPRIME        GO TEST PRIME QUANTITY
.TRK     ANOP  TRACK SPEC REQ OR DEFAULTED
         SPACE
***********************************************************************
**       SPACE QUANTITY IN TRACKS                                    **
***********************************************************************
         MVI   S99TUKEY+1,DALTRK       SET TEXT UNIT KEY
         RCPDINC 4
         AGO   .TPRIME
.CYL     ANOP  CYL QUANTITY
         SPACE 1
***********************************************************************
**      SPACE UNIT IN CYLINDERS                                      **
***********************************************************************
         MVI   S99TUKEY+1,DALCYL       SET TEXT UNIT KEY
         RCPDINC 4                     STORE TEXT UNIT ADDR
.TPRIME  RCPSUBL &SPACE(2)             BREAK UP SUBLIST
         AIF   (&RCPSUB# EQ 0).TCONTIG
         AIF   ('&RCPSUBL(1)' EQ '').TSP2
         SPACE
***********************************************************************
**       PRIMARY SPACE QUANTITY                                      **
***********************************************************************
         RCPNTU DALPRIME,3,&RCPSUBL(1)
.TSP2    AIF   (&RCPSUB# LT 2).TCONTIG
         AIF   ('&RCPSUBL(2)' EQ '').TSP3
         SPACE
***********************************************************************
**       SECONDARY SPACE QUANTITY                                    **
***********************************************************************
         RCPNTU DALSECND,3,&RCPSUBL(2)
.TSP3    AIF   (&RCPSUB# LT 3).TCONTIG
         AIF   ('&RCPSUBL(3)' EQ '').TCONTIG
         SPACE
***********************************************************************
**       DIRECTORY BLOCK QUANTITY                                    **
***********************************************************************
         RCPNTU DALDIR,3,&RCPSUBL(3)
.TCONTIG AIF  ('&SPACE(3)' EQ 'CONTIG' OR '&SPACE(4)' EQ 'CONTIG').CON
         AIF   ('&SPACE(3)' EQ 'MXIG' OR '&SPACE(4)' EQ 'MXIG').MXIG
         AIF   ('&SPACE(3)' EQ 'ALX' OR '&SPACE(4)' EQ 'ALX').ALX
.TRLSE   AIF   ('&SPACE(3)' EQ 'RLSE' OR '&SPACE(4)' EQ 'RLSE').RLSE
.TROUND  AIF   ('&SPACE(4)'EQ'ROUND'OR'&SPACE(5)'EQ'ROUND').ROUND
         MEXIT
.CON     ANOP
***********************************************************************
**      CONTIGUOUS SPACE TEXT UNIT                                   **
***********************************************************************
         RCPNTU DALSPFRM,1,8
         AGO   .TRLSE
.MXIG    ANOP
***********************************************************************
**       MAXIMUM CONTIGUOUS SPACE TEXT UNIT                          **
***********************************************************************
         RCPNTU DALSPFRM,1,4
         AGO   .TRLSE
.ALX     ANOP
***********************************************************************
**       'ALX' SPACE TEXT UNIT                                       **
***********************************************************************
         RCPNTU DALSPFRM,1,2
         AGO   .TRLSE
.RLSE    ANOP
***********************************************************************
**      RELEASE UNUSED SPACE TEXT UNIT                               **
***********************************************************************
         MVI   S99TUKEY+1,DALRLSE      SET TEXT UNIT KEY
         RCPDINC 4
         AGO   .TROUND
.ROUND   ANOP
***********************************************************************
**      RELEASE UNUSED SPACE TEXT UNIT                               **
***********************************************************************
         MVI   S99TUKEY+1,DALROUND     MOVE IN TEXT UNIT KEY
         RCPDINC 4
         MEND
./ ADD NAME=RCPSPEC
         MACRO - SET UP USER DEFINED TEXT UNIT
         RCPSPEC &T
         LCLA  &I,&J
&I       SETA  1
&J       SETA  K'&T
         SPACE
***********************************************************************
**       PROCESS SPECIAL TEXT UNITS                                  **
***********************************************************************
.LOOP    RCPVCHAR &T(&I),&T(&I+2),&T(&I+3),N=&T(&I+1)
&I       SETA  &I+4
         AIF   (&I LE &J).LOOP
         MEND
./ ADD NAME=RCPSR2
         MACRO
         RCPSR2 &A
         GBLB  &RCPSR2
         GBLC  &DYNP
         LCLC  &C
.*   TO SAVE REG 2 IN REG 0 FOR ALLOC INNER MACROS FIRST TIME ONLY
.*    IF OPERAND SUPPLIED AND SAVE DONE, RESTORES REG 2 AND
.*    GENERATES MOVE INSTRUCTION FOR EXECUTE
         AIF   ('&A' NE '').UNSAVE
         AIF   (&RCPSR2).EXIT
&RCPSR2  SETB  1
         LR    R0,R2                   SAVE CONTENTS OF REGISTER 2
         MEXIT
.UNSAVE  AIF   (NOT &RCPSR2).EXIT
         B     *+10                    SKIP NEXT INSTRUCTION
&C       SETC  '&DYNP.MVC'
&C       MVC   S99TUPAR(0),0(R14)      EXECUTED MOVE
         LR    R2,R0                   RESTORE CONTENTS OF REGISTER 2
&RCPSR2  SETB  0
.EXIT    MEND
./ ADD NAME=RCPSSREQ
         MACRO
         RCPSSREQ
         SPACE 1
***********************************************************************
**       SUBSYSTEM REQUEST TEXT UNIT                                 **
***********************************************************************
         SPACE 1
         MVI   S99TUKEY+1,DALSSREQ MOVE IN TEXT UNIT KEY
         RCPDINC                   4
         MEND
./ ADD NAME=RCPSUBL
         MACRO - BREAK DOWN A SUBLIST
         RCPSUBL &L
         GBLA  &RCPSUB#                NO OF ELEMENTS FOUND
         GBLC  &RCPSUBL(100)           ELEMENTS
         LCLA  &I,&J,&K
&RCPSUB# SETA  0                       INITIALIZE
         AIF   ('&L' EQ '').EXIT       EXIT IF NULL STRING
         AIF   ('&L'(1,1) NE '(').NOSUB
&K       SETA  K'&L-1
&I       SETA  2
&J       SETA  1
.LOOP    ANOP
&J       SETA  &J+1
         AIF   (&J  GT &K).LAST
         AIF   ('&L'(&J,1) NE ',').LOOP
&RCPSUB# SETA &RCPSUB#+1
         AIF   (&J EQ &I).NULL
&RCPSUBL(&RCPSUB#) SETC '&L'(&I,&J-&I)
&I       SETA  &J+1
         AGO   .LOOP
.NULL    ANOP
&RCPSUBL(&RCPSUB#) SETC ''
&I       SETA  &J+1
         AGO   .LOOP
.LAST    AIF   (&J EQ &I).LASTNUL
&RCPSUB# SETA  &RCPSUB#+1
&RCPSUBL(&RCPSUB#) SETC '&L'(&I,&J-&I)
         AGO   .EXIT
.LASTNUL ANOP
&RCPSUB# SETA  &RCPSUB#+1
&RCPSUBL(&RCPSUB#) SETC ''
         AGO   .EXIT
.NOSUB   ANOP
&RCPSUBL(1) SETC '&L'
&RCPSUB# SETA 1
.EXIT    MEND
./ ADD NAME=RCPSYSOU
         MACRO
         RCPSYSOU &CLASS,&COPIES=,&FREE=,&DEST=,&FORMS=,&FCB=,&UCS=
         GBLC  &DYNP
         LCLC  &C
         AIF   ('&CLASS(1)' EQ '').TPGN
&C       SETC  '&CLASS(1)'
         SPACE
***********************************************************************
**       SYSOUT CLASS TEXT UNIT                                      **
***********************************************************************
         AIF   ('&C'(1,1) EQ '''').Q
         AIF   ('&C'(K'&C,1) EQ '/').BS
         AIF   ('&C'(1,1) EQ '(').REG
         L     R14,&C                  LOAD ADDRESS OF SYSOUT CLASS
         MVC   S99TUPAR(1),0(R14)       AND MOVE IT TO TEXT UNIT
         AGO   .SKEY
.REG     MVC   S99TUPAR(1),0&C         MOVE SYSOUT CLASS TO TEXT UNIT
.SKEY    MVI   S99TUKEY+1,DALSYSOU     SET SYSOUT KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         MVI   S99TULNG+1,1            SET LENGTH FIELD
         CLI   S99TUPAR,C'*' DEFAULT ?                           88267
         BNE   *+8             NO                                88267
         MVI   S99TUNUM+1,0  SET FOR MSGCLASS DEFAULT            88267
         RCPDINC 8
         AGO   .TPGN
.BS      RCPTUBFR DALSYSOU,14,&C
         AGO   .TPGN
.Q       RCPBTU DALSYSOU,1,&C
.TPGN    AIF   ('&CLASS(2)' EQ '').TCOP
         SPACE
***********************************************************************
**   SYSOUT PROGRAM NAME TEXT UNIT                                   **
***********************************************************************
&C       SETC  '&CLASS(2)'
         RCPVCHAR DALSPGNM,14,&C
.TCOP    AIF   ('&COPIES' EQ '').TFREE
         SPACE
***********************************************************************
**    SYSOUT COPIES TEXT UNIT                                        **
***********************************************************************
         RCPNTU DALCOPYS,1,&COPIES
.TFREE   AIF   ('&FREE' EQ '').TDEST
         SPACE
***********************************************************************
**     FREE = CLOSE TEXT UNIT                                        **
***********************************************************************
         AIF   ('&FREE' EQ 'CLOSE').CLOSEOK
         MNOTE 4,' **** FREE=&FREE INVALID, FREE=CLOSE USED'
.CLOSEOK MVI   S99TUKEY+1,DALCLOSE     MOVE IN TEXT UNIT KEY
         RCPDINC 4
.TDEST   AIF   ('&DEST' EQ '').TFORMS
         SPACE
***********************************************************************
**       SYSOUT DESTINATION TEXT UNIT                                **
***********************************************************************
         RCPVCHAR DALSUSER,14,&DEST
.TFORMS  AIF   ('&FORMS' EQ '').TFCB
         SPACE
***********************************************************************
**     SYSOUT FORMS NUMBER TEXT UNIT                                 **
***********************************************************************
         RCPVCHAR DALSFMNO,14,&FORMS
.TFCB    AIF   ('&FCB' EQ '').TUCS                               92013
         SPACE
***********************************************************************
**     SYSOUT FCB NAME TEXT UNIT                                     **
***********************************************************************
         RCPVCHAR DALFCBIM,14,&FCB
.TUCS    AIF   ('&UCS' EQ '').EXIT                               92013
         SPACE                                                   92013
***********************************************************************
**     SYSOUT UCS NAME TEXT UNIT                                     **
***********************************************************************
         RCPVCHAR DALUCS,14,&UCS                                 92013
.EXIT    MEND
./ ADD NAME=RCPTERM
         MACRO
         RCPTERM
         SPACE
***********************************************************************
**      TERM  DATASET TEXT UNIT                                      **
***********************************************************************
         MVI   S99TUKEY+1,DALTERM      MOVE IN TERM DS TEXT UNIT KEY
         RCPDINC 4
         MEND
./ ADD NAME=RCPTUBFR
         MACRO  - BUILD TEXT UNIT FROM BUFFER
         RCPTUBFR &KEY,                TEXT UNIT KEY                   X
               &L,                     MAXIMUM LENGTH VALUE            X
               &C,                     TEXT UNIT                       X
               &N=1                    TEXT UNIT NUMBER
         GBLC  &EXECNAM
         LCLC  &C1,&C2
         LCLA  &I,&K
         MVI   S99TUKEY+1,&KEY         SET TEXT UNIT KEY
         AIF   ('&N' EQ '' OR '&N' EQ '1').N1
         LA    R14,&N                  LOAD TEXT UNIT NUMBER
         STH   R14,S99TUNUM             AND STORE INTO TEXT UNIT
         AGO   .ENDN
.N1      MVI   S99TUNUM+1,1            SET TEXT UNIT NUMBER
.ENDN    ANOP
&K       SETA  K'&C
&I       SETA  &K-1
.LOOP1   ANOP
&K       SETA  &K-1
         AIF   (&K LE 0).STD
         AIF   ('&C'(&K,1) NE '/').LOOP1
&C2      SETC  '&C'(&K+1,&I-&K)
&C1      SETC  '&C'(1,&K-1)
         AIF   ('&C1'(1,1) NE '(').TC2
&C1      SETC  '0&C1'
.TC2     AIF   ('&C2' EQ '0000').V2B
         AIF   ('&C2' EQ '00').V1B
         AIF   ('&C2' EQ '0').V0B
         AIF   ('&C2'(1,1) EQ '(').RL
         MVI   S99TULNG+1,&C2          SET LENGTH FIELD
         MVC   S99TUPAR(&C2.),&C1      MOVE IN TEXT UNIT
         RCPDINC &L
         MEXIT
.STD     ANOP
&K       SETA  &L-6
         MVI   S99TULNG+1,&K           SET TEXT UNIT LENGTH
&C1      SETC  '&C'(1,&I)              REMOVE TRAILING SLASH
         MVC   S99TUPAR(&K),&C1        MOVE IN TEXT UNIT
         RCPDINC &L
         MEXIT
.V2B     LH    R14,&C1                 LOAD TEXT UNIT LENGTH
         S     R14,=A(4)               EXCLUDE LENGTH OF HEADER
&C1      SETC  '4+&C1'
         AGO   .MOVE
.V1B     LH    R14,&C1                 LOAD TEXT UNIT LENGTH
&C1      SETC  '2+&C1'
         AGO   .MOVE
.V0B     SLR   R14,R14                 CLEAR FOR IC
         IC    R14,&C1                 INSERT TEXT UNIT LENGTH
&C1      SETC  '1+&C1'
         AGO   .MOVE
.RL      ANOP
&C2      SETC  '&C2'(2,K'&C2-2)
         LR    R14,&C2                 LOAD TEXT UNIT LENGTH
.MOVE    STH   R14,S99TULNG             AND STORE INTO LENGTH FIELD
         BCTR  R14,0                   GET MACHINE LENGTH
         EXECUTE ,MVC,S99TUPAR-S99TUNIT(0,R15),&C1
         EX    R14,&EXECNAM            MOVE IN TEXT UNIT
         RCPDINC &L
         MEND
./ ADD NAME=RCPTU
         MACRO
         RCPTU &TU            TEXT UNIT LIST
         GBLA  &DTUPO         TEXT UNIT POINTER OFFSET
         GBLC  &DYNP          ALLOC SYMBOL PREFIX
         LCLA  &I,&J
         LCLC  &C
         SPACE 1
***********************************************************************
**       ADD SPECIAL TEXT UNITS                                      **
***********************************************************************
&J       SETA  N'&SYSLIST
.LOOP    ANOP
&I       SETA  &I+1
         AIF   (&I GT &J).EXIT
         AIF   ('&TU(&I)'(1,1) EQ '(').R
         LA    R15,&TU(&I)             LOAD TEXT UNIT ADDRESS
         ST    R15,&DYNP.TUP+&DTUPO     AND STORE IT IN POINTER LIST
&DTUPO   SETA  &DTUPO+4
         AGO   .LOOP
.R       ANOP
&C       SETC  '&TU(&I)'(2,K'&TU(&I)-2)
         ST    &C,&DYNP.TUP+&DTUPO     STORE TEXT UNIT ADDR IN PTR LIST
&DTUPO   SETA  &DTUPO+4
         AGO   .LOOP
.EXIT    MEND
./ ADD NAME=RCPTXTL
         MACRO - TO COUNT CHARACTERS IN A STRING
         RCPTXTL &S
         GBLA  &RCPTXTL
         LCLA  &I,&K,&L
&RCPTXTL SETA  0
         AIF   (K'&S LT 3).MEND
&RCPTXTL SETA  K'&S-2
&L       SETA  &RCPTXTL
&I       SETA  1
.LOOP    ANOP
&I       SETA  &I+1
.LOOP2   AIF   (&I GT &L).MEND
         AIF   ('&S'(&I,2) NE '''''' AND '&S'(&I,2) NE '&&').LOOP
&I       SETA  &I+2
&RCPTXTL SETA  &RCPTXTL-1
         AGO   .LOOP2
.MEND    MEND
./ ADD NAME=RCPTYPE
         MACRO
         RCPTYPE &T
         GBLC  &RCPTYPE
         LCLA  &I,&K
&K       SETA  K'&T
&RCPTYPE SETC  ''
         AIF   (&K EQ 0).EXIT
&RCPTYPE SETC  'C'
.LOOP    ANOP
&I       SETA  &I+1
         AIF   ('&T'(&I,1) LT '0' OR '&T'(&I,1) GT '9').EXIT
         AIF   (&I LT &K).LOOP
&RCPTYPE SETC  'N'
.EXIT    MEND
./ ADD NAME=RCPUNALC
         MACRO
         RCPUNALC
         SPACE 1
***********************************************************************
**     FREE EVEN IF PERMANENTLY ALLOCATED                            **
***********************************************************************
         MVI   S99TUKEY+1,DUNUNALC     SET TEXT UNIT KEY
         RCPDINC  4
         MEND
./ ADD NAME=RCPUNITC
         MACRO
         RCPUNITC &CNT
         SPACE
***********************************************************************
**     UNIT ALLOCATION COUNT TEXT UNIT                               **
***********************************************************************
         RCPNTU DALUNCNT,1,&CNT
         MEND
./ ADD NAME=RCPUNIT
         MACRO
         RCPUNIT &U,&V
         GBLC  &DYNP
         AIF   ('&U' EQ '').TVOL
         SPACE 1
***********************************************************************
**       UNIT NAME TEXT UNIT                                         **
***********************************************************************
         RCPVCHAR DALUNIT,14,&U
.TVOL    AIF   ('&V' EQ '').EXIT
         SPACE 1
***********************************************************************
**       VOLUME SERIAL TEXT UNIT                                     **
***********************************************************************
         RCPVCHAR DALVLSER,14,&V
.EXIT    MEND
./ ADD NAME=RCPVCHAR
         MACRO
         RCPVCHAR &KEY,&LEN,&C,&N=1
         GBLC  &DYNP
         AIF   ('&C'(K'&C,1) EQ '/').BM
         AIF   ('&C'(1,1) EQ '''').QM
         RCPSR2
         AIF   ('&C'(1,1) EQ '(').RM
         LH    R2,&C+4                 LOAD LENGTH OF TEXT UNIT
         LTR   R2,R2                   TEST FOR ZERO
         BZ    *+30                    IF NO TEXT UNIT, SKIP
         L     R14,&C                  LOAD ADDRESS OF TEXT UNIT
         AGO   .STHM
.RM      LH    R2,4&C                  LOAD LENGTH OF TEXT UNIT
         LTR   R2,R2                   AND TEST FOR ZERO
         BZ    *+30                    IF NO TEXT UNIT, SKIP
         L     R14,0&C                 LOAD ADDRESS OF TEXT UNIT
.STHM    STH   R2,S99TULNG             STORE LENGTH OF TEXT UNIT
         BCTR  R2,0                    DECREMENT FOR EXECUTE
         EX    R2,&DYNP.MVC            MOVE IN TEXT UNIT
         MVI   S99TUKEY+1,&KEY         MOVE IN TEXT UNIT KEY
         AIF   ('&N' EQ '1' OR '&N' EQ '').N1
         LA    R14,&N                  LOAD TEXT UNIT NUMBER
         STH   R14,S99TUNUM             AND STORE IT IN TEXT UNIT
         AGO   .ENDN
.N1      MVI   S99TUNUM+1,1            SET NUMBER FIELD
.ENDN    RCPDINC &LEN
         MEXIT
.BM      RCPTUBFR &KEY,&LEN,&C
         MEXIT
.QM      RCPBTU &KEY,&N,&C
         MEND
./ ADD NAME=RCPVOLRT
         MACRO
         RCPVOLRT
         SPACE 1
***********************************************************************
**    VOLUME SERIAL RETURN TEXT UNIT                                 **
***********************************************************************
         MVI   S99TUKEY+1,DALRTVOL     SET RETURN VOLUME SERIAL KEY
         MVI   S99TUNUM+1,1            SET NUMBER FIELD
         MVI   S99TULNG+1,6            SET LENGTH FIELD
         MVC   S99TUPAR(6),=CL6' '     INITIALIZE FIELD TO BLANKS
         RCPDINC 14
         MEND
./ ADD NAME=#REGS
         MACRO
         #REGS &GEN=YES
.*
.*
.*                                                            09/84 DBC
.* LAST CHANGE DATE - SEPTEMBER 11, 1984                      09/84 DBC
.*                  - ADDED SUPPORT FOR PL/S STYLE REGISTER   09/84 DBC
.*                    NAMES (@00, @01, ---, @15).             09/84 DBC
.*                  - ATTEMPTS TO MULTIPLY DEFINE THE SAME    09/84 DBC
.*                    NAME TO THE SAME VALUE WILL NOW BE      09/84 DBC
.*                    SUPPRESSED WITHOUT ERROR.               09/84 DBC
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $REGS TO #REGS
.*
.* LAST CHANGE DATE - APRIL 20, 1981
.*                  - SUPPORT FOR THE "GEN={YES×NO}" OPERAND IS ADDED.
.*
.* LAST CHANGE DATE - DECEMBER 5, 1977
.*                  - SINGLE REGISTER EQUATES NOW LINE UP CORRECTLY IN
.*                    THE LISTING.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - APRIL 1, 1975
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THE #REGS MACRO HAS TWO FUNCTIONS. ITS PRIMARY
.* PURPOSE IS TO PROVIDE A SIMPLE MEANS OF DEFINING SETS
.* OF REGISTER NAME EQUATES. ITS SECONDARY PURPOSE IS TO
.* CREATE AN INTERNAL TABLE OF ALL REGISTER NAMES
.* COUPLED WITH THEIR NUMERIC VALUES. THIS TABLE IS THEN
.* MADE AVAILABLE TO CERTAIN OTHER MACROS (E.G. #ENTER
.* AND #EXIT) SO THAT THEY CAN DETERMINE (IF NECESSARY)
.* THE VALUES OF ANY REGISTER NAMES THAT MIGHT BE PASSED
.* TO THEM AS OPERANDS AND SO THAT THEY CAN USE REGISTER
.* NAME EQUATES INSTEAD OF REGISTER NUMBERS IN THE CODE
.* THAT THEY GENERATE. THIS IS SO THAT ALL REFERENCES TO
.* REGISTERS WILL BE INDICATED IN THE ASSEMBLER'S CROSS
.* REFERENCE LISTING.
.*                                                            09/84 DBC
.*   THE #REGS MACRO CAN BE USED ANY NUMBER OF TIMES IN
.* AN ASSEMBLY. EACH TIME THAT IT IS USED, IT CAN BE
.* GIVEN ANY NUMBER OF POSITIONAL OPERANDS. EACH OPERAND      09/84 DBC
.* CAN BE EITHER A SINGLE TERM OR A SUB-LIST OF TWO
.* TERMS.
.*                                                            09/84 DBC
.*   IF AN OPERAND IS A SUB-LIST OF TWO TERMS, THEN THE
.* #REGS MACRO TREATS IT AS A REQUEST TO DEFINE A SINGLE
.* REGISTER NAME AND IT GENERATES A STATEMENT OF THE
.* FORM: " TERM1 EQU TERM2 ". THE FIRST TERM MUST BE ANY
.* VALID NAME NOT PREVIOUSLY DEFINED. THE SECOND TERM
.* MUST BE ANY SELF-DEFINING TERM OR ANY REGISTER NAME
.* THAT HAS BEEN PREVIOUSLY DEFINED BY THIS OR A
.* PREVIOUS #REGS MACRO. IT SHOULD NOT BE AN EXPRESSION,
.* AND IT SHOULD NOT BE ANY NAME NOT PREVIOUSLY DEFINED.
.* THE VALUE OF THE SECOND TERM SHOULD FALL IN THE RANGE
.* OF 0 THROUGH 15. IF THE SECOND TERM FITS THESE
.* REQUIREMENTS, THEN THE REGISTER NAME IS SAVED IN AN
.* INTERNAL TABLE FOR USE BY OTHER MACROS.
.*                                                            09/84 DBC
.*   IF AN OPERAND IS ONLY A SINGLE TERM, THEN THE MACRO
.* TREATS IT AS A REQUEST TO DEFINE A FULL SET OF
.* REGISTER NAME EQUATES WITH THE GIVEN TERM USED AS THE
.* REGISTER NAME PREFIX. AS AN EXAMPLE, ASSUME THAT THE
.* OPERAND IS "GPR". IN THIS CASE, THE #REGS MACRO WILL
.* GENERATE EQUATES DEFINING GPR0, GPR1, ---, GPR15 AND
.* GPRA, GPRB, ---, GPRF (EQUAVALENT TO GPR10, GPR11,
.* ---, GPR15). IN ADDITION, THE GENERATED REGISTER
.* NAMES ARE SAVED IN AN INTERNAL TABLE FOR USE BY OTHER
.* MACROS.
.*                                                            09/84 DBC
.*   A SPECIAL CASE. IF THE SINGLE TERM IS AN "AT SIGN"       09/84 DBC
.* (@), THEN THEN THE GENERATED NAMES WILL BE @00, @01,       09/84 DBC
.* ---, @15. THIS CONFORMS TO PL/S CONVENTIONS.               09/84 DBC
.*                                                            09/84 DBC
.*   IF #REGS IS CALLED WITHOUT OPERANDS, THEN IT IS
.* TREATED AS A REQUEST TO GENERATE A FULL SET OF
.* EQUATES USING "R" AS THE PREFIX.
.*
.*
.*
.* GEN={YES×NO}    (DEFAULT IS GEN=YES)
.*       THIS CONTROLS WHETHER OR NOT THIS MACRO ACTUALLY GENERATES THE
.*       'EQU' STATEMENTS THAT CREATE THE DESIRED REGISTER NAMES. IF
.*       "GEN=NO" IS GIVEN, THEN PRESUMEDLY THE DESIRED NAMES ARE
.*       GENERATED ELSEWHERE. IN THIS CASE THE ONLY FUNCTION PERFORMED
.*       BY THIS MACRO IS TO UPDATE INTERNAL TABLES.
.*
.*
.*
.* INNER MACROS USED - #TEST
.*
         GBLA  &#REGVAL(255)
         GBLA  &#TESERR
         GBLC  &#REGNME(255)
         GBLC  &#TESRET(20)
         LCLA  &ARG,&CTR,&NEXT,&A1
         LCLB  &B1
         LCLC  &LPFX,&C1
&NEXT    SETA  0
.LP1     AIF   (&NEXT GE 255).END1
&NEXT    SETA  &NEXT+1
         AIF   ('&#REGNME(&NEXT)' NE '').LP1
&NEXT    SETA  &NEXT-1
.END1    ANOP
&ARG     SETA  0
.LP2     AIF   (&ARG GE N'&SYSLIST).DONE
&ARG     SETA  &ARG+1
         AIF   (N'&SYSLIST(&ARG) EQ 0).LP2
         AIF   (&NEXT LT 255).NOTFULL
         MNOTE 4,'THE REGISTER NAME SAVE TABLE IS FULL.'
         MNOTE 4,'THE MAXIMUM CAPACITY IS 255 ENTRIES.'
.NOTFULL ANOP
&C1      SETC  '&SYSLIST(&ARG,1)'
         AIF   (N'&SYSLIST(&ARG) GE 2).ONEREG
.NULL    ANOP
&B1      SETB  (1)
         #TEST PFIX=
&LPFX    SETC  '&#TESRET(1)'
.*                                                            09/84 DBC
         AIF   ('&C1' NE '@').NOT@                            09/84 DBC
&CTR     SETA  0-1                                            09/84 DBC
.LP@     AIF   (&CTR EQ 15).END@                              09/84 DBC
&CTR     SETA  &CTR+1                                         09/84 DBC
&C1      SETC  '0&CTR'                                        09/84 DBC
&C1      SETC  '&C1'(K'&C1-1,2)                               09/84 DBC
         #REGS (@&C1,&LPFX&CTR)                               09/84 DBC
         AGO   .LP@                                           09/84 DBC
.END@    AIF   (&NEXT GE 255).LP2                             09/84 DBC
&NEXT    SETA  &NEXT+1                                        09/84 DBC
         AIF   ('&#REGNME(&NEXT)' NE '').END@                 09/84 DBC
&NEXT    SETA  &NEXT-1                                        09/84 DBC
         AGO   .LP2                                           09/84 DBC
.NOT@    ANOP                                                 09/84 DBC
.*                                                            09/84 DBC
&CTR     SETA  0
.LP2A    AIF   (&CTR GE &NEXT).PXSAVE
&CTR     SETA  &CTR+1
         AIF   (&#REGVAL(&CTR) LT 16 OR '&#REGNME(&CTR)' NE '&C1').LP2A
         AGO   .LP2                                           09/84 DBC
.PXSAVE  AIF   (&NEXT GE 255).NOSAVE1                         09/84 DBC
&NEXT    SETA  &NEXT+1
&#REGNME(&NEXT) SETC '&C1'
&#REGVAL(&NEXT) SETA 16
.NOSAVE1 AIF   ('&GEN(1)'(1,1) NE 'Y').LP2
&CTR     SETA  0
.LP3     AIF   (&CTR GT 15).HEX
&C1&CTR  EQU   &LPFX&CTR
&CTR     SETA  &CTR+1
         AGO   .LP3
.HEX     ANOP
&C1.A    EQU   &C1.10
&C1.B    EQU   &C1.11
&C1.C    EQU   &C1.12
&C1.D    EQU   &C1.13
&C1.E    EQU   &C1.14
&C1.F    EQU   &C1.15
         AGO   .LP2
.ONEREG  ANOP
&B1      SETB  (1)
         AIF   (N'&SYSLIST(&ARG) EQ 2).NOXCESS
         MNOTE 4,'"&SYSLIST(&ARG)" CONTAINS EXCESS INFORMATION.'
         MNOTE 4,'THE EXCESS WILL BE IGNORED.'
.NOXCESS #TEST REGS=&SYSLIST(&ARG,2)
         AIF   (&#TESERR EQ 0).REGOK
         MNOTE 4,'THE VALUE OF "&SYSLIST(&ARG,2)" IS NOT DETERMINABLE.'
         AGO   .REGEQU
.REGOK   ANOP                                                 09/84 DBC
&A1      SETA  &#TESRET(1)
&CTR     SETA  0
.LP3A    AIF   (&CTR GE &NEXT).RGSAVE
&CTR     SETA  &CTR+1
         AIF   (&#REGVAL(&CTR) NE &A1 OR '&#REGNME(&CTR)' NE '&C1').LP3*
               A
         AGO   .LP2                                           09/84 DBC
.RGSAVE  AIF   (&NEXT GE 255).REGEQU                          09/84 DBC
&NEXT    SETA  &NEXT+1
&#REGNME(&NEXT) SETC '&C1'
&#REGVAL(&NEXT) SETA &A1
.REGEQU  AIF   ('&GEN(1)'(1,1) NE 'Y').LP2
&C1      EQU   &SYSLIST(&ARG,2)
         AGO   .LP2
.DONE    ANOP
&C1      SETC  'R'
         AIF   (NOT &B1).NULL
         MEND
./ ADD NAME=#RELOAD
         MACRO
&N       #RELOAD &D,&LCLBASE=NONE,&PGMBASE=
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $RELOAD TO #RELOAD.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - AUGUST 23, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING IT
.* MAY BE DIRECTED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
         GBLA  &#TESERR
         GBLC  &#TESRET(20)
         GBLC  &#BS(14)
         LCLA  &A1,&A2
         LCLC  &PBA,&#
&#       SETC  '&SYSNDX.'
         AIF   ('&#BS(14)' EQ '').MEND
&PBA     SETC  '#RL&#.B'
         AIF   (K'&PGMBASE EQ 0).RTY
         AIF   ('&PGMBASE' EQ '&PGMBASE(1)').GHJ
         AIF   ('&#BS(13)' EQ '&PGMBASE(1)').KLM
         #TEST REGS=(&#BS(13),&PGMBASE(1))
         AIF   (&#TESERR NE 0).MNO
         AIF   ('&#TESRET(1)' EQ '&#TESRET(2)').KLM
.MNO     ANOP
&N       LR    &#BS(13),&PGMBASE(1) LOAD 1ST PROGRAM BASE
         AGO   .HK
.KLM     ANOP
&N       DS    0H                  1ST PROGRAM BASE ALREADY LOADED
         AGO   .HK
.GHJ     ANOP
&PBA     SETC  '&PGMBASE'
.RTY     AIF   ('&LCLBASE' NE 'NONE').GJ
&N       BALR  &#BS(13),0          GET TEMPORARY BASE
         L     &#BS(13),&PBA-*(,&#BS(13)) LOAD 1ST PROGRAM BASE
         AGO   .HK
.GJ      ANOP
&N       L     &#BS(13),&PBA       LOAD 1ST PROGRAM BASE
.HK      ANOP
&A1      SETA  13
&A2      SETA  13
.LP3     AIF   (&A1 EQ 1).END3
&A1      SETA  &A1-1
         AIF   ('&#BS(&A1)' EQ '').LP3
         LA    &#BS(&A1),X'FFF'(,&#BS(&A2)) LOAD NEXT BASE
&A2      SETA  &A1
         AGO   .LP3
.END3    #USING
         AIF   (K'&PGMBASE NE 0).MEND
         B     #RL&#.Y             SKIP OVER DATA AREA
#RL&#.B  DC    A(&#BS(14))         PROGRAM BASE ADDRESS
#RL&#.Y  DS    0H                  RECEIVE BRANCH
.MEND    MEND
./ ADD NAME=#SAL
         MACRO
&N       #SAL  &D
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $SAL TO #SAL.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1976
.*
.* THIS MACRO WAS PUT TOGETHER BY DAVID B. COLE. ANY QUESTIONS
.* CONCERNING IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO CAUSES IBM'S STANDARD STAND ALONE LOADER (THE
.* "BOOTSTRAP" LOADER) TO BE PUNCHED. NO OPERANDS ARE ACCEPTED BY THIS
.* MACRO. THE CALL TO THIS MACRO SHOULD BE PLACED PRIOR TO ANY START OR
.* CSECT CARD.
.*
.* THE LOADER PUNCHED BY THIS MACRO IS THE SAME LOADER DISTRIBUTED BY
.* IBM WITH THE IBCDASDI AND IBCDMPRS UTILITIES. THE FOLLOWING
.* INFORMATION IS PERTENANT:
.* - THE LOADER CONSISTS OF SIX CARDS. IT MUST BE PLACED IMMEDIATELY
.*   PRECEEDING THE DECK TO BE LOADED.
.* - THE LOADER WILL LOAD SUCCESSFULLY FROM ANY CARD READER OR TAPE
.*   DRIVE. IF A TAPE DRIVE IS USED, THEN BOTH THE LOADER AND THE DECK
.*   BEING LOADED MUST BE IN THE FORM OF 80-BYTE UNBLOCKED CARD IMAGES.
.* - THE LOADER LOADS SINGLE OBJECT DECKS INTO CORE AT THE ABSOLUTE
.*   ADDRESSES DEFINED IN THE DECK ITSELF.
.* - THE LOADER ITSELF RESIDES BETWEEN X'7C20' AND X'7D9C', SO THE DECK
.*   BEING LOADED MUST NO OVERLAY THESE LOCATIONS.
.* - THE DECK BEING LOADED MUST DEFINE AN INITIAL PSW LOCATED AT
.*   LOCATION X'0'. IT MAY ALSO DEFINE ANY INTERRUPT OLD AND NEW PSW'S
.*   THAT IT REQUIRES EXCEPT FOR THE EXTERNAL INTERRUPT PSW'S SINCE
.*   THESE MIGHT BE OVERLAID IF THE LOADER ENCOUNTERS AN I/O ERROR
.*   DURING ITS PROCESSING.
.* - IF DURING THE LOADING PROCESS AN I/O ERROR DOES OCCURE, THEN THE
.*   LOADER WAITS FOR AN EXTERNAL INTERRUPT UPON WHICH A RE-READ IS
.*   ATTEMPTED.
.* - THE VARIOUS TYPES OF OBJECT CARDS ARE RECOGNIZED BY TESTING ONLY
.*   COLUMN THREE OF EACH CARD.
.* - ESD CARDS ARE IGNORED. NO ATTEMPT IS MADE TO DEAL WITH EXTERNAL
.*   REFERENCES.
.* - RLD CARDS ARE ALSO IGNORED. SINCE ALL RELATIVE ADDRESSES IN THE
.*   OBJECT DECK ARE TREATED AS BEING ABSOLUTE, RLD INFORMATION IS
.*   SIMPLY NOT NEEDED.
.* - THE FOLLOWING TXT FIELDS ARE USED. NEEDLESS TO SAY, THEY CONTAIN
.*   BINARY DATA.
.*   +DISPL(LEN) - USAGE
.*   +2(1)       - MUST BE THE CHARACTER "X" TO IDENTIFY THE CARD
.*   +4(4)       - MUST BE THE ABSOLUTE STARTING ADDRESS FOR THE
.*                 FOLLOWING TEXT DATA
.*   +10(2)      - MUST BE THE LENGTH (IN BYTES) OF THE TEXT DATA
.*   +16(---)    - MUST BE THE TEXT DATA TO BE LOADED
.* - THE END CARD CAUSES THE LOADER TO DO THE FOLLOWING:
.*   - THE IPL DEVICE ADDRESS IS STORED AT LOCATION +2(2).
.*   - A PSW IS LOADED FROM LOCATION +0(8).
.* - IF A CARD IS OTHERWISE UNRECOGNIZABLE, THEN IT IS ASSUMED TO BE A
.*   MANUALLY KEYPUNCHED REP CARD. SUCH CARDS MUST HAVE THE FOLLOWING
.*   FORMAT:
.*   +6(6)       - THE ABSOLUTE ADDRESS (IN HEX) FOR THE START OF THE
.*                 REP DATA
.*   +16(---)    - CONSECUATIVE 4-DIGIT FIELDS (SEPERATED BY COMMAS) OF
.*                 HEX REP DATA.
.*   THE REP DATA MUST BE GIVEN IN UNITS OF 4-DIGIT (2-BYTE) FIELDS.
.*   THE REP ADDRESS NEED NOT, HOWEVER, BE HALFWORD ALIGNED. NO
.*   VALIDITY CHECKING WHATSOEVER IS DONE. INVALID REP DATA WILL RESULT
.*   IN EITHER UNEXPECTED REP'S OR PROGRAM CHECK LOOPS. LEADING ZEROS
.*   MUST BE SUPPLIED. EXAMPLES:
.*    000341    1234,ABCD,EFGH
.*    01ABCC    8888
.*   REP CARDS MAY BE FREELY INTERMIXED INTO THE OBJECT DECK.
.*
.* INNER MACROS USED - NONE
.*
.*
.*
         REPRO

         REPRO

         REPRO
0Åó
         REPRO
† 0ä7}
         REPRO

         REPRO
Õ 1§ûÕ 0Êû
         MEND
./ ADD NAME=#SEARCH
         MACRO
&NME     #SEARCH &DUMMY,&PFIX=,&SVID=,&CMPRID=
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - SEPTEMBER 8, 1981
.*                  - THE PARAMETERIZATION OF THE SEARCH ROUTINE WAS
.*                    CHANGED FROM:
.*                       - R15 = L'LIST
.*                       - R0 = L'LIST ENTRY
.*                       - R1 --> SO-LIST
.*                    TO:
.*                       - R15 --> SO-LIST
.*                         R0 = L'LIST ENTRY
.*                         R1 --> EO-LIST
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $SEARCH TO #SEARCH.
.*
.* LAST CHANGE DATE - APRIL 20, 1981
.*                  - "#REGS GEN=NO" SUPPORT ADDED.
.*
.* LAST CHANGE DATE - MARCH 12, 1980
.*                  - SUPPORT FOR AN EMPTY LIST HAS BEEN FIXED.
.*
.* LAST CHANGE DATE - SEPTEMBER 18, 1978
.*                  - IF THE LIST HAS MULTIPLE ENTRIES WITH THE SAME
.*                    KEY AS THE SEARCH OBJECT, THEN THIS ROUTINE NOW
.*                    RETURNS THE LAST (SEQUENTIALLY) SUCH LIST ENTRY.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - OCTOBER 28, 1975
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES A SUBROUTINE THAT SEARCHES A SORTED LIST.  A
.* BINARY SEARCH METHOD IS USED.
.*
.*   THE SEARCH SUBROUTINE REQUIRES THE FOLLOWING INPUTS:
.*       - R14 POINTS TO THE RETURN ADDRESS;
.*       - R15 POINTS TO THE START OF THE LIST TO BE SEARCHED;
.*       - R0 CONTAINS THE LENGTH (IN BYTES) OF EACH ENTRY IN
.*         THE LIST;
.*       - R1 POINTS PAST THE END OF THE LIST.
.*
.*   ON OUTPUT THE CONDITION CODE AND R1 ARE SET AS FOLLOWS:
.*       - CC=0 (EQUAL) MEANS THAT THE SEARCH OBJECT WAS FOUND. R1
.*         POINTS TO IT. IF THE LIST CONTAINS MORE THAN ONE OBJECT
.*         HAVING THE SAME KEY AS THE SEARCH OBJECT, THEN R1 POINTS TO
.*         THE LAST SUCH OBJECT IN THE LIST.
.*       - CC=2 (HIGH) MEANS THAT THE SEARCH OBJECT WAS NOT FOUND. R1
.*         POINTS TO WHERE IT SHOULD BE INSERTED.
.* ALL OTHER REGISTERS ARE RESTORED.
.*
.*   THE SEARCH SUBROUTINE IS NOT REENTRANT, BUT IT IS SERIALLY
.* REUSABLE.
.*
.*   THE NAME FIELD
.* THIS FIELD IS OPTIONAL. IT CAN BE USED TO DEFINE BOTH THE NAME OF
.* THE SUBROUTINE AND THE PREFIX USED IN ALL GENERATED STATEMENT
.* LABELS. IF OMITTED, THEN THE CHARACTERS "SRCH" WILL BE USED. IF
.* GIVEN, THEN NO MORE THAN FOUR CHARACTERS SHOULD BE SPECIFIED.
.*
.*   THE PFIX= OPERAND
.* THE #SEARCH MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS EXPANSION
.* WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE OF THE REGISTERS
.* WILL BE INDICATED IN THE CROSS REFERENCE LISTING. THE PFIX= OPERAND
.* CAN BE USED TO CONTROL THE SET OF EQUATES USED. FOR EXAMPLE, IF
.* "PFIX=GPR" IS SPECIFIED, THEN "GPR1" WILL BE USED WHENEVER THE
.* EXPANSION REFERS TO REGISTER 1.
.*   IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF EQUATES DEFINED
.* BY THE FIRST PRIOR #REGS MACRO WILL BE USED. IF THERE IS NO PRIOR
.* #REGS MACRO, THEN NO EQUATES WILL BE USED.
.*
.*   THE CMPRID= OPERAND
.* THIS OPERAND IS REQUIRED. IT INDICATES THE ADDRESS OF A SUBROUTINE
.* TO BE USED BY THE SEARCH ROUTINE FOR COMPARING THE SEARCH OBJECT
.* WITH A LIST ENTRY. EITHER A STATEMENT LABEL OR A REGISTER MAY BE
.* SPECIFIED.
.*   THE SPECIFIED COMPARISON ROUTINE IS GIVEN THE FOLLOWING INPUTS:
.*       - R14 POINTS TO THE RETURN ADDRESS.
.*       - R15 POINTS TO THE LIST ENTRY TO BE COMPARED AGAINST THE
.*         SEARCH OBJECT.
.*       - R0 THROUGH R6 CONTAIN DATA ON WHICH THE SEARCH ROUTINE IS
.*         DEPENDANT.
.*       - R7 THROUGH R13 CONTAIN WHAT WAS IN THEM PRIOR TO THE START
.*         OF THE SEARCH ROUTINE.
.* ON OUTPUT THE COMPARISON ROUTINE MUST PROVIDE THE FOLLOWING:
.*       - R14 THROUGH R6 MAY NOT BE ALTERED BY THE COMPARISON
.*         ROUTINE.
.*       - R7 THROUGH R13 (EXCEPT THOSE THAT ARE USED AS
.*         PROGRAM BASES) MAY BE USED FREELY SINCE THE SEARCH ROUTINE
.*         RESTORES ALL REGISTERS WHEN IT IS FINISHED.
.*       - THE CONDITION CODE MUST BE SET AS FOLLOWS:
.*             - CC=0 (EQUAL) MEANS THAT THE SEARCH OBJECT MATCHES THE
.*               CURRENT LIST ENTRY.
.*             - CC=1 (LOW) MEANS THAT THE SEARCH OBJECT IS LOWER THAN
.*               THE CURRENT LIST ENTRY.
.*             - CC=2 (HIGH) MEANS THAT THE SEARCH OBJECT IS HIGHER
.*               THAN THE CURRENT LIST ENTRY.
.*
.*   THE SVID= OPERAND
.* USE THIS OPERAND TO SPECIFY THE NAME OF A 16-WORD REGISTER SAVE
.* AREA. IF OMITTED, THEN THE MACRO WILL GENERATE A LOCAL SAVE AREA.
.*
.* INNER MACROS USED - #REGS #TEST #DIE
.*
.*
.*
         GBLA  &#TESERR
         GBLC  &#TESRET(20)
         LCLC  &N,&R,&SAVEA
&N       SETC  'SRCH'
         AIF   (K'&NME EQ 0).GOTNME
&N       SETC  '&NME'
.GOTNME  ANOP
.*
&R       SETC  '&PFIX'
         AIF   (K'&PFIX NE 0).GOTPFIX
         #TEST PFIX=
&R       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.GOTPFIX #REGS &PFIX,GEN=NO
.DONPFIX ANOP
.*
* BINARY SEARCH ROUTINE
*        INPUTS:
*        - R14 POINTS TO THE RETURN ADDRESS
*        - R15 POINTS TO THE START OF THE LIST TO BE SEARCHED
*        - R0 CONTAINS THE LENGTH OF EACH LIST ENTRY (IN BYTES)
*        - R1 POINTS PAST THE END OF THE LIST
         SPACE 1
* COMPARE ROUTINE INPUTS:
*        - R15 POINTS TO THE LIST ENTRY TO BE COMPARED AGAINST THE
*          SEARCH OBJECT.
*        - R14 POINTS TO THE RETURN ADDRESS.
*        - R14 THROUGH R6 CONTAIN DATA CRITICAL TO THE SEARCH ROUTINE.
*          THEY MUST NOT BE ALTERED BY THE COMPARE ROUTINE.
         SPACE 1
* COMPARE ROUTINE OUTPUTS:
*        - CC=0 (EQUAL) MEANS THAT THE SEARCH OBJECT MATCHES THE
*          CURRENT LIST ENTRY.
*        - CC=1 (LOW) MEANS THAT THE SEARCH OBJECT IS LOWER THAN THE
*          CURRENT LIST ENTRY.
*        - CC=2 (HIGH) MEANS THAT THE SEARCH OBJECT IS HIGHER THAN THE
*          CURRENT LIST ENTRY.
&SAVEA   SETC  '&N.SAVE'
         AIF   (K'&SVID EQ 0).GOTSAVE
&SAVEA   SETC  '&SVID'
.GOTSAVE ANOP
&N       STM   &R.14,&R.13,&SAVEA  SAVE ALL WORK REGISTERS
         AIF   ('&CMPRID' EQ '&CMPRID(1)').NOCMPR1
         #TEST REGS=&CMPRID(1)
         AIF   ( &#TESERR EQ 0 AND '&#TESRET(1)' EQ '2').NOCMPR2
         LR    &R.2,&CMPRID(1)     POINT TO COMPARISON ROUTINE
         AGO   .NOCMPR2
.NOCMPR1 LA    &R.2,&CMPRID        POINT TO COMPARISON ROUTINE
.NOCMPR2 ANOP
         LNR   &R.15,&R.15         GET NEGATIVE SO-LIST
         AR    &R.15,&R.1          GET L'LIST
         SR    &R.1,&R.15          GET SO-LIST
         SR    &R.14,&R.14         CLEAR FOR DIVIDE
         DR    &R.14,&R.0          GET LIST'S ENTRY COUNT
         LTR   &R.4,&R.14          GET INITIAL SEARCH INDEX; WAS THE
*                                  ABOVE DIVIDE EVEN?
         #DIE  NZ,'SEARCH LIST SIZE INCOMPATIBLE WITH LIST ENTRY SIZE'
         LTR   &R.3,&R.15          YES, GET LIST ENTRY COUNT; EMPTY?
         BZ    &N.NULL             YES, GO RETURN "NOT FOUND"
         LA    &R.5,1(,&R.3)       NO, GET INITIAL WIDTH; INSURE ^0
         LA    &R.6,1              GET COMBO OF "1" AND DIRECTION FLAG
         SPACE 1
&N.UP    LCR   &R.6,&R.6           COMPLEMENT FOR TEST
         CR    &R.5,&R.6           WAS PREV WIDTH 1 AND DIRECTION DOWN?
         BE    &N.NFND             YES, SEARCH OBJECT NOT FOUND
&N.UP2   LPR   &R.6,&R.6           NO, SET DIRECTION UP
         AR    &R.5,&R.6           FORCE WIDTH TO ROUND UP UPON DIVIDE
         SRL   &R.5,1              CUT WIDTH IN HALF (ROUND UP)
         AR    &R.4,&R.5           ADVANCE THE SEARCH INDEX
         B     &N.CMPR             PROCEED
         SPACE 1
&N.DOWN  LNR   &R.6,&R.6           SET DIRECTION DOWN
         SR    &R.5,&R.6           FORCE WIDTH TO ROUND UP UPON DIVIDE
         SRL   &R.5,1              CUT WIDTH IN HALF (ROUND UP)
         SR    &R.4,&R.5           RETREAT THE SEARCH INDEX
         SPACE 1
&N.CMPR  BM    &N.UP               SCAN UP IF BELOW THE LIST
         CR    &R.4,&R.3           ABOVE THE LIST?
         BNL   &N.DOWN             YES, MUST SCAN DOWN
         SPACE 1
         LR    &R.15,&R.4          GET CURRENT POSITION INDEX
         MR    &R.14,&R.0          CNVRT TO LIST DISPLACEMENT
         AR    &R.15,&R.1          CNVRT TO ABSOLUTE ENTRY POINTER
         BALR  &R.14,&R.2          LINK TO COMPARISON ROUTINE
         BL    &N.DOWN             TOO HIGH; MUST MOVE DOWN
         BH    &N.UP               TOO LOW; MUST MOVE UP
         SPACE 1
         LCR   &R.6,&R.6           A HIT; COMPLEMENT FOR TEST
         CR    &R.5,&R.6           WAS PREVIOUS WIDTH 1 AND DOWN?
         BE    &N.FND              YES, FOUND LAST OF DUPLICATE KEYS
         LA    &R.5,4              NO, SET TO FWD SCAN PAST DUP KEYS
         B     &N.UP2              LOOP TO SKIP PAST DUP KEYS
         SPACE 1
&N.NFND  LA    &R.15,1(,&R.4)      NOT FOUND; GET INDEX TO INSERT POINT
         MR    &R.14,&R.0          CNVRT TO DISPLACEMENT
&N.NULL  AR    &R.15,&R.1          CNVRT TO ABSOLUTE
         CLI   *,0                 SET CC^=0
&N.FND   ST    &R.15,&SAVEA+12     PRESERVE ENTRY PTR IN R1 SLOT
         LM    &R.14,&R.13,&SAVEA  RESTORE REGISTERS
         BR    &R.14               RETURN TO CALLER WITH CC SET
         AIF   (K'&SVID NE 0).MEND
         SPACE 1
&N.SAVE  DC    16A(0)              REGISTER SAVE AREA
.MEND    MEND
./ ADD NAME=SFR
         MACRO
&NME     SFR   &MF=D
.*
.*     MODIFICATION HISTORY
.*
.*     07/86 MDL ADDED EQUATES TO SUPPORT NEW WEEKDAY FEATURES:
.*               SFRWKXCT - WEEKDAY IS CURRENT DAY
.*               SFRWKNOT - WEEKDAY IS NOT CURRENT DAY
.*
         LCLC  &D
         AIF   ('&MF(1)' EQ 'L').MFL
         AIF   ('&MF(1)' EQ 'D').MFD
         MNOTE 8,'MF(1)=&MF(1) IS INVALID. MF(1)=D IS ASSUMED.'
.MFD     ANOP
&D       SETC  'DSECT'
         AIF   ('&MF(2)' EQ '').MFDOK3
         AIF   ('&MF(2)'(1,1) EQ 'Y').MFDOK3
&D       SETC  'DS'
         AIF   ('&MF(2)'(1,1) EQ 'N').MFDOK3
         MNOTE 8,'MF=&MF IS INVALID. MF=(&MF(1),N) IS ASSUMED.'
.MFDOK3  ANOP
*************************************************************
*                                                           *
*        SFR -- SCHEDULE FILE RECORD                        *
*                                                           * 02/84 DBC
*        ALL TIMESTAMPS IN THE SCHEDULE FILE ARE LOCAL      * 02/84 DBC
*        TIME.                                              * 02/84 DBC
*                                                           *
*        A NEW SCHEDULE FILE CAN BE ALLOCATED VIA AN AMS    *
*        COMMAND SUCH AS THE FOLLOWING:                     *
*                                                           *
*        DEF CL(NAME('SYSVSAM.SCHEDULE') VOL(SYSRES) -      *
*            ATT(3) CODE(SCHEDULE) ERAS KEYS(12 0) -        *
*            OWNER(DAVECOLE) TO(99365) UNQ MRPW(XYZZY) -    *
*            RDPW(SCHEDULE)) -                              *
*                                                           *
*            DATA(NAME('SYSVSAM.SCHEDULE.DATA')-            *
*            RECORDS(1000 100) RECSZ(X'48' X'12D')) -       *
*                                                           *
*            INDEX(NAME('SYSVSAM.SCHEDULE.INDEX'))          *
*                                                           *
*************************************************************
         SPACE 1
         AIF   ('&NME' EQ '').MFDOK1
&NME     &D    0H
SFR      DS    0H                  MAP NAME
         AGO   .MFDOK2
.MFDOK1  ANOP
SFR      &D    0H
.MFDOK2  ANOP
SFRYR    DS    H                   SCHEDULED YEAR
SFRMO    DS    H                   SCHEDULED MONTH
SFRDY    DS    H                   SCHEDULED DAY
SFRYMD   EQU   SFRYR,*-SFRYR       DATE SEGMENT
SFRHR    DS    H                   SCHEDULED HOUR
SFRMN    DS    H                   SCHEDULED MINUTE
SFRHM    EQU   SFRHR,*-SFRHR       TIME-OF-DAY SEGMENT
SFRTIME  EQU   SFRYR,*-SFRYR       SCHEDULED TIME
SFRID    DS    H                   UNIQUE IDENTIFICATION NUMBER
SFRKEY   EQU   SFRYR,*-SFRYR       RECORD'S KEY FIELD
SFRNAME  DS    CL8                 APPLICATION NAME
         SPACE 1
SFRWK    DS    H                   SCHEDULED DAY OF THE WEEK
SFRWKNOT EQU   B'10000000'            EXACT WEEKDAY MATCHING    7/86MDL
SFRWKXCT EQU   B'01000000'            EXACT WEEKDAY MATCHING    7/86MDL
         SPACE 1
SFRXYR   DS    H                   YEAR OF LAST EXECUTION
SFRXMO   DS    H                   MONTH OF LAST EXECUTION
SFRXDY   DS    H                   DAY OF LAST EXECUTION
SFRXYMD  EQU   SFRXYR,*-SFRXYR     DATE SEGMENT
SFRXHR   DS    H                   HOUR OF LAST EXECUTION
SFRXMN   DS    H                   MINUTE OF LAST EXECUTION
SFRXHM   EQU   SFRXHR,*-SFRXHR     TIME-OF-DAY SEGMENT
SFRXTIME EQU   SFRXYR,*-SFRXYR     LAST EXECUTION TIME
         SPACE 1
SFRWHR   DS    H                   LENGTH OF EXECUTION WINDOW: HOURS
SFRWMN   DS    H                   LENGTH OF EXECUTION WINDOW: MINUTES
         SPACE 1
SFRSYSID DS    CL4                 SMF-SYSID OF SYSTEM ON WHICH THIS
*                                  COMMAND MAY BE EXECUTED. X'0000'
*                                  IMPLIES "ANY" SYSTEM.
         SPACE 1
SFRFLAG  DS    B                   FLAG BYTE
SFRFIPLF EQU   B'10000000'         FORCE REEXECUTION OF THIS COMMAND
*                                  IF AN IPL OCCURS WITHIN THE CURRENT
*                                  WINDOW EVEN IF THIS COMMAND HAS
*                                  ALREADY BEEN EXECUTED IN THIS SAME
*                                  WINDOW.
SFRFOVRD EQU   B'01000000'         THIS COMMAND OVERRIDES ALL OTHER
*                                  COMMANDS FOR THIS APPLICATION FOR
*                                  THIS DATE FOR WHICH THIS FLAG IS
*                                  OFF.
SFRFOBSO EQU   B'00100000'         THIS COMMAND IS OBSOLETE. (SET BY
*                                  SCHEDRUN. CHECKED BY SCHEDULE).
         SPACE 1
SFRCMDL  DS    2H                  L'COMMAND TEXT, ZERO
SFRCMD   DS    CL255               COMMAND TEXT
         SPACE 1
SFREND   EQU   *                   MAX END OF SFR
SFRLEN   EQU   SFREND-SFR          MAX L'SFR
         SPACE 3
*************************************************************
*        SCHEDULE FILE'S MASTER RECORD                      *
*************************************************************
         SPACE 1
         ORG   SFR                 LOCATE TO START OF BUFFER
SFMR     DS    0H
SFMRKEY  DS    XL(L'SFRKEY)      × KEY (ALL HEX-FF)
SFMRFID  DC    CL8'SCHEDULE'     V FILE ID
SFMRNXID DS    H                   NEXT DATA RECORD ID VALUE
SFMREND  EQU   *                   EO-SFMR
SFMRLEN  EQU   SFMREND-SFMR        L'SFMR
         ORG   SFREND              RELOCATE HIGH
         MEXIT
.*
.MFL     AIF   ('&MF(2)' EQ '').MFLD
         AIF   ('&MF(2)'(1,1) EQ 'M').MFLM
         MNOTE 8,'MF=&MF IS INVALID. MF=L IS ASSUMED'
.MFLD    ANOP
&NME     DS    0H
         DC    5H'0'               SCHEDULED TIME
         DC    2H'0'               SFRID, SFRWK
         DC    5H'0'               LAST EXECUTION TIME
         DC    2H'0'               EXECUTION WINDOW
         DC    CL4' '              APPLICATION NAME
         DC    B'00000000'         FLAG BYTE
         DC    2H'0'               SFRCMDL
         DC    CL255' '            SFRCMD
         MEXIT
.*
.MFLM    ANOP
&NME     DS    0H
         DC    (L'SFMRKEY)X'FF'    SFMRKEY
         DC    CL(L'SFMRFID)'SCHEDULE' SFMRFID
         DC    H'0'                SFMRNXID
         MEND
./ ADD NAME=#SLD
         MACRO
&N       #SLD  &R,&A
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM SLD TO #SLD.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - FEBRUARY 3, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING IT
.* MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO SIMULATES A HYPOTHETICAL "SUBTRACT LOGICAL DOUBLE"
.* MACHINE INSTRUCTION. ITS FUNCTION IS SIMILAR TO THE "SL" MACHINE
.* INSTRUCTION EXCEPT THAT IT OPERATES ON 64-BIT NUMBERS. WARNING, THE
.* RESULTING CONDITION CODE IS NOT AN ANALOGOUS EXTENSION FROM THE "SL"
.* INSTRUCTION.
.*
.* INNER MACROS USED - #TEST
.*
.*
.*
         GBLA  &#TESERR
         GBLC  &#TESRET(20)
         LCLA  &A1
         LCLC  &RODD
&RODD    SETC  '&R+1'
         #TEST REGS=&R
         AIF   (&#TESERR NE 0).REGUNKN
&A1      SETA  &#TESRET(1)+1
         AIF   (&A1 LE 15).GOTA1
&A1      SETA  0
.GOTA1   #TEST PFIX=
&RODD    SETC  '&#TESRET(1)&A1'
.REGUNKN ANOP
&N       SL    &RODD,4+&A LOW-ORDER DIFFERENCE; HI-ORDER AFFECTED?
         BC    3,SLD&SYSNDX NO, SKIP
         BCTR  &R,0 YES, ADJUST HI-ORDER
SLD&SYSNDX SL  &R,&A GET HI-ORDER DIFFERENCE
         MEND
./ ADD NAME=#SLDR
         MACRO
&N       #SLDR &R1,&R2
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $SLDR TO #SLDR.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - FEBRUARY 3, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING IT
.* MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO SIMULATES A HYPOTHETICAL "SUBTRACT LOGICAL DOUBLE
.* REGISTER" MACHINE INSTRUCTION. ITS FUNCTION IS SIMILAR TO THE "SLR"
.* MACHINE INSTRUCTION EXCEPT THAT IT OPERATES ON 64-BIT NUMBERS.
.* WARNING, THE RESULTING CONDITION CODE IS NOT AN ANALOGOUS EXTENSION
.* FROM THE "SLR" INSTRUCTION.
.*
.* INNER MACROS USED - #TEST
.*
.*
.*
         GBLA  &#TESERR
         GBLC  &#TESRET(20)
         LCLA  &A1
         LCLC  &R1ODD,&R2ODD
&R1ODD   SETC  '&R1+1'
         #TEST REGS=&R1
         AIF   (&#TESERR NE 0).R1UNKN
&A1      SETA  &#TESRET(1)+1
         AIF   (&A1 LE 15).GOTA1A
&A1      SETA  0
.GOTA1A  #TEST PFIX=
&R1ODD   SETC  '&#TESRET(1)&A1'
.R1UNKN  ANOP
&R2ODD   SETC  '&R2+1'
         #TEST REGS=&R2
         AIF   (&#TESERR NE 0).R2UNKN
&A1      SETA  &#TESRET(1)+1
         AIF   (&A1 LE 15).GOTA1B
&A1      SETA  0
.GOTA1B  #TEST PFIX=
&R2ODD   SETC  '&#TESRET(1)&A1'
.R2UNKN  ANOP
&N       SLR   &R1ODD,&R2ODD LOW-ORDER DIFFERENCE; HI-ORDER AFFECTED?
         BC    3,SLDR&SYSNDX NO, SKIP
         BCTR  &R1,0 YES, ADJUST HI-ORDER
SLDR&SYSNDX SLR &R1,&R2 GET HI-ORDER DIFFERENCE
         MEND
./ ADD NAME=#SORTDB
         MACRO
&NME     #SORTDB &SEQ,&PFIX=,&SVID=,&CMPRID=                    GP11205
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $SORT TO #SORT.
.*
.* LAST CHANGE DATE - APRIL 20, 1981
.*                  - "#REGS GEN=NO" SUPPORT ADDED.
.*
.* LAST CHANGE DATE - SEPTEMBER 12, 1978
.*                  - THE SORT ROUTINE IS NOW REENTRANT.
.*                  - THE LINKAGE FROM A COMPARE ROUTINE IS DIFFERENT
.*                    IF THE COMPARE ROUTINE ALSO PERFORMS THE
.*                    EXCHANGE.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - OCTOBER 28, 1975
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES A SUBROUTINE THAT SORTS A LIST OF ITEMS USING
.* THE INTERNAL SORTING TECHNIQUE KNOWN AS THE "SHELL SORT".
.*
.*   THE SORT SUBROUTINE REQUIRES THE FOLLOWING INPUTS:
.*       - REGISTER 14 POINTS TO THE RETURN ADDRESS;
.*       - REGISTER 15 CONTAINS THE LENGTH (IN BYTES) OF THE LIST TO BE
.*         SORTED;
.*       - REGISTER 0 CONTAINS THE LENGTH (IN BYTES) OF EACH ENTRY IN
.*         THE LIST;
.*       - REGISTER 1 POINTS TO THE START OF THE LIST.
.*
.*   ON OUTPUT THE LIST DESCRIBED BY THE INPUT PARAMETERS IS SORTED IN
.* EITHER ASCENDING OR DESCENDING COALATING SEQUENCE (DEPENDING UPON
.* THE MACRO CALL). ALL REGISTERS ARE RESTORED.
.*
.*   THE SORT SUBROUTINE IS REENTRANT.
.*
.*   THE NAME FIELD
.* THIS FIELD IS OPTIONAL. IT CAN BE USED TO DEFINE BOTH THE NAME OF
.* THE SUBROUTINE AND THE PREFIX USED IN ALL GENERATED STATEMENT
.* LABELS. IF OMITTED, THE THE CHARACTERS "SORT" WILL BE USED. IF
.* GIVEN, THEN NO MORE THAN FOUR CHARACTERS SHOULD BE SPECIFIED.
.*
.*   THE FIRST POSITIONAL OPERAND
.* THIS FIELD IS OPTIONAL. IT CAN BE USED TO SPECIFY WHETHER THE LIST
.* IS TO BE SORTED IN ASCENDING OT DESCENDING ORDER. IF OMITTED, THEN
.* ASCENDING ORDER IS ASSUMED. IF GIVEN, THEN IT SHOULD BE EITHER "A"
.* OR "D".
.*
.*   THE PFIX= OPERAND
.* THE #SORT MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS EXPANSION
.* WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE OF THE REGISTERS
.* WILL BE INDICATED IN THE CROSS REFERENCE LISTING. THE PFIX= OPERAND
.* CAN BE USED TO CONTROL THE SET OF EQUATES USED. FOR EXAMPLE, IF
.* "PFIX=GPR" IS SPECIFIED, THEN "GPR1" WILL BE USED WHENEVER THE
.* EXPANSION REFERS TO REGISTER 1.
.*   IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF EQUATES DEFINED
.* BY THE FIRST PRIOR #REGS MACRO WILL BE USED. IF THERE IS NO PRIOR
.* #REGS MACRO, THEN NO EQUATES WILL BE USED.
.*
.*   THE CMPRID= OPERAND
.* WHEN THE CMPRID= OPERAND IS OMITTED, THEN FOR SORTING PURPOSES A CLC
.* INSTRUCTION IS USED TO COMPARE THE ENTIRETY OF EACH LIST ENTRY. IF
.* THIS IS NOT ACCEPTABLE, THEN THE CMPRID= OPERAND CAN BE USED TO
.* INDICATE THAT ENTRY COMPARISON IS TO BE DONE VIA SUBROUTINE CALL. IF
.* "CMPRID=0" IS GIVEN, THEN THE PROGRAMMER MUST PRELOAD THE ADDRESS OF
.* THE COMPARE SUBROUTINE INTO THE ADDRESS LABELLED "SORTCMPR". ON THE
.* OTHER HAND, IF CMPRID= IS SET EQUAL TO SOMETHING OTHER THAN 0, THEN
.* THE GIVEN VALUE IS USED EITHER AS THE NAME OF THE SUBROUTINE, OR AS
.* A REGISTER CONTAINING A POINTER TO THE SUBROUTINE.
.*   ON ENTRY TO THE COMPARE ROUTINE, R6 AND R7 POINT TO THE TWO LIST
.* ENTRIES TO BE COMPARED. THE ENTRY POINTED TO BY R6 SEQUENTIALLY
.* PRECEEDS THE ONE POINTED TO BY R7. R14 POINTS TO THE RETURN ADDRESS.
.*   UPON RETURN, THE COMPARE ROUTINE MUST SET THE CONDITION CODE AS
.* FOLLOWS:
.*       - CC=0 (EQUAL) MEANS THAT THE TWO LIST ENTRIES ARE EQUAL IN
.*         VALUE AND SHOULD NOT BE EXCHANGED.
.*       - CC=1 (LOW) MEANS THAT THE FIRST LIST ENTRY HAS A LOWER VALUE
.*         THAN THE SECOND. WHETHER OR NOT THEY ARE EXCHANGED DEPENDS
.*         UPON WHETHER OR NOT THIS IS AN ASCENDING OR DESCENDING ORDER
.*         SORT.
.*       - CC=2 (HIGH) MEANS THAT THE FIRST LIST ENTRY HAS A HIGHER
.*         VALUE THAN THE SECOND. WHETHER OR NOT THEY ARE EXCHANGED
.*         DEPENDS UPON WHETHER OR NOT THIS IS AN ASCENDING OR
.*         DESCENDING ORDER SORT.
.*       - CC=3 (OVERFLOW) MEANDS THAT THE TWO ENTRIES WERE OUT OF
.*         ORDER AND HAVE ALREADY BEEN EXCHANGED BY THE COMPARE
.*         ROUTINE. THIS RETURN SHOULD BE USED IF THE NATURE OF THE
.*         LIST ENTRIES IS SUCH THAT THE EXCHANGE CODE GENERATED BY
.*         THIS MACRO WOULD FAIL.
.*   THE COMPARE ROUTINE MUST NOT ALTER REGISTERS 14 THROUGH 7. ANY OF
.* THE REGISTERS 8 THROUGH 13 (EXCEPTING THOSE THAT ARE USED AS PROGRAM
.* BASES) MAY BE USED FREELY SINCE THE SORT ROUTINE RESTORES ALL
.* REGISTERS WHEN IT IS FINISHED.
.*
.*   THE SVID= OPERAND
.* USE THIS OPERAND TO SPECIFY THE NAME OF A REGISTER SAVE AREA. IF
.* OMITTED, THEN THE MACRO WILL GENERATE A LOCAL SAVE AREA.
.*   IF THE CMPRID= OPERAND IS GIVEN, THEN THE SAVE AREA MUST BE 16
.* WORDS LONG; OTHERWISE, IT NEED BE ONLY 10 WORDS LONG (FOR REGISTERS
.* 14 THROUGH 7).
.*
.*
.*
.* INNER MACROS USED - #REGS #TEST
.*
.*
.*
         GBLA  &#TESERR
         GBLC  &#TESRET(20)
         LCLA  &SAVESZ,&SL
         LCLC  &N,&R,&SAVEA
&N       SETC  'SORT'
         AIF   (K'&NME EQ 0).GOTNME
&N       SETC  '&NME'
.GOTNME  ANOP
.*
&R       SETC  '&PFIX'
         AIF   (K'&PFIX NE 0).GOTPFIX
         #TEST PFIX=
&R       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.GOTPFIX #REGS &PFIX,GEN=NO
.DONPFIX ANOP
.*
         AIF   ('&SEQ' EQ 'D').DCNDCMT
* ASCENDING ORDER SORT ROUTINE
         AGO   .CMT2
.DCNDCMT ANOP
* DESCENDING ORDER SORT ROUTINE
.CMT2    ANOP
&SAVESZ  SETA  10
&SL      SETA  7
         AIF   (K'&CMPRID EQ 0).NOCMPR1
&SAVESZ  SETA  16
&SL      SETA  13
* WITH A CALLER PROVIDED SUBROUTINE FOR ENTRY COMPARISON
.NOCMPR1 SPACE 1
* SORT ROUTINE INPUTS:
*        - R14 POINTS TO THE RETURN ADDRESS
*        - R15 CONTAINS THE LENGTH OF THE LIST (IN BYTES)
*        - R0 CONTAINS THE LENGTH OF EACH LIST ENTRY (IN BYTES)
*        - R1 POINTS TO THE START OF THE LIST
         AIF   (K'&CMPRID EQ 0).NOCMPR2
         SPACE 1
* COMPARE ROUTINE INPUTS:
*        - R6 AND R7 POINT TO THE TWO LIST ENTRIES TO BE COMPARED. THE
*          ENTRY POINTED TO BY R6 SEQUENTIALLY PRECEEDS THE ONE POINTED
*          TO BY R7.
*        - R14 POINTS TO A RETURN ADDRESS.
*        - R14 THROUGH R7 CONTAIN DATA CRITICAL TO THE SORT ROUTINE.
*          THEY MUST NOT BE ALTERED BY THE COMPARE ROUTINE.
         SPACE 1
* COMPARE ROUTINE OUTPUTS:
*        - CC=0 (EQUAL) MEANS THAT THE TWO LIST ENTRIES HAVE EQUAL
*          VALUES. THEY SHOULD NOT BE EXCHANGED.
*        - CC=1 (LOW) MEANS THAT THE FIRST ENTRY HAS A VALUE LOWER THAN
*          THE SECOND. IF THIS IS AN ASCENDING SORT, THEN THEY WILL NOT
*          BE EXCHANGED.
*        - CC=2 (HIGH) MEANS THAT THE FIRST ENTRY HAS A VALUE HIGHER
*          THAN THE SECOND. IF THIS IS AN ASCENDING SORT, THEN THEY
*          WILL BE EXCHANGED.
*        - CC=3 (OVERFLOW) MEANS THAT THE TWO ENTRIES WERE OUT OF ORDER
*          AND THAT THEY HAVE NOW BEEN EXCHANGED BY THE COMPARE
*          ROUTINE.
.NOCMPR2 SPACE 1
&SAVEA   SETC  '&N.SAVE'
         AIF   (K'&SVID EQ 0).GOTSAVE
&SAVEA   SETC  '&SVID'
.GOTSAVE ANOP
&N       STM   &R.14,&R.&SL,&SAVEA SAVE ALL WORK REGSITERS
         AIF   ('&CMPRID' EQ '').GOTCMPR
         AIF   ('&CMPRID' NE '0').CMPRN0
         L     &R.15,&N.CMPR       --> CALLER PROVIDED COMPARE ROUTINE
         AGO   .GOTCMPR
.CMPRN0  AIF   ('&CMPRID' NE '&CMPRID(1)').CMPRREG
         LA    &R.15,&CMPRID       --> CALLER PROVIDED COMPARE ROUTINE
         AGO   .GOTCMPR
.CMPRREG #TEST REGS=&CMPRID(1)
         AIF   (&#TESERR NE 0).CMPRROK
         AIF   (&#TESRET(1) GT 1 AND &#TESRET(1) LT 14).CMPRROK
         MNOTE 8,'ERROR - CMPRID= MAY NOT IDENTIFY REGISTER &#TESRET(1)*
               '
.CMPRROK LR    &R.15,&CMPRID(1)    --> CALLER PROVIDED COMPARE ROUTINE
.GOTCMPR LR    &R.4,&R.0           GET THE ENTRY LENGTH
         LR    &R.1,&R.0           GET ENTRY LENGTH FOR 'EX'
         BCTR  &R.1,0              GET THE ENTRY'S MACHINE LENGTH
         LR    &R.2,&R.4           GET ENTRY LENGTH
&N.DUBL  AR    &R.2,&R.2           DOUBLE IT
         C     &R.2,&SAVEA+4       GREATER THAN LIST LENGTH YET?
         BNH   &N.DUBL             NO, GO RE-DOUBLE
         SR    &R.2,&R.4           YES, GOT INITIAL INCREMENT
&N.LP1   SR    &R.2,&R.4           SUBTRACT ONE AND -
         BZ    &N.RET               (ALL DONE; GO RESTORE AND RETURN)
         SRL   &R.2,1               TRUNCATE DEVIDE BY TWO
         L     &R.5,&SAVEA+4       GET THE LENGTH OF THE LIST
         SR    &R.5,&R.2           SUBTRACT OFF THE INCREMENT
         LR    &R.3,&R.4           GET THE ENTRY LENGTH
&N.LP2   LR    &R.0,&R.4           GET THE ENTRY LENGTH AGAIN
&N.LP3   LR    &R.6,&R.3           DEVELOPE -
         A     &R.6,&SAVEA+12       THE FIRST -
         SR    &R.6,&R.0             COMPARE ADDRESS
         LA    &R.7,0(&R.2,&R.6)   DEVELOPE THE SECOND ADDRESS
         AIF   ('&CMPRID' EQ '').NOCMPR6
         BALR  &R.14,&R.15         LINK TO CALLER'S COMPARE ROUTINE
         BO    &N.NLP3             OUT OF ORDER BUT NOW EXCHANGED
         AGO   .NOCMPR7
.NOCMPR6 EX    &R.1,&N.CLC         COMPARE THE TWO ENTRIES
.NOCMPR7 AIF   ('&SEQ' EQ 'D').DESCEND
         BNH   &N.NLP2             RIGHT ORDER (ASCENDING); DON'T XCHNG
         AGO   .ASCEND
.DESCEND BNL   &N.NLP2             RIGHT ORDER (DESCENDING); DON'T XCHG
.ASCEND  EX    &R.1,&N.XC1         WRONG -
         EX    &R.1,&N.XC2          ORDER; -
         EX    &R.1,&N.XC1           EXCHANGE
&N.NLP3  BXLE  &R.0,&R.2,&N.LP3    LOOP TO SHIFT THE BUBBLE
&N.NLP2  BXLE  &R.3,&R.4,&N.LP2    ADVANCE ONE INCREMENT
         B     &N.LP1              LOOP TO DECREASE THE INCREMENT
&N.RET   LM    &R.14,&R.&SL,&SAVEA SORT DONE; RESTORE REGS
         BR    &R.14               RETURN TO CALLER
         SPACE 1
         AIF   ('&CMPRID' NE '').DATA1
&N.CLC   CLC   0(*-*,&R.6),0(&R.7) (EXECUTED)
.DATA1   ANOP
&N.XC1   XC    0(*-*,&R.6),0(&R.7) (EXECUTED)
&N.XC2   XC    0(*-*,&R.7),0(&R.6) (EXECUTED)
         AIF   ('&CMPRID' NE '0' AND '&SVID' NE '').DATA2
         SPACE 1
.DATA2   AIF   ('&CMPRID' NE '0').DATA3
&N.CMPR  DC    A(*-*)              THE POINTER TO THE COMPARE ROUTINE
*                                  MUST BE FILLED IN PRIOR TO THE FIRST
*                                  CALL TO THE SORT ROUTINE.
.DATA3   AIF   ('&SVID' NE '').DATA4
&N.SAVE  DC    &SAVESZ.A(0)        LOCAL SAVE AREA
.DATA4   MEND
./ ADD NAME=#SORT
         MACRO
&NME     #SORT &SEQ,&PFIX=,&SVID=,&CMPRID=
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $SORT TO #SORT.
.*
.* LAST CHANGE DATE - APRIL 20, 1981
.*                  - "#REGS GEN=NO" SUPPORT ADDED.
.*
.* LAST CHANGE DATE - SEPTEMBER 12, 1978
.*                  - THE SORT ROUTINE IS NOW REENTRANT.
.*                  - THE LINKAGE FROM A COMPARE ROUTINE IS DIFFERENT
.*                    IF THE COMPARE ROUTINE ALSO PERFORMS THE
.*                    EXCHANGE.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - OCTOBER 28, 1975
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES A SUBROUTINE THAT SORTS A LIST OF ITEMS USING
.* THE INTERNAL SORTING TECHNIQUE KNOWN AS THE "SHELL SORT".
.*
.*   THE SORT SUBROUTINE REQUIRES THE FOLLOWING INPUTS:
.*       - REGISTER 14 POINTS TO THE RETURN ADDRESS;
.*       - REGISTER 15 CONTAINS THE LENGTH (IN BYTES) OF THE LIST TO BE
.*         SORTED;
.*       - REGISTER 0 CONTAINS THE LENGTH (IN BYTES) OF EACH ENTRY IN
.*         THE LIST;
.*       - REGISTER 1 POINTS TO THE START OF THE LIST.
.*
.*   ON OUTPUT THE LIST DESCRIBED BY THE INPUT PARAMETERS IS SORTED IN
.* EITHER ASCENDING OR DESCENDING COALATING SEQUENCE (DEPENDING UPON
.* THE MACRO CALL). ALL REGISTERS ARE RESTORED.
.*
.*   THE SORT SUBROUTINE IS REENTRANT.
.*
.*   THE NAME FIELD
.* THIS FIELD IS OPTIONAL. IT CAN BE USED TO DEFINE BOTH THE NAME OF
.* THE SUBROUTINE AND THE PREFIX USED IN ALL GENERATED STATEMENT
.* LABELS. IF OMITTED, THE THE CHARACTERS "SORT" WILL BE USED. IF
.* GIVEN, THEN NO MORE THAN FOUR CHARACTERS SHOULD BE SPECIFIED.
.*
.*   THE FIRST POSITIONAL OPERAND
.* THIS FIELD IS OPTIONAL. IT CAN BE USED TO SPECIFY WHETHER THE LIST
.* IS TO BE SORTED IN ASCENDING OT DESCENDING ORDER. IF OMITTED, THEN
.* ASCENDING ORDER IS ASSUMED. IF GIVEN, THEN IT SHOULD BE EITHER "A"
.* OR "D".
.*
.*   THE PFIX= OPERAND
.* THE #SORT MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS EXPANSION
.* WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE OF THE REGISTERS
.* WILL BE INDICATED IN THE CROSS REFERENCE LISTING. THE PFIX= OPERAND
.* CAN BE USED TO CONTROL THE SET OF EQUATES USED. FOR EXAMPLE, IF
.* "PFIX=GPR" IS SPECIFIED, THEN "GPR1" WILL BE USED WHENEVER THE
.* EXPANSION REFERS TO REGISTER 1.
.*   IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF EQUATES DEFINED
.* BY THE FIRST PRIOR #REGS MACRO WILL BE USED. IF THERE IS NO PRIOR
.* #REGS MACRO, THEN NO EQUATES WILL BE USED.
.*
.*   THE CMPRID= OPERAND
.* WHEN THE CMPRID= OPERAND IS OMITTED, THEN FOR SORTING PURPOSES A CLC
.* INSTRUCTION IS USED TO COMPARE THE ENTIRETY OF EACH LIST ENTRY. IF
.* THIS IS NOT ACCEPTABLE, THEN THE CMPRID= OPERAND CAN BE USED TO
.* INDICATE THAT ENTRY COMPARISON IS TO BE DONE VIA SUBROUTINE CALL. IF
.* "CMPRID=0" IS GIVEN, THEN THE PROGRAMMER MUST PRELOAD THE ADDRESS OF
.* THE COMPARE SUBROUTINE INTO THE ADDRESS LABELLED "SORTCMPR". ON THE
.* OTHER HAND, IF CMPRID= IS SET EQUAL TO SOMETHING OTHER THAN 0, THEN
.* THE GIVEN VALUE IS USED EITHER AS THE NAME OF THE SUBROUTINE, OR AS
.* A REGISTER CONTAINING A POINTER TO THE SUBROUTINE.
.*   ON ENTRY TO THE COMPARE ROUTINE, R6 AND R7 POINT TO THE TWO LIST
.* ENTRIES TO BE COMPARED. THE ENTRY POINTED TO BY R6 SEQUENTIALLY
.* PRECEEDS THE ONE POINTED TO BY R7. R14 POINTS TO THE RETURN ADDRESS.
.*   UPON RETURN, THE COMPARE ROUTINE MUST SET THE CONDITION CODE AS
.* FOLLOWS:
.*       - CC=0 (EQUAL) MEANS THAT THE TWO LIST ENTRIES ARE EQUAL IN
.*         VALUE AND SHOULD NOT BE EXCHANGED.
.*       - CC=1 (LOW) MEANS THAT THE FIRST LIST ENTRY HAS A LOWER VALUE
.*         THAN THE SECOND. WHETHER OR NOT THEY ARE EXCHANGED DEPENDS
.*         UPON WHETHER OR NOT THIS IS AN ASCENDING OR DESCENDING ORDER
.*         SORT.
.*       - CC=2 (HIGH) MEANS THAT THE FIRST LIST ENTRY HAS A HIGHER
.*         VALUE THAN THE SECOND. WHETHER OR NOT THEY ARE EXCHANGED
.*         DEPENDS UPON WHETHER OR NOT THIS IS AN ASCENDING OR
.*         DESCENDING ORDER SORT.
.*       - CC=3 (OVERFLOW) MEANDS THAT THE TWO ENTRIES WERE OUT OF
.*         ORDER AND HAVE ALREADY BEEN EXCHANGED BY THE COMPARE
.*         ROUTINE. THIS RETURN SHOULD BE USED IF THE NATURE OF THE
.*         LIST ENTRIES IS SUCH THAT THE EXCHANGE CODE GENERATED BY
.*         THIS MACRO WOULD FAIL.
.*   THE COMPARE ROUTINE MUST NOT ALTER REGISTERS 14 THROUGH 7. ANY OF
.* THE REGISTERS 8 THROUGH 13 (EXCEPTING THOSE THAT ARE USED AS PROGRAM
.* BASES) MAY BE USED FREELY SINCE THE SORT ROUTINE RESTORES ALL
.* REGISTERS WHEN IT IS FINISHED.
.*
.*   THE SVID= OPERAND
.* USE THIS OPERAND TO SPECIFY THE NAME OF A REGISTER SAVE AREA. IF
.* OMITTED, THEN THE MACRO WILL GENERATE A LOCAL SAVE AREA.
.*   IF THE CMPRID= OPERAND IS GIVEN, THEN THE SAVE AREA MUST BE 16
.* WORDS LONG; OTHERWISE, IT NEED BE ONLY 10 WORDS LONG (FOR REGISTERS
.* 14 THROUGH 7).
.*
.*
.*
.* INNER MACROS USED - #REGS #TEST
.*
.*
.*
         GBLA  &#TESERR
         GBLC  &#TESRET(20)
         LCLA  &SAVESZ,&SL
         LCLC  &N,&R,&SAVEA
&N       SETC  'SORT'
         AIF   (K'&NME EQ 0).GOTNME
&N       SETC  '&NME'
.GOTNME  ANOP
.*
&R       SETC  '&PFIX'
         AIF   (K'&PFIX NE 0).GOTPFIX
         #TEST PFIX=
&R       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.GOTPFIX #REGS &PFIX,GEN=NO
.DONPFIX ANOP
.*
         AIF   ('&SEQ' EQ 'D').DCNDCMT
* ASCENDING ORDER SORT ROUTINE
         AGO   .CMT2
.DCNDCMT ANOP
* DESCENDING ORDER SORT ROUTINE
.CMT2    ANOP
&SAVESZ  SETA  10
&SL      SETA  7
         AIF   (K'&CMPRID EQ 0).NOCMPR1
&SAVESZ  SETA  16
&SL      SETA  13
* WITH A CALLER PROVIDED SUBROUTINE FOR ENTRY COMPARISON
.NOCMPR1 SPACE 1
* SORT ROUTINE INPUTS:
*        - R14 POINTS TO THE RETURN ADDRESS
*        - R15 CONTAINS THE LENGTH OF THE LIST (IN BYTES)
*        - R0 CONTAINS THE LENGTH OF EACH LIST ENTRY (IN BYTES)
*        - R1 POINTS TO THE START OF THE LIST
         AIF   (K'&CMPRID EQ 0).NOCMPR2
         SPACE 1
* COMPARE ROUTINE INPUTS:
*        - R6 AND R7 POINT TO THE TWO LIST ENTRIES TO BE COMPARED. THE
*          ENTRY POINTED TO BY R6 SEQUENTIALLY PRECEEDS THE ONE POINTED
*          TO BY R7.
*        - R14 POINTS TO A RETURN ADDRESS.
*        - R14 THROUGH R7 CONTAIN DATA CRITICAL TO THE SORT ROUTINE.
*          THEY MUST NOT BE ALTERED BY THE COMPARE ROUTINE.
         SPACE 1
* COMPARE ROUTINE OUTPUTS:
*        - CC=0 (EQUAL) MEANS THAT THE TWO LIST ENTRIES HAVE EQUAL
*          VALUES. THEY SHOULD NOT BE EXCHANGED.
*        - CC=1 (LOW) MEANS THAT THE FIRST ENTRY HAS A VALUE LOWER THAN
*          THE SECOND. IF THIS IS AN ASCENDING SORT, THEN THEY WILL NOT
*          BE EXCHANGED.
*        - CC=2 (HIGH) MEANS THAT THE FIRST ENTRY HAS A VALUE HIGHER
*          THAN THE SECOND. IF THIS IS AN ASCENDING SORT, THEN THEY
*          WILL BE EXCHANGED.
*        - CC=3 (OVERFLOW) MEANS THAT THE TWO ENTRIES WERE OUT OF ORDER
*          AND THAT THEY HAVE NOW BEEN EXCHANGED BY THE COMPARE
*          ROUTINE.
.NOCMPR2 SPACE 1
&SAVEA   SETC  '&N.SAVE'
         AIF   (K'&SVID EQ 0).GOTSAVE
&SAVEA   SETC  '&SVID'
.GOTSAVE ANOP
&N       STM   &R.14,&R.&SL,&SAVEA SAVE ALL WORK REGSITERS
         AIF   ('&CMPRID' EQ '').GOTCMPR
         AIF   ('&CMPRID' NE '0').CMPRN0
         L     &R.15,&N.CMPR       --> CALLER PROVIDED COMPARE ROUTINE
         AGO   .GOTCMPR
.CMPRN0  AIF   ('&CMPRID' NE '&CMPRID(1)').CMPRREG
         LA    &R.15,&CMPRID       --> CALLER PROVIDED COMPARE ROUTINE
         AGO   .GOTCMPR
.CMPRREG #TEST REGS=&CMPRID(1)
         AIF   (&#TESERR NE 0).CMPRROK
         AIF   (&#TESRET(1) GT 1 AND &#TESRET(1) LT 14).CMPRROK
         MNOTE 8,'ERROR - CMPRID= MAY NOT IDENTIFY REGISTER &$TESRET(1)*
               '
.CMPRROK LR    &R.15,&CMPRID(1)    --> CALLER PROVIDED COMPARE ROUTINE
.GOTCMPR LR    &R.4,&R.0           GET THE ENTRY LENGTH
         LR    &R.1,&R.0           GET ENTRY LENGTH FOR 'EX'
         BCTR  &R.1,0              GET THE ENTRY'S MACHINE LENGTH
         LR    &R.2,&R.4           GET ENTRY LENGTH
&N.DUBL  AR    &R.2,&R.2           DOUBLE IT
         C     &R.2,&SAVEA+4       GREATER THAN LIST LENGTH YET?
         BNH   &N.DUBL             NO, GO RE-DOUBLE
         SR    &R.2,&R.4           YES, GOT INITIAL INCREMENT
&N.LP1   SR    &R.2,&R.4           SUBTRACT ONE AND -
         BZ    &N.RET               (ALL DONE; GO RESTORE AND RETURN)
         SRL   &R.2,1               TRUNCATE DEVIDE BY TWO
         L     &R.5,&SAVEA+4       GET THE LENGTH OF THE LIST
         SR    &R.5,&R.2           SUBTRACT OFF THE INCREMENT
         LR    &R.3,&R.4           GET THE ENTRY LENGTH
&N.LP2   LR    &R.0,&R.4           GET THE ENTRY LENGTH AGAIN
&N.LP3   LR    &R.6,&R.3           DEVELOPE -
         A     &R.6,&SAVEA+12       THE FIRST -
         SR    &R.6,&R.0             COMPARE ADDRESS
         LA    &R.7,0(&R.2,&R.6)   DEVELOPE THE SECOND ADDRESS
         AIF   ('&CMPRID' EQ '').NOCMPR6
         BALR  &R.14,&R.15         LINK TO CALLER'S COMPARE ROUTINE
         BO    &N.NLP3             OUT OF ORDER BUT NOW EXCHANGED
         AGO   .NOCMPR7
.NOCMPR6 EX    &R.1,&N.CLC         COMPARE THE TWO ENTRIES
.NOCMPR7 AIF   ('&SEQ' EQ 'D').DESCEND
         BNH   &N.NLP2             RIGHT ORDER (ASCENDING); DON'T XCHNG
         AGO   .ASCEND
.DESCEND BNL   &N.NLP2             RIGHT ORDER (DESCENDING); DON'T XCHG
.ASCEND  EX    &R.1,&N.XC1         WRONG -
         EX    &R.1,&N.XC2          ORDER; -
         EX    &R.1,&N.XC1           EXCHANGE
&N.NLP3  BXLE  &R.0,&R.2,&N.LP3    LOOP TO SHIFT THE BUBBLE
&N.NLP2  BXLE  &R.3,&R.4,&N.LP2    ADVANCE ONE INCREMENT
         B     &N.LP1              LOOP TO DECREASE THE INCREMENT
&N.RET   LM    &R.14,&R.&SL,&SAVEA SORT DONE; RESTORE REGS
         BR    &R.14               RETURN TO CALLER
         SPACE 1
         AIF   ('&CMPRID' NE '').DATA1
&N.CLC   CLC   0(*-*,&R.6),0(&R.7) (EXECUTED)
.DATA1   ANOP
&N.XC1   XC    0(*-*,&R.6),0(&R.7) (EXECUTED)
&N.XC2   XC    0(*-*,&R.7),0(&R.6) (EXECUTED)
         AIF   ('&CMPRID' NE '0' AND '&SVID' NE '').DATA2
         SPACE 1
.DATA2   AIF   ('&CMPRID' NE '0').DATA3
&N.CMPR  DC    A(*-*)              THE POINTER TO THE COMPARE ROUTINE
*                                  MUST BE FILLED IN PRIOR TO THE FIRST
*                                  CALL TO THE SORT ROUTINE.
.DATA3   AIF   ('&SVID' NE '').DATA4
&N.SAVE  DC    &SAVESZ.A(0)        LOCAL SAVE AREA
.DATA4   MEND
./ ADD NAME=SSSHIPE
         MACRO
&NME     SSSHIPE &IID=,&IADR=,&IDA=,&SVC#=,&PRIO=,&MF=D
         LCLC  &LIID,&LIADR,&LIDA,&LSVC#,&LPRIO,&LBL,&R,&D
&LIID    SETC  '*'
         AIF   ('&IID' EQ '').GOTIID
&LIID    SETC  '&IID'
         AIF   ('&IID'(1,1) NE '''').GOTIID
&LIID    SETC  ' '
         AIF   (K'&IID LE 2).GOTIID
&LIID    SETC  '&IID'(2,K'&IID-2)
.GOTIID  ANOP
&LIADR   SETC  '*-*'
         AIF   ('&IADR' EQ '').GOTIADR
&LIADR   SETC  '&IADR'
.GOTIADR ANOP
&LIDA    SETC  '*-*'
         AIF   ('&IDA' EQ '').GOTIDA
&LIDA    SETC  '&IDA'
.GOTIDA  ANOP
&LSVC#   SETC  '*-*'
         AIF   ('&SVC#' EQ '').GOTSVC#
&LSVC#   SETC  '&SVC#'
.GOTSVC# ANOP
&LPRIO   SETC  '128'
         AIF   ('&PRIO' EQ '').GOTPRIO
&LPRIO   SETC  '&PRIO'
.GOTPRIO AIF   ('&MF(1)' EQ 'E').MFE
         AIF   ('&MF(1)' EQ 'L').MFL
         AIF   ('&MF(1)' EQ 'D').MFD
         MNOTE 8,'MF(1)=&MF(1) IS INVALID. MF(1)=D IS ASSUMED.'
.MFD     ANOP
&D       SETC  'DSECT'
         AIF   ('&MF(2)' EQ '').MFDOK3
         AIF   ('&MF(2)'(1,1) EQ 'Y').MFDOK3
&D       SETC  'DS'
         AIF   ('&MF(2)'(1,1) EQ 'N').MFDOK3
         MNOTE 8,'MF(2)=&MF(2) IS INVALID. MF(2)=N IS ASSUMED.'
.MFDOK3  ANOP
*************************************************************
*                                                           *
*        SSSHIPE -- INTERCEPT PARAMETER ENTRY. THIS AREA    *
*        DESCRIBES DATA ASSOCIATED WITH INDIVIDUAL          *
*        SUB-SCREEN INTERCEPT ROUTINES.                     *
*                                                           *
*************************************************************
         SPACE 1
         AIF   ('&NME' EQ ' ').MFDOK1
&NME     &D    0F
SSSHIPE  DS    0F                  MAP NAME
         AGO   .MFDOK2
.MFDOK1  ANOP
SSSHIPE  &D    0F
.MFDOK2  ANOP
SSSHIID  DC    CL8'&LIID'          IDENTIFIER ASSOCIATED WITH THIS
*                                  INTERCEPT.
SSSHIADR DC    A(&LIADR)           ADDRESS OF THE INTERCEPT ROUTINE. A
*                                  ZERO VALUE MEANS THAT THE INTERCEPT
*                                  DESCRIBED BY THIS IPE IS TO BE
*                                  DELETED.
SSSHIDA  DC    A(&LIDA)            VALUE TO BE MADE AVAILABLE TO THE
*                                  INTERCEPT ROUTINE.
SSSHSVC# DC    AL1(&LSVC#)         SVC WHICH THIS ROUTINE INTERCEPTS.
SSSHPRIO DC    AL1(&LPRIO)         PRIORITY AT WHICH THIS INTERCEPT
*                                  ROUTINE IS QUEUED.
SSSHCC   DC    FL1'0'              HIGHEST COMPLETION CODE ASSOCIATED
*                                  WITH THE ATTEMPT TO QUEUE OR DELETE
*                                  THIS INTERCEPT ROUTINE. MUST BE
*                                  PRE-ZEROED BY THE CALLER.
SSSHIEND DS    0F                  END OF THE IPE.
SSSHILEN EQU   SSSHIEND-SSSHIPE    LENGTH OF THE IPE.
         MEXIT
.MFL     ANOP
&NME     DS    0F
         DC    CL8'&LIID'          IDENTIFIER ASSOCIATED WITH THIS
*                                  INTERCEPT
         DC    A(&LIADR)           ADDRESS OF THE INTERCEPT ROUTINE. A
*                                  ZERO VALUE MEANS THAT THE INTERCEPT
*                                  DESCRIBED BY THIS IPE IS TO BE
*                                  DELETED.
         DC    A(&LIDA)            VALUE TO BE MADE AVAILABLE TO THE
*                                  INTERCEPT ROUTINE.
         DC    AL1(&LSVC#)         SVC WHICH THIS ROUTINE INTERCEPTS.
         DC    AL1(&LPRIO)         PRIORITY AT WHICH THIS INTERCEPT
*                                  ROUTINE IS QUEUED.
         DC    FL1'0'              HIGHEST COMPLETION CODE ASSOCIATED
*                                  WITH THE ATTEMPT TO QUEUE OR DELETE
*                                  THIS INTERCEPT ROUTINE. MUST BE
*                                  PRE-ZEROED BY THE CALLER.
         MEXIT
.MFE     ANOP
&LBL     SETC  '&NME'
&R       SETC  '1'
         AIF   ('&MF(2)' NE '').GETMF2
&LBL     LA    &R,SSSHIPE          --> IPE
&LBL     SETC  ''
         AGO   .GOTMF2
.GETMF2  AIF   ('&MF(2)'(1,1) EQ '(').MF2REG
&LBL     LA    &R,&MF(2)           --> IPE
&LBL     SETC  ''
         AGO   .GOTMF2
.MF2REG  ANOP
&R       SETC  '&MF(2)'(2,K'&MF(2)-2)
.GOTMF2  ANOP
&LBL     MVI   SSSHCC-SSSHIPE(&R),0 RESET THE COMPLETION CODE
&LBL     SETC  ''
         AIF   ('&IID' EQ '').NOIID
         AIF   (K'&LIID LE 8).LIIDOK
         MNOTE 4,'IID=&IID IS TOO LONG.'
.LIIDOK  ANOP
         MVC   SSSHIID-SSSHIPE(,&R),=CL8'&LIID' SET NEW IID
.NOIID   AIF   ('&IADR' EQ '').NOIADR
         AIF   ('&IADR' NE '&IADR(1)').IADRREG
         LA    0,&IADR             --> INTERCEPT
         ST    0,SSSHIADR-SSSHIPE(,&R) STORE
         AGO   .NOIADR
.IADRREG ST    &IADR(1),SSSHIADR-SSSHIPE(,&R) STORE DATA AREA ADDRESS
.NOIADR  AIF   ('&IDA' EQ '').NOIDA
         AIF   ('&IDA' NE '&IDA(1)').IDAREG
         LA    0,&IDA              --> DATA AREA
         ST    0,SSSHIDA-SSSHIPE(,&R) STORE
         AGO   .NOIDA
.IDAREG  ST    &IDA(1),SSSHIDA-SSSHIPE(,&R) STORE DATA AREA ADDRESS
.NOIDA   AIF   ('&SVC#' EQ '').NOSVC#
         AIF   ('&SVC#' NE '&SVC#(1)').SVC#REG
         MVI   SSSHSVC#-SSSHIPE(&R),&SVC# SET SVC NUMBER
         AGO   .NOSVC#
.SVC#REG STC   &SVC#(1),SSSHSVC#-SSSHIPE(,&R) STORE SVC NUMBER
.NOSVC#  AIF   ('&PRIO' EQ '').NOPRIO
         AIF   ('&PRIO' NE '&PRIO(1)').PRIOREG
         MVI   SSSHPRIO-SSSHIPE(&R),&PRIO SET QUEUING PRIORITY
         AGO   .NOPRIO
.PRIOREG STC   &PRIO(1),SSSHPRIO-SSSHIPE(,&R) STORE QUEUING PRIORITY
.NOPRIO  ANOP
         MEND
./ ADD NAME=SSSHSPE
         MACRO
&NME     SSSHSPE &SID=,&SDA=,&TCB=,&FLAG=,&SVC=XXX,&MF=D
         LCLA  &A1
         LCLC  &LSID,&LSDA,&LTCB,&LFLAG,&LBL,&R,&D
&LSID    SETC  '*'
         AIF   ('&SID' EQ '').GOTSID
&LSID    SETC  '&SID'
         AIF   ('&SID'(1,1) NE '''').GOTSID
&LSID    SETC  ' '
         AIF   (K'&SID LE 2).GOTSID
&LSID    SETC  '&SID'(2,K'&SID-2)
.GOTSID  ANOP
&LSDA    SETC  '*-*'
         AIF   ('&SDA' EQ '').GOTSDA
&LSDA    SETC  '&SDA'
.GOTSDA  ANOP
&LTCB    SETC  '0'
         AIF   ('&TCB' EQ '').GOTTCB
&LTCB    SETC  '&TCB'
.GOTTCB  ANOP
&LFLAG   SETC  '0'
&A1      SETA  0
.LP1     AIF   (&A1 EQ N'&FLAG).END1
&A1      SETA  &A1+1
&LFLAG   SETC  '&LFLAG+SSSH&FLAG(&A1)'
         AGO   .LP1
.END1    AIF   ('&LFLAG' EQ '0').GOTFLAG
&LFLAG   SETC  '&LFLAG'(3,K'&LFLAG-2)
.GOTFLAG AIF   ('&MF(1)' EQ 'E').MFE
         AIF   ('&MF(1)' EQ 'L').MFL
         AIF   ('&MF(1)' EQ 'D').MFD
         MNOTE 8,'MF(1)=&MF(1) IS INVALID. MF(1)=D IS ASSUMED.'
.MFD     ANOP
&D       SETC  'DSECT'
         AIF   ('&MF(2)' EQ '').MFDOK3
         AIF   ('&MF(2)'(1,1) EQ 'Y').MFDOK3
&D       SETC  'DS'
         AIF   ('&MF(2)'(1,1) EQ 'N').MFDOK3
         MNOTE 8,'MF(2)=&MF(2) IS INVALID. MF(2)=N IS ASSUMED.'
.MFDOK3  ANOP
*************************************************************
*                                                           *
*        SSSHSPE -- SUB-SCREEN PARAMETER ENTRY. THIS AREA   *
*        CONTAINS DATA THAT IS ASSOCIATED WITH AN ENTIRE    *
*        SUB-SCREEN.                                        *
*                                                           *
*************************************************************
         SPACE 1
         AIF   ('&NME' EQ '').MFDOK1
&NME     &D    0F
SSSHSPE  DS    0F                  MAP NAME
         AGO   .MFDOK2
.MFDOK1  ANOP
SSSHSPE  &D    0F
.MFDOK2  ANOP
SSSHSID  DC    CL8'&LSID'          NAME OF THIS SUB-SCREEN
SSSHSDA  DC    A(&LSDA)            VALUE TO BE PROVIDED TO ALL
*                                  INTERCEPT ROUTINES DEFINED UNDER
*                                  THIS SUB-SCREEN.
SSSHTCB  DC    A(&LTCB)            IF SSSHFTCB, THEN THE ADDRESS OF THE
*                                  (FIRST) TCB TO WHICH THIS SUB-SCREEN
*                                  IS TO BE ASSIGNED.
SSSHFLAG DC    AL1(&LFLAG)         FLAG BYTE.
*                                  NOTE, AT LEAST ONE OF SSSHFCRN,
*                                  SSSHFTCB, OR SSSHFALL MUST BE ON.
*                                  REDUNDANT BIT SETTINGS ARE ALLOWED.
SSSHFDEL EQU   B'10000000'         THIS SUB-SCREEN IS TO BE DELETED
*                                  FROM THE INDICATED TCB'S.
SSSHFSUB EQU   B'01000000'         THIS SUB-SCREEN IS TO BE PROPAGATED
*                                  TO OR DELETED FROM ALL SUBTASKS OF
*                                  THE INDICATED TCB'S.
SSSHFCRN EQU   B'00100000'         THIS SUB-SCREEN IS TO BE ASSIGNED TO
*                                  OR DELETED FROM THE CURRENT TCB.
SSSHFTCB EQU   B'00010000'         THIS SUB-SCREEN IS TO BE ASSIGNED TO
*                                  OR DELETED FROM THE TCB POINTED TO
*                                  BY SSSHTCB.
SSSHFALL EQU   B'00001000'         THIS SUB-SCREEN IS TO BE ASSIGNED TO
*                                  OR DELETED FROM ALL TCB'S IN THIS
*                                  ADDRESS SPACE EXCEPT THE FIRST TCB
*                                  (THE REGION CONTROL TASK'S TCB).
SSSHSEND DS    0F                  END OF SSSHSPE.
SSSHSLEN EQU   SSSHSEND-SSSHSPE    LENGTH OF SSSHSPE.
         AIF   ('&SVC' EQ '').NOSVC
         SPACE 3
SSSHSVC  EQU   &SVC                SSSH'S SVC
.NOSVC   MEXIT
.MFL     ANOP
&NME     DS    0F
         DC    CL8'&LSID'          NAME OF THIS SUB-SCREEN
         DC    A(&LSDA)            VALUE TO BE PROVIDED TO ALL
*                                  INTERCEPT ROUTINES DEFINED UNDER
*                                  THIS SUB-SCREEN.
         DC    A(&LTCB)            IF SSSHFTCB, THEN THE ADDRESS OF THE
*                                  (FIRST) TCB TO WHICH THIS SUB-SCREEN
*                                  IS TO BE ASSIGNED.
         DC    AL1(&LFLAG)         FLAG BYTE.
*                                  NOTE, AT LEAST ONE OF SSSHFCRN,
*                                  SSSHFTCB, OR SSSHFALL MUST BE ON.
*                                  REDUNDANT BIT SETTINGS ARE ALLOWED.
         MEXIT
.MFE     ANOP
&LBL     SETC  '&NME'
&R       SETC  '1'
         AIF   ('&MF(2)' NE '').GETMF2
&LBL     LA    &R,SSSHSPE          --> SPE
&LBL     SETC  ''
         AGO   .GOTMF2
.GETMF2  AIF   ('&MF(2)'(1,1) EQ '(').MF2REG
&LBL     LA    &R,&MF(2)           --> SPE
&LBL     SETC  ''
         AGO   .GOTMF2
.MF2REG  ANOP
&R       SETC  '&MF(2)'(2,K'&MF(2)-2)
.GOTMF2  AIF   ('&SID' EQ '').NOSID
         AIF   (K'&LSID LE 8).LSIDOK
         MNOTE 4,'SID=&SID IS TOO LONG.'
.LSIDOK  ANOP
&LBL     MVC   SSSHSID-SSSHSPE(,&R),=CL8'&LSID' SET NEW SID
&LBL     SETC  ''
.NOSID   AIF   ('&SDA' EQ '').NOSDA
         AIF   ('&SDA' NE '&SDA(1)').SDAREG
&LBL     LA    0,&SDA              --> SDA
&LBL     SETC  ''
         ST    0,SSSHSDA-SSSHSPE(,&R) STORE
         AGO   .NOSDA
.SDAREG  ANOP
&LBL     ST    &SDA(1),SSSHSDA-SSSHSPE(,&R) STORE SDA VALUE
&LBL     SETC  ''
.NOSDA   AIF   ('&TCB' EQ '').NOTCB
         AIF   ('&TCB' NE '&TCB(1)').TCBREG
&LBL     LA    0,&TCB              --> OBJECT TCB
&LBL     SETC  ''
         ST    0,SSSHTCB-SSSHSPE(,&R) STORE
         AGO   .NOTCB
.TCBREG  ANOP
&LBL     ST    &TCB(1),SSSHTCB-SSSHSPE(,&R) STORE TCB PTR
&LBL     SETC  ''
.NOTCB   AIF   ('&FLAG' EQ '').NOFLAG
&LBL     MVC   SSSHFLAG-SSSHSPE(&R),&LFLAG SET FLAGS
&LBL     SETC  ''
.NOFLAG  ANOP
         MEND
./ ADD NAME=STRING10
*/GILBERTM JOB (ACCT#),STRING,
*/ NOTIFY=&SYSUID,
*/ CLASS=A,MSGCLASS=X,COND=(0,NE)
*/ASMH EXEC PGM=ASMA90,PARM=(OBJECT,NODECK,NOESD,NORLD,NOXREF,
*/*FLAG(PAGE0),NORXREF,NODXREF,                   HLASM R3
*/ NOBATCH)
***********************************************************************
*                                                                     *
* MACRO NAME = STRING                                                 *
*                                                                     *
* DESCRIPTIVE NAME = STRING Macro Instruction.                        *
*                                                                     *
* FUNCTION = Provide capabilities similar to PUT EDIT (of PL/I)       *
*            or STRING (of COBOL) to assembler programs.              *
*                                                                     *
* STATUS = R510                                                       *
*                                                                     *
* AUTHOR = Gilbert Saint-flour <gsf@pobox.com>                        *
*                                                                     *
* ENVIRONMENT = SEE BELOW                                             *
*                                                                     *
*     AMODE = ANY                                                     *
*     SCP   = OS/360, OS/VS, MVS/370, MVS/XA, MVS/ESA, OS/390 or z/OS *
*     KEY   = ANY                                                     *
*     MODE  = ANY                                                     *
*     APF   = OFF                                                     *
*                                                                     *
* OPERATION = SEE DOCUMENTATION AT THE END OF THIS FILE               *
*                                                                     *
* INVOCATION = SEE DOCUMENTATION AT THE END OF THIS FILE              *
*                                                                     *
* NOTES = SEE DOCUMENTATION AT THE END OF THIS FILE                   *
*                                                                     *
* CHANGE ACTIVITY                                                     *
*                                                                     *
*  $301  FIX BUGS WITH (XYZ,,L8) AND INTO=((R1),(R2))                 *
*  $302  GENERATE $DEBUG BOOT-STRAP IN FINAL_CALL INVOCATION          *
*  $303  REPLACE @STRPAD WITH @STRBLANKS                              *
*  $304  USE @00-@15 INSTEAD OF R0-R15 FOR REGISTER EQUATES           *
*  $306  TAILOR @STRING CSECT TO PROGRAM'S REQUIREMENTS               *
*        LITERALS CAN BE CODED AS 'ABC' OR C'ABC' OR X'C1C2C3'        *
*        USE L'PSATOLD AS IMPLICIT LENGTH FOR (PSATOLD-PSA,,X)        *
*  $307  FINAL_CALL OPTION CHANGED TO GENERATE                        *
*        ADD NOCSECT AND LOCTR OPTIONS TO GENERATE CALL               *
*  $308  USE L'RBCDE AS IMPLICIT LENGTH FOR (PRB.RBCDE,,X)            *
*        PREVENT S0C4 WHEN ADDR IS BAD AND LENGTH IS ZERO             *
*  $400  REORG THE CODE TO SIMPLIFY FEATURE SELECTION                 *
*        BLANKS NO LONGER USED OR GENERATED                           *
*  $401  GENERATE @STRHEXT WHEN ((REG),,X) ONLY HEX FIELD             *
*  $502  @STRING ROUTINE REWRITTEN FOR MVS/ESA:                       *
*        -  USE LINKAGE STACK TO STORE CALLER'S REGISTERS             *
*        -  ADD SUPPORT FOR AR MODE                                   *
*        -  @STRING now executes in caller's AMODE                    *
*        -  Rename previous version to STRINGXA                       *
*  $503  DATE CONVERSION TO YYYY-MM-DD FORMAT (ISO STANDARD)          *
*        IMPROVE SUPPORT FOR AR MODE                                  *
*  $504  SET &STRBLANKS TO 10 WHEN PROCESSING DATES                   *
*  $505  Allow for 128K-offset                                        *
*  $506  Remove literal from BAL instruction for HLASM R3             *
*  $507  Merge STRINGXA code, add AR_MODE option                      *
*        Change syntax of GENERATE call                               *
*  $508  Compatibility with FLAG(PAGE0) in HLASM R3                   *
*        Compatibility with pre-XA version of the SAVE macro          *
*        Remove AMODE-based R2 cleanup in 370 mode                    *
*  $509  Length of parm-list entries can vary between 2 and 6 bytes   *
*        Short Literals (one to five bytes) are generated in parm list*
*        Hex string can contain commas, e.g. X'12,3456,7890'          *
*  $510  JDATE=90366 produces 90/13/01 instead of S0C7                *
***********************************************************************
         MACRO
&NAME    STRING &INTO=,&PRINT=NOGEN
         GBLC  &STRING_MACRO_VERSION
&STRING_MACRO_VERSION SETC '510'       current version
         AIF   ('&PRINT' EQ 'NOGEN').NOGEN
         PUSH  PRINT
         PRINT GEN
.NOGEN   GBLA  &$_LIT
         GBLB  &$_FEAT(16)             FEATURES
.*                                       1 LITERALS
.*                                       2 REGISTER (BIN)
.*                                       3 REGISTER (HEX)
.*                                       4 PACKED
.*                                       5 JDATE
.*                                       6 BINARY
.*                                       7 HEX
.*                                       8 NUMERIC
.*                                       9 LEFT JUST (NUMERIC)
.*                                       10 LEADING ZEROES
.*                                       11 TRUNCATE (CHAR STRING)
.*                                       12 %TIME
         GBLC  &$_LITS(9999)           LITERALS
         LCLA  &I,&J,&N
         AIF   (T'&INTO EQ 'O' AND N'&SYSLIST EQ 1                     X
               AND '&SYSLIST(1,1)' EQ 'GENERATE').GENL
&LABEL   SETC  'IHB&SYSNDX'            STEM FOR LOCAL LABELS
&LQ      SETC  'L'''                   LENGTH ATTRIBUTE
&STR     SETC  '  R&STRING_MACRO_VERSION'
&NAME    BAS   R14,$STRING&STR         CALL @STRING SUB-ROUTINE
         AIF   (N'&SYSLIST EQ 0).ERR1  NO POSITIONAL OPERANDS, ERROR
         AIF   (T'&INTO EQ 'O').ERR2   NO RECEIVING FIELD, ERROR
         AIF   (N'&INTO GT 2).ERR2     INTO=(A,32,BLURB)
         DC    AL2((&LABEL.P-*)/2)     OFFSET TO FIELD DESCRIPTORS
         AIF   (D'$LITERAL).LOCTR2     NOT FIRST TIME, JUMP
$LTORG   LOCTR                         ADDRESSABLE CONSTANTS
$FARRTNE LOCTR                         FAR ROUTINES
.LOCTR2  ANOP
$LITERAL LOCTR                         NON-ADDRESSABLE CONSTANTS
&TO1     SETC  '&INTO(1)'
&TO2     SETC  '&LQ&INTO'
         AIF   (N'&INTO EQ 1).PUNTO8            JUMP IF INTO=XXX
         AIF   ('&INTO(1)'(1,1) NE '(').PUNTO3  JUMP IF INTO=(XXX,44)
&TO1     SETC  '0&INTO(1)'                              INTO=((R3),44)
.PUNTO3  ANOP
&TO2     SETC  '&INTO(2)'                               INTO=(XXX,LL)
         AIF   ('&INTO(2)'(1,1) NE '(').PUNTO8  JUMP IF INTO=(XXX,44)
&TO2     SETC  '0&INTO(2)'                              INTO=(XXX,(R1))
.PUNTO8  ANOP
&LABEL.P DC    S(&TO1,&TO2)
.*--------------------------------------------------------------------*
.*-------      FIELDS       ------------------------------------------*
.*--------------------------------------------------------------------*
         LCLB  &LAST,&BIN,&HEX,&REG,&PACKED,&LEFT,&ZERO,&TRUNC
&I       SETA  1
.*LOOP
.LOOP1   ANOP
         ACTR  200                            SYSDEBUG/DIAG055
         AIF   (N'&SYSLIST(&I) GT 3).FLD990   TOO MANY SUB-OPERANDS
&LAST    SETB  (&I EQ N'&SYSLIST)                LOOP
         AIF   ('&SYSLIST(&I)'(1,1) EQ '''').LIT00
         AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) EQ '''').LIT01
.*--------------------------------------------------------------------*
.*       PROCESS FIRST SUBPARAMETER (ADDRESS)                         *
.*--------------------------------------------------------------------*
&P1S     SETC  '&SYSLIST(&I,1)'
&P2L     SETC  '0'                     INPUT LENGTH
&P3L     SETC  '0'                     OUTPUT LENGTH
         AIF   ('&SYSLIST(&I)'(1,1) GE '0').FLD180 SPACES
         AIF   ('&SYSLIST(&I)' EQ '%TIME').FLD190 %TIME
         AIF   ('&SYSLIST(&I,1)'(1,1) NE '(').FLD115 (R2)
         AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD250
&P1S     SETC  '0&SYSLIST(&I,1)'       CHANGE (R1) TO 0(R1)
.FLD115  ANOP
.*
.*       EXTRACT RBCDE FROM PRB.RBCDE (HLASM)
.*
&L       SETA  1
.*--LOOP
.FLD131  AIF   ('&P1S'(&L,1) EQ '.').FLD133
&L       SETA  &L+1
         AIF   (&L LT K'&P1S).FLD131
.*--ENDLOOP
&P2L     SETC  '&P1S'                    ABCDEF FROM ABCDEF
         AGO   .FLD134
.FLD133  ANOP
&P2L     SETC  '&P1S'(&L+1,K'&P1S-&L)    RBCDE FROM PRB.RBCDE
.FLD134  ANOP
.*
         AIF   (T'&SYSLIST(&I,2) NE 'O').FLD200
.*
         AIF   (NOT D'&P2L).FLD140
&P2C     SETC  T'&P2L
.*MNOTE *,'&P1 &P2C'
         AIF   ('&P2C' EQ 'F' OR '&P2C' EQ 'H' OR '&P2C' EQ 'P').FLD220
         AIF   ('&P2C' EQ 'G').FLD210  FL2
.FLD140  ANOP
.*
.*       EXTRACT PSATOLD FROM PSATOLD-PSA
.*
&L       SETA  1
.*--LOOP
.FLD141  AIF   ('&P2L'(&L,1) EQ '-').FLD143
         AIF   ('&P2L'(&L,1) EQ '+').FLD143
&L       SETA  &L+1
         AIF   (&L LT K'&P2L).FLD141
.*--ENDLOOP
&P2L     SETC  '&LQ&P2L'               L'ABCDEF
         AGO   .FLD300
.FLD143  ANOP
&P2L     SETC  '&LQ'.'&P2L'(1,&L-1)    L'PSATOLD FROM PSATOLD-PSA
         AGO   .FLD300
.*
.FLD180  AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) NE 'X').FLD800
&P2L     SETC  '&SYSLIST(&I)'(1,K'&SYSLIST(&I)-1) 12
         AIF   (&LAST).FLD186
         DC    X'60',AL1(&P2L)         BLANKS
         AGO   .LIT90
.FLD186  DC    X'E0',AL1(&P2L),0S(0)   BLANKS
         AGO   .LIT90
.*
.FLD190  ANOP                          %TIME
&P1S     SETC  '1(14)'                 %TIME
&$_FEAT(12) SETB 1                     %TIME
         AGO   .FLD800
.*--------------------------------------------------------------------*
.*       PROCESS SECOND SUBPARAMETER (LENGTH/TYPE)                    *
.*--------------------------------------------------------------------*
.FLD200  AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD300 NO LENGTH SPECIFIED
&P2C     SETC  '&SYSLIST(&I,2)'
         AGO   .FLD220
.*T'&P1=G
.FLD210  ANOP
&L       SETA  L'&SYSLIST(&I)          T'&P1 = 'G'
&P2C     SETC  'FL&L'                  T'&P1 = 'G'
.*
.FLD220  ANOP
&P2L     SETC  '0&P2C'                 (R2) LENGTH
         AIF   ('&P2C'(1,1) EQ '(').FLD300
&P2L     SETC  '&P2C'                  3(R2) LENGTH
         AIF   ('&P2C'(K'&P2C,1) EQ ')').FLD300
&P2L     SETC  '0'
&PACKED  SETB  ('&P2C' EQ 'P')
         AIF   (&PACKED).FLD300
&P2L     SETC  '1'
         AIF   ('&P2C' EQ 'FL1').FLD240
&P2L     SETC  '3'
         AIF   ('&P2C' EQ 'FL2' OR '&P2C' EQ 'H').FLD240
&P2L     SETC  '7'
         AIF   ('&P2C' EQ 'FL3').FLD240
&P2L     SETC  '15'
         AIF   ('&P2C' EQ 'F').FLD240
&P2L     SETC  '&P2C'                  IMMEDIATE LENGTH, FIELD
         AGO   .FLD300
.*
.FLD240  ANOP                          BINARY VARIABLE
&BIN     SETB  1
         AGO   .FLD300
.*
.FLD250  ANOP                          REGISTER CONTENT
&REG     SETB  1
.*--------------------------------------------------------------------*
.*       PROCESS THIRD SUBPARAMETER (OUTPUT FORMAT)                   *
.*--------------------------------------------------------------------*
.FLD300  AIF   (T'&SYSLIST(&I,3) EQ 'O').FLD800
&HEX     SETB  ('&SYSLIST(&I,3)' EQ 'X') HEXADECIMAL
&TRUNC   SETB  ('&SYSLIST(&I,3)' EQ 'T') TRUNCATE
         AIF   (&HEX OR &TRUNC).FLD800
.*
&P3C     SETC  '&SYSLIST(&I,3)'
&P3L     SETC  '248'
         AIF   ('&P3C' EQ 'YYYY-MM-DD' AND &PACKED).FLD308
&P3L     SETC  '249'
         AIF   ('&P3C' EQ 'YYYYMMDD' AND &PACKED).FLD308
&P3L     SETC  '250'
         AIF   ('&P3C' EQ 'YY/MM/DD' AND &PACKED).FLD308
&P3L     SETC  '251'
         AIF   ('&P3C' EQ 'DD/MM/YY' AND &PACKED).FLD308
&P3L     SETC  '252'
         AIF   ('&P3C' EQ 'MM/DD/YY' AND &PACKED).FLD308
&P3L     SETC  '253'
         AIF   ('&P3C' EQ 'YYMMDD'   AND &PACKED).FLD308
&P3L     SETC  '0'
         AIF   (T'&SYSLIST(&I,2) NE 'N').FLD310
         MNOTE 8,'EDIT PATTERN NOT ALLOWED WITH CHARACTER STRING'
         AGO   .FLD310
.FLD308  ANOP
&$_FEAT(5) SETB 1                      JDATE
         AGO   .FLD800
.*--LOOP
.FLD310  AIF   ('&P3C'(1,1) EQ 'R').FLD318       DEFAULT
         AIF   ('&P3C'(1,1) EQ 'B').FLD318       DEFAULT
         AIF   ('&P3C'(1,1) NE 'L').FLD311
&LEFT    SETB  1
         AGO   .FLD318
.FLD311  AIF   ('&P3C'(1,1) NE 'Z').FLD312
&ZERO    SETB  1
         AGO   .FLD318
.FLD312  AIF   ('&P3C'(1,1) LT '0').FLD993
&P3L     SETC  '&P3L'.'&P3C'(1,1)
.FLD318  ANOP
.*MNOTE *,'&SYSLIST(&I) P3C=/&P3C/ P3L=/&P3L/'
&P3C     SETC  '&P3C'(2,K'&P3C-1)     STRIP OFF FIRST CHARACTER
         AIF   (K'&P3C GT 0).FLD310
.*--ENDLOOP
.*--------------------------------------------------------------------*
.FLD800  ANOP
&NUMERIC SETB  (&BIN OR &PACKED OR (&REG AND NOT &HEX))
&TRUNC   SETB  (&TRUNC OR (&LEFT AND NOT &NUMERIC))
&LEFT    SETB  (&LEFT AND &NUMERIC)
         AIF   (NOT &NUMERIC).FLD810
         AIF   (&LEFT OR '&P3L' NE '0').FLD810
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH ((R3))
         AIF   (&REG).FLD810
&P3L     SETC  '3'                     DEFAULT OUTPUT LENGTH
         AIF   ('&P2C' EQ 'FL1').FLD810
&P3L     SETC  '5'                     DEFAULT OUTPUT LENGTH
         AIF   ('&P2C' EQ 'H' OR '&P2C' EQ 'FL2').FLD810
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH
.FLD810  ANOP
&FLAG    SETA  &HEX*8+&BIN*4+&PACKED*2+&REG*1
&LEN2    SETA  &TRUNC*128+&LEFT*128+&ZERO*64+&P3L
&$_FEAT(2) SETB (&$_FEAT(2) OR (&REG AND NOT &HEX))   REGISTER (BIN)
&$_FEAT(3) SETB (&$_FEAT(3) OR (&REG AND &HEX))       REGISTER (HEX)
&$_FEAT(4) SETB (&$_FEAT(4) OR &PACKED)               PACKED
&$_FEAT(6) SETB (&$_FEAT(6) OR &BIN)                  BINARY
&$_FEAT(7) SETB (&$_FEAT(7) OR (&HEX AND NOT &REG))   HEX
&$_FEAT(8) SETB (&$_FEAT(8) OR &NUMERIC)              BIN,PACKED
&$_FEAT(9) SETB (&$_FEAT(9) OR (&LEFT AND &NUMERIC))
&$_FEAT(10) SETB (&$_FEAT(10) OR &ZERO)
&$_FEAT(11) SETB (&$_FEAT(11) OR &TRUNC)
&BIN     SETB  0                    RESET FLAGS
&HEX     SETB  0                    RESET FLAGS
&REG     SETB  0                    RESET FLAGS
&PACKED  SETB  0                    RESET FLAGS
&LEFT    SETB  0                    RESET FLAGS
&ZERO    SETB  0                    RESET FLAGS
&TRUNC   SETB  0                    RESET FLAGS
         AIF   (&FLAG GE 10).FLD995
         AIF   (&LAST).FLD816
         DC    X'0&FLAG',AL1(&LEN2),SL2(&P1S,&P2L)
         AGO   .LIT99
.FLD816  DC    X'8&FLAG',AL1(&LEN2),SL2(&P1S,&P2L),0S(0)
         AGO   .LIT99
.FLD990  MNOTE 12,'OPERAND &I HAS TOO MANY SUB-OPERANDS'
         AGO   .LIT99
.FLD993  MNOTE 8,'THIRD SUBPARAMETER IS INVALID: ''&SYSLIST(&I,3)'''
         AGO   .LIT99
.FLD995  MNOTE 8,'Invalid Combination of Attributes: &SYSLIST(&I)'
         AGO   .LIT99
.*--------------------------------------------------------------------*
.*------------ LITERALS ----------------------------------------------*
.*--------------------------------------------------------------------*
.LIT00   ANOP
&LIT     SETC  'C&SYSLIST(&I)'
         AGO   .LIT09
.LIT01   ANOP
&LIT     SETC  '&SYSLIST(&I)'
.LIT09   ANOP                              calculate length of literal
&J       SETA  3
&L       SETA  0
         AIF   ('&LIT'(1,1) EQ 'X').LIT11X
.*LOOP
.LIT11C  AIF   ('&LIT'(&J,1) NE '''' AND '&LIT'(&J,1) NE '&&').LIT12C
&J       SETA  &J+1
.LIT12C  ANOP
&J       SETA  &J+1
&L       SETA  &L+1
         AIF   (&J LT K'&LIT).LIT11C
.*ENDLOOP
         AGO   .LIT15
.*LOOP
.LIT11X  AIF   ('&LIT'(&J,1) EQ ',').LIT12X
&L       SETA  &L+1
.LIT12X  ANOP
&J       SETA  &J+1
         AIF   (&J LT K'&LIT).LIT11X
.*ENDLOOP
&L       SETA  (&L+1)/2
.LIT15   ANOP                               generate in-line literal
         AIF   (&L GT 5).LIT40
         AIF   (&LAST).LIT16
         DC    X'4&L',&LIT
         AGO   .LIT90
.LIT16   DC    X'C&L',&LIT,0S(0)
         AGO   .LIT90
.LIT40   ANOP                                  check literal table
         AIF   (&$_LIT EQ 0).LIT50
&N       SETA  1
         ACTR  &$_LIT*3+200
.LIT41   AIF   ('&LIT' EQ '&$_LITS(&N)').LIT80       LOOP
&N       SETA  &N+1                                  LOOP
         AIF   (&N LE &$_LIT).LIT41                  LOOP
.LIT50   ANOP
&$_LIT   SETA  &$_LIT+1
&$_LITS(&$_LIT) SETC '&LIT'
&N       SETA  &$_LIT
.LIT80   ANOP                               generate remote literal
&N       SETA  &N+1000
         AIF   (&LAST).LIT86
         DC    X'40',AL1(&L),AL2($LIT&N-*)
         AGO   .LIT90
.LIT86   DC    X'C0',AL1(&L),AL2($LIT&N-*),0S(0)
.LIT90   ANOP
&$_FEAT(1) SETB 1                      LITERAL
.LIT99   ANOP
.*--------------------------------------------------------------------*
&I       SETA  1+&I                              LOOP
         AIF   (&I LE N'&SYSLIST).LOOP1          LOOP
.*ENDLOOP
&SYSLOC  LOCTR
         AGO   .MEND
.ERR1    MNOTE 12,'AT LEAST ONE INPUT FIELD MUST BE SPECIFIED'
         AGO   .MEND
.ERR2    MNOTE 12,'INVALID OUTPUT AREA SPECIFICATION'
         AGO   .MEND
.**********************************************************************
.*       GENERATE: GENERATE LITERALS AND PROCESSING ROUTINE           *
.**********************************************************************
.GENL    ANOP
         AIF   (N'&SYSLIST(1) GT 3).GENL1R
&OPT2    SETC  '&SYSLIST(1,2)'
         AIF   ('&OPT2' NE '' AND '&OPT2' NE 'AR_MODE'                 X
               AND '&OPT2' NE 'NO_CSECT').GENL1R
&OPT3    SETC  '&SYSLIST(1,3)'
         AIF   ('&OPT3' NE '' AND '&OPT3' NE 'LOCTR').GENL1R
         AIF   ('&OPT2' EQ 'NO_CSECT' AND '&OPT3' EQ 'LOCTR').GENL1R
         AGO   .GENL1D
.GENL1R  MNOTE 12,'Invalid GENERATE options, default used'
&OPT2    SETC  ''
&OPT3    SETC  ''
.GENL1D  ANOP
&STRBLANKS SETA 2
         AIF   (D'$LTORG).GENL1F             CSECT-ONLY
&$_FEAT(1) SETB 1,1,1,1,1,1,1,1,1,1,1,1
.GENL1F  ANOP
&ALLFEAT SETC  '&$_FEAT(1)&$_FEAT(2)&$_FEAT(3)&$_FEAT(4)'
&ALLFEAT SETC  '&ALLFEAT&$_FEAT(5)&$_FEAT(7)&$_FEAT(6)&$_FEAT(8)'
&ALLFEAT SETC  '&ALLFEAT&$_FEAT(9)&$_FEAT(10)&$_FEAT(11)&$_FEAT(12)'
&STR     SETC  '&STRING_MACRO_VERSION'
         MNOTE *,'STRING R&STR - FEATURES GENERATED: &ALLFEAT'
         AIF   (&$_LIT EQ 0).GENL3
$LITERAL LOCTR
.GENL2   ANOP                                LOOP
&N       SETA  &N+1                          LOOP
&I       SETA  &N+1000                       LOOP
$LIT&I   DC    &$_LITS(&N)
         AIF   (&N LT &$_LIT).GENL2          LOOP
.GENL3   ANOP
         AIF   ('&OPT2' EQ 'NO_CSECT').GENL8
&ARMODE  SETB  ('&OPT2' EQ 'AR_MODE')
.**********************************************************************
.*                                                                    *
.*       STRING SUB-ROUTINE                                           *
.*                                                                    *
.*             CAUTION: BYTES 49-72 OF THE CALLER'S SAVE AREA         *
.*                      (R7-R12 SLOTS) ARE USED AS WORK SPACE         *
.*                                                                    *
.**********************************************************************
         AIF   ('&OPT3' EQ 'LOCTR').GENL4L
@STRING  CSECT
@STRING  RMODE ANY
         AGO   .GENL4X
.GENL4L  PUSH  USING                   GENERATE,LOCTR
         DROP
@STRING@ LOCTR
@STRING  DS    0H                      ALIGNMENT
.GENL4X  ANOP
@00      EQU   0                       WORK REGISTER
@01      EQU   1                       WORK REGISTER
@02      EQU   2                       WORK REGISTER
@03      EQU   3                       WORK REGISTER
@04      EQU   4                       WORK REGISTER
@05      EQU   5                       WORK REGISTER
@06      EQU   6                       WORK REGISTER
         AIF   (NOT &ARMODE).ARMODE1X
@07      EQU   7                       WORK REGISTER
@08      EQU   8                       WORK REGISTER
@09      EQU   9                       WORK REGISTER
@10      EQU   10                      WORK REGISTER
@11      EQU   11                      WORK REGISTER
@12      EQU   12                      WORK REGISTER
.ARMODE1X ANOP
@13      EQU   13                      CALLER'S SAVE AREA
@14      EQU   14                      WORK REGISTER
@15      EQU   15                      BASE REG
         USING @STRING,@15
         USING @STRSAVE,@13
         B     @STR003                 BRANCH AROUND EYE-CATCHER
         AIF   (&ARMODE).ARMODE2Y
         DC    C'@STRING/370 R&STRING_MACRO_VERSION &ALLFEAT',0H'0'
@STR003  STM   @14,@06,12(@13)         Save caller's registers
         SLR   @06,@06                 R6=0
&LAE     SETC  'LA'
         AGO   .ARMODE2X
.ARMODE2Y ANOP
         DC    C'@STRING/390 R&STRING_MACRO_VERSION &ALLFEAT',0H'0'
@STR002  DC    H'2'
@STR003  STM   @14,@12,12(@13)         SAVE REGS (DEBUGGING ONLY)
.*+++    BSM   @14,0                   Set AMODE when invoked via BAL
         AH    @14,@STR002-@STRING(@15,0) POINT PAST PARM-LIST OFFSET
         BAKR  @14,0                   SAVE REGS AND RETURN ADDRESS
         BCTR  @14,0                   ADJUST ADDR OF PARM LIST OFFSET
         BCTR  @14,0                   ADJUST ADDR OF PARM LIST OFFSET
         LAE   @06,0                   R6=0 AR6=0
         CPYA  @14,@06                 AR14=0
         CPYA  @15,@06                 AR15=0
&LAE     SETC  'LAE'
.ARMODE2X ANOP
         ICM   @06,B'0011',0(@14)      PICK UP PARM-LIST 1/2 OFFSET
         ALR   @06,@06                 PICK UP PARM-LIST OFFSET
         ALR   @06,@14                 R6 NOW POINTS TO PARM LIST
         USING @STRSCON,@06
         &LAE  @02,@STRSCON            RESOLVE FIRST SCON (ADDR)
         BAL   @14,@STRS2A             GET ADDRESS IN R2
         &LAE  @04,0(,@02)             KEEP ADDRESS OF "INTO" FIELD
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO
         OI    0(@04),0                GET CLEAN S0C4 IF ADDRESS IS BAD
         &LAE  @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)
         BAL   @14,@STRS2A             GET LENGTH IN R2
         LR    @05,@02                 KEEP LENGTH OF "INTO" FIELD
         CR    @05,@04                 END ADDRESS?
         BL    @STR282                 NO, JUMP
         SR    @05,@04                 CALCULATE LENGTH
         AIF   (NOT &ARMODE).ARMODE5N
@STR282  MSTA  @04                     SAVE R4,R5 ON LINKAGE STACK
.*SLAC   DC    X'B247,0040'            SLAC assembler error
         AGO   .ARMODE5X
.ARMODE5N ANOP
@STR282  ST    @04,8(,@13)             SAVE ADDRESS FOR LATER
.ARMODE5X ANOP
.**********************************************************************
.*       MOVE FIELDS TO OUTPUT AREA                                   *
.**********************************************************************
         &LAE  @06,@STRNEXT            POINT TO 1ST FIELD DESC
         USING @STRPARM,@06
.*LOOP
@STR310  EQU   *
         AIF   (NOT &$_FEAT(1)).FEAT1A
         TM    @STRFLAG,@STRLIT        IS THIS A LITERAL?
         BO    @STR372                 YES, JUMP
.FEAT1A  ANOP
         AIF   (NOT &$_FEAT(2) AND NOT &$_FEAT(3)).FEAT23A
         TM    @STRFLAG,@STRREG        REGISTER?
         BO    @STR323                 YES, JUMP
.FEAT23A ANOP
         AIF   (NOT &$_FEAT(12)).FEAT12A
         CLI   @STRSCON,X'E0'          IS IT %TIME ?
         BE    @STR378                 YES, JUMP
.FEAT12A ANOP
.*
.*       IT'S A FIELD (SCON)
.*
         &LAE  @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)
         BAL   @14,@STRS2A             GET LENGTH IN R2
         AIF   (&$_FEAT(9)).FEAT9E1    (LEFT JUSTIFICATION, NUMERIC)
         LTR   @03,@02                 KEEP/TEST LENGTH
         BZ    @STR398                 ZERO LENGTH, DO NOT EDIT
         AGO   .FEAT9E2
.FEAT9E1 LTR   @03,@02                 KEEP/TEST LENGTH
         BP    @STR313                 LENGTH POSITIVE, JUMP
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?
         BNO   @STR398                 NO, ZERO LENGTH IS NOT OK
@STR313  EQU   *
.FEAT9E2 ANOP
         &LAE  @02,@STRSCON            RESOLVE FIRST SCON (ADDR)
         BAL   @14,@STRS2A             GET ADDRESS IN R2
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO
         CLI   0(@02),0                GET CLEAN S0C4 IF ADDRESS IS BAD
         CR    @03,@02                 END ADDRESS?
         BL    @STR314                 NO, JUMP
         LA    @00,X'0080'             PSA ADDRESS
         CLR   @02,@00                 PSA REFERENCE?
         BL    @STR314                 YES, JUMP
         SR    @03,@02                 CALCULATE LENGTH
@STR314  EQU   *
         AIF   (NOT &$_FEAT(6)).FEAT6A
         TM    @STRFLAG,@STRBIN        BINARY FIELD?
         BO    @STR328                 YES, JUMP
.FEAT6A  ANOP
         AIF   (NOT &$_FEAT(4)).FEAT4A
         TM    @STRFLAG,@STRPACK       PACKED FIELD?
         BO    @STR351                 YES, JUMP
.FEAT4A  ANOP
         AIF   (NOT &$_FEAT(7)).FEAT7A
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?
         BO    @STR376                 YES, JUMP
.FEAT7A  ANOP
.*
.*       TRUNCATE CHARACTER STRING
.*
         AIF   (NOT &$_FEAT(11)).FEAT11A
         CLI   @STRLEN2,@STRLEFT       CHECK JUSTIFICATION, OUTPUT LEN
         BNE   @STR390                 NO STRING TRUNCATION, JUMP
         &LAE  @01,0(@03,@02)          FIRST BYTE AFTER FIELD
@STR318  BCTR  @01,0                   DOWN 1 BYTE                 LOOP
         CLI   0(@01),C' '             IS IT A SPACE ?             LOOP
         BNE   @STR390                 LAST NON-BLANK BYTE         LOOP
         BCT   @03,@STR318             LOOP UNTIL 1ST NON-BLANK    LOOP
         B     @STR398                 BLANK FIELD, DO NOT EDIT
         AGO   .FEAT11B
.FEAT11A ANOP
         AIF (&$_FEAT(2)+&$_FEAT(3)+&$_FEAT(4)+&$_FEAT(6) EQ 0).FEAT11B
         B     @STR390                 EDIT
.FEAT11B ANOP
.*
.*       REGISTER (R0-R13)
.*
         AIF   (NOT &$_FEAT(2) AND NOT &$_FEAT(3)).FEAT23B
         AIF   (NOT &ARMODE).ARMODE7N
@STR323  LH    @01,@STRSCON            REG NUMBER IN BITS 28-31
         CLI   @STRSCON+1,@06          IS THIS R7-R13?
         BH    @STR323R                YES, JUMP
         LR    @07,@01                 SAVE R1 (R)
         LAE   @10,0(,@04)             SAVE R4
         LR    @11,@05                 SAVE R5
         LAE   @12,0(,@06)             SAVE R6
         EREG  @00,@06                 RELOAD CALLER'S R0-R6
         EX    @07,@STR323L            COPY R0-R6 VALUE
         LAE   @04,0(,@10)             RESTORE R4
         LR    @05,@11                 RESTORE R5
         LAE   @06,0(,@12)             RESTORE R6
         B     @STR323T
@STR323L LR    @00,*-*                 COPY VALUE FOR EDIT
@STR323R EQU   *
         EREG  @07,@12                 RESTORE CALLER'S R7-R12
         EX    @01,@STR323L            COPY R7-R13 INTO R0
         AGO   .ARMODE7X
.ARMODE7N ANOP
@STR323L LR    @00,*-*                 COPY VALUE FOR EDIT
@STR323  LH    @01,@STRSCON            REG NUMBER IN BITS 28-31
         EX    @01,@STR323L            COPY R7-R13 INTO R0
         CLI   @STRSCON+1,@06+1        IS THIS R7-R13?
         BNL   @STR323T                YES, JUMP
         SLL   @01,2                   R1= 000000BB BASE * 4
         L     @00,20(@01,@13)         PICK UP VALUE FOR R0-R6
.ARMODE7X ANOP
@STR323T EQU   *
         AIF   (NOT &$_FEAT(3)).FEAT3R REG,HEX
         AIF   (NOT &$_FEAT(2)).FEAT2H REG,BIN
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?
         BNO   @STR330                 NO, EDIT FWD
.FEAT2H  ANOP
         STCM  @00,B'1111',@STRDWD-1   STORE IT FOR CVD/UNPK
         LA    @03,8                   OUTPUT LENGTH
         B     @STR376X                EDIT IN HEX
.FEAT3R  ANOP
         AIF   (NOT &$_FEAT(6)).FEAT23B
         B     @STR330                 EDIT R0
.FEAT23B ANOP
.*
.*       BINARY VARIABLE: @03 CONTAINS THE ICM MASK (1 3 7 F)
.*
         AIF   (NOT &$_FEAT(6)).FEAT6B
@STR328M ICM   @00,*-*,0(@02)          **EXECUTED INSTRUCTION**
@STR328  SLR   @00,@00
         EX    @03,@STR328M            LOAD THE BINARY VARIABLE
.FEAT6B  ANOP
.*
         AIF   (NOT &$_FEAT(2) AND NOT &$_FEAT(6)).FEAT6C
@STR330  CVD   @00,@STRDWD             CONVERT VALUE TO DECIMAL
         AIF   (NOT &$_FEAT(4)).FEAT6C
         B     @STR361                 EDIT DWD
.FEAT6C  ANOP
.*
.*       PACKED FIELD
.*
         AIF   (NOT &$_FEAT(4)).FEAT4B
@STRZAP  ZAP   @STRDWD,0(*-*,@02)      MOVE TO @STRDWD
@STR351  &LAE  @03,0(,@02)             FIRST BYTE OF PACKED FIELD
         BALR  @14,0
         TM    0(@03),X'0C'            IS THIS THE SIGN BYTE?
         LA    @03,1(,@03)              (NEXT BYTE)
         BNOR  @14                     NO, LOOP MORE
         SLR   @03,@02                 GET LENGTH OF PACKED FIELD
         BCTR  @03,0
         EX    @03,@STRZAP             EXECUTE ZAP
         AIF   (NOT &$_FEAT(5)).FEAT4B
         CLI   @STRLEN2,248            JULIAN-TO-YYMMDD CONV?
         BNL   @STR375                 YES, JUMP
.FEAT4B  ANOP                          PACKED
.*
.*       EDIT @STRDWD (BIN, REG, PACKED)
.*
         AIF   (NOT &$_FEAT(8)).FEAT8B
@STR361  IC    @00,@STRLEN2            OUTPUT LENGTH
         LA    @03,X'003F'             MASK FOR "AND"
         NR    @03,@00                 OUTPUT LENGTH
         MVC   @STRWK16(16),@STRMASK   EDIT MASK
.*
.*       LEFT-JUSTIFICATION (NUMERIC)
.*
         AIF   (NOT &$_FEAT(9)).FEAT9B
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?
         BNO   @STR367                 NO, JUMP
         &LAE  @01,@STRWK16+15         PREVENT BAD R1
         EDMK  @STRWK16(16),@STRDWD    ZONED DECIMAL
         &LAE  @02,0(,@01)             FIRST STRING POSITION
         LTR   @03,@03                 CHECK OUTPUT LENGTH
         BNZ   @STR363                 JUMP IF NOT ZERO
.*       L0    (LEFT JUSTIFIED, NO PADDING)
         &LAE  @03,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @03,@02                 COMPUTE STRING LENGTH
         B     @STR390                 MOVE FIELD TO OUTPUT LINE
.*       L1-L63 (LEFT JUSTIFIED, PADDING)
@STR363  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN
         BNH   @STR364                 LARGE ENOUGH, JUMP
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.
@STR364  SR    @05,@03                 COMPUTE REMAINING LENGTH
         AIF   (NOT &ARMODE).ARMODE8N
         LAE   @08,0(,@04)             POINTER IN OUTPUT LINE
         LR    @09,@03                 LENGTH WITH PADDING
         AGO   .ARMODE8X
.ARMODE8N ANOP
         LR    @00,@04                 POINTER IN OUTPUT LINE
         LR    @01,@03                 LENGTH WITH PADDING
.ARMODE8X ANOP
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @03,@02                 COMPUTE STRING LENGTH
         B     @STR392                 MOVE FIELD TO OUTPUT LINE
@STR367  EQU   *
.FEAT9B  ANOP
         AIF   (NOT &$_FEAT(10)).FEAT10B
         TM    @STRLEN2,@STRZERO       LEADING ZEROES REQ'D?
         BNO   @STR368                 NO, JUMP
         MVI   @STRWK16,C'0'           YES, CHANGE X'40' TO C'0'
@STR368  EQU   *
.FEAT10B ANOP
         ED    @STRWK16(16),@STRDWD    ZONED DECIMAL
         &LAE  @02,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @02,@03                 FIRST STRING POSITION
.FEAT8B  ANOP
.*
.*       LITERAL (@STRSCON IS A 16-BIT OFFSET)
.*       Short Literal (low-order 4 bits of @STRFLAG contains length)
.*       BLANKS  (@STRSCON=ZERO)
.*
         AIF   (NOT &$_FEAT(1)).FEAT1B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR372  LA    @01,7                   mask for NR
         &LAE  @02,@STRLEN2            1st byte of short literal
         IC    @03,@STRFLAG            pick up flags+length
         NR    @03,@01                 R3 has length of short literal
         BNZ   @STR390                 short literal, go move it
         SLR   @02,@02                 Clear Address Register
         IC    @03,@STRLEN2            GET LITERAL LENGTH
         TM    @STRFLAG,@STRX40        string of spaces?
         BO    @STR390                 yes, go move them
         ICM   @02,B'0011',@STRSCON    LOAD LITERAL OFFSET
         LA    @02,@STRSCON(@02)       CONVERT OFFSET TO FULL ADDRESS
.FEAT1B  ANOP
.*
.*       CONVERT JULIAN DATE TO YYMMDD
.*
         AIF   (NOT &$_FEAT(5)).FEAT5F
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR375  LA    @00,248                 MASK FOR 'SLR'
         SLR   @01,@01
         IC    @01,@STRLEN2            248-255
         SLR   @01,@00                 000-007
         LA    @00,12                  L'@STR375W
         MR    @00,@00                 COMPUTE OFFSET
         &LAE  @01,@STR375W(@01)       ENTRY IN "TR" MASK TABLE
         SLR   @03,@03
         IC    @03,0(,@01)             LENGTH OF DATE (6, 8 OR 10)
         ZAP   @STRDWD,@STRDWD         DATE=0000000?               @JDT
         BNZ   @STR375B                NO, JUMP                    @JDT
@STR375Z &LAE  @02,@STRBLANKS          WORK AREA
&STRBLANKS SETA 10                     WE NEED AT LEAST 10 BLANKS
         B     @STR390                 MOVE FIELD TO OUTPUT LINE
@STR375W DC    AL1(10,C'-',0,1,2,3,8,4,5,8,6,7) YYYY-MM-DD   248
         DC    AL1(8,C' ',0,1,2,3,4,5,6,7,8,8)  YYYYMMDD//   249
         DC    AL1(8,C'/',2,3,8,4,5,8,6,7,8,8)  YY/MM/DD//   250
         DC    AL1(8,C'/',6,7,8,4,5,8,2,3,8,8)  DD/MM/YY//   251
         DC    AL1(8,C'/',4,5,8,6,7,8,2,3,8,8)  MM/DD/YY//   252
         DC    AL1(6,C' ',2,3,4,5,6,7,8,8,8,8)  YYMMDD       253
.*       DC    AL1(6,C' ',6,7,4,5,2,3,8,8,8,8)  DDMMYY       254
.*       DC    AL1(6,C' ',4,5,6,7,2,3,8,8,8,8)  MMDDYY       255
@STR375T DC    P'59,31,29,31,30,31,30,31,31,30,31,30,31'
         DC    P'999'                  Prevent S0C7 with 90366
@STR375B CLI   @STRDWD+4,X'01'         YEAR 2000 OR ABOVE?
         BH    @STR375C                CC>01, JUMP (MUST BE 19 OR 20)
         MVI   @STRDWD+4,X'20'         CC=01, CHANGE TO CC=20
         BE    @STR375C                CC=01, USE CC=20
         CLI   @STRDWD+4+1,X'50'       YY<50?
         BL    @STR375C                YES, USE CC=20
         MVI   @STRDWD+4,X'19'         NO, FORCE CC=19
@STR375C UNPK  @STRWK16(5),@STRDWD+4(3) CCYY?
         ZAP   @STRDWD+1(2),@STR375T+9(1) INIT MONTH COUNTER
         &LAE  @02,@STR375T            TABLE OF MONTHS (NUMBER OF DAYS)
         TM    @STRDWD+4+1,X'01'       ODD YEARS
         BO    @STR375N                  AREN'T LEAP YEARS
         TM    @STRDWD+4+1,X'12'       ZEROES IN 1980, ALL ONES IN 1992
         BNM   @STR375L                MIXED IN 1982/1990
.*       IF IT'S NOT A LEAP YEAR AND DDD>59, THEN ADD 1 TO DDD
@STR375N CP    @STRDWD+4+2(2),@STR375T ARE WE PAST FEB 28 (DDD>59) ?
         BNH   @STR375L                NO, JUMP
         AP    @STRDWD+4+2(2),@STR375T+3(1) ADD 1 (FROM 31) TO DDD
.*--LOOP WHILE DDD > 0
@STR375L AP    @STRDWD+1(2),@STR375T+3(1)   ADD 1 (FROM 31) TO MONTH
         LA    @02,2(,@02)             NEXT ENTRY IN "MONTHS" TABLE
         SP    @STRDWD+4+2(2),0(2,@02) SUB DAYS-IN-MONTH FROM DDD
         BP    @STR375L
.*--ENDLOOP
         AP    @STRDWD+4+2(2),0(2,@02) UNDO LAST "SP" INSTRUCTION
         UNPK  @STRWK16+4(2),@STRDWD+1(2) FYFYFYFY,FMCM??
         UNPK  @STRWK16+6(2),@STRDWD+6(2) FYFYFYFY,FMCMFDCD
         MVZ   @STRWK16+1(7),@STRWK16     FYFYFYFY,FMCMFDCD
         MVC   @STRWK16+8(1),1(@01)    SEPARATOR
         &LAE  @02,@STRWK16+9          WORK AREA
         MVC   0(10,@02),2(@01)        MOVE CORRESPONDING MASK
         TR    0(10,@02),@STRWK16      CONVERT DATE TO THE RIGHT FORMAT
.FEAT5F  ANOP                          JDATE
.*
.*       HEX STRING
.*
         AIF   (NOT &$_FEAT(7)).FEAT7B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR376M MVC   @STRDWD-1(*-*),0(@02)   PREVENT S0C4 IN UNPK
@STR376  LA    @00,8                   MAX LENGTH
         CLR   @03,@00                 CHECK LENGTH
         BNH   @STR376B                JUMP IF LE 8
         LR    @03,@00                 TRUNCATE TO MAXIMUM LENGTH
@STR376B LR    @01,@03                 INPUT LENGTH
         BCTR  @01,0
         EX    @01,@STR376M            MOVE DATA TO SAFE STORAGE
         ALR   @03,@03                 OUTPUT LENGTH
         AGO   .FEAT37B
.FEAT7B  ANOP
         AIF   (NOT &$_FEAT(3)).FEAT37C
         B     @STR390                 MOVE STRING TO OUTPUT LINE
.FEAT37B ANOP
@STR376X &LAE  @02,@STRWK16            WORK AREA
         UNPK  0(9,@02),@STRDWD-1(5)   EXPAND SOURCE BYTES FOR "TR"
         UNPK  8(9,@02),@STRDWD+3(5)   EXPAND SOURCE BYTES FOR "TR"
         TR    0(16,@02),@STRHEXT-240  =C'0123456789ABCDEF'
.FEAT37C ANOP
.*
.*       %TIME
.*
         AIF   (NOT &$_FEAT(12)).FEAT12B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STRTIME DC    X'4021207A20207A20207A20204000'    0X.XX.XX.XX
@STR378  LR    @02,@15                 SAVE BASE REG
         TIME  DEC                     GET HHMMSSHH
         LR    @15,@02                 RESTORE BASE REG
         ST    @00,@STRDWD             STORE HHMMSSHH
         MVC   @STRWK16(13),@STRTIME   MOVE EDIT MASK
         ED    @STRWK16(13),@STRDWD    EDIT HH:MM:SS:HH
         &LAE  @02,@STRWK16+1          WORK AREA
         LA    @03,12                  HH:MM:SS:HH+ SPACE
.FEAT12B ANOP
.*MOVE
@STR390  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN
         BNH   @STR391                 LARGE ENOUGH, JUMP
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.
@STR391  SR    @05,@03                 COMPUTE REMAINING LENGTH
         AIF   (NOT &ARMODE).ARMODE39N
         LAE   @08,0(,@04)             POINTER IN OUTPUT LINE
         LR    @09,@03                 PASS REMAINING LENGTH
         AGO   .ARMODE39X
.ARMODE39N ANOP
         LR    @00,@04                 POINTER IN OUTPUT LINE
         LR    @01,@03                 PASS REMAINING LENGTH
.ARMODE39X ANOP
         LTR   @02,@02                 BLANKS?
         BNZ   @STR392                 NO, JUMP
         SLR   @03,@03                 YES, ZERO LENGTH
@STR392  ICM   @03,B'1000',@STRBLANKS  PAD WITH BLANKS
         AIF   (NOT &ARMODE).ARMODE44N
         MVCL  @08,@02                 MOVE FIELD TO OUTPUT LINE
         LR    @04,@08                 NEW POINTER IN OUTPUT LINE
         AGO   .ARMODE44X
.ARMODE44N ANOP
         MVCL  @00,@02                 MOVE FIELD TO OUTPUT LINE
         LR    @04,@00                 NEW POINTER IN OUTPUT LINE
.ARMODE44X ANOP
@STR398  TM    @STRFLAG,@STRLAST       TEST LAST-ENTRY INDICATOR
         BO    @STR399                 Done, exit
         AIF   (NOT &$_FEAT(1)).FEAT1C
         TM    @STRFLAG,@STRLIT+@STRX40 literal or spaces?
         BM    @STR398L                Literal, not spaces
         BZ    @STR398X                Neither literal nor spaces
         LA    @06,@STRSCON            2-byte entry for blank spaces
         B     @STR310                 PROCESS NEXT ENTRY
@STR398L LA    @01,7                   mask for NR
         IC    @03,@STRFLAG            pick up flags+length
         NR    @03,@01                 R3 has length of short literal
         BZ    @STR398T                not an in-line literal, jump
         LA    @06,@STRLEN2(@03)       Skip VL parm for in-line literal
         B     @STR310                 PROCESS NEXT ENTRY
@STR398T LA    @06,@STRFLEN            4-byte parm for remote literal
         B     @STR310                 PROCESS NEXT ENTRY
.FEAT1C  ANOP
@STR398X LA    @06,@STRNEXT            BUMP UP TO NEXT ENTRY
         B     @STR310                 PROCESS NEXT ENTRY
.*ENDLOOP
.*
.*       END-OF-LINE PROCESSING - PAD WITH BLANKS
.*
@STR399  SLR   @01,@01                 SET UP R1 FOR PADDING
         ICM   @01,B'1000',@STRBLANKS  SET UP R1 FOR PADDING
.***     DROP  @06,@13,@15
         AIF   (&ARMODE).ARMODE52Y
         LA    @14,2                   INCREMENT
         AL    @14,12(,@13)            RETURN ADDRESS
         LR    @15,@04                 CURRENT POINTER IN OUTPUT FIELD
         SL    @15,8(,@13)             CALCULATE LENGTH USED
         MVCL  @04,@00                 PAD WITH BLANKS
         LM    @00,@06,20(@13)         RESTORE WORK REGISTERS
         STM   @06+1,@13-1,48(@13)     MAKE SAVE AREA LOOK NORMAL
         OI    15(@13),1               SIMULATE "T" OPTION OF RETURN
         BR    @14                     RETURN TO CALLER
         AGO   .ARMODE52X
.ARMODE52Y ANOP
         LA    @15,3                   MODIFIABLE AREA
         ESTA  @14,@15                 R14 = START OF OUTPUT AREA
         LR    @15,@04                 END OF OUTPUT AREA
         SR    @15,@14                 R15 = LENGTH USED IN OUTPUT AREA
         MVCL  @04,@00                 PAD WITH BLANKS
         EREG  @00,@14                 RESTORE WORK REGISTERS
         STM   @14,@12,12(@13)         MAKE SAVE AREA LOOK NORMAL
         OI    15(@13),1               SIMULATE "T" OPTION OF RETURN
         PR                            RETURN TO CALLER
.ARMODE52X ANOP
.*
         AIF   (NOT (&$_FEAT(3) OR &$_FEAT(7))).FEAT37T
@STRHEXT DC    C'0123456789ABCDEF'     HEX-TO-EBCDIC CONVERSION
.FEAT37T ANOP
         AIF   (NOT &$_FEAT(8)).FEAT8T
@STRMASK DC    X'4020202020202020,2020202020202120'
.FEAT8T  ANOP
.**********************************************************************
.*       Convert S-con to address                                     *
.*             Input: GPR2 points to an S-CON in the remote parm list *
.*             Output: GPR2 contains the address                      *
.**********************************************************************
@STRS2A  SLR   @00,@00
         ICM   @00,B'0011',0(@02)      R0 = 0000BDDD
         SRDL  @00,12                  R0 = 0000000B, R1= DDD.....
         SRL   @01,20                  R1 = 00000DDD (DISPLACEMENT)
         AIF   (&ARMODE).ARMODE88Y
         CLI   0(@02),@06*16+15        R7-R13?
         BH    @STRS2A3                YES, JUMP
.*BASE REG IS R0-R6
         LTR   @02,@00                 IS R0 THE BASE REG?
         BNZ   @STRS2A2                NO, JUMP
         LTR   @02,@01                 IS THIS A PSA ADDRESS?
         BNZR  @14                     YES, GOBACK
@STRS2A2 SLL   @02,2                   R2= 000000BB BASE * 4
         L     @02,20(@02,@13)         PICK UP BASE REG VALUE
         LA    @02,0(@02,@01)          ADD BASE REG VALUE TO DISPL
         BR    @14
.*BASE REG IS R7-R13
@STRS2A3 LR    @02,@00                 R2= 0000000B (BASE REG NUMBER)
         EX    @02,@STRS2A4            ADD BASE REG VALUE TO DISPL
         BR    @14
@STRS2A4 LA    @02,0(*-*,@01)          ADD BASE REG VALUE TO DISPL
         AGO   .ARMODE88X
.ARMODE88Y ANOP                        AR_MODE=YES
         SLL   @00,2                   MULT BY 4
         CLI   0(@02),@06*16+15        R7-R13?
         BH    @STRS2A6                YES, JUMP
.*BASE REG IS R0-R6
         LTR   @02,@00                 IS R0 THE BASE REG?
         BNZ   @STRS2A5                NO, JUMP
         LTR   @02,@01                 IS THIS A PSA ADDRESS?
         BNZR  @14                     YES, GOBACK
         EREG  @00,@00                 POP R0
         LAE   @02,0                   R2=0 AR2=0
         LR    @02,@00                 PASS R0 VALUE
         LA    @02,0(,@02)             CLEAN UP
         BR    @14                     GOBACK
.*BASE REG IS R1-R6
@STRS2A5 LR    @07,@00                 SAVE R0 (B*4)
         LR    @08,@01                 SAVE R1 (DDD)
         LR    @09,@03                 SAVE R3
         LAE   @10,0(,@04)             SAVE R4
         LR    @11,@05                 SAVE R5
         LAE   @12,0(,@06)             SAVE R6
         EREG  @01,@06                 RELOAD CALLER'S R1-R6
         EX    0,@STRS2A7-4(@07)       ADD BASE REG VALUE TO DISPL
         LR    @03,@09                 RESTORE R3
         LAE   @04,0(,@10)             RESTORE R4
         LR    @05,@11                 RESTORE R5
         LAE   @06,0(,@12)             RESTORE R6
         BR    @14                     GOBACK
.*BASE REG IS R7-R13
@STRS2A6 LR    @02,@00                 R2= 0000000B (BASE REG NUMBER)
         EREG  @07,@12                 RESTORE CALLER'S R7-R12
         EX    0,@STRS2A7-4(@02)       ADD BASE REG VALUE TO DISPL
         BR    @14                     GOBACK
@STRS2A7 LAE   @02,0(@08,@01)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@02)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@03)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@04)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@05)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@06)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@07)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@08)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@09)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@10)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@11)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@12)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@13)          ADD BASE REG VALUE TO DISPL
.ARMODE88X ANOP
         AIF   ('&OPT3' EQ 'LOCTR').GENL7L
@STRBLANKS DC  CL((((*+&STRBLANKS+7-@STRING)/8)*8)-(*-@STRING))' '
         AGO   .GENL7X
.GENL7L  ANOP
@STRBLANKS DC  CL&STRBLANKS.' '
.GENL7X  ANOP
@STRING_SIZE EQU *-@STRING             SIZE OF GENERATED CSECT
         DROP  @06,@13,@15
.**********************************************************************
.*       WORK AREA (CALLER'S SAVE AREA)                               *
.**********************************************************************
@STRSAVE DSECT                         24-BYTE WORK AREA
         DS    A(0,@STRSAVE,@STRSAVE,14,15,0,1,2,3,4,5,6)
@STRWK16 DS    F'7,8,9,10'             WORK AREA
@STRDWD  DS    D'1112'                 WORK AREA
@STRPARM DSECT
@STRFLAG DS    B                   +0  FORMAT, FLAGS
@STRLAST EQU   X'80'                     LAST ENTRY
@STRLIT  EQU   X'40'                     LITERAL, @STRSCON IS AN OFFSET
@STRX40  EQU   X'20'                   String of Spaces
.*             X'0F'                   CONVERSION REQUIRED
.*                                     or length of short literal
@STRHEX  EQU   X'08'                     HEXADECIMAL
@STRBIN  EQU   X'04'                     BINARY
@STRPACK EQU   X'02'                     PACKED
@STRREG  EQU   X'01'                     REGISTER
@STRLEN2 DS    B                   +1  FORMAT, OUTPUT LENGTH
.*                                     or start of short literal
@STRLEFT EQU   X'80'                     LEFT JUSTIFICATION
@STRZERO EQU   X'40'                     LEADING ZEROES
.*             X'3F'                     OUTPUT LENGTH, 0 MEANS TRUNC.
@STRSCON DS    S                   +2  FIELD ADDRESS
@STRFLEN DS    S                   +4  FIELD LENGTH
@STRNEXT EQU   *                   +6
         AIF   (NOT D'$LTORG).MEND99   CSECT-ONLY
         AIF   (D'$STRING).MEND99      CSECT-ONLY
$LTORG   LOCTR
         AIF   ('&OPT3' EQ 'LOCTR').GENL9L
         CNOP  0,4
$STRING  BALR  @15,0                   LOCAL BASE
         L     @15,6(@15,0)            ROUTINE ADDRESS           00
         BR    @15                     GO TO @STRING             04
         AGO   .GENL9
.GENL8   ANOP                          GENERATE,NOCSECT
&$_LIT   SETA  0                       DO NOT GENERATE LITERALS TWICE
&$_FEAT(1) SETB 1,1,1,1,1,1,1,1,1,1,1,1   GENERATE=FULL
$LTORG   LOCTR
         CNOP  0,4
$STRING  BALR  R15,0                   LOCAL BASE
         L     R15,6(R15,0)            ROUTINE ADDRESS           00
         BR    R15                     GO TO @STRING             04
.GENL9   ANOP
         DC    V(@STRING)              ROUTINE ADDRESS           06
         AGO   .MEND
.GENL9L  POP   USING                   GENERATE,LOCTR
$STRING  BAS   R15,$STRING2
         DC    Y(@STRING-*)            OFFSET TO @STRING ROUTINE
$STRING2 AH    R15,0(R15,0)
         BR    R15
.MEND    AIF   ('&PRINT' EQ 'NOGEN').MEND99
         POP   PRINT
.MEND99  MEND
         EJECT
**********************************************************************
**********************************************************************
********* TEST PROGRAM FOR THE 'STRING' MACRO ************************
**********************************************************************
**********************************************************************
**       STRING (GENERATE,AR_MODE)
**       END
         LCLA  &ASMH_HLASM
         AIF   (T'&ASMH_HLASM EQ 'N').ASMH1X
&SYSVER  SETC  'ASMH'
&SYSDATC SETC  '20'.'&SYSDATE'(7,2)'&SYSDATE'(1,2)'&SYSDATE'(4,2)
.ASMH1X  ANOP
TESTPGM  START X'027000'
TESTPGM  AMODE 24                      (PRE-SMS PUT)
         BALR  R12,0
         USING *,R12
 LOAD EP=SYSDEBUG
 LR R15,R0
 BASSM R14,R15
*STRING 1X,INTO=XXX
         OPEN  (SYSPRINT,OUTPUT)
         STRING 'Assembler is &SYSVER, DATE is &SYSDATC',INTO=XXX
         PUT   SYSPRINT,XXX
RBPREFIX EQU   *
RBINTCOD EQU   *+6,2,C'H'
ASCBASID EQU   *+8,2,C'X'
         L     R1,PSATOLD-PSA(0,0)
         L     R1,0(,R1)               TCBRBP
         STRING 'SVC',(RBINTCOD-RBPREFIX(R1),H,R3Z),                   X
               1X,(WWWW,,T),' - ',     VV.MM OF SVC RTNE               X
               ((R8),,X),1X,           COM-REG ADDR                    X
               (ASCBASID,,X),1X,       ASID                            X
               PARM1,1X,               MAIN PGM NAME                   X
               INTO=XXX
         PUT   SYSPRINT,XXX
*
         LA    R2,XXX
         STRING 1X,INTO=((R2),8)
         MACRO                                       JDATE MACRO
        @JDATE &DATE                                 JDATE MACRO
         LA    R1,=P'&DATE'                          JDATE MACRO
*** STRING ((R1),P),2X,((R1),P),INTO=XXX
         STRING ((R1),P),2X,((R1),P,YYMMDD),INTO=XXX,                  X
               3X,((R1),P,YY/MM/DD),                                   X
               3X,((R1),P,DD/MM/YY),                                   X
               3X,((R1),P,MM/DD/YY),                                   X
               3X,((R1),P,YYYYMMDD),                                   X
               3X,((R1),P,YYYY-MM-DD)
         PUT   SYSPRINT,XXX                          JDATE MACRO
         MEND                                        JDATE MACRO
        @JDATE 90058
        @JDATE 91059
        @JDATE 93060
        @JDATE 94365
        @JDATE 80058
        @JDATE 84059
        @JDATE 88060
        @JDATE 92061
        @JDATE 00366
         LA    R2,1234
         STRING 'CVTPTR=X''',(CVTPTR,4,X),'''',INTO=XXX,               X
               ' 1234=',((R2),,R4Z)
         PUT   SYSPRINT,XXX
         L     R1,CVTPTR(0,0)
         STRING 'CVTDATE=',(56(R1),P,YYMMDD),INTO=XXX
         PUT   SYSPRINT,XXX
         LA    R0,1000
         LA    R3,0033
         STRING 'D1=/',D1,'/,WWWW=/',WWWW,'/',                         X
               ((R3),,L),'/',((R3),,X),'/',((R0),,L),'/',              X
               ((R3),,R9B),'/',INTO=XXX
         LR    R4,R15                   LENGTH USED
         PUT   SYSPRINT,XXX
         STRING WWWW,                                                  X
               (4(R13),4,X),'''',(4(R13),F),'''',                      X
               (4(R13),F,L),'''',                                      X
               (4(R13),F,L11),'''',                                    X
               (4(R13),F,Z9),'''',                                     X
               8X,'R4=',((R4),,L),      LENGTH USED                    X
               INTO=XXX
         PUT   SYSPRINT,XXX
         STRING %TIME,D1,'B12345678B',5X,(CTR1,P),1X,PARM1,1X,PARM2,   X
               INTO=XXX
         PUT   SYSPRINT,XXX
         LA    R3,22
         STRING INTO=XXX,'CCC1234A',(D1,(R3)),'.',(CTR1,P,R7Z)
         PUT   SYSPRINT,XXX
         STRING C'DDN2(',(D1,,T),')',X'40C1C2,C3C4',                   +
               ' PSATOLD=',(PSATOLD-PSA,,X),                           +
               INTO=XXX
         PUT   SYSPRINT,XXX
         AIF   ('&SYSVER' EQ 'ASMH').HLASM99
PSABASE  USING PSA,R0
         STRING 'PSATOLD=',(PSABASE.PSATOLD,,X),                       +
               ' PSATOLD=',(PSATOLD-PSA,,X),                           +
               INTO=XXX
         PUT   SYSPRINT,XXX
.HLASM99 ANOP
         BALR  R0,0
         STRING 'R0=',((R0),,X),'   16(R0)=',(16(R0),4,X),INTO=XXX
         PUT   SYSPRINT,XXX
*
AMODE31  LA    R12,0(,R12)             BALR->BASR
         L     R15,=A(*+X'80000006')   AMODE=31
         BSM   0,R15                   AMODE=31
         L     R1,=A(@STRING)          point at CSECT
         CLC   =C'/390',11(R1)         (GENERATE,AR_MODE) ?
         BNE   EXIT                    no, prevent S0E0
         DSPSERV CREATE,                                               X
               NAME==CL8'STRING00',    C'STRING00'                     X
               BLOCKS=HDRBLKS,         SIZE IN PAGES                   X
               STOKEN=DSPCSTKN,                                        X
               ORIGIN=DSPCORG
         ALESERV ADD,STOKEN=DSPCSTKN,ALET=DSPCALET
         LAM   R8,R8,DSPCALET          POINT TO THE DATA SPACE
         L     R8,DSPCORG              SPACE ORIGIN (0 OR 4096)
         SAC   512                     MODE=AR
 LAM R14,R6,TESTPGM+40                 S0E0-28
         STRING WWWW,'AR_MODE WORKED OK',INTO=(2048(R8),100)
         LAM   R2,R2,DSPCALET          POINT TO THE DATA SPACE
         LR    R2,R8                   SPACE ORIGIN (0 OR 4096)
         STRING (2048+L'WWWW(R2),30,T),INTO=XXX
         SAC   0                       MODE=AR OFF
         PUT   SYSPRINT,XXX            AR_MODE WORKS OK
*
EXIT     SLR   R15,R15
         SVC   3                       GOBACK
D1       DC    C'D1-----D1    '
WWWW     DC    C'WWWW'
CTR1     DC    P'1'
PARM1    DC    C'<-PARM1->'
PARM2    DC    C'<-PARM2->'
XXX      DS    CL132
DSPCSTKN DS    XL8                  0  AS TOKEN
DSPCALET DS    F                    8  ALET
DSPCORG  DS    F                   12  ORIGIN
HDRBLKS  DC    F'100'
CVTPTR   EQU   0016,4,C'A'
SYSPRINT DCB   DSORG=PS,DDNAME=SYSPRINT,MACRF=PM,RECFM=FB,LRECL=121
XYZ      DSECT
         STRING GENERATE
**       STRING (GENERATE,NO_CSECT) -\
**       STRING (GENERATE,AR_MODE)  -/
**       STRING (GENERATE,,LOCTR)
**       STRING (GENERATE,AR_MODE,LOCTR)
**       STRING (GENERATE,NO_CSECT,LOCTR)    invalid combination
         YREGS
PSA      DSECT
PSATOLD  EQU   *+X'21C',4,C'A'
         END
//SYSPRINT DD SYSOUT=*
//SYSLIB   DD DISP=SHR,DSN=SYS1.MACLIB
//SYSUT1   DD UNIT=VIO,SPACE=(CYL,2)
//SYSLIN   DD UNIT=VIO,SPACE=(TRK,1),DISP=(,PASS),DCB=BLKSIZE=3200
//*
//GO      EXEC PGM=LOADER,PARM=PRINT,TIME=(,2)
//SYSLIN   DD DSN=*.ASMH.SYSLIN,DISP=(OLD,DELETE)
//SYSLOUT  DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//ABNLTERM DD SYSOUT=*
//ABNLIGNR DD DUMMY
//SYSDEBUG DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*

          +----------------------------------------+
          +                                        +
          +   Documentation for the STRING macro   +
          +                                        +
          +       Last update: 13 Dec 1999         +
          +----------------------------------------+

  The STRING macro is functionally similar to the COBOL DISPLAY
  or PL/I PUT EDIT instructions.

  Using STRING, you can concatenate any number of fields, edit
  each of them if necessary, and get the result in the work area
  you specify.

  Formats:

    (1) label  STRING {field_specification1}
                      {,field_specification2}...
                      ,INTO=workarea×(workarea,length)
                      {,PRINT=GEN×NOGEN}

    (2) label  STRING GENERATE
                      {,PRINT=GEN×NOGEN}


  field_specification

    Each field to be printed is described as a positional
    operand.  Each operand specifies the field address, its
    length, and its formatting requirements.

    Four field description formats are supported:

      1.   symbol
      2.   (symbol,length,format)
      3.   (d(r)×(r),length,format)
      4.   ((r),,format)
      5.   'character string'

  Symbol specifies the field address.  It must be an
    S-type (relocatable) address.

  d(r) may be used to specify the field address in S/370
    base-displacement format.  If d is zero, it may be omitted.
    If d(r) or (r) is used, length must also be specified.  R14
    and R15 may not be used.  If d(0) is used, it is handled the
    way the assembler does, i.e. R0 as a base register is assumed
    to contain zero: 16(0) is equivalent to 16, CVTPTR or, X'10'.

  ((r),,format) specifies that (r) contains the value
    itself, not an address.  R14 and R15 may not be used.

  'character string' specifies a literal enclosed in single quotes
    as specified in a DC instruction.  Hex strings or character
    strings are supported.  The following expressions are equivalent:
    'ABC' C'ABC' X'C1C2C3'

  Length specifies the length and/or the type of the input field.
    It may be specified as an integer, a symbol, a register, or a
    constant.  When used with symbol, it overrides the assembled
    length and/or type.  Length is required if field is specified
    as d(r) or (r).  If a zero length is specified, the field is
    ignored.

        nn   field length in bytes
        H    half-word
        F    full-word
        FL1  1-byte binary integer
        FL3  3-byte binary integer
        P    packed field
        (r)  length of character string (R0 thru R12)
        d(r) length of character string (R1 thru R12)

  Notes:  If the field address is specified as a symbol that
          has been defined previously in the program, the symbol
          type is known and there is no need to specify it.

          If the length is specified as (r) or d(r) and the value
          is greater than the address itself, (r) or d(r) is
          considered to be the end address +1 instead of the length.

          The length is not specified for packed fields.  The
          @STRING subroutine scans the field left-to-right until it
          finds a byte with a valid sign in the low-order 4 bits.

          If symbol is an arithmetic expression and no length is coded,
          the implicit length will be that of the first symbol in the
          expression;  for example, if symbol is coded as PSATOLD-PSA,
          then the implicit length will be L'PSATOLD.

          All numeric items are assumed positive.

  format  optionally indicates editing options that must
          be applied to a field.

      L        left justified
      R        right justified
      nn       output length
      0        adjust length
      Z        leading zeroes
      B        leading/trailing blanks
      T        truncate character string after last non-blank
      X        display in hexadecimal
      YYMMDD   convert julian date to YYMMDD
      YY/MM/DD convert julian date to YY/MM/DD
      DD/MM/YY convert julian date to DD/MM/YY
      MM/DD/YY convert julian date to MM/DD/YY
      YYYYMMDD convert julian date to YYYYMMDD
      YYYY-MM-DD convert julian date to YYYY-MM-DD

    The default format depends on the field type:

        Type                   Default Format

        character string             L
        FL1                          R3B
        H or FL2                     R5B
        other numeric fields         R7B

    Note: L0 and T are equivalent for character strings.

  'character string' is any character string enclosed in
  single quotes.  Blank spaces may be specified as nnX,
  where nn is the number of X'40' bytes you want to be
  inserted in the output line.  %TIME may be specified to
  obtain the current time in hh.mm.ss.hh format.

INTO=workarea×(workarea,length)

  INTO indicates the address and length of the output work area
  into which the result of the concatenation should be placed
  (left justified).  If the work area is too small, truncation
  will occur.  If it is too large, it is padded with blanks.

  The address may be a symbol, d(r) (S-type address) or (r).

  The length may be specified as an integer, a symbol, a register,
  or a constant; it is required if the address is coded as d(r) or
  (r).  If length is not specified for a symbol-type address, the
  assembled length of the symbol is used.

  Upon return from STRING, R15 contains the length actually used
  in the output work area (before padding).

PRINT=GEN×NOGEN

  This operand allows you to temporarily override the PRINT
  specification (GEN or NOGEN).

GENERATE (format 2)

  The GENERATE format must be specified once at the end of the
  program.  It generates the @STRING sub-routine as well as all
  the literals specified in previous invocations of the macro.

  The GENERATE format allows the specifications of the
  AR_MODE, NO_CSECT and LOCTR options.

         STRING GENERATE<,AR_MODE<,LOCTR>>>
                          NO_CSECT

  The following combinations are valid:

         STRING GENERATE
         STRING (GENERATE,NO_CSECT)
         STRING (GENERATE,AR_MODE)
         STRING (GENERATE,,LOCTR)
         STRING (GENERATE,AR_MODE,LOCTR)

  The AR_MODE option can be specified to generate a
  @STRING module that supportd AR mode.  Example:

         STRING (GENERATE,AR_MODE)

  The NO_CSECT option can be specified to indicate that the
  @STRING module should not be generated; only the literals
  are generated in this case.  Example:

         STRING (GENERATE,NO_CSECT)

  The @STRING CSECT contains optional functions (such as %TIME or
  julian date conversion) that are only generated if they have
  been specified in the previous invocations of the STRING macro.

  To generate of a @STRING CSECT that supports all of the optional
  functions, two STRING GENERATE macros must be specified:

         STRING (GENERATE,NO_CSECT)        Generate Literals
         STRING GENERATE                   Generate CSECT

Examples:

     STRING 'ERROR===>',LINE1,'<=== POS ',((R6),,L0),INTO=WORKAREA

     STRING 8X,C'ERRORS FOUND: ',(ERRORS,,L0),INTO=((R7),44)

     STRING 'CVT ADDR IS ',(CVTPTR,4,X),X'40C1C2C3C4',INTO=LINE

     LA    R5,WORK+16              end addr +1
     STRING 'R4=',((R4),,X),INTO=(WORK,(R5)),PRINT=GEN

     STRING '//JOBLIB DD DSN=',(DSN1,,T),',DISP=SHR',INTO=((R2),72)

     PUT31 SYSLIN
     LH    R0,SYSLIN+82            LRECL
     STRING '   NAME  ',(4(R3),8,T),'(R)',INTO=((R1),(R0))

     STRING GENERATE         Generate literals and sub-routine


Programming Notes:

  A STRING macro generates only 6 bytes that need to be covered by
  base registers.  More code is generated at the end of the
  current CSECT (using LOCTR pseudo instruction), but this code
  does not require addressability.  This is particularly useful
  when STRING calls specify a large number of literals.

  Additionally, STRING does not use A-type constants (ACON), but
  S-type constants (SCON) which require symbols to be addressable
  at the point in the program where STRING is issued.

  While this reduces the number of base registers required to
  cover the program's code and makes it easier to write reentrant
  programs, it will produce assembly errors in the following
  situations:

  a.  STRING is used in more than one CSECT in the same assembly

  b.  the CSECT in which STRING is used is longer than 64K

  c.  symbols are not addressable at the point in the program
      where STRING is issued

  d.  the $STRING symbol (generated in the $LTORG LOCTR during the
      GENERATE invocation of the STRING macro) is not addressable
      in some of the STRING calls.
./ ADD NAME=STRING38
***********************************************************************
*                                                                     *
* MACRO NAME = STRING                                                 *
*                                                                     *
* DESCRIPTIVE NAME = STRING Macro Instruction for Assembler XF        *
*                                                                     *
* FUNCTION = Provide capabilities similar to PUT EDIT (of PL/I)       *
*            or STRING (of COBOL) to assembler programs.              *
*                                                                     *
* STATUS = R101                                                       *
*                                                                     *
* AUTHOR = Gilbert Saint-Flour <gsf@pobox.com>                        *
*                                                                     *
* ENVIRONMENT = SEE BELOW                                             *
*                                                                     *
*    AMODE  = ANY                                                     *
*    RMODE  = ANY                                                     *
*     SCP   = S/360 OS, OS/VS, MVS/370                                *
* Processor = Assembler XF, Assembler H, High-Level Assembler         *
*     KEY   = ANY                                                     *
*     MODE  = ANY                                                     *
*     APF   = ANY                                                     *
*                                                                     *
* OPERATION = SEE DOCUMENTATION AT THE END OF THIS FILE               *
*                                                                     *
* INVOCATION = SEE DOCUMENTATION AT THE END OF THIS FILE              *
*                                                                     *
* NOTES = SEE DOCUMENTATION AT THE END OF THIS FILE                   *
*                                                                     *
* CHANGE ACTIVITY                                                     *
*                                                                     *
* $101 ASM XF version of STRING R514                                  *
***********************************************************************
         MACRO
&NAME    STRING &INTO=,&PRINT=NOGEN
         GBLA  &$$LIT
         GBLB  &$$FEAT(16)             FEATURES
.*                                       1 LITERALS
.*                                       2 REGISTER (BIN)
.*                                       3 REGISTER (HEX)
.*                                       4 PACKED
.*                                       5 JDATE
.*                                       6 BINARY
.*                                       7 HEX
.*                                       8 NUMERIC
.*                                       9 LEFT JUST (NUMERIC)
.*                                       10 LEADING ZEROES
.*                                       11 TRUNCATE (CHAR STRING)
.*                                       12 %TIME
         GBLC  &MACVERS
         GBLC  &$$LITS(9999)           LITERALS
         LCLA  &I,&J,&L,&N,&FLAG,&LEN2,&BLANKS
         LCLB  &LAST,&BIN,&HEX,&REG,&PACKED,&LEFT,&ZERO,&TRUNC,&NUMERIC
         LCLC  &LABEL,&LQ,&STR,&TO1,&TO2,&P1S,&P2C,&P2L,&P3C,&P3L
         LCLC  &LIT,&ALLFEAT
&MACVERS SETC '101'                    current version
         AIF   ('&PRINT' EQ 'NOGEN').NOGEN
         PUSH  PRINT
         PRINT GEN
.NOGEN   ANOP
         AIF   (T'&INTO EQ 'O' AND N'&SYSLIST EQ 1                     X
               AND '&SYSLIST(1)' EQ 'GENERATE').GENL
&LABEL   SETC  'IHB&SYSNDX'            STEM FOR LOCAL LABELS
&LQ      SETC  'L'''                   LENGTH ATTRIBUTE
&STR     SETC  ' R&MACVERS XF '
&NAME    L     R15,=A(@STR002)&STR     Routine Address
         BALR  R14,R15                 CALL @STRING Routine
         AIF   (N'&SYSLIST EQ 0).ERR1  NO POSITIONAL OPERANDS, ERROR
         AIF   (T'&INTO EQ 'O').ERR2   NO RECEIVING FIELD, ERROR
         AIF   (N'&INTO GT 2).ERR2     INTO=(A,32,BLURB)
         DC    AL2((&LABEL.P-@STRING)/2) OFFSET TO FIELD DESCRIPTORS
@STRING  CSECT                         NON-ADDRESSABLE CONSTANTS
&TO1     SETC  '&INTO(1)'
&TO2     SETC  '&LQ&INTO'
         AIF   (N'&INTO EQ 1).PUNTO8            JUMP IF INTO=XXX
         AIF   ('&INTO(1)'(1,1) NE '(').PUNTO3  JUMP IF INTO=(XXX,44)
&TO1     SETC  '0&INTO(1)'                              INTO=((R3),44)
.PUNTO3  ANOP
&TO2     SETC  '&INTO(2)'                               INTO=(XXX,LL)
         AIF   ('&INTO(2)'(1,1) NE '(').PUNTO8  JUMP IF INTO=(XXX,44)
&TO2     SETC  '0&INTO(2)'                              INTO=(XXX,(R1))
.PUNTO8  ANOP
&LABEL.P DC    S(&TO1,&TO2)
.*--------------------------------------------------------------------*
.*-------      FIELDS       ------------------------------------------*
.*--------------------------------------------------------------------*
&I       SETA  1
.*LOOP
.LOOP1   ANOP
         ACTR  200                            SYSDEBUG/DIAG055
         AIF   (N'&SYSLIST(&I) GT 3).FLD990   TOO MANY SUB-OPERANDS
&LAST    SETB  (&I EQ N'&SYSLIST)                LOOP
         AIF   ('&SYSLIST(&I)'(1,1) EQ '''').LIT00
         AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) EQ '''').LIT01
.*--------------------------------------------------------------------*
.*       PROCESS FIRST SUBPARAMETER (ADDRESS)                         *
.*--------------------------------------------------------------------*
&P1S     SETC  '&SYSLIST(&I,1)'
&P2L     SETC  '0'                     INPUT LENGTH
&P3L     SETC  '0'                     OUTPUT LENGTH
         AIF   ('&SYSLIST(&I)'(1,1) GE '0').FLD180 SPACES
         AIF   ('&SYSLIST(&I)' EQ '%TIME').FLD190 %TIME
         AIF   ('&SYSLIST(&I,1)'(1,1) NE '(').FLD115 (R2)
         AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD250
&P1S     SETC  '0&SYSLIST(&I,1)'       CHANGE (R1) TO 0(R1)
.FLD115  ANOP
.*
         AIF   (T'&SYSLIST(&I,2) NE 'O').FLD200
.*
.*       EXTRACT RBCDE FROM PRB.RBCDE (HLASM)
.*
&L       SETA  1
.*--LOOP
.FLD131  AIF   ('&P1S'(&L,1) EQ '.').FLD133
&L       SETA  &L+1
         AIF   (&L LT K'&P1S).FLD131
.*--ENDLOOP
&P2L     SETC  '&P1S'                    ABCDEF FROM ABCDEF
         AGO   .FLD134
.FLD133  ANOP
&P2L     SETC  '&P1S'(&L+1,K'&P1S-&L)    RBCDE FROM PRB.RBCDE
.FLD134  ANOP
.*
.*XF     AIF   (NOT D'&P2L).FLD140
&P2C     SETC  T'&P2L
.*MNOTE *,'&P1 &P2C'
         AIF   ('&P2C' EQ 'F' OR '&P2C' EQ 'H' OR '&P2C' EQ 'P').FLD220
         AIF   ('&P2C' EQ 'G').FLD210  FL2
.FLD140  ANOP
.*
.*       EXTRACT PSATOLD FROM PSATOLD-PSA
.*
&L       SETA  1
.*--LOOP
.FLD141  AIF   ('&P2L'(&L,1) EQ '-').FLD143
         AIF   ('&P2L'(&L,1) EQ '+').FLD143
&L       SETA  &L+1
         AIF   (&L LT K'&P2L).FLD141
.*--ENDLOOP
&P2L     SETC  '&LQ&P2L'               L'ABCDEF
         AGO   .FLD300
.FLD143  ANOP
&P2L     SETC  '&LQ'.'&P2L'(1,&L-1)    L'PSATOLD FROM PSATOLD-PSA
         AGO   .FLD300
.*
.FLD180  AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) NE 'X').FLD800
&P2L     SETC  '&SYSLIST(&I)'(1,K'&SYSLIST(&I)-1) 12
         AIF   (&LAST).FLD186
         DC    X'60',AL1(&P2L)         BLANKS
         AGO   .LIT90
.FLD186  DC    X'E0',AL1(&P2L),0S(0)   BLANKS
         AGO   .LIT90
.*
.FLD190  ANOP                          %TIME
&P1S     SETC  '1(14)'                 %TIME
&$$FEAT(12) SETB 1                     %TIME
         AGO   .FLD800
.*--------------------------------------------------------------------*
.*       PROCESS SECOND SUBPARAMETER (LENGTH/TYPE)                    *
.*--------------------------------------------------------------------*
.FLD200  AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD300 NO LENGTH SPECIFIED
&P2C     SETC  '&SYSLIST(&I,2)'
         AGO   .FLD220
.*T'&P1=G
.FLD210  ANOP
&L       SETA  L'&SYSLIST(&I)          T'&P1 = 'G'
&P2C     SETC  'FL&L'                  T'&P1 = 'G'
.*
.FLD220  ANOP
&P2L     SETC  '0&P2C'                 (R2) LENGTH
         AIF   ('&P2C'(1,1) EQ '(').FLD300
&P2L     SETC  '&P2C'                  3(R2) LENGTH
         AIF   ('&P2C'(K'&P2C,1) EQ ')').FLD300
&P2L     SETC  '0'
&PACKED  SETB  ('&P2C' EQ 'P')
         AIF   (&PACKED).FLD290
&P2L     SETC  '1'
         AIF   ('&P2C' EQ 'FL1').FLD240
&P2L     SETC  '3'
         AIF   ('&P2C' EQ 'FL2' OR '&P2C' EQ 'H').FLD240
&P2L     SETC  '7'
         AIF   ('&P2C' EQ 'FL3').FLD240
&P2L     SETC  '15'
         AIF   ('&P2C' EQ 'F').FLD240
&P2L     SETC  '&P2C'                  IMMEDIATE LENGTH, FIELD
         AGO   .FLD300
.*
.FLD240  ANOP                          BINARY VARIABLE
&BIN     SETB  1
         AGO   .FLD300
.*
.FLD250  ANOP                          REGISTER CONTENT
&REG     SETB  1
         AGO   .FLD300
.*
.FLD290  ANOP                          PACKED
&P2L     SETC  '1'
.*--------------------------------------------------------------------*
.*       PROCESS THIRD SUBPARAMETER (OUTPUT FORMAT)                   *
.*--------------------------------------------------------------------*
.FLD300  AIF   (T'&SYSLIST(&I,3) EQ 'O').FLD800
&HEX     SETB  ('&SYSLIST(&I,3)' EQ 'X') HEXADECIMAL
&TRUNC   SETB  ('&SYSLIST(&I,3)' EQ 'T') TRUNCATE
         AIF   (&HEX OR &TRUNC).FLD800
.*
&P3C     SETC  '&SYSLIST(&I,3)'
&P3L     SETC  '248'
         AIF   ('&P3C' EQ 'YYYY-MM-DD' AND &PACKED).FLD308
&P3L     SETC  '249'
         AIF   ('&P3C' EQ 'YYYYMMDD' AND &PACKED).FLD308
&P3L     SETC  '250'
         AIF   ('&P3C' EQ 'YY/MM/DD' AND &PACKED).FLD308
&P3L     SETC  '251'
         AIF   ('&P3C' EQ 'DD/MM/YY' AND &PACKED).FLD308
&P3L     SETC  '252'
         AIF   ('&P3C' EQ 'MM/DD/YY' AND &PACKED).FLD308
&P3L     SETC  '253'
         AIF   ('&P3C' EQ 'YYMMDD'   AND &PACKED).FLD308
&P3L     SETC  '0'
         AIF   (T'&SYSLIST(&I,2) NE 'N').FLD310
         MNOTE 8,'EDIT PATTERN NOT ALLOWED WITH CHARACTER STRING'
         AGO   .FLD310
.FLD308  ANOP
&$$FEAT(5) SETB 1                      JDATE
         AGO   .FLD800
.*--LOOP
.FLD310  AIF   ('&P3C'(1,1) EQ 'R').FLD318       DEFAULT
         AIF   ('&P3C'(1,1) EQ 'B').FLD318       DEFAULT
         AIF   ('&P3C'(1,1) NE 'L').FLD311
&LEFT    SETB  1
         AGO   .FLD318
.FLD311  AIF   ('&P3C'(1,1) NE 'Z').FLD312
&ZERO    SETB  1
         AGO   .FLD318
.FLD312  AIF   ('&P3C'(1,1) LT '0').FLD993       nn in RnnB is not num
         AIF   ('&P3C'(1,1) GT '9').FLD993       nn in RnnB is not num
&P3L     SETC  '&P3L'.'&P3C'(1,1)
.FLD318  ANOP
.*MNOTE *,'&SYSLIST(&I) P3C=/&P3C/ P3L=/&P3L/'
&P3C     SETC  '&P3C '(2,K'&P3C-1)     STRIP OFF FIRST CHARACTER
         AIF   (K'&P3C GT 0).FLD310
.*--ENDLOOP
         AIF   (&P3L GT 16).FLD993               nn in RnnB is too big
.*--------------------------------------------------------------------*
.FLD800  ANOP
&NUMERIC SETB  (&BIN OR &PACKED OR (&REG AND NOT &HEX))
&TRUNC   SETB  (&TRUNC OR (&LEFT AND NOT &NUMERIC))
&LEFT    SETB  (&LEFT AND &NUMERIC)
         AIF   (NOT &NUMERIC).FLD810
         AIF   (&LEFT OR '&P3L' NE '0').FLD810
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH ((R3))
         AIF   (&REG).FLD810
&P3L     SETC  '3'                     DEFAULT OUTPUT LENGTH
         AIF   ('&P2C' EQ 'FL1').FLD810
&P3L     SETC  '5'                     DEFAULT OUTPUT LENGTH
         AIF   ('&P2C' EQ 'H' OR '&P2C' EQ 'FL2').FLD810
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH
.FLD810  ANOP
&FLAG    SETA  &HEX*8+&BIN*4+&PACKED*2+&REG*1
&LEN2    SETA  &TRUNC*128+&LEFT*128+&ZERO*64+&P3L
&$$FEAT(2) SETB (&$$FEAT(2) OR (&REG AND NOT &HEX))   REGISTER (BIN)
&$$FEAT(3) SETB (&$$FEAT(3) OR (&REG AND &HEX))       REGISTER (HEX)
&$$FEAT(4) SETB (&$$FEAT(4) OR &PACKED)               PACKED
&$$FEAT(6) SETB (&$$FEAT(6) OR &BIN)                  BINARY
&$$FEAT(7) SETB (&$$FEAT(7) OR (&HEX AND NOT &REG))   HEX
&$$FEAT(8) SETB (&$$FEAT(8) OR &NUMERIC)              BIN,PACKED
&$$FEAT(9) SETB (&$$FEAT(9) OR (&LEFT AND &NUMERIC))
&$$FEAT(10) SETB (&$$FEAT(10) OR &ZERO)
&$$FEAT(11) SETB (&$$FEAT(11) OR &TRUNC)
&BIN     SETB  0                    RESET FLAGS
&HEX     SETB  0                    RESET FLAGS
&REG     SETB  0                    RESET FLAGS
&PACKED  SETB  0                    RESET FLAGS
&LEFT    SETB  0                    RESET FLAGS
&ZERO    SETB  0                    RESET FLAGS
&TRUNC   SETB  0                    RESET FLAGS
         AIF   (&FLAG GE 10).FLD995
         AIF   (&LAST).FLD816
         DC    X'0&FLAG',AL1(&LEN2),SL2(&P1S,&P2L)
         AGO   .LIT99
.FLD816  DC    X'8&FLAG',AL1(&LEN2),SL2(&P1S,&P2L),0S(0)
         AGO   .LIT99
.FLD990  MNOTE 12,'OPERAND &I HAS TOO MANY SUB-OPERANDS'
         AGO   .LIT99
.FLD993  MNOTE 8,'THIRD SUBPARAMETER IS INVALID: ''&SYSLIST(&I,3)'''
         AGO   .LIT99
.FLD995  MNOTE 8,'Invalid Combination of Attributes: &SYSLIST(&I)'
         AGO   .LIT99
.*--------------------------------------------------------------------*
.*------------ LITERALS ----------------------------------------------*
.*--------------------------------------------------------------------*
.LIT00   ANOP
&LIT     SETC  'C&SYSLIST(&I)'
         AGO   .LIT09
.LIT01   ANOP
&LIT     SETC  '&SYSLIST(&I)'
.LIT09   ANOP                              calculate length of literal
&J       SETA  3
&L       SETA  0
         ACTR  K'&LIT+K'&LIT+100
         AIF   ('&LIT'(1,1) EQ 'X').LIT11X
.*LOOP
.LIT11C AIF ('&LIT'(&J,1) NE '''' AND '&LIT'(&J,1) NE '&&'(1,1)).LIT12C
&J       SETA  &J+1
.LIT12C  ANOP
&J       SETA  &J+1
&L       SETA  &L+1
         AIF   (&J LT K'&LIT).LIT11C
.*ENDLOOP
         AGO   .LIT15
.*LOOP
.LIT11X  AIF   ('&LIT'(&J,1) EQ ',').LIT12X
&L       SETA  &L+1
.LIT12X  ANOP
&J       SETA  &J+1
         AIF   (&J LT K'&LIT).LIT11X
.*ENDLOOP
&L       SETA  (&L+1)/2
.LIT15   ANOP                               generate in-line literal
         AIF   (&L GT 5).LIT40
         AIF   (&LAST).LIT16
         DC    X'4&L',&LIT
         AGO   .LIT90
.LIT16   DC    X'C&L',&LIT,0S(0)
         AGO   .LIT90
.LIT40   ANOP                                  check literal table
         AIF   (&$$LIT EQ 0).LIT50
&N       SETA  1
         ACTR  &$$LIT*3+200
.LIT41   AIF   ('&LIT' EQ '&$$LITS(&N)').LIT80       LOOP
&N       SETA  &N+1                                  LOOP
         AIF   (&N LE &$$LIT).LIT41                  LOOP
.LIT50   ANOP
&$$LIT   SETA  &$$LIT+1
&$$LITS(&$$LIT) SETC '&LIT'
&N       SETA  &$$LIT
.LIT80   ANOP                               generate remote literal
&N       SETA  &N+1000
         AIF   (&LAST).LIT86
         DC    X'40',AL1(&L),AL2($LIT&N-*)
         AGO   .LIT90
.LIT86   DC    X'C0',AL1(&L),AL2($LIT&N-*),0S(0)
.LIT90   ANOP
&$$FEAT(1) SETB 1                      LITERAL
.LIT99   ANOP
.*--------------------------------------------------------------------*
&I       SETA  1+&I                              LOOP
         AIF   (&I LE N'&SYSLIST).LOOP1          LOOP
.*ENDLOOP
&SYSECT  CSECT
         AGO   .MEND
.ERR1    MNOTE 12,'AT LEAST ONE INPUT FIELD MUST BE SPECIFIED'
         AGO   .MEND
.ERR2    MNOTE 12,'INVALID OUTPUT AREA SPECIFICATION'
         AGO   .MEND
.**********************************************************************
.*       GENERATE: GENERATE LITERALS AND PROCESSING ROUTINE           *
.**********************************************************************
.GENL    ANOP
&ALLFEAT SETC  '&$$FEAT(1)&$$FEAT(2)&$$FEAT(3)&$$FEAT(4)'
&ALLFEAT SETC  '&ALLFEAT&$$FEAT(5)&$$FEAT(7)&$$FEAT(6)&$$FEAT(8)'
&ALLFEAT SETC  '&ALLFEAT&$$FEAT(9)&$$FEAT(10)&$$FEAT(11)&$$FEAT(12)'
&BLANKS  SETA  2
         MNOTE *,'STRING R&MACVERS - FEATURES GENERATED: &ALLFEAT'
@STRING  CSECT
         AIF   (&$$LIT EQ 0).GENL3
.GENL2   ANOP                                LOOP
&N       SETA  &N+1                          LOOP
&I       SETA  &N+1000                       LOOP
$LIT&I   DC    &$$LITS(&N)
         AIF   (&N LT &$$LIT).GENL2          LOOP
.GENL3   DC    0H'0'
.**********************************************************************
.*                                                                    *
.*       STRING SUB-ROUTINE                                           *
.*                                                                    *
.*             CAUTION: BYTES 49-72 OF THE CALLER'S SAVE AREA         *
.*                      (R7-R12 SLOTS) ARE USED AS WORK SPACE         *
.*                                                                    *
.**********************************************************************
@00      EQU   0                       WORK REGISTER
@01      EQU   1                       WORK REGISTER
@02      EQU   2                       WORK REGISTER
@03      EQU   3                       WORK REGISTER
@04      EQU   4                       WORK REGISTER
@05      EQU   5                       WORK REGISTER
@06      EQU   6                       WORK REGISTER
@13      EQU   13                      CALLER'S SAVE AREA
@14      EQU   14                      WORK REGISTER
@15      EQU   15                      BASE REG
         USING @STR002,@15
         USING @STRSAVE,@13
@STR002  B     @STR011                 BRANCH AROUND EYE-CATCHER
         DC    AL1(@STR003-*),C'@STRING/XF R&MACVERS &ALLFEAT',0H'0'
@STR003  DC    Y(@STR002-@STRING)      Offset to @STRING
@STR011  STM   @14,@06,12(@13)         Save caller's registers
         SLR   @06,@06                 R6=0
         ICM   @06,B'0011',0(@14)      PICK UP PARM-LIST 1/2 OFFSET
         ALR   @06,@06                 PICK UP PARM-LIST OFFSET
         ALR   @06,@15                 R6 NOW POINTS TO PARM LIST
         SH    @06,@STR003             R6 NOW POINTS TO PARM LIST
         USING @STRSCON,@06
         LA    @02,@STRSCON            RESOLVE FIRST SCON (ADDR)
         BAL   @14,@STRS2A             GET ADDRESS IN R2
         LA    @04,0(,@02)             KEEP ADDRESS OF "INTO" FIELD
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO
         OI    0(@04),0                GET CLEAN S0C4 IF ADDRESS IS BAD
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)
         BAL   @14,@STRS2A             GET LENGTH IN R2
         LR    @05,@02                 KEEP LENGTH OF "INTO" FIELD
         CR    @05,@04                 END ADDRESS?
         BL    @STR282                 NO, JUMP
         SR    @05,@04                 CALCULATE LENGTH
@STR282  ST    @04,8(,@13)             SAVE ADDRESS FOR LATER
.**********************************************************************
.*       MOVE FIELDS TO OUTPUT AREA                                   *
.**********************************************************************
         LA    @06,@STRNEXT            POINT TO 1ST FIELD DESC
         USING @STRPARM,@06
.*LOOP
@STR310  EQU   *
         AIF   (NOT &$$FEAT(1)).FEAT1A
         TM    @STRFLAG,@STRLIT        IS THIS A LITERAL?
         BO    @STR372                 YES, JUMP
.FEAT1A  ANOP
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(3)).FEAT23A
         TM    @STRFLAG,@STRREG        REGISTER?
         BO    @STR323                 YES, JUMP
.FEAT23A ANOP
         AIF   (NOT &$$FEAT(12)).FEAT12A
         CLI   @STRSCON,X'E0'          IS IT %TIME ?
         BE    @STR378                 YES, JUMP
.FEAT12A ANOP
.*
.*       IT'S A FIELD (SCON)
.*
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)
         BAL   @14,@STRS2A             GET LENGTH IN R2
         AIF   (&$$FEAT(9)).FEAT9E1    (LEFT JUSTIFICATION, NUMERIC)
         LTR   @03,@02                 KEEP/TEST LENGTH
         BZ    @STR398                 ZERO LENGTH, DO NOT EDIT
         AGO   .FEAT9E2
.FEAT9E1 LTR   @03,@02                 KEEP/TEST LENGTH
         BP    @STR313                 LENGTH POSITIVE, JUMP
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?
         BNO   @STR398                 NO, ZERO LENGTH IS NOT OK
@STR313  EQU   *
.FEAT9E2 ANOP
         LA    @02,@STRSCON            RESOLVE FIRST SCON (ADDR)
         BAL   @14,@STRS2A             GET ADDRESS IN R2
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO
         CLI   0(@02),0                GET CLEAN S0C4 IF ADDRESS IS BAD
         CR    @03,@02                 END ADDRESS?
         BL    @STR314                 NO, JUMP
         LA    @00,X'0080'             PSA ADDRESS
         CLR   @02,@00                 PSA REFERENCE?
         BL    @STR314                 YES, JUMP
         SR    @03,@02                 CALCULATE LENGTH
@STR314  EQU   *
         AIF   (NOT &$$FEAT(6)).FEAT6A
         TM    @STRFLAG,@STRBIN        BINARY FIELD?
         BO    @STR328                 YES, JUMP
.FEAT6A  ANOP
         AIF   (NOT &$$FEAT(4)).FEAT4A
         TM    @STRFLAG,@STRPACK       PACKED FIELD?
         BO    @STR351                 YES, JUMP
.FEAT4A  ANOP
         AIF   (NOT &$$FEAT(7)).FEAT7A
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?
         BO    @STR376                 YES, JUMP
.FEAT7A  ANOP
.*
.*       TRUNCATE CHARACTER STRING
.*
         AIF   (NOT &$$FEAT(11)).FEAT11A
         CLI   @STRLEN2,@STRLEFT       CHECK JUSTIFICATION, OUTPUT LEN
         BNE   @STR390                 NO STRING TRUNCATION, JUMP
         LA    @01,0(@03,@02)          FIRST BYTE AFTER FIELD
@STR318  BCTR  @01,0                   DOWN 1 BYTE                 LOOP
         CLI   0(@01),C' '             IS IT A SPACE ?             LOOP
         BNE   @STR390                 LAST NON-BLANK BYTE         LOOP
         BCT   @03,@STR318             LOOP UNTIL 1ST NON-BLANK    LOOP
         B     @STR398                 BLANK FIELD, DO NOT EDIT
         AGO   .FEAT11B
.FEAT11A ANOP
         AIF (&$$FEAT(2)+&$$FEAT(3)+&$$FEAT(4)+&$$FEAT(6) EQ 0).FEAT11B
         B     @STR390                 EDIT
.FEAT11B ANOP
.*
.*       REGISTER (R0-R13)
.*
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(3)).FEAT23B
@STR323L LR    @00,*-*                 COPY VALUE FOR EDIT
@STR323  LH    @01,@STRSCON            REG NUMBER IN BITS 28-31
         EX    @01,@STR323L            COPY R7-R13 INTO R0
         CLI   @STRSCON+1,@06+1        IS THIS R7-R13?
         BNL   @STR323T                YES, JUMP
         SLL   @01,2                   R1= 000000BB BASE * 4
         L     @00,20(@01,@13)         PICK UP VALUE FOR R0-R6
@STR323T EQU   *
         AIF   (NOT &$$FEAT(3)).FEAT3R REG,HEX
         AIF   (NOT &$$FEAT(2)).FEAT2H REG,BIN
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?
         BNO   @STR330                 NO, EDIT FWD
.FEAT2H  ANOP
         STCM  @00,B'1111',@STRDWD-1   STORE IT FOR CVD/UNPK
         LA    @03,8                   OUTPUT LENGTH
         B     @STR376X                EDIT IN HEX
.FEAT3R  ANOP
         AIF   (NOT &$$FEAT(6)).FEAT23B
         B     @STR330                 EDIT R0
.FEAT23B ANOP
.*
.*       BINARY VARIABLE: @03 CONTAINS THE ICM MASK (1 3 7 F)
.*
         AIF   (NOT &$$FEAT(6)).FEAT6B
@STR328M ICM   @00,*-*,0(@02)          **EXECUTED INSTRUCTION**
@STR328  SLR   @00,@00
         EX    @03,@STR328M            LOAD THE BINARY VARIABLE
.FEAT6B  ANOP
.*
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(6)).FEAT6C
@STR330  CVD   @00,@STRDWD             CONVERT VALUE TO DECIMAL
         AIF   (NOT &$$FEAT(4)).FEAT6C
         B     @STR361                 EDIT DWD
.FEAT6C  ANOP
.*
.*       PACKED FIELD
.*
         AIF   (NOT &$$FEAT(4)).FEAT4B
@STRZAP  ZAP   @STRDWD,0(*-*,@02)      MOVE TO @STRDWD
@STR351  LA    @03,0(,@02)             FIRST BYTE OF PACKED FIELD
         BALR  @14,0
         TM    0(@03),X'0C'            IS THIS THE SIGN BYTE?
         LA    @03,1(,@03)              (NEXT BYTE)
         BNOR  @14                     NO, LOOP MORE
         SLR   @03,@02                 GET LENGTH OF PACKED FIELD
         BCTR  @03,0
         EX    @03,@STRZAP             EXECUTE ZAP
         AIF   (NOT &$$FEAT(5)).FEAT4B
         CLI   @STRLEN2,248            JULIAN-TO-YYMMDD CONV?
         BNL   @STR375                 YES, JUMP
.FEAT4B  ANOP                          PACKED
.*
.*       EDIT @STRDWD (BIN, REG, PACKED)
.*
         AIF   (NOT &$$FEAT(8)).FEAT8B
@STR361  IC    @00,@STRLEN2            OUTPUT LENGTH
         LA    @03,X'003F'             MASK FOR "AND"
         NR    @03,@00                 OUTPUT LENGTH
         MVC   @STRWK16(16),@STRMASK   EDIT MASK
.*
.*       LEFT-JUSTIFICATION (NUMERIC)
.*
         AIF   (NOT &$$FEAT(9)).FEAT9B
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?
         BNO   @STR367                 NO, JUMP
         LA    @01,@STRWK16+15         PREVENT BAD R1
         EDMK  @STRWK16(16),@STRDWD    ZONED DECIMAL
         LA    @02,0(,@01)             FIRST STRING POSITION
         LTR   @03,@03                 CHECK OUTPUT LENGTH
         BNZ   @STR363                 JUMP IF NOT ZERO
.*       L0    (LEFT JUSTIFIED, NO PADDING)
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @03,@02                 COMPUTE STRING LENGTH
         B     @STR390                 MOVE FIELD TO OUTPUT LINE
.*       L1-L63 (LEFT JUSTIFIED, PADDING)
@STR363  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN
         BNH   @STR364                 LARGE ENOUGH, JUMP
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.
@STR364  SR    @05,@03                 COMPUTE REMAINING LENGTH
         LR    @00,@04                 POINTER IN OUTPUT LINE
         LR    @01,@03                 LENGTH WITH PADDING
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @03,@02                 COMPUTE STRING LENGTH
         B     @STR392                 MOVE FIELD TO OUTPUT LINE
@STR367  EQU   *
.FEAT9B  ANOP
         AIF   (NOT &$$FEAT(10)).FEAT10B
         TM    @STRLEN2,@STRZERO       LEADING ZEROES REQ'D?
         BNO   @STR368                 NO, JUMP
         MVI   @STRWK16,C'0'           YES, CHANGE X'40' TO C'0'
@STR368  EQU   *
.FEAT10B ANOP
         ED    @STRWK16(16),@STRDWD    ZONED DECIMAL
         LA    @02,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @02,@03                 FIRST STRING POSITION
.FEAT8B  ANOP
.*
.*       LITERAL (@STRSCON IS A 16-BIT OFFSET)
.*       Short Literal (low-order 4 bits of @STRFLAG contains length)
.*       BLANKS  (@STRSCON=ZERO)
.*
         AIF   (NOT &$$FEAT(1)).FEAT1B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR372  LA    @01,7                   mask for NR
         LA    @02,@STRLEN2            1st byte of short literal
         IC    @03,@STRFLAG            pick up flags+length
         NR    @03,@01                 R3 has length of short literal
         BNZ   @STR390                 short literal, go move it
         SLR   @02,@02                 Clear Address Register
         IC    @03,@STRLEN2            GET LITERAL LENGTH
         TM    @STRFLAG,@STRX40        string of spaces?
         BO    @STR390                 yes, go move them
         ICM   @02,B'0011',@STRSCON    LOAD LITERAL OFFSET
         LA    @02,@STRSCON(@02)       CONVERT OFFSET TO FULL ADDRESS
.FEAT1B  ANOP
.*
.*       CONVERT JULIAN DATE TO YYMMDD
.*
         AIF   (NOT &$$FEAT(5)).FEAT5F
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR375  LA    @00,248                 MASK FOR 'SLR'
         SLR   @01,@01
         IC    @01,@STRLEN2            248-255
         SLR   @01,@00                 000-007
         LA    @00,12                  L'@STR375W
         MR    @00,@00                 COMPUTE OFFSET
         LA    @01,@STR375W(@01)       ENTRY IN "TR" MASK TABLE
         SLR   @03,@03
         IC    @03,0(,@01)             LENGTH OF DATE (6, 8 OR 10)
         ZAP   @STRDWD,@STRDWD         DATE=0000000?               @JDT
         BNZ   @STR375B                NO, JUMP                    @JDT
@STR375Z LA    @02,@BLANKS             WORK AREA
&BLANKS  SETA  10                      WE NEED AT LEAST 10 BLANKS
         B     @STR390                 MOVE FIELD TO OUTPUT LINE
@STR375W DC    AL1(10,C'-',0,1,2,3,8,4,5,8,6,7) YYYY-MM-DD   248
         DC    AL1(8,C' ',0,1,2,3,4,5,6,7,8,8)  YYYYMMDD//   249
         DC    AL1(8,C'/',2,3,8,4,5,8,6,7,8,8)  YY/MM/DD//   250
         DC    AL1(8,C'/',6,7,8,4,5,8,2,3,8,8)  DD/MM/YY//   251
         DC    AL1(8,C'/',4,5,8,6,7,8,2,3,8,8)  MM/DD/YY//   252
         DC    AL1(6,C' ',2,3,4,5,6,7,8,8,8,8)  YYMMDD       253
.*       DC    AL1(6,C' ',6,7,4,5,2,3,8,8,8,8)  DDMMYY       254
.*       DC    AL1(6,C' ',4,5,6,7,2,3,8,8,8,8)  MMDDYY       255
@STR375T DC    P'59,31,29,31,30,31,30,31,31,30,31,30,31'
         DC    P'999'                  Prevent S0C7 with 90366
@STR375B CLI   @STRDWD+4,X'01'         YEAR 2000 OR ABOVE?
         BH    @STR375C                CC>01, JUMP (MUST BE 19 OR 20)
         MVI   @STRDWD+4,X'20'         CC=01, CHANGE TO CC=20
         BE    @STR375C                CC=01, USE CC=20
         CLI   @STRDWD+4+1,X'50'       YY<50?
         BL    @STR375C                YES, USE CC=20
         MVI   @STRDWD+4,X'19'         NO, FORCE CC=19
@STR375C UNPK  @STRWK16(5),@STRDWD+4(3) CCYY?
         ZAP   @STRDWD+1(2),@STR375T+9(1) INIT MONTH COUNTER
         LA    @02,@STR375T            TABLE OF MONTHS (NUMBER OF DAYS)
         TM    @STRDWD+4+1,X'01'       ODD YEARS
         BO    @STR375N                  AREN'T LEAP YEARS
         TM    @STRDWD+4+1,X'12'       ZEROES IN 1980, ALL ONES IN 1992
         BNM   @STR375L                MIXED IN 1982/1990
.*       IF IT'S NOT A LEAP YEAR AND DDD>59, THEN ADD 1 TO DDD
@STR375N CP    @STRDWD+4+2(2),@STR375T ARE WE PAST FEB 28 (DDD>59) ?
         BNH   @STR375L                NO, JUMP
         AP    @STRDWD+4+2(2),@STR375T+3(1) ADD 1 (FROM 31) TO DDD
.*--LOOP WHILE DDD > 0
@STR375L AP    @STRDWD+1(2),@STR375T+3(1)   ADD 1 (FROM 31) TO MONTH
         LA    @02,2(,@02)             NEXT ENTRY IN "MONTHS" TABLE
         SP    @STRDWD+4+2(2),0(2,@02) SUB DAYS-IN-MONTH FROM DDD
         BP    @STR375L
.*--ENDLOOP
         AP    @STRDWD+4+2(2),0(2,@02) UNDO LAST "SP" INSTRUCTION
         UNPK  @STRWK16+4(2),@STRDWD+1(2) FYFYFYFY,FMCM??
         UNPK  @STRWK16+6(2),@STRDWD+6(2) FYFYFYFY,FMCMFDCD
         MVZ   @STRWK16+1(7),@STRWK16     FYFYFYFY,FMCMFDCD
         MVC   @STRWK16+8(1),1(@01)    SEPARATOR
         LA    @02,@STRWK16+9          WORK AREA
         MVC   0(10,@02),2(@01)        MOVE CORRESPONDING MASK
         TR    0(10,@02),@STRWK16      CONVERT DATE TO THE RIGHT FORMAT
.FEAT5F  ANOP                          JDATE
.*
.*       HEX STRING
.*
         AIF   (NOT &$$FEAT(7)).FEAT7B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR376M MVC   @STRDWD-1(*-*),0(@02)   PREVENT S0C4 IN UNPK
@STR376  LA    @00,8                   MAX LENGTH
         CLR   @03,@00                 CHECK LENGTH
         BNH   @STR376B                JUMP IF LE 8
         LR    @03,@00                 TRUNCATE TO MAXIMUM LENGTH
@STR376B LR    @01,@03                 INPUT LENGTH
         BCTR  @01,0
         EX    @01,@STR376M            MOVE DATA TO SAFE STORAGE
         ALR   @03,@03                 OUTPUT LENGTH
         AGO   .FEAT37B
.FEAT7B  ANOP
         AIF   (NOT &$$FEAT(3)).FEAT37C
         B     @STR390                 MOVE STRING TO OUTPUT LINE
.FEAT37B ANOP
@STR376X LA    @02,@STRWK16            WORK AREA
         UNPK  0(9,@02),@STRDWD-1(5)   EXPAND SOURCE BYTES FOR "TR"
         UNPK  8(9,@02),@STRDWD+3(5)   EXPAND SOURCE BYTES FOR "TR"
         TR    0(16,@02),@STRHEXT-240  =C'0123456789ABCDEF'
.FEAT37C ANOP
.*
.*       %TIME
.*
         AIF   (NOT &$$FEAT(12)).FEAT12B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STRTIME DC    X'4021207A20207A20207A20204000'    0X.XX.XX.XX
@STR378  LR    @02,@15                 SAVE BASE REG
         TIME  DEC                     GET HHMMSSHH
         LR    @15,@02                 RESTORE BASE REG
         ST    @00,@STRDWD             STORE HHMMSSHH
         MVC   @STRWK16(13),@STRTIME   MOVE EDIT MASK
         ED    @STRWK16(13),@STRDWD    EDIT HH:MM:SS:HH
         LA    @02,@STRWK16+1          WORK AREA
         LA    @03,12                  HH:MM:SS:HH+ SPACE
.FEAT12B ANOP
.*MOVE
@STR390  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN
         BNH   @STR391                 LARGE ENOUGH, JUMP
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.
@STR391  SR    @05,@03                 COMPUTE REMAINING LENGTH
         LR    @00,@04                 POINTER IN OUTPUT LINE
         LR    @01,@03                 PASS REMAINING LENGTH
         LTR   @02,@02                 BLANKS?
         BNZ   @STR392                 NO, JUMP
         SLR   @03,@03                 YES, ZERO LENGTH
@STR392  ICM   @03,B'1000',@BLANKS     PAD WITH BLANKS
         MVCL  @00,@02                 MOVE FIELD TO OUTPUT LINE
         LR    @04,@00                 NEW POINTER IN OUTPUT LINE
@STR398  TM    @STRFLAG,@STRLAST       TEST LAST-ENTRY INDICATOR
         BO    @STR399                 Done, exit
         AIF   (NOT &$$FEAT(1)).FEAT1C
         TM    @STRFLAG,@STRLIT+@STRX40 literal or spaces?
         BM    @STR398L                Literal, not spaces
         BZ    @STR398X                Neither literal nor spaces
         LA    @06,@STRSCON            2-byte entry for blank spaces
         B     @STR310                 PROCESS NEXT ENTRY
@STR398L LA    @01,7                   mask for NR
         IC    @03,@STRFLAG            pick up flags+length
         NR    @03,@01                 R3 has length of short literal
         BZ    @STR398T                not an in-line literal, jump
         LA    @06,@STRLEN2(@03)       Skip VL parm for in-line literal
         B     @STR310                 PROCESS NEXT ENTRY
@STR398T LA    @06,@STRFLEN            4-byte parm for remote literal
         B     @STR310                 PROCESS NEXT ENTRY
.FEAT1C  ANOP
@STR398X LA    @06,@STRNEXT            BUMP UP TO NEXT ENTRY
         B     @STR310                 PROCESS NEXT ENTRY
.*ENDLOOP
.*
.*       END-OF-LINE PROCESSING - PAD WITH BLANKS
.*
@STR399  SLR   @01,@01                 SET UP R1 FOR PADDING
         ICM   @01,B'1000',@BLANKS     SET UP R1 FOR PADDING
.***     DROP  @06,@13,@15
         LA    @14,2                   INCREMENT
         AL    @14,12(,@13)            RETURN ADDRESS
         LR    @15,@04                 CURRENT POINTER IN OUTPUT FIELD
         SL    @15,8(,@13)             CALCULATE LENGTH USED
         MVCL  @04,@00                 PAD WITH BLANKS
         LM    @00,@06,20(@13)         RESTORE WORK REGISTERS
         STM   @06+1,@13-1,48(@13)     MAKE SAVE AREA LOOK NORMAL
         OI    15(@13),1               SIMULATE "T" OPTION OF RETURN
         BR    @14                     RETURN TO CALLER
.*
         AIF   (NOT (&$$FEAT(3) OR &$$FEAT(7))).FEAT37T
@STRHEXT DC    C'0123456789ABCDEF'     HEX-TO-EBCDIC CONVERSION
.FEAT37T ANOP
         AIF   (NOT &$$FEAT(8)).FEAT8T
@STRMASK DC    X'4020202020202020,2020202020202120'
.FEAT8T  ANOP
.**********************************************************************
.*       Convert S-con to address                                     *
.*             Input: GPR2 points to an S-CON in the remote parm list *
.*             Output: GPR2 contains the address                      *
.**********************************************************************
@STRS2A  SLR   @00,@00
         ICM   @00,B'0011',0(@02)      R0 = 0000BDDD
         SRDL  @00,12                  R0 = 0000000B, R1= DDD.....
         SRL   @01,20                  R1 = 00000DDD (DISPLACEMENT)
         CLI   0(@02),@06*16+15        R7-R13?
         BH    @STRS2A3                YES, JUMP
.*BASE REG IS R0-R6
         LTR   @02,@00                 IS R0 THE BASE REG?
         BNZ   @STRS2A2                NO, JUMP
         LTR   @02,@01                 IS THIS A PSA ADDRESS?
         BNZR  @14                     YES, GOBACK
@STRS2A2 SLL   @02,2                   R2= 000000BB BASE * 4
         L     @02,20(@02,@13)         PICK UP BASE REG VALUE
         LA    @02,0(@02,@01)          ADD BASE REG VALUE TO DISPL
         BR    @14
.*BASE REG IS R7-R13
@STRS2A3 LR    @02,@00                 R2= 0000000B (BASE REG NUMBER)
         EX    @02,@STRS2A4            ADD BASE REG VALUE TO DISPL
         BR    @14
@STRS2A4 LA    @02,0(*-*,@01)          ADD BASE REG VALUE TO DISPL
@BLANKS  DC    CL((((*+&BLANKS+7-@STRING)/8)*8)-(*-@STRING))' '
@STRSIZE EQU   *-@STRING               SIZE OF GENERATED CSECT
         DROP  @06,@13,@15
.**********************************************************************
.*       WORK AREA (CALLER'S SAVE AREA)                               *
.**********************************************************************
@STRSAVE DSECT                         24-BYTE WORK AREA
         DS    A(0,@STRSAVE,@STRSAVE,14,15,0,1,2,3,4,5,6)
@STRWK16 DS    F'7,8,9,10'             WORK AREA
@STRDWD  DS    D'1112'                 WORK AREA
@STRPARM DSECT
@STRFLAG DS    B                   +0  FORMAT, FLAGS
@STRLAST EQU   X'80'                     LAST ENTRY
@STRLIT  EQU   X'40'                     LITERAL, @STRSCON IS AN OFFSET
@STRX40  EQU   X'20'                   String of Spaces
.*             X'0F'                   CONVERSION REQUIRED
.*                                     or length of short literal
@STRHEX  EQU   X'08'                     HEXADECIMAL
@STRBIN  EQU   X'04'                     BINARY
@STRPACK EQU   X'02'                     PACKED
@STRREG  EQU   X'01'                     REGISTER
@STRLEN2 DS    B                   +1  FORMAT, OUTPUT LENGTH
.*                                     or start of short literal
@STRLEFT EQU   X'80'                     LEFT JUSTIFICATION
@STRZERO EQU   X'40'                     LEADING ZEROES
.*             X'3F'                     OUTPUT LENGTH, 0 MEANS TRUNC.
@STRSCON DS    S                   +2  FIELD ADDRESS
@STRFLEN DS    S                   +4  FIELD LENGTH
@STRNEXT EQU   *                   +6
.MEND    AIF   ('&PRINT' EQ 'NOGEN').MEND99
         POP   PRINT
.MEND99  MEND
./ ADD NAME=STRING
***********************************************************************
*                                                                     *
* MACRO NAME = STRING                                                 *
*                                                                     *
* DESCRIPTIVE NAME = STRING Macro Instruction for Assembler XF        *
*                                                                     *
* FUNCTION = Provide capabilities similar to PUT EDIT (of PL/I)       *
*            or STRING (of COBOL) to assembler programs.              *
*                                                                     *
* STATUS = R101                                                       *
*                                                                     *
* AUTHOR = Gilbert Saint-Flour <gsf@pobox.com>                        *
*                                                                     *
* ENVIRONMENT = SEE BELOW                                             *
*                                                                     *
*    AMODE  = ANY                                                     *
*    RMODE  = ANY                                                     *
*     SCP   = S/360 OS, OS/VS, MVS/370                                *
* Processor = Assembler XF, Assembler H, High-Level Assembler         *
*     KEY   = ANY                                                     *
*     MODE  = ANY                                                     *
*     APF   = ANY                                                     *
*                                                                     *
* OPERATION = SEE DOCUMENTATION AT THE END OF THIS FILE               *
*                                                                     *
* INVOCATION = SEE DOCUMENTATION AT THE END OF THIS FILE              *
*                                                                     *
* NOTES = SEE DOCUMENTATION AT THE END OF THIS FILE                   *
*                                                                     *
* CHANGE ACTIVITY                                                     *
*                                                                     *
* $101 ASM XF version of STRING R514                                  *
***********************************************************************
         MACRO
&NAME    STRING &INTO=,&PRINT=NOGEN
         GBLA  &$$LIT
         GBLB  &$$FEAT(16)             FEATURES
.*                                       1 LITERALS
.*                                       2 REGISTER (BIN)
.*                                       3 REGISTER (HEX)
.*                                       4 PACKED
.*                                       5 JDATE
.*                                       6 BINARY
.*                                       7 HEX
.*                                       8 NUMERIC
.*                                       9 LEFT JUST (NUMERIC)
.*                                       10 LEADING ZEROES
.*                                       11 TRUNCATE (CHAR STRING)
.*                                       12 %TIME
         GBLC  &MACVERS
         GBLC  &$$LITS(9999)           LITERALS
         LCLA  &I,&J,&L,&N,&FLAG,&LEN2,&BLANKS
         LCLB  &LAST,&BIN,&HEX,&REG,&PACKED,&LEFT,&ZERO,&TRUNC,&NUMERIC
         LCLC  &LABEL,&LQ,&STR,&TO1,&TO2,&P1S,&P2C,&P2L,&P3C,&P3L
         LCLC  &LIT,&ALLFEAT
&MACVERS SETC '101'                    current version
         AIF   ('&PRINT' EQ 'NOGEN').NOGEN
         PUSH  PRINT
         PRINT GEN
.NOGEN   ANOP
         AIF   (T'&INTO EQ 'O' AND N'&SYSLIST EQ 1                     X
               AND '&SYSLIST(1)' EQ 'GENERATE').GENL
&LABEL   SETC  'IHB&SYSNDX'            STEM FOR LOCAL LABELS
&LQ      SETC  'L'''                   LENGTH ATTRIBUTE
&STR     SETC  ' R&MACVERS XF '
&NAME    L     R15,=A(@STR002)&STR     Routine Address
         BALR  R14,R15                 CALL @STRING Routine
         AIF   (N'&SYSLIST EQ 0).ERR1  NO POSITIONAL OPERANDS, ERROR
         AIF   (T'&INTO EQ 'O').ERR2   NO RECEIVING FIELD, ERROR
         AIF   (N'&INTO GT 2).ERR2     INTO=(A,32,BLURB)
         DC    AL2((&LABEL.P-@STRING)/2) OFFSET TO FIELD DESCRIPTORS
@STRING  CSECT                         NON-ADDRESSABLE CONSTANTS
&TO1     SETC  '&INTO(1)'
&TO2     SETC  '&LQ&INTO'
         AIF   (N'&INTO EQ 1).PUNTO8            JUMP IF INTO=XXX
         AIF   ('&INTO(1)'(1,1) NE '(').PUNTO3  JUMP IF INTO=(XXX,44)
&TO1     SETC  '0&INTO(1)'                              INTO=((R3),44)
.PUNTO3  ANOP
&TO2     SETC  '&INTO(2)'                               INTO=(XXX,LL)
         AIF   ('&INTO(2)'(1,1) NE '(').PUNTO8  JUMP IF INTO=(XXX,44)
&TO2     SETC  '0&INTO(2)'                              INTO=(XXX,(R1))
.PUNTO8  ANOP
&LABEL.P DC    S(&TO1,&TO2)
.*--------------------------------------------------------------------*
.*-------      FIELDS       ------------------------------------------*
.*--------------------------------------------------------------------*
&I       SETA  1
.*LOOP
.LOOP1   ANOP
         ACTR  200                            SYSDEBUG/DIAG055
         AIF   (N'&SYSLIST(&I) GT 3).FLD990   TOO MANY SUB-OPERANDS
&LAST    SETB  (&I EQ N'&SYSLIST)                LOOP
         AIF   ('&SYSLIST(&I)'(1,1) EQ '''').LIT00
         AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) EQ '''').LIT01
.*--------------------------------------------------------------------*
.*       PROCESS FIRST SUBPARAMETER (ADDRESS)                         *
.*--------------------------------------------------------------------*
&P1S     SETC  '&SYSLIST(&I,1)'
&P2L     SETC  '0'                     INPUT LENGTH
&P3L     SETC  '0'                     OUTPUT LENGTH
         AIF   ('&SYSLIST(&I)'(1,1) GE '0').FLD180 SPACES
         AIF   ('&SYSLIST(&I)' EQ '%TIME').FLD190 %TIME
         AIF   ('&SYSLIST(&I,1)'(1,1) NE '(').FLD115 (R2)
         AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD250
&P1S     SETC  '0&SYSLIST(&I,1)'       CHANGE (R1) TO 0(R1)
.FLD115  ANOP
.*
         AIF   (T'&SYSLIST(&I,2) NE 'O').FLD200
.*
.*       EXTRACT RBCDE FROM PRB.RBCDE (HLASM)
.*
&L       SETA  1
.*--LOOP
.FLD131  AIF   ('&P1S'(&L,1) EQ '.').FLD133
&L       SETA  &L+1
         AIF   (&L LT K'&P1S).FLD131
.*--ENDLOOP
&P2L     SETC  '&P1S'                    ABCDEF FROM ABCDEF
         AGO   .FLD134
.FLD133  ANOP
&P2L     SETC  '&P1S'(&L+1,K'&P1S-&L)    RBCDE FROM PRB.RBCDE
.FLD134  ANOP
.*
.*XF     AIF   (NOT D'&P2L).FLD140
&P2C     SETC  T'&P2L
.*MNOTE *,'&P1 &P2C'
         AIF   ('&P2C' EQ 'F' OR '&P2C' EQ 'H' OR '&P2C' EQ 'P').FLD220
         AIF   ('&P2C' EQ 'G').FLD210  FL2
.FLD140  ANOP
.*
.*       EXTRACT PSATOLD FROM PSATOLD-PSA
.*
&L       SETA  1
.*--LOOP
.FLD141  AIF   ('&P2L'(&L,1) EQ '-').FLD143
         AIF   ('&P2L'(&L,1) EQ '+').FLD143
&L       SETA  &L+1
         AIF   (&L LT K'&P2L).FLD141
.*--ENDLOOP
&P2L     SETC  '&LQ&P2L'               L'ABCDEF
         AGO   .FLD300
.FLD143  ANOP
&P2L     SETC  '&LQ'.'&P2L'(1,&L-1)    L'PSATOLD FROM PSATOLD-PSA
         AGO   .FLD300
.*
.FLD180  AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) NE 'X').FLD800
&P2L     SETC  '&SYSLIST(&I)'(1,K'&SYSLIST(&I)-1) 12
         AIF   (&LAST).FLD186
         DC    X'60',AL1(&P2L)         BLANKS
         AGO   .LIT90
.FLD186  DC    X'E0',AL1(&P2L),0S(0)   BLANKS
         AGO   .LIT90
.*
.FLD190  ANOP                          %TIME
&P1S     SETC  '1(14)'                 %TIME
&$$FEAT(12) SETB 1                     %TIME
         AGO   .FLD800
.*--------------------------------------------------------------------*
.*       PROCESS SECOND SUBPARAMETER (LENGTH/TYPE)                    *
.*--------------------------------------------------------------------*
.FLD200  AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD300 NO LENGTH SPECIFIED
&P2C     SETC  '&SYSLIST(&I,2)'
         AGO   .FLD220
.*T'&P1=G
.FLD210  ANOP
&L       SETA  L'&SYSLIST(&I)          T'&P1 = 'G'
&P2C     SETC  'FL&L'                  T'&P1 = 'G'
.*
.FLD220  ANOP
&P2L     SETC  '0&P2C'                 (R2) LENGTH
         AIF   ('&P2C'(1,1) EQ '(').FLD300
&P2L     SETC  '&P2C'                  3(R2) LENGTH
         AIF   ('&P2C'(K'&P2C,1) EQ ')').FLD300
&P2L     SETC  '0'
&PACKED  SETB  ('&P2C' EQ 'P')
         AIF   (&PACKED).FLD290
&P2L     SETC  '1'
         AIF   ('&P2C' EQ 'FL1').FLD240
&P2L     SETC  '3'
         AIF   ('&P2C' EQ 'FL2' OR '&P2C' EQ 'H').FLD240
&P2L     SETC  '7'
         AIF   ('&P2C' EQ 'FL3').FLD240
&P2L     SETC  '15'
         AIF   ('&P2C' EQ 'F').FLD240
&P2L     SETC  '&P2C'                  IMMEDIATE LENGTH, FIELD
         AGO   .FLD300
.*
.FLD240  ANOP                          BINARY VARIABLE
&BIN     SETB  1
         AGO   .FLD300
.*
.FLD250  ANOP                          REGISTER CONTENT
&REG     SETB  1
         AGO   .FLD300
.*
.FLD290  ANOP                          PACKED
&P2L     SETC  '1'
.*--------------------------------------------------------------------*
.*       PROCESS THIRD SUBPARAMETER (OUTPUT FORMAT)                   *
.*--------------------------------------------------------------------*
.FLD300  AIF   (T'&SYSLIST(&I,3) EQ 'O').FLD800
&HEX     SETB  ('&SYSLIST(&I,3)' EQ 'X') HEXADECIMAL
&TRUNC   SETB  ('&SYSLIST(&I,3)' EQ 'T') TRUNCATE
         AIF   (&HEX OR &TRUNC).FLD800
.*
&P3C     SETC  '&SYSLIST(&I,3)'
&P3L     SETC  '248'
         AIF   ('&P3C' EQ 'YYYY-MM-DD' AND &PACKED).FLD308
&P3L     SETC  '249'
         AIF   ('&P3C' EQ 'YYYYMMDD' AND &PACKED).FLD308
&P3L     SETC  '250'
         AIF   ('&P3C' EQ 'YY/MM/DD' AND &PACKED).FLD308
&P3L     SETC  '251'
         AIF   ('&P3C' EQ 'DD/MM/YY' AND &PACKED).FLD308
&P3L     SETC  '252'
         AIF   ('&P3C' EQ 'MM/DD/YY' AND &PACKED).FLD308
&P3L     SETC  '253'
         AIF   ('&P3C' EQ 'YYMMDD'   AND &PACKED).FLD308
&P3L     SETC  '0'
         AIF   (T'&SYSLIST(&I,2) NE 'N').FLD310
         MNOTE 8,'EDIT PATTERN NOT ALLOWED WITH CHARACTER STRING'
         AGO   .FLD310
.FLD308  ANOP
&$$FEAT(5) SETB 1                      JDATE
         AGO   .FLD800
.*--LOOP
.FLD310  AIF   ('&P3C'(1,1) EQ 'R').FLD318       DEFAULT
         AIF   ('&P3C'(1,1) EQ 'B').FLD318       DEFAULT
         AIF   ('&P3C'(1,1) NE 'L').FLD311
&LEFT    SETB  1
         AGO   .FLD318
.FLD311  AIF   ('&P3C'(1,1) NE 'Z').FLD312
&ZERO    SETB  1
         AGO   .FLD318
.FLD312  AIF   ('&P3C'(1,1) LT '0').FLD993       nn in RnnB is not num
         AIF   ('&P3C'(1,1) GT '9').FLD993       nn in RnnB is not num
&P3L     SETC  '&P3L'.'&P3C'(1,1)
.FLD318  ANOP
.*MNOTE *,'&SYSLIST(&I) P3C=/&P3C/ P3L=/&P3L/'
&P3C     SETC  '&P3C '(2,K'&P3C-1)     STRIP OFF FIRST CHARACTER
         AIF   (K'&P3C GT 0).FLD310
.*--ENDLOOP
         AIF   (&P3L GT 16).FLD993               nn in RnnB is too big
.*--------------------------------------------------------------------*
.FLD800  ANOP
&NUMERIC SETB  (&BIN OR &PACKED OR (&REG AND NOT &HEX))
&TRUNC   SETB  (&TRUNC OR (&LEFT AND NOT &NUMERIC))
&LEFT    SETB  (&LEFT AND &NUMERIC)
         AIF   (NOT &NUMERIC).FLD810
         AIF   (&LEFT OR '&P3L' NE '0').FLD810
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH ((R3))
         AIF   (&REG).FLD810
&P3L     SETC  '3'                     DEFAULT OUTPUT LENGTH
         AIF   ('&P2C' EQ 'FL1').FLD810
&P3L     SETC  '5'                     DEFAULT OUTPUT LENGTH
         AIF   ('&P2C' EQ 'H' OR '&P2C' EQ 'FL2').FLD810
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH
.FLD810  ANOP
&FLAG    SETA  &HEX*8+&BIN*4+&PACKED*2+&REG*1
&LEN2    SETA  &TRUNC*128+&LEFT*128+&ZERO*64+&P3L
&$$FEAT(2) SETB (&$$FEAT(2) OR (&REG AND NOT &HEX))   REGISTER (BIN)
&$$FEAT(3) SETB (&$$FEAT(3) OR (&REG AND &HEX))       REGISTER (HEX)
&$$FEAT(4) SETB (&$$FEAT(4) OR &PACKED)               PACKED
&$$FEAT(6) SETB (&$$FEAT(6) OR &BIN)                  BINARY
&$$FEAT(7) SETB (&$$FEAT(7) OR (&HEX AND NOT &REG))   HEX
&$$FEAT(8) SETB (&$$FEAT(8) OR &NUMERIC)              BIN,PACKED
&$$FEAT(9) SETB (&$$FEAT(9) OR (&LEFT AND &NUMERIC))
&$$FEAT(10) SETB (&$$FEAT(10) OR &ZERO)
&$$FEAT(11) SETB (&$$FEAT(11) OR &TRUNC)
&BIN     SETB  0                    RESET FLAGS
&HEX     SETB  0                    RESET FLAGS
&REG     SETB  0                    RESET FLAGS
&PACKED  SETB  0                    RESET FLAGS
&LEFT    SETB  0                    RESET FLAGS
&ZERO    SETB  0                    RESET FLAGS
&TRUNC   SETB  0                    RESET FLAGS
         AIF   (&FLAG GE 10).FLD995
         AIF   (&LAST).FLD816
         DC    X'0&FLAG',AL1(&LEN2),SL2(&P1S,&P2L)
         AGO   .LIT99
.FLD816  DC    X'8&FLAG',AL1(&LEN2),SL2(&P1S,&P2L),0S(0)
         AGO   .LIT99
.FLD990  MNOTE 12,'OPERAND &I HAS TOO MANY SUB-OPERANDS'
         AGO   .LIT99
.FLD993  MNOTE 8,'THIRD SUBPARAMETER IS INVALID: ''&SYSLIST(&I,3)'''
         AGO   .LIT99
.FLD995  MNOTE 8,'Invalid Combination of Attributes: &SYSLIST(&I)'
         AGO   .LIT99
.*--------------------------------------------------------------------*
.*------------ LITERALS ----------------------------------------------*
.*--------------------------------------------------------------------*
.LIT00   ANOP
&LIT     SETC  'C&SYSLIST(&I)'
         AGO   .LIT09
.LIT01   ANOP
&LIT     SETC  '&SYSLIST(&I)'
.LIT09   ANOP                              calculate length of literal
&J       SETA  3
&L       SETA  0
         ACTR  K'&LIT+K'&LIT+100
         AIF   ('&LIT'(1,1) EQ 'X').LIT11X
.*LOOP
.LIT11C AIF ('&LIT'(&J,1) NE '''' AND '&LIT'(&J,1) NE '&&'(1,1)).LIT12C
&J       SETA  &J+1
.LIT12C  ANOP
&J       SETA  &J+1
&L       SETA  &L+1
         AIF   (&J LT K'&LIT).LIT11C
.*ENDLOOP
         AGO   .LIT15
.*LOOP
.LIT11X  AIF   ('&LIT'(&J,1) EQ ',').LIT12X
&L       SETA  &L+1
.LIT12X  ANOP
&J       SETA  &J+1
         AIF   (&J LT K'&LIT).LIT11X
.*ENDLOOP
&L       SETA  (&L+1)/2
.LIT15   ANOP                               generate in-line literal
         AIF   (&L GT 5).LIT40
         AIF   (&LAST).LIT16
         DC    X'4&L',&LIT
         AGO   .LIT90
.LIT16   DC    X'C&L',&LIT,0S(0)
         AGO   .LIT90
.LIT40   ANOP                                  check literal table
         AIF   (&$$LIT EQ 0).LIT50
&N       SETA  1
         ACTR  &$$LIT*3+200
.LIT41   AIF   ('&LIT' EQ '&$$LITS(&N)').LIT80       LOOP
&N       SETA  &N+1                                  LOOP
         AIF   (&N LE &$$LIT).LIT41                  LOOP
.LIT50   ANOP
&$$LIT   SETA  &$$LIT+1
&$$LITS(&$$LIT) SETC '&LIT'
&N       SETA  &$$LIT
.LIT80   ANOP                               generate remote literal
&N       SETA  &N+1000
         AIF   (&LAST).LIT86
         DC    X'40',AL1(&L),AL2($LIT&N-*)
         AGO   .LIT90
.LIT86   DC    X'C0',AL1(&L),AL2($LIT&N-*),0S(0)
.LIT90   ANOP
&$$FEAT(1) SETB 1                      LITERAL
.LIT99   ANOP
.*--------------------------------------------------------------------*
&I       SETA  1+&I                              LOOP
         AIF   (&I LE N'&SYSLIST).LOOP1          LOOP
.*ENDLOOP
&SYSECT  CSECT
         AGO   .MEND
.ERR1    MNOTE 12,'AT LEAST ONE INPUT FIELD MUST BE SPECIFIED'
         AGO   .MEND
.ERR2    MNOTE 12,'INVALID OUTPUT AREA SPECIFICATION'
         AGO   .MEND
.**********************************************************************
.*       GENERATE: GENERATE LITERALS AND PROCESSING ROUTINE           *
.**********************************************************************
.GENL    ANOP
&ALLFEAT SETC  '&$$FEAT(1)&$$FEAT(2)&$$FEAT(3)&$$FEAT(4)'
&ALLFEAT SETC  '&ALLFEAT&$$FEAT(5)&$$FEAT(7)&$$FEAT(6)&$$FEAT(8)'
&ALLFEAT SETC  '&ALLFEAT&$$FEAT(9)&$$FEAT(10)&$$FEAT(11)&$$FEAT(12)'
&BLANKS  SETA  2
         MNOTE *,'STRING R&MACVERS - FEATURES GENERATED: &ALLFEAT'
@STRING  CSECT
         AIF   (&$$LIT EQ 0).GENL3
.GENL2   ANOP                                LOOP
&N       SETA  &N+1                          LOOP
&I       SETA  &N+1000                       LOOP
$LIT&I   DC    &$$LITS(&N)
         AIF   (&N LT &$$LIT).GENL2          LOOP
.GENL3   DC    0H'0'
.**********************************************************************
.*                                                                    *
.*       STRING SUB-ROUTINE                                           *
.*                                                                    *
.*             CAUTION: BYTES 49-72 OF THE CALLER'S SAVE AREA         *
.*                      (R7-R12 SLOTS) ARE USED AS WORK SPACE         *
.*                                                                    *
.**********************************************************************
@00      EQU   0                       WORK REGISTER
@01      EQU   1                       WORK REGISTER
@02      EQU   2                       WORK REGISTER
@03      EQU   3                       WORK REGISTER
@04      EQU   4                       WORK REGISTER
@05      EQU   5                       WORK REGISTER
@06      EQU   6                       WORK REGISTER
@13      EQU   13                      CALLER'S SAVE AREA
@14      EQU   14                      WORK REGISTER
@15      EQU   15                      BASE REG
         USING @STR002,@15
         USING @STRSAVE,@13
@STR002  B     @STR011                 BRANCH AROUND EYE-CATCHER
         DC    AL1(@STR003-*),C'@STRING/XF R&MACVERS &ALLFEAT',0H'0'
@STR003  DC    Y(@STR002-@STRING)      Offset to @STRING
@STR011  STM   @14,@06,12(@13)         Save caller's registers
         SLR   @06,@06                 R6=0
         ICM   @06,B'0011',0(@14)      PICK UP PARM-LIST 1/2 OFFSET
         ALR   @06,@06                 PICK UP PARM-LIST OFFSET
         ALR   @06,@15                 R6 NOW POINTS TO PARM LIST
         SH    @06,@STR003             R6 NOW POINTS TO PARM LIST
         USING @STRSCON,@06
         LA    @02,@STRSCON            RESOLVE FIRST SCON (ADDR)
         BAL   @14,@STRS2A             GET ADDRESS IN R2
         LA    @04,0(,@02)             KEEP ADDRESS OF "INTO" FIELD
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO
         OI    0(@04),0                GET CLEAN S0C4 IF ADDRESS IS BAD
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)
         BAL   @14,@STRS2A             GET LENGTH IN R2
         LR    @05,@02                 KEEP LENGTH OF "INTO" FIELD
         CR    @05,@04                 END ADDRESS?
         BL    @STR282                 NO, JUMP
         SR    @05,@04                 CALCULATE LENGTH
@STR282  ST    @04,8(,@13)             SAVE ADDRESS FOR LATER
.**********************************************************************
.*       MOVE FIELDS TO OUTPUT AREA                                   *
.**********************************************************************
         LA    @06,@STRNEXT            POINT TO 1ST FIELD DESC
         USING @STRPARM,@06
.*LOOP
@STR310  EQU   *
         AIF   (NOT &$$FEAT(1)).FEAT1A
         TM    @STRFLAG,@STRLIT        IS THIS A LITERAL?
         BO    @STR372                 YES, JUMP
.FEAT1A  ANOP
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(3)).FEAT23A
         TM    @STRFLAG,@STRREG        REGISTER?
         BO    @STR323                 YES, JUMP
.FEAT23A ANOP
         AIF   (NOT &$$FEAT(12)).FEAT12A
         CLI   @STRSCON,X'E0'          IS IT %TIME ?
         BE    @STR378                 YES, JUMP
.FEAT12A ANOP
.*
.*       IT'S A FIELD (SCON)
.*
         LA    @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)
         BAL   @14,@STRS2A             GET LENGTH IN R2
         AIF   (&$$FEAT(9)).FEAT9E1    (LEFT JUSTIFICATION, NUMERIC)
         LTR   @03,@02                 KEEP/TEST LENGTH
         BZ    @STR398                 ZERO LENGTH, DO NOT EDIT
         AGO   .FEAT9E2
.FEAT9E1 LTR   @03,@02                 KEEP/TEST LENGTH
         BP    @STR313                 LENGTH POSITIVE, JUMP
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?
         BNO   @STR398                 NO, ZERO LENGTH IS NOT OK
@STR313  EQU   *
.FEAT9E2 ANOP
         LA    @02,@STRSCON            RESOLVE FIRST SCON (ADDR)
         BAL   @14,@STRS2A             GET ADDRESS IN R2
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO
         CLI   0(@02),0                GET CLEAN S0C4 IF ADDRESS IS BAD
         CR    @03,@02                 END ADDRESS?
         BL    @STR314                 NO, JUMP
         LA    @00,X'0080'             PSA ADDRESS
         CLR   @02,@00                 PSA REFERENCE?
         BL    @STR314                 YES, JUMP
         SR    @03,@02                 CALCULATE LENGTH
@STR314  EQU   *
         AIF   (NOT &$$FEAT(6)).FEAT6A
         TM    @STRFLAG,@STRBIN        BINARY FIELD?
         BO    @STR328                 YES, JUMP
.FEAT6A  ANOP
         AIF   (NOT &$$FEAT(4)).FEAT4A
         TM    @STRFLAG,@STRPACK       PACKED FIELD?
         BO    @STR351                 YES, JUMP
.FEAT4A  ANOP
         AIF   (NOT &$$FEAT(7)).FEAT7A
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?
         BO    @STR376                 YES, JUMP
.FEAT7A  ANOP
.*
.*       TRUNCATE CHARACTER STRING
.*
         AIF   (NOT &$$FEAT(11)).FEAT11A
         CLI   @STRLEN2,@STRLEFT       CHECK JUSTIFICATION, OUTPUT LEN
         BNE   @STR390                 NO STRING TRUNCATION, JUMP
         LA    @01,0(@03,@02)          FIRST BYTE AFTER FIELD
@STR318  BCTR  @01,0                   DOWN 1 BYTE                 LOOP
         CLI   0(@01),C' '             IS IT A SPACE ?             LOOP
         BNE   @STR390                 LAST NON-BLANK BYTE         LOOP
         BCT   @03,@STR318             LOOP UNTIL 1ST NON-BLANK    LOOP
         B     @STR398                 BLANK FIELD, DO NOT EDIT
         AGO   .FEAT11B
.FEAT11A ANOP
         AIF (&$$FEAT(2)+&$$FEAT(3)+&$$FEAT(4)+&$$FEAT(6) EQ 0).FEAT11B
         B     @STR390                 EDIT
.FEAT11B ANOP
.*
.*       REGISTER (R0-R13)
.*
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(3)).FEAT23B
@STR323L LR    @00,*-*                 COPY VALUE FOR EDIT
@STR323  LH    @01,@STRSCON            REG NUMBER IN BITS 28-31
         EX    @01,@STR323L            COPY R7-R13 INTO R0
         CLI   @STRSCON+1,@06+1        IS THIS R7-R13?
         BNL   @STR323T                YES, JUMP
         SLL   @01,2                   R1= 000000BB BASE * 4
         L     @00,20(@01,@13)         PICK UP VALUE FOR R0-R6
@STR323T EQU   *
         AIF   (NOT &$$FEAT(3)).FEAT3R REG,HEX
         AIF   (NOT &$$FEAT(2)).FEAT2H REG,BIN
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?
         BNO   @STR330                 NO, EDIT FWD
.FEAT2H  ANOP
         STCM  @00,B'1111',@STRDWD-1   STORE IT FOR CVD/UNPK
         LA    @03,8                   OUTPUT LENGTH
         B     @STR376X                EDIT IN HEX
.FEAT3R  ANOP
         AIF   (NOT &$$FEAT(6)).FEAT23B
         B     @STR330                 EDIT R0
.FEAT23B ANOP
.*
.*       BINARY VARIABLE: @03 CONTAINS THE ICM MASK (1 3 7 F)
.*
         AIF   (NOT &$$FEAT(6)).FEAT6B
@STR328M ICM   @00,*-*,0(@02)          **EXECUTED INSTRUCTION**
@STR328  SLR   @00,@00
         EX    @03,@STR328M            LOAD THE BINARY VARIABLE
.FEAT6B  ANOP
.*
         AIF   (NOT &$$FEAT(2) AND NOT &$$FEAT(6)).FEAT6C
@STR330  CVD   @00,@STRDWD             CONVERT VALUE TO DECIMAL
         AIF   (NOT &$$FEAT(4)).FEAT6C
         B     @STR361                 EDIT DWD
.FEAT6C  ANOP
.*
.*       PACKED FIELD
.*
         AIF   (NOT &$$FEAT(4)).FEAT4B
@STRZAP  ZAP   @STRDWD,0(*-*,@02)      MOVE TO @STRDWD
@STR351  LA    @03,0(,@02)             FIRST BYTE OF PACKED FIELD
         BALR  @14,0
         TM    0(@03),X'0C'            IS THIS THE SIGN BYTE?
         LA    @03,1(,@03)              (NEXT BYTE)
         BNOR  @14                     NO, LOOP MORE
         SLR   @03,@02                 GET LENGTH OF PACKED FIELD
         BCTR  @03,0
         EX    @03,@STRZAP             EXECUTE ZAP
         AIF   (NOT &$$FEAT(5)).FEAT4B
         CLI   @STRLEN2,248            JULIAN-TO-YYMMDD CONV?
         BNL   @STR375                 YES, JUMP
.FEAT4B  ANOP                          PACKED
.*
.*       EDIT @STRDWD (BIN, REG, PACKED)
.*
         AIF   (NOT &$$FEAT(8)).FEAT8B
@STR361  IC    @00,@STRLEN2            OUTPUT LENGTH
         LA    @03,X'003F'             MASK FOR "AND"
         NR    @03,@00                 OUTPUT LENGTH
         MVC   @STRWK16(16),@STRMASK   EDIT MASK
.*
.*       LEFT-JUSTIFICATION (NUMERIC)
.*
         AIF   (NOT &$$FEAT(9)).FEAT9B
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?
         BNO   @STR367                 NO, JUMP
         LA    @01,@STRWK16+15         PREVENT BAD R1
         EDMK  @STRWK16(16),@STRDWD    ZONED DECIMAL
         LA    @02,0(,@01)             FIRST STRING POSITION
         LTR   @03,@03                 CHECK OUTPUT LENGTH
         BNZ   @STR363                 JUMP IF NOT ZERO
.*       L0    (LEFT JUSTIFIED, NO PADDING)
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @03,@02                 COMPUTE STRING LENGTH
         B     @STR390                 MOVE FIELD TO OUTPUT LINE
.*       L1-L63 (LEFT JUSTIFIED, PADDING)
@STR363  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN
         BNH   @STR364                 LARGE ENOUGH, JUMP
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.
@STR364  SR    @05,@03                 COMPUTE REMAINING LENGTH
         LR    @00,@04                 POINTER IN OUTPUT LINE
         LR    @01,@03                 LENGTH WITH PADDING
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @03,@02                 COMPUTE STRING LENGTH
         B     @STR392                 MOVE FIELD TO OUTPUT LINE
@STR367  EQU   *
.FEAT9B  ANOP
         AIF   (NOT &$$FEAT(10)).FEAT10B
         TM    @STRLEN2,@STRZERO       LEADING ZEROES REQ'D?
         BNO   @STR368                 NO, JUMP
         MVI   @STRWK16,C'0'           YES, CHANGE X'40' TO C'0'
@STR368  EQU   *
.FEAT10B ANOP
         ED    @STRWK16(16),@STRDWD    ZONED DECIMAL
         LA    @02,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @02,@03                 FIRST STRING POSITION
.FEAT8B  ANOP
.*
.*       LITERAL (@STRSCON IS A 16-BIT OFFSET)
.*       Short Literal (low-order 4 bits of @STRFLAG contains length)
.*       BLANKS  (@STRSCON=ZERO)
.*
         AIF   (NOT &$$FEAT(1)).FEAT1B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR372  LA    @01,7                   mask for NR
         LA    @02,@STRLEN2            1st byte of short literal
         IC    @03,@STRFLAG            pick up flags+length
         NR    @03,@01                 R3 has length of short literal
         BNZ   @STR390                 short literal, go move it
         SLR   @02,@02                 Clear Address Register
         IC    @03,@STRLEN2            GET LITERAL LENGTH
         TM    @STRFLAG,@STRX40        string of spaces?
         BO    @STR390                 yes, go move them
         ICM   @02,B'0011',@STRSCON    LOAD LITERAL OFFSET
         LA    @02,@STRSCON(@02)       CONVERT OFFSET TO FULL ADDRESS
.FEAT1B  ANOP
.*
.*       CONVERT JULIAN DATE TO YYMMDD
.*
         AIF   (NOT &$$FEAT(5)).FEAT5F
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR375  LA    @00,248                 MASK FOR 'SLR'
         SLR   @01,@01
         IC    @01,@STRLEN2            248-255
         SLR   @01,@00                 000-007
         LA    @00,12                  L'@STR375W
         MR    @00,@00                 COMPUTE OFFSET
         LA    @01,@STR375W(@01)       ENTRY IN "TR" MASK TABLE
         SLR   @03,@03
         IC    @03,0(,@01)             LENGTH OF DATE (6, 8 OR 10)
         ZAP   @STRDWD,@STRDWD         DATE=0000000?               @JDT
         BNZ   @STR375B                NO, JUMP                    @JDT
@STR375Z LA    @02,@BLANKS             WORK AREA
&BLANKS  SETA  10                      WE NEED AT LEAST 10 BLANKS
         B     @STR390                 MOVE FIELD TO OUTPUT LINE
@STR375W DC    AL1(10,C'-',0,1,2,3,8,4,5,8,6,7) YYYY-MM-DD   248
         DC    AL1(8,C' ',0,1,2,3,4,5,6,7,8,8)  YYYYMMDD//   249
         DC    AL1(8,C'/',2,3,8,4,5,8,6,7,8,8)  YY/MM/DD//   250
         DC    AL1(8,C'/',6,7,8,4,5,8,2,3,8,8)  DD/MM/YY//   251
         DC    AL1(8,C'/',4,5,8,6,7,8,2,3,8,8)  MM/DD/YY//   252
         DC    AL1(6,C' ',2,3,4,5,6,7,8,8,8,8)  YYMMDD       253
.*       DC    AL1(6,C' ',6,7,4,5,2,3,8,8,8,8)  DDMMYY       254
.*       DC    AL1(6,C' ',4,5,6,7,2,3,8,8,8,8)  MMDDYY       255
@STR375T DC    P'59,31,29,31,30,31,30,31,31,30,31,30,31'
         DC    P'999'                  Prevent S0C7 with 90366
@STR375B CLI   @STRDWD+4,X'01'         YEAR 2000 OR ABOVE?
         BH    @STR375C                CC>01, JUMP (MUST BE 19 OR 20)
         MVI   @STRDWD+4,X'20'         CC=01, CHANGE TO CC=20
         BE    @STR375C                CC=01, USE CC=20
         CLI   @STRDWD+4+1,X'50'       YY<50?
         BL    @STR375C                YES, USE CC=20
         MVI   @STRDWD+4,X'19'         NO, FORCE CC=19
@STR375C UNPK  @STRWK16(5),@STRDWD+4(3) CCYY?
         ZAP   @STRDWD+1(2),@STR375T+9(1) INIT MONTH COUNTER
         LA    @02,@STR375T            TABLE OF MONTHS (NUMBER OF DAYS)
         TM    @STRDWD+4+1,X'01'       ODD YEARS
         BO    @STR375N                  AREN'T LEAP YEARS
         TM    @STRDWD+4+1,X'12'       ZEROES IN 1980, ALL ONES IN 1992
         BNM   @STR375L                MIXED IN 1982/1990
.*       IF IT'S NOT A LEAP YEAR AND DDD>59, THEN ADD 1 TO DDD
@STR375N CP    @STRDWD+4+2(2),@STR375T ARE WE PAST FEB 28 (DDD>59) ?
         BNH   @STR375L                NO, JUMP
         AP    @STRDWD+4+2(2),@STR375T+3(1) ADD 1 (FROM 31) TO DDD
.*--LOOP WHILE DDD > 0
@STR375L AP    @STRDWD+1(2),@STR375T+3(1)   ADD 1 (FROM 31) TO MONTH
         LA    @02,2(,@02)             NEXT ENTRY IN "MONTHS" TABLE
         SP    @STRDWD+4+2(2),0(2,@02) SUB DAYS-IN-MONTH FROM DDD
         BP    @STR375L
.*--ENDLOOP
         AP    @STRDWD+4+2(2),0(2,@02) UNDO LAST "SP" INSTRUCTION
         UNPK  @STRWK16+4(2),@STRDWD+1(2) FYFYFYFY,FMCM??
         UNPK  @STRWK16+6(2),@STRDWD+6(2) FYFYFYFY,FMCMFDCD
         MVZ   @STRWK16+1(7),@STRWK16     FYFYFYFY,FMCMFDCD
         MVC   @STRWK16+8(1),1(@01)    SEPARATOR
         LA    @02,@STRWK16+9          WORK AREA
         MVC   0(10,@02),2(@01)        MOVE CORRESPONDING MASK
         TR    0(10,@02),@STRWK16      CONVERT DATE TO THE RIGHT FORMAT
.FEAT5F  ANOP                          JDATE
.*
.*       HEX STRING
.*
         AIF   (NOT &$$FEAT(7)).FEAT7B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR376M MVC   @STRDWD-1(*-*),0(@02)   PREVENT S0C4 IN UNPK
@STR376  LA    @00,8                   MAX LENGTH
         CLR   @03,@00                 CHECK LENGTH
         BNH   @STR376B                JUMP IF LE 8
         LR    @03,@00                 TRUNCATE TO MAXIMUM LENGTH
@STR376B LR    @01,@03                 INPUT LENGTH
         BCTR  @01,0
         EX    @01,@STR376M            MOVE DATA TO SAFE STORAGE
         ALR   @03,@03                 OUTPUT LENGTH
         AGO   .FEAT37B
.FEAT7B  ANOP
         AIF   (NOT &$$FEAT(3)).FEAT37C
         B     @STR390                 MOVE STRING TO OUTPUT LINE
.FEAT37B ANOP
@STR376X LA    @02,@STRWK16            WORK AREA
         UNPK  0(9,@02),@STRDWD-1(5)   EXPAND SOURCE BYTES FOR "TR"
         UNPK  8(9,@02),@STRDWD+3(5)   EXPAND SOURCE BYTES FOR "TR"
         TR    0(16,@02),@STRHEXT-240  =C'0123456789ABCDEF'
.FEAT37C ANOP
.*
.*       %TIME
.*
         AIF   (NOT &$$FEAT(12)).FEAT12B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STRTIME DC    X'4021207A20207A20207A20204000'    0X.XX.XX.XX
@STR378  LR    @02,@15                 SAVE BASE REG
         TIME  DEC                     GET HHMMSSHH
         LR    @15,@02                 RESTORE BASE REG
         ST    @00,@STRDWD             STORE HHMMSSHH
         MVC   @STRWK16(13),@STRTIME   MOVE EDIT MASK
         ED    @STRWK16(13),@STRDWD    EDIT HH:MM:SS:HH
         LA    @02,@STRWK16+1          WORK AREA
         LA    @03,12                  HH:MM:SS:HH+ SPACE
.FEAT12B ANOP
.*MOVE
@STR390  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN
         BNH   @STR391                 LARGE ENOUGH, JUMP
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.
@STR391  SR    @05,@03                 COMPUTE REMAINING LENGTH
         LR    @00,@04                 POINTER IN OUTPUT LINE
         LR    @01,@03                 PASS REMAINING LENGTH
         LTR   @02,@02                 BLANKS?
         BNZ   @STR392                 NO, JUMP
         SLR   @03,@03                 YES, ZERO LENGTH
@STR392  ICM   @03,B'1000',@BLANKS     PAD WITH BLANKS
         MVCL  @00,@02                 MOVE FIELD TO OUTPUT LINE
         LR    @04,@00                 NEW POINTER IN OUTPUT LINE
@STR398  TM    @STRFLAG,@STRLAST       TEST LAST-ENTRY INDICATOR
         BO    @STR399                 Done, exit
         AIF   (NOT &$$FEAT(1)).FEAT1C
         TM    @STRFLAG,@STRLIT+@STRX40 literal or spaces?
         BM    @STR398L                Literal, not spaces
         BZ    @STR398X                Neither literal nor spaces
         LA    @06,@STRSCON            2-byte entry for blank spaces
         B     @STR310                 PROCESS NEXT ENTRY
@STR398L LA    @01,7                   mask for NR
         IC    @03,@STRFLAG            pick up flags+length
         NR    @03,@01                 R3 has length of short literal
         BZ    @STR398T                not an in-line literal, jump
         LA    @06,@STRLEN2(@03)       Skip VL parm for in-line literal
         B     @STR310                 PROCESS NEXT ENTRY
@STR398T LA    @06,@STRFLEN            4-byte parm for remote literal
         B     @STR310                 PROCESS NEXT ENTRY
.FEAT1C  ANOP
@STR398X LA    @06,@STRNEXT            BUMP UP TO NEXT ENTRY
         B     @STR310                 PROCESS NEXT ENTRY
.*ENDLOOP
.*
.*       END-OF-LINE PROCESSING - PAD WITH BLANKS
.*
@STR399  SLR   @01,@01                 SET UP R1 FOR PADDING
         ICM   @01,B'1000',@BLANKS     SET UP R1 FOR PADDING
.***     DROP  @06,@13,@15
         LA    @14,2                   INCREMENT
         AL    @14,12(,@13)            RETURN ADDRESS
         LR    @15,@04                 CURRENT POINTER IN OUTPUT FIELD
         SL    @15,8(,@13)             CALCULATE LENGTH USED
         MVCL  @04,@00                 PAD WITH BLANKS
         LM    @00,@06,20(@13)         RESTORE WORK REGISTERS
         STM   @06+1,@13-1,48(@13)     MAKE SAVE AREA LOOK NORMAL
         OI    15(@13),1               SIMULATE "T" OPTION OF RETURN
         BR    @14                     RETURN TO CALLER
.*
         AIF   (NOT (&$$FEAT(3) OR &$$FEAT(7))).FEAT37T
@STRHEXT DC    C'0123456789ABCDEF'     HEX-TO-EBCDIC CONVERSION
.FEAT37T ANOP
         AIF   (NOT &$$FEAT(8)).FEAT8T
@STRMASK DC    X'4020202020202020,2020202020202120'
.FEAT8T  ANOP
.**********************************************************************
.*       Convert S-con to address                                     *
.*             Input: GPR2 points to an S-CON in the remote parm list *
.*             Output: GPR2 contains the address                      *
.**********************************************************************
@STRS2A  SLR   @00,@00
         ICM   @00,B'0011',0(@02)      R0 = 0000BDDD
         SRDL  @00,12                  R0 = 0000000B, R1= DDD.....
         SRL   @01,20                  R1 = 00000DDD (DISPLACEMENT)
         CLI   0(@02),@06*16+15        R7-R13?
         BH    @STRS2A3                YES, JUMP
.*BASE REG IS R0-R6
         LTR   @02,@00                 IS R0 THE BASE REG?
         BNZ   @STRS2A2                NO, JUMP
         LTR   @02,@01                 IS THIS A PSA ADDRESS?
         BNZR  @14                     YES, GOBACK
@STRS2A2 SLL   @02,2                   R2= 000000BB BASE * 4
         L     @02,20(@02,@13)         PICK UP BASE REG VALUE
         LA    @02,0(@02,@01)          ADD BASE REG VALUE TO DISPL
         BR    @14
.*BASE REG IS R7-R13
@STRS2A3 LR    @02,@00                 R2= 0000000B (BASE REG NUMBER)
         EX    @02,@STRS2A4            ADD BASE REG VALUE TO DISPL
         BR    @14
@STRS2A4 LA    @02,0(*-*,@01)          ADD BASE REG VALUE TO DISPL
@BLANKS  DC    CL((((*+&BLANKS+7-@STRING)/8)*8)-(*-@STRING))' '
@STRSIZE EQU   *-@STRING               SIZE OF GENERATED CSECT
         DROP  @06,@13,@15
.**********************************************************************
.*       WORK AREA (CALLER'S SAVE AREA)                               *
.**********************************************************************
@STRSAVE DSECT                         24-BYTE WORK AREA
         DS    A(0,@STRSAVE,@STRSAVE,14,15,0,1,2,3,4,5,6)
@STRWK16 DS    F'7,8,9,10'             WORK AREA
@STRDWD  DS    D'1112'                 WORK AREA
@STRPARM DSECT
@STRFLAG DS    B                   +0  FORMAT, FLAGS
@STRLAST EQU   X'80'                     LAST ENTRY
@STRLIT  EQU   X'40'                     LITERAL, @STRSCON IS AN OFFSET
@STRX40  EQU   X'20'                   String of Spaces
.*             X'0F'                   CONVERSION REQUIRED
.*                                     or length of short literal
@STRHEX  EQU   X'08'                     HEXADECIMAL
@STRBIN  EQU   X'04'                     BINARY
@STRPACK EQU   X'02'                     PACKED
@STRREG  EQU   X'01'                     REGISTER
@STRLEN2 DS    B                   +1  FORMAT, OUTPUT LENGTH
.*                                     or start of short literal
@STRLEFT EQU   X'80'                     LEFT JUSTIFICATION
@STRZERO EQU   X'40'                     LEADING ZEROES
.*             X'3F'                     OUTPUT LENGTH, 0 MEANS TRUNC.
@STRSCON DS    S                   +2  FIELD ADDRESS
@STRFLEN DS    S                   +4  FIELD LENGTH
@STRNEXT EQU   *                   +6
.MEND    AIF   ('&PRINT' EQ 'NOGEN').MEND99
         POP   PRINT
.MEND99  MEND
./ ADD NAME=STRINGOS
         MACRO
&NAME    STRING &INTO=,&PRINT=NOGEN
         GBLC  &STRING_MACRO_VERSION
&STRING_MACRO_VERSION SETC '513'       current version
.**********************************************************************
.*                                                                    *
.*MACRO NAME = STRING                                                 *
.*                                                                    *
.*DESCRIPTIVE NAME = STRING Macro Instruction.                        *
.*                                                                    *
.*FUNCTION = Provide capabilities similar to PUT EDIT (of PL/I)       *
.*           or STRING (of COBOL) to assembler programs.              *
.*                                                                    *
.*STATUS = R513                                                       *
.*                                                                    *
.*AUTHOR = Gilbert Saint-flour <gsf@pobox.com>                        *
.*                                                                    *
.*ENVIRONMENT = SEE BELOW                                             *
.*                                                                    *
.*    AMODE = ANY                                                     *
.*    SCP   = OS/360, OS/VS, MVS/370, MVS/XA, MVS/ESA, OS/390 or z/OS *
.*    KEY   = ANY                                                     *
.*    MODE  = ANY                                                     *
.*    APF   = OFF                                                     *
.*                                                                    *
.*OPERATION = SEE DOCUMENTATION AT THE END OF THIS FILE               *
.*                                                                    *
.*INVOCATION = SEE DOCUMENTATION AT THE END OF THIS FILE              *
.*                                                                    *
.*NOTES = SEE DOCUMENTATION AT THE END OF THIS FILE                   *
.*                                                                    *
.*CHANGE ACTIVITY                                                     *
.*                                                                    *
.* $301  FIX BUGS WITH (XYZ,,L8) AND INTO=((R1),(R2))                 *
.* $302  GENERATE $DEBUG BOOT-STRAP IN FINAL_CALL INVOCATION          *
.* $303  REPLACE @STRPAD WITH @STRBLANKS                              *
.* $304  USE @00-@15 INSTEAD OF R0-R15 FOR REGISTER EQUATES           *
.* $306  TAILOR @STRING CSECT TO PROGRAM'S REQUIREMENTS               *
.*       LITERALS CAN BE CODED AS 'ABC' OR C'ABC' OR X'C1C2C3'        *
.*       USE L'PSATOLD AS IMPLICIT LENGTH FOR (PSATOLD-PSA,,X)        *
.* $307  FINAL_CALL OPTION CHANGED TO GENERATE                        *
.*       ADD NOCSECT AND LOCTR OPTIONS TO GENERATE CALL               *
.* $308  USE L'RBCDE AS IMPLICIT LENGTH FOR (PRB.RBCDE,,X)            *
.*       PREVENT S0C4 WHEN ADDR IS BAD AND LENGTH IS ZERO             *
.* $400  REORG THE CODE TO SIMPLIFY FEATURE SELECTION                 *
.*       BLANKS NO LONGER USED OR GENERATED                           *
.* $401  GENERATE @STRHEXT WHEN ((REG),,X) ONLY HEX FIELD             *
.* $502  @STRING ROUTINE REWRITTEN FOR MVS/ESA:                       *
.*       -  USE LINKAGE STACK TO STORE CALLER'S REGISTERS             *
.*       -  ADD SUPPORT FOR AR MODE                                   *
.*       -  @STRING now executes in caller's AMODE                    *
.*       -  Rename previous version to STRINGXA                       *
.* $503  DATE CONVERSION TO YYYY-MM-DD FORMAT (ISO STANDARD)          *
.*       IMPROVE SUPPORT FOR AR MODE                                  *
.* $504  SET &STRBLANKS TO 10 WHEN PROCESSING DATES                   *
.* $505  Allow for 128K-offset                                        *
.* $506  Remove literal from BAL instruction for HLASM R3             *
.* $507  Merge STRINGXA code, add AR_MODE option                      *
.*       Change syntax of GENERATE call                               *
.* $508  Compatibility with FLAG(PAGE0) in HLASM R3                   *
.*       Compatibility with pre-XA version of the SAVE macro          *
.*       Remove AMODE-based R2 cleanup in 370 mode                    *
.* $509  Length of parm-list entries can vary between 2 and 6 bytes   *
.*       Short Literals (one to five bytes) are generated in parm list*
.*       Hex string can contain commas, e.g. X'12,3456,7890'          *
.* $510  JDATE=90366 produces 90/13/01 instead of S0C7                *
.* $511  Prevent ACTR error with long literals                        *
.* $512  Packed fields no longer ignored when R0=0                    *
.* $513  Local base not needed for (GENERATE,,LOCTR)                  *
.**********************************************************************
         AIF   ('&PRINT' EQ 'NOGEN').NOGEN
         PUSH  PRINT
         PRINT GEN
.NOGEN   GBLA  &$_LIT
         GBLB  &$_FEAT(16)             FEATURES
.*                                       1 LITERALS
.*                                       2 REGISTER (BIN)
.*                                       3 REGISTER (HEX)
.*                                       4 PACKED
.*                                       5 JDATE
.*                                       6 BINARY
.*                                       7 HEX
.*                                       8 NUMERIC
.*                                       9 LEFT JUST (NUMERIC)
.*                                       10 LEADING ZEROES
.*                                       11 TRUNCATE (CHAR STRING)
.*                                       12 %TIME
         GBLC  &$_LITS(9999)           LITERALS
         LCLA  &I,&J,&N
         AIF   (T'&INTO EQ 'O' AND N'&SYSLIST EQ 1                     X
               AND '&SYSLIST(1,1)' EQ 'GENERATE').GENL
&LABEL   SETC  'IHB&SYSNDX'            STEM FOR LOCAL LABELS
&LQ      SETC  'L'''                   LENGTH ATTRIBUTE
&STR     SETC  '  R&STRING_MACRO_VERSION'
&NAME    BAS   R14,$STRING&STR         CALL @STRING SUB-ROUTINE
         AIF   (N'&SYSLIST EQ 0).ERR1  NO POSITIONAL OPERANDS, ERROR
         AIF   (T'&INTO EQ 'O').ERR2   NO RECEIVING FIELD, ERROR
         AIF   (N'&INTO GT 2).ERR2     INTO=(A,32,BLURB)
         DC    AL2((&LABEL.P-*)/2)     OFFSET TO FIELD DESCRIPTORS
         AIF   (D'$LITERAL).LOCTR2     NOT FIRST TIME, JUMP
$LTORG   LOCTR                         ADDRESSABLE CONSTANTS
$FARRTNE LOCTR                         FAR ROUTINES
.LOCTR2  ANOP
$LITERAL LOCTR                         NON-ADDRESSABLE CONSTANTS
&TO1     SETC  '&INTO(1)'
&TO2     SETC  '&LQ&INTO'
         AIF   (N'&INTO EQ 1).PUNTO8            JUMP IF INTO=XXX
         AIF   ('&INTO(1)'(1,1) NE '(').PUNTO3  JUMP IF INTO=(XXX,44)
&TO1     SETC  '0&INTO(1)'                              INTO=((R3),44)
.PUNTO3  ANOP
&TO2     SETC  '&INTO(2)'                               INTO=(XXX,LL)
         AIF   ('&INTO(2)'(1,1) NE '(').PUNTO8  JUMP IF INTO=(XXX,44)
&TO2     SETC  '0&INTO(2)'                              INTO=(XXX,(R1))
.PUNTO8  ANOP
&LABEL.P DC    S(&TO1,&TO2)
.*--------------------------------------------------------------------*
.*-------      FIELDS       ------------------------------------------*
.*--------------------------------------------------------------------*
         LCLB  &LAST,&BIN,&HEX,&REG,&PACKED,&LEFT,&ZERO,&TRUNC
&I       SETA  1
.*LOOP
.LOOP1   ANOP
         ACTR  200                            SYSDEBUG/DIAG055
         AIF   (N'&SYSLIST(&I) GT 3).FLD990   TOO MANY SUB-OPERANDS
&LAST    SETB  (&I EQ N'&SYSLIST)                LOOP
         AIF   ('&SYSLIST(&I)'(1,1) EQ '''').LIT00
         AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) EQ '''').LIT01
.*--------------------------------------------------------------------*
.*       PROCESS FIRST SUBPARAMETER (ADDRESS)                         *
.*--------------------------------------------------------------------*
&P1S     SETC  '&SYSLIST(&I,1)'
&P2L     SETC  '0'                     INPUT LENGTH
&P3L     SETC  '0'                     OUTPUT LENGTH
         AIF   ('&SYSLIST(&I)'(1,1) GE '0').FLD180 SPACES
         AIF   ('&SYSLIST(&I)' EQ '%TIME').FLD190 %TIME
         AIF   ('&SYSLIST(&I,1)'(1,1) NE '(').FLD115 (R2)
         AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD250
&P1S     SETC  '0&SYSLIST(&I,1)'       CHANGE (R1) TO 0(R1)
.FLD115  ANOP
.*
.*       EXTRACT RBCDE FROM PRB.RBCDE (HLASM)
.*
&L       SETA  1
.*--LOOP
.FLD131  AIF   ('&P1S'(&L,1) EQ '.').FLD133
&L       SETA  &L+1
         AIF   (&L LT K'&P1S).FLD131
.*--ENDLOOP
&P2L     SETC  '&P1S'                    ABCDEF FROM ABCDEF
         AGO   .FLD134
.FLD133  ANOP
&P2L     SETC  '&P1S'(&L+1,K'&P1S-&L)    RBCDE FROM PRB.RBCDE
.FLD134  ANOP
.*
         AIF   (T'&SYSLIST(&I,2) NE 'O').FLD200
.*
         AIF   (NOT D'&P2L).FLD140
&P2C     SETC  T'&P2L
.*MNOTE *,'&P1 &P2C'
         AIF   ('&P2C' EQ 'F' OR '&P2C' EQ 'H' OR '&P2C' EQ 'P').FLD220
         AIF   ('&P2C' EQ 'G').FLD210  FL2
.FLD140  ANOP
.*
.*       EXTRACT PSATOLD FROM PSATOLD-PSA
.*
&L       SETA  1
.*--LOOP
.FLD141  AIF   ('&P2L'(&L,1) EQ '-').FLD143
         AIF   ('&P2L'(&L,1) EQ '+').FLD143
&L       SETA  &L+1
         AIF   (&L LT K'&P2L).FLD141
.*--ENDLOOP
&P2L     SETC  '&LQ&P2L'               L'ABCDEF
         AGO   .FLD300
.FLD143  ANOP
&P2L     SETC  '&LQ'.'&P2L'(1,&L-1)    L'PSATOLD FROM PSATOLD-PSA
         AGO   .FLD300
.*
.FLD180  AIF   ('&SYSLIST(&I)'(K'&SYSLIST(&I),1) NE 'X').FLD800
&P2L     SETC  '&SYSLIST(&I)'(1,K'&SYSLIST(&I)-1) 12
         AIF   (&LAST).FLD186
         DC    X'60',AL1(&P2L)         BLANKS
         AGO   .LIT90
.FLD186  DC    X'E0',AL1(&P2L),0S(0)   BLANKS
         AGO   .LIT90
.*
.FLD190  ANOP                          %TIME
&P1S     SETC  '1(14)'                 %TIME
&$_FEAT(12) SETB 1                     %TIME
         AGO   .FLD800
.*--------------------------------------------------------------------*
.*       PROCESS SECOND SUBPARAMETER (LENGTH/TYPE)                    *
.*--------------------------------------------------------------------*
.FLD200  AIF   (T'&SYSLIST(&I,2) EQ 'O').FLD300 NO LENGTH SPECIFIED
&P2C     SETC  '&SYSLIST(&I,2)'
         AGO   .FLD220
.*T'&P1=G
.FLD210  ANOP
&L       SETA  L'&SYSLIST(&I)          T'&P1 = 'G'
&P2C     SETC  'FL&L'                  T'&P1 = 'G'
.*
.FLD220  ANOP
&P2L     SETC  '0&P2C'                 (R2) LENGTH
         AIF   ('&P2C'(1,1) EQ '(').FLD300
&P2L     SETC  '&P2C'                  3(R2) LENGTH
         AIF   ('&P2C'(K'&P2C,1) EQ ')').FLD300
&P2L     SETC  '0'
&PACKED  SETB  ('&P2C' EQ 'P')
         AIF   (&PACKED).FLD290
&P2L     SETC  '1'
         AIF   ('&P2C' EQ 'FL1').FLD240
&P2L     SETC  '3'
         AIF   ('&P2C' EQ 'FL2' OR '&P2C' EQ 'H').FLD240
&P2L     SETC  '7'
         AIF   ('&P2C' EQ 'FL3').FLD240
&P2L     SETC  '15'
         AIF   ('&P2C' EQ 'F').FLD240
&P2L     SETC  '&P2C'                  IMMEDIATE LENGTH, FIELD
         AGO   .FLD300
.*
.FLD240  ANOP                          BINARY VARIABLE
&BIN     SETB  1
         AGO   .FLD300
.*
.FLD250  ANOP                          REGISTER CONTENT
&REG     SETB  1
         AGO   .FLD300
.*
.FLD290  ANOP                          PACKED
&P2L     SETC  '1'
.*--------------------------------------------------------------------*
.*       PROCESS THIRD SUBPARAMETER (OUTPUT FORMAT)                   *
.*--------------------------------------------------------------------*
.FLD300  AIF   (T'&SYSLIST(&I,3) EQ 'O').FLD800
&HEX     SETB  ('&SYSLIST(&I,3)' EQ 'X') HEXADECIMAL
&TRUNC   SETB  ('&SYSLIST(&I,3)' EQ 'T') TRUNCATE
         AIF   (&HEX OR &TRUNC).FLD800
.*
&P3C     SETC  '&SYSLIST(&I,3)'
&P3L     SETC  '248'
         AIF   ('&P3C' EQ 'YYYY-MM-DD' AND &PACKED).FLD308
&P3L     SETC  '249'
         AIF   ('&P3C' EQ 'YYYYMMDD' AND &PACKED).FLD308
&P3L     SETC  '250'
         AIF   ('&P3C' EQ 'YY/MM/DD' AND &PACKED).FLD308
&P3L     SETC  '251'
         AIF   ('&P3C' EQ 'DD/MM/YY' AND &PACKED).FLD308
&P3L     SETC  '252'
         AIF   ('&P3C' EQ 'MM/DD/YY' AND &PACKED).FLD308
&P3L     SETC  '253'
         AIF   ('&P3C' EQ 'YYMMDD'   AND &PACKED).FLD308
&P3L     SETC  '0'
         AIF   (T'&SYSLIST(&I,2) NE 'N').FLD310
         MNOTE 8,'EDIT PATTERN NOT ALLOWED WITH CHARACTER STRING'
         AGO   .FLD310
.FLD308  ANOP
&$_FEAT(5) SETB 1                      JDATE
         AGO   .FLD800
.*--LOOP
.FLD310  AIF   ('&P3C'(1,1) EQ 'R').FLD318       DEFAULT
         AIF   ('&P3C'(1,1) EQ 'B').FLD318       DEFAULT
         AIF   ('&P3C'(1,1) NE 'L').FLD311
&LEFT    SETB  1
         AGO   .FLD318
.FLD311  AIF   ('&P3C'(1,1) NE 'Z').FLD312
&ZERO    SETB  1
         AGO   .FLD318
.FLD312  AIF   ('&P3C'(1,1) LT '0').FLD993
&P3L     SETC  '&P3L'.'&P3C'(1,1)
.FLD318  ANOP
.*MNOTE *,'&SYSLIST(&I) P3C=/&P3C/ P3L=/&P3L/'
&P3C     SETC  '&P3C'(2,K'&P3C-1)     STRIP OFF FIRST CHARACTER
         AIF   (K'&P3C GT 0).FLD310
.*--ENDLOOP
.*--------------------------------------------------------------------*
.FLD800  ANOP
&NUMERIC SETB  (&BIN OR &PACKED OR (&REG AND NOT &HEX))
&TRUNC   SETB  (&TRUNC OR (&LEFT AND NOT &NUMERIC))
&LEFT    SETB  (&LEFT AND &NUMERIC)
         AIF   (NOT &NUMERIC).FLD810
         AIF   (&LEFT OR '&P3L' NE '0').FLD810
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH ((R3))
         AIF   (&REG).FLD810
&P3L     SETC  '3'                     DEFAULT OUTPUT LENGTH
         AIF   ('&P2C' EQ 'FL1').FLD810
&P3L     SETC  '5'                     DEFAULT OUTPUT LENGTH
         AIF   ('&P2C' EQ 'H' OR '&P2C' EQ 'FL2').FLD810
&P3L     SETC  '7'                     DEFAULT OUTPUT LENGTH
.FLD810  ANOP
&FLAG    SETA  &HEX*8+&BIN*4+&PACKED*2+&REG*1
&LEN2    SETA  &TRUNC*128+&LEFT*128+&ZERO*64+&P3L
&$_FEAT(2) SETB (&$_FEAT(2) OR (&REG AND NOT &HEX))   REGISTER (BIN)
&$_FEAT(3) SETB (&$_FEAT(3) OR (&REG AND &HEX))       REGISTER (HEX)
&$_FEAT(4) SETB (&$_FEAT(4) OR &PACKED)               PACKED
&$_FEAT(6) SETB (&$_FEAT(6) OR &BIN)                  BINARY
&$_FEAT(7) SETB (&$_FEAT(7) OR (&HEX AND NOT &REG))   HEX
&$_FEAT(8) SETB (&$_FEAT(8) OR &NUMERIC)              BIN,PACKED
&$_FEAT(9) SETB (&$_FEAT(9) OR (&LEFT AND &NUMERIC))
&$_FEAT(10) SETB (&$_FEAT(10) OR &ZERO)
&$_FEAT(11) SETB (&$_FEAT(11) OR &TRUNC)
&BIN     SETB  0                    RESET FLAGS
&HEX     SETB  0                    RESET FLAGS
&REG     SETB  0                    RESET FLAGS
&PACKED  SETB  0                    RESET FLAGS
&LEFT    SETB  0                    RESET FLAGS
&ZERO    SETB  0                    RESET FLAGS
&TRUNC   SETB  0                    RESET FLAGS
         AIF   (&FLAG GE 10).FLD995
         AIF   (&LAST).FLD816
         DC    X'0&FLAG',AL1(&LEN2),SL2(&P1S,&P2L)
         AGO   .LIT99
.FLD816  DC    X'8&FLAG',AL1(&LEN2),SL2(&P1S,&P2L),0S(0)
         AGO   .LIT99
.FLD990  MNOTE 12,'OPERAND &I HAS TOO MANY SUB-OPERANDS'
         AGO   .LIT99
.FLD993  MNOTE 8,'THIRD SUBPARAMETER IS INVALID: ''&SYSLIST(&I,3)'''
         AGO   .LIT99
.FLD995  MNOTE 8,'Invalid Combination of Attributes: &SYSLIST(&I)'
         AGO   .LIT99
.*--------------------------------------------------------------------*
.*------------ LITERALS ----------------------------------------------*
.*--------------------------------------------------------------------*
.LIT00   ANOP
&LIT     SETC  'C&SYSLIST(&I)'
         AGO   .LIT09
.LIT01   ANOP
&LIT     SETC  '&SYSLIST(&I)'
.LIT09   ANOP                              calculate length of literal
&J       SETA  3
&L       SETA  0
         ACTR  K'&LIT+K'&LIT+100
         AIF   ('&LIT'(1,1) EQ 'X').LIT11X
.*LOOP
.LIT11C  AIF   ('&LIT'(&J,1) NE '''' AND '&LIT'(&J,1) NE '&&').LIT12C
&J       SETA  &J+1
.LIT12C  ANOP
&J       SETA  &J+1
&L       SETA  &L+1
         AIF   (&J LT K'&LIT).LIT11C
.*ENDLOOP
         AGO   .LIT15
.*LOOP
.LIT11X  AIF   ('&LIT'(&J,1) EQ ',').LIT12X
&L       SETA  &L+1
.LIT12X  ANOP
&J       SETA  &J+1
         AIF   (&J LT K'&LIT).LIT11X
.*ENDLOOP
&L       SETA  (&L+1)/2
.LIT15   ANOP                               generate in-line literal
         AIF   (&L GT 5).LIT40
         AIF   (&LAST).LIT16
         DC    X'4&L',&LIT
         AGO   .LIT90
.LIT16   DC    X'C&L',&LIT,0S(0)
         AGO   .LIT90
.LIT40   ANOP                                  check literal table
         AIF   (&$_LIT EQ 0).LIT50
&N       SETA  1
         ACTR  &$_LIT*3+200
.LIT41   AIF   ('&LIT' EQ '&$_LITS(&N)').LIT80       LOOP
&N       SETA  &N+1                                  LOOP
         AIF   (&N LE &$_LIT).LIT41                  LOOP
.LIT50   ANOP
&$_LIT   SETA  &$_LIT+1
&$_LITS(&$_LIT) SETC '&LIT'
&N       SETA  &$_LIT
.LIT80   ANOP                               generate remote literal
&N       SETA  &N+1000
         AIF   (&LAST).LIT86
         DC    X'40',AL1(&L),AL2($LIT&N-*)
         AGO   .LIT90
.LIT86   DC    X'C0',AL1(&L),AL2($LIT&N-*),0S(0)
.LIT90   ANOP
&$_FEAT(1) SETB 1                      LITERAL
.LIT99   ANOP
.*--------------------------------------------------------------------*
&I       SETA  1+&I                              LOOP
         AIF   (&I LE N'&SYSLIST).LOOP1          LOOP
.*ENDLOOP
&SYSLOC  LOCTR
         AGO   .MEND
.ERR1    MNOTE 12,'AT LEAST ONE INPUT FIELD MUST BE SPECIFIED'
         AGO   .MEND
.ERR2    MNOTE 12,'INVALID OUTPUT AREA SPECIFICATION'
         AGO   .MEND
.**********************************************************************
.*       GENERATE: GENERATE LITERALS AND PROCESSING ROUTINE           *
.**********************************************************************
.GENL    ANOP
         AIF   (N'&SYSLIST(1) GT 3).GENL1R
&OPT2    SETC  '&SYSLIST(1,2)'
         AIF   ('&OPT2' NE '' AND '&OPT2' NE 'AR_MODE'                 X
               AND '&OPT2' NE 'NO_CSECT').GENL1R
&OPT3    SETC  '&SYSLIST(1,3)'
         AIF   ('&OPT3' NE '' AND '&OPT3' NE 'LOCTR').GENL1R
         AIF   ('&OPT2' EQ 'NO_CSECT' AND '&OPT3' EQ 'LOCTR').GENL1R
         AGO   .GENL1D
.GENL1R  MNOTE 12,'Invalid GENERATE options, default used'
&OPT2    SETC  ''
&OPT3    SETC  ''
.GENL1D  ANOP
&STRBLANKS SETA 2
         AIF   (D'$LTORG).GENL1F             CSECT-ONLY
&$_FEAT(1) SETB 1,1,1,1,1,1,1,1,1,1,1,1
.GENL1F  ANOP
&ALLFEAT SETC  '&$_FEAT(1)&$_FEAT(2)&$_FEAT(3)&$_FEAT(4)'
&ALLFEAT SETC  '&ALLFEAT&$_FEAT(5)&$_FEAT(7)&$_FEAT(6)&$_FEAT(8)'
&ALLFEAT SETC  '&ALLFEAT&$_FEAT(9)&$_FEAT(10)&$_FEAT(11)&$_FEAT(12)'
&STR     SETC  '&STRING_MACRO_VERSION'
         MNOTE *,'STRING R&STR - FEATURES GENERATED: &ALLFEAT'
         AIF   (&$_LIT EQ 0).GENL3
$LITERAL LOCTR
.GENL2   ANOP                                LOOP
&N       SETA  &N+1                          LOOP
&I       SETA  &N+1000                       LOOP
$LIT&I   DC    &$_LITS(&N)
         AIF   (&N LT &$_LIT).GENL2          LOOP
.GENL3   ANOP
         AIF   ('&OPT2' EQ 'NO_CSECT').GENL8
&ARMODE  SETB  ('&OPT2' EQ 'AR_MODE')
.**********************************************************************
.*                                                                    *
.*       STRING SUB-ROUTINE                                           *
.*                                                                    *
.*             CAUTION: BYTES 49-72 OF THE CALLER'S SAVE AREA         *
.*                      (R7-R12 SLOTS) ARE USED AS WORK SPACE         *
.*                                                                    *
.**********************************************************************
         AIF   ('&OPT3' EQ 'LOCTR').GENL4L
@STRING  CSECT
@STRING  RMODE ANY
         AGO   .GENL4X
.GENL4L  PUSH  USING                   GENERATE,LOCTR
         DROP
@STRING@ LOCTR
@STRING  DS    0H                      ALIGNMENT
.GENL4X  ANOP
@00      EQU   0                       WORK REGISTER
@01      EQU   1                       WORK REGISTER
@02      EQU   2                       WORK REGISTER
@03      EQU   3                       WORK REGISTER
@04      EQU   4                       WORK REGISTER
@05      EQU   5                       WORK REGISTER
@06      EQU   6                       WORK REGISTER
         AIF   (NOT &ARMODE).ARMODE1X
@07      EQU   7                       WORK REGISTER
@08      EQU   8                       WORK REGISTER
@09      EQU   9                       WORK REGISTER
@10      EQU   10                      WORK REGISTER
@11      EQU   11                      WORK REGISTER
@12      EQU   12                      WORK REGISTER
.ARMODE1X ANOP
@13      EQU   13                      CALLER'S SAVE AREA
@14      EQU   14                      WORK REGISTER
@15      EQU   15                      BASE REG
         USING @STRING,@15
         USING @STRSAVE,@13
         B     @STR003                 BRANCH AROUND EYE-CATCHER
         AIF   (&ARMODE).ARMODE2Y
         DC    C'@STRING/370 R&STRING_MACRO_VERSION &ALLFEAT',0H'0'
@STR003  STM   @14,@06,12(@13)         Save caller's registers
         SLR   @06,@06                 R6=0
&LAE     SETC  'LA'
         AGO   .ARMODE2X
.ARMODE2Y ANOP
         DC    C'@STRING/390 R&STRING_MACRO_VERSION &ALLFEAT',0H'0'
@STR002  DC    H'2'
@STR003  STM   @14,@12,12(@13)         SAVE REGS (DEBUGGING ONLY)
.*+++    BSM   @14,0                   Set AMODE when invoked via BAL
         AH    @14,@STR002-@STRING(@15,0) POINT PAST PARM-LIST OFFSET
         BAKR  @14,0                   SAVE REGS AND RETURN ADDRESS
         BCTR  @14,0                   ADJUST ADDR OF PARM LIST OFFSET
         BCTR  @14,0                   ADJUST ADDR OF PARM LIST OFFSET
         LAE   @06,0                   R6=0 AR6=0
         CPYA  @14,@06                 AR14=0
         CPYA  @15,@06                 AR15=0
&LAE     SETC  'LAE'
.ARMODE2X ANOP
         ICM   @06,B'0011',0(@14)      PICK UP PARM-LIST 1/2 OFFSET
         ALR   @06,@06                 PICK UP PARM-LIST OFFSET
         ALR   @06,@14                 R6 NOW POINTS TO PARM LIST
         USING @STRSCON,@06
         &LAE  @02,@STRSCON            RESOLVE FIRST SCON (ADDR)
         BAL   @14,@STRS2A             GET ADDRESS IN R2
         &LAE  @04,0(,@02)             KEEP ADDRESS OF "INTO" FIELD
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO
         OI    0(@04),0                GET CLEAN S0C4 IF ADDRESS IS BAD
         &LAE  @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)
         BAL   @14,@STRS2A             GET LENGTH IN R2
         LR    @05,@02                 KEEP LENGTH OF "INTO" FIELD
         CR    @05,@04                 END ADDRESS?
         BL    @STR282                 NO, JUMP
         SR    @05,@04                 CALCULATE LENGTH
         AIF   (NOT &ARMODE).ARMODE5N
@STR282  MSTA  @04                     SAVE R4,R5 ON LINKAGE STACK
.*SLAC   DC    X'B247,0040'            SLAC assembler error
         AGO   .ARMODE5X
.ARMODE5N ANOP
@STR282  ST    @04,8(,@13)             SAVE ADDRESS FOR LATER
.ARMODE5X ANOP
.**********************************************************************
.*       MOVE FIELDS TO OUTPUT AREA                                   *
.**********************************************************************
         &LAE  @06,@STRNEXT            POINT TO 1ST FIELD DESC
         USING @STRPARM,@06
.*LOOP
@STR310  EQU   *
         AIF   (NOT &$_FEAT(1)).FEAT1A
         TM    @STRFLAG,@STRLIT        IS THIS A LITERAL?
         BO    @STR372                 YES, JUMP
.FEAT1A  ANOP
         AIF   (NOT &$_FEAT(2) AND NOT &$_FEAT(3)).FEAT23A
         TM    @STRFLAG,@STRREG        REGISTER?
         BO    @STR323                 YES, JUMP
.FEAT23A ANOP
         AIF   (NOT &$_FEAT(12)).FEAT12A
         CLI   @STRSCON,X'E0'          IS IT %TIME ?
         BE    @STR378                 YES, JUMP
.FEAT12A ANOP
.*
.*       IT'S A FIELD (SCON)
.*
         &LAE  @02,@STRFLEN            RESOLVE SECOND S-CON (LEN)
         BAL   @14,@STRS2A             GET LENGTH IN R2
         AIF   (&$_FEAT(9)).FEAT9E1    (LEFT JUSTIFICATION, NUMERIC)
         LTR   @03,@02                 KEEP/TEST LENGTH
         BZ    @STR398                 ZERO LENGTH, DO NOT EDIT
         AGO   .FEAT9E2
.FEAT9E1 LTR   @03,@02                 KEEP/TEST LENGTH
         BP    @STR313                 LENGTH POSITIVE, JUMP
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?
         BNO   @STR398                 NO, ZERO LENGTH IS NOT OK
@STR313  EQU   *
.FEAT9E2 ANOP
         &LAE  @02,@STRSCON            RESOLVE FIRST SCON (ADDR)
         BAL   @14,@STRS2A             GET ADDRESS IN R2
         L     @14,12(,@13)            RESTORE ADDR OF STRING MACRO
         CLI   0(@02),0                GET CLEAN S0C4 IF ADDRESS IS BAD
         CR    @03,@02                 END ADDRESS?
         BL    @STR314                 NO, JUMP
         LA    @00,X'0080'             PSA ADDRESS
         CLR   @02,@00                 PSA REFERENCE?
         BL    @STR314                 YES, JUMP
         SR    @03,@02                 CALCULATE LENGTH
@STR314  EQU   *
         AIF   (NOT &$_FEAT(6)).FEAT6A
         TM    @STRFLAG,@STRBIN        BINARY FIELD?
         BO    @STR328                 YES, JUMP
.FEAT6A  ANOP
         AIF   (NOT &$_FEAT(4)).FEAT4A
         TM    @STRFLAG,@STRPACK       PACKED FIELD?
         BO    @STR351                 YES, JUMP
.FEAT4A  ANOP
         AIF   (NOT &$_FEAT(7)).FEAT7A
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?
         BO    @STR376                 YES, JUMP
.FEAT7A  ANOP
.*
.*       TRUNCATE CHARACTER STRING
.*
         AIF   (NOT &$_FEAT(11)).FEAT11A
         CLI   @STRLEN2,@STRLEFT       CHECK JUSTIFICATION, OUTPUT LEN
         BNE   @STR390                 NO STRING TRUNCATION, JUMP
         &LAE  @01,0(@03,@02)          FIRST BYTE AFTER FIELD
@STR318  BCTR  @01,0                   DOWN 1 BYTE                 LOOP
         CLI   0(@01),C' '             IS IT A SPACE ?             LOOP
         BNE   @STR390                 LAST NON-BLANK BYTE         LOOP
         BCT   @03,@STR318             LOOP UNTIL 1ST NON-BLANK    LOOP
         B     @STR398                 BLANK FIELD, DO NOT EDIT
         AGO   .FEAT11B
.FEAT11A ANOP
         AIF (&$_FEAT(2)+&$_FEAT(3)+&$_FEAT(4)+&$_FEAT(6) EQ 0).FEAT11B
         B     @STR390                 EDIT
.FEAT11B ANOP
.*
.*       REGISTER (R0-R13)
.*
         AIF   (NOT &$_FEAT(2) AND NOT &$_FEAT(3)).FEAT23B
         AIF   (NOT &ARMODE).ARMODE7N
@STR323  LH    @01,@STRSCON            REG NUMBER IN BITS 28-31
         CLI   @STRSCON+1,@06          IS THIS R7-R13?
         BH    @STR323R                YES, JUMP
         LR    @07,@01                 SAVE R1 (R)
         LAE   @10,0(,@04)             SAVE R4
         LR    @11,@05                 SAVE R5
         LAE   @12,0(,@06)             SAVE R6
         EREG  @00,@06                 RELOAD CALLER'S R0-R6
         EX    @07,@STR323L            COPY R0-R6 VALUE
         LAE   @04,0(,@10)             RESTORE R4
         LR    @05,@11                 RESTORE R5
         LAE   @06,0(,@12)             RESTORE R6
         B     @STR323T
@STR323L LR    @00,*-*                 COPY VALUE FOR EDIT
@STR323R EQU   *
         EREG  @07,@12                 RESTORE CALLER'S R7-R12
         EX    @01,@STR323L            COPY R7-R13 INTO R0
         AGO   .ARMODE7X
.ARMODE7N ANOP
@STR323L LR    @00,*-*                 COPY VALUE FOR EDIT
@STR323  LH    @01,@STRSCON            REG NUMBER IN BITS 28-31
         EX    @01,@STR323L            COPY R7-R13 INTO R0
         CLI   @STRSCON+1,@06+1        IS THIS R7-R13?
         BNL   @STR323T                YES, JUMP
         SLL   @01,2                   R1= 000000BB BASE * 4
         L     @00,20(@01,@13)         PICK UP VALUE FOR R0-R6
.ARMODE7X ANOP
@STR323T EQU   *
         AIF   (NOT &$_FEAT(3)).FEAT3R REG,HEX
         AIF   (NOT &$_FEAT(2)).FEAT2H REG,BIN
         TM    @STRFLAG,@STRHEX        EDIT IN HEX?
         BNO   @STR330                 NO, EDIT FWD
.FEAT2H  ANOP
         STCM  @00,B'1111',@STRDWD-1   STORE IT FOR CVD/UNPK
         LA    @03,8                   OUTPUT LENGTH
         B     @STR376X                EDIT IN HEX
.FEAT3R  ANOP
         AIF   (NOT &$_FEAT(6)).FEAT23B
         B     @STR330                 EDIT R0
.FEAT23B ANOP
.*
.*       BINARY VARIABLE: @03 CONTAINS THE ICM MASK (1 3 7 F)
.*
         AIF   (NOT &$_FEAT(6)).FEAT6B
@STR328M ICM   @00,*-*,0(@02)          **EXECUTED INSTRUCTION**
@STR328  SLR   @00,@00
         EX    @03,@STR328M            LOAD THE BINARY VARIABLE
.FEAT6B  ANOP
.*
         AIF   (NOT &$_FEAT(2) AND NOT &$_FEAT(6)).FEAT6C
@STR330  CVD   @00,@STRDWD             CONVERT VALUE TO DECIMAL
         AIF   (NOT &$_FEAT(4)).FEAT6C
         B     @STR361                 EDIT DWD
.FEAT6C  ANOP
.*
.*       PACKED FIELD
.*
         AIF   (NOT &$_FEAT(4)).FEAT4B
@STRZAP  ZAP   @STRDWD,0(*-*,@02)      MOVE TO @STRDWD
@STR351  &LAE  @03,0(,@02)             FIRST BYTE OF PACKED FIELD
         BALR  @14,0
         TM    0(@03),X'0C'            IS THIS THE SIGN BYTE?
         LA    @03,1(,@03)              (NEXT BYTE)
         BNOR  @14                     NO, LOOP MORE
         SLR   @03,@02                 GET LENGTH OF PACKED FIELD
         BCTR  @03,0
         EX    @03,@STRZAP             EXECUTE ZAP
         AIF   (NOT &$_FEAT(5)).FEAT4B
         CLI   @STRLEN2,248            JULIAN-TO-YYMMDD CONV?
         BNL   @STR375                 YES, JUMP
.FEAT4B  ANOP                          PACKED
.*
.*       EDIT @STRDWD (BIN, REG, PACKED)
.*
         AIF   (NOT &$_FEAT(8)).FEAT8B
@STR361  IC    @00,@STRLEN2            OUTPUT LENGTH
         LA    @03,X'003F'             MASK FOR "AND"
         NR    @03,@00                 OUTPUT LENGTH
         MVC   @STRWK16(16),@STRMASK   EDIT MASK
.*
.*       LEFT-JUSTIFICATION (NUMERIC)
.*
         AIF   (NOT &$_FEAT(9)).FEAT9B
         TM    @STRLEN2,@STRLEFT       LEFT JUSTIFICATION?
         BNO   @STR367                 NO, JUMP
         &LAE  @01,@STRWK16+15         PREVENT BAD R1
         EDMK  @STRWK16(16),@STRDWD    ZONED DECIMAL
         &LAE  @02,0(,@01)             FIRST STRING POSITION
         LTR   @03,@03                 CHECK OUTPUT LENGTH
         BNZ   @STR363                 JUMP IF NOT ZERO
.*       L0    (LEFT JUSTIFIED, NO PADDING)
         &LAE  @03,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @03,@02                 COMPUTE STRING LENGTH
         B     @STR390                 MOVE FIELD TO OUTPUT LINE
.*       L1-L63 (LEFT JUSTIFIED, PADDING)
@STR363  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN
         BNH   @STR364                 LARGE ENOUGH, JUMP
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.
@STR364  SR    @05,@03                 COMPUTE REMAINING LENGTH
         AIF   (NOT &ARMODE).ARMODE8N
         LAE   @08,0(,@04)             POINTER IN OUTPUT LINE
         LR    @09,@03                 LENGTH WITH PADDING
         AGO   .ARMODE8X
.ARMODE8N ANOP
         LR    @00,@04                 POINTER IN OUTPUT LINE
         LR    @01,@03                 LENGTH WITH PADDING
.ARMODE8X ANOP
         LA    @03,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @03,@02                 COMPUTE STRING LENGTH
         B     @STR392                 MOVE FIELD TO OUTPUT LINE
@STR367  EQU   *
.FEAT9B  ANOP
         AIF   (NOT &$_FEAT(10)).FEAT10B
         TM    @STRLEN2,@STRZERO       LEADING ZEROES REQ'D?
         BNO   @STR368                 NO, JUMP
         MVI   @STRWK16,C'0'           YES, CHANGE X'40' TO C'0'
@STR368  EQU   *
.FEAT10B ANOP
         ED    @STRWK16(16),@STRDWD    ZONED DECIMAL
         &LAE  @02,@STRWK16+16         FIRST POSITION AFTER STRING
         SR    @02,@03                 FIRST STRING POSITION
.FEAT8B  ANOP
.*
.*       LITERAL (@STRSCON IS A 16-BIT OFFSET)
.*       Short Literal (low-order 4 bits of @STRFLAG contains length)
.*       BLANKS  (@STRSCON=ZERO)
.*
         AIF   (NOT &$_FEAT(1)).FEAT1B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR372  LA    @01,7                   mask for NR
         &LAE  @02,@STRLEN2            1st byte of short literal
         IC    @03,@STRFLAG            pick up flags+length
         NR    @03,@01                 R3 has length of short literal
         BNZ   @STR390                 short literal, go move it
         SLR   @02,@02                 Clear Address Register
         IC    @03,@STRLEN2            GET LITERAL LENGTH
         TM    @STRFLAG,@STRX40        string of spaces?
         BO    @STR390                 yes, go move them
         ICM   @02,B'0011',@STRSCON    LOAD LITERAL OFFSET
         LA    @02,@STRSCON(@02)       CONVERT OFFSET TO FULL ADDRESS
.FEAT1B  ANOP
.*
.*       CONVERT JULIAN DATE TO YYMMDD
.*
         AIF   (NOT &$_FEAT(5)).FEAT5F
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR375  LA    @00,248                 MASK FOR 'SLR'
         SLR   @01,@01
         IC    @01,@STRLEN2            248-255
         SLR   @01,@00                 000-007
         LA    @00,12                  L'@STR375W
         MR    @00,@00                 COMPUTE OFFSET
         &LAE  @01,@STR375W(@01)       ENTRY IN "TR" MASK TABLE
         SLR   @03,@03
         IC    @03,0(,@01)             LENGTH OF DATE (6, 8 OR 10)
         ZAP   @STRDWD,@STRDWD         DATE=0000000?               @JDT
         BNZ   @STR375B                NO, JUMP                    @JDT
@STR375Z &LAE  @02,@STRBLANKS          WORK AREA
&STRBLANKS SETA 10                     WE NEED AT LEAST 10 BLANKS
         B     @STR390                 MOVE FIELD TO OUTPUT LINE
@STR375W DC    AL1(10,C'-',0,1,2,3,8,4,5,8,6,7) YYYY-MM-DD   248
         DC    AL1(8,C' ',0,1,2,3,4,5,6,7,8,8)  YYYYMMDD//   249
         DC    AL1(8,C'/',2,3,8,4,5,8,6,7,8,8)  YY/MM/DD//   250
         DC    AL1(8,C'/',6,7,8,4,5,8,2,3,8,8)  DD/MM/YY//   251
         DC    AL1(8,C'/',4,5,8,6,7,8,2,3,8,8)  MM/DD/YY//   252
         DC    AL1(6,C' ',2,3,4,5,6,7,8,8,8,8)  YYMMDD       253
.*       DC    AL1(6,C' ',6,7,4,5,2,3,8,8,8,8)  DDMMYY       254
.*       DC    AL1(6,C' ',4,5,6,7,2,3,8,8,8,8)  MMDDYY       255
@STR375T DC    P'59,31,29,31,30,31,30,31,31,30,31,30,31'
         DC    P'999'                  Prevent S0C7 with 90366
@STR375B CLI   @STRDWD+4,X'01'         YEAR 2000 OR ABOVE?
         BH    @STR375C                CC>01, JUMP (MUST BE 19 OR 20)
         MVI   @STRDWD+4,X'20'         CC=01, CHANGE TO CC=20
         BE    @STR375C                CC=01, USE CC=20
         CLI   @STRDWD+4+1,X'50'       YY<50?
         BL    @STR375C                YES, USE CC=20
         MVI   @STRDWD+4,X'19'         NO, FORCE CC=19
@STR375C UNPK  @STRWK16(5),@STRDWD+4(3) CCYY?
         ZAP   @STRDWD+1(2),@STR375T+9(1) INIT MONTH COUNTER
         &LAE  @02,@STR375T            TABLE OF MONTHS (NUMBER OF DAYS)
         TM    @STRDWD+4+1,X'01'       ODD YEARS
         BO    @STR375N                  AREN'T LEAP YEARS
         TM    @STRDWD+4+1,X'12'       ZEROES IN 1980, ALL ONES IN 1992
         BNM   @STR375L                MIXED IN 1982/1990
.*       IF IT'S NOT A LEAP YEAR AND DDD>59, THEN ADD 1 TO DDD
@STR375N CP    @STRDWD+4+2(2),@STR375T ARE WE PAST FEB 28 (DDD>59) ?
         BNH   @STR375L                NO, JUMP
         AP    @STRDWD+4+2(2),@STR375T+3(1) ADD 1 (FROM 31) TO DDD
.*--LOOP WHILE DDD > 0
@STR375L AP    @STRDWD+1(2),@STR375T+3(1)   ADD 1 (FROM 31) TO MONTH
         LA    @02,2(,@02)             NEXT ENTRY IN "MONTHS" TABLE
         SP    @STRDWD+4+2(2),0(2,@02) SUB DAYS-IN-MONTH FROM DDD
         BP    @STR375L
.*--ENDLOOP
         AP    @STRDWD+4+2(2),0(2,@02) UNDO LAST "SP" INSTRUCTION
         UNPK  @STRWK16+4(2),@STRDWD+1(2) FYFYFYFY,FMCM??
         UNPK  @STRWK16+6(2),@STRDWD+6(2) FYFYFYFY,FMCMFDCD
         MVZ   @STRWK16+1(7),@STRWK16     FYFYFYFY,FMCMFDCD
         MVC   @STRWK16+8(1),1(@01)    SEPARATOR
         &LAE  @02,@STRWK16+9          WORK AREA
         MVC   0(10,@02),2(@01)        MOVE CORRESPONDING MASK
         TR    0(10,@02),@STRWK16      CONVERT DATE TO THE RIGHT FORMAT
.FEAT5F  ANOP                          JDATE
.*
.*       HEX STRING
.*
         AIF   (NOT &$_FEAT(7)).FEAT7B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STR376M MVC   @STRDWD-1(*-*),0(@02)   PREVENT S0C4 IN UNPK
@STR376  LA    @00,8                   MAX LENGTH
         CLR   @03,@00                 CHECK LENGTH
         BNH   @STR376B                JUMP IF LE 8
         LR    @03,@00                 TRUNCATE TO MAXIMUM LENGTH
@STR376B LR    @01,@03                 INPUT LENGTH
         BCTR  @01,0
         EX    @01,@STR376M            MOVE DATA TO SAFE STORAGE
         ALR   @03,@03                 OUTPUT LENGTH
         AGO   .FEAT37B
.FEAT7B  ANOP
         AIF   (NOT &$_FEAT(3)).FEAT37C
         B     @STR390                 MOVE STRING TO OUTPUT LINE
.FEAT37B ANOP
@STR376X &LAE  @02,@STRWK16            WORK AREA
         UNPK  0(9,@02),@STRDWD-1(5)   EXPAND SOURCE BYTES FOR "TR"
         UNPK  8(9,@02),@STRDWD+3(5)   EXPAND SOURCE BYTES FOR "TR"
         TR    0(16,@02),@STRHEXT-240  =C'0123456789ABCDEF'
.FEAT37C ANOP
.*
.*       %TIME
.*
         AIF   (NOT &$_FEAT(12)).FEAT12B
         B     @STR390                 MOVE STRING TO OUTPUT LINE
@STRTIME DC    X'4021207A20207A20207A20204000'    0X.XX.XX.XX
@STR378  LR    @02,@15                 SAVE BASE REG
         TIME  DEC                     GET HHMMSSHH
         LR    @15,@02                 RESTORE BASE REG
         ST    @00,@STRDWD             STORE HHMMSSHH
         MVC   @STRWK16(13),@STRTIME   MOVE EDIT MASK
         ED    @STRWK16(13),@STRDWD    EDIT HH:MM:SS:HH
         &LAE  @02,@STRWK16+1          WORK AREA
         LA    @03,12                  HH:MM:SS:HH+ SPACE
.FEAT12B ANOP
.*MOVE
@STR390  CR    @03,@05                 COMPARE LENGTH TO REMAINING LEN
         BNH   @STR391                 LARGE ENOUGH, JUMP
         LR    @03,@05                 TOO BIG, TRUNCATE TO REM. LEN.
@STR391  SR    @05,@03                 COMPUTE REMAINING LENGTH
         AIF   (NOT &ARMODE).ARMODE39N
         LAE   @08,0(,@04)             POINTER IN OUTPUT LINE
         LR    @09,@03                 PASS REMAINING LENGTH
         AGO   .ARMODE39X
.ARMODE39N ANOP
         LR    @00,@04                 POINTER IN OUTPUT LINE
         LR    @01,@03                 PASS REMAINING LENGTH
.ARMODE39X ANOP
         LTR   @02,@02                 BLANKS?
         BNZ   @STR392                 NO, JUMP
         SLR   @03,@03                 YES, ZERO LENGTH
@STR392  ICM   @03,B'1000',@STRBLANKS  PAD WITH BLANKS
         AIF   (NOT &ARMODE).ARMODE44N
         MVCL  @08,@02                 MOVE FIELD TO OUTPUT LINE
         LR    @04,@08                 NEW POINTER IN OUTPUT LINE
         AGO   .ARMODE44X
.ARMODE44N ANOP
         MVCL  @00,@02                 MOVE FIELD TO OUTPUT LINE
         LR    @04,@00                 NEW POINTER IN OUTPUT LINE
.ARMODE44X ANOP
@STR398  TM    @STRFLAG,@STRLAST       TEST LAST-ENTRY INDICATOR
         BO    @STR399                 Done, exit
         AIF   (NOT &$_FEAT(1)).FEAT1C
         TM    @STRFLAG,@STRLIT+@STRX40 literal or spaces?
         BM    @STR398L                Literal, not spaces
         BZ    @STR398X                Neither literal nor spaces
         LA    @06,@STRSCON            2-byte entry for blank spaces
         B     @STR310                 PROCESS NEXT ENTRY
@STR398L LA    @01,7                   mask for NR
         IC    @03,@STRFLAG            pick up flags+length
         NR    @03,@01                 R3 has length of short literal
         BZ    @STR398T                not an in-line literal, jump
         LA    @06,@STRLEN2(@03)       Skip VL parm for in-line literal
         B     @STR310                 PROCESS NEXT ENTRY
@STR398T LA    @06,@STRFLEN            4-byte parm for remote literal
         B     @STR310                 PROCESS NEXT ENTRY
.FEAT1C  ANOP
@STR398X LA    @06,@STRNEXT            BUMP UP TO NEXT ENTRY
         B     @STR310                 PROCESS NEXT ENTRY
.*ENDLOOP
.*
.*       END-OF-LINE PROCESSING - PAD WITH BLANKS
.*
@STR399  SLR   @01,@01                 SET UP R1 FOR PADDING
         ICM   @01,B'1000',@STRBLANKS  SET UP R1 FOR PADDING
.***     DROP  @06,@13,@15
         AIF   (&ARMODE).ARMODE52Y
         LA    @14,2                   INCREMENT
         AL    @14,12(,@13)            RETURN ADDRESS
         LR    @15,@04                 CURRENT POINTER IN OUTPUT FIELD
         SL    @15,8(,@13)             CALCULATE LENGTH USED
         MVCL  @04,@00                 PAD WITH BLANKS
         LM    @00,@06,20(@13)         RESTORE WORK REGISTERS
         STM   @06+1,@13-1,48(@13)     MAKE SAVE AREA LOOK NORMAL
         OI    15(@13),1               SIMULATE "T" OPTION OF RETURN
         BR    @14                     RETURN TO CALLER
         AGO   .ARMODE52X
.ARMODE52Y ANOP
         LA    @15,3                   MODIFIABLE AREA
         ESTA  @14,@15                 R14 = START OF OUTPUT AREA
         LR    @15,@04                 END OF OUTPUT AREA
         SR    @15,@14                 R15 = LENGTH USED IN OUTPUT AREA
         MVCL  @04,@00                 PAD WITH BLANKS
         EREG  @00,@14                 RESTORE WORK REGISTERS
         STM   @14,@12,12(@13)         MAKE SAVE AREA LOOK NORMAL
         OI    15(@13),1               SIMULATE "T" OPTION OF RETURN
         PR                            RETURN TO CALLER
.ARMODE52X ANOP
.*
         AIF   (NOT (&$_FEAT(3) OR &$_FEAT(7))).FEAT37T
@STRHEXT DC    C'0123456789ABCDEF'     HEX-TO-EBCDIC CONVERSION
.FEAT37T ANOP
         AIF   (NOT &$_FEAT(8)).FEAT8T
@STRMASK DC    X'4020202020202020,2020202020202120'
.FEAT8T  ANOP
.**********************************************************************
.*       Convert S-con to address                                     *
.*             Input: GPR2 points to an S-CON in the remote parm list *
.*             Output: GPR2 contains the address                      *
.**********************************************************************
@STRS2A  SLR   @00,@00
         ICM   @00,B'0011',0(@02)      R0 = 0000BDDD
         SRDL  @00,12                  R0 = 0000000B, R1= DDD.....
         SRL   @01,20                  R1 = 00000DDD (DISPLACEMENT)
         AIF   (&ARMODE).ARMODE88Y
         CLI   0(@02),@06*16+15        R7-R13?
         BH    @STRS2A3                YES, JUMP
.*BASE REG IS R0-R6
         LTR   @02,@00                 IS R0 THE BASE REG?
         BNZ   @STRS2A2                NO, JUMP
         LTR   @02,@01                 IS THIS A PSA ADDRESS?
         BNZR  @14                     YES, GOBACK
@STRS2A2 SLL   @02,2                   R2= 000000BB BASE * 4
         L     @02,20(@02,@13)         PICK UP BASE REG VALUE
         LA    @02,0(@02,@01)          ADD BASE REG VALUE TO DISPL
         BR    @14
.*BASE REG IS R7-R13
@STRS2A3 LR    @02,@00                 R2= 0000000B (BASE REG NUMBER)
         EX    @02,@STRS2A4            ADD BASE REG VALUE TO DISPL
         BR    @14
@STRS2A4 LA    @02,0(*-*,@01)          ADD BASE REG VALUE TO DISPL
         AGO   .ARMODE88X
.ARMODE88Y ANOP                        AR_MODE=YES
         SLL   @00,2                   MULT BY 4
         CLI   0(@02),@06*16+15        R7-R13?
         BH    @STRS2A6                YES, JUMP
.*BASE REG IS R0-R6
         LTR   @02,@00                 IS R0 THE BASE REG?
         BNZ   @STRS2A5                NO, JUMP
         LTR   @02,@01                 IS THIS A PSA ADDRESS?
         BNZR  @14                     YES, GOBACK
         EREG  @00,@00                 POP R0
         LAE   @02,0                   R2=0 AR2=0
         LR    @02,@00                 PASS R0 VALUE
         LA    @02,0(,@02)             CLEAN UP
         BR    @14                     GOBACK
.*BASE REG IS R1-R6
@STRS2A5 LR    @07,@00                 SAVE R0 (B*4)
         LR    @08,@01                 SAVE R1 (DDD)
         LR    @09,@03                 SAVE R3
         LAE   @10,0(,@04)             SAVE R4
         LR    @11,@05                 SAVE R5
         LAE   @12,0(,@06)             SAVE R6
         EREG  @01,@06                 RELOAD CALLER'S R1-R6
         EX    0,@STRS2A7-4(@07)       ADD BASE REG VALUE TO DISPL
         LR    @03,@09                 RESTORE R3
         LAE   @04,0(,@10)             RESTORE R4
         LR    @05,@11                 RESTORE R5
         LAE   @06,0(,@12)             RESTORE R6
         BR    @14                     GOBACK
.*BASE REG IS R7-R13
@STRS2A6 LR    @02,@00                 R2= 0000000B (BASE REG NUMBER)
         EREG  @07,@12                 RESTORE CALLER'S R7-R12
         EX    0,@STRS2A7-4(@02)       ADD BASE REG VALUE TO DISPL
         BR    @14                     GOBACK
@STRS2A7 LAE   @02,0(@08,@01)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@02)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@03)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@04)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@05)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@08,@06)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@07)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@08)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@09)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@10)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@11)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@12)          ADD BASE REG VALUE TO DISPL
         LAE   @02,0(@01,@13)          ADD BASE REG VALUE TO DISPL
.ARMODE88X ANOP
         AIF   ('&OPT3' EQ 'LOCTR').GENL7L
@STRBLANKS DC  CL((((*+&STRBLANKS+7-@STRING)/8)*8)-(*-@STRING))' '
         AGO   .GENL7X
.GENL7L  ANOP
@STRBLANKS DC  CL&STRBLANKS.' '
.GENL7X  ANOP
@STRING_SIZE EQU *-@STRING             SIZE OF GENERATED CSECT
         DROP  @06,@13,@15
.**********************************************************************
.*       WORK AREA (CALLER'S SAVE AREA)                               *
.**********************************************************************
@STRSAVE DSECT                         24-BYTE WORK AREA
         DS    A(0,@STRSAVE,@STRSAVE,14,15,0,1,2,3,4,5,6)
@STRWK16 DS    F'7,8,9,10'             WORK AREA
@STRDWD  DS    D'1112'                 WORK AREA
@STRPARM DSECT
@STRFLAG DS    B                   +0  FORMAT, FLAGS
@STRLAST EQU   X'80'                     LAST ENTRY
@STRLIT  EQU   X'40'                     LITERAL, @STRSCON IS AN OFFSET
@STRX40  EQU   X'20'                   String of Spaces
.*             X'0F'                   CONVERSION REQUIRED
.*                                     or length of short literal
@STRHEX  EQU   X'08'                     HEXADECIMAL
@STRBIN  EQU   X'04'                     BINARY
@STRPACK EQU   X'02'                     PACKED
@STRREG  EQU   X'01'                     REGISTER
@STRLEN2 DS    B                   +1  FORMAT, OUTPUT LENGTH
.*                                     or start of short literal
@STRLEFT EQU   X'80'                     LEFT JUSTIFICATION
@STRZERO EQU   X'40'                     LEADING ZEROES
.*             X'3F'                     OUTPUT LENGTH, 0 MEANS TRUNC.
@STRSCON DS    S                   +2  FIELD ADDRESS
@STRFLEN DS    S                   +4  FIELD LENGTH
@STRNEXT EQU   *                   +6
         AIF   (NOT D'$LTORG).MEND99   CSECT-ONLY
         AIF   (D'$STRING).MEND99      CSECT-ONLY
$LTORG   LOCTR
         AIF   ('&OPT3' EQ 'LOCTR').GENL9L
         CNOP  0,4
$STRING  BALR  @15,0                   LOCAL BASE
         L     @15,6(@15,0)            ROUTINE ADDRESS           00
         BR    @15                     GO TO @STRING             04
         AGO   .GENL9
.GENL8   ANOP                          GENERATE,NOCSECT
&$_LIT   SETA  0                       DO NOT GENERATE LITERALS TWICE
&$_FEAT(1) SETB 1,1,1,1,1,1,1,1,1,1,1,1   GENERATE=FULL
$LTORG   LOCTR
         CNOP  0,4
$STRING  BALR  R15,0                   LOCAL BASE
         L     R15,6(R15,0)            ROUTINE ADDRESS           00
         BR    R15                     GO TO @STRING             04
.GENL9   ANOP
         DC    V(@STRING)              ROUTINE ADDRESS           06
         AGO   .MEND
.GENL9L  POP   USING                   GENERATE,LOCTR
$STRING  BASR  R15,0                   Local Base
         AH    R15,6(R15,0)            Add offset to @STRING routine
         BR    R15                     Branch to @STRING
         DC    Y(@STRING-$STRING-2)    Offset to @STRING Routine
.MEND    AIF   ('&PRINT' EQ 'NOGEN').MEND99
         POP   PRINT
.MEND99  MEND
./ ADD NAME=#TEST
         MACRO
&N       #TEST &DCODE=OMITTED,&MEXCL=,&NUM=OMITTED,                    *
               &PFIX=OMITTED,&REGS=,&SIZE=,&GEN=
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - MARCH 16, 1983
.*                  - COMMENTARY CHANGES
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - DECEMBER 4, 1981
.*                  - THE "SIZE=" FUNCTION HAS BEEN ENHANCED
.*                    TO INCLUDE SUPPORT FOR THE "NE"
.*                    RELATION.
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $TEST TO #TEST
.*
.* LAST CHANGE DATE - APRIL 15, 1981
.*                  - THE "NUM" FUNCTION NO LONGER ISSUES AN
.*                    ERROR MESSAGE WHEN IT ENCOUNTERS A
.*                    NON-DIGIT. IT ONLY SETS A RETURN CODE
.*                    OF 16 IN &#TESERR.
.*
.* LAST CHANGE DATE - APRIL 24, 1978
.*                    THE GEN=EBCDIC FUNCTION HAS BEEN ADDED
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - SEPTEMBER 10, 1976
.*                    THE &SIZE= OPERAND SUPPORT IS CHANGED
.*                    TO REQUIRE THREE SUB-OPERANDS WITH THE
.*                    SECOND SPECIFYING ONE OF THE RELATION
.*                    OPERATIONS: LT, LE, EQ, GE, OR GT.
.*                    NOTE, THIS IS NOT COMPATIBLE WITH THE
.*                    PREVIOUS IMPLEMENTATION.
.*
.* LAST CHANGE DATE - FEBRUARY 10, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS
.* CONCERNING IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*                                 GENERAL INFORMATION
.*   THE #TEST MACRO IS INTENDED TO BE USED AS AN INNER
.* MACRO. IT PERFORMS A NUMBER OF DIFFERENT TESTS AND
.* MANIPULATIONS WHICH ARE WHOLELY INDEPENDANT OF EACH OTHER.
.* FOR EACH OF THESE FUNCTIONS, INPUT MIGHT BE SPECIFIED
.* EITHER VIA MACRO OPERANDS OR BOTH MACRO OPERANDS AND GLOBAL
.* SYMBOLS (DEPENDING UPON THE FUNCTION). OUTPUT IS USUALLY
.* COMMUNICATED VIA THE GLOBAL SYMBOLS &#TESERR AND &#TESRET.
.*   &#TESERR IS A SCALER SETA SYMBOL WHICH IS USED IN A
.* MANNER SIMILAR TO A PROGRAM'S COMPLETION CODE TO
.* COMMUNICATE A GROSS INDICATION OF AN UNUSUAL OR ERROR
.* CONDITION. IF UPON RETURN FROM #TEST &#TESERR EQUALS ZERO,
.* THEN THE MACRO FUNCTIONED "OK"; OTHERWISE, THE VALUE OF
.* &#TESERR VARIES DIRECTLY WITH THE SERIOUSNESS OF THE
.* UNUSUAL OR ERROR CONDITION, AND IT IS ALWAYS SET TO
.* REFLECT THE MOST SERIOUS CONDITION ENCOUNTERED DURING A
.* PARTICULAR INVOCATION OF THE #TEST MACRO.
.*   &#TESRET IS A SETC ARRAY WHICH IS USED TO CONTAIN
.* RETURN VALUES FOR THOSE FUNCTIONS FOR WHICH RETURN
.* VALUES ARE APPROPIATE. EACH ELEMENT OF THE ARRAY HOLDS ONE
.* RETURN VALUE. ONLY AS MANY ELEMENTS ARE USED AS ARE
.* NEEDED. THOSE ELEMENTS USED ALWAYS START WITH ELEMENT
.* NUMBER ONE. IF TWO OR MORE TEST FUNCTIONS ARE INVOKED ON
.* A SINGLE CALL AND IF EACH OF THEM GENERATE ONE OR MORE
.* RETURN VALUES, THEN THE FIRST FUNCTION PROCESSED WILL USE
.* THE LOW ORDER ENTRIES IN &#TESRET. THE NEXT FUNCTION WILL
.* USE THE NEXT ENTRIES, ETC. THE VARIOUS TEST FUNCTIONS
.* WILL ALWAYS BE PROCESSED IN THE SAME ORDER WITH WHICH
.* THEY APPEAR BELOW.
.*
.*
.*
.* INNER MACROS USED - NONE
.*
         GBLA  &#REGVAL(255),&#TESERR
         GBLC  &#REGNME(255),&#TESRET(20)
         GBLC  &#EBCDIC(256)
         LCLA  &CTR,&RETPTR,&P,&L,&K,&A1,&A2,&RV(22)
         LCLB  &MESW
         LCLC  &RN(22),&BASE
&#TESERR SETA  0
.*
.*
.*
.*                                 THE "DCODE" FUNCTION
.*   THIS FUNCTION WAS WRITTEN BECAUSE THE MACRO LANGUAGE
.* DOES NOT SUPPORT THE DECODING OF "SUB-SUB-LIST" NOTATION.
.*   THE INPUT IS COMMUNICATED VIA THE "DCODE=" OPERAND AS A
.* CHARACTER STRING TO BE DECODED. THIS STRING MUST BE
.* EITHER NULL, UNPARENTHESIZED, OR A PARENTHESIZED LIST OF
.* ELEMENTS SEPERATED FROM EACH OTHER BY COMMAS. THE
.* ELEMENTS THEMSELVES MAY BE NULL.
.*   FOR A STRING OF N ELEMENTS, THE OUTPUT CONSISTS OF N+1
.* ENTRIES IN &#TESRET. THE FIRST ENTRY CONTAINS THE VALUE
.* N. THE REMAINING ENTRIES CONTAIN EACH OF THE ELEMENTS
.* EXTRACTED FROM THE ORIGINAL STRING. IF THE ORIGINAL
.* STRING IS NULL, THEN IT IS TREATED AS A SUB-LIST
.* CONTAINING ZERO ELEMENTS. IF THE STRING IS
.* UNPARENTHESIZED, THEN IT IS TREATED AS A SUB-LIST
.* CONTAINING A SINGLE ELEMENT - NAMELY, ITSELF.
.*   THE DCODE FUNCTION WILL NOT PROPERLY HANDLE THE
.* FOLLOWING CONDITIONS:
.*       A.) A SUB-LIST ELEMENT LONGER THAN EIGHT CHARACTERS;
.*       B.) A SUB-LIST ELEMENT THAT ITSELF CONSISTS OF A
.*           SUB-LIST;
.*       C.) A SUB-LIST CONTAINING MORE THAN NINETEEN
.*           ELEMENTS.
.*
.DCODE   AIF   ('&DCODE' EQ 'OMITTED').DCODEND
&RETPTR  SETA  &RETPTR+1
&CTR     SETA  0
         AIF   (K'&DCODE EQ 0).DCDFIN
         AIF   ('&DCODE'(1,1) EQ '(').DCDSLST
&CTR     SETA  1
&#TESRET(&RETPTR+1) SETC '&DCODE'
         AGO   .DCDFIN
.DCDSLST ANOP
&K       SETA  K'&DCODE
         AIF   ('&DCODE'(&K,1) EQ ')').DCDOK
         MNOTE 8,'"&DCODE" HAS INVALID SUBLIST SYNTAX.'
&#TESERR SETA  16
         AGO   .DCDFIN
.DCDOK   ANOP
&P       SETA  1
&L       SETA  0
.DCDLP1  ANOP
&P       SETA  &P+&L+1
&L       SETA  0-1
&CTR     SETA  &CTR+1
&#TESRET(&RETPTR+&CTR) SETC ''
.DCDLP2  ANOP
&L       SETA  &L+1
         AIF   ('&DCODE'(&P+&L,1) NE ',' AND &P+&L NE &K).DCDLP2
         AIF   (&L EQ 0).DCDLPET
&#TESRET(&RETPTR+&CTR) SETC '&DCODE'(&P,&L)
         AIF   ('&#TESRET(&RETPTR+&CTR)' EQ '&DCODE'(&P,&L)).DCDLPET
         MNOTE 8,'ERROR - THE FOLLOWING TRUNCATION HAS OCCURED:'
         MNOTE *,'        &DCODE'
         MNOTE *,'        &#TESRET(&RETPTR+&CTR)'
.DCDLPET AIF   (&P+&L NE &K).DCDLP1
.DCDFIN  ANOP
&#TESRET(&RETPTR) SETC '&CTR'
&RETPTR  SETA  &RETPTR+&CTR
.DCODEND ANOP
.*
.*
.*
.*                            THE "MEXCL" FUNCTION
.*   THIS FUNCTION CAN BE USED TO DETERMINE IF TWO OR MORE
.* MUTUALLY EXCLUSIVE OPERANDS (OR WHATEVER) HAVE BEEN
.* PASSED TO THE CALLING MACRO.
.*   THE INPUT IS COMMUNICATED VIA THE "MEXCL=" OPERAND AS A
.* SUB-LIST WITH ANY NUMBER OF ENTRIES. IF THE MEXCL
.* FUNCTION FINDS MORE THAN ONE NON-NULL ENTRY IN THE
.* SUB-LIST, THEN IT ISSUES A SEVERITY 8 ERROR MESSAGE, AND
.* IT SETS &#TESERR TO A VALUE OF 16.
.*
.MEXCL   AIF   (N'&MEXCL LT 2).MEXCEND
&CTR     SETA  0
.MELP    AIF   (&CTR EQ N'&MEXCL).MEXCEND
&CTR     SETA  &CTR+1
         AIF   (K'&MEXCL(&CTR) EQ 0).MELP
         AIF   (&MESW EQ 1).MEERR
&MESW    SETB  (1)
         AGO   .MELP
.MEERR   MNOTE 8,'ERROR - MUTUALLY EXCLUSIVE OPERANDS HAVE BEEN USED:'
         MNOTE *,'        &MEXCL'
&#TESERR SETA  16
.MEXCEND ANOP
.*
.*
.*
.*                            THE "NUM" FUNCTION
.*   THIS FUNCTION CAN BE USED TO DETERMINE WHETHER OR NOT A
.* GIVEN VALUE CONSISTS ENTIRELY OF DIGITS.
.*   THE INPUT IS COMMUNICATED VIA THE "NUM=" OPERAND. IF
.* THE NUM FUNCTION FINDS THAT ANY CHARACTER IN THE GIVEN
.* STRING IS NOT A DIGIT, THEN IT SETS &#TESERR TO A VALUE
.* OF 16.
.*
.NUM     AIF   ('&NUM' EQ 'OMITTED').NUMEND
         AIF   (K'&NUM EQ 0).NUMERR
&CTR     SETA  0
.NUMLP   AIF   (&CTR EQ K'&NUM).NUMEND
&CTR     SETA  &CTR+1
         AIF   ('&NUM'(&CTR,1) LT '0').NUMERR
         AIF   ('&NUM'(&CTR,1) LE '9').NUMLP
.NUMERR  ANOP
&#TESERR SETA  16
.NUMEND  ANOP
.*
.*
.*
.*                            THE "PFIX" FUNCTION
.*   THIS FUNCTION CAN BE USED TO DETERMINE THE VALIDITY OF
.* A GIVEN REGISTER NAME PREFIX (E.G. "R" IN "R15").
.*   THE INPUT CONSISTS OF A REGISTER NAME PREFIX
.* COMMUNICATED VIA THE "PFIX=" OPERAND AND A TABLE OF VALID
.* REGISTER NAME PREFIXES GENERATED VIA PRIOR #REGS MACROS
.* AND CONTAINED IN THE &#REGNME AND &#REGVAL GLOBAL
.* SYMBOLS. IF THE GIVEN PREFIX IS NULL, THEN A DEFAULT IS
.* USED. IF THE GIVEN PREFIX IS INVALID, THEN A SEVERITY 4
.* ERROR MESSAGE IS ISSUED AND A DEFAULT PREFIX IS USED. THE
.* DEFAULT IS EITHER THE FIRST PREFIX DEFINED VIA A PRIOR
.* #REGS MACRO OR NULL IF NO PRIOR #REGS MACRO HAS DEFINED
.* ANY PREFIXES.
.*   FOR OUTPUT, THE NEXT AVAILABLE &#TESRET ENTRY IS FILLED
.* WITH EITHER THE GIVEN PREFIX OR THE DEFAULT PREFIX.
.*
.PFIX    AIF   ('&PFIX' EQ 'OMITTED').PFIXEND
&RETPTR  SETA  &RETPTR+1
&#TESRET(&RETPTR) SETC ''
&CTR     SETA  0
.PFXLP1  AIF   (&CTR GE 255).PFXGDEF
&CTR     SETA  &CTR+1
         AIF   ('&#REGNME(&CTR)' EQ '').PFXGDEF
         AIF   (&#REGVAL(&CTR) LE 15).PFXLP1
&#TESRET(&RETPTR) SETC '&#REGNME(&CTR)'
.PFXGDEF AIF   (K'&PFIX EQ 0).PFIXEND
&CTR     SETA  &CTR-1
.PFXLP2  AIF   (&CTR GE 255).PFXERR
&CTR     SETA  &CTR+1
         AIF   ('&#REGNME(&CTR)' EQ '').PFXERR
         AIF   (&#REGVAL(&CTR) LE 15).PFXLP2
         AIF   ('&PFIX' NE '&#REGNME(&CTR)').PFXLP2
&#TESRET(&RETPTR) SETC '&PFIX'
         AGO   .PFIXEND
.PFXERR  MNOTE 4,'WARNING - "&PFIX" HAS NOT BEEN PREDEFINED.'
         MNOTE *,'          A DEFAULT VALUE WILL BE USED.'
         MNOTE *,'          CHECK YOUR USAGE OF THE #REGS'
         MNOTE *,'          MACRO.'
.PFIXEND ANOP
.*
.*
.*
.*                            THE "REGS" FUNCTION
.*   THIS FUNCTION CAN BE USED TO CONVERT A CERTAIN CLASS OF
.* REGISTER NAMES TO THEIR CORRESPONDING NUMERIC VALUES. IN
.* ORDER FOR A GIVEN NAME TO BE CONVERTED, IT MUST BE EITHER
.* A SELF-DEFINING NUMERIC OR A NAME THAT HAS BEEN DEFINED
.* VIA A PRIOR #REGS MACRO. THE PURPOSE OF THIS FUNCTION IS
.* TO PROVIDE ARITHMETICLY MANIPULATABLE REGISTER NUMBERS.
.*   THE INPUT CONSISTS OF A SUB-LIST OF REGISTER NAMES
.* COMMUNICATED VIA THE "REGS=" OPERAND AND A TABLE OF VALID
.* REGISTER NAME PREFIXES GENERATED VIA PRIOR #REGS MACROS
.* AND COMMUNICATED VIA THE &#REGNME AND &#REGVAL GLOBAL
.* SYMBOLS.
.*   FOR A SUB-LIST OF N REGISTER NAMES, THE OUTPUT CONSISTS
.* OF N ENTRIES IN &#TESRET EACH CONTAINING THE NUMBER OF
.* THE REGISTER REPRESENTED BY THE CORRESPONDING NAME FROM
.* THE INPUT SUB-LIST.
.*   IF ANY REGISTER NAME CANNOT BE PROPERLY DECODED, THEN
.* &#TESERR IS SET TO A VALUE OF 16. NO ERROR MESSAGE IS
.* ISSUED.
.*
.REGS    AIF   (N'&REGS EQ 0).REGSEND
&CTR     SETA  0
.REGLP1  AIF   (&CTR GE 16).REGLP2
&RN(&CTR+1) SETC '&CTR'
&RV(&CTR+1) SETA &CTR
&CTR     SETA  &CTR+1
         AGO   .REGLP1
.REGLP2  AIF   (&CTR GE 22).REGND2
&CTR     SETA  &CTR+1
&RN(&CTR) SETC 'ABCDEF'(&CTR-16,1)
&RV(&CTR) SETA &CTR-7
         AGO   .REGLP2
.REGND2  ANOP
&CTR     SETA  0
.REGLP3  AIF   (&CTR GE N'&REGS).REGSEND
&CTR     SETA  &CTR+1
&RETPTR  SETA  &RETPTR+1
&#TESRET(&RETPTR) SETC ''
         AIF   (K'&REGS(&CTR) EQ 0).REGLP3
&#TESRET(&RETPTR) SETC '&REGS(&CTR)'
         AIF   (T'&REGS(&CTR) NE 'N').REGLP3A
         AIF   (&REGS(&CTR) LT 0 OR &REGS(&CTR) GT 15).REGERR
         AGO   .REGLP3
.REGLP3A ANOP
&A1      SETA  0
.REGLP4  AIF   (&A1 GE 255).REGND4
&A1      SETA  &A1+1
         AIF   ('&#REGNME(&A1)' EQ '').REGND4
         AIF   (&#REGVAL(&A1) GT 15).REGPFX
         AIF   ('&REGS(&CTR)' NE '&#REGNME(&A1)').REGLP4
&#TESRET(&RETPTR) SETC '&#REGVAL(&A1)'
         AGO   .REGLP3
.REGPFX  ANOP
&A2      SETA  0
.REGLP5  AIF   (&A2 GE 22).REGLP4
&A2      SETA  &A2+1
         AIF   ('&REGS(&CTR)' NE '&#REGNME(&A1)&RN(&A2)').REGLP5
&#TESRET(&RETPTR) SETC '&RV(&A2)'
         AGO   .REGLP3
.REGND4  ANOP
&A2      SETA  0
.REGLP6  AIF   (&A2 GE 16).REGERR
&A2      SETA  &A2+1
         AIF   ('&REGS(&CTR)' NE '&RN(&A2)').REGLP6
         AGO   .REGLP3
.REGERR  ANOP
&#TESERR SETA  16
         AGO   .REGLP3
.REGSEND ANOP
.*
.*
.*
.*                            THE "SIZE" FUNCTION
.*   THIS FUNCTION WAS WRITTEN BECAUSE OF THE LIMITATION
.* THAT MACRO CODE CANNOT ARITHMETICLY MANIPULATE OPERANDS
.* CONSISTING OF EITHER EXPRESSIONS OR EQUATE SYMBOLS.
.* BECAUSE OF THIS, IN SITUATIONS WHERE A PROGRAMMER WOULD
.* NORMALLY WANT TO USE AN EXPRESSION, ETC., HE MAY INSTEAD
.* BE FORCED TO USE A SELF DEFINING NUMERIC. THE PROBLEM IS
.* THAT IF SUBSEQUENT MODIFICATIONS AFFECT THE VALUE OF SUCH
.* AN EXPRESSION, THE PROGRAMMER MIGHT FORGET TO CHANGE THE
.* SELF DEFINING NUMERIC ACCORDINGLY. THE SIZE FUNCTION CAN
.* BE USED TO ALLEVIATE THIS PROBLEM.
.*   THE INPUT IS COMMUNICATED VIA THE "SIZE=" OPERAND AND
.* IT MUST CONSIST OF A THREE ELEMENT SUB-LIST. THE FIRST
.* AND THIRD ELEMENTS MUST BE SUCH THAT THEY RESULTS IN
.* NON-RELOCATABLE VALUES WHEN ASSEMBLED. THE SECOND OPERAND
.* MUST BE ONE OF THE FOLLOWING RELATIONAL OPERATORS:
.*       LT, LE, EQ, GE, GT, NE
.* MEANING "LESS THAN", "LESS THAN OR EQUAL", "EQUAL",
.* "GREATER THAN OR EQUAL", "GREATER THAN", AND "NOT EQUAL"
.* RESPECTIVELY.
.*   THE OUTPUT CONSISTS OF A GENERATED STATEMENT WHICH
.* PRODUCES NO OBJECT CODE BUT WHICH CAUSES AN ERROR
.* WHENEVER THE TWO GIVEN ELEMENTS VIOLATE THE INDICATED
.* RELATION.
.*
.SIZE    AIF   (N'&SIZE EQ 0).SIZEEND
         AIF   ('&SIZE(2)' EQ 'EQ' OR '&SIZE(2)' EQ 'GE' OR '&SIZE(2)' *
               EQ 'LE' OR '&SIZE(2)' EQ 'GT' OR '&SIZE(2)' EQ 'LT' OR '*
               &SIZE(2)' EQ 'NE').OPOK
         MNOTE 8,'ERROR - "&SIZE(2)" NOT A VALID RELATIONAL OPERATOR'
&#TESERR SETA  16
         AGO   .SIZEEND
.OPOK    AIF   ('&SIZE(2)' NE 'EQ').OPNTEQ
         DC    0YL2(X'7FFF'-(&SIZE(1))+&SIZE(3),X'7FFF'-(&SIZE(3))+&SIZ*
               E(1))
         AGO   .SIZEEND
.OPNTEQ  AIF   ('&SIZE(2)' NE 'NE').OPNTNE
         DC    0YL2(X'8000'-(&SIZE(3)-(&SIZE(1)))/(&SIZE(3)-(&SIZE(1)))*
               )
         AGO   .SIZEEND
.OPNTNE  ANOP
&BASE    SETC  '7FFF'
         AIF   ('&SIZE(2)'(2,1) EQ 'E').NOPLUS1
&BASE    SETC  '8000'
.NOPLUS1 AIF   ('&SIZE(2)'(1,1) EQ 'G').OPG
         DC    0YL2(X'&BASE'-(&SIZE(3))+&SIZE(1))
         AGO   .SIZEEND
.OPG     ANOP
         DC    0YL2(X'&BASE'-(&SIZE(1))+&SIZE(3))
.SIZEEND ANOP
.*
.*
.*
.*                                 THE "GEN" FUNCTION
.*    THIS FUNCTION CAN BE USED TO GENERATE VARIOUS SPECIFIC
.* OBJECTS. CURRENTLY, THE SUPPORTED OBJECTS ARE:
.*       EBCDIC - A GLOBAL TABLE CONTAINING THE ENTIRE
.*                256-ENTRY EBCDIC CHARACTER SET SUCH THAT
.*                THE VALUE OF THE ITH ENTRY IS I-1.
.*
.GEN     AIF   (N'&GEN EQ 0).GENEND
&A1      SETA  0
.GENLP   AIF   (&A1 EQ N'&GEN).GENEND
&A1      SETA  &A1+1
         AIF   ('&GEN(&A1)' EQ '').GENLP
         AIF   ('&GEN(&A1)' NE 'EBCDIC').GNTEBCD
         AIF   ('&#EBCDIC(194)' EQ 'A').GENLP
&#EBCDIC(001) SETC '
&#EBCDIC(002) SETC ''
&#EBCDIC(003) SETC ''
&#EBCDIC(004) SETC ''
&#EBCDIC(005) SETC '¦'
&#EBCDIC(006) SETC '	'
&#EBCDIC(007) SETC '§'
&#EBCDIC(008) SETC ''
&#EBCDIC(009) SETC '©'
&#EBCDIC(010) SETC '°'
&#EBCDIC(011) SETC '±'
&#EBCDIC(012) SETC ''
&#EBCDIC(013) SETC ''
&#EBCDIC(014) SETC ''
&#EBCDIC(015) SETC ''
&#EBCDIC(016) SETC ''
&#EBCDIC(017) SETC ''
&#EBCDIC(018) SETC ''
&#EBCDIC(019) SETC ''
&#EBCDIC(020) SETC ''
&#EBCDIC(021) SETC '²'
&#EBCDIC(022) SETC '
'
&#EBCDIC(023) SETC ''
&#EBCDIC(024) SETC '·'
&#EBCDIC(025) SETC ''
&#EBCDIC(026) SETC ''
&#EBCDIC(027) SETC ''
&#EBCDIC(028) SETC '¸'
&#EBCDIC(029) SETC 'º'
&#EBCDIC(030) SETC ''
&#EBCDIC(031) SETC '»'
&#EBCDIC(032) SETC ''
&#EBCDIC(033) SETC '½'
&#EBCDIC(034) SETC 'À'
&#EBCDIC(035) SETC ''
&#EBCDIC(036) SETC 'Á'
&#EBCDIC(037) SETC 'Â'
&#EBCDIC(038) SETC '
'
&#EBCDIC(039) SETC ''
&#EBCDIC(040) SETC ''
&#EBCDIC(041) SETC 'Ã'
&#EBCDIC(042) SETC 'Ä'
&#EBCDIC(043) SETC 'Å'
&#EBCDIC(044) SETC 'Æ'
&#EBCDIC(045) SETC 'Ç'
&#EBCDIC(046) SETC ''
&#EBCDIC(047) SETC ''
&#EBCDIC(048) SETC ''
&#EBCDIC(049) SETC 'È'
&#EBCDIC(050) SETC 'É'
&#EBCDIC(051) SETC ''
&#EBCDIC(052) SETC 'Ë'
&#EBCDIC(053) SETC 'Ì'
&#EBCDIC(054) SETC ''
&#EBCDIC(055) SETC 'Í'
&#EBCDIC(056) SETC ''
&#EBCDIC(057) SETC 'Î'
&#EBCDIC(058) SETC 'Ð'
&#EBCDIC(059) SETC 'Ñ'
&#EBCDIC(060) SETC 'Ò'
&#EBCDIC(061) SETC ''
&#EBCDIC(062) SETC ''
&#EBCDIC(063) SETC 'Ó'
&#EBCDIC(064) SETC 'ü'
&#EBCDIC(065) SETC ' '
&#EBCDIC(066) SETC 'Ô'
&#EBCDIC(067) SETC 'ƒ'
&#EBCDIC(068) SETC '„'
&#EBCDIC(069) SETC '…'
&#EBCDIC(070) SETC ' '
&#EBCDIC(071) SETC 'Õ'
&#EBCDIC(072) SETC '†'
&#EBCDIC(073) SETC '‡'
&#EBCDIC(074) SETC '¤'
&#EBCDIC(075) SETC 'Ö'
&#EBCDIC(076) SETC '.'
&#EBCDIC(077) SETC '<'
&#EBCDIC(078) SETC '('
&#EBCDIC(079) SETC '+'
&#EBCDIC(080) SETC '×'
&#EBCDIC(081) SETC '&&'
&#EBCDIC(082) SETC '‚'
&#EBCDIC(083) SETC 'ˆ'
&#EBCDIC(084) SETC '‰'
&#EBCDIC(085) SETC 'Š'
&#EBCDIC(086) SETC '¡'
&#EBCDIC(087) SETC 'Œ'
&#EBCDIC(088) SETC '‹'
&#EBCDIC(089) SETC ''
&#EBCDIC(090) SETC 'Ø'
&#EBCDIC(091) SETC '!'
&#EBCDIC(092) SETC '$'
&#EBCDIC(093) SETC '*'
&#EBCDIC(094) SETC ')'
&#EBCDIC(095) SETC ';'
&#EBCDIC(096) SETC '^'
&#EBCDIC(097) SETC '-'
&#EBCDIC(098) SETC '/'
&#EBCDIC(099) SETC 'Ù'
&#EBCDIC(100) SETC 'Ž'
&#EBCDIC(101) SETC 'Û'
&#EBCDIC(102) SETC 'Ü'
&#EBCDIC(103) SETC 'Ý'
&#EBCDIC(104) SETC ''
&#EBCDIC(105) SETC '€'
&#EBCDIC(106) SETC '¥'
&#EBCDIC(107) SETC '|'
&#EBCDIC(108) SETC ','
&#EBCDIC(109) SETC '%'
&#EBCDIC(110) SETC '_'
&#EBCDIC(111) SETC '>'
&#EBCDIC(112) SETC '?'
&#EBCDIC(113) SETC 'Þ'
&#EBCDIC(114) SETC ''
&#EBCDIC(115) SETC 'ß'
&#EBCDIC(116) SETC 'à'
&#EBCDIC(117) SETC 'â'
&#EBCDIC(118) SETC 'ã'
&#EBCDIC(119) SETC 'ä'
&#EBCDIC(120) SETC 'å'
&#EBCDIC(121) SETC 'æ'
&#EBCDIC(122) SETC '`'
&#EBCDIC(123) SETC ':'
&#EBCDIC(124) SETC '#'
&#EBCDIC(125) SETC '@'
&#EBCDIC(126) SETC ''''''
&#EBCDIC(127) SETC '='
&#EBCDIC(128) SETC '"'
&#EBCDIC(129) SETC 'ç'
&#EBCDIC(130) SETC 'a'
&#EBCDIC(131) SETC 'b'
&#EBCDIC(132) SETC 'c'
&#EBCDIC(133) SETC 'd'
&#EBCDIC(134) SETC 'e'
&#EBCDIC(135) SETC 'f'
&#EBCDIC(136) SETC 'g'
&#EBCDIC(137) SETC 'h'
&#EBCDIC(138) SETC 'i'
&#EBCDIC(139) SETC '®'
&#EBCDIC(140) SETC '¯'
&#EBCDIC(141) SETC 'è'
&#EBCDIC(142) SETC 'é'
&#EBCDIC(143) SETC 'ê'
&#EBCDIC(144) SETC 'ì'
&#EBCDIC(145) SETC 'ð'
&#EBCDIC(146) SETC 'j'
&#EBCDIC(147) SETC 'k'
&#EBCDIC(148) SETC 'l'
&#EBCDIC(149) SETC 'm'
&#EBCDIC(150) SETC 'n'
&#EBCDIC(151) SETC 'o'
&#EBCDIC(152) SETC 'p'
&#EBCDIC(153) SETC 'q'
&#EBCDIC(154) SETC 'r'
&#EBCDIC(155) SETC 'ñ'
&#EBCDIC(156) SETC 'ò'
&#EBCDIC(157) SETC '‘'
&#EBCDIC(158) SETC 'ó'
&#EBCDIC(159) SETC '’'
&#EBCDIC(160) SETC 'ô'
&#EBCDIC(161) SETC 'õ'
&#EBCDIC(162) SETC '~'
&#EBCDIC(163) SETC 's'
&#EBCDIC(164) SETC 't'
&#EBCDIC(165) SETC 'u'
&#EBCDIC(166) SETC 'v'
&#EBCDIC(167) SETC 'w'
&#EBCDIC(168) SETC 'x'
&#EBCDIC(169) SETC 'y'
&#EBCDIC(170) SETC 'z'
&#EBCDIC(171) SETC '­'
&#EBCDIC(172) SETC '¨'
&#EBCDIC(173) SETC 'ö'
&#EBCDIC(174) SETC '['
&#EBCDIC(175) SETC '÷'
&#EBCDIC(176) SETC 'ø'
&#EBCDIC(177) SETC '›'
&#EBCDIC(178) SETC 'œ'
&#EBCDIC(179) SETC ''
&#EBCDIC(180) SETC 'ž'
&#EBCDIC(181) SETC 'Ÿ'
&#EBCDIC(182) SETC 'µ'
&#EBCDIC(183) SETC '¶'
&#EBCDIC(184) SETC '¬'
&#EBCDIC(185) SETC '«'
&#EBCDIC(186) SETC '¹'
&#EBCDIC(187) SETC 'ª'
&#EBCDIC(188) SETC '³'
&#EBCDIC(189) SETC '¼'
&#EBCDIC(190) SETC ']'
&#EBCDIC(191) SETC '¾'
&#EBCDIC(192) SETC '¿'
&#EBCDIC(193) SETC '{'
&#EBCDIC(194) SETC 'A'
&#EBCDIC(195) SETC 'B'
&#EBCDIC(196) SETC 'C'
&#EBCDIC(197) SETC 'D'
&#EBCDIC(198) SETC 'E'
&#EBCDIC(199) SETC 'F'
&#EBCDIC(200) SETC 'G'
&#EBCDIC(201) SETC 'H'
&#EBCDIC(202) SETC 'I'
&#EBCDIC(203) SETC 'Ê'
&#EBCDIC(204) SETC '“'
&#EBCDIC(205) SETC '”'
&#EBCDIC(206) SETC '•'
&#EBCDIC(207) SETC '¢'
&#EBCDIC(208) SETC 'Ï'
&#EBCDIC(209) SETC '}'
&#EBCDIC(210) SETC 'J'
&#EBCDIC(211) SETC 'K'
&#EBCDIC(212) SETC 'L'
&#EBCDIC(213) SETC 'M'
&#EBCDIC(214) SETC 'N'
&#EBCDIC(215) SETC 'O'
&#EBCDIC(216) SETC 'P'
&#EBCDIC(217) SETC 'Q'
&#EBCDIC(218) SETC 'R'
&#EBCDIC(219) SETC 'Ú'
&#EBCDIC(220) SETC '–'
&#EBCDIC(221) SETC ''
&#EBCDIC(222) SETC '—'
&#EBCDIC(223) SETC '£'
&#EBCDIC(224) SETC '˜'
&#EBCDIC(225) SETC '\'
&#EBCDIC(226) SETC 'á'
&#EBCDIC(227) SETC 'S'
&#EBCDIC(228) SETC 'T'
&#EBCDIC(229) SETC 'U'
&#EBCDIC(230) SETC 'V'
&#EBCDIC(231) SETC 'W'
&#EBCDIC(232) SETC 'X'
&#EBCDIC(233) SETC 'Y'
&#EBCDIC(234) SETC 'Z'
&#EBCDIC(235) SETC 'ý'
&#EBCDIC(236) SETC 'ë'
&#EBCDIC(237) SETC '™'
&#EBCDIC(238) SETC 'í'
&#EBCDIC(239) SETC 'î'
&#EBCDIC(240) SETC 'ï'
&#EBCDIC(241) SETC '0'
&#EBCDIC(242) SETC '1'
&#EBCDIC(243) SETC '2'
&#EBCDIC(244) SETC '3'
&#EBCDIC(245) SETC '4'
&#EBCDIC(246) SETC '5'
&#EBCDIC(247) SETC '6'
&#EBCDIC(248) SETC '7'
&#EBCDIC(249) SETC '8'
&#EBCDIC(250) SETC '9'
&#EBCDIC(251) SETC 'þ'
&#EBCDIC(252) SETC 'û'
&#EBCDIC(253) SETC 'š'
&#EBCDIC(254) SETC 'ù'
&#EBCDIC(255) SETC 'ú'
&#EBCDIC(256) SETC 'ÿ'
         AGO   .GENLP
.GNTEBCD MNOTE 8,'ERROR - &&GEN(&A1)=&GEN(&A1) IS UNRECOGNIZED'
         AGO   .GENLP
.GENEND  ANOP
.*
.*
.*
.END     MEND
./ ADD NAME=#UNALIGN
         MACRO
&NME     #UNALIGN &OP,&A1,&A2,&A3
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $UNALIGN TO #UNALIGN.
.*
.* LAST CHANGE DATE - FEBRUARY 19, 1978
.*                  - ADD SUPPORT FOR THE 'LM' AND 'STM' INSTRUCTIONS
.*
.* LAST CHANGE DATE - JANUARY 15, 1978
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
         LCLA  &W1
         LCLC  &N(30),&O(30)
&N(1)    SETC  'A'
&N(2)    SETC  'C'
&N(3)    SETC  'D'
&N(4)    SETC  'L'
&N(5)    SETC  'M'
&N(6)    SETC  'N'
&N(7)    SETC  'O'
&N(8)    SETC  'S'
&N(9)    SETC  'X'
&N(10)   SETC  'AH'
&N(11)   SETC  'AL'
&N(12)   SETC  'CH'
&N(13)   SETC  'CL'
&N(14)   SETC  'LH'
&N(15)   SETC  'LM'
&N(16)   SETC  'MH'
&N(17)   SETC  'SH'
&N(18)   SETC  'SL'
&N(19)   SETC  'ST'
&N(20)   SETC  'CVB'
&N(21)   SETC  'CVD'
&N(22)   SETC  'STH'
&N(23)   SETC  'STM'
&N(24)   SETC  'ÿÿÿÿÿÿÿÿ'
&O(1)    SETC  '5A'
&O(2)    SETC  '59'
&O(3)    SETC  '5D'
&O(4)    SETC  '58'
&O(5)    SETC  '5C'
&O(6)    SETC  '54'
&O(7)    SETC  '56'
&O(8)    SETC  '5B'
&O(9)    SETC  '57'
&O(10)   SETC  '4A'
&O(11)   SETC  '5E'
&O(12)   SETC  '49'
&O(13)   SETC  '55'
&O(14)   SETC  '48'
&O(15)   SETC  '98'
&O(16)   SETC  '4C'
&O(17)   SETC  '4B'
&O(18)   SETC  '5F'
&O(19)   SETC  '50'
&O(20)   SETC  '4F'
&O(21)   SETC  '4E'
&O(22)   SETC  '40'
&O(23)   SETC  '90'
&O(24)   SETC  ''
&W1      SETA  0
.LP1     ANOP
&W1      SETA  &W1+1
         AIF   ('&N(&W1)' LT '&OP').LP1
         AIF   ('&N(&W1)' EQ '&OP').END1
&W1      SETA  24
.END1    AIF   ('&O(&W1)' GE '80').RS
&NME     LA    &A1,&A2
         AGO   .COMMON
.RS      ANOP
&NME     ICM   &A1,&A2,&A3
.COMMON  ANOP
         ORG   *-4
         DC    X'&O(&W1)'
         ORG   *+3
         MEND
./ ADD NAME=#USING
         MACRO
         #USING &D
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $USING TO #USING.
.*
.* LAST CHANGE DATE - FEBRUARY 2, 1977
.*                  - MAILING ADDRESS CHANGE.
.*
.* LAST CHANGE DATE - AUGUST 23, 1976
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*
.*   THIS MACRO GENERATES A USING INSTRUCTION THAT REDECLARES ALL BASES
.* (IF ANY) DECLARED BY A PRIOR #ENTER MACRO EXPANSION.
.*
.*
.*
.* INNER MACROS USED - NONE
.*
         GBLC  &#BS(14)
         LCLA  &A1,&DISPL
         AIF   ('&#BS(14)' EQ '').END
&DISPL   SETA  &DISPL-4095
&A1      SETA  14
.LP      AIF   (&A1 EQ 2).END
&A1      SETA  &A1-1
         AIF   ('&#BS(&A1)' EQ '').LP
&DISPL   SETA  &DISPL+4095
         USING &#BS(14)+&DISPL,&#BS(&A1)
         AGO   .LP
.END     MEND
./ ADD NAME=#XXC
         MACRO
&NAME    #XXC  &OP,&T,&F,&L,&SVID=,&PFIX=,&MF=INLINE
.*
.*
.*
.* LAST CHANGE DATE - OCTOBER 18, 1983
.*                  - MAILING ADDRESS CHANGE
.*
.* LAST CHANGE DATE - MARCH 8, 1982
.*                  - THE LENGTH VERIFICATION TEST ("#TEST SIZE=...")
.*                    HAS BEEN REMOVED. IT IS NOT REALLY NEEDED.
.*
.* LAST CHANGE DATE - DECEMBER 1, 1981
.*                  - "PFIX=(...,NOSAVE)" SUPPORT HAS BEEN ADDED. THIS
.*                    PROVIDES A MEANS OF BYPASSING THE ISSUENCE OF THE
.*                    #REGS MACRO. (SOMETIMES, THE #REGS MACRO THAT I
.*                    WOULD GET IS NOT THE ONE I WANT).
.*
.* LAST CHANGE DATE - JUNE 1, 1981
.*                  - BUG FIXED. NOW, IF MF=SUBROUTINE AND
.*                    OP=TRT, THEN R1 IS NOT RESTORED WHEN
.*                    THE SUBROUTINE COMPLETES.
.*
.* LAST CHANGE DATE - APRIL 21, 1981
.*                  - MACRO NAME CHANGED FROM $XXC TO #XXC.
.*
.* LAST CHANGE DATE - APRIL 20, 1981
.*                  - "#REGS GEN=NO" SUPPORT ADDED.
.*
.* LAST CHANGE DATE - DECEMBER 12, 1977
.*                  - SYNTAX BUG FIXED. A CONTINUATION CHARACTER WAS
.*                    NOT PLACE IN COLUMN 72.
.*
.* LAST CHANGE DATE - JANUARY 17, 1977
.*                  - FORM OF &MF= OPERAND REDESIGNED.
.*                  - DEFAULT MEANINGS OF &T AND &F AND (FOR
.*                    MF=SUBROUTINE) &L REMOVED.
.*                  - REGISTER FORM OF &SVID= SUPPORTED.
.*                  - COMMENTARY REWRITTEN.
.*                  - MAILING ADDRESS CHANGED.
.*
.* LAST CHANGE DATE - SEPTEMBER 27, 1976
.*                  - FIXED BUG: MISSING SEQUENCE SYMBOL - .TLR
.*                  - CHANGED LENGTH SIZE TEST TO BE GENERATED WHENEVER
.*                    &L(2) WAS GIVEN WITH THE NON-ROUTINE/SUBROUTINE
.*                    FORM OF THE MACRO CALL.
.*                  - IF THE CALL IS A KNOWN LENGTH FORM AND IF THE
.*                    LENGTH'S DERIVATIVE EXPRESSION IS GIVEN (&L(2))
.*                    THEN IT IS USED TO DERIVE THE LENGTH FIELD OF THE
.*                    LAST SS INSTRUCTION GENERATED.
.*
.* LAST CHANGE DATE - JUNE 6, 1975
.*
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING
.* IT MAY BE ADDRESSED TO:
.*       RR#2 BOX 712
.*       AFTON, VA. 22920
.*
.*
.*    THIS MACRO GENERATES ROUTINES TO PERFORM NON-DECIMAL
.* STORAGE-TO-STORAGE FUNCTIONS ON OPERANDS OF ARBITRARY LENGTH.
.* SUPPORTED "SS-FUNCTIONS" ARE:
.*       CLC      MVZ      TR
.*       MVC      NC       TRT
.*       MVN      OC       XC
.*
.*    THIS MACRO HAS TWO DIFFERENT MODES OF EXPANSION:
.*     - IF THE OPERAND LENGTHS ARE KNOWN AT ASSEMBLY TIME, THEN THE
.*       EXPANSION CAN CONSIST OF A SUFFICIENT NUMBER OF
.*       SS-INSTRUCTIONS TO ACCOMODATE THE GIVEN LENGTH. FOR EACH
.*       SUCCESSIVE SS-INSTRUCTION GENERATED, THE SINK OPERAND POINTER
.*       IS INCREMENTED BY 256. THE SOURCE OPERAND POINTER IS ALSO
.*       INCREMENTED BY 256 UNLESS THE OPERATOR IS EITHER "TR" OR
.*       "TRT". ADDITIONALLY FOR "CLC" AND "TRT", CONDITIONAL BRANCH
.*       INSTRUCTIONS ARE INSERTED IN THE EXPANSION SO THAT UPON THE
.*       FIRST OCCURANCE OF A NOT-EQUAL CONDITION THE REMAINING
.*       SS-INSTRUCTIONS ARE BYPASSED.
.*     - IF THE OPERAND LENGTHS CANNOT BE KNOWN UNTIL EXECUTION TIME,
.*       THEN THE EXPANSION IS A GENERALIZED SUBROUTINE CAPABLE OF
.*       PROCESSING ANY LENGTH.
.*
.* &NAME
.*       THIS OPERAND IS OPTIONAL. IF GIVEN, THEN, OF COURSE, IT LABELS
.*       THE GENERATED CODE.
.*
.* &MF=
.*       THIS OPERAND IS OPTIONAL. IF OMITTED, THEN MF=INLINE IS
.*       ASSUMED. &MF= CONTROLS THE OVERALL FORM OF THE MACRO
.*       EXPANSION. &MF= MAY BE GIVEN IN THE FOLLOWING FORMS:
.*       -OMITTED-
.*       MF=
.*       MF=INLINE
.*             THE SINK OPERAND LENGTH IS KNOWN AT ASSEMBLY TIME. THE
.*             MACRO EXPANDS INTO A SUFFICIENT REPETITION OF
.*             SS-INSTRUCTIONS TO PERFORM THE FUNCTION.
.*       MF=(SUBROUTINE, --- )
.*             THE SINK OPERAND LENGTH CANNOT BE KNOWN UNTIL ASSEMBLY
.*             TIME. THE MACRO EXPANDS INTO A GENERALIZED SUBROUTINE
.*             CAPABLE OF PERFORMING THE REQUIRED SS-FUNCTION ON
.*             OPERANDS OF ARBITRARY LENGTH.
.*       MF=SUBROUTINE
.*       MF=(SUBROUTINE,(14))
.*             WHEN THE SS-FUNCTION SUBROUTINE COMPLETES, A RETURN IS
.*             MADE TO THE ADDRESS POINTED TO BY REGISTER-14.
.*       MF=(SUBROUTINE,(-X-))
.*             -X- MUST BE AN ABSOLUTE EXPRESSION REPRESENTING THE NAME
.*             OF A REGISTER CONTAINING A RETURN ADDRESS.
.*       MF=(SUBROUTINE,-X-)
.*             -X- MUST BE AN ABSOLUTE OR RELOCATABLE EXPRESSION
.*             REPRESENTING A FIXED RETURN ADDRESS.
.*       MF=(SUBROUTINE,*)
.*             WHEN THE SS-FUNCTION SUBROUTINE COMPLETES, IT FALLS
.*             THROUGH TO CODE FOLLOWING THE MACRO EXPANSION.
.*
.* &OP
.*       THIS OPERAND IS REQUIRED. IT IDENTIFIES THE SS-FUNCTION TO BE
.*       PERFORMED. IF SHOULD BE EITHER:
.*          CLC      MVZ      TR
.*          MVC      NC       TRT
.*          MVN      OC       XC
.*       IF IT IS ANYTHING ELSE, THEN A SEVERITY-4 DIAGNOSTIC IS ISSUED
.*       BEFORE THE EXPANSION IS ATTEMPTED.
.*
.* &T
.*       THIS OPERAND IS REQUIRED. IT IDENTIFIES THE LOCATION OF THE
.*       SINK OPERAND. IT MAY BE EITHER AN ADDRESS, A REGISTER, OR A
.*       BASE/DISPLACEMENT COMBINATION AS FOLLOWS:
.*       -X- (ADDRESS FORM): -X- MAY BE EITHER AN ABSOLUTE OR
.*           RELOCATABLE EXPRESSION IDENTIFYING THE LOCATION OF THE
.*           SINK OPERAND.
.*       (-X-) (REGISTER FORM): -X- MUST BE AN ABSOLUTE EXPRESSION
.*             REPRESENTING THE NAME OF A REGISTER CONTAINING THE
.*             ADDRESS OF THE SINK OPERAND.
.*       (-X-,-Y-) (BASE/DISPLACEMENT FORM): -X- MUST BE AN ABSOLUTE
.*                 EXPRESSION REPRESENTING A DISPLACEMENT. -Y- MUST BE
.*                 AN ABSOLUTE EXPRESSION REPRESENTING A BASE (REGISTER
.*                 NAME). TOGETHER THE BASE AND DISPLACEMENT GIVE THE
.*                 ADDRESS OF THE SINK OPERAND.
.*
.* &F
.*       THIS OPERAND IS REQUIRED. IT IDENTIFIES THE LOCATION OF THE
.*       SOURCE OPERAND. IT MAY BE EITHER AN ADDRESS, A REGISTER, OR A
.*       BASE/DISPLACEMENT COMBINATION AS FOLLOWS:
.*       -X- (ADDRESS FORM): -X- MAY BE EITHER AN ABSOLUTE OR
.*           RELOCATABLE EXPRESSION IDENTIFYING THE LOCATION OF THE
.*           SOURCE OPERAND.
.*       (-X-) (REGISTER FORM): -X- MUST BE AN ABSOLUTE EXPRESSION
.*             REPRESENTING THE NAME OF A REGISTER CONTAINING THE
.*             ADDRESS OF THE SOURCE OPERAND.
.*       (-X-,-Y-) (BASE/DISPLACEMENT FORM): -X- MUST BE AN ABSOLUTE
.*                 EXPRESSION REPRESENTING A DISPLACEMENT. -Y- MUST BE
.*                 AN ABSOLUTE EXPRESSION REPRESENTING A BASE (REGISTER
.*                 NAME). TOGETHER THE BASE AND DISPLACEMENT GIVE THE
.*                 ADDRESS OF THE SOURCE OPERAND.
.*
.* &L
.*       THIS OPERAND RELATES TO THE LENGTH OF THE SINK (AND,
.*       THEREFORE, THE SOURCE) OPERAND. ITS PRECISE MEANING, HOWEVER,
.*       DIFFERS DEPENDING UPON THE VALUE OF THE &MF= OPERAND.
.*
.* &L    (MF ==> INLINE)
.*       HERE, &L IS AN OPTIONAL OPERAND REPRESENTING THE ACTUAL LENGTH
.*       OF THE SINK OPERAND. IT MAY BE GIVEN IN THE FOLLOWING FORMS:
.*       -OMITTED-: THE MACRO ATTEMPTS TO USE THE IMPLIED LENGTH OF THE
.*                  SINK OPERAND. IF THE IMPLIED LENGTH CAN BE KNOWN AT
.*                  MACRO PASS TIME, THEN IT IS USED TO CONTROL THE
.*                  NUMBER OF SS-INSTRUCTIONS GENERATED; OTHERWISE,
.*                  ONLY ONE SS-INSTRUCTION IS GENERATED.
.*       -X-: -X- MUST BE A SELF-DEFINING TERM (I.E. RESOLVABLE AT
.*            MACRO PASS TIME) REPRESENTING THE LENGTH OF THE SINK
.*            OPERAND. IT IS USED TO CONTROL THE NUMBER OF
.*            SS-INSTRUCTIONS GENERATED.
.*       (-X-,-Y-): -X- MUST BE AS ABOVE. -Y- MUST BE A ABSOLUTE
.*                  EXPRESSION (I.E. RESOLVABLE BY FINAL-PASS TIME)
.*                  ALSO REPRESENTING THE LENGTH OF THE SINK OPERAND.
.*                  IN OTHER WORDS, -X- AND -Y- MUST REPRESENT THE SAME
.*                  VALUE. THE DISTINCTION IS THAT -Y- CAN BE
.*                  RESPONSIVE TO SUCH THINGS AS LENGTH ATTRIBUTES AND
.*                  EQUATE VALUES WHEREAS -X- CANNOT. -Y- IS NOT USED
.*                  TO CONTROL THE MACRO EXPANSION. INSTEAD, IT IS USED
.*                  AS A VALIDITY CHECK ON -X-. WHEN -Y- IS GIVEN, A
.*                  STATEMENT OF THE FORM:
.*                        DC    0Y(X'7FFF'-(X-Y),X'7FFF'-(Y-X))
.*                  IS GENERATED. THIS STATEMENT DOES NOT CREATE ANY
.*                  OBJECT CODE, BUT IF -X- AND -Y- HAVE UNEQUAL
.*                  VALUES, THEN IT DOES GENERATE AN ASSEMBLY ERROR.
.*
.* &L    (&MF ==> SUBROUTINE)
.*       HERE, &L IS A REQUIRED OPERAND. IT REPRESENTS THE LOCATION OF
.*       THE LENGTH OF THE SINK OPERAND. IT MAY BE GIVEN IN THE
.*       FOLLOWING FORMS:
.*       -X- (ADDRESS FORM): -X- MUST BE EITHER AN ABSOLUTE OR
.*           RELOCATABLE EXPRESSION REPRESENTING THE ADDRESS OF A
.*           FULLWORD CONTAINING THE LENGTH OF THE SINK OPERAND.
.*       (-X-) (REGISTER FORM): -X- MUST BE AN ABSOLUTE EXPRESSION
.*             REPRESENTING THE NAME OF A REGISTER CONTAINING THE
.*             LENGTH OF THE SINK OPERAND.
.*
.* &SVID=
.*       THIS OPERAND IS OPTIONAL. IT IDENTIFIES THE LOCATION OF A
.*       4-WORD REGISTER SAVE AREA FOR USE BY THE SUBROUTINE FORM OF
.*       THE MACRO EXPANSION. (FOR THE INLINE FORM OF THE EXPANSION,
.*       THE &SVID= OPERAND IS IGNORED.) &SVID= MAY BE GIVEN IN THE
.*       FOLLOWING FORMS:
.*       -OMITTED-: A 4-WORD SAVE AREA IS INCLUDED IN THE EXPANSION
.*       SVID=-X- (ADDRESS FORM): -X- MUST BE EITHER AN ABSOLUTE OR
.*             RELOCATABLE EXPRESSION REPRESENTING THE ADDRESS OF A
.*             4-WORD SAVE AREA.
.*       SVID=(-X-) (REGISTER FORM): -X- MUST BE AN ABSOLUTE EXPRESSION
.*             REPRESENTING THE NAME OF A REGISTER CONTAINING THE
.*             ADDRESS OF A 4-WORD REGISTER SAVE AREA. WARNING, THE
.*             REGISTER CANNOT BE EITHER 14, 15, 0, OR 1.
.*
.* &PFIX=
.*       THE #XXC MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS
.*       EXPANSION WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE
.*       OF THE REGISTERS IS INDICATED IN THE ASSEMBLER'S CROSS
.*       REFERENCE LISTING. THE PFIX= OPERAND CAN BE USED TO CONTROL
.*       THE SET OF EQUATES USED. FOR EXAMPLE, IF "PFIX=GPR" IS GIVEN,
.*       THEN "GPR1" IS USED WHENEVER THE EXPANSION REFERS FO REGISTER
.*       1.
.*          IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF REGISTER
.*       EQUATES DEFINED BY THE NEXT PRIOR #REGS MACRO IS USED. IF
.*       THERE ARE NO PRIOR #REGS MACROS, THEN SELF-DEFINING NUMERICS
.*       ARE USED.
.*
.* &PFIX=(...,NOSAVE)
.*       THIS PREVENTS THIS MACRO FROM INVOKING THE #REGS MACRO.
.*
.*
.*
.*
.* INNER MACROS USED - #REGS #TEST
.*
         GBLA  &#TESERR
         GBLC  &#TESRET(20)
         LCLA  &TDI,&TRI,&TR
         LCLA  &FDI,&FRI,&FR
         LCLA  &A,&D1,&RE
         LCLB  &LOADR1
         LCLC  &TZ,&TC,&TLP,&TRP
         LCLC  &FZ,&FC,&FLP,&FRP
         LCLC  &#,&C,&CD1,&CD2,&N,&@,&SAVEA
&#       SETC  '&SYSNDX'
&N       SETC  '&NAME'
         AIF   ('&OP' EQ 'CLC' OR '&OP' EQ 'MVC' OR '&OP' EQ 'MVN' OR '*
               &OP' EQ 'MVZ' OR '&OP' EQ 'NC' OR '&OP' EQ 'OC' OR '&OP'*
                EQ 'TR' OR '&OP' EQ 'TRT' OR '&OP' EQ 'XC').OPCOK
         MNOTE 4,'WARNING - THE OPCODE IS "&OP".'
         MNOTE 4,'THIS EXPANSION WILL PROBABLY NOT BE USEFULL.'
.OPCOK   ANOP
&TDI     SETA  1
&TRI     SETA  2
         AIF   (N'&T GT 1).TREG
         AIF   ('&T' EQ '&T(1)').TDCODED
&TDI     SETA  2
&TRI     SETA  1
&TZ      SETC  '0'
.TREG    ANOP
&TC      SETC  ','
&TLP     SETC  '('
&TRP     SETC  ')'
.TDCODED ANOP
&FDI     SETA  1
&FRI     SETA  2
         AIF   (N'&F GT 1).FREG
         AIF   ('&F' EQ '&F(1)').FDCODED
&FDI     SETA  2
&FRI     SETA  1
&FZ      SETC  '0'
.FREG    ANOP
&FC      SETC  ','
&FLP     SETC  '('
&FRP     SETC  ')'
.FDCODED ANOP
         AIF   ('&MF(1)' EQ 'INLINE' OR '&MF(1)' EQ '').XXC
         AIF   ('&MF(1)' EQ 'SUBROUTINE').SUB
         MNOTE 8,'MF(1)=&MF(1) IS INVALID.'
         AGO   .MEND
.SUB     ANOP
.*
&@       SETC  '&PFIX(1)'
         AIF   ('&PFIX(2). '(1,1) EQ 'N').DONPFIX
         AIF   (K'&PFIX(1) NE 0).GOTPFIX
         #TEST PFIX=
&@       SETC  '&#TESRET(1)'
         AGO   .DONPFIX
.GOTPFIX #REGS &PFIX(1),GEN=NO
.DONPFIX ANOP
.*
&SAVEA   SETC  'X&#.SVA'
         AIF   ('&SVID(1)' EQ '').GOTSAVA
&SAVEA   SETC  '&SVID'
         AIF   ('&SVID(1)' EQ '&SVID').GOTSAVA
&SAVEA   SETC  '0(&SVID(1))'
.GOTSAVA AIF   ('&F(&FRI)' EQ '').GOTFR
         #TEST REGS=&F(&FRI)
         AIF   (&#TESERR NE 0).GOTFR
&FR      SETA  &#TESRET(1)
.GOTFR   ANOP
&N       STM   &@.14,&@.1,&SAVEA   SAVE WORK REGISTERS
         AIF   ('&TZ' EQ '').TLA
         #TEST REGS=&T(&TRI)
         AIF   (&#TESERR NE 0).TLR
&TR      SETA  &#TESRET(1)
         AIF   (&TR EQ 15).TDONE
.TLR     LR    &@.15,&T(&TRI)      LOAD SINK REGISTER
         AGO   .TDONE
.TLA     LA    &@.15,&T(&TDI)&TLP&TC&T(&TRI)&TRP LOAD SINK REGISTER
.TDONE   ANOP
&LOADR1  SETB  (('&OP' NE 'TR' AND '&OP' NE 'TRT') OR (&FR EQ 14 OR &FR*
                EQ 15))
         AIF   (NOT &LOADR1).FDONE
         AIF   ('&FZ' EQ '').FLA
         AIF   (&FR EQ 15).FLR15
         AIF   (&FR EQ 1).FDONE
.FLR     LR    &@.1,&F(&FRI)       LOAD SOURCE REGISTER
         AGO   .FDONE
.FLR15   AIF   (&TR EQ 15).FLR
         L     &@.1,&SAVEA+4       LOAD SOURCE REGISTER
         AGO   .FDONE
.FLA     AIF   (&FR EQ 15).FL15
&FR      SETA  0
.FLAA    LA    &@.1,&F(&FDI)&FLP&FC&F(&FRI)&FRP LOAD SOURCE REGISTER
         AGO   .FDONE
.FL15    AIF   (&TR EQ 15).FLAA
         L     &@.1,&SAVEA+4       LOAD -
         LA    &@.1,&F(&FDI)(,&@.1) SOURCE REGISTER
.FDONE   AIF   ('&L' EQ '&L(1)').LL
         #TEST REGS=&L(1)
         AIF   (&#TESERR NE 0).LLTR
&A       SETA  4
&D1      SETA  &TR
         AIF   (&#TESRET(1) EQ 15).LLSVA
         AIF   (&#TESRET(1) NE 1).LLTR
&A       SETA  12
&D1      SETA  &FR
.LLSVA   AIF   (&#TESRET(1) EQ &D1).LLTR
         L     &@.0,&SAVEA+&A      LOAD LENGTH REGISTER
         AGO   .LLTR0
.LLTR    LTR   &@.0,&L(1)          LOAD LENGTH REGISTER; > 0?
         AGO   .LDONE
.LL      L     &@.0,&L             LOAD LENGTH REGISTER
.LLTR0   LTR   &@.0,&@.0           LENGTH > 0?
.LDONE   BNP   X&#.RET             NO, EXIT
         LA    &@.14,256           YES, GET MAX SINGLE EXECUTE LENGTH
X&#.LP   CR    &@.14,&@.0          LENGTH NEARLY EXHAUSTED YET?
         BNH   X&#.EX              NO, GO EXECUTE MAX LENGTH
         LR    &@.14,&@.0          YES, GET LAST EXECUTE LENGTH
X&#.EX   BCTR  &@.14,0             CONVERT LENGTH TO MACHINE LENGTH
         EX    &@.14,X&#.XXC       EXECUTE THE FUNCTION
         AIF   ('&OP' NE 'TRT' AND '&OP' NE 'CLC').TSTSKIP
         BNE   X&#.RET             INEQUALITY; GO RETURN TO CALLER
.TSTSKIP LA    &@.14,1(,&@.14)     RESTORE ORIGINAL LENGTH
         AR    &@.15,&@.14         ADVANCE SINK REGISTER
         AIF   ('&OP' EQ 'TRT' OR '&OP' EQ 'TR').NOINCR
         AR    &@.1,&@.14          ADVANCE SOURCE REGISTER
.NOINCR  SR    &@.0,&@.14          DECRIMENT THE LENGTH; DONE YET?
         BP    X&#.LP              NO, KEEP LOOPING
         AIF   ('&OP' NE 'TRT' OR NOT &LOADR1).NOTTRTX
         L     &@.1,&SAVEA+12      YES, TRT MISSED; INSURE R1 RESTORED
.NOTTRTX ANOP
&RE      SETA  1
         AIF   ('&OP' NE 'TRT').GOTRE
&RE      SETA  0
.GOTRE   ANOP
X&#.RET  LM    &@.14,&@&RE,&SAVEA  RESTORE REGISTERS
         AIF   ('&MF(2)' EQ '').BR14
         AIF   ('&MF(2)' EQ '*').BEND
         AIF   ('&MF(2)'(1,1) EQ '(').BREG
         B     &MF(2)              EXIT
         AGO   .DATA
.BR14    BR    &@.14               RETURN
         AGO   .DATA
.BEND    B     X&#.END             EXIT
         AGO   .DATA
.BREG    #TEST DCODE=&MF(2)
         BR    &#TESRET(2)         RETURN
.DATA    AIF   ('&SVID(1)' NE '').NOSAVEA
X&#.SVA  DS    4A                  REGISTER SAVE AREA
.NOSAVEA AIF   (NOT &LOADR1).TRTRTO
X&#.XXC  &OP   0(*-*,&@.15),0(&@.1) (EXECUTED)
         AGO   .ENDTST
.TRTRTO  ANOP
X&#.XXC  &OP   0(*-*,&@.15),&FZ&F(&FDI)&FLP&F(&FRI)&FRP (EXECUTED)
.ENDTST  AIF   ('&MF(2)' NE '*').MEND
X&#.END  DS    0H
         AGO   .MEND
.XXC     ANOP
&C       SETC  T'&L(1)
         AIF   ('&C' NE 'O').GOTLEN
&C       SETC  T'&T(&TDI)
         AIF   ('&C' EQ 'M' OR '&C' EQ 'N' OR '&C' EQ 'O' OR '&C' EQ 'T*
               ' OR '&C' EQ 'U' OR '&C' EQ 'W' OR '&C' EQ '$').NOLNGTH
&A       SETA  L'&T(&TDI)
         AIF   (&A GT 256).LONGMVC
.NOLNGTH ANOP
&N       &OP   &TZ&T(&TDI)&TLP&TC&T(&TRI)&TRP,&FZ&F(&FDI)&FLP&F(&FRI)&F*
               RP
         AGO   .MEND
.GOTLEN  AIF   ('&C' NE 'N').SHORTMV
&A       SETA  &L(1)
         AIF   (&A GT 256).LONGMVC
.SHORTMV ANOP
&A       SETA  1
         AIF   (K'&L(2) EQ 0).SHORT2
&A       SETA  2
.SHORT2  ANOP
&N       &OP   &TZ&T(&TDI)(&L(&A)&TC&T(&TRI)),&FZ&F(&FDI)&FLP&F(&FRI)&F*
               RP
         AGO   .MEND
.LONGMVC ANOP
&N       &OP   &TZ&T(&TDI)&CD1.(256&TC&T(&TRI)),&FZ&F(&FDI)&CD2&FLP&F(&*
               FRI)&FRP
&N       SETC  ''
         AIF   ('&OP' NE 'TRT' AND '&OP' NE 'CLC').NOTEST
         BNE   X&#.END
.NOTEST  ANOP
&A       SETA  &A-256
&D1      SETA  &D1+256
&CD1     SETC  '+'.'&D1'
         AIF   ('&OP' EQ 'TR' OR '&OP' EQ 'TRT').TROP
&CD2     SETC  '&CD1'
.TROP    AIF   (&A GT 256).LONGMVC
         AIF   (K'&L(2) EQ 0).LAST2
         &OP   &TZ&T(&TDI)&CD1.(&L(2)-&D1&TC&T(&TRI)),&FZ&F(&FDI)&CD2&F*
               LP&F(&FRI)&FRP
         AGO   .LAST1
.LAST2   &OP   &TZ&T(&TDI)&CD1.(&A&TC&T(&TRI)),&FZ&F(&FDI)&CD2&FLP&F(&F*
               RI)&FRP
.LAST1   AIF   ('&OP' NE 'CLC' AND '&OP' NE 'TRT').MEND
X&#.END  DS    0H
.MEND    MEND
@@
//*
