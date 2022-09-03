//SHOWDSCB  JOB (TSO),
//             'Install SHOWDSCB',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00000200
//* SOURCE: CBT (V488) FILE #860                                        00000300
//* TARGET: SYSC.LINKLIB  SYSC.JCLLIB                                   00000400
//*                                                                     00000500
//********************************************************************* 00000600
//* THIS JOB INSTALLS THE SHOWDSCB LOAD MODULE & JOB.                 * 00000700
//********************************************************************* 00000800
//*                                                                     00000900
//INSTALL PROC SOUT='*',               <=== SYSOUT CLASS                00001000
//             LIB='SYSC.LINKLIB',     <=== TARGET LOAD LIBRARY         00001100
//             JCL='SYSC.JCLLIB',      <=== JCL LIBRARY                 00001200
//             SYSTS=SYSDA,            <=== UNITNAME FOR WORK DATASETS  00001300
//             ASMBLR=IFOX00,          <=== NAME OF YOUR ASSEMBLER      00001400
//             ALIB='SYSC.LINKLIB',    <=== LOCATION OF YOUR ASSEMBLER  00001500
//             MACLIB='SYS1.MACLIB',   <=== MACLIB DATASET NAME         00001600
//             AMODGEN='SYS1.AMODGEN'  <=== AMODGEN DATASET NAME        00001700
//*                                                                     00001800
//LOADMACS EXEC PGM=IEBUPDTE,PARM=NEW                                   00001900
//SYSPRINT DD  SYSOUT=&SOUT                                             00002000
//SYSUT2   DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(,PASS),                   00002100
//             SPACE=(TRK,(150,,50),RLSE),DCB=(SYS1.MACLIB)             00002200
//*                                                                     00002300
//ASM1    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00002403
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00002503
//SYSTERM  DD  SYSOUT=&SOUT                                             00002603
//SYSPRINT DD  SYSOUT=&SOUT                                             00002703
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00002803
//         DD  DSN=&AMODGEN,DISP=SHR                                    00002903
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00003003
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003103
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003203
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003303
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(,PASS,DELETE),            00003403
//             SPACE=(TRK,(90,15))                                      00003503
//*                                                                     00003603
//ASM2    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00003703
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00003803
//SYSTERM  DD  SYSOUT=&SOUT                                             00003903
//SYSPRINT DD  SYSOUT=&SOUT                                             00004003
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00004103
//         DD  DSN=&AMODGEN,DISP=SHR                                    00004203
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00004303
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00004403
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00004503
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00004603
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00004703
//*                                                                     00004803
//ASM3    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00004903
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00005003
//SYSTERM  DD  SYSOUT=&SOUT                                             00005103
//SYSPRINT DD  SYSOUT=&SOUT                                             00005203
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00005303
//         DD  DSN=&AMODGEN,DISP=SHR                                    00005403
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00005503
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00005603
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00005703
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00005803
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00005903
//*                                                                     00006003
//ASM4    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00006103
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00006203
//SYSTERM  DD  SYSOUT=&SOUT                                             00006303
//SYSPRINT DD  SYSOUT=&SOUT                                             00006403
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00006503
//         DD  DSN=&AMODGEN,DISP=SHR                                    00006603
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00006703
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00006803
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00006903
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00007003
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00007103
//*                                                                     00007203
//ASM5    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00007303
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00007403
//SYSTERM  DD  SYSOUT=&SOUT                                             00007503
//SYSPRINT DD  SYSOUT=&SOUT                                             00007603
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00007703
//         DD  DSN=&AMODGEN,DISP=SHR                                    00007803
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00007903
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00008003
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00008103
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00008203
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00008303
//*                                                                     00008403
//LKED    EXEC PGM=IEWL,COND=(0,NE),                                    00008503
//             PARM='XREF,LET,LIST,SIZE=(600K,64K)'                     00008603
//SYSPRINT DD  SYSOUT=&SOUT                                             00008703
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,10)                               00008803
//SYSLMOD  DD  DSN=&LIB,DISP=SHR                                        00008903
//SYSLIN   DD  DSN=&&SYSLIN,DISP=(OLD,DELETE)                           00009003
//         DD  DDNAME=SYSIN                                             00009103
//*                                                                     00009203
//LOADJCL  EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                       00009303
//SYSPRINT DD  SYSOUT=&SOUT                                             00009403
//SYSUT2   DD  DSN=&JCL,DISP=MOD                                        00009503
//*                                                                     00009603
//        PEND                                                          00009703
//*                                                                     00009803
//        EXEC INSTALL                                                  00009903
//*                                                                     00010003
//LOADMACS.SYSIN DD *                                                   00010103
./ ADD NAME=#TRC     0111-98300-13222-0105-00185-00028-00000-GERHARD 00 00010203
         MACRO ,                                                        00010303
&NM      #TRC  &MODE,&FAST,&ARG,&LOAD=YES,&ADDR=@TRACE,     NEW GP98300*00010403
               &RENT=YES,&PFX=PGT,&DSECT=NO,&ADCON=NO,          GP08157*00010503
               &REGSAVE=                                        GP11243 00010603
         GBLB  &ZZ$TRFG,&ZZ$TDFG                                GP00192 00010703
         GBLB  &ZZ$TRNT                                         GP99364 00010803
         GBLC  &ZZ$TRAD                                                 00010903
         GBLC  &MACPLAB                                         GP99364 00011003
.********************************************************************** 00011103
.*                                                                   ** 00011203
.*   #TRC CONTROLS THE INVOCATION OF THE PROGRAM TRACE ROUTINE;      ** 00011303
.*                                                                   ** 00011403
.*   REQUIRES TRACEIN AND TRACEOUT DD CARDS. SEE PGMTRACE SOURCE.    ** 00011503
.*                                                                   ** 00011603
.********************************************************************** 00011703
         LCLC  &EPNAME,&PTR,&P                                          00011803
         LCLB  &DYN                                                     00011903
         LCLA  &OFF,&I,&RQC                                             00012003
&I       SETA  &SYSNDX                                                  00012103
&DYN     SETB  ('&LOAD' EQ 'YES' OR '&MODE' EQ 'DYN')                   00012203
&PTR     SETC  '&ADDR'                                                  00012303
&MACPLAB SETC  '&NM'                                            GP99364 00012403
&ZZ$TRNT SETB  (&ZZ$TRNT OR ('&RENT' EQ 'YES'))                 GP99364 00012503
&EPNAME  SETC  'TRACEON'                                                00012603
&OFF     SETA  8                                                        00012703
&RQC     SETA  1             TRACE ON FLAG                              00012803
         AIF   ('&MODE' EQ 'DATA').DODATA                       GP00192 00012903
         AIF   ('&REGSAVE' NE 'YES').NOSAVE                     GP11243 00013003
&MACPLAB STM   R14,R1,ZZ&I.V      SAVE USER'S REGISTERS         GP13222 00013103
&MACPLAB SETC  ''                                               GP11243 00013203
.NOSAVE  AIF   (NOT &DYN).HAVBAS                                        00013303
         AIF   ('&ADDR' EQ '').DEFBAS                                   00013403
&ZZ$TRAD SETC  '&ADDR'                                                  00013503
.DEFBAS  ANOP  ,                                                        00013603
&PTR     SETC  '&ZZ$TRAD'                                               00013703
         AIF   ('&PTR' NE '').HAVBAS                                    00013803
&PTR     SETC  '=A(PGMTRACE)'                                           00013903
&ZZ$TRAD SETC  '=A(PGMTRACE)'                                           00014003
.HAVBAS  AIF   (&ZZ$TRFG).LATER                                         00014103
&ZZ$TRFG SETB  1                                                        00014203
         WXTRN TRACE,TRACEON,TRACEOFF,TRACKILL                          00014303
.LATER   AIF   ('&MODE' EQ 'ON' OR '&MODE' EQ 'TRACEON').SPEED          00014403
&RQC     SETA  0             TRACE OFF FLAG                             00014503
&OFF     SETA  12                                                       00014603
&EPNAME  SETC  'TRACEOFF'                                               00014703
         AIF   ('&MODE' EQ 'OFF' OR '&MODE' EQ 'TRACEOFF').SPEED        00014803
&OFF     SETA  4                                                        00014903
&EPNAME  SETC  'TRACKILL'                                               00015003
         AIF   ('&MODE' EQ 'END' OR '&MODE' EQ 'TRACKILL').EXPAND       00015103
         AIF   ('&MODE' EQ 'QUIT' OR '&MODE' EQ 'EXIT').EXPAND          00015203
         AIF   ('&MODE' EQ 'KILL' OR '&MODE' EQ 'DONE').EXPAND          00015303
&OFF     SETA  24                                               \       00015403
&EPNAME  SETC  'TRACSUSP'                                       \       00015503
         AIF   ('&MODE' EQ 'SUS' OR '&MODE' EQ 'TRACSUSP').EXPAND       00015603
         AIF   ('&MODE' EQ 'SUSP' OR '&MODE' EQ 'SUSPEND').EXPAND       00015703
&OFF     SETA  0                                                        00015803
&EPNAME  SETC  'TRACE'                                                  00015903
         AIF   ('&MODE' EQ 'INIT' OR '&MODE' EQ 'TRACE').SPECIAL        00016003
         MNOTE 8,'#TRC - UNRECOGNIZED OPERAND : &MODE'                  00016103
         MEXIT ,                                                        00016203
.SPECIAL AIF   ('&ADCON' NE 'YES').NOADCON                      GP08157 00016303
         AIF   (&ZZ$TRNT).NOADZER                               GP99364 00016403
&MACPLAB B     4+&PTR                                           GP08255 00016503
&PTR     DC    A(0)                                             GP08157 00016603
&MACPLAB SETC  ''                                               GP99364 00016703
         AGO   .NOADCON                                                 00016803
.NOADZER ANOP  ,                                                GP08157 00016903
&MACPLAB B     4+&ZZ$TRAD                                       GP08157 00017003
&ZZ$TRAD DC    V(PGMTRACE)                                      GP08157 00017103
&MACPLAB SETC  ''                                               GP99364 00017203
&DYN     SETB  0             CANCEL DYNAMIC OPTION              GP99364 00017303
.NOADCON AIF   (&ZZ$TRNT).NOMOD                                 GP99364 00017403
&MACPLAB NOP   ZZ&I.B                                                   00017503
&MACPLAB SETC  ''                                               GP99364 00017603
         OI    *-4+1,X'F0'                                              00017703
.NOMOD   AIF   (&DYN).DYNLOAD                                   GP99364 00017803
         MACPARM R1,&FAST,NULL=0  SET API OPTION ADDRESS OR 0           00017903
         ICM   R15,15,&PTR                                              00018003
         BZ    ZZ&I.Z                                                   00018103
         BASR  R14,R15                                                  00018203
&EPNAME  SETC  'TRACEON'                                                00018303
&OFF     SETA  8                                                        00018403
ZZ&I.B   DS    0H                                                       00018503
         AGO   .EXPAND                                                  00018603
.DYNLOAD ANOP  ,                                                GP10164 00018703
&MACPLAB ICM   R15,15,&PTR  LOADED BEFORE?                              00018803
&MACPLAB SETC  ''                                               GP10164 00018903
         BNZ   ZZ&I.A        YES?                                       00019003
         LOAD  EP=PGMTRACE,ERRET=ZZ&I.Z                                 00019103
         LA    R15,&PTR                                                 00019203
         ST    R0,0(,R15)    NON-KOSHER                                 00019303
ZZ&I.A   DS    0H                                                       00019403
         AIF   ('&FAST' EQ 'ON').PRM3ON                                 00019503
         AIF   ('&FAST' NE 'OFF').PRM2                                  00019603
&OFF     SETA  12            INITIALIZE WITHOUT TRACE PRINTING          00019703
&EPNAME  SETC  'TRACEOFF'                                               00019803
         AGO   .PRM3                                                    00019903
.PRM3ON  ANOP  ,             ALREADY SET FOR NORMAL TRACE?              00020003
.PRM3    MACPARM R1,&ARG,NULL=0   SET API OPTION ADDRESS OR 0           00020103
         AGO   .PRM2N3                                                  00020203
.PRM2    MACPARM R1,&FAST,NULL=0  SET API OPTION ADDRESS OR 0           00020303
.PRM2N3  L     R15,&PTR                                                 00020403
         L     R15,&OFF+64(,R15)                                        00020503
         BASR  R14,R15                                                  00020603
         B     ZZ&I.Z                                                   00020703
         AIF   ('&REGSAVE' NE 'YES').NOS14R1                    GP13222 00020803
ZZ&I.V   DC    4A(0)                                            GP13222 00020903
.NOS14R1 AIF   (&OFF NE 0).PRMOFF                                       00021003
&EPNAME  SETC  'TRACEON'                                                00021103
&OFF     SETA  8                                                        00021203
.PRMOFF  ANOP  ,                                                        00021303
ZZ&I.B   DS    0H                                                       00021403
.EXPAND  AIF   (NOT &DYN).EXPANDS                                       00021503
         MACPARM  R15,15,&PTR,OP=ICM,MODE=THREE                 GP02242 00021603
         BZ    ZZ&I.Z                                                   00021703
         L     R15,&OFF+64(,R15)                                        00021803
         BASR  R14,R15                                                  00021903
         AGO   .REGREST                                                 00022003
.EXPANDS ANOP  ,                                                        00022103
         MACPARM  R15,15,=A(&EPNAME),OP=ICM,MODE=THREE          GP02242 00022203
         BZ    ZZ&I.Z                                                   00022303
         BASR  R14,R15                                                  00022403
.REGREST AIF   ('&REGSAVE' NE 'YES').GOAWAY                     GP11243 00022503
ZZ&I.Z   LM    R14,R1,ZZ&I.V                                    GP13222 00022603
         MEXIT ,                                                        00022703
.GOAWAY  ANOP  ,                                                        00022803
ZZ&I.Z   DS    0H                                                       00022903
         MEXIT ,                                                        00023003
.SPEED   AIF   ('&FAST' NE 'FAST').EXPAND                               00023103
         AIF   ('&ARG' EQ '').NOARG                                     00023203
&MACPLAB DC    0H'0',X'83',AL1(X'C0'+&RQC),SL2(&ARG)                    00023303
         MEXIT ,                                                        00023403
.NOARG   ANOP  ,                                                        00023503
&MACPLAB DC    0H'0',X'83',AL1(X'C0'+&RQC),AL2(0)                       00023603
         MEXIT ,                                                GP00192 00023703
.DODATA  ANOP  ,                                                GP00192 00023803
&P       SETC  '&PFX'                                           GP00192 00023903
         AIF   ('&DSECT' EQ 'NO').NODADSC                       GP00192 00024003
         AIF   ('&NM' EQ '').NFDADS                             GP00203 00024103
&MACPLAB DSECT ,             PGMTRADA PARAMETER LIST            GP00203 00024203
         AGO   .NODADS                                          GP00203 00024303
.NFDADS  ANOP  ,                                                GP00203 00024403
&P.SECT  DSECT ,             CALLER'S PARM DSECT                GP00192 00024503
         AGO   .NODADS                                          GP00203 00024603
.NODADSC AIF   ('&NM' EQ '').NODADS                             GP00203 00024703
&MACPLAB DS    0D            PGMTRADA PARAMETER LIST            GP00203 00024803
.NODADS  ANOP  ,                                                GP00192 00024903
&P.FUN   DS    X             FUNCTION (L - LOOK-UP; M-MEMBER BUILD)     00025003
         AIF   (&ZZ$TDFG).HAVEMAP                               GP00192 00025103
CFLOOK   EQU   C'L'            LOCATE MEMBER/OFFSET - PRINT DATA        00025203
CFPOINT  EQU   C'M'            LOCATE MEMBER; BUILD SOURCE CHAIN        00025303
CFCLOSE  EQU   C'C'            CLOSE AND FREE EVERYTHING        GP00192 00025403
.HAVEMAP ANOP  ,                                                GP00192 00025503
&P.FLG1  DC    AL1(0)        PRINT OPTIONS                      GP00192 00025603
         AIF   (&ZZ$TDFG).HAVEFG1                               GP00192 00025703
CFDIR    EQU   X'80'           PRINT DIRECTORY ENTRY DATA       GP00192 00025803
CFESD    EQU   X'40'           PRINT CESD LISTING               GP00192 00025903
CFRLD    EQU   X'20'           PRINT RLD LISTING                GP00192 00026003
CFSYM    EQU   X'10'           PRINT SYM LISTING                GP00192 00026103
CFDAT    EQU   X'08'           PRINT SYSADATA INFO (LATER)      GP00192 00026203
CFLBL    EQU   X'02'           PRINT LABELS                     GP00192 00026303
CFTRC    EQU   X'01'           PRINT THE TRACE TABLE ON ABNORMAL END    00026403
.HAVEFG1 ANOP  ,                                                GP00192 00026503
&P.FLG2  DC    AL1(0)        PRINT OPTIONS                      GP00192 00026603
         AIF   (&ZZ$TDFG).HAVEFG2                               GP00192 00026703
CFHEX    EQU   X'80'           PRINT THE CSECT HEX DUMP         GP00192 00026803
CFLST    EQU   X'40'           PRINT THE ASSEMBLY LISTING       GP00192 00026903
CFXRF    EQU   X'20'           PRINT A LABEL CROSS-REFERENCE    GP00192 00027003
CFPUN    EQU   X'10'           PUNCH OUTPUT (?)                 GP00192 00027103
CFBUG    EQU   X'01'           PRINT ADDITIONAL DEBUG INFO      GP00192 00027203
.HAVEFG2 ANOP  ,                                                GP00192 00027303
&P.FLG3  DC    X'00'         PROCESSING FLAG                    GP00192 00027403
         AIF   (&ZZ$TDFG).HAVEFG3                               GP00192 00027503
CFMAC    EQU   X'80'           INCLUDE MACRO EXPANDED CODE (SYSADATA)   00027603
CFIMAC   EQU   X'40'           INCLUDE INLINE MACRO CODE   (SYSADATA)   00027703
CFCMT    EQU   X'20'           INCLUDE COMMENTS            (SYSADATA)   00027803
CFASM    EQU   X'10'           INCLUDE ASSEMBLER PSEUDO-OPS(SYSADATA)   00027903
&ZZ$TDFG SETB  1                                                GP00192 00028003
.HAVEFG3 ANOP  ,                                                GP00192 00028103
&P.MEM   DC    CL8' '        MEMBER NAME                        GP00192 00028203
&P.ESD   DC    CL8' '        CSECT NAME (NOT USED AT PRESENT)   GP00192 00028303
&P.OFF   DC    AL4(0)        OFFSET FROM LOAD POINT             GP00192 00028403
&P.WORK  DC    A(0)          WORK AREA (BUILT/FREED HERE)       GP00192 00028503
&P.@PRT  DC    A(0)          CALLER'S PRINT ROUTINE (R1 - BUFFER)       00028603
.MEND    MEND  ,                                                        00028703
./ ADD NAME=AMODE    8000-04314-04314-2100-00004-00004-00000-GERHARD 00 00028803
         MACRO ,                                                        00028903
         AMODE ,                                                        00029003
.*   DUMMY MACRO CREATED TO SUPPORT ASSEMBLY UNDFER HERCULES (XF ASM)   00029103
         MEND  ,                                                        00029203
./ ADD NAME=BAS      8005-04307-08278-0210-00013-00015-00000-GERHARD 00 00029303
         MACRO ,                                                        00029403
&NM      BAS   &R,&A                                    ADDED ON 04234  00029503
.*                                                                      00029603
.*       THIS MODULE GENERATES EITHER A BAL FOR MVS COMPATIBILIY        00029703
.*       BAS NEEDS AT LEAST SP 2 (MVS/XA) OR HERCULES 370               00029803
.*                                                                      00029903
         GBLB  &MVT                                                     00030003
&NM      BAL   &R,&A                                                    00030103
         AIF   (&MVT).MEND                                              00030203
         ORG   *-4                                                      00030303
         DC    X'4D'         BAS                                        00030403
         ORG   *+3                                                      00030503
.MEND    MEND  ,                                                        00030603
./ ADD NAME=BASR     8004-04307-08278-0212-00007-00015-00000-GERHARD 00 00030703
         MACRO ,                                                        00030803
&NM      BASR  &R,&S                                    ADDED ON 04234  00030903
.*                                                                      00031003
.*       THIS MODULE GENERATES A BASR FOR IFOX (MIN. HERC 370)          00031103
.*                                                                      00031203
&NM      DC    0H'0',AL.4(0,13,&R,&S)   BASR                            00031303
         MEND  ,                                                        00031403
./ ADD NAME=BASSM    8004-04315-10054-2023-00021-00015-00000-GERHARD 00 00031503
         MACRO ,                                                        00031603
&NM      BASSM &R,&A                                   ADDED ON GP04234 00031703
         GBLB  &MVSXA                                           GP10054 00031803
         GBLC  &MODEL                                           GP10054 00031903
.*                                                                      00032003
.*       THIS MODULE GENERATES A BASR FOR MVS COMPATIBILITY.            00032103
.*       REQUIRE AT LEAST SP 2 (MVS/XA) FOR HARDWARE SUPPORT            00032203
.*                                                                      00032303
.*R1   R2   BALR R1,R2                                                  00032403
.*0    R2   BALR 0,R2                                                   00032503
.*R1   0    BALR R1,0                                                   00032603
.*0    0    BALR 0,0                                                    00032703
.*                                                                      00032803
         AIF   ('&MODEL' EQ '380').BASSM                        GP10054 00032903
         AIF   (NOT &MVSXA).OLD                                 GP10054 00033003
.BASSM   ANOP  ,                                                GP10054 00033103
&NM      DC    0H'0',AL.4(0,12,&R,&A)   BASSM                   GP10054 00033203
         MEXIT ,                                                        00033303
.OLD     ANOP  ,                                                GP10054 00033403
&NM      DC    0H'0',AL.4(0,13,&R,&A)   BASR                    GP10054 00033503
         MEND  ,                                                        00033603
./ ADD NAME=BSM      8011-04315-10159-0146-00022-00010-00000-GERHARD 00 00033703
         MACRO ,                                                        00033803
&NM      BSM   &R,&A                                   ADDED ON GP04234 00033903
         GBLB  &MVSXA                                           GP08292 00034003
         GBLC  &MODEL                                           GP08292 00034103
.*                                                                      00034203
.*       THIS MODULE GENERATES A BALR FOR COMPATIBILITY                 00034303
.*                                                                      00034403
         AIF   ('&MODEL' EQ '380').BSM                          GP08292 00034503
         AIF   (NOT &MVSXA).OLD                                 GP08292 00034603
.BSM     ANOP  ,                                                GP08292 00034703
&NM      DC    0H'0',AL.4(0,11,&R,&A)   BSM                     GP08292 00034803
         MEXIT ,                                                        00034903
.OLD     AIF   ('&R(1)' EQ '0' OR '&R(1)' EQ 'R0').BR           GP08292 00035003
         AIF   ('&A(1)' EQ '0' OR '&A(1)' EQ 'R0').SETAM        GP10159 00035103
&NM      MACPARM MODE=LBL                                       GP08292 00035203
         MEXIT ,                                                GP08292 00035303
.SETAM   ANOP  ,                                                GP10159 00035403
&NM      LA    &R,0(,&R)     CLEAN HIGH BYTE (AM24)             GP10159 00035503
         MEXIT ,                                                GP10159 00035603
.BR      ANOP  ,                                                GP08292 00035703
&NM      BR    &A                                                       00035803
         MEND  ,                                                        00035903
./ ADD NAME=CLRL     0102-03093-09016-1527-00045-00047-00000-GERHARD 00 00036003
         MACRO ,                                                        00036103
&NM      CLRL  &A,&LN,&WORK1=14,&WORK2=0,&FILL=,&OPLEN=LA        84254  00036203
.*--------------------------------------------------------------------* 00036303
.*  CLEAR AN AREA OF ANY LENGTH USING MVCL (WAS MVC ON 360)           * 00036403
.*--------------------------------------------------------------------* 00036503
         LCLA  &K                                                       00036603
         LCLC  &W1E,&W1O,&W2E,&W2O                                      00036703
         LCLC  &L                                               GP03093 00036803
&L       SETC  'L'''                                            GP03093 00036903
.*                                                                      00037003
         AIF   (T'&A EQ 'O').BADA                                       00037103
.*USE L*  AIF   (T'&LN EQ 'O').BADLN                                    00037203
         AIF   (N'&A NE 1).BADA                                         00037303
.*USE L*  AIF   (N'&LN NE 1).BADLN                                      00037403
.*                                                                      00037503
         AIF   (T'&WORK1 EQ 'O').BADW1                                  00037603
         AIF   (T'&WORK2 EQ 'O').BADW2                                  00037703
         AIF   (N'&WORK1 NE 1).BADW1                                    00037803
         AIF   (N'&WORK2 NE 1).BADW2                                    00037903
.*                                                                      00038003
&W1E     SETC  '&WORK1(1)'                                              00038103
&W1O     SETC  '&W1E'.'+1'                                              00038203
&W2E     SETC  '&WORK2(1)'                                              00038303
&W2O     SETC  '&W2E'.'+1'                                              00038403
&NM      MACPARM &W1E,&A                                                00038503
         MACPARM &W1O,&LN,NULL=&L&A,OP=&OPLEN                   GP03093 00038603
         MACPARM &W2O,0      CLEAR SOURCE LENGTH                        00038703
         AIF   (T'&FILL EQ 'O').NOFILL                                  00038803
         AIF   ('&FILL' EQ '0').NOFILL                                  00038903
&K       SETA  K'&FILL                                                  00039003
         AIF   ('&FILL'(&K,1) EQ '''' OR '&FILL'(&K,1) EQ ')').FILLICM  00039103
         ICM   &W2O,8,=AL1(&FILL)                                       00039203
         AGO   .NOFILL                                                  00039303
.FILLICM ICM   &W2O,8,=&FILL                                            00039403
.NOFILL  MVCL  &W1E,&W2E     CLEAR THE AREA                             00039503
         MEXIT ,                                                        00039603
.BADLN   MNOTE 4,'INVALID AREA LENGTH &LN'                              00039703
         MEXIT ,                                                        00039803
.BADA    MNOTE 4,'INVALID AREA ADDRESS &A'                              00039903
         MEXIT ,                                                        00040003
.BADW1   MNOTE 4,'INVALID WORK1 &WORK1'                                 00040103
         MEXIT ,                                                        00040203
.BADW2   MNOTE 4,'INVALID WORK2 &WORK2'                                 00040303
         MEXIT ,                                                        00040403
         MEND  ,                                                        00040503
./ ADD NAME=DBO      8005-05016-09183-1943-00328-00327-00000-GERHARD 00 00040603
         MACRO ,                                                        00040703
&NM      DBO   &LBL,&TEXT=,&REGS=YES,&HEX=,&MODE=S,     ADDED ON 85360 *00040803
               &WK=R9,&DEV=1,&TCB=,         WTO VS @PRT  CHANGED 94011 *00040903
               &ROUT=13,&DES=4,&BUGPARM=NO,                ADDED 95067 *00041003
               &CTEXT=,&PACK=,                             ADDED 96081 *00041103
               &PRTMODE=0,&DCB=0,   USER PRINT DCB/MODE    ADDED 99058 *00041203
               &COUNT=,&CALL=DYN,&OPT=,                  CHANGED 98222 *00041303
               &WA=DBTSAVE,                                ADDED 99114 *00041403
               &LIST=                                      ADDED 95235  00041503
.********************************************************************** 00041603
.*>>>>>>>>> KEPT FOR OLD CODE ONLY - NEW CODE SHOULD USE DBT <<<<<<<<<* 00041703
.********************************************************************** 00041803
.*                                                                    * 00041903
.*  THIS MACRO INVOKES EXTERNAL LOAD MODULE DEBTROLD TO PRODUCE       * 00042003
.*  TRACING, REGISTER CONTENTS, AND VARIABLES. (DEBTROLD SHOULD BE IN * 00042103
.*  A LINKLIB; AUTHORIZATION IS NOT REQUIRED). OUTPUT WILL BE BY WTO  * 00042203
.*  UNLESS A DEBTRACE DD CARD IS SUPPLIED.                            * 00042303
.*                                                                    * 00042403
.*  REQUIRED:  IN A CSECT OR RSECT:   DEBTRACE MODE=C  DEFINES CODE   * 00042503
.*             IN A CSECT OR DSECT:   DEBTRACE MODE=D  DEFINES DATA   * 00042603
.*             IN A CSECT (^RENT) :   DEBTRACE MODE=DC   BOTH         * 00042703
.*    (NOTE: REQUIRED FORMS MUST NOT APPEAR PRIOR TO FIRST OPTIONAL)  * 00042803
.*                                                                    * 00042903
.*  OPTIONAL:  LABEL DEBTRACE ...                                     * 00043003
.*                TAG OR ,  -  IDENTIFIER ON OUTPUT LISTING / CONSOLE * 00043103
.*                                                                    * 00043203
.*                REGS= (DEFAULT)   REGS=NO - NO REGISTERS            * 00043303
.*                REGS=YES  -  REGISTERS R0 THROUGH R15               * 00043403
.*                REGS=(R1,R2) - REGISTERS R1 THROUGH R2              * 00043503
.*                REGS=SHORT   - R14 THROUGH R1                       * 00043603
.*                                                                    * 00043703
.*                TEXT=NAME -  TEXT STRING TO BE SHOWN                * 00043803
.*                TEXT=(NAME,LEN) - TEXT W/EXPLICIT LENGTH            * 00043903
.*                                                                    * 00044003
.*                CTEXT=NAME - CONDITIONAL TEXT STRING TO BE SHOWN    * 00044103
.*                CTEXT=(NAME,LEN) - TEXT W/EXPLICIT LENGTH           * 00044203
.*                              OUTPUT IN HEX IF NOT PRINTABLE        * 00044303
.*                                                                    * 00044403
.*                HEX=NAME   -  DATA TO BE SHOWN IN HEXADECIMAL       * 00044503
.*                HEX=(NAME,LEN) - TEXT W/EXPLICIT LENGTH             * 00044603
.*                                                                    * 00044703
.*                PACK=NAME  -  DATA TO BE CONVERTED FROM PACKED      * 00044803
.*                PACK=(NAME,LEN) - TEXT W/EXPLICIT LENGTH (LEN IGNRD)* 00044903
.*                                                                    * 00045003
.*           LIST=((OP1,LN1,FM1),(OP2,LN2,FM2), ... )                 * 00045103
.*                                                                    * 00045203
.*                OP - ADDRESS EXPRESSION VALID IN S CONSTANT         * 00045303
.*                LN - LENGTH EXPRESSION; DEFAULT IS L'OP             * 00045403
.*                FM - TEXT   CTEXT   HEX   PACK - DEFAULT IS HEX     * 00045503
.*                     OR ABBREVIATED   T   CT   H   P                * 00045603
.*                                                                    * 00045703
.*  THE REQUIRED FORMS MAY BE OMITTED WHEN PGMTRACE WILL ALSO BE USED * 00045803
.*  AND ACTIVATED. IN THAT CASE THE FIRST OPTIONAL FORM MUST INCLUDE  * 00045903
.*  CALL=TRC TO GENERATE SHORTER PARAMETER LISTS.                     * 00046003
.*                                                                    * 00046103
.********************************************************************** 00046203
.*  MAINTENANCE:                                                      * 00046303
.*                                                                    * 00046403
.*  2000/01/03  GYP  REMOVED IN-LINE DEBUG CODE;                      * 00046503
.*                   FIXED MODE=C AND MODE=D FOR USE WITH REENTRANT   * 00046603
.*                     PROGRAMS.                                      * 00046703
.*                                                                    * 00046803
.********************************************************************** 00046903
     GBLB  &BUGBEAR,&BUGTCB,&BUGSWCH,&BUGSWRT,&BUGFAR,&BUGEXT,&BUGDYN   00047003
     GBLB  &BUGTRC,&BUGDBO   USED WITH ACTIVE PGMTRACE (ESPIE)  GP99113 00047103
         GBLA  &MACP#        NUMBER OF SUBLIST PARAMETERS       GP04234 00047203
         GBLC  &MACP1,&MACP2,&MACP3,&MACP4,&MACP5               GP04234 00047303
         GBLC  &MACP6,&MACP7,&MACP8,&MACP9,&MACP10              GP04234 00047403
         GBLC  &V                                                       00047503
         LCLA  &LN,&I,&EN,&EM,&EO                               GP95235 00047603
         LCLC  &L,&ET,&EL,&EK                                   GP95235 00047703
&L       SETC  'L'''                                            GP95235 00047803
&V       SETC  'DBT'.'&SYSNDX'                                          00047903
&BUGFAR  SETB  (&BUGFAR OR ('&CALL' EQ 'FAR'))                   95079  00048003
&BUGEXT  SETB  (&BUGEXT OR ('&CALL' EQ 'EXTRN'))                 95227  00048103
&BUGDYN  SETB  (&BUGDYN OR ('&CALL' EQ 'DYN'))                  GP97261 00048203
&BUGDYN  SETB  (&BUGDYN OR ('&CALL' EQ ''))  DROP LOCAL CODE    GP00004 00048303
&BUGDYN  SETB  (&BUGDYN OR ('&CALL' EQ 'DYNAMIC'))              GP97261 00048403
&BUGTRC  SETB  (&BUGTRC OR ('&CALL' EQ 'TRC'))                  GP99113 00048503
&BUGTRC  SETB  (&BUGTRC OR ('&CALL' EQ 'TRACE'))                GP99113 00048603
&BUGTRC  SETB  (&BUGTRC OR ('&CALL' EQ 'PGMTRACE'))             GP99113 00048703
         AIF   (&BUGBEAR OR '&BUGPARM' EQ 'NO').DOSOME                  00048803
         AIF   ('&NM' EQ '').MEND                                       00048903
&NM      DS    0H            DEBUG SWITCH NOT ON                        00049003
         AGO   .MEND                                                    00049103
.DOSOME  ANOP  ,                                                 95067  00049203
&BUGSWCH SETB  1                                                 95067  00049303
         AIF   ('&MODE' EQ 'D' OR '&MODE' EQ 'M').DATA           95228  00049403
         AIF   ('&MODE' EQ 'C').CODE                                    00049503
         AIF   ('&MODE' EQ 'DC').CODE   EXPAND BOTH              95067  00049603
         AIF   ('&MODE' EQ 'ON').SWON                            95079  00049703
         AIF   ('&MODE' EQ 'OFF').SWOFF                          95079  00049803
         AIF   ('&MODE' EQ 'CLOSE').SWEND  CLOSE AND QUIT       GP98222 00049903
&BUGDBO  SETB  1             DBO STATEMENT EXPANDED             GP09183 00050003
         AIF   (NOT &BUGTRC).NOTTRC                             GP99113 00050103
&NM      DC    X'83CD',S(&WA,&V.X-*)             INVOKE TRACE   GP99113 00050203
         AGO   .DONEBAS                                         GP99113 00050303
.NOTTRC  ANOP  ,                                                GP99113 00050403
&NM      STM   R0,R15,&WA    SAVE ALL REGISTERS                         00050503
         AIF   ('&COUNT' EQ '').DONECNT                          95079  00050603
.*  COUNT(3) - SKIP FIRST N CALLS                                95079  00050703
         AIF   ('&COUNT(3)' EQ '').CNTNO3                        95079  00050803
         ICM   R14,15,&V.3   LOAD SKIP COUNT                     95079  00050903
         BNP   &V.C          LIMIT REACHED - PROCESS             95079  00051003
         BCTR  R14,0         DECREMENT                           95079  00051103
         STCM  R14,15,&V.3   SAVE FOR NEXT TIME                  95079  00051203
         B     &V.X          AND SKIP CALL                       95079  00051303
&V.3     DC    AL4(&COUNT(3))  INITIAL SKIP COUNT                95079  00051403
&V.C     DS    0H                                                95079  00051503
.CNTNO3  AIF   ('&COUNT(2)' EQ '').CNTNO2                        95079  00051603
         AIF   ('&COUNT(2)' EQ '1').CNTNO2                       95079  00051703
         AIF   ('&COUNT(2)' EQ '0').CNTNO2                       95079  00051803
.*  COUNT(2) - PROCESS EVERY NTH CALL ONLY                       95079  00051903
         ICM   R14,15,&V.2   LOAD COUNTER                        95079  00052003
         BNP   &V.L          BAD - PROCESS CALL                  95079  00052103
         BCT   R14,&V.N      NON-ZERO; SAVE AND SKIP             95079  00052203
         MVC   &V.2,=AL4(&COUNT(2))  REFRESH                     95079  00052303
         B     &V.L          AND GO                              95079  00052403
&V.2     DC    AL4(1)        INTERVAL COUNTER (DO FIRST ONE)     95079  00052503
&V.N     STCM  R14,15,&V.2   UPDATE COUNTER                      95079  00052603
         B     &V.X          AND EXIT                            95079  00052703
.CNTNO2  AIF   ('&COUNT(1)' EQ '').DONECNT                       95079  00052803
         AIF   ('&COUNT(1)' EQ '0').DONECNT                      95079  00052903
         ICM   R14,15,&V.1   LOAD LIMIT COUNT                    95079  00053003
         BNP   &V.X          SKIP OUT IF NOT VALID               95079  00053103
         BCTR  R14,0         DECREMENT                           95079  00053203
         B     &V.M          SAVE, AND CONTINUE                  95079  00053303
&V.1     DC    AL4(&COUNT(1))  MAXIMUM CALLS TO MAKE             95079  00053403
&V.M     STCM  R14,15,&V.1   SAVE FOR NEXT TIME                  95079  00053503
.DONECNT ANOP  ,                                                 95079  00053603
&V.L     BAS   R1,&V.B                                           95079  00053703
.DONEBAS AIF   ('&LBL' EQ '' AND (&BUGEXT OR &BUGDYN OR &BUGTRC)).NOLBL 00053803
         DC    CL8'&LBL '                                               00053903
.NOLBL   AIF   ('&REGS' EQ '' OR '&REGS' EQ 'NO').NOREGS         95079  00054003
         AIF   ('&REGS' EQ 'R15' OR '&REGS' EQ 'SHORT'                 *00054103
               OR '&REGS' EQ 'RET').RETREG                      GP97225 00054203
         AIF   ('&REGS' EQ 'YES' OR '&REGS' EQ 'ALL').REGSALL   GP02246 00054303
         AIF   (N'&REGS EQ 2).REGS2                             GP97225 00054403
         DC    AL1(0,0),SL2(&REGS(1),&REGS(1))                  GP97225 00054503
         AGO   .NOREGS                                          GP97225 00054603
.REGS2   DC    AL1(0,0),SL2(&REGS(1),&REGS(2))                  GP97225 00054703
         AGO   .NOREGS                                          GP97225 00054803
.REGSALL DC    AL1(0,0),SL2(0,15)                               GP97225 00054903
         AGO   .NOREGS                                          GP97225 00055003
.RETREG  DC    SL2(0,14,1)    R15-R1 ONLY                       GP97225 00055103
.NOREGS  AIF   ('&TEXT' EQ '').NOTEXT                                   00055203
         AIF   (N'&TEXT GE 2).TEXT2                             GP97225 00055303
         DC    AL1(1,0),SL2(&TEXT(1)),AL2(&L&TEXT(1))           GP97225 00055403
         AGO   .NOTEXT                                          GP97225 00055503
.TEXT2   DC    AL1(1,0),SL2(&TEXT(1),&TEXT(2))                          00055603
.NOTEXT  AIF   ('&CTEXT' EQ '').NOCTEXT                         GP97225 00055703
         AIF   (N'&CTEXT GE 2).CTEXT2                           GP97225 00055803
         DC    AL1(2,0),SL2(&CTEXT(1)),AL2(&L&CTEXT(1))         GP97225 00055903
         AGO   .NOCTEXT                                         GP97225 00056003
.CTEXT2  DC    AL1(2,0),SL2(&CTEXT(1),&CTEXT(2))                GP97225 00056103
.NOCTEXT AIF   ('&HEX' EQ '').NOHEX                             GP97225 00056203
         AIF   (N'&HEX GE 2).HEX2                               GP97225 00056303
         DC    AL1(3,0),SL2(&HEX(1)),AL2(&L&HEX(1))             GP97225 00056403
         AGO   .NOHEX                                           GP97225 00056503
.HEX2    DC    AL1(3,0),SL2(&HEX(1),&HEX(2))                    GP97225 00056603
.NOHEX   AIF   ('&PACK' EQ '').NOPACK                           GP97225 00056703
         AIF   (N'&PACK GE 2).PACK2                             GP97225 00056803
         DC    AL1(4,0),SL2(&PACK(1)),AL2(&L&PACK(1))           GP97225 00056903
         AGO   .NOPACK                                          GP97225 00057003
.PACK2   DC    AL1(4,0),SL2(&PACK(1),&PACK(2))                  GP97225 00057103
.NOPACK  AIF   ('&LIST' EQ '' OR N'&LIST LT 1).NOLIST           GP95235 00057203
&LN      SETA  N'&LIST                                          GP95235 00057303
.DOLIST  AIF   (&I GE &LN).NOLIST   DONE WITH LIST              GP95235 00057403
&I       SETA  &I+1          BUMP LOOP INDEX                    GP95235 00057503
&EN      SETA  K'&EK         GET LENGTH                         GP04234 00057603
         MACLIST &LIST(&I)   GET SUBLIST ITEMS                  GP04234 00057703
&EN      SETA  &MACP#        NUMBER OF ENTRIES (CHG FOR XF ASM) GP04234 00057803
         AIF   (&EN LT 1).DOLIST  USER IN COMA?                 GP95235 00057903
         AIF   (&EN LT 4).TOOLIST WARN                          GP95235 00058003
         MNOTE 4,'MORE THAN 3 SUBPARMS IN &LIST(&I) '           GP95235 00058103
.TOOLIST ANOP  ,                                                GP95235 00058203
&EK      SETC  '&MACP1'                                         GP04234 00058303
&EM      SETA  K'&EK         LENGTH OF FIRST OPERAND                    00058403
&EO      SETA  0             PRESET FOR NORMAL ADDRESSING MODE          00058503
&ET      SETC  '03'          PRESET FOR HEX DEFAULT             GP95235 00058603
         AIF   (&EM GT 0).TPFX                                  GP04234 00058703
&EK      SETC  '0'           ALLOW EXPANSION WITHOUT ERROR      GP04234 00058803
&EM      SETA  1                                                GP04234 00058903
         MNOTE 4,'DEBTRACE: PARAMETER &I REQUIRES AN ADDRESS'   GP04234 00059003
.TPFX    AIF   (&EM LT 2).NOTA31                                GP04234 00059103
         AIF   ('&EK'(1,1) NE '/').NOTIND                               00059203
&EO      SETA  &EO+1         REQUEST INDIRECT ADDRESSING                00059303
&EK      SETC  '&EK'(2,&EM-1)  DELETE LEADING CONTROL BYTE              00059403
&EM      SETA  K'&EK         LENGTH OF FIRST OPERAND                    00059503
.NOTIND  AIF   ('&EK'(&EM,1) NE '%').NOTA24                             00059603
&EO      SETA  &EO+2         REQUEST FORCED 24-BIT ADDRESSING           00059703
&EK      SETC  '&EK'(1,&EM-1)  DELETE TRAILING CONTROL BYTE             00059803
&EM      SETA  K'&EK         LENGTH OF FIRST OPERAND                    00059903
.NOTA24  AIF   ('&EK'(&EM,1) NE '?').NOTA31                             00060003
&EO      SETA  &EO+4         REQUEST FORCED 31-BIT ADDRESSING           00060103
&EK      SETC  '&EK'(1,&EM-1)  DELETE TRAILING CONTROL BYTE             00060203
&EM      SETA  K'&EK         LENGTH OF FIRST OPERAND                    00060303
.NOTA31  AIF   (&EN LT 3 OR '&MACP3' EQ 'HEX').HTYPE            GP95235 00060403
         AIF   ('&MACP3' EQ 'X').HTYPE                          GP97225 00060503
         AIF   ('&MACP3' EQ 'HEX').HTYPE                                00060603
         AIF   ('&MACP3' EQ 'T').TTYPE                          GP98189 00060703
         AIF   ('&MACP3' EQ 'TEXT').TTYPE                       GP95235 00060803
         AIF   ('&MACP3' EQ 'TXT').TTYPE                                00060903
         AIF   ('&MACP3' EQ 'C').CTYPE                          GP97225 00061003
         AIF   ('&MACP3' EQ 'CT').CTYPE                                 00061103
         AIF   ('&MACP3' EQ 'CTEXT').CTYPE                      GP97225 00061203
         AIF   ('&MACP3' EQ 'PACK').PTYPE                       GP97225 00061303
         AIF   ('&MACP3' EQ 'PACKED').PTYPE                     GP97225 00061403
         AIF   ('&MACP3' EQ 'P').PTYPE                          GP97225 00061503
         AIF   ('&MACP3' EQ 'PD').PTYPE                                 00061603
         AIF   ('&MACP3' EQ 'D').PTYPE                          GP97225 00061703
 MNOTE 4,'TYPE MUST BE TEXT, CTEXT, HEX, OR PACKED, NOT &MACP3'         00061803
         AGO   .HTYPE                                           GP95235 00061903
.TTYPE   ANOP  ,                                                GP95235 00062003
&ET      SETC  '01'          SET FOR TEXT                       GP95235 00062103
         AGO   .HTYPE                                           GP95235 00062203
.CTYPE   ANOP  ,                                                GP97225 00062303
&ET      SETC  '02'          SET FOR CONDITIONAL TEXT, ELSE HEX GP97225 00062403
         AGO   .HTYPE                                           GP97225 00062503
.PTYPE   ANOP  ,                                                GP97225 00062603
&ET      SETC  '04'          SET FOR PACKED                     GP97225 00062703
.HTYPE   ANOP  ,                                                GP97225 00062803
&EL      SETC  '&MACP2'                                         GP95235 00062903
         AIF   ('&EL' NE '').HLEN                               GP95235 00063003
&EL      SETC  '&L'.'&EK'                                               00063103
.HLEN    DC    X'0800',CL8'&MACP1',AL1(&ET,&EO),S(&EK,&EL)              00063203
         AGO   .DOLIST                                          GP95235 00063303
.NOLIST  AIF   (&BUGFAR).FARCL                                   95079  00063403
         AIF   (&BUGDYN).FARCL                                  GP97261 00063503
         AIF   (&BUGTRC).TRCCL                                  GP99113 00063603
         AIF   (&BUGEXT).EXTCL                                   95215  00063703
&V.B     BAL   R14,DBTRACE                                       92271  00063803
         AGO   .CMCAL                                            95079  00063903
.EXTCL   ANOP  ,                                                 95215  00064003
&V.B     L     R15,=V(DEBTRACE)    MEMBER DEBTROLD              GP05013 00064103
         LA    R0,&WA        PASS ADDRESS OF WORK AREA           95215  00064203
         AGO   .FARCM                                            95215  00064303
.FARCL   ANOP  ,                                                 95079  00064403
&V.B     L     R15,=A(DBTRACE)                                   95079  00064503
.FARCM   BASR  R14,R15                                           95079  00064603
.CMCAL   ANOP  ,                                                 95079  00064703
&V.X     LM    R0,R15,&WA                                               00064803
         AGO   .MEND                                                    00064903
.TRCCL   ANOP  ,             INVOKE PGMTRACE VIA ESPIE          GP99113 00065003
&V.X     DS    0H            END OF TRACE LIST                  GP99113 00065103
         AGO   .MEND                                            GP99113 00065203
.SWON    OI    DBTFLAG,DBTFLON  SET TRACING ON                   95079  00065303
         AGO   .MEND                                             95079  00065403
.SWEND   OI    DBTFLAG,DBTFLEND  CLOSE DCB AND STOP TRACE       GP98222 00065503
.SWOFF   NI    DBTFLAG,255-DBTFLON  SET TRACING OFF              95079  00065603
         AGO   .MEND                                             95079  00065703
.CODE    AIF   (&BUGFAR OR &BUGEXT).TESTDC                      GP97262 00065803
         AIF   ('&NM' EQ '').NONAME                                     00065903
&NM      DS    0H                                                       00066003
.NONAME  AIF   (NOT &BUGDYN).NOLODYN                            GP97262 00066103
         AIF   ('&MODE' NE 'DC').NOPUP                          GP00004 00066203
         PUSH  PRINT                                            GP00004 00066303
         PUSH  USING                                            GP00004 00066403
.*       PRINT GEN                                              GP00004 00066503
         DROP  ,                                                GP00004 00066603
         USING DBTRACE,R15                                      GP97265 00066703
.NOPUP   ANOP  ,                                                GP00004 00066803
DBTRACE  LA    R0,&WA        PASS ADDRESS OF WORK AREA          GP97262 00066903
         STM   R12,R1,DBTLOCSV  SAVE BASE AND RETURN            GP97265 00067003
         ICM   R15,15,@DEBTRAC  SEE IF PREVIOUSLY LOADED        GP97265 00067103
         BNZR  R15           INVOKE; RETURN VIA R14 TO CALLER   GP97265 00067203
         AIF   ('&MODE' NE 'DC').NODRP                          GP00004 00067303
         BASR  R12,0         MAKE LOCAL BASE                    GP97262 00067403
         DROP  R15                                              GP97265 00067503
         USING *,R12                                            GP97265 00067603
         AGO   .CMDRP                                           GP00004 00067703
.NODRP   MVC   #DEBTRAC,=CL8'DEBTROLD'                          GP00004 00067803
.CMDRP   LOAD  EPLOC=#DEBTRAC  LOAD EXTERNAL MODULE             GP97261 00067903
         ST    R0,@DEBTRAC   SAVE FOR NEXT TIME                 GP97261 00068003
.*FAILS* AIF   ('&MODE' NE 'DC').NOCLB                          GP00004 00068103
         ST    R0,DBTLOCSV+(15-12)*4  UPDATE TARGET ADDRESS     GP97265 00068203
  MACPARM DBTFLAG,(&OPT),NULL=&BUGSWCH*DBTFLON+DBTFLWID,OP=MVI,OPR=MVI  00068303
.NOCLB   LM    R12,R1,DBTLOCSV  RESTORE                         GP97265 00068403
         BR    R15           RETURN TO CALLER VIA R14           GP97262 00068503
         AIF   ('&MODE' NE 'DC').NOPOP                          GP00004 00068603
         POP   USING                                            GP97262 00068703
         POP   PRINT                                            GP97262 00068803
.NOPOP   AGO   .TESTDC                                          GP97262 00068903
.NOLODYN MNOTE 4,'INLINE EXPANSION NOT SUPPORTED - USE MACRO DEBINLIN'  00069003
.TESTDC  AIF   ('&MODE' NE 'DC').MEND                            95067  00069103
         AGO   .NODRTE                                           95067  00069203
.DATA    AIF   ('&NM' EQ '').NODLBL                                     00069303
&NM      DS    0D                                                       00069403
.NODLBL  AIF   ('&ROUT' EQ '').NODRTE                            95067  00069503
&BUGSWRT SETB  1                                                 95067  00069603
.NODRTE  AIF   (&BUGFAR OR '&MODE' EQ 'M').MEND                  95230  00069703
DBTPRESV DC    2F'0'  1/2    FOR SHORT FORMATTING                95230  00069803
.NOSVPFX ANOP  ,                                                 95230  00069903
&WA      DC    16F'0' 2/2    DEBUG SAVE AREA                    GP97265 00070003
DBTFLAG  DC    AL1(&BUGSWCH*DBTFLON+DBTFLWID)  DEBUG FLAG       GP98222 00070103
DBTFLTCB EQU   128             INCLUDE TCB ADDRESS IN MESSAGE           00070203
DBTFLWTO EQU   64              USE WTO INSTEAD OF PRT            95240  00070303
DBTFLWID EQU   32              USE WIDE FORMAT WHEN PRINTING    GP98222 00070403
DBTFLPRO EQU   16              PRODUCTION MODE / NEED DD TO PRT GP99113 00070503
DBTFLEND EQU   2               THIS IS A TERMINATION CALL       GP98222 00070603
DBTFLON  EQU   1               DEBUG BIT                                00070703
DBTFLAG2 DC    AL1(0)        ..RESERVED..                       GP99062 00070803
DBTFLAG3 DC    AL1(0)        ..RESERVED..                       GP99062 00070903
DBTFLAG4 DC    AL1(0)        ..RESERVED..                       GP99062 00071003
DBTCNT1  DC    A(&COUNT(1)+0)  COUNT OPTION                      95228  00071103
DBTCNT2  DC    A(&COUNT(2)+0)  COUNT OPTION                      95228  00071203
DBTCNT3  DC    A(&COUNT(3)+0)  COUNT OPTION                      95228  00071303
         MAPCMPRT PFX=DBT,DCB=&DCB,PRTMODE=&PRTMODE,DEV=&DEV    GP99113 00071403
         AIF   (NOT &BUGDYN).NODYNS                             GP97261 00071503
.BUGDYN  WXTRN DEBTRACE      SUPPORT LINKED-IN VERSION          GP97262 00071603
@DEBTRAC DC    A(DEBTRACE)   ADDRESS OF LOADED DEBTRACE         GP97261 00071703
#DEBTRAC DC    CL8'DEBTROLD'  LOAD MODULE NAME                  GP97261 00071803
DBTLOCSV DC    6A(0)         SAVE AREA                          GP97265 00071903
         AGO   .MEND                                            GP97262 00072003
.NODYNS  AIF   (&BUGFAR OR '&MODE' EQ 'M').MEND                  95228  00072103
DBTLOCSV DC    4F'0'         BASE SAVE                                  00072203
DBTWTO   DC    Y(56,0)       VCON                                93357  00072303
DBTWTOM  DC    C'MSG666 '    DEBUG HEADER                        93357  00072403
DBTWTON  DC    CL8' ',C' '   USER'S LABEL                               00072503
DBTWTOT  DC    CL36' '       USER'S HEX OR EBCDIC TEXT                  00072603
         AIF   ('&TCB' NE 'YES' AND NOT &BUGTCB).NOTCB           94011  00072703
         DC    C' '          EXTRA FOR UNPACK                    94011  00072803
DBTWTCB  DC    CL8' '        CURRENT TCB ADDRESS                 94011  00072903
.NOTCB   ANOP  ,                                                 94011  00073003
DBTWTOC  DC    C' '          EXTRA FOR UNPACK                           00073103
         AIF   (NOT &BUGSWRT).MEND  NO ROUTING CODE              95067  00073203
         DC    XL3'0'        EXTRA FOR DESCRIPTOR/ROUTING CODES  95067  00073303
.MEND    MEND  ,                                                        00073403
./ ADD NAME=FD       0112-02308-10240-1607-00152-00130-00024-GERHARD 00 00073503
         MACRO                                                          00073603
&NM      FD    &STR,&EXLEN,&VALUES,&LEN=0,&TYPE=                GP03287 00073703
.*--------------------------------------------------------------------* 00073803
.*   TYPE= ADDED FOR FDFLAG AND FDBAR SUPPORT                         * 00073903
.*--------------------------------------------------------------------* 00074003
         GBLA  &FDCNTR                                                  00074103
         LCLA  &C,&I,&J,&K,&N                                           00074203
         LCLA  &T,&E,&O7,&O9,&DATA                                      00074303
         LCLC  &L,&FDCHAIN,&W                                           00074403
         LCLB  &NOP,&IN,&SKPLEN,&INDAD,&CNLOCK                   92086  00074503
         LCLB  &NL,&DEBL,&DEBR,&DEBZ,&PADL,&PADR,&RADJ,&UP              00074603
         LCLB  &BLUE,&GREEN,&PINK,&RED,&TURQ,&WHITE,&YELLOW,&C1,&C2,&C3 00074703
         LCLB  &UNDER,&BLINK,&REVERSE,&MDT,&MONO                 87313  00074803
         LCLB  &INTENSE,&DETECT,&NONDISP,&NUMERIC,&SKIP,&PROTECT        00074903
         LCLB  &DEFAULT,&PREV                                           00075003
&FDCNTR  SETA  &FDCNTR+1                                                00075103
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                          00075203
&C       SETA  &FDCNTR+1                                                00075303
         AIF   ('&NM' EQ '').NONAME                                     00075403
&NM      EQU   *                                                        00075503
.NONAME  AIF   ('&STR' NE 'END' AND '&STR' NE '*END').PROCESS           00075603
&FDCHAIN DC    AL1(0)        END OF FD LIST                             00075703
         MEXIT ,                                                        00075803
.PROCESS ANOP  ,                                                        00075903
&I       SETA  N'&SYSLIST                                               00076003
&J       SETA  1                                                        00076103
         AIF   ('&STR'(1,1) NE '''').PRMLOOP                    GP04048 00076203
&SKPLEN  SETB  1                                                        00076303
.*--------------------------------------------------------------------* 00076403
.*   LOOP THROUGH POSITIONAL PARAMETERS:                              * 00076503
.*   #1 - VARIABLE NAME OR QUOTED STRING                              * 00076603
.*   #2 - IF UNRECOGNIZED, EXPLICIT VARIABLE LENGTH                   * 00076703
.*--------------------------------------------------------------------* 00076803
.PRMLOOP AIF   (&J GE &I).CHECK                                 GP04048 00076903
&J       SETA  &J+1                                                     00077003
&L       SETC  '&SYSLIST(&J)'                                           00077103
         AIF   ('&L' EQ '').PRMLOOP                             GP04048 00077203
&NOP     SETB  (&NOP  OR '&L' EQ 'NOP')                                 00077303
&CNLOCK  SETB  (&CNLOCK OR '&L' EQ 'LOCK' OR '&L' EQ 'LOCKED')   92086  00077403
&NL      SETB  (&NL  OR '&L' EQ 'NL' OR '&L' EQ 'NEWLINE')              00077503
&DEBL SETB (&DEBL OR '&L' EQ 'DEBL' OR '&L' EQ 'DEB' OR '&L' EQ 'DEBZ') 00077603
&DEBR    SETB  (&DEBR OR '&L' EQ 'DEBR' OR '&L' EQ 'DEB')               00077703
&DEBZ    SETB  (&DEBZ OR '&L' EQ 'DEBZ')                                00077803
&PADL    SETB  (&PADL OR '&L' EQ 'PADL' OR '&L' EQ 'PAD')               00077903
&PADR    SETB  (&PADR OR '&L' EQ 'PADR' OR '&L' EQ 'PAD')               00078003
&RADJ    SETB  (&RADJ OR '&L' EQ 'RADJ')                                00078103
&UP    SETB  (&UP OR '&L' EQ 'UP')                                      00078203
&UP      SETB  (&UP OR '&L' EQ 'UPPER')                                 00078303
&BLUE    SETB  (&BLUE OR '&L' EQ 'BLUE')                                00078403
&GREEN   SETB  (&GREEN OR '&L' EQ 'GREEN')                              00078503
&PINK    SETB  (&PINK OR '&L' EQ 'PINK')                                00078603
&RED     SETB  (&RED  OR '&L' EQ 'RED')                                 00078703
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TURQ' OR '&L' EQ 'CYAN')      90326  00078803
&WHITE   SETB  (&WHITE OR '&L' EQ 'WHITE')                              00078903
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YELLOW')                            00079003
&BLUE    SETB  (&BLUE OR '&L' EQ 'BL')                          GP10240 00079103
&GREEN   SETB  (&GREEN OR '&L' EQ 'GR')                         GP10240 00079203
&PINK    SETB  (&PINK OR '&L' EQ 'PI')                          GP10240 00079303
&RED     SETB  (&RED  OR '&L' EQ 'RE')                          GP10240 00079403
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TU' OR '&L' EQ 'CY')         GP10240 00079503
&WHITE   SETB  (&WHITE OR '&L' EQ 'WH')                         GP10240 00079603
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YE')                        GP10240 00079703
&MONO    SETB  (&MONO OR '&L' EQ 'MONO')                         87313  00079803
&UNDER   SETB  (&UNDER OR '&L' EQ 'UL' OR '&L' EQ 'UNDER')              00079903
&BLINK   SETB  (&BLINK OR '&L' EQ 'BLINK')                              00080003
&REVERSE SETB  (&REVERSE OR '&L' EQ 'REVERSE')                          00080103
&INTENSE SETB  (&INTENSE OR '&L' EQ 'INTENSE' OR '&L' EQ 'INT')         00080203
&DETECT  SETB  (&DETECT OR '&L' EQ 'DETECT' OR '&L' EQ 'LP')            00080303
&NONDISP SETB  (&NONDISP OR '&L' EQ 'NONDISP' OR '&L' EQ 'NDISP')       00080403
&NUMERIC SETB  (&NUMERIC OR '&L' EQ 'NUMERIC' OR '&L' EQ 'NUM')         00080503
&SKIP    SETB  (&SKIP OR '&L' EQ 'SKIP')                                00080603
&PROTECT SETB  (&PROTECT OR '&L' EQ 'PROTECT')                          00080703
&MDT     SETB  (&MDT OR '&L' EQ 'MDT')                           87313  00080803
&DEFAULT SETB  (&DEFAULT OR '&L' EQ 'DEFAULT' OR '&L' EQ 'DFLT')        00080903
&PREV    SETB  (&PREV OR '&L' EQ 'PREVIOUS' OR '&L' EQ 'PREV')          00081003
&INDAD   SETB  (&INDAD OR '&L' EQ '*')                           81270  00081103
         AIF   (K'&L GT 4).OMLEN                                        00081203
&K       SETA  0                                                        00081303
&L       SETC  '&L'.'    '                                              00081403
&L       SETC  '&L'(1,4)                                                00081503
         AIF   ('&L' NE 'X   ').DT                              GP10240 00081603
&L       SETC  'HEX '                                           GP10240 00081703
.DT      AIF   (&K GE 35).OMLEN                                 GP07004 00081803
&K       SETA  &K+1                                                     00081903
&N       SETA  (&K-1)*4+1                                               00082003
&W      SETC  'CHARCON ASISADDRHEX SHEXBIT I   $I  D   $D  F   TIMETIMD*00082103
               DATEDATJWDAYMTH DAY MD  DMY MDY CHEXICM ICN IZ  IA  DCM *00082203
               DCN DZ  DA  EDATDATDCCHHTTR '(&N,4)              GP07004 00082303
         AIF   ('&L' NE '&W').DT                                        00082403
&DATA    SETA  &K                                                       00082503
         AIF   (&J EQ 2).OMSET                                          00082603
         AGO   .PRMLOOP                                         GP04048 00082703
.OMLEN   AIF   (&J NE 2 OR &SKPLEN).PRMLOOP                     GP04048 00082803
 AIF (&NOP OR &IN OR &NL OR &DEBL OR &DEBR OR &DEBZ OR &PADL).OMSET     00082903
 AIF (&UP OR &PADR OR &RADJ OR &BLUE OR &GREEN OR &PINK OR &RED).OMSET  00083003
 AIF (&TURQ OR &WHITE OR &YELLOW OR &UNDER OR &BLINK).OMSET             00083103
 AIF (&REVERSE OR &INTENSE OR &DETECT OR &NONDISP OR &INDAD).OMSET      00083203
 AIF (&NUMERIC OR &SKIP OR &PROTECT OR &DEFAULT OR &PREV).OMSET         00083303
         AIF   (&MDT OR &MONO OR &CNLOCK).OMSET                  92086  00083403
         AGO   .PRMLOOP                                         GP04048 00083503
.OMSET   ANOP  ,             EXPLICIT LENGTH OMITTED                    00083603
&SKPLEN  SETB  1             USE L'                                     00083703
         AGO   .PRMLOOP                                         GP04048 00083803
.*--------------------------------------------------------------------* 00083903
.*   END OF PARAMETER LOOP                                            * 00084003
.*--------------------------------------------------------------------* 00084103
.CHECK   AIF   ('&TYPE' EQ '').NOTYPE                           GP03287 00084203
.*FDBAR  AIF   (T'&TYPE NE 'N').NOTYPE                          GP03287 00084303
&DATA    SETA  &TYPE                                            GP03287 00084403
.NOTYPE  ANOP  ,                                                GP04048 00084503
&T       SETA  128*&IN+64*&NOP+8*&CNLOCK+&INDAD                  92086  00084603
&E SETA 128*&NL+64*&DEBL+32*&DEBR+16*&DEBZ+8*&PADL+4*&PADR+2*&RADJ+&UP  00084703
&O7      SETA  128*&DEFAULT+64*&PREV                                    00084803
&PROTECT SETB  (&PROTECT OR &SKIP)                                      00084903
&NUMERIC SETB  (&NUMERIC OR &SKIP)                                      00085003
&DETECT  SETB  (&DETECT  OR &NONDISP)                                   00085103
&INTENSE SETB  (&INTENSE OR &NONDISP)                                   00085203
&O7      SETA  &O7+32*&PROTECT+16*&NUMERIC+8*&INTENSE+4*&DETECT+&MDT    00085303
&C1      SETB  (&GREEN OR &TURQ OR &WHITE OR &YELLOW)                   00085403
&C2      SETB  (&RED OR &PINK OR &WHITE OR &YELLOW)                     00085503
&C3      SETB  (&BLUE OR &PINK OR &TURQ OR &WHITE)                      00085603
&O9      SETA  64*&C1+32*&C2+16*&C3+8*&MONO+4*&UNDER+2*&REVERSE+&BLINK  00085703
         AIF   (&O7 NE 0 OR &O9 NE 0).LONG                       81138  00085803
&T       SETA  &T+48         USE FDPRT SHORT FORM                81138  00085903
         AIF   ('&STR'(1,1) EQ '''').CSTRPRT                     81138  00086003
         AIF   ('&EXLEN' NE '' AND NOT &SKPLEN).PRTLEN           81138  00086103
&L       SETC  'L'''                                             81138  00086203
&FDCHAIN DC    AL1(ZFD&C-*,&T,&E,&DATA,&LEN,&L&STR),SL2(&STR)    81138  00086303
         MEXIT ,                                                 81138  00086403
.PRTLEN  AIF   (K'&EXLEN LT 2).NOPLEN                            81270  00086503
         AIF   ('&EXLEN'(1,1) NE '(' OR '&EXLEN'(2,1) EQ '(').NOPLEN    00086603
&T       SETA  &T+2          ILEN IS REGISTER FORM               81270  00086703
.NOPLEN  ANOP  ,                                                 81270  00086803
&FDCHAIN DC    AL1(ZFD&C-*,&T,&E,&DATA,&LEN,&EXLEN),SL2(&STR)    81138  00086903
         MEXIT ,                                                 81138  00087003
.CSTRPRT ANOP  ,                                                 81138  00087103
&FDCHAIN DC    AL1(ZFD&C-*,&T,&E,129,&LEN,ZFD&C-*-1),C&STR       81138  00087203
         MEXIT ,                                                 81138  00087303
.LONG    AIF   ('&STR'(1,1) EQ '''').CSTRING                     81138  00087403
         AIF   ('&EXLEN' NE '' AND NOT &SKPLEN).EXLEN                   00087503
&L       SETC  'L'''                                                    00087603
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&O9,&O7,&DATA,&LEN,&L&STR),SL2(&STR)      00087703
         MEXIT                                                          00087803
.EXLEN   AIF   (K'&EXLEN LT 2).NORLEN                            81270  00087903
         AIF   ('&EXLEN'(1,1) NE '(' OR '&EXLEN'(2,1) EQ '(').NORLEN    00088003
&T       SETA  &T+2          ILEN IS REGISTER FORM               81270  00088103
.NORLEN  ANOP  ,                                                 81270  00088203
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&O9,&O7,&DATA,&LEN,&EXLEN),SL2(&STR)      00088303
         MEXIT ,                                                        00088403
.CSTRING ANOP  ,                                                        00088503
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&O9,&O7,129,&LEN,ZFD&C-*-1),C&STR         00088603
         MEND  ,                                                        00088703
./ ADD NAME=FDCLC                                                       00088803
         MACRO                                                          00088903
&NM      FDCLC &STR,&STR2,&LEN,&BE=0,&BL=0,&BH=0,&BNE=0                 00089003
         GBLA  &FDCNTR                                                  00089103
         LCLA  &T,&C                                                    00089203
         LCLB  &NOP                                              81133  00089303
         LCLC  &FDCHN,&FL,&FH,&L                                        00089403
&FDCNTR  SETA  &FDCNTR+1                                                00089503
&FDCHN SETC  'ZFD'.'&FDCNTR'                                            00089603
&C       SETA  &FDCNTR+1                                                00089703
         AIF   ('&NM' EQ '').NONAME                                     00089803
&NM      EQU   *                                                        00089903
.NONAME  AIF   ('&STR' NE 'END' AND '&STR' NE '*END').PROCESS           00090003
&FDCHN DC    AL1(0)        END OF FD LIST                               00090103
         MEXIT ,                                                        00090203
.PROCESS ANOP  ,                                                        00090303
&FL      SETC  '&BL'                                                    00090403
&FH      SETC  '&BH'                                                    00090503
         AIF   ('&BNE' EQ '0').CHECK                                    00090603
&FL      SETC  '&BNE'                                                   00090703
&FH      SETC  '&BNE'                                                   00090803
         AIF   ('&BL' EQ '0' AND '&BH' EQ '0').CHECK                    00090903
         MNOTE 8,'*** MUTUALLY EXCLUSIVE BNE AND BL/BH'                 00091003
.CHECK   ANOP  ,                                                        00091103
&T       SETA  64*&NOP+21                                               00091203
         AIF   (T'&LEN EQ 'O').NOL                                      00091303
&FDCHN DC AL1(ZFD&C-*,&T),SL2(&BE,&FL,&FH,&STR),AL1(&LEN),SL2(&STR2)    00091403
         MEXIT ,                                                        00091503
.NOL     ANOP                                                           00091603
&L       SETC  'L'''                                                    00091703
&FDCHN DC AL1(ZFD&C-*,&T),SL2(&BE,&FL,&FH,&STR),AL1(&L&STR),SL2(&STR2)  00091803
         MEND  ,                                                        00091903
./ ADD NAME=FDCLI                                                       00092003
         MACRO                                                          00092103
&NM      FDCLI &STR,&MASK,&BE=0,&BL=0,&BH=0,&BNE=0                      00092203
         GBLA  &FDCNTR                                                  00092303
         LCLA  &T,&C                                                    00092403
         LCLB  &NOP                                              81133  00092503
         LCLC  &FDCHAIN,&FL,&FH                                         00092603
&FDCNTR  SETA  &FDCNTR+1                                                00092703
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                          00092803
&C       SETA  &FDCNTR+1                                                00092903
         AIF   ('&NM' EQ '').NONAME                                     00093003
&NM      EQU   *                                                        00093103
.NONAME  AIF   ('&STR' NE 'END' AND '&STR' NE '*END').PROCESS           00093203
&FDCHAIN DC    AL1(0)        END OF FD LIST                             00093303
         MEXIT ,                                                        00093403
.PROCESS ANOP  ,                                                        00093503
&FL      SETC  '&BL'                                                    00093603
&FH      SETC  '&BH'                                                    00093703
         AIF   ('&BNE' EQ '0').CHECK                                    00093803
&FL      SETC  '&BNE'                                                   00093903
&FH      SETC  '&BNE'                                                   00094003
         AIF   ('&BL' EQ '0' AND '&BH' EQ '0').CHECK                    00094103
         MNOTE 8,'*** MUTUALLY EXCLUSIVE BNE AND BL/BH'                 00094203
.CHECK   ANOP  ,                                                        00094303
&T       SETA  64*&NOP+20                                               00094403
&FDCHAIN DC AL1(ZFD&C-*,&T),SL2(&BE,&FL,&FH,&STR),AL1(&MASK)            00094503
         MEND  ,                                                        00094603
./ ADD NAME=FDFLAG   0105-03284-03287-1425-00019-00027-00014-GERHARD 00 00094703
         MACRO                                                          00094803
&NM      FDFLAG &O1,&O2,&O3,&O4,&O5,&O6,&O7,&O8,&O9,&O10,&O11,&O12,&O13*00094903
               ,&O14,&O15,&O16,&LEN=0,                                 *00095003
               &TABLE='?',&SEP=C',',&SPACE=0                            00095103
.*--------------------------------------------------------------------* 00095203
.*   FDFLAG REQUESTS TABLE LOOKUP. THE TABLE IS DEFINED WITH MACRO    * 00095303
.*     FLGTAB (FLAG BITS,TEXT,MLEN=L'FLAG)                            * 00095403
.*     THE EXPANSION IS THE SAME AS FOR A REGULAR FD, FOLLOWED BY THE * 00095503
.*     TABLE ADDRESS AND THE ATTRIBUTES:                              * 00095603
.*   OPERANDS ARE: S(TABLE ADDRESS) AL1(SEP CHAR OR 0) AL1(ADDL SPC)  * 00095703
.*--------------------------------------------------------------------* 00095803
&NM      FD    &O1,&O2,&O3,&O4,&O5,&O6,&O7,&O8,&O9,&O10,&O11,&O12,&O13,*00095903
               &O14,&O15,&O16,LEN=&LEN,TYPE=64                          00096003
         AIF   ('&O1' EQ 'END' OR '&O1' EQ '*END').MEND                 00096103
         AIF   ('&SEP' EQ 'NO' OR '&SEP' EQ 'NONE').NONO                00096203
         DC    SL2(&TABLE),AL1(&SEP,&SPACE)                             00096303
         AGO   .MEND                                                    00096403
.NONO    DC    SL2(&TABLE),AL1(0,&SPACE)                                00096503
.MEND    MEND  ,                                                        00096603
./ ADD NAME=FDGOTO                                                      00096703
         MACRO                                                          00096803
&NM      FDGOTO &S                                                      00096903
         GBLA  &FDCNTR                                                  00097003
         LCLA  &T,&C                                                    00097103
         LCLC  &FDCHN                                                   00097203
&FDCNTR  SETA  &FDCNTR+1                                                00097303
&FDCHN   SETC  'ZFD'.'&FDCNTR'                                          00097403
&C       SETA  &FDCNTR+1                                                00097503
         AIF   ('&NM' EQ '').NONAME                                     00097603
&NM      EQU   *                                                        00097703
.NONAME  AIF   ('&S' NE 'END' AND '&S' NE '*END').PROCESS               00097803
&FDCHN   DC    AL1(0)        END OF FD LIST                             00097903
         MEXIT ,                                                        00098003
.PROCESS ANOP  ,                                                        00098103
&T       SETA  16                                                       00098203
&FDCHN   DC    AL1(ZFD&C-*,&T),SL2(&S)                                  00098303
         MEND  ,                                                        00098403
./ ADD NAME=FDINP    8003-07004-10240-1611-00181-00165-00000-GERHARD 00 00098503
         MACRO                                                          00098603
&NM      FDINP &S,&EXLEN,&VALUES,&LEN=0,&MAX=,&EXIT=             89095  00098703
         GBLA  &FDCNTR,&FDOFFS                                   84146  00098803
         LCLA  &C,&I,&J,&K,&N,&U                                 84146  00098903
         LCLA  &T,&E,&O7,&O9,&DATA,&EXFG                         89095  00099003
         LCLC  &L,&FDCHAIN,&W,&UEX                               89095  00099103
         LCLB  &NOP,&IN,&SKPLEN,&INDAD                           81270  00099203
         LCLB  &NL,&DEBL,&DEBR,&DEBZ,&PADL,&PADR,&RADJ,&UP              00099303
         LCLB  &BLUE,&GREEN,&PINK,&RED,&TURQ,&WHITE,&YELLOW,&C1,&C2,&C3 00099403
         LCLB  &UNDER,&BLINK,&REVERSE,&MONO,&MDT                 87313  00099503
         LCLB  &INTENSE,&DETECT,&NONDISP,&NUMERIC,&SKIP,&PROTECT,&NULL  00099603
         LCLB  &DEFAULT,&PREV                                           00099703
&U       SETA  &FDOFFS                                           84146  00099803
&UEX     SETC  'AL2('.'&U'.')'                                   89095  00099903
&FDCNTR  SETA  &FDCNTR+1                                                00100003
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                          00100103
&C       SETA  &FDCNTR+1                                                00100203
         AIF   ('&S' EQ '*END' OR '&S' EQ 'END').NOOFF  END OF CHAIN    00100303
         AIF   ('&S' NE '*EXPAND').NOEXPND                       84214  00100403
         AIF   ('&NM' EQ '').NOXNAM                              84214  00100503
&NM      DC    (&FDOFFS)X'00'                                    84214  00100603
         AGO   .NOXCOM                                           84214  00100703
.NOXNAM  AIF   (&FDOFFS LT 1).NOXCOM                             84214  00100803
         DC    (&FDOFFS)X'00'                                    84214  00100903
.NOXCOM  AIF   ('&SYSLIST(2)' EQ 'NORESET').MEND                 84214  00101003
&FDOFFS  SETA  0                                                 84214  00101103
         AGO   .MEND                                             84214  00101203
.NOEXPND AIF   (T'&EXIT EQ 'O').NOUEX                            89095  00101303
         AIF   (N'&EXIT GE 2).CKUEXR                            GP07008 00101403
&UEX     SETC  '&UEX'.',SL2('.'&EXIT'.')'                        89095  00101503
&EXFG    SETA  1                                                 89095  00101603
         AGO   .NOUEX                                           GP07008 00101703
.CKUEXR  AIF   ('&EXIT(1)' NE 'R').BADUEX                       GP07008 00101803
&UEX     SETC  '&UEX'.',SL2('.'&EXIT(2)'.'-*)'                  GP07008 00101903
&EXFG    SETA  1                                                 89095  00102003
         AGO   .NOUEX                                           GP07008 00102103
.BADUEX  MNOTE 8,'FDIN: EXIT= ADDRESS MALFORMED'                GP07008 00102203
.NOUEX   AIF   (T'&MAX EQ 'O').NOMAXQ                            89095  00102303
         AIF   (T'&MAX EQ 'N').CKMAX                             89095  00102403
         MNOTE 4,'NON-NUMERIC MAX= NOT SUPPORTED'                89095  00102503
         AGO   .NOMAXQ                                           89095  00102603
.CKMAX   AIF   (&MAX LT 0).NOMAXQ                                89095  00102703
&I       SETA  &MAX                                              84146  00102803
         AGO   .HAVMAX                                           84146  00102903
.NOMAXQ  AIF   (T'&LEN NE 'N' OR '&LEN' EQ '0').NOLENQ           84146  00103003
         AIF   (&LEN LE 0).NOLENQ                                84146  00103103
&I       SETA  &LEN                                              84146  00103203
         AGO   .HAVMAX                                           84146  00103303
.NOLENQ  MNOTE 0,'LEN=/MAX= MISSING - WIDTH DEFAULTED TO 255'    84146  00103403
&I       SETA  255                                               84146  00103503
.HAVMAX  ANOP  ,                                                 84146  00103603
&FDOFFS  SETA  &FDOFFS+4+&I  SET OFFSET OF NEXT ENTRY            84146  00103703
         AIF   (K'&SYSLIST(0) LT 1 OR K'&SYSLIST(0) GT 4).NOOFF  84146  00103803
OFFS&SYSLIST(0) EQU &U,&I                                        84214  00103903
.NOOFF   AIF   ('&NM' EQ '').NONAME                              84146  00104003
&NM      EQU   *                                                        00104103
.NONAME  AIF   ('&S' NE 'END' AND '&S' NE '*END').PROCESS        84146  00104203
&FDCHAIN DC    AL1(0)        END OF FD LIST                             00104303
         MEXIT ,                                                        00104403
.PROCESS ANOP  ,                                                        00104503
&I       SETA  N'&SYSLIST                                               00104603
&J       SETA  1                                                        00104703
         AIF   ('&S'(1,1) NE '''').NOLIT                         84146  00104803
&SKPLEN  SETB  1                                                        00104903
.NOLIT   AIF   (&J GE &I).CHECK                                         00105003
&J       SETA  &J+1                                                     00105103
&L       SETC  '&SYSLIST(&J)'                                           00105203
         AIF   ('&L' EQ '').NOLIT                                       00105303
&NOP     SETB  (&NOP  OR '&L' EQ 'NOP')                                 00105403
&NL      SETB  (&NL  OR '&L' EQ 'NL' OR '&L' EQ 'NEWLINE')              00105503
&DEBL SETB (&DEBL OR '&L' EQ 'DEBL' OR '&L' EQ 'DEB' OR '&L' EQ 'DEBZ') 00105603
&DEBR    SETB  (&DEBR OR '&L' EQ 'DEBR' OR '&L' EQ 'DEB')               00105703
&DEBZ    SETB  (&DEBZ OR '&L' EQ 'DEBZ')                                00105803
&PADL    SETB  (&PADL OR '&L' EQ 'PADL' OR '&L' EQ 'PAD')               00105903
&PADR    SETB  (&PADR OR '&L' EQ 'PADR' OR '&L' EQ 'PAD')               00106003
&RADJ    SETB  (&RADJ OR '&L' EQ 'RADJ')                                00106103
&UP      SETB  (&UP OR '&L' EQ 'UP')                                    00106203
&UP      SETB  (&UP OR '&L' EQ 'UPPER')                                 00106303
&BLUE    SETB  (&BLUE OR '&L' EQ 'BLUE')                                00106403
&GREEN   SETB  (&GREEN OR '&L' EQ 'GREEN')                              00106503
&PINK    SETB  (&PINK OR '&L' EQ 'PINK')                                00106603
&RED     SETB  (&RED  OR '&L' EQ 'RED')                                 00106703
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TURQ' OR '&L' EQ 'CYAN')      90326  00106803
&WHITE   SETB  (&WHITE OR '&L' EQ 'WHITE')                              00106903
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YELLOW')                            00107003
&BLUE    SETB  (&BLUE OR '&L' EQ 'BL')                          GP10240 00107103
&GREEN   SETB  (&GREEN OR '&L' EQ 'GR')                         GP10240 00107203
&PINK    SETB  (&PINK OR '&L' EQ 'PI')                          GP10240 00107303
&RED     SETB  (&RED  OR '&L' EQ 'RE')                          GP10240 00107403
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TU' OR '&L' EQ 'CY')         GP10240 00107503
&WHITE   SETB  (&WHITE OR '&L' EQ 'WH')                         GP10240 00107603
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YE')                        GP10240 00107703
&MONO    SETB  (&MONO OR '&L' EQ 'MONO')                         87313  00107803
&UNDER   SETB  (&UNDER OR '&L' EQ 'UL' OR '&L' EQ 'UNDER')              00107903
&BLINK   SETB  (&BLINK OR '&L' EQ 'BLINK')                              00108003
&REVERSE SETB  (&REVERSE OR '&L' EQ 'REVERSE')                          00108103
&INTENSE SETB  (&INTENSE OR '&L' EQ 'INTENSE' OR '&L' EQ 'INT')         00108203
&DETECT  SETB  (&DETECT OR '&L' EQ 'DETECT' OR '&L' EQ 'LP')            00108303
&NONDISP SETB  (&NONDISP OR '&L' EQ 'NONDISP' OR '&L' EQ 'NDISP')       00108403
&NUMERIC SETB  (&NUMERIC OR '&L' EQ 'NUMERIC' OR '&L' EQ 'NUM')         00108503
&SKIP    SETB  (&SKIP OR '&L' EQ 'SKIP')                                00108603
&PROTECT SETB  (&PROTECT OR '&L' EQ 'PROTECT')                          00108703
&MDT     SETB  (&MDT OR '&L' EQ 'MDT')                           87313  00108803
&NULL    SETB  (&NULL OR '&L' EQ 'NULL')                         84146  00108903
&DEFAULT SETB  (&DEFAULT OR '&L' EQ 'DEFAULT' OR '&L' EQ 'DFLT')        00109003
&PREV    SETB  (&PREV OR '&L' EQ 'PREVIOUS' OR '&L' EQ 'PREV')          00109103
&INDAD   SETB  (&INDAD OR '&L' EQ '*')                           81270  00109203
         AIF   (&DATA NE 0).NOLIT                                       00109303
         AIF   (K'&L GT 4).OMLEN                                        00109403
&K       SETA  0                                                        00109503
&L       SETC  '&L'.'    '                                              00109603
&L       SETC  '&L'(1,4)                                                00109703
         AIF   ('&L' NE 'X   ').DT                              GP10240 00109803
&L       SETC  'HEX '                                           GP10240 00109903
.DT      AIF   (&K GE 35).OMLEN                                 GP07004 00110003
&K       SETA  &K+1                                                     00110103
&N       SETA  (&K-1)*4+1                                               00110203
&W      SETC  'CHARCON ASISADDRHEX SHEXBIT I   $I  D   $D  F   TIMETIMD*00110303
               DATEDATJWDAYMTH DAY MD  DMY MDY CHEXICM ICN IZ  IA  DCM *00110403
               DCN DZ  DA  EDATDATDCCHHTTR '(&N,4)              GP07004 00110503
         AIF   ('&L' NE '&W').DT                                        00110603
&DATA    SETA  &K                                                       00110703
         AIF   (&J EQ 2).OMSET                                          00110803
         AGO   .NOLIT                                                   00110903
.OMLEN   AIF   (&J NE 2 OR &SKPLEN).NOLIT                               00111003
 AIF (&NOP OR &IN OR &NL OR &DEBL OR &DEBR OR &DEBZ OR &PADL).OMSET     00111103
 AIF (&UP OR &PADR OR &RADJ OR &BLUE OR &GREEN OR &PINK OR &RED).OMSET  00111203
 AIF (&TURQ OR &WHITE OR &YELLOW OR &UNDER OR &BLINK OR &NULL).OMSET    00111303
 AIF (&REVERSE OR &INTENSE OR &DETECT OR &NONDISP OR &INDAD).OMSET      00111403
 AIF (&NUMERIC OR &SKIP OR &PROTECT OR &DEFAULT OR &PREV).OMSET         00111503
         AIF   (&MDT OR &MONO).OMSET                             87313  00111603
         AGO   .NOLIT                                                   00111703
.OMSET   ANOP  ,             EXPLICIT LENGTH OMITTED                    00111803
&SKPLEN  SETB  1             USE L'                                     00111903
         AGO   .NOLIT                                                   00112003
.CHECK   ANOP  ,                                                        00112103
&L       SETC  'L'''                                                    00112203
&IN      SETB  1                                                 84146  00112303
&T       SETA  128*&IN+64*&NOP+4*&EXFG+&INDAD+8                  89107  00112403
&E SETA 128*&NL+64*&DEBL+32*&DEBR+16*&DEBZ+8*&PADL+4*&PADR+2*&RADJ+&UP  00112503
&O7      SETA  128*&DEFAULT+64*&PREV+&MDT                        87313  00112603
&PROTECT SETB  (&PROTECT OR &SKIP)                                      00112703
&NUMERIC SETB  (&NUMERIC OR &SKIP)                                      00112803
&DETECT  SETB  (&DETECT  OR &NONDISP)                                   00112903
&INTENSE SETB  (&INTENSE OR &NONDISP)                                   00113003
&O7      SETA  &O7+32*&PROTECT+16*&NUMERIC+8*&INTENSE+4*&DETECT+2*&NULL 00113103
&C1      SETB  (&GREEN OR &TURQ OR &WHITE OR &YELLOW)                   00113203
&C2      SETB  (&RED OR &PINK OR &WHITE OR &YELLOW)                     00113303
&C3      SETB  (&BLUE OR &PINK OR &TURQ OR &WHITE)                      00113403
&O9      SETA  64*&C1+32*&C2+16*&C3+8*&MONO+4*&UNDER+2*&REVERSE+&BLINK  00113503
         AIF   ((&O7 NE 0 AND &O7 NE 128) OR &O9 NE 0).LONG      86244  00113603
&T       SETA  &T+48         USE FDPRT SHORT FORM                86244  00113703
         AIF   ('&S'(1,1) EQ '''').CSTRPRT                       86244  00113803
         AIF   ('&EXLEN' NE '' AND NOT &SKPLEN).EXLPRT           86244  00113903
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&DATA,&LEN,&L&S),SL2(&S),&UEX      89095  00114003
         MEXIT ,                                                 86244  00114103
.EXLPRT  AIF   (K'&EXLEN LT 2).NORLPRT                           86244  00114203
         AIF   ('&EXLEN'(1,1) NE '(' OR '&EXLEN'(2,1) EQ '(').NORLPRT   00114303
&T       SETA  &T+2          ILEN IS REGISTER FORM               86244  00114403
.NORLPRT ANOP  ,                                                 86244  00114503
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&DATA,&LEN,&EXLEN),SL2(&S),&UEX    89095  00114603
         MEXIT ,                                                 86244  00114703
.CSTRPRT ANOP  ,                                                 86244  00114803
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,1,&LEN,&L.ZFD&C.C),SL2(ZFD&C.C),&UEX      00114903
ZFD&C.C  DC    C&S                                               89095  00115003
         MEXIT ,                                                 86244  00115103
.LONG    AIF   ('&S'(1,1) EQ '''').CSTRING                       86244  00115203
         AIF   ('&EXLEN' NE '' AND NOT &SKPLEN).EXLEN                   00115303
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&O9,&O7,&DATA,&LEN,&L&S),SL2(&S),&UEX     00115403
         MEXIT                                                          00115503
.EXLEN   AIF   (K'&EXLEN LT 2).NORLEN                            81270  00115603
         AIF   ('&EXLEN'(1,1) NE '(' OR '&EXLEN'(2,1) EQ '(').NORLEN    00115703
&T       SETA  &T+2          ILEN IS REGISTER FORM               81270  00115803
.NORLEN  ANOP  ,                                                 81270  00115903
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&O9,&O7,&DATA,&LEN,&EXLEN),SL2(&S),&UEX   00116003
         MEXIT ,                                                        00116103
.CSTRING ANOP  ,                                                        00116203
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&O9,&O7,1,&LEN,&L.ZFD&C.C),SL2(ZFD&C.C),&*00116303
               UEX                                               89095  00116403
ZFD&C.C  DC    C&S                                               89095  00116503
.MEND    MEND  ,                                                 84214  00116603
./ ADD NAME=FDOPT                                                       00116703
         MACRO                                                          00116803
&NM      FDOPT &S,&SBA=,&CUR=,&CC=,&IND=                         81270  00116903
         GBLA  &FDCNTR                                                  00117003
         LCLA  &C,&I,&J,&K,&N                                           00117103
         LCLA  &T,&E,&O7,&O9,&WCC                                       00117203
         LCLC  &SB1,&CU1,&L,&FDCHN,&COM1,&COM2                          00117303
         LCLB  &NOP,&OPT,&SKPLEN                                        00117403
         LCLB  &NL,&WCCP,&SBAP,&CURP,&CCP,&INDP,&ALARM,&O79      81270  00117503
         LCLB  &BLUE,&GREEN,&PINK,&RED,&TURQ,&WHITE,&YELLOW,&C1,&C2,&C3 00117603
         LCLB  &UNDER,&BLINK,&REVERSE,&MONO,&MDT                 87313  00117703
         LCLB  &INTENSE,&DETECT,&NONDISP,&NUMERIC,&SKIP,&PROTECT        00117803
         LCLB  &DEFAULT,&PREV                                           00117903
&FDCNTR  SETA  &FDCNTR+1                                                00118003
&FDCHN   SETC  'ZFD'.'&FDCNTR'                                          00118103
&C       SETA  &FDCNTR+1                                                00118203
         AIF   ('&NM' EQ '').NONAME                                     00118303
&NM      EQU   *                                                        00118403
.NONAME  AIF   ('&S' NE 'END' AND '&S' NE '*END').PROCESS               00118503
&FDCHN   DC    AL1(0)        END OF FD LIST                             00118603
         MEXIT ,                                                        00118703
.PROCESS ANOP  ,                                                        00118803
&I       SETA  N'&SYSLIST                                               00118903
&J       SETA  0                                                        00119003
&SB1     SETC  '0'                                                      00119103
&CU1     SETC  '0'                                                      00119203
.NOLIT   AIF   (&J GE &I).CHECK                                         00119303
&J       SETA  &J+1                                                     00119403
&L       SETC  '&SYSLIST(&J)'                                           00119503
         AIF   ('&L' EQ '').NOLIT                                       00119603
&NOP     SETB  (&NOP  OR '&L' EQ 'NOP')                                 00119703
&NL      SETB  (&NL  OR '&L' EQ 'NL' OR '&L' EQ 'NEWLINE')              00119803
&ALARM    SETB  (&ALARM OR '&L' EQ 'ALARM')                             00119903
&BLUE    SETB  (&BLUE OR '&L' EQ 'BLUE')                                00120003
&GREEN   SETB  (&GREEN OR '&L' EQ 'GREEN')                              00120103
&PINK    SETB  (&PINK OR '&L' EQ 'PINK')                                00120203
&RED     SETB  (&RED  OR '&L' EQ 'RED')                                 00120303
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TURQ' OR '&L' EQ 'CYAN')      90326  00120403
&WHITE   SETB  (&WHITE OR '&L' EQ 'WHITE')                              00120503
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YELLOW')                            00120603
&MONO    SETB  (&MONO OR '&L' EQ 'MONO')                         87313  00120703
&UNDER   SETB  (&UNDER OR '&L' EQ 'UL' OR '&L' EQ 'UNDER')              00120803
&BLINK   SETB  (&BLINK OR '&L' EQ 'BLINK')                              00120903
&REVERSE SETB  (&REVERSE OR '&L' EQ 'REVERSE')                          00121003
&INTENSE SETB  (&INTENSE OR '&L' EQ 'INTENSE' OR '&L' EQ 'INT')         00121103
&DETECT  SETB  (&DETECT OR '&L' EQ 'DETECT' OR '&L' EQ 'LP')            00121203
&NONDISP SETB  (&NONDISP OR '&L' EQ 'NONDISP' OR '&L' EQ 'NDISP')       00121303
&NUMERIC SETB  (&NUMERIC OR '&L' EQ 'NUMERIC' OR '&L' EQ 'NUM')         00121403
&SKIP    SETB  (&SKIP OR '&L' EQ 'SKIP')                                00121503
&PROTECT SETB  (&PROTECT OR '&L' EQ 'PROTECT')                          00121603
&MDT     SETB  (&MDT OR '&L' EQ 'MDT')                           87313  00121703
&DEFAULT SETB  (&DEFAULT OR '&L' EQ 'DEFAULT' OR '&L' EQ 'DFLT')        00121803
         AGO   .NOLIT                                                   00121903
.CHECK   ANOP  ,                                                        00122003
         AIF   ('&SBA' EQ '').NOSBA                                     00122103
         AIF   (N'&SBA EQ 2).SBA2                                       00122203
         AIF   (N'&SBA NE 1).BADSBA                                     00122303
&SBAP    SETB  1                                                        00122403
&SB1     SETC  '254*256+254'                                            00122503
         AIF   ('&SBA(1)' EQ '*').NOSBA                                 00122603
&SB1     SETC  '&SBA'                                                   00122703
         AGO   .NOSBA                                                   00122803
.BADSBA  MNOTE 4,'INVALID SBA= FIELD'                                   00122903
         AGO   .NOSBA                                                   00123003
.SBA2    ANOP  ,                                                        00123103
&COM1    SETC  '&SBA(1)'                                                00123203
&COM2    SETC  '&SBA(2)'                                                00123303
         AGO   .ADDCOM                                                  00123403
.RETSBA  ANOP  ,                                                        00123503
&SBAP    SETB  1             SET SBA PRESENT                            00123603
&SB1     SETC  '&CU1'                                                   00123703
&CU1     SETC  '0'                                                      00123803
.NOSBA   AIF   ('&CUR' EQ '').NOCUR                                     00123903
&CURP    SETB  1                                                        00124003
         AIF   (N'&CUR EQ 2).CUR2                                       00124103
         AIF   (N'&CUR NE 1).BADCUR                                     00124203
&CU1     SETC  '254*256+254'                                            00124303
         AIF   ('&CUR(1)' EQ '*').NOCUR                                 00124403
         AIF   ('&CUR(1)' EQ '0').BADCUR                                00124503
&CU1     SETC  '&CUR'                                                   00124603
         AGO   .NOCUR                                                   00124703
.ADDERR  AIF   (NOT &CURP).BADSBA                                       00124803
.BADCUR  MNOTE 4,'INVALID CUR= FIELD'                                   00124903
         AGO   .NOCUR                                                   00125003
.CUR2    ANOP  ,                                                        00125103
&COM1    SETC  '&CUR(1)'                                                00125203
&COM2    SETC  '&CUR(2)'                                                00125303
.ADDCOM  ANOP  ,                                                        00125403
&CU1     SETC  '254'                                                    00125503
         AIF   ('&COM1' EQ 'NULL' OR '&COM1' EQ '*').AD1COM             00125603
&CU1     SETC  '255'                                                    00125703
         AIF   ('&COM1' EQ 'NEXT' OR '&COM1' EQ '+').AD1COM             00125803
&CU1     SETC  '253'                                                    00125903
         AIF   ('&COM1' EQ 'PREV' OR '&COM1' EQ '-').AD1COM             00126003
&CU1     SETC  '253'.'&COM1'                                            00126103
         AIF   (K'&COM1 LT 1).ADDERR                                    00126203
         AIF   ('&COM1'(1,1) EQ '-').AD1COM                             00126303
         AIF   ('&COM1' EQ '0').ADDERR                                  00126403
&CU1     SETC  '&COM1'.'+63'                                            00126503
.AD1COM  ANOP  ,                                                        00126603
&COM1    SETC  '254'                                                    00126703
         AIF   ('&COM2' EQ 'NULL' OR '&COM2' EQ '*').AD2COM             00126803
&COM1    SETC  '255'                                                    00126903
         AIF   ('&COM2' EQ 'NEXT' OR '&COM2' EQ '+').AD2COM             00127003
&COM1    SETC  '253'                                                    00127103
         AIF   ('&COM2' EQ 'PREV' OR '&COM2' EQ '-').AD2COM             00127203
&COM1    SETC  '253'.'&COM2'                                            00127303
         AIF   (K'&COM2 LT 1).ADDERR                                    00127403
         AIF   ('&COM2'(1,1) EQ '-').AD2COM                             00127503
         AIF   ('&COM2' EQ '0').ADDERR                                  00127603
&COM1    SETC  '&COM2'.'-1'                                             00127703
.AD2COM  ANOP  ,                                                        00127803
&CU1     SETC  '('.'&CU1'.')*256+'.'&COM1'                              00127903
         AIF   (NOT &CURP).RETSBA                                       00128003
.NOCUR   AIF   (T'&CC EQ 'O').NOCC                               81201  00128103
         AIF   (NOT &CURP).SETCC                                 81201  00128203
         MNOTE 4,'CC= AND CUR= ARE MUTUALLY EXCLUSIVE'           81201  00128303
         AGO   .NOCC                                             81201  00128403
.SETCC   ANOP  ,                                                 81201  00128503
&CCP     SETB  1                                                 81201  00128603
.NOCC    AIF   (T'&IND EQ 'O').NOIND                             81270  00128703
         AIF   (NOT &CURP).SETIND                                81270  00128803
         MNOTE 4,'IND= AND CUR= ARE MUTUALLY EXCLUSIVE'          81270  00128903
         AGO   .NOIND                                            81270  00129003
.SETIND  ANOP  ,                                                 81270  00129103
&INDP    SETB  1                                                 81270  00129203
.NOIND   ANOP  ,                                                 81270  00129303
&OPT     SETB  1                                                        00129403
&T       SETA  32*&OPT+64*&NOP                                          00129503
&WCC     SETA  4*&ALARM                                                 00129603
&WCCP    SETB  (&ALARM)                                                 00129703
&E      SETA 128*&NL+64*&WCCP+32*&SBAP+8*&CURP+&CCP+4*&INDP      81270  00129803
&O7      SETA  128*&DEFAULT+64*&PREV+&MDT                        87313  00129903
&PROTECT SETB  (&PROTECT OR &SKIP)                                      00130003
&NUMERIC SETB  (&NUMERIC OR &SKIP)                                      00130103
&DETECT  SETB  (&DETECT  OR &NONDISP)                                   00130203
&INTENSE SETB  (&INTENSE OR &NONDISP)                                   00130303
&O7      SETA  &O7+32*&PROTECT+16*&NUMERIC+8*&INTENSE+4*&DETECT         00130403
&C1      SETB  (&GREEN OR &TURQ OR &WHITE OR &YELLOW)                   00130503
&C2      SETB  (&RED OR &PINK OR &WHITE OR &YELLOW)                     00130603
&C3      SETB  (&BLUE OR &PINK OR &TURQ OR &WHITE)                      00130703
&O9      SETA  64*&C1+32*&C2+16*&C3+8*&MONO+4*&UNDER+2*&REVERSE+&BLINK  00130803
&O79     SETB  (&O7 NE 0 OR &O9 NE 0)                                   00130903
&E       SETA  &E+2*&O79                                                00131003
         AIF   (&CCP).EXPCC                                      81201  00131103
         AIF   (&CURP).EXPSC                                     81270  00131203
         AIF   (&INDP).EXPSI                                     81270  00131303
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1)                00131403
         MEXIT ,                                                 81270  00131503
.EXPSI   ANOP  ,                                                 81270  00131603
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1),AL1(0,&IND)    00131703
         MEXIT ,                                                 81270  00131803
.EXPSC   ANOP  ,                                                 81270  00131903
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1,&CU1)           00132003
         MEXIT ,                                                 81201  00132103
.EXPCC   AIF   (&INDP).EXPCI                                     81270  00132203
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1),AL1(&CC)       00132303
         MEXIT ,                                                 81270  00132403
.EXPCI   ANOP  ,                                                 81270  00132503
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1),AL1(&CC,&IND)  00132603
         MEND  ,                                                        00132703
./ ADD NAME=FDSNAP   8000-11288-11288-2233-00092-00092-00000-GERHARD 00 00132803
         MACRO ,                                                        00132903
&NM      FDSNAP &ADR,&HLN,&OPTS,&BASE=,&LEN=            ADDED ON 83331  00133003
.*                                                                      00133103
.*   FDSNAP IS USED IN AN FDLIST TO DUMP MEMORY.                        00133203
.*     LIST    (DEFAULT) PRINTS TEXT ONLY (PERIOD FOR UNPRINTABLES)     00133303
.*     HEX     FORMATS IN HEXADECIMAL                                   00133403
.*     DUAL    FORMATS HEX ON LEFT, AND TEXT ON RIGHT                   00133503
.*     VERT    FORMATS THREE LINES: TEXT/ZONES/NUMERICS                 00133603
.*                                                                      00133703
.*     ABS     DISPLAYS MEMORY ADDRESS (DEFAULT)                        00133803
.*     NOABS   OMITS MEMORY ADDRESS                                     00133903
.*                                                                      00134003
.*     OFFSET  PRINTS OFFSET RELATIVE TO BASE= VALUE                    00134103
.*     NOOFFSET  OMITS OFFSET VALUE                                     00134203
.*                                                                      00134303
.*     ANSI                                                             00134403
.*     ASCII   CONVERT TEXT TO ASCII (IN OUTPUT LINE)                   00134503
.*                                                                      00134603
.*                                                                      00134703
         GBLA  &FDCNTR                                                  00134803
         LCLA  &T,&C,&FG1,&FG2,&I,&MAX                                  00134903
         LCLB  &NOP,&B0,&B1,&B2,&B3,&B4,&B5,&B6,&B7,&B8,&B9,&B10,&B11   00135003
         LCLB  &B12,&B13,&B14,&B15                                      00135103
         LCLC  &FDCHN,&OP,&RA,&RB,&LOP,&LVAL                    GP11288 00135203
&LOP     SETC  'AL'                                             GP11288 00135303
&LVAL    SETC  '&HLN'                                           GP11288 00135403
&FDCNTR  SETA  &FDCNTR+1                                                00135503
&FDCHN   SETC  'ZFD'.'&FDCNTR'                                          00135603
&C       SETA  &FDCNTR+1                                                00135703
&MAX     SETA  N'&SYSLIST                                               00135803
&I       SETA  3             FIRST OPTION                               00135903
         AIF   (T'&LEN EQ 'O').NOLEN                                    00136003
&I       SETA  2             KEYWORD, NOT POSITIONAL, LENGTH            00136103
&B0      SETB  1             SET S-FORMAT LENGTH FIELD                  00136203
&LVAL    SETC  '&LEN'                                           GP11288 00136303
.NOLEN   AIF   (T'&BASE EQ 'O').NOBASE                                  00136403
&B13     SETB  1                                                        00136503
.NOBASE  AIF   ('&NM' EQ '').NONAME                                     00136603
&NM      EQU   *                                                        00136703
.NONAME  AIF   ('&ADR' NE 'END' AND '&ADR' NE '*END').PROCESS           00136803
&FDCHN   DC    AL1(0)        END OF FD LIST                             00136903
         MEXIT ,                                                        00137003
.PROCESS AIF   (&I GT &MAX).CHECK                                       00137103
&OP      SETC  '&SYSLIST(&I)'                                           00137203
&I       SETA  &I+1                                                     00137303
         AIF   ('&OP' EQ '').PROCESS                                    00137403
&B7      SETB  (&B7 OR ('&OP' EQ 'ASCII') OR ('&OP' EQ 'ANSI'))  83331  00137503
&B8      SETB  (&B8 OR ('&OP' EQ 'HEX') OR ('&OP' EQ 'VERT'))           00137603
&B9      SETB  (&B9 OR ('&OP' EQ 'DUAL') OR ('&OP' EQ 'VERT'))          00137703
&B8      SETB  (&B8 AND '&OP' NE 'LIST' AND '&OP' NE 'DUAL')            00137803
&B9      SETB  (&B9 AND '&OP' NE 'LIST' AND '&OP' NE 'HEX')             00137903
&B14     SETB  ((&B14 OR '&OP' EQ 'NOABS') AND '&OP' NE 'ABS')          00138003
&B15     SETB  ((&B15 OR '&OP' EQ 'OFFSET') AND '&OP' NE 'NOOFFSET')    00138103
         AGO   .PROCESS                                                 00138203
.CHECK   ANOP  ,                                                        00138303
&T       SETA  64*&NOP+29                                               00138403
         AIF   (K'&ADR LT 3).NORA                                85118  00138503
         AIF   ('&ADR'(1,1) NE '(').NORA                         85118  00138603
         AIF   ('&ADR'(K'&ADR,1) NE ')').NORA                    85118  00138703
         AIF   ('&ADR'(2,1) EQ '(').NORA                         85118  00138803
&RA      SETC  '0'           CHANGE R TO S FORMAT                85118  00138903
.NORA    AIF   (&B0).TESTL                                       85118  00139003
         AIF   (K'&HLN LT 3).TESTD                               85118  00139103
         AIF   ('&HLN'(1,1) NE '(').TESTD                        85118  00139203
         AIF   ('&HLN'(K'&HLN,1) NE ')').TESTD                   85118  00139303
         AIF   ('&HLN'(2,1) EQ '(').TESTD                        85118  00139403
&B0      SETB  1             SET S-FORMAT LENGTH FIELD          GP11288 00139503
&LOP     SETC  'SL'                                             GP11288 00139603
&LVAL    SETC  '0'.'&HLN'                                       GP11288 00139703
         AGO   .TESTD                                            85118  00139803
.TESTL   AIF   (K'&LEN LT 3).TESTD                               85118  00139903
         AIF   ('&LEN'(1,1) NE '(').TESTD                        85118  00140003
         AIF   ('&LEN'(K'&LEN,1) NE ')').TESTD                   85118  00140103
         AIF   ('&LEN'(2,1) EQ '(').TESTD                        85118  00140203
&LOP     SETC  'SL'                                             GP11288 00140303
&LVAL    SETC  '0'.'&LEN'                                       GP11288 00140403
.TESTD   ANOP  ,                                                GP11288 00140503
&FG1     SETA  128*&B0+64*&B1+32*&B2+16*&B3+8*&B4+4*&B5+2*&B6+&B7       00140603
&FG2     SETA  128*&B8+64*&B9+32*&B10+16*&B11+8*&B12+4*&B13+2*&B14+&B15 00140703
&FG1     SETA  &FG1*256+&FG2                                            00140803
         AIF   (&B13).BASED                                      85118  00140903
&FDCHN   DC    AL1(ZFD&C-*,&T),AL2(&FG1),SL2(&RA&ADR),&LOP.2(&LVAL)     00141003
         MEXIT ,                                                        00141103
.BASED   AIF   (K'&BASE LT 3).BASES                              85118  00141203
         AIF   ('&BASE'(1,1) NE '(').BASES                       85118  00141303
         AIF   ('&BASE'(K'&BASE,1) NE ')').BASES                 85118  00141403
         AIF   ('&BASE'(2,1) EQ '(').BASES                       85118  00141503
&RB      SETC  '0'           CHANGE R TO S FORMAT                85118  00141603
.BASES   ANOP  ,                                                GP11288 00141703
&FDCHN   DC    AL1(ZFD&C-*,&T),AL2(&FG1),SL2(&RA&ADR),&LOP.2(&LVAL),SL2*00141803
               (&RB&BASE)                                       GP11288 00141903
         MEND  ,                                                        00142003
./ ADD NAME=FDTM                                                        00142103
         MACRO                                                          00142203
&NM      FDTM  &STR,&MASK,&BZ=0,&BM=0,&BO=0,&BNO=0,&BNZ=0        81264  00142303
         GBLA  &FDCNTR                                                  00142403
         LCLA  &T,&C                                                    00142503
         LCLB  &NOP                                              81133  00142603
         LCLC  &FDCHAIN,&FZ,&FM,&FO                              81264  00142703
&FDCNTR  SETA  &FDCNTR+1                                                00142803
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                          00142903
&C       SETA  &FDCNTR+1                                                00143003
         AIF   ('&NM' EQ '').NONAME                                     00143103
&NM      EQU   *                                                        00143203
.NONAME  AIF   ('&STR' NE 'END' AND '&STR' NE '*END').PROCESS           00143303
&FDCHAIN DC    AL1(0)        END OF FD LIST                             00143403
         MEXIT ,                                                        00143503
.PROCESS ANOP  ,                                                        00143603
&FZ      SETC  '&BZ'                                             81264  00143703
&FM      SETC  '&BM'                                             81264  00143803
&FO      SETC  '&BO'                                             81264  00143903
         AIF   ('&BNZ' EQ '0' OR '&BNO' EQ '0').BNZBNO           81264  00144003
         MNOTE 8,'MUTUALLY EXCLUSIVE BNZ AND BNO'                81264  00144103
.BNZBNO  AIF   ('&BNO' EQ '0').NOBNO                             81264  00144203
         AIF   ('&FZ' EQ '0' AND '&FM' EQ '0').DOBNO             81264  00144303
         MNOTE 8,'MUTUALLY EXCLUSIVE BNO AND BZ/BM'              81264  00144403
.DOBNO   ANOP  ,                                                 81264  00144503
&FZ      SETC  '&BNO'                                            81264  00144603
&FM      SETC  '&BNO'                                            81264  00144703
.NOBNO   AIF   ('&BNZ' EQ '0').CHECK                             81264  00144803
         AIF   ('&FM' EQ '0' AND '&FO' EQ '0').DOBNZ             81264  00144903
         MNOTE 8,'MUTUALLY EXCLUSIVE BNZ AND BM/BO'              81264  00145003
.DOBNZ   ANOP  ,                                                 81264  00145103
&FM      SETC  '&BNZ'                                            81264  00145203
&FO      SETC  '&BNZ'                                            81264  00145303
.CHECK   ANOP  ,                                                        00145403
&T       SETA  64*&NOP+19                                               00145503
&FDCHAIN DC AL1(ZFD&C-*,&T),SL2(&FZ,&FM,&FO,&STR),AL1(&MASK)     81264  00145603
         MEND  ,                                                        00145703
./ ADD NAME=FLGTAB   0100-03019-03019-2005-00068-00068-00000-GERHARD 00 00145803
         MACRO                                                          00145903
&NM      FLGTAB &MASK,&TEXT,&LEN,&MLEN=                                 00146003
.*--------------------------------------------------------------------* 00146103
.*                                                                    * 00146203
.*  THIS MACRO GENERATES A TABLE OF FLAG BITS AND MATCHING TEXT       * 00146303
.*  &MLEN (ON THE FIRST ENTRY) SPECIFIES THE BIT MASK LENGTH IN BYTES * 00146403
.*  &TEXT SPECIFIES THE QUOTED OR UNQUOTED TEXT FOR THAT BIT MASK     * 00146503
.*  &LEN OPTIONALLY OVERRIDES THE CALCULATED TEXT LENGTH              * 00146603
.*                                                                    * 00146703
.*--------------------------------------------------------------------* 00146803
         GBLC  &ZZFGTLN      REMEMBER LENGTH GLOBALLY                   00146903
         LCLA  &I,&K,&L                                                 00147003
         LCLB  &QUO                                                     00147103
         LCLC  &Q1,&Q2                                                  00147203
.*                                                                      00147303
.*  CHECK END REQUEST                                                   00147403
.*                                                                      00147503
         AIF   ('&MASK' EQ '*END').END                                  00147603
.*                                                                      00147703
&QUO     SETB  ('&TEXT'(1,1) EQ '''')                                   00147803
         AIF   (&QUO).NOFRAME                                           00147903
&Q1      SETC  ''''                                                     00148003
&Q2      SETC  ''''                                                     00148103
.*                                                                      00148203
.*  SET GLOBAL LENGTH AS NEEDED                                         00148303
.*                                                                      00148403
.NOFRAME AIF   ('&MLEN' EQ '').NOTMLEN                                  00148503
&ZZFGTLN SETC  '&MLEN'                                                  00148603
.NOTMLEN AIF   ('&ZZFGTLN' NE '').GOTMLEN                               00148703
&ZZFGTLN SETC  '1'           DEFAULT MASK LENGTH IS 1                   00148803
.*                                                                      00148903
.*  WHEN USER SPECIFIES A LENGTH, JUST EXPAND THE REQUEST               00149003
.*                                                                      00149103
.GOTMLEN AIF   ('&LEN' EQ '').COMLEN                                    00149203
&NM      DC    AL1(&LEN-1),AL(&ZZFGTLN)(&MASK),CL(&LEN)&Q1&TEXT&Q2      00149303
         MEXIT ,                                                        00149403
.*                                                                      00149503
.*  CALCULATE THE LENGTH OF THE TEXT ITEM                               00149603
.*                                                                      00149703
.COMLEN  AIF   (&QUO).QUOADJ                                            00149803
&L       SETA  K'&TEXT                                                  00149903
&I       SETA  &L                                                       00150003
&K       SETA  1                                                        00150103
         AGO   .LOOP                                                    00150203
.QUOADJ  ANOP  ,                                                        00150303
&L       SETA  K'&TEXT-2                                                00150403
&I       SETA  &L-1                                                     00150503
&K       SETA  2                                                        00150603
.*                                                                      00150703
.*  SCAN FOR DOUBLE QUOTES, AND DECREMENT LENGTH BY ONE                 00150803
.*                                                                      00150903
.LOOP    AIF   (&K GE &I).EXPQ                                          00151003
         AIF   ('&TEXT'(&K,2) EQ '''''').DOUB                           00151103
&K       SETA  &K+1                                                     00151203
         AGO   .LOOP                                                    00151303
.DOUB    ANOP                                                           00151403
&K       SETA  &K+2                                                     00151503
&L       SETA  &L-1                                                     00151603
         AGO   .LOOP                                                    00151703
.*                                                                      00151803
.*  GENERATE WITH FORCED LENGTH                                         00151903
.*                                                                      00152003
.EXPQ    ANOP                                                           00152103
&NM      DC    AL1(&L-1),AL(&ZZFGTLN)(&MASK),CL(&L)&Q1&TEXT&Q2          00152203
         MEXIT ,                                                        00152303
.END     ANOP  ,                                                        00152403
&NM      DC    AL1(255)      END OF FLAG TABLE ENTRIES                  00152503
         MEND  ,                                                        00152603
./ ADD NAME=LAM      8001-05189-05189-1113-00009-00009-00000-GERHARD 00 00152703
         MACRO ,                                                        00152803
&NM      LAM   &R,&S,&T                                 ADDED ON 05189  00152903
.*                                                                      00153003
.*       THIS MODULE GENERATES A LABEL FOR MVS COMPATIBILITY            00153103
.*                                                                      00153203
         GBLC  &MACPLAB                                                 00153303
&MACPLAB SETC  '&NM'                                                    00153403
         MACPARM MODE=LBL                                               00153503
         MEND  ,                                                        00153603
./ ADD NAME=LPALOOK  0103-03261-06263-0020-00051-00025-00025-GERHARD 00 00153703
         MACRO ,                                                        00153803
&NM      LPALOOK &EP=,&EPLOC=,&DCB=,&MEMBER=,&ALIAS=,&ERR=              00153903
.*--------------------------------------------------------------------* 00154003
.*  LPALOOK INVOKES SUBROUTINE SUBLPALK, WHICH USES CSVQUERY TO LOOK  * 00154103
.*    FOR THE MODULE REQUESTED BY EITHER EP=, OR NAMED IN EPLOC.      * 00154203
.*  WHEN THE DCB IS NON-ZERO, THE MODULE IS LOADED IF NOT IN AN LPA   * 00154303
.*    LIST. LOAD USES DCB=0 WHEN DCB PARAMETER<256                    * 00154403
.*                                                                    * 00154503
.*  AN EXTRN IS ISSUED UNLESS THE MODULE WAS NAMED IN A SERVLOAD REQ. * 00154603
.*--------------------------------------------------------------------* 00154703
         GBLC  &MACPLAB                                                 00154803
         GBLC  &SRVLMOD(20),&SRVLDEL(20)                                00154903
         GBLB  &MVSXA                                           GP04234 00155003
         GBLB  &SRVBMOD(20)                                             00155103
         GBLA  &SRVNMOD                                                 00155203
         GBLB  &ZLPAKFG                                                 00155303
         LCLA  &I                                                       00155403
         LCLC  &CALLMOD                                                 00155503
&CALLMOD SETC  '=A(SUBLPALK)'    LPA LOOKUP/LOAD MODULE                 00155603
&MACPLAB SETC  '&NM'                                                    00155703
.LOOKLUK AIF   (&I GE &SRVNMOD).SKIPLUK  NOT IN SERVLOAD LIST           00155803
&I       SETA  &I+1                                                     00155903
         AIF   ('&SRVLDEL(&I)' NE 'SUBLPALK').LOOKLUK                   00156003
&CALLMOD SETC  '&SRVLMOD(&I)'  USE SERVLOAD ADDRESS                     00156103
         AGO   .COMMLUK                                                 00156203
.SKIPLUK AIF   (&ZLPAKFG).COMMLUK                                       00156303
         EXTRN SUBLPALK                                                 00156403
&ZLPAKFG SETB  1                                                        00156503
.COMMLUK AIF   ('&EP' EQ '' AND '&EPLOC' EQ '').OMIT                    00156603
         AIF   ('&EP' NE '' AND '&EPLOC' NE '').DUPE                    00156703
         MACPARM R0,&DCB,NULL=0                                         00156803
         AIF   ('&EP' EQ '').NOEP                                       00156903
         MACPARM R1,=CL8'&EP '                                          00157003
         AGO   .COMMON                                                  00157103
.NOEP    MACPARM R1,&EPLOC                                              00157203
.COMMON  MACPARM R15,&CALLMOD,OP=L                                      00157303
         AIF   (&MVSXA).DOBAS                                   GP04234 00157403
         MACPARM R14,(R15),OP=BAL,OPR=BALR                              00157503
         AGO   .NOBAS                                                   00157603
.DOBAS   MACPARM R14,(R15),OP=BAS,OPR=BASR                              00157703
.NOBAS   AIF ('&MEMBER' EQ '' AND '&ALIAS' EQ '' AND '&ERR' EQ '').MEND 00157803
         MACPARM R15,=H'4',OP=CH  CHECK RETURN CODE                     00157903
         MACPARM &MEMBER,OP=BL,OPR=BLR,MODE=ONE,NULL=SKIP               00158003
         MACPARM &ALIAS,OP=BE,OPR=BER,MODE=ONE,NULL=SKIP                00158103
         MACPARM &ERR,OP=BH,OPR=BHR,MODE=ONE,NULL=SKIP                  00158203
         MEXIT ,                                                 81169  00158303
.OMIT    MNOTE 8,'NEITHER EP= NOR EPLOC= SUPPLIED'               81169  00158403
         AGO   .DEFLAB                                           81169  00158503
.DUPE    MNOTE 8,'EP= AND EPLOC= ARE MUTUALLY EXCLUSIVE'         81169  00158603
.DEFLAB  MACPARM MODE=LBL    EXPAND LABEL ONLY                          00158703
.MEND    MEND  ,                                                        00158803
./ ADD NAME=MACLIST  8007-04317-05197-2212-00048-00019-00000-GERHARD 00 00158903
         MACRO ,                                                        00159003
         MACLIST &LIST                                 NEW 2004.234 GYP 00159103
.*--------------------------------------------------------------------* 00159203
.*   MACLIST IS USED TO COMPENSATE FOR ASSEMBLER F/XF INABILITY TO    * 00159303
.*     PROVIDE N'&LIST(&I,&N) SUPPORT.                                * 00159403
.*   THIS MACRO STRIPS UP TO 10 INNER VALUES                          * 00159503
.*   E.G.       MYMAC OPTS=(A,(B1,B2,B3),(C1,C2))                     * 00159603
.*    HLASM N'&OPTS(2) IS 3, N'&OPTS(3) IS 2                          * 00159703
.*    XFASM FAILS ALL BUT N'&OPTS                                     * 00159803
.*                                                                    * 00159903
.*    USE:   MACLIST &OPTS(2)                                         * 00160003
.*    RESULTS RETURNED IN &MACP1-&MACP10, COUNT IN &MACP#             * 00160103
.*--------------------------------------------------------------------* 00160203
         GBLA  &MACP#        NUMBER OF (SUB)LIST ARGUMENTS              00160303
         GBLC  &MACP1,&MACP2,&MACP3,&MACP4,&MACP5                       00160403
         GBLC  &MACP6,&MACP7,&MACP8,&MACP9,&MACP10                      00160503
         LCLA  &I,&J,&K,&N                                              00160603
         LCLC  &STR,&LAB                                                00160703
         LCLC  &VAL(10)                                                 00160803
&STR     SETC  '&LIST'                                                  00160903
&J       SETA  1             OUTPUT SUBSCRIPT                           00161003
&K       SETA  K'&LIST                                                  00161103
         AIF   (&K LT 1).DONE                                           00161203
         AIF   ('&STR'(1,1) NE '(' OR '&STR'(&K,1) NE ')').NOPARS       00161303
&STR     SETC  '&STR'(2,&K-2)                                           00161403
&K       SETA  K'&STR                                                   00161503
.NOPARS  AIF   (&I GE &K).DONE                                          00161603
&I       SETA  &I+1                                                     00161703
         AIF   ('&STR'(&I,1) EQ ',').NEWPRM                             00161803
&VAL(&J) SETC  '&VAL(&J)'.'&STR'(&I,1)                                  00161903
         AGO   .NOPARS                                                  00162003
.NEWPRM  AIF   (&J GE 10).TOOMANY                                       00162103
&J       SETA  &J+1                                                     00162203
         AGO   .NOPARS                                                  00162303
.TOOMANY MNOTE 8,'MACLIST SUPPORTS MAX OF 10 SUBLIST ITEMS'             00162403
.DONE    ANOP  ,                                                        00162503
&MACP#   SETA  &J            GET NUMBER OF OPERANDS                     00162603
&MACP1   SETC  '&VAL(1)'                                                00162703
&MACP2   SETC  '&VAL(2)'                                                00162803
&MACP3   SETC  '&VAL(3)'                                                00162903
&MACP4   SETC  '&VAL(4)'                                                00163003
&MACP5   SETC  '&VAL(5)'                                                00163103
&MACP6   SETC  '&VAL(6)'                                                00163203
&MACP7   SETC  '&VAL(7)'                                                00163303
&MACP8   SETC  '&VAL(8)'                                                00163403
&MACP9   SETC  '&VAL(9)'                                                00163503
&MACP10  SETC  '&VAL(10)'                                               00163603
.MEND    MEND  ,                                                        00163703
./ ADD NAME=MACPARM  8002-08090-08279-1431-00329-00454-00000-GERHARD 00 00163803
         MACRO                                                          00163903
&NM    MACPARM &OP1,         FIRST OPERAND (USUALLY R1 FOR LA/LR)      *00164003
               &OP2,         SECOND OPERAND (R2/B2D2 OR R3 IF MODE=3   *00164103
               &OP3,         THIRD OPERAND (B2D2 WHEN MODE=3)          *00164203
               &OP4,         FOURTH OPERAND  (WHEN MODE=4)             *00164303
               &OP=LA,       OPCODE WHEN OP2 IS NOT A REGISTER         *00164403
               &OPM=,        OPCODE WHEN OP2 IS NEGATED AND NOT REG.   *00164503
               &OPR=LR,      OPCODE WHEN OP2 IS REGISTER               *00164603
               &OPMR=LCR,    OPCODE WHEN OP2 IS NEGATED REGISTER       *00164703
               &QUOTE=,      OPCODE FOR QUOTED STRING/EXPLICIT LEN     *00164803
               &MODE=,       ONE/THREE/REV/EQU/EVEN                    *00164903
               &NAME=,       OUTER MACRO FOR MNOTES                    *00165003
               &OMIT=NO,     SKIP COMPLETELY IF BLANK                  *00165103
               &NULL=  SKIP, YES, OR OPERAND TO USE FOR NULL &OP2       00165203
.*                                                              GP00196 00165303
.*   THIS IS AN INNER MACRO USED TO CONVERT MACRO PARAMETERS TO         00165403
.*     INSTRUCTIONS APPROPRIATE TO THE OPERAND TYPE.                    00165503
.*                                                                      00165603
.*   THIS MACRO WAS SUGGESTED BY A MUCH OLDER VERSION (LODE/LODESTAR)   00165703
.*     BY SEYMOUR (SHMUEL) J. METZ THAT HANDLED TWO OPERANDS ONLY.      00165803
.*     NONE OF THE ORIGINAL CODE IS USED HEREIN.                        00165903
.*                                                                      00166003
.*   WITH DEFAULTS, IT EXPANDS:                                         00166103
.*    MACPARM R5,WORD      AS   LA R5,WORD                              00166203
.*    MACPARM R5,(R5)      AS   NOTHING (LABEL IS SAVED IN MACPLAB)     00166303
.*    MACPARM R5,(R4)      AS   LR R5,R4                                00166403
.*                                                                      00166503
.*   IN ORDER TO BE RECOGNIZED AS MATCHING, REGISTER SPECIFICATIONS     00166603
.*    SHOULD BE MADE IN ABSOLUTE FORM (0)-(15), OR MNEMONIC (R0)-(R15). 00166703
.*    OP1 NORMALLY DOES NOT NEED THE PARENTHESES.                       00166803
.*                                                                      00166903
.*   TO AVOID CONFLICTS WITH REGISTER SPECIFICATIONS, EXPRESSIONS MUST  00167003
.*    EITHER BEGIN WITHOUT A PARENTHESIS, OR WITH TWO: ((B-A)/(C-A))    00167103
.*                                                                      00167203
.*    OP2 PARAMETER IS AN EXPRESSION OR (REG)                           00167303
.*       EITHER FORM MAY BE PREFIXED BY A MINUS SIGN                    00167403
.*       LA REQUESTS MAY BE PREFIXED BY / TO USE L =A(OP2)              00167503
.*       LA REQUESTS MAY BE PREFIXED BY * TO USE L ,OP2                 00167603
.*       FOR A NEGATED SECOND OPERAND, THE EXPANSION WILL USE           00167703
.*       &OPMR FOR REGISTER, &OPM IF SPECIFIED, OR &OP/LNR              00167803
.*    MODE=REV      FOR &OPR, REVERSE REGISTERS                         00167903
.*    MODE=EQU      IF FIRST=SECOND OPERAND, EXPAND ANYWAY              00168003
.*    MODE=NONE     EXPAND OP= ONLY; EITHER NO OPERAND OR OPT.  GP03144 00168103
.*                    OPERAND. (MAY BE ENCLOSED IN QUOTES)      GP03144 00168203
.*    MODE=ONE      SINGLE OPERAND (E.G., BX, BXR TYPE)                 00168303
.*    MODE=THREE    THREE OPERAND TYPE; EXPANDS &OP &OP1,&OP2,&OP3      00168403
.*    MODE=FOUR     FOUR OPERAND TYPE; EXPANDS &OP &OP1,&OP2,&OP3,&OP4  00168503
.*    MODE=EVEN     EXPAND (TWO OPERAND FORM) EVEN WHEN SAME    GP01028 00168603
.*    MODE=LBL      NO OPERANDS - EXPANDS PENDING LABEL(S)              00168703
.*                    OPERAND 1 - OPTIONAL ALIGNMENT (E.G., 0F) GP03144 00168803
.*    NULL=         OMITTED PARM CAUSES ASSEMBLY ERROR (?)              00168903
.*    NULL=YES      NULL FINAL PARAMETER EXPANDS WITHOUT PARM           00169003
.*    NULL=TERM     EXPANSION USES SUPPLIED TERM IF PARM=NULL           00169103
.*    NULL=SKIP     NULL FINAL PARAMETER SKIPS EXPANSION                00169203
.*    NAME=         (OPTIONAL) NAME OF OUTER MACRO FOR MNOTES           00169303
.*                                                                      00169403
.*    QUOTE=(LA,8)  TURNS  'TEXT' INTO   LA RX,=CL(8)'TEXT' (MODE 2)    00169503
.*                                                                      00169603
         GBLC  &MACPLAB,&MACPSIZ,&MACQSTR                       GP08090 00169703
         GBLB  &MACPERR,&MACPNUL,&MVS,&MVSXA,&MVSESA            GP00196 00169803
         GBLB  &MACQUOT                                         GP08090 00169903
         GBLA  &MACPLEN                                         GP08090 00170003
         LCLA  &K,&I,&J                                         GP08090 00170103
         LCLB  &MINUS,&MOD0,&MOD1,&MOD3,&MOD4,&MODQ,&MODR,&MODV         00170203
         LCLB  &FGR1,&FGR2   ON WITH REGISTER OPERAND                   00170303
         LCLC  &FD1,&FD2,&FD3,&FD4,&LBL,&OPRR,&MNONM,&OPLA,&L   GP08090 00170403
         AIF   ('&OMIT' EQ '').NO  SKIP COMPLETELY IF NULL      GP06277 00170503
&MNONM   SETC  'MACPARM:'                                               00170603
&MACPERR SETB  0             RESET RETURN FLAG                  GP00196 00170703
&MACPNUL SETB  0             RESET RETURN FLAG                  GP00196 00170803
&OPLA    SETC  '&OP'         MAY NEED UPDATING                  GP08090 00170903
&MACPSIZ SETC  ''                                               GP08090 00171003
         AIF   ('&NM' EQ '').NONAME                                     00171103
&MNONM   SETC  '&NAME'.'/MACPARM:'                                      00171203
.NONAME  ANOP  ,                                                        00171303
&MOD0    SETB  ('&MODE' EQ 'NONE' OR '&MODE' EQ '0')            GP03144 00171403
&MOD1    SETB  ('&MODE' EQ 'ONE' OR '&MODE' EQ '1')                     00171503
&MOD3    SETB  ('&MODE' EQ 'THREE' OR '&MODE' EQ '3')                   00171603
&MOD4    SETB  ('&MODE' EQ 'FOUR' OR '&MODE' EQ '4')            GP00196 00171703
&MODQ    SETB  ('&MODE' EQ 'EQU' OR '&MODE' EQ 'EQUAL')                 00171803
&MODR    SETB  ('&MODE' EQ 'REV' OR '&MODE' EQ 'REVERSE')               00171903
&MODV    SETB  ('&MODE' EQ 'EVEN' OR '&MODE' EQ 'SAME')         GP01028 00172003
.*                                                                      00172103
.*  TEST FOR UNUSED LABEL EXPANSION ONLY                                00172203
.*                                                                      00172303
&LBL     SETC  '&NM'         INDICATE LOCAL LABEL UNUSED                00172403
         AIF   ('&MODE' NE 'LBL' AND '&MODE' NE 'LABEL').NOTLBL         00172503
&FD1     SETC  '&OP1(1)'     ALLOW USER TO SPECIFY ALIGNMENT    GP03144 00172603
         AIF   ('&FD1' NE '').XAV                               GP03144 00172703
&FD1     SETC  '0H'          DEFAULT ALIGNMENT                  GP03144 00172803
.XAV     AIF   ('&MACPLAB' EQ '').XNM                                   00172903
         AIF   ('&MACPLAB' EQ '&LBL').X1LBL                             00173003
&MACPLAB DS    &FD1                                             GP03144 00173103
.X1LBL   ANOP  ,                                                        00173203
&MACPLAB SETC  ''                                                       00173303
.XNM     AIF   ('&LBL' EQ '').XNOP                                      00173403
&NM      DS    &FD1                                             GP03144 00173503
&LBL     SETC  ''            LOCAL LABEL EXPANDED                       00173603
.XNOP    AIF   (T'&OP2 EQ 'O' AND T'&OP3 EQ 'O'                        *00173703
               AND T'&OP4 EQ 'O').MEXIT                                 00173803
         MNOTE 4,'&MNONM POSITIONAL PARAMETERS IGNORED'                 00173903
&MACPERR SETB  1             RETURN ERROR                       GP00196 00174003
.MEXIT   MEXIT ,                                                        00174103
.*                                                                      00174203
.*  TEST FOR CORRECT MODE OPERAND                                       00174303
.*                                                                      00174403
.NOTLBL  AIF   ('&MODE' EQ '' OR &MOD0 OR &MOD1 OR &MOD3 OR &MODQ      *00174503
               OR &MODR OR &MODV).MODG                          GP03144 00174603
         MNOTE 8,'&MNONM INVALID MODE=&MODE '                           00174703
&MACPERR SETB  1             RETURN ERROR                       GP00196 00174803
.*                                                                      00174903
.*  CHECK LOCAL VS. GLOBAL LABEL, EXPAND GLOBAL AND RELOAD              00175003
.*                                                                      00175103
.MODG    AIF   ('&MACPLAB' EQ '' OR '&LBL' EQ '').N2LBL                 00175203
&MACPLAB DS    0H                                                       00175303
         AGO   .PROPLBL      PROPAGATE LOCAL LABEL                      00175403
.N2LBL   AIF   ('&MACPLAB' NE '').NOLAB                                 00175503
.PROPLBL ANOP  ,                                                        00175603
&MACPLAB SETC  '&LBL'        NO GLOBAL LABEL - USE LOCAL                00175703
&LBL     SETC  ''                                                       00175803
.*                                                                      00175903
.*  TEST FOR CORRECT NUMBER OF PARAMETERS, AND SUBSTITUTE &NULL         00176003
.*                                                                      00176103
.NOLAB   AIF   (NOT &MOD0).NOTNONE   OPCODE ONLY ?              GP03144 00176203
&FD1     SETC  '&OP1'                                           GP03144 00176303
         AIF   ('&FD1' EQ '').DONONE                            GP03144 00176403
         AIF   ('&FD1'(1,1) NE '"').DONONE                      GP03144 00176503
&FD1     SETC  '&FD1'(2,K'&FD1-2)                               GP03144 00176603
.DONONE  ANOP  ,                                                GP03144 00176703
&MACPLAB &OP   &FD1                                             GP03144 00176803
         AIF   (T'&OP2 EQ 'O' AND T'&OP3 EQ 'O'                        *00176903
               AND T'&OP4 EQ 'O').GO                            GP03144 00177003
         MNOTE 4,'&MNONM POSITIONAL PARAMETERS IGNORED'         GP03144 00177103
&MACPERR SETB  1             RETURN ERROR                       GP03144 00177203
         AGO   .GO                                              GP03144 00177303
.NOTNONE ANOP  ,                                                        00177403
&FD1     SETC  '&OP1'                                                   00177503
&FD2     SETC  '&OP2'                                                   00177603
&FD3     SETC  '&OP3'                                                   00177703
&FD4     SETC  '&OP4'                                           GP00196 00177803
         AIF   (T'&OP1 NE 'O').HAVE1                                    00177903
&MACPNUL SETB  1             RETURN NULL FLAG                   GP00196 00178003
         AIF   (NOT &MOD1).NOTONE                                       00178103
         AIF   ('&NULL' EQ '').NOTONE                                   00178203
         AIF   ('&NULL' EQ 'YES').HAVE1  NOTHING ELSE TO DO     GP01009 00178303
         AIF   ('&NULL' EQ 'SKIP').MEXIT  SKIP OUT WITHOUT      GP01009 00178403
&FD1     SETC  '&NULL'                                                  00178503
         AGO   .HAVE1                                                   00178603
.NOTONE  MNOTE 8,'&MNONM FIRST POSITIONAL OPERAND REQUIRED'             00178703
&MACPERR SETB  1             RETURN ERROR                       GP00196 00178803
         AGO   .MEXIT                                                   00178903
.HAVE1   AIF   (&MOD1).HAVEALL                                          00179003
         AIF   (T'&OP2 NE 'O').HAVE2                                    00179103
&MACPNUL SETB  1             RETURN NULL FLAG                   GP00196 00179203
         AIF   (&MOD3).NOTTWO                                           00179303
         AIF   ('&NULL' EQ '').NOTTWO                                   00179403
         AIF   ('&NULL' EQ 'YES').HAVE2  NOTHING ELSE TO DO     GP01009 00179503
         AIF   ('&NULL' EQ 'SKIP').MEXIT  SKIP OUT WITHOUT      GP01009 00179603
&FD2     SETC  '&NULL'                                                  00179703
         AGO   .HAVE2                                                   00179803
.NOTTWO  MNOTE 8,'&MNONM SECOND POSITIONAL OPERAND REQUIRED'            00179903
&MACPERR SETB  1             RETURN ERROR                       GP00196 00180003
         AGO   .MEXIT                                                   00180103
.HAVE2   AIF   (NOT &MOD3).HAVE3                                GP00196 00180203
         AIF   (T'&OP3 NE 'O').HAVE3                            GP00196 00180303
&MACPNUL SETB  1             RETURN NULL FLAG                   GP00196 00180403
         AIF   ('&NULL' EQ '').NOTHREE                                  00180503
         AIF   ('&NULL' EQ 'YES').HAVEALL  NOTHING ELSE TO DO   GP01009 00180603
         AIF   ('&NULL' EQ 'SKIP').MEXIT  SKIP OUT WITHOUT      GP01009 00180703
&FD3     SETC  '&NULL'                                                  00180803
         AGO   .HAVEALL                                                 00180903
.NOTHREE MNOTE 8,'&MNONM THIRD POSITIONAL OPERAND REQUIRED'             00181003
&MACPERR SETB  1             RETURN ERROR                       GP00196 00181103
         AGO   .MEXIT                                                   00181203
.HAVE3   AIF   (NOT &MOD4).HAVEALL                              GP00196 00181303
         AIF   (T'&OP4 NE 'O').HAVEALL                          GP00196 00181403
&MACPNUL SETB  1             RETURN NULL FLAG                   GP00196 00181503
         AIF   ('&NULL' EQ '').NOFOUR                           GP00196 00181603
         AIF   ('&NULL' EQ 'YES').HAVEALL  NOTHING ELSE TO DO   GP01009 00181703
         AIF   ('&NULL' EQ 'SKIP').MEXIT  SKIP OUT WITHOUT      GP01009 00181803
&FD4     SETC  '&NULL'                                          GP00196 00181903
         AGO   .HAVEALL                                         GP00196 00182003
.NOFOUR  MNOTE 8,'&MNONM FOURTH POSITIONAL OPERAND REQUIRED'    GP03207 00182103
&MACPERR SETB  1             RETURN ERROR                       GP00196 00182203
         AGO   .MEXIT                                           GP00196 00182303
.*                                                                      00182403
.*  CHANGE OP1 AND OP2 (UNLESS MOD1 OR MOD3) TO PREFERRED FORM          00182503
.*    IF MODE 3, GO TO EXPAND IT                                        00182603
.*                                                                      00182703
.HAVEALL AIF   (&MOD3).DO3   SIMPLE EXPANSION OF THREE OPERANDS         00182803
         AIF   (&MOD4).DO4   SIMPLE EXPANSION OF FOUR OPERANDS  GP00196 00182903
         AIF   (NOT &MOD1).CLNOP1                                       00183003
&K       SETA  K'&FD1                                                   00183103
         AIF   (&K LT 2 OR '&FD1'(1,1) NE '-').CLNOP1                   00183203
&MINUS   SETB  1                                                        00183303
&FD1     SETC  '&FD1'(2,&K-1)                                           00183403
&K       SETA  K'&FD1                                                   00183503
.CLNOP1  ANOP  ,                                                        00183603
         AIF   (&K LT 3).NORG1                                          00183703
         AIF   ('&FD1'(1,1) NE '(' OR '&FD1'(&K,1) NE ')').NORG1        00183803
         AIF   ('&FD1'(2,1) EQ '(').NOSY1        ((EXPRESSION)) ?       00183903
&FGR1    SETB  1             FLAG OP1 AS REGISTER EXPRESSION            00184003
&FD1     SETC  '&FD1'(2,&K-2)                                           00184103
&K       SETA  K'&FD1                                                   00184203
.*  LOOK FOR SINGLE OR DOUBLE DIGIT - PREFIX BY R                       00184303
.NORG1   AIF   (&K LT 1 OR &K GT 2).NOSY1                               00184403
         AIF   ('&FD1'(1,1) LT '0').NOSY1  LEAVE IF NOT NUMERIC         00184503
&FD1     SETC  'R'.'&FD1'    MAKE SYMBOLIC REGISTER                     00184603
.*                                                                      00184703
.*  HAVE OP1 CLEANED FROM (N) TO RN; GO TO EXPAND MODE 1                00184803
.*    ELSE TEST AND CLEAN OPERAND 2                                     00184903
.*                                                                      00185003
.NOSY1   AIF   (&MOD1).DO1                                              00185103
&K       SETA  K'&FD2                                                   00185203
&OPRR    SETC  '&OPR'                                                   00185303
         AIF   (&K LT 2 OR '&FD2'(1,1) NE '-').NONEG2                   00185403
&MINUS   SETB  1                                                        00185503
&FD2     SETC  '&FD2'(2,&K-1)                                           00185603
&K       SETA  K'&FD2                                                   00185703
&OPRR    SETC  '&OPMR'                                                  00185803
.NONEG2  AIF   (&K LT 3).NORG2                                          00185903
         AIF   ('&FD2'(1,1) NE '''' OR T'&QUOTE EQ 'O').NOQUO2  GP08090 00186003
         AIF   ('&FD2'(&K,1) NE '''').NOQUO2                    GP08090 00186103
         MACQOLIT &FD2,LEN=&QUOTE(2)                            GP08090 00186203
         AIF   (&MACPERR OR &MACPNUL).NOQUO2                    GP08090 00186303
&FD2     SETC  '&MACQSTR'                                       GP08090 00186403
&MACPSIZ SETC  '&MACPLEN'    RETURN LENGTH                      GP08090 00186503
         AIF   ('&QUOTE(1)' EQ '').LOPP                         GP08090 00186603
&OPLA    SETC  '&QUOTE(1)'                                      GP08090 00186703
         AGO   .LOPP                                            GP08090 00186803
.NOQUO2  AIF   ('&FD2'(1,1) NE '(' OR '&FD2'(&K,1) NE ')').NORG2        00186903
         AIF   ('&FD2'(2,1) EQ '(').NORG2        ((EXPRESSION)) ?       00187003
&FD2     SETC  '&FD2'(2,&K-2)                                           00187103
&K       SETA  K'&FD2                                                   00187203
&FGR2    SETB  1             FLAG OP1 AS REGISTER EXPRESSION            00187303
         AIF   ('&FD2'(1,1) LT '0').NOSY2  LEAVE IF NOT NUMERIC         00187403
         AIF   (&K LT 1 OR &K GT 2).NOSY2                               00187503
&FD2     SETC  'R'.'&FD2'    MAKE SYMBOLIC REGISTER                     00187603
.*                                                                      00187703
.*  REG: CHECK FOR NEG PREFIX, MODE=EQU, ELSE IF OP1=OP2, NO EXPANSION  00187803
.*                                                                      00187903
.NOSY2   AIF   ('&FD2' NE '&FD1' OR &MINUS OR &MODV).LR         GP01028 00188003
         AIF   (NOT &MODQ).NO                                           00188103
.LR      AIF   (NOT &MODR).NOREV                                        00188203
&MACPLAB &OPRR &FD2,&FD1                                                00188303
         AGO   .GO                                                      00188403
.*                                                                      00188503
.*  REG: NORMAL FORM, (EXPANDS MINUS, ALSO - OPR OR OPMR IN OPRR)       00188603
.*                                                                      00188703
.NOREV   ANOP  ,                                                        00188803
&MACPLAB &OPRR &FD1,&FD2                                                00188903
         AGO   .GO                                                      00189003
.*                                                                      00189103
.*  NOT REG: CHECK FOR LA AND SPECIAL CASES                             00189203
.*                                                                      00189303
.NORG2   ANOP  ,                                                GP08090 00189403
&MACPSIZ SETC  '&L'.'&FD2'   RETURN LENGTH FOR USUAL CASE       GP08090 00189503
         AIF   ('&OPLA' EQ 'LA' AND '&FD2' EQ '0').SR                   00189603
         AIF   ('&OPM' NE '' AND &MINUS).OPM                            00189703
.*                                                                      00189803
.*  LA OP1,/OP2   GENERATES L OP1,=A(OP2)                               00189903
.*                                                                      00190003
         AIF   ('&OPLA' NE 'LA' OR '&FD2'(1,1) NE '/').LOP              00190103
&FD2     SETC  '&FD2'(2,K'&FD2-1)                                       00190203
&MACPLAB L     &FD1,=A(&FD2)                                            00190303
         AGO   .LOPCO                                                   00190403
.*                                                                      00190503
.*  LA OP1,*OP2   GENERATES L OP1,OP2                                   00190603
.*                                                                      00190703
.LOP     AIF   ('&OPLA' NE 'LA' OR '&FD2'(1,1) NE '*').LOPP             00190803
         AIF   (K'&FD2 EQ 1).LOPP                                       00190903
&FD2     SETC  '&FD2'(2,K'&FD2-1)                                       00191003
&MACPLAB L     &FD1,&FD2                                                00191103
         AGO   .LOPCO                                                   00191203
.LOPP    ANOP  ,                                                        00191303
&MACPLAB &OPLA &FD1,&FD2                                                00191403
.LOPCO   AIF   (NOT &MINUS).GO                                          00191503
         &OPMR &FD1,&FD1                                                00191603
         AGO   .GO                                                      00191703
.*                                                                      00191803
.*  USER SPECIFIED OPM AND -OP2                                         00191903
.*                                                                      00192003
.OPM     ANOP  ,                                                        00192103
&MACPLAB &OPM  &FD1,&FD2                                                00192203
         AGO   .GO                                                      00192303
.*                                                                      00192403
.*  SINGLE OPERAND INSTRUCTION - EXPAND, CHECK FOR -OP1                 00192503
.*                                                                      00192603
.DO1     AIF   (&FGR1).DO1REG                                           00192703
         AIF   ('&OPM' NE '' AND &MINUS).DO1NEG                         00192803
&MACPLAB &OP   &FD1                                                     00192903
         AIF   (NOT &MINUS).GO                                          00193003
         &OPMR &FD1                                                     00193103
         AGO   .GO                                                      00193203
.DO1NEG  ANOP  ,                                                        00193303
&MACPLAB &OPM  &FD1                                                     00193403
         AGO   .GO                                                      00193503
.DO1REG  AIF   (&MINUS).DO1MIN                                          00193603
&MACPLAB &OPR  &FD1                                                     00193703
         AGO   .GO                                                      00193803
.DO1MIN  ANOP  ,                                                        00193903
&MACPLAB &OPMR &FD1                                                     00194003
         AGO   .GO                                                      00194103
.*                                                                      00194203
.*  EXPAND THREE OPERAND INSTRUCTIONS - NO SPECIAL CHECKING             00194303
.*                                                                      00194403
.DO3     ANOP  ,                                                        00194503
&MACPLAB &OPLA &FD1,&FD2,&FD3                                           00194603
         AGO   .GO                                                      00194703
.*                                                              GP00196 00194803
.*  EXPAND FOUR OPERAND INSTRUCTIONS - NO SPECIAL CHECKING      GP00196 00194903
.*                                                              GP00196 00195003
.DO4     ANOP  ,                                                GP00196 00195103
&MACPLAB &OPLA &FD1,&FD2,&FD3,&FD4                              GP00196 00195203
         AGO   .GO                                              GP00196 00195303
.*                                                                      00195403
.*  ON SOME OLD MACHINES LA,0 WAS SLOWER AND LONGER. RETAIN SR ?        00195503
.*                                                                      00195603
.SR      ANOP                                                           00195703
&MACPLAB SR    &FD1,&FD1                                                00195803
.*                                                                      00195903
.*  INSTRUCTION(S) EXPANDED; CLEAR LABEL                                00196003
.*                                                                      00196103
.GO      ANOP  ,                                                        00196203
&MACPLAB SETC  ''                                                       00196303
.*                                                                      00196403
.*  NOTHING EXPANDED - MAINTAIN LABELS                                  00196503
.*                                                                      00196603
.NO      MEND  ,                                                        00196703
./ ADD NAME=MACQOLIT 8002-06357-08090-1652-00092-00180-00000-GERHARD 00 00196803
         MACRO ,                                                        00196903
       MACQOLIT &STR,&LEN=   DETERMINE LENGTH OF STRING; MAKE QUOTED    00197003
.*                                                                      00197103
.*   INNER MACRO FOR MACRO PROCESSING                                   00197203
.*       MACQOLIT &STR  WHERE &STR IS UNQUOTED, QUOTED, OR CONSTANT     00197303
.*                 FORMAT (E.G.,  XYZ, 'text', X'12AB', CL8'HI', =C'A') 00197403
.*   RETURNS:                                                           00197503
.*       MACPNUL   FOR OMITTED PARAMETER OR EMPTY STRING ('')           00197603
.*       MACQUOT=0  IF UNQUOTED; &STR IN MACQSTR, K'&STR IN MACPLEN     00197703
.*       MACQUOT=1  =CLnn'text' OR =X'hex' - LITERAL FORMAT             00197803
.*                                                                      00197903
         GBLA  &MACPLEN      RETURN SIGNIFICANT LENGTH OF STRING        00198003
         GBLB  &MACPNUL      TRUE IF NULL STRING                        00198103
         GBLB  &MACQUOT      TRUE IF ORIGINAL WAS QUOTED                00198203
         GBLB  &MACPERR      TRUE IF ERROR                              00198303
         GBLC  &MACQSTR      RETURN QUOTED STRING                       00198403
         LCLA  &I,&J,&K,&L                                              00198503
         LCLC  &C,&D,&TYPE                                              00198603
.*                                                                      00198703
&MACQUOT SETB  0             SET UNQUOTED                               00198803
&MACPERR SETB  0             SET NOT IN ERROR                           00198903
&MACPLEN SETA  K'&STR        SET PROVISIONAL LENGTH                     00199003
&MACQSTR SETC  '&STR'        DEFAULT - RETURN AS IS                     00199103
&TYPE    SETC  'C'           SET STRING TYPE (C OR X) DEFAULT           00199203
.*  RETURN IF STRING IS NULL                                            00199303
&MACPNUL SETB  (T'&STR EQ 'O')                                          00199403
&MACPNUL SETB  (&MACPNUL OR ('&STR' EQ ''''''))                         00199503
         AIF   (&MACPNUL).MEND   DONE IF NULL STRING                    00199603
.*  RETURN IF STRING IS UNQUOTED                                        00199703
         AIF   (&MACPLEN LT 2).SHORT                                    00199803
&MACQUOT SETB  ('&STR'(1,1) EQ '''' OR '&STR'(&MACPLEN,1) EQ '''')      00199903
.SHORT   AIF   (NOT &MACQUOT).MEND                                      00200003
.*  DELETE LITERAL'S EQUAL SIGN IF PRESENT                              00200103
         AIF   ('&STR'(1,1) NE '=').NOTEQU                              00200203
&MACQSTR SETC  '&MACQSTR'(2,&MACPLEN)  STRIP EQUAL                      00200303
&MACPLEN SETA  K'&MACQSTR              UPDATE LENGTH                    00200403
.*  LOOK FOR LEADING QUOTE, C OR X - FAIL REST                          00200503
.NOTEQU  ANOP  ,                                                        00200603
&C       SETC  '&MACQSTR'(1,1)         ISOLATE FIRST BYTE               00200703
&MACPERR SETB  ('&C' NE '''' AND '&C' NE 'C' AND '&C' NE 'X')           00200803
         AIF   (&MACPERR).ERROR                                         00200903
.*  LOOK FOR LEADING QUOTE, C OR X - FAIL REST                          00201003
.STRING  AIF   ('&C' EQ '''').COUNT                                     00201103
&TYPE    SETC  '&C'                    REMEMBER THE TYPE                00201203
&MACQSTR SETC  '&MACQSTR'(2,K'&MACQSTR)  STRIP TYPE                     00201303
&MACPLEN SETA  K'&MACQSTR              UPDATE LENGTH                    00201403
&C       SETC  '&MACQSTR'(1,1)         ISOLATE FIRST BYTE               00201503
&MACPERR SETB  ('&C' NE '''' AND '&C' NE 'L')                           00201603
         AIF   (&MACPERR).ERROR                                         00201703
.*  LOOK FOR LEADING QUOTE OR L  (I.E., WE WANT 'text' OR CLn'text')    00201803
         AIF   ('&C' EQ '''').COUNT    COUNT LENGTH                     00201903
&L       SETA  0                       NO LENGTH YET                    00202003
.EXPLOOP ANOP  ,                                                        00202103
&MACQSTR SETC  '&MACQSTR'(2,K'&MACQSTR)  STRIP TYPE                     00202203
&MACPLEN SETA  K'&MACQSTR              UPDATE LENGTH                    00202303
&C       SETC  '&MACQSTR'(1,1)         ISOLATE FIRST BYTE               00202403
         AIF   ('&C' EQ '''').HAVEXPL  DONE WITH EXPLICIT LENGTH        00202503
         AIF   ('&C' LT '0' OR '&C' GT '9').ERROR                       00202603
&L       SETA  &L*10+&C                UPDATE LENGTH                    00202703
         AGO   .EXPLOOP                TRY ONE MORE                     00202803
.*   MACQSTR NOW HAS QUOTED STRING, AND L HAS THE LENGTH                00202903
.HAVEXPL ANOP  ,                                                        00203003
&MACPLEN SETA  &L                      RETURN THE LENGTH                00203103
&MACQSTR SETC  '='.'&TYPE'.'L'.'&L'.'&MACQSTR'                          00203203
         MEXIT ,                                                        00203303
.ERROR   ANOP  ,                                                        00203403
&MACPERR SETB  1                       RETURN AN ERROR                  00203503
         MEXIT ,                                                        00203603
.*   MACQSTR IS A QUOTED STRING WHOSE LENGTH WE NEED                    00203703
.*     NOTE THAT APOSTROPHES AND AMPERSANDS ARE DOUBLED (ELSE ERROR)    00203803
.COUNT   ANOP  ,                                                        00203903
&MACPLEN SETA  K'&MACQSTR              UPDATE LENGTH                    00204003
&I       SETA  1                       LOOP INDEX (2 TO K'-2)           00204103
&L       SETA  0                       SIGNIFICANT LENGTH               00204203
.CNTLOOP  ANOP ,                                                        00204303
&I       SETA  &I+1                                                     00204403
&C       SETC  '&MACQSTR'(&I,1)                                         00204503
         AIF   ('&C' NE '''' AND '&C' NE '&&').CNTONE                   00204603
         AIF   (&I GE &MACPLEN-1).ERROR                                 00204703
         AIF   ('&MACQSTR'(&I+1,1) NE '&C').ERROR    ERROR?             00204803
&I       SETA  &I+1          SKIP DOUBLED CARACTER                      00204903
.CNTONE  ANOP  ,                                                        00205003
&L       SETA  &L+1                                                     00205103
         AIF   (&I LE &MACPLEN-2).CNTLOOP                               00205203
&MACPLEN SETA  &L            SET STRIPPED LENGTH                        00205303
         AIF   ('&LEN' EQ '').DEFLN                             GP08090 00205403
&MACQSTR SETC  '='.'&TYPE'.'L('.'&LEN'.')'.'&MACQSTR'           GP08090 00205503
&MACPLEN SETA  &LEN                                             GP08090 00205603
         AGO   .MEND                                            GP08090 00205703
.DEFLN   ANOP  ,                                                GP08090 00205803
&MACQSTR SETC  '='.'&TYPE'.'&MACQSTR'                                   00205903
.MEND    MEND  ,                                                        00206003
./ ADD NAME=MAPCMPRT 0102-99113-06263-0022-00018-00017-00006-GERHARD 00 00206103
         MACRO ,                                                        00206203
&NM    MAPCMPRT &PFX=CPR,&DCB=0,&PRTMODE=0,&DEV=1                       00206303
.*  THIS MACRO MAPS THE COMMON PRINTER DEFINITION SHARED BY PGMTRACE,   00206403
.*    DEBTRACE, EXORCIST, AND ?                                         00206503
         LCLC  &P                                                       00206603
&P       SETC  '&PFX'                                                   00206703
&P.@UDCB DC    A(&DCB)       USER (OPEN) PRINT DCB                      00206803
&P.FGMOD DC    AL1(&PRTMODE)  USER OUTPUT DCB/PRT MODE                  00206903
&P.F@LCL EQU   0                 ADDR IS LOCAL DCB                      00207003
&P.F@WTO EQU   1                 ISSUE WTO (NARROW)                     00207103
&P.F@DCB EQU   2                 ADDR IS AN OPEN (PRINT) DCB            00207203
&P.F@EXT EQU   3                 ADDR IS FOR USER EXIT                  00207303
&P.FXPRT EQU   4                 ADDR IS FOR XPRINT                     00207403
&P.F@PRT EQU   5                 ADDR IS FOR @PRINTER                   00207503
&P.FGOPT DC    AL1(0)        ..RESERVED..                               00207603
&P.FGSPR DC    AL1(0)        ..RESERVED..                               00207703
&P.F@DEV DC    AL1(&DEV)     @PRINTER DEVICE SELECTION BITS             00207803
         MEND  ,                                                        00207903
./ ADD NAME=MAPEXTNT                                                    00208003
         MACRO ,                                                        00208103
&NM      MAPEXTNT &DSECT=YES                            ADDED ON 82105  00208203
         LCLC  &DS                                                      00208303
&DS      SETC  'MAPEXTNT'    SET DEFAULT NAME                           00208403
         AIF   ('&NM' EQ '').HAVEDS                                     00208503
&DS      SETC  '&NM'                                                    00208603
.HAVEDS  AIF   ('&DSECT' NE 'YES').NODSECT                              00208703
&DS      DSECT ,             MAPPING OF DSCB1/DSCB3 EXTENT ENTRY        00208803
         AGO   .COMDS                                                   00208903
.NODSECT ANOP  ,                                                        00209003
&DS      DS    0H            MAPPING OF DSCB1/DSCB3 EXTENT ENTRY        00209103
.COMDS   ANOP  ,                                                        00209203
XTWHOLE  DC    0XL10'0'      DUMMY FOR CLC/XC                           00209303
XTTYPE   DC    X'0'          EXTENT TYPE                                00209403
XTTTRK   EQU   X'01'           TRACK ALIGNMENT                          00209503
XTTLABEL EQU   X'40'           LABEL EXTENT                             00209603
XTTCYL   EQU   X'81'           CYLINDER ALIGNMENT                       00209703
XTTSPLIT EQU   X'80'           SPLIT CYLINDER ALLOCATION                00209803
XTSEQ    DC    X'0'          EXTENT SEQUENCE (0-15)                     00209903
XTLOCYL  DC    H'0'          LOW CYLINDER                               00210003
XTLOTRK  DC    H'0'          LOW TRACK                                  00210103
XTHICYL  DC    H'0'          HIGH CYLINDER                              00210203
XTHITRK  DC    H'0'          HIGH TRACK                                 00210303
XTLEN    EQU   *-&DS         LENGTH OF ONE ENTRY                        00210403
         MEND  ,                                                        00210503
./ ADD NAME=MASKEQU                                                     00210603
         MACRO ,                                                        00210703
         MASKEQU ,                                      ADDED ON 87223  00210803
.*  THE FUNCTION OF THIS SET OF EQUATES IS TO PROVIDE AN EASY           00210903
.*  WAY OF DEFINING CLM, ICM AND SIMILAR MASK BITS, WITHOUT             00211003
.*  HAVING TO USE B' ' FORMS OR INTEGERS.                               00211103
OOOO     EQU   0                                                        00211203
OOOI     EQU   1                                                        00211303
OOIO     EQU   2                                                        00211403
OOII     EQU   3                                                        00211503
OIOO     EQU   4                                                        00211603
OIOI     EQU   5                                                        00211703
OIIO     EQU   6                                                        00211803
OIII     EQU   7                                                        00211903
IOOO     EQU   8                                                        00212003
IOOI     EQU   9                                                        00212103
IOIO     EQU   10                                                       00212203
IOII     EQU   11                                                       00212303
IIOO     EQU   12                                                       00212403
IIOI     EQU   13                                                       00212503
IIIO     EQU   14                                                       00212603
IIII     EQU   15                                                       00212703
         MEND  ,                                                        00212803
./ ADD NAME=MVICC    0102-03105-06284-0153-00016-00014-00000-GERHARD 00 00212903
         MACRO ,                                                        00213003
&N       MVICC &CODE,&REAS,&RESULT=                    NEW 2003.091 GYP 00213103
         GBLC  &ZZCCNAM                                                 00213203
         LCLC  &L                                                       00213303
&L       SETC  'L'''                                                    00213403
         AIF   ('&RESULT' EQ '').NONEW                                  00213503
&ZZCCNAM SETC  '&RESULT'                                                00213603
.NONEW   AIF   ('&ZZCCNAM' NE '').NODEF                                 00213703
         MNOTE *,'MVICC: RESULT= NOT SPECIFIED - DEFAULTED TO RETCODE'  00213803
&ZZCCNAM SETC  'RETCODE'                                                00213903
.NODEF   ANOP  ,                                                        00214003
&N MACPARM &ZZCCNAM+&L&ZZCCNAM-1,&CODE,OP=MVI,OPR=STC,NULL=SKIP,       *00214103
               MODE=REV                                                 00214203
   MACPARM &ZZCCNAM+&L&ZZCCNAM+3,&REAS,OP=MVI,OPR=STC,NULL=SKIP,       *00214303
               MODE=REV                                                 00214403
         MEND  ,                                                        00214503
./ ADD NAME=OICC     0101-03105-06282-0923-00014-00014-00000-GERHARD 00 00214603
         MACRO ,                                                        00214703
&N       OICC  &CODE,&REAS,&RESULT=                    NEW 2003.091 GYP 00214803
         GBLC  &ZZCCNAM                                                 00214903
         LCLC  &L                                                       00215003
&L       SETC  'L'''                                                    00215103
         AIF   ('&RESULT' EQ '').NONEW                                  00215203
&ZZCCNAM SETC  '&RESULT'                                                00215303
.NONEW   AIF   ('&ZZCCNAM' NE '').NODEF                                 00215403
         MNOTE *,'OICC: RESULT= NOT SPECIFIED - DEFAULTED TO RETCODE'   00215503
&ZZCCNAM SETC  'RETCODE'                                                00215603
.NODEF   ANOP  ,                                                        00215703
&N MACPARM &ZZCCNAM+&L&ZZCCNAM-1,&CODE,OP=OI,OPR=STC,NULL=SKIP,MODE=REV 00215803
   MACPARM &ZZCCNAM+&L&ZZCCNAM+3,&REAS,OP=OI,OPR=STC,NULL=SKIP,MODE=REV 00215903
         MEND  ,                                                        00216003
./ ADD NAME=OPTIONGB                                                    00216103
         GBLA  &SYSPRM#            NUMBER OF SYSPARM TOKENS             00216203
         GBLA  &SVCJFCB            0 OR MODJFCB SVC NUMBER       82099  00216303
         GBLA  &SVC@SVC            0 OR @SERVICE ROUTINE SVC     83100  00216403
         GBLA  &SVCTMSX,&SVCTMSY   0 OR UCC-1 (TMS) SVC X/Y NMBR 83100  00216503
         GBLB  &BUGBEAR      (WAS &DEBUG - SAME AS HASP)         81331  00216603
         GBLB  &MVS                1 IF OS/VS2 MVS                      00216703
         GBLB  &MVSSP              1 IF OS/VS2 MVS/SP            82068  00216803
         GBLB  &MVSXA              1 IF OS/VS2 MVS/XA (SP2)      82068  00216903
         GBLB  &MVSESA             1 IF OS/VS2 MVS/ESA (SP3)     90217  00217003
         GBLB  &SVS                1 IF OS/VS2 SVS                      00217103
         GBLB  &VS1                1 IF OS/VS1                   82137  00217203
         GBLC  &CPU                360/370/470                          00217303
         GBLC  &JES2REL            JES2 LEVEL                    85076  00217403
         GBLC  &LOCAL              INSTALLATION ACRONYM/NAME            00217503
         GBLC  &MODEL              360/370/470                          00217603
         GBLC  &SPVEREL            MVS/SP VERSION/RELEASE/LEVEL  82091  00217703
         GBLC  &SYSPRMS(10)        SYSPARM TOKENS                       00217803
         GBLC  &PRTMAC             GEN OPTION FOR LOCAL  MAPS    81142  00217903
         GBLC  &PRTSOR             GEN OPTION FOR SOURCE         81142  00218003
         GBLC  &PRTSYS             GEN OPTION FOR SYSTEM MAPS    81142  00218103
         GBLC  &SYSTEM             MVT/SVS/MVS                          00218203
./ ADD NAME=PGMEXIT  0117-98365-09183-1946-00220-00119-00209-GERHARD 00 00218303
         MACRO ,                                                        00218403
&NM      PGMEXIT &DUMMY,&PFX=,&NEXT=,                                  *00218503
               &RC=,&RC0=,&RC1=,&COPYRET=,&RETADDR=(R14)         83087  00218603
         GBLB  &ZZSVBSM      SET BY SAVEM WHEN BSM IS USED ON ENTRY     00218703
         GBLB  &MVS,&MVSXA,&MVSESA,&OS390,&Z900,&BUGDBO         GP04234 00218803
         GBLC  &SAVTYPE,&SAVNAME                                GP04050 00218903
         GBLC  &MACPLAB                                         GP04051 00219003
.*--------------------------------------------------------------------* 00219103
.*                                                                    * 00219203
.*    PGMEXIT PROVIDES THE LOGICAL END OF A PROGRAM INITIATED WITH    * 00219303
.*    A PGMHEAD REQUEST. INFORMATION IS PASSED WITH GLOBALS, AND THE  * 00219403
.*    CODE DOES NOT SUPPORT INTERLEAVED PGMHEAD/PGMEXIT STATEMENTS.   * 00219503
.*                                                                    * 00219603
.*    THE PARAMETERS ARE:                                             * 00219703
.*                                                                    * 00219803
.*    RC=     NUMERIC VALUE (0-4095), REGISTER, OR RELOCATABLE WORD   * 00219903
.*    RC0=    NUMERIC VALUE (0-4095), REGISTER, OR RELOCATABLE WORD   * 00220003
.*    RC1=    NUMERIC VALUE (0-4095), REGISTER, OR RELOCATABLE WORD   * 00220103
.*                                                                    * 00220203
.*    RC LOADS REGISTER 15 (STANDARD RETURN CODE CONVENTION)          * 00220303
.*    RC0 AND RC1 LOAD REGISTERS 0 AND 1 AND ARE OPTIONAL             * 00220403
.*                                                                    * 00220503
.*    COPYRET=ADDRESS  OR COPYRET=(ADDRESS) LOAD R15 FROM STORAGE     * 00220603
.*    COPYRET=(ADDRESS,LENGTH)  LOAD R15,R0, ETC. DEPENDING ON LENGTH * 00220703
.*      ADDRESS IS A RELOCATABLE; LENGTH MUST BE AN ABSOLUTE TERM     * 00220803
.*                                                                    * 00220903
.*    ANY REGISTER NOT SPECIFIED IS RESTORED TO ITS VALUE ON ENTRY,   * 00221003
.*      UNLESS THE NOSAVE OPTION IS IN EFFECT                         * 00221103
.*                                                                    * 00221203
.*    R14 IS NOT SUPPORTED FOR A REGISTER OPERAND                     * 00221303
.*    R15, R0, AND R1 ARE ALLOWED PROVIDING THEY DO NOT CONFLICT      * 00221403
.*      E.G.  RC=(R15),RC1=(R1) IS VALID                              * 00221503
.*            RC=(R1),RC1=(R15) WILL FAIL                             * 00221603
.*                                                                    * 00221703
.*    RETADDR=R14 SPECIFIES THE REGISTER CONTAINING THE RETURN        * 00221803
.*      ADDRESS. IN BSM MODE, THIS MUST INCLUDE THE APPROPRIATE MODE  * 00221903
.*      SETTING BITS. OPERAND IS IGNORED IN BAKR/PR AND XCTL MODES.   * 00222003
.*                                                                    * 00222103
.*    PFX= SPECIFIES AN OVERRIDE TO THE SAVE AREA AND REGISTER NAME   * 00222203
.*      PREFIX. BY DEFAULT THE PFX FROM PGMHEAD IS USED.              * 00222303
.*                                                                    * 00222403
.*    NEXT= SPECIFIES THE NAME OF A MODULE TO XCTL TO, EITHER AS AN   * 00222503
.*      UNQUOTED NAME, OR AS QUOTED STRING, OR AS =CL8' ' LITERAL.    * 00222603
.*                                                                    * 00222703
.*                                                                    * 00222803
.*    THIS MACRO WAS SUGGESTED BY ENDM WRITTEN BY SHMUEL (SEYMOUR J.  * 00222903
.*    METZ, WHICH IS COPYRIGHT 1978 BY SHMUEL (SEYMOUR J.) METZ       * 00223003
.*                                                                      00223103
.*    THIS MACRO IS NOT TO BE DISTRIBUTED WITHOUT PERMISSION,           00223203
.*    AS DESCRIBED IN MEMBER $$RIGHTS.                                  00223303
.*                                                                      00223403
.*--------------------------------------------------------------------* 00223503
         LCLC  &SAVBASE      SAVE AREA START                            00223603
         LCLC  &OSVREG,&C    WORK REGISTER FOR HIGH LEVEL SAVE AREA     00223703
         LCLB  &NOSAVE,&OLDSAVE  PASSED BY PGMEXIT              GP04051 00223803
         LCLB  &OSVLOAD      FLAG THAT OSVREG LOADED AND SET            00223903
         LCLB  &LR15         FLAG THAT R15 HAS RC                       00224003
         LCLB  &LR0          FLAG THAT R15 HAS RC0                      00224103
         LCLB  &LR1          FLAG THAT R15 HAS RC0                      00224203
         LCLA  &I,&K                                                    00224303
&I       SETA  &SYSNDX                                                  00224403
&MACPLAB SETC  '&NM'                                            GP04051 00224503
         AIF   ('&PFX' EQ '').NOPFX                             GP04050 00224603
&SAVNAME SETC  '&PFX'                                                   00224703
.NOPFX   AIF   ('&SAVNAME' NE '').DFPFX                         GP04050 00224803
&SAVNAME SETC  'SAVE'                                           GP04050 00224903
.DFPFX   ANOP  ,                                                GP04050 00225003
&SAVBASE SETC '&SAVNAME'.'SPLN'                                         00225103
&NOSAVE  SETB  ('&SAVTYPE' EQ 'NO')                             GP04051 00225203
&OLDSAVE SETB  ('&SAVTYPE' EQ 'OLD')                            GP04051 00225303
.*--------------------------------------------------------------------* 00225403
.*   STEP 1:  EXCEPT FOR NOSAVE, SAVE ANY NON-NUMERIC RETURN CODES    * 00225503
.*       USE R14 AS A WORKING REGISTER                                * 00225603
.*       WITH NOSAVE, JUST LOAD THE NON-NUMERICS                      * 00225703
.*--------------------------------------------------------------------* 00225803
         AIF   (&NOSAVE).CPRCOM                                         00225903
         AIF   (NOT &OLDSAVE).CPROLD                                    00226003
&OSVREG  SETC  'R13'                                                    00226103
&OSVLOAD SETB  1                                                        00226203
         MACPARM R13,&SAVNAME.13-&SAVBASE.(R13),OP=L  LOAD OLD SV       00226303
         AGO   .CPRCOM                                                  00226403
.CPROLD  MACPARM R14,&SAVNAME.13-&SAVBASE.(R13),OP=L  LOAD WORK         00226503
&OSVREG  SETC  'R14'         WORK REGISTER                              00226603
&OSVLOAD SETB  1             WORK REGISTER LOADED                       00226703
.CPRCOM  AIF   (T'&COPYRET EQ 'O').DONCOPY                              00226803
         AIF   (N'&COPYRET LT 2).CPRONE                                 00226903
         AIF   (N'&COPYRET EQ 2).CPRTWO                                 00227003
.CPRBAD  MNOTE 4,' COPYRET PARAMETER INVALID; USE (ADDR-EXPR,LENGTH)'   00227103
         MEXIT ,                                                        00227203
.CPRTWO  AIF   (&NOSAVE).CPRTWON                                        00227303
&MACPLAB MVC   &SAVNAME.15-&SAVBASE.(&COPYRET(2),&OSVREG),&COPYRET(1)   00227403
&MACPLAB SETC  ''                                                       00227503
         AGO   .NOL15                                                   00227603
.CPRTWON MACPARM R15,&COPYRET(2)/4-2,&COPYRET(1),OP=LM                  00227703
         AGO   .NOL15                                                   00227803
.CPRONE  AIF   (&NOSAVE).CPRONEN                                        00227903
&MACPLAB MVC   &SAVNAME.15-&SAVBASE.(4,&OSVREG),&COPYRET(1)             00228003
&MACPLAB SETC  ''                                                       00228103
         AGO   .NOL15                                                   00228203
.CPRONEN MACPLAB R15,&COPYRET(1),OP=L,OPR=LR                            00228303
         AGO   .NOL15                                                   00228403
.*--------------------------------------------------------------------* 00228503
.*    NOTE THAT NUMERIC (T' = 'N') CODES ARE LOADED LATER ON          * 00228603
.*--------------------------------------------------------------------* 00228703
.DONCOPY AIF   (T'&RC EQ 'O').NOL15                                     00228803
         AIF   (T'&RC EQ 'N').NOL15                             GP04051 00228903
         AIF   (&NOSAVE).NSL15                                          00229003
         AIF   ('&RC'(1,1) EQ '(').STL15                                00229103
.NSL15   MACPARM R15,&RC,OP=L                                   GP04051 00229203
         AIF   (&NOSAVE).NOL15                                          00229303
         MACPARM R15,&SAVNAME.15-&SAVBASE.(,&OSVREG),OP=ST      GP04051 00229403
         AGO   .NOL15                                           GP04051 00229503
.STL15   MACPARM &RC(1),&SAVNAME.15-&SAVBASE.(,&OSVREG),OP=ST   GP04051 00229603
.NOL15   AIF   (T'&RC0 EQ 'O').NOL0                                     00229703
         AIF   (T'&RC0 EQ 'N').NOL0                             GP04051 00229803
         AIF   (&NOSAVE).NSL0                                           00229903
         AIF   ('&RC0'(1,1) EQ '(').STL0                                00230003
.NSL0    MACPARM R0,&RC0,OP=L                                   GP04051 00230103
         AIF   (&NOSAVE).NOL0                                           00230203
         MACPARM R0,&SAVNAME.0-&SAVBASE.(,&OSVREG),OP=ST        GP04051 00230303
         AGO   .NOL0                                            GP04051 00230403
.STL0    MACPARM &RC0(1),&SAVNAME.0-&SAVBASE.(,&OSVREG),OP=ST   GP04051 00230503
.NOL0    AIF   (T'&RC1 EQ 'O').DONLREG                                  00230603
         AIF   (T'&RC1 EQ 'N').DONLREG                          GP04051 00230703
         AIF   (&NOSAVE).NSL1                                           00230803
         AIF   ('&RC1'(1,1) EQ '(').STL1                                00230903
.NSL1    MACPARM R1,&RC1,OP=L                                   GP04051 00231003
         AIF   (&NOSAVE).DONLREG                                        00231103
         MACPARM R1,&SAVNAME.1-&SAVBASE.(,&OSVREG),OP=ST        GP04051 00231203
         AGO   .DONLREG                                         GP04051 00231303
.STL1    MACPARM &RC1(1),&SAVNAME.1-&SAVBASE.(,&OSVREG),OP=ST   GP04051 00231403
.*--------------------------------------------------------------------* 00231503
.*   STEP 2: REGAIN OLD SAVE AREA                                     * 00231603
.*--------------------------------------------------------------------* 00231703
.DONLREG AIF   (&NOSAVE).DONLSAV                                        00231803
         AIF   (&OLDSAVE).NSVSAVE                                       00231903
         MACPARM R1,(R13),OP=LR        SAVE STORAGE ADDRESS             00232003
.NSVSAVE AIF   (NOT &OSVLOAD).NSVLOAD  GET CALLER'S SAVE AREA           00232103
         AIF   ('&OSVREG' EQ 'R13').DONLSAV  HAVE IT ALREADY            00232203
         MACPARM R13,(&OSVREG),OP=LR   SKIP STORAGE IF HAVE             00232303
         AGO   .DONLSAV                                                 00232403
.NSVLOAD MACPARM R13,&SAVNAME.13-&SAVBASE.(R13),OP=L  OLD SAVE AREA     00232503
.*--------------------------------------------------------------------* 00232603
.*   STEP 3: FREE WORKING STORAGE                                     * 00232703
.*--------------------------------------------------------------------* 00232803
.DONLSAV AIF   (&NOSAVE OR &OLDSAVE).DONFREE                            00232903
         MACPARM R0,&SAVNAME.SPLN-&SAVBASE.(R1),OP=L                    00233003
         AIF   (&MVSESA).STOREL                                 GP04234 00233103
         ICM   R15,7,&SAVNAME.SPLN+1-&SAVBASE.(R1)  ANY LENGTH? GP04234 00233203
         BZ    ZZZ&I.L       SKIP IF ZERO LENGTH                GP04234 00233303
         FREEMAIN R,LV=(0),A=(1)  FREE STORAGE                  GP04234 00233403
ZZZ&I.L  DS    0H                                               GP06277 00233503
         AGO   .COMREL                                          GP04234 00233603
.STOREL  LR    R15,R0        COPY POSSIBLE SUBPOOL              GP04051 00233703
         SLL   R0,8                                                     00233803
         SRA   R0,8          REMOVE SUBPOOL                             00233903
         BZ    ZZZ&I.L       SKIP IF ZERO LENGTH                        00234003
         SRL   R15,24        RIGHT-JUSTIFY SUBPOOL                      00234103
.*       STORAGE RELEASE,ADDR=(1),LENGTH=(0),SP=(15)                    00234203
         STORAGE RELEASE,ADDR=(1),LENGTH=(0),SP=(15)                    00234303
.COMREL  ANOP  ,                                                GP04234 00234403
.*--------------------------------------------------------------------* 00234503
.*   STEP 4: LOAD NUMERIC RETURN CODES                                * 00234603
.*--------------------------------------------------------------------* 00234703
.DONFREE AIF   (T'&COPYRET NE 'O').LARDONE  COPYRET DONE ALREADY        00234803
         AIF   (T'&RC NE 'N').NOR15                                     00234903
&LR15    SETB  1             SHOW REGISTER LOADED                       00235003
         MACPARM R15,&RC,OP=LA                                  GP04051 00235103
.NOR15   AIF   (T'&RC0 NE 'N').NOR0                                     00235203
         MACPARM R0,&RC0,OP=LA                                          00235303
&LR0     SETB  1             SHOW REGISTER LOADED                       00235403
.NOR0    AIF   (T'&RC1 NE 'N').LARDONE                                  00235503
         MACPARM R1,&RC1,OP=LA                                          00235603
&LR1     SETB  1             SHOW REGISTER LOADED                       00235703
.*--------------------------------------------------------------------* 00235803
.*   STEP 5: RESTORE NON-RC REGISTERS AS NEEDED                       * 00235903
.*--------------------------------------------------------------------* 00236003
.LARDONE AIF   (&NOSAVE).LNRDONE                                        00236103
&OSVREG  SETC  'R14'                                                    00236203
         AIF   (NOT &LR15 AND NOT &LR0 AND NOT &LR1).LNRALL             00236303
         AIF   (NOT &LR15).LNR2LM                                       00236403
         MACPARM R14,&SAVNAME.14-&SAVBASE.(R13),OP=L                    00236503
&OSVREG  SETC  'R2'                                                     00236603
         AIF   (&LR1 AND &LR0).LNRALL                                   00236703
&OSVREG  SETC  'R0'                                                     00236803
         AIF   (NOT &LR0 AND NOT &LR1).LNRALL                           00236903
&OSVREG  SETC  'R1'                                                     00237003
         AIF   (&LR0).LNRALL                                            00237103
&OSVREG  SETC  'R2'                                                     00237203
         MACPARM R0,&SAVNAME.0-&SAVBASE.(R13),OP=L                      00237303
         AGO   .LNRALL                                                  00237403
.LNR2LM  ANOP  ,                                                        00237503
&OSVREG  SETC  'R1'.'+&LR1*4'                                           00237603
         AIF   (NOT &LR0).LNRE0                                         00237703
         MACPARM R14,R15,&SAVNAME.14-&SAVBASE.(R13),OP=LM,MODE=THREE    00237803
         AGO   .LNRALL                                                  00237903
.LNRE0   MACPARM R14,R0,&SAVNAME.14-&SAVBASE.(R13),OP=LM,MODE=THREE     00238003
.LNRALL  ANOP  ,                                                        00238103
&K       SETA   K'&OSVREG                                               00238203
&C       SETC   '&OSVREG'(2,&K-1)                                       00238303
        MACPARM &OSVREG,R12,&SAVNAME.&C-&SAVBASE.(R13),OP=LM,MODE=THREE 00238403
.*--------------------------------------------------------------------* 00238503
.*   STEP 6: XCTL OR RETURN ACCORDING TO ENTRY LINKAGE                * 00238603
.*--------------------------------------------------------------------* 00238703
.LNRDONE AIF   (&NOSAVE).RETFOOT                                GP04051 00238803
         MVI   &SAVNAME.14-&SAVBASE.(R13),X'FF' FLAG AS LAST SAVE AREA  00238903
.RETFOOT AIF   (T'&NEXT NE 'O').GOXCTL                                  00239003
         AIF   (T'&RETADDR EQ 'O').BUGME                        GP09183 00239103
         AIF   (&ZZSVBSM).GOBSM                                         00239203
         MACPARM &RETADDR,OP=B,OPR=BR,MODE=ONE                  GP04051 00239303
         AGO   .BUGME                                           GP09183 00239403
.GOXCTL  ANOP  ,                                                GP04234 00239503
&MACPLAB LA    R15,ZZZ&SYSNDX.X                                         00239603
         XCTL  SF=(E,(15))                                      GP04050 00239703
ZZZ&SYSNDX.X XCTL EP=&NEXT,SF=L                                         00239803
         AGO   .BUGME                                           GP09183 00239903
.GOBSM   MACPARM 0,&RETADDR(1),OP=BSM,OPR=BSM                   GP04051 00240003
.BUGME   AIF   (NOT &BUGDBO).END                                GP09183 00240103
         DBO   MODE=C        EXPAND DEBUG SUPPORT               GP09183 00240203
.END     MEND  ,                                                        00240303
./ ADD NAME=PGMHEAD  8006-04312-10180-0312-00506-00487-00000-GERHARD 00 00240403
         MACRO ,                                                        00240503
&L       PGMHEAD &DUMMY,&PFX=SAVE,&END=,&ENDZERO=,&DSECT=,&PARM=R1,    *00240603
               &SAVE=*,                                          81208 *00240703
               &STARTOF=0,     LISTING OFFSET FOR SUBROUTINES   GP02257*00240803
               &BASE=R12,&BASED=*,&BREG=,                       GP02264*00240903
               &EID=SHORT,&ENTRY=,&ENTNO=,                             *00241003
               &RIGHT=,                                                *00241103
               &ID=*,&DATE=,&SP=0,&BNDRY=,&LOC=,                 82002 *00241203
               &CSECT=CSECT,&XOPT=BSM,&AM=31,&RM=24,&SETAM=     GP02285 00241303
.*                                                                      00241403
.*    REWRITTEN FROM MACRO SAVEM, WITH ESA AND OS/390 CHANGES   GP98365 00241503
.*             COPYRIGHT 1978 BY SHMUEL (SEYMOUR J.) METZ               00241603
.*                        ALL RIGHTS RESERVED.                          00241703
.*             NEW CODE COPYRIGHT 1998 GERHARD POSTPISCHIL      GP98365 00241803
.*                                                              GP98365 00241903
.*             THIS MACRO IS NOT TO BE DISTRIBUTED WITHOUT PERMISSION,  00242003
.*             AS DESCRIBED IN MEMBER $$RIGHTS.                         00242103
.*                                                                      00242203
.*       FOR SIMPLE ENTRIES, PARM=R1 NOW RELOADS R0 AND R1       87223  00242303
.*       BASE=(B1,B2,B3,B4) SUPPORT ADDED (EASIER TO USE?)       87223  00242403
.*                                                               87223  00242503
         COPY  OPTIONGB                                                 00242603
         GBLB  &DROP@1                                           81163  00242703
         GBLB  &MAPONCE                                                 00242803
         GBLB  &AMSET,&RMSET                                    GP98365 00242903
         GBLB  &SAV@REG                                                 00243003
         GBLB  &SAV@DYN(10)                                             00243103
         GBLC  &SAV@NAM(10)                                             00243203
         GBLC  &SAVTYPE,&SAVNAME                                GP04050 00243303
         GBLC  &MACPLAB                                          81154  00243403
         GBLC  &SYSSPLV      VERSION OF SP (1, 2...)             93174  00243503
         GBLC  &SAVZPRM      PGMHEAD SAVED PARM                 GP09247 00243603
         LCLA  &I,&J,&K,&N,&NUMENT                              GP03245 00243703
         LCLB  &BWOPT,&Y           BIGWORK  OPTION                      00243803
         LCLB  &HWOPT              HUGEWORK OPTION ( > 32767 )   84307  00243903
         LCLB  &CME                BASED/ENTRY PRESENT           81163  00244003
         LCLB  &CPYREGS            CPYREGS  OPTION                      00244103
         LCLB  &DSOPT              NODSECT  OPTION                      00244203
         LCLB  &EQUOPT             NOEQU    OPTION                      00244303
         LCLB  &NOENTRY            NOENTRY  OPTION                      00244403
         LCLB  &NOREG              NOREG    OPTION - SKIP YREGS GP04115 00244503
         LCLB  &NOSAVE             NOSAVE   OPTION                      00244603
         LCLB  &NWOPT              NOWORK   OPTION                      00244703
         LCLB  &OLDSAVE            OLDSAVE  OPTION                      00244803
         LCLB  &BZOPT              ZERO > 256                           00244903
         LCLB  &NOT1ST             NOT FIRST USE OF &PFX                00245003
         LCLB  &ZERO               ZERO     OPTION                      00245103
         LCLB  &ZERO8              ZERO     <= 256               81202  00245203
         LCLB  &ZERO12             ZERO     >  256               81202  00245303
         LCLB  &ZERO15             ZERO     > 4095               81208  00245403
         LCLB  &ZERO31             ZERO     > 32767              84307  00245503
         LCLC  &B@                 GENERATED LABEL FOR B TARGET         00245603
         LCLC  &CMB                COMMON BASE NAME              81163  00245703
         LCLC  &CMU                BASE REG. STRING FOR USING    81163  00245803
         LCLC  &DSVAR              NON-RENT SAVE AREA NAME      GP03033 00245903
         LCLC  &DSCTG              DSECT NAME                    81208  00246003
         LCLC  &ENDG               END LABEL FOR GETMAINED AREA  81208  00246103
         LCLC  &ENDZ               END OF ZEROED AREA            81208  00246203
         LCLC  &LAB                LABEL FOR ENTRY POINT                00246303
         LCLC  &LQ                 L'                                   00246403
         LCLC  &N@                 GENERATED NAME FOR DC                00246503
         LCLC  &SECT               CSECT NAME                           00246603
         LCLC  &SP@                SUBPOOL FOR GETMAIN                  00246703
         LCLC  &NAME               NAME FOR CONSTRUCTED ID              00246803
         LCLC  &SV                 SAVE AREA PREFIX              81208  00246903
         LCLC  &BEGZ         WHERE TO START ZEROING (FWD DEFAULT)       00247003
         LCLC  &PARMEXP                                                 00247103
         LCLC  &PARMREG                                                 00247203
         LCLC  &CASE         FIRST/ONLY BASE REG                GP02264 00247303
         LCLC  &C9           TEMP                               GP03245 00247403
         LCLC  &YOPT         COPY OF XOPT OR 'STM'              GP04234 00247503
       SYSPARM ,                   SET GLOBALS                          00247603
&SECT    SETC  '&SYSECT'                                                00247703
&SV      SETC  '&PFX'                                                   00247803
&SAVNAME SETC  '&PFX'                                           GP04050 00247903
&DSCTG   SETC  '&PFX'                                                   00248003
&ENDG    SETC  '&PFX'.'END'                                     GP98365 00248103
&BEGZ    SETC  '&PFX'.'FWD'        ZERO BEGINNING AT FWD LINK    94272  00248203
&SAVTYPE SETC  'DYN'         (AS OPPOSED TO NO OR OLD)          GP04050 00248303
.*                                                                      00248403
&YOPT    SETC  '&XOPT'                                          GP04234 00248503
         AIF   ('&YOPT' EQ 'BSM' AND &MVSESA).ASMVS             GP04234 00248603
&YOPT    SETC  'STM'                                            GP04234 00248703
.*                                                                      00248803
.ASMVS   AIF   ('&DSECT' EQ '').DSGOK                                   00248903
&DSCTG   SETC  '&DSECT'                                                 00249003
.DSGOK   AIF   ('&END' EQ '').ENDGOK                                    00249103
&ENDG    SETC  '&END'                                                   00249203
.ENDGOK  ANOP  ,                                                        00249303
&DSVAR   SETC  '&DSCTG'      DEFAULT START OF SAVE AREA         GP03033 00249403
         AIF   ('&SAVE' EQ '*').ENDGSV                          GP03033 00249503
&DSVAR   SETC  '&SAVE'       FOR LENGTH DEFINITION              GP03033 00249603
.ENDGSV  ANOP  ,                                                GP03033 00249703
.*                                                                      00249803
&N       SETA  1                                                        00249903
.NXTSLOT ANOP  ,                                                        00250003
&NOT1ST  SETB  (&NOT1ST OR ('&PFX' EQ '&SAV@NAM(&N)'))                  00250103
         AIF   (&NOT1ST).FND1ST                                         00250203
         AIF   ('&SAV@NAM(&N)' EQ '').FNDSLOT                           00250303
&N       SETA  &N+1                                                     00250403
         AIF   (&N LE 10).NXTSLOT                                       00250503
         MNOTE 12,'TOO MANY PGMHEAD DSECTS'                     GP98365 00250603
         MEXIT ,                                                        00250703
.FNDSLOT ANOP  ,                                                        00250803
&SAV@NAM(&N) SETC '&PFX'                                                00250903
.*                                                                      00251003
.FND1ST  AIF   ('&SYSECT' EQ '' AND T'&L EQ 'O').NOL                    00251103
&I       SETA  1                                                        00251203
         AIF   (N'&SYSLIST EQ 0).ENDOPT                                 00251303
.*                                                                      00251403
.LOOP    AIF   ('&SYSLIST(&I)' EQ 'CPYREGS').CPYREGS                    00251503
         AIF   ('&SYSLIST(&I)' EQ 'COPYREGS').CPYREGS            81154  00251603
         AIF   ('&SYSLIST(&I)' EQ 'COPYREGISTERS').CPYREGS              00251703
         AIF   ('&SYSLIST(&I)' EQ 'NODSECT').NODSECT                    00251803
         AIF   ('&SYSLIST(&I)' EQ 'NOEQU').NOEQU                        00251903
         AIF   ('&SYSLIST(&I)' EQ 'NOREG').NOREGS               GP04115 00252003
         AIF   ('&SYSLIST(&I)' EQ 'NOREGS').NOREGS              GP04115 00252103
         AIF   ('&SYSLIST(&I)' EQ 'BIGWORK').BIGWORK                    00252203
         AIF   ('&SYSLIST(&I)' EQ 'HUGEWORK').HUGWORK            84307  00252303
         AIF   ('&SYSLIST(&I)' EQ 'BIGZERO').BIGZERO                    00252403
         AIF   ('&SYSLIST(&I)' EQ 'HUGEZERO').ZERO31             84307  00252503
         AIF   ('&SYSLIST(&I)' EQ 'NOWORK').NOWORK                      00252603
         AIF   ('&SYSLIST(&I)' EQ 'NOSAVE').NOSAVE1                     00252703
         AIF   ('&SYSLIST(&I)' EQ 'OLDSAVE').OLDSAVE                    00252803
         AIF   ('&SYSLIST(&I)' EQ 'NOENTRY').NOENTRY                    00252903
         AIF   ('&SYSLIST(&I)' EQ 'ZERO').ZERO                          00253003
         AIF   ('&SYSLIST(&I)' EQ 'ZERO8').ZERO8                 81208  00253103
         AIF   ('&SYSLIST(&I)' EQ 'ZERO12').ZERO12               81208  00253203
         AIF   ('&SYSLIST(&I)' EQ 'ZERO15').ZERO15               81208  00253303
         AIF   ('&SYSLIST(&I)' EQ 'ZERO31').ZERO31               84307  00253403
         AIF   ('&SYSLIST(&I)' EQ '').NXTOPT                            00253503
         MNOTE 4,'''&SYSLIST(&I)'' IS AN INVALID OPTION - IGNORED'      00253603
         AGO   .NXTOPT                                                  00253703
.*                                                                      00253803
.NOL     MNOTE 12,'LABEL REQUIRED IF NO CSECT'                          00253903
         MEXIT ,                                                        00254003
.*                                                                      00254103
.CPYREGS ANOP  ,                                                        00254203
&CPYREGS SETB  1                                                        00254303
         AGO   .NXTOPT                                                  00254403
.NODSECT ANOP  ,                                                        00254503
&DSOPT   SETB  1                                                        00254603
         AGO   .NXTOPT                                                  00254703
.NOEQU   ANOP  ,                                                        00254803
&EQUOPT  SETB  1                                                        00254903
         AGO   .NXTOPT                                                  00255003
.NOREGS  ANOP  ,                                                GP04115 00255103
&NOREG   SETB  1                                                GP04115 00255203
         AGO   .NXTOPT                                          GP04115 00255303
.HUGWORK ANOP  ,                                                 84307  00255403
&HWOPT   SETB  1             SET FOR HUGE WORK AREA              84307  00255503
.BIGWORK ANOP  ,                                                        00255603
&BWOPT   SETB  1                                                        00255703
&BZOPT   SETB  1                                                        00255803
         AGO   .NXTOPT                                                  00255903
.NOWORK  ANOP  ,                                                        00256003
&NWOPT   SETB  1                                                        00256103
         AGO   .NXTOPT                                                  00256203
.NOSAVE1 ANOP  ,                                                        00256303
&NOSAVE  SETB  1                                                        00256403
&SAVTYPE SETC  'NO'                                             GP04050 00256503
         AGO   .NXTOPT                                                  00256603
.OLDSAVE ANOP  ,                                                        00256703
&OLDSAVE SETB  1                                                        00256803
&SAVTYPE SETC  'OLD'                                            GP04050 00256903
         AGO   .NXTOPT                                                  00257003
.NOENTRY ANOP  ,                                                        00257103
&NOENTRY SETB  1                                                        00257203
         AGO   .NXTOPT                                                  00257303
.*                                                               81208  00257403
.ZERO8   ANOP  ,                                                 81208  00257503
&ZERO8   SETB  1                                                 81208  00257603
         AGO   .ZERO                                             81208  00257703
.ZERO12  ANOP  ,                                                 81208  00257803
&ZERO12  SETB  1                                                 81218  00257903
         AGO   .ZERO                                             81208  00258003
.ZERO31  ANOP  ,                                                 84307  00258103
&HWOPT   SETB  1             HUGE WORK AREA > 32767              84307  00258203
&ZERO31  SETB  1                                                 84307  00258303
.ZERO15  ANOP  ,                                                 81208  00258403
&ZERO15  SETB  1                                                 81208  00258503
&BWOPT   SETB  1                                                 81218  00258603
         AGO   .ZERO                                             81208  00258703
.BIGZERO ANOP  ,                                                        00258803
&BZOPT   SETB  1                                                 81202  00258903
.ZERO    ANOP  ,                                                        00259003
&ZERO    SETB  1                                                        00259103
.NXTOPT  ANOP  ,                                                        00259203
&I       SETA  &I+1                                                     00259303
         AIF   (&I LE N'&SYSLIST).LOOP                                  00259403
.ENDOPT  AIF   (&ZERO8 OR &ZERO12 OR &ZERO15).GOTZERO            81208  00259503
&ZERO31  SETB  (&ZERO AND &HWOPT)                                84307  00259603
&ZERO15  SETB  (&ZERO AND &BWOPT)                                81208  00259703
&ZERO12  SETB  (&BZOPT AND NOT &ZERO15)                          81208  00259803
&ZERO8   SETB  (&ZERO AND NOT &ZERO12 AND NOT &ZERO15)           81202  00259903
.GOTZERO AIF   (T'&L EQ 'O').CSECTOK                             81208  00260003
         AIF   ('&SYSECT' EQ '' OR '&L' EQ '&SYSECT').LABOK      81202  00260103
&LAB     SETC  '&L'                                                     00260203
         AIF   (&NOENTRY).LABOK                                         00260303
         SPACE 1                                                GP04050 00260403
         ENTRY &L                                                       00260503
.LABOK   AIF   ('&SYSECT' NE '').CSECTOK                                00260603
         AIF   ('&CSECT' EQ 'CSECT').CSECTDO                    GP98322 00260703
         AIF   ('&CSECT' EQ 'RSECT').RSECTDO                    GP98322 00260803
         AIF   ('&CSECT' EQ 'START').STARTDO                    GP98322 00260903
         MNOTE 4,'INVALID CSECT OPERAND; USE CSECT OR RSECT'    GP98322 00261003
         AGO   .CSECTDO      TRY TO CONTINUE?                   GP98322 00261103
.*                                                              GP98322 00261203
.STARTDO ANOP  ,                                                GP98322 00261303
&L       START &STARTOF                                         GP04234 00261403
         AGO   .CSECTCM                                         GP98322 00261503
.*                                                              GP98322 00261603
.RSECTDO AIF   (NOT &MVSESA).CSECTDO                            GP04234 00261703
&L       RSECT ,                                                GP98322 00261803
         AGO   .CSECTCM                                         GP98322 00261903
.*                                                              GP98322 00262003
.CSECTDO ANOP  ,                                                GP98322 00262103
&L       CSECT ,                                                        00262203
.CSECTCM ANOP  ,                                                        00262303
&SECT    SETC  '&L'                                                     00262403
         AIF   ('&AM' EQ '' OR &AMSET).NOAM                     GP98365 00262503
         AIF   (NOT &MVSESA).NOAM                               GP04234 00262603
&L       AMODE &AM                                                      00262703
&AMSET   SETB  1                                                GP98365 00262803
.NOAM    AIF   ('&RM' EQ '' OR &RMSET).CSECTOK                  GP98365 00262903
         AIF   (NOT &MVSESA).CSECTOK                            GP04234 00263003
         AIF   ('&RM' EQ '24' OR '&RM' EQ 'ANY').SETRM          GP99120 00263103
         AIF   ('&RM' EQ '31').ANYRM                            GP99120 00263203
         MNOTE 4,'INVALID RM=&RM '                              GP99120 00263303
.ANYRM   ANOP  ,                                                GP99120 00263403
&L       RMODE ANY                                              GP99120 00263503
         AGO   .FLGRM                                           GP99120 00263603
.SETRM   ANOP  ,                                                GP99120 00263703
&L       RMODE &RM                                              GP98365 00263803
.FLGRM   ANOP  ,                                                GP99120 00263903
&RMSET   SETB  1                                                GP98365 00264003
.CSECTOK AIF   (T'&BASED EQ 'O' AND T'&ENTRY EQ 'O').NOCLAB      81163  00264103
         AIF   (T'&ENTRY NE 'O').DOCLAB                          81163  00264203
         AIF   ('&BASED' EQ '*').NOCLAB                          81163  00264303
.DOCLAB  ANOP  ,                                                 81163  00264403
&CME     SETB  1             SET SPECIAL BASE PROCESSING         81163  00264503
.NOCLAB  AIF   ('&LAB' NE '').OKCLAB                             81163  00264603
&LAB     SETC  'A@&SYSNDX'                                       81163  00264703
.OKCLAB  ANOP  ,                                                 81163  00264803
&CMB     SETC  '&LAB'        DEFAULT BASED VALUE                 81163  00264903
         AIF   (T'&BASED EQ 'O').CLABOK                          81163  00265003
         AIF   ('&BASED' EQ '*').CLABOK                          81163  00265103
         AIF   ('&BASED' NE '*SYSECT').CLABSET                   81163  00265203
         AIF   ('&SYSECT' EQ '').CLABOK   BOO                    81163  00265303
&CMB     SETC  '&SYSECT'                                         81163  00265403
         AGO   .CLABOK                                           81163  00265503
.CLABSET ANOP  ,                                                 81163  00265603
&CMB     SETC  '&BASED'                                          81163  00265703
.CLABOK  ANOP  ,                                                 81163  00265803
&CMU     SETC  'R15'         DEFAULT BASE REGISTER ON USING      81163  00265903
&SAV@DYN(&N) SETB (NOT &NOSAVE)                                         00266003
&PARMEXP SETC  '(R1)'                                                   00266103
&PARMREG SETC  '1'                                                      00266203
&SAVZPRM SETC  ''            PASS TO PARMLOAD MACRO             GP10180 00266303
         AIF   ('&PARM' EQ '').PARM1                                    00266403
&SAVZPRM SETC  '&PARM(1)'    PASS TO PARMLOAD MACRO             GP10180 00266503
         AIF   (N'&PARM LT 2).PARM1                                     00266603
&PARMEXP SETC  '&PARM(2)'                                               00266703
         AIF   (NOT &OLDSAVE).PARM1                                     00266803
&PARMREG SETC  '&PARM(2)'                                               00266903
         AIF   ('&PARMEXP'(1,1) EQ '(').STRIP                           00267003
         MNOTE 8,'PARM=&PARM INVALID'                                   00267103
         MNOTE 8,'PARM=(&PARM(1),(&PARM(2)) ASSUMED'                    00267203
         AGO   .STRIPT                                                  00267303
.STRIP   AIF   ('&PARMEXP'(K'&PARMEXP,1) EQ ')').STRIP1                 00267403
         MNOTE 12,'PARM=&PARM INVALID'                                  00267503
         MEXIT ,                                                        00267603
.STRIP1  ANOP  ,                                                        00267703
&PARMREG SETC  '&PARMREG'(2,K'&PARMREG-2)                               00267803
.STRIPT  AIF   ('&PARMREG'(1,1) GE '0').PARM1                           00267903
&PARMREG SETC  '&PARMREG'(2,K'&PARMREG-1)                               00268003
.PARM1   AIF   (NOT &DROP@1).NODROP                              81163  00268103
         DROP  ,                                                 81163  00268203
.NODROP  ANOP  ,                                                 81163  00268303
&DROP@1  SETB  1                                                 81163  00268403
         AIF   (T'&BASE NE 'O').NEWBASE                         GP02264 00268503
.NOBASE  AIF   (T'&ENTRY NE 'O').NOBASEU                         81163  00268603
         DS    0H                                                       00268703
         USING *,R15                                             81163  00268803
.NOBASEU ANOP  ,                                                 81163  00268903
&CMU     SETC  'R15'                                             81163  00269003
&CASE    SETC  'R15'                                             81163  00269103
         AGO   .BASED                                                   00269203
.NEWBASE ANOP  ,                                                GP02264 00269303
&K       SETA  N'&BASE      MAX NUMBER OF BASES SPECIFIED       GP02264 00269403
&CMU     SETC  '&BASE(1)'    SET THE FIRST ONE                  GP02264 00269503
&CASE    SETC  '&BASE(1)'    SET THE FIRST ONE                  GP02264 00269603
.BASED   AIF   (&NOSAVE).BASED2                                 GP04050 00269703
         USING &DSCTG,R13                                               00269803
.BASED2  AIF   ('&ID' EQ 'NO').NAMEOK                           GP05017 00269903
&B@      SETC  'B@&SYSNDX'                                              00270003
&N@      SETC  'N@&SYSNDX'                                              00270103
&LQ      SETC  'L'''                                                    00270203
&LAB     B     &B@-*(,R15)                                              00270303
&LAB     SETC  ''                                               GP05017 00270403
         DC    AL1(&LQ&N@)                                              00270503
         AIF   ('&ID' NE '*').USEID                                     00270603
&NAME    SETC  '&L'                                                     00270703
         AIF   (T'&L NE 'O').USENAME                                    00270803
&NAME    SETC  '&SYSECT'                                                00270903
.USENAME AIF   ('&RIGHT' EQ '').NORIGHT                                 00271003
&NAME    SETC  '&NAME'.' '.'COPYRIGHT '.'&RIGHT'                        00271103
.NORIGHT AIF   ('&DATE' EQ 'NO').NODATE                                 00271203
&N@      DC    C'&NAME - &SYSDATE - &SYSTIME'                           00271303
         AGO   .NAMEOK                                                  00271403
.NODATE  ANOP  ,                                                        00271503
&N@      DC    C'&NAME'                                                 00271603
         AGO   .NAMEOK                                                  00271703
.USEID   ANOP  ,                                                        00271803
         AIF   ('&ID'(1,1) NE '''').USEIDQ                              00271903
&N@      DC    C&ID                                                     00272003
         AGO   .NAMEOK                                                  00272103
.USEIDQ  ANOP  ,                                                        00272203
&N@      DC    C'&ID'                                                   00272303
.NAMEOK  ANOP  ,                                                GP05017 00272403
&LAB     MACPARM MODE=LBL                                       GP05017 00272503
         AIF   (T'&ENTRY EQ 'O').NOENTR                          81163  00272603
&I       SETA  0                                                 81163  00272703
&J       SETA  N'&ENTRY                                          81163  00272803
&N@      SETC  ''            SHORT ID                            81163  00272903
         AIF   ('&EID' EQ 'SHORT').ENTRSH                        81163  00273003
&N@      SETC  ' - '.'&SYSDATE'.' - '.'&SYSTIME'                 81163  00273103
.ENTRSH  AIF   (&I GE &J).ENTRDN                                 81163  00273203
&I       SETA  &I+1                                              81163  00273303
&C9      SETC  '&ENTRY(&I)'                                      81163  00273403
         AIF   (&NOENTRY).ENTRNN                                 81347  00273503
         SPACE 1                                                GP04051 00273603
         ENTRY &C9                                               81163  00273703
.ENTRNN  AIF   (T'&ENTNO EQ 'O').ENTRNM                          88255  00273803
&NUMENT  SETA  &NUMENT+1     INCREASE ENTRY NUMBER               88255  00273903
         DC    Y(&NUMENT)    MAKE ENTRY PREFIX                   88255  00274003
.ENTRNM  ANOP  ,                                                 88255  00274103
&C9      B     &B@-*(,R15)                                       81163  00274203
         AIF   ('&EID' EQ 'NONE').ENTRSH                        GP99055 00274303
&N       SETA  K'&N@+K'&C9                                       81163  00274403
&N       SETA  ((&N/2)*2)+1  MAKE ODD LENGTH FOR ALIGNMENT       81163  00274503
         DC    AL1(&N),CL(&N)'&ENTRY(&I)&N@'                     81163  00274603
         AGO   .ENTRSH                                           81163  00274703
.ENTRDN  AIF   (T'&ENTNO EQ 'O').ENTRDM                          88255  00274803
         DC    Y(0)          SET ENTRY PREFIX =0 (MAIN)          88255  00274903
.ENTRDM  AIF   (&NOSAVE).ELDSVAM                                GP04050 00275003
.*WHY?   USING &C9,R15                                          GP99158 00275103
&B@      SAVEX R14,R12,&SV.14,TYPE=&YOPT,SETAM=&SETAM           GP99018 00275203
&B@      SETC  ''                                                81163  00275303
.*WHY?   DROP  R15                                              GP99158 00275403
         AGO   .ELDSV                                           GP04050 00275503
.ELDSVAM AIF   ('&SETAM' EQ '').ELDSV                           GP04050 00275603
         AIF   (NOT &MVSESA).ELDSV                              GP04234 00275703
         BASR  &CASE,0                                          GP04050 00275803
         USING *,&CASE                                          GP04050 00275903
         AM&SETAM WORK=&CASE                                    GP04050 00276003
         DROP  &CASE                                            GP04050 00276103
.ELDSV   AIF   ('&CASE' EQ '').ELDSVLR                           87223  00276203
         AIF   ('&BASED' NE '*').ELDSVLR                         81263  00276303
&B@      BASR  &CASE,0                                           93006  00276403
         LA    R15,*-&CMB                                        81263  00276503
         SLR   &CASE,R15                                         87223  00276603
         AGO   .COMBAS2                                          81263  00276703
.ELDSVLR ANOP  ,                                                 81263  00276803
&B@      BASR  R15,0                                             93006  00276903
         USING *,R15                                             81163  00277003
         AIF   ('&CASE' EQ '').NOBASE2                           87223  00277103
         L     &CASE,=A(&CMB)                                    87223  00277203
         DROP  R15                                               81163  00277303
         AGO   .COMBAS2                                          81163  00277403
.NOENTR  AIF   (&NOSAVE).OLDSVAM                                GP04050 00277503
.*WHY    USING &LAB,R15                                         GP99158 00277603
&B@      SAVEX R14,R12,&SV.14,TYPE=&YOPT,SETAM=&SETAM           GP98322 00277703
&B@      SETC  ''                                                       00277803
.*WHY    DROP  R15                                              GP99158 00277903
         AGO   .OLDSV                                           GP04050 00278003
.OLDSVAM AIF   ('&SETAM' EQ '').OLDSV                           GP04050 00278103
         AIF   (NOT &MVSESA).OLDSV                              GP04234 00278203
         BASR  &CASE,0                                          GP04050 00278303
         USING *,&CASE                                          GP04050 00278403
         AM&SETAM WORK=&CASE                                    GP04050 00278503
         DROP  &CASE                                            GP04050 00278603
.OLDSV   AIF   ('&CASE' EQ '').NOBASE2                           87223  00278703
         AIF   ('&BREG' NE 'SET').BASREG                        GP05017 00278803
&B@      BASR  &CASE,0                                          GP05017 00278903
         LA    R15,*-&CMB                                       GP05017 00279003
         SLR   &CASE,R15                                        GP05017 00279103
         AGO   .COMBAS2                                         GP05017 00279203
.BASREG  AIF   (&CME).BASEL                                      81163  00279303
&B@      LA    &CASE,0(,R15)  REMOVE AM BIT                             00279403
         AGO   .COMBAS2                                          81163  00279503
.BASEL   USING &LAB,R15                                          81163  00279603
&B@      L     &CASE,=A(&CMB)                                    87223  00279703
         DROP  R15                                               81163  00279803
.COMBAS2 ANOP  ,                                                GP02264 00279903
&K       SETA  N'&BASE                                          GP02264 00280003
         AIF   (&K LT 2).NOBASE2                                GP02264 00280103
&I       SETA  1                                                GP02264 00280203
         LA    &BASE(&K),2048                                   GP02264 00280303
.NOBASLP AIF   (&I GE &K).NOBASE2                               GP02264 00280403
&I       SETA  &I+1                                             GP02264 00280503
         AIF   ('&BASE(&I)' EQ '').NOBASLP                      GP02264 00280603
         LA    &BASE(&I),2048(&BASE(&K),&BASE(&I-1))            GP02264 00280703
&CMU     SETC  '&CMU'.','.'&BASE(&I)'                           GP02264 00280803
         AGO   .NOBASLP                                         GP02264 00280903
.NOBASE2 AIF   ('&CASE' EQ '').NOUSEB                            87223  00281003
         USING &CMB,&CMU                                         81163  00281103
.NOUSEB  AIF   (NOT &OLDSAVE).NOLDSV                                    00281203
.*WHY?   L     R15,&SV.13                                       GP04050 00281303
.*WHY?   ST    &CASE,&SV.15-&DSCTG.(,R15)                        87223  00281403
         AIF   ('&PARM' EQ '').NOLDSV1                                  00281503
         AIF   ('&PARM' EQ 'R1' OR '&PARM' EQ '1').NOLDSV1      GP04052 00281603
         AIF   (&NOSAVE).NOLDSV1                                GP04052 00281703
         L     &PARM(1),&SV.&PARMREG-&DSCTG.(,R13)              GP04050 00281803
         AGO   .NOLDSV1                                                 00281903
.NOLDSV  AIF   (&NOSAVE).NOLDSV1                                        00282003
         AIF   ('&PARM' EQ '').NOPARM                                   00282103
         AIF   ('&PARM' EQ '1' OR '&PARM' EQ 'R1').NOPARM        87223  00282203
&MACPLAB SETC  ''                                                81154  00282303
         MACPARM &PARM(1),&PARMEXP                               81154  00282403
.*                                                                      00282503
.NOPARM  AIF   ('&SAVE' NE '*' AND NOT &ZERO).NOSTO NON-RENT/NO LENGTH  00282603
         AIF   (NOT &HWOPT).LYLEN                                84307  00282703
         L     R14,=A(&ENDG-&DSVAR)                             GP03033 00282803
         AGO   .NOLA                                             84307  00282903
.LYLEN   AIF   (NOT &BWOPT).LALEN                                84307  00283003
         LH    R14,=Y(&ENDG-&DSVAR)                             GP03033 00283103
         AGO   .NOLA                                                    00283203
.LALEN   LA    R14,&ENDG-&DSVAR                                 GP03033 00283303
.*  NOTE THAT R14-R1 ARE USED BY STORAGE                                00283403
.NOLA    ST    R14,&SV.FWD   TEMP: LEN INTO OLD SAVE AREA       GP02304 00283503
         AIF   ('&SAVE' NE '*').NOSTO                           GP03033 00283603
*        STORAGE OBTAIN,LENGTH=(R14),SP=&SP,BNDRY=&BNDRY,LOC=&LOC       00283703
         STORAGE OBTAIN,LENGTH=(R14),SP=&SP,BNDRY=&BNDRY,LOC=&LOC       00283803
         AGO   .GTSTO                                           GP03033 00283903
.NOSTO   LA    R14,&SAVE     LOAD NON-RENT SAVE AREA            GP03033 00284003
         AIF   (&ZERO).ZRSTO                                    GP03033 00284103
         XC    0(4*18,R14),0(R14)  PREVENT S978 IN EXIT         GP03033 00284203
         AGO   .SKPLEN                                          GP03033 00284303
.GTSTO   LR    R14,R1        SAVE OVER CLEAR                    GP02264 00284403
         AIF   (NOT &ZERO).UNCLEAN                              GP02264 00284503
.ZRSTO   SR    R15,R15       ZERO SOURCE LENGTH AND INSERTION   GP02264 00284603
         LR    R0,R14        SET CLEAR ADDRESS                  GP02264 00284703
         L     R1,&SV.FWD    GET SAVED LENGTH                   GP02304 00284803
         MVCL  R0,R14        CLEAR GOTTEN STORAGE               GP02264 00284903
         AIF   ('&SAVE' NE '*').SKPLEN  PREVENT S978 IN EXIT    GP03033 00285003
.UNCLEAN MVC   &SV.SPLN-&DSCTG.(4,R14),&SV.FWD  SET LENGTH FOR PGMEXIT  00285103
.SKPLEN  AIF   ('&SP' EQ '0').NOGM                               82002  00285203
         MVI   &SV.SPLN-&DSCTG.(R14),&SP  AND SUBPOOL           GP02264 00285303
.NOGM    ST    R14,&SV.FWD   MAKE FOWARD SAVE AREA LINK         GP02264 00285403
         ST    R13,&SV.13-&DSCTG.(,R14) MAKE BACKWARD LINK      GP02264 00285503
         AIF   (NOT &CPYREGS).LR13                                      00285603
         MVC   &SV.14-&DSCTG.(&SV.12+4-&SV.14,R14),&SV.14        81151  00285703
.LR13    LR    R13,R14       ESTABLISH NEW SAVE AREA                    00285803
         AIF   (T'&ENTRY EQ 'O' OR T'&ENTNO EQ 'O').NOLDENT      88255  00285903
         L     R1,&SV.13     GET OLD SAVE AREA BACK              88255  00286003
         CLM   &CASE,7,&SV.15+1-&DSCTG.(R1)  MAIN ENTRY ?        88255  00286103
         BE    *+16          YES; DON'T MOVE                     88255  00286203
         L     R1,&SV.15-&DSCTG.(,R1) GET ENTRY ADDRESS BACK     88255  00286303
         BCTR  R1,0          SPACE TO ENTRY COUNTER              88255  00286403
         MVC   &ENTNO+L'&ENTNO-1(1),0(R1) COPY COUNT             88255  00286503
.NOLDSV1 ANOP  ,       TRY IT HERE                              GP03033 00286603
.NOLDENT AIF   ('&PARM' NE '1' AND '&PARM' NE 'R1').NOPARM1      87223  00286703
         AIF   (&NOSAVE OR &OLDSAVE).NOPARM1                    GP04052 00286803
         L     R1,&SV.13     OLD SAVE AREA                       87223  00286903
         LM    R0,R1,&SV.0-&DSCTG.(R1)  RESTORE ENTRY REGISTERS  94272  00287003
.NOPARM1 AIF   (&NOT1ST).END                                            00287103
         AIF   (&DSOPT OR &NOSAVE).DSOPT                        GP04050 00287203
&DSCTG   DSECT ,                                                GP04051 00287303
&SV.SPLN DS    F                                                        00287403
&SV.13   DS    F                                                        00287503
&SV.FWD  DS    A                                                        00287603
&SV.14   DS    A                                                        00287703
&SV.15   DS    A                                                        00287803
&SV.0    DS    A                                                        00287903
&SV.1    DS    A                                                        00288003
&SV.2    DS    A                                                        00288103
&SV.3    DS    A                                                        00288203
&SV.4    DS    A                                                        00288303
&SV.5    DS    A                                                        00288403
&SV.6    DS    A                                                        00288503
&SV.7    DS    A                                                        00288603
&SV.8    DS    A                                                        00288703
&SV.9    DS    A                                                        00288803
&SV.10   DS    A                                                        00288903
&SV.11   DS    A                                                        00289003
&SV.12   DS    A                                                        00289103
&SV.FWK  EQU   *                                                 94272  00289203
         AIF   (NOT &NWOPT).NOEND                                       00289303
&ENDG    EQU   *                                                        00289403
.NOEND   AIF   ('&CSECT' NE 'RSECT').NOENDC                             00289503
&SECT    RSECT ,                                                        00289603
         AGO   .DSOPT                                                   00289703
.NOENDC  ANOP  ,                                                        00289803
&SECT    CSECT ,                                                        00289903
.DSOPT   AIF   (&EQUOPT OR &MAPONCE OR &SAV@REG).END                    00290003
&MAPONCE SETB  1                                                        00290103
&SAV@REG SETB  1                                                        00290203
         AIF   (&NOREG).SKPYREG  AVOID JES2 MAPPING CONFLICT    GP04115 00290303
         YREGS ,                                                        00290403
.SKPYREG MASKEQU ,                                               87223  00290503
.END     AIF   ('&SAVE' EQ '*' OR &NOSAVE).MEND                 GP04051 00290603
&SAVTYPE SETC  'OLD'         PREVENT STORAGE RELEASE IN PGMEXIT GP04051 00290703
 MNOTE *,'SAVE IS &SAVE'                                                00290803
 MNOTE *,'SAVTYPE IS &SAVTYPE'                                          00290903
.MEND    MEND  ,                                                GP04051 00291003
./ ADD NAME=PRTCOM   0111-03025-05192-0057-00185-00161-00042-GERHARD 00 00291103
         MACRO                                                          00291203
&NM      PRTCOM &OM,&B0=,&B1=0,&DEV=0,&FUN=,&A0=,&A1=,&OPT=,&A80=OFF    00291303
.*--------------------------------------------------------------------* 00291403
.*                                                                    * 00291503
.*   PRTCOM PROVIDES THE INTERFACE TO THE @PRINTER SERVICE ROUTINE    * 00291603
.*   ARGUMENTS ARE PASSED IN R0, R1, AND ACR0 (PRTF,PRTS)             * 00291703
.*                                                                    * 00291803
.*   IN AM24, THE REGISTER USE IS:                                    * 00291903
.*                                                                    * 00292003
.*   R0:0           R0:1           R0:2           R0:3                * 00292103
.*                                                                    * 00292203
.*   OPT FLAGS      TITLE/FOOTER#  DEVICE MASK    PRT FUNCTION REQ.   * 00292303
.*   (EITHER B0 OR OPT=)                                              * 00292403
.*                                                                    * 00292503
.*   R1:0           R1:1           R1:2           R1:3                * 00292603
.*                                                                    * 00292703
.*   LENGTH/ENDCH   ADDRESS-OF-LIST-OR-PRINT-DATA-ETC.                * 00292803
.*                                                                    * 00292903
.*--------------------------------------------------------------------* 00293003
.*                                                                    * 00293103
.*   IN AM31, THE REGISTER USE IS:                                    * 00293203
.*                                                                    * 00293303
.*   R0:0           R0:1           R0:2           R0:3                * 00293403
.*                                                                    * 00293503
.*   FLAG           TITLE/FOOTER#  DEVICE MASK    PRT FUNCTION REQ.   * 00293603
.*   (EITHER B0 OR OPT=)                                              * 00293703
.*                                                                    * 00293803
.*   R1:0           R1:1           R1:2           R1:3                * 00293903
.*                                                                    * 00294003
.*   ADDRESS-OF-LIST-OR-PRINT-DATA-ETC.---------------                * 00294103
.*                                                                    * 00294203
.*                                                                    * 00294303
.*   ACR0:0         ACR0:1         ACR0:2         ACR0:3              * 00294403
.*                                                                    * 00294503
.*                                                LENGTH/ENDCH        * 00294603
.*                                                                    * 00294703
.*--------------------------------------------------------------------* 00294803
.*                                                                    * 00294903
.*   FLAGS :                                                          * 00295003
.*                                                                    * 00295103
.*     80 - ABEND IF NOT OPENED         (PRTOPEN)                     * 00295203
.*     40 - (DON'T USE)                 (PRTOPEN)                     * 00295303
.*     20 - SUPPRESS WTO IF NOT OPENED  (PRTOPEN)                     * 00295403
.*     10 - ABEND IF DD DUMMY           (PRTOPEN)                     * 00295503
.*     20 - NEW SHEET ON NEXT PAGE EJECT                              * 00295603
.*     04 - THIS RECORD CONTAINS ASA                                  * 00295703
.*     02 - THIS RECORD CONTAINS MACHINE CODE                         * 00295803
.*     01 - NO CONTROL CHARACTER IN RECORD                            * 00295903
.*                                                                    * 00296003
.*   TITLE/FOOTER #:   4 BITS EACH; TOTAL OF EACH (PRTOPEN); ELSE     * 00296103
.*     NUMBER OF TITLE/FOOTER THIS RECORD IS TO BE USED FOR           * 00296203
.*                                                                    * 00296303
.*   DEVICE MASK:  (0 TREATED AS 1)                                   * 00296403
.*                                                                    * 00296503
.*   NUMBER OF DEVICE (1-8) FOR PRTOPEN; R1 POINTS TO PRTWORK/PUNWORK * 00296603
.*   ONE BIT PER DEVICE TO RECEIVE THIS RECORD (E.G. DEV=3 WRITES THE * 00296703
.*     REQUEST TO DEVICES 1 AND 2)                                    * 00296803
.*                                                                    * 00296903
.*   FUNCTION:  INDEX TO REQUESTED FUNCTION:                          * 00297003
.*                                                                    * 00297103
.*   0 - CLOSE      1 - TCLOSE     2 - OPEN       3 - ROOM (COND.SPC) * 00297203
.*   4 - SPACE N    5 - FD LIST    6 - V-RECORD   7 - FIXED REC.      * 00297303
.*   8 - SEPARATE   9 - SNAP      10 - FD ITEM   11 - STRING RECORD   * 00297403
.*  12 - AM31 FREC                                                    * 00297503
.*                                                                    * 00297603
.*--------------------------------------------------------------------* 00297703
         GBLC  &MACPLAB,&PRTMODE                                        00297803
         GBLB  &MVSESA                                          GP04234 00297903
         LCLA  &I,&J,&K,&OPA,&VD,&D(8)                                  00298003
         LCLB  &F01,&F02,&F04,&F08,&F10,&F20,&F40,&F80,&B0Z             00298103
         LCLB  &INDEV                                            81259  00298203
         LCLC  &DC,&LNR,&OP                                      81259  00298303
.*   FOR AM31 SUPPORT, THE A0 FIELD IS NOW PLACED INTO ACCESS REGISTER  00298403
.*   0, BYTE 3                                                          00298503
.*                                                                      00298603
&MACPLAB SETC  '&NM'                                             81259  00298703
&K       SETA  N'&OPT                                            90309  00298803
&B0Z     SETB  ('&B0' EQ '' OR '&B0' EQ '0')  OPTION FLAGS ?            00298903
         AIF   (&B0Z AND &K EQ 0).DEFOPT      NO FLAGS, NO OPTIONS      00299003
         AIF   (&B0Z OR  &K EQ 0).WHATOPT                               00299103
         MNOTE 8,'&OM: BOTH B0 AND OPT SPECIFIED; B0=&B0 IGNORED'       00299203
.WHATOPT AIF   (NOT &B0Z).ITMNOPT    B0 - USE IT                        00299303
&J       SETA  0             COUNT OF PROCESSED OPERANDS                00299403
.ITMLOOP AIF   (&I GE &K).ITMTEST                                90309  00299503
&I       SETA  &I+1                                              90309  00299603
&DC      SETC  '&OPT(&I)'                                               00299703
         AIF   ('&DC' EQ '').ITMLOOP  IGNORE NULLS                      00299803
         AIF   ('&DC' EQ 'WTO').ITMLOOP  IGNORE SEMANTIC NULLS  GP03027 00299903
         AIF   ('&DC' EQ 'DUMMY').ITMLOOP  IGNORE SEMANTIC NULL         00300003
&J       SETA  &J+1                                              90309  00300103
&F80     SETB  (&F80 OR '&DC' EQ 'ABE' OR '&DC' EQ 'LIST')              00300203
&F80     SETB  (&F80 OR '&DC' EQ 'ABEND')                               00300303
&F40     SETB  (&F40 OR '&DC' EQ 'X9700')                               00300403
&F20     SETB  (&F20 OR '&DC' EQ 'SHEET' OR '&DC' EQ 'PAGE')            00300503
&F20     SETB  (&F20 OR '&DC' EQ 'AUX' OR '&DC' EQ 'TRAY2')             00300603
&F20     SETB  (&F20 OR '&DC' EQ 'AUXTRAY' OR '&DC' EQ 'TRAY')          00300703
&F20     SETB  (&F20 OR '&DC' EQ 'NOWTO')                               00300803
&F10     SETB  (&F40 OR '&DC' EQ 'NODUMMY')                             00300903
&F10     SETB  (&F40 OR '&DC' EQ 'ABDUMMY')                             00301003
&F04     SETB  (&F04 OR '&DC' EQ 'ASA')   (DEFAULT)                     00301103
&F02     SETB  (&F02 OR '&DC' EQ 'MC' OR '&DC' EQ 'MCC')                00301203
&F02     SETB  (&F02 OR '&DC' EQ 'SKIPEJE' OR '&DC' EQ 'NOEJE')         00301303
&F01     SETB  (&F01 OR '&DC' EQ 'NO' OR '&DC' EQ 'NOCC')               00301403
&F01     SETB  (&F01 OR '&DC' EQ 'EJECT2' OR '&DC' EQ '2EJECT')         00301503
         AGO   .ITMLOOP                                          90309  00301603
.ITMTEST ANOP  ,                                                        00301703
&OPA     SETA  &F80+&F40+&F20+&F10+&F08+&F04+&F02+&F01                  00301803
         AIF   (&OPA EQ &J).DONOPT  EACH OPERAND VALID ?                00301903
.BADOPT  MNOTE 4,'&OM: ERROR - OPT PARAMETER BAD: &OPT'                 00302003
.DONOPT  ANOP  ,                                                        00302103
&OPA     SETA  &F80*128+&F40+&F20*32+&F10*16+&F08*8+&F04*4+&F02*2+&F01  00302203
&OP      SETC  '&OPA'                                                   00302303
         AGO   .POSTOPT                                         GP99029 00302403
.ITMNOPT ANOP  ,                                                 90309  00302503
&OP      SETC  '&B0'         USE USER'S PASSED VALUE             90309  00302603
         AIF   ('&OP' NE '').POSTOPT                                    00302703
.DEFOPT  ANOP  ,                                                        00302803
&OP      SETC  '0'           MAKE IT NON-BLANK                          00302903
.POSTOPT AIF   ('&DEV' EQ '' OR '&DEV' EQ '0').NODV                     00303003
         AIF   ('&DEV' NE 'ALL' AND '&DEV' NE '255').DVSOM      GP03240 00303103
&VD      SETA  255                                                      00303203
         AGO   .NODV                                                    00303303
.DVSOM   AIF   (K'&DEV LT 2).DVSOL                               81259  00303403
         AIF   ('&DEV'(1,1) NE '=').DVSOL                        81259  00303503
&INDEV   SETB  1             SET INDIRECT DEVICE NUMBER          81259  00303603
         AGO   .NODV                                             81259  00303703
.DVSOL   ANOP  ,                                                 81259  00303803
&I       SETA  0                                                        00303903
&K       SETA  N'&DEV                                                   00304003
.DEVLOOP ANOP  ,                                                        00304103
&I       SETA  &I+1                                                     00304203
         AIF   (&I GT &K).DVEND                                         00304303
         AIF   ('&DEV(&I)' EQ '').DEVLOOP                               00304403
         AIF   ('&DEV(&I)' EQ '0').DEVLOOP                              00304503
         AIF   ('&DEV(&I)' LT '1' OR '&DEV(&I)' GT '8').DVERR           00304603
&D(&DEV(&I)) SETA  1                                                    00304703
         AGO   .DEVLOOP                                                 00304803
.DVERR   MNOTE 8,'*** INVALID DEVICE NUMBER &DEV(&I)'                   00304903
         AGO   .DEVLOOP                                                 00305003
.DVEND   ANOP  ,                                                        00305103
&VD      SETA  128*&D(8)+64*&D(7)+32*&D(6)+16*&D(5)+8*&D(4)             00305203
&VD      SETA  &VD+4*&D(3)+2*&D(2)+&D(1)                                00305303
.NODV    AIF   ('&OP' NE '0' OR '&B1' NE '0').LONG              GP99029 00305403
         AIF   (&VD GT 15).LONG                                         00305503
&J       SETA  &VD*256+&FUN                                             00305603
         MACPARM R0,&J       LOAD DEVICE/FUNCTION INDEX          81259  00305703
         AGO   .POST0                                            81259  00305803
.LONG    ANOP  ,                                                 82326  00305903
&MACPLAB L     R0,=AL1(&OP,&B1+0,&VD,&FUN)                       81259  00306003
&MACPLAB SETC  ''            CANCEL LABEL                        81259  00306103
.POST0   AIF   (NOT &INDEV).LOAD1                                81259  00306203
&VD      SETA  K'&DEV-1                                          81259  00306303
&DC      SETC  '&DEV'(2,&VD)                                     81259  00306403
&MACPLAB ICM   R0,2,&DC                                          81259  00306503
&MACPLAB SETC  ''                                                81259  00306603
.LOAD1   AIF   ('&FUN' EQ '0' OR '&FUN' EQ '1').BAL  CLOSE?             00306703
         MACPARM R1,&A1      LOAD PARAMETER REGISTER                    00306803
         AIF   ('&A80' EQ 'OFF').NOTHIGH                        GP03025 00306903
         O     R1,=X'80000000'  SET LIST BIT (PRTBIG)           GP03025 00307003
.NOTHIGH AIF   (NOT &MVSESA).VER24                              GP04234 00307103
         AIF   (&FUN EQ 11 OR &FUN EQ 12).ACR                   GP03025 00307203
.VER24   AIF   ('&A0' EQ '' OR '&A0' EQ '0').BAL                 90309  00307303
&K       SETA  K'&A0                                            GP05190 00307403
         AIF   (&K LT 3).VER24I                                 GP05190 00307503
         AIF   ('&A0'(1,1) NE '(' OR '&A0'(2,1) EQ '(').VER24I  GP05190 00307603
         AIF   ('&A0'(&K,1) NE ')' OR '&A0'(&K-1,1) EQ ')').VER24I      00307703
         LA    R1,0(,R1)     CLEAR HIGH BYTE                    GP05190 00307803
         MACPARM R14,&A0,OP=LR,OPR=LR                           GP05190 00307903
         SLL   R14,24                                           GP05190 00308003
         OR    R1,R14        INSERT LENGTH                      GP05190 00308103
         AGO   .BAL                                             GP05190 00308203
.VER24I  ICM   R1,8,=AL1(&A0)                                    90309  00308303
         AGO   .BAL                                                     00308403
.ACR     AIF   ('&A0' NE '' AND '&A0' NE '0').ACRLOAD                   00308503
         AIF   (&FUN EQ 11 AND '&A0' EQ '0').ACRLOAD                    00308603
         MNOTE 8,'&OM: REQUIRED LENGTH VALUE MISSING'                   00308703
.ACRLOAD MACPARM R15,&A0                                                00308803
.*NEED(R) N     R15,=X'000000FF'  FOR FUTURE USE                        00308903
         SAR   R0,R15        LOAD INTO ACCESS REGISTER                  00309003
.BAL     AIF   ('&PRTMODE' EQ 'V').VCON                                 00309103
         L     R15,@PRINTER                                             00309203
         AGO   .BALR                                                    00309303
.VCON    L     R15,=V(@PRINTER)                                         00309403
.BALR    BASR  R14,R15                                          GP99020 00309503
         MEND  ,                                                        00309603
./ ADD NAME=PRTL     0100-03025-03025-1534-00024-00024-00000-GERHARD 00 00309703
         MACRO                                                          00309803
&NM      PRTL  &VAD,&DEV=,&TITLE=0,&FOOTER=0,&CC=,&OPT=,&MODE=  GP02301 00309903
.*--------------------------------------------------------------------* 00310003
.*                                                                    * 00310103
.*  PRTL SERVES AS A TEMPORARY ADJUNCT TO PRTF, PENDING CORRECTION    * 00310203
.*  OF PRTF AND PRTS IN 31-BIT ADDRESSING MODE                        * 00310303
.*                                                                    * 00310403
.*  PRTL 'LITERAL STRING' - CONVERTED TO VCON AND INVOKES PRTV        * 00310503
.*                                                                    * 00310603
.*--------------------------------------------------------------------* 00310703
         LCLA  &CT,&I                                                   00310803
         LCLC  &M#                                                      00310903
&M#      SETC  '&SYSNDX'                                                00311003
&NM      PRTV  ZZVC&M#,                                                *00311103
               DEV=&DEV,                                               *00311203
               TITLE=&TITLE,                                           *00311303
               FOOTER=&FOOTER,                                         *00311403
               CC=&CC,                                                 *00311503
               OPT=&OPT,                                               *00311603
               MODE=&MODE                                               00311703
         B     ZZVD&M#                                                  00311803
ZZVC&M#  VCON  &VAD                                                     00311903
ZZVD&M#  DS    0H                                                       00312003
         MEND                                                           00312103
./ ADD NAME=PRTLIST  0100-03025-03025-1534-00011-00011-00000-GERHARD 00 00312203
         MACRO                                                          00312303
&NM      PRTLIST &LAD,&DEV=,&TITLE=0,&FOOTER=0,&OPT=,&MODE=      90309  00312403
         LCLA  &HF                                                      00312503
         AIF   (&TITLE EQ 0 AND &FOOTER EQ 0).NOT                       00312603
         AIF   (&TITLE LT 16 AND &FOOTER LT 16).OKT                     00312703
         MNOTE 4,'*** NON-NUMERIC TITLE/FOOTER NOT SUPPORTED'           00312803
.OKT     ANOP  ,                                                        00312903
&HF      SETA  &TITLE*16+&FOOTER                                        00313003
.NOT     ANOP  ,                                                        00313103
&NM      PRTCOM PRTLIST,FUN=5,B1=&HF,A1=&LAD,DEV=&DEV,OPT=&OPT          00313203
         MEND                                                           00313303
./ ADD NAME=PRTOPEN  0100-03025-03025-1534-00007-00007-00000-GERHARD 00 00313403
         MACRO                                                          00313503
&NM      PRTOPEN &WORK,&DEV=,&OPT=                                      00313603
         LCLA  &I,&J                                                    00313703
         LCLB  &A,&D,&W                                                 00313803
&J       SETA  N'&OPT                                                   00313903
&NM      PRTCOM PRTOPEN,FUN=2,A1=&WORK,DEV=&DEV,OPT=&OPT                00314003
         MEND  ,                                                        00314103
./ ADD NAME=PRTV     0101-03025-04080-2216-00011-00011-00001-GERHARD 00 00314203
         MACRO                                                          00314303
&NM      PRTV  &VAD,&DEV=,&TITLE=0,&FOOTER=0,&CC=,&OPT=,&MODE=   90309  00314403
         LCLA  &CT,&HF                                                  00314503
         AIF   (&TITLE EQ 0 AND &FOOTER EQ 0).NOT                       00314603
         AIF   (&TITLE LT 16 AND &FOOTER LT 16).OKT                     00314703
         MNOTE 4,'*** NON-NUMERIC TITLE/FOOTER NOT SUPPORTED'           00314803
.OKT     ANOP  ,                                                        00314903
&HF      SETA  &TITLE*16+&FOOTER                                        00315003
.NOT     ANOP  ,                                                 90309  00315103
&NM      PRTCOM PRTV,FUN=6,B1=&HF,A1=&VAD,DEV=&DEV,OPT=(&OPT,&CC)       00315203
         MEND                                                           00315303
./ ADD NAME=PRTWORK  0103-03025-08104-1218-00075-00068-00000-GERHARD 00 00315403
         MACRO                                                          00315503
&NM      PRTWORK &DD,&ALTDD,&TITLE=0,&FOOTER=0,&LPP=0,&WIDTH=0,        *00315603
               &FILL=0,&RECFM=0,&PAGE=0,&SPAGE=0,&PGUP=NO,&EXLST=0,    *00315703
               &BUF=                                            GP08088 00315803
         LCLA  &PFG,&I,&J,&K                                     84169  00315903
         LCLB  &B0,&B1,&B2,&B3,&B4,&B5,&B6,&B7,&PFX             GP05120 00316003
         LCLC  &REC                                              81155  00316103
&REC     SETC  '&RECFM'                                          81155  00316203
         AIF   ('&REC' EQ '0').DEFREC                            81155  00316303
&I       SETA  K'&RECFM                                          81155  00316403
         AIF   (&I LT 4).NRECSD                                  81155  00316503
         AIF   ('&RECFM'(&I,1) EQ '''').DEFREC                   81155  00316603
.NRECSD  AIF   (&J GE &I).DONREC                                 81155  00316703
&J       SETA  &J+1                                              81155  00316803
         AIF   ('&REC'(&J,1) EQ 'U').RECU                        81155  00316903
         AIF   ('&REC'(&J,1) EQ 'V').RECV                        81155  00317003
         AIF   ('&REC'(&J,1) EQ 'F').RECF                        81155  00317103
         AIF   ('&REC'(&J,1) EQ 'D').RECD                        81155  00317203
         AIF   ('&REC'(&J,1) EQ 'T').RECT                        81155  00317303
         AIF   ('&REC'(&J,1) EQ 'B').RECB                        81155  00317403
         AIF   ('&REC'(&J,1) EQ 'S').RECS                        81155  00317503
         AIF   ('&REC'(&J,1) EQ 'M').RECM                        81155  00317603
         AIF   ('&REC'(&J,1) EQ 'N').RECN                        81271  00317703
         AIF   ('&REC'(&J,1) NE 'A').DEFREC                      81155  00317803
&B5      SETB  1                                                 81155  00317903
         AGO   .NRECSD                                           81155  00318003
.RECM    ANOP  ,                                                 81155  00318103
&B6      SETB  1                                                 81155  00318203
         AGO   .NRECSD                                           81155  00318303
.RECN    ANOP  ,             SUPPRESS CC DEFAULT IN PRTOPEN      81271  00318403
&B7      SETB  1                                                 81271  00318503
         AGO   .NRECSD                                           81271  00318603
.RECS    ANOP  ,                                                 81155  00318703
&B4      SETB  1                                                 81155  00318803
         AGO   .NRECSD                                           81155  00318903
.RECB    ANOP  ,                                                 81155  00319003
&B3      SETB  1                                                 81155  00319103
         AGO   .NRECSD                                           81155  00319203
.RECD    AIF   (&B0 OR &B1).DEFREC     FAIL VD, ETC.             81155  00319303
.RECT    ANOP  ,                                                 81155  00319403
&B2      SETB  1                                                 81155  00319503
.RECV    ANOP  ,                                                 81155  00319603
&B1      SETB  1                                                 81155  00319703
         AGO   .NRECSD                                           81155  00319803
.RECU    ANOP  ,                                                 81155  00319903
&B1      SETB  1                                                 81155  00320003
.RECF    ANOP  ,                                                 81155  00320103
&B0      SETB  1                                                 81155  00320203
         AGO   .NRECSD                                           81155  00320303
.DONREC  ANOP  ,                                                 81155  00320403
&REC     SETC  'B'''.'&B0&B1&B2&B3&B4&B5&B6&B7'.''''             81155  00320503
.DEFREC  AIF   ('&PAGE' EQ '0').NOPG                                    00320603
&I       SETA  &PAGE                                                    00320703
&PFG     SETA  1             SET PAGE FEED-BACK                         00320803
&PFX     SETB  1             EXPAND PAGE VALUES                 GP05120 00320903
.NOPG    AIF   ('&SPAGE' EQ '0').NOSPG                                  00321003
&J       SETA  &SPAGE                                                   00321103
&PFG     SETA  1             SET PAGE FEED-BACK                         00321203
&PFX     SETB  1             EXPAND PAGE VALUES                 GP05120 00321303
.NOSPG   AIF   ('&PGUP' EQ 'NO').NOPGUP                          84169  00321403
&PFG     SETA  3             SET UPDATING BY USER                84169  00321503
         AIF   ('&PGUP' EQ '' OR '&PGUP' EQ 'YES').NOPGUP        84169  00321603
         MNOTE 4,'INVALID PGUP OPTION : &PGUP'                   84169  00321703
.NOPGUP  AIF   ('&EXLST' EQ '0').NOLST1                                 00321803
&PFG     SETA  &PFG+4        SET EXIT LIST FLAG                  84169  00321903
.NOLST1  AIF   ('&BUF' NE '1').NOBUF1                           GP08088 00322003
&PFG     SETA  &PFG+16       SIGNLE BUFFER                      GP08088 00322103
.NOBUF1  ANOP  ,                                                GP08088 00322203
         DC    0H'0'                                                    00322303
&NM      DC    CL8'&DD ',CL8'&ALTDD ',AL2(&LPP),AL1(&FILL,&WIDTH,&TITLE*00322403
               ,&FOOTER,&REC,&PFG)                                      00322503
         AIF   (NOT &PFX).SKIPPG#                               GP05120 00322603
         DC    Y(&I,&J)      PAGE/SUB-PAGE FEEDBACK AREA                00322703
.SKIPPG# DC    AL4(&EXLST)   EXIT LIST POINTER                   84169  00322803
.MEND    MEND  ,                                                        00322903
./ ADD NAME=RMODE    8000-04314-04314-2101-00004-00004-00000-GERHARD 00 00323003
         MACRO ,                                                        00323103
         RMODE ,                                                        00323203
.*   DUMMY MACRO CREATED TO SUPPORT ASSEMBLY UNDFER HERCULES (XF ASM)   00323303
         MEND  ,                                                        00323403
./ ADD NAME=RSECT    8002-04312-04314-2101-00004-00013-00000-GERHARD 00 00323503
         MACRO ,                                                        00323603
&NM      RSECT ,             HERCULES SUPPORT             ADDED GP04234 00323703
&NM      CSECT ,                                                        00323803
         MEND  ,                                                        00323903
./ ADD NAME=SAVEX    0105-98322-04342-1546-00043-00023-00025-GERHARD 00 00324003
         MACRO ,                                                        00324103
&L       SAVEX &R1,&R3,&LOC,&TYPE=*,&SETAM=,&WORK=R14           GP99018 00324203
         GBLB  &ZZSVBSM,&MVSESA                                         00324303
         LCLC  &NM                                                      00324403
&NM      SETC  '&L'                                                     00324503
.*                                                                      00324603
.*             COPYRIGHT 1981 BY EXPERT SYSTEMS PROGRAMMING INC.        00324703
.*                               347 ORCHARD STREET                     00324803
.*                               VIENNA, VIRGINIA   22180               00324903
.*                                                                      00325003
.*                        ALL RIGHTS RESERVED.                          00325103
.*                                                                      00325203
.*             THIS MACRO IS NOT TO BE DISTRIBUTED WITHOUT PERMISSION,  00325303
.*             AS DESCRIBED IN MEMBER $$RIGHTS.                         00325403
.*                                                                      00325503
.*       CODE ADDED TO PRESERVE CALLER'S AMODE AND OPTIONALLY SET AMODE 00325603
.*                                                              GP99018 00325703
         AIF   ('&TYPE' EQ 'BSM').BSM                           GP98322 00325803
         AIF   ('&TYPE' EQ 'STM').STM                                   00325903
         MNOTE 8,'SAVEX: TYPE=&TYPE UNKNOWN - TYPE=STM ASSUMED'         00326003
         AGO   .STM                                                     00326103
.BSM     ANOP  ,                                                GP98322 00326203
&ZZSVBSM SETB  1             SET FLAG FOR ENDM                          00326303
&NM      BSM   R14,0         GET CALLER'S AMODE                 GP98322 00326403
&NM      SETC  ''            DONE WITH LABEL                            00326503
.STM     ANOP  ,                                                        00326603
&NM      STM   &R1,&R3,&LOC                                             00326703
&NM      SETC  ''            DONE WITH LABEL                            00326803
         AIF   ('&SETAM' EQ '' OR '&SETAM' EQ 'ANY').MEND       GP04234 00326903
         AIF   (NOT &MVSESA).MEND                               GP04234 00327003
         BASR  R14,0                                            GP04050 00327103
         USING *,R14                                            GP04050 00327203
         LA    &WORK,ZZSV&SYSNDX                                GP99018 00327303
         AIF   ('&SETAM' EQ '24' OR '&SETAM' EQ 'AM24').SETCM   GP99018 00327403
         LA    R0,1                                             GP99018 00327503
         SLL   R0,31         MAKE 80000000                      GP99018 00327603
         OR    &WORK,R0                                         GP99018 00327703
         AIF   ('&SETAM' EQ '31' OR '&SETAM' EQ 'AM31').SETCM   GP99018 00327803
   MNOTE 8,'SAVEX: UNSUPPORTED SETAM VALUE: &SETAM - AM31 ASSUMED'      00327903
.SETCM   BSM   0,&WORK       CHANGE TO REQUIRED MODE            GP99018 00328003
         DROP  R14                                              GP04050 00328103
ZZSV&SYSNDX DS 0H                                               GP99018 00328203
.MEND    MEND  ,                                                GP99018 00328303
./ ADD NAME=SERVCALL 0109-99020-12043-1622-00073-00044-00006-GERHARD 00 00328403
         MACRO ,                                                        00328503
&NM      SERVCALL &CODE,&ADDR,&REG2,&ERR=,&CC0=,&CC4=,&CC8=,&LEN=,     *00328603
               &MODE=BAL,&OPT=                                  GP06287 00328703
         GBLC  &SRVCM@R,&MACPLAB                                 81148  00328803
         GBLB  &MVSXA                                           GP04234 00328903
         LCLA  &I,&J,&K,&OPA                                    GP06287 00329003
         LCLB  &F0,&F1,&F2,&F3,&F4,&F5,&F6,&F7                  GP06287 00329103
         LCLC  &LERR,&DC                                         81148  00329203
         AIF   ('&MACPLAB' NE '' AND '&NM' NE '').LABTWO        GP12043 00329303
         AIF   ('&MACPLAB' NE '').LABCOM                        GP12043 00329403
         AGO   .LABSET                                          GP12043 00329503
.LABTWO  MACPARM MODE=LBL    EXPAND LABEL FOR MACPLAB           GP12043 00329603
.LABSET  ANOP  ,                                                GP12043 00329703
&MACPLAB SETC  '&NM'                                            GP12043 00329803
.LABCOM  MACPARM R2,&REG2,NULL=SKIP                              85070  00329903
         MACPARM R1,&ADDR,NULL=SKIP                                     00330003
         AIF   ('&CODE' EQ '').NOR0                                     00330103
         AIF   ('&CODE'(1,1) EQ '(').REG0                               00330203
         MACPARM R0,VEN&CODE                                            00330303
         AIF   ('&LEN' EQ '').NOR0                               81148  00330403
         MACPARM R0,8,=AL1(&LEN),OP=ICM,MODE=THREE               81148  00330503
         AGO   .NOR0                                                    00330603
.REG0    MACPARM R0,&CODE                                               00330703
.NOR0    AIF   (T'&OPT EQ 'O').NOOPT   NO OPTIONS               GP06287 00330803
&K       SETA  N'&OPT                                           GP06287 00330903
&J       SETA  0             COUNT OF PROCESSED OPERANDS        GP06287 00331003
&I       SETA  0             CLEAR INDEX                        GP06287 00331103
.ITMLOOP AIF   (&I GE &K).ITMTEST                               GP06287 00331203
&I       SETA  &I+1                                             GP06287 00331303
&DC      SETC  '&OPT(&I)'                                       GP06287 00331403
         AIF   ('&DC' EQ '').ITMLOOP  IGNORE REAL NULL          GP06287 00331503
         AIF   ('&DC' EQ 'NONE').ITMLOOP  IGNORE SEMANTIC NULL  GP06287 00331603
&J       SETA  &J+1                                             GP06287 00331703
&F0      SETB  (&F0  OR '&DC' EQ 'LIST')                        GP06287 00331803
&F0      SETB  (&F0  OR '&DC' EQ 'TEXT')                        GP06287 00331903
&F6      SETB  (&F6  OR '&DC' EQ '2'  OR '&DC' EQ '3')          GP06287 00332003
&F7      SETB  (&F7  OR '&DC' EQ '1'  OR '&DC' EQ '3')          GP06287 00332103
         AGO   .ITMLOOP                                         GP06287 00332203
.ITMTEST ANOP  ,                                                GP06287 00332303
&OPA     SETA  &F0+&F1+&F2+&F3+&F4+&F5+&F6+&F7                  GP06287 00332403
         AIF   (&OPA EQ &J).DONOPT  EACH OPERAND VALID ?        GP06287 00332503
.BADOPT  MNOTE 4,'SERVCALL: ERROR - OPT PARAMETER BAD: &OPT'    GP06287 00332603
.DONOPT  AIF   (&J EQ 0).NOOPT    SKIP IF ONLY NULLS            GP06287 00332703
         MACPARM R0,4,=AL1(&OPA),OP=ICM,MODE=THREE              GP06287 00332803
.NOOPT   MACPARM R15,@SERVICE,OP=L  GET MODULE ADDRESS                  00332903
         AIF   ('&MODE' EQ 'BAL' OR '&MODE' EQ '').BALMODE       90337  00333003
         AIF   ('&MODE' EQ 'SYNCH').SYNMODE                      90337  00333103
         MNOTE 8,'INVALID MODE=&MODE'                            90337  00333203
.SYNMODE ANOP  ,                                                 90337  00333303
&MACPLAB SYNCH (15),RESTORE=YES  INVOKE AND SAVE MODE            90337  00333403
         AGO   .COMMODE                                          90337  00333503
.BALMODE AIF   (&MVSXA).BASMODE                                 GP04234 00333603
&MACPLAB BALR  R14,R15       CALL THE @SERVICE ROUTINE          GP04234 00333703
         AGO   .COMMODE                                         GP04234 00333803
.BASMODE ANOP  ,                                                 90337  00333903
&MACPLAB BASSM R14,R15       CALL THE @SERVICE ROUTINE                  00334003
.COMMODE AIF   ('&CC0' EQ '' AND '&CC4' EQ '' AND '&CC8' EQ '').NOCC    00334103
         AIF   ('&ERR' EQ '' OR '&ERR' EQ 'NO').NODUPE           81148  00334203
         MNOTE 4,'CC= AND ERR= ARE MUTUALLY EXCLUSIVE'           81148  00334303
.NODUPE  ANOP  ,                                                 81148  00334403
&MACPLAB SETC  ''                                                81148  00334503
         CH    R15,=H'4'     TEST RETURN                         81148  00334603
         MACPARM &CC0,OP=BL,OPR=BLR,MODE=ONE,NULL=SKIP          GP02241 00334703
         MACPARM &CC4,OP=BE,OPR=BER,MODE=ONE,NULL=SKIP          GP02241 00334803
         MACPARM &CC8,OP=BH,OPR=BHR,MODE=ONE,NULL=SKIP          GP02241 00334903
         AGO   .MEND                                             81148  00335003
.NOCC    AIF   ('&ERR' EQ 'NO').MEND                             81148  00335103
&LERR    SETC  '&ERR'                                            81148  00335203
         AIF   ('&LERR' NE '').DOERR                             81148  00335303
&LERR    SETC  '&SRVCM@R'                                        81148  00335403
         AIF   ('&LERR' EQ '').MEND                              81148  00335503
.DOERR   BXH   R15,R15,&LERR  GO TO SET ERROR MESSAGE            81148  00335603
.MEND    MEND  ,                                                        00335703
./ ADD NAME=SERVDEFS 0107-03112-09277-1509-00041-00029-00007-GERHARD 00 00335803
         MACRO ,                                                        00335903
&NM     SERVDEFS &PARM=10                                               00336003
         GBLC  &MACPLAB,&SRVLMOD(20),&SRVLDEL(20)                       00336103
         GBLB  &SRVBMOD(20)                                             00336203
         GBLB  &BUGBEAR,&ZZSPIE                                         00336303
         GBLB  &MVS,&MVSSP,&MVSXA,&MVSESA                               00336403
         GBLA  &SRVNMOD                                                 00336503
.*--------------------------------------------------------------------* 00336603
.*  SERVDEFS IS USED IN THE PROGRAM'S MAIN SAVE AREA TO EXPAND THE    * 00336703
.*  ADDRESS LABELS FOR STANDARD SERVICE ROUTINES (@SERVICE, @PRINTER, * 00336803
.*  ETC.).  WHEN RUNNING IN DEBUG MODE, IT ALSO EXPANDS PGMTRACE AND  * 00336903
.*  DEBTRACE WORK AREAS.                                              * 00337003
.*--------------------------------------------------------------------* 00337103
         LCLA  &I,&J                                                    00337203
&MACPLAB SETC  '&NM'         ENSURE CORRECT VALUE                       00337303
         MACPARM MODE=LABEL                                             00337403
@SERVICE DS    A             ADDRESS OF @SERVICE ROUTINE                00337503
@SERVEXC DS    A             EXECUTED INSTRUCITON (SVC, BASSM, ...)     00337603
@SERVTCA DS    A             ADDRESS OF @SERVICE TASK CONTROL AREA      00337703
.DSLOOP  AIF   (&I GE &SRVNMOD).NDLOOP                                  00337803
&I       SETA  &I+1                                                     00337903
         AIF   (&SRVBMOD(&I)).DSLOOP  SKIP EXPANSION ?                  00338003
&SRVLMOD(&I)  DS  A                                                     00338103
         AGO   .DSLOOP                                                  00338203
.NDLOOP  AIF   (NOT &ZZSPIE).NDSPIE  SKIP IF NOT (E)SPIE MODE   GP09277 00338303
@SPIEDER DS    A  *DEBUG*    @SPIEDER (E)SPIE INTERCEPT         GP09277 00338403
.NDSPIE  AIF   (NOT &BUGBEAR).PARM  SKIP IF NOT DEBUG MODE              00338503
@TRACE   DS    A  *DEBUG*    PGMTRACE ROUTINE                           00338603
    #TRC  DATA    *DEBUG*    PGMTRACE RE-ENTRANT WORK AREA      GP06319 00338703
         AIF   (NOT &MVSXA AND NOT &MVSESA).OLDBUG                      00338803
         DBT   MODE=D  *DEBUG*  DEBTRACE WORK AREA                      00338903
         AGO   .PARM                                                    00339003
.OLDBUG  DBO   MODE=D  *DEBUG*  DEBTROLD WORK AREA                      00339103
.PARM    AIF   ('&PARM' EQ '').MEND                                     00339203
.*DEFER* AIF   (T'&PARM' NE 'N').MEND                                   00339303
         AIF   ('&PARM' EQ '0').MEND                                    00339403
CALLPARM DS    (&PARM)A      PARAMETER LIST FOR SUBCALL, ETC.           00339503
RETCODE  DS    F             PROGRAM RETURN CODE                        00339603
RSNCODE  DS    F             ERROR REASON                               00339703
RR1CODE  DS    F             RETURNED R1                        GP04068 00339803
.MEND    MEND  ,                                                        00339903
./ ADD NAME=SERVFLAG 0101-02265-03075-2228-00149-00146-00003-GERHARD 00 00340003
*        SERVFLAG                                     UPDATED ON 90316  00340103
*          CALL CODES (R0) FOR '@SERVICE' ROUTINE                       00340203
VENMOD1  EQU   256           ENTRY MODIFIER 1                           00340303
VENMOD2  EQU   512           ENTRY MODIFIER 2                           00340403
VENMOD3  EQU   768           ENTRY MODIFIER 3                           00340503
VENMOD4  EQU   1024          ENTRY MODIFIER 4                           00340603
VENMOD5  EQU   1280          ENTRY MODIFIER 5                           00340703
VENMOD6  EQU   1536          ENTRY MODIFIER 6                           00340803
VENMOD7  EQU   1792          ENTRY MODIFIER 7                           00340903
VENMOD8  EQU   2048          ENTRY MODIFIER 8                           00341003
VENMOD9  EQU   2304          ENTRY MODIFIER 9                           00341103
VENMOD10 EQU   2560          ENTRY MODIFIER 10                          00341203
VENMOD11 EQU   2816          ENTRY MODIFIER 11                          00341303
VENMOD12 EQU   3072          ENTRY MODIFIER 12                          00341403
VENMOD13 EQU   3328          ENTRY MODIFIER 13                          00341503
VENMOD14 EQU   3584          ENTRY MODIFIER 14                          00341603
VENMOD15 EQU   3840          ENTRY MODIFIER 15                          00341703
         SPACE 1                                                        00341803
VENCLOSE EQU   00            CLOSE/FREEMAIN ENTRY                       00341903
VENFREEM EQU   00+VENMOD1      CLOSE/FREE - KEEP @PRINTER OPEN          00342003
VENINITG EQU   01            INIT - LOCAL GETMAINS/LOADS/OPENS          00342103
VENLPALD EQU   02            MODULE LOAD FROM LPA, OR STEP/LINKLIB      00342203
VENLPADL EQU   02+VENMOD1      MODULE CLOSE AND DELETE                  00342303
VENLPA@0 EQU   02+VENMOD2      ZERO MODULE ADDRESS IN USERCVT           00342403
VENAPFON EQU   03            AUTH - SET APF ON                          00342503
VENAPFOF EQU   03+VENMOD1      SET APF AUTH OFF                         00342603
VENPASON EQU   03+VENMOD2      SET JSCB PASS ON                         00342703
VENPASOF EQU   03+VENMOD3      SET JSCBPASS OFF                         00342803
VENCANON EQU   03+VENMOD4      SET CSCB CANCEL ON                       00342903
VENCANOF EQU   03+VENMOD5      SET CSCB CANCEL OFF                      00343003
VENNO522 EQU   03+VENMOD6      SET SMF OFF (NO 522)                     00343103
VENDO522 EQU   03+VENMOD7      SET SMF ON (ALLOW 522, ETC.)             00343203
VENUCBUM EQU   05            SEQUENTIAL UCB LOOKUP                      00343303
VENUCBNM EQU   05+VENMOD1      LOCATE UCB BY UNITNAME                   00343403
VENUCBVS EQU   05+VENMOD2      LOCATE UCB BY VOLSER                     00343503
VENUCBDK EQU   05+VENMOD3      LOCATE DISK UCB BY VOLSER                00343603
VENUCBGN EQU   05+VENMOD4      GET GENERIC NAME FOR UCB                 00343703
VENUCBDT EQU   05+VENMOD5      GET UCB TYPE FROM GENERIC                00343803
VENTIOLP EQU   06            TIOT ENTRY LOOP                            00343903
VENTIOLK EQU   06+VENMOD1      TIOT LOOP - SKIP SPECIAL ENTRIES         00344003
VENTIODD EQU   06+VENMOD2      TIOT - LOCATE DDNAME                     00344103
VENTIOUA EQU   06+VENMOD3      TIOT - LOCATE BY UCB ADDRESS             00344203
VENSIOTE EQU   06+VENMOD4      SIOT - LOCATE BY TIOT ADDRESS            00344303
VENSWARL EQU   06+VENMOD5      SWARL - GET SWA (TEXT) FROM SVA TOKEN    00344403
VENSWAAD EQU   06+VENMOD6      SWAAD - GET SWA ADDRESS FROM SVA TOKEN   00344503
VENSWAAB EQU   06+VENMOD7      SWAAB - GET SWA ADDRESS FROM SVA TOKEN   00344603
VENDSABL EQU   06+VENMOD8      DSAB - LOOP THROUGH ENTRIES              00344703
VENDSABD EQU   06+VENMOD9      DSAB - FIND BY DDNAME                    00344803
VENSORTB EQU   07            BUBBLE SORT                                00344903
VENSORTH EQU   07+VENMOD1      HEAP SORT                                00345003
VENBINLK EQU   07+VENMOD3      BINARY TABLE LOOKUP                      00345103
VENDVTBL EQU   08            DEVICE TABLE LOCATE                        00345203
VENDVCAP EQU   08+VENMOD1      DEVICE CAPACITY/BALANCE                  00345303
VENDVEXT EQU   08+VENMOD2      DEVICE EXTENT SIZE CALCULATION           00345403
VENDVSPC EQU   08+VENMOD3      TRK=>CYL; CYL=>TRK CONVERSION            00345503
VENSCHFR EQU   09            SCHEDULE - FREE WORK AREA                  00345603
VENSCHIN EQU   09+VENMOD1      GET/INIT CSA WORK AREA                   00345703
VENSCHMV EQU   09+VENMOD2      MOVE/UPDATE WORK AREA                    00345803
VENSCHED EQU   09+VENMOD3      SCHEDULE AN SRB                          00345903
VENPGFIX EQU   09+VENMOD4      PAGEFIX LPA PAGE                         00346003
VENSWAPY EQU   09+VENMOD5      SET SWAPPABLE                            00346103
VENSWAPN EQU   09+VENMOD6      SET ADDRESS SPACE NONSWAPPABLE           00346203
VENGASID EQU   09+VENMOD7      VALIDATE BY ASID                         00346303
VENGASJB EQU   09+VENMOD8      VALIDATE BY JOBNAME                      00346403
VENGASCB EQU   09+VENMOD9      VALIDATE ASCB ONLY                       00346503
VENSSLOC EQU   09+VENMOD10     LOCATE SUBSYSTEM                         00346603
VENSSSET EQU   09+VENMOD11     SPECIFY SUBSYSTEM                        00346703
VENLOCAT EQU   10            CATALOG LOOKUP                             00346803
VENLOCMT EQU   10+VENMOD1      CAT. LOOK ON P/R AND RSV PACKS           00346903
VENLOCRT EQU   10+VENMOD2      CAT. LOOK ON P/R PACKS ONLY              00347003
VENCATCO EQU   10+VENMOD6      CAT. CONNECT CVOL INDEX                  00347103
VENJESVC EQU   11            JES(2) GENERIC SERVICES :                  00347203
VENJ2INF EQU   11              JES(2) INFO - GET SUBSYSTEM NAME         00347303
VENLOJOB EQU   11+VENMOD1      GET JOB DATA                             00347403
VENMDJOB EQU   11+VENMOD2      RESET HOLD (AND LOCAL) FLAGS             00347503
VENACGET EQU   16            GET CURRENT ACCOUNT/PRIVILEGES             00347603
VENACTST EQU   16+VENMOD1      TEST ACCOUNT NUMBER IN R1                00347703
VENACTSM EQU   16+VENMOD2      TEST AND RETURN ACCOUNT IN R1            00347803
VENACCON EQU   16+VENMOD3      CONVERT INTEGER TO EBCDIC ACCOUNT        00347903
VENACCNX EQU   16+VENMOD4      CONVERT INTEGER ACCOUNT TO EXTERNAL      00348003
VENUSGET EQU   17            GET USER ID                                00348103
VENUSTST EQU   17+VENMOD1      TEST USER ID                             00348203
VENAUTST EQU   18            TEST ACCOUNT/USERID COMBINATION            00348303
VENAUWYL EQU   18+VENMOD1      TEST ACCT/UID COMBINATION FOR WYLBUR     00348403
VENFMTAC EQU   19            CHECK FORMAT, BUT NOT VALIDITY OF ACCT     00348503
VENFMTLB EQU   19+VENMOD1      CHECK FORMAT OF LIBPAK NAME              00348603
VENFMTWY EQU   19+VENMOD2      CHECK FORMAT OF WYLBUR NAME              00348703
VENFMTTS EQU   19+VENMOD3      CHECK FORMAT OF TSO DSN                  00348803
VENGFORM EQU   20            CHECK GDA FORM TABLE                       00348903
VENGPAPR EQU   20+VENMOD1      CHECK GDA PAPER COST TABLE               00349003
VENDSTST EQU   32            CHECK DSN (NON-CATLG)                      00349103
VENDSCAT EQU   32+VENMOD1      CHECK DSN FROM CATALOG (GDG)             00349203
VENDSABB EQU   32+VENMOD2      EXTRACT 8-BYTE PORTION FROM DSN          00349303
VENDSDS1 EQU   32+VENMOD3      OBTAIN FORMAT 1 DSCB                     00349403
VENDSDJ1 EQU   32+VENMOD4      OBTAIN FORMAT 1 DSCB FROM JFCB           00349503
VENDSFMT EQU   32+VENMOD5      FORMAT DSORG/RECFM/OPTCD/BLKL/LRECL      00349603
VENDSMEM EQU   32+VENMOD6      CHECK MEMBER NAME                        00349703
VENRJFCB EQU   32+VENMOD7      GET JFCB FOR DDNAME                      00349803
VENPDSDE EQU   32+VENMOD8      DECODE PDS DIRECTORY ENTRY               00349903
VENDSDS4 EQU   32+VENMOD9      OBTAIN FORMAT 4 DSCB                     00350003
VENDSDJ4 EQU   32+VENMOD10     OBTAIN FORMAT 4 DSCB                     00350103
VENDDCLR EQU   32+VENMOD11     RE-INITIALIZE DD HAVING DISP=MOD         00350203
VENDSLIB EQU   33            CHECK DSN ON LIBPAK                        00350303
VENDSLIX EQU   33+VENMOD1      CHECK LIBPAK INDEX                       00350403
VENDSWYL EQU   34            CHECK WYLBUR DSN                           00350503
VENWYLDX EQU   34+VENMOD1      CHECK FOR WYLBUR INDEX                   00350603
VENDSGET EQU   34+VENMOD2      CHANGE SHORT TO LONG WYLBUR NAME         00350703
VENDSWYC EQU   34+VENMOD3      CHECK WYLBUR DSN IN CATALOG              00350803
VENDSTSO EQU   35            CHECK TSO DSNAME                           00350903
VENDSTSX EQU   35+VENMOD1      CHECK TSO INDEX                          00351003
VENDSTET EQU   35+VENMOD2      CHANGE SHORT TO LONG TSO NAME            00351103
VENVSNFG EQU   36            CHECK VOLUME ATTRIBUTE FLAGS               00351203
VENDSANY EQU   36+VENMOD1      CHECK VOLUME/DSNAME FOR VALIDITY         00351303
VENVSTMS EQU   36+VENMOD2      CHECK VS FOR TMS ELIGIBILITY             00351403
VENWCOMP EQU   37            WYLBUR COMPRESS ROUTINE                    00351503
VENWDCOM EQU   37+VENMOD1      WYLBUR DECOMPRESS ROUTINE                00351603
VENALCVS EQU   38            ALLOCATION - GET DDNAME FOR VTOC OPEN      00351703
VENALCDS EQU   38+VENMOD1      ALLOCATE A (PERM) DSN FROM JFCB          00351803
VENALCFR EQU   38+VENMOD2      RELEASE ALLOCATED TIOT ENTRY             00351903
VENALCDD EQU   38+VENMOD3      ALLOCATE DD FOR DSN                      00352003
VENALCFD EQU   38+VENMOD4      FREE DD                                  00352103
VENWYLOC EQU   39            WYLBUR MULTI-VOLUME LOCATE                 00352203
         SPACE 1                                                        00352303
VAASTC   EQU   X'80'    ACCOUNT PRIVILEGES - INSTALLATION DEFAULT       00352403
VAASYS   EQU   X'40'         SYSTEM PRIVILEGES                          00352503
VAASUP   EQU   X'20'         TECH SUPPORT                               00352603
VAAINH   EQU   X'10'         IN-HOUSE STAFF                             00352703
VAAUSER  EQU   X'08'         PLAIN OLD USER                             00352803
VAAOHD   EQU   X'04'         OVERHEAD ACCOUNT (WITH STC,SYS,SUP)        00352903
         SPACE 1                                                        00353003
VRPGER   EQU   0        RETURN VALUES : DISASTROUS ERROR                00353103
VRPARM   EQU   1             BAD PARM OR ENTRY                          00353203
VRSYNT   EQU   2             BAD CHARACTER OR SYNTAX ERROR              00353303
VRACCT   EQU   3             BAD ACCOUNT                                00353403
         SPACE 1                                                        00353503
VRNTOS   EQU   4             NON-OS DSN                                 00353603
VRNWYL   EQU   5             NOT LIBPAK/WYLBUR NAME                     00353703
VRDLEN   EQU   6             TOO FEW INDEX LEVELS                       00353803
VRDLON   EQU   7             TOO MANY INDEX LEVELS                      00353903
VRNWYX   EQU   8             INVALID SPECIAL (WYLBUR) INDEX             00354003
VRNPSW   EQU   9             NO PASSWORD ENTRY FOR WYLBUR USER          00354103
         SPACE 1                                                        00354203
VCMPARM  EQU   1   WCOMP/WDCOM:  INVALID PARM OR OPTION LIST            00354303
VCMNEDIT EQU   2             BLOCK NOT IN EDIT FORMAT                   00354403
VCMBKLEN EQU   3             INVALID BLOCK LENGTH                       00354503
VCMRCLEN EQU   4             INVALID/TRUNCATED RECORD LENGTH            00354603
VCMSEQ#  EQU   5             INVALID SEQUENCE # OR OVERFLOW             00354703
VCMSEQSQ EQU   6             LINE NUMBER OUT OF SEQUENCE                00354803
         SPACE 1                                                        00354903
./ ADD NAME=SERVINIT 0108-99124-09179-1137-00062-00044-00057-GERHARD 00 00355003
         MACRO ,                                                        00355103
&NM    SERVINIT &LPA=YES,&MAP=YES,&ERR=,&LIST=NO,&AMODE=*,       81167 *00355203
               &MODE=NEW                                        GP03129 00355303
         GBLA  &SVC@SVC      SVC NUMBER OF @SERVICE              83100  00355403
         GBLB  &MVSESA                                          GP04234 00355503
         GBLB  &SRVCM@P,&SRV#NUT                                        00355603
         GBLC  &PRTMAC,&SRVCM@R,&MACPLAB                                00355703
         AIF   ('&MODE' EQ 'NEW' AND &MVSESA).CALLSUB           GP04234 00355803
&NM      MACPARM R15,15,@SERVICE,MODE=THREE,OP=ICM,OPR=ICM              00355903
         BNZ   ZZZZ&SYSNDX+4                                            00356003
         AIF   ('&LPA' NE 'YES').NOLPA                                  00356103
         AIF   (&SVC@SVC EQ 0).DOLPA                             83100  00356203
         SR    R0,R0         REQUEST GETMAIN/INITIALIZATION      83100  00356303
         SVC   &SVC@SVC      CALL IT                             83100  00356403
         AGO   .COMMON                                           83100  00356503
.DOLPA   LPALOOK EP=@SERVICE,DCB=4                              GP03262 00356603
         AGO   .COMMON                                                  00356703
.NOLPA   AIF   ('&LPA' NE 'LINK').DOLOAD                        GP09179 00356803
         L     R0,=V(@SERVICE)    LINK IN                       GP09179 00356903
         AGO   .COMMON                                          GP09179 00357003
.DOLOAD  LOAD  EP=@SERVICE                                              00357103
.COMMON  AIF   ('&AMODE' EQ '*' AND &MVSESA).BSM                GP04234 00357203
ZZZZ&SYSNDX ST R0,@SERVICE                                              00357303
         AIF   (NOT &MVSESA).COMSET                             GP04234 00357403
         AIF   ('&AMODE' EQ '31').AM31                          GP99124 00357503
         AIF   ('&AMODE' EQ '24').AM24                          GP99124 00357603
         MNOTE 4,'UNDEFINED AMODE=&AMODE - AM24 ASSUMED'        GP99124 00357703
.AM24    MVI   @SERVICE,0    FORCE LOW                          GP99124 00357803
         AGO   .COMSET                                          GP99124 00357903
.AM31    OI    @SERVICE,X'80'   SET AM31 ON BASSM INVOCATION    GP99124 00358003
         AGO   .COMSET                                          GP99124 00358103
.BSM     ANOP  ,                                                GP99124 00358203
ZZZZ&SYSNDX LR R15,R0        COPY ADDRESS                       GP99124 00358303
         BSM   R15,0         IMPART CURRENT MODE                GP99124 00358403
         ST    R15,@SERVICE  AND STASH IT                       GP99124 00358503
         AGO   .COMSET                                                  00358603
.*--------------------------------------------------------------------* 00358703
.*  NEW INTERFACE FOR EXTERNAL INITIALIZATION ROUTINE SUBSERV         * 00358803
.*--------------------------------------------------------------------* 00358903
.*                                                                      00359003
.CALLSUB ANOP  ,                                                GP03129 00359103
&NM      MACPARM R0,(R0),MODE=EVEN,OP=SR,OPR=SR                 GP03129 00359203
         MACPARM R1,@SERVICE   LOCATE THE SERVDEFS AREA         GP03129 00359303
         L     R15,=V(SUBSERV)  CALL INITIALIZATION ROUTINE     GP03129 00359403
         BASR  R14,R15       CALL IT                            GP03129 00359503
&SRV#NUT SETB  1             USE NEW INTERFACE                  GP03129 00359603
.COMSET  AIF   ('&ERR' EQ '').NOERR                              81148  00359703
&SRVCM@R SETC  ''                                                81148  00359803
         AIF   ('&ERR' EQ 'NO').NOERR                            81148  00359903
&SRVCM@R SETC  '&ERR'                                            81148  00360003
.NOERR   AIF   ('&MAP' EQ 'NO').MEND                                    00360103
         AIF   (&SRVCM@P).MEND                                          00360203
&SRVCM@P SETB  1                                                        00360303
         PUSH  PRINT                                                    00360403
         AIF   ('&LIST' NE 'NO').DOLIST                          81167  00360503
         PRINT OFF                                               81167  00360603
         AGO   .CMLIST                                           81167  00360703
.DOLIST  PRINT ON,GEN                                            81167  00360803
.CMLIST  SPACE 1                                                 81167  00360903
         COPY  SERVFLAG                                                 00361003
         POP   PRINT                                                    00361103
.MEND    MEND  ,                                                        00361203
./ ADD NAME=SERVLOAD 0107-03078-12154-1542-00097-00026-00022-GERHARD 00 00361303
         MACRO ,                                                        00361403
&NM      SERVLOAD &NAME1,&NAME2,&LFETCH=NO                      GP03246 00361503
.*--------------------------------------------------------------------* 00361603
.*  SERVLOAD INVOKES THE @SERVICE ROUTINE TO LOAD AND STORE MODULES   * 00361703
.*  USING STANDARD CONVENTIONS (E.G., @INPREAD, @PRINTER)             * 00361803
.*  MODULE NAME IS THE SAME AS THE ADDRESS {I.E., @INPREAD DC A(0)}   * 00361903
.*  UNLESS A SECOND PARAMETER IS SPECIFIED {E.G., (@INPREAD,READER) } * 00362003
.*    A THIRD SUBPARAMETER OF N MAY BE SPECIFIED TO INHIBIT EXPANSION * 00362103
.*  OF A DS BY SERVDEFS                                               * 00362203
.*                                                                    * 00362303
.*  2006-06-28  GYP  ADDED LFETCH VALUE LINK. EXPANDS V-CONSTANT TO   * 00362403
.*                   FORCE LINKER TO INCLUDE MODULE STATICALLY.       * 00362503
.*  2003-09-03  GYP  ADDED LFETCH KEYWORD. LFETCH=NO USES SERVCALL    * 00362603
.*                   LPA LOAD (OR PLAIN LOAD IF NOT IN LP); LFETCH=Y  * 00362703
.*                   USES LOAD; LFETCH=DFLT USES SERVCALL UNLESS      * 00362803
.*                   THE DEBUG SWITCH IS SET, THEN IT USES LOAD.      * 00362903
.*--------------------------------------------------------------------* 00363003
         GBLC  &MACPLAB                                                 00363103
         GBLC  &SRVLMOD(20),&SRVLDEL(20)                                00363203
         GBLB  &SRVBMOD(20),&BUGBEAR                            GP03246 00363303
         GBLA  &SRVNMOD                                                 00363403
         LCLC  &CL,&CM                                                  00363503
         LCLB  &USELOAD                                         GP03246 00363603
         LCLA  &I,&J,&K,&N                                              00363703
&N       SETA  N'&SYSLIST                                               00363803
&MACPLAB SETC  '&NM'                                                    00363903
         AIF   ('&LFETCH' EQ '').DEFLOAD                        GP03246 00364003
         AIF   ('&LFETCH'(1,1) EQ 'Y').SETLOAD                  GP03246 00364103
         AIF   ('&LFETCH' EQ 'LINK').SETLOAD                    GP09179 00364203
         AIF   ('&LFETCH'(1,1) EQ 'N').SVCLOAD                  GP03246 00364303
         AIF   ('&LFETCH' EQ 'DFLT').DEFLOAD                    GP09179 00364403
         MNOTE 4,'SERVLOAD: LFETCH=&LFETCH UNSUPPORTED; USING DFLT'     00364503
.DEFLOAD AIF   (NOT &BUGBEAR).SVCLOAD                           GP03246 00364603
.SETLOAD ANOP  ,                                                GP03246 00364703
&USELOAD SETB  1             USE LOAD RATHER THAN SERVCALL LPALD        00364803
.SVCLOAD AIF   (&N LT 1).OOPS                                   GP03246 00364903
.MEMLOOP AIF   (&I GE &N).TEST                                          00365003
&I       SETA  &I+1                                                     00365103
&SRVBMOD(&SRVNMOD+1) SETB 0    JUST IN CASE                             00365203
.*--------------------------------------------------------------------* 00365303
.*  SUBOPERAND OF FORM (MOD-NAME,DC-NAME)                             * 00365403
.*--------------------------------------------------------------------* 00365503
         AIF   (N'&SYSLIST(&I) EQ 1).TRYONE                             00365603
         AIF   ('&SYSLIST(&I,1)' EQ '').MEMLOOP                         00365703
&CL      SETC  '&SYSLIST(&I,1)'                                         00365803
&CM      SETC  '&SYSLIST(&I,1)'                                         00365903
         AIF   ('&SYSLIST(&I,2)' EQ '').SEE3SUB                         00366003
&CM      SETC  '&SYSLIST(&I,2)'                                         00366103
         AGO   .SEE3                                                    00366203
.SEE3SUB AIF   (K'&CL LT 4).SEE3                                        00366303
&K       SETA  K'&CM                                                    00366403
         AIF   ('&CL'(1,3) NE 'SUB').SEE3                               00366503
&CM      SETC  '&CM'(4,&K-3)                                    GP12154 00366603
&CM      SETC  '@UB'.'&CM.'      '                              GP12154 00366703
&CM      SETC  '&CM'(1,8)                                               00366803
.SEE3    AIF   (N'&SYSLIST(&I) LT 3).DONTWO                             00366903
         AIF   ('&SYSLIST(&I,3)' NE 'N' AND '&SYSLIST(&I,3)' NE 'NO'   *00367003
               AND '&SYSLIST(&I,3)' NE '''N''').DONTWO                  00367103
&SRVBMOD(&SRVNMOD+1) SETB 1    INHIBIT DS/DC EXPANSION                  00367203
         AGO   .DONTWO                                                  00367303
.*--------------------------------------------------------------------* 00367403
.*  SUBOPERAND OF FORM MOD-NAME - SAVE IN SAME NAME UNLESS SUB----    * 00367503
.*--------------------------------------------------------------------* 00367603
.TRYONE  ANOP  ,                                                        00367703
&CL      SETC  '&SYSLIST(&I)'                                           00367803
&CM      SETC  '&SYSLIST(&I)'                                           00367903
         AIF   (K'&CL LT 4).DONTWO                                      00368003
         AIF   ('&CL'(1,3) NE 'SUB').DONTWO                             00368103
&K       SETA  K'&CM                                                    00368203
&CM      SETC  '&CM'(4,&K-3)                                    GP12154 00368303
&CM      SETC  '@UB'.'&CM.'      '                              GP12154 00368403
&CM      SETC  '&CM'(1,8)                                               00368503
.DONTWO  AIF   ('&CL' EQ '').MEMLOOP                                    00368603
&J       SETA  &J+1                                                     00368703
         AIF   (&USELOAD).DOLOAD                                GP03246 00368803
.DOSVC   ANOP  ,                                                GP05013 00368903
         SERVCALL LPALD,=CL8'&CL '                              GP05013 00369003
         AGO   .SV8COM                                          GP03246 00369103
.DOLOAD  AIF   ('&LFETCH' NE 'LINK').SV8LOAD                    GP09179 00369203
&MACPLAB L     R0,=V(&CL)    LINK MODULE                        GP09179 00369303
         AGO   .SV8COM                                          GP09179 00369403
.SV8LOAD ANOP  ,                                                GP09179 00369503
&MACPLAB LOAD  0,EPLOC==CL8'&CL '                               GP03250 00369603
.SV8COM  ANOP  ,                                                GP09179 00369703
&MACPLAB SETC  ''                                               GP03250 00369803
         ST    R0,&CM                                           GP03246 00369903
.*--------------------------------------------------------------------* 00370003
.*  REMEMBER DS NAME FOR SAVE AREA; IF DS DIFFERENT, REMEMBER DELETE  * 00370103
.*--------------------------------------------------------------------* 00370203
&SRVNMOD SETA  &SRVNMOD+1                                               00370303
&SRVLMOD(&SRVNMOD) SETC  '&CM'                                          00370403
&SRVLDEL(&SRVNMOD) SETC  '&CL'                                          00370503
         AGO   .MEMLOOP                                                 00370603
.TEST    AIF   (&J GT 0).GOODBYE                                        00370703
.OOPS    MNOTE 0,'SERVLOAD - NO USABLE MODULE NAMES SPECIFIED'          00370803
         MACPARM MODE=LBL                                               00370903
.GOODBYE MEND  ,                                                        00371003
./ ADD NAME=SERVTERM 0107-99020-06263-0041-00038-00009-00034-GERHARD 00 00371103
         MACRO ,                                                        00371203
&NM    SERVTERM &DELETE=YES                             ADDED ON 81148  00371303
         GBLC  &MACPLAB                                                 00371403
         GBLC  &SRVLMOD(20),&SRVLDEL(20)                        GP03258 00371503
         GBLB  &MVSXA                                           GP04234 00371603
         GBLA  &SRVNMOD                                         GP03258 00371703
.*--------------------------------------------------------------------* 00371803
.*  SERVTERM OPTIONALLY FREES MODULES LOADED BY SERVLOAD (W/EXPLICIT  * 00371903
.*    SECOND NAME).                                                   * 00372003
.*  SERVTERM CALLS @SERVICE TO CLOSE AND FREE KNOWN WORK AREAS AND    * 00372103
.*    MODULES                                                         * 00372203
.*  SERVTERM FREES AND CLEARS THE @SERVICE POINTER                    * 00372303
.*--------------------------------------------------------------------* 00372403
         LCLA  &I,&J                                            GP03258 00372503
         LCLC  &X                                               GP03258 00372603
&X       SETC  '&SYSNDX'                                        GP03258 00372703
&NM      MACPARM R15,15,@SERVICE,OP=ICM,MODE=THREE                      00372803
         BZ    ZZZZ&SYSNDX                                              00372903
         SR    R0,R0                                                    00373003
         AIF   (&MVSXA).BASSM                                   GP04234 00373103
         BALR  R14,R15       CLOSE/FREE                         GP04234 00373203
         AGO   .DELETE                                          GP04234 00373303
.BASSM   BASSM R14,R15       CLOSE/FREE                                 00373403
.DELETE  DELETE EP=@SERVICE                                             00373503
ZZZZ&SYSNDX XC @SERVICE,@SERVICE                                        00373603
         AIF   ('&DELETE' NE 'YES').SKIPDEL                     GP03258 00373703
.DELLOOP AIF   (&I GE &SRVNMOD).SKIPDEL                         GP03258 00373803
&I       SETA  &I+1                                             GP03258 00373903
         AIF   ('&SRVLMOD(&I)' EQ '' OR '&SRVLDEL(&I)' EQ '').DELLOOP   00374003
         MACPARM R15,15,&SRVLMOD(&I),OP=ICM,MODE=THREE          GP03258 00374103
&J       SETA  &J+1                                             GP03258 00374203
         BZ    ZZ&X.D&J                                         GP03258 00374303
&MACPLAB SETC  'ZZ&X.D'.'&J'                                    GP03258 00374403
         DELETE EPLOC==CL8'&SRVLDEL(&I) '                       GP03258 00374503
         XC    &SRVLMOD(&I).(4),&SRVLMOD(&I)                    GP03258 00374603
         AGO   .DELLOOP                                         GP03258 00374703
.SKIPDEL MACPARM MODE=LBL    EXPAND FINAL LABEL                 GP03258 00374803
         MEND  ,                                                        00374903
./ ADD NAME=STORAGE  8021-04308-12043-1629-00072-00018-00000-GERHARD 00 00375003
         MACRO ,                                                        00375103
&NM      STORAGE &FUN,&LENGTH=,&ADDR=,&SP=,&BNDRY=,&LOC=,&COND=,       *00375203
               &CALLRKY=,&RELEASE=                                      00375303
.*                                                                      00375403
.*    BACKWARD COMPATIBILITY FOR MVS 3.8 UNDER HERCULES         GP04234 00375503
.*    ALLOW MOST OPERANDS USING GETMAIN/FREEMAIN                        00375603
.*                                                                      00375703
         LCLA  &K,&RK                                                   00375803
         LCLC  &SB                                              GP08258 00375903
         AIF   ('&SP' EQ '0').NOPOOL  TREAT AS SP=              GP08258 00376003
&SB      SETC  '&SP'                                            GP08258 00376103
.NOPOOL  ANOP  ,                                                GP08258 00376203
&K       SETA  K'&SB                                                    00376303
&RK      SETA  K'&LENGTH                                                00376403
         AIF   ('&FUN' EQ 'OBTAIN').GET                                 00376503
         AIF   ('&FUN' EQ 'RELEASE').FREE                               00376603
         MNOTE 8,'STORAGE: FUNCTION &FUN INVALID'                       00376703
         MEXIT ,                                                        00376803
.GET     AIF   ('&COND' EQ 'YES').GETC                                  00376903
         AIF   ('&BNDRY' NE '' OR '&SB' NE '').GETU                     00377003
&NM      GETMAIN R,A=&ADDR,LV=&LENGTH                                   00377103
         MEXIT ,                                                        00377203
.*                                                                      00377303
.GETC    AIF   ('&SB' EQ '' OR &K LT 3).GETCB                           00377403
         AIF   ('&SB'(1,1) EQ '(' AND '&SB'(2,1) NE '(' AND            *00377503
               '&SB'(&K,1) EQ ')' AND '&SB'(&K-1,1) NE ')').GETCR       00377603
.GETCB   ANOP  ,                                                        00377703
&NM      GETMAIN RC,A=&ADDR,LV=&LENGTH,BNDRY=&BNDRY,SP=&SB              00377803
         MEXIT ,                                                        00377903
.GETCR   ANOP  ,                                                        00378003
&NM      MACPARM R0,&LENGTH                                             00378103
         MACPARM R15,&SB     GET SUBPOOL                        GP08089 00378203
         GETMAIN RC,A=&ADDR,LV=(0),SP=(15),BNDRY=&BNDRY         GP08089 00378303
         MEXIT ,                                                        00378403
.*                                                                      00378503
.GETU    AIF   ('&SB' EQ '' OR &K LT 3).GETUB                           00378603
         AIF   ('&SB'(1,1) EQ '(' AND '&SB'(2,1) NE '(' AND            *00378703
               '&SB'(&K,1) EQ ')' AND '&SB'(&K-1,1) NE ')').GETUR       00378803
.GETUB   ANOP  ,                                                        00378903
&NM      GETMAIN RU,A=&ADDR,LV=&LENGTH,BNDRY=&BNDRY,SP=&SB              00379003
         MEXIT ,                                                        00379103
.GETUR   ANOP  ,                                                        00379203
&NM      MACPARM R0,&LENGTH                                             00379303
         MACPARM R15,&SB     GET SUBPOOL                        GP08089 00379403
         GETMAIN RU,A=&ADDR,LV=(0),SP=(15),BNDRY=&BNDRY         GP08089 00379503
         MEXIT ,                                                        00379603
.*                                                                      00379703
.FREE   AIF   ('&SB' NE '' AND '&LENGTH' EQ '' AND '&ADDR' EQ '').FPOOL 00379803
         AIF   ('&SB' NE '').FREESP                                     00379903
&NM      FREEMAIN R,A=&ADDR,LV=&LENGTH                                  00380003
         MEXIT ,                                                        00380103
.FREESP  AIF   ('&LENGTH' EQ '' OR &RK LT 3).FREESR                     00380203
         AIF   ('&LENGTH'(1,1) EQ '(' AND '&LENGTH'(2,1) NE '(' AND    *00380303
               '&LENGTH'(&RK,1) EQ ')' AND                             *00380403
               '&LENGTH'(&RK-1,1) NE ')').FRUR                          00380503
.FREESR  AIF   ('&SB' EQ '' OR &K LT 3).FREEUB                          00380603
         AIF   ('&SB'(1,1) EQ '(' AND '&SB'(2,1) NE '(' AND            *00380703
               '&SB'(&K,1) EQ ')' AND '&SB'(&K-1,1) NE ')').FRUR        00380803
.FREEUB  ANOP  ,                                                        00380903
&NM      FREEMAIN R,A=&ADDR,LV=&LENGTH,SP=&SB                           00381003
         MEXIT ,                                                        00381103
.FRUR  ANOP  ,                                                          00381203
&NM      MACPARM R0,&LENGTH                                             00381303
         MACPARM R0,8(R13),OP=ST      SAVE LENGTH                       00381403
         MACPARM R0,&SB                                                 00381503
         MACPARM R0,8(R13),OP=STC    COMBINE WITH SUBPOOL               00381603
         MACPARM R0,8(R13),OP=L      AND RELOAD                 GP08251 00381703
         FREEMAIN R,A=&ADDR,LV=(0)  LV=&LENGTH,SP=&SB                   00381803
         MEXIT ,                                                        00381903
.FPOOL   ANOP  ,                                                        00382003
&NM      FREEMAIN R,SP=&SB   FREE ENTIRE SUBPOOL                        00382103
.MEND    MEND  ,                                                        00382203
./ ADD NAME=SUBCALL  0113-99020-12162-0116-00104-00016-00092-GERHARD 00 00382303
         MACRO ,                                                 88150  00382403
&NM      SUBCALL &NAME,&PARM,&VL,&MF=S,&OP=BALR,&MODE=L     ADDED 88150 00382503
         GBLC  &MACPLAB                                          88150  00382603
         GBLC  &SRVLMOD(20),&SRVLDEL(20)                        GP03150 00382703
         GBLB  &MVSESA,&OS390,&Z900                             GP08076 00382803
         GBLA  &SRVNMOD                                         GP03150 00382903
         LCLA  &I,&J,&N                                                 00383003
         LCLC  &LBL,&M,&LOP                                     GP08076 00383103
         AIF   ('&MF' EQ 'L').MFL                                       00383203
&LOP     SETC  '&OP'                                            GP08076 00383303
.*--------------------------------------------------------------------* 00383403
.*  NON-STANDARD HANDLING OF PARAMETER LIST:                          * 00383503
.*                                                                    * 00383603
.*  &N IS 1 - GENERATE SIMPLE LA UNLESS PARENTHESIZED (BACKWARD COMP) * 00383703
.*  &N IS 2 OR MORE - NORMAL PARAMETER LIST                           * 00383803
.*--------------------------------------------------------------------* 00383903
&MACPLAB SETC  '&NM'                                             88150  00384003
&N       SETA  N'&PARM                                          GP03041 00384103
         AIF   (&MVSESA OR &OS390 OR &Z900).OKBASS              GP08076 00384203
         AIF   ('&OP' EQ 'BSM').FLAKY                           GP12162 00384303
         AIF   ('&OP' NE 'BASSM' AND '&LOP' NE 'BASR').OKBASS   GP12162 00384403
.FLAKY   ANOP  ,                                                GP12162 00384503
&LOP     SETC  'BALR'        DON'T HAVE BASSM ON 360/370        GP08076 00384603
.OKBASS  AIF   (&N LT 1).LAPARM                                 GP03041 00384703
         AIF   (&N GT 1).NOTONE                                 GP03041 00384803
         AIF   ('&PARM'(1,1) EQ '(' AND '&PARM'(1,2) NE '(').NOTONE     00384903
         AIF   ('&PARM'(1,2) NE '((' OR                                *00385003
               '&PARM'(K'&PARM-1,2) NE '))').LAPARM             GP03041 00385103
.NOTONE  AIF   ('&MF(1)' EQ 'E').NODC                           GP03041 00385203
*TEST*   CNOP  0,4           WORD ALIGN FOR PARM LIST           GP03041 00385303
         MACPARM *+(&N+1)*4,OP=B,MODE=ONE                       GP03041 00385403
&M       SETC  '&SYSNDX'                                        GP03041 00385503
ZZ&M.P   DC    &N.AL4(0)     INLINE, NONREFRESHABLE PARM LIST   GP03041 00385603
&LBL     SETC  'ZZ'.'&M'.'P'                                    GP03041 00385703
         AGO   .HAVEDC                                          GP03041 00385803
.NODC    ANOP  ,                                                GP03041 00385903
&LBL     SETC  '&MF(2)'                                         GP03041 00386003
.HAVEDC  MACPARM R1,&LBL     LOAD PARM LIST                     GP03041 00386103
&I       SETA  0             JUST IN CASE                       GP03041 00386203
         AIF   ((&N+1) EQ K'&PARM).GETADDR  PARM LIST IS NULL   GP03041 00386303
&M       SETC  'R1'          ABNORMAL PARM LIST POINTER         GP03041 00386403
&J       SETA  0             PARM OFFSET FOR NEXT ITEM          GP03041 00386503
.PRMLOOP AIF   (&I GE &N).GETADDR    DONE                       GP03041 00386603
&I       SETA  &I+1                                             GP03041 00386703
         AIF   ('&PARM(&I)' EQ '').NOLST                        GP03041 00386803
         AIF   ('&PARM(&I)'(1,1) NE '''').NOCHAR                GP03041 00386903
         MACPARM R0,=C&PARM(&I)                                 GP03041 00387003
         AGO   .HAVER0                                          GP03041 00387103
.NOCHAR  MACPARM R0,&PARM(&I)  LOAD USER'S ADDRESS              GP03041 00387203
.HAVER0  MACPARM R0,&J.(,&M),OP=ST   PLACE INTO PARAMETER LIST  GP03041 00387303
.NOLST   AIF   (&I NE &N).PRMBUMP                               GP03041 00387403
         AIF   ('&VL' NE 'VL').GETADDR                          GP03041 00387503
         OI    &J.(&M),X'80'  END LIST BIT                      GP03041 00387603
         AGO   .GETADDR                                         GP03041 00387703
.PRMBUMP ANOP  ,                                                GP03041 00387803
&J       SETA  &J+4          NEXT LIST OFFSET                   GP03041 00387903
         AGO   .PRMLOOP                                         GP03041 00388003
.*                                                              GP03041 00388103
.LAPARM  MACPARM R1,&PARM,NULL=SKIP                             GP02241 00388203
.*                                                                      00388303
.*--------------------------------------------------------------------* 00388403
.*  HAVE PARM ADDRESS IN R1; NOW GET ROUTINE ADDRESS IN R15 AND GO    * 00388503
.*--------------------------------------------------------------------* 00388603
.GETADDR AIF   ('&NAME' EQ '(15)' OR '&NAME' EQ '(R15)').BALR    88150  00388703
         AIF   ('&NAME'(1,1) NE '(').GETPGM                      88150  00388803
&MACPLAB LR    R15,&NAME(1)                                      88150  00388903
         AGO   .BAL                                              88150  00389003
.*--------------------------------------------------------------------* 00389103
.*  SUBCALL INTERACTS WITH THE SERVLOAD MACRO.                        * 00389203
.*  WHEN A MODULE LOADED BY SERVLOAD IS REFERENCED HERE, THE L =A()   * 00389303
.*  IS REPLACED BY A LOAD FROM THE NAME USED BY SERVLOAD.             * 00389403
.*                                                                    * 00389503
.*--------------------------------------------------------------------* 00389603
.GETPGM  AIF   ('&NAME'(1,1) NE '/' AND '&NAME'(1,1) NE '*').GETLOAD    00389703
         MACPARM R15,&NAME                                      GP03264 00389803
         AGO   .BAL                                             GP03264 00389903
.GETLOAD ANOP  ,                                                 88150  00390003
&I       SETA  0             SCAN THROUGH SERVLOAD MODULES      GP03150 00390103
.LODLOOP AIF   (&I GE &SRVNMOD).SKIPLOD                         GP03150 00390203
&I       SETA  &I+1                                             GP03150 00390303
 AIF ('&SRVLMOD(&I)' NE '&NAME' AND '&SRVLDEL(&I)' NE '&NAME').LODLOOP  00390403
         MACPARM R15,&SRVLMOD(&I),OP=L                          GP03150 00390503
         AGO   .BALR                                            GP03150 00390603
.SKIPLOD AIF   ('&MODE' NE 'LA').LOAD                           GP03150 00390703
&MACPLAB LA    R15,&NAME     GET SUBROUTINE ADDRESS             GP03150 00390803
.LOAD    ANOP  ,                                                 88150  00390903
&MACPLAB L     R15,=A(&NAME)  GET SUBROUTINE ADDRESS             88150  00391003
.BAL     ANOP  ,                                                 88150  00391103
&MACPLAB SETC  ''                                                88150  00391203
.BALR    AIF   ('&LOP' EQ 'BALR').OLDBAL                        GP12162 00391303
         AIF   (NOT &MVSESA).OLDBAL                             GP12162 00391403
&MACPLAB &LOP  R14,R15       INVOKE IT                          GP00020 00391503
&MACPLAB SETC  ''                                               GP00020 00391603
         MEXIT ,                                                GP03041 00391703
.OLDBAL  ANOP  ,                                                GP12162 00391803
&MACPLAB BALR  R14,R15       INVOKE IT                          GP12162 00391903
&MACPLAB SETC  ''                                               GP12162 00392003
         MEXIT ,                                                GP03041 00392103
.MFL     AIF   ('&PARM' EQ '' AND '&NAME' NE '').OOPS           GP03041 00392203
&NM      DC    A&PARM REMOTE PARM LIST FOR SUBCALL              GP03041 00392303
         MEXIT ,                                                GP03041 00392403
.OOPS    ANOP  ,                                                GP03041 00392503
&NM      DC    A&NAME REMOTE PARM LIST FOR SUBCALL              GP03041 00392603
.MEND    MEND  ,                                                 88150  00392703
./ ADD NAME=SYSPARM  0112-97202-06263-0047-00255-00328-00003-GERHARD 00 00392803
         MACRO ,                                                        00392903
       SYSPARM &DBTEST=YES,&SETS=YES,&LIST=YES,&SHOW=,&PARM=            00393003
.********************************************************************** 00393103
.*   THIS MACRO, FOLLOWING OPTIONGB, SETS GLOBAL ASSEMBLY OPTIONS.      00393203
.*   OVERRIDES ARE MERGED FROM THE CONTENTS OF THE ASSEMBLER EXEC       00393303
.*   PARM SUBFIELD SYSPARM:  // EXEC ASMHC,PARM='SYSPARM(MVS/ESA)'      00393403
.********************************************************************** 00393503
         COPY  OPTIONGB                                                 00393603
         LCLA  &CURSOR                                                  00393703
         LCLA  &I,&J,&K                                                 00393803
         LCLB  &GOTLOC                                                  00393903
         LCLC  &CHAR                                                    00394003
         LCLC  &DEFSP1R,&DEFSP2R,&DEFSP3R,&DEFJES2               90217  00394103
         LCLC  &DELIM                                                   00394203
         LCLC  &TOKEN                                                   00394303
         LCLC  &DEFMOD,&DEFLOC,&DEFMAC,&DEFSOR,&DEFSYM,&DEFSYS   81169  00394403
&GOTLOC  SETB  ('&SETS' EQ 'NO' OR '&LOCAL' NE '' OR &SYSPRM# GT 0)     00394503
         AIF   (&SYSPRM# NE 0).BYEBYE                            81154  00394603
         AIF   ('&PARM' EQ 'IGNORE').NOFRAME                     83100  00394703
         AIF   (T'&PARM EQ 'O').OKPPRM                           83100  00394803
         MNOTE 8,'INVALID PARM=&PARM'                            83100  00394903
.OKPPRM  ANOP  ,                                                 83100  00395003
&K       SETA  K'&SYSPARM                                        82099  00395103
         AIF   (&K LT 2).NOFRAME                                 82099  00395203
         AIF   ('&SYSPARM'(1,1) NE '(').NOFRAME                  82099  00395303
         AIF   ('&SYSPARM'(&K,1) NE ')').NOFRAME                 82099  00395403
&K       SETA  &K-1          SUPPORT FORMAT (A,B,C)              82099  00395503
&CURSOR  SETA  &CURSOR+1                                         82099  00395603
.NOFRAME AIF   (&GOTLOC).FINDTOK                                 82099  00395703
&DEFLOC  SETC  'MVS'           INSTALLATION                      81154  00395803
&DEFMOD  SETC  '370'             DEFAULTS        (360 OR 370)    81154  00395903
&DEFSYS  SETC  'MVS'               HERE          (SYSTEM FLAVOR) 85077  00396003
&DEFSP1R SETC  '0303'                            SP1 RELEASE     85077  00396103
&DEFSP2R SETC  '0200'                            SP2 RELEASE     90252  00396203
&DEFSP3R SETC  '0100'                            SP3 RELEASE     90217  00396303
&DEFJES2 SETC  '41'                              JES2 VERSION    90189  00396403
&DEFMAC  SETC  'GEN'         PRINT OPTION FOR LOCAL MACROS       81154  00396503
&DEFSOR  SETC  'NOGEN'       PRINT OPTION FOR SOURCE CODE        81154  00396603
&DEFSYM  SETC  'NOGEN'       PRINT OPTION FOR SYSTEM MACROS      81154  00396703
&SVCJFCB SETA  0             MODJFCB SVC (SOURCE MEMBER IGC00240)82099  00396803
&SVC@SVC SETA  0             @SERVICE INSTALLED AS SVC ? (255)   84160  00396903
&SVCTMSX SETA  0             UCC-1 (TMS) SVC X                   92271  00397003
&SVCTMSY SETA  0             UCC-1 (TMS) SVC Y                   92271  00397103
.*                                                                      00397203
.FINDTOK AIF   (&CURSOR GE &K).MERGE                             82099  00397303
&CURSOR  SETA  &CURSOR+1                                                00397403
         AIF   ('&SYSPARM'(&CURSOR,1) EQ ' ').FINDTOK                   00397503
         AIF   ('&SYSPARM'(&CURSOR,1) EQ ',').FINDTOK                   00397603
.*                                                                      00397703
&DELIM   SETC  '&SYSPARM'(&CURSOR,1)                                    00397803
&TOKEN   SETC  '&DELIM'                                                 00397903
         AIF   ('&DELIM' EQ '''' OR '&DELIM' EQ '"').CURINC2            00398003
&DELIM   SETC  ''                                                       00398103
&TOKEN   SETC  ''                                                       00398203
.*                                                                      00398303
.SCANTOK AIF   (&CURSOR GT &K).ENDTOK                           82099   00398403
&CHAR    SETC  '&SYSPARM'(&CURSOR,1)                                    00398503
         AIF   ('&DELIM' EQ '&CHAR').CATDEL                             00398603
         AIF   ('&DELIM' EQ '').TESTEND                                 00398703
         AGO   .CATTOK                                                  00398803
.CATDEL  ANOP  ,                                                        00398903
&TOKEN   SETC  '&TOKEN'.'&CHAR'                                         00399003
&CURSOR  SETA  &CURSOR+1                                                00399103
         AIF   (&CURSOR GT &K).GOODTOK                           82099  00399203
&CHAR    SETC  '&SYSPARM'(&CURSOR,1)                                    00399303
         AIF   ('&CHAR' NE '&DELIM').ENDQTOK                            00399403
.TESTEND AIF   ('&CHAR' EQ ' ').GOODTOK                                 00399503
         AIF   ('&CHAR' EQ ',').GOODTOK                                 00399603
.CATTOK  ANOP  ,                                                        00399703
&TOKEN   SETC  '&TOKEN'.'&CHAR'                                         00399803
.CURINC2 ANOP  ,                                                        00399903
&CURSOR  SETA  &CURSOR+1                                                00400003
         AGO   .SCANTOK                                                 00400103
.ENDQTOK AIF   ('&CHAR' EQ ' ' OR '&CHAR' EQ ',').GOODTOK               00400203
         MNOTE 4,'TOKENS RUN TOGETHER - COMMA ASSUMED'                  00400303
         AGO   .GOODTOK                                                 00400403
.ENDTOK  AIF   ('&DELIM' EQ '').GOODTOK                                 00400503
         AIF   ('&TOKEN' NE '').GOODTOK                                 00400603
         MNOTE 8,'UNPAIRED DELIMITER IN &&SYSPARM:'                     00400703
         MNOTE 8,'&SYSPARM'                                             00400803
.GOODTOK AIF   ('&TOKEN' NE 'DEBUG' OR '&DBTEST' EQ 'NO').NOTDB         00400903
&BUGBEAR SETB  1                                                 81331  00401003
         AGO   .FINDTOK                                                 00401103
.NOTDB   AIF   ('&TOKEN' EQ '360' OR '&TOKEN' EQ '370' OR '&TOKEN'     *00401203
               EQ '470' OR '&TOKEN' EQ '390').SETMODL           GP04234 00401303
         AIF   ('&TOKEN' EQ 'MVS' OR '&TOKEN' EQ 'SVS' OR '&TOKEN'     *00401403
               EQ 'VS1' OR '&TOKEN' EQ 'MVT').SETSYS             82137  00401503
         AIF   ('&TOKEN     '(1,6) EQ 'MVS/SP').SETSP            82091  00401603
         AIF   ('&TOKEN     '(1,6) EQ 'MVS/XA').SETXA            82091  00401703
         AIF   ('&TOKEN     '(1,7) EQ 'MVS/ESA').SETESA          90217  00401803
         AIF   ('&TOKEN   '(1,3) EQ 'J2/').SETJES2               85076  00401903
         AIF   (K'&TOKEN NE 6 AND K'&TOKEN NE 8).NOTPROP         82099  00402003
         AIF   ('&TOKEN'(1,1) NE 'P').NOTPROP                    82099  00402103
         AIF   ('&TOKEN'(3,1) NE '/').NOTPROP                    82099  00402203
         AIF   ('&TOKEN'(K'&TOKEN-2,3) NE 'GEN').NOTPROP         82099  00402303
         AIF   ('&TOKEN'(2,1) EQ 'S').PROPSOR                    82099  00402403
         AIF   ('&TOKEN'(2,1) EQ 'M').PROPMAC                    82099  00402503
         AIF   ('&TOKEN'(2,1) EQ 'Y').PROPSYS                    82099  00402603
.NOTPROP AIF   (NOT &GOTLOC).GETLOC                              82099  00402703
         AIF   (&SYSPRM# GE 10).TOOMANY                                 00402803
&SYSPRM# SETA  &SYSPRM#+1                                               00402903
&SYSPRMS(&SYSPRM#) SETC '&TOKEN'                                        00403003
         AGO   .FINDTOK                                                 00403103
.SETMODL ANOP  ,                                                 81154  00403203
&MODEL   SETC  '&TOKEN'                                          81154  00403303
         AGO   .FINDTOK                                          81154  00403403
.*                                                               82099  00403503
.*       PRINT OPTIONS MAY BE PARTIALLY SET WITH THE FORM        82099  00403603
.*       SYSPARM=P?/GEN AND =P?/NOGEN, WHERE ? IS S, M, OR Y     82099  00403703
.PROPSOR ANOP  ,             PS/ - SET SOURCE OPTION             82099  00403803
&PRTSOR  SETC  '&TOKEN'(4,K'&TOKEN-3)                            82099  00403903
         AGO   .FINDTOK                                          82099  00404003
.PROPMAC ANOP  ,             PM/ - SET LOCAL MACRO OPTION        82099  00404103
&PRTMAC  SETC  '&TOKEN'(4,K'&TOKEN-3)                            82099  00404203
         AGO   .FINDTOK                                          82099  00404303
.PROPSYS ANOP  ,             PY/ - SET SYSTEM MACRO OPTION       82099  00404403
&PRTSYS  SETC  '&TOKEN'(4,K'&TOKEN-3)                            82099  00404503
         AGO   .FINDTOK                                          82099  00404603
.SETJES2 ANOP  ,                                                 85076  00404703
&JES2REL SETC  '&TOKEN'(4,K'&TOKEN-3)                            85076  00404803
         AGO   .FINDTOK                                          85076  00404903
.*                                                               85076  00405003
.*                                                               82091  00405103
.SETSP   AIF   ('&TOKEN' EQ 'MVS/SP').SETSYS                     82091  00405203
&SPVEREL SETC  ''                                                82091  00405303
.SETSP1  ANOP  ,                                                 82091  00405403
&I       SETA  6                                                 82091  00405503
.SETSP2  ANOP  ,                                                 82091  00405603
&CHAR    SETC  ''                                                82091  00405703
.SETSP3  ANOP  ,                                                 82091  00405803
&I       SETA  &I+1                                              82091  00405903
         AIF   ('&TOKEN'(&I,1) EQ '.').SPENDL                    82091  00406003
&CHAR    SETC  '&CHAR'.'&TOKEN'(&I,1)                            82091  00406103
         AIF   (&I LT K'&TOKEN).SETSP3                           82091  00406203
.SPENDL  AIF   (K'&CHAR LE 2).SPNERRL                            82091  00406303
         MNOTE 8,'"&CHAR" IN "&TOKEN" IS MORE THAN 2 DIGITS'     82091  00406403
         MNOTE 8,'"&CHAR" WILL BE TRUNCATED ON THE LEFT'         82091  00406503
.SPNERRL ANOP  ,                                                 82091  00406603
&CHAR    SETC  '00'.'&CHAR'                                      82091  00406703
&SPVEREL SETC  '&SPVEREL'.'&CHAR'(K'&CHAR-1,2)                   82091  00406803
         AIF   (&I LT K'&TOKEN).SETSP2                           82091  00406903
&SYSTEM  SETC  'MVS/SP'                                          82091  00407003
         AIF   (K'&SPVEREL GT 2).SPRELS                          82091  00407103
&SPVEREL SETC  '&SPVEREL'.'01'                                   82091  00407203
.SPRELS  AIF   (K'&SPVEREL GT 4).SPLEVS                          82091  00407303
&SPVEREL SETC  '&SPVEREL'.'00'                                   82091  00407403
.SPLEVS  AIF   ('&SPVEREL'(1,2) LE '01').FINDTOK                 82091  00407503
&MVSXA   SETB  1                                                 82091  00407603
         AIF   ('&SPVEREL'(1,2) LE '02').FINDTOK                 90217  00407703
&MVSESA  SETB  1                                                 90217  00407803
         AGO   .FINDTOK                                          82091  00407903
.*                                                               82091  00408003
.SETXA   AIF   ('&TOKEN' EQ 'MVS/XA').SETSYS                     82091  00408103
&SYSTEM  SETC  'MVS/SP'                                          82091  00408203
&SPVEREL SETC  '02'                                              82091  00408303
         AGO   .SETSP1                                           82091  00408403
.*                                                               90217  00408503
.SETESA  AIF   ('&TOKEN' EQ 'MVS/ESA').SETSYS                    90217  00408603
&SYSTEM  SETC  'MVS/SP'                                          90217  00408703
&SPVEREL SETC  '03'                                              90217  00408803
         AGO   .SETSP1                                           90217  00408903
.*                                                               82091  00409003
.SETSYS  ANOP  ,                                                 81154  00409103
&SYSTEM  SETC  '&TOKEN'                                          81154  00409203
         AGO   .FINDTOK                                          81154  00409303
.GETLOC  AIF   ('&TOKEN' EQ 'TSM').OPTTSM                               00409403
&DEFSYS  SETC  'MVS'         DEFAULT SYSTEM                      94217  00409503
&DEFMAC  SETC  'GEN'         LOCAL MACROS                        90031  00409603
&DEFSOR  SETC  'NOGEN'       LOCAL SOURCE                        90031  00409703
&DEFSYM  SETC  'NOGEN'       SYSTEM MACROS                       90031  00409803
&SVCJFCB SETA  0             NO MODJFCB SVC                      90031  00409903
&SVC@SVC SETA  0             @SERVICE NOT INSTALLED AS SVC       90031  00410003
&SVCTMSX SETA  0             UCC-1 TMS ?                         90031  00410103
&SVCTMSY SETA  0             UCC-1 TMS ?                         90031  00410203
         AGO   .OPT370                                           90031  00410303
.*                                                                      00410403
.OPTTSM  ANOP  ,                                                 82099  00410503
&SVCJFCB SETA  240           MODJFCB SVC                         82099  00410603
&SVC@SVC SETA  0             @SERVICE NOT INSTALLED AS SVC       83100  00410703
&SVCTMSX SETA  0             UCC-1 TMS ?                         82099  00410803
&SVCTMSY SETA  0             UCC-1 TMS ?                         82099  00410903
.*                                                                      00411003
.OPTMVS  ANOP  ,                                                        00411103
&DEFSYS  SETC  'MVS'                                                    00411203
         AGO   .OPT370                                           82099  00411303
.*                                                                      00411403
.OPTSVS  ANOP  ,                                                        00411503
&DEFSYS  SETC  'SVS'                                                    00411603
.OPT370  ANOP  ,                                                        00411703
&DEFMOD  SETC  '370'                                                    00411803
.COMLOC  ANOP  ,                                                 81154  00411903
&LOCAL   SETC  '&TOKEN'                                          81154  00412003
&GOTLOC  SETB  1                                                        00412103
         AGO   .FINDTOK                                          81154  00412203
.TOOMANY MNOTE 8,'MORE THAN 10 ELEMENTS IN &&SYSPARM:'                  00412303
         MNOTE 8,'&SYSPARM'                                             00412403
.MERGE   AIF   ('&LOCAL' NE '').MGLOC                            81154  00412503
&LOCAL   SETC  '&DEFLOC'                                         81154  00412603
.MGLOC   AIF   ('&MODEL' NE '').MGMOD                            81154  00412703
&MODEL   SETC  '&DEFMOD'                                         81154  00412803
.MGMOD   AIF   ('&PRTMAC' NE '').MGMAC                           81154  00412903
&PRTMAC  SETC  '&DEFMAC'                                         81154  00413003
.MGMAC   AIF   ('&PRTSOR' NE '').MGSOR                           81154  00413103
&PRTSOR  SETC  '&DEFSOR'                                         81154  00413203
.MGSOR   AIF   ('&PRTSYS' NE '').MGSYM                           81154  00413303
&PRTSYS  SETC  '&DEFSYM'                                         81154  00413403
.MGSYM   AIF   ('&SYSTEM' NE '').MGSYS                           81154  00413503
&SYSTEM  SETC  '&DEFSYS'                                         81154  00413603
.MGSYS   ANOP  ,                                                 81154  00413703
&MVSESA  SETB  (&MVSESA OR '&SYSTEM' EQ 'MVS/ESA')               90217  00413803
&MVSXA   SETB  (&MVSXA OR &MVSESA OR '&SYSTEM' EQ 'MVS/XA')      90217  00413903
&MVSSP   SETB  (&MVSSP OR &MVSXA OR '&SYSTEM' EQ 'MVS/SP')       90217  00414003
         AIF   (NOT &MVSSP OR '&SPVEREL' NE '').MGSP             82091  00414103
&SYSTEM  SETC  'MVS/SP'                                          82091  00414203
&SPVEREL SETC  '01'.'&DEFSP1R'                                   82091  00414303
         AIF   (NOT &MVSXA).MGSP                                 82091  00414403
&SYSTEM  SETC  'MVS/XA'                                          90217  00414503
&SPVEREL SETC  '02'.'&DEFSP2R'                                   82091  00414603
         AIF   (NOT &MVSESA).MGSP                                90217  00414703
&SYSTEM  SETC  'MVS/ESA'                                         90217  00414803
&SPVEREL SETC  '03'.'&DEFSP3R'                                   90217  00414903
.MGSP    AIF   ('&JES2REL' NE '').MGSJ2                          85076  00415003
&JES2REL SETC  '&DEFJES2'                                        85076  00415103
.MGSJ2   ANOP  ,                                                 85076  00415203
&CPU     SETC  '&MODEL'                                                 00415303
&MVS     SETB  ('&SYSTEM'(1,3) EQ 'MVS')                         82091  00415403
&SVS     SETB  ('&SYSTEM' EQ 'SVS')                                     00415503
&VS1     SETB  ('&SYSTEM' EQ 'VS1')                              82137  00415603
.BYEBYE  AIF   ('&LIST' EQ 'NO').MEND                            81154  00415703
         MNOTE *,'                                                  '   00415803
         MNOTE *,'               INSTALLATION &LOCAL                '   00415903
&CHAR    SETC  ''                                                85076  00416003
         AIF   (NOT &MVSSP).PRTVER                               85076  00416103
&CHAR    SETC  'V'.'&SPVEREL'(1,2)                               82091  00416203
         AIF   (K'&SPVEREL LE 3).PRTVER                          82091  00416303
&CHAR    SETC  '&CHAR'.'.R'.'&SPVEREL'(3,2)                      82091  00416403
         AIF   (K'&SPVEREL LE 5).PRTVER                          82091  00416503
&CHAR    SETC  '&CHAR'.'.L'.'&SPVEREL'(5,2)                      82091  00416603
.PRTVER  MNOTE *,'      CPU   &MODEL      SYSTEM &SYSTEM  &CHAR     '   00416703
         AIF   ('&JES2REL' EQ '').NOTJES2                        85076  00416803
         MNOTE *,'      JES2 RELEASE &JES2REL                       '   00416903
.NOTJES2 MNOTE *,'      PRINT SOR &PRTSOR  MAC &PRTMAC  SYS &PRTSYS '   00417003
         MNOTE *,'      SVC:  TMS=&SVCTMSX/&SVCTMSY  JFCB=&SVCJFCB  @SV*00417103
               C=&SVC@SVC '                                      83100  00417203
         MNOTE *,'                                                  '   00417303
         AIF   ('&SHOW' EQ '').IFBUG                                    00417403
         MNOTE *,'      MVS &MVS  MVS/SP &MVSSP  MVS/XA &MVSXA  MVS/ESA*00417503
               &MVSESA'                                                 00417603
.IFBUG   AIF   (NOT &BUGBEAR).MEND                               82099  00417703
         MNOTE *,'**************************************************'   00417803
         MNOTE *,'*                                                *'   00417903
         MNOTE *,'*              DEBUG MODE IN EFFECT              *'   00418003
         MNOTE *,'*                                                *'   00418103
         MNOTE *,'**************************************************'   00418203
.MEND    MEND  ,                                                 81154  00418303
./ ADD NAME=VCON     0100-02242-02242-2026-00074-00074-00000-GERHARD 00 00418403
         MACRO ,                                                        00418503
&NM      VCON  &STR,&END=,&BNDRY=H                      ADDED ON 81155  00418603
         GBLB  &VCON@OP                                                 00418703
         GBLC  &VCON@NM                                                 00418803
         LCLA  &I,&J,&K,&L                                              00418903
         LCLC  &L2                                                      00419003
.********************************************************************** 00419103
.**                                                                  ** 00419203
.**  VCON BUILDS A TEXT MESSAGE BEGINNING WITH A TWO-BYTE LENGTH,    ** 00419303
.**  TWO BYTES OF ZERO, AND TEXT OF THAT LENGTH (WTO / RECFM=V FMT)  ** 00419403
.**                                                                  ** 00419503
.**  USE   VCON  'TEXT'                                              ** 00419603
.**                                                                  ** 00419703
.**  OR    VCON  'TEXT1',END=LABEL                                   ** 00419803
.**        DC     ...ZERO OR MORE STORAGE ITEMS                      ** 00419903
.**  LABEL VCON   *END    TO GENERATE A SINGLE MESSAGE               ** 00420003
.**                                                                  ** 00420103
.********************************************************************** 00420203
&K       SETA  K'&STR                                                   00420303
         AIF   (T'&END NE 'O').TSTOPEN                                  00420403
         AIF   (T'&STR EQ 'O').CLOSE                                    00420503
         AIF   ('&STR'(1,1) EQ '*').CLOSE                               00420603
.TSTOPEN AIF   (&K EQ 0).COMLEN                                         00420703
         AIF   ('&STR'(1,1) NE '''').COMLEN                             00420803
&I       SETA  2                                                        00420903
&J       SETA  &K-2                                                     00421003
&K       SETA  &J                                                       00421103
.LOOP    AIF   ('&STR'(&I,2) EQ '''''').SK2                             00421203
         AIF   ('&STR'(&I,2) EQ '&&').SK2                               00421303
&I       SETA  &I+1                                                     00421403
         AGO   .INC                                                     00421503
.SK2     ANOP  ,                                                        00421603
&I       SETA  &I+2                                                     00421703
&K       SETA  &K-1                                                     00421803
.INC     AIF   (&I LE &J).LOOP                                          00421903
.COMLEN  AIF   (NOT &VCON@OP).NOPEN                                     00422003
         MNOTE 4,'PRIOR VCON NOT TERMINATED'                            00422103
&VCON@OP SETB  0                                                        00422203
.NOPEN   AIF   ('&BNDRY' EQ 'H' OR '&BNDRY' EQ 'Y').NOBOUND             00422303
         AIF   ('&BNDRY' NE 'X' AND '&BNDRY' NE 'C').DOBOUND            00422403
&L2      SETC  'L2'                                                     00422503
         AGO   .NOBOUND                                                 00422603
.DOBOUND DS    0&BNDRY                                                  00422703
.NOBOUND AIF   (T'&END NE 'O').OPEN                                     00422803
         AIF   (&K EQ 0).REQSTR                                         00422903
         AIF   ('&STR'(1,1) EQ '''').QSTR                               00423003
&NM      DC    Y&L2.(&K+4,0),C'&STR'                                    00423103
         AGO   .MEND                                                    00423203
.QSTR    ANOP  ,                                                        00423303
&NM      DC    Y&L2.(&K+4,0),C&STR                                      00423403
         AGO   .MEND                                                    00423503
.OPEN    AIF   (&K NE 0).OPSTR                                          00423603
&NM      DC    Y&L2.(&END-*,0)                                          00423703
         AGO   .SETOPEN                                                 00423803
.OPSTR   AIF   ('&STR'(1,1) EQ '''').OQSTR                              00423903
&NM      DC    Y&L2.(&END-*,0),C'&STR'                                  00424003
         AGO   .SETOPEN                                                 00424103
.OQSTR   ANOP  ,                                                        00424203
&NM      DC    Y&L2.(&END-*,0),C&STR                                    00424303
.SETOPEN ANOP  ,                                                        00424403
&VCON@NM SETC  '&END'                                                   00424503
&VCON@OP SETB  1                                                        00424603
         MEXIT ,                                                        00424703
.REQSTR  MNOTE 4,'TEXT STRING REQUIRED'                                 00424803
         MEXIT ,                                                        00424903
.CLOSE   AIF   (&VCON@OP).WASOPEN                                       00425003
         MNOTE 4,'VCON END OUT OF SEQUENCE'                             00425103
.WASOPEN AIF   ('&NM' EQ '' OR '&NM' EQ '&VCON@NM').BLAB                00425203
&NM      EQU   *                                                        00425303
.BLAB    ANOP  ,                                                        00425403
&VCON@NM EQU   *                                                        00425503
&VCON@NM SETC  ''                                                       00425603
&VCON@OP SETB  0                                                        00425703
.MEND    MEND  ,                                                        00425803
./ ADD NAME=YREGS    8000-12011-12011-0124-00011-00011-00000-GERHARD 00 00425903
         MACRO                                                          00426003
         YREGS ,                                                        00426103
         GBLA  &REGS                                                    00426203
         AIF   (&REGS EQ 1).MEND  ONLY EXPAND ONCE                      00426303
&REGS    SETA  1             MAINTAIN IBM COMPATIBILITY                 00426403
         LCLA  &I                                                       00426503
.LOUPE   AIF   (&I GT 15).MEND                                          00426603
R&I      EQU   &I                                                       00426703
&I       SETA  &I+1                                                     00426803
         AGO   .LOUPE                                                   00426903
.MEND    MEND                                                           00427003
./ ENDUP       "REVIEW" PDS MEMBER OFFLOAD AT 07:12 ON 22-01-25         00427103
/*                                                                      00427203
//*------------------------------------------------------ LOADMACS      00427303
//*                                                                     00427403
//ASM1.SYSIN DD *                                                       00427503
SHOWDSCB TITLE 'S H O W D S C B  ***  DISPLAY FMT1/FMT3 DSCB INFO'      00427603
         PUNCH '  ORDER SHOWDSCB(P) '  MAKE DUMPS EASIER                00427703
         COPY  OPTIONGB                                                 00427803
         SPACE 1                                                        00427903
         SYSPARM LIST=YES                                               00428003
         SPACE 1                                                        00428103
*********************************************************************** 00428203
*                                                                     * 00428303
*    PROGRAM SHOWDSCB USES THE PARM FIELD TO OBTAIN THE NAME OF A     * 00428403
*    DATASET TO BE ANALYZED. ONLY DIRECT ACCESS (DASD) SUPPORTED.     * 00428503
*                                                                     * 00428603
*    A VOLUME SERIAL MAY BE SUPPLIED FOR DATASETS NOT CATALOGED.      * 00428703
*                                                                     * 00428803
*    MULTIPLE REQUESTS MAY BE STACKED, UP TO THE PARM LIMIT OF        * 00428903
*    100 BYTES, BY SEPARATING THEM WITH A SEMI-COLON.                 * 00429003
*                                                                     * 00429103
*    IN ADDITION TO THE PARM FIELD, DATA SET NAMES TO BE ANALYZED     * 00429203
*    MAY BE SUPPLIED ON SYSUTnnn DD CARDS. "nnn" MAY BE 0 TO 3 VALID  * 00429303
*    CHARACTERS, BUT THE NAME MUST BE UNIQUE.                         * 00429403
*                                                                     * 00429503
*    A MEMBER NAME SPECIFICATION IS IGNORED.                          * 00429603
*                                                                     * 00429703
*    SAMPLE JCL:                                                      * 00429803
*                                                                     * 00429903
*    //JOBNAME JOB ....                                               * 00430003
*    /* OR //* AS PER YOUR INSTALLATION                               * 00430103
*    //EXEC PGM=SHOWDSCB,PARM='DSN1;DSN2,VOL2;DSN3(MEM)'              * 00430203
*    //SYSPRINT DD SYSOUT=*  REQUIRED PRINT FILE                      * 00430303
*    //SYSUDUMP DD SYSOUT=*  OPTIONAL DEBUG FILE                      * 00430403
*                                                                     * 00430503
*    SHOWDSCB IS RE-ENTRANT, REFRESHABLE AND RE-USABLE.               * 00430603
*                                                                     * 00430703
*    UNDER MVS 3.8J (AT LEAST), CATALOG DATA SET NAMES ARE NOT        * 00430803
*    RECOGNIZED CORRECTLY - USE THE ACTUAL DSCB NAME.                 * 00430903
*                                                                     * 00431003
*    CODE ADDED TO CHECK VOLUME SERIAL FOR MIGRAT.                    * 00431103
*    THIS PROGRAM WILL NOT ISSUE A RECALL REQUEST.                    * 00431203
*                                                                     * 00431303
*********************************************************************** 00431403
*  MAINTENANCE:                                                       * 00431503
* 2005-06-11 GYP ADDED CODE TO ALLOW TSO CP FORMAT PARM FIELD         * 00431603
*                                                                     * 00431703
*********************************************************************** 00431803
         EJECT ,                                                        00431903
         PRINT &PRTSOR                                                  00432003
SHOWDSCB PGMHEAD ZERO12,BASE=(R11,R12),PARM=R9,BNDRY=PAGE,             *00432103
               LOC=BELOW,AM=31,RM=24 (DCB ADDRESSES)                    00432203
         SPACE 1                                                        00432303
         EXTRN SUBDSATR,SUBD2J,SUBXTSUM                                 00432403
         SPACE 1                                                        00432503
         SERVINIT ,                                                     00432603
         SERVLOAD @PRINTER                                              00432703
         LM    R14,R1,PATDSCB1                                          00432803
         ALR   R15,R13       RELOCATE DSNAME                            00432903
         ALR   R0,R13        RELOCATE VOLSER                            00433003
         ALR   R1,R13        RELOCATE WORK AREA                         00433103
         STM   R14,R1,CAMDSCB1  COMPLETE OBTAIN REQUEST                 00433203
         LM    R14,R1,PATSEEK3                                          00433303
         ALR   R15,R13       RELOCATE CCHHR                             00433403
         ALR   R0,R13        RELOCATE VOLSER                            00433503
         ALR   R1,R13        RELOCATE WORK AREA                         00433603
         STM   R14,R1,CAMSEEK3  COMPLETE OBTAIN REQUEST                 00433703
         LM    R14,R1,PATLOCAT                                          00433803
         ALR   R15,R13       RELOCATE USER'S DSN                        00433903
         ALR   R1,R13        RELOCATE WORK AREA                         00434003
         STM   R14,R1,CAMLOCAT                                          00434103
         SERVCALL TIODD,=CL8'SYSUDUMP'  DOES USER WISH DEBUG MODE ?     00434203
         LTR   R0,R0         TEST RETURN                                00434303
         BNZ   SETBUGGY      FOUND - SET DEBUG MODE                     00434403
         SERVCALL TIODD,=CL8'SYSMDUMP'  ALTERNATE ?                     00434503
         LTR   R0,R0                                                    00434603
         BNZ   SETBUGGY      FOUND                                      00434703
         SERVCALL TIODD,=CL8'SYSABEND'                                  00434803
         LTR   R0,R0                                                    00434903
         BZ    NOTBUGGY      NO                                         00435003
SETBUGGY OI    FLAGS,FGDEBUG  SET DEBUG MODE                            00435103
NOTBUGGY PRTOPEN SYSPRINT,OPT=(WTO,ABEND)                               00435203
HAVEPRT  PRTLIST HEADER,TITLE=1  PRINT A TITLE                          00435303
         LR    R7,R9         PRESERVE SIGN                      GP09218 00435403
         LA    R9,0(,R9)     CLEAN PARM POINTER                         00435503
         LTR   R9,R9         ANY ADDRESS ?                      GP09218 00435603
         BZ    PARMNULL      NO; ERROR                                  00435703
         ICM   R9,15,0(R9)   GET PARM ADDRESS                           00435803
         BZ    PARMNULL      NONE; ERROR                                00435903
         SLR   R8,R8                                                    00436003
         ICM   R8,3,0(R9)    TEST PARM LENGTH (<100 ?)                  00436103
         BNP   PARMNULL      INVALID                                    00436203
         LTR   R7,R7         POSSIBLE TSO CP INVOCATION ?       GP05162 00436303
         BM    NOTSOCP       NO                                 GP05162 00436403
         CLI   2(R9),0       SECOND COUNT FIELD ?               GP05162 00436503
         BH    NOTSOCP       NO                                 GP05162 00436603
         SH    R8,2(,R9)     ADJUST                             GP05162 00436703
         AH    R9,2(,R9)     POSITION AFTER COMMAND             GP05162 00436803
         SH    R8,=H'4'      ALLOW FOR HEADER LENGTH            GP05162 00436903
         LA    R9,2(,R9)     SKIP DISPLACEMENT                  GP05162 00437003
NOTSOCP  LA    R9,2(,R9)     MAKE SCANNING EASIER                       00437103
         STM   R8,R9,PARMPTRS  STASH FOR LOOP                           00437203
         SPACE 1                                                        00437303
*********************************************************************** 00437403
*                                                                     * 00437503
*    PRELIMINARIES ARE OUT OF THE WAY - LOOP THROUGH PARM FIELD       * 00437603
*    SCANNING FOR TEXT SEPARATED BY SEMI-COLONS. EXPECTED FORMS:      * 00437703
*    'DSNAME;...'   'DSNAME(MEMBER)'   'GDGNAME(-1)'                  * 00437803
*    'DSNAME,VOLSER;...'                                              * 00437903
*    NULL FIELDS ';;' ARE SKIPPED; NULL MEMBER NAMES ARE SKIPPED      * 00438003
*    NULL SERIALS ARE IGNORED;  OTHER NULLS ARE ERRORS.               * 00438103
*                                                                     * 00438203
*********************************************************************** 00438303
RESTART  CLRL  WORKBLK,WORKBLKN,FILL=C' '  BLANK THE EBCDIC WORK        00438403
         CLRL  WORKCLR,WORKCLRN        CLEAR THE REST OF THE WORK AREA  00438503
         LM    R8,R9,PARMPTRS  LOAD THE PARM LENGTH AND ADDRESS         00438603
         L     R0,=AL1(0,C',',C'(',C';')  SET SCAN STOPS                00438703
         BAL   R14,SCANNER   LOOK FOR SOMETHING                         00438803
         LTR   R15,R15       NULL DSN ?                                 00438903
         BP    HAVEDSNP      NO                                         00439003
         CLM   R2,1,SCANTAB+C';'  NULL FIELD ?                          00439103
         BNE   HAVEDSNP      NO                                         00439203
         STM   R8,R9,PARMPTRS  SAVE UPDATED POINTERS                    00439303
         B     POSTPRMS      LOOK FOR MORE                              00439403
         SPACE 1                                                        00439503
HAVEDSNP CLM   R2,1,SCANTAB+C'('  DID WE FIND A PARENTHESIS ?           00439603
         BNE   PLAINDSN      NO; DO NORMAL DSN PROCESSING               00439703
         LTR   R8,R8         ANY MORE AFTER THIS ?                      00439803
         BNP   PARMBAD       NO; MALFORMED PARM                         00439903
         CLI   0(R9),C'-'    GDG REFERENCE ?                            00440003
         BE    SCANGDG       YES; RESCAN                                00440103
         CLI   0(R9),C'+'    INVALID GDG REFERENCE ?                    00440203
         BE    SCANGDG                                                  00440303
         CLI   0(R9),C'0'    OTHER FORMAT GDG ?                         00440403
         BL    SCANMEM       NO; SCAN FOR MEMBER NAME                   00440503
         CLI   0(R9),C'9'    GDG ?                                      00440603
         BH    SCANMEM       NO; PROBABLY BAD MEMBER NAME               00440703
SCANGDG  LM    R8,R9,PARMPTRS  RELOAD PARM POINTERS                     00440803
         L     R0,=A(0,0,C',',C';')  SCAN FOR VOLUME OR END             00440903
         BAL   R14,SCANNER                                              00441003
         B     PLAINDSN      PROCESS DSNAME ONLY                        00441103
         SPACE 1                                                        00441203
SCANMEM  CH    R15,=Y(L'DSNAME)  VALID LENGTH ?                         00441303
         BH    PARMBDSN      NO; COMPLAIN                               00441403
         SH    R15,=H'1'     MAKE LENGTH FOR EXECUTE                    00441503
         BM    PARMNULL      NONE; COMPLAIN                             00441603
         EX    R15,MVCDSN    MOVE DSN                                   00441703
         OI    FLAGS,FGONE   SHOW USER SPECIFIED A DATA SET             00441803
         L     R0,=AL1(C'(',C')',C',',C';')  SET SCAN STOPS             00441903
         BAL   R14,SCANNER   LOOK FOR END                               00442003
         CLM   R2,1,SCANTAB+C')'  CORRECT STOP FOR MEMBER NAME ?        00442103
         BNE   PARMBMEM      NO; INVALID MEMBER NAME                    00442203
         CH    R15,=Y(L'MEMBER)  VALID LENGTH ?                         00442303
         BH    PARMBMEM      NO                                         00442403
         LTR   R15,R15       VALID LENGTH ?                             00442503
         BM    PARMBMEM      NO                                         00442603
         L     R0,=AL1(0,0,C',',C';')  SCAN FOR POSSIBLE SERIAL         00442703
         BAL   R14,SCANNER   LOOK FOR IT                                00442803
         LTR   R15,R15       DID WE HIT ANY TEXT ?                      00442903
         BP    PARMBMEM      YES; BAD NAME                              00443003
         B     SCANPVOL      SCAN FOR POSSIBLE VOLUME                   00443103
         SPACE 1                                                        00443203
PLAINDSN CH    R15,=Y(L'DSNAME)  VALID LENGTH ?                         00443303
         BH    PARMBDSN      NO; COMPLAIN                               00443403
         SH    R15,=H'1'     MAKE LENGTH FOR EXECUTE                    00443503
         BM    PARMNULL      NONE; COMPLAIN                             00443603
         EX    R15,MVCDSN    MOVE DSN                                   00443703
         OI    FLAGS,FGONE   SHOW USER SPECIFIED A DATA SET             00443803
SCANPVOL CLM   R2,1,SCANTAB+C','  POSSIBLE VOLUME SERIAL                00443903
         BNE   SCANNVOL      NO                                         00444003
         L     R0,=AL1(C'(',C')',C',',C';')  BUT WANT SEMI-COLON ONLY   00444103
         BAL   R14,SCANNER   LOOK FOR END                               00444203
         CLM   R2,1,SCANTAB+C';'  EXPECTED FIELD END ?                  00444303
         BNE   PARMBVOL      NO; ERROR                                  00444403
         CH    R15,=Y(L'SERIAL)  VALID LENGTH ?                         00444503
         BH    PARMBVOL      NO; FAIL                                   00444603
         SH    R15,=H'1'     ADJUST FOR EXECUTE                         00444703
         BM    SCANNVOL      NO SERIAL; NEED CATALOG ENTRY              00444803
         SPACE 1                                                        00444903
SCANYVOL EX    R15,MVCVOL    MOVE SERIAL                                00445003
SCANNVOL DS    0H            NO SERIAL SUPPLIED                         00445103
PARMEND  STM   R8,R9,PARMPTRS  SAVE RESIDUAL LENGTH, ETC. FOR NEXT PASS 00445203
*DEBUG*  PRTLIST MGCURPRM    SHOW WHAT WE HAVE SO FAR                   00445303
         SPACE 2                                                        00445403
*********************************************************************** 00445503
*                                                                     * 00445603
*    FINISHED PARSING OF DSN / MEMBER /VOLUME                         * 00445703
*    CHECK WHETHER THE DATASET IS IN THE CATALOG IF NO SERIAL GIVEN   * 00445803
*                                                                     * 00445903
*********************************************************************** 00446003
COMMFORM MVC   TRUENAME,DSNAME  COPY NAME FOR CATALOG MANIPULATION      00446103
         LOCATE CAMLOCAT     IS IT CATALOGED ?                          00446203
         BXH   R15,R15,EXAMNLOC   NO, OR ERROR                          00446303
         CLC   =H'1',CATVOL#  ONE VOLUME ENTRY ?                        00446403
         BH    EXAMNLOC      NO; TREAT AS UNCATALOGED                   00446503
         CLC   =C'MIGRAT',CATVOL  MIGRATED DATASET?             GP99062 00446603
         BE    SHOWMIGR      YES; BYPASS DEVICE TYPE CHECK      GP99062 00446703
         CLI   CATDEV+2,UCB3DACC  DASD DEVICE ?                         00446803
         BNE   EXAMNLOC      NO; TREAT AS UNCATALOGED                   00446903
EXAMNDEV CLC   SERIAL,BLANKS  DID USER SPECIFY A VOLUME ?               00447003
         BNE   EXAMYLOC      YES; LOOK AT IT                            00447103
         MVC   SERIAL,CATVOL  USE IT                                    00447203
EXAMYLOC CLC   CATVOL,SERIAL   SAME VOLSER  ?                           00447303
         BE    POSTLOC       YES                                        00447403
EXAMNLOC MVC   TRUENAME,DSNAME  RESTORE USER'S NAME                     00447503
         SPACE 1                                                        00447603
POSTLOC  CLC   SERIAL,BLANKS  HAVE WE A VOLUME SERIAL ?                 00447703
         BNE   SEEMOUNT      YES; TRY TO FIND UCB                       00447803
         PRTLIST MGNOSER     NOT LOCATED - GONE ?                       00447903
         OICC  4,RESULT=RETCODE   SET MINOR ERROR                       00448003
         B     POSTPROC      GO TO END OF REQUEST                       00448103
         SPACE 1                                                        00448203
SHOWMIGR PRTLIST PLFMIG      SHOW MIGRATED                      GP99062 00448303
         B    POSTPROC       AND IGNORE                         GP99062 00448403
         SPACE 1                                                        00448503
*********************************************************************** 00448603
*                                                                     * 00448703
*    LOCATE UCB FOR DATA SET                                          * 00448803
*                                                                     * 00448903
*********************************************************************** 00449003
         SPACE 1                                                        00449103
SEEMOUNT LA    R5,PLBADMNT   PRESET MESSAGE FOR FAILED MOUNT            00449203
         SERVCALL UCBDK,SERIAL,ERR=POSTPATE  LOCATE UCB BY VOLSER       00449303
         LTR   R3,R0         FOUND ?                                    00449403
         BNP   POSTPATE      NO; SET MESSAGE AND ERROR                  00449503
         USING UCBOB,R3      DECLARE UCB                                00449603
         SERVCALL UCBGN,(R3)  GET GENERIC FOR UNIT                      00449703
         MVC   MEMBER,0(R1)                                             00449803
         SERVCALL DVTBL,(R3) GET DEVICE TABLE                           00449903
         LR    R1,R0                                                    00450003
         MVC   HIGHTRK,DVCTRK-DVCT(R1)  TRACKS PER CYLINDER             00450103
         SPACE 1                                                        00450203
*********************************************************************** 00450303
*                                                                     * 00450403
*    VERIFY THAT DATASET EXISTS - OBTAIN DSCB                         * 00450503
*                                                                     * 00450603
*********************************************************************** 00450703
SEQVER   OBTAIN CAMDSCB1     FIND DSCB 1 FOR DATASET                    00450803
         STM   R15,R0,DB     SAVE ERROR CODE FOR MESSAGE                00450903
         LA    R5,PLNOFMT1   NOT FOUND                                  00451003
         BXH   R15,R15,POSTPATE  ISSUE MESSAGE AND ERROR CODE           00451103
         BAL   R14,FORMDSCB  FORMT THE INFORMATION                      00451203
         B     POSTPROC      WRITE DUMP IF DEBUG MODE                   00451303
         SPACE 2                                                        00451403
*********************************************************************** 00451503
*                                                                     * 00451603
*    THE REQUEST HAS BEEN PROCESSED (SUCCESSFULLY OR OTHERWISE);      * 00451703
*    NOW WE CLEAN UP AND ISSUE MESSAGES, SET RETURN CODE, ETC.        * 00451803
*                                                                     * 00451903
*********************************************************************** 00452003
POSTPATE OICC  8             SET ERROR                                  00452103
         B     POSTLIST      WRITE MESSAGE                              00452203
         SPACE 1                                                        00452303
POSTFOUR OICC  4             SET LESSER ERROR                           00452403
POSTLIST PRTLIST (R5)        WRITE NON-CRITICAL MESSAGE                 00452503
         SPACE 1                                                        00452603
POSTPROC PRTLIST MGCURPRM    DUMP STORAGE AREA IF DEBUG MODE            00452703
POSTPRMS ICM   R8,15,PARMPTRS  ANY MORE TEXT TO SCAN ?                  00452803
         BP    RESTART       YES; EXAMINE NEXT REQUEST                  00452903
         SPACE 2                                                        00453003
*********************************************************************** 00453103
*                                                                     * 00453203
*    FINISHED WITH PARM PROCESSING. NOW LOOP THROUGH THE TIOT AND     * 00453303
*    REPEAT FOR ALL UNIQUE SYSUTxxx DD CARDS.                         * 00453403
*                                                                     * 00453503
*********************************************************************** 00453603
PARMNULL L     R2,@TIOT      GET PREVIOUS TIOT ENTRY                    00453703
LOOPTIOT SERVCALL TIOLK,(R2) LOOP - GET NEXT TIOT ENTRY                 00453803
         LTR   R2,R0         DID WE GET ONE ?                           00453903
         BZ    PGMEXIT       NO; ALL DONE                               00454003
         USING TIOELNGH,R2   DECLARE IT                                 00454103
         CLC   =C'SYSUT',TIOEDDNM   ONE OF OURS ?                       00454203
         BNE   LOOPTIOT      NO; TRY ANOTHER                            00454303
         ST    R2,@TIOT      SAVE FOR NEXT TIME                         00454403
         SERVCALL TIODD,TIOEDDNM  DO TIOT LOOKUP                        00454503
         CR    R2,R0         SAME ENTRY ?                               00454603
         BE    HAVETIOT      YES                                        00454703
         PRTLIST MGDUPDDN    SIGNAL DUPLICATE DD NAME                   00454803
         B     LOOPTIOT      SKIP THIS ONE                              00454903
SKIPTIOT PRTLIST MGDBADDD    SIGNAL DUPLICATE DD NAME                   00455003
         B     LOOPTIOT      SKIP THIS ONE                              00455103
         SPACE 1                                                        00455203
HAVETIOT ICM   R3,7,TIOEFSRT  IS THERE A UCB ?                          00455303
         BZ    SKIPTIOT      NO; IGNORE                                 00455403
         CLI   UCBTBYT3,UCB3DACC   DASD ?                               00455503
         BNE   SKIPTIOT      NO; IGNORE                                 00455603
         OI    FLAGS,FGONE   SHOW USER SPECIFIED A DATA SET             00455703
         SERVCALL UCBGN,(R3)  GET GENERIC FOR UNIT                      00455803
         MVC   MEMBER,0(R1)                                             00455903
         SERVCALL DVTBL,(R3) GET DEVICE TABLE                           00456003
         LR    R1,R0                                                    00456103
         MVC   HIGHTRK,DVCTRK-DVCT(R1)  TRACKS PER CYLINDER             00456203
         ICM   R1,7,TIOEJFCB                                            00456303
         SERVCALL SWAAD,(R1)  GET JFCB ADDRESS                          00456403
         BXH   R15,R15,SKIPTIOT                                         00456503
         USING JFCBDSNM,R1   DECLARE IT                                 00456603
         MVC   DSNAME,JFCBDSNM                                          00456703
         MVC   TRUENAME,DSNAME  COPY NAME FOR CATALOG CHECK             00456803
         MVC   SERIAL,JFCBVOLS                                          00456903
         B     COMMFORM      GO TO COMMON FOMRATTING                    00457003
         SPACE 1                                                        00457103
PGMEXIT  TM    FLAGS,FGONE   ANY SPECIFIED ?                            00457203
         BNZ   PGMEXITX      OK                                         00457303
         PRTL  '0No Data Sets specified',CC=ASA                         00457403
         OICC  4                                                        00457503
PGMEXITX SERVTERM ,                                                     00457603
         L     R9,RETCODE                                               00457703
EXITCODE PGMEXIT RC=(R9)     RETURN WITH CONDITION                      00457803
         SPACE 1                                                        00457903
PARMBAD  LA    R2,MGPRMSYN                                              00458003
         B     PARMERR       SYNTAX ERROR                               00458103
PARMBVOL LA    R2,MGPRMVOL                                              00458203
         B     PARMERR                                                  00458303
PARMBMEM LA    R2,MGPRMMEM                                              00458403
         B     PARMERR                                                  00458503
PARMBDSN LA    R2,MGPRMDSN                                              00458603
PARMERR  LM    R3,R4,PARMPTRS  GET OLD VALUES FOR MESSAGE               00458703
         STM   R8,R9,PARMPTRS  STORE FOR NEXT PARM                      00458803
         LA    R5,PLBADPRM   SET ERROR MESSAGE                          00458903
         B     POSTPATE      GO TO PASS END                             00459003
         SPACE 1                                                        00459103
MGDUPDDN FDOPT NL,CC=C'0'                                               00459203
         FD    'DD'                                                     00459303
         FD    TIOEDDNM,PAD                                             00459403
         FD    'is a duplicate - skipped'                               00459503
         FD    *END                                                     00459603
MGDBADDD FDOPT NL,CC=C'0'                                               00459703
         FD    'DD'                                                     00459803
         FD    TIOEDDNM,PAD                                             00459903
         FD    'is not usable'                                          00460003
         FD    *END                                                     00460103
         SPACE 1                                                        00460203
PLBADPRM FD    'PARM error:',NL,PAD                                     00460303
         FD    0(R2),L'MGPRMSYN,DEB                                     00460403
         FD    '''',PADL                                                00460503
         FD    0(R4),(R3)                                               00460603
         FD    ''''                                                     00460703
         FD    *END                                                     00460803
MGPRMSYN DC    C'Syntax error    '                                      00460903
MGPRMVOL DC    C'Volume Serial   '                                      00461003
MGPRMMEM DC    C'Member Name     '                                      00461103
MGPRMDSN DC    C'Data Set Name   '                                      00461203
         SPACE 1                                                        00461303
MGCURPRM FDTM  FLAGS,FGDEBUG,BZ=MGCURPRX                                00461403
         FD    ' ',NL                                                   00461503
         FDSNAP SAVE,SAVEEND-SAVE,DUAL,NOOFFSET,BASE=0                  00461603
MGCURPRX FD    *END                                                     00461703
         SPACE 1                                                        00461803
MGNOSER  FDOPT NL,CC=C'0'                                               00461903
         FD    DSNAME,DEB,NL                                            00462003
         FD    'not found in Catalog',PAD                               00462103
         FD    *END                                                     00462203
         SPACE 1                                                        00462303
PLFOUND  FDOPT NL,CC=C'0'                                               00462403
         FD    DSNAME,DEB                                               00462503
         FD    'found on',PAD                                           00462603
         FD    SERIAL,DEB                                               00462703
         FD    MEMBER,PAD    UNIT NAME                                  00462803
         FDCLC DSNAME,TRUENAME,BE=PLFOUNDX                              00462903
         FD    'named',PAD                                              00463003
         FD    TRUENAME,DEB                                             00463103
PLFOUNDX FD    *END                                                     00463203
         SPACE 1                                                        00463303
PLFMIG   FDOPT NL,CC=C'0'                                       GP99062 00463403
         FD    DSNAME,DEB                                       GP99062 00463503
         FD    'migrated',PAD                                   GP99062 00463603
         FDCLI CATDEV+2,UCB3DACC,BNE=PLFMIGX                    GP99062 00463703
         FD    'to DASD'                                        GP99062 00463803
PLFMIGX  FD    *END                                             GP99062 00463903
         SPACE 1                                                        00464003
PLNOFMT1 FDOPT NL,CC=C'0'                                               00464103
         FD    DSNAME,DEB                                               00464203
         FD    'on',PAD                                                 00464303
         FD    SERIAL,DEB                                               00464403
         FD    MEMBER,PAD    UNIT NAME                                  00464503
         FD    'DSCB1 could not be found, return code',PAD              00464603
         FD    DB,4,I,DEB                                               00464703
         FD    ', reason code X'''                                      00464803
         FD    DB+4,4,HEX                                               00464903
         FD    ''''                                                     00465003
         FD    *END                                                     00465103
         SPACE 1                                                        00465203
PLBADMNT FDOPT NL,CC=C'0'                                               00465303
         FD    SERIAL,NL,DEB                                            00465403
         FD    MEMBER,PAD    UNIT NAME                                  00465503
         FD    'could not be mounted for',PAD                           00465603
         FD    DSNAME,DEB                                               00465703
         FD    *END                                                     00465803
         EJECT ,                                                        00465903
*        SCANNER SUBROUTINE                                             00466003
*        R0 - 1-4 STOP CHARACTERS, FOURTH IS DEFAULT (UNMATCHED)        00466103
*        R8 - RESIDUAL TEXT LENGTH                                      00466203
*        R9 - START SCAN ADDRESS                                        00466303
*                                                                       00466403
*        RETURN:                                                        00466503
*        R15 - LENGTH OF TEXT BEFORE HIT                                00466603
*        R0  - ADDRESS OF HIT CHARACTER OR END OF TEXT+1                00466703
*        R1  - ADDRESS OF TEXT (R9) BEFORE HIT                          00466803
*        R2  - HIT CHARACTER OR DEFAULT                                 00466903
*        R8, R9 UPDATED TO POINT PAST HIT CHARACTER                     00467003
SCANNER  XC    SCANTAB,SCANTAB  CLEAR SCANNER TABLE                     00467103
         LR    R2,R0         COPY DEFAULT CHARACTER                     00467203
         ICM   R2,IIIO,ZEROES  CLEAR REMAINING BYTES                    00467303
         LA    R1,0(R8,R9)   POINT TO HIT COLUMN DEFAULT                00467403
         STM   R3,R7,20+3*4(R13)  SAVE WORK REGISTERS                   00467503
         LA    R5,4          REPEAT THIS THING FOR FOUR BYTES           00467603
SCANNERC LR    R4,R0         COPY CHRACTERS                             00467703
         SRL   R0,8          SHIFT REQUEST TO NEXT ONE                  00467803
         ICM   R4,IIIO,ZEROES  CLEAR UNUSED BYTES                       00467903
         LA    R6,SCANTAB(R4)  GET ADDRESS OF BYTE INSERTION            00468003
         STC   R4,0(,R6)     ENTER INTO TABLE                           00468103
         BCT   R5,SCANNERC   REPEAT FOR ALL FOUR CHARACTERS             00468203
         SPACE 1                                                        00468303
         LTR   R3,R8         GET LENGTH TO SCAN                         00468403
         BNP   SCANNER0      TOO BAD                                    00468503
         EX    R3,SCANNTRT   LOOK FOR SOMETHING                         00468603
SCANNER0 LR    R15,R1        COPY HIT COLUMN                            00468703
         SR    R15,R9        LESS START = LENGTH OF TEXT BEFORE         00468803
         LR    R0,R1         SAVE HIT ADDRESS FOR USER                  00468903
         LR    R1,R9         SAVE START SCAN ADDRESS                    00469003
         LR    R9,R0         SET NEW TEXT ADDRESS(HIT)                  00469103
         SR    R8,R15        RESIDUAL LENGTH                            00469203
         LTR   R2,R2         ANYTHING HIT ?                             00469303
         BZ    SCANNERX      NO                                         00469403
         BCTR  R8,0          SET RESIDUAL LENGTH                        00469503
         LA    R9,1(,R9)     SKIP OVER HIT CHARACTER                    00469603
SCANNERX LM    R3,R7,20+3*4(R13)  RESTORE OTHER REGISTERS               00469703
         BR    R14           RETURN TO CALLER                           00469803
SCANNTRT TRT   0(0,R9),SCANTAB  THE END IS NEAR ?                       00469903
MVCDSN   MVC   DSNAME(0),0(R1)  COPY USER'S DSN                         00470003
MVCVOL   MVC   SERIAL(0),0(R1)  VOLUME SERIAL                           00470103
         SPACE 3                                                        00470203
*********************************************************************** 00470303
*                                                                     * 00470403
*    FORMAT THE DSCB 1 AND ANY DSCB 3 INFORMATION                     * 00470503
*                                                                     * 00470603
*********************************************************************** 00470703
         SPACE 1                                                        00470803
FORMDSCB STM   R0,R15,SUBSAVE     JUST IN CASE - SAVE EVERYTHING        00470903
         XC    IECSDSL3(DS3END-IECSDSL3),IECSDSL3  CLEAR 3              00471003
         MVC   DATECRD,=C'(YYYY.DDD)'                                   00471103
         MVC   DATEREF,=C'(YYYY.DDD)'                                   00471203
         MVC   DATEEXP,=C'(YYYY.DDD)'                                   00471303
*WRONG   CLI   DS1NOEPV,3    MORE THAN THREE EXTENTS ?                  00471403
*DUE TO  BH    *+10          YES                                        00471503
*LABELS  XC    DS1PTRDS,DS1PTRDS  ELSE PREVENT FMT 2 READ               00471603
         SR    R1,R1                                                    00471703
         ICM   R1,7,DS1LSTAR  GET TRACK PORTION OF HWM          GP09186 00471803
         BZ    SETMAXTK      NO, SKIP ROUND                     GP09186 00471903
         SRL   R1,8                                             GP09186 00472003
         LA    R1,1(,R1)     ROUND UP                                   00472103
SETMAXTK STH   R1,NUMTRK     AND SAVE IT                                00472203
         LA    R0,DS1DSORG   POINT TO DSORG/OPTCD/RECFM.....            00472303
         LA    R1,IECSDSL3   USE DSCB 3 AREA AS WORK SPACE              00472403
         SUBCALL SUBDSATR    MAKE ATTRIBUTES PRINTABLE                  00472503
         MVC   RECLN,22(R1)  RECORD LENGTH                              00472603
*EXTRA*  MVC   BLKSI,17(R1)  BLOCKSIZE                                  00472703
         LA    R1,DS1CREDT   GET CREATION DATE                          00472803
         SUBCALL SUBD2J      MAKE JULIAN                                00472903
         STCM  R0,15,DATECRD+1  SAVE YEAR                               00473003
         STCM  R1,15,DATECRD+5  SAVE .DAY                               00473103
         LA    R1,DS1EXPDT   GET EXPIRATION DATE                        00473203
         SUBCALL SUBD2J      MAKE JULIAN                                00473303
         STCM  R0,15,DATEEXP+1  SAVE YEAR                               00473403
         STCM  R1,15,DATEEXP+5  SAVE .DAY                               00473503
         LA    R1,DS1REFD    GET EXPIRATION DATE                        00473603
         SUBCALL SUBD2J      MAKE JULIAN                                00473703
         STCM  R0,15,DATEREF+1  SAVE YEAR                               00473803
         STCM  R1,15,DATEREF+5  SAVE .DAY                               00473903
         MVC   CCHHR(5),DS1PTRDS  SAVE FMT2/FMT3 ADDRESS                00474003
         SLR   R3,R3         TRACK COUNT                                00474103
         SLR   R4,R4         EXTENT COUNT                               00474203
         IC    R4,DS1NOEPV   NUMBER OF EXTENTS                          00474303
         CLI   DS1EXT1+XTTYPE-XTWHOLE,XTTLABEL  USER LABELS ?           00474403
         BNE   *+8                                                      00474503
         LA    R4,1(,R4)     PHYSICAL EXTENTS                           00474603
         LA    R1,DS1EXT1    GET FIRST EXTENT                           00474703
         LA    R0,3          MAX OF 3                                   00474803
EX1      LH    R2,HIGHTRK    TRACKS/CYLINDER                            00474903
         SUBCALL SUBXTSUM    GET TOTAL TRACKS                           00475003
         AR    R3,R15        ACCUMULATE TRACKS                          00475103
EX2      OC    CCHHR(5),CCHHR  DSCB EXTENSION ?                         00475203
         BZ    DOEXT         NO                                         00475303
         OBTAIN CAMSEEK3     GET FORMAT 2 OR 3 DSCB                     00475403
         MVC   CCHHR(5),DS3PTRDS  SET NEXT READ ADDRESS                 00475503
         CLI   DS3FMTID,C'3'     A TYPE 3 ?                             00475603
         BNE   EX2           NO, SHOULD BE TWO, TRY AGAIN               00475703
         MVC   DS3FMTID(L'DS3ADEXT),DS3ADEXT  MAKE ENTRIES CONTIGUOUS   00475803
         LA    R1,DS3EXTNT   GET FIRST SET                              00475903
         LA    R0,13         THIRTEEN EXTENTS AVAILABLE                 00476003
         B     EX1           DO THE REMAINING EXTENTS                   00476103
         SPACE                                                          00476203
DOEXT    ST    R3,TRKS       SAVE TRACKS IN DATA SET                    00476303
         ST    R4,EXTS       EXTENTS IN DATA SET                        00476403
         CLI   DS1DSORG+1,0       PDS ?                         GP09186 00476503
         BNE   FORMEXIT      NO; IGNORE                         GP09186 00476603
         TM    DS1DSORG,X'42'  DSORG = PS OR PO ?                       00476703
         BZ    FORMEXIT      NO, NO % DISPLAY                           00476803
         LTR   R5,R5         PATTERN DSCB, SPACE = 0  ??                00476903
         BNP   FORMEXIT      YES, SKIP SPACE DISPLAY                    00477003
         LH    R1,NUMTRK     GET TRACKS USED                            00477103
         SLR   R0,R0         FOR DIVIDE                                 00477203
         MH    R1,=H'100'    FOR %                                      00477303
         DR    R0,R3         DIVIDE USED BY ALLOCATED TRACKS            00477403
         LTR   R1,R1         NON-ZERO PERCENTAGE ?                      00477503
         BP    PERCON        YES; DISPLAY                               00477603
         LTR   R0,R0         ANY REMAINDER ?                            00477703
         BZ    PERCON        NO                                         00477803
         LA    R1,1          SHOW SOME USE                              00477903
PERCON   ST    R1,PCNT       SHOW PERCENTAGE                            00478003
FORMEXIT PRTLIST MSGDSCB     FORMAT THE INFORMATION                     00478103
         LM    R0,R15,SUBSAVE   RESTORE REGISTERS                       00478203
         BR    R14           RETURN TO CALLER                           00478303
         SPACE 1                                                        00478403
MSGDSCB  FDOPT NL,CC=C'0'    DOUBLE SPACE                               00478503
         FD    DSNAME,PADR                                              00478603
         FD    'on'                                                     00478703
         FD    SERIAL,PAD                                               00478803
         FD    MEMBER,PAD    GENERIC OR DEVICE TYPE                     00478903
         FDOPT IND=3         INDENT SUBSEQUENT LINES                    00479003
         FDCLC DSNAME,TRUENAME,BE=MGGUDNAM                              00479103
         FD    '  alias of',PAD                                         00479203
         FD    TRUENAME,DEB                                             00479303
*        LINE 2                                                         00479403
MGGUDNAM FD    'Created',TURQ,RADJ,NL,DEB                               00479503
         FD    DS1CREDT,HEX,RADJ,DEB,PAD,LEN=5                          00479603
         FD    DATECRD,PAD                                              00479703
         FD    'on',TURQ,PAD                                            00479803
         FD    DS1DSSN,DEB,NULL,UPPER,PAD                               00479903
         FD    'seq',PAD                                                00480003
         FD    DS1VOLSQ,I,PAD,LEN=4                                     00480103
         FD    'System code',TURQ,RADJ,LEN=13                           00480203
         FD    DS1SYSCD,DEB,UPPER,NULL,PAD                              00480303
         FD    'Expires',TURQ,RADJ,LEN=9                                00480403
         FD    DS1EXPDT,HEX,RADJ,DEB,PAD,LEN=5                          00480503
         FD    DATEEXP,PAD                                              00480603
         FD    'Last ref',TURQ,RADJ,PAD,LEN=10                          00480703
         FDINP DS1REFD,HEX,DEB,RADJ,GREEN,LEN=5  ACCESS DATE            00480803
         FD    DATEREF,PAD                                              00480903
         SPACE 1                                                        00481003
*        LINE 3                                                         00481103
         FD    'DSOrg',TURQ,NL                                          00481203
         FDINP DS1DSORG,HEX,GREEN,LEN=4                                 00481303
         FDFLAG DS1DSORG,PAD,TABLE=TABDSORG,SEP=0,LEN=3                 00481403
         FD    'RecFm',RADJ,LEN=9                                       00481503
         FDINP DS1RECFM,HEX,DEB,GREEN,LEN=2                             00481603
         FDFLAG DS1RECFM,PAD,TABLE=TABRECFM,SEP=0,LEN=5                 00481703
         FD    'LRecL',RADJ,LEN=9                                       00481803
         FDINP DS1LRECL,I,GREEN,LEN=5                                   00481903
         FD    'BlkLn',TURQ,RADJ,LEN=9                                  00482003
         FDINP DS1BLKL,I,GREEN,LEN=5                                    00482103
         FD    'KeyLn',TURQ,RADJ,LEN=9                                  00482203
         FDINP DS1KEYL,I,GREEN,LEN=3                                    00482303
         FD    'RelKeyPos',TURQ,RADJ,LEN=13                             00482403
         FD    DS1RKP,I,PAD                                             00482503
         SPACE 1                                                        00482603
*        LINE 4                                                         00482703
         FD    'LStar',NL                                               00482803
         FDINP DS1LSTAR,HEX,GREEN,LEN=6                                 00482903
         FD    'TrkBal',TURQ,RADJ,LEN=12                                00483003
         FDINP DS1TRBAL,IA,GREEN,LEN=5                                  00483103
         FD    'OptCode',RADJ,LEN=13                                    00483203
         FDINP DS1OPTCD,HEX,GREEN,LEN=2                                 00483303
         FDTM  DS1DSORG,DS1DSGDA,BZ=OPTNOTDA                            00483403
         FDFLAG DS1OPTCD,PAD,TABLE=TABOPTDA,SEP=C',',LEN=64             00483503
         FDGOTO OPTNOTAM                                                00483603
OPTNOTDA FDTM  DS1DSORG,DS1DSGIS,BZ=OPTNOTIS                            00483703
         FDFLAG DS1OPTCD,PAD,TABLE=TABOPTIS,SEP=0,LEN=64                00483803
         FDGOTO OPTNOTAM                                                00483903
OPTNOTIS FDTM  DS1DSORG,DS1DSGPS+DS1DSGPO,BZ=OPTNOTPS                   00484003
         FDFLAG DS1OPTCD,PAD,TABLE=TABOPTPS,SEP=0,LEN=64                00484103
         FDGOTO OPTNOTAM                                                00484203
OPTNOTPS FDTM  DS1DSORG+1,DS1ORGAM,BZ=OPTNOTAM                          00484303
         FDFLAG DS1OPTCD,PAD,TABLE=TABOPTAM,SEP=0,LEN=64                00484403
OPTNOTAM DS    0X                                                       00484503
         SPACE 1                                                        00484603
*        LINE 5                                                         00484703
         FD    'DS Ind',TURQ,NL                                         00484803
         FDINP DS1DSIND,HEX,GREEN,LEN=2                                 00484903
         FDFLAG DS1DSIND,PAD,TABLE=TABDSIND,SEP=C',',LEN=64             00485003
         SPACE 1                                                        00485103
*        LINE 5                                                         00485203
         FD    'Sec.Alloc',NL,RADJ                                      00485303
         FD    DS1SCAL1,HEX,PAD,LEN=2                                   00485403
         FDCLI DS1SCAL1,0,BE=ABSTRK                                     00485503
         FD    DS1SCAL3,I,RADJ,PAD,LEN=8                                00485603
         FDFLAG DS1SCAL1,PAD,TABLE=TABSCAL1,SEP=C',',LEN=16             00485703
         FDGOTO COMTRK                                                  00485803
ABSTRK   FDINP DS1SCAL3,HEX,LEN=6                                       00485903
COMTRK   DS    0X                                                       00486003
         FDTM  DS1DSORG,X'02',BZ=NODEBLK                                00486103
         FD    DS1NOBDB,I,RADJ,PAD,LEN=5                                00486203
         FD    'Bytes in last Dir Blk',TURQ,LEN=25                      00486303
NODEBLK  DS    0X                                                       00486403
         SPACE 1                                                        00486503
*        LINE 6                                                         00486603
         FD    TRKS,I,RADJ,NL,GREEN,LEN=10                              00486703
         FD    'Tracks allocated, in',TURQ,PAD                          00486803
         FD    EXTS,I,PAD                                               00486903
         FD    'extent(s)'                                              00487003
         FDTM  DS1DSORG,X'42',BZ=NOPCNT                                 00487103
         FD    PCNT,I,RADJ,DEB,LEN=6                                    00487203
         FD    '% used.',TURQ                                           00487303
         SPACE 1                                                        00487403
*        LINE 15                                                        00487503
NOPCNT   FDOPT NL            FORCE LINE COMPLETION                      00487603
         FD    *END                                                     00487703
         SPACE 1                                                        00487803
TABDSORG FLGTAB X'4008',ICF,MLEN=2                              GP09186 00487903
         FLGTAB X'8000',IS                                              00488003
         FLGTAB X'4000',PS                                              00488103
         FLGTAB X'2000',DA                                              00488203
         FLGTAB X'1000',CX                                              00488303
         FLGTAB X'0800',CQ                                              00488403
         FLGTAB X'0400',MQ                                              00488503
         FLGTAB X'0200',PO                                              00488603
         FLGTAB X'0100',U                                               00488703
         FLGTAB X'0080',GS                                              00488803
         FLGTAB X'0040',TX                                              00488903
         FLGTAB X'0020',TQ                                              00489003
         FLGTAB X'0010','??'                                            00489103
         FLGTAB X'0008',AM                                              00489203
         FLGTAB X'0004',TR                                              00489303
         FLGTAB X'0002','??'                                            00489403
         FLGTAB X'0001','??'                                            00489503
         FLGTAB X'0000','none'                                          00489603
         FLGTAB *END                                                    00489703
         SPACE 1                                                        00489803
TABRECFM FLGTAB X'C0',U,MLEN=1     DEFINE RECORD FORMAT                 00489903
         FLGTAB X'80',F                                                 00490003
         FLGTAB X'40',V                                                 00490103
         FLGTAB X'20',T                                                 00490203
         FLGTAB X'10',B                                                 00490303
         FLGTAB X'08',S                                                 00490403
         FLGTAB X'04',A                                                 00490503
         FLGTAB X'02',M                                                 00490603
         FLGTAB *END                                                    00490703
         SPACE 1                                                        00490803
TABOPTDA FLGTAB X'80','WrtValCk',MLEN=1                                 00490903
         FLGTAB X'40','TkOvr'                                           00491003
         FLGTAB X'20','ExtSrch'                                         00491103
         FLGTAB X'10','Fdbck'                                           00491203
         FLGTAB X'08','ActAdr'                                          00491303
         FLGTAB X'04','DynBuf'                                          00491403
         FLGTAB X'02','RdExc'                                           00491503
         FLGTAB X'01','RelBkAdr'                                        00491603
         FLGTAB *END                                                    00491703
         SPACE 1                                                        00491803
TABOPTIS FLGTAB X'80',W,MLEN=1                                          00491903
         FLGTAB X'40',U                                                 00492003
         FLGTAB X'20',M                                                 00492103
         FLGTAB X'10',I                                                 00492203
         FLGTAB X'08',Y                                                 00492303
         FLGTAB X'04','?'                                               00492403
         FLGTAB X'02',L                                                 00492503
         FLGTAB X'01',R                                                 00492603
         FLGTAB *END                                                    00492703
         SPACE 1                                                        00492803
TABOPTPS FLGTAB X'80',W,MLEN=1                                          00492903
         FLGTAB X'40',U                                                 00493003
         FLGTAB X'20',C                                                 00493103
         FLGTAB X'10',H                                                 00493203
         FLGTAB X'08',B                                                 00493303
         FLGTAB X'04',Z                                                 00493403
         FLGTAB X'02',T                                                 00493503
         FLGTAB X'01',J                                                 00493603
         FLGTAB *END                                                    00493703
         SPACE 1                                                        00493803
TABOPTAM FLGTAB X'80','Cat in ICF ctlg'                                 00493903
         FLGTAB X'40','Is icf ctlg'                                     00494003
         FLGTAB *END                                                    00494103
         SPACE 1                                                        00494203
TABDSIND FLGTAB X'80','LastVol',MLEN=1                                  00494303
         FLGTAB X'40',RACF                                              00494403
         FLGTAB X'20',Blk8                                              00494503
         FLGTAB X'14','Write Pswd'                                      00494603
         FLGTAB X'10','Pswd Rd/Wt'                                      00494703
         FLGTAB X'02','Changed'                                         00494803
         FLGTAB *END                                                    00494903
         SPACE 1                                                        00495003
TABSCAL1 FLGTAB X'C0','Cyl',MLEN=1                                      00495103
         FLGTAB X'80','Trk'                                             00495203
         FLGTAB X'40','Blk'                                             00495303
         FLGTAB X'20','MSVGp'                                           00495403
         FLGTAB X'08','Contig'                                          00495503
         FLGTAB X'04','MXIG'                                            00495603
         FLGTAB X'02','ALX'                                             00495703
         FLGTAB X'01','Round'                                           00495803
         FLGTAB X'00','AbsTrk'                                          00495903
         FLGTAB *END                                                    00496003
         SPACE 1                                                        00496103
HEXTRAB  DC    C'0123456789ABCDEF'  HEX TRANSLATE                       00496203
         SPACE 2                                                        00496303
PATDSCB1 CAMLST SEARCH,TRUENAME-SAVE,SERIAL-SAVE,DS1FMTID-SAVE          00496403
PATSEEK3 CAMLST SEEK,CCHHR-SAVE,SERIAL-SAVE,IECSDSL3-SAVE               00496503
PATLOCAT CAMLST NAME,TRUENAME-SAVE,,CATWORK-SAVE  LOCATE CATALOG        00496603
ZEROES   DC    F'0'          CONSTANCE                                  00496703
BLANKS   DC    CL80' '       CONSTANCE                                  00496803
SYSPRINT PRTWORK SYSPRINT,*USER,TITLE=5  TSO OR BATCH OUTPUT            00496903
         SPACE 1                                                        00497003
HEADER   FDOPT NL,CC=C'#'    REQUEST AUTOMATIC PAGINATION               00497103
         FD    'DATA SET CONTROL BLOCK INFORMATION'                     00497203
         FDTM  FLAGS,FGDEBUG,BZ=HEADERD                                 00497303
         FD    '(DEBUG MODE)',PAD                                       00497403
HEADERD  FD    *END                                                     00497503
         SPACE 1                                                        00497603
SAVE     DSECT ,                                                        00497703
SUBSAVE  DS    16A           SUBROUTINE SAVE AREA                       00497803
DB       DS    D                                                        00497903
DB2      DS    D                                                        00498003
         SERVDEFS ,          SERVICE MODULES, ETC.                      00498103
@TIOT    DC    A(0)          CURRENT TIOT ENTRY BEING PROCESSED         00498203
FLAGS    DC    X'00'         PROCESSING FLAGS                           00498303
FGDEBUG  EQU   X'80'           DEBUG MODE (SNAPS, ETC.)                 00498403
FGONE    EQU   X'01'           AT LEAST ONE DS SPECIFIED                00498503
DATECRD  DC    C'(YYYY.DDD)'                                            00498603
DATEEXP  DC    C'(YYYY.DDD)'                                            00498703
DATEREF  DC    C'(YYYY.DDD)'                                            00498803
CAMDSCB1 CAMLST SEARCH,DSNAME,SERIAL,DS1FMTID                           00498903
CAMSEEK3 CAMLST SEARCH,CCHHR,SERIAL,IECSDSL3                            00499003
CAMLOCAT CAMLST NAME,TRUENAME,,CATWORK  LOCATE CATALOG ENTRIES          00499103
PARMPTRS DS    2A            LENGTH/ADDRESS OF RESIDUAL PARM TEXT       00499203
HIGHTRK  DS    H             TRACKS / CYLINDER                          00499303
NUMTRK   DS    H             HOLD AREA FOR LSTAR TRACK COUNT            00499403
         SPACE 1                                                        00499503
WORKBLK  DS    0C            START OF BLANKING                          00499603
DSNAME   DC    CL44' '       DATASET NAME FROM PARM                     00499703
TRUENAME DC    CL44' '       NAME AFTER CATALOG LOOKUP                  00499803
SERIAL   DC    CL6' '        USER'S VOLUME SERIAL                       00499903
MEMBER   DC    CL8' '        MEMBER NAME FOR PDS MEMBER DELETE          00500003
SPFLAG   DC    C' '          SPACE PROCESSING FLAG                      00500103
RECLN    DC    CL5' '        RECORD LENGTH                              00500203
SPLIT    DC    C' '          SPLIT EXTENT FLAG                          00500303
WORKBLKN EQU   *-WORKBLK     LENGTH TO MAKE BLANK                       00500403
         SPACE 1                                                        00500503
WORKCLR  DS    0D            START OF ZEROING                           00500603
TRKS     DC    F'0'          TRACKS IN DATASET                          00500703
EXTS     DC    F'0'          EXTENTS                                    00500803
PCNT     DC    F'0'          PERCENT USED                               00500903
CATWORK  DS    35D'0'        CATALOG WORK/RETURN AREA                   00501003
CATVOL#  EQU   CATWORK,2,C'H'  NUMBER OF VOLUMES                        00501103
CATDEV   EQU   CATWORK+2,4,C'X'  CATALOG DEVICE TYPE                    00501203
CATVOL   EQU   CATWORK+6,6,C'C'  VOLUME SERIAL                          00501303
         ORG   CATWORK                                                  00501403
         SPACE 1                                                        00501503
CCHHR    DS    XL5           ADDRESS OF FORMAT 3 DSCB                   00501603
WORKCLRN EQU   *-WORKCLR     LENGTH TO ZERO                             00501703
         SPACE 1                                                        00501803
         IECSDSL1 1          MAP FORMAT 1                               00501903
         AIF   (&MVSXA).SYSDEF                                  GP04234 00502003
DS1FLAG1 EQU   DS1NOBDB+1,1,C'X'  MORE FLAGS                            00502103
DS1COMPR EQU   X'80'           COMPRESSABLE EXTENDED IF DS1STRP         00502203
DS1CPOIT EQU   X'40'           CHECKPOINTED D S                         00502303
DS1SMSFG EQU   DS1FLAG1+17,1,C'X'  SMS FLAG                             00502403
DS1SMSDS EQU   X'80'           SMS D S                                  00502503
DS1SMSUC EQU   X'40'           NO BCS ENTRY                             00502603
DS1REBLK EQU   X'20'           MAY BE REBLOCKED                         00502703
DS1CRSDB EQU   X'10'           BLKSZ BY DADSM                           00502803
DS1PDSE  EQU   X'08'           PDS/E                                    00502903
DS1STRP  EQU   X'04'           EXTENDED FORMAT D S                      00503003
DS1PDSEX EQU   X'02'           HFS D S                                  00503103
DS1DSAE  EQU   X'01'           EXTENDED ATTRIBUTES EXISY                00503203
DS1SCEXT EQU   DS1SMSFG+1,3,C'X'  SECONDARY SPACE EXTENSION             00503303
DS1SCXTF EQU   DS1SCEXT,1,C'X'  -"- FLAG                                00503403
DS1SCAVB EQU   X'80'           SCXTV IS AVG BLOCK LEN                   00503503
DS1SCMB  EQU   X'40'                 IS IN MEGBYTES                     00503603
DS1SCKB  EQU   X'20'                 IS IN KILOBYTES                    00503703
DS1SCUB  EQU   X'10'                 IS IN BYTES                        00503803
DS1SCCP1 EQU   X'08'           SCXTV COMPACTED BY 256                   00503903
DS1SCCP2 EQU   X'04'                 COMPACTED BY 65536                 00504003
DS1SCXTV EQU   DS1SCXTF+1,2,C'X'  SEC SPACE EXTNSION VALUE              00504103
DS1ORGAM EQU   DS1ACBM         CONSISTENT NAMING - VSAM D S             00504203
DS1RECFF EQU   X'80'           RECFM F                                  00504303
DS1RECFV EQU   X'40'           RECFM V                                  00504403
DS1RECFU EQU   X'C0'           RECFM U                                  00504503
DS1RECFT EQU   X'20'           RECFM T   001X XXXX IS D                 00504603
DS1RECFB EQU   X'10'           RECFM B                                  00504703
DS1RECFS EQU   X'08'           RECFM S                                  00504803
DS1RECFA EQU   X'04'           RECFM A                                  00504903
DS1RECMC EQU   X'02'           RECFM M                                  00505003
*   OPTCD DEFINITIONS   BDAM    W.EFA..R                                00505103
*                       ISAM    WUMIY.LR                                00505203
*             BPAM/BSAM/QSAM    WUCHBZTJ                                00505303
DS1OPTIC EQU   X'80'  FOR DS1ORGAM - CATLG IN ICF CAT                   00505403
DS1OPTBC EQU   X'40'           ICF CATALOG                              00505503
DS1RACDF EQU   DS1IND40                                                 00505603
DS1SECTY EQU   DS1IND10                                                 00505703
DS1WRSEC EQU   DS1IND04                                                 00505803
DS1SCAL1 EQU   DS1SCALO,1,C'X'    SEC. ALLOC FLAGS                      00505903
DS1DSPAC EQU   X'C0'         SPACE REQUEST MASK                         00506003
DS1CYL   EQU   X'C0'           CYLINDER BOUND                           00506103
DS1TRK   EQU   X'80'           TRACK                                    00506203
DS1AVRND EQU   X'41'           AVG BLOCK + ROUND                        00506303
DS1AVR   EQU   X'40'           AVG BLOCK LEN                            00506403
DS1MSGP  EQU   X'20'                                                    00506503
DS1EXT   EQU   X'10'           SEC. EXTENSION EXISTS                    00506603
DS1CONTG EQU   X'08'           REQ. CONTIGUOUS                          00506703
DS1MXIG  EQU   X'04'           MAX                                      00506803
DS1ALX   EQU   X'02'           ALX                                      00506903
DS1DSABS EQU   X'00'           ABSOLUTE TRACK                           00507003
DS1SCAL3 EQU   DS1SCAL1+1,3,C'X'  SEC ALLOC QUANTITY                    00507103
.SYSDEF  SPACE 1                                                        00507203
         IECSDSL1 3          MAP FORMAT 3                               00507303
         SPACE 1                                                        00507403
SCANTAB  DC    XL256'00'     TRT SCANNING TABLE                         00507503
SAVEEND  EQU   *                                                        00507603
         SPACE 1                                                        00507703
         PRINT NOGEN                                                    00507803
         CVT   DSECT=YES                                                00507903
         SPACE 1                                                        00508003
         IHACDE ,                                                       00508103
         SPACE 1                                                        00508203
         IHAPSA ,            LOW                                        00508303
         SPACE 1                                                        00508403
         IKJTCB ,                                                       00508503
         SPACE 1                                                        00508603
         IEFUCBOB ,                                                     00508703
         SPACE 1                                                        00508803
         IEFJFCBN ,                                                     00508903
         SPACE 1                                                        00509003
         IHADVCT ,           FOR BLOCKS/TRACK                           00509103
         SPACE 1                                                        00509203
MYTIOT   DSECT  ,                                                       00509303
         IEFTIOT1 ,                                                     00509403
         SPACE 1                                                        00509503
         MAPEXTNT ,          SINGLE EXTENT MAPPING                      00509603
         END   ,                                                        00509703
/*                                                                      00509803
//*------------------------------------------------------ ASM: SHOWDSCB 00509903
//*                                                                     00510003
//ASM2.SYSIN DD *                                                       00510103
SUBDSATR TITLE 'S U B D S A T R  ***  FORMAT DCB/DSCB ATTRIBUTES'       00510203
         COPY  OPTIONGB                                                 00510303
         SPACE 1                                                        00510403
         SYSPARM LIST=YES                                               00510503
         SPACE 2                                                        00510603
*    FORMAT DSORG/OPTCD/RECFM/LRECL/BLKSIZE                             00510703
*    INPUT R0 POINTS TO JFCB OR DSCB1 DSORG FIELD                       00510803
*    USER'S R1 POINTS TO OUTPUT FIELD :                                 00510903
*      CL3 DSORG CL6 RECFM CL8 OPTCD CL5 BLKL CL5 LRECL                 00511003
         SPACE 1                                                        00511103
SUBDSATR START 0                                                        00511203
         USING SUBDSATR,R15  SET BY CALLER                              00511303
         B     BEGIN                                                    00511403
         DC    AL1(BEGIN-*),C'SUBDSATR &SYSDATE'                        00511503
BEGIN    STM   R14,R8,12(R13)                                           00511603
         LTR   R7,R0         ANY INPUT?                                 00511703
         BZR   R14           NO; TOO BAD - RETURN R15 NON-ZERO          00511803
         LTR   R8,R1         ANY OUTPUT?                                00511903
         BZR   R14           NO; TOO BAD - RETURN R15 NON-ZERO          00512003
         LA    R6,10*4+20(,R13)  USE SAVE AREA FOR WORK                 00512103
         N     R6,=X'7FFFFFF8'   USE DOUBLE-WORD BOUNDARY               00512203
         USING WORKSECT,R6   RE-USE SAVE AREA                           00512303
         USING FMTDSECT,R8   USER'S OUTPUT AREA                         00512403
         MVI   FMTDSORG,C' '                                            00512503
         MVC   FMTDSORG+1(FMTORGLN-1),FMTDSORG  CLEAR RETURN AREA       00512603
         USING DS1DSORG,R7   USE COMMON DSCB1/JFCB MAPPING              00512703
         SPACE 1                                                        00512803
         SR    R0,R0                                                    00512903
         MVI   FMTLRECL+L'FMTLRECL-1,C'X'  PRESET FOR X                 00513003
         ICM   R0,3,DS1LRECL   GET THE RECORD LENGTH                    00513103
         BM    DSATRRLN      SKIP LRECL=X                               00513203
         CVD   R0,DB         MAKE PACKED                                00513303
         MVC   FMTLRECL(5),=X'2020202120'  MAKE EDIT MASK               00513403
         ED    FMTLRECL-1(6),DB+5  EDIT RECORD LENGTH                   00513503
         SPACE 1                                                        00513603
DSATRRLN ICM   R0,3,DS1BLKL  GET BLOCK SIZE                             00513703
         CVD   R0,DB                                                    00513803
         MVC   FMTBLKL(5),=X'2020202120'  MAKE EDIT MASK                00513903
         ED    FMTBLKL-1(6),DB+5                                        00514003
         SPACE 1                                                        00514103
         LA    R1,PATDSORG   POINT TO LIST OF DSORGS                    00514203
         LA    R2,16         LOOP THROUGH 16 BITS                       00514303
         SR    R3,R3                                                    00514403
*  LOADING BACKWARDS, SO 'U' ATTRIBUTE LAST                             00514503
         ICM   R3,2,DS1DSORG     FIRST DSORG SECOND                     00514603
         ICM   R3,4,DS1DSORG+1   SECOND DSORG FIRST                     00514703
         SLL   R3,7          ADJUST FOR BXLE                            00514803
         OR    R3,R2         MAKE NON-ZERO LOW BYTE                     00514903
         LA    R4,FMTDSORG   SET OUTPUT FIELD                           00515003
         MVI   FMTDSORG,C'?'   PRESET IF ALL BITS ARE OFF               00515103
DSATROGL BXH   R3,R3,DSATROGN  SKIP IF BIT OFF                          00515203
         MVC   0(2,R4),0(R1)   MOVE ABBREVIATION                        00515303
         LA    R4,FMTDSORG+2   SET SECOND POSITION (TWO BITS AT MOST?)  00515403
DSATROGN LA    R1,2(,R1)     SKIP TO NEXT ABBREVIATION                  00515503
         BCT   R2,DSATROGL   TRY AGAIN                                  00515603
         SPACE 1                                                        00515703
         LA    R4,FMTRECFM   POINT TO OUTPUT FIELD                      00515803
         TM    DS1DSORG+1,DS1DSGTQ  TCAM MESSY QUEUE ?           83100  00515903
         BZ    *+10          NO                                  83100  00516003
         MVC   DB+3(2),PATRECMQ  YES; ADJUST RECFM               83100  00516103
         MVI   FMTRECFM,C'?'  PRESET IF NOTHING FOUND                   00516203
         MVC   DB(6),PATRECFM+4  COPY MINOR OPTIONS                     00516303
         SR    R1,R1                                                    00516403
         IC    R1,DS1RECFM   GET RECORD FORMAT                          00516503
         SRA   R1,6          ISOLATE MAJOR TYPE                         00516603
         BP    DSATRFMD      OK                                         00516703
         MVI   DB,C'D'       CHANGE FOR POSSIBLE D FORMAT (?)           00516803
         B     DSATRFMB                                                 00516903
DSATRFMD LA    R1,PATRECFM(R1)  POINT TO MAJOR TYPE                     00517003
         MVC   0(1,R4),0(R1)  MOVE IT                                   00517103
         LA    R4,1(,R4)     UP OUTPUT ADDRESS                          00517203
DSATRFMB IC    R1,DS1RECFM   GET FULL FORMAT AGAIN                      00517303
         LA    R2,5          SET FOR 5 ADDITIONAL OPTIONS               00517403
         LA    R3,DB         POINT TO ABBREVIATIONS                     00517503
         SLL   R1,25         ADJUST FOR BXLE                            00517603
         OR    R1,R2         MAKE LOW NON-ZERO                          00517703
DSATRFML BXH   R1,R1,DSATRFMN                                           00517803
         MVC   0(1,R4),0(R3)  MOVE ABBREVIATION                         00517903
         LA    R4,1(,R4)                                                00518003
DSATRFMN LA    R3,1(,R3)                                                00518103
         BCT   R2,DSATRFML   DO ALL                                     00518203
         SPACE 1                                                        00518303
         LA    R3,PATOPTPS   SET QSAM/BSAM COMMON                       00518403
         TM    DS1DSORG,DS1DSGPO+DS1DSGPS  PS/PO ?                      00518503
         BNZ   DSATROPT      YES; USE IT                                00518603
         LA    R3,PATOPTDA   SET DA                                     00518703
         TM    DS1DSORG,DS1DSGDA  IS IT BDAM ?                          00518803
         BNZ   DSATROPT      YES; USE IT                                00518903
         LA    R3,PATOPTIS   SET ISAM                                   00519003
         TM    DS1DSORG,DS1DSGIS  IS IT ISAM ?                          00519103
         BZ    DSATREX       NO; DO COMMON EXIT CODE                    00519203
DSATROPT LA    R4,FMTOPTCD   POINT TO OUTPUT FIELD                      00519303
         SR    R1,R1                                                    00519403
         IC    R1,DS1OPTCD   GET OPTIONS                                00519503
         LA    R2,8          SET FOR EIGHT OPTIONS                      00519603
         SLL   R1,23         ADJUST FOR BXLE                            00519703
         OR    R1,R2         MAKE LOW NON-ZERO                          00519803
DSATROPL BXH   R1,R1,DSATROPN                                           00519903
         MVC   0(1,R4),0(R3)  MOVE ABBREVIATION                         00520003
         LA    R4,1(,R4)                                                00520103
DSATROPN LA    R3,1(,R3)                                                00520203
         BCT   R2,DSATROPL   DO ALL                                     00520303
DSATREX  LM    R14,R8,12(R13)  RELOAD ALTERED REGISTERS                 00520403
         SR    R15,R15       SET GOOD RETURN                            00520503
         BR    R14           RETURN TO USER                             00520603
         SPACE 1             RETURN OK                                  00520703
PATDSORG DC    C'GSTXTQ? AMTR? ? ISPSDACXCQMQPOU '                      00520803
PATRECMQ DC    C'GR'         TCAM MESSAGE QUEUE                  83100  00520903
PATRECFM DC    C' VFUTBSAM '                                            00521003
PATOPTPS DC    C'WUCHQZTJ'   PS/PO COMMON OPTCD                         00521103
PATOPTDA DC    C'WOEFA84R'   DA OPTCD - 2 RESERVED                      00521203
PATOPTIS DC    C'WUMIY4LR'   IS OPTCD - 1 RESERVED, U NOT USED          00521303
         SPACE 1                                                        00521403
WORKSECT DSECT ,             USER'S SAVE AREA                           00521503
DB       DS    D             CONVERSION WORD                            00521603
         SPACE 1                                                        00521703
FMTDSECT DSECT ,             MAP OF USER'S AREA                         00521803
FMTDSORG DC    CL3' '        DSATR - DSORG                              00521903
FMTRECFM DC    CL6' '        DSATR - RECFM                              00522003
FMTOPTCD DC    CL8' '        DSATR - OPTCD                              00522103
FMTBLKL  DC    CL5' '        DSATR - BLKSIZE                            00522203
FMTLRECL DC    CL5' '        DSATR - LRECL                              00522303
FMTORGLN EQU   *-FMTDSORG    LENGTH OF LIST                             00522403
         SPACE 1                                                        00522503
         PRINT &PRTSYS                                                  00522603
         IECSDSL1 1                                                     00522703
         SPACE 1                                                        00522803
         YREGS ,                                                        00522903
         END   ,                                                        00523003
/*                                                                      00523103
//*------------------------------------------------------ ASM: SUBDSATR 00523203
//*                                                                     00523303
//ASM3.SYSIN DD *                                                       00523403
DTD2J    TITLE 'S U B D 2 J   **   JULIAN DATE FROM DSCB DATE'          00523503
*   THIS SUBROUTINE CONVERTS A DATE FIELD FROM A DSCB TO A FULL         00523603
*   YEAR AND JULIAN DAY FORMAT.                                         00523703
*     R1 - POINTS TO DSCB DATE FIELD                                    00523803
*                                                                       00523903
*   OUTPUT R15 = 0F  IF INVALID DATE PASSED                             00524003
*          R15 = YYYYDDDF IN HEX                                        00524103
*        R0-R1 = YYYY.DDD IN EBCDIC                                     00524203
*                                                                       00524303
SUBD2J   START 0                                                        00524403
SUBD2J   AMODE ANY                                                      00524503
SUBD2J   RMODE ANY                                                      00524603
         STM   R14,R12,12(R13)                                          00524703
         LR    R6,R15        AND NEW BASE                               00524803
         USING SUBD2J,R6                                                00524903
         LA    R4,7+20+8*4(,R13)  POINT INTO USER'S SAVE AREA           00525003
         N     R4,=X'7FFFFFF8'  MAKE DOUBLE-WORD BOUNDARY               00525103
         USING USERSAVE,R4   MAKE THIS CODE RE-ENTRANT/REFRESHABLE      00525203
         LA    R5,DATEBAD    PRESET FOR FAST BRANCHES                   00525303
         LTR   R1,R1         DID USER SUPPLY A DATE?                    00525403
         BZR   R5            NO                                         00525503
         SPACE 2                                                        00525603
*   TEST FOR VALID DSCB FORMAT DATE - RETURN 0 IF BAD                   00525703
*                                                                       00525803
         ICM   R0,7,0(R1)    ALL ZERO ?                                 00525903
         BZR   R5            RETURN ZEROES                              00526003
         CLC   0(3,R1),BLANKS  REALLY OLD FORMAT ?                      00526103
         BER   R5            YES; RETURN                                00526203
         SR    R2,R2                                                    00526303
         IC    R2,0(,R1)     LOAD YEAR                                  00526403
         LA    R2,1900(,R2)  ADD CENTURY                                00526503
         CH    R2,=H'1964'   PLAUSIBLE YEAR ?                   GP05158 00526603
         BNL   NOFIX         YES                                GP05158 00526703
         LA    R2,100(,R2)   FINAGLE                            GP05158 00526803
NOFIX    SR    R3,R3                                                    00526903
         ICM   R3,3,1(R1)    LOAD JULIAN DAY                            00527003
         BNPR  R5            NOT VALID                                  00527103
         CH    R3,=H'366'    IN RANGE ?                                 00527203
         BHR   R5            NO; RETURN 0                               00527303
         BL    DATEFORM      YES; FORMAT                                00527403
         EX    R2,EXISLEAP   LEAP YEAR ?                                00527503
         BNZR  R5            NO; 366 IS INVALID IN NON-LEAP             00527603
DATEFORM MH    R2,=H'1000'   MULTIPLY YEAR BY 1000                      00527703
         AR    R2,R3         ADD DAYS TO YEAR                           00527803
         CVD   R2,DB         MAKE PACKED                                00527903
         OI    DB+7,X'0F'    MAKE PREFERRED PLUS SIGN                   00528003
         ICM   R15,15,DB+4   RELOAD SIGNED, PACKED VALUE                00528103
         MVC   PACKWORK,PACKPAT                                         00528203
         ED    PACKWORK,DB+4                                            00528303
         ICM   R0,15,PACKWORK+1  LOAD YEAR                              00528403
         ICM   R1,15,PACKWORK+5  LOAD .DDD                              00528503
         SPACE 1                                                        00528603
PGMEXIT  LM    R2,R6,28(R13)  RESTORE REST                              00528703
         BR    R14           RETURN TO CALLER                           00528803
         SPACE 1                                                        00528903
*   DATE FIELD IS BAD OR 0 - RETURN ZERO                                00529003
DATEBAD  LA    R15,X'0F'     ZERO SIGNED PACKED RETURN                  00529103
         ICM   R0,15,BLANKS                                             00529203
         ICM   R1,15,EBZERO-3  MAKE CL8'   0'                           00529303
         B     PGMEXIT       COMMON EXIT                                00529403
         SPACE 1                                                        00529503
EXISLEAP TM    =X'03',*-*    LEAP YEAR ?                                00529603
BLANKS   DC    CL4' '  1/2                                              00529703
EBZERO   DC    C'0'    2/2   W/BLANKS+1 - ZERO PRINTABLE                00529803
PACKPAT  DC    X'40212020204B202020'                                    00529903
         LTORG ,                                                        00530003
         SPACE 1                                                        00530103
         PRINT NOGEN                                                    00530203
         SPACE 1                                                GP04075 00530303
USERSAVE DSECT ,             MY WORK AREA MAPPED ON USER'S SAVE AREA    00530403
PACKWORK DS    C' YYYY.DDD'                                             00530503
         ORG   PACKWORK+8    OVERLAP A LITTLE                           00530603
DB       DS    D                                                GP04075 00530703
         SPACE 1                                                        00530803
         YREGS ,                                                        00530903
         END   ,                                                        00531003
/*                                                                      00531103
//*------------------------------------------------------ ASM: SUBD2J   00531203
//*                                                                     00531303
//ASM4.SYSIN DD *                                                       00531403
SUBXTSUM TITLE 'S U B X T S U M  ***  ADD UP DSCB EXTENT SIZES'         00531503
         COPY  OPTIONGB                                                 00531603
         SPACE 1                                                        00531703
         SYSPARM LIST=YES                                               00531803
         SPACE 2                                                        00531903
*********************************************************************** 00532003
**                                                                   ** 00532103
**  SUBXTSUM - ACCUMULATE CONSECUTIVE EXTENT SIZES                   ** 00532203
**                                                                   ** 00532303
**  R0 - NUMBER OF EXTENTS IN GROUP                                  ** 00532403
**  R1 - ADDRESS OF FIRST EXTENT                                     ** 00532503
**  R2 - NUMBER OF TRACKS PER CYLINDER FOR THIS DEVICE               ** 00532603
**                                                                   ** 00532703
**  R15 - 0 ON ERROR, ELSE NUMBER OF TRACKS                          ** 00532803
**                                                                   ** 00532903
*********************************************************************** 00533003
SUBXTSUM START 0                                                        00533103
         USING SUBXTSUM,R15                                             00533203
         B     BEGIN                                                    00533303
         DC    AL1(17),CL17'SUBXTSUM &SYSDATE'                          00533403
BEGIN    STM   R14,R12,12(R13)   SAVE CALLER'S REGISTERS                00533503
         DROP  R15                                                      00533603
         LR    R12,R15                                                  00533703
         USING SUBXTSUM,R12                                             00533803
         SPACE 1                                                        00533903
         SR    R15,R15       ZERO SIZE TO DATE                          00534003
         SR    R9,R9         CLEAR FOR ICM                              00534103
         USING MAPEXTNT,R1   DECLARE EXTENT MAPPING                     00534203
DVEXTSUB CLI   XTWHOLE,0     UNUSED EXTENT ?                            00534303
         BE    PGMEXIT       YES; TOO BAD                               00534403
         SR    R11,R11       CLEAR FOR ICM                              00534503
         ICM   R11,3,XTHICYL   GET END CYLINDER                         00534603
         ICM   R9,3,XTLOCYL    AND START CYLINDER                       00534703
         SR    R11,R9        LESS START CYLINDER                        00534803
         BM    PGMEXIT       FAIL BAD RANGE                             00534903
         LH    R5,XTHITRK    GET HIGH TRACK                             00535003
         SH    R5,XTLOTRK    LESS START TRACK                           00535103
         AH    R5,=Y(1)      PLUS ONE FOR RELATIVITY                    00535203
         CLI   XTTYPE,XTTSPLIT  SPLIT EXTENT ?                          00535303
         BNE   DVEXTSEQ      NO                                         00535403
         AH    R11,=Y(1)     GET CYLINDER COUNT                         00535503
         MR    R4,R11        GET TOTAL TRACKS                           00535603
         AR    R15,R5        ADD TO RUNNING TOTAL                       00535703
         B     DVEXTSUP      GET NEXT EXTENT                            00535803
DVEXTSEQ MR    R10,R2        CONVERT TO TRACKS                          00535903
         AR    R11,R5        ADD ODD TRACKS                             00536003
         AR    R15,R11       ACCUMULATE                                 00536103
DVEXTSUP LA    R1,XTLEN(,R1)   GET NEXT EXTENT                          00536203
         BCT   R0,DVEXTSUB   DO NEXT EXTENT                             00536303
         SPACE 1                                                        00536403
PGMEXIT  L     R14,12(,R13)  GET RETURN ADDRESS                         00536503
         LM    R0,R12,20(R13)  RESTORE OTHER REGISTERS                  00536603
         BR    R14           RETURN TO CALLER                           00536703
         SPACE 2                                                        00536803
         PRINT &PRTMAC                                                  00536903
         MAPEXTNT ,          MAP AN EXTENT ENTRY                        00537003
         SPACE 2                                                        00537103
         PRINT &PRTSYS                                                  00537203
         YREGS ,                                                        00537303
         END   ,                                                        00537403
/*                                                                      00537503
//*------------------------------------------------------ ASM: SUBXTSUM 00537603
//*                                                                     00537703
//ASM5.SYSIN DD *                                                       00537803
SUBLPALK TITLE 'S U B L P A L K  ***  LOCATE LPA RESIDENT CODE'         00537903
         COPY  OPTIONGB                                                 00538003
         SPACE 1                                                        00538103
         SYSPARM LIST=YES                                               00538203
         SPACE 1                                                        00538303
*********************************************************************** 00538403
*                                                                     * 00538503
*                                                                     * 00538603
*        COPYRIGHT 2003  EXPERT SYSTEM PROGRAMMING                    * 00538703
*                        176 OLD STAGE COACH ROAD                     * 00538803
*                        BRADFORD, VT 05033-8844                      * 00538903
*                                                                     * 00539003
*                    ALL RIGHTS RESERVED                              * 00539103
*                                                                     * 00539203
*********************************************************************** 00539303
         EJECT ,                                                        00539403
*********************************************************************** 00539503
**                                                                   ** 00539603
**  SUBLPALK  LOCATES A MODULE RESIDING IN AN LPA AREA               ** 00539703
**                                                                   ** 00539803
**  PARAMETER:  R1 POINTING TO A CL8 AREA CONTAINING THE DESIRED     ** 00539903
**    MODULE NAME.                                                   ** 00540003
**                                                                   ** 00540103
**  WHEN USED WITH THE LOADLPA MACRO, R0 IS EITHER 0 OR A DCB        ** 00540203
**    ADDRESS. WHEN THE DCB ADDRESS IS SUPPLIED, AND THE MODULE IS   ** 00540303
**    NOT IN LPA, A LOAD WILL BE ISSUED.                             ** 00540403
**                                                                   ** 00540503
*********************************************************************** 00540603
**                                                                   ** 00540703
**  NORMAL RETURN:                                                   ** 00540803
**    R0 : MODULE ENTRY POINT (WITH X'80' ON IF AM31)                ** 00540903
**    R1 : LENGTH                                                    ** 00541003
**    R15: 0 (MAIN MODULE)   4 (ALIAS)                               ** 00541103
**   AR0 : MODULE LOAD POINT (IF AVAILABLE), ELSE SAME AS R0         ** 00541203
**                                                                   ** 00541303
**  ERROR RETURNS IN R15:                                            ** 00541403
**                                                                   ** 00541503
**  16 - ERROR PROCESSING PARAMETER LIST, OR OTHER SEVERE PROBLEM    ** 00541603
**   8 - ERROR CSVQUERY OR LOAD                                      ** 00541703
**                                                                   ** 00541803
*********************************************************************** 00541903
         SPACE 1                                                        00542003
         PRINT &PRTSOR                                                  00542103
SUBLPALK PGMHEAD ZERO31,BASE=R12,PARM=R1,AM=ANY,RM=24,LOC=BELOW         00542203
         STM   R0,R1,CALLR0  SAVE CALLER'S PARMS                        00542303
         SPACE 1                                                        00542403
         OICC  16,RESULT=RETCODE  PROVISIONAL RETURN CODE               00542503
         LA    R5,0(,R1)     DID USER PASS A PARM AREA ?                00542603
         LTR   R5,R5                                                    00542703
         BZ    PGMEXIT       NO; FATAL BOO-BOO                          00542803
         SPACE 1                                                        00542903
         L     R3,CVTPTR     GET CVT IN R3 FOR IEAVVMSR                 00543003
         USING CVTMAP,R3                                                00543103
         SPACE 1                                                        00543203
         AIF   (NOT &MVSESA).NOCSVQ                             GP04234 00543303
         CSVQUERY INEPNAME=(R5),OUTLENGTH=@LENGTH,OUTEPA=@ENTRY,       *00543403
               OUTLOADPT=@LOAD,OUTATTR1=DB,OUTATTR2=DB+1,OUTATTR3=DB+2,*00543503
               RETCODE=RETCODE,MF=(E,MYQUERY)                           00543603
         CH    R15,=H'8'     SUCCESSFUL ?                               00543703
         BNL   TESTOLD       NO                                         00543803
         TM    DB,X'02'      ALIAS ENTRY ?                              00543903
         BZ    NOTALIAS      NO                                         00544003
         MVICC 4             SET WARNING FLAG                           00544103
NOTALIAS TM    DB+1,X'30'    ATTR2 AM31 OR AMANY ?                      00544203
         BZ    LOADTWO       NO; RETURN                         GP03330 00544303
         OI    @ENTRY,X'80'  SET FOR AM31 PREFERRED                     00544403
*---------------------------------------------------------------------* 00544503
*  NOTE THAT CSVQUERY (AS ISSUED) WILL FIND A MODULE IN LPA, ETC. AS  * 00544603
*  WELL AS IN JPA.                                                    * 00544703
*  WHEN THE USER PASSES A NON-ZERO DCB PARAMETER AND THE MODULE IS IN * 00544803
*  JPA, WE MUST ISSUE A LOAD FOR IT BECAUSE CALLER WILL EVENTUALLY    * 00544903
*  ISSUE A DELETE FOR IT.                                             * 00545003
*---------------------------------------------------------------------* 00545103
LOADTWO  TM    DB+2,X'40'    FOUND IN JPA ?                     GP03330 00545203
         BZ    SETLOAD       NO; RETURN LPA, ETC. ADDRESS       GP03330 00545303
         ICM   R4,15,CALLR0  DID USER SUPPLY A DCB PARAMETER ?  GP03330 00545403
         BZ    SETLOAD       NO; JUST RETURN                    GP03330 00545503
         B     TESTLOAD      YES; DO EXTRA LOAD                 GP03330 00545603
         SPACE 1                                                        00545703
TESTOLD  CH    R15,=H'20'    UNAVAILABLE ON THIS SYSTEM ?               00545803
         BNE   TESTLOAD      NO; SEE WHETHER USER WANTS LOAD            00545903
.NOCSVQ  SPACE 1                                                GP04234 00546003
*---------------------------------------------------------------------* 00546103
*   SCAN THE MLPA FOR THE REQUESTED MODULE                            * 00546203
*---------------------------------------------------------------------* 00546303
         PUSH  USING                                                    00546403
MLPALOOK ICM   R0,15,0(R5)                                              00546503
         ICM   R1,15,4(R5)                                              00546603
         L     R15,CVTQLPAQ  GET MLPA CDE CHAIN                         00546703
         USING CDENTRY,R15                                              00546803
MLPALOOP ICM   R15,15,CDCHAIN   GET NEXT CDE; ELSE TRY PLPA             00546903
         BZ    PLPALOOK                                                 00547003
         C     R1,CDNAME+4   MATCH ?                                    00547103
         BNE   MLPALOOP      NO; TRY NEXT                               00547203
         C     R0,CDNAME     FULL MATCH ?                               00547303
         BNE   MLPALOOP      NO; TRY NEXT                               00547403
         B     FOUNDLP2      JOIN COMMON                                00547503
         POP   USING                                                    00547603
         SPACE 1                                                        00547703
*---------------------------------------------------------------------* 00547803
*   SCAN THE PLPA FOR THE REQUESTED MODULE                            * 00547903
*---------------------------------------------------------------------* 00548003
         PUSH  USING                                                    00548103
PLPALOOK L     R7,CVTLPDSR   IEAVVMSR                                   00548203
         ICM   R0,15,0(R5)                                              00548303
         ICM   R1,15,4(R5)                                              00548403
         BASR  R14,R7    NOTE THAT R7-R9 ARE CLOBBERED                  00548503
           B   FOUNDLPA      MODULE FOUND                               00548603
         XC    @ENTRY(8),@ENTRY    SHOW NOT FOUND                       00548703
         MVICC 8,RESULT=RETCODE  SET LEVEL 8 ERROR                      00548803
         B     TESTLOAD                                                 00548903
         SPACE 1                                                        00549003
*---------------------------------------------------------------------* 00549103
*   HAVE A CDE OR LPDE - EXTRACT LOAD ADDRESS AND SIZE                * 00549203
*---------------------------------------------------------------------* 00549303
         USING CDENTRY,R15                                              00549403
FOUNDLPA LR    R15,R0        COPY CDE ADDRESS                           00549503
FOUNDLP2 MVICC 0             RESET THE RETURN CODE              GP04234 00549603
         ICM   R0,15,CDENTPT  LOAD ENTRY ADDRESS                        00549703
         ST    R0,@ENTRY     RETURN ENTRY ADDRESS                       00549803
         ST    R0,@LOAD      SET AS LOAD ADDRESS, ALSO                  00549903
         AIF   (NOT &MVSESA).NOAMF                              GP04234 00550003
         TM    CDATTR2,CDEANYM  AM ANY ?                                00550103
         BZ    LOOKMIN       NO                                         00550203
         OI    @ENTRY,X'80'  SET AM31 PREFERRED                         00550303
.NOAMF   ANOP  ,                                                GP04234 00550403
LOOKMIN  TM    CDATTR,CDMIN  MINOR ?                                    00550503
         BZ    GETXTLST      NO; GET EXTENT LIST                        00550603
         OICC  4             INDICATE ALIAS ENTRY                       00550703
         TM    CDATTR2,CDXLE  EXTENT LIST PRESENT ?             GP05303 00550803
         BNZ   GETXTLST      YES; DON'T NEED MAJOR              GP05303 00550903
         ICM   R15,15,CDXLMJP  GET POINTER TO MAJOR                     00551003
         BNZ   LOOKMIN                                                  00551103
         B     SETLOAD       RESTORE REGS                               00551203
GETXTLST L     R14,CDXLMJP   GET EXTENT LIST ADDRESS                    00551303
         USING XTLST,R14                                                00551403
         MVC   @LOAD,XTLMSBAD LOAD ADDRESS                              00551503
         MVC   @LENGTH,XTLMSBLA  LENGTH                                 00551603
         NI    @LENGTH,255-X'80'  RESET END OF LIST BIT                 00551703
         B     SETLOAD       JOIN COMMON                                00551803
         POP   USING         RESTORE ASSIGNMENTS                        00551903
         SPACE 1                                                        00552003
*---------------------------------------------------------------------* 00552103
*   USER WANTS A LOAD ISSUED WHEN DCB IS NON-ZERO.                    * 00552203
*     WHEN DCB (R0)<256, THEN USE DCB=0 ON LOAD                       * 00552303
*---------------------------------------------------------------------* 00552403
TESTLOAD ICM   R4,15,CALLR0  DID USER SUPPLY A DCB PARAMETER ?          00552503
         BZ    SETLOAD       NO; JUST RETURN ERROR CODE                 00552603
         CH    R4,=H'256'    VALID DCB ?                                00552703
         BNL   *+4+2         PERHAPS                                    00552803
         SR    R4,R4         ELSE FLAG TO REQUEST LOAD                  00552903
         LOAD  DCB=(R4),EPLOC=(R5),ERRET=SETLOAD                        00553003
         STM   R0,R1,@ENTRY   RETURN ENTRY / LENGTH                     00553103
         XC    RETCODE,RETCODE    CLEAR RETURN                          00553203
         ICM   R14,15,@LOAD  IS LOAD ADDRESS SET ?              GP03330 00553303
         BNZ   SETLOAD       YES; RETURN IT                     GP03330 00553403
         LR    R14,R0        COPY ENTRY ADDRESS                 GP03330 00553503
         LA    R14,0(,R14)   CLEAN AM BIT                       GP03330 00553603
         ST    R14,@LOAD     SET TO RETURN ENTRY AS LOAD        GP03330 00553703
         SPACE 1                                                        00553803
*---------------------------------------------------------------------* 00553903
*   RETURN LOAD ADDRESS IN AR0                                        * 00554003
*---------------------------------------------------------------------* 00554103
SETLOAD  DS    0H                                               GP04234 00554203
         AIF   (NOT &MVSESA).NOLAM                              GP04234 00554303
         LAM   R0,R0,@LOAD   GET LOAD POINT                             00554403
.NOLAM   SPACE 1                                                GP04234 00554503
*---------------------------------------------------------------------* 00554603
*   EXIT: WHEN R15=0, ENTRY ADD IN R0, LENGTH IN R1, LOAD ADDR IN AR0 * 00554703
*     R15=8 MODULE NOT FOUND; R15=16 INVALID NAME ADDRESS             * 00554803
*---------------------------------------------------------------------* 00554903
PGMEXIT  PGMEXIT COPYRET=(RETCODE,12)   RETURN R15, R0, R1              00555003
         SPACE 1                                                        00555103
         LTORG ,                                                        00555203
         SPACE 2                                                        00555303
SAVE     DSECT ,             SAVE & WORK AREA                           00555403
DB       DS    D                                                        00555503
CALLR0   DS    A     1/2                                                00555603
CALLR1   DS    A     2/2                                                00555703
         SERVDEFS ,          EXPAND COMMON STUFF                        00555803
         ORG   RETCODE+4     RETURN CODE                                00555903
@ENTRY   DS    A     2/3     RETURN ENTRY POINT ADDRESS                 00556003
@LENGTH  DS    F     3/3     RETURN LENGTH                              00556103
@LOAD    DS    A             RETURN LOAD POINT ADDRESS                  00556203
         SPACE 1                                                        00556303
         AIF   (NOT &MVSESA).SKPCVSQ                            GP04234 00556403
         CSVQUERY MF=(L,MYQUERY)                                        00556503
.SKPCVSQ SPACE 1                                                GP04234 00556603
SAVEEND  EQU   *             END OF DYNAMIC AREA                        00556703
         SPACE 2                                                        00556803
         PRINT &PRTSYS                                                  00556903
         CVT   DSECT=YES                                                00557003
         IHACDE ,                                                       00557103
         IHAXTLST ,                                                     00557203
         END   ,                                                        00557303
/*                                                                      00557403
//*------------------------------------------------------ ASM: SUBLPALK 00557503
//*                                                                     00557603
//LKED.SYSIN DD *                                                       00557703
  NAME SHOWDSCB(R)                                                      00557803
/*                                                                      00557903
//*------------------------------------------------------ LKED          00558003
//*                                                                     00558103
//LOADJCL.SYSIN DD DATA,DLM='><'                                        00558203
./ ADD NAME=SHOWDSCB                                                    00558303
./ NUMBER NEW1=100,INCR=100                                             00558403
//SHOWDSCB JOB (1),'SHOWDSCB',CLASS=A,MSGCLASS=X                        00558503
//SHOWDSCB EXEC PGM=SHOWDSCB                                            00558603
//SYSPRINT DD SYSOUT=*                    REQUIRED PRINT FILE           00558703
//SYSUT001 DD DISP=SHR,DSN=SYSC.LINKLIB   1ST FILE TO PRINT             00558803
//SYSUT002 DD DISP=SHR,DSN=SYSC.SORTLIB   2ND FILE TO PRINT             00558903
//                                                                      00559003
./ ENDUP                                                                00559103
><                                                                      00559203
/*                                                                      00559303
//*------------------------------------------------------ LOADJCL       00559403
//                                                                      00559503

