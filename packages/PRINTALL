//PRINTALL  JOB (TSO),
//             'Install PRINTALL',
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
//* THIS JOB INSTALLS THE PRINTALL LOAD MODULE & JOB.                 * 00000700
//********************************************************************* 00000800
//*                                                                     00000900
//INSTALL PROC SOUT='*',               <=== SYSOUT CLASS                00001000
//             LIB='SYSC.LINKLIB',     <=== TARGET LOAD LIBRARY         00001100
//             JCL='SYSC.JCLLIB',      <=== JCL LIBRARY                 00001200
//             SYSTS=SYSDA,            <=== UNITNAME FOR WORK DATASETS  00001300
//             ASMBLR=AFOX00,          <=== NAME OF YOUR ASSEMBLER      00001402
//             ALIB='SYSC.LINKLIB',    <=== LOCATION OF YOUR ASSEMBLER  00001500
//             MACLIB='SYS1.MACLIB',   <=== MACLIB DATASET NAME         00001600
//             AMODGEN='SYS1.AMODGEN'  <=== AMODGEN DATASET NAME        00001700
//*                                                                     00001800
//LOADMACS EXEC PGM=IEBUPDTE,PARM=NEW                                   00001900
//SYSPRINT DD  SYSOUT=&SOUT                                             00002000
//SYSUT2   DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(,PASS),                   00002100
//             SPACE=(TRK,(150,,50),RLSE),DCB=(SYS1.MACLIB)             00002200
//*                                                                     00002300
//ASM1    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00002400
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00002500
//MACREF   DD    DUMMY                                                  00002602
//SYSTERM  DD  SYSOUT=&SOUT                                             00002700
//SYSPRINT DD  SYSOUT=&SOUT                                             00002800
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00002900
//         DD  DSN=&AMODGEN,DISP=SHR                                    00003000
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00003100
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003200
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003300
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00003400
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(,PASS,DELETE),            00003500
//             SPACE=(TRK,(90,15))                                      00003600
//*                                                                     00003700
//ASM2    EXEC PGM=&ASMBLR,REGION=2048K,PARM=(NOLOAD,DECK,'LINECNT=55') 00003800
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00003900
//SYSTERM  DD  SYSOUT=&SOUT                                             00004000
//SYSPRINT DD  SYSOUT=&SOUT                                             00004100
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00004200
//         DD  DSN=&AMODGEN,DISP=SHR                                    00004300
//         DD  DSN=&&LCLMAC,UNIT=&SYSTS,DISP=(OLD,PASS)                 00004400
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00004500
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00004600
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00004700
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS)                 00004800
//*                                                                     00004900
//LKED    EXEC PGM=IEWL,COND=(0,NE),                                    00005000
//             PARM='XREF,LET,LIST,SIZE=(600K,64K)'                     00005100
//SYSPRINT DD  SYSOUT=&SOUT                                             00005200
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,10)                               00005300
//SYSLMOD  DD  DSN=&LIB,DISP=SHR                                        00005400
//SYSLIN   DD  DSN=&&SYSLIN,DISP=(OLD,DELETE)                           00005500
//         DD  DDNAME=SYSIN                                             00005600
//*                                                                     00005700
//LOADJCL  EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                       00005800
//SYSPRINT DD  SYSOUT=&SOUT                                             00005900
//SYSUT2   DD  DSN=&JCL,DISP=MOD                                        00006000
//*                                                                     00006100
//        PEND                                                          00006200
//*                                                                     00006300
//        EXEC INSTALL                                                  00006400
//*                                                                     00006500
//LOADMACS.SYSIN DD *                                                   00006600
./ ADD NAME=AMODE    8000-04314-04314-2100-00004-00004-00000-GERHARD 00 00006703
         MACRO ,                                                        00006803
         AMODE ,                                                        00006903
.*   DUMMY MACRO CREATED TO SUPPORT ASSEMBLY UNDFER HERCULES (XF ASM)   00007003
         MEND  ,                                                        00007103
./ ADD NAME=BALSR    8003-04319-08278-2306-00014-00015-00000-GERHARD 00 00007203
         MACRO ,                                                        00007303
&NM      BALSR &R,&S                                    ADDED ON 92281  00007403
.*                                                                      00007503
.*       THIS MODULE GENERATES EITHER                                   00007603
.*       A BALR OR BASR, DEPENDING ON THE SYSTEM. BAS AND BASR          00007703
.*       REQUIRE AT LEAST SP 2 (MVS/XA)                                 00007803
.*                                                                      00007903
         GBLB  &MVT          SET BY OPTIONGB/SYSPARM            GP04234 00008003
         AIF   (&MVT).BAL                                               00008103
&NM      DC    0H'0',AL.4(0,13,&R,&S)                                   00008203
         MEXIT ,                                                        00008303
.BAL     ANOP  ,                                                        00008403
&NM      BALR  &R,&S                                                    00008503
         MEND  ,                                                        00008603
./ ADD NAME=BAS      8005-04307-08278-0210-00013-00015-00000-GERHARD 00 00008703
         MACRO ,                                                        00008803
&NM      BAS   &R,&A                                    ADDED ON 04234  00008903
.*                                                                      00009003
.*       THIS MODULE GENERATES EITHER A BAL FOR MVS COMPATIBILIY        00009103
.*       BAS NEEDS AT LEAST SP 2 (MVS/XA) OR HERCULES 370               00009203
.*                                                                      00009303
         GBLB  &MVT                                                     00009403
&NM      BAL   &R,&A                                                    00009503
         AIF   (&MVT).MEND                                              00009603
         ORG   *-4                                                      00009703
         DC    X'4D'         BAS                                        00009803
         ORG   *+3                                                      00009903
.MEND    MEND  ,                                                        00010003
./ ADD NAME=BASR     8004-04307-08278-0212-00007-00015-00000-GERHARD 00 00010103
         MACRO ,                                                        00010203
&NM      BASR  &R,&S                                    ADDED ON 04234  00010303
.*                                                                      00010403
.*       THIS MODULE GENERATES A BASR FOR IFOX (MIN. HERC 370)          00010503
.*                                                                      00010603
&NM      DC    0H'0',AL.4(0,13,&R,&S)   BASR                            00010703
         MEND  ,                                                        00010803
./ ADD NAME=BASSM    8004-04315-10054-2023-00021-00015-00000-GERHARD 00 00010903
         MACRO ,                                                        00011003
&NM      BASSM &R,&A                                   ADDED ON GP04234 00011103
         GBLB  &MVSXA                                           GP10054 00011203
         GBLC  &MODEL                                           GP10054 00011303
.*                                                                      00011403
.*       THIS MODULE GENERATES A BASR FOR MVS COMPATIBILITY.            00011503
.*       REQUIRE AT LEAST SP 2 (MVS/XA) FOR HARDWARE SUPPORT            00011603
.*                                                                      00011703
.*R1   R2   BALR R1,R2                                                  00011803
.*0    R2   BALR 0,R2                                                   00011903
.*R1   0    BALR R1,0                                                   00012003
.*0    0    BALR 0,0                                                    00012103
.*                                                                      00012203
         AIF   ('&MODEL' EQ '380').BASSM                        GP10054 00012303
         AIF   (NOT &MVSXA).OLD                                 GP10054 00012403
.BASSM   ANOP  ,                                                GP10054 00012503
&NM      DC    0H'0',AL.4(0,12,&R,&A)   BASSM                   GP10054 00012603
         MEXIT ,                                                        00012703
.OLD     ANOP  ,                                                GP10054 00012803
&NM      DC    0H'0',AL.4(0,13,&R,&A)   BASR                    GP10054 00012903
         MEND  ,                                                        00013003
./ ADD NAME=BSM      8011-04315-10159-0146-00022-00010-00000-GERHARD 00 00013103
         MACRO ,                                                        00013203
&NM      BSM   &R,&A                                   ADDED ON GP04234 00013303
         GBLB  &MVSXA                                           GP08292 00013403
         GBLC  &MODEL                                           GP08292 00013503
.*                                                                      00013603
.*       THIS MODULE GENERATES A BALR FOR COMPATIBILITY                 00013703
.*                                                                      00013803
         AIF   ('&MODEL' EQ '380').BSM                          GP08292 00013903
         AIF   (NOT &MVSXA).OLD                                 GP08292 00014003
.BSM     ANOP  ,                                                GP08292 00014103
&NM      DC    0H'0',AL.4(0,11,&R,&A)   BSM                     GP08292 00014203
         MEXIT ,                                                        00014303
.OLD     AIF   ('&R(1)' EQ '0' OR '&R(1)' EQ 'R0').BR           GP08292 00014403
         AIF   ('&A(1)' EQ '0' OR '&A(1)' EQ 'R0').SETAM        GP10159 00014503
&NM      MACPARM MODE=LBL                                       GP08292 00014603
         MEXIT ,                                                GP08292 00014703
.SETAM   ANOP  ,                                                GP10159 00014803
&NM      LA    &R,0(,&R)     CLEAN HIGH BYTE (AM24)             GP10159 00014903
         MEXIT ,                                                GP10159 00015003
.BR      ANOP  ,                                                GP08292 00015103
&NM      BR    &A                                                       00015203
         MEND  ,                                                        00015303
./ ADD NAME=DBO      8005-05016-09183-1943-00328-00327-00000-GERHARD 00 00015403
         MACRO ,                                                        00015503
&NM      DBO   &LBL,&TEXT=,&REGS=YES,&HEX=,&MODE=S,     ADDED ON 85360 *00015603
               &WK=R9,&DEV=1,&TCB=,         WTO VS @PRT  CHANGED 94011 *00015703
               &ROUT=13,&DES=4,&BUGPARM=NO,                ADDED 95067 *00015803
               &CTEXT=,&PACK=,                             ADDED 96081 *00015903
               &PRTMODE=0,&DCB=0,   USER PRINT DCB/MODE    ADDED 99058 *00016003
               &COUNT=,&CALL=DYN,&OPT=,                  CHANGED 98222 *00016103
               &WA=DBTSAVE,                                ADDED 99114 *00016203
               &LIST=                                      ADDED 95235  00016303
.********************************************************************** 00016403
.*>>>>>>>>> KEPT FOR OLD CODE ONLY - NEW CODE SHOULD USE DBT <<<<<<<<<* 00016503
.********************************************************************** 00016603
.*                                                                    * 00016703
.*  THIS MACRO INVOKES EXTERNAL LOAD MODULE DEBTROLD TO PRODUCE       * 00016803
.*  TRACING, REGISTER CONTENTS, AND VARIABLES. (DEBTROLD SHOULD BE IN * 00016903
.*  A LINKLIB; AUTHORIZATION IS NOT REQUIRED). OUTPUT WILL BE BY WTO  * 00017003
.*  UNLESS A DEBTRACE DD CARD IS SUPPLIED.                            * 00017103
.*                                                                    * 00017203
.*  REQUIRED:  IN A CSECT OR RSECT:   DEBTRACE MODE=C  DEFINES CODE   * 00017303
.*             IN A CSECT OR DSECT:   DEBTRACE MODE=D  DEFINES DATA   * 00017403
.*             IN A CSECT (^RENT) :   DEBTRACE MODE=DC   BOTH         * 00017503
.*    (NOTE: REQUIRED FORMS MUST NOT APPEAR PRIOR TO FIRST OPTIONAL)  * 00017603
.*                                                                    * 00017703
.*  OPTIONAL:  LABEL DEBTRACE ...                                     * 00017803
.*                TAG OR ,  -  IDENTIFIER ON OUTPUT LISTING / CONSOLE * 00017903
.*                                                                    * 00018003
.*                REGS= (DEFAULT)   REGS=NO - NO REGISTERS            * 00018103
.*                REGS=YES  -  REGISTERS R0 THROUGH R15               * 00018203
.*                REGS=(R1,R2) - REGISTERS R1 THROUGH R2              * 00018303
.*                REGS=SHORT   - R14 THROUGH R1                       * 00018403
.*                                                                    * 00018503
.*                TEXT=NAME -  TEXT STRING TO BE SHOWN                * 00018603
.*                TEXT=(NAME,LEN) - TEXT W/EXPLICIT LENGTH            * 00018703
.*                                                                    * 00018803
.*                CTEXT=NAME - CONDITIONAL TEXT STRING TO BE SHOWN    * 00018903
.*                CTEXT=(NAME,LEN) - TEXT W/EXPLICIT LENGTH           * 00019003
.*                              OUTPUT IN HEX IF NOT PRINTABLE        * 00019103
.*                                                                    * 00019203
.*                HEX=NAME   -  DATA TO BE SHOWN IN HEXADECIMAL       * 00019303
.*                HEX=(NAME,LEN) - TEXT W/EXPLICIT LENGTH             * 00019403
.*                                                                    * 00019503
.*                PACK=NAME  -  DATA TO BE CONVERTED FROM PACKED      * 00019603
.*                PACK=(NAME,LEN) - TEXT W/EXPLICIT LENGTH (LEN IGNRD)* 00019703
.*                                                                    * 00019803
.*           LIST=((OP1,LN1,FM1),(OP2,LN2,FM2), ... )                 * 00019903
.*                                                                    * 00020003
.*                OP - ADDRESS EXPRESSION VALID IN S CONSTANT         * 00020103
.*                LN - LENGTH EXPRESSION; DEFAULT IS L'OP             * 00020203
.*                FM - TEXT   CTEXT   HEX   PACK - DEFAULT IS HEX     * 00020303
.*                     OR ABBREVIATED   T   CT   H   P                * 00020403
.*                                                                    * 00020503
.*  THE REQUIRED FORMS MAY BE OMITTED WHEN PGMTRACE WILL ALSO BE USED * 00020603
.*  AND ACTIVATED. IN THAT CASE THE FIRST OPTIONAL FORM MUST INCLUDE  * 00020703
.*  CALL=TRC TO GENERATE SHORTER PARAMETER LISTS.                     * 00020803
.*                                                                    * 00020903
.********************************************************************** 00021003
.*  MAINTENANCE:                                                      * 00021103
.*                                                                    * 00021203
.*  2000/01/03  GYP  REMOVED IN-LINE DEBUG CODE;                      * 00021303
.*                   FIXED MODE=C AND MODE=D FOR USE WITH REENTRANT   * 00021403
.*                     PROGRAMS.                                      * 00021503
.*                                                                    * 00021603
.********************************************************************** 00021703
     GBLB  &BUGBEAR,&BUGTCB,&BUGSWCH,&BUGSWRT,&BUGFAR,&BUGEXT,&BUGDYN   00021803
     GBLB  &BUGTRC,&BUGDBO   USED WITH ACTIVE PGMTRACE (ESPIE)  GP99113 00021903
         GBLA  &MACP#        NUMBER OF SUBLIST PARAMETERS       GP04234 00022003
         GBLC  &MACP1,&MACP2,&MACP3,&MACP4,&MACP5               GP04234 00022103
         GBLC  &MACP6,&MACP7,&MACP8,&MACP9,&MACP10              GP04234 00022203
         GBLC  &V                                                       00022303
         LCLA  &LN,&I,&EN,&EM,&EO                               GP95235 00022403
         LCLC  &L,&ET,&EL,&EK                                   GP95235 00022503
&L       SETC  'L'''                                            GP95235 00022603
&V       SETC  'DBT'.'&SYSNDX'                                          00022703
&BUGFAR  SETB  (&BUGFAR OR ('&CALL' EQ 'FAR'))                   95079  00022803
&BUGEXT  SETB  (&BUGEXT OR ('&CALL' EQ 'EXTRN'))                 95227  00022903
&BUGDYN  SETB  (&BUGDYN OR ('&CALL' EQ 'DYN'))                  GP97261 00023003
&BUGDYN  SETB  (&BUGDYN OR ('&CALL' EQ ''))  DROP LOCAL CODE    GP00004 00023103
&BUGDYN  SETB  (&BUGDYN OR ('&CALL' EQ 'DYNAMIC'))              GP97261 00023203
&BUGTRC  SETB  (&BUGTRC OR ('&CALL' EQ 'TRC'))                  GP99113 00023303
&BUGTRC  SETB  (&BUGTRC OR ('&CALL' EQ 'TRACE'))                GP99113 00023403
&BUGTRC  SETB  (&BUGTRC OR ('&CALL' EQ 'PGMTRACE'))             GP99113 00023503
         AIF   (&BUGBEAR OR '&BUGPARM' EQ 'NO').DOSOME                  00023603
         AIF   ('&NM' EQ '').MEND                                       00023703
&NM      DS    0H            DEBUG SWITCH NOT ON                        00023803
         AGO   .MEND                                                    00023903
.DOSOME  ANOP  ,                                                 95067  00024003
&BUGSWCH SETB  1                                                 95067  00024103
         AIF   ('&MODE' EQ 'D' OR '&MODE' EQ 'M').DATA           95228  00024203
         AIF   ('&MODE' EQ 'C').CODE                                    00024303
         AIF   ('&MODE' EQ 'DC').CODE   EXPAND BOTH              95067  00024403
         AIF   ('&MODE' EQ 'ON').SWON                            95079  00024503
         AIF   ('&MODE' EQ 'OFF').SWOFF                          95079  00024603
         AIF   ('&MODE' EQ 'CLOSE').SWEND  CLOSE AND QUIT       GP98222 00024703
&BUGDBO  SETB  1             DBO STATEMENT EXPANDED             GP09183 00024803
         AIF   (NOT &BUGTRC).NOTTRC                             GP99113 00024903
&NM      DC    X'83CD',S(&WA,&V.X-*)             INVOKE TRACE   GP99113 00025003
         AGO   .DONEBAS                                         GP99113 00025103
.NOTTRC  ANOP  ,                                                GP99113 00025203
&NM      STM   R0,R15,&WA    SAVE ALL REGISTERS                         00025303
         AIF   ('&COUNT' EQ '').DONECNT                          95079  00025403
.*  COUNT(3) - SKIP FIRST N CALLS                                95079  00025503
         AIF   ('&COUNT(3)' EQ '').CNTNO3                        95079  00025603
         ICM   R14,15,&V.3   LOAD SKIP COUNT                     95079  00025703
         BNP   &V.C          LIMIT REACHED - PROCESS             95079  00025803
         BCTR  R14,0         DECREMENT                           95079  00025903
         STCM  R14,15,&V.3   SAVE FOR NEXT TIME                  95079  00026003
         B     &V.X          AND SKIP CALL                       95079  00026103
&V.3     DC    AL4(&COUNT(3))  INITIAL SKIP COUNT                95079  00026203
&V.C     DS    0H                                                95079  00026303
.CNTNO3  AIF   ('&COUNT(2)' EQ '').CNTNO2                        95079  00026403
         AIF   ('&COUNT(2)' EQ '1').CNTNO2                       95079  00026503
         AIF   ('&COUNT(2)' EQ '0').CNTNO2                       95079  00026603
.*  COUNT(2) - PROCESS EVERY NTH CALL ONLY                       95079  00026703
         ICM   R14,15,&V.2   LOAD COUNTER                        95079  00026803
         BNP   &V.L          BAD - PROCESS CALL                  95079  00026903
         BCT   R14,&V.N      NON-ZERO; SAVE AND SKIP             95079  00027003
         MVC   &V.2,=AL4(&COUNT(2))  REFRESH                     95079  00027103
         B     &V.L          AND GO                              95079  00027203
&V.2     DC    AL4(1)        INTERVAL COUNTER (DO FIRST ONE)     95079  00027303
&V.N     STCM  R14,15,&V.2   UPDATE COUNTER                      95079  00027403
         B     &V.X          AND EXIT                            95079  00027503
.CNTNO2  AIF   ('&COUNT(1)' EQ '').DONECNT                       95079  00027603
         AIF   ('&COUNT(1)' EQ '0').DONECNT                      95079  00027703
         ICM   R14,15,&V.1   LOAD LIMIT COUNT                    95079  00027803
         BNP   &V.X          SKIP OUT IF NOT VALID               95079  00027903
         BCTR  R14,0         DECREMENT                           95079  00028003
         B     &V.M          SAVE, AND CONTINUE                  95079  00028103
&V.1     DC    AL4(&COUNT(1))  MAXIMUM CALLS TO MAKE             95079  00028203
&V.M     STCM  R14,15,&V.1   SAVE FOR NEXT TIME                  95079  00028303
.DONECNT ANOP  ,                                                 95079  00028403
&V.L     BAS   R1,&V.B                                           95079  00028503
.DONEBAS AIF   ('&LBL' EQ '' AND (&BUGEXT OR &BUGDYN OR &BUGTRC)).NOLBL 00028603
         DC    CL8'&LBL '                                               00028703
.NOLBL   AIF   ('&REGS' EQ '' OR '&REGS' EQ 'NO').NOREGS         95079  00028803
         AIF   ('&REGS' EQ 'R15' OR '&REGS' EQ 'SHORT'                 *00028903
               OR '&REGS' EQ 'RET').RETREG                      GP97225 00029003
         AIF   ('&REGS' EQ 'YES' OR '&REGS' EQ 'ALL').REGSALL   GP02246 00029103
         AIF   (N'&REGS EQ 2).REGS2                             GP97225 00029203
         DC    AL1(0,0),SL2(&REGS(1),&REGS(1))                  GP97225 00029303
         AGO   .NOREGS                                          GP97225 00029403
.REGS2   DC    AL1(0,0),SL2(&REGS(1),&REGS(2))                  GP97225 00029503
         AGO   .NOREGS                                          GP97225 00029603
.REGSALL DC    AL1(0,0),SL2(0,15)                               GP97225 00029703
         AGO   .NOREGS                                          GP97225 00029803
.RETREG  DC    SL2(0,14,1)    R15-R1 ONLY                       GP97225 00029903
.NOREGS  AIF   ('&TEXT' EQ '').NOTEXT                                   00030003
         AIF   (N'&TEXT GE 2).TEXT2                             GP97225 00030103
         DC    AL1(1,0),SL2(&TEXT(1)),AL2(&L&TEXT(1))           GP97225 00030203
         AGO   .NOTEXT                                          GP97225 00030303
.TEXT2   DC    AL1(1,0),SL2(&TEXT(1),&TEXT(2))                          00030403
.NOTEXT  AIF   ('&CTEXT' EQ '').NOCTEXT                         GP97225 00030503
         AIF   (N'&CTEXT GE 2).CTEXT2                           GP97225 00030603
         DC    AL1(2,0),SL2(&CTEXT(1)),AL2(&L&CTEXT(1))         GP97225 00030703
         AGO   .NOCTEXT                                         GP97225 00030803
.CTEXT2  DC    AL1(2,0),SL2(&CTEXT(1),&CTEXT(2))                GP97225 00030903
.NOCTEXT AIF   ('&HEX' EQ '').NOHEX                             GP97225 00031003
         AIF   (N'&HEX GE 2).HEX2                               GP97225 00031103
         DC    AL1(3,0),SL2(&HEX(1)),AL2(&L&HEX(1))             GP97225 00031203
         AGO   .NOHEX                                           GP97225 00031303
.HEX2    DC    AL1(3,0),SL2(&HEX(1),&HEX(2))                    GP97225 00031403
.NOHEX   AIF   ('&PACK' EQ '').NOPACK                           GP97225 00031503
         AIF   (N'&PACK GE 2).PACK2                             GP97225 00031603
         DC    AL1(4,0),SL2(&PACK(1)),AL2(&L&PACK(1))           GP97225 00031703
         AGO   .NOPACK                                          GP97225 00031803
.PACK2   DC    AL1(4,0),SL2(&PACK(1),&PACK(2))                  GP97225 00031903
.NOPACK  AIF   ('&LIST' EQ '' OR N'&LIST LT 1).NOLIST           GP95235 00032003
&LN      SETA  N'&LIST                                          GP95235 00032103
.DOLIST  AIF   (&I GE &LN).NOLIST   DONE WITH LIST              GP95235 00032203
&I       SETA  &I+1          BUMP LOOP INDEX                    GP95235 00032303
&EN      SETA  K'&EK         GET LENGTH                         GP04234 00032403
         MACLIST &LIST(&I)   GET SUBLIST ITEMS                  GP04234 00032503
&EN      SETA  &MACP#        NUMBER OF ENTRIES (CHG FOR XF ASM) GP04234 00032603
         AIF   (&EN LT 1).DOLIST  USER IN COMA?                 GP95235 00032703
         AIF   (&EN LT 4).TOOLIST WARN                          GP95235 00032803
         MNOTE 4,'MORE THAN 3 SUBPARMS IN &LIST(&I) '           GP95235 00032903
.TOOLIST ANOP  ,                                                GP95235 00033003
&EK      SETC  '&MACP1'                                         GP04234 00033103
&EM      SETA  K'&EK         LENGTH OF FIRST OPERAND                    00033203
&EO      SETA  0             PRESET FOR NORMAL ADDRESSING MODE          00033303
&ET      SETC  '03'          PRESET FOR HEX DEFAULT             GP95235 00033403
         AIF   (&EM GT 0).TPFX                                  GP04234 00033503
&EK      SETC  '0'           ALLOW EXPANSION WITHOUT ERROR      GP04234 00033603
&EM      SETA  1                                                GP04234 00033703
         MNOTE 4,'DEBTRACE: PARAMETER &I REQUIRES AN ADDRESS'   GP04234 00033803
.TPFX    AIF   (&EM LT 2).NOTA31                                GP04234 00033903
         AIF   ('&EK'(1,1) NE '/').NOTIND                               00034003
&EO      SETA  &EO+1         REQUEST INDIRECT ADDRESSING                00034103
&EK      SETC  '&EK'(2,&EM-1)  DELETE LEADING CONTROL BYTE              00034203
&EM      SETA  K'&EK         LENGTH OF FIRST OPERAND                    00034303
.NOTIND  AIF   ('&EK'(&EM,1) NE '%').NOTA24                             00034403
&EO      SETA  &EO+2         REQUEST FORCED 24-BIT ADDRESSING           00034503
&EK      SETC  '&EK'(1,&EM-1)  DELETE TRAILING CONTROL BYTE             00034603
&EM      SETA  K'&EK         LENGTH OF FIRST OPERAND                    00034703
.NOTA24  AIF   ('&EK'(&EM,1) NE '?').NOTA31                             00034803
&EO      SETA  &EO+4         REQUEST FORCED 31-BIT ADDRESSING           00034903
&EK      SETC  '&EK'(1,&EM-1)  DELETE TRAILING CONTROL BYTE             00035003
&EM      SETA  K'&EK         LENGTH OF FIRST OPERAND                    00035103
.NOTA31  AIF   (&EN LT 3 OR '&MACP3' EQ 'HEX').HTYPE            GP95235 00035203
         AIF   ('&MACP3' EQ 'X').HTYPE                          GP97225 00035303
         AIF   ('&MACP3' EQ 'HEX').HTYPE                                00035403
         AIF   ('&MACP3' EQ 'T').TTYPE                          GP98189 00035503
         AIF   ('&MACP3' EQ 'TEXT').TTYPE                       GP95235 00035603
         AIF   ('&MACP3' EQ 'TXT').TTYPE                                00035703
         AIF   ('&MACP3' EQ 'C').CTYPE                          GP97225 00035803
         AIF   ('&MACP3' EQ 'CT').CTYPE                                 00035903
         AIF   ('&MACP3' EQ 'CTEXT').CTYPE                      GP97225 00036003
         AIF   ('&MACP3' EQ 'PACK').PTYPE                       GP97225 00036103
         AIF   ('&MACP3' EQ 'PACKED').PTYPE                     GP97225 00036203
         AIF   ('&MACP3' EQ 'P').PTYPE                          GP97225 00036303
         AIF   ('&MACP3' EQ 'PD').PTYPE                                 00036403
         AIF   ('&MACP3' EQ 'D').PTYPE                          GP97225 00036503
 MNOTE 4,'TYPE MUST BE TEXT, CTEXT, HEX, OR PACKED, NOT &MACP3'         00036603
         AGO   .HTYPE                                           GP95235 00036703
.TTYPE   ANOP  ,                                                GP95235 00036803
&ET      SETC  '01'          SET FOR TEXT                       GP95235 00036903
         AGO   .HTYPE                                           GP95235 00037003
.CTYPE   ANOP  ,                                                GP97225 00037103
&ET      SETC  '02'          SET FOR CONDITIONAL TEXT, ELSE HEX GP97225 00037203
         AGO   .HTYPE                                           GP97225 00037303
.PTYPE   ANOP  ,                                                GP97225 00037403
&ET      SETC  '04'          SET FOR PACKED                     GP97225 00037503
.HTYPE   ANOP  ,                                                GP97225 00037603
&EL      SETC  '&MACP2'                                         GP95235 00037703
         AIF   ('&EL' NE '').HLEN                               GP95235 00037803
&EL      SETC  '&L'.'&EK'                                               00037903
.HLEN    DC    X'0800',CL8'&MACP1',AL1(&ET,&EO),S(&EK,&EL)              00038003
         AGO   .DOLIST                                          GP95235 00038103
.NOLIST  AIF   (&BUGFAR).FARCL                                   95079  00038203
         AIF   (&BUGDYN).FARCL                                  GP97261 00038303
         AIF   (&BUGTRC).TRCCL                                  GP99113 00038403
         AIF   (&BUGEXT).EXTCL                                   95215  00038503
&V.B     BAL   R14,DBTRACE                                       92271  00038603
         AGO   .CMCAL                                            95079  00038703
.EXTCL   ANOP  ,                                                 95215  00038803
&V.B     L     R15,=V(DEBTRACE)    MEMBER DEBTROLD              GP05013 00038903
         LA    R0,&WA        PASS ADDRESS OF WORK AREA           95215  00039003
         AGO   .FARCM                                            95215  00039103
.FARCL   ANOP  ,                                                 95079  00039203
&V.B     L     R15,=A(DBTRACE)                                   95079  00039303
.FARCM   BASR  R14,R15                                           95079  00039403
.CMCAL   ANOP  ,                                                 95079  00039503
&V.X     LM    R0,R15,&WA                                               00039603
         AGO   .MEND                                                    00039703
.TRCCL   ANOP  ,             INVOKE PGMTRACE VIA ESPIE          GP99113 00039803
&V.X     DS    0H            END OF TRACE LIST                  GP99113 00039903
         AGO   .MEND                                            GP99113 00040003
.SWON    OI    DBTFLAG,DBTFLON  SET TRACING ON                   95079  00040103
         AGO   .MEND                                             95079  00040203
.SWEND   OI    DBTFLAG,DBTFLEND  CLOSE DCB AND STOP TRACE       GP98222 00040303
.SWOFF   NI    DBTFLAG,255-DBTFLON  SET TRACING OFF              95079  00040403
         AGO   .MEND                                             95079  00040503
.CODE    AIF   (&BUGFAR OR &BUGEXT).TESTDC                      GP97262 00040603
         AIF   ('&NM' EQ '').NONAME                                     00040703
&NM      DS    0H                                                       00040803
.NONAME  AIF   (NOT &BUGDYN).NOLODYN                            GP97262 00040903
         AIF   ('&MODE' NE 'DC').NOPUP                          GP00004 00041003
         PUSH  PRINT                                            GP00004 00041103
         PUSH  USING                                            GP00004 00041203
.*       PRINT GEN                                              GP00004 00041303
         DROP  ,                                                GP00004 00041403
         USING DBTRACE,R15                                      GP97265 00041503
.NOPUP   ANOP  ,                                                GP00004 00041603
DBTRACE  LA    R0,&WA        PASS ADDRESS OF WORK AREA          GP97262 00041703
         STM   R12,R1,DBTLOCSV  SAVE BASE AND RETURN            GP97265 00041803
         ICM   R15,15,@DEBTRAC  SEE IF PREVIOUSLY LOADED        GP97265 00041903
         BNZR  R15           INVOKE; RETURN VIA R14 TO CALLER   GP97265 00042003
         AIF   ('&MODE' NE 'DC').NODRP                          GP00004 00042103
         BASR  R12,0         MAKE LOCAL BASE                    GP97262 00042203
         DROP  R15                                              GP97265 00042303
         USING *,R12                                            GP97265 00042403
         AGO   .CMDRP                                           GP00004 00042503
.NODRP   MVC   #DEBTRAC,=CL8'DEBTROLD'                          GP00004 00042603
.CMDRP   LOAD  EPLOC=#DEBTRAC  LOAD EXTERNAL MODULE             GP97261 00042703
         ST    R0,@DEBTRAC   SAVE FOR NEXT TIME                 GP97261 00042803
.*FAILS* AIF   ('&MODE' NE 'DC').NOCLB                          GP00004 00042903
         ST    R0,DBTLOCSV+(15-12)*4  UPDATE TARGET ADDRESS     GP97265 00043003
  MACPARM DBTFLAG,(&OPT),NULL=&BUGSWCH*DBTFLON+DBTFLWID,OP=MVI,OPR=MVI  00043103
.NOCLB   LM    R12,R1,DBTLOCSV  RESTORE                         GP97265 00043203
         BR    R15           RETURN TO CALLER VIA R14           GP97262 00043303
         AIF   ('&MODE' NE 'DC').NOPOP                          GP00004 00043403
         POP   USING                                            GP97262 00043503
         POP   PRINT                                            GP97262 00043603
.NOPOP   AGO   .TESTDC                                          GP97262 00043703
.NOLODYN MNOTE 4,'INLINE EXPANSION NOT SUPPORTED - USE MACRO DEBINLIN'  00043803
.TESTDC  AIF   ('&MODE' NE 'DC').MEND                            95067  00043903
         AGO   .NODRTE                                           95067  00044003
.DATA    AIF   ('&NM' EQ '').NODLBL                                     00044103
&NM      DS    0D                                                       00044203
.NODLBL  AIF   ('&ROUT' EQ '').NODRTE                            95067  00044303
&BUGSWRT SETB  1                                                 95067  00044403
.NODRTE  AIF   (&BUGFAR OR '&MODE' EQ 'M').MEND                  95230  00044503
DBTPRESV DC    2F'0'  1/2    FOR SHORT FORMATTING                95230  00044603
.NOSVPFX ANOP  ,                                                 95230  00044703
&WA      DC    16F'0' 2/2    DEBUG SAVE AREA                    GP97265 00044803
DBTFLAG  DC    AL1(&BUGSWCH*DBTFLON+DBTFLWID)  DEBUG FLAG       GP98222 00044903
DBTFLTCB EQU   128             INCLUDE TCB ADDRESS IN MESSAGE           00045003
DBTFLWTO EQU   64              USE WTO INSTEAD OF PRT            95240  00045103
DBTFLWID EQU   32              USE WIDE FORMAT WHEN PRINTING    GP98222 00045203
DBTFLPRO EQU   16              PRODUCTION MODE / NEED DD TO PRT GP99113 00045303
DBTFLEND EQU   2               THIS IS A TERMINATION CALL       GP98222 00045403
DBTFLON  EQU   1               DEBUG BIT                                00045503
DBTFLAG2 DC    AL1(0)        ..RESERVED..                       GP99062 00045603
DBTFLAG3 DC    AL1(0)        ..RESERVED..                       GP99062 00045703
DBTFLAG4 DC    AL1(0)        ..RESERVED..                       GP99062 00045803
DBTCNT1  DC    A(&COUNT(1)+0)  COUNT OPTION                      95228  00045903
DBTCNT2  DC    A(&COUNT(2)+0)  COUNT OPTION                      95228  00046003
DBTCNT3  DC    A(&COUNT(3)+0)  COUNT OPTION                      95228  00046103
         MAPCMPRT PFX=DBT,DCB=&DCB,PRTMODE=&PRTMODE,DEV=&DEV    GP99113 00046203
         AIF   (NOT &BUGDYN).NODYNS                             GP97261 00046303
.BUGDYN  WXTRN DEBTRACE      SUPPORT LINKED-IN VERSION          GP97262 00046403
@DEBTRAC DC    A(DEBTRACE)   ADDRESS OF LOADED DEBTRACE         GP97261 00046503
#DEBTRAC DC    CL8'DEBTROLD'  LOAD MODULE NAME                  GP97261 00046603
DBTLOCSV DC    6A(0)         SAVE AREA                          GP97265 00046703
         AGO   .MEND                                            GP97262 00046803
.NODYNS  AIF   (&BUGFAR OR '&MODE' EQ 'M').MEND                  95228  00046903
DBTLOCSV DC    4F'0'         BASE SAVE                                  00047003
DBTWTO   DC    Y(56,0)       VCON                                93357  00047103
DBTWTOM  DC    C'MSG666 '    DEBUG HEADER                        93357  00047203
DBTWTON  DC    CL8' ',C' '   USER'S LABEL                               00047303
DBTWTOT  DC    CL36' '       USER'S HEX OR EBCDIC TEXT                  00047403
         AIF   ('&TCB' NE 'YES' AND NOT &BUGTCB).NOTCB           94011  00047503
         DC    C' '          EXTRA FOR UNPACK                    94011  00047603
DBTWTCB  DC    CL8' '        CURRENT TCB ADDRESS                 94011  00047703
.NOTCB   ANOP  ,                                                 94011  00047803
DBTWTOC  DC    C' '          EXTRA FOR UNPACK                           00047903
         AIF   (NOT &BUGSWRT).MEND  NO ROUTING CODE              95067  00048003
         DC    XL3'0'        EXTRA FOR DESCRIPTOR/ROUTING CODES  95067  00048103
.MEND    MEND  ,                                                        00048203
./ ADD NAME=FD       0112-02308-10240-1607-00152-00130-00024-GERHARD 00 00048303
         MACRO                                                          00048403
&NM      FD    &STR,&EXLEN,&VALUES,&LEN=0,&TYPE=                GP03287 00048503
.*--------------------------------------------------------------------* 00048603
.*   TYPE= ADDED FOR FDFLAG AND FDBAR SUPPORT                         * 00048703
.*--------------------------------------------------------------------* 00048803
         GBLA  &FDCNTR                                                  00048903
         LCLA  &C,&I,&J,&K,&N                                           00049003
         LCLA  &T,&E,&O7,&O9,&DATA                                      00049103
         LCLC  &L,&FDCHAIN,&W                                           00049203
         LCLB  &NOP,&IN,&SKPLEN,&INDAD,&CNLOCK                   92086  00049303
         LCLB  &NL,&DEBL,&DEBR,&DEBZ,&PADL,&PADR,&RADJ,&UP              00049403
         LCLB  &BLUE,&GREEN,&PINK,&RED,&TURQ,&WHITE,&YELLOW,&C1,&C2,&C3 00049503
         LCLB  &UNDER,&BLINK,&REVERSE,&MDT,&MONO                 87313  00049603
         LCLB  &INTENSE,&DETECT,&NONDISP,&NUMERIC,&SKIP,&PROTECT        00049703
         LCLB  &DEFAULT,&PREV                                           00049803
&FDCNTR  SETA  &FDCNTR+1                                                00049903
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                          00050003
&C       SETA  &FDCNTR+1                                                00050103
         AIF   ('&NM' EQ '').NONAME                                     00050203
&NM      EQU   *                                                        00050303
.NONAME  AIF   ('&STR' NE 'END' AND '&STR' NE '*END').PROCESS           00050403
&FDCHAIN DC    AL1(0)        END OF FD LIST                             00050503
         MEXIT ,                                                        00050603
.PROCESS ANOP  ,                                                        00050703
&I       SETA  N'&SYSLIST                                               00050803
&J       SETA  1                                                        00050903
         AIF   ('&STR'(1,1) NE '''').PRMLOOP                    GP04048 00051003
&SKPLEN  SETB  1                                                        00051103
.*--------------------------------------------------------------------* 00051203
.*   LOOP THROUGH POSITIONAL PARAMETERS:                              * 00051303
.*   #1 - VARIABLE NAME OR QUOTED STRING                              * 00051403
.*   #2 - IF UNRECOGNIZED, EXPLICIT VARIABLE LENGTH                   * 00051503
.*--------------------------------------------------------------------* 00051603
.PRMLOOP AIF   (&J GE &I).CHECK                                 GP04048 00051703
&J       SETA  &J+1                                                     00051803
&L       SETC  '&SYSLIST(&J)'                                           00051903
         AIF   ('&L' EQ '').PRMLOOP                             GP04048 00052003
&NOP     SETB  (&NOP  OR '&L' EQ 'NOP')                                 00052103
&CNLOCK  SETB  (&CNLOCK OR '&L' EQ 'LOCK' OR '&L' EQ 'LOCKED')   92086  00052203
&NL      SETB  (&NL  OR '&L' EQ 'NL' OR '&L' EQ 'NEWLINE')              00052303
&DEBL SETB (&DEBL OR '&L' EQ 'DEBL' OR '&L' EQ 'DEB' OR '&L' EQ 'DEBZ') 00052403
&DEBR    SETB  (&DEBR OR '&L' EQ 'DEBR' OR '&L' EQ 'DEB')               00052503
&DEBZ    SETB  (&DEBZ OR '&L' EQ 'DEBZ')                                00052603
&PADL    SETB  (&PADL OR '&L' EQ 'PADL' OR '&L' EQ 'PAD')               00052703
&PADR    SETB  (&PADR OR '&L' EQ 'PADR' OR '&L' EQ 'PAD')               00052803
&RADJ    SETB  (&RADJ OR '&L' EQ 'RADJ')                                00052903
&UP    SETB  (&UP OR '&L' EQ 'UP')                                      00053003
&UP      SETB  (&UP OR '&L' EQ 'UPPER')                                 00053103
&BLUE    SETB  (&BLUE OR '&L' EQ 'BLUE')                                00053203
&GREEN   SETB  (&GREEN OR '&L' EQ 'GREEN')                              00053303
&PINK    SETB  (&PINK OR '&L' EQ 'PINK')                                00053403
&RED     SETB  (&RED  OR '&L' EQ 'RED')                                 00053503
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TURQ' OR '&L' EQ 'CYAN')      90326  00053603
&WHITE   SETB  (&WHITE OR '&L' EQ 'WHITE')                              00053703
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YELLOW')                            00053803
&BLUE    SETB  (&BLUE OR '&L' EQ 'BL')                          GP10240 00053903
&GREEN   SETB  (&GREEN OR '&L' EQ 'GR')                         GP10240 00054003
&PINK    SETB  (&PINK OR '&L' EQ 'PI')                          GP10240 00054103
&RED     SETB  (&RED  OR '&L' EQ 'RE')                          GP10240 00054203
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TU' OR '&L' EQ 'CY')         GP10240 00054303
&WHITE   SETB  (&WHITE OR '&L' EQ 'WH')                         GP10240 00054403
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YE')                        GP10240 00054503
&MONO    SETB  (&MONO OR '&L' EQ 'MONO')                         87313  00054603
&UNDER   SETB  (&UNDER OR '&L' EQ 'UL' OR '&L' EQ 'UNDER')              00054703
&BLINK   SETB  (&BLINK OR '&L' EQ 'BLINK')                              00054803
&REVERSE SETB  (&REVERSE OR '&L' EQ 'REVERSE')                          00054903
&INTENSE SETB  (&INTENSE OR '&L' EQ 'INTENSE' OR '&L' EQ 'INT')         00055003
&DETECT  SETB  (&DETECT OR '&L' EQ 'DETECT' OR '&L' EQ 'LP')            00055103
&NONDISP SETB  (&NONDISP OR '&L' EQ 'NONDISP' OR '&L' EQ 'NDISP')       00055203
&NUMERIC SETB  (&NUMERIC OR '&L' EQ 'NUMERIC' OR '&L' EQ 'NUM')         00055303
&SKIP    SETB  (&SKIP OR '&L' EQ 'SKIP')                                00055403
&PROTECT SETB  (&PROTECT OR '&L' EQ 'PROTECT')                          00055503
&MDT     SETB  (&MDT OR '&L' EQ 'MDT')                           87313  00055603
&DEFAULT SETB  (&DEFAULT OR '&L' EQ 'DEFAULT' OR '&L' EQ 'DFLT')        00055703
&PREV    SETB  (&PREV OR '&L' EQ 'PREVIOUS' OR '&L' EQ 'PREV')          00055803
&INDAD   SETB  (&INDAD OR '&L' EQ '*')                           81270  00055903
         AIF   (K'&L GT 4).OMLEN                                        00056003
&K       SETA  0                                                        00056103
&L       SETC  '&L'.'    '                                              00056203
&L       SETC  '&L'(1,4)                                                00056303
         AIF   ('&L' NE 'X   ').DT                              GP10240 00056403
&L       SETC  'HEX '                                           GP10240 00056503
.DT      AIF   (&K GE 35).OMLEN                                 GP07004 00056603
&K       SETA  &K+1                                                     00056703
&N       SETA  (&K-1)*4+1                                               00056803
&W      SETC  'CHARCON ASISADDRHEX SHEXBIT I   $I  D   $D  F   TIMETIMD*00056903
               DATEDATJWDAYMTH DAY MD  DMY MDY CHEXICM ICN IZ  IA  DCM *00057003
               DCN DZ  DA  EDATDATDCCHHTTR '(&N,4)              GP07004 00057103
         AIF   ('&L' NE '&W').DT                                        00057203
&DATA    SETA  &K                                                       00057303
         AIF   (&J EQ 2).OMSET                                          00057403
         AGO   .PRMLOOP                                         GP04048 00057503
.OMLEN   AIF   (&J NE 2 OR &SKPLEN).PRMLOOP                     GP04048 00057603
 AIF (&NOP OR &IN OR &NL OR &DEBL OR &DEBR OR &DEBZ OR &PADL).OMSET     00057703
 AIF (&UP OR &PADR OR &RADJ OR &BLUE OR &GREEN OR &PINK OR &RED).OMSET  00057803
 AIF (&TURQ OR &WHITE OR &YELLOW OR &UNDER OR &BLINK).OMSET             00057903
 AIF (&REVERSE OR &INTENSE OR &DETECT OR &NONDISP OR &INDAD).OMSET      00058003
 AIF (&NUMERIC OR &SKIP OR &PROTECT OR &DEFAULT OR &PREV).OMSET         00058103
         AIF   (&MDT OR &MONO OR &CNLOCK).OMSET                  92086  00058203
         AGO   .PRMLOOP                                         GP04048 00058303
.OMSET   ANOP  ,             EXPLICIT LENGTH OMITTED                    00058403
&SKPLEN  SETB  1             USE L'                                     00058503
         AGO   .PRMLOOP                                         GP04048 00058603
.*--------------------------------------------------------------------* 00058703
.*   END OF PARAMETER LOOP                                            * 00058803
.*--------------------------------------------------------------------* 00058903
.CHECK   AIF   ('&TYPE' EQ '').NOTYPE                           GP03287 00059003
.*FDBAR  AIF   (T'&TYPE NE 'N').NOTYPE                          GP03287 00059103
&DATA    SETA  &TYPE                                            GP03287 00059203
.NOTYPE  ANOP  ,                                                GP04048 00059303
&T       SETA  128*&IN+64*&NOP+8*&CNLOCK+&INDAD                  92086  00059403
&E SETA 128*&NL+64*&DEBL+32*&DEBR+16*&DEBZ+8*&PADL+4*&PADR+2*&RADJ+&UP  00059503
&O7      SETA  128*&DEFAULT+64*&PREV                                    00059603
&PROTECT SETB  (&PROTECT OR &SKIP)                                      00059703
&NUMERIC SETB  (&NUMERIC OR &SKIP)                                      00059803
&DETECT  SETB  (&DETECT  OR &NONDISP)                                   00059903
&INTENSE SETB  (&INTENSE OR &NONDISP)                                   00060003
&O7      SETA  &O7+32*&PROTECT+16*&NUMERIC+8*&INTENSE+4*&DETECT+&MDT    00060103
&C1      SETB  (&GREEN OR &TURQ OR &WHITE OR &YELLOW)                   00060203
&C2      SETB  (&RED OR &PINK OR &WHITE OR &YELLOW)                     00060303
&C3      SETB  (&BLUE OR &PINK OR &TURQ OR &WHITE)                      00060403
&O9      SETA  64*&C1+32*&C2+16*&C3+8*&MONO+4*&UNDER+2*&REVERSE+&BLINK  00060503
         AIF   (&O7 NE 0 OR &O9 NE 0).LONG                       81138  00060603
&T       SETA  &T+48         USE FDPRT SHORT FORM                81138  00060703
         AIF   ('&STR'(1,1) EQ '''').CSTRPRT                     81138  00060803
         AIF   ('&EXLEN' NE '' AND NOT &SKPLEN).PRTLEN           81138  00060903
&L       SETC  'L'''                                             81138  00061003
&FDCHAIN DC    AL1(ZFD&C-*,&T,&E,&DATA,&LEN,&L&STR),SL2(&STR)    81138  00061103
         MEXIT ,                                                 81138  00061203
.PRTLEN  AIF   (K'&EXLEN LT 2).NOPLEN                            81270  00061303
         AIF   ('&EXLEN'(1,1) NE '(' OR '&EXLEN'(2,1) EQ '(').NOPLEN    00061403
&T       SETA  &T+2          ILEN IS REGISTER FORM               81270  00061503
.NOPLEN  ANOP  ,                                                 81270  00061603
&FDCHAIN DC    AL1(ZFD&C-*,&T,&E,&DATA,&LEN,&EXLEN),SL2(&STR)    81138  00061703
         MEXIT ,                                                 81138  00061803
.CSTRPRT ANOP  ,                                                 81138  00061903
&FDCHAIN DC    AL1(ZFD&C-*,&T,&E,129,&LEN,ZFD&C-*-1),C&STR       81138  00062003
         MEXIT ,                                                 81138  00062103
.LONG    AIF   ('&STR'(1,1) EQ '''').CSTRING                     81138  00062203
         AIF   ('&EXLEN' NE '' AND NOT &SKPLEN).EXLEN                   00062303
&L       SETC  'L'''                                                    00062403
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&O9,&O7,&DATA,&LEN,&L&STR),SL2(&STR)      00062503
         MEXIT                                                          00062603
.EXLEN   AIF   (K'&EXLEN LT 2).NORLEN                            81270  00062703
         AIF   ('&EXLEN'(1,1) NE '(' OR '&EXLEN'(2,1) EQ '(').NORLEN    00062803
&T       SETA  &T+2          ILEN IS REGISTER FORM               81270  00062903
.NORLEN  ANOP  ,                                                 81270  00063003
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&O9,&O7,&DATA,&LEN,&EXLEN),SL2(&STR)      00063103
         MEXIT ,                                                        00063203
.CSTRING ANOP  ,                                                        00063303
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&O9,&O7,129,&LEN,ZFD&C-*-1),C&STR         00063403
         MEND  ,                                                        00063503
./ ADD NAME=FDCLC                                                       00063603
         MACRO                                                          00063703
&NM      FDCLC &STR,&STR2,&LEN,&BE=0,&BL=0,&BH=0,&BNE=0                 00063803
         GBLA  &FDCNTR                                                  00063903
         LCLA  &T,&C                                                    00064003
         LCLB  &NOP                                              81133  00064103
         LCLC  &FDCHN,&FL,&FH,&L                                        00064203
&FDCNTR  SETA  &FDCNTR+1                                                00064303
&FDCHN SETC  'ZFD'.'&FDCNTR'                                            00064403
&C       SETA  &FDCNTR+1                                                00064503
         AIF   ('&NM' EQ '').NONAME                                     00064603
&NM      EQU   *                                                        00064703
.NONAME  AIF   ('&STR' NE 'END' AND '&STR' NE '*END').PROCESS           00064803
&FDCHN DC    AL1(0)        END OF FD LIST                               00064903
         MEXIT ,                                                        00065003
.PROCESS ANOP  ,                                                        00065103
&FL      SETC  '&BL'                                                    00065203
&FH      SETC  '&BH'                                                    00065303
         AIF   ('&BNE' EQ '0').CHECK                                    00065403
&FL      SETC  '&BNE'                                                   00065503
&FH      SETC  '&BNE'                                                   00065603
         AIF   ('&BL' EQ '0' AND '&BH' EQ '0').CHECK                    00065703
         MNOTE 8,'*** MUTUALLY EXCLUSIVE BNE AND BL/BH'                 00065803
.CHECK   ANOP  ,                                                        00065903
&T       SETA  64*&NOP+21                                               00066003
         AIF   (T'&LEN EQ 'O').NOL                                      00066103
&FDCHN DC AL1(ZFD&C-*,&T),SL2(&BE,&FL,&FH,&STR),AL1(&LEN),SL2(&STR2)    00066203
         MEXIT ,                                                        00066303
.NOL     ANOP                                                           00066403
&L       SETC  'L'''                                                    00066503
&FDCHN DC AL1(ZFD&C-*,&T),SL2(&BE,&FL,&FH,&STR),AL1(&L&STR),SL2(&STR2)  00066603
         MEND  ,                                                        00066703
./ ADD NAME=FDCLI                                                       00066803
         MACRO                                                          00066903
&NM      FDCLI &STR,&MASK,&BE=0,&BL=0,&BH=0,&BNE=0                      00067003
         GBLA  &FDCNTR                                                  00067103
         LCLA  &T,&C                                                    00067203
         LCLB  &NOP                                              81133  00067303
         LCLC  &FDCHAIN,&FL,&FH                                         00067403
&FDCNTR  SETA  &FDCNTR+1                                                00067503
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                          00067603
&C       SETA  &FDCNTR+1                                                00067703
         AIF   ('&NM' EQ '').NONAME                                     00067803
&NM      EQU   *                                                        00067903
.NONAME  AIF   ('&STR' NE 'END' AND '&STR' NE '*END').PROCESS           00068003
&FDCHAIN DC    AL1(0)        END OF FD LIST                             00068103
         MEXIT ,                                                        00068203
.PROCESS ANOP  ,                                                        00068303
&FL      SETC  '&BL'                                                    00068403
&FH      SETC  '&BH'                                                    00068503
         AIF   ('&BNE' EQ '0').CHECK                                    00068603
&FL      SETC  '&BNE'                                                   00068703
&FH      SETC  '&BNE'                                                   00068803
         AIF   ('&BL' EQ '0' AND '&BH' EQ '0').CHECK                    00068903
         MNOTE 8,'*** MUTUALLY EXCLUSIVE BNE AND BL/BH'                 00069003
.CHECK   ANOP  ,                                                        00069103
&T       SETA  64*&NOP+20                                               00069203
&FDCHAIN DC AL1(ZFD&C-*,&T),SL2(&BE,&FL,&FH,&STR),AL1(&MASK)            00069303
         MEND  ,                                                        00069403
./ ADD NAME=FDEXEC                                                      00069503
         MACRO                                                          00069603
&NM      FDEXEC &S,&N                                                   00069703
         GBLA  &FDCNTR                                                  00069803
         LCLA  &T,&C                                                    00069903
         LCLC  &FDCHN                                                   00070003
&FDCNTR  SETA  &FDCNTR+1                                                00070103
&FDCHN   SETC  'ZFD'.'&FDCNTR'                                          00070203
&C       SETA  &FDCNTR+1                                                00070303
         AIF   ('&NM' EQ '').NONAME                                     00070403
&NM      EQU   *                                                        00070503
.NONAME  AIF   ('&S' NE 'END' AND '&S' NE '*END').PROCESS               00070603
&FDCHN   DC    AL1(0)        END OF FD LIST                             00070703
         MEXIT ,                                                        00070803
.PROCESS ANOP  ,                                                        00070903
&T       SETA  17                                                       00071003
         AIF   ('&N' NE '').TWO                                         00071103
&FDCHN   DC    AL1(ZFD&C-*,&T),2SL2(&S)                                 00071203
         AGO   .MEND                                                    00071303
.TWO     ANOP  ,                                                        00071403
&FDCHN   DC    AL1(ZFD&C-*,&T),SL2(&S,&N)                               00071503
.MEND    MEND  ,                                                        00071603
./ ADD NAME=FDGOTO                                                      00071703
         MACRO                                                          00071803
&NM      FDGOTO &S                                                      00071903
         GBLA  &FDCNTR                                                  00072003
         LCLA  &T,&C                                                    00072103
         LCLC  &FDCHN                                                   00072203
&FDCNTR  SETA  &FDCNTR+1                                                00072303
&FDCHN   SETC  'ZFD'.'&FDCNTR'                                          00072403
&C       SETA  &FDCNTR+1                                                00072503
         AIF   ('&NM' EQ '').NONAME                                     00072603
&NM      EQU   *                                                        00072703
.NONAME  AIF   ('&S' NE 'END' AND '&S' NE '*END').PROCESS               00072803
&FDCHN   DC    AL1(0)        END OF FD LIST                             00072903
         MEXIT ,                                                        00073003
.PROCESS ANOP  ,                                                        00073103
&T       SETA  16                                                       00073203
&FDCHN   DC    AL1(ZFD&C-*,&T),SL2(&S)                                  00073303
         MEND  ,                                                        00073403
./ ADD NAME=FDOPT                                                       00073503
         MACRO                                                          00073603
&NM      FDOPT &S,&SBA=,&CUR=,&CC=,&IND=                         81270  00073703
         GBLA  &FDCNTR                                                  00073803
         LCLA  &C,&I,&J,&K,&N                                           00073903
         LCLA  &T,&E,&O7,&O9,&WCC                                       00074003
         LCLC  &SB1,&CU1,&L,&FDCHN,&COM1,&COM2                          00074103
         LCLB  &NOP,&OPT,&SKPLEN                                        00074203
         LCLB  &NL,&WCCP,&SBAP,&CURP,&CCP,&INDP,&ALARM,&O79      81270  00074303
         LCLB  &BLUE,&GREEN,&PINK,&RED,&TURQ,&WHITE,&YELLOW,&C1,&C2,&C3 00074403
         LCLB  &UNDER,&BLINK,&REVERSE,&MONO,&MDT                 87313  00074503
         LCLB  &INTENSE,&DETECT,&NONDISP,&NUMERIC,&SKIP,&PROTECT        00074603
         LCLB  &DEFAULT,&PREV                                           00074703
&FDCNTR  SETA  &FDCNTR+1                                                00074803
&FDCHN   SETC  'ZFD'.'&FDCNTR'                                          00074903
&C       SETA  &FDCNTR+1                                                00075003
         AIF   ('&NM' EQ '').NONAME                                     00075103
&NM      EQU   *                                                        00075203
.NONAME  AIF   ('&S' NE 'END' AND '&S' NE '*END').PROCESS               00075303
&FDCHN   DC    AL1(0)        END OF FD LIST                             00075403
         MEXIT ,                                                        00075503
.PROCESS ANOP  ,                                                        00075603
&I       SETA  N'&SYSLIST                                               00075703
&J       SETA  0                                                        00075803
&SB1     SETC  '0'                                                      00075903
&CU1     SETC  '0'                                                      00076003
.NOLIT   AIF   (&J GE &I).CHECK                                         00076103
&J       SETA  &J+1                                                     00076203
&L       SETC  '&SYSLIST(&J)'                                           00076303
         AIF   ('&L' EQ '').NOLIT                                       00076403
&NOP     SETB  (&NOP  OR '&L' EQ 'NOP')                                 00076503
&NL      SETB  (&NL  OR '&L' EQ 'NL' OR '&L' EQ 'NEWLINE')              00076603
&ALARM    SETB  (&ALARM OR '&L' EQ 'ALARM')                             00076703
&BLUE    SETB  (&BLUE OR '&L' EQ 'BLUE')                                00076803
&GREEN   SETB  (&GREEN OR '&L' EQ 'GREEN')                              00076903
&PINK    SETB  (&PINK OR '&L' EQ 'PINK')                                00077003
&RED     SETB  (&RED  OR '&L' EQ 'RED')                                 00077103
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TURQ' OR '&L' EQ 'CYAN')      90326  00077203
&WHITE   SETB  (&WHITE OR '&L' EQ 'WHITE')                              00077303
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YELLOW')                            00077403
&MONO    SETB  (&MONO OR '&L' EQ 'MONO')                         87313  00077503
&UNDER   SETB  (&UNDER OR '&L' EQ 'UL' OR '&L' EQ 'UNDER')              00077603
&BLINK   SETB  (&BLINK OR '&L' EQ 'BLINK')                              00077703
&REVERSE SETB  (&REVERSE OR '&L' EQ 'REVERSE')                          00077803
&INTENSE SETB  (&INTENSE OR '&L' EQ 'INTENSE' OR '&L' EQ 'INT')         00077903
&DETECT  SETB  (&DETECT OR '&L' EQ 'DETECT' OR '&L' EQ 'LP')            00078003
&NONDISP SETB  (&NONDISP OR '&L' EQ 'NONDISP' OR '&L' EQ 'NDISP')       00078103
&NUMERIC SETB  (&NUMERIC OR '&L' EQ 'NUMERIC' OR '&L' EQ 'NUM')         00078203
&SKIP    SETB  (&SKIP OR '&L' EQ 'SKIP')                                00078303
&PROTECT SETB  (&PROTECT OR '&L' EQ 'PROTECT')                          00078403
&MDT     SETB  (&MDT OR '&L' EQ 'MDT')                           87313  00078503
&DEFAULT SETB  (&DEFAULT OR '&L' EQ 'DEFAULT' OR '&L' EQ 'DFLT')        00078603
         AGO   .NOLIT                                                   00078703
.CHECK   ANOP  ,                                                        00078803
         AIF   ('&SBA' EQ '').NOSBA                                     00078903
         AIF   (N'&SBA EQ 2).SBA2                                       00079003
         AIF   (N'&SBA NE 1).BADSBA                                     00079103
&SBAP    SETB  1                                                        00079203
&SB1     SETC  '254*256+254'                                            00079303
         AIF   ('&SBA(1)' EQ '*').NOSBA                                 00079403
&SB1     SETC  '&SBA'                                                   00079503
         AGO   .NOSBA                                                   00079603
.BADSBA  MNOTE 4,'INVALID SBA= FIELD'                                   00079703
         AGO   .NOSBA                                                   00079803
.SBA2    ANOP  ,                                                        00079903
&COM1    SETC  '&SBA(1)'                                                00080003
&COM2    SETC  '&SBA(2)'                                                00080103
         AGO   .ADDCOM                                                  00080203
.RETSBA  ANOP  ,                                                        00080303
&SBAP    SETB  1             SET SBA PRESENT                            00080403
&SB1     SETC  '&CU1'                                                   00080503
&CU1     SETC  '0'                                                      00080603
.NOSBA   AIF   ('&CUR' EQ '').NOCUR                                     00080703
&CURP    SETB  1                                                        00080803
         AIF   (N'&CUR EQ 2).CUR2                                       00080903
         AIF   (N'&CUR NE 1).BADCUR                                     00081003
&CU1     SETC  '254*256+254'                                            00081103
         AIF   ('&CUR(1)' EQ '*').NOCUR                                 00081203
         AIF   ('&CUR(1)' EQ '0').BADCUR                                00081303
&CU1     SETC  '&CUR'                                                   00081403
         AGO   .NOCUR                                                   00081503
.ADDERR  AIF   (NOT &CURP).BADSBA                                       00081603
.BADCUR  MNOTE 4,'INVALID CUR= FIELD'                                   00081703
         AGO   .NOCUR                                                   00081803
.CUR2    ANOP  ,                                                        00081903
&COM1    SETC  '&CUR(1)'                                                00082003
&COM2    SETC  '&CUR(2)'                                                00082103
.ADDCOM  ANOP  ,                                                        00082203
&CU1     SETC  '254'                                                    00082303
         AIF   ('&COM1' EQ 'NULL' OR '&COM1' EQ '*').AD1COM             00082403
&CU1     SETC  '255'                                                    00082503
         AIF   ('&COM1' EQ 'NEXT' OR '&COM1' EQ '+').AD1COM             00082603
&CU1     SETC  '253'                                                    00082703
         AIF   ('&COM1' EQ 'PREV' OR '&COM1' EQ '-').AD1COM             00082803
&CU1     SETC  '253'.'&COM1'                                            00082903
         AIF   (K'&COM1 LT 1).ADDERR                                    00083003
         AIF   ('&COM1'(1,1) EQ '-').AD1COM                             00083103
         AIF   ('&COM1' EQ '0').ADDERR                                  00083203
&CU1     SETC  '&COM1'.'+63'                                            00083303
.AD1COM  ANOP  ,                                                        00083403
&COM1    SETC  '254'                                                    00083503
         AIF   ('&COM2' EQ 'NULL' OR '&COM2' EQ '*').AD2COM             00083603
&COM1    SETC  '255'                                                    00083703
         AIF   ('&COM2' EQ 'NEXT' OR '&COM2' EQ '+').AD2COM             00083803
&COM1    SETC  '253'                                                    00083903
         AIF   ('&COM2' EQ 'PREV' OR '&COM2' EQ '-').AD2COM             00084003
&COM1    SETC  '253'.'&COM2'                                            00084103
         AIF   (K'&COM2 LT 1).ADDERR                                    00084203
         AIF   ('&COM2'(1,1) EQ '-').AD2COM                             00084303
         AIF   ('&COM2' EQ '0').ADDERR                                  00084403
&COM1    SETC  '&COM2'.'-1'                                             00084503
.AD2COM  ANOP  ,                                                        00084603
&CU1     SETC  '('.'&CU1'.')*256+'.'&COM1'                              00084703
         AIF   (NOT &CURP).RETSBA                                       00084803
.NOCUR   AIF   (T'&CC EQ 'O').NOCC                               81201  00084903
         AIF   (NOT &CURP).SETCC                                 81201  00085003
         MNOTE 4,'CC= AND CUR= ARE MUTUALLY EXCLUSIVE'           81201  00085103
         AGO   .NOCC                                             81201  00085203
.SETCC   ANOP  ,                                                 81201  00085303
&CCP     SETB  1                                                 81201  00085403
.NOCC    AIF   (T'&IND EQ 'O').NOIND                             81270  00085503
         AIF   (NOT &CURP).SETIND                                81270  00085603
         MNOTE 4,'IND= AND CUR= ARE MUTUALLY EXCLUSIVE'          81270  00085703
         AGO   .NOIND                                            81270  00085803
.SETIND  ANOP  ,                                                 81270  00085903
&INDP    SETB  1                                                 81270  00086003
.NOIND   ANOP  ,                                                 81270  00086103
&OPT     SETB  1                                                        00086203
&T       SETA  32*&OPT+64*&NOP                                          00086303
&WCC     SETA  4*&ALARM                                                 00086403
&WCCP    SETB  (&ALARM)                                                 00086503
&E      SETA 128*&NL+64*&WCCP+32*&SBAP+8*&CURP+&CCP+4*&INDP      81270  00086603
&O7      SETA  128*&DEFAULT+64*&PREV+&MDT                        87313  00086703
&PROTECT SETB  (&PROTECT OR &SKIP)                                      00086803
&NUMERIC SETB  (&NUMERIC OR &SKIP)                                      00086903
&DETECT  SETB  (&DETECT  OR &NONDISP)                                   00087003
&INTENSE SETB  (&INTENSE OR &NONDISP)                                   00087103
&O7      SETA  &O7+32*&PROTECT+16*&NUMERIC+8*&INTENSE+4*&DETECT         00087203
&C1      SETB  (&GREEN OR &TURQ OR &WHITE OR &YELLOW)                   00087303
&C2      SETB  (&RED OR &PINK OR &WHITE OR &YELLOW)                     00087403
&C3      SETB  (&BLUE OR &PINK OR &TURQ OR &WHITE)                      00087503
&O9      SETA  64*&C1+32*&C2+16*&C3+8*&MONO+4*&UNDER+2*&REVERSE+&BLINK  00087603
&O79     SETB  (&O7 NE 0 OR &O9 NE 0)                                   00087703
&E       SETA  &E+2*&O79                                                00087803
         AIF   (&CCP).EXPCC                                      81201  00087903
         AIF   (&CURP).EXPSC                                     81270  00088003
         AIF   (&INDP).EXPSI                                     81270  00088103
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1)                00088203
         MEXIT ,                                                 81270  00088303
.EXPSI   ANOP  ,                                                 81270  00088403
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1),AL1(0,&IND)    00088503
         MEXIT ,                                                 81270  00088603
.EXPSC   ANOP  ,                                                 81270  00088703
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1,&CU1)           00088803
         MEXIT ,                                                 81201  00088903
.EXPCC   AIF   (&INDP).EXPCI                                     81270  00089003
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1),AL1(&CC)       00089103
         MEXIT ,                                                 81270  00089203
.EXPCI   ANOP  ,                                                 81270  00089303
&FDCHN   DC    AL1(ZFD&C-*,&T,&E,&O9,&O7,&WCC),AL2(&SB1),AL1(&CC,&IND)  00089403
         MEND  ,                                                        00089503
./ ADD NAME=FDPRT    8002-07004-12023-1545-00159-00150-00000-GERHARD 00 00089603
         MACRO                                                          00089703
&NM      FDPRT &STR,&EXLEN,&VALUES,&LEN=0,                             *00089803
               &LABEL=,&LABOPT=PAD,&KEEP=0                       85119  00089903
.*                                                                      00090003
.*       PROVIDED FOR COMPATIBILITY WITH GOSSIP FD MACROS, BUT USING    00090103
.*       SHORTER DATA SECTION TO SAVE STORAGE                           00090203
.*         INTENDED FOR PRINT PROCESSING                                00090303
.*                                                                      00090403
         GBLA  &FDCNTR                                                  00090503
         LCLA  &C,&I,&J,&K,&N,&Z                                 85119  00090603
         LCLA  &T,&E,&O7,&O9,&DATA                                      00090703
         LCLC  &L,&FDCHAIN,&W                                           00090803
         LCLB  &NOP,&IN,&SKPLEN,&INDAD,&CNLOCK                   92086  00090903
         LCLB  &NL,&DEBL,&DEBR,&DEBZ,&PADL,&PADR,&RADJ,&UP              00091003
         LCLB  &BLUE,&GREEN,&PINK,&RED,&TURQ,&WHITE,&YELLOW,&C1,&C2,&C3 00091103
         LCLB  &UNDER,&BLINK,&REVERSE,&MONO,&MDT                 87313  00091203
         LCLB  &INTENSE,&DETECT,&NONDISP,&NUMERIC,&SKIP,&PROTECT        00091303
         LCLB  &DEFAULT,&PREV                                           00091403
&FDCNTR  SETA  &FDCNTR+1                                                00091503
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                          00091603
&C       SETA  &FDCNTR+1                                                00091703
         AIF   ('&NM' EQ '').NONAME                                     00091803
&NM      EQU   *                                                        00091903
.NONAME  AIF   ('&STR' NE 'END' AND '&STR' NE '*END').PROCESS           00092003
&FDCHAIN DC    AL1(0)        END OF FD LIST                             00092103
         MEXIT ,                                                        00092203
.PROCESS AIF   (T'&LABEL EQ 'O').PROCEED                         85118  00092303
         AIF   ('&KEEP' EQ '' OR '&KEEP' EQ '0').DEFKEEP         85119  00092403
&FDCHAIN DC    AL1(ZFD&C-*,30,0,&KEEP)  FDKEEP                   85119  00092503
         AGO   .DEFKCOM                                          85119  00092603
.DEFKEEP ANOP  ,                                                 85119  00092703
&Z       SETA  &C+1                                              85119  00092803
&FDCHAIN DC    AL1(ZFD&C-*,30,0,12+ZFD&Z-*)  FDKEEP              85119  00092903
.DEFKCOM ANOP  ,                                                 85119  00093003
&FDCNTR  SETA  &FDCNTR+1                                         85119  00093103
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                   85119  00093203
&C       SETA  &FDCNTR+1                                         85119  00093303
&I       SETA  12            DEFAULT PADL+PADR                   85118  00093403
         AIF   ('&LABOPT' EQ 'PAD').PROPAD                       85119  00093503
         AIF   ('&LABOPT' EQ '' OR '&LABOPT' EQ '0').PADNONE     85119  00093603
&I       SETA  8                                                 85118  00093703
         AIF   ('&LABOPT' EQ 'PADL').PROPAD                      85118  00093803
&I       SETA  4                                                 85118  00093903
         AIF   ('&LABOPT' EQ 'PADR').PROPAD                      85118  00094003
         MNOTE 4,'UNSUPPORTED LABOPT=&LABOPT'                    85118  00094103
.PADNONE ANOP  ,                                                 85119  00094203
&I       SETA  0                                                 85119  00094303
.PROPAD  AIF   ('&LABEL'(1,1) NE '''').LABNQ                     85118  00094403
&FDCHAIN DC    AL1(ZFD&C-*,48,&I,129,0,ZFD&C-*-1),C&LABEL        85118  00094503
         AGO   .PROCOM                                           85118  00094603
.LABNQ   ANOP  ,                                                 85118  00094703
&FDCHAIN DC    AL1(ZFD&C-*,48,&I,129,0,ZFD&C-*-1),C'&LABEL'      85118  00094803
.PROCOM  ANOP  ,                                                 85118  00094903
&FDCNTR  SETA  &FDCNTR+1                                         85118  00095003
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                   85118  00095103
&C       SETA  &FDCNTR+1                                         85118  00095203
.PROCEED ANOP  ,                                                 85118  00095303
&I       SETA  N'&SYSLIST                                               00095403
&J       SETA  1                                                        00095503
         AIF   ('&STR'(1,1) NE '''').NOLIT                              00095603
&SKPLEN  SETB  1                                                        00095703
.NOLIT   AIF   (&J GE &I).CHECK                                         00095803
&J       SETA  &J+1                                                     00095903
&L       SETC  '&SYSLIST(&J)'                                           00096003
         AIF   ('&L' EQ '').NOLIT                                       00096103
&NOP     SETB  (&NOP  OR '&L' EQ 'NOP')                                 00096203
&CNLOCK  SETB  (&CNLOCK OR '&L' EQ 'LOCK' OR '&L' EQ 'LOCKED')   92086  00096303
&NL      SETB  (&NL  OR '&L' EQ 'NL' OR '&L' EQ 'NEWLINE')              00096403
&DEBL SETB (&DEBL OR '&L' EQ 'DEBL' OR '&L' EQ 'DEB' OR '&L' EQ 'DEBZ') 00096503
&DEBR    SETB  (&DEBR OR '&L' EQ 'DEBR' OR '&L' EQ 'DEB')               00096603
&DEBZ    SETB  (&DEBZ OR '&L' EQ 'DEBZ')                                00096703
&PADL    SETB  (&PADL OR '&L' EQ 'PADL' OR '&L' EQ 'PAD')               00096803
&PADR    SETB  (&PADR OR '&L' EQ 'PADR' OR '&L' EQ 'PAD')               00096903
&RADJ    SETB  (&RADJ OR '&L' EQ 'RADJ')                                00097003
&UP    SETB  (&UP OR '&L' EQ 'UP')                                      00097103
&UP      SETB  (&UP OR '&L' EQ 'UPPER')                                 00097203
&BLUE    SETB  (&BLUE OR '&L' EQ 'BLUE')                                00097303
&GREEN   SETB  (&GREEN OR '&L' EQ 'GREEN')                              00097403
&PINK    SETB  (&PINK OR '&L' EQ 'PINK')                                00097503
&RED     SETB  (&RED  OR '&L' EQ 'RED')                                 00097603
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TURQ')                               00097703
&WHITE   SETB  (&WHITE OR '&L' EQ 'WHITE')                              00097803
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YELLOW')                            00097903
&BLUE    SETB  (&BLUE OR '&L' EQ 'BL')                          GP10240 00098003
&GREEN   SETB  (&GREEN OR '&L' EQ 'GR')                         GP10240 00098103
&PINK    SETB  (&PINK OR '&L' EQ 'PI')                          GP10240 00098203
&RED     SETB  (&RED  OR '&L' EQ 'RE')                          GP10240 00098303
&TURQ    SETB  (&TURQ  OR '&L' EQ 'TU' OR '&L' EQ 'CY')         GP10240 00098403
&WHITE   SETB  (&WHITE OR '&L' EQ 'WH')                         GP10240 00098503
&YELLOW  SETB  (&YELLOW OR '&L' EQ 'YE')                        GP10240 00098603
&MONO    SETB  (&MONO OR '&L' EQ 'MONO')                         87313  00098703
&UNDER   SETB  (&UNDER OR '&L' EQ 'UL' OR '&L' EQ 'UNDER')              00098803
&BLINK   SETB  (&BLINK OR '&L' EQ 'BLINK')                              00098903
&REVERSE SETB  (&REVERSE OR '&L' EQ 'REVERSE')                          00099003
&INTENSE SETB  (&INTENSE OR '&L' EQ 'INTENSE' OR '&L' EQ 'INT')         00099103
&DETECT  SETB  (&DETECT OR '&L' EQ 'DETECT' OR '&L' EQ 'LP')            00099203
&NONDISP SETB  (&NONDISP OR '&L' EQ 'NONDISP' OR '&L' EQ 'NDISP')       00099303
&NUMERIC SETB  (&NUMERIC OR '&L' EQ 'NUMERIC' OR '&L' EQ 'NUM')         00099403
&SKIP    SETB  (&SKIP OR '&L' EQ 'SKIP')                                00099503
&PROTECT SETB  (&PROTECT OR '&L' EQ 'PROTECT')                          00099603
&MDT     SETB  (&MDT OR '&L' EQ 'MDT')                           87313  00099703
&DEFAULT SETB  (&DEFAULT OR '&L' EQ 'DEFAULT' OR '&L' EQ 'DFLT')        00099803
&PREV    SETB  (&PREV OR '&L' EQ 'PREVIOUS' OR '&L' EQ 'PREV')          00099903
&INDAD   SETB  (&INDAD OR '&L' EQ '*')                           81270  00100003
         AIF   (&DATA NE 0).NOLIT                                       00100103
         AIF   (K'&L GT 4).OMLEN                                        00100203
&K       SETA  0                                                        00100303
&L       SETC  '&L'.'    '                                              00100403
&L       SETC  '&L'(1,4)                                                00100503
         AIF   ('&L' NE 'X   ').DT                              GP10240 00100603
&L       SETC  'HEX '                                           GP10240 00100703
.DT      AIF   (&K GE 35).OMLEN                                 GP07004 00100803
&K       SETA  &K+1                                                     00100903
&N       SETA  (&K-1)*4+1                                               00101003
&W      SETC  'CHARCON ASISADDRHEX SHEXBIT I   $I  D   $D  F   TIMETIMD*00101103
               DATEDATJWDAYMTH DAY MD  DMY MDY CHEXICM ICN IZ  IA  DCM *00101203
               DCN DZ  DA  EDATDATDCCHHTTR '(&N,4)              GP07004 00101303
         AIF   ('&L' NE '&W').DT                                        00101403
&DATA    SETA  &K                                                       00101503
         AIF   (&J EQ 2).OMSET                                          00101603
         AGO   .NOLIT                                                   00101703
.OMLEN   AIF   (&J NE 2 OR &SKPLEN).NOLIT                               00101803
 AIF (&NOP OR &IN OR &NL OR &DEBL OR &DEBR OR &DEBZ OR &PADL).OMSET     00101903
 AIF (&UP OR &PADR OR &RADJ OR &BLUE OR &GREEN OR &PINK OR &RED).OMSET  00102003
 AIF (&TURQ OR &WHITE OR &YELLOW OR &UNDER OR &BLINK).OMSET             00102103
 AIF (&REVERSE OR &INTENSE OR &DETECT OR &NONDISP OR &INDAD).OMSET      00102203
 AIF (&NUMERIC OR &SKIP OR &PROTECT OR &DEFAULT OR &PREV).OMSET         00102303
         AIF   (&MDT OR &MONO OR &CNLOCK).OMSET                  92086  00102403
         AGO   .NOLIT                                                   00102503
.OMSET   ANOP  ,             EXPLICIT LENGTH OMITTED                    00102603
&SKPLEN  SETB  1             USE L'                                     00102703
         AGO   .NOLIT                                                   00102803
.CHECK   ANOP  ,                                                        00102903
&T       SETA  64*&NOP+48+&INDAD+8*&CNLOCK                       92086  00103003
&E SETA 128*&NL+64*&DEBL+32*&DEBR+16*&DEBZ+8*&PADL+4*&PADR+2*&RADJ+&UP  00103103
&O7      SETA  128*&DEFAULT+64*&PREV+&MDT                        87313  00103203
&PROTECT SETB  (&PROTECT OR &SKIP)                                      00103303
&NUMERIC SETB  (&NUMERIC OR &SKIP)                                      00103403
&DETECT  SETB  (&DETECT  OR &NONDISP)                                   00103503
&INTENSE SETB  (&INTENSE OR &NONDISP)                                   00103603
&O7      SETA  &O7+32*&PROTECT+16*&NUMERIC+8*&INTENSE+4*&DETECT         00103703
&C1      SETB  (&GREEN OR &TURQ OR &WHITE OR &YELLOW)                   00103803
&C2      SETB  (&RED OR &PINK OR &WHITE OR &YELLOW)                     00103903
&C3      SETB  (&BLUE OR &PINK OR &TURQ OR &WHITE)                      00104003
&O9      SETA  64*&C1+32*&C2+16*&C3+8*&MONO+4*&UNDER+2*&REVERSE+&BLINK  00104103
         AIF   ('&STR'(1,1) EQ '''').CSTRING                            00104203
         AIF   ('&EXLEN' NE '' AND NOT &SKPLEN).EXLEN                   00104303
&L       SETC  'L'''                                                    00104403
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&DATA,&LEN,&L&STR),SL2(&STR)              00104503
         MEXIT                                                          00104603
.EXLEN   AIF   (K'&EXLEN LT 2).NORLEN                            81270  00104703
         AIF   ('&EXLEN'(1,1) NE '(' OR '&EXLEN'(2,1) EQ '(').NORLEN    00104803
&T       SETA  &T+2          ILEN IS REGISTER FORM               81270  00104903
.NORLEN  ANOP  ,                                                 81270  00105003
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,&DATA,&LEN,&EXLEN),SL2(&STR)              00105103
         MEXIT ,                                                        00105203
.CSTRING ANOP  ,                                                        00105303
&FDCHAIN DC AL1(ZFD&C-*,&T,&E,129,&LEN,ZFD&C-*-1),C&STR                 00105403
         MEND  ,                                                        00105503
./ ADD NAME=FDREPT                                                      00105603
         MACRO                                                          00105703
&NM      FDREPT &N,&S                                   ADDED ON 82109  00105803
         GBLA  &FDCNTR                                                  00105903
         LCLA  &T,&C                                                    00106003
         LCLC  &FDCHN                                                   00106103
&FDCNTR  SETA  &FDCNTR+1                                                00106203
&FDCHN   SETC  'ZFD'.'&FDCNTR'                                          00106303
&C       SETA  &FDCNTR+1                                                00106403
         AIF   ('&NM' EQ '').NONAME                                     00106503
&NM      EQU   *                                                        00106603
.NONAME  AIF   ('&N' NE 'END' AND '&N' NE '*END').PROCESS               00106703
&FDCHN   DC    AL1(0)        END OF FD LIST                             00106803
         MEXIT ,                                                        00106903
.PROCESS ANOP  ,                                                        00107003
&T       SETA  30                                                       00107103
         AIF   (T'&S EQ 'O').BLANK                                      00107203
         AIF   (T'&N EQ 'O').DFLT                                       00107303
&FDCHN   DC    AL1(ZFD&C-*,&T),AL1(&N,&S)                               00107403
         MEXIT ,                                                        00107503
.DFLT    ANOP  ,                                                        00107603
&FDCHN   DC    AL1(ZFD&C-*,&T),AL1(1,&S)                                00107703
         MEXIT ,                                                        00107803
.BLANK   ANOP  ,                                                        00107903
         AIF   (T'&N EQ 'O').BLAND                                      00108003
&FDCHN   DC    AL1(ZFD&C-*,&T),AL1(&N,C' ')                             00108103
         MEXIT ,                                                        00108203
.BLAND   ANOP  ,                                                        00108303
&FDCHN   DC    AL1(ZFD&C-*,&T),AL1(1,C' ')                              00108403
         MEND  ,                                                        00108503
./ ADD NAME=FDSECT   0103-03287-08076-1343-00129-00126-00000-GERHARD 00 00108603
         MACRO                                                          00108703
&NM      FDSECT ,                                                       00108803
         GBLB  &MAPFDS                                                  00108903
         AIF   (&MAPFDS).MEND                                           00109003
&MAPFDS  SETB  1                                                        00109103
         AIF   ('&NM' NE '').EXNAME                                     00109203
FDSECT   DSECT ,             FD ITEM MAPPING                            00109303
         AGO   .COMNAME                                                 00109403
.EXNAME  ANOP  ,                                                        00109503
&NM      DSECT ,             FD ITEM MAPPING                            00109603
.COMNAME ANOP  ,                                                        00109703
FDLINK   DS    AL1           LENGTH TO NEXT ENTRY OR 0                  00109803
FDTYPE   DS    X             ENTRY TYPE (IN, OUT, NOP)                  00109903
FDFNOP   EQU   X'40'           IGNORE THIS ENTRY                        00110003
FDFIN    EQU   X'80'           INPUT ENTRY                              00110103
FDFCIN   EQU   X'08'           FDIN IS LOCKED (COND. INPUT)      87156  00110203
FDFPRT   EQU   X'30'           FD/FDIN - NO 3270 FIELDS          81127  00110303
FDFIND@  EQU   X'01'             FDSADD IS INDIRECT ADDRESS      81270  00110403
FDFINDAD EQU   X'01'             FDSADD IS INDIRECT ADDRESS     GP08076 00110503
FDFREG#  EQU   X'02'             FDILEN IS REGISTER WITH LENGTH  81270  00110603
FDFREGLN EQU   X'02'             FDILEN IS REGISTER WITH LENGTH GP08076 00110703
FDFEXAD  EQU   X'04'             EXPANSION HAS USER EXIT ADDRESS 89095  00110803
FDFOPT   EQU   X'20'           OPTION LIST                              00110903
FDFGOTO  EQU   X'10'           BRANCH TO ANOTHER FD ENTRY               00111003
FDFEXEC  EQU   X'11'           PERFORM NEW FD RANGE              81131  00111103
FDFBR    EQU   X'12'           BRANCH/TEST AFTER PRIOR TEST      81131  00111203
FDFTM    EQU   X'13'           TM/BRANCH                         81131  00111303
FDFCLI   EQU   X'14'           CLI/BRANCH                        81131  00111403
FDFCLC   EQU   X'15'           CLC/BRANCH                        81131  00111503
FDFSPC   EQU   X'1E'           SPACE/ROOM/REPT SERVICE           82109  00111603
FDFUEX   EQU   X'1F'           USER EXIT                         81193  00111703
FDGOTO   DS    0SL2(0)       ADDRESS OF TARGET FD OF GO TO              00111803
FDEDIT   DS    X             EDITING OPTIONS                            00111903
FDFNL    EQU   X'80'           POSITION TO NEW LINE                     00112003
FDFDEBL  EQU   X'40'           STRIP LEADING BLANKS                     00112103
FDFDEBR  EQU   X'20'           STRIP TRAILING BLANKS                    00112203
FDFDEBZ  EQU   X'10'           STRIP LEADING ZEROES                     00112303
FDFPADL  EQU   X'08'           LEFT BLANK OR SF                         00112403
FDFPADR  EQU   X'04'           RIGHT BLANK OR SF                        00112503
FDFRADJ  EQU   X'02'           RIGHT-ADJUST IN OUTPUT                   00112603
FDFUP    EQU   X'01'           UPPER CASE INPUT TRANSLATE               00112703
*        REDEFINITION FOR FDOPT                                  82109  00112803
*FDFNL   EQU   X'80'           POSITION TO NEW LINE                     00112903
FDOWCCP  EQU   X'40'         WCC OPTIONS PRESENT                        00113003
FDOSBAP  EQU   X'20'         SBA PRESENT                                00113103
FDOCURP  EQU   X'08'         CURSOR ADDRESS PRESENT                     00113203
FDOINDP  EQU   X'04'           AUTO INDENT VALUE PRESENT         81270  00113303
FDOPTP   EQU   X'02'         COLOR OR DISPLAY OPTIONS PRESENT           00113403
FDOPCCP  EQU   X'01'         PRT CARRIAGE CONTROL INSTEAD OF CURP       00113503
FDOPT9   DS    X             3279 OPTIONS                               00113603
FDFCOLOR EQU   X'70' 0DFLT,1BLUE,2RED,3PINK,4GREEN,5TURQ,6YELLOW,7WHITE 00113703
FDFMONO  EQU   X'08'         APPLY HIGH-LIGHT ON MONOCHROME ONLY 87313  00113803
FDFUNDER EQU   X'04'           UNDERLINE                                00113903
FDFREV   EQU   X'02'           REVERSE                                  00114003
FDFBLINK EQU   X'01'           BLINK                                    00114103
FDOPT7   DS    X             3277/3278 OPTIONS                          00114203
FDFINT   EQU   X'08'           INTENSIFIED                              00114303
FDFLPEN  EQU   X'04'           LIGHT-PEN DETECTABLE                     00114403
FDFNDISP EQU   X'0C'           NON-DISPLAY                              00114503
FDFNUM   EQU   X'10'           NUMERIC INPUT                            00114603
FDFSKIP  EQU   X'30'           SKIP DISPLAY                             00114703
FDFPROT  EQU   X'20'           PROTECTED                                00114803
FDFDFLT  EQU   X'80'           DEFAULT OPTIONS/COLORS                   00114903
FDFPREV  EQU   X'40'           PREVIOUS OPTIONS/COLORS                  00115003
FDFNULL  EQU   X'02'           SUPPRESS X'00' IN INPUT FIELDS           00115103
FDFMTD   EQU   X'01'           MODIFIED DATA TAG                 87313  00115203
FDDATA   DS    X             DATA TYPE                                  00115303
FDDLIT   EQU   X'80'           FD CONTAINS LITERAL, NOT ADDRESS         00115403
FDDCHAR  EQU   1               EBCDIC, TRANSLATED                       00115503
FDDCON   EQU   2               EBCDIC WITH CONTROL CHARACTERS           00115603
FDDASIS  EQU   3               EBCDIC(?), NO TRANSLATE                  00115703
FDDADDR  EQU   4               ADDRESS                                  00115803
FDDHEX   EQU   5               HEXADECIMAL                              00115903
FDDSHEX  EQU   6               HEXADECIMAL WITH EXPLICIT SIGN           00116003
FDDBIT   EQU   7               BIT STRING                               00116103
FDDINT   EQU   8               INTEGER                                  00116203
FDD$INT  EQU   9               INTEGER.DD                               00116303
FDDDEC   EQU   10              PACKED DECIMAL                           00116403
FDD$DEC  EQU   11              PACKED DECIMAL.DD                        00116503
FDDFIX   EQU   12              FLOATING POINT                           00116603
FDDTIME  EQU   13              TIME (BIN 1/100 SECONDS)          81193  00116703
FDDTIMD  EQU   14              TIME (PACKED)                     81193  00116803
FDDDATE  EQU   15              DATE (PACKED; O/P MM/DD/YY)       81193  00116903
FDDDATJ  EQU   16              DATE (PACKED; O/P YY.DDD)         81193  00117003
FDDFLAG  EQU   64              FLAG/TABLE FORMATTING            GP06273 00117103
FDOLEN   DS    AL1           OUTPUT LENGTH; 0 FOR DEFAULT; MAX FOR FDIN 00117203
FDILEN   DS    AL1           CURRENT LENGTH OF ITEM                     00117303
FDTEXT   DS    0CL132        (FD/FDPRT) LITERAL TEXT                    00117403
FDSADD   DS    SL2           ADDRESS OF DATA ITEM                       00117503
FDIOFF   DS    AL2           FDIN - OFFSET TO FIW AREA           84237  00117603
FDIXAD   DS    SL2           FDIN - USER EXIT ADDRESS            89095  00117703
         ORG   FDIOFF          REDEFINE FOR FLAG PROCESSING     GP03287 00117803
FDTBAD   DS    SL2           ADDRESS OF BIT EQUIVALENT TEXT     GP03287 00117903
FDTSEP   DS    C             OUTPUT SEPARATOR CHARACTER OR 00   GP03287 00118003
FDTSPC   DS    XL1           NUMBER OF SPACES BETWEEN ITEMS     GP03287 00118103
         ORG   FDDATA                                                   00118203
FDOWCC   DC    X'0'          WCC OPTIONS                                00118303
FDOSBA   DC    XL2'0'        SBA ADDRESS                                00118403
FDOCUR   DC    0XL2'0'       CURSOR ADDRESS                             00118503
FDOCC    DS    C             PRINTER CARRIAGE CONTROL            81201  00118603
FDOIND   DS    AL1           AUTOMATIC LINE INDENT               81270  00118703
         SPACE 1                                                 81127  00118803
         ORG   FDGOTO                                            81127  00118903
FDBRE    DS    SL2           BRANCH EQUAL                        81127  00119003
FDBRL    DS    SL2           BRANCH LOW/MIXED                    81127  00119103
FDBRH    DS    SL2           BRANCH HIGH/ONES                    81127  00119203
FDBVAR   DS    SL2           TEST VARIABLE                       81127  00119303
FDBIDA   DS    0X              IMMEDIATE DATA FOR TEST           81127  00119403
FDBLEN   DS    X               LENGTH FOR FDCLC                  81127  00119503
FDBCLC   DS    SL2           COMPARE STRING                      81127  00119603
         SPACE 1                                                 81193  00119703
         ORG   FDGOTO        DEFINITION FOR USER EXIT REQUEST    81193  00119803
FDUXAD   DS    SL2           USER EXIT ADDRESS                   81193  00119903
FDUXFPRM DS    0X            USER SUPPLIED PARM INFO             81193  00120003
         ORG   ,                                                 81193  00120103
FDXOK    EQU   0             RETURN CODES - NORMAL PROCESSING    81193  00120203
FDXGOTO  EQU   2               NEW FD ADDRESS IN R1              81193  00120303
FDXCLR   EQU   4               CLEAR CURRENT LINE                81193  00120403
FDXPRT   EQU   8               PRINT CURRENT LINE                81193  00120503
FDXADD   EQU   FDXCLR+FDXPRT   DATA ADDED TO LINE                81193  00120603
FDXQUIT  EQU   16              TERMINATE CURRENT PRTLIST         81193  00120703
         SPACE 1                                                 81193  00120803
FDUXPARM DSECT ,             MAPPING OF R1 LIST SUPPLIED TO EXIT 81193  00120903
FDUXFD   DS    A               ADDRESS OF CURRENT FD             81193  00121003
FDUXPWRK DS    A               ADDRESS OF PRINTER WORK AREA      81193  00121103
FDUXSAVE DS    A               ADDRESS OF ORIGINAL SAVE AREA     81193  00121203
FDUXPRT  DS    A               ADDRESS OF CURRENT PRINT LINE     81193  00121303
         DS    A                 RESERVED                        81193  00121403
.MEND    MEND  ,                                                        00121503
./ ADD NAME=FDTM                                                        00121603
         MACRO                                                          00121703
&NM      FDTM  &STR,&MASK,&BZ=0,&BM=0,&BO=0,&BNO=0,&BNZ=0        81264  00121803
         GBLA  &FDCNTR                                                  00121903
         LCLA  &T,&C                                                    00122003
         LCLB  &NOP                                              81133  00122103
         LCLC  &FDCHAIN,&FZ,&FM,&FO                              81264  00122203
&FDCNTR  SETA  &FDCNTR+1                                                00122303
&FDCHAIN SETC  'ZFD'.'&FDCNTR'                                          00122403
&C       SETA  &FDCNTR+1                                                00122503
         AIF   ('&NM' EQ '').NONAME                                     00122603
&NM      EQU   *                                                        00122703
.NONAME  AIF   ('&STR' NE 'END' AND '&STR' NE '*END').PROCESS           00122803
&FDCHAIN DC    AL1(0)        END OF FD LIST                             00122903
         MEXIT ,                                                        00123003
.PROCESS ANOP  ,                                                        00123103
&FZ      SETC  '&BZ'                                             81264  00123203
&FM      SETC  '&BM'                                             81264  00123303
&FO      SETC  '&BO'                                             81264  00123403
         AIF   ('&BNZ' EQ '0' OR '&BNO' EQ '0').BNZBNO           81264  00123503
         MNOTE 8,'MUTUALLY EXCLUSIVE BNZ AND BNO'                81264  00123603
.BNZBNO  AIF   ('&BNO' EQ '0').NOBNO                             81264  00123703
         AIF   ('&FZ' EQ '0' AND '&FM' EQ '0').DOBNO             81264  00123803
         MNOTE 8,'MUTUALLY EXCLUSIVE BNO AND BZ/BM'              81264  00123903
.DOBNO   ANOP  ,                                                 81264  00124003
&FZ      SETC  '&BNO'                                            81264  00124103
&FM      SETC  '&BNO'                                            81264  00124203
.NOBNO   AIF   ('&BNZ' EQ '0').CHECK                             81264  00124303
         AIF   ('&FM' EQ '0' AND '&FO' EQ '0').DOBNZ             81264  00124403
         MNOTE 8,'MUTUALLY EXCLUSIVE BNZ AND BM/BO'              81264  00124503
.DOBNZ   ANOP  ,                                                 81264  00124603
&FM      SETC  '&BNZ'                                            81264  00124703
&FO      SETC  '&BNZ'                                            81264  00124803
.CHECK   ANOP  ,                                                        00124903
&T       SETA  64*&NOP+19                                               00125003
&FDCHAIN DC AL1(ZFD&C-*,&T),SL2(&FZ,&FM,&FO,&STR),AL1(&MASK)     81264  00125103
         MEND  ,                                                        00125203
./ ADD NAME=INC      0106-02250-09017-1944-00053-00028-00000-GERHARD 00 00125303
         MACRO ,                                                        00125403
&NM      INC   &R,&INC=,&WORK=R0,&IN@=     RENAMED FROM COUNT ON 89247  00125503
         GBLC  &MACPLAB                                                 00125603
         LCLA  &K                                               GP09016 00125703
         LCLB  &REG                                             GP09016 00125803
         LCLC  &WROK,&CNI                                       GP09016 00125903
&WROK    SETC  '&WORK(1)'                                       GP09016 00126003
&MACPLAB SETC  '&NM'                                                    00126103
&K       SETA  K'&R                                             GP09016 00126203
         AIF   (&K LT 3).NREG                                   GP09016 00126303
         AIF   ('&R'(1,1) NE '(' OR '&R'(2,1) EQ '(').NREG      GP09016 00126403
         AIF   ('&R'(&K,1) NE ')' OR '&R'(&K-1,1) EQ ')').NREG  GP09016 00126503
&WROK    SETC  '&R(1)'                                          GP09016 00126603
&REG     SETB  1                                                GP09016 00126703
.NREG    AIF   ('&IN@' NE '').LOAD                              GP02250 00126803
         AIF   ('&INC' EQ '-1').BCTR                                    00126903
&K       SETA  K'&INC                                           GP09016 00127003
         AIF   (&K LT 3).LA                                     GP09016 00127103
         AIF   ('&INC'(1,1) NE '(' OR '&INC'(2,1) EQ '(').LA            00127203
         AIF   ('&INC'(&K,1) NE ')' OR '&INC'(&K-1,1) EQ ')').LA        00127303
         MACPARM &WROK,&R,OP=L,OPR=LR                                   00127403
         MACPARM &WROK,&INC,OPR=AR,OPMR=SR                       82003  00127503
         AGO   .COMST                                                   00127603
.BCTR    MACPARM &WROK,&R,OP=L,OPR=LR                                   00127703
         MACPARM &WROK,(0-0),OPR=BCTR                                   00127803
         AGO   .COMST                                                   00127903
.LOAD    AIF   ('&INC' EQ '' OR '&INC' EQ '1').LOADER           GP02250 00128003
         MNOTE 'INC: INC KEYWORD &INC CONFLICTS WITH IN@; IGNORED'      00128103
.LOADER  AIF   ('&WROK' EQ '&IN@(1)').LOADRV                    GP02250 00128203
         MACPARM &WROK,&R,OP=L,OPR=LR                           GP02250 00128303
         MACPARM &WROK,&IN@,OP=A,OPR=AR,OPM=S,OPMR=SR           GP02250 00128403
         AGO   .COMST                                           GP02250 00128503
.LOADRV  MACPARM &WROK,(&IN@(1)),OP=L,OPR=LR                    GP02250 00128603
         MACPARM &WROK,&R,OP=A,OPR=AR                           GP02250 00128703
         AGO   .COMST                                           GP02250 00128803
.LA      AIF   (NOT &REG).LAST                                  GP09016 00128903
         AIF   ('&INC' NE '').AINC                              GP09016 00129003
         MACPARM &WROK,=F'1',OP=A                               GP09016 00129103
         MEXIT ,                                                GP09016 00129203
.AINC    AIF   ('&INC'(1,1) NE '-').BINC                        GP09016 00129303
&K       SETA  K'&INC                                           GP09016 00129403
         AIF   (&K LT 4).BINC                                   GP09016 00129503
         AIF   ('&INC'(2,1) NE '(' OR '&INC'(3,1) EQ '(').BINC  GP09016 00129603
         AIF   ('&INC'(&K,1) NE ')' OR '&INC'(&K-1,1) EQ ')').BINC      00129703
&CNI     SETC  '&INC'(2,&K-1)                                           00129803
         MACPARM &WROK,&CNI,OP=SR,OPR=SR                        GP09016 00129903
         MEXIT ,                                                GP09016 00130003
.BINC    MACPARM &WROK,=A(&INC),OP=A                            GP09016 00130103
         MEXIT ,                                                GP09016 00130203
.LAST    MACPARM &WROK,&INC,NULL=1                                      00130303
         MACPARM &WROK,&R,OP=A,OPR=AR,OPM=S,OPMR=SR              82003  00130403
.COMST   MACPARM &WROK,&R,OP=ST,OPR=LR,MODE=REV                         00130503
         MEND  ,                                                        00130603
./ ADD NAME=INPCOM   0105-98365-06263-0011-00064-00055-00013-GERHARD 00 00130703
         MACRO                                                          00130803
&NM      INPCOM &B0,&B1,&FN,&P0,&A1,&DEV=             UPDATED ON 99007  00130903
.********************************************************************** 00131003
.*                                                                   ** 00131103
.*   COMMON INNER MACRO FOR @INPREAD INVOCATION                      ** 00131203
.*                                                                   ** 00131303
.*  ESA AND OS/390 CHANGE - FLAG BYTE NOW IN R0:1 FROM R1:0     GP98365 00131403
.********************************************************************** 00131503
         GBLC  &MACPLAB,&INPMODE                                        00131603
         LCLA  &I,&J,&K,&VD,&D(8)                                       00131703
         LCLB  &INDEV                                            82116  00131803
         LCLC  &DC,&A0                                           82116  00131903
&MACPLAB SETC  '&NM'                                             82116  00132003
&A0      SETC  '&P0'                                            GP99007 00132103
         AIF   ('&A0' NE '').LENOK                              GP99007 00132203
&A0      SETC  '0'                                              GP99007 00132303
.LENOK   AIF   ('&DEV' EQ '' OR '&DEV' EQ '0').NODV             GP99007 00132403
         AIF   ('&DEV' NE 'ALL').DVSOM                                  00132503
&VD      SETA  255                                                      00132603
         AGO   .NODV                                                    00132703
.DVSOM   AIF   (K'&DEV LT 2).DVSOL                               82116  00132803
         AIF   ('&DEV'(1,1) NE '=').DVSOL                        82116  00132903
&INDEV   SETB  1             SET INDIRECT DEVICE NUMBER          82116  00133003
         AGO   .NODV                                             82116  00133103
.DVSOL   ANOP  ,                                                 82116  00133203
&I       SETA  0                                                        00133303
&J       SETA  N'&DEV                                                   00133403
.DEVLOOP ANOP  ,                                                        00133503
&I       SETA  &I+1                                                     00133603
         AIF   (&I GT &J).DVEND                                         00133703
         AIF   ('&DEV(&I)' EQ '').DEVLOOP                               00133803
         AIF   ('&DEV(&I)' EQ '0').DEVLOOP                              00133903
         AIF   ('&DEV(&I)' LT '1' OR '&DEV(&I)' GT '8').DVERR           00134003
&D(&DEV(&I)) SETA  1                                                    00134103
         AGO   .DEVLOOP                                                 00134203
.DVERR   MNOTE 8,'*** INVALID DEVICE NUMBER &DEV(&I)'                   00134303
         AGO   .DEVLOOP                                                 00134403
.DVEND   ANOP  ,                                                        00134503
&VD      SETA  128*&D(8)+64*&D(7)+32*&D(6)+16*&D(5)+8*&D(4)             00134603
&VD      SETA  &VD+4*&D(3)+2*&D(2)+&D(1)                                00134703
.NODV    AIF   ('&B0' NE '0' OR '&A0' NE '0').LONG              GP98365 00134803
         AIF   (&VD GT 15).LONG                                         00134903
&K       SETA  &VD*256+&FN                                              00135003
         MACPARM R0,&K       LOAD DEVICE/FUNCTION INDEX          82116  00135103
         AIF   ('&A0' EQ '0').POST0                             GP99007 00135203
         ICM   R0,4,=AL1(&A0)                                   GP98365 00135303
         AGO   .POST0                                            82116  00135403
.LONG    ANOP  ,                                                        00135503
&MACPLAB L     R0,=AL1(&B0,&A0,&VD,&FN)                         GP98365 00135603
&MACPLAB SETC  ''            CANCEL LABEL                        82116  00135703
.POST0   AIF   (NOT &INDEV).LOAD1                                82116  00135803
&VD      SETA  K'&DEV-1                                          82116  00135903
&DC      SETC  '&DEV'(2,&VD)                                     82116  00136003
&MACPLAB ICM   R0,2,&DC                                          82116  00136103
&MACPLAB SETC  ''                                                82116  00136203
.LOAD1   AIF   ('&FN' EQ '0' OR '&FN' EQ '1').BAL                       00136303
         MACPARM R1,&A1      LOAD PARAMETER REGISTER                    00136403
.BAL     AIF   ('&INPMODE' EQ 'V').VCON                                 00136503
         L     R15,@INPREAD                                             00136603
         AGO   .BALR                                                    00136703
.VCON    L     R15,=V(@INPREAD)                                         00136803
.BALR    BALSR R14,R15                                                  00136903
.*ALR    BASSM R14,R15                                                  00137003
         MEND  ,                                                        00137103
./ ADD NAME=INPGET   0101-03034-03035-1741-00042-00042-00006-GERHARD 00 00137203
         MACRO                                                          00137303
&NM      INPGET &IMAGE,&DEV=,&FILL=                      ADDED ON 81194 00137403
         LCLA  &N                                               GP03034 00137503
         LCLC  &L                                               GP03034 00137603
&NM      INPCOM 0,0,3,,0,DEV=&DEV                                       00137703
.*                                                              GP03034 00137803
.*--------------------------------------------------------------------* 00137903
.*                                                                    * 00138003
.*  WHEN A POSITIONAL OPERNAD IS SPECIFIED, IT INDICATES MOVE MODE.   * 00138103
.*  INPGET X     EXPANDS  MVC X{L'X},0(R1)                            * 00138203
.*  INPGET (X,L) EXPANDS  MVC X{L},0(R1)                              * 00138303
.*    NOTE THAT LENGTH OF X MUST BE LESS THAN OR EQUAL TO THE         * 00138403
.*    WIDTH= PARAMETER ON THE CORRESPONDING INPWORK MACRO             * 00138503
.*                                                                    * 00138603
.*  WHEN FILL= IS SPECIFIED, A LONGER EXPANSION USING MVCL ALLOWS     * 00138703
.*  DISPARATE LENGTHS                                                 * 00138803
.*                                                                    * 00138903
.*--------------------------------------------------------------------* 00139003
&N       SETA  N'&IMAGE                                         GP03034 00139103
         AIF   (&N LT 1).MEND                                   GP03034 00139203
         AIF   (T'&FILL NE 'O').FILL                            GP03034 00139303
         AIF   (&N EQ 1).DEFLEN                                 GP03034 00139403
         MVC   &IMAGE(1)(&IMAGE(2)),0(R1)                       GP03034 00139503
         MEXIT ,                                                GP03034 00139603
.DEFLEN  MVC   &IMAGE(1),0(R1)                                  GP03034 00139703
         MEXIT ,                                                GP03034 00139803
.FILL    SAR   R0,R15        PRESERVE RETURN CODE               GP03034 00139903
         LR    R14,R1        SET SOURCE RECORD ADDRESS          GP03034 00140003
         LR    R15,R0        SET SOURCE LENGTH                  GP03034 00140103
         AIF   (&N EQ 1).DEFILL                                 GP03034 00140203
         MACPARM R0,&IMAGE(1)  LOAD ADDRESS                     GP03034 00140303
         MACPARM R1,&IMAGE(2)  LOAD LENGTH                      GP03034 00140403
         AGO   .FILLCOM                                         GP03034 00140503
.DEFILL  MACPARM R0,&IMAGE     LOAD RECORD ADDRESS              GP03034 00140603
&L       SETC  'L'''                                            GP03034 00140703
         MACPARM R1,&L&IMAGE   LOAD LENGTH                      GP03034 00140803
.FILLCOM AIF   ('&FILL' EQ '0' OR '&FILL' EQ 'X''0'''                  *00140903
               OR '&FILL' EQ 'X''00''').FILLZER                 GP03034 00141003
         ICM   R15,8,=AL1(&FILL)  INSERT FILL CHARACTER         GP03034 00141103
.FILLZER MVCL  R0,R14        MOVE INPUT RECORD                  GP03034 00141203
         EAR   R15,R0        RESTORE RETURN CODE                GP03034 00141303
.MEND    MEND  ,                                                GP03034 00141403
./ ADD NAME=INPOPEN  0100-04114-04114-2235-00022-00022-00000-GERHARD 00 00141503
         MACRO                                                          00141603
&NM      INPOPEN &WORK,&DEV=,&OPT=                    UPDATED ON 93307  00141703
         LCLA  &I,&J                                                    00141803
         LCLB  &A,&D,&W,&F,&X,&U                                GP04114 00141903
&J       SETA  N'&OPT                                                   00142003
.OPTL    ANOP  ,                                                        00142103
&I       SETA  &I+1                                                     00142203
         AIF   (&I GT &J).OPTN                                          00142303
         AIF   ('&OPT(&I)' EQ '').OPTL                                  00142403
&A       SETB  (&A OR '&OPT(&I)' EQ 'ABE' OR '&OPT(&I)' EQ 'ABEND')     00142503
&D       SETB  (&D OR '&OPT(&I)' EQ 'DUMMY')                            00142603
&W       SETB  (&W OR '&OPT(&I)' EQ 'NOWTO')                            00142703
&F       SETB  (&F OR '&OPT(&I)' EQ 'JFCB' OR '&OPT(&I)' EQ 'OPENJ')    00142803
&X       SETB  (&X OR '&OPT(&I)' EQ 'VER' OR '&OPT(&I)' EQ 'EXIST')     00142903
&U       SETB  (&U OR '&OPT(&I)' EQ 'FOLD')                     GP04114 00143003
         AGO   .OPTL                                                    00143103
.OPTN    AIF   (&J EQ (&A+&D+&W+&F+&X+&U)).OPTOK                GP04114 00143203
         MNOTE 4,'UNDEFINED OR DUPLICATE OPTION SPECIFIED'       82116  00143303
.OPTOK   ANOP  ,                                                 82116  00143403
&I       SETA  128*&A+64*&D+32*&W+16*&F+8*&X+1*&U               GP04114 00143503
&NM      INPCOM 0,0,2,&I,&WORK,DEV=&DEV                                 00143603
         MEND  ,                                                        00143703
./ ADD NAME=INPWORK  8002-08088-08104-1217-00033-00030-00000-GERHARD 00 00143803
         MACRO                                                          00143903
&NM      INPWORK &DD,&ALTDD,&WIDTH=80,&EODAD=1,&FILL=0,&EDIT=0,&JFCB=, *00144003
               &PDE=0,&PDS=NO,&BUF=                             GP08088 00144103
         LCLA  &PFG,&IPDS,&I,&J,&K                               89351  00144203
         LCLB  &I0,&I1,&I2,&I3,&I4,&I5,&I6,&I7                   89351  00144303
&K       SETA  N'&PDS                                            89351  00144403
         AIF   ('&BUF' NE '1').NOBUF1  NOT SINGLE BUFFER OPTION GP08088 00144503
&PFG     SETA  &PFG+1        SET ONE BUFFER ONLY                GP08088 00144603
.NOBUF1  AIF   ('&PDE' EQ '0').NOPDE                            GP08088 00144703
&PFG     SETA  &PFG+8        SHOW PDS FEEDBACK REQUESTED         89351  00144803
.NOPDE   AIF   (&I GE &K).ENDPDE                                 89351  00144903
&I       SETA  &I+1                                              89351  00145003
&I0      SETB  (&I0 OR ('&PDS(&I)' EQ 'DIR'))  PROCESS DIRECTORY 89351  00145103
&I1      SETB  (&I1 OR ('&PDS(&I)' EQ 'MEM'))  PROCESS MEMBERS   89351  00145203
&I2      SETB  (&I2 OR ('&PDS(&I)' EQ 'ALI'))  PROCESS ALIAS TOO 89351  00145303
&I7      SETB  (&I7 OR ('&PDS(&I)' EQ 'UPD'))  BUILD ./ ADD      89351  00145403
         AGO   .NOPDE        TRY NEXT ENTRY                      89351  00145503
.ENDPDE  AIF   ('&PDS' EQ 'NO').OKPDE                            89351  00145603
&IPDS    SETA  128*&I0+64*&I1+32*&I2+16*&I3+8*&I4+4*&I5+2*&I6+&I7       00145703
&J       SETA  &I0+&I1+&I2+&I3+&I4+&I5+&I6+&I7                   89351  00145803
         AIF   (&J EQ &K).OKPDE                                  89351  00145903
         MNOTE 4,'*** INVALID PDS= PARAMETER ***'                89351  00146003
.OKPDE   AIF   (T'&JFCB NE 'O').ADDJFCB                          82116  00146103
         DC    0F'0'                                                    00146203
&NM      DC    CL8'&DD ',CL8'&ALTDD ',A(&EODAD,&PDE),AL2(&WIDTH,0),AL1(*00146303
               &PFG,&FILL,&EDIT,&IPDS)                           89351  00146403
         MEXIT ,                                                 82116  00146503
.ADDJFCB ANOP  ,                                                 82116  00146603
&PFG     SETA  16+&PFG       SET JFCB PRESENT                    82116  00146703
         DC    0F'0'                                                    00146803
&NM      DC    CL8'&DD ',CL8'&ALTDD ',A(&EODAD,&PDE),AL2(&WIDTH,0),AL1(*00146903
               &PFG,&FILL,&EDIT,&IPDS),A(&JFCB)                  89351  00147003
         MEND  ,                                                        00147103
./ ADD NAME=LAT      0102-99126-08278-0024-00033-00036-00000-GERHARD 00 00147203
         MACRO ,                                                        00147303
&NM      LAT   &R,&ADDR,&BZ,&BNZ,&LA=FW                         GP98339 00147403
         GBLB  &MVSXA                                            91216  00147503
         GBLC  &MACPLAB                                                 00147603
.********************************************************************** 00147703
.*                                                                   ** 00147803
.*   LOAD AN ADDRESS INTO A REGISTER AND TEST FOR ZERO; OPTIONALLY   ** 00147903
.*     BRANCH ON ZERO BZ=label  OR NONZERO BNZ=label                 ** 00148003
.*                                                                   ** 00148103
.********************************************************************** 00148203
&MACPLAB SETC  '&NM'                                                    00148303
         AIF   ('&LA' EQ '').ICM370                                     00148403
         AIF   ('&LA' EQ '34' OR '&LA' EQ '34Z').ICM34           91216  00148503
         AIF   ('&LA' EQ 'RX').ICMRX                                    00148603
         AIF   ('&LA' EQ '0').ICMSR                                     00148703
         AIF   ('&LA' EQ 'FW').ICMFW                                    00148803
         MNOTE 8,'&&LA=&LA INVALID - MUST BE RX, 0, OR FW'              00148903
         MNOTE 8,'&&LA=RX ASSUMED'                                      00149003
.ICMRX   MACPARM &R,&ADDR,OP=LA  OFFSET(X,BASE)                         00149103
         MACPARM &R(1),7,1(&R(1)),OP=ICM,MODE=THREE                     00149203
         AGO   .BRT                                                     00149303
.ICMSR   MACPARM &R,0                                                   00149403
         AGO   .ICM370                                           92273  00149503
.ICM34   AIF   (&MVSXA).ICMFW                                    91216  00149603
.ICM370  MACPARM &R(1),7,1+&ADDR,OP=ICM,MODE=THREE  LOAD ADDRESS        00149703
         AGO   .BRT                                                     00149803
.ICMFW   MACPARM &R(1),15,&ADDR,OP=ICM,MODE=THREE  LOAD FULLWORD        00149903
         AIF   ('&LA' NE '34Z').BRT                              91216  00150003
         MACPARM &R(1),0(,&R(1))                                 91216  00150103
.*                                                                      00150203
.BRT     MACPARM &BZ,OP=BZ,OPR=BZR,MODE=ONE,NULL=SKIP           GP06266 00150303
         MACPARM &BNZ,OP=BNZ,OPR=BNZR,MODE=ONE,NULL=SKIP        GP06266 00150403
.MEX     MEND  ,                                                        00150503
./ ADD NAME=LPALOOK  0103-03261-06263-0020-00051-00025-00025-GERHARD 00 00150603
         MACRO ,                                                        00150703
&NM      LPALOOK &EP=,&EPLOC=,&DCB=,&MEMBER=,&ALIAS=,&ERR=              00150803
.*--------------------------------------------------------------------* 00150903
.*  LPALOOK INVOKES SUBROUTINE SUBLPALK, WHICH USES CSVQUERY TO LOOK  * 00151003
.*    FOR THE MODULE REQUESTED BY EITHER EP=, OR NAMED IN EPLOC.      * 00151103
.*  WHEN THE DCB IS NON-ZERO, THE MODULE IS LOADED IF NOT IN AN LPA   * 00151203
.*    LIST. LOAD USES DCB=0 WHEN DCB PARAMETER<256                    * 00151303
.*                                                                    * 00151403
.*  AN EXTRN IS ISSUED UNLESS THE MODULE WAS NAMED IN A SERVLOAD REQ. * 00151503
.*--------------------------------------------------------------------* 00151603
         GBLC  &MACPLAB                                                 00151703
         GBLC  &SRVLMOD(20),&SRVLDEL(20)                                00151803
         GBLB  &MVSXA                                           GP04234 00151903
         GBLB  &SRVBMOD(20)                                             00152003
         GBLA  &SRVNMOD                                                 00152103
         GBLB  &ZLPAKFG                                                 00152203
         LCLA  &I                                                       00152303
         LCLC  &CALLMOD                                                 00152403
&CALLMOD SETC  '=A(SUBLPALK)'    LPA LOOKUP/LOAD MODULE                 00152503
&MACPLAB SETC  '&NM'                                                    00152603
.LOOKLUK AIF   (&I GE &SRVNMOD).SKIPLUK  NOT IN SERVLOAD LIST           00152703
&I       SETA  &I+1                                                     00152803
         AIF   ('&SRVLDEL(&I)' NE 'SUBLPALK').LOOKLUK                   00152903
&CALLMOD SETC  '&SRVLMOD(&I)'  USE SERVLOAD ADDRESS                     00153003
         AGO   .COMMLUK                                                 00153103
.SKIPLUK AIF   (&ZLPAKFG).COMMLUK                                       00153203
         EXTRN SUBLPALK                                                 00153303
&ZLPAKFG SETB  1                                                        00153403
.COMMLUK AIF   ('&EP' EQ '' AND '&EPLOC' EQ '').OMIT                    00153503
         AIF   ('&EP' NE '' AND '&EPLOC' NE '').DUPE                    00153603
         MACPARM R0,&DCB,NULL=0                                         00153703
         AIF   ('&EP' EQ '').NOEP                                       00153803
         MACPARM R1,=CL8'&EP '                                          00153903
         AGO   .COMMON                                                  00154003
.NOEP    MACPARM R1,&EPLOC                                              00154103
.COMMON  MACPARM R15,&CALLMOD,OP=L                                      00154203
         AIF   (&MVSXA).DOBAS                                   GP04234 00154303
         MACPARM R14,(R15),OP=BAL,OPR=BALR                              00154403
         AGO   .NOBAS                                                   00154503
.DOBAS   MACPARM R14,(R15),OP=BAS,OPR=BASR                              00154603
.NOBAS   AIF ('&MEMBER' EQ '' AND '&ALIAS' EQ '' AND '&ERR' EQ '').MEND 00154703
         MACPARM R15,=H'4',OP=CH  CHECK RETURN CODE                     00154803
         MACPARM &MEMBER,OP=BL,OPR=BLR,MODE=ONE,NULL=SKIP               00154903
         MACPARM &ALIAS,OP=BE,OPR=BER,MODE=ONE,NULL=SKIP                00155003
         MACPARM &ERR,OP=BH,OPR=BHR,MODE=ONE,NULL=SKIP                  00155103
         MEXIT ,                                                 81169  00155203
.OMIT    MNOTE 8,'NEITHER EP= NOR EPLOC= SUPPLIED'               81169  00155303
         AGO   .DEFLAB                                           81169  00155403
.DUPE    MNOTE 8,'EP= AND EPLOC= ARE MUTUALLY EXCLUSIVE'         81169  00155503
.DEFLAB  MACPARM MODE=LBL    EXPAND LABEL ONLY                          00155603
.MEND    MEND  ,                                                        00155703
./ ADD NAME=MACLIST  8007-04317-05197-2212-00048-00019-00000-GERHARD 00 00155803
         MACRO ,                                                        00155903
         MACLIST &LIST                                 NEW 2004.234 GYP 00156003
.*--------------------------------------------------------------------* 00156103
.*   MACLIST IS USED TO COMPENSATE FOR ASSEMBLER F/XF INABILITY TO    * 00156203
.*     PROVIDE N'&LIST(&I,&N) SUPPORT.                                * 00156303
.*   THIS MACRO STRIPS UP TO 10 INNER VALUES                          * 00156403
.*   E.G.       MYMAC OPTS=(A,(B1,B2,B3),(C1,C2))                     * 00156503
.*    HLASM N'&OPTS(2) IS 3, N'&OPTS(3) IS 2                          * 00156603
.*    XFASM FAILS ALL BUT N'&OPTS                                     * 00156703
.*                                                                    * 00156803
.*    USE:   MACLIST &OPTS(2)                                         * 00156903
.*    RESULTS RETURNED IN &MACP1-&MACP10, COUNT IN &MACP#             * 00157003
.*--------------------------------------------------------------------* 00157103
         GBLA  &MACP#        NUMBER OF (SUB)LIST ARGUMENTS              00157203
         GBLC  &MACP1,&MACP2,&MACP3,&MACP4,&MACP5                       00157303
         GBLC  &MACP6,&MACP7,&MACP8,&MACP9,&MACP10                      00157403
         LCLA  &I,&J,&K,&N                                              00157503
         LCLC  &STR,&LAB                                                00157603
         LCLC  &VAL(10)                                                 00157703
&STR     SETC  '&LIST'                                                  00157803
&J       SETA  1             OUTPUT SUBSCRIPT                           00157903
&K       SETA  K'&LIST                                                  00158003
         AIF   (&K LT 1).DONE                                           00158103
         AIF   ('&STR'(1,1) NE '(' OR '&STR'(&K,1) NE ')').NOPARS       00158203
&STR     SETC  '&STR'(2,&K-2)                                           00158303
&K       SETA  K'&STR                                                   00158403
.NOPARS  AIF   (&I GE &K).DONE                                          00158503
&I       SETA  &I+1                                                     00158603
         AIF   ('&STR'(&I,1) EQ ',').NEWPRM                             00158703
&VAL(&J) SETC  '&VAL(&J)'.'&STR'(&I,1)                                  00158803
         AGO   .NOPARS                                                  00158903
.NEWPRM  AIF   (&J GE 10).TOOMANY                                       00159003
&J       SETA  &J+1                                                     00159103
         AGO   .NOPARS                                                  00159203
.TOOMANY MNOTE 8,'MACLIST SUPPORTS MAX OF 10 SUBLIST ITEMS'             00159303
.DONE    ANOP  ,                                                        00159403
&MACP#   SETA  &J            GET NUMBER OF OPERANDS                     00159503
&MACP1   SETC  '&VAL(1)'                                                00159603
&MACP2   SETC  '&VAL(2)'                                                00159703
&MACP3   SETC  '&VAL(3)'                                                00159803
&MACP4   SETC  '&VAL(4)'                                                00159903
&MACP5   SETC  '&VAL(5)'                                                00160003
&MACP6   SETC  '&VAL(6)'                                                00160103
&MACP7   SETC  '&VAL(7)'                                                00160203
&MACP8   SETC  '&VAL(8)'                                                00160303
&MACP9   SETC  '&VAL(9)'                                                00160403
&MACP10  SETC  '&VAL(10)'                                               00160503
.MEND    MEND  ,                                                        00160603
./ ADD NAME=MACPARM  8002-08090-08279-1431-00329-00454-00000-GERHARD 00 00160703
         MACRO                                                          00160803
&NM    MACPARM &OP1,         FIRST OPERAND (USUALLY R1 FOR LA/LR)      *00160903
               &OP2,         SECOND OPERAND (R2/B2D2 OR R3 IF MODE=3   *00161003
               &OP3,         THIRD OPERAND (B2D2 WHEN MODE=3)          *00161103
               &OP4,         FOURTH OPERAND  (WHEN MODE=4)             *00161203
               &OP=LA,       OPCODE WHEN OP2 IS NOT A REGISTER         *00161303
               &OPM=,        OPCODE WHEN OP2 IS NEGATED AND NOT REG.   *00161403
               &OPR=LR,      OPCODE WHEN OP2 IS REGISTER               *00161503
               &OPMR=LCR,    OPCODE WHEN OP2 IS NEGATED REGISTER       *00161603
               &QUOTE=,      OPCODE FOR QUOTED STRING/EXPLICIT LEN     *00161703
               &MODE=,       ONE/THREE/REV/EQU/EVEN                    *00161803
               &NAME=,       OUTER MACRO FOR MNOTES                    *00161903
               &OMIT=NO,     SKIP COMPLETELY IF BLANK                  *00162003
               &NULL=  SKIP, YES, OR OPERAND TO USE FOR NULL &OP2       00162103
.*                                                              GP00196 00162203
.*   THIS IS AN INNER MACRO USED TO CONVERT MACRO PARAMETERS TO         00162303
.*     INSTRUCTIONS APPROPRIATE TO THE OPERAND TYPE.                    00162403
.*                                                                      00162503
.*   THIS MACRO WAS SUGGESTED BY A MUCH OLDER VERSION (LODE/LODESTAR)   00162603
.*     BY SEYMOUR (SHMUEL) J. METZ THAT HANDLED TWO OPERANDS ONLY.      00162703
.*     NONE OF THE ORIGINAL CODE IS USED HEREIN.                        00162803
.*                                                                      00162903
.*   WITH DEFAULTS, IT EXPANDS:                                         00163003
.*    MACPARM R5,WORD      AS   LA R5,WORD                              00163103
.*    MACPARM R5,(R5)      AS   NOTHING (LABEL IS SAVED IN MACPLAB)     00163203
.*    MACPARM R5,(R4)      AS   LR R5,R4                                00163303
.*                                                                      00163403
.*   IN ORDER TO BE RECOGNIZED AS MATCHING, REGISTER SPECIFICATIONS     00163503
.*    SHOULD BE MADE IN ABSOLUTE FORM (0)-(15), OR MNEMONIC (R0)-(R15). 00163603
.*    OP1 NORMALLY DOES NOT NEED THE PARENTHESES.                       00163703
.*                                                                      00163803
.*   TO AVOID CONFLICTS WITH REGISTER SPECIFICATIONS, EXPRESSIONS MUST  00163903
.*    EITHER BEGIN WITHOUT A PARENTHESIS, OR WITH TWO: ((B-A)/(C-A))    00164003
.*                                                                      00164103
.*    OP2 PARAMETER IS AN EXPRESSION OR (REG)                           00164203
.*       EITHER FORM MAY BE PREFIXED BY A MINUS SIGN                    00164303
.*       LA REQUESTS MAY BE PREFIXED BY / TO USE L =A(OP2)              00164403
.*       LA REQUESTS MAY BE PREFIXED BY * TO USE L ,OP2                 00164503
.*       FOR A NEGATED SECOND OPERAND, THE EXPANSION WILL USE           00164603
.*       &OPMR FOR REGISTER, &OPM IF SPECIFIED, OR &OP/LNR              00164703
.*    MODE=REV      FOR &OPR, REVERSE REGISTERS                         00164803
.*    MODE=EQU      IF FIRST=SECOND OPERAND, EXPAND ANYWAY              00164903
.*    MODE=NONE     EXPAND OP= ONLY; EITHER NO OPERAND OR OPT.  GP03144 00165003
.*                    OPERAND. (MAY BE ENCLOSED IN QUOTES)      GP03144 00165103
.*    MODE=ONE      SINGLE OPERAND (E.G., BX, BXR TYPE)                 00165203
.*    MODE=THREE    THREE OPERAND TYPE; EXPANDS &OP &OP1,&OP2,&OP3      00165303
.*    MODE=FOUR     FOUR OPERAND TYPE; EXPANDS &OP &OP1,&OP2,&OP3,&OP4  00165403
.*    MODE=EVEN     EXPAND (TWO OPERAND FORM) EVEN WHEN SAME    GP01028 00165503
.*    MODE=LBL      NO OPERANDS - EXPANDS PENDING LABEL(S)              00165603
.*                    OPERAND 1 - OPTIONAL ALIGNMENT (E.G., 0F) GP03144 00165703
.*    NULL=         OMITTED PARM CAUSES ASSEMBLY ERROR (?)              00165803
.*    NULL=YES      NULL FINAL PARAMETER EXPANDS WITHOUT PARM           00165903
.*    NULL=TERM     EXPANSION USES SUPPLIED TERM IF PARM=NULL           00166003
.*    NULL=SKIP     NULL FINAL PARAMETER SKIPS EXPANSION                00166103
.*    NAME=         (OPTIONAL) NAME OF OUTER MACRO FOR MNOTES           00166203
.*                                                                      00166303
.*    QUOTE=(LA,8)  TURNS  'TEXT' INTO   LA RX,=CL(8)'TEXT' (MODE 2)    00166403
.*                                                                      00166503
         GBLC  &MACPLAB,&MACPSIZ,&MACQSTR                       GP08090 00166603
         GBLB  &MACPERR,&MACPNUL,&MVS,&MVSXA,&MVSESA            GP00196 00166703
         GBLB  &MACQUOT                                         GP08090 00166803
         GBLA  &MACPLEN                                         GP08090 00166903
         LCLA  &K,&I,&J                                         GP08090 00167003
         LCLB  &MINUS,&MOD0,&MOD1,&MOD3,&MOD4,&MODQ,&MODR,&MODV         00167103
         LCLB  &FGR1,&FGR2   ON WITH REGISTER OPERAND                   00167203
         LCLC  &FD1,&FD2,&FD3,&FD4,&LBL,&OPRR,&MNONM,&OPLA,&L   GP08090 00167303
         AIF   ('&OMIT' EQ '').NO  SKIP COMPLETELY IF NULL      GP06277 00167403
&MNONM   SETC  'MACPARM:'                                               00167503
&MACPERR SETB  0             RESET RETURN FLAG                  GP00196 00167603
&MACPNUL SETB  0             RESET RETURN FLAG                  GP00196 00167703
&OPLA    SETC  '&OP'         MAY NEED UPDATING                  GP08090 00167803
&MACPSIZ SETC  ''                                               GP08090 00167903
         AIF   ('&NM' EQ '').NONAME                                     00168003
&MNONM   SETC  '&NAME'.'/MACPARM:'                                      00168103
.NONAME  ANOP  ,                                                        00168203
&MOD0    SETB  ('&MODE' EQ 'NONE' OR '&MODE' EQ '0')            GP03144 00168303
&MOD1    SETB  ('&MODE' EQ 'ONE' OR '&MODE' EQ '1')                     00168403
&MOD3    SETB  ('&MODE' EQ 'THREE' OR '&MODE' EQ '3')                   00168503
&MOD4    SETB  ('&MODE' EQ 'FOUR' OR '&MODE' EQ '4')            GP00196 00168603
&MODQ    SETB  ('&MODE' EQ 'EQU' OR '&MODE' EQ 'EQUAL')                 00168703
&MODR    SETB  ('&MODE' EQ 'REV' OR '&MODE' EQ 'REVERSE')               00168803
&MODV    SETB  ('&MODE' EQ 'EVEN' OR '&MODE' EQ 'SAME')         GP01028 00168903
.*                                                                      00169003
.*  TEST FOR UNUSED LABEL EXPANSION ONLY                                00169103
.*                                                                      00169203
&LBL     SETC  '&NM'         INDICATE LOCAL LABEL UNUSED                00169303
         AIF   ('&MODE' NE 'LBL' AND '&MODE' NE 'LABEL').NOTLBL         00169403
&FD1     SETC  '&OP1(1)'     ALLOW USER TO SPECIFY ALIGNMENT    GP03144 00169503
         AIF   ('&FD1' NE '').XAV                               GP03144 00169603
&FD1     SETC  '0H'          DEFAULT ALIGNMENT                  GP03144 00169703
.XAV     AIF   ('&MACPLAB' EQ '').XNM                                   00169803
         AIF   ('&MACPLAB' EQ '&LBL').X1LBL                             00169903
&MACPLAB DS    &FD1                                             GP03144 00170003
.X1LBL   ANOP  ,                                                        00170103
&MACPLAB SETC  ''                                                       00170203
.XNM     AIF   ('&LBL' EQ '').XNOP                                      00170303
&NM      DS    &FD1                                             GP03144 00170403
&LBL     SETC  ''            LOCAL LABEL EXPANDED                       00170503
.XNOP    AIF   (T'&OP2 EQ 'O' AND T'&OP3 EQ 'O'                        *00170603
               AND T'&OP4 EQ 'O').MEXIT                                 00170703
         MNOTE 4,'&MNONM POSITIONAL PARAMETERS IGNORED'                 00170803
&MACPERR SETB  1             RETURN ERROR                       GP00196 00170903
.MEXIT   MEXIT ,                                                        00171003
.*                                                                      00171103
.*  TEST FOR CORRECT MODE OPERAND                                       00171203
.*                                                                      00171303
.NOTLBL  AIF   ('&MODE' EQ '' OR &MOD0 OR &MOD1 OR &MOD3 OR &MODQ      *00171403
               OR &MODR OR &MODV).MODG                          GP03144 00171503
         MNOTE 8,'&MNONM INVALID MODE=&MODE '                           00171603
&MACPERR SETB  1             RETURN ERROR                       GP00196 00171703
.*                                                                      00171803
.*  CHECK LOCAL VS. GLOBAL LABEL, EXPAND GLOBAL AND RELOAD              00171903
.*                                                                      00172003
.MODG    AIF   ('&MACPLAB' EQ '' OR '&LBL' EQ '').N2LBL                 00172103
&MACPLAB DS    0H                                                       00172203
         AGO   .PROPLBL      PROPAGATE LOCAL LABEL                      00172303
.N2LBL   AIF   ('&MACPLAB' NE '').NOLAB                                 00172403
.PROPLBL ANOP  ,                                                        00172503
&MACPLAB SETC  '&LBL'        NO GLOBAL LABEL - USE LOCAL                00172603
&LBL     SETC  ''                                                       00172703
.*                                                                      00172803
.*  TEST FOR CORRECT NUMBER OF PARAMETERS, AND SUBSTITUTE &NULL         00172903
.*                                                                      00173003
.NOLAB   AIF   (NOT &MOD0).NOTNONE   OPCODE ONLY ?              GP03144 00173103
&FD1     SETC  '&OP1'                                           GP03144 00173203
         AIF   ('&FD1' EQ '').DONONE                            GP03144 00173303
         AIF   ('&FD1'(1,1) NE '"').DONONE                      GP03144 00173403
&FD1     SETC  '&FD1'(2,K'&FD1-2)                               GP03144 00173503
.DONONE  ANOP  ,                                                GP03144 00173603
&MACPLAB &OP   &FD1                                             GP03144 00173703
         AIF   (T'&OP2 EQ 'O' AND T'&OP3 EQ 'O'                        *00173803
               AND T'&OP4 EQ 'O').GO                            GP03144 00173903
         MNOTE 4,'&MNONM POSITIONAL PARAMETERS IGNORED'         GP03144 00174003
&MACPERR SETB  1             RETURN ERROR                       GP03144 00174103
         AGO   .GO                                              GP03144 00174203
.NOTNONE ANOP  ,                                                        00174303
&FD1     SETC  '&OP1'                                                   00174403
&FD2     SETC  '&OP2'                                                   00174503
&FD3     SETC  '&OP3'                                                   00174603
&FD4     SETC  '&OP4'                                           GP00196 00174703
         AIF   (T'&OP1 NE 'O').HAVE1                                    00174803
&MACPNUL SETB  1             RETURN NULL FLAG                   GP00196 00174903
         AIF   (NOT &MOD1).NOTONE                                       00175003
         AIF   ('&NULL' EQ '').NOTONE                                   00175103
         AIF   ('&NULL' EQ 'YES').HAVE1  NOTHING ELSE TO DO     GP01009 00175203
         AIF   ('&NULL' EQ 'SKIP').MEXIT  SKIP OUT WITHOUT      GP01009 00175303
&FD1     SETC  '&NULL'                                                  00175403
         AGO   .HAVE1                                                   00175503
.NOTONE  MNOTE 8,'&MNONM FIRST POSITIONAL OPERAND REQUIRED'             00175603
&MACPERR SETB  1             RETURN ERROR                       GP00196 00175703
         AGO   .MEXIT                                                   00175803
.HAVE1   AIF   (&MOD1).HAVEALL                                          00175903
         AIF   (T'&OP2 NE 'O').HAVE2                                    00176003
&MACPNUL SETB  1             RETURN NULL FLAG                   GP00196 00176103
         AIF   (&MOD3).NOTTWO                                           00176203
         AIF   ('&NULL' EQ '').NOTTWO                                   00176303
         AIF   ('&NULL' EQ 'YES').HAVE2  NOTHING ELSE TO DO     GP01009 00176403
         AIF   ('&NULL' EQ 'SKIP').MEXIT  SKIP OUT WITHOUT      GP01009 00176503
&FD2     SETC  '&NULL'                                                  00176603
         AGO   .HAVE2                                                   00176703
.NOTTWO  MNOTE 8,'&MNONM SECOND POSITIONAL OPERAND REQUIRED'            00176803
&MACPERR SETB  1             RETURN ERROR                       GP00196 00176903
         AGO   .MEXIT                                                   00177003
.HAVE2   AIF   (NOT &MOD3).HAVE3                                GP00196 00177103
         AIF   (T'&OP3 NE 'O').HAVE3                            GP00196 00177203
&MACPNUL SETB  1             RETURN NULL FLAG                   GP00196 00177303
         AIF   ('&NULL' EQ '').NOTHREE                                  00177403
         AIF   ('&NULL' EQ 'YES').HAVEALL  NOTHING ELSE TO DO   GP01009 00177503
         AIF   ('&NULL' EQ 'SKIP').MEXIT  SKIP OUT WITHOUT      GP01009 00177603
&FD3     SETC  '&NULL'                                                  00177703
         AGO   .HAVEALL                                                 00177803
.NOTHREE MNOTE 8,'&MNONM THIRD POSITIONAL OPERAND REQUIRED'             00177903
&MACPERR SETB  1             RETURN ERROR                       GP00196 00178003
         AGO   .MEXIT                                                   00178103
.HAVE3   AIF   (NOT &MOD4).HAVEALL                              GP00196 00178203
         AIF   (T'&OP4 NE 'O').HAVEALL                          GP00196 00178303
&MACPNUL SETB  1             RETURN NULL FLAG                   GP00196 00178403
         AIF   ('&NULL' EQ '').NOFOUR                           GP00196 00178503
         AIF   ('&NULL' EQ 'YES').HAVEALL  NOTHING ELSE TO DO   GP01009 00178603
         AIF   ('&NULL' EQ 'SKIP').MEXIT  SKIP OUT WITHOUT      GP01009 00178703
&FD4     SETC  '&NULL'                                          GP00196 00178803
         AGO   .HAVEALL                                         GP00196 00178903
.NOFOUR  MNOTE 8,'&MNONM FOURTH POSITIONAL OPERAND REQUIRED'    GP03207 00179003
&MACPERR SETB  1             RETURN ERROR                       GP00196 00179103
         AGO   .MEXIT                                           GP00196 00179203
.*                                                                      00179303
.*  CHANGE OP1 AND OP2 (UNLESS MOD1 OR MOD3) TO PREFERRED FORM          00179403
.*    IF MODE 3, GO TO EXPAND IT                                        00179503
.*                                                                      00179603
.HAVEALL AIF   (&MOD3).DO3   SIMPLE EXPANSION OF THREE OPERANDS         00179703
         AIF   (&MOD4).DO4   SIMPLE EXPANSION OF FOUR OPERANDS  GP00196 00179803
         AIF   (NOT &MOD1).CLNOP1                                       00179903
&K       SETA  K'&FD1                                                   00180003
         AIF   (&K LT 2 OR '&FD1'(1,1) NE '-').CLNOP1                   00180103
&MINUS   SETB  1                                                        00180203
&FD1     SETC  '&FD1'(2,&K-1)                                           00180303
&K       SETA  K'&FD1                                                   00180403
.CLNOP1  ANOP  ,                                                        00180503
         AIF   (&K LT 3).NORG1                                          00180603
         AIF   ('&FD1'(1,1) NE '(' OR '&FD1'(&K,1) NE ')').NORG1        00180703
         AIF   ('&FD1'(2,1) EQ '(').NOSY1        ((EXPRESSION)) ?       00180803
&FGR1    SETB  1             FLAG OP1 AS REGISTER EXPRESSION            00180903
&FD1     SETC  '&FD1'(2,&K-2)                                           00181003
&K       SETA  K'&FD1                                                   00181103
.*  LOOK FOR SINGLE OR DOUBLE DIGIT - PREFIX BY R                       00181203
.NORG1   AIF   (&K LT 1 OR &K GT 2).NOSY1                               00181303
         AIF   ('&FD1'(1,1) LT '0').NOSY1  LEAVE IF NOT NUMERIC         00181403
&FD1     SETC  'R'.'&FD1'    MAKE SYMBOLIC REGISTER                     00181503
.*                                                                      00181603
.*  HAVE OP1 CLEANED FROM (N) TO RN; GO TO EXPAND MODE 1                00181703
.*    ELSE TEST AND CLEAN OPERAND 2                                     00181803
.*                                                                      00181903
.NOSY1   AIF   (&MOD1).DO1                                              00182003
&K       SETA  K'&FD2                                                   00182103
&OPRR    SETC  '&OPR'                                                   00182203
         AIF   (&K LT 2 OR '&FD2'(1,1) NE '-').NONEG2                   00182303
&MINUS   SETB  1                                                        00182403
&FD2     SETC  '&FD2'(2,&K-1)                                           00182503
&K       SETA  K'&FD2                                                   00182603
&OPRR    SETC  '&OPMR'                                                  00182703
.NONEG2  AIF   (&K LT 3).NORG2                                          00182803
         AIF   ('&FD2'(1,1) NE '''' OR T'&QUOTE EQ 'O').NOQUO2  GP08090 00182903
         AIF   ('&FD2'(&K,1) NE '''').NOQUO2                    GP08090 00183003
         MACQOLIT &FD2,LEN=&QUOTE(2)                            GP08090 00183103
         AIF   (&MACPERR OR &MACPNUL).NOQUO2                    GP08090 00183203
&FD2     SETC  '&MACQSTR'                                       GP08090 00183303
&MACPSIZ SETC  '&MACPLEN'    RETURN LENGTH                      GP08090 00183403
         AIF   ('&QUOTE(1)' EQ '').LOPP                         GP08090 00183503
&OPLA    SETC  '&QUOTE(1)'                                      GP08090 00183603
         AGO   .LOPP                                            GP08090 00183703
.NOQUO2  AIF   ('&FD2'(1,1) NE '(' OR '&FD2'(&K,1) NE ')').NORG2        00183803
         AIF   ('&FD2'(2,1) EQ '(').NORG2        ((EXPRESSION)) ?       00183903
&FD2     SETC  '&FD2'(2,&K-2)                                           00184003
&K       SETA  K'&FD2                                                   00184103
&FGR2    SETB  1             FLAG OP1 AS REGISTER EXPRESSION            00184203
         AIF   ('&FD2'(1,1) LT '0').NOSY2  LEAVE IF NOT NUMERIC         00184303
         AIF   (&K LT 1 OR &K GT 2).NOSY2                               00184403
&FD2     SETC  'R'.'&FD2'    MAKE SYMBOLIC REGISTER                     00184503
.*                                                                      00184603
.*  REG: CHECK FOR NEG PREFIX, MODE=EQU, ELSE IF OP1=OP2, NO EXPANSION  00184703
.*                                                                      00184803
.NOSY2   AIF   ('&FD2' NE '&FD1' OR &MINUS OR &MODV).LR         GP01028 00184903
         AIF   (NOT &MODQ).NO                                           00185003
.LR      AIF   (NOT &MODR).NOREV                                        00185103
&MACPLAB &OPRR &FD2,&FD1                                                00185203
         AGO   .GO                                                      00185303
.*                                                                      00185403
.*  REG: NORMAL FORM, (EXPANDS MINUS, ALSO - OPR OR OPMR IN OPRR)       00185503
.*                                                                      00185603
.NOREV   ANOP  ,                                                        00185703
&MACPLAB &OPRR &FD1,&FD2                                                00185803
         AGO   .GO                                                      00185903
.*                                                                      00186003
.*  NOT REG: CHECK FOR LA AND SPECIAL CASES                             00186103
.*                                                                      00186203
.NORG2   ANOP  ,                                                GP08090 00186303
&MACPSIZ SETC  '&L'.'&FD2'   RETURN LENGTH FOR USUAL CASE       GP08090 00186403
         AIF   ('&OPLA' EQ 'LA' AND '&FD2' EQ '0').SR                   00186503
         AIF   ('&OPM' NE '' AND &MINUS).OPM                            00186603
.*                                                                      00186703
.*  LA OP1,/OP2   GENERATES L OP1,=A(OP2)                               00186803
.*                                                                      00186903
         AIF   ('&OPLA' NE 'LA' OR '&FD2'(1,1) NE '/').LOP              00187003
&FD2     SETC  '&FD2'(2,K'&FD2-1)                                       00187103
&MACPLAB L     &FD1,=A(&FD2)                                            00187203
         AGO   .LOPCO                                                   00187303
.*                                                                      00187403
.*  LA OP1,*OP2   GENERATES L OP1,OP2                                   00187503
.*                                                                      00187603
.LOP     AIF   ('&OPLA' NE 'LA' OR '&FD2'(1,1) NE '*').LOPP             00187703
         AIF   (K'&FD2 EQ 1).LOPP                                       00187803
&FD2     SETC  '&FD2'(2,K'&FD2-1)                                       00187903
&MACPLAB L     &FD1,&FD2                                                00188003
         AGO   .LOPCO                                                   00188103
.LOPP    ANOP  ,                                                        00188203
&MACPLAB &OPLA &FD1,&FD2                                                00188303
.LOPCO   AIF   (NOT &MINUS).GO                                          00188403
         &OPMR &FD1,&FD1                                                00188503
         AGO   .GO                                                      00188603
.*                                                                      00188703
.*  USER SPECIFIED OPM AND -OP2                                         00188803
.*                                                                      00188903
.OPM     ANOP  ,                                                        00189003
&MACPLAB &OPM  &FD1,&FD2                                                00189103
         AGO   .GO                                                      00189203
.*                                                                      00189303
.*  SINGLE OPERAND INSTRUCTION - EXPAND, CHECK FOR -OP1                 00189403
.*                                                                      00189503
.DO1     AIF   (&FGR1).DO1REG                                           00189603
         AIF   ('&OPM' NE '' AND &MINUS).DO1NEG                         00189703
&MACPLAB &OP   &FD1                                                     00189803
         AIF   (NOT &MINUS).GO                                          00189903
         &OPMR &FD1                                                     00190003
         AGO   .GO                                                      00190103
.DO1NEG  ANOP  ,                                                        00190203
&MACPLAB &OPM  &FD1                                                     00190303
         AGO   .GO                                                      00190403
.DO1REG  AIF   (&MINUS).DO1MIN                                          00190503
&MACPLAB &OPR  &FD1                                                     00190603
         AGO   .GO                                                      00190703
.DO1MIN  ANOP  ,                                                        00190803
&MACPLAB &OPMR &FD1                                                     00190903
         AGO   .GO                                                      00191003
.*                                                                      00191103
.*  EXPAND THREE OPERAND INSTRUCTIONS - NO SPECIAL CHECKING             00191203
.*                                                                      00191303
.DO3     ANOP  ,                                                        00191403
&MACPLAB &OPLA &FD1,&FD2,&FD3                                           00191503
         AGO   .GO                                                      00191603
.*                                                              GP00196 00191703
.*  EXPAND FOUR OPERAND INSTRUCTIONS - NO SPECIAL CHECKING      GP00196 00191803
.*                                                              GP00196 00191903
.DO4     ANOP  ,                                                GP00196 00192003
&MACPLAB &OPLA &FD1,&FD2,&FD3,&FD4                              GP00196 00192103
         AGO   .GO                                              GP00196 00192203
.*                                                                      00192303
.*  ON SOME OLD MACHINES LA,0 WAS SLOWER AND LONGER. RETAIN SR ?        00192403
.*                                                                      00192503
.SR      ANOP                                                           00192603
&MACPLAB SR    &FD1,&FD1                                                00192703
.*                                                                      00192803
.*  INSTRUCTION(S) EXPANDED; CLEAR LABEL                                00192903
.*                                                                      00193003
.GO      ANOP  ,                                                        00193103
&MACPLAB SETC  ''                                                       00193203
.*                                                                      00193303
.*  NOTHING EXPANDED - MAINTAIN LABELS                                  00193403
.*                                                                      00193503
.NO      MEND  ,                                                        00193603
./ ADD NAME=MACQOLIT 8002-06357-08090-1652-00092-00180-00000-GERHARD 00 00193703
         MACRO ,                                                        00193803
       MACQOLIT &STR,&LEN=   DETERMINE LENGTH OF STRING; MAKE QUOTED    00193903
.*                                                                      00194003
.*   INNER MACRO FOR MACRO PROCESSING                                   00194103
.*       MACQOLIT &STR  WHERE &STR IS UNQUOTED, QUOTED, OR CONSTANT     00194203
.*                 FORMAT (E.G.,  XYZ, 'text', X'12AB', CL8'HI', =C'A') 00194303
.*   RETURNS:                                                           00194403
.*       MACPNUL   FOR OMITTED PARAMETER OR EMPTY STRING ('')           00194503
.*       MACQUOT=0  IF UNQUOTED; &STR IN MACQSTR, K'&STR IN MACPLEN     00194603
.*       MACQUOT=1  =CLnn'text' OR =X'hex' - LITERAL FORMAT             00194703
.*                                                                      00194803
         GBLA  &MACPLEN      RETURN SIGNIFICANT LENGTH OF STRING        00194903
         GBLB  &MACPNUL      TRUE IF NULL STRING                        00195003
         GBLB  &MACQUOT      TRUE IF ORIGINAL WAS QUOTED                00195103
         GBLB  &MACPERR      TRUE IF ERROR                              00195203
         GBLC  &MACQSTR      RETURN QUOTED STRING                       00195303
         LCLA  &I,&J,&K,&L                                              00195403
         LCLC  &C,&D,&TYPE                                              00195503
.*                                                                      00195603
&MACQUOT SETB  0             SET UNQUOTED                               00195703
&MACPERR SETB  0             SET NOT IN ERROR                           00195803
&MACPLEN SETA  K'&STR        SET PROVISIONAL LENGTH                     00195903
&MACQSTR SETC  '&STR'        DEFAULT - RETURN AS IS                     00196003
&TYPE    SETC  'C'           SET STRING TYPE (C OR X) DEFAULT           00196103
.*  RETURN IF STRING IS NULL                                            00196203
&MACPNUL SETB  (T'&STR EQ 'O')                                          00196303
&MACPNUL SETB  (&MACPNUL OR ('&STR' EQ ''''''))                         00196403
         AIF   (&MACPNUL).MEND   DONE IF NULL STRING                    00196503
.*  RETURN IF STRING IS UNQUOTED                                        00196603
         AIF   (&MACPLEN LT 2).SHORT                                    00196703
&MACQUOT SETB  ('&STR'(1,1) EQ '''' OR '&STR'(&MACPLEN,1) EQ '''')      00196803
.SHORT   AIF   (NOT &MACQUOT).MEND                                      00196903
.*  DELETE LITERAL'S EQUAL SIGN IF PRESENT                              00197003
         AIF   ('&STR'(1,1) NE '=').NOTEQU                              00197103
&MACQSTR SETC  '&MACQSTR'(2,&MACPLEN)  STRIP EQUAL                      00197203
&MACPLEN SETA  K'&MACQSTR              UPDATE LENGTH                    00197303
.*  LOOK FOR LEADING QUOTE, C OR X - FAIL REST                          00197403
.NOTEQU  ANOP  ,                                                        00197503
&C       SETC  '&MACQSTR'(1,1)         ISOLATE FIRST BYTE               00197603
&MACPERR SETB  ('&C' NE '''' AND '&C' NE 'C' AND '&C' NE 'X')           00197703
         AIF   (&MACPERR).ERROR                                         00197803
.*  LOOK FOR LEADING QUOTE, C OR X - FAIL REST                          00197903
.STRING  AIF   ('&C' EQ '''').COUNT                                     00198003
&TYPE    SETC  '&C'                    REMEMBER THE TYPE                00198103
&MACQSTR SETC  '&MACQSTR'(2,K'&MACQSTR)  STRIP TYPE                     00198203
&MACPLEN SETA  K'&MACQSTR              UPDATE LENGTH                    00198303
&C       SETC  '&MACQSTR'(1,1)         ISOLATE FIRST BYTE               00198403
&MACPERR SETB  ('&C' NE '''' AND '&C' NE 'L')                           00198503
         AIF   (&MACPERR).ERROR                                         00198603
.*  LOOK FOR LEADING QUOTE OR L  (I.E., WE WANT 'text' OR CLn'text')    00198703
         AIF   ('&C' EQ '''').COUNT    COUNT LENGTH                     00198803
&L       SETA  0                       NO LENGTH YET                    00198903
.EXPLOOP ANOP  ,                                                        00199003
&MACQSTR SETC  '&MACQSTR'(2,K'&MACQSTR)  STRIP TYPE                     00199103
&MACPLEN SETA  K'&MACQSTR              UPDATE LENGTH                    00199203
&C       SETC  '&MACQSTR'(1,1)         ISOLATE FIRST BYTE               00199303
         AIF   ('&C' EQ '''').HAVEXPL  DONE WITH EXPLICIT LENGTH        00199403
         AIF   ('&C' LT '0' OR '&C' GT '9').ERROR                       00199503
&L       SETA  &L*10+&C                UPDATE LENGTH                    00199603
         AGO   .EXPLOOP                TRY ONE MORE                     00199703
.*   MACQSTR NOW HAS QUOTED STRING, AND L HAS THE LENGTH                00199803
.HAVEXPL ANOP  ,                                                        00199903
&MACPLEN SETA  &L                      RETURN THE LENGTH                00200003
&MACQSTR SETC  '='.'&TYPE'.'L'.'&L'.'&MACQSTR'                          00200103
         MEXIT ,                                                        00200203
.ERROR   ANOP  ,                                                        00200303
&MACPERR SETB  1                       RETURN AN ERROR                  00200403
         MEXIT ,                                                        00200503
.*   MACQSTR IS A QUOTED STRING WHOSE LENGTH WE NEED                    00200603
.*     NOTE THAT APOSTROPHES AND AMPERSANDS ARE DOUBLED (ELSE ERROR)    00200703
.COUNT   ANOP  ,                                                        00200803
&MACPLEN SETA  K'&MACQSTR              UPDATE LENGTH                    00200903
&I       SETA  1                       LOOP INDEX (2 TO K'-2)           00201003
&L       SETA  0                       SIGNIFICANT LENGTH               00201103
.CNTLOOP  ANOP ,                                                        00201203
&I       SETA  &I+1                                                     00201303
&C       SETC  '&MACQSTR'(&I,1)                                         00201403
         AIF   ('&C' NE '''' AND '&C' NE '&&').CNTONE                   00201503
         AIF   (&I GE &MACPLEN-1).ERROR                                 00201603
         AIF   ('&MACQSTR'(&I+1,1) NE '&C').ERROR    ERROR?             00201703
&I       SETA  &I+1          SKIP DOUBLED CARACTER                      00201803
.CNTONE  ANOP  ,                                                        00201903
&L       SETA  &L+1                                                     00202003
         AIF   (&I LE &MACPLEN-2).CNTLOOP                               00202103
&MACPLEN SETA  &L            SET STRIPPED LENGTH                        00202203
         AIF   ('&LEN' EQ '').DEFLN                             GP08090 00202303
&MACQSTR SETC  '='.'&TYPE'.'L('.'&LEN'.')'.'&MACQSTR'           GP08090 00202403
&MACPLEN SETA  &LEN                                             GP08090 00202503
         AGO   .MEND                                            GP08090 00202603
.DEFLN   ANOP  ,                                                GP08090 00202703
&MACQSTR SETC  '='.'&TYPE'.'&MACQSTR'                                   00202803
.MEND    MEND  ,                                                        00202903
./ ADD NAME=MAPCMPRT 0102-99113-06263-0022-00018-00017-00006-GERHARD 00 00203003
         MACRO ,                                                        00203103
&NM    MAPCMPRT &PFX=CPR,&DCB=0,&PRTMODE=0,&DEV=1                       00203203
.*  THIS MACRO MAPS THE COMMON PRINTER DEFINITION SHARED BY PGMTRACE,   00203303
.*    DEBTRACE, EXORCIST, AND ?                                         00203403
         LCLC  &P                                                       00203503
&P       SETC  '&PFX'                                                   00203603
&P.@UDCB DC    A(&DCB)       USER (OPEN) PRINT DCB                      00203703
&P.FGMOD DC    AL1(&PRTMODE)  USER OUTPUT DCB/PRT MODE                  00203803
&P.F@LCL EQU   0                 ADDR IS LOCAL DCB                      00203903
&P.F@WTO EQU   1                 ISSUE WTO (NARROW)                     00204003
&P.F@DCB EQU   2                 ADDR IS AN OPEN (PRINT) DCB            00204103
&P.F@EXT EQU   3                 ADDR IS FOR USER EXIT                  00204203
&P.FXPRT EQU   4                 ADDR IS FOR XPRINT                     00204303
&P.F@PRT EQU   5                 ADDR IS FOR @PRINTER                   00204403
&P.FGOPT DC    AL1(0)        ..RESERVED..                               00204503
&P.FGSPR DC    AL1(0)        ..RESERVED..                               00204603
&P.F@DEV DC    AL1(&DEV)     @PRINTER DEVICE SELECTION BITS             00204703
         MEND  ,                                                        00204803
./ ADD NAME=MASKEQU                                                     00204903
         MACRO ,                                                        00205003
         MASKEQU ,                                      ADDED ON 87223  00205103
.*  THE FUNCTION OF THIS SET OF EQUATES IS TO PROVIDE AN EASY           00205203
.*  WAY OF DEFINING CLM, ICM AND SIMILAR MASK BITS, WITHOUT             00205303
.*  HAVING TO USE B' ' FORMS OR INTEGERS.                               00205403
OOOO     EQU   0                                                        00205503
OOOI     EQU   1                                                        00205603
OOIO     EQU   2                                                        00205703
OOII     EQU   3                                                        00205803
OIOO     EQU   4                                                        00205903
OIOI     EQU   5                                                        00206003
OIIO     EQU   6                                                        00206103
OIII     EQU   7                                                        00206203
IOOO     EQU   8                                                        00206303
IOOI     EQU   9                                                        00206403
IOIO     EQU   10                                                       00206503
IOII     EQU   11                                                       00206603
IIOO     EQU   12                                                       00206703
IIOI     EQU   13                                                       00206803
IIIO     EQU   14                                                       00206903
IIII     EQU   15                                                       00207003
         MEND  ,                                                        00207103
./ ADD NAME=MVICC    0102-03105-06284-0153-00016-00014-00000-GERHARD 00 00207203
         MACRO ,                                                        00207303
&N       MVICC &CODE,&REAS,&RESULT=                    NEW 2003.091 GYP 00207403
         GBLC  &ZZCCNAM                                                 00207503
         LCLC  &L                                                       00207603
&L       SETC  'L'''                                                    00207703
         AIF   ('&RESULT' EQ '').NONEW                                  00207803
&ZZCCNAM SETC  '&RESULT'                                                00207903
.NONEW   AIF   ('&ZZCCNAM' NE '').NODEF                                 00208003
         MNOTE *,'MVICC: RESULT= NOT SPECIFIED - DEFAULTED TO RETCODE'  00208103
&ZZCCNAM SETC  'RETCODE'                                                00208203
.NODEF   ANOP  ,                                                        00208303
&N MACPARM &ZZCCNAM+&L&ZZCCNAM-1,&CODE,OP=MVI,OPR=STC,NULL=SKIP,       *00208403
               MODE=REV                                                 00208503
   MACPARM &ZZCCNAM+&L&ZZCCNAM+3,&REAS,OP=MVI,OPR=STC,NULL=SKIP,       *00208603
               MODE=REV                                                 00208703
         MEND  ,                                                        00208803
./ ADD NAME=OICC     0101-03105-06282-0923-00014-00014-00000-GERHARD 00 00208903
         MACRO ,                                                        00209003
&N       OICC  &CODE,&REAS,&RESULT=                    NEW 2003.091 GYP 00209103
         GBLC  &ZZCCNAM                                                 00209203
         LCLC  &L                                                       00209303
&L       SETC  'L'''                                                    00209403
         AIF   ('&RESULT' EQ '').NONEW                                  00209503
&ZZCCNAM SETC  '&RESULT'                                                00209603
.NONEW   AIF   ('&ZZCCNAM' NE '').NODEF                                 00209703
         MNOTE *,'OICC: RESULT= NOT SPECIFIED - DEFAULTED TO RETCODE'   00209803
&ZZCCNAM SETC  'RETCODE'                                                00209903
.NODEF   ANOP  ,                                                        00210003
&N MACPARM &ZZCCNAM+&L&ZZCCNAM-1,&CODE,OP=OI,OPR=STC,NULL=SKIP,MODE=REV 00210103
   MACPARM &ZZCCNAM+&L&ZZCCNAM+3,&REAS,OP=OI,OPR=STC,NULL=SKIP,MODE=REV 00210203
         MEND  ,                                                        00210303
./ ADD NAME=OPTIONGB                                                    00210403
         GBLA  &SYSPRM#            NUMBER OF SYSPARM TOKENS             00210503
         GBLA  &SVCJFCB            0 OR MODJFCB SVC NUMBER       82099  00210603
         GBLA  &SVC@SVC            0 OR @SERVICE ROUTINE SVC     83100  00210703
         GBLA  &SVCTMSX,&SVCTMSY   0 OR UCC-1 (TMS) SVC X/Y NMBR 83100  00210803
         GBLB  &BUGBEAR      (WAS &DEBUG - SAME AS HASP)         81331  00210903
         GBLB  &MVS                1 IF OS/VS2 MVS                      00211003
         GBLB  &MVSSP              1 IF OS/VS2 MVS/SP            82068  00211103
         GBLB  &MVSXA              1 IF OS/VS2 MVS/XA (SP2)      82068  00211203
         GBLB  &MVSESA             1 IF OS/VS2 MVS/ESA (SP3)     90217  00211303
         GBLB  &SVS                1 IF OS/VS2 SVS                      00211403
         GBLB  &VS1                1 IF OS/VS1                   82137  00211503
         GBLC  &CPU                360/370/470                          00211603
         GBLC  &JES2REL            JES2 LEVEL                    85076  00211703
         GBLC  &LOCAL              INSTALLATION ACRONYM/NAME            00211803
         GBLC  &MODEL              360/370/470                          00211903
         GBLC  &SPVEREL            MVS/SP VERSION/RELEASE/LEVEL  82091  00212003
         GBLC  &SYSPRMS(10)        SYSPARM TOKENS                       00212103
         GBLC  &PRTMAC             GEN OPTION FOR LOCAL  MAPS    81142  00212203
         GBLC  &PRTSOR             GEN OPTION FOR SOURCE         81142  00212303
         GBLC  &PRTSYS             GEN OPTION FOR SYSTEM MAPS    81142  00212403
         GBLC  &SYSTEM             MVT/SVS/MVS                          00212503
./ ADD NAME=PGMEXIT  0117-98365-09183-1946-00220-00119-00209-GERHARD 00 00212603
         MACRO ,                                                        00212703
&NM      PGMEXIT &DUMMY,&PFX=,&NEXT=,                                  *00212803
               &RC=,&RC0=,&RC1=,&COPYRET=,&RETADDR=(R14)         83087  00212903
         GBLB  &ZZSVBSM      SET BY SAVEM WHEN BSM IS USED ON ENTRY     00213003
         GBLB  &MVS,&MVSXA,&MVSESA,&OS390,&Z900,&BUGDBO         GP04234 00213103
         GBLC  &SAVTYPE,&SAVNAME                                GP04050 00213203
         GBLC  &MACPLAB                                         GP04051 00213303
.*--------------------------------------------------------------------* 00213403
.*                                                                    * 00213503
.*    PGMEXIT PROVIDES THE LOGICAL END OF A PROGRAM INITIATED WITH    * 00213603
.*    A PGMHEAD REQUEST. INFORMATION IS PASSED WITH GLOBALS, AND THE  * 00213703
.*    CODE DOES NOT SUPPORT INTERLEAVED PGMHEAD/PGMEXIT STATEMENTS.   * 00213803
.*                                                                    * 00213903
.*    THE PARAMETERS ARE:                                             * 00214003
.*                                                                    * 00214103
.*    RC=     NUMERIC VALUE (0-4095), REGISTER, OR RELOCATABLE WORD   * 00214203
.*    RC0=    NUMERIC VALUE (0-4095), REGISTER, OR RELOCATABLE WORD   * 00214303
.*    RC1=    NUMERIC VALUE (0-4095), REGISTER, OR RELOCATABLE WORD   * 00214403
.*                                                                    * 00214503
.*    RC LOADS REGISTER 15 (STANDARD RETURN CODE CONVENTION)          * 00214603
.*    RC0 AND RC1 LOAD REGISTERS 0 AND 1 AND ARE OPTIONAL             * 00214703
.*                                                                    * 00214803
.*    COPYRET=ADDRESS  OR COPYRET=(ADDRESS) LOAD R15 FROM STORAGE     * 00214903
.*    COPYRET=(ADDRESS,LENGTH)  LOAD R15,R0, ETC. DEPENDING ON LENGTH * 00215003
.*      ADDRESS IS A RELOCATABLE; LENGTH MUST BE AN ABSOLUTE TERM     * 00215103
.*                                                                    * 00215203
.*    ANY REGISTER NOT SPECIFIED IS RESTORED TO ITS VALUE ON ENTRY,   * 00215303
.*      UNLESS THE NOSAVE OPTION IS IN EFFECT                         * 00215403
.*                                                                    * 00215503
.*    R14 IS NOT SUPPORTED FOR A REGISTER OPERAND                     * 00215603
.*    R15, R0, AND R1 ARE ALLOWED PROVIDING THEY DO NOT CONFLICT      * 00215703
.*      E.G.  RC=(R15),RC1=(R1) IS VALID                              * 00215803
.*            RC=(R1),RC1=(R15) WILL FAIL                             * 00215903
.*                                                                    * 00216003
.*    RETADDR=R14 SPECIFIES THE REGISTER CONTAINING THE RETURN        * 00216103
.*      ADDRESS. IN BSM MODE, THIS MUST INCLUDE THE APPROPRIATE MODE  * 00216203
.*      SETTING BITS. OPERAND IS IGNORED IN BAKR/PR AND XCTL MODES.   * 00216303
.*                                                                    * 00216403
.*    PFX= SPECIFIES AN OVERRIDE TO THE SAVE AREA AND REGISTER NAME   * 00216503
.*      PREFIX. BY DEFAULT THE PFX FROM PGMHEAD IS USED.              * 00216603
.*                                                                    * 00216703
.*    NEXT= SPECIFIES THE NAME OF A MODULE TO XCTL TO, EITHER AS AN   * 00216803
.*      UNQUOTED NAME, OR AS QUOTED STRING, OR AS =CL8' ' LITERAL.    * 00216903
.*                                                                    * 00217003
.*                                                                    * 00217103
.*    THIS MACRO WAS SUGGESTED BY ENDM WRITTEN BY SHMUEL (SEYMOUR J.  * 00217203
.*    METZ, WHICH IS COPYRIGHT 1978 BY SHMUEL (SEYMOUR J.) METZ       * 00217303
.*                                                                      00217403
.*    THIS MACRO IS NOT TO BE DISTRIBUTED WITHOUT PERMISSION,           00217503
.*    AS DESCRIBED IN MEMBER $$RIGHTS.                                  00217603
.*                                                                      00217703
.*--------------------------------------------------------------------* 00217803
         LCLC  &SAVBASE      SAVE AREA START                            00217903
         LCLC  &OSVREG,&C    WORK REGISTER FOR HIGH LEVEL SAVE AREA     00218003
         LCLB  &NOSAVE,&OLDSAVE  PASSED BY PGMEXIT              GP04051 00218103
         LCLB  &OSVLOAD      FLAG THAT OSVREG LOADED AND SET            00218203
         LCLB  &LR15         FLAG THAT R15 HAS RC                       00218303
         LCLB  &LR0          FLAG THAT R15 HAS RC0                      00218403
         LCLB  &LR1          FLAG THAT R15 HAS RC0                      00218503
         LCLA  &I,&K                                                    00218603
&I       SETA  &SYSNDX                                                  00218703
&MACPLAB SETC  '&NM'                                            GP04051 00218803
         AIF   ('&PFX' EQ '').NOPFX                             GP04050 00218903
&SAVNAME SETC  '&PFX'                                                   00219003
.NOPFX   AIF   ('&SAVNAME' NE '').DFPFX                         GP04050 00219103
&SAVNAME SETC  'SAVE'                                           GP04050 00219203
.DFPFX   ANOP  ,                                                GP04050 00219303
&SAVBASE SETC '&SAVNAME'.'SPLN'                                         00219403
&NOSAVE  SETB  ('&SAVTYPE' EQ 'NO')                             GP04051 00219503
&OLDSAVE SETB  ('&SAVTYPE' EQ 'OLD')                            GP04051 00219603
.*--------------------------------------------------------------------* 00219703
.*   STEP 1:  EXCEPT FOR NOSAVE, SAVE ANY NON-NUMERIC RETURN CODES    * 00219803
.*       USE R14 AS A WORKING REGISTER                                * 00219903
.*       WITH NOSAVE, JUST LOAD THE NON-NUMERICS                      * 00220003
.*--------------------------------------------------------------------* 00220103
         AIF   (&NOSAVE).CPRCOM                                         00220203
         AIF   (NOT &OLDSAVE).CPROLD                                    00220303
&OSVREG  SETC  'R13'                                                    00220403
&OSVLOAD SETB  1                                                        00220503
         MACPARM R13,&SAVNAME.13-&SAVBASE.(R13),OP=L  LOAD OLD SV       00220603
         AGO   .CPRCOM                                                  00220703
.CPROLD  MACPARM R14,&SAVNAME.13-&SAVBASE.(R13),OP=L  LOAD WORK         00220803
&OSVREG  SETC  'R14'         WORK REGISTER                              00220903
&OSVLOAD SETB  1             WORK REGISTER LOADED                       00221003
.CPRCOM  AIF   (T'&COPYRET EQ 'O').DONCOPY                              00221103
         AIF   (N'&COPYRET LT 2).CPRONE                                 00221203
         AIF   (N'&COPYRET EQ 2).CPRTWO                                 00221303
.CPRBAD  MNOTE 4,' COPYRET PARAMETER INVALID; USE (ADDR-EXPR,LENGTH)'   00221403
         MEXIT ,                                                        00221503
.CPRTWO  AIF   (&NOSAVE).CPRTWON                                        00221603
&MACPLAB MVC   &SAVNAME.15-&SAVBASE.(&COPYRET(2),&OSVREG),&COPYRET(1)   00221703
&MACPLAB SETC  ''                                                       00221803
         AGO   .NOL15                                                   00221903
.CPRTWON MACPARM R15,&COPYRET(2)/4-2,&COPYRET(1),OP=LM                  00222003
         AGO   .NOL15                                                   00222103
.CPRONE  AIF   (&NOSAVE).CPRONEN                                        00222203
&MACPLAB MVC   &SAVNAME.15-&SAVBASE.(4,&OSVREG),&COPYRET(1)             00222303
&MACPLAB SETC  ''                                                       00222403
         AGO   .NOL15                                                   00222503
.CPRONEN MACPLAB R15,&COPYRET(1),OP=L,OPR=LR                            00222603
         AGO   .NOL15                                                   00222703
.*--------------------------------------------------------------------* 00222803
.*    NOTE THAT NUMERIC (T' = 'N') CODES ARE LOADED LATER ON          * 00222903
.*--------------------------------------------------------------------* 00223003
.DONCOPY AIF   (T'&RC EQ 'O').NOL15                                     00223103
         AIF   (T'&RC EQ 'N').NOL15                             GP04051 00223203
         AIF   (&NOSAVE).NSL15                                          00223303
         AIF   ('&RC'(1,1) EQ '(').STL15                                00223403
.NSL15   MACPARM R15,&RC,OP=L                                   GP04051 00223503
         AIF   (&NOSAVE).NOL15                                          00223603
         MACPARM R15,&SAVNAME.15-&SAVBASE.(,&OSVREG),OP=ST      GP04051 00223703
         AGO   .NOL15                                           GP04051 00223803
.STL15   MACPARM &RC(1),&SAVNAME.15-&SAVBASE.(,&OSVREG),OP=ST   GP04051 00223903
.NOL15   AIF   (T'&RC0 EQ 'O').NOL0                                     00224003
         AIF   (T'&RC0 EQ 'N').NOL0                             GP04051 00224103
         AIF   (&NOSAVE).NSL0                                           00224203
         AIF   ('&RC0'(1,1) EQ '(').STL0                                00224303
.NSL0    MACPARM R0,&RC0,OP=L                                   GP04051 00224403
         AIF   (&NOSAVE).NOL0                                           00224503
         MACPARM R0,&SAVNAME.0-&SAVBASE.(,&OSVREG),OP=ST        GP04051 00224603
         AGO   .NOL0                                            GP04051 00224703
.STL0    MACPARM &RC0(1),&SAVNAME.0-&SAVBASE.(,&OSVREG),OP=ST   GP04051 00224803
.NOL0    AIF   (T'&RC1 EQ 'O').DONLREG                                  00224903
         AIF   (T'&RC1 EQ 'N').DONLREG                          GP04051 00225003
         AIF   (&NOSAVE).NSL1                                           00225103
         AIF   ('&RC1'(1,1) EQ '(').STL1                                00225203
.NSL1    MACPARM R1,&RC1,OP=L                                   GP04051 00225303
         AIF   (&NOSAVE).DONLREG                                        00225403
         MACPARM R1,&SAVNAME.1-&SAVBASE.(,&OSVREG),OP=ST        GP04051 00225503
         AGO   .DONLREG                                         GP04051 00225603
.STL1    MACPARM &RC1(1),&SAVNAME.1-&SAVBASE.(,&OSVREG),OP=ST   GP04051 00225703
.*--------------------------------------------------------------------* 00225803
.*   STEP 2: REGAIN OLD SAVE AREA                                     * 00225903
.*--------------------------------------------------------------------* 00226003
.DONLREG AIF   (&NOSAVE).DONLSAV                                        00226103
         AIF   (&OLDSAVE).NSVSAVE                                       00226203
         MACPARM R1,(R13),OP=LR        SAVE STORAGE ADDRESS             00226303
.NSVSAVE AIF   (NOT &OSVLOAD).NSVLOAD  GET CALLER'S SAVE AREA           00226403
         AIF   ('&OSVREG' EQ 'R13').DONLSAV  HAVE IT ALREADY            00226503
         MACPARM R13,(&OSVREG),OP=LR   SKIP STORAGE IF HAVE             00226603
         AGO   .DONLSAV                                                 00226703
.NSVLOAD MACPARM R13,&SAVNAME.13-&SAVBASE.(R13),OP=L  OLD SAVE AREA     00226803
.*--------------------------------------------------------------------* 00226903
.*   STEP 3: FREE WORKING STORAGE                                     * 00227003
.*--------------------------------------------------------------------* 00227103
.DONLSAV AIF   (&NOSAVE OR &OLDSAVE).DONFREE                            00227203
         MACPARM R0,&SAVNAME.SPLN-&SAVBASE.(R1),OP=L                    00227303
         AIF   (&MVSESA).STOREL                                 GP04234 00227403
         ICM   R15,7,&SAVNAME.SPLN+1-&SAVBASE.(R1)  ANY LENGTH? GP04234 00227503
         BZ    ZZZ&I.L       SKIP IF ZERO LENGTH                GP04234 00227603
         FREEMAIN R,LV=(0),A=(1)  FREE STORAGE                  GP04234 00227703
ZZZ&I.L  DS    0H                                               GP06277 00227803
         AGO   .COMREL                                          GP04234 00227903
.STOREL  LR    R15,R0        COPY POSSIBLE SUBPOOL              GP04051 00228003
         SLL   R0,8                                                     00228103
         SRA   R0,8          REMOVE SUBPOOL                             00228203
         BZ    ZZZ&I.L       SKIP IF ZERO LENGTH                        00228303
         SRL   R15,24        RIGHT-JUSTIFY SUBPOOL                      00228403
.*       STORAGE RELEASE,ADDR=(1),LENGTH=(0),SP=(15)                    00228503
         STORAGE RELEASE,ADDR=(1),LENGTH=(0),SP=(15)                    00228603
.COMREL  ANOP  ,                                                GP04234 00228703
.*--------------------------------------------------------------------* 00228803
.*   STEP 4: LOAD NUMERIC RETURN CODES                                * 00228903
.*--------------------------------------------------------------------* 00229003
.DONFREE AIF   (T'&COPYRET NE 'O').LARDONE  COPYRET DONE ALREADY        00229103
         AIF   (T'&RC NE 'N').NOR15                                     00229203
&LR15    SETB  1             SHOW REGISTER LOADED                       00229303
         MACPARM R15,&RC,OP=LA                                  GP04051 00229403
.NOR15   AIF   (T'&RC0 NE 'N').NOR0                                     00229503
         MACPARM R0,&RC0,OP=LA                                          00229603
&LR0     SETB  1             SHOW REGISTER LOADED                       00229703
.NOR0    AIF   (T'&RC1 NE 'N').LARDONE                                  00229803
         MACPARM R1,&RC1,OP=LA                                          00229903
&LR1     SETB  1             SHOW REGISTER LOADED                       00230003
.*--------------------------------------------------------------------* 00230103
.*   STEP 5: RESTORE NON-RC REGISTERS AS NEEDED                       * 00230203
.*--------------------------------------------------------------------* 00230303
.LARDONE AIF   (&NOSAVE).LNRDONE                                        00230403
&OSVREG  SETC  'R14'                                                    00230503
         AIF   (NOT &LR15 AND NOT &LR0 AND NOT &LR1).LNRALL             00230603
         AIF   (NOT &LR15).LNR2LM                                       00230703
         MACPARM R14,&SAVNAME.14-&SAVBASE.(R13),OP=L                    00230803
&OSVREG  SETC  'R2'                                                     00230903
         AIF   (&LR1 AND &LR0).LNRALL                                   00231003
&OSVREG  SETC  'R0'                                                     00231103
         AIF   (NOT &LR0 AND NOT &LR1).LNRALL                           00231203
&OSVREG  SETC  'R1'                                                     00231303
         AIF   (&LR0).LNRALL                                            00231403
&OSVREG  SETC  'R2'                                                     00231503
         MACPARM R0,&SAVNAME.0-&SAVBASE.(R13),OP=L                      00231603
         AGO   .LNRALL                                                  00231703
.LNR2LM  ANOP  ,                                                        00231803
&OSVREG  SETC  'R1'.'+&LR1*4'                                           00231903
         AIF   (NOT &LR0).LNRE0                                         00232003
         MACPARM R14,R15,&SAVNAME.14-&SAVBASE.(R13),OP=LM,MODE=THREE    00232103
         AGO   .LNRALL                                                  00232203
.LNRE0   MACPARM R14,R0,&SAVNAME.14-&SAVBASE.(R13),OP=LM,MODE=THREE     00232303
.LNRALL  ANOP  ,                                                        00232403
&K       SETA   K'&OSVREG                                               00232503
&C       SETC   '&OSVREG'(2,&K-1)                                       00232603
        MACPARM &OSVREG,R12,&SAVNAME.&C-&SAVBASE.(R13),OP=LM,MODE=THREE 00232703
.*--------------------------------------------------------------------* 00232803
.*   STEP 6: XCTL OR RETURN ACCORDING TO ENTRY LINKAGE                * 00232903
.*--------------------------------------------------------------------* 00233003
.LNRDONE AIF   (&NOSAVE).RETFOOT                                GP04051 00233103
         MVI   &SAVNAME.14-&SAVBASE.(R13),X'FF' FLAG AS LAST SAVE AREA  00233203
.RETFOOT AIF   (T'&NEXT NE 'O').GOXCTL                                  00233303
         AIF   (T'&RETADDR EQ 'O').BUGME                        GP09183 00233403
         AIF   (&ZZSVBSM).GOBSM                                         00233503
         MACPARM &RETADDR,OP=B,OPR=BR,MODE=ONE                  GP04051 00233603
         AGO   .BUGME                                           GP09183 00233703
.GOXCTL  ANOP  ,                                                GP04234 00233803
&MACPLAB LA    R15,ZZZ&SYSNDX.X                                         00233903
         XCTL  SF=(E,(15))                                      GP04050 00234003
ZZZ&SYSNDX.X XCTL EP=&NEXT,SF=L                                         00234103
         AGO   .BUGME                                           GP09183 00234203
.GOBSM   MACPARM 0,&RETADDR(1),OP=BSM,OPR=BSM                   GP04051 00234303
.BUGME   AIF   (NOT &BUGDBO).END                                GP09183 00234403
         DBO   MODE=C        EXPAND DEBUG SUPPORT               GP09183 00234503
.END     MEND  ,                                                        00234603
./ ADD NAME=PGMHEAD  8006-04312-10180-0312-00506-00487-00000-GERHARD 00 00234703
         MACRO ,                                                        00234803
&L       PGMHEAD &DUMMY,&PFX=SAVE,&END=,&ENDZERO=,&DSECT=,&PARM=R1,    *00234903
               &SAVE=*,                                          81208 *00235003
               &STARTOF=0,     LISTING OFFSET FOR SUBROUTINES   GP02257*00235103
               &BASE=R12,&BASED=*,&BREG=,                       GP02264*00235203
               &EID=SHORT,&ENTRY=,&ENTNO=,                             *00235303
               &RIGHT=,                                                *00235403
               &ID=*,&DATE=,&SP=0,&BNDRY=,&LOC=,                 82002 *00235503
               &CSECT=CSECT,&XOPT=BSM,&AM=31,&RM=24,&SETAM=     GP02285 00235603
.*                                                                      00235703
.*    REWRITTEN FROM MACRO SAVEM, WITH ESA AND OS/390 CHANGES   GP98365 00235803
.*             COPYRIGHT 1978 BY SHMUEL (SEYMOUR J.) METZ               00235903
.*                        ALL RIGHTS RESERVED.                          00236003
.*             NEW CODE COPYRIGHT 1998 GERHARD POSTPISCHIL      GP98365 00236103
.*                                                              GP98365 00236203
.*             THIS MACRO IS NOT TO BE DISTRIBUTED WITHOUT PERMISSION,  00236303
.*             AS DESCRIBED IN MEMBER $$RIGHTS.                         00236403
.*                                                                      00236503
.*       FOR SIMPLE ENTRIES, PARM=R1 NOW RELOADS R0 AND R1       87223  00236603
.*       BASE=(B1,B2,B3,B4) SUPPORT ADDED (EASIER TO USE?)       87223  00236703
.*                                                               87223  00236803
         COPY  OPTIONGB                                                 00236903
         GBLB  &DROP@1                                           81163  00237003
         GBLB  &MAPONCE                                                 00237103
         GBLB  &AMSET,&RMSET                                    GP98365 00237203
         GBLB  &SAV@REG                                                 00237303
         GBLB  &SAV@DYN(10)                                             00237403
         GBLC  &SAV@NAM(10)                                             00237503
         GBLC  &SAVTYPE,&SAVNAME                                GP04050 00237603
         GBLC  &MACPLAB                                          81154  00237703
         GBLC  &SYSSPLV      VERSION OF SP (1, 2...)             93174  00237803
         GBLC  &SAVZPRM      PGMHEAD SAVED PARM                 GP09247 00237903
         LCLA  &I,&J,&K,&N,&NUMENT                              GP03245 00238003
         LCLB  &BWOPT,&Y           BIGWORK  OPTION                      00238103
         LCLB  &HWOPT              HUGEWORK OPTION ( > 32767 )   84307  00238203
         LCLB  &CME                BASED/ENTRY PRESENT           81163  00238303
         LCLB  &CPYREGS            CPYREGS  OPTION                      00238403
         LCLB  &DSOPT              NODSECT  OPTION                      00238503
         LCLB  &EQUOPT             NOEQU    OPTION                      00238603
         LCLB  &NOENTRY            NOENTRY  OPTION                      00238703
         LCLB  &NOREG              NOREG    OPTION - SKIP YREGS GP04115 00238803
         LCLB  &NOSAVE             NOSAVE   OPTION                      00238903
         LCLB  &NWOPT              NOWORK   OPTION                      00239003
         LCLB  &OLDSAVE            OLDSAVE  OPTION                      00239103
         LCLB  &BZOPT              ZERO > 256                           00239203
         LCLB  &NOT1ST             NOT FIRST USE OF &PFX                00239303
         LCLB  &ZERO               ZERO     OPTION                      00239403
         LCLB  &ZERO8              ZERO     <= 256               81202  00239503
         LCLB  &ZERO12             ZERO     >  256               81202  00239603
         LCLB  &ZERO15             ZERO     > 4095               81208  00239703
         LCLB  &ZERO31             ZERO     > 32767              84307  00239803
         LCLC  &B@                 GENERATED LABEL FOR B TARGET         00239903
         LCLC  &CMB                COMMON BASE NAME              81163  00240003
         LCLC  &CMU                BASE REG. STRING FOR USING    81163  00240103
         LCLC  &DSVAR              NON-RENT SAVE AREA NAME      GP03033 00240203
         LCLC  &DSCTG              DSECT NAME                    81208  00240303
         LCLC  &ENDG               END LABEL FOR GETMAINED AREA  81208  00240403
         LCLC  &ENDZ               END OF ZEROED AREA            81208  00240503
         LCLC  &LAB                LABEL FOR ENTRY POINT                00240603
         LCLC  &LQ                 L'                                   00240703
         LCLC  &N@                 GENERATED NAME FOR DC                00240803
         LCLC  &SECT               CSECT NAME                           00240903
         LCLC  &SP@                SUBPOOL FOR GETMAIN                  00241003
         LCLC  &NAME               NAME FOR CONSTRUCTED ID              00241103
         LCLC  &SV                 SAVE AREA PREFIX              81208  00241203
         LCLC  &BEGZ         WHERE TO START ZEROING (FWD DEFAULT)       00241303
         LCLC  &PARMEXP                                                 00241403
         LCLC  &PARMREG                                                 00241503
         LCLC  &CASE         FIRST/ONLY BASE REG                GP02264 00241603
         LCLC  &C9           TEMP                               GP03245 00241703
         LCLC  &YOPT         COPY OF XOPT OR 'STM'              GP04234 00241803
       SYSPARM ,                   SET GLOBALS                          00241903
&SECT    SETC  '&SYSECT'                                                00242003
&SV      SETC  '&PFX'                                                   00242103
&SAVNAME SETC  '&PFX'                                           GP04050 00242203
&DSCTG   SETC  '&PFX'                                                   00242303
&ENDG    SETC  '&PFX'.'END'                                     GP98365 00242403
&BEGZ    SETC  '&PFX'.'FWD'        ZERO BEGINNING AT FWD LINK    94272  00242503
&SAVTYPE SETC  'DYN'         (AS OPPOSED TO NO OR OLD)          GP04050 00242603
.*                                                                      00242703
&YOPT    SETC  '&XOPT'                                          GP04234 00242803
         AIF   ('&YOPT' EQ 'BSM' AND &MVSESA).ASMVS             GP04234 00242903
&YOPT    SETC  'STM'                                            GP04234 00243003
.*                                                                      00243103
.ASMVS   AIF   ('&DSECT' EQ '').DSGOK                                   00243203
&DSCTG   SETC  '&DSECT'                                                 00243303
.DSGOK   AIF   ('&END' EQ '').ENDGOK                                    00243403
&ENDG    SETC  '&END'                                                   00243503
.ENDGOK  ANOP  ,                                                        00243603
&DSVAR   SETC  '&DSCTG'      DEFAULT START OF SAVE AREA         GP03033 00243703
         AIF   ('&SAVE' EQ '*').ENDGSV                          GP03033 00243803
&DSVAR   SETC  '&SAVE'       FOR LENGTH DEFINITION              GP03033 00243903
.ENDGSV  ANOP  ,                                                GP03033 00244003
.*                                                                      00244103
&N       SETA  1                                                        00244203
.NXTSLOT ANOP  ,                                                        00244303
&NOT1ST  SETB  (&NOT1ST OR ('&PFX' EQ '&SAV@NAM(&N)'))                  00244403
         AIF   (&NOT1ST).FND1ST                                         00244503
         AIF   ('&SAV@NAM(&N)' EQ '').FNDSLOT                           00244603
&N       SETA  &N+1                                                     00244703
         AIF   (&N LE 10).NXTSLOT                                       00244803
         MNOTE 12,'TOO MANY PGMHEAD DSECTS'                     GP98365 00244903
         MEXIT ,                                                        00245003
.FNDSLOT ANOP  ,                                                        00245103
&SAV@NAM(&N) SETC '&PFX'                                                00245203
.*                                                                      00245303
.FND1ST  AIF   ('&SYSECT' EQ '' AND T'&L EQ 'O').NOL                    00245403
&I       SETA  1                                                        00245503
         AIF   (N'&SYSLIST EQ 0).ENDOPT                                 00245603
.*                                                                      00245703
.LOOP    AIF   ('&SYSLIST(&I)' EQ 'CPYREGS').CPYREGS                    00245803
         AIF   ('&SYSLIST(&I)' EQ 'COPYREGS').CPYREGS            81154  00245903
         AIF   ('&SYSLIST(&I)' EQ 'COPYREGISTERS').CPYREGS              00246003
         AIF   ('&SYSLIST(&I)' EQ 'NODSECT').NODSECT                    00246103
         AIF   ('&SYSLIST(&I)' EQ 'NOEQU').NOEQU                        00246203
         AIF   ('&SYSLIST(&I)' EQ 'NOREG').NOREGS               GP04115 00246303
         AIF   ('&SYSLIST(&I)' EQ 'NOREGS').NOREGS              GP04115 00246403
         AIF   ('&SYSLIST(&I)' EQ 'BIGWORK').BIGWORK                    00246503
         AIF   ('&SYSLIST(&I)' EQ 'HUGEWORK').HUGWORK            84307  00246603
         AIF   ('&SYSLIST(&I)' EQ 'BIGZERO').BIGZERO                    00246703
         AIF   ('&SYSLIST(&I)' EQ 'HUGEZERO').ZERO31             84307  00246803
         AIF   ('&SYSLIST(&I)' EQ 'NOWORK').NOWORK                      00246903
         AIF   ('&SYSLIST(&I)' EQ 'NOSAVE').NOSAVE1                     00247003
         AIF   ('&SYSLIST(&I)' EQ 'OLDSAVE').OLDSAVE                    00247103
         AIF   ('&SYSLIST(&I)' EQ 'NOENTRY').NOENTRY                    00247203
         AIF   ('&SYSLIST(&I)' EQ 'ZERO').ZERO                          00247303
         AIF   ('&SYSLIST(&I)' EQ 'ZERO8').ZERO8                 81208  00247403
         AIF   ('&SYSLIST(&I)' EQ 'ZERO12').ZERO12               81208  00247503
         AIF   ('&SYSLIST(&I)' EQ 'ZERO15').ZERO15               81208  00247603
         AIF   ('&SYSLIST(&I)' EQ 'ZERO31').ZERO31               84307  00247703
         AIF   ('&SYSLIST(&I)' EQ '').NXTOPT                            00247803
         MNOTE 4,'''&SYSLIST(&I)'' IS AN INVALID OPTION - IGNORED'      00247903
         AGO   .NXTOPT                                                  00248003
.*                                                                      00248103
.NOL     MNOTE 12,'LABEL REQUIRED IF NO CSECT'                          00248203
         MEXIT ,                                                        00248303
.*                                                                      00248403
.CPYREGS ANOP  ,                                                        00248503
&CPYREGS SETB  1                                                        00248603
         AGO   .NXTOPT                                                  00248703
.NODSECT ANOP  ,                                                        00248803
&DSOPT   SETB  1                                                        00248903
         AGO   .NXTOPT                                                  00249003
.NOEQU   ANOP  ,                                                        00249103
&EQUOPT  SETB  1                                                        00249203
         AGO   .NXTOPT                                                  00249303
.NOREGS  ANOP  ,                                                GP04115 00249403
&NOREG   SETB  1                                                GP04115 00249503
         AGO   .NXTOPT                                          GP04115 00249603
.HUGWORK ANOP  ,                                                 84307  00249703
&HWOPT   SETB  1             SET FOR HUGE WORK AREA              84307  00249803
.BIGWORK ANOP  ,                                                        00249903
&BWOPT   SETB  1                                                        00250003
&BZOPT   SETB  1                                                        00250103
         AGO   .NXTOPT                                                  00250203
.NOWORK  ANOP  ,                                                        00250303
&NWOPT   SETB  1                                                        00250403
         AGO   .NXTOPT                                                  00250503
.NOSAVE1 ANOP  ,                                                        00250603
&NOSAVE  SETB  1                                                        00250703
&SAVTYPE SETC  'NO'                                             GP04050 00250803
         AGO   .NXTOPT                                                  00250903
.OLDSAVE ANOP  ,                                                        00251003
&OLDSAVE SETB  1                                                        00251103
&SAVTYPE SETC  'OLD'                                            GP04050 00251203
         AGO   .NXTOPT                                                  00251303
.NOENTRY ANOP  ,                                                        00251403
&NOENTRY SETB  1                                                        00251503
         AGO   .NXTOPT                                                  00251603
.*                                                               81208  00251703
.ZERO8   ANOP  ,                                                 81208  00251803
&ZERO8   SETB  1                                                 81208  00251903
         AGO   .ZERO                                             81208  00252003
.ZERO12  ANOP  ,                                                 81208  00252103
&ZERO12  SETB  1                                                 81218  00252203
         AGO   .ZERO                                             81208  00252303
.ZERO31  ANOP  ,                                                 84307  00252403
&HWOPT   SETB  1             HUGE WORK AREA > 32767              84307  00252503
&ZERO31  SETB  1                                                 84307  00252603
.ZERO15  ANOP  ,                                                 81208  00252703
&ZERO15  SETB  1                                                 81208  00252803
&BWOPT   SETB  1                                                 81218  00252903
         AGO   .ZERO                                             81208  00253003
.BIGZERO ANOP  ,                                                        00253103
&BZOPT   SETB  1                                                 81202  00253203
.ZERO    ANOP  ,                                                        00253303
&ZERO    SETB  1                                                        00253403
.NXTOPT  ANOP  ,                                                        00253503
&I       SETA  &I+1                                                     00253603
         AIF   (&I LE N'&SYSLIST).LOOP                                  00253703
.ENDOPT  AIF   (&ZERO8 OR &ZERO12 OR &ZERO15).GOTZERO            81208  00253803
&ZERO31  SETB  (&ZERO AND &HWOPT)                                84307  00253903
&ZERO15  SETB  (&ZERO AND &BWOPT)                                81208  00254003
&ZERO12  SETB  (&BZOPT AND NOT &ZERO15)                          81208  00254103
&ZERO8   SETB  (&ZERO AND NOT &ZERO12 AND NOT &ZERO15)           81202  00254203
.GOTZERO AIF   (T'&L EQ 'O').CSECTOK                             81208  00254303
         AIF   ('&SYSECT' EQ '' OR '&L' EQ '&SYSECT').LABOK      81202  00254403
&LAB     SETC  '&L'                                                     00254503
         AIF   (&NOENTRY).LABOK                                         00254603
         SPACE 1                                                GP04050 00254703
         ENTRY &L                                                       00254803
.LABOK   AIF   ('&SYSECT' NE '').CSECTOK                                00254903
         AIF   ('&CSECT' EQ 'CSECT').CSECTDO                    GP98322 00255003
         AIF   ('&CSECT' EQ 'RSECT').RSECTDO                    GP98322 00255103
         AIF   ('&CSECT' EQ 'START').STARTDO                    GP98322 00255203
         MNOTE 4,'INVALID CSECT OPERAND; USE CSECT OR RSECT'    GP98322 00255303
         AGO   .CSECTDO      TRY TO CONTINUE?                   GP98322 00255403
.*                                                              GP98322 00255503
.STARTDO ANOP  ,                                                GP98322 00255603
&L       START &STARTOF                                         GP04234 00255703
         AGO   .CSECTCM                                         GP98322 00255803
.*                                                              GP98322 00255903
.RSECTDO AIF   (NOT &MVSESA).CSECTDO                            GP04234 00256003
&L       RSECT ,                                                GP98322 00256103
         AGO   .CSECTCM                                         GP98322 00256203
.*                                                              GP98322 00256303
.CSECTDO ANOP  ,                                                GP98322 00256403
&L       CSECT ,                                                        00256503
.CSECTCM ANOP  ,                                                        00256603
&SECT    SETC  '&L'                                                     00256703
         AIF   ('&AM' EQ '' OR &AMSET).NOAM                     GP98365 00256803
         AIF   (NOT &MVSESA).NOAM                               GP04234 00256903
&L       AMODE &AM                                                      00257003
&AMSET   SETB  1                                                GP98365 00257103
.NOAM    AIF   ('&RM' EQ '' OR &RMSET).CSECTOK                  GP98365 00257203
         AIF   (NOT &MVSESA).CSECTOK                            GP04234 00257303
         AIF   ('&RM' EQ '24' OR '&RM' EQ 'ANY').SETRM          GP99120 00257403
         AIF   ('&RM' EQ '31').ANYRM                            GP99120 00257503
         MNOTE 4,'INVALID RM=&RM '                              GP99120 00257603
.ANYRM   ANOP  ,                                                GP99120 00257703
&L       RMODE ANY                                              GP99120 00257803
         AGO   .FLGRM                                           GP99120 00257903
.SETRM   ANOP  ,                                                GP99120 00258003
&L       RMODE &RM                                              GP98365 00258103
.FLGRM   ANOP  ,                                                GP99120 00258203
&RMSET   SETB  1                                                GP98365 00258303
.CSECTOK AIF   (T'&BASED EQ 'O' AND T'&ENTRY EQ 'O').NOCLAB      81163  00258403
         AIF   (T'&ENTRY NE 'O').DOCLAB                          81163  00258503
         AIF   ('&BASED' EQ '*').NOCLAB                          81163  00258603
.DOCLAB  ANOP  ,                                                 81163  00258703
&CME     SETB  1             SET SPECIAL BASE PROCESSING         81163  00258803
.NOCLAB  AIF   ('&LAB' NE '').OKCLAB                             81163  00258903
&LAB     SETC  'A@&SYSNDX'                                       81163  00259003
.OKCLAB  ANOP  ,                                                 81163  00259103
&CMB     SETC  '&LAB'        DEFAULT BASED VALUE                 81163  00259203
         AIF   (T'&BASED EQ 'O').CLABOK                          81163  00259303
         AIF   ('&BASED' EQ '*').CLABOK                          81163  00259403
         AIF   ('&BASED' NE '*SYSECT').CLABSET                   81163  00259503
         AIF   ('&SYSECT' EQ '').CLABOK   BOO                    81163  00259603
&CMB     SETC  '&SYSECT'                                         81163  00259703
         AGO   .CLABOK                                           81163  00259803
.CLABSET ANOP  ,                                                 81163  00259903
&CMB     SETC  '&BASED'                                          81163  00260003
.CLABOK  ANOP  ,                                                 81163  00260103
&CMU     SETC  'R15'         DEFAULT BASE REGISTER ON USING      81163  00260203
&SAV@DYN(&N) SETB (NOT &NOSAVE)                                         00260303
&PARMEXP SETC  '(R1)'                                                   00260403
&PARMREG SETC  '1'                                                      00260503
&SAVZPRM SETC  ''            PASS TO PARMLOAD MACRO             GP10180 00260603
         AIF   ('&PARM' EQ '').PARM1                                    00260703
&SAVZPRM SETC  '&PARM(1)'    PASS TO PARMLOAD MACRO             GP10180 00260803
         AIF   (N'&PARM LT 2).PARM1                                     00260903
&PARMEXP SETC  '&PARM(2)'                                               00261003
         AIF   (NOT &OLDSAVE).PARM1                                     00261103
&PARMREG SETC  '&PARM(2)'                                               00261203
         AIF   ('&PARMEXP'(1,1) EQ '(').STRIP                           00261303
         MNOTE 8,'PARM=&PARM INVALID'                                   00261403
         MNOTE 8,'PARM=(&PARM(1),(&PARM(2)) ASSUMED'                    00261503
         AGO   .STRIPT                                                  00261603
.STRIP   AIF   ('&PARMEXP'(K'&PARMEXP,1) EQ ')').STRIP1                 00261703
         MNOTE 12,'PARM=&PARM INVALID'                                  00261803
         MEXIT ,                                                        00261903
.STRIP1  ANOP  ,                                                        00262003
&PARMREG SETC  '&PARMREG'(2,K'&PARMREG-2)                               00262103
.STRIPT  AIF   ('&PARMREG'(1,1) GE '0').PARM1                           00262203
&PARMREG SETC  '&PARMREG'(2,K'&PARMREG-1)                               00262303
.PARM1   AIF   (NOT &DROP@1).NODROP                              81163  00262403
         DROP  ,                                                 81163  00262503
.NODROP  ANOP  ,                                                 81163  00262603
&DROP@1  SETB  1                                                 81163  00262703
         AIF   (T'&BASE NE 'O').NEWBASE                         GP02264 00262803
.NOBASE  AIF   (T'&ENTRY NE 'O').NOBASEU                         81163  00262903
         DS    0H                                                       00263003
         USING *,R15                                             81163  00263103
.NOBASEU ANOP  ,                                                 81163  00263203
&CMU     SETC  'R15'                                             81163  00263303
&CASE    SETC  'R15'                                             81163  00263403
         AGO   .BASED                                                   00263503
.NEWBASE ANOP  ,                                                GP02264 00263603
&K       SETA  N'&BASE      MAX NUMBER OF BASES SPECIFIED       GP02264 00263703
&CMU     SETC  '&BASE(1)'    SET THE FIRST ONE                  GP02264 00263803
&CASE    SETC  '&BASE(1)'    SET THE FIRST ONE                  GP02264 00263903
.BASED   AIF   (&NOSAVE).BASED2                                 GP04050 00264003
         USING &DSCTG,R13                                               00264103
.BASED2  AIF   ('&ID' EQ 'NO').NAMEOK                           GP05017 00264203
&B@      SETC  'B@&SYSNDX'                                              00264303
&N@      SETC  'N@&SYSNDX'                                              00264403
&LQ      SETC  'L'''                                                    00264503
&LAB     B     &B@-*(,R15)                                              00264603
&LAB     SETC  ''                                               GP05017 00264703
         DC    AL1(&LQ&N@)                                              00264803
         AIF   ('&ID' NE '*').USEID                                     00264903
&NAME    SETC  '&L'                                                     00265003
         AIF   (T'&L NE 'O').USENAME                                    00265103
&NAME    SETC  '&SYSECT'                                                00265203
.USENAME AIF   ('&RIGHT' EQ '').NORIGHT                                 00265303
&NAME    SETC  '&NAME'.' '.'COPYRIGHT '.'&RIGHT'                        00265403
.NORIGHT AIF   ('&DATE' EQ 'NO').NODATE                                 00265503
&N@      DC    C'&NAME - &SYSDATE - &SYSTIME'                           00265603
         AGO   .NAMEOK                                                  00265703
.NODATE  ANOP  ,                                                        00265803
&N@      DC    C'&NAME'                                                 00265903
         AGO   .NAMEOK                                                  00266003
.USEID   ANOP  ,                                                        00266103
         AIF   ('&ID'(1,1) NE '''').USEIDQ                              00266203
&N@      DC    C&ID                                                     00266303
         AGO   .NAMEOK                                                  00266403
.USEIDQ  ANOP  ,                                                        00266503
&N@      DC    C'&ID'                                                   00266603
.NAMEOK  ANOP  ,                                                GP05017 00266703
&LAB     MACPARM MODE=LBL                                       GP05017 00266803
         AIF   (T'&ENTRY EQ 'O').NOENTR                          81163  00266903
&I       SETA  0                                                 81163  00267003
&J       SETA  N'&ENTRY                                          81163  00267103
&N@      SETC  ''            SHORT ID                            81163  00267203
         AIF   ('&EID' EQ 'SHORT').ENTRSH                        81163  00267303
&N@      SETC  ' - '.'&SYSDATE'.' - '.'&SYSTIME'                 81163  00267403
.ENTRSH  AIF   (&I GE &J).ENTRDN                                 81163  00267503
&I       SETA  &I+1                                              81163  00267603
&C9      SETC  '&ENTRY(&I)'                                      81163  00267703
         AIF   (&NOENTRY).ENTRNN                                 81347  00267803
         SPACE 1                                                GP04051 00267903
         ENTRY &C9                                               81163  00268003
.ENTRNN  AIF   (T'&ENTNO EQ 'O').ENTRNM                          88255  00268103
&NUMENT  SETA  &NUMENT+1     INCREASE ENTRY NUMBER               88255  00268203
         DC    Y(&NUMENT)    MAKE ENTRY PREFIX                   88255  00268303
.ENTRNM  ANOP  ,                                                 88255  00268403
&C9      B     &B@-*(,R15)                                       81163  00268503
         AIF   ('&EID' EQ 'NONE').ENTRSH                        GP99055 00268603
&N       SETA  K'&N@+K'&C9                                       81163  00268703
&N       SETA  ((&N/2)*2)+1  MAKE ODD LENGTH FOR ALIGNMENT       81163  00268803
         DC    AL1(&N),CL(&N)'&ENTRY(&I)&N@'                     81163  00268903
         AGO   .ENTRSH                                           81163  00269003
.ENTRDN  AIF   (T'&ENTNO EQ 'O').ENTRDM                          88255  00269103
         DC    Y(0)          SET ENTRY PREFIX =0 (MAIN)          88255  00269203
.ENTRDM  AIF   (&NOSAVE).ELDSVAM                                GP04050 00269303
.*WHY?   USING &C9,R15                                          GP99158 00269403
&B@      SAVEX R14,R12,&SV.14,TYPE=&YOPT,SETAM=&SETAM           GP99018 00269503
&B@      SETC  ''                                                81163  00269603
.*WHY?   DROP  R15                                              GP99158 00269703
         AGO   .ELDSV                                           GP04050 00269803
.ELDSVAM AIF   ('&SETAM' EQ '').ELDSV                           GP04050 00269903
         AIF   (NOT &MVSESA).ELDSV                              GP04234 00270003
         BASR  &CASE,0                                          GP04050 00270103
         USING *,&CASE                                          GP04050 00270203
         AM&SETAM WORK=&CASE                                    GP04050 00270303
         DROP  &CASE                                            GP04050 00270403
.ELDSV   AIF   ('&CASE' EQ '').ELDSVLR                           87223  00270503
         AIF   ('&BASED' NE '*').ELDSVLR                         81263  00270603
&B@      BASR  &CASE,0                                           93006  00270703
         LA    R15,*-&CMB                                        81263  00270803
         SLR   &CASE,R15                                         87223  00270903
         AGO   .COMBAS2                                          81263  00271003
.ELDSVLR ANOP  ,                                                 81263  00271103
&B@      BASR  R15,0                                             93006  00271203
         USING *,R15                                             81163  00271303
         AIF   ('&CASE' EQ '').NOBASE2                           87223  00271403
         L     &CASE,=A(&CMB)                                    87223  00271503
         DROP  R15                                               81163  00271603
         AGO   .COMBAS2                                          81163  00271703
.NOENTR  AIF   (&NOSAVE).OLDSVAM                                GP04050 00271803
.*WHY    USING &LAB,R15                                         GP99158 00271903
&B@      SAVEX R14,R12,&SV.14,TYPE=&YOPT,SETAM=&SETAM           GP98322 00272003
&B@      SETC  ''                                                       00272103
.*WHY    DROP  R15                                              GP99158 00272203
         AGO   .OLDSV                                           GP04050 00272303
.OLDSVAM AIF   ('&SETAM' EQ '').OLDSV                           GP04050 00272403
         AIF   (NOT &MVSESA).OLDSV                              GP04234 00272503
         BASR  &CASE,0                                          GP04050 00272603
         USING *,&CASE                                          GP04050 00272703
         AM&SETAM WORK=&CASE                                    GP04050 00272803
         DROP  &CASE                                            GP04050 00272903
.OLDSV   AIF   ('&CASE' EQ '').NOBASE2                           87223  00273003
         AIF   ('&BREG' NE 'SET').BASREG                        GP05017 00273103
&B@      BASR  &CASE,0                                          GP05017 00273203
         LA    R15,*-&CMB                                       GP05017 00273303
         SLR   &CASE,R15                                        GP05017 00273403
         AGO   .COMBAS2                                         GP05017 00273503
.BASREG  AIF   (&CME).BASEL                                      81163  00273603
&B@      LA    &CASE,0(,R15)  REMOVE AM BIT                             00273703
         AGO   .COMBAS2                                          81163  00273803
.BASEL   USING &LAB,R15                                          81163  00273903
&B@      L     &CASE,=A(&CMB)                                    87223  00274003
         DROP  R15                                               81163  00274103
.COMBAS2 ANOP  ,                                                GP02264 00274203
&K       SETA  N'&BASE                                          GP02264 00274303
         AIF   (&K LT 2).NOBASE2                                GP02264 00274403
&I       SETA  1                                                GP02264 00274503
         LA    &BASE(&K),2048                                   GP02264 00274603
.NOBASLP AIF   (&I GE &K).NOBASE2                               GP02264 00274703
&I       SETA  &I+1                                             GP02264 00274803
         AIF   ('&BASE(&I)' EQ '').NOBASLP                      GP02264 00274903
         LA    &BASE(&I),2048(&BASE(&K),&BASE(&I-1))            GP02264 00275003
&CMU     SETC  '&CMU'.','.'&BASE(&I)'                           GP02264 00275103
         AGO   .NOBASLP                                         GP02264 00275203
.NOBASE2 AIF   ('&CASE' EQ '').NOUSEB                            87223  00275303
         USING &CMB,&CMU                                         81163  00275403
.NOUSEB  AIF   (NOT &OLDSAVE).NOLDSV                                    00275503
.*WHY?   L     R15,&SV.13                                       GP04050 00275603
.*WHY?   ST    &CASE,&SV.15-&DSCTG.(,R15)                        87223  00275703
         AIF   ('&PARM' EQ '').NOLDSV1                                  00275803
         AIF   ('&PARM' EQ 'R1' OR '&PARM' EQ '1').NOLDSV1      GP04052 00275903
         AIF   (&NOSAVE).NOLDSV1                                GP04052 00276003
         L     &PARM(1),&SV.&PARMREG-&DSCTG.(,R13)              GP04050 00276103
         AGO   .NOLDSV1                                                 00276203
.NOLDSV  AIF   (&NOSAVE).NOLDSV1                                        00276303
         AIF   ('&PARM' EQ '').NOPARM                                   00276403
         AIF   ('&PARM' EQ '1' OR '&PARM' EQ 'R1').NOPARM        87223  00276503
&MACPLAB SETC  ''                                                81154  00276603
         MACPARM &PARM(1),&PARMEXP                               81154  00276703
.*                                                                      00276803
.NOPARM  AIF   ('&SAVE' NE '*' AND NOT &ZERO).NOSTO NON-RENT/NO LENGTH  00276903
         AIF   (NOT &HWOPT).LYLEN                                84307  00277003
         L     R14,=A(&ENDG-&DSVAR)                             GP03033 00277103
         AGO   .NOLA                                             84307  00277203
.LYLEN   AIF   (NOT &BWOPT).LALEN                                84307  00277303
         LH    R14,=Y(&ENDG-&DSVAR)                             GP03033 00277403
         AGO   .NOLA                                                    00277503
.LALEN   LA    R14,&ENDG-&DSVAR                                 GP03033 00277603
.*  NOTE THAT R14-R1 ARE USED BY STORAGE                                00277703
.NOLA    ST    R14,&SV.FWD   TEMP: LEN INTO OLD SAVE AREA       GP02304 00277803
         AIF   ('&SAVE' NE '*').NOSTO                           GP03033 00277903
*        STORAGE OBTAIN,LENGTH=(R14),SP=&SP,BNDRY=&BNDRY,LOC=&LOC       00278003
         STORAGE OBTAIN,LENGTH=(R14),SP=&SP,BNDRY=&BNDRY,LOC=&LOC       00278103
         AGO   .GTSTO                                           GP03033 00278203
.NOSTO   LA    R14,&SAVE     LOAD NON-RENT SAVE AREA            GP03033 00278303
         AIF   (&ZERO).ZRSTO                                    GP03033 00278403
         XC    0(4*18,R14),0(R14)  PREVENT S978 IN EXIT         GP03033 00278503
         AGO   .SKPLEN                                          GP03033 00278603
.GTSTO   LR    R14,R1        SAVE OVER CLEAR                    GP02264 00278703
         AIF   (NOT &ZERO).UNCLEAN                              GP02264 00278803
.ZRSTO   SR    R15,R15       ZERO SOURCE LENGTH AND INSERTION   GP02264 00278903
         LR    R0,R14        SET CLEAR ADDRESS                  GP02264 00279003
         L     R1,&SV.FWD    GET SAVED LENGTH                   GP02304 00279103
         MVCL  R0,R14        CLEAR GOTTEN STORAGE               GP02264 00279203
         AIF   ('&SAVE' NE '*').SKPLEN  PREVENT S978 IN EXIT    GP03033 00279303
.UNCLEAN MVC   &SV.SPLN-&DSCTG.(4,R14),&SV.FWD  SET LENGTH FOR PGMEXIT  00279403
.SKPLEN  AIF   ('&SP' EQ '0').NOGM                               82002  00279503
         MVI   &SV.SPLN-&DSCTG.(R14),&SP  AND SUBPOOL           GP02264 00279603
.NOGM    ST    R14,&SV.FWD   MAKE FOWARD SAVE AREA LINK         GP02264 00279703
         ST    R13,&SV.13-&DSCTG.(,R14) MAKE BACKWARD LINK      GP02264 00279803
         AIF   (NOT &CPYREGS).LR13                                      00279903
         MVC   &SV.14-&DSCTG.(&SV.12+4-&SV.14,R14),&SV.14        81151  00280003
.LR13    LR    R13,R14       ESTABLISH NEW SAVE AREA                    00280103
         AIF   (T'&ENTRY EQ 'O' OR T'&ENTNO EQ 'O').NOLDENT      88255  00280203
         L     R1,&SV.13     GET OLD SAVE AREA BACK              88255  00280303
         CLM   &CASE,7,&SV.15+1-&DSCTG.(R1)  MAIN ENTRY ?        88255  00280403
         BE    *+16          YES; DON'T MOVE                     88255  00280503
         L     R1,&SV.15-&DSCTG.(,R1) GET ENTRY ADDRESS BACK     88255  00280603
         BCTR  R1,0          SPACE TO ENTRY COUNTER              88255  00280703
         MVC   &ENTNO+L'&ENTNO-1(1),0(R1) COPY COUNT             88255  00280803
.NOLDSV1 ANOP  ,       TRY IT HERE                              GP03033 00280903
.NOLDENT AIF   ('&PARM' NE '1' AND '&PARM' NE 'R1').NOPARM1      87223  00281003
         AIF   (&NOSAVE OR &OLDSAVE).NOPARM1                    GP04052 00281103
         L     R1,&SV.13     OLD SAVE AREA                       87223  00281203
         LM    R0,R1,&SV.0-&DSCTG.(R1)  RESTORE ENTRY REGISTERS  94272  00281303
.NOPARM1 AIF   (&NOT1ST).END                                            00281403
         AIF   (&DSOPT OR &NOSAVE).DSOPT                        GP04050 00281503
&DSCTG   DSECT ,                                                GP04051 00281603
&SV.SPLN DS    F                                                        00281703
&SV.13   DS    F                                                        00281803
&SV.FWD  DS    A                                                        00281903
&SV.14   DS    A                                                        00282003
&SV.15   DS    A                                                        00282103
&SV.0    DS    A                                                        00282203
&SV.1    DS    A                                                        00282303
&SV.2    DS    A                                                        00282403
&SV.3    DS    A                                                        00282503
&SV.4    DS    A                                                        00282603
&SV.5    DS    A                                                        00282703
&SV.6    DS    A                                                        00282803
&SV.7    DS    A                                                        00282903
&SV.8    DS    A                                                        00283003
&SV.9    DS    A                                                        00283103
&SV.10   DS    A                                                        00283203
&SV.11   DS    A                                                        00283303
&SV.12   DS    A                                                        00283403
&SV.FWK  EQU   *                                                 94272  00283503
         AIF   (NOT &NWOPT).NOEND                                       00283603
&ENDG    EQU   *                                                        00283703
.NOEND   AIF   ('&CSECT' NE 'RSECT').NOENDC                             00283803
&SECT    RSECT ,                                                        00283903
         AGO   .DSOPT                                                   00284003
.NOENDC  ANOP  ,                                                        00284103
&SECT    CSECT ,                                                        00284203
.DSOPT   AIF   (&EQUOPT OR &MAPONCE OR &SAV@REG).END                    00284303
&MAPONCE SETB  1                                                        00284403
&SAV@REG SETB  1                                                        00284503
         AIF   (&NOREG).SKPYREG  AVOID JES2 MAPPING CONFLICT    GP04115 00284603
         YREGS ,                                                        00284703
.SKPYREG MASKEQU ,                                               87223  00284803
.END     AIF   ('&SAVE' EQ '*' OR &NOSAVE).MEND                 GP04051 00284903
&SAVTYPE SETC  'OLD'         PREVENT STORAGE RELEASE IN PGMEXIT GP04051 00285003
 MNOTE *,'SAVE IS &SAVE'                                                00285103
 MNOTE *,'SAVTYPE IS &SAVTYPE'                                          00285203
.MEND    MEND  ,                                                GP04051 00285303
./ ADD NAME=PRTCOM   0111-03025-05192-0057-00185-00161-00042-GERHARD 00 00285403
         MACRO                                                          00285503
&NM      PRTCOM &OM,&B0=,&B1=0,&DEV=0,&FUN=,&A0=,&A1=,&OPT=,&A80=OFF    00285603
.*--------------------------------------------------------------------* 00285703
.*                                                                    * 00285803
.*   PRTCOM PROVIDES THE INTERFACE TO THE @PRINTER SERVICE ROUTINE    * 00285903
.*   ARGUMENTS ARE PASSED IN R0, R1, AND ACR0 (PRTF,PRTS)             * 00286003
.*                                                                    * 00286103
.*   IN AM24, THE REGISTER USE IS:                                    * 00286203
.*                                                                    * 00286303
.*   R0:0           R0:1           R0:2           R0:3                * 00286403
.*                                                                    * 00286503
.*   OPT FLAGS      TITLE/FOOTER#  DEVICE MASK    PRT FUNCTION REQ.   * 00286603
.*   (EITHER B0 OR OPT=)                                              * 00286703
.*                                                                    * 00286803
.*   R1:0           R1:1           R1:2           R1:3                * 00286903
.*                                                                    * 00287003
.*   LENGTH/ENDCH   ADDRESS-OF-LIST-OR-PRINT-DATA-ETC.                * 00287103
.*                                                                    * 00287203
.*--------------------------------------------------------------------* 00287303
.*                                                                    * 00287403
.*   IN AM31, THE REGISTER USE IS:                                    * 00287503
.*                                                                    * 00287603
.*   R0:0           R0:1           R0:2           R0:3                * 00287703
.*                                                                    * 00287803
.*   FLAG           TITLE/FOOTER#  DEVICE MASK    PRT FUNCTION REQ.   * 00287903
.*   (EITHER B0 OR OPT=)                                              * 00288003
.*                                                                    * 00288103
.*   R1:0           R1:1           R1:2           R1:3                * 00288203
.*                                                                    * 00288303
.*   ADDRESS-OF-LIST-OR-PRINT-DATA-ETC.---------------                * 00288403
.*                                                                    * 00288503
.*                                                                    * 00288603
.*   ACR0:0         ACR0:1         ACR0:2         ACR0:3              * 00288703
.*                                                                    * 00288803
.*                                                LENGTH/ENDCH        * 00288903
.*                                                                    * 00289003
.*--------------------------------------------------------------------* 00289103
.*                                                                    * 00289203
.*   FLAGS :                                                          * 00289303
.*                                                                    * 00289403
.*     80 - ABEND IF NOT OPENED         (PRTOPEN)                     * 00289503
.*     40 - (DON'T USE)                 (PRTOPEN)                     * 00289603
.*     20 - SUPPRESS WTO IF NOT OPENED  (PRTOPEN)                     * 00289703
.*     10 - ABEND IF DD DUMMY           (PRTOPEN)                     * 00289803
.*     20 - NEW SHEET ON NEXT PAGE EJECT                              * 00289903
.*     04 - THIS RECORD CONTAINS ASA                                  * 00290003
.*     02 - THIS RECORD CONTAINS MACHINE CODE                         * 00290103
.*     01 - NO CONTROL CHARACTER IN RECORD                            * 00290203
.*                                                                    * 00290303
.*   TITLE/FOOTER #:   4 BITS EACH; TOTAL OF EACH (PRTOPEN); ELSE     * 00290403
.*     NUMBER OF TITLE/FOOTER THIS RECORD IS TO BE USED FOR           * 00290503
.*                                                                    * 00290603
.*   DEVICE MASK:  (0 TREATED AS 1)                                   * 00290703
.*                                                                    * 00290803
.*   NUMBER OF DEVICE (1-8) FOR PRTOPEN; R1 POINTS TO PRTWORK/PUNWORK * 00290903
.*   ONE BIT PER DEVICE TO RECEIVE THIS RECORD (E.G. DEV=3 WRITES THE * 00291003
.*     REQUEST TO DEVICES 1 AND 2)                                    * 00291103
.*                                                                    * 00291203
.*   FUNCTION:  INDEX TO REQUESTED FUNCTION:                          * 00291303
.*                                                                    * 00291403
.*   0 - CLOSE      1 - TCLOSE     2 - OPEN       3 - ROOM (COND.SPC) * 00291503
.*   4 - SPACE N    5 - FD LIST    6 - V-RECORD   7 - FIXED REC.      * 00291603
.*   8 - SEPARATE   9 - SNAP      10 - FD ITEM   11 - STRING RECORD   * 00291703
.*  12 - AM31 FREC                                                    * 00291803
.*                                                                    * 00291903
.*--------------------------------------------------------------------* 00292003
         GBLC  &MACPLAB,&PRTMODE                                        00292103
         GBLB  &MVSESA                                          GP04234 00292203
         LCLA  &I,&J,&K,&OPA,&VD,&D(8)                                  00292303
         LCLB  &F01,&F02,&F04,&F08,&F10,&F20,&F40,&F80,&B0Z             00292403
         LCLB  &INDEV                                            81259  00292503
         LCLC  &DC,&LNR,&OP                                      81259  00292603
.*   FOR AM31 SUPPORT, THE A0 FIELD IS NOW PLACED INTO ACCESS REGISTER  00292703
.*   0, BYTE 3                                                          00292803
.*                                                                      00292903
&MACPLAB SETC  '&NM'                                             81259  00293003
&K       SETA  N'&OPT                                            90309  00293103
&B0Z     SETB  ('&B0' EQ '' OR '&B0' EQ '0')  OPTION FLAGS ?            00293203
         AIF   (&B0Z AND &K EQ 0).DEFOPT      NO FLAGS, NO OPTIONS      00293303
         AIF   (&B0Z OR  &K EQ 0).WHATOPT                               00293403
         MNOTE 8,'&OM: BOTH B0 AND OPT SPECIFIED; B0=&B0 IGNORED'       00293503
.WHATOPT AIF   (NOT &B0Z).ITMNOPT    B0 - USE IT                        00293603
&J       SETA  0             COUNT OF PROCESSED OPERANDS                00293703
.ITMLOOP AIF   (&I GE &K).ITMTEST                                90309  00293803
&I       SETA  &I+1                                              90309  00293903
&DC      SETC  '&OPT(&I)'                                               00294003
         AIF   ('&DC' EQ '').ITMLOOP  IGNORE NULLS                      00294103
         AIF   ('&DC' EQ 'WTO').ITMLOOP  IGNORE SEMANTIC NULLS  GP03027 00294203
         AIF   ('&DC' EQ 'DUMMY').ITMLOOP  IGNORE SEMANTIC NULL         00294303
&J       SETA  &J+1                                              90309  00294403
&F80     SETB  (&F80 OR '&DC' EQ 'ABE' OR '&DC' EQ 'LIST')              00294503
&F80     SETB  (&F80 OR '&DC' EQ 'ABEND')                               00294603
&F40     SETB  (&F40 OR '&DC' EQ 'X9700')                               00294703
&F20     SETB  (&F20 OR '&DC' EQ 'SHEET' OR '&DC' EQ 'PAGE')            00294803
&F20     SETB  (&F20 OR '&DC' EQ 'AUX' OR '&DC' EQ 'TRAY2')             00294903
&F20     SETB  (&F20 OR '&DC' EQ 'AUXTRAY' OR '&DC' EQ 'TRAY')          00295003
&F20     SETB  (&F20 OR '&DC' EQ 'NOWTO')                               00295103
&F10     SETB  (&F40 OR '&DC' EQ 'NODUMMY')                             00295203
&F10     SETB  (&F40 OR '&DC' EQ 'ABDUMMY')                             00295303
&F04     SETB  (&F04 OR '&DC' EQ 'ASA')   (DEFAULT)                     00295403
&F02     SETB  (&F02 OR '&DC' EQ 'MC' OR '&DC' EQ 'MCC')                00295503
&F02     SETB  (&F02 OR '&DC' EQ 'SKIPEJE' OR '&DC' EQ 'NOEJE')         00295603
&F01     SETB  (&F01 OR '&DC' EQ 'NO' OR '&DC' EQ 'NOCC')               00295703
&F01     SETB  (&F01 OR '&DC' EQ 'EJECT2' OR '&DC' EQ '2EJECT')         00295803
         AGO   .ITMLOOP                                          90309  00295903
.ITMTEST ANOP  ,                                                        00296003
&OPA     SETA  &F80+&F40+&F20+&F10+&F08+&F04+&F02+&F01                  00296103
         AIF   (&OPA EQ &J).DONOPT  EACH OPERAND VALID ?                00296203
.BADOPT  MNOTE 4,'&OM: ERROR - OPT PARAMETER BAD: &OPT'                 00296303
.DONOPT  ANOP  ,                                                        00296403
&OPA     SETA  &F80*128+&F40+&F20*32+&F10*16+&F08*8+&F04*4+&F02*2+&F01  00296503
&OP      SETC  '&OPA'                                                   00296603
         AGO   .POSTOPT                                         GP99029 00296703
.ITMNOPT ANOP  ,                                                 90309  00296803
&OP      SETC  '&B0'         USE USER'S PASSED VALUE             90309  00296903
         AIF   ('&OP' NE '').POSTOPT                                    00297003
.DEFOPT  ANOP  ,                                                        00297103
&OP      SETC  '0'           MAKE IT NON-BLANK                          00297203
.POSTOPT AIF   ('&DEV' EQ '' OR '&DEV' EQ '0').NODV                     00297303
         AIF   ('&DEV' NE 'ALL' AND '&DEV' NE '255').DVSOM      GP03240 00297403
&VD      SETA  255                                                      00297503
         AGO   .NODV                                                    00297603
.DVSOM   AIF   (K'&DEV LT 2).DVSOL                               81259  00297703
         AIF   ('&DEV'(1,1) NE '=').DVSOL                        81259  00297803
&INDEV   SETB  1             SET INDIRECT DEVICE NUMBER          81259  00297903
         AGO   .NODV                                             81259  00298003
.DVSOL   ANOP  ,                                                 81259  00298103
&I       SETA  0                                                        00298203
&K       SETA  N'&DEV                                                   00298303
.DEVLOOP ANOP  ,                                                        00298403
&I       SETA  &I+1                                                     00298503
         AIF   (&I GT &K).DVEND                                         00298603
         AIF   ('&DEV(&I)' EQ '').DEVLOOP                               00298703
         AIF   ('&DEV(&I)' EQ '0').DEVLOOP                              00298803
         AIF   ('&DEV(&I)' LT '1' OR '&DEV(&I)' GT '8').DVERR           00298903
&D(&DEV(&I)) SETA  1                                                    00299003
         AGO   .DEVLOOP                                                 00299103
.DVERR   MNOTE 8,'*** INVALID DEVICE NUMBER &DEV(&I)'                   00299203
         AGO   .DEVLOOP                                                 00299303
.DVEND   ANOP  ,                                                        00299403
&VD      SETA  128*&D(8)+64*&D(7)+32*&D(6)+16*&D(5)+8*&D(4)             00299503
&VD      SETA  &VD+4*&D(3)+2*&D(2)+&D(1)                                00299603
.NODV    AIF   ('&OP' NE '0' OR '&B1' NE '0').LONG              GP99029 00299703
         AIF   (&VD GT 15).LONG                                         00299803
&J       SETA  &VD*256+&FUN                                             00299903
         MACPARM R0,&J       LOAD DEVICE/FUNCTION INDEX          81259  00300003
         AGO   .POST0                                            81259  00300103
.LONG    ANOP  ,                                                 82326  00300203
&MACPLAB L     R0,=AL1(&OP,&B1+0,&VD,&FUN)                       81259  00300303
&MACPLAB SETC  ''            CANCEL LABEL                        81259  00300403
.POST0   AIF   (NOT &INDEV).LOAD1                                81259  00300503
&VD      SETA  K'&DEV-1                                          81259  00300603
&DC      SETC  '&DEV'(2,&VD)                                     81259  00300703
&MACPLAB ICM   R0,2,&DC                                          81259  00300803
&MACPLAB SETC  ''                                                81259  00300903
.LOAD1   AIF   ('&FUN' EQ '0' OR '&FUN' EQ '1').BAL  CLOSE?             00301003
         MACPARM R1,&A1      LOAD PARAMETER REGISTER                    00301103
         AIF   ('&A80' EQ 'OFF').NOTHIGH                        GP03025 00301203
         O     R1,=X'80000000'  SET LIST BIT (PRTBIG)           GP03025 00301303
.NOTHIGH AIF   (NOT &MVSESA).VER24                              GP04234 00301403
         AIF   (&FUN EQ 11 OR &FUN EQ 12).ACR                   GP03025 00301503
.VER24   AIF   ('&A0' EQ '' OR '&A0' EQ '0').BAL                 90309  00301603
&K       SETA  K'&A0                                            GP05190 00301703
         AIF   (&K LT 3).VER24I                                 GP05190 00301803
         AIF   ('&A0'(1,1) NE '(' OR '&A0'(2,1) EQ '(').VER24I  GP05190 00301903
         AIF   ('&A0'(&K,1) NE ')' OR '&A0'(&K-1,1) EQ ')').VER24I      00302003
         LA    R1,0(,R1)     CLEAR HIGH BYTE                    GP05190 00302103
         MACPARM R14,&A0,OP=LR,OPR=LR                           GP05190 00302203
         SLL   R14,24                                           GP05190 00302303
         OR    R1,R14        INSERT LENGTH                      GP05190 00302403
         AGO   .BAL                                             GP05190 00302503
.VER24I  ICM   R1,8,=AL1(&A0)                                    90309  00302603
         AGO   .BAL                                                     00302703
.ACR     AIF   ('&A0' NE '' AND '&A0' NE '0').ACRLOAD                   00302803
         AIF   (&FUN EQ 11 AND '&A0' EQ '0').ACRLOAD                    00302903
         MNOTE 8,'&OM: REQUIRED LENGTH VALUE MISSING'                   00303003
.ACRLOAD MACPARM R15,&A0                                                00303103
.*NEED(R) N     R15,=X'000000FF'  FOR FUTURE USE                        00303203
         SAR   R0,R15        LOAD INTO ACCESS REGISTER                  00303303
.BAL     AIF   ('&PRTMODE' EQ 'V').VCON                                 00303403
         L     R15,@PRINTER                                             00303503
         AGO   .BALR                                                    00303603
.VCON    L     R15,=V(@PRINTER)                                         00303703
.BALR    BASR  R14,R15                                          GP99020 00303803
         MEND  ,                                                        00303903
./ ADD NAME=PRTLIST  0100-03025-03025-1534-00011-00011-00000-GERHARD 00 00304003
         MACRO                                                          00304103
&NM      PRTLIST &LAD,&DEV=,&TITLE=0,&FOOTER=0,&OPT=,&MODE=      90309  00304203
         LCLA  &HF                                                      00304303
         AIF   (&TITLE EQ 0 AND &FOOTER EQ 0).NOT                       00304403
         AIF   (&TITLE LT 16 AND &FOOTER LT 16).OKT                     00304503
         MNOTE 4,'*** NON-NUMERIC TITLE/FOOTER NOT SUPPORTED'           00304603
.OKT     ANOP  ,                                                        00304703
&HF      SETA  &TITLE*16+&FOOTER                                        00304803
.NOT     ANOP  ,                                                        00304903
&NM      PRTCOM PRTLIST,FUN=5,B1=&HF,A1=&LAD,DEV=&DEV,OPT=&OPT          00305003
         MEND                                                           00305103
./ ADD NAME=PRTOPEN  0100-03025-03025-1534-00007-00007-00000-GERHARD 00 00305203
         MACRO                                                          00305303
&NM      PRTOPEN &WORK,&DEV=,&OPT=                                      00305403
         LCLA  &I,&J                                                    00305503
         LCLB  &A,&D,&W                                                 00305603
&J       SETA  N'&OPT                                                   00305703
&NM      PRTCOM PRTOPEN,FUN=2,A1=&WORK,DEV=&DEV,OPT=&OPT                00305803
         MEND  ,                                                        00305903
./ ADD NAME=PRTROOM  0100-03025-03025-1534-00004-00004-00000-GERHARD 00 00306003
         MACRO                                                          00306103
&NM      PRTROOM &COUNT,&DEV=,&OPT=                              90309  00306203
&NM      PRTCOM PRTROOM,FUN=3,A1=&COUNT,DEV=&DEV,OPT=&OPT               00306303
         MEND                                                           00306403
./ ADD NAME=PRTSPACE 0100-03025-03025-1534-00004-00004-00000-GERHARD 00 00306503
         MACRO                                                          00306603
&NM      PRTSPACE &COUNT,&DEV=,&OPT=                             90309  00306703
&NM      PRTCOM PRTSPACE,FUN=4,A1=&COUNT,DEV=&DEV,OPT=&OPT              00306803
         MEND                                                           00306903
./ ADD NAME=PRTV     0101-03025-04080-2216-00011-00011-00001-GERHARD 00 00307003
         MACRO                                                          00307103
&NM      PRTV  &VAD,&DEV=,&TITLE=0,&FOOTER=0,&CC=,&OPT=,&MODE=   90309  00307203
         LCLA  &CT,&HF                                                  00307303
         AIF   (&TITLE EQ 0 AND &FOOTER EQ 0).NOT                       00307403
         AIF   (&TITLE LT 16 AND &FOOTER LT 16).OKT                     00307503
         MNOTE 4,'*** NON-NUMERIC TITLE/FOOTER NOT SUPPORTED'           00307603
.OKT     ANOP  ,                                                        00307703
&HF      SETA  &TITLE*16+&FOOTER                                        00307803
.NOT     ANOP  ,                                                 90309  00307903
&NM      PRTCOM PRTV,FUN=6,B1=&HF,A1=&VAD,DEV=&DEV,OPT=(&OPT,&CC)       00308003
         MEND                                                           00308103
./ ADD NAME=PRTWORK  0103-03025-08104-1218-00075-00068-00000-GERHARD 00 00308203
         MACRO                                                          00308303
&NM      PRTWORK &DD,&ALTDD,&TITLE=0,&FOOTER=0,&LPP=0,&WIDTH=0,        *00308403
               &FILL=0,&RECFM=0,&PAGE=0,&SPAGE=0,&PGUP=NO,&EXLST=0,    *00308503
               &BUF=                                            GP08088 00308603
         LCLA  &PFG,&I,&J,&K                                     84169  00308703
         LCLB  &B0,&B1,&B2,&B3,&B4,&B5,&B6,&B7,&PFX             GP05120 00308803
         LCLC  &REC                                              81155  00308903
&REC     SETC  '&RECFM'                                          81155  00309003
         AIF   ('&REC' EQ '0').DEFREC                            81155  00309103
&I       SETA  K'&RECFM                                          81155  00309203
         AIF   (&I LT 4).NRECSD                                  81155  00309303
         AIF   ('&RECFM'(&I,1) EQ '''').DEFREC                   81155  00309403
.NRECSD  AIF   (&J GE &I).DONREC                                 81155  00309503
&J       SETA  &J+1                                              81155  00309603
         AIF   ('&REC'(&J,1) EQ 'U').RECU                        81155  00309703
         AIF   ('&REC'(&J,1) EQ 'V').RECV                        81155  00309803
         AIF   ('&REC'(&J,1) EQ 'F').RECF                        81155  00309903
         AIF   ('&REC'(&J,1) EQ 'D').RECD                        81155  00310003
         AIF   ('&REC'(&J,1) EQ 'T').RECT                        81155  00310103
         AIF   ('&REC'(&J,1) EQ 'B').RECB                        81155  00310203
         AIF   ('&REC'(&J,1) EQ 'S').RECS                        81155  00310303
         AIF   ('&REC'(&J,1) EQ 'M').RECM                        81155  00310403
         AIF   ('&REC'(&J,1) EQ 'N').RECN                        81271  00310503
         AIF   ('&REC'(&J,1) NE 'A').DEFREC                      81155  00310603
&B5      SETB  1                                                 81155  00310703
         AGO   .NRECSD                                           81155  00310803
.RECM    ANOP  ,                                                 81155  00310903
&B6      SETB  1                                                 81155  00311003
         AGO   .NRECSD                                           81155  00311103
.RECN    ANOP  ,             SUPPRESS CC DEFAULT IN PRTOPEN      81271  00311203
&B7      SETB  1                                                 81271  00311303
         AGO   .NRECSD                                           81271  00311403
.RECS    ANOP  ,                                                 81155  00311503
&B4      SETB  1                                                 81155  00311603
         AGO   .NRECSD                                           81155  00311703
.RECB    ANOP  ,                                                 81155  00311803
&B3      SETB  1                                                 81155  00311903
         AGO   .NRECSD                                           81155  00312003
.RECD    AIF   (&B0 OR &B1).DEFREC     FAIL VD, ETC.             81155  00312103
.RECT    ANOP  ,                                                 81155  00312203
&B2      SETB  1                                                 81155  00312303
.RECV    ANOP  ,                                                 81155  00312403
&B1      SETB  1                                                 81155  00312503
         AGO   .NRECSD                                           81155  00312603
.RECU    ANOP  ,                                                 81155  00312703
&B1      SETB  1                                                 81155  00312803
.RECF    ANOP  ,                                                 81155  00312903
&B0      SETB  1                                                 81155  00313003
         AGO   .NRECSD                                           81155  00313103
.DONREC  ANOP  ,                                                 81155  00313203
&REC     SETC  'B'''.'&B0&B1&B2&B3&B4&B5&B6&B7'.''''             81155  00313303
.DEFREC  AIF   ('&PAGE' EQ '0').NOPG                                    00313403
&I       SETA  &PAGE                                                    00313503
&PFG     SETA  1             SET PAGE FEED-BACK                         00313603
&PFX     SETB  1             EXPAND PAGE VALUES                 GP05120 00313703
.NOPG    AIF   ('&SPAGE' EQ '0').NOSPG                                  00313803
&J       SETA  &SPAGE                                                   00313903
&PFG     SETA  1             SET PAGE FEED-BACK                         00314003
&PFX     SETB  1             EXPAND PAGE VALUES                 GP05120 00314103
.NOSPG   AIF   ('&PGUP' EQ 'NO').NOPGUP                          84169  00314203
&PFG     SETA  3             SET UPDATING BY USER                84169  00314303
         AIF   ('&PGUP' EQ '' OR '&PGUP' EQ 'YES').NOPGUP        84169  00314403
         MNOTE 4,'INVALID PGUP OPTION : &PGUP'                   84169  00314503
.NOPGUP  AIF   ('&EXLST' EQ '0').NOLST1                                 00314603
&PFG     SETA  &PFG+4        SET EXIT LIST FLAG                  84169  00314703
.NOLST1  AIF   ('&BUF' NE '1').NOBUF1                           GP08088 00314803
&PFG     SETA  &PFG+16       SIGNLE BUFFER                      GP08088 00314903
.NOBUF1  ANOP  ,                                                GP08088 00315003
         DC    0H'0'                                                    00315103
&NM      DC    CL8'&DD ',CL8'&ALTDD ',AL2(&LPP),AL1(&FILL,&WIDTH,&TITLE*00315203
               ,&FOOTER,&REC,&PFG)                                      00315303
         AIF   (NOT &PFX).SKIPPG#                               GP05120 00315403
         DC    Y(&I,&J)      PAGE/SUB-PAGE FEEDBACK AREA                00315503
.SKIPPG# DC    AL4(&EXLST)   EXIT LIST POINTER                   84169  00315603
.MEND    MEND  ,                                                        00315703
./ ADD NAME=RMODE    8000-04314-04314-2101-00004-00004-00000-GERHARD 00 00315803
         MACRO ,                                                        00315903
         RMODE ,                                                        00316003
.*   DUMMY MACRO CREATED TO SUPPORT ASSEMBLY UNDFER HERCULES (XF ASM)   00316103
         MEND  ,                                                        00316203
./ ADD NAME=RSECT    8002-04312-04314-2101-00004-00013-00000-GERHARD 00 00316303
         MACRO ,                                                        00316403
&NM      RSECT ,             HERCULES SUPPORT             ADDED GP04234 00316503
&NM      CSECT ,                                                        00316603
         MEND  ,                                                        00316703
./ ADD NAME=SAVEX    0105-98322-04342-1546-00043-00023-00025-GERHARD 00 00316803
         MACRO ,                                                        00316903
&L       SAVEX &R1,&R3,&LOC,&TYPE=*,&SETAM=,&WORK=R14           GP99018 00317003
         GBLB  &ZZSVBSM,&MVSESA                                         00317103
         LCLC  &NM                                                      00317203
&NM      SETC  '&L'                                                     00317303
.*                                                                      00317403
.*             COPYRIGHT 1981 BY EXPERT SYSTEMS PROGRAMMING INC.        00317503
.*                               347 ORCHARD STREET                     00317603
.*                               VIENNA, VIRGINIA   22180               00317703
.*                                                                      00317803
.*                        ALL RIGHTS RESERVED.                          00317903
.*                                                                      00318003
.*             THIS MACRO IS NOT TO BE DISTRIBUTED WITHOUT PERMISSION,  00318103
.*             AS DESCRIBED IN MEMBER $$RIGHTS.                         00318203
.*                                                                      00318303
.*       CODE ADDED TO PRESERVE CALLER'S AMODE AND OPTIONALLY SET AMODE 00318403
.*                                                              GP99018 00318503
         AIF   ('&TYPE' EQ 'BSM').BSM                           GP98322 00318603
         AIF   ('&TYPE' EQ 'STM').STM                                   00318703
         MNOTE 8,'SAVEX: TYPE=&TYPE UNKNOWN - TYPE=STM ASSUMED'         00318803
         AGO   .STM                                                     00318903
.BSM     ANOP  ,                                                GP98322 00319003
&ZZSVBSM SETB  1             SET FLAG FOR ENDM                          00319103
&NM      BSM   R14,0         GET CALLER'S AMODE                 GP98322 00319203
&NM      SETC  ''            DONE WITH LABEL                            00319303
.STM     ANOP  ,                                                        00319403
&NM      STM   &R1,&R3,&LOC                                             00319503
&NM      SETC  ''            DONE WITH LABEL                            00319603
         AIF   ('&SETAM' EQ '' OR '&SETAM' EQ 'ANY').MEND       GP04234 00319703
         AIF   (NOT &MVSESA).MEND                               GP04234 00319803
         BASR  R14,0                                            GP04050 00319903
         USING *,R14                                            GP04050 00320003
         LA    &WORK,ZZSV&SYSNDX                                GP99018 00320103
         AIF   ('&SETAM' EQ '24' OR '&SETAM' EQ 'AM24').SETCM   GP99018 00320203
         LA    R0,1                                             GP99018 00320303
         SLL   R0,31         MAKE 80000000                      GP99018 00320403
         OR    &WORK,R0                                         GP99018 00320503
         AIF   ('&SETAM' EQ '31' OR '&SETAM' EQ 'AM31').SETCM   GP99018 00320603
   MNOTE 8,'SAVEX: UNSUPPORTED SETAM VALUE: &SETAM - AM31 ASSUMED'      00320703
.SETCM   BSM   0,&WORK       CHANGE TO REQUIRED MODE            GP99018 00320803
         DROP  R14                                              GP04050 00320903
ZZSV&SYSNDX DS 0H                                               GP99018 00321003
.MEND    MEND  ,                                                GP99018 00321103
./ ADD NAME=SERVCALL 0109-99020-12043-1622-00073-00044-00006-GERHARD 00 00321203
         MACRO ,                                                        00321303
&NM      SERVCALL &CODE,&ADDR,&REG2,&ERR=,&CC0=,&CC4=,&CC8=,&LEN=,     *00321403
               &MODE=BAL,&OPT=                                  GP06287 00321503
         GBLC  &SRVCM@R,&MACPLAB                                 81148  00321603
         GBLB  &MVSXA                                           GP04234 00321703
         LCLA  &I,&J,&K,&OPA                                    GP06287 00321803
         LCLB  &F0,&F1,&F2,&F3,&F4,&F5,&F6,&F7                  GP06287 00321903
         LCLC  &LERR,&DC                                         81148  00322003
         AIF   ('&MACPLAB' NE '' AND '&NM' NE '').LABTWO        GP12043 00322103
         AIF   ('&MACPLAB' NE '').LABCOM                        GP12043 00322203
         AGO   .LABSET                                          GP12043 00322303
.LABTWO  MACPARM MODE=LBL    EXPAND LABEL FOR MACPLAB           GP12043 00322403
.LABSET  ANOP  ,                                                GP12043 00322503
&MACPLAB SETC  '&NM'                                            GP12043 00322603
.LABCOM  MACPARM R2,&REG2,NULL=SKIP                              85070  00322703
         MACPARM R1,&ADDR,NULL=SKIP                                     00322803
         AIF   ('&CODE' EQ '').NOR0                                     00322903
         AIF   ('&CODE'(1,1) EQ '(').REG0                               00323003
         MACPARM R0,VEN&CODE                                            00323103
         AIF   ('&LEN' EQ '').NOR0                               81148  00323203
         MACPARM R0,8,=AL1(&LEN),OP=ICM,MODE=THREE               81148  00323303
         AGO   .NOR0                                                    00323403
.REG0    MACPARM R0,&CODE                                               00323503
.NOR0    AIF   (T'&OPT EQ 'O').NOOPT   NO OPTIONS               GP06287 00323603
&K       SETA  N'&OPT                                           GP06287 00323703
&J       SETA  0             COUNT OF PROCESSED OPERANDS        GP06287 00323803
&I       SETA  0             CLEAR INDEX                        GP06287 00323903
.ITMLOOP AIF   (&I GE &K).ITMTEST                               GP06287 00324003
&I       SETA  &I+1                                             GP06287 00324103
&DC      SETC  '&OPT(&I)'                                       GP06287 00324203
         AIF   ('&DC' EQ '').ITMLOOP  IGNORE REAL NULL          GP06287 00324303
         AIF   ('&DC' EQ 'NONE').ITMLOOP  IGNORE SEMANTIC NULL  GP06287 00324403
&J       SETA  &J+1                                             GP06287 00324503
&F0      SETB  (&F0  OR '&DC' EQ 'LIST')                        GP06287 00324603
&F0      SETB  (&F0  OR '&DC' EQ 'TEXT')                        GP06287 00324703
&F6      SETB  (&F6  OR '&DC' EQ '2'  OR '&DC' EQ '3')          GP06287 00324803
&F7      SETB  (&F7  OR '&DC' EQ '1'  OR '&DC' EQ '3')          GP06287 00324903
         AGO   .ITMLOOP                                         GP06287 00325003
.ITMTEST ANOP  ,                                                GP06287 00325103
&OPA     SETA  &F0+&F1+&F2+&F3+&F4+&F5+&F6+&F7                  GP06287 00325203
         AIF   (&OPA EQ &J).DONOPT  EACH OPERAND VALID ?        GP06287 00325303
.BADOPT  MNOTE 4,'SERVCALL: ERROR - OPT PARAMETER BAD: &OPT'    GP06287 00325403
.DONOPT  AIF   (&J EQ 0).NOOPT    SKIP IF ONLY NULLS            GP06287 00325503
         MACPARM R0,4,=AL1(&OPA),OP=ICM,MODE=THREE              GP06287 00325603
.NOOPT   MACPARM R15,@SERVICE,OP=L  GET MODULE ADDRESS                  00325703
         AIF   ('&MODE' EQ 'BAL' OR '&MODE' EQ '').BALMODE       90337  00325803
         AIF   ('&MODE' EQ 'SYNCH').SYNMODE                      90337  00325903
         MNOTE 8,'INVALID MODE=&MODE'                            90337  00326003
.SYNMODE ANOP  ,                                                 90337  00326103
&MACPLAB SYNCH (15),RESTORE=YES  INVOKE AND SAVE MODE            90337  00326203
         AGO   .COMMODE                                          90337  00326303
.BALMODE AIF   (&MVSXA).BASMODE                                 GP04234 00326403
&MACPLAB BALR  R14,R15       CALL THE @SERVICE ROUTINE          GP04234 00326503
         AGO   .COMMODE                                         GP04234 00326603
.BASMODE ANOP  ,                                                 90337  00326703
&MACPLAB BASSM R14,R15       CALL THE @SERVICE ROUTINE                  00326803
.COMMODE AIF   ('&CC0' EQ '' AND '&CC4' EQ '' AND '&CC8' EQ '').NOCC    00326903
         AIF   ('&ERR' EQ '' OR '&ERR' EQ 'NO').NODUPE           81148  00327003
         MNOTE 4,'CC= AND ERR= ARE MUTUALLY EXCLUSIVE'           81148  00327103
.NODUPE  ANOP  ,                                                 81148  00327203
&MACPLAB SETC  ''                                                81148  00327303
         CH    R15,=H'4'     TEST RETURN                         81148  00327403
         MACPARM &CC0,OP=BL,OPR=BLR,MODE=ONE,NULL=SKIP          GP02241 00327503
         MACPARM &CC4,OP=BE,OPR=BER,MODE=ONE,NULL=SKIP          GP02241 00327603
         MACPARM &CC8,OP=BH,OPR=BHR,MODE=ONE,NULL=SKIP          GP02241 00327703
         AGO   .MEND                                             81148  00327803
.NOCC    AIF   ('&ERR' EQ 'NO').MEND                             81148  00327903
&LERR    SETC  '&ERR'                                            81148  00328003
         AIF   ('&LERR' NE '').DOERR                             81148  00328103
&LERR    SETC  '&SRVCM@R'                                        81148  00328203
         AIF   ('&LERR' EQ '').MEND                              81148  00328303
.DOERR   BXH   R15,R15,&LERR  GO TO SET ERROR MESSAGE            81148  00328403
.MEND    MEND  ,                                                        00328503
./ ADD NAME=SERVCOMP                                                    00328603
         MACRO ,                                                        00328703
&NM      SERVCOMP &DSECT=YES,&PFX=WCM,                                 *00328803
               &FG1=0,&FG2=0,&LINE#=0,&DELTA=1,                        *00328903
               &BUFAD=0,&BUFMX=0,&RECAD=0,&RECMX=0,&CODAD=0      81263  00329003
         LCLC  &P,&NAME                                                 00329103
&NAME    SETC  '&NM'                                                    00329203
&P       SETC  'WCM'                                                    00329303
         AIF   ('&NAME' NE '').HAVENM                                   00329403
&NAME    SETC  'SERVCOMP'                                               00329503
.HAVENM  AIF   ('&DSECT' NE 'YES').NOSECT                               00329603
&NAME    DSECT ,                                                        00329703
         AGO   .TESTP                                                   00329803
.NOSECT  AIF   ('&NM' EQ '').TESTP                                      00329903
&NM      DS    0A .                                                     00330003
.TESTP   AIF   ('&PFX' EQ '').HAVEP                                     00330103
&P       SETC  '&PFX'                                                   00330203
.HAVEP   ANOP  ,                                                        00330303
&P.BUFAD DC    A(&BUFAD)     CURRENT BLOCK ADDRESS                      00330403
&P.BUFMX DC    AL2(&BUFMX)     MAXIMUM BLOCK SIZE                       00330503
&P.BUFLN DC    H'0'            OFFSET TO NEXT LINE                      00330603
&P.RECAD DC    A(&RECAD)     CURRENT RECORD ADDRESS                     00330703
&P.RECMX DC    AL2(&RECMX)     MAXIMUM RECORD LENGTH                    00330803
&P.RECLN DC    H'0'            CURRENT RECORD LENGTH                    00330903
&P.CODAD DC    A(&CODAD)     ADDRESS OF CODE WORD OR ZERO               00331003
&P.LINE# DC    A(&LINE#)       CURRENT LINE NUMBER (BINARY)             00331103
&P.LINEB DC    CL8' '          CURRENT LINE NUMBER (EBCDIC)             00331203
&P.LINEP DC    A(&LINE#-&DELTA)  PREVIOUS LINE NUMBER                   00331303
&P.DELTA DC    A(&DELTA)     LINE NUMBERING INCREMENT (FWD)             00331403
&P.FG1   DC    AL1(&FG1)     PROCESSING FLAGS                           00331503
&P.F1NIH EQU   X'80'           NIH FORMAT                               00331603
&P.F1OSI EQU   X'40'           OSI FORMAT (X'80' IN LINE# FOR FWD)      00331703
&P.F1HWD EQU   X'20'           HALF-WORD LINE # (RAND, ETC.)            00331803
&P.F1INT EQU   X'10'           NUMBER IN CL8 FORMAT                     00331903
&P.F1EDT EQU   X'08'           NUMBER IN 4C.3C FORMAT                   00332003
&P.F1TSO EQU   X'04'           LEFT-ADJUSTED LINE NUMBERS               00332103
&P.F1LCC EQU   X'02'           RETURN CARR.CONTROL/LINE/TEXT (+F1TSO)   00332203
&P.F1NB# EQU   X'01'           INSERT NUMBERS EVEN IF NON-BLANK         00332303
&P.FG2   DC    AL1(&FG2)     CONTROL FLAGS                              00332403
&P.F2NPR EQU   X'80'           NO LINE DECOMPRESSION (REBLOCK)          00332503
&P.F2RDW EQU   X'40'           V-FORMAT RECORD RETURNED                 00332603
&P.F2COD EQU   X'02'           CODE WORD CHECKED                        00332703
&P.F2PSW EQU   X'01'           ENCRYPTION/DECRYPTION REQUIRED           00332803
&P.FG3   DC    X'00'         NEW NIH LINE FLAGS                         00332903
&P.F3RAW EQU   X'80'           UNCOMPRESSED RECORD                      00333003
&P.F3L16 EQU   X'40'           16-BIT LENGTH FIELD                      00333103
&P.F3NSP EQU   X'20'           NO SPECIAL CONTROL CHARACTERS            00333203
&P.F3MRK EQU   X'10'           LINE IS FLAGGED 'CHANGED'                00333303
&P.F308  EQU   X'08'             RESERVED                               00333403
&P.F3NIF EQU   X'F8'           ALL NEW NIH FLAG BITS                    00333503
&P.F3NIH EQU   X'04'           COPY OF NIH FORMAT FLAG                  00333603
&P.LINEH DC    C' '         SPILL BYTE FOR EDIT LINE OVERFLOW           00333703
         MEND  ,                                                        00333803
./ ADD NAME=SERVDEFS 0107-03112-09277-1509-00041-00029-00007-GERHARD 00 00333903
         MACRO ,                                                        00334003
&NM     SERVDEFS &PARM=10                                               00334103
         GBLC  &MACPLAB,&SRVLMOD(20),&SRVLDEL(20)                       00334203
         GBLB  &SRVBMOD(20)                                             00334303
         GBLB  &BUGBEAR,&ZZSPIE                                         00334403
         GBLB  &MVS,&MVSSP,&MVSXA,&MVSESA                               00334503
         GBLA  &SRVNMOD                                                 00334603
.*--------------------------------------------------------------------* 00334703
.*  SERVDEFS IS USED IN THE PROGRAM'S MAIN SAVE AREA TO EXPAND THE    * 00334803
.*  ADDRESS LABELS FOR STANDARD SERVICE ROUTINES (@SERVICE, @PRINTER, * 00334903
.*  ETC.).  WHEN RUNNING IN DEBUG MODE, IT ALSO EXPANDS PGMTRACE AND  * 00335003
.*  DEBTRACE WORK AREAS.                                              * 00335103
.*--------------------------------------------------------------------* 00335203
         LCLA  &I,&J                                                    00335303
&MACPLAB SETC  '&NM'         ENSURE CORRECT VALUE                       00335403
         MACPARM MODE=LABEL                                             00335503
@SERVICE DS    A             ADDRESS OF @SERVICE ROUTINE                00335603
@SERVEXC DS    A             EXECUTED INSTRUCITON (SVC, BASSM, ...)     00335703
@SERVTCA DS    A             ADDRESS OF @SERVICE TASK CONTROL AREA      00335803
.DSLOOP  AIF   (&I GE &SRVNMOD).NDLOOP                                  00335903
&I       SETA  &I+1                                                     00336003
         AIF   (&SRVBMOD(&I)).DSLOOP  SKIP EXPANSION ?                  00336103
&SRVLMOD(&I)  DS  A                                                     00336203
         AGO   .DSLOOP                                                  00336303
.NDLOOP  AIF   (NOT &ZZSPIE).NDSPIE  SKIP IF NOT (E)SPIE MODE   GP09277 00336403
@SPIEDER DS    A  *DEBUG*    @SPIEDER (E)SPIE INTERCEPT         GP09277 00336503
.NDSPIE  AIF   (NOT &BUGBEAR).PARM  SKIP IF NOT DEBUG MODE              00336603
@TRACE   DS    A  *DEBUG*    PGMTRACE ROUTINE                           00336703
    #TRC  DATA    *DEBUG*    PGMTRACE RE-ENTRANT WORK AREA      GP06319 00336803
         AIF   (NOT &MVSXA AND NOT &MVSESA).OLDBUG                      00336903
         DBT   MODE=D  *DEBUG*  DEBTRACE WORK AREA                      00337003
         AGO   .PARM                                                    00337103
.OLDBUG  DBO   MODE=D  *DEBUG*  DEBTROLD WORK AREA                      00337203
.PARM    AIF   ('&PARM' EQ '').MEND                                     00337303
.*DEFER* AIF   (T'&PARM' NE 'N').MEND                                   00337403
         AIF   ('&PARM' EQ '0').MEND                                    00337503
CALLPARM DS    (&PARM)A      PARAMETER LIST FOR SUBCALL, ETC.           00337603
RETCODE  DS    F             PROGRAM RETURN CODE                        00337703
RSNCODE  DS    F             ERROR REASON                               00337803
RR1CODE  DS    F             RETURNED R1                        GP04068 00337903
.MEND    MEND  ,                                                        00338003
./ ADD NAME=SERVFLAG 0101-02265-03075-2228-00149-00146-00003-GERHARD 00 00338103
*        SERVFLAG                                     UPDATED ON 90316  00338203
*          CALL CODES (R0) FOR '@SERVICE' ROUTINE                       00338303
VENMOD1  EQU   256           ENTRY MODIFIER 1                           00338403
VENMOD2  EQU   512           ENTRY MODIFIER 2                           00338503
VENMOD3  EQU   768           ENTRY MODIFIER 3                           00338603
VENMOD4  EQU   1024          ENTRY MODIFIER 4                           00338703
VENMOD5  EQU   1280          ENTRY MODIFIER 5                           00338803
VENMOD6  EQU   1536          ENTRY MODIFIER 6                           00338903
VENMOD7  EQU   1792          ENTRY MODIFIER 7                           00339003
VENMOD8  EQU   2048          ENTRY MODIFIER 8                           00339103
VENMOD9  EQU   2304          ENTRY MODIFIER 9                           00339203
VENMOD10 EQU   2560          ENTRY MODIFIER 10                          00339303
VENMOD11 EQU   2816          ENTRY MODIFIER 11                          00339403
VENMOD12 EQU   3072          ENTRY MODIFIER 12                          00339503
VENMOD13 EQU   3328          ENTRY MODIFIER 13                          00339603
VENMOD14 EQU   3584          ENTRY MODIFIER 14                          00339703
VENMOD15 EQU   3840          ENTRY MODIFIER 15                          00339803
         SPACE 1                                                        00339903
VENCLOSE EQU   00            CLOSE/FREEMAIN ENTRY                       00340003
VENFREEM EQU   00+VENMOD1      CLOSE/FREE - KEEP @PRINTER OPEN          00340103
VENINITG EQU   01            INIT - LOCAL GETMAINS/LOADS/OPENS          00340203
VENLPALD EQU   02            MODULE LOAD FROM LPA, OR STEP/LINKLIB      00340303
VENLPADL EQU   02+VENMOD1      MODULE CLOSE AND DELETE                  00340403
VENLPA@0 EQU   02+VENMOD2      ZERO MODULE ADDRESS IN USERCVT           00340503
VENAPFON EQU   03            AUTH - SET APF ON                          00340603
VENAPFOF EQU   03+VENMOD1      SET APF AUTH OFF                         00340703
VENPASON EQU   03+VENMOD2      SET JSCB PASS ON                         00340803
VENPASOF EQU   03+VENMOD3      SET JSCBPASS OFF                         00340903
VENCANON EQU   03+VENMOD4      SET CSCB CANCEL ON                       00341003
VENCANOF EQU   03+VENMOD5      SET CSCB CANCEL OFF                      00341103
VENNO522 EQU   03+VENMOD6      SET SMF OFF (NO 522)                     00341203
VENDO522 EQU   03+VENMOD7      SET SMF ON (ALLOW 522, ETC.)             00341303
VENUCBUM EQU   05            SEQUENTIAL UCB LOOKUP                      00341403
VENUCBNM EQU   05+VENMOD1      LOCATE UCB BY UNITNAME                   00341503
VENUCBVS EQU   05+VENMOD2      LOCATE UCB BY VOLSER                     00341603
VENUCBDK EQU   05+VENMOD3      LOCATE DISK UCB BY VOLSER                00341703
VENUCBGN EQU   05+VENMOD4      GET GENERIC NAME FOR UCB                 00341803
VENUCBDT EQU   05+VENMOD5      GET UCB TYPE FROM GENERIC                00341903
VENTIOLP EQU   06            TIOT ENTRY LOOP                            00342003
VENTIOLK EQU   06+VENMOD1      TIOT LOOP - SKIP SPECIAL ENTRIES         00342103
VENTIODD EQU   06+VENMOD2      TIOT - LOCATE DDNAME                     00342203
VENTIOUA EQU   06+VENMOD3      TIOT - LOCATE BY UCB ADDRESS             00342303
VENSIOTE EQU   06+VENMOD4      SIOT - LOCATE BY TIOT ADDRESS            00342403
VENSWARL EQU   06+VENMOD5      SWARL - GET SWA (TEXT) FROM SVA TOKEN    00342503
VENSWAAD EQU   06+VENMOD6      SWAAD - GET SWA ADDRESS FROM SVA TOKEN   00342603
VENSWAAB EQU   06+VENMOD7      SWAAB - GET SWA ADDRESS FROM SVA TOKEN   00342703
VENDSABL EQU   06+VENMOD8      DSAB - LOOP THROUGH ENTRIES              00342803
VENDSABD EQU   06+VENMOD9      DSAB - FIND BY DDNAME                    00342903
VENSORTB EQU   07            BUBBLE SORT                                00343003
VENSORTH EQU   07+VENMOD1      HEAP SORT                                00343103
VENBINLK EQU   07+VENMOD3      BINARY TABLE LOOKUP                      00343203
VENDVTBL EQU   08            DEVICE TABLE LOCATE                        00343303
VENDVCAP EQU   08+VENMOD1      DEVICE CAPACITY/BALANCE                  00343403
VENDVEXT EQU   08+VENMOD2      DEVICE EXTENT SIZE CALCULATION           00343503
VENDVSPC EQU   08+VENMOD3      TRK=>CYL; CYL=>TRK CONVERSION            00343603
VENSCHFR EQU   09            SCHEDULE - FREE WORK AREA                  00343703
VENSCHIN EQU   09+VENMOD1      GET/INIT CSA WORK AREA                   00343803
VENSCHMV EQU   09+VENMOD2      MOVE/UPDATE WORK AREA                    00343903
VENSCHED EQU   09+VENMOD3      SCHEDULE AN SRB                          00344003
VENPGFIX EQU   09+VENMOD4      PAGEFIX LPA PAGE                         00344103
VENSWAPY EQU   09+VENMOD5      SET SWAPPABLE                            00344203
VENSWAPN EQU   09+VENMOD6      SET ADDRESS SPACE NONSWAPPABLE           00344303
VENGASID EQU   09+VENMOD7      VALIDATE BY ASID                         00344403
VENGASJB EQU   09+VENMOD8      VALIDATE BY JOBNAME                      00344503
VENGASCB EQU   09+VENMOD9      VALIDATE ASCB ONLY                       00344603
VENSSLOC EQU   09+VENMOD10     LOCATE SUBSYSTEM                         00344703
VENSSSET EQU   09+VENMOD11     SPECIFY SUBSYSTEM                        00344803
VENLOCAT EQU   10            CATALOG LOOKUP                             00344903
VENLOCMT EQU   10+VENMOD1      CAT. LOOK ON P/R AND RSV PACKS           00345003
VENLOCRT EQU   10+VENMOD2      CAT. LOOK ON P/R PACKS ONLY              00345103
VENCATCO EQU   10+VENMOD6      CAT. CONNECT CVOL INDEX                  00345203
VENJESVC EQU   11            JES(2) GENERIC SERVICES :                  00345303
VENJ2INF EQU   11              JES(2) INFO - GET SUBSYSTEM NAME         00345403
VENLOJOB EQU   11+VENMOD1      GET JOB DATA                             00345503
VENMDJOB EQU   11+VENMOD2      RESET HOLD (AND LOCAL) FLAGS             00345603
VENACGET EQU   16            GET CURRENT ACCOUNT/PRIVILEGES             00345703
VENACTST EQU   16+VENMOD1      TEST ACCOUNT NUMBER IN R1                00345803
VENACTSM EQU   16+VENMOD2      TEST AND RETURN ACCOUNT IN R1            00345903
VENACCON EQU   16+VENMOD3      CONVERT INTEGER TO EBCDIC ACCOUNT        00346003
VENACCNX EQU   16+VENMOD4      CONVERT INTEGER ACCOUNT TO EXTERNAL      00346103
VENUSGET EQU   17            GET USER ID                                00346203
VENUSTST EQU   17+VENMOD1      TEST USER ID                             00346303
VENAUTST EQU   18            TEST ACCOUNT/USERID COMBINATION            00346403
VENAUWYL EQU   18+VENMOD1      TEST ACCT/UID COMBINATION FOR WYLBUR     00346503
VENFMTAC EQU   19            CHECK FORMAT, BUT NOT VALIDITY OF ACCT     00346603
VENFMTLB EQU   19+VENMOD1      CHECK FORMAT OF LIBPAK NAME              00346703
VENFMTWY EQU   19+VENMOD2      CHECK FORMAT OF WYLBUR NAME              00346803
VENFMTTS EQU   19+VENMOD3      CHECK FORMAT OF TSO DSN                  00346903
VENGFORM EQU   20            CHECK GDA FORM TABLE                       00347003
VENGPAPR EQU   20+VENMOD1      CHECK GDA PAPER COST TABLE               00347103
VENDSTST EQU   32            CHECK DSN (NON-CATLG)                      00347203
VENDSCAT EQU   32+VENMOD1      CHECK DSN FROM CATALOG (GDG)             00347303
VENDSABB EQU   32+VENMOD2      EXTRACT 8-BYTE PORTION FROM DSN          00347403
VENDSDS1 EQU   32+VENMOD3      OBTAIN FORMAT 1 DSCB                     00347503
VENDSDJ1 EQU   32+VENMOD4      OBTAIN FORMAT 1 DSCB FROM JFCB           00347603
VENDSFMT EQU   32+VENMOD5      FORMAT DSORG/RECFM/OPTCD/BLKL/LRECL      00347703
VENDSMEM EQU   32+VENMOD6      CHECK MEMBER NAME                        00347803
VENRJFCB EQU   32+VENMOD7      GET JFCB FOR DDNAME                      00347903
VENPDSDE EQU   32+VENMOD8      DECODE PDS DIRECTORY ENTRY               00348003
VENDSDS4 EQU   32+VENMOD9      OBTAIN FORMAT 4 DSCB                     00348103
VENDSDJ4 EQU   32+VENMOD10     OBTAIN FORMAT 4 DSCB                     00348203
VENDDCLR EQU   32+VENMOD11     RE-INITIALIZE DD HAVING DISP=MOD         00348303
VENDSLIB EQU   33            CHECK DSN ON LIBPAK                        00348403
VENDSLIX EQU   33+VENMOD1      CHECK LIBPAK INDEX                       00348503
VENDSWYL EQU   34            CHECK WYLBUR DSN                           00348603
VENWYLDX EQU   34+VENMOD1      CHECK FOR WYLBUR INDEX                   00348703
VENDSGET EQU   34+VENMOD2      CHANGE SHORT TO LONG WYLBUR NAME         00348803
VENDSWYC EQU   34+VENMOD3      CHECK WYLBUR DSN IN CATALOG              00348903
VENDSTSO EQU   35            CHECK TSO DSNAME                           00349003
VENDSTSX EQU   35+VENMOD1      CHECK TSO INDEX                          00349103
VENDSTET EQU   35+VENMOD2      CHANGE SHORT TO LONG TSO NAME            00349203
VENVSNFG EQU   36            CHECK VOLUME ATTRIBUTE FLAGS               00349303
VENDSANY EQU   36+VENMOD1      CHECK VOLUME/DSNAME FOR VALIDITY         00349403
VENVSTMS EQU   36+VENMOD2      CHECK VS FOR TMS ELIGIBILITY             00349503
VENWCOMP EQU   37            WYLBUR COMPRESS ROUTINE                    00349603
VENWDCOM EQU   37+VENMOD1      WYLBUR DECOMPRESS ROUTINE                00349703
VENALCVS EQU   38            ALLOCATION - GET DDNAME FOR VTOC OPEN      00349803
VENALCDS EQU   38+VENMOD1      ALLOCATE A (PERM) DSN FROM JFCB          00349903
VENALCFR EQU   38+VENMOD2      RELEASE ALLOCATED TIOT ENTRY             00350003
VENALCDD EQU   38+VENMOD3      ALLOCATE DD FOR DSN                      00350103
VENALCFD EQU   38+VENMOD4      FREE DD                                  00350203
VENWYLOC EQU   39            WYLBUR MULTI-VOLUME LOCATE                 00350303
         SPACE 1                                                        00350403
VAASTC   EQU   X'80'    ACCOUNT PRIVILEGES - INSTALLATION DEFAULT       00350503
VAASYS   EQU   X'40'         SYSTEM PRIVILEGES                          00350603
VAASUP   EQU   X'20'         TECH SUPPORT                               00350703
VAAINH   EQU   X'10'         IN-HOUSE STAFF                             00350803
VAAUSER  EQU   X'08'         PLAIN OLD USER                             00350903
VAAOHD   EQU   X'04'         OVERHEAD ACCOUNT (WITH STC,SYS,SUP)        00351003
         SPACE 1                                                        00351103
VRPGER   EQU   0        RETURN VALUES : DISASTROUS ERROR                00351203
VRPARM   EQU   1             BAD PARM OR ENTRY                          00351303
VRSYNT   EQU   2             BAD CHARACTER OR SYNTAX ERROR              00351403
VRACCT   EQU   3             BAD ACCOUNT                                00351503
         SPACE 1                                                        00351603
VRNTOS   EQU   4             NON-OS DSN                                 00351703
VRNWYL   EQU   5             NOT LIBPAK/WYLBUR NAME                     00351803
VRDLEN   EQU   6             TOO FEW INDEX LEVELS                       00351903
VRDLON   EQU   7             TOO MANY INDEX LEVELS                      00352003
VRNWYX   EQU   8             INVALID SPECIAL (WYLBUR) INDEX             00352103
VRNPSW   EQU   9             NO PASSWORD ENTRY FOR WYLBUR USER          00352203
         SPACE 1                                                        00352303
VCMPARM  EQU   1   WCOMP/WDCOM:  INVALID PARM OR OPTION LIST            00352403
VCMNEDIT EQU   2             BLOCK NOT IN EDIT FORMAT                   00352503
VCMBKLEN EQU   3             INVALID BLOCK LENGTH                       00352603
VCMRCLEN EQU   4             INVALID/TRUNCATED RECORD LENGTH            00352703
VCMSEQ#  EQU   5             INVALID SEQUENCE # OR OVERFLOW             00352803
VCMSEQSQ EQU   6             LINE NUMBER OUT OF SEQUENCE                00352903
         SPACE 1                                                        00353003
./ ADD NAME=SERVINIT 0108-99124-09179-1137-00062-00044-00057-GERHARD 00 00353103
         MACRO ,                                                        00353203
&NM    SERVINIT &LPA=YES,&MAP=YES,&ERR=,&LIST=NO,&AMODE=*,       81167 *00353303
               &MODE=NEW                                        GP03129 00353403
         GBLA  &SVC@SVC      SVC NUMBER OF @SERVICE              83100  00353503
         GBLB  &MVSESA                                          GP04234 00353603
         GBLB  &SRVCM@P,&SRV#NUT                                        00353703
         GBLC  &PRTMAC,&SRVCM@R,&MACPLAB                                00353803
         AIF   ('&MODE' EQ 'NEW' AND &MVSESA).CALLSUB           GP04234 00353903
&NM      MACPARM R15,15,@SERVICE,MODE=THREE,OP=ICM,OPR=ICM              00354003
         BNZ   ZZZZ&SYSNDX+4                                            00354103
         AIF   ('&LPA' NE 'YES').NOLPA                                  00354203
         AIF   (&SVC@SVC EQ 0).DOLPA                             83100  00354303
         SR    R0,R0         REQUEST GETMAIN/INITIALIZATION      83100  00354403
         SVC   &SVC@SVC      CALL IT                             83100  00354503
         AGO   .COMMON                                           83100  00354603
.DOLPA   LPALOOK EP=@SERVICE,DCB=4                              GP03262 00354703
         AGO   .COMMON                                                  00354803
.NOLPA   AIF   ('&LPA' NE 'LINK').DOLOAD                        GP09179 00354903
         L     R0,=V(@SERVICE)    LINK IN                       GP09179 00355003
         AGO   .COMMON                                          GP09179 00355103
.DOLOAD  LOAD  EP=@SERVICE                                              00355203
.COMMON  AIF   ('&AMODE' EQ '*' AND &MVSESA).BSM                GP04234 00355303
ZZZZ&SYSNDX ST R0,@SERVICE                                              00355403
         AIF   (NOT &MVSESA).COMSET                             GP04234 00355503
         AIF   ('&AMODE' EQ '31').AM31                          GP99124 00355603
         AIF   ('&AMODE' EQ '24').AM24                          GP99124 00355703
         MNOTE 4,'UNDEFINED AMODE=&AMODE - AM24 ASSUMED'        GP99124 00355803
.AM24    MVI   @SERVICE,0    FORCE LOW                          GP99124 00355903
         AGO   .COMSET                                          GP99124 00356003
.AM31    OI    @SERVICE,X'80'   SET AM31 ON BASSM INVOCATION    GP99124 00356103
         AGO   .COMSET                                          GP99124 00356203
.BSM     ANOP  ,                                                GP99124 00356303
ZZZZ&SYSNDX LR R15,R0        COPY ADDRESS                       GP99124 00356403
         BSM   R15,0         IMPART CURRENT MODE                GP99124 00356503
         ST    R15,@SERVICE  AND STASH IT                       GP99124 00356603
         AGO   .COMSET                                                  00356703
.*--------------------------------------------------------------------* 00356803
.*  NEW INTERFACE FOR EXTERNAL INITIALIZATION ROUTINE SUBSERV         * 00356903
.*--------------------------------------------------------------------* 00357003
.*                                                                      00357103
.CALLSUB ANOP  ,                                                GP03129 00357203
&NM      MACPARM R0,(R0),MODE=EVEN,OP=SR,OPR=SR                 GP03129 00357303
         MACPARM R1,@SERVICE   LOCATE THE SERVDEFS AREA         GP03129 00357403
         L     R15,=V(SUBSERV)  CALL INITIALIZATION ROUTINE     GP03129 00357503
         BASR  R14,R15       CALL IT                            GP03129 00357603
&SRV#NUT SETB  1             USE NEW INTERFACE                  GP03129 00357703
.COMSET  AIF   ('&ERR' EQ '').NOERR                              81148  00357803
&SRVCM@R SETC  ''                                                81148  00357903
         AIF   ('&ERR' EQ 'NO').NOERR                            81148  00358003
&SRVCM@R SETC  '&ERR'                                            81148  00358103
.NOERR   AIF   ('&MAP' EQ 'NO').MEND                                    00358203
         AIF   (&SRVCM@P).MEND                                          00358303
&SRVCM@P SETB  1                                                        00358403
         PUSH  PRINT                                                    00358503
         AIF   ('&LIST' NE 'NO').DOLIST                          81167  00358603
         PRINT OFF                                               81167  00358703
         AGO   .CMLIST                                           81167  00358803
.DOLIST  PRINT ON,GEN                                            81167  00358903
.CMLIST  SPACE 1                                                 81167  00359003
         COPY  SERVFLAG                                                 00359103
         POP   PRINT                                                    00359203
.MEND    MEND  ,                                                        00359303
./ ADD NAME=SERVTERM 0107-99020-06263-0041-00038-00009-00034-GERHARD 00 00359403
         MACRO ,                                                        00359503
&NM    SERVTERM &DELETE=YES                             ADDED ON 81148  00359603
         GBLC  &MACPLAB                                                 00359703
         GBLC  &SRVLMOD(20),&SRVLDEL(20)                        GP03258 00359803
         GBLB  &MVSXA                                           GP04234 00359903
         GBLA  &SRVNMOD                                         GP03258 00360003
.*--------------------------------------------------------------------* 00360103
.*  SERVTERM OPTIONALLY FREES MODULES LOADED BY SERVLOAD (W/EXPLICIT  * 00360203
.*    SECOND NAME).                                                   * 00360303
.*  SERVTERM CALLS @SERVICE TO CLOSE AND FREE KNOWN WORK AREAS AND    * 00360403
.*    MODULES                                                         * 00360503
.*  SERVTERM FREES AND CLEARS THE @SERVICE POINTER                    * 00360603
.*--------------------------------------------------------------------* 00360703
         LCLA  &I,&J                                            GP03258 00360803
         LCLC  &X                                               GP03258 00360903
&X       SETC  '&SYSNDX'                                        GP03258 00361003
&NM      MACPARM R15,15,@SERVICE,OP=ICM,MODE=THREE                      00361103
         BZ    ZZZZ&SYSNDX                                              00361203
         SR    R0,R0                                                    00361303
         AIF   (&MVSXA).BASSM                                   GP04234 00361403
         BALR  R14,R15       CLOSE/FREE                         GP04234 00361503
         AGO   .DELETE                                          GP04234 00361603
.BASSM   BASSM R14,R15       CLOSE/FREE                                 00361703
.DELETE  DELETE EP=@SERVICE                                             00361803
ZZZZ&SYSNDX XC @SERVICE,@SERVICE                                        00361903
         AIF   ('&DELETE' NE 'YES').SKIPDEL                     GP03258 00362003
.DELLOOP AIF   (&I GE &SRVNMOD).SKIPDEL                         GP03258 00362103
&I       SETA  &I+1                                             GP03258 00362203
         AIF   ('&SRVLMOD(&I)' EQ '' OR '&SRVLDEL(&I)' EQ '').DELLOOP   00362303
         MACPARM R15,15,&SRVLMOD(&I),OP=ICM,MODE=THREE          GP03258 00362403
&J       SETA  &J+1                                             GP03258 00362503
         BZ    ZZ&X.D&J                                         GP03258 00362603
&MACPLAB SETC  'ZZ&X.D'.'&J'                                    GP03258 00362703
         DELETE EPLOC==CL8'&SRVLDEL(&I) '                       GP03258 00362803
         XC    &SRVLMOD(&I).(4),&SRVLMOD(&I)                    GP03258 00362903
         AGO   .DELLOOP                                         GP03258 00363003
.SKIPDEL MACPARM MODE=LBL    EXPAND FINAL LABEL                 GP03258 00363103
         MEND  ,                                                        00363203
./ ADD NAME=STORAGE  8021-04308-12043-1629-00072-00018-00000-GERHARD 00 00363303
         MACRO ,                                                        00363403
&NM      STORAGE &FUN,&LENGTH=,&ADDR=,&SP=,&BNDRY=,&LOC=,&COND=,       *00363503
               &CALLRKY=,&RELEASE=                                      00363603
.*                                                                      00363703
.*    BACKWARD COMPATIBILITY FOR MVS 3.8 UNDER HERCULES         GP04234 00363803
.*    ALLOW MOST OPERANDS USING GETMAIN/FREEMAIN                        00363903
.*                                                                      00364003
         LCLA  &K,&RK                                                   00364103
         LCLC  &SB                                              GP08258 00364203
         AIF   ('&SP' EQ '0').NOPOOL  TREAT AS SP=              GP08258 00364303
&SB      SETC  '&SP'                                            GP08258 00364403
.NOPOOL  ANOP  ,                                                GP08258 00364503
&K       SETA  K'&SB                                                    00364603
&RK      SETA  K'&LENGTH                                                00364703
         AIF   ('&FUN' EQ 'OBTAIN').GET                                 00364803
         AIF   ('&FUN' EQ 'RELEASE').FREE                               00364903
         MNOTE 8,'STORAGE: FUNCTION &FUN INVALID'                       00365003
         MEXIT ,                                                        00365103
.GET     AIF   ('&COND' EQ 'YES').GETC                                  00365203
         AIF   ('&BNDRY' NE '' OR '&SB' NE '').GETU                     00365303
&NM      GETMAIN R,A=&ADDR,LV=&LENGTH                                   00365403
         MEXIT ,                                                        00365503
.*                                                                      00365603
.GETC    AIF   ('&SB' EQ '' OR &K LT 3).GETCB                           00365703
         AIF   ('&SB'(1,1) EQ '(' AND '&SB'(2,1) NE '(' AND            *00365803
               '&SB'(&K,1) EQ ')' AND '&SB'(&K-1,1) NE ')').GETCR       00365903
.GETCB   ANOP  ,                                                        00366003
&NM      GETMAIN RC,A=&ADDR,LV=&LENGTH,BNDRY=&BNDRY,SP=&SB              00366103
         MEXIT ,                                                        00366203
.GETCR   ANOP  ,                                                        00366303
&NM      MACPARM R0,&LENGTH                                             00366403
         MACPARM R15,&SB     GET SUBPOOL                        GP08089 00366503
         GETMAIN RC,A=&ADDR,LV=(0),SP=(15),BNDRY=&BNDRY         GP08089 00366603
         MEXIT ,                                                        00366703
.*                                                                      00366803
.GETU    AIF   ('&SB' EQ '' OR &K LT 3).GETUB                           00366903
         AIF   ('&SB'(1,1) EQ '(' AND '&SB'(2,1) NE '(' AND            *00367003
               '&SB'(&K,1) EQ ')' AND '&SB'(&K-1,1) NE ')').GETUR       00367103
.GETUB   ANOP  ,                                                        00367203
&NM      GETMAIN RU,A=&ADDR,LV=&LENGTH,BNDRY=&BNDRY,SP=&SB              00367303
         MEXIT ,                                                        00367403
.GETUR   ANOP  ,                                                        00367503
&NM      MACPARM R0,&LENGTH                                             00367603
         MACPARM R15,&SB     GET SUBPOOL                        GP08089 00367703
         GETMAIN RU,A=&ADDR,LV=(0),SP=(15),BNDRY=&BNDRY         GP08089 00367803
         MEXIT ,                                                        00367903
.*                                                                      00368003
.FREE   AIF   ('&SB' NE '' AND '&LENGTH' EQ '' AND '&ADDR' EQ '').FPOOL 00368103
         AIF   ('&SB' NE '').FREESP                                     00368203
&NM      FREEMAIN R,A=&ADDR,LV=&LENGTH                                  00368303
         MEXIT ,                                                        00368403
.FREESP  AIF   ('&LENGTH' EQ '' OR &RK LT 3).FREESR                     00368503
         AIF   ('&LENGTH'(1,1) EQ '(' AND '&LENGTH'(2,1) NE '(' AND    *00368603
               '&LENGTH'(&RK,1) EQ ')' AND                             *00368703
               '&LENGTH'(&RK-1,1) NE ')').FRUR                          00368803
.FREESR  AIF   ('&SB' EQ '' OR &K LT 3).FREEUB                          00368903
         AIF   ('&SB'(1,1) EQ '(' AND '&SB'(2,1) NE '(' AND            *00369003
               '&SB'(&K,1) EQ ')' AND '&SB'(&K-1,1) NE ')').FRUR        00369103
.FREEUB  ANOP  ,                                                        00369203
&NM      FREEMAIN R,A=&ADDR,LV=&LENGTH,SP=&SB                           00369303
         MEXIT ,                                                        00369403
.FRUR  ANOP  ,                                                          00369503
&NM      MACPARM R0,&LENGTH                                             00369603
         MACPARM R0,8(R13),OP=ST      SAVE LENGTH                       00369703
         MACPARM R0,&SB                                                 00369803
         MACPARM R0,8(R13),OP=STC    COMBINE WITH SUBPOOL               00369903
         MACPARM R0,8(R13),OP=L      AND RELOAD                 GP08251 00370003
         FREEMAIN R,A=&ADDR,LV=(0)  LV=&LENGTH,SP=&SB                   00370103
         MEXIT ,                                                        00370203
.FPOOL   ANOP  ,                                                        00370303
&NM      FREEMAIN R,SP=&SB   FREE ENTIRE SUBPOOL                        00370403
.MEND    MEND  ,                                                        00370503
./ ADD NAME=SWAPR                                                       00370603
         MACRO ,                                                        00370703
&NM      SWAPR &A,&B         EXCHANGE TWO REGISTERS              86197  00370803
&NM      XR    &A,&B                                                    00370903
         XR    &B,&A                                                    00371003
         XR    &A,&B                                                    00371103
         MEND  ,                                                        00371203
./ ADD NAME=SYSPARM  0112-97202-06263-0047-00255-00328-00003-GERHARD 00 00371303
         MACRO ,                                                        00371403
       SYSPARM &DBTEST=YES,&SETS=YES,&LIST=YES,&SHOW=,&PARM=            00371503
.********************************************************************** 00371603
.*   THIS MACRO, FOLLOWING OPTIONGB, SETS GLOBAL ASSEMBLY OPTIONS.      00371703
.*   OVERRIDES ARE MERGED FROM THE CONTENTS OF THE ASSEMBLER EXEC       00371803
.*   PARM SUBFIELD SYSPARM:  // EXEC ASMHC,PARM='SYSPARM(MVS/ESA)'      00371903
.********************************************************************** 00372003
         COPY  OPTIONGB                                                 00372103
         LCLA  &CURSOR                                                  00372203
         LCLA  &I,&J,&K                                                 00372303
         LCLB  &GOTLOC                                                  00372403
         LCLC  &CHAR                                                    00372503
         LCLC  &DEFSP1R,&DEFSP2R,&DEFSP3R,&DEFJES2               90217  00372603
         LCLC  &DELIM                                                   00372703
         LCLC  &TOKEN                                                   00372803
         LCLC  &DEFMOD,&DEFLOC,&DEFMAC,&DEFSOR,&DEFSYM,&DEFSYS   81169  00372903
&GOTLOC  SETB  ('&SETS' EQ 'NO' OR '&LOCAL' NE '' OR &SYSPRM# GT 0)     00373003
         AIF   (&SYSPRM# NE 0).BYEBYE                            81154  00373103
         AIF   ('&PARM' EQ 'IGNORE').NOFRAME                     83100  00373203
         AIF   (T'&PARM EQ 'O').OKPPRM                           83100  00373303
         MNOTE 8,'INVALID PARM=&PARM'                            83100  00373403
.OKPPRM  ANOP  ,                                                 83100  00373503
&K       SETA  K'&SYSPARM                                        82099  00373603
         AIF   (&K LT 2).NOFRAME                                 82099  00373703
         AIF   ('&SYSPARM'(1,1) NE '(').NOFRAME                  82099  00373803
         AIF   ('&SYSPARM'(&K,1) NE ')').NOFRAME                 82099  00373903
&K       SETA  &K-1          SUPPORT FORMAT (A,B,C)              82099  00374003
&CURSOR  SETA  &CURSOR+1                                         82099  00374103
.NOFRAME AIF   (&GOTLOC).FINDTOK                                 82099  00374203
&DEFLOC  SETC  'MVS'           INSTALLATION                      81154  00374303
&DEFMOD  SETC  '370'             DEFAULTS        (360 OR 370)    81154  00374403
&DEFSYS  SETC  'MVS'               HERE          (SYSTEM FLAVOR) 85077  00374503
&DEFSP1R SETC  '0303'                            SP1 RELEASE     85077  00374603
&DEFSP2R SETC  '0200'                            SP2 RELEASE     90252  00374703
&DEFSP3R SETC  '0100'                            SP3 RELEASE     90217  00374803
&DEFJES2 SETC  '41'                              JES2 VERSION    90189  00374903
&DEFMAC  SETC  'GEN'         PRINT OPTION FOR LOCAL MACROS       81154  00375003
&DEFSOR  SETC  'NOGEN'       PRINT OPTION FOR SOURCE CODE        81154  00375103
&DEFSYM  SETC  'NOGEN'       PRINT OPTION FOR SYSTEM MACROS      81154  00375203
&SVCJFCB SETA  0             MODJFCB SVC (SOURCE MEMBER IGC00240)82099  00375303
&SVC@SVC SETA  0             @SERVICE INSTALLED AS SVC ? (255)   84160  00375403
&SVCTMSX SETA  0             UCC-1 (TMS) SVC X                   92271  00375503
&SVCTMSY SETA  0             UCC-1 (TMS) SVC Y                   92271  00375603
.*                                                                      00375703
.FINDTOK AIF   (&CURSOR GE &K).MERGE                             82099  00375803
&CURSOR  SETA  &CURSOR+1                                                00375903
         AIF   ('&SYSPARM'(&CURSOR,1) EQ ' ').FINDTOK                   00376003
         AIF   ('&SYSPARM'(&CURSOR,1) EQ ',').FINDTOK                   00376103
.*                                                                      00376203
&DELIM   SETC  '&SYSPARM'(&CURSOR,1)                                    00376303
&TOKEN   SETC  '&DELIM'                                                 00376403
         AIF   ('&DELIM' EQ '''' OR '&DELIM' EQ '"').CURINC2            00376503
&DELIM   SETC  ''                                                       00376603
&TOKEN   SETC  ''                                                       00376703
.*                                                                      00376803
.SCANTOK AIF   (&CURSOR GT &K).ENDTOK                           82099   00376903
&CHAR    SETC  '&SYSPARM'(&CURSOR,1)                                    00377003
         AIF   ('&DELIM' EQ '&CHAR').CATDEL                             00377103
         AIF   ('&DELIM' EQ '').TESTEND                                 00377203
         AGO   .CATTOK                                                  00377303
.CATDEL  ANOP  ,                                                        00377403
&TOKEN   SETC  '&TOKEN'.'&CHAR'                                         00377503
&CURSOR  SETA  &CURSOR+1                                                00377603
         AIF   (&CURSOR GT &K).GOODTOK                           82099  00377703
&CHAR    SETC  '&SYSPARM'(&CURSOR,1)                                    00377803
         AIF   ('&CHAR' NE '&DELIM').ENDQTOK                            00377903
.TESTEND AIF   ('&CHAR' EQ ' ').GOODTOK                                 00378003
         AIF   ('&CHAR' EQ ',').GOODTOK                                 00378103
.CATTOK  ANOP  ,                                                        00378203
&TOKEN   SETC  '&TOKEN'.'&CHAR'                                         00378303
.CURINC2 ANOP  ,                                                        00378403
&CURSOR  SETA  &CURSOR+1                                                00378503
         AGO   .SCANTOK                                                 00378603
.ENDQTOK AIF   ('&CHAR' EQ ' ' OR '&CHAR' EQ ',').GOODTOK               00378703
         MNOTE 4,'TOKENS RUN TOGETHER - COMMA ASSUMED'                  00378803
         AGO   .GOODTOK                                                 00378903
.ENDTOK  AIF   ('&DELIM' EQ '').GOODTOK                                 00379003
         AIF   ('&TOKEN' NE '').GOODTOK                                 00379103
         MNOTE 8,'UNPAIRED DELIMITER IN &&SYSPARM:'                     00379203
         MNOTE 8,'&SYSPARM'                                             00379303
.GOODTOK AIF   ('&TOKEN' NE 'DEBUG' OR '&DBTEST' EQ 'NO').NOTDB         00379403
&BUGBEAR SETB  1                                                 81331  00379503
         AGO   .FINDTOK                                                 00379603
.NOTDB   AIF   ('&TOKEN' EQ '360' OR '&TOKEN' EQ '370' OR '&TOKEN'     *00379703
               EQ '470' OR '&TOKEN' EQ '390').SETMODL           GP04234 00379803
         AIF   ('&TOKEN' EQ 'MVS' OR '&TOKEN' EQ 'SVS' OR '&TOKEN'     *00379903
               EQ 'VS1' OR '&TOKEN' EQ 'MVT').SETSYS             82137  00380003
         AIF   ('&TOKEN     '(1,6) EQ 'MVS/SP').SETSP            82091  00380103
         AIF   ('&TOKEN     '(1,6) EQ 'MVS/XA').SETXA            82091  00380203
         AIF   ('&TOKEN     '(1,7) EQ 'MVS/ESA').SETESA          90217  00380303
         AIF   ('&TOKEN   '(1,3) EQ 'J2/').SETJES2               85076  00380403
         AIF   (K'&TOKEN NE 6 AND K'&TOKEN NE 8).NOTPROP         82099  00380503
         AIF   ('&TOKEN'(1,1) NE 'P').NOTPROP                    82099  00380603
         AIF   ('&TOKEN'(3,1) NE '/').NOTPROP                    82099  00380703
         AIF   ('&TOKEN'(K'&TOKEN-2,3) NE 'GEN').NOTPROP         82099  00380803
         AIF   ('&TOKEN'(2,1) EQ 'S').PROPSOR                    82099  00380903
         AIF   ('&TOKEN'(2,1) EQ 'M').PROPMAC                    82099  00381003
         AIF   ('&TOKEN'(2,1) EQ 'Y').PROPSYS                    82099  00381103
.NOTPROP AIF   (NOT &GOTLOC).GETLOC                              82099  00381203
         AIF   (&SYSPRM# GE 10).TOOMANY                                 00381303
&SYSPRM# SETA  &SYSPRM#+1                                               00381403
&SYSPRMS(&SYSPRM#) SETC '&TOKEN'                                        00381503
         AGO   .FINDTOK                                                 00381603
.SETMODL ANOP  ,                                                 81154  00381703
&MODEL   SETC  '&TOKEN'                                          81154  00381803
         AGO   .FINDTOK                                          81154  00381903
.*                                                               82099  00382003
.*       PRINT OPTIONS MAY BE PARTIALLY SET WITH THE FORM        82099  00382103
.*       SYSPARM=P?/GEN AND =P?/NOGEN, WHERE ? IS S, M, OR Y     82099  00382203
.PROPSOR ANOP  ,             PS/ - SET SOURCE OPTION             82099  00382303
&PRTSOR  SETC  '&TOKEN'(4,K'&TOKEN-3)                            82099  00382403
         AGO   .FINDTOK                                          82099  00382503
.PROPMAC ANOP  ,             PM/ - SET LOCAL MACRO OPTION        82099  00382603
&PRTMAC  SETC  '&TOKEN'(4,K'&TOKEN-3)                            82099  00382703
         AGO   .FINDTOK                                          82099  00382803
.PROPSYS ANOP  ,             PY/ - SET SYSTEM MACRO OPTION       82099  00382903
&PRTSYS  SETC  '&TOKEN'(4,K'&TOKEN-3)                            82099  00383003
         AGO   .FINDTOK                                          82099  00383103
.SETJES2 ANOP  ,                                                 85076  00383203
&JES2REL SETC  '&TOKEN'(4,K'&TOKEN-3)                            85076  00383303
         AGO   .FINDTOK                                          85076  00383403
.*                                                               85076  00383503
.*                                                               82091  00383603
.SETSP   AIF   ('&TOKEN' EQ 'MVS/SP').SETSYS                     82091  00383703
&SPVEREL SETC  ''                                                82091  00383803
.SETSP1  ANOP  ,                                                 82091  00383903
&I       SETA  6                                                 82091  00384003
.SETSP2  ANOP  ,                                                 82091  00384103
&CHAR    SETC  ''                                                82091  00384203
.SETSP3  ANOP  ,                                                 82091  00384303
&I       SETA  &I+1                                              82091  00384403
         AIF   ('&TOKEN'(&I,1) EQ '.').SPENDL                    82091  00384503
&CHAR    SETC  '&CHAR'.'&TOKEN'(&I,1)                            82091  00384603
         AIF   (&I LT K'&TOKEN).SETSP3                           82091  00384703
.SPENDL  AIF   (K'&CHAR LE 2).SPNERRL                            82091  00384803
         MNOTE 8,'"&CHAR" IN "&TOKEN" IS MORE THAN 2 DIGITS'     82091  00384903
         MNOTE 8,'"&CHAR" WILL BE TRUNCATED ON THE LEFT'         82091  00385003
.SPNERRL ANOP  ,                                                 82091  00385103
&CHAR    SETC  '00'.'&CHAR'                                      82091  00385203
&SPVEREL SETC  '&SPVEREL'.'&CHAR'(K'&CHAR-1,2)                   82091  00385303
         AIF   (&I LT K'&TOKEN).SETSP2                           82091  00385403
&SYSTEM  SETC  'MVS/SP'                                          82091  00385503
         AIF   (K'&SPVEREL GT 2).SPRELS                          82091  00385603
&SPVEREL SETC  '&SPVEREL'.'01'                                   82091  00385703
.SPRELS  AIF   (K'&SPVEREL GT 4).SPLEVS                          82091  00385803
&SPVEREL SETC  '&SPVEREL'.'00'                                   82091  00385903
.SPLEVS  AIF   ('&SPVEREL'(1,2) LE '01').FINDTOK                 82091  00386003
&MVSXA   SETB  1                                                 82091  00386103
         AIF   ('&SPVEREL'(1,2) LE '02').FINDTOK                 90217  00386203
&MVSESA  SETB  1                                                 90217  00386303
         AGO   .FINDTOK                                          82091  00386403
.*                                                               82091  00386503
.SETXA   AIF   ('&TOKEN' EQ 'MVS/XA').SETSYS                     82091  00386603
&SYSTEM  SETC  'MVS/SP'                                          82091  00386703
&SPVEREL SETC  '02'                                              82091  00386803
         AGO   .SETSP1                                           82091  00386903
.*                                                               90217  00387003
.SETESA  AIF   ('&TOKEN' EQ 'MVS/ESA').SETSYS                    90217  00387103
&SYSTEM  SETC  'MVS/SP'                                          90217  00387203
&SPVEREL SETC  '03'                                              90217  00387303
         AGO   .SETSP1                                           90217  00387403
.*                                                               82091  00387503
.SETSYS  ANOP  ,                                                 81154  00387603
&SYSTEM  SETC  '&TOKEN'                                          81154  00387703
         AGO   .FINDTOK                                          81154  00387803
.GETLOC  AIF   ('&TOKEN' EQ 'TSM').OPTTSM                               00387903
&DEFSYS  SETC  'MVS'         DEFAULT SYSTEM                      94217  00388003
&DEFMAC  SETC  'GEN'         LOCAL MACROS                        90031  00388103
&DEFSOR  SETC  'NOGEN'       LOCAL SOURCE                        90031  00388203
&DEFSYM  SETC  'NOGEN'       SYSTEM MACROS                       90031  00388303
&SVCJFCB SETA  0             NO MODJFCB SVC                      90031  00388403
&SVC@SVC SETA  0             @SERVICE NOT INSTALLED AS SVC       90031  00388503
&SVCTMSX SETA  0             UCC-1 TMS ?                         90031  00388603
&SVCTMSY SETA  0             UCC-1 TMS ?                         90031  00388703
         AGO   .OPT370                                           90031  00388803
.*                                                                      00388903
.OPTTSM  ANOP  ,                                                 82099  00389003
&SVCJFCB SETA  240           MODJFCB SVC                         82099  00389103
&SVC@SVC SETA  0             @SERVICE NOT INSTALLED AS SVC       83100  00389203
&SVCTMSX SETA  0             UCC-1 TMS ?                         82099  00389303
&SVCTMSY SETA  0             UCC-1 TMS ?                         82099  00389403
.*                                                                      00389503
.OPTMVS  ANOP  ,                                                        00389603
&DEFSYS  SETC  'MVS'                                                    00389703
         AGO   .OPT370                                           82099  00389803
.*                                                                      00389903
.OPTSVS  ANOP  ,                                                        00390003
&DEFSYS  SETC  'SVS'                                                    00390103
.OPT370  ANOP  ,                                                        00390203
&DEFMOD  SETC  '370'                                                    00390303
.COMLOC  ANOP  ,                                                 81154  00390403
&LOCAL   SETC  '&TOKEN'                                          81154  00390503
&GOTLOC  SETB  1                                                        00390603
         AGO   .FINDTOK                                          81154  00390703
.TOOMANY MNOTE 8,'MORE THAN 10 ELEMENTS IN &&SYSPARM:'                  00390803
         MNOTE 8,'&SYSPARM'                                             00390903
.MERGE   AIF   ('&LOCAL' NE '').MGLOC                            81154  00391003
&LOCAL   SETC  '&DEFLOC'                                         81154  00391103
.MGLOC   AIF   ('&MODEL' NE '').MGMOD                            81154  00391203
&MODEL   SETC  '&DEFMOD'                                         81154  00391303
.MGMOD   AIF   ('&PRTMAC' NE '').MGMAC                           81154  00391403
&PRTMAC  SETC  '&DEFMAC'                                         81154  00391503
.MGMAC   AIF   ('&PRTSOR' NE '').MGSOR                           81154  00391603
&PRTSOR  SETC  '&DEFSOR'                                         81154  00391703
.MGSOR   AIF   ('&PRTSYS' NE '').MGSYM                           81154  00391803
&PRTSYS  SETC  '&DEFSYM'                                         81154  00391903
.MGSYM   AIF   ('&SYSTEM' NE '').MGSYS                           81154  00392003
&SYSTEM  SETC  '&DEFSYS'                                         81154  00392103
.MGSYS   ANOP  ,                                                 81154  00392203
&MVSESA  SETB  (&MVSESA OR '&SYSTEM' EQ 'MVS/ESA')               90217  00392303
&MVSXA   SETB  (&MVSXA OR &MVSESA OR '&SYSTEM' EQ 'MVS/XA')      90217  00392403
&MVSSP   SETB  (&MVSSP OR &MVSXA OR '&SYSTEM' EQ 'MVS/SP')       90217  00392503
         AIF   (NOT &MVSSP OR '&SPVEREL' NE '').MGSP             82091  00392603
&SYSTEM  SETC  'MVS/SP'                                          82091  00392703
&SPVEREL SETC  '01'.'&DEFSP1R'                                   82091  00392803
         AIF   (NOT &MVSXA).MGSP                                 82091  00392903
&SYSTEM  SETC  'MVS/XA'                                          90217  00393003
&SPVEREL SETC  '02'.'&DEFSP2R'                                   82091  00393103
         AIF   (NOT &MVSESA).MGSP                                90217  00393203
&SYSTEM  SETC  'MVS/ESA'                                         90217  00393303
&SPVEREL SETC  '03'.'&DEFSP3R'                                   90217  00393403
.MGSP    AIF   ('&JES2REL' NE '').MGSJ2                          85076  00393503
&JES2REL SETC  '&DEFJES2'                                        85076  00393603
.MGSJ2   ANOP  ,                                                 85076  00393703
&CPU     SETC  '&MODEL'                                                 00393803
&MVS     SETB  ('&SYSTEM'(1,3) EQ 'MVS')                         82091  00393903
&SVS     SETB  ('&SYSTEM' EQ 'SVS')                                     00394003
&VS1     SETB  ('&SYSTEM' EQ 'VS1')                              82137  00394103
.BYEBYE  AIF   ('&LIST' EQ 'NO').MEND                            81154  00394203
         MNOTE *,'                                                  '   00394303
         MNOTE *,'               INSTALLATION &LOCAL                '   00394403
&CHAR    SETC  ''                                                85076  00394503
         AIF   (NOT &MVSSP).PRTVER                               85076  00394603
&CHAR    SETC  'V'.'&SPVEREL'(1,2)                               82091  00394703
         AIF   (K'&SPVEREL LE 3).PRTVER                          82091  00394803
&CHAR    SETC  '&CHAR'.'.R'.'&SPVEREL'(3,2)                      82091  00394903
         AIF   (K'&SPVEREL LE 5).PRTVER                          82091  00395003
&CHAR    SETC  '&CHAR'.'.L'.'&SPVEREL'(5,2)                      82091  00395103
.PRTVER  MNOTE *,'      CPU   &MODEL      SYSTEM &SYSTEM  &CHAR     '   00395203
         AIF   ('&JES2REL' EQ '').NOTJES2                        85076  00395303
         MNOTE *,'      JES2 RELEASE &JES2REL                       '   00395403
.NOTJES2 MNOTE *,'      PRINT SOR &PRTSOR  MAC &PRTMAC  SYS &PRTSYS '   00395503
         MNOTE *,'      SVC:  TMS=&SVCTMSX/&SVCTMSY  JFCB=&SVCJFCB  @SV*00395603
               C=&SVC@SVC '                                      83100  00395703
         MNOTE *,'                                                  '   00395803
         AIF   ('&SHOW' EQ '').IFBUG                                    00395903
         MNOTE *,'      MVS &MVS  MVS/SP &MVSSP  MVS/XA &MVSXA  MVS/ESA*00396003
               &MVSESA'                                                 00396103
.IFBUG   AIF   (NOT &BUGBEAR).MEND                               82099  00396203
         MNOTE *,'**************************************************'   00396303
         MNOTE *,'*                                                *'   00396403
         MNOTE *,'*              DEBUG MODE IN EFFECT              *'   00396503
         MNOTE *,'*                                                *'   00396603
         MNOTE *,'**************************************************'   00396703
.MEND    MEND  ,                                                 81154  00396803
./ ADD NAME=VCON     0100-02242-02242-2026-00074-00074-00000-GERHARD 00 00396903
         MACRO ,                                                        00397003
&NM      VCON  &STR,&END=,&BNDRY=H                      ADDED ON 81155  00397103
         GBLB  &VCON@OP                                                 00397203
         GBLC  &VCON@NM                                                 00397303
         LCLA  &I,&J,&K,&L                                              00397403
         LCLC  &L2                                                      00397503
.********************************************************************** 00397603
.**                                                                  ** 00397703
.**  VCON BUILDS A TEXT MESSAGE BEGINNING WITH A TWO-BYTE LENGTH,    ** 00397803
.**  TWO BYTES OF ZERO, AND TEXT OF THAT LENGTH (WTO / RECFM=V FMT)  ** 00397903
.**                                                                  ** 00398003
.**  USE   VCON  'TEXT'                                              ** 00398103
.**                                                                  ** 00398203
.**  OR    VCON  'TEXT1',END=LABEL                                   ** 00398303
.**        DC     ...ZERO OR MORE STORAGE ITEMS                      ** 00398403
.**  LABEL VCON   *END    TO GENERATE A SINGLE MESSAGE               ** 00398503
.**                                                                  ** 00398603
.********************************************************************** 00398703
&K       SETA  K'&STR                                                   00398803
         AIF   (T'&END NE 'O').TSTOPEN                                  00398903
         AIF   (T'&STR EQ 'O').CLOSE                                    00399003
         AIF   ('&STR'(1,1) EQ '*').CLOSE                               00399103
.TSTOPEN AIF   (&K EQ 0).COMLEN                                         00399203
         AIF   ('&STR'(1,1) NE '''').COMLEN                             00399303
&I       SETA  2                                                        00399403
&J       SETA  &K-2                                                     00399503
&K       SETA  &J                                                       00399603
.LOOP    AIF   ('&STR'(&I,2) EQ '''''').SK2                             00399703
         AIF   ('&STR'(&I,2) EQ '&&').SK2                               00399803
&I       SETA  &I+1                                                     00399903
         AGO   .INC                                                     00400003
.SK2     ANOP  ,                                                        00400103
&I       SETA  &I+2                                                     00400203
&K       SETA  &K-1                                                     00400303
.INC     AIF   (&I LE &J).LOOP                                          00400403
.COMLEN  AIF   (NOT &VCON@OP).NOPEN                                     00400503
         MNOTE 4,'PRIOR VCON NOT TERMINATED'                            00400603
&VCON@OP SETB  0                                                        00400703
.NOPEN   AIF   ('&BNDRY' EQ 'H' OR '&BNDRY' EQ 'Y').NOBOUND             00400803
         AIF   ('&BNDRY' NE 'X' AND '&BNDRY' NE 'C').DOBOUND            00400903
&L2      SETC  'L2'                                                     00401003
         AGO   .NOBOUND                                                 00401103
.DOBOUND DS    0&BNDRY                                                  00401203
.NOBOUND AIF   (T'&END NE 'O').OPEN                                     00401303
         AIF   (&K EQ 0).REQSTR                                         00401403
         AIF   ('&STR'(1,1) EQ '''').QSTR                               00401503
&NM      DC    Y&L2.(&K+4,0),C'&STR'                                    00401603
         AGO   .MEND                                                    00401703
.QSTR    ANOP  ,                                                        00401803
&NM      DC    Y&L2.(&K+4,0),C&STR                                      00401903
         AGO   .MEND                                                    00402003
.OPEN    AIF   (&K NE 0).OPSTR                                          00402103
&NM      DC    Y&L2.(&END-*,0)                                          00402203
         AGO   .SETOPEN                                                 00402303
.OPSTR   AIF   ('&STR'(1,1) EQ '''').OQSTR                              00402403
&NM      DC    Y&L2.(&END-*,0),C'&STR'                                  00402503
         AGO   .SETOPEN                                                 00402603
.OQSTR   ANOP  ,                                                        00402703
&NM      DC    Y&L2.(&END-*,0),C&STR                                    00402803
.SETOPEN ANOP  ,                                                        00402903
&VCON@NM SETC  '&END'                                                   00403003
&VCON@OP SETB  1                                                        00403103
         MEXIT ,                                                        00403203
.REQSTR  MNOTE 4,'TEXT STRING REQUIRED'                                 00403303
         MEXIT ,                                                        00403403
.CLOSE   AIF   (&VCON@OP).WASOPEN                                       00403503
         MNOTE 4,'VCON END OUT OF SEQUENCE'                             00403603
.WASOPEN AIF   ('&NM' EQ '' OR '&NM' EQ '&VCON@NM').BLAB                00403703
&NM      EQU   *                                                        00403803
.BLAB    ANOP  ,                                                        00403903
&VCON@NM EQU   *                                                        00404003
&VCON@NM SETC  ''                                                       00404103
&VCON@OP SETB  0                                                        00404203
.MEND    MEND  ,                                                        00404303
./ ADD NAME=YREGS    8000-12011-12011-0124-00011-00011-00000-GERHARD 00 00404403
         MACRO                                                          00404503
         YREGS ,                                                        00404603
         GBLA  &REGS                                                    00404703
         AIF   (&REGS EQ 1).MEND  ONLY EXPAND ONCE                      00404803
&REGS    SETA  1             MAINTAIN IBM COMPATIBILITY                 00404903
         LCLA  &I                                                       00405003
.LOUPE   AIF   (&I GT 15).MEND                                          00405103
R&I      EQU   &I                                                       00405203
&I       SETA  &I+1                                                     00405303
         AGO   .LOUPE                                                   00405403
.MEND    MEND                                                           00405503
./ ENDUP       "REVIEW" PDS MEMBER OFFLOAD AT 13:17 ON 22-01-25         00405603
/*                                                                      00405700
//*------------------------------------------------------ LOADMACS      00405800
//*                                                                     00405900
//ASM1.SYSIN DD *                                                       00406000
PALL     TITLE 'P R I N T A L L  ***  PRINT OR DUMP ANY DATA SET'       00406102
         PUNCH ' ORDER PRINTALL(P) '  EASIER DUMPS              GP05035 00406202
         SPACE 1                                                        00406302
         COPY  OPTIONGB                                                 00406402
         SPACE 1                                                        00406502
         SYSPARM LIST=YES                                               00406602
         SPACE 2                                                        00406702
*********************************************************************** 00406802
*                                                                     * 00406902
*                                                                     * 00407002
*        COPYRIGHT 1981,1990  EXPERT SYSTEM PROGRAMMING, INC.         * 00407102
*        COPYRIGHT 1998,1999  GERHARD POSTPISCHIL                     * 00407202
*        COPYRIGHT 2002       EXPERT SYSTEM PROGRAMMING               * 00407302
*                        176 OLD STAGE COACH ROAD                     * 00407402
*                        BRADFORD, VT 05033-8844                      * 00407502
*          E-MAIL : GERHARD@POSTPISCHIL.COM                           * 00407602
*                                                                     * 00407702
*                    ALL RIGHTS RESERVED                              * 00407802
*                                                                     * 00407902
*                                                                     * 00408002
*********************************************************************** 00408102
         SPACE 1                                                GP99187 00408202
*   DEPENDENCIES - LOCAL AND IBM MACROS                         GP99187 00408302
*                                                               GP99187 00408402
*   JCL - 64K TO 512K REGION, DEPENDING ON BLOCKSIZE            GP99187 00408502
*   SYSPRINT DD - LISTING FBA/VBA/FBM/VBM, 133/137              GP99187 00408602
*                                                               GP99187 00408702
*   SYSUT1 TO SYSUT99 - INPUT FILES TO BE PROCESSED             GP99187 00408802
*     DCB PARAMETERS ARE REQUIRED FOR THE RECORD OPTIONS.       GP99187 00408902
*     DATASET MAY BE VSAM (PASSWORD REQUIRED IF PROTECTED)      GP99187 00409002
*     IN VSAM MODE, RECORDS ARE TREATED AS BLOCKS UNLESS A RECORD       00409102
*     OPTION IS CHOSEN; KEYS ARE IGNORED (PRINTED AS DATA); RDWS        00409202
*     ARE PART OF THE DATA.                                     GP99187 00409302
*                                                               GP99187 00409402
*   PARM FIELD:   0 TO 3 NUMERIC VALUES, SEPARATED BY BLANKS,   GP99187 00409502
*     COMMAS, OR PROCESSING OPTIONS. EXTRA COMMA INDICATES MISSING      00409602
*     OPERAND FIELD:                                            GP99187 00409702
*                                                               GP99187 00409802
*     1) FIRST NUMERIC IS NUMBER OF RECORDS/BLOCKS TO PRINT     GP99187 00409902
*     2) SECOND IS STARTING RECORD/BLOCK NUMBER                 GP99187 00410002
*     3) THIRD IS ENDING RECORD/BLOCK NUMBER                    GP99187 00410102
*                                                               GP99187 00410202
*   PRINT FORMATTING OPTIONS :                                  GP99187 00410302
*                                                               GP99187 00410402
*     L (DEFAULT) - PLAIN LISTING, 100 CHARACTERS PER LINE      GP99187 00410502
*     H - HEXADECIMAL DUMP, 48 BYTES PER LINE                   GP99187 00410602
*     D - HEXADECIMAL AND EBCDIC (SIDE BY SIDE), 32 BYTES PER LINE      00410702
*     V - VERTICAL DUMP, 100 BYTES PER LINE; TOP LINE SHOWS EBCDIC,     00410802
*         MIDDLE LINE SHOWS ZONES, BOTTOM LINE SHOWS NUMERICS.  GP99187 00410902
*     Y - HORIZONTAL DUMP, 48 BYTES PER LINE; TOP LINE SHOWS EBCDIC,    00411002
*         BOTTOM LINE SHOWS (TWO-BYTE) HEXADECIMAL.             GP99187 00411102
*                                                               GP99187 00411202
*   PROCESSING OPTIONS (DEFAULT IS BLOCK MODE) :                GP99187 00411302
*                                                               GP99187 00411402
*     X - USE EXCP ACCESS METHOD (TAPE ONLY)                    GP99187 00411502
*     K - KEYS ARE TREATED AS DATA -> FORCES BLOCK MODE         GP99187 00411602
*     W - PROCESS RECORDS RATHER THAN BLOCKS; DATA ASSUMED TO BE IN     00411702
*         NIH COMPATIBLE WYLBUR EDIT FORMAT (RECFM=U). OPTION TREATED   00411802
*         AS R FOR RECFM=F, D AND V, AND BLOCK MODE IF U, NOT WYLBUR.   00411902
*     R - PROCESS RECORDS RATHER THAN BLOCKS. VALID FOR RECFM=F, D,     00412002
*         AND V. FOR V FORMAT, SEGMENTS ARE PROCESSED AS RECORDS.       00412102
*     S - TREATED AS R EXCEPT FOR RECFM=VS AND VBS. FOR THESE,  GP99187 00412202
*         SEGMENTS ARE CONSOLIDATED (UP TO 32K) INTO SINGLE RECORDS.    00412302
*     I - WITH R AND S, RDW/SDW ARE PRINTED WITH DATA           GP99187 00412402
*     Q - FORCE ASCII TRANSLATE REGARDLESS OF OPTCD             GP99187 00412502
*     F - CONVERT FROM 9-BIT ASCII FIELD-DATA FORMAT (UNIVAC)   GP99187 00412602
*     A - CONVERTS PRINTABLE OUTPUT TO ASCII, BUT NOT HEX DATA; ALSO    00412702
*         CONVERTS SCAN STRINGS TO ASCII.                       GP99187 00412802
*     - - READ BACKWARDS (SUPPORTED FOR TAPE ONLY)              GP99187 00412902
*                                                               GP99187 00413002
*   SCAN OPTIONS :                                              GP99187 00413102
*                                                               GP99187 00413202
*     OPTIONS ARE SUPPLIED ON SYSIN, ONE REQUEST PER CARD.      GP99187 00413302
*     INPUT IS FREE FORMAT USING COLUMNS 1 TO 72.               GP99187 00413402
*     IF NOT SUPPLIED, ALL RECORDS WITHIN THE START/END RANGE ARE       00413502
*     PRINTED.  IF SUPPLIED, RECORDS ARE PRINTED WHEN ANY SCAN OPT      00413602
*     IS SATISFIED.                                             GP99187 00413702
*                                                               GP99187 00413802
*         FORMATS ARE:                                          GP99187 00413902
*                                                               GP99187 00414002
*         'STRING' (COL,COL)                                    GP99187 00414102
*                                                               GP99187 00414202
*        'STRING' MAY BE 'STRING', "STRING" OR X'STRING'        GP99187 00414302
*        (COL,COL) MAY BE OMITTED (ALL BYTES SCANNED)           GP99187 00414402
*        (COL) REQUESTS A SINGLE COLUMN TEST                    GP99187 00414502
*        (COL,COL) SPECIFIES THE START AND END COLUMNS IN WHICH THE     00414602
*              FIRST BYTE OF THE STRING MAY APPEAR.             GP99187 00414702
*********************************************************************** 00414802
         EJECT ,                                                        00414902
         PRINT &PRTSOR                                                  00415002
PRINTALL PGMHEAD ZERO12,BASE=(R11,R12),PARM=R7,BNDRY=PAGE       GP02338 00415102
         DROP  R13                                               81222  00415202
         LR    R10,R13       SET WORK AREA FOR DCB AND SYNAD EXITS      00415302
         USING SAVE,R10                                          81222  00415402
         L     R8,=A(INITCODE)  GET INITIALIZATION              GP99187 00415502
         BR    R8            AND INVOKE IT                      GP99187 00415602
         SPACE 2                                                 81222  00415702
NEXTDDNM BAS   R9,MAKEDD     INITIALIZE FOR NEW DD               81222  00415802
         SPACE 1                                                        00415902
         RDJFCB MF=(E,OPUT1)   READ JFCB FOR INPUT FILE                 00416002
         BXLE  R15,R15,PARMTEST  OK; MAKE TITLE AND CHECK PARM          00416102
         PRTLIST MSGNOUT1    SET FOR SYSUT1 MISSING              81222  00416202
QUIT8    MVI   CONDCODE+1,8  SET LEVEL 8 ERROR                   83277  00416302
         B     EOJCOM        AND EXIT                            83277  00416402
         SPACE 1                                                        00416502
PARMTEST PRTLIST TITLE,TITLE=1   BUILD TITLE                            00416602
         TM    PARMFG,FPDONE   RECURSION ?                      GP99187 00416702
         BNZ   PARMDONE      YES; SKIP PARM PROCESSING           81222  00416802
         L     R8,=A(INITCODE)  GET INITIALIZATION              GP99187 00416902
         B     INITPARM-INITCODE(,R8)  INVOKE PARM ANALYSIS     GP99187 00417002
PARMDONE MVC   OPTMODE,=C' BLOCK'  SET DEFAULT OF BLOCK MODE            00417102
         TM    FLAGC,FGASC9+FGASCII  FORCED ASCII ?              81253  00417202
         BZ    *+8           NO                                  81253  00417302
         OI    FLAGC,FGTRANS  YES; SET ASCII TRANSLATE ON        81253  00417402
         TM    FLAGS,FGREC   RECORD MODE ?                              00417502
         BZ    *+10          NO                                         00417602
         MVC   OPTMODE,=C'RECORD'                                       00417702
         MVC   SAVSTART(12),START  SAVE COUNTERS                 81222  00417802
         MVC   SAVFLAGS(2),FLAGS       AND FLAGS                GP99187 00417902
         SR    R1,R1                                            GP99187 00418002
         IC    R1,FLAGS      GET FLAG BITS                              00418102
         SRL   R1,5          RETAIN ONLY FORMAT OPTION BITS     GP99187 00418202
         MH    R1,=AL2(OPTFORM2-OPTFORM1)  MAKE OFFSET                  00418302
         LA    R1,OPTFORM1(R1)  POINT TO OPTION LIST                    00418402
         MVC   OPTSUB1(OPTFORM2-OPTFORM1),0(R1)  SET CURRENT OPTIONS    00418502
         L     R1,OPTSUB1    GET TOPT SUBTITLE                          00418602
         PRTLIST (R1),TITLE=3  PRINT TOP                                00418702
         L     R1,OPTSUB2    GET BOTTOM SUBTITLE                        00418802
         PRTLIST (R1),TITLE=4  PRINT BOTTOM                             00418902
         TM    PARMFG,FPDFLT   DEFAULT PARMS ?                          00419002
         BZ    PARMSCON      NO                                         00419102
         PRTV  MSGDFPRM,CC=NO  SHOW DEFAULT PARM FIELD                  00419202
PARMSCON TM    PARMFG,FPERR  PRIOR ERROR ?                              00419302
         BZ    PARMSCOR      NO; NO MESSAGE                             00419402
         BAS   R9,CLRLINE    CLEAR PRINT LINE                   GP99187 00419502
         MVI   LINECC,C'0'                                              00419602
         MVC   LINEHEAD(19),=C'ERROR IN PARM FIELD'                     00419702
         LA    R3,LINETEXT                                              00419802
         L     R9,4(,R13)    GET CALLER'S SAVE AREA              86150  00419902
         L     R9,24(,R9)    GET ORIGINAL R1                     86150  00420002
         L     R9,0(,R9)     LOAD PARM ADDRESS                   86150  00420102
         LA    R5,2(,R9)     PARM ADDRESS                               00420202
         LH    R4,0(,R9)     LENGTH                                     00420302
         BCTR  R4,0                                                     00420402
         EX    R4,EXMVC35    MOVE TEXT                                  00420502
         BAS   R9,PRINT      PRINT THE LINE                     GP99187 00420602
PARMSCOR PRTLIST OPTMSG      PRINT OPTIONS                              00420702
         TM    PARMFG,FPDONE   RECURSION ?                      GP99187 00420802
         BNZ   SCNDONE       YES; SKIP SCAN                      81222  00420902
         L     R8,=A(SCANINIT)  GET SCAN INITIALIZATION          90277  00421002
         BR    R8            INVOKE IT                           90277  00421102
         SPACE 1                                                 90277  00421202
SCNDONE  LA    R8,SYSUT1     SET FOR CONVENIENT ADDRESSING              00421302
         USING IHADCB,R8                                                00421402
         DEVTYPE DCBDDNAM,DB  GET BLOCKSIZE                      90277  00421502
         BXH   R15,R15,OPENBAD                                          00421602
         TM    DB+2,UCB3DACC+UCB3TAPE+UCB3UREC  USABLE DEVICE ?  83277  00421702
         BNM   OPENNUSE      NO; SKIP IT WITH WARNING MESSAGE    83277  00421802
         MVI   RECFM,DCBOPTQ   PRESET TO ALLOW OPTCD=Q                  00421902
         MVI   LRECL,0       SET MASK FOR KEYLEN                 81222  00422002
         MVI   VSPREV,0      SET SEGMENT COMPLETE                81222  00422102
         CLI   DB+2,UCB3TAPE   TAPE DEVICE ?                     81222  00422202
         BNE   OPENNTA       NO                                  81222  00422302
         OI    RECFM,DCBOPTZ   YES; SET REDUCED ERROR RECOVERY   81222  00422402
         TM    FLAGS,FGEXCP  USER SELECTED EXCP ?                89365  00422502
         BZ    OPENNDA       NO                                  89365  00422602
         MVC   SYSUT1(PATEXCPL),PATEXCP  MOVE EXCP DCB           89365  00422702
         MVC   DCBDDNAM,CURRDDNM  UPDATE DDNAME                  90277  00422802
         LA    R1,EXITLIST                                       89365  00422902
         STCM  R1,7,DCBEXLST+1                                   90277  00423002
         MVI   READCCW,2     SET TAPE READ CODE                  89365  00423102
         L     R1,=A(65535)  GET MAXIMUM READ SIZE               89365  00423202
         ST    R1,READCCW+4  MAKE REST OF CCW                    89365  00423302
         OI    READCCW+4,X'20'  SILLY BIT                        89365  00423402
         XC    READIOB(IOBEXTEN-IOBSTDRD),READIOB  CLEAR         89365  00423502
         LA    R2,READIOB                                        89365  00423602
         USING IOBSTDRD,R2   TEMPORARY DECLARATION               89365  00423702
         LA    R1,READECB    GET ECB ADDRESS                     89365  00423802
         ST    R1,IOBECBPT   STASH POINTER                       89365  00423902
         LA    R1,READCCW    GET CCW                             89365  00424002
         ST    R1,IOBSTART   SET NORMAL START                    89365  00424102
         ST    R1,IOBRESTR   AND RESTART                         89365  00424202
         MVI   IOBINCAM+L'IOBINCAM-1,1  SET BLOCK COUNT = 1      89365  00424302
         ST    R8,IOBDCBPT   DCB POINTER                         90277  00424402
         MVI   IOBFLAG1,IOBUNREL  SET UNRELATED                  89365  00424502
         DROP  R2                                                89365  00424602
         B     OPENEXCP      SKIP AROUND DISK STUFF              89365  00424702
OPENNUSE PRTLIST MSGBDDVC    UNUSABLE INPUT DEVICE               83277  00424802
         OI    CONDCODE+1,4  SET WARNING                         83277  00424902
         B     EOJCOM        TRY FOR ANOTHER REQUEST             83277  00425002
OPENNTA  NI    FLAGC,255-FGBACK  RDBACK ONLY ON TAPE            GP99187 00425102
         OI    JFCBTSDM,JFCNWRIT   KEEP JOBQUEUE CHASTE                 00425202
         CLI   DB+2,UCB3DACC  DASD ?                             81222  00425302
         BNE   OPENNDA       NO                                         00425402
         MVI   LRECL,X'FF'   PERMIT KEYS                         81222  00425502
         SERVCALL DSDJ1,INFMJFCB   GET DSCB 1 FOR DATASET        81222  00425602
         BXLE  R15,R15,OPENDSCB  OK                              81222  00425702
         CLI   JFCBDSNM,X'04'  VTOC ?                            81222  00425802
         BNE   OPENNDA       NO; OPEN WILL FAIL                  81222  00425902
OPENDSCB CLI   JFCBDSNM,X'04'   VTOC ?                           81223  00426002
         BNE   OPENNVTO      NO                                  81222  00426102
         MVI   DCBKEYLE,44   SET KEY LENGTH                      90277  00426202
         MVI   RECFM,0       KILL OPTCD                          81222  00426302
         LA    R0,96         SET DATA LENGTH                     81222  00426402
         STH   R0,DCBLRECL    INTO RECORD LENGTH                 90277  00426502
         LA    R0,44+96      SET KEY+DATA LENGTH                 81222  00426602
         STH   R0,DCBBLKSI                                       90277  00426702
         STH   R0,BLKSIZE    ALSO SAVE BLOCKSIZE                 81222  00426802
         B     OPENQIRK      GO TO OVERRIDE                      81222  00426902
RDBACK   OPEN  (OPENNVTO,(RDBACK)),MF=L                         GP99187 00427002
RDAHED   OPEN  (OPENNVTO,(INPUT,REREAD)),MF=L                   GP99187 00427102
         USING DS1FMTID,R1   DECLARE FORMAT 1 DSCB MAPPING       81223  00427202
OPENNVTO TM    DS1DSORG+1,X'08'   VSAM ?                         90277  00427302
         BZ    OPENNASM      NO                                  90277  00427402
         MVI   PFLAG,FGVSAM  SET VSAM MODE                       90277  00427502
         MVC   UT1ACB(PATACBLN),PATACB  REFRESH THE ACB          90277  00427602
         MVC   DCBDDNAM,CURRDDNM  RESTORE DDN                    90277  00427702
         OPEN  MF=(E,OPUT1)   OPEN ACB FOR INPUT                 90277  00427802
         TM    DCBOFLGS,DCBOFOPN  OPEN ?                         90277  00427902
         BZ    OPENBAD       NO                                  90277  00428002
         GENCB BLK=RPL,ACB=(S,UT1ACB),AM=VSAM,                         *00428102
               OPTCD=(KEY,SEQ,ASY,KGE,LOC),                            *00428202
               AREA=(S,UT1RECAD),AREALEN=4,ARG=(S,UT1RRN),             *00428302
               MSGAREA=(S,LINETEXT),MSGLEN=132,MF=(G,UT1GNCB,UT1GNCBL)  00428402
         BXH   R15,R15,OPENBAD  FAILED                           90277  00428502
         XC    UT1RRN,UT1RRN   CLEAR, JUST IN CASE               90277  00428602
         LR    R15,R1        SWAP ADDRESS                        90277  00428702
         STM   R15,R0,BLOCKADE  SAVE RPL ADDRESS/LENGTH          90277  00428802
         MVI   RECFM,0       NO RECFM                            90277  00428902
         MVC   BLKSIZE,=H'32767'  SET MAXIMUM BLOCKSIZE          90277  00429002
         SHOWCB ACB=(S,SYSUT1),AREA=(S,DB),LENGTH=8,FIELDS=KEYLEN,     *00429102
               MF=(G,UT1SHCB,UT1SHCBL)                           90351  00429202
         BXH   R15,R15,OPENOK2   MAY FAIL LATER ?                90277  00429302
         ICM   R15,15,DB     TEST KEY LENGTH                     90277  00429402
         BNZ   OPENOK2       PROCEED                             90277  00429502
         L     R2,BLOCKADE   RELOAD RPL ADDRESS                  90277  00429602
         MODCB RPL=(R2),OPTCD=(ADR,SEQ,ASY,NUP,LOC), NO KEY           R*00429702
               MF=(G,UT1MDCB,UT1MDCBL)                           90351  00429802
         B     OPENOK2       START PROCESSING                    90277  00429902
OPENNASM TM    DS1DSORG,255-JFCORGPS-JFCORGPO-JFCORGU  FUNNY ?   81222  00430002
         BNZ   OPENNOPQ      YES; KILL OPTCD=Q                          00430102
         CLI   DS1DSORG+1,0                                             00430202
         BE    OPENQOK       LEAVE IT SET TO Q                          00430302
OPENNOPQ MVI   RECFM,0       KILL OPTCD                                 00430402
OPENQOK  TM    DS1DSORG,JFCORGPO  PARTITIONED ?                         00430502
         BZ    OPENKEYT      NO; SEE IF KEYED                    81223  00430602
         CLI   JFCBELNM,C' '   MEMBER SPECIFIED ?                       00430702
         BH    OPENKEYT      YES; SEE IF KEYED                   81223  00430802
         MVI   DCBKEYLE,8    SET KEY LENGTH                      90277  00430902
         OI    PFLAG,FGPDS   INDICATE PDS PROCESSING            GP99187 00431002
         MVI   DCBRECFM,DCBRECKL+DCBRECU  SET KEYLE+UNDEF       GP99187 00431102
         NI    FLAGS,255-FGSEG  RESET SEGMENTATION PROCESSING   GP99187 00431202
         B     OPENKEYT      DEBLOCK DIRECTORY WHEN FEASIBLE    GP99187 00431302
OPENQIRK NI    FLAGS,255-FGREC-FGSEG  NO DEBLOCKING FOR DIRECTORY       00431402
         MVI   DCBRECFM,DCBRECKL+DCBRECU  KEYLE/UNDEF            90277  00431502
         MVC   OPTMODE,=C' BLOCK'  SET TO BLOCK MODE                    00431602
         AIF   (NOT &MVS).NOTMVSO                                83026  00431702
OPENKEYT DS    0H            FOR MVS, QSAM GETS MAXIMUM RATHER THAN     00431802
OPENNDA  DS    0H            TRUE BLOCKSIZES, HENCE BSAM IS FORCED.     00431902
OPENBSAM MVI   DCBMACR1,DCBMRRD  FORCE BSAM READ MODE            90277  00432002
         DROP  R1                                                83026  00432102
OPENEXCP L     R0,DB+4       GET DEVICE CAPACITY                 89365  00432202
         AGO   .NOTMVSP                                          83026  00432302
.NOTMVSO B     OPENBSAM      USE BSAM TO GET KEYS                83026  00432402
OPENKEYT CLI   DS1KEYL,0     ANY KEYS ?                          81223  00432502
         BNE   OPENBSAM      YES; USE BSAM TO GET KEYS           81223  00432602
         CLI   JFCKEYLE,0    DOES USER WANT TO FORCE KEYS ?      81223  00432702
         BE    OPENNDA       NO; USE QSAM                        81223  00432802
OPENBSAM MVI   DCBMACR1,DCBMRRD  SET BSAM READ MODE              90277  00432902
         DROP  R1                                                       00433002
OPENEXCP DS    0H                                                89365  00433102
OPENNDA  L     R0,DB+4       GET DEVICE CAPACITY                        00433202
.NOTMVSP LH    R1,=H'32760'  SET FOR VS OPEN LIMIT               83026  00433302
         CR    R0,R1         EXCEEDS SYSTEM MAX ?                       00433402
         BNH   *+6           NO                                         00433502
         LR    R0,R1         TRUNCATE TO MAX                            00433602
         STH   R0,BLKSIZE    TEMPORARY SAVE FOR OPEN EXIT               00433702
         MVC   OPUT1(1),RDAHED  MAKE OPEN OPTION BYTE           GP99187 00433802
         TM    FLAGC,FGBACK  READ BACKWARDS?                    GP99187 00433902
         BZ    *+10          NO                                 GP99187 00434002
         MVC   OPUT1(1),RDBACK  COPY OPEN OPTION BYTE           GP99187 00434102
         OPEN  MF=(E,OPUT1),TYPE=J   OPEN INPUT FILE            GP02338 00434202
         TM    DCBOFLGS,DCBOFOPN   OPENED OK ?                          00434302
         BNZ   OPENOK        YES                                        00434402
OPENBAD  PRTLIST MSGBDOPN    EXPLAIN ABOUT BAD OPEN              81222  00434502
         B     QUIT8         QUIT WITH ERROR CODE 8              83277  00434602
OPENOK   SR    R3,R3         SET BSAM BUFFER SIZE (NONE)         81222  00434702
         TM    DCBMACF1,DCBMRRD   USING BSAM ?                   81222  00434802
         BZ    OPENNBSM      NO                                  81222  00434902
         LH    R3,BLKSIZE    GET KEY LENGTH+BLOCKSIZE            81222  00435002
         MVC   DECB(LENDECB),PATDECB  COMPLETE DECB FOR BSAM     81222  00435102
OPENNBSM SR    R2,R2         CLEAR RECORD INTERFACE AREA         81222  00435202
         TM    RECFM,DCBRECU   F, U, OR V ?                      81253  00435302
         BZ    *+8           NO                                  81253  00435402
         NI    RECFM,255-DCBRECTO  YES; RESET TRACK OVERFLOW     81253  00435502
         TM    RECFM,DCBRECLA-DCBRECV  ANY NON-V FORM ?          81253  00435602
         BNZ   OPENGETM      YES; NOT V                          81222  00435702
         TM    RECFM,DCBRECV+DCBRECSB  VARIABLE SPANNED ?        81222  00435802
         BNO   OPENGETM      NO                                  81222  00435902
         TM    FLAGS,FGSEG   DEBLOCK SPANNED OPTION ?            81222  00436002
         BZ    OPENGETM      NO                                  81222  00436102
         ICM   R2,3,LRECL    GET RECORD AREA SIZE                81222  00436202
         BNZ   OPENGETS      OK IF NOT ZERO                      81222  00436302
         LH    R2,DCBBLKSI   ELSE USE BLOCKSIZE AS DEFAULT       81222  00436402
         B     OPENGETS      RUN WITH SEGMENTS                   81222  00436502
OPENGETM NI    FLAGS,255-FGSEG  RESET SPANNED OPTION             81222  00436602
OPENGETS TM    DCBMACF1,DCBMRECP  EXCP MODE ?                    89365  00436702
         BZ    OPENGETX      NO                                  89365  00436802
         L     R2,=A(65535)  SET FOR MAXIMUM BLOCK               89365  00436902
OPENGETX ST    R2,RECADE+4   SAVE RECORD AREA SIZE               89365  00437002
         AR    R2,R3         GET TOTAL SIZE REQUIRED             81222  00437102
         BNP   OPENOK2       NONE                                81222  00437202
         LA    R2,4(,R2)     INSURANCE FOR XC                    83026  00437302
         GETMAIN R,LV=(R2)   GET STORAGE                         81222  00437402
         STM   R1,R2,BLOCKADE  SAVE ADDRESS AND LENGTH           81222  00437502
         AR    R1,R3         GET START OF RECORD INTERFACE AREA  81222  00437602
         ST    R1,RECADE     SAVE START                          81222  00437702
         XC    0(4,R1),0(R1)  CLEAR NEW RDW                      81222  00437802
OPENOK2  NI    RECFM,DCBRECLA   CLEAR USER'S RECORD FORMAT       81253  00437902
         CLI   RECFM,DCBRECU   U FORMAT ?                        81253  00438002
         BNE   *+8           NO                                  81253  00438102
         MVI   RECFM,0       CLEAR IT COMPLETELY                        00438202
         BAS   R9,CLRLINE    CLEAR PRINT LINE                   GP99187 00438302
         DROP  R8            FINISHED WITH DCB MAPPING                  00438402
         SPACE 2                                                 90277  00438502
READ     MVC   BLOCKMTX,BLOCKMTX-1   CLEAR MESSAGES              81222  00438602
         XC    BLOCKHED,BLOCKHED  RESET BLOCK MESSAGE                   00438702
         TM    PFLAG,FGVSAM  VSAM MODE ?                         90277  00438802
         BNZ   READVSAM      YES                                 90277  00438902
         TM    DCBMACF1-IHADCB+SYSUT1,DCBMRECP  EXCP MODE ?      90277  00439002
         BZ    READBSAM      NO                                  89365  00439102
         L     R5,BLOCKADE   GET BUFFER ADDRESS                  89365  00439202
         STCM  R5,7,READCCW+1  STASH READ ADDRESS                89365  00439302
         LA    R2,READIOB                                        89365  00439402
         USING IOBSTDRD,R2   DECLARE IT                          89365  00439502
         XC    IOBSENS0(2),IOBSENS0  CLEAR IOB                   89365  00439602
         XC    IOBCSW,IOBCSW FOR DEBUGGING                       89365  00439702
         ST    R2,DCBIOBAD-IHADCB+SYSUT1                         89365  00439802
         EXCP  READIOB       SCHEDULE THE READ                   89365  00439902
         WAIT  ECB=READECB   WAIT FOR COMPLETION                 89365  00440002
         SR    R6,R6                                             89365  00440102
         ICM   R6,3,READCCW+6  GET READ LENGTH                   89365  00440202
         SR    R0,R0                                             89365  00440302
         ICM   R0,3,IOBCSW+5  GET UNREAD LENGTH                  89365  00440402
         SR    R6,R0         GET TRUE LENGTH READ                89365  00440502
         BNM   *+6                                               89365  00440602
         SR    R6,R6                                             89365  00440702
         STH   R6,DCBLRECL-IHADCB+SYSUT1   SET INTO FAKE POSITION       00440802
         CLI   READECB,X'7F' GOOD COMPLETION ?                   89365  00440902
         BE    READCOMM      JOIN COMMON                         89365  00441002
         CLI   READECB,X'41'   END-FILE ?                        89365  00441102
         BNE   SYNADX        NO; DIAGNOSE ERROR                  89365  00441202
         TM    IOBCSW+3,1    END-FILE (UNIT EXCEPTION) HIT ?     89365  00441302
         BNZ   EOJEOF        YES; TAKE END-FILE                  89365  00441402
         B     SYNADX        NO; PROCESS SYNAD                   89365  00441502
         DROP  R2                                                89365  00441602
READBSAM TM    DCBMACF1-IHADCB+SYSUT1,DCBMRRD    BSAM MODE ?     90277  00441702
         BZ    READQSAM      NO; GET A BLOCK                     81222  00441802
         L     R5,BLOCKADE   GET THE BUFFER ADDRESS              81222  00441902
         READ  DECB,SF,SYSUT1,(R5),MF=E   READ A BLOCK           81222  00442002
         CHECK DECB          WAIT FOR COMPLETION                 81222  00442102
         B     READCOMM      JOIN COMMON CODE                    81222  00442202
         SPACE 1                                                 90277  00442302
READVSAM L     R2,BLOCKADE   GET RPL ADDRESS                     90277  00442402
         USING IFGRPL,R2     DECLARE IT                          90277  00442502
         XC    LINETEXT(4),LINETEXT  CLEAR VSAM SYNAD MESSAGE    90277  00442602
         GET   RPL=(R2)      GET A RECORD                        90277  00442702
         BXH   R15,R15,VSERROR                                   90277  00442802
         CHECK RPL=(R2)      TAKE EXITS                          90277  00442902
         BXH   R15,R15,VSERROR                                   90277  00443002
         L     R6,RPLRLEN-IFGRPL(,R2)  GET RECORD LENGTH         90277  00443102
         L     R5,UT1RECAD   GET RECORD ADDRESS                  90277  00443202
         CH    R6,=H'14'     IS THIS AN SMF EOF RECORD ?         92279  00443302
         BNE   READCOML      NO                                  92279  00443402
         CLC   =C'SMFEOFMARK',4(R5)  LOGICAL EOF ?               92279  00443502
         BE    EOJEOFN       YES; QUIT NOW                       92279  00443602
         B     READCOML      JOIN COMMON                         90277  00443702
         SPACE 1                                                 90277  00443802
VSERROR  PRTLIST BADVSAM                                         90277  00443902
         INC   NUMERR        COUNT ERROR                         90277  00444002
         OI    CONDCODE+L'CONDCODE-1,8  SET ERROR                90277  00444102
         ICM   R0,3,LINETEXT  WAS A SYNAD MESSAGE WRITTEN ?      90277  00444202
         BZ    EOJCOM        NO                                  90277  00444302
         PRTLIST LNTXT27     WRITE THE ERROR MESSAGE            GP02338 00444402
         B     EOJCOM        AND QUIT                            90277  00444502
LNTXT27  FD    LINETEXT+27,101,NL  ERROR MESSAGE                GP02338 00444602
         FD    *END                                             GP02338 00444702
BADVSAM  FD    '***** VSAM ERROR AFTER BLOCK',NL                 90277  00444802
         FD    NUMBLOCK,I,PADL                                   90277  00444902
         FD    '; RC='                                           90277  00445002
         FD    RPLRTNCD,I                                        90277  00445102
         FD    ', FTNCD='                                        90277  00445202
         FD    RPLFDB2,I                                         90277  00445302
         FD    ', FDBK='                                         90277  00445402
         FD    RPLERRCD,I                                        90277  00445502
         FD    '('                                               90277  00445602
         FD    RPLERRCD,HEX                                      90277  00445702
         FD    ') *****'                                         90277  00445802
         FD    *END                                              90277  00445902
         DROP  R2                                                90277  00446002
         SPACE 1                                                 90277  00446102
READQSAM GET   SYSUT1        GET THE NEXT BLOCK                  81222  00446202
         LR    R5,R1         SAVE BLOCK ADDRESS                  81222  00446302
READCOMM SR    R6,R6                                             89365  00446402
         ICM   R6,3,DCBLRECL-IHADCB+SYSUT1   LRECL IS SIZE OF BLOCK     00446502
         SPACE 1                                                        00446602
READCOML LA    R7,1          INCREASE                            81222  00446702
         A     R7,NUMBLOCK                                              00446802
         ST    R7,NUMBLOCK   UPDATE CURRENT BLOCK NUMBER                00446902
         ST    R6,CURLEN     SAVE FOR BLOCK HEADER               81253  00447002
         LA    R7,0(R5,R6)   GET LAST BYTE +                            00447102
         BCTR  R7,0          SET LAST BYTE                              00447202
         TM    FLAGC,FGASC9  FIELD DATA ?                        81253  00447302
         BZ    NOTFIELD      NO                                  81253  00447402
         LTR   R3,R6         COPY LENGTH                         81253  00447502
         BZ    NOTASCII      ZERO LENGTH RECORD ?                81253  00447602
         SR    R2,R2         CLEAR FOR DIVIDE                    81253  00447702
         SLL   R3,3          MULTIPLY BY 8 TO GET BITS           81253  00447802
         D     R2,=F'9'      GET FULL BYTES AFTER CONVERSION     81253  00447902
         LR    R6,R3         SET NEW LENGTH                      81253  00448002
         ST    R6,CURLEN     UPDATE LENGTH FOR BLOCK HEADER      81253  00448102
         LA    R7,0(R5,R6)   SET NEW END ADDRESS+1               81253  00448202
         BCTR  R7,0          SET NEW END ADDRESS                 81253  00448302
         LA    R2,1          SET BX INCREMENT                    81253  00448402
         LR    R3,R7         COPY END ADDRESS                    81253  00448502
         LR    R14,R5        COPY START ADDRESS                  81253  00448602
         LR    R15,R14       COPY START ADDRESS FOR OUTPUT       81253  00448702
FIELDGRP LM    R0,R1,0(R14)    LOAD 8 BYTES UNALIGNED            81253  00448802
         SLDL  R0,1          DELETE HIGH ORDER BIT               81253  00448902
         LA    R4,7          PROCESS SEVEN BYTES                 81253  00449002
FIELDBYT STCM  R0,8,0(R15)   STASH A BYTE                        81253  00449102
         BXH   R15,R2,NOTFIELD  QUIT IF ALL DONE                 81253  00449202
         SLDL  R0,9          POSITION NEXT BYTE                  81253  00449302
         BCT   R4,FIELDBYT   DO NEXT BYTE                        81253  00449402
         MVC   0(1,R15),8(R14)  DO LAST BYTE OF GROUP            81253  00449502
         LA    R14,9(,R14)   POSITION TO NEXT GROUP              81253  00449602
         BXLE  R15,R2,FIELDGRP  DO NEXT GROUP                    81253  00449702
         SPACE 1                                                 81253  00449802
NOTFIELD TM    FLAGC,FGTRANS   ASCII TRANSLATE REQUIRED ?        81253  00449902
         BZ    NOTASCII      NO                                  81253  00450002
         LR    R2,R5         COPY START ADDRESS                  81253  00450102
         LTR   R3,R6         CHECK LENGTH                        81253  00450202
         BNP   NOTASCII      NOTHING TO DO                       81253  00450302
         LA    R0,256        MAXIMUM TRANSLATABLE                81253  00450402
TRTOLOOP LR    R1,R0         SET LENGTH                          81253  00450502
         CR    R1,R3         COMPARE TO RESIDUAL LENGTH          81253  00450602
         BNH   *+6           MORE THAN ONE SEGMENT               81253  00450702
         LR    R1,R3         PARTIAL SEGMENT LENGTH              81253  00450802
         BCTR  R1,0          SET LENGTH FOR EXECUTE              81253  00450902
         EX    R1,TRTOTR     TRANSLATE                           81253  00451002
         AR    R2,R0         NEXT SEGMENT ADDRESS                81253  00451102
         SR    R3,R0         REMAINING LENGTH                    81253  00451202
         BP    TRTOLOOP                                          81253  00451302
         B     NOTASCII      PROCEED WITH PROCESSING             81253  00451402
TRTOTR   TR    0(0,R2),TRANSTAB  TRANSLATE TO EBCDIC             81253  00451502
         SPACE 1                                                 81253  00451602
NOTASCII LR    R14,R5        SAVE BLOCK START ADDRESS            81253  00451702
         SR    R15,R15       CLEAR PRESUMED KEY LENGTH           81222  00451802
         TM    PFLAG,FGVSAM  VSAM MODE ?                         90277  00451902
         BNZ   READNOKY      TREAT KEY AS DATA                   90277  00452002
         TM    DCBMACF1-IHADCB+SYSUT1,DCBMRRD  USING BSAM ?      90277  00452102
         BZ    READNOKY      NO; QSAM DOES NOT MAKE KEY AVAILABLE       00452202
         TM    FLAGS,FGKEY   USER WANTS KEYS AS DATA ?           81222  00452302
         BNZ   READNOKY      YES                                 81222  00452402
         IC    R15,DCBKEYLE-IHADCB+SYSUT1  GET KEY LENGTH        90277  00452502
         AR    R5,R15        ADJUST BLOCK START ADDRESS          81222  00452602
         SR    R6,R15        ADJUST RESIDUAL LENGTH              81222  00452702
READNOKY STM   R14,R15,KEYADE  SAVE FOR BLOCK FORMATTING         81222  00452802
         STM   R5,R7,BLOCKXLE  SET BLOCK BXLE POINTERS           81222  00452902
         LA    R1,BLOCKFD    GET PRT LIST FOR BLOCK HEADER       81222  00453002
         ST    R1,BLOCKHED   INDICATE MESSAGE PRESENT                   00453102
         MVC   BLOCKMER(10),=C'BAD LENGTH'  PRESET                      00453202
         LTR   R6,R6         ZERO LENGTH READ ?                         00453302
         BNP   BLOCKED       VERRRRY INTERRRRESTING...                  00453402
         CH    R6,BLKSIZE    LONGER THAN USER'S DCB SIZE ?              00453502
         BH    BLOCKED                                                  00453602
         TM    FLAGS,FGREC   DEBLOCK OPTION ?                           00453702
         BZ    BLOCKNER      NO; JUST DUMP THE BLOCK                    00453802
         SR    R15,R15       SET BLOCK DESCRIPTOR LENGTH         81253  00453902
         CLI   RECFM,DCBRECTO  D FORMAT ?                        81253  00454002
         BE    VADCOMM       YES                                 81253  00454102
         CLI   RECFM,DCBRECV  CHECK FORMAT                              00454202
         BH    FIXED         FIXED                                      00454302
         BL    TESTWYL       UNDEFINED (OR D) - SEE IF WYLBUR           00454402
         MVC   BLOCKMTX(4),=C'BDW='                                     00454502
         UNPK  BLOCKMTX+4(9),0(5,R5)  SHOW BLOCK HEADER                 00454602
         TR    BLOCKMTX+4(8),TRANNUM                                    00454702
         MVI   BLOCKMTX+12,C' '                                         00454802
         CLM   R6,3,0(R5)    LENGTH=BDW LENGTH ?                        00454902
         BL    BLOCKED       NO; DUMP FULL BLOCK                 81222  00455002
         BE    *+8           SAME                                81222  00455102
         LH    R6,0(,R5)     USE BDW LENGTH                      81222  00455202
         LA    R15,4         SET INITIAL OFFSET                  81253  00455302
VADCOMM  SR    R14,R14                                           81253  00455402
         MVC   BLOCKMER(11),=C'INVALID RDW'                      81253  00455502
         CH    R6,=H'8'      AT LEAST 8 BYTES ?                         00455602
         BL    BLOCKED       NO; ERROR                                  00455702
VARCHECK CR    R15,R6        DONE ?                                     00455802
         BH    BLOCKED       BDW NOT = SUM OF RDW                       00455902
         BE    VARCHEKT      OK                                         00456002
         LA    R1,0(R5,R15)  SPACE TO NEXT RDW                          00456102
         CR    R1,R7         ROOM FOR 2 BYTES ?                         00456202
         BNL   BLOCKED       NO; ERROR                                  00456302
         CLI   RECFM,DCBRECTO  D FORMAT ?                        81253  00456402
         BNE   VADCOMR       NO                                  81253  00456502
         MVC   DB(4),0(R1)   MOVE EBCDIC LENGTH                  81253  00456602
         NC    DB(4),=4C'0'  MASK ZONES                          81253  00456702
         CLC   DB(4),=4C'0'  NUMERIC ZONES ?                     81253  00456802
         BNE   BLOCKED       NO; BAD RDW                         81253  00456902
         CLI   0(R1),C'9'    VALID NUMERIC ?                     81253  00457002
         BH    BLOCKED       NO                                  81253  00457102
         CLI   1(R1),C'9'    VALID NUMERIC ?                     81253  00457202
         BH    BLOCKED       NO                                  81253  00457302
         CLI   2(R1),C'9'    VALID NUMERIC ?                     81253  00457402
         BH    BLOCKED       NO                                  81253  00457502
         CLI   3(R1),C'9'    VALID NUMERIC ?                     81253  00457602
         BH    BLOCKED       NO                                  81253  00457702
         PACK  DB,0(4,R1)    PACK IT                             81253  00457802
         CVB   R14,DB        GET LENGTH                          81253  00457902
         B     VADCOMRD                                          81253  00458002
VADCOMR  ICM   R14,3,0(R1)   GET LENGTH IN THIS RDW              81253  00458102
         BNP   BLOCKED       INVALID RDW                                00458202
VADCOMRD CH    R14,=H'4'                                         81253  00458302
         BL    BLOCKED       ERROR ON SHORT RDW                  81253  00458402
         AR    R15,R14                                                  00458502
         B     VARCHECK                                                 00458602
VARCHEKT CLI   RECFM,DCBRECTO  D FORMAT ?                        81253  00458702
         BE    VARCHEKM      YES; SKIP BDW SKIP                  81253  00458802
         LA    R5,4(,R5)     SKIP BDW                            81253  00458902
VARCHEKM MVC   BLOCKMER,BLOCKMER-1  RESET ERROR MESSAGE          81253  00459002
VARLOOP  CLI   RECFM,DCBRECTO   D FORMAT ?                       81253  00459102
         BNE   VARLOOPV      NO                                  81253  00459202
         PACK  DB,0(4,R5)    GET RECORD LENGTH                   81253  00459302
         CVB   R6,DB                                             81253  00459402
         B     VARLOOPD                                          81253  00459502
VARLOOPV ICM   R6,3,0(R5)    GET LENGTH                          81253  00459602
VARLOOPD STM   R5,R7,BLOCKXLE  UPDATE BXLE POINTERS              81253  00459702
         TM    FLAGS,FGSEG   DOING SEGMENT CONSOLIDATION ?       81222  00459802
         BZ    VARNSPAN      NO; PROCESS INDIVIDUALLY            81222  00459902
         CLI   RECFM,DCBRECTO D FORMAT?                         GP99187 00460002
         BE    VARNSPAN      YES; NO CONTROL BITS               GP99187 00460102
         TM    VSPREV,1      ANY DATA STACKED ?                  81222  00460202
         BZ    VARSOFF       NO                                  81222  00460302
         TM    2(R5),2       ADDITIONAL SEGMENT ?                81222  00460402
         BZ    VARPERR       NO; SEQUENCE ERROR                  81222  00460502
         L     R4,RECADE     GET RECORD AREA                     81222  00460602
         LH    R2,0(,R4)     GET CURRENT LENGTH                  81222  00460702
         LA    R3,0(R2,R6)                                       81222  00460802
         SH    R3,=H'4'      LENGTH AFTER MOVE                   81222  00460902
         C     R3,RECADE+4   WILL IT FIT ?                       81222  00461002
         BH    VARPTRUN      NO; NEED TO TRUNCATE                81222  00461102
         LA    R14,0(R4,R2)  GET DESTINATION ADDRESS             81222  00461202
         LR    R15,R6        COPY LENGTH                         81222  00461302
         SH    R15,=H'4'     AFTER MOVE                          81222  00461402
         LA    R0,4(,R5)     GET TEXT START                      81222  00461502
         LR    R1,R15        COPY LENGTH                         81222  00461602
         MVCL  R14,R0        MOVE TO RECORD AREA                 81222  00461702
         STH   R3,0(,R4)     SET NEW LENGTH                      81222  00461802
         MVC   VSPREV,2(R5)  COPY SPAN CONTROL BITS              81222  00461902
         TM    VSPREV,1      LAST ?                              81222  00462002
         BNZ   VARBLE        NO; DO NEXT SEGMENT                 81222  00462102
         BAS   R9,SPANFORM   FORMAT CONSOLIDATED RECORD          81222  00462202
         B     VARBLE        GET ANOTHER                         81222  00462302
VARPERR  MVC   BLOCKMER(18),=C'SDW SEQUENCE ERROR'               81222  00462402
         B     VARPDUMP                                          81222  00462502
VARPTRUN MVC   BLOCKMER(18),=C'SDW LENGTH > LRECL'               81222  00462602
VARPDUMP BAS   R9,SPANFORM   FORMAT PRIOR RECORD                 81222  00462702
VARSOFF  TM    2(R5),3       SINGLE SEGMENT ?                    81222  00462802
         BZ    VARNSPAN      YES; DO NORMALLY                    81222  00462902
         TM    2(R5),2       OUT OF SEQUENCE ?                   81222  00463002
         BZ    VARSSAVE      NO; JUST SAVE                       81222  00463102
         MVC   BLOCKMER(18),=C'SDW SEQUENCE ERROR'               81222  00463202
         B     VARNSPAN                                          81222  00463302
VARSSAVE L     R4,RECADE     GET RECORD AREA                     81222  00463402
         LA    R2,4          SET CURRENT LENGTH                  81222  00463502
         LA    R3,0(R2,R6)                                       81222  00463602
         SR    R3,R2         LENGTH AFTER MOVE                   81222  00463702
         C     R3,RECADE+4   WILL IT FIT ?                       81222  00463802
         BH    VARSTRUN      NO; NEED TO TRUNCATE                81222  00463902
         LA    R14,0(R4,R2)  GET DESTINATION ADDRESS             81222  00464002
         LR    R15,R6        COPY LENGTH                         81222  00464102
         SR    R15,R2        AFTER MOVE                          81222  00464202
         LA    R0,4(,R5)     GET TEXT START                      81222  00464302
         LR    R1,R15        COPY LENGTH                         81222  00464402
         MVCL  R14,R0        MOVE TO RECORD AREA                 81222  00464502
         STH   R3,0(,R4)     SET NEW LENGTH                      81222  00464602
         MVC   VSPREV,2(R5)  COPY SPAN CONTROL BITS              81222  00464702
         B     VARBLE        GET ANOTHER SEGMENT                 81222  00464802
VARSTRUN MVC   BLOCKMER(18),=C'SDW LENGTH > LRECL'               81222  00464902
         B     VARNSPAN      DO NORMALLY                         81222  00465002
         SPACE 1                                                 81222  00465102
SPANFORM STM   R1,R9,SPANSAVE  SAGE REGISTERS                    81222  00465202
         L     R5,RECADE     GET SPANNED RECORD                  81222  00465302
         LH    R6,0(,R5)     AND CURRENT LENGTH                  81222  00465402
         TM    FLAGS,FGINDW  INCLUDE RDW WITH DATA ?             84165  00465502
         BNZ   VARINCL1      YES; DO NOT INDENT                  84165  00465602
         MVC   WORK(4),0(R5)  MAKE SIMULATED SDW                 81222  00465702
         UNPK  LINERDW+1(9),WORK(5)                              81222  00465802
         TR    LINERDW+1(8),TRANNUM   FORMAT RDW IN HEX          81222  00465902
         MVI   LINERDW,C'('                                      81222  00466002
         MVI   LINERDW+9,C')'                                    81222  00466102
         LA    R5,4(,R5)     SKIP RDW                            81222  00466202
         SH    R6,=H'4'      ADJUST TO TEXT LENGTH               81222  00466302
VARINCL1 BAS   R9,FORMLINE                                       84165  00466402
         L     R5,RECADE     GET RECORD AGAIN                    81222  00466502
         LA    R6,4          SET CURRENT LENGTH                  81222  00466602
         STH   R6,0(,R5)                                         81222  00466702
         MVI   VSPREV,0      RESET SEQUENCE BITS                 81222  00466802
         MVC   BLOCKMER,BLOCKMER-1  RESET ERROR MESSAGE          81222  00466902
         LM    R1,R9,SPANSAVE  RESTORE REGISTERS                 81222  00467002
         BR    R9            RETURN TO CALLER                    81222  00467102
         SPACE 1                                                 81222  00467202
VARNSPAN TM    FLAGS,FGINDW  INCLUDE RDW WITH DATA ?             84165  00467302
         BNZ   VARINCL2      YES; DO NOT INDENT                  84165  00467402
         MVC   WORK(4),0(R5) SAVE RDW                            84165  00467502
         UNPK  LINERDW+1(9),WORK(5)                                     00467602
         TR    LINERDW+1(8),TRANNUM   FORMAT RDW IN HEX                 00467702
         MVI   LINERDW,C'('                                             00467802
         MVI   LINERDW+9,C')'                                           00467902
         LA    R5,4(,R5)     SKIP RDW                                   00468002
         SH    R6,=H'4'      ADJUST TO TEXT LENGTH               81222  00468102
VARINCL2 BAS   R9,FORMLINE                                       84165  00468202
VARBLE   LM    R5,R7,BLOCKXLE                                           00468302
         BXLE  R5,R6,VARLOOP   DO NEXT RECORD                           00468402
         B     READ          ELSE READ ONE                              00468502
         SPACE 1                                                        00468602
FIXED    LH    R2,LRECL      GET USER'S RECORD LENGTH                   00468702
         LTR   R2,R2                                                    00468802
         BNP   BLOCKED       TOO BAD                                    00468902
         LR    R1,R6         GET BLOCK LENGTH                           00469002
         SR    R0,R0                                            GP99187 00469102
         DR    R0,R2         SEE HOW MANY                               00469202
         LTR   R0,R0         REMAINDER ?                                00469302
         BZ    FIXDIVID      NO; PROCEED                         86014  00469402
         CH    R0,=H'7'      POSSIBLE DOUBLE-WORD ROUNDING ?     86014  00469502
         BH    BLOCKED       NO, TREAT AS BLOCK IN ERROR         86014  00469602
         SR    R7,R0         ADJUST END TO SKIP EXTRA            86014  00469702
         B     FIXOOPS       PROCEED, BUT WITH ERROR MSG         86014  00469802
FIXDIVID MVC   BLOCKMER,BLOCKMER-1  RESET ERROR MESSAGE                 00469902
FIXOOPS  LR    R6,R2         SET RECORD LENGTH FOR INCREMENT     86014  00470002
FIXLOOP  BAS   R9,FORMLINE                                      GP99187 00470102
         TM    FLAGC,FGBACK  READING BACKWARDS?                 GP99187 00470202
         BNZ   FIXLOOPB      YES; APPROACH DIFFERENTLY          GP99187 00470302
         BXLE  R5,R6,FIXLOOP                                            00470402
         B     READ                                                     00470502
FIXLOOPB SWAPR R5,R7         SET FIRST AS LOW LIMIT; END AS START       00470602
         SR    R5,R6         STOP AT THE LAST RECORD            GP99187 00470702
         SR    R7,R6         SET FOR BXH                        GP99187 00470802
FIXLOOPR LPR   R6,R6         AND SET INCREMENT                  GP99187 00470902
         BAS   R9,FORMLINE   PROCESS DATA                       GP99187 00471002
         LNR   R6,R6         AND CHANGE INCREMENT TO DECREMENT  GP99187 00471102
         BXH   R5,R6,FIXLOOPR  FOR ALL RECORDS                  GP99187 00471202
         B     READ          DO ANOTHER BLOCK                   GP99187 00471302
         SPACE 1                                                        00471402
TESTWYL  ST    R5,DCMBUFAD   SET BUFFER ADDRESS                         00471502
         MVC   BLOCKMER,BLOCKMER-1  RESET ERROR MESSAGE                 00471602
         MVC   DCMBUFMX,0(R5)  MOVE BDW                                 00471702
         NI    DCMBUFMX,255-X'80'  RESET NIH FLAG                       00471802
         CH    R6,DCMBUFMX   POSSIBLE WYLBUR RECORD ?                   00471902
         BL    BLOCKNWY      NO; CHECK DIRECTORY BLOCK           81222  00472002
         STCM  R6,12,DCMBUFLN  CLEAR OFFSET                             00472102
WYLLOOP  SERVCALL WDCOM,DECOMPRS  TRY TO DECOMPRESS THE RECORD          00472202
         CH    R15,=H'4'                                                00472302
         BH    BLOCKNWY      NOT WYLBUR RECORD                   81222  00472402
         BE    READ          END OF BLOCK                               00472502
         L     R5,DCMRECAD   GET RECORD ADDRESS                         00472602
         LH    R6,DCMRECLN   GET CURRENT LENGTH OF RECORD               00472702
         MVC   LINEWYL#+1(8),0(R5)  MOVE RETAINED PORTION OF LINE       00472802
         MVC   LINEWYL#(1),DCMLINEH  MOVE OVERFLOW BYTE          81253  00472902
         LA    R5,8(,R5)     SKIP LINE NUMBER                    81253  00473002
         SH    R6,=H'8'      ADJUST LENGTH                       81253  00473102
         BAS   R9,FORMLINE   PRINT IT                           GP99187 00473202
         B     WYLLOOP                                                  00473302
         SPACE 1                                                GP99187 00473402
BLOCKNWY TM    PFLAG,FGPDS   DOING A PDS?                       GP99187 00473502
         BZ    BLOCKOFF      NO                                 GP99187 00473602
         TM    FLAGS,FGREC   USER WANTS INDIVIDUAL ENTRIES?     GP99187 00473702
         BZ    BLOCKOFF      NO                                 GP99187 00473802
         CH    R6,=H'256'    REAL DIRECTORY LENGTH ?            GP99187 00473902
         BNE   BLOCKOFF      NO; QUIT NOW                       GP99187 00474002
         CLC   0(2,R5),=H'256'   IS THE LENGTH <= 256 ?         GP02338 00474102
         BH    BLOCKOFF      SHOULD NOT HAPPEN ?                GP02338 00474202
         LR    R8,R6         SAVE ORIGINAL LENGTH               GP99187 00474302
         LA    R6,2          SET FOR LENGTH OF LENGTH           GP99187 00474402
         BAS   R9,FORMLINE   FORMAT LENGTH                      GP99187 00474502
         LH    R7,0(,R5)     LOAD LENGTH                        GP99187 00474602
         LA    R7,0(R5,R7)   POINT TO END OF BLOCK              GP99187 00474702
         BCTR  R7,0          LAST VALID BYTE                    GP99187 00474802
         LA    R5,2(,R5)     SKIP LENGTH                        GP99187 00474902
PDSLOOP  IC    R6,11(,R5)    INSERT FLAG/LENGTH BYTE            GP99187 00475002
         LA    R0,X'1F'      SET MAXIMUM LENGTH MASK            GP99187 00475102
         NR    R6,R0         STRIP ALIAS AND TTRN BITS          GP99187 00475202
         LA    R6,12(R6,R6)  CONVERT TO LENGTH                  GP99187 00475302
         BAS   R9,FORMLINE                                      GP99187 00475402
         BXLE  R5,R6,PDSLOOP  DO ALL                            GP99187 00475502
         L     R5,KEYADE     RELOAD ADDRESS                     GP99187 00475602
         CLC   =X'FFFFFFFFFFFFFFFF0000',0(R5)  END BLOCK ?      GP99187 00475702
         BNE   READ          NO; READ NEXT DIRECTORY BLOCK      GP99187 00475802
         NI    FLAGS,255-FGREC  DUMP REST AS BLOCKS             GP99187 00475902
         B     READ           AND GO TO IT                      GP99187 00476002
         SPACE 1                                                GP99187 00476102
BLOCKOFF NI    FLAGS,255-FGREC-FGSEG   NOT WYLBUR; USE BLOCK MODE       00476202
         MVC   OPTMODE,=C' BLOCK'  SET TO BLOCK MODE             81222  00476302
BLOCKNER CLC   BLOCKSYN,BLOCKSYN-1  SYNAD MESSAGE PRESENT ?             00476402
         BNE   BLOCKED       YES; PRINT WITH MESSAGE                    00476502
         MVC   BLOCKMER,BLOCKMER-1  RESET ERROR MESSAGE                 00476602
         ICM   R0,15,KEYADE+4  ANY KEY?                         GP99187 00476702
         BNZ   BLOCKED         YES; LEAVE MESSAGE               GP99187 00476802
         XC    BLOCKHED,BLOCKHED  ELSE KILL MESSAGE                     00476902
BLOCKED  LM    R5,R7,BLOCKXLE  FOR ENTRY FROM FUNNY TESTS               00477002
         LA    R9,READ       RETURN TO READ AFTER FORMAT                00477102
         SPACE 1                                                        00477202
*        SUBROUTINE TO FORMAT ONE RECORD SEGMENT                        00477302
*        R5 ADDRESS; R6 LENGTH; R9 RETURN ADDRESS                       00477402
*                                                                       00477502
FORMLINE CLC   BLOCKERR,BLOCKERR-1  ERROR ?                      86014  00477602
         BE    FORMLINK      NO                                  86014  00477702
         CLC   ERRBLOCK,NUMBLOCK  ALREADY COUNTED THIS ONE ?     86014  00477802
         BE    FORMLINK      YES; DON'T REPEAT                   86014  00477902
         MVC   ERRBLOCK,NUMBLOCK  SAVE LAST COUNTED ERROR BLOCK  86014  00478002
         OI    CONDCODE+L'CONDCODE-1,4  SET ERROR                86014  00478102
         INC   NUMERR        UP ERROR COUNT                      86014  00478202
FORMLINK STM   R5,R9,FORMSAVE   SAVE REGISTERS                   86014  00478302
         LA    R7,1                                                     00478402
         A     R7,NUMREC     INCREMENT CURRENT RECORD NUMBER            00478502
         ST    R7,NUMREC                                                00478602
         C     R7,START      STARTING RECORD REACHED ?                  00478702
         BL    FORMEXIT      NO; RETURN                                 00478802
         OI    PFLAG,FGFOUND   RECORD FOUND IN USER'S RANGE      81222  00478902
         ICM   R9,15,TABLE   ANY SCAN SELECTION ?                       00479002
         BZ    FORMSELC      NO; DO UNCONDITIONALLY                     00479102
         LH    R9,TABOFFLO   GET FIRST SCAN OFFSET                      00479202
         CR    R9,R6         WILL IT FIT ?                              00479302
         BNL   FORMOMIT      NO; SKIP TESTS AND RECORD                  00479402
         BCTR  R9,0                                                     00479502
TSTNEXT  LA    R9,1(,R9)     GO TO NEXT COLUMN                          00479602
         LA    R14,0(R5,R9)  GET NEXT COMPARE COLUMN                    00479702
         LH    R15,TABOFFHI  GET END OF SCAN                            00479802
         CR    R15,R6        ANY ?                                      00479902
         BL    *+8           YES                                        00480002
         LR    R15,R6                                                   00480102
         BCTR  R15,0         ELSE TRUNCATE TO CURRENT LENGTH            00480202
         SR    R15,R9        NO. OF COLUMNS TO SCAN-1                   00480302
         BM    FORMOMIT      DONE WITH THIS CARD                        00480402
         LA    R0,255        SET COMPARE AND INCREMENT VALUE     82248  00480502
         CR    R15,R0        TOO LONG FOR ONE EXECUTE ?          82248  00480602
         BNH   TSTNXTEX      NO                                  82248  00480702
         TRT   0(256,R14),TRTTAB  TEST ON FIRST CHARACTER        82248  00480802
         BNZ   TSTNXTFD      FOUND; CHECK FOR TABLE MATCH        82248  00480902
         AR    R9,R0         BUMP BY ONE SEGMENT                 82248  00481002
         B     TSTNEXT       AND TRY IT                          82248  00481102
TSTNXTEX EX    R15,SCANTRT   LOOK FOR SIGNIFICANT CHARACTER      82248  00481202
         BZ    FORMOMIT      NONE; NEXT CARD                            00481302
TSTNXTFD LR    R9,R1         GET HIT COLUMN                      82248  00481402
         SR    R9,R5         GET EQUIVALENT OFFSET                      00481502
         L     R14,TABLE     GET TABLE START ADDRESS                    00481602
         USING TABSECT,R14   DECLARE TABLE ENTRY                        00481702
         SR    R2,R2                                            GP99187 00481802
TSTNXTAB CLI   TABLEN,X'FF'  ALL DONE ?                                 00481902
         BE    TSTNEXT       TRY NEXT COLUMN                            00482002
         IC    R2,TABLEN     GET ENTRY LENGTH - 1                       00482102
         SR    R15,R15                                          GP99187 00482202
         ICM   R15,3,TABOFF  GET START COLUMN                           00482302
         CR    R9,R15        IN RANGE ?                                 00482402
         BL    TSTNXCOL      NO; SKIP THIS ITEM                         00482502
         ICM   R15,3,TABOFFE AND END COLUMN                             00482602
         CR    R9,R15        IN RANGE ?                                 00482702
         BH    TSTNXCOL      NO; SKIP                                   00482802
         EX    R2,SCANCLC    DO CLC                                     00482902
         BE    FORMSELC      MATCH - PUT IT OUT                         00483002
TSTNXCOL LA    R14,TABTEXT-TABSECT+1(R14,R2)  BUMP TO NEXT TABLE ENTRY  00483102
         B     TSTNXTAB                                                 00483202
         DROP  R14                                               90277  00483302
         SPACE 1                                                        00483402
FORMSELC MVC   LINERECL,=X'4020202020202120'                     81222  00483502
         CVD   R6,DB                                                    00483602
         ED    LINERECL,DB+4   EDIT RECORD LENGTH                       00483702
         CVD   R7,DB                                                    00483802
         MVC   LINEREC#,=X'4020202020202120'                            00483902
         ED    LINEREC#,DB+4   EDIT RECORD/BLOCK NUMBER                 00484002
         LR    R8,R6         COPY LENGTH                                00484102
         LA    R7,0(R5,R6)                                              00484202
         BCTR  R7,0          SET END ADDRESS FOR BXLE                   00484302
         ICM   R2,15,BLOCKHED  BLOCK MESSAGE PRESENT ?                  00484402
         BZ    FORMTLEN      NO; TEST LENGTH                            00484502
         CLI   KEYADE+7,0    ANY KEY LENGTH ?                    81222  00484602
         BE    FORMNKEY      NO                                  81222  00484702
         MVI   KEYLINE,C' '                                      81222  00484802
         MVC   KEYLINE+1(L'KEYLINE-1),KEYLINE  CLEAR IT          81222  00484902
         MVC   KLKEYD,=C'KEYLEN='                                81222  00485002
         MVC   KLKEYH,=C'KEY='                                   81222  00485102
         LM    R14,R15,KEYADE                                    81222  00485202
         CVD   R15,DB        MAKE LENGTH DECIMAL                 81222  00485302
         MVC   WORK(6),=X'402021204040'  MAKE EDIT MASK + PADDING       00485402
         ED    WORK(4),DB+6  EDIT KEY LENGTH                     81222  00485502
         LA    R1,WORK+1     POINT TO FIRST BYTE                 81222  00485602
         LA    R0,3          MAX OF THREE                        81222  00485702
FORMKEYB CLI   0(R1),C' '    LEADING BLANK ?                     81222  00485802
         BNE   FORMKEYM      NO; MOVE                            81222  00485902
         LA    R1,1(,R1)                                         81222  00486002
         BCT   R0,FORMKEYB                                       81222  00486102
FORMKEYM MVC   KLKEYL,0(R1)  MOVE TO LINE                        81222  00486202
         LA    R0,L'KLKEYT   GET MAX EBCDIC TEXT LENGTH          81222  00486302
         CR    R15,R0        LARGER ?                            81222  00486402
         BNH   *+6           NO                                  81222  00486502
         LR    R15,R0        TRUNCATE IT                         81222  00486602
         BCTR  R15,0         LESS ONE FOR EXECUTE                81222  00486702
         L     R1,=A(TRANEBCD)                                   89365  00486802
         EX    R15,TESTEBCD  SEE IF EBCDIC                       81222  00486902
         BNZ   FORMHKEY      NO; FORMAT IN HEX                   81222  00487002
         EX    R15,TESTKEY   MOVE KEY TEXT                       81222  00487102
         MVI   KLKEYT-1,C''''  PUT FRAMING QUOTE IN              81222  00487202
         LA    R1,KLKEYT+2(R15)  ADDRESS OF TRAILING QUOTE       81222  00487302
         MVI   0(R1),C''''   ADD TRAILING FRAME                  81222  00487402
         B     FORMNKEY      GO TO PRINT BLOCK AND KEY           81222  00487502
TESTEBCD TRT   0(0,R14),0(R1)  SEE IF KEY IS EBCDIC              89365  00487602
TESTKEY  MVC   KLKEYT(0),0(R14)   MOVE KEY TEXT IN EBCDIC        81222  00487702
         SPACE 1                                                 81222  00487802
FORMHKEY LA    R2,1(,R15)    GET LENGTH BACK                     81222  00487902
         LA    R0,(L'KLKEYT+2+1)/9*4  GET NUMBER OF FIELDS*4     81222  00488002
         CR    R2,R0         WILL IT FIT ?                       81222  00488102
         BNH   *+6           YES                                 81222  00488202
         LR    R2,R0         ELSE TRUNCATE                       81222  00488302
         LR    R3,R14        SET TO START OF KEY                 81222  00488402
         LA    R1,KLKEYT-1   POINT TO OUTPUT LINE                81222  00488502
         LA    R0,4          SET CONSTANT                        81222  00488602
         LA    R15,8         SET ANOTHER CONSTANT                81222  00488702
FORMKEXW LA    R14,8*16+4    SET INPUT/OUTPUT LENGTH             81222  00488802
         CR    R2,R0         LESS THAN ONE WORD ?                81222  00488902
         BNL   FORMKEX4      NO                                  81222  00489002
         LR    R14,R2        GET LENGTH                          81222  00489102
         SLL   R14,5         GET LENGTH*2 IN HIGH NYBBLE         81222  00489202
         OR    R14,R2        GET UNPACK MASK                     81222  00489302
FORMKEX4 EX    R14,EXUNPK    UNPACK IT                           81222  00489402
         SRL   R14,4         ISOLATE THE OUTPUT LENGTH           81222  00489502
         BCTR  R14,0         LESS ONE                            81222  00489602
         EX    R14,EXTRHEX   TRANSLATE TO PRINTABLE EBCDIC       81222  00489702
         LA    R14,1(R14,R1) GET LAST BYTE+1                     81222  00489802
         MVI   0(R14),C' '   PUT TRAILING BLANK IN               81222  00489902
         LA    R1,1(R15,R1)  SET NEXT OUTPUT ADDRESS             81222  00490002
         AR    R3,R0         SET NEXT INPUT ADDRESS              81222  00490102
         SR    R2,R0         GET RESIDUAL LENGTH                 81222  00490202
         BP    FORMKEXW      DO NEXT WORD                        81222  00490302
         L     R2,BLOCKHED   RESTORE MESSAGE ADDRESS             81222  00490402
FORMNKEY PRTROOM 3           ROOM FOR THREE LINES ?              81222  00490502
         PRTSPACE 1          DOUBLE-SPACE                        81222  00490602
         PRTLIST (R2)        PRINT THE BLOCK MESSAGE             81222  00490702
         XC    BLOCKHED,BLOCKHED  KILL MESSAGE                          00490802
         B     FORMLOOF      SINGLE SPACE RECORD                 81222  00490902
FORMTLEN TM    PFLAG,FGPRINT  FIRST TIME ?                       81222  00491002
         BZ    FORMTLEF      YES; DOUBLE-SPACE                   81222  00491102
         TM    PFLAG,FGSPC   FORCED SPACE ?                      81222  00491202
         BNZ   FORMTLEF      YES; FORCE SPACE                    81222  00491302
         CH    R6,OPTLEN     MORE THAN ONE LINE ?                81222  00491402
         BNH   FORMLOOF      NO                                  81222  00491502
FORMTLEF PRTROOM 3                                               81222  00491602
         MVI   LINECC,C'0'   DOUBLE-SPACE                               00491702
FORMLOOF OI    PFLAG,FGPRINT   AT LEAST ONE RECORD PRINTED       81222  00491802
         NI    PFLAG,255-FGSPC   RESET FORCED SPACE FLAG         81222  00491902
         CH    R6,OPTLEN     MULTI-LINE PRINT ?                  81222  00492002
         BNH   FORMLOOP      NO                                  81222  00492102
         OI    PFLAG,FGSPC   YES; FORCE SPACE NEXT CALL          81222  00492202
FORMLOOP LTR   R6,R8         ANY LENGTH AT ALL ?                        00492302
         BNP   FORMPUMP      NO; PRINT EMPTY LINE                       00492402
         CLC   LINEHEAD,LINEHEAD+1  EMPTY LINE ?                        00492502
         BNE   FORMNOFF      NO                                         00492602
         LR    R0,R5         GET CURRENT START ADDRESS                  00492702
         S     R0,FORMSAVE     LESS STARTING ADDRESS                    00492802
         ST    R0,OFFSET     SAVE LINE OFFSET                           00492902
         CVD   R0,DB                                                    00493002
         MVC   LINEOFFD,=X'4020202020202120'                            00493102
         ED    LINEOFFD,DB+4                                            00493202
         UNPK  LINEOFFH(5),OFFSET+2(3)                                  00493302
         TR    LINEOFFH(4),TRANNUM   MAKE PRINTABLE                     00493402
         MVI   LINEOFFH+L'LINEOFFH,C' '                                 00493502
FORMNOFF LH    R2,OPTLEN     GET LOGICAL SIZE OF ONE LINE               00493602
         CR    R6,R2         IS IT LESS THAN MAXIMUM ?                  00493702
         BNL   *+6           YES                                        00493802
         LR    R2,R6         ELSE COPY BYTES TO DO IN THIS LINE         00493902
         LR    R6,R2         SET LENGTH PROCESSED                       00494002
         TM    FLAGS,FGVERT  CHECK FORMATTING OPTION                    00494102
         BO    FORMVERT      VERTICAL FORMAT                            00494202
         BZ    FORMTEXT      PLAIN TEXT                                 00494302
         TM    FLAGS,FGDUAL  TEXT + HEX ?                               00494402
         BZ    FORMTHEX      NO; JUST HEX                               00494502
         SPACE 1                                                        00494602
FORMDUAL LA    R3,LINETEXT+L'LINETEXT-2-((L'LINETEXT-2)/3)  TEXT        00494702
         MVI   0(R3),C'*'    FRAME                                      00494802
         LA    R3,1(,R3)                                                00494902
         MVI   LINETEXT+L'LINETEXT-1,C'*'                               00495002
         LR    R15,R2                                                   00495102
         BCTR  R15,0         EXECUTE LENGTH                             00495202
         EX    R15,EXMVC35   MOVE TEXT TO END OF LINE                   00495302
         LA    R1,LINETEXT-6  FIRST WORD                                00495402
         TM    FLAGC,FGASCIT  PRINT-ONLY TRANSLATE ?             89171  00495502
         BZ    FORMDUHX      NO                                  89171  00495602
         EX    R15,EXTRANA   TRANSLATE PRINT ONLY                89171  00495702
         LR    R3,R5         SET HEX FROM ADDRESS                89171  00495802
         B     FORMDUHX      GO TO COMMON HEX FORMATTING                00495902
         SPACE 1                                                        00496002
FORMTHEX TM    FLAGS,FGALT   ALTERNATE (HORIZ. TEXT/HEX?)       GP99187 00496102
         BZ    FORMOHEX      NO; HEX ONLY                       GP99187 00496202
         PRTROOM 3           NEED THREE CONTIGUOUS LINES        GP99187 00496302
         MVI   LINECC,C'0'   DOUBLE-SPACE FIRST                 GP99187 00496402
         LR    R15,R6        COPY LENGTH                        GP99187 00496502
         MVI   LINETEXT-8,C' '  DIAPER DUTY                     GP99187 00496602
         MVC   LINETEXT-7(L'LINETEXT+7),LINETEXT-8  CLEAR       GP99187 00496702
         LA    R3,LINETEXT-7  POINT TO OUTPUT                   GP99187 00496802
         LR    R1,R5         COPY INPUT ADDRESS                 GP99187 00496902
FORMYWRD LA    R0,4          SET BYTE/WORD COUNTER              GP99187 00497002
FORMYLUP MVC   0(1,R3),0(R1)  MOVE ONE CHARACTER                GP99187 00497102
         LA    R3,2(,R3)     DOUBLE-SPACE OUTPUT                GP99187 00497202
         LA    R1,1(,R1)     NEXT INPUT                         GP99187 00497302
         BCT   R15,FORMTWRD  TEST IF WORD BOUNDARY              GP99187 00497402
         BAS   R9,PRINT      PRINT AND RESET THIS LINE          GP99187 00497502
         B     FORMOHEX                                         GP99187 00497602
FORMTWRD BCT   R0,FORMYLUP   DO NEXT BYTE IN THIS WORD          GP99187 00497702
         LA    R3,1(,R3)     ELSE INSERT INTER-WORD SPACING     GP99187 00497802
         B     FORMYWRD      AND RESET                          GP99187 00497902
         SPACE 1                                                GP99187 00498002
FORMOHEX LR    R15,R6        COPY LENGTH                        GP99187 00498102
         BCTR  R15,0         LESS ONE FOR MOVE                          00498202
         LA    R3,WORKLINE   SET TO START OF COPIED LINE                00498302
         EX    R15,EXMVC35   COPY TEXT TO WORK AREA                     00498402
         LA    R1,LINETEXT-7  POINT TO TEXT                             00498502
FORMDUHX LA    R0,4          SET CONSTANT                               00498602
         LA    R15,8         SET ANOTHER CONSTANT                       00498702
FORMHEXW LA    R14,8*16+4    SET INPUT/OUTPUT LENGTH                    00498802
         CR    R2,R0         LESS THAN ONE WORD ?                       00498902
         BNL   FORMHEX4      NO                                         00499002
         LR    R14,R2        GET LENGTH                                 00499102
         SLL   R14,5         GET LENGTH*2 IN HIGH NYBBLE                00499202
         OR    R14,R2        GET UNPACK MASK                            00499302
FORMHEX4 EX    R14,EXUNPK    UNPACK IT                                  00499402
         SRL   R14,4         ISOLATE THE OUTPUT LENGTH                  00499502
         BCTR  R14,0         LESS ONE                                   00499602
         EX    R14,EXTRHEX   TRANSLATE TO PRINTABLE EBCDIC              00499702
         LA    R14,1(R14,R1) GET LAST BYTE+1                            00499802
         MVI   0(R14),C' '   PUT TRAILING BLANK IN                      00499902
         LA    R1,1(R15,R1)  SET NEXT OUTPUT ADDRESS                    00500002
         AR    R3,R0         SET NEXT INPUT ADDRESS                     00500102
         SR    R2,R0         GET RESIDUAL LENGTH                        00500202
         BP    FORMHEXW      DO NEXT WORD                               00500302
         B     FORMPUMP      PRINT AND GET NEXT                         00500402
         SPACE 1                                                        00500502
EXTRHEX  TR    0(0,R1),TRANNUM   TRANSLATE TO PRINTABLE HEX             00500602
EXUNPK   UNPK  0(0,R1),0(0,R3)  UNPACK ONE WORD                         00500702
EXMVC35  MVC   0(0,R3),0(R5)   MOVE DATA TO PRINT LINE                  00500802
EXTRANZ  TR    0(0,R3),TRANZONE  TRANSLATE ZONES                        00500902
EXTRANN  TR    0(0,R3),TRANNUM   TRANSLATE NUMERICS                     00501002
EXTRANA  TR    0(0,R3),TRANSTAB  MAKE ASCII READABLE             89171  00501102
         SPACE 1                                                        00501202
FORMVERT PRTROOM 4           SET FOR FOUR LINES                         00501302
         MVI   LINECC,C'0'   DOUBLE-SPACE FIRST LINE OF THREE           00501402
FORMTEXT BCTR  R2,0          SET FOR EXECUTE                            00501502
         LA    R3,LINETEXT   SET OUTPUT ADDRESS                         00501602
         EX    R2,EXMVC35    MOVE TEXT TO PRINT LINE                    00501702
         TM    FLAGC,FGASCIT  PRINT TRANSL. ONLY ?               89171  00501802
         BZ    *+8           NO                                  89171  00501902
         EX    R2,EXTRANA    YES; MAKE ASCII PRINTABLE           89171  00502002
         TM    FLAGS,FGVERT  VERTICAL FORMAT ?                          00502102
         BNO   FORMPUMP      NO; PRINT AND INCREMENT                    00502202
         BAS   R9,PRINT      PRINT THE LINE                     GP99187 00502302
         EX    R2,EXMVC35    MOVE THE DATA AGAIN                        00502402
         EX    R2,EXTRANZ    TRANSLATE ZONES                            00502502
         BAS   R9,PRINT      PRINT THE ZONES                    GP99187 00502602
         EX    R2,EXMVC35    MOVE THE DATA AGAIN                        00502702
         EX    R2,EXTRANN    TRANSLATE NUMERICS                         00502802
*        B     FORMPUMP      PRINT AND BUMP                             00502902
         SPACE 1                                                        00503002
FORMPUMP BAS   R9,PRINT      PRINT THE LINE                     GP99187 00503102
FORMBUMP SR    R8,R6         GET RESIDUAL LENGTH                        00503202
         BXLE  R5,R6,FORMLOOP  DO ANOTHER LINE                          00503302
FORMOMIT L     R1,LIMIT      GET SCAN LIMIT                             00503402
         BCT   R1,*+8        NOT ZERO                                   00503502
         B     EOJLIM        QUIT                                       00503602
         ST    R1,LIMIT      SET REMAINING LIMIT                        00503702
         L     R1,INTVAL     GET PRINT INTERVAL                         00503802
         A     R1,START                                                 00503902
         ST    R1,START      SET NEXT RECORD TO BE DONE                 00504002
FORMEXIT LM    R5,R9,FORMSAVE  RESTORE THE REGISTERS                    00504102
         BR    R9            AND RETURN                                 00504202
         SPACE 1                                                        00504302
PRINT    PRTV  VLINE         PRINT A LINE                               00504402
CLRLINE  MVI   LINECC,C' '                                              00504502
         MVC   LINE(132),LINECC                                         00504602
         BR    R9            RETURN                                     00504702
         SPACE 1                                                        00504802
EOJLIM   PRTLIST MSGLIMIT    SHOW STOP DUE TO LIMIT                     00504902
         B     EOJCOM                                                   00505002
EOJEOF   TM    FLAGS,FGSEG   SEGMENT CONSOLIDATION ?             81222  00505102
         BZ    EOJEOFN                                           81222  00505202
         TM    VSPREV,1      ANY DATA STACKED ?                  81222  00505302
         BZ    EOJEOFN                                           81222  00505402
         BAS   R9,SPANFORM   PROCESS THE LAST RECORD             81222  00505502
EOJEOFN  PRTLIST MSGEOF      SHOW STOP DUE TO END-FILE           81222  00505602
EOJCOM   PRTLIST MSGBDERR    SEE IF ANY ERRORS                   86014  00505702
EOJCOMCO LH    R9,CONDCODE   SET RETURN CODE                     86014  00505802
         CLOSE (,DISP),MF=(E,OPUT1)   CLOSE SYSUT1               82248  00505902
         LM    R1,R2,BLOCKADE   GET GETMAIN PARAMETERS           81222  00506002
         LTR   R1,R1                                             81222  00506102
         BZ    QUITNFRI      NONE                                81222  00506202
         FREEMAIN R,LV=(R2),A=(R1)  FREE THE STORAGE             81222  00506302
         XC    BLOCKADE(4*4),BLOCKADE  CLEAR BLOCK AND RECORD AREA      00506402
QUITNFRI LA    R1,1                                              81222  00506502
         CLM   R1,7,DCBBUFCA-IHADCB+SYSUT1                       81222  00506602
         BNL   QUITNPUL                                          81222  00506702
         FREEPOOL SYSUT1                                         81222  00506802
QUITNPUL CH    R9,=H'4'      SEVERE ERROR ?                      81222  00506902
         BH    QUITTERM      YES; GET OUT                        81222  00507002
         PRTV  NULLLINE,TITLE=1   RESET TITLE                    81222  00507102
         PRTV  NULLLINE,TITLE=3   RESET SUB-TITLES               81222  00507202
         PRTV  NULLLINE,TITLE=4                                  81222  00507302
QUITNDD  LA    R3,1                                              81222  00507402
         AH    R3,CURRDD#    GET CURRENT DD NUMBER               81222  00507502
         CH    R3,=H'100'    PAST 99 ?                           81222  00507602
         BNL   QUITTERM      YES; QUIT                           81222  00507702
         STH   R3,CURRDD#    STASH BACK                          81222  00507802
         LA    R14,10                                            81222  00507902
         SR    R2,R2                                             81222  00508002
         DR    R2,R14        SEPARATE UNITS/TENS                 81222  00508102
         LA    R2,C'0'(,R2)  MAKE EBCDIC                         81222  00508202
         STC   R2,CURRDDNM+5  MAKE NEW NAME                      81222  00508302
         LTR   R3,R3         LESS THAN 10 ?                      81222  00508402
         BNP   QUITIOD       YES; LOOK FOR TIOT                  81222  00508502
         LA    R3,C'0'(,R3)                                      81222  00508602
         STC   R3,CURRDDNM+5                                     81222  00508702
         STC   R2,CURRDDNM+6                                     81222  00508802
QUITIOD  SERVCALL TIODD,CURRDDNM  SEE IF PRESENT                 81222  00508902
         LTR   R0,R0                                             81222  00509002
         BZ    QUITNDD       NO; LOOK FOR NEXT                   81222  00509102
         B     NEXTDDNM      GET THE JFCB                        81222  00509202
QUITTERM SERVTERM ,          CLOSE, FREE AND DELETE EVERYTHING   81222  00509302
         DROP  R10                                               81222  00509402
         USING SAVE,R13                                          81222  00509502
         PGMEXIT RC=(R9)     RETURN WITH CODE                   GP02338 00509602
         DROP  R13                                               81222  00509702
         USING SAVE,R10                                          81222  00509802
         SPACE 2                                                        00509902
         USING IHADCB,R4     DECLARE DCB                         81222  00510002
INEXIT   LR    R4,R1         PRESERVE DCB ADDRESS                81222  00510102
         LR    R9,R14        SAVE RETURN OVER GETMAIN            81222  00510202
         TM    RECFM,DCBOPTZ REDUCED ERROR RECOVERY ?            87174  00510302
         BZ    *+8           NO; NO CHANGE                       87174  00510402
         OI    DCBOPTCD-IHADCB+SYSUT1,DCBOPTZ  ALSO SET IN DCB   81222  00510502
         NC    DCBOPTCD,RECFM  LEAVE ONLY OPTCD=Q (ASCII)        81222  00510602
         TM    DCBOPTCD,DCBOPTQ   ASCII ?                        81253  00510702
         BZ    INEXNOQ       NO                                  89167  00510802
         TM    JFCBLTYP,JFCBAL  LABEL=AL ?                       89167  00510902
         BNZ   INEXNOQ       YES; AVOID 013 IN IFG0196M          89167  00511002
         NI    DCBOPTCD,255-DCBOPTQ  RESET IT                    81253  00511102
         OI    FLAGC,FGTRANS   USE FASTER LOCAL CODE             81253  00511202
INEXNOQ  NC    DCBKEYLE,LRECL  KEEP KEYLEN FOR DASD ONLY         89167  00511302
         LH    R2,BLKSIZE    LOAD SYSTEM OR DEVICE MAXIMUM              00511402
         MVC   LRECL,DCBLRECL                                           00511502
         MVC   RECFM,DCBRECFM                                           00511602
         LH    R3,DCBBLKSI   GET DATASET SIZE                           00511702
         LTR   R3,R3         IS IT VALID ?                              00511802
         BNZ   *+6           LET'S HOPE SO                              00511902
         LR    R3,R2         ELSE USE DEVICE MAXIMUM                    00512002
         TM    FLAGC,FGASC9  FIELD-DATA OPTION ?                 81253  00512102
         BZ    INEXITN9      NO                                  81253  00512202
         LA    R3,7(,R3)     ROUND                               81253  00512302
         SRA   R3,3          DIVIDE BY 8                         81253  00512402
         MH    R3,=H'9'      MAKE FIELD-DATA LENGTH              81253  00512502
INEXITN9 LA    R1,264        SET MINIMUM FOR DIRECTORIES, ETC.   81253  00512602
         CR    R3,R1         SUFFICES ?                                 00512702
         BNL   *+6           YES                                        00512802
         LR    R3,R1         IF NOT, USE DIRECTORY SIZE                 00512902
         LA    R3,8(,R3)     INCREASE FOR LONG BLOCK CHECKING           00513002
         CR    R3,R2         EXCEEDS CAPACITY OF DEVICE ?               00513102
         BNH   *+6           NO                                         00513202
         LR    R3,R2         ELSE TRUNCATE TO MAXIMUM                   00513302
         STH   R3,DCBBLKSI   SET OUR SIZE INTO THE DCB                  00513402
         SR    R0,R0                                             81222  00513502
         IC    R0,DCBKEYLE   GET KEY LENGTH                      81222  00513602
         AR    R3,R0         ADD IT IN                           81222  00513702
         STH   R3,BLKSIZE    SET FOR FORMATTING                  81222  00513802
         MVI   DCBRECFM,DCBRECU  SET FOR FULL BLOCK PROCESSING          00513902
         TM    DCBMACR1,DCBMRRD+DCBMRECP  BSAM OR EXCP READ ?    89365  00514002
         BNZR  R9            YES; RETURN                         81222  00514102
         OI    DCBOPTCD,DCBOPTC   REQUEST CHAINED SCHEDULING     81222  00514202
         SR    R2,R2         CLEAR RECORD INTERFACE AREA SIZE    81223  00514302
         TM    RECFM,DCBRECU-DCBRECV  ANY RECFM ?                81223  00514402
         BNZ   INEXSIZE      YES; NOT V                          81223  00514502
         TM    RECFM,DCBRECV+DCBRECSB  VARIABLE SPANNED ?        81223  00514602
         BNO   INEXSIZE      NO                                  81223  00514702
         TM    FLAGS,FGSEG   DEBLOCK SPANNED OPTION ?            81223  00514802
         BZ    INEXSIZE      NO                                  81223  00514902
         ICM   R2,3,LRECL    GET RECORD AREA SIZE                81223  00515002
         BNZ   INEXSIZE      OK IF NOT ZERO                      81223  00515102
         LH    R2,DCBBLKSI   ELSE USE BLOCKSIZE AS DEFAULT       81223  00515202
INEXSIZE LA    R2,4095(,R2)                                      81223  00515302
         N     R2,=X'0007FC00'  ROUND TO 4K MULTIPLE             81223  00515402
         AH    R2,=AL2(12*1024)  ALLOW FOR OVERHEAD              81223  00515502
         ST    R2,MAINTAB    SET IN GETMAIN LOWER LIMIT          81223  00515602
         GETMAIN VC,A=DB,LA=MAINTAB,MF=(E,GETMAIN)  GET SOME STORAGE    00515702
         LM    R1,R2,DB      GET SIZE GOTTEN                     81222  00515802
         FREEMAIN R,LV=(R2),A=(R1)  FREE IT                      81222  00515902
         SR    R6,R6                                             81222  00516002
         L     R7,DB+4       GET SIZE                            81222  00516102
         S     R7,MAINTAB    ALLOW FOR OVERHEAD                  81223  00516202
         AH    R7,=AL2(4*1024)  PLUS A LITTLE MORE               81223  00516302
         DR    R6,R3         GET BLOCKS                          81222  00516402
         LTR   R7,R7                                             81222  00516502
         BP    *+8                                               81222  00516602
         LA    R7,1                                              81222  00516702
         LA    R1,20         SET REASONABLE MAX ?                81222  00516802
         CR    R7,R1         REASONABLE ?                        81222  00516902
         BNH   *+6                                               81222  00517002
         LR    R7,R1         SET MAX                             81222  00517102
         STC   R7,DCBBUFNO                                       81222  00517202
         LR    R14,R9        RESTORE RETURN                      81222  00517302
         BR    R14                                                      00517402
         DROP  R4                                                81222  00517502
         SPACE 1                                                        00517602
SYNADX   SYNADAF ACSMETH=EXCP,PARM1=READIOB                      89365  00517702
         LA    R14,READCOMM                                      89365  00517802
         B     SYNADC        GO TO COMMON                        89365  00517902
         SPACE 1                                                 89365  00518002
SYNAD    TM    SYSUT1+DCBMACF1-IHADCB,DCBMRRD  BSAM ?            81222  00518102
         BZ    SYNADQ        NO, QSAM                            81222  00518202
         SYNADAF ACSMETH=BSAM   FORMAT ERROR                     81222  00518302
         B     SYNADC                                            81222  00518402
SYNADQ   SYNADAF ACSMETH=QSAM   FORMAT ERROR                     81222  00518502
SYNADC   MVC   BLOCKSYN,84(R1)   MOVE MESSAGE                    81222  00518602
         SYNADRLS ,          FREE UP                                    00518702
         BR    R14           RETURN                                     00518802
         SPACE 1                                                 90277  00518902
VSSYNAD  MVC   BLOCKSYN(5),=C'SYNAD'                             90277  00519002
         B     VSLERSYN                                          90277  00519102
VSLERAD  MVC   BLOCKSYN(5),=C'LERAD'                             90277  00519202
VSLERSYN MVC   BLOCKSYN+6(12),=C'EXIT ENTERED'                   90277  00519302
         BR    R14                                               90277  00519402
         SPACE 1                                                 81222  00519502
MAKEDD   MVC   SYSUT1(SYSUTLEN),PATUT1   REFRESH DCB             81222  00519602
         MVC   DECOMPRS(LENWYLDC),PATDECMP  REFRESH WYLDCOMP AREA       00519702
         MVC   SYSUT1+DCBDDNAM-IHADCB(8),CURRDDNM                81222  00519802
         LA    R1,SYNAD                                          81222  00519902
         STCM  R1,7,DCBSYNAD+1-IHADCB+SYSUT1                     81222  00520002
         LA    R1,EOJEOF                                         81222  00520102
         STCM  R1,7,DCBEODAD+1-IHADCB+SYSUT1                     81222  00520202
         LA    R1,EXITLIST                                       81222  00520302
         STCM  R1,7,DCBEXLST+1-IHADCB+SYSUT1                     81222  00520402
         MVC   START(12),SAVSTART  RESET COUNTERS                81222  00520502
         XC    NUMBLOCK(FLAGS-NUMBLOCK),NUMBLOCK  CLEAR COUNTERS 81222  00520602
         MVC   FLAGS(2),SAVFLAGS   RESET FLAGS                  GP99187 00520702
         MVI   PFLAG,0       RESET PRINT/SCAN FLAGS              81222  00520802
         NI    FLAGC,255-FGTRANS  RESET TRANSLATION              81253  00520902
         MVC   MAINTAB(8),TABMAIN  SET SIZE RANGE (AVOID 604 IN SVS)    00521002
         BR    R9            RETURN TO CALLER                    81222  00521102
         SPACE 2                                                        00521202
         LTORG ,                                                        00521302
         SPACE 1                                                        00521402
OPTFORM1 DC    A(SUB1TOP,SUB1BOT),AL2(L'LINETEXT),C'L '  LINE FORMAT    00521502
OPTFORM2 DC    A(SUB2TOP,SUB2BOT),AL2(32),C'D ' DUAL HEX + EBCDIC       00521602
OPTFORM3 DC    A(SUB3TOP,SUB3BOT),AL2(48),C'H '  HEX FORMAT             00521702
OPTFORM4 DC    A(SUB4TOP,SUB4BOT),AL2(L'LINETEXT),C'V ' VERT. HEX       00521802
OPTFORM5 DC    A(SUB1TOP,SUB1BOT),AL2(L'LINETEXT),C'? '  AVAIL  GP99187 00521902
OPTFORM6 DC    A(SUB2TOP,SUB2BOT),AL2(32),C'? '          AVAIL  GP99187 00522002
OPTFORM7 DC    A(SUB3TOP,SUB3BOT),AL2(48),C'Y '  HOR. HEX + TEXT        00522102
OPTFORM8 DC    A(SUB4TOP,SUB4BOT),AL2(L'LINETEXT),C'? '  AVAIL  GP99187 00522202
TABMAIN  DC    A(20*1024,512*1024)  GETMAIN SIZES                       00522302
GETMEAN  GETMAIN VC,LA=TABMAIN,MF=L                              81222  00522402
GETMEANL EQU   *-GETMEAN                                         81222  00522502
SCANCLC  CLC   0(0,R1),TABTEXT-TABSECT(R14)                             00522602
SCANTRT  TRT   0(0,R14),TRTTAB  TEST ON FIRST CHARACTER                 00522702
SCNWKMVC MVC   WORKLINE+1(0),TABTEXT-TABSECT(R4)  MOVE HEX TEXT         00522802
SSCOL    DC    C'....:....'                                             00522902
SSNUM    DC    C'1234567890'                                            00523002
SSHEX    DC    C'0 1 2 3  4 5 6 7  8 9 A B  C D E F  '                  00523102
         SPACE 1                                                        00523202
SYSIN    INPWORK SYSIN,WIDTH=80,EODAD=SCANEOJ,FILL=C' '          81222  00523302
PATUT1   DCB   DDNAME=SYSUT1,DSORG=PS,MACRF=(GL),SYNAD=SYNAD,          X00523402
               EROPT=ACC,EODAD=EOJEOF,EXLST=123                  81222  00523502
PATUT1LN EQU   *-PATUT1                                          89365  00523602
         READ  PATDECB,SF,1,3,'S',MF=L   BSAM MODE DECB PATTERN  81222  00523702
SYSPRINT PRTWORK SYSPRINT,SYSOUT,TITLE=5,FILL=C'.'   OUTPUT DATASET     00523802
PATDECMP SERVCOMP DSECT=NO,PFX=PDC                               81222  00523902
         ORG   PDCFG1                                                   00524002
         DC    AL1(DCMF1EDT+DCMF1TSO)  LEFT EDIT LINE NUMBERS    81253  00524102
         ORG   PDCRECMX                                                 00524202
         DC    H'508'        SET FOR MAX NIH SIZE + LINE NUMBERS 81222  00524302
         ORG   ,                                                        00524402
PATEXCP  DCB   DDNAME=SYSUT1,DSORG=PS,MACRF=(E), SYNAD=SYNADX,         *00524502
               EODAD=EOJEOF,EXLST=123,DEVD=TA                    89365  00524602
         ORG   PATUT1LN+PATEXCP                                  89365  00524702
PATEXCPL EQU   *-PATEXCP     LENGTH                              89365  00524802
         SPACE 1                                                 90277  00524902
PATACB   ACB   DDNAME=SYSUT1,MACRF=(KEY,SEQ,CNV,DIR,IN),EXLST=EXLSTACB  00525002
PATACBLN EQU   *-PATACB      LENGTH TO MOVE                      90277  00525102
EXLSTACB EXLST AM=VSAM,EODAD=EOJEOF LERAD=VSLERAD,SYNAD=VSSYNAD  90277  00525202
         SPACE 1                                                 81222  00525302
NULLLINE VCON  '  '          NULL LINE                           81222  00525402
         SPACE 1                                                        00525502
TITLE    FDOPT NL,CC=C'#'    AUTOMATIC DATE/TIME/PAGE OPTION     81278  00525602
         FDPRT 'PRINTALL LISTING OF',PADR                        81278  00525702
         FDCLI JFCBDSNM,X'04',BNE=TITLEDSN   VTOC ?              81222  00525802
         FDPRT 'VTOC'                                            81222  00525902
         FDGOTO TITLEVOL                                         81222  00526002
TITLEDSN FDPRT JFCBDSNM,DEBR                                     81222  00526102
         FDCLI JFCBELNM,C' ',BE=TITLEVOL,BL=TITLEVOL                    00526202
         FDPRT '('           SHOW MEMBER NAME ALSO                      00526302
         FDPRT JFCBELNM,DEBR                                            00526402
         FDPRT ')'                                                      00526502
TITLEVOL FDPRT 'ON',PAD                                                 00526602
         FDPRT JFCBVOLS,6                                               00526702
         FDPRT *END                                                     00526802
         SPACE 1                                                 81222  00526902
BLOCKFD  FDPRT 'BLOCK',PADR,NL                                   81222  00527002
         FDPRT NUMBLOCK,I,RADJ,LEN=7                             81222  00527102
         FDPRT ',',PADR                                          81222  00527202
         FDPRT CURLEN,I,RADJ,PAD,LEN=5                           81253  00527302
         FDPRT 'BYTES  '                                         81222  00527402
         FDPRT BLOCKMTX,DEB                                      81222  00527502
         FDCLI KEYADE+7,0,BE=BLOCKFDX  SKIP KEY IF NONE          81222  00527602
         FDPRT KEYLINE,PAD,DEB   ELSE PRINT KEY                  81222  00527702
BLOCKFDX FDPRT *END                                              81222  00527802
         SPACE 1                                                        00527902
SUB4TOP  EQU   *                                                        00528002
SUB1TOP  EQU   *                                                        00528102
SUBTPFX  FDPRT OPTMODE,RADJ,DEB,NL,LEN=7                                00528202
         FDPRT OPTMODE,RADJ,DEB,PADL,LEN=6                              00528302
         FDPRT '   OFFSET'                                              00528402
SUBTPFXX FDOPT SBA=(*,+33)                                              00528502
         FDPRT SSCOL         PRINT TOP COLUMN LINE                      00528602
         FDPRT '1'                                                      00528702
         FDPRT SSCOL                                                    00528802
         FDPRT '2'                                                      00528902
         FDPRT SSCOL                                                    00529002
         FDPRT '3'                                                      00529102
         FDPRT SSCOL                                                    00529202
         FDPRT '4'                                                      00529302
         FDPRT SSCOL                                                    00529402
         FDPRT '5'                                                      00529502
         FDPRT SSCOL                                                    00529602
         FDPRT '6'                                                      00529702
         FDPRT SSCOL                                                    00529802
         FDPRT '7'                                                      00529902
         FDPRT SSCOL                                                    00530002
         FDPRT '8'                                                      00530102
         FDPRT SSCOL                                                    00530202
         FDPRT '9'                                                      00530302
         FDPRT SSCOL                                                    00530402
         FDPRT '1'                                                      00530502
         FDPRT *END                                                     00530602
         SPACE 1                                                        00530702
SUB4BOT  EQU   *                                                        00530802
SUB1BOT  EQU   *                                                        00530902
SUBBPFX  FDPRT 'NUMBER',RADJ,NL,PADR,LEN=7                              00531002
         FDPRT 'LENGTH',LEN=6                                           00531102
         FDPRT ' DEC   HEX'                                             00531202
SUBBPFXX FDOPT SBA=(*,+33)                                              00531302
         FDPRT SSNUM                                                    00531402
         FDPRT SSNUM                                                    00531502
         FDPRT SSNUM                                                    00531602
         FDPRT SSNUM                                                    00531702
         FDPRT SSNUM                                                    00531802
         FDPRT SSNUM                                                    00531902
         FDPRT SSNUM                                                    00532002
         FDPRT SSNUM                                                    00532102
         FDPRT SSNUM                                                    00532202
         FDPRT SSNUM                                                    00532302
         FDPRT *END                                                     00532402
         SPACE 1                                                        00532502
SUB2TOP  FDEXEC SUBTPFX,SUBTPFXX                                        00532602
         FDOPT SBA=(*,+27)                                              00532702
         FDPRT '0',LEN=36                                               00532802
         FDPRT '1',LEN=36                                               00532902
         FDPRT '0               1',PADL                                 00533002
         FDPRT *END                                                     00533102
         SPACE 1                                                        00533202
SUB2BOT  FDEXEC SUBBPFX,SUBBPFXX                                        00533302
         FDOPT SBA=(*,+27)                                              00533402
         FDPRT SSHEX                                                    00533502
         FDPRT SSHEX                                                    00533602
         FDPRT TRANNUM,32,PADL                                          00533702
         FDPRT *END                                                     00533802
         SPACE 1                                                        00533902
SUB3TOP  FDEXEC SUBTPFX,SUBTPFXX                                        00534002
         FDOPT SBA=(*,+26)                                              00534102
         FDPRT '0',LEN=36                                               00534202
         FDPRT '1',LEN=36                                               00534302
         FDPRT '2'                                                      00534402
         FDPRT *END                                                     00534502
         SPACE 1                                                        00534602
SUB3BOT  FDEXEC SUBBPFX,SUBBPFXX                                        00534702
         FDOPT SBA=(*,+26)                                              00534802
         FDPRT SSHEX                                                    00534902
         FDPRT SSHEX                                                    00535002
         FDPRT SSHEX,35                                                 00535102
         FDPRT *END                                                     00535202
         SPACE 1                                                        00535302
OPTMSG   FDPRT ' ',NL,LEN=18                                            00535402
         FDPRT 'ACCEPTED OPTIONS ARE :',PADR                            00535502
         FDPRT OPTFORM                                                  00535602
         FDPRT 'FORMAT,',PAD                                            00535702
         FDTM  FLAGS,FGSEG,BZ=OPTMNSP                            81222  00535802
         FDPRT 'SPANNED',PAD                                     81222  00535902
OPTMNSP  FDPRT OPTMODE,PAD,DEB                                   81222  00536002
         FDPRT 'MODE,',PAD                                              00536102
         FDPRT 'START',PAD                                              00536202
         FDPRT OPTMODE,PAD,DEB                                          00536302
         FDPRT START,I                                                  00536402
         FDPRT ', INTERVAL',PADR                                        00536502
         FDPRT INTVAL,I                                                 00536602
         FDPRT ', LIMIT',PADR                                           00536702
         FDPRT LIMIT,I                                                  00536802
         FDCLI FLAGC,0,BE=OPTMSGEX  ANY TRANSLATION/CONVERSION ? 81253  00536902
         FDTM  FLAGC,FGASC9,BZ=OPTMSGC9                          81253  00537002
         FDPRT ' ',NL,LEN=20                                     81253  00537102
         FDPRT 'DATA WILL BE CONVERTED FROM UNIVAC FIELD-DATA FORMAT'   00537202
OPTMSGC9 FDTM  FLAGC,FGTRANS+FGASCII,BZ=OPTMSGEX                 81253  00537302
         FDPRT ' ',NL,LEN=20                                     81253  00537402
         FDPRT 'DATA WILL BE TRANSLATED FROM ASCII TO EBCDIC'    81253  00537502
OPTMSGEX FDPRT *END                                              81253  00537602
         SPACE 1                                                        00537702
MSGCARD  FDOPT NL,SBA=(*,-80)                                           00537802
         FDPRT 0(R5),80      PRINT SCAN CARD IMAGE                      00537902
         FDPRT *END                                                     00538002
         SPACE 1                                                        00538102
MSGLIMIT FDEXEC MSGEOF,MSGEOFX  DOUBLE-SPACE                            00538202
         FDPRT '*** LIMIT COUNT REACHED AT'                             00538302
         FDGOTO MSGEOFC      GO TO COMMON                               00538402
MSGEOF   FDPRT ' ',NL                                                   00538502
MSGEOFX  FDPRT ' ',NL                                                   00538602
         FDPRT '*** END-FILE AFTER'                                     00538702
MSGEOFC  FDPRT 'BLOCK',PAD                                              00538802
         FDPRT NUMBLOCK,I,PAD                                           00538902
         FDPRT '***',PADL                                               00539002
         FDTM  PFLAG,FGFOUND+FGPRINT,BO=MSGEOFEX                 81222  00539102
         FDPRT 'NO',RADJ,LEN=10                                         00539202
         FDPRT OPTMODE,PADL,DEB                                         00539302
         FDPRT 'S',PADR                                                 00539402
         FDTM  PFLAG,FGPRINT,BZ=MSGEOFMT                         81222  00539502
         FDPRT 'IN RANGE'                                               00539602
         FDGOTO MSGEOFEX                                                00539702
MSGEOFMT FDPRT 'MATCHING SCAN REQUEST'                                  00539802
MSGEOFEX FDPRT *END                                                     00539902
         SPACE 1                                                        00540002
MSGNOUT1 FDPRT CURRDDNM,DEBR,PADR,NL                             81222  00540102
         FDPRT 'DD CARD OMITTED'                                 81222  00540202
         FDPRT *END                                              81222  00540302
         SPACE 1                                                 86014  00540402
MSGBDERR FDCLC NUMERR,ZEROES,4,BE=MSGBDERX                       86014  00540502
         FDOPT NL,CC=C'0'    DOUBLE SPACE                        86014  00540602
         FDREPT 10,C'*'      FRAME                               86014  00540702
         FDPRT NUMERR,I,DEB,PAD                                  86014  00540802
         FDPRT 'BLOCKS WITH LOGICAL OR PHYSICAL ERRORS',PAD      86014  00540902
         FDREPT 10,C'*'                                          86014  00541002
MSGBDERX FDPRT *END                                              86014  00541102
         SPACE 1                                                        00541202
MSGDFPRM VCON  'PARM DATA OMITTED; DEFAULTS WILL BE USED'               00541302
         SPACE 1                                                 83277  00541402
MSGBDDVC FDPRT 'DD CARD',NL,RADJ,LEN=12                          83277  00541502
         FDPRT CURRDDNM,PAD,DEB                                  83277  00541602
         FDPRT 'SPECIFIES AN INVALID INPUT DEVICE'               83277  00541702
         FDPRT *END                                              83277  00541802
         SPACE 1                                                        00541902
MSGBDOPN FDPRT 'OPEN FAILED FOR',PADR,NL,RADJ,LEN=20             81222  00542002
         FDPRT CURRDDNM                                          81222  00542102
         FDPRT *END                                              81222  00542202
         SPACE 1                                                        00542302
MSGBDCRD VCON  '&&CARD IN ERROR'                                        00542402
MSGBDSCN VCON  '-RUN TERMINATED DUE TO ABOVE ERRORS'                    00542502
MSG2MANY VCON  '0 TOO MANY REQUESTS - RERUN IN LARGER REGION'           00542602
MSGHDSCN VCON  '0                  SCAN OPTIONS :'                      00542702
         SPACE 1                                                        00542802
TRANTOHX DC    X'000A0B0C0D0E0F00000000000000000000010203040506070809'  00542902
TRANZONE DC    16C'0',16C'1',16C'2',16C'3'   VERTICAL FORMAT            00543002
         DC    16C'4',16C'5',16C'6',16C'7'   ZONE TRANSLATION TABLE     00543102
         DC    16C'8',16C'9',16C'A',16C'B'                              00543202
         DC    16C'C',16C'D',16C'E',16C'F'                              00543302
TRANNUM  DC    16C'0123456789ABCDEF'   VERTICAL FORMAT - NUMERICS       00543402
         SPACE 1                                                 81222  00543502
TRANEBCD DC    256X'01'      TRT FOR PRINTABLE CHARACTERS        81222  00543602
         ORG   TRANEBCD+C' '                                     81222  00543702
         DC    X'00'         BLANK                               81222  00543802
         ORG   TRANEBCD+C'A'                                     81222  00543902
         DC    9X'00'        A-I                                 81222  00544002
         ORG   TRANEBCD+C'J'                                     81222  00544102
         DC    9X'00'        J-R                                 81222  00544202
         ORG   TRANEBCD+C'S'                                     81222  00544302
         DC    8X'00'        S-Z                                 81222  00544402
         ORG   TRANEBCD+C'0'                                     81222  00544502
         DC    10X'00'       0-9                                 81222  00544602
         ORG   TRANEBCD+C'.'                                     81222  00544702
         DC    6X'00'        . < ( +   &                         81222  00544802
         ORG   TRANEBCD+C'$'                                     81222  00544902
         DC    7X'00'        $ * ) ; ^ - /                       81222  00545002
         ORG   TRANEBCD+C','                                     81222  00545102
         DC    5X'00'        , % _ > ?                           81222  00545202
         ORG   TRANEBCD+C':'                                     81222  00545302
         DC    6X'00'        : # @ ' = "                         81222  00545402
         ORG   ,                                                 81222  00545502
         SPACE 1                                                 81253  00545602
*        TRANSLATE TABLE LIFTED FROM SVC 103 (PTF 77533 LEVEL)          00545702
TRASTOEB DC    X'00010203372D2E2F1605250B0C0D0E0F'               81253  00545802
         DC    X'101112133C3D322618193F271C1D1E1F'               81253  00545902
         DC    X'404F7F7B5B6C507D4D5D5C4E6B604B61'               81253  00546002
         DC    X'F0F1F2F3F4F5F6F7F8F97A5E4C7E6E6F'               81253  00546102
         DC    X'7CC1C2C3C4C5C6C7C8C9D1D2D3D4D5D6'               81253  00546202
         DC    X'D7D8D9E2E3E4E5E6E7E8E9ADE0BD5F6D'               81253  00546302
         DC    X'79818283848586878889919293949596'               81253  00546402
         DC    X'979899A2A3A4A5A6A7A8A9C06AD0A107'               81253  00546502
*        DC    X'3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F'                      00546602
*        DC    X'3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F'                      00546702
*        DC    X'3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F'                      00546802
*        DC    X'3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F'                      00546902
*        DC    X'3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F'                      00547002
*        DC    X'3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F'                      00547102
*        DC    X'3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F'                      00547202
*        DC    X'3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F3F'                      00547302
         SPACE 1                                                 89171  00547402
*        TRANSLATE TABLES LIFTED FROM SVC 103 (PTF 77533 LEVEL)  89171  00547502
TREBTOAS DC    X'000102031A091A7F1A1A1A0B0C0D0E0F'               89171  00547602
         DC    X'101112131A1A081A18191A1A1C1D1E1F'               89171  00547702
         DC    X'1A1A1A1A1A0A171B1A1A1A1A1A050607'               89171  00547802
         DC    X'1A1A161A1A1A1A041A1A1A1A14151A1A'               89171  00547902
         DC    X'201A1A1A1A1A1A1A1A1A1A2E3C282B21'               89171  00548002
         DC    X'261A1A1A1A1A1A1A1A1A1A242A293B5E'               89171  00548102
         DC    X'2D2F1A1A1A1A1A1A1A1A7C2C255F3E3F'               89171  00548202
         DC    X'1A1A1A1A1A1A1A1A1A603A2340273D22'               89171  00548302
         DC    X'1A6162636465666768691A1A1A1A1A1A'               89171  00548402
         DC    X'1A6A6B6C6D6E6F7071721A1A1A1A1A1A'               89171  00548502
         DC    X'1A7E737475767778797A1A1A1A1A1A1A'               89171  00548602
         DC    X'1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A'               89171  00548702
         DC    X'7B4142434445464748491A1A1A1A1A1A'               89171  00548802
         DC    X'7D4A4B4C4D4E4F5051521A1A1A1A1A1A'               89171  00548902
         DC    X'5C1A535455565758595A1A1A1A1A1A1A'               89171  00549002
         DC    X'303132333435363738391A1A1A1A1A1A'               89171  00549102
         TITLE 'P R I N T A L L  ***  PROGRAM INITIALIZATION'   GP99187 00549202
*        ONE TIME CALL AS PART OF INITIALIZATION                 90277  00549302
*                                                                90277  00549402
         USING INITCODE,R8   SET BY CALLER                       90277  00549502
INITCODE SERVINIT ,          LOAD THE SERVICE ROUTINE           GP99187 00549602
         SERVCALL LPALD,=CL8'@PRINTER'  LOAD THE PRINTER ROUTINE        00549702
         ST    R0,@PRINTER                                      GP99187 00549802
         PRTOPEN SYSPRINT,OPT=(ABEND,NOWTO)                     GP99187 00549902
         SPACE 1                                                 81222  00550002
         LA    R1,SYSUT1                                         81222  00550102
         ST    R1,OPUT1      MAKE OPEN/CLOSE LIST                81222  00550202
         MVC   OPUT1(1),RDAHED   MAKE INPUT/REREAD              GP99187 00550302
         LA    R14,INFMJFCB                                      81222  00550402
         LA    R15,INEXIT    DCB EXIT                            81222  00550502
         STM   R14,R15,EXITLIST                                  81222  00550602
         MVI   EXITLIST,X'07'                                    81222  00550702
         MVI   EXITLIST+4,X'85'  EXIT LIST                       81222  00550802
         MVC   TABOFFLO,=H'32767'  INITIALIZE SCAN RANGE         81222  00550902
         LA    R1,VLINEX-VLINE                                   81222  00551002
         STH   R1,VLINE      MAKE VCON LENGTH                    81222  00551102
         MVI   BLOCKMTB,C' '                                     81222  00551202
         MVC   CURRDDNM,PATUT1+DCBDDNAM-IHADCB  INIT DDNAME      81222  00551302
         LA    R1,1                                              81222  00551402
         ST    R1,SAVSTART   SET START BLOCK(RECORD) TO 1        81222  00551502
         ST    R1,SAVSTART+8  SET INTERVAL                       81222  00551602
         STH   R1,CURRDD#    SET CURRENT DD NUMBER               81222  00551702
         MVC   GETMAIN,GETMEAN  MOVE GETMAIN PATTERN             81222  00551802
         MVI   TRANSTAB+128,X'3F'  MAKE SECOND HALF OF ASCII     81253  00551902
         MVC   TRANSTAB+129(127),TRANSTAB+128  INVALID           81253  00552002
         L     R1,=A(TRASTOEB)  GET LOW HALF PATTERN             89365  00552102
         MVC   TRANSTAB(128),0(R1)  INITIALIZE VALID HALF        89365  00552202
         B     NEXTDDNM      COMPLETED INITIALIZATION           GP99187 00552302
         SPACE 2                                                GP99187 00552402
INITPARM LA    R9,0(,R7)     DON'T TRUST ANYBODY                 81222  00552502
         LTR   R9,R9         IS THERE A PARM ?                  GP99187 00552602
         BZ    PARMDFLT      NO; SET DEFAULT                    GP99187 00552702
         LAT   R9,0(R9),PARMDFLT  GET PARM ADDRESS OR SET MESSAGE       00552802
         LH    R4,0(,R9)     GET PARM LENGTH                    GP99187 00552902
         LTR   R4,R4         ANY PARM LENGTH ?                  GP99187 00553002
         BNP   PARMDFLT      NO; PRINT DEFAULT MESSAGE          GP99187 00553102
         LA    R5,1(R4,R9)   POINT TO LAST BYTE OF PARM         GP99187 00553202
         LA    R6,1(,R9)     SET TO FIRST BYTE -1               GP99187 00553302
         LA    R4,1          SET BX INCREMENT                   GP99187 00553402
PARMLOOP BXH   R6,R4,INITPARX  GET NEXT CHARACTER OR OUT        GP99187 00553502
PARMLOOT CLI   0(R6),C'0'    NUMERIC FIELD ?                    GP99187 00553602
         BL    PARMNNUM      NO                                 GP99187 00553702
         CLI   0(R6),C'9'    REALLY ?                           GP99187 00553802
         BNH   PARMNUMB      CHECK FURTHER                      GP99187 00553902
PARMNNUM LA    R14,PARMCHAR  GET CHARACTER TABLE                 81222  00554002
         LA    R15,(PARMCHAT-PARMCHAR)/(PARMCHA2-PARMCHAR) NUMBER       00554102
PARMLOOK CLC   0(1,R6),0(R14)   MATCH ?                          81222  00554202
         BNE   PARMLUMP      NO; BUMP                            81222  00554302
         LA    R15,FLAGS     POINT TO FIRST FLAG BYTE            81253  00554402
         C     R14,=A(PARMCHB2)  DOING SECOND BYTE ?             81253  00554502
         BL    PARMLSET      NO; SET OPTION                      81253  00554602
         LA    R15,FLAGC     POINT TO SECOND BYTE                81253  00554702
PARMLSET NC    0(1,R15),2(R14)  RESET FLAGS                      81253  00554802
         OC    0(1,R15),1(R14)  OR IN FLAGS FOR CURRENT FUNCTION 81253  00554902
         B     PARMLOOP      SKIP TO NEXT ENTRY                  81222  00555002
         SPACE 1                                                 81222  00555102
PARMCHAR DC    C'L',AL1(0,255-FGFOFF)            EBCDIC LIST    GP99187 00555202
PARMCHA2 DC    C'D',AL1(FGDUAL,255-FGFOFF)       HEX+EBCDIC LIST        00555302
         DC    C'H',AL1(FGHEX,255-FGFOFF)    HEX ONLY           GP99187 00555402
         DC    C'V',AL1(FGVERT,255-FGFOFF)   VERTICAL HEX+EBCDIC        00555502
         DC    C'Y',AL1(FGHEX+FGALT,255-FGFOFF)   HORIZ. HEX+EBCDIC     00555602
         DC    C'R',AL1(FGREC,255-FGKEY)     DEBLOCK OPTION     GP99187 00555702
         DC    C'W',AL1(FGREC,255-FGKEY)     DEBLOCK (WYLBUR)   GP99187 00555802
         DC    C'K',AL1(FGKEY,255-FGREC-FGSEG) KEYS AS DATA; BLOCKS     00555902
         DC    C'S',AL1(FGSEG+FGREC,255-FGKEY) STACK SPANNED RECORDS    00556002
         DC    C'I',AL1(FGINDW,255)          RDW AS DATA        GP99187 00556102
         DC    C'E',AL1(FGEXCP,255)          USE EXCP           GP99187 00556202
         DC    C'X',AL1(FGEXCP,255)          USE EXCP           GP99187 00556302
PARMCHB2 DC    C'Q',AL1(FGASCII,255)             ASCII TRANSLATE        00556402
         DC    C'N',AL1(FGASC9+FGASCII,255)  9-BIT ASCII/FIELDDATA      00556502
         DC    C'A',AL1(FGASCII,255)         ASCII PRINT ONLY   GP99187 00556602
         DC    C'-',AL1(FGBACK,255)          READ BACKWARDS     GP99187 00556702
         DC    C' ',AL1(0,255)               SPACER             GP99187 00556802
PARMCHAT EQU   *             END OF PARM LIST                    81222  00556902
         SPACE 1                                                 81222  00557002
PARMLUMP LA    R14,PARMCHA2-PARMCHAR(,R14)                       81222  00557102
         BCT   R15,PARMLOOK  TRY ANOTHER                         81222  00557202
         SR    R0,R0                                            GP99187 00557302
         CLI   0(R6),C','    NUMBER SEPARATOR ?                 GP99187 00557402
         BE    PARMWNUM      YES; INCREMENT NUMERIC             GP99187 00557502
PARMERR  OI    PARMFG,FPERR  SET ERROR FLAG                     GP99187 00557602
         B     PARMLOOP                                         GP99187 00557702
         SPACE 1                                                GP99187 00557802
PARMNUMB SR    R0,R0         CLEAR ACCUMULATOR                  GP99187 00557902
         LA    R1,C'0'       PRESET FOR NULL                     82031  00558002
         LA    R14,7         SET MAXIMUM LENGTH ACCEPTABLE      GP99187 00558102
PARMNULP CLI   0(R6),C'0'    NUMERIC ?                          GP99187 00558202
         BL    PARMNULR      NO                                 GP99187 00558302
         CLI   0(R6),C'9'                                       GP99187 00558402
         BH    PARMNULR                                         GP99187 00558502
         SLDL  R0,8          SHIFT OVER                         GP99187 00558602
         IC    R1,0(,R6)     LOAD NEXT DIGIT                    GP99187 00558702
         BXH   R6,R4,PARMNULN  TRY NEXT ONE                     GP99187 00558802
         BCT   R14,PARMNULP                                     GP99187 00558902
PARMNULR BCTR  R6,0          FAKE FOR AUTO INCREMENT AFTER STORE        00559002
PARMNULN STM   R0,R1,WORK    STASH CHARACTER STRING             GP99187 00559102
         PACK  DB,WORK(8)    PACK IT                            GP99187 00559202
         CVB   R0,DB         GET BINARY VALUE                   GP99187 00559302
PARMWNUM TM    PARMFG,FPNUM1  LIMIT SUPPLIED ?                  GP99187 00559402
         BNZ   PARMWNM2      YES                                GP99187 00559502
         LA    R14,LIMIT     SET FOR LIMIT                      GP99187 00559602
         OI    PARMFG,FPNUM1  SET SUPPLIED                      GP99187 00559702
         B     PARMWCOM                                         GP99187 00559802
PARMWNM2 TM    PARMFG,FPNUM2   START SUPPLIED ?                 GP99187 00559902
         BNZ   PARMWNM3      YES                                GP99187 00560002
         LA    R14,START     SET START NUMBER                   GP99187 00560102
         OI    PARMFG,FPNUM2                                    GP99187 00560202
         B     PARMWCOM                                         GP99187 00560302
PARMWNM3 TM    PARMFG,FPNUM3  INTERVAL SUPPLIED ?               GP99187 00560402
         BNZ   PARMERR       YES; ERROR                         GP99187 00560502
         LA    R14,INTVAL    SET INCREMENT                      GP99187 00560602
         OI    PARMFG,FPNUM3                                    GP99187 00560702
PARMWCOM LTR   R0,R0                                            GP99187 00560802
         BNP   PARMLNUM      IGNORE IF ZERO                     GP99187 00560902
         ST    R0,0(,R14)    SET VALUE                          GP99187 00561002
PARMLNUM BXH   R6,R4,INITPARX  EXIT IF NO MORE                  GP99187 00561102
         CLI   0(R6),C','    COMMA AFTER FIELD ?                GP99187 00561202
         BNE   PARMLOOT      NO; SEE WHAT IT IS                 GP99187 00561302
         B     PARMLOOP      ELSE SKIP IT                       GP99187 00561402
         SPACE 1                                                GP99187 00561502
PARMDFLT OI    PARMFG,FPDFLT  INDICATE DEFAULT PARM             GP99187 00561602
INITPARX B     PARMDONE      RETURN TO CALLER                   GP99187 00561702
         TITLE 'P R I N T A L L  ***  SCAN REQUEST PROCESSING'   90277  00561802
*        ONE TIME CALL AS PART OF INITIALIZATION                 90277  00561902
*                                                                90277  00562002
         USING SCANINIT,R8   SET BY CALLER                       90277  00562102
SCANINIT OI    PARMFG,FPDONE   SHOW INITIALIZATION DONE         GP99187 00562202
         SERVCALL LPALD,=CL8'@INPREAD'  GET THE READ ROUTINE     81222  00562302
         ST    R0,@INPREAD   SAVE THE ADDRESS                    81222  00562402
         INPOPEN SYSIN,OPT=(DUMMY,NOWTO)  OPEN QUIETLY           81222  00562502
         BXH   R15,R15,SCANEOJ   DUMMY OR NOT FOUND              81222  00562602
SCANGET  INPGET DEV=1        GET A CARD                          81222  00562702
         BXH   R15,R15,SCANEOJ   TERMINATE ON ERROR              81222  00562802
         LR    R5,R1         SAVE TEXT POINTER                          00562902
         ICM   R15,15,TABLE  FIRST TIME ?                               00563002
         BNZ   SCNNTONE      NO                                         00563102
         PRTV  MSGHDSCN      PRINT SCAN HEADER                          00563202
         GETMAIN VC,A=TABLE,LA=MAINTAB,MF=(E,GETMAIN)            81223  00563302
         BXLE  R15,R15,SCNSTRGE YES; HAVE STORAGE                       00563402
         PRTV  MSG2MANY      TATTLE                                     00563502
         B     SCANEOJ                                                  00563602
SCNSTRGE LM    R3,R4,TABLE   GET START/SIZE                             00563702
         LH    R2,=AL2(16*1024)   LEAVE ROOM FOR ABENDS                 00563802
         LA    R1,0(R3,R4)   GET END ADDRESS                            00563902
         SR    R1,R2         MAKE ADDRESS TO FREE                       00564002
         FREEMAIN R,LV=(R2),A=(R1)  FREE SOME                           00564102
         SR    R4,R2         NEW LENGTH                                 00564202
         LR    R14,R3        COPY ADDRESS                               00564302
         LR    R15,R4        COPY LENGTH                                00564402
         SR    R0,R0                                            GP99187 00564502
         SR    R1,R1                                            GP99187 00564602
         MVCL  R14,R0        CLEAR IT                                   00564702
         SH    R4,=H'100'    PREVENT OVERRUN                            00564802
         AR    R4,R3         MAKE END ADDRESS                           00564902
         STM   R3,R4,TABLE   SET NEW LIMITS                             00565002
         ST    R3,TABLECUR   SET CURRENT TABLE POSITION                 00565102
         MVC   0(5,R3),=5X'FF'  INITIALIZE                              00565202
         SPACE 1                                                        00565302
         USING TABSECT,R4                                               00565402
SCNNTONE PRTLIST MSGCARD     LIST CARD                                  00565502
         LA    R7,71(,R5)    SCAN 72 COLUMNS                            00565602
         LA    R6,1          SET BXH INCREMENT                          00565702
         L     R4,TABLECUR   GET NEXT AVAILABLE ENTRY IN TABLE          00565802
         BAS   R9,NOBLANK    GET FIRST NON-BLANK                GP99187 00565902
         B     SCANGET       JUST PRINT BLANK CARDS                     00566002
         CLI   0(R5),C'*'    COMMENTS CARD ?                            00566102
         BE    SCANGET       YES - JUST GET ANOTHER CARD                00566202
         NI    PARMFG,255-FSHEX  RESET HEX STRING OPTION                00566302
SCNNEXT  CLI   0(R5),C'*'    TRAILING COMMENTS ?                        00566402
         BE    SCNCDONE      YES - CARD DONE                            00566502
         CLI   0(R5),C'('    CARD COLUMN SPECIFICATION ?                00566602
         BE    SCNCOLS                                                  00566702
         CLI   0(R5),C'X'    POSSIBLE HEX REQUEST ?                     00566802
         BNE   SCNNEXTQ      NO; TRY FOR QUOTE                          00566902
         OI    PARMFG,FSHEX  SET HEX OPTIONS                            00567002
         BAS   R9,NXBLANK    SKIP TO NEXT NON-BLANK             GP99187 00567102
         B     SCNBCARD      ERROR IF WIDOW                             00567202
SCNNEXTQ CLI   0(R5),C''''   QUOTED STRING ?                            00567302
         BE    SCNQUOTE                                                 00567402
         CLI   0(R5),C'"'    ALTERNATE QUOTE ?                          00567502
         BE    SCNQUOTE      YES                                        00567602
SCNBCARD PRTV  MSGBDCRD      BAD CARD                                   00567702
         OI    CONDCODE+1,8  SHOW BAD CARD                              00567802
         MVC   0(5,R4),=5X'FF'  RESET CURRENT TABLE ENTRY               00567902
         B     SCANGET                                                  00568002
         SPACE 1                                                        00568102
SCNQUOTE MVC   QUOTE,0(R5)   SAVE DELIMITER                             00568202
         CLI   TABLEN,X'FF'  BEEN HERE BEFORE ?                         00568302
         BNE   SCNBCARD      YES - FAIL                                 00568402
         LA    R1,TABTEXT    POINT TO OUTPUT FIELD                      00568502
         LR    R15,R1        SAVE START ADDRESS                         00568602
SCNQUOTT BXH   R5,R6,SCNBCARD  SKIP A BYTE                              00568702
         CLC   QUOTE,0(R5)   TRAILING/DOUBLED DELIMITER ?               00568802
         BNE   SCNQUOTM      NEITHER - MOVE IT                          00568902
         BXH   R5,R6,SCNQUOTZ  GET NEXT BYTE                            00569002
         CLC   QUOTE,0(R5)   DOUBLED DELIMITER ?                        00569102
         BNE   SCNQUOTZ      ELSE END OF INPUT                          00569202
SCNQUOTM MVC   0(1,R1),0(R5) MOVE ONE BYTE                              00569302
         TM    PARMFG,FSHEX  DOING HEX STRING ?                         00569402
         BZ    SCNQUOT1      NO                                         00569502
         CLI   0(R1),C'A'    VALID ?                                    00569602
         BL    SCNBCARD      NO                                         00569702
         CLI   0(R1),C'F'    IN LOW RANGE ?                             00569802
         BNH   SCNQUOT1      YES; KEEP IT                               00569902
         CLI   0(R1),C'0'    NUMERIC ?                                  00570002
         BL    SCNBCARD      NO; ERROR                                  00570102
         CLI   0(R1),C'9'                                               00570202
         BH    SCNBCARD      DITTO                                      00570302
SCNQUOT1 LA    R1,1(,R1)     SET NEXT O/P LOCATION                      00570402
         B     SCNQUOTT      TRY NEXT BYTE                              00570502
SCNQUOTZ SR    R1,R15        GET LENGTH OF STRING                       00570602
         BNP   SCNBCARD      LESS THAN ONE ?                            00570702
         BCTR  R1,0          - ONE FOR EXECUTE                          00570802
         STC   R1,TABLEN     SAVE STRING LENGTH                         00570902
         TM    PARMFG,FSHEX  HEX STRING ?                        89171  00571002
         BNZ   SCNSKIPB      YES; PROCESS NEXT FIELD             89171  00571102
         TM    FLAGC,FGASCIT ASCII MODE ?                        89171  00571202
         BZ    SCNSKIPB      NO                                  89171  00571302
         L     R14,=A(TREBTOAS)  TRANSLATE TO ASCII              89171  00571402
         EX    R1,EXTRASTA   TRANSLATE STRING TO ASCII           89171  00571502
         B     SCNSKIPB      GO TO NEXT FIELD PROCESSOR                 00571602
EXTRASTA TR    TABTEXT(0),0(R14)  TRANS TO ANS                   89171  00571702
         SPACE 1                                                        00571802
SCNCOLS  CLI   TABOFF,X'FF'  BEEN HERE BEFORE ?                         00571902
         BNE   SCNBCARD      YES - TOO BAD                              00572002
         MVC   TABOFFS,=AL2(0,32767)  SET FIRST/LAST VALID COLUMNS      00572102
         BAS   R9,NXBLANK    GET NEXT NON-BLANK CHARACTER       GP99187 00572202
         B     SCNBCARD      NONE ?                                     00572302
         BAS   R9,GETNUM     CONVERT TO NUMERIC                 GP99187 00572402
         B     SCNBCARD      TOO BAD                                    00572502
         LTR   R0,R0         TEST IT                                    00572602
         BNP   SCNBCARD      ZERO COLUMN ?                              00572702
         CH    R0,=H'32767'  AFTER LAST ?                               00572802
         BH    SCNBCARD                                                 00572902
         BCTR  R0,0          SUBTRACT ONE                               00573002
         STCM  R0,3,TABOFF   SET INTO START                             00573102
         STCM  R0,3,TABOFFE  SET DEFAULT END COLUMN                     00573202
         CLI   0(R5),C')'    SINGLE COLUMN ?                            00573302
         BE    SCNSKIPL      YES; SKIP ONE AND TRY AGAIN                00573402
         CLI   0(R5),C','    ELSE COMMA TO NEXT ONE ?                   00573502
         BNE   SCNBCARD      NO; TOO BAD                                00573602
         BAS   R9,NXBLANK    NEXT CHAR                          GP99187 00573702
         B     SCNBCARD      TOO BAD - NONE                             00573802
         BAS   R9,GETNUM     CONVERT TO NUMERICS                GP99187 00573902
         B     SCNBCARD      NONE ?                                     00574002
         LTR   R0,R0         VALID ?                                    00574102
         BNP   SCNBCARD      NO                                         00574202
         CH    R0,=H'32767'  IN RANGE ?                                 00574302
         BH    SCNBCARD      NO                                         00574402
         BCTR  R0,0          MINUS ONE                                  00574502
         STCM  R0,3,TABOFFE  SAVE IT                                    00574602
         CLC   TABOFF,TABOFFE  VALID RANGE ?                            00574702
         BH    SCNBCARD      NO                                         00574802
         CLI   0(R5),C')'    PROPER TERMINATION ?                       00574902
         BNE   SCNBCARD      NO; FAIL                                   00575002
SCNSKIPL BXH   R5,R6,SCNCDONE  SKIP A COLUMN                            00575102
SCNSKIPB BAS   R9,NOBLANK                                       GP99187 00575202
         B     SCNCDONE      NO NON-BLANK                               00575302
         B     SCNNEXT       TRY MORE PARMS                             00575402
         SPACE 1                                                        00575502
NOBLANK  BCTR  R5,0          BACKSPACE ONE                              00575602
NXBLANK  BXH   R5,R6,0(R9)   RETURN ERROR IF ALL GONE                   00575702
NOBLANK1 CLI   0(R5),C' '    BLANK ?                                    00575802
         BNE   4(,R9)        RETURN OK                                  00575902
         BXLE  R5,R6,NOBLANK1  ELSE TRY NEXT ONE                        00576002
         BR    R9            NO MORE                                    00576102
         SPACE 1                                                        00576202
GETNUM   BCTR  R5,0          ANY INPUT REMAINING ?                      00576302
         BXH   R5,R6,0(R9)   TOO BAD                                    00576402
         CLI   0(R5),C'0'    FIRST DIGIT NUMERIC ?                      00576502
         BLR   R9            TOO BAD, TOO                               00576602
         CLI   0(R5),C'9'                                               00576702
         BHR   R9                                                       00576802
         SR    R14,R14                                          GP99187 00576902
         SR    R15,R15       INITIALIZE ACCUMULATORS            GP99187 00577002
         LA    R1,8          DO MAX OF EIGHT TIMES                      00577102
GETNUM1  SLDL  R14,8         MAKE ROOM FOR CURRENT DIGIT                00577202
         IC    R15,0(,R5)    TACK THE CURRENT ONE ON                    00577302
         BXH   R5,R6,0(R9)   ERROR IF NO MORE                           00577402
         CLI   0(R5),C'0'    NUMERIC ?                                  00577502
         BL    GETNUM2       NO                                         00577602
         CLI   0(R5),C'9'                                               00577702
         BH    GETNUM2                                                  00577802
         BCT   R1,GETNUM1    TRY IT                                     00577902
         B     SCNBCARD                                                 00578002
GETNUM2  STM   R14,R15,WORK  STASH NUMBERS                              00578102
         PACK  DB,WORK       MAKE PACKED                                00578202
         CVB   R0,DB         GET BINARY                                 00578302
         B     4(,R9)                                                   00578402
         SPACE 1                                                        00578502
SCNCDONE CLI   TABLEN,X'FF'  STRING SUPPLIED ?                          00578602
         BE    SCNBCARD      NO                                         00578702
         CLI   TABOFF,X'FF'  COLUMNS SUPPLIED ?                         00578802
         BNE   SCNHVCOL      OK                                         00578902
         MVC   TABOFFS,=AL2(0,32767)  SET DEFAULT COLUMNS               00579002
SCNHVCOL SR    R14,R14                                          GP99187 00579102
         IC    R14,TABLEN    GET STRING LENGTH - 1                      00579202
         TM    PARMFG,FSHEX  HEX STRING SUPPLIED ?                      00579302
         BZ    SCNHVNHX      NO                                         00579402
         EX    R14,SCNWKMVC  MOVE TEXT TO WORK AREA+1                   00579502
         LA    R2,WORKLINE+1 POINT TO START OF TEXT                     00579602
         TM    TABLEN,1      ODD LENGTH STRING ?                        00579702
         BNZ   SCNHXEVN      NO (REMEMBER, L-1 IN TABLEN)               00579802
         BCTR  R2,0          BACK-SPACE ONE BYTE                        00579902
         MVI   0(R2),C'0'    PUT A LEADING ZERO IN                      00580002
SCNHXEVN LA    R14,1(,R14)   GET NUMBER OF INPUT BYTES                  00580102
         LA    R15,1(,R14)                                              00580202
         SRL   R15,1         NUMBER OF OUTPUT BYTES                     00580302
         LR    R14,R15       SAVE LENGTH                                00580402
         BCTR  R14,0         LENGTH-1                                   00580502
         STC   R14,TABLEN    SET NEW LENGTH                             00580602
         LA    R1,TABTEXT    GET START OUTPUT ADDRESS                   00580702
SCNDOHEX NC    0(2,R2),=X'1F1F'  CREAM HIGH BITS                        00580802
         TR    0(2,R2),TRANTOHX  MAKE NUMERICS ONLY                     00580902
         PACK  0(2,R1),0(3,R2)  PACK                                    00581002
         LA    R2,2(,R2)     UP INPUT ADDRESS                           00581102
         LA    R1,1(,R1)     UP OUTPUT                                  00581202
         BCT   R15,SCNDOHEX  REPEAT                                     00581302
SCNHVNHX NI    PARMFG,255-FSHEX  RESET HEX OPTION                       00581402
         LA    R3,TABTEXT-TABSECT+1(R4,R14)  SET NEW START ADDRESS      00581502
         LH    R15,=AL2(32767-2)  GET LAST COLUMN - 2                   00581602
         SR    R15,R14       COMPUTE LAST VALID COMPARE POSITION        00581702
         ICM   R14,3,TABOFFE  GET LAST REQUEST                          00581802
         CR    R14,R15       WILL IT FIT ?                              00581902
         BNH   *+8           YES                                        00582002
         STCM  R15,3,TABOFFE   ELSE SET NEW END LOCATION                00582102
         CLC   TABOFF,TABOFFE  VALID RANGE ?                            00582202
         BH    SCNBCARD      NO; FAIL IT                                00582302
         SR    R14,R14                                          GP99187 00582402
         ICM   R14,3,TABOFF                                             00582502
         CH    R14,TABOFFLO                                             00582602
         BNL   *+8                                                      00582702
         STH   R14,TABOFFLO  SET LOW START SCAN COLUMN                  00582802
         ICM   R14,3,TABOFFE   GET END SCAN                             00582902
         CH    R14,TABOFFHI                                             00583002
         BNH   *+8                                                      00583102
         STH   R14,TABOFFHI  SET HIGH END SCAN COLUMN                   00583202
         SR    R14,R14                                          GP99187 00583302
         IC    R14,TABTEXT                                              00583402
         STC   R14,TRTTAB(R14)   SET TRT STOP                           00583502
         LTR   R14,R14       USER LOOKING FOR HEX ZERO ?         90179  00583602
         BNZ   *+8           NO                                  90179  00583702
         OI    TRTTAB,X'FF'  TRIGGER TRT ON MATCH OF ZERO        90179  00583802
         C     R3,TABLEND    WILL IT FIT ?                              00583902
         BNH   SCNCFITS      YES                                        00584002
         PRTV  MSG2MANY      NO MORE ROOM                               00584102
         MVI   CONDCODE+1,12  SET ERROR                                 00584202
         L     R3,TABLE      RESET TO START - DIAGNOSE REMAINING INPUT  00584302
         SPACE 1                                                        00584402
SCNCFITS ST    R3,TABLECUR   SET NEW TABLE LOCATION                     00584502
         MVC   0(5,R3),=5X'FF'  INITIALIZE NEW TABLE                    00584602
         B     SCANGET       GET THE NEXT CARD                          00584702
         DROP  R4                                                       00584802
         SPACE 1                                                        00584902
SCANEOJ  CLI   CONDCODE+1,0  TEST FOR ERRORS                            00585002
         BE    SCNCDSOK      OK                                         00585102
         PRTV  MSGBDSCN      TOO BAD                                    00585202
         B     QUIT8         EXIT UNGRACEFULLY                          00585302
         SPACE 1                                                        00585402
SCNCDSOK SERVCALL LPADL,=CL8'@INPREAD'  CLOSE AND FREE SYSIN     81222  00585502
         ICM   R4,15,TABLE+4 GET TABLE END ADDRESS-100                  00585602
         BZ    SCNDONE       SKIP IF NONE                               00585702
         LA    R4,100(,R4)   ADD SAFETY BACK IN                         00585802
         L     R3,TABLE      GET START OF TABLE                         00585902
         CLI   0(R3),X'FF'   ANY ENTRIES ?                              00586002
         BNE   SCANFRET      YES; FREE SOME                             00586102
         SR    R4,R3         MAKE LENGTH                                00586202
         XC    TABLE(8),TABLE   CLEAR TABLE POINTERS                    00586302
         B     SCANFREE      FREE ALL OF GOTTEN STORAGE                 00586402
SCANFRET SRL   R4,3                                                     00586502
         SLL   R4,3          ROUND TO DOUBLE WORD                       00586602
         L     R3,TABLECUR   GET STOP ADDRESS                           00586702
         LA    R3,TABTEXT-TABLEN+7(,R3)   ALLOW FOR END AND ROUND       00586802
         SRL   R3,3                                                     00586902
         SLL   R3,3          ROUND TO NEXT HIGHER DOUBLE WORD           00587002
         SR    R4,R3         GET LENGTH TO FREE                         00587102
         BNP   SCNDONE       NONE?  WILL BOMB ON 80A                    00587202
SCANFREE FREEMAIN R,LV=(R4),A=(R3)  FREE IT UP                          00587302
         B     SCNDONE       RETURN                              90277  00587402
         SPACE 1                                                 90277  00587502
         LTORG ,                                                 90277  00587602
         SPACE 3                                                        00587702
TABSECT  DSECT ,             TABLE ENTRY MAPPING                        00587802
TABLEN   DC    AL1(0)        LENGTH OF TEXT-1                           00587902
TABOFFS  DC    0AL4(0)       START/END OFFSET                           00588002
TABOFF   DC    AL2(0)        START OFFSET                               00588102
TABOFFE  DC    AL2(0)        END OFFSET                                 00588202
TABTEXT  DC    0C' '         VARIABLE LENGTH TEXT STRING                00588302
         SPACE 1                                                 89365  00588402
         IEZIOB ,                                                89365  00588502
         EJECT ,                                                 81222  00588602
SAVE     DSECT ,             DYNAMIC WORK AREA                   81222  00588702
DB       DC    D'0'                                                     00588802
WORK     DC    CL8' '        MUST FOLLOW DB                             00588902
         SPACE 1                                                        00589002
@SERVICE DC    A(0)          ADDRESS OF SERVICE ROUTINE                 00589102
@INPREAD DC    A(0)          ADDRESS OF INPUT PROCESSOR          81222  00589202
@PRINTER DC    A(0)          ADDRESS OF PRINTER ROUTINE                 00589302
@TRACE   DC    A(0)          ADDRESS OF DEBUG PGMTRACE ROUTINE          00589402
ZEROES   DC    F'0'          CONSTANT 0                          86014  00589502
BLOCKADE DC    A(0,0)        ADDRESS/LENGTH OF BSAM BUFFER       81222  00589602
RECADE   DC    A(0,0)        ADDRESS OF V(B)S RECORD INTERFACE AREA     00589702
KEYADE   DC    A(0,0)        ADDRESS/LENGTH OF KEY               81222  00589802
FORMSAVE DC    5A(0)         LINE FORMATTING SAVE AREA                  00589902
SPANSAVE DC    9A(0)         SPANNING/FORMAT SAVE AREA           81222  00590002
BLOCKXLE DC    3A(0)         BXLE POINTERS FOR BLOCK PROCESSING         00590102
BLOCKHED DC    A(0)          0 OR ADDRESS OF BLOCK HEADER               00590202
START    DC    F'1'          STARTING RECORD TO DUMP                    00590302
LIMIT    DC    F'0'          NUMBER OF RECORDS TO DO (0=ALL)            00590402
INTVAL   DC    F'1'          DUMP EVERY NTH RECORD                      00590502
NUMBLOCK DC    F'0'          NUMBER OF CURRENT BLOCK                    00590602
NUMREC   DC    F'0'          NUMBER OF CURRENT RECORD                   00590702
NUMERR   DC    F'0'          NUMBER OF ERRORS                    86014  00590802
ERRBLOCK DC    F'0'          BLOCK # WITH LAST COUNTED ERROR     86014  00590902
OFFSET   DC    F'0'          OFFSET IN CURRENT RECORD                   00591002
BLKSIZE  DC    H'0'          USER'S BLOCKSIZE                           00591102
LRECL    DC    H'0'          USER'S RECORD LENGTH                       00591202
CURLEN   DC    F'0'          LENGTH OF CURRENT BLOCK             81253  00591302
RECFM    DC    AL1(DCBOPTQ)  USER'S RECORD FORMAT (MASK BEFORE OPEN)    00591402
FLAGS    DC    X'00'         PROCESSING FLAGS                           00591502
FGALT    EQU   X'80'           ALTERNATE FORMATTING             GP99187 00591602
FGHEX    EQU   X'40'           DUMP IN HEX ONLY                 GP99187 00591702
FGDUAL   EQU   X'20'           PRINT HEX AND TEXT               GP99187 00591802
FGLIST   EQU   FGHEX+FGDUAL    BOTH OFF - TEXT ONLY             GP99187 00591902
FGVERT   EQU   FGHEX+FGDUAL    BOTH ON - VERTICAL TEXT AND HEX  GP99187 00592002
FGFOFF   EQU   FGHEX+FGDUAL+FGALT  ALL FORMATTING BITS          GP99187 00592102
FGREC    EQU   X'10'           DEBLOCK INTO RECORDS             GP99187 00592202
FGKEY    EQU   X'08'           NO DEBLOCK; TREAT KEY AS DATA    GP99187 00592302
FGSEG    EQU   X'04'           VS SPANNED RECORD INTERFACE OPTION       00592402
FGINDW   EQU   X'02'           INCLUDE RDW/SDW WITH DATA DISPLAY        00592502
FGEXCP   EQU   X'01'           USE EXCP FOR TAPE                GP99187 00592602
FLAGC    DC    X'00'         CONVERSION PROCESSING               81253  00592702
FGASCIT  EQU   X'80'           ASCII ON TEXT DISPLAY LINE ONLY   89171  00592802
FGBACK   EQU   X'40'           READ FILE BACKWARDS              GP95208 00592902
FGASC9   EQU   X'04'           9-BIT ASCII INPUT                 81253  00593002
FGASCII  EQU   X'02'           ASCII TRANSLATE IN PARM           81253  00593102
FGTRANS  EQU   X'01'           OPTCD=Q SPECIFIED                 81253  00593202
PFLAG    DC    X'00'         PRINT/SCAN CONTROL FLAG             81222  00593302
FGVSAM   EQU   X'80'           PROCESSING A VSAM FILE            90277  00593402
FGPDS    EQU   X'40'           PROCESSING A DIRECTORY           GP99187 00593502
FGPDSE   EQU   FGPDS+FGVSAM    PROCESSING A PDSE (NOT YET)      GP99187 00593602
FGFOUND  EQU   X'08'           RECORD IN START/LIMIT RANGE FOUND 81222  00593702
FGPRINT  EQU   X'04'           RECORD PRINTED                    81222  00593802
FGSPC    EQU   X'01'           DOUBLE-SPACE NEXT RECORD          81222  00593902
PARMFG   DC    X'00'         PARM PROCESSING FLAG                       00594002
FPDFLT   EQU   X'80'           DEFAULT PARM IN USE                      00594102
FPERR    EQU   X'40'           ERROR IN PARM FIELD                      00594202
FPDONE   EQU   X'04'           RECURSION SWITCH                 GP99187 00594302
FSHEX    EQU   X'08'           SCAN - HEX STRING REQUEST                00594402
FPNUM3   EQU   X'04'           THIRD NUMERIC PARM FOUND                 00594502
FPNUM2   EQU   X'02'           SECOND NUMERIC PARM FOUND                00594602
FPNUM1   EQU   X'01'           FIRST NUMERIC PARM FOUND                 00594702
OPTSUB1  DC    A(0)          CURRENT SUBTITLE TOP LINE                  00594802
OPTSUB2  DC    A(0)          CURRENT SUBTITLE BOTTOM LINE               00594902
OPTLEN   DC    H'0'          DATA LENGTH OF A TEXT LINE                 00595002
OPTFORM  DC    C' '          FORMAT CHARACTER                           00595102
         DC    X'00'         RESERVED                                   00595202
SAVSTART DC    3F'0'         SAVE START/LIMIT/INTERVAL           81222  00595302
SAVFLAGS DC    2X'00'        SAVE FLAGS                         GP99187 00595402
VSPREV   DC    0X'00'        SDW CONTROL FLAG OF PRIOR SEGMENT   81222  00595502
QUOTE    DC    C''''         DELIMITER OF CURRENT STRING                00595602
OPTMODE  DC    CL6' BLOCK'   BLOCK OR RECORD MODE                       00595702
OPUT1    OPEN  (SYSUT1,(INPUT,REREAD)),MF=L                             00595802
EXITLIST DC    0A(0),X'07',AL3(INFMJFCB),X'85',AL3(INEXIT)              00595902
MAINTAB  DC    A(20*1024,512*1024)  GETMAIN SIZES                81223  00596002
TABLE    DC    A(0)          START OF SCAN REQUEST TABLE                00596102
TABLEND  DC    A(0)          END OF TABLE                               00596202
TABLECUR DC    A(0)          NEXT AVAILABLE ENTRY ADDRESS               00596302
GETMAIN  DC    XL(GETMEANL)'0'  GETMAIN LIST                     81222  00596402
TABOFFLO DC    H'32767'      LOW SCAN OFFSET                            00596502
TABOFFHI DC    H'0'          HIGH SCAN OFFSET                           00596602
CONDCODE DC    H'0'          RETURN CODE                                00596702
CURRDD#  DC    H'0'          CURRENT SYSUT NUMBER                81222  00596802
CURRDDNM DC    CL8'SYSUT1'   CURRENT DD NAME                     81222  00596902
VLINE    VCON  END=VLINEX    PRINT LINE                                 00597002
LINECC   DC    C' '                                                     00597102
LINE     DC    0CL132' '                                                00597202
LINEHEAD DC    CL32' '       DATA PREFIX                                00597302
LINERECL EQU   LINEHEAD+6,8,C'C'                                        00597402
LINEREC# EQU   LINEHEAD,8,C'C'                                          00597502
LINERDW  EQU   LINERECL+8,10,C'C'  (RDW)                                00597602
LINEOFFD EQU   LINEHEAD+10,8,C'C'                                       00597702
LINEOFFH EQU   LINEOFFD+L'LINEOFFD+2,4,C'C'                             00597802
LINEWYL# EQU   LINEOFFH-4-1,9,C'C'                                      00597902
LINETEXT DC    CL100' '      TEXT PORTION                               00598002
         ORG   LINETEXT      RE-USE FOR KEY PROCESSING           81222  00598102
KEYLINE  DC    CL132' '                                          81222  00598202
         ORG   KEYLINE                                           81222  00598302
KLKEYD   DC    C'KEYLEN='                                        81222  00598402
KLKEYL   DC    CL3' '        LENGTH OF KEY                       81222  00598502
         DC    C' '                                              81222  00598602
KLKEYH   DC    C'KEY='                                           81222  00598702
         DC    C' '                                              81222  00598802
KLKEYT   DC    CL116' '      MAX TEXT LENGTH                     81222  00598902
         DC    C' '                                              81222  00599002
         ORG   ,                                                 81222  00599102
         DC    C' '          FOR OVERRUN OF UNPACK (HEX KEY)     81222  00599202
         VCON  ,                                                        00599302
         DC    C' '          OVERFLOW FOR UNPACK OF LINETEXT            00599402
WORKLINE DC    CL(L'LINETEXT)' '   COPY OF USER'S TEXT                  00599502
BLOCKMTB DC    C' '          BLOCKMTX-1                          81222  00599602
BLOCKMTX DC    CL80' '       EXTRA TEXT                                 00599702
BLOCKMER EQU   BLOCKMTX+13,20,C'C'  ERROR MESSAGE                       00599802
BLOCKSYN EQU   BLOCKMTX+24,44,C'C'  SYNAD MESSAGE                       00599902
BLOCKERR EQU   BLOCKMTX+13,55,C'C'  ANY ERROR MESSAGE            86014  00600002
         SPACE 1                                                        00600102
SYSUT1   DCB   DDNAME=SYSUT1,DSORG=PS,MACRF=(GL),SYNAD=123,            X00600202
               EROPT=ACC,EODAD=123,EXLST=123                     81222  00600302
SYSUTLEN EQU   *-SYSUT1      LENGTH OF DCB FOR MOVE              81222  00600402
READIOB  DC    XL(IOBEXTEN-IOBSTDRD)'0'  IOB                     89365  00600502
READECB  DC    F'0'          EXCP ECB                            89365  00600602
READCCW  CCW   2,1-1,X'20',65535       TAPE CCW                  89365  00600702
         ORG   READIOB                                           89365  00600802
         READ  DECB,SF,SYSUT1,MF=L   BSAM MODE DECB              81222  00600902
LENDECB  EQU   *-DECB        LENGTH FOR MOVE                     81222  00601002
         ORG   SYSUT1                                            90277  00601102
UT1ACB   ACB   DDNAME=SYSUT1,MACRF=(ADR,SEQ,IN)                  90277  00601202
UT1RRN   DC    A(0)          RELATIVE RECORD NO. FEEDBACK        90277  00601302
UT1RECAD DC    A(0)          ADDRESS OF CURRENT RECORD           90277  00601402
UT1GNCB  DS    XL(UT1GNCBL)  SHOWCB WORK AREA                    90351  00601502
UT1MDCB  DS    XL(UT1MDCBL)  MODCB WORK AREA                     90351  00601602
UT1SHCB  DS    XL(UT1SHCBL)  SHOWCB WORK AREA                    90351  00601702
         ORG   ,                                                 89365  00601802
DECOMPRS SERVCOMP DSECT=NO,PFX=DCM                                      00601902
         ORG   DCMFG1                                                   00602002
         DC    AL1(DCMF1EDT+DCMF1TSO)  LEFT EDIT LINE NUMBERS    81253  00602102
         ORG   DCMRECMX                                                 00602202
         DC    H'500'        SET FOR MAX NIH SIZE                       00602302
         ORG   ,                                                        00602402
LENWYLDC EQU   *-DECOMPRS    LENGTH FOR PATTERN MOVE             81222  00602502
         SPACE 1                                                        00602602
         IEFJFCBN ,          EXPAND JFCB IN-LINE                        00602702
         SPACE 1                                                        00602802
TRANSTAB DC    2XL128'0'     ASCII OR USER TRANSLATE TABLE       81253  00602902
         SPACE 1                                                 81253  00603002
TRTTAB   DC    16XL16'0'     TRT TABLE FOR SCAN                         00603102
         SPACE 1                                                 81222  00603202
SAVEEND  EQU   *                                                GP02338 00603302
         SPACE 1                                                        00603402
         DCBD  DSORG=PS      EXPAND DCB MAPPING                         00603502
         SPACE 1                                                        00603602
CVTDSECT DSECT ,                                                        00603702
         CVT   DSECT=YES                                                00603802
         SPACE 1                                                        00603902
         IHACDE ,                                                       00604002
         SPACE 1                                                        00604102
         IEFUCBOB ,                                                     00604202
         SPACE 1                                                        00604302
         IECSDSL1 1                                                     00604402
         SPACE 1                                                        00604502
FDSECT   FDSECT ,            MAP FD EXPANSIONS                          00604602
         SPACE 1                                                 90277  00604702
         IFGRPL ,                                                90277  00604802
         END   ,                                                        00604902
/*                                                                      00605000
//*------------------------------------------------------ ASM: PRINTALL 00605101
//*                                                                     00605200
//ASM2.SYSIN DD *                                                       00605300
SUBLPALK TITLE 'S U B L P A L K  ***  LOCATE LPA RESIDENT CODE'         00605402
         COPY  OPTIONGB                                                 00605502
         SPACE 1                                                        00605602
         SYSPARM LIST=YES                                               00605702
         SPACE 1                                                        00605802
*********************************************************************** 00605902
*                                                                     * 00606002
*                                                                     * 00606102
*        COPYRIGHT 2003  EXPERT SYSTEM PROGRAMMING                    * 00606202
*                        176 OLD STAGE COACH ROAD                     * 00606302
*                        BRADFORD, VT 05033-8844                      * 00606402
*                                                                     * 00606502
*                    ALL RIGHTS RESERVED                              * 00606602
*                                                                     * 00606702
*********************************************************************** 00606802
         EJECT ,                                                        00606902
*********************************************************************** 00607002
**                                                                   ** 00607102
**  SUBLPALK  LOCATES A MODULE RESIDING IN AN LPA AREA               ** 00607202
**                                                                   ** 00607302
**  PARAMETER:  R1 POINTING TO A CL8 AREA CONTAINING THE DESIRED     ** 00607402
**    MODULE NAME.                                                   ** 00607502
**                                                                   ** 00607602
**  WHEN USED WITH THE LOADLPA MACRO, R0 IS EITHER 0 OR A DCB        ** 00607702
**    ADDRESS. WHEN THE DCB ADDRESS IS SUPPLIED, AND THE MODULE IS   ** 00607802
**    NOT IN LPA, A LOAD WILL BE ISSUED.                             ** 00607902
**                                                                   ** 00608002
*********************************************************************** 00608102
**                                                                   ** 00608202
**  NORMAL RETURN:                                                   ** 00608302
**    R0 : MODULE ENTRY POINT (WITH X'80' ON IF AM31)                ** 00608402
**    R1 : LENGTH                                                    ** 00608502
**    R15: 0 (MAIN MODULE)   4 (ALIAS)                               ** 00608602
**   AR0 : MODULE LOAD POINT (IF AVAILABLE), ELSE SAME AS R0         ** 00608702
**                                                                   ** 00608802
**  ERROR RETURNS IN R15:                                            ** 00608902
**                                                                   ** 00609002
**  16 - ERROR PROCESSING PARAMETER LIST, OR OTHER SEVERE PROBLEM    ** 00609102
**   8 - ERROR CSVQUERY OR LOAD                                      ** 00609202
**                                                                   ** 00609302
*********************************************************************** 00609402
         SPACE 1                                                        00609502
         PRINT &PRTSOR                                                  00609602
SUBLPALK PGMHEAD ZERO31,BASE=R12,PARM=R1,AM=ANY,RM=24,LOC=BELOW         00609702
         STM   R0,R1,CALLR0  SAVE CALLER'S PARMS                        00609802
         SPACE 1                                                        00609902
         OICC  16,RESULT=RETCODE  PROVISIONAL RETURN CODE               00610002
         LA    R5,0(,R1)     DID USER PASS A PARM AREA ?                00610102
         LTR   R5,R5                                                    00610202
         BZ    PGMEXIT       NO; FATAL BOO-BOO                          00610302
         SPACE 1                                                        00610402
         L     R3,CVTPTR     GET CVT IN R3 FOR IEAVVMSR                 00610502
         USING CVTMAP,R3                                                00610602
         SPACE 1                                                        00610702
         AIF   (NOT &MVSESA).NOCSVQ                             GP04234 00610802
         CSVQUERY INEPNAME=(R5),OUTLENGTH=@LENGTH,OUTEPA=@ENTRY,       *00610902
               OUTLOADPT=@LOAD,OUTATTR1=DB,OUTATTR2=DB+1,OUTATTR3=DB+2,*00611002
               RETCODE=RETCODE,MF=(E,MYQUERY)                           00611102
         CH    R15,=H'8'     SUCCESSFUL ?                               00611202
         BNL   TESTOLD       NO                                         00611302
         TM    DB,X'02'      ALIAS ENTRY ?                              00611402
         BZ    NOTALIAS      NO                                         00611502
         MVICC 4             SET WARNING FLAG                           00611602
NOTALIAS TM    DB+1,X'30'    ATTR2 AM31 OR AMANY ?                      00611702
         BZ    LOADTWO       NO; RETURN                         GP03330 00611802
         OI    @ENTRY,X'80'  SET FOR AM31 PREFERRED                     00611902
*---------------------------------------------------------------------* 00612002
*  NOTE THAT CSVQUERY (AS ISSUED) WILL FIND A MODULE IN LPA, ETC. AS  * 00612102
*  WELL AS IN JPA.                                                    * 00612202
*  WHEN THE USER PASSES A NON-ZERO DCB PARAMETER AND THE MODULE IS IN * 00612302
*  JPA, WE MUST ISSUE A LOAD FOR IT BECAUSE CALLER WILL EVENTUALLY    * 00612402
*  ISSUE A DELETE FOR IT.                                             * 00612502
*---------------------------------------------------------------------* 00612602
LOADTWO  TM    DB+2,X'40'    FOUND IN JPA ?                     GP03330 00612702
         BZ    SETLOAD       NO; RETURN LPA, ETC. ADDRESS       GP03330 00612802
         ICM   R4,15,CALLR0  DID USER SUPPLY A DCB PARAMETER ?  GP03330 00612902
         BZ    SETLOAD       NO; JUST RETURN                    GP03330 00613002
         B     TESTLOAD      YES; DO EXTRA LOAD                 GP03330 00613102
         SPACE 1                                                        00613202
TESTOLD  CH    R15,=H'20'    UNAVAILABLE ON THIS SYSTEM ?               00613302
         BNE   TESTLOAD      NO; SEE WHETHER USER WANTS LOAD            00613402
.NOCSVQ  SPACE 1                                                GP04234 00613502
*---------------------------------------------------------------------* 00613602
*   SCAN THE MLPA FOR THE REQUESTED MODULE                            * 00613702
*---------------------------------------------------------------------* 00613802
         PUSH  USING                                                    00613902
MLPALOOK ICM   R0,15,0(R5)                                              00614002
         ICM   R1,15,4(R5)                                              00614102
         L     R15,CVTQLPAQ  GET MLPA CDE CHAIN                         00614202
         USING CDENTRY,R15                                              00614302
MLPALOOP ICM   R15,15,CDCHAIN   GET NEXT CDE; ELSE TRY PLPA             00614402
         BZ    PLPALOOK                                                 00614502
         C     R1,CDNAME+4   MATCH ?                                    00614602
         BNE   MLPALOOP      NO; TRY NEXT                               00614702
         C     R0,CDNAME     FULL MATCH ?                               00614802
         BNE   MLPALOOP      NO; TRY NEXT                               00614902
         B     FOUNDLP2      JOIN COMMON                                00615002
         POP   USING                                                    00615102
         SPACE 1                                                        00615202
*---------------------------------------------------------------------* 00615302
*   SCAN THE PLPA FOR THE REQUESTED MODULE                            * 00615402
*---------------------------------------------------------------------* 00615502
         PUSH  USING                                                    00615602
PLPALOOK L     R7,CVTLPDSR   IEAVVMSR                                   00615702
         ICM   R0,15,0(R5)                                              00615802
         ICM   R1,15,4(R5)                                              00615902
         BASR  R14,R7    NOTE THAT R7-R9 ARE CLOBBERED                  00616002
           B   FOUNDLPA      MODULE FOUND                               00616102
         XC    @ENTRY(8),@ENTRY    SHOW NOT FOUND                       00616202
         MVICC 8,RESULT=RETCODE  SET LEVEL 8 ERROR                      00616302
         B     TESTLOAD                                                 00616402
         SPACE 1                                                        00616502
*---------------------------------------------------------------------* 00616602
*   HAVE A CDE OR LPDE - EXTRACT LOAD ADDRESS AND SIZE                * 00616702
*---------------------------------------------------------------------* 00616802
         USING CDENTRY,R15                                              00616902
FOUNDLPA LR    R15,R0        COPY CDE ADDRESS                           00617002
FOUNDLP2 MVICC 0             RESET THE RETURN CODE              GP04234 00617102
         ICM   R0,15,CDENTPT  LOAD ENTRY ADDRESS                        00617202
         ST    R0,@ENTRY     RETURN ENTRY ADDRESS                       00617302
         ST    R0,@LOAD      SET AS LOAD ADDRESS, ALSO                  00617402
         AIF   (NOT &MVSESA).NOAMF                              GP04234 00617502
         TM    CDATTR2,CDEANYM  AM ANY ?                                00617602
         BZ    LOOKMIN       NO                                         00617702
         OI    @ENTRY,X'80'  SET AM31 PREFERRED                         00617802
.NOAMF   ANOP  ,                                                GP04234 00617902
LOOKMIN  TM    CDATTR,CDMIN  MINOR ?                                    00618002
         BZ    GETXTLST      NO; GET EXTENT LIST                        00618102
         OICC  4             INDICATE ALIAS ENTRY                       00618202
         TM    CDATTR2,CDXLE  EXTENT LIST PRESENT ?             GP05303 00618302
         BNZ   GETXTLST      YES; DON'T NEED MAJOR              GP05303 00618402
         ICM   R15,15,CDXLMJP  GET POINTER TO MAJOR                     00618502
         BNZ   LOOKMIN                                                  00618602
         B     SETLOAD       RESTORE REGS                               00618702
GETXTLST L     R14,CDXLMJP   GET EXTENT LIST ADDRESS                    00618802
         USING XTLST,R14                                                00618902
         MVC   @LOAD,XTLMSBAD LOAD ADDRESS                              00619002
         MVC   @LENGTH,XTLMSBLA  LENGTH                                 00619102
         NI    @LENGTH,255-X'80'  RESET END OF LIST BIT                 00619202
         B     SETLOAD       JOIN COMMON                                00619302
         POP   USING         RESTORE ASSIGNMENTS                        00619402
         SPACE 1                                                        00619502
*---------------------------------------------------------------------* 00619602
*   USER WANTS A LOAD ISSUED WHEN DCB IS NON-ZERO.                    * 00619702
*     WHEN DCB (R0)<256, THEN USE DCB=0 ON LOAD                       * 00619802
*---------------------------------------------------------------------* 00619902
TESTLOAD ICM   R4,15,CALLR0  DID USER SUPPLY A DCB PARAMETER ?          00620002
         BZ    SETLOAD       NO; JUST RETURN ERROR CODE                 00620102
         CH    R4,=H'256'    VALID DCB ?                                00620202
         BNL   *+4+2         PERHAPS                                    00620302
         SR    R4,R4         ELSE FLAG TO REQUEST LOAD                  00620402
         LOAD  DCB=(R4),EPLOC=(R5),ERRET=SETLOAD                        00620502
         STM   R0,R1,@ENTRY   RETURN ENTRY / LENGTH                     00620602
         XC    RETCODE,RETCODE    CLEAR RETURN                          00620702
         ICM   R14,15,@LOAD  IS LOAD ADDRESS SET ?              GP03330 00620802
         BNZ   SETLOAD       YES; RETURN IT                     GP03330 00620902
         LR    R14,R0        COPY ENTRY ADDRESS                 GP03330 00621002
         LA    R14,0(,R14)   CLEAN AM BIT                       GP03330 00621102
         ST    R14,@LOAD     SET TO RETURN ENTRY AS LOAD        GP03330 00621202
         SPACE 1                                                        00621302
*---------------------------------------------------------------------* 00621402
*   RETURN LOAD ADDRESS IN AR0                                        * 00621502
*---------------------------------------------------------------------* 00621602
SETLOAD  DS    0H                                               GP04234 00621702
         AIF   (NOT &MVSESA).NOLAM                              GP04234 00621802
         LAM   R0,R0,@LOAD   GET LOAD POINT                             00621902
.NOLAM   SPACE 1                                                GP04234 00622002
*---------------------------------------------------------------------* 00622102
*   EXIT: WHEN R15=0, ENTRY ADD IN R0, LENGTH IN R1, LOAD ADDR IN AR0 * 00622202
*     R15=8 MODULE NOT FOUND; R15=16 INVALID NAME ADDRESS             * 00622302
*---------------------------------------------------------------------* 00622402
PGMEXIT  PGMEXIT COPYRET=(RETCODE,12)   RETURN R15, R0, R1              00622502
         SPACE 1                                                        00622602
         LTORG ,                                                        00622702
         SPACE 2                                                        00622802
SAVE     DSECT ,             SAVE & WORK AREA                           00622902
DB       DS    D                                                        00623002
CALLR0   DS    A     1/2                                                00623102
CALLR1   DS    A     2/2                                                00623202
         SERVDEFS ,          EXPAND COMMON STUFF                        00623302
         ORG   RETCODE+4     RETURN CODE                                00623402
@ENTRY   DS    A     2/3     RETURN ENTRY POINT ADDRESS                 00623502
@LENGTH  DS    F     3/3     RETURN LENGTH                              00623602
@LOAD    DS    A             RETURN LOAD POINT ADDRESS                  00623702
         SPACE 1                                                        00623802
         AIF   (NOT &MVSESA).SKPCVSQ                            GP04234 00623902
         CSVQUERY MF=(L,MYQUERY)                                        00624002
.SKPCVSQ SPACE 1                                                GP04234 00624102
SAVEEND  EQU   *             END OF DYNAMIC AREA                        00624202
         SPACE 2                                                        00624302
         PRINT &PRTSYS                                                  00624402
         CVT   DSECT=YES                                                00624502
         IHACDE ,                                                       00624602
         IHAXTLST ,                                                     00624702
         END   ,                                                        00624802
/*                                                                      00624900
//*------------------------------------------------------ ASM: SUBLPALK 00625001
//*                                                                     00625100
//LKED.SYSIN DD *                                                       00625200
  NAME PRINTALL(R)                                                      00625301
/*                                                                      00625400
//*------------------------------------------------------ LKED          00625500
//*                                                                     00625600
//LOADJCL.SYSIN DD DATA,DLM='><'                                        00625700
./ ADD NAME=PRINTALL                                                    00625804
./ NUMBER NEW1=100,INCR=100                                             00625900
//PRINTALL JOB (SYS),'PRINTALL',CLASS=S,MSGCLASS=X                      00626002
//PRINTALL EXEC PGM=PRINTALL,REGION=4096K,PARM='10,,,V'                 00626102
//* --------------------------------------------------                  00626202
//* PARM values: n1,n2,n3,option characters                             00626302
//*   n1 = total number of records to print                             00626402
//*   n2 = ordinal number of first record to be printed                 00626502
//*   n3 = ordinal number of last record to be printed                  00626602
//*   option characters:                                                00626702
//*     L (default) - plain listing, 100 characters per line            00626802
//*     H - hexadecimal dump, 48 characters per line                    00626902
//*     D - hexadecimal and EBCDIC (side by side), 32 bytes per line    00627002
//*     V - vertical, top line EBCDIC, middle line Zones, bottom        00627102
//*         line numerics, 100 bytes per line                           00627202
//*     Y - horizontal, top line EBCDIC, bottom line hexadecimal,       00627302
//*         48 bytes per line                                           00627402
//*   processing options (default is block mode):                       00627502
//*     X - use EXCP access method (tape only)                          00627602
//*     K - keys treated as data (forces block mode)                    00627702
//*     R - process records rather than blocks, valid for RECFM=F, D,   00627802
//*         and V. For V format, segments are processed as records.     00627902
//*     S - same as R, except for RECFM=VS and VBS, where segments      00628002
//*         are consolidated (up to 32K) into single records            00628102
//*     I - with R and S, RDW/SDW are printed with data                 00628202
//*     Q - force ASCII translate regardless of OPTCD                   00628302
//*     A - converts printable to ASCII, but not hex data               00628402
//* --------------------------------------------------                  00628502
//SYSPRINT DD  SYSOUT=*                                                 00628602
//SYSUT1   DD DISP=SHR,DSN=SYS1.LOGREC            FIRST FILE TO PRINT   00628702
//SYSUT2   DD DISP=SHR,DSN=SYS1.BRODCAST          SECOND FILE TO PRINT  00628802
//* --------------------------------------------------                  00628902
//* SCAN requests supplied in (optional) SYSIN, one per card            00629002
//* Free format:                                                        00629102
//*   'string' (col,col)                                                00629202
//*                                                                     00629302
//* where: 'string' may be 'string', "string", or X'string'             00629402
//*        (col,col) may be omitted (all bytes scanned)                 00629502
//*        (col) requests a single column test                          00629602
//*        (col,col) specifies the start/end columns in which the       00629702
//*                  first byte of the string may appear                00629802
//* --------------------------------------------------                  00629902
//*SYSIN    DD *                                                        00630002
//                                                                      00630102
./ ENDUP                                                                00630200
><                                                                      00630300
/*                                                                      00630400
//*------------------------------------------------------ LOADJCL       00630500
//                                                                      00630600

