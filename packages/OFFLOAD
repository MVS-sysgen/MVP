//OFFLOAD  JOB (TSO),
//             'Install OFFLOAD',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*          DATA SET CBT880     AT LEVEL 002 AS OF 10/28/84            00020000
//*                                                                     00030000
//***********************************************************           00040000
//*                                                         *           00050000
//*      SAMPLE JCL TO ASSEMBLE AND LINKEDIT THE OFFLOAD    *           00060000
//*      PROGRAM.                                           *           00070000
//*                                                         *           00080000
//***********************************************************           00090000
//*                                                                     00100000
//ASM     EXEC PGM=IFOX00,REGION=1024K,                                 00110000
//             PARM=(TERM,LOAD,NODECK)                                  00120000
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR                                 00130000
//         DD  DSN=SYS1.AMODGEN,DISP=SHR                                00140000
//*                                                                     00150000
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSSQ,SPACE=(CYL,(15,5)),              00160000
//             DISP=(,PASS)                                             00170000
//SYSUT2   DD  UNIT=SYSSQ,SPACE=(CYL,(15,5))                            00180000
//SYSUT3   DD  UNIT=SYSSQ,SPACE=(CYL,(15,5))                            00190000
//SYSTERM  DD  SYSOUT=*                                                 00200000
//SYSPRINT DD  SYSOUT=*,DCB=BLKSIZE=1089                                00210000
//SYSGO    DD  DSN=&&OBJSET,UNIT=SYSSQ,SPACE=(CYL,(1,1),RLSE),          00220000
//             DISP=(MOD,PASS)                                          00230000
//SYSIN    DD  *                                                        00240000
         MACRO                                                          00000100
         #REGS &GEN=YES                                                 00000200
.*                                                                      00000300
.*                                                                      00000400
.*                                                            09/84 DBC 00000500
.* LAST CHANGE DATE - SEPTEMBER 11, 1984                      09/84 DBC 00000600
.*                  - ADDED SUPPORT FOR PL/S STYLE REGISTER   09/84 DBC 00000700
.*                    NAMES (@00, @01, ---, @15).             09/84 DBC 00000800
.*                  - ATTEMPTS TO MULTIPLY DEFINE THE SAME    09/84 DBC 00000900
.*                    NAME TO THE SAME VALUE WILL NOW BE      09/84 DBC 00001000
.*                    SUPPRESSED WITHOUT ERROR.               09/84 DBC 00001100
.*                                                                      00001200
.* LAST CHANGE DATE - OCTOBER 18, 1983                                  00001300
.*                  - MAILING ADDRESS CHANGE                            00001400
.*                                                                      00001500
.* LAST CHANGE DATE - APRIL 21, 1981                                    00001600
.*                  - MACRO NAME CHANGED FROM $REGS TO #REGS            00001700
.*                                                                      00001800
.* LAST CHANGE DATE - APRIL 20, 1981                                    00001900
.*                  - SUPPORT FOR THE "GEN={YES|NO}" OPERAND IS ADDED.  00002000
.*                                                                      00002100
.* LAST CHANGE DATE - DECEMBER 5, 1977                                  00002200
.*                  - SINGLE REGISTER EQUATES NOW LINE UP CORRECTLY IN  00002300
.*                    THE LISTING.                                      00002400
.*                                                                      00002500
.* LAST CHANGE DATE - FEBRUARY 2, 1977                                  00002600
.*                  - MAILING ADDRESS CHANGE.                           00002700
.*                                                                      00002800
.* LAST CHANGE DATE - APRIL 1, 1975                                     00002900
.*                                                                      00003000
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING   00003100
.* IT MAY BE ADDRESSED TO:                                              00003200
.*       RR#2 BOX 712                                                   00003300
.*       AFTON, VA. 22920                                               00003400
.*                                                                      00003500
.*                                                                      00003600
.*                                                                      00003700
.*   THE #REGS MACRO HAS TWO FUNCTIONS. ITS PRIMARY                     00003800
.* PURPOSE IS TO PROVIDE A SIMPLE MEANS OF DEFINING SETS                00003900
.* OF REGISTER NAME EQUATES. ITS SECONDARY PURPOSE IS TO                00004000
.* CREATE AN INTERNAL TABLE OF ALL REGISTER NAMES                       00004100
.* COUPLED WITH THEIR NUMERIC VALUES. THIS TABLE IS THEN                00004200
.* MADE AVAILABLE TO CERTAIN OTHER MACROS (E.G. #ENTER                  00004300
.* AND #EXIT) SO THAT THEY CAN DETERMINE (IF NECESSARY)                 00004400
.* THE VALUES OF ANY REGISTER NAMES THAT MIGHT BE PASSED                00004500
.* TO THEM AS OPERANDS AND SO THAT THEY CAN USE REGISTER                00004600
.* NAME EQUATES INSTEAD OF REGISTER NUMBERS IN THE CODE                 00004700
.* THAT THEY GENERATE. THIS IS SO THAT ALL REFERENCES TO                00004800
.* REGISTERS WILL BE INDICATED IN THE ASSEMBLER'S CROSS                 00004900
.* REFERENCE LISTING.                                                   00005000
.*                                                            09/84 DBC 00005100
.*   THE #REGS MACRO CAN BE USED ANY NUMBER OF TIMES IN                 00005200
.* AN ASSEMBLY. EACH TIME THAT IT IS USED, IT CAN BE                    00005300
.* GIVEN ANY NUMBER OF POSITIONAL OPERANDS. EACH OPERAND      09/84 DBC 00005400
.* CAN BE EITHER A SINGLE TERM OR A SUB-LIST OF TWO                     00005500
.* TERMS.                                                               00005600
.*                                                            09/84 DBC 00005700
.*   IF AN OPERAND IS A SUB-LIST OF TWO TERMS, THEN THE                 00005800
.* #REGS MACRO TREATS IT AS A REQUEST TO DEFINE A SINGLE                00005900
.* REGISTER NAME AND IT GENERATES A STATEMENT OF THE                    00006000
.* FORM: " TERM1 EQU TERM2 ". THE FIRST TERM MUST BE ANY                00006100
.* VALID NAME NOT PREVIOUSLY DEFINED. THE SECOND TERM                   00006200
.* MUST BE ANY SELF-DEFINING TERM OR ANY REGISTER NAME                  00006300
.* THAT HAS BEEN PREVIOUSLY DEFINED BY THIS OR A                        00006400
.* PREVIOUS #REGS MACRO. IT SHOULD NOT BE AN EXPRESSION,                00006500
.* AND IT SHOULD NOT BE ANY NAME NOT PREVIOUSLY DEFINED.                00006600
.* THE VALUE OF THE SECOND TERM SHOULD FALL IN THE RANGE                00006700
.* OF 0 THROUGH 15. IF THE SECOND TERM FITS THESE                       00006800
.* REQUIREMENTS, THEN THE REGISTER NAME IS SAVED IN AN                  00006900
.* INTERNAL TABLE FOR USE BY OTHER MACROS.                              00007000
.*                                                            09/84 DBC 00007100
.*   IF AN OPERAND IS ONLY A SINGLE TERM, THEN THE MACRO                00007200
.* TREATS IT AS A REQUEST TO DEFINE A FULL SET OF                       00007300
.* REGISTER NAME EQUATES WITH THE GIVEN TERM USED AS THE                00007400
.* REGISTER NAME PREFIX. AS AN EXAMPLE, ASSUME THAT THE                 00007500
.* OPERAND IS "GPR". IN THIS CASE, THE #REGS MACRO WILL                 00007600
.* GENERATE EQUATES DEFINING GPR0, GPR1, ---, GPR15 AND                 00007700
.* GPRA, GPRB, ---, GPRF (EQUAVALENT TO GPR10, GPR11,                   00007800
.* ---, GPR15). IN ADDITION, THE GENERATED REGISTER                     00007900
.* NAMES ARE SAVED IN AN INTERNAL TABLE FOR USE BY OTHER                00008000
.* MACROS.                                                              00008100
.*                                                            09/84 DBC 00008200
.*   A SPECIAL CASE. IF THE SINGLE TERM IS AN "AT SIGN"       09/84 DBC 00008300
.* (@), THEN THEN THE GENERATED NAMES WILL BE @00, @01,       09/84 DBC 00008400
.* ---, @15. THIS CONFORMS TO PL/S CONVENTIONS.               09/84 DBC 00008500
.*                                                            09/84 DBC 00008600
.*   IF #REGS IS CALLED WITHOUT OPERANDS, THEN IT IS                    00008700
.* TREATED AS A REQUEST TO GENERATE A FULL SET OF                       00008800
.* EQUATES USING "R" AS THE PREFIX.                                     00008900
.*                                                                      00009000
.*                                                                      00009100
.*                                                                      00009200
.* GEN={YES|NO}    (DEFAULT IS GEN=YES)                                 00009300
.*       THIS CONTROLS WHETHER OR NOT THIS MACRO ACTUALLY GENERATES THE 00009400
.*       'EQU' STATEMENTS THAT CREATE THE DESIRED REGISTER NAMES. IF    00009500
.*       "GEN=NO" IS GIVEN, THEN PRESUMEDLY THE DESIRED NAMES ARE       00009600
.*       GENERATED ELSEWHERE. IN THIS CASE THE ONLY FUNCTION PERFORMED  00009700
.*       BY THIS MACRO IS TO UPDATE INTERNAL TABLES.                    00009800
.*                                                                      00009900
.*                                                                      00010000
.*                                                                      00010100
.* INNER MACROS USED - #TEST                                            00010200
.*                                                                      00010300
         GBLA  &#REGVAL(255)                                            00010400
         GBLA  &#TESERR                                                 00010500
         GBLC  &#REGNME(255)                                            00010600
         GBLC  &#TESRET(20)                                             00010700
         LCLA  &ARG,&CTR,&NEXT,&A1                                      00010800
         LCLB  &B1                                                      00010900
         LCLC  &LPFX,&C1                                                00011000
&NEXT    SETA  0                                                        00011100
.LP1     AIF   (&NEXT GE 255).END1                                      00011200
&NEXT    SETA  &NEXT+1                                                  00011300
         AIF   ('&#REGNME(&NEXT)' NE '').LP1                            00011400
&NEXT    SETA  &NEXT-1                                                  00011500
.END1    ANOP                                                           00011600
&ARG     SETA  0                                                        00011700
.LP2     AIF   (&ARG GE N'&SYSLIST).DONE                                00011800
&ARG     SETA  &ARG+1                                                   00011900
         AIF   (N'&SYSLIST(&ARG) EQ 0).LP2                              00012000
         AIF   (&NEXT LT 255).NOTFULL                                   00012100
         MNOTE 4,'THE REGISTER NAME SAVE TABLE IS FULL.'                00012200
         MNOTE 4,'THE MAXIMUM CAPACITY IS 255 ENTRIES.'                 00012300
.NOTFULL ANOP                                                           00012400
&C1      SETC  '&SYSLIST(&ARG,1)'                                       00012500
         AIF   (N'&SYSLIST(&ARG) GE 2).ONEREG                           00012600
.NULL    ANOP                                                           00012700
&B1      SETB  (1)                                                      00012800
         #TEST PFIX=                                                    00012900
&LPFX    SETC  '&#TESRET(1)'                                            00013000
.*                                                            09/84 DBC 00013100
         AIF   ('&C1' NE '@').NOT@                            09/84 DBC 00013200
&CTR     SETA  0-1                                            09/84 DBC 00013300
.LP@     AIF   (&CTR EQ 15).END@                              09/84 DBC 00013400
&CTR     SETA  &CTR+1                                         09/84 DBC 00013500
&C1      SETC  '0&CTR'                                        09/84 DBC 00013600
&C1      SETC  '&C1'(K'&C1-1,2)                               09/84 DBC 00013700
         #REGS (@&C1,&LPFX&CTR)                               09/84 DBC 00013800
         AGO   .LP@                                           09/84 DBC 00013900
.END@    AIF   (&NEXT GE 255).LP2                             09/84 DBC 00014000
&NEXT    SETA  &NEXT+1                                        09/84 DBC 00014100
         AIF   ('&#REGNME(&NEXT)' NE '').END@                 09/84 DBC 00014200
&NEXT    SETA  &NEXT-1                                        09/84 DBC 00014300
         AGO   .LP2                                           09/84 DBC 00014400
.NOT@    ANOP                                                 09/84 DBC 00014500
.*                                                            09/84 DBC 00014600
&CTR     SETA  0                                                        00014700
.LP2A    AIF   (&CTR GE &NEXT).PXSAVE                                   00014800
&CTR     SETA  &CTR+1                                                   00014900
         AIF   (&#REGVAL(&CTR) LT 16 OR '&#REGNME(&CTR)' NE '&C1').LP2A 00015000
         AGO   .LP2                                           09/84 DBC 00015100
.PXSAVE  AIF   (&NEXT GE 255).NOSAVE1                         09/84 DBC 00015200
&NEXT    SETA  &NEXT+1                                                  00015300
&#REGNME(&NEXT) SETC '&C1'                                              00015400
&#REGVAL(&NEXT) SETA 16                                                 00015500
.NOSAVE1 AIF   ('&GEN(1)'(1,1) NE 'Y').LP2                              00015600
&CTR     SETA  0                                                        00015700
.LP3     AIF   (&CTR GT 15).HEX                                         00015800
&C1&CTR  EQU   &LPFX&CTR                                                00015900
&CTR     SETA  &CTR+1                                                   00016000
         AGO   .LP3                                                     00016100
.HEX     ANOP                                                           00016200
&C1.A    EQU   &C1.10                                                   00016300
&C1.B    EQU   &C1.11                                                   00016400
&C1.C    EQU   &C1.12                                                   00016500
&C1.D    EQU   &C1.13                                                   00016600
&C1.E    EQU   &C1.14                                                   00016700
&C1.F    EQU   &C1.15                                                   00016800
         AGO   .LP2                                                     00016900
.ONEREG  ANOP                                                           00017000
&B1      SETB  (1)                                                      00017100
         AIF   (N'&SYSLIST(&ARG) EQ 2).NOXCESS                          00017200
         MNOTE 4,'"&SYSLIST(&ARG)" CONTAINS EXCESS INFORMATION.'        00017300
         MNOTE 4,'THE EXCESS WILL BE IGNORED.'                          00017400
.NOXCESS #TEST REGS=&SYSLIST(&ARG,2)                                    00017500
         AIF   (&#TESERR EQ 0).REGOK                                    00017600
         MNOTE 4,'THE VALUE OF "&SYSLIST(&ARG,2)" IS NOT DETERMINABLE.' 00017700
         AGO   .REGEQU                                                  00017800
.REGOK   ANOP                                                 09/84 DBC 00017900
&A1      SETA  &#TESRET(1)                                              00018000
&CTR     SETA  0                                                        00018100
.LP3A    AIF   (&CTR GE &NEXT).RGSAVE                                   00018200
&CTR     SETA  &CTR+1                                                   00018300
         AIF   (&#REGVAL(&CTR) NE &A1 OR '&#REGNME(&CTR)' NE '&C1').LP3*00018400
               A                                                        00018500
         AGO   .LP2                                           09/84 DBC 00018600
.RGSAVE  AIF   (&NEXT GE 255).REGEQU                          09/84 DBC 00018700
&NEXT    SETA  &NEXT+1                                                  00018800
&#REGNME(&NEXT) SETC '&C1'                                              00018900
&#REGVAL(&NEXT) SETA &A1                                                00019000
.REGEQU  AIF   ('&GEN(1)'(1,1) NE 'Y').LP2                              00019100
&C1      EQU   &SYSLIST(&ARG,2)                                         00019200
         AGO   .LP2                                                     00019300
.DONE    ANOP                                                           00019400
&C1      SETC  'R'                                                      00019500
         AIF   (NOT &B1).NULL                                           00019600
         MEND                                                           00019700
         MACRO                                                          00000001
&N       #ENTER &NME,&ESDTYPE=DEFAULT,&BASES=1,&SAVTYPE=LOCAL,&PFIX=    00000002
.*                                                                      00000003
.*                                                                      00000004
.*                                                            08/84 DBC 00000005
.* LAST CHANGE DATE - AUGUST 8, 1984                          08/84 DBC 00000006
.*                  - WHEN BASES=* IS SPECIFIED, #ENTER NOW   08/84 DBC 00000007
.*                    DERIVES THE DESIRED BASE ADDRESS BY     08/84 DBC 00000008
.*                    SUBTRACTING AN OFFSET FROM THE          08/84 DBC 00000009
.*                    CURRENT ENTRY ADDRESS. PREVIOUSLY, IT   08/84 DBC 00000010
.*                    WAS JUST LOADING AN ADCON FOR THE       08/84 DBC 00000011
.*                    DESIRED BASE ADDRESS. THIS CAUSED       08/84 DBC 00000012
.*                    PROBLEMS IF THE #ENTER MACRO WAS        08/84 DBC 00000013
.*                    LOCATED WITHIN DYNAMICALLY RELOCATED    08/84 DBC 00000014
.*                    CODE.                                   08/84 DBC 00000015
.*                  - SIMILARLY, WHEN SAVTYPE=(REMOTE,NME%)   08/84 DBC 00000016
.*                    IS SPECIFIED, THE DESIRED SAVE AREA     08/84 DBC 00000017
.*                    IS LOCATED BY ADDING AN OFFSET (WHICH   08/84 DBC 00000018
.*                    MAY BE NEGATIVE) TO THE CURRENT ENTRY   08/84 DBC 00000019
.*                    ADDRESS.                                08/84 DBC 00000020
.*                                                            06/84 DBC 00000021
.* LAST CHANGE DATE - JUNE 11, 1984                           06/84 DBC 00000022
.*                  - ADDED "SAVTYPE=NONE" SUPPORT.           06/84 DBC 00000023
.*                  - WHEN A REMOTE SAVE AREA WAS USED,       06/84 DBC 00000024
.*                    #ENTER USE TO GENERATE A "USING"        06/84 DBC 00000025
.*                    STATEMENT DECLARING R13 AS A BASE FOR   06/84 DBC 00000026
.*                    THAT SAVE AREA. THAT "USING" STATEMENT  06/84 DBC 00000027
.*                    IS NO LONGER GENERATED.                 06/84 DBC 00000028
.*                                                                      00000029
.* LAST CHANGE DATE - OCTOBER 18, 1983                                  00000030
.*                  - MAILING ADDRESS CHANGE                            00000031
.*                                                                      00000032
.* LAST CHANGE DATE - APRIL 27, 1983                                    00000033
.*                  - MAILING ADDRESS CHANGE.                           00000034
.*                  - USE OF IBM'S "SAVE" MACRO HAS BEEN                00000035
.*                    REPLACED BY LOCAL CODE.                           00000036
.*                  - THE ASSEMBLY DATE AND TIME ARE NOW                00000037
.*                    INCLUDED IN THE MODULE IDENTIFIER                 00000038
.*                    TEXT.                                             00000039
.*                                                                      00000040
.* LAST CHANGE DATE - APRIL 21, 1981                                    00000041
.*                  - MACRO NAME CHANGED FROM $ENTER TO #ENTER.         00000042
.*                                                                      00000043
.* LAST CHANGE DATE - APRIL 15, 1981                                    00000044
.*                  - ADDED ENTRY LINKAGE FOR A PLI ENVIRONMENT.        00000045
.*                  - ADDED "#REGS GEN=NO" SUPPORT.                     00000046
.*                                                                      00000047
.* LAST CHANGE DATE - JULY 18, 1980                                     00000048
.*                  - BUG FIXED: THE PRECEEDING MODIFICATION INTRODUCED 00000049
.*                    AN ERROR WHICH UNDER CERTAIN CIRCUMSTANCES        00000050
.*                    GENERATED ASSEMBLY ERRORS.                        00000051
.*                                                                      00000052
.* LAST CHANGE DATE - JULY 10, 1980                                     00000053
.*                  - FOR GETMAINED REENTRANT SAVE AREAS, CODE HAS BEEN 00000054
.*                    ADDED TO CLEAR THE ENTIRE GETMAINED AREA TO ZEROS 00000055
.*                    BEFORE SETING THE CHAIN FIELD.                    00000056
.*                  - INDIRECT ADDRESSING TO A REMOTE SAVE AREA IS NOW  00000057
.*                    SIGNALLED BY A TRAILING PERCENT SIGN RATHER THAN  00000058
.*                    A LEADING PERCENT SIGN.                           00000059
.*                                                                      00000060
.* LAST CHANGE DATE - OCTOBER 3, 1979                                   00000061
.*                  - CODE HAS BEEN ALTERED SO THAT ADDRESSABILITY TO   00000062
.*                    A REMOTE SAVE AREA DOES NOT HAVE TO BE BASED ON   00000063
.*                    R15 (I.E., ON THE ENTRY ADDRESS).                 00000064
.*                                                                      00000065
.* LAST CHANGE DATE - OCTOBER 3, 1978                                   00000066
.*                  - THE GETMAIN FOR THE RENTRANT SAVE AREA HAS BEEN   00000067
.*                    CHANGED SO THAT MORE THAN 4K BYTES CAN BE GOTTEN. 00000068
.*                                                                      00000069
.* LAST CHANGE DATE - FEBRUARY 28, 1978                                 00000070
.*                  - BUG FIXED IN REMOTE SAVE AREA HANDLING            00000071
.*                                                                      00000072
.* LAST CHANGE DATE - JANUARY 29, 1978                                  00000073
.*                  - IN MOST CASES IT IS LOGICALLY INCONSISTANT TO     00000074
.*                    CODE 'BASES=*' WHEN ONE OF THE OLD BASES IS R13.  00000075
.*                    THIS PROBLEM IS NOW RECOGNIZED AND FLAGGED.       00000076
.*                                                                      00000077
.*                  - A REMOTE SAVE AREA'S NAME CAN NOW BE GIVEN EITHER 00000078
.*                    WITH OR WITHOUT A PRECEEDING PERCENT (%) SIGN TO  00000079
.*                    INDICATE WHETHER THE NAMED ADDRESS MUST BE        00000080
.*                    REACHED BY INDIRECT ADDRESSING.                   00000081
.*                                                                      00000082
.* LAST CHANGE DATE - NOVEMBER 4, 1977                                  00000083
.*                  - SUPPORT IS ADDED FOR DEFINING A LOCAL SAVE AREA   00000084
.*                    WHOSE LENGTH IS OTHER THAN 72 BYTES.              00000085
.*                                                                      00000086
.* LAST CHANGE DATE - JANUARY 13, 1977                                  00000087
.*                  - THE MF= AND SVID= OPERANDS ARE REPLACED BY THE    00000088
.*                    SAVTYPE= OPERAND.                                 00000089
.*                  - SUPPORT FOR THE HANDLING OF A REMOTELY ASSEMBLED  00000090
.*                    SAVE AREA.                                        00000091
.*                  - MAILING ADDRESS CHANGE.                           00000092
.*                                                                      00000093
.* LAST CHANGE DATE - SEPTEMBER 14, 1976                                00000094
.*                  - IMPLEMENT SUPPORT FOR "BASES=*" WHICH IMPLIES     00000095
.*                    THAT BOTH THE BASE ADDRESS AND BASE REGISTERS     00000096
.*                    DEFINED BY THE PHYSICALLY PREVIOUS USE OF THE     00000097
.*                    #ENTER MACRO ARE TO BE REUSED.                    00000098
.*                                                                      00000099
.* LAST CHANGE DATE - AUGUST 23, 1976                                   00000100
.*                                                                      00000101
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING   00000102
.* IT MAY BE ADDRESSED TO:                                              00000103
.*       RR#2 BOX 712                                                   00000104
.*       AFTON, VA. 22920                                               00000105
.*                                                                      00000106
.*                                                                      00000107
.*                                                                      00000108
.*   THIS MACRO GENERATES OS STANDARD ENTRY LINKAGE. IT WAS WRITTEN TO  00000109
.* PROVIDE A SIMPLE, FLEXIBLE, AND COMPLETE METHOD OF GENERATING SUCH   00000110
.* LINKAGE. IN ADDITION, FOR ALMOST ALL OPERAND COMBINATIONS, #ENTER    00000111
.* WILL GENERATE THE ABSOLUTE MINIMUM AMOUNT OF CODE NECESSARY.         00000112
.*                                                                      00000113
.*   THE #ENTER MACRO WILL GENERATE THE FOLLOWING:                      00000114
.*     - A CSECT OR ENTRY CARD (IF DESIRED)                             00000115
.*     - A MODULE IDENTIFIER WHICH WILL INCLUDE THE ASSEMBLY            00000116
.*       DATE AND TIME.                                                 00000117
.*     - CODE TO SAVE ALL REGISTERS IN THE HIGHER SAVE AREA             00000118
.*     - CODE TO LOAD R13 WITH A POINTER TO A LOWER SAVE AREA           00000119
.*     - CODE TO CROSS LINK THE LOWER SAVE AREA WITH THE HIGHER SAVE    00000120
.*       AREA                                                           00000121
.*     - CODE TO LOAD ANY SET OF BASE REGISTERS                         00000122
.*     - A USING STATEMENT DECLARING THE SET OF BASE REGISTERS          00000123
.*     - EITHER THE LOWER SAVE AREA ITSELF OR CODE TO GETMAIN A         00000124
.*       REENTRANT SAVE AREA OF 72 OR MORE BYTES OR CODE TO LOAD THE    00000125
.*       ADDRESS OF AN ASSEMBLED SAVE AREA THAT IS REMOTE FROM THE      00000126
.*       MACRO EXPANSION.                                               00000127
.*     - FOR A GETMAINED REENTRANT SAVE AREA, CODE TO CLEAR THE AREA TO 00000128
.*       ZEROS.                                                         00000129
.*                                                                      00000130
.* &N                                                                   00000131
.*       THIS IS THE ONLY FIELD REQUIRED FOR THE MACRO CALL. IT MUST    00000132
.*       SPECIFY THE DESIRED CONTROL SECTION OR ENTRY NAME.             00000133
.*                                                                      00000134
.* &NME                                                                 00000135
.*       USE THIS FIELD TO SPECIFY OPTIONAL TEXT TO BE                  00000136
.*       INCLUDED INTO THE MODULE IDENTIFIER. ENCLOSING                 00000137
.*       QUOTES ARE OPTIONAL.                                           00000138
.*                                                                      00000139
.* &ESDTYPE=                                                            00000140
.*       THIS OPERAND CONTROLS THE TYPE OF EXTERNAL SYMBOL (IF ANY) TO  00000141
.*       BE GENERATED USING &N. VALID VALUES FOR THIS OPERAND IMPLY THE 00000142
.*       FOLLOWING:                                                     00000143
.*              -OMITTED-    ==> ESDTYPE=ENTRY IF SAVTYPE=PLI           00000144
.*              -OMITTED-    ==> ESDTYPE=CSECT OTHERWISE                00000145
.*             ESDTYPE=CSECT ==>                                        00000146
.*                     &N       CSECT                                   00000147
.*             ESDTYPE=START ==>                                        00000148
.*                     &N       START                                   00000149
.*             ESDTYPE=ENTRY ==>                                        00000150
.*                              ENTRY &N                                00000151
.*                     &N       DS    0H                                00000152
.*             ESDTYPE=     <==>                                        00000153
.*             ESDTYPE=NONE  ==>                                        00000154
.*                     &N       DS    0H                                00000155
.*                                                                      00000156
.* &BASES=                                                              00000157
.*       USE THIS OPERAND TO SPECIFY EITHER HOW MANY OR EXACTLY WHICH   00000158
.*       BASE REGISTERS TO DECLARE AND LOAD. VALID VALUES FOR THIS      00000159
.*       OPERAND ARE:                                                   00000160
.*             BASES= -A SINGLE SELF DEFINING NUMERIC-                  00000161
.*                   THIS REQUESTS THAT A SPECIFIC NUMBER OF BASES BE   00000162
.*                   LOADED AND DECLARED. THE MACRO IS ALLOWED TO       00000163
.*                   DETERMINE FOR ITSELF PRECISELY WHICH REGISERS TO   00000164
.*                   DECLARE AS FOLLOWS. FOR SAVTYPE=LOCAL (SEE BELOW)  00000165
.*                   THE FIRST BASE REGISTER WILL BE R13; OTHERWISE     00000166
.*                   (I.E. FOR SAVTYPE=RENT OR SAVTYPE=REMOTE), THE     00000167
.*                   FIRST BASE REGISTER WILL BE R12. IN EITHER CASE,   00000168
.*                   ADDITIONAL BASES WILL BE SUCCESSIVELY LOWER        00000169
.*                   NUMBERED REGISTERS. EXAMPLES:                      00000170
.*                   BASES=3,SAVTYPE=RENT  ==> R12, R11, AND R10.       00000171
.*                   BASES=2,SAVTYPE=LOCAL ==> R13, AND R12.            00000172
.*             BASES= -A SUBLIST OF ONE OR MORE REGISTER NAMES-         00000173
.*                   THE LISTED REGISTERS ARE LOADED AND DECLARED AS    00000174
.*                   BASES. THE LEFTMOST LISTED REGISTER IS LOADED WITH 00000175
.*                   THE LOWEST ADDRESS. EXAMPLE:                       00000176
.*                   BASES=(R5,6,4) ==> R5, R6, AND R4 IN THAT ORDER.   00000177
.*             BASES=*                                                  00000178
.*                   THE BASE ADDRESS AND BASE REGISTERS DEFINED BY THE 00000179
.*                   PHYSICALLY PREVIOUS #ENTER MACRO ARE REUSED.       00000180
.*          THE BASES= OPERAND MAY BE NULLIFIED BY SPECIFYING EITHER:   00000181
.*             BASES=                                                   00000182
.*             BASES=0                                                  00000183
.*       IN THIS CASE, NO BASE REGISTERS ARE LOADED OR DECLARED.        00000184
.*          IF THE BASES= OPERAND IS OMITTED, THEN A DEFAULT OF BASES=1 00000185
.*       WILL BE USED.                                                  00000186
.*                                                                      00000187
.* &SAVTYPE=                                                            00000188
.*       THIS OPERAND IDENTIFIES THE TYPE OR LOCATION OF THE SAVE AREA  00000189
.*       TO BE GENERATED OR USED. VALID VALUES ARE:                     00000190
.*             -OMITTED-                                                00000191
.*             SAVTYPE=                                                 00000192
.*             SAVTYPE=LOCAL                                            00000193
.*             SAVTYPE=(,-SAVE AREA NAME-)                              00000194
.*             SAVTYPE=(LOCAL,-SAVE AREA NAME-)                         00000195
.*             SAVTYPE=(LOCAL,,-SAVE AREA LENGTH-)                      00000196
.*             SAVTYPE=(LOCAL,-SAVE AREA NAME-,-SAVE AREA LENGTH-)      00000197
.*                     A STANDARD SAVE AREA IS GENERATED IN THE         00000198
.*                     THE MACRO EXPANSION AND ITS ADDRESS IS LOADED    00000199
.*                     INTO R13. NOTE, DEPENDING UPON THE BASES=        00000200
.*                     OPERAND (SEE ABOVE) R13 MAY ALSO BE DECLARED AS  00000201
.*                     A PROGRAM BASE.                                  00000202
.*                        IF A -SAVE AREA NAME- IS GIVEN, THEN IT IS    00000203
.*                     USED TO LABEL THE SAVE AREA; OTHERWISE, AN       00000204
.*                     INTERNAL NAME IS GENERATED.                      00000205
.*                              IF -SAVE AREA LENGTH- IS GIVEN, THEN IT 00000206
.*                              IS USED TO SET THE LENGTH OF THE SAVE   00000207
.*                              AREA; OTHERWISE, THE DEFAULT LENGTH OF  00000208
.*                              72 BYTES IS USED.                       00000209
.*             SAVTYPE=(REMOTE,-SAVE AREA ADDRESS-)                     00000210
.*                     THE ADDRESS OF THE REMOTE SAVE AREA IS           00000211
.*                     LOADED INTO R13. NOTE, IN THIS CASE -SAVE AREA   00000212
.*                     ADDRESS- IS A REQUIRED SUB-OPERAND. IT MAY BE    00000213
.*                     EITHER AN ADDRESS LABEL OR A PARENTHESIZED       00000214
.*                     REGISTER NAME OR AN ADDRESS LABEL FOLLOWED BY A  00000215
.*                     PERCENT (%) SIGN. IF THE NAME IS JUST AN ADDRESS 00000216
.*                     LABEL, THEN A 'LA' INSTRUCTION IS USED TO LOAD   00000217
.*                     THE SAVE AREA'S ADDRESS. IF A PERCENT SIGN       00000218
.*                     FOLLOWS THE NAME, THEN AN ADDRESS CONSTANT IS    00000219
.*                     GENERATED AND A 'L' INSTRUCTION IS USED. IF A    00000220
.*                     REGISTER NAME IS GIVEN, THEN A 'LR' INSTRUCTION  00000221
.*                     IS USED UNLESS THE MACRO CAN DETERMINE THAT THE  00000222
.*                     NAMED REGISTER IS ACTUALLY R13 IN WHICH CASE IT  00000223
.*                     IS ASSUMED THAT THE LOWER SAVE AREA IS ALREADY   00000224
.*                     PRESENT AND INITIALIZED, SO THE SAVING OF        00000225
.*                     REGISTERS AND THE CROSS-CHAINING OF THE SAVE     00000226
.*                     AREAS IS BYPASSED.                               00000227
.*             SAVTYPE=RENT                                             00000228
.*             SAVTYPE=(RENT,(-LENGTH-,-SUBPOOL-),-ERROR ADDRESS-)      00000229
.*             SAVTYPE=(RENT,(-LENGTH-,-SUBPOOL-),RETURN)               00000230
.*                     THE MACRO EXPANSION IS TO BE REENTRANT. THE SAVE 00000231
.*                     AREA IS TO BE GETMAINED. THE SUB-OPERANDS HAVE   00000232
.*                     THE FOLLOWING AFFECT:                            00000233
.*                     -LENGTH- IS OPTIONAL. IF OMITTED, THEN A VALUE   00000234
.*                              OF 72 IS USED. IF GIVEN, THEN IT        00000235
.*                              INDICATES THE SIZE OF THE SAVE AREA TO  00000236
.*                              BE GOTTEN. WARNING, THE VALUE OF        00000237
.*                              -LENGTH- SHOULD NEVER BE LESS THAN 72.  00000238
.*                     -SUBPOOL- IS OPTIONAL. IF OMITTED, THEN A VALUE  00000239
.*                               OF 0 IS IMPLIED. IF GIVEN, THEN IT     00000240
.*                               SPECIFIES THE SUBPOOL OUT OF WHICH THE 00000241
.*                               SAVE AREA IS TO BE GOTTEN.             00000242
.*                     -ERROR ADDRESS- IS OPTIONAL. IF OMITTED, THEN    00000243
.*                                     THE SAVE AREA GETMAIN REQUEST IS 00000244
.*                                     UNCONDITIONAL. IF GIVEN, THEN    00000245
.*                                     THE GETMAIN IS CONDITIONAL, AND  00000246
.*                                     IF IT FAILS, THEN CONTROL IS     00000247
.*                                     PASSED TO THE INDICATED ADDRESS. 00000248
.*                                     NOTE, -ERROR ADDRESS- MAY BE     00000249
.*                                     EITHER A STATEMENT LABEL OR A    00000250
.*                                     PARENTHESIZED REGISTER NAME.     00000251
.*                                     WARNING, -ERROR ADDRESS- MAY BE  00000252
.*                                     USED ONLY IN A MVS ENVIRONMENT.  00000253
.*                                     IT IS NOT SUPPORTED UNDER MVT.   00000254
.*                     RETURN IS A SPECIAL FORM OF -ERROR ADDRESS-      00000255
.*                            WHICH, IF GIVEN, CAUSES CONTROL TO BE     00000256
.*                            RETURNED IMMEDIATELY TO THE CALLER IN THE 00000257
.*                            EVENT OF A GETMAIN FAILURE. ALL REGISTERS 00000258
.*                            ARE RESTORED EXCEPT R15 WHICH CONTAINS    00000259
.*                            THE RETURN CODE FROM GETMAIN.             00000260
.*             SAVTYPE=PLI                                              00000261
.*             SAVTYPE=NONE                                   06/84 DBC 00000262
.*                     NO LOCAL LEVEL SAVEAREA IS DEFINED OR  06/84 DBC 00000263
.*                     USED.                                  06/84 DBC 00000264
.*             SAVTYPE=(PLI,-LENGTH-)                                   00000265
.*                     THE MACRO IS TO EXPAND INTO THE FORMAT OF A PLI  00000266
.*                     PROLOG. THE EXPANSION IS REENTRANT. THE SAVE     00000267
.*                     AREA WILL BE A PLI DSA. IT WILL PROBABLY BE      00000268
.*                     OBTAINED FROM PLI'S ISA. IF -LENGTH- IS GIVEN,   00000269
.*                     THEN IT SPECIFIES THE DESIRED LENGTH OF THE DSA  00000270
.*                     THAT THIS EXPANSION OBTAINS. WARNING, THE VALUE  00000271
.*                     OF -LENGTH- MUST NEVER BE LESS THAN 88.          00000272
.*                                                                      00000273
.* &PFIX=                                                               00000274
.*       THE #ENTER MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS       00000275
.*       EXPANSION WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE  00000276
.*       OF THE REGISTERS IS INDICATED IN THE ASSEMBLER'S CROSS         00000277
.*       REFERENCE LISTING. THE PFIX= OPERAND CAN BE USED TO CONTROL    00000278
.*       THE SET OF EQUATES USED. FOR EXAMPLE, IF "PFIX=GPR" IS GIVEN,  00000279
.*       THEN "GPR1" IS USED WHENEVER THE EXPANSION REFERS FO REGISTER  00000280
.*       1.                                                             00000281
.*          IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF REGISTER   00000282
.*       EQUATES DEFINED BY THE NEXT PRIOR #REGS MACRO IS USED. IF      00000283
.*       THERE ARE NO PRIOR #REGS MACROS, THEN SELF-DEFINING NUMERICS   00000284
.*       ARE USED.                                                      00000285
.*                                                                      00000286
.* MISCELLANIOUS CONSIDERATIONS                                         00000287
.*     - SINCE THE #ENTER MACRO EXPANSION USUALLY INCLUDES A CSECT      00000288
.*       CARD, THE MACRO CALL SHOULD BE PLACED AT THE PHYSICAL          00000289
.*       BEGINNING OF A CONTROL SECTION.                                00000290
.*     - FOR LOCAL SAVE AREAS IT IS BOTH POSSIBLE AND REASONABLE FOR    00000291
.*       R13 TO SERVE AS BOTH THE SAVE AREA POINTER AND A PROGRAM BASE. 00000292
.*       THERE ARE, HOWEVER, CERTAIN PRECAUTIONS THAT HAVE TO BE TAKEN  00000293
.*       FOR VARIOUS SYSTEM EXIT ROUTINES IF THEY ARE INCLUDED IN THE   00000294
.*       PROGRAM. HERE ARE SOME EXAMPLES:                               00000295
.*           - IOS APPENDAGE ROUTINES: NO BASE REGISTER FOR THE MAIN    00000296
.*             PROGRAM, INCLUDING R13, IS AVAILABLE FROM WITHIN AN IOS  00000297
.*             APPENDAGE.                                               00000298
.*           - DCB OPEN EXITS: R13 REMAINS A VALID BASE REGISTER.       00000299
.*           - EOD ROUTINES: R13 REMAINS A VALID BASE REGISTER.         00000300
.*           - SYNAD EXITS: R13 REMAINS A VALID BASE REGISTER UNTIL A   00000301
.*             SYNADAF MACRO IS ISSUED. AFTER A SUBSEQUENT SYNADRLS     00000302
.*             MACRO, R13 IS AGAIN A VALID PROGRAM BASE.                00000303
.*                                                                      00000304
.*                                                                      00000305
.*                                                                      00000306
.* INNER MACROS USED - #USING, #TEST, SAVE, GETMAIN                     00000307
.*                                                                      00000308
         GBLA  &#TESERR                                                 00000309
         GBLB  &#ENTRNT,&#ENTPLI,&#ENTNUN                     06/84 DBC 00000310
         GBLC  &#TESRET(20),&#ENTSIZ,&#ENTSP,&#BS(14)                   00000311
         LCLA  &A1,&A2,&C1,&C2,&C3,&C4,&B(13),&RMTREGA                  00000312
         LCLB  &REDUN(13),&OLDBASE,&REMOTE(5),&ALIGND,&LOCAL,&LENGTH    00000313
         LCLC  &LID,&@,&#,&N1,&N2,&W2,&W3,&R,&RMTREGC,&SAVLEN,&RMTNAME  00000314
         LCLC  &SPOOL,&TB,&ESDT,&C                            06/84 DBC 00000315
&#       SETC  '&SYSNDX'                                                00000316
&#ENTRNT SETB  (0)                                                      00000317
&#ENTPLI SETB  (0)                                                      00000318
&#ENTNUN SETB  (0)                                                      00000319
&C1      SETA  11                                                       00000320
.*                                                            06/84 DBC 00000321
         AIF   ('&SAVTYPE' NE 'NONE').TYPNNON                 06/84 DBC 00000322
&#ENTNUN SETB  (1)                                            06/84 DBC 00000323
         AGO   .PFXTST                                        06/84 DBC 00000324
.TYPNNON ANOP                                                 06/84 DBC 00000325
.*                                                            06/84 DBC 00000326
         AIF   ('&SAVTYPE(1)' NE 'RENT').TYPNRNT                        00000327
&#ENTRNT SETB  (1)                                                      00000328
&#ENTSIZ SETC  '72'                                                     00000329
&#ENTSP  SETC  ''                                                       00000330
         #TEST DCODE=&SAVTYPE(2)                                        00000331
&A1      SETA  &#TESRET(1)                                              00000332
         AIF   (&A1 EQ 0).PFXTST                                        00000333
         AIF   ('&#TESRET(2)' EQ '').DFLTLEN                            00000334
&#ENTSIZ SETC  '&#TESRET(2)'                                            00000335
.DFLTLEN AIF   (&A1 EQ 1).PFXTST                                        00000336
&#ENTSP  SETC  '&#TESRET(3)'                                            00000337
         AGO   .PFXTST                                                  00000338
.TYPNRNT ANOP                                                           00000339
.*                                                                      00000340
         AIF   ('&SAVTYPE(1)' NE 'REMOTE').TYPNRMT                      00000341
&REMOTE(1) SETB (1)                                                     00000342
         AIF   ('&SAVTYPE(2)' NE '').GOTRMT2                            00000343
         MNOTE 12,'ERROR - SAVTYPE(2) (REMOTE AREA''S NAME) OMITTED.'   00000344
.GOTRMT2 AIF   ('&SAVTYPE(2)'(1,1) EQ '(').TYPLCL2                      00000345
&REMOTE(2) SETB (1)                                                     00000346
&RMTNAME SETC  '&SAVTYPE(2)'                                            00000347
         AIF   ('&SAVTYPE(2)'(K'&SAVTYPE(2),1) NE '%').PFXTST           00000348
&REMOTE(5) SETB (1)                                                     00000349
&RMTNAME SETC  '&SAVTYPE(2)'(1,K'&SAVTYPE(2)-1)                         00000350
         AGO   .PFXTST                                                  00000351
.TYPLCL2 #TEST DCODE=&SAVTYPE(2)                                        00000352
&RMTREGC SETC  '&#TESRET(2)'                                            00000353
         #TEST REGS=&RMTREGC                                            00000354
         AIF   (&#TESERR NE 0).PFXTST                                   00000355
&RMTREGA SETA  &#TESRET(1)                                              00000356
         AIF   (&RMTREGA NE 13).PFXTST                                  00000357
&REMOTE(3) SETB (1)                                                     00000358
         AGO   .PFXTST                                                  00000359
.TYPNRMT ANOP                                                           00000360
.*                                                                      00000361
         AIF   ('&SAVTYPE(1)' NE 'PLI').TYPNPLI                         00000362
&#ENTPLI SETB  (1)                                                      00000363
&C1      SETA  10                                                       00000364
         AGO   .PFXTST                                                  00000365
.TYPNPLI ANOP                                                           00000366
.*                                                                      00000367
         AIF   ('&SAVTYPE(1)' EQ '' OR '&SAVTYPE(1)' EQ 'LOCAL').TYPLCL 00000368
         MNOTE 4,'SAVTYPE(1)=&SAVTYPE(1) IS INVALID.'                   00000369
         MNOTE 4,'SAVTYPE(1)=LOCAL ASSUMED.'                            00000370
.TYPLCL  ANOP                                                           00000371
&LOCAL   SETB  (1)                                                      00000372
&C1      SETA  12                                                       00000373
&LID     SETC  'E&#.SVA'                                                00000374
         AIF   ('&SAVTYPE(2)' EQ '').GOTSLID                            00000375
&LID     SETC  '&SAVTYPE(2)'                                            00000376
.GOTSLID ANOP                                                           00000377
&SAVLEN  SETC  '72'                                                     00000378
         AIF   ('&SAVTYPE(3)' EQ '').PFXTST                             00000379
&SAVLEN  SETC  '&SAVTYPE(3)'                                            00000380
.*                                                                      00000381
.PFXTST  ANOP                                                           00000382
&@       SETC  '&PFIX'                                                  00000383
         AIF   (K'&PFIX NE 0).GOTPFIX                                   00000384
         #TEST PFIX=                                                    00000385
&@       SETC  '&#TESRET(1)'                                            00000386
         AGO   .DONPFIX                                                 00000387
.GOTPFIX #REGS &PFIX,GEN=NO                                             00000388
.DONPFIX ANOP                                                           00000389
.*                                                            06/84 DBC 00000390
&A1      SETA  0                                                        00000391
         AIF   ('&BASES' NE '*').BSCLR                                  00000392
.BSOLD   AIF   (&A1 EQ 13).BASEND                                       00000393
&A1      SETA  &A1+1                                                    00000394
         AIF   ('&#BS(&A1)' EQ '').BSOLD                                00000395
         #TEST REGS=&#BS(&A1)                                           00000396
&B(&A1)  SETA  16                                                       00000397
         AIF   (&#TESERR NE 0).BSOLD                                    00000398
&B(&A1)  SETA  &#TESRET(1)                                              00000399
         AIF   (&B(&A1) NE 13).BSOLD                                    00000400
         AIF   (&A1 NE 13 OR '&#BS(14)' NE '&RMTNAME' OR &#ENTRNT OR &#*00000401
               ENTPLI).BSERROR                                          00000402
&REMOTE(4) SETB (1)                                                     00000403
         AGO   .BSOLD                                                   00000404
.BSERROR ANOP                                                           00000405
         MNOTE 4,'THE OLD BASE REGISTER &B(&A1) CANNOT ALSO FUNCTION'   00000406
         MNOTE 4,'AS A SAVE AREA POINTER IN THIS CONTEXT.'              00000407
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*00000408
               '                                                        00000409
         AGO   .BSOLD                                                   00000410
.BSCLR   AIF   (&A1 EQ 14).BSCLRD                                       00000411
&A1      SETA  &A1+1                                                    00000412
&#BS(&A1) SETC ''                                                       00000413
         AGO   .BSCLR                                                   00000414
.BSCLRD  AIF   (K'&BASES EQ 0).BASEND                                   00000415
         AIF   ('&BASES' NE '&BASES(1)').TSTNBSE                        00000416
         #TEST NUM=&BASES                                               00000417
         AIF   (&#TESERR EQ 0).BSEOKX                                   00000418
         MNOTE 4,'"BASES=&BASES" IS INVALID.'                           00000419
         MNOTE 4,'"BASES=1" ASSUMED.'                                   00000420
&C3      SETA  1                                                        00000421
         AGO   .BSESET                                                  00000422
.BSEOKX  ANOP                                                           00000423
&C3      SETA  &BASES                                                   00000424
         AIF   (&C3 LE &C1).BSESET                                      00000425
         MNOTE 4,'"BASES=&BASES" IS OUTSIDE THE RANGE OF 0...&C1..'     00000426
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*00000427
               '                                                        00000428
.BSESET  ANOP                                                           00000429
&C1      SETA  &C1+1                                                    00000430
&C3      SETA  &C1-&C3                                                  00000431
&C4      SETA  13                                                       00000432
.BOK     AIF   (&C1 LE &C3).BASEND                                      00000433
&#BS(&C4) SETC '&@&C1'                                                  00000434
&B(&C4)  SETA  &C1                                                      00000435
&C1      SETA  &C1-1                                                    00000436
&C4      SETA  &C4-1                                                    00000437
         AGO   .BOK                                                     00000438
.TSTNBSE ANOP                                                           00000439
&C3      SETA  N'&BASES                                                 00000440
         AIF   (&C3 LE &C1).NBSOK                                       00000441
         MNOTE 4,'"BASES=&BASES" SPECIFIES TOO MAY REGISTERS.'          00000442
         MNOTE 4,'ONLY THE FIRST &C1 REGISTERS WILL BE USED.'           00000443
&C3      SETA  &C1                                                      00000444
.NBSOK   ANOP                                                           00000445
&C1      SETA  &C1+1                                                    00000446
&C4      SETA  13                                                       00000447
&C2      SETA  0                                                        00000448
.GETBSE  AIF   (&C2 GE &C3).BASEND                                      00000449
&C2      SETA  &C2+1                                                    00000450
         AIF   ('&BASES(&C2)' EQ '').IGNR                               00000451
         #TEST REGS=&BASES(&C2)                                         00000452
&B(&C4)  SETA  16                                                       00000453
         AIF   (&#TESERR EQ 16).REGUNK                                  00000454
         AIF   (&#TESRET(1) GE 2 AND &#TESRET(1) LE &C1).BSEOK2         00000455
         MNOTE 4,'"BASES(&C2)=&BASES(&C2)" IS OUTSIDE THE RANGE OF 2...*00000456
               &C1..'                                                   00000457
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*00000458
               '                                                        00000459
         AGO   .BSEOK2                                                  00000460
.IGNR    AIF   (&C3 GE N'&BASES).GETBSE                                 00000461
&C3      SETA  &C3+1                                                    00000462
         AGO   .GETBSE                                                  00000463
.BSEOK2  AIF   (NOT &REDUN(&#TESRET(1))).BSEOK3                         00000464
         MNOTE 4,'"BASES(&C2)=&BASES(&C2) IS REDUNDANT.'                00000465
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*00000466
               '                                                        00000467
.BSEOK3  ANOP                                                           00000468
&REDUN(&#TESRET(1)) SETB (1)                                            00000469
&B(&C4)  SETA &#TESRET(1)                                               00000470
.REGUNK  ANOP                                                           00000471
&C1      SETA  12                                                       00000472
&#BS(&C4) SETC '&BASES(&C2)'                                            00000473
&C4      SETA  &C4-1                                                    00000474
         AGO   .GETBSE                                                  00000475
.BASEND  ANOP                                                           00000476
.*                                                                      00000477
&R       SETC  '&@.1'                                                   00000478
&C3      SETA  0                                                        00000479
.WRLP    AIF   (&C3 GE 13).GOTWRG                                       00000480
&C3      SETA  &C3+1                                                    00000481
         AIF   ('&#BS(&C3)' EQ '').WRLP                                 00000482
         AIF   (&B(&C3) EQ 13).GOTWRG                                   00000483
&R       SETC  '&#BS(&C3)'                                              00000484
.GOTWRG  ANOP                                                           00000485
.*                                                                      00000486
&ESDT    SETC  '&ESDTYPE(1)'                                            00000487
         AIF   (&#ENTPLI).PLIGEN                                        00000488
.*                                                                      00000489
         AIF   ('&ESDT' NE 'DEFAULT').GOTESD                            00000490
&ESDT    SETC  'CSECT'                                                  00000491
.GOTESD  ANOP                                                           00000492
&N2      SETC  '&N'                                                     00000493
         AIF   ('&ESDT' EQ 'NONE' OR '&ESDT' EQ '' OR '&ESDT' EQ 'ENTRY*00000494
               ').NCSCETC                                               00000495
         AIF   ('&ESDT' NE 'CSECT').ESDNCSC                             00000496
&N       CSECT ,                   START CONTROL SECTION                00000497
         AGO   .ESDDONE                                                 00000498
.ESDNCSC AIF   ('&ESDT' NE 'START').ESDNSTA                             00000499
&N       START ,                   START CONTOL SECTION                 00000500
         AGO   .ESDDONE                                                 00000501
.ESDNSTA ANOP                                                           00000502
&W2      SETC  '&ESDT'                                                  00000503
&N       &W2   0H'0'               START                                00000504
.ESDDONE ANOP                                                           00000505
&N2      SETC  ''                                                       00000506
.*                                                                      00000507
.NCSCETC AIF   ('&ESDT' NE 'ENTRY').NENTRY                              00000508
         ENTRY &N                  MAKE NAME EXTERNALLY AVAILABLE       00000509
.NENTRY  ANOP                                                           00000510
.*                                                                      00000511
         AIF   (K'&NME EQ 0 AND '&ESDT' NE 'CSECT' AND '&ESDT' NE 'STAR*00000512
               T').NMODID                                               00000513
&N2      B     E&#.ZID-&N.(,&@.15) SKIP AROUND THE MODULE ID            00000514
&N2      SETC  'E&#.ZID'                                                00000515
         DC    AL1(&N2-E&#.MID)    LENGTH OF TEXT                       00000516
E&#.MID  DC    C'&N '              ENTRY NAME                           00000517
&W2      SETC  '&SYSDATE       '(1,8).' '                               00000518
         DC    C'&W2'              ASSEMBLY DATE                        00000519
&W2      SETC  '&SYSTIME    '(1,5)                                      00000520
         AIF   (K'&NME EQ 0).NMEZ1                                      00000521
&W2      SETC  '&W2 - '                                                 00000522
.NMEZ1   DC    C'&W2'              ASSEMBLY TIME                        00000523
         AIF   (K'&NME EQ 0).NMEZ2                                      00000524
         AIF   ('&NME'(1,1) EQ '''').QNME                               00000525
         DC    C'&NME'                                                  00000526
         AGO   .NMEZ2                                                   00000527
.QNME    DC    C&NME                                                    00000528
.NMEZ2   ANOP                                                           00000529
.NMODID  ANOP                                                           00000530
.*                                                                      00000531
         AIF   (&REMOTE(3)).RNTRNT1                                     00000532
&W2      SETC  '&@.14,&@.12,12(&@.13)'                                  00000533
&N2      STM   &W2                 SAVE CALLER'S REGISTERS              00000534
&N2      SETC  ''                                                       00000535
.*                                                                      00000536
         AIF   (&#ENTRNT OR &#ENTNUN).RNTRNT1                 06/84 DBC 00000537
         LR    &R,&@.13            POINT TO HIGHER SA                   00000538
         AIF   (&REMOTE(1)).LRMTSV1                                     00000539
&C       SETC  '&@.13,&LID-&N.(,&@.15)'                       06/84 DBC 00000540
         LA    &C                  POINT TO LOWER SA                    00000541
         AGO   .LRMTSV2                                                 00000542
.LRMTSV1 AIF   (&REMOTE(2)).LRMTSV3                                     00000543
         #TEST REGS=&R                                                  00000544
         AIF   (&RMTREGA NE &#TESRET(1)).LRMTSV4                        00000545
&A1      SETA  20+&RMTREGA*4-&RMTREGA/13*44                             00000546
         L     &@.13,&A1.(,&@.13)  POINT TO LOWER SA                    00000547
         AGO   .LRMTSV2                                                 00000548
.LRMTSV4 LR    &@.13,&RMTREGC      POINT TO LOWER SA                    00000549
         AGO   .LRMTSV2                                                 00000550
.LRMTSV3 AIF   (&REMOTE(5)).LRMTSV5                                     00000551
         PUSH  USING               SAVE USING ENVIRONMENT               00000552
         USING &N,&@.15            DECLARE TEMP BASE                    00000553
         LA    &@.13,&RMTNAME      POINT TO LOWER SA                    00000554
         POP   USING               RESTORE USING ENVIRONMENT            00000555
         AGO   .LRMTSV2                                       06/84 DBC 00000556
.LRMTSV5 ANOP                                                 06/84 DBC 00000557
         LR    &@.13,&@.15         POINT TO LOWER SA          08/84 DBC 00000558
&C       SETC  '&@.13,E&#.SAP-&N.(,&@.15)'                    06/84 DBC 00000559
         AH    &C                                             08/84 DBC 00000560
.LRMTSV2 ST    &@.13,8(,&R)        FORWARD CHAIN THE SAVE AREAS         00000561
         ST    &R,4(,&@.13)        BACK CHAIN THE SAVE AREAS            00000562
         AIF   ('&R' NE '&@.1').RNTRNT1                                 00000563
         L     &@.1,24(,&@.1)      RESTORE REGISTER 1                   00000564
.RNTRNT1 AIF   ('&#BS(13)' EQ '').SKIPUSE                               00000565
&C1      SETA  13                                                       00000566
         AIF   ('&BASES' NE '*').BSEADR                                 00000567
         AIF   (&REMOTE(4)).EQUATE                                      00000568
&OLDBASE SETB  (1)                                                      00000569
&N2      LR    &#BS(13),&@.15      LOAD 1ST BASE REGISTER     08/84 DBC 00000570
&N2      SETC  ''                                             08/84 DBC 00000571
&C       SETC  '&#BS(13),E&#.BSE-&N.(,&@.15)'                 06/84 DBC 00000572
         SH    &C                                             08/84 DBC 00000573
         AGO   .EQUATE                                                  00000574
.BSEADR  ANOP                                                           00000575
&C2      SETA  15                                                       00000576
&#BS(14) SETC  '&N'                                                     00000577
         AIF   (&#ENTRNT OR &REMOTE(1) OR &#ENTNUN).FLDTST    06/84 DBC 00000578
&C2      SETA  13                                                       00000579
&#BS(14) SETC  '&LID'                                                   00000580
.FLDTST  AIF   (&B(13) EQ &C2).EQUATE                                   00000581
&N2      LR    &#BS(13),&@&C2      LOAD FIRST BASE REGISTER             00000582
&N2      SETC  ''                                                       00000583
.EQUATE  ANOP                                                           00000584
&W2      SETC  '&#BS(&C1)'                                              00000585
         AIF   (&C1 EQ 2).ENDLA                                         00000586
&C1      SETA  &C1-1                                                    00000587
         AIF   ('&#BS(&C1)' EQ '').ENDLA                                00000588
&N2      LA    &#BS(&C1),X'FFF'(,&W2) LOAD NEXT BASE                    00000589
&N2      SETC  ''                                                       00000590
         AGO   .EQUATE                                                  00000591
.ENDLA   #USING                                                         00000592
.SKIPUSE AIF   (NOT &#ENTRNT).DATACHK                                   00000593
&W2      SETC  ''                                                       00000594
&TB      SETC  ''                                                       00000595
         AIF   ('&#BS(13)' NE '').GETM2                                 00000596
&N2      LR    &@.14,&@.15         LOAD TEMPORARY BASE                  00000597
&N2      SETC  ''                                                       00000598
         PUSH  USING               SAVE BASES                           00000599
         DROP  ,                   CLEAR BASES                          00000600
         USING &N,&@.14            DECLARE TEMPORARY BASE               00000601
&TB      SETC  '-&N.(,&@.14)'                                           00000602
.GETM2   ANOP                                                           00000603
&N2      L     &@.0,E&#.LEN        LOAD LENGTH (MAYBE SUBPOOL TOO)      00000604
&N2      SETC  ''                                                       00000605
         AIF   ('&#ENTSP' EQ '' OR '&SAVTYPE(3)' EQ '').GETM4           00000606
&SPOOL   SETC  ''                                                       00000607
         MNOTE '         GETMAIN RC,LV=(0),SP=&#ENTSP'                  00000608
         GETMAIN RC,LV=(0),SP=&#ENTSP                                   00000609
         AGO   .GETM5                                                   00000610
.GETM4   ANOP                                                           00000611
&SPOOL   SETC  '&#ENTSP'                                                00000612
&W3      SETC  'R'                                                      00000613
         AIF   ('&SAVTYPE(3)' EQ '').GETM4A                             00000614
&W3      SETC  'RC'                                                     00000615
.GETM4A  MNOTE '         GETMAIN &W3,LV=(0)'                            00000616
         GETMAIN &W3,LV=(0)                                             00000617
.GETM5   AIF   ('&#BS(13)' NE '').GETM5A                                00000618
         POP   USING               RESTURE BASES                        00000619
.GETM5A  AIF   ('&SAVTYPE(3)' EQ '').GETM7                              00000620
         LTR   &@.15,&@.15         GETMAIN OK?                          00000621
         AIF   ('&SAVTYPE(3)' EQ 'RETURN').GETM8                        00000622
         AIF   ('&SAVTYPE(3)'(1,1) EQ '(').GETM6                        00000623
         BNZ   &SAVTYPE(3)         NO, TAKE ERROR EXIT                  00000624
         AGO   .GETM7                                                   00000625
.GETM6   #TEST DCODE=&SAVTYPE(3)                                        00000626
         BCR   7,&#TESRET(2)       NO, TAKE ERROR EXIT                  00000627
         AGO   .GETM7                                                   00000628
.GETM8   ANOP                                                           00000629
&W2      SETC  'E&#.GO'                                                 00000630
         BZ    &W2&TB              YES, PROCEED                         00000631
         L     &@.14,12(,&@.13)    NO, RESTORE REGISTER                 00000632
         LM    &@.0,&@.12,20(&@.13) RESTORE REGISTERS                   00000633
         MVI   12(&@.13),X'FF'     SET RETURNED SIGNEL                  00000634
         BR    &@.14               RETURN TO CALLER                     00000635
.GETM7   ANOP                                                           00000636
&W2      LR    &@.0,&@.1           POINT TO AREA TO CLEAR               00000637
         L     &@.1,E&#.LEN&TB     GET LENGTH TO CLEAR                  00000638
         LR    &@.14,&@.0          SAVE AREA POINTER                    00000639
         MVCL  &@.0,&@.14          CLEAR THE AREA (R15 SET BY GETMAIN)  00000640
         ST    &@.14,8(,&@.13)     FORWARD CHAIN THE SAVE AREAS         00000641
         ST    &@.13,4(,&@.14)     BACK CHAIN THE SAVE AREAS            00000642
         LM    &@.13,&@.1,8(&@.13) RESTORE REGS AND POINT TO LOWER SA   00000643
.DATACHK AIF   (NOT &OLDBASE AND NOT &#ENTRNT AND NOT &REMOTE(5) AND NO*00000644
               T &LOCAL).ENDCHK                                         00000645
         AIF   ('&#BS(13)' EQ '').NOUSING                               00000646
&N2      B     E&#.END             SKIP AROUND DATA AREA                00000647
&N2      SETC  ''                                                       00000648
         AGO   .DFNDATA                                                 00000649
.NOUSING ANOP                                                           00000650
&N2      B     E&#.END-&N.(,&@.15) SKIP AROUND DATA AREA                00000651
&N2      SETC  ''                                                       00000652
.DFNDATA ANOP                                                 08/84 DBC 00000653
.*                                                            08/84 DBC 00000654
         AIF   (NOT &OLDBASE).NOLDBSE                         08/84 DBC 00000655
E&#.BSE  DC    Y(&N-&#BS(14))       OLD BASE ADDRESS          08/84 DBC 00000656
.NOLDBSE ANOP                                                 08/84 DBC 00000657
.*                                                            08/84 DBC 00000658
         AIF   (NOT &#ENTRNT).NLENGTH                         08/84 DBC 00000659
         AIF   ('&SPOOL' EQ '').NSUBPOO                                 00000660
         AIF   (&ALIGND).ALIGND1                                        00000661
         DS    0F                  ALIGNMENT                            00000662
&ALIGND  SETB  (1)                                                      00000663
.ALIGND1 ANOP                                                           00000664
E&#.LEN  DC    AL1(&SPOOL),AL3(&#ENTSIZ) SAVE AREA SUBPOOL AND LENGTH   00000665
         AGO   .NLENGTH                                                 00000666
.NSUBPOO ANOP                                                           00000667
E&#.LEN  DC    A(&#ENTSIZ)         SAVE AREA LENGTH                     00000668
&ALIGND  SETB  (1)                                                      00000669
.NLENGTH AIF   (&#ENTRNT OR &#ENTNUN).NSVAREA                 06/84 DBC 00000670
         AIF   (&REMOTE(1)).RMTSVPT                                     00000671
         AIF   (&ALIGND).ALIGND2                                        00000672
         DS    0F                  ALIGNMENT                            00000673
&ALIGND  SETB  (1)                                                      00000674
.ALIGND2 ANOP                                                           00000675
&LID     DC    (&SAVLEN)X'00'      LOCAL SAVE AREA                      00000676
         AGO   .NSVAREA                                                 00000677
.RMTSVPT ANOP                                                 08/84 DBC 00000678
.*                                                            08/84 DBC 00000679
         AIF   (NOT &REMOTE(5)).NSVAREA                       08/84 DBC 00000680
E&#.SAP  DC    Y(&RMTNAME-&N)       PTR TO REMOTE SA          08/84 DBC 00000681
.NSVAREA ANOP                                                 08/84 DBC 00000682
.*                                                            08/84 DBC 00000683
         AGO   .END                                           08/84 DBC 00000684
.*                                                                      00000685
.PLIGEN  AIF   ('&ESDT' EQ 'DEFAULT').PESDSET                           00000686
         AIF   (K'&N GT 0 OR '&ESDT' NE 'ENTRY').PESDOK1                00000687
         MNOTE 4,'"ESDTYPE=&ESDTYPE" IS INVALID WHEN THE NAME FIELD IS' 00000688
         MNOTE 4,'OMITTED FROM THE MACRO CALL.'                         00000689
         MNOTE 4,'"ESDTYPE=NONE" WILL BE USED INSTEAD.'                 00000690
&ESDT    SETC  'NONE'                                                   00000691
.PESDOK1 ANOP                                                           00000692
         AIF   ('&ESDT' EQ 'ENTRY' OR '&ESDT' EQ 'NONE' OR '&ESDT' EQ '*00000693
               ').PESDOK                                                00000694
         MNOTE 4,'"ESDTYPE=&ESDTYPE" IS INVALID WHEN "SAVTYPE=PLI".'    00000695
.PESDSET ANOP                                                           00000696
&ESDT    SETC  'ENTRY'                                                  00000697
         AIF   (K'&N GT 0).PESDOK2                                      00000698
&ESDT    SETC  'NONE'                                                   00000699
.PESDOK2 AIF   ('&ESDTYPE' EQ 'DEFAULT').PESDOK                         00000700
         MNOTE 4,'"ESDTYPE=&ESDT" WILL BE USED INSTEAD.'                00000701
.PESDOK  ANOP                                                           00000702
         AIF   ('&ESDT' NE 'ENTRY').PNOTENT                             00000703
         ENTRY &N                  MAKE NAME EXTERNALLY AVAILABLE       00000704
.PNOTENT ANOP                                                           00000705
.*                                                                      00000706
&N2      SETC  '&N'                                                     00000707
&A1      SETA  K'&N                                                     00000708
         AIF   (K'&NME EQ 0).GOTN2                                      00000709
&N2      SETC  '&NME'                                                   00000710
&A1      SETA  K'&NME                                                   00000711
.GOTN2   ANOP                                                           00000712
&N2      SETC  ' '(1,1-(&A1-&A1/2*2)).'&N2'                             00000713
         DS    0H                  ALIGNMENT                            00000714
         DC    C'&N2'              ENTRY NAME                           00000715
         DC    AL1(&A1)            LENGTH OF NAME                       00000716
.*                                                                      00000717
&N1      SETC  '&N'                                                     00000718
         AIF   (K'&N GT 0).PGOTN1                                       00000719
&N1      SETC  'E&#.ENT'                                                00000720
.PGOTN1  ANOP                                                           00000721
         USING &N1,&@.15           DCL LOCAL BASE                       00000722
&N1      STM   &@.14,&@.12,12(&@.13) SAVE CALLER'S REGISTERS            00000723
.*                                                                      00000724
         #TEST DCODE=&SAVTYPE(2)                                        00000725
&A1      SETA  &#TESRET(1)                                              00000726
&A2      SETA  120                                                      00000727
         AIF   (&A1 EQ 0).DSALLA                                        00000728
         AIF   ('&#TESRET(2)' NE '&SAVTYPE(2)').DSALREG                 00000729
         #TEST NUM=&#TESRET(2)                                          00000730
         AIF   (&#TESERR NE 0).DSALL                                    00000731
&A2      SETA  &#TESRET(2)                                              00000732
         AIF   (&A2 GE 4096-7).DSALL                                    00000733
         AIF   (&A2 GE 120-7).DSALLA                                    00000734
         MNOTE 4,'"SAVTYPE(2)=&SAVTYPE(2)" IS TOO SHORT A LENGTH.'      00000735
         MNOTE 4,'THE CODE GENERATED BELOW WILL NOT FUNCTION CORRECTLY.*00000736
               '                                                        00000737
.DSALLA  ANOP                                                           00000738
         LA    &@.0,(&A2+7)/8*8    GET DESIRED DSA LENGTH               00000739
         AGO   .GOTDSAL                                                 00000740
.DSALREG #TEST REGS=&#TESRET(2)                                         00000741
         AIF   (&#TESERR NE 0).DSALLR                                   00000742
         AIF   (&#TESRET(1) EQ 0).GOTLLR                                00000743
.DSALLR  LR    &@.0,&#TESRET(2)    GET DESIRED DSA LENGTH               00000744
.GOTLLR  LA    &@.14,7             ROUND UP -                           00000745
         AR    &@.0,&@.14           TO -                                00000746
         OR    &@.0,&@.14            DOUBLE WORD -                      00000747
         XR    &@.0,&@.14             LENGTH                            00000748
         AGO   .GOTDSAL                                                 00000749
.DSALL   ANOP                                                           00000750
&LENGTH  SETB  (1)                                                      00000751
         L     &@.0,E&#.LEN        GET DESIRED DSA LENGTH               00000752
.GOTDSAL ANOP                                                           00000753
.*                                                                      00000754
         L     &@.1,76(,&@.13)     GET NXT AVAILABLE BLOCK POINTER      00000755
         ALR   &@.0,&@.1           --> PAST DESIRED AREA                00000756
         CL    &@.0,12(,&@.12)     WOULD THE ISA OVERFLOW?              00000757
         BNH   E&#.GOT             NO, PROCEED                          00000758
         L     &@.15,116(,&@.12)   YES, --> SPECIAL HANDLER             00000759
         DROP  &@.15               RELEASE CLOBBERED BASE               00000760
         BALR  &@.14,&@.15         GO OBTAIN DESIRED DSA FROM ELSEWHERE 00000761
E&#.GOT  LR    &@.14,&@.1          SAVE PTR TO NEW DSA                  00000762
         LR    &@.15,&@.0          SAVE HI-BYTE OF NAB POINTER REG      00000763
         SRL   &@.15,24            ISSOLATE IT                          00000764
         SLL   &@.15,24            RESTORE ITS POSITION. SET MVCL       00000765
*                                  SOURCE LENGTH TO ZERO                00000766
         SR    &@.0,&@.1           GET LENGTH OF NEW DSA                00000767
         LR    &@.1,&@.0           COPY FOR MVCL SINK LENGTH            00000768
         LR    &@.0,&@.14          GET MVCL SINK POINTER                00000769
         MVCL  &@.0,&@.14          CLEAR THE NEW DSA                    00000770
         OR    &@.0,&@.15          RESTORE HI-BYTE TO NAB POINTER REG   00000771
         LR    &@.1,&@.0           COPY NEXT AVAILABLE BLOCK POINTER    00000772
         L     &@.15,72(,&@.13)    GET LIBRARY WORKSPACE POINTER        00000773
         STM   &@.15,&@.1,72(&@.14) STORE INTO OUR NEW DSA              00000774
         ST    &@.5,88(,&@.14)     STORE PASSED PARAMETERS POINTER      00000775
         ST    &@.13,4(,&@.14)     BACK CHAIN THE DSA                   00000776
         L     &@.1,24(,&@.13)     RESTORE PLIST POINTER                00000777
         LR    &@.13,&@.14         --> NEW DSA (R14 PURIFIED BY MVCL)   00000778
         MVI   0(&@.13),X'80'      SET FOR -                            00000779
         MVI   1(&@.13),X'00'       PLI -                               00000780
         MVI   86(&@.13),X'91'       ERROR -                            00000781
         MVI   87(&@.13),X'C0'        HANDLING                          00000782
.*                                                                      00000783
&N2      SETC  ''                                                       00000784
         AIF   ('&#BS(13)' EQ '').PSKPUSE                               00000785
&C1      SETA  13                                                       00000786
         AIF   ('&BASES' NE '*').PBSEADR                                00000787
&OLDBASE SETB  (1)                                                      00000788
         BALR  &#BS(13),0          LOAD TEMP LOCAL BASE                 00000789
         L     &#BS(13),E&#.BSE-*(,&#BS(13)) LOAD 1ST PROGRAM BASE      00000790
         AGO   .PEQUATE                                                 00000791
.PBSEADR ANOP                                                           00000792
&N2      SETC  'E&#.BSE'                                                00000793
&#BS(14) SETC  '&N2'                                                    00000794
         BALR  &#BS(13),0          LOAD 1ST PROGRAM BASE                00000795
.PEQUATE ANOP                                                           00000796
&W2      SETC  '&#BS(&C1)'                                              00000797
&C1      SETA  &C1-1                                                    00000798
         AIF   (&C1 EQ 1 OR '&#BS(&C1)' EQ '').PENDLA                   00000799
&N2      LA    &#BS(&C1),X'FFF'(,&W2) LOAD NEXT PROGRAM BASE            00000800
&N2      SETC  ''                                                       00000801
         AGO   .PEQUATE                                                 00000802
.PENDLA  #USING ,                                                       00000803
.PSKPUSE ANOP                                                           00000804
.*                                                                      00000805
         AIF   (NOT &LENGTH AND NOT &OLDBASE).ENDCHK                    00000806
         AIF   ('&#BS(13)' NE '').PGOTBAS                               00000807
&N2      BALR  &@.15,0             LOAD TEMP BASE                       00000808
&N2      SETC  ''                                                       00000809
         B     E&#.END-*(,&@.15)   SKIP DATA AREA                       00000810
         AGO   .PDFNDAT                                                 00000811
.PGOTBAS ANOP                                                           00000812
&N2      B     E&#.END             SKIP DATA AREA                       00000813
&N2      SETC  ''                                                       00000814
.PDFNDAT AIF   (NOT &LENGTH).PNOLEN                                     00000815
E&#.LEN  DC    A((&SAVTYPE(2)+7)/8*8) DESIRED DSA LENGTH                00000816
.PNOLEN  AIF   (NOT &OLDBASE).PNOOBAS                                   00000817
E&#.BSE  DC    A(&#BS(14))         OLD BASE ADDRESS                     00000818
.PNOOBAS ANOP                                                           00000819
.*                                                                      00000820
.END     ANOP                                                           00000821
E&#.END  DS    0H                                                       00000822
.ENDCHK  AIF   ('&N2' EQ '').MEND                                       00000823
&N2      DS    0H                                                       00000824
.MEND    MEND                                                           00000825
         MACRO                                                          00000100
&NME     #EXIT &R,&PFIX=,&RC=                                           00000200
.*                                                                      00000300
.*                                                                      00000400
.*                                                            10/84 DBC 00000500
.* LAST CHANGE DATE - OCTOBER 1, 1984                         10/84 DBC 00000600
.*                  - DELETED CODE THAT SET A X'FF' "RETURN   10/84 DBC 00000700
.*                    INDICATOR" IN THE HI-BYTE OF DSAR14.    10/84 DBC 00000800
.*                    IT WAS NOT APPROPRIATE FOR MVS/XA.      10/84 DBC 00000900
.*                                                            06/84 DBC 00001000
.* LAST CHANGE DATE - JUNE 11, 1984                           06/84 DBC 00001100
.*                  - ADDED SUPPORT FOR "SAVTYPE=NONE" ON THE 06/84 DBC 00001200
.*                    #ENTER MACRO.                           06/84 DBC 00001300
.*                  - FOR REENTRANT EXIT LINKAGE, CHANGED     06/84 DBC 00001400
.*                    THE FREEMAIN SO THAT IT WOULD NO        06/84 DBC 00001500
.*                    LONGER GENERATE AN INLINE PLIST.        06/84 DBC 00001600
.*                                                                      00001700
.* LAST CHANGE DATE - OCTOBER 18, 1983                                  00001800
.*                  - MAILING ADDRESS CHANGE                            00001900
.*                                                                      00002000
.* LAST CHANGE DATE - APRIL 21, 1981                                    00002100
.*                  - CHANGE THE MACRO NAME FROM $EXIT TO #EXIT         00002200
.*                                                                      00002300
.* LAST CHANGE DATE - APRIL 15, 1981                                    00002400
.*                  - ADDED EXIT LINKAGE FOR A PLI ENVIRONMENT.         00002500
.*                  - "#REGS GEN=NO" SUPPORT ADDED.                     00002600
.*                                                                      00002700
.* LAST CHANGE DATE - OCTOBER 3, 1978                                   00002800
.*                  - FOR REENTRANT SAVE AREAS, THE FREEMAIN HAS BEEN   00002900
.*                    CHANGED SO THAT MORE THAN 4K CAN BE FREED.        00003000
.*                                                                      00003100
.* LAST CHANGE DATE - FEBRUARY 2, 1977                                  00003200
.*                  - MAILING ADDRESS CHANGE.                           00003300
.*                                                                      00003400
.* LAST CHANGE DATE - FEBRUARY 10, 1976                                 00003500
.*                                                                      00003600
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS CONCERNING   00003700
.* IT MAY BE ADDRESSED TO:                                              00003800
.*       RR#2 BOX 712                                                   00003900
.*       AFTON, VA. 22920                                               00004000
.*                                                                      00004100
.*                                                                      00004200
.*                                                                      00004300
.*   THIS MACRO GENERATES EITHER OS STANDARD OR PLI STANDARD EXIT       00004400
.* LINKAGE. IT WAS WRITTEN TO PROVIDE A SIMPLE, FLEXIBLE, AND COMPLETE  00004500
.* METHOD FOR GENERATING SUCH LINKAGE. IN ADDITION, FOR ALL POSSIBLE    00004600
.* OPERAND COMBINATIONS, #EXIT WILL GENERATE THE ABSOLUTE MINIMUM       00004700
.* AMOUNT OF CODE NECESSARY.                                            00004800
.*                                                                      00004900
.*   THE #EXIT MACRO WILL GENERATE THE FOLLOWING:                       00005000
.* - CODE TO LOAD REGISTER 13 WITH A POINTER TO THE HIGHER OS SAVE AREA 00005100
.*   OR PLI DATA STORAGE AREA                                           00005200
.* - CODE TO RELEASE (VIA FREEMAIN) THE LOWER SAVE AREA IF THE          00005300
.*   PRECEEDING #ENTER MACRO CALL GENERATED A REENTRANT EXPANSION       00005400
.* - CODE TO RESTORE ANY PARTICULAR SET OF REGISTERS                    00005500
.* - CODE TO LOAD REGISTER 15 WITH A RETURN CODE THAT IS EITHER AN      00005600
.*   ABSOLUTE VALUE OR A VALUE PRELOADED INTO ANY REGISTER              00005700
.* - CODE TO RETURN TO THE CALLING PROGRAM VIA REGISTER 14              00005800
.*                                                                      00005900
.*   THE NAME FIELD                                                     00006000
.* USE THIS FIELD TO ASSIGN A STATEMENT LABEL TO THE FIRST MACHINE      00006100
.* INSTRUCTION OF THE EXPANSION. IF THE NAME FIELD IS OMITTED, THEN NO  00006200
.* STATEMENT LABEL IS ASSIGNED.                                         00006300
.*                                                                      00006400
.*   THE FIRST POSITIONAL OPERAND                                       00006500
.* THIS OPERAND MUST CONSIST OF A SUB-LIST OF ANY NUMBER OF ENTRIES.    00006600
.* EACH ENTRY MAY BE EITHER A SINGLE REGISTER NAME OR A PARENTHESIZED   00006700
.* PAIR (SEPERATED BY A COMMA) OF REGISTER NAMES - E.G.                 00006800
.* " (1,11,(7,9),5) ". EACH SINGLE REGISTER NAME SPECIFIES A PARTICULAR 00006900
.* REGISTER TO BE RESTORED FROM THE HIGHER SAVE AREA. EACH              00007000
.* PARENTHESIZED PAIR OF REGISTER NAMES SPECIFIES A RANGE OF REGISTERS  00007100
.* TO BE RESTORED. THUS, THE ABOVE EXAMPLE WOULD CAUSE REGISTERS 1, 5,  00007200
.* 7, 8, 9, AND 11 TO BE RESTORED.                                      00007300
.*   SOME NOTES AND WARNINGS:                                           00007400
.* - A REQUEST TO RESTORE REGISTER 13 IS MEANINGLESS AND IS IGNORED.    00007500
.* - IF REGISTER 14 IS TO BE LOADED WITH THE RETURN ADDRESS FOUND IN    00007600
.*   THE HIGHER SAVE AREA, THEN YOU MUST SPECIFICALLY REQUEST THAT IT   00007700
.*   (REGISTER 14) BE RESTORED; OTHERWISE, WHATEVER VALUE IS FOUND IN   00007800
.*   REGISTER 14 PRIOR TO THE MACRO CALL WILL BE USED FOR THE RETURN    00007900
.*   ADDRESS.                                                           00008000
.* - TO RESTORE ALL REGISTERS FROM 14 THROUGH 12, YOU MUST CODE         00008100
.*   " ((14,12)) ". CODING " (14,12) " WILL CAUSE ONLY REGISTERS 14 AND 00008200
.*   12 TO BE RESTORED.                                                 00008300
.* - THE NUMERIC VALUES OF ALL REGISTER NAMES USED IN THIS OPERAND MUST 00008400
.*   BE DETERMINABLE AT MACRO PASS TIME. THUS, EACH REGISTER NAME USED  00008500
.*   MUST BE EITHER A SELF-DEFINING NUMERIC OR A NAME DEFINED VIA THE   00008600
.*   #REGS MACRO.                                                       00008700
.* - IF ONLY A SINGLE REGISTER IS TO BE RESTORED, THEN IT NEED NOT BE   00008800
.*   ENCLOSED IN PARENTHESES.                                           00008900
.* - IF THE FIRST POSITIONAL OPERAND IS OMITTED, THEN NO REGISTERS ARE  00009000
.*   RESTORED.                                                          00009100
.*                                                                      00009200
.*   THE RC= OPERAND                                                    00009300
.* THIS OPERAND MUST CONSIST OF A SINGLE VALUE EITHER WITHIN OR NOT     00009400
.* WITHIN PARENTHESES. IF ENCLOSED WITHIN PARENTHESES, THEN THE VALUE   00009500
.* IS TREATED AS THE NAME OF A REGISTER CONTAINING A RETURN CODE. IF    00009600
.* NOT ENCLOSED WITHIN PARENTHESES, THEN THE VALUE IS TREATED AS BEING  00009700
.* THE RETURN CODE ITSELF.                                              00009800
.*   IF THE RC= OPERAND SPECIFIES A REGISTER NAME, THEN:                00009900
.* - THE VALUE OF THAT NAME NEED NOT BE DETERMINABLE AT MACRO PASS      00010000
.*   TIME;                                                              00010100
.* - THE REGISTER NAME MAY IDENTIFY ANY REGISTER WHATSOEVER REGUARDLESS 00010200
.*   OF WHICH REGISTERS ARE TO BE RESTORED SINCE IN CASES OF POTENTIAL  00010300
.*   CONFLICT, THE RETURN CODE IS COPIED INTO REGISTER 15 PRIOR TO      00010400
.*   REGISTER RESTORATION;                                              00010500
.* - IT IS ILLOGICAL FOR THE RC= OPERAND TO SPECIFY REGISTER 13.        00010600
.* NOTE THAT IF THE RC= OPERAND IS SPECIFIED BUT THE FIRST POSITIONAL   00010700
.* OPERAND INDICATES THAT REGISTER 15 IS ALSO TO BE RESTORED, THEN      00010800
.* REGISTER 15 IS NOT RESTORED. INSTEAD, IT IS LOADED WITH THE RETURN   00010900
.* CODE VALUE.                                                          00011000
.*   IF THE RC= OPERAND IS OMITTED, THEN NO CODE IS GENERATED TO LOAD   00011100
.* REGISTER 15 WITH A RETURN CODE.                                      00011200
.*                                                                      00011300
.*   THE PFIX= OPERAND                                                  00011400
.* THE #EXIT MACRO ATTEMPTS TO USE REGISTER EQUATES IN ITS EXPANSION    00011500
.* WHEN REFERING TO REGISTERS. THIS IS SO THAT THE USE OF THE REGISTERS 00011600
.* WILL BE INDICATED IN THE CROSS REFERENCE LISTING. THE PFIX= OPERAND  00011700
.* CAN BE USED TO CONTROL THE SET OF EQUATES USED. FOR EXAMPLE, IF      00011800
.* "PFIX=GPR" IS SPECIFIED, THEN "GPR1" WILL BE USED WHENEVER THE       00011900
.* EXPANSION REFERS TO REGISTER 1.                                      00012000
.*   IF THE PFIX= OPERAND IS OMITTED, THEN THE SET OF EQUATES DEFINED   00012100
.* BY THE FIRST PRIOR #REGS MACRO WILL BE USED. IF THERE IS NO PRIOR    00012200
.* #REGS MACRO, THEN NO EQUATES WILL BE USED.                           00012300
.*                                                                      00012400
.*   CONSIDERATIONS                                                     00012500
.* THE #EXIT MACRO WILL GENERATE AN EXPANSION THAT WILL ACCURATELY      00012600
.* RESTORE ALL DESIRED REGISTERS AND SET THE RETURN CODE REGUARDLESS OF 00012700
.* WHETHER OR NOT A FREEMAIN SVC IS ISSUED TO RELEASE THE LOWER SAVE    00012800
.* AREA AND REGUARDLESS OF THE RELATIONSHIP BETWEEN THE RC= OPERAND AND 00012900
.* THE SET OF REGISTERS RESTORED.                                       00013000
.*                                                                      00013100
.*                                                                      00013200
.*                                                                      00013300
.* INNER MACROS USED - #REGS #TEST AND FREEMAIN                         00013400
.*                                                                      00013500
         GBLA  &#TESERR                                                 00013600
         GBLB  &#ENTRNT,&#ENTPLI,&#ENTNUN                     06/84 DBC 00013700
         GBLC  &#TESRET(20),&#ENTSIZ,&#ENTSP                            00013800
         LCLA  &C1,&R1,&R2,&W1,&W2,&ERRCODE                             00013900
         LCLB  &RCLA,&RCST,&RSW(16)                                     00014000
         LCLC  &LNME,&@,&RG(16),&REGNME,&RG2SAVE,&RG4SAVE,&RG5SAVE      00014100
         LCLC  &C,&#                                          06/84 DBC 00014200
&#       SETC  '&SYSNDX'                                      06/84 DBC 00014300
&LNME    SETC  '&NME'                                                   00014400
.*                                                                      00014500
&@       SETC  '&PFIX'                                                  00014600
         AIF   (K'&PFIX NE 0).GOTPFIX                                   00014700
         #TEST PFIX=                                                    00014800
&@       SETC  '&#TESRET(1)'                                            00014900
         AGO   .DONPFIX                                                 00015000
.GOTPFIX #REGS &PFIX,GEN=NO                                             00015100
.DONPFIX ANOP                                                           00015200
.*                                                                      00015300
&C1      SETA  0                                              06/84 DBC 00015400
.RLP     AIF   (&C1 GE N'&R).RFIN                                       00015500
&C1      SETA  &C1+1                                                    00015600
         #TEST DCODE=&R(&C1)                                            00015700
         AIF   (&#TESERR NE 0).END                                      00015800
&W1      SETA  &#TESRET(1)                                              00015900
         AIF   (&W1 EQ 0).RLP                                           00016000
&REGNME  SETC  '&#TESRET(2)'                                            00016100
         #TEST REGS=&REGNME                                             00016200
&ERRCODE SETA  1                                                        00016300
         AIF   (&#TESERR NE 0).REGERR                                   00016400
.REGOK1  ANOP                                                           00016500
&R1      SETA  &#TESRET(1)                                              00016600
&R2      SETA  &R1+3-&R1/14*16                                          00016700
&RG(&R2) SETC  '&REGNME'                                                00016800
&RSW(&R1+1) SETB (1)                                                    00016900
         AIF   (&W1 EQ 1).RLP                                           00017000
         AIF   (&W1 EQ 2).TWOND                                         00017100
         MNOTE 4,'"&R(&C1)" CONTAINS EXCESS INFORMATION.'               00017200
.TWOND   ANOP                                                           00017300
&REGNME  SETC  '&#TESRET(3)'                                            00017400
         #TEST REGS=&REGNME                                             00017500
&ERRCODE SETA  2                                                        00017600
         AIF   (&#TESERR NE 0).REGERR                                   00017700
.REGOK2  ANOP                                                           00017800
&W2      SETA  &#TESRET(1)                                              00017900
&RSW(&W2+1) SETB (1)                                                    00018000
.ENTLP   AIF   (&R1 EQ &#TESRET(1)).ENTEND                              00018100
&R1      SETA  &R1+1                                                    00018200
&R2      SETA  &R2+1                                                    00018300
         AIF   (&R1 LE 15).R1OK                                         00018400
&R1      SETA  0                                                        00018500
.R1OK    AIF   (&R2 LE 16).R2OK                                         00018600
&R2      SETA  1                                                        00018700
.R2OK    ANOP                                                           00018800
&RG(&R2) SETC  '&@&R1'                                                  00018900
         AGO   .ENTLP                                                   00019000
.ENTEND  ANOP                                                           00019100
&RG(&R2) SETC  '&REGNME'                                                00019200
         AGO   .RLP                                                     00019300
.REGERR  AIF   (&#TESRET(1) GE 0 OR &#TESRET(1) LT 0).REGVALU           00019400
         MNOTE 0,'THE ABOVE ERROR IS NOT DUE TO A BUG IN THE MACRO.'    00019500
         MNOTE 8,'THE VALUE OF "&REGNME" IS NOT DETERMINABLE.'          00019600
         MEXIT                                                          00019700
.REGVALU AIF   (&#TESRET(1) GE 0 AND &#TESRET(1) LE 15).REGOK           00019800
         MNOTE 8,'THE VALUE OF "&REGNME" IS OUTSIDE THE RANGE OF 0 ... *00019900
               15'                                                      00020000
         MEXIT                                                          00020100
.REGOK   AIF   (&ERRCODE EQ 1).REGOK1                                   00020200
         AGO   .REGOK2                                                  00020300
.RFIN    AIF   (NOT &#ENTPLI).RGOK                                      00020400
         AIF   (NOT &RSW(1)).RG0OK                                      00020500
         MNOTE 4,'&RG(3) NEEDED BY THE EXIT LINKAGE - NOT RESTORED.'    00020600
.RG0OK   AIF   (NOT &RSW(2)).RG1OK                                      00020700
         MNOTE 4,'&RG(4) NEEDED BY THE EXIT LINKAGE - NOT RESTORED.'    00020800
.RG1OK   ANOP                                                           00020900
&RG(3)   SETC  ''                                                       00021000
&RG(4)   SETC  ''                                                       00021100
.RGOK    ANOP                                                           00021200
.*                                                                      00021300
&RG(16)  SETC  ''                                                       00021400
.*                                                                      00021500
         AIF   (K'&RC EQ 0).NORC                                        00021600
&RG2SAVE SETC  '&RG(2)'                                                 00021700
&RG(2)   SETC  ''                                                       00021800
         AIF   ('&RG2SAVE' NE '').RG2SOK                                00021900
&RG2SAVE SETC  '&@.15'                                                  00022000
.RG2SOK  ANOP                                                           00022100
.*                                                                      00022200
         AIF   (NOT &RSW(16)).NOPRBLM                                   00022300
         MNOTE 4,'&RG2SAVE SET TO THE RETURN CODE - NOT RESTORED.'      00022400
.NOPRBLM ANOP                                                           00022500
.*                                                                      00022600
         AIF   ('&RC' EQ '&RC(1)').RCNTRG                               00022700
         #TEST REGS=&RC(1)                                              00022800
         AIF   (&#TESERR NE 0).LOADRC                                   00022900
         AIF   ('&#TESRET(1)' NE '13').RCOK                             00023000
         MNOTE 4,'"RC=&RC" IS ILLOGICAL.'                               00023100
.RCOK    AIF   ('&#TESRET(1)' EQ '15').NORC                             00023200
.LOADRC  AIF   ('&RG(1)' EQ '' OR '&RG(3)' EQ '' OR &#ENTPLI).RCLR      00023300
&RCST    SETB  (1)                                                      00023400
         AGO   .NORC                                                    00023500
.RCLR    ANOP                                                           00023600
&LNME    LR    &@.15,&RC(1)        LOAD THE RETURN CODE                 00023700
&LNME    SETC  ''                                                       00023800
         AGO   .NORC                                                    00023900
.RCNTRG  ANOP                                                           00024000
&RCLA    SETB  (1)                                                      00024100
         AIF   ('&RG(3)' EQ '' OR '&RG(1)' EQ '').NORC                  00024200
&RG(2)   SETC  '&RG2SAVE'                                               00024300
.NORC    ANOP                                                           00024400
.*                                                                      00024500
         AIF   (NOT &#ENTPLI).NOTPLI2                                   00024600
&LNME    LR    &@.0,&@.13          COPY OUR DSA POINTER                 00024700
&LNME    SETC  ''                                                       00024800
.NOTPLI2 ANOP                                                           00024900
.*                                                                      00025000
         AIF   (NOT &#ENTRNT OR '&RG(4)' EQ '').NOLRR1                  00025100
&LNME    LR    &@.1,&@.13          GET SAVE AREA ADDRESS FOR FREEMAIN   00025200
&LNME    SETC  ''                                                       00025300
.NOLRR1  ANOP                                                           00025400
.*                                                                      00025500
         AIF   (&#ENTNUN).NOLSA                               06/84 DBC 00025600
&LNME    L     &@.13,4(,&@.13)     POINT TO THE HIGHER SAVE AREA        00025700
&LNME    SETC  ''                                             06/84 DBC 00025800
.NOLSA   ANOP                                                 06/84 DBC 00025900
         AIF   (NOT &RCST).NORCST                                       00026000
&LNME    ST    &RC(1),16(,&@.13)   STORE THE RC FOR LATER     06/84 DBC 00026100
&LNME    SETC  ''                                             06/84 DBC 00026200
&RG(2)   SETC  '&RG2SAVE'                                               00026300
.NORCST  ANOP                                                           00026400
.*                                                                      00026500
         AIF   (NOT &#ENTRNT).NTRENT                                    00026600
         AIF   (NOT &RCLA).RG2OK                                        00026700
&RG(2)   SETC  'X'                                                      00026800
         AIF   ('&RG(1)&RG(3)' NE '').RG2OK                             00026900
&RG(2)   SETC  ''                                                       00027000
.RG2OK   ANOP                                                           00027100
&RG4SAVE SETC  '&RG(4)'                                                 00027200
&RG5SAVE SETC  '&RG(5)'                                                 00027300
&RG(5)   SETC  'X'                                                      00027400
&C1      SETA  0                                                        00027500
.STMLP   AIF   (&C1 GE 4).STMEND                                        00027600
&C1      SETA  &C1+1                                                    00027700
         AIF   ('&RG(&C1)' NE '').STMLP                                 00027800
&R1      SETA  &C1+13-(&C1+13)/16*16                                    00027900
&R2      SETA  &R1-1                                                    00028000
&W1      SETA  &C1*4+8                                                  00028100
.STMLP2  ANOP                                                           00028200
&R2      SETA  &R2+1                                                    00028300
         AIF   (&R2 LE 15).STMR2OK                                      00028400
&R2      SETA  0                                                        00028500
.STMR2OK ANOP                                                           00028600
&RG(&C1) SETC  '&@&R2'                                                  00028700
&C1      SETA  &C1+1                                                    00028800
         AIF   ('&RG(&C1)' EQ '').STMLP2                                00028900
         AIF   (&R1 EQ &R2).ST                                          00029000
&C       SETC  '&@&R1,&@&R2,&W1.(&@.13)'                      06/84 DBC 00029100
&LNME    STM   &C                  SAVE AGAINST FREEMAIN      06/84 DBC 00029200
&LNME    SETC  ''                                             06/84 DBC 00029300
         AGO   .STMLP                                                   00029400
.ST      ANOP                                                 06/84 DBC 00029500
&LNME    ST    &@&R1,&W1.(,&@.13)  SAVE AGAINST FREEMAIN      06/84 DBC 00029600
&LNME    SETC  ''                                             06/84 DBC 00029700
         AGO   .STMLP                                                   00029800
.STMEND  ANOP                                                           00029900
&RG(5)   SETC  '&RG5SAVE'                                               00030000
.GTR1M   AIF   ('&RG4SAVE' NE '').NOGTR1                                00030100
&LNME    L     &@.1,8(,&@.13)      GET RSA PTR FOR FREEMAIN   06/84 DBC 00030200
&LNME    SETC  ''                                             06/84 DBC 00030300
.NOGTR1  ANOP                                                 06/84 DBC 00030400
&LNME    L     &@.0,E&#.LEN        GET RSA LEN (AND SUBPOOL)  06/84 DBC 00030500
&LNME    SETC  ''                                             06/84 DBC 00030600
         MNOTE '         FREEMAIN R,A=(1),LV=(0)'             06/84 DBC 00030700
         FREEMAIN R,A=(1),LV=(0)                              06/84 DBC 00030800
.NTRENT  ANOP                                                           00030900
.*                                                                      00031000
&C1      SETA  0                                                        00031100
.LMLP    AIF   (&C1 GE 16).SETRCM                                       00031200
&C1      SETA  &C1+1                                                    00031300
         AIF   ('&RG(&C1)' EQ '').LMLP                                  00031400
&R1      SETA  &C1                                                      00031500
&W1      SETA  &C1*4+8                                                  00031600
.LMLP2   ANOP                                                           00031700
&C1      SETA  &C1+1                                                    00031800
         AIF   ('&RG(&C1)' NE '').LMLP2                                 00031900
         AIF   (&R1 EQ &C1-1).L                                         00032000
&C       SETC  '&RG(&R1),&RG(&C1-1),&W1.(&@.13)'              06/84 DBC 00032100
&LNME    LM    &C                  RESTORE REGISTERS          06/84 DBC 00032200
&LNME    SETC  ''                                             06/84 DBC 00032300
         AGO   .LMLP                                                    00032400
.L       ANOP                                                 06/84 DBC 00032500
&C       SETC  '&RG(&R1),&W1.(,&@.13)'                        06/84 DBC 00032600
&LNME    L     &C                  RESTORE THE REGISTER       06/84 DBC 00032700
&LNME    SETC  ''                                             06/84 DBC 00032800
         AGO   .LMLP                                                    00032900
.SETRCM  ANOP                                                           00033000
.*                                                                      00033100
         AIF   (NOT &RCLA).RETURN                                       00033200
         AIF   ('&RC' EQ '0').SR                                        00033300
&LNME    LA    &@.15,&RC           GET THE RETURN CODE        06/84 DBC 00033400
&LNME    SETC  ''                                             06/84 DBC 00033500
         AGO   .RETURN                                                  00033600
.SR      ANOP                                                 06/84 DBC 00033700
&LNME    SLR   &@.15,&@.15         ZERO THE RETURN CODE       06/84 DBC 00033800
&LNME    SETC  ''                                             06/84 DBC 00033900
.RETURN  ANOP                                                           00034000
.*                                                                      00034100
         AIF   (NOT &#ENTPLI).NOTPLI3                                   00034200
&LNME    BALR  &@.1,&@.14          RETURN TO CALLER           06/84 DBC 00034300
         MEXIT                                                          00034400
.NOTPLI3 ANOP                                                           00034500
.*                                                                      00034600
&LNME    BR    &@.14               RETURN                     10/84 DBC 00034700
.*                                                            06/84 DBC 00034800
         AIF   (NOT &#ENTRNT).END                             06/84 DBC 00034900
         AIF   ('&#ENTSP' EQ '').NOSPOOL                      06/84 DBC 00035000
         DS    0F                  ALIGN                      06/84 DBC 00035100
&C       SETC  'AL1(&#ENTSP),AL3(&#ENTSIZ)'                   06/84 DBC 00035200
E&#.LEN  DC    &C                  RSA SUBPOOL AND LENGTH     06/84 DBC 00035300
         MEXIT                                                06/84 DBC 00035400
.NOSPOOL ANOP                                                           00035500
E&#.LEN  DC    A(&#ENTSIZ)         RSA LENGTH                 06/84 DBC 00035600
.END     MEND                                                           00035700
         MACRO                                                          00000100
&NME     #PUT  &MSG,&PFIX=,&SUBAD=,&MF=                                 00000200
.*                                                                      00000300
.*                                                                      00000400
.*                                                                      00000500
.* LAST CHANGE DATE - OCTOBER 18, 1983                                  00000600
.*                  - MAILING ADDRESS CHANGE                            00000700
.*                                                                      00000800
.* LAST CHANGE DATE - APRIL 21, 1981                                    00000900
.*                  - MACRO NAME CHANGED FROM $PUT TO #PUT.             00001000
.*                                                                      00001100
.* LAST CHANGE DATE - APRIL 20, 1981                                    00001200
.*                  - "#REGS GEN=NO" SUPPORT ADDED.                     00001300
.*                                                                      00001400
.* LAST CHANGE DATE - JULY 18, 1980                                     00001500
.*                  - INDIRECT ADDRESSING IS NOW INDICATED BY A         00001600
.*                    TRAILING PERCENT SIGN (%) RATHER THAN A LEADING   00001700
.*                    ONE.                                              00001800
.*                                                                      00001900
.* LAST CHANGE DATE - JANUARY 12, 1977                                  00002000
.*                  - HANDLING OF THE SUBAD= OPERAND IS REWRITTEN.      00002100
.*                  - MAILING ADDRESS CHANGE.                           00002200
.*                                                                      00002300
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING    00002400
.* IT MAY BE ADDRESSED TO:                                              00002500
.*       RR#2 BOX 712                                                   00002600
.*       AFTON, VA. 22920                                               00002700
.*                                                                      00002800
         GBLA  &#TESERR                                                 00002900
         GBLC  &#PUTSUB                                                 00003000
         GBLC  &#TESRET(20)                                             00003100
         LCLA  &A1                                                      00003200
         LCLC  &@,&C1,&N                                                00003300
         AIF   ('&SUBAD' EQ '').SUBOK                                   00003400
         AIF   ('&SUBAD(1)' NE '&SUBAD').SUBOK                          00003500
&#PUTSUB SETC  '&SUBAD'                                                 00003600
.SUBOK   AIF   ('&MF(1)' EQ 'INIT').MEND                                00003700
&N       SETC  '&NME'                                                   00003800
.*                                                                      00003900
&@       SETC  '&PFIX'                                                  00004000
         AIF   ('&PFIX' NE '').PFIXOK                                   00004100
         #TEST PFIX=                                                    00004200
&@       SETC  '&#TESRET(1)'                                            00004300
         AGO   .DONPFIX                                                 00004400
.PFIXOK  #REGS &PFIX,GEN=NO                                             00004500
.DONPFIX ANOP                                                           00004600
.*                                                                      00004700
         AIF   ('&MSG(1)' EQ '&MSG').NOTREG                             00004800
         #TEST REGS=&MSG(1)                                             00004900
         AIF   (&#TESERR NE 0).LR                                       00005000
         AIF   (&#TESRET(1) EQ 1).BAL                                   00005100
.LR      ANOP                                                           00005200
&N       LR    &@.1,&MSG(1)        --> MESSAGE LENGTH FIELD             00005300
&N       SETC  ''                                                       00005400
         AGO   .BAL                                                     00005500
.NOTREG  AIF   ('&MSG' EQ '').BAL                                       00005600
         AIF   ('&MSG'(K'&MSG,1) EQ '%').MSGINDR                        00005700
&N       LA    &@.1,&MSG-1         --> MESSAGE LENGTH FIELD             00005800
         AGO   .MSGOK                                                   00005900
.MSGINDR ANOP                                                           00006000
&C1      SETC  '&MSG'(1,K'&MSG-1)                                       00006100
&N       L     &@.1,=A(&C1-1)      --> MESSAGE LENGTH FIELD             00006200
.MSGOK   ANOP                                                           00006300
&N       SETC  ''                                                       00006400
.BAL     AIF   ('&SUBAD(1)' NE '&SUBAD').BALR                           00006500
         AIF   ('&#PUTSUB'(1,1) EQ '%').BALINDR                         00006600
&N       BAL   &@.14,&#PUTSUB      GO DISPLAY THE MESSAGE               00006700
         AGO   .MEND                                                    00006800
.BALR    ANOP                                                           00006900
&N       BALR  &@.14,&SUBAD(1)     GO DISPLAY THE MESSAGE               00007000
         AGO   .MEND                                                    00007100
.BALINDR ANOP                                                           00007200
&A1      SETA  0                                                        00007300
.LP1     ANOP                                                           00007400
&A1      SETA  &A1+1                                                    00007500
         AIF   ('&#PUTSUB'(1,&A1) NE '&#PUTSUB').LP1                    00007600
&C1      SETC  '&#PUTSUB'(2,&A1-1)                                      00007700
&N       L     &@.15,=A(&C1)       --> MESSAGE PRINTING ROUTINE         00007800
         BALR  &@.14,&@.15         GO DISPLAY THE MESSAGE               00007900
.MEND    MEND                                                           00008000
         MACRO                                                          00000100
&N       #DCBD &DSORG=,&DEVD=                                       DBC 00000200
.*                                                                  DBC 00000300
.*                                                                  DBC 00000400
.*                                                                      00000500
.* LAST CHANGE DATE - APRIL 21, 1981                                    00000600
.*                  - MACRO NAME CHANGED FROM $DCBD TO #DCBD            00000700
.*                                                                  DBC 00000800
.* LAST CHANGE DATE - FEBRUARY 2, 1977                              DBC 00000900
.*                  - MAILING ADDRESS CHANGE.                       DBC 00001000
.*                                                                  DBC 00001100
.* LAST CHANGE DATE - APRIL 20, 1976                                    00001200
.*                                                                  DBC 00001300
.* THIS MACRO IS A MODIFICATION OF IBM'S DCBD MACRO FROM R21.7 OF   DBC 00001400
.* OS. ANY QUESTIONS CONCERNING IT MAY BE ADDRESSED TO:             DBC 00001500
.*       809 WHITNEY AVE.                                           DBC 00001600
.*       NEW HAVEN, CT. 06511                                       DBC 00001700
.*                                                                  DBC 00001800
.*                                                                  DBC 00001900
.*                                                                  DBC 00002000
.*   THIS MACRO DUPLICATES THE FUNCTION OF IBM'S DCBD MACRO BUT     DBC 00002100
.* WITH TWO IMPORTANT DIFFERENCES:                                  DBC 00002200
.* - THE DSECT STATEMENT IS NOT GENERATED.                          DBC 00002300
.* - A FACILITY IS PROVIDED WHEREBY THE FIRST THREE CHARACTERS      DBC 00002400
.*   (I.E. "DCB") OF EACH FIELD NAME (BUT NOT BIT NAME) CAN BE      DBC 00002500
.*   REPLACED BY ANY ONE, TWO, OR THREE CHARACTERS OF YOUR CHOICE.  DBC 00002600
.* AS A RESULT, THE #DCBD MACRO CAN BE USED ANY NUMBER OF TIMES IN  DBC 00002700
.* AN ASSEMBLY. REFER TO THE EXAMPLES GIVEN BELOW FOR A             DBC 00002800
.* PARTICULARLY USEFUL TECHNIQUE.                                   DBC 00002900
.*                                                                  DBC 00003000
.*   THIS MACRO USES THE SAME OPERANDS AS DOES THE DCBD MACRO, SO   DBC 00003100
.* PLEASE REFER TO IBM'S DATA MANAGEMENT MACROS MANUAL FOR FURTHER  DBC 00003200
.* DETAILS CONCERNING THEM.                                         DBC 00003300
.*   IN ADDITION TO THE OPERANDS, THE #DCBD MACRO RECOGNIZES THE    DBC 00003400
.* NAME FIELD WHICH SHOULD EITHER BE OMITTED OR CONTAIN A ONE, TWO, DBC 00003500
.* OR THREE CHARACTER NAME. IF THE NAME FIELD IS OMITTED, THEN THE  DBC 00003600
.* CHARACTERS "DCB" ARE USED TO PREFIX ALL FIELD NAMES. IF THE NAME DBC 00003700
.* FIELD IS GIVEN, THEN IT RATHER THAN "DCB" IS USED TO PREFIX ALL  DBC 00003800
.* FIELD NAMES. IF THE GIVEN NAME IS LONGER THAN THREE CHARACTERS,  DBC 00003900
.* THEN ASSEMBLY ERRORS ARE LIKELY TO RESULT.                       DBC 00004000
.*                                                                  DBC 00004100
.*   AS INDICATED ABOVE, THE GIVEN NAME FIELD AFFECTS ONLY FIELD    DBC 00004200
.* NAMES (E.G. "DCBOFLGS" MIGHT BE CHANGED TO "RDROFLGS"), BUT BIT  DBC 00004300
.* NAMES (E.G. "DCBOFOPN") ARE NEVER AFFECTED, SINCE THE PREFIX     DBC 00004400
.* "DCB" WILL BE USED REGUARDLESS OF WHAT IS GIVEN IN THE NAME      DBC 00004500
.* FIELD, SO IN ORDER TO TEST, FOR EXAMPLE, IF A DCB HAS BEEN       DBC 00004600
.* OPENED, YOU MIGHT USE  " TM RDROFLGS,DCBOFOPN ".                 DBC 00004700
.*   IN ADDITION, A PARTICULAR BIT NAME WILL BE GENERATED ONLY BY   DBC 00004800
.* THE FIRST #DCBD MACRO THAT REQUIRES IT, THUS A PARTICULAR BIT    DBC 00004900
.* NAME WILL BE GENERATED ONLY ONCE PER ASSEMBLY.                   DBC 00005000
.*                                                                  DBC 00005100
.*   EXAMPLES - I HAVE FOUND THAT THE MOST VALUABLE USE OF THE      DBC 00005200
.* #DCBD MACRO HAS BEEN IN SIMPLIFYING THE PROCEDURE FOR REFERING   DBC 00005300
.* TO DCB FIELDS WITHOUT USING A SPECIAL BASE REGISTER. CONSIDER    DBC 00005400
.* THE FOLLOWING EXAMPLES:                                          DBC 00005500
.*                                                                  DBC 00005600
.*       PRINT NOGEN (IF YOU'VE SEEN ONE DCB, YOU'VE SEEN THEM ALL) DBC 00005700
.*                                                                  DBC 00005800
.*RDR    #DCBD DSORG=QS                                             DBC 00005900
.*       ORG   RDRDCB                                               DBC 00006000
.*SYSIN  DCB   DDNAME=SYSIN,DSORG=PS, ...                           DBC 00006100
.*                                                                  DBC 00006200
.*SYSPRINT DCB DDNAME=SYSPRINT,DSORG=PS, ...                        DBC 00006300
.*       ORG   SYSPRINT                                             DBC 00006400
.*PRT    #DCBD DSORG=QS                                             DBC 00006500
.*                                                                  DBC 00006600
.*       ORG   *-16                                                 DBC 00006700
.*UT1    #DCBD DSORG=IS                                             DBC 00006800
.*       ORG   UT1DCB+16                                            DBC 00006900
.*SYSUT1 DCB   DDNAME=SYSUT1,DSORG=IS, ...                          DBC 00007000
.*                                                                  DBC 00007100
.*SYSUT2 DCB   DDNAME=SYSUT2,DSORG=IS, ...                          DBC 00007200
.*       ORG   SYSUT2                                               DBC 00007300
.*UT2    #DCBD DSORG=IS                                             DBC 00007400
.*                                                                  DBC 00007500
.*       PRINT GEN                                                  DBC 00007600
.*                                                                  DBC 00007700
.*   IN THE FIRST AND THIRD EXAMPLES (RDR AND UT1), THE #DCBD MACRO DBC 00007800
.* GENERATES FIELD NAMES (E.G. "RDROFLGS" AND "UT1OFLGS"); THE ORG  DBC 00007900
.* STATEMENT RESETS THE LOCATION COUNTER TO THE START OF THE FIELD  DBC 00008000
.* NAMES; AND THE DCB MACRO GENERATES THE ACTUAL DCB'S ON TOP OF    DBC 00008100
.* THE FIELD NAMES.                                                 DBC 00008200
.*   IN THE SECOND AND FOURTH EXAMPLES (PRT AND UT2) THE DCB MACRO  DBC 00008300
.* GENERATES THE DCB CODE; THE ORG STATEMENT RELOCATES BACK TO THE  DBC 00008400
.* START OF THE DCB; AND THEN THE #DCBD MACRO GENERATES THE FIELD   DBC 00008500
.* NAMES ON TOP OF THE DCB CODE. NOTE THAT THE DCB CODE IS NOT      DBC 00008600
.* DESTROYED BECAUSE THE #DCBD MACRO GENERATES ONLY DS              DBC 00008700
.* INSTRUCTIONS.                                                    DBC 00008800
.*   BIT NAMES ARE GENERATED ONLY BY THE FIRST AND THIRD #DCBD      DBC 00008900
.* MACROS. THE SECOND MACRO HAS THE IDENTICAL EXPANSION PATH AS THE DBC 00009000
.* FIRST, SO ALL BIT NAMES THAT IT MIGHT HAVE GENERATED WOULD HAVE  DBC 00009100
.* BEEN REDUNDANT, SO NONE IS GENERATED. THE SAME CAN BE SAID FOR   DBC 00009200
.* THE FOURTH #DCBD MACRO WITH RESPECT TO THE THIRD. THE THIRD      DBC 00009300
.* #DCBD MACRO HAS A DIFFERENT EXPANSION PATH FROM EITHER OF THE    DBC 00009400
.* PRECEEDING TWO, SO SOME OF THE BIT NAMES THAT IT WOULD HAVE      DBC 00009500
.* GENERATED ARE NOT REDUNDANT. THOSE BIT NAMES THAT ARE REDUNDANT  DBC 00009600
.* ARE NOT GENERATED. THOSE BIT NAMES THAT ARE NOT REDUNDANT ARE    DBC 00009700
.* GENERATED.                                                       DBC 00009800
.*   BOTH THE THIRD AND FOURTH DCB'S ARE ISAM, SO ONLY A SHORT      DBC 00009900
.* DEVICE DEPENDANT SECTION IS GENERATED BY THE DCB MACRO. TO       DBC 00010000
.* ACCOMPLISH THIS, THE DCB MACRO RELOCATES BACK 16 BYTES,          DBC 00010100
.* GENERATES THE DCB NAME, RELOCATES FORWARD 16 BYTES, AND THEN     DBC 00010200
.* GENERATES THE DCB CODE. ON THE OTHER HAND, THE PRIMARY FUNCTION  DBC 00010300
.* OF THE DCBD MACRO AND, THEREFORE, THE #DCBD MACRO IS TO CREATE   DBC 00010400
.* DSECTS. BECAUSE OF THIS IT WOULD BE LOGICALLY INCONSISTANT FOR   DBC 00010500
.* THEM TO ATTEMPT TO RELOCATE BACKWARD PRIOR TO DEFINING THE       DBC 00010600
.* STARTING POINT. THEREFORE, IN THE CASE OF ISAM (AND BDAM, AND    DBC 00010700
.* EXCP, ETC.), THE DCBD AND #DCBD MACROS FIRST DEFINE THE STARTING DBC 00010800
.* NAME AND THEN RELOCATE FORWARD (USUALLY 16 BYTES) BEFORE         DBC 00010900
.* DEFINING FIELD NAMES. IT IS FOR THIS REASON THAT THE EXTRA ORG   DBC 00011000
.* STATEMENT APPEARS IN THE THIRD EXAMPLE ABOVE. IN THE FOURTH      DBC 00011100
.* EXAMPLE THE EXTRA ORG STATEMENT IS NOT NEEDED.                   DBC 00011200
.*                                                                  DBC 00011300
.*                                                                  DBC 00011400
.*                                                                  DBC 00011500
.* INNER MACROS USED - #DSORG                                       DBC 00011600
.*                                                                  DBC 00011700
         GBLB  &#DCBDSG                                             DBC 00011800
         GBLB  &#DCBSW(150)                                         DBC 00011900
         GBLB  &ONESW                                                   00012000
         GBLA  &IEZBITS                                                 00012100
         LCLA  &A0            FOR DSORG= AND DEVD= ANALYSIS LOOPS       00012200
         LCLB  &DSORGIS       SET BY DSORG=IS - ISAM                    00012300
         LCLB  &DSORGBX       SET BY DSORG=BX OR CX - BTAM              00012400
         LCLB  &DSORGDA       SET BY DSORG=DA - BDAM                    00012500
         LCLB  &DSORGQX       SET BY DSORG=QX OR CX - QTAM              00012600
         LCLB  &DSORGCQ       SET BY DSORG=CQ - QTAM DIRECT ACCESS      00012700
.*                            MESSAGE QUEUE                             00012800
         LCLB  &DSORGMQ       SET BY DSORG=MQ - QTAM PROBLEM PROGRAM    00012900
.*                            MESSAGE QUEUE INTERFACE                   00013000
         LCLB  &DSORGXA       SET BY DSORG=XA - EXCP WITH APPENDAGES    00013100
         LCLB  &DSORGQS       SET BY DSORG=QS OR PS - QSAM              00013200
         LCLB  &DSORGBS       SET BY DSORG=BS OR PS OR PO - BSAM,BPAM   00013300
         LCLB  &DSORGXE       SET BY DSORG=XE - EXCP WITH EXTENSION     00013400
         LCLB  &DSORGLR       SET BY DSORG=LR - DCBLRECL FIELD ONLY     00013500
         LCLB  &DSORGGS       SET BY DSORG=GS - GAM                     00013600
         LCLB  &DSORGTX       SET BY DSORG=TX - TCAM LINE GROUP         00013700
         LCLB  &DSORGTQ       SET BY DSORG=TQ - TCAM MESSAGE QUEUE      00013800
         LCLB  &DSORGTR       SET BY DSORG=TR 3705 LINE GROUP    S22024 00013900
         LCLB  &DEVDDA        DIRECT ACCESS                             00014000
         LCLB  &DEVDTA        MAGNETIC TAPE                             00014100
         LCLB  &DEVDPT        PAPER TAPE                                00014200
         LCLB  &DEVDRD        READER OR PUNCH, DEVD=RD OR PC            00014300
         LCLB  &DEVDPR        PRINTER                                   00014400
         LCLB  &DEVDBS        BINARY SYNCHRONOUS                        00014500
         LCLB  &DEVDWT        WORLD TRADE TELEGRAPH                     00014600
         LCLB  &DEVDMR        MAGNETIC CARD READER                      00014700
         LCLB  &DEVDOR        OPTICAL READER                            00014800
         LCLC  &C0            SET TO EACH VALUE OF DSORG AND DEVD       00014900
         LCLB  &LSW(150)                                            DBC 00015000
         LCLC  &P                                                   DBC 00015100
&A0      SETA  0                                                    DBC 00015200
.LPXYZ   AIF   (&A0 EQ 150).ENDXYZ                                  DBC 00015300
&A0      SETA  &A0+1                                                DBC 00015400
&#DCBSW(&A0) SETB (&#DCBSW(&A0) OR &ONESW)                          DBC 00015500
         AGO   .LPXYZ                                               DBC 00015600
.ENDXYZ  ANOP                                                       DBC 00015700
&#DCBDSG SETB  (&#DCBDSG OR &ONESW)                                 DBC 00015800
&P       SETC  'DCB'                                                DBC 00015900
         AIF   ('&N' EQ '').GOTPFIX                                 DBC 00016000
&P       SETC  '&N'                                                 DBC 00016100
.GOTPFIX ANOP                                                       DBC 00016200
         AIF   (&IEZBITS GT 0).SETBTS                                   00016300
&IEZBITS SETA  1                                                        00016400
BIT0     EQU   128                                                      00016500
BIT1     EQU   64                                                       00016600
BIT2     EQU   32                                                       00016700
BIT3     EQU   16                                                       00016800
BIT4     EQU   8                                                        00016900
BIT5     EQU   4                                                        00017000
BIT6     EQU   2                                                        00017100
BIT7     EQU   1                                                        00017200
.SETBTS  ANOP                                                           00017300
.*                                                                      00017400
.*                  ANALYZE DSORG OPERAND                               00017500
.*                                                                      00017600
&A0      SETA  N'&DSORG       SET NUMBER OF DSORG FLEMENTS              00017700
.A1      AIF   (&A0 LE 0).D0  IF ZERO, LOOP FINISHED                    00017800
&C0      SETC  '&DSORG(&A0)'  SET TO A DSORG ELEMENT                    00017900
.*                                                                      00018000
.*                  TEST FOR VALID DSORG ELEMENT                        00018100
.*                                                                      00018200
         AIF   ('&C0' EQ 'IS' OR '&C0' EQ 'PS' OR '&C0' EQ 'BS' OR     *00018300
               '&C0' EQ 'QS' OR '&C0' EQ 'DA' OR '&C0' EQ 'CX' OR      *00018400
               '&C0' EQ 'CQ' OR '&C0' EQ 'MQ' OR '&C0' EQ 'LR').A2      00018500
         AIF   ('&C0' EQ 'XE' OR '&C0' EQ 'XA' OR '&C0' EQ 'PO' OR     *00018600
               '&C0' EQ 'BX' OR '&C0' EQ 'QX' OR '&C0' EQ 'GS' OR      *00018700
               '&C0' EQ 'TX' OR '&C0' EQ 'TQ' OR '&C0' EQ '').A2        00018800
         AIF   ('&C0' EQ 'TR').A2                                S22024 00018900
         IHBERMAC 156,DSORG,&C0                                         00019000
         AGO   .AA                                                      00019100
.*                                                                      00019200
.*                  SET VARIABLES FOR DSORG                             00019300
.*                                                                      00019400
.A2      ANOP                                                           00019500
&DSORGIS SETB  (&DSORGIS OR '&C0' EQ 'IS')                              00019600
&DSORGBX SETB  (&DSORGBX OR '&C0' EQ 'BX' OR '&C0' EQ 'CX')             00019700
&DSORGDA SETB  (&DSORGDA OR '&C0' EQ 'DA')                              00019800
&DSORGQX SETB  (&DSORGQX OR '&C0' EQ 'QX' OR '&C0' EQ 'CX')             00019900
&DSORGCQ SETB  (&DSORGCQ OR '&C0' EQ 'CQ')                              00020000
&DSORGMQ SETB  (&DSORGMQ OR '&C0' EQ 'MQ')                              00020100
&DSORGXA SETB  (&DSORGXA OR '&C0' EQ 'XA')                              00020200
&DSORGQS SETB  (&DSORGQS OR '&C0' EQ 'QS' OR '&C0' EQ 'PS')             00020300
&DSORGBS SETB  (&DSORGBS OR '&C0' EQ 'BS' OR '&C0' EQ 'PS' OR '&C0' EQ *00020400
               'PO')                                                    00020500
&DSORGXE SETB  (&DSORGXE OR '&C0' EQ 'XE')                              00020600
&DSORGLR SETB  (&DSORGLR OR '&C0' EQ 'LR')                              00020700
&DSORGGS SETB  (&DSORGGS OR '&C0' EQ 'GS')                              00020800
&DSORGTX SETB  (&DSORGTX OR '&C0' EQ 'TX')                              00020900
&DSORGTQ SETB  (&DSORGTQ OR '&C0' EQ 'TQ')                              00021000
&DSORGTR SETB  (&DSORGTR OR '&C0' EQ 'TR')                       S22024 00021100
.AA      ANOP                                                           00021200
&A0      SETA  &A0-1          DECREMENT ELEMENT COUNTER                 00021300
         AGO   .A1            TO DO NEW LOOP                            00021400
.*                                                                      00021500
.*                  TEST FOR ANY VALID DSORG OPERAND                    00021600
.*                                                                      00021700
.D0      AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *00021800
               &DSORGCQ OR &DSORGMQ OR &DSORGXA OR &DSORGQS OR         *00021900
               &DSORGBS OR &DSORGXE OR &DSORGLR OR &DSORGGS).D0A        00022000
         AIF   (&DSORGTX OR &DSORGTQ OR &DSORGTR).D0A            S22024 00022100
         IHBERMAC 173                                                   00022200
.D0A     ANOP                                                           00022300
         AIF   (T'&DEVD NE 'O').D1      BRANCH IF DEVD CODED       1318 00022400
         AIF   (&DSORGGS).B1                                            00022500
         AIF   (NOT (&DSORGQS OR &DSORGBS OR &DSORGXE)).B1              00022600
.*                                                                      00022700
.*                  SET DEVD DEFAULTS                                   00022800
.*                                                                      00022900
&DEVDDA  SETB  1                                                        00023000
&DEVDTA  SETB  ('&DSORG' NE '(PO)')                                     00023100
&DEVDPT  SETB  ('&DSORG' NE '(PO)')                                     00023200
&DEVDPR  SETB  ('&DSORG' NE '(PO)')                                     00023300
&DEVDRD  SETB  ('&DSORG' NE '(PO)')                                     00023400
&DEVDMR  SETB  ('&DSORG' NE '(PO)')                                     00023500
&DEVDOR  SETB  ('&DSORG' NE '(PO)')                                     00023600
         AIF   ('&DSORG' EQ '(PO)').D1                                  00023700
         IHBERMAC 174                                                   00023800
         AGO   .B1                                                      00023900
.D1      ANOP                                                           00024000
.*                                                                      00024100
.*                  ANALYZE DEVD OPERAND                                00024200
.*                                                                      00024300
&A0      SETA  N'&DEVD        SET ELEMENT COUNT                         00024400
.D2      ANOP                                                           00024500
         AIF   (&A0 LE 0).B1  IF ZERO, LOOP FINISHED                    00024600
&C0      SETC  '&DEVD(&A0)'   SET TO A DEVD ELEMENT                     00024700
.*                                                                      00024800
.*                  TEST FOR VALID DEVD ELEMENT                         00024900
.*                                                                      00025000
         AIF   ('&C0' EQ 'DA' OR '&C0' EQ 'TA' OR '&C0' EQ 'PT' OR     *00025100
               '&C0' EQ 'PR' OR '&C0' EQ 'RD' OR '&C0' EQ 'PC' OR      *00025200
               '&C0' EQ 'BS' OR '&C0' EQ 'WT').D3                       00025300
         AIF   ('&C0' EQ 'MR' OR '&C0' EQ 'OR').D3                      00025400
         AIF   ('&C0' EQ '').D4                                         00025500
         IHBERMAC 157,DEVD,&C0                                          00025600
         AGO   .D4            TO DO NEW LOOP                            00025700
.*                                                                      00025800
.*                  SET VARIABLES FOR DEVD                              00025900
.*                                                                      00026000
.D3      ANOP                                                           00026100
&DSORGXE SETB  (&DSORGXE OR T'&DSORG EQ 'O') FORCE EXCP EXTENDED        00026200
&DEVDDA  SETB  (&DEVDDA OR '&C0' EQ 'DA')    DIRECT ACCESS DEVICE       00026300
&DEVDTA  SETB  (&DEVDTA OR '&C0' EQ 'TA')    MAGNETIC TAPE DEVICE       00026400
&DEVDPT  SETB  (&DEVDPT OR '&C0' EQ 'PT')    PAPER TAPE DEVICE          00026500
&DEVDPR  SETB  (&DEVDPR OR '&C0' EQ 'PR')    PRINTER                    00026600
&DEVDRD  SETB  (&DEVDRD OR '&C0' EQ 'RD' OR '&C0' EQ 'PC') READER,PUNCH 00026700
&DEVDBS  SETB  (&DEVDBS OR '&C0' EQ 'BS')    BINARY SYNCHRONOUS COMM.   00026800
&DEVDWT  SETB  (&DEVDWT OR '&C0' EQ 'WT')    WORLD TRADE TELEGRAPH      00026900
&DEVDMR  SETB  (&DEVDMR OR '&C0' EQ 'MR')    MAGNETIC CHAR READER       00027000
&DEVDOR  SETB  (&DEVDOR OR '&C0' EQ 'OR')    OPTICAL READER             00027100
.D4      ANOP                                                           00027200
&A0      SETA  &A0-1          DECREMENT ELEMENT COUNTER                 00027300
         AGO   .D2            TO DO NEW LOOP                            00027400
.*                                                                      00027500
.B1      ANOP                                                           00027600
         AIF   ('&DSORG(1)' EQ 'LR').BA SKIP OVER COMMENTS              00027700
         SPACE 2                                                        00027800
*                       DCB SYMBOLIC DEFINITION FOR                     00027900
         AIF   (NOT &DSORGIS).B2                                        00028000
*                       INDEXED SEQUENTIAL                              00028100
.B2      AIF   (NOT (&DSORGQS AND &DSORGBS)).B2A                        00028200
*                       PHYSICAL SEQUENTIAL                             00028300
         AGO   .B3                                                      00028400
.B2A     AIF   (NOT &DSORGQS).B2B                                       00028500
*                       QSAM                                            00028600
.B2B     AIF   (NOT &DSORGBS).B3                                        00028700
*                       BSAM-BPAM                                       00028800
.B3      AIF   (NOT &DSORGDA).B4                                        00028900
*                       DIRECT ACCESS                                   00029000
.B4      AIF   (NOT (&DSORGBX AND &DSORGQX)).B4A                        00029100
*                       COMMUNICATIONS LINE GROUP                       00029200
         AGO   .B5                                                      00029300
.B4A     AIF   (NOT &DSORGBX).B4B                                       00029400
*                       BTAM LINE GROUP                                 00029500
.B4B     AIF   (NOT &DSORGQX).B5                                        00029600
*                       QTAM LINE GROUP                                 00029700
.B5      AIF   (NOT &DSORGCQ).B6                                        00029800
*                       COMMUNICATIONS DIRECT ACCESS QUEUE              00029900
.B6      AIF   (NOT &DSORGMQ).B6A                                       00030000
*                       QTAM MESSAGE QUEUE                              00030100
.B6A     AIF   (NOT &DSORGTX).B6B                                       00030200
*                       TCAM LINE GROUP                                 00030300
.B6B     AIF   (NOT &DSORGTQ).B6C                                S22024 00030400
*                       TCAM MESSAGE QUEUE                              00030500
.B6C     AIF   (NOT &DSORGTR).B7                                 S22024 00030600
*                       3705 LINE GROUP                                 00030700
.B7      AIF   (NOT (&DSORGXA AND &DSORGXE)).B8                         00030800
*                       EXCP WITH EXTENSION AND APPENDAGES              00030900
         AGO   .BA                                                      00031000
.B8      AIF   (NOT &DSORGXE).B9                                        00031100
*                       EXCP WITH EXTENSION                             00031200
         AGO   .BA                                                      00031300
.B9      AIF   (NOT &DSORGXA).B0                                        00031400
*                       EXCP WITH APPENDAGES                            00031500
         AGO   .BA                                                      00031600
.B0      AIF   (NOT &DSORGGS).B00                                       00031700
*                       GRAPHICS WITH APPENDAGES                        00031800
         AGO   .BA                                                      00031900
.B00     AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *00032000
               &DSORGCQ OR &DSORGMQ OR &DSORGQS OR &DSORGBS OR         *00032100
               &DSORGTX OR &DSORGTQ OR &DSORGLR OR &DSORGTR).BA  S22024 00032200
*                       EXCP WITH EXTENSION                             00032300
&DSORGXE SETB  1                                                        00032400
.BA      ANOP                                                           00032500
         SPACE 1                                                        00032600
**********************************************************************  00032700
*   02/29/72          LEVEL=04                                          00032800
*                                                                       00032900
**********************************************************************  00033000
         SPACE 1                                                        00033100
         AIF   (&DSORGGS).SLIP                                          00033200
&LSW(001) SETB (1)                                                  DBC 00033300
&P.DCB   DS    0A
         SPACE 1                                                        00033500
         AIF   (NOT (&DEVDDA OR &DEVDTA OR &DEVDPT OR &DEVDPR OR       *00033600
               &DEVDRD OR &DEVDWT OR &DEVDOR OR &DEVDMR)).C4            00033700
&LSW(002) SETB (1)                                                  DBC 00033800
*                       DEVICE INTERFACES                               00033900
         SPACE 1                                                        00034000
         AIF   (NOT &DEVDDA).C1                                         00034100
&LSW(003) SETB (1)                                                  DBC 00034200
*                       DIRECT ACCESS DEVICES                           00034300
         SPACE 1                                                        00034400
&P.RELAD DS    CL4 -          PARTITIONED ORGANIZATION DATA SET -       00034500
*                             ADDRESS (IN THE FORM TTRN) OF MEMBER      00034600
*                             CURRENTLY USED.  ---                      00034700
*                             SYS1.LOGREC DATA SET - IF CCH OPTION HAS  00034800
*                             BEEN SPECIFIED IN SYSGEN PROCESS, ADDRESS 00034900
*                             OF A 12-BYTE PARAMETER IN THE EXPANSION   00035000
*                             OF MACRO INSTRUCTION IGFCATAP             00035100
&P.KEYCN DS    FL1 -          KEYED BLOCK OVERHEAD CONSTANT             00035200
&P.FDAD  DS    CL8 -          FULL DISK ADDRESS IN THE FORM OF MBBCCHHR 00035300
*                             OF RECORD THAT WAS JUST READ OR WRITTEN   00035400
         SPACE 1                                                        00035500
         ORG   &P.FDAD+7                                                00035600
&P.DVTBL DS    0A -           SAME AS DCBDVTBA BELOW                    00035700
         DS    X -            LAST BYTE OF DCBFDAD                      00035800
&P.DVTBA DS    AL3 -          ADDRESS OF ENTRY IN I/O DEVICE            00035900
*                             CHARACTERISTICS TABLE FOR DEVICE BEING    00036000
*                             USED                                      00036100
         DS    FL1 -          DCBKEYLE - KEY LENGTH OF DATA SET         00036200
         DS    C -            DCBDEVT - DEVICE TYPE                     00036300
*   FOR MASKS FOR ISAM DIRECT ACCESS, SEE DCBOVDEV IN ISAM SECTION      00036400
         AIF   (&#DCBSW(003)).SKP003A                               DBC 00036500
DCBDV311 EQU   X'21' -        2311 DISK DRIVE                           00036600
DCBDV301 EQU   X'22' -        2301 PARALLEL DRUM                        00036700
DCBDV303 EQU   X'23' -        2303 SERIAL DRUM                          00036800
DCBDV302 EQU   X'24' -        2302 DISK STORAGE                         00036900
DCBDV321 EQU   X'25' -        2321 DATA CELL DRIVE                      00037000
DCBDV314 EQU   X'28' -        2314 DISK STORAGE FACILITY                00037100
.SKP003A ANOP  ,                                                    DBC 00037200
&P.TRBAL DS    H -            TRACK BALANCE.  NUMBER OF BYTES REMAINING 00037300
*                             ON CURRENT TRACK AFTER A WRITE OPERATION  00037400
*                             (THIS QUANTITY MAY BE NEGATIVE IF THERE   00037500
*                             ARE NO BYTES REMAINING ON TRACK).         00037600
         SPACE 1                                                        00037700
.C1      AIF   (NOT &DEVDTA).C2                                         00037800
&LSW(004) SETB (1)                                                  DBC 00037900
*                       MAGNETIC TAPE                                   00038000
         SPACE 1                                                        00038100
         ORG   &P.DCB                                                   00038200
         DS    CL12 -         RESERVED FOR I/O SUPERVISOR               00038300
&P.BLKCT DS    F -            BLOCK COUNT FOR EACH VOLUME               00038400
&P.TRTCH DS    C -            TAPE RECORDING TECHNIQUE FOR 7-TRACK TAPE 00038500
         AIF   (&#DCBSW(004)).SKP004A                               DBC 00038600
DCBMTE   EQU   X'23' -        E  - EVEN PARITY                          00038700
DCBMTT   EQU   X'3B' -        T  - BCD/EBCDIC TRANSLATION               00038800
DCBMTC   EQU   X'13' -        C  - DATA CONVERSION                      00038900
DCBMTET  EQU   X'2B' -        ET - EVEN PARITY AND TRANSLATION          00039000
.SKP004A ANOP  ,                                                    DBC 00039100
         DS    C -            DCBDEVT - DEVICE TYPE                     00039200
         AIF   (&#DCBSW(004)).SKP004B                               DBC 00039300
DCBDVMT  EQU   X'81' -        2400 SERIES MAGNETIC TAPE UNIT (7-TRACK   00039400
*                             OR 9-TRACK)                               00039500
DCBDVMT3 EQU   X'83' -        3400 SERIES MAGNETIC TAPE UNIT     ICB277 00039600
.SKP004B ANOP  ,                                                    DBC 00039700
&P.DEN   DS    C -            TAPE DENSITY - 2400 SERIES MAGNETIC TAPE  00039800
*                             UNITS                                     00039900
         AIF   (&#DCBSW(004)).SKP004C                               DBC 00040000
*                             CODE    7-TRACK     9-TRACK               00040100
DCBMTDN0 EQU   X'03' -         0       200 BPI       -                  00040200
DCBMTDN1 EQU   X'43' -         1       556 BPI       -                  00040300
DCBMTDN2 EQU   X'83' -         2       800 BPI     800 BPI              00040400
DCBMTDN3 EQU   X'C3' -         3         -        1600 BPI              00040500
.SKP004C ANOP  ,                                                    DBC 00040600
         DS    X -            RESERVED                                  00040700
         SPACE 1                                                        00040800
.C2      AIF   (NOT &DEVDPT).C3                                         00040900
&LSW(005) SETB (1)                                                  DBC 00041000
*                       PAPER TAPE                                      00041100
         SPACE 1                                                        00041200
         ORG   &P.DCB+8                                                 00041300
&P.LCTBL DS    A -            ADDRESS OF TRANSLATE TABLE                00041400
         DS    XL4 -          RESERVED                                  00041500
&P.CODE  DS    C -            PAPER TAPE CODE BEING USED.  THE          00041600
*                             APPROPRIATE TRANSLATE TABLE IS MADE       00041700
*                             AVAILABLE                                 00041800
         AIF   (&#DCBSW(005)).SKP005A                               DBC 00041900
DCBPTCDN EQU   X'80' -        N - NO CONVERSION                         00042000
DCBPTCDI EQU   X'40' -        I - IBM BCD                               00042100
DCBPTCDF EQU   X'20' -        F - FRIDEN                                00042200
DCBPTCDB EQU   X'10' -        B - BURROUGHS                             00042300
DCBPTCDC EQU   X'08' -        C - NATIONAL CASH REGISTER                00042400
DCBPTCDA EQU   X'04' -        A - ASCII (8-TRACK)                       00042500
DCBPTCDT EQU   X'02' -        T - TELETYPE                              00042600
.SKP005A ANOP  ,                                                    DBC 00042700
         DS    C -            DCBDEVT - DEVICE TYPE                     00042800
         AIF   (&#DCBSW(005)).SKP005B                               DBC 00042900
DCBDVPTP EQU   X'50' -        2671 PAPER TAPE READER                    00043000
.SKP005B ANOP  ,                                                    DBC 00043100
         DS    X -            RESERVED                                  00043200
&P.PTFLG DS    BL1 -          PAPER TAPE FLAGS                          00043300
         AIF   (&#DCBSW(005)).SKP005C                               DBC 00043400
DCBPTIC  EQU   BIT3 -         INVALID CHARACTER IN LAST RECORD READ     00043500
DCBPTECT EQU   BIT4 -         END OF RECORD CHARACTER REACHED IN        00043600
*                             TRANSLATION                               00043700
DCBPTECR EQU   BIT5 -         END OF RECORD CHARACTER DETECTED DURING   00043800
*                             READ                                      00043900
DCBPTUCT EQU   BIT6 -         IF ONE, UPPER CASE TRANSLATE.             00044000
*                             IF ZERO, LOWER CASE TRANSLATE             00044100
DCBPTERR EQU   BIT7 -         ERROR DETECTED ON READ                    00044200
.SKP005C ANOP  ,                                                    DBC 00044300
         SPACE 1                                                        00044400
.C3      AIF   (NOT &DEVDPR).C3A                                        00044500
&LSW(006) SETB (1)                                                  DBC 00044600
*                       PRINTER                                         00044700
         SPACE 1                                                        00044800
         ORG   &P.DCB+16                                                00044900
&P.PRTSP DS    C -            NUMBER INDICATING NORMAL PRINTER SPACING  00045000
         AIF   (&#DCBSW(006)).SKP006A                               DBC 00045100
DCBPRSP0 EQU   X'01' -        0 - NO SPACING                            00045200
DCBPRSP1 EQU   X'09' -        1 - SPACE ONE LINE                        00045300
DCBPRSP2 EQU   X'11' -        2 - SPACE TWO LINES                       00045400
DCBPRSP3 EQU   X'19' -        3 - SPACE THREE LINES                     00045500
.SKP006A ANOP  ,                                                    DBC 00045600
         DS    C -            DCBDEVT - DEVICE TYPE                     00045700
         AIF   (&#DCBSW(006)).SKP006B                               DBC 00045800
DCBDVPR1 EQU   X'48' -        1403 PRINTER AND 1404 PRINTER (CONTINUOUS 00045900
*                             FORM SUPPORT ONLY)                        00046000
DCBDVPR2 EQU   X'4A' -        1443 PRINTER                              00046100
DCBDVPR3 EQU   X'49' -        3211 PRINTER                              00046200
.SKP006B ANOP  ,                                                    DBC 00046300
&P.PRTOV DS    C -            TEST-FOR-PRINTER-OVERFLOW MASK            00046400
*                             (PRTOV MASK)                              00046500
         AIF   (&#DCBSW(006)).SKP006C                               DBC 00046600
DCBPRC9  EQU   X'20' -        9  - TEST FOR CHANNEL 9 OVERFLOW          00046700
DCBPRC12 EQU   X'10' -        12 - TEST FOR CHANNEL 12 OVERFLOW         00046800
.SKP006C ANOP  ,                                                    DBC 00046900
         DS    X -            RESERVED                                  00047000
         SPACE 1                                                        00047100
.C3A     AIF   (NOT &DEVDRD).C3B                                        00047200
&LSW(007) SETB (1)                                                  DBC 00047300
*                       CARD READER, CARD PUNCH                         00047400
         SPACE 1                                                        00047500
         ORG   &P.DCB+16                                                00047600
&P.MODE  DS    0B -           MODE OF OPERATION FOR 1442 CARD READ      00047700
*                             PUNCH (BITS 0-3)                          00047800
&P.STACK DS    B -            STACKER SELECTION (BITS 4-7)              00047900
         AIF   (&#DCBSW(007)).SKP007A                               DBC 00048000
DCBMODEC EQU   BIT0 -         COLUMN BINARY MODE                        00048100
DCBMODEE EQU   BIT1 -         EBCDIC MODE                               00048200
DCBMODEO EQU   BIT2 -         OPTICAL MARK READ MODE                    00048300
DCBMODER EQU   BIT3 -         READ COLUMN ELIMINATE MODE                00048400
DCBSTCK2 EQU   BIT6 -         STACKER 2                                 00048500
DCBSTCK1 EQU   BIT7 -         STACKER 1                                 00048600
.SKP007A ANOP  ,                                                    DBC 00048700
         DS    C -            DCBDEVT - DEVICE TYPE                     00048800
         AIF   (&#DCBSW(007)).SKP007B                               DBC 00048900
DCBDVCR0 EQU   X'41' -        2540 CARD READER                          00049000
DCBDVCP0 EQU   X'42' -        2540 CARD PUNCH                           00049100
DCBDVCRP EQU   X'43' -        1442 CARD READ PUNCH                      00049200
DCBDVCR1 EQU   X'44' -        2501 CARD READER                          00049300
DCBDVCPR EQU   X'45' -        2520 CARD READ PUNCH                      00049400
DCBDVCR2 EQU   X'46'          3505 CARD READER                   XM0629 00049500
DCBDVCP1 EQU   X'4C'          3525 CARD PUNCH                    XM0629 00049600
.SKP007B ANOP  ,                                                    DBC 00049700
         DS    X -            RESERVED                                  00049800
&P.FUNC  DS    B -            FUNCTION INDICATOR FOR THE 3525           00049900
         AIF   (&#DCBSW(007)).SKP007C                               DBC 00050000
DCBFNCBI EQU   BIT0 -         INTERPRET (PUNCH AND PRINT TWO LINES)     00050100
DCBFNCBR EQU   BIT1 -         READ                                      00050200
DCBFNCBP EQU   BIT2 -         PUNCH                                     00050300
DCBFNCBW EQU   BIT3 -         PRINT                                     00050400
DCBFNCBD EQU   BIT4 -         DATA PROTECTION                           00050500
DCBFNCBX EQU   BIT5 -         THIS DATA SET IS TO BE PRINTED            00050600
DCBFNCBT EQU   BIT6 -         TWO-LINE PRINT SUPPORT REQUEST            00050700
.SKP007C ANOP  ,                                                    DBC 00050800
         SPACE 1                                                        00050900
.C3B     AIF   (NOT &DEVDWT).C3C                                        00051000
&LSW(008) SETB (1)                                                  DBC 00051100
*                       WORLD TRADE TELEGRAPH                           00051200
         SPACE 1                                                        00051300
         ORG   &P.DCB+16                                                00051400
&P.BQFLG DS    BL1 -          WTTA FLAG BYTE                            00051500
         AIF   (&#DCBSW(008)).SKP008A                               DBC 00051600
DCBBQWRU EQU   BIT1 -         WRU FEATURE IS TO BE USED                 00051700
DCBBQIAM EQU   BIT2 -         IAM FEATURE IS TO BE USED                 00051800
DCBBQWRS EQU   BIT3 -         WRU FEATURE TO BE USED IN SEND HEADER     00051900
*                             SUBGROUP                                  00052000
DCBBQWRE EQU   BIT4 -         WRU FEATURE TO BE USED IN END SEND        00052100
*                             SUBGROUP                                  00052200
.SKP008A ANOP  ,                                                    DBC 00052300
&P.WTEOM DS    C -            EOM CHARACTER                             00052400
&P.WTEOT DS    C -            EOT CHARACTER                             00052500
&P.WTPAD DS    FL1 -          NUMBER OF PAD (LTRS) CHARACTERS REQUIRED  00052600
*                             FOR MOTOR-ON DELAY                        00052700
         SPACE 1                                                        00052800
.C3C     AIF   (NOT (&DEVDOR OR &DEVDMR)).C4                            00052900
&LSW(009) SETB (1)                                                  DBC 00053000
*                       OPTICAL READER AND MAGNETIC CHAR READER         00053100
         SPACE 1                                                        00053200
         ORG   &P.DCB                                                   00053300
&P.WTOID DS    0A -           SAME AS DCBWTOIA BELOW                    00053400
         DS    X -            RESERVED                                  00053500
&P.WTOIA DS    AL3 -          A BINARY IDENTIFICATION NUMBER ASSIGNED   00053600
*                             BY COMMUNICATIONS TASK TO MESSAGE ISSUED  00053700
*                             BY WTO MACRO.  THIS NUMBER IS USED BY THE 00053800
*                             DOM MACRO WHEN MESSAGE IS NO LONGER       00053900
*                             REQUIRED (MCS SUPPORT).  ---              00054000
*                             FOR MAGNETIC CHAR READER - AFTER FIRST    00054100
*                             READ HAS BEEN ISSUED, CONTAINS ADDRESS OF 00054200
*                             MAGNETIC INTERRUPT CONTROL BLOCK (MICB)   00054300
*                             BEING USED BY THE APPENDAGES.             00054400
         SPACE 1                                                        00054500
         AIF   (NOT &DEVDOR).C3D                                        00054600
&LSW(010) SETB (1)                                                  DBC 00054700
*                       OPTICAL READER DEVICES                          00054800
*                       1285, 1287, 1288                                00054900
         SPACE 1                                                        00055000
         ORG   &P.WTOID+4                                               00055100
&P.ERRCN DS    0A -           SAME AS DCBERRCA BELOW                    00055200
         DS    X -            RESERVED                                  00055300
&P.ERRCA DS    AL3 -          ADDRESS OF 32 BYTES OF DECLARED STORAGE   00055400
*                             SPECIFIED BY THE USER IN HIS PROGRAM.     00055500
*                             THIS STORAGE WILL BE USED BY THE          00055600
*                             PROGRAMMING SUPPORT AS EIGHT 4-BYTE       00055700
*                             COUNTERS IN WHICH TOTALS OF CERTAIN 1285, 00055800
*                             1287 AND 1288 ERROR CONDITIONS ARE        00055900
*                             ACCUMULATED.                              00056000
&P.DSPLY DS    0A -           SAME AS DCBDSPLA BELOW                    00056100
         DS    X -            RESERVED                                  00056200
&P.DSPLA DS    AL3 -          ADDRESS OF DSPLY (BSAM) ROUTINE USED FOR  00056300
*                             KEYBOARD ENTRY OF A COMPLETE FIELD        00056400
&P.RESCN DS    0A -           SAME AS DCBRESCA BELOW                    00056500
&P.RDLNE DS    0A -           SAME AS DCBRDLNA BELOW                    00056600
         DS    X -            RESERVED                                  00056700
&P.RESCA DS    0AL3 -         ADDRESS OF RESCN (BSAM) ROUTINE USED TO   00056800
*                             FORCE ON-LINE CORRECTION OF UNREADABLE    00056900
*                             CHARACTERS                                00057000
&P.RDLNA DS    AL3 -          ADDRESS OF RDLNE (QSAM) ROUTINE USED TO   00057100
*                             FORCE ON-LINE CORRECTION OF UNREADABLE    00057200
*                             CHARACTERS                                00057300
&P.ORBYT DS    BL1 -          OPTICAL READER BYTE USED BY BSAM/QSAM     00057400
         AIF   (&#DCBSW(010)).SKP010A                               DBC 00057500
DCBORSYN EQU   BIT0 -         SYNAD IN CONTROL                          00057600
DCBOREOF EQU   BIT1 -         END OF FILE (EOF)                         00057700
DCBORBFP EQU   BIT2 -         BUFFERS PRIMED (QSAM)                     00057800
.SKP010A ANOP  ,                                                    DBC 00057900
         DS    C -            DCBDEVT - DEVICE TYPE                     00058000
         AIF   (&#DCBSW(010)).SKP010B                               DBC 00058100
DCBDVOR5 EQU   X'5A' -        1285 OPTICAL READER                       00058200
DCBDVOR7 EQU   X'5B' -        1287 OPTICAL READER                       00058300
DCBDVOR8 EQU   X'5C' -        1288 OPTICAL READER                       00058400
.SKP010B ANOP  ,                                                    DBC 00058500
&P.EIB   DS    BL1 -          ERROR INDICATOR BYTE                      00058600
         AIF   (&#DCBSW(010)).SKP010C                               DBC 00058700
DCBORNRM EQU   BIT1 -         THE 1287 OR 1288 SCANNER WAS UNABLE TO    00058800
*                             LOCATE THE REFERENCE MARK                 00058900
DCBORREJ EQU   BIT2 -         FOR 1287, A STACKER SELECT COMMAND WAS    00059000
*                             GIVEN AFTER ALLOTTED TIME HAD ELAPSED AND 00059100
*                             THE DOCUMENT HAS BEEN PUT IN REJECT       00059200
*                             POCKET.  FOR 1288 UNFORMATTED ONLY,       00059300
*                             END-OF-PAGE HAS OCCURRED.                 00059400
DCBORERR EQU   BIT3 -         A NONRECOVERABLE ERROR HAS OCCURRED.      00059500
DCBORECK EQU   BIT4 -         AN EQUIPMENT CHECK RESULTED IN AN         00059600
*                             INCOMPLETE READ                           00059700
DCBORWLR EQU   BIT5 -         A WRONG-LENGTH RECORD CONDITION HAS       00059800
*                             OCCURRED                                  00059900
DCBORHPR EQU   BIT6 -         FOR QSAM - OPERATOR ENTERED ONE OR MORE   00060000
*                             CHARACTERS FROM THE KEYBOARD.             00060100
*                             FOR BSAM - A HOPPER EMPTY CONDITION HAS   00060200
*                             OCCURRED                                  00060300
DCBORDCK EQU   BIT7 -         A DATA CHECK HAS OCCURRED                 00060400
.SKP010C ANOP  ,                                                    DBC 00060500
         DS    X -            RESERVED                                  00060600
         SPACE 1                                                        00060700
.C3D     AIF   (NOT &DEVDMR).C4                                         00060800
&LSW(011) SETB (1)                                                  DBC 00060900
*                       MAGNETIC CHARACTER READER DEVICES               00061000
*                       1419 MAGNETIC CHARACTER READER                  00061100
*                       1275 OPTICAL READER SORTER                      00061200
         SPACE 1                                                        00061300
         ORG   &P.DCB                                                   00061400
&P.SSID  DS    CL8 -          BEFORE DCB IS OPENED - NAME OF USER'S     00061500
*                             STACKER SELECT ROUTINE.                   00061600
         SPACE 1                                                        00061700
         ORG   &P.SSID                                                  00061800
         DS    A -            AFTER DCB IS OPENED - DCBWTOID            00061900
&P.SSAD  DS    0A -           ADDRESS OF USER'S STACKER SELECT ROUTINE  00062000
         DS    X -            RESERVED                                  00062100
&P.SSADA DS    AL3 -          ADDRESS OF USER'S STACKER SELECT ROUTINE  00062200
&P.IMAGE DS    0A -           SAME AS DCBIMAGA BELOW                    00062300
&P.MRFG  DS    BL1 -          BUFFER INDICATOR                          00062400
         AIF   (&#DCBSW(011)).SKP011A                               DBC 00062500
DCBMRBCT EQU   BIT0+BIT1 -    TWO-BIT BINARY COUNTER WHICH INDICATES    00062600
*                             INTO WHICH BUFFER STATUS INFORMATION IS   00062700
*                             TO BE POSTED                              00062800
.SKP011A ANOP  ,                                                    DBC 00062900
&P.IMAGA DS    AL3 -          ADDRESS OF PARAMETER LIST USED TO         00063000
*                             COMMUNICATE BETWEEN USER'S PROCESSING     00063100
*                             ROUTINES AND HIS STACKER SELECT ROUTINES  00063200
&P.ECBLT DS    0A -           SAME AS DCBECBLA BELOW                    00063300
&P.MRIND DS    BL1 -          INDICATOR AND COUNTER BYTE                00063400
         AIF   (&#DCBSW(011)).SKP011B                               DBC 00063500
DCBMRDCT EQU   BIT0+BIT1+BIT2 THREE-BIT BINARY COUNTER OF NUMBER OF     00063600
*                             DOCUMENTS READ AFTER DISENGAGE            00063700
DCBMRSCU EQU   BIT3 -         DCB WAS ALTERED WHEN SYNAD ROUTINE WAS    00063800
*                             ENTERED DUE TO SECONDARY CONTROL UNIT     00063900
*                             (SCU) ERROR                               00064000
DCBMRPLO EQU   BIT4 -         POCKET LIGHT HAS BEEN TURNED ON           00064100
DCBMRPLS EQU   BIT5 -         POCKET LIGHT 0-6 IS BEING SET ON          00064200
DCBMRERP EQU   BIT6 -         ERROR RECOVERY PROCEDURE IS EXECUTING FOR 00064300
*                             PRIMARY CONTROL UNIT (PCU)                00064400
DCBMRERS EQU   BIT7 -         ERROR RECOVERY PROCEDURE IS EXECUTING FOR 00064500
*                             SECONDARY CONTROL UNIT (SCU)              00064600
.SKP011B ANOP  ,                                                    DBC 00064700
&P.ECBLA DS    AL3 -          ADDRESS OF ECB LIST PASSED TO WAIT MACRO  00064800
*                             BY CHECK MACRO WHEN NO 1419/1275 IS       00064900
*                             AVAILABLE FOR PROCESSING                  00065000
&P.MRFLG DS    BL1 -          FLAG BYTE                                 00065100
         AIF   (&#DCBSW(011)).SKP011C                               DBC 00065200
DCBMRSCC EQU   BIT0 -         FIRST OR SECOND SECONDARY CONTROL UNIT    00065300
*                             COMMAND CHAIN IS BEING USED               00065400
DCBMRDBG EQU   BIT1 -         DEBUGGING MODE IN USE                     00065500
DCBMRDRU EQU   BIT2 -         DISENGAGE REQUESTED BY USER               00065600
DCBMRDR  EQU   BIT3 -         DISENGAGE REQUESTED                       00065700
DCBMRPCC EQU   BIT4+BIT5 -    TWO-BIT BINARY COUNTER INDICATING FIRST,  00065800
*                             SECOND OR THIRD PRIMARY CONTROL UNIT      00065900
*                             COMMAND CHAIN IS BEING USED               00066000
DCBMRDWT EQU   BIT6 -         WTO MESSAGE MUST BE DELETED               00066100
DCBMRUE  EQU   BIT7 -         UNIT EXCEPTION                            00066200
.SKP011C ANOP  ,                                                    DBC 00066300
         DS    C -            DCBDEVT - DEVICE TYPE                     00066400
         AIF   (&#DCBSW(011)).SKP011D                               DBC 00066500
DCBDVMR  EQU   X'5D' -        1419 MAGNETIC CHARACTER READER            00066600
DCBDVORS EQU   X'5F' -        1275 OPTICAL READER SORTER                00066700
.SKP011D ANOP  ,                                                    DBC 00066800
&P.APPIN DS    C -            AN INDICATOR USED BY THE APPENDAGES TO    00066900
*                             PASS INFORMATION ABOUT ONE CHANNEL CHAIN  00067000
*                             TO AN APPENDAGE ASSOCIATED WITH ANOTHER   00067100
*                             CHANNEL CHAIN                             00067200
         DS    X -            RESERVED                                  00067300
         SPACE 1                                                        00067400
.C4      AIF   (NOT &DSORGTR).C4A                                S22024 00067500
&LSW(012) SETB (1)                                                  DBC 00067600
*                       3705 LINE TERMINAL                       S22024 00067700
         ORG   &P.DCB+8                                          S22024 00067800
&P.IPLTX DS    CL8            NAME OF MODULE TO BE USED TO IPL   S22024 00067900
*                             THE 3705                           S22024 00068000
&P.BCKUP DS    0A             FULL WORD LABEL                    S22024 00068100
         DS    BL1            RESERVED                           S22024 00068200
&P.BCKUA DS    AL3            ADDRESS OF THE DCB FOR THE         S22024 00068300
*                             BACKUP 3705.                       S22024 00068400
.C4A     AIF   (NOT (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR   *00068500
               &DSORGXE)).C5                                            00068600
&LSW(013) SETB (1)                                                  DBC 00068700
*                       ACCESS METHOD COMMON INTERFACE                  00068800
         SPACE 1                                                        00068900
         ORG   &P.DCB+16                                                00069000
&P.RELB  DS    0F -           SAME AS DCBREL BELOW                      00069100
&P.KEYLE DS    FL1 -          KEY LENGTH OF DATA SET                    00069200
&P.DEVT  DS    0C -           DEVICE TYPE                               00069300
         AIF   (&#DCBSW(13)).SKP13A                                 DBC 00069400
DCBDVTRM EQU   X'4F' -        TERMINAL.  (DD CONTAINS TERM=TS)          00069500
.SKP13A  ANOP  ,                                                    DBC 00069600
&P.REL   DS    FL3 -          NUMBER OF RELATIVE TRACKS OR BLOCKS IN    00069700
*                             THIS DATA SET (BDAM)                      00069800
&P.BUFCB DS    0A -           ADDRESS OF BUFFER POOL CONTROL BLOCK      00069900
&P.BUFNO DS    FL1 -          NUMBER OF BUFFERS REQUIRED FOR THIS DATA  00070000
*                             SET.  MAY RANGE FROM 0 TO 255.  IF        00070100
*                             UNBLOCKED SPANNED RECORDS ARE USED,       00070200
*                             NUMBER OF SEGMENT WORK AREAS REQUIRED     00070300
*                             FOR THIS DATA SET.                        00070400
&P.BUFCA DS    AL3 -          ADDRESS OF BUFFER POOL CONTROL BLOCK      00070500
&P.BUFL  DS    H -            LENGTH OF BUFFER.  MAY RANGE FROM 0 TO    00070600
*                             32,767.                                   00070700
&P       #DSORG                                                     DBC 00070800
&P.IOBAD DS    0A -           ADDRESS OF IOB WHEN CHAINED SCHEDULING IS 00070900
*                             USED OR FOR 1419/1275                     00071000
&P.ODEB  DS    0A -           ADDRESS OF OLD DEB                        00071100
&P.LNP   DS    0FL1 -         3525 PRINTER LINE POSITION COUNTER        00071200
&P.QSLM  DS    BL1 -          QSAM LOCATE MODE LOGICAL RECORD INTERFACE 00071300
*                             INDICATOR BYTE FOR UPDAT PROCESSING OF    00071400
*                             SPANNED RECORDS                           00071500
         AIF   (&#DCBSW(013)).SKP013A                               DBC 00071600
DCB1DVDS EQU   BIT0 -         ONLY ONE DEVICE IS ALLOCATED TO THIS      00071700
*                             DATA SET                                  00071800
DCBUPDCM EQU   BIT1 -         UPDATE COMPLETE, FREE OLD DEB             00071900
DCBUPDBT EQU   BIT2+BIT3 -    UPDATE BITS                               00072000
DCBUPDT  EQU   BIT2 -         UPDATE TO TAKE PLACE                      00072100
DCBNUPD  EQU   BIT2+BIT3 -    NO UPDATE TO TAKE PLACE                   00072200
DCBSVDEB EQU   BIT3 -         OLD DEB ADDRESS MUST BE SAVED             00072300
.SKP013A ANOP  ,                                                    DBC 00072400
&P.IOBAA DS    0AL3 -         SAME AS DCBIOBAD ABOVE                    00072500
&P.ODEBA DS    AL3 -          ADDRESS OF OLD DEB                        00072600
         ORG   &P.DCB+28                                     ICBI DCB-4 00072700
&P.SVCXL DS    0A -           SAME AS DCBSVCXA BELOW         ICBI DCB-4 00072800
         DS    X -            RESERVED                       ICBI DCB-4 00072900
&P.SVCXA DS    AL3 -          POINTER TO EXIT LIST OF JES    ICBI DCB-4 00073000
*                             C.I. INTERFACE CONTROL SVC     ICBI DCB-4 00073100
         SPACE 1                                                        00073200
*                       FOUNDATION EXTENSION                            00073300
         SPACE 1                                                        00073400
&P.EODAD DS    0A -           SAME AS DCBEODA BELOW                     00073500
&P.HIARC DS    0BL1 -         HIERARCHY BITS                            00073600
&P.BFTEK DS    0BL1 -         BUFFERING TECHNIQUE BITS                  00073700
&P.BFALN DS    BL1 -          BUFFER ALIGNMENT BITS                     00073800
         AIF   (&#DCBSW(13) OR &#DCBSW(16)).SKP013B                 DBC 00073900
DCBH1    EQU   BIT0 -         HIERARCHY 1 MAIN STORAGE - BIT 5 IS ZERO  00074000
DCBBFT   EQU   BIT1+BIT2+BIT3 BUFFERING TECHNIQUE                       00074100
DCBBFTA  EQU   BIT1+BIT2 -    QSAM LOCATE MODE PROCESSING OF SPANNED    00074200
*                             RECORDS - OPEN IS TO CONSTRUCT A RECORD   00074300
*                             AREA IF IT AUTOMATICALLY CONSTRUCTS       00074400
*                             BUFFERS                                   00074500
DCBBFTR  EQU   BIT2 -         FOR BSAM CREATE BDAM PROCESSING OF        00074600
*                             UNBLOCKED SPANNED RECORDS - SOFTWARE      00074700
*                             TRACK OVERFLOW.  FOR BSAM INPUT           00074800
*                             PROCESSING OF UNBLOCKED SPANNED RECORDS   00074900
*                             WITH KEYS - RECORD OFFSET PROCESSING.     00075000
DCBBFTS  EQU   BIT1 -         SIMPLE BUFFERING - BIT 3 IS ZERO          00075100
DCBBFTKR EQU   BIT2 -         UNBLOCKED SPANNED RECORDS - SOFTWARE      00075200
*                             TRACK OVERFLOW (BDAM)                     00075300
DCBBFTE  EQU   BIT3 -         EXCHANGE BUFFERING - BIT 1 IS ZERO        00075400
DCBBFTKD EQU   BIT4 -         DYNAMIC BUFFERING (BTAM)                  00075500
DCBH0    EQU   BIT5 -         HIERARCHY 0 MAIN STORAGE - BIT 0 IS ZERO  00075600
DCBBFA   EQU   BIT6+BIT7 -    BUFFER ALIGNMENT                          00075700
DCBBFAD  EQU   BIT6 -         DOUBLEWORD BOUNDARY                       00075800
DCBBFAF1 EQU   BIT7 -         FULLWORD NOT A DOUBLEWORD BOUNDARY,       00075900
*                             CODED IN DCB MACRO INSTRUCTION            00076000
DCBBFAF2 EQU   BIT6+BIT7 -    FULLWORD NOT A DOUBLEWORD BOUNDARY,       00076100
*                             CODED IN DCB MACRO INSTRUCTION            00076200
.SKP013B ANOP  ,                                                    DBC 00076300
&P.EODA  DS    AL3 -          ADDRESS OF A USER-PROVIDED ROUTINE TO     00076400
*                             HANDLE END-OF-DATA CONDITIONS             00076500
&P.EXLST DS    0A -           ADDRESS OF USER-PROVIDED LIST OF EXITS    00076600
&P.RECFM DS    BL1 -          RECORD FORMAT                             00076700
         AIF   (&#DCBSW(013)).SKP013C                               DBC 00076800
DCBRECLA EQU   BIT0+BIT1+BIT2 RECORD LENGTH INDICATOR - ASCII           00076900
DCBRECD  EQU   BIT2 -         ASCII VARIABLE RECORD LENGTH              00077000
DCBRECL  EQU   BIT0+BIT1 -    RECORD LENGTH INDICATOR                   00077100
DCBRECF  EQU   BIT0 -         FIXED RECORD LENGTH                       00077200
DCBRECV  EQU   BIT1 -         VARIABLE RECORD LENGTH                    00077300
DCBRECU  EQU   BIT0+BIT1 -    UNDEFINED RECORD LENGTH                   00077400
DCBRECTO EQU   BIT2 -         TRACK OVERFLOW                            00077500
DCBRECBR EQU   BIT3 -         BLOCKED RECORDS                           00077600
DCBRECSB EQU   BIT4 -         FOR FIXED LENGTH RECORD FORMAT - STANDARD 00077700
*                             BLOCKS.  FOR VARIABLE LENGTH RECORD       00077800
*                             FORMAT - SPANNED RECORDS                  00077900
DCBRECCC EQU   BIT5+BIT6 -    CONTROL CHARACTER INDICATOR               00078000
DCBRECCA EQU   BIT5           ASA CONTROL CHARACTER                     00078100
DCBRECCM EQU   BIT6 -         MACHINE CONTROL CHARACTER                 00078200
DCBRECC  EQU   X'00' -        NO CONTROL CHARACTER                      00078300
DCBRECKL EQU   BIT7 -         KEY LENGTH (KEYLEN) WAS SPECIFIED IN DCB  00078400
*                             MACRO INSTRUCTION                         00078500
.SKP013C ANOP  ,                                                    DBC 00078600
&P.EXLSA DS    AL3 -          ADDRESS OF USER-PROVIDED LIST OF EXITS    00078700
         SPACE 1                                                        00078800
.C5      AIF   (NOT &DSORGBX).C5B                                       00078900
&LSW(014) SETB (1)                                                  DBC 00079000
         AIF   (&DSORGQX AND (&DSORGIS OR &DSORGDA OR &DSORGQS OR      *00079100
               &DSORGBS OR &DSORGXE)).C5A                               00079200
&LSW(015) SETB (1)                                                  DBC 00079300
*                       BTAM LINE GROUP INTERFACE                       00079400
         SPACE 1                                                        00079500
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR        *00079600
               &DSORGXE).C5A                                            00079700
&LSW(016) SETB (1)                                                  DBC 00079800
         ORG   &P.DCB+20                                                00079900
&P.BUFCB DS    0A -           ADDRESS OF BUFFER POOL CONTROL BLOCK      00080000
&P.BUFNO DS    FL1 -          NUMBER OF BUFFERS OBTAINED BY OPEN        00080100
&P.BUFCA DS    AL3 -          ADDRESS OF BUFFER POOL CONTROL BLOCK      00080200
&P.BUFL  DS    H -            BUFFER LENGTH                             00080300
&P       #DSORG                                                     DBC 00080400
&P.IOBAD DS    0A -           BASE FOR ADDRESSING IOB'S (BASE = ADDRESS 00080500
*                             OF FIRST IOB MINUS LENGTH OF AN IOB)      00080600
         DS    FL1 -          DCBDEVTP - INDEX TO DEVICE ENTRY IN THE   00080700
*                             DEVICE I/O DIRECTORY                      00080800
&P.IOBAA DS    AL3 -          SAME AS DCBIOBAD ABOVE                    00080900
&P.HIARC DS    0BL1 -         HIERARCHY FLAG BITS                       00081000
&P.BFTEK DS    BL1 -          BUFFERING TECHNIQUE FLAG BITS             00081100
         AIF   (&#DCBSW(13) OR &#DCBSW(16)).SKP016A                 DBC 00081200
DCBH1    EQU   BIT0 -         HIERARCHY 1 MAIN STORAGE - BIT 5 IS ZERO  00081300
DCBBFT   EQU   BIT1+BIT2+BIT3 BUFFERING TECHNIQUE                       00081400
DCBBFTA  EQU   BIT1+BIT2 -    QSAM LOCATE MODE PROCESSING OF SPANNED    00081500
*                             RECORDS - OPEN IS TO CONSTRUCT A RECORD   00081600
*                             AREA IF IT AUTOMATICALLY CONSTRUCTS       00081700
*                             BUFFERS                                   00081800
DCBBFTR  EQU   BIT2 -         FOR BSAM CREATE BDAM PROCESSING OF        00081900
*                             UNBLOCKED SPANNED RECORDS - SOFTWARE      00082000
*                             TRACK OVERFLOW.  FOR BSAM INPUT           00082100
*                             PROCESSING OF UNBLOCKED SPANNED RECORDS   00082200
DCBBFTS  EQU   BIT1 -         SIMPLE BUFFERING - BIT 3 IS ZERO          00082300
DCBBFTKR EQU   BIT2 -         UNBLOCKED SPANNED RECORDS - SOFTWARE      00082400
*                             TRACK OVERFLOW (BDAM)                     00082500
DCBBFTE  EQU   BIT3 -         EXCHANGE BUFFERING - BIT 1 IS ZERO        00082600
DCBBFTKD EQU   BIT4 -         DYNAMIC BUFFERING (BTAM)                  00082700
DCBH0    EQU   BIT5 -         HIERARCHY 0 MAIN STORAGE - BIT 0 IS ZERO  00082800
DCBBFA   EQU   BIT6+BIT7 -    BUFFER ALIGNMENT                          00082900
DCBBFAD  EQU   BIT6 -         DOUBLEWORD BOUNDARY                       00083000
DCBBFAF1 EQU   BIT7 -         FULLWORD NOT A DOUBLEWORD BOUNDARY,       00083100
*                             CODED IN DCB MACRO INSTRUCTION            00083200
DCBBFAF2 EQU   BIT6+BIT7 -    FULLWORD NOT A DOUBLEWORD BOUNDARY,       00083300
*                             CODED IN DCB MACRO INSTRUCTION            00083400
.SKP016A ANOP  ,                                                    DBC 00083500
         DS    BL1 -          DCBERROP - ERROR RECOVERY PROCEDURE BITS  00083600
         DS    FL1 -          DCBBUFCT - MAX NUMBER OF READ BUFFERS     00083700
         DS    X -            RESERVED                                  00083800
&P.EXLST DS    0A -           ADDRESS OF USER-PROVIDED EXIT LIST        00083900
         DS    FL1 -          DCBEIOBX - SIZE OF IOB                    00084000
&P.EXLSA DS    AL3 -          ADDRESS OF USER-PROVIDED EXIT LIST        00084100
         SPACE 1                                                        00084200
.C5A     ANOP                                                           00084300
         ORG   &P.DCB+33                                                00084400
&P.ERROP DS    BL1 -          ERROR RECOVERY PROCEDURE BITS             00084500
         AIF   (&#DCBSW(014)).SKP014A                               DBC 00084600
DCBERPT  EQU   BIT3 -         ON-LINE TEST FACILITIES TO BE USED        00084700
DCBERPC  EQU   BIT4 -         THRESHOLD AND CUMULATIVE ERROR COUNTS TO  00084800
*                             BE MAINTAINED                             00084900
DCBERPW  EQU   BIT5 -         TEXT-WRITE ERRORS TO BE RETRIED           00085000
DCBERPR  EQU   BIT6 -         TEXT-READ ERRORS TO BE RETRIED            00085100
DCBERPN  EQU   BIT7 -         IF ZERO, BASIC ERP TO BE FOLLOWED ---     00085200
*                             IF ONE, NO ERP TO BE FOLLOWED             00085300
.SKP014A ANOP  ,                                                    DBC 00085400
&P.BUFCT DS    FL1 -          CONTAINS MAXIMUM NUMBER OF BUFFERS TO BE  00085500
*                             OBTAINED BY BTAM FOR READ OPERATION       00085600
*                             (DYNAMIC BUFFERING ONLY)                  00085700
         SPACE 1                                                        00085800
         AIF   (&DSORGQX OR &DSORGTX).C5B                               00085900
&LSW(017) SETB (1)                                                  DBC 00086000
         ORG   &P.DCB+28                                                00086100
&P.DEVTP DS    FL1 -          INDEX TO DEVICE ENTRY IN THE DEVICE I/O   00086200
*                             DIRECTORY                                 00086300
         SPACE 1                                                        00086400
         ORG   &P.DCB+36                                                00086500
&P.EIOBX DS    FL1 -          SIZE OF EXTENDED IOB.  SIZE OF AN IOB     00086600
*                             ASSOCIATED WITH THIS DCB                  00086700
         SPACE 1                                                        00086800
.C5B     AIF   (NOT &DSORGTX).C5B1                                      00086900
&LSW(018) SETB (1)                                                  DBC 00087000
*                       TCAM LINE GROUP INTERFACE                       00087100
         SPACE 1                                                        00087200
         ORG   &P.DCB+20                                                00087300
&P.MHA   DS    0A -           SAME AS DCBMH BELOW                       00087400
&P.BUFIN DS    0BL1 -         NUMBER OF INPUT BUFFERS (BITS 0-3)        00087500
&P.BUFOU DS    BL1 -          NUMBER OF OUTPUT BUFFERS (BITS 4-7)       00087600
         AIF   (&#DCBSW(018)).SKP018A                               DBC 00087700
DCBBFIN  EQU   BIT0+BIT1+BIT2+BIT3 NUMBER OF BUFFERS ASSIGNED           00087800
*                             INITIALLY FOR RECEIVING OPERATIONS, FOR   00087900
*                             EACH LINE IN LINE GROUP                   00088000
DCBBFOUT EQU   BIT4+BIT5+BIT6+BIT7 NUMBER OF BUFFERS ASSIGNED           00088100
*                             INITIALLY FOR SENDING OPERATIONS, FOR     00088200
*                             EACH LINE IN LINE GROUP                   00088300
.SKP018A ANOP  ,                                                    DBC 00088400
&P.MH    DS    AL3 -          ADDRESS OF MESSAGE HANDLER FOR THIS LINE  00088500
*                             GROUP                                     00088600
         DS    FL1 -          DCBINTVL - NUMBER OF SECONDS OF           00088700
*                             INVITATION DELAY                          00088800
&P.PCI   DS    BL1 -          PROGRAM CONTROLLED INTERRUPTION HANDLING  00088900
         AIF   (&#DCBSW(018)).SKP018B                               DBC 00089000
DCBPCIX1 EQU   BIT0 -         PCI=(X,)                       ICBI DCB-8 00089100
DCBPCIX2 EQU   BIT1 -         PCI=(,X)                       ICBI DCB-8 00089200
DCBPCIA1 EQU   BIT2 -         PCI=(A,)                                  00089300
DCBPCIA2 EQU   BIT3 -         PCI=(,A)                                  00089400
DCBPCIN1 EQU   BIT4 -         PCI=(N,)                                  00089500
DCBPCIN2 EQU   BIT5 -         PCI=(,N)                                  00089600
DCBPCIR1 EQU   BIT6 -         PCI=(R,)                                  00089700
DCBPCIR2 EQU   BIT7 -         PCI=(,R)                                  00089800
.SKP018B ANOP  ,                                                    DBC 00089900
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *00090000
               &DSORGCQ OR &DSORGQS OR &DSORGBS OR &DSORGXE).C5B2       00090100
&LSW(019) SETB (1)                                                  DBC 00090200
&P       #DSORG                                                     DBC 00090300
         AGO   .C5B3                                                    00090400
.C5B2    ANOP                                                           00090500
&LSW(020) SETB (1)                                                  DBC 00090600
         DS    0BL2 -         DCBDSORG - DATA SET ORGANIZATION          00090700
         DS    BL1 -          DCBDSRG1 - FIRST BYTE OF DCBDSORG         00090800
         DS    BL1 -          DCBDSRG2 - SECOND BYTE OF DCBDSORG        00090900
.C5B3    ANOP                                                           00091000
&P.BUFMA DS    FL1 -          MAXIMUM NUMBER OF BUFFERS TO BE USED FOR  00091100
*                             DATA TRANSFER FOR EACH LINE IN THIS GROUP 00091200
         SPACE 1                                                        00091300
.C5B1    AIF   (NOT (&DSORGQX OR &DSORGTX)).C6                          00091400
&LSW(021) SETB (1)                                                  DBC 00091500
*                       QTAM LINE GROUP INTERFACE                       00091600
         SPACE 1                                                        00091700
         ORG   &P.DCB+20                                                00091800
&P.CLPS  DS    0A -           ADDRESS OF LINE PROCEDURE SPECIFICATION   00091900
*                             ROUTINE                                   00092000
&P.BUFRQ DS    FL1 -          NUMBER OF BUFFERS REQUESTED FOR A READ    00092100
*                             OR WRITE OPERATION                        00092200
&P.CLPSA DS    AL3 -          SAME AS DCBCLPS ABOVE                     00092300
&P.INTVL DS    FL1 -          NUMBER OF SECONDS OF INTENTIONAL DELAY    00092400
*                             BETWEEN PASSES THROUGH A POLLING LIST     00092500
*                             FOR NONSWITCHED LINES                     00092600
         DS    X -            RESERVED                                  00092700
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQS OR        *00092800
               &DSORGBS OR &DSORGXE OR &DSORGTX).C5C                    00092900
&LSW(022) SETB (1)                                                  DBC 00093000
&P       #DSORG                                                     DBC 00093100
&P.IOBAD DS    0A -           ADDRESS OF FIRST IOB                      00093200
&P.DEVTP DS    AL1 -          DEVICE TYPE POINTER                       00093300
&P.IOBAA DS    AL3 -          ADDRESS OF FIRST IOB                      00093400
&P.LCBAD DS    0A -           BASE FOR ADDRESSING LCB'S (BASE = ADDRESS 00093500
*                             OF FIRST LCB MINUS LENGTH OF ONE LCB)     00093600
&P.CPRI  DS    BL1 -          COMMUNICATION PRIORITY BITS               00093700
         AIF   (&#DCBSW(22) OR &#DCBSW(23)).SKP022A                 DBC 00093800
DCBCPR   EQU   BIT5 -         RECEIVING HAS PRIORITY                    00093900
DCBCPE   EQU   BIT6 -         RECEIVING AND SENDING HAVE EQUAL PRIORITY 00094000
DCBCPS   EQU   BIT7 -         SENDING HAS PRIORITY                      00094100
.SKP022A ANOP  ,                                                    DBC 00094200
&P.LCBA  DS    AL3 -          SAME AS DCBLCBAD ABOVE                    00094300
&P.EXLST DS    0A -           ADDRESS OF EXIT LIST                      00094400
&P.EIOBX DS    FL1 -          EXTENDED IOB INDEX.  SIZE OF LCB.         00094500
&P.EXLSA DS    AL3 -          ADDRESS OF EXIT LIST                      00094600
         SPACE 1                                                        00094700
         AGO   .C6                                                      00094800
.C5C     ANOP                                                           00094900
&LSW(023) SETB (1)                                                  DBC 00095000
         DS    0BL2 -         DCBDSORG - DATA SET ORGANIZATION          00095100
         DS    BL1 -          DCBDSRG1 - FIRST BYTE OF DCBDSORG         00095200
         DS    BL1 -          DCBDSRG2 - SECOND BYTE OF DCBDSORG        00095300
         AIF   (NOT &DSORGTX).C5C1                                      00095400
&LSW(024) SETB (1)                                                  DBC 00095500
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGCQ OR        *00095600
               &DSORGQS OR &DSORGBS OR &DSORGXE).C5C1                   00095700
&LSW(025) SETB (1)                                                  DBC 00095800
&P.IOBAD DS    0A -           ADDRESS OF FIRST IOB                      00095900
&P.DEVTP DS    AL1 -          DEVICE TYPE POINTER                       00096000
&P.IOBAA DS    AL3 -          ADDRESS OF FIRST IOB                      00096100
         SPACE 1                                                        00096200
         AGO   .C5C2                                                    00096300
.C5C1    ANOP                                                           00096400
&LSW(026) SETB (1)                                                  DBC 00096500
&P.DEVTP DS    AL1 -          DEVICE TYPE POINTER                       00096600
         DS    AL3 -          DCBIOBAA - ADDRESS OF FIRST IOB           00096700
         SPACE 1                                                        00096800
.C5C2    AIF   (NOT &DSORGTX).C5D                                       00096900
&LSW(027) SETB (1)                                                  DBC 00097000
         ORG   &P.DCB+32                                                00097100
&P.TRANA DS    0A -           ADDRESS OF TRANSLATION TABLE              00097200
         DS    BL1 -          DCBCPRI - COMMUNICATION PRIORITY BITS     00097300
&P.TRANS DS    AL3 -          ADDRESS OF TRANSLATION TABLE              00097400
         SPACE 1                                                        00097500
.C5D     ANOP                                                           00097600
         ORG   &P.DCB+32                                                00097700
&P.LCBAD DS    0A -           BASE FOR ADDRESSING LCB'S (BASE = ADDRESS 00097800
*                             OF FIRST LCB MINUS LENGTH OF ONE LCB)     00097900
&P.CPRI  DS    BL1 -          COMMUNICATION PRIORITY BITS               00098000
         AIF   (&#DCBSW(22) OR &#DCBSW(23)).SKP023A                 DBC 00098100
DCBCPR   EQU   BIT5 -         RECEIVING HAS PRIORITY                    00098200
DCBCPE   EQU   BIT6 -         RECEIVING AND SENDING HAVE EQUAL PRIORITY 00098300
DCBCPS   EQU   BIT7 -         SENDING HAS PRIORITY                      00098400
.SKP023A ANOP  ,                                                    DBC 00098500
&P.LCBA  DS    AL3 -          SAME AS DCBLCBAD ABOVE                    00098600
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQS OR        *00098700
               &DSORGBS OR &DSORGXE).C5E                                00098800
&LSW(028) SETB (1)                                                  DBC 00098900
&P.EXLST DS    0A -           ADDRESS OF EXIT LIST                      00099000
&P.EIOBX DS    FL1 -          EXTENDED IOB INDEX.  SIZE OF LCB          00099100
&P.EXLSA DS    AL3 -          ADDRESS OF EXIT LIST                      00099200
         SPACE 1                                                        00099300
         AGO   .C6                                                      00099400
.C5E     ANOP                                                           00099500
&LSW(029) SETB (1)                                                  DBC 00099600
&P.EIOBX DS    FL1 -          EXTENDED IOB INDEX.  SIZE OF LCB.         00099700
         DS    AL3 -          DCBEXLSA - ADDRESS OF EXIT LIST           00099800
         SPACE 1                                                        00099900
.C6      AIF   (NOT (&DSORGMQ OR &DSORGTQ)).C7                          00100000
&LSW(030) SETB (1)                                                  DBC 00100100
*                       QTAM PROBLEM PROGRAM MESSAGE QUEUE INTERFACE    00100200
         SPACE 1                                                        00100300
         ORG   &P.DCB+20                                                00100400
&P.TRMAD DS    0A -           ADDRESS OF USER-PROVIDED AREA IN WHICH    00100500
*                             THE TERMINAL NAME IS STORED               00100600
         AIF   (&DSORGQX OR &DSORGTX).C6A                               00100700
&LSW(031) SETB (1)                                                  DBC 00100800
&P.BUFRQ DS    FL1 -          NUMBER OF BUFFERS TO BE FILLED FROM THE   00100900
*                             DIRECT ACCESS QUEUE                       00101000
         AGO   .C6B                                                     00101100
.C6A     ANOP                                                           00101200
&LSW(032) SETB (1)                                                  DBC 00101300
         DS    FL1 -          DCBBUFRQ - NUMBER OF BUFFERS TO BE FILLED 00101400
*                             FROM THE DIRECT ACCESS QUEUE              00101500
.C6B     ANOP                                                           00101600
&P.TRMA  DS    AL3 -          SAME AS DCBTRMAD ABOVE                    00101700
&P.SOWA  DS    H -            SIZE OF USER-PROVIDED WORK AREA           00101800
         AIF   (&DSORGIS OR &DSORGBX OR &DSORGDA OR &DSORGQX OR        *00101900
               &DSORGQS OR &DSORGBS OR &DSORGXE OR &DSORGTX).C6C        00102000
&LSW(033) SETB (1)                                                  DBC 00102100
&P       #DSORG                                                     DBC 00102200
&P.IOBAD DS    0A -           BEFORE OPEN - ADDRESS OF AVT  ---         00102300
*                             AFTER OPEN - BASE FOR ADDRESSING IOB'S    00102400
*                             (BASE = ADDRESS OF FIRST IOB MINUS LENGTH 00102500
*                             OF ONE IOB)                               00102600
         DS    FL1 -          DCBBUFMA - MAXIMUM NUMBER OF BUFFERS TO   00102700
*                             BE USED FOR DATA TRANSFER FOR EACH LINE   00102800
*                             IN THIS GROUP                             00102900
&P.IOBAA DS    AL3 -          SAME AS DCBIOBAD ABOVE                    00103000
         AGO   .C6D                                                     00103100
.C6C     ANOP                                                           00103200
&LSW(034) SETB (1)                                                  DBC 00103300
         DS    0BL2 -         DCBDSORG - DATA SET ORGANIZATION          00103400
         DS    BL1 -          DCBDSRG1 - FIRST BYTE OF DCBDSORG         00103500
         DS    BL1 -          DCBDSRG2 - SECOND BYTE OF DCBDSORG        00103600
         DS    A -            DCBIOBAD - BASE FOR ADDRESSING IOB'S      00103700
.C6D     ANOP                                                           00103800
         SPACE 1                                                        00103900
         ORG   &P.DCB+28                                                00104000
&P.SEGAD DS    A -            ADDRESS OF CURRENT SEGMENT                00104100
         AIF   (NOT &DSORGTQ).C6D1                                      00104200
&LSW(035) SETB (1)                                                  DBC 00104300
&P.THRES DS    FL1 -          FOR NON-REUSABLE MESSAGE QUEUE RECORDS,   00104400
*                             PERCENTAGE OF NON-REUSABLE DISK MESSAGE   00104500
*                             QUEUE RECORDS TO BE USED BEFORE A FLUSH   00104600
*                             CLOSEDOWN OF THE SYSTEM IS INITIATED.     00104700
*                             FOR REUSABLE MESSAGE QUEUE RECORDS AND    00104800
*                             CHECKPOINT RECORDS, THIS FIELD IS         00104900
*                             RESERVED                                  00105000
         AGO   .C6D2                                                    00105100
.C6D1    ANOP                                                           00105200
&LSW(036) SETB (1)                                                  DBC 00105300
         DS    X -            RESERVED                                  00105400
.C6D2    ANOP                                                           00105500
         SPACE 1                                                        00105600
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR        *00105700
               &DSORGXE).C7                                             00105800
&LSW(037) SETB (1)                                                  DBC 00105900
         ORG   &P.DCB+32                                                00106000
&P.EODAD DS    A -            ADDRESS OF USER-PROVIDED ROUTINE          00106100
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX).C6E                   00106200
&LSW(038) SETB (1)                                                  DBC 00106300
&P.EXLST DS    0A -           ADDRESS OF EXIT LIST                      00106400
         AGO   .C6F                                                     00106500
.C6E     ANOP                                                           00106600
&LSW(039) SETB (1)                                                  DBC 00106700
         DS    0A -           DCBEXLST - ADDRESS OF EXIT LIST           00106800
.C6F     ANOP                                                           00106900
&P.RECFM DS    C -            RECORD FORMAT                             00107000
         AIF   (&#DCBSW(037)).SKP037A                               DBC 00107100
DCBRECR  EQU   X'02' -        RECORD                                    00107200
DCBRECG  EQU   X'04' -        MESSAGE                                   00107300
DCBRECS  EQU   X'08' -        SEGMENT                                   00107400
.SKP037A ANOP  ,                                                    DBC 00107500
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX).C6G                   00107600
&LSW(040) SETB (1)                                                  DBC 00107700
&P.EXLSA DS    AL3 -          ADDRESS OF EXIT LIST                      00107800
         AGO   .C7                                                      00107900
.C6G     ANOP                                                           00108000
&LSW(041) SETB (1)                                                  DBC 00108100
         DS    AL3 -          DCBEXLSA - ADDRESS OF EXIT LIST           00108200
.C7      ANOP                                                           00108300
         SPACE 1                                                        00108400
         AIF   (NOT &DSORGCQ OR (&DSORGIS OR &DSORGBX OR &DSORGDA OR   *00108500
               &DSORGQX OR &DSORGQS OR &DSORGBS OR &DSORGXE)).C8        00108600
&LSW(042) SETB (1)                                                  DBC 00108700
*                       QTAM DIRECT ACCESS MESSAGE QUEUE                00108800
         SPACE 1                                                        00108900
         ORG   &P.DCB+20                                                00109000
&P.BUFCB DS    0A -           ADDRESS OF TERMINAL TABLE                 00109100
&P.BUFNO DS    X -            RESERVED                                  00109200
&P.BUFCA DS    AL3 -          ADDRESS OF TERMINAL TABLE                 00109300
&P.BUFL  DS    H -            SIZE OF THE DATA IN BUFFER EQUATED TO     00109400
*                             IECKBUFL                                  00109500
&P       #DSORG                                                     DBC 00109600
&P.IOBAD DS    A -            ADDRESS OF IOB                            00109700
         SPACE 1                                                        00109800
.C8      ANOP                                                    S22024 00109900
         AIF   (NOT &DSORGTR).C8A7                               S22024 00110000
&LSW(043) SETB (1)                                                  DBC 00110100
         ORG   &P.DCB+20                                         S22024 00110200
&P.DUMPD DS    0A             FULL WORD LABEL                    S22024 00110300
&P.UNITN DS    BL1            NUMBER OF UNITS FOR READ FOLLOWING S22024 00110400
*                             ATTENTION.                         S22024 00110500
&P.DUMPA DS    AL3            ADDRESS OF THE DCB USED FOR        S22024 00110600
*                             DUMPING THE 3705                   S22024 00110700
         DS    AL1            RESERVED                           S22024 00110800
&P.TRSTA DS    BL1            STATUS BYTE. WHEN SET TO 1,        S22024 00110900
*                             THE INDICATORS HAVE THE SPECIFIED  S22024 00111000
*                             MEANING                            S22024 00111100
         AIF   (&#DCBSW(043)).SKP043A                               DBC 00111200
DCBAUTOI EQU   BIT0           IPLAUTO=YES WAS SPECIFIED IN THE   S22024 00111300
*                             DCB                                S22024 00111400
DCBAUTOD EQU   BIT1           DMPAUTO=YES WAS SPECIFIED IN THE   S22024 00111500
*                             DCB MACRO.                         S22024 00111600
DCBINITL EQU   BIT2           BRINGUP=YES WAS SPECIFIED IN THE   S22024 00111700
*                             DCB MACRO.                         S22024 00111800
DCBRSTRT EQU   BIT3           RESTART IS IN PROCESS              S22024 00111900
DCBIPLED EQU   BIT4           3705 HAS BEEN IPL'D.               S22024 00112000
DCBBAKUP EQU   BIT5           BACKUP=YES WAS SPECIFIED IN THE    S22024 00112100
*                             DCB MACRO.                         S22024 00112200
DCBNIDLE EQU   BIT6           IDLE=NO WAS SPECIFIED IN THE OPEN  S22024 00112300
*                             MACRO OR WAS IMPLIED BY DEFAULT    S22024 00112400
DCBCHNGL EQU   BIT7           IPL TEXT HAS BEEN CHANGED          S22024 00112500
.SKP043A ANOP  ,                                                    DBC 00112600
         AIF   (NOT &DSORGTR OR (&DSORGCQ OR &DSORGIS OR &DSORGBX OR   *00112700
               &DSORGDA OR &DSORGQX OR &DSORGQS OR &DSORGBS OR         *00112800
               &DSORGXE OR &DSORGMQ OR &DSORGTQ OR &DSORGTX)).C8A0      00112900
&LSW(044) SETB (1)                                                  DBC 00113000
&P.DSORG DS    0BL2           DATA SET ORGANIZATION BEING USED   S22024 00113100
&P.DSRG1 DS    BL1            FIRST BYTE OF DCBDSORG             S22024 00113200
&P.DSRG2 DS    BL1            SECOND BYTE OF DCBDSORG            S22024 00113300
         AIF   (&#DCBDSG OR &#DCBSW(44)).SKP044A                    DBC 00113400
DCBDSGTR EQU   BIT5           DSORG=TR SPECIFIED                 S22024 00113500
&P.IOBAD DS    A              ADDRESS OF IOB                     S22024 00113600
.SKP044A ANOP  ,                                                    DBC 00113700
         AGO   .C8A1                                             S22024 00113800
.C8A0    ANOP                                                    S22024 00113900
&LSW(045) SETB (1)                                                  DBC 00114000
         DS    H              DCBDSORG                           S22024 00114100
         DS    A              DCBIOBAD                           S22024 00114200
.C8A1    ANOP                                                    S22024 00114300
&P.RNCKD DS    0A             FULL WORD LABEL                    S22024 00114400
         DS    BL1            RESERVED                           S22024 00114500
&P.RNCKA DS    AL3            ADDRESS OF THE DCB USED TO RETAIN  S22024 00114600
*                             INCIDENT CHECKPOINT RECORDS        S22024 00114700
*                             GENERATED BY THE 3705.             S22024 00114800
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS OR        *00114900
               &DSORGXE OR &DSORGBX OR &DSORGQX OR &DSORGTX OR         *00115000
               &DSORGMQ OR &DSORGTQ).C8A2                        S22024 00115100
&LSW(046) SETB (1)                                                  DBC 00115200
&P.EXLST DS    0A             FULL WORD LABEL FOR EXLIST         S22024 00115300
         AGO   .C8A3                                             S22024 00115400
.C8A2    ANOP                                                    S22024 00115500
&LSW(047) SETB (1)                                                  DBC 00115600
         DS    0A             DCBEXLST                           S22024 00115700
.C8A3    ANOP                                                    S22024 00115800
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX).C8A4           S22024 00115900
&LSW(048) SETB (1)                                                  DBC 00116000
&P.EIOBX DS    BL1            SIZE, IN BYTES, OF THE IOB.        S22024 00116100
         AGO   .C8A5                                             S22024 00116200
.C8A4    ANOP                                                    S22024 00116300
&LSW(049) SETB (1)                                                  DBC 00116400
         DS    BL1            DCBEIOBX                           S22024 00116500
.C8A5    ANOP                                                    S22024 00116600
         AIF   (&DSORGBX OR &DSORGQX OR &DSORGTX OR &DSORGIS OR        *00116700
               &DSORGDA OR &DSORGQS OR &DSORGBS OR &DSORGXE  OR        *00116800
               &DSORGMQ OR &DSORGTQ).C8A6                        S22024 00116900
&LSW(050) SETB (1)                                                  DBC 00117000
&P.EXLSA DS    AL3            ADDRESS OF THE EXIT LIST.          S22024 00117100
         AGO   .C8A7                                             S22024 00117200
.C8A6    ANOP                                                    S22024 00117300
&LSW(051) SETB (1)                                                  DBC 00117400
         DS    AL3            DCBEXLSA                           S22024 00117500
.C8A7    AIF   (&DSORGLR AND NOT (&DSORGIS OR &DSORGBX OR &DSORGDA OR  *00117600
               &DSORGQX OR &DSORGCQ OR &DSORGMQ OR &DSORGXA OR         *00117700
               &DSORGQS OR &DSORGBS OR &DSORGXE OR &DSORGTR)).CF        00117800
&LSW(052) SETB (1)                                                  DBC 00117900
*                       FOUNDATION BEFORE OPEN                          00118000
         SPACE 1                                                        00118100
         ORG   &P.DCB+40                                                00118200
&P.DDNAM DS    CL8 -          NAME ON THE DD STATEMENT WHICH DEFINES    00118300
*                             THE DATA SET ASSOCIATED WITH THIS DCB     00118400
&P.OFLGS DS    BL1 -          FLAGS USED BY OPEN ROUTINE                00118500
         AIF   (&#DCBSW(052)).SKP052A                               DBC 00118600
DCBOFLWR EQU   BIT0 -         IF ZERO, LAST I/O OPERATION WAS READ OR   00118700
*                             POINT.  IF ONE, LAST I/O OPERATION WAS    00118800
*                             WRITE.                                    00118900
DCBOFIOD EQU   BIT0 -         DATA SET IS BEING OPENED FOR INPUT OR     00119000
*                             OUTPUT (BDAM)                             00119100
DCBOFLRB EQU   BIT1 -         LAST I/O OPERATION WAS IN READ BACKWARD   00119200
*                             MODE                                      00119300
         AIF   (&#DCBSW(108)).SKP052A                               DBC 00119400
DCBOFEOV EQU   BIT2 -         SET TO 1 BY EOV WHEN IT CALLS CLOSE       00119500
*                             ROUTINE FOR CONCATENATION OF DATA SETS    00119600
*                             WITH UNLIKE ATTRIBUTES                    00119700
DCBOFOPN EQU   BIT3 -         AN OPEN HAS BEEN SUCCESSFULLY COMPLETED   00119800
DCBOFPPC EQU   BIT4 -         SET TO 1 BY PROBLEM PROGRAM TO INDICATE A 00119900
*                             CONCATENATION OF UNLIKE ATTRIBUTES        00120000
DCBOFTM  EQU   BIT5 -         TAPE MARK HAS BEEN READ                   00120100
DCBOFUEX EQU   BIT6 -         SET TO 0 BY AN I/O SUPPORT FUNCTION WHEN  00120200
*                             THAT FUNCTION TAKES A USER EXIT. SET TO 1 00120300
*                             ON RETURN FROM USER EXIT TO THE I/O       00120400
*                             SUPPORT FUNCTION WHICH TOOK THE EXIT.     00120500
DCBOFIOF EQU   BIT7 -         SET TO 1 BY AN I/O SUPPORT FUNCTION IF    00120600
*                             DCB IS TO BE PROCESSED BY THAT FUNCTION   00120700
.SKP052A ANOP  ,                                                    DBC 00120800
&P.IFLG  DS    BL1 -          FLAGS USED BY IOS IN COMMUNICATING ERROR  00120900
*                             CONDITIONS AND IN DETERMINING CORRECTIVE  00121000
*                             PROCEDURES                                00121100
         AIF   (&#DCBSW(052)).SKP052B                               DBC 00121200
DCBIBEC  EQU   BIT0+BIT1 -    ERROR CORRECTION INDICATOR                00121300
DCBIFNEP EQU   X'00' -        NOT IN ERROR PROCEDURE                    00121400
DCBEX    EQU   BIT1           ERROR CORRECTION OR IOS PAGE FIX IN       00121500
*                             PROCESS                                   00121600
DCBIFPEC EQU   BIT0+BIT1 -    PERMANENT ERROR CORRECTION                00121700
DCBIBPCT EQU   BIT2+BIT3 -    PRINTER CARRIAGE TAPE PUNCH INDICATOR     00121800
DCBIFC9  EQU   BIT2 -         CHANNEL 9 PRINTER CARRIAGE TAPE PUNCH     00121900
*                             SENSED                                    00122000
DCBIFC12 EQU   BIT3 -         CHANNEL 12 PRINTER CARRIAGE TAPE PUNCH    00122100
*                             SENSED                                    00122200
DCBIBIOE EQU   BIT4+BIT5 -    IOS ERROR ROUTINE USE INDICATOR           00122300
DCBIFER  EQU   X'00' -        ALWAYS USE I/O SUPERVISOR ERROR ROUTINE   00122400
DCBIFNE1 EQU   BIT5 -         NEVER USE I/O SUPERVISOR ERROR ROUTINE    00122500
DCBIFTIM EQU   BIT5 -         TEST IOS MASK (IMSK) FOR ERROR PROCEDURE  00122600
*                             (BTAM)                                    00122700
DCBIFNE2 EQU   BIT4 -         NEVER USE I/O SUPERVISOR ERROR ROUTINE    00122800
DCBIFNE3 EQU   BIT4+BIT5 -    NEVER USE I/O SUPERVISOR ERROR ROUTINE    00122900
.SKP052B ANOP  ,                                                    DBC 00123000
&P.MACR  DS    0BL2 -         MACRO INSTRUCTION REFERENCE               00123100
&P.MACR1 DS    BL1 -          FIRST BYTE OF DCBMACR                     00123200
         AIF   (&#DCBSW(052)).SKP052E                               DBC 00123300
DCBMRECP EQU   BIT0 -         EXECUTE CHANNEL PROGRAM (EXCP) ---        00123400
*                             ALWAYS ZERO (BSAM, QSAM, BPAM, BISAM,     00123500
*                             QISAM, BDAM) --- RESERVED (QTAM, BTAM)    00123600
DCBMRFE  EQU   BIT1 -         FOUNDATION EXTENSION IS PRESENT (EXCP)    00123700
DCBMRGET EQU   BIT1 -         GET (QSAM, QISAM, TCAM)                   00123800
DCBMRPTQ EQU   BIT1 -         PUT FOR MESSAGE GROUP (QTAM) ---          00123900
*                             ALWAYS ZERO (BSAM, BPAM, BISAM, BDAM) --- 00124000
*                             RESERVED (BTAM)                           00124100
DCBMRAPG EQU   BIT2 -         APPENDAGES ARE REQUIRED (EXCP)            00124200
         AIF   (&#DCBSW(108)).SKP052C                               DBC 00124300
DCBMRRD  EQU   BIT2 -         READ (BSAM, BPAM, BISAM, BDAM, BTAM)      00124400
.SKP052C ANOP  ,                                                    DBC 00124500
DCBMRWRQ EQU   BIT2 -         WRITE FOR LINE GROUP (QTAM) ---           00124600
*                             ALWAYS ZERO (QSAM, QISAM)                 00124700
DCBMRCI  EQU   BIT3 -         COMMON INTERFACE (EXCP)                   00124800
DCBMRMVG EQU   BIT3 -         MOVE MODE OF GET (QSAM, QISAM)            00124900
DCBMRRDK EQU   BIT3 -         KEY SEGMENT WITH READ (BDAM) ---          00125000
*                             ALWAYS ZERO (BISAM) ---                   00125100
*                             RESERVED (BSAM, BPAM, QTAM, BTAM)         00125200
DCBMRLCG EQU   BIT4 -         LOCATE MODE OF GET (QSAM, QISAM)          00125300
DCBMRRDI EQU   BIT4 -         ID ARGUMENT WITH READ (BDAM) ---          00125400
*                             ALWAYS ZERO (BISAM) ---                   00125500
*                             RESERVED (EXCP, BSAM, BPAM, QTAM, BTAM)   00125600
DCBMRABC EQU   BIT5 -         USER'S PROGRAM MAINTAINS ACCURATE BLOCK   00125700
*                             COUNT (EXCP)                              00125800
DCBMRPT1 EQU   BIT5 -         POINT (WHICH IMPLIES NOTE) (BSAM, BPAM)   00125900
DCBMRSBG EQU   BIT5 -         SUBSTITUTE MODE OF GET (QSAM)             00126000
DCBMRDBF EQU   BIT5 -         DYNAMIC BUFFERING (BISAM, BDAM) ---       00126100
*                             ALWAYS ZERO (QISAM) ---                   00126200
*                             RESERVED (QTAM, BTAM)                     00126300
DCBPGFXA EQU   BIT6 -         PAGE FIX APPENDAGE IS SPECIFIED (EXCP)    00126400
         AIF   (&#DCBSW(108)).SKP052D                               DBC 00126500
DCBMRCRL EQU   BIT6 -         CNTRL (BSAM, QSAM)                        00126600
.SKP052D ANOP  ,                                                    DBC 00126700
DCBMRCHK EQU   BIT6 -         CHECK (BISAM)                             00126800
DCBMRRDX EQU   BIT6 -         READ EXCLUSIVE (BDAM) ---                 00126900
*                             RESERVED (BPAM, QISAM, QTAM, BTAM)        00127000
DCBMRDMG EQU   BIT7 -         DATA MODE OF GET (QSAM)                   00127100
DCBMRCK  EQU   BIT7 -         CHECK (BDAM) --- RESERVED (EXCP, BSAM,    00127200
*                             BPAM, BISAM, QISAM, QTAM, BTAM)           00127300
.SKP052E ANOP  ,                                                    DBC 00127400
&P.MACR2 DS    BL1 -          SECOND BYTE OF DCBMACR                    00127500
         AIF   (&#DCBSW(052)).SKP052H                               DBC 00127600
DCBMRSTL EQU   BIT0 -         SETL (QISAM) --- ALWAYS ZERO (BSAM, QSAM, 00127700
*                             BPAM, BISAM, BDAM) ---                    00127800
*                             RESERVED (EXCP, QTAM, BTAM)               00127900
DCBMRPUT EQU   BIT1 -         PUT (QSAM, TCAM) - PUT OR PUTX (QISAM)    00128000
DCBMRGTQ EQU   BIT1 -         GET FOR MESSAGE GROUP (QTAM) ---          00128100
*                             ALWAYS ZERO (BSAM, BPAM, BISAM, BDAM) --- 00128200
*                             RESERVED (EXCP, BTAM)                     00128300
         AIF   (&#DCBSW(108)).SKP052F                               DBC 00128400
DCBMRWRT EQU   BIT2 -         WRITE (BSAM, BPAM, BISAM, BDAM, BTAM)     00128500
.SKP052F ANOP  ,                                                    DBC 00128600
DCBMRRDQ EQU   BIT2 -         READ FOR LINE GROUP (QTAM) ---            00128700
*                             ALWAYS ZERO (QSAM, QISAM) ---             00128800
*                             RESERVED (EXCP)                           00128900
DCBMRMVP EQU   BIT3 -         MOVE MODE OF PUT (QSAM, QISAM)            00129000
DCBMRWRK EQU   BIT3 -         KEY SEGMENT WITH WRITE (BDAM) ---         00129100
*                             ALWAYS ZERO (BISAM) ---                   00129200
*                             RESERVED (EXCP, BSAM, BPAM, QTAM, BTAM)   00129300
DCBMR5WD EQU   BIT4 -         FIVE-WORD DEVICE INTERFACE (EXCP)         00129400
DCBMRLDM EQU   BIT4 -         LOAD MODE BSAM (CREATE BDAM DATA SET)     00129500
*                             (BSAM)                                    00129600
DCBMRLCP EQU   BIT4 -         LOCATE MODE OF PUT (QSAM, QISAM)          00129700
DCBMRIDW EQU   BIT4 -         ID ARGUMENT WITH WRITE (BDAM) ---         00129800
*                             ALWAYS ZERO (BISAM) ---                   00129900
*                             RESERVED (BPAM, QTAM, BTAM)               00130000
DCBMR4WD EQU   BIT5 -         FOUR-WORD DEVICE INTERFACE (EXCP)         00130100
DCBMRPT2 EQU   BIT5 -         POINT (WHICH IMPLIES NOTE) (BSAM, BPAM)   00130200
DCBMRTMD EQU   BIT5 -         SUBSTITUTE MODE (QSAM)                    00130300
DCBMRUIP EQU   BIT5 -         UPDATE IN PLACE (PUTX) (QISAM) ---        00130400
*                             ALWAYS ZERO (BISAM) ---                   00130500
*                             RESERVED (BDAM, QTAM, BTAM)               00130600
DCBMR3WD EQU   BIT6 -         THREE-WORD DEVICE INTERFACE (EXCP)        00130700
         AIF   (&#DCBSW(108)).SKP052G                               DBC 00130800
DCBMRCTL EQU   BIT6 -         CNTRL (BSAM, QSAM)                        00130900
.SKP052G ANOP  ,                                                    DBC 00131000
DCBMRSTK EQU   BIT6 -         SETL BY KEY (QISAM)                       00131100
DCBMRAWR EQU   BIT6 -         ADD TYPE OF WRITE (BDAM) ---              00131200
*                             ALWAYS ZERO (BISAM) ---                   00131300
*                             RESERVED (BPAM, QTAM, BTAM)               00131400
DCBMR1WD EQU   BIT7 -         ONE-WORD DEVICE INTERFACE (EXCP)          00131500
DCBMRSWA EQU   BIT7 -         USER'S PROGRAM HAS PROVIDED A SEGMENT     00131600
*                             WORK AREA POOL (BSAM CREATE BDAM, BDAM)   00131700
DCBMRDMD EQU   BIT7 -         DATA MODE (QSAM)                          00131800
DCBMRSTI EQU   BIT7 -         SETL BY ID (QISAM) ---                    00131900
*                             ALWAYS ZERO (BISAM) ---                   00132000
*                             RESERVED (BPAM, QTAM, BTAM)               00132100
.SKP052H ANOP  ,                                                    DBC 00132200
         SPACE 1                                                        00132300
*                       FOUNDATION AFTER OPEN                           00132400
         SPACE 1                                                        00132500
         ORG   &P.DCB+40                                                00132600
&P.TIOT  DS    H -            OFFSET FROM TIOT ORIGIN TO TIOELNGH FIELD 00132700
*                             IN TIOT ENTRY FOR DD STATEMENT ASSOCIATED 00132800
*                             WITH THIS DCB                             00132900
&P.MACRF DS    0BL2 -         SAME AS DCBMACR BEFORE OPEN               00133000
&P.MACF1 DS    BL1 -          FIRST BYTE OF DCBMACRF                    00133100
&P.MACF2 DS    BL1 -          SECOND BYTE OF DCBMACRF                   00133200
&P.DEBAD DS    0A -           ADDRESS OF ASSOCIATED DEB                 00133300
&P.IFLGS DS    BL1 -          SAME AS DCBIFLG BEFORE OPEN               00133400
         AIF   (&#DCBSW(052)).SKP052I                               DBC 00133500
DCBIFEC  EQU   BIT0+BIT1 -    ERROR CORRECTION INDICATOR                00133600
DCBIFPCT EQU   BIT2+BIT3 -    PRINTER CARRIAGE TAPE PUNCH INDICATOR     00133700
DCBIFIOE EQU   BIT4+BIT5 -    IOS ERROR ROUTINE USE INDICATOR           00133800
.SKP052I ANOP  ,                                                    DBC 00133900
&P.DEBA  DS    AL3 -          ADDRESS OF ASSOCIATED DEB                 00134000
         SPACE 1                                                        00134100
         AIF   (NOT (&DSORGBX OR &DSORGDA OR &DSORGQX OR &DSORGBS)).C8A 00134200
&LSW(053) SETB (1)                                                  DBC 00134300
         ORG   &P.DCB+48                                                00134400
&P.READ  DS    0A -           ADDRESS OF READ MODULE                    00134500
&P.WRITE DS    A -            ADDRESS OF WRITE MODULE                   00134600
         SPACE 1                                                        00134700
.C8A     AIF   (NOT (&DSORGIS OR &DSORGQX OR &DSORGMQ OR &DSORGQS OR   *00134800
               &DSORGTR)).C8B                                    S22024 00134900
&LSW(054) SETB (1)                                                  DBC 00135000
         ORG   &P.DCB+48                                                00135100
&P.GET   DS    0A -           ADDRESS OF GET MODULE                     00135200
&P.PUT   DS    A -            ADDRESS OF PUT MODULE                     00135300
         SPACE 1                                                        00135400
.C8B     ANOP                                                           00135500
         AIF   (NOT (&DSORGTX OR &DSORGTR)).C8B1                        00135600
&LSW(055) SETB (1)                                                  DBC 00135700
*                       TCAM LINE GROUP EXTENSION                       00135800
*                       3705 EXTENSION                                  00135900
         SPACE 1                                                        00136000
         ORG   &P.DCB+48                                                00136100
&P.SCTAB DS    0A -           ADDRESS OF SPECIAL CHARACTERS TABLE (SCT) 00136200
         DS    BL1 -          DCBOFLGS - FLAGS USED BY OPEN ROUTINE     00136300
&P.SCTAD DS    AL3 -          ADDRESS OF SPECIAL CHARACTERS TABLE (SCT) 00136400
&P.ILCT  DS    FL1 -          COUNT OF INVITATION LISTS                 00136500
&P.UNTCT DS    FL1 -          BEFORE OPEN - NUMERICAL VALUE OF SCT.     00136600
*                             AFTER OPEN - COUNT OF UNITS FOR 1 BUFFER. 00136700
&P.BUFSI DS    H -            SIZE OF ALL BUFFERS USED FOR THIS LINE    00136800
*                             GROUP                                     00136900
         AIF   (NOT &DSORGTX).C8B1                               S22024 00137000
&LSW(056) SETB (1)                                                  DBC 00137100
&P.RESER DS    0CL4 -         NUMBER OF RESERVED BYTES IN BUFFERS       00137200
&P.RESB1 DS    FL1 -          NUMBER OF BYTES RESERVED IN THE BUFFER    00137300
*                             RECEIVING FIRST INCOMING SEGMENT OF A     00137400
*                             MESSAGE                                   00137500
&P.RESB2 DS    FL1 -          NUMBER OF BYTES RESERVED IN ALL BUFFERS   00137600
*                             EXCEPT THE ONE CONTAINING FIRST SEGMENT   00137700
*                             OF A MESSAGE                              00137800
         DS    XL2 -          RESERVED                                  00137900
         SPACE 1                                                        00138000
*        THE FOLLOWING 4 BYTES MAY BE REPEATED 'N' TIMES                00138100
&P.INVLI DS    0A -           ADDRESS OF INVITATION LIST                00138200
&P.INVCI DS    BL1 -          TYPE OF COMMUNICATION INTERFACE FOR 2701  00138300
*                             DATA ADAPTER UNIT                         00138400
         AIF   (&#DCBSW(056)).SKP056A                               DBC 00138500
DCBINVB1 EQU   BIT2 -         IF ZERO, UNIT (A,)                        00138600
*                             IF ONE, UNIT (B,)                         00138700
DCBINVB2 EQU   BIT4 -         IF ZERO, UNIT (,A)                        00138800
*                             IF ONE, UNIT (,B)                         00138900
.SKP056A ANOP  ,                                                    DBC 00139000
&P.INVLA DS    AL3 -          ADDRESS OF INVITATION LIST                00139100
         SPACE 1                                                        00139200
.C8B1    ANOP                                                           00139300
         AIF   (NOT (&DSORGXA OR &DSORGXE)).C9                          00139400
&LSW(057) SETB (1)                                                  DBC 00139500
*                       EXCP WITH EXTENSION OR APPENDAGES               00139600
         SPACE 1                                                        00139700
         ORG   &P.DCB+52                                                00139800
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS).C8C       00139900
&LSW(058) SETB (1)                                                  DBC 00140000
&P.OPTCD DS    BL1 -          OPTION CODES                              00140100
         AGO   .C8D                                                     00140200
.C8C     ANOP                                                           00140300
&LSW(059) SETB (1)                                                  DBC 00140400
         DS    BL1 -          DCBOPTCD - OPTION CODES                   00140500
.C8D     ANOP                                                           00140600
         AIF   (&DSORGQS OR &DSORGBS).C8E                               00140700
&LSW(060) SETB (1)                                                  DBC 00140800
         AIF   (&#DCBSW(60) OR &#DCBSW(83)).SKP060A                 DBC 00140900
DCBOPTZ  EQU   BIT5 -         MAGNETIC TAPE DEVICES - USE REDUCED ERROR 00141000
*                             RECOVERY PROCEDURE (EXCP, BSAM, BPAM,     00141100
*                             QSAM)                                     00141200
DCBSRCHD EQU   BIT5 -         USE SEARCH DIRECT, INSTEAD OF SEARCH      00141300
*                             PREVIOUS, ON RECORD POSITION SENSING      00141400
*                             DEVICE  (EXCP, BSAM, BPAM, QSAM)   ICB217 00141500
.SKP060A ANOP  ,                                                    DBC 00141600
.C8E     ANOP                                                           00141700
         DS    XL7 -          RESERVED                                  00141800
         SPACE 1                                                        00141900
         AIF   (NOT &DSORGXA).C9                                        00142000
&LSW(061) SETB (1)                                                  DBC 00142100
*                       EXCP APPENDAGE LIST                             00142200
         SPACE 1                                                        00142300
         ORG   &P.DCB+60                                                00142400
&P.EOEA  DS    CL2 -          END OF EXTENT APPENDAGE ID                00142500
&P.PCIA  DS    CL2 -          PROGRAM CONTROLLED INTERRUPTION           00142600
*                             APPENDAGE ID                              00142700
&P.SIOA  DS    CL2 -          START I/O APPENDAGE ID                    00142800
&P.CENDA DS    CL2 -          CHANNEL END APPENDAGE ID                  00142900
&P.XENDA DS    CL2 -          ABNORMAL END APPENDAGE ID                 00143000
         DS    XL2 -          RESERVED                                  00143100
         SPACE 1                                                        00143200
.C9      AIF   (NOT &DSORGIS).CA                                        00143300
&LSW(062) SETB (1)                                                  DBC 00143400
*                       BISAM-QISAM INTERFACE                           00143500
         SPACE 1                                                        00143600
         ORG   &P.DCB+52                                                00143700
&P.OPTCD DS    BL1 -          OPTION CODES                              00143800
         AIF   (&DSORGQS OR &DSORGBS).C9A                               00143900
&LSW(063) SETB (1)                                                  DBC 00144000
         AIF   (&#DCBSW(63) OR &#DCBSW(68) OR &#DCBSW(38)).SKP063A  DBC 00144100
DCBOPTW  EQU   BIT0 -         WRITE VALIDITY CHECK (DASD)               00144200
*                             (BSAM, BPAM, QSAM, ISAM, BDAM)            00144300
.SKP063A ANOP  ,                                                    DBC 00144400
         AGO   .C9B                                                     00144500
.C9A     ANOP                                                           00144600
&LSW(064) SETB (1)                                                  DBC 00144700
*        BIT0 IS DCBOPTW - SAME AS BSAM                                 00144800
.C9B     ANOP                                                           00144900
         AIF   (&#DCBSW(062)).SKP062A                               DBC 00145000
DCBOPTUF EQU   BIT1 -         FULL-TRACK INDEX WRITE                    00145100
DCBOPTM  EQU   BIT2 -         MASTER INDEXES                            00145200
DCBOPTI  EQU   BIT3 -         INDEPENDENT OVERFLOW AREA                 00145300
DCBOPTY  EQU   BIT4 -         CYLINDER OVERFLOW AREA                    00145400
DCBOPTL  EQU   BIT6 -         DELETE OPTION                             00145500
DCBOPTR  EQU   BIT7 -         REORGANIZATION CRITERIA                   00145600
.SKP062A ANOP  ,                                                    DBC 00145700
&P.MAC   DS    BL1 -          EXTENSION OF DCBMACRF FIELD FOR ISAM      00145800
         AIF   (&#DCBSW(062)).SKP062B                               DBC 00145900
DCBMACUR EQU   BIT4 -         UPDATE FOR READ                           00146000
DCBMACUW EQU   BIT5 -         UPDATE TYPE OF WRITE                      00146100
DCBMACAW EQU   BIT6 -         ADD TYPE OF WRITE                         00146200
DCBMACRE EQU   BIT7 -         READ EXCLUSIVE                            00146300
.SKP062B ANOP  ,                                                    DBC 00146400
&P.NTM   DS    FL1 -          NUMBER OF TRACKS THAT DETERMINE THE       00146500
*                             DEVELOPMENT OF A MASTER INDEX             00146600
*                             MAXIMUM PERMISSABLE VALUE - 99            00146700
&P.CYLOF DS    FL1 -          NUMBER OF TRACKS TO BE RESERVED ON EACH   00146800
*                             PRIME DATA CYLINDER FOR RECORDS THAT      00146900
*                             OVERFLOW FROM OTHER TRACKS ON THAT        00147000
*                             CYLINDER                                  00147100
&P.SYNAD DS    A -            ADDRESS OF USER'S SYNAD ROUTINE           00147200
&P.RKP   DS    H -            RELATIVE POSITION OF FIRST BYTE OF KEY    00147300
*                             WITHIN EACH LOGICAL RECORD                00147400
&P.BLKSI DS    H -            BLOCK SIZE                                00147500
&P.LPDT  DS    0BL8 -         FOR RESUME LOAD,THE LAST PRIME DATA       00147600
*                             TRACK ON THE LAST PRIME DATA CYLINDER     00147700
*                             IN THE FORM MBBCCHHR.          ICBI DCB-5 00147800
&P.MSWA  DS    A -            ADDRESS OF MAIN STORAGE WORK AREA FOR USE 00147900
*                             BY CONTROL PROGRAM WHEN NEW RECORDS ARE   00148000
*                             BEING ADDED TO AN EXISTING DATA SET       00148100
&P.SMSI  DS    H -            NUMBER OF BYTES IN AREA RESERVED TO HOLD  00148200
*                             HIGHEST LEVEL INDEX                       00148300
&P.SMSW  DS    H -            NUMBER OF BYTES IN WORK AREA USED BY      00148400
*                             CONTROL PROGRAM WHEN NEW RECORDS ARE      00148500
*                             BEING ADDED TO DATA SET                   00148600
&P.MSHI  DS    0A -           ADDRESS OF MAIN STORAGE AREA TO HOLD      00148700
*                             HIGHEST LEVEL INDEX                       00148800
&P.NCP   DS    FL1 -          NUMBER OF COPIES OF READ-WRITE (TYPE K)   00148900
*                             CHANNEL PROGRAMS THAT ARE TO BE           00149000
*                             ESTABLISHED FOR THIS DCB.  (99 MAXIMUM)   00149100
&P.MSHIA DS    AL3 -          SAME AS DCBMSHI ABOVE                     00149200
&P.SETL  DS    A -            ADDRESS OF SETL MODULE FOR QISAM.         00149300
*                             ADDRESS OF CHECK MODULE FOR BISAM         00149400
&P.EXCD1 DS    BL1 -          FIRST BYTE IN WHICH EXCEPTIONAL           00149500
*                             CONDITIONS DETECTED IN PROCESSING DATA    00149600
*                             RECORDS ARE REPORTED TO THE USER          00149700
         AIF   (&#DCBSW(062)).SKP062C                               DBC 00149800
DCBEXNKY EQU   BIT0 -         LOWER KEY LIMIT NOT FOUND                 00149900
DCBEXIDA EQU   BIT1 -         INVALID DEVICE ADDRESS FOR LOWER LIMIT    00150000
DCBEXNSP EQU   BIT2 -         SPACE NOT FOUND                           00150100
DCBEXINV EQU   BIT3 -         INVALID REQUEST                           00150200
DCBEXIER EQU   BIT4 -         UNCORRECTABLE INPUT ERROR                 00150300
DCBEXOER EQU   BIT5 -         UNCORRECTABLE OUTPUT ERROR                00150400
DCBEXBLI EQU   BIT6 -         BLOCK COULD NOT BE REACHED (INPUT)        00150500
DCBEXBLU EQU   BIT7 -         BLOCK COULD NOT BE REACHED (UPDATE)       00150600
.SKP062C ANOP  ,                                                    DBC 00150700
&P.EXCD2 DS    BL1 -          SECOND BYTE IN WHICH EXCEPTIONAL          00150800
*                             CONDITIONS DETECTED IN PROCESSING DATA    00150900
*                             RECORDS ARE REPORTED TO THE USER          00151000
         AIF   (&#DCBSW(062)).SKP062D                               DBC 00151100
DCBEXSEQ EQU   BIT0 -         SEQUENCE CHECK                            00151200
DCBEXDUP EQU   BIT1 -         DUPLICATE RECORD                          00151300
DCBEXCLD EQU   BIT2 -         DCB CLOSED WHEN ERROR WAS DETECTED        00151400
DCBEXOFL EQU   BIT3 -         OVERFLOW RECORD                           00151500
DCBEXLTH EQU   BIT4 -         FOR PUT - LENGTH FIELD OF RECORD LARGER   00151600
*                             THAN LENGTH INDICATED IN DCBLRECL         00151700
DCBEXRDE EQU   BIT4 -         READ EXCLUSIVE                            00151800
.SKP062D ANOP  ,                                                    DBC 00151900
&P.LRECL DS    H -            FOR FIXED-LENGTH RECORD FORMATS, LOGICAL  00152000
*                             RECORD LENGTH.  FOR VARIABLE-LENGTH       00152100
*                             RECORD FORMATS, MAXIMUM LOGICAL RECORD    00152200
*                             LENGTH OR AN ACTUAL LOGICAL RECORD LENGTH 00152300
*                             CHANGED DYNAMICALLY BY USER WHEN CREATING 00152400
*                             THE DATA SET                              00152500
&P.ESETL DS    A -            ADDRESS OF ESETL ROUTINE IN GET MODULE    00152600
&P.LRAN  DS    A -            ADDRESS OF READ-WRITE K MODULE OR         00152700
*                             EXCLUSIVE MODULE                          00152800
&P.LWKN  DS    A -            ADDRESS OF WRITE KN MODULE                00152900
&P.RELSE DS    A -            WORK AREA FOR TEMPORARY STORAGE OF        00153000
*                             REGISTER CONTENTS                         00153100
&P.PUTX  DS    A -            WORK AREA FOR TEMPORARY STORAGE OF        00153200
*                             REGISTER CONTENTS                         00153300
&P.RELEX DS    A -            ADDRESS OF READ EXCLUSIVE MODULE          00153400
&P.FREED DS    A -            ADDRESS OF DYNAMIC BUFFERING MODULE       00153500
&P.HIRTI DS    FL1 -          NUMBER OF INDEX ENTRIES THAT FIT ON A     00153600
*                             PRIME DATA TRACK                          00153700
&P.FTMI2 DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST     00153800
*                             TRACK OF SECOND LEVEL MASTER INDEX (IN    00153900
*                             THE FORM MBBCCHH)                         00154000
&P.LEMI2 DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST      00154100
*                             ACTIVE ENTRY IN SECOND LEVEL MASTER INDEX 00154200
*                             (IN THE FORM CCHHR)                       00154300
&P.FTMI3 DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST     00154400
*                             TRACK OF THIRD LEVEL MASTER INDEX (IN     00154500
*                             THE FORM MBBCCHH)                         00154600
&P.LEMI3 DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST      00154700
*                             ACTIVE ENTRY IN THIRD LEVEL MASTER INDEX  00154800
*                             (IN THE FORM CCHHR)                       00154900
&P.NLEV  DS    FL1 -          NUMBER OF LEVELS OF INDEX                 00155000
&P.FIRSH DS    CL3 -          HHR OF FIRST DATA RECORD ON EACH          00155100
*                             CYLINDER.  FOR VARIABLE LENGTH RECORD     00155200
*                             PROCESSING, R PORTION OF THIS FIELD IS    00155300
*                             ALWAYS X'01'.                             00155400
&P.HMASK DS    CL1 -          BYTE INDICATING 2301 OR NOT               00155500
         AIF   (&#DCBSW(062)).SKP062E                               DBC 00155600
DCBHMDRM EQU   X'07' -        DEVICE IS 2301 DRUM                       00155700
DCBHMNDM EQU   X'FF' -        DEVICE IS OTHER THAN 2301 DRUM            00155800
.SKP062E ANOP  ,                                                    DBC 00155900
&P.LDT   DS    CL2 -          HH IS THE LAST PRIME DATA TRACK ON EACH   00156000
*                             CYLINDER                                  00156100
&P.HIRCM DS    CL1 -          HIGHEST POSSIBLE R FOR TRACKS OF THE      00156200
*                             CYLINDER AND MASTER INDICES               00156300
&P.HIRPD DS    CL1 -          HIGHEST R ON ANY PRIME TRACK IN DATA SET. 00156400
*                             FOR VARIABLE-LENGTH RECORDS, THIS         00156500
*                             REPRESENTS THE GREATEST NUMBER OF         00156600
*                             PHYSICAL RECORDS ON ANY PRIME TRACK IN    00156700
*                             THE DATA SET                              00156800
&P.HIROV DS    CL1 -          FOR FIXED-LENGTH RECORD FORMAT, HIGHEST   00156900
*                             POSSIBLE R FOR OVERFLOW DATA TRACKS.  FOR 00157000
*                             VARIABLE-LENGTH RECORD FORMAT, UNUSED.    00157100
&P.HIRSH DS    CL1 -          FOR FIXED-LENGTH RECORD FORMAT, R OF LAST 00157200
*                             DATA RECORD ON A SHARED TRACK, IF         00157300
*                             APPLICABLE.  FOR VARIABLE-LENGTH RECORD   00157400
*                             FORMAT, UNUSED.                           00157500
&P.TDC   DS    H -            USER-SUPPLIED NUMBER OF RECORDS TAGGED    00157600
*                             FOR DELETION.                             00157700
&P.NCRHI DS    H -            NUMBER OF STORAGE LOCATIONS NEEDED TO     00157800
*                             HOLD THE HIGHEST LEVEL INDEX              00157900
&P.RORG3 DS    F -            FOR EACH USE OF DATA SET, NUMBER OF READ  00158000
*                             OR WRITE ACCESSES TO AN OVERFLOW RECORD   00158100
*                             WHICH IS NOT FIRST IN A CHAIN OF SUCH     00158200
*                             RECORDS                                   00158300
&P.NREC  DS    F -            NUMBER OF LOGICAL RECORDS IN PRIME DATA   00158400
*                             AREA                                      00158500
&P.ST    DS    BL1 -          STATUS INDICATORS                         00158600
         AIF   (&#DCBSW(062)).SKP062F                               DBC 00158700
DCBSTSSM EQU   BIT0 -         SINGLE SCHEDULE MODE                      00158800
DCBSTKSQ EQU   BIT1 -         KEY SEQUENCE CHECKING IS TO BE PERFORMED  00158900
DCBSTLOD EQU   BIT2 -         LOADING HAS COMPLETED.  SET TO 1 BY CLOSE 00159000
*                             ROUTINE AND TO 0 BY FIRST EXECUTION OF    00159100
*                             PUT ROUTINE.                              00159200
DCBSTNCY EQU   BIT3 -         EXTENSION OF DATA SET WILL BEGIN ON NEW   00159300
*                             CYLINDER                                  00159400
DCBSTNMC EQU   BIT5 -         FIRST MACRO INSTRUCTION NOT YET RECEIVED  00159500
DCBSTLBF EQU   BIT6 -         LAST BLOCK FULL                           00159600
DCBSTLTF EQU   BIT7 -         LAST TRACK FULL                           00159700
.SKP062F ANOP  ,                                                    DBC 00159800
&P.FTCI  DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST     00159900
*                             TRACK OF CYLINDER INDEX (IN THE FORM      00160000
*                             MBBCCHH).                                 00160100
&P.HIIOV DS    CL1 -          FOR FIXED LENGTH RECORD FORMAT, HIGHEST   00160200
*                             POSSIBLE R FOR INDEPENDENT OVERFLOW DATA  00160300
*                             TRACKS.  FOR VARIABLE LENGTH RECORD       00160400
*                             FORMAT, UNUSED                            00160500
&P.FTMI1 DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST     00160600
*                             TRACK OF FIRST LEVEL MASTER INDEX (IN     00160700
*                             THE FORM MBBCCHH).                        00160800
&P.NTHI  DS    FL1 -          NUMBER OF TRACKS OF HIGH-LEVEL INDEX      00160900
&P.FTHI  DS    CL7 -          DIRECT ACCESS DEVICE ADDRESS OF FIRST     00161000
*                             TRACK OF HIGHEST LEVEL INDEX (IN THE      00161100
*                             FORM MBBCCHH).                            00161200
&P.LPDA  DS    CL8 -          DIRECT ACCESS DEVICE ADDRESS OF LAST      00161300
*                             PRIME DATA RECORD IN PRIME DATA AREA      00161400
*                             (IN THE FORM MBBCCHHR).                   00161500
&P.LETI  DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST      00161600
*                             ACTIVE NORMAL ENTRY OF TRACK INDEX ON     00161700
*                             LAST ACTIVE CYLINDER (IN THE FORM CCHHR). 00161800
&P.OVDEV DS    CL1 -          DEVICE TYPE FOR INDEPENDENT OVERFLOW      00161900
         AIF   (&#DCBSW(062)).SKP062G                               DBC 00162000
*        THESE SAME MASKS APPLY TO DCBDEVT FOR ISAM DIRECT ACCESS       00162100
DCBDVI11 EQU   X'01' -        2311 DISK DRIVE                           00162200
DCBDVI01 EQU   X'02' -        2301 PARALLEL DRUM                        00162300
DCBDVI03 EQU   X'03' -        2303 SERIAL DRUM                          00162400
DCBDVI02 EQU   X'04' -        2302 DISK STORAGE                         00162500
DCBDVI21 EQU   X'05' -        2321 DATA CELL DRIVE                      00162600
DCBDVI14 EQU   X'08' -        2314 DISK STORAGE FACILITY                00162700
.SKP062G ANOP  ,                                                    DBC 00162800
&P.NBOV  DS    H -            FOR FIXED LENGTH RECORD FORMAT, RESERVED. 00162900
*                             FOR VARIABLE LENGTH RECORD FORMAT, IF THE 00163000
*                             INDEPENDENT OVERFLOW OPTION IS SELECTED,  00163100
*                             CONTAINS, IN BINARY, NUMBER OF BYTES LEFT 00163200
*                             ON CURRENT TRACK OF INDEPENDENT OVERFLOW  00163300
*                             AREA                                      00163400
&P.LECI  DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST      00163500
*                             ACTIVE ENTRY IN CYLINDER INDEX (IN THE    00163600
*                             FORM CCHHR).                              00163700
         DS    X -            RESERVED                                  00163800
&P.RORG2 DS    H -            NUMBER OF TRACKS (PARTIALLY OR WHOLLY)    00163900
*                             REMAINING IN INDEPENDENT OVERFLOW AREA    00164000
&P.LEMI1 DS    CL5 -          DIRECT ACCESS DEVICE ADDRESS OF LAST      00164100
*                             ACTIVE ENTRY IN FIRST LEVEL MASTER INDEX  00164200
*                             (IN THE FORM CCHHR).                      00164300
         DS    X -            RESERVED                                  00164400
&P.NOREC DS    H -            NUMBER OF LOGICAL RECORDS IN AN OVERFLOW  00164500
*                             AREA                                      00164600
&P.LIOV  DS    CL8 -          DIRECT ACCESS DEVICE ADDRESS OF LAST      00164700
*                             AREA (IN THE FORM MBBCCHHR).              00164800
&P.RORG1 DS    H -            NUMBER OF CYLINDER OVERFLOW AREAS THAT    00164900
*                             ARE FULL                                  00165000
         DS    XL2 -          RESERVED                                  00165100
&P.WKPT1 DS    A -            POINTER TO WORK AREA OR TO CONSTRUCTED    00165200
*                             CHANNEL PROGRAM FOR WHICH SPACE IS        00165300
*                             OBTAINED BY GETMAIN MACRO INSTRUCTIONS    00165400
*                             ISSUED BY OPEN EXECUTORS                  00165500
&P.WKPT2 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1         00165600
&P.WKPT3 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1         00165700
&P.WKPT4 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1         00165800
&P.WKPT5 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1         00165900
&P.WKPT6 DS    A -            ADDITIONAL POINTER AS IN DCBWKPT1         00166000
         SPACE 1                                                        00166100
.CA      AIF   (NOT &DSORGDA).CB                                        00166200
&LSW(065) SETB (1)                                                  DBC 00166300
*                       BDAM INTERFACE                                  00166400
         SPACE 1                                                        00166500
         ORG   &P.DCB+52                                                00166600
&P.CHECK DS    0A -           ADDRESS OF CHECK MODULE                   00166700
         AIF   (&DSORGIS).CA1                                           00166800
&LSW(066) SETB (1)                                                  DBC 00166900
&P.OPTCD DS    BL1 -          OPTION CODES                              00167000
         AGO   .CA2                                                     00167100
.CA1     ANOP                                                           00167200
&LSW(067) SETB (1)                                                  DBC 00167300
         DS    BL1 -          DCBOPTCD - OPTION CODES                   00167400
.CA2     ANOP                                                           00167500
         AIF   (&DSORGIS OR &DSORGQS OR &DSORGBS).CA3                   00167600
&LSW(068) SETB (1)                                                  DBC 00167700
         AIF   (&#DCBSW(63) OR &#DCBSW(68) OR &#DCBSW(38)).SKP068A  DBC 00167800
DCBOPTW  EQU   BIT0 -         WRITE VALIDITY CHECK (DASD)               00167900
*                             (BSAM, BPAM, QSAM, ISAM, BDAM)            00168000
.SKP068A ANOP  ,                                                    DBC 00168100
         AGO   .CA4                                                     00168200
.CA3     ANOP                                                           00168300
&LSW(069) SETB (1)                                                  DBC 00168400
*        BIT0 IS DCBOPTW - SAME AS BSAM AND ISAM                        00168500
.CA4     ANOP                                                           00168600
         AIF   (&#DCBSW(065)).SKP065A                               DBC 00168700
DCBOPTTO EQU   BIT1 -         TRACK OVERFLOW                            00168800
DCBOPTE  EQU   BIT2 -         EXTENDED SEARCH                           00168900
DCBOPTF  EQU   BIT3 -         FEEDBACK                                  00169000
DCBOPTA  EQU   BIT4 -         ACTUAL ADDRESSING                         00169100
DCBOPTDB EQU   BIT5 -         DYNAMIC BUFFERING                         00169200
DCBOPTRE EQU   BIT6 -         READ EXCLUSIVE                            00169300
DCBOPTRB EQU   BIT7 -         RELATIVE BLOCK ADDRESSING                 00169400
.SKP065A ANOP  ,                                                    DBC 00169500
&P.CHCKA DS    AL3 -          ADDRESS OF CHECK MODULE                   00169600
         AIF   (&DSORGIS).CAA                                           00169700
&LSW(070) SETB (1)                                                  DBC 00169800
&P.SYNAD DS    A -            ADDRESS OF SYNAD ROUTINE                  00169900
         DS    XL2 -          RESERVED                                  00170000
&P.BLKSI DS    H -            MAXIMUM BLOCK SIZE                        00170100
         AGO   .CAB                                                     00170200
.CAA     ANOP                                                           00170300
&LSW(071) SETB (1)                                                  DBC 00170400
         DS    A -            DCBSYNAD - ADDRESS OF SYNAD ROUTINE       00170500
         DS    XL2 -          RESERVED                                  00170600
         DS    H -            DCBBLKSI - MAXIMUM BLOCK SIZE             00170700
.CAB     ANOP                                                           00170800
&P.IOBSQ DS    A -            ADDRESS OF FIRST IOB ON UNSCHEDULED QUEUE 00170900
*                             FOR EITHER A WRITE-ADD REQUEST WHEN       00171000
*                             ANOTHER WRITE-ADD IS IN PROGRESS OR A     00171100
*                             READ-EXCLUSIVE REQUEST WHEN THE           00171200
*                             READ-EXCLUSIVE LIST IS FULL               00171300
&P.SQND  DS    A -            ADDRESS OF LAST IOB ON UNSCHEDULED QUEUE  00171400
&P.IOBUQ DS    A -            ADDRESS OF FIRST IOB ON UNPOSTED QUEUE    00171500
&P.UQND  DS    A -            ADDRESS OF LAST JOB ON UNPOSTED QUEUE     00171600
*                             THAT IS MAINTAINED BY THE READ EXCLUSIVE  00171700
*                             MODULE                                    00171800
         DS    X -            RESERVED                                  00171900
&P.LIMCT DS    FL3 -          NUMBER OF TRACKS OR NUMBER OF RELATIVE    00172000
*                             BLOCKS TO BE SEARCHED (EXTENDED SEARCH    00172100
*                             OPTION)                                   00172200
&P.XARG  DS    0A -           ADDRESS OF READ EXCLUSIVE LIST            00172300
&P.XCNT  DS    FL1 -          NUMBER OF ENTRIES IN READ EXCLUSIVE LIST  00172400
&P.XARGA DS    AL3 -          ADDRESS OF READ EXCLUSIVE LIST            00172500
&P.DRDX  DS    0A -           ADDRESS OF READ EXCLUSIVE MODULE          00172600
&P.MVXNO DS    FL1 -          TOTAL NUMBER OF EXTENTS IN MULTIVOLUME    00172700
*                             DATA SET                                  00172800
&P.DRDXA DS    AL3 -          ADDRESS OF READ EXCLUSIVE MODULE          00172900
&P.DFOR  DS    A -            ADDRESS OF A FORMAT MODULE                00173000
&P.DFBK  DS    A -            ADDRESS OF A FEEDBACK MODULE              00173100
&P.DYNB  DS    A -            FOR DYNAMIC BUFFERING, ADDRESS OF DYNAMIC 00173200
*                             BUFFER MODULE.  FOR UNBLOCKED SPANNED     00173300
*                             RECORDS WITH BFTEK=R SPECIFIED AND NO     00173400
*                             DYNAMIC BUFFERING, ADDRESS OF SEGMENT     00173500
*                             WORK AREA CONTROL BLOCK                   00173600
         SPACE 1                                                        00173700
.CB      AIF   (NOT &DSORGQX).CC                                        00173800
&LSW(072) SETB (1)                                                  DBC 00173900
*                       QTAM INTERFACE                                  00174000
         SPACE 1                                                        00174100
         ORG   &P.DCB+52                                                00174200
&P.KSTAT DS    0CL4 -         FOUR THRESHOLD VALUES FOR ERROR COUNTS    00174300
&P.KSTA1 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF             00174400
*                             TRANSMISSIONS                             00174500
&P.KSTA2 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF DATA CHECKS 00174600
&P.KSTA3 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF             00174700
*                             INTERVENTIONS REQUIRED                    00174800
&P.KSTA4 DS    FL1 -          THRESHOLD VALUE FOR NUMBER OF TIMEOUTS    00174900
         SPACE 1                                                        00175000
*                       QTAM POLLING LIST ORIGIN                        00175100
         SPACE 1                                                        00175200
&P.CPOLL DS    0A -           A 4-BYTE FIELD FOR EACH POLLING LIST      00175300
&P.PLBYT DS    BL1 -          ADAPTER TYPE                              00175400
         AIF   (&#DCBSW(072)).SKP072A                               DBC 00175500
DCBCPWTT EQU   BIT4 -         WTTA                                      00175600
.SKP072A ANOP  ,                                                    DBC 00175700
&P.CPOLA DS    AL3 -          ADDRESS OF THE POLLING LIST               00175800
         SPACE 1                                                        00175900
.CC      AIF   (NOT &DSORGTQ).CC1                                       00176000
&LSW(073) SETB (1)                                                  DBC 00176100
*                       TCAM MESSAGE QUEUE INTERFACE                    00176200
         SPACE 1                                                        00176300
         ORG   &P.DCB+52                                                00176400
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS).CC1A      00176500
&LSW(074) SETB (1)                                                  DBC 00176600
&P.OPTCD DS    BL1 -          OPTION CODES                              00176700
         AGO   .CC1B                                                    00176800
.CC1A    ANOP                                                           00176900
&LSW(075) SETB (1)                                                  DBC 00177000
         DS    BL1 -          DCBOPTCD - OPTION CODES                   00177100
.CC1B    ANOP                                                           00177200
         AIF   (&#DCBSW(073)).SKP073A                               DBC 00177300
DCBOPTWP EQU   BIT0 -         SOURCE OR DESTINATION NAME PRECEDES       00177400
*                             MESSAGE (AFTER CONTROL BYTE)              00177500
*                             (TCAM PROCESS QUEUE)                      00177600
DCBOPTUM EQU   BIT1 -         WORK UNIT IS A MESSAGE.  DEFAULT WORK     00177700
*                             UNIT IS A RECORD.  (TCAM PROCESS QUEUE)   00177800
DCBOPTCB EQU   BIT2 -         CONTROL BYTE PRECEDES WORK UNIT           00177900
*                             (TCAM PROCESS QUEUE)                      00178000
DCBOPTCP EQU   BIT2 -         CHECKPOINT DATA SET                       00178100
DCBOPTIM EQU   BIT6 -         NON-REUSABLE MESSAGE QUEUE DATA SET       00178200
DCBOPTRM EQU   BIT7 -         REUSABLE MESSAGE QUEUE DATA SET           00178300
.SKP073A ANOP  ,                                                    DBC 00178400
         AIF   (&DSORGIS OR &DSORGDA OR &DSORGQS OR &DSORGBS).CC1D      00178500
&LSW(076) SETB (1)                                                  DBC 00178600
*                                                            ICBI DCB-9 00178700
         DS    XL9 -           RESERVED                      ICBI DCB-9 00178800
&P.BLKSI DS    H -             BLOCK SIZE                    ICBI DCB-9 00178900
         AGO   .CC1E                                         ICBI DCB-9 00179000
.CC1D    ANOP                                                ICBI DCB-9 00179100
&LSW(077) SETB (1)                                                  DBC 00179200
         DS    XL11 -         RESERVED                                  00179300
.CC1E    ANOP                                                ICBI DCB-9 00179400
         SPACE 1                                                        00179500
.CC1     ANOP                                                           00179600
         AIF   (NOT &DSORGMQ).CD                                        00179700
&LSW(078) SETB (1)                                                  DBC 00179800
*                       QTAM PROBLEM PROGRAM MESSAGE QUEUE INTERFACE    00179900
         SPACE 1                                                        00180000
         ORG   &P.DCB+52                                                00180100
&P.RECRD DS    A -            NOT USED BY QTAM                          00180200
         AIF   (&DSORGIS OR &DSORGDA).CCA                               00180300
&LSW(079) SETB (1)                                                  DBC 00180400
&P.SYNAD DS    A -            ADDRESS OF USER-PROVIDED SYNAD ROUTINE TO 00180500
*                             BE ENTERED IF A WORK UNIT IS LONGER THAN  00180600
*                             THE WORK AREA PROVIDED FOR INPUT          00180700
         AGO   .CCB                                                     00180800
.CCA     ANOP                                                           00180900
&LSW(080) SETB (1)                                                  DBC 00181000
         DS    A -            DCBSYNAD - ADDRESS OF USER-PROVIDED SYNAD 00181100
*                             ROUTINE TO BE ENTERED IF A WORK UNIT IS   00181200
*                             LONGER THAN THE WORK AREA PROVIDED FOR    00181300
*                             INPUT                                     00181400
.CCB     ANOP                                                           00181500
&P.EOBLK DS    A -            NOT USED BY QTAM                          00181600
         SPACE 1                                                        00181700
.CD      AIF   (NOT &DSORGBX).CDF                                       00181800
&LSW(081) SETB (1)                                                  DBC 00181900
*                       BTAM INTERFACE                                  00182000
         SPACE 1                                                        00182100
         ORG   &P.DCB+52                                                00182200
&P.LERB  DS    A -            ADDRESS OF LINE ERROR BLOCK               00182300
         SPACE 1                                                        00182400
         AIF   (NOT &DEVDBS).CDF                                        00182500
&LSW(082) SETB (1)                                                  DBC 00182600
*                       BSC INTERFACE                                   00182700
         SPACE 1                                                        00182800
         ORG   &P.DCB+56                                                00182900
&P.XMODE DS    BL1 -          MODE OF TRANSMISSION FOR BINARY           00183000
*                             SYNCHRONOUS COMMUNICATION (BSC)           00183100
         AIF   (&#DCBSW(082)).SKP082A                               DBC 00183200
DCBXMIBC EQU   BIT1 -         INTERMEDIATE BLOCK CHECKING IS TO BE      00183300
*                             PERFORMED                                 00183400
DCBXMDA1 EQU   BIT2 -         TRANSMISSION IS THROUGH A 2701 DATA       00183500
*                             ADAPTER UNIT DUAL COMMUNICATION           00183600
*                             INTERFACE B                               00183700
DCBXMDA2 EQU   BIT4 -         TRANSMISSION IS IN CODE B FOR A 2701      00183800
*                             DATA ADAPTER UNIT DUAL CODE FEATURE       00183900
.SKP082A ANOP  ,                                                    DBC 00184000
&P.XCODE DS    BL1 -          BSC CONTROL STATION FLAG AND              00184100
*                             TRANSMISSION CODE                         00184200
         AIF   (&#DCBSW(082)).SKP082B                               DBC 00184300
DCBXCCSF EQU   BIT0 -         BSC CONTROL STATION FLAG ---              00184400
*                             IF ZERO, THIS IS THE CONTROL STATION.     00184500
*                             IF ONE, THIS IS THE REMOTE STATION.       00184600
DCBXCPTP EQU   BIT1 -         IF PTOP IS SPECIFIED IN SYSGEN PROCEDURE  00184700
*                             - SCHEDULE AN ASYNCHRONOUS EXIT TO        00184800
*                             INTERFACE RESOLUTION ROUTINE              00184900
DCBXCTR1 EQU   BIT2 -         6-BIT TRANSCODE IS BEING USED (BIT 4 IS   00185000
*                             ALSO ON)                                  00185100
DCBXCAS1 EQU   BIT3 -         USASCII TRANSMISSION CODE IS BEING USED   00185200
*                             (BIT 5 IS ALSO ON)                        00185300
DCBXCEBC EQU   BIT4+BIT5 -    IF BOTH BITS ARE ZERO, EBCDIC             00185400
*                             TRANSMISSION CODE IS BEING USED.          00185500
DCBXCTR2 EQU   BIT4 -         6-BIT TRANSCODE IS BEING USED (BIT 2 IS   00185600
*                             ALSO ON)                                  00185700
DCBXCAS2 EQU   BIT5 -         USASCII TRANSMISSION CODE IS BEING USED   00185800
*                             (BIT 3 IS ALSO ON)                        00185900
.SKP082B ANOP  ,                                                    DBC 00186000
&P.BSRSV DS    CL1 -          DLE CONTROL CHARACTER                     00186100
&P.BSWBT DS    X -            RESERVED                                  00186200
&P.IRRAD DS    0A -           BEFORE OPEN - IF PTOP IS SPECIFIED IN THE 00186300
*                             SYSGEN PROCEDURE, ADDRESS OF INTERFACE    00186400
*                             RESOLUTION ROUTINE.                       00186500
*                             AFTER OPEN, THE FOLLOWING 4 CHARACTERS    00186600
*                             OCCUPY THIS SPACE.                        00186700
&P.BSTSX DS    CL1 -          DLE CONTROL CHARACTER                     00186800
&P.BSSTX DS    CL1 -          STX CONTROL CHARACTER                     00186900
&P.BSTEX DS    CL1 -          DLE CONTROL CHARACTER                     00187000
&P.BSETX DS    CL1 -          ETX CONTROL CHARACTER                     00187100
&P.BSAK0 DS    CL2 -          ACK-0 CONTROL CHARACTER                   00187200
&P.BSAK1 DS    CL2 -          ACK-1 CONTROL CHARACTER                   00187300
&P.BSENQ DS    CL1 -          ENQ CONTROL CHARACTER                     00187400
&P.BSNAK DS    CL1 -          NAK CONTROL CHARACTER                     00187500
&P.BSETB DS    CL1 -          ETB CONTROL CHARACTER                     00187600
&P.BSDLE DS    CL1 -          DLE CONTROL CHARACTER                     00187700
&P.BSEOT DS    CL1 -          EOT CONTROL CHARACTER                     00187800
&P.BSSYN DS    CL3 -          SYN, SYN, SYN CONTROL CHARACTERS          00187900
&P.BSONL DS    CL2 -          SOH % CONTROL CHARACTERS                  00188000
&P.BSSAK DS    CL2 -          WACK CONTROL CHARACTERS                   00188100
&P.BSRVI DS    CL2 -          DLE @ CONTROL CHARACTERS                  00188200
         DS    XL18 -         RESERVED                                  00188300
         SPACE 1                                                        00188400
.CDF     AIF   (NOT (&DSORGQS OR &DSORGBS)).FIN                         00188500
&LSW(083) SETB (1)                                                  DBC 00188600
*                       QSAM-BSAM-BPAM COMMON INTERFACE                 00188700
         SPACE 1                                                        00188800
         ORG   &P.DCB+52                                                00188900
         AIF   (&DSORGDA).CDA1                                          00189000
&LSW(084) SETB (1)                                                  DBC 00189100
&P.GERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR GET  00189200
&P.PERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT  00189300
&P.CHECK DS    0A -           ADDRESS OF CHECK MODULE                   00189400
         AIF   (&DSORGIS).CDA                                           00189500
&LSW(085) SETB (1)                                                  DBC 00189600
&P.OPTCD DS    BL1 -          OPTION CODES                              00189700
         AGO   .CD2                                                     00189800
.CDA1    ANOP                                                           00189900
&LSW(086) SETB (1)                                                  DBC 00190000
         DS    0A -           DCBGERR, DCBPERR OR DCBCHECK              00190100
.CDA     ANOP                                                           00190200
&LSW(087) SETB (1)                                                  DBC 00190300
         DS    BL1 -          DCBOPTCD - OPTION CODES                   00190400
.CD2     ANOP                                                           00190500
         AIF   (&#DCBSW(083)).SKP083C                               DBC 00190600
         AIF   (&#DCBSW(63) OR &#DCBSW(68)).SKP083A                 DBC 00190700
DCBOPTW  EQU   BIT0 -         WRITE VALIDITY CHECK (DASD)               00190800
*                             (BSAM, BPAM, QSAM, ISAM, BDAM)            00190900
.SKP083A ANOP  ,                                                    DBC 00191000
DCBOPTU  EQU   BIT1 -         ALLOW DATA CHECK CAUSED BY INVALID        00191100
*                             CHARACTER (1403 PRINTER WITH UCS FEATURE) 00191200
*                             (BSAM, BPAM, QSAM)                        00191300
DCBOPTC  EQU   BIT2 -         CHAINED SCHEDULING USING PCI              00191400
*                             (BSAM, BPAM, QSAM)                        00191500
DCBOPTH  EQU   BIT3 -         1287/1288 OPTICAL READER - HOPPER EMPTY   00191600
*                             EXIT (BSAM, BPAM)                         00191700
DCBOPTO  EQU   BIT3 -         1285/1287 OPTICAL READER - ON-LINE        00191800
*                             CORRECTION (QSAM)                         00191900
DCBBCKPT EQU   BIT3 -         CHANNEL-END APPENDAGE IS TO BYPASS DOS    00192000
*                             EMBEDDED CHECKPOINT RECORDS ON TAPE       00192100
*                             (BSAM, QSAM)                       ICB226 00192200
DCBOPTQ  EQU   BIT4 -         TRANSLATION TO OR FROM ASCII              00192300
*                             (BSAM, BPAM, QSAM)                        00192400
         AIF   (&#DCBSW(060)).SKP083B                               DBC 00192500
DCBOPTZ  EQU   BIT5 -         MAGNETIC TAPE DEVICES - USE REDUCED ERROR 00192600
*                             RECOVERY PROCEDURE (EXCP, BSAM, BPAM,     00192700
*                             QSAM)                                     00192800
DCBSRCHD EQU   BIT5 -         USE SEARCH DIRECT, INSTEAD OF SEARCH      00192900
*                             PREVIOUS, ON RECORD POSITION SENSING      00193000
*                             DEVICE  (EXCP, BSAM, BPAM, QSAM)   ICB217 00193100
.SKP083B ANOP  ,                                                    DBC 00193200
DCBOPTT  EQU   BIT6 -         USER TOTALING (BSAM, QSAM)                00193300
.SKP083C ANOP  ,                                                    DBC 00193400
         AIF   (&DSORGDA).CD1                                           00193500
&LSW(088) SETB (1)                                                  DBC 00193600
&P.GERRA DS    0AL3 -         ADDRESS OF SYNCHRONIZING ROUTINE FOR GET  00193700
&P.PERRA DS    0AL3 -         ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT  00193800
&P.CHCKA DS    AL3 -          ADDRESS OF CHECK MODULE                   00193900
         AGO   .CD3                                                     00194000
.CD1     ANOP                                                           00194100
&LSW(089) SETB (1)                                                  DBC 00194200
         DS    AL3 -          DCBGERRA, DCBPERRA OR DCBCHCKA            00194300
.CD3     AIF   (&DSORGIS OR &DSORGDA OR &DSORGMQ).CDB                   00194400
&LSW(090) SETB (1)                                                  DBC 00194500
&P.SYNAD DS    0A -           ADDRESS OF USER-PROVIDED SYNAD ROUTINE    00194600
&P.IOBL  DS    FL1 -          IOB LENGTH IN DOUBLE WORDS                00194700
&P.SYNA  DS    AL3 -          ADDRESS OF USER-PROVIDED SYNAD ROUTINE    00194800
         AGO   .CD4                                                     00194900
.CDB     ANOP                                                           00195000
&LSW(091) SETB (1)                                                  DBC 00195100
         DS    0A -           DCBSYNAD - ADDRESS OF SYNAD ROUTINE       00195200
&P.IOBL  DS    FL1 -          IOB LENGTH IN DOUBLE WORDS                00195300
         DS    AL3 -          DCBSYNA - ADDRESS OF SYNAD ROUTINE        00195400
.CD4     ANOP                                                           00195500
&P.FLAG1 DS    0BL1           TCAM APPLICATION PROGRAM FLAGS ICBI DCB-3 00195600
*                             (BSAM, BPAM, QSAM)                        00195700
&P.CIND1 DS    BL1 -          CONDITION INDICATORS                      00195800
         AIF   (&#DCBSW(083)).SKP083D                               DBC 00195900
DCBCNTOV EQU   BIT0 -         DIRECT ACCESS - TRACK OVERFLOW IN USE     00196000
*                             (BSAM, BPAM, QSAM)                        00196100
*                             2540 CARD PUNCH - DATA SET WAS OPENED BUT 00196200
*                             NO DATA WAS WRITTEN (QSAM)                00196300
DCBSTQCK EQU   BIT0 -         STOP EQUAL QUICK WAS SPECIFIED FOR        00196400
*                             APPLICATION PROG. DCBS (TCAM)  ICBI DCB-3 00196500
DCBSTFLS EQU   BIT1 -         STOP EQUAL FLUSH WAS SPECIFIED FOR        00196600
*                             APPLICATION PROG. DCBS (TCAM)  ICBI DCB-3 00196700
DCBCNSRD EQU   BIT1 -         SEARCH DIRECT (BSAM, BPAM, QSAM)          00196800
DCBCNEVB EQU   BIT2 -         END OF VOLUME - USED BY EOB ROUTINES      00196900
*                             (BSAM, BPAM, QSAM)                        00197000
DCBCNEVA EQU   BIT3 -         END OF VOLUME - USED BY CHANNEL-END       00197100
*                             APPENDAGE ROUTINES (BSAM, BPAM, QSAM)     00197200
DCBCNBRM EQU   BIT5 -         BLOCKED RECORD BIT MODIFIED (BSAM,BPAM,   00197300
*                             QSAM)                          ICBI DCB-2 00197400
DCBCNEXB EQU   BIT7 -         EXCHANGE BUFFERING SUPPORTED (QSAM)       00197500
.SKP083D ANOP  ,                                                    DBC 00197600
&P.CIND2 DS    BL1 -          CONDITION INDICATORS                      00197700
         AIF   (&#DCBSW(083)).SKP083E                               DBC 00197800
DCBCNSTO EQU   BIT0 -         PARTITIONED DATA SET - STOW HAS BEEN      00197900
*                             PERFORMED (BSAM, BPAM, QSAM)              00198000
*                             SEQUENTIAL DATA SET - UPDATE (BSAM, BPAM) 00198100
DCBCNWR0 EQU   BIT1 -         DIRECT ORGANIZATION DATA SET - LAST I/O   00198200
*                             WAS A WRITE RECORD ZERO                   00198300
*                             (BSAM, BPAM, QSAM)                        00198400
*                             SEQUENTIAL DATA SET - UPDATE EOF IS       00198500
*                             INDICATED (BSAM, BPAM)                    00198600
DCBCNCLO EQU   BIT2 -         CLOSE IN PROCESS (QSAM)                   00198700
DCBCNIOE EQU   BIT3 -         PERMANENT I/O ERROR (BSAM, BPAM, QSAM)    00198800
DCBCNBFP EQU   BIT4 -         OPEN ACQUIRED BUFFER POOL                 00198900
*                             (BSAM, BPAM, QSAM)                        00199000
DCBCNCHS EQU   BIT5 -         CHAINED SCHEDULING BEING SUPPORTED        00199100
*                             (BSAM, BPAM, QSAM)                        00199200
DCBCNFEO EQU   BIT6 -         FEOV BIT (BSAM, BPAM, QSAM)               00199300
DCBCNQSM EQU   BIT7 -         ALWAYS ZERO (BSAM, BPAM)                  00199400
*                             THIS IS A QSAM DCB (QSAM)                 00199500
.SKP083E ANOP  ,                                                    DBC 00199600
         AIF   (&DSORGIS OR &DSORGDA).CDC                               00199700
&LSW(092) SETB (1)                                                  DBC 00199800
&P.BLKSI DS    H -            MAXIMUM BLOCK SIZE                        00199900
         AGO   .CD7                                                     00200000
.CDC     ANOP                                                           00200100
&LSW(093) SETB (1)                                                  DBC 00200200
         DS    H -            DCBBLKSI - MAXIMUM BLOCK SIZE             00200300
.CD7     ANOP                                                           00200400
&P.WCPO  DS    AL1 -          OFFSET OF WRITE CHANNEL PROGRAM FROM THE  00200500
*                             START OF IOB                              00200600
&P.WCPL  DS    FL1 -          LENGTH OF WRITE CHANNEL PROGRAM           00200700
&P.OFFSR DS    AL1 -          OFFSET OF READ CCW FROM BSAM/BPAM PREFIX  00200800
*                             OF IOB                                    00200900
&P.OFFSW DS    AL1 -          OFFSET OF WRITE CCW FROM BSAM/BPAM PREFIX 00201000
*                             OF IOB                                    00201100
&P.IOBA  DS    A -            FOR NORMAL SCHEDULING, ADDRESS OF QSAM OR 00201200
*                             BSAM/BPAM PREFIX OF IOB.  FOR CHAINED     00201300
*                             SCHEDULING, ADDRESS OF ICB.  FOR          00201400
*                             1419/1275, ADDRESS OF MAGNETIC INTERRUPT  00201500
*                             CONTROL BLOCK (MICB) CURRENTLY BEING      00201600
*                             PROCESSED BY READ ROUTINE.  FOR TSO       00201700
*                             TERMINAL DATA SET OPENED FOR INPUT AND    00201800
*                             FORMAT U, SIMULATED LOW-ORDER FOUR BYTES  00201900
*                             OF IOBCSW                                 00202000
         SPACE 1                                                        00202100
         ORG   &P.DCB+68                                         ICB354 00202200
&P.CICB  DS    0A -           SAME AS DCBCICBA BELOW             ICB354 00202300
         DS    X -            DCBNCP  (BSAM,BPAM)                ICB354 00202400
&P.CICBA DS    AL3 -          POINTER TO JES C.I.                ICB354 00202500
*                             CONTROL BLOCK (CICB)               ICB354 00202600
         SPACE 1                                                        00202700
         ORG   &P.DCB+80                                     ICBI DCB-4 00202800
&P.DIRCT DS    0H -           NUMBER OF BYTES USED IN LAST DIRECTORY    00202900
*                             BLOCK (RANGE 0-254)  (BSAM, BPAM)  ICB295 00203000
&P.QSWS  DS    0BL1 -         FLAG BYTE                          ICB295 00203100
&P.USASI DS    B -            FLAG BYTE FOR ASCII TAPES                 00203200
         AIF   (&#DCBSW(083)).SKP083F                               DBC 00203300
DCBBLBP  EQU   BIT1 -         BLOCK PREFIX IS FOUR BYTE FIELD           00203400
*                             CONTAINING BLOCK LENGTH IN UNPACKED       00203500
*                             DECIMAL (SPECIFIED BY BUFFER=L).          00203600
DCBQADFS EQU   BIT2+BIT3+BIT4 USED TO PERFORM SEQUENCE CHECKING WITH    00203700
*                             MULTIPLE FUNCTION SUPPORT FOR 3525        00203800
*                             (BSAM, QSAM)                              00203900
DCBQADF1 EQU   BIT2 -         FIRST BIT OF DCBQADFS                     00204000
DCBQADF2 EQU   BIT3 -         SECOND BIT OF DCBQADFS                    00204100
DCBQADF3 EQU   BIT4 -         THIRD BIT OF DCBQADFS                     00204200
DCBQSTRU EQU   BIT7 -         TRUNC ENTRY POINT ENTERED (QSAM)          00204300
.SKP083F ANOP  ,                                                    DBC 00204400
&P.BUFOF DS    0FL1 -         BLOCK PREFIX LENGTH (0-99), SPECIFIED BY  00204500
*                             BUFOFF=N OR BUFOFF=L                      00204600
&P.DIRCQ DS    FL1 -          NUMBER OF BYTES USED IN LAST DIRECTORY    00204700
*                             BLOCK (RANGE 0-254)  (QSAM)        ICB295 00204800
         SPACE 1                                                        00204900
         AIF   (NOT &DSORGBS).CE                                        00205000
&LSW(094) SETB (1)                                                  DBC 00205100
*                       BSAM-BPAM INTERFACE                             00205200
         SPACE 1                                                        00205300
         ORG   &P.DCB+72                                                00205400
&P.EOBR  DS    0A -           ADDRESS OF END-OF-BLOCK MODULE FOR READ   00205500
         AIF   (&DSORGIS).CDD                                           00205600
&LSW(095) SETB (1)                                                  DBC 00205700
&P.NCP   DS    FL1 -          NUMBER OF CHANNEL PROGRAMS.               00205800
         AGO   .CD8                                                     00205900
.CDD     ANOP                                                           00206000
&LSW(096) SETB (1)                                                  DBC 00206100
         DS    FL1 -          DCBNCP - NUMBER OF CHANNEL PROGRAMS.      00206200
.CD8     ANOP                                                           00206300
*                             NUMBER OF READ OR WRITE REQUESTS WHICH    00206400
*                             MAY BE ISSUED PRIOR TO A CHECK, NUMBER    00206500
*                             OF IOB'S GENERATED.  (99 MAXIMUM)         00206600
&P.EOBRA DS    AL3 -          ADDRESS OF END-OF-BLOCK MODULE FOR READ   00206700
&P.EOBW  DS    A -            ADDRESS OF END-OF-BLOCK MODULE FOR WRITE. 00206800
*                             FOR BSAM CREATE BDAM PROCESSING OF        00206900
*                             UNBLOCKED SPANNED RECORDS WITH BKTEK=R    00207000
*                             SPECIFIED, ADDRESS OF SEGMENT WORK AREA   00207100
*                             CONTROL BLOCK                             00207200
         DS    H -            DCBDIRCT - NUMBER OF BYTES USED IN LAST   00207300
*                             DIRECTORY BLOCK  (RANGE 0-254)     ICB295 00207400
         AIF   (&DSORGIS).CDE                                           00207500
&LSW(097) SETB (1)                                                  DBC 00207600
&P.LRECL DS    H -            LOGICAL RECORD LENGTH                     00207700
         AGO   .CD9                                                     00207800
.CDE     ANOP                                                           00207900
&LSW(098) SETB (1)                                                  DBC 00208000
         DS    H -            DCBLRECL - LOGICAL RECORD LENGTH          00208100
.CD9     ANOP                                                           00208200
&P.CNTRL DS    0A -           ADDRESS OF CNTRL MODULE                   00208300
&P.NOTE  DS    0A -           ADDRESS OF NOTE/POINT MODULE              00208400
&P.POINT DS    A -            ADDRESS OF NOTE/POINT MODULE              00208500
         SPACE 1                                                        00208600
.CE      AIF   (NOT &DSORGQS).FIN                                       00208700
&LSW(099) SETB (1)                                                  DBC 00208800
*                       QSAM INTERFACE                                  00208900
         SPACE 1                                                        00209000
         AIF   (NOT &DSORGDA).CE1                                       00209100
&LSW(100) SETB (1)                                                  DBC 00209200
         ORG   &P.DCB+52                                                00209300
&P.GERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR GET  00209400
&P.PERR  DS    0A -           ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT  00209500
         DS    BL1 -          DCBOPTCD - OPTION CODES                   00209600
&P.GERRA DS    0AL3 -         ADDRESS OF SYNCHRONIZING ROUTINE FOR GET  00209700
&P.PERRA DS    AL3 -          ADDRESS OF SYNCHRONIZING ROUTINE FOR PUT  00209800
         SPACE 1                                                        00209900
.CE1     ANOP                                                           00210000
         ORG   &P.DCB+72                                                00210100
&P.LCCW  DS    0A -           FOR EXCHANGE BUFFERING, ADDRESS OF LAST   00210200
*                             CCW IN LIST                               00210300
&P.EOBAD DS    A -            FOR SIMPLE BUFFERING, ADDRESS OF LAST     00210400
*                             BYTE OF CURRENT BUFFER                    00210500
&P.CCCW  DS    0A -           FOR EXCHANGE BUFFERING, ADDRESS OF        00210600
*                             CURRENT OR NEXT CCW                       00210700
&P.RECAD DS    0A -           ADDRESS OF CURRENT OR NEXT LOGICAL RECORD 00210800
&P.RECBT DS    BL1 -          FLAG BYTE                                 00210900
         AIF   (&#DCBSW(099)).SKP099A                               DBC 00211000
DCBRCREL EQU   BIT0+BIT1+BIT2+BIT3 RELSE MACRO HAS BEEN ISSUED          00211100
*                             (QSAM WITH SIMPLE BUFFERING)              00211200
DCBRCTRU EQU   BIT0 -         TRUNC MACRO HAS BEEN ISSUED (QSAM LOCATE  00211300
*                             MODE)                                     00211400
DCBRCFGT EQU   BIT1 -         FIRST GET AFTER OPEN (QSAM LOCATE MODE)   00211500
.SKP099A ANOP  ,                                                    DBC 00211600
&P.RECA  DS    AL3 -          ADDRESS OF CURRENT OR NEXT LOGICAL RECORD 00211700
         DS    B -            DCBQSWS - FLAG BYTE                ICB295 00211800
         DS    FL1 -          DCBDIRCQ - NUMBER OF BYTES USED IN LAST   00211900
*                             DIRECTORY BLOCK (RANGE 0-254)      ICB295 00212000
         AIF   (&DSORGIS OR &DSORGBS).CEE                               00212100
&LSW(101) SETB (1)                                                  DBC 00212200
&P.LRECL DS    H -            LOGICAL RECORD LENGTH                     00212300
         AGO   .CEEA                                                    00212400
.CEE     ANOP                                                           00212500
&LSW(102) SETB (1)                                                  DBC 00212600
         DS    H -            DCBLRECL - LOGICAL RECORD LENGTH          00212700
.CEEA    AIF   (&DSORGBS).CEF                                           00212800
&LSW(103) SETB (1)                                                  DBC 00212900
&P.CNTRL DS    0A -           ADDRESS OF CNTRL MODULE                   00213000
         AGO   .CEF1                                                    00213100
.CEF     ANOP                                                           00213200
&LSW(104) SETB (1)                                                  DBC 00213300
         DS    0A -           DCBCNTRL - ADDRESS OF CNTRL MODULE        00213400
.CEF1    ANOP                                                           00213500
&P.EROPT DS    BL1 -          ERROR OPTION                              00213600
         AIF   (&#DCBSW(099)).SKP099B                               DBC 00213700
DCBERACC EQU   BIT0 -         ACCEPT PERMANENT ERROR                    00213800
DCBERSKP EQU   BIT1 -         SKIP PERMANENT ERROR                      00213900
DCBERABE EQU   BIT2 -         ABNORMAL END OF TASK                      00214000
.SKP099B ANOP  ,                                                    DBC 00214100
         AIF   (&DSORGBS).CEF2                                          00214200
&LSW(105) SETB (1)                                                  DBC 00214300
&P.CNTRA DS    AL3 -          ADDRESS OF CNTRL MODULE                   00214400
         AGO   .CEF3                                                    00214500
.CEF2    ANOP                                                           00214600
&LSW(106) SETB (1)                                                  DBC 00214700
         DS    AL3 -          DCBCNTRA - ADDRESS OF CNTRL MODULE        00214800
.CEF3    ANOP                                                           00214900
         DS    XL2 -          RESERVED                                  00215000
&P.PRECL DS    H -            BLOCK LENGTH, MAXIMUM BLOCK LENGTH OR     00215100
*                             DATA LENGTH                               00215200
&P.EOB   DS    A -            ADDRESS OF END OF BLOCK MODULE            00215300
         SPACE 1                                                        00215400
.CF      AIF   (&DSORGIS OR &DSORGQS OR &DSORGBS OR NOT &DSORGLR).FIN   00215500
&LSW(107) SETB (1)                                                  DBC 00215600
         ORG   &P.DCB+82                                                00215700
&P.LRECL DS    H -            LOGICAL RECORD LENGTH                     00215800
         SPACE 1                                                        00215900
         AGO   .FIN                                                     00216000
.SLIP    ANOP                                                           00216100
&LSW(108) SETB (1)                                                  DBC 00216200
*                       GRAPHIC DEVICE INTERFACE                        00216300
         SPACE 1                                                        00216400
&P.DCB   DS    0A
         DS    XL12 -         RESERVED                                  00216600
&P.BRSA  DS    AL2 -          BUFFER RESTART ADDRESS.  BLANK BEFORE     00216700
*                             EXECUTION OF SECOND I/O OPERATION         00216800
&P.GTYPE DS    CL1 -          TYPE OF BUFFER MANAGEMENT AND ATTENTION   00216900
*                             HANDLING                                  00217000
         AIF   (&#DCBSW(108)).SKP108A                               DBC 00217100
DCBGTEXP EQU   X'00' -        EXPRESS                                   00217200
DCBGTBAS EQU   X'01' -        BASIC                                     00217300
.SKP108A ANOP  ,                                                    DBC 00217400
         DS    X -            RESERVED                                  00217500
&P.BFRST DS    AL2 -          BLANK BEFORE EXECUTION OF OPEN ROUTINE.   00217600
*                             STARTING ADDRESS FOR BUFFER AFTER         00217700
*                             EXECUTION OF OPEN ROUTINE                 00217800
&P.BFRSZ DS    H -            BLANK BEFORE EXECUTION OF OPEN ROUTINE.   00217900
*                             SIZE OF BUFFER AFTER EXECUTION OF OPEN    00218000
*                             ROUTINE.                                  00218100
         SPACE 1                                                        00218200
*                       COMMON INTERFACE                                00218300
         SPACE 1                                                        00218400
         DS    XL6 -          RESERVED                                  00218500
&P       #DSORG                                                     DBC 00218600
&P.IOBAD DS    A -            BLANK BEFORE EXECUTION OF OPEN ROUTINE.   00218700
*                             ADDRESS OF STANDARD FIELDS OF FIRST IOB   00218800
*                             AFTER EXECUTION OF OPEN ROUTINE           00218900
         SPACE 1                                                        00219000
*                       FOUNDATION EXTENSION                            00219100
         SPACE 1                                                        00219200
&P.POLST DS    0A -           ADDRESS OF AREA WHERE A DCB LIST IS TO BE 00219300
*                             CONSTRUCTED FOR POLLING PURPOSES          00219400
&P.GNCP  DS    FL1 -          NUMBER OF I/O INSTRUCTIONS TO BE ISSUED   00219500
*                             BEFORE A WAIT MACRO INSTRUCTION           00219600
&P.POLSA DS    AL3 -          SAME AS DCBPOLST ABOVE                    00219700
&P.EXLST DS    0A -           ADDRESS OF USER'S EXIT LIST               00219800
         DS    X -            RESERVED                                  00219900
&P.EXLSA DS    AL3 -          ADDRESS OF USER'S EXIT LIST               00220000
         SPACE 1                                                        00220100
*                       FOUNDATION BEFORE OPEN                          00220200
         SPACE 1                                                        00220300
&P.DDNAM DS    CL8 -          8-BYTE NAME FROM DD STATEMENT THAT        00220400
*                             DEFINES DATA SET ASSOCIATED WITH THIS DCB 00220500
&P.OFLG  DS    BL1 -          FLAGS USED BY OPEN ROUTINE                00220600
         AIF   (&#DCBSW(108)).SKP108B                               DBC 00220700
DCBOFGRW EQU   BIT0 -         IF ZERO, LAST I/O OPERATION WAS GREAD.    00220800
*                             IF ONE, LAST I/O OPERATION WAS GWRITE.    00220900
         AIF   (&#DCBSW(052)).SKP108B                               DBC 00221000
DCBOFEOV EQU   BIT2 -         SET TO 1 BY EOV WHEN IT CALLS CLOSE       00221100
*                             ROUTINE FOR CONCATENATION OF DATA SETS    00221200
*                             WITH UNLIKE ATTRIBUTES                    00221300
DCBOFOPN EQU   BIT3 -         AN OPEN HAS BEEN SUCCESSFULLY COMPLETED   00221400
DCBOFPPC EQU   BIT4 -         SET TO 1 BY PROBLEM PROGRAM TO INDICATE A 00221500
*                             CONCATENATION OF UNLIKE ATTRIBUTES        00221600
DCBOFTM  EQU   BIT5 -         TAPE MARK HAS BEEN READ                   00221700
DCBOFUEX EQU   BIT6 -         SET TO 0 BY AN I/O SUPPORT FUNCTION WHEN  00221800
*                             THAT FUNCTION TAKES A USER EXIT. SET TO 1 00221900
*                             ON RETURN FROM USER EXIT TO THE I/O       00222000
*                             SUPPORT FUNCTION WHICH TOOK THE EXIT.     00222100
DCBOFIOF EQU   BIT7 -         SET TO 1 BY AN I/O SUPPORT FUNCTION IF    00222200
*                             DCB IS TO BE PROCESSED BY THAT FUNCTION   00222300
.SKP108B ANOP  ,                                                    DBC 00222400
&P.IFLG  DS    BL1 -          SET TO ZERO BY GRAPHIC ROUTINES BUT USED  00222500
*                             BY IOS IN COMMUNICATING ERROR CONDITIONS  00222600
*                             AND IN DETERMINING CORRECTIVE PROCEDURES  00222700
&P.MACR  DS    0BL2 -         MACRO INSTRUCTION REFERENCE               00222800
&P.MACR1 DS    BL1 -          FIRST BYTE OF DCBMACR                     00222900
         AIF   (&#DCBSW(52) OR &#DCBSW(108)).SKP108C                DBC 00223000
DCBMRRD  EQU   BIT2 -         READ                                      00223100
DCBMRCRL EQU   BIT6 -         CNTRL                                     00223200
.SKP108C ANOP  ,                                                    DBC 00223300
&P.MACR2 DS    BL1 -          SECOND BYTE OF DCBMACR                    00223400
         AIF   (&#DCBSW(52) OR &#DCBSW(108)).SKP108D                DBC 00223500
DCBMRWRT EQU   BIT2 -         WRITE                                     00223600
DCBMRCTL EQU   BIT6 -         CNTRL                                     00223700
.SKP108D ANOP  ,                                                    DBC 00223800
         SPACE 1                                                        00223900
*                       FOUNDATION AFTER OPEN                           00224000
         SPACE 1                                                        00224100
         ORG   &P.DCB+40                                                00224200
&P.TIOT  DS    AL2 -          OFFSET FROM TIOT ORIGIN TO DD ENTRY       00224300
*                             ASSOCIATED WITH THIS DCB                  00224400
&P.MACRF DS    0BL2 -         SAME AS DCBMACR BEFORE OPEN               00224500
&P.MACF1 DS    BL1 -          FIRST BYTE OF DCBMACRF                    00224600
&P.MACF2 DS    BL1 -          SECOND BYTE OF DCBMACRF                   00224700
&P.DEBAD DS    0A -           ADDRESS OF ASSOCIATED DEB                 00224800
&P.IFLGS DS    BL1 -          SAME AS DCBIFLG BEFORE OPEN               00224900
&P.DEBA  DS    AL3 -          ADDRESS OF ASSOCIATED DEB                 00225000
&P.GIOCR DS    0A -           ADDRESS OF GRAPHICS I/O CONTROL ROUTINE   00225100
&P.OFLGS DS    BL1 -          SAME AS DCBOFLG BEFORE OPEN               00225200
&P.GIOCA DS    AL3 -          ADDRESS OF GRAPHICS I/O CONTROL ROUTINE   00225300
         SPACE 1                                                        00225400
.FIN     ANOP                                                           00225500
&A0      SETA  0                                                    DBC 00225600
.LP1     AIF   (&A0 EQ 150).EXIT                                    DBC 00225700
&A0      SETA  &A0+1                                                DBC 00225800
&#DCBSW(&A0) SETB (&#DCBSW(&A0) OR &LSW(&A0))                       DBC 00225900
         AGO   .LP1                                                 DBC 00226000
.EXIT    ANOP  ,                                                    DBC 00226100
         MEND                                                           00226200
         MACRO                                                          00000100
&N       #DSORG &D                                                      00000200
.*                                                                      00000300
.*                                                                      00000400
.*                                                                      00000500
.* LAST CHANGE DATE - APRIL 21, 1981                                    00000600
.*                  - MACRO NAME CHANGED FROM $DSORG TO #DSORG.         00000700
.*                                                                      00000800
.* LAST CHANGE DATE - FEBRUARY 2, 1977                                  00000900
.*                  - MAILING ADDRESS CHANGE.                           00001000
.*                                                                      00001100
.* LAST CHANGE DATE - APRIL 1, 1975                                     00001200
.*                                                                      00001300
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING    00001400
.* IT MAY BE ADDRESSED TO:                                              00001500
.*       809 WHITNEY AVE.                                               00001600
.*       NEW HAVEN, CT. 06511                                           00001700
.*                                                                      00001800
.*                                                                      00001900
.*                                                                      00002000
.*   THIS MACRO WAS WRITTEN TO BE AN INNER MACRO FOR THE #DCBD MACRO.   00002100
.* ITS SOLE PURPOSE IS TO GENERATE DSORG FIELD AND BIT NAMES FOR THE    00002200
.* #DCBD MACRO.                                                         00002300
.*                                                                      00002400
.*                                                                      00002500
.*                                                                      00002600
.* INNER MACROS USED - NONE                                             00002700
.*                                                                      00002800
         GBLB  &#DCBDSG                                                 00002900
         GBLB  &#DCBSW(150)                                             00003000
         LCLC  &P                                                       00003100
&P       SETC  'DCB'                                                    00003200
         AIF   ('&N' EQ '').GOTPFIX                                     00003300
&P       SETC  '&N'                                                     00003400
.GOTPFIX ANOP                                                           00003500
&P.DSORG DS    0BL2 -         DATA SET ORGANIZATION BEING USED          00003600
&P.DSRG1 DS    BL1 -          FIRST BYTE OF DCBDSORG                    00003700
         AIF   (&#DCBDSG).SKP1                                          00003800
DCBDSGIS EQU   BIT0 -         IS - INDEXED SEQUENTIAL ORGANIZATION      00003900
DCBDSGPS EQU   BIT1 -         PS - SHYSICAL SEQUENTIAL ORGANIZATION     00004000
DCBDSGDA EQU   BIT2 -         DA - DIRECT ORGANIZATION                  00004100
DCBDSGCX EQU   BIT3 -         CX - BTAM OR QTAM LINE GROUP              00004200
DCBDSGCQ EQU   BIT4 -         CQ - QTAM DIRECT ACCESS MESSAGE QUEUE     00004300
DCBDSGMQ EQU   BIT5 -         MQ - QTAM PROBLEM PROGRAM MESSAGE QUEUE   00004400
DCBDSGPO EQU   BIT6 -         PO - PARTITIONED ORGANIZATION             00004500
DCBDSGU  EQU   BIT7 -         U  - UNMOVABLE, THE DATA CONTAINS         00004600
*                                  LOCATION DEPENDENT INFORMATION       00004700
.SKP1    ANOP  ,                                                        00004800
&P.DSRG2 DS    BL1 -          SECOND BYTE OF DCBDSORG                   00004900
         AIF   (&#DCBDSG).SKP2                                          00005000
DCBDSGGS EQU   BIT0 -         GS - GRAPHICS ORGANIZATION                00005100
DCBDSGTX EQU   BIT1 -         TX - TCAM LINE GROUP                      00005200
DCBDSGTQ EQU   BIT2 -         TQ - TCAM MESSAGE QUEUE                   00005300
DCBACBM  EQU   BIT4 -         ACCESS METHOD CONTROL BLOCK   ICBI DCB-1  00005400
         AIF   (&#DCBSW(44)).SKP2                                       00005500
&#DCBSW(44) SETB (1)                                                    00005600
DCBDSGTR EQU   BIT5 -         TR - TCAM 3705                    S22024  00005700
.SKP2    ANOP                                                           00005800
&#DCBDSG SETB  (1)                                                      00005900
         MEND                                                           00006000
         MACRO                                                          00000100
&N       #TEST &DCODE=OMITTED,&MEXCL=,&NUM=OMITTED,                    *00000200
               &PFIX=OMITTED,&REGS=,&SIZE=,&GEN=                        00000300
.*                                                                      00000400
.*                                                                      00000500
.*                                                                      00000600
.* LAST CHANGE DATE - OCTOBER 18, 1983                                  00000700
.*                  - MAILING ADDRESS CHANGE                            00000800
.*                                                                      00000900
.* LAST CHANGE DATE - MARCH 16, 1983                                    00001000
.*                  - COMMENTARY CHANGES                                00001100
.*                  - MAILING ADDRESS CHANGE                            00001200
.*                                                                      00001300
.* LAST CHANGE DATE - DECEMBER 4, 1981                                  00001400
.*                  - THE "SIZE=" FUNCTION HAS BEEN ENHANCED            00001500
.*                    TO INCLUDE SUPPORT FOR THE "NE"                   00001600
.*                    RELATION.                                         00001700
.*                                                                      00001800
.* LAST CHANGE DATE - APRIL 21, 1981                                    00001900
.*                  - MACRO NAME CHANGED FROM $TEST TO #TEST            00002000
.*                                                                      00002100
.* LAST CHANGE DATE - APRIL 15, 1981                                    00002200
.*                  - THE "NUM" FUNCTION NO LONGER ISSUES AN            00002300
.*                    ERROR MESSAGE WHEN IT ENCOUNTERS A                00002400
.*                    NON-DIGIT. IT ONLY SETS A RETURN CODE             00002500
.*                    OF 16 IN &#TESERR.                                00002600
.*                                                                      00002700
.* LAST CHANGE DATE - APRIL 24, 1978                                    00002800
.*                    THE GEN=EBCDIC FUNCTION HAS BEEN ADDED            00002900
.*                                                                      00003000
.* LAST CHANGE DATE - FEBRUARY 2, 1977                                  00003100
.*                  - MAILING ADDRESS CHANGE.                           00003200
.*                                                                      00003300
.* LAST CHANGE DATE - SEPTEMBER 10, 1976                                00003400
.*                    THE &SIZE= OPERAND SUPPORT IS CHANGED             00003500
.*                    TO REQUIRE THREE SUB-OPERANDS WITH THE            00003600
.*                    SECOND SPECIFYING ONE OF THE RELATION             00003700
.*                    OPERATIONS: LT, LE, EQ, GE, OR GT.                00003800
.*                    NOTE, THIS IS NOT COMPATIBLE WITH THE             00003900
.*                    PREVIOUS IMPLEMENTATION.                          00004000
.*                                                                      00004100
.* LAST CHANGE DATE - FEBRUARY 10, 1976                                 00004200
.*                                                                      00004300
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE.  ANY QUESTIONS              00004400
.* CONCERNING IT MAY BE ADDRESSED TO:                                   00004500
.*       RR#2 BOX 712                                                   00004600
.*       AFTON, VA. 22920                                               00004700
.*                                                                      00004800
.*                                                                      00004900
.*                                                                      00005000
.*                                 GENERAL INFORMATION                  00005100
.*   THE #TEST MACRO IS INTENDED TO BE USED AS AN INNER                 00005200
.* MACRO. IT PERFORMS A NUMBER OF DIFFERENT TESTS AND                   00005300
.* MANIPULATIONS WHICH ARE WHOLELY INDEPENDANT OF EACH OTHER.           00005400
.* FOR EACH OF THESE FUNCTIONS, INPUT MIGHT BE SPECIFIED                00005500
.* EITHER VIA MACRO OPERANDS OR BOTH MACRO OPERANDS AND GLOBAL          00005600
.* SYMBOLS (DEPENDING UPON THE FUNCTION). OUTPUT IS USUALLY             00005700
.* COMMUNICATED VIA THE GLOBAL SYMBOLS &#TESERR AND &#TESRET.           00005800
.*   &#TESERR IS A SCALER SETA SYMBOL WHICH IS USED IN A                00005900
.* MANNER SIMILAR TO A PROGRAM'S COMPLETION CODE TO                     00006000
.* COMMUNICATE A GROSS INDICATION OF AN UNUSUAL OR ERROR                00006100
.* CONDITION. IF UPON RETURN FROM #TEST &#TESERR EQUALS ZERO,           00006200
.* THEN THE MACRO FUNCTIONED "OK"; OTHERWISE, THE VALUE OF              00006300
.* &#TESERR VARIES DIRECTLY WITH THE SERIOUSNESS OF THE                 00006400
.* UNUSUAL OR ERROR CONDITION, AND IT IS ALWAYS SET TO                  00006500
.* REFLECT THE MOST SERIOUS CONDITION ENCOUNTERED DURING A              00006600
.* PARTICULAR INVOCATION OF THE #TEST MACRO.                            00006700
.*   &#TESRET IS A SETC ARRAY WHICH IS USED TO CONTAIN                  00006800
.* RETURN VALUES FOR THOSE FUNCTIONS FOR WHICH RETURN                   00006900
.* VALUES ARE APPROPIATE. EACH ELEMENT OF THE ARRAY HOLDS ONE           00007000
.* RETURN VALUE. ONLY AS MANY ELEMENTS ARE USED AS ARE                  00007100
.* NEEDED. THOSE ELEMENTS USED ALWAYS START WITH ELEMENT                00007200
.* NUMBER ONE. IF TWO OR MORE TEST FUNCTIONS ARE INVOKED ON             00007300
.* A SINGLE CALL AND IF EACH OF THEM GENERATE ONE OR MORE               00007400
.* RETURN VALUES, THEN THE FIRST FUNCTION PROCESSED WILL USE            00007500
.* THE LOW ORDER ENTRIES IN &#TESRET. THE NEXT FUNCTION WILL            00007600
.* USE THE NEXT ENTRIES, ETC. THE VARIOUS TEST FUNCTIONS                00007700
.* WILL ALWAYS BE PROCESSED IN THE SAME ORDER WITH WHICH                00007800
.* THEY APPEAR BELOW.                                                   00007900
.*                                                                      00008000
.*                                                                      00008100
.*                                                                      00008200
.* INNER MACROS USED - NONE                                             00008300
.*                                                                      00008400
         GBLA  &#REGVAL(255),&#TESERR                                   00008500
         GBLC  &#REGNME(255),&#TESRET(20)                               00008600
         GBLC  &#EBCDIC(256)                                            00008700
         LCLA  &CTR,&RETPTR,&P,&L,&K,&A1,&A2,&RV(22)                    00008800
         LCLB  &MESW                                                    00008900
         LCLC  &RN(22),&BASE                                            00009000
&#TESERR SETA  0                                                        00009100
.*                                                                      00009200
.*                                                                      00009300
.*                                                                      00009400
.*                                 THE "DCODE" FUNCTION                 00009500
.*   THIS FUNCTION WAS WRITTEN BECAUSE THE MACRO LANGUAGE               00009600
.* DOES NOT SUPPORT THE DECODING OF "SUB-SUB-LIST" NOTATION.            00009700
.*   THE INPUT IS COMMUNICATED VIA THE "DCODE=" OPERAND AS A            00009800
.* CHARACTER STRING TO BE DECODED. THIS STRING MUST BE                  00009900
.* EITHER NULL, UNPARENTHESIZED, OR A PARENTHESIZED LIST OF             00010000
.* ELEMENTS SEPERATED FROM EACH OTHER BY COMMAS. THE                    00010100
.* ELEMENTS THEMSELVES MAY BE NULL.                                     00010200
.*   FOR A STRING OF N ELEMENTS, THE OUTPUT CONSISTS OF N+1             00010300
.* ENTRIES IN &#TESRET. THE FIRST ENTRY CONTAINS THE VALUE              00010400
.* N. THE REMAINING ENTRIES CONTAIN EACH OF THE ELEMENTS                00010500
.* EXTRACTED FROM THE ORIGINAL STRING. IF THE ORIGINAL                  00010600
.* STRING IS NULL, THEN IT IS TREATED AS A SUB-LIST                     00010700
.* CONTAINING ZERO ELEMENTS. IF THE STRING IS                           00010800
.* UNPARENTHESIZED, THEN IT IS TREATED AS A SUB-LIST                    00010900
.* CONTAINING A SINGLE ELEMENT - NAMELY, ITSELF.                        00011000
.*   THE DCODE FUNCTION WILL NOT PROPERLY HANDLE THE                    00011100
.* FOLLOWING CONDITIONS:                                                00011200
.*       A.) A SUB-LIST ELEMENT LONGER THAN EIGHT CHARACTERS;           00011300
.*       B.) A SUB-LIST ELEMENT THAT ITSELF CONSISTS OF A               00011400
.*           SUB-LIST;                                                  00011500
.*       C.) A SUB-LIST CONTAINING MORE THAN NINETEEN                   00011600
.*           ELEMENTS.                                                  00011700
.*                                                                      00011800
.DCODE   AIF   ('&DCODE' EQ 'OMITTED').DCODEND                          00011900
&RETPTR  SETA  &RETPTR+1                                                00012000
&CTR     SETA  0                                                        00012100
         AIF   (K'&DCODE EQ 0).DCDFIN                                   00012200
         AIF   ('&DCODE'(1,1) EQ '(').DCDSLST                           00012300
&CTR     SETA  1                                                        00012400
&#TESRET(&RETPTR+1) SETC '&DCODE'                                       00012500
         AGO   .DCDFIN                                                  00012600
.DCDSLST ANOP                                                           00012700
&K       SETA  K'&DCODE                                                 00012800
         AIF   ('&DCODE'(&K,1) EQ ')').DCDOK                            00012900
         MNOTE 8,'"&DCODE" HAS INVALID SUBLIST SYNTAX.'                 00013000
&#TESERR SETA  16                                                       00013100
         AGO   .DCDFIN                                                  00013200
.DCDOK   ANOP                                                           00013300
&P       SETA  1                                                        00013400
&L       SETA  0                                                        00013500
.DCDLP1  ANOP                                                           00013600
&P       SETA  &P+&L+1                                                  00013700
&L       SETA  0-1                                                      00013800
&CTR     SETA  &CTR+1                                                   00013900
&#TESRET(&RETPTR+&CTR) SETC ''                                          00014000
.DCDLP2  ANOP                                                           00014100
&L       SETA  &L+1                                                     00014200
         AIF   ('&DCODE'(&P+&L,1) NE ',' AND &P+&L NE &K).DCDLP2        00014300
         AIF   (&L EQ 0).DCDLPET                                        00014400
&#TESRET(&RETPTR+&CTR) SETC '&DCODE'(&P,&L)                             00014500
         AIF   ('&#TESRET(&RETPTR+&CTR)' EQ '&DCODE'(&P,&L)).DCDLPET    00014600
         MNOTE 8,'ERROR - THE FOLLOWING TRUNCATION HAS OCCURED:'        00014700
         MNOTE *,'        &DCODE'                                       00014800
         MNOTE *,'        &#TESRET(&RETPTR+&CTR)'                       00014900
.DCDLPET AIF   (&P+&L NE &K).DCDLP1                                     00015000
.DCDFIN  ANOP                                                           00015100
&#TESRET(&RETPTR) SETC '&CTR'                                           00015200
&RETPTR  SETA  &RETPTR+&CTR                                             00015300
.DCODEND ANOP                                                           00015400
.*                                                                      00015500
.*                                                                      00015600
.*                                                                      00015700
.*                            THE "MEXCL" FUNCTION                      00015800
.*   THIS FUNCTION CAN BE USED TO DETERMINE IF TWO OR MORE              00015900
.* MUTUALLY EXCLUSIVE OPERANDS (OR WHATEVER) HAVE BEEN                  00016000
.* PASSED TO THE CALLING MACRO.                                         00016100
.*   THE INPUT IS COMMUNICATED VIA THE "MEXCL=" OPERAND AS A            00016200
.* SUB-LIST WITH ANY NUMBER OF ENTRIES. IF THE MEXCL                    00016300
.* FUNCTION FINDS MORE THAN ONE NON-NULL ENTRY IN THE                   00016400
.* SUB-LIST, THEN IT ISSUES A SEVERITY 8 ERROR MESSAGE, AND             00016500
.* IT SETS &#TESERR TO A VALUE OF 16.                                   00016600
.*                                                                      00016700
.MEXCL   AIF   (N'&MEXCL LT 2).MEXCEND                                  00016800
&CTR     SETA  0                                                        00016900
.MELP    AIF   (&CTR EQ N'&MEXCL).MEXCEND                               00017000
&CTR     SETA  &CTR+1                                                   00017100
         AIF   (K'&MEXCL(&CTR) EQ 0).MELP                               00017200
         AIF   (&MESW EQ 1).MEERR                                       00017300
&MESW    SETB  (1)                                                      00017400
         AGO   .MELP                                                    00017500
.MEERR   MNOTE 8,'ERROR - MUTUALLY EXCLUSIVE OPERANDS HAVE BEEN USED:'  00017600
         MNOTE *,'        &MEXCL'                                       00017700
&#TESERR SETA  16                                                       00017800
.MEXCEND ANOP                                                           00017900
.*                                                                      00018000
.*                                                                      00018100
.*                                                                      00018200
.*                            THE "NUM" FUNCTION                        00018300
.*   THIS FUNCTION CAN BE USED TO DETERMINE WHETHER OR NOT A            00018400
.* GIVEN VALUE CONSISTS ENTIRELY OF DIGITS.                             00018500
.*   THE INPUT IS COMMUNICATED VIA THE "NUM=" OPERAND. IF               00018600
.* THE NUM FUNCTION FINDS THAT ANY CHARACTER IN THE GIVEN               00018700
.* STRING IS NOT A DIGIT, THEN IT SETS &#TESERR TO A VALUE              00018800
.* OF 16.                                                               00018900
.*                                                                      00019000
.NUM     AIF   ('&NUM' EQ 'OMITTED').NUMEND                             00019100
         AIF   (K'&NUM EQ 0).NUMERR                                     00019200
&CTR     SETA  0                                                        00019300
.NUMLP   AIF   (&CTR EQ K'&NUM).NUMEND                                  00019400
&CTR     SETA  &CTR+1                                                   00019500
         AIF   ('&NUM'(&CTR,1) LT '0').NUMERR                           00019600
         AIF   ('&NUM'(&CTR,1) LE '9').NUMLP                            00019700
.NUMERR  ANOP                                                           00019800
&#TESERR SETA  16                                                       00019900
.NUMEND  ANOP                                                           00020000
.*                                                                      00020100
.*                                                                      00020200
.*                                                                      00020300
.*                            THE "PFIX" FUNCTION                       00020400
.*   THIS FUNCTION CAN BE USED TO DETERMINE THE VALIDITY OF             00020500
.* A GIVEN REGISTER NAME PREFIX (E.G. "R" IN "R15").                    00020600
.*   THE INPUT CONSISTS OF A REGISTER NAME PREFIX                       00020700
.* COMMUNICATED VIA THE "PFIX=" OPERAND AND A TABLE OF VALID            00020800
.* REGISTER NAME PREFIXES GENERATED VIA PRIOR #REGS MACROS              00020900
.* AND CONTAINED IN THE &#REGNME AND &#REGVAL GLOBAL                    00021000
.* SYMBOLS. IF THE GIVEN PREFIX IS NULL, THEN A DEFAULT IS              00021100
.* USED. IF THE GIVEN PREFIX IS INVALID, THEN A SEVERITY 4              00021200
.* ERROR MESSAGE IS ISSUED AND A DEFAULT PREFIX IS USED. THE            00021300
.* DEFAULT IS EITHER THE FIRST PREFIX DEFINED VIA A PRIOR               00021400
.* #REGS MACRO OR NULL IF NO PRIOR #REGS MACRO HAS DEFINED              00021500
.* ANY PREFIXES.                                                        00021600
.*   FOR OUTPUT, THE NEXT AVAILABLE &#TESRET ENTRY IS FILLED            00021700
.* WITH EITHER THE GIVEN PREFIX OR THE DEFAULT PREFIX.                  00021800
.*                                                                      00021900
.PFIX    AIF   ('&PFIX' EQ 'OMITTED').PFIXEND                           00022000
&RETPTR  SETA  &RETPTR+1                                                00022100
&#TESRET(&RETPTR) SETC ''                                               00022200
&CTR     SETA  0                                                        00022300
.PFXLP1  AIF   (&CTR GE 255).PFXGDEF                                    00022400
&CTR     SETA  &CTR+1                                                   00022500
         AIF   ('&#REGNME(&CTR)' EQ '').PFXGDEF                         00022600
         AIF   (&#REGVAL(&CTR) LE 15).PFXLP1                            00022700
&#TESRET(&RETPTR) SETC '&#REGNME(&CTR)'                                 00022800
.PFXGDEF AIF   (K'&PFIX EQ 0).PFIXEND                                   00022900
&CTR     SETA  &CTR-1                                                   00023000
.PFXLP2  AIF   (&CTR GE 255).PFXERR                                     00023100
&CTR     SETA  &CTR+1                                                   00023200
         AIF   ('&#REGNME(&CTR)' EQ '').PFXERR                          00023300
         AIF   (&#REGVAL(&CTR) LE 15).PFXLP2                            00023400
         AIF   ('&PFIX' NE '&#REGNME(&CTR)').PFXLP2                     00023500
&#TESRET(&RETPTR) SETC '&PFIX'                                          00023600
         AGO   .PFIXEND                                                 00023700
.PFXERR  MNOTE 4,'WARNING - "&PFIX" HAS NOT BEEN PREDEFINED.'           00023800
         MNOTE *,'          A DEFAULT VALUE WILL BE USED.'              00023900
         MNOTE *,'          CHECK YOUR USAGE OF THE #REGS'              00024000
         MNOTE *,'          MACRO.'                                     00024100
.PFIXEND ANOP                                                           00024200
.*                                                                      00024300
.*                                                                      00024400
.*                                                                      00024500
.*                            THE "REGS" FUNCTION                       00024600
.*   THIS FUNCTION CAN BE USED TO CONVERT A CERTAIN CLASS OF            00024700
.* REGISTER NAMES TO THEIR CORRESPONDING NUMERIC VALUES. IN             00024800
.* ORDER FOR A GIVEN NAME TO BE CONVERTED, IT MUST BE EITHER            00024900
.* A SELF-DEFINING NUMERIC OR A NAME THAT HAS BEEN DEFINED              00025000
.* VIA A PRIOR #REGS MACRO. THE PURPOSE OF THIS FUNCTION IS             00025100
.* TO PROVIDE ARITHMETICLY MANIPULATABLE REGISTER NUMBERS.              00025200
.*   THE INPUT CONSISTS OF A SUB-LIST OF REGISTER NAMES                 00025300
.* COMMUNICATED VIA THE "REGS=" OPERAND AND A TABLE OF VALID            00025400
.* REGISTER NAME PREFIXES GENERATED VIA PRIOR #REGS MACROS              00025500
.* AND COMMUNICATED VIA THE &#REGNME AND &#REGVAL GLOBAL                00025600
.* SYMBOLS.                                                             00025700
.*   FOR A SUB-LIST OF N REGISTER NAMES, THE OUTPUT CONSISTS            00025800
.* OF N ENTRIES IN &#TESRET EACH CONTAINING THE NUMBER OF               00025900
.* THE REGISTER REPRESENTED BY THE CORRESPONDING NAME FROM              00026000
.* THE INPUT SUB-LIST.                                                  00026100
.*   IF ANY REGISTER NAME CANNOT BE PROPERLY DECODED, THEN              00026200
.* &#TESERR IS SET TO A VALUE OF 16. NO ERROR MESSAGE IS                00026300
.* ISSUED.                                                              00026400
.*                                                                      00026500
.REGS    AIF   (N'&REGS EQ 0).REGSEND                                   00026600
&CTR     SETA  0                                                        00026700
.REGLP1  AIF   (&CTR GE 16).REGLP2                                      00026800
&RN(&CTR+1) SETC '&CTR'                                                 00026900
&RV(&CTR+1) SETA &CTR                                                   00027000
&CTR     SETA  &CTR+1                                                   00027100
         AGO   .REGLP1                                                  00027200
.REGLP2  AIF   (&CTR GE 22).REGND2                                      00027300
&CTR     SETA  &CTR+1                                                   00027400
&RN(&CTR) SETC 'ABCDEF'(&CTR-16,1)                                      00027500
&RV(&CTR) SETA &CTR-7                                                   00027600
         AGO   .REGLP2                                                  00027700
.REGND2  ANOP                                                           00027800
&CTR     SETA  0                                                        00027900
.REGLP3  AIF   (&CTR GE N'&REGS).REGSEND                                00028000
&CTR     SETA  &CTR+1                                                   00028100
&RETPTR  SETA  &RETPTR+1                                                00028200
&#TESRET(&RETPTR) SETC ''                                               00028300
         AIF   (K'&REGS(&CTR) EQ 0).REGLP3                              00028400
&#TESRET(&RETPTR) SETC '&REGS(&CTR)'                                    00028500
         AIF   (T'&REGS(&CTR) NE 'N').REGLP3A                           00028600
         AIF   (&REGS(&CTR) LT 0 OR &REGS(&CTR) GT 15).REGERR           00028700
         AGO   .REGLP3                                                  00028800
.REGLP3A ANOP                                                           00028900
&A1      SETA  0                                                        00029000
.REGLP4  AIF   (&A1 GE 255).REGND4                                      00029100
&A1      SETA  &A1+1                                                    00029200
         AIF   ('&#REGNME(&A1)' EQ '').REGND4                           00029300
         AIF   (&#REGVAL(&A1) GT 15).REGPFX                             00029400
         AIF   ('&REGS(&CTR)' NE '&#REGNME(&A1)').REGLP4                00029500
&#TESRET(&RETPTR) SETC '&#REGVAL(&A1)'                                  00029600
         AGO   .REGLP3                                                  00029700
.REGPFX  ANOP                                                           00029800
&A2      SETA  0                                                        00029900
.REGLP5  AIF   (&A2 GE 22).REGLP4                                       00030000
&A2      SETA  &A2+1                                                    00030100
         AIF   ('&REGS(&CTR)' NE '&#REGNME(&A1)&RN(&A2)').REGLP5        00030200
&#TESRET(&RETPTR) SETC '&RV(&A2)'                                       00030300
         AGO   .REGLP3                                                  00030400
.REGND4  ANOP                                                           00030500
&A2      SETA  0                                                        00030600
.REGLP6  AIF   (&A2 GE 16).REGERR                                       00030700
&A2      SETA  &A2+1                                                    00030800
         AIF   ('&REGS(&CTR)' NE '&RN(&A2)').REGLP6                     00030900
         AGO   .REGLP3                                                  00031000
.REGERR  ANOP                                                           00031100
&#TESERR SETA  16                                                       00031200
         AGO   .REGLP3                                                  00031300
.REGSEND ANOP                                                           00031400
.*                                                                      00031500
.*                                                                      00031600
.*                                                                      00031700
.*                            THE "SIZE" FUNCTION                       00031800
.*   THIS FUNCTION WAS WRITTEN BECAUSE OF THE LIMITATION                00031900
.* THAT MACRO CODE CANNOT ARITHMETICLY MANIPULATE OPERANDS              00032000
.* CONSISTING OF EITHER EXPRESSIONS OR EQUATE SYMBOLS.                  00032100
.* BECAUSE OF THIS, IN SITUATIONS WHERE A PROGRAMMER WOULD              00032200
.* NORMALLY WANT TO USE AN EXPRESSION, ETC., HE MAY INSTEAD             00032300
.* BE FORCED TO USE A SELF DEFINING NUMERIC. THE PROBLEM IS             00032400
.* THAT IF SUBSEQUENT MODIFICATIONS AFFECT THE VALUE OF SUCH            00032500
.* AN EXPRESSION, THE PROGRAMMER MIGHT FORGET TO CHANGE THE             00032600
.* SELF DEFINING NUMERIC ACCORDINGLY. THE SIZE FUNCTION CAN             00032700
.* BE USED TO ALLEVIATE THIS PROBLEM.                                   00032800
.*   THE INPUT IS COMMUNICATED VIA THE "SIZE=" OPERAND AND              00032900
.* IT MUST CONSIST OF A THREE ELEMENT SUB-LIST. THE FIRST               00033000
.* AND THIRD ELEMENTS MUST BE SUCH THAT THEY RESULTS IN                 00033100
.* NON-RELOCATABLE VALUES WHEN ASSEMBLED. THE SECOND OPERAND            00033200
.* MUST BE ONE OF THE FOLLOWING RELATIONAL OPERATORS:                   00033300
.*       LT, LE, EQ, GE, GT, NE                                         00033400
.* MEANING "LESS THAN", "LESS THAN OR EQUAL", "EQUAL",                  00033500
.* "GREATER THAN OR EQUAL", "GREATER THAN", AND "NOT EQUAL"             00033600
.* RESPECTIVELY.                                                        00033700
.*   THE OUTPUT CONSISTS OF A GENERATED STATEMENT WHICH                 00033800
.* PRODUCES NO OBJECT CODE BUT WHICH CAUSES AN ERROR                    00033900
.* WHENEVER THE TWO GIVEN ELEMENTS VIOLATE THE INDICATED                00034000
.* RELATION.                                                            00034100
.*                                                                      00034200
.SIZE    AIF   (N'&SIZE EQ 0).SIZEEND                                   00034300
         AIF   ('&SIZE(2)' EQ 'EQ' OR '&SIZE(2)' EQ 'GE' OR '&SIZE(2)' *00034400
               EQ 'LE' OR '&SIZE(2)' EQ 'GT' OR '&SIZE(2)' EQ 'LT' OR '*00034500
               &SIZE(2)' EQ 'NE').OPOK                                  00034600
         MNOTE 8,'ERROR - "&SIZE(2)" NOT A VALID RELATIONAL OPERATOR'   00034700
&#TESERR SETA  16                                                       00034800
         AGO   .SIZEEND                                                 00034900
.OPOK    AIF   ('&SIZE(2)' NE 'EQ').OPNTEQ                              00035000
         DC    0YL2(X'7FFF'-(&SIZE(1))+&SIZE(3),X'7FFF'-(&SIZE(3))+&SIZ*00035100
               E(1))                                                    00035200
         AGO   .SIZEEND                                                 00035300
.OPNTEQ  AIF   ('&SIZE(2)' NE 'NE').OPNTNE                              00035400
         DC    0YL2(X'8000'-(&SIZE(3)-(&SIZE(1)))/(&SIZE(3)-(&SIZE(1)))*00035500
               )                                                        00035600
         AGO   .SIZEEND                                                 00035700
.OPNTNE  ANOP                                                           00035800
&BASE    SETC  '7FFF'                                                   00035900
         AIF   ('&SIZE(2)'(2,1) EQ 'E').NOPLUS1                         00036000
&BASE    SETC  '8000'                                                   00036100
.NOPLUS1 AIF   ('&SIZE(2)'(1,1) EQ 'G').OPG                             00036200
         DC    0YL2(X'&BASE'-(&SIZE(3))+&SIZE(1))                       00036300
         AGO   .SIZEEND                                                 00036400
.OPG     ANOP                                                           00036500
         DC    0YL2(X'&BASE'-(&SIZE(1))+&SIZE(3))                       00036600
.SIZEEND ANOP                                                           00036700
.*                                                                      00036800
.*                                                                      00036900
.*                                                                      00037000
.*                                 THE "GEN" FUNCTION                   00037100
.*    THIS FUNCTION CAN BE USED TO GENERATE VARIOUS SPECIFIC            00037200
.* OBJECTS. CURRENTLY, THE SUPPORTED OBJECTS ARE:                       00037300
.*       EBCDIC - A GLOBAL TABLE CONTAINING THE ENTIRE                  00037400
.*                256-ENTRY EBCDIC CHARACTER SET SUCH THAT              00037500
.*                THE VALUE OF THE ITH ENTRY IS I-1.                    00037600
.*                                                                      00037700
.GEN     AIF   (N'&GEN EQ 0).GENEND                                     00037800
&A1      SETA  0                                                        00037900
.GENLP   AIF   (&A1 EQ N'&GEN).GENEND                                   00038000
&A1      SETA  &A1+1                                                    00038100
         AIF   ('&GEN(&A1)' EQ '').GENLP                                00038200
         AIF   ('&GEN(&A1)' NE 'EBCDIC').GNTEBCD                        00038300
         AIF   ('&#EBCDIC(194)' EQ 'A').GENLP                           00038400
         PUSH  PRINT                                          09/96 X32 44090000
         PRINT OFF                 GENERATE #TESTX-- EQUATES  11/96 X32 44160000
#TESTX00 EQU   000,,X'00'                                     10/97 X33 44230000
#TESTX01 EQU   001,,X'01'                                     09/96 X32 44300000
#TESTX02 EQU   002,,X'02'                                     09/96 X32 44380000
#TESTX03 EQU   003,,X'03'                                     09/96 X32 44450000
#TESTX04 EQU   004,,X'04'                                     09/96 X32 44520000
#TESTX05 EQU   005,,X'05'                                     09/96 X32 44590000
#TESTX06 EQU   006,,X'06'                                     09/96 X32 44660000
#TESTX07 EQU   007,,X'07'                                     09/96 X32 44730000
#TESTX08 EQU   008,,X'08'                                     09/96 X32 44810000
#TESTX09 EQU   009,,X'09'                                     09/96 X32 44880000
#TESTX0A EQU   010,,X'0A'                                     09/96 X32 44950000
#TESTX0B EQU   011,,X'0B'                                     09/96 X32 45020000
#TESTX0C EQU   012,,X'0C'                                     09/96 X32 45090000
#TESTX0D EQU   013,,X'0D'                                     09/96 X32 45160000
#TESTX0E EQU   014,,X'0E'                                     09/96 X32 45230000
#TESTX0F EQU   015,,X'0F'                                     09/96 X32 45310000
#TESTX10 EQU   016,,X'10'                                     09/96 X32 45380000
#TESTX11 EQU   017,,X'11'                                     09/96 X32 45450000
#TESTX12 EQU   018,,X'12'                                     09/96 X32 45520000
#TESTX13 EQU   019,,X'13'                                     09/96 X32 45590000
#TESTX14 EQU   020,,X'14'                                     09/96 X32 45660000
#TESTX15 EQU   021,,X'15'                                     09/96 X32 45740000
#TESTX16 EQU   022,,X'16'                                     09/96 X32 45810000
#TESTX17 EQU   023,,X'17'                                     09/96 X32 45880000
#TESTX18 EQU   024,,X'18'                                     09/96 X32 45950000
#TESTX19 EQU   025,,X'19'                                     09/96 X32 46020000
#TESTX1A EQU   026,,X'1A'                                     09/96 X32 46090000
#TESTX1B EQU   027,,X'1B'                                     09/96 X32 46170000
#TESTX1C EQU   028,,X'1C'                                     09/96 X32 46240000
#TESTX1D EQU   029,,X'1D'                                     09/96 X32 46310000
#TESTX1E EQU   030,,X'1E'                                     09/96 X32 46380000
#TESTX1F EQU   031,,X'1F'                                     09/96 X32 46450000
#TESTX20 EQU   032,,X'20'                                     09/96 X32 46520000
#TESTX21 EQU   033,,X'21'                                     09/96 X32 46590000
#TESTX22 EQU   034,,X'22'                                     09/96 X32 46670000
#TESTX23 EQU   035,,X'23'                                     09/96 X32 46740000
#TESTX24 EQU   036,,X'24'                                     09/96 X32 46810000
#TESTX25 EQU   037,,X'25'                                     09/96 X32 46880000
#TESTX26 EQU   038,,X'26'                                     09/96 X32 46950000
#TESTX27 EQU   039,,X'27'                                     09/96 X32 47020000
#TESTX28 EQU   040,,X'28'                                     09/96 X32 47100000
#TESTX29 EQU   041,,X'29'                                     09/96 X32 47170000
#TESTX2A EQU   042,,X'2A'                                     09/96 X32 47240000
#TESTX2B EQU   043,,X'2B'                                     09/96 X32 47310000
#TESTX2C EQU   044,,X'2C'                                     09/96 X32 47380000
#TESTX2D EQU   045,,X'2D'                                     09/96 X32 47450000
#TESTX2E EQU   046,,X'2E'                                     09/96 X32 47530000
#TESTX2F EQU   047,,X'2F'                                     09/96 X32 47600000
#TESTX30 EQU   048,,X'30'                                     09/96 X32 47670000
#TESTX31 EQU   049,,X'31'                                     09/96 X32 47740000
#TESTX32 EQU   050,,X'32'                                     09/96 X32 47810000
#TESTX33 EQU   051,,X'33'                                     09/96 X32 47880000
#TESTX34 EQU   052,,X'34'                                     09/96 X32 47950000
#TESTX35 EQU   053,,X'35'                                     09/96 X32 48030000
#TESTX36 EQU   054,,X'36'                                     09/96 X32 48100000
#TESTX37 EQU   055,,X'37'                                     09/96 X32 48170000
#TESTX38 EQU   056,,X'38'                                     09/96 X32 48240000
#TESTX39 EQU   057,,X'39'                                     09/96 X32 48310000
#TESTX3A EQU   058,,X'3A'                                     09/96 X32 48380000
#TESTX3B EQU   059,,X'3B'                                     09/96 X32 48460000
#TESTX3C EQU   060,,X'3C'                                     09/96 X32 48530000
#TESTX3D EQU   061,,X'3D'                                     09/96 X32 48600000
#TESTX3E EQU   062,,X'3E'                                     09/96 X32 48670000
#TESTX3F EQU   063,,X'3F'                                     09/96 X32 48740000
#TESTX40 EQU   064,,X'40'                                     09/96 X32 48810000
#TESTX41 EQU   065,,X'41'                                     09/96 X32 48890000
#TESTX42 EQU   066,,X'42'                                     09/96 X32 48960000
#TESTX43 EQU   067,,X'43'                                     09/96 X32 49030000
#TESTX44 EQU   068,,X'44'                                     09/96 X32 49100000
#TESTX45 EQU   069,,X'45'                                     09/96 X32 49170000
#TESTX46 EQU   070,,X'46'                                     09/96 X32 49240000
#TESTX47 EQU   071,,X'47'                                     09/96 X32 49310000
#TESTX48 EQU   072,,X'48'                                     09/96 X32 49390000
#TESTX49 EQU   073,,X'49'                                     09/96 X32 49460000
#TESTX4A EQU   074,,X'4A'                                     09/96 X32 49530000
#TESTX4B EQU   075,,X'4B'                                     09/96 X32 49600000
#TESTX4C EQU   076,,X'4C'                                     09/96 X32 49670000
#TESTX4D EQU   077,,X'4D'                                     09/96 X32 49740000
#TESTX4E EQU   078,,X'4E'                                     09/96 X32 49820000
#TESTX4F EQU   079,,X'4F'                                     09/96 X32 49890000
#TESTX50 EQU   080,,X'50'                                     09/96 X32 49960000
#TESTX51 EQU   081,,X'51'                                     09/96 X32 50030000
#TESTX52 EQU   082,,X'52'                                     09/96 X32 50100000
#TESTX53 EQU   083,,X'53'                                     09/96 X32 50170000
#TESTX54 EQU   084,,X'54'                                     09/96 X32 50250000
#TESTX55 EQU   085,,X'55'                                     09/96 X32 50320000
#TESTX56 EQU   086,,X'56'                                     09/96 X32 50390000
#TESTX57 EQU   087,,X'57'                                     09/96 X32 50460000
#TESTX58 EQU   088,,X'58'                                     09/96 X32 50530000
#TESTX59 EQU   089,,X'59'                                     09/96 X32 50600000
#TESTX5A EQU   090,,X'5A'                                     09/96 X32 50680000
#TESTX5B EQU   091,,X'5B'                                     09/96 X32 50750000
#TESTX5C EQU   092,,X'5C'                                     09/96 X32 50820000
#TESTX5D EQU   093,,X'5D'                                     09/96 X32 50890000
#TESTX5E EQU   094,,X'5E'                                     09/96 X32 50960000
#TESTX5F EQU   095,,X'5F'                                     09/96 X32 51030000
#TESTX60 EQU   096,,X'60'                                     09/96 X32 51100000
#TESTX61 EQU   097,,X'61'                                     09/96 X32 51180000
#TESTX62 EQU   098,,X'62'                                     09/96 X32 51250000
#TESTX63 EQU   099,,X'63'                                     09/96 X32 51320000
#TESTX64 EQU   100,,X'64'                                     09/96 X32 51390000
#TESTX65 EQU   101,,X'65'                                     09/96 X32 51460000
#TESTX66 EQU   102,,X'66'                                     09/96 X32 51530000
#TESTX67 EQU   103,,X'67'                                     09/96 X32 51610000
#TESTX68 EQU   104,,X'68'                                     09/96 X32 51680000
#TESTX69 EQU   105,,X'69'                                     09/96 X32 51750000
#TESTX6A EQU   106,,X'6A'                                     09/96 X32 51820000
#TESTX6B EQU   107,,X'6B'                                     09/96 X32 51890000
#TESTX6C EQU   108,,X'6C'                                     09/96 X32 51960000
#TESTX6D EQU   109,,X'6D'                                     09/96 X32 52040000
#TESTX6E EQU   110,,X'6E'                                     09/96 X32 52110000
#TESTX6F EQU   111,,X'6F'                                     09/96 X32 52180000
#TESTX70 EQU   112,,X'70'                                     09/96 X32 52250000
#TESTX71 EQU   113,,X'71'                                     09/96 X32 52320000
#TESTX72 EQU   114,,X'72'                                     09/96 X32 52390000
#TESTX73 EQU   115,,X'73'                                     09/96 X32 52460000
#TESTX74 EQU   116,,X'74'                                     09/96 X32 52540000
#TESTX75 EQU   117,,X'75'                                     09/96 X32 52610000
#TESTX76 EQU   118,,X'76'                                     09/96 X32 52680000
#TESTX77 EQU   119,,X'77'                                     09/96 X32 52750000
#TESTX78 EQU   120,,X'78'                                     09/96 X32 52820000
#TESTX79 EQU   121,,X'79'                                     09/96 X32 52890000
#TESTX7A EQU   122,,X'7A'                                     09/96 X32 52970000
#TESTX7B EQU   123,,X'7B'                                     09/96 X32 53040000
#TESTX7C EQU   124,,X'7C'                                     09/96 X32 53110000
#TESTX7D EQU   125,,X'7D'                                     09/96 X32 53180000
#TESTX7E EQU   126,,X'7E'                                     09/96 X32 53250000
#TESTX7F EQU   127,,X'7F'                                     09/96 X32 53320000
#TESTX80 EQU   128,,X'80'                                     09/96 X32 53400000
#TESTX81 EQU   129,,X'81'                                     09/96 X32 53470000
#TESTX82 EQU   130,,X'82'                                     09/96 X32 53540000
#TESTX83 EQU   131,,X'83'                                     09/96 X32 53610000
#TESTX84 EQU   132,,X'84'                                     09/96 X32 53680000
#TESTX85 EQU   133,,X'85'                                     09/96 X32 53750000
#TESTX86 EQU   134,,X'86'                                     09/96 X32 53820000
#TESTX87 EQU   135,,X'87'                                     09/96 X32 53900000
#TESTX88 EQU   136,,X'88'                                     09/96 X32 53970000
#TESTX89 EQU   137,,X'89'                                     09/96 X32 54040000
#TESTX8A EQU   138,,X'8A'                                     09/96 X32 54110000
#TESTX8B EQU   139,,X'8B'                                     09/96 X32 54180000
#TESTX8C EQU   140,,X'8C'                                     09/96 X32 54250000
#TESTX8D EQU   141,,X'8D'                                     09/96 X32 54330000
#TESTX8E EQU   142,,X'8E'                                     09/96 X32 54400000
#TESTX8F EQU   143,,X'8F'                                     09/96 X32 54470000
#TESTX90 EQU   144,,X'90'                                     09/96 X32 54540000
#TESTX91 EQU   145,,X'91'                                     09/96 X32 54610000
#TESTX92 EQU   146,,X'92'                                     09/96 X32 54680000
#TESTX93 EQU   147,,X'93'                                     09/96 X32 54760000
#TESTX94 EQU   148,,X'94'                                     09/96 X32 54830000
#TESTX95 EQU   149,,X'95'                                     09/96 X32 54900000
#TESTX96 EQU   150,,X'96'                                     09/96 X32 54970000
#TESTX97 EQU   151,,X'97'                                     09/96 X32 55040000
#TESTX98 EQU   152,,X'98'                                     09/96 X32 55110000
#TESTX99 EQU   153,,X'99'                                     09/96 X32 55180000
#TESTX9A EQU   154,,X'9A'                                     09/96 X32 55260000
#TESTX9B EQU   155,,X'9B'                                     09/96 X32 55330000
#TESTX9C EQU   156,,X'9C'                                     09/96 X32 55400000
#TESTX9D EQU   157,,X'9D'                                     09/96 X32 55470000
#TESTX9E EQU   158,,X'9E'                                     09/96 X32 55540000
#TESTX9F EQU   159,,X'9F'                                     09/96 X32 55610000
#TESTXA0 EQU   160,,X'A0'                                     09/96 X32 55690000
#TESTXA1 EQU   161,,X'A1'                                     09/96 X32 55760000
#TESTXA2 EQU   162,,X'A2'                                     09/96 X32 55830000
#TESTXA3 EQU   163,,X'A3'                                     09/96 X32 55900000
#TESTXA4 EQU   164,,X'A4'                                     09/96 X32 55970000
#TESTXA5 EQU   165,,X'A5'                                     09/96 X32 56040000
#TESTXA6 EQU   166,,X'A6'                                     09/96 X32 56120000
#TESTXA7 EQU   167,,X'A7'                                     09/96 X32 56190000
#TESTXA8 EQU   168,,X'A8'                                     09/96 X32 56260000
#TESTXA9 EQU   169,,X'A9'                                     09/96 X32 56330000
#TESTXAA EQU   170,,X'AA'                                     09/96 X32 56400000
#TESTXAB EQU   171,,X'AB'                                     09/96 X32 56470000
#TESTXAC EQU   172,,X'AC'                                     09/96 X32 56540000
#TESTXAD EQU   173,,X'AD'                                     09/96 X32 56620000
#TESTXAE EQU   174,,X'AE'                                     09/96 X32 56690000
#TESTXAF EQU   175,,X'AF'                                     09/96 X32 56760000
#TESTXB0 EQU   176,,X'B0'                                     09/96 X32 56830000
#TESTXB1 EQU   177,,X'B1'                                     09/96 X32 56900000
#TESTXB2 EQU   178,,X'B2'                                     09/96 X32 56970000
#TESTXB3 EQU   179,,X'B3'                                     09/96 X32 57050000
#TESTXB4 EQU   180,,X'B4'                                     09/96 X32 57120000
#TESTXB5 EQU   181,,X'B5'                                     09/96 X32 57190000
#TESTXB6 EQU   182,,X'B6'                                     09/96 X32 57260000
#TESTXB7 EQU   183,,X'B7'                                     09/96 X32 57330000
#TESTXB8 EQU   184,,X'B8'                                     09/96 X32 57400000
#TESTXB9 EQU   185,,X'B9'                                     09/96 X32 57480000
#TESTXBA EQU   186,,X'BA'                                     09/96 X32 57550000
#TESTXBB EQU   187,,X'BB'                                     09/96 X32 57620000
#TESTXBC EQU   188,,X'BC'                                     09/96 X32 57690000
#TESTXBD EQU   189,,X'BD'                                     09/96 X32 57760000
#TESTXBE EQU   190,,X'BE'                                     09/96 X32 57830000
#TESTXBF EQU   191,,X'BF'                                     09/96 X32 57900000
#TESTXC0 EQU   192,,X'C0'                                     09/96 X32 57980000
#TESTXC1 EQU   193,,X'C1'                                     09/96 X32 58050000
#TESTXC2 EQU   194,,X'C2'                                     09/96 X32 58120000
#TESTXC3 EQU   195,,X'C3'                                     09/96 X32 58190000
#TESTXC4 EQU   196,,X'C4'                                     09/96 X32 58260000
#TESTXC5 EQU   197,,X'C5'                                     09/96 X32 58330000
#TESTXC6 EQU   198,,X'C6'                                     09/96 X32 58410000
#TESTXC7 EQU   199,,X'C7'                                     09/96 X32 58480000
#TESTXC8 EQU   200,,X'C8'                                     09/96 X32 58550000
#TESTXC9 EQU   201,,X'C9'                                     09/96 X32 58620000
#TESTXCA EQU   202,,X'CA'                                     09/96 X32 58690000
#TESTXCB EQU   203,,X'CB'                                     09/96 X32 58760000
#TESTXCC EQU   204,,X'CC'                                     09/96 X32 58840000
#TESTXCD EQU   205,,X'CD'                                     09/96 X32 58910000
#TESTXCE EQU   206,,X'CE'                                     09/96 X32 58980000
#TESTXCF EQU   207,,X'CF'                                     09/96 X32 59050000
#TESTXD0 EQU   208,,X'D0'                                     09/96 X32 59120000
#TESTXD1 EQU   209,,X'D1'                                     09/96 X32 59190000
#TESTXD2 EQU   210,,X'D2'                                     09/96 X32 59260000
#TESTXD3 EQU   211,,X'D3'                                     09/96 X32 59340000
#TESTXD4 EQU   212,,X'D4'                                     09/96 X32 59410000
#TESTXD5 EQU   213,,X'D5'                                     09/96 X32 59480000
#TESTXD6 EQU   214,,X'D6'                                     09/96 X32 59550000
#TESTXD7 EQU   215,,X'D7'                                     09/96 X32 59620000
#TESTXD8 EQU   216,,X'D8'                                     09/96 X32 59690000
#TESTXD9 EQU   217,,X'D9'                                     09/96 X32 59770000
#TESTXDA EQU   218,,X'DA'                                     09/96 X32 59840000
#TESTXDB EQU   219,,X'DB'                                     09/96 X32 59910000
#TESTXDC EQU   220,,X'DC'                                     09/96 X32 59980000
#TESTXDD EQU   221,,X'DD'                                     09/96 X32 60050000
#TESTXDE EQU   222,,X'DE'                                     09/96 X32 60120000
#TESTXDF EQU   223,,X'DF'                                     09/96 X32 60200000
#TESTXE0 EQU   224,,X'E0'                                     09/96 X32 60270000
#TESTXE1 EQU   225,,X'E1'                                     09/96 X32 60340000
#TESTXE2 EQU   226,,X'E2'                                     09/96 X32 60410000
#TESTXE3 EQU   227,,X'E3'                                     09/96 X32 60480000
#TESTXE4 EQU   228,,X'E4'                                     09/96 X32 60550000
#TESTXE5 EQU   229,,X'E5'                                     09/96 X32 60620000
#TESTXE6 EQU   230,,X'E6'                                     09/96 X32 60700000
#TESTXE7 EQU   231,,X'E7'                                     09/96 X32 60770000
#TESTXE8 EQU   232,,X'E8'                                     09/96 X32 60840000
#TESTXE9 EQU   233,,X'E9'                                     09/96 X32 60910000
#TESTXEA EQU   234,,X'EA'                                     09/96 X32 60980000
#TESTXEB EQU   235,,X'EB'                                     09/96 X32 61050000
#TESTXEC EQU   236,,X'EC'                                     09/96 X32 61130000
#TESTXED EQU   237,,X'ED'                                     09/96 X32 61200000
#TESTXEE EQU   238,,X'EE'                                     09/96 X32 61270000
#TESTXEF EQU   239,,X'EF'                                     09/96 X32 61340000
#TESTXF0 EQU   240,,X'F0'                                     09/96 X32 61410000
#TESTXF1 EQU   241,,X'F1'                                     09/96 X32 61480000
#TESTXF2 EQU   242,,X'F2'                                     09/96 X32 61560000
#TESTXF3 EQU   243,,X'F3'                                     09/96 X32 61630000
#TESTXF4 EQU   244,,X'F4'                                     09/96 X32 61700000
#TESTXF5 EQU   245,,X'F5'                                     09/96 X32 61770000
#TESTXF6 EQU   246,,X'F6'                                     09/96 X32 61840000
#TESTXF7 EQU   247,,X'F7'                                     09/96 X32 61910000
#TESTXF8 EQU   248,,X'F8'                                     09/96 X32 61980000
#TESTXF9 EQU   249,,X'F9'                                     09/96 X32 62060000
#TESTXFA EQU   250,,X'FA'                                     09/96 X32 62130000
#TESTXFB EQU   251,,X'FB'                                     09/96 X32 62200000
#TESTXFC EQU   252,,X'FC'                                     09/96 X32 62270000
#TESTXFD EQU   253,,X'FD'                                     09/96 X32 62340000
#TESTXFE EQU   254,,X'FE'                                     09/96 X32 62410000
#TESTXFF EQU   255,,X'FF'                                     09/96 X32 62490000
         POP   PRINT                                          09/96 X32 62560000
&#EBCDIC(001) SETC T'#TESTX00                                 10/97 X33 62630000
&#EBCDIC(002) SETC T'#TESTX01                                 09/96 X32 62700000
&#EBCDIC(003) SETC T'#TESTX02                                 09/96 X32 62770000
&#EBCDIC(004) SETC T'#TESTX03                                 09/96 X32 62840000
&#EBCDIC(005) SETC T'#TESTX04                                 09/96 X32 62920000
&#EBCDIC(006) SETC T'#TESTX05                                 09/96 X32 62990000
&#EBCDIC(007) SETC T'#TESTX06                                 09/96 X32 63060000
&#EBCDIC(008) SETC T'#TESTX07                                 09/96 X32 63130000
&#EBCDIC(009) SETC T'#TESTX08                                 09/96 X32 63200000
&#EBCDIC(010) SETC T'#TESTX09                                 09/96 X32 63270000
&#EBCDIC(011) SETC T'#TESTX0A                                 09/96 X32 63350000
&#EBCDIC(012) SETC T'#TESTX0B                                 09/96 X32 63420000
&#EBCDIC(013) SETC T'#TESTX0C                                 09/96 X32 63490000
&#EBCDIC(014) SETC T'#TESTX0D                                 09/96 X32 63560000
&#EBCDIC(015) SETC T'#TESTX0E                                 09/96 X32 63630000
&#EBCDIC(016) SETC T'#TESTX0F                                 09/96 X32 63700000
&#EBCDIC(017) SETC T'#TESTX10                                 09/96 X32 63770000
&#EBCDIC(018) SETC T'#TESTX11                                 09/96 X32 63850000
&#EBCDIC(019) SETC T'#TESTX12                                 09/96 X32 63920000
&#EBCDIC(020) SETC T'#TESTX13                                 09/96 X32 63990000
&#EBCDIC(021) SETC T'#TESTX14                                 09/96 X32 64060000
&#EBCDIC(022) SETC T'#TESTX15                                 09/96 X32 64130000
&#EBCDIC(023) SETC T'#TESTX16                                 09/96 X32 64200000
&#EBCDIC(024) SETC T'#TESTX17                                 09/96 X32 64280000
&#EBCDIC(025) SETC T'#TESTX18                                 09/96 X32 64350000
&#EBCDIC(026) SETC T'#TESTX19                                 09/96 X32 64420000
&#EBCDIC(027) SETC T'#TESTX1A                                 09/96 X32 64490000
&#EBCDIC(028) SETC T'#TESTX1B                                 09/96 X32 64560000
&#EBCDIC(029) SETC T'#TESTX1C                                 09/96 X32 64630000
&#EBCDIC(030) SETC T'#TESTX1D                                 09/96 X32 64710000
&#EBCDIC(031) SETC T'#TESTX1E                                 09/96 X32 64780000
&#EBCDIC(032) SETC T'#TESTX1F                                 09/96 X32 64850000
&#EBCDIC(033) SETC T'#TESTX20                                 09/96 X32 64920000
&#EBCDIC(034) SETC T'#TESTX21                                 09/96 X32 64990000
&#EBCDIC(035) SETC T'#TESTX22                                 09/96 X32 65060000
&#EBCDIC(036) SETC T'#TESTX23                                 09/96 X32 65130000
&#EBCDIC(037) SETC T'#TESTX24                                 09/96 X32 65210000
&#EBCDIC(038) SETC T'#TESTX25                                 09/96 X32 65280000
&#EBCDIC(039) SETC T'#TESTX26                                 09/96 X32 65350000
&#EBCDIC(040) SETC T'#TESTX27                                 09/96 X32 65420000
&#EBCDIC(041) SETC T'#TESTX28                                 09/96 X32 65490000
&#EBCDIC(042) SETC T'#TESTX29                                 09/96 X32 65560000
&#EBCDIC(043) SETC T'#TESTX2A                                 09/96 X32 65640000
&#EBCDIC(044) SETC T'#TESTX2B                                 09/96 X32 65710000
&#EBCDIC(045) SETC T'#TESTX2C                                 09/96 X32 65780000
&#EBCDIC(046) SETC T'#TESTX2D                                 09/96 X32 65850000
&#EBCDIC(047) SETC T'#TESTX2E                                 09/96 X32 65920000
&#EBCDIC(048) SETC T'#TESTX2F                                 09/96 X32 65990000
&#EBCDIC(049) SETC T'#TESTX30                                 09/96 X32 66070000
&#EBCDIC(050) SETC T'#TESTX31                                 09/96 X32 66140000
&#EBCDIC(051) SETC T'#TESTX32                                 09/96 X32 66210000
&#EBCDIC(052) SETC T'#TESTX33                                 09/96 X32 66280000
&#EBCDIC(053) SETC T'#TESTX34                                 09/96 X32 66350000
&#EBCDIC(054) SETC T'#TESTX35                                 09/96 X32 66420000
&#EBCDIC(055) SETC T'#TESTX36                                 09/96 X32 66490000
&#EBCDIC(056) SETC T'#TESTX37                                 09/96 X32 66570000
&#EBCDIC(057) SETC T'#TESTX38                                 09/96 X32 66640000
&#EBCDIC(058) SETC T'#TESTX39                                 09/96 X32 66710000
&#EBCDIC(059) SETC T'#TESTX3A                                 09/96 X32 66780000
&#EBCDIC(060) SETC T'#TESTX3B                                 09/96 X32 66850000
&#EBCDIC(061) SETC T'#TESTX3C                                 09/96 X32 66920000
&#EBCDIC(062) SETC T'#TESTX3D                                 09/96 X32 67000000
&#EBCDIC(063) SETC T'#TESTX3E                                 09/96 X32 67070000
&#EBCDIC(064) SETC T'#TESTX3F                                 09/96 X32 67140000
&#EBCDIC(065) SETC T'#TESTX40                                 09/96 X32 67210000
&#EBCDIC(066) SETC T'#TESTX41                                 09/96 X32 67280000
&#EBCDIC(067) SETC T'#TESTX42                                 09/96 X32 67350000
&#EBCDIC(068) SETC T'#TESTX43                                 09/96 X32 67430000
&#EBCDIC(069) SETC T'#TESTX44                                 09/96 X32 67500000
&#EBCDIC(070) SETC T'#TESTX45                                 09/96 X32 67570000
&#EBCDIC(071) SETC T'#TESTX46                                 09/96 X32 67640000
&#EBCDIC(072) SETC T'#TESTX47                                 09/96 X32 67710000
&#EBCDIC(073) SETC T'#TESTX48                                 09/96 X32 67780000
&#EBCDIC(074) SETC T'#TESTX49                                 09/96 X32 67850000
&#EBCDIC(075) SETC T'#TESTX4A                                 09/96 X32 67930000
&#EBCDIC(076) SETC T'#TESTX4B                                 09/96 X32 68000000
&#EBCDIC(077) SETC T'#TESTX4C                                 09/96 X32 68070000
&#EBCDIC(078) SETC T'#TESTX4D                                 09/96 X32 68140000
&#EBCDIC(079) SETC T'#TESTX4E                                 09/96 X32 68210000
&#EBCDIC(080) SETC T'#TESTX4F                                 09/96 X32 68280000
&#EBCDIC(081) SETC '&&'                                                 68360000
&#EBCDIC(082) SETC T'#TESTX51                                 09/96 X32 68430000
&#EBCDIC(083) SETC T'#TESTX52                                 09/96 X32 68500000
&#EBCDIC(084) SETC T'#TESTX53                                 09/96 X32 68570000
&#EBCDIC(085) SETC T'#TESTX54                                 09/96 X32 68640000
&#EBCDIC(086) SETC T'#TESTX55                                 09/96 X32 68710000
&#EBCDIC(087) SETC T'#TESTX56                                 09/96 X32 68790000
&#EBCDIC(088) SETC T'#TESTX57                                 09/96 X32 68860000
&#EBCDIC(089) SETC T'#TESTX58                                 09/96 X32 68930000
&#EBCDIC(090) SETC T'#TESTX59                                 09/96 X32 69000000
&#EBCDIC(091) SETC T'#TESTX5A                                 09/96 X32 69070000
&#EBCDIC(092) SETC T'#TESTX5B                                 09/96 X32 69140000
&#EBCDIC(093) SETC T'#TESTX5C                                 09/96 X32 69210000
&#EBCDIC(094) SETC T'#TESTX5D                                 09/96 X32 69290000
&#EBCDIC(095) SETC T'#TESTX5E                                 09/96 X32 69360000
&#EBCDIC(096) SETC T'#TESTX5F                                 09/96 X32 69430000
&#EBCDIC(097) SETC T'#TESTX60                                 09/96 X32 69500000
&#EBCDIC(098) SETC T'#TESTX61                                 09/96 X32 69570000
&#EBCDIC(099) SETC T'#TESTX62                                 09/96 X32 69640000
&#EBCDIC(100) SETC T'#TESTX63                                 09/96 X32 69720000
&#EBCDIC(101) SETC T'#TESTX64                                 09/96 X32 69790000
&#EBCDIC(102) SETC T'#TESTX65                                 09/96 X32 69860000
&#EBCDIC(103) SETC T'#TESTX66                                 09/96 X32 69930000
&#EBCDIC(104) SETC T'#TESTX67                                 09/96 X32 70000000
&#EBCDIC(105) SETC T'#TESTX68                                 09/96 X32 70070000
&#EBCDIC(106) SETC T'#TESTX69                                 09/96 X32 70150000
&#EBCDIC(107) SETC T'#TESTX6A                                 09/96 X32 70220000
&#EBCDIC(108) SETC T'#TESTX6B                                 09/96 X32 70290000
&#EBCDIC(109) SETC T'#TESTX6C                                 09/96 X32 70360000
&#EBCDIC(110) SETC T'#TESTX6D                                 09/96 X32 70430000
&#EBCDIC(111) SETC T'#TESTX6E                                 09/96 X32 70500000
&#EBCDIC(112) SETC T'#TESTX6F                                 09/96 X32 70570000
&#EBCDIC(113) SETC T'#TESTX70                                 09/96 X32 70650000
&#EBCDIC(114) SETC T'#TESTX71                                 09/96 X32 70720000
&#EBCDIC(115) SETC T'#TESTX72                                 09/96 X32 70790000
&#EBCDIC(116) SETC T'#TESTX73                                 09/96 X32 70860000
&#EBCDIC(117) SETC T'#TESTX74                                 09/96 X32 70930000
&#EBCDIC(118) SETC T'#TESTX75                                 09/96 X32 71000000
&#EBCDIC(119) SETC T'#TESTX76                                 09/96 X32 71080000
&#EBCDIC(120) SETC T'#TESTX77                                 09/96 X32 71150000
&#EBCDIC(121) SETC T'#TESTX78                                 09/96 X32 71220000
&#EBCDIC(122) SETC T'#TESTX79                                 09/96 X32 71290000
&#EBCDIC(123) SETC T'#TESTX7A                                 09/96 X32 71360000
&#EBCDIC(124) SETC T'#TESTX7B                                 09/96 X32 71430000
&#EBCDIC(125) SETC T'#TESTX7C                                 09/96 X32 71510000
&#EBCDIC(126) SETC ''''''                                               71580000
&#EBCDIC(127) SETC T'#TESTX7E                                 09/96 X32 71650000
&#EBCDIC(128) SETC T'#TESTX7F                                 09/96 X32 71720000
&#EBCDIC(129) SETC T'#TESTX80                                 09/96 X32 71790000
&#EBCDIC(130) SETC T'#TESTX81                                 09/96 X32 71860000
&#EBCDIC(131) SETC T'#TESTX82                                 09/96 X32 71930000
&#EBCDIC(132) SETC T'#TESTX83                                 09/96 X32 72010000
&#EBCDIC(133) SETC T'#TESTX84                                 09/96 X32 72080000
&#EBCDIC(134) SETC T'#TESTX85                                 09/96 X32 72150000
&#EBCDIC(135) SETC T'#TESTX86                                 09/96 X32 72220000
&#EBCDIC(136) SETC T'#TESTX87                                 09/96 X32 72290000
&#EBCDIC(137) SETC T'#TESTX88                                 09/96 X32 72360000
&#EBCDIC(138) SETC T'#TESTX89                                 09/96 X32 72440000
&#EBCDIC(139) SETC T'#TESTX8A                                 09/96 X32 72510000
&#EBCDIC(140) SETC T'#TESTX8B                                 09/96 X32 72580000
&#EBCDIC(141) SETC T'#TESTX8C                                 09/96 X32 72650000
&#EBCDIC(142) SETC T'#TESTX8D                                 09/96 X32 72720000
&#EBCDIC(143) SETC T'#TESTX8E                                 09/96 X32 72790000
&#EBCDIC(144) SETC T'#TESTX8F                                 09/96 X32 72870000
&#EBCDIC(145) SETC T'#TESTX90                                 09/96 X32 72940000
&#EBCDIC(146) SETC T'#TESTX91                                 09/96 X32 73010000
&#EBCDIC(147) SETC T'#TESTX92                                 09/96 X32 73080000
&#EBCDIC(148) SETC T'#TESTX93                                 09/96 X32 73150000
&#EBCDIC(149) SETC T'#TESTX94                                 09/96 X32 73220000
&#EBCDIC(150) SETC T'#TESTX95                                 09/96 X32 73290000
&#EBCDIC(151) SETC T'#TESTX96                                 09/96 X32 73370000
&#EBCDIC(152) SETC T'#TESTX97                                 09/96 X32 73440000
&#EBCDIC(153) SETC T'#TESTX98                                 09/96 X32 73510000
&#EBCDIC(154) SETC T'#TESTX99                                 09/96 X32 73580000
&#EBCDIC(155) SETC T'#TESTX9A                                 09/96 X32 73650000
&#EBCDIC(156) SETC T'#TESTX9B                                 09/96 X32 73720000
&#EBCDIC(157) SETC T'#TESTX9C                                 09/96 X32 73800000
&#EBCDIC(158) SETC T'#TESTX9D                                 09/96 X32 73870000
&#EBCDIC(159) SETC T'#TESTX9E                                 09/96 X32 73940000
&#EBCDIC(160) SETC T'#TESTX9F                                 09/96 X32 74010000
&#EBCDIC(161) SETC T'#TESTXA0                                 09/96 X32 74080000
&#EBCDIC(162) SETC T'#TESTXA1                                 09/96 X32 74150000
&#EBCDIC(163) SETC T'#TESTXA2                                 09/96 X32 74230000
&#EBCDIC(164) SETC T'#TESTXA3                                 09/96 X32 74300000
&#EBCDIC(165) SETC T'#TESTXA4                                 09/96 X32 74370000
&#EBCDIC(166) SETC T'#TESTXA5                                 09/96 X32 74440000
&#EBCDIC(167) SETC T'#TESTXA6                                 09/96 X32 74510000
&#EBCDIC(168) SETC T'#TESTXA7                                 09/96 X32 74580000
&#EBCDIC(169) SETC T'#TESTXA8                                 09/96 X32 74650000
&#EBCDIC(170) SETC T'#TESTXA9                                 09/96 X32 74730000
&#EBCDIC(171) SETC T'#TESTXAA                                 09/96 X32 74800000
&#EBCDIC(172) SETC T'#TESTXAB                                 09/96 X32 74870000
&#EBCDIC(173) SETC T'#TESTXAC                                 09/96 X32 74940000
&#EBCDIC(174) SETC T'#TESTXAD                                 09/96 X32 75010000
&#EBCDIC(175) SETC T'#TESTXAE                                 09/96 X32 75080000
&#EBCDIC(176) SETC T'#TESTXAF                                 09/96 X32 75160000
&#EBCDIC(177) SETC T'#TESTXB0                                 09/96 X32 75230000
&#EBCDIC(178) SETC T'#TESTXB1                                 09/96 X32 75300000
&#EBCDIC(179) SETC T'#TESTXB2                                 09/96 X32 75370000
&#EBCDIC(180) SETC T'#TESTXB3                                 09/96 X32 75440000
&#EBCDIC(181) SETC T'#TESTXB4                                 09/96 X32 75510000
&#EBCDIC(182) SETC T'#TESTXB5                                 09/96 X32 75590000
&#EBCDIC(183) SETC T'#TESTXB6                                 09/96 X32 75660000
&#EBCDIC(184) SETC T'#TESTXB7                                 09/96 X32 75730000
&#EBCDIC(185) SETC T'#TESTXB8                                 09/96 X32 75800000
&#EBCDIC(186) SETC T'#TESTXB9                                 09/96 X32 75870000
&#EBCDIC(187) SETC T'#TESTXBA                                 09/96 X32 75940000
&#EBCDIC(188) SETC T'#TESTXBB                                 09/96 X32 76020000
&#EBCDIC(189) SETC T'#TESTXBC                                 09/96 X32 76090000
&#EBCDIC(190) SETC T'#TESTXBD                                 09/96 X32 76160000
&#EBCDIC(191) SETC T'#TESTXBE                                 09/96 X32 76230000
&#EBCDIC(192) SETC T'#TESTXBF                                 09/96 X32 76300000
&#EBCDIC(193) SETC T'#TESTXC0                                 09/96 X32 76370000
&#EBCDIC(194) SETC T'#TESTXC1                                 09/96 X32 76440000
&#EBCDIC(195) SETC T'#TESTXC2                                 09/96 X32 76520000
&#EBCDIC(196) SETC T'#TESTXC3                                 09/96 X32 76590000
&#EBCDIC(197) SETC T'#TESTXC4                                 09/96 X32 76660000
&#EBCDIC(198) SETC T'#TESTXC5                                 09/96 X32 76730000
&#EBCDIC(199) SETC T'#TESTXC6                                 09/96 X32 76800000
&#EBCDIC(200) SETC T'#TESTXC7                                 09/96 X32 76870000
&#EBCDIC(201) SETC T'#TESTXC8                                 09/96 X32 76950000
&#EBCDIC(202) SETC T'#TESTXC9                                 09/96 X32 77020000
&#EBCDIC(203) SETC T'#TESTXCA                                 09/96 X32 77090000
&#EBCDIC(204) SETC T'#TESTXCB                                 09/96 X32 77160000
&#EBCDIC(205) SETC T'#TESTXCC                                 09/96 X32 77230000
&#EBCDIC(206) SETC T'#TESTXCD                                 09/96 X32 77300000
&#EBCDIC(207) SETC T'#TESTXCE                                 09/96 X32 77380000
&#EBCDIC(208) SETC T'#TESTXCF                                 09/96 X32 77450000
&#EBCDIC(209) SETC T'#TESTXD0                                 09/96 X32 77520000
&#EBCDIC(210) SETC T'#TESTXD1                                 09/96 X32 77590000
&#EBCDIC(211) SETC T'#TESTXD2                                 09/96 X32 77660000
&#EBCDIC(212) SETC T'#TESTXD3                                 09/96 X32 77730000
&#EBCDIC(213) SETC T'#TESTXD4                                 09/96 X32 77800000
&#EBCDIC(214) SETC T'#TESTXD5                                 09/96 X32 77880000
&#EBCDIC(215) SETC T'#TESTXD6                                 09/96 X32 77950000
&#EBCDIC(216) SETC T'#TESTXD7                                 09/96 X32 78020000
&#EBCDIC(217) SETC T'#TESTXD8                                 09/96 X32 78090000
&#EBCDIC(218) SETC T'#TESTXD9                                 09/96 X32 78160000
&#EBCDIC(219) SETC T'#TESTXDA                                 09/96 X32 78230000
&#EBCDIC(220) SETC T'#TESTXDB                                 09/96 X32 78310000
&#EBCDIC(221) SETC T'#TESTXDC                                 09/96 X32 78380000
&#EBCDIC(222) SETC T'#TESTXDD                                 09/96 X32 78450000
&#EBCDIC(223) SETC T'#TESTXDE                                 09/96 X32 78520000
&#EBCDIC(224) SETC T'#TESTXDF                                 09/96 X32 78590000
&#EBCDIC(225) SETC T'#TESTXE0                                 09/96 X32 78660000
&#EBCDIC(226) SETC T'#TESTXE1                                 09/96 X32 78740000
&#EBCDIC(227) SETC T'#TESTXE2                                 09/96 X32 78810000
&#EBCDIC(228) SETC T'#TESTXE3                                 09/96 X32 78880000
&#EBCDIC(229) SETC T'#TESTXE4                                 09/96 X32 78950000
&#EBCDIC(230) SETC T'#TESTXE5                                 09/96 X32 79020000
&#EBCDIC(231) SETC T'#TESTXE6                                 09/96 X32 79090000
&#EBCDIC(232) SETC T'#TESTXE7                                 09/96 X32 79160000
&#EBCDIC(233) SETC T'#TESTXE8                                 09/96 X32 79240000
&#EBCDIC(234) SETC T'#TESTXE9                                 09/96 X32 79310000
&#EBCDIC(235) SETC T'#TESTXEA                                 09/96 X32 79380000
&#EBCDIC(236) SETC T'#TESTXEB                                 09/96 X32 79450000
&#EBCDIC(237) SETC T'#TESTXEC                                 09/96 X32 79520000
&#EBCDIC(238) SETC T'#TESTXED                                 09/96 X32 79590000
&#EBCDIC(239) SETC T'#TESTXEE                                 09/96 X32 79670000
&#EBCDIC(240) SETC T'#TESTXEF                                 09/96 X32 79740000
&#EBCDIC(241) SETC T'#TESTXF0                                 09/96 X32 79810000
&#EBCDIC(242) SETC T'#TESTXF1                                 09/96 X32 79880000
&#EBCDIC(243) SETC T'#TESTXF2                                 09/96 X32 79950000
&#EBCDIC(244) SETC T'#TESTXF3                                 09/96 X32 80020000
&#EBCDIC(245) SETC T'#TESTXF4                                 09/96 X32 80100000
&#EBCDIC(246) SETC T'#TESTXF5                                 09/96 X32 80170000
&#EBCDIC(247) SETC T'#TESTXF6                                 09/96 X32 80240000
&#EBCDIC(248) SETC T'#TESTXF7                                 09/96 X32 80310000
&#EBCDIC(249) SETC T'#TESTXF8                                 09/96 X32 80380000
&#EBCDIC(250) SETC T'#TESTXF9                                 09/96 X32 80450000
&#EBCDIC(251) SETC T'#TESTXFA                                 09/96 X32 80520000
&#EBCDIC(252) SETC T'#TESTXFB                                 09/96 X32 80600000
&#EBCDIC(253) SETC T'#TESTXFC                                 09/96 X32 80670000
&#EBCDIC(254) SETC T'#TESTXFD                                 09/96 X32 80740000
&#EBCDIC(255) SETC T'#TESTXFE                                 09/96 X32 80810000
&#EBCDIC(256) SETC T'#TESTXFF                                 09/96 X32 80880000
         AGO   .GENLP                                                   00064100
.GNTEBCD MNOTE 8,'ERROR - &&GEN(&A1)=&GEN(&A1) IS UNRECOGNIZED'         00064200
         AGO   .GENLP                                                   00064300
.GENEND  ANOP                                                           00064400
.*                                                                      00064500
.*                                                                      00064600
.*                                                                      00064700
.END     MEND                                                           00064800
         MACRO                                                          00000100
         #USING &D                                                      00000200
.*                                                                      00000300
.*                                                                      00000400
.*                                                                      00000500
.* LAST CHANGE DATE - OCTOBER 18, 1983                                  00000600
.*                  - MAILING ADDRESS CHANGE                            00000700
.*                                                                      00000800
.* LAST CHANGE DATE - APRIL 21, 1981                                    00000900
.*                  - MACRO NAME CHANGED FROM $USING TO #USING.         00001000
.*                                                                      00001100
.* LAST CHANGE DATE - FEBRUARY 2, 1977                                  00001200
.*                  - MAILING ADDRESS CHANGE.                           00001300
.*                                                                      00001400
.* LAST CHANGE DATE - AUGUST 23, 1976                                   00001500
.*                                                                      00001600
.* THIS MACRO WAS WRITTEN BY DAVID B. COLE. ANY QUESTIONS CONCERNING    00001700
.* IT MAY BE ADDRESSED TO:                                              00001800
.*       RR#2 BOX 712                                                   00001900
.*       AFTON, VA. 22920                                               00002000
.*                                                                      00002100
.*                                                                      00002200
.*                                                                      00002300
.*   THIS MACRO GENERATES A USING INSTRUCTION THAT REDECLARES ALL BASES 00002400
.* (IF ANY) DECLARED BY A PRIOR #ENTER MACRO EXPANSION.                 00002500
.*                                                                      00002600
.*                                                                      00002700
.*                                                                      00002800
.* INNER MACROS USED - NONE                                             00002900
.*                                                                      00003000
         GBLC  &#BS(14)                                                 00003100
         LCLA  &A1,&DISPL                                               00003200
         AIF   ('&#BS(14)' EQ '').END                                   00003300
&DISPL   SETA  &DISPL-4095                                              00003400
&A1      SETA  14                                                       00003500
.LP      AIF   (&A1 EQ 2).END                                           00003600
&A1      SETA  &A1-1                                                    00003700
         AIF   ('&#BS(&A1)' EQ '').LP                                   00003800
&DISPL   SETA  &DISPL+4095                                              00003900
         USING &#BS(14)+&DISPL,&#BS(&A1)                                00004000
         AGO   .LP                                                      00004100
.END     MEND                                                           00004200
OFFL     TITLE 'OFFLOAD -- COMMENTS '                                   00010000
*$DOC@***************************************************************   00020000
*                                                                   *   00030000
* OFFLOAD - UNLOAD A PDS TO SEQUENTIAL IEBUPDTE DATA SET            *   00040000
*                                                                   *   00050000
* OBTAINED FROM SHARE MVS MODS TAPE, VERSION 81-013, FILE 85        *   00060000
*                                                                   *   00070000
* USER MACROS ARE ON SAME TAPE, FILE 88.                            *   00080000
*                                                                   *   00090000
* FIXED BUG 103-4C ABEND PROCESSING UNBLOCKED PDS                   *   00100000
* BY ADDING BUFL=256 TO DIRCKW DCB.                                 *   00110000
*                                                                   *   00120000
*  DDNAMES REQUIRED:                                                *   00130000
*      SYSPRINT - OUTPUT MESSAGES                                   *   00140000
*      SYSIN    - INPUT CONTROL STATEMENT                           *   00150000
*      2 OTHER DDNAMES, DEFINED ON THE CONTROL STATEMENT            *   00160000
*                                                                   *   00170000
*  CONTROL STATEMENT FORMAT IS: (STARTING AFTER COLUMN 1)           *   00180000
*                                                                   *   00190000
*                O I=INDD,O=OUTDD,T=IEBUPDTE                        *   00200000
*                                                                   *   00210000
*                S M=XXXX    (SELECT MEMBER)                        *   00220000
*                                                                   *   00230000
*                E M=XXX     (EXCLUDE MEMBER)                       *   00240000
*                                                                   *   00250000
*                         O R                                       *   00260000
*                                                                   *   00270000
*                O I=INDD,O=OUTDD  (IN WHICH CASE YOU WILL NOT      *   00280000
*                                  GET IEBUPDTE ./ CARDS IN THE     *   00290000
*                                  STREAM)                          *   00300000
*                                                                   *   00310000
*  IF T=IEBUPDTE IS SPECIFIED AND A RECORD IS FOUND RESEMBLING AN   *   00320000
*  IEBUPDTE CONTROL STATEMENT WITHIN A MEMBER, THE './' IN THE      *   00330000
*  RECORD WILL BE CHANGED TO '><' AND A WARNING MESSAGE ISSUED.     *   00340000
*                                                                   *   00350000
*$DOC$***************************************************************   00360000
         TITLE 'OFFLOAD -- MISCELLANIOUS EQUATES'                       00370000
         #REGS R                                                        00380000
         SPACE 3                                                        00390000
LINECT   EQU   58                  LINES PER PAGE                       00400000
OSCORE   EQU   4096                SIZE OF OS ELBOW ROOM                00410000
BUFSPACE EQU   1000                APPROXIMATE REASONABLE BUF SPACE     00420000
         SPACE 3                                                        00430000
         #PUT  SUBAD=PUTSYSP,MF=INIT                                    00440000
         TITLE 'OFFLOAD -- DSECTS'                                      00450000
         PRINT NOGEN                                                    00460000
         SPACE 3                                                        00470000
CVT      DSECT                                                          00480000
         CVT                                                            00490000
         SPACE 3                                                        00500000
         IKJTCB                                                         00510000
         SPACE 3                                                        00520000
TIOT     DSECT                                                          00530000
         IEFTIOT1                                                       00540000
         SPACE 3                                                        00550000
DCB      DSECT                                                          00560000
         #DCBD DSORG=(QS,PO)                                            00570000
         SPACE 3                                                        00580000
OPENWORK DSECT ,                                                        00590000
         ORG   OPENWORK+100                                             00600000
         IEFJFCBN                                                       00610000
         SPACE 3                                                        00620000
         PRINT GEN                                                      00630000
         TITLE 'OFFLOAD -- DSECTS - DATA EVENT CONTROL BLOCK'           00640000
DECB     DSECT ,                                                        00650000
DECBECB  DS    A                   EVENT CONTROL BLOCK                  00660000
DECBTYPE DS    2X                  FLAG BYTES                           00670000
DECBLGTH DS    Y                   KEY PLUS DATA LENGTH                 00680000
DECBDCB  DS    A                   DCB POINTER                          00690000
DECBBUFR DS    A                   BUFFER POINTER                       00700000
DECBIOB  DS    A                   IOB POINTER                          00710000
DECBLINK DS    A                   LINK TO RELATED DECB                 00720000
DECBLEN  EQU   *-DECB              DECB LENGTH                          00730000
         TITLE 'OFFLOAD -- DSECTS - PROGRAM INTERRUPT ELEMENT'          00740000
PIE      DSECT ,                                                        00750000
PIEPICA  DS    A                   PICA PTR                             00760000
PIEOPSW  DS    XL8                 OLD PSW                              00770000
PIER14   DS    A                   R14 SAVE AREA                        00780000
PIER15   DS    A                   R15 SAVE AREA                        00790000
PIER0    DS    A                   R0 SAVE AREA                         00800000
PIER1    DS    A                   R1 SAVE AREA                         00810000
PIER2    DS    A                   R2 SAVE AREA                         00820000
         TITLE 'OFFLOAD -- INITIAL ENTRY'                               00830000
OFFLOAD  #ENTER BASES=3,SAVTYPE=(LOCAL,BASE)                            00840000
         SPACE 1                                                        00850000
* REINITIALIZE DATA FOR SERIAL REUSABILITY                              00860000
         SP    PAGECTR,PAGECTR                                          00870000
         XC    LINES2GO,LINES2GO                                        00880000
         MVI   OUTDDNAM,C' '                                            00890000
         MVI   RCD+1,0                                                  00900000
         MVI   FLAG,0                                                   00910000
         SPACE 1                                                        00920000
* OPEN UNIT RECORD DATA SETS                                            00930000
         OPEN  (SYSIN,,SYSPRINT,OUTPUT)                                 00940000
         TM    PRTOFLGS,DCBOFOPN   SYSPRINT OPENED OK?                  00950000
         BZ    EXIT16              NO, TERMINATE                        00960000
         #PUT  TITLE1              YES, SET UP A TITLE                  00970000
         TM    RDROFLGS,DCBOFOPN   SYSIN OPENED OK?                     00980000
         BO    CONTROL             YES, CONTINUE                        00990000
         #PUT  NOSYSIN             NO, PRINT AN ERROR MSG               01000000
         B     EXIT16              GO TERMINATE                         01010000
         TITLE 'OFFLOAD -- ANALYZE THE SYSIN CONTROL CARDS'             01020000
* GET WORKING STORAGE                                                   01030000
CONTROL  TM    RDROFLGS,DCBOFOPN   READER STILL OPENED?                 01040000
         BZ    EOP                 NO, END OF PROGRAM                   01050000
         GETMAIN VC,LA=GETMQTY,A=WORKSTAR YES, GET WORKING CORE         01060000
         LTR   R15,R15             ANY GOTTEN?                          01070000
         BZ    COREOK              YES, CONTINUE                        01080000
         #PUT  NOCOREM             NO, ISSUE ERROR MSG                  01090000
         B     EXIT16              TERMINATE                            01100000
COREOK   LM    R0,R1,WORKSTAR      GET CORE PTR & LEN                   01110000
         AR    R1,R0               PNT TO CORE END                      01120000
         LH    R0,=Y(OSCORE)       GET OS ELBOW ROOM SIZE               01130000
         SR    R1,R0               GET END OF CORE TO KEEP              01140000
         ST    R1,WORKEND          SAVE FOR LATER                       01150000
         FREEMAIN R,LV=(0),A=(1)   GIVE OS SOME ELBOW ROOM              01160000
         TM    FLAG,UNLOAD         NEXT CARD ALREADY READ?              01170000
         MVI   FLAG,0              (CLEAR FLAGS IN ANY CASE)            01180000
         BO    GOTCARD             YES, GO PROCESS IT                   01190000
         SPACE 1                                                        01200000
* SETUP TO SKIP TO NEXT "O" CARD                                        01210000
RESTART  MVI   FLAG,0              CLEAR FUNCTION FLAGS                 01220000
GETCARD1 MVI   FLAG2,0             CLEAR SYNTAX FLAGS                   01230000
         MVI   PRECARD,C' '        CLEAR ERROR -                        01240000
         MVC   PRECARD+1(L'PRECARD-2),PRECARD  MSG AREA                 01250000
         SPACE 1                                                        01260000
* GET THE NEXT CARD AND CHECK IT FOR SUBSTANCE                          01270000
GETCARD2 GET   SYSIN,CARD          GET THE NEXT CARD                    01280000
         CLC   CARD(71),CARD-1     CARD BLANK?                          01290000
         BE    GETCARD2            YES, IGNORE IT                       01300000
         CLI   CARD,C'*'           NO, COMMENT CARD?                    01310000
         BE    CARDECHO            YES, GO PRINT IT                     01320000
         SPACE 1                                                        01330000
* SEARCH FOR AND IDENTIFY THE CARD'S VERB                               01340000
GOTCARD  LA    R3,CARD-1           GET CARD COLUMN SCANNER              01350000
         LA    R4,1                GET BXLE INCREMENT                   01360000
         LA    R5,CARD+70          GET BXLE LIMIT                       01370000
NAMELP   BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN                  01380000
         CLI   0(R3),C' '          BLANK YET?                           01390000
         BNE   NAMELP              NO, STILL THE NAME FIELD             01400000
VERBLP   BXH   R3,R4,CARDSYNT      YES, SCAN TO NEXT COLUMN             01410000
         CLI   0(R3),C' '          STILL BLANK?                         01420000
         BE    VERBLP              YES, LOOP BACK                       01430000
         LA    R3,1(,R3)           NO, POINT PAST THE VERB              01440000
         CLI   0(R3),C' '          VERB MUST BE A SINGLE CHARACTER      01450000
         BNE   CARDSYNT            IT'S NOT; ERROR                      01460000
         BCTR  R3,0                IT IS; POINT BACK TO IT              01470000
         CLI   0(R3),C'O'          "OFFLOAD" CARD?                      01480000
         BE    VO                  YES, GO PROCESS                      01490000
         TM    FLAG,UNLOAD         NO, MUST IT BE?                      01500000
         BZ    CARDIGNR            YES, IGNORE THE CARD                 01510000
         CLI   0(R3),C'S'          NO, SELECT CARD?                     01520000
         BE    VS                  YES, GO PROCESS                      01530000
         CLI   0(R3),C'E'          NO, EXCLUDE CARD?                    01540000
         BNE   CARDSYNT            NO, ERROR                            01550000
         TM    FLAG,SELECT         YES, ALREADY SELECT CARDS?           01560000
         BO    CARDNCAP            YES, ERROR                           01570000
         OI    FLAG,EXCLUDE        NO, REMEMBER EXCLUDE                 01580000
         B     GETOPND             GO GET OPERANDS                      01590000
VS       TM    FLAG,EXCLUDE        SELECT GIVEN; PREVIOUS EXCLUDE?      01600000
         BO    CARDNCAP            YES, ERROR                           01610000
         OI    FLAG,SELECT         NO, REMEMBER SELECT                  01620000
         B     GETOPND             GO GET OPERANDS                      01630000
VO       TM    FLAG,UNLOAD         OFFLOAD GIVEN; ALREADY GOT ONE?      01640000
         BO    PROCESS             YES, GO PERFORM FUNCTION             01650000
         OI    FLAG,UNLOAD         NO, GOT ONE NOW                      01660000
         #PUT  BLNK3               PUT SOME BLANK LINES                 01670000
         MVI   PRECARD,C'-'        ECHO CARRAGE CONTROL                 01680000
         SPACE 1                                                        01690000
* SEARCH FOR OPERANDS                                                   01700000
GETOPND  BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN                  01710000
         CLI   0(R3),C' '          STILL BLANK?                         01720000
         BE    GETOPND             YES, LOOP BACK                       01730000
         LA    R3,1(,R3)           NO, PNT PAST KEYWORD                 01740000
         CLI   0(R3),C'='          KEYWD MUST BE 1 CHARACTER LONG       01750000
         BNE   CARDSYNT            IT'S NOT; ERROR                      01760000
         BCTR  R3,0                IT IS; POINT BACK TO IT              01770000
         ST    R3,SAVESCAN         SAVE THE POINTER ICO ERROR           01780000
         NI    FLAG2,255-DDNAME    CLEAR DDN FLAG                       01790000
         TM    FLAG,SELECT+EXCLUDE UNLOAD CARD?                         01800000
         BNZ   NOTUNLD             NO, SKIP                             01810000
         SPACE 1                                                        01820000
* IDENTIFY UNLOAD CARD OPERANDS                                         01830000
         CLI   0(R3),C'T'          "T="?                                01840000
         BE    TOPND               YES, GO PROCESS                      01850000
         OI    FLAG2,DDNAME        NO, OPND OBJECTS MUST BE DDNAMES     01860000
         CLI   0(R3),C'I'          "I="?                                01870000
         BE    IOPND               YES, GO PROCESS                      01880000
         CLI   0(R3),C'O'          NO, "O="?                            01890000
         BNE   CARDSYNT            NO, ERROR                            01900000
         SPACE 1                                                        01910000
* PROCESS THE "O=" OPERAND                                              01920000
         TM    FLAG2,GOTOUT        YES, ALREADY GOT "O="?               01930000
         BO    CARDRDUN            YES, ERROR                           01940000
         OI    FLAG2,GOTOUT        NO, GOT ONE NOW                      01950000
         BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN                  01960000
         BAL   R14,GETNAME         SCAN OUT THE DDNAME                  01970000
         LA    R2,OUT              POINT TO THE OUTPUT DCB              01980000
         BAL   R14,CLOSE           INSURE THAT IT IS CLOSED             01990000
         MVC   OUTDDNAM,WORKAREA   RESET THE DCB'S DDNAME               02000000
         B     OPNDNEXT            GO SCAN FOR NEXT OPERAND             02010000
         SPACE 1                                                        02020000
* PROCESS THE "T=" OPERAND                                              02030000
TOPND    TM    FLAG2,GOTTYPE       "T=" ALREADY ENCOUNTERED?            02040000
         BO    CARDRDUN            YES, REDUNDANCY ERROR                02050000
         OI    FLAG2,GOTTYPE       NO, REMEMBER ICO ANOTHER             02060000
         BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN                  02070000
         BAL   R14,GETNAME         GO VERIFY SYNTAX                     02080000
         LR    R14,R3              SAVE EOD OF OPND PTR                 02090000
         L     R3,SAVESCAN         PNT BACK TO START OF OPERAND         02100000
         LA    R15,=C'IEBUPD'      PNT TO VALID PREFIXES                02110000
         LA    R1,5                GET LOOP CONTROL                     02120000
IEBLOOP  STC   R1,IEBCLC+1         SET CLC LENGTH FIELD                 02130000
         NOPR  0                   (FOR CP-67 TRACE)                    02140000
IEBCLC   CLC   0(*-*,R3),0(R15)    PREFIX OK?                           02150000
         BE    IEBOK1              YES, PROCEED                         02160000
         LA    R15,1(,R15)         NO, PNT TO NEXT VALID PREFIX         02170000
         BCTR  R1,0                GET ITS MACHINE LENGTH               02180000
         LTR   R1,R1               ALL VALID PREFIXES TESTED?           02190000
         BNM   IEBLOOP             NOPE, GO TEST THIS ONE               02200000
IEBOK1   LA    R3,1(R1,R3)         YEP, ASSUME NO PREFIX; PNT PAST PFX  02210000
         CLC   0(2,R3),=C'TE'      "T=IEBUPDTE"?                        02220000
         BNE   IEBTRY2             NO, SKIP                             02230000
         OI    FLAG,UPDTE          YES, REMEMBER                        02240000
         B     IEBOK2              DONE HERE                            02250000
IEBTRY2  CLC   0(2,R3),=C'AT'      NO, "T=IEBUPDAT"?                    02260000
         BNE   CARDSYNT            NO, SYNTAX ERROR                     02270000
         OI    FLAG,UPDAT          YES, REMEMBER IT                     02280000
IEBOK2   LA    R3,2(,R3)           PNT PAST "TE"/"AT"                   02290000
         CR    R14,R3              AT END OF OPERAND?                   02300000
         BNE   CARDSYNT            NO, ERROR                            02310000
         B     OPNDNEXT            YES, GO SCAN NEXT OPERAND            02320000
         SPACE 1                                                        02330000
* PROCESS THE "I=" OPERAND                                              02340000
IOPND    TM    FLAG2,GOTIN         ALREADY GOT "I="?                    02350000
         BO    CARDRDUN            YES, ERROR                           02360000
         OI    FLAG2,GOTIN         NO, GOT ONE NOW                      02370000
         L     R0,WORKSTAR         GET START OF INPUT DDNAMES LIST      02380000
         LR    R1,R0               INITIALIZE IT TO EMPTY               02390000
         BAL   R14,PUTNAMES        GO BUILD THE DDNAMES LIST            02400000
         ST    R1,ENDDDNS          SAVE END OF DDNAMES LIST             02410000
         SPACE 1                                                        02420000
* CHECK FOR MORE UNLOAD STATEMENT OPERANDS                              02430000
OPNDNEXT CLI   0(R3),C','          MORE OPERANDS?                       02440000
         BNE   CARDEND             NO, SKIP                             02450000
         CLI   1(R3),C' '          YES, CONTU CARD NEEDED?              02460000
         BNE   GETOPND             NO, JUST GO GET NEXT OPERAND         02470000
         BAL   R14,NEXTCARD        YES, GET NEXT CARD                   02480000
         B     GETOPND             GO GET NEXT OPERAND                  02490000
CARDEND  TM    FLAG2,GOTIN         "I=" GIVEN?                          02500000
         BZ    CARDMISO            NO, ERROR                            02510000
         MVC   ENDMNMS,ENDDDNS     INITIALIZE MEMBER NAMES LIST EMPTY   02520000
         TM    OUTOFLGS,DCBOFOPN   "O=" OPERAND REQUIRED?               02530000
         BO    CARDECHO            NO, GO ECHO THE CARD                 02540000
         CLI   OUTDDNAM,C' '       YES, "O=" GIVEN?                     02550000
         BE    CARDMISO            NO, ERROR                            02560000
         B     CARDECHO            YES, GO ECHO THE CARD                02570000
         SPACE 1                                                        02580000
* PROCESS S/E CARD OPERANDS                                             02590000
NOTUNLD  CLI   0(R3),C'M'          "M="?                                02600000
         BNE   CARDSYNT            NO, ERROR                            02610000
         L     R0,ENDDDNS          YES, PNT TO START OF MBR NMES LIST   02620000
         L     R1,ENDMNMS          PNT TO END OF MBR NMES LIST          02630000
         BAL   R14,PUTNAMES        GO GET MORE NAMES FROM THIS CARD     02640000
         ST    R1,ENDMNMS          STORE NEW END OF LIST                02650000
         CLI   0(R3),C' '          MORE OPERANDS?                       02660000
         BNE   CARDSYNT            ERROR; THERE ARE MORE OPERANDS       02670000
         SPACE 1                                                        02680000
* ECHO PRINT THE CONTROL CARD                                           02690000
CARDECHO #PUT  PRECARD             ECHO THE GOOD CARD                   02700000
         B     GETCARD1            GO GET NEXT CARD                     02710000
         SPACE 1                                                        02720000
* SYSIN END OF DATA ROUTINE                                             02730000
RDREOD   LA    R2,SYSIN            POINT TO THE DCB                     02740000
         BAL   R14,CLOSE           GO CLOSE THE DCB                     02750000
         TM    FLAG2,CONTINUE      READING CONTINU CARD?                02760000
         BZ    EODOK               NO, AOK                              02770000
         #PUT  EODERR              YES, ERROR                           02780000
         MVI   RCD+1,8             SET COMPLETION CODE                  02790000
         B     EOP                 END OF PROGRAM                       02800000
EODOK    TM    FLAG,UNLOAD         ANYTHING LEFT TO DO?                 02810000
         BZ    EOP                 NO, GO TERMINATE                     02820000
         TITLE 'OFFLOAD -- PREPARE TO PERFORM THE CURRENT OFFLOAD FUNCT*02830000
               ION'                                                     02840000
* INSERT THE MEMBER NAMES LIST TRAILER                                  02850000
PROCESS  L     R0,WORKEND          PNT TO END OF WORK AREA              02860000
         L     R14,ENDMNMS         PNT TO END OF USED PART              02870000
         LA    R1,L'DCBDDNAM(,R14) MAKE ROOM FOR TRAILER ENTRY          02880000
         SR    R0,R1               ENOUGH ROOM?                         02890000
         BNM   NOTFULL             YES, CONTINUE                        02900000
         #PUT  NOCOREM             NO, ISSUE ERROR MSG                  02910000
         B     RCD8                LOOP TO NEXT FUNCTION                02920000
         SPACE 1                                                        02930000
* RELEASE ANY UNUSED PART OF THE WORK AREA                              02940000
NOTFULL  MVC   0(L'DCBDDNAM,R14),EFFS GET TRAILER ENTRY                 02950000
         ST    R1,WORKEND          SET NEW END OF WORK AREA             02960000
         BZ    NOFREE              NONE, SKIP                           02970000
         FREEMAIN R,LV=(0),A=(1)   SOME; FREE IT                        02980000
         SPACE 1                                                        02990000
* SORT THE MEMBER NAMES LIST                                            03000000
NOFREE   L     R1,ENDDDNS          PNT TO START OF MEMBER NAMES         03010000
         L     R15,WORKEND         PNT TO END OF SAME                   03020000
         LA    R8,L'DCBDDNAM       GET ENTRY LENGTH                     03030000
         SR    R15,R1              GET LENGTH OF LIST                   03040000
         LR    R6,R8               GET ENTRY LENGTH (EL)                03050000
SORTSLL  AR    R6,R6               GET EL*(2**N)                        03060000
         CR    R6,R15              LIST LEN EXCEED YET?                 03070000
         BNH   SORTSLL             NO, LOOP                             03080000
         SR    R6,R8               YES, GET EL*((2**N)-1)               03090000
SORTLP1  SR    R6,R8                                                    03100000
         BZ    SORTED                                                   03110000
         SRL   R6,1                                                     03120000
         LR    R9,R15                                                   03130000
         SR    R9,R6                                                    03140000
         LR    R7,R8                                                    03150000
SORTLP2  LR    R4,R8                                                    03160000
SORTLP3  LA    R2,0(R7,R1)                                              03170000
         SR    R2,R4               GET 1ST COMPARE LOCATION             03180000
         LA    R3,0(R6,R2)         GET 2ND COMPARE LOCATION             03190000
         CLC   0(L'DCBDDNAM,R2),0(R3) SHOULD THESE 2 ELE BE SWITCHED?   03200000
         BNH   SORTNLP2            NO, SKIP                             03210000
         XC    0(L'DCBDDNAM,R2),0(R3) YES, -                            03220000
         XC    0(L'DCBDDNAM,R3),0(R2)  DO -                             03230000
         XC    0(L'DCBDDNAM,R2),0(R3)   SO                              03240000
         BXLE  R4,R6,SORTLP3       LOOP TO SEE IF ELE IS IN RIGHT PLACE 03250000
SORTNLP2 BXLE  R7,R8,SORTLP2       RIGHT PLACE, LOOP 4 NXT ELE          03260000
         B     SORTLP1             LOOP FOR NEXT LEVEL                  03270000
         SPACE 1                                                        03280000
* OPEN THE OUTPUT DATA SET                                              03290000
SORTED   TM    OUTOFLGS,DCBOFOPN   OUTPUT DS ALREADY OPEN?              03300000
         BO    OUTOPEND            YES, SKIP                            03310000
         MVC   OFFLDDNS,OFFLDDNS-1 NO, CLEAR INFO MSG                   03320000
         MVC   OFFLDDNS(L'DCBDDNAM),OUTDDNAM GET OUTPUT DDN TO MSG      03330000
         LA    R1,OFFLDDNS         SETUP FOR BLANK SCAN                 03340000
OFFLBLNK LA    R1,1(,R1)           PNT TO NEXT COLUMN                   03350000
         CLI   0(R1),C' '          BLANK YET?                           03360000
         BNE   OFFLBLNK            NO, LOOP                             03370000
         ST    R1,OFFLPTR          YES, REMEMBER THIS SPOT              03380000
         MVC   1(4,R1),=C'FROM'    INSERT MORE MSG TEXT                 03390000
         OPEN  (OUT,OUTPUT)        NO, OPEN IT                          03400000
* NOTE - THE CONTROL CARD SCAN HAS ALREADY VERIFIED THAT THIS OUTPUT DD 03410000
* CARD EXISTS.                                                          03420000
OUTOPEND L     R10,WORKSTAR        PNT TO INPUT DDNAMES                 03430000
         TITLE 'OFFLOAD -- OFFLOAD (SELECTED) PDS MEMBERS'              03440000
* REGISTER USAGE IN THIS SECTION                                        03450000
*        R10 --> DDNAME OF CURRENT INPUT DATA SET                       03460000
*        R7 --> CURRENT DIRECTORY ENTRY                                 03470000
*        R6 --> NEXT MEMBER NAME TO BE SELECTED OR EXCLUDED             03480000
*        R2 --> CURRENT INPUT DECB                                      03490000
         SPACE 1                                                        03500000
* OPEN AN INPUT DATA SET                                                03510000
DATSETLP SR    R0,R0               GET A ZERO                           03520000
         STH   R0,INBLKSI          INSURE BLKSIZE CLEAR                 03530000
         STH   R0,INBUFL           INSURE BUFL CLEAR                    03540000
         STC   R0,INBUFNO          INSURE BUFNO CLEAR                   03550000
         MVC   INDDNAM,0(R10)      GET INPUT DDNAME                     03560000
         L     R1,OFFLPTR          PNT TO INFO MSG TEXT                 03570000
         MVC   6(L'DCBDDNAM,R1),0(R10) GET INPUT DDNAME                 03580000
         NI    FLAG,255-NOTPDS     ASSUME DS IS PARTITIONED             03590000
         OPEN  IN                  OPEN THE INPUT DATA SET              03600000
         TM    FLAG,NOTPDS         PDS?                                 03610000
         BZ    ISPDS               YES, AOK                             03620000
         LA    R1,2+L'DCBDDNAM+SEQFAKLN NO, -                           03630000
         STH   R1,RDAREA            FAKE A -                            03640000
         MVC   RDAREA+2(L'DCBDDNAM),0(R10) DIRECTORY -                  03650000
         MVC   RDAREA+2+L'DCBDDNAM(SEQFAKLN),SEQFAKRY BLOCK.            03660000
         B     SEQFAKIT            GO PROCESS AS A SINGLE MEMBER        03670000
* NOTE - THE CONTROL CARD SCAN HAS ALREADY VERIFIED THAT THIS INPUT DD  03680000
* CARD EXISTS.                                                          03690000
         SPACE 1                                                        03700000
* PARTITIONED INPUT. OPEN THE DIRECTORY                                 03710000
ISPDS    MVC   DRCDDNAM,0(R10)     GET THE DDNAME                       03720000
         OPEN  DRCTY               OPEN THE DIRECTORY                   03730000
         SPACE 1                                                        03740000
* SETUP FOR DIRECTORY BLOCK READ AND SCAN                               03750000
         XC    RDAREA(2),RDAREA    FORCE INITIAL DRCTY BLK READ         03760000
SEQFAKIT L     R6,ENDDDNS          PNT TO START OF MEMBER NAMES         03770000
DRCTYLP  LH    R1,RDAREA           GET USED LENGTH OF DRCTY BLK         03780000
         LA    R1,RDAREA(R1)       PNT PAST THE USED AREA               03790000
         ST    R1,DRCTYEND         SAVE FOR LATER                       03800000
         MVI   RDAREA+1,0          SETUP TO ADVANCE TO 1ST ENTRY        03810000
         LA    R7,RDAREA-10        SETUP TO ADVANCE TO 1ST ENTRY        03820000
         SPACE 1                                                        03830000
* SCAN THE DIRECTORY BLOCK FOR THE NEXT DIRECTORY ENTRY                 03840000
MEMBERLP NI    11(R7),X'1F'        ISOLATE ENTRY LENGTH                 03850000
         SR    R1,R1               CLEAR FOR 'IC'                       03860000
         IC    R1,11(,R7)          GET ENTRY 'LENGTH'                   03870000
         LA    R1,12(R1,R1)        GET REAL ENTRY LENGTH                03880000
         AR    R7,R1               PNT TO NEXT DIRECTORY ENTRY          03890000
         C     R7,DRCTYEND         END OF THIS BLOCK?                   03900000
         BL    FINDMBR             NO, CONTINUE PROCESSING THIS BLK     03910000
         SPACE 1                                                        03920000
* READ THE NEXT DIRECTORY BLOCK. HANDLE EOD OR I/O ERRORS               03930000
         GET   DRCTY,RDAREA        READ THE DIRECTORY BLOCK             03940000
         B     DRCTYLP             LOOP TO SCAN IT                      03950000
DRCERR   MVC   DRCTYERR+L'DRCTYERR(L'DCBDDNAM),0(R10) NO, GET DDNAME    03960000
         #PUT  DRCTYERR            PRINT ERROR MSG                      03970000
         MVI   RCD+1,8             SET COMPLETION CODE                  03980000
         B     DSDONE              SKIP TO NEXT DATA SET                03990000
         SPACE 1                                                        04000000
* DETERMINE IF THE MEMBER POINTED TO BY THE CURRENT DIRECTORY ENTRY     04010000
* SHOULD BE OFFLOADED                                                   04020000
FINDMBR  CLC   0(L'DCBDDNAM,R7),EFFS EO DRCTY 4 THIS DS?                04030000
         BE    DSDONE              YES, SKIP OUT                        04040000
NMEGOTST CLC   0(L'DCBDDNAM,R7),0(R6) NO, CURRENT NAME IN THE LIST?     04050000
         BL    NOGOTNME            NO, GO CHECK FUNCTION                04060000
         BE    GOTNME              YES, GO CHECK FUNCTION               04070000
         LA    R6,L'DCBDDNAM(,R6)  DON'T KNOW; ADVANCE LIST SCANNER     04080000
         B     NMEGOTST            GO TEST NAME AGAIN                   04090000
NOGOTNME TM    FLAG,SELECT         SELECT FUNCTION?                     04100000
         BO    MEMBERLP            YES, SKIP THIS MEMBER                04110000
         B     GOTMEMBR            NO, OFFLOAD THIS MEMBER              04120000
GOTNME   TM    FLAG,EXCLUDE        EXCLUDE FUNCTION?                    04130000
         BO    MEMBERLP            YES, SKIP THIS MEMBER                04140000
         XC    0(L'DCBDDNAM,R6),0(R6) NO, CLR NME FROM LIST             04150000
         SPACE 1                                                        04160000
* MEMBER LOCATED - TELL USER ABOUT IT                                   04170000
GOTMEMBR NI    FLAG,255-CTLCDERR   CLEAR IEBUPDTE CTL CD FND ERR FLG    04180000
         MVC   OFFLMBRN,0(R7)      PNT MEMBER NAME INTO INFO MSG        04190000
         #PUT  OFFLMSG             TELL USER WHAT'S HAPPENING           04200000
         SPACE 1                                                        04210000
* POINT THE INPUT DATA SET TO THE MEMBER TO BE OFFLOADED                04220000
         TM    FLAG,NOTPDS         SEQUENTIAL DS?                       04230000
         BO    SKIPFIND            YES, FORGET ABOUT "FIND"             04240000
         MVC   BLDLTTR0(3),8(R7)   SET TTR OF TTR0                      04250000
         POINT IN,BLDLTTR0         POINT TO THE CORRECT MEMBER          04260000
         SPACE 1                                                        04270000
* PRIME THE INPUT BUFFERS                                               04280000
SKIPFIND L     R2,BUFSSTAR         PNT TO 1ST DECB                      04290000
         USING DECB,R2             DECLARE DECB BASE                    04300000
PRIMELIB READ  (R2),SF,MF=E        START A READ TO IT                   04310000
         L     R2,DECBLINK         PNT TO NEXT DECB                     04320000
         C     R2,BUFSSTAR         ALL READS STARTED YET?               04330000
         BNE   PRIMELIB            NO, LOOP FOR NEXT                    04340000
         SPACE 1                                                        04350000
* CREATE AND PUNCH THE ./ADD CARD (IF DESRIED)                          04360000
         TM    FLAG,UPDTE+UPDAT    ./ADD WANTED?                        04370000
         BZ    CHECKLIB            NO, SKIP                             04380000
         MVC   ADDCARD+15(L'ADDCARD-15),ADDCARD+14 CLEAR OLD OPNDS      04390000
         MVC   ADDCARD+15(5),=C'NAME=' ASSUME IEBUPDTE FORMAT           04400000
         MVC   ADDCARD+20(L'DCBDDNAM),0(R7) GET THE MEMBER NAME         04410000
         TM    FLAG,UPDAT          ASSUMPTION CORRECT?                  04420000
         BZ    PUTADD              YES, GO PUNCH THE CARD               04430000
         MVC   ADDCARD+15(L'DCBDDNAM+1),ADDCARD+20 NO, IEBUPDAT FORMAT  04440000
         LA    R1,ADDCARD+15       SETUP FOR BLANK SCAN                 04450000
ADDBLNK  LA    R1,1(,R1)           PNT TO NEXT COLUMN                   04460000
         CLI   0(R1),C' '          BLANK YET?                           04470000
         BNE   ADDBLNK             NO, LOOP                             04480000
         MVC   0(7,R1),=C',00,0,0' YES, GET REST OF UPDAT OPNDS         04490000
PUTADD   PUT   OUT,ADDCARD         PUNCH IT                             04500000
         SPACE 1                                                        04510000
* CHECK THE NEXT INPUT BLOCK AND SETUP TO SCAN IT FOR LOGICAL RECORDS   04520000
CHECKLIB NI    FLAG2,255-IOERROR   CLEAR I/O ERROR FLAG                 04530000
         CHECK (R2)                WAIT FOR A READ TO COMPLETE          04540000
         L     R3,DECBBUFR         POINT TO 1ST RECORD IN THE BUFFER    04550000
         LH    R4,INBUFL           GET BUFFER LENGTH                    04560000
         AR    R4,R3               PNT TO END OF BUFFER                 04570000
         L     R1,DECBIOB          PNT TO RELATED IOB                   04580000
         SH    R4,14(,R1)          PNT TO END OF BLOCK READ             04590000
         SPACE 1                                                        04600000
* SCAN THE INPUT BLOCK AND HANDLE EOB AND I/O ERROR CONDITIONS          04610000
CARDLOOP CR    R3,R4               END OF BLOCK YET?                    04620000
         BL    PUTCARD             NO, GO PUT NEXT CARD                 04630000
         TM    FLAG2,IOERROR       YES, HAS AN I/O ERR OCCURED?         04640000
         BZ    READLIB             NO, SKIP                             04650000
         #PUT  IOERPFIX            YES, PRINT THE ERROR MSG             04660000
READLIB  READ  (R2),SF,MF=E        READ FUTURE BLOCK                    04670000
         L     R2,DECBLINK         LINK TO NEXT DECB                    04680000
         B     CHECKLIB            GO CHECK ITS I/O                     04690000
         SPACE 1                                                        04700000
* TEST FOR IEBUPDTE CONTROL CARDS FROM THE INPUT DATA SET               04710000
PUTCARD  CLC   0(2,R3),ADDCARD     IEBUPDTE CTL CARD?                   04720000
         BNE   PUTITOUT            NO, AOK                              04730000
         TM    FLAG,UPDAT+UPDTE    YES, ./ADD'S BEING INSERTED?         04740000
         BZ    PUTITOUT            NO, AOK                              04750000
         TM    FLAG,CTLCDERR       YES, ERROR BEFORE FROM SAME MEMBER?  04760000
         BO    PRTERRCD            YES, JUST PRINT THE CARD             04770000
         OI    FLAG,CTLCDERR       NO, SET THE FLAG                     04780000
         #PUT  CTLERR1             PRINT ERROR EXPLAINATION             04790000
         #PUT  CTLERR2             MORE EXPLAINATION                    04800000
         CLI   RCD+1,4             COMPLETION CODE ALREADY SET?         04810000
         BNL   PRTERRCD            YES, SKIP                            04820000
         MVI   RCD+1,4             NO, SET IT                           04830000
PRTERRCD MVC   CARD2,0(R3)         COPY ERROR CARD TO BUFFER            04840000
         #PUT  ERRCARD             PRINT IT                             04850000
         MVC   0(2,R3),=C'><'      KILL THE './'                        04860000
         SPACE 1                                                        04870000
* PUNCH THE NEXT CARD AND LOOP FOR THE FOLLOWING CARD                   04880000
PUTITOUT PUT   OUT,(R3)            PUT THE NEXT CARD                    04890000
         AH    R3,INLRECL          PNT TO FOLLOWING CARD                04900000
         B     CARDLOOP            LOOP TO PROCESS                      04910000
         DROP  R2                  RELEASE DECB BASE                    04920000
         SPACE 1                                                        04930000
* CURRENT DATA SET FINISHED - CLOSE IT AND FREE ITS BUFFERS             04940000
DSDONE   CLOSE (IN,,DRCTY)         CLOSE THE INPUT DATA SET             04950000
         LM    R0,R1,BUFSSIZE      GET IN BUFS LEN AND ADDR             04960000
         FREEMAIN R,LV=(0),A=(1)   RELEASE THE BUFFERS                  04970000
         LA    R10,L'DCBDDNAM(,R10) PNT TO NEXT INPUT DDNAME            04980000
         C     R10,ENDDDNS         END OF NAMES?                        04990000
         BL    DATSETLP            NO, LOOP TO PROCESS THIS DS          05000000
         SPACE 1                                                        05010000
* CURRENT OFFLOADING FUNCTION FINISHED - FOR SELECT FUNCTIONS DETERMINE 05020000
* IF ALL GIVEN NAMES WERE SELECTED                                      05030000
         TM    FLAG,SELECT         SELECT FUNCTION?                     05040000
         BZ    WARLSE              NO, SKIP                             05050000
         L     R2,ENDDDNS          YES, PNT TO START OF MBR MNES        05060000
NOTSELCT C     R2,ENDMNMS          END OF GIVEN NAMES YET?              05070000
         BNL   WARLSE              YES, DONE                            05080000
         OC    0(L'DCBDDNAM,R2),0(R2) NO, THIS NME USED?                05090000
         BZ    SELECTD             YES, CONTINUE                        05100000
         MVC   NTSELMBR,0(R2)      NO, PUT NME INTO MSG                 05110000
         #PUT  NTSELMSG            TELL USER                            05120000
         CLI   RCD+1,4             TEST RETURN CODE                     05130000
         BNL   SELECTD             ALREADY SET                          05140000
         MVI   RCD+1,4             NOT SET; SET IT                      05150000
SELECTD  LA    R2,L'DCBDDNAM(,R2)  PNT TO NEXT GIVEN NAME               05160000
         B     NOTSELCT            LOOP TO TEST IT                      05170000
         SPACE 1                                                        05180000
* FREE THE WORK AREA AND LOOP FOR MORE CONTROL CARDS                    05190000
WARLSE   LM    R1,R2,WORKSTAR      YES, GET WA START & END              05200000
         SR    R2,R1               GET WORK AREA LENGTH                 05210000
         FREEMAIN R,LV=(R2),A=(1)  RELEASE THE WORKAREA                 05220000
         B     CONTROL             GO READ SOME MORE CTL CARDS          05230000
         TITLE 'OFFLOAD -- END OF JOB PROCESSING'                       05240000
EXIT16   MVI   RCD+1,16            SET ERROR COMPLETION CODE            05250000
         SPACE 1                                                        05260000
* WRITE THE EOP MSG IF SYSPRINT IS OPEN                                 05270000
EOP      TM    PRTOFLGS,DCBOFOPN   SYSPRINT OPEN?                       05280000
         BZ    RETURN              NO, SKIP                             05290000
         #PUT  BLNK3               YES, PRINT SOME BLANK LINES          05300000
         LA    R1,EOPM-1           ASSUME RELATIVELY GOOD ENDING        05310000
         CLI   RCD+1,8             CORRECT?                             05320000
         BNH   PUTEND              YES, GO PRINT MSG                    05330000
         LA    R1,PGMTERM-1        NO, PNT TO DIFFERENT MSG             05340000
PUTEND   #PUT  (R1)                PRINT THE END OF PROGRAM MSG         05350000
         SPACE 1                                                        05360000
* CLOSE ALL DATA SETS                                                   05370000
         LA    R2,OUT              POINT TO THE OUTPUT DATA SET         05380000
         BAL   R14,CLOSE           CLOSE IT                             05390000
         LA    R2,SYSPRINT         POINT TO THE SYSPRINT DATA SET       05400000
         BAL   R14,CLOSE           CLOSE IT                             05410000
         SPACE 1                                                        05420000
* RETURN TO CALLER                                                      05430000
RETURN   LH    R15,RCD             GET THE COMPLETION CODE              05440000
         #EXIT ((R14,R12)),RC=(R15) RETURN TO CALLER                    05450000
         TITLE 'OFFLOAD -- INPUT CONTROL CARD ERROR HANDLING ROUTINE'   05460000
CARDNCAP MVC   PRECARD+L'PRECARD-24(23),=C'INCOMPATABLE FUNCTION -'     05470000
         B     CARDPUT             GO PRINT THE CARD ERROR              05480000
         SPACE 1                                                        05490000
CARDSYNT TM    FLAG,UNLOAD         SHOULD THE CARD HAVE BEEN IGNORED?   05500000
         BZ    CARDIGNR            YES, GO SAY SO                       05510000
         MVC   PRECARD+L'PRECARD-15(14),=C'SYNTAX ERROR -' NO,          05520000
         B     CARDPUT             GO PRINT THE CARD ERROR              05530000
         SPACE 1                                                        05540000
CARDIGNR MVC   PRECARD+L'PRECARD-15(14),=C'CARD IGNORED -'              05550000
         B     CARDPUT2            GO PRINT THE ERROR CARD              05560000
         SPACE 1                                                        05570000
CARDMISO MVC   PRECARD+L'PRECARD-28(27),=C'REQUIRED OPERANDS MISSING -' 05580000
         B     CARDPUT2            GO PRINT THE ERROR CARD              05590000
         SPACE 1                                                        05600000
CARDCORE MVC   PRECARD+L'PRECARD-17(16),=C'CORE EXHAUSTED -'            05610000
         B     GETSCAN             GO GET ERROR POINTER                 05620000
         SPACE 1                                                        05630000
CARDRDUN MVC   PRECARD+L'PRECARD-21(20),=C'REDUNDANT OPERANDS -'        05640000
         B     GETSCAN             GO GET ERROR POINTER                 05650000
         SPACE 1                                                        05660000
CARDBDDN MVC   PRECARD+L'PRECARD-18(17),=C'MISSING DD CARD -'           05670000
GETSCAN  L     R3,SAVESCAN         RESET THE ERROR POINTER              05680000
         SPACE 1                                                        05690000
CARDPUT  #PUT  PRECARD             PRINT THE CARD ERROR                 05700000
         LA    R2,OUT              POINT TO THE OUTPUT DATA SET         05710000
         BAL   R14,CLOSE           INSURE THAT IT IS CLOSED             05720000
         MVI   OUTDDNAM,C' '       FLAG "O=" OPERAND REQUIRED           05730000
         MVI   PRECARD,C' '        CLEAR THE ERROR CARD                 05740000
         MVC   PRECARD+1(L'PRECARD+L'CARD-1),PRECARD CLR ERR CRD        05750000
         MVI   0(R3),C'*'          UNDERLINE THE ERROR                  05760000
CARDPUT2 #PUT  PRECARD             UNDERLINE THE ERROR                  05770000
RCD8     MVI   RCD+1,8             SET THE COMPLETION CODE              05780000
         TM    RDROFLGS,DCBOFOPN   SYSIN STILL OPEN?                    05790000
         BZ    WARLSE              NO, GO FORCE PGM TERMINATION         05800000
         B     RESTART             LOOP FOR NEXT CONTROL STATEMENT      05810000
         TITLE 'OFFLOAD -- CARDEXIT - OPEN DCB EXIT FOR OUT, IN, AND SY*05820000
               SIN'                                                     05830000
* FORCE THE BLKSIZE AND BUFL TO A (LOWER) MULTIPLE OF THE LRECL         05840000
         USING DCB,R1              DECLARE THE DCB BASE                 05850000
CARDEXIT LH    R3,DCBBLKSI         GET THE BLOCK SIZE (IF ANY)          05860000
         LH    R4,DCBLRECL         GET THE LRECL                        05870000
         SR    R2,R2               CLEAR FOR DEVIDE                     05880000
         DR    R2,R4               GET THE BLOCKING FACTOR              05890000
         MR    R2,R4               INSURE BLKSI = N*LRECL               05900000
         CR    R3,R4               IS FILE BLOCKED?                     05910000
         BH    GOTBLKSI            YES, SKIP                            05920000
         LR    R3,R4               NO, INSURE BLKSI = LRECL             05930000
GOTBLKSI STH   R3,DCBBLKSI         SET THE BLOCK SIZE                   05940000
         STH   R3,DCBBUFL          SET THE BUFL (ASSUME QS)             05950000
         SPACE 1                                                        05960000
* IF AN NCP (BPAM) OF BUFNO (QSAM OR BPAM) ARE GIVEN VIA JCL OR DATA    05970000
* SET LABEL, THEN USE IT. FOR BPAM PREFER A GIVEN NCP OVER A GIVEN      05980000
* BUFNO. ALSO FOR BPAM INHIBIT AUTOMATIC BUFFER ALLOCATION.             05990000
         LA    R6,DCBBUFNO         POINT TO BUFNO FIELD                 06000000
         TM    DCBMACF1,DCBMRRD    BSAM/BPAM DS?                        06010000
         BZ    NOTBASIC            NO, DSORG=PS; SKIP                   06020000
         LA    R6,DCBNCP           YES, POINT TO NCP (NOT BUFNO)        06030000
         CLI   DCBNCP,0            NCP GIVEN?                           06040000
         BNE   GOTNCP              YES, USE IT                          06050000
         MVC   DCBNCP,DCBBUFNO     NO, USE BUFNO (IF ANY)               06060000
GOTNCP   MVI   DCBBUFNO,0          INSURE NOW BUFFERS GOTTEN            06070000
NOTBASIC CLI   0(R6),0             BUFNO/NCP GIVEN?                     06080000
         BNE   BUFSGEN             YES, SKIP                            06090000
         SPACE 1                                                        06100000
* BUFNO AND NCP NUT GIVEN - CALCULATE A REASONABLE VALUE AND USE IT.    06110000
         LA    R5,BUFSPACE         NO, GET REASONABLE BUFFER SPACE      06120000
         SR    R4,R4               CLEAR FRO DEVICE                     06130000
         DR    R4,R3               GET REASONABLE BUFNO/NCP             06140000
         STC   R5,0(,R6)           STORE REASONABLE BUFNO/NCP           06150000
         LA    R5,10               GET MAX REASONABLE BUFNO/NCP         06160000
         CLI   0(R6),10            MAX EXCEEDED?                        06170000
         BH    BUFNRSET            YES, GO RESET                        06180000
         LA    R5,3                NO, GET MIN REASONABLE BUFNO/NCP     06190000
         CLI   0(R6),3             MIN EXCEEDED?                        06200000
         BNL   BUFSGEN             NO, CONTINUE                         06210000
BUFNRSET STC   R5,0(,R6)           YES, RESET TO MAX/MIN REASONABLE     06220000
         SPACE 1                                                        06230000
* FOR BPAM CREATE A SPECIAL POOL OF BUFFERS AND DECB'S                  06240000
BUFSGEN  TM    DCBMACF1,DCBMRRD    BASM/BPAM DS?                        06250000
         BCR   8,R14               NO, RETURN TO OPEN                   06260000
         L     R2,DCBDEBAD         YES, PNT TO OPEN'S WORK AREA         06270000
         TM    JFCDSRG1-OPENWORK(R2),JFCORGPO DS ACTUALLY PDS?          06280000
         BO    ACTULPDS            YES, SKIP                            06290000
         OI    FLAG,NOTPDS         NO, REMEMBER THIS                    06300000
ACTULPDS ST    R14,SAVER14A        SAVE RETURN ADDRESS                  06310000
         SR    R2,R2               CLEAR FOR 'IC'                       06320000
         IC    R2,0(,R6)           GET NUMBER OF BUFFERS TO BUILD       06330000
         LA    R5,DECBLEN          GET DECB LENGTH                      06340000
         LA    R1,0(R5,R3)         GET LENGTH OF DECB+BUFFER            06350000
         DROP  R1                  RELEASE DCB BASE                     06360000
         MR    R0,R2               GET TOTAL CORE REQUIRED              06370000
         ST    R1,BUFSSIZE         SAVE LENGTH FOR LATER FREEMAIN       06380000
         GETMAIN R,LV=(R1)         GET CORE FOR BUFFERS                 06390000
         ST    R1,BUFSSTAR         SAVE START FOR LATER                 06400000
         MR    R4,R2               GET TOTAL LENGTH OF DECB'S           06410000
         AR    R5,R1               PNT TO 1ST BUFFER                    06420000
DECBINIT LR    R4,R1               PNT TO NEXT DECB                     06430000
         USING DECB,R4             DECLARE DECB BASE                    06440000
         MVC   0(DECBBUFR-DECB,R4),DECBMODL GET SKELETON DECB           06450000
         ST    R5,DECBBUFR         SET BUFFER POINTER                   06460000
         AR    R5,R3               ADVANCE BUFFER PTR                   06470000
         LA    R1,DECBLEN(,R4)     PNT TO NEXT DECB                     06480000
         ST    R1,DECBLINK         LINK IT TO THIS ONE                  06490000
         BCT   R2,DECBINIT         LOOP TO PROCESS NEXT DECB            06500000
         MVC   DECBLINK,BUFSSTAR   MAKE DECB LINKS CIRCULAR             06510000
         DROP  R4                  RELEASE DECB BASE                    06520000
         L     R14,SAVER14A        RESTORE RETURN ADDRESS               06530000
         BR    R14                 RETURN TO OPEN                       06540000
         TITLE 'OFFLOAD -- CLOSE - CLOSE A DATA SET AND FREE ITS BUFFER*06550000
               S'                                                       06560000
         USING DCB,R2              DECLARE DCB BASE                     06570000
CLOSE    TM    DCBOFLGS,DCBOFOPN   IS THE DATA SET OPEN?                06580000
         BCR   8,R14               NO, RETURN TO CALLER                 06590000
         ST    R14,SAVER14A        YES, SAVE THE RETURN ADDRESS         06600000
         SPACE 1                                                        06610000
* CLOSE THE DATA SET                                                    06620000
         CLOSE ((R2))              CLOSE THE DATA SET                   06630000
         SPACE 1                                                        06640000
* FREE ITS BUFFERS                                                      06650000
         FREEPOOL (R2)             FREE ITS BUFFERS                     06660000
         L     R14,SAVER14A        RESTORE THE RETURN ADDRESS           06670000
         BR    R14                 RETURN TO CALLER                     06680000
         DROP  R2                  RELEASE THE DCB BASE                 06690000
         TITLE 'OFFLOAD -- GETNAME - SCAN OUT A NAME FROM AN INPUT CONT*06700000
               ROL STATEMENT'                                           06710000
GETNAME  STM   R14,R1,SAVER14B     SAVE WORK REGISTERS                  06720000
         LA    R1,1(,R3)           POINT TO NAME TO BE SCANNED          06730000
         ST    R1,SAVESCAN         SAVE ICO ERROR                       06740000
         MVI   WORKAREA,C' '       CLEAR THE -                          06750000
         MVC   WORKAREA+1(L'DCBDDNAM-1),WORKAREA  WORK AREA             06760000
         LA    R15,WORKAREA-1      GET OUTPUT SCANNER                   06770000
         LR    R0,R4               GET BXLE INCREMENT ("1")             06780000
         LA    R1,WORKAREA+L'DCBDDNAM-1 GET BXLE LIMIT                  06790000
         LA    R14,VALID+L'VALID-10 PNT 2 VALID 1ST CHARACTERS          06800000
         SPACE 1                                                        06810000
* VALIDATE THE NAME'S SYNTAX                                            06820000
GETNLP1  BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN                  06830000
GETNLP2  BCTR  R14,0               POINT TO NEXT VALID CHARACTER        06840000
         CLC   0(1,R3),0(R14)      CARD CHARACTER VALID?                06850000
         BL    GETNLP2             DON'T KNOW YET; LOOP                 06860000
         BH    GETNTDLM            NO, GO SEE IF IT'S A DELIMITER       06870000
         CLI   0(R3),0             PROBABLE; IS IT X'00'?               06880000
         BE    CARDSYNT            YES, CHARACTER IS INVALID            06890000
         BXH   R15,R0,CARDSYNT     NO, CHAR VLD; ADV 2 NXT OPUT LOCATN  06900000
         MVC   0(1,R15),0(R3)      SAVE THIS CHARACTER                  06910000
         LA    R14,VALID+L'VALID   PNT TO LIST OF VALID NOT-FIRST CHARS 06920000
         B     GETNLP1             LOOP TO TEST NEXT COLUMN             06930000
         SPACE 1                                                        06940000
* VALIDATE THE DELIMITER FOLLOWING THE NAME                             06950000
GETNTDLM CLI   0(R3),C','          COMMA DELIMITER?                     06960000
         BE    GOTNAME             YES, GOT A NAME                      06970000
         TM    FLAG2,PAREN         NO, WITHIN PARENS?                   06980000
         BZ    GETNTBLK            NO, DLM MUST BE BLANK                06990000
         CLI   0(R3),C')'          YES, DLM = CLOSE PARENS?             07000000
         BNE   CARDSYNT            NO, SYNTAX ERROR                     07010000
         BXH   R3,R4,CARDSYNT      YES, ADVANCE TO NEXT COLUMN          07020000
         NI    FLAG2,255-PAREN     CLEAR PARENS FLAG                    07030000
         B     GETNTDLM            GO TEST NEXT DELIMITER               07040000
GETNTBLK CLI   0(R3),C' '          DLM BLANK?                           07050000
         BNE   CARDSYNT            NO, ERROR                            07060000
GOTNAME  CLI   WORKAREA,C' '       YES, NULL NAME?                      07070000
         BE    CARDSYNT            YES, ERROR                           07080000
         SPACE 1                                                        07090000
* IF THE NAME IS A DDNAME, THEN VALIDATE IT AGAINST THE TIOT.           07100000
         TM    FLAG2,DDNAME        NO, OPND OBJECT A DDNAME?            07110000
         BZ    GOTNRET             NO, GO RETURN TO CALLER              07120000
         L     R1,CVTPTR           POINT TO THE CVT                     07130000
         L     R1,CVTTCBP-CVT(,R1) PNT TO TCB NEW/OLD PTRS              07140000
         L     R1,4(,R1)           PNT TO OUR TCB                       07150000
         L     R1,TCBTIO-TCB(,R1)  PNT TO TIOT                          07160000
         LA    R1,TIOENTRY-TIOT(,R1) POINT TO 1ST DD ENTRY              07170000
         USING TIOENTRY,R1         DECLARE TIOT DD ENTRY BASE           07180000
         SR    R0,R0               CLEAR FOR "IC"                       07190000
TIOTLP   AR    R1,R0               POINT TO NEXT TIOT DD ENTRY          07200000
         IC    R0,TIOELNGH         GET ITS LENGTH                       07210000
         LTR   R0,R0               END OF TIOT?                         07220000
         BZ    CARDBDDN            YES, MISSING DDNAME ERROR            07230000
         CLC   TIOEDDNM,WORKAREA   NO, RIGHT DDNAME?                    07240000
         BNE   TIOTLP              NO, LOOP FOR NEXT DD ENTRY           07250000
GOTNRET  LM    R14,R1,SAVER14B     RESTORE ALL WORK REGISTERS           07260000
         BR    R14                 RETURN TO CALLER                     07270000
         DROP  R1                  RELEASE THE TIOT DD ENTRY BASE       07280000
         TITLE 'OFFLIAD -- INERR - INPUT DATA SET SYNAD ROUTINE'        07290000
         USING DECB,R2             DECLARE THE INPUT DECB BASE          07300000
INERR    SYNADAF ACSMETH=BSAM      NO, GET SYNAD MSG                    07310000
         L     R13,4(,R13)         RESTORE BASE REGISTER                07320000
         MVI   RCD+1,8             SET COMPLETION CODE                  07330000
         OI    FLAG2,IOERROR       REMEMBER THE ERROR                   07340000
         MVC   IOERTEXT,68(R1)     SAVE SYNAD MSG TEXT                  07350000
         SPACE 1                                                        07360000
* FORCE THE RESIDUAL COUNT TO A (HIGHER) MULTIPLE OF THE LRECL.         07370000
         L     R15,DECBIOB         PNT TO THE IOB                       07380000
         LH    R1,14(,R15)         GET THE RESIDUAL COUNT               07390000
         LH    R15,INLRECL         GET THE LRECL                        07400000
         AR    R1,R15              ROUND -                              07410000
         BCTR  R1,0                 THE -                               07420000
         SR    R0,R0                 RESIDUAL -                         07430000
         DR    R0,R15                 COUNT -                           07440000
         MR    R0,R15                  UP                               07450000
         L     R15,DECBIOB         POINT TO THE IOB AGAIN               07460000
         STH   R1,14(,R15)         STORE NEW RESIDUAL COUNT             07470000
         SPACE 1                                                        07480000
* ISSUE SYNADRLS TO RELEASE SYNAD'S SAVE AREA AND ERROR MESSAGE BUFFER  07490000
         L     R13,8(,R13)         RESTORE SYNAD'S SAVE AREA POINTER    07500000
         LNR   R15,R13             INSURE R15 NEGITIVE                  07510000
         SVC   68                  ISSUE SYNADRLS                       07520000
         BR    R14                 RETURN TO CHECK ROUTINE              07530000
         DROP  R2                  RELEASE DECB ABSE                    07540000
         TITLE 'OFFLOAD -- NEXTCARD - GET A CONTINUATION CARD'          07550000
NEXTCARD STM   R14,R1,SAVER14B     SAVE ALL WORK REGISTERS              07560000
         #PUT  PRECARD             ECHO OUT THE CURRENT CARD            07570000
         MVI   PRECARD,C' '        CLEAR ANY POSSIBLE CARRAGE CONTROL   07580000
         OI    FLAG2,CONTINUE      FLAG CONTU ICO EOD                   07590000
         GET   SYSIN,CARD          GET THE NEXT CARD                    07600000
         SPACE 1                                                        07610000
* RE-INITIALIZE THE CARD SCANNER                                        07620000
         LA    R3,CARD             RESET THE CARD SCANNER               07630000
         CLI   0(R3),C' '          MUST BE BLANK                        07640000
         BNE   CARDSYNT            IT'S NOT; ERROR                      07650000
OPNDLP2  BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN                  07660000
         CLI   0(R3),C' '          STILL BLANK?                         07670000
         BE    OPNDLP2             YES, LOOP BACK                       07680000
         BCTR  R3,0                NO, BACK OFF                         07690000
         LM    R14,R1,SAVER14B     RESTORE ALL WORK REGISTERS           07700000
         BR    R14                 RETURN TO CALLER                     07710000
         TITLE 'OFFLOAD -- OUTERR - OUTPUT DATA SET SYNAD ROUTINE'      07720000
OUTERR   SYNADAF ACSMETH=QSAM      GET SYNAD MSG                        07730000
         L     R13,4(,R13)         RESTORE LOCAL BASE REGISTER          07740000
         MVI   RCD+1,12            SET PROGRAM COMPLETION CODE          07750000
         SPACE 1                                                        07760000
* PRINT THE SYNAD MESSAGE                                               07770000
         MVC   67-L'IOERPFIX(L'IOERPFIX+1,R1),IOERPFIX-1 MSG PREFIX     07780000
         LA    R1,67-L'IOERPFIX(,R1) PNT TO ERR MSG                     07790000
         #PUT  (R1)                PRINT IT                             07800000
         SPACE 1                                                        07810000
* ISSUE SYNADRLS TO RELEASE SYNAD'S SAVE AREA AND MESSAGE BUFFER.       07820000
         L     R13,8(,R13)         PNT BACK TO SYNAD SAVE AREA          07830000
         LNR   R15,R13             INSURE R15 NEGITIVE                  07840000
         SVC   68                  ISSUE SYNADRLS                       07850000
         SPACE 1                                                        07860000
* FORCE PROGRAM TERMINATION                                             07870000
         LA    R2,SYSIN            PNT TO THE READER DCB                07880000
         BAL   R14,CLOSE           CLOSE IT                             07890000
         L     R10,ENDDDNS         PNT TO END OF DDNAMES LIST           07900000
         B     DSDONE              GO RETURN TO CALLER (EVENTUALLY)     07910000
         TITLE 'OFFLOAD -- PUTNAMES - SCAN NAMES FROM CONTROL STATEMENT*07920000
                S AND PUT THEM INTO A LIST'                             07930000
PUTNAMES STM   R14,R15,SAVER14A    SAVE THE RETURN ADDRESS              07940000
         BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN                  07950000
         SPACE 1                                                        07960000
* CHECK FOR LEADING PARENS, THEN GET A NAME                             07970000
         CLI   1(R3),C'('          OPEN PARENS?                         07980000
         BNE   GNAME               NO, SKIP                             07990000
         OI    FLAG2,PAREN         YES, REMEMBER IT                     08000000
         BXH   R3,R4,CARDSYNT      SCAN TO NEXT COLUMN                  08010000
GNAME    BAL   R14,GETNAME         GO SCAN OUT THIS NAME                08020000
         SPACE 1                                                        08030000
* CHECK FOR REDUNDANT NAMES                                             08040000
         LR    R15,R0              POINT TO START OF LIST               08050000
RDUNTEST CR    R15,R1              LIST EXAUSTED YET?                   08060000
         BNL   NEWNAME             YES, NEW NAME IS UNIQUE              08070000
         CLC   0(L'DCBDDNAM,R15),WORKAREA NO, NEW NAME UNIQUE?          08080000
         BE    CARDRDUN            NO, ERROR                            08090000
         LA    R15,L'DCBDDNAM(,R15) MAYBE, ADVANCE THE LIST POINTER     08100000
         B     RDUNTEST            LOOP TO TEST AGAIN                   08110000
         SPACE 1                                                        08120000
* SAVE THE NEW NAME IN THE LIST (IF THERE IS ROOM FOR IT)               08130000
NEWNAME  LA    R14,L'DCBDDNAM(,R1) GET NEW END OF LIST                  08140000
         C     R14,WORKEND         CORE EXHAUSTED?                      08150000
         BH    CARDCORE            YES, ERROR                           08160000
         MVC   0(L'DCBDDNAM,R1),WORKAREA NO, COPY NEW NAME INTO LIST    08170000
         LR    R1,R14              SET NEW END OF LIST PTR              08180000
         SPACE 1                                                        08190000
* PROCESS TRAILING DELIMITERS AND POSSIBILY LOOP BACK FOR ANOTHER       08200000
* NAME                                                                  08210000
         CLI   0(R3),C','          COMMA DELIMITER?                     08220000
         BNE   PUTNRET             NO, MUST B END OF STATEMENT (CC^=0)  08230000
         TM    FLAG2,PAREN         YES, WITHIN PARENS?                  08240000
         BZ    PUTNRET             NO, END OF OPERAND (CC=0)            08250000
         CLI   1(R3),C' '          YES, CONTINUE CARD NEEDED?           08260000
         BNE   GNAME               NO, GO SCAN OUT THE NEXT NAME        08270000
         BAL   R14,NEXTCARD        YES, GO GET THE NEXT CARD            08280000
         B     GNAME               GO SCAN OUT THE NEXT NAME            08290000
PUTNRET  LM    R14,R15,SAVER14A    RESTORE THE RETURN ADDRESS (CC SET)  08300000
         BR    R14                 RETURN TO CALLER                     08310000
         TITLE 'OFFLOAD -- PUTSYSP - PRINT MESSAGES AND CONTROL PAGINAT*08320000
               ION'                                                     08330000
PUTSYSP  STM   R14,R1,SAVER14D     SAVE WORK REGISTERS                  08340000
         MVC   SAVECCC,1(R1)       SAVE CARRAGE CONTROL CHARACTER       08350000
         SPACE 1                                                        08360000
* DETERMINE THE CARRAGE CONTROL CHARACTER.                              08370000
         LA    R15,CCC+L'CCC       POINT PAST VALID CC CHARACTERS       08380000
         LA    R0,L'CCC            GET LOOP CONTROL                     08390000
CCCLP    BCTR  R15,0               POINT TO NEXT CCC                    08400000
         CLC   1(1,R1),0(R15)      MATCH?                               08410000
         BE    GOTCCC              YES, SKIP OUT                        08420000
         BCT   R0,CCCLP            NO, LOOP FOR NEXT CCC                08430000
         SPACE 1                                                        08440000
* UNRECOGNIZED CONTROL CHARACTER - SET NEW TITLE LINE AND FORCE PAGE    08450000
* EJECT ON NEXT CALL.                                                   08460000
         MVC   TITLEBUF,TITLEBUF-1 NO MATCH; CLEAR TITLE BUFFER         08470000
         MVC   TITLEMVC+1(1),0(R1) SET MOVE INSTR LENGTH FIELD          08480000
TITLEMVC MVC   TITLEBUF(*-*),1(R1) SET NEW TITLE                        08490000
         STH   R0,LINES2GO         FORCE PAGE EJEXT ON NEXT CALL        08500000
         B     PUTSRET             GO RETURN TO CALLER                  08510000
         SPACE 1                                                        08520000
* CARRAGE CONTROL CHARACTER IDENTIFIED - INCRIMENT LINE COUNT FOR THIS  08530000
* PAGE.                                                                 08540000
GOTCCC   BCTR  R0,0                GET # LINES REPRESENTED BY THE CCC   08550000
         AH    R0,LINES2GO         GET # LINES LEFT ON PAGE             08560000
         BNP   NOEJECT             PAGE NOT DONE YET                    08570000
         SPACE 1                                                        08580000
* PAGE FULL - EJECT TO NEW PAGE AND PRINT THE TITLE LINE WITH A PAGE    08590000
* COUNTER.                                                              08600000
         AP    PAGECTR,=P'1'       PAGE DONE; INCR PAGE #               08610000
         MVC   PAGENO,EDITMASK     GET THE EDIT MASK                    08620000
         ED    PAGENO-1(L'PAGENO+1),PAGECTR CNVRT TO EBCDIC             08630000
         MVI   PRTLRECL+1,121      SET LRECL                            08640000
         PUT   SYSPRINT,TITLEBUF   PRINT THE TITLE -                    08650000
         MVI   PRTLRECL+1,L'BLNK1   AND A -                             08660000
         PUT   SYSPRINT,BLNK1        BLANK LINE                         08670000
         L     R1,SAVER1D          RESTORE THE ORIGINAL MSG PTR         08680000
         MVI   1(R1),C' '          CHANGE TO BLANK CCC                  08690000
         LH    R0,=Y(3-LINECT)     RESET LINES TO GO                    08700000
NOEJECT  STH   R0,LINES2GO         STORE NEW LINES 2 GO                 08710000
         SPACE 1                                                        08720000
* PRINT THE MESSAGE AND RETURN TO CALLER.                               08730000
         MVC   PRTLRECL+1(1),0(R1) SET LRECL                            08740000
         LA    R0,1(,R1)           POINT TO THE MESSAGE                 08750000
         PUT   SYSPRINT,(0)        PRINT THE MESSAGE                    08760000
PUTSRET  LM    R14,R1,SAVER14D     RESTORE WORK REGISTERS               08770000
         MVC   1(1,R1),SAVECCC     RESTORE THE ORIGINAL CCC             08780000
         BR    R14                 RETURN TO CALLER                     08790000
         TITLE 'OFFLOAD -- RDRERR - SYSIN DATA SET SYNAD ROUTINE'       08800000
RDRERR   ST    R14,SAVER14C        SAVE THE RETURN ADDRESS              08810000
         SYNADAF ACSMETH=QSAM      GET THE SYNAD MESSAGE                08820000
         LR    R15,R13             SAVE PTR TO SYNAD'S SAVE AREA        08830000
         L     R13,4(,R13)         PNT BACK TO OUR OWN SAVE AREA        08840000
         MVC   12(60,R15),12(R13)  SAVE ASVE AREA CONTENTS              08850000
         MVI   RCD+1,8             SET PGM COMPLETION CODE              08860000
         MVC   67-L'IOERPFIX(L'IOERPFIX+1,R1),IOERPFIX-1 MSG PREFIX     08870000
         LA    R1,67-L'IOERPFIX(,R1) PNT TO MSG TEXT                    08880000
         #PUT  (R1)                PRINT SYNAD MSG                      08890000
         LR    R15,R13             SAVE LOCAL SAVE AREA PTR             08900000
         L     R13,8(,R13)         PNT BACK TO SYNAD'S SAVE AREA        08910000
         MVC   12(60,R15),12(R13)  RESTORE LOCAL SAVE AREA CONTENTS     08920000
         LNR   R15,R13             INSURE R15 NEGITIVE                  08930000
         SVC   68                  ISSUE SYNADRLS                       08940000
         L     R14,SAVER14C        RESTORE RETURN ADDRESS               08950000
         BR    R14                 RETURN TO QSAM GET                   08960000
         TITLE 'OFFLOAD -- DATA - DATA CONTROL BLOCKS'                  08970000
         PRINT NOGEN                                                    08980000
         SPACE 3                                                        08990000
SYSIN    DCB   DDNAME=SYSIN,DSORG=PS,MACRF=GM,RECFM=FB,LRECL=80,       *09000000
               OPTCD=C,EODAD=RDREOD,EXLST=CARDXLST,SYNAD=RDRERR,       *09010000
               EROPT=ACC                                                09020000
         ORG   SYSIN                                                    09030000
RDR      #DCBD DSORG=QS                                                 09040000
         ORG   ,                                                        09050000
         SPACE 3                                                        09060000
SYSPRINT DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=PM,RECFM=UA,LRECL=121,   *09070000
               BLKSIZE=121,OPTCD=C,BUFNO=3,EROPT=ACC                    09080000
         ORG   SYSPRINT                                                 09090000
PRT      #DCBD DSORG=QS                                                 09100000
         ORG   ,                                                        09110000
         SPACE 3                                                        09120000
OUT      DCB   DSORG=PS,MACRF=PM,RECFM=FB,LRECL=80,OPTCD=C,            *09130000
               EXLST=CARDXLST,SYNAD=OUTERR                              09140000
         ORG   OUT                                                      09150000
OUT      #DCBD DSORG=QS                                                 09160000
         ORG   ,                                                        09170000
         SPACE 3                                                        09180000
IN       DCB   DSORG=PS,MACRF=RP,RECFM=FB,LRECL=80,OPTCD=C,            *09190000
               EODAD=MEMBERLP,EXLST=CARDXLST,SYNAD=INERR                09200000
         ORG   IN                                                       09210000
IN       #DCBD DSORG=BS                                                 09220000
         ORG   ,                                                        09230000
         SPACE 3                                                        09240000
DRCTY    DCB   DSORG=PS,MACRF=GM,RECFM=F,LRECL=256,BLKSIZE=256,OPTCD=C,*09250000
               BUFNO=5,EODAD=DRCERR,SYNAD=DRCERR,BUFL=256               09260000
         ORG   DRCTY                                                    09270000
DRC      #DCBD DSORG=QS                                                 09280000
         ORG   ,                                                        09290000
         SPACE 3                                                        09300000
         PRINT GEN                                                      09310000
         TITLE 'OFFLOAD -- DATA - MODEL READ DECB'                      09320000
         READ  DECBMODL,SF,IN,*-*,MF=L                                  09330000
         ORG   DECBMODL+(DECBBUFR-DECB) SAVE SOME SPACE                 09340000
         TITLE 'OFFLOAD -- DATA - MISCELLANIOUS'                        09350000
WORKAREA DS    D                   WORK AREA                            09360000
RDAREA   DS    XL256               DIRECTORY BLOCK LOCAL BUFFER         09370000
         SPACE 3                                                        09380000
GETMQTY  DC    A(OSCORE+2048,X'FFFFF8') VARIABLE GETMAIN REQUIREMENTS   09390000
         SPACE 1                                                        09400000
WORKSTAR DS    A                   GOTTEN CORE START                    09410000
WORKEND  DS    A                   GOTTEN CORE END                      09420000
         SPACE 1                                                        09430000
BUFSSIZE DS    A                   INPUT BUFFER 'POOL' SIZE             09440000
BUFSSTAR DS    A                   INPUT BUFFER 'POOL' START            09450000
         SPACE 1                                                        09460000
ENDDDNS  DS    A                   END OF INPUT DDNAMES LIST            09470000
ENDMNMS  DS    A                   END OF MEMBER NAMES LIST (USUALLY)   09480000
         SPACE 1                                                        09490000
SAVER14A DS    2A                  R14 - R15 SAVE AREA                  09500000
SAVER14B DS    4A                  R14 - R1 SAVE AREA                   09510000
SAVER14C DS    A                   R14 SAVE AREA                        09520000
SAVER14D DS    3A                  R14 - R0 SAVE AREA                   09530000
SAVER1D  DS    A                   R1 SAVE AREA                         09540000
SAVESCAN DS    A                   CONTROL CARD SCANNER SAVE AREA       09550000
         SPACE 1                                                        09560000
CARDXLST DC    0A(0),X'85',AL3(CARDEXIT) SYSIN, IN, OUT DCB EXIT LIST   09570000
DRCTYEND DS    A                   END OF CURRENT DRCTY BLK             09580000
OFFLPTR  DS    A                   LOCATION IN MSG TXT 4 IN DDN         09590000
         SPACE 3                                                        09600000
BLDLPARM DS    0Y                  SPECIAL BLDL PARAMETER LIST          09610000
BLDENTCT DC    Y(0)                ENTRY COUNT (THE MAGIC NUMBER)       09620000
BLDENTLN DC    Y(12)               ENTRY LENGTH (PROBABLY IGNORED)      09630000
BLDLKEY  DC    XL8'00'             COMPARE KEY (MUST BE ZERO)           09640000
BLDLTTR0 DC    Y(*-*),AL1(*-*,0)   TTR0 OF DESIRED BLOCK                09650000
         SPACE 1                                                        09660000
SEQFAKRY DC    Y(0),AL1(1,0)       FAKE TTR0                            09670000
EFFS     DC    (L'DCBDDNAM)X'FF',Y(0),2AL1(0) FAKE TERMINATER           09680000
SEQFAKLN EQU   *-SEQFAKRY          LENGTH OF FAKES                      09690000
         SPACE 1                                                        09700000
RCD      DC    Y(0)                PROGRAM COMPLETION CODE              09710000
LINES2GO DC    Y(0)                PRINT PAGE LINES 2 GO                09720000
         SPACE 3                                                        09730000
FLAG     DS    X                   FUNCTION FLAG BYTE                   09740000
UNLOAD   EQU   X'80'               UNLOAD CONTROL CARD ENCOUNTERED      09750000
SELECT   EQU   X'40'               SELECT CONTROL CARD ENCOUNTERED      09760000
EXCLUDE  EQU   X'20'               EXCLUDE CONTROL CARD ENCOUNTERED     09770000
UPDAT    EQU   X'10'               OUTPUT 4 IEBUPDAT (NOT IEBUPDTE)     09780000
NOTPDS   EQU   X'08'               INPUT DS IS NOT PARTITIONED          09790000
CTLCDERR EQU   X'04'               IEBUPDTE CTL CARD FROM PDS           09800000
UPDTE    EQU   X'02'               OUTPUT 4 IEBUPDTE (NOT IEBUPDAT)     09810000
         SPACE 1                                                        09820000
FLAG2    DS    X                   SYNTAX FLAG BYTE                     09830000
GOTIN    EQU   X'80'               "I=" OPERAND ENCOUNTERED             09840000
GOTOUT   EQU   X'40'               "O=" OPERAND ENCOUNTERED             09850000
GOTTYPE  EQU   X'20'               "T=" OPERAND ENCOUNTERED             09860000
PAREN    EQU   X'10'               OPEN PERENTHESIS ENCOUNTERED         09870000
CONTINUE EQU   X'08'               CONTINUATION CARD BEING READ         09880000
DDNAME   EQU   X'04'               OPND OBJECT IS A DDNAME              09890000
IOERROR  EQU   X'04'               AN I/O ERROR HAS OCCURED             09900000
         SPACE 1                                                        09910000
VALID    DC    C'$#@ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
CCC      DC    C'+ 0-'             CARRAGE CONTROL CHARACTERS           09930000
         SPACE 1                                                        09940000
PAGECTR  DC    PL3'0'                                                   09950000
EDITMASK DC    X'2020202120'                                            09960000
SAVECCC  DS    C                                                        09970000
         SPACE 3                                                        09980000
         LTORG                                                          09990000
         TITLE 'OFFLOAD -- DATA - MESSAGES'                             10000000
ADDCARD  DC    CL80'./       ADD'                                       10010000
         SPACE 3                                                        10020000
         DC    AL1(L'BLNK1)                                             10030000
BLNK1    DC    C'  '                                                    10040000
         SPACE 3                                                        10050000
         DC    AL1(L'BLNK3)                                             10060000
BLNK3    DC    C'- '                                                    10070000
         SPACE 3                                                        10080000
         DC    AL1(L'CTLERR1)                                           10090000
CTLERR1  DC    C' WARNING - THE FOLLOWING CARD(S) RESEMBLE ''IEBUPDTE''*10100000
                CONTROL CARDS'                                          10110000
         SPACE 3                                                        10120000
         DC    AL1(L'CTLERR2)                                           10130000
CTLERR2  DC    C'           THE ''./'' WILL BE REPLACED BY ''><'''      10140000
         SPACE 3                                                        10150000
         DC    AL1(L'IOERPFIX+L'IOERTEXT)                               10160000
IOERPFIX DC    C' I/O ERROR - '                                         10170000
IOERTEXT DS    CL60                                                     10180000
         SPACE 3                                                        10190000
         DC    AL1(L'DRCTYERR+L'DCBDDNAM)                               10200000
DRCTYERR DC    C' I/O ERROR - WHILE READING THE DIRECTORY FOR '         10210000
         DS    CL(L'DCBDDNAM)                                           10220000
         SPACE 3                                                        10230000
         DC    AL1(L'EODERR)                                            10240000
EODERR   DC    C' ERROR - MISSING CONTINUATION CARD'                    10250000
         SPACE 3                                                        10260000
         DC    AL1(L'EOPM)                                              10270000
EOPM     DC    C'-END OF PROGRAM'                                       10280000
         SPACE 3                                                        10290000
         DC    AL1(L'ERRCARD+L'CARD2)                                   10300000
ERRCARD  DC    CL11' '                                                  10310000
CARD2    DS    CL80                                                     10320000
         SPACE 3                                                        10330000
         DC    AL1(L'NOCOREM)                                           10340000
NOCOREM  DC    C' ERROR - THERE IS INSUFFICIENT CORE TO PROCESS THIS RE*10350000
               QUEST'                                                   10360000
         SPACE 3                                                        10370000
         DC    AL1(L'NOSYSIN)                                           10380000
NOSYSIN  DC    C' ERROR - THE SYSIN DD STATEMENT IS MISSING'            10390000
         SPACE 3                                                        10400000
         DC    AL1(NTSELLEN)                                            10410000
NTSELMSG DC    C' WARNING - '                                           10420000
NTSELMBR DS    CL(L'DCBDDNAM)                                           10430000
         DC    C' NOT FOUND'                                            10440000
NTSELLEN EQU   *-NTSELMSG                                               10450000
         SPACE 3                                                        10460000
         DC    AL1(OFFLMLEN)                                            10470000
OFFLMSG  DC    C' OFFLOADING '                                          10480000
OFFLMBRN DS    CL(L'DCBDDNAM)                                           10490000
         DC    C' TO '                                                  10500000
OFFLDDNS DS    CL(6+2*(L'DCBDDNAM))'******** FROM ********'             10510000
OFFLMLEN EQU   *-OFFLMSG                                                10520000
         SPACE 3                                                        10530000
         DC    AL1(L'PGMTERM)                                           10540000
PGMTERM  DC    C'-PROGRAM TERMINATED'                                   10550000
         SPACE 3                                                        10560000
         DC    AL1(L'PRECARD+L'CARD)                                    10570000
PRECARD  DC    CL30' '                                                  10580000
CARD     DS    CL80                                                     10590000
         SPACE 3                                                        10600000
         DC    C' '                                                     10610000
TITLEBUF DC    CL111'1',C'PAGE '                                        10620000
PAGENO   DS    CL5                                                      10630000
         SPACE 3                                                        10640000
         DC    AL1(L'TITLE1-1)                                          10650000
TITLE1   DC    C'1ADS -- OFFLOAD - PDS OFFLOADING PROGRAM'              10660000
         SPACE 3                                                        10670000
         END   OFFLOAD                                                  10680000
//*                                                                     00260000
//LKED    EXEC PGM=IEWL,REGION=1024K,                                   00270000
//             PARM='TEST,XREF,LET,LIST,NCAL',                          00280000
//             COND=(0,LT)                                              00290000
//SYSLMOD  DD  DSN=SYS2.LINKLIB,DISP=SHR                                00300000
//SYSUT1   DD  DSN=&&SYSUT1,DISP=(OLD,DELETE)                           00310000
//SYSPRINT DD  SYSOUT=*                                                 00320000
//SYSLIN   DD  DSN=&&OBJSET,DISP=(OLD,DELETE)                           00330000
//         DD  *                                                        00340000
  NAME OFFLOAD(R)                                                       00350000
//                                                                      00360000
