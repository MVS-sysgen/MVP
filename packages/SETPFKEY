//SETPFKEY  JOB (TSO),
//             'Install SETPFKEY',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
//* ***************************************************************** * 00030000
//* INSTALL SETPFKEY: SETS CONSOLE PFKEYS AT IPL TIME                 * 00040000
//* ***************************************************************** * 00050000
//*                                                                     00060000
//*                                                                     00070000
//ASML    EXEC ASMFCL,PARM.ASM='LIST,NODECK,OBJ,TERM',                  00080000
//             MAC1='SYS1.AMODGEN'                                      00090000
//ASM.SYSIN DD *                                                        00100000
*          DATA SET CBT1115    AT LEVEL 005 AS OF 10/23/80              00010000
         PRINT OFF                                                      00020000
         MACRO                                                          00030000
&NAME    INIT  &RG=1,&PC=,&REQ=,&PATCH=,&SAVE=,&PARM=,&PGM=,&ENTRY      00040000
         GBLB  &ACSC1(8)                                                00050000
         GBLA  &ACSC2(4)                                                00060000
         GBLC  &ACSC3(4)                                                00070000
         LCLA  &A1,&A2,&A3,&A4                                          00080000
         SPACE 2                                                        00090000
*********************************************************************** 00100000
*        INIT MACRO - PROGRAM ENTRY AND HOUSEKEEPING                  * 00110000
*********************************************************************** 00120000
         SPACE 2                                                        00130000
         AIF   (T'&REQ EQ 'O').REGS                                     00140000
         AIF   ('&REQ' EQ 'NO').EREGS                                   00150000
         MNOTE 0,'INVALID ''REQ'' PARAM, NO ASSUMED'                    00160000
         AGO   .EREGS                                                   00170000
.REGS    ANOP                                                           00180000
.* ISSUE EQUR MACRO FOR REGISTER EQUATED SYMBOLS                        00190000
         EQUATE                                                         00200000
.EREGS   ANOP                                                           00210000
.* CHECK PC AND SET APPROPRIATE SWITCH                                  00220000
         AIF   (T'&PC EQ 'O').NOPCX                                     00230000
         AIF   ('&PC' NE 'ARM' AND '&PC' NE 'YES').NOPC                 00240000
&ACSC1(1) SETB 1                                                        00250000
         AGO   .NOPCX                                                   00260000
.NOPC    ANOP                                                           00270000
         MNOTE 0,'INVALID PC, IGNORED'                                  00280000
.NOPCX   AIF   (T'&ENTRY EQ 'O').NOENTRY                                00290000
         AIF   ('&ENTRY' EQ 'ENTRY').ENTOK                              00300000
         MNOTE 4,'INVALID POSITIONAL OPERAND, NO ENTRY GENERATED'       00310000
         AGO   .NOENTRY                                                 00320000
.ENTOK   ANOP                                                           00330000
         ENTRY &NAME                                                    00340000
.NOENTRY ANOP                                                           00350000
&NAME    DS    0H                                                       00360000
*---------------------------------------------------------------------* 00370000
*        ISSUE SAVE MACRO                                             * 00380000
*---------------------------------------------------------------------* 00390000
         SAVE  (14,12),,&SYSECT-&SYSDATE-&SYSTIME                       00400000
         SPACE 2                                                        00410000
*---------------------------------------------------------------------* 00420000
*        SAVE PARM, GET SAVE AREA, SET UP BASE REGS                   * 00430000
*---------------------------------------------------------------------* 00440000
         SPACE 2                                                        00450000
         LR    2,1                      SAVE PASSED PARAMS              00460000
.NPARM1  AIF   (T'&SAVE EQ 'O').NOSAVE                                  00470000
         USING &NAME,15                 SET UP BASE                     00480000
&ACSC3(1) SETC '&SAVE'                  SAVE LENGTH                     00490000
         LA    0,&SAVE+72+&ACSC1(1)*4   SET GETMAIN LENGTH              00500000
* ISSUE GETMAIN FOR SAVE AREA AND WORK SPACE                            00510000
         GETMAIN R,LV=(0)                                               00520000
.CHAIN   ST    13,4(1)                  SAVE BACKWARD POINTER           00530000
         ST    1,8(13)                  SAVE FORWARD POINTER            00540000
         LR    13,1                     SET SAVE AREA                   00550000
         DROP  15                                                       00560000
         AGO   .ADDRS                                                   00570000
.NOSAVE  USING &NAME,15                 SET UP BASE                     00580000
         CNOP  0,4                      SET ON BOUNDRY                  00590000
         BAL   1,*+76+&ACSC1(1)*4       SET REG SAVE PLUS WORK AREA     00600000
         USING *,13                                                     00610000
         DS    18F                      SAVE AREA                       00620000
         AIF   (NOT &ACSC1(1)).CHAIN                                    00630000
         DS    F                        SPIE SAVE AREA                  00640000
         AGO   .CHAIN                                                   00650000
.ADDRS   AIF   (T'&SAVE EQ 'O').NSAV1                                   00660000
         AIF   (T'&RG NE 'O').OKBASE                                    00670000
         MNOTE 4,'YOU REQUESTED NO BASE REGISTERS, WILL GIVE YOU ONE'   00680000
&A1      SETA  1                                                        00690000
         AGO   .NOBASE                                                  00700000
.OKBASE  ANOP                                                           00710000
&A1      SETA  &RG                                                      00720000
.NOBASE  ANOP                                                           00730000
&A2      SETA  11                                                       00740000
&A3      SETA  1                                                        00750000
&A4      SETA  0                                                        00760000
         BALR  12,0                     SET BASE REG                    00770000
         USING *,12                                                     00780000
.ADRLP   ANOP                                                           00790000
&A1      SETA  &A1-1                                                    00800000
         AIF   (&A1 EQ 0).EADDR                                         00810000
         LA    &A2,4095(&A2+1)          SET MORE BASES                  00820000
         USING *+4095*&A3-&A4-4*&A3-&ACSC1(1)*4,&A2                     00830000
&A3      SETA  &A3+1                                                    00840000
&A2      SETA  &A2-1                                                    00850000
         AGO   .ADRLP                                                   00860000
.NSAV1   ANOP                                                           00870000
         AIF   (T'&RG EQ 'O').OKBASE1                                   00880000
         AIF   (T'&RG NE 'O').OKBASE1                                   00890000
         MNOTE 4,'YOU REQUESTED NO BASE REGISTERS, WILL GIVE YOU ONE'   00900000
&A1      SETA  1                                                        00910000
         AGO   .NOBASE1                                                 00920000
.OKBASE1 ANOP                                                           00930000
&A1      SETA  &RG                                                      00940000
.NOBASE1 ANOP                                                           00950000
&A2      SETA  12                                                       00960000
&A3      SETA  1                                                        00970000
&A4      SETA  82                                                       00980000
         AGO   .ADRLP                                                   00990000
.EADDR   AIF   (T'&PARM EQ 'O').PATCHS                                  01000000
         SPACE 2                                                        01010000
*---------------------------------------------------------------------* 01020000
*        SAVE PARM ADDRESS OR INFO                                    * 01030000
*---------------------------------------------------------------------* 01040000
         SPACE 2                                                        01050000
         AIF   ('&PARM(2)' NE 'ADDR').NPARM2                            01060000
         ST    2,&PARM(1)               SAVE PARM ADDRESS POINTER       01070000
         AGO   .PATCHS                                                  01080000
.NPARM2  AIF   ('&PARM(2)' NE 'DATA').NPARM3                            01090000
         L     1,0(0,2)                 GET PARM ADDRESS                01100000
         LH    3,0(0,1)                 GET LENGTH                      01110000
         LA    3,1(0,3)                 SET FOR MVC                     01120000
         EX    3,*+8                    DO THE MOVE                     01130000
         B     *+10                     GO AROUND                       01140000
         MVC   &PARM(1).(0),0(1)        EXECUTED MOVE                   01150000
         AGO   .PATCHS                                                  01160000
.NPARM3  MNOTE 4,'INVALID ''PARM'' PARAM, NO INFO SAVED'                01170000
.PATCHS  AIF   ('&PATCH' EQ 'NO').LEAVE                                 01180000
         AIF   ('&PATCH' NE 'YES').LEAVE                                01190000
         SPACE 2                                                        01200000
*---------------------------------------------------------------------* 01210000
*        PATCH AREA                                                   * 01220000
*---------------------------------------------------------------------* 01230000
         B     *+104                    GO AROUND                       01240000
         NOP   *                        SET UP ADDRESS INDICATOR        01250000
         DC    96X'00'                  CLEAR PATCH AREA                01260000
.LEAVE   AIF   ('&PC' NE 'YES').NPCYES                                  01270000
         SPACE 2                                                        01280000
*---------------------------------------------------------------------* 01290000
*        ISSUE SPIEPC MACRO                                           * 01300000
*---------------------------------------------------------------------* 01310000
         SPACE 2                                                        01320000
         SPIEPC                                                         01330000
.NPCYES ANOP                                                            01340000
         SPACE 2                                                        01350000
         AIF   (T'&PGM EQ 'O').OUT                                      01360000
*        ISSUE WTO FOR PROGRAM NAME                                     01370000
         SPACE                                                          01380000
         WTO   '&PGM EXECUTING',ROUTCDE=2                               01390000
         SPACE                                                          01400000
.OUT     ANOP                                                           01410000
         LR 1,2                         RESTORE PARM INFO               01420000
         SPACE 2                                                        01430000
         MEND                                                           01440000
         MACRO                                                          01450000
&NAME    LEAVE &SAVE=,&RC=0,&RECREG=                                    01460000
*        COPY  ACSCGBLS                                                 01470000
         GBLB  &ACSC1(8)                                                01480000
         GBLA  &ACSC2(4)                                                01490000
         GBLC  &ACSC3(4)                                                01500000
         SPACE 2                                                        01510000
*********************************************************************** 01520000
*        LEAVE MACRO                                                  * 01530000
*********************************************************************** 01540000
         SPACE 2                                                        01550000
&NAME    DS    0H                                                       01560000
         AIF   (NOT &ACSC1(2)).NOSP1                                    01570000
*---------------------------------------------------------------------* 01580000
*        RESET SPIE ROUTINE                                           * 01590000
*---------------------------------------------------------------------* 01600000
         SPACE 2                                                        01610000
         L     1,&ACSC3(1)+72(13)       GET SAVED ADDRESS               01620000
* ISSUE SPIE MACRO                                                      01630000
         SPIE  MF=(E,(1))                                               01640000
         SPACE 2                                                        01650000
.NOSP1   ANOP                                                           01660000
*---------------------------------------------------------------------* 01670000
*        RESET SAVE AREA AND EXIT                                     * 01680000
*---------------------------------------------------------------------* 01690000
         SPACE 2                                                        01700000
         AIF   (T'&SAVE EQ 'O').NSAV                                    01710000
         LR    1,13                     SET FOR RELEASE                 01720000
.NSAV    L     13,4(13)                 UNCHAIN SAVE AREA               01730000
         AIF   (T'&SAVE EQ 'O').NSAV1                                   01740000
* ISSUE FREEMAIN MACRO                                                  01750000
         LA    0,&SAVE+72+&ACSC1(1)*4   GET LENGTH OF AREA              01760000
         FREEMAIN R,LV=(0),A=(1)                                        01770000
.NSAV1   AIF   (T'&RECREG EQ 'O').RET                                   01780000
         AIF   ('&RECREG'(1,1) EQ '(').RECOK                            01790000
         MNOTE 4,'INVALID RECREG, NOT REGISTER NOTATION, IGNORED'       01800000
         AGO   .RET                                                     01810000
.RECOK   ST    &RECREG(1),24(13)        SAVE RECREG IN R1 AREA          01820000
.RET     RETURN (14,12),T,RC=&RC                                        01830000
         SPACE 2                                                        01840000
         MEND                                                           01850000
         MACRO                                                          01860000
         EQUATE                                                         01870000
**                           EQUATES FOR SYMBOLIC REG USAGE             01880000
R0       EQU   0                                                        01890000
R1       EQU   1                                                        01900000
R2       EQU   2                                                        01910000
R3       EQU   3                                                        01920000
R4       EQU   4                                                        01930000
R5       EQU   5                                                        01940000
R6       EQU   6                                                        01950000
R7       EQU   7                                                        01960000
R8       EQU   8                                                        01970000
R9       EQU   9                                                        01980000
R10      EQU   10                                                       01990000
R11      EQU   11                                                       02000000
R12      EQU   12                                                       02010000
R13      EQU   13                                                       02020000
R14      EQU   14                                                       02030000
R15      EQU   15                                                       02040000
RA       EQU   10                                                       02050000
RB       EQU   11                                                       02060000
RC       EQU   12                                                       02070000
RD       EQU   13                                                       02080000
RE       EQU   14                                                       02090000
RF       EQU   15                                                       02100000
         MEND                                                           02110000
         PRINT ON                                                       02120000
PFK      TITLE 'PFK CREATION - MODIFICATION PROGRAM'                    02130000
****DATE - 10/24/78**************************************************** 02140000
*  PROGRAMMER NAME -                                                  * 02150000
*                                                                     * 02160000
*         BARRY GOLDBERG                                              * 02170000
*         SYSTEMS PROG.                                               * 02180000
*         TECHNICAL SUPPORT                                           * 02190000
*         213-741-4875                                                * 02200000
*         AUTO CLUB OF SOUTHERN CALIFORNIA                            * 02210000
*         2601 S. FIGUEROA                                            * 02220000
*         LOS ANGELES, CA. 90007                                      * 02230000
*  REQUESTED BY -                                                     * 02240000
*                                                                     * 02250000
*         BARRY GOLDBERG                                              * 02260000
*  EFFECTIVE DATE -                                                   * 02270000
*                                                                     * 02280000
*        SOON                                                         * 02290000
*  PARM USAGE -                                                       * 02300000
*                                                                     * 02310000
*        NA                                                           * 02320000
*  REGISTER USAGE -                                                   * 02330000
*                                                                     * 02340000
*        SEE TEXT                                                     * 02350000
*  SWITCHES -                                                         * 02360000
*                                                                     * 02370000
*        NA                                                           * 02380000
*  INPUT FILES -                                                      * 02390000
*                                                                     * 02400000
*        SYSIN - PFK CONTROL                                          * 02410000
*  OUTPUT FILES -                                                     * 02420000
*                                                                     * 02430000
*        NEW PFK AREA IN STORAGE                                      * 02440000
*  MODULES USED -                                                     * 02450000
*                                                                     * 02460000
*        NA                                                           * 02470000
*  ENVIROMENTS  -                                                     * 02480000
*                                                                     * 02490000
*        MVS                                                          * 02500000
*        VS1                                                          * 02510000
*  DESCRIPTION -                                                      * 02520000
*                                                                     * 02530000
*        THIS ROUTINE WILL MODIFY THE PFK AREAS IN MAIN               * 02540000
*        STORAGE.  IN ORDER TO MAKE THEM PERMANENT,                   * 02550000
*        THE OPERATOR WILL HAVE TO MAKE A                             * 02560000
*        REAL PFK DEFINITION.                                         * 02570000
*        NOTE -- THIS PROGRAM MUST RUN AUTHORIZED OR KEY ZERO.        * 02580000
*                                                                     * 02590000
*   PROGRAM DESCRIPTION -                                             * 02600000
*         SSP79PFK WILL LOAD THE RESIDENT PFK AREA FROM CARD IMAGE    * 02610000
*         INPUT.                                                      * 02620000
*         THE OPERATORS MAY MAKE THE CHANGES PERMANENT AND UPDATE     * 02630000
*         SYS1.DCMLIB BY MEARLY UPDATING ANY PFK.                     * 02640000
*         IT IS SUGGESTED THAT THE SIMPLEST ONE BE UPDATED.  THIS WILL* 02650000
*         CAUSE THE IEEPFKEY MEMBER TO BE REWRITTEN WITH ALL PFK'S.   * 02660000
*                                                                     * 02670000
*INPUT RECORD FORMAT:                                                 * 02680000
*                                                                     * 02690000
*   COL               FUNCTION                                        * 02700000
*                                                                     * 02710000
*  1 - 2             CONSOLE ID FOR PFK                               * 02720000
*  3 - 4             PFK #                                            * 02730000
*    5               PFK CONTROL                                      * 02740000
*                    BLANK  =  NULL PFK ENTRY                         * 02750000
*                       N   =  NON CONVERSATIONAL                     * 02760000
*                       Y   =  CONVERSATIONAL                         * 02770000
*  7 - 71            THE COMMAND AS IT WOULD BE ISSUED AND IF         * 02780000
*                     MULTIPLE, SEPARATED WITH A SEMICOLON. ALSO      * 02790000
*                     UNDERSCORE ALLOWED (TAKES A POSITION).          * 02800000
*   72               CONTINUATION COLUMN IF COMMAND(S) REQUIRE        * 02810000
*                     ADDITIONAL SPACE ON ANOTHER CARD.               * 02820000
*   CARD # 2 -                                                        * 02830000
*   16-55            CONTINUATION OF COMMAND IF PREVIOUS CARD         * 02840000
*                     IS NONBLANK IN COL 72.                          * 02850000
*                                                                     * 02860000
*NOTE :                                                               * 02870000
*         1) MAXIMUM LENGTH OF ALL COMMANDS INCLUDING SPECIAL         * 02880000
*               CHARACTERS AND BLANKS IS 105 CHARACTERS.              * 02890000
*         2) THIS PROGRAM MUST RUN AUTHORIZED OR AS A SYSTEM TASK.    * 02900000
*                                                                     * 02910000
*EXAMPLE:                                                             * 02920000
*                                                                     * 02930000
*  //TSGBLGAC JOB 269,GOLDBERG,CLASS=X                                  02940000
*  //PFK   EXEC  PGM=SSP79PFK                                           02950000
*  //SYSIN DD    *                                                      02960000
*  0101N K E,1                                                          02970000
*  0102Y V 020,CONSOLE,ROUT=(1,2,3,4,5,6,8,9,10,11,12,14,15)            02980000
*  0103Y S SSP98LBL.P3;S SSP97LBL.P3                                    02990000
*  0104Y V 520,CONSOLE,ROUT=(1,2,3,4,5)                                 03000000
*  0105Y S LBLREADC.P3;S LBLREAD.P3,ID=_X                               03010000
*  0106Y V 301,CONSOLE,ROUT=7,AUTH=ALL;V 300,CONSOLE,ROUT=3,AUTH=INFO   03020000
*  0107                                                                 03030000
*  0108Y S MSC1010P.P3,RUN=_X                                           03040000
*  0109Y CENOUT C=A,J=_                                                 03050000
*  0110Y S MSC9010P.P3,RUN=_                                            03060000
*  0111Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N                       03070000
*  0112Y S SSP93CPY.P3,NEWVOL='SER=_CAPS'                               03080000
*  0201N K E,1                                                          03090000
*  0202                                                                 03100000
*  0203                                                                 03110000
*  0204Y V 020,MSTCONS                                                  03120000
*  0205                                                                 03130000
*  0206                                                                 03140000
*  0207Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              03150000
*  0208N D A                                                            03160000
*  0209N D N=SOUT                                                       03170000
*  0210N D R                                                            03180000
*  0211N D N                                                            03190000
*  0212N K                                                              03200000
*  0301N K E,1                                                          03210000
*  0302Y S DUMWTR.P                                                     03220000
*  0303Y S IRPT.P3;S IRPTJ.P3                                           03230000
*  0304Y V 002,ONLINE;V 00E,ONLINE;V 011,ONLINE;V 010,ONLINE            03240000
*  0305Y SF PRINT,002,,_;SF PRINT,00E;SF PUNCH,011                      03250000
*  0306Y SF RDR,010                                                     03260000
*  0307Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              03270000
*  0308N D A                                                            03280000
*  0309N D N                                                            03290000
*  0310Y D Q;D R                                                        03300000
*  0311N D N=SOUT                                                       03310000
*  0312N K                                                              03320000
*  0401N K E,1                                                          03330000
*  0402Y V 017,CONSOLE,ROUT=(1,2,3,4,5,6,8,9,10,11,12,14,15)            03340000
*  0403Y S SSP98LBL.P3;S SSP97LBL.P3                                    03350000
*  0404                                                                 03360000
*  0405Y S LBLREADC.P3;S LBLREAD.P3,ID=_X                               03370000
*  0406Y V 301,CONSOLE,ROUT=7,AUTH=ALL;V 300,CONSOLE,ROUT=3,AUTH=INFO   03380000
*  0407                                                                 03390000
*  0408Y S MSC1010P.P3,RUN=_X                                           03400000
*  0409Y CENOUT C=A,J=_                                                 03410000
*  0410Y S MSC9010P.P3,RUN=_                                            03420000
*  0411Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              03430000
*  0412Y S SSP93CPY.P3,NEWVOL='SER=_CAPS'                               03440000
*  0501N K E,1                                                          03450000
*  0502                                                                 03460000
*  0503                                                                 03470000
*  0504                                                                 03480000
*  0505                                                                 03490000
*  0506                                                                 03500000
*  0507Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              03510000
*  0508                                                                 03520000
*  0509                                                                 03530000
*  0510                                                                 03540000
*  0511                                                                 03550000
*  0512                                                                 03560000
*                                                                       03570000
****DATE - 10/24/78**************************************************** 03580000
         EJECT                                                          03590000
SETPFKEY START 0                                                  *JLM* 03600000
XX       INIT  PGM=SETPFKEY                                       *JLM* 03610000
         SPACE                                                          03620000
         OPEN  SYSIN                                                    03630000
         SPACE 2                                                        03640000
         MODESET KEY=ZERO                                               03650000
         SPACE                                                          03660000
G1       EQU   *                                                        03670000
         SPACE                                                          03680000
**********************************************************************  03690000
*        THIS IS THE LOOP START                                       * 03700000
*        INITIALIZE THE PFK AREA AND READ/FILL WITH DATA              * 03710000
**********************************************************************  03720000
         SPACE                                                          03730000
         MVI   CTLLINE,C' '             BLANK OUT IMAGE                 03740000
         MVC   CTLLINE+1(L'CTLLINE-1),CTLLINE                           03750000
         XC    CTLIND,CTLIND            AND INDICATORS                  03760000
G1A      EQU   *                                                  *JLM* 03761000
         GET   SYSIN                    GET A RECORD                    03770000
         CLI   0(R1),C'*'               IS THIS A COMMENT         *JLM* 03771000
         BE    G1A                        YES, IGNORE             *JLM* 03772000
         LR    R3,R1                    POINT TO IT                     03780000
         PACK  DBL,0(2,R3)              GET CONSOLE ID                  03790000
         CVB   R1,DBL                                                   03800000
         STH   R1,CONID                 AND SAVE IT                     03810000
         PACK  DBL,2(2,R3)              GET PFK NUMBER                  03820000
         CVB   R1,DBL                                                   03830000
         STH   R1,PFKNO                 SAVE IT FOR LATER               03840000
         CLI   4(R3),C' '               NULL ENTRY?                     03850000
         BE    SETX                     YES, WE ARE FINISHED            03860000
         OI    CTLIND,ACTIVE            INDICATE ACTIVE ENTRY           03870000
         CLI   4(R3),C'N'               IS IT NON CONVERSATIONAL        03880000
         BE    SET1                     YES                             03890000
         CLI   4(R3),C'Y'               COONVERSATIONAL                 03900000
         BNE   SET1                     TOO BAD, IGNORE                 03910000
         OI    CTLIND,CONY              SET CONVERSATIONAL BIT          03920000
         SPACE 2                                                        03930000
SET1     EQU   *                                                        03940000
         MVC   CTLLINE1,6(R3)           SET COMMAND                     03950000
         CLI   71(R3),C' '              ANY CONTINUATION?               03960000
         BE    SETX                     NO                              03970000
G2A      EQU   *                                                  *JLM* 03971000
         GET   SYSIN                                                    03980000
         CLI   0(R1),C'*'               IS THIS A COMMENT         *JLM* 03981000
         BE    G2A                        YES, IGNORE             *JLM* 03982000
         MVC   CTLLINE2,15(R1)          GET REST OF THE COMMAND         03990000
         SPACE 2                                                        04000000
**********************************************************************  04010000
*        DATA AREA NOW COMPLETE, CHECK AND SET PFK                    * 04020000
**********************************************************************  04030000
         SPACE                                                          04040000
SETX     EQU   *                                                        04050000
         L     R15,CVTPTR               POINT TO CVT                    04060000
         USING CVT,R15                                                  04070000
         L     R15,CVTCUCB                                              04080000
         USING UCM,R15                                                  04090000
         LM    R3,R5,UCMVDATA           SET TO SEARCH                   04100000
         DROP  R15                                                      04110000
         LH    R6,CONID                 GET CON NUMBER                  04120000
SRCH1    EQU   *                                                        04130000
         BCT   R6,SRCH2                 NEXT ONE?                       04140000
         B     SRCH3                    CONTINUE TO LOOK                04150000
SRCH2    EQU   *                                                        04160000
         BXLE  R3,R4,SRCH1              GO GET MORE                     04170000
         B     G1                       NOT HERE, IGNORE                04180000
         SPACE 2                                                        04190000
SRCH3    EQU   *                                                        04200000
**********************************************************************  04210000
*        HAVE FOUND THE ENTRY, NOW MUST GO TO                         * 04220000
*        GET THE PFK AREA                                             * 04230000
**********************************************************************  04240000
         SPACE 2                                                        04250000
         USING UCMLIST,R3                                               04260000
         ICM   R15,B'1111',UCMXB        DCM ADDRESS                     04270000
         BZ    G1                       NOT HERE                        04280000
         DROP  R3                                                       04290000
         USING DCM,R15                                                  04300000
         ICM   R15,B'1111',DCMADPFK                                     04310000
         BZ    G1                       NOT HERE, IGNORE                04320000
         SPACE 2                                                        04330000
**********************************************************************  04340000
*        WE ARE NOW POINTING TO THE PFK AREA                          * 04350000
**********************************************************************  04360000
         SPACE                                                          04370000
         LH    R4,PFKNO                 GET PFK NUMBER                  04380000
PFK1     EQU   *                                                        04390000
         BCT   R4,NXTPFK                GET ANOTHER                     04400000
         CLC   0(1,R15),PFKNO+1         IS IT THE SAME?                 04410000
         BNE   G1                       NO, ERROR                       04420000
         MVC   1(PFKLEN-1,R15),CTLIND   SET UP THE PFK                  04430000
         B     G1                                                       04440000
         SPACE                                                          04450000
NXTPFK   EQU   *                                                        04460000
         LA    R15,PFKLEN(R15)          POINT TO NEXT ONE               04470000
         CLI   0(R15),100               END?                            04480000
         BE    G1                       YES, NOT HERE                   04490000
         B     PFK1                     GO LOOK AGAIN                   04500000
         SPACE 2                                                        04510000
EOFIN    EQU   *                                                        04520000
         MODESET KEY=NZERO                                        *JLM* 04521000
         CLOSE SYSIN                                                    04530000
         LEAVE                                                          04540000
         SPACE 2                                                        04550000
         LTORG                                                          04560000
         SPACE                                                          04570000
DBL      DS    D                        A WORK AREA                     04580000
CONID    DS    H                        REQUESTED CONSOLE               04590000
PFKNO    DS    H                        REQUESTED PFK NUMBER            04600000
         DS    0D                                                       04610000
CTLIND   DS    X                        CONTROL BYTE                    04620000
ACTIVE   EQU   X'80'                                                    04630000
CONY     EQU   X'20'                    PFK IS CONVERSATIONAL           04640000
CTLLINE  DS    0CL108                   LENGTH OF DATA                  04650000
CTLLINE1 DS    CL65                     FIRST CARD IMAGE AMOUNT         04660000
CTLLINE2 DS    CL40                     NEXT AMOUNT (ALLOW ONLY 105)    04670000
         DS    CL3                      THE REST                        04680000
         SPACE 2                                                        04690000
PFKLEN   EQU   110                      LENGTH OF A PFK ANTRY           04700000
         SPACE 2                                                        04710000
SYSIN    DCB   DDNAME=SYSIN,MACRF=GL,EODAD=EOFIN,DSORG=PS               04720000
         SPACE 2                                                        04730000
         CVT   SYS=AOS1,DSECT=YES                                       04740000
         IEECUCM SYS=AOS1,FORMAT=NEW                                    04750000
DCM      DSECT                                                          04760000
         IEECRDCM DSECT=YES                                             04770000
         SPACE 2                                                        04780000
         END                                                            04790000
//ASM.SYSTERM DD SYSOUT=*                                               00120000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             00130000
//LKED.SYSIN DD *                                                       00140000
  SETCODE AC(1)                                                         00150000
  NAME SETPFKEY(R)                                                      00160000
//                                                                      00170000
