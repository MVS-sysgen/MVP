//FIND  JOB (TSO),
//             'Install FIND',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* ********************************************************
//* *  INSTALL THE 'FIND' TSO COMMAND                    *
//* ********************************************************
//ASSEM    EXEC ASMFCL,COND=(0,NE),
//         PARM.ASM='OBJECT,NODECK,LIST,RENT',
//         PARM.LKED='LIST,MAP,RENT,REUS,REFR',
//         MAC1='SYS1.AMODGEN'
//SYSIN    DD *
         MACRO                                                          00220000
         EXIT  &RC=,&R=                                                 00230000
.*                                                                      00240000
.*--------------------------------------------------------------------  00250000
.*                                                                      00260000
.*       MACRO = EXIT                                                   00270000
.*                                                                      00280000
.*       AUTHOR = VINH VU                                               00290000
.*                                                                      00300000
.*       DATE-WRITTEN = 09/02/81                                        00310000
.*                                                                      00320000
.*       PURPOSE = TO EXIT FROM A MODULE                                00330000
.*             THIS HAS TO BE USED IN CONJUNCTION WITH MACRO            00340000
.*             SET WHICH IS THE INITIALIZATION MACRO.                   00350000
.*                                                                      00360000
.*       EXAMPLES :                                                     00370000
.*                                                                      00380000
.*         *   EXIT RC=10                                               00390000
.*                                                                      00400000
.*             THIS WILL GENERATE A RETURN CODE OF 10. NO FREEMAIN      00410000
.*                WILL BE DONE                                          00420000
.*                                                                      00430000
.*         *   EXIT R=R9                                                00440000
.*                                                                      00450000
.*             THIS WILL GENERATE RETURN CODE FROM CONTENTS OF REG 9.   00460000
.*                A FREEMAIN WILL BE DONE ALSO ASSUMING ADDRESS OF      00470000
.*                GETMAINED AREA IS IN REG13 (DONE IN MACRO SET).       00480000
.*                                                                      00490000
.*--------------------------------------------------------------------  00500000
.*                                                                      00510000
         GBLC  &SETEXIT                                                 00520000
         AIF   ('&SETEXIT' EQ '').NOFREE                                00530000
         LR    R1,R13              GET ADDR OF GMAREA INTO R1           00540000
         L     R13,4(,R13)         GET PREVIOUS SAVE AREA               00550000
         FREEMAIN R,LV=&SETEXIT,A=(R1)                                  00560000
         AGO   .FREEIT                                                  00570000
.NOFREE  ANOP                                                           00580000
         L     R13,4(,R13)         GET PREVIOUS SAVE AREA               00590000
.FREEIT  ANOP                                                           00600000
         AIF   (T'&RC EQ 'O').CKR                                       00610000
         LA    R15,&RC             GET RETURN CODE                      00620000
         AGO   .THRU                                                    00630000
.CKR     ANOP                                                           00640000
         AIF   (T'&R EQ 'O').NONE                                       00650000
         LR    R15,&R              GET RETURN CODE                      00660000
         AGO   .THRU                                                    00670000
.NONE    ANOP                                                           00680000
         SR    R15,R15             RETURN CODE IS ZERO                  00690000
.THRU    ANOP                                                           00700000
         L     R14,12(0,R13)       GET R14                              00710000
         LM    R0,R12,20(R13)      GET REG 0 - 12                       00720000
         BR    R14                 RETURN TO CALLER                     00730000
         MEND                                                           00740000
         MACRO                                                          00750000
         SET   &BASE,&LV=,&DSECT=,&REG=                                 00760000
.*                                                                      00770000
.*--------------------------------------------------------------------  00780000
.*                                                                      00790000
.*       MACRO = SET                                                    00800000
.*                                                                      00810000
.*       AUTHOR = VINH VU                                               00820000
.*                                                                      00830000
.*       DATE-WRITTEN = 08/31/81                                        00840000
.*                                                                      00850000
.*       PURPOSE = TO SET UP BASE REGS FOR A SOURCE MODULE.             00860000
.*             IF LV IS OMITTED, PROGRAM IS ASSUMED NOT TO BE           00870000
.*             RE-ENTRANT. USE EXIT MACRO IN CONJUNCTION WITH THIS      00880000
.*             MACRO.                                                   00890000
.*             FOR RE-ENTRANT MODULE, 18F AS SAVE AREA WILL             00900000
.*             BE ASSUMED TO BE FIRST. PAY ATTENTION TO THIS            00910000
.*             WHEN YOU SET UP THE DSECT.                               00920000
.*                                                                      00930000
.*       EXAMPLES =                                                     00940000
.*                                                                      00950000
.*         *   SET (R12,R11),LV=WKLENG,DSECT=WKAREA                     00960000
.*                                                                      00970000
.*             R12 AND R11 WILL BE BASE REGISTERS.                      00980000
.*             LV WILL ASSUME A RE-ENTRANT MODULE AND THIS IS THE       00990000
.*                LENGTH OF THE GETMAIN AREA BEING OBTAINED (WHICH      01000000
.*                ALSO INCLUDES THE SAVE AREA OF 18F THAT WILL BE       01010000
.*                AT THE BEGINNING OF THE GETMAINED AREA.)              01020000
.*                THE GETMAINED DSECT SHOULD LOOK LIKE :                01030000
.*                           WKAREA    DSECT                            01040000
.*                                     DS    18F                        01050000
.*                           USER1     DS    CL256                      01060000
.*                                     ETC   ........                   01070000
.*                                                                      01080000
.*         *   SET LV=WKLENG                                            01090000
.*                                                                      01100000
.*             THIS ASSUMES R12 AS BASE REGISTER, A RE-ENTRANT          01110000
.*                MODULE, BUT YOU WILL HAVE TO CODE THE USING           01120000
.*                STATEMENT FOR THE GETMAIN AREA DSECT                  01130000
.*                                                                      01140000
.*         *   SET                                                      01150000
.*                                                                      01160000
.*             THIS ASSUMES A NON-RE-ENTRANT MODULE, R12 WILL BE        01170000
.*                BASE REGISTER.                                        01180000
.*                                                                      01190000
.*--------------------------------------------------------------------  01200000
.*                                                                      01210000
         LCLA  &C,&TEST,&ORG,&CSLENG                                    01220000
         LCLC  &B,&SAVE                                                 01230000
         GBLC  &SETEXIT                                                 01240000
&SETEXIT SETC  ''                                                       01250000
&ORG     SETA  0                                                        01260000
&TEST    SETA  1                                                        01270000
         AIF   (T'&BASE EQ 'O').DEF                                     01280000
&C       SETA  N'&BASE                                                  01290000
&B       SETC  '&BASE(&TEST)'                                           01300000
         AGO   .EQU                                                     01310000
.DEF     ANOP                                                           01320000
&C       SETA  1                                                        01330000
&B       SETC  'R12'                                                    01340000
.EQU     ANOP                                                           01350000
         AIF   (T'&REG EQ 'O').SETREG                                   01360000
         AGO   .REGDONE                                                 01370000
.SETREG  ANOP                                                           01380000
R0       EQU   0                                                        01390000
R1       EQU   1                                                        01400000
R2       EQU   2                                                        01410000
R3       EQU   3                                                        01420000
R4       EQU   4                                                        01430000
R5       EQU   5                                                        01440000
R6       EQU   6                                                        01450000
R7       EQU   7                                                        01460000
R8       EQU   8                                                        01470000
R9       EQU   9                                                        01480000
R10      EQU   10                                                       01490000
R11      EQU   11                                                       01500000
R12      EQU   12                                                       01510000
R13      EQU   13                                                       01520000
R14      EQU   14                                                       01530000
R15      EQU   15                                                       01540000
*                                                                       01550000
.REGDONE ANOP                                                           01560000
         B     28(R15)             BRANCH AROUND                        01570000
&CSLENG  SETA  K'&SYSECT                                                01580000
         DC    AL2(&CSLENG)        LENG OF CSECT NAME                   01590000
         DC    CL8'&SYSECT'        CSECT NAME                           01600000
         DC    CL8'&SYSDATE'       THIS IS DATE                         01610000
         DC    CL6'&SYSTIME'       THIS IS TIME                         01620000
         CNOP  0,4                 GET ALIGNMENT                        01630000
         STM   R14,R12,12(R13)     SAVE PREVIOUS REGS                   01640000
         LR    &B,R15              GET BASE REG SETUP                   01650000
         USING &SYSECT,&B          ESTABLISH ADDRESABILITY              01660000
.LOOP    ANOP                                                           01670000
         AIF   (&TEST EQ &C).THRU                                       01680000
&SAVE    SETC  '&BASE(&TEST)'                                           01690000
&ORG     SETA  &ORG+4096                                                01700000
&TEST    SETA  &TEST+1                                                  01710000
&B       SETC  '&BASE(&TEST)'                                           01720000
         USING &SYSECT+&ORG,&B                                          01730000
         LA    &B,4095(,&SAVE)                                          01740000
         LA    &B,1(,&B)                                                01750000
         AGO   .LOOP                                                    01760000
.THRU    ANOP                                                           01770000
         AIF   (T'&LV EQ 'O').NORENT                                    01780000
&SETEXIT SETC  '&LV'                                                    01790000
         GETMAIN R,LV=&LV                                               01800000
         ST    R13,4(,R1)          SAVE REG13 FROM PREVIOUS             01810000
         L     R15,24(,R13)        SAVE REG 1                           01820000
         ST    R1,8(,R13)          SAVE MY REG13 IN PREV SAVE AREA      01830000
         LR    R13,R1              GET MY REG 13 READY                  01840000
         LR    R1,R15              GET REG1 BACK                        01850000
         AIF   (T'&DSECT EQ 'O').FINAL                                  01860000
         USING &DSECT,R13          ESTABLISH ADDRESSABILITY             01870000
.FINAL   ANOP                                                           01880000
         MEXIT                                                          01890000
.NORENT  ANOP                                                           01900000
         CNOP  0,4                 GET ALIGNMENT                        01910000
         BAL   R1,*+76             BRANCH AROUND                        01920000
         DS    18F                 THIS IS MY SAVE AREA                 01930000
         ST    R13,4(,R1)          SAVE REG13 FROM PREVIOUS             01940000
         L     R15,24(,R13)        SAVE REG 1                           01950000
         ST    R1,8(,R13)          SAVE MY REG13 IN PREV SAVE AREA      01960000
         LR    R13,R1              GET MY REG 13 READY                  01970000
         LR    R1,R15              GET REG1 BACK                        01980000
         MEND                                                           01990000
FIND     TITLE 'FIND     --- TO SEARCH FOR AN ARGUMENT IN A PDS'        00000010
*                                                                       00000020
*-------------------------------------------------------------------    00000030
*        PROGRAM - FIND                                                 00000040
*        PURPOSE - TO SEARCH ARGUMENT IN A PDS.                         00000050
*        AUTHOR - VINH VU                                               00000060
*        DATE-WRITTEN - 02/82                                           00000070
*        MACROS USED -                                                  00000080
*              SET,EXIT,READ,NOTE,POINT,TPUT,TGET,GETMAIN,              00000090
*              FREEMAIN,OPEN,CLOSE,DCB,DCBD                             00000100
*        NOTE -                                                         00000110
*              THIS IS A COMMAND PROCESSOR. ALL MISSING POSITIONAL      00000120
*              PARAMETERS WILL BE PROMPTED AT TERMINAL.                 00000130
*                                                                       00000140
*        COMMAND FORMAT :                                               00000150
*              FIND    'XXXXX' S('YYY') LOWER GROUP('TTT') QUICK        00000160
*        WHERE :                                                        00000170
*          *   'XXXX' = PDS TO BE SEARCHED                              00000180
*          *   S - STANDS FOR STRING, 'YYY' = ARGUMENT STRING. THIS     00000190
*              IS OPTIONAL. IF NOT ENTERED, YOU WILL BE PROMPTED FOR.   00000200
*          *   LOWER = OPTIONAL KEYWORD. TO USE SEARCH ARGUMENT AS      00000210
*              ENTERED (DO NOT TURN TO UPPERCASE). THE DEFAULT IS       00000220
*              'NOT LOWER'.                                             00000230
*          *   GROUP = OPTIONAL KEYWORD. TO START SEARCHING ON          00000240
*              MEMBERS WITH NAME PREFIXED BY THIS.                      00000250
*          *   QUICK = OPTIONAL KEYWORD. STOP THE SEARCH IN A MEMBER    00000240
*              ONCE THE CHARACTER STRING IS FOUND.  WILL PROCEED TO     00000250
*              NEXT MEMBER.                                             00000250
*-------------------------------------------------------------------    00000260
*                                                                       00000270
FIND     CSECT                                                          00000280
         SET   DSECT=WKAREA,LV=WKLENG                                   00000290
         LR    R11,R1              GET CPPL ADDR                        00000300
         USING CPPL,R11            ESTAB ADDR                           00000310
         XC    WKPPL,WKPPL         CLEAR OUT PPL                        00000320
         LA    R4,WKPPL            GET ADDR OF PPL                      00000330
         USING PPL,R4              ESTAB ADDR                           00000340
         MVC   PPLUPT,CPPLUPT      GET UPT                              00000350
         MVC   PPLECT,CPPLECT      GET ECT                              00000360
         XC    MYECB,MYECB         CLEAR MY ECB                         00000370
         LA    R1,MYECB            GET A(ECB)                           00000380
         ST    R1,PPLECB           GET ECB                              00000390
         MVC   PPLPCL,APCL         GET PPLPCL                           00000400
         LA    R1,ANS                                                   00000410
         ST    R1,PPLANS           GET ANS                              00000420
         MVC   PPLCBUF,CPPLCBUF    GET CBUF                             00000430
         CALLTSSR EP=IKJPARS,MF=(E,WKPPL)                               00000440
         LTR   R15,R15                                                  00000450
         BNZ   ABEND020                                                 00000460
         L     R7,ANS              GET ANS                              00000470
         LA    R1,0(0,R7)          CLEAR OUT TRASH                      00000480
         LTR   R1,R1               CHECK IF ANS IS THERE                00000490
         BZ    ABEND020            NOP - THEN ABEND                     00000500
         USING IKJPARMD,R7         ESTAB ADDR                           00000510
*
         XC    QKSW,QKSW           TURN OFF 'QK' SW
         CLI   FQUICK+1,0          IS 'QK' ENTERED ??
         BE    CKLOWER             NOP - THEN GO THRU
         MVI   QKSW,X'FF'          ELSE - TURN ON SWITCH
*
CKLOWER  EQU   *
*                                                                       00000520
         MVI   UPPER,X'FF'         TURN ON SWITCH                       00000530
         CLI   FLOWER+1,0          IS 'LOWER' ENTERED ?                 00000540
         BE    NOLOWER             NOP - THEN GO THRU                   00000550
         MVI   UPPER,X'00'         ELSE - TURN OFF SWITCH               00000560
*                                                                       00000570
NOLOWER  EQU   *                                                        00000580
         MVC   WKDSTEST,BLANK                                           00000590
         LA    R1,44               GET LENG OF DSNAME                   00000600
         STH   R1,WKDST            STORE IT                             00000610
         TM    FPDS+6,X'80'        SOURCE NAME THERE ?                  00000620
         BZ    ABEND020            NOP - THEN ABEND                     00000630
         TM    FPDS+6,X'40'        SOURCE NAME HAS QUOTES ?             00000640
         BZ    PREFIXTS            NOP - THEN PREFIX IT WITH USER-ID    00000650
         L     R1,FPDS             GET POINTER TO SOURCE NAME           00000660
         LH    R2,FPDS+4           GET LENG OF SOURCE NAME              00000670
         BCTR  R2,R0               MINUS 1 FOR EX                       00000680
         EX    R2,MVCFPDS          MOVE DSN                             00000690
         B     CHECK               GOTO ALLOCIT                         00000700
*                                                                       00000710
PREFIXTS EQU   *                                                        00000720
         L     R1,CPPLPSCB         GET ADDR OF PSCB                     00000730
         USING PSCB,R1             ESTABLISH ADDRESSABILITY             00000740
         MVC   WKDSTEST(L'PSCBUSER),PSCBUSER                            00000750
         XR    R15,R15             CLEAR REG 15                         00000760
         LA    R14,WKDSTEST        GET ADDR OF WKSOURCE                 00000770
         IC    R15,PSCBUSRL        GET LENG OF USER-ID                  00000780
         LA    R14,0(R14,R15)      GET TO END OF IT                     00000790
         MVI   0(R14),C'.'         MOVE '.' IN THERE                    00000800
         DROP  R1                  DROP ADDR                            00000810
         L     R1,FPDS             GET POINTER TO SOURCE NAME           00000820
         LH    R2,FPDS+4           GET LENG OF SOURCE NAME              00000830
         BCTR  R2,R0               MINUS 1 FOR EX                       00000840
         EX    R2,MVCPFR           MOVE DSN                             00000850
         B     CHECK               GO TO ALLOCIT                        00000860
*                                                                       00000870
MVCFPDS  MVC   WKDSTEST(0),0(R1)                                        00000880
MVCPFR   MVC   1(0,R14),0(R1)                                           00000890
*                                                                       00000900
*                                                                       00000910
CHECK    EQU   *                                                        00000920
         CLI   FKEYWD+1,0          KEY WORD ENTERED ?                   00000930
         BE    ALLOCIT             NOP - THEN DO THE TGET               00000940
         L     R10,STRING          ELSE - GET POINTER INTO R10          00000950
         LH    R2,STRING+4         GET LENG                             00000960
         ST    R2,ARGLENG          STORE LENG                           00000970
         BCTR  R2,R0               MINUS 1 FOR EX                       00000980
         MVC   ARG,BLANK           BLANK OUT THIS AREA FIRST            00000990
         EX    R2,SAVEARG          SAVE THE ARGUMENT                    00001000
         B     THRU                THEN GO THRU                         00001010
*                                                                       00001020
ALLOCIT  EQU   *                                                        00001030
         LA    R9,MSG1             GET MSG                              00001040
         TPUT  (R9),L'MSG1         DO TPUT                              00001050
         LTR   R15,R15             GOOD RET CODE ?                      00001060
         BNZ   BADPUT              NOP - THEN ABEND 10                  00001070
*                                                                       00001080
LOOPGET  EQU   *                                                        00001090
         MVC   BUFFER,BLANK        BLANK THE BUFFER                     00001100
         LA    R9,BUFFER           GET ADDR OF BUFFER                   00001110
         TGET  (R9),L'BUFFER       DO THE GET                           00001120
         CLC   BUFFER,BLANK        IS IT STILL BLANK ?                  00001130
         BE    ALLOCIT             YEAH - THEN GOBACK TO LOOP           00001140
         TRT   BUFFER,QUOTE        CHECK FOR 1ST QUOTE                  00001150
         BC    8,PROMPT            IF NOT FND , GO TO PROMPT            00001160
         LA    R15,BUFFER+L'BUFFER GET TO THE END                       00001170
         LA    R1,1(0,R1)          GET PAST THE QUOTE                   00001180
         LR    R10,R1              SAVE BEGINNING ADDRESSS              00001190
         SR    R15,R1              GET LENG                             00001200
         BZ    PROMPT              IF ZERO THEN GOTO PROMPT             00001210
         BCTR  R15,R0              MINUS 1 FOR EX                       00001220
         EX    R15,TRTQUOTE        DO THE SEARCH AGAIN                  00001230
         BC    8,PROMPT            IF NOT FND , GO TO PROMPT            00001240
         SR    R1,R10              GET LENG OF ARGUMENT                 00001250
         ST    R1,ARGLENG          SAVE ARGUMENT LENG                   00001260
         BCTR  R1,R0               MINUS 1 FOR EX                       00001270
         MVC   ARG,BLANK           BLANK OUT THIS AREA FIRST            00001280
         EX    R1,SAVEARG          SAVE THE ARGUMENT                    00001290
         B     THRU                THEN GO THRU                         00001300
*                                                                       00001310
TRTQUOTE TRT   0(0,R10),QUOTE      DO THE TRT                           00001320
SAVEARG  MVC   ARG(0),0(R10)                                            00001330
*                                                                       00001340
PROMPT   EQU   *                                                        00001350
         LA    R9,MSG2             GET MSG                              00001360
         TPUT  (R9),L'MSG2         DO TPUT                              00001370
         LTR   R15,R15             GOOD RET CODE ?                      00001380
         BNZ   BADPUT              NOP - THEN ABEND 10                  00001390
         B     LOOPGET             GOBACK TO GET                        00001400
*                                                                       00001410
THRU     EQU   *                                                        00001420
         CLI   UPPER,X'FF'         SHOULD TURN ON TO UPPERCASE ?        00001430
         BNE   THRU01              NOP - THEN GO THRU                   00001440
         TR    ARG,TRTABLE         ELSE - TURN ON TO UPPERCASE          00001450
*                                                                       00001460
THRU01   EQU   *                                                        00001470
*--------------                                                         00001480
         B     THRU02              BYPASS EVERYBODY FOR NOW             00001490
*--------------                                                         00001500
         L     R15,CPPLPSCB        GET PSCB POINTER                     00001510
         TM    PSCBATR1-PSCB(R15),PSCBCTRL     OPER ON ?                00001520
         BO    THRU02              YEAH - THEN GO THRU                  00001530
         MVC   TESTUSER,BLANK      TO BLANK IT FIRST                    00001540
         MVC   TESTUSER(7),PSCBUSER-PSCB(R15)    ELSE - SAVE USER-ID    00001550
         XR    R14,R14             CLEAR REG 14                         00001560
         IC    R14,PSCBUSRL-PSCB(R15)   GET LENG OF USER                00001570
         LA    R1,TESTUSER         GET ADDR INTO R1                     00001580
         LA    R1,0(R14,R1)                                             00001590
         MVI   0(R1),C'.'          PUT IN PERIOD                        00001600
         EX    R14,TESTDSN         THEN DO THE TEST                     00001610
         BE    THRU02              IF EQ THEN LET THRU                  00001620
         LA    R9,ERR3             GET MSG                              00001630
         TPUT  (R9),L'ERR3         DO TPUT                              00001640
         LTR   R15,R15             GOOD RET CODE ?                      00001650
         BNZ   BADPUT              NOP - THEN ABEND 10                  00001660
         B     ENDIT                                                    00001670
*                                                                       00001680
TESTDSN  CLC   TESTUSER(0),WKDSTEST                                     00001690
MOVE     MVC   KGROUP(0),0(R1)                                          00001700
COMPARE  CLC   SAVEMEM(0),KGROUP                                        00001710
*                                                                       00001720
THRU02   EQU   *                                                        00001730
*                                                                       00001740
         XC    GROUPSW,GROUPSW     CLEAR GROUPSW                        00001750
         TM    PGROUP+6,X'80'      IS GROUP THERE ???                   00001760
         BZ    THRU03              NOP - THEN FORGET IT                 00001770
         MVI   GROUPSW,X'FF'       TURN ON SWITCH                       00001780
         L     R1,PGROUP           ELSE - GET ITS ADDRESS               00001790
         LH    R9,PGROUP+4         GET LENG                             00001800
         BCTR  R9,R0               MINUS 1 FOR EXECUTE                  00001810
         ST    R9,LGROUP           SAVE LENG FOR LATER COMPARE          00001820
         EX    R9,MOVE             NOW DO THE MOVE                      00001830
*                                                                       00001840
THRU03   EQU   *                                                        00001850
         XC    ETAB,ETAB           CLEAR TABLE                          00001860
         XR    R9,R9               CLEAR REG 9                          00001870
         IC    R9,ARG              GET FIRST BYTE INTO TABLE            00001880
         LA    R9,ETAB(R9)         GET TO THE POINT                     00001890
         MVI   0(R9),X'FF'         TURN ON STOP INDICATOR               00001900
         XC    WKDAP08,WKDAP08     CLEAR AREA                           00001910
         LA    R9,WKDAP08          GET ADDR INTO R9                     00001920
         USING DAPB08,R9           ESTAB ADDR                           00001930
         MVC   DA08CD,=XL2'0008'                                        00001940
         LA    R1,WKDST            GET ADDR OF DSNAME BUFF (TEST)       00001950
         ST    R1,DA08PDSN                                              00001960
         MVC   DA08DDN,BLANK       BLANK OUT DDNAME                     00001970
         MVC   DA08UNIT,=CL8'SYSDA'                                     00001980
         MVC   DA08SER,BLANK       BLANK SER NO                         00001990
         MVC   DA08PSWD,BLANK      BLANK OUT PSWD                       00002000
         MVC   DA08MNM,BLANK       BLANK OUT MEMBER NAME                00002010
         OI    DA08DSP1,DA08SHR    SHR                                  00002020
         DROP  R9                                                       00002030
         XC    WKDAPL,WKDAPL       CLEAR AREA                           00002040
         LA    R9,WKDAPL           GET ADDR INTO R9                     00002050
         USING DAPL,R9             ESTAB ADDTR                          00002060
         MVC   DAPLUPT,CPPLUPT     GET UPT                              00002070
         MVC   DAPLECT,CPPLECT     GET ECT                              00002080
         XC    MYECB,MYECB         CLEAR MYECB                          00002090
         LA    R1,MYECB            GET ITS ADDR                         00002100
         ST    R1,DAPLECB          GET IT INTO LIST                     00002110
         MVC   DAPLPSCB,CPPLPSCB   GET PSCB                             00002120
         LA    R1,WKDAP08          GET ADDR OF DAIR                     00002130
         ST    R1,DAPLDAPB         GET IT INTO LIST                     00002140
         LA    R1,WKDAPL           GET ADDR OF PARM LIST                00002150
         CALLTSSR EP=IKJDAIR,MF=(E,WKDAPL)                              00002160
         LTR   R15,R15             TEST RET CODE                        00002170
         BNZ   DAIRERR             BAD - GOTO DAIR ERR                  00002180
         LA    R9,WKDAP08          GET ADDR INTO R9                     00002190
         USING DAPB08,R9           ESTAB ADDR                           00002200
         MVC   SAVEDDTS,DA08DDN    SAVE DD NAME FOR TEST MASTER         00002210
         TM    DA08DSO,X'02'       PDS ???                              00002220
         BZ    NOTPDS              NOP - THEN ERROR                     00002230
         ZAP   COUNTER,=P'0'                                            00002240
         ZAP   TOT,=P'0'           ZERO OUT FINAL COUNTER               00002250
         ZAP   MEMTOT,=P'0'        ZERO OUT THE MEMBER TOTAL            00002260
         DROP  R9                                                       00002270
*                                                                       00002280
         MVC   WMASTER,MASTER      GET DCB INTO WK AREA                 00002290
         MVC   WOPENM,OPENM        MOVE OPEN LIST TO WK AREA            00002300
         LA    R1,WMASTER          GET DCB                              00002310
         USING IHADCB,R1           ESTAB ADDR                           00002320
         MVC   DCBDDNAM,SAVEDDTS   GET DDNAME                           00002330
         DROP  R1                  DROP ADDR                            00002340
         OPEN  (WMASTER),MF=(E,WOPENM)                                  00002350
         LA    R1,WMASTER          GET DCB                              00002360
         USING IHADCB,R1           ESTAB ADDR                           00002370
         TM    DCBOFLGS,DCBOFOPN   IS OPEN GOOD ?                       00002380
         BC    8,BADOPEN           NO - GOTO BAD OPEN                   00002390
         MVC   SAVESIZE,DCBBLKSI   SAVE REAL BLK SIZE                   00002400
         MVC   SAVERECL,DCBLRECL   SAVE REAL LRECL                      00002410
         MVC   DCBBLKSI,H256       BLKSIZE IS 256                       00002420
         DROP  R1                  DROP ADDR                            00002430
         LA    R5,DIR              GET ADD OF DIR AREA                  00002440
         BAL   R9,READNXT          DO THE READ                          00002450
         BAL   R9,CHECKIT          DO THE CHECK                         00002460
         NOTE  WMASTER             GET CURR TTR                         00002470
         LA    R1,1(0,R1)          BUMP IT BY 1                         00002480
         ST    R1,TTR              SAVE IT                              00002490
         LA    R1,WMASTER          GET DCB ADDR                         00002500
         USING IHADCB,R1           ESTAB ADDR                           00002510
         LA    R2,LOOPPROC         GET ADDRT OF EODAD                   00002520
         STCM  R2,B'0111',DCBEODA  STORE INTO DCB                       00002530
         DROP  R1                  DROP ADDR                            00002540
         B     LOOPCK              GOTO LOOP                            00002550
*                                                                       00002560
READIR   EQU   *                                                        00002570
*                                                                       00002580
*--------------                                                         00002590
*        THIS IS A LOOP TO READ DIR                                     00002600
*--------------                                                         00002610
*                                                                       00002620
         LA    R1,WMASTER          GET DCB ADDR                         00002630
         USING IHADCB,R1           ESTAB ADDR                           00002640
         MVC   DCBBLKSI,H256       BLKSIZE IS 256                       00002650
         MVC   SAVEODAD,DCBEODA    SAVE EODAD                           00002660
         LA    R2,ENDRPT           GET NEW EODAD                        00002670
         STCM  R2,B'0111',DCBEODA                                       00002680
         DROP  R1                  DROP ADDR                            00002690
         POINT WMASTER,TTR         GET TO NEW POS                       00002700
         LA    R5,DIR              GET RECORD AREA                      00002710
         BAL   R9,READNXT          THEN READ                            00002720
         BAL   R9,CHECKIT          AND CHECK THE READ                   00002730
         LA    R1,WMASTER          GET DCB ADDR                         00002740
         USING IHADCB,R1           ESTAB ADDR                           00002750
         MVC   DCBEODA(3),SAVEODAD GET OLD EODAD BACK                   00002760
         DROP  R1                  DROP R1                              00002770
         NOTE  WMASTER                                                  00002780
         LA    R1,1(0,R1)          BUMP IT BY 1                         00002790
         ST    R1,TTR              SAVE THE TTR FOR NEXT TIME           00002800
*                                                                       00002810
LOOPCK   EQU   *                                                        00002820
         LA    R1,DIR              GET DIR ADDR                         00002830
         LH    R2,DIR              GET THE BYTES USED                   00002840
         AR    R1,R2               GET ENDING ADDR                      00002850
         ST    R1,SAVEEND          SAVE IT                              00002860
         LA    R5,2                GET R5 AS 1ST BASE TO MEM NAME       00002870
         ST    R5,SAVECURR         SAVE CURR POS                        00002880
*                                                                       00002890
LOOPPROC EQU   *                                                        00002900
*                                                                       00002910
*--------------                                                         00002920
*        THIS IS A LOOP TO CHECK DIR ENTRY FOR EACH MEMBER              00002930
*--------------                                                         00002940
*                                                                       00002950
         CP    COUNTER,=P'0'       IS COUNTER GREATER THAN 0            00002960
         BE    LOOPPR1             ZERO ? THEN GO THRU                  00002970
         AP    TOT,COUNTER         ADD COUNTER TO FINAL TOTAL           00002980
         AP    MEMTOT,=P'1'        ADD 1 TO MEMBER TOTAL                00002990
         MVC   BUFFER,BLANK        CLEAR BUFFER                         00003000
         MVC   BUFFER(L'MSG4),MSG4 GET MSG                              00003010
         MVC   BUFFER+44(L'SAVEMEM),SAVEMEM                             00003020
         MVC   BUFFER+55(L'PAT),PAT                                     00003030
         ED    BUFFER+55(L'PAT),COUNTER+2                               00003040
         LA    R9,BUFFER           GET MSG                              00003050
         TPUT  (R9),L'BUFFER       DO TPUT                              00003060
         LTR   R15,R15             GOOD RET CODE ?                      00003070
         BNZ   BADPUT              NOP - THEN ABEND 10                  00003080
*                                                                       00003090
LOOPPR1  EQU   *                                                        00003100
         ZAP   COUNTER,=P'0'       ZERO OUT COUNTER                     00003110
         LA    R1,DIR              GET DIR ADDTR                        00003120
         L     R5,SAVECURR         GET CURR DISP                        00003130
         AR    R1,R5               GET TO CURR POS                      00003140
         C     R1,SAVEEND          CHECK AGST ENDING ADDR               00003150
         BNL   READIR              IF END - READ ANOTHER DIR BLK        00003160
         CLC   0(8,R1),=8XL1'FF'   IS THIS LAST MEM                     00003170
         BE    ENDRPT              YES - GOTO ENDIT                     00003180
         XR    R2,R2               CLEAR REG 2                          00003190
         IC    R2,11(R1)           GET NUM                              00003200
         N     R2,=F'31'           CLEAN OUT TRASH                      00003210
         AR    R2,R2               DOUBLE LENGTH                        00003220
         LA    R2,12(0,R2)         PLUS MEM NAME AND MISC               00003230
         A     R2,SAVECURR         ADD TO CURR DISP                     00003240
         ST    R2,SAVECURR         SAVE THIS CURR DISP                  00003250
*                                                                       00003260
GOON     EQU   *                                                        00003270
         LR    R2,R1               R2 HAS GOOD ADDR                     00003280
         MVC   SAVEMEM,0(R2)       SAVE MOD NAME                        00003290
*                                                                       00003300
         CLI   GROUPSW,X'FF'       GROUP SW ON ???                      00003310
         BNE   GOON1               NOP - THEN NO CHECK                  00003320
         L     R15,LGROUP          GET LENG FOR COMPARE                 00003330
         EX    R15,COMPARE         DO THE COMPARE                       00003340
         BL    LOOPPR1             LOW - THEN GO BACK                   00003350
         BH    ENDRPT              HIGH - THEN TERMINATE                00003360
*                                                                       00003370
GOON1    EQU   *                                                        00003380
         XR    R1,R1               CLEAR REG1                           00003390
         ICM   R1,B'1110',8(R2)    GET TTR                              00003400
         ST    R1,WKTTR            SAVE MEM TTR                         00003410
         LA    R1,WMASTER          GET DCB ADDR                         00003420
         USING IHADCB,R1           ESTAB ADDR                           00003430
         MVC   DCBBLKSI,SAVESIZE   GET REAL BLKSIZE                     00003440
         MVC   DCBLRECL,SAVERECL   GET REAL LRECL                       00003450
         DROP  R1                  DROP ADDR                            00003460
         POINT WMASTER,WKTTR                                            00003470
         LA    R5,RECORD           GET REC ADDR INTO R5                 00003480
         XC    SW,SW               CLEAR BLK SWITCH                     00003490
         ZAP   COUNTER,=P'0'       MOVE 0 TO COUNTER                    00003500
*                                                                       00003510
FIND0010 EQU   *                                                        00003520
         BAL   R9,READNXT          DO THE READ                          00003530
         BAL   R9,CHECKIT          DO THE CHECK                         00003540
         LA    R8,RECORD           GET ADDR OF RECORD                   00003550
         L     R1,WREADM+16        GET IOB ADDR                         00003560
         LH    R15,SAVESIZE        GET SAVED SIZE                       00003570
         SH    R15,14(R1)          GET RECORD SIZE HERE                 00003580
         STH   R15,SAVECNT         SAVE IT                              00003590
         LA    R14,RECORD(R15)     GET ENDING ADDR                      00003600
         ST    R14,SAVEREC         SAVE IT                              00003610
         TM    SW,NEXTBLK          SHOULD CHECK THIS BLK COMPARE ?      00003620
         BZ    FIND0020            NOP - THEN KEEP ON                   00003630
         NI    SW,X'FF'-NEXTBLK    TURN OFF SW                          00003640
         LH    R14,SAVECOMP        GET SAVE COMP                        00003650
         LA    R7,ARG(R14)         GET ADDR OF THE REST OF CONSTANT     00003660
         XR    R1,R1               CLEAR REG 1                          00003670
         L     R1,ARGLENG          GET LENG OF CONSTANT                 00003680
         SR    R1,R14              R1 HAS RESIDUAL LENG                 00003690
         CR    R15,R1              CHECK SAVECNT                        00003700
         BL    FIND0020            IS LESS THEN KEEP ON                 00003710
         BCTR  R1,R0               MINUS 1 FOR EX                       00003720
         EX    R1,CKREST           DO THE COMPARE                       00003730
         BNE   FIND0015            IF NOT EQ THEN GO THRU               00003740
         AP    COUNTER,=P'1'       ADD 1 TO COUNTER                     00003750
         CLI   QKSW,X'FF'          IS 'QK' SW ON ??
         BE    LOOPPROC            YEAH - THEN BYE BYE
*                                                                       00003760
FIND0015 EQU   *                                                        00003770
         LA    R8,1(0,R8)          ELSE - RESET R8                      00003780
         BCTR  R15,R0              SUBTRACT SAVECNT BY 1                00003790
         STH   R15,SAVECNT         AND RESAVE IT                        00003800
*                                                                       00003810
FIND0020 EQU   *                                                        00003820
         XR    R15,R15             CLEAR R15                            00003830
         ICM   R15,B'0011',SAVECNT GET SAVED COUNT                      00003840
         BZ    FIND0010            IF ZERO THEN GOBACK TO READ          00003850
         CH    R15,H256            IS IT GREATER THAN 256               00003860
         BNH   FIND0030            NOP - THEN GO THRU                   00003870
         SH    R15,H256            ELSE - SUBTRACT IT BY 256            00003880
         STH   R15,SAVECNT         THEN SAVE IT FOR NEXT TIME           00003890
         LH    R15,H256            LOAD 256 FOR TRT                     00003900
         B     FIND0040            THEN GOTO DO TRT                     00003910
*                                                                       00003920
FIND0030 EQU   *                                                        00003930
         XC    SAVECNT,SAVECNT     ZERO OUT SAVE CNT                    00003940
*                                                                       00003950
FIND0040 EQU   *                                                        00003960
         BCTR  R15,R0              MINUS 1 FOR EX                       00003970
         EX    R15,TRTEE           CHECK FOR X'EE'                      00003980
         BC    8,FIND0070          NOT FND - THEN BUMP ADDR             00003990
         L     R14,SAVEREC         GET ENDING ADDR                      00004000
         SR    R14,R1              SUBTRACT FOR LENG                    00004010
         L     R15,ARGLENG         GET ARG LENG                         00004020
         CR    R14,R15             CHECK AGSNT ARG LNG                  00004030
         BNL   FIND0060            IF HIGH THEN SKIP THIS               00004040
         STH   R14,SAVECOMP        SAVE THIS LENG                       00004050
         BCTR  R14,R0              MINUS 1 FOR EX                       00004060
         EX    R14,CLCEE           DO THE COMPARE                       00004070
         BNE   FIND0050            IS NOT EQ THEN GO ON                 00004080
         OI    SW,NEXTBLK          TURN ON SW FOR NEXT BLK COMPARE      00004090
         B     FIND0010            GOBACK TO READ                       00004100
*                                                                       00004110
FIND0050 EQU   *                                                        00004120
         LA    R8,1(0,R1)          ELSE - RESET R8                      00004130
         L     R15,SAVEREC         GET ENDING ADDR                      00004140
         SR    R15,R8              GET RESIDUAL LENG                    00004150
         STH   R15,SAVECNT         SAVE IT                              00004160
         B     FIND0020            GOBACK TO LOOP                       00004170
*                                                                       00004180
FIND0060 EQU   *                                                        00004190
         L     R3,ARGLENG          GET ARG LENG                         00004200
         BCTR  R3,R0               MINUS 1 FOR EX                       00004210
         EX    R3,CLCEE            DO THE COMPARE                       00004220
         BNE   FIND0065            IF NOT MATCH - THEN GO THU           00004230
         AP    COUNTER,=P'1'       ADD 1 TO COUNTER                     00004240
         CLI   QKSW,X'FF'          IS 'QK' SW ON ??
         BE    LOOPPROC            YEAH - THEN BYE BYE
*                                                                       00004250
FIND0065 EQU   *                                                        00004260
         LA    R8,1(0,R1)          ELSE - RESET R8                      00004270
         L     R15,SAVEREC         GET ENDING ADDR                      00004280
         SR    R15,R8              GET RESIDUAL LENG                    00004290
         STH   R15,SAVECNT         SAVE IT                              00004300
         B     FIND0020            GOBACK TO LOOP                       00004310
*                                                                       00004320
FIND0070 EQU   *                                                        00004330
         LA    R8,1(R15,R8)        GET TO NEXT ADDR                     00004340
         B     FIND0020            GOBACK TO LOOP                       00004350
*                                                                       00004360
TRTEE    TRT   0(0,R8),ETAB                                             00004370
CLCEE    CLC   0(0,R1),ARG                                              00004380
CKREST   CLC   0(0,R8),0(R7)                                            00004390
*                                                                       00004400
*                                                                       00004410
READNXT  EQU   *                                                        00004420
*                                                                       00004430
*--------------                                                         00004440
*        AT ENTRY TO THIS ROUTINE, R5 WILL POINT TO THE AREA            00004450
*        FOR THE RECORD TO BE READ INTO                                 00004460
*--------------                                                         00004470
*                                                                       00004480
         MVC   WREADM,READM        MOVE READ LIST TO WK AREA            00004490
         READ  WREADM,SF,WMASTER,(R5),'S',MF=E                          00004500
         BR    R9                                                       00004510
         SPACE 3                                                        00004520
*                                                                       00004530
CHECKIT  EQU   *                                                        00004540
*                                                                       00004550
*--------------                                                         00004560
*        THIS IS TO CHECK AFTER ANY BSAM I/O                            00004570
*        WREADM WILL BE THE DECB TO BE CHECKED ON                       00004580
*--------------                                                         00004590
*                                                                       00004600
         CHECK WREADM                                                   00004610
         BR    R9                                                       00004620
         EJECT                                                          00004630
*                                                                       00004640
DAIRERR  EQU   *                                                        00004650
         ST    R15,DAIRRET         SAVE R15                             00004660
         LA    R9,ERR2             GET MSG                              00004670
         TPUT  (R9),L'ERR2         DO TPUT                              00004680
         LTR   R15,R15             GOOD RET CODE ?                      00004690
         BNZ   BADPUT              NOP - THEN ABEND 10                  00004700
         LA    R9,WKDERR           GET ADDR OF DAIRFAIL PARM            00004710
         USING DFDSECTD,R9         ESTAB ADDR                           00004720
         LA    R15,WKDAPL                                               00004730
         ST    R15,DFDAPLP                                              00004740
         LA    R15,DAIRRET                                              00004750
         ST    R15,DFRCP                                                00004760
         LA    R15,HEXZEROS                                             00004770
         ST    R15,DFJEFF02                                             00004780
         LA    R15,DAIRBYTE                                             00004790
         ST    R15,DFIDP                                                00004800
         LR    R1,R9               GET R1 SET UP                        00004810
         MVC   WLINKIT,LINKIT      GET LINK LIST FORM                   00004820
         LINK  SF=(E,WLINKIT)                                           00004830
         LTR   R15,R15             RETN CODE GOOD ?                     00004840
         BZ    ENDIT               YEAH - THEN THRU                     00004850
         ABEND 30                  ELSE ABEND 30                        00004860
*                                                                       00004870
ABEND020 EQU   *                                                        00004880
         LA    R2,ERR1             GET BUFFER ADDR                      00004890
         TPUT  (R2),L'ERR1         DO TPUT                              00004900
         LTR   R15,R15             TEST R15                             00004910
         BNZ   BADPUT              NO GOOD - GOTO BADTPUT               00004920
         ABEND 20                                                       00004930
*                                                                       00004940
NOTPDS   EQU   *                                                        00004950
         LA    R2,ERR4             GET BUFFER ADDR                      00004960
         TPUT  (R2),L'ERR4         DO TPUT                              00004970
         LTR   R15,R15             TEST R15                             00004980
         BNZ   BADPUT              NO GOOD - GOTO BADTPUT               00004990
         B     ENDIT               ELSE - GO TO EXIT                    00005000
*                                                                       00005010
BADOPEN  EQU   *                                                        00005020
         MVC   BUFFER,MSG3         GET MSG                              00005030
         LA    R2,BUFFER           GET BUFFER ADDR                      00005040
         TPUT  (R2),EIGHTY         DO TPUT                              00005050
         LTR   R15,R15             TEST R15                             00005060
         BC    7,BADPUT            NO GOOD - GOTO BADTPUT               00005070
         B     ENDIT               ELSE - GOTO EXIT                     00005080
         EJECT                                                          00005090
*                                                                       00005100
*                                                                       00005110
*                                                                       00005120
ENDRPT   EQU   *                                                        00005130
         CP    COUNTER,=P'0'       IS COUNTER GREATER THAN 0            00005140
         BE    ENDRPT1             ZERO ? THEN GO THRU                  00005150
         MVC   BUFFER,BLANK        CLEAR BUFFER                         00005160
         MVC   BUFFER(L'MSG4),MSG4 GET MSG                              00005170
         MVC   BUFFER+44(L'SAVEMEM),SAVEMEM                             00005180
         MVC   BUFFER+55(L'PAT),PAT                                     00005190
         ED    BUFFER+55(L'PAT),COUNTER+2                               00005200
         LA    R9,BUFFER           GET MSG                              00005210
         TPUT  (R9),L'BUFFER       DO TPUT                              00005220
         LTR   R15,R15             GOOD RET CODE ?                      00005230
         BNZ   BADPUT              NOP - THEN ABEND 10                  00005240
*                                                                       00005250
ENDRPT1  EQU   *                                                        00005260
         MVC   BUFFER,BLANK        CLEAR BUFFER                         00005270
         LA    R9,BUFFER           GET MSG                              00005280
         TPUT  (R9),L'BUFFER       DO TPUT                              00005290
         LTR   R15,R15             GOOD RET CODE ?                      00005300
         BNZ   BADPUT              NOP - THEN ABEND 10                  00005310
         MVC   BUFFER,BLANK        CLEAR BUFFER                         00005320
         MVC   BUFFER(L'MSG5),MSG5 GET MSG                              00005330
         MVC   BUFFER+7(L'PAT1),PAT1                                    00005340
         ED    BUFFER+7(L'PAT1),MEMTOT+3                                00005350
         MVC   BUFFER+55(L'PAT),PAT                                     00005360
         ED    BUFFER+55(L'PAT),TOT+2                                   00005370
         LA    R9,BUFFER           GET MSG                              00005380
         TPUT  (R9),L'BUFFER       DO TPUT                              00005390
         LTR   R15,R15             GOOD RET CODE ?                      00005400
         BNZ   BADPUT              NOP - THEN ABEND 10                  00005410
         CLI   QKSW,X'FF'          'QK' SWITCH ON ??
         BNE   NONOTES             NO - THEN NO NOTES DISPLAYED
         MVC   BUFFER,BLANK        CLEAR BUFFER                         00005320
         MVC   BUFFER(L'MSG5A),MSG5A GET MSG                            00005330
         LA    R9,BUFFER           GET MSG                              00005380
         TPUT  (R9),L'BUFFER       DO TPUT                              00005390
         LTR   R15,R15             GOOD RET CODE ?                      00005400
         BNZ   BADPUT              NOP - THEN ABEND 10                  00005410
*
NONOTES  EQU   *
         MVC   WCLOSEM,CLOSEM      MOVE CLOSE LIST TO WK AREA           00005420
         CLOSE (WMASTER),MF=(E,WCLOSEM)                                 00005430
         B     ENDIT                                                    00005440
*                                                                       00005450
BADPUT   EQU   *                                                        00005460
         ABEND 10                  ABEND FOR TPUT                       00005470
*                                                                       00005480
*                                                                       00005490
ENDIT    EQU   *                                                        00005500
         EXIT                                                           00005510
         EJECT                                                          00005520
*                                                                       00005530
EIGHTY   EQU   80                                                       00005540
MSG1     DC    C'Enter search argument in quotes'                       00005550
MSG2     DC    C'Missing quotes - Re-Enter'                             00005560
MSG3     DC    C'Open on ''MASTER'' file has failed'                    00005570
MSG4     DC    C'Total Number of Occurrences found in Member XXXXXXXX iX00005580
               s XXXXXXX'                                               00005590
MSG5     DC    C'   From XXXXX Members, the Final Total is   ==========X00005600
               > XXXXXXX'                                               00005610
MSG5A    DC    C'   Note - QUICK option invoked. Number of occurrences X00005600
               is always 1 if found'                                    00005610
ERR1     DC    C'IKJPARS ERROR - COMMAND TERMINATED'                    00005620
ERR2     DC    C'IKJDAIR ERROR - COMMAND TERMINATED'                    00005630
ERR3     DC    C'Command is restricted to search your own Data Sets OnlX00005640
               y'                                                       00005650
ERR4     DC    C'Data set is not Partitioned. Command Terminated'       00005660
*                                                                       00005670
H256     DC    H'256'                                                   00005680
PAT      DC    X'4020202020202120'                                      00005690
PAT1     DC    X'402020202120'                                          00005700
*                                                                       00005710
*                                                                       00005720
         EJECT                                                          00005730
         DS    0F                                                       00005740
LINKIT   LINK  EP=IKJEFF18,SF=L                                         00005750
LINKITL  EQU   *-LINKIT                                                 00005760
*                                                                       00005770
*                                                                       00005780
*                                                                       00005790
*                                                                       00005800
MASTER   DCB   DSORG=PS,MACRF=(RP),EODAD=ENDRPT,RECFM=U,DDNAME=MASTER   00005810
MASTERL  EQU   *-MASTER            LENG OF THE INPUT DCB                00005820
*                                                                       00005830
*                                                                       00005840
*                                                                       00005850
*                                                                       00005860
         DS    0F                  ALIGN ON FWD BOUNDARY                00005870
OPENM    OPEN  (MASTER),MF=L                                            00005880
OPENML   EQU   *-OPENM                                                  00005890
*                                                                       00005900
*                                                                       00005910
*                                                                       00005920
*                                                                       00005930
         DS    0F                  ALIGN ON FWD BOUNDARY                00005940
CLOSEM   CLOSE (MASTER,FREE),MF=L                                       00005950
CLOSEML  EQU   *-CLOSEM                                                 00005960
*                                                                       00005970
*                                                                       00005980
*                                                                       00005990
*                                                                       00006000
         DS    0F                                                       00006010
READM    READ  HDECB,SF,MASTER,,'S',MF=L                                00006020
READML   EQU   *-READM                                                  00006030
*                                                                       00006040
*                                                                       00006050
*                                                                       00006060
APCL     DC    A(PCL)                                                   00006070
HEXZEROS DC    F'0'                                                     00006080
DAIRBYTE DC    XL2'0001'                                                00006090
*                                                                       00006100
*                                                                       00006110
BLANK    DC    CL80' '                                                  00006120
*                                                                       00006130
         EJECT                                                          00006140
         SPACE 3                                                        00006150
QUOTE    DS    0CL256                                                   00006160
         DC    256X'00'                                                 00006170
         ORG   QUOTE+C''''         STOP AT QUOTE                        00006180
         DC    X'FF'                                                    00006190
         ORG                                                            00006200
         SPACE 3                                                        00006210
*                                                                       00006220
TRTABLE  DC    256AL1(*-TRTABLE)                                        00006230
*                                                                       00006430
         ORG   TRTABLE+X'81'       THIS IS LOWER CASE 'A'               00006440
         DC    C'ABCDEFGHI'                                             00006450
         ORG   TRTABLE+X'91'       THIS IS LOWER CASE 'J'               00006460
         DC    C'JKLMNOPQR'                                             00006470
         ORG   TRTABLE+X'A2'       THIS IS LOWER CASE 'S'               00006480
         DC    C'STUVWXYZ'                                              00006490
         ORG                                                            00006500
*                                                                       00006510
         EJECT                                                          00007380
         IKJPPL                                                         00007390
PPLLENG  EQU   *-PPL               LENG OF PPL                          00007400
         EJECT                                                          00007410
         IKJDAPL                                                        00007420
DAPLLENG EQU   *-DAPL              LENG OF DAPL                         00007430
         EJECT                                                          00007440
         IKJDAP08                                                       00007450
DAP08LEN EQU   *-DAPB08            LENG                                 00007460
         EJECT                                                          00007470
         IKJEFFDF DFDSECT=YES                                           00007480
         EJECT                                                          00006520
*                                                                       00006530
WKAREA   DSECT                                                          00006540
         DS    18F                 SAVE AREA                            00006550
WKPARM   DS    20F                 THIS IS USED FOR CALL                00006560
*                                                                       00006570
         DS    0F                                                       00006580
WLINKIT  DS    CL(LINKITL)                                              00006590
*                                                                       00006600
*                                                                       00006610
*                                                                       00006620
         DS    0F                                                       00006630
WKPPL    DS    CL(PPLLENG)                                              00006640
         DS    0F                                                       00006650
WKDAPL   DS    CL(DAPLLENG)                                             00006660
         DS    0F                                                       00006670
WKDAP08  DS    CL(DAP08LEN)                                             00006680
         DS    0F                                                       00006690
WKDERR   DS    CL(DFLEN)                                                00006700
*                                                                       00006710
*                                                                       00006720
TTR      DS    F                   THIS IS SAVED TTR                    00006730
WKTTR    DS    F                   THIS IS SAVED TTR                    00006740
SAVEEND  DS    F                                                        00006750
SAVECURR DS    F                                                        00006760
SAVESIZE DS    H                                                        00006770
SAVERECL DS    H                                                        00006780
SAVE15   DS    F                                                        00006790
SAVECOMP DS    H                                                        00006800
SAVECNT  DS    H                                                        00006810
SAVEREC  DS    F                                                        00006820
ANS      DS    F                                                        00006830
MYECB    DS    F                                                        00006840
DAIRRET  DS    F                                                        00006850
ARGLENG  DS    F                   ARGUMENT LENG                        00006860
WDWORD   DS    D                   WORK DOUBLE WORD                     00006870
SAVEODAD DS    CL3                                                      00006880
         DS    0H                                                       00006890
WKDST    DS    0CL46                                                    00006900
         DS    CL2                 LENG                                 00006910
WKDSTEST DS    CL44                NAME                                 00006920
         DS    0H                                                       00006930
*                                                                       00006940
SAVEDDTS DS    CL8                 SAVED DDNAME FOR TEST MASTER         00006950
SAVEMEM  DS    CL8                 SAVED MEMBER NAME                    00006960
*                                                                       00006970
*                                                                       00006980
BUFFER   DS    CL80                                                     00006990
ARG      DS    CL80                SAVED ARGUMENT                       00007000
SW       DS    X                                                        00007010
NEXTBLK  EQU   X'80'                                                    00007020
*                                                                       00007030
UPPER    DS    X                   SWITCH USED FOR UPPERCASE            00007040
QKSW     DS    X                   SWITCH USED FOR 'QUICK' OPTION
COUNTER  DS    PL6                                                      00007050
TOT      DS    PL6                 THIS IS FINAL TOTAL                  00007060
MEMTOT   DS    PL6                 THIS IS TOTAL OF MEMBERS             00007070
TESTUSER DS    CL8                                                      00007080
KGROUP   DS    CL7                 SAVED GROUP PREFIX                   00007090
LGROUP   DS    F                   LENG USED TO COMPARE                 00007100
GROUPSW  DS    X                   SWITCH                               00007110
*                                                                       00007120
*                                                                       00007130
         DS    0F                  ALIGN ON FWD BOUNDARY                00007140
WMASTER  DS    CL(MASTERL)         WK DCB                               00007150
*                                                                       00007160
         DS    0F                  ALIGN ON FWD BOUNDARY                00007170
WOPENM   DS    CL(OPENML)                                               00007180
*                                                                       00007190
         DS    0F                                                       00007200
WCLOSEM  DS    CL(CLOSEML)                                              00007210
*                                                                       00007220
         DS    0F                                                       00007230
WREADM   DS    CL(READML)                                               00007240
*                                                                       00007250
ETAB     DS    CL256                                                    00007260
*                                                                       00007270
         DS    0F                                                       00007280
DIR      DS    CL256                                                    00007290
RECORD   DS    CL32767             THIS IS A LARGEST BLK                00007300
*                                                                       00007310
WKLENG   EQU   *-WKAREA            THIS IS LENG OF GETMAINED AREA       00007320
*                                                                       00007330
         EJECT                                                          00007340
         CVT   DSECT=YES                                                00007350
         EJECT                                                          00007360
         IKJCPPL                                                        00007370
         EJECT                                                          00007490
         IKJPSCB                                                        00007500
         EJECT                                                          00007510
         DCBD  DSORG=PS                                                 00007520
         EJECT                                                          00007530
FIND     CSECT                                                          00007540
         PRINT NOGEN                                                    00007550
PCL      IKJPARM                                                        00007560
FPDS     IKJPOSIT DSNAME,PROMPT='DSNAME OF PDS',                       X00007570
               HELP=('DSNAME OF PDS TO BE SEARCHED')                    00007580
*                                                                       00007590
FKEYWD   IKJKEYWD                                                       00007600
         IKJNAME 'STRING',SUBFLD=FSTRING                                00007610
*                                                                       00007620
FLOWER   IKJKEYWD                                                       00007630
         IKJNAME 'LOWER'                                                00007640
*                                                                       00007620
FQUICK   IKJKEYWD                                                       00007630
         IKJNAME 'QUICK'                                                00007640
*                                                                       00007650
GROUP    IKJKEYWD                                                       00007660
         IKJNAME 'GROUP',SUBFLD=@GROUP                                  00007670
*                                                                       00007680
*                                                                       00007690
FSTRING  IKJSUBF                                                        00007700
STRING   IKJIDENT 'ARGUMENT STRING',MAXLNTH=80,CHAR,ASIS,              X00007710
               PROMPT='STRING OF ARGUMENT TO BE SEARCHED'               00007720
*                                                                       00007730
@GROUP   IKJSUBF                                                        00007740
PGROUP   IKJIDENT 'MEMBER NAME PREFIX',MAXLNTH=7,                      X00007750
               FIRST=ALPHA,OTHER=ALPHANUM,                             X00007760
               PROMPT='MEMBER NAME PREFIX USED FOR SEARCH'              00007770
*                                                                       00007780
         IKJENDP                                                        00007790
*                                                                       00007800
         END                                                            00007810
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR         <= TARGET LIBRARY
//LKED.SYSIN DD *
 NAME FIND(R)
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP              <= TARGET LIBRARY
//SYSIN    DD  *
./ ADD NAME=FIND
)F FUNCTION -

     The FIND TSO Command searches for a character string in a PDS
 (Partitioned Data Set) and displays the number of occurrences found
 in each member.

     This command can be executed under native TSO mode or SPF/Option 6.
 The command will prompt for Data Set Name and search argument.  The
 total for occurrences and members will also be displayed at the end of
 execution.


)X SYNTAX -

          FIND 'DSNAME' S('CHARACTER STRING')
                        GROUP(XX)
                        LOWER
                        QUICK


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


./ ENDUP
//
