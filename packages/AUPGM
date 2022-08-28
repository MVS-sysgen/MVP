//AUPGM JOB (JOB),
//             'INSTALL AUPGM',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* THIS JCL WAS GENERATED AUTOMATICALLY BY make_release.sh
//*
//RACUPDT EXEC PGM=IEBGENER
//SYSUT1   DD  DATA,DLM=@@
*************************************************************JFS03NOV77 00010000
*             LATEST UPDATE DATE                             JFS03NOV77 00020000
         LCLC  &DATE                                         JFS03NOV77 00030000
&DATE    SETC  '28AUG80'                                     JFS03NOV77 00040000
*************************************************************JFS03NOV77 00050000
AUPG TITLE ' AUPGM, AUTHORIZED VERSION OF DOPROG/DOPGM TSO CMD (&DATE)' 00060000
*                                                            JFS03NOV77 00070000
**                                                                      00080000
** THE DOPROG TSO COMMAND WAS                                           00090000
** DESIGNED AND DEVELOPED BY J. SCHINDLER (CSC), MARCH 1975             00100000
**   THE DOPROG COMMAND IS BASED ON AN EARLIER VERSION THAT WAS         00110000
**   DESIGNED AND DEVELOPED BY                                          00120000
**       GENE CZARCINSKI                                                00130000
**       NASA, GODDARD SPACE FLIGHT CENTER                              00140000
**       GREENBELT, MARYLAND                                            00150000
**                                                                      00160000
** UPDATED 10APR77, GENE CZARCINSKI, NASA/GSFC (GT00501)                00170000
**      . RENAME CSECT FROM 'GSFTSODO' TO 'DOPROG' WITH AN              00180000
**        ENTRY POINT OF DOPGM ... MORE CONSISTANCY AND                 00190000
**        CLEANER CMDLIB                                                00200000
**      . CHANGE BASE REG INIT CODE .. IT IS FLAKY AND MAY CAUSE        00210000
**        ERROR*********                                                00220000
** UPDATED 03NOV77, J. SCHINDLER, (PAC) CSC                             00230000
**      . INCLUDED CODE SO THAT THE LATEST UPDATE DATE COULD BE         00240000
**        CHANGED EASILY                                                00250000
**      . CHANGED THE LOGIC SO THAT THE 2 BYTES FROM THE ATTACH         00260000
**        ECB IS USED AS THE RETURN CODE THAT IS PASSED ON EXIT         00270000
**        FROM THIS COMMAND PROCESSOR                                   00280000
** UPDATED 18APR80, B. GODFREY, AFDSC                                   00290000
**      . BALR TO IKJPARS INSTEAD OF LINK, FOR MVS                      00300000
**      . REPLACE REGS MACRO WITH 16 EQUATES                            00310000
**      . USE UPTPREFX INSTEAD OF PSCBUSER. ADDED IKJUPT MACRO.         00320000
** UPDATED 28AUG80, B. GODFREY, AFDSC                                   00330000
**      . THIS IS A COPY OF 'DOPGM' NAMED 'AUPGM'                       00340000
**        WHICH IS LINK-EDITED WITH AC=1 AND HAS                        00350000
**        ITS NAME IN THE IKJEFTE2 TABLE OF AUTHORIZED COMMANDS.        00360000
**      . A ROUTINE HAS BEEN ADDED TO CHECK THE USER'S AUTHORITY        00370000
**        TO USE THIS VERSION.  ITS USE SHOULD BE RESTRICTED            00380000
**        TO USERS HAVING SYSTEMS SUPPORT FUNCTIONS, BECAUSE            00390000
**        IT GIVES THE PROGRAMS IT CALLS THE CAPABILITY TO              00400000
**        BYPASS SYSTEM INTEGRITY CHECKING.                             00410000
**      . THE 'USING' STMT FOR THE CPPL WAS MOVED, AND THE              00420000
**        'IKJECT' AND 'IKJPSCB' MACROS WERE ADDED.                     00430000
**      . TESTAUTH AND MSG28 ADDED.  ANSWER ZEROED IN CASE              00440000
**        ERRORRTN ENTERED PRIOR TO PARSE.                              00450000
** Updated 14APR12 - Authorization through RAC added.           JW12105 00452000
**   On systems with resource access control active the AUPGM   JW12105 00454000
**   command is executed only if the user has read access to    JW12105 00456000
**   profile PGMAUTH in the FACILITY class.                     JW12105 00458000
** ATTR - RENT                                                          00460000
** ALIAS - AUPGM                                                        00470000
**                                                                      00480000
** COMMAND -                                                            00490000
**                                                                      00500000
** AUPROG/AUPGM 'LOAD MOD NAME' ''PARMS'' TASKLIB/LIB('DSLIST')         00510000
**               RC/NORC/ALLRC                                          00520000
**    'LOAD MOD NAME' - NAME OF THE LOAD MODULE TO BE EXECUTED.         00530000
**    ''PARMS'' - PARM FIELD TO BE PASSED TO THE LOAD MODULE, QUOTED    00540000
**               STRING.                                                00550000
**    TASKLIB('DSLIST') - SPECIFIES THE DATA SET NAME(S) THAT ARE       00560000
**             CONTENATED AND USED FOR THE TASK LIBRARY.                00570000
**    LIB('DSLIST') - SAME AS TASKLIB                                   00580000
**    RC - SPECIFIES THAT THE NON-ZERO RETURN CODE FROM THE EXECUTED    00590000
**              PROGRAM IS TO BE PRINTED.                               00600000
**    NORC - SPECIFIES THAT THE RETURN CODE IS NOT TO BE PRINTED.       00610000
**          (DEFAULT).                                                  00620000
**    ALLRC - SPECIFIES THAT THE RETURN CODE IS TO BE PRINTED.          00630000
**      NOTE - THE RETURN CODE FROM THE EXECUTED PROGRAM IS ALWAYS      00640000
**             PASSED. IF THIS COMMAND PROCESSOR ENCOUNTERS AN ERROR    00650000
**             OR AN ATTENTION IS ENTERED, THE RETURN CODE IS 16.       00660000
**                                                                      00670000
**                                                                      00680000
** THIS COMMAND PROCESSOR IS DESIGNED TO LOAD AND EXECUTE (ATTACH) A    00690000
** PROGRAM IN ONE OF THE SYSTEM LINK LIBRARIES OR A USER                00700000
** LIBRARY (TASKLIB).                                                   00710000
**                                                                      00720000
AUPROG   CSECT                                                          00730000
         ENTRY AUPGM                                                    00740000
*        REGS                                                           00750000
R0       EQU   0                                                        00760000
R1       EQU   1                                                        00770000
R2       EQU   2                                                        00780000
R3       EQU   3                                                        00790000
R4       EQU   4                                                        00800000
R5       EQU   5                                                        00810000
R6       EQU   6                                                        00820000
R7       EQU   7                                                        00830000
R8       EQU   8                                                        00840000
R9       EQU   9                                                        00850000
R10      EQU   10                                                       00860000
R11      EQU   11                                                       00870000
R12      EQU   12                                                       00880000
R13      EQU   13                                                       00890000
R14      EQU   14                                                       00900000
R15      EQU   15                                                       00910000
**                                                                      00920000
** FLAGS SETTINGS -                                                     00930000
**                                                                      00940000
**       1... ... - RC (PRINT NON-ZERO RETURN CODES)                    00950000
**       .1.. .... - ALLRC (PRINT ALL RETURN CODES)                     00960000
**       ..1. .... - TASKLIB SPECIFIED                                  00970000
**       ...1 .... - DSNAME INPUT IN QOUTES                             00980000
**       .... 1111 - NOT USED                                           00990000
**                                                                      01000000
**                                                                      01010000
** FLAGS EQUATES -                                                      01020000
**                                                                      01030000
RCFLG    EQU   B'10000000' PRINT NON-ZERO RC FLAG                       01040000
ALLRCFLG EQU   B'01000000' PRINT ALL RC FLAG                            01050000
TASKFLG  EQU   B'00100000' TASKLIB WAS SPECIFIED                        01060000
QFLG     EQU   B'00010000' DSNAME WAS INPUT IN QUOTES FLAG              01070000
**                                                                      01080000
AUPGM    SAVE  (14,12),,AUPROG/AUPGM-GT00501-&DATE                      01090000
         LA    R10,0(,R15)    *** INIT BASE REGS ***        GT00501     01100000
         LA    R11,2048                                     GT00501     01110000
         AR    R11,R11                                      GT00501     01120000
         AR    R11,R10                                      GT00501     01130000
         USING AUPROG,R10,R11                                           01140000
         LR    R9,R1 SAVE CPPL PTR                                      01150000
         USING CPPL,R9                                                  01160000
         LA    R0,RENTLEN LOAD LENGTH OF RENT DSECT                     01170000
         GETMAIN R,LV=(0) SP=0                                          01180000
         USING SAVE,R1                                                  01190000
         ST    R1,8(R13)                                                01200000
         ST    R13,4(R1)                                                01210000
         LR    R13,R1                                                   01220000
         DROP  R1                                                       01230000
         USING SAVE,R13                                                 01240000
         SPACE                                                          01250000
*********************************************************************** 01260000
*                                                                     * 01270000
*         CHECK USER'S AUTHORITY,                                     * 01280000
*         IF CHECK FAILS THEN CALL 'EXEC'                             * 01290000
*         WHICH WILL EITHER ISSUE 'COMMAND NOT FOUND' MESSAGE         * 01300000
*         OR EXECUTE USER'S SYSPROC MEMBER IF FOUND.                  * 01310000
*                                                                     * 01320000
*********************************************************************** 01330000
         SPACE                                                          01340000
         L     R1,CVTPTR     get CVT address                    JW12105 01340800
         ICM   R1,B'1111',CVTSAF(R1) SAFV defined ?             JW12105 01341600
         BZ    IMPLEXUS      no RAC, try standard authorization JW12105 01342400
         USING SAFV,R1       addressability of SAFV             JW12105 01343200
         CLC   SAFVIDEN(4),SAFVID SAFV initialized ?            JW12105 01344000
         BNE   IMPLEXUS      no RAC, try standard authorization JW12105 01344800
         DROP  R1            SAFV no longer needed              JW12105 01345600
         RACHECK ENTITY=PGMAUTH,CLASS='FACILITY',ATTR=READ      JW12105 01346400
         LTR   R15,R15       RAC authorization granted?         JW12105 01347200
         BNZ   IMPLEXEC      no, exit                           JW12105 01348000
         B     IMPLTHRU      yes, go execute                    JW12105 01348800
IMPLEXUS L     R1,548              PSAAOLD, CURRENT ASCB                01350000
         L     R15,60(,R1)         ASCBTSB, TERMINAL STATUS BLOCK       01360000
         LA    R15,0(,R15)         CLEAR HIGH ORDER BYTE                01370000
         LTR   R15,R15             IS THIS A TSO SESSION                01380000
         BZ    IMPLXBKG            NO, BRANCH TO BACKGROUND TEST        01390000
         L     R1,540              PSAAOLD, CURRENT TCB                 01400000
         L     R1,180(,R1)         TCBJSCB                              01410000
         L     R1,264(,R1)         JSCBPSCB                             01420000
         LTR   R1,R1               ANY PSCB?                            01430000
         BZ    IMPLEXEC            NO - BUT THIS NEVER HAPPENS          01440000
         USING PSCB,R1                                                  01450000
*        TM    PSCBATR1,PSCBCTRL   OPERATOR                             01460000
*        BZ    IMPLEXEC            NO - BRANCH                          01470000
         CLC   PSCBUSER(2),IMPLSSP SYSTEMS SUPPORT USERID               01480000
         BE    IMPLTHRU            YES, BRANCH                          01490000
         LA    R15,USERIDS                                              01500000
IMPLLOOP CLI   0(R15),0            END OF LIST?                         01510000
         BE    IMPLEXEC            YES, NOT AUTHORIZED                  01520000
         CLC   PSCBUSER(7),0(R15)                                       01530000
         BE    IMPLTHRU                                                 01540000
         LA    R15,8(,R15)         POINT TO NEXT USERID                 01550000
         B     IMPLLOOP            GO CHECK IT                          01560000
         DROP  R1                  PSCB                                 01570000
IMPLSSP  DC    C'SY'                                                    01580000
USERIDS  DC    0D'0'               ALIGN FOR EASY ZAPS                  01590000
         DC    8D'0'               ROOM FOR 8 USERIDS                   01600000
         DC    H'0'                END OF USERID LIST                   01610000
         SPACE                                                          01620000
IMPLXBKG L     R15,172(,R1)        ASCBJBNI                             01630000
         LTR   R15,R15             IS THIS A STARTED TASK               01640000
         BZ    IMPLTHRU            YES, BRANCH                          01650000
         CLC   0(2,R15),IMPLSSP    IS JOBNAME SYSTEMS SUPPORT           01660000
         BE    IMPLTHRU                                                 01670000
         SPACE                                                          01680000
IMPLEXEC EQU   *                                                        01690000
         L     R1,CPPLCBUF                                              01700000
         XC    2(2,R1),2(R1)       SET CBUF TO IMPLICIT EXEC            01710000
         L     R1,CPPLECT          GET ECT ADDRESS                      01720000
         USING ECT,R1                                                   01730000
         CLI   ECTSCMD,C' '        IS THIS A SUBCOMMAND                 01740000
         BNE   *+10                YES - SAY SUBCOMMAND NOT FOUND       01750000
         MVC   ECTPCMD,=CL8'EXEC'  NO  - SAY COMMAND NOT FOUND          01760000
         DROP  R1                  ECT                                  01770000
         LR    R1,R13                                                   01780000
         LA    R0,RENTLEN                                               01790000
         L     R13,4(,R13)                                              01800000
         FREEMAIN R,A=(1),LV=(0)                                        01810000
         L     R1,24(,R13)         RESTORE CPPL POINTER                 01820000
         LA    R15,12(,R13)        POINT TO 2-WORD XCTL PARM            01830000
         XC    0(8,R15),0(R15)     CLEAR IT                             01840000
         XCTL  (2,12),EP=EXEC,SF=(E,(15))                               01850000
         SPACE                                                          01860000
IMPLTHRU EQU   *                                                        01870000
**                                                                      01880000
** INIT PARSE PARM LIST (PPL)                                           01890000
**                                                                      01900000
BEGIN    LA    R8,PARSEPL                                               01910000
         USING PPL,R8                                                   01920000
         MVC   PPLUPT,CPPLUPT USER PROFILE TABLE                        01930000
         MVC   PPLECT,CPPLECT ENVIRONMENT CONTROL TABLE                 01940000
         LA    R2,ECB                                                   01950000
         ST    R2,PPLECB ECB                                            01960000
         LA    R2,ANSWER                                                01970000
         XC    0(4,R2),0(R2) ZERO IF IKJRLSA BEFORE PARSE               01980000
         ST    R2,PPLANS ADDR OF PDL                                    01990000
         MVC   PPLCBUF,CPPLCBUF COMMAND BUFFER                          02000000
         XC    PPLUWA,PPLUWA USER WORK AREA ADDR                        02010000
         MVC   PPLPCL,=A(PPLPARM) PARM CONTROL LIST                     02020000
**                                                                      02030000
** INIT STACK IOPL                                                      02040000
**                                                                      02050000
         LA    R8,STAKIOPL                                              02060000
         USING IOPL,R8                                                  02070000
         MVC   IOPLUPT,CPPLUPT                                          02080000
         MVC   IOPLECT,CPPLECT                                          02090000
         LA    R2,STAKECB                                               02100000
         ST    R2,IOPLECB                                               02110000
** IOPLIOPB WILL BE INIT BY STACK MACRO                                 02120000
**                                                                      02130000
** INIT IOPL                                                            02140000
**                                                                      02150000
         LA    R8,IOPLA                                                 02160000
         USING IOPL,R8                                                  02170000
         MVC   IOPLUPT,CPPLUPT                                          02180000
         MVC   IOPLECT,CPPLECT                                          02190000
         LA    R2,ECB                                                   02200000
         ST    R2,IOPLECB                                               02210000
** IOPLIOPB WILL BE INIT BY MACRO                                       02220000
**                                                                      02230000
** INIT DAIR PARM LIST                                                  02240000
         LA    R8,DAIRPL                                                02250000
         USING DAPL,R8                                                  02260000
         MVC   DAPLUPT,CPPLUPT                                          02270000
         MVC   DAPLECT,CPPLECT                                          02280000
         LA    R2,ECB                                                   02290000
         ST    R2,DAPLECB                                               02300000
         MVC   DAPLPSCB,CPPLPSCB PROTECTED STEP CONTROL BLOCK           02310000
         LA    R2,DAIRPB                                                02320000
         ST    R2,DAPLDAPB ADDR OF DAIR PARM BLOCK                      02330000
**                                                                      02340000
** SAVE PREFIX AND LENGTH WITH PERIOD (.)                               02350000
**                                                                      02360000
         L     R15,CPPLUPT                                              02370000
         SR    R3,R3                                                    02380000
         IC    R3,UPTPREFL-UPT(,R15)  LENGTH OF PREFIX                  02390000
         LA    R15,UPTPREFX-UPT(,R15) ADDRESS OF PREFIX                 02400000
         LR    R4,R3                                                    02410000
         BCTR  R3,R0                                                    02420000
         LA    R14,USERID                                               02430000
         EX    R3,MOVE                                                  02440000
*MOVE    MVC   0(0,R14),0(R15)                                          02450000
         LA    R3,USERID(R4)                                            02460000
         MVI   0(R3),C'.' MOVE IN PERIOD                                02470000
         LA    R4,1(R4) FOR PERIOB                                      02480000
         STH   R4,USERIDL                                               02490000
         DROP  R8,R9                                                    02500000
**                                                                      02510000
** INIT PARM BLOCK, ETC.                                                02520000
**                                                                      02530000
         MVC   PTPB(PTREFL),PTREF INIT PUTLINE PARM BLOCK               02540000
         MVC   PGPB(PGREFL),PGREF INIT PUTGET PARM BLOCK                02550000
         MVC   STAK(STAKREFL),STAKREF INIT STACK PARM BLOCK             02560000
         XC    NODDNS,NODDNS INIT NO. OF DDNAMES SPECIFIED BY TASKLIB   02570000
         XC    ANSWER1,ANSWER1                                          02580000
         MVI   FLAGS,0                                                  02590000
         MVC   EPENTRY(4),=Y(1,58)                                      02600000
         MVC   EPNAME,BLANKS                                            02610000
         LA    R2,1                                                     02620000
         ST    R2,OLD ONLY ONE SEGMENT                                  02630000
**                                                                      02640000
** CHECK FOR AUTHORIZED ENVIRONMENT                                     02650000
**                                                                      02660000
         TESTAUTH FCTN=1                                                02670000
         LTR   R15,R15                                                  02680000
         BZ    OKAUTH                                                   02690000
         LA    R0,MSG28                                                 02700000
         BAL   R14,PUTLINE  NOT IN APF AUTHORIZED ENVIRONMENT           02710000
         B     ERRORRTN                                                 02720000
OKAUTH   EQU   *                                                        02730000
**                                                                      02740000
** PARSE THE COMMAND                                                    02750000
**                                                                      02760000
         XC    ECB,ECB                                                  02770000
         LA    R1,PARSEPL                                               02780000
         L     R15,16              CVTPTR                               02790000
         TM    524(R15),X'80'      IS IKJPARS IN LPA ?                  02800000
         BNO   PARSELNK            IF NOT, DO LINK                      02810000
         L     R15,524(,R15)       GET ADDRESS OF IKJPARS               02820000
         BALR  R14,R15             CALL PARSE SERVICE ROUTINE           02830000
         B     PARSELTR            BRANCH AROUND LINK                   02840000
PARSELNK EQU   *                                                        02850000
         LINK  EP=IKJPARS LINK TO PARSE                                 02860000
PARSELTR LTR   R15,R15                                                  02870000
         BZ    PARSEOK                                                  02880000
         LA    R0,MSG01                                                 02890000
         BAL   R14,PUTLINE PARSE ERROR                                  02900000
         B     ERRORRTN                                                 02910000
PARSEOK  DC    0H'0'                                                    02920000
         L     R12,ANSWER                                               02930000
         USING PDL,R12                                                  02940000
         STM   R14,R12,STAXSAVE+12 SAVE REGS SO STAXEXIT CAN USE        02950000
**             PROGRAM BASE REGS                                        02960000
         MVC   STAXLIST(STAXREFL),STAXREF                               02970000
         LA    R2,STAXSAVE                                              02980000
         STAX  STAXEXIT,USADDR=(R2),MF=(E,STAXLIST)                     02990000
         CLI   RCKEYWD+1,2 TEST FOR NO RC                               03000000
         BE    TESTAKW GO TEST FOR TASK LIB                             03010000
         BH    SETALLFL                                                 03020000
         OI    FLAGS,RCFLG                                              03030000
         B     TESTAKW GO TEST FOR TASK LIB                             03040000
SETALLFL OI    FLAGS,ALLRCFLG                                           03050000
TESTAKW  DC    0H'0'                                                    03060000
         CLI   TASKEYWD+1,0 TEST FOR TASKLIB                            03070000
         BE    MOVEEP                                                   03080000
**                                                                      03090000
**INIT PARSEPL WITH DIFFERENT ANSWER ADDR AND COMMAND BUFFER IN         03100000
** CASE THE DATA SET(S) WHOSE NAMES ARE SPECIFIED BY THE TASKLIB        03110000
** KEYWORD ARE NOT IN THE CATALOG, CANT BE ALLOCATED, ETC.              03120000
**                                                                      03130000
         OI    FLAGS,TASKFLG                                            03140000
         LA    R2,ANSWER1                                               03150000
         ST    R2,PARSEPL+16                                            03160000
         LA    R2,CMDLEN                                                03170000
         ST    R2,PARSEPL+20                                            03180000
         MVC   PARSEPL+12,=A(DSNPPL)                                    03190000
         LA    R9,TASKDSNA USE R9 AS BASE FOR DSNAME PDE                03200000
TASKLP1  DC    0H'0'                                                    03210000
         NI    FLAGS,255-QFLG                                           03220000
         L     R15,0(R9) LOAD ADDR OF DSNAME                            03230000
         LTR   R15,R15 TEST FOR DSNAME                                  03240000
         BNZ   TASKB                                                    03250000
**                                                                      03260000
** DATA SET NAME ERROR -- PROBABLY MEMBER NAME ONLY                     03270000
**                                                                      03280000
         TM    14(R9),B'10000000' TEST FOR MEMBER NAME                  03290000
         BO    TASKA                                                    03300000
         LA    R0,MSG02 INVALID DSNAME                                  03310000
         BAL   R14,PUTLINE                                              03320000
         B     TASKPRMT GO PROMPT FOR ANOTHER DSNAME                    03330000
TASKA    OI    FLAGS,QFLG SO ''( ... )'' WILL BE PRINTED                03340000
         LA    R14,CMDBUF                                               03350000
         MVI   0(R14),C'('                                              03360000
         LA    R14,1(R14)                                               03370000
         LA    R2,1                                                     03380000
         LH    R3,12(R9) LOAD LENGTH OF MEMBER NAME                     03390000
         AR    R2,R3                                                    03400000
         BCTR  R3,R0                                                    03410000
         L     R15,8(R9) LOAD ADDR OF MEMBER NAME                       03420000
         EX    R3,MOVE                                                  03430000
         LA    R14,1(R3,R14)                                            03440000
         MVI   0(R14),C')'                                              03450000
         LA    R2,1(R2)                                                 03460000
         STH   R2,CMDOFF                                                03470000
         LA    R0,CMDOFF                                                03480000
         LA    R1,MSG03                                                 03490000
         BAL   R14,MSGOUT GO OUTPUT 'DATA SET --- INVALID'              03500000
         B     TASKPRMT GO PROMPT FOR ANOTHER DATA SET NAME             03510000
**                                                                      03520000
** DATA SET NAME WAS SPECIFIED, IGNORE MEMBER NAME IF SPECIFIED.        03530000
**                                                                      03540000
TASKB    DC    0H'0'                                                    03550000
         TM    6(R9),B'01000000' TEST FOR QUOTES                        03560000
         BZ    TASKC                                                    03570000
         OI    FLAGS,QFLG SET QUOTES FLAG                               03580000
TASKC    LH    R2,4(R9) LOAD LENGTH OF DATA SET NAME                    03590000
         STH   R2,DSNLEN                                                03600000
         BCTR  R2,R0                                                    03610000
         LA    R14,DSNAME R15= ADDR OF INPUT DSNAME                     03620000
         EX    R2,MOVE                                                  03630000
**                                                                      03640000
** ALLOC DSNAME                                                         03650000
**                                                                      03660000
         LA    R8,DAIRPB                                                03670000
         USING DAPB08,R8                                                03680000
         MVC   DA08CD,=X'0008'                                          03690000
         XC    DA08FLG(6),DA08FLG                                       03700000
         LA    R2,DSNLEN                                                03710000
         ST    R2,DA08PDSN                                              03720000
         MVI   DA08DDN,C' '                                             03730000
         MVC   DA08DDN+1(23),DA08DDN                                    03740000
         XC    DA08BLK(16),DA08BLK                                      03750000
         MVI   DA08MNM,C' '                                             03760000
         MVC   DA08MNM+1(15),DA08MNM                                    03770000
         MVI   DA08DSP1,B'00001000' SHR                                 03780000
         MVI   DA08DPS2,B'00001000' KEEP                                03790000
         MVI   DA08DPS3,B'00001000' KEEP                                03800000
         XC    DA08CTL(5),DA08CTL                                       03810000
         TM    FLAGS,QFLG TEST FOR QUOTE                                03820000
         BO    *+8                                                      03830000
         OI    DA08CTL,B'00100000' PREFIX USER ID                       03840000
         XC    ECB,ECB                                                  03850000
         LA    R1,DAIRPL                                                03860000
         LINK  EP=IKJEFD00 LINK TO DAIR                                 03870000
         LTR   R15,R15                                                  03880000
         BZ    TESTPDS                                                  03890000
         CH    R15,=H'8' TEST FOR CATALOG ERROR                         03900000
         BNE   TASKDAER                                                 03910000
         CLC   DA08DARC,=X'170C'                                        03920000
         BNE   TASKCTER                                                 03930000
         TM    FLAGS,QFLG TEST FOR QUOTES                               03940000
         BO    TASKDAER                                                 03950000
**                                                                      03960000
** SINCE THE DSNAME IS NOT IN QUOTES, APPEND .LOAD AND TRY ALLOC AGAIN  03970000
**                                                                      03980000
         LH    R2,DSNLEN                                                03990000
         LA    R3,DSNAME(R2)                                            04000000
         MVC   0(5,R3),=C'.LOAD'                                        04010000
         LA    R2,5(R2)                                                 04020000
         STH   R2,DSNLEN                                                04030000
         XC    DA08DARC(4),DA08DARC                                     04040000
         XC    ECB,ECB                                                  04050000
         LA    R1,DAIRPL                                                04060000
         LINK  EP=IKJEFD00 LINK TO DAIR                                 04070000
         LTR   R15,R15                                                  04080000
         BZ    TESTPDS                                                  04090000
         LH    R2,DSNLEN                                                04100000
         SH    R2,=H'5' FOR .LOAAD                                      04110000
         STH   R2,DSNLEN                                                04120000
         CH    R15,=H'8' TEST FOR CATALOG ERROR                         04130000
         BNE   TASKDAER                                                 04140000
TASKCTER DC    0H'0' CATALOG ERROR                                      04150000
         LA    R0,DSNLEN                                                04160000
         LA    R1,MSG06 DSNAME NOT IN CATALOG                           04170000
         BAL   R14,MSGOUT                                               04180000
         B     TASKPRMT GO PROMPT                                       04190000
**                                                                      04200000
** ALLOCATION ERROR                                                     04210000
**                                                                      04220000
TASKDAER DC    0H'0'                                                    04230000
         ST    R15,SAVER15                                              04240000
         LA    R0,DSNLEN                                                04250000
         LA    R1,MSG25                                                 04260000
         BAL   R14,MSGOUT                                               04270000
         L     R15,SAVER15                                              04280000
         CH    R15,=H'12'                                               04290000
         BH    TESTDARC                                                 04300000
         MVC   WORKBUFF(L'MSG07),MSG07                                  04310000
         LA    R6,L'MSG07+8                                             04320000
         STH   R6,WORKLEN                                               04330000
         XC    WORKOFF,WORKOFF                                          04340000
         LH    R2,DA08DARC                                              04350000
         LA    R4,4                                                     04360000
         SR    R5,R5                                                    04370000
         LA    R6,WORKBUFF+L'MSG07-1                                    04380000
TASKLP2  SRDL  R2,4                                                     04390000
         SRL   R3,28                                                    04400000
         IC    R5,TABLE(R3)                                             04410000
         STC   R5,0(R4,R6)                                              04420000
         BCT   R4,TASKLP2                                               04430000
         LA    R0,WORKLEN                                               04440000
         BAL   R14,PUTLINE                                              04450000
         B     TASKPRMT                                                 04460000
TESTDARC DC    0H'0'                                                    04470000
         CH    R15,=H'16'                                               04480000
         BH    TSTDARC1                                                 04490000
         LA    R0,MSG26  ALLOCATIONS EXCEEDED                           04500000
         BAL   R14,PUTLINE                                              04510000
         B     TASKPRMT                                                 04520000
TSTDARC1 DC    0H'0'                                                    04530000
         MVC   WORKBUFF(L'MSG27),MSG27                                  04540000
         CVD   R15,DWORK                                                04550000
         UNPK  DWORK(3),DWORK+6(2)                                      04560000
         OI    DWORK+2,C'0'                                             04570000
         MVC   WORKBUFF+L'MSG27(2),DWORK+1                              04580000
         LA    R2,L'MSG27+6                                             04590000
         STH   R2,WORKLEN                                               04600000
         XC    WORKOFF,WORKOFF                                          04610000
         LA    R0,WORKLEN                                               04620000
         BAL   R14,PUTLINE                                              04630000
         B     TASKPRMT                                                 04640000
**                                                                      04650000
** DATA SET IS ALLOCATED, TEST FOR PDS.                                 04660000
**                                                                      04670000
TESTPDS  TM    DA08DSO,B'00000010' TEST FOR PDS                         04680000
         BO    SAVEDDN                                                  04690000
         LA    R0,DSNLEN                                                04700000
         LA    R1,MSG13                                                 04710000
         BAL   R14,MSGOUT DS IS NOT PDS                                 04720000
         LA    R1,DA08DDN                                               04730000
         BAL   R14,FREEDDN GO FREE DDNAME                               04740000
         B     TASKPRMT                                                 04750000
SAVEDDN  LH    R2,NODDNS SAVE ALLOCATED DDNAME                          04760000
         SLL   R2,3 MULTIPLY BY 8                                       04770000
         LA    R3,DDNAMES(R2)                                           04780000
         MVC   0(8,R3),DA08DDN SAVE DDNAME                              04790000
         DROP  R8                                                       04800000
         LH    R2,NODDNS                                                04810000
         LA    R2,1(R2)                                                 04820000
         STH   R2,NODDNS                                                04830000
         CH    R2,=H'10'                                                04840000
         BL    TASKENLP                                                 04850000
         CLI   24(R9),X'FF' TEST FOR LAST PDE                           04860000
         BE    CONCAT  LAST PDE -- GO CONCAT.                           04870000
         LA    R0,MSG08  MORE THAN 10 DSNAMES SPECIFIED BY TASKLIB      04880000
         BAL   R14,PUTLINE                                              04890000
         LA    R2,MSG09                                                 04900000
         ST    R2,OLD+4                                                 04910000
         TCLEARQ INPUT                                                  04920000
         XC    ECB,ECB                                                  04930000
         PUTGET PARM=PGPB,OUTPUT=(OLD,SINGLE,PROMPT),                  X04940000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),                     X04950000
               TERMGET=(EDIT,WAIT),MF=(E,IOPLA)                         04960000
         CH    R15,=H'4'                                                04970000
         BH    TESTPGRC GO TEST RETURN CODE                             04980000
         L     R2,PGPB+12 LOAD ADDR OF INPUT LINE                       04990000
         LH    R3,0(R2) LOAD LENGTH OF INPUT LINE                       05000000
         CH    R3,=H'4' TEST FOR NULL LINE                              05010000
         BNH   CONCAT                                                   05020000
         B     ERRORRTN                                                 05030000
**                                                                      05040000
** PROMPT FOR TASK LIBRARY DATA SET NAME                                05050000
**                                                                      05060000
TASKPRMT LA    R2,MSG04 ENTER TASKLIB DSNAME                            05070000
         ST    R2,OLD+4                                                 05080000
         TCLEARQ INPUT                                                  05090000
         XC    ECB,ECB                                                  05100000
TASKPRMA PUTGET PARM=PGPB,OUTPUT=(OLD,SINGLE,PROMPT),                  X05110000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),                     X05120000
               TERMGET=(EDIT,WAIT),MF=(E,IOPLA)                         05130000
         CH    R15,=H'4'                                                05140000
         BH    TESTPGRC                                                 05150000
         L     R15,PGPB+12 LOAD ADDR OF INPUT LINE                      05160000
         LH    R2,0(R15) LOAD LENGTH                                    05170000
         CH    R2,=H'48' TEST FOR MAX DSN LENGTH                        05180000
         BNH   TASKPRMB                                                 05190000
         LR    R0,R2                                                    05200000
         O     R0,=X'01000000' SP=1                                     05210000
         LR    R1,R15                                                   05220000
         FREEMAIN R,LV=(0),A=(1)                                        05230000
         LA    R0,MSG05 DSNAME TOO LONG                                 05240000
         BAL   R14,PUTLINE                                              05250000
         B     TASKPRMA                                                 05260000
TASKPRMB BCTR  R2,R0                                                    05270000
         LA    R14,CMDLEN                                               05280000
         EX    R2,MOVE                                                  05290000
         LH    R0,0(R15)                                                05300000
         O     R0,=X'01000000' SP=1                                     05310000
         LR    R1,R15                                                   05320000
         FREEMAIN R,LV=(0),A=(1)                                        05330000
         LH    R2,CMDLEN                                                05340000
         CH    R2,=H'4' TEST FOR NULL LINE RESPONSE                     05350000
         BNH   TASKENLP GO TO END OF LOOP                               05360000
         IKJRLSA ANSWER1                                                05370000
         XC    ECB,ECB                                                  05380000
         LA    R1,PARSEPL                                               05390000
         LINK  EP=IKJPARS PARSE DSNAME                                  05400000
         CH    R15,=H'8' TEST FOR ATTN                                  05410000
         BE    ERRORRTN GO RETURN                                       05420000
         BH    TASKPRMC                                                 05430000
         CH    R15,=H'4'                                                05440000
         BNE   TASKPRMD                                                 05450000
TASKPRMC LA    R0,MSG01 PARSE ERROR                                     05460000
         BAL   R14,PUTLINE                                              05470000
         B     ERRORRTN                                                 05480000
TASKPRMD L     R15,ANSWER1                                              05490000
         USING DSNPDL,R15                                               05500000
         MVC   0(24,R9),NEWDSN OVERLAY OLD PDE WITH NEW ONE             05510000
         B     TASKLP1                                                  05520000
         DROP  R15                                                      05530000
**                                                                      05540000
** TEST FOR RETURN CODE FROM PUTGET                                     05550000
**                                                                      05560000
TESTPGRC CH    R15,=H'8'                                                05570000
         BE    ERRORRTN                                                 05580000
         CH    R15,=H'12'                                               05590000
         BNE   TESTRC24                                                 05600000
         TPUT  MSG10,MSG10L NO PROMPTING                                05610000
         B     ERRORRTN                                                 05620000
TESTRC24 CH    R15,=H'24'                                               05630000
         BNE   PGRC28                                                   05640000
         TPUT  MSG11,MSG11L INVALID PARMS                               05650000
         B     ERRORRTN                                                 05660000
PGRC28   TPUT  MSG12,MSG12L NOT ENOUGH SPACE                            05670000
         B     ERRORRTN                                                 05680000
**                                                                      05690000
** TEST FOR END OF PDE'S                                                05700000
**                                                                      05710000
TASKENLP CLI   24(R9),X'FF'                                             05720000
         BE    CONCAT                                                   05730000
         L     R9,24(R9) ADDR OF NEXT PDE                               05740000
         B     TASKLP1                                                  05750000
**                                                                      05760000
** ALL DATA SETS FOR TASKLIB HAVE BEEN ALLOCATED. NOW CONCATENATE THEM. 05770000
**                                                                      05780000
CONCAT   DC    0H'0'                                                    05790000
         LH    R2,NODDNS TEST FOR TASK LIBRARIES                        05800000
         CH    R2,=H'1'                                                 05810000
         BE    MOVEEP                                                   05820000
         BH    CONCAT1                                                  05830000
         LA    R0,MSG16                                                 05840000
         BAL   R14,PUTLINE                                              05850000
         LA    R2,MSG09                                                 05860000
         ST    R2,OLD+4                                                 05870000
         TCLEARQ INPUT                                                  05880000
         XC    ECB,ECB                                                  05890000
         PUTGET PARM=PGPB,OUTPUT=(OLD,SINGLE,PROMPT),                  X05900000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),                     X05910000
               TERMGET=(EDIT,WAIT),MF=(E,IOPLA)                         05920000
         CH    R15,=H'4'                                                05930000
         BH    TESTPGRC GO TEST RC FROM PUTGET                          05940000
         L     R1,PGPB+12 LOAD ADDR OF INPUT LINE                       05950000
         LH    R0,0(R1) LOAD LENGTH                                     05960000
         CH    R0,=H'4' TEST FOR NULL LINE                              05970000
         BH    ERRORRTN                                                 05980000
         O     R0,=X'01000000' SP=1                                     05990000
         FREEMAIN R,LV=(0),A=(1)                                        06000000
         B     MOVEEP                                                   06010000
CONCAT1  LA    R8,DAIRPB                                                06020000
         USING DAPB0C,R8                                                06030000
         MVC   DA0CCD,=X'000C'                                          06040000
         XC    DA0CFLG(6),DA0CFLG                                       06050000
         LH    R2,NODDNS                                                06060000
         STH   R2,DA0CNUMB                                              06070000
         XC    DA0CNUMB+2(2),DA0CNUMB+2                                 06080000
         SLL   R2,3 MULTIPLY BY 8                                       06090000
         BCTR  R2,R0                                                    06100000
         LA    R14,DA0CDDN                                              06110000
         LA    R15,DDNAMES                                              06120000
         EX    R2,MOVE                                                  06130000
         DROP  R8                                                       06140000
         XC    ECB,ECB                                                  06150000
         LA    R1,DAIRPL                                                06160000
         LINK  EP=IKJEFD00 LINK TO DAIR                                 06170000
         LTR   R15,R15                                                  06180000
         BZ    MOVEEP GO MOVE IN EP NAME                                06190000
         LA    R0,MSG14 CONCAT ERROR                                    06200000
         BAL   R14,PUTLINE                                              06210000
         B     ERRORRTN                                                 06220000
**                                                                      06230000
** HAVE ALLOCATED AND CONCATENATED THE TASK LIBRARIES, WILL NOW         06240000
** MOVE LOAD MOD NAME, MOVE THE PARM FIELD (IF SPECIFIED), OPEN THE     06250000
** TASKLIB DCB, AND ATTACH.                                             06260000
**                                                                      06270000
MOVEEP   DC    0H'0'                                                    06280000
         LH    R2,LOADMOD+4                                             06290000
         STH   R2,EPNAMEL   SAVE LENGTH OF PROGRAM NAME                 06300000
         BCTR  R2,R0                                                    06310000
         L     R15,LOADMOD                                              06320000
         LA    R14,EPNAME                                               06330000
         EX    R2,MOVE MOVE EP NAME                                     06340000
         TM    PARMA+6,B'10000000' TEST FOR PARM FIELD                  06350000
         BO    TSTPARML                                                 06360000
         XC    PARMFLDL,PARMFLDL                                        06370000
         B     TESTTSK                                                  06380000
TSTPARML LH    R2,PARMA+4 LOAD LENGTH OF QSTRING (PARMFIELD)            06390000
         CH    R2,=H'256'                                               06400000
         BNH   MOVEPARM                                                 06410000
         LA    R0,MSG15 PARM FIELD EXCEEDS 256 BYTES                    06420000
         BAL   R14,PUTLINE                                              06430000
         LA    R2,256                                                   06440000
MOVEPARM STH   R2,PARMFLDL                                              06450000
         BCTR  R2,R0                                                    06460000
         L     R15,PARMA LOAD ADDR OF QSTRING                           06470000
         LA    R14,PARMFLD                                              06480000
         EX    R2,MOVE MOVE PARM FIELD                                  06490000
         LH    R2,NODDNS                                                06500000
         TM    FLAGS,TASKFLG WAS TASKLIB SPECIFIED?                     06510000
         BZ    RELEASE NO                                               06520000
TESTTSK  DC    0H'0' TEST FOR TASKLIB DSNAMES                           06530000
         LH    R2,NODDNS                                                06540000
         LTR   R2,R2                                                    06550000
         BZ    RELEASE                                                  06560000
         MVC   TASKLIB(SKELDCBL),SKELDCB                                06570000
         LA    R8,TASKLIB                                               06580000
         USING IHADCB,R8                                                06590000
         MVC   DCBDDNAM,DDNAMES MOVE DDNAME TO DCB                      06600000
         DROP  R8                                                       06610000
**                                                                      06620000
** RELEASE PARSE SPACE BEFORE ATTACHING                                 06630000
**                                                                      06640000
RELEASE  DC    0H'0'                                                    06650000
         IKJRLSA ANSWER                                                 06660000
         XC    ANSWER,ANSWER                                            06670000
         IKJRLSA ANSWER1                                                06680000
         XC    ANSWER1,ANSWER1                                          06690000
         SR    R0,R0                                                    06700000
         GETMAIN R,LV=(0) CLEAN UP SEGMENTS                             06710000
         MVC   ATTACH(ATREFL),ATREF                                     06720000
         XC    ECB,ECB                                                  06730000
         LA    R1,PARMFLDL                                              06740000
         ST    R1,PARMADDR                                              06750000
         OI    PARMADDR,X'80'                                           06760000
         TM    FLAGS,TASKFLG TEST FOR TASK LIB                          06770000
         BZ    ATTACH1                                                  06780000
         LH    R2,NODDNS                                                06790000
         LTR   R2,R2                                                    06800000
         BNZ   OPENDCB                                                  06810000
ATTACH1  DC    0H'0'                                                    06820000
         BLDL  0,EPENTRY                                                06830000
         LTR   R15,R15 WAS EP NAME FOUND?                               06840000
         BNZ   EPERROR NO - GO PUT OUT MSG                              06850000
         LA    R1,PARMADDR                                              06860000
         LA    R15,ATTACH                                               06870000
         LA    R2,ECB                                                   06880000
         LA    R3,EPNAME                                                06890000
         XC    ECB,ECB                                                  06900000
         ATTACH DE=(3),ECB=(2),SHSPL=SPLIST,                           X06910000
               MF=(E,(1)),SF=(E,(15))                                   06920000
         B     WAIT                                                     06930000
OPENDCB  DC    0H'0'                                                    06940000
         MVC   OPEN(OPENREFL),OPENREF                                   06950000
         LA    R1,OPEN                                                  06960000
         LA    R2,TASKLIB                                               06970000
         OPEN  ((2),(INPUT)),MF=(E,(1))                                 06980000
         LA    R8,TASKLIB                                               06990000
         USING IHADCB,R8                                                07000000
         TM    DCBOFLGS,B'00010000' TEST FOR OPEN                       07010000
         BO    OPENOK                                                   07020000
         LA    R0,MSG17 DCB NOT OPEN                                    07030000
         BAL   R14,PUTLINE                                              07040000
         B     ERRORRTN                                                 07050000
         DROP  R8                                                       07060000
OPENOK   DC    0H'0'                                                    07070000
         LA    R3,TASKLIB                                               07080000
         BLDL  (3),EPENTRY                                              07090000
         LTR   R15,R15                                                  07100000
         BZ    GOTEPOK                                                  07110000
EPERROR  DC    0H'0' PROGRAM NAME NOT FOUND                             07120000
         LA    R14,WORKBUFF                                             07130000
         MVC   0(MSG20L,R14),MSG20  PROGRAM NAME                        07140000
         LA    R14,MSG20L(R14)                                          07150000
         MVI   0(R14),C''''                                             07160000
         LA    R14,1(R14)                                               07170000
         LH    R2,EPNAMEL                                               07180000
         BCTR  R2,R0                                                    07190000
         LA    R15,EPNAME                                               07200000
         EX    R2,MOVE  EPNAME                                          07210000
         LA    R14,1(R2,R14)                                            07220000
         MVI   0(R14),C''''                                             07230000
         LA    R14,1(R14)                                               07240000
         MVC   0(MSG21L,R14),MSG21  NOT FOUND                           07250000
         LA    R14,MSG21L(R14)                                          07260000
         LH    R2,NODDNS                                                07270000
         LTR   R2,R2                                                    07280000
         BZ    EPERROR1                                                 07290000
         MVC   0(4,R14),=C'TASK'                                        07300000
         B     EPERROR2                                                 07310000
EPERROR1 MVC   0(4,R14),=C'LINK'                                        07320000
EPERROR2 LA    R14,4(R14)                                               07330000
         MVC   0(MSG22L,R14),MSG22  LIBRARIES                           07340000
         LA    R14,MSG22L(R14)                                          07350000
         LA    R2,WORKLEN                                               07360000
         SR    R14,R2                                                   07370000
         STH   R14,WORKLEN                                              07380000
         XC    WORKOFF,WORKOFF                                          07390000
         LA    R0,WORKLEN                                               07400000
         BAL   R14,PUTLINE                                              07410000
EPERROR3 LA    R2,MSG23                                                 07420000
         ST    R2,OLD+4                                                 07430000
         TCLEARQ INPUT                                                  07440000
         XC    ECB,ECB                                                  07450000
         PUTGET PARM=PGPB,OUTPUT=(OLD,SINGLE,PROMPT),                  X07460000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),                     X07470000
               TERMGET=(EDIT,WAIT),MF=(E,IOPLA)                         07480000
         CH    R15,=H'4'                                                07490000
         BH    TESTPGRC                                                 07500000
         L     R1,PGPB+12  LOAD ADDR                                    07510000
         LH    R3,0(R1)                                                 07520000
         SH    R3,=H'4'                                                 07530000
         BNP   ERRORRTN                                                 07540000
         CH    R3,=H'8'                                                 07550000
         BNH   EPERROR4                                                 07560000
         LA    R0,MSG24  PROGRAM NAME EXCEEDS 8 CHAR                    07570000
         BAL   R14,PUTLINE                                              07580000
         B     EPERROR3                                                 07590000
EPERROR4 STH   R3,EPNAMEL  STORE LENGTH OF EP NAME                      07600000
         MVC   EPNAME,BLANKS                                            07610000
         LA    R14,EPNAME                                               07620000
         LA    R15,4(R1)                                                07630000
         BCTR  R3,R0                                                    07640000
         EX    R3,MOVE                                                  07650000
         OC    EPNAME,BLANKS UPPER CASE EP NAME                         07660000
         LH    R0,0(R1)                                                 07670000
         O     R0,=X'01000000'  SP=1                                    07680000
         FREEMAIN R,LV=(0),A=(1)                                        07690000
         LH    R2,NODDNS                                                07700000
         LTR   R2,R2                                                    07710000
         BNZ   OPENOK                                                   07720000
         B     ATTACH1                                                  07730000
GOTEPOK  DC    0H'0'                                                    07740000
         LA    R1,PARMADDR                                              07750000
         LA    R15,ATTACH                                               07760000
         LA    R2,ECB                                                   07770000
         LA    R4,EPNAME                                                07780000
         XC    ECB,ECB                                                  07790000
         ATTACH DE=(4),ECB=(2),TASKLIB=(3),SHSPL=SPLIST,               X07800000
               MF=(E,(1)),SF=(E,(15))                                   07810000
WAIT     ST    R1,TCB                                                   07820000
         LA    R2,ECB                                                   07830000
         WAIT  ECB=(2)                                                  07840000
         DETACH TCB                                                     07850000
         XC    COMPCDE,COMPCDE   CLEAR CONDITION CODE        JFS04NOV77 07860000
         MVC   COMPCDE+2(2),ECB+2  SAVE CONDITION CODE     JFS03NOV77   07870000
         B     RETURN                                                   07880000
**                                                                      07890000
** ERROR RETURN. ATTN, ERROR, ETC. WILL CAUSE CONTROL TO BE PASSED TO   07900000
** THIS SECTION.                                                        07910000
**                                                                      07920000
ERRORRTN DC    0H'0'                                                    07930000
         IKJRLSA ANSWER                                                 07940000
         IKJRLSA ANSWER1                                                07950000
         XC    STAKECB,STAKECB                                          07960000
         STACK PARM=STAK,DELETE=ALL,MF=(E,STAKIOPL)                     07970000
         XC    COMPCDE,COMPCDE                               JFS03NOV77 07980000
         MVI   COMPCDE+3,16                                  JFS03NOV77 07990000
**                                                                      08000000
** CLEAN UP AND RETURN                                                  08010000
**                                                                      08020000
RETURN   DC    0H'0'                                                    08030000
         LH    R2,NODDNS ANY DDNAME TO DECONCATENATE?                   08040000
         LTR   R2,R2                                                    08050000
         BZ    RETURN1                                                  08060000
         MVC   CLOSE(CLOSREFL),CLOSREF                                  08070000
         LA    R1,CLOSE                                                 08080000
         LA    R3,TASKLIB                                               08090000
         CLOSE ((3)),MF=(E,(1))                                         08100000
         CH    R2,=H'1'                                                 08110000
         BE    FREEUP ONLY ONE                                          08120000
         LA    R8,DAIRPB                                                08130000
         USING DAPB10,R8                                                08140000
         MVC   DA10CD,=X'0010'                                          08150000
         XC    DA10FLG(6),DA10FLG                                       08160000
         MVC   DA10DDN,DDNAMES                                          08170000
         XC    ECB,ECB                                                  08180000
         LA    R1,DAIRPL                                                08190000
         LINK  EP=IKJEFD00 LINK TO DAIR                                 08200000
         LTR   R15,R15                                                  08210000
         BZ    FREEUP                                                   08220000
         LA    R0,MSG18 DECONT. ERROR                                   08230000
         BAL   R14,PUTLINE                                              08240000
         DROP  R8                                                       08250000
**                                                                      08260000
** FREE THE ALLOCATED DDNAMES                                           08270000
**                                                                      08280000
FREEUP   LR    R3,R2                                                    08290000
         BCTR  R3,R0                                                    08300000
         SLL   R3,3                                                     08310000
         LA    R1,DDNAMES(R3)                                           08320000
         BAL   R14,FREEDDN                                              08330000
         BCT   R2,FREEUP                                                08340000
RETURN1  DC 0H'0'                                            JFS03NOV77 08350000
         L     R2,COMPCDE     RETURN CODE FROM ATTACHED PROG JFS03NOV77 08360000
         TM    FLAGS,ALLRCFLG                                           08370000
         BO    PUTRCOUT                                                 08380000
         TM    FLAGS,RCFLG                                              08390000
         BZ    RETURN2                                                  08400000
         LTR   R2,R2                                                    08410000
         BZ    RETURN2                                                  08420000
**                                                           JFS03NOV77 08430000
** CONVERT CONDITION CODE FROM BINARY TO EBCDIC AND          JFS04NOV77 08440000
** OUTPUT THE MESSAGE                                        JFS04NOV77 08450000
**                                                           JFS03NOV77 08460000
PUTRCOUT MVC   WORKBUFF(L'MSG19),MSG19                                  08470000
         CVD   R2,DWORK                                                 08480000
         UNPK  DWORK(5),DWORK+5(3)                           JFS03NOV77 08490000
         OI    DWORK+4,C'0'                                  JFS03NOV77 08500000
         LA    R15,DWORK-1                                   JFS03NOV77 08510000
         LA    R4,5                                          JFS04NOV77 08520000
PUTRC1   LA    R15,1(R15)                                    JFS03NOV77 08530000
         CLI   0(R15),C'0'  FIND NON ZERO                    JFS03NOV77 08540000
         BNE   PUTRC2                                        JFS03NOV77 08550000
         BCT   R4,PUTRC1                                     JFS03NOV77 08560000
         LA    R4,1  SO AT LEAST 1 0 WILL BE OUTPUT          JFS04NOV77 08570000
PUTRC2   LA    R14,WORKBUFF+L'MSG19                          JFS03NOV77 08580000
         BCTR  R4,R0  -1 FOR MVC                             JFS03NOV77 08590000
         EX    R4,MOVE MOVE IN COMP CODE RETURNED            JFS03NOV77 08600000
         LA    R3,L'MSG19+4+1(R4) +4 FOR HEADER,+1 FOR MVC   JFS03NOV77 08610000
         STH   R3,WORKLEN                                               08620000
         XC    WORKOFF,WORKOFF                                          08630000
         LA    R0,WORKLEN                                               08640000
         BAL   R14,PUTLINE                                              08650000
RETURN2  DC    0H'0'                                                    08660000
         LA    R0,RENTLEN                                               08670000
         LR    R1,R13                                                   08680000
         L     R13,SAVE+4                                               08690000
         FREEMAIN R,LV=(0),A=(1)                                        08700000
         LR    R15,R2 LOAD RETURN CODE                                  08710000
         RETURN (14,12),T,RC=(15)                                       08720000
**                                                                      08730000
**                                                                      08740000
**                                                                      08750000
MOVE     MVC   0(0,R14),0(R15)                                          08760000
**                                                                      08770000
**                                                                      08780000
**                                                                      08790000
** PUTLINE SECTION                                                      08800000
** ON ENTRY - R0 = ADDR OF MSG, R14 = RETURN ADDR                       08810000
**                                                                      08820000
PUTLINE  DC    0H'0'                                                    08830000
         ST    R14,PTSVE14                                              08840000
         XC    ECB,ECB                                                  08850000
         PUTLINE PARM=PTPB,OUTPUT=((R0),TERM,SINGLE,DATA),             X08860000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),MF=(E,IOPLA)          08870000
         LTR   R15,R15 TEST FOR ERRORS, ATTN ETC                        08880000
         BZ    PTRTN                                                    08890000
         CH    R15,=H'4' TEST FOR ATTN                                  08900000
         BE    ERRORRTN GO RETURN WITH RC=16                            08910000
         TPUT  PTERROR,PTERRORL PUTLINE ERROR                           08920000
         B     ERRORRTN                                                 08930000
PTRTN    L     R14,PTSVE14                                              08940000
         BR    R14                                                      08950000
**                                                                      08960000
** END OF PUTLINE SECTION                                               08970000
**                                                                      08980000
**                                                                      08990000
** STAX EXIT                                                            09000000
**                                                                      09010000
STAXEXIT SAVE  (14,12)                                                  09020000
         LR    R2,R13 SAVE SAVE ADDR                                    09030000
         L     R13,8(R1) LOAD ADDR OF STAXSAVE                          09040000
         USING STAXSAVE,R13                                             09050000
         ST    R2,STAXSAVE+4                                            09060000
         ST    R13,8(R2)                                                09070000
         LM    R14,R12,STAXSAVE+12 RESTORE REGS                         09080000
         LA    R2,ECB                                                   09090000
         POST  (R2),16  SET RC=16                                       09100000
         XC    STAKECB,STAKECB                                          09110000
         STACK PARM=STAK,DELETE=ALL,MF=(E,STAKIOPL)                     09120000
         TCLEARQ INPUT                                                  09130000
         TCLEARQ OUTPUT                                                 09140000
         L     R13,STAXSAVE+4                                           09150000
         RETURN (14,12),RC=0                                            09160000
         USING SAVE,R13 REESTABLISH BASE REG                            09170000
**                                                                      09180000
** END OF STAXEXIT                                                      09190000
**                                                                      09200000
**                                                                      09210000
** OUTPUT -- 'DATA SET ---- ' MSG                                       09220000
** ON ENTRY - R0 = ADDR OF DSNAME, R1 = ADDR OF LAST PART OF MSG,       09230000
** R14 = RETURN ADDR.                                                   09240000
**                                                                      09250000
MSGOUT   DC    0H'0'                                                    09260000
         ST    R14,MSGSVE14                                             09270000
         STM   R2,R8,MSGSVE28                                           09280000
         LA    R14,WORKBUFF                                             09290000
         LA    R3,DATAMSGL                                              09300000
         LA    R15,DATAMSG                                              09310000
         BCTR  R3,R0                                                    09320000
         EX    R3,MOVE                                                  09330000
*MOVE    MVC   0(0,R14),0(R15)                                          09340000
         LA    R14,1(R3,R14)                                            09350000
         MVI   0(R14),C''''                                             09360000
         LA    R14,1(R14)                                               09370000
         TM    FLAGS,QFLG TEST FOR QUOTES                               09380000
         BZ    MSGOUT1                                                  09390000
         MVI   0(R14),C''''                                             09400000
         LA    R14,1(R14)                                               09410000
         B     MSGOUT2                                                  09420000
MSGOUT1  LH    R3,USERIDL LOAD LENGTH OF USER ID                        09430000
         LA    R15,USERID                                               09440000
         BCTR  R3,R0                                                    09450000
         EX    R3,MOVE ID                                               09460000
         LA    R14,1(R3,R14)                                            09470000
MSGOUT2  DC    0H'0'                                                    09480000
         LR    R15,R0                                                   09490000
         LH    R3,0(R15) LOAD LENGTH OF DSNAME                          09500000
         LA    R15,2(R15)                                               09510000
         BCTR  R3,R0                                                    09520000
         EX    R3,MOVE                                                  09530000
         LA    R14,1(R3,R14)                                            09540000
         TM    FLAGS,QFLG                                               09550000
         BZ    MSGOUT3                                                  09560000
         MVI   0(R14),C''''                                             09570000
         LA    R14,1(R14)                                               09580000
MSGOUT3  MVI   0(R14),C''''                                             09590000
         LA    R14,1(R14)                                               09600000
         MVI   0(R14),C' '                                              09610000
         LA    R14,1(R14)                                               09620000
         LH    R3,0(R1)                                                 09630000
         LA    R15,2(R1) LOAD ADDR DSNAME                               09640000
         BCTR  R3,R0                                                    09650000
         EX    R3,MOVE                                                  09660000
         LA    R14,1(R3,R14)                                            09670000
         LA    R3,WORKLEN                                               09680000
         SR    R14,R3                                                   09690000
         STH   R14,WORKLEN                                              09700000
         XC    WORKOFF,WORKOFF                                          09710000
         LA    R0,WORKLEN                                               09720000
         BAL   R14,PUTLINE                                              09730000
         LM    R2,R8,MSGSVE28                                           09740000
         L     R14,MSGSVE14                                             09750000
         BR    R14                                                      09760000
**                                                                      09770000
** END OF OUTMSG SECTION                                                09780000
**                                                                      09790000
**                                                                      09800000
** FREE DATA SETS BY DDNAME SECTION                                     09810000
** ON ENTRY - R1 = ADDR OF DDNAME, R14 = RETURN ADDR                    09820000
**                                                                      09830000
FREEDDN  DC    0H'0'                                                    09840000
         STM   R2,R8,FREESV28                                           09850000
         ST    R14,FREESV14                                             09860000
         LA    R8,DAIRPB                                                09870000
         USING DAPB18,R8                                                09880000
         MVC   DA18CD,=X'0018'                                          09890000
         XC    DA18FLG(10),DA18FLG                                      09900000
         MVC   DA18DDN,0(R1) MOVE IN DDNAME                             09910000
         MVI   DA18MNM,C' '                                             09920000
         MVC   DA18MNM+1(9),DA18MNM                                     09930000
         MVI   DA18DPS2,B'00001000' KEEP                                09940000
         MVI   DA18CTL,B'00010000' UNALLOC PERM                         09950000
         MVC   DA18JBNM,BLANKS                                          09960000
         XC    ECB,ECB                                                  09970000
         LA    R1,DAIRPL                                                09980000
         LINK  EP=IKJEFD00 LINK TO DAIR                                 09990000
         LM    R2,R8,FREESV28                                           10000000
         L     R14,FREESV14                                             10010000
         BR    R14 RETURN                                               10020000
         DROP  R8                                                       10030000
**                                                                      10040000
** END OF FREE DDNAME SECTION                                           10050000
**                                                                      10060000
         EJECT                                                          10070000
**                                                                      10080000
** ADDRS, PARM BLOCKS, ETC.                                             10090000
**                                                                      10100000
ATREF    ATTACH SF=L                                                    10110000
ATREFL  EQU   *-ATREF                                                   10120000
STAXREF  STAX  STAXEXIT,MF=L                                            10130000
STAXREFL EQU   *-STAXREF                                                10140000
PGREF    PUTGET MF=L                                                    10150000
PGREFL   EQU   *-PGREF                                                  10160000
PTREF    PUTLINE MF=L                                                   10170000
PTREFL   EQU   *-PTREF                                                  10180000
STAKREF  STACK MF=L                                                     10190000
STAKREFL EQU   *-STAKREF                                                10200000
OPENREF  OPEN  (,),MF=L                                                 10210000
OPENREFL EQU   *-OPENREF                                                10220000
CLOSREF  CLOSE (,),MF=L                                                 10230000
CLOSREFL EQU   *-CLOSREF                                                10240000
         PRINT NOGEN                                                    10250000
SKELDCB  DCB   DSORG=PO,MACRF=R  SKELETON DCB                           10260000
SKELDCBL EQU   *-SKELDCB                                                10270000
SPLIST   DC    AL1(3,0,1,78) SHARE SUBPOOL LIST                         10280000
BLANKS   DC    CL8' '                                                   10290000
TABLE    DC    C'0123456789ABCDEF' CONVERT TABLE                        10300000
PGMAUTH  DC    CL39'PGMAUTH' facility name to authorize         JW12105 10303000
SAFVID   DC    CL4'SAFV'     SAFV eye catcher                   JW12105 10306000
         EJECT                                                          10310000
**                                                                      10320000
** MESSAGES                                                             10330000
**                                                                      10340000
MSG01    DC    Y(MSG01L,0)                                              10350000
         DC    C'PARSE ERROR'                                           10360000
MSG01L   EQU   *-MSG01                                                  10370000
MSG02    DC    Y(MSG02L,0)                                              10380000
         DC    C'INVALID DATA SET NAME'                                 10390000
MSG02L   EQU   *-MSG02                                                  10400000
MSG03    DC    Y(MSG03L)                                                10410000
         DC    C'IS AN INVALID TASK LIBRARY NAME'                       10420000
MSG03L   EQU   *-MSG03-2                                                10430000
MSG04    DC    Y(MSG04L,0)                                              10440000
         DC    C' ENTER TASKLIB DATA SET NAME-'                         10450000
MSG04L   EQU   *-MSG04                                                  10460000
MSG05    DC    Y(MSG05L,0)                                              10470000
         DC    C'DATA SET NAME LENGTH EXCEEDS 44 BYTES'                 10480000
MSG05L   EQU   *-MSG05                                                  10490000
MSG06    DC    Y(MSG06L)                                                10500000
         DC    C'NOT IN CATALOG'                                        10510000
MSG06L   EQU   *-MSG06-2                                                10520000
MSG07    DC    C'DYNAMIC ALLOCATION ERROR CODE = '                      10530000
MSG08    DC    Y(MSG08L,0)                                              10540000
         DC    C'NO MORE THAN 10 DATA SET NAMES MAY BE SPECIFIED BY THEX10550000
                TASKLIB KEYWORD'                                        10560000
MSG08L   EQU   *-MSG08                                                  10570000
MSG09    DC    Y(MSG09L,0)                                              10580000
         DC    C' ENTER NULL LINE TO CONTINUE, NON-NULL LINE TO STOP-'  10590000
MSG09L   EQU   *-MSG09                                                  10600000
MSG10    DC    C'CANNOT PROMPT, EITHER YOU ARE EXECUTING A CLIST OR YOUX10610000
               R PROFILE REQUESTS NO PROMPTING.'                        10620000
MSG10L   EQU   *-MSG10                                                  10630000
MSG11    DC    C'INVALID PARAMETERS PASSED TO PUTGET'                   10640000
MSG11L   EQU   *-MSG11                                                  10650000
MSG12    DC    C'NOT ENOUGH SPACE FOR PUTGET TO OBTAIN OUTPUT BUFFERS'  10660000
MSG12L   EQU   *-MSG12                                                  10670000
MSG13    DC    Y(MSG13L)                                                10680000
         DC    C'IS NOT PARTITIONED'                                    10690000
MSG13L   EQU   *-MSG13-2                                                10700000
MSG14    DC    Y(MSG14L,0)                                              10710000
         DC    C'ERROR IN CONCATENATING TASK LIBRARIES'                 10720000
MSG14L   EQU   *-MSG14                                                  10730000
MSG15    DC    Y(MSG15L,0)                                              10740000
         DC    C'PARM FIELD SPECIFIED EXCEEDS 256 BYTES, TRUNCATED'     10750000
MSG15L   EQU   *-MSG15                                                  10760000
MSG16    DC    Y(MSG16L,0)                                              10770000
         DC    C'NO TASK LIBRARIES'                                     10780000
MSG16L   EQU   *-MSG16                                                  10790000
MSG17    DC    Y(MSG17L,0)                                              10800000
         DC    C'TASKLIB DCB OPEN ERROR'                                10810000
MSG17L   EQU   *-MSG17                                                  10820000
MSG18    DC    Y(MSG18L,0)                                              10830000
         DC    C'ERROR IN DECONTENATING TASK LIBRARIES'                 10840000
MSG18L   EQU   *-MSG18                                                  10850000
MSG19    DC    C'CONDITION CODE = '                          JFS03NOV77 10860000
MSG20    DC    C'PROGRAM NAME '                                         10870000
MSG20L   EQU   *-MSG20                                                  10880000
MSG21    DC    C' NOT FOUND IN CONCATENATED '                           10890000
MSG21L   EQU   *-MSG21                                                  10900000
MSG22    DC    C' LIBRARIES'                                            10910000
MSG22L   EQU   *-MSG22                                                  10920000
MSG23    DC    Y(MSG23L,0)                                              10930000
         DC    C' ENTER PROGRAM NAME, OR NULL LINE TO STOP'             10940000
MSG23L   EQU   *-MSG23                                                  10950000
MSG24    DC    Y(MSG24L,0)                                              10960000
         DC    C'PROGRAM NAME EXCEEDS 8 CHARACTERS'                     10970000
MSG24L   EQU   *-MSG24                                                  10980000
MSG25    DC    Y(MSG25L)                                                10990000
         DC    C'ALLOCATION ERROR'                                      11000000
MSG25L   EQU   *-MSG25-2                                                11010000
MSG26    DC    Y(MSG26L,0)                                              11020000
         DC    C'MAXIMUM NUMBER OF ALLOCATIONS EXCEEDED'                11030000
MSG26L   EQU   *-MSG26                                                  11040000
MSG27    DC    C'DYNAMIC ALLOCATION RETURN CODE = '                     11050000
MSG28    DC    Y(MSG28L,0)                                              11060000
         DC    C'ENVIRONMENT IS NOT APF AUTHORIZED'                     11070000
MSG28L   EQU   *-MSG28                                                  11080000
*              THE ABOVE MESSAGE MEANS EITHER THE COMMAND IS            11090000
*              BEING ISSUED FROM AN UNAUTHORIZED ENVIRONMENT            11100000
*              (SUCH AS SPF FOR EXAMPLE) OR IT WAS NOT PROPERLY         11110000
*              INSTALLED. ITS NAME MUST BE IN THE IKJEFTE2 TABLE,       11120000
*              AND IT ONLY WORKS FROM 'READY' MODE.                     11130000
DATAMSG  DC    C'DATA SET '                                             11140000
DATAMSGL EQU   *-DATAMSG                                                11150000
PTERROR  DC    C'PUTLINE ERROR'                                         11160000
PTERRORL EQU   *-PTERROR                                                11170000
         PRINT GEN                                                      11180000
         EJECT                                                          11190000
**                                                                      11200000
** DSECT FOR REENTRANCY                                                 11210000
**                                                                      11220000
RENTDSCT DSECT                                                          11230000
SAVE     DS    18F                                                      11240000
STAXSAVE DS    18F                                                      11250000
DWORK    DS    D                                                        11260000
MSGSVE28 DS    7F                                                       11270000
MSGSVE14 DS    F                                                        11280000
PTSVE14  DS    F                                                        11290000
FREESV28 DS    7F                                                       11300000
FREESV14 DS    F                                                        11310000
SAVER15  DS    F                                                        11320000
IOPLA    DS    4F                                                       11330000
STAKIOPL DS    4F                                                       11340000
PARMADDR DS    F ADDR OF PARM FIELD                                     11350000
STAKECB  DS    F                                                        11360000
ECB      DS    F                                                        11370000
TCB      DS    F                                                        11380000
PARSEPL  DS    7F PARSE PARM LIST                                       11390000
DAIRPL   DS    5F DAIR PARM LIST                                        11400000
DAIRPB   DS    25F DAIR PARM BLOCK                                      11410000
DA0CDDN  EQU   DAIRPB+12 DA0CDDN NOT DEFINED IN CSECT                   11420000
ANSWER   DS    F                                                        11430000
ANSWER1  DS    F                                                        11440000
OLD      DS    F                                                        11450000
COMPCDE  DS    F                                             JFS04NOV77 11460000
         DS    A                                                        11470000
EPENTRY  DC    Y(1,58) BLDL                                             11480000
EPNAME   DS    CL8                                                      11490000
         DS    CL58                                                     11500000
WORKLEN  DS    H                                                        11510000
WORKOFF  DS    H                                                        11520000
WORKBUFF DS    CL120                                                    11530000
ATTACH   ATTACH SF=L                                                    11540000
STAXLIST STAX  STAXEXIT,USADDR=STAXSAVE,MF=L                            11550000
PGPB     PUTGET MF=L                                                    11560000
PTPB     PUTLINE MF=L                                                   11570000
STAK     STACK MF=L                                                     11580000
OPEN     OPEN  (,),MF=L                                                 11590000
CLOSE    CLOSE (,),MF=L                                                 11600000
USERIDL  DS    H                                                        11610000
USERID   DS    CL8                                                      11620000
         PRINT NOGEN                                                    11630000
TASKLIB  DCB   DSORG=PO,MACRF=R                                         11640000
         PRINT GEN                                                      11650000
EPNAMEL  DS    H                                                        11660000
CMDLEN   DS    H                                                        11670000
CMDOFF   DS    H                                                        11680000
CMDBUF   DS    CL44                                                     11690000
DSNLEN   DS    H                                                        11700000
DSNAME   DS    CL44                                                     11710000
NODDNS   DS    H                                                        11720000
DDNAMES  DS    10CL8                                                    11730000
PARMFLDL DS    H                                                        11740000
PARMFLD  DS    CL256                                                    11750000
FLAGS    DS    X                                                        11760000
RENTLEN  EQU   *-RENTDSCT                                               11770000
AUPROG   CSECT                                                          11780000
         PRINT NOGEN                                                    11790000
         EJECT                                                          11800000
**                                                                      11810000
** PARAMETER CONTROL LIST FOR PARSE                                     11820000
**                                                                      11830000
PPLPARM  IKJPARM DSECT=PDL                                              11840000
LOADMOD  IKJIDENT 'LOAD MOD NAME',MAXLNTH=8,FIRST=ALPHA,               X11850000
               OTHER=ALPHANUM,PROMPT='NAME OF PROGRAM TO BE EXECUTED'   11860000
PARMA    IKJPOSIT QSTRING,HELP='PARM FIELD TO BE PASSED TO PROGRAM'     11870000
TASKEYWD IKJKEYWD                                                       11880000
         IKJNAME 'TASKLIB',SUBFLD=TASKSUBF                              11890000
         IKJNAME 'LIB',SUBFLD=TASKSUBF                                  11900000
RCKEYWD  IKJKEYWD DEFAULT='RC'                                          11910000
         IKJNAME 'RC'                                                   11920000
         IKJNAME 'NORC'                                                 11930000
         IKJNAME 'ALLRC'                                                11940000
TASKSUBF IKJSUBF                                                        11950000
TASKDSNA IKJPOSIT DSNAME,LIST,PROMPT='TASKLIB DATA SET NAME'            11960000
         IKJENDP                                                        11970000
**                                                                      11980000
** PARSE PARM CONTROL LIST FOR PROMPT OF TASKLIB DATA SET NAME          11990000
**                                                                      12000000
DSNPPL   IKJPARM DSECT=DSNPDL                                           12010000
NEWDSN   IKJPOSIT DSNAME,PROMPT='TASKLIB DATA SET NAME'                 12020000
         IKJENDP                                                        12030000
         PRINT NOGEN                                                    12040000
         EJECT                                                          12050000
**                                                                      12060000
** MAPPING DSECTS                                                       12070000
**                                                                      12080000
         CVT   DSECT=YES                                        JW12105 12082000
         IHAPSA ,                                               JW12105 12084000
CVTSAF   EQU   248 CVTSAF doesn't exist but is a reserved field in 3.8J 12086000
         ICHSAFV  DSECT=YES  map SAFV                           JW12105 12088000
         IKJCPPL                                                        12090000
         IKJIOPL                                                        12100000
         IKJUPT                                                         12110000
         IKJPPL                                                         12120000
         IKJDAPL                                                        12130000
         IKJDAP08                                                       12140000
         IKJDAP0C                                                       12150000
         IKJDAP10                                                       12160000
         IKJDAP18                                                       12170000
         DCBD  DSORG=PO                                                 12180000
         IKJECT                                                         12190000
         IKJPSCB                                                        12200000
AUPROG   CSECT                                                          12210000
         LTORG                                                          12220000
         END   AUPROG                                                   12230000
@@
//SYSUT2   DD  DISP=(,PASS),UNIT=VIO,
//             SPACE=(CYL,(1,1)),DCB=(LRECL=80,RECFM=FB,BLKSIZE=3120)
//SYSIN    DD  DUMMY
//SYSPRINT DD  SYSOUT=*   
//ASMCL   EXEC ASMFCL,
//             PARM.ASM=(OBJ,NODECK),
//             PARM.LKED='LIST,MAP,RENT,REUS,REFR,AC=1',
//        MAC1='SYS1.AMODGEN',
//        MAC2='SYS2.MACLIB'
//ASM.SYSIN    DD DISP=(OLD,DELETE),DSN=*.RACUPDT.SYSUT2
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB(AUPGM),DISP=SHR
//*
//* Add the RAKF permissions
//*
//EXEC     EXEC PGM=IKJEFT01,                  
//       REGION=8192K                                         
//TSOLIB   DD   DSN=BREXX.CURRENT.RXLIB,DISP=SHR                             
//RXLIB    DD   DSN=BREXX.CURRENT.RXLIB,DISP=SHR                             
//SYSEXEC  DD   DSN=SYS2.EXEC,DISP=SHR                         
//SYSPRINT DD   SYSOUT=*                                      
//SYSTSPRT DD   SYSOUT=*                                      
//SYSTSIN  DD   *
 RX RDEFINE 'FACILITY PGMAUTH UACC(NONE)'
 RX PERMIT 'PGMAUTH CLASS(FACILITY) ID(ADMIN) ACCESS(READ)'
//STDOUT   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDERR   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDIN    DD   DUMMY   
