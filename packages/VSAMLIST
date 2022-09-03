//VSAMLIST  JOB (TSO),
//             'Install VSAMLIST',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//A        EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF'               00120000
//ASM.SYSIN DD *                                                        00130000
         PRINT OFF                                                      00010000
         MACRO                                                          00020000
&NAME    INIT &BASE=3,&REGS=Y,&PATCH=3,&RENT=N,&SAVE=Y                  00030000
         AIF   ((&BASE LT 13) AND (&BASE GT 1)).N020                    00040000
         MNOTE 12,'INVALID BASE REGISTER'                               00050000
         MEXIT                                                          00060000
.N020    ANOP                                                           00070000
         PUSH  PRINT                                                    00080000
         PRINT ON,GEN                                                   00090000
         EJECT                                                          00100000
&SYSECT  CSECT                                                          00110000
         USING *,15                                                     00120000
         B     BEGIN                                                    00130000
         DC    AL1(24)                                                  00140000
         DC    CL8'&SYSECT'                                             00150000
         DC    CL16'-&SYSDATE-&SYSTIME'                                 00160000
         AIF   ('&RENT' EQ 'Y').N004                                    00170000
MYSAVE   DC    18F'-1'                                                  00180000
.N004    ANOP                                                           00190000
         AIF   ('&PATCH' EQ '0').N005                                   00200000
PATCH    DC    &PATCH.CL8'*PATCH*'                                      00210000
.N005    ANOP                                                           00220000
         AIF   ('&REGS' EQ 'N').N030                                    00230000
         AIF   ('&REGS' EQ 'Y').N010                                    00240000
         MNOTE 4,'REGS OPERAND INVALID. Y SUBSTITUTED'                  00250000
.N010    ANOP                                                           00260000
R0       EQU   0                                                        00270000
R1       EQU   1                                                        00280000
R2       EQU   2                                                        00290000
R3       EQU   3                                                        00300000
R4       EQU   4                                                        00310000
R5       EQU   5                                                        00320000
R6       EQU   6                                                        00330000
R7       EQU   7                                                        00340000
R8       EQU   8                                                        00350000
R9       EQU   9                                                        00360000
R10      EQU   10                                                       00370000
R11      EQU   11                                                       00380000
R12      EQU   12                                                       00390000
R13      EQU   13                                                       00400000
R14      EQU   14                                                       00410000
R15      EQU   15                                                       00420000
.N030    ANOP                                                           00430000
BEGIN    DS   0H                                                        00440000
         STM   14,12,12(13)                                             00450000
         LR    &BASE,15                                                 00460000
         DROP  15                                                       00470000
         USING &SYSECT,&BASE                                            00480000
         AIF   ('&SAVE' EQ 'N').N003                                    00490000
         AIF   ('&RENT' EQ 'Y').N002                                    00500000
         AIF   ('&RENT' EQ 'N').N001                                    00510000
         MNOTE 4,'RENT OPERAND INVALID. N SUBSTITUTED'                  00520000
.N001    ANOP                                                           00530000
         ST    13,MYSAVE+4                                              00540000
         LR    15,13                                                    00550000
         LA    13,MYSAVE                                                00560000
         ST    13,8(15)                                                 00570000
         AGO   .N003                                                    00580000
.N002    ANOP                                                           00590000
         GETMAIN R,LV=72                                                00600000
         ST    13,4(1)                                                  00610000
         ST    1,8(13)                                                  00620000
         LR    13,1                                                     00630000
.N003    ANOP                                                           00640000
         POP   PRINT                                                    00650000
         EJECT                                                          00660000
         MEND                                                           00670000
         PRINT ON                                                       00680000
*  MODULE NAME:         VSAMLIST     (REL. 1.1  08/10/79)               00690000
*                                                                       00700000
*  MODULE DESCRIPTION:  VSAM CATALOG LIST UTILITY - CONTROL MODULE      00710000
*                                                                       00720000
*  RETURN LINKAGE:      RETURN (14,12),RC=(15)                          00730000
*                                                                       00740000
*  LINKAGE TABLE:       NONE - SINGLE ENTRY                             00750000
*                                                                       00760000
*  PARAMETERS:          R1 POINTS TO THE ADDRESS OF THE INPUT PARAMETER 00770000
*                       LIST ALIGNED ON A HALF WORD BOUNDARY.           00780000
*                        BYTE 0-1:  LENGTH OF PARAMETER                 00790000
*                        BYTE 2-4:  PARAMETER (ALL OR SUM)              00800000
*                                                                       00810000
*  EXIT:                RC=00 - SUCCESSFUL                              00820000
*                       RC=04 - UNABLE TO OPEN DCB'S                    00830000
*                       RC=08 - ERROR WHILE PROCESSING VSAM CATLG       00840000
*                                                                       00850000
*  MODULE FUNCTION:     AS THE CONTROL MODULE, IT PERFORMS MOST OF THE  00860000
*                       INITIALIZATIONS AND EXTRACT THE VSAM CATALOGS'  00870000
*                       INFORMATIONS FROM THE CONTROL STATEMENTS.       00880000
*                       FOR EACH OF THE VSAM CATALOGS SPECIFIED IN THE  00890000
*                       CONTROL STATEMENTS, THIS MODULE CREATES ONE TO  00900000
*                       THREE REPORTS ACCORDING TO THE INPUT PARAMETER  00910000
*                       SPECIFIED ON THE 'EXEC' CARD.  THEY ARE THE     00920000
*                       VSAM CATALOG SUMMARY, THE VSAM VOLUME CONTENTS  00930000
*                       AND THE VSAM TRACK ALLOCATION MAP.  IF 'ALL'    00940000
*                       IS SPECIFIED, ALL THREE REPORTS ARE PRODUCED    00950000
*                       AND IF AN OPTIONAL DD CARD 'VSAMHIST' IS IN THE 00960000
*                       JCL POINTING TO A DASD DATASET, A HISTORY FILE  00970000
*                       FOR ALL ENTRIES IN THE VSAM CATALOG IS CREATED. 00980000
*                       IF 'SUM' IS SPECIFIED, ONLY THE SUMMARY REPORT  00990000
*                       IS PRODUCED.  THE DEFAULT IS 'ALL'.             01000000
*                                                                       01010000
*  CALLER:              NONE                                            01020000
*                                                                       01030000
*  CALLS:               VSAMLST2, SORT/VSAMLST5                         01040000
*                                                                       01050000
*  SYSTEMS SERVICES:    CALL, CLOSE, DELETE, FREEMAIN, GETMAIN, LOAD,   01060000
*                       OPEN, PUT (QSAM), TIME                          01070000
*                                                                       01080000
*  MODULE ENVIRONMENT:  OS/VS1, OS/VS2                                  01090000
         TITLE 'VSAMLIST --- VSAM CATALOG LIST UTILITY CONTROL MODULE (101100000
               0)'                                                      01110000
*        *-------------------------*                                    01120000
*        *   S A M P L E   J C L   *                                    01130000
*        *-------------------------*                                    01140000
*                                                                       01150000
*  //STEP1  EXEC  PGM=VSAMLIST,PARM=ALL                                 01160000
*  //STEPLIB  DD  DSN=SYS2.LINKLIB,DISP=SHR                             01170000
*  //STEPCAT  DD  DSN=VSAMCAT1,DISP=SHR                                 01180000
*  //          .                                                        01190000
*  //          .                                                        01200000
*  //          .                                                        01210000
*  //         DD  DSN=VSAMCATN,DISP=SHR                                 01220000
*  //SYSABEND DD  SYSOUT=A                                              01230000
*  //MSGFILE  DD  SYSOUT=A,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)       01240000
*  //RPTFIL1  DD  SYSOUT=A,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)       01250000
*  //RPTFIL2  DD  SYSOUT=A,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)       01260000
*  //SYSOUT   DD  SYSOUT=A                                              01270000
*  //SORTIN   DD  DSN=&&DETAIL,DISP=(NEW,DELETE),UNIT=DISK,             01280000
*  //             SPACE=(CYL,(5,5))                                     01290000
*  //SORTWK01 DD  UNIT=DISK,SPACE=(CYL,(3))                             01300000
*  //SORTWK02 DD  UNIT=DISK,SPACE=(CYL,(3))                             01310000
*  //SORTWK03 DD  UNIT=DISK,SPACE=(CYL,(3))                             01320000
*  //*                                                                  01330000
*  //* FOLLOWING OUTPUT FILE IS OPTIONAL.  IF IT POINTS TO A DASD DATA  01340000
*  //* SET AND 'PARM=ALL' IS SPECIFIED ON THE 'EXEC' CARD, A HISTORY    01350000
*  //* FILE FOR VSAM ENTRIES WILL BE CREATED.  PLEASE NOTE THE SPEC'S   01360000
*  //* IN THE DCB.  RECORDS ARE 158 BYTES LONG.                         01370000
*  //VSAMHIST DD  DSN=VSAM.HIST.FILE,DISP=(,PASS),                      01380000
*  //             UNIT=SYSDA,SPACE=(CYL,(2,1)),                         01390000
*  //             DCB=(RECFM=FB,LRECL=158,BLKSIZE=6162)                 01400000
*  //*                                                                  01410000
*  //*  FOLLOWING INPUT FILE IS FOR VSAM CATLG LIST UTIL CONTROL STMTS  01420000
*  //*  '/' INDICATE AN OPTIONAL MASTER PASSWORD FOLLOWS                01430000
*  //*  ALL KEYWORDS SHOULD BE ON COL. 1 THRU 71 OF THE SAME CARD FOR   01440000
*  //*  EACH VSAM CATALOG TO BE LISTED                                  01450000
*  //VCLCNTL  DD  *                                                     01460000
*    LISTCAT  VSAMCAT1 / MSTRPSWD                                       01470000
*       .                                                               01480000
*       .                                                               01490000
*       .                                                               01500000
*    LISTCAT  VSAMCATN                                                  01510000
*  /*                                                                   01520000
         EJECT                                                          01530000
*        *-----------------------------------*                          01540000
*        *   R E G I S T E R   U S A G E S   *                          01550000
*        *-----------------------------------*                          01560000
*                                                                       01570000
*  R0  -  STANDARD LINKAGE                                              01580000
*  R1  -  STANDARD LINKAGE (ADDR OF PARM LIST) & TEMP WORK SPACE        01590000
*  R2  -  TEMP WORK SPACE                                               01600000
*  R3  -  TEMP WORK SPACE                                               01610000
*  R4  -  TEMP WORK SPACE                                               01620000
*  R5  -  TEMP WORK SPACE                                               01630000
*  R6  -  NOT USED                                                      01640000
*  R7  -  NOT USED                                                      01650000
*  R8  -  NOT USED                                                      01660000
*  R9  -  NOT USED                                                      01670000
*  R10 -  NOT USED                                                      01680000
*  R11 -  RETURN ADDR FOR SUBROUTINES                                   01690000
*  R12 -  BASE REG FOR CSECT VSAMLIST                                   01700000
*  R13 -  STANDARD LINKAGE (ADDR OF SAVE AREA)                          01710000
*  R14 -  STANDARD LINKAGE (ADDR TO RETURN)                             01720000
*  R15 -  STANDARD LINKAGE (ADDR OF ENTRY POINT & RETURN CODE)          01730000
         SPACE 5                                                        01740000
VSAMLIST CSECT                                                          01750000
         INIT  BASE=12                                                  01760000
*********************************************************************** 01770000
*                                                                     * 01780000
*  1.0    CONTROL MODULE                                              * 01790000
*                                                                     * 01800000
*     FUNCTION:  CHECK THE INPUT PARAMETER (ALL OR SUM).              * 01810000
*                CHECK THE FILES MSGFILE, RPTFIL1, SORTIN & VCLCNTL.  * 01820000
*                CHECK THE CONTROL STATEMENTS & THEN CALL THE PROCESS * 01830000
*                MODULE FOR EACH CATALOG.                             * 01840000
*                IF 'PARM=ALL' IS SPECIFIED, LINK TO SORT TO PRODUCE  * 01850000
*                THE TWO DETAIL REPORTS.  (SEE PAGE 2)                * 01860000
*                                                                     * 01870000
*     ERRORS:  IF OPEN OF ANY DCB FAILS, PRINT ERROR MESSAGE, SET     * 01880000
*              RETURN CODE AND RETURN.                                * 01890000
*              IF ERROR WHILE PROCESSING ANY VSAM CATALOG, SET        * 01900000
*              ERROR CODE AND PROCESS THE NEXT ONE.                   * 01910000
*                                                                     * 01920000
*********************************************************************** 01930000
         SPACE 3                                                        01940000
VCL10    EQU   *                                                        01950000
         L     R1,0(,R1)                ADDR OF THE PARM LIST           01960000
         LA    R1,0(,R1)                CLEAR THE HIGH ORDER BYTE       01970000
         MVC   PARM9410(5),0(R1)        SAVE THE PARM                   01980000
         CLC   PARM(3),=C'SUM'          IS ONLY THE SUMMARY REQUESTED?  01990000
         BNE   CHKALL                   NO - DO NOT SET 'SUM' SWITCH    02000000
         MVI   RPTSW,X'80'              YES - SET IT FOR SUMMARY ONLY   02010000
         B     INIT1                    AND GO TO INIT PART 1           02020000
         SPACE                                                          02030000
CHKALL   EQU   *                                                        02040000
         CLC   PARM(3),=C'ALL'          ARE ALL THE REPORTS REQUESTED?  02050000
         BNE   BADPARM                  NO - INVALID PARAMETER          02060000
         MVI   RPTSW,X'E0'              YES - SET IT 'ALL'              02070000
         B     INIT1                    AND GO TO INIT PART 1           02080000
         SPACE                                                          02090000
BADPARM  EQU   *                                                        02100000
         MVI   RPTSW,X'E0'              DEFAULT IS 'ALL' - SET SWITCH   02110000
         B     INIT1                    AND GO TO INIT PART 1           02120000
         EJECT                                                          02130000
*        *----------------------------*                                 02140000
*        *  CHECK THE REQUIRED DCB'S  *                                 02150000
*        *----------------------------*                                 02160000
         SPACE                                                          02170000
INIT1    EQU   *                                                        02180000
         OPEN  (MSGFILE,(OUTPUT))                                       02190000
         SPACE                                                          02200000
         TM    MSGFILE+48,X'10'                                         02210000
         BO    MSGDCBOK                                                 02220000
         SPACE                                                          02230000
         MVI   RC,X'04'                 RC=04 - OPEN FAILED FOR MSGFILE 02240000
         B     GOHOME                   RETURN IMMEDIATELY              02250000
         SPACE 2                                                        02260000
MSGDCBOK EQU   *                                                        02270000
         BAL   R11,DATETIME             DATE AND TIME AT THIS POINT     02280000
         MVC   MSG01+44(5),HHMM         TIME = HH:MM                    02290000
         MVC   MSG01+53(8),MMDDYY       DATE = MM/DD/YY                 02300000
         MVI   CNTL,C'1'                SKIP TO TOP OF NEXT PAGE        02310000
         MVC   LINE(L'MSG01),MSG01                                      02320000
         BAL   R11,PUTMSG               WRITE STARTING MESSAGE          02330000
         SPACE                                                          02340000
         OPEN  (RPTFIL1,(OUTPUT))                                       02350000
         SPACE                                                          02360000
         TM    RPTFIL1+48,X'10'                                         02370000
         BO    RP1DCBOK                                                 02380000
         SPACE                                                          02390000
         MVC   ERRDDN(8),RPTFIL1+40     SET DDNAME IN ERROR MESSAGE     02400000
         BAL   R11,ERRDCB               PRINT THE MESSAGE & SET RC=4    02410000
         B     ENDMSG                   PRINT ENDING MESSAGE            02420000
         EJECT                                                          02430000
RP1DCBOK EQU   *                                                        02440000
         TM    RPTSW,X'60'              ARE THE DETAILS REPORTS NEEDED? 02450000
         BZ    SORTINOK                 NO - SKIP THE SORT              02460000
         SPACE                                                          02470000
         OPEN  (SORTIN,(OUTPUT))                                        02480000
         SPACE                                                          02490000
         TM    SORTIN+48,X'10'                                          02500000
         BO    SORTINOK                                                 02510000
         SPACE                                                          02520000
         MVC   ERRDDN(8),SORTIN+40      SET DDNAME IN ERROR MESSAGE     02530000
         BAL   R11,ERRDCB               PRINT THE MESSAGE & SET RC=4    02540000
         B     CLSRPT                   CLEAN IT UP & QUIT              02550000
         SPACE 2                                                        02560000
SORTINOK EQU   *                                                        02570000
         OPEN  (VCLCNTL,(INPUT))                                        02580000
         SPACE                                                          02590000
         TM    VCLCNTL+48,X'10'                                         02600000
         BO    INIT2                    ALL DCB'S ARE OK - INIT PART 2  02610000
         SPACE                                                          02620000
         MVC   ERRDDN(8),VCLCNTL+40     SET DDNAME IN ERROR MESSAGE     02630000
         BAL   R11,ERRDCB               PRINT THE MESSAGE & SET RC=4    02640000
         B     CLSORT                   CLEAN IT UP & QUIT              02650000
         EJECT                                                          02660000
*        *-------------------------------------*                        02670000
*        *  LOAD VSAMLST2, VSAMLST3, VSAMLST4  *                        02680000
*        *-------------------------------------*                        02690000
         SPACE                                                          02700000
INIT2    EQU   *                                                        02710000
         LOAD  EP=VSAMLST2                                              02720000
         ST    R0,EPVCL20               ENTRY PT OF PROCESS MOD (2.0)   02730000
         SPACE                                                          02740000
         LOAD  EP=VSAMLST3                                              02750000
         ST    R0,EPVCL21               ENTRY PT OF SUMMARY MOD (2.1)   02760000
         SPACE                                                          02770000
         TM    RPTSW,X'60'              ARE THE DETAILS REPORTS NEEDED? 02780000
         BZ    INIT3                    NO - THAT'S ALL WE NEED         02790000
         SPACE                                                          02800000
         LOAD  EP=VSAMLST4                                              02810000
         ST    R0,EPVCL22               ENTRY PT OF DETAILS MOD (2.2)   02820000
         EJECT                                                          02830000
*        *--------------------------------------*                       02840000
*        *  GET WORK AREAS AND INITIALIZE THEM  *                       02850000
*        *--------------------------------------*                       02860000
         SPACE                                                          02870000
INIT3    EQU   *                                                        02880000
         GETMAIN R,LV=4096              GIMME 4K                        02890000
         SPACE                                                          02900000
         ST    R1,WORKADDR              SAVE ADDR OF AREA GOTTEN        02910000
         ST    R0,WORKLENG              SAVE LENGTH OF AREA GOTTEN      02920000
         SRL   R0,1                     DIVIDE IT IN HALVES             02930000
         ST    R1,WORKA                 ADDR OF WORK AREA 'A'           02940000
         STH   R0,0(,R1)                SET IT UP FOR SVC 26            02950000
         AR    R1,R0                    BUMP TO THE SECOND HALF         02960000
         ST    R1,WORKB                 ADDR OF WORK AREA 'B'           02970000
         STH   R0,0(,R1)                SET IT UP FOR SVC 26            02980000
         EJECT                                                          02990000
*        *----------------------------------------------------*         03000000
*        *  ALL INIT'S DONE - READ VCL CNTL STMT'S UNTIL EOD  *         03010000
*        *  CHECK THE FORMAT OF THE CONTROL STATEMENT         *         03020000
*        *  EXTRACT THE CATALOG NAME & PASSWORD IF ANY        *         03030000
*        *----------------------------------------------------*         03040000
         SPACE                                                          03050000
READCNTL EQU   *                                                        03060000
         GET   VCLCNTL,LINE             GET A 'LISTCAT' STATEMENT       03070000
         SPACE                                                          03080000
         MVI   CATNAME,X'40'            CLEAR PREVIOUS CATALOG NAME     03090000
         MVC   CATNAME+1(L'CATNAME-1),CATNAME                           03100000
         MVI   PASSWORD,X'40'           AND ITS PASSWORD                03110000
         MVC   PASSWORD+1(L'PASSWORD),PASSWORD                          03120000
         SPACE                                                          03130000
         MVI   CNTL,C'0'                SKIP 2 LINES WHEN PRINT         03140000
         LA    R1,LINE                  ADDR OF THE CONTROL STATEMENT   03150000
         LA    R2,71                    LENGTH TO SCAN                  03160000
CHKBLK1  EQU   *                                                        03170000
         CLI   0(R1),X'40'              SQUEEZE OUT THE LEADING BLANKS  03180000
         BNE   NONBLK1                                                  03190000
         LA    R1,1(,R1)                                                03200000
         BCT   R2,CHKBLK1                                               03210000
         B     BADCNTL                  A BLANK CARD                    03220000
         SPACE                                                          03230000
NONBLK1  EQU   *                                                        03240000
         CLC   LISTCAT,0(R1)                                            03250000
         BNE   BADCNTL                  FIRST KEYWORD IS NOT 'LISTCAT ' 03260000
         AR    R2,R1                                                    03270000
         LA    R1,L'LISTCAT(,R1)        ADDR OF 2ND FIELD TO PARSE      03280000
         SR    R2,R1                    LENGTH OF CHARACTERS REMAINING  03290000
CHKBLK2  EQU   *                                                        03300000
         CLI   0(R1),X'40'              SQUEEZE OUT BLANKS IN BETWEEN   03310000
         BNE   NONBLK2                                                  03320000
         LA    R1,1(,R1)                                                03330000
         BCT   R2,CHKBLK2                                               03340000
         B     BADCNTL                  'LISTCAT' IS THE ONLY KEYWORD   03350000
         EJECT                                                          03360000
*        *----------------------------------------------------*         03370000
*        *  COPY THE VSAM CATALOG FROM THE CONTROL STATEMENT  *         03380000
*        *----------------------------------------------------*         03390000
         SPACE                                                          03400000
NONBLK2  EQU   *                                                        03410000
         CLI   0(R1),C'A'               IS THE 1ST CHAR 'A-Z'?          03420000
         BL    BADCNTL                  NO - INVALID CATLG NAME         03430000
         CLI   0(R1),C'Z'                                               03440000
         BH    BADCNTL                                                  03450000
         SR    R3,R3                    CLEAR THE LENGTH OF CATLG NAME  03460000
         LR    R4,R1                    SAVE THE ADDR OF CATLG NAME     03470000
CHKCATNM EQU   *                                                        03480000
         CLI   0(R1),X'40'              KEEP SCANNING UNTIL CATLG NAME  03490000
         BE    CTNMEND                  IS ENDED BY A BLANK             03500000
         CLI   0(R1),C'/'               OR                              03510000
         BE    CTNMEND                  A SLASH                         03520000
         LA    R3,1(,R3)                OR UNTIL                        03530000
         C     R3,=F'44'                ITS LENGTH                      03540000
         BH    BADCNTL                  EXCEEDS 44 BYTES                03550000
         LA    R1,1(,R1)                                                03560000
         BCT   R2,CHKCATNM                                              03570000
         SPACE                                                          03580000
         BCTR  R3,0                     LESS ONE FOR MACHINE CODE       03590000
         EX    R3,MOVECTNM              SAVE THE CATALOG NAME AND       03600000
         B     NOPASSWD                 LOOK UP ITS PASSWORD            03610000
         SPACE 2                                                        03620000
CTNMEND  EQU   *                                                        03630000
         BCTR  R3,0                     LESS ONE FOR MACHINE CODE       03640000
         EX    R3,MOVECTNM              SAVE THE CATALOG NAME           03650000
CHKSLASH EQU   *                                                        03660000
         CLI   0(R1),C'/'               IS THERE A PASSWORD?            03670000
         BE    SKPSLASH                 YES - GO TAKE A LOOK            03680000
         LA    R1,1(,R1)                KEEP SCANNING UNTIL FOUND '/'   03690000
         BCT   R2,CHKSLASH              OR END OF STMT                  03700000
         B     NOPASSWD                 THEN LOOK UP ITS PASSWORD       03710000
         SPACE 2                                                        03720000
MOVECTNM MVC   CATNAME(0),0(R4)         DUMMY INSTRUCTION 1             03730000
         EJECT                                                          03740000
*        *-----------------------------------------------*              03750000
*        *  SAVE PASSWORD FROM THE CONTROL STATEMENT OR  *              03760000
*        *  LOOK IT UP FROM THE PRE-DEFINED TABLE        *              03770000
*        *-----------------------------------------------*              03780000
         SPACE                                                          03790000
SKPSLASH EQU   *                                                        03800000
         LA    R1,1(,R1)                BUMP PAST THE '/'               03810000
         BCTR  R2,0                     DECREMENT LENGTH TO SCAN        03820000
         SPACE                                                          03830000
CHKBLK3  EQU   *                                                        03840000
         CLI   0(R1),X'40'              SQUEEZE OUT THE BLANKS BETWEEN  03850000
         BNE   NONBLK3                  '/' AND THE PASSWORD            03860000
         LA    R1,1(,R1)                                                03870000
         BCT   R2,CHKBLK3                                               03880000
         B     BADCNTL                  MISSING PASSWORD                03890000
         SPACE                                                          03900000
NONBLK3  EQU   *                                                        03910000
         LA    R3,8                     MAXIMUM LENGTH OF THE PASSWORD  03920000
         CR    R2,R3                    CHECK TO SEE IF IT'S LESS       03930000
         BNL   SAVEPSWD                 NO - LENGTH = 8                 03940000
         LR    R3,R2                    USE REMAINING BYTES OF STMT     03950000
SAVEPSWD EQU   *                        LENGTH OF CHARACTERS REMAINING  03960000
         BCTR  R3,0                                                     03970000
         EX    R3,MOVEPSWD              SAVE THE PASSWORD               03980000
         EX    R3,COVERPSW              COVER UP THE PASSWORD WITH '*'  03990000
         MVI   PSWDFLG,X'FF'            USER SUPPLIED PASSWORD          04000000
         B     PRTCNTL                  PRINT THE CNTL STMT & START     04010000
         SPACE 2                                                        04020000
MOVEPSWD MVC   PASSWORD(0),0(R1)        DUMMY INSTRUCTION 2             04030000
COVERPSW MVC   0(0,R1),=CL8'********'   DUMMY INSTRUCTION 3             04040000
         SPACE 2                                                        04050000
NOPASSWD EQU   *                                                        04060000
         LA    R1,TBLPSWD               ADDR OF CATNAME/PASSWORD TABLE  04070000
         LA    R2,TBLPSNO               NUMBER OF ENTRIES IN TABLE      04080000
FINDPSWD EQU   *                                                        04090000
         CLC   CATNAME,0(R1)            IS THIS THE ENTRY?              04100000
         BE    COPYPSWD                 YES - COPY THE PASSWORD         04110000
         LA    R1,L'CATNAME+L'PASSWORD(,R1)  NO - BUMP TO NEXT ENTRY    04120000
         BCT   R2,FINDPSWD              AND KEEP LOOKING                04130000
         SPACE                                                          04140000
         MVI   PASSWORD,X'FF'           NEITHER SUPPLIED NOR DEFINED    04150000
         MVC   PASSWORD+1(L'PASSWORD-1),PASSWORD SET IT TO X'FFFFFFFF'  04160000
         MVI   PSWDFLG,X'01'            MARK AS 'PASSWORD NOT SUPPLIED' 04170000
         B     PRTCNTL                  PRINT THE CNTL STMT & START     04180000
         SPACE                                                          04190000
COPYPSWD EQU   *                                                        04200000
         MVC   PASSWORD,L'CATNAME(R1)   COPY THE PASSWORD & CONTINUE    04210000
         MVI   PSWDFLG,X'00'            SYSTEM DEFAULT PASSWORD         04220000
         EJECT                                                          04230000
*        *-----------------------------------------------------*        04240000
*        *  PRINT THE INPUT CONTROL STATEMENT JUST SCANNED     *        04250000
*        *  SET UP PARMLIST AND CALL PROCESS MODULE (VCL 2.0)  *        04260000
*        *-----------------------------------------------------*        04270000
         SPACE                                                          04280000
PRTCNTL  EQU   *                                                        04290000
         BAL   R11,PUTMSG                                               04300000
         SPACE                                                          04310000
         LA    R1,PARM9420              PARMLIST TO VSAMLST2            04320000
         L     R15,EPVCL20                                              04330000
         CALL  (15)                                                     04340000
         SPACE                                                          04350000
         LTR   R15,R15                  IS PROCESSING SUCCESSFUL?       04360000
         BNZ   SAVEHIRC                 NO - CHECK IF TO SAVE THE R.C.  04370000
         LH    R1,CNTCAT                YES - BUMP CATLG COUNTER        04380000
         LA    R1,1(,R1)                                                04390000
         STH   R1,CNTCAT                                                04400000
         B     NOTHIGH                  ZERO R.C. - LOWEST IT CAN BE    04410000
         SPACE                                                          04420000
SAVEHIRC EQU   *                                                        04430000
         MVI   RC,X'08'                 ERROR WHILE PROCESSING VSAM CAT 04440000
         C     R15,RCVCL20              IS THIS RETURN CODE HIGHER?     04450000
         BNH   NOTHIGH                  NO - KEEP ON KEEPING ON         04460000
         ST    R15,RCVCL20              YES - SAVE IT                   04470000
         SPACE                                                          04480000
NOTHIGH  EQU   *                                                        04490000
         CVD   R15,WORK                 CONVERT RETURN CODE TO DECIMAL  04500000
         OI    WORK+7,X'0F'             AND THEN                        04510000
         UNPK  MSG02+52(2),WORK+6(2)    SET UP COMPLETION MESSAGE       04520000
         MVC   LINE(L'MSG02),MSG02      AND                             04530000
         BAL   R11,PUTMSG               PRINT IT                        04540000
         B     READCNTL                 PROCESS THE NEXT 'LISTCAT' STMT 04550000
         SPACE 2                                                        04560000
BADCNTL  EQU   *                                                        04570000
         MVC   LINE+81(L'ERR00),ERR00   *** INVALID CONTROL STATEMENT   04580000
         BAL   R11,PUTMSG               PRINT IT AND                    04590000
         B     READCNTL                 READ NEXT STMT                  04600000
         EJECT                                                          04610000
*        *-----------------------------------*                          04620000
*        *   W R A P   U P   P H A S E   I   *                          04630000
*        *-----------------------------------*                          04640000
         SPACE                                                          04650000
EODCNTL  EQU   *                                                        04660000
         BAL   R11,DATETIME             SET TIME AT THIS POINT          04670000
         MVC   MSG03+16(5),HHMM         TIME = HH:MM                    04680000
         LH    R1,CNTCAT                CATALOG COUNT                   04690000
         CVD   R1,WORK                                                  04700000
         OI    WORK+7,X'0F'                                             04710000
         UNPK  MSG03+22(2),WORK+6(2)    CATALOGS PROCESSED SUCCESSFULLY 04720000
         L     R15,RCVCL20              HIGHEST RETURN CODE SO FAR      04730000
         CVD   R15,WORK                 CONVERT IT TO DECIMAL           04740000
         OI    WORK+7,X'0F'             AND THEN                        04750000
         UNPK  MSG03+77(2),WORK+6(2)    SET UP SUMMARY MESSAGE          04760000
         MVI   CNTL,C'0'                SKIP 2 LINES                    04770000
         MVC   LINE(L'MSG03),MSG03      AND                             04780000
         BAL   R11,PUTMSG               PRINT IT                        04790000
         SPACE                                                          04800000
         L     R0,WORKLENG                                              04810000
         L     R1,WORKADDR                                              04820000
         FREEMAIN R,LV=(0),A=(1)        FREE GOTTEN WORK AREA           04830000
         SPACE 2                                                        04840000
         DELETE EP=VSAMLST2                                             04850000
         SPACE                                                          04860000
         DELETE EP=VSAMLST3                                             04870000
         SPACE                                                          04880000
         TM    RPTSW,X'60'              ARE MODULES FOR DETAILS LOADED? 04890000
         BZ    CLSCTL                   NO - DON'T DELETE               04900000
         SPACE                                                          04910000
         DELETE EP=VSAMLST4                                             04920000
         EJECT                                                          04930000
*        *-------------------*                                          04940000
*        *  CLOSE THE DCB'S  *                                          04950000
*        *-------------------*                                          04960000
         SPACE                                                          04970000
CLSCTL   EQU   *                                                        04980000
         CLOSE VCLCNTL                                                  04990000
         SPACE                                                          05000000
CLSORT   EQU   *                                                        05010000
         TM    SORTIN+48,X'10'          HAS DCB SORTIN BEEN OPENED?     05020000
         BZ    CLSRPT                   NO - SORTIN'S NEVER OPENED      05030000
         SPACE                                                          05040000
         CLOSE SORTIN                                                   05050000
         SPACE                                                          05060000
CLSRPT   EQU   *                                                        05070000
         CLOSE RPTFIL1                                                  05080000
         SPACE                                                          05090000
         TM    RPTSW,X'60'              ARE THE DETAIL REPORTS NEEDED?  05100000
         BZ    ENDMSG                   NO - SKIP PHASE II              05110000
         LH    R1,CNTCAT                HAS ANY VSAM CATALOG BEEN       05120000
         LTR   R1,R1                    PROCESSED SUCCESSFULLY?         05130000
         BZ    ENDMSG                   NO - SKIP PHASE II TOO          05140000
         EJECT                                                          05150000
*        *---------------------------------------------------*          05160000
*        *   P H A S E   I I   -   L I N K   T O   S O R T   *          05170000
*        *---------------------------------------------------*          05180000
         SPACE                                                          05190000
INIT4    EQU   *                                                        05200000
         LOAD  EP=VSAMLST5              VCL 3.0 - E35 PRINT ROUTINE     05210000
         ST    R0,EPVCL30               ENTRY POINT ADDR OF VCL 3.0     05220000
         ST    R0,E35ADDR               SET UP SORT PARM LIST           05230000
         SPACE                                                          05240000
         LA    R1,PARMSORT              ADDR OF SORT LIST               05250000
         LINK  EP=SORT                                                  05260000
         SPACE                                                          05270000
         DELETE EP=VSAMLST5                                             05280000
         EJECT                                                          05290000
*        *---------------------------------*                            05300000
*        *  WRITE ENDING MESSAGE AND QUIT  *                            05310000
*        *---------------------------------*                            05320000
         SPACE                                                          05330000
ENDMSG   EQU   *                                                        05340000
         BAL   R11,DATETIME             DATE AND TIME AT THIS POINT     05350000
         MVC   MSG04+43(5),HHMM         TIME = HH:MM                    05360000
         MVC   MSG04+52(8),MMDDYY       DATE = MM/DD/YY                 05370000
         MVI   CNTL,C'0'                SKIP 2 LINES                    05380000
         MVC   LINE(L'MSG04),MSG04                                      05390000
         BAL   R11,PUTMSG               WRITE ENDING MESSAGE            05400000
         SPACE                                                          05410000
         CLOSE MSGFILE                                                  05420000
         SPACE 2                                                        05430000
*        *-------------*                                                05440000
*        *   E X I T   *                                                05450000
*        *-------------*                                                05460000
         SPACE                                                          05470000
GOHOME   EQU   *                                                        05480000
         SR    R15,R15                                                  05490000
         IC    R15,RC                                                   05500000
         L     R13,MYSAVE+4                                             05510000
         RETURN (14,12),RC=(15)                                         05520000
         EJECT                                                          05530000
*        *-----------------------------------------------*              05540000
*        *  DCB ERRORS - PRINT ERROR MESSAGE & SET RC=4  *              05550000
*        *-----------------------------------------------*              05560000
         SPACE                                                          05570000
ERRDCB   EQU   *                                                        05580000
         MVI   RC,X'04'                 SET RETURN CODE                 05590000
         MVI   CNTL,C'0'                SKIP 2 LINES                    05600000
         MVC   LINE(L'ERR01),ERR01      MOVE TO OUTPUT AREA & PRINT IT  05610000
         SPACE 2                                                        05620000
*        *-----------------------------*                                05630000
*        *  PRINT ROUTINE FOR MSGFILE  *                                05640000
*        *-----------------------------*                                05650000
         SPACE                                                          05660000
PUTMSG   EQU   *                                                        05670000
         PUT   MSGFILE,OAREA                                            05680000
         SPACE                                                          05690000
         MVI   CNTL,X'40'               CLEAR OUTPUT AREA               05700000
         MVC   LINE,CNTL                                                05710000
         BR    R11                      RETURN                          05720000
         EJECT                                                          05730000
*        *---------------------*                                        05740000
*        *  DATE-TIME ROUTINE  *                                        05750000
*        *---------------------*                                        05760000
         SPACE                                                          05770000
DATETIME EQU   *                                                        05780000
         TIME  DEC                                                      05790000
         SPACE                                                          05800000
         ST    R0,WORD                  WORD = X'HHMMSSTH'              05810000
         UNPK  WORK,WORD                WORK = C'0HHMMSS*'  *=X'HT'     05820000
         MVC   HHMM(2),WORK+1           SET HOURS                       05830000
         MVC   HHMM+3(2),WORK+3         SET MINUTES                     05840000
         SPACE                                                          05850000
         ST    R1,WORD                  WORD = X'00YYDDDF'              05860000
         UNPK  YYDDD,WORD                                               05870000
         MVC   MMDDYY+6(2),YYDDD        SET YEAR                        05880000
         SPACE                                                          05890000
         XC    WORK,WORK                WORK = X'00000000 00000000'     05900000
         MVO   WORK+6(2),YEAR           WORK = X'00000000 00000YY0'     05910000
         OI    WORK+7,X'0F'             WORK = X'00000000 00000YYF'     05920000
         CVB   R4,WORK                                                  05930000
         STC   R4,YEAR                  YEAR IN BINARY                  05940000
         MVC   WORK+6(2),DAYS           WORK = X'00000000 0000DDDF'     05950000
         CVB   R4,WORK                                                  05960000
         STH   R4,DAYS                  DAYS IN BINARY                  05970000
         SPACE                                                          05980000
         LA    R3,2                     ASSUME LEAP YEAR                05990000
         TM    YEAR,X'03'               IS YEAR A MULTIPLE OF 4?        06000000
         BZ    LEAPYEAR                 YES - THIS IS A LEAP YEAR       06010000
         LA    R3,2(,R3)                NO - THIS IS NOT A LEAP YEAR    06020000
LEAPYEAR EQU   *                                                        06030000
         LA    R2,DTCVNTAB              ADDR OF DATE CONVERSION TABLE   06040000
         LA    R5,12                    NUMBER OF MONTHS                06050000
CMPDAYS  EQU   *                                                        06060000
         CH    R4,6(R3,R2)              NUMBER OF DAYS UP TO NEXT MONTH 06070000
         BNH   WHICHDAY                 THIS IS THE MONTH               06080000
         LA    R2,6(,R2)                BUMP TO NEXT ENTRY              06090000
         BCT   R5,CMPDAYS               NEXT MONTH, PLEASE              06100000
         B     DATESET                  NOT IN TABLE - FORGET IT        06110000
         SPACE                                                          06120000
WHICHDAY EQU   *                                                        06130000
         SH    R4,0(R3,R2)                                              06140000
         CVD   R4,WORK                                                  06150000
         OI    WORK+7,X'0F'                                             06160000
         UNPK  MMDDYY+3(2),WORK+6(2)    SET DAY                         06170000
         MVC   MMDDYY(2),0(R2)          SET MONTH                       06180000
         SPACE                                                          06190000
DATESET  EQU   *                                                        06200000
         BR    R11                      RETURN                          06210000
         EJECT                                                          06220000
         LTORG                                                          06230000
         SPACE                                                          06240000
WORK     DC    D'0'                                                     06250000
WORD     DC    F'0'                                                     06260000
         ORG   WORD+1                                                   06270000
YEAR     DS    XL1                                                      06280000
DAYS     DS    H                                                        06290000
MMDDYY   DC    C'MM/DD/YY'                                              06300000
HHMM     DC    C'HH:MM'                                                 06310000
YYDDD    DC    C'YYDDD'                                                 06320000
         SPACE                                                          06330000
DTCVNTAB DS    0H                                                       06340000
         DC    C'01',H'0',H'0'                                          06350000
         DC    C'02',H'31',H'31'                                        06360000
         DC    C'03',H'60',H'59'                                        06370000
         DC    C'04',H'91',H'90'                                        06380000
         DC    C'05',H'121',H'120'                                      06390000
         DC    C'06',H'152',H'151'                                      06400000
         DC    C'07',H'182',H'181'                                      06410000
         DC    C'08',H'213',H'212'                                      06420000
         DC    C'09',H'244',H'243'                                      06430000
         DC    C'10',H'274',H'273'                                      06440000
         DC    C'11',H'305',H'304'                                      06450000
         DC    C'12',H'335',H'334'                                      06460000
TOTDAYS  DC    C'13',H'366',H'365'                                      06470000
         EJECT                                                          06480000
*        *-----------------------*                                      06490000
*        *   C O N S T A N T S   *                                      06500000
*        *-----------------------*                                      06510000
         SPACE                                                          06520000
TBLPSWD  DS    0F                                                       06530000
         DC    CL44'CATALOG.VCATMVA',CL8'VMVAXXXM'                      06540000
         DC    CL44'CATALOG.VCATMVB',CL8'VMVBXXXM'                      06550000
         DC    CL44'CATALOG.VCATMVC',CL8'VMVCXXXM'                      06560000
         DC    CL44'VSAMCAT1',CL8'SHALOMM'                              06570000
         DC    CL44'VSAMCAT2',CL8'BADSHAHM'                             06580000
         DC    CL44'VSAMCAT3',CL8'MANTRAM'                              06590000
         DC    CL44'VSAMCAT4',CL8'DEBITM'                               06600000
         DC    CL44'VSAMCAT5',CL8'SHOWBISM'                             06610000
         DC    CL44'VSAMCAT9',CL8'DRYSACM'                              06620000
TBLPSNO  EQU   (*-TBLPSWD)/52           L'CATNAME + L'PASSWORD = 44 + 8 06630000
         SPACE                                                          06640000
PARM9410 DS    0H                                                       06650000
LENG     DC    H'0'                                                     06660000
PARM     DC    CL3' '                                                   06670000
         DS    0H                                                       06680000
RPTSW    DC    X'00'                                                    06690000
RC       DC    X'00'                                                    06700000
RCVCL20  DC    F'0'                                                     06710000
CNTCAT   DC    H'0'                                                     06720000
         SPACE                                                          06730000
WORKADDR DC    A(0)                                                     06740000
WORKLENG DC    F'0'                                                     06750000
         SPACE                                                          06760000
LISTCAT  DC    CL8'LISTCAT'                                             06770000
CATNAME  DC    CL44'VSAM.CATALOG.NAME'                                  06780000
PASSWORD DC    CL8'PASSWORD'            X'FFFFFFFF' IF NO PASSWORD      06790000
PSWDFLG  DC    X'00'                    X'FF' - SUPPLIED                06800000
*                                       X'00' - DEFAULT                 06810000
*                                       X'01' - NOT SUPPLIED            06820000
         SPACE                                                          06830000
EPVCL    DS    0F                                                       06840000
EPVCL20  DC    A(0)                                                     06850000
EPVCL21  DC    A(0)                                                     06860000
EPVCL22  DC    A(0)                                                     06870000
EPVCL30  DC    A(0)                                                     06880000
         EJECT                                                          06890000
*        *-------------------------*                                    06900000
*        *   P A R M   L I S T S   *                                    06910000
*        *-------------------------*                                    06920000
         SPACE                                                          06930000
PARM9420 DS    0F                                                       06940000
         DC    A(CATNAME)                                               06950000
         DC    A(PASSWORD)                                              06960000
         DC    A(RPTSW)                                                 06970000
         DC    A(EPVCL)                                                 06980000
         DC    A(MSGFILE)                                               06990000
         DC    A(RPTFIL1)                                               07000000
         DC    A(SORTIN)                                                07010000
WORKA    DC    A(0)                                                     07020000
WORKB    DC    A(0)                                                     07030000
         SPACE 2                                                        07040000
PARMSORT DS    0F                                                       07050000
         DC    X'80',AL3(SORTLIST)                                      07060000
         DC    H'0'                                                     07070000
SORTLIST DC    H'24'                                                    07080000
         DC    A(SORTSTRT)                                              07090000
         DC    A(SORTEND)                                               07100000
         DC    A(RECDSTRT)                                              07110000
         DC    A(RECDEND)                                               07120000
         DC    A(0)                     NOT USED                        07130000
E35ADDR  DC    A(0)                     EP ADDR OF VSAMLST5             07140000
         SPACE                                                          07150000
SORTSTRT DC    C' SORT FIELDS=(1,12,CH,A)'                              07160000
SORTEND  DC    C' '                                                     07170000
RECDSTRT DC    C' RECORD TYPE=F,LENGTH=(92)'                            07180000
RECDEND  DC    C' '                                                     07190000
         EJECT                                                          07200000
*        *---------------------*                                        07210000
*        *   M E S S A G E S   *                                        07220000
*        *---------------------*                                        07230000
         SPACE                                                          07240000
OAREA    DS    0CL133                                                   07250000
CNTL     DC    XL1'40'                                                  07260000
LINE     DC    CL132' '                                                 07270000
         SPACE 2                                                        07280000
MSG01    DC    C'VSAMLIST-01  VSAM CATLG LIST UTIL STARTS AT HH:MM ON MX07290002
               M/DD/YY'                                                 07300002
         SPACE                                                          07310000
MSG02    DC    C'VSAMLIST-02  FUNCTION COMPLETED WITH CONDITION CODE XXX07320002
               '                                                        07330002
         SPACE                                                          07340000
MSG03    DC    C'VSAMLIST-03  AT HH:MM NN VSAM CATALOGS PROCESSED WITH X07350002
               MAXIMUM CONDITION CODE XX'                               07360002
         SPACE                                                          07370000
MSG04    DC    C'VSAMLIST-04  VSAM CATLG LIST UTIL STOPS AT HH:MM ON MMX07380002
               /DD/YY'                                                  07390002
         SPACE 2                                                        07400000
ERR00    DC    C'*** INVALID CONTROL STATEMENT ***'                     07410000
         SPACE                                                          07420000
ERR01    DC    C'VSAMLIST-05  UNABLE TO OPEN FILE RPTFILE - PROCESSING X07430002
               IS TERMINATED'                                           07440002
ERRDDN   EQU   ERR01+33                                                 07450000
         EJECT                                                          07460000
MSGFILE  DCB   DDNAME=MSGFILE,DSORG=PS,MACRF=(PM)                       07470000
         EJECT                                                          07480000
RPTFIL1  DCB   DDNAME=RPTFIL1,DSORG=PS,MACRF=(PM)                       07490000
         EJECT                                                          07500000
SORTIN   DCB   DDNAME=SORTIN,DSORG=PS,MACRF=(PM),                      X07510000
               RECFM=FB,LRECL=92,BLKSIZE=4600                           07520000
         EJECT                                                          07530000
VCLCNTL  DCB   DDNAME=VCLCNTL,DSORG=PS,MACRF=(GM),EODAD=EODCNTL         07540000
         END                                                            07550000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD                             00210000
//LKED.SYSIN DD *                                                       00220000
  NAME VSAMLIST(R)                                                      00230000
//A        EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF'               00120000
//ASM.SYSIN DD *                                                        00130000
         PRINT OFF                                                      00010000
         MACRO                                                          00020000
&NAME    INIT &BASE=3,&REGS=Y,&PATCH=3,&RENT=N,&SAVE=Y                  00030000
         AIF   ((&BASE LT 13) AND (&BASE GT 1)).N020                    00040000
         MNOTE 12,'INVALID BASE REGISTER'                               00050000
         MEXIT                                                          00060000
.N020    ANOP                                                           00070000
         PUSH  PRINT                                                    00080000
         PRINT ON,GEN                                                   00090000
         EJECT                                                          00100000
&SYSECT  CSECT                                                          00110000
         USING *,15                                                     00120000
         B     BEGIN                                                    00130000
         DC    AL1(24)                                                  00140000
         DC    CL8'&SYSECT'                                             00150000
         DC    CL16'-&SYSDATE-&SYSTIME'                                 00160000
         AIF   ('&RENT' EQ 'Y').N004                                    00170000
MYSAVE   DC    18F'-1'                                                  00180000
.N004    ANOP                                                           00190000
         AIF   ('&PATCH' EQ '0').N005                                   00200000
PATCH    DC    &PATCH.CL8'*PATCH*'                                      00210000
.N005    ANOP                                                           00220000
         AIF   ('&REGS' EQ 'N').N030                                    00230000
         AIF   ('&REGS' EQ 'Y').N010                                    00240000
         MNOTE 4,'REGS OPERAND INVALID. Y SUBSTITUTED'                  00250000
.N010    ANOP                                                           00260000
R0       EQU   0                                                        00270000
R1       EQU   1                                                        00280000
R2       EQU   2                                                        00290000
R3       EQU   3                                                        00300000
R4       EQU   4                                                        00310000
R5       EQU   5                                                        00320000
R6       EQU   6                                                        00330000
R7       EQU   7                                                        00340000
R8       EQU   8                                                        00350000
R9       EQU   9                                                        00360000
R10      EQU   10                                                       00370000
R11      EQU   11                                                       00380000
R12      EQU   12                                                       00390000
R13      EQU   13                                                       00400000
R14      EQU   14                                                       00410000
R15      EQU   15                                                       00420000
.N030    ANOP                                                           00430000
BEGIN    DS   0H                                                        00440000
         STM   14,12,12(13)                                             00450000
         LR    &BASE,15                                                 00460000
         DROP  15                                                       00470000
         USING &SYSECT,&BASE                                            00480000
         AIF   ('&SAVE' EQ 'N').N003                                    00490000
         AIF   ('&RENT' EQ 'Y').N002                                    00500000
         AIF   ('&RENT' EQ 'N').N001                                    00510000
         MNOTE 4,'RENT OPERAND INVALID. N SUBSTITUTED'                  00520000
.N001    ANOP                                                           00530000
         ST    13,MYSAVE+4                                              00540000
         LR    15,13                                                    00550000
         LA    13,MYSAVE                                                00560000
         ST    13,8(15)                                                 00570000
         AGO   .N003                                                    00580000
.N002    ANOP                                                           00590000
         GETMAIN R,LV=72                                                00600000
         ST    13,4(1)                                                  00610000
         ST    1,8(13)                                                  00620000
         LR    13,1                                                     00630000
.N003    ANOP                                                           00640000
         POP   PRINT                                                    00650000
         EJECT                                                          00660000
         MEND                                                           00670000
         PRINT ON                                                       00680000
*  MODULE NAME:         VSAMLST2     (REL. 1.1  08/10/79)               00690000
*                                                                       00700000
*  MODULE DESCRIPTION:  VSAM CATALOG LIST UTILITY - PROCESS MODULE      00710000
*                                                                       00720000
*  RETURN LINKAGE:      RETURN (14,12),RC=(15)                          00730000
*                                                                       00740000
*  LINKAGE TABLE:       NONE - SINGLE ENTRY                             00750000
*                                                                       00760000
*  PARAMETERS:          R1 POINTS TO A PARAMETER LIST ALIGNED ON A      00770000
*                       FULL WORD BOUNDARY.                             00780000
*                        WORD 1 - ADDRESS OF VSAM CATALOG NAME          00790000
*                        WORD 2 - ADDRESS OF PASSWORD & ITS FLAG        00800000
*                        WORD 3 - ADDRESS OF REPORT SWITCH              00810000
*                        WORD 4 - ADDRESS OF VCL MODULES' ENTRY POINTS  00820000
*                        WORD 5 - ADDRESS OF MSGFILE DCB                00830000
*                        WORD 6 - ADDRESS OF RPTFIL1 DCB                00840000
*                        WORD 7 - ADDRESS OF SORTIN DCB                 00850000
*                        WORD 8 - ADDRESS OF WORK AREA 'A'              00860000
*                        WORD 9 - ADDRESS OF WORK AREA 'B'              00870000
*                                                                       00880000
*  EXIT:                RC=00 - SUCCESSFUL                              00890000
*                       RC=04 - WRONG MASTER PASSWORD                   00900000
*                       RC=08 - INVALID VSAM CATALOG NAME               00910000
*                                                                       00920000
*  MODULE FUNCTION:     FOR THE VSAM CATALOG SPECIFIED IN THE PARAMETER 00930000
*                       LIST, THIS MODULE CREATES ONE TO THREE REPORTS  00940000
*                       ACCORDING TO THE REPORT SWITCH (RPTSW).         00950000
*                                                                       00960000
*  CALLER:              VSAMLIST                                        00970000
*                                                                       00980000
*  CALLS:               VSAMLST3, VSAMLST4                              00990000
*                                                                       01000000
*  SYSTEMS SERVICES:    CALL, PUT (QSAM), SVC 26                        01010000
*                                                                       01020000
*  MODULE ENVIRONMENT:  OS/VS1, OS/VS2                                  01030000
         TITLE 'VSAMLST2 --- VSAM CATALOG LIST UTILITY PROCESS MODULE (X01040000
               2.0)'                                                    01050000
*        *-----------------------------------*                          01060000
*        *   R E G I S T E R   U S A G E S   *                          01070000
*        *-----------------------------------*                          01080000
*                                                                       01090000
*  R0  -  STANDARD LINKAGE                                              01100000
*  R1  -  STANDARD LINKAGE (ADDR OF PARM LIST) & TEMP WORK SPACE        01110000
*  R2  -  NOT USED                                                      01120000
*  R3  -  NOT USED                                                      01130000
*  R4  -  NOT USED                                                      01140000
*  R5  -  NOT USED                                                      01150000
*  R6  -  NOT USED                                                      01160000
*  R7  -  NOT USED                                                      01170000
*  R8  -  NOT USED                                                      01180000
*  R9  -  NOT USED                                                      01190000
*  R10 -  NOT USED                                                      01200000
*  R11 -  NOT USED                                                      01210000
*  R12 -  BASE REG FOR CSECT VSAMLST2                                   01220000
*  R13 -  STANDARD LINKAGE (ADDR OF SAVE AREA)                          01230000
*  R14 -  STANDARD LINKAGE (ADDR TO RETURN)                             01240000
*  R15 -  STANDARD LINKAGE (ADDR OF ENTRY POINT & RETURN CODE)          01250000
         SPACE 3                                                        01260000
VSAMLST2 CSECT                                                          01270000
         INIT  BASE=12                                                  01280000
*********************************************************************** 01290000
*                                                                     * 01300000
*  2.0    PROCESS MODULE                                              * 01310000
*                                                                     * 01320000
*     FUNCTION:  CHECK THE MASTER PASSWORD & GET THE ADDR OF CATACB.  * 01330000
*                SET THE MASTER CTGPL & READ THRU THE VSAM CATLG      * 01340000
*                SEQUENTIALLY.                                        * 01350000
*                FOR EACH ENTRY CALL THE SUMMARY REPORT GENERATOR.    * 01360000
*                IF 'PARM=ALL' IS SPECIFIED, CALL THE DETAIL REPORTS  * 01370000
*                GENERATOR ALSO.                                      * 01380000
*                                                                     * 01390000
*     ERRORS:  IF INVALID VSAM CATALOG, PRINT ERROR MESSAGE, SET      * 01400000
*              RETURN CODE AND RETURN.                                * 01410000
*              IF WRONG MASTER PASSWORD, PRINT ERROR MESSAGE, SET     * 01420000
*              RETURN CODE AND RETURN.                                * 01430000
*                                                                     * 01440000
*********************************************************************** 01450000
         SPACE 3                                                        01460000
VCL20    EQU   *                                                        01470000
         MVI   RC20,X'00'               RESET RETURN CODE               01480000
         MVC   PARM9420(36),0(R1)       SAVE 9 FULL WORDS PARM LIST     01490000
         L     R1,ACATNAME              SET CATALOG NAME                01500000
         MVC   CATNAME,0(R1)                                            01510000
         MVI   CATENTRY,X'40'           SET CATALOG ENTRY TO BLANKS     01520000
         MVC   CATENTRY+1(L'CATENTRY-1),CATENTRY                        01530000
         L     R1,APASSWRD              SET MASTER PASSWORD & ITS FLAG  01540000
         MVC   MSTRPSWD,0(R1)                                           01550000
         MVC   MPSWDFLG,8(R1)           X'FF', X'00' OR X'01'           01560000
         SPACE                                                          01570000
         L     R1,AWORKA                SET ADDR OF WORK AREA 'A'       01580000
         ST    R1,CTGZWKA               IN CTLGPL-Z                     01590000
         ST    R1,CTGAWKA               IN CTLGPL-A                     01600000
         ST    R1,CTGBWKA               IN CTLGPL-B                     01610000
         SPACE                                                          01620000
CHKCATNM EQU   *                                                        01630000
         LA    R1,CTLGPLZ               SET PARM LIST FOR SVC 26        01640000
         SVC   26                                                       01650000
         LTR   R15,R15                  IS THE SUPPLIED CATNAME FOUND?  01660000
         BZ    CHKMPSWD                 YES - CHECK THE MASTER PASSWORD 01670000
         SPACE                                                          01680000
         MVI   RC20,X'08'               SET FOR WRONG CATLG NAME        01690000
         L     R1,AMSGFILE                                              01700000
         PUT   (1),MSG01                WRITE ERROR MESSAGE             01710000
         SPACE                                                          01720000
         B     GOHOME                   AND RETURN                      01730000
         EJECT                                                          01740000
*        *-------------------------*                                    01750000
*        *  CHECK MASTER PASSWORD  *                                    01760000
*        *-------------------------*                                    01770000
         SPACE                                                          01780000
CHKMPSWD EQU   *                                                        01790000
         LA    R1,CTLGPLA               SET PARM LIST FOR SVC 26        01800000
         SVC   26                                                       01810000
         LTR   R15,R15                  IS IT THE MASTER PASSWORD?      01820000
         BZ    INIT20                   YES - SAVE THE ADDR OF CATACB   01830000
         SPACE                                                          01840000
         MVI   RC20,X'04'               SET FOR WRONG MASTER PASSWORD   01850000
         CLI   MPSWDFLG,X'00'           IS PASSWORD PRE-DEFINED?        01860000
         BE    DEFAULT                  YES - MUST HAVE BEEN CHANGED    01870000
         CLI   MPSWDFLG,X'01'           IS PASSWORD SUPPLIED BY USER?   01880000
         BE    NOTSUPPL                 NO - DON'T GIVE OUT PASSWORD    01890000
         SPACE                                                          01900000
         MVC   MSG02PSW,MSTRPSWD        YES - SET ERROR MESSAGE         01910000
         LA    R0,MSG02                 ERROR MESSAGE TO BE PRINTED     01920000
         B     PRTMSG                                                   01930000
         SPACE                                                          01940000
DEFAULT  EQU   *                                                        01950000
         LA    R0,MSG03                                                 01960000
         B     PRTMSG                                                   01970000
         SPACE                                                          01980000
NOTSUPPL EQU   *                                                        01990000
         LA    R0,MSG04                                                 02000000
         SPACE                                                          02010000
PRTMSG   EQU   *                                                        02020000
         L     R1,AMSGFILE                                              02030000
         PUT   (1),(0)                  WRITE ERROR MESSAGE             02040000
         SPACE                                                          02050000
         B     GOHOME                   AND RETURN                      02060000
         EJECT                                                          02070000
*        *------------------*                                           02080000
*        *  SET PARM LISTS  *                                           02090000
*        *------------------*                                           02100000
         SPACE                                                          02110000
INIT20   EQU   *                                                        02120000
         L     R1,CTLGFLA4+20                                           02130000
         MVC   CATACB,0(R1)             ADDR OF CATLG ACB               02140000
         SPACE                                                          02150000
         L     R1,ARPTSW                ADDR OF REPORT SWITCH           02160000
         MVC   RPTSW,0(R1)              SAVE IT                         02170000
         L     R1,AEPVCL                ADDR OF VCL ENTRY POINTS        02180000
         MVC   EPVCL,4(R1)              SAVE ONLY VCL 2.1 & VCL 2.2     02190000
         L     R1,AMSGFILE              ADDR OF DCB MSGFILE             02200000
         ST    R1,AMSGF21                                               02210000
         ST    R1,AMSGF22                                               02220000
         SPACE                                                          02230000
         L     R1,ARPTFIL1              ADDR OF DCB RPTFIL1             02240000
         ST    R1,ARPTF21                                               02250000
         L     R1,ASORTIN               ADDR OF DCB SORTIN              02260000
         ST    R1,ASORT22                                               02270000
         L     R1,AWORKB                ADDR OF WORK AREA 'B'           02280000
         ST    R1,AWORK21                                               02290000
         ST    R1,AWORK22                                               02300000
         SPACE                                                          02310000
         LA    R0,4                     INIT CALL TO SET TITLES         02320000
         LA    R1,PARM9421                                              02330000
         L     R15,EPVCL21                                              02340000
         CALL  (15)                     CALL VCL 2.1                    02350000
         EJECT                                                          02360000
*        *------------------------------------*                         02370000
*        *  PROCESS - CALL VCL 2.1 & VCL 2.2  *                         02380000
*        *------------------------------------*                         02390000
         SPACE                                                          02400000
NEXTENT  EQU   *                                                        02410000
         LA    R1,CTLGPLB                                               02420000
         SVC   26                                                       02430000
         LTR   R15,R15                                                  02440000
         BNZ   GOHOME                                                   02450000
         SPACE                                                          02460000
CALL9421 EQU   *                                                        02470000
         SR    R0,R0                    REGULAR CALL                    02480000
         LA    R1,PARM9421                                              02490000
         L     R15,EPVCL21                                              02500000
         CALL  (15)                     CALL VCL 2.1                    02510000
         SPACE                                                          02520000
         TM    RPTSW,X'60'              ARE THE DETAIL REPORTS NEEDED?  02530000
         BZ    BUMPENT                  NO - BUMP TO NEXT ENTRY         02540000
         SPACE                                                          02550000
CALL9422 EQU   *                                                        02560000
         LA    R1,PARM9422                                              02570000
         L     R15,EPVCL22                                              02580000
         CALL  (15)                     CALL VCL 2.2                    02590000
         SPACE                                                          02600000
BUMPENT  EQU   *                                                        02610000
         L     R1,CTLGFLB2+20                                           02620000
         MVC   CATENTRY,0(R1)           ENTRY NAME                      02630000
         SR    R1,R1                                                    02640000
         IC    R1,CATENTRY+43                                           02650000
         LA    R1,1(,R1)                                                02660000
         STC   R1,CATENTRY+43                                           02670000
         B     NEXTENT                  NEXT ENTRY                      02680000
         EJECT                                                          02690000
*        *-------------*                                                02700000
*        *   E X I T   *                                                02710000
*        *-------------*                                                02720000
         SPACE                                                          02730000
GOHOME   EQU   *                                                        02740000
         SR    R15,R15                                                  02750000
         IC    R15,RC20                                                 02760000
         L     R13,MYSAVE+4                                             02770000
         RETURN (14,12),RC=(15)                                         02780000
         SPACE 2                                                        02790000
RC20     DC    X'00'                                                    02800000
MPSWDFLG DC    X'00'                                                    02810000
RPTSW    DC    X'00'                                                    02820000
         SPACE                                                          02830000
MSG01    DS    0CL133                                                   02840000
         DC    C'0'                     SKIP 2 LINES                    02850000
         DC    CL132'VSAMLST2---01 CATALOG NAME SPECIFIED ON THE CONTROX02860000
               L CARD IS NOT FOUND'                                     02870000
         SPACE                                                          02880000
MSG02    DS    0CL133                                                   02890000
         DC    C'0'                     SKIP 2 LINES                    02900000
         DC    CL132'VSAMLST2---02 XXXXXXXX IS AN INVALID MASTER PASSWOX02910000
               RD FOR ABOVE VSAM CATALOG'                               02920000
         ORG   MSG02+14                                                 02930000
MSG02PSW DS    CL8                                                      02940000
         ORG                                                            02950000
         SPACE                                                          02960000
MSG03    DS    0CL133                                                   02970000
         DC    C'0'                     SKIP 2 LINES                    02980000
         DC    CL132'VSAMLST2---03 DEFAULT MASTER PASSWORD IS INVALID FX02990000
               OR ABOVE VSAM CATALOG'                                   03000000
         SPACE                                                          03010000
MSG04    DS    0CL133                                                   03020000
         DC    C'0'                     SKIP 2 LINES                    03030000
         DC    CL132'VSAMLST2---04 PASSWORD MUST BE SUPPLIED FOR THIS VX03040000
               SAM CATALOG'                                             03050000
         SPACE                                                          03060000
CATACB   DC    A(0)                                                     03070000
CATNAME  DC    CL44'VSAM.CATALOG.NAME'                                  03080000
CATENTRY DC    CL44'VSAM.CATALOG.ENTRY.BLANKS'                          03090000
PASSWORD DS    0CL32                                                    03100000
MSTRPSWD DC    CL8'MSTRPSWD'                                            03110000
CNTLPSWD DC    CL8' '                                                   03120000
UPDTPSWD DC    CL8' '                                                   03130000
READPSWD DC    CL8' '                                                   03140000
         SPACE                                                          03150000
EPVCL    DS    0CL8                                                     03160000
EPVCL21  DC    A(0)                                                     03170000
EPVCL22  DC    A(0)                                                     03180000
         EJECT                                                          03190000
*        *-------------------------*                                    03200000
*        *   P A R M   L I S T S   *                                    03210000
*        *-------------------------*                                    03220000
         SPACE                                                          03230000
PARM9420 DS    0F                                                       03240000
ACATNAME DC    A(0)                                                     03250000
APASSWRD DC    A(0)                                                     03260000
ARPTSW   DC    A(0)                                                     03270000
AEPVCL   DC    A(0)                                                     03280000
AMSGFILE DC    A(0)                                                     03290000
ARPTFIL1 DC    A(0)                                                     03300000
ASORTIN  DC    A(0)                                                     03310000
AWORKA   DC    A(0)                                                     03320000
AWORKB   DC    A(0)                                                     03330000
         SPACE 2                                                        03340000
PARM9421 DS    0F                                                       03350000
ACATN21  DC    A(CATNAME)                                               03360000
ACTPL21  DC    A(CTLGPLB)                                               03370000
AMSGF21  DC    A(0)                                                     03380000
ARPTF21  DC    A(0)                                                     03390000
AWORK21  DC    A(0)                                                     03400000
         SPACE 2                                                        03410000
PARM9422 DS    0F                                                       03420000
ACATN22  DC    A(CATNAME)                                               03430000
ACTPL22  DC    A(CTLGPLB)                                               03440000
AMSGF22  DC    A(0)                                                     03450000
ASORT22  DC    A(0)                                                     03460000
AWORK22  DC    A(0)                                                     03470000
         EJECT                                                          03480000
*        *---------------------------------------------*                03490000
*        *   C T L G P L   A N D   C T G F L   -   Z   *                03500000
*        *---------------------------------------------*                03510000
         SPACE                                                          03520000
CTLGPLZ  DS    0D                                                       03530000
CTGZOPT1 DC    X'06008108'                                              03540000
*  CTGENT CONTAINS ADDR OF ENTRY NAME                                   03550000
*  CTGCAT CONTAINS ADDR OF CATLG NAME                                   03560000
*  A CATALOG MANAGEMENT SERVICES FUNCTION                               03570000
*  THE CALL IS A VSAM CATALOG MANAGEMENT REQUEST                        03580000
*  BYPASS SECURITY PROMPTING TO SYSTEM OPERATOR                         03590000
*                                                                       03600000
CTGZENT  DC    A(CATNAME)                                               03610000
CTGZCAT  DC    A(CATNAME)                                               03620000
CTGZWKA  DC    A(0)                                                     03630000
CTGZOPT2 DC    X'2000C302'                                              03640000
CTGZDDNM DC    A(0)                                                     03650000
CTGZPSWD DC    A(0)                                                     03660000
CTGZFLDS DS    0F                                                       03670000
         DC    A(CTLGFLZ1)                                              03680000
         DC    A(CTLGFLZ2)                                              03690000
         SPACE 2                                                        03700000
CTGFLZ   DS    0D                                                       03710000
         SPACE                                                          03720000
CTLGFLZ1 DC    X'01',AL3(0),F'0'                                        03730000
         DC    A(REQUES02)              ENTYPE    (LTH = 1)             03740000
         DC    3F'0'                                                    03750000
         SPACE                                                          03760000
CTLGFLZ2 DC    X'01',AL3(0),F'0'                                        03770000
         DC    A(REQUES03)              ENTNAME   (LTH = 44)            03780000
         DC    3F'0'                                                    03790000
         EJECT                                                          03800000
*        *---------------------------------------------*                03810000
*        *   C T L G P L   A N D   C T G F L   -   A   *                03820000
*        *---------------------------------------------*                03830000
         SPACE                                                          03840000
CTLGPLA  DS    0D                                                       03850000
CTGAOPT1 DC    X'46008108'                                              03860000
*  CHECK THE MASTER PASSWORD                                            03870000
*  CTGENT CONTAINS ADDR OF ENTRY NAME                                   03880000
*  CTGCAT CONTAINS ADDR OF CATLG NAME                                   03890000
*  A CATALOG MANAGEMENT SERVICES FUNCTION                               03900000
*  THE CALL IS A VSAM CATALOG MANAGEMENT REQUEST                        03910000
*  BYPASS SECURITY PROMPTING TO SYSTEM OPERATOR                         03920000
*                                                                       03930000
CTGAENT  DC    A(CATNAME)                                               03940000
CTGACAT  DC    A(CATNAME)                                               03950000
CTGAWKA  DC    A(0)                                                     03960000
CTGAOPT2 DC    X'2000C304'                                              03970000
CTGADDNM DC    A(0)                                                     03980000
CTGAPSWD DC    A(PASSWORD)                                              03990000
CTGAFLDS DS    0F                                                       04000000
         DC    A(CTLGFLA1)                                              04010000
         DC    A(CTLGFLA2)                                              04020000
         DC    A(CTLGFLA3)                                              04030000
         DC    A(CTLGFLA4)                                              04040000
         SPACE 2                                                        04050000
CTGFLA   DS    0D                                                       04060000
         SPACE                                                          04070000
CTLGFLA1 DC    X'01',AL3(0),F'0'                                        04080000
         DC    A(REQUES02)              ENTYPE    (LTH = 1)             04090000
         DC    3F'0'                                                    04100000
         SPACE                                                          04110000
CTLGFLA2 DC    X'01',AL3(0),F'0'                                        04120000
         DC    A(REQUES03)              ENTNAME   (LTH = 44)            04130000
         DC    3F'0'                                                    04140000
         SPACE                                                          04150000
CTLGFLA3 DC    X'01',AL3(0),F'0'                                        04160000
         DC    A(REQUES24)              PASSWORD  (LTH = 32)            04170000
         DC    3F'0'                                                    04180000
         SPACE                                                          04190000
CTLGFLA4 DC    X'01',AL3(0),F'0'                                        04200000
         DC    A(REQUES33)              CATACB    (LTH = 4)             04210000
         DC    3F'0'                                                    04220000
         EJECT                                                          04230000
*        *---------------------------------------------*                04240000
*        *   C T L G P L   A N D   C T G F L   -   B   *                04250000
*        *---------------------------------------------*                04260000
         SPACE                                                          04270000
CTLGPLB  DS    0D                                                       04280000
CTGBOPT1 DC    X'44108108'                                              04290000
*  CHECK THE MASTER PASSWORD                                            04300000
*  CTGENT CONTAINS ADDR OF ENTRY NAME                                   04310000
*  GET NEXT OPTION WITH LISTCAT                                         04320000
*  A CATALOG MANAGEMENT SERVICES FUNCTION                               04330000
*  THE CALL IS A VSAM CATALOG MANAGEMENT REQUEST                        04340000
*  BYPASS SECURITY PROMPTING TO SYSTEM OPERATOR                         04350000
*                                                                       04360000
CTGBENT  DC    A(CATENTRY)                                              04370000
CTGBCAT  DC    A(CATACB)                                                04380000
CTGBWKA  DC    A(0)                                                     04390000
CTGBOPT2 DC    X'20000008'                                              04400000
CTGBDDNM DC    A(0)                                                     04410000
CTGBPSWD DC    A(PASSWORD)                                              04420000
CTGBFLDS DS    0F                                                       04430000
         DC    A(CTLGFLB1)                                              04440000
         DC    A(CTLGFLB2)                                              04450000
         DC    A(CTLGFLB3)                                              04460000
         DC    A(CTLGFLB4)                                              04470000
         DC    A(CTLGFLB5)                                              04480000
         DC    A(CTLGFLB6)                                              04490000
         DC    A(CTLGFLB7)                                              04500000
         DC    A(CTLGFLB8)                                              04510000
         SPACE                                                          04520000
CTGFLB   DS    0D                                                       04530000
CTLGFLB1 DC    X'01',AL3(0),F'0'                                        04540000
         DC    A(REQUES02)              ENTYPE    (LTH = 1)             04550000
         DC    3F'0'                                                    04560000
CTLGFLB2 DC    X'01',AL3(0),F'0'                                        04570000
         DC    A(REQUES03)              ENTNAME   (LTH = 44)            04580000
         DC    3F'0'                                                    04590000
CTLGFLB3 DC    X'01',AL3(0),F'0'                                        04600000
         DC    A(REQUES04)              CATTR     (LTH = 1)             04610000
         DC    3F'0'                                                    04620000
CTLGFLB4 DC    X'01',AL3(0),F'0'                                        04630000
         DC    A(REQUES05)              NAMEDS    (LTH = (4))           04640000
         DC    3F'0'                                                    04650000
CTLGFLB5 DC    X'01',AL3(0),F'0'                                        04660000
         DC    A(REQUES06)              DSETEXDT  (LTH = 3)             04670000
         DC    3F'0'                                                    04680000
CTLGFLB6 DC    X'01',AL3(0),F'0'                                        04690000
         DC    A(REQUES07)              DSETCRDT  (LTH = 3)             04700000
         DC    3F'0'                                                    04710000
CTLGFLB7 DC    X'01',AL3(0),F'0'                                        04720000
         DC    A(REQUES08)              OWNERID   (LTH = 8)             04730000
         DC    3F'0'                                                    04740000
CTLGFLB8 DC    X'01',AL3(0),F'0'                                        04750000
         DC    A(REQUES10)              CATVOL    (LTH = (15))          04760000
         DC    3F'0'                                                    04770000
         EJECT                                                          04780000
*        *---------------------------*                                  04790000
*        *   F I E L D   N A M E S   *                                  04800000
*        *---------------------------*                                  04810000
         SPACE                                                          04820000
REQUES01 DC    CL8'MULTITYP'                                            04830000
REQUES02 DC    CL8'ENTYPE'              LTH = 1                         04840000
REQUES03 DC    CL8'ENTNAME'             LTH = 44                        04850000
REQUES04 DC    CL8'CATTR'               LTH = 1                         04860000
REQUES05 DC    CL8'NAMEDS'              LTH = VL: (4) / ASSOC           04870000
REQUES06 DC    CL8'DSETEXDT'            LTH = 3                         04880000
REQUES07 DC    CL8'DSETCRDT'            LTH = 3                         04890000
REQUES08 DC    CL8'OWNERID'             LTH = 8                         04900000
REQUES09 DC    CL8'CRAVOL'              LTH = 6                         04910000
REQUES10 DC    CL8'CATVOL'              LTH = VL: (15) / VOL            04920000
         SPACE                                                          04930000
REQUES11 DC    CL8'VOLDVCHR'            LTH = 20                        04940000
REQUES12 DC    CL8'SPACPARM'            LTH = 7                         04950000
REQUES13 DC    CL8'HURBADS'             LTH = 4                         04960000
REQUES14 DC    CL8'HARBADS'             LTH = 4                         04970000
REQUES15 DC    CL8'ENTVOL'              LTH = VL: (45+) / VOL           04980000
REQUES18 DC    CL8'SYSEXTDS'            LTH = 1                         04990000
REQUES19 DC    CL8'NODSPACE'            LTH = 2                         05000000
REQUES20 DC    CL8'NODSET'              LTH = 2                         05010000
REQUES23 DC    CL8'DSPDSCRP'            LTH = VL: (13+SPACE MAP) / EXT  05020000
REQUES24 DC    CL8'PASSWORD'            LTH = 32                        05030000
         SPACE                                                          05040000
REQUES25 DC    CL8'AMDSBCAT'            LTH = 96                        05050000
REQUES28 DC    CL8'LRECL'               LTH = 4                         05060000
REQUES33 DC    CL8'CATACB'              LTH = 4                         05070000
         END                                                            05080000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD                             00210000
//LKED.SYSIN DD *                                                       00220000
  NAME VSAMLST2(R)                                                      00230000
//A        EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF'               00120000
//ASM.SYSIN DD *                                                        00130000
         PRINT OFF                                                      00010000
         MACRO                                                          00020000
&NAME    INIT &BASE=3,&REGS=Y,&PATCH=3,&RENT=N,&SAVE=Y                  00030000
         AIF   ((&BASE LT 13) AND (&BASE GT 1)).N020                    00040000
         MNOTE 12,'INVALID BASE REGISTER'                               00050000
         MEXIT                                                          00060000
.N020    ANOP                                                           00070000
         PUSH  PRINT                                                    00080000
         PRINT ON,GEN                                                   00090000
         EJECT                                                          00100000
&SYSECT  CSECT                                                          00110000
         USING *,15                                                     00120000
         B     BEGIN                                                    00130000
         DC    AL1(24)                                                  00140000
         DC    CL8'&SYSECT'                                             00150000
         DC    CL16'-&SYSDATE-&SYSTIME'                                 00160000
         AIF   ('&RENT' EQ 'Y').N004                                    00170000
MYSAVE   DC    18F'-1'                                                  00180000
.N004    ANOP                                                           00190000
         AIF   ('&PATCH' EQ '0').N005                                   00200000
PATCH    DC    &PATCH.CL8'*PATCH*'                                      00210000
.N005    ANOP                                                           00220000
         AIF   ('&REGS' EQ 'N').N030                                    00230000
         AIF   ('&REGS' EQ 'Y').N010                                    00240000
         MNOTE 4,'REGS OPERAND INVALID. Y SUBSTITUTED'                  00250000
.N010    ANOP                                                           00260000
R0       EQU   0                                                        00270000
R1       EQU   1                                                        00280000
R2       EQU   2                                                        00290000
R3       EQU   3                                                        00300000
R4       EQU   4                                                        00310000
R5       EQU   5                                                        00320000
R6       EQU   6                                                        00330000
R7       EQU   7                                                        00340000
R8       EQU   8                                                        00350000
R9       EQU   9                                                        00360000
R10      EQU   10                                                       00370000
R11      EQU   11                                                       00380000
R12      EQU   12                                                       00390000
R13      EQU   13                                                       00400000
R14      EQU   14                                                       00410000
R15      EQU   15                                                       00420000
.N030    ANOP                                                           00430000
BEGIN    DS   0H                                                        00440000
         STM   14,12,12(13)                                             00450000
         LR    &BASE,15                                                 00460000
         DROP  15                                                       00470000
         USING &SYSECT,&BASE                                            00480000
         AIF   ('&SAVE' EQ 'N').N003                                    00490000
         AIF   ('&RENT' EQ 'Y').N002                                    00500000
         AIF   ('&RENT' EQ 'N').N001                                    00510000
         MNOTE 4,'RENT OPERAND INVALID. N SUBSTITUTED'                  00520000
.N001    ANOP                                                           00530000
         ST    13,MYSAVE+4                                              00540000
         LR    15,13                                                    00550000
         LA    13,MYSAVE                                                00560000
         ST    13,8(15)                                                 00570000
         AGO   .N003                                                    00580000
.N002    ANOP                                                           00590000
         GETMAIN R,LV=72                                                00600000
         ST    13,4(1)                                                  00610000
         ST    1,8(13)                                                  00620000
         LR    13,1                                                     00630000
.N003    ANOP                                                           00640000
         POP   PRINT                                                    00650000
         EJECT                                                          00660000
         MEND                                                           00670000
         PRINT ON                                                       00680000
*  MODULE NAME:         VSAMLST3     (REL. 1.1  08/10/79)               00690000
*                                                                       00700000
*  MODULE DESCRIPTION:  VSAM CATALOG LIST UTILITY - SUMMARY REPORT      00710000
*                                                                       00720000
*  RETURN LINKAGE:      RETURN (14,12),RC=0                             00730000
*                                                                       00740000
*  LINKAGE TABLE:       NONE - SINGLE ENTRY                             00750000
*                                                                       00760000
*  PARAMETERS:          R1 POINTS TO A PARAMETER LIST ALIGNED ON A      00770000
*                       FULL WORD BOUNDARY.                             00780000
*                        WORD 1 - ADDRESS OF VSAM CATALOG NAME          00790000
*                        WORD 2 - ADDRESS OF CTLGPL-B                   00800000
*                        WORD 3 - ADDRESS OF MSGFILE DCB                00810000
*                        WORD 4 - ADDRESS OF RPTFIL1 DCB                00820000
*                        WORD 5 - ADDRESS OF WORK AREA 'B'              00830000
*                                                                       00840000
*  EXIT:                RC=00 - SUCCESSFUL                              00850000
*                                                                       00860000
*  MODULE FUNCTION:     NORMAL MODE - R0=00                             00870000
*                        FOR EACH VSAM ENTRY SPECIFIED IN THE PARAMETER 00880000
*                        LIST, THIS MODULE FORMATS IT FOR THE VSAM      00890000
*                        CATALOG SUMMARY REPORT.  FOR ENTRIES WITHOUT   00900000
*                        ANY VOLUME INFORMATIONS, IT WILL USE THEIR     00910000
*                        ASSOCIATED 'DATA' ENTRIES'.                    00920000
*                       INIT MODE - R0=04                               00930000
*                        FOR THE NEW VSAM CATALOG SPECIFIED IN THE      00940000
*                        PARAMETER LIST, THIS MODULE RESETS THE TITLES  00950000
*                        AND LINE COUNTS FOR THE NEXT REPORT.           00960000
*                                                                       00970000
*  CALLER:              VSAMLST2                                        00980000
*                                                                       00990000
*  CALLS:               NONE                                            01000000
*                                                                       01010000
*  SYSTEMS SERVICES:    PUT (QSAM), SVC 26                              01020000
*                                                                       01030000
*  MODULE ENVIRONMENT:  OS/VS1, OS/VS2                                  01040000
         TITLE 'VSAMLST3 --- VSAM CATALOG LIST UTILITY SUMMARY REPORT GX01050000
               ENERATOR (2.1)'                                          01060000
*        *-----------------------------------*                          01070000
*        *   R E G I S T E R   U S A G E S   *                          01080000
*        *-----------------------------------*                          01090000
*                                                                       01100000
*  R0  -  STANDARD LINKAGE                                              01110000
*  R1  -  STANDARD LINKAGE (ADDR OF PARM LIST) & TEMP WORK SPACE        01120000
*  R2  -  TEMP WORK SPACE                                               01130000
*  R3  -  TEMP WORK SPACE                                               01140000
*  R4  -  TEMP WORK SPACE                                               01150000
*  R5  -  TEMP WORK SPACE                                               01160000
*  R6  -  NOT USED                                                      01170000
*  R7  -  BASE REG FOR DSECT NMDSECT                                    01180000
*  R8  -  BASE REG FOR DSECT CTVOL                                      01190000
*  R9  -  BASE REG FOR DSECT CTLGPLB                                    01200000
*  R10 -  BASE REG FOR DSECT TXTSECT                                    01210000
*  R11 -  RETURN ADDR FOR SUBROUTINES                                   01220000
*  R12 -  BASE REG FOR CSECT VSAMLST3                                   01230000
*  R13 -  STANDARD LINKAGE (ADDR OF SAVE AREA)                          01240000
*  R14 -  STANDARD LINKAGE (ADDR TO RETURN)                             01250000
*  R15 -  STANDARD LINKAGE (ADDR OF ENTRY POINT & RETURN CODE)          01260000
         SPACE 3                                                        01270000
VSAMLST3 CSECT                                                          01280000
         INIT  BASE=12                                                  01290000
*********************************************************************** 01300000
*                                                                     * 01310000
*  2.1    SUMMARY REPORT GENERATOR                                    * 01320000
*                                                                     * 01330000
*     FUNCTION:  IF INIT MODE, SET TITLES AND RETURN.                 * 01340000
*                ELSE PROCESS ENTRY ACCORDING TO ITS TYPE.            * 01350000
*                                                                     * 01360000
*     ERRORS:  NONE                                                   * 01370000
*                                                                     * 01380000
*********************************************************************** 01390000
         SPACE 3                                                        01400000
VCL21    EQU   *                                                        01410000
         MVC   PARM9421(20),0(R1)       SAVE 5 FULL WORDS PARM LIST     01420000
         SPACE                                                          01430000
         L     R9,ACTPL21               ADDR OF CTLGPL-B                01440000
         USING CTLGPLB,R9                                               01450000
         MVC   CTGCCAT,CTGBCAT          ADDR OF CATACB                  01460000
         MVC   CTGCWKB,AWORK21          ADDR OF WORK AREA 'B'           01470000
         MVC   CTGCPSWD,CTGBPSWD        ADDR OF PASSWORD                01480000
         SPACE                                                          01490000
         LTR   R0,R0                    IS IT AN 'INIT' CALL?           01500000
         BNZ   INIT21                   YES - SET TITLES                01510000
         SPACE                                                          01520000
         LA    R10,LINE                 SET BASE REG FOR DSECT TXTSECT  01530000
         USING TXTSECT,R10                                              01540000
         SPACE                                                          01550000
         L     R1,CTGBAFL1              ADDR OF CTGFL-B1                01560000
         L     R1,20(,R1)               ADDR OF ENTRY TYPE              01570000
         MVC   ENTYPE,0(R1)             SAVE IT FOR CHECKING            01580000
         SR    R2,R2                    INDEX FOR BRANCH TABLE          01590000
         TRT   ENTYPE,RECTBL            SEE WHICH TYPE IT IS            01600000
         BZ    TYPE0                    TYPE 0 - UNDEFINED              01610000
         SPACE                                                          01620000
         SLL   R2,2                     DISPLACEMENT OF BRANCH TABLE    01630000
RECTYP   B     RECTYP(R2)                                               01640000
         B     TYPEA                    TYPE 5                          01650000
         B     TYPEB                    TYPE 4                          01660000
         B     TYPEC                    TYPE 3                          01670000
         B     TYPED                    TYPE 2                          01680000
         B     TYPEG                    TYPE 3                          01690000
         B     TYPEI                    TYPE 2                          01700000
         B     TYPER                    TYPE 4                          01710000
         B     TYPEU                    TYPE 5                          01720000
         B     TYPEV                    TYPE 1                          01730000
         B     TYPEX                    TYPE 6                          01740000
         EJECT                                                          01750000
*        *---------------------------------------*                      01760000
*        *  INIT MODE - SET TITLES AND HEADINGS  *                      01770000
*        *---------------------------------------*                      01780000
         SPACE                                                          01790000
INIT21   EQU   *                                                        01800000
         TIME  DEC                                                      01810000
         SPACE                                                          01820000
         ST    R0,WORD                  WORD = X'HHMMSSTH'              01830000
         UNPK  WORK,WORD                WORK = C'0HHMMSS*'  *=X'HT'     01840000
         MVC   TIME1(2),WORK+1          SET HOURS                       01850000
         MVI   TIME1+2,C':'                                             01860000
         MVC   TIME1+3(2),WORK+3        SET MINUTES                     01870000
         SPACE                                                          01880000
         ST    R1,WORK                  WORK = X'00YYDDDF'              01890000
         LA    R1,WORK+1                R1 = ADDR OF YYDDD              01900000
         BAL   R11,CNVDATE              CONVERT 'YYDDD' TO 'MM/DD/YY'   01910000
         MVC   DATE1,0(R1)                                              01920000
         SPACE 2                                                        01930000
         L     R1,ACATN21               SET VSAM CATALOG NAME           01940000
         MVC   CATNAME2,0(R1)                                           01950000
         MVC   CATYPE2,=CL6'USER'       SET VSAM CATALOG TYPE           01960000
         SPACE                                                          01970000
         L     R1,CTGBCAT                                               01980000
         L     R0,0(,R1)                ADDR OF THIS CATALOG'S ACB      01990000
         L     R1,16                    ADDR OF CVT                     02000000
         L     R1,256(,R1)              ADDR OF AMCBS                   02010000
         L     R1,8(,R1)                ADDR OF MASTER CATALOG'S ACB    02020000
         CLR   R0,R1                    IS THIS THE MASTER CATALOG?     02030000
         BNE   USERCTLG                 NO - MUST BE USER CATALOG       02040000
         MVC   CATYPE2,=CL6'MASTER'     YES - RESET IT                  02050000
         EJECT                                                          02060000
*        *---------------------*                                        02070000
*        *  INIT MODE - CONT.  *                                        02080000
*        *---------------------*                                        02090000
         SPACE                                                          02100000
USERCTLG EQU   *                                                        02110000
         LA    R1,CIZERO                ADDR OF CI NO. 0                02120000
         ST    R1,CTGCENT               SET CTLGPL-C                    02130000
         MVI   CTGCRCTP,C'D'            FOR 'DATA' ENTRY                02140000
         MVI   CTGCFLDN,X'03'           INCLUDE 'CATVOL' ALSO           02150000
         LA    R1,REQUES10              'CATVOL'                        02160000
         ST    R1,CTLGFLC3+8                                            02170000
         LA    R1,CTLGPLC                                               02180000
         SVC   26                                                       02190000
         SPACE                                                          02200000
         LTR   R15,R15                  IS SVC SUCCESSFUL?              02210000
         BZ    SVCOK211                 YES - CONTINUE                  02220000
         SPACE                                                          02230000
         ABEND 211,DUMP                                                 02240000
         SPACE                                                          02250000
SVCOK211 EQU   *                                                        02260000
         MVI   CTGCRCTP,X'00'           RESET TO ANY ENTRY              02270000
         MVI   CTGCFLDN,X'02'           AND 'ENTYPE' & 'ENTNAME' ONLY   02280000
         L     R8,CTLGFLC3+20           ADDR OF CATVOL                  02290000
         USING CTVOL,R8                                                 02300000
         SPACE                                                          02310000
         LA    R1,CTVDEVTP              ADDR OF 4-BYTES DEVICE TYPE     02320000
         BAL   R11,CNVDVTP              MAKE IT PRINTABLE               02330000
         MVC   DEVTYPE2,0(R1)           MOVE IT IN                      02340000
         MVC   VOLSER2,CTVOLSER         VOLUME SERIAL NUMBER            02350000
         SPACE                                                          02360000
         MVI   LINECNT+1,X'FF'          INDICATE NEW VSAM CATALOG       02370000
         B     GOHOME                   RETURN NOW                      02380000
         EJECT                                                          02390000
*        *----------------------*                                       02400000
*        *  TYPE 0 - UNDEFINED  *                                       02410000
*        *----------------------*                                       02420000
         SPACE                                                          02430000
TYPE0    EQU   *                                                        02440000
         L     R1,CTGBAFL2              ADDR OF CTLGFL-B2               02450000
         L     R1,20(,R1)               ADDR OF ENTRY NAME              02460000
         MVC   TXTENTRY,0(R1)                                           02470000
         MVI   TXTENTYP,C'('            TRANSLATE ENTRY TYPE INTO HEX   02480000
         UNPK  TXTENTYP+1(3),ENTYPE(2)                                  02490000
         TR    TXTENTYP+1(2),HEXTBL                                     02500000
         MVI   TXTENTYP+3,C')'                                          02510000
         BAL   R11,PUTRPT               PRINT IT AND                    02520000
         B     GOHOME                   RETURN NOW                      02530000
         SPACE 2                                                        02540000
*        *----------------*                                             02550000
*        *  TYPE 1 - 'V'  *                                             02560000
*        *----------------*                                             02570000
         SPACE                                                          02580000
TYPE1    EQU   *                                                        02590000
TYPEV    EQU   *                                                        02600000
         L     R1,CTGBAFL2              ADDR OF CTLGFL-B2               02610000
         L     R1,20(,R1)               ADDR OF ENTRY NAME              02620000
         MVC   TXTENTRY(6),0(R1)        ONLY THE FIRST 6 CHAR FOR 'V'   02630000
         MVC   TXTENTYP(5),=CL5'SPACE'  SET ENTRY TYPE                  02640000
         BAL   R11,PUTRPT               PRINT IT                        02650000
         B     GOHOME                   AND RETURN                      02660000
         EJECT                                                          02670000
*        *----------------------*                                       02680000
*        *  TYPE 2 - 'D' & 'I'  *                                       02690000
*        *----------------------*                                       02700000
         SPACE                                                          02710000
TYPED    EQU   *                                                        02720000
         MVC   TXTENTYP(4),=CL4'DATA'                                   02730000
         MVI   ENTYPE,X'02'                                             02740000
         B     TYPE2                                                    02750000
         SPACE                                                          02760000
TYPEI    EQU   *                                                        02770000
         MVC   TXTENTYP(5),=CL5'INDEX'                                  02780000
         MVI   ENTYPE,X'02'                                             02790000
         B     TYPE2                                                    02800000
         SPACE 2                                                        02810000
*        *----------------------*                                       02820000
*        *  TYPE 3 - 'C' & 'G'  *                                       02830000
*        *----------------------*                                       02840000
         SPACE                                                          02850000
TYPEC    EQU   *                                                        02860000
         MVC   TXTENTYP(7),=CL7'CLUSTER'                                02870000
         MVI   ENTYPE,X'03'                                             02880000
         BAL   R11,CHKCLSTR             CHECK IF PAGE SPACE OR CATALOG  02890000
         LTR   R15,R15                  IS IT A SPECIAL CLUSTER?        02900000
         BZ    TYPE2                    NO - PROCESS FOR CLUSTER        02910000
         MVI   ENTYPE,X'04'             YES - GET ASSOC ENTRIES ALSO    02920000
         B     TYPE2                                                    02930000
         SPACE                                                          02940000
TYPEG    EQU   *                                                        02950000
         MVC   TXTENTYP(15),=CL15'ALTERNATE INDEX'                      02960000
         MVI   ENTYPE,X'03'                                             02970000
         B     TYPE2                                                    02980000
         EJECT                                                          02990000
*        *----------------------*                                       03000000
*        *  TYPE 4 - 'B' & 'R'  *                                       03010000
*        *----------------------*                                       03020000
         SPACE                                                          03030000
TYPEB    EQU   *                                                        03040000
         MVC   TXTENTYP(3),=CL3'GDG'                                    03050000
         MVI   ENTYPE,X'04'                                             03060000
         B     TYPE2                                                    03070000
         SPACE                                                          03080000
TYPER    EQU   *                                                        03090000
         MVC   TXTENTYP(4),=CL4'PATH'                                   03100000
         MVI   ENTYPE,X'04'                                             03110000
         B     TYPE2                                                    03120000
         SPACE 2                                                        03130000
*        *----------------------*                                       03140000
*        *  TYPE 5 - 'A' & 'U'  *                                       03150000
*        *----------------------*                                       03160000
         SPACE                                                          03170000
TYPEA    EQU   *                                                        03180000
         MVC   TXTENTYP(8),=CL8'NON VSAM'                               03190000
         MVI   ENTYPE,X'05'                                             03200000
         B     TYPE2                                                    03210000
         SPACE                                                          03220000
TYPEU    EQU   *                                                        03230000
         MVC   TXTENTYP(12),=CL12'USER CATALOG'                         03240000
         MVI   ENTYPE,X'05'                                             03250000
         B     TYPE2                                                    03260000
         SPACE 2                                                        03270000
*        *----------------*                                             03280000
*        *  TYPE 6 - 'X'  *                                             03290000
*        *----------------*                                             03300000
         SPACE                                                          03310000
TYPEX    EQU   *                                                        03320000
         L     R1,CTGBAFL2              ADDR OF CTLGFL-B2               03330000
         L     R1,20(,R1)               ADDR OF ENTRY NAME              03340000
         MVC   TXTENTRY,0(R1)           SET ENTRY NAME                  03350000
         SPACE                                                          03360000
         MVC   TXTENTYP(5),=CL5'ALIAS'                                  03370000
         LA    R0,4                     ONLY THE FIRST ASSOC            03380000
         L     R1,CTGBAFL4              ADDR OF CTLGFL-B4 (NAMEDS)      03390000
         ST    R0,16(,R1)               CHANGE IT FROM 3 NAMEDS'ES TO 1 03400000
         MVI   ENTYPE,X'06'                                             03410000
         B     TYPE4                                                    03420000
         EJECT                                                          03430000
*        *---------------------------------------------------------*    03440000
*        *  COMMON FOR ALL TYPE 2 , TYPE 3, TYPE 4 AND TYPE 5      *    03450000
*        *  SET ENTRY NAME, OWNER ID, CREATION & EXPIRATION DATES  *    03460000
*        *---------------------------------------------------------*    03470000
         SPACE                                                          03480000
TYPE2    EQU   *                                                        03490000
         L     R1,CTGBAFL2              ADDR OF CTLGFL-B2               03500000
         L     R1,20(,R1)               ADDR OF ENTRY NAME              03510000
         MVC   TXTENTRY,0(R1)           SET ENTRY NAME                  03520000
         SPACE                                                          03530000
CHKEXPDT EQU   *                                                        03540000
         L     R1,CTGBAFL5              ADDR OF CTLGFL-B5               03550000
         L     R1,20(,R1)               ADDR OF EXPIRATION DATE         03560000
         CLI   0(R1),X'FF'              IS THERE ONE?                   03570000
         BE    CHKCREDT                 NO - SKIP MOVE                  03580000
         CLC   CIZERO(2),0(R1)          IS IT A ZERO DATE?              03590000
         BNE   CNVEXPDT                 NO - CONVERT EXPIRATION DATE    03600000
         TM    2(R1),X'F0'              IS IT A ZERO DATE?              03610000
         BZ    CHKCREDT                 YES - SKIP MOVE                 03620000
CNVEXPDT EQU   *                                                        03630000
         BAL   R11,CNVDATE              NO - CONVERT IT TO 'MM/DD/YY'   03640000
         MVC   TXTEXPDT,0(R1)           MOVE IT                         03650000
         SPACE                                                          03660000
CHKCREDT EQU   *                                                        03670000
         L     R1,CTGBAFL6              ADDR OF CTLGFL-B6               03680000
         L     R1,20(,R1)               ADDR OF CREATION DATE           03690000
         CLI   0(R1),X'FF'              IS THERE ONE?                   03700000
         BE    CHKOWNER                 NO - SKIP MOVE                  03710000
         BAL   R11,CNVDATE              YES - CONVERT IT TO 'MM/DD/YY'  03720000
         MVC   TXTCREDT,0(R1)           MOVE IT                         03730000
         SPACE                                                          03740000
CHKOWNER EQU   *                                                        03750000
         L     R1,CTGBAFL7              ADDR OF CTLGFL-B7               03760000
         L     R1,20(,R1)               ADDR OF OWNER ID                03770000
         CLI   0(R1),X'FF'              IS THERE ONE?                   03780000
         BE    CHKDEVOL                 NO - SKIP MOVE                  03790000
         MVC   TXTOWNER,0(R1)           MOVE IT                         03800000
         EJECT                                                          03810000
*        *------------------------------------------------*             03820000
*        *  FOR TYPE 2 AND TYPE 5 ONLY                    *             03830000
*        *  SET DEVICE TYPE, VOLUME SERIAL & VOLUME STAT  *             03840000
*        *------------------------------------------------*             03850000
         SPACE                                                          03860000
CHKDEVOL EQU   *                                                        03870000
         CLI   ENTYPE,X'03'             IS THIS A TYPE-3 ENTRY?         03880000
         BE    TYPE3                    YES - GET THE ASSOC RECORDS     03890000
         CLI   ENTYPE,X'04'             IS THIS A TYPE-4 ENTRY?         03900000
         BE    TYPE4                    YES - GET THE ASSOC RECORDS     03910000
         SPACE                                                          03920000
         L     R1,CTGBAFL8              ADDR OF CTLGFL-B8 (CATVOL)      03930000
         BAL   R11,SETVOLS              FORMATS AND PRINTS              03940000
         SPACE                                                          03950000
         CLI   ENTYPE,X'05'             IS THIS A TYPE-5 ENTRY?         03960000
         BE    TYPE5                    YES - GET THE ASSOC RECORDS     03970000
         B     GOHOME                   NO - THAT'S ALL FOR TYPE 2      03980000
         EJECT                                                          03990000
*        *-----------------------------------------------------------*  04000000
*        *  FOR TYPE 3 ONLY                                          *  04010000
*        *  SET THE ENTRY NAME(S) OF ALL ITS ASSOCIATED NON-'D' AND  *  04020000
*        *  NON-'I' RECORDS.  NO OTHER INFO'S ARE NEEDED.            *  04030000
*        *-----------------------------------------------------------*  04040000
         SPACE                                                          04050000
TYPE3    EQU   *                                                        04060000
         BAL   R11,PUTRPT               PRINT PRIMARY RECORDS           04070000
         SPACE                                                          04080000
         USING NMDSECT,R7                                               04090000
         L     R1,CTGBAFL4              ADDR OF CTLGFL-B4               04100000
         L     R0,16(,R1)               LENGTH OF NAMEDS(ES)            04110000
         L     R7,20(,R1)               ADDR OF NAMEDS(ES)              04120000
         AR    R0,R7                    ADDR OF END OF NAMEDS(ES)       04130000
         ST    R0,EONMDS                UPPER LIMIT                     04140000
         SPACE                                                          04150000
NEXTNMDS EQU   *                                                        04160000
         CLI   NMDSTYP,C'D'             IS IT FOR A TYPE 'D' ENTRY?     04170000
         BE    BUMPNMDS                 YES - BUMP TO NEXT 'NAMEDS' FLD 04180000
         CLI   NMDSTYP,C'I'             IS IT FOR A TYPE 'I' ENTRY?     04190000
         BE    BUMPNMDS                 YES - BUMP TO NEXT 'NAMEDS' FLD 04200000
         SPACE                                                          04210000
         LA    R1,NMDSCIN               ADDR OF CI NUMBER               04220000
         ST    R1,CTGCENT               SET CTLGPL-C                    04230000
         MVC   CTGCRCTP,NMDSTYP         SET REC TYPE IN CTGCOPT2        04240000
         LA    R1,CTLGPLC                                               04250000
         SVC   26                                                       04260000
         SPACE                                                          04270000
         LTR   R15,R15                  IS SVC SUCCESSFUL?              04280000
         BZ    SVCOK212                 YES - CONTINUE                  04290000
         SPACE                                                          04300000
         ABEND 212,DUMP                                                 04310000
         SPACE                                                          04320000
SVCOK212 EQU   *                                                        04330000
         BAL   R11,FMTASSOC             FORMAT ASSOC ENTRY AND          04340000
         BAL   R11,PUTRPT               PRINT                           04350000
         SPACE                                                          04360000
BUMPNMDS EQU   *                                                        04370000
         LA    R7,4(,R7)                ADD LENGTH OF NAMEDS            04380000
         C     R7,EONMDS                IS IT STILL WITHIN LIMIT?       04390000
         BL    NEXTNMDS                 YES - KEEP ON KEEPING ON        04400000
         MVI   CTGCRCTP,X'00'           RESET IT TO ANY ENTRY           04410000
         B     GOHOME                                                   04420000
         SPACE                                                          04430000
EONMDS   DC    F'-1'                                                    04440000
         EJECT                                                          04450000
*        *-----------------------------------------------------------*  04460000
*        *  FOR TYPE 4 AND TYPE 6 ONLY                               *  04470000
*        *  SET THE ENTRY NAME(S) OF ALL ITS ASSOCIATED RECORDS AND  *  04480000
*        *  NO OTHER INFO'S ARE NEEDED.                              *  04490000
*        *-----------------------------------------------------------*  04500000
         SPACE                                                          04510000
TYPE4    EQU   *                                                        04520000
         BAL   R11,PUTRPT               PRINT PRIMARY RECORDS           04530000
         SPACE                                                          04540000
         L     R1,CTGBAFL4              ADDR OF CTLGFL-B4               04550000
         L     R0,16(,R1)               LENGTH OF NAMEDS(ES)            04560000
         LTR   R0,R0                    IS THERE ANY?                   04570000
         BZ    GOHOME                   NO - THAT'S ALL THEN            04580000
         L     R7,20(,R1)               ADDR OF NAMEDS(ES)              04590000
         AR    R0,R7                    ADDR OF END OF NAMEDS(ES)       04600000
         ST    R0,EONMDS                UPPER LIMIT                     04610000
         SPACE                                                          04620000
NXTNMDS  EQU   *                                                        04630000
         LA    R1,NMDSCIN               ADDR OF CI NUMBER               04640000
         ST    R1,CTGCENT               SET CTLGPL-C                    04650000
         MVC   CTGCRCTP,NMDSTYP         SET REC TYPE IN CTGCOPT2        04660000
         LA    R1,CTLGPLC                                               04670000
         SVC   26                                                       04680000
         SPACE                                                          04690000
         LTR   R15,R15                  IS SVC SUCCESSFUL?              04700000
         BZ    SVCOK213                 YES - CONTINUE                  04710000
         SPACE                                                          04720000
         ABEND 213,DUMP                                                 04730000
         SPACE                                                          04740000
SVCOK213 EQU   *                                                        04750000
         BAL   R11,FMTASSOC             FORMAT ASSOC ENTRY AND          04760000
         BAL   R11,PUTRPT               PRINT                           04770000
         SPACE                                                          04780000
         LA    R7,4(,R7)                ADD LENGTH OF NAMEDS            04790000
         C     R7,EONMDS                IS IT STILL WITHIN LIMIT?       04800000
         BL    NXTNMDS                  YES - KEEP ON KEEPING ON        04810000
         MVI   CTGCRCTP,X'00'           RESET IT TO ANY ENTRY           04820000
         B     GOHOME                                                   04830000
         EJECT                                                          04840000
*        *-----------------------------------------------------------*  04850000
*        *  FOR TYPE 5 ONLY                                          *  04860000
*        *  SET THE ENTRY NAME(S) OF ALL ITS ASSOCIATED RECORDS AND  *  04870000
*        *  THE ALIAS CHAIN.                                         *  04880000
*        *-----------------------------------------------------------*  04890000
         SPACE                                                          04900000
TYPE5    EQU   *                                                        04910000
         L     R1,CTGBAFL4              ADDR OF CTLGFL-B4               04920000
         L     R0,16(,R1)               LENGTH OF NAMEDS(ES)            04930000
         LTR   R0,R0                    IS THERE ANY?                   04940000
         BZ    GOHOME                   NO - THAT'S ALL THEN            04950000
         L     R7,20(,R1)               ADDR OF NAMEDS(ES)              04960000
         AR    R0,R7                    ADDR OF END OF NAMEDS(ES)       04970000
         ST    R0,EONMDS                UPPER LIMIT                     04980000
         SPACE                                                          04990000
NXTNAMDS EQU   *                                                        05000000
         LA    R1,NMDSCIN               ADDR OF CI NUMBER               05010000
         ST    R1,CTGCENT               SET CTLGPL-C                    05020000
         CLI   NMDSTYP,C'X'             IS THIS AN 'ALIAS' ASSOC ENTRY? 05030000
         BE    XCHAIN                   YES - MUST BE LAST ENTRY        05040000
         MVC   CTGCRCTP,NMDSTYP         SET REC TYPE IN CTGCOPT2        05050000
         LA    R1,CTLGPLC                                               05060000
         SVC   26                                                       05070000
         SPACE                                                          05080000
         LTR   R15,R15                  IS SVC SUCCESSFUL?              05090000
         BZ    SVCOK214                 YES - CONTINUE                  05100000
         SPACE                                                          05110000
         ABEND 214,DUMP                                                 05120000
         SPACE                                                          05130000
SVCOK214 EQU   *                                                        05140000
         BAL   R11,FMTASSOC             FORMAT ASSOC ENTRY AND          05150000
         BAL   R11,PUTRPT               PRINT                           05160000
         SPACE                                                          05170000
         LA    R7,4(,R7)                ADD LENGTH OF NAMEDS            05180000
         C     R7,EONMDS                IS IT STILL WITHIN LIMIT?       05190000
         BL    NXTNMDS                  YES - KEEP ON KEEPING ON        05200000
         MVI   CTGCRCTP,X'00'           RESET IT TO ANY ENTRY           05210000
         B     GOHOME                                                   05220000
         SPACE                                                          05230000
         DROP  R7                                                       05240000
         EJECT                                                          05250000
*        *------------------------------------------------------------* 05260000
*        *  PRINT THE ALIAS CHAIN - EACH TYPE 'X' ENTRY HAS FOLLOWING * 05270000
*        *  THREE (3) ASSOC 'NAMEDS' ENTRIES:                         * 05280000
*        *    1.  ASSOC TYPE 'A' OR 'U'                               * 05290000
*        *    2.  NEXT  TYPE 'X' ON THE CHAIN OR ZERO IF END OF CHAIN * 05300000
*        *    3.  LAST  TYPE 'X' ON THE CHAIN OR ZERO IF FIRST ONE    * 05310000
*        *------------------------------------------------------------* 05320000
         SPACE                                                          05330000
XCHAIN   EQU   *                                                        05340000
         MVI   CTGCRCTP,C'X'            SET CTLGPL-C                    05350000
         MVI   CTGCFLDN,X'03'           INCLUDE CTLGFLC3                05360000
         LA    R1,REQUES05              'NAMEDS'                        05370000
         ST    R1,CTLGFLC3+8                                            05380000
         SPACE                                                          05390000
NEXTTYPX EQU   *                                                        05400000
         LA    R1,CTLGPLC                                               05410000
         SVC   26                                                       05420000
         SPACE                                                          05430000
         LTR   R15,R15                  IS SVC SUCCESSFUL?              05440000
         BZ    SVCOK215                 YES - CONTINUE                  05450000
         SPACE                                                          05460000
         ABEND 215,DUMP                                                 05470000
         SPACE                                                          05480000
SVCOK215 EQU   *                                                        05490000
         BAL   R11,FMTASSOC             FORMAT ASSOC ENTRY AND          05500000
         BAL   R11,PUTRPT               PRINT                           05510000
         SPACE                                                          05520000
         L     R1,CTLGFLC3+20           ADDR OF 'NAMEDS' FLD'S          05530000
         CLC   CIZERO,5(R1)             IS IT THE END OF ALIAS CHAIN?   05540000
         BE    EOXCHAIN                 YES - CLEAN UP AND GO HOME      05550000
         LA    R1,5(,R1)                ADDR OF CI NO. OF NEXT ALIAS    05560000
         ST    R1,CTGCENT               SET CTLGPL-C                    05570000
         B     NEXTTYPX                 AND GO GET IT                   05580000
         SPACE                                                          05590000
EOXCHAIN EQU   *                                                        05600000
         MVI   CTGCRCTP,X'00'           RESET IT TO ANY ENTRY TYPE      05610000
         MVI   CTGCFLDN,X'02'           'ENTYPE' & 'ENTNAME' ONLY       05620000
         B     GOHOME                   AND RETURN                      05630000
         EJECT                                                          05640000
*        ***************                                                05650000
*        *             *                                                05660000
*        *   E X I T   *                                                05670000
*        *             *                                                05680000
*        ***************                                                05690000
         SPACE                                                          05700000
GOHOME   EQU   *                                                        05710000
         L     R13,MYSAVE+4                                             05720000
         RETURN (14,12),RC=0                                            05730000
         SPACE 2                                                        05740000
         LTORG                                                          05750000
         SPACE                                                          05760000
         DS    0H                                                       05770000
         EJECT                                                          05780000
*        *------------------------------------------------*             05790000
*        *  CHECK CLUSTER TO SEE IF CATALOG OR PAGESPACE  *             05800000
*        *  IN:     CTGBAFL3, CTGBAFL4                    *             05810000
*        *  OUT:    TXTENTYP IF SPECIAL CLUSTER           *             05820000
*        *          R15=04 IF SPECIAL CLUSTER             *             05830000
*        *------------------------------------------------*             05840000
         SPACE                                                          05850000
CHKCLSTR EQU   *                                                        05860000
         SR    R15,R15                  CLEAR RETURN CODE               05870000
         L     R1,CTGBAFL3              ADDR OF CTLGFL-B3               05880000
         L     R1,20(,R1)               ADDR OF 'CATTR' FIELD           05890000
         CLI   0(R1),X'00'              IS THERE A CLUSTER ATTRIBUTE?   05900000
         BE    NOCATTR                  NO - GO CHECK IF CATALOG        05910000
         SPACE                                                          05920000
         LA    R15,4                    MARK AS SPECIAL CLUSTER         05930000
         TM    0(R1),X'02'              IS IT A SWAP SPACE?             05940000
         BZ    NOTSWAP                  NO - NEXT TEST                  05950000
         MVC   TXTENTYP+8(7),=CL7'/ SWSPC'                              05960000
         BR    R11                      RETURN 1                        05970000
         SPACE                                                          05980000
NOTSWAP  EQU   *                                                        05990000
         TM    0(R1),X'01'              IS IT A PAGE SPACE?             06000000
         BZ    NOTPAGE                  NO - TRANSLATE IT IN HEX        06010000
         MVC   TXTENTYP+8(7),=CL7'/ PGSPC'                              06020000
         BR    R11                      RETURN 2                        06030000
         SPACE                                                          06040000
NOTPAGE  EQU   *                                                        06050000
         MVC   TXTENTYP+8(7),=CL7'/ AT=XX'                              06060000
         UNPK  TXTENTYP+13(3),0(2,R1)                                   06070000
         TR    TXTENTYP+13(2),HEXTBL                                    06080000
         MVI   TXTENTYP+15,X'40'                                        06090000
         BR    R11                      RETURN 3                        06100000
         SPACE                                                          06110000
NOCATTR  EQU   *                                                        06120000
         L     R1,CTGBAFL4              ADDR OF CTLGFL-B4               06130000
         L     R1,20(,R1)               ADDR OF 'NAMEDS' FIELDS         06140000
         CLC   CIZERO,1(R1)             IS THIS ENTRY A CATALOG NAME?   06150000
         BNER  R11                      NO - RETURN 4 - NORMAL          06160000
         MVC   TXTENTYP+8(7),=CL7'/ CATLG'                              06170000
         LA    R15,4                    MARK AS SPECIAL CLUSTER         06180000
         BR    R11                      RETURN 5                        06190000
         SPACE 2                                                        06200000
         LTORG                                                          06210000
         SPACE                                                          06220000
         DS    0H                                                       06230000
         EJECT                                                          06240000
*        *--------------------------------------*                       06250000
*        *  FORMAT ASSOC ENTRIES FROM CTLGPL-C  *                       06260000
*        *  IN:     CTLGFLC1, CTLGFLC2          *                       06270000
*        *  OUT:    TXTENTRY, TXTENTYP          *                       06280000
*        *--------------------------------------*                       06290000
         SPACE                                                          06300000
FMTASSOC EQU   *                                                        06310000
         L     R1,CTLGFLC2+20           ADDR OF 'ENTNAME'               06320000
         MVI   TXTENTRY+1,C'-'                                          06330000
         MVC   TXTENTRY+2,0(R1)                                         06340000
         SPACE                                                          06350000
         L     R1,CTLGFLC1+20           ADDR OF 'ENTYPE'                06360000
         SR    R2,R2                    INDEX FOR ENTRY TYPE TABLE      06370000
         TRT   0(1,R1),RECTBL           SEE WHICH TYPE IT IS            06380000
         BZ    BADENTP                  TYPE 0 - UNDEFINED              06390000
         SPACE                                                          06400000
         BCTR  R2,0                     LESS ONE                        06410000
         SLL   R2,4                     TIMES SIXTEEN                   06420000
         LA    R1,ENTTBL(R2)            ADDR OF ENTRY TYPE              06430000
         MVC   TXTENTYP,1(R1)                                           06440000
         BR    R11                      RETURN 1                        06450000
         SPACE                                                          06460000
BADENTP  EQU   *                                                        06470000
         MVI   TXTENTYP,C'('            TRANSLATE ENTRY TYPE INTO HEX   06480000
         UNPK  TXTENTYP+1(3),ENTYPE(2)                                  06490000
         TR    TXTENTYP+1(2),HEXTBL                                     06500000
         MVI   TXTENTYP+3,C')'                                          06510000
         BR    R11                      RETURN 2                        06520000
         EJECT                                                          06530000
*        *-----------------------------------------------------------*  06540000
*        *  VOLUME INFO'S PROCESSING ROUTINE - FORMATS & PRINTS      *  06550000
*        *  IN:     R1 = ADDR OF 'CATVOL' CTLGFL                     *  06560000
*        *  OUT:    NONE                                             *  06570000
*        *  CALLS:  CNVDVTP, PUTRPT                                  *  06580000
*        *-----------------------------------------------------------*  06590000
         SPACE                                                          06600000
SETVOLS  EQU   *                                                        06610000
         ST    R11,SAVER11              SAVE RETURN ADDR                06620000
         L     R0,16(,R1)               LENGTH OF CATVOL(S)             06630000
         L     R8,20(,R1)               ADDR OF CATVOL(S)               06640000
         AR    R0,R8                    ADDR OF END OF CATVOL(S)        06650000
         ST    R0,EOCTVL                UPPER LIMIT                     06660000
         SPACE                                                          06670000
NEXTVOL  EQU   *                                                        06680000
         LA    R1,CTVDEVTP              ADDR OF DEVICE CODE             06690000
         BAL   R11,CNVDVTP              CONVERT TO PRINTABLE FORM       06700000
         MVC   TXTDEVTP,0(R1)           MOVE IT IN                      06710000
         MVC   TXTVOLSR,CTVOLSER        VOLUME SERIAL NUMBER            06720000
         SPACE                                                          06730000
TSTPRIME EQU   *                                                        06740000
         TM    CTVOLFLG,X'80'           IS IT PRIME?                    06750000
         BZ    TSTCAND                  NO - NEXT TEST                  06760000
         MVC   TXTSTATS,=CL5'PRIME'                                     06770000
         B     PRTLN2                   PRINT IT                        06780000
         SPACE                                                          06790000
TSTCAND  EQU   *                                                        06800000
         TM    CTVOLFLG,X'40'           IS IT CANDIDATE?                06810000
         BZ    TSTOVFL                  NO - NEXT TEST                  06820000
         MVC   TXTSTATS(4),=CL4'CAND'                                   06830000
         B     PRTLN2                   PRINT IT                        06840000
         SPACE                                                          06850000
TSTOVFL  EQU   *                                                        06860000
         TM    CTVOLFLG,X'20'           IS IT OVERFLOW?                 06870000
         BZ    BADVFLG                  NO - BAD VOLUME FLAG            06880000
         MVC   TXTSTATS(4),=CL4'OVFL'                                   06890000
         B     PRTLN2                   PRINT IT                        06900000
         EJECT                                                          06910000
*        *---------------------------*                                  06920000
*        *  CALL PUTRPT TO PRINT IT  *                                  06930000
*        *---------------------------*                                  06940000
         SPACE                                                          06950000
BADVFLG  EQU   *                                                        06960000
         MVI   TXTSTATS,C'('            TRANSLATE IT INTO HEX           06970000
         UNPK  TXTSTATS+1(3),CTVOLFLG(2)                                06980000
         TR    TXTSTATS+1(2),HEXTBL                                     06990000
         MVI   TXTSTATS+3,C')'                                          07000000
         SPACE                                                          07010000
PRTLN2   EQU   *                                                        07020000
         BAL   R11,PUTRPT               PRINT THS LINE NOW              07030000
         LA    R8,15(,R8)               BUMP TO NEXT CATVOL             07040000
         C     R8,EOCTVL                IS IT STILL WITHIN LIMIT?       07050000
         BL    NEXTVOL                  YES - KEEP ON KEEPING ON        07060000
         SPACE                                                          07070000
         L     R11,SAVER11              RESTORE RETURN ADDR             07080000
         BR    R11                      RETURN                          07090000
         SPACE 2                                                        07100000
SAVER11  DC    F'-1'                                                    07110000
EOCTVL   DC    F'-1'                                                    07120000
         SPACE                                                          07130000
         LTORG                                                          07140000
         SPACE                                                          07150000
         DS    0H                                                       07160000
         EJECT                                                          07170000
*        *-------------------------------------*                        07180000
*        *  PRINT ROUTINE FOR THE REPORT FILE  *                        07190000
*        *-------------------------------------*                        07200000
         SPACE                                                          07210000
PUTRPT   EQU   *                                                        07220000
         LH    R1,LINECNT               CURRENT NUMBER OF LINES ON PAGE 07230000
         CH    R1,LINEMAX               IS IT OVER THE LIMIT?           07240000
         BL    SAMEPAGE                 NO - IT'S STILL THE SAME PAGE   07250000
         SPACE 2                                                        07260000
         AP    PAGECNT,=P'1'            YES - TIME FOR A NEW PAGE       07270000
         MVC   PAGE1,PAT3A                                              07280000
         ED    PAGE1,PAGECNT            SET NEW PAGE NUMBER             07290000
         L     R1,ARPTF21                                               07300000
         PUT   (1),TITLE1               PRINT TITLE 1                   07310000
         SPACE                                                          07320000
         L     R1,ARPTF21                                               07330000
         PUT   (1),TITLE2               PRINT TITLE 2                   07340000
         SPACE                                                          07350000
         L     R1,ARPTF21                                               07360000
         PUT   (1),HEADING1             PRINT HEADING                   07370000
         SPACE                                                          07380000
         L     R1,ARPTF21                                               07390000
         PUT   (1),HEADING2             PRINT UNDERSCORE                07400000
         SPACE                                                          07410000
         MVI   CNTL,C'0'                SKIP 2 LINES FOR THE NEXT LINE  07420000
         LA    R1,7                     WE HAVE PRINTED 7 LINES ALREADY 07430000
         SPACE 2                                                        07440000
SAMEPAGE EQU   *                                                        07450000
         LA    R1,1(,R1)                                                07460000
         STH   R1,LINECNT               BUMP LINE COUNT                 07470000
         L     R1,ARPTF21                                               07480000
         PUT   (1),OAREA                                                07490000
         SPACE                                                          07500000
         MVI   CNTL,X'40'                                               07510000
         MVC   LINE,CNTL                                                07520000
         BR    R11                                                      07530000
         EJECT                                                          07540000
LINECNT  DC    H'0'                                                     07550000
LINEMAX  DC    H'60'                                                    07560000
PAGECNT  DC    PL2'0'                                                   07570000
PAT3A    DC    XL4'40202120'                                            07580000
         SPACE                                                          07590000
OAREA    DS    0CL133                                                   07600000
CNTL     DC    X'40'                                                    07610000
LINE     DC    CL132' '                                                 07620000
         SPACE                                                          07630000
         LTORG                                                          07640000
         EJECT                                                          07650000
*        *-------------------------------------------*                  07660000
*        *   T I T L E S   A N D   H E A D I N G S   *                  07670000
*        *-------------------------------------------*                  07680000
         SPACE                                                          07690000
TITLE1   DS    0CL133                                                   07700000
         DC    C'1',CL14'             ',CL16'              '            07710000
         DC    CL24'PGM=VSAMLIST',CL43'VSAM CATALOG SUMMARY'            07720000
         DC    CL5'DATE '                                               07730000
DATE1    DC    CL8'MM/DD/YY'                                            07740000
         DC    CL7'  TIME'                                              07750000
TIME1    DC    CL5'HH:MM'                                               07760000
         DC    CL6'  PAGE'                                              07770000
PAGE1    DC    CL4' 000'                                                07780000
         SPACE                                                          07790000
TITLE2   DS    0CL133                                                   07800000
         DC    C'0',CL29' CATALOG INFORMATION:  NAME='                  07810000
CATNAME2 DC    CL44'VSAM.CATALOG.NAME'                                  07820000
         DC    CL8'  TYPE= '                                            07830000
CATYPE2  DC    CL6'CATYPE'                                              07840000
         DC    CL11'  DEVTYPE= '                                        07850000
DEVTYPE2 DC    CL8'3330-1'                                              07860000
         DC    CL9' VOLSER= '                                           07870000
VOLSER2  DC    CL6'123456'                                              07880000
         DC    CL11' '                                                  07890000
         SPACE                                                          07900000
HEADING1 DS    0CL133                                                   07910000
         DC    C'-'                                                     07920000
         DC    C'    V S A M   E N T R Y   N A M E                   '  07930000
         DC    C'  ENTRY  TYPE    OWNER ID  CRE DATE  EXP DATE  '       07940000
         DC    C'DEVTYPE  VOLSER  STAT            '                     07950000
         SPACE                                                          07960000
HEADING2 DS    0CL133                                                   07970000
         DC    C'+'                                                     07980000
         DC    4C' ',44C'_',4C' ',15C'_',2C' ',8C'_',2C' ',8C'_'        07990000
         DC    2C' ',8C'_',2C' ',7C'_',2C' ',6C'_',2C' ',5C'_',11C' '   08000000
         SPACE                                                          08010000
         DS    0H                                                       08020000
         EJECT                                                          08030000
*        *-----------------------------------------------------------*  08040000
*        *  DATE CONVERSION ROUTINE FOR CREATION & EXPIRATION DATES  *  08050000
*        *  IN:  R1 = ADDR OF PACKED DATE (XL3'YYDDDC')              *  08060000
*        *  OUT: R1 = ADDR OF CHAR DATE (C'MM/DD/YY' OR C'YY.DDD  ') *  08070000
*        *-----------------------------------------------------------*  08080000
         SPACE                                                          08090000
CNVDATE  EQU   *                                                        08100000
         MVC   YEAR(3),0(R1)            SAVE PARM XL3'YYDDDC'           08110000
         OI    DAYS+1,X'0F'                                             08120000
         UNPK  YYDDD,WORD                                               08130000
         MVC   MMDDYY+6(2),YYDDD        SET YEAR                        08140000
         SPACE                                                          08150000
         XC    WORK,WORK                WORK = X'00000000 00000000'     08160000
         MVO   WORK+6(2),YEAR           WORK = X'00000000 00000YY0'     08170000
         OI    WORK+7,X'0F'             WORK = X'00000000 00000YYF'     08180000
         CVB   R4,WORK                                                  08190000
         STC   R4,YEAR                  YEAR IN BINARY                  08200000
         MVC   WORK+6(2),DAYS           WORK = X'00000000 0000DDDF'     08210000
         CVB   R4,WORK                                                  08220000
         LTR   R4,R4                    IS IT ZERO DAYS?                08230000
         BZ    ZERODATE                 YES - SET 00/00/YY              08240000
         STH   R4,DAYS                  DAYS IN BINARY                  08250000
         SPACE                                                          08260000
         LA    R3,2                     ASSUME LEAP YEAR                08270000
         TM    YEAR,X'03'               IS YEAR A MULTIPLE OF 4?        08280000
         BZ    LEAPYEAR                 YES - THIS IS A LEAP YEAR       08290000
         LA    R3,2(,R3)                NO - THIS IS NOT A LEAP YEAR    08300000
LEAPYEAR EQU   *                                                        08310000
         LA    R2,DTCVNTAB              ADDR OF DATE CONVERSION TABLE   08320000
         LA    R5,12                    NUMBER OF MONTHS                08330000
CMPDAYS  EQU   *                                                        08340000
         CH    R4,6(R3,R2)              NUMBER OF DAYS UP TO NEXT MONTH 08350000
         BNH   WHICHDAY                 THIS IS THE MONTH               08360000
         LA    R2,6(,R2)                BUMP TO NEXT ENTRY              08370000
         BCT   R5,CMPDAYS               NEXT MONTH, PLEASE              08380000
         EJECT                                                          08390000
*        *-----------------------*                                      08400000
*        *  SET DATE AND RETURN  *                                      08410000
*        *-----------------------*                                      08420000
         SPACE                                                          08430000
BADYYDDD EQU   *                                                        08440000
         MVC   BADDATE(2),YYDDD         BADDATE = C'YY.***  '           08450000
         MVC   BADDATE+3(3),YYDDD+2     BADDATE = C'YY.DDD  '           08460000
         LA    R1,BADDATE                                               08470000
         BR    R11                                                      08480000
         SPACE                                                          08490000
ZERODATE EQU   *                                                        08500000
         MVC   MMDDYY(5),MMDD0          MMDDYY = C'00/00/YY'            08510000
         B     DATESET                  THOUGH INPUT'S ALL ZERO         08520000
         SPACE                                                          08530000
WHICHDAY EQU   *                                                        08540000
         SH    R4,0(R3,R2)                                              08550000
         CVD   R4,WORK                                                  08560000
         OI    WORK+7,X'0F'                                             08570000
         UNPK  MMDDYY+3(2),WORK+6(2)    SET DAY                         08580000
         MVC   MMDDYY(2),0(R2)          SET MONTH                       08590000
         SPACE                                                          08600000
DATESET  EQU   *                                                        08610000
         LA    R1,MMDDYY                                                08620000
         BR    R11                      RETURN                          08630000
         SPACE 2                                                        08640000
WORK     DC    D'0'                                                     08650000
WORD     DC    F'0'                                                     08660000
         ORG   WORD+1                                                   08670000
YEAR     DS    XL1                                                      08680000
DAYS     DS    H                                                        08690000
MMDDYY   DC    C'MM/DD/YY'                                              08700000
BADDATE  DC    C'YY.DDD  '                                              08710000
YYDDD    DC    C'YYDDD'                                                 08720000
MMDD0    DC    C'00/00'                                                 08730000
         SPACE                                                          08740000
DTCVNTAB DS    0H                                                       08750000
         DC    C'01',H'0',H'0'                                          08760000
         DC    C'02',H'31',H'31'                                        08770000
         DC    C'03',H'60',H'59'                                        08780000
         DC    C'04',H'91',H'90'                                        08790000
         DC    C'05',H'121',H'120'                                      08800000
         DC    C'06',H'152',H'151'                                      08810000
         DC    C'07',H'182',H'181'                                      08820000
         DC    C'08',H'213',H'212'                                      08830000
         DC    C'09',H'244',H'243'                                      08840000
         DC    C'10',H'274',H'273'                                      08850000
         DC    C'11',H'305',H'304'                                      08860000
         DC    C'12',H'335',H'334'                                      08870000
TOTDAYS  DC    C'13',H'366',H'365'                                      08880000
         EJECT                                                          08890000
*        *----------------------------------------------------*         08900000
*        *  DEVICE TYPE LOOK-UP ROUTINE                       *         08910000
*        *  IN:  R1 = ADDR OF 4-BYTES DEVICE TYPE             *         08920000
*        *  OUT: R1 = ADDR OF 8 BYTES OF DEVICE TYPE IN CHAR  *         08930000
*        *----------------------------------------------------*         08940000
         SPACE                                                          08950000
CNVDVTP  EQU   *                                                        08960000
         MVC   DEVTYPE,0(R1)            SAVE INPUT                      08970000
         MVI   DEVTYPC,X'40'            CLEAR DEVICE TYPE               08980000
         MVC   DEVTYPC+1(L'DEVTYPC-1),DEVTYPC                           08990000
         SPACE                                                          09000000
         CLI   DEVTYPE+2,X'20'          IS IT A DASD?                   09010000
         BE    CNVDASD                  YES - CONVERT IT                09020000
         CLI   DEVTYPE+2,X'80'          IS IT A MAGNETIC TAPE DEVICS?   09030000
         BE    CNVMTD                   YES - CONVERT IT                09040000
         SPACE                                                          09050000
BADDVTP  EQU   *                                                        09060000
         UNPK  DEVTYPC(9),DEVTYPE(5)    TRANSLATE INTO HEX              09070000
         TR    DEVTYPC,HEXTBL                                           09080000
         LA    R1,DEVTYPC               ADDR OF DEV TYPE (8 BYTES)      09090000
         BR    R11                      RETURN 1                        09100000
         SPACE 2                                                        09110000
DEVTYPC  DC    CL8'DEVTYPC'                                             09120000
         DC    XL2'00'                  BUFFER ZONE FOR 'UNPK'          09130000
DEVTYPE  DC    XL4'00'                                                  09140000
         SPACE 2                                                        09150000
CNVDASD  EQU   *                                                        09160000
         CLI   DEVTYPE+3,X'00'          IS IT AN ESOTERIC?              09170000
         BE    CNVESTR                  YES - CONVERT IT                09180000
         SR    R2,R2                    CLEAR INDEX FOR DASD TYPE TABLE 09190000
         IC    R2,DEVTYPE+3             GET DEVICE CODE                 09200000
         SLL   R2,3                     MULTIPLY BY 8 FOR DISPLACEMENT  09210000
         LA    R2,TYPEDASD(R2)          R2 = ADDR OF DASD TYPE ENTRY    09220000
         LA    R1,TYPEDEND              R1 = ADDR OF END OF TABLE       09230000
         CR    R1,R2                    IS THIS A VALID ENTRY?          09240000
         BNH   BADDVTP                  NO - GO TRANSLATE IT            09250000
         CLI   0(R2),X'40'              IS THIS A VALID ENTRY?          09260000
         BE    BADDVTP                  NO - GO TRANSLATE IT            09270000
         MVC   DEVTYPC(7),1(R2)         ONLY 7 BYTES FROM TABLE         09280000
         LA    R1,DEVTYPC               ADDR OF DEV TYPE (8 BYTES)      09290000
         BR    R11                      RETURN 2                        09300000
         EJECT                                                          09310000
CNVMTD   EQU   *                                                        09320000
         CLI   DEVTYPE+3,X'00'          IS IT AN ESOTERIC?              09330000
         BE    CNVESTR                  YES - CONVERT IT                09340000
         LA    R1,TYPEMTD               R1 = ADDR OF MTD TYPE TABLE     09350000
         LA    R2,NUMOMTD               R2 = NO. OF ENTRIES OF TABLE    09360000
CMPMTD   EQU   *                                                        09370000
         CLC   DEVTYPE,0(R1)            IS IT IN THE TABLE?             09380000
         BE    MOVEMTD                  YES - FOUND ONE                 09390000
         LA    R1,12(,R1)               NO - BUMP TO NEXT ONE           09400000
         BCT   R2,CMPMTD                KEEP TRYING UNTIL WE RUN OUT    09410000
         B     BADDVTP                  THEN JUST TRANSLATE IT IN HEX   09420000
MOVEMTD  EQU   *                                                        09430000
         LA    R1,4(,R1)                BUMP TO MAGNETIC TAPE DEV TYPE  09440000
         BR    R11                      RETURN 3                        09450000
         SPACE 2                                                        09460000
CNVESTR  EQU   *                                                        09470000
         CLI   DEVTYPE+1,X'00'          IS THIS A VALID INDEX?          09480000
         BE    BADDVTP                  NO - JUST TRANSLATE IT IN HEX   09490000
         SR    R2,R2                    CLEAR INDEX FOR ESOTERICS TABLE 09500000
         IC    R2,DEVTYPE+1             GET ESOTERICS INDEX             09510000
         SLL   R2,3                     MULTIPLY BY 8 FOR DISPLACEMENT  09520000
         LA    R2,TYPESTRC(R2)          R2 = ADDR OF ESOTERICS ENTRY    09530000
         LA    R1,TYPESEND              R1 = ADDR OF END OF TABLE       09540000
         CR    R1,R2                    IS THIS A VALID ENTRY?          09550000
         BNH   BADDVTP                  NO - GO TRANSLATE IT            09560000
         MVC   DEVTYPC(7),1(R2)         ONLY 7 BYTES FROM TABLE         09570000
         LA    R1,DEVTYPC               ADDR OF DEV TYPE (8 BYTES)      09580000
         BR    R11                      RETURN 4                        09590000
         EJECT                                                          09600000
*        *------------------------------------------------------------* 09610000
*        *  THIS TABLE IS INDEXED BY DASD CLASS OF UCBTYP FIELD OF    * 09620000
*        *  UCB AS DESCRIBED IN OS/VS1 SYSTEM DATA AREAS (SY28-0605). * 09630000
*        *  EACH ENTRY IS 8 BYTES LONG WITH THE FOLLOWING,            * 09640000
*        *    BYTE 0   :  DASD CLASS IN HEXADECIMAL FORMAT            * 09650000
*        *    BYTE 1-7 :  DASD TYPE IN CHARACTER FORMAT -             * 09660000
*        *                A BLANK FIRST BYTE IMPLIES ENTRY NOT USED   * 09670000
*        *------------------------------------------------------------* 09680000
         SPACE                                                          09690000
TYPEDASD DS    0F                                                       09700000
         DC    X'00',CL7' '             * * *   NO SUCH DEVICE   * * *  09710000
         DC    X'01',CL7'2311'          2311 DISK STORAGE DRIVE         09720000
         DC    X'02',CL7'2301'          2301 PARALLEL DRUM              09730000
         DC    X'03',CL7'2303'          2303 SERIAL DRUM                09740000
         DC    X'04',CL7'2302'          2302 DISK STORAGE               09750000
         DC    X'05',CL7'2321'          2321 DATA CELL DRIVE            09760000
         DC    X'06',CL7'2305-1'        2305 FIXED HEAD STORAGE MODEL 1 09770000
         DC    X'07',CL7'2305-2'        2305 FIXED HEAD STORAGE MODEL 2 09780000
         DC    X'08',CL7'2314'          2314/2319 DIRECT ACCESS STORAGE 09790000
         DC    X'09',CL7'3330'          3330/3333 MODEL 1 DISK STORAGE  09800000
         DC    X'0A',CL7'3340'          3340 DISK STORAGE               09810000
         DC    X'0B',CL7'3350'          3350 DIRECT ACCESS STORAGE      09820000
         DC    X'0C',CL7'3375'          3375 DISK STORAGE         *JLM* 09830000
         DC    X'0D',CL7'3330-1'        3330/3333 MODEL 11 DISK STORAGE 09840000
         DC    X'0E',CL7'3380'          3380 DISK STORAGE         *JLM* 09830000
         DC    X'0F',CL7'3390'          3390 DISK STORAGE         *JLM* 09830000
TYPEDEND EQU   *                                                        09850000
         SPACE 2                                                        09860000
*        *------------------------------------------------------------* 09870000
*        *  THIS TABLE CONTAINS ENTRIES OF ALL MAGNETIC TAPE DEVICE   * 09880000
*        *  TYPES AS GENERATED IN 'DEVMASKT' BY SYSGEN AT BANKERS.    * 09890000
*        *  EACH ENTRY IS 12 BYTES LONG WITH THE FOLLOWING,           * 09900000
*        *    BYTE 0-3  :  MTD CLASS IN HEXADECIMAL FORMAT            * 09910000
*        *    BYTE 4-11 :  MTD TYPE IN CHARACTER FORMAT               * 09920000
*        *------------------------------------------------------------* 09930000
         SPACE                                                          09940000
TYPEMTD  DS    0F                                                       09950000
         DC    X'34008001',CL8'2400-3'  1600 BPI                        09960000
         DC    X'34008003',CL8'3400-3'  1600 BPI                        09970000
         DC    X'32008003',CL8'3400-5'  6250 BPI                        09980000
         DC    X'32108003',CL8'3400-6'  DUAL DENSITY (1600/6250) BPI    09990000
NUMOMTD  EQU   (*-TYPEMTD)/12                                           10000000
         EJECT                                                          10010000
*        *------------------------------------------------------------* 10020000
*        *  THIS TABLE CONTAINS ENTRIES OF ALL THE ESOTERICS DEVICE   * 10030000
*        *  TYPES AS GENERATED IN 'DEVMASKT' BY SYSGEN AT BANKERS.    * 10040000
*        *  EACH ENTRY IS 8 BYTES LONG WITH THE FOLLOWING,            * 10050000
*        *    BYTE 0   :  ESOTERICS CLASS IN HEXADECIMAL FORMAT       * 10060000
*        *    BYTE 1-7 :  ESOTERICS TYPE IN CHARACTER FORMAT          * 10070000
*        *------------------------------------------------------------* 10080000
         SPACE                                                          10090000
TYPESTRC DS    0F                                                       10100000
         DC    X'00',CL7' '             * * *   N O T   U S E D   * * * 10110000
         DC    X'01',CL7'SYSSQ'         DASD                            10120000
         DC    X'02',CL7'SYSDA'         MTD                             10130000
         DC    X'03',CL7'SYSCP'         URD                             10140000
         DC    X'04',CL7'SYSPR'         URD                             10150000
         DC    X'05',CL7'DISK'          DASD                            10160000
         DC    X'06',CL7'DISC'          DASD                            10170000
         DC    X'07',CL7'TAPE'          MTD                             10180000
         DC    X'08',CL7'SORT'          DASD                            10190000
         DC    X'09',CL7'CICSLOG'       MTD                             10200000
         DC    X'0A',CL7'T160'          MTD                             10210000
         DC    X'0B',CL7'T625'          MTD                             10220000
         DC    X'0C',CL7'D3350'         DASD                            10230000
         DC    X'0D',CL7'SYSTS'         DASD                            10240000
         DC    X'0E',CL7'VIO'           DASD                            10250000
         DC    X'0F',CL7'SYS3800'       URD                             10260000
TYPESEND EQU   *                                                        10270000
         EJECT                                                          10280000
*        *-----------------------*                                      10290000
*        *   C O N S T A N T S   *                                      10300000
*        *-----------------------*                                      10310000
         SPACE                                                          10320000
CIZERO   DC    XL3'000000'              CI NO. OF 'D' REC OF CATLG 'C'  10330000
ENTYPE   DC    X'40'                    ENTRY TYPE                      10340000
         SPACE 2                                                        10350000
RECTBL   DC    256X'00'                 TYPE 0 - UNDEFINED              10360000
         ORG   RECTBL+C'A'                                              10370000
         DC    X'01'                    TYPE 5                          10380000
         ORG   RECTBL+C'B'                                              10390000
         DC    X'02'                    TYPE 4                          10400000
         ORG   RECTBL+C'C'                                              10410000
         DC    X'03'                    TYPE 3                          10420000
         ORG   RECTBL+C'D'                                              10430000
         DC    X'04'                    TYPE 2                          10440000
         ORG   RECTBL+C'G'                                              10450000
         DC    X'05'                    TYPE 3                          10460000
         ORG   RECTBL+C'I'                                              10470000
         DC    X'06'                    TYPE 2                          10480000
         ORG   RECTBL+C'R'                                              10490000
         DC    X'07'                    TYPE 4                          10500000
         ORG   RECTBL+C'U'                                              10510000
         DC    X'08'                    TYPE 5                          10520000
         ORG   RECTBL+C'V'                                              10530000
         DC    X'09'                    TYPE 1                          10540000
         ORG   RECTBL+C'X'                                              10550000
         DC    X'0A'                    TYPE 6                          10560000
         ORG                                                            10570000
         EJECT                                                          10580000
*        *-------------------------------------*                        10590000
*        *   E N T R Y   T Y P E   T A B L E   *                        10600000
*        *-------------------------------------*                        10610000
         SPACE                                                          10620000
ENTTBL   DS    0D                                                       10630000
         DC    X'01',CL15'NON VSAM'                                     10640000
         DC    X'02',CL15'GDG'                                          10650000
         DC    X'03',CL15'CLUSTER'                                      10660000
         DC    X'04',CL15'DATA'                                         10670000
         DC    X'05',CL15'ALTERNATE INDEX'                              10680000
         DC    X'06',CL15'INDEX'                                        10690000
         DC    X'07',CL15'PATH'                                         10700000
         DC    X'08',CL15'USER CATALOG'                                 10710000
         DC    X'09',CL15'SPACE'                                        10720000
         DC    X'0A',CL15'ALIAS'                                        10730000
EOENTTBL EQU   *                                                        10740000
         SPACE 2                                                        10750000
HEXNUM   DC    CL16'0123456789ABCDEF'                                   10760000
HEXTBL   EQU   HEXNUM-240                                               10770000
         SPACE 2                                                        10780000
*        *-------------------------*                                    10790000
*        *   P A R M   L I S T S   *                                    10800000
*        *-------------------------*                                    10810000
         SPACE                                                          10820000
PARM9421 DS    0F                                                       10830000
ACATN21  DC    A(0)                                                     10840000
ACTPL21  DC    A(0)                                                     10850000
AMSGF21  DC    A(0)                                                     10860000
ARPTF21  DC    A(0)                                                     10870000
AWORK21  DC    A(0)                                                     10880000
         EJECT                                                          10890000
*        *---------------------------------------------*                10900000
*        *   C T L G P L   A N D   C T G F L   -   C   *                10910000
*        *---------------------------------------------*                10920000
         SPACE                                                          10930000
CTLGPLC  DS    0D                                                       10940000
CTGCOPT1 DC    X'40008108'                                              10950000
*  CHECK THE MASTER PASSWORD                                            10960000
*  A CATALOG MANAGEMENT SERVICES FUNCTION                               10970000
*  THE CALL IS A VSAM CATALOG MANAGEMENT REQUEST                        10980000
*  BYPASS SECURITY PROMPTING TO SYSTEM OPERATOR                         10990000
*                                                                       11000000
CTGCENT  DC    A(0)                     ADDR OF CI NUMBER               11010000
CTGCCAT  DC    A(0)                     ADDR OF CATACB                  11020000
CTGCWKB  DC    A(0)                     ADDR OF WORK AREA 'B'           11030000
CTGCOPT2 DC    X'20000002'                                              11040000
         ORG   CTGCOPT2+2                                               11050000
CTGCRCTP DS    CL1                      RECORD TYPE                     11060000
CTGCFLDN DS    CL1                      NO. OF CTLGFLD'S                11070000
         ORG                                                            11080000
CTGCDDNM DC    A(0)                                                     11090000
CTGCPSWD DC    A(0)                     ADDR OF PASSWORD                11100000
CTGCFLDS DS    0F                                                       11110000
         DC    A(CTLGFLC1)                                              11120000
         DC    A(CTLGFLC2)                                              11130000
         DC    A(CTLGFLC3)                                              11140000
         SPACE 2                                                        11150000
CTGFLC   DS    0D                                                       11160000
         SPACE                                                          11170000
CTLGFLC1 DC    X'01',AL3(0),F'0'                                        11180000
         DC    A(REQUES02)              ENTYPE    (LTH = 1)             11190000
         DC    3F'0'                                                    11200000
         SPACE                                                          11210000
CTLGFLC2 DC    X'01',AL3(0),F'0'                                        11220000
         DC    A(REQUES03)              ENTNAME   (LTH = 44)            11230000
         DC    3F'0'                                                    11240000
         SPACE                                                          11250000
CTLGFLC3 DC    X'01',AL3(0),F'0'                                        11260000
         DC    A(0)                     NAMEDS / CATVOL                 11270000
         DC    3F'0'                                                    11280000
         SPACE 2                                                        11290000
*        *---------------------------*                                  11300000
*        *   F I E L D   N A M E S   *                                  11310000
*        *---------------------------*                                  11320000
         SPACE                                                          11330000
REQUES02 DC    CL8'ENTYPE'              LTH = 1                         11340000
REQUES03 DC    CL8'ENTNAME'             LTH = 44                        11350000
REQUES05 DC    CL8'NAMEDS'              LTH = VL: (4) / ASSOC           11360000
REQUES10 DC    CL8'CATVOL'              LTH = VL: (15) / VOL            11370000
         EJECT                                                          11380000
*        *-------------------*                                          11390000
*        *   D S E C T ' S   *                                          11400000
*        *-------------------*                                          11410000
         SPACE                                                          11420000
CTLGPLB  DSECT                                                          11430000
CTGBOPT1 DS    XL4                                                      11440000
CTGBENT  DS    F                                                        11450000
CTGBCAT  DS    F                                                        11460000
CTGBWKA  DS    F                                                        11470000
CTGBOPT2 DS    XL4                                                      11480000
CTGBDDNM DS    F                                                        11490000
CTGBPSWD DS    F                                                        11500000
CTGBFLDS DS    0F                                                       11510000
CTGBAFL1 DS    F                        ENTYPE                          11520000
CTGBAFL2 DS    F                        ENTNAME                         11530000
CTGBAFL3 DS    F                        CATTR                           11540000
CTGBAFL4 DS    F                        NAMEDS                          11550000
CTGBAFL5 DS    F                        DSETEXDT                        11560000
CTGBAFL6 DS    F                        DSETCRDT                        11570000
CTGBAFL7 DS    F                        OWNERID                         11580000
CTGBAFL8 DS    F                        CATVOL                          11590000
         SPACE 2                                                        11600000
NMDSECT  DSECT                                                          11610000
NMDSTYP  DS    CL1                      ENTRY TYPE                      11620000
NMDSCIN  DS    XL3                      CI NUMBER                       11630000
         EJECT                                                          11640000
CTVOL    DSECT                                                          11650000
CTVREPNO DS    CL2                      RELATIVE REPETITION NUMBER      11660000
CTVDEVTP DS    CL4                      DEVICE TYPE                     11670000
CTVOLSER DS    CL6                      VOLUME SERIAL NUMBER            11680000
CTVFILSQ DS    CL2                      FILE SEQUENCE NUMBER            11690000
CTVOLFLG DS    CL1                      VOLUME FLAGS                    11700000
*                                       BIT 0    X'80' PRIME            11710000
*                                       BIT 1    X'40' CANDIDATE        11720000
*                                       BIT 2    X'20' OVERFLOW         11730000
*                                       BIT 3-7  X'1F' RESERVED         11740000
         SPACE 2                                                        11750000
TXTSECT  DSECT                                                          11760000
         DS    CL4                                                      11770000
TXTENTRY DS    CL44                                                     11780000
         DS    CL4                                                      11790000
TXTENTYP DS    CL15                                                     11800000
         DS    CL2                                                      11810000
TXTOWNER DS    CL8                                                      11820000
         DS    CL2                                                      11830000
TXTCREDT DS    CL8                                                      11840000
         DS    CL2                                                      11850000
TXTEXPDT DS    CL8                                                      11860000
         DS    CL2                                                      11870000
TXTDEVTP DS    CL8                                                      11880000
         DS    CL1                                                      11890000
TXTVOLSR DS    CL6                                                      11900000
         DS    CL2                                                      11910000
TXTSTATS DS    CL5                                                      11920000
         END                                                            11930000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD                             00210000
//LKED.SYSIN DD *                                                       00220000
  NAME VSAMLST3(R)                                                      00230000
//A        EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF'               00120000
//ASM.SYSIN DD *                                                        00130000
         PRINT OFF                                                      00010000
         MACRO                                                          00020000
&NAME    INIT &BASE=3,&REGS=Y,&PATCH=3,&RENT=N,&SAVE=Y                  00030000
         AIF   ((&BASE LT 13) AND (&BASE GT 1)).N020                    00040000
         MNOTE 12,'INVALID BASE REGISTER'                               00050000
         MEXIT                                                          00060000
.N020    ANOP                                                           00070000
         PUSH  PRINT                                                    00080000
         PRINT ON,GEN                                                   00090000
         EJECT                                                          00100000
&SYSECT  CSECT                                                          00110000
         USING *,15                                                     00120000
         B     BEGIN                                                    00130000
         DC    AL1(24)                                                  00140000
         DC    CL8'&SYSECT'                                             00150000
         DC    CL16'-&SYSDATE-&SYSTIME'                                 00160000
         AIF   ('&RENT' EQ 'Y').N004                                    00170000
MYSAVE   DC    18F'-1'                                                  00180000
.N004    ANOP                                                           00190000
         AIF   ('&PATCH' EQ '0').N005                                   00200000
PATCH    DC    &PATCH.CL8'*PATCH*'                                      00210000
.N005    ANOP                                                           00220000
         AIF   ('&REGS' EQ 'N').N030                                    00230000
         AIF   ('&REGS' EQ 'Y').N010                                    00240000
         MNOTE 4,'REGS OPERAND INVALID. Y SUBSTITUTED'                  00250000
.N010    ANOP                                                           00260000
R0       EQU   0                                                        00270000
R1       EQU   1                                                        00280000
R2       EQU   2                                                        00290000
R3       EQU   3                                                        00300000
R4       EQU   4                                                        00310000
R5       EQU   5                                                        00320000
R6       EQU   6                                                        00330000
R7       EQU   7                                                        00340000
R8       EQU   8                                                        00350000
R9       EQU   9                                                        00360000
R10      EQU   10                                                       00370000
R11      EQU   11                                                       00380000
R12      EQU   12                                                       00390000
R13      EQU   13                                                       00400000
R14      EQU   14                                                       00410000
R15      EQU   15                                                       00420000
.N030    ANOP                                                           00430000
BEGIN    DS   0H                                                        00440000
         STM   14,12,12(13)                                             00450000
         LR    &BASE,15                                                 00460000
         DROP  15                                                       00470000
         USING &SYSECT,&BASE                                            00480000
         AIF   ('&SAVE' EQ 'N').N003                                    00490000
         AIF   ('&RENT' EQ 'Y').N002                                    00500000
         AIF   ('&RENT' EQ 'N').N001                                    00510000
         MNOTE 4,'RENT OPERAND INVALID. N SUBSTITUTED'                  00520000
.N001    ANOP                                                           00530000
         ST    13,MYSAVE+4                                              00540000
         LR    15,13                                                    00550000
         LA    13,MYSAVE                                                00560000
         ST    13,8(15)                                                 00570000
         AGO   .N003                                                    00580000
.N002    ANOP                                                           00590000
         GETMAIN R,LV=72                                                00600000
         ST    13,4(1)                                                  00610000
         ST    1,8(13)                                                  00620000
         LR    13,1                                                     00630000
.N003    ANOP                                                           00640000
         POP   PRINT                                                    00650000
         EJECT                                                          00660000
         MEND                                                           00670000
         PRINT ON                                                       00680000
*  MODULE NAME:         VSAMLST4     (REL. 1.1  08/10/79)               00690000
*                                                                       00700000
*  MODULE DESCRIPTION:  VSAM CATALOG LIST UTILITY - DETAILS REPORT      00710000
*                                                                       00720000
*  RETURN LINKAGE:      RETURN (14,12),RC=0                             00730000
*                                                                       00740000
*  LINKAGE TABLE:       NONE - SINGLE ENTRY                             00750000
*                                                                       00760000
*  PARAMETERS:          R1 POINTS TO A PARAMETER LIST ALIGNED ON A      00770000
*                       FULL WORD BOUNDARY.                             00780000
*                        WORD 1 - ADDRESS OF VSAM CATALOG NAME          00790000
*                        WORD 2 - ADDRESS OF CTLGPL-B                   00800000
*                        WORD 3 - ADDRESS OF MSGFILE DCB                00810000
*                        WORD 4 - ADDRESS OF SORTIN DCB                 00820000
*                        WORD 5 - ADDRESS OF WORK AREA 'B'              00830000
*                                                                       00840000
*  EXIT:                RC=00 - SUCCESSFUL                              00850000
*                                                                       00860000
*  MODULE FUNCTION:     FOR EACH VSAM ENTRY SPECIFIED IN THE PARAMETER  00870000
*                       LIST, THIS MODULE FORMATS IT FOR THE VSAM       00880000
*                       VOLUME CONTENTS REPORT.  THE FORMATTED DATA     00890000
*                       WILL BE SORTED IN ASCENDING ORDER OF VOLUME     00900000
*                       SERIAL NUMBER AND THEN PRINTED VIA AN E15 SORT  00910000
*                       EXIT.  ONLY TYPES 'C', 'G' AND 'V' ARE          00920000
*                       PROCESSED BY THIS MODULE.                       00930000
*                                                                       00940000
*  CALLER:              VSAMLST2                                        00950000
*                                                                       00960000
*  CALLS:               NONE                                            00970000
*                                                                       00980000
*  SYSTEMS SERVICES:    PUT (QSAM), SVC 26                              00990000
*                                                                       01000000
*  MODULE ENVIRONMENT:  OS/VS1, OS/VS2                                  01010000
*                       NOTE - SEE DSECT UCBOB AT THE END OF SOURCE     01020000
*                       UCBVTOC HAS DIFFERENT OFFSETS IN VS1 AND MVS    01030000
         TITLE 'VSAMLST4 --- VSAM CATALOG LIST UTILITY DETAILS REPORTS'X01040000
               ' DATA GENERATOR (2.2)'                                  01050000
*        *-----------------------------------*                          01060000
*        *   R E G I S T E R   U S A G E S   *                          01070000
*        *-----------------------------------*                          01080000
*                                                                       01090000
*  R0  -  STANDARD LINKAGE                                              01100000
*  R1  -  STANDARD LINKAGE (ADDR OF PARM LIST) & TEMP WORK SPACE        01110000
*  R2  -  TEMP WORK SPACE                                               01120000
*  R3  -  TEMP WORK SPACE                                               01130000
*  R4  -  NOT USED                                                      01140000
*  R5  -  NOT USED                                                      01150000
*  R6  -  BASE REG FOR DSECT ETXDSCRP                                   01160000
*  R7  -  BASE REG FOR DSECT ETVOL                                      01170000
*  R8  -  BASE REG FOR DSECT NMDSECT, DSPDSCRP, DSCB4, DSCB5            01180000
*  R9  -  BASE REG FOR DSECT CTLGPLB                                    01190000
*  R10 -  BASE REG FOR DSECT TXVLCHR, TXDTAIL, TXEXTNT                  01200000
*  R11 -  RETURN ADDR FOR SUBROUTINES                                   01210000
*  R12 -  BASE REG FOR CSECT VSAMLST4                                   01220000
*  R13 -  STANDARD LINKAGE (ADDR OF SAVE AREA)                          01230000
*  R14 -  STANDARD LINKAGE (ADDR TO RETURN)                             01240000
*  R15 -  STANDARD LINKAGE (ADDR OF ENTRY POINT & RETURN CODE)          01250000
         SPACE 3                                                        01260000
VSAMLST4 CSECT                                                          01270000
         INIT  BASE=12                                                  01280000
*********************************************************************** 01290000
*                                                                     * 01300000
*  2.2    DETAILS REPORT GENERATOR                                    * 01310000
*                                                                     * 01320000
*     FUNCTION:  IF TYPE 'V', SET VOLUME INFO'S FOR TITLES AND FREE   * 01330000
*                EXTENT INFO'S FOR TRACK ALLOCATION MAP.              * 01340000
*                IF TYPE 'C' OR 'G', FORMAT DETAIL INFO'S AND WRITE   * 01350000
*                IT OUT TO THE SORT INPUT DATA SET.                   * 01360000
*                                                                     * 01370000
*     ERRORS:  NONE                                                   * 01380000
*                                                                     * 01390000
*********************************************************************** 01400000
         SPACE 3                                                        01410000
VCL22    EQU   *                                                        01420000
         MVC   PARM9422(20),0(R1)       SAVE 5 FULL WORDS PARM LIST     01430000
         LA    R10,LINE                 SET BASE REG FOR OUTPUT         01440000
         L     R9,ACTPL22               ADDR OF CTLGPL-B                01450000
         USING CTLGPLB,R9                                               01460000
         SPACE                                                          01470000
         L     R1,CTGBAFL1              ADDR OF CTGFL-B1                01480000
         L     R1,20(,R1)               ADDR OF ENTRY TYPE              01490000
         MVC   ENTYPE,0(R1)             SAVE IT FOR CHECKING            01500000
         SR    R2,R2                    INDEX FOR BRANCH TABLE          01510000
         TRT   ENTYPE,RECTBL            SEE WHICH TYPE IT IS            01520000
         BZ    TYPE0                    TYPE 0 - UNDEFINED              01530000
         SPACE                                                          01540000
         SLL   R2,2                     DISPLACEMENT OF BRANCH TABLE    01550000
RECTYP   B     RECTYP(R2)                                               01560000
         B     TYPE2                    TYPE 'C'                        01570000
         B     TYPE2                    TYPE 'G'                        01580000
         B     TYPE1                    TYPE 'V'                        01590000
         EJECT                                                          01600000
*        *******************************************************        01610000
*        *                                                     *        01620000
*        *  TYPE 1 - SET VOLUME CHAR'S AND FREE EXTENT INFO'S  *        01630000
*        *                                                     *        01640000
*        *******************************************************        01650000
         SPACE                                                          01660000
TYPE1    EQU   *                                                        01670000
         USING TXEXTNT,R10                                              01680000
         L     R1,CTGBAFL2              ADDR OF CTLGFL-B2               01690000
         L     R1,20(,R1)               ADDR OF ENTRY NAME              01700000
         ST    R1,CTGDENT                                               01710000
         MVC   CTGDCAT,CTGBCAT          ADDR OF CATACB                  01720000
         MVC   CTGDWKB,AWORK22          ADDR OF WORK AREA 'B'           01730000
         MVC   CTGDPSWD,CTGBPSWD        ADDR OF PASSWORD                01740000
         SPACE                                                          01750000
         LA    R1,CTLGPLD                                               01760000
         SVC   26                                                       01770000
         SPACE                                                          01780000
         LTR   R15,R15                  IS SVC SUCCESSFUL?              01790000
         BZ    SVCOK221                 YES - CONTINUE                  01800000
         ABEND 221,DUMP                                                 01810000
         SPACE                                                          01820000
SVCOK221 EQU   *                                                        01830000
         L     R1,CTLGFLD2+20           ADDR OF ENTNAME                 01840000
         MVC   TXEVOLNO,0(R1)                                           01850000
         SPACE                                                          01860000
         USING VOLDVCHR,R1                                              01870000
         L     R1,CTLGFLD3+20           ADDR OF VOLDVCHR                01880000
         MVC   TRKCYL,VOLDNTRK          SAVE TRK'S PER CYL              01890000
         DROP  R1                                                       01900000
         SPACE                                                          01910000
         MVC   VSFSNUM(6),ZERO          AND VSFSTRK & VSFSLGE           01920000
         MVC   OSFSNUM(8),ZERO          AND OSFSTRK & OSFSCYL & OSFSLGE 01930000
         EJECT                                                          01940000
*        *------------------------------*                               01950000
*        *  SET VSAM FREE SPACE INFO'S  *                               01960000
*        *------------------------------*                               01970000
         SPACE                                                          01980000
SETVSFS  EQU   *                                                        01990000
         USING DSPDSCRP,R8                                              02000000
         L     R0,CTLGFLD7+16           LENGTH OF DSPDSCRP(S)           02010000
         L     R8,CTLGFLD7+20           ADDR OF DSPDSCRP(S)             02020000
         AR    R0,R8                    ADDR OF END OF DSPDSCRP(S)      02030000
         ST    R0,EODSPX                UPPER LIMIT                     02040000
         SPACE                                                          02050000
NEXTDSPX EQU   *                                                        02060000
         MVC   BEGINRCH,DSPXSTRT        GET THE BEGINNING ADDR IN CCHH  02070000
         LH    R1,BEGINCC               CYL OF BEGINNING ADDR           02080000
         MH    R1,TRKCYL                IN TRK'S                        02090000
         AH    R1,BEGINHH               PLUS TRK OF BEGINNING ADDR      02100000
         STH   R1,BEGINTT               SAVE ITS RELATIVE TRACK ADDR    02110000
         SPACE                                                          02120000
         MVC   TXELOTCH,DSPXSTRT        SET BEGINNING CCHH OF DSP EXT   02130000
         STH   R1,TXELOTRK              SET BEGINNING TT OF DSP EXT     02140000
         LH    R15,DSPTKEXT             LTH OF DSP EXT                  02150000
         STH   R15,TXEXTLTH                                             02160000
         AR    R15,R1                                                   02170000
         BCTR  R15,0                    UPPER = LOWER + LTH - 1         02180000
         STH   R15,TXEHITRK             SET ENDING TT OF DSP EXT        02190000
         SPACE                                                          02200000
         LH    R0,TRKCYL                NO. OF TRK'S PER CYL            02210000
         SR    R14,R14                                                  02220000
         DR    R14,R0                   CONVERT TT TO CCHH              02230000
         STH   R15,TXEHICC              CYL                             02240000
         STH   R14,TXEHIHH              TRK                             02250000
         MVC   TXERCKEY,ZERO            MAKE SURE THIS COMES FIRST      02260000
         MVI   TXEXTNM,X'00'            CLEAR EXT NAME                  02270000
         MVC   TXEXTNM+1(L'TXEXTNM-1),TXEXTNM                           02280000
         MVC   TXEXTNO,ZERO             EXT SEQ NO. (NOT USED)          02290000
         OI    TXERPTNO,X'80'           FOR REPORT 2                    02300000
         SPACE                                                          02310000
         L     R1,ASORT22               WRITE TO FILE SORTIN            02320000
         PUT   (1),LINE                 REPORT 2                        02330000
         EJECT                                                          02340000
*        *--------------------------------------*                       02350000
*        *  SET VSAM FREE SPACE INFO'S - CONT.  *                       02360000
*        *--------------------------------------*                       02370000
         SPACE                                                          02380000
         MVC   TXELOTCH,TXEHITCH        SET DUMMY EXT FOR END OF DSP    02390000
         MVC   TXERCKEY,FFFF            MAKE SURE THIS COMES LAST       02400000
         MVI   TXEXTNM,X'FF'            SET EXT NAME                    02410000
         MVC   TXEXTNM+1(L'TXEXTNM-1),TXEXTNM                           02420000
         MVC   TXELOTRK,TXEHITRK                                        02430000
         OI    TXERPTNO,X'80'           FOR REPORT 2                    02440000
         SPACE                                                          02450000
         L     R1,ASORT22               WRITE TO FILE SORTIN            02460000
         PUT   (1),LINE                 REPORT 2                        02470000
         SPACE 2                                                        02480000
         SR    R2,R2                                                    02490000
         ICM   R2,3,DSPSMLTH            LTH OF RLC (RUN LENGTH CODE)    02500000
         LTR   R2,R2                    IS THERE A SPACE MAP?           02510000
         BZ    BUMPDSPX                 NO - BUMP TO NEXT DSPDSCRP      02520000
         CLC   DSPTUSED,DSPTKEXT        IS DATA SPACE ALL USED?         02530000
         BE    BUMPDSPX                 YES - NO FREE SPACE HERE        02540000
         SPACE                                                          02550000
         LA    R7,DSPSMRLC              BEGINNING OF RLC                02560000
         MVC   TXEXTNM,VSFSNAM          * * *  VSAM FREE SPACE  * * *   02570000
         MVI   TXERCKEY+1,X'01'         ONLY DSP EXT HAS KEY X'0000'    02580000
         EJECT                                                          02590000
*        *------------------------------*                               02600000
*        *  DECODE THE SPACE MAP'S RLC  *                               02610000
*        *------------------------------*                               02620000
         SPACE                                                          02630000
NEXTVSE  EQU   *                                                        02640000
         SR    R3,R3                                                    02650000
         CLI   0(R7),X'FA'              IS IT AN 1-BYTE RLC?            02660000
         BL    RLC1B1                   YES                             02670000
RLC2B1   EQU   *                                                        02680000
         ICM   R3,3,1(R7)               NO - IT'S A 2-BYTES RLC         02690000
         LA    R7,3(,R7)                                                02700000
         SH    R2,THREE                                                 02710000
         B     USEDVSE                  USED VSAM EXT                   02720000
RLC1B1   EQU   *                                                        02730000
         IC    R3,0(,R7)                ONLY AN 1-BYTE RLC              02740000
         LA    R7,1(,R7)                                                02750000
         BCTR  R2,0                                                     02760000
         SPACE                                                          02770000
USEDVSE  EQU   *                                                        02780000
         LH    R15,BEGINTT              BEGINNING OF EXTENT             02790000
         AR    R15,R3                   PLUS ITS LENGTH                 02800000
         STH   R15,BEGINTT              FOR NEXT EXTENT                 02810000
         LTR   R2,R2                    ARE THERE ANY MORE RLC'S?       02820000
         BZ    BUMPDSPX                 NO - PROCESS NEXT DSPDSCRP      02830000
         SPACE 2                                                        02840000
         SR    R3,R3                    YES - IT MUST BE FOR FREE EXT   02850000
         CLI   0(R7),X'FA'              IS IT AN 1-BYTE RLC?            02860000
         BL    RLC1B2                   YES                             02870000
RLC2B2   EQU   *                                                        02880000
         ICM   R3,3,1(R7)               NO - IT'S A 2-BYTES RLC         02890000
         LA    R7,3(,R7)                                                02900000
         SH    R2,THREE                                                 02910000
         B     FREEVSE                  FREE VSAM EXT                   02920000
RLC1B2   EQU   *                                                        02930000
         IC    R3,0(,R7)                ONLY AN 1-BYTE RLC              02940000
         LA    R7,1(,R7)                                                02950000
         BCTR  R2,0                                                     02960000
         EJECT                                                          02970000
*        *--------------------------------------*                       02980000
*        *  FORMAT THE VSAM FREE SPACE EXTENTS  *                       02990000
*        *--------------------------------------*                       03000000
         SPACE                                                          03010000
FREEVSE  EQU   *                                                        03020000
         LH    R0,TRKCYL                NO. OF TRK'S PER CYL            03030000
         LH    R15,BEGINTT              BEGINNING ADDR IN RELATIVE TRK  03040000
         STH   R15,TXELOTRK                                             03050000
         SR    R14,R14                                                  03060000
         DR    R14,R0                   CONVERT TT TO CCHH              03070000
         STH   R15,TXELOCC              CYL                             03080000
         STH   R14,TXELOHH              TRK                             03090000
         SPACE                                                          03100000
         LH    R15,BEGINTT              BEGINNING OF EXTENT             03110000
         AR    R15,R3                   PLUS ITS LENGTH                 03120000
         STH   R15,BEGINTT              FOR NEXT EXTENT                 03130000
         BCTR  R15,0                    LESS ONE FOR THIS EXTENT'S END  03140000
         STH   R15,TXEHITRK                                             03150000
         SR    R14,R14                                                  03160000
         DR    R14,R0                   CONVERT TT TO CCHH              03170000
         STH   R15,TXEHICC              CYL                             03180000
         STH   R14,TXEHIHH              TRK                             03190000
         SPACE                                                          03200000
         LH    R1,VSFSNUM               SEQ NO. OF VSAM FREE EXT        03210000
         STH   R1,TXEXTNO                                               03220000
         LA    R1,1(,R1)                BUMP NO. OF VSAM FREE EXT'S     03230000
         STH   R1,VSFSNUM                                               03240000
         LH    R1,VSFSTRK               TOTAL NO. OF VSAM FREE TRK'S    03250000
         AR    R1,R3                    PLUS LENGTH OF THIS EXT         03260000
         STH   R1,VSFSTRK               BUMP TOTAL                      03270000
         CH    R3,VSFSLGE               IS THIS THE LARGEST EXT SO FAR? 03280000
         BNH   NOTVSLGE                 NO - SKIP IT THEN               03290000
         STH   R3,VSFSLGE               YES - SWAP IT                   03300000
         SPACE                                                          03310000
NOTVSLGE EQU   *                                                        03320000
         STH   R3,TXEXTLTH              LTH OF VSAM FREE EXT            03330000
         OI    TXERPTNO,X'80'           FOR REPORT 2                    03340000
         EJECT                                                          03350000
*        *-----------------------------------------*                    03360000
*        *  WRITE OUT THE VSAM FREE SPACE EXTENTS  *                    03370000
*        *-----------------------------------------*                    03380000
         SPACE                                                          03390000
         L     R1,ASORT22               WRITE TO FILE SORTIN            03400000
         PUT   (1),LINE                 REPORT 2                        03410000
         SPACE                                                          03420000
         LTR   R2,R2                    ARE THERE ANY MORE RLC'S?       03430000
         BNZ   NEXTVSE                  YES - PROCESS THEM              03440000
         SPACE 2                                                        03450000
BUMPDSPX EQU   *                                                        03460000
         SR    R0,R0                                                    03470000
         ICM   R0,3,DSPSMLTH            BUMP PAST THE SPACE MAP         03480000
         LA    R8,DSPSMRLC              TO THE NEXT DSPDSCRP            03490000
         AR    R8,R0                                                    03500000
         C     R8,EODSPX                IS IT STILL WITHIN LIMIT?       03510000
         BL    NEXTDSPX                 YES - KEEP ON GOING             03520000
         SPACE                                                          03530000
         B     SETOSFS                  NO - ALL DONE WITH VSAM FREE SP 03540000
         DROP  R8                                                       03550000
         SPACE 2                                                        03560000
EODSPX   DC    F'-1'                                                    03570000
         EJECT                                                          03580000
*        *----------------------------*                                 03590000
*        *  SET OS FREE SPACE INFO'S  *                                 03600000
*        *----------------------------*                                 03610000
         SPACE                                                          03620000
SETOSFS  EQU   *                                                        03630000
         MVI   TXERCKEY+1,X'01'         ONLY DSP EXT HAS KEY '0000'     03640000
         SPACE                                                          03650000
*        *-----------------------------------*                          03660000
*        *  SEARCH THE UCB'S FOR THE VOLUME  *                          03670000
*        *-----------------------------------*                          03680000
         SPACE                                                          03690000
         L     R2,16                    R2 = ADDR OF CVT                03700000
         L     R2,40(,R2)               R2 = ADDR OF UCB ADDR LIST      03710000
         USING UCBOB,R1                                                 03720000
         SPACE                                                          03730000
ADDRUCB  EQU   *                                                        03740000
         LH    R1,0(,R2)                R1 = ADDR OF UCB                03750000
         CH    R1,FFFF                  IS THIS THE LAST ENTRY?         03760000
         BE    NOOSFS                   YES - NOT FOUND                 03770000
         SPACE                                                          03780000
         LTR   R1,R1                    IS THIS A NULL ENTRY?           03790000
         BZ    NEXTUCBP                 YES - NEXT UCB POINTER, PLEASE  03800000
         CLI   UCBTYP+2,X'20'           IS DEVICE TYPE DASD?            03810000
         BNE   NEXTUCBP                 NO - NEXT UCB POINTER, PLEASE   03820000
         TM    UCBSTAT,X'80'            IS DEVICE STATUS ON-LINE?       03830000
         BZ    NEXTUCBP                 NO - NEXT UCB POINTER, PLEASE   03840000
         CLC   UCBVOLI,TXEVOLNO         IS THIS THE RIGHT VOLUME?       03850000
         BE    UCBFOUND                 YES - STOP SEARCHING            03860000
         SPACE                                                          03870000
NEXTUCBP EQU   *                                                        03880000
         LA    R2,2(,R2)                R2 = ADDR OF NEXT UCB POINTER   03890000
         B     ADDRUCB                  LET'S DO IT AGAIN               03900000
         SPACE 2                                                        03910000
UCBFOUND EQU   *                                                        03920000
         MVC   VOLSTAB,UCBSTAB          SAVE VOLUME USE                 03930000
         LH    R0,TRKCYL                NO. OF TRK'S PER CYL            03940000
         SR    R14,R14                  CONVERT TTRZ OF VTOC TO CCHHR   03950000
         LH    R15,UCBVTOC              R15 = TT OF TTRZ                03960000
         DR    R14,R0                   R14 = HH & R15 = CC             03970000
         STH   R15,F45CC                                                03980000
         STH   R14,F45HH                                                03990000
         MVC   F45R,UCBVTOC+2           R1 = R OF TTRZ                  04000000
         ST    R10,F45VOLNO             SET VOLSER IN CAMLST            04010000
         SPACE                                                          04020000
         DROP  R1                                                       04030000
         EJECT                                                          04040000
*        *---------------------------------------*                      04050000
*        *  OBTAIN FMT 4 DSCB - SET VTOC EXTENT  *                      04060000
*        *---------------------------------------*                      04070000
         SPACE                                                          04080000
FMT4     EQU   *                                                        04090000
         USING DSCB4,R8                                                 04100000
         LA    R8,F45DSCB               SET BASE REG FOR DSCB4 & DSCB5  04110000
         OBTAIN F45CMLST                                                04120000
         SPACE                                                          04130000
         LTR   R15,R15                  IS OBTAIN SUCCESSFUL?           04140000
         BNZ   NOOSFS                   NO - SKIP ALL OS EXT'S          04150000
         CLI   DS4DSNAM,X'04'           MAKE SURE                       04160000
         BNE   NOOSFS                   THIS IS A                       04170000
         CLI   DS4IDFMT,X'F4'           VALID                           04180000
         BNE   NOOSFS                   FMT 4 DSCB AND                  04190000
         TM    DS4VTOCI,X'80'           IS THERE A VALID FMT 5 DSCB?    04200000
         BO    NOOSFS                   NO - EITHER NONE OR INVALID     04210000
         SPACE                                                          04220000
         MVC   TXEXTNM,VTOCNAM          * * *  VTOC  * * *              04230000
         MVC   TXELOTCH,DS4VTLOR        LOW CCHH OF VTOC                04240000
         MVC   TXEHITCH,DS4VTUPR        HIGH CCHH OF VTOC               04250000
         SPACE                                                          04260000
         DROP  R8                                                       04270000
         EJECT                                                          04280000
*        *----------------------------------------*                     04290000
*        *  FORMAT AND WRITE OUT THE VTOC EXTENT  *                     04300000
*        *----------------------------------------*                     04310000
         SPACE                                                          04320000
         LH    R2,TXELOCC               LOW CYL                         04330000
         MH    R2,TRKCYL                TIMES TRK'S/CYL                 04340000
         AH    R2,TXELOHH               PLUS LOW TRK                    04350000
         STH   R2,TXELOTRK              LOWER LIMIT IN TRACKS           04360000
         SPACE                                                          04370000
         LH    R3,TXEHICC               HIGH CYL                        04380000
         MH    R3,TRKCYL                TIMES TRK'S/CYL                 04390000
         AH    R3,TXEHIHH               PLUS HIGH TRK                   04400000
         STH   R3,TXEHITRK              UPPER LIMIT IN TRACKS           04410000
         SPACE                                                          04420000
         SR    R3,R2                                                    04430000
         LA    R3,1(,R3)                                                04440000
         STH   R3,TXEXTLTH              TOTAL TRK'S = UPPER - LOWER + 1 04450000
         MVC   TXEXTNO,ZERO             EXT SEQ NO. = 0                 04460000
         OI    TXERPTNO,X'80'           FOR REPORT 2                    04470000
         L     R1,ASORT22               WRITE TO FILE SORTIN            04480000
         PUT   (1),LINE                                                 04490000
         EJECT                                                          04500000
*        *------------------------------------------*                   04510000
*        *  OBTAIN FMT 5 DSCB - SET OS FREE EXTENT  *                   04520000
*        *------------------------------------------*                   04530000
         SPACE                                                          04540000
         IC    R1,F45R                  1ST DSCB OF VTOC IS FMT 4 DSCB  04550000
         LA    R1,1(,R1)                2ND DSCB OF VTOC IS FMT 5 DSCB  04560000
         STC   R1,F45R                  GET READY TO OBTAIN             04570000
         SPACE                                                          04580000
FMT5     EQU   *                                                        04590000
         USING DSCB5,R8                                                 04600000
         OBTAIN F45CMLST                                                04610000
         SPACE                                                          04620000
         LTR   R15,R15                  IS OBTAIN SUCCESSFUL?           04630000
         BNZ   NOOSFS                   NO - SKIP ALL OS EXT'S          04640000
         CLI   DS5KEYID,X'05'           MAKE SURE                       04650000
         BNE   NOOSFS                   THIS IS A                       04660000
         CLI   DS5FMTID,X'F5'           VALID                           04670000
         BNE   NOOSFS                   FMT 5 DSCB                      04680000
         SPACE                                                          04690000
         MVC   TXEXTNM,OSFSNAM          * * *  OS FREE SPACE  * * *     04700000
         LA    R7,DS5AVEXT              SKIP KEY IDENTIFICATION         04710000
         LA    R2,8                     PROCESS THE FIRST 8 EXTENTS     04720000
NXTF5EX1 EQU   *                                                        04730000
         CLC   0(5,R7),ZERO             IS THIS A GOOD EXTENT?          04740000
         BE    LASTF5EX                 NO - WRAP IT UP THEN            04750000
         BAL   R11,FREEEXT              YES - PROCESS THIS FREE EXTENT  04760000
         LA    R7,5(,R7)                NEXT FREE EXTENT, PLEASE.       04770000
         BCT   R2,NXTF5EX1                                              04780000
         SPACE                                                          04790000
         LA    R7,DS5MAVET              SKIP FMT 5 IDENTIFIER           04800000
         LA    R2,18                    PROCESS THE NEXT 18 EXTENTS     04810000
NXTF5EX2 EQU   *                                                        04820000
         CLC   0(5,R7),ZERO             IS THIS A GOOD EXTENT?          04830000
         BE    LASTF5EX                 NO - WRAP IT UP THEN            04840000
         BAL   R11,FREEEXT              YES - PROCESS THIS FREE EXTENT  04850000
         LA    R7,5(,R7)                NEXT FREE EXTENT, PLEASE.       04860000
         BCT   R2,NXTF5EX2                                              04870000
         SPACE                                                          04880000
LASTF5EX EQU   *                                                        04890000
         CLC   DS5PTRDS,ZERO            IS THERE ANOTHER FMT 5 DSCB?    04900000
         BZ    SETVCHR                  NO - ALL DONE WITH OS FREE EXT  04910000
         SPACE                                                          04920000
         MVC   F45CCHHR,DS5PTRDS        ADDR OF TNE NEXT FMT 5 DSCB     04930000
         B     FMT5                     DO IT AGAIN                     04940000
         DROP  R8                                                       04950000
         EJECT                                                          04960000
*        *--------------------------------------*                       04970000
*        *  OS FREE SPACE INFO'S NOT AVAILABLE  *                       04980000
*        *--------------------------------------*                       04990000
         SPACE                                                          05000000
NOOSFS   EQU   *                                                        05010000
         MVC   OSFSNUM,FFFF             OS SPACE INFO'S NOT AVAILABLE   05020000
         MVC   MSG01VL(L'TXEVOLNO),TXEVOLNO                             05030000
         SPACE                                                          05040000
         L     R1,AMSGF22               WRITE ERROR MESSAGE             05050000
         PUT   (1),MSG01                                                05060000
         SPACE                                                          05070000
         B     SETVCHR                  SET VOL CHAR                    05080000
         EJECT                                                          05090000
*        *---------------------------------------*                      05100000
*        *  PROCESS FREE EXTENT                  *                      05110000
*        *  IN:   R7 = ADDR OF EXTENT            *                      05120000
*        *  OUT:  WRITE OS FREE EXT TO REPORT 2  *                      05130000
*        *---------------------------------------*                      05140000
         SPACE                                                          05150000
FREEEXT  EQU   *                                                        05160000
         MVC   DFXEXT,0(R7)             ALIGN FREE EXTENT DESCRIPTOR    05170000
         LH    R1,OSFSNUM               SEQ NO. OF OS FREE EXT          05180000
         STH   R1,TXEXTNO                                               05190000
         LA    R1,1(,R1)                BUMP NO. OF OS FREE EXT'S       05200000
         STH   R1,OSFSNUM                                               05210000
         SPACE                                                          05220000
         LH    R1,DFXCYL                NO. OF FREE CYL'S IN THIS EXT   05230000
         LR    R0,R1                    SAVE IT TO CALC LTH IN TRK'S    05240000
         AH    R1,OSFSCYL                                               05250000
         STH   R1,OSFSCYL               TOTAL NO. OF UNUSED CYL'S       05260000
         SPACE                                                          05270000
         MH    R0,TRKCYL                UNUSED CYL'S IN TRK'S           05280000
         SR    R1,R1                                                    05290000
         IC    R1,DFXTRK                ADDITIONAL TRK'S IF ANY         05300000
         AR    R1,R0                                                    05310000
         STH   R1,TXEXTLTH              TOTAL NO. OF TRK'S IN THIS EXT  05320000
         CH    R1,OSFSLGE               IS THIS THE LARGEST EXT SO FAR? 05330000
         BNH   NOTOSLGE                 NO - SKIP IT THEN               05340000
         STH   R1,OSFSLGE               YES - SWAP IT                   05350000
         SPACE                                                          05360000
NOTOSLGE EQU   *                                                        05370000
         AH    R1,OSFSTRK                                               05380000
         STH   R1,OSFSTRK               TOTAL NO. OF UNUSED TRK'S       05390000
         EJECT                                                          05400000
*        *------------------------------------*                         05410000
*        *  FORMAT THE OS FREE SPACE EXTENTS  *                         05420000
*        *------------------------------------*                         05430000
         SPACE                                                          05440000
         LH    R0,TRKCYL                NO. OF TRK'S PER CYL            05450000
         LH    R15,DFXRTA               BEGINNING ADDR IN RELATIVE TRK  05460000
         STH   R15,TXELOTRK                                             05470000
         SR    R14,R14                                                  05480000
         DR    R14,R0                   CONVERT TT TO CCHH              05490000
         STH   R15,TXELOCC              CYL                             05500000
         STH   R14,TXELOHH              TRK                             05510000
         SPACE                                                          05520000
         LH    R15,DFXRTA               BEGINNING OF EXTENT             05530000
         AH    R15,TXEXTLTH             PLUS ITS LENGTH                 05540000
         BCTR  R15,0                    LESS ONE FOR THIS EXTENT'S END  05550000
         STH   R15,TXEHITRK                                             05560000
         SR    R14,R14                                                  05570000
         DR    R14,R0                   CONVERT TT TO CCHH              05580000
         STH   R15,TXEHICC              CYL                             05590000
         STH   R14,TXEHIHH              TRK                             05600000
         SPACE                                                          05610000
         OI    TXERPTNO,X'80'           FOR REPORT 2                    05620000
         L     R1,ASORT22               WRITE TO FILE SORTIN            05630000
         PUT   (1),LINE                                                 05640000
         SPACE                                                          05650000
         BR    R11                      RETURN                          05660000
         EJECT                                                          05670000
*        *-------------------------*                                    05680000
*        *  SET VOL CHAR'S INFO'S  *                                    05690000
*        *-------------------------*                                    05700000
         SPACE                                                          05710000
SETVCHR  EQU   *                                                        05720000
         DROP  R10                                                      05730000
         USING TXVLCHR,R10                                              05740000
         MVC   TXVRCKEY,ZERO            CLEAR SORT KEY                  05750000
         SR    R1,R1                                                    05760000
         IC    R1,VOLSTAB               VOLUME USE (UCBSTAB)            05770000
         STH   R1,TXVOLUSE              FIRST BYTE NOT USED             05780000
         SPACE                                                          05790000
         USING VOLDVCHR,R1                                              05800000
         L     R1,CTLGFLD3+20           ADDR OF VOLDVCHR                05810000
         MVC   TXVDVTYP,VOLDVTYP                                        05820000
         MVC   TXVCYLVL,VOLDNCYL                                        05830000
         MVC   TXVTRKCL,VOLDNTRK                                        05840000
         MVC   TXVBYTTK,VOLDNBYT                                        05850000
         DROP  R1                                                       05860000
         SPACE                                                          05870000
         L     R1,CTLGFLD6+20           ADDR OF NODSET                  05880000
         MVC   TXVNODST,0(R1)                                           05890000
         L     R1,CTLGFLD5+20           ADDR OF NODSPACE                05900000
         MVC   TXVNODSP,0(R1)                                           05910000
         L     R1,CTLGFLD4+20           ADDR OF SYSEXTDS                05920000
         MVI   TXVSYSXT,X'00'                                           05930000
         MVC   TXVSYSXT+1(1),0(R1)                                      05940000
         SPACE                                                          05950000
         L     R1,ACATN22               ADDR OF CATLG NAME              05960000
         MVC   TXVCATNM,0(R1)                                           05970000
         SPACE                                                          05980000
         MVC   TXVVSEXT,VSFSNUM         NO. OF VSAM FREE EXT'S          05990000
         MVC   TXVVSTRK,VSFSTRK         NO. OF VSAM FREE TRK'S          06000000
         MVC   TXVVSLGE,VSFSLGE         LTH OF LARGEST VSAM FREE EXT    06010000
         SPACE                                                          06020000
         MVC   TXVOSEXT,OSFSNUM         NO. OF OS FREE EXT'S            06030000
         MVC   TXVOSTRK,OSFSTRK         NO. OF OS FREE TRK'S            06040000
         MVC   TXVOSCYL,OSFSCYL         NO. OF OS FREE CYL'S            06050000
         MVC   TXVOSLGE,OSFSLGE         LTH OF LARGEST OS FREE EXT      06060000
         EJECT                                                          06070000
*        *---------------------------------------------------*          06080000
*        *  WRITE OUT DETAIL VOL INFO'S & REPORT SEPARATORS  *          06090000
*        *---------------------------------------------------*          06100000
         SPACE                                                          06110000
         L     R1,ASORT22                                               06120000
         PUT   (1),LINE                 FOR REPORT 1                    06130000
         SPACE                                                          06140000
         OI    TXVRPTNO,X'80'                                           06150000
         L     R1,ASORT22                                               06160000
         PUT   (1),LINE                 REPORT 2                        06170000
         SPACE                                                          06180000
         MVI   TXVRCKEY,X'FF'           LAST RECORD FOR REPORT 2        06190000
         MVC   TXVRCKEY+1(L'LINE-L'TXVVOLNO-1),TXVRCKEY                 06200000
         L     R1,ASORT22                                               06210000
         PUT   (1),LINE                 REPORT 2                        06220000
         SPACE                                                          06230000
         NI    TXVRPTNO,X'7F'           LAST RECORD FOR REPORT 1        06240000
         L     R1,ASORT22                                               06250000
         PUT   (1),LINE                 REPORT 1                        06260000
         SPACE                                                          06270000
         MVI   LINE,X'00'               CLEAR LINE                      06280000
         MVC   LINE+1(L'LINE-1),LINE                                    06290000
         B     GOHOME                   AND RETURN                      06300000
         EJECT                                                          06310000
*        *************************************************************  06320000
*        *                                                           *  06330000
*        *  TYPE 2 - SET DETAIL INFO'S FOR TYPE 'C' & 'G' & ASSOC'S  *  06340000
*        *                                                           *  06350000
*        *************************************************************  06360000
         SPACE                                                          06370000
TYPE2    EQU  *                                                         06380000
         DROP  R10                                                      06390000
         USING TXDTAIL,R10                                              06400000
         MVC   CTGECAT,CTGBCAT          ADDR OF CATACB                  06410000
         MVC   CTGEWKB,AWORK22          ADDR OF WORK AREA 'B'           06420000
         MVC   CTGEPSWD,CTGBPSWD        ADDR OF PASSWORD                06430000
         L     R1,RPT0KEY               SET KEY FOR FLD TXDRCKEY        06440000
         LA    R1,1(,R1)                                                06450000
         ST    R1,RPT0KEY                                               06460000
         SPACE                                                          06470000
         SR    R1,R1                                                    06480000
         STH   R1,RPT0VLN               RESET NO. OF VOL'S ALLOCATED    06490000
         MVI   RPT0VLS,X'40'            CLEAR ALL 16 VOL SERIAL NUMBERS 06500000
         MVC   RPT0VLS+1(16*L'RPT0VLS-1),RPT0VLS                        06510000
         SPACE 2                                                        06520000
         USING NMDSECT,R8                                               06530000
         L     R1,CTGBAFL4              ADDR OF CTLGFL-B4               06540000
         L     R0,16(,R1)               LENGTH OF NAMEDS(ES)            06550000
         L     R8,20(,R1)               ADDR OF NAMEDS(ES)              06560000
         AR    R0,R8                    ADDR OF END OF NAMEDS(ES)       06570000
         ST    R0,EONMDS                UPPER LIMIT                     06580000
         SPACE                                                          06590000
NEXTNMDS EQU   *                                                        06600000
         LH    R1,RPT0SEQ               INCREMENT RPT0SEQ               06610000
         LA    R1,1(,R1)                                                06620000
         STH   R1,RPT0SEQ                                               06630000
         SPACE                                                          06640000
         LA    R1,NMDSCIN               ADDR OF CI NUMBER               06650000
         ST    R1,CTGEENT               SET CTLGPL-E                    06660000
         MVC   CTGERCTP,NMDSTYP         ENTRY TYPE                      06670000
         SPACE                                                          06680000
         CLI   NMDSTYP,C'D'             IS IT FOR A TYPE 'D' ENTRY?     06690000
         BE    SETVLXT                  YES - SET VOL'S & EXT'S         06700000
         CLI   NMDSTYP,C'I'             IS IT FOR A TYPE 'I' ENTRY?     06710000
         BE    SETVLXT                  YES - SET VOL'S & EXT'S         06720000
         SPACE                                                          06730000
*        *--------------------------------------------------------*     06740000
*        *  ASSUMING THE ORDER OF NAMEDS AS FOLLOWS, D, I, G, R.  *     06750000
*        *  THE ENTVOL FLD'S OF TYPES 'D' AND 'I' DETERMINE THE   *     06760000
*        *  VOL'S W/ ALLOC FOR THE OTHER ENTRIES.                 *     06770000
*        *--------------------------------------------------------*     06780000
         EJECT                                                          06790000
*        *----------------------------------------------------------*   06800000
*        *  NAMEDS OF ASSOCIATED ENTRY - NEITHER TYPE 'D' NOR 'I'   *   06810000
*        *  WRITE THIS ENTRY'S NAME AND TYPE FOR EACH VOL W/ ALLOC  *   06820000
*        *----------------------------------------------------------*   06830000
         SPACE                                                          06840000
         LH    R2,RPT0VLN               NO. OF VOL'S W/ ALLOC           06850000
         LTR   R2,R2                    ARE THERE ANY VOL'S?            06860000
         BZ    GOHOME                   NO - QUIT FOR THIS 'C' OR 'G'   06870000
         SPACE                                                          06880000
         LA    R3,RPT0VLS               1ST VOL W/ ALLOC                06890000
         MVI   CTGEFLDN,X'02'           ENTYPE & ENTNAME ONLY           06900000
         LA    R1,CTLGPLE                                               06910000
         SVC   26                                                       06920000
         SPACE                                                          06930000
         LTR   R15,R15                  IS SVC SUCCESSFUL?              06940000
         BZ    SVCOK222                 YES - CONTINUE                  06950000
         ABEND 222,DUMP                                                 06960000
         SPACE                                                          06970000
SVCOK222 EQU   *                                                        06980000
         MVI   TXDDSORG,X'FF'           INDICATE AN ASSOCIATED ENTRY    06990000
         L     R1,CTLGFLE1+20           ENTYPE                          07000000
         MVC   TXDENTYP,0(R1)                                           07010000
         L     R1,CTLGFLE2+20           ENTNAME                         07020000
         MVC   TXDENTNM,0(R1)                                           07030000
         MVC   TXDRPTNO(L'TXDRCKEY+L'TXDRCSEQ),RPT0KEY                  07040000
         SPACE                                                          07050000
NEXTNCG  EQU   *                                                        07060000
         MVC   TXDVOLNO,0(R3)           VOLUME SERIAL NUMBER            07070000
         L     R1,ASORT22                                               07080000
         PUT   (1),LINE                                                 07090000
         SPACE                                                          07100000
         LA    R3,6(,R3)                NEXT VOLUME PLEASE              07110000
         BCT   R2,NEXTNCG               FOR THIS NON-'C' OR NON-'G' ENT 07120000
         SPACE                                                          07130000
         B     BUMPNMDS                 NEXT NAMEDS FLD                 07140000
         EJECT                                                          07150000
*        *----------------------------------------------------*         07160000
*        *  NAMEDS OF TYPE 'D' & 'I' - NEED MORE THAN A NAME  *         07170000
*        *  SET INFO'S OF EXTENTS AND VOLUMES                 *         07180000
*        *----------------------------------------------------*         07190000
         SPACE                                                          07200000
SETVLXT  EQU   *                                                        07210000
         MVI   CTGEFLDN,X'09'           ALL THE INFO'S                  07220000
         LA    R1,CTLGPLE                                               07230000
         SVC   26                                                       07240000
         SPACE                                                          07250000
         LTR   R15,R15                  IS SVC SUCCESSFUL?              07260000
         BZ    SVCOK223                 YES - CONTINUE                  07270000
         ABEND 223,DUMP                                                 07280000
         SPACE                                                          07290000
SVCOK223 EQU   *                                                        07300000
         USING ETVOL,R7                                                 07310000
         L     R0,CTLGFLE7+16           LENGTH OF ENTVOL(S)             07320000
         L     R7,CTLGFLE7+20           ADDR OF ENTVOL(S)               07330000
         AR    R0,R7                    ADDR OF END OF ENTVOL(S)        07340000
         ST    R0,EOENTV                UPPER LIMIT                     07350000
         SPACE                                                          07360000
NEXTENTV EQU   *                                                        07370000
         SR    R1,R1                                                    07380000
         LA    R6,ETVLOKEY              SET BASE REG FOR DSECT ETXDSCRP 07390000
         ICM   R1,3,0(R6)                                               07400000
         LA    R6,2(R1,R6)              BUMP LO KEY                     07410000
         ICM   R1,3,0(R6)                                               07420000
         LA    R6,2(R1,R6)              BUMP HI KEY                     07430000
         ICM   R1,3,0(R6)               R1 = ETVXTLTH                   07440000
         LA    R6,2(,R6)                R6 = A(ETVXTENT)                07450000
         LTR   R1,R1                    ARE THERE ANY EXTENTS?          07460000
         BNZ   HAVEXTS                  YES - PROCESS THEM              07470000
         ST    R6,NXENTV                NO - R6 HAS ADDR OF NEXT ENTVOL 07480000
         B     BUMPENTV                 PROCESS NEXT ENTVOL             07490000
         SPACE                                                          07500000
HAVEXTS  EQU   *                                                        07510000
         AR    R1,R6                    R1 HAS ADDR OF NEXT ENTVOL      07520000
         ST    R1,NXENTV                                                07530000
         TM    ETVOLFLG,X'80'           IS THIS A PRIME VOLUME?         07540000
         BZ    BUMPENTV                 NO - SKIP IT                    07550000
         TM    ETVITYPE,X'80'           IS INDEX'ES SEQ SET W/ DATA?    07560000
         BO    BUMPENTV                 YES - SKIP IT                   07570000
         EJECT                                                          07580000
*        *-----------------------------------------------------------*  07590000
*        *  THIS VOL'S LOOKIN' GOOD - ADD IT TO THE LIST OF VOLUMES  *  07600000
*        *-----------------------------------------------------------*  07610000
         SPACE                                                          07620000
         LH    R2,RPT0VLN               GET NUMBER OF VOLUMES ON LIST   07630000
         LA    R3,RPT0VLS               GET ADDR OF LIST OF VOLUMES     07640000
         SPACE                                                          07650000
CHKRPT0V EQU   *                                                        07660000
         CLI   0(R3),X'40'              IS THIS THE END OF LIST?        07670000
         BE    ADDRPT0V                 YES - ADD A NEW ONE             07680000
         CLC   ETVOLSER,0(R3)           IS THIS VOLUME ON LIST ALREADY? 07690000
         BE    DONTADDV                 YES - DON'T ADD TO THE LIST     07700000
         LA    R3,L'RPT0VLS(,R3)        BUMP TO NEXT VOLUME ON LIST     07710000
         BCT   R2,CHKRPT0V              AND KEEP CHECKING               07720000
         SPACE                                                          07730000
ADDRPT0V EQU   *                                                        07740000
         MVC   0(L'ETVOLSER,R3),ETVOLSER                                07750000
         LH    R2,RPT0VLN                                               07760000
         LA    R2,1(,R2)                                                07770000
         STH   R2,RPT0VLN               NEW VOLUME ADDED TO LIST        07780000
DONTADDV EQU   *                                                        07790000
         SPACE 2                                                        07800000
*        *-----------------------------*                                07810000
*        *  NOW CHECK OUT ITS EXTENTS  *                                07820000
*        *-----------------------------*                                07830000
         SPACE                                                          07840000
         USING ETXDSCRP,R6                                              07850000
         MVC   TXDVOLNO,ETVOLSER        SAME AS SETTING TXEVOLNO        07860000
         MVC   TXDRCSEQ,ONE             SAME AS SETTING TXERCKEY        07870000
         L     R1,CTLGFLE2+20           ADDR OF ENTNAME                 07880000
         MVC   TXDENTNM,0(R1)           SAME AS SETTING TXENTNM         07890000
         MVC   EXTXNUM(4),ZERO          CLEAR EXTXNUM & EXTXTRK         07900000
         MVC   EXTXLOR,ETXLORBA         LOW RBA OF THE 1ST EXTENT       07910000
         SPACE                                                          07920000
NEXTETXD EQU   *                                                        07930000
         BAL   R11,SETEXTS                                              07940000
         LA    R6,L'ETVXTENT(,R6)                                       07950000
         C     R6,NXENTV                                                07960000
         BL    NEXTETXD                                                 07970000
         SPACE                                                          07980000
         BAL   R11,SETDTLS              ALL EXT'S DONE, SET UP THE REST 07990000
         SPACE 2                                                        08000000
BUMPENTV EQU   *                                                        08010000
         L     R7,NXENTV                ADDR OF NEXT ENTVOL             08020000
         C     R7,EOENTV                IS IT STILL WITHIN LIMIT?       08030000
         BL    NEXTENTV                 YES - KEEP ON KEEPING ON        08040000
         B     BUMPNMDS                 NO - ALL DONE WITH THIS ENTRY   08050000
         SPACE 2                                                        08060000
EOENTV   DC    F'-1'                                                    08070000
NXENTV   DC    F'-1'                                                    08080000
         EJECT                                                          08090000
*        *------------------------------------------------------*       08100000
*        *  BUMP TO NEXT NAMEDS FIELD AND CONTINUE PROCESSING.  *       08110000
*        *  IF ALL DONE, DO THE TYPE 'C' OR 'G' AND QUIT THEN.  *       08120000
*        *------------------------------------------------------*       08130000
         SPACE                                                          08140000
BUMPNMDS EQU   *                                                        08150000
         LA    R8,4(,R8)                ADD LENGTH OF NAMEDS            08160000
         C     R8,EONMDS                IS IT STILL WITHIN LIMIT?       08170000
         BL    NEXTNMDS                 YES - KEEP ON KEEPING ON        08180000
         SPACE                                                          08190000
         LH    R2,RPT0VLN               NO. OF VOL'S W/ ALLOC           08200000
         LTR   R2,R2                    ARE THERE ANY VOL'S?            08210000
         BZ    GOHOME                   NO - QUIT FOR THIS 'C' OR 'G'   08220000
         SPACE                                                          08230000
         LA    R3,RPT0VLS               1ST VOL W/ ALLOC                08240000
         MVC   TXDDSORG,ENTORG          AMDATTR                         08250000
         L     R1,CTGBAFL5                                              08260000
         L     R1,20(,R1)                                               08270000
         MVC   TXDEXPDT,0(R1)           DSETEXDT                        08280000
         L     R1,CTGBAFL6                                              08290000
         L     R1,20(,R1)                                               08300000
         MVC   TXDCREDT,0(R1)           DSETCRDT                        08310000
         SPACE                                                          08320000
         L     R1,CTGBAFL1                                              08330000
         L     R1,20(,R1)                                               08340000
         MVC   TXDENTYP,0(R1)           ENTYPE                          08350000
         L     R1,CTGBAFL2                                              08360000
         L     R1,20(,R1)                                               08370000
         MVC   TXDENTNM,0(R1)           ENTNAME                         08380000
         MVC   RPT0SEQ,ZERO             RESET SEQ NO. TO ZERO           08390000
         MVC   TXDRPTNO(L'TXDRCKEY+L'TXDRCSEQ),RPT0KEY                  08400000
         SPACE                                                          08410000
NEXTCG   EQU   *                                                        08420000
         MVC   TXDVOLNO,0(R3)           VOLUME SERIAL NUMBER            08430000
         L     R1,ASORT22                                               08440000
         PUT   (1),LINE                                                 08450000
         SPACE                                                          08460000
         LA    R3,6(,R3)                NEXT VOLUME PLEASE              08470000
         BCT   R2,NEXTCG                FOR THIS 'C' OR 'G' ENT         08480000
         SPACE                                                          08490000
         MVI   LINE,X'00'               CLEAR LINE                      08500000
         MVC   LINE+1(L'LINE-1),LINE                                    08510000
         B     GOHOME                                                   08520000
         SPACE 2                                                        08530000
EONMDS   DC    F'-1'                                                    08540000
         EJECT                                                          08550000
*        *-----------------------------------------*                    08560000
*        *  SET EXTENTS' INFO'S                    *                    08570000
*        *  IN:    R6 POINTS TO EXTENT DESCRIPTOR  *                    08580000
*        *  FUNC:  FORMAT, ACCUMULATE AND WRITE    *                    08590000
*        *  OUT:   NONE                            *                    08600000
*        *-----------------------------------------*                    08610000
         SPACE                                                          08620000
SETEXTS  EQU   *                                                        08630000
         DROP  R10                                                      08640000
         USING TXEXTNT,R10                                              08650000
         MVC   TXELOTCH,ETXLOTRK        SET SORT KEY (LO TRK ADDR)      08660000
         OI    TXERPTNO,X'80'           FOR REPORT 2                    08670000
         MVC   TXEHITCH,ETXHITRK        HI TRK ADDR OF EXT              08680000
         LH    R1,ETXNOTRK                                              08690000
         STH   R1,TXEXTLTH              LENGTH OF EXT                   08700000
         AH    R1,EXTXTRK               TOTAL TRK'S ON THIS VOL         08710000
         STH   R1,EXTXTRK               ALLOCATED                       08720000
         SPACE                                                          08730000
         LH    R1,EXTXNUM               NUMBER OF EXT                   08740000
         STH   R1,TXEXTNO               SEQ NO OF EXT                   08750000
         LA    R1,1(,R1)                BUMP IT BY ONE                  08760000
         STH   R1,EXTXNUM                                               08770000
         SPACE                                                          08780000
         L     R1,ASORT22                                               08790000
         PUT   (1),LINE                                                 08800000
         SPACE                                                          08810000
         BR    R11                      RETURN                          08820000
         EJECT                                                          08830000
*        *-----------------------------------------*                    08840000
*        *  SET DETAILS' INFO'S                    *                    08850000
*        *  IN:    R7 POINTS TO ENTVOL STILL       *                    08860000
*        *  FUNC:  FORMAT THE REMAINING AND WRITE  *                    08870000
*        *  OUT:   NONE                            *                    08880000
*        *-----------------------------------------*                    08890000
         SPACE                                                          08900000
SETDTLS  EQU   *                                                        08910000
         DROP  R10                                                      08920000
         USING TXDTAIL,R10                                              08930000
         MVC   TXDRPTNO(L'TXDRCKEY+L'TXDRCSEQ),RPT0KEY                  08940000
         L     R1,CTLGFLE1+20           ADDR OF ENTYPE                  08950000
         MVC   TXDENTYP,0(R1)                                           08960000
         L     R1,CTLGFLE4+20           ADDR OF DSETCRDT                08970000
         MVC   TXDCREDT,0(R1)                                           08980000
         L     R1,CTLGFLE3+20           ADDR OF DSETEXDT                08990000
         MVC   TXDEXPDT,0(R1)                                           09000000
         L     R1,CTLGFLE5+20           ADDR OF CRAVOL                  09010000
         MVC   TXDCRAVL,0(R1)                                           09020000
         SPACE                                                          09030000
         USING SPACPR,R1                                                09040000
         L     R1,CTLGFLE6+20           ADDR OF SPACPARM                09050000
         MVC   TXDSCALC,SPACSCON                                        09060000
         MVC   TXDSCTYP,SPACOPTN                                        09070000
         DROP  R1                                                       09080000
         SPACE                                                          09090000
         USING AMDSB,R1                                                 09100000
         L     R1,CTLGFLE8+20           ADDR OF AMDSBCAT                09110000
         CLI   NMDSTYP,C'D'             IS THIS TYPE A 'D' ENTRY?       09120000
         BNE   SETAMDS                  NO - MUST BE A TYPE 'I'         09130000
         MVC   ENTORG,AMDATTR           YES - SAVE IT FOR 'CL' OR 'AIX' 09140000
SETAMDS  EQU   *                                                        09150000
         MVC   TXDDSORG,AMDATTR                                         09160000
         MVC   TXDCISIZ,AMDCINV                                         09170000
         MVC   TXDMAXRC,AMDLRECL                                        09180000
         DROP  R1                                                       09190000
         SPACE                                                          09200000
         L     R1,CTLGFLE9+20           ADDR OF LRECL                   09210000
         MVC   TXDAVGRC,0(R1)                                           09220000
         EJECT                                                          09230000
*        *---------------------------------------*                      09240000
*        *   A L L O C A T I O N   I N F O ' S   *                      09250000
*        *---------------------------------------*                      09260000
         SPACE                                                          09270000
         SR    R2,R2                                                    09280000
         ICM   R3,15,ETVHURBA           HIGH-USED RBA IN ENTVOL         09290000
         BNP   NOTUSED                  S/B POSITIVE                    09300000
         ICM   R1,15,ETVHARBA           HIGH-ALLOCATED RBA IN ENTVOL    09310000
         BNP   NOTUSED                  S/B POSITIVE                    09320000
         CLR   R1,R3                    ARE THEY THE SAME?              09330000
         BNH   ALLUSED                  YES - ALL ALLOC'D ARE USED      09340000
         SPACE                                                          09350000
         S     R1,EXTXLOR               ADJUST HARBA                    09360000
         BNP   NOTUSED                  S/B POSITIVE                    09370000
         S     R3,EXTXLOR               ADJUST HURBA                    09380000
         BNP   NOTUSED                  S/B POSITIVE                    09390000
         SPACE                                                          09400000
         LR    R0,R3                    SAVE HURBA FOR A WHILE          09410000
         M     R2,HUNDRED                                               09420000
         DR    R2,R1                    R3 = ( HURBA * 100 ) / HARBA    09430000
         STC   R3,TXDPCTUS              PERCENTAGE OF TRACKS USED       09440000
         SPACE                                                          09450000
         SR    R2,R2                                                    09460000
         LR    R3,R0                    RESTORE HURBA                   09470000
         LH    R0,EXTXTRK               TOTAL NO. OF TRK'S ALLOCATED    09480000
         MR    R2,R0                                                    09490000
         DR    R2,R1                    R3 = ALLOC * ( HURBA / HARBA )  09500000
         LTR   R2,R2                    IS THERE A FRACTION OF A TRACK? 09510000
         BZ    NOEXTRA                  NO - NO NEED TO ROUND IT UP     09520000
         LA    R3,1(,R3)                YES - ONE MORE                  09530000
NOEXTRA  EQU   *                                                        09540000
         STH   R3,TXDTRKUS              TOTAL NO. OF TRK'S USED         09550000
         B     SETALLC                                                  09560000
         EJECT                                                          09570000
*        *-------------------------------------------------------*      09580000
*        *   A L L O C A T I O N   I N F O ' S   -   C O N T .   *      09590000
*        *-------------------------------------------------------*      09600000
         SPACE                                                          09610000
NOTUSED  EQU   *                                                        09620000
         MVI   TXDPCTUS,0               ZERO PERCENTAGE USED            09630000
         STH   R2,TXDTRKUS              R2 S/B ZERO                     09640000
         B     SETALLC                                                  09650000
         SPACE                                                          09660000
ALLUSED  EQU   *                                                        09670000
         MVI   TXDPCTUS,100             A HUNDRED PERCENTS USED         09680000
         MVC   TXDTRKUS,EXTXTRK         TRK'S USED SAME AS ALLOC'D      09690000
         SPACE                                                          09700000
SETALLC  EQU   *                                                        09710000
         MVC   TXDTRKAL,EXTXTRK         NO. OF TRK'S ALLOC'D ON VOL     09720000
         MVC   TXDNOEXT,EXTXNUM+1       NO. OF EXT'S FOR ENT ON VOL     09730000
         L     R1,ASORT22                                               09740000
         PUT   (1),LINE                                                 09750000
         SPACE                                                          09760000
         MVI   LINE,X'00'               CLEAR LINE                      09770000
         MVC   LINE+1(L'LINE-1),LINE                                    09780000
         BR    R11                      RETURN FROM SETDTLS             09790000
         EJECT                                                          09800000
*        ***************                                                09810000
*        *             *                                                09820000
*        *   E X I T   *                                                09830000
*        *             *                                                09840000
*        ***************                                                09850000
         SPACE                                                          09860000
TYPE0    EQU   *                                                        09870000
GOHOME   EQU   *                                                        09880000
         L     R13,MYSAVE+4                                             09890000
         RETURN (14,12),RC=0                                            09900000
         SPACE 2                                                        09910000
         LTORG                                                          09920000
         SPACE                                                          09930000
         DS    0H                                                       09940000
         EJECT                                                          09950000
*        *-----------------------*                                      09960000
*        *   V A R I A B L E S   *                                      09970000
*        *-----------------------*                                      09980000
         SPACE                                                          09990000
RPT0KEY  DC    F'0'                     COUNTER FOR TYPE 'C' & 'G' ENT  10000000
RPT0SEQ  DC    H'0'                     SEQ NO. FOR NAMEDS FLD'S        10010000
RPT0VLN  DC    H'0'                     NO. OF VOL'S W/ ALLOC           10020000
RPT0VLS  DC    16CL6'VOLSER'            MAX OF 16 VOL'S                 10030000
         SPACE                                                          10040000
EXTXLOR  DC    F'0'                     LOWEST RBA IN ENTVOL            10050000
EXTXNUM  DC    H'0'                     NO. OF EXT'S FOR ENT ON VOL     10060000
EXTXTRK  DC    H'0'                     NO. OF TRK'S ALLOC'D FOR ENT/V  10070000
         SPACE                                                          10080000
VSFSNUM  DC    H'0'                     NO. OF VSAM FREE SPACE EXT'S    10090000
VSFSTRK  DC    H'0'                     NO. OF VSAM FREE SPACE TRK'S    10100000
VSFSLGE  DC    H'0'                     NO. OF TRK'S IN LARGEST VSFS    10110000
         SPACE                                                          10120000
OSFSNUM  DC    H'0'                     NO. OF OS FREE SPACE EXT'S      10130000
OSFSTRK  DC    H'0'                     NO. OF OS FREE SPACE TRK'S      10140000
OSFSCYL  DC    H'0'                     NO. OF OS FREE SPACE CYL'S      10150000
OSFSLGE  DC    H'0'                     NO. OF TRK'S IN LARGEST VSFS    10160000
         SPACE                                                          10170000
TRKCYL   DC    H'0'                     NO. OF TRK'S PER CYL            10180000
BEGINTT  DC    H'0'                     BEGINNING IN RELATIVE TRK ADDR  10190000
BEGINRCH DS    0CL4                     BEGINNING ADDR IN CCHH          10200000
BEGINCC  DC    H'0'                     BEGINNING CYL                   10210000
BEGINHH  DS    H'0'                     BEGINNING TRK                   10220000
         SPACE                                                          10230000
ENTYPE   DC    X'40'                    ENTRY TYPE                      10240000
ENTORG   DC    X'00'                    ENTRY ORGANIZATION              10250000
VOLSTAB  DC    X'00'                    VOLUME USE (UCBSTAB)            10260000
         DS    0H                                                       10270000
LINE     DC    XL92'00'                                                 10280000
         EJECT                                                          10290000
*        *---------------------------------------------------------*    10300000
*        *   T O   O B T A I N   F M T   4   &   5   D S C B ' S   *    10310000
*        *---------------------------------------------------------*    10320000
         SPACE                                                          10330000
F45CMLST CAMLST SEEK,F45CCHHR,0,F45DSCB                                 10340000
         SPACE                                                          10350000
         ORG   F45CMLST+8                                               10360000
F45VOLNO DS    A                                                        10370000
         ORG                                                            10380000
         SPACE                                                          10390000
F45CCHHR DS    0CL5                                                     10400000
F45CC    DC    H'0'                                                     10410000
F45HH    DC    H'0'                                                     10420000
F45R     DC    X'00'                                                    10430000
         DS    0H                                                       10440000
         SPACE                                                          10450000
DFXEXT   DS    0CL5                                                     10460000
DFXRTA   DC    H'0'                     RELATIVE TRK ADDR OF FREE EXT   10470000
DFXCYL   DC    H'0'                     NO. OF UNUSED CYL'S IN FREE EXT 10480000
DFXTRK   DC    X'00'                    NO. OF UNUSED TRK'S BESIDES CYL 10490000
         SPACE                                                          10500000
F45DSCB  DS    0D                                                       10510000
         DC    CL148' '                                                 10520000
         EJECT                                                          10530000
*        *-----------------------*                                      10540000
*        *   C O N S T A N T S   *                                      10550000
*        *-----------------------*                                      10560000
         SPACE                                                          10570000
RECTBL   DC    256X'00'                 TYPE 0 - UNDEFINED              10580000
         ORG   RECTBL+C'C'                                              10590000
         DC    X'01'                    TYPE 2 - CLUSTER                10600000
         ORG   RECTBL+C'G'                                              10610000
         DC    X'02'                    TYPE 2 - ALERNATE INDEX         10620000
         ORG   RECTBL+C'V'                                              10630000
         DC    X'03'                    TYPE 1 - SPACE                  10640000
         ORG                                                            10650000
         SPACE 2                                                        10660000
HEXNUM   DC    CL16'0123456789ABCDEF'                                   10670000
HEXTBL   EQU   HEXNUM-240                                               10680000
         SPACE                                                          10690000
ZERO     DC    D'0'                                                     10700000
ONE      DC    H'1'                                                     10710000
THREE    DC    H'3'                                                     10720000
FFFF     DC    H'-1'                                                    10730000
HUNDRED  DC    F'100'                                                   10740000
VSFSNAM  DC    CL44'* * *  VSAM FREE SPACE  * * *'                      10750000
OSFSNAM  DC    CL44'* * *  OS FREE SPACE  * * *'                        10760000
VTOCNAM  DC    CL44'* * *  VTOC  * * *'                                 10770000
         SPACE                                                          10780000
MSG01    DC    X'40',CL132'VSAMLST4---01 OS FREE SPACE INFORMATION NOT X10790000
               AVAILABLE FOR VOLUME XXXXXX'                             10800000
MSG01VL  EQU   MSG01+65                                                 10810000
         SPACE 2                                                        10820000
*        *-------------------------*                                    10830000
*        *   P A R M   L I S T S   *                                    10840000
*        *-------------------------*                                    10850000
         SPACE                                                          10860000
PARM9422 DS    0F                                                       10870000
ACATN22  DC    A(0)                                                     10880000
ACTPL22  DC    A(0)                                                     10890000
AMSGF22  DC    A(0)                                                     10900000
ASORT22  DC    A(0)                                                     10910000
AWORK22  DC    A(0)                                                     10920000
         EJECT                                                          10930000
*        *---------------------------------------------*                10940000
*        *   C T L G P L   A N D   C T G F L   -   D   *                10950000
*        *---------------------------------------------*                10960000
         SPACE                                                          10970000
CTLGPLD  DS    0D                                                       10980000
CTGDOPT1 DC    X'44008108'                                              10990000
*  CHECK THE MASTER PASSWORD                                            11000000
*  CTGENT CONTAINS ADDR OF ENTRY NAME                                   11010000
*  A CATALOG MANAGEMENT SERVICES FUNCTION                               11020000
*  THE CALL IS A VSAM CATALOG MANAGEMENT REQUEST                        11030000
*  BYPASS SECURITY PROMPTING TO SYSTEM OPERATOR                         11040000
*                                                                       11050000
CTGDENT  DC    A(0)                                                     11060000
CTGDCAT  DC    A(0)                     ADDR OF CATACB                  11070000
CTGDWKB  DC    A(0)                     ADDR OF WORK AREA 'B'           11080000
CTGDOPT2 DC    X'2000E507'                                              11090000
CTGDDDNM DC    A(0)                                                     11100000
CTGDPSWD DC    A(0)                     ADDR OF PASSWORD                11110000
CTGDFLDS DS    0F                                                       11120000
         DC    A(CTLGFLD1)                                              11130000
         DC    A(CTLGFLD2)                                              11140000
         DC    A(CTLGFLD3)                                              11150000
         DC    A(CTLGFLD4)                                              11160000
         DC    A(CTLGFLD5)                                              11170000
         DC    A(CTLGFLD6)                                              11180000
         DC    A(CTLGFLD7)                                              11190000
         SPACE 2                                                        11200000
CTGFLD   DS    0D                                                       11210000
         SPACE                                                          11220000
CTLGFLD1 DC    X'01',AL3(0),F'0'                                        11230000
         DC    A(REQUES02)              ENTYPE    (LTH = 1)             11240000
         DC    3F'0'                                                    11250000
CTLGFLD2 DC    X'01',AL3(0),F'0'                                        11260000
         DC    A(REQUES03)              ENTNAME   (LTH = 44)            11270000
         DC    3F'0'                                                    11280000
CTLGFLD3 DC    X'01',AL3(0),F'0'                                        11290000
         DC    A(REQUES11)              VOLDVCHR  (LTH = 20)            11300000
         DC    3F'0'                                                    11310000
CTLGFLD4 DC    X'01',AL3(0),F'0'                                        11320000
         DC    A(REQUES18)              SYSEXTDS  (LTH = 1)             11330000
         DC    3F'0'                                                    11340000
CTLGFLD5 DC    X'01',AL3(0),F'0'                                        11350000
         DC    A(REQUES19)              NODSPACE  (LTH = 2)             11360000
         DC    3F'0'                                                    11370000
CTLGFLD6 DC    X'01',AL3(0),F'0'                                        11380000
         DC    A(REQUES20)              NODSET    (LTH = 2)             11390000
         DC    3F'0'                                                    11400000
CTLGFLD7 DC    X'01',AL3(0),F'0'                                        11410000
         DC    A(REQUES23)              DSPDSCRP  (LTH = (15+))         11420000
         DC    3F'0'                                                    11430000
         EJECT                                                          11440000
*        *-------------------------*                                    11450000
*        *   C T L G P L   -   E   *                                    11460000
*        *-------------------------*                                    11470000
         SPACE                                                          11480000
CTLGPLE  DS    0D                                                       11490000
CTGEOPT1 DC    X'40008108'                                              11500000
*  CHECK THE MASTER PASSWORD                                            11510000
*  A CATALOG MANAGEMENT SERVICES FUNCTION                               11520000
*  THE CALL IS A VSAM CATALOG MANAGEMENT REQUEST                        11530000
*  BYPASS SECURITY PROMPTING TO SYSTEM OPERATOR                         11540000
*                                                                       11550000
CTGEENT  DC    A(0)                     ADDR OF CI NUMBER               11560000
CTGECAT  DC    A(0)                     ADDR OF CATACB                  11570000
CTGEWKB  DC    A(0)                     ADDR OF WORK AREA 'B'           11580000
CTGEOPT2 DC    X'2000C409'                                              11590000
         ORG   CTGEOPT2+2                                               11600000
CTGERCTP DS    CL1                                                      11610000
CTGEFLDN DS    CL1                                                      11620000
         ORG                                                            11630000
CTGEDDNM DC    A(0)                                                     11640000
CTGEPSWD DC    A(0)                     ADDR OF PASSWORD                11650000
CTGEFLDS DS    0F                                                       11660000
         DC    A(CTLGFLE1)                                              11670000
         DC    A(CTLGFLE2)                                              11680000
         DC    A(CTLGFLE3)                                              11690000
         DC    A(CTLGFLE4)                                              11700000
         DC    A(CTLGFLE5)                                              11710000
         DC    A(CTLGFLE6)                                              11720000
         DC    A(CTLGFLE7)                                              11730000
         DC    A(CTLGFLE8)                                              11740000
         DC    A(CTLGFLE9)                                              11750000
         EJECT                                                          11760000
*        *-----------------------*                                      11770000
*        *   C T G F L   -   E   *                                      11780000
*        *-----------------------*                                      11790000
         SPACE                                                          11800000
CTGFLE   DS    0D                                                       11810000
         SPACE                                                          11820000
CTLGFLE1 DC    X'01',AL3(0),F'0'                                        11830000
         DC    A(REQUES02)              ENTYPE    (LTH = 1)             11840000
         DC    3F'0'                                                    11850000
CTLGFLE2 DC    X'01',AL3(0),F'0'                                        11860000
         DC    A(REQUES03)              ENTNAME   (LTH = 44)            11870000
         DC    3F'0'                                                    11880000
CTLGFLE3 DC    X'01',AL3(0),F'0'                                        11890000
         DC    A(REQUES06)              DSETEXDT  (LTH = 3)             11900000
         DC    3F'0'                                                    11910000
CTLGFLE4 DC    X'01',AL3(0),F'0'                                        11920000
         DC    A(REQUES07)              DSETCRDT  (LTH = 3)             11930000
         DC    3F'0'                                                    11940000
CTLGFLE5 DC    X'01',AL3(0),F'0'                                        11950000
         DC    A(REQUES09)              CRAVOL    (LTH = 6)             11960000
         DC    3F'0'                                                    11970000
CTLGFLE6 DC    X'01',AL3(0),F'0'                                        11980000
         DC    A(REQUES12)              SPACPARM  (LTH = 7)             11990000
         DC    3F'0'                                                    12000000
CTLGFLE7 DC    X'01',AL3(0),F'0'                                        12010000
         DC    A(REQUES15)              ENTVOL    (LTH = (45+))         12020000
         DC    3F'0'                                                    12030000
CTLGFLE8 DC    X'01',AL3(0),F'0'                                        12040000
         DC    A(REQUES25)              AMDSBCAT  (LTH = 96)            12050000
         DC    3F'0'                                                    12060000
CTLGFLE9 DC    X'01',AL3(0),F'0'                                        12070000
         DC    A(REQUES28)              LRECL     (LTH = 4)             12080000
         DC    3F'0'                                                    12090000
         EJECT                                                          12100000
*        *---------------------------*                                  12110000
*        *   F I E L D   N A M E S   *                                  12120000
*        *---------------------------*                                  12130000
         SPACE                                                          12140000
REQUES01 DC    CL8'MULTITYP'                                            12150000
REQUES02 DC    CL8'ENTYPE'              LTH = 1                         12160000
REQUES03 DC    CL8'ENTNAME'             LTH = 44                        12170000
REQUES04 DC    CL8'CATTR'               LTH = 1                         12180000
REQUES05 DC    CL8'NAMEDS'              LTH = VL: (4) / ASSOC           12190000
REQUES06 DC    CL8'DSETEXDT'            LTH = 3                         12200000
REQUES07 DC    CL8'DSETCRDT'            LTH = 3                         12210000
REQUES08 DC    CL8'OWNERID'             LTH = 8                         12220000
REQUES09 DC    CL8'CRAVOL'              LTH = 6                         12230000
REQUES10 DC    CL8'CATVOL'              LTH = VL: (15) / VOL            12240000
         SPACE                                                          12250000
REQUES11 DC    CL8'VOLDVCHR'            LTH = 20                        12260000
REQUES12 DC    CL8'SPACPARM'            LTH = 7                         12270000
REQUES13 DC    CL8'HURBADS'             LTH = 4                         12280000
REQUES14 DC    CL8'HARBADS'             LTH = 4                         12290000
REQUES15 DC    CL8'ENTVOL'              LTH = VL: (45+) / VOL           12300000
REQUES18 DC    CL8'SYSEXTDS'            LTH = 1                         12310000
REQUES19 DC    CL8'NODSPACE'            LTH = 2                         12320000
REQUES20 DC    CL8'NODSET'              LTH = 2                         12330000
REQUES23 DC    CL8'DSPDSCRP'            LTH = VL: (13+SPACE MAP) / EXT  12340000
REQUES24 DC    CL8'PASSWORD'            LTH = 32                        12350000
         SPACE                                                          12360000
REQUES25 DC    CL8'AMDSBCAT'            LTH = 96                        12370000
REQUES28 DC    CL8'LRECL'               LTH = 4                         12380000
REQUES33 DC    CL8'CATACB'              LTH = 4                         12390000
         EJECT                                                          12400000
*        *-------------------*                                          12410000
*        *   D S E C T ' S   *                                          12420000
*        *-------------------*                                          12430000
         SPACE                                                          12440000
CTLGPLB  DSECT                                                          12450000
CTGBOPT1 DS    XL4                                                      12460000
CTGBENT  DS    F                                                        12470000
CTGBCAT  DS    F                                                        12480000
CTGBWKA  DS    F                                                        12490000
CTGBOPT2 DS    XL4                                                      12500000
CTGBDDNM DS    F                                                        12510000
CTGBPSWD DS    F                                                        12520000
CTGBFLDS DS    0F                                                       12530000
CTGBAFL1 DS    F                        ENTYPE                          12540000
CTGBAFL2 DS    F                        ENTNAME                         12550000
CTGBAFL3 DS    F                        CATTR                           12560000
CTGBAFL4 DS    F                        NAMEDS                          12570000
CTGBAFL5 DS    F                        DSETEXDT                        12580000
CTGBAFL6 DS    F                        DSETCRDT                        12590000
CTGBAFL7 DS    F                        OWNERID                         12600000
CTGBAFL8 DS    F                        CATVOL                          12610000
         SPACE 2                                                        12620000
NMDSECT  DSECT                                                          12630000
NMDSTYP  DS    CL1                      ENTRY TYPE                      12640000
NMDSCIN  DS    XL3                      CI NUMBER                       12650000
         SPACE 2                                                        12660000
VOLDVCHR DSECT                                                          12670000
VOLDVTYP DS    CL4            VOL DEVIVE TYPE                           12680000
VOLDMBLK DS    CL4            MAX DEVICE BLKSIZE                        12690000
VOLDNCYL DS    CL2            NO. OF CYL'S PER VOL                      12700000
VOLDNTRK DS    CL2            NO. OF TRK'S PER CYL                      12710000
VOLDNBYT DS    CL2            NO. OF BYTES PER TRK                      12720000
VOLDBGAP DS    3CL1           NO. OF BYTES REQUIRED FOR GAPS & CHK BITS 12730000
VOLDTFLG DS    CL1            BITS 0-6 RESERVED, BIT 7 TOLERANCE FLAG   12740000
VOLDTFAC DS    CL2            TOLERANCE FACTOR FOR EFFECTIVE LTH OF BLK 12750000
         SPACE 2                                                        12760000
DSPDSCRP DSECT                                                          12770000
DSPREPNO DS    CL2            RELATIVE REPETITION NUMBER                12780000
DSPTUSED DS    CL2            NO. OF TRACKS ALLOCATED IN EXTENT         12790000
DSPXSTRT DS    CL4            BEGINNING ADDR OF EXTENT IN CCHH          12800000
DSPTKEXT DS    CL2            NO. OF TRACKS IN EXTENT                   12810000
DSPSNSPH DS    CL2            SEQ NO. OF SETS-O-FLD'S FOR EXT'S DATA SP 12820000
DSPSPMAP DS    0CL3           SPACE MAP OF EXTENT                       12830000
DSPSMLTH DS    CL2            LTH OF RLC OF SPACE MAP                   12840000
DSPSMRLC DS    CL1            (VL) RUN LENGTH CODE OF SPACE MAP         12850000
         EJECT                                                          12860000
SPACPR   DSECT                                                          12870000
SPACPRIM DS    CL3                      PRIMARY SPACE ALLOCATION        12880000
SPACSCON DS    CL3                      SECONDARY SPACE ALLOCATION      12890000
SPACOPTN DS    CL1                      SPACE ALLOCATION OPTION         12900000
*                                       B'10.. ....'  IN TRACKS         12910000
*                                       B'11.. ....'  IN CYLINDERS      12920000
         SPACE 2                                                        12930000
AMDSB    DSECT                                                          12940000
AMDSBCAT DS    CL96                                                     12950000
         ORG   AMDSBCAT+1                                               12960000
AMDATTR  DS    CL1                      ATTRIBUTES OF THE DATA SET      12970000
*              BIT 0  B'1... ....'      KEY-SEQUENCED DATA SET (KSDS)   12980000
*                     B'0... ....'      ENTRY-SEQUENCED DATA SET (ESDS) 12990000
*              BIT 1  B'.1.. ....'      WRITECHECK                      13000000
*              BIT 2  B'..1. ....'      IMBED                           13010000
*              BIT 3  B'...1 ....'      REPLICATE                       13020000
*              BIT 4  B'.... 1...'      ORDERED                         13030000
*              BIT 5  B'.... .1..'      KEYRANGES                       13040000
*              BIT 6  B'.... ..1.'      RELATIVE RECORD DATA SET (RRDS) 13050000
*              BIT 7  B'.... ...1'      SPANNED                         13060000
         ORG   AMDSBCAT+20                                              13070000
AMDCINV  DS    CL4                      CONTROL INTERVAL SIZE           13080000
AMDLRECL DS    CL4                      MAXIMUM RECORD SIZE             13090000
         ORG                                                            13100000
         EJECT                                                          13110000
ETVOL    DSECT                                                          13120000
ETVREPNO DS    CL2                      RELATIVE REPETITION NUMBER      13130000
ETVDEVTP DS    CL4                      DEVICE TYPE                     13140000
ETVOLSER DS    CL6                      VOLUME SERIAL NUMBER            13150000
ETVFILSQ DS    CL2                      FILE SEQUENCE NUMBER            13160000
ETVOLFLG DS    CL1                      VOLUME FLAGS                    13170000
*                                       BIT 0    X'80' PRIME            13180000
*                                       BIT 1    X'40' CANDIDATE        13190000
*                                       BIT 2    X'20' OVERFLOW         13200000
*                                       BIT 3-7  X'1F' RESERVED         13210000
ETVNOEXT DS    CL1                      NO. OF EXT IN THIS SET OF EXT   13220000
ETVHKRBA DS    CL4                      RBA OF DATA CI W/ THE HIGH KEY  13230000
ETVHURBA DS    CL4                      HIGH-USED RBA                   13240000
ETVHARBA DS    CL4                      HIGH-ALLOCATED RBA              13250000
ETVPBSIZ DS    CL4                      PHYSICAL BLOCKSIZE              13260000
ETVBLKTK DS    CL2                      NO. OF BLOCKS PER TRACK         13270000
ETVTRKAU DS    CL2                      NO. OF TRACKS PER ALLOC UNIT    13280000
ETVITYPE DS    CL1                      INDEX EXTENT TYPE               13290000
*                                       BIT 0    X'80' SEQ SET W/ DATA  13300000
*                                       BIT 1    X'40' EXT'S ARE NOT    13310000
*                                                      PREFORMATTED     13320000
*                                       BIT 2-7  X'3F' RESERVED         13330000
ETVDSDSN DS    CL2                      DATA SET DIR SEQ NO. IN 'V' REC 13340000
ETVLOKEY DS    0CL3                     LOW KEY ON VOL - 64 BYTES MAX   13350000
ETVLKLTH DS    CL2                      LENGTH OF KEY - 0 IF NO KEY     13360000
ETVLKKEY DS    0CL64                    KEY - 0 TO 64 BYTES LONG        13370000
ETVHIKEY DS    0CL3                     HIGH KEY ON VOL - 64 BYTES MAX  13380000
ETVHKLTH DS    CL2                      LENGTH OF KEY - 0 IF NO KEY     13390000
ETVHKKEY DS    0CL64                    KEY - 0 TO 64 BYTES LONG        13400000
ETVEXTNT DS    0CL22                    EXTENT DESCRIPTION FIELDS       13410000
ETVXTLTH DS    CL2                      LTH OF THIS FLD: MULTIPLE OF 20 13420000
ETVXTENT DS    0CL20                    ONE EXTENT (SEE DSECT ETXDSCRP) 13430000
         SPACE 2                                                        13440000
ETXDSCRP DSECT                                                          13450000
ETXDSESN DS    CL2                      DATA SPACE EXTENT'S SEQ NO.     13460000
ETXLOTRK DS    CL4                      LOW CCHH                        13470000
ETXHITRK DS    CL4                      HIGH CCHH                       13480000
ETXNOTRK DS    CL2                      NO. OF TRACKS                   13490000
ETXLORBA DS    CL4                      LOW RBA OF EXTENT               13500000
ETXHIRBA DS    CL4                      HIGH RBA OF EXTENT              13510000
         EJECT                                                          13520000
TXDTAIL  DSECT                                                          13530000
TXDVOLNO DS    CL6                      VOLUME SERIAL NUMBER            13540000
TXDRPTNO DS    0CL1                     REPORT NO. B'0... ....' DETAILS 13550000
TXDRCKEY DS    CL4                      RECORD KEY - GENERATED SORT KEY 13560000
TXDRCSEQ DS    CL2                      RECORD SEQ - SEQ NO IN 'NAMEDS' 13570000
TXDENTNM DS    CL44                     ENTRY NAME                      13580000
TXDENTYP DS    CL1                      ENTRY TYPE                      13590000
TXDCREDT DS    CL3                      CREATION DATE                   13600000
TXDEXPDT DS    CL3                      EXPIRATION DATE                 13610000
TXDCRAVL DS    CL6                      CRA VOLUME SERIAL NUMBER        13620000
TXDSCALC DS    CL3                      SECONDARY SPACE ALLOCATION      13630000
TXDSCTYP DS    CL1                      SPACE ALLOCATION TYPE           13640000
TXDDSORG DS    CL1                      DATA SET ORGANIZATION (AMDATTR) 13650000
TXDCISIZ DS    CL4                      CONTROL INTERVAL SIZE           13660000
TXDMAXRC DS    CL4                      MAXIMUM RECORD SIZE             13670000
TXDAVGRC DS    CL4                      AVERAGE RECORD SIZE             13680000
TXDTRKAL DS    H                        TRK'S ALLOC'D FOR DATA SET      13690000
TXDTRKUS DS    H                        TRK'S USED FOR DATA SET         13700000
TXDPCTUS DS    CL1                      PERCENT OF USAGE                13710000
TXDNOEXT DS    CL1                      NO. OF EXTENTS FOR DATA SET     13720000
         EJECT                                                          13730000
TXVLCHR  DSECT                                                          13740000
TXVVOLNO DS    CL6                      VOLUME SERIAL NUMBER            13750000
TXVRPTNO DS    0CL1                     REPORT NO. B'X... ....'  X=0,1  13760000
TXVRCKEY DS    CL4                      RECORD KEY - ZERO FOR THIS REC  13770000
TXVOLUSE DS    CL2                      VOLUME USE (UCBSTAB)            13780000
TXVDVTYP DS    CL4                      DEVICE TYPE                     13790000
TXVCYLVL DS    H                        NO. OF CYLINDERS PER VOLUME     13800000
TXVTRKCL DS    H                        NO. OF TRACKS PER CYLINDER      13810000
TXVBYTTK DS    H                        NO. OF BYTES PER TRACK          13820000
TXVNODST DS    H                        NUMBER OF DATA SETS ON VOLUME   13830000
TXVNODSP DS    H                        NUMBER OF DATA SPACES ON VOLUME 13840000
TXVSYSXT DS    H                        SYSTEM ALLOWED EXTENTS          13850000
TXVCATNM DS    CL44                     VSAM CATALOG NAME               13860000
TXVVSEXT DS    H                        NUMBER OF VSAM FREE EXTENTS     13870000
TXVVSTRK DS    H                        NUMBER OF VSAM FREE TRACKS      13880000
TXVVSLGE DS    H                        TRACKS OF VSAM LARGEST FREE EXT 13890000
TXVOSEXT DS    H                        NUMBER OF OS FREE EXTENTS       13900000
TXVOSTRK DS    H                        NUMBER OF OS FREE TRACKS        13910000
TXVOSCYL DS    H                        NUMBER OF OS FREE CYLINDERS     13920000
TXVOSLGE DS    H                        TRACKS OF OS LARGEST FREE EXT   13930000
         EJECT                                                          13940000
TXEXTNT  DSECT                                                          13950000
TXEVOLNO DS    CL6                      VOLUME SERIAL NUMBER            13960000
TXERPTNO DS    0CL1                     REPORT NO. B'1... ....' EXTENTS 13970000
TXELOTCH DS    0CL4                     LOW CCHH OF EXTENT (SORT KEY)   13980000
TXELOCC  DS    H                                                        13990000
TXELOHH  DS    H                                                        14000000
TXERCKEY DS    H                        H'0' FOR DSP EXT, H'1' OTHERS   14010000
TXEXTNM  DS    CL44                     DATA SET NAME (ENTRY NAME)      14020000
TXEHITCH DS    0CL4                     HIGH CCHH OF EXTENT             14030000
TXEHICC  DS    H                                                        14040000
TXEHIHH  DS    H                                                        14050000
TXEXTLTH DS    H                        LENGTH OF EXTENT IN TRACKS      14060000
TXELOTRK DS    H                        LOW TRACK NUMBER                14070000
TXEHITRK DS    H                        HIGH TRACK NUMBER               14080000
TXEXTNO  DS    H                        EXTENT NUMBER                   14090000
         EJECT                                                          14100000
DSCB4    DSECT                                                          14110000
DS4DSNAM DS    XL44                     X'04' IN EACH BYTE              14120000
DS4IDFMT DS    CL1                      FMT 4 ID - X'F4'                14130000
DS4HPCHR DS    CL5                                                      14140000
DS4DSREC DS    H                                                        14150000
DS4HCCHH DS    CL4                                                      14160000
DS4NOATK DS    H                                                        14170000
DS4VTOCI DS    XL1                      B'1... ....'  NO FMT 5 DSCB     14180000
DS4NOEXT DS    XL1                      NO. OF VTOC EXT'S - X'01'       14190000
         DS    CL2                                                      14200000
DS4DEVSZ DS    2H                                                       14210000
DS4DEVTK DS    H                                                        14220000
DS4DEVOV DS    0XL2                                                     14230000
DS4DEVI  DS    XL1                                                      14240000
DS4DEVL  DS    XL1                                                      14250000
DS4DEVK  DS    XL1                                                      14260000
DS4DEVFG DS    XL1                                                      14270000
DS4DEVTL DS    H                                                        14280000
DS4DEVDT DS    XL1                                                      14290000
DS4DEVDB DS    XL1                                                      14300000
DS4AMTIM DS    CL8                                                      14310000
DS4AMCAT DS    0XL3                                                     14320000
DS4VSIND DS    XL1                                                      14330000
DS4VSCRA DS    XL2                                                      14340000
DS4R2TIM DS    CL8                                                      14350000
         DS    CL5                                                      14360000
DS4F6PTR DS    CL5                                                      14370000
DS4VTOCE DS    0CL10                    VTOC EXT - SAME AS DS1EXT1      14380000
DS4VTIND DS    XL1                      DATA SET EXT TYPE INDICATOR     14390000
DS4VTSEQ DS    XL1                      EXT SEQ NO.                     14400000
DS4VTLOR DS    XL4                      LOWER LIMIT OF EXT (CCHH)       14410000
DS4VTUPR DS    XL4                      UPPER LIMIT OF EXT (CCHH)       14420000
         DS    CL25                                                     14430000
         EJECT                                                          14440000
DSCB5    DSECT                                                          14450000
DS5KEYID DS    XL4                      X'05' IN EACH BYTE              14460000
DS5AVEXT DS    XL5                      1 FREE EXT - SEE DSECT DFXEXT   14470000
DS5EXTAV DS    7XL5                     7 MORE FREE EXT'S               14480000
DS5FMTID DS    CL1                      FMT 5 ID - X'F5'                14490000
DS5MAVET DS    18XL5                    18 MORE FREE EXT'S              14500000
DS5PTRDS DS    XL5                      CCHHR OF NEXT FMT 5 DSCB IF ANY 14510000
         SPACE 2                                                        14520000
UCBOB    DSECT                                                          14530000
UCB      DS    CL64                                                     14540000
         ORG   UCB+3                                                    14550000
UCBSTAT  DS    XL1                      B'1... ....'  DEVICE ON-LINE    14560000
         ORG   UCB+16                                                   14570000
UCBTYP   DS    XL4                      X'.... 20..'  DASD              14580000
         ORG   UCB+28                                                   14590000
UCBVOLI  DS    CL6                      VOLUME SERIAL NUMBER            14600000
UCBSTAB  DS    XL1                      VOLUME USE                      14610000
*                                       B'...1 ....'  PRIVATE           14620000
*                                       B'.... 1...'  PUBLIC            14630000
*                                       B'.... .1..'  STORAGE           14640000
*                                                                       14650000
* NOTE:  THE FOLLOWING FIELD 'UCBVTOC' HAS DIFFERENT OFFSETS IN         14660000
*        VS1 AND MVS.                                                   14670000
*        ORG   UCB+36                   USED FOR VS1                    14680000
*        ORG   UCB+24                   USED FOR MVS                    14690000
*                                                                       14700000
         ORG   UCB+24                   MVS VERSION                     14710000
UCBVTOC  DS    XL4                      ADDR OF VTOC IN TTRZ            14720000
         ORG                                                            14730000
         END                                                            14740000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD                             00210000
//LKED.SYSIN DD *                                                       00220000
  NAME VSAMLST4(R)                                                      00230000
//A        EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF'               00120000
//ASM.SYSIN DD *                                                        00130000
         PRINT OFF                                                      00010000
         MACRO                                                          00020000
&NAME    INIT &BASE=3,&REGS=Y,&PATCH=3,&RENT=N,&SAVE=Y                  00030000
         AIF   ((&BASE LT 13) AND (&BASE GT 1)).N020                    00040000
         MNOTE 12,'INVALID BASE REGISTER'                               00050000
         MEXIT                                                          00060000
.N020    ANOP                                                           00070000
         PUSH  PRINT                                                    00080000
         PRINT ON,GEN                                                   00090000
         EJECT                                                          00100000
&SYSECT  CSECT                                                          00110000
         USING *,15                                                     00120000
         B     BEGIN                                                    00130000
         DC    AL1(24)                                                  00140000
         DC    CL8'&SYSECT'                                             00150000
         DC    CL16'-&SYSDATE-&SYSTIME'                                 00160000
         AIF   ('&RENT' EQ 'Y').N004                                    00170000
MYSAVE   DC    18F'-1'                                                  00180000
.N004    ANOP                                                           00190000
         AIF   ('&PATCH' EQ '0').N005                                   00200000
PATCH    DC    &PATCH.CL8'*PATCH*'                                      00210000
.N005    ANOP                                                           00220000
         AIF   ('&REGS' EQ 'N').N030                                    00230000
         AIF   ('&REGS' EQ 'Y').N010                                    00240000
         MNOTE 4,'REGS OPERAND INVALID. Y SUBSTITUTED'                  00250000
.N010    ANOP                                                           00260000
R0       EQU   0                                                        00270000
R1       EQU   1                                                        00280000
R2       EQU   2                                                        00290000
R3       EQU   3                                                        00300000
R4       EQU   4                                                        00310000
R5       EQU   5                                                        00320000
R6       EQU   6                                                        00330000
R7       EQU   7                                                        00340000
R8       EQU   8                                                        00350000
R9       EQU   9                                                        00360000
R10      EQU   10                                                       00370000
R11      EQU   11                                                       00380000
R12      EQU   12                                                       00390000
R13      EQU   13                                                       00400000
R14      EQU   14                                                       00410000
R15      EQU   15                                                       00420000
.N030    ANOP                                                           00430000
BEGIN    DS   0H                                                        00440000
         STM   14,12,12(13)                                             00450000
         LR    &BASE,15                                                 00460000
         DROP  15                                                       00470000
         USING &SYSECT,&BASE                                            00480000
         AIF   ('&SAVE' EQ 'N').N003                                    00490000
         AIF   ('&RENT' EQ 'Y').N002                                    00500000
         AIF   ('&RENT' EQ 'N').N001                                    00510000
         MNOTE 4,'RENT OPERAND INVALID. N SUBSTITUTED'                  00520000
.N001    ANOP                                                           00530000
         ST    13,MYSAVE+4                                              00540000
         LR    15,13                                                    00550000
         LA    13,MYSAVE                                                00560000
         ST    13,8(15)                                                 00570000
         AGO   .N003                                                    00580000
.N002    ANOP                                                           00590000
         GETMAIN R,LV=72                                                00600000
         ST    13,4(1)                                                  00610000
         ST    1,8(13)                                                  00620000
         LR    13,1                                                     00630000
.N003    ANOP                                                           00640000
         POP   PRINT                                                    00650000
         EJECT                                                          00660000
         MEND                                                           00670000
         PRINT ON                                                       00680000
*  MODULE NAME:         VSAMLST5     (REL. 1.1  08/10/79)               00690000
*                                                                       00700000
*  MODULE DESCRIPTION:  VSAM CATALOG LIST UTILITY - E35/DETAILS REPORTS 00710000
*                                                                       00720000
*  RETURN LINKAGE:      RETURN (14,12),RC=(15)                          00730000
*                                                                       00740000
*  LINKAGE TABLE:       NONE - SINGLE ENTRY                             00750000
*                                                                       00760000
*  PARAMETERS:          R1 POINTS TO THE ADDRESS OF A 92-BYTES RECORD   00770000
*                       FROM SORT.                                      00780000
*                                                                       00790000
*  EXIT:                RC=04 - DELETE RECORD                           00800000
*                       RC=08 - DO NOT RETURN                           00810000
*                                                                       00820000
*  MODULE FUNCTION:     FOR EACH RECORD FROM SORT, THIS MODULE FORMATS  00830000
*                       IT AFTER DETERMINING THE REPORT TO WHICH IT     00840000
*                       BELONGS.  THIS MODULE PRODUCES TWO FORMATTED    00850000
*                       DETAILS REPORTS.  THEY ARE THE VSAM VOLUME      00860000
*                       CONTENTS AND THE VSAM TRACK ALLOCATION MAP.     00870000
*                       IF THE OPTIONAL DD CARD 'VSAMHIST' IS IN THE    00880000
*                       JCL AND POINTS TO A DASD DATA SET, IT WILL      00890000
*                       WRITE OUT A SUMMARY VSAM HISTORY FILE WITH      00900000
*                       INFORMATION FROM THE VSAM VOLUME CONTENTS       00910000
*                       REPORT.                                         00920000
*                                                                       00930000
*  CALLER:              VSAMLIST/SORT                                   00940000
*                                                                       00950000
*  CALLS:               VSAMLST6, VSAMLST7, VSAMLST8                    00960000
*                                                                       00970000
*  SYSTEMS SERVICES:    CALL, CLOSE, DELETE, DEVTYPE, LOAD, OPEN,       00980000
*                       PUT (QSAM), TIME                                00990000
*                                                                       01000000
*  MODULE ENVIRONMENT:  OS/VS1                                          01010000
         TITLE 'VSAMLST5 --- VSAM CATALOG LIST UTILITY DETAILS REPORTS X01020000
               GENERATOR (3.0) - SORT EXIT E35'                         01030000
*        *-----------------------------------*                          01040000
*        *   R E G I S T E R   U S A G E S   *                          01050000
*        *-----------------------------------*                          01060000
*                                                                       01070000
*  R0  -  STANDARD LINKAGE                                              01080000
*  R1  -  STANDARD LINKAGE (ADDR OF PARM LIST) & TEMP WORK SPACE        01090000
*  R2  -  TEMP WORK SPACE                                               01100000
*  R3  -  TEMP WORK SPACE                                               01110000
*  R4  -  TEMP WORK SPACE                                               01120000
*  R5  -  TEMP WORK SPACE                                               01130000
*  R6  -  NOT USED                                                      01140000
*  R7  -  NOT USED                                                      01150000
*  R8  -  NOT USED                                                      01160000
*  R9  -  NOT USED                                                      01170000
*  R10 -  BASE REG FOR DSECT INDTAIL, INVLCHR, INEXTNT                  01180000
*  R11 -  RETURN ADDR FOR SUBROUTINES                                   01190000
*  R12 -  BASE REG FOR CSECT VSAMLST5                                   01200000
*  R13 -  STANDARD LINKAGE (ADDR OF SAVE AREA)                          01210000
*  R14 -  STANDARD LINKAGE (ADDR TO RETURN)                             01220000
*  R15 -  STANDARD LINKAGE (ADDR OF ENTRY POINT & RETURN CODE)          01230000
         SPACE 3                                                        01240000
VSAMLST5 CSECT                                                          01250000
         INIT  BASE=12                                                  01260000
*********************************************************************** 01270000
*                                                                     * 01280000
*  3.0    E35 EXIT TO FORMAT DETAILS REPORTS                          * 01290000
*                                                                     * 01300000
*     FUNCTION:  IF REPORT NO. IS ZERO, PROCESS IT FOR VSAM VOLUME    * 01310000
*                CONTENTS & OPTIONAL VSAM HISTORY FILE.               * 01320000
*                IF REPORT NO. IS ONE, PROCESS IT FOR VSAM TRACK      * 01330000
*                ALLOCATION MAP.                                      * 01340000
*                                                                     * 01350000
*     ERRORS:  NONE                                                   * 01360000
*                                                                     * 01370000
*********************************************************************** 01380000
         SPACE 3                                                        01390000
VCL30    EQU   *                                                        01400000
         SPACE                                                          01410000
NOP0     B     INIT0                    INITIALIZE IF 1ST TIME THRU     01420000
         L     R10,0(,R1)               GET ADDR OF A SORTED RECORD     01430000
         LA    R10,0(,R10)              CLEAR HIGH ORDER BYTE           01440000
         LTR   R10,R10                  IS IT EOD YET?                  01450000
         BZ    EOSORTIN                 YES - CLEAN IT UP AND GO HOME   01460000
         SPACE                                                          01470000
         USING INDTAIL,R10                                              01480000
         TM    INDRPTNO,X'80'           IS THE REPORT NO. ONE?          01490000
         BO    RPT2                     YES - VSAM TRK ALLOC MAP        01500000
*                                       NO - VSAM VOLUME CONTENTS       01510000
         EJECT                                                          01520000
*        *************************************                          01530000
*        *                                   *                          01540000
*        *  REPORT 1 - VSAM VOLUME CONTENTS  *                          01550000
*        *                                   *                          01560000
*        *************************************                          01570000
         SPACE                                                          01580000
RPT1     EQU   *                                                        01590000
         CLI   RECPEND,X'00'            IS THERE A RECORD PENDING?      01600000
         BE    CHKRCTYP                 NO - GO CHECK THE RECORD TYPE   01610000
         CLC   SAVKEYSQ,INDRCKEY        YES - IS THIS A NEW RECORD?     01620000
         BNE   PRTSAVED                 YES - PRINT THE PENDING RECORD  01630000
         SPACE                                                          01640000
         LH    R2,INDTRKAL              TRACKS ALLOCATED                01650000
         AH    R2,SAVTRKAL                                              01660000
         STH   R2,SAVTRKAL                                              01670000
         SPACE                                                          01680000
         LH    R1,INDTRKUS              TRACKS USED                     01690000
         AH    R1,SAVTRKUS                                              01700000
         STH   R1,SAVTRKUS                                              01710000
         SPACE                                                          01720000
         SR    R0,R0                    PERCENTS OF USAGE               01730000
         MH    R1,HUNDRED                                               01740000
         DR    R0,R2                                                    01750000
         STC   R1,SAVPCTUS                                              01760000
         SPACE                                                          01770000
         SR    R0,R0                    NUMBER OF EXTENTS               01780000
         IC    R0,SAVNOEXT                                              01790000
         SR    R1,R1                                                    01800000
         IC    R1,INDNOEXT                                              01810000
         AR    R0,R1                                                    01820000
         STC   R0,SAVNOEXT                                              01830000
         B     RPT1TYP3                 ALL DONE FOR NOW                01840000
         EJECT                                                          01850000
*        *-----------------------------*                                01860000
*        *  THERE IS A RECORD PENDING  *                                01870000
*        *-----------------------------*                                01880000
         SPACE                                                          01890000
PRTSAVED EQU   *                                                        01900000
         MVI   RECPEND,X'00'            RESET RECORD-PENDING FLAG       01910000
         SPACE                                                          01920000
         LA    R1,SAVEDREC              FORMAT & PRINT THE SAVED RECORD 01930000
         ST    R1,RECD31                                                01940000
         ST    R1,RECD33                                                01950000
         LA    R1,PARM9431                                              01960000
         L     R15,EPVCL31                                              01970000
         CALL  (15)                                                     01980000
         SPACE                                                          01990000
         BAL   R11,PUTRPT                                               02000000
         SPACE 2                                                        02010000
         CLI   VSMHSTSW,X'FF'           IS HISTORY FILE NEEDED?         02020000
         BNE   CHKRCTYP                 NO - SKIP THIS                  02030000
         LA    R1,PARM9433                                              02040000
         L     R15,EPVCL33                                              02050000
         CALL  (15)                                                     02060000
         EJECT                                                          02070000
*        *-----------------------------------------------*              02080000
*        *  THERE ARE 4 TYPES OF RECORDS FROM SORT.      *              02090000
*        *    TYPE 0 - VOL CHAR INFO'S:  BEGINNING       *              02100000
*        *    TYPE 1 - CL, AIX & ASSOC                   *              02110000
*        *    TYPE 2 - D & I:  MAYBE MULTIPLE RECORDS    *              02120000
*        *    TYPE 3 - DUMMY RECORD:  ENDING             *              02130000
*        *-----------------------------------------------*              02140000
         SPACE                                                          02150000
CHKRCTYP EQU   *                                                        02160000
         CLC   INDRCKEY,ZERO            IS THIS TYPE 0?                 02170000
         BE    RPT1TYP0                 YES - BEGINNING OF A NEW VOLUME 02180000
         SPACE                                                          02190000
         CLC   INDRCSEQ,FOXES           IS THIS TYPE 3?                 02200000
         BE    RPT1TYP3                 YES - END OF THE CURRENT VOLUME 02210000
         SPACE                                                          02220000
         CLI   INDENTYP,C'D'            IS THIS TYPE 2?                 02230000
         BE    RPT1TYP2                                                 02240000
         CLI   INDENTYP,C'I'                                            02250000
         BE    RPT1TYP2                 YES - CHECK IF MULTIPLE RECORDS 02260000
         SPACE                                                          02270000
         B     RPT1TYP1                 NO - MUST BE CL, AIX OR ASSOC   02280000
         DROP  R10                                                      02290000
         EJECT                                                          02300000
*        *----------------------------------------------------------*   02310000
*        *  TYPE 0 - BEGIN A REPORT OF A NEW VOLUME - PRINT TITLES  *   02320000
*        *----------------------------------------------------------*   02330000
         SPACE                                                          02340000
RPT1TYP0 EQU   *                                                        02350000
         USING INVLCHR,R10                                              02360000
         MVI   RPTFLAG,X'00'            INDICATE REPORT 1               02370000
         SPACE                                                          02380000
*        *---------------*                                              02390000
*        *  SET TITLE 1  *                                              02400000
*        *---------------*                                              02410000
         SPACE                                                          02420000
         MVI   RPTFLAG,X'00'            INDICATE REPORT 1               02430000
         MVC   SUBTIT1,SUBTIT11         VSAM VOLUME CONTENTS            02440000
         MVC   VOLSER1,INVVOLNO         VOLUME SERIAL NUMBER            02450000
         AP    PAGECNT,=P'1'            YES - TIME FOR A NEW PAGE       02460000
         MVC   PAGE1,PAT3                                               02470000
         ED    PAGE1,PAGECNT            SET NEW PAGE NUMBER             02480000
         SPACE 2                                                        02490000
*        *---------------*                                              02500000
*        *  SET TITLE 2  *                                              02510000
*        *---------------*                                              02520000
         SPACE                                                          02530000
         CLI   INVDVTYP+2,X'20'         IS IT A DASD?                   02540000
         BNE   BADDVTP                  NO - GO TRANSLATE IT            02550000
         SPACE                                                          02560000
         MVI   DEVTYPE2,X'40'           CLEAR DEVICE TYPE               02570000
         MVC   DEVTYPE2+1(L'DEVTYPE2-1),DEVTYPE2                        02580000
         SR    R2,R2                    CLEAR INDEX FOR DASD TYPE TABLE 02590000
         IC    R2,INVDVTYP+3            GET DEVICE CODE                 02600000
         SLL   R2,3                     MULTIPLY BY 8 FOR DISPLACEMENT  02610000
         LA    R2,TYPEDASD(R2)          R2 = ADDR OF DASD TYPE ENTRY    02620000
         LA    R1,TYPEDEND              R1 = ADDR OF END OF TABLE       02630000
         CR    R1,R2                    IS THIS A VALID ENTRY?          02640000
         BNH   BADDVTP                  NO - GO TRANSLATE IT            02650000
         MVC   DEVTYPE2(7),1(R2)        ONLY 7 BYTES FROM TABLE         02660000
         B     STAB1                                                    02670000
         SPACE                                                          02680000
BADDVTP  EQU   *                                                        02690000
         UNPK  DEVTYPE2(9),INVDVTYP(5)  TRANSLATE INTO HEX              02700000
         TR    DEVTYPE2,HEXTBL                                          02710000
         MVI   DEVTYPE2+8,X'40'                                         02720000
         EJECT                                                          02730000
*        *-----------------------*                                      02740000
*        *  SET TITLE 2 - CONT.  *                                      02750000
*        *-----------------------*                                      02760000
         SPACE                                                          02770000
STAB1    EQU   *                        DASD VOLUME USE                 02780000
         TM    INVOLUSE+1,X'10'                                         02790000
         BZ    STAB2                                                    02800000
         MVC   VOLUSE2,=C'PRIVATE'                                      02810000
         B     CYLVOL                                                   02820000
         SPACE                                                          02830000
STAB2    EQU   *                                                        02840000
         TM    INVOLUSE+1,X'08'                                         02850000
         BZ    STAB3                                                    02860000
         MVC   VOLUSE2,=C'PUBLIC '                                      02870000
         B     CYLVOL                                                   02880000
         SPACE                                                          02890000
STAB3    EQU   *                                                        02900000
         TM    INVOLUSE+1,X'04'                                         02910000
         BZ    STAB4                                                    02920000
         MVC   VOLUSE2,=C'STORAGE'                                      02930000
         B     CYLVOL                                                   02940000
         SPACE                                                          02950000
STAB4    EQU   *                                                        02960000
         MVC   VOLUSE2,=C'STAB XX'      TRANSLATE INTO HEX              02970000
         UNPK  VOLUSE2+5(3),INVOLUSE+1(2)                               02980000
         TR    VOLUSE2+5(2),HEXTBL                                      02990000
         MVI   VOLUSE2+7,X'40'                                          03000000
         SPACE                                                          03010000
CYLVOL   EQU   *                                                        03020000
         LH    R1,INVCYLVL              NO. OF LOGICAL CYLINDERS/VOL    03030000
         CVD   R1,WORK                                                  03040000
         MVO   WORK(3),WORK+5(3)                                        03050000
         MVC   CYLVOL2,PAT4                                             03060000
         ED    CYLVOL2,WORK                                             03070000
         SPACE                                                          03080000
         LH    R1,INVTRKCL              NO. OF TRACKS/CYL               03090000
         CVD   R1,WORK                                                  03100000
         MVC   TRKCYL2,PAT3                                             03110000
         ED    TRKCYL2,WORK+6                                           03120000
         SPACE                                                          03130000
         LH    R1,INVBYTTK              NO. OF BYTES/TRK                03140000
         CVD   R1,WORK                                                  03150000
         MVC   BYTTRK2,PAT5                                             03160000
         ED    BYTTRK2,WORK+5                                           03170000
         EJECT                                                          03180000
*        *---------------*                                              03190000
*        *  SET TITLE 3  *                                              03200000
*        *---------------*                                              03210000
         SPACE                                                          03220000
         LH    R1,INVNODST              NO. OF VSAM DATA SETS ON VOL    03230000
         CVD   R1,WORK                                                  03240000
         MVC   DSNVOL3,PAT5                                             03250000
         ED    DSNVOL3,WORK+5                                           03260000
         SPACE                                                          03270000
         LH    R1,INVNODSP              NO. OF DATA SPACES ON VOL       03280000
         CVD   R1,WORK                                                  03290000
         MVC   DSPCVOL3,PAT3                                            03300000
         ED    DSPCVOL3,WORK+6                                          03310000
         SPACE                                                          03320000
         LH    R1,INVSYSXT              NO. OF SYSTEM ALLOWED EXTENTS   03330000
         CVD   R1,WORK                                                  03340000
         MVC   MXTALLC3,PAT3                                            03350000
         ED    MXTALLC3,WORK+6                                          03360000
         SPACE                                                          03370000
         MVC   CATNAME3,INVCATNM        VSAM CATALOG NAME               03380000
         SPACE 2                                                        03390000
*        *---------------*                                              03400000
*        *  SET TITLE 4  *                                              03410000
*        *---------------*                                              03420000
         SPACE                                                          03430000
         LH    R1,INVVSEXT              NO. OF VSAM FREE EXTENTS        03440000
         CVD   R1,WORK                                                  03450000
         MVC   VSFSNUM4,PAT5                                            03460000
         ED    VSFSNUM4,WORK+5                                          03470000
         SPACE                                                          03480000
         LH    R1,INVVSTRK              NO. OF VSAM FREE TRACKS         03490000
         CVD   R1,WORK                                                  03500000
         MVC   VSFSTRK4,PAT5                                            03510000
         ED    VSFSTRK4,WORK+5                                          03520000
         SPACE                                                          03530000
         LH    R1,INVVSLGE              TRK'S OF VSAM LARGEST FREE EXT  03540000
         CVD   R1,WORK                                                  03550000
         MVC   VSFSLGE4,PAT5                                            03560000
         ED    VSFSLGE4,WORK+5                                          03570000
         EJECT                                                          03580000
*        *---------------*                                              03590000
*        *  SET TITLE 5  *                                              03600000
*        *---------------*                                              03610000
         SPACE                                                          03620000
         CLC   INVOSEXT,FOXES           ARE OS SPACE INFO'S AVAILABLE?  03630000
         BNE   SETITLE5                 YES - CONTINUE                  03640000
         LA    R1,TITLE5A               NO - SET MESSAGE                03650000
         ST    R1,ATITLE5                                               03660000
         B     ALLTSET                  AND PRINT THEM                  03670000
         SPACE                                                          03680000
SETITLE5 EQU   *                                                        03690000
         LH    R1,INVOSEXT              NO. OF OS FREE EXTENTS          03700000
         CVD   R1,WORK                                                  03710000
         MVC   OSFSNUM5,PAT5                                            03720000
         ED    OSFSNUM5,WORK+5                                          03730000
         SPACE                                                          03740000
         LH    R1,INVOSTRK              NO. OF OS FREE TRACKS           03750000
         CVD   R1,WORK                                                  03760000
         MVC   OSFSTRK5,PAT5                                            03770000
         ED    OSFSTRK5,WORK+5                                          03780000
         SPACE                                                          03790000
         LH    R1,INVOSCYL              NO. OF OS FREE CYLINDERS        03800000
         CVD   R1,WORK                                                  03810000
         MVO   WORK(3),WORK+5(3)                                        03820000
         MVC   OSFSCYL5,PAT4                                            03830000
         ED    OSFSCYL5,WORK                                            03840000
         SPACE                                                          03850000
         LH    R1,INVOSLGE              TRK'S OF OS LARGEST FREE EXT    03860000
         CVD   R1,WORK                                                  03870000
         MVC   OSFSLGE5,PAT5                                            03880000
         ED    OSFSLGE5,WORK+5                                          03890000
         EJECT                                                          03900000
*        *-----------------------------------*                          03910000
*        *  ALL TITLES SET - PRINT THEM NOW  *                          03920000
*        *-----------------------------------*                          03930000
         SPACE                                                          03940000
ALLTSET  EQU   *                                                        03950000
         LA    R2,TITLENO               NO. OF TITLES S/B 7             03960000
         LA    R3,TITLES                BEGINNING OF ADDR'S OF TITLES   03970000
         SPACE                                                          03980000
PRNTITLE EQU   *                                                        03990000
         L     R0,0(,R3)                ADDR OF TITLE OR HEADING        04000000
         PUT   RPTFIL2,(0)                                              04010000
         SPACE                                                          04020000
         LA    R3,4(,R3)                NEXT TITLE ADDR                 04030000
         BCT   R2,PRNTITLE              PRINT THEM ALL                  04040000
         SPACE                                                          04050000
         CLC   INVOSEXT,FOXES           ARE OS SPACE INFO'S AVAILABLE?  04060000
         BNE   NORSTL5                  YES - CONTINUE                  04070000
         LA    R1,TITLE5                NO - RESET ADDR OF TITLE        04080000
         ST    R1,ATITLE5                                               04090000
         SPACE                                                          04100000
NORSTL5  EQU   *                                                        04110000
         MVI   LINECNT+1,X'0D'          13 LINES HAVE BEEN PRINTED      04120000
         MVI   CNTL,C'0'                SKIP 2 LINES FOR NEXT PRINT     04130000
         B     RPT1TYP3                 ALL DONE FOR TYPE 0 RECORD      04140000
         SPACE                                                          04150000
         DROP  R10                                                      04160000
         EJECT                                                          04170000
*        *-----------------------------*                                04180000
*        *  TYPE 1 - FORMAT AND PRINT  *                                04190000
*        *-----------------------------*                                04200000
         SPACE                                                          04210000
RPT1TYP1 EQU   *                                                        04220000
         USING INDTAIL,R10                                              04230000
         ST    R10,RECD31                                               04240000
         LA    R1,PARM9431                                              04250000
         L     R15,EPVCL31                                              04260000
         CALL  (15)                                                     04270000
         SPACE                                                          04280000
         CLC   INDRCSEQ,ZERO            IS THIS THE FIRST OF THE SET?   04290000
         BNE   ASSOCREC                 NO - JUST PRINT IT              04300000
         CLI   CNTL,C'0'                IS THIS THE FIRST LINE?         04310000
         BE    ASSOCREC                 YES - JUST PRINT IT             04320000
         SPACE                                                          04330000
         LH    R1,LINEMAX               CHECK IF IT'S THE LAST LINE     04340000
         BCTR  R1,0                                                     04350000
         CH    R1,LINECNT                                               04360000
         BH    NOTLAST                                                  04370000
         MVI   LINECNT+1,X'FF'          PRINT IT ON A NEW PAGE          04380000
         B     ASSOCREC                                                 04390000
         SPACE                                                          04400000
NOTLAST  EQU   *                                                        04410000
         MVI   CNTL,C'0'                SKIP 2 LINES                    04420000
         BAL   R11,PUTRPT                                               04430000
         LH    R1,LINECNT                                               04440000
         LA    R1,1(,R1)                                                04450000
         STH   R1,LINECNT                                               04460000
         B     RPT1TYP3                                                 04470000
         SPACE                                                          04480000
ASSOCREC EQU   *                                                        04490000
         BAL   R11,PUTRPT                                               04500000
         B     RPT1TYP3                                                 04510000
         EJECT                                                          04520000
*        *--------------------------------------------------*           04530000
*        *  TYPE 2 - IF MULTIPLE RECORDS POSSIBLE, SAVE IT  *           04540000
*        *           ELSE FORMAT AND PRINT                  *           04550000
*        *--------------------------------------------------*           04560000
         SPACE                                                          04570000
RPT1TYP2 EQU   *                                                        04580000
         TM    INDDSORG,X'04'           KEY RANGE?                      04590000
         BO    HOLDTYP2                 YES - HOLD IT                   04600000
         DROP  R10                                                      04610000
         SPACE                                                          04620000
         ST    R10,RECD31                                               04630000
         ST    R10,RECD33                                               04640000
         LA    R1,PARM9431                                              04650000
         L     R15,EPVCL31                                              04660000
         CALL  (15)                                                     04670000
         SPACE                                                          04680000
         BAL   R11,PUTRPT                                               04690000
         SPACE 2                                                        04700000
         CLI   VSMHSTSW,X'FF'           IS HISTORY FILE NEEDED?         04710000
         BNE   RPT1TYP3                 NO - SKIP THIS                  04720000
         LA    R1,PARM9433                                              04730000
         L     R15,EPVCL33                                              04740000
         CALL  (15)                                                     04750000
         B     RPT1TYP3                                                 04760000
         SPACE                                                          04770000
HOLDTYP2 EQU   *                                                        04780000
         MVC   SAVEDREC,0(R10)          SAVE RECORD                     04790000
         MVI   RECPEND,X'FF'            AND INDICATE SO                 04800000
         B     RPT1TYP3                                                 04810000
         SPACE 2                                                        04820000
*        *-----------------------*                                      04830000
*        *  TYPE 3 - DO NOTHING  *                                      04840000
*        *-----------------------*                                      04850000
         SPACE                                                          04860000
RPT1TYP3 EQU   *                                                        04870000
         LA    R15,4                    TELL SORT TO DELETE THIS RECORD 04880000
         B     GOHOME                                                   04890000
         EJECT                                                          04900000
*        ******************************************                     04910000
*        *                                        *                     04920000
*        *  REPORT 2 - VSAM TRACK ALLOCATION MAP  *                     04930000
*        *                                        *                     04940000
*        ******************************************                     04950000
         SPACE                                                          04960000
RPT2     EQU   *                                                        04970000
         USING INEXTNT,R10                                              04980000
         SPACE                                                          04990000
*        *-----------------------------------------------*              05000000
*        *  THERE ARE 3 TYPES OF RECORDS FROM SORT.      *              05010000
*        *    TYPE 0 - VOL CHAR INFO'S:  BEGINNING       *              05020000
*        *    TYPE 1 - EXTENT INFO'S                     *              05030000
*        *    TYPE 2 - DUMMY RECORD:  ENDING             *              05040000
*        *-----------------------------------------------*              05050000
         SPACE                                                          05060000
         CLC   INELOTCH,FOXES           IS THIS TYPE 2?                 05070000
         BE    RPT2TYP2                 YES - END OF THE CURRENT VOLUME 05080000
         SPACE                                                          05090000
         NI    INERPTNO,X'7F'           TURN OFF THE FIRST BIT          05100000
         CLC   INELOTCH,ZERO            IS THIS TYPE 0?                 05110000
         BE    RPT2TYP0                 YES - BEGINNING OF A NEW VOLUME 05120000
         SPACE                                                          05130000
         B     RPT2TYP1                 ELSE MUST BE EXTENT INFO'S      05140000
         DROP  R10                                                      05150000
         EJECT                                                          05160000
*        *--------------------------*                                   05170000
*        *  TYPE 0 - RESET TITLE 1  *                                   05180000
*        *--------------------------*                                   05190000
         SPACE                                                          05200000
RPT2TYP0 EQU   *                                                        05210000
         MVI   RPTFLAG,X'FF'            INDICATE REPORT 2               05220000
         MVC   SUBTIT1,SUBTIT21         VSAM TRACK ALLOCATION MAP       05230000
         MVI   LINECNT+1,X'FF'          MAKE SURE TO START A NEW PAGE   05240000
         SPACE                                                          05250000
         ST    R10,RECD32                                               05260000
         LA    R1,PARM9432                                              05270000
         L     R15,EPVCL32                                              05280000
         CALL  (15)                     PASS VCL 3.2 THE VOLUME INFO'S  05290000
         SPACE                                                          05300000
         BAL   R11,PUTRPT               WRITE OUT THE VOL LABEL EXT     05310000
         B     RPT1TYP3                 DELETE AND GO HOME              05320000
         SPACE 2                                                        05330000
*        *-----------------------------*                                05340000
*        *  TYPE 1 - FORMAT AND PRINT  *                                05350000
*        *-----------------------------*                                05360000
         SPACE                                                          05370000
RPT2TYP1 EQU   *                                                        05380000
         ST    R10,RECD32                                               05390000
         LA    R1,PARM9432                                              05400000
         L     R15,EPVCL32                                              05410000
         CALL  (15)                                                     05420000
         SPACE                                                          05430000
         LTR   R4,R1                    IS THERE A SECOND LINE?         05440000
         BZ    ONELINE1                 NO - ONLY ONE LINE              05450000
         BAL   R11,PUTRPT                                               05460000
         MVC   LINE,0(R4)               SET UP SECOND LINE              05470000
         SPACE                                                          05480000
ONELINE1 EQU   *                                                        05490000
         BAL   R11,PUTRPT                                               05500000
         B     RPT1TYP3                                                 05510000
         EJECT                                                          05520000
*        *-------------------------------------------------*            05530000
*        *  TYPE 3 - END OF CURRENT REPORT - SUMMARIZE IT  *            05540000
*        *-------------------------------------------------*            05550000
         SPACE                                                          05560000
RPT2TYP2 EQU   *                                                        05570000
         ST    R10,RECD32                                               05580000
         LA    R1,PARM9432                                              05590000
         L     R15,EPVCL32                                              05600000
         CALL  (15)                                                     05610000
         SPACE                                                          05620000
         LTR   R4,R1                    IS THERE A SECOND LINE?         05630000
         BZ    ONELINE2                 NO - ONLY ONE LINE              05640000
         BAL   R11,PUTRPT                                               05650000
         MVC   LINE,0(R4)               SET UP SECOND LINE              05660000
         SPACE                                                          05670000
ONELINE2 EQU   *                                                        05680000
         LH    R1,LINECNT               CHECK IF THERE'S ENOUGH ROOM    05690000
         LA    R1,4(,R1)                FOR 4 MORE LINES                05700000
         CH    R1,LINEMAX                                               05710000
         BNH   FOURMORE                 YES - THERE'S ENOUGH            05720000
         MVI   LINECNT+1,X'FF'          NO - PRINT IT ON A NEW PAGE     05730000
         SPACE                                                          05740000
FOURMORE EQU   *                                                        05750000
         MVI   CNTL,C'0'                SKIP 2 LINES                    05760000
         BAL   R11,PUTRPT                                               05770000
         SPACE                                                          05780000
         MVI   CNTL,C'0'                SKIP 2 LINES                    05790000
         MVC   LINE+51(L'ENDING1),ENDING1                               05800000
         BAL   R11,PUTRPT                                               05810000
         B     RPT1TYP3                                                 05820000
         EJECT                                                          05830000
*        *---------------------------------------------*                05840000
*        *   I N I T   -   S E T   D A T E / T I M E   *                05850000
*        *---------------------------------------------*                05860000
         SPACE                                                          05870000
INIT0    EQU   *                                                        05880000
         TIME  DEC                                                      05890000
         SPACE                                                          05900000
         ST    R0,WORD                  WORD = X'HHMMSSTH'              05910000
         UNPK  WORK,WORD                WORK = C'0HHMMSS*'  *=X'HT'     05920000
         MVC   TIME1(2),WORK+1          SET HOURS                       05930000
         MVC   TIME1+3(2),WORK+3        SET MINUTES                     05940000
         SPACE                                                          05950000
         ST    R1,WORD                  WORD = X'00YYDDDF'              05960000
         UNPK  YYDDD,WORD                                               05970000
         MVC   DATE1+6(2),YYDDD         SET YEAR                        05980000
         SPACE                                                          05990000
         XC    WORK,WORK                WORK = X'00000000 00000000'     06000000
         MVO   WORK+6(2),YEAR           WORK = X'00000000 00000YY0'     06010000
         OI    WORK+7,X'0F'             WORK = X'00000000 00000YYF'     06020000
         CVB   R4,WORK                                                  06030000
         STC   R4,YEAR                  YEAR IN BINARY                  06040000
         MVC   WORK+6(2),DAYS           WORK = X'00000000 0000DDDF'     06050000
         CVB   R4,WORK                                                  06060000
         STH   R4,DAYS                  DAYS IN BINARY                  06070000
         SPACE                                                          06080000
         LA    R3,2                     ASSUME LEAP YEAR                06090000
         TM    YEAR,X'03'               IS YEAR A MULTIPLE OF 4?        06100000
         BZ    LEAPYEAR                 YES - THIS IS A LEAP YEAR       06110000
         LA    R3,2(,R3)                NO - THIS IS NOT A LEAP YEAR    06120000
LEAPYEAR EQU   *                                                        06130000
         LA    R2,DTCVNTAB              ADDR OF DATE CONVERSION TABLE   06140000
         LA    R5,12                    NUMBER OF MONTHS                06150000
CMPDAYS  EQU   *                                                        06160000
         CH    R4,6(R3,R2)              NUMBER OF DAYS UP TO NEXT MONTH 06170000
         BNH   WHICHDAY                 THIS IS THE MONTH               06180000
         LA    R2,6(,R2)                BUMP TO NEXT ENTRY              06190000
         BCT   R5,CMPDAYS               NEXT MONTH, PLEASE              06200000
         B     DATESET                  NOT IN TABLE - FORGET IT        06210000
         SPACE                                                          06220000
WHICHDAY EQU   *                                                        06230000
         SH    R4,0(R3,R2)                                              06240000
         CVD   R4,WORK                                                  06250000
         OI    WORK+7,X'0F'                                             06260000
         UNPK  DATE1+3(2),WORK+6(2)     SET DAY                         06270000
         MVC   DATE1(2),0(R2)           SET MONTH                       06280000
         EJECT                                                          06290000
*        *---------------------------------------------------*          06300000
*        *   I N I T   -   O P E N   R E P O R T   F I L E   *          06310000
*        *---------------------------------------------------*          06320000
         SPACE                                                          06330000
DATESET  EQU   *                                                        06340000
         OPEN  (RPTFIL2,(OUTPUT))                                       06350000
         SPACE                                                          06360000
         TM    RPTFIL2+48,X'10'         IS OPEN SUCCESSFUL?             06370000
         BO    DCBOK                    YES - KEEP ON GOING             06380000
         LA    R15,8                    NO - STOP PROCESSING            06390000
         B     GOHOME                                                   06400000
         SPACE 2                                                        06410000
*        *---------------------------------------------------*          06420000
*        *   I N I T   -   L O A D   S U B R O U T I N E S   *          06430000
*        *---------------------------------------------------*          06440000
         SPACE                                                          06450000
DCBOK    EQU   *                                                        06460000
         LOAD  EP=VSAMLST6                                              06470000
         ST    R0,EPVCL31               ENTRY PT OF REPORT 1 MOD (3.1)  06480000
         SPACE                                                          06490000
         LOAD  EP=VSAMLST7                                              06500000
         ST    R0,EPVCL32               ENTRY PT OF REPORT 2 MOD (3.2)  06510000
         EJECT                                                          06520000
*        *-----------------------------------------------*              06530000
*        *   I N I T   -   C H E C K   V S A M H I S T   *              06540000
*        *-----------------------------------------------*              06550000
         SPACE                                                          06560000
CKVSMHST EQU   *                                                        06570000
         MVI   VSMHSTSW,X'00'           RESET VSAM HISTORY SWITCH       06580000
         DEVTYPE VSAMHIST,DEVINFO       GET SOME DEVICE INFOS           06590000
         SPACE                                                          06600000
         LTR   R15,R15                  IS THERE A DD CARD 'VSAMHIST'?  06610000
         BNZ   NOVSMHST                 NO - CONTINUE W/ VSMHSTSW OFF   06620000
         CLI   DEVCLASS,X'20'           DOES IT POINT TO A DASD DEVICE? 06630000
         BNE   NOVSMHST                 NO - CONTINUE W/ VSMHSTSW OFF   06640000
         SPACE                                                          06650000
         LOAD  EP=VSAMLST8                                              06660000
         ST    R0,EPVCL33               ENTRY PT OF VSAM HIST MOD (3.3) 06670000
         SPACE                                                          06680000
         SR    R1,R1                                                    06690000
         ST    R1,FUNC33                FUNCTION CODE 0 - INIT CALL     06700000
         LA    R1,PARM9433              ADDR OF PARM LIST               06710000
         LR    R15,R0                   ADDR OF VSAMLST8                06720000
         CALL  (15)                                                     06730000
         SPACE                                                          06740000
         LTR   R15,R15                  HOW WAS IT?                     06750000
         BZ    SETHSTSW                 OK - ALL INIT DONE              06760000
         DELETE EP=VSAMLST8             OR ELSE GET RID OF IT           06770000
         B     NOVSMHST                 AND CONTINUE                    06780000
         SPACE                                                          06790000
SETHSTSW EQU   *                                                        06800000
         MVI   VSMHSTSW,X'FF'           READY FOR SOME HISTORY          06810000
         MVI   FUNC33+3,X'04'           LATER ON                        06820000
NOVSMHST EQU   *                                                        06830000
         L     R1,MYSAVE+4              RESTORE PARM LIST POINTER       06840000
         L     R1,24(,R1)               AND                             06850000
         MVI   NOP0+1,X'00'             COME HERE ONLY ONCE             06860000
         B     VCL30                    TAKE IT FROM THE TOP AGAIN      06870000
         SPACE                                                          06880000
VSAMHIST DC    CL8'VSAMHIST'            DD NAME                         06890000
DEVINFO  DC    2F'0'                                                    06900000
DEVCLASS EQU   DEVINFO+2                                                06910000
         EJECT                                                          06920000
*        *-----------------------*                                      06930000
*        *   L A S T   R I D E   *                                      06940000
*        *-----------------------*                                      06950000
         SPACE                                                          06960000
EOSORTIN EQU   *                                                        06970000
         DELETE EP=VSAMLST6                                             06980000
         SPACE                                                          06990000
         DELETE EP=VSAMLST7                                             07000000
         SPACE                                                          07010000
         CLI   VSMHSTSW,X'FF'           IS HISTORY OPTION IN EFFECT?    07020000
         BNE   NO9433D                  NO - BYPASS THIS CLEAN UP       07030000
         MVI   FUNC33+3,X'08'           FUNCTION CODE 8 - FINAL CALL    07040000
         LA    R1,PARM9433              ADDR OF PARM LIST               07050000
         L     R15,EPVCL33              ADDR OF VSAMLST8                07060000
         CALL  (15)                                                     07070000
         SPACE                                                          07080000
         DELETE EP=VSAMLST8                                             07090000
         SPACE                                                          07100000
NO9433D  EQU   *                                                        07110000
         CLOSE RPTFIL2                                                  07120000
         SPACE                                                          07130000
         LA    R15,8                    TELL SORT WE ARE ALL DONE       07140000
         SPACE 2                                                        07150000
*        *-------------*                                                07160000
*        *   E X I T   *                                                07170000
*        *-------------*                                                07180000
         SPACE                                                          07190000
GOHOME   EQU   *                                                        07200000
         L     R13,MYSAVE+4                                             07210000
         RETURN (14,12),RC=(15)                                         07220000
         EJECT                                                          07230000
*        *-------------------------------------*                        07240000
*        *  PRINT ROUTINE FOR THE REPORT FILE  *                        07250000
*        *-------------------------------------*                        07260000
         SPACE                                                          07270000
PUTRPT   EQU   *                                                        07280000
         LH    R1,LINECNT               CURRENT NUMBER OF LINES ON PAGE 07290000
         CH    R1,LINEMAX               IS IT OVER THE LIMIT?           07300000
         BL    SAMEPAGE                 NO - IT'S STILL THE SAME PAGE   07310000
         SPACE 2                                                        07320000
         AP    PAGECNT,=P'1'            YES - TIME FOR A NEW PAGE       07330000
         MVC   PAGE1,PAT3                                               07340000
         ED    PAGE1,PAGECNT            SET NEW PAGE NUMBER             07350000
         PUT   RPTFIL2,TITLE1           PRINT TITLE 1                   07360000
         SPACE                                                          07370000
         CLI   RPTFLAG,X'00'            IS THIS REPORT 1?               07380000
         BNE   RPT2HEAD                 NO - MUST BE REPORT 2           07390000
         SPACE                                                          07400000
RPT1HEAD EQU   *                                                        07410000
         LA    R2,HEADING1                                              07420000
         LA    R3,HEADING2                                              07430000
         B     PRNTHEAD                                                 07440000
         SPACE                                                          07450000
RPT2HEAD EQU   *                                                        07460000
         LA    R2,HEADING3                                              07470000
         LA    R3,HEADING4                                              07480000
         EJECT                                                          07490000
*        *---------------------------------------------*                07500000
*        *  PRINT ROUTINE FOR THE REPORT FILE - CONT.  *                07510000
*        *---------------------------------------------*                07520000
         SPACE                                                          07530000
PRNTHEAD EQU   *                                                        07540000
         PUT   RPTFIL2,(2)              PRINT HEADING                   07550000
         SPACE                                                          07560000
         PUT   RPTFIL2,(3)              PRINT UNDERSCORE                07570000
         SPACE                                                          07580000
         MVI   CNTL,C'0'                SKIP 2 LINES FOR THE NEXT LINE  07590000
         LA    R1,5                     WE HAVE PRINTED 5 LINES ALREADY 07600000
         SPACE 2                                                        07610000
SAMEPAGE EQU   *                                                        07620000
         LA    R1,1(,R1)                                                07630000
         STH   R1,LINECNT               BUMP LINE COUNT                 07640000
         PUT   RPTFIL2,OAREA                                            07650000
         SPACE                                                          07660000
         MVI   CNTL,X'40'                                               07670000
         MVC   LINE,CNTL                                                07680000
         BR    R11                                                      07690000
         SPACE 2                                                        07700000
         LTORG                                                          07710000
         EJECT                                                          07720000
*        *-------------------------------------------*                  07730000
*        *   F O R   D A T E   C O N V E R S I O N   *                  07740000
*        *-------------------------------------------*                  07750000
         SPACE                                                          07760000
WORK     DC    D'0'                                                     07770000
WORD     DC    F'0'                                                     07780000
         ORG   WORD+1                                                   07790000
YEAR     DS    XL1                                                      07800000
DAYS     DS    H                                                        07810000
YYDDD    DC    C'YYDDD'                                                 07820000
         SPACE                                                          07830000
DTCVNTAB DS    0H                                                       07840000
         DC    C'01',H'0',H'0'                                          07850000
         DC    C'02',H'31',H'31'                                        07860000
         DC    C'03',H'60',H'59'                                        07870000
         DC    C'04',H'91',H'90'                                        07880000
         DC    C'05',H'121',H'120'                                      07890000
         DC    C'06',H'152',H'151'                                      07900000
         DC    C'07',H'182',H'181'                                      07910000
         DC    C'08',H'213',H'212'                                      07920000
         DC    C'09',H'244',H'243'                                      07930000
         DC    C'10',H'274',H'273'                                      07940000
         DC    C'11',H'305',H'304'                                      07950000
         DC    C'12',H'335',H'334'                                      07960000
TOTDAYS  DC    C'13',H'366',H'365'                                      07970000
         SPACE 2                                                        07980000
*        *-------------------------------------*                        07990000
*        *   C O N S T A N T   N U M B E R S   *                        08000000
*        *-------------------------------------*                        08010000
         SPACE                                                          08020000
HEXNUM   DC    CL16'0123456789ABCDEF'                                   08030000
HEXTBL   EQU   HEXNUM-240                                               08040000
         SPACE                                                          08050000
ZERO     DC    F'0'                                                     08060000
FOXES    DC    F'-1'                                                    08070000
HUNDRED  DC    H'100'                                                   08080000
         EJECT                                                          08090000
*        *------------------------------------------------------------* 08100000
*        *  THIS TABLE IS INDEXED BY DASD CLASS OF UCBTYP FIELD OF    * 08110000
*        *  UCB AS DESCRIBED IN OS/VS1 SYSTEM DATA AREAS (SY28-0605). * 08120000
*        *  EACH ENTRY IS 8 BYTES LONG WITH THE FOLLOWING,            * 08130000
*        *    BYTE 0   :  DASD CLASS IN HEXADECIMAL FORMAT            * 08140000
*        *    BYTE 1-7 :  DASD TYPE IN CHARACTER FORMAT -             * 08150000
*        *                A BLANK FIRST BYTE IMPLIES ENTRY NOT USED   * 08160000
*        *------------------------------------------------------------* 08170000
         SPACE                                                          08180000
TYPEDASD DS    0F                                                       08190000
         DC    X'00',CL7' '             * * *   NO SUCH DEVICE   * * *  08200000
         DC    X'01',CL7'2311'          2311 DISK STORAGE DRIVE         08210000
         DC    X'02',CL7'2301'          2301 PARALLEL DRUM              08220000
         DC    X'03',CL7'2303'          2303 SERIAL DRUM                08230000
         DC    X'04',CL7'2302'          2302 DISK STORAGE               08240000
         DC    X'05',CL7'2321'          2321 DATA CELL DRIVE            08250000
         DC    X'06',CL7'2305-1'        2305 FIXED HEAD STORAGE MODEL 1 08260000
         DC    X'07',CL7'2305-2'        2305 FIXED HEAD STORAGE MODEL 2 08270000
         DC    X'08',CL7'2314'          2314/2319 DIRECT ACCESS STORAGE 08280000
         DC    X'09',CL7'3330'          3330/3333 MODEL 1 DISK STORAGE  08290000
         DC    X'0A',CL7'3340'          3340 DISK STORAGE               08300000
         DC    X'0B',CL7'3350'          3350 DIRECT ACCESS STORAGE      08310000
         DC    X'0C',CL7' '             * * *   NO SUCH DEVICE   * * *  08320000
         DC    X'0D',CL7'3330-1'        3330/3333 MODEL 11 DISK STORAGE 08330000
TYPEDEND EQU   *                                                        08340000
         EJECT                                                          08350000
*        *-----------------------------------------------*              08360000
*        *  USED FOR MULTIPLE RECORDS WITH THE SAME KEY  *              08370000
*        *-----------------------------------------------*              08380000
         SPACE                                                          08390000
SAVEDREC DS    0CL92                                                    08400000
SAVVOLNO DS    CL6                      VOLUME SERIAL NUMBER            08410000
SAVKEYSQ DS    0CL6                     SORT KEY - LTH OF 6             08420000
SAVRCKEY DS    CL4                      RECORD KEY - GENERATED SORT KEY 08430000
SAVRCSEQ DS    CL2                      RECORD SEQ - SEQ NO IN 'NAMEDS' 08440000
SAVENTNM DS    CL44                     ENTRY NAME                      08450000
SAVENTYP DS    CL1                      ENTRY TYPE                      08460000
SAVCREDT DS    CL3                      CREATION DATE                   08470000
SAVEXPDT DS    CL3                      EXPIRATION DATE                 08480000
SAVCRAVL DS    CL6                      CRA VOLUME SERIAL NUMBER        08490000
SAVSCALC DS    CL3                      SECONDARY SPACE ALLOCATION      08500000
SAVSCTYP DS    CL1                      SPACE ALLOCATION TYPE           08510000
SAVDSORG DS    CL1                      DATA SET ORGANIZATION (AMDATTR) 08520000
SAVCISIZ DS    CL4                      CONTROL INTERVAL SIZE           08530000
SAVMAXRC DS    CL4                      MAXIMUM RECORD SIZE             08540000
SAVAVGRC DS    CL4                      AVERAGE RECORD SIZE             08550000
SAVTRKAL DS    H                        TRK'S ALLOC'D FOR DATA SET      08560000
SAVTRKUS DS    H                        TRK'S USED FOR DATA SET         08570000
SAVPCTUS DS    CL1                      PERCENT OF USAGE                08580000
SAVNOEXT DS    CL1                      NO. OF EXTENTS FOR DATA SET     08590000
         EJECT                                                          08600000
*        *-----------------------------------------------*              08610000
*        *   F O R   R E P O R T   F O R M A T T I N G   *              08620000
*        *-----------------------------------------------*              08630000
         SPACE                                                          08640000
LINECNT  DC    H'0'                                                     08650000
LINEMAX  DC    H'60'                                                    08660000
PAGECNT  DC    PL2'0'                                                   08670000
PAT3     DC    XL4'40202120'                                            08680000
PAT4     DC    XL5'4020202120'                                          08690000
PAT5     DC    XL6'402020202120'                                        08700000
OAREA    DS    0CL133                                                   08710000
CNTL     DC    X'40'                                                    08720000
LINE     DC    CL132' '                                                 08730000
         SPACE                                                          08740000
VSMHSTSW DC    X'00'                    X'FF' IF VSAM HISTORY OPTION    08750000
RPTFLAG  DC    X'00'                    X'00' - RPT 1, X'FF' - RPT 2    08760000
RECPEND  DC    X'00'                    X'FF' IF A RECORD IS PENDING    08770000
         SPACE                                                          08780000
TITLES   DS    0F                                                       08790000
         DC    A(TITLE1)                                                08800000
         DC    A(TITLE2)                                                08810000
         DC    A(TITLE3)                                                08820000
         DC    A(TITLE4)                                                08830000
ATITLE5  DC    A(TITLE5)                                                08840000
         DC    A(HEADING1)                                              08850000
         DC    A(HEADING2)                                              08860000
TITLENO  EQU   (*-TITLES)/4                                             08870000
         SPACE 2                                                        08880000
*        *-------------------------------------------------------*      08890000
*        *   P A R M   L I S T S   &   E N T R Y   P O I N T S   *      08900000
*        *-------------------------------------------------------*      08910000
         SPACE                                                          08920000
PARM9431 DS    0F                                                       08930000
RECD31   DC    A(0)                                                     08940000
LINE31   DC    A(LINE)                                                  08950000
         SPACE                                                          08960000
PARM9432 DS    0F                                                       08970000
RECD32   DC    A(0)                                                     08980000
LINE32   DC    A(LINE)                                                  08990000
         SPACE                                                          09000000
PARM9433 DS    0F                                                       09010000
FUNC33   DC    A(0)                                                     09020000
RECD33   DC    A(0)                                                     09030000
         SPACE                                                          09040000
EPVCL31  DC    A(0)                                                     09050000
EPVCL32  DC    A(0)                                                     09060000
EPVCL33  DC    A(0)                                                     09070000
         EJECT                                                          09080000
RPTFIL2  DCB   DDNAME=RPTFIL2,DSORG=PS,MACRF=(PM)                       09090000
         EJECT                                                          09100000
*        *---------------------------------------------------*          09110000
*        *   T I T L E   F O R   R E P O R T S   1   &   2   *          09120000
*        *---------------------------------------------------*          09130000
         SPACE                                                          09140000
TITLE1   DS    0CL133                                                   09150000
         DC    C'1',CL14'             ',CL16'              '            09160000
         DC    CL19'PGM=VSAMLIST'                                       09170000
SUBTIT1  DC    CL25' '                                                  09180000
         DC    CL5' FOR'                                                09190000
VOLSER1  DC    CL6'123456'                                              09200000
         DC    CL12' ',CL5'DATE'                                        09210000
DATE1    DC    CL8'MM/DD/YY'                                            09220000
         DC    CL7'  TIME'                                              09230000
TIME1    DC    CL5'HH:MM'                                               09240000
         DC    CL6'  PAGE'                                              09250000
PAGE1    DC    CL4' 000'                                                09260000
         SPACE                                                          09270000
SUBTIT11 DC    CL25'     VSAM VOLUME CONTENTS'                          09280000
SUBTIT21 DC    CL25'VSAM TRACK ALLOCATION MAP'                          09290000
         EJECT                                                          09300000
*        *-------------------------------------------*                  09310000
*        *   T I T L E S   F O R   R E P O R T   1   *                  09320000
*        *-------------------------------------------*                  09330000
         SPACE                                                          09340000
TITLE2   DS    0CL133                                                   09350000
         DC    C'0',CL28' DEVICE DESCRIPTION:  TYPE='                   09360000
DEVTYPE2 DC    CL8'DEVTYPE'                                             09370000
         DC    CL6'  USE='                                              09380000
VOLUSE2  DC    CL7'UCBSTAB'                                             09390000
         DC    CL10'  CYL/VOL='                                         09400000
CYLVOL2  DC    CL5' NNNN'                                               09410000
         DC    CL10'  TRK/CYL='                                         09420000
TRKCYL2  DC    CL4' NNN'                                                09430000
         DC    CL11'  BYTE/TRK='                                        09440000
BYTTRK2  DC    CL6' NNNNN'                                              09450000
         DC    CL37' '                                                  09460000
         SPACE                                                          09470000
TITLE3   DS    0CL133                                                   09480000
         DC    C'0',CL35' VOLUME INFORMATION:  VSAM DSN/VOL='           09490000
DSNVOL3  DC    CL6' NNNNN'                                              09500000
         DC    CL11'  DSPC/VOL='                                        09510000
DSPCVOL3 DC    CL4' NNN'                                                09520000
         DC    CL16'  MAX-EXT/ALLOC='                                   09530000
MXTALLC3 DC    CL4' NNN'                                                09540000
         DC    CL9'  CATLG='                                            09550000
CATNAME3 DC    CL44'VSAM.CATALOG.NAME'                                  09560000
         DC    CL3' '                                                   09570000
         SPACE                                                          09580000
TITLE4   DS    0CL133                                                   09590000
         DC    C'0',CL24' AVAILABLE SPACE:  VSAM='                      09600000
VSFSNUM4 DC    CL6' NNNNN'                                              09610000
         DC    CL18' EXTENTS, TOTAL OF'                                 09620000
VSFSTRK4 DC    CL6' NNNNN'                                              09630000
         DC    CL23' TRACKS. **************'                            09640000
         DC    CL38'****************  LARGEST FREE EXTENT='             09650000
VSFSLGE4 DC    CL6' NNNNN'                                              09660000
         DC    CL11' TRACKS'                                            09670000
         SPACE                                                          09680000
TITLE5   DS    0CL133                                                   09690000
         DC    C'0',CL21' ',CL3'OS='                                    09700000
OSFSNUM5 DC    CL6' NNNNN'                                              09710000
         DC    CL18' EXTENTS, TOTAL OF'                                 09720000
OSFSTRK5 DC    CL6' NNNNN'                                              09730000
         DC    CL18' TRACKS, INCLUDING'                                 09740000
OSFSCYL5 DC    CL5' NNNN'                                               09750000
         DC    CL38' FULL CYLINDERS.  LARGEST FREE EXTENT='             09760000
OSFSLGE5 DC    CL6' NNNNN'                                              09770000
         DC    CL11' TRACKS'                                            09780000
         SPACE                                                          09790000
TITLE5A  DS    0CL133                                                   09800000
         DC    C'0',CL21' ',CL111'OS SPACE INFORMATION NOT AVAILABLE'   09810000
         EJECT                                                          09820000
*        *-----------------------------------------------*              09830000
*        *   H E A D I N G S   F O R   R E P O R T   1   *              09840000
*        *-----------------------------------------------*              09850000
         SPACE                                                          09860000
HEADING1 DS    0CL133                                                   09870000
         DC    C'-'                                                     09880000
         DC    C' V S A M   E N T R Y   N A M E                '        09890000
         DC    C'TYPE  CRE DATE EXP DATE ORG   CISZ  AVG R MAX R '      09900000
         DC    C'TRKAL TRKUS PCT EXT SECAL T RCV-VL F  '                09910000
         SPACE                                                          09920000
HEADING2 DS    0CL133                                                   09930000
         DC    C'+'                                                     09940000
         DC    1C' ',44C'_',1C' ',5C'_',1C' ',8C'_',1C' ',8C'_'         09950000
         DC    1C' ',5C'_',1C' ',5C'_',1C' ',5C'_',1C' ',5C'_'          09960000
         DC    1C' ',5C'_',1C' ',5C'_',1C' ',3C'_',1C' ',3C'_'          09970000
         DC    1C' ',5C'_',1C' ',1C'_',1C' ',6C'_',1C' ',1C'_',2C' '    09980000
         SPACE 2                                                        09990000
*        *-----------------------------------------------*              10000000
*        *   H E A D I N G S   F O R   R E P O R T   2   *              10010000
*        *-----------------------------------------------*              10020000
         SPACE                                                          10030000
HEADING3 DS    0CL133                                                   10040000
         DC    C'-'                                                     10050000
         DC    C'          F I R S T   T K  L A S T   T R K  '          10060000
         DC    C'TRKAL  TRKAV  EXT  E N T R Y   N A M E   O R   '       10070000
         DC    C'U S A G E         EXCEPTIONS IF ANY      '             10080000
         SPACE                                                          10090000
HEADING4 DS    0CL133                                                   10100000
         DC    C'+'                                                     10110000
         DC    10C' ',15C'_',2C' ',15C'_',2C' ',5C'_',2C' ',5C'_'       10120000
         DC    2C' ',3C'_',2C' ',44C'_',2C' ',17C'_',6C' '              10130000
         SPACE                                                          10140000
ENDING1  DC    C'***** END OF TRACK MAP *****'                          10150000
         SPACE                                                          10160000
         DS    0H                                                       10170000
         EJECT                                                          10180000
INDTAIL  DSECT                                                          10190000
INDVOLNO DS    CL6                      VOLUME SERIAL NUMBER            10200000
INDRPTNO DS    0CL1                     REPORT NO. B'0... ....' DETAILS 10210000
INDRCKEY DS    CL4                      RECORD KEY - GENERATED SORT KEY 10220000
INDRCSEQ DS    CL2                      RECORD SEQ - SEQ NO IN 'NAMEDS' 10230000
INDENTNM DS    CL44                     ENTRY NAME                      10240000
INDENTYP DS    CL1                      ENTRY TYPE                      10250000
INDCREDT DS    CL3                      CREATION DATE                   10260000
INDEXPDT DS    CL3                      EXPIRATION DATE                 10270000
INDCRAVL DS    CL6                      CRA VOLUME SERIAL NUMBER        10280000
INDSCALC DS    CL3                      SECONDARY SPACE ALLOCATION      10290000
INDSCTYP DS    CL1                      SPACE ALLOCATION TYPE           10300000
INDDSORG DS    CL1                      DATA SET ORGANIZATION (AMDATTR) 10310000
INDCISIZ DS    CL4                      CONTROL INTERVAL SIZE           10320000
INDMAXRC DS    CL4                      MAXIMUM RECORD SIZE             10330000
INDAVGRC DS    CL4                      AVERAGE RECORD SIZE             10340000
INDTRKAL DS    H                        TRK'S ALLOC'D FOR DATA SET      10350000
INDTRKUS DS    H                        TRK'S USED FOR DATA SET         10360000
INDPCTUS DS    CL1                      PERCENT OF USAGE                10370000
INDNOEXT DS    CL1                      NO. OF EXTENTS FOR DATA SET     10380000
         EJECT                                                          10390000
INVLCHR  DSECT                                                          10400000
INVVOLNO DS    CL6                      VOLUME SERIAL NUMBER            10410000
INVRPTNO DS    0CL1                     REPORT NO. B'X... ....'  X=0,1  10420000
INVRCKEY DS    CL4                      RECORD KEY - ZERO FOR THIS REC  10430000
INVOLUSE DS    CL2                      VOLUME USE (UCBSTAB)            10440000
INVDVTYP DS    CL4                      DEVICE TYPE                     10450000
INVCYLVL DS    H                        NO. OF CYLINDERS PER VOLUME     10460000
INVTRKCL DS    H                        NO. OF TRACKS PER CYLINDER      10470000
INVBYTTK DS    H                        NO. OF BYTES PER TRACK          10480000
INVNODST DS    H                        NUMBER OF DATA SETS ON VOLUME   10490000
INVNODSP DS    H                        NUMBER OF DATA SPACE ON VOLUME  10500000
INVSYSXT DS    H                        SYSTEM ALLOWED EXTENTS          10510000
INVCATNM DS    CL44                     VSAM CATALOG NAME               10520000
INVVSEXT DS    H                        NUMBER OF VSAM FREE EXTENTS     10530000
INVVSTRK DS    H                        NUMBER OF VSAM FREE TRACKS      10540000
INVVSLGE DS    H                        TRACKS OF VSAM LARGEST FREE EXT 10550000
INVOSEXT DS    H                        NUMBER OF OS FREE EXTENTS       10560000
INVOSTRK DS    H                        NUMBER OF OS FREE TRACKS        10570000
INVOSCYL DS    H                        NUMBER OF OS FREE CYLINDERS     10580000
INVOSLGE DS    H                        TRACKS OF OS LARGEST FREE EXT   10590000
         EJECT                                                          10600000
INEXTNT  DSECT                                                          10610000
INEVOLNO DS    CL6                      VOLUME SERIAL NUMBER            10620000
INERPTNO DS    0CL1                     REPORT NO. B'1... ....' EXTENTS 10630000
INELOTCH DS    0CL4                     LOW CCHH OF EXTENT (SORT KEY)   10640000
INELOCC  DS    H                                                        10650000
INELOHH  DS    H                                                        10660000
INERCKEY DS    H                        H'0' FOR DSP EXT, H'1' OTHERS   10670000
INEXTNM  DS    CL44                     DATA SET NAME (ENTRY NAME)      10680000
INEHITCH DS    0CL4                     HIGH CCHH OF EXTENT             10690000
INEHICC  DS    H                                                        10700000
INEHIHH  DS    H                                                        10710000
INEXTLTH DS    H                        LENGTH OF EXTENT IN TRACKS      10720000
INELOTRK DS    H                        LOW TRACK NUMBER                10730000
INEHITRK DS    H                        HIGH TRACK NUMBER               10740000
INEXTNO  DS    H                        EXTENT NUMBER                   10750000
         END                                                            10760000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD                             00210000
//LKED.SYSIN DD *                                                       00220000
  NAME VSAMLST5(R)                                                      00230000
//A        EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF'               00120000
//ASM.SYSIN DD *                                                        00130000
         PRINT OFF                                                      00010001
         MACRO                                                          00020001
&NAME    INIT &BASE=3,&REGS=Y,&PATCH=3,&RENT=N,&SAVE=Y                  00030001
         AIF   ((&BASE LT 13) AND (&BASE GT 1)).N020                    00040001
         MNOTE 12,'INVALID BASE REGISTER'                               00050001
         MEXIT                                                          00060001
.N020    ANOP                                                           00070001
         PUSH  PRINT                                                    00080001
         PRINT ON,GEN                                                   00090001
         EJECT                                                          00100001
&SYSECT  CSECT                                                          00110001
         USING *,15                                                     00120001
         B     BEGIN                                                    00130001
         DC    AL1(24)                                                  00140001
         DC    CL8'&SYSECT'                                             00150001
         DC    CL16'-&SYSDATE-&SYSTIME'                                 00160001
         AIF   ('&RENT' EQ 'Y').N004                                    00170001
MYSAVE   DC    18F'-1'                                                  00180001
.N004    ANOP                                                           00190001
         AIF   ('&PATCH' EQ '0').N005                                   00200001
PATCH    DC    &PATCH.CL8'*PATCH*'                                      00210001
.N005    ANOP                                                           00220001
         AIF   ('&REGS' EQ 'N').N030                                    00230001
         AIF   ('&REGS' EQ 'Y').N010                                    00240001
         MNOTE 4,'REGS OPERAND INVALID. Y SUBSTITUTED'                  00250001
.N010    ANOP                                                           00260001
R0       EQU   0                                                        00270001
R1       EQU   1                                                        00280001
R2       EQU   2                                                        00290001
R3       EQU   3                                                        00300001
R4       EQU   4                                                        00310001
R5       EQU   5                                                        00320001
R6       EQU   6                                                        00330001
R7       EQU   7                                                        00340001
R8       EQU   8                                                        00350001
R9       EQU   9                                                        00360001
R10      EQU   10                                                       00370001
R11      EQU   11                                                       00380001
R12      EQU   12                                                       00390001
R13      EQU   13                                                       00400001
R14      EQU   14                                                       00410001
R15      EQU   15                                                       00420001
.N030    ANOP                                                           00430001
BEGIN    DS   0H                                                        00440001
         STM   14,12,12(13)                                             00450001
         LR    &BASE,15                                                 00460001
         DROP  15                                                       00470001
         USING &SYSECT,&BASE                                            00480001
         AIF   ('&SAVE' EQ 'N').N003                                    00490001
         AIF   ('&RENT' EQ 'Y').N002                                    00500001
         AIF   ('&RENT' EQ 'N').N001                                    00510001
         MNOTE 4,'RENT OPERAND INVALID. N SUBSTITUTED'                  00520001
.N001    ANOP                                                           00530001
         ST    13,MYSAVE+4                                              00540001
         LR    15,13                                                    00550001
         LA    13,MYSAVE                                                00560001
         ST    13,8(15)                                                 00570001
         AGO   .N003                                                    00580001
.N002    ANOP                                                           00590001
         GETMAIN R,LV=72                                                00600001
         ST    13,4(1)                                                  00610001
         ST    1,8(13)                                                  00620001
         LR    13,1                                                     00630001
.N003    ANOP                                                           00640001
         POP   PRINT                                                    00650001
         EJECT                                                          00660001
         MEND                                                           00670001
         PRINT ON                                                       00680001
*  MODULE NAME:         VSAMLST6     (REL. 1.1  08/10/79)               00690001
*                                                                       00700001
*  MODULE DESCRIPTION:  VSAM CATALOG LIST UTILITY - REPORT 1 FORMATTER  00710001
*                                                                       00720001
*  RETURN LINKAGE:      RETURN (14,12),RC=(15)                          00730001
*                                                                       00740001
*  LINKAGE TABLE:       NONE - SINGLE ENTRY                             00750001
*                                                                       00760001
*  PARAMETERS:          R1 POINTS TO THE ADDRESS OF A 92-BYTES RECORD   00770001
*                       FROM SORT/VSAMLST5                              00780001
*                                                                       00790001
*  EXIT:                RC=00 - SUCCESSFUL                              00800001
*                                                                       00810001
*  MODULE FUNCTION:     FOR EACH RECORD FROM SORT/VSAMLST5, THIS MODULE 00820001
*                       FORMATS IT FOR THE VSAM VOLUME CONTENTS REPORT. 00830001
*                                                                       00840001
*  CALLER:              VSAMLST5                                        00850001
*                                                                       00860001
*  CALLS:               NONE                                            00870001
*                                                                       00880001
*  SYSTEMS SERVICES:    NONE                                            00890001
*                                                                       00900001
*  MODULE ENVIRONMENT:  OS/VS1                                          00910001
         TITLE 'VSAMLST6 --- VSAM CATALOG LIST UTILITY REPORT 1 FORMATTX00920001
                ER(3.1) - VSAM VOLUME CONTENTS'                         00930001
*        *-----------------------------------*                          00940001
*        *   R E G I S T E R   U S A G E S   *                          00950001
*        *-----------------------------------*                          00960001
*                                                                       00970001
*  R0  -  STANDARD LINKAGE                                              00980001
*  R1  -  STANDARD LINKAGE (ADDR OF PARM LIST) & TEMP WORK SPACE        00990001
*  R2  -  TEMP WORK SPACE                                               01000001
*  R3  -  TEMP WORK SPACE                                               01010001
*  R4  -  TEMP WORK SPACE                                               01020001
*  R5  -  TEMP WORK SPACE                                               01030001
*  R6  -  NOT USED                                                      01040001
*  R7  -  NOT USED                                                      01050001
*  R8  -  NOT USED                                                      01060001
*  R9  -  BASE REG FOR DSECT INDTAIL                                    01070001
*  R10 -  BASE REG FOR DSECT TXTVCTNT                                   01080001
*  R11 -  RETURN ADDR FOR SUBROUTINES                                   01090001
*  R12 -  BASE REG FOR CSECT VSAMLST6                                   01100001
*  R13 -  STANDARD LINKAGE (ADDR OF SAVE AREA)                          01110001
*  R14 -  STANDARD LINKAGE (ADDR TO RETURN)                             01120001
*  R15 -  STANDARD LINKAGE (ADDR OF ENTRY POINT & RETURN CODE)          01130001
         SPACE 3                                                        01140001
VSAMLST6 CSECT                                                          01150001
         INIT  BASE=12                                                  01160001
*********************************************************************** 01170001
*                                                                     * 01180001
*  3.1    VSAM VOLUME CONTENTS REPORT FORMATTER                       * 01190001
*                                                                     * 01200001
*     FUNCTION:  FOR EACH RECORD PASSED TO THIS MODULE, IT FORMATS    * 01210001
*                IT FOR THE VSAM VOLUME CONTENTS REPORT.  THIS        * 01220001
*                REPORT CONTAINS THE DEVICE DESCRIPTION, THE VOLUME   * 01230001
*                INFORMATION, THE AVAILABLE SPACE (OS AND VSAM),      * 01240001
*                ENTRY NAME, ENTRY TYPE, CREATION DATE, EXPIRATION    * 01250001
*                DATE, ENTRY ORGANIZATION, CONTROL INTERVAL SIZE,     * 01260001
*                AVERAGE RECORD SIZE, MAXIMUM RECORD SIZE, TRACKS     * 01270001
*                ALLOCATED, TRACKS USED, PERCENTS OF USAGE, NUMBER    * 01280001
*                OF EXTENTS, SECONDARY ALLOCATION AND ITS TYPE,       * 01290001
*                RECOVERY VOLUME AND AN ENTRY FLAG WHICH IS SET TO    * 01300001
*                AN '*' IF SPACE USED IS MORE THAN 85 PERCENTS.       * 01310001
*                                                                     * 01320001
*     ERRORS:  NONE                                                   * 01330001
*                                                                     * 01340001
*********************************************************************** 01350001
         SPACE 3                                                        01360001
VCL31    EQU   *                                                        01370001
         USING INDTAIL,R9                                               01380001
         USING TXTVCTNT,R10                                             01390001
         LM    R9,R10,0(R1)                                             01400001
         SPACE                                                          01410001
         MVC   TXTENTNM,INDENTNM        ENTRY NAME                      01420001
         SR    R2,R2                    CLEAR INDEX                     01430001
         TRT   INDENTYP,RECTBL          IS RECORD TYPE DEFINED?         01440001
         BZ    SETCREDT                 NO - SKIP THIS FIELD            01450001
         BCTR  R2,0                     YES - PROCESS IT                01460001
         MH    R2,FIVE                  DISPL = ( INDEX - 1 ) * 5       01470001
         LA    R1,ENTYPE(R2)            A(ENTYP) = A(ENTYPTAB) + DISPL  01480001
         MVC   TXTENTYP,0(R1)           ENTRY TYPE                      01490001
         SPACE                                                          01500001
SETCREDT EQU   *                                                        01510001
         CLC   INDCREDT(2),ZERO         IS IT A ZERO DATE?              01520001
         BE    SETEXPDT                 YES - SKIP IT                   01530001
         LA    R1,INDCREDT              NO - CONVERT IT                 01540001
         BAL   R11,CNVDATE                                              01550001
         MVC   TXTCREDT,0(R1)           CREATION DATE                   01560001
         SPACE                                                          01570001
SETEXPDT EQU   *                                                        01580001
         CLC   INDEXPDT(2),ZERO         IS IT A ZERO DATE?              01590001
         BE    SETDSORG                 YES - SKIP IT                   01600001
         LA    R1,INDEXPDT              NO - CONVERT IT                 01610001
         BAL   R11,CNVDATE                                              01620001
         MVC   TXTEXPDT,0(R1)           EXPIRATION DATE                 01630001
         EJECT                                                          01640001
SETDSORG EQU   *                                                        01650001
         CLI   INDDSORG,X'FF'           IS THIS AN 'ASSOC' RECORD?      01660001
         BNE   NOTASSOC                 NO - NEXT TEST                  01670001
         MVC   TXTDSORG(5),=C'ASSOC'    YES - SET IT                    01680001
         B     CHKTYPE                  AND SEE IF THERE ARE MORE       01690001
         SPACE                                                          01700001
NOTASSOC EQU   *                                                        01710001
         CLI   INDENTYP,C'I'            IS THIS AN 'INDEX' RECORD?      01720001
         BNE   NOTINDEX                 NO - NEXT TEST                  01730001
         MVC   TXTDSORG(3),=C'IXD'      YES - SET IT                    01740001
         B     CHKTYPE                  AND SEE IF THERE ARE MORE       01750001
         SPACE                                                          01760001
NOTINDEX EQU   *                                                        01770001
         TM    INDDSORG,X'80'           IS THIS A 'KSDS' ENTRY?         01780001
         BZ    NOTAKSDS                 NO - NEXT TEST                  01790001
         MVC   TXTDSORG(3),=C'IXD'      YES - SET IT                    01800001
         B     CHKTYPE                  AND SEE IF THERE ARE MORE       01810001
         SPACE                                                          01820001
NOTAKSDS EQU   *                                                        01830001
         TM    INDDSORG,X'02'           IS THIS AN 'ESDS' ENTRY?        01840001
         BO    NOTAESDS                 NO - IT'S AN 'RRDS' ENTRY       01850001
         MVC   TXTDSORG(4),=C'NIXD'     YES - SET IT                    01860001
         B     CHKTYPE                  AND SEE IF THERE ARE MORE       01870001
         SPACE                                                          01880001
NOTAESDS EQU   *                                                        01890001
         MVC   TXTDSORG(4),=C'NUMD'     ENTRY ORGANIZATION              01900001
         SPACE 2                                                        01910001
CHKTYPE  EQU   *                                                        01920001
         CLI   INDENTYP,C'D'            IF IT IS A TYPE 'D'             01930001
         BE    RPT1PAR2                 OR                              01940001
         CLI   INDENTYP,C'I'            TYPE 'I'                        01950001
         BE    RPT1PAR2                 THEN MORE PROCESSING            01960001
         B     GOHOME                   ELSE GO HOME                    01970001
         EJECT                                                          01980001
*        *------------------------------------------------------------* 01990001
*        *  REPORT 1 PART II - MORE INFO'S ON TYPE 'D' & 'I' RECORDS  * 02000001
*        *------------------------------------------------------------* 02010001
         SPACE                                                          02020001
RPT1PAR2 EQU   *                                                        02030001
         ICM   R1,15,INDCISIZ           CONTROL INTERVAL SIZE           02040001
         CVD   R1,WORK                                                  02050001
         MVC   TXTCISIZ,PAT5                                            02060001
         ED    TXTCISIZ,WORK+5                                          02070001
         SPACE                                                          02080001
         ICM   R1,15,INDAVGRC           AVERAGE RECORD SIZE             02090001
         BP    SETAVGRC                 ONLY IF IT'S POSITIVE           02100001
         MVI   TXTAVGRC+5,C'0'          EITHER ALL X'00' OR ALL X'FF'   02110001
         B     SETMAXRC                 NEXT FIELD                      02120001
         SPACE                                                          02130001
SETAVGRC EQU   *                                                        02140001
         CVD   R1,WORK                                                  02150001
         MVC   TXTAVGRC,PAT5                                            02160001
         ED    TXTAVGRC,WORK+5                                          02170001
         SPACE                                                          02180001
SETMAXRC EQU   *                                                        02190001
         ICM   R1,15,INDMAXRC           MAXIMUM RECORD SIZE             02200001
         CVD   R1,WORK                                                  02210001
         MVC   TXTMAXRC,PAT5                                            02220001
         ED    TXTMAXRC,WORK+5                                          02230001
         SPACE                                                          02240001
         LH    R1,INDTRKAL              TRACKS ALLOCATED                02250001
         CVD   R1,WORK                                                  02260001
         MVC   TXTTRKAL,PAT5                                            02270001
         ED    TXTTRKAL,WORK+5                                          02280001
         SPACE                                                          02290001
         LH    R1,INDTRKUS              TRACKS USED                     02300001
         CVD   R1,WORK                                                  02310001
         MVC   TXTTRKUS,PAT5                                            02320001
         ED    TXTTRKUS,WORK+5                                          02330001
         SPACE                                                          02340001
         SR    R1,R1                    PERCENTS OF USAGE               02350001
         IC    R1,INDPCTUS                                              02360001
         CVD   R1,WORK                                                  02370001
         MVC   TXTPCTUS,PAT3                                            02380001
         ED    TXTPCTUS,WORK+6                                          02390001
         SPACE                                                          02400001
         IC    R1,INDNOEXT              NUMBER OF EXTENTS               02410001
         CVD   R1,WORK                                                  02420001
         MVC   TXTNOEXT,PAT3                                            02430001
         ED    TXTNOEXT,WORK+6                                          02440001
         EJECT                                                          02450001
*        *----------------------------*                                 02460001
*        *  REPORT 1 PART II - CONT.  *                                 02470001
*        *----------------------------*                                 02480001
         SPACE                                                          02490001
         ICM   R1,7,INDSCALC            SECONDARY SPACE ALLOCATION      02500001
         CVD   R1,WORK                                                  02510001
         MVC   TXTSCALC,PAT5                                            02520001
         ED    TXTSCALC,WORK+5                                          02530001
         SPACE                                                          02540001
         TM    INDSCTYP,X'C0'           SPACE ALLOCATION TYPE           02550001
         BNO   NOTCYL                                                   02560001
         MVI   TXTSCTYP,C'C'            B'11.. ....' - CYLINDERS        02570001
         B     SETCRAVL                                                 02580001
         SPACE                                                          02590001
NOTCYL   EQU   *                                                        02600001
         TM    INDSCTYP,X'80'                                           02610001
         BZ    SETCRAVL                                                 02620001
         MVI   TXTSCTYP,C'T'            B'10.. ....' - TRACKS           02630001
         SPACE                                                          02640001
SETCRAVL EQU   *                                                        02650001
         CLI   INDCRAVL,X'00'           IS THERE A RECOVERY VOLUME?     02660001
         BE    SETENTFL                 NO - SKIP THIS FIELD            02670001
         MVC   TXTCRAVL,INDCRAVL        YES - MOVE IT                   02680001
         SPACE                                                          02690001
SETENTFL EQU   *                                                        02700001
         CLI   INDPCTUS,85              ARE 85 PERCENTS OR MORE USED?   02710001
         BL    GOHOME                   NO - ALL DONE                   02720001
         MVI   TXTENTFL,C'*'            YES - INDICATE SO               02730001
         SPACE 2                                                        02740001
*        *-------------*                                                02750001
*        *   E X I T   *                                                02760001
*        *-------------*                                                02770001
         SPACE                                                          02780001
GOHOME   EQU   *                                                        02790001
         L     R13,MYSAVE+4                                             02800001
         RETURN (14,12),RC=(15)                                         02810001
         SPACE 2                                                        02820001
         LTORG                                                          02830001
         EJECT                                                          02840001
*        *-----------------------*                                      02850001
*        *   C O N S T A N T S   *                                      02860001
*        *-----------------------*                                      02870001
         SPACE                                                          02880001
ZERO     DC    H'0'                                                     02890001
FIVE     DC    H'5'                                                     02900001
PAT3     DC    XL4'40202120'                                            02910001
PAT5     DC    XL6'402020202120'                                        02920001
         SPACE 2                                                        02930001
RECTBL   DC    256X'00'                 TYPE  0 - UNDEFINED             02940001
         ORG   RECTBL+C'A'                                              02950001
         DC    X'01'                    TYPE  1 - NON VSAM              02960001
         ORG   RECTBL+C'B'                                              02970001
         DC    X'02'                    TYPE  2 - GENERATION DATA GROUP 02980001
         ORG   RECTBL+C'C'                                              02990001
         DC    X'03'                    TYPE  3 - CLUSTER               03000001
         ORG   RECTBL+C'D'                                              03010001
         DC    X'04'                    TYPE  4 - DATA                  03020001
         ORG   RECTBL+C'G'                                              03030001
         DC    X'05'                    TYPE  5 - ALTERNATE INDEX       03040001
         ORG   RECTBL+C'I'                                              03050001
         DC    X'06'                    TYPE  6 - INDEX                 03060001
         ORG   RECTBL+C'R'                                              03070001
         DC    X'07'                    TYPE  7 - PATH                  03080001
         ORG   RECTBL+C'U'                                              03090001
         DC    X'08'                    TYPE  8 - USER CATALOG          03100001
         ORG   RECTBL+C'V'                                              03110001
         DC    X'09'                    TYPE  9 - SPACE (VOLUME)        03120001
         ORG   RECTBL+C'X'                                              03130001
         DC    X'0A'                    TYPE 10 - ALIAS                 03140001
         ORG   RECTBL+C'Y'                                              03150001
         DC    X'0B'                    TYPE 11 - UPGRADE               03160001
         ORG                                                            03170001
         SPACE 2                                                        03180001
ENTYPE   DS    0CL40                                                    03190001
         DC    CL5'NVSAM'               A                               03200001
         DC    CL5'GDG'                 B                               03210001
         DC    CL5'CL'                  C                               03220001
         DC    CL5'DATA'                D                               03230001
         DC    CL5'AIX'                 G                               03240001
         DC    CL5'INDEX'               I                               03250001
         DC    CL5'PATH'                R                               03260001
         DC    CL5'U-CAT'               U                               03270001
         DC    CL5'SPACE'               V                               03280001
         DC    CL5'ALIAS'               X                               03290001
         DC    CL5'UPGRD'               Y                               03300001
         SPACE 2                                                        03310001
         DS    0H                                                       03320001
         EJECT                                                          03330001
*        *-----------------------------------------------------------*  03340001
*        *  DATE CONVERSION ROUTINE FOR CREATION & EXPIRATION DATES  *  03350001
*        *  IN:  R1 = ADDR OF PACKED DATE (XL3'YYDDDC')              *  03360001
*        *  OUT: R1 = ADDR OF CHAR DATE (C'MM/DD/YY' OR C'YY.DDD  ') *  03370001
*        *-----------------------------------------------------------*  03380001
         SPACE                                                          03390001
CNVDATE  EQU   *                                                        03400001
         MVC   YEAR(3),0(R1)            SAVE PARM XL3'YYDDDC'           03410001
         OI    DAYS+1,X'0F'                                             03420001
         UNPK  YYDDD,WORD                                               03430001
         MVC   MMDDYY+6(2),YYDDD        SET YEAR                        03440001
         SPACE                                                          03450001
         XC    WORK,WORK                WORK = X'00000000 00000000'     03460001
         MVO   WORK+6(2),YEAR           WORK = X'00000000 00000YY0'     03470001
         OI    WORK+7,X'0F'             WORK = X'00000000 00000YYF'     03480001
         CVB   R4,WORK                                                  03490001
         STC   R4,YEAR                  YEAR IN BINARY                  03500001
         MVC   WORK+6(2),DAYS           WORK = X'00000000 0000DDDF'     03510001
         CVB   R4,WORK                                                  03520001
         LTR   R4,R4                    IS IT ZERO DAYS?                03530001
         BZ    ZERODATE                 YES - SET 00/00/YY              03540001
         STH   R4,DAYS                  DAYS IN BINARY                  03550001
         SPACE                                                          03560001
         LA    R3,2                     ASSUME LEAP YEAR                03570001
         TM    YEAR,X'03'               IS YEAR A MULTIPLE OF 4?        03580001
         BZ    LEAPYEAR                 YES - THIS IS A LEAP YEAR       03590001
         LA    R3,2(,R3)                NO - THIS IS NOT A LEAP YEAR    03600001
LEAPYEAR EQU   *                                                        03610001
         LA    R2,DTCVNTAB              ADDR OF DATE CONVERSION TABLE   03620001
         LA    R5,12                    NUMBER OF MONTHS                03630001
CMPDAYS  EQU   *                                                        03640001
         CH    R4,6(R3,R2)              NUMBER OF DAYS UP TO NEXT MONTH 03650001
         BNH   WHICHDAY                 THIS IS THE MONTH               03660001
         LA    R2,6(,R2)                BUMP TO NEXT ENTRY              03670001
         BCT   R5,CMPDAYS               NEXT MONTH, PLEASE              03680001
         EJECT                                                          03690001
*        *-----------------------*                                      03700001
*        *  SET DATE AND RETURN  *                                      03710001
*        *-----------------------*                                      03720001
         SPACE                                                          03730001
BADYYDDD EQU   *                                                        03740001
         MVC   BADDATE(2),YYDDD         BADDATE = C'YY.***  '           03750001
         MVC   BADDATE+3(3),YYDDD+2     BADDATE = C'YY.DDD  '           03760001
         LA    R1,BADDATE                                               03770001
         BR    R11                                                      03780001
         SPACE                                                          03790001
ZERODATE EQU   *                                                        03800001
         MVC   MMDDYY(5),MMDD0          MMDDYY = C'00/00/YY'            03810001
         B     DATESET                  THOUGH INPUT'S ALL ZERO         03820001
         SPACE                                                          03830001
WHICHDAY EQU   *                                                        03840001
         SH    R4,0(R3,R2)                                              03850001
         CVD   R4,WORK                                                  03860001
         OI    WORK+7,X'0F'                                             03870001
         UNPK  MMDDYY+3(2),WORK+6(2)    SET DAY                         03880001
         MVC   MMDDYY(2),0(R2)          SET MONTH                       03890001
         SPACE                                                          03900001
DATESET  EQU   *                                                        03910001
         LA    R1,MMDDYY                                                03920001
         BR    R11                      RETURN                          03930001
         SPACE 2                                                        03940001
WORK     DC    D'0'                                                     03950001
WORD     DC    F'0'                                                     03960001
         ORG   WORD+1                                                   03970001
YEAR     DS    XL1                                                      03980001
DAYS     DS    H                                                        03990001
MMDDYY   DC    C'MM/DD/YY'                                              04000001
BADDATE  DC    C'YY.DDD  '                                              04010001
YYDDD    DC    C'YYDDD'                                                 04020001
MMDD0    DC    C'00/00'                                                 04030001
         SPACE                                                          04040001
DTCVNTAB DS    0H                                                       04050001
         DC    C'01',H'0',H'0'                                          04060001
         DC    C'02',H'31',H'31'                                        04070001
         DC    C'03',H'60',H'59'                                        04080001
         DC    C'04',H'91',H'90'                                        04090001
         DC    C'05',H'121',H'120'                                      04100001
         DC    C'06',H'152',H'151'                                      04110001
         DC    C'07',H'182',H'181'                                      04120001
         DC    C'08',H'213',H'212'                                      04130001
         DC    C'09',H'244',H'243'                                      04140001
         DC    C'10',H'274',H'273'                                      04150001
         DC    C'11',H'305',H'304'                                      04160001
         DC    C'12',H'335',H'334'                                      04170001
TOTDAYS  DC    C'13',H'366',H'365'                                      04180001
         EJECT                                                          04190001
INDTAIL  DSECT                                                          04200001
INDVOLNO DS    CL6                      VOLUME SERIAL NUMBER            04210001
INDRPTNO DS    0CL1                     REPORT NO. B'0... ....' DETAILS 04220001
INDRCKEY DS    CL4                      RECORD KEY - GENERATED SORT KEY 04230001
INDRCSEQ DS    CL2                      RECORD SEQ - SEQ NO IN 'NAMEDS' 04240001
INDENTNM DS    CL44                     ENTRY NAME                      04250001
INDENTYP DS    CL1                      ENTRY TYPE                      04260001
INDCREDT DS    CL3                      CREATION DATE                   04270001
INDEXPDT DS    CL3                      EXPIRATION DATE                 04280001
INDCRAVL DS    CL6                      CRA VOLUME SERIAL NUMBER        04290001
INDSCALC DS    CL3                      SECONDARY SPACE ALLOCATION      04300001
INDSCTYP DS    CL1                      SPACE ALLOCATION TYPE           04310001
INDDSORG DS    CL1                      DATA SET ORGANIZATION (AMDATTR) 04320001
INDCISIZ DS    CL4                      CONTROL INTERVAL SIZE           04330001
INDMAXRC DS    CL4                      MAXIMUM RECORD SIZE             04340001
INDAVGRC DS    CL4                      AVERAGE RECORD SIZE             04350001
INDTRKAL DS    H                        TRK'S ALLOC'D FOR DATA SET      04360001
INDTRKUS DS    H                        TRK'S USED FOR DATA SET         04370001
INDPCTUS DS    CL1                      PERCENT OF USAGE                04380001
INDNOEXT DS    CL1                      NO. OF EXTENTS FOR DATA SET     04390001
         EJECT                                                          04400001
TXTVCTNT DSECT                                                          04410001
         DS    CL1                                                      04420001
TXTENTNM DS    CL44                                                     04430001
         DS    CL1                                                      04440001
TXTENTYP DS    CL5                                                      04450001
         DS    CL1                                                      04460001
TXTCREDT DS    CL8                                                      04470001
         DS    CL1                                                      04480001
TXTEXPDT DS    CL8                                                      04490001
         DS    CL1                                                      04500001
TXTDSORG DS    CL5                                                      04510001
TXTCISIZ DS    CL6                                                      04520001
TXTAVGRC DS    CL6                                                      04530001
TXTMAXRC DS    CL6                                                      04540001
TXTTRKAL DS    CL6                                                      04550001
TXTTRKUS DS    CL6                                                      04560001
TXTPCTUS DS    CL4                                                      04570001
TXTNOEXT DS    CL4                                                      04580001
TXTSCALC DS    CL6                                                      04590001
         DS    CL1                                                      04600001
TXTSCTYP DS    CL1                                                      04610001
         DS    CL1                                                      04620001
TXTCRAVL DS    CL6                                                      04630001
         DS    CL1                                                      04640001
TXTENTFL DS    CL1                                                      04650001
         END                                                            04660001
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD                             00210000
//LKED.SYSIN DD *                                                       00220000
  NAME VSAMLST6(R)                                                      00230000
//A        EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF'               00120000
//ASM.SYSIN DD *                                                        00130000
         PRINT OFF                                                      00010001
         MACRO                                                          00020001
&NAME    INIT &BASE=3,&REGS=Y,&PATCH=3,&RENT=N,&SAVE=Y                  00030001
         AIF   ((&BASE LT 13) AND (&BASE GT 1)).N020                    00040001
         MNOTE 12,'INVALID BASE REGISTER'                               00050001
         MEXIT                                                          00060001
.N020    ANOP                                                           00070001
         PUSH  PRINT                                                    00080001
         PRINT ON,GEN                                                   00090001
         EJECT                                                          00100001
&SYSECT  CSECT                                                          00110001
         USING *,15                                                     00120001
         B     BEGIN                                                    00130001
         DC    AL1(24)                                                  00140001
         DC    CL8'&SYSECT'                                             00150001
         DC    CL16'-&SYSDATE-&SYSTIME'                                 00160001
         AIF   ('&RENT' EQ 'Y').N004                                    00170001
MYSAVE   DC    18F'-1'                                                  00180001
.N004    ANOP                                                           00190001
         AIF   ('&PATCH' EQ '0').N005                                   00200001
PATCH    DC    &PATCH.CL8'*PATCH*'                                      00210001
.N005    ANOP                                                           00220001
         AIF   ('&REGS' EQ 'N').N030                                    00230001
         AIF   ('&REGS' EQ 'Y').N010                                    00240001
         MNOTE 4,'REGS OPERAND INVALID. Y SUBSTITUTED'                  00250001
.N010    ANOP                                                           00260001
R0       EQU   0                                                        00270001
R1       EQU   1                                                        00280001
R2       EQU   2                                                        00290001
R3       EQU   3                                                        00300001
R4       EQU   4                                                        00310001
R5       EQU   5                                                        00320001
R6       EQU   6                                                        00330001
R7       EQU   7                                                        00340001
R8       EQU   8                                                        00350001
R9       EQU   9                                                        00360001
R10      EQU   10                                                       00370001
R11      EQU   11                                                       00380001
R12      EQU   12                                                       00390001
R13      EQU   13                                                       00400001
R14      EQU   14                                                       00410001
R15      EQU   15                                                       00420001
.N030    ANOP                                                           00430001
BEGIN    DS   0H                                                        00440001
         STM   14,12,12(13)                                             00450001
         LR    &BASE,15                                                 00460001
         DROP  15                                                       00470001
         USING &SYSECT,&BASE                                            00480001
         AIF   ('&SAVE' EQ 'N').N003                                    00490001
         AIF   ('&RENT' EQ 'Y').N002                                    00500001
         AIF   ('&RENT' EQ 'N').N001                                    00510001
         MNOTE 4,'RENT OPERAND INVALID. N SUBSTITUTED'                  00520001
.N001    ANOP                                                           00530001
         ST    13,MYSAVE+4                                              00540001
         LR    15,13                                                    00550001
         LA    13,MYSAVE                                                00560001
         ST    13,8(15)                                                 00570001
         AGO   .N003                                                    00580001
.N002    ANOP                                                           00590001
         GETMAIN R,LV=72                                                00600001
         ST    13,4(1)                                                  00610001
         ST    1,8(13)                                                  00620001
         LR    13,1                                                     00630001
.N003    ANOP                                                           00640001
         POP   PRINT                                                    00650001
         EJECT                                                          00660001
         MEND                                                           00670001
         PRINT ON                                                       00680001
*  MODULE NAME:         VSAMLST7     (REL. 1.1  08/10/79)               00690001
*                                                                       00700001
*  MODULE DESCRIPTION:  VSAM CATALOG LIST UTILITY - REPORT 2 FORMATTER  00710001
*                                                                       00720001
*  RETURN LINKAGE:      RETURN (14,12),RC=(15)                          00730001
*                                                                       00740001
*  LINKAGE TABLE:       NONE - SINGLE ENTRY                             00750001
*                                                                       00760001
*  PARAMETERS:          R1 POINTS TO THE ADDRESS OF A 92-BYTES RECORD   00770001
*                       FROM SORT.                                      00780001
*                                                                       00790001
*  EXIT:                RC=00 - SUCCESSFUL                              00800001
*                                                                       00810001
*  MODULE FUNCTION:     FOR EACH RECORD FROM SORT/VSAMLST5, THIS MODULE 00820001
*                       FORMATS IT FOR THE VSAM TRACK ALLOCATION MAP    00830001
*                       REPORT.  IN ORDER TO FILL THE GAPS BETWEEN THE  00840001
*                       EXTENTS RECEIVED FROM THE CALLER, THIS MODULE   00850001
*                       MAY GENERATE TWO FORMATTED LINES TO PRINT.      00860001
*                       UPON EXIT THE GENERAL REGISTER R1 MAY CONTAINS  00870001
*                       ZERO OR THE ADDRESS OF THE SECOND TO PRINT.     00880001
*                                                                       00890001
*  CALLER:              VSAMLST5                                        00900001
*                                                                       00910001
*  CALLS:               NONE                                            00920001
*                                                                       00930001
*  SYSTEMS SERVICES:    NONE                                            00940001
*                                                                       00950001
*  MODULE ENVIRONMENT:  OS/VS1                                          00960001
         TITLE 'VSAMLST7 --- VSAM CATALOG LIST UTILITY REPORT 2 FORMATTE00970001
                (3.2) - VSAM TRACK ALLOCATION MAP'                      00980001
*        *-----------------------------------*                          00990001
*        *   R E G I S T E R   U S A G E S   *                          01000001
*        *-----------------------------------*                          01010001
*                                                                       01020001
*  R0  -  STANDARD LINKAGE                                              01030001
*  R1  -  STANDARD LINKAGE (ADDR OF PARM LIST) & TEMP WORK SPACE        01040001
*  R2  -  TEMP WORK SPACE                                               01050001
*  R3  -  TEMP WORK SPACE                                               01060001
*  R4  -  TEMP WORK SPACE                                               01070001
*  R5  -  NOT USED                                                      01080001
*  R6  -  NOT USED                                                      01090001
*  R7  -  NOT USED                                                      01100001
*  R8  -  NOT USED                                                      01110001
*  R9  -  BASE REG FOR DSECT INEXTNT, INVLCHR                           01120001
*  R10 -  BASE REG FOR DSECT TXTRKALC                                   01130001
*  R11 -  RETURN ADDR FOR SUBROUTINES                                   01140001
*  R12 -  BASE REG FOR CSECT VSAMLST7                                   01150001
*  R13 -  STANDARD LINKAGE (ADDR OF SAVE AREA)                          01160001
*  R14 -  STANDARD LINKAGE (ADDR TO RETURN)                             01170001
*  R15 -  STANDARD LINKAGE (ADDR OF ENTRY POINT & RETURN CODE)          01180001
         SPACE 3                                                        01190001
VSAMLST7 CSECT                                                          01200001
         INIT  BASE=12                                                  01210001
*********************************************************************** 01220001
*                                                                     * 01230001
*  3.1    VSAM TRACK ALLOCATION MAP REPORT FORMATTER                  * 01240001
*                                                                     * 01250001
*     FUNCTION:  FOR EACH RECORD PASSED TO THIS MODULE, IT FORMATS    * 01260001
*                IT FOR THE VSAM TRACK ALLOCATION MAP.  THIS REPORT   * 01270001
*                CONTAINS THE UPPER AND LOWER LIMITS OF THE EXTENT,   * 01280001
*                NUMBER OF TRACKS ALLOCATED AND AVAILABLE, EXTENT'S   * 01290001
*                SEQUENCE NUMBER, ENTRY NAME AND EXCEPTION IF ANY.    * 01300001
*                                                                     * 01310001
*     ERRORS:  NONE                                                   * 01320001
*                                                                     * 01330001
*********************************************************************** 01340001
         SPACE 3                                                        01350001
VCL32    EQU   *                                                        01360001
         USING INEXTNT,R9                                               01370001
         USING TXTRKALC,R10                                             01380001
         LM    R9,R10,0(R1)                                             01390001
         MVC   RTNPARM,ZERO             CLEAR RETURN PARM               01400001
         SPACE 2                                                        01410001
*        *-----------------------------------------------*              01420001
*        *  THERE ARE 3 TYPES OF RECORDS FROM VSAMLST5.  *              01430001
*        *    TYPE 0 - VOL CHAR INFO'S:  BEGINNING       *              01440001
*        *    TYPE 1 - EXTENT INFO'S                     *              01450001
*        *    TYPE 2 - DUMMY RECORD:  ENDING             *              01460001
*        *-----------------------------------------------*              01470001
         SPACE                                                          01480001
         CLC   INELOTCH,ZERO            IS THIS TYPE 0?                 01490001
         BE    TYPE0                    YES - GET NEW VOL INFO'S        01500001
         SPACE                                                          01510001
         CLC   INELOTCH,FOXES           IS THIS TYPE 2?                 01520001
         BE    TYPE2                    YES - SUMMARIZE IT              01530001
         SPACE                                                          01540001
         B     TYPE1                    ELSE IT MUST BE EXTENT RECORDS  01550001
         DROP  R9                                                       01560001
         EJECT                                                          01570001
*        *------------------------------------*                         01580001
*        *  TYPE 0 - BEGINNING OF NEW VOLUME  *                         01590001
*        *------------------------------------*                         01600001
         SPACE                                                          01610001
TYPE0    EQU   *                                                        01620001
         USING INVLCHR,R9                                               01630001
         MVC   DEVTYP,INVDVTYP          DEVICE TYPE                     01640001
         MVC   CYLVOL,INVCYLVL          CYLINDERS PER VOLUME            01650001
         MVC   TRKCYL,INVTRKCL          TRACKS PER CYLINDER             01660001
         SPACE                                                          01670001
         MVI   TXTLOTRK+5,C'0'          FORMAT VOLUME LABEL ENTRY       01680001
         MVC   TXTLOTCH,ZEROCCHH                                        01690001
         MVI   TXTHITRK+5,C'0'                                          01700001
         MVC   TXTHITCH,ZEROCCHH                                        01710001
         MVI   TXTTRKAL+5,C'1'                                          01720001
         MVC   TXTENTNM(L'VLABEL),VLABEL MOVE IN VOLUME LABEL NAME      01730001
         SPACE                                                          01740001
         LA    R1,1                                                     01750001
         STH   R1,TRKAL                 TRACKS ALLOCATED = 1            01760001
         MVC   TRKAV,ZERO               TRACKS AVAILABLE = 0            01770001
         STH   R1,LASTUPT               LAST UPPER LIMIT + 1            01780001
         SPACE                                                          01790001
         MVI   OSFLG,X'00'              RESET OS FLAG                   01800001
         CLC   INVOSEXT,FOXES           ARE THERE ANY OS SPACE INFO'S?  01810001
         BNE   GOHOME                   YES - ALL DONE FOR THIS VOLUME  01820001
         SPACE                                                          01830001
         MVI   OSFLG,X'FF'              NO - INDICATE NOT AVAILABLE     01840001
         B     GOHOME                                                   01850001
         SPACE                                                          01860001
         DROP  R9                                                       01870001
         EJECT                                                          01880001
*        *--------------------------------*                             01890001
*        *  TYPE 1 - EXTENT INFORMATIONS  *                             01900001
*        *--------------------------------*                             01910001
         SPACE                                                          01920001
TYPE1    EQU   *                                                        01930001
         USING INEXTNT,R9                                               01940001
         SPACE 2                                                        01950001
*        *-----------------------------------------------*              01960001
*        *  THERE ARE 6 TYPES OF EXTENTS.                *              01970001
*        *    TYPE 1A - VTOC                             *              01980001
*        *    TYPE 1B - OS FREE SPACE                    *              01990001
*        *    TYPE 1C - VSAM FREE SPACE                  *              02000001
*        *    TYPE 1D - DUMMY:  VSAM DATA SPACE START    *              02010001
*        *    TYPE 1E - DUMMY:  VSAM DATA SPACE END      *              02020001
*        *    TYPE 1F - VSAM ALLOCATED SPACE             *              02030001
*        *-----------------------------------------------*              02040001
         SPACE                                                          02050001
         CLI   INEXTNM,C'*'             IS THIS TYPE 1A, 1B OR 1C?      02060001
         BE    TYPE1ABC                 YES - TAKE A CLOSER LOOK        02070001
         SPACE                                                          02080001
         CLI   INEXTNM,X'00'            IS THIS TYPE 1D?                02090001
         BE    TYPE1D                   YES - GO SET VSAM FLAG          02100001
         SPACE                                                          02110001
         CLI   INEXTNM,X'FF'            IS THIS TYPE 1E?                02120001
         BE    TYPE1E                   YES - GO RESET VSAM FLAG        02130001
         SPACE                                                          02140001
         B     TYPE1F                   ELSE MUST BE TYPE 1F            02150001
         EJECT                                                          02160001
*        *-----------------------------------------*                    02170001
*        *  TYPE 1A, 1B, 1C - ALL INFO'S COMPLETE  *                    02180001
*        *-----------------------------------------*                    02190001
         SPACE                                                          02200001
TYPE1ABC EQU   *                                                        02210001
         SR    R0,R0                    NORMAL RECORD                   02220001
         LH    R1,INELOTRK              LOWER LIMIT                     02230001
         BAL   R11,CHKGAP               FILL THE GAP IF THERE IS ONE    02240001
         SPACE                                                          02250001
         MVC   LOTRK,INELOTRK           SET EXT DESCRIPTOR              02260001
         MVC   LOTCH,INELOTCH                                           02270001
         MVC   HITRK,INEHITRK                                           02280001
         MVC   HITCH,INEHITCH                                           02290001
         MVC   EXTLN,INEXTLTH                                           02300001
         MVC   EXTNO,INEXTNO                                            02310001
         MVC   TXTENTNM,INEXTNM         SET ENTRY NAME NOW              02320001
         SPACE                                                          02330001
         CLC   VTOC,INEXTNM+7           IS IT VTOC?                     02340001
         BE    TYPE1A                   YES - NOT FREE SPACE            02350001
         SPACE                                                          02360001
TYPE1BC  EQU   *                                                        02370001
         SR    R0,R0                    INDICATE FREE SPACE             02380001
         BAL   R11,FMTEXT                                               02390001
         B     GOHOME                                                   02400001
         SPACE                                                          02410001
TYPE1A   EQU   *                                                        02420001
         LA    R0,4                     INDICATE USED SPACE             02430001
         BAL   R11,FMTEXT                                               02440001
         B     GOHOME                                                   02450001
         SPACE 2                                                        02460001
*        *-------------------------------*                              02470001
*        *  TYPE 1D, 1E - DUMMY RECORDS  *                              02480001
*        *-------------------------------*                              02490001
         SPACE                                                          02500001
TYPE1D   EQU   *                                                        02510001
         LA    R0,4                     DUMMY RECORD                    02520001
         LH    R1,INELOTRK              LOWER LIMIT                     02530001
         BAL   R11,CHKGAP               FILL THE GAP IF THERE IS ONE    02540001
         MVI   VSAMFLG,X'FF'            INDICATE START OF NEW VSAM DSP  02550001
         B     GOHOME                                                   02560001
         SPACE                                                          02570001
TYPE1E   EQU   *                                                        02580001
         LA    R0,4                     DUMMY RECORD                    02590001
         LH    R1,INELOTRK              LOWER LIMIT                     02600001
         LA    R1,1(,R1)                ONE AFTER THE END OF VSAM DSP   02610001
         BAL   R11,CHKGAP               FILL THE GAP IF THERE IS ONE    02620001
         MVI   VSAMFLG,X'00'            INDICATE END OF THIS VSAM DSP   02630001
         B     GOHOME                                                   02640001
         EJECT                                                          02650001
*        *------------------------------------------------------------* 02660001
*        *  TYPE 1F - VSAM ALLOCATED SPACE W/O RELATIVE TRACK INFO'S  * 02670001
*        *------------------------------------------------------------* 02680001
         SPACE                                                          02690001
TYPE1F   EQU   *                                                        02700001
         LH    R4,INELOCC               LOWER LIMIT                     02710001
         MH    R4,TRKCYL                                                02720001
         AH    R4,INELOHH                                               02730001
         SPACE                                                          02740001
         SR    R0,R0                    NORMAL RECORD                   02750001
         LR    R1,R4                    LOWER LIMIT                     02760001
         BAL   R11,CHKGAP               FILL THE GAP IF THERE IS ONE    02770001
         SPACE                                                          02780001
         STH   R4,LOTRK                                                 02790001
         MVC   LOTCH,INELOTCH                                           02800001
         SPACE                                                          02810001
         LH    R1,INEHICC               UPPER LIMIT                     02820001
         MH    R1,TRKCYL                                                02830001
         AH    R1,INEHIHH                                               02840001
         STH   R1,HITRK                                                 02850001
         MVC   HITCH,INEHITCH                                           02860001
         SPACE                                                          02870001
         SR    R1,R4                    LENGTH AND SEQ NO.              02880001
         LA    R1,1(,R1)                                                02890001
         STH   R1,EXTLN                                                 02900001
         MVC   EXTNO,INEXTNO                                            02910001
         SPACE                                                          02920001
         LA    R0,4                     INDICATE USED SPACE             02930001
         BAL   R11,FMTEXT                                               02940001
         MVC   TXTENTNM,INEXTNM         SET ENTRY NAME NOW              02950001
         B     GOHOME                                                   02960001
         SPACE                                                          02970001
         DROP  R9                                                       02980001
         EJECT                                                          02990001
*        *----------------------------------*                           03000001
*        *  TYPE 2 - END OF CURRENT VOLUME  *                           03010001
*        *----------------------------------*                           03020001
         SPACE                                                          03030001
TYPE2    EQU   *                                                        03040001
         CLI   DEVTYP+2,X'20'           IS IT A DASD?                   03050001
         BNE   BADDVTP                  NO - OMIT CHECKING FOR GAP      03060001
         SPACE                                                          03070001
         SR    R2,R2                    CLEAR INDEX FOR DASD TABLE      03080001
         IC    R2,DEVTYP+3              GET DEVICE CODE                 03090001
         SLL   R2,2                     MULTIPLY BY 4 FOR DISPLACEMENT  03100001
         LA    R2,TRKSDASD(R2)          R2 = ADDR OF DASD TRACKS ENTRY  03110001
         LA    R1,TRKSDEND              R1 = ADDR OF END OF TABLE       03120001
         CR    R1,R2                    IS THIS A VALID ENTRY?          03130001
         BNH   BADDVTP                  NO - OMIT CHECKING FOR GAP      03140001
         SPACE                                                          03150001
         LH    R1,2(,R2)                YES - TOTAL NO. OF TRK'S ON VOL 03160001
         LTR   R1,R1                    IS THIS A VALID ENTRY?          03170001
         BZ    BADDVTP                  NO - OMIT CHECKING FOR GAP      03180001
         SR    R0,R0                    NORMAL RECORD                   03190001
         BAL   R11,CHKGAP                                               03200001
         SPACE                                                          03210001
BADDVTP  EQU   *                                                        03220001
         LH    R1,TRKAL                 TRACKS ALLOCATED                03230001
         CVD   R1,WORK                                                  03240001
         MVC   ENDTRKAL(6),PAT5                                         03250001
         ED    ENDTRKAL(6),WORK+5                                       03260001
         SPACE                                                          03270001
         LH    R1,TRKAV                 TRACKS AVAILABLE                03280001
         CVD   R1,WORK                                                  03290001
         MVC   ENDTRKAV(6),PAT5                                         03300001
         ED    ENDTRKAV(6),WORK+5                                       03310001
         SPACE                                                          03320001
         AH    R1,TRKAL                 TRACKS ACCOUNTED FOR            03330001
         CVD   R1,WORK                                                  03340001
         MVC   ENDTOTAL(6),PAT5                                         03350001
         ED    ENDTOTAL(6),WORK+5                                       03360001
         MVC   TXTTRKAL(L'ENDING1),ENDING1                              03370001
         EJECT                                                          03380001
*        *-------------*                                                03390001
*        *   E X I T   *                                                03400001
*        *-------------*                                                03410001
         SPACE                                                          03420001
GOHOME   EQU   *                                                        03430001
         L     R13,MYSAVE+4             RESTORE REGISTER 13             03440001
         L     R14,12(,R13)             RESTORE REGISTER 14             03450001
         SR    R15,R15                  CLEAR REGISTER 15               03460001
         L     R0,20(,R13)              RESTORE REGISTER 0              03470001
         L     R1,RTNPARM               SET RETURN PARM IN REGISTER 1   03480001
         LM    R2,R12,28(R13)           RESTORE REGISTERS 2 THROUGH 12  03490001
         BR    R14                      RETURN                          03500001
         SPACE 2                                                        03510001
RTNPARM  DC    F'0'                     MAYBE ADDR OF 2ND LINE          03520001
         EJECT                                                          03530001
*        *---------------------------------------------------------*    03540001
*        *  CHKGAP - SUBROUTINE TO CHECK FOR GAPS BETWEEN EXTENTS  *    03550001
*        *    IN:   R0 = 0 FOR NORMAL RECORD & 4 FOR DUMMY RECORD  *    03560001
*        *          R1 = CURRENT LOW TRACK ADDRESS                 *    03570001
*        *    OUT:  IF THERE IS A GAP, IT FORMATS A LINE FOR IT &  *    03580001
*        *          SWAP A NEW LINE BEFORE CONTINUING              *    03590001
*        *---------------------------------------------------------*    03600001
         SPACE                                                          03610001
CHKGAP   EQU   *                                                        03620001
         CH    R1,LASTUPT               IS CURRENT TRK SAME AS LAST+1?  03630001
         BER   R11                      YES - THERE IS NO GAP - RETURN  03640001
         BH    HAVEGAP                  NO - EITHER THERE IS A GAP      03650001
         SPACE                                                          03660001
OVERLAP  EQU   *                        OR THERE IS AN OVERLAP          03670001
         LTR   R0,R0                    IS IT A DUMMY RECORD?           03680001
         BNZR  R11                      YES - BYPASS OVERLAP            03690001
         MVC   TXTEXCPT(L'EXCEPT1),EXCEPT1                              03700001
         BR    R11                      RETURN                          03710001
         SPACE                                                          03720001
HAVEGAP  EQU   *                                                        03730001
         ST    R11,SAVER11              SAVE RETURN ADDR                03740001
         LH    R0,TRKCYL                SET EXT DESCRIPTOR              03750001
         LH    R3,LASTUPT                                               03760001
         STH   R3,LOTRK                 LOWER LIMIT                     03770001
         SR    R2,R2                                                    03780001
         DR    R2,R0                                                    03790001
         STH   R3,LOCC                                                  03800001
         STH   R2,LOHH                                                  03810001
         SPACE                                                          03820001
         LR    R3,R1                                                    03830001
         BCTR  R3,0                                                     03840001
         STH   R3,HITRK                 UPPER LIMIT                     03850001
         SR    R2,R2                                                    03860001
         DR    R2,R0                                                    03870001
         STH   R3,HICC                                                  03880001
         STH   R2,HIHH                                                  03890001
         SPACE                                                          03900001
         LR    R3,R1                    LENGTH AND SEQUENCE NUMBER      03910001
         SH    R3,LOTRK                                                 03920001
         STH   R3,EXTLN                                                 03930001
         MVC   EXTNO,FOXES              NO EXT SEQ NO.                  03940001
         EJECT                                                          03950001
*        *-----------------------------*                                03960001
*        *  CHKGAP SUBROUTINE - CONT.  *                                03970001
*        *-----------------------------*                                03980001
         SPACE                                                          03990001
         CLI   VSAMFLG,X'00'            IT IT WITHIN A VSAM DATA SPACE? 04000001
         BNE   MISSING                  YES - VSAM MISSING TRACKS       04010001
         SPACE                                                          04020001
         LA    R0,4                     NO - NON VSAM ALLOCATION        04030001
         BAL   R11,FMTEXT               FORMAT IT AND                   04040001
         SPACE                                                          04050001
         CLI   OSFLG,X'00'              ARE THERE ANY OS SPACE INFO'S?  04060001
         BE    GOTOSDS                  YES - USE 'OS DATA SET(S)'      04070001
         MVC   TXTENTNM(L'NONVSAM),NONVSAM SET ENTRY NAME AND           04080001
         B     SWAPLINE                 SWAP                            04090001
         SPACE                                                          04100001
GOTOSDS  EQU   *                                                        04110001
         MVC   TXTENTNM(L'OSDSET),OSDSET SET ENTRY NAME AND             04120001
         B     SWAPLINE                 SWAP                            04130001
         SPACE                                                          04140001
MISSING  EQU   *                                                        04150001
         LA    R0,8                     DO NOT ACCUMULATE THESE TRACKS  04160001
         BAL   R11,FMTEXT               JUST FORMAT IT AND              04170001
         MVC   TXTEXCPT(L'EXCEPT2),EXCEPT2 INDICATE MISSING TRK & SWAP  04180001
         SPACE 2                                                        04190001
SWAPLINE EQU   *                                                        04200001
         MVI   LINE,X'40'               CLEAR NEW LINE                  04210001
         MVC   LINE+1(L'LINE-1),LINE                                    04220001
         LA    R10,LINE                 RESET BASE REG OF TXTRKALC      04230001
         ST    R10,RTNPARM              SET RETURN PARM                 04240001
         SPACE                                                          04250001
         L     R11,SAVER11              RESTORE THE RETURN ADDR         04260001
         BR    R11                      RETURN                          04270001
         SPACE 2                                                        04280001
SAVER11  DC    F'0'                                                     04290001
         EJECT                                                          04300001
*        *-------------------------------------------------------*      04310001
*        *  FMTEXT - SUBROUTINE TO FORMAT AN EXTENT              *      04320001
*        *    IN:   EXTDSCP (LTH=16) HAS THE EXTENT DESCRIPTOR   *      04330001
*        *          R0=0 - FREE EXTENT                           *      04340001
*        *          R0=4 - USED EXTENT                           *      04350001
*        *          R0=8 - MISSING EXTENT                        *      04360001
*        *    OUT:  TXTRKALC IS FILLED EXCEPT ITS LAST 2 FIELDS  *      04370001
*        *          COUNTERS FOR TRACKS ALLOCATED & AVAILABLE    *      04380001
*        *          ARE UPDATED ACCORDINGLY                      *      04390001
*        *-------------------------------------------------------*      04400001
         SPACE                                                          04410001
FMTEXT   EQU   *                                                        04420001
         LH    R1,LOTRK                 LOWER LIMIT                     04430001
         CVD   R1,WORK                                                  04440001
         MVC   TXTLOTRK,PAT5                                            04450001
         ED    TXTLOTRK,WORK+5                                          04460001
         UNPK  WORK(9),LOTCH(5)                                         04470001
         TR    WORK,HEXTBL                                              04480001
         MVC   TXTLOTCC,WORK                                            04490001
         MVI   TXTLODOT,C'.'                                            04500001
         MVC   TXTLOTHH,WORK+4                                          04510001
         SPACE                                                          04520001
         LH    R1,HITRK                 UPPER LIMIT                     04530001
         CVD   R1,WORK                                                  04540001
         MVC   TXTHITRK,PAT5                                            04550001
         ED    TXTHITRK,WORK+5                                          04560001
         UNPK  WORK(9),HITCH(5)                                         04570001
         TR    WORK,HEXTBL                                              04580001
         MVC   TXTHITCC,WORK                                            04590001
         MVI   TXTHIDOT,C'.'                                            04600001
         MVC   TXTHITHH,WORK+4                                          04610001
         SPACE                                                          04620001
         LA    R1,1(,R1)                UPPER LIMIT + 1                 04630001
         STH   R1,LASTUPT                                               04640001
         SPACE                                                          04650001
         LH    R1,EXTNO                 EXTENT SEQUENCE NUMBER          04660001
         LTR   R1,R1                                                    04670001
         BM    CHKEXT                   SKIP IT IF ALL X'FF'            04680001
         SPACE                                                          04690001
         CVD   R1,WORK                                                  04700001
         MVC   TXTEXTNO,PAT3                                            04710001
         ED    TXTEXTNO,WORK+6                                          04720001
         EJECT                                                          04730001
*        *-----------------------------*                                04740001
*        *  FMTEXT SUBROUTINE - CONT.  *                                04750001
*        *-----------------------------*                                04760001
         SPACE                                                          04770001
CHKEXT   EQU   *                                                        04780001
         LR    R1,R0                    GET EXTENT TYPE                 04790001
         B     HERE(R1)                                                 04800001
HERE     B     FREEEXT                  FREE EXT                        04810001
         B     USEDEXT                  USED EXT                        04820001
         BR    R11                      MISSING EXT                     04830001
         SPACE                                                          04840001
FREEEXT  EQU   *                                                        04850001
         LH    R1,EXTLN                 EXTENT LENGTH                   04860001
         CVD   R1,WORK                                                  04870001
         MVC   TXTTRKAV,PAT5            FOR TRACKS AVAILABLE            04880001
         ED    TXTTRKAV,WORK+5                                          04890001
         AH    R1,TRKAV                                                 04900001
         STH   R1,TRKAV                                                 04910001
         BR    R11                      ALL DONE                        04920001
         SPACE                                                          04930001
USEDEXT  EQU   *                                                        04940001
         LH    R1,EXTLN                 EXTENT LENGTH                   04950001
         CVD   R1,WORK                                                  04960001
         MVC   TXTTRKAL,PAT5            FOR TRACKS ALLOCATED            04970001
         ED    TXTTRKAL,WORK+5                                          04980001
         AH    R1,TRKAL                                                 04990001
         STH   R1,TRKAL                                                 05000001
         BR    R11                      ALL DONE                        05010001
         EJECT                                                          05020001
*        *-----------------------*                                      05030001
*        *   C O N S T A N T S   *                                      05040001
*        *-----------------------*                                      05050001
         SPACE                                                          05060001
ZERO     DC    F'0'                                                     05070001
FOXES    DC    F'-1'                                                    05080001
PAT3     DC    XL4'40202120'                                            05090001
PAT5     DC    XL6'402020202120'                                        05100001
         SPACE                                                          05110001
ZEROCCHH DC    CL9'0000.0000'                                           05120001
VLABEL   DC    C'* * *  VOL LABEL  * * *'                               05130001
OSDSET   DC    C'* * *  OS DATA SET(S)  * * *'                          05140001
NONVSAM  DC    C'* * *  NON VSAM  * * *'                                05150001
EXCEPT1  DC    C'***  OVERLAP  ***'                                     05160001
EXCEPT2  DC    C'***  MISSING  ***'                                     05170001
VTOC     DC    C'VTOC'                                                  05180001
         SPACE 2                                                        05190001
*        *------------------------------------------------------------* 05200001
*        *  THIS TABLE IS INDEXED BY DASD CLASS OF UCBTYP FIELD OF    * 05210001
*        *  UCB AS DESCRIBED IN OS/VS1 SYSTEM DATA AREAS (SY28-0605). * 05220001
*        *  EACH ENTRY IS 4 BYTES LONG WITH THE FOLLOWING,            * 05230001
*        *    BYTE 0   :  DASD CLASS IN HEXADECIMAL FORMAT            * 05240001
*        *    BYTE 1   :  NOT USED - X'00'                            * 05250001
*        *    BYTE 2-3 :  TOTAL NUMBER OF TRACKS ON THIS DASD VOLUME  * 05260001
*        *------------------------------------------------------------* 05270001
         SPACE                                                          05280001
TRKSDASD DS    0F                                                       05290001
         DC    X'00',X'00',H'0'         * * *   NO SUCH DEVICE   * * *  05300001
         DC    X'01',X'00',H'0'         2311 DISK STORAGE DRIVE         05310001
         DC    X'02',X'00',H'0'         2301 PARALLEL DRUM              05320001
         DC    X'03',X'00',H'0'         2303 SERIAL DRUM                05330001
         DC    X'04',X'00',H'0'         2302 DISK STORAGE               05340001
         DC    X'05',X'00',H'0'         2321 DATA CELL DRIVE            05350001
         DC    X'06',X'00',H'0'         2305 FIXED HEAD STORAGE MODEL 1 05360001
         DC    X'07',X'00',H'0'         2305 FIXED HEAD STORAGE MODEL 2 05370001
         DC    X'08',X'00',H'0'         2314/2319 DIRECT ACCESS STORAGE 05380001
         DC    X'09',X'00',H'0'         3330/3333 MODEL 1 DISK STORAGE  05390001
         DC    X'0A',X'00',H'0'         3340 DISK STORAGE               05400001
         DC    X'0B',X'00',H'16650'     3350 DIRECT ACCESS STORAGE      05410001
         DC    X'0C',X'00',H'0'         * * *   NO SUCH DEVICE   * * *  05420001
         DC    X'0D',X'00',H'15352'     3330/3333 MODEL 11 DISK STORAGE 05430001
TRKSDEND EQU   *                                                        05440001
         SPACE 2                                                        05450001
HEXNUM   DC    CL16'0123456789ABCDEF'                                   05460001
HEXTBL   EQU   HEXNUM-240                                               05470001
         SPACE                                                          05480001
ENDING1  DC    C' TRKAL+ TRKAV = TOTAL TRACKS ACCOUNTED FOR'            05490001
ENDTRKAL EQU   ENDING1                                                  05500001
ENDTRKAV EQU   ENDING1+7                                                05510001
ENDTOTAL EQU   ENDING1+15                                               05520001
         EJECT                                                          05530001
*        *-----------------------*                                      05540001
*        *   V A R I A B L E S   *                                      05550001
*        *-----------------------*                                      05560001
         SPACE                                                          05570001
VSAMFLG  DC    X'00'                    X'FF' IF WITHIN A VSAM DATA SP  05580001
OSFLG    DC    X'00'                    X'FF' IF NO OS SPACE INFO'S     05590001
TRKAL    DC    H'0'                                                     05600001
TRKAV    DC    H'0'                                                     05610001
LASTUPT  DC    H'0'                                                     05620001
         SPACE                                                          05630001
EXTDSCP  DS    0CL16                                                    05640001
LOTRK    DC    H'0'                                                     05650001
LOTCH    DS    0CL4                                                     05660001
LOCC     DC    H'0'                                                     05670001
LOHH     DC    H'0'                                                     05680001
HITRK    DC    H'0'                                                     05690001
HITCH    DS    0CL4                                                     05700001
HICC     DC    H'0'                                                     05710001
HIHH     DC    H'0'                                                     05720001
EXTLN    DC    H'0'                                                     05730001
EXTNO    DC    H'0'                                                     05740001
         SPACE                                                          05750001
DEVTYP   DC    XL4'00'                                                  05760001
CYLVOL   DC    H'0'                                                     05770001
TRKCYL   DC    H'0'                                                     05780001
         SPACE 2                                                        05790001
WORK     DC    D'0'                                                     05800001
WORD     DC    F'0'                                                     05810001
LINE     DC    CL132' '                                                 05820001
         EJECT                                                          05830001
INVLCHR  DSECT                                                          05840001
INVVOLNO DS    CL6                      VOLUME SERIAL NUMBER            05850001
INVRPTNO DS    0CL1                     REPORT NO. B'X... ....'  X=0,1  05860001
INVRCKEY DS    CL4                      RECORD KEY - ZERO FOR THIS REC  05870001
INVOLUSE DS    CL2                      VOLUME USE (UCBSTAB)            05880001
INVDVTYP DS    CL4                      DEVICE TYPE                     05890001
INVCYLVL DS    H                        NO. OF CYLINDERS PER VOLUME     05900001
INVTRKCL DS    H                        NO. OF TRACKS PER CYLINDER      05910001
INVBYTTK DS    H                        NO. OF BYTES PER TRACK          05920001
INVNODST DS    H                        NUMBER OF DATA SETS ON VOLUME   05930001
INVNODSP DS    H                        NUMBER OF DATA SPACE ON VOLUME  05940001
INVSYSXT DS    H                        SYSTEM ALLOWED EXTENTS          05950001
INVCATNM DS    CL44                     VSAM CATALOG NAME               05960001
INVVSEXT DS    H                        NUMBER OF VSAM FREE EXTENTS     05970001
INVVSTRK DS    H                        NUMBER OF VSAM FREE TRACKS      05980001
INVVSLGE DS    H                        TRACKS OF VSAM LARGEST FREE EXT 05990001
INVOSEXT DS    H                        NUMBER OF OS FREE EXTENTS       06000001
INVOSTRK DS    H                        NUMBER OF OS FREE TRACKS        06010001
INVOSCYL DS    H                        NUMBER OF OS FREE CYLINDERS     06020001
INVOSLGE DS    H                        TRACKS OF OS LARGEST FREE EXT   06030001
         SPACE 2                                                        06040001
INEXTNT  DSECT                                                          06050001
INEVOLNO DS    CL6                      VOLUME SERIAL NUMBER            06060001
INERPTNO DS    0CL1                     REPORT NO. B'1... ....' EXTENTS 06070001
INELOTCH DS    0CL4                     LOW CCHH OF EXTENT (SORT KEY)   06080001
INELOCC  DS    H                                                        06090001
INELOHH  DS    H                                                        06100001
INERCKEY DS    H                        H'0' FOR DSP EXT, H'1' OTHERS   06110001
INEXTNM  DS    CL44                     DATA SET NAME (ENTRY NAME)      06120001
INEHITCH DS    0CL4                     HIGH CCHH OF EXTENT             06130001
INEHICC  DS    H                                                        06140001
INEHIHH  DS    H                                                        06150001
INEXTLTH DS    H                        LENGTH OF EXTENT IN TRACKS      06160001
INELOTRK DS    H                        LOW TRACK NUMBER                06170001
INEHITRK DS    H                        HIGH TRACK NUMBER               06180001
INEXTNO  DS    H                        EXTENT NUMBER                   06190001
         EJECT                                                          06200001
TXTRKALC DSECT                                                          06210001
         DS    CL9                                                      06220001
TXTLOTRK DS    CL6                                                      06230001
         DS    CL1                                                      06240001
TXTLOTCH DS    0CL9                                                     06250001
TXTLOTCC DS    CL4                                                      06260001
TXTLODOT DS    CL1                                                      06270001
TXTLOTHH DS    CL4                                                      06280001
         DS    CL1                                                      06290001
TXTHITRK DS    CL6                                                      06300001
         DS    CL1                                                      06310001
TXTHITCH DS    0CL9                                                     06320001
TXTHITCC DS    CL4                                                      06330001
TXTHIDOT DS    CL1                                                      06340001
TXTHITHH DS    CL4                                                      06350001
         DS    CL1                                                      06360001
TXTTRKAL DS    CL6                                                      06370001
         DS    CL1                                                      06380001
TXTTRKAV DS    CL6                                                      06390001
         DS    CL1                                                      06400001
TXTEXTNO DS    CL4                                                      06410001
         DS    CL2                                                      06420001
TXTENTNM DS    CL44                                                     06430001
         DS    CL2                                                      06440001
TXTEXCPT DS    CL17                                                     06450001
         END                                                            06460001
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD                             00210000
//LKED.SYSIN DD *                                                       00220000
  NAME VSAMLST7(R)                                                      00230000
//A        EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF'               00120000
//ASM.SYSIN DD *                                                        00130000
         TITLE '*----VSAMLST8----* VSAM CATALOG LIST UTILITY --  HISTOR*00010000
               Y FILE WRITER'                                           00020000
         PRINT OFF                                                      00030000
         MACRO                                                          00040000
&NAME    INIT &BASE=3,&REGS=Y,&PATCH=3,&RENT=N,&SAVE=Y                  00050000
         AIF   ((&BASE LT 13) AND (&BASE GT 1)).N020                    00060000
         MNOTE 12,'INVALID BASE REGISTER'                               00070000
         MEXIT                                                          00080000
.N020    ANOP                                                           00090000
         PUSH  PRINT                                                    00100000
         PRINT ON,GEN                                                   00110000
         EJECT                                                          00120000
&SYSECT  CSECT                                                          00130000
         USING *,15                                                     00140000
         B     BEGIN                                                    00150000
         DC    AL1(24)                                                  00160000
         DC    CL8'&SYSECT'                                             00170000
         DC    CL16'-&SYSDATE-&SYSTIME'                                 00180000
         AIF   ('&RENT' EQ 'Y').N004                                    00190000
MYSAVE   DC    18F'-1'                                                  00200000
.N004    ANOP                                                           00210000
         AIF   ('&PATCH' EQ '0').N005                                   00220000
PATCH    DC    &PATCH.CL8'*PATCH*'                                      00230000
.N005    ANOP                                                           00240000
         AIF   ('&REGS' EQ 'N').N030                                    00250000
         AIF   ('&REGS' EQ 'Y').N010                                    00260000
         MNOTE 4,'REGS OPERAND INVALID. Y SUBSTITUTED'                  00270000
.N010    ANOP                                                           00280000
R0       EQU   0                                                        00290000
R1       EQU   1                                                        00300000
R2       EQU   2                                                        00310000
R3       EQU   3                                                        00320000
R4       EQU   4                                                        00330000
R5       EQU   5                                                        00340000
R6       EQU   6                                                        00350000
R7       EQU   7                                                        00360000
R8       EQU   8                                                        00370000
R9       EQU   9                                                        00380000
R10      EQU   10                                                       00390000
R11      EQU   11                                                       00400000
R12      EQU   12                                                       00410000
R13      EQU   13                                                       00420000
R14      EQU   14                                                       00430000
R15      EQU   15                                                       00440000
.N030    ANOP                                                           00450000
BEGIN    DS   0H                                                        00460000
         STM   14,12,12(13)                                             00470000
         LR    &BASE,15                                                 00480000
         DROP  15                                                       00490000
         USING &SYSECT,&BASE                                            00500000
         AIF   ('&SAVE' EQ 'N').N003                                    00510000
         AIF   ('&RENT' EQ 'Y').N002                                    00520000
         AIF   ('&RENT' EQ 'N').N001                                    00530000
         MNOTE 4,'RENT OPERAND INVALID. N SUBSTITUTED'                  00540000
.N001    ANOP                                                           00550000
         ST    13,MYSAVE+4                                              00560000
         LR    15,13                                                    00570000
         LA    13,MYSAVE                                                00580000
         ST    13,8(15)                                                 00590000
         AGO   .N003                                                    00600000
.N002    ANOP                                                           00610000
         GETMAIN R,LV=72                                                00620000
         ST    13,4(1)                                                  00630000
         ST    1,8(13)                                                  00640000
         LR    13,1                                                     00650000
.N003    ANOP                                                           00660000
         POP   PRINT                                                    00670000
         EJECT                                                          00680000
         MEND                                                           00690000
         PRINT ON                                                       00700000
*  MODULE NAME:         VSAMLST8     (REL. 1.1  08/10/79)               00710000
*                                                                       00720000
*  MODULE DESCRIPTION:  VSAM CATALOG LIST UTILITY - HISTORY FILE WRITER 00730000
*                                                                       00740000
*  RETURN LINKAGE:      RETURN (14,12),RC=(15)                          00750000
*                                                                       00760000
*  LINKAGE TABLE:       NONE - SINGLE ENTRY                             00770000
*                                                                       00780000
*  PARAMETERS:          R1 POINTS TO THE ADDRESS OF A 92-BYTES RECORD   00790000
*                       FROM SORT/ESPXXX                                00800000
*                                                                       00810000
*  EXIT:                RC=00 - SUCCESSFUL                              00820000
*                       RC=04 - UNABLE TO OPEN DCB FOR HISTORY FILE     00830000
*                                                                       00840000
*  MODULE FUNCTION:     FOR EACH RECORD FROM SORT/ESPXXX,   THIS MODULE 00850000
*                       FORMATS AND WRITES IT OUT TO THE VSAM HISTORY   00860000
*                       FILE.                                           00870000
*                                                                       00880000
*  CALLER:              ESPXXX                                          00890000
*                                                                       00900000
*  CALLS:               NONE                                            00910000
*                                                                       00920000
*  SYSTEMS SERVICES:    CLOSE, OPEN, PUT (QSAM)                         00930000
*                                                                       00940000
*  MODULE ENVIRONMENT:  OS/VS1                                          00950000
         TITLE 'VSAMLST8 --- VSAM CATALOG LIST UTILITY HISTORY FILE WRIX00960000
               TER (3.3) - VSAM VOLUME CONTENTS'                        00970000
*        *-----------------------------------*                          00980000
*        *   R E G I S T E R   U S A G E S   *                          00990000
*        *-----------------------------------*                          01000000
*                                                                       01010000
*  R0  -  STANDARD LINKAGE                                              01020000
*  R1  -  STANDARD LINKAGE (ADDR OF PARM LIST) & TEMP WORK SPACE        01030000
*  R2  -  TEMP WORK SPACE                                               01040000
*  R3  -  BASE REG FOR DSECT INREC                                      01050000
*  R4  -  NOT USED                                                      01060000
*  R5  -  NOT USED                                                      01070000
*  R6  -  NOT USED                                                      01080000
*  R7  -  NOT USED                                                      01090000
*  R8  -  NOT USED                                                      01100000
*  R9  -  NOT USED                                                      01110000
*  R10 -  NOT USED                                                      01120000
*  R11 -  RETURN ADDR FROM SUBROUTINE                                   01130000
*  R12 -  BASE REG FOR CSECT VSAMLST8                                   01140000
*  R13 -  STANDARD LINKAGE (ADDR OF SAVE AREA)                          01150000
*  R14 -  STANDARD LINKAGE (ADDR TO RETURN)                             01160000
*  R15 -  STANDARD LINKAGE (ADDR OF ENTRY POINT & RETURN CODE)          01170000
         SPACE 3                                                        01180000
VSAMLST8 CSECT                                                          01190000
         INIT  BASE=12                                                  01200000
*********************************************************************** 01210000
*                                                                     * 01220000
*  3.3    VSAM HISTORY FILE WRITER                                    * 01230000
*                                                                     * 01240000
*     FUNCTION:  FOR EACH RECORD PASSED TO THIS MODULE, IT FORMATS    * 01250000
*                AND WRITES IT FOR THE VSAM HISTORY FILE.             * 01260000
*                                                                     * 01270000
*     ERRORS:  NONE                                                   * 01280000
*                                                                     * 01290000
*********************************************************************** 01300000
         SPACE 3                                                        01310000
VCL33    EQU   *                                                        01320000
         USING INREC,R3                                                 01330000
         LM    R2,R3,0(R1)                                              01340000
         B     HERE(R2)                                                 01350000
HERE     EQU   *                                                        01360000
         B     INIT                     FUNC CODE 0                     01370000
         B     PROCESS                  FUNC CODE 4                     01380000
         B     FINAL                    FUNC CODE 8                     01390000
         EJECT                                                          01400000
*        *-------------------*                                          01410000
*        *   P R O C E S S   *                                          01420000
*        *-------------------*                                          01430000
         SPACE                                                          01440000
PROCESS  EQU   *                                                        01450000
         MVC   DSCB7VOL,INVOLNO                                         01460000
         MVC   DS7DSNAM,INENTNM                                         01470000
         MVC   DS7DSSN,INVOLNO                                          01480000
         LA    R1,INCREDT                                               01490000
         LA    R2,DS7CREDT                                              01500000
         BAL   R11,BINDATE              XL3'YYDDDF' TO XL3'YYDDDD'      01510000
         LA    R1,INEXPDT                                               01520000
         LA    R2,DS7EXPDT                                              01530000
         BAL   R11,BINDATE              XL3'YYDDDF' TO XL3'YYDDDD'      01540000
         SPACE                                                          01550000
         MVC   DS7SCTYP,INSCTYP                                         01560000
         MVC   DS7SCQTY,INSCALC                                         01570000
         MVC   DS7NOEXT,INNOEXT                                         01580000
         MVC   DS7TRKAL(L'INTRKAL+L'INTRKUS),INTRKAL                    01590000
         MVC   DS7ENTYP,INENTYP                                         01600000
         MVC   DS7DATTR,INDSORG                                         01610000
         MVC   DS7CISIZ(L'INCISIZ+L'INMAXRC+L'INAVGRC),INCISIZ          01620000
         SPACE                                                          01630000
         PUT   VSAMHIST,DSCB7DTE                                        01640000
         SR    R15,R15                  CLEAR RETURN CODE               01650000
         SPACE 2                                                        01660000
*        *-------------*                                                01670000
*        *   E X I T   *                                                01680000
*        *-------------*                                                01690000
         SPACE                                                          01700000
GOHOME   EQU   *                                                        01710000
         L     R13,MYSAVE+4                                             01720000
         RETURN (14,12),RC=(15)                                         01730000
         EJECT                                                          01740000
*        *---------------------*                                        01750000
*        *   B I N   D A T E   *                                        01760000
*        *---------------------*                                        01770000
         SPACE                                                          01780000
BINDATE  EQU   *                                                        01790000
         MVC   0(3,R2),ZERO             CLEAR TARGET AREA               01800000
         CLC   ZERO(3),0(R1)            IS IT A GOOD DATE?              01810000
         BER   R11                      NO - RETURN NOW                 01820000
         CLC   FOXES(3),0(R1)           IS IT A GOOD DATE?              01830000
         BER   R11                      NO - RETURN NOW                 01840000
         SPACE                                                          01850000
         MVO   YEAR+6(2),0(1,R1)        'YY' OF X'YYDDDF'               01860000
         OI    YEAR+7,X'0F'                                             01870000
         CVB   R0,YEAR                                                  01880000
         STC   R0,0(,R2)                                                01890000
         SPACE                                                          01900000
         MVC   DAYS+6(2),1(R1)          'DDDF' OF X'YYDDDF'             01910000
         CVB   R0,DAYS                                                  01920000
         STCM  R0,3,1(R2)                                               01930000
         SPACE                                                          01940000
         BR    R11                       SPACE                          01950000
         SPACE 2                                                        01960000
ZERO     DC    F'0'                                                     01970000
FOXES    DC    F'-1'                                                    01980000
YEAR     DC    D'0'                                                     01990000
DAYS     DC    D'0'                                                     02000000
         EJECT                                                          02010000
*        *-------------*                                                02020000
*        *   I N I T   *                                                02030000
*        *-------------*                                                02040000
         SPACE                                                          02050000
INIT     EQU   *                                                        02060000
         OPEN  (VSAMHIST,(OUTPUT))                                      02070000
         SPACE                                                          02080000
         TM    VSAMHIST+48,X'10'        IS OPEN SUCCESSFUL?             02090000
         BO    DCBOK                    YES - INIT'S DONE               02100000
         LA    R15,4                    NO - SET RETURN CODE            02110000
         B     GOHOME                   AND QUIT                        02120000
         SPACE                                                          02130000
DCBOK    EQU   *                                                        02140000
         TIME  DEC                                                      02150000
         SPACE                                                          02160000
         STCM  R1,7,DSCB7DTE                                            02170000
         STCM  R0,15,DSCB7TME                                           02180000
         OI    DSCB7TME+3,X'0F'                                         02190000
         SR    R15,R15                  CLEAR RETURN CODE               02200000
         B     GOHOME                                                   02210000
         SPACE 2                                                        02220000
*        *---------------*                                              02230000
*        *   F I N A L   *                                              02240000
*        *---------------*                                              02250000
         SPACE                                                          02260000
FINAL    EQU   *                                                        02270000
         CLOSE VSAMHIST                                                 02280000
         SPACE                                                          02290000
         SR    R15,R15                  CLEAR RETURN CODE               02300000
         B     GOHOME                   AND QUIT                        02310000
         EJECT                                                          02320000
VSAMHIST DCB   DDNAME=VSAMHIST,DSORG=PS,MACRF=(PM)                      02330000
         EJECT                                                          02340000
*        *-------------------------*                                    02350000
*        *   F M T   7   D S C B   *                                    02360000
*        *-------------------------*                                    02370000
         SPACE                                                          02380000
         DS    0D                                                       02390000
DSCB7DTE DS    PL3                                                      02400000
DSCB7TME DS    PL4                                                      02410000
DSCB7VOL DS    CL6                                                      02420000
DSCB7CHR DC    XL5'00'                                                  02430000
DSCB7    DS    140X'00'                 ALIGNED AS 0D                   02440000
         ORG   DSCB7                                                    02450000
DS7DSNAM DS    CL44                     ENTRY NAME                      02460000
DS7FMTID DC    CL1'7'                                                   02470000
DS7DSSN  DS    CL6                      VOLUME SERIAL NUMBER            02480000
DS7VOLSQ DC    XL2'0001'                                                02490000
DS7CREDT DS    XL3                      CREATION DATE                   02500000
DS7EXPDT DS    XL3                      EXPIRATION DATE                 02510000
DS7NOEPV DS    XL1                                                      02520000
DS7NOBDB DS    XL1                                                      02530000
         DS    CL1                                                      02540000
DS7SYSCD DC    CL13'IBMOS/360'                                          02550000
         DS    CL7                                                      02560000
DS7DSORG DC    XL2'0008'                                                02570000
DS7RECFM DC    XL1'C0'                                                  02580000
DS7OPTCD DS    XL1                                                      02590000
DS7BLKL  DS    H                                                        02600000
DS7LRECL DS    H                                                        02610000
DS7KEYL  DS    XL1                                                      02620000
DS7RKP   DS    XL2                                                      02630000
DS7DSIND DS    XL1                                                      02640000
DS7SCALO DS    0XL4                                                     02650000
DS7SCTYP DS    XL1                      SECONDARY ALLOCATION TYPE       02660000
DS7SCQTY DS    XL3                      SECONDARY ALLOCATION QUANTITY   02670000
DS7LSTAR DS    XL3                                                      02680000
DS7TRBAL DS    XL2                                                      02690000
         DS    CL2                                                      02700000
DS7EXT1  DS    XL10                                                     02710000
DS7EXT2  DS    XL10                                                     02720000
DS7EXT3  DS    XL10                                                     02730000
DS7PTRDS DS    XL5                                                      02740000
         ORG   DS7EXT1                  VSAM INFO'S STARTS HERE         02750000
DS7NOEXT DS    XL1                      NO. OF EXTENTS FOR DATA SET     02760000
DS7TRKAL DS    H                        TRK'S ALLOC'D FOR DATA SET      02770000
DS7TRKUS DS    H                        TRK'S USED FOR DATA SET         02780000
DS7ENTYP DS    CL1                      ENTRY TYPE                      02790000
DS7DATTR DS    XL1                      DATA SET ORGANIZATION (AMDATTR) 02800000
DS7CISIZ DS    XL4                      CONTROL INTERVAL SIZE           02810000
DS7MAXRC DS    XL4                      MAXIMUM RECORD SIZE             02820000
DS7AVGRC DS    XL4                      AVERAGE RECORD SIZE             02830000
         ORG                                                            02840000
         EJECT                                                          02850000
INREC    DSECT                                                          02860000
INVOLNO  DS    CL6                      VOLUME SERIAL NUMBER            02870000
INRPTNO  DS    0CL1                     REPORT NO. B'0... ....' DETAILS 02880000
INRCKEY  DS    CL4                      RECORD KEY - GENERATED SORT KEY 02890000
INRCSEQ  DS    CL2                      RECORD SEQ - SEQ NO IN 'NAMEDS' 02900000
INENTNM  DS    CL44                     ENTRY NAME                      02910000
INENTYP  DS    CL1                      ENTRY TYPE                      02920000
INCREDT  DS    CL3                      CREATION DATE                   02930000
INEXPDT  DS    CL3                      EXPIRATION DATE                 02940000
INCRAVL  DS    CL6                      CRA VOLUME SERIAL NUMBER        02950000
INSCALC  DS    CL3                      SECONDARY SPACE ALLOCATION      02960000
INSCTYP  DS    CL1                      SPACE ALLOCATION TYPE           02970000
INDSORG  DS    CL1                      DATA SET ORGANIZATION (AMDATTR) 02980000
INCISIZ  DS    CL4                      CONTROL INTERVAL SIZE           02990000
INMAXRC  DS    CL4                      MAXIMUM RECORD SIZE             03000000
INAVGRC  DS    CL4                      AVERAGE RECORD SIZE             03010000
INTRKAL  DS    H                        TRK'S ALLOC'D FOR DATA SET      03020000
INTRKUS  DS    H                        TRK'S USED FOR DATA SET         03030000
INPCTUS  DS    CL1                      PERCENT OF USAGE                03040000
INNOEXT  DS    CL1                      NO. OF EXTENTS FOR DATA SET     03050000
         END                                                            03060000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=MOD                             00210000
//LKED.SYSIN DD *                                                       00220000
  NAME VSAMLST8(R)                                                      00230000
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=VSAMLIST
//VSAMLIST JOB (SYS),'VSAMLIST',CLASS=A,MSGCLASS=A
//STEP1  EXEC  PGM=VSAMLIST,PARM=ALL
//STEPLIB  DD  DSN=SYS2.LINKLIB,DISP=SHR   PROGRAM LOAD MODULES
//*                                      & MVT SORT/MERGE
//*STEPCAT  DD  DSN=VSAMCAT1,DISP=SHR      OPTIONAL STEP CATALOG
//*         .                              OPTIONAL STEP CATALOG
//*         .                              OPTIONAL STEP CATALOG
//*         .                              OPTIONAL STEP CATALOG
//*        DD  DSN=VSAMCATN,DISP=SHR       OPTIONAL STEP CATALOG
//SYSABEND DD  SYSOUT=*
//MSGFILE  DD  SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)
//RPTFIL1  DD  SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)
//RPTFIL2  DD  SYSOUT=*,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1330)
//SYSOUT   DD  SYSOUT=*
//SORTIN   DD  DSN=&&DETAIL,DISP=(NEW,DELETE),UNIT=2314,
//             SPACE=(CYL,(5,5))
//SORTLIB  DD  DSN=SYS1.SORTLIB,DISP=SHR   MVT SORT/MERGE LIBRARY
//SORTWK01 DD  UNIT=2314,SPACE=(CYL,(3))
//SORTWK02 DD  UNIT=2314,SPACE=(CYL,(3))
//SORTWK03 DD  UNIT=2314,SPACE=(CYL,(3))
//*                                                                     
//* FOLLOWING OUTPUT FILE IS OPTIONAL.  IF IT POINTS TO A DASD DATA     
//* SET AND 'PARM=ALL' IS SPECIFIED ON THE 'EXEC' CARD, A HISTORY       
//* FILE FOR VSAM ENTRIES WILL BE CREATED.  PLEASE NOTE THE SPEC'S      
//* IN THE DCB.  RECORDS ARE 158 BYTES LONG.                            
//*VSAMHIST DD  DSN=VSAM.HIST.FILE,DISP=(,PASS),                        
//*             UNIT=SYSDA,SPACE=(CYL,(2,1)),                           
//*             DCB=(RECFM=FB,LRECL=158,BLKSIZE=6162)                   
//*                                                                     
//*  FOLLOWING INPUT FILE IS FOR VSAM CATLG LIST UTIL CONTROL STMTS     
//*  '/' INDICATE AN OPTIONAL MASTER PASSWORD FOLLOWS                   
//*  ALL KEYWORDS SHOULD BE ON COL. 1 THRU 71 OF THE SAME CARD FOR      
//*  EACH VSAM CATALOG TO BE LISTED                                     
//VCLCNTL  DD  *                                                        
  LISTCAT  UCMVS801                                                     
//       
