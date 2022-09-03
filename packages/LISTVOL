//LISTVOL  JOB (TSO),
//             'Install LISTVOL',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
//*------------------------------------------------------------------*  00030000
//*  INSTALL THE LISTVOL TSO COMMAND FROM CBT129 -                   *  00040000
//*    ASSEMBLE/LINK LOAD MODULE INTO SYS2.CMDLIB                    *  00050000
//*    CREATE HELP TEXT IN SYS2.HELP                                 *  00060000
//*------------------------------------------------------------------*  00070000
//*                                                                     00080000
//ASM1    EXEC ASMFC,PARM=(LOAD,NODECK,'LINECNT=55'),                   00090000
//             MAC1='SYS1.AMODGEN'                                      00100000
//ASM.SYSPRINT DD SYSOUT=*                                              00110000
//ASM.SYSGO DD DSN=&&OBJECT,UNIT=SYSDA,SPACE=(TRK,(50,20)),             00120000
//             DISP=(MOD,PASS)                                          00130000
//ASM.SYSIN DD *                                                        00140000
         MACRO                                                          00150000
         EQUATE                                                         00160000
R0       EQU   0                                                        00170000
R1       EQU   1                                                        00180000
R2       EQU   2                                                        00190000
R3       EQU   3                                                        00200000
R4       EQU   4                                                        00210000
R5       EQU   5                                                        00220000
R6       EQU   6                                                        00230000
R7       EQU   7                                                        00240000
R8       EQU   8                                                        00250000
R9       EQU   9                                                        00260000
R10      EQU   10                                                       00270000
R11      EQU   11                                                       00280000
R12      EQU   12                                                       00290000
R13      EQU   13                                                       00300000
R14      EQU   14                                                       00310000
R15      EQU   15                                                       00320000
         MEND                                                           00330000
*          DATA SET CBT500     AT LEVEL 002 AS OF 12/16/75              00010000
         TITLE 'LISTVOL - DOCUMENTATION'                                00020000
*.....................................................................* 00030000
*.                                                                   .* 00040000
*.   LISTVOL                                                         .* 00050000
*.                                                                   .* 00060000
*        LISTVOL   LEVEL('DSNAME') DA('DSLIST')                       * 00070000
*                  VOL('SER')                                         * 00080000
*        'DSNAME' IS EITHER HIGHER LEVEL QUALIFIERS OF CATALOG        * 00090000
*                 STRUCTURE OR CONTAINS EMBEDDED ASTERISK AS          * 00100000
*                 PART OF THE NAME.                                   * 00110000
*        'DSLIST' IS LIST OF DATASETS.                                * 00120000
*        'SER'    IS LEADING CHARACTERS OF VOLUME SERIAL. ONLY        * 00130000
*                 DATASETS CATALOGED ON VOLUMES BEGINNING THUSLY      * 00140000
*                 WILL BE LISTED.                                     * 00150000
*              NOW THIS CODE IS BORROWED FROM THE LISTS COMMAND FROM  * 00160000
*              SHARE, AND IS A VERY MUCH STRIPPED DOWN VERSION OF     * 00170000
*              THAT COMMAND.                                          * 00180000
*              THE REAL WORK IS DONE IN SUBROUTINE LOCINDEX, AND SINCE* 00190000
*              I WROTE THAT MYSELF, I SUGGEST YOU LOOK THERE FOR      * 00200000
*              INSIGHT INTO MVS CATALOG MANAGEMENT.                   * 00210000
*        LEVEL CURRENT TO ALL TSO SUBJECT TO WHICH LOCINDEX YOU HAVE  * 00220000
*                                                                     * 00230000
*.....................................................................* 00240000
         TITLE 'LISTVOL - LIST VOLUME COMMAND'                          00250000
LISTVOL  START 0                                                        00260000
         STM   R14,R12,12(R13)                                          00270000
         USING LISTVOL,R15                                              00280000
         ST    R13,SAVE+4                                               00290000
         LA    R13,SAVE                                                 00300000
         B     BEGIN                                                    00310000
SAVE     DC    18F'0'                                                   00320000
         DROP  R15                                                      00330000
         USING SAVE,R13                                                 00340000
         TITLE 'LISTVOL - GETMAIN AND PARSE'                            00350000
BEGIN    LR    R4,R1                                                    00360000
         USING CPPL,R4                                                  00370000
         LA    R0,LWORK            GET LENGTH OF WORK AREA              00380000
         AH    R0,=H'4096'         GET EXTRA 4K FOR LOCINDEX WORK       00390000
         ICM   R0,B'1000',=FL1'1'  SUBPOOL 1                            00400000
         GETMAIN R,LV=(0)                                               00410000
         LR    R12,R1               SAVE ADDR OF WORK AREA              00420000
         USING WORKAREA,R12                                             00430000
         LA    R0,LOCINDXW          POINT TO LOCINDEX WORK AREA         00440000
         ST    R0,PARM+12           SAVE ADDR IN PARM LIST              00450000
         MVI   PARM+12,X'80'        INDICATE END OF PARM LIST           00460000
         MVI   WXTNT+4,X'04'                                            00470000
         LA    R3,WPPL             GET ADDR OF PPL                      00480000
         USING PPL,R3                                                   00490000
         L     R0,CPPLCBUF                                              00500000
         ST    R0,PPLCBUF          SAVE ADDR OF COMMAND BUFFER          00510000
         L     R0,CPPLUPT                                               00520000
         ST    R0,PPLUPT           SAVE ADDR OF UPT                     00530000
         L     R0,CPPLPSCB                                              00540000
         LR    R10,R0              GET ADDR OF USERID FROM PSCB         00550000
         SR    R11,R11                                                  00560000
         IC    R11,7(R10)          GET LENGTH OF USERID                 00570000
         BCTR  R11,0                                                    00580000
AUTHRZD  OI    FLAG,X'80'                                               00590000
LECT     L     R0,CPPLECT                                               00600000
         ST    R0,PPLECT           SAVE ADDR OF ECT                     00610000
         LA    R0,CPECB                                                 00620000
         ST    R0,PPLECB           SAVE ADDR OF ECB                     00630000
         XC    CPECB,CPECB         CLEAR ECB                            00640000
         L     R0,=A(PARSLIST)                                          00650000
         ST    R0,PPLPCL           SAVE ADDR OF PCL                     00660000
         LA    R0,WPDL                                                  00670000
         ST    R0,PPLANS           SAVE ADDR OF PDL AREA                00680000
         XC    PPLUWA,PPLUWA       CLEAR UWA                            00690000
         LR    R1,R3               PASS ADDR OF PPL TO PARSE            00700000
         LINK  EP=IKJPARS          LINK TO PARSE SERVICE ROUTINE        00710000
         LTR   R15,R15             PARSE ERROR                          00720000
         BNZ   RETC                 YES                                 00730000
         TM    PPLANS,X'FF'        PARSE ERROR                          00740000
         BO    RETC                 YES                                 00750000
         DROP  R3,R4                                                    00760000
         L     R3,WPDL             GET ADDR OF PDL                      00770000
         USING IKJPARMD,R3                                              00780000
         TM    EXTENT+1,X'01'      EXTENT SPECIFIED?                    00790000
         BZ    *+8                  NO                                  00800000
         OI    FLAG,X'20'           YES, SET FLAG BIT                   00810000
         TM    DSNLIST+6,X'80'      DSNAME SPECIFIED?                   00820000
         BO    DSNRTN                YES                                00830000
         TM    LEV+6,X'80'          INDEX LEVEL SPECIFIED?              00840000
         BO    LEVEL                 YES                                00850000
         B     EUSLO                NO PARMS - LISTS FOR USERID         00860000
         TITLE 'LISTVOL - DATASET NAME ROUTINE'                         00870000
DSNRTN   LA    R4,DSNLIST           POINT TO DSN LIST                   00880000
         LA    R0,L'MSG1                                                00890000
         LA    R1,MSG1                                                  00900000
         SVC   93                  TPUT HEADING                         00910000
DSNAME   L     R5,0(R4)             POINT TO DSNAME                     00920000
         LH    R6,4(R4)             GET LENGTH OF DSNAME                00930000
         BCTR  R6,0                                                     00940000
         MVI   DSN1,X'40'                                               00950000
         MVC   DSN1+1(43),DSN1     BLANK DSN WORK AREA                  00960000
         LA    R8,DSN1              POINT TO DSN WORK AREA              00970000
         TM    6(R4),X'40'          DSNAME CONTAINED IN QUOTES?         00980000
         BO    MVCDSNAM               YES                               00990000
         EX    R11,USIDMVC         MOVE USERID TO WORK AREA             01000000
         LA    R8,1(R11,R8)        BUMP PAST USERID                     01010000
         MVI   0(R8),C'.'          MOVE PERIOD TO WORK AREA             01020000
         LA    R8,1(0,R8)          BUMP PAST                            01030000
MVCDSNAM EX    R6,DSNMVC           MOVE DSNAME TO WORK AREA             01040000
         MVC   VOLS,BLANKS         MOVE BLANKS TO VOLSER WORK AREA      01050000
         TM    FLAG,X'80'          AUTHORIZED USER?                     01060000
         BZ    LOCDSN               NO                                  01070000
         TM    VOL+6,X'80'         VOLUME SPECIFIED?                    01080000
         BZ    LOCDSN               NO, LOCATE DSN                      01090000
         LA    R1,VOLS             POINT TO VOLSER WORK AREA            01100000
         L     R5,VOL              POINT TO VOLUME PARAMETER            01110000
         LH    R6,VOL+4            GET LENGTH OF VOL PARM               01120000
         BCTR  R6,0                                                     01130000
         EX    R6,MVCVOLS          MOVE VOL PARM TO VOLSER WORK AREA    01140000
         B     BALOBTN             BYPASS LOCATE                        01150000
LOCDSN   OI    FLAG,X'10'                                               01160000
         LOCATE INDS                                                    01170000
         LTR   R15,R15              DID IT FIND VOL SER                 01180000
         BNZ   ERR03                 NO WRITE ERROR MSG                 01190000
         NI    FLAG,X'EF'                                               01200000
         MVC   VOLS(6),BUF1+6      YES - MOVE IN VOL SER                01210000
         SPACE 2                                                        01220000
BALOBTN  TPUT  VOLS,52                                                  01230000
RETDSN   L     R4,24(R4)           GET DSN LINK POINTER                 01240000
         C     R4,ENDLIST          END OF LIST?                         01250000
         BE    RETC                 YES                                 01260000
         B     DSNAME               NO, GET NEXT DSNAME                 01270000
         TITLE 'LISTVOL - LEVEL ROUTINE'                                01280000
EUSLO    EQU   *                                                        01290000
**     FOR ENTIRE USERID LOGGED ON                                      01300000
         MVC   USERID(7),0(R10)        MOVE IN USERID                   01310000
         B     GOTUS               BRANCH TO GOT USERID                 01320000
         SPACE 3                                                        01330000
LEVEL    EQU   *                                                        01340000
         L     R5,LEV              POINT TO INDEX STRUCTURE             01350000
         LH    R6,LEV+4            GET LENGTH OF INDEX STRUCT           01360000
         BCTR  R6,0                                                     01370000
         LA    R8,USERID           POINT TO WORK AREA                   01380000
         EX    R6,DSNMVC           MOVE INDEX STRUCTURE TO WORK AREA    01390000
GOTUS    EQU   *                                                        01400000
         TM    VOL+8,X'80'                                              01410000
         BZ    NVOLL                                                    01420000
         LA    R1,VOLZ                                                  01430000
         L     R5,VOL              POINT TO VOLUME PARAMETER            01440000
         LH    R6,VOL+4            GET LENGTH OF VOL PARM               01450000
         BCTR  R6,0                                                     01460000
         STH   R6,LENVOLS                                               01470000
         EX    R6,MVCVOLS                                               01480000
NVOLL    LA    R0,L'MSG1                                                01490000
         LA    R1,MSG1                                                  01500000
         SVC   93                  TPUT HEADING                         01510000
         OI    FLAG,X'40'          SET FLAG FOR DATA SETS FOR USERID    01520000
LOOP     EQU   *                   LOOP FOR DATASET IN USERID           01530000
         L     R15,=V(LOCINDEX)    POINT TO EP(LOCINDEX)                01540000
         LA    R1,PARM             POINT TO PARM LIST                   01550000
         BALR  R14,R15             GO TO LOCINDEX                       01560000
         B     *+4(R15)            BRANCH , DEP ON RETURN CODE          01570000
         B     OK                                                       01580000
         B     NOUSER              4-PRINT MSG & RETRUN TO SYS          01590000
         B     OK                                                       01600000
         B     OK                                                       01610000
         B     RETC                16-FINISHED-RETURN TO SYS            01620000
         B     RDERR               20-PRINT MSG&RETRUN TO SUS           01630000
OK       TM    VOL+6,X'80'                                              01640000
         BZ    NVOL2                                                    01650000
         LH    R1,LENVOLS                                               01660000
         EX    R1,CLCVOLS                                               01670000
         BNE   LOOP                                                     01680000
NVOL2    TPUT  VOLS,52                                                  01690000
         B     LOOP                GET NEXT DSNAME FROM LOCINDEX        01700000
         SPACE 3                                                        01710000
         TITLE 'LISTVOL - EOJ AND ERROR ROUTINES'                       01720000
RETC     EQU   *                                                        01730000
         L     13,SAVE+4           LOAD R13 PREVIOUS SPACE AREA         01740000
         LM    2,12,28(13)         RELOAD REGISTERS                     01750000
         L     14,12(13)           LOAD RETURN ADDRESS                  01760000
         MVI   12(13),X'FF'        INDICATE CONTROL RETURN CALLING PROG 01770000
         BCR   15,14               RETURN TO CALLING PROGRAM            01780000
         SPACE 3                                                        01790000
ERR01    EQU   *                                                        01800000
* NO VALID COMMAND                                                      01810000
         TPUT  ERMSG1,18                                                01820000
         B     RETC                BRANCH TO RETURN CODE                01830000
* DATASET NAME NOT FOUND                                                01840000
ERR03    MVC   MSG2(L'ERMSG9),ERMSG9                                    01850000
         MVC   MSG2+L'ERMSG9(44),DSN1                                   01860000
         LA    R0,L'ERMSG9+44                                           01870000
         LA    R1,MSG2                                                  01880000
         SVC   93                                                       01890000
         TM    FLAG,X'40'                                               01900000
         BO    LOOP                                                     01910000
         TM    FLAG,X'10'                                               01920000
         B     RETDSN                                                   01930000
         SPACE 3                                                        01940000
RDERR    MVC   MSG2(L'ERMSG8),ERMSG8                                    01950000
         MVC   MSG2+L'ERMSG8(44),DSN1                                   01960000
         LA    R0,L'ERMSG8+44                                           01970000
         LA    R1,MSG2                                                  01980000
         SVC   93                                                       01990000
         B     LOOP                CONTINUE THRU LOOP                   02000000
         SPACE 3                                                        02010000
NOUSER   MVC   MSG2(L'ERMSG3),ERMSG3                                    02020000
         MVC   MSG2+L'ERMSG3(44),USERID                                 02030000
         LA    R0,L'ERMSG3+44                                           02040000
         LA    R1,MSG2                                                  02050000
         SVC   93                                                       02060000
         B     RETC                RETURN TO SYSTEM                     02070000
         SPACE 3                                                        02080000
ERRF     EQU   *                                                        02090000
         TPUT  ERMSG10,20                                               02100000
         B     RETC                                                     02110000
USIDMVC  MVC   0(0,R8),0(R10)                                           02120000
DSNMVC   MVC   0(0,R8),0(R5)                                            02130000
MVCVOLS  MVC   0(0,R1),0(R5)                                            02140000
CLCVOLS  CLC   VOLS(0),VOLZ                                             02150000
         TITLE 'LISTVOL - DATA AREAS'                                   02160000
ENDLIST  DS    0F                                                       02170000
         DC    XL4'FF000000'                                            02180000
* SETUP CONSTANTS                                                       02190000
VOLDSCTK DC    H'0'                NUM DSCH ON A TRK                    02200000
VOLF4CHR DC    XL5'00'             CCHHR OF DSCB                        02210000
VOLNO    DC    H'0'                NUM VOLUMES PROCESSED                02220000
PEXCTR   DC    H'0'                CTR FOR TATAL NUM FREE SPACE         02230000
LASTTRK  DC    H'0'                                                     02240000
VOLS     DC    CL6' '                 VOLUME SER                        02250000
         DC    C'  '                                                    02260000
DSN1     DC    CL44' '              DATASET NAME                        02270000
USERID   DC    CL44' '              USERID                              02280000
FLAG     DC    X'00'                                                    02290000
DSORG    DC    X'00'                                                    02300000
BLANKS   DC    CL6' '                                                   02310000
VOLZ     DC    CL6' '                                                   02320000
LENVOLS  DC    H'0'                                                     02330000
BUF1     DS    0D                                                       02340000
         DS    265C                                                     02350000
TRTBL    EQU   *-240                                                    02360000
         DC    C'0123456789ABCDEF'                                      02370000
MASKED   DC    XL6'402020202020'   MASK FIELD FOR EDIT                  02380000
         SPACE 3                                                        02390000
* MESSAGE TO BE PRINTED                                                 02400000
MSG1     DC    C'VOLUME  DATASET NAME             '                     02410000
MSG2     DC    CL76' '                                                  02420000
MSG4     DC    C'VOLUME  DATASET NAME             '                     02430000
         SPACE 3                                                        02440000
* ERROR MESSAGES                                                        02450000
ERMSG1   DC    C'NO VALID COMMAND'                                      02460000
ERMSG3   DC    C'INVALID LEVEL - '                                      02470000
ERMSG8   DC    C'ERROR READING CATALOG DSN - '                          02480000
ERMSG9   DC    C'DSN NOT FOUND - '                                      02490000
ERMSG10  DC    C'UNSUPPORTED KEYWORD'                                   02500000
         LTORG                                                          02510000
         EJECT                                                          02520000
* CAMLST FOR DSCB3                                                      02530000
SEEKCAM  CAMLST SEEK,VTOCCHHR,VOLS,BUF1                                 02540000
         SPACE 2                                                        02550000
* CAMLST LOCATE DSN OV VOL SER                                          02560000
INDS     CAMLST NAME,DSN1,,BUF1                                         02570000
         SPACE 2                                                        02580000
* CAMLST FOR DSCB 1                                                     02590000
SERCHCAM CAMLST SEARCH,DSN1,VOLS,BUF1                                   02600000
         EJECT                                                          02610000
* CONSTANTS AND WORK AREAS                                              02620000
HWK1     DC    H'0'                HALF WORD WORK AREA                  02630000
VOLTKCYL DC    H'19'               # TRKS/CYL                           02640000
VTOCCHHR DC    XL5'0'              TRACK ADDR WORK AREA                 02650000
WKD      DC    D'0'                DOUBLE WORK WORK AREA                02660000
         SPACE 3                                                        02670000
* PARM SETUP FOR GETDSN                                                 02680000
PARM     DC    A(USERID)           USERID ADDRESS                       02690000
         DC    A(DSN1)             DSN ADDRESS                          02700000
         DC    A(VOLS)             VOL SER ADDRESS                      02710000
         DC    A(0)                ADDRESS FOR GETMAIN                  02720000
         SPACE 3                                                        02730000
* EQUATE REGISTERS                                                      02740000
         EQUATE                                                         02750000
RCC      EQU   R8                                                       02760000
RHH      EQU   R7                                                       02770000
RR       EQU   R6                                                       02780000
         TITLE 'LISTVOL - PARSE PARAMETER LIST'                         02790000
PARSLIST IKJPARM                                                        02800000
DSNKYD   IKJKEYWD                                                       02810000
         IKJNAME 'DATASET',SUBFLD=DSNSUBF                               02820000
LEVL     IKJKEYWD                                                       02830000
         IKJNAME 'LEVEL',SUBFLD=LVL                                     02840000
VOLUME   IKJKEYWD                                                       02850000
         IKJNAME 'VOLUME',SUBFLD=VLM                                    02860000
EXTENT   IKJKEYWD                                                       02870000
         IKJNAME 'EXTENTS'                                              02880000
DSNSUBF  IKJSUBF                                                        02890000
DSNLIST  IKJPOSIT DSNAME,LIST                                           02900000
LVL      IKJSUBF                                                        02910000
LEV      IKJIDENT 'LEVEL',OTHER=ANY,MAXLNTH=44                          02920000
VLM      IKJSUBF                                                        02930000
VOL      IKJIDENT 'VOLUME',LIST,FIRST=ALPHANUM,OTHER=ALPHANUM,MAXLNTH=6 02940000
         IKJENDP                                                        02950000
         IKJPPL                                                         02960000
         IKJCPPL                                                        02970000
         TITLE 'LISTVOL - WORK AREA'                                    02980000
WORKAREA DSECT                                                          02990000
WPPL     DS    7A                                                       03000000
CPECB    DS    F                                                        03010000
WPDL     DS    F                                                        03020000
OBTNSAVE DS    10F                                                      03030000
XTNTS    DS    16XL10                                                   03040000
WXTNT    DS    CL5                                                      03050000
         DS    0D                                                       03060000
LOCINDXW EQU   *                                                        03070000
LWORK    EQU   *-WORKAREA                                               03080000
         END                                                            03090000
//*                                                                     00350000
//ASM2    EXEC ASMFC,PARM=(LOAD,NODECK,'LINECNT=55'),                   00360000
//             MAC1='SYS1.AMODGEN'                                      00370000
//ASM.SYSPRINT DD SYSOUT=*                                              00380000
//ASM.SYSGO DD DSN=&&OBJECT,UNIT=SYSDA,SPACE=(TRK,(50,20)),             00390000
//             DISP=(MOD,PASS)                                          00400000
//ASM.SYSIN DD *                                                        00410000
*          DATA SET CBT502     AT LEVEL 001 AS OF 12/15/75              00010000
*        LOCINDEX SUBROUTINE REPLACEMENT FOR MVS                        00020000
*        THIS ROUTINE SUPPORTS 'LEVEL' OPTIONS OF THE 'LISTS' COMMAND   00030000
*             AND IS USED BY OTHER YCC VARIATIONS OF THAT PROGRAM       00040000
*        FOLLOWING IS ORIGINAL LOCINDEX DESCRIPTION:                    00050000
*.....................................................................* 00060000
*.                                                                   .* 00070000
*.   LOCINDEX                                                        .* 00080000
*.                                                                   .* 00090000
*.....................................................................* 00100000
*.                                                                   .* 00110000
*.   1.0  GENERAL DESCRIPTION                                        .* 00120000
*.                                                                   .* 00130000
*.   THIS SUBROUTINE IS USED TO RETURN DSNAMES AND THE VOLSER        .* 00140000
*.   FOR A SPECIFIED INDEX STRUCTURE.  THE INDEX STRUCTURE CAN BE    .* 00150000
*.   SPECIFIED AS SEVERAL HIGH-LEVEL QUALIFIERS OR IT CAN BE A       .* 00160000
*.   DSNAME CONTAINING ONE EMBEDDED ASTERISK NOT AS THE HIGH-LEVEL   .* 00170000
*.   QUALIFIER.                                                      .* 00180000
*.                                                                   .* 00190000
*.....................................................................* 00200000
*.                                                                   .* 00210000
*.....................................................................* 00220000
*.                                                                   .* 00230000
*.   2.0  PARAMETER LIST AND RETURN CODE DESCRIPTION                 .* 00240000
*.                                                                   .* 00250000
*.   THE PARAMETER LIST IS A FOUR OR FIVE WORD LIST CONTAINING THE   .* 00260000
*.   FOLLOWING:                                                      .* 00270000
*.                                                                   .* 00280000
*.    WORD         DESCRIPTION                                       .* 00290000
*.                                                                   .* 00300000
*.      1          ADDRESS OF THE 44-BYTE FIELD CONTAINING THE       .* 00310000
*.                 INDEX STRUCTURE.                                  .* 00320000
*.      2          ADDRESS OF THE 44-BYTE FIELD INTO WHICH           .* 00330000
*.                 LOCINDEX WILL PLACE THE DSNAME.                   .* 00340000
*.      3          ADDRESS OF THE 6-BYTE FIELD INTO WHICH            .* 00350000
*.                 LOCINDEX WILL PLACE THE VOLSER OF THE DATASET.    .* 00360000
*.      4          ADDRESS OF A 4K WORK AREA TO BE USED BY           .* 00370000
*.                 LOCINDEX FOR STORING CATALOG BLOCKS.              .* 00380000
*.      5          OPTIONAL FIELD FOR THE ADDRESS OF A FULLWORD      .* 00390000
*.                 OF STORAGE ON AN INTEGRAL BOUNDARY INTO WHICH     .* 00400000
*.                 LOCINDEX WILL STORE THE RETURN CODE.              .* 00410000
*.                                                                   .* 00420000
*.   THE HIGH ORDER BIT OF THE LAST WORD OF THE PARAMETER LIST       .* 00430000
*.   MUST BE SET ON.                                                 .* 00440000
*.                                                                   .* 00450000
*.    RETURN CODE  MEANING                                           .* 00460000
*.                                                                   .* 00470000
*.         0       A DSNAME WAS FOUND AND THE DATASET RESIDES ON     .* 00480000
*.                 A DISK VOLUME.                                    .* 00490000
*.         4       THE INDEX STRUCTURE WAS NOT FOUND.                .* 00500000
*.         8       A DSNAME WAS FOUND AND THE DATASET RESIDES ON     .* 00510000
*.                 A TAPE VOLUME.                                    .* 00520000
*.        12       A DSNAME WAS FOUND BUT IT RESIDES ON MULTIPLE     .* 00530000
*.                 VOLUMES.                                          .* 00540000
*.        16       THERE ARE NO MORE DATASETS FOR THIS INDEX         .* 00550000
*.                 STRUCTURE.                                        .* 00560000
*.        20       AN I/O ERROR WAS DETECTED ON THE CATALOG.         .* 00570000
*.                                                                   .* 00580000
*.....................................................................* 00590000
*                                                                       00600000
*        THE PRIMARY DIFFERENCES BETWEEN THE MVS VERSION                00610000
*        AND THE OS VERSION ARE:                                        00620000
*              1) THE PART ABOVE THE ASTERISK IS PASSED TO VSAM         00630000
*                 GENERIC LOCATE AND WORKS HOWEVER GENERIC LOCATE       00640000
*                 LOCATE WORKS. ONLY NON-VSAM DATASETS ARE PASSED       00650000
*                 BACK TO THE CALLER.                                   00660000
*              2) THE CHARACTERS BELOW THE ASTERISK MUST APPEAR         00670000
*                 IN THE DATASET NAME ANYWHERE AFTER THE SEARCH         00680000
*                 KEY CHARACTERS. THEY MAY CONTAIN LEADING              00690000
*                 AND TRAILING PARTIAL INDEX LEVELS.                    00700000
*              3) THE AMOUNT OF STORAGE REQUIRED FOR A VSAM GENERIC     00710000
*                 LOCATE IS MUCH MORE THAN THE 4K AREA PROVIDED BY      00720000
*                 THE OS CALLER. A 24K AREA IS GETMAINED AND ITS        00730000
*                 ADDRESS IS STORED IN THE FIRST WORK OF THE 4K         00740000
*                 CALLER WORK AREA. IT IS FREEMAINED WHEN THE           00750000
*                 RETURN CODE OF 16 SIGNALS THE END OF DATASETS         00760000
*                 UNDER THIS INDEX.                                     00770000
*              5) THE 265 BYTE CAMLIST WORK AREA IS PUT IN THE          00780000
*                 CALLER PROVIDED WORK AREA AT OFFSET 4. THIS           00790000
*                 ALLOWS ACCESS TO THE COMPLETE VOLUME LIST.            00800000
         EJECT                                                          00810000
LOCINDEX CSECT                                                          00820000
         USING *,15                                                     00830000
         SAVE  (14,12),,*                                               00840000
         GETMAIN R,LV=LSECT                                             00850000
         ST    R13,4(R1)                                                00860000
         ST    R1,8(13)                                                 00870000
         LR    R12,R13                                                  00880000
         LR    R13,R1                                                   00890000
         L     R1,24(R12)                                               00900000
         BALR  R12,0                                                    00910000
         USING *,12                                                     00920000
         USING DSASECT,R13                                              00930000
R0       EQU   0                                                        00940000
R1       EQU   1                                                        00950000
R2       EQU   2                                                        00960000
R3       EQU   3                                                        00970000
R4       EQU   4                                                        00980000
R5       EQU   5                                                        00990000
R6       EQU   6                                                        01000000
R7       EQU   7                                                        01010000
R8       EQU   8                                                        01020000
R9       EQU   9                                                        01030000
R10      EQU   10                                                       01040000
R11      EQU   11                                                       01050000
R12      EQU   12                                                       01060000
R13      EQU   13                                                       01070000
R14      EQU   14                                                       01080000
R15      EQU   15                                                       01090000
         ST    R1,INPARM                                                01100000
         L     R10,12(R1)                                               01110000
         USING WORKSECT,R10                                             01120000
         L     R2,0(R1)                                                 01130000
         CLC   OINDEX,0(R2)   SAME INDEX STRUCTURE AS LAST CALL?        01140000
         BE    CAMLOC         GO GET NEXT DATASET                       01150000
         MVC   OINDEX,0(R2)                                             01160000
         SPACE 10                                                       01170000
BLDKEY   DS    0H                                                       01180000
*        THIS BLOCK ANALIZES THE NEW INDEX STRUCTURE INTO COMPONENTS    01190000
*        IT CONSTRUCTS A VSAM CATALOG GENERIC SEARCH KEY AND AN         01200000
*        OPTIONAL LOWER LEVEL QUALIFIER VERIFICATION STRING             01210000
*        ONE ASTERISK IS ALLOWED ANYWHERE BELOW THE USERID.             01220000
*        IT MAY REPRESENT ALL OR PART OF AN 'INDEX LEVEL'               01230000
*        EXITS: NORMAL TO 'GENLOC'                                      01240000
*               TO 'ERROR4' IF A SYSTAX ERROR OCCURS IN INDEX STRUCTURE 01250000
         SR    R1,R1                                                    01260000
         SR    R2,R2          CLEAR FOR TRT INSTRUCTION                 01270000
         MVI   TRTAB,0                                                  01280000
         MVC   TRTAB+1(255),TRTAB                                       01290000
         MVI   TRTAB+C' ',4                                             01300000
         MVI   TRTAB+C'*',8                                             01310000
         MVI   TRTAB+C'.',12                                            01320000
         MVC   KEY,OINDEX     COPY INDEX INPUT                          01330000
         TRT   KEY,TRTAB                                                01340000
         BZ    ERROR4         MUST BE A BLANK IN 44 CHARS               01350000
         B     *(R2)                                                    01360000
         B     USERID         C' ' MUST BE A USERID                     01370000
         B     ERROR4         C'*' ASTERISK MUST FOLLOW PERIOD          01380000
         B     COMPLEX        C'.' MUST DO FULL ANALYSIS OF STRUCTURE   01390000
         SPACE 5                                                        01400000
USERID   DS    0H                                                       01410000
*        THIS BLOCK BUILDS SEARCH KEY FOR USERID. MUST BE <9 CHARACTERS 01420000
*        AND WE ADD A PERIOD TO IT TO INDICATE TO GENERIC LOCATE        01430000
*        THAT WE ARE INTERESTED IN THE DATASETS UNDER THE NAME AND      01440000
*        NOT THE ALIAS RECORD (CVOL POINTER) OF THE NAME ITSELF.        01450000
         MVI   0(R1),C'.'                                               01460000
         LA    R3,KEY                                                   01470000
         SR    R1,R3                                                    01480000
         CH    R1,=H'8'                                                 01490000
         BH    ERROR4                                                   01500000
         LA    R1,1(R1)                                                 01510000
         STC   R1,NAME        STORE KEY LENGTH                          01520000
         MVI   LREST,X'80'                                              01530000
         B     GENLOC                                                   01540000
* END OF USERID                                                         01550000
         SPACE 5                                                        01560000
COMPLEX  DS    0H                                                       01570000
*        NOW WE SEARCH FOR AN IMBEDDED ASTERISK IN INDEX STRUCTURE      01580000
*        CHARACTERS ABOVE IT ARE USED AS THE GENERIC KEY.               01590000
*        CHARACTERS AFTER IT ARE USED AS THE LOWER LEVEL QUALIFIERS.    01600000
         MVI   TRTAB+C'.',0   NO LONGER INTERESTED IN PERIODS           01610000
         TRT   KEY,TRTAB                                                01620000
         BZ    ERROR4                                                   01630000
         B     *(R2)                                                    01640000
         B     ONEPART        NO ASTERISK                               01650000
         B     TWOPART                                                  01660000
         SPACE 5                                                        01670000
ONEPART  MVI   LREST,X'80'                                              01680000
         LA    R3,KEY                                                   01690000
         SR    R1,R3                                                    01700000
         STH   R1,LKEY                                                  01710000
         STC   R1,NAME                                                  01720000
         B     GENLOC                                                   01730000
         SPACE 5                                                        01740000
TWOPART  DS    0H                                                       01750000
*        THIS BLOCK HANDLES THE CASE WHERE THERE IS AN EMBEDDED         01760000
*        ASTERISK IN THE INDEX LEVEL. THE CHARACTERS BELOW THE          01770000
*        ASTERISK ARE MOVED TO 'REST'. THE CHARACTERS FROM THE          01780000
*        ASTERISK ON ARE BLANKED IN 'KEY'. THE LENGTH OF THE            01790000
*        NON BLANK PART OF 'REST' LESS ONE IS SAVED IN 'LREST'          01800000
*        FOR USE IN THE LATER CLC INSTRUCTION. THE LENGTH OF            01810000
*        THE KEY IS SAVED IN 'NAME' FOR THE GENERIC LOCATE AND          01820000
*        IN 'LKEY' FOR AN INDICATION OF HOW MANY CHARS TO               01830000
*        SKIP BEFORE LOOKING IN A DSN FOR A MATCH TO 'REST'.            01840000
         LA    R3,KEY+42                                                01850000
         SR    R3,R1         GET LENGTH OF KEY ABOVE ASTERISK           01860000
         MVC   REST,=CL44' ' INITIALIZE REST                            01870000
         EX    R3,MOVREST    MOVE PART BELOW ASTERISK                   01880000
         LA    R3,1(R3)                                                 01890000
         EX    R3,BLNKEY     BLANK KEY FROM ASTERISK ON                 01900000
         SH    R3,=H'43'                                                01910000
         LPR   R3,R3         GET CHARS ABOVE ASTERISK                   01920000
         STH   R3,LKEY                                                  01930000
         STC   R3,NAME                                                  01940000
         TRT   REST,TRTAB    FIND NON-BLANK LENGTH OF REST              01950000
         B     *(R2)                                                    01960000
         B     LENRST                                                   01970000
         B     ERROR4        SORRY, ONLY ONE * PER CUSTOMER             01980000
LENRST   LA    R3,REST+1     CALCULATE LENGTH FROM ADDRESS              01990000
         SR    R1,R3            OF FIRST BLANK                          02000000
         STH   R1,LREST                                                 02010000
         B     GENLOC                                                   02020000
MOVREST  MVC   REST(0),1(R1)                                            02030000
BLNKEY   MVC   0(0,R1),=CL44' '                                         02040000
* END OF TWOPART                                                        02050000
* END OF COMPLEX                                                        02060000
* END OF BLDKEY                                                         02070000
         EJECT                                                          02080000
GENLOC   DS    0H                                                       02090000
*        THIS BLOCK ISSUES A VSAM GENERIC LOCATE TO BUILD AN IN-CORE    02100000
*        LIST OF DATASETS THAT BEGIN WITH THE SEARCH KEY                02110000
*        THERE IS NO GOOD DOCUMENTATION ON THE GENERIC                  02120000
*        LOCATE IN THE LITERATURE. SEE THE CATALOG PLM FOR              02130000
*        WHAT LITTLE THERE IS. THE FORM BELOW WAS INFERRED              02140000
*        FROM SOME FICHE AND BY INTERCEPTING SVC 26 WITH DSS.           02150000
         MVC   GENFLAG,=X'05201100'                                     02160000
         XC    GENX1,GENX1                                              02170000
         XC    GENX2,GENX2                                              02180000
         LA    R1,NAME                                                  02190000
         ST    R1,GENNAME                                               02200000
         GETMAIN R,LV=X'7FF8'                                           02210000
         ST    R1,GETADDR                                               02220000
         ST    R1,GENWORK                                               02230000
         MVC   0(4,R1),=X'7FF80004'                                     02240000
         LA    R1,GENPARM                                               02250000
         SVC   26                                                       02260000
         LTR   R15,R15                                                  02270000
         BNZ   ERROR4                                                   02280000
         L     R1,GETADDR    GET VSAM CATLG RETURN AREA                 02290000
         LA    R1,0(R1)       CLEAR HIGH BYTE                           02300000
         LH    R2,2(R1)       GET NUMBER BYTES USED                     02310000
         AR    R2,R1          ADD START ADDR                            02320000
         ST    R2,LAST        SAVE LAST BYTE ADDR                       02330000
         LA    R1,49(R1)      SKIP 4 BYTE PREFIX AND FIRST 45 BYTE ENTR 02340000
         ST    R1,NEXT        SAVE ADDR OF FIRST DSN ENTRY              02350000
* END OF GENLOC                                                         02360000
         SPACE 10                                                       02370000
CAMLOC   DS    0H                                                       02380000
*        NOW DO A REGULAR CAMLIST NAME LOCATE FOR THE NEXT NON-VSAM     02390000
*        DATASET IN THE INCORE LIST                                     02400000
         XC    CAMLST(16),CAMLST                                        02410000
         LA    R1,KEY                                                   02420000
         ST    R1,CAMLST+4                                              02430000
         LA    R1,VOLCNT                                                02440000
         ST    R1,CAMLST+12                                             02450000
         L     R1,NEXT                                                  02460000
TEST     C     R1,LAST                                                  02470000
         BNL   RET16          NO MORE DATASETS IN LIST                  02480000
         CLI   0(R1),C'A'     IS IT NON-VSAM                            02490000
         BE    S1                                                       02500000
NOGO     LA    R1,45(R1)                                                02510000
         B     TEST                                                     02520000
S1       TM    LREST,X'80'    IS THERE A LOWER QUALIFIER                02530000
         BO    S2             NO, SO GO LOCATE                          02540000
         SPACE 5                                                        02550000
TESTQUAL DS    0H                                                       02560000
*        THIS CODE LOOKS FOR THE CHARACTER STRING BELOW THE             02570000
*        ASTERISK IN THE INDEX SEARCH KEY. THIS STRING MUST             02580000
*        BE SOMEWHERE IN THE DATASET NAME BELOW THE KEY OR              02590000
*        THE NAME IS REJECTED                                           02600000
         LA    R2,1(R1)                                                 02610000
         AH    R2,LKEY        SKIP GENERIC KEY CHARS                    02620000
         LH    R3,LREST       GET # CHARS-1 IN REST                     02630000
         LA    R4,44                                                    02640000
         SR    R4,R3                                                    02650000
         SH    R4,LKEY        R4=# CHARS BELOW KEY + 1                  02660000
COMP     EX    R3,COMPAR                                                02670000
         BE    S2                                                       02680000
         LA    R2,1(R2)                                                 02690000
         BCT   R4,COMP                                                  02700000
         B     NOGO                                                     02710000
COMPAR   CLC   REST(0),0(R2)                                            02720000
* END OF TESTQUAL                                                       02730000
         SPACE 5                                                        02740000
S2       DS    0H                                                       02750000
*        NOW TO ISSUE NORMAL LOCATE BY NAME AND RETURN INFORMATION      02760000
*        ACCORDING TO DESCRIPTION OF PARAMETERS IN LEADING DOCUMENT     02770000
         MVC   KEY,1(R1)      COPY DSN                                  02780000
         LA    R1,45(R1)      PT TO NXT DSN                             02790000
         ST    R1,NEXT        SAVE FOR NXT TIME                         02800000
         LA    R1,CAMLST                                                02810000
         SVC   26             LOCATE BY NAME                            02820000
         L     R2,INPARM      NOW GET PASSED PARM LIST                  02830000
         LM    R2,R3,4(R2)    PICK UP 2ND & 3RD USER PARMS              02840000
         MVC   0(44,R2),KEY   SAVE DSN                                  02850000
         MVC   0(6,R3),SER    SAVE VOL                                  02860000
         TM    DEVT+2,X'20'   IS THIS DISK                              02870000
         BZ    ERROR8         NO, PASS 8 RETURN CODE                    02880000
         CLI   VOLCNT+1,1     IS IT MULTI-VOL                           02890000
         BNE   ERROR12        YES, PASS 12 RETURN CODE                  02900000
* END OF CAMLOC                                                         02910000
         SPACE 10                                                       02920000
*VARIOUS RETURNS                                                        02930000
RETN     SR    R15,R15                                                  02940000
         B     RC                                                       02950000
ERROR4   LA    R15,4                                                    02960000
         B     RC                                                       02970000
ERROR8   LA    R15,8                                                    02980000
         B     RC                                                       02990000
ERROR12  LA    R15,12                                                   03000000
         B     RC                                                       03010000
RET16    FREEMAIN R,LV=X'7FF8',A=GETADDR                                03020000
         LA    R15,16                                                   03030000
         B     RC                                                       03040000
ERROR20  LA    R15,20                                                   03050000
RC       L     R1,INPARM                                                03060000
         TM    12(R1),X'80'                                             03070000
         BO    EXIT                                                     03080000
         L     R1,16(R1)                                                03090000
         ST    R15,0(R1)                                                03100000
EXIT     L     R13,4(R13)                                               03110000
         ST    R15,16(R13)                                              03120000
         L     R1,8(R13)                                                03130000
         FREEMAIN R,LV=LSECT,A=(1)                                      03140000
         LM    R14,R12,12(R13)                                          03150000
         BR    R14                                                      03160000
         EJECT                                                          03170000
DSASECT  DSECT                                                          03180000
         DS    18A                                                      03190000
GENPARM  DS    0F            GENERIC LOCATE PARM AREA                   03200000
GENFLAG  DS    F                                                        03210000
GENNAME  DS    A                                                        03220000
GENX1    DS    A                                                        03230000
GENWORK  DS    A                                                        03240000
GENX2    DS    3A                                                       03250000
KEYLEN   DS    0H                                                       03260000
         DS    X                                                        03270000
NAME     DS    X                                                        03280000
KEY      DS    CL44                                                     03290000
INPARM   DS    A             SAVE R1 UPON ENTRY TO LOCINDEX             03300000
CAMLST   DS    4A                                                       03310000
         DS    0D                                                       03320000
LSECT    EQU   *-DSASECT                                                03330000
         SPACE 10                                                       03340000
GETSECT  DSECT               GETMAINED VSAM WORK AREA                   03350000
GETLEN   DS    H             LENGT OF AREA                              03360000
GETUSED  DS    H             AMOUNT ALLOCATED CURRENTLY TO DATA         03370000
GETENTY  DS    0CL45         FIRST ELEMENT OF DSN ARRAY                 03380000
GETTYPE  DS    C             TYPE FLAG ('A'=NONVSAM)                    03390000
GETNAME  DS    CL44          DSN                                        03400000
         SPACE 10                                                       03410000
WORKSECT DSECT               WORK AREA PASSED AS PARM                   03420000
GETADDR  DS    A             ADDRESS OF GETMAINED VSAM WORK AREA        03430000
TRTAB    DS    0CL256                                                   03440000
VOLCNT   DS    H                                                        03450000
DEVT     DS    XL4                                                      03460000
SER      DS    CL6                                                      03470000
SEQ      DS    H                                                        03480000
         DS    CL251                                                    03490000
OINDEX   DS    CL44          PREVIOUS VALUE OF FIRST PARM               03500000
REST     DS    CL44          LOW QUALIFIER COMPARE STRING               03510000
NEXT     DS    A             ADDRESS OF NEXT DSN IN INCORE TABLE        03520000
LAST     DS    A             ADDRESS OF BYTE AFTER END OF DSNLIST       03530000
LKEY     DS    H             LENGTH OF GENERIC KEY                      03540000
NOREST   DS    0B            FLAG IF 'REST' IS EMPTY                    03550000
LREST    DS    H             LENGTH OF REST                             03560000
         END                                                            03570000
//LKED    EXEC PGM=IEWL,REGION=96K,COND=(0,NE),                         00430000
//             PARM='XREF,LET,LIST,AC=0'                                00440000
//SYSPRINT DD  SYSOUT=*                                                 00450000
//SYSLIN   DD  DSN=&&OBJECT,DISP=(OLD,DELETE)                           00460000
//         DD  *                                                        00470000
  NAME LISTVOL(R)                                                       00480000
//SYSLMOD  DD  DSN=SYS2.CMDLIB,DISP=SHR                                 00490000
//*                                                                     00500000
//IEBUPDTE EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW,COND=(0,NE)          00510000
//SYSUT2   DD  DSN=SYS2.HELP,DISP=SHR                                   00520000
//SYSPRINT DD  SYSOUT=*                                                 00530000
//SYSIN    DD  *                                                        00540000
./  ADD  NAME=LISTVOL                                                   00550000
)F   FUNCTION -
   THE LISTVOL COMMAND LISTS THE VOLUMES OF CATALOGED DATASETS.
   THE OUTPUT IS MORE COMPACT THAN LISTC VOL.
)X  SYNTAX -
    LISTVOL   LEVEL('DSNAME')         DA('DSLIST')
              VOL('SER')
    DEFAULTS - LEVEL(USERID)
)O  OPERANDS -
))LEVEL('DSNAME')
             - SPECIFIES EITHER HIGHER LEVEL QUALIFIERS OF
               CATALOG STRUCTURE OR CONTAINS EMBEDDED ASTERISK
               AS PART OF THE NAME.
))DATASET('DSLIST')
             - SPECIFIES A DSNAME LIST FOR WHICH YOU DESIRE TO
               HAVE VOLUME INFORMATION LISTED.
))VOLUME('VOLSER')
             - SPECIFIES LEADING CHARACTERS OF VOLUME SERIAL.  ONLY
               DATASETS CATALOGED ON VOLUMES BEGINNING WITH THE
               SPECIFIED CHARACTERS WILL BE LISTED.
./  ENDUP                                                               00570000
//                                                                      00580000
