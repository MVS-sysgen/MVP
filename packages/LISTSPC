//LISTSPC  JOB (TSO),
//             'Install LISTSPC',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00011000
//* SOURCE: CBT (V129) FILE #49                                         00012000
//* TARGET: SYS2.CMDLIB   SYS2.HELP                                     00013000
//*                                                                     00014000
//********************************************************************* 00015000
//* This job installs the LISTSPC TSO command and help.               * 00016002
//********************************************************************* 00019100
//*                                                                     00020000
//INSTALL PROC SOUT='*',               <=== SYSOUT CLASS                00190000
//             LIB='SYS2.CMDLIB',      <=== TARGET LOAD LIBRARY         00200000
//             HELP='SYS2.HELP',       <=== HELP LIBRARY                00210000
//             SYSTS=SYSDA,            <=== UNITNAME FOR WORK DATASETS  00220000
//             ASMBLR=IFOX00,          <=== NAME OF YOUR ASSEMBLER      00230000
//             ALIB='SYS1.LINKLIB',    <=== LOCATION OF YOUR ASSEMBLER  00240003
//             MACLIB='SYS1.MACLIB',   <=== MACLIB DATASET NAME         00260000
//             AMODGEN='SYS1.AMODGEN'  <=== AMODGEN DATASET NAME        00270001
//*                                                                     00300000
//IEBUPDTE EXEC PGM=IEBUPDTE,PARM=NEW                                   00310000
//SYSPRINT DD  SYSOUT=&SOUT                                             00320000
//SYSUT1   DD  DSN=&HELP,DISP=SHR                                       00330000
//SYSUT2   DD  DSN=&HELP,DISP=SHR                                       00340000
//*                                                                     00350000
//ASM     EXEC PGM=&ASMBLR,REGION=2048K,PARM='NOOBJECT,DECK,NOALIGN'    00360000
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00370000
//SYSTERM  DD  SYSOUT=&SOUT                                             00380000
//SYSPRINT DD  SYSOUT=&SOUT                                             00390000
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00400000
//         DD  DSN=&AMODGEN,DISP=SHR                                    00430000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00440000
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00450000
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00460000
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(,PASS,DELETE),            00470000
//             SPACE=(TRK,(10,1),RLSE)                                  00480000
//*                                                                     00490000
//ASM2    EXEC PGM=&ASMBLR,REGION=2048K,PARM='NOOBJECT,DECK,NOALIGN'    00360000
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00370000
//SYSTERM  DD  SYSOUT=&SOUT                                             00380000
//SYSPRINT DD  SYSOUT=&SOUT                                             00390000
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00400000
//         DD  DSN=&AMODGEN,DISP=SHR                                    00430000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00440000
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00450000
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00460000
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(MOD,PASS) ,               00470000
//*                                                                     00490000
//LKED    EXEC PGM=IEWL,COND=(0,NE),PARM='LIST,MAP,XREF'                00500000
//SYSPRINT DD  SYSOUT=&SOUT                                             00510000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,10)                               00520000
//SYSLMOD  DD  DSN=&LIB,DISP=SHR                                        00530000
//SYSLIN   DD  DSN=&&SYSLIN,DISP=(OLD,DELETE)                           00540000
//         DD  DDNAME=SYSIN                                             00550000
//        PEND                                                          00560000
//*                                                                     00570000
//        EXEC INSTALL                                                  00580000
//*                                                                     00020000
//IEBUPDTE.SYSIN DD *                                                   00540000
./  ADD  NAME=LISTSPC                                                   00550000
)F FUNCTION -
    THE LISTSPC COMMAND LISTS THE TRACKS ALLOCATED, TRACKS UNUSED, AND
    NUMBER OF EXTENTS FOR CATALOGED DISK DATA SETS, INDIVIDUALLY OR IN
    SMALL GROUPS. IT IS MORE EFFICIENT FOR THIS KIND OF DISPLAY THAN
    THE SPACE COMMAND SINCE IT SEARCHES FOR CATALOGED DATA SETS
    ONLY, WHEREAS SPACE SEARCHES FOR ANY DISK-RESIDENT DATA SETS -
    CATALOGED OR NOT.
)X SYNTAX -
    LISTSPC   DATASET('DSNAME-LIST')  LEVEL('INDEX-LEVEL')
              VOLUME('VOLUME-LIST')  EXTENTS
    REQUIRED - DATESET('DSNAME') OR LEVEL('INDEX-LEVEL') OR
               VOLUME('VOLUME')
    DEFAULTS - NONE
    ALIAS    - NONE
)O OPERANDS -
))DATASET('DSNAME-LIST') -
               - SPECIFIES A LIST OF DATA SET NAMES ABOUT WHICH YOU WISH
                 TO HAVE SPACE INFORMATION DISPLAYED.
))LEVEL('INDEX-LEVEL') -
               - SPECIFIES AN INDEX LEVEL IN THE CATALOG FOR
                 WHICH YOU WANT SPACE INFORMATION. THIS MAY BE
                 A DSNAME CONTAINING ONE EMBEDDED ASTERISK.
))VOLUME('VOLUME-LIST') -
               - IF USED WITH THE DATASET PARAMETER, SPECIFIES THE
                 VOLUME THE DATA SET RESIDES ON. NO CATALOG SEARCH
                 WILL BE PERFORMED.
                 IF SPECIFIED ALONE, SPECIFIES A LIST OF VOLUMES FOR
                 WHICH FREE SPACE INFORMATION WILL BE PRODUCED.
))EXTENTS      - IF SPECIFIED, THEN THE EXTENTS FOR EACH DATA SET WILL
                 BE DISPLAYED WITH HEXADECIMAL ADDRESSES.
./  ENDUP                                                               00870000
/*
//*----------------------------------------------------------- IEBUPDTE
//*
//ASM.SYSIN DD *
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
*          DATA SET CBT501     AT LEVEL 002 AS OF 12/30/77              00010000
*          DATA SET CBT501     AT LEVEL 002 AS OF 12/16/75              00020000
         TITLE 'LISTSPC - DOCUMENTATION'                                00030000
*.....................................................................* 00040000
*.                                                                   .* 00050000
*.   LISTSPC                                                         .* 00060000
*.                                                                   .* 00070000
*.....................................................................* 00080000
*.                                                                   .* 00090000
*.   1.0  GENERAL DESCRIPTION                                        .* 00100000
*.                                                                   .* 00110000
*.   THIS TSO COMMAND IS A MULTI-PURPOSE PROGRAM FOR LISTING         .* 00120000
*.   THE SPACE (IN TRACKS) USED BY DATASETS AND THE FREE SPACE       .* 00130000
*.   ON DISK VOLUMES.  IT CAN ALSO LIST THE EXTENTS AND VOLSER       .* 00140000
*.   OF DATASETS.                                                    .* 00150000
*.                                                                   .* 00160000
*.....................................................................* 00170000
*.                                                                   .* 00180000
*.....................................................................* 00190000
*.                                                                   .* 00200000
*.   2.0  COMMAND SYNTAX                                             .* 00210000
*.                                                                   .* 00220000
*.    LISTSPC  DATASET(DSNAME-LIST) LEVEL(INDEX-STRUCTURE)           .* 00230000
*.             VOLUME(VOLUME-LIST) EXTENTS                           .* 00240000
*.                                                                   .* 00250000
*.       DATASET(DSNAME-LIST)                                        .* 00260000
*.          -  SPECIFIES A DSNAME LIST FOR WHICH YOU DESIRE TO       .* 00270000
*.             HAVE THE SPACE INFORMATION LISTED.  TSO DATASET       .* 00280000
*.             NAMING CONVENTIONS APPLY FOR THE DSNAMES.             .* 00290000
*.                                                                   .* 00300000
*.       LEVEL(INDEX-STRUCTURE)                                      .* 00310000
*.          -  SPECIFIES AN INDEX STRUCTURE FOR WHICH YOU DESIRE     .* 00320000
*.             TO HAVE THE SPACE INFORMATION LISTED FOR EACH         .* 00330000
*.             DATASET WITHIN THIS INDEX STRUCTURE.  THIS MAY        .* 00340000
*.             BE A DSNAME CONTAINING ONE EMBEDDED ASTERISK NOT      .* 00350000
*.             AS THE HIGH-LEVEL QUALIFIER OR IT MAY BE AN INDEX     .* 00360000
*.             LEVEL.                                                .* 00370000
*.                                                                   .* 00380000
*.       VOLUME(VOLUME-LIST)                                         .* 00390000
*.          -  IF USED WITH THE 'DATASET' PARAMETER, SPECIFIES THE   .* 00400000
*.             VOLUME ON WHICH THE DATASET RESIDES.  NO CATALOG      .* 00410000
*.             SEARCH WILL BE PERFORMED FOR THE DATASETS.            .* 00420000
*.          -  IF USED AS THE ONLY PARAMETER, SPECIFIES A LIST OF    .* 00430000
*.             VOLUMES FOR WHICH FREE SPACE INFORMATION WILL BE      .* 00440000
*.             LISTED.                                               .* 00450000
*.                                                                   .* 00460000
*.       EXTENTS                                                     .* 00470000
*.          -  SPECIFIES THAT THE VOLSER AND THE DATASET'S EXTENTS   .* 00480000
*.             (IN A HEXADECIMAL FORMAT) WILL BE LISTED FOR EACH     .* 00490000
*.             DATASET.                                              .* 00500000
*.                                                                   .* 00510000
*.     NOTE:  THE USE OF THE 'VOLUME' AND THE 'EXTENTS'              .* 00520000
*.            PARAMETERS CAN BE RESTRICTED TO CERTAIN USERIDS        .* 00530000
*.            THROUGH THE USE OF BIT 0 IN THE FIELD LABELED          .* 00540000
*.            'FLAG'.                                                .* 00550000
*.                                                                   .* 00560000
*.....................................................................* 00570000
*.                                                                   .* 00580000
*.....................................................................* 00590000
*.                                                                   .* 00600000
*.   3.0  PROGRAM STRUCTURE                                          .* 00610000
*.                                                                   .* 00620000
*.                                                                   .* 00630000
*.   3.0.1  INITIALIZATION                                           .* 00640000
*.                                                                   .* 00650000
*.   THIS SECTION BUILDS THE PARAMETER LISTS FOR PARSE AND           .* 00660000
*.   LOCINDEX.  IT CAN, IF DESIRED, CHECK THE USERID FOR             .* 00670000
*.   AUTHORIZATION AND SET BIT 0 OF THE FLAG FIELD IF THE USER       .* 00680000
*.   IS AUTHORIZED.  IT THEN LINKS TO PARSE AND DETERMINES           .* 00690000
*.   WHICH TYPE OF REQUEST IS BEING MADE.                            .* 00700000
*.                                                                   .* 00710000
*.                                                                   .* 00720000
*.   3.0.2  DATASET ROUTINE                                          .* 00730000
*.                                                                   .* 00740000
*.   THIS ROUTINE IS ENTERED IF THE 'DATASET' PARAMETER IS           .* 00750000
*.   SPECIFIED.  IF THE 'VOLUME' PARAMETER IS ALSO SPECIFIED,        .* 00760000
*.   THE 'LOCATE' IS BYPASSED.  IT THEN LINKS TO THE OBTAIN AND      .* 00770000
*.   PRINT ROUTINE.                                                  .* 00780000
*.                                                                   .* 00790000
*.                                                                   .* 00800000
*.   3.0.3  LEVEL ROUTINE                                            .* 00810000
*.                                                                   .* 00820000
*.   THIS ROUTINE IS ENTERED IF THE 'LEVEL' PARAMETER IS SPECIFIED.  .* 00830000
*.   IT LINKS TO LOCINDEX TO GET THE DSNAMES FOR THE INDEX           .* 00840000
*.   STRUCTURE AND THEN LINKS TO THE OBTAIN AND PRINT ROUTINE.       .* 00850000
*.                                                                   .* 00860000
*.                                                                   .* 00870000
*.   3.0.4  VOLUME ROUTINE                                           .* 00880000
*.                                                                   .* 00890000
*.   THIS ROUTINE IS ENTERED IF THE 'VOLUME' PARAMETER IS            .* 00900000
*.   SPECIFIED AS THE ONLY PARAMETER.  IT 'OBTAIN'S THE FORMAT 4     .* 00910000
*.   DSCB AND EXTRACTS CERTAIN FIELDS.  IT THEN 'OBTAIN'S ALL        .* 00920000
*.   OF THE FORMAT 5 DSCB RECORDS AND ACCUMULATES THE FREE           .* 00930000
*.   EXTENTS.                                                        .* 00940000
*.                                                                   .* 00950000
*.                                                                   .* 00960000
*.   3.0.5  OBTAIN AND PRINT ROUTINE                                 .* 00970000
*.                                                                   .* 00980000
*.   THIS ROUTINE 'OBTAIN'S THE FORMAT 1 DSCB AND THE FORMAT 3       .* 00990000
*.   DSCB (IF ONE EXISTS) FOR A DATASET AND ACCUMULATES THE          .* 01000000
*.   EXTENTS.  IT THEN PRINTS THE DSORG, # OF ALLOCATED TRACKS,      .* 01010000
*.   # OF UNUSED TRACKS, # OF EXTENTS, AND THE DSNAME.  IF THE       .* 01020000
*.   'EXTENTS' KEYWORD WAS SPECIFIED, IT ALSO PRINTS THE VOLSER      .* 01030000
*.   AND THE EXTENTS FOR THE DATASET.                                .* 01040000
*.                                                                   .* 01050000
*.....................................................................* 01060000
         TITLE 'LISTSPC - LIST SPACE COMMAND'                           01070000
LISTSPC  START 0                                                        01080000
         STM   R14,R12,12(R13)                                          01090000
         USING LISTSPC,R15                                              01100000
         ST    R13,SAVE+4                                               01110000
         LA    R13,SAVE                                                 01120000
         B     BEGIN                                                    01130000
SAVE     DC    18F'0'                                                   01140000
         DROP  R15                                                      01150000
         USING SAVE,R13                                                 01160000
         TITLE 'LISTSPC - GETMAIN AND PARSE'                            01170000
BEGIN    LR    R4,R1                                                    01180000
         USING CPPL,R4                                                  01190000
         LA    R0,LWORK            GET LENGTH OF WORK AREA              01200000
         AH    R0,=H'4096'         GET EXTRA 4K FOR LOCINDEX WORK       01210000
         ICM   R0,B'1000',=FL1'1'  SUBPOOL 1                            01220000
         GETMAIN R,LV=(0)                                               01230000
         LR    R12,R1               SAVE ADDR OF WORK AREA              01240000
         USING WORKAREA,R12                                             01250000
         LA    R0,LOCINDXW          POINT TO LOCINDEX WORK AREA         01260000
         ST    R0,PARM+12           SAVE ADDR IN PARM LIST              01270000
         MVI   PARM+12,X'80'        INDICATE END OF PARM LIST           01280000
         MVI   WXTNT+4,X'04'                                            01290000
         LA    R3,WPPL             GET ADDR OF PPL                      01300000
         USING PPL,R3                                                   01310000
         L     R0,CPPLCBUF                                              01320000
         ST    R0,PPLCBUF          SAVE ADDR OF COMMAND BUFFER          01330000
         L     R0,CPPLUPT                                               01340000
         ST    R0,PPLUPT           SAVE ADDR OF UPT                     01350000
         L     R0,CPPLPSCB                                              01360000
         LR    R10,R0              GET ADDR OF USERID FROM PSCB         01370000
         SR    R11,R11                                                  01380000
         IC    R11,7(R10)          GET LENGTH OF USERID                 01390000
         BCTR  R11,0                                                    01400000
*        CLC   0(6,R10),=C'SYSTEM'                           ** VPI **  01410000
*        BE    AUTHRZD                                       ** VPI **  01420000
*        CLC   0(6,R10),=C'CONSUT'                           ** VPI **  01430000
*        BNE   LECT                                          ** VPI **  01440000
AUTHRZD  OI    FLAG,X'80'                                               01450000
LECT     L     R0,CPPLECT                                               01460000
         ST    R0,PPLECT           SAVE ADDR OF ECT                     01470000
         LA    R0,CPECB                                                 01480000
         ST    R0,PPLECB           SAVE ADDR OF ECB                     01490000
         XC    CPECB,CPECB         CLEAR ECB                            01500000
         L     R0,=A(PARSLIST)                                          01510000
         ST    R0,PPLPCL           SAVE ADDR OF PCL                     01520000
         LA    R0,WPDL                                                  01530000
         ST    R0,PPLANS           SAVE ADDR OF PDL AREA                01540000
         XC    PPLUWA,PPLUWA       CLEAR UWA                            01550000
         LR    R1,R3               PASS ADDR OF PPL TO PARSE            01560000
         LINK  EP=IKJPARS          LINK TO PARSE SERVICE ROUTINE        01570000
         LTR   R15,R15             PARSE ERROR                          01580000
         BNZ   RETC                 YES                                 01590000
         TM    PPLANS,X'FF'        PARSE ERROR                          01600000
         BO    RETC                 YES                                 01610000
         DROP  R3,R4                                                    01620000
         L     R3,WPDL             GET ADDR OF PDL                      01630000
         USING IKJPARMD,R3                                              01640000
         TM    EXTENT+1,X'01'      EXTENT SPECIFIED?                    01650000
         BZ    *+8                  NO                                  01660000
         OI    FLAG,X'20'           YES, SET FLAG BIT                   01670000
         TM    DSNLIST+6,X'80'      DSNAME SPECIFIED?                   01680000
         BO    DSNRTN                YES                                01690000
         TM    LEV+6,X'80'          INDEX LEVEL SPECIFIED?              01700000
         BO    LEVEL                 YES                                01710000
         TM    VOL+6,X'80'          VOLUME SPECIFIED?                   01720000
         BO    FREEA                 YES                                01730000
         B     EUSLO                NO PARMS - LISTS FOR USERID         01740000
         TITLE 'LISTSPC - DATASET NAME ROUTINE'                         01750000
DSNRTN   LA    R4,DSNLIST           POINT TO DSN LIST                   01760000
         LA    R0,L'MSG1                                                01770000
         LA    R1,MSG1                                                  01780000
         SVC   93                  TPUT HEADING                         01790000
DSNAME   L     R5,0(R4)             POINT TO DSNAME                     01800000
         LH    R6,4(R4)             GET LENGTH OF DSNAME                01810000
         BCTR  R6,0                                                     01820000
         MVI   DSN1,X'40'                                               01830000
         MVC   DSN1+1(43),DSN1     BLANK DSN WORK AREA                  01840000
         LA    R8,DSN1              POINT TO DSN WORK AREA              01850000
         TM    6(R4),X'40'          DSNAME CONTAINED IN QUOTES?         01860000
         BO    MVCDSNAM               YES                               01870000
         EX    R11,USIDMVC         MOVE USERID TO WORK AREA             01880000
         LA    R8,1(R11,R8)        BUMP PAST USERID                     01890000
         MVI   0(R8),C'.'          MOVE PERIOD TO WORK AREA             01900000
         LA    R8,1(0,R8)          BUMP PAST                            01910000
MVCDSNAM EX    R6,DSNMVC           MOVE DSNAME TO WORK AREA             01920000
         MVC   VOLS,BLANKS         MOVE BLANKS TO VOLSER WORK AREA      01930000
         TM    FLAG,X'80'          AUTHORIZED USER?                     01940000
         BZ    LOCDSN               NO                                  01950000
         TM    VOL+6,X'80'         VOLUME SPECIFIED?                    01960000
         BZ    LOCDSN               NO, LOCATE DSN                      01970000
         LA    R1,VOLS             POINT TO VOLSER WORK AREA            01980000
         L     R5,VOL              POINT TO VOLUME PARAMETER            01990000
         LH    R6,VOL+4            GET LENGTH OF VOL PARM               02000000
         BCTR  R6,0                                                     02010000
         EX    R6,MVCVOLS          MOVE VOL PARM TO VOLSER WORK AREA    02020000
         B     BALOBTN             BYPASS LOCATE                        02030000
LOCDSN   OI    FLAG,X'10'                                               02040000
         LOCATE INDS                                                    02050000
         LTR   R15,R15              DID IT FIND VOL SER                 02060000
         BNZ   ERR03                 NO WRITE ERROR MSG                 02070000
         NI    FLAG,X'EF'                                               02080000
         MVC   VOLS(6),BUF1+6      YES - MOVE IN VOL SER                02090000
         SPACE 2                                                        02100000
BALOBTN  BAL   R9,OBTNRTN          LINK TO OBTAIN AND PRINT ROUTINE     02110000
RETDSN   L     R4,24(R4)           GET DSN LINK POINTER                 02120000
         C     R4,ENDLIST          END OF LIST?                         02130000
         BE    RETC                 YES                                 02140000
         B     DSNAME               NO, GET NEXT DSNAME                 02150000
         TITLE 'LISTSPC - LEVEL ROUTINE'                                02160000
EUSLO    EQU   *                                                        02170000
**     FOR ENTIRE USERID LOGGED ON                                      02180000
         MVC   USERID(7),0(R10)        MOVE IN USERID                   02190000
         B     GOTUS               BRANCH TO GOT USERID                 02200000
         SPACE 3                                                        02210000
LEVEL    EQU   *                                                        02220000
         L     R5,LEV              POINT TO INDEX STRUCTURE             02230000
         LH    R6,LEV+4            GET LENGTH OF INDEX STRUCT           02240000
         BCTR  R6,0                                                     02250000
         LA    R8,USERID           POINT TO WORK AREA                   02260000
         EX    R6,DSNMVC           MOVE INDEX STRUCTURE TO WORK AREA    02270000
GOTUS    EQU   *                                                        02280000
         LA    R0,L'MSG1                                                02290000
         LA    R1,MSG1                                                  02300000
         SVC   93                  TPUT HEADING                         02310000
         OI    FLAG,X'40'          SET FLAG FOR DATA SETS FOR USERID    02320000
LOOP     EQU   *                   LOOP FOR DATASET IN USERID           02330000
         L     R15,=V(LOCINDEX)    POINT TO EP(LOCINDEX)                02340000
         LA    R1,PARM             POINT TO PARM LIST                   02350000
         BALR  R14,R15             GO TO LOCINDEX                       02360000
         B     *+4(R15)            BRANCH , DEP ON RETURN CODE          02370000
         B     OK                  0-GET INFOR FOR DSN & CONT           02380000
         B     NOUSER              4-PRINT MSG & RETRUN TO SYS          02390000
         B     NOTDISK             8-PRINT MSG&CONTINUE                 02400000
         B     MULTIVOL            12-PRINT MSG&CONTINUE                02410000
         B     RETC                16-FINISHED-RETURN TO SYS            02420000
         B     RDERR               20-PRINT MSG&RETRUN TO SUS           02430000
OK       BAL   R9,OBTNRTN          LINK TO OBTAIN AND PRINT ROUTINE     02440000
         B     LOOP                GET NEXT DSNAME FROM LOCINDEX        02450000
         SPACE 3                                                        02460000
         TITLE 'LISTSPC - VOLUME ROUTINE'                               02470000
FREEA    EQU   *                                                        02480000
** TO GET AVAILABLE SPACE ON PARTICULAR VOLUME                          02490000
         TM    FLAG,X'80'          AUTHORIZED USER?                     02500000
         BZ    ERRF                 NO                                  02510000
         LA    R0,L'MSG4                                                02520000
         LA    R1,MSG4                                                  02530000
         SVC   93                  TPUT HEADING                         02540000
         SPACE 2                                                        02550000
         LA    R4,VOL              POINT TO VOLUME LIST                 02560000
VOLLOOP  ST    R4,OBTNSAVE         SAVE ADDRESS                         02570000
         MVI   MSG2,X'40'                                               02580000
         MVC   MSG2+1(75),MSG2     BLANK MESSAGE AREA                   02590000
         XC    PEXCTR,PEXCTR       CLEAR EXTENT COUNTER                 02600000
         LA    R1,VOLS             POINT TO VOLSER WORK AREA            02610000
         L     R5,0(R4)            POINT TO VOLUME PARM                 02620000
         LH    R6,4(R4)            GET LENGTH OF VOL PARM               02630000
         BCTR  R6,0                                                     02640000
         EX    R6,MVCVOLS          MOVE VOL PARM TO VOLSER WORK AREA    02650000
         LA    R1,MSG2             POINT TO MESSAGE AREA                02660000
         EX    R6,MVCVOLS          MOVE VOLSER TO MESSAGE AREA          02670000
         MVI   DSN1,X'04'          MOVE 'DSN' FOR F4 DSCB...            02680000
         MVC   DSN1+1(43),DSN1     ...INTO DSN1                         02690000
         OBTAIN SERCHCAM                                                02700000
         CLI   BUF1,X'F4'          DID IT READ OK?                      02710000
         BNE   ERRF4               NO                                   02720000
         MVC   VOLTKCYL(2),BUF1+20 GET NUM OF TRKS/CYL                  02730000
         MVC   VOLDSCTK+1(1),BUF1+30   GET NUM OF DSCBS PER TRK         02740000
         MVC   VOLF4CHR(4),BUF1+63 GET CCHH OF START OF VTOC            02750000
         MVI   VOLF4CHR+4,X'01'    F4 DSCB IS FIRST REC                 02760000
         LH    RCC,VOLF4CHR        SETUP...                             02770000
         LH    RHH,VOLF4CHR+2         DISK...                           02780000
         SR    RR,RR                     ADDRESSING...                  02790000
         IC    RR,VOLF4CHR+4                                            02800000
         MVC   WXTNT(4),BUF1+63    MOVE BEGIN XTNT TO WORK AREA         02810000
         UNPK  MSG2+29(9),WXTNT(5) UNPACK INTO MESSAGE AREA             02820000
         TR    MSG2+29(8),TRTBL    TRANSLATE EXTENT                     02830000
         MVC   WXTNT(4),BUF1+67    MOVE END XTNT TO WORK AREA           02840000
         UNPK  MSG2+38(9),WXTNT(5) UNPACK INTO MESSAGE AREA             02850000
         TR    MSG2+38(8),TRTBL    TRANSLATE EXTENT                     02860000
         SR    R9,R9                                                    02870000
         LH    R9,BUF1+6             MOVE IN NUMBER FREE DSCB           02880000
         CVD   R9,WKD                                                   02890000
         MVC   MSG2+6(6),MASKED        MOVE IN MASK                     02900000
         ED    MSG2+6(6),WKD+5         EDIT FREE DSCB                   02910000
         LH    R1,VOLNO            GET NUM VOLS PROCESSED               02920000
         LA    R1,1(R1)            BUMP                                 02930000
         STH   R1,VOLNO            REPLACE                              02940000
         LA    RR,1(RR)            BUMP RECORD PTR                      02950000
         CH    RR,VOLDSCTK         TEST FOR LAST REC ON TRK             02960000
         BNH   VBSET               GO SET VTOCCHHR                      02970000
         LA    RR,1                RESET RR TO 1                        02980000
         LA    RHH,1(RHH)          BUMP TRK COUNTER                     02990000
         CH    RHH,VOLTKCYL        PAST END OF CYL?                     03000000
         BL    VBSET               GO SET VTOCCHHR                      03010000
         SR    RHH,RHH             ELSE SET RHH TO TRK 0                03020000
         LA    RCC,1(RCC)          BUMP TO NEXT CYL                     03030000
VBSET    STH   RCC,VTOCCHHR        SET...                               03040000
         STH   RHH,VTOCCHHR+2         UP...                             03050000
         STC   RR,VTOCCHHR+4            VTOCCHHR                        03060000
         SR    R4,R4               ZERO COUNTER                         03070000
         SPACE 2                                                        03080000
PFOBT    OBTAIN SEEKCAM                                                 03090000
         LA    R2,1                INDICATE FIRST EXTENT                03100000
         LA    R3,BUF1+4           POINT TO FIRST EXTENT                03110000
PFLOOP   CLC   0(5,R3),=D'0'       LAST EXTENT?                         03120000
         BE    PFGOT               EXIT IF SO                           03130000
         MVC   HWK1(2),2(R3)       MOVE NUM CYLS TO HWD                 03140000
         LH    R1,HWK1             LOAD                                 03150000
         MH    R1,VOLTKCYL         CONVERT TO TRKS                      03160000
         SR    R0,R0               ZERO R0                              03170000
         IC    R0,4(R3)            LOAD NUM TRKS                        03180000
         AR    R1,R0               ADD TRKS                             03190000
         AR    R4,R1               ACCUMULATE                           03200000
         LA    R2,1(R2)            BUMP CTR                             03210000
         CH    R2,=H'9'            NINTH EXTENT?                        03220000
         BE    PFXT9               BRANCH IF SO                         03230000
         CH    R2,=H'27'           27TH EXTNET?                         03240000
         BE    PFXT27              BRANCH IF SO                         03250000
         LA    R3,5(R3)            BUMP NORMALLY                        03260000
         B     PFLOOP              AND CONTINUE                         03270000
PFXT9    LA    R3,6(R3)            SKIP OVER F5 IF AT DSCH+44           03280000
         B     PFLOOP              AND CONTINUE                         03290000
PFXT27   AH    R2,PEXCTR           ACCUM EXT TOTALS                     03300000
         BCTR  R2,0                                                     03310000
         STH   R2,PEXCTR           SAVE EXT TOTALS                      03320000
         CLC   BUF1+135(5),=D'0'   PTR TO NEXT F5?                      03330000
         BE    PFGOT               DONE. NO MORE F5                     03340000
         MVC   VTOCCHHR(5),BUF1+135                                     03350000
         B     PFOBT               GO READ NEXT F5                      03360000
PFGOT    EQU   *                                                        03370000
         AH    R2,PEXCTR           ACCUM EXTENT TOTALS                  03380000
         CVD   R2,WKD              CONVERT NUM EXTENTS                  03390000
         MVC   MSG2+14(4),MASKED     MOVE IN MASK                       03400000
         ED    MSG2+14(4),WKD+6      EDIT NUM EXTENTS                   03410000
         CVD   R4,WKD              CONVERT NUM TRKS FREE                03420000
         MVC   MSG2+21(6),MASKED     MOVE IN MASK                       03430000
         ED    MSG2+21(6),WKD+5      EDIT FREE TRKS                     03440000
         TPUT  MSG2,76             WRITE FREE INFO                      03450000
VOLRET   L     R4,OBTNSAVE         LOAD POINTER TO VOLUME LIST          03460000
         L     R4,8(R4)            GET LINK POINTER                     03470000
         C     R4,ENDLIST          END OF LIST                          03480000
         BE    RETC                 YES                                 03490000
         B     VOLLOOP              NO, GET NEXT VOLUME                 03500000
         TITLE 'LISTSPC - OBTAIN AND PRINT ROUTINES'                    03510000
*   READ DSCB1 AND GET SPACE                                            03520000
OBTNRTN  STM   R2,R11,OBTNSAVE     SAVE CALLER'S REGISTERS              03530000
         MVC   DSNSAVE(44),DSN1                                         03540000
         MVI   DSN1,X'04'          MOVE 'DSN' FOR F4 DSCB...            03550000
         MVC   DSN1+1(43),DSN1     ...INTO DSN1                         03560000
         OBTAIN SERCHCAM                                                03570000
         CLI   BUF1,X'F4'          DID IT READ OK?                      03580000
         BNE   ERRF4               NO                                   03590000
         MVC   VOLTKCYL(2),BUF1+20 GET NUM OF TRKS/CYL                  03600000
         MVC   DSN1(44),DSNSAVE                                         03610000
         MVI   MSG2,X'40'                                               03620000
         MVC   MSG2+1(75),MSG2     BLANK MESSAGE AREA                   03630000
         OBTAIN SERCHCAM                                                03640000
         LTR   R15,R15             DID OBTAIN WORK                      03650000
         BNZ   ERR03               NO - WRITE ERROR MSG                 03660000
         SPACE 2                                                        03670000
         CLI   BUF1,C'1'            IS IT F1 DSCB?                      03680000
         BNE   ERR01                NOT F1                              03690000
         LA    R9,XTNTS            POINT TO EXTENTS SAVE AREA           03700000
         LH    R6,BUF1+54          GET LAST RELATIVE TRK USED           03710000
         CLI   BUF1+56,X'00'       IS TRK UNUSED?                       03720000
         BE    *+8                  YES                                 03730000
         LA    R6,1(R6)             NO, ANOTHER TRACK                   03740000
         STH   R6,LASTTRK          SAVE LAST REL. TRK                   03750000
         MVC   DSORG,BUF1+38       SAVE DSORG                           03760000
AROUND   EQU   *                                                        03770000
* TEST FOR TYPE OF DSORG AND MOVE TO PRINT LINE                         03780000
         CLI   BUF1+38,X'80'        IS IT IS                            03790000
         BNE   NIS                  NO                                  03800000
         MVC   MSG2+2(2),=C'IS'     YES - MOVE IN IS                    03810000
         B     GDSO                 BRANCH TO GOT DSORG                 03820000
NIS      CLI   BUF1+38,X'40'        IS IT PS                            03830000
         BNE   NPS                   NO                                 03840000
         MVC   MSG2+2(2),=C'PS'      YES-MOVE IN PS                     03850000
         B     GDSO                 BRANCH TO GOT DSORG                 03860000
NPS      CLI   BUF1+38,X'20'        IS IT DA                            03870000
         BNE   NDA                   NO                                 03880000
         MVC   MSG2+2(2),=C'DA'      YES - MOVE IN DA                   03890000
         B     GDSO                 BRANCH TO GOT DSORG                 03900000
NDA      CLI   BUF1+38,X'02'        IS IT PO                            03910000
         BNE   NPO                   NO                                 03920000
         MVC   MSG2+2(2),=C'PO'      YES - MOVE IN PO                   03930000
         B     GDSO                 BRANCH TO DSORG                     03940000
NPO      CLI   BUF1+38,X'01'        IS IT UNMOVEABLE                    03950000
         MVC   MSG2+2(2),=C' U'      YES                                03960000
GDSO     SR    R2,R2                ZERO REG 2                          03970000
         IC    R2,BUF1+15           PICK UP NUMBER OF EXTENTS           03980000
         LA    R3,1                 SET EXTENT COUNTER                  03990000
         SR    R4,R4                CLEAR R4 TO TRK ACCUMULATION        04000000
         LTR   R2,R2               NO XTNTS? - GDG PATTERN. DSCB        04010000
         BZ    VVALID               YES, BYPASS ACCUMULATE              04020000
         LA    R5,BUF1+61           POINT TO FIRST EXTENT               04030000
VXTLOOP  MVC   0(10,R9),0(R5)      MOVE EXTENT TO SAVE AREA             04040000
         LA    R9,10(R9)           BUMP TO NEXT SAVE AREA               04050000
         MVC   HWK1(2),6(R5)        MOVE HI-CYL TO HWD                  04060000
         LH    R0,HWK1              LOAD                                04070000
         MVC   HWK1(2),8(R5)        MOVE HI-TRK TO HWD                  04080000
         LH    R1,HWK1              LOAD                                04090000
         MVC   HWK1(2),2(R5)        MOVE LOW-CYL TO HWD                 04100000
         SH    R0,HWK1              SUBTRACT                            04110000
         MVC   HWK1(2),4(R5)        MOVE LOW-TRK TO HWD                 04120000
         SH    R1,HWK1              SUBTRACT                            04130000
         MH    R0,VOLTKCYL          CONVERT CYL TO TRK                  04140000
         AR    R1,R0                GET TOTAL MINUS1                    04150000
         LA    R4,1(R1,R4)          GET TOTAL AND ACCUMULATE            04160000
         CR    R3,R2                DONE LAST EXTENT?                   04170000
         BE    VVALID               GO TO VVALID IF SO                  04180000
         LA    R3,1(R3)             BUMP TO EXTENT COUNTER              04190000
         CH    R3,=H'4'             FOURTH EXTENT?                      04200000
         BE    VXT4                   BRANCH IF SO                      04210000
         CH    R3,=H'8'             EIGHTTH EXTENT?                     04220000
         BE   VXT8                                                      04230000
         LA    R5,10(R5)            ELSE BUMP EXTENT POINTER            04240000
         B     VXTLOOP              AND GO TO NEXT EXTENT               04250000
VXT4     MVC   VTOCCHHR(5),BUF1+91  POINT NEXT DSCH(F2 OR F3)           04260000
VXT4OBT  OBTAIN SEEKCAM                                                 04270000
         CLI   BUF1+44,C'3'         IF IT F3 DSCB                       04280000
         BE    VXT4F3               BRANCH IF SO                        04290000
         MVC   VTOCCHHR(5),BUF1+135  ELSE ITS F2-POINT TO F3            04300000
         B     VXT4OBT                                                  04310000
VXT4F3   LA    R5,BUF1+4            POINT TO FIRST EXTENT IN F3         04320000
         B     VXTLOOP              CONTINUE LOOP FOR SIZE              04330000
VXT8     LA    R5,BUF1+45           SKIP OVER F3 ID IN F3 DSCB          04340000
         B     VXTLOOP              CONTINUE LOOP FOR SIZE              04350000
VVALID   EQU   *                                                        04360000
* CONVERT SPACE ALLOC AND EDIT IN PRINT LINE                            04370000
         CVD   R4,WKD               CONVERT DEC. R4  #TRKS ALLOC        04380000
         MVC   MSG2+5(6),MASKED      MOVE MASKED FIELD IN               04390000
         ED    MSG2+5(6),WKD+5       EDIT  # TRKS MSG2+5                04400000
         TM    DSORG,X'42'        IS IT PO OR PS                        04410000
         BNZ   CVDU                  YES - BRANCH                       04420000
         MVI   MSG2+17,C'*'                                             04430000
         B     CVDX                 BRANCH TO EXTENT CONVERSION         04440000
CVDU     LH    R6,LASTTRK            GET LAST RELATIVE TRACK            04450000
         LR    R7,R4                                                    04460000
         SR    R7,R6                 R7 CONTAINS UNUSED SPACE           04470000
         CVD   R7,WKD               CONVERT DEC R7                      04480000
         MVC   MSG2+12(6),MASKED      MOVE IN MASKED FIELD              04490000
         ED    MSG2+12(6),WKD+5       EDIT # TRKD UNUSED                04500000
CVDX     CVD   R2,WKD                   PICK UP # EXTENTS               04510000
         MVC   MSG2+20(4),MASKED      MOVE IN MASKED FIELD              04520000
         ED    MSG2+20(4),WKD+6      EDIT # EXTENTS USED                04530000
         MVC   MSG2+27(44),DSN1     MOVE DSN TO PRINT LINE              04540000
         TPUT  MSG2,76             WRITE OUT LINE                       04550000
         TM    FLAG,X'A0'          AUTHORIZED USER?                     04560000
         BNO   OBTNRET              NO                                  04570000
         LA    R9,XTNTS            POINT TO EXTENT SAVE AREA            04580000
         MVC   MSG2(6),VOLS        MOVE VOLSER TO MESSAGE AREA          04590000
OUTLOOP  LA    R3,3                SET LOOP CONTROL                     04600000
         LA    R4,MSG2+8           POINT TO MESSAGE AREA                04610000
         MVI   MSG2+6,X'40'                                             04620000
         MVC   MSG2+7(69),MSG2+6   BLANK MESSAGE AREA                   04630000
INLOOP   MVC   WXTNT+3(1),1(R9)    MOVE EXTENT # TO WORK AREA           04640000
         UNPK  0(2,R4),WXTNT+3(2)  UNPACK EXTENT #                      04650000
         TR    0(1,R4),TRTBL       TRANSLATE EXTENT #                   04660000
         MVC   WXTNT(4),2(R9)      MOVE BEGIN XTNT TO WORK AREA         04670000
         UNPK  2(9,R4),WXTNT(5)    UNPACK XTNT                          04680000
         TR    2(8,R4),TRTBL       TRANSLATE EXTENT                     04690000
         MVC   WXTNT(4),6(R9)      MOVE END XTNT TO WORK AREA           04700000
         UNPK  11(9,R4),WXTNT(5)   UNPACK XTNT                          04710000
         TR    11(8,R4),TRTBL      TRANSLATE EXTENT                     04720000
         LA    R9,10(R9)           BUMP TO NEXT EXTENT                  04730000
         LA    R4,22(R4)           BUMP MESSAGE AREA POINTER            04740000
         BCTR  R2,0                DECREASE # EXTENTS                   04750000
         LTR   R2,R2               ANY MORE EXTENTS                     04760000
         BZ    *+8                  NO                                  04770000
         BCT   R3,INLOOP           CONVERT NEXT EXTENT                  04780000
         LA    R0,L'MSG2                                                04790000
         LA    R1,MSG2                                                  04800000
         SVC   93                  TPUT EXTENTS                         04810000
         MVC   MSG2(6),BLANKS      BLANK VOLSER                         04820000
         LTR   R2,R2               ANY MORE EXTENTS?                    04830000
         BNZ   OUTLOOP              YES                                 04840000
OBTNRET  LM    R2,R11,OBTNSAVE     RELOAD CALLER'S REGISTERS            04850000
         BR    R9                  RETURN TO CALLER                     04860000
         TITLE 'LISTSPC - EOJ AND ERROR ROUTINES'                       04870000
RETC     EQU   *                                                        04880000
         L     13,SAVE+4           LOAD R13 PREVIOUS SPACE AREA         04890000
         LM    2,12,28(13)         RELOAD REGISTERS                     04900000
         L     14,12(13)           LOAD RETURN ADDRESS                  04910000
         MVI   12(13),X'FF'        INDICATE CONTROL RETURN CALLING PROG 04920000
         BCR   15,14               RETURN TO CALLING PROGRAM            04930000
         SPACE 3                                                        04940000
ERR01    EQU   *                                                        04950000
* NO VALID COMMAND                                                      04960000
         TPUT  ERMSG1,18                                                04970000
         B     RETC                BRANCH TO RETURN CODE                04980000
* DATASET NAME NOT FOUND                                                04990000
ERR03    MVC   MSG2(L'ERMSG9),ERMSG9                                    05000000
         MVC   MSG2+L'ERMSG9(44),DSN1                                   05010000
         LA    R0,L'ERMSG9+44                                           05020000
         LA    R1,MSG2                                                  05030000
         SVC   93                                                       05040000
         TM    FLAG,X'40'                                               05050000
         BO    LOOP                                                     05060000
         TM    FLAG,X'10'                                               05070000
         BO    RETDSN                                                   05080000
         B     OBTNRET                                                  05090000
         SPACE 3                                                        05100000
RDERR    MVC   MSG2(L'ERMSG8),ERMSG8                                    05110000
         MVC   MSG2+L'ERMSG8(44),DSN1                                   05120000
         LA    R0,L'ERMSG8+44                                           05130000
         LA    R1,MSG2                                                  05140000
         SVC   93                                                       05150000
         B     LOOP                CONTINUE THRU LOOP                   05160000
         SPACE 3                                                        05170000
NOUSER   MVC   MSG2(L'ERMSG3),ERMSG3                                    05180000
         MVC   MSG2+L'ERMSG3(44),USERID                                 05190000
         LA    R0,L'ERMSG3+44                                           05200000
         LA    R1,MSG2                                                  05210000
         SVC   93                                                       05220000
         B     RETC                RETURN TO SYSTEM                     05230000
         SPACE 3                                                        05240000
NOTDISK  MVC   MSG2(L'ERMSG6),ERMSG6                                    05250000
         MVC   MSG2+L'ERMSG6(44),DSN1                                   05260000
         LA    R0,L'ERMSG6+44                                           05270000
         LA    R1,MSG2                                                  05280000
         SVC   93                                                       05290000
         B     LOOP                PICK UP NEXT DATASET                 05300000
         SPACE 3                                                        05310000
MULTIVOL MVC   MSG2(L'ERMSG7),ERMSG7                                    05320000
         MVC   MSG2+L'ERMSG7(44),DSN1                                   05330000
         LA    R0,L'ERMSG7+44                                           05340000
         LA    R1,MSG2                                                  05350000
         SVC   93                                                       05360000
         B     LOOP                CONTINUE WITH NEXT DSN               05370000
         SPACE 3                                                        05380000
ERRF     EQU   *                                                        05390000
         TPUT  ERMSG10,20                                               05400000
         B     RETC                                                     05410000
ERRF4    EQU   *                   ERROR READING F4                     05420000
         TPUT  ERMSG11,21                                               05430000
         B     VOLRET                                                   05440000
USIDMVC  MVC   0(0,R8),0(R10)                                           05450000
DSNMVC   MVC   0(0,R8),0(R5)                                            05460000
MVCVOLS  MVC   0(0,R1),0(R5)                                            05470000
         TITLE 'LISTSPC - DATA AREAS'                                   05480000
ENDLIST  DS    0F                                                       05490000
         DC    XL4'FF000000'                                            05500000
* SETUP CONSTANTS                                                       05510000
VOLDSCTK DC    H'0'                NUM DSCH ON A TRK                    05520000
VOLF4CHR DC    XL5'00'             CCHHR OF DSCB                        05530000
VOLNO    DC    H'0'                NUM VOLUMES PROCESSED                05540000
PEXCTR   DC    H'0'                CTR FOR TATAL NUM FREE SPACE         05550000
LASTTRK  DC    H'0'                                                     05560000
DSN1     DC    CL44' '              DATASET NAME                        05570000
DSNSAVE  DC    CL44' '                                                  05580000
VOLS     DC    CL6' '                 VOLUME SER                        05590000
USERID   DC    CL44' '              USERID                              05600000
FLAG     DC    X'00'                                                    05610000
DSORG    DC    X'00'                                                    05620000
BLANKS   DC    CL6' '                                                   05630000
BUF1     DS    0D                                                       05640000
         DS    265C                                                     05650000
TRTBL    EQU   *-240                                                    05660000
         DC    C'0123456789ABCDEF'                                      05670000
MASKED   DC    XL6'402020202020'   MASK FIELD FOR EDIT                  05680000
         SPACE 3                                                        05690000
* MESSAGE TO BE PRINTED                                                 05700000
MSG1     DC    C'DSORG ALLOC UNUSED EXTENTS DSNAME'                     05710000
MSG2     DC    CL76' '                                                  05720000
MSG4     DC    C'VOLUME DSCBS EXTENTS TRACKS  VTOC'                     05730000
         SPACE 3                                                        05740000
* ERROR MESSAGES                                                        05750000
ERMSG1   DC    C'NO VALID COMMAND'                                      05760000
ERMSG3   DC    C'INVALID LEVEL - '                                      05770000
ERMSG6   DC    C'DSN NOT ON DISK - '                                    05780000
ERMSG7   DC    C'DSN ON MULTIVOLUMES - '                                05790000
ERMSG8   DC    C'ERROR READING CATALOG DSN - '                          05800000
ERMSG9   DC    C'DSN NOT FOUND - '                                      05810000
ERMSG10  DC    C'UNSUPPORTED KEYWORD'                                   05820000
ERMSG11  DC    C'ERROR READING F4 DSCB'                                 05830000
         EJECT                                                          05840000
* CAMLST FOR DSCB3                                                      05850000
SEEKCAM  CAMLST SEEK,VTOCCHHR,VOLS,BUF1                                 05860000
         SPACE 2                                                        05870000
* CAMLST LOCATE DSN OV VOL SER                                          05880000
INDS     CAMLST NAME,DSN1,,BUF1                                         05890000
         SPACE 2                                                        05900000
* CAMLST FOR DSCB 1                                                     05910000
SERCHCAM CAMLST SEARCH,DSN1,VOLS,BUF1                                   05920000
         EJECT                                                          05930000
* CONSTANTS AND WORK AREAS                                              05940000
HWK1     DC    H'0'                HALF WORD WORK AREA                  05950000
VOLTKCYL DC    H'19'               # TRKS/CYL                           05960000
VTOCCHHR DC    XL5'0'              TRACK ADDR WORK AREA                 05970000
WKD      DC    D'0'                DOUBLE WORK WORK AREA                05980000
         SPACE 3                                                        05990000
* PARM SETUP FOR GETDSN                                                 06000000
PARM     DC    A(USERID)           USERID ADDRESS                       06010000
         DC    A(DSN1)             DSN ADDRESS                          06020000
         DC    A(VOLS)             VOL SER ADDRESS                      06030000
         DC    A(0)                ADDRESS FOR GETMAIN                  06040000
         SPACE 3                                                        06050000
* EQUATE REGISTERS                                                      06060000
         EQUATE                                                         06070000
RCC      EQU   R8                                                       06080000
RHH      EQU   R7                                                       06090000
RR       EQU   R6                                                       06100000
         TITLE 'LISTSPC - PARSE PARAMETER LIST'                         06110000
PARSLIST IKJPARM                                                        06120000
DSNKYD   IKJKEYWD                                                       06130000
         IKJNAME 'DATASET',SUBFLD=DSNSUBF                               06140000
LEVL     IKJKEYWD                                                       06150000
         IKJNAME 'LEVEL',SUBFLD=LVL                                     06160000
VOLUME   IKJKEYWD                                                       06170000
         IKJNAME 'VOLUME',SUBFLD=VLM                                    06180000
EXTENT   IKJKEYWD                                                       06190000
         IKJNAME 'EXTENTS'                                              06200000
DSNSUBF  IKJSUBF                                                        06210000
DSNLIST  IKJPOSIT DSNAME,LIST                                           06220000
LVL      IKJSUBF                                                        06230000
LEV      IKJIDENT 'LEVEL',OTHER=ANY,MAXLNTH=44                          06240000
VLM      IKJSUBF                                                        06250000
VOL      IKJIDENT 'VOLUME',LIST,FIRST=ALPHANUM,OTHER=ALPHANUM,MAXLNTH=6 06260000
         IKJENDP                                                        06270000
         IKJPPL                                                         06280000
         IKJCPPL                                                        06290000
         TITLE 'LISTSPC - WORK AREA'                                    06300000
WORKAREA DSECT                                                          06310000
WPPL     DS    7A                                                       06320000
CPECB    DS    F                                                        06330000
WPDL     DS    F                                                        06340000
OBTNSAVE DS    10F                                                      06350000
XTNTS    DS    16XL10                                                   06360000
WXTNT    DS    CL5                                                      06370000
         DS    0D                                                       06380000
LOCINDXW EQU   *                                                        06390000
LWORK    EQU   *-WORKAREA                                               06400000
         END                                                            06410000
/*
//*----------------------------------------------------------- ASM
//*
//ASM2.SYSIN DD *                                                       00410000
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
/*
//*----------------------------------------------------------- ASM2
//*                                                                     00430000
//LKED.SYSIN DD *                                                       00460000
  NAME LISTSPC(R)                                                       00480000
/*
//*----------------------------------------------------------- LKED
//                                                                      00880000
