//CATLG  JOB (TSO),
//             'Install CATLG',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
//*  CATLG, UNCATLG AND SCRATCH -                                       00030000
//*    THESE TSO COMMANDS DO THE SAME THING THAT THE                    00040000
//*    CORRESPONDING IEHPROGM CONTROL STATEMENTS DO.                    00050000
//*    YOU CAN CATALOG AND UNCATALOG DATASETS TO                        00060000
//*    ARBITRARY VOLUMES AND UNITS, AND SCRATCH DATASETS                00070000
//*    ON DIRECT ACCESS VOLUMES WITHOUT REGARD TO THEIR                 00080000
//*    CATALOG STATUS.                                                  00090000
//*                                                                     00100000
//*  INSTALLATION -                                                     00110000
//*    MODIFY THE JOB CARD ABOVE AND THE PROCEDURE DEFAULTS             00120000
//*    BELOW TO SUIT YOUR TASTE.  IF YOU WANT TO CHANGE THE             00130000
//*    COMMAND NAME AND ALIASES YOU WILL HAVE TO DOCTOR THE             00140000
//*    JOBSTREAM.                                                       00150000
//*                                                                     00160000
//INSTALL PROC SOUT='*',               <=== SYSOUT CLASS                00170000
//             LIB='SYS2.CMDLIB',      <=== TARGET LOAD LIBRARY         00180000
//             HELP='SYS2.HELP',       <=== HELP LIBRARY                00190000
//             SYSTS=SYSDA,            <=== UNITNAME FOR WORK DATASETS  00200000
//             ASMBLR=IEUASM,          <=== NAME OF YOUR ASSEMBLER      00210000
//             ALIB='SYS1.LINKLIB',    <=== LOCATION OF YOUR ASSEMBLER  00220000
//             SMPMTS='SYS1.SMPMTS',   <=== SMPMTS DATASET NAME         00230000
//             MACLIB='SYS1.MACLIB',   <=== MACLIB DATASET NAME         00240000
//             AMODGEN='SYS1.AMODGEN', <=== AMODGEN DATASET NAME        00250000
//             MODGEN='SYS1.SMPMTS'    <=== MODGEN DATASET NAME         00260000
//*                                         USE SYS1.SMPMTS FOR MVS-370 00270000
//*                                                                     00280000
//IEBUPDTE EXEC PGM=IEBUPDTE,PARM=NEW                                   00290000
//SYSPRINT DD  SYSOUT=&SOUT                                             00300000
//SYSUT1   DD  DSN=&HELP,DISP=SHR                                       00310000
//SYSUT2   DD  DSN=&HELP,DISP=SHR                                       00320000
//*                                                                     00330000
//ASM     EXEC PGM=&ASMBLR,REGION=2048K,PARM='NOOBJECT,DECK,NOALIGN'    00340000
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00350000
//SYSTERM  DD  SYSOUT=&SOUT                                             00360000
//SYSPRINT DD  SYSOUT=&SOUT                                             00370000
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00380000
//         DD  DSN=&MODGEN,DISP=SHR                                     00390000
//         DD  DSN=&SMPMTS,DISP=SHR                                     00400000
//         DD  DSN=&AMODGEN,DISP=SHR                                    00410000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00420000
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00430000
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00440000
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(,PASS,DELETE),            00450000
//             SPACE=(TRK,(5,1),RLSE)                                   00460000
//*                                                                     00470000
//LKED    EXEC PGM=HEWL,COND=(0,NE),PARM='LIST,MAP,XREF'                00480000
//SYSPRINT DD  SYSOUT=&SOUT                                             00490000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,5)                                00500000
//SYSLMOD  DD  DSN=&LIB,DISP=SHR                                        00510000
//SYSLIN   DD  DSN=&&SYSLIN,DISP=(OLD,DELETE)                           00520000
//         DD  DDNAME=SYSIN                                             00530000
//        PEND                                                          00540000
//*                                                                     00550000
//        EXEC INSTALL                                                  00560000
//IEBUPDTE.SYSIN DD *                                                   00570000
./         ADD    NAME=CATLG,LIST=ALL                                   00580000
./         NUMBER NEW1=1000,INCR=1000                                   00590000
)F FUNCTION -                                                           00600000
             THIS PROGRAM PERFORMS CATLG FUNCTIONS SIMILAR TO IEHPROGM. 00610000
                                                                        00620000
  AUTHOR -                                                              00630000
             DAVE PHILLIPS                                              00640000
             A. DUDA AND SONS, INC.                                     00650000
             P.O. BOX 257                                               00660000
             OVIEDO, FL  32765                                          00670000
                                                                        00680000
)X SYNTAX -                                                             00690000
      CATLG  dsname                     {REQUIRED}                      00700000
             VOL(volser)                {REQUIRED}                      00710000
             UNIT(unitname)             {OPTIONAL}                      00720000
                                                                        00730000
)O OPERANDS -                                                           00740000
))DSNAME                                                                00750000
           - REQUIRED.  DATASET NAME TO BE CATALOGED.  NORMAL TSO       00760000
             DATASET NAMING CONVENTIONS APPLY.                          00770000
))VOL(volser)                                                           00780000
           - REQUIRED.  VOLUME SERIAL YOU WANT THE DATASET CATALOGED    00790000
             ON.                                                        00800000
))UNIT(unitname)                                                        00810000
           - OPTIONAL.  IF THE VOLUME SERIAL IS THAT OF A CURRENTLY     00820000
             MOUNTED DIRECT-ACCESS VOLUME, THE DEFAULT VALUE IS         00830000
             TAKEN FROM THE VOLUME'S UCB.  OTHERWISE THE OPERAND        00840000
             IS REQUIRED.                                               00850000
./         ADD    NAME=SCRATCH,LIST=ALL                                 00860000
./         NUMBER NEW1=1000,INCR=1000                                   00870000
./         ALIAS  NAME=SCR                                              00880000
)F FUNCTION -                                                           00890000
             THIS PROGRAM PERFORMS SCRATCH FUNCTIONS SIMILAR TO         00900000
             IEHPROGM.  THE CATALOG IS NOT AFFECTED.                    00910000
                                                                        00920000
  AUTHOR -                                                              00930000
             DAVE PHILLIPS                                              00940000
             A. DUDA AND SONS, INC.                                     00950000
             P.O. BOX 257                                               00960000
             OVIEDO, FL  32765                                          00970000
                                                                        00980000
)X SYNTAX -                                                             00990000
      SCRATCH dsname                     {REQUIRED}                     01000000
              VOL(volser)                {OPTIONAL}                     01010000
                                                                        01020000
   ALIAS - SCR                                                          01030000
                                                                        01040000
)O OPERANDS -                                                           01050000
))DSNAME                                                                01060000
           - REQUIRED.  DATASET NAME TO BE SCRATCHED.  NORMAL TSO       01070000
             DATASET NAMING CONVENTIONS APPLY.                          01080000
))VOL(volser)                                                           01090000
           - OPTIONAL.  VOLUME SERIAL YOU WANT THE DATASET SCRATCHED    01100000
             FROM.  IF NOT SPECIFIED, THE dataset MUST BE CATALOGED,    01110000
             AND THE VOLUME SERIAL IS TAKEN FROM THE CATALOG ENTRY.     01120000
./         ADD    NAME=UNCATLG,LIST=ALL                                 01130000
./         NUMBER NEW1=1000,INCR=1000                                   01140000
./         ALIAS  NAME=UNCAT                                            01150000
)F FUNCTION -                                                           01160000
             THIS PROGRAM PERFORMS UNCATLG FUNCTIONS SIMILAR TO         01170000
             IEHPROGM.  THE TARGET DATASET IS NOT SCRATCHED.            01180000
                                                                        01190000
  AUTHOR -                                                              01200000
             DAVE PHILLIPS                                              01210000
             A. DUDA AND SONS, INC.                                     01220000
             P.O. BOX 257                                               01230000
             OVIEDO, FL  32765                                          01240000
                                                                        01250000
)X SYNTAX -                                                             01260000
      UNCATLG dsname                     {REQUIRED}                     01270000
                                                                        01280000
   ALIAS - UNCAT                                                        01290000
                                                                        01300000
)O OPERANDS -                                                           01310000
))DSNAME                                                                01320000
           - REQUIRED.  DATASET NAME TO BE SCRATCHED.  NORMAL TSO       01330000
             DATASET NAMING CONVENTIONS APPLY.                          01340000
./        ENDUP                                                         01350000
/*                                                                      01360000
//ASM.SYSIN DD *                                                        01370000
CATLG    TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          HOUSEKEEPING AN*01380000
               D PARSE COMMAND BUFFER'                                  01390000
*                                                                       01400000
* FUNCTION -                                                            01410000
*            THIS PROGRAM PERFORMS CATLG,UNCATLG AND SCRATCH FUNCTIONS  01420000
*            SIMILAR TO IEHPROGM.                                       01430000
*                                                                       01440000
* AUTHOR -                                                              01450000
*            DAVE PHILLIPS                                              01460000
*              A. DUDA AND SONS, INC.                                   01470000
*              P.O. BOX 257                                             01480000
*              OVIEDO, FL  32765                                        01490000
*                                                                       01500000
* CHANGES OCTOBER 1990 (DAVID ANDREWS) -                                01510000
*            THE UCB LOOKUP TABLE WENT AWAY IN MVS/XA.  ADDED SOME      01520000
*            CODE TO USE THE IOSVSUCB UCB SCAN ROUTINE INSTEAD.         01530000
*                                                                       01540000
* CHANGES NOVEMBER 1990 (DAVID ANDREWS) -                               01550000
*            OOPS.  DEVICE NAME TABLE WENT AWAY TOO.  ADDED CODE        01560000
*            TO USE XA UNIT VERIFICATION SERVICES.  WHATTA PAIN.        01570000
*                                                                       01580000
         SPACE                                                          01590000
         ENTRY UNCATLG                                                  01600000
         ENTRY UNCAT                                                    01610000
         ENTRY SCRATCH                                                  01620000
         ENTRY SCR                                                      01630000
         SPACE                                                          01640000
CATLG    CSECT                                                          01650000
         SPACE                                                          01660000
         SAVE  (14,12),,CATLG-&SYSDATE-&SYSTIME                         01670000
         SPACE                                                          01680000
         LA    R12,START-CATLG(,R15)      LOAD BASE REGISTER            01690000
         USING START,R12                                                01700000
         LA    R2,EPCAT                   REMEMBER ENTRY POINT          01710000
         L     R3,=V(CATPARS)             -> PARSE TABLE                01720000
         BR    R12                        TO COMMON CODE                01730000
         SPACE 2                                                        01740000
UNCATLG  SAVE  (14,12),,*                                               01750000
         SPACE                                                          01760000
         LA    R12,START-UNCATLG(,R15)    LOAD BASE REGISTER            01770000
         LA    R2,EPUCAT                  REMEMBER ENTRY POINT          01780000
         L     R3,=V(UCATPARS)            -> PARSE TABLE                01790000
         BR    R12                        TO COMMON CODE                01800000
UNCAT    EQU   UNCATLG                                                  01810000
         SPACE 2                                                        01820000
SCRATCH  SAVE  (14,12),,*                                               01830000
         SPACE                                                          01840000
         LA    R12,START-SCRATCH(,R15)    LOAD BASE REGISTER            01850000
         LA    R2,EPSCR                   REMEMBER ENTRY POINT          01860000
         L     R3,=V(SCRPARS)             -> PARSE TABLE                01870000
SCR      EQU   SCRATCH                                                  01880000
         EJECT                                                          01890000
         SPACE                                                          01900000
START    LR    R6,R1              SAVE PARM                             01910000
         USING CPPL,R6                                                  01920000
         LA    R0,CORESIZE                                              01930000
         GETMAIN R,LV=(0)         GET A WORK AREA                       01940000
         ST    R13,4(,R1)         )                                     01950000
         ST    R1,8(,R13)         ) CHAIN SAVE AREAS                    01960000
         LR    R13,R1             )                                     01970000
         USING CORE,R13                                                 01980000
         SPACE 2                                                        01990000
* BUILD ALL SERVICE ROUTINE PARAMETER BLOCKS & LISTS                    02000000
         SPACE                                                          02010000
         STC   R2,FLAGS           SET EP TYPE                           02020000
         LA    R5,PPLSECT         -> PPL                                02030000
         USING PPL,R5                                                   02040000
         ST    R3,PPLPCL          -> PARSE PCL                          02050000
         L     R1,CPPLUPT         -> UPT                                02060000
         L     R2,CPPLECT         -> ECT                                02070000
         SR    R3,R3                                                    02080000
         ST    R3,ECB             ZERO ECB                              02090000
         LA    R3,ECB             -> ECB                                02100000
         STM   R1,R3,PPLSECT      PUT ADDR OF UPT,ECT,ECB IN PPL        02110000
         SPACE                                                          02120000
         LA    R2,PARSBACK        -> PARSE ANSWER AREA                  02130000
         ST    R2,PPLANS          IN PPL                                02140000
         L     R1,CPPLCBUF        CBUF PTR IN PPL                       02150000
         ST    R1,PPLCBUF                                               02160000
         ST    R13,PPLUWA         PASS ADDR OF WORKAREA TO VALID CHK    02170000
         SPACE                                                          02180000
         XC    MTPARML(MTLENMT),MTPARML  ZERO IKJEFF02 PARM LIST        02190000
         LA    R1,MTCSECTP        -> MSG DESC SECTION OF PARMLIST       02200000
         ST    R1,MTPLPTR         IN PARM LIST                          02210000
         ST    R6,MTCPPLP         PTR TO CPPL FOR IKJEFF02              02220000
         ST    R3,MTECBP          PTR TO ECB  FOR IKJEFF02              02230000
         SR    R1,R1              ZERO RESERVED                         02240000
         ST    R1,MTRESV1         WORD IN IKJEFF02 PARM LIST            02250000
         MVI   MTHIGH,X'80'       SET VL BIT IN IKJEFF02 PARM LIST      02260000
         SPACE                                                          02270000
         L     R1,=V(MSGCSECT)    PTR TO MSG CSECT                      02280000
         ST    R1,MTCSECTP        IN MSG DESC SECTION                   02290000
         MVI   MTSW1,MTPUTLSW     ISSUE MSGS WITH PUTLINE               02300000
         MVI   MTSW2,0                                                  02310000
         SPACE                                                          02320000
         DROP  R5,R6                                                    02330000
         EJECT                                                          02340000
* CALL PARSE SERVICE ROUTINE TO GET PARAMETERS                          02350000
         SPACE                                                          02360000
CALLPARS CALLTSSR EP=IKJPARS,MF=(E,PPLSECT)                             02370000
         SPACE                                                          02380000
         LTR   R15,R15            PARSE OK ?                            02390000
         BZ    PARSEOK            YES                                   02400000
         SPACE                                                          02410000
         CH    R15,=H'4'          USER PROFILE NOPROMPT ?               02420000
         BE    RETURN             YES - GO AWAY                         02430000
         SPACE                                                          02440000
         LA    R1,GFPARSE         ROUTINE ID FOR GENERAL FAIL           02450000
         B     GNRLFAIL           GET GENERAL FAIL TO DIAGNOSE ERROR    02460000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          PROCESS PARMS R*02470000
               ETURNED BY PARSE'                                        02480000
PARSEOK  TM    FLAGS,EPUCAT       UNCATLG ?                             02490000
         BO    PARMSET            PARMS ALL SET                         02500000
         SPACE                                                          02510000
         L     R9,PARSBACK        -> PDL AREA                           02520000
         SPACE                                                          02530000
* MAKE SURE VOLSER HAS BEEN SUPPLIED                                    02540000
         SPACE                                                          02550000
         TM    FLAGS,SVOL         VOLSER SUPPLIED ?                     02560000
         BO    GOTVOL             YEP                                   02570000
         TM    FLAGS,EPSCR        SCRATCH ?                             02580000
         BNO   NEEDVOL            NO - MUST BE CATLG AND IT NEEDS VOL   02590000
         SPACE                                                          02600000
* ISSUE LOCATE TO FIND VOLUME(S) FOR SCRATCH                            02610000
         SPACE                                                          02620000
         LA    R3,DSNAME          -> DSNAME                             02630000
         ST    R3,CAMLSTP2                                              02640000
         SR    R3,R3              NO CVOL PARM                          02650000
         ST    R3,CAMLSTP3                                              02660000
         LA    R3,CAMAREA         -> AREA TO PUT VOLUME LIST            02670000
         ST    R3,CAMLSTP4                                              02680000
         MVC   CAMLST,NAMECAM     CAMLST NAME FLAGS                     02690000
         LOCATE CAMLST            LOCATE DSNAME IN CATALOG              02700000
         SPACE                                                          02710000
         LTR   R15,R15            LOCATE OK ?                           02720000
         BNZ   BADLOC             NOPE                                  02730000
         SPACE                                                          02740000
         B     SCRPRM             GO FINISH OFF SCRATCH PARAMETERS      02750000
         SPACE                                                          02760000
NEEDVOL  MVC   MTMSGID,=CL4'V04'  "VOLSER REQUIRED"                     02770000
         OI    FLAGS,QUIT         NO GO                                 02780000
         LA    LINK,PARMSET                                             02790000
         B     ISSUEMSG                                                 02800000
         SPACE                                                          02810000
GOTVOL   TM    FLAGS,SUNIT        UNIT SUPPLIED ?                       02820000
         BNO   UCBLOOK            NO - LOOK IT UP IN UCB                02830000
         SPACE                                                          02840000
         CLI   DEVTYPE+2,X'20'    DASD DEVICE SPECIFIED ?               02850000
         BNE   SUCBLOOK           NOPE - SKIP LOOKUP                    02860000
         SPACE                                                          02870000
* LOOK UP UCB FOR GIVEN VOLSER TO GET DEVICE TYPE                       02880000
         SPACE                                                          02890000
UCBLOOK  L     R3,CVTPTR          -> CVT                                02900000
         USING CVTMAP,R3                                                02910000
         XC    IOSVSWRK,IOSVSWRK  INITIALIZE IOSVSUCB WORKAREA 10/16/90 02920000
         LA    R1,IOSVSWRK        BUILD IOSVSUCB PARM LIST     10/16/90 02930000
         ST    R1,IOSVSPRM             "                       10/16/90 02940000
         LA    R1,IOSVSDEV             "                       10/16/90 02950000
         ST    R1,IOSVSPRM+4           "                       10/16/90 02960000
         LA    R1,IOSVSUCB             "                       10/16/90 02970000
         ST    R1,IOSVSPRM+8           "                       10/16/90 02980000
         OI    IOSVSPRM+8,X'80'        "                       10/16/90 02990000
         MVI   IOSVSDEV,UCB3DACC       "                       10/16/90 03000000
         AGO   .SKIP38J                                        * JLM *  03010000
UCBLOOP  L     R15,CVTUCBSC       -> IOSVSUCB UCB SCAN RTN     10/16/90 03020000
         LA    R1,IOSVSPRM        -> IOSVSUCB PARAMETER LIST   10/16/90 03030000
         BALR  R14,R15            GET POINTER TO NEXT UCB      10/16/90 03040000
         LTR   R15,R15            ANYMORE UCBS?                10/16/90 03050000
         BNZ   VOLUNAV            NO, VOLUME NOT MOUNTED       10/16/90 03060000
         L     R2,IOSVSUCB        POINT TO UCB                 10/16/90 03070000
         USING UCB,R2                  "                       10/16/90 03080000
         AGO   .BYPASS1                                        10/16/90 03090000
.SKIP38J ANOP  ,                                               * JLM *  03100000
*        L     R3,CVTILK2         -> UCB ADDR TABLE            * JLM *  03110000
         L     R3,CVT+X'028'      ADDR (UCB VECTOR TABLE)      * JLM *  03120000
*        DROP  R3                                              * JLM *  03130000
         SR    R2,R2                                                    03140000
         USING UCB,R2                                                   03150000
UCBLOOP  ICM   R2,B'0011',0(R3)   -> UCB                                03160000
         LA    R3,2(,R3)          -> NEXT UCB PTR                       03170000
         BZ    UCBLOOP            NEXT UCB IF NULL                      03180000
         CL    R2,=X'0000FFFF'    END OF TABLE ?                        03190000
         BE    VOLUNAV            YES - VOLUME NOT MOUNTED              03200000
         TM    UCBTYP+2,X'20'     DASD ?                                03210000
         BNO   UCBLOOP            NO                                    03220000
.BYPASS1 ANOP  ,                                               10/16/90 03230000
         CLC   VOLSER,UCBVOLI     THIS THE VOLUME ?                     03240000
         BNE   UCBLOOP            NO                                    03250000
         DROP  R3                 FORGET ABOUT CVT             10/16/90 03260000
         SPACE                                                          03270000
GOTUCB   TM    FLAGS,SUNIT        UNIT SUPPLIED ??                      03280000
         BNO   GOTUCB2            NO - FILL IN DEVTYPE                  03290000
         CLC   DEVTYPE,UCBTYP     YES - SAME AS MOUNTED VOL ?           03300000
         BE    SUCBLOOK           YES - ALL OK                          03310000
         OI    FLAGS,SKIPOBTN     NOT SAME - BYPASS OBTAIN              03320000
         B     SUCBLOOK                                                 03330000
         SPACE                                                          03340000
GOTUCB2  MVC   DEVTYPE,UCBTYP     SAVE DEVICE TYPE                      03350000
         B     SUCBLOOK           SKIP ERROR EXIT                       03360000
         DROP  R2                                                       03370000
         SPACE                                                          03380000
VOLUNAV  DS    0H                 VOLUME UNAVAILBLE                     03390000
         TM    FLAGS,SUNIT        UNIT SUPPLIED ?                       03400000
         BNO   *+12               NO - ISSUE ERROR MSG                  03410000
         OI    FLAGS,SKIPOBTN     YES - BYPASS OBTAIN                   03420000
         B     SUCBLOOK                                                 03430000
         MVC   MTMSGID,=CL4'V02'  VOLUME NOT MOUNTED MSG                03440000
         TM    FLAGS,EPSCR        SCRATCH ?                             03450000
         BNO   *+10               NOPE - MSGID IS RIGHT                 03460000
         MVC   MTMSGID,=CL4'V03'  YES - FIX MSGID                       03470000
         SPACE                                                          03480000
         LA    R1,VOLSER          -> VOLUME                             03490000
         ST    R1,MTINSRT         PASS TO IKJEFF02                      03500000
         MVI   MTINSRT,L'VOLSER   LENGTH                                03510000
         OI    FLAGS,QUIT                                               03520000
         LA    LINK,PARMSET       RETURN ADDR                           03530000
         B     ISSUEMSG                                                 03540000
         SPACE                                                          03550000
SUCBLOOK DS    0H                 SKIP UCB LOOKUP                       03560000
         SPACE                                                          03570000
         TM    FLAGS,EPSCR        SCRATCH EP ?                          03580000
         BO    SCRPRM             YES                                   03590000
         SPACE                                                          03600000
* BUILD VOLUME LIST FOR CATALOG MACRO                                   03610000
         SPACE                                                          03620000
         USING CATPARM,R9         DSECT                                 03630000
         LA    R8,CAMAREA+2       -> AREA FOR VOLUME LIST               03640000
         USING VOLLIST,R8         DESCRIPTION OF VOLUME LIST            03650000
         SPACE                                                          03660000
VPTR     EQU   5                  VOLUME PDE LIST REGISTER              03670000
UPTR     EQU   6                  UNIT PDE LIST REGISTER                03680000
FPTR     EQU   7                  FILESEQ PDE LIST REGISTER             03690000
         SPACE                                                          03700000
         LA    VPTR,CATVOLIN      -> VOLUME PDE LIST                    03710000
         LA    UPTR,CATUNTIN      -> UNIT TYPE PDE LIST                 03720000
         LA    FPTR,CATFSQIN      -> FILE SEQUENCE NUMBER PDE LIST      03730000
         SR    R4,R4              COUNT NUMBER OF VOLUMES               03740000
         SPACE 2                                                        03750000
VOLLOOP  MVI   CATVOL,C' '        BLANK OUT VOLSER FIELD                03760000
         MVC   CATVOL+1(L'CATVOL-1),CATVOL                              03770000
         L     R1,0(,VPTR)        -> VOLUME                             03780000
         LH    R2,4(,VPTR)        LENGTH                                03790000
         BCTR  R2,0               -1 FOR EX                             03800000
         LA    R3,CATVOL          -> DESTINATION                        03810000
         EX    R2,MOVEPARM        MOVE IT IN                            03820000
*        MVC   0(*-*,R3),0(R1)                                          03830000
         L     VPTR,8(,VPTR)      -> NEXT VOLUME PDE                    03840000
         SPACE                                                          03850000
*                                                              11/16/90 03860000
*  I'M NOT SURE WHY THE FOLLOWING CODE IS HERE.  IT CAUSED     11/16/90 03870000
*  A MESS WHEN I PUT IN THE IEFEB4UV MODS FOR XA.  DELETE      11/16/90 03880000
*  IT AND SEE WHAT HAPPENS...   DBA                            11/16/90 03890000
*                                                              11/16/90 03900000
*+*      LTR   UPTR,UPTR          ANY UNIT TYPE ?              11/16/90 03910000
*+*      BZ    V1UNIT             NO - USE UNIT & DEVTYPE FLDS 11/16/90 03920000
*+*      TM    6(UPTR),X'80'      UNIT TYPE SUPPLIED ?         11/16/90 03930000
*+*      BNO   V1UNIT             NO - IN UNIT & DEVTYPE FLDS  11/16/90 03940000
*+*      SPACE ,                                               11/16/90 03950000
*+*      L     R1,0(,UPTR)        -> DEVNAMET ENTRY            11/16/90 03960000
*+*      MVC   DEVCODE,8(R1)      GET DEVICE TYPE              11/16/90 03970000
*+*      L     UPTR,8(,UPTR)      YES - GET NEXT PDE           11/16/90 03980000
*+*      LA    UPTR,0(,UPTR)      CLEAR HIGH BYTE              11/16/90 03990000
*+*      B     VFSEQN             HAVE UNIT TYPE NOW           11/16/90 04000000
         SPACE                                                          04010000
V1UNIT   MVC   DEVCODE,DEVTYPE    GET DEVICE TYPE                       04020000
         SPACE                                                          04030000
VFSEQN   TM    6(FPTR),X'80'      FILE SEQ SUPPLIED ?                   04040000
         BNO   VNFSEQ             NO - DETERMINE WHAT IT SHOULD BE      04050000
         SPACE                                                          04060000
         L     R1,0(,FPTR)        -> FILE SEQ NUM                       04070000
         L     R1,0(,R1)          FILE SEQ NUM                          04080000
         STH   R1,DSEQNUM         PUT IN LIST                           04090000
         CLI   8(FPTR),X'FF'      ANY MORE FILE SEQ NUM ??              04100000
         BE    NXTVOL             NO - SKIP CHAINING                    04110000
         L     FPTR,8(,FPTR)      YES - GET IT                          04120000
         B     NXTVOL             TO NEXT ENTRY                         04130000
         SPACE                                                          04140000
VNFSEQ   SR    R1,R1              ASSUME ZERO FILE SEQ NUMBER           04150000
         CLI   DEVCODE+2,X'80'    MAG TAPE DEVICE ?                     04160000
         BNE   *+8                NOPE                                  04170000
         LA    R1,1               YES - FSEQ NUM OF ONE                 04180000
         STH   R1,DSEQNUM         PUT IN LIST                           04190000
         SPACE 2                                                        04200000
NXTVOL   LA    R4,1(,R4)          COUNT THEM VOLS                       04210000
         CH    R4,=Y(#VOLS)       MORE THAN MAX VOLUMES ?               04220000
         BH    VOLHIGH            TOO MUCH                              04230000
         LA    R8,NEXTVOL         -> NEXT VOLUME LIST ENTRY             04240000
         LA    VPTR,0(,VPTR)      CLEAR HIGH BYTE                       04250000
         LTR   VPTR,VPTR          ANY MORE VOLS ?                       04260000
         BNZ   VOLLOOP            YES - GET THEM                        04270000
         SPACE                                                          04280000
         STH   R4,CAMAREA         STORE VOLUME COUNT                    04290000
         B     PARMSET            DONE WITH CATLG PARAMETERS            04300000
         SPACE                                                          04310000
VOLHIGH  BCTR  R4,0               -1                                    04320000
         STH   R4,CAMAREA         STORE VOLUME COUNT                    04330000
         SPACE                                                          04340000
         MVC   MTMSGID,=CL4'V01'        TOO MANY VOLUMES MSG            04350000
         LA    LINK,PARMSET             RETURN ADDR                     04360000
         B     ISSUEMSG                 TELL USER ABOUT IT              04370000
         SPACE                                                          04380000
         DROP  R8                                                       04390000
         EJECT                                                          04400000
* PROCESS SCRATCH PARAMETERS                                            04410000
         SPACE                                                          04420000
         USING SCRPARM,R9         DSECT                                 04430000
SCRPRM   CLI   SCRPRGKY+1,0       PURGE SPECIFIED ?                     04440000
         BE    PARMSET            NOPE - ALL DONE                       04450000
         OI    FLAGS,SCRPURGE     YES - REMEMBER THAT                   04460000
         SPACE                                                          04470000
         DROP  R9                 FINISHED WITH PDL                     04480000
         SPACE 4                                                        04490000
PARMSET  IKJRLSA PARSBACK          RELEASE PARSE STORAGE                04500000
         SPACE 2                                                        04510000
         TM    FLAGS,QUIT         ANY ERRORS ?                          04520000
         BO    RETURN             YES - HANG IT UP                      04530000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          PROCESSING'     04540000
         TM    FLAGS,EPUCAT+EPSCR+SKIPOBTN   SCR,UNCAT OR SKIP OBTN ?   04550000
         BNZ   BLDCAML            YEP - BYPASS OBTAIN                   04560000
         SPACE                                                          04570000
         CLI   DEVTYPE+2,X'20'    DASD DEVICE ???                       04580000
         BNE   BLDCAML            NOPE - BYPASS OBTAIN                  04590000
         SPACE                                                          04600000
* VERIFY DATASET IS ON VOLUME SPECIFIED                                 04610000
         SPACE                                                          04620000
         LA    R3,DSNAME          -> DSNAME                             04630000
         ST    R3,CAMLSTP2                                              04640000
         LA    R3,VOLSER          -> VOLUME ID                          04650000
         ST    R3,CAMLSTP3                                              04660000
         LA    R3,F1DSCB          -> AREA FOR F1 DSCB                   04670000
         ST    R3,CAMLSTP4                                              04680000
         MVC   CAMLST,SRCHCAM     CAMLST SEARCH FLAGS                   04690000
         OBTAIN CAMLST            GET THE DSCB FOR DATASET              04700000
         SPACE                                                          04710000
         LTR   R15,R15            OBTAIN OK ???                         04720000
         BNZ   BADOBTN            NOPE                                  04730000
         SPACE 2                                                        04740000
         EJECT                                                          04750000
* BUILD CAMLST FOR REQUIRED FUNCTION                                    04760000
         SPACE                                                          04770000
BLDCAML  TM    FLAGS,EPSCR        SCRATCH EP ?                          04780000
         BO    SCRCAML            YES - BUILD SCRATCH CAMLST            04790000
         SPACE                                                          04800000
CATCAML  LA    R3,DSNAME          -> DSNAME                             04810000
         ST    R3,CAMLSTP2                                              04820000
         SR    R3,R3              NO CVOL PARM                          04830000
         ST    R3,CAMLSTP3                                              04840000
         LA    R3,CAMAREA         -> VOLUME LIST                        04850000
         ST    R3,CAMLSTP4                                              04860000
         MVC   CAMLST,CATCAM      ASSUME CATBX CAMLST                   04870000
         TM    FLAGS,EPCAT        RIGHT ?                               04880000
         BO    *+10               YES                                   04890000
         MVC   CAMLST,UCATCAM     NO - UNCATALOG                        04900000
         SPACE                                                          04910000
         CATALOG CAMLST           PERFORM THE FUNCTION                  04920000
         SPACE                                                          04930000
         LTR   R15,R15            CATALOG OK ??                         04940000
         BNZ   BADCAT             NOPE                                  04950000
         SPACE                                                          04960000
         B     CONFIRM            GO WRITE CONFIMATION MSG              04970000
         SPACE                                                          04980000
SCRCAML  TM    FLAGS,SVOL         WAS VOLUME SUPPLIED BY USER ?         04990000
         BNO   SCRCAML2           NO - VOL LIST WAS BUILT BY LOCATE     05000000
         LA    R1,1               ONE VOLUME                            05010000
         STH   R1,CAMAREA         IN VOLUME LIST                        05020000
         LA    R5,CAMAREA+2       -> VOLUME LIST ENTRY                  05030000
         USING VOLLIST,R5         DSECT                                 05040000
         MVC   DEVCODE,DEVTYPE    DEVICE TYPE                           05050000
         MVC   CATVOL,VOLSER      VOLSER                                05060000
         SR    R1,R1              ZERO FILE SEQ NUM                     05070000
         STH   R1,DSEQNUM         STORE IT                              05080000
         DROP  R5                                                       05090000
         SPACE                                                          05100000
SCRCAML2 LA    R3,DSNAME          -> DSNAME                             05110000
         ST    R3,CAMLSTP2                                              05120000
         SR    R3,R3              NO THIRD PARM                         05130000
         ST    R3,CAMLSTP3                                              05140000
         LA    R3,CAMAREA         -> VOLUME LIST                        05150000
         ST    R3,CAMLSTP4                                              05160000
         MVC   CAMLST,SCRCAM      ASSUME NOPURGE CAMLST                 05170000
         TM    FLAGS,SCRPURGE     RIGHT ?                               05180000
         BNO   *+10               YES                                   05190000
         MVC   CAMLST,SCRPCAM     NO - SCRATCH PURGE                    05200000
         SPACE                                                          05210000
         SR    R0,R0              DON'T MOUNT ANY VOLUMES               05220000
         SCRATCH CAMLST           PERFORM THE FUNCTION                  05230000
         SPACE                                                          05240000
         LTR   R15,R15            SCRATCH OK ??                         05250000
         BNZ   BADSCR             NOPE                                  05260000
         SPACE                                                          05270000
         TM    FLAGS,SVOL         DID USER SUPPLY VOLUME FOR SCRATCH ?  05280000
         BO    CONFIRM            YES - ALL DONE WITH SCRATCH THEN      05290000
         SPACE                                                          05300000
         MVC   CAMLST,UCATCAM     OTHERWISE NEED TO UNCATALOG DS ALSO   05310000
         CATALOG CAMLST           DO IT     (PARM LIST SAME AS SCRATCH) 05320000
         SPACE                                                          05330000
         LTR   R15,R15            UNCATALOG OK ?                        05340000
         BNZ   BADCAT             NOPE                                  05350000
         EJECT                                                          05360000
* WRITE CONFIRMATION MESSAGE TO USER                                    05370000
         SPACE                                                          05380000
CONFIRM  DS    0H                 WRITE OUT CONFIRMATION MSG            05390000
         SPACE                                                          05400000
         LA    R1,DSNAME          -> DSNAME                             05410000
         ST    R1,MTINSRT         1ST VARIBLE                           05420000
         MVC   MTINSRT(1),DSNLEN  LENGTH                                05430000
         SPACE                                                          05440000
         TM    FLAGS,EPUCAT       UNCATALOG ?                           05450000
         BO    CONFUCAT           YES                                   05460000
         SPACE                                                          05470000
         TM    FLAGS,EPCAT        CATALOG ?                             05480000
         BNO   CONFSCR            NO - SCRATCH                          05490000
         SPACE                                                          05500000
         MVC   MTMSGID,=CL4'CAT'  CATALOG CONFIRMATION MSG              05510000
         B     CONFMSG                                                  05520000
         SPACE                                                          05530000
CONFSCR  TM    FLAGS,SVOL         DID USER SUPPLY VOLUME ?              05540000
         BNO   CONFSCR2           NO - SO DIFFERENT MSG                 05550000
         SPACE                                                          05560000
         MVC   MTMSGID,=CL4'SCR'  SCRATCH CONFIRMATION MSG              05570000
         B     CONFMSG                                                  05580000
         SPACE                                                          05590000
CONFSCR2 MVC   MTMSGID,=CL4'DEL'  DELETE CONFIRMATION MSG               05600000
         B     CONFMSG                                                  05610000
         SPACE                                                          05620000
CONFUCAT MVC   MTMSGID,=CL4'UCAT' UNCATALOG CONFIRMATION MSG            05630000
         SPACE                                                          05640000
CONFMSG  BAL   LINK,ISSUEMSG                                            05650000
         SPACE                                                          05660000
RETURN   LA    R1,CORE            -> WORKAREA                           05670000
         LA    R0,CORESIZE        LENGTH                                05680000
         L     R13,4(,R13)        GET OLD SAVE AREA                     05690000
         SVC   10                 FREE WORKAREA                         05700000
         SPACE                                                          05710000
         SR    R15,R15            RETURN CODE ZERO                      05720000
         RETURN (14,12),,RC=(15)  RETURN TO CALLER                      05730000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          SUBROUTINES'    05740000
         SPACE                                                          05750000
ISSUEMSG CALLTSSR EP=IKJEFF02,MF=(E,MTPARML)                            05760000
         SPACE                                                          05770000
         LTR   R15,R15            IKJEFF02 OK ?                         05780000
         BZR   LINK               YES - RETURN                          05790000
         SPACE                                                          05800000
         CH    R15,=H'76'         IKJEFF02 PARMLIST INVALID ?           05810000
         BNE   NEFF02ER           NO                                    05820000
         SPACE                                                          05830000
         ABEND 200,DUMP                                                 05840000
         SPACE                                                          05850000
NEFF02ER LA    R1,GFPUTL          NO - PUTLINE ERROR                    05860000
         B     GNRLFAIL           GET GENERAL FAIL TO DIAGNOSE ERROR    05870000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          PARSE PCLS'     05880000
         PRINT NOGEN                                                    05890000
CATPARS  IKJPARM DSECT=CATPARM                                          05900000
CATDSNIN IKJPOSIT DSNAME,USID,PROMPT='DSNAME',VALIDCK=DSNCHK            05910000
CATVOLKY IKJKEYWD                                                       05920000
         IKJNAME 'VOLUME',SUBFLD=CATVOLSF,ALIAS='VOLSER'                05930000
CATFSQKY IKJKEYWD                                                       05940000
         IKJNAME 'FILESEQUENCE',SUBFLD=CATFSQSF,ALIAS='FSEQN'           05950000
CATUNTKY IKJKEYWD                                                       05960000
         IKJNAME 'UNIT',SUBFLD=CATUNTSF,ALIAS='DEVTYPE'                 05970000
         SPACE                                                          05980000
CATVOLSF IKJSUBF                                                        05990000
CATVOLIN IKJIDENT 'VOLUME',LIST,MAXLNTH=6,PROMPT='VOLUME',             *06000000
               FIRST=ALPHANUM,OTHER=ALPHANUM,VALIDCK=VOLCHK             06010000
CATFSQSF IKJSUBF                                                        06020000
CATFSQIN IKJIDENT 'FILE SEQUENCE NUMBER',LIST,INTEG,                   *06030000
               PROMPT='FILE SEQUENCE NUMBER'                            06040000
CATUNTSF IKJSUBF                                                        06050000
CATUNTIN IKJIDENT 'UNIT TYPE',LIST,MAXLNTH=8,VALIDCK=UNITCHK,          *06060000
               FIRST=ALPHANUM,OTHER=ALPHANUM,                          *06070000
               PROMPT='UNIT TYPE'                                       06080000
         IKJENDP                                                        06090000
         SPACE 4                                                        06100000
UCATPARS IKJPARM DSECT=UCATPARM                                         06110000
UCATDSN  IKJPOSIT DSNAME,USID,PROMPT='DSNAME',VALIDCK=DSNCHK            06120000
         IKJENDP                                                        06130000
         SPACE 4                                                        06140000
SCRPARS  IKJPARM DSECT=SCRPARM                                          06150000
SCRDSNIN IKJPOSIT DSNAME,USID,PROMPT='DSNAME',VALIDCK=DSNCHK            06160000
SCRVOLKY IKJKEYWD                                                       06170000
         IKJNAME 'VOLUME',SUBFLD=SCRVOLSF,ALIAS='VOLSER'                06180000
SCRPRGKY IKJKEYWD                                                       06190000
         IKJNAME 'PURGE'                                                06200000
         SPACE                                                          06210000
SCRVOLSF IKJSUBF                                                        06220000
SCRVOLIN IKJIDENT 'VOLUME',MAXLNTH=6,PROMPT='VOLUME',                  *06230000
               FIRST=ALPHANUM,OTHER=ALPHANUM,VALIDCK=VOLCHK             06240000
         IKJENDP                                                        06250000
         SPACE 2                                                        06260000
         PRINT GEN                                                      06270000
         SPACE 3                                                        06280000
         PUSH  USING              SAVE MAINLINE ADDRESSABLITY           06290000
         DROP  ,                  DROP MAINLINE ADDRESSABLITY           06300000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          PARSE VALIDITY *06310000
               CHECKING ROUTINES'                                       06320000
DSNCHK   SAVE  (14,6),,*                                                06330000
         SPACE                                                          06340000
         LR    R6,R1              SAVE PARM ADDR                        06350000
         LR    R5,R15             LOAD BASE                             06360000
         USING DSNCHK,R5                                                06370000
         L     R4,4(,R6)          -> WORKAREA                           06380000
         USING CORE,R4                                                  06390000
         SPACE                                                          06400000
         MVI   DSNAME,C' '        BLANK OUT DSNAME FIELD                06410000
         MVC   DSNAME+1(L'DSNAME-1),DSNAME                              06420000
         SPACE                                                          06430000
         L     R3,0(,R6)          -> PDE FOR DSNAME                     06440000
         L     R1,0(,R3)          -> DSNAME                             06450000
         LH    R2,4(,R3)          LENGTH OF DSNAME                      06460000
         SPACE                                                          06470000
         STC   R2,DSNLEN          SAVE DSNAME LENGTH                    06480000
         LA    R3,DSNAME          -> AREA TO MOVE TO                    06490000
         BCTR  R2,0               MINUS ONE FOR EX MVC                  06500000
         EX    R2,MOVEPARM        GET DSN                               06510000
*        MVC   0(*-*,R3),0(R1)                                          06520000
         SPACE                                                          06530000
         SR    R15,R15                                                  06540000
         RETURN (14,6),,RC=(15)                                         06550000
         SPACE                                                          06560000
         DROP  R5,R4                                                    06570000
         EJECT                                                          06580000
VOLCHK   SAVE  (14,6),,*                                                06590000
         SPACE                                                          06600000
         LR    R6,R1              SAVE PARM ADDR                        06610000
         LR    R5,R15             LOAD BASE                             06620000
         USING VOLCHK,R5                                                06630000
         L     R4,4(,R6)          -> WORKAREA                           06640000
         USING CORE,R4                                                  06650000
         SPACE                                                          06660000
         TM    FLAGS,SVOL         GOT VOLSER ALREADY ?                  06670000
         BO    VOLCRET            YES - RETURN                          06680000
         SPACE                                                          06690000
         MVI   VOLSER,C' '        BLANK OUT VOLSER FIELD                06700000
         MVC   VOLSER+1(L'VOLSER-1),VOLSER                              06710000
         OI    FLAGS,SVOL         INDICATE WE HAVE A VOLSER             06720000
         SPACE                                                          06730000
         L     R3,0(,R6)          -> PDE FOR VOLSER                     06740000
         L     R1,0(,R3)          -> VOLSER                             06750000
         LH    R2,4(,R3)          LENGTH OF VOLSER                      06760000
         SPACE                                                          06770000
         LA    R3,VOLSER          -> AREA TO MOVE TO                    06780000
         BCTR  R2,0               MINUS ONE FOR EX MVC                  06790000
         EX    R2,MOVEPARM        GET DSN                               06800000
*        MVC   0(*-*,R3),0(R1)                                          06810000
         SPACE                                                          06820000
VOLCRET  SR    R15,R15                                                  06830000
         RETURN (14,6),,RC=(15)                                         06840000
         SPACE                                                          06850000
         DROP  R5,R4                                                    06860000
         EJECT                                                          06870000
UNITCHK  SAVE  (14,12),,*                                               06880000
         SPACE                                                          06890000
         LR    R12,R1             SAVE PARM ADDR                        06900000
         LR    R10,R15            LOAD BASE                             06910000
         USING UNITCHK,R10                                              06920000
         L     R11,4(,R12)        -> WORKAREA                           06930000
         USING CORE,R11                                                 06940000
         SPACE 2                                                        06950000
         MVI   UNIT,C' '          BLANK OUT UNIT FIELD                  06960000
         MVC   UNIT+1(L'UNIT-1),UNIT                                    06970000
         SPACE                                                          06980000
         L     R3,0(,R12)         -> PDE FOR UNIT                       06990000
         L     R1,0(,R3)          -> UNIT                               07000000
         LH    R2,4(,R3)          LENGTH OF UNIT                        07010000
         LA    R3,UNIT            -> AREA FOR UNIT                      07020000
         BCTR  R2,0               MINUS ONE FOR EX                      07030000
         EX    R2,MOVEPARM                                              07040000
*        MVC   0(*-*,R3),0(R1)                                          07050000
         SPACE                                                          07060000
*                                                              11/16/90 07070000
*  IF MVS/XA THEN VERIFY THE UNITNAME WITH THE UNIT            11/16/90 07080000
*  VERIFICATION SERVICE.  IF MVS-370 THEN GO MANHANDLE         11/16/90 07090000
*  DEVNAMET DIRECTLY.                                          11/16/90 07100000
*                                                              11/16/90 07110000
         AGO   .SKIP38A                                        * JLM *  07120000
         L     R3,CVTPTR          -> CVT                       11/16/90 07130000
         USING CVT,R3                  "                       11/16/90 07140000
         TM    CVTDCB,CVTMVSE     XA SYSTEM?                   11/16/90 07150000
         BNO   DEVN370            NO, FIDDLE WITH DEVNAMET     11/16/90 07160000
         DROP  R3                                              11/16/90 07170000
         SPACE ,                                               11/16/90 07180000
*                                                              11/16/90 07190000
*  IEFEB4UV NEEDS AN 18-WORD SAVE AREA POINTED TO BY           11/16/90 07200000
*  REG13.                                                      11/16/90 07210000
*                                                              11/16/90 07220000
         ST    R13,UVSRSAVE+4     PUSH OLD SAVE AREA ADDRESS   11/16/90 07230000
         LA    R13,UVSRSAVE       POINT TO NEW ONE             11/16/90 07240000
         SPACE ,                                               11/16/90 07250000
*                                                              11/16/90 07260000
*  SETUP TO CALL IEFEB4UV FUNCTIONS #3 AND #8 (SEE MVS/XA      11/16/90 07270000
*  SYSTEM MODIFICATIONS SPL).  FUNCTION #3 RETURNS A LIST      11/16/90 07280000
*  OF UCBS ASSOCIATED WITH THIS UNITNAME.  FUNCTION #8         11/16/90 07290000
*  RETURNS THE ATTRIBUTES OF THE UNITNAME.                     11/16/90 07300000
*                                                              11/16/90 07310000
         LA    R5,UVSRUTAB        POINT TO UVSR UNIT TABLE     11/16/90 07320000
         ST    R5,UVSRPARM             "                       11/16/90 07330000
         LA    R5,UVSRFLGS        POINT TO UVSR FLAGS          11/16/90 07340000
         ST    R5,UVSRPARM+4           "                       11/16/90 07350000
         MVC   UVSRFLGS,=XL4'10800000' SET FNS #3 AND #8       11/16/90 07360000
         XC    UVSRUTAB,UVSRUTAB  CLEAR UNIT TABLE             11/16/90 07370000
         XC    UVSRATTR,UVSRATTR  CLEAR ATTRIBUTE AREA         11/16/90 07380000
         MVI   UVSRATTR,X'0A'     SET LENGTH OF ATTRIBUTE AREA 11/16/90 07390000
         MVC   UVSRUTAB(8),UNIT   MOVE IN UNIT NAME            11/16/90 07400000
         LA    R1,UVSRATTR        POINT TO ATTRIBUTE AREA      11/16/90 07410000
         ST    R1,UVSRUTAB+12          "                       11/16/90 07420000
         LA    R1,UVSRPARM        POINT TO PARAMETER LIST      11/16/90 07430000
         LINK  EP=IEFEB4UV        VERIFY UNITNAME              11/16/90 07440000
         LTR   R15,R15            GO OKAY?                     11/16/90 07450000
         BZ    EB4OK              YES, BRANCH                  11/16/90 07460000
EB4NOK   L     R13,4(,R13)        RESTORE OLD S/A POINTER      11/16/90 07470000
         LA    R15,4              TELL PARSE NAME INVALID      11/16/90 07480000
         B     UNITCRET           RETURN                       11/16/90 07490000
         SPACE ,                                               11/16/90 07500000
*                                                              11/16/90 07510000
*  CALL TO IEFEB4UV WENT OKAY.  FUNCTION #3 RETURNED A         11/16/90 07520000
*  LIST OF UCBS; LOOK AT THE FIRST ONE AND GET ITS DEVICE      11/16/90 07530000
*  TYPE, THEN FREEMAIN THE UCB POINTER LIST.                   11/16/90 07540000
*                                                              11/16/90 07550000
EB4OK    L     R1,UVSRUTAB+8      POINT TO UCB LIST            11/16/90 07560000
         CLC   4(4,R1),=F'0'      AT LEAST ONE ENTRY?          11/16/90 07570000
         BNH   EB4NOK             NO, RETURN ERROR TO PARSE    11/16/90 07580000
         L     R0,0(,R1)          GET SP/LENGTH OF UCB LIST    11/16/90 07590000
         L     R15,8(,R1)         POINT TO FIRST UCB           11/16/90 07600000
         USING UCB,R15                 "                       11/16/90 07610000
         MVC   DEVTYPE,UCBTYP     MOVE DEVICE TYPE FROM UCB    11/16/90 07620000
         DROP  R15                                             11/16/90 07630000
         FREEMAIN R,LV=(0),A=(1)  FREE UCB POINTER LIST        11/16/90 07640000
         SPACE ,                                               11/16/90 07650000
*                                                              11/16/90 07660000
*  HAVING RETRIEVED A DEVICE TYPE, MAKE SURE IT'S THE ONLY     11/16/90 07670000
*  ONE ASSOCIATED WITH THIS UNITNAME.                          11/16/90 07680000
*                                                              11/16/90 07690000
         CLI   UVSRATTR+3,X'01'   MORE THAN ONE DEVICE CLASS?  11/16/90 07700000
         BH    EB4NOK             YES, FAIL PARSE              11/16/90 07710000
         CLC   UVSRATTR+4(4),=F'1' MORE THAN ONE DEVICE TYPE?  11/16/90 07720000
         BH    EB4NOK             YES, FAIL IT AGAIN           11/16/90 07730000
         OI    FLAGS,SUNIT        INDICATE UNIT IS AVAILABLE   11/16/90 07740000
         L     R13,4(,R13)        RESTORE OLD S/A POINTER      11/16/90 07750000
         SR    R15,R15            ZERO RC                      11/16/90 07760000
         B     UNITCRET           RETURN TO PARSE              11/16/90 07770000
         SPACE ,                                               11/16/90 07780000
.SKIP38A ANOP  ,                                               * JLM *  07790000
*                                                              11/16/90 07800000
*  IN MVS/370, DANCE THROUGH THE DEVICE NAME TABLE             11/16/90 07810000
*                                                              11/16/90 07820000
DEVN370  L     R5,@DEVNAME        LOAD ADDR OF DEVNAMET                 07830000
         LTR   R5,R5              GOT IT ?                              07840000
         BNZ   HAVNAMET           YES - SKIP LPA SEARCH                 07850000
         SPACE                                                          07860000
         L     R3,CVTPTR          GET ADDRESS OF CVT                    07870000
         L     R15,CVTLPDSR-CVT(,R3) GET ADDR OF LPDE SEARCH ROUTINE    07880000
         LM    R0,R1,DEVNAMET     GET 'DEVNAMET' IN R0 & R1             07890000
         BALR  R14,R15            CALL IEAVVMSR - DESTROYS R6, R8 & R9  07900000
         B     GOTNAMET           +0 - DEVNAMET FOUND                  *07910000
                                  +4 - DEVNAMET NOT FOUND               07920000
         LA    R15,12             TELL PARSE WE CAN'T GO ON             07930000
         B     UNITCRET           ERROR EXIT                            07940000
GOTNAMET DS    0H                 DEVNAMET LPDE FOUND                   07950000
         LR    R1,R0              GET ADDRESS OF DEVNAMET LPDE          07960000
         L     R5,LPDENTP-LPDE(,R1) GET EP ADDRESS OF DEVNAMET          07970000
         ST    R5,@DEVNAME        SAVE FOR FUTURE USE                   07980000
HAVNAMET L     R15,0(,R5)         GET NUMBER OF ENTRIES                 07990000
         LA    R5,4(,R5)          GET ADDRESS OF 1ST ENTRY              08000000
         SPACE                                                          08010000
DEVTLOOP CLC   UNIT,0(R5)         UNIT NAME MATCH ?                     08020000
         BE    UNITGOT            YES - UNIT FOUND                      08030000
         LA    R5,12(,R5)         NO - POINT TO NEXT ENTRY              08040000
         BCT   R15,DEVTLOOP       LOOP UNTIL END                        08050000
         SPACE                                                          08060000
         LA    R15,4              TELL PARSE NAME INVALID               08070000
         B     UNITCRET           RETURN                                08080000
         SPACE                                                          08090000
UNITGOT  DS    0H                 UNIT NAME FOUND IN DEVNAMET           08100000
         CLI   11(R5),X'00'       A DEVICE GROUP NAME ?                 08110000
         BNE   UNITACT            NO - AN ACTUAL UNIT                   08120000
         LA    R2,GRPTABLE        -> GROUP TABLE                        08130000
         LA    R1,GRPCNT          NUMBER OF GROUPS IN TABLE             08140000
GRPLOOP  CLC   10(1,R5),0(R2)     DEVICE CLASS MATCH ?                  08150000
         BE    GOTDEVCL           YES                                   08160000
         LA    R2,9(,R2)          -> NEXT GROUP ENTRY                   08170000
         BCT   R1,GRPLOOP         LOOP                                  08180000
         SPACE                                                          08190000
         LA    R15,4              TELL PARSE BAD UNIT                   08200000
         B     UNITCRET           RETURN                                08210000
         SPACE                                                          08220000
GOTDEVCL MVC   UNIT,1(R2)         GET ACTUAL UNIT WE WANT               08230000
         L     R5,@DEVNAME        -> DEVNAMET                           08240000
         B     HAVNAMET           GO LOOK UP ACTUAL DEVICE              08250000
         SPACE                                                          08260000
UNITACT  DS    0H                 ACTUAL UNIT NAME FOUND                08270000
         MVC   DEVTYPE,8(R5)      GET DEVICE TYPE FIELD                 08280000
         SPACE                                                          08290000
         OI    FLAGS,SUNIT        HAVE A UNIT                           08300000
         L     R1,0(,R12)         -> PDE FOR UNIT                       08310000
         ST    R5,0(,R1)          SAVE DEVNAMET ENTRY @ IN PDE          08320000
         SR    R15,R15            ZERO RC                               08330000
         SPACE                                                          08340000
UNITCRET RETURN (14,12),,RC=(15)                                        08350000
         SPACE                                                          08360000
         DROP  R10,R11                                                  08370000
         SPACE 4                                                        08380000
         POP   USING              RESTORE MAINLINE ADDRESSABLITY        08390000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          CONSTANTS'      08400000
MOVEPARM MVC   0(*-*,R3),0(R1)    MOVE PARAMTERS FROM PDL               08410000
         SPACE                                                          08420000
CATCAM   CAMLST CATBX,0,,0        CATALOG A DATASET                     08430000
         ORG   CATCAM+4                                                 08440000
UCATCAM  CAMLST UCATDX,0          UNCATALOG A DATASET                   08450000
         ORG   UCATCAM+4                                                08460000
SCRCAM   CAMLST SCRATCH,0,,0      SCRATCH A DATASET                     08470000
         ORG   SCRCAM+4                                                 08480000
SCRPCAM  CAMLST SCRATCH,0,,0,,OVRD  SCRATCH A DATASET (PURGE)           08490000
         ORG   SCRPCAM+4                                                08500000
NAMECAM  CAMLST NAME,0,,0         LOCATE A DATASET                      08510000
         ORG   NAMECAM+4                                                08520000
SRCHCAM  CAMLST SEARCH,0,0,0      OBTAIN A DATASET'S DSCB               08530000
         ORG   SRCHCAM+4                                                08540000
         SPACE                                                          08550000
CAT8LDES DS    0F                 DESCRIPTIONS OF CATALOG ERROR CODES   08560000
         DC    AL1(L'ALRCAT),AL3(ALRCAT)        CATALOG R1 = 0          08570000
         DC    AL1(L'CVOLCH),AL3(CVOLCH)        CATALOG R1 = 4          08580000
         DC    AL1(L'NOQUAL),AL3(NOQUAL)        CATALOG R1 = 8          08590000
         DC    AL1(L'NEEDQUAL),AL3(NEEDQUAL)    CATALOG R1 = 12         08600000
         DC    AL1(L'OVERQUAL),AL3(OVERQUAL)    CATALOG R1 = 16         08610000
         SPACE                                                          08620000
DEVNAMET DC    CL8'DEVNAMET'      NAME OF DEVICE TABLE                  08630000
         SPACE                                                          08640000
GRPTABLE DC    X'80',CL8'3400-5'  MAGNETIC TAPE DEVICE CLASS            08650000
         DC    X'20',CL8'3350'    DIRECT ACCESS DEVICE CLASS            08660000
GRPCNT   EQU   (*-GRPTABLE)/9                                           08670000
         SPACE                                                          08680000
ALRCAT   DC    C'QUALIFICATION ALREADY EXISTS'                          08690000
CVOLCH   DC    C'CLOSED CHAIN OF CVOL POINTERS'                         08700000
NOQUAL   DC    C'QUALIFICATION DOES NOT EXIST'                          08710000
NEEDQUAL DC    C'INSUFFICIENT QUALIFICATION'                            08720000
OVERQUAL DC    C'TOO MUCH QUALIFICATION'                                08730000
         SPACE 2                                                        08740000
         LTORG                                                          08750000
         SPACE 2                                                        08760000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          ERROR EXITS'    08770000
         SPACE                                                          08780000
BADCAT   LA    LINK,RETURN        SET RETURN ADDR FOR ISSUEMSG          08790000
         CH    R15,=H'4'                                                08800000
         BL    CATRC                                                    08810000
         CH    R15,=H'24'                                               08820000
         BH    CATRC                                                    08830000
         B     *(R15)             DETERMINE RETURN CODE                 08840000
         B     CAT4                                                     08850000
         B     CAT8                                                     08860000
         B     CATRC                                                    08870000
         B     CAT16                                                    08880000
         B     CAT20                                                    08890000
         B     CAT24                                                    08900000
         SPACE 2                                                        08910000
CAT4     MVC   MTMSGID,=CL4'C04'  "CATALOG DOES NOT EXIST"              08920000
         B     ISSUEMSG                                                 08930000
         SPACE                                                          08940000
CAT8     LR    R4,R0              SAVE INDEX LEVEL                      08950000
         LTR   R3,R1              DO WE HAVE A "LOCATE" RC ???          08960000
         BNZ   CAT8CVOL           YES - MUST BE A CVOL CATALOG          08970000
         SPACE                                                          08980000
         CH    R4,=H'56'          USER UNAUTHORIZED ?                   08990000
         BNE   CAT8VS             NO - MUST BE VSAM CTLG RC             09000000
         MVC   MTMSGID,=CL4'C08B' "USER UNAUTHORIZED"                   09010000
         B     ISSUEMSG                                                 09020000
         SPACE                                                          09030000
CAT8VS   MVC   MTMSGID,=CL4'C08C' "VSAM CATALOG RETURN CODE"            09040000
         ST    R4,DBLWRD          VSAM CTLG RC                          09050000
         LA    R1,DBLWRD                                                09060000
         ST    R1,MTINSRT         FIRST INSERT                          09070000
         MVI   MTINSRT,X'84'      CNVT TO DEC,LENGTH OF 4               09080000
         B     ISSUEMSG           GO ISSUE MSG                          09090000
         SPACE                                                          09100000
CAT8CVOL MVC   MTMSGID,=CL4'C08'  "OPERATION INCONSISTENT ... "         09110000
         LA    R3,0(,R3)          CLEAR HIGH BYTE                       09120000
         CH    R3,=H'16'                                                09130000
         BH    CAT8LRC                                                  09140000
         LA    R1,B'11'           MASK                                  09150000
         NR    R1,R3              MULTIPLE OF 4 ?                       09160000
         BNZ   CAT8LRC            NOPE                                  09170000
         SPACE                                                          09180000
         L     R1,CAT8LDES(R3)    LOAD PTR ENGLISH DESCRIPTION OF RC    09190000
         ST    R1,MTINSRT         SET FIRST INSERT                      09200000
         SPACE                                                          09210000
CAT8ILVL ST    R4,DBLWRD          INDEX LEVEL                           09220000
         LA    R1,DBLWRD                                                09230000
         ST    R1,MTINSRT+4       SECOND INSERT                         09240000
         MVI   MTINSRT+4,X'84'    CNVT TO DEC,LENGTH OF 4               09250000
         B     ISSUEMSG           GO ISSUE MSG                          09260000
         SPACE                                                          09270000
CAT8LRC  MVC   MTMSGID,=CL4'C08A' 2ND LVL MSG IS "LOCATE RETURN CODE"   09280000
         ST    R3,DBLWRD+4        STORE "LOCATE" TYPE RETURN CODE       09290000
         LA    R1,DBLWRD+4                                              09300000
         ST    R1,MTINSRT                                               09310000
         MVI   MTINSRT,X'84'      CNVT TO DEC,LENGTH OF 4               09320000
         B     CAT8ILVL           GO GET INDEX LEVEL INSERT             09330000
         SPACE                                                          09340000
CAT16    MVC   MTMSGID,=CL4'C16'  "INDEX STRUCTURE DOES NOT EXIST"      09350000
         B     ISSUEMSG                                                 09360000
         SPACE                                                          09370000
CAT20    MVC   MTMSGID,=CL4'C20'  "CATALOG FULL"                        09380000
         B     ISSUEMSG                                                 09390000
         SPACE                                                          09400000
CAT24    MVC   MTMSGID,=CL4'C24'  "CANNOT CATALOG GDG DATASET"          09410000
         B     ISSUEMSG                                                 09420000
         SPACE                                                          09430000
CATRC    MVC   MTMSGID,=CL4'CRC'  "UNRECOVERABLE ERROR"                 09440000
         ST    R15,DBLWRD         CATALOG RC                            09450000
         LA    R1,DBLWRD                                                09460000
         ST    R1,MTINSRT         FIRST INSERT                          09470000
         MVI   MTINSRT,X'84'      CNVT TO DEC,LENGTH OF 4               09480000
         B     ISSUEMSG           GO ISSUE MSG                          09490000
         EJECT                                                          09500000
         SPACE                                                          09510000
BADLOC   LA    LINK,RETURN        SET RETURN ADDR FOR ISSUEMSG          09520000
         CH    R15,=H'4'                                                09530000
         BL    LOCRC                                                    09540000
         BE    CAT4               RC = 4 SAME AS CATALOG                09550000
         CH    R15,=H'16'                                               09560000
         BH    LOCRC                                                    09570000
         LA    R1,B'11'           MASK                                  09580000
         NR    R1,R15             RC MULTIPLE OF 4 ?                    09590000
         BNZ   LOCRC              NOPE                                  09600000
         SPACE 2                                                        09610000
LOCQUAL  MVC   MTMSGID,=CL4'QUAL' "..... - INDEX LEVEL IS ... "         09620000
         L     R1,CAT8LDES(R15)   LOAD PTR ENGLISH DESCRIPTION OF RC    09630000
         ST    R1,MTINSRT         SET FIRST INSERT                      09640000
         ST    R0,DBLWRD          INDEX LEVEL                           09650000
         LA    R1,DBLWRD                                                09660000
         ST    R1,MTINSRT+4       SECOND INSERT                         09670000
         MVI   MTINSRT+4,X'84'    CNVT TO DEC,LENGTH OF 4               09680000
         B     ISSUEMSG           GO ISSUE MSG                          09690000
         SPACE                                                          09700000
LOCRC    MVC   MTMSGID,=CL4'LOC'  "LOCATE RETURN CODE"                  09710000
         ST    R15,DBLWRD         LOCATE RC                             09720000
         LA    R1,DBLWRD                                                09730000
         ST    R1,MTINSRT         FIRST INSERT                          09740000
         MVI   MTINSRT,X'84'      CNVT TO DEC,LENGTH OF 4               09750000
         B     ISSUEMSG           GO ISSUE MSG                          09760000
         EJECT                                                          09770000
BADSCR   LA    LINK,RETURN        SET RETURN ADDR FOR ISSUEMSG          09780000
         CH    R15,=H'4'                                                09790000
         BL    SCRRC                                                    09800000
         CH    R15,=H'8'                                                09810000
         BH    SCRRC                                                    09820000
         B     *(R15)             DETERMINE RETURN CODE                 09830000
         B     SCRR4                                                    09840000
         B     SCRR8                                                    09850000
         SPACE 2                                                        09860000
SCRR4    MVC   MTMSGID,=CL4'V03'  "VOLUME XXXXXX NOT MOUNTED"           09870000
         LA    R1,VOLSER          -> VOLUME                             09880000
         ST    R1,MTINSRT         1ST VARIBLE                           09890000
         MVI   MTINSRT,L'VOLSER   LENGTH                                09900000
         B     ISSUEMSG                                                 09910000
         SPACE                                                          09920000
SCRR8    DS    0H                 STATUS CODE RETURNED IN VOL LIST      09930000
         LA    R1,DSNAME          MOST MSGS HAVE A DSN AS A VARIBLE     09940000
         ST    R1,MTINSRT         1ST VARIBLE                           09950000
         MVC   MTINSRT(1),DSNLEN  LENGTH                                09960000
         SR    R15,R15            CLEAR FOR INSERT                      09970000
         IC    R15,CAMAREA+13     PICK UP SCRATCH STATUS CODE           09980000
         CH    R15,=H'1'          CHECK WITHIN BOUNDS                   09990000
         BL    SCRSC                                                    10000000
         CH    R15,=H'9'                                                10010000
         BH    SCRSC                                                    10020000
         SLL   R15,2              * 4 TO GET BRANCH TABLE OFFSET        10030000
         B     *(R15)             DETERMINE RETURN CODE                 10040000
         B     SCR1                                                     10050000
         B     SCR2                                                     10060000
         B     SCR3                                                     10070000
         B     SCR4                                                     10080000
         B     SCRR4                                                    10090000
         B     SCR6                                                     10100000
         B     SCR7                                                     10110000
         B     SCR8                                                     10120000
         B     SCR9                                                     10130000
         SPACE                                                          10140000
SCR1     MVC   MTMSGID,=CL4'SSC1' "DATASET NOT FOUND"                   10150000
         LA    R1,VOLSER          -> VOLUME                             10160000
         ST    R1,MTINSRT+4       1ST VARIBLE                           10170000
         MVI   MTINSRT+4,L'VOLSER     LENGTH                            10180000
         B     ISSUEMSG                                                 10190000
         SPACE                                                          10200000
SCR2     MVC   MTMSGID,=CL4'SSC2' "INCORRECT PASSWORD SUPPLIED"         10210000
         B     ISSUEMSG                                                 10220000
         SPACE                                                          10230000
SCR3     MVC   MTMSGID,=CL4'SSC3' "DATASET NOT EXPIRED"                 10240000
         B     ISSUEMSG                                                 10250000
         SPACE                                                          10260000
SCR4     MVC   MTMSGID,=CL4'SSC4' "I/O ERROR READING VTOC"              10270000
         B     ISSUEMSG                                                 10280000
         SPACE                                                          10290000
SCR6     MVC   MTMSGID,=CL4'SSC6' "UNABLE TO MOUNT VOLUME"              10300000
         LA    R1,VOLSER          -> VOLUME                             10310000
         ST    R1,MTINSRT         1ST VARIBLE                           10320000
         MVI   MTINSRT,L'VOLSER   LENGTH                                10330000
         B     ISSUEMSG                                                 10340000
         SPACE                                                          10350000
SCR7     MVC   MTMSGID,=CL4'SSC7' "DATASET IN USE"                      10360000
         B     ISSUEMSG                                                 10370000
         SPACE                                                          10380000
SCR8     MVC   MTMSGID,=CL4'SSC8' "USER NOT AUTHORIZED TO SCRATCH DS"   10390000
         B     ISSUEMSG                                                 10400000
         SPACE                                                          10410000
SCR9     MVC   MTMSGID,=CL4'SSC9' "UNABLE TO DELETE DATASET FROM RACF"  10420000
         B     ISSUEMSG                                                 10430000
         SPACE                                                          10440000
SCRSC    MVC   MTMSGID,=CL4'SSC'  "SCRATCH STATUS CODE XX"              10450000
         B     SCRNUMC                                                  10460000
         SPACE                                                          10470000
SCRRC    MVC   MTMSGID,=CL4'SRC'  "SCRATCH RETURN CODE XX"              10480000
SCRNUMC  ST    R15,DBLWRD         SCRATCH RC                            10490000
         LA    R1,DBLWRD                                                10500000
         ST    R1,MTINSRT         FIRST INSERT                          10510000
         MVI   MTINSRT,X'84'      CNVT TO DEC,LENGTH OF 4               10520000
         B     ISSUEMSG           GO ISSUE MSG                          10530000
         EJECT                                                          10540000
BADOBTN  LA    LINK,RETURN        RETURN ADDR FOR ISSUEMSG              10550000
         CH    R15,=H'4'                                                10560000
         BL    OBTNRC                                                   10570000
         CH    R15,=H'12'                                               10580000
         BH    OBTNRC                                                   10590000
         B     *(R15)             DETERMINE RETURN CODE                 10600000
         B     OBTN4      4                                             10610000
         B     OBTN8      8                                             10620000
         B     SCR4      12       "I/O ERROR READING VTOC"              10630000
         SPACE 2                                                        10640000
OBTN4    MVC   MTMSGID,=CL4'V02'  "VOLUME XXXXXX NOT MOUNTED"           10650000
         LA    R1,VOLSER          -> VOLUME                             10660000
         ST    R1,MTINSRT         1ST VARIBLE                           10670000
         MVI   MTINSRT,L'VOLSER   LENGTH                                10680000
         B     ISSUEMSG                                                 10690000
         SPACE                                                          10700000
OBTN8    MVC   MTMSGID,=CL4'SSC1' "DATASET NOT FOUND"                   10710000
         LA    R1,DSNAME          -> DSNAME                             10720000
         ST    R1,MTINSRT         1ST VARIBLE                           10730000
         MVC   MTINSRT(1),DSNLEN  LENGTH                                10740000
         LA    R1,VOLSER          -> VOLUME                             10750000
         ST    R1,MTINSRT+4       1ST VARIBLE                           10760000
         MVI   MTINSRT+4,L'VOLSER     LENGTH                            10770000
         B     ISSUEMSG                                                 10780000
         SPACE                                                          10790000
OBTNRC   MVC   MTMSGID,=CL4'ORC'  "OBTAIN RETURN CODE XX"               10800000
         ST    R15,DBLWRD         OBTAIN RC                             10810000
         LA    R1,DBLWRD                                                10820000
         ST    R1,MTINSRT         FIRST INSERT                          10830000
         MVI   MTINSRT,X'84'      CNVT TO DEC,LENGTH OF 4               10840000
         B     ISSUEMSG           GO ISSUE MSG                          10850000
         EJECT                                                          10860000
GNRLFAIL DS    0H                 CALL GENERAL FAIL                     10870000
         SPACE                                                          10880000
         XC    GFPARMS(GFLENGF),GFPARMS    INIT GENERAL FAIL PARM LIST  10890000
         SPACE                                                          10900000
         ST    R15,GFRCODE        RETURN CODE IN PARM LIST              10910000
         STH   R1,GFCALLID        FAILING ROUTINE ID                    10920000
         L     R1,MTCPPLP         GET CPPL FROM IKJEFF02 PARM LIST      10930000
         ST    R1,GFCPPLP         -> CPPL FOR GENRL FAIL                10940000
         SPACE                                                          10950000
         LA    R1,ECB             -> DUMMY ECB                          10960000
         ST    R1,GFECBP          FOR GENERAL FAIL TO GIVE TO PUTLINE   10970000
         SR    R1,R1                                                    10980000
         ST    R1,ECB             CLEAR ECB                             10990000
         SPACE                                                          11000000
         LA    R1,GFPARMS         -> GENERAL FAIL PARMS                 11010000
         ST    R1,GFPARMP         ADDR LIST FOR LINK                    11020000
         SPACE                                                          11030000
         LINK  EP=IKJEFF19,MF=(E,GFPARMP)  DIAGNOSE RETURN CODE         11040000
         SPACE                                                          11050000
         LTR   R15,R15            GENERAL FAIL EXECUTE OK ?             11060000
         BZ    RETURN                                                   11070000
         SPACE 2                                                        11080000
         ABEND 100,DUMP           WHY ?                                 11090000
         SPACE 3                                                        11100000
         LTORG                                                          11110000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          MESSAGES'       11120000
MSGCSECT CSECT                                                          11130000
         IKJTSMSG (' ',,' CATALOGED'),CAT                               11140000
         SPACE                                                          11150000
         IKJTSMSG (' ',,' UNCATALOGED'),UCAT                            11160000
         SPACE                                                          11170000
         IKJTSMSG (' ',,' SCRATCHED'),SCR                               11180000
         SPACE                                                          11190000
         IKJTSMSG (' ',,' DELETED'),DEL                                 11200000
         SPACE                                                          11210000
         IKJTSMSG (' TOO MANY VOLUMES+'),VX01                           11220000
         IKJTSMSG (' ALL VOLUMES AFTER THE FIRST 20 HAVE BEEN IGNORED')*11230000
               ,V01,VX01                                                11240000
         SPACE                                                          11250000
         IKJTSMSG (' VOLUME ',,' NOT MOUNTED+'),VX02                    11260000
         IKJTSMSG (' YOU MUST SUPPLY THE UNIT TYPE TO CATALOG A DATASET*11270000
                ON A NON-MOUNTED VOLUME'),V02,VX02                      11280000
         SPACE                                                          11290000
         IKJTSMSG (' DASD VOLUME ',,' NOT MOUNTED'),V03                 11300000
         SPACE                                                          11310000
         IKJTSMSG (' VOLUME SERIAL IS REQUIRED'),V04                    11320000
         SPACE                                                          11330000
         IKJTSMSG (' REQUIRED CATALOG DOES NOT EXIST'),C04              11340000
         SPACE                                                          11350000
         IKJTSMSG (' OPERATION INCONSISTENT WITH EXISTING CATALOG STRUC*11360000
               TURE+'),CX08                                             11370000
         IKJTSMSG (' ',,' - INDEX LEVEL IS ',),C08,CX08                 11380000
         IKJTSMSG (' VSAM CATALOG RETURN CODE IS ',),C08C,CX08          11390000
         SPACE                                                          11400000
         IKJTSMSG (' OPERATION CANNOT BE PERFORMED+'),CY08              11410000
         IKJTSMSG (' CATALOG MACRO R15 = 8, R1 = ',,', R0 = ',),C08A,CY*11420000
               08                                                       11430000
         IKJTSMSG (' USER IS NOT AUTHORIZED TO PERFORM THE OPERATION'),*11440000
               C08B,CY08                                                11450000
         SPACE                                                          11460000
         IKJTSMSG (' INDEX STRUCTURE REQUIRED TO CATALOG DATASET DOES N*11470000
               OT EXIST'),C16                                           11480000
         SPACE                                                          11490000
         IKJTSMSG (' CATALOG DATASET IS FULL'),C20                      11500000
         SPACE                                                          11510000
         IKJTSMSG (' UNABLE TO CATALOG GENERATION DATASET+'),CX24       11520000
         IKJTSMSG ('  GENERATION DATASET HAS IMPROPER NAME OR IS TOO OL*11530000
               D TO FIT IN CATALOG'),C24,CX24                           11540000
         SPACE                                                          11550000
         IKJTSMSG (' UNRECOVERABLE ERROR DURING CATALOG OPERATION+'),CX*11560000
               RC                                                       11570000
         IKJTSMSG (' CATALOG MACRO RETURN CODE IS ',),CRC,CXRC          11580000
         SPACE                                                          11590000
         IKJTSMSG (' ',,' NOT ON VOLUME ',),SSC1                        11600000
         SPACE                                                          11610000
         IKJTSMSG (' INCORRECT PASSWORD SUPPLIED'),SSC2                 11620000
         SPACE                                                          11630000
         IKJTSMSG (' ',,' NOT EXPIRED'),SSC3                            11640000
         SPACE                                                          11650000
         IKJTSMSG (' I/O ERROR READING VTOC - NOTIFY SYSTEMS'),SSC4     11660000
         SPACE                                                          11670000
         IKJTSMSG (' UNABLE TO MOUNT VOLUME ',),SSC6                    11680000
         SPACE                                                          11690000
         IKJTSMSG (' ',,' IN USE'),SSC7                                 11700000
         SPACE                                                          11710000
         IKJTSMSG (' USER NOT AUTHORIZED TO SCRATCH DATASET ',),SSC8    11720000
         SPACE                                                          11730000
         IKJTSMSG (' UNABLE TO DELETE DATASET FROM RACF'),SSC9          11740000
         SPACE                                                          11750000
         IKJTSMSG (' SCRATCH STATUS CODE ',),SSC                        11760000
         SPACE                                                          11770000
         IKJTSMSG (' SCRATCH RETURN CODE ',),SRC                        11780000
         SPACE                                                          11790000
         IKJTSMSG (' ',,' - INDEX LEVEL IS ',),QUAL                     11800000
         SPACE                                                          11810000
         IKJTSMSG (' LOCATE RETURN CODE ',),LOC                         11820000
         SPACE                                                          11830000
         IKJTSMSG (' OBTAIN RETURN CODE ',),ORC                         11840000
         SPACE                                                          11850000
         IKJTSMSG                                                       11860000
         SPACE                                                          11870000
CATLG    CSECT                                                          11880000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          EQUATES AND DSE*11890000
               CTS'                                                     11900000
         SPACE                                                          11910000
#VOLS    EQU   20                 MAXIMUM NUMBER OF VOLUMES            *11920000
                                  IF CHANGED THEN CHANGE ERROR MSG TOO  11930000
         SPACE 2                                                        11940000
R0       EQU   0                  WORK REGISTER                         11950000
R1       EQU   1                  WORK REGISTER                         11960000
R2       EQU   2                  WORK REGISTER                         11970000
R3       EQU   3                  WORK REGISTER                         11980000
R4       EQU   4                  WORK REGISTER                         11990000
R5       EQU   5                  WORK REGISTER                         12000000
R6       EQU   6                  WORK REGISTER                         12010000
R7       EQU   7                  WORK REGISTER                         12020000
R8       EQU   8                  WORK REGISTER                         12030000
R9       EQU   9   ) DUAL         WORK REGISTER                         12040000
LINK     EQU   9   ) DEFINITION   LINKAGE REGISTER FOR SUBROUTINES      12050000
R10      EQU   10                 WORK REGISTER                         12060000
R11      EQU   11                 WORK REGISTER                         12070000
R12      EQU   12                 BASE REGISTER FOR CATLG CSECT         12080000
R13      EQU   13                 BASE REGISTER FOR CORE DSECT          12090000
R14      EQU   14                 WORK REGISTER                         12100000
R15      EQU   15                 WORK REGISTER                         12110000
         EJECT                                                          12120000
         IKJCPPL                                                        12130000
CPPLLEN  EQU   *-CPPL                                                   12140000
         SPACE 2                                                        12150000
         IKJPPL                                                         12160000
PPLLEN   EQU   *-PPL                                                    12170000
         EJECT                                                          12180000
         SPACE                                                          12190000
         CVT   DSECT=YES,LIST=NO                                        12200000
         SPACE                                                          12210000
         EJECT                                                          12220000
         SPACE                                                          12230000
         IHALPDE                                                        12240000
         SPACE                                                          12250000
         EJECT                                                          12260000
         SPACE                                                          12270000
UCB      DSECT                                                          12280000
         IEFUCBOB LIST=NO                                               12290000
         TITLE 'CATLG/UNCATLG/SCRATCH FUNCTION          WORK AREA'      12300000
CORE     DSECT                                                          12310000
         DS    18F                                                      12320000
         SPACE                                                          12330000
DBLWRD   DS    D                                                        12340000
@DEVNAME DS    A                  -> DEVNAMET MODULE IN LPA             12350000
         SPACE                                                          12360000
PPLSECT  DS    0F                 PPL AREA                              12370000
         DS    CL(PPLLEN)                                               12380000
ECB      DS    F                  ECB FOR PARSE                         12390000
PARSBACK DS    F                  -> PARSE PDL                          12400000
GFPARMP  DS    F                  -> GENERAL FAIL PARM BLOCK            12410000
         IKJEFFGF                 GENERAL FAIL PARM BLOCK               12420000
         EJECT                                                          12430000
         IKJEFFMT MTNINST=4       IKJEFF02 MSG ISSUER PARM BLOCK        12440000
         EJECT                                                          12450000
CAMLST   DS    F                  FLAG BYTES INDICATING FUNC OF CAMLST  12460000
CAMLSTP2 DS    A                  PARAMETER TWO OF CAMLST               12470000
CAMLSTP3 DS    A                  PARAMETER THREE OF CAMLST             12480000
CAMLSTP4 DS    A                  PARAMETER FOUR OF CAMLST              12490000
         SPACE                                                          12500000
         DS    0D                                                       12510000
CAMAREA  DS    (#VOLS)CL12        VOLUME LIST AREA                      12520000
         SPACE                                                          12530000
         DS    0D                                                       12540000
F1DSCB   DS    CL140              SPACE FOR F1 DSCB                     12550000
         SPACE                                                          12560000
         ORG   CAMAREA                                                  12570000
         DS    CL265              MAKE SURE ENOUGH ROOM FOR LOCATE      12580000
         ORG   ,                  SET LOC CNTR TO HIGHEST USED ADDR     12590000
         SPACE                                                          12600000
DSNLEN   DS    AL1                LENGTH OF DSNAME                      12610000
DSNAME   DS    CL44               DSNAME                                12620000
VOLSER   DS    CL6                VOLSER OF DATASET                     12630000
UNIT     DS    CL8                UNIT TYPE                             12640000
DEVTYPE  DS    XL4                DEVICE TYPE OF ABOVE UNIT             12650000
FLAGS    DS    X                                                        12660000
EPCAT    EQU   X'80'              CATLG   EP                            12670000
EPUCAT   EQU   X'40'              UNCATLG EP                            12680000
EPSCR    EQU   X'20'              SCRATCH EP                            12690000
SVOL     EQU   X'10'              VOLUME SERIAL SUPPLIED  (SCR & CATLG) 12700000
SUNIT    EQU   X'08'              UNIT TYPE SUPPLIED      (SCR & CATLG) 12710000
SCRPURGE EQU   X'04'              PURGE SPECIFIED         (SCRATCH)     12720000
QUIT     EQU   X'02'              ERROR DURING PARM VERIFICATION        12730000
SKIPOBTN EQU   X'01'              DONT ISSUE OBTAIN TO VERIFY DS EXISTS 12740000
         SPACE ,                                               10/16/90 12750000
IOSVSWRK DC    100X'00'           IOSVSUCB WORKAREA            10/16/90 12760000
IOSVSPRM DC    A(IOSVSWRK)         )                           10/16/90 12770000
         DC    A(IOSVSDEV)         ) MUST BE KEPT TOGETHER     10/16/90 12780000
         DC    X'80',AL3(IOSVSUCB) )                           10/16/90 12790000
IOSVSUCB DS    A                  UCB ADDRESS                  10/16/90 12800000
IOSVSDEV DC    AL1(UCB3DACC)      DASD DEVICE CLASS            10/16/90 12810000
         SPACE ,                                               11/16/90 12820000
UVSRSAVE DS    18F                IEFEB4UV SAVE AREA           11/16/90 12830000
UVSRPARM DS    2F                 IEFEB4UV PARAMETER LIST      11/16/90 12840000
UVSRUTAB DS    4F                 IEFEB4UV UNIT TABLE          11/16/90 12850000
UVSRATTR DS    CL10               IEFEB4UV ATTRIBUTE AREA      11/16/90 12860000
UVSRFLGS DS    CL2                IEFEB4UV FLAGS               11/16/90 12870000
         SPACE                                                          12880000
         DS    0D                 ROUND UP TO DOUBLE WORD BOUNDRY       12890000
         SPACE                                                          12900000
CORESIZE EQU   *-CORE                                                   12910000
         SPACE 3                                                        12920000
VOLLIST  DSECT                    FORMAT FOR A VOLUME ENTRY ON CATALOG  12930000
DEVCODE  DS    XL4                UCB DEVICE TYPE FIELD                 12940000
CATVOL   DS    CL6                SERIAL OF A VOLUME                    12950000
DSEQNUM  DS    H                  DATASET SEQUENCE NUMBER               12960000
NEXTVOL  EQU   *                  NEXT VOLUME ENTRY                     12970000
         END                                                            12980000
/*                                                                      12990000
//LKED.SYSIN DD *                                                       13000000
  ENTRY CATLG                                                           13010000
  ENTRY UNCATLG                                                         13020000
  ENTRY UNCAT                                                           13030000
  ENTRY SCRATCH                                                         13040000
  ALIAS UNCATLG                                                         13050000
  ALIAS UNCAT                                                           13060000
  ALIAS SCRATCH                                                         13070000
  ALIAS SCR                                                             13080000
  NAME  CATLG(R)                                                        13090000
/*                                                                      13100000
//                                                                      13110000
