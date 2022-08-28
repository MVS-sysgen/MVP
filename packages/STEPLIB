//STEPLIB JOB (JOB),
//             'INSTALL STEPLIB',
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
STEPLIB  TITLE '*****  S T E P L I B   C O M M A N D   P R O C E S S O X00010000
               R  *****'                                                00020000
*********************************************************************   00030000
*                                                                   *   00040000
*   THIS IS A TSO COMMAND PROCESSOR THAT WILL ALLOW CONCATENATION   *   00050000
*   OF LOAD LIBRARIES AS THE 'STEPLIB' IN A TSO SESSION.            *   00060000
*                                                                   *   00070000
*   THE PROGRAM MUST RESIDE IN A LINKLIST LIBRARY AND BE LINKED     *   00080000
*   AC(1), I.E., AUTHORIZED.                                        *   00090000
*                                                                   *   00100000
*   THE COMMAND IS EXECUTED AS FOLLOWS :                            *   00110000
*     ON THE TSO COMMAND LINE ENTER :                               *   00120000
*                                                                   *   00130000
*        STEPLIB DA(DSN1,DSN2,DSN3...) SHR/OLD                      *   00140000
*    WHERE :                                                        *   00150000
*       DSN1/2/3 IS FULLY QUALIFIED AND IN QUOTES IF NOT BELONGING  *   00160000
*       TO THE USER WHOSE SESSION IS EXECUTING, E.G.,               *   00170000
*                                                                   *   00180000
*             STEPLIB ('EXPL.PHASEII') SHR     - OR -               *   00190000
*                                                                   *   00200000
*       SIMPLY THE 2ND, 3RD, ETC. LEVEL QUALIFIERS IF BELONGING TO  *   00210000
*       THE USER WHOSE SESSION IS EXECUTING, E.G.,                  *   00220000
*                                                                   *   00230000
*             STEPLIB (JOBS.LOAD) SHR                               *   00240000
*                                                                   *   00250000
*    UP TO SIX 'LOAD' DATA SETS MAY BE INCLUDED IN THE CONCATEN-    *   00260000
*    ATION, EACH SEPARATED BY COMMAS. IF THE DISPOSITION IS NOT     *   00270000
*    INCLUDED, (I.E., OLD OR SHR), SHR IS ASSUMED.                  *   00280000
*                                                                   *   00290000
*    AN EXAMPLE OF MULTIPLE D.S. CONCATENATION IS :                 *   00300000
*                                                                   *   00310000
*         STEPLIB DA('EXPL.PHASEII',JOBS.LOAD,LINK.LOAD) SHR        *   00320000
*                                                                   *   00330000
*    TO DELETE THE 'STEPLIB' CONCATENATIONS SIMPLY ENTER :          *   00340000
*                                                                   *   00350000
*         STEPLIB                                                   *   00360000
*                                                                   *   00370000
*    ON THE COMMAND LINE.                                           *   00380000
*                                                                   *   00390000
*                                                                   *   00400000
*    THIS PROGRAM WAS WRITTEN BY :                                  *   00410000
*                                                                   *   00420000
*       JIM WHEATON                                                 *   00430000
*       EDPC - ROOM 249 SOUTH TOWER                                 *   00440000
*       PHONE : 965-7529                                            *   00450000
*                                                                   *   00460000
*    PLEASE DIRECT ANY COMMENTS, PROBLEMS, ETC. TO JIM.             *   00470000
*                                                                   *   00480000
*********************************************************************   00490000
*                                                                   *   00500000
*    NEW GOODIES:                                                   *   00510000
*                                                                   *   00520000
*      STEPLIB  LIST      WILL LIST CURRENT AND OLD LIBRARIES       *   00530000
*                                                                   *   00540000
*      STEPLIB (DS...) APF       WILL AUTHORIZE THE NEW             *   00550000
*                                CONCATENATION                      *   00560000
*                                                                   *   00570000
*      STEPLIB (DS...) APPEND    WILL CONCATENATE EXISTING SYSTEM   *   00580000
*                                STEPLIB/JOBLIB DATA AFTER USER'S   *   00590000
*                                                                   *   00600000
*      STEPLIB (DS...) LIST      WILL LIST CURRENT AND OLD ENTRIES  *   00610000
*                                AFTER ALLOCATION.                  *   00620000
*                                                                   *   00630000
*    TO AVOID CONFLICTS WITH THE OLD VERSION OF STEPLIB, THE NEW    *   00640000
*    CODE USES A DDNAME PREFIX OF  STEPDYN                          *   00650000
*                                                                   *   00660000
*    On systems with resource access control active the APF or AUTH *   00662000
*    parameter will work only if the user has read access to        *   00664000
*    profile STEPAUTH in the FACILITY class.                        *   00666000
*                                                                   *   00668000
*********************************************************************   00670000
*                                                                   *   00680000
*    MAINTENANCE/MODIFICATIONS:                                     *   00690000
*                                                                   *   00700000
*    1998/12/10   GYP     MAPPING MACROS, EFFICIENCY CHANGES.       *   00710000
*                         SUPPORT FOR EXISTING STEPLIB.             *   00720000
*                         UPDATED TO RUN IN 31-BIT SYSTEMS (TIOT    *   00730000
*                         ABOVE THE LINE; SWA TOKENS, ETC.)         *   00740000
*                         I COULD NOT GET RESET TO FREE THE STEPDYN *   00750000
*                         ALLOCATIONS, SO I ADDED EXPLICIT UNALLOC  *   00760000
*                         CODE TO THE IRB EXIT.                     *   00770000
*                         BASE REGISTERS REARRANGED TO MAKE FOR     *   00780000
*    THROUGH 1998/12/16   EASIER MAINTENANCE.                       *   00790000
*    1999/01/12   GYP     FIXED ADDRESSING ERROR                    *   00800000
*    2006/11/23   GYP     CHANGED TO SUPPORT MVS 3.8J. FOR LATER    *   00810000
*                         SYSTEMS, SET GBLB &MVSXA TO 1             *   00820000
*    2012/04/04   JW      if RAC is active check for read access to *   00822000
*                         profile STEPAUTH in the FACILITY class    *   00824000
*                         before acting upon the APF/AUTH parameter *   00826000
*********************************************************************   00830000
         SPACE 1                                                        00840000
         MACRO ,                                                        00850000
&NM      WORKAREA &PFX=PAT                                              00860000
.*   THIS AREA IS EXPANDED ONCE INLINE TO PRODUCE DC DATA, AND          00870000
.*   ONE MORE TIME IN THE DSECT FOR RE-ENTRANCY                         00880000
         LCLA  &I,&J                                            GP98348 00890000
         LCLC  &P            SHORT NAME FOR PREFIX                      00900000
         LCLC  &C            CHARACTER                          GP98348 00910000
         GBLA  &MAXCAT       MAXIMUM CONCATENATION COUNT        GP98348 00920000
&P       SETC  '&PFX'                                                   00930000
&NM      DS    0D                                                       00940000
&P.STPRQ    DC A(&P.REQCT+X'80000000')                                  00950000
&P.REQCT    DC AL1(S99RBEND-S99RB,S99VRBAL,S99NOMNT,0)          GP98349 00960000
&P.STPER    DC F'0'                                                     00970000
&P.STPPA    DC A(&P.REQPS)                                              00980000
         DC    2F'0'                                                    00990000
         SPACE 1                                                        01000000
&P.REQPS    DC A(&P.RQDDN)                                              01010000
&P.REQDA    DC A(&P.RQDSN)                                              01020000
&P.RQDPA    DC A(&P.RQDSP+X'80000000')                                  01030000
         SPACE 1                                                        01040000
&P.RQDDN    DC Y(DALDDNAM,1,L'&P.DDNAM)   DDNAME = STEPDYN      GP98349 01050000
&P.DDNAM    DC C'STEPDYN '                                              01060000
&P.RQDSN    DC Y(DALDSNAM,1,L'&P.DSNAM)   DSNAME OF STEPDYN     GP98349 01070000
&P.DSNAM    DC CL44' '                                                  01080000
&P.RQFRE    DC Y(DALCLOSE,0)              CLOSE = FREE          GP98349 01090000
&P.RQDSP    DC Y(DALSTATS,1,L'&P.DSPFG)     DISP=SHR            GP98349 01100000
&P.DSPFG    DC X'08'                      DISP=SHR              GP98349 01110000
         SPACE 1                                                        01120000
         DS    0D                                                       01130000
&P.CONCT    DC A(&P.CONCL+X'80000000')                                  01140000
&P.CONCL    DC AL1(S99RBEND-S99RB,S99VRBCC,S99NOMNT,0)          GP98349 01150000
&P.CNCER    DC F'0'                                                     01160000
&P.CNCAT    DC A(&P.CNCPT)                                              01170000
         DC    2F'0'                                                    01180000
         SPACE 1                                                        01190000
&P.CNCPT    DC A(&P.CNCIT+X'80000000')                                  01200000
         SPACE 1                                                        01210000
&P.CNCIT    DC Y(DCCDDNAM)         REQUIRED - DDNAM LIST        GP98349 01220000
&P.NUMCT    DC AL2(&MAXCAT)        COUNT OF CONCAT D.S.         GP98348 01230000
         DC    AL2(L'&P.BASDD)     LENGTH OF 1ST                GP98345 01240000
&P.BASDD    DC CL8'STEPDYN'        1ST CONCATENATION            GP98345 01250000
         DC    AL2(L'&P.BASD1)     LENGTH OF 2ND                GP98345 01260000
&P.BASD1    DC CL8'STEPDYN1'       2ND CONCATENATION            GP98345 01270000
&J       SETA  &MAXCAT                                          GP98348 01280000
&I       SETA  2                                                GP98348 01290000
.CONLOOP AIF   (&I GE &J).CONDONE                               GP98348 01300000
&I       SETA  &I+1                                             GP98348 01310000
&C SETC ' 123456789ABCDEFGHIJKLMNOPQRSTUVWZ@#$'(&I,1)           GP98348 01320000
         DC    AL2(8),CL8'STEPDYN&C'                            GP98348 01330000
         AGO   .CONLOOP                                         GP98348 01340000
.CONDONE ANOP  ,                                                GP98348 01350000
&P.DATND EQU   *                                                        01360000
         MEND  ,                                                        01370000
         MACRO ,                                                        01380000
&NM      MSGCODE &SAVE=12(R13)                                  GP98344 01390000
*********************************************************************** 01400000
**                                                                   ** 01410000
**  MAKEMSG :  BUILD WTO MESSAGE TEXT.                               ** 01420000
**             MSG ADDRESS IN R0 (BUILT BY MSGDEF MACRO)             ** 01430000
**            NEWWFLAG: NEWWTPUT - ISSUE AS TPUT (IF TS SESSION)     ** 01440000
**                      NEWWWTO  - ISSUE MESSAGE AS WTO              ** 01450000
**                      NEWWONCE - DON'T ISUE WTO IF TPUT WAS DONE   ** 01460000
**                      NEWWONLY - FORMAT WTO TEXT ONLY              ** 01470000
**                      NEWWHAVE - WRITE (PRE)FORMATTED MESSAGE      ** 01480000
**                                                                   ** 01490000
**    RETURN TO R14, WITH 0 (BAD LENGTH) OR NEW MSG TEXT IN R0       ** 01500000
**                                                                   ** 01510000
*********************************************************************** 01520000
         SPACE 1                                                        01530000
         PUSH  USING                                                    01540000
         AIF   ('&NM' EQ '').NOLBL                                      01550000
&NM      DS    0H                                                       01560000
.NOLBL   ANOP  ,                                                        01570000
MAKEMSG  STM   R14,R4,&SAVE     PARSIMONY                               01580000
         TM    NEWWFLAG,NEWWHAVE  REQUEST TO DO PREFORMATTED MESSAGE?   01590000
         BNZ   MAKEMUSE      YES                                        01600000
         LR    R4,R0         COPY MESSAGE POINTER                       01610000
         USING MSGDEFDS,R4   DECLARE MAPPING (I'M LAZY)                 01620000
         SR    R15,R15       CLEAR FOR IC                               01630000
         LR    R1,R15        CLEAR FOR MAKEMESS RETURN                  01640000
         ICM   R15,1,MSDLEN  GET TEXT LENGTH                            01650000
         BNP   MAKEMESS      NON-POSITIVE - IGNORE REQUEST              01660000
         MVC   NEWWTO(4+MSGPFX),MSGPAT MOVE PATTER NTEXT                01670000
         LA    R0,L'NEWTXT   GET MAXIMUM LENGTH                         01680000
         CR    R15,R0        LEGAL?                                     01690000
         BNH   MAKEMSGM                                                 01700000
         LR    R15,R0        TRUNCATE TEXT - NO WARNING                 01710000
MAKEMSGM BCTR  R15,0         MAKE EX LENGTH                             01720000
         EX    R15,EXMVCWTO  MOVE TEXT                                  01730000
         LA    R15,1+4+MSGPFX(,R15)  TOTAL LENGTH OF WTO                01740000
         STH   R15,NEWWTO    STASH IN REQUEST                           01750000
         LA    R1,NEWWTO(R15) POINT TO FIRST BYTE AFTER                 01760000
         MVC   0(L'MSGDESC,R1),MSGDESC  MOVE DESCRIPTOR AND ROUTING     01770000
         IC    R15,MSDNUM    GET MESSAGE NUMBER                         01780000
         CVD   R15,NEWDB     CONVERT TO PACKED FORM                     01790000
         OI    NEWDB+7,X'0F' MAKE POSITIVELY PLUS                       01800000
         UNPK  NEWHEAD,NEWDB MOVE TO MESSAGE                            01810000
         MVC   NEWHEAD+L'NEWHEAD(1),MSDFLG  APPEND MESSAGE SEVERITY     01820000
*   NOW WE HAVE A WTO FORMAT MESSAGE. SEE WHAT THE USER WANTS TO DO     01830000
*     WITH IT.                                                          01840000
MAKEMUSE TM    NEWWFLAG,255-NEWWHAVE-NEWWONCE ANY REQUEST FOR OUTPUT?   01850000
         BNZ   MAKEMOUT      YES; PROCEED                               01860000
         MVC   NEWWFLAG,MSGPFLAG  RESTORE DEFAULT FLAGS                 01870000
MAKEMOUT TM    NEWWFLAG,NEWWTPUT ISSUE TPUT?                            01880000
         BZ    MAKEMPUT      NO                                         01890000
         L     R15,PSAAOLD-PSA  GET CURRENT ASCB                        01900000
         ICM   R15,15,ASCBTSB-ASCB(R15)  ANY TSB ?                      01910000
         BZ    MAKEMPUT      NO                                         01920000
         LH    R2,NEWWTO     RELOAD LENGTH                              01930000
         SH    R2,=H'4'      ADJUST                                     01940000
         TPUT  NEWWTO+4,(R2) ISSUE                                      01950000
         LTR   R15,R15       SUCCESSFUL ?                               01960000
         BP    MAKEMPUT      NO ?                                       01970000
         TM    NEWWFLAG,NEWWONCE STOP ON FIRST MESSAGE?                 01980000
         BNZ   MAKEMEXT                                                 01990000
MAKEMPUT TM    NEWWFLAG,NEWWWTO ISSUE WTO ?                             02000000
         BZ    MAKEMEXT      NO                                         02010000
         WTO   MF=(E,NEWWTO) WRITE TO OPSIE AND JOB LOG                 02020000
MAKEMEXT LA    R1,NEWWTO     POINT TO NEW MESSAGE                       02030000
MAKEMESS ST    R1,2*4+&SAVE  RETURN MESSAGE ADDRESS IN R0               02040000
         MVC   NEWWFLAG,MSGPFLAG  RESTORE DEFAULT FLAGS                 02050000
         LM    R14,R4,&SAVE     RELOAD REGISTERS                        02060000
         BR    R14           RETURN TO CALLER                           02070000
EXMVCWTO MVC   NEWTXT(0),MSDTXT  MOVE CALLER TEXT                       02080000
         POP   USING                                                    02090000
         MEND  ,                                                        02100000
         MACRO ,                                                        02110000
&NM      MSGWORK &TYPE,&MOD=HDTXX,&MODNM=' ',&ROUTCDE=7,&DESC=4,&FLAG=(*02120000
               NEWWTPUT+NEWWWTO+NEWWONCE)                               02130000
.*    DEFINE WORK AREA(S) FOR MSGCODE SUBROUTINE                        02140000
.*    TYPE FIELD IS ONE OR MORE OF:                                     02150000
.*       PATTERN       EXPAND PATTERN CODE                              02160000
.*              THIS SHOULD HAVE MOD, MODNM, ETC. KEYWORDS SET          02170000
.*       DATA          EXPAND WORK AREA                                 02180000
.*       DSECT         EXPAND MSGDEF MAPPING                            02190000
.*                                                                      02200000
.*    ADDED IN SUPPORT OF HDT MESSAGING. ALSO SEE MSGDEF AND MSGCODE    02210000
.*    MACROS, AND PROGRAM HDTT085 FOR SAMPLE USE                GP98343 02220000
.*                                                                      02230000
         LCLA  &I,&J,&K                                                 02240000
         LCLB  &P1,&D1,&S1                                              02250000
         LCLC  &C,&MB,&STR,&CODE(16)                            GP06327 02260000
&I       SETA  1                                                        02270000
&K       SETA  N'&SYSLIST                                               02280000
.LOOP    AIF   (&I GT &K).DONE                                          02290000
&C       SETC  '&SYSLIST(&I)'                                           02300000
&I       SETA  &I+1                                                     02310000
         AIF   ('&C' EQ 'PATTERN' OR '&C' EQ 'PAT').DOPAT               02320000
         AIF   ('&C' EQ 'DATA' OR '&C' EQ 'DAT').DODAT                  02330000
         AIF   ('&C' EQ 'DSECT' OR '&C' EQ 'DS').DODSECT                02340000
 MNOTE 4,'UNRECOGNIZED PARAMETER : &C '                                 02350000
 MNOTE 4,'USE ONE OR MORE OF : PATTERN, DATA, DSECT'                    02360000
         MEXIT ,                                                        02370000
.DOPAT   AIF   (&P1).LOOP                                               02380000
&P1      SETB  1                                                        02390000
&MB      SETC  '0000'                                                   02400000
         AIF   ('&ROUTCDE' EQ '' AND '&DESC' EQ '').NORTDS              02410000
&MB      SETC  '8000'        ROUTING/DESCRIPTOR PRESENT                 02420000
.NORTDS  ANOP  ,                                                        02430000
MSGPAT   DC    AL2(MSGDESC-*),X'&MB'  WTO LEN; ROUT/DESC FG             02440000
         AIF   ('&MOD'(1,1) EQ '''').MODSTR                             02450000
&J       SETA  K'&MOD                                                   02460000
MSGPATT  DC    CL(&J)'&MOD '                                            02470000
         AGO   .MODCOM                                                  02480000
.MODSTR  ANOP  ,                                                        02490000
MSGPATT  DC    C&MOD                                                    02500000
.MODCOM  ANOP  ,                                                        02510000
MSGPAT#  DC    C'00E'        MESSAGE NUMBER + FLAG                      02520000
         AIF   ('&MODNM'(1,1) EQ '''').NAMSTR                           02530000
&J       SETA  K'&MODNM                                                 02540000
MSGPATS  DC    CL(&J)'&MODNM '                                          02550000
         AGO   .NAMCOM                                                  02560000
.NAMSTR  ANOP  ,                                                        02570000
MSGPATS  DC    C&MODNM                                                  02580000
.NAMCOM  ANOP  ,                                                        02590000
&CODE(1)  SETC  '8000'                                                  02600000
&CODE(2)  SETC  '4000'                                                  02610000
&CODE(3)  SETC  '2000'                                                  02620000
&CODE(4)  SETC  '1000'                                                  02630000
&CODE(5)  SETC  '0800'                                                  02640000
&CODE(6)  SETC  '0400'                                                  02650000
&CODE(7)  SETC  '0200'                                                  02660000
&CODE(8)  SETC  '0100'                                                  02670000
&CODE(9)  SETC  '0080'                                                  02680000
&CODE(10) SETC  '0040'                                                  02690000
&CODE(11) SETC  '0020'                                                  02700000
&CODE(12) SETC  '0010'                                                  02710000
&CODE(13) SETC  '0008'                                                  02720000
&CODE(14) SETC  '0004'                                                  02730000
&CODE(15) SETC  '0002'                                                  02740000
&CODE(16) SETC  '0001'                                                  02750000
&MB      SETC  '0000'                                                   02760000
&STR     SETC  '0000'                                                   02770000
         AIF   ('&DESC' EQ '').NODESC                                   02780000
&MB      SETC  '&CODE(&DESC)'                                           02790000
.NODESC  AIF   ('&ROUTCDE' EQ '').NOROUT                                02800000
&STR     SETC  '&CODE(&ROUTCDE)'                                        02810000
.NOROUT  ANOP  ,                                                        02820000
MSGDESC  DC    X'&STR,&MB'      ROUT/DESC FLAGS                         02830000
MSGPFX   EQU   MSGDESC-MSGPATT  LENGTH OF HDTT TO : PREFIX              02840000
MSG#HD   EQU   MSGPAT#-MSGPATT  LENGTH TO NUMBER                        02850000
MSGPFLAG DC    AL1(&FLAG)    DEFAULT PROCESSING FLAGS                   02860000
         AGO   .LOOP                                                    02870000
.DODAT   AIF   (&D1).LOOP                                               02880000
&D1      SETB  1                                                        02890000
NEWDB    DC    D'0'          WORK                                       02900000
NEWWTO   DC    Y(128-128,0)  1/4   WTO HEADER - LENGTH & MCSFLAGS       02910000
NEWPFX   DC    CL(MSGPFX)' ' 2/4   VARIABLE LENGTH TEXT                 02920000
NEWTXT   DC    CL(128-MSGPFX)' '  3/4   VARIABLE LENGTH TEXT            02930000
NEWRTDS  DC    XL4'0'        4/4   VARIABLE INSERTION - DESC/ROUT CODES 02940000
NEWHEAD  EQU   NEWPFX+MSG#HD,2,C'C'  MSG NUMBER (XX AFTER HDTT5)        02950000
NEWWFLAG DC    AL1(&FLAG)    TPUT/WTO FLAGS                             02960000
NEWWTPUT EQU   X'80'         ISSUE TPUT IF TS SESSION                   02970000
NEWWWTO  EQU   X'40'         ISSUE WTO                                  02980000
NEWWHAVE EQU   X'04'         MESSAGE PREFORMATTED (NO R0)               02990000
NEWWONLY EQU   X'02'         FORMAT; DO NOT OUTPUT                      03000000
NEWWONCE EQU   X'01'         ONLY ISSUE MESSAGE TO ONE DESTINATION      03010000
         AGO   .LOOP                                                    03020000
.DODSECT AIF   (&S1).LOOP                                               03030000
&S1      SETB  1                                                        03040000
MSGDEFDS DSECT ,             DEFINE MESSAGE                             03050000
MSDLEN   DS    X             LENGTH OF USER TEXT                        03060000
MSDNUM   DS    X             MESSAGE NUMBER                             03070000
MSDFLG   DS    C             ACTION FLAG (A, D, E, W, I)                03080000
MSDTXT   DS    0C            VARIABLE LENGTH TEXT                       03090000
MSDOFF   EQU   MSDTXT-MSDLEN   OFFSET TO TEXT                           03100000
         AGO   .LOOP                                                    03110000
.DONE    AIF   (&P1 OR &D1 OR &S1).MEND                                 03120000
  MNOTE 0,'NO CODE EXPANDED. ???'                                       03130000
.MEND    MEND  ,                                                        03140000
         MACRO ,                                                        03150000
&NM      MSGDEF &NUM,&TEXT,&FLAG=E                                      03160000
.*  ADDED FOR DEFINING REPLACEABLE PORTION OF HDT MESSAGE       GP98343 03170000
.*  USED WITH MSGWORK AND MSGCODE MACROS (SEE HDTT085 FOR EXAMPLE)      03180000
         GBLA  &ZZ@MSG                                                  03190000
         LCLA  &NDX                                                     03200000
         LCLA  &I,&K,&L,&M                                              03210000
&K       SETA  K'&TEXT                                                  03220000
&NDX     SETA  &SYSNDX                                                  03230000
         AIF   (T'&NUM EQ 'O').MISSNUM                                  03240000
         AIF   (T'&TEXT NE 'O').HAVETEX                                 03250000
         MNOTE 8,'MESSAGE TEXT IS MISSING'                              03260000
         MEXIT ,                                                        03270000
.MISSNUM MNOTE 4,'MESSAGE NUMBER MISSING; ASSIGNING &ZZ@MSG'            03280000
&ZZ@MSG  SETA  &ZZ@MSG+1                                                03290000
.HAVETEX AIF   ('&FLAG' EQ 'E').HAVEFLG                                 03300000
         AIF   ('&FLAG' EQ 'W').HAVEFLG                                 03310000
         AIF   ('&FLAG' EQ 'I').HAVEFLG                                 03320000
         AIF   ('&FLAG' EQ 'A').HAVEFLG                                 03330000
         AIF   ('&FLAG' EQ 'D').HAVEFLG                                 03340000
         MNOTE 4,'FLAG NOT RECOGNIZED; EXPECTING E, W, I, A, OR D'      03350000
.HAVEFLG AIF   ('&TEXT'(1,1) EQ '''').QUOTED                            03360000
&NM      DC    AL1(&K,&NUM),CL1'&FLAG ',CL(&K)'&TEXT '                  03370000
         MEXIT ,                                                        03380000
.QUOTED  ANOP  ,                                                        03390000
&K       SETA  &K-2          ALLOW FOR FRAMING QUOTES                   03400000
&I       SETA  2                                                        03410000
&M       SETA  &K            LAST COMPARE POSITION                      03420000
.QUOLOOP AIF   ('&TEXT'(&I,2) EQ '''''').LESS                           03430000
         AIF   ('&TEXT'(&I,2) NE '&&').BUMP                             03440000
.LESS    ANOP  ,                                                        03450000
&K       SETA  &K-1                                                     03460000
.BUMP    ANOP  ,                                                        03470000
&I       SETA  &I+1                                                     03480000
         AIF   (&I LE &M).QUOLOOP  LOOK FOR ANOTHER                     03490000
&NM      DC    AL1(&K,&NUM),CL1'&FLAG ',CL(&K)&TEXT                     03500000
         MEND  ,                                                        03510000
         EJECT ,                                                        03520000
         PUNCH ' ORDER STEPLIB(P) '   MAKE DEBUGGING EASIER     GP98348 03530000
         PUNCH '  SETCODE AC(1) '    USELESS OTHERWISE          GP04234 03540000
         GBLA  &MAXCAT       MAXIMUM DATA SETS TO BE PROCESSED  GP98348 03550000
         GBLB  &MVSXA        SET 1 IF XA, ESA, OR LATER SYSTEM  GP06327 03560000
&MAXCAT  SETA  6             2-39                               GP98348 03570000
         PRINT NOGEN                                            GP98350 03580000
         AIF   (&MVSXA).NEWSECT                                 GP06333 03590000
STEPLIB  START 0                                                GP06333 03600000
         AGO   .COMSECT                                         GP06333 03610000
.NEWSECT ANOP  ,                                                GP06333 03620000
STEPLIB  RSECT ,             CATCH IBM MACRO (AND MY?) PROBLEMS GP98344 03630000
STEPLIB  AMODE 31                                                       03640000
STEPLIB  RMODE 24                                                       03650000
.COMSECT B     BEGIN-STEPLIB(,R15)     BRANCH AROUND ID         GP06333 03660000
         DC    AL1(17),C'STEPLIB  &SYSDATE'                     GP06333 03670000
BEGIN    STM   R14,R12,12(R13)                                          03680000
         LA    R10,0(,R15)   COPY AND CLEAR BASE REGISTER       GP98349 03690000
         LA    R11,2048(,R10)  MAKE MAINTENACE                  GP98349 03700000
         LA    R11,2048(,R11)    A BIT EASIER                   GP98349 03710000
         USING STEPLIB,R10,R11                                  GP98349 03720000
         LR    R7,R1         PRESERVE THE PARM POINTER          GP98344 03730000
         SPACE 1                                                GP98348 03740000
*********************************************************************** 03750000
*                                                                     * 03760000
*    THIS SECTION OF CODE DOES THE GETMAIN(IN S.P. 0) FOR THE DATA    * 03770000
*    AREA AND INITIALIZES THE POINTERS FOR THE DYNAMIC ALLOCATION.    * 03780000
*    ALL OF THIS GARBAGE IS FOR RE-ENTRANCY.                          * 03790000
*                                                                     * 03800000
*********************************************************************** 03810000
         SPACE 1                                                        03820000
         LA    R3,LDWSIZE    LOAD REQUIRED STORAGE SIZE                 03830000
         GETMAIN  R,LV=(R3)  SUBPOOL 0 (252)                            03840000
         SR    R5,R5         FROM - USE ZERO FILL                       03850000
         LR    R2,R1         COPY OVER THE START ADDRESS                03860000
         MVCL  R2,R4         CLEAR NEW STORAGE                          03870000
         ST    R13,4(,R1)                                               03880000
         ST    R1,8(,R13)                                               03890000
         LA    R13,FAKESAVE-LDWDSECT(,R1)  SKIP OVER REAL SAVE  GP06327 03900000
         USING FAKESAVE,R13                                     GP06327 03910000
         LA    R4,PATTERN    GET PATTERN DATA                           03920000
         LA    R2,DYNWORK    POINT TO THE DYNAMIC WORK AREA             03930000
         LA    R3,DATND-DYNWORK     GET THE LENGTH OF THE AREA          03940000
         LR    R5,R3                 .. TWICE ..                        03950000
         MVCL  R2,R4                 ... MOVE DATA AREA TO GOTTEN AREA  03960000
         LA    R2,REQCT            ... SET                              03970000
         ST    R2,STPRQ            ...  UP                      GP98345 03980000
         OI    STPRQ,X'80'                                      GP98345 03990000
         LA    R2,REQPS            ...   THE                            04000000
         ST    R2,STPPA            ...    NEWLY                         04010000
         LA    R2,RQDDN            ...     GOTTEN                       04020000
         ST    R2,REQPS            ...      AREA                        04030000
         LA    R2,RQDSN            ...       WITH                       04040000
         ST    R2,REQDA            ...        THE                       04050000
         LA    R2,RQDSP            ...         DYNAMIC                  04060000
         ST    R2,RQDPA            ...          ALLOC           GP98345 04070000
         OI    RQDPA,X'80'                                      GP98345 04080000
         LA    R2,CONCL            ...           CALLS                  04090000
         ST    R2,CONCT            ...       D                  GP98345 04100000
         OI    CONCT,X'80'                                      GP98345 04110000
         LA    R2,CNCPT            ...        O                         04120000
         ST    R2,CNCAT            ...         I                        04130000
         LA    R2,CNCIT            ...          T                       04140000
         ST    R2,CNCPT            ...                          GP98345 04150000
         OI    CNCPT,X'80'                                      GP98345 04160000
         MVI   PARMWORD+L'PARMWORD,C' '  MAKE PARSE STOPPER     GP98348 04170000
         SPACE 1                                                GP98345 04180000
*********************************************************************** 04190000
*    GET AND SAVE COMMON ADDRESSES                                    * 04200000
*********************************************************************** 04210000
         L     R1,PSATOLD-PSA      ... CVT                      GP98345 04220000
         ST    R1,@TCB             ... SAVE MY TCB ADDRESS      GP98345 04230000
         L     R2,TCBTIO-TCB(,R1)  ... TIOT POINTER             GP98345 04240000
         ST    R2,@TIOT            ... SAVE TIOT ADDRESS        GP98345 04250000
         L     R2,TCBJSTCB-TCB(,R1)                             GP98345 04260000
         ST    R2,@JSTCB                                        GP98345 04270000
         SPACE 2                                                GP98348 04280000
*********************************************************************** 04290000
*    EXAMINE THE INPUT PARAMETER OR COMMAND LINE TEXT                 * 04300000
*********************************************************************** 04310000
         SPACE 1                                                GP98348 04320000
         L     R5,0(,R7)           GET THE PARM POINTER         GP98344 04330000
         LTR   R5,R5         ONLY PARAMETER IN LIST?            GP98344 04340000
         BNM   HAVECMD       NO; ASSUME WE WERE INVOKED AS A COMMAND    04350000
         LH    R4,0(,R5)     GET PARM LENGTH                    GP98344 04360000
         LTR   R4,R4         ANY ?                              GP98344 04370000
         BNP   RESETPAR      NO; RESET REQUEST                  GP98344 04380000
         CLI   2(R5),0       IS THIS NEW OR OLD FORMAT TSO?     GP98349 04390000
         BE    HAVECMD       NEW; BUFFER SAME AS CMD ENTRY      GP98349 04400000
         LA    R15,2(,R5)    POINT TO TEXT                      GP98344 04410000
         B     COMPARM                                          GP98344 04420000
HAVECMD  LH    R4,0(,R5)           GET PARM LGTH                GP98348 04430000
         LA    R15,4(,R5)    POINT TO TEXT                      GP98344 04440000
         LH    R3,2(,R5)           GET OFFSET TO REAL DATA      GP98344 04450000
         AR    R15,R3        SKIP TO PARM PORTION               GP98344 04460000
         SR    R4,R3         ADJUST LENGTH TO MATCH             GP98344 04470000
         SH    R4,=H'4'            GET THE MESSAGE LGTH         GP98344 04480000
         SPACE 1                                                GP98348 04490000
*********************************************************************** 04500000
*  PARSING:  R15 IS THE CURRENT POINTER                               * 04510000
*            R0  IS THE INCREMENT (=1)                                * 04520000
*            R1  IS THE LAST VALID CHARACTER                          * 04530000
*            R2-R4 ARE AVAILABLE FOR FUN AND GAMES                    * 04540000
*            R5  IS THE POINTER TO THE NEXT AVAILABLE DSN SLOT        * 04550000
*            R8  USED FOR SUBROUTINE RETURN                           * 04560000
*********************************************************************** 04570000
COMPARM  LA    R0,1          MAKE A CONSTANT FOR BXLE INCREMENT GP98348 04580000
         SR    R4,R0         ADJUST FOR EXECUTE AND TEST        GP98344 04590000
         BM    RESETPAR      NOTHING - GO TO RESET IT           GP98344 04600000
         LA    R1,0(R15,R4)  MAKE BXLE END ADDRESS              GP98348 04610000
         XC    NUMCT,NUMCT      CLEAR DATA SET COUNT            GP98348 04620000
BEGPARM  BAL   R14,PARSEWRD  LOOK FOR A WORD IN INPUT           GP98348 04630000
           B   RESETPAR      ... IF BLANK, IS RESET REQUEST     GP98348 04640000
         SPACE 1                                                GP98348 04650000
LFTPARM  CLC   =C'LIST ',PARMWORD  IS IT A LIST REQUEST?        GP98348 04660000
         BE    LISTER        YES; SHOW SOME FIREWORKS           GP98348 04670000
* THE REMAINDER OF PARM SCANNING DEFERRED UNTIL WE ARE AUTHORIZED       04680000
         STM   R15,R1,DBLWD  SAVE                               GP98349 04690000
         B     SUSPARM                                          GP98349 04700000
RESETPAR EX    0,RESETPRM    SET NULL PARM FLAG                 GP98349 04710000
         SPACE 1                                                GP98344 04720000
*********************************************************************** 04730000
*    CHECK THAT WE ARE AUTHORIZED TO RUN THIS - I HATE ABENDS         * 04740000
*********************************************************************** 04750000
SUSPARM  TESTAUTH FCTN=1     ARE WE ALLOWED TO DO THIS?         GP98344 04760000
         LTR   R15,R15                                          GP98344 04770000
         BZ    GETSUP        WOW                                GP98344 04780000
         LA    R0,MSGUNAUT   ELSE WRITE A NASTYGRAM             GP98344 04790000
         BAL   R14,MAKEMSG                                      GP98344 04800000
         B     QUICKOUT      AND LEAVE IN A HUFF                GP98344 04810000
GETSUP   MODESET KEY=ZERO,MODE=SUP   STUPIDVISE THE WHOLE DAMN THING    04820000
         OI    PROCFLAG,PFSEQSUP+PFSEQZER  REMIND ME TO RESET THIS      04830000
         SPACE 1                                                GP98349 04840000
*********************************************************************** 04850000
*  IN ORDER TO ACCOMMODATE BOTH COMMAND ENTRY AND CALL ENTRY, WE GET  * 04860000
*   THE UPT, AND THE USER'S PREFIX HERE                               * 04870000
*********************************************************************** 04880000
         PUSH  USING                                            GP98349 04890000
         L     R2,@JSTCB     GET A TCB OF OURS                  GP98349 04900000
         L     R2,TCBJSCB-TCB(,R2)  GET THE JSCB                GP98349 04910000
         USING IEZJSCB,R2                                       GP98349 04920000
         ICM   R2,15,JSCBPSCB  IF THIS FAILS, WE'RE IN DEEEEEEEEEEEP SH 04930000
         BZ    DONEPFX       SKIP PREFIX CODE                   GP98349 04940000
         USING PSCB,R2                                          GP98349 04950000
         ICM   R2,15,PSCBUPT  GET THE UPT                       GP98349 04960000
         BZ    DONEPFX                                          GP98349 04970000
         USING UPT,R2                                           GP98349 04980000
         MVC   PREFIX,UPTPREFX  MOVE THE PREFIX                 GP98349 04990000
         MVC   PREFLEN,UPTPREFL  AND ITS LENGTH                 GP98349 05000000
         POP   USING                                            GP98349 05010000
         SPACE 1                                                GP98349 05020000
*********************************************************************** 05030000
*  SCAN DATASET OPERAND(S) AND OPTIONS IN PARM FIELD                  * 05040000
*********************************************************************** 05050000
DONEPFX  LM    R15,R1,DBLWD  RESTORE PARM SCANNING REGISTERS    GP98349 05060000
         TM    PROCFLAG,PFRESET  NULL PARM?                     GP98349 05070000
         BNZ   DONEPARM      YES; DON'T SCAN ANY MORE           GP98349 05080000
         CLC   =C'DATASET ',PARMWORD   NEW STEPLIB REQUEST?     GP98348 05090000
         BE    YDATPARM      YES                                GP98348 05100000
         CLC   =C'DATA ',PARMWORD  NEW STEPLIB?                 GP98348 05110000
         BE    YDATPARM      YES                                GP98348 05120000
         CLC   =C'DA ',PARMWORD  REALLY SHORT FORM?             GP98348 05130000
         BE    YDATPARM      YES                                GP98348 05140000
         CLC   =C'( ',PARMWORD  REALLY, REALLY SHORT FORM?      GP98349 05150000
         BE    YLPRPARM                                         GP98349 05160000
         B     ERRBDDEL      NO; GET OUT                        GP98348 05170000
YDATPARM CLI   0(R15),C'('   MANDATORY CHARACTER?               GP98348 05180000
         BNE   ERRBDDEL      NO; FAIL IT                        GP98348 05190000
         BXH   R15,R0,ERRBDDEL SET BAD DELIMITER                GP98348 05200000
YLPRPARM LA    R5,DSN1       POINT TO FIRST DSN ENTRY           GP98348 05210000
GDSNPARM BAL   R14,PARSEWRD  GO FIND NEXT PARM                  GP98348 05220000
           B   ERRBDDEL      NONE - MISSING )                   GP98349 05230000
         CLC   =C') ',PARMWORD  IS IT END STRING?               GP98349 05240000
         BE    GENDPARM      YES; DONE WITH DS NAMES            GP98349 05250000
         BAL   R14,PARSEDSN  GO TO PROCESS DATA SET NAME        GP98348 05260000
         TRT   0(L'DSN1,R5),TRTDSNAM  VALID NAME?               GP98349 05270000
         BNZ   ERRBDDSN      NO; FAIL ON NAME                   GP98349 05280000
         LA    R2,DSN1       LOOP THROUGH NAMES FOR DUPLICATE   GP98348 05290000
LDUPPARM CR    R2,R5         CURRENT?                           GP98348 05300000
         BE    NDUPPARM      YES; NOT A DUPLICATE               GP98348 05310000
         CLC   DSN1-DSN1(L'DSN1,R2),DSN1-DSN1(R5)  SAME DATA SET?       05320000
         BE    GDSNPARM      YES; IGNORE SECOND COMING          GP98348 05330000
         LA    R2,L'DSN1(,R2)                                   GP98348 05340000
         B     LDUPPARM                                         GP98348 05350000
NDUPPARM LA    R5,L'DSN1(,R5)  POINT TO NEXT SLOT               GP98348 05360000
         LA    R3,1                                             GP98348 05370000
         AH    R3,NUMCT                                         GP98348 05380000
         CH    R3,=Y(&MAXCAT)  NOT TOO MANY?                    GP98349 05390000
         BH    ERR2MANY      QUIT BEFORE DOING HARM             GP98349 05400000
         STH   R3,NUMCT      COUNT DATA SETS FOUND              GP98348 05410000
         B     GDSNPARM      LOOK FOR ANOTHER DATA SET NAME     GP98349 05420000
GENDPARM MVI   DSN1-DSN1(R5),C' '  INVALIDATE NEXT DSN FIELD    GP98349 05430000
GOLDPARM BAL   R14,PARSEWRD  GET NEXT WORD(S), IF ANY           GP98348 05440000
           B   DONEPARM      GET OUT IF NO MORE                 GP98348 05450000
         CLC   =C'OLD ',PARMWORD  UNSHARED REQUEST?             GP98348 05460000
         BE    DUMBPARM      HOPE THERE'S A GOOOOOOOD REASON    GP98348 05470000
         CLC   =C'SHR ',PARMWORD  SHARED REQUEST?               GP04053 05480000
         BE    GOLDPARM      YES; IGNORE THE DEFAULT            GP04053 05490000
         CLC   =C'APPEND ',PARMWORD  APPEND OLD STEPLIB?        GP98348 05500000
         BE    APNDPARM      YES; NOT CODED YET                 GP98348 05510000
         CLC   =C'APP ',PARMWORD  SHORT FORM?                   GP98348 05520000
         BE    APNDPARM      YES; NOT CODED YET                 GP98348 05530000
         CLC   =C'ADD ',PARMWORD  SOME PEOPLE CAN'T READ?       GP04053 05540000
         BE    APNDPARM      YES; NOT CODED YET                 GP04053 05550000
         CLC   =C'APF ',PARMWORD  SPECIAL?                      GP98349 05560000
         BE    AAPFPARM      YES                                GP98350 05570000
         CLC   =C'AUTH ',PARMWORD  SPECIAL?                     GP04053 05580000
         BE    AAPFPARM      YES                                GP04053 05590000
         CLC   =C'LIST ',PARMWORD  LIST AFTER ALLOCATION?       GP98350 05600000
         BNE   ERRBDPRM      BR IF NOT                          GP98349 05610000
         OI    STEPFLAG,PFLIST   REQUEST LISTING AFTER ALLOC.   GP98349 05620000
         B     GOLDPARM      GO FOR MORE                        GP98349 05630000
AAPFPARM L     R2,CVTPTR     get CVT address                    JW12095 05640000
         ICM   R2,B'1111',CVTSAF(R2) SAFV defined ?             JW12095 05640500
         BZ    RACOK         no RAC, permit APF authorization   JW12095 05641000
         USING SAFV,R2       addressability of SAFV             JW12095 05641500
         CLC   SAFVIDEN(4),SAFVID SAFV initialized ?            JW12095 05642000
         BNE   RACOK         no RAC, permit APF authorization   JW12095 05642500
         DROP  R2            SAFV no longer needed              JW12095 05643000
         LR    R2,R0         remember R0                        JW12095 05643500
         LR    R3,R1         remember R1                        JW12095 05644000
         LR    R4,R15        remember R15                       JW12095 05644500
         RACHECK ENTITY=STEPAUTH,CLASS='FACILITY',ATTR=READ     JW12095 05645000
         LR    R0,R2         restore R0                         JW12095 05645500
         LR    R1,R3         restore R1                         JW12095 05646000
         LR    R2,R15        remember return code               JW12095 05646500
         LR    R15,R4        restore R15                        JW12095 05647000
         LTR   R2,R2         RAC authorization granted?         JW12095 05647500
         BNZ   GOLDPARM      ignore if not and go for more      JW12095 05648000
RACOK    OI    PROCFLAG,PFAPF  REQUEST TO AUTHORIZE STEPLIBS    JW12095 05648500
         B     GOLDPARM      GO FOR MORE                        GP98349 05650000
APNDPARM OI    PROCFLAG,PFAPPEND  APPEND OLD STEPLIB DATA SETS  GP98348 05660000
         B     GOLDPARM      GO FOR MORE                        GP98348 05670000
DUMBPARM MVI   DSPFG,X'01'   ELSE SET DISP=OLD                  GP98349 05680000
         B     GOLDPARM      GO FOR MORE                        GP98348 05690000
         SPACE 1                                                GP98348 05700000
RESETPRM OI    PROCFLAG,PFRESET  NULL PARM - RESET FUNCTION     GP98348 05710000
DONEPARM DS    0H            FINISHED WITH PARM ANALYSIS        GP98348 05720000
         EJECT ,                                                        05730000
*********************************************************************** 05740000
*                                                                     * 05750000
*    THIS SECTION RELOCATES TO A GETMAIN(IN S.P. 254) THE IRB EXIT    * 05760000
*                                                                     * 05770000
*********************************************************************** 05780000
         SPACE 1                                                        05790000
         L     R0,SP1              ... DO THE GETMAIN FOR THE EXIT      05800000
         GETMAIN  R,LV=(0)                                              05810000
         LR    R12,R1              ... R12 IS BASE FOR EXIT AREA        05820000
         LR    R2,R1               POINT TO GOTTEN STORAGE              05830000
         L     R4,=A(STEPLIBX)     POINT TO THE 'CANNED' DATA           05840000
         LA    R3,EXITEND-STEPLIBX   GET THE LENGTH OF THE AREA GP98345 05850000
         LR    R5,R3                 .. TWICE ..                        05860000
         MVCL  R2,R4                 ... MOVE DATA AREA TO GOTTEN AREA  05870000
         USING STEPLIBX,R12        DECLARE IT                           05880000
         SPACE 1                                                GP98348 05890000
         TM    PROCFLAG,PFRESET  IS THIS CALL A RESET REQUEST?  GP98348 05900000
         BNZ   RESET         YES; PERFORM RESET FUNCTION        GP98348 05910000
         SPACE 1                                                GP98348 05920000
*********************************************************************** 05930000
*   IF THE USER SPECIFIED APPEND, THEN WE LOOK FOR THE OLD STEPLIB    * 05940000
*   OR JOBLIB DD, AND ADD THOSE DATA SET NAMES AT THE END OF THE LIST * 05950000
*********************************************************************** 05960000
         SR    R6,R6                                            GP98349 05970000
         ICM   R6,3,NUMCT    GET DATASET COUNT                  GP98349 05980000
         BZ    ERRBDPRM      NONE; OOPS                         GP98349 05990000
         TM    PROCFLAG,PFAPPEND  COMBINE SYSTEM DATASETS?      GP98349 06000000
         BZ    DONEAPP       NO                                 GP98349 06010000
         LA    R1,=CL8'STEPLIB '  POINT TO PREFERRED NAME       GP98349 06020000
         BAL   R14,LOOKTIOT  IS IT PRESENT?                     GP98349 06030000
         LTR   R5,R1                                            GP98349 06040000
         BNZ   DOAPPEND                                         GP98349 06050000
         LA    R1,=CL8'JOBLIB '                                 GP98349 06060000
         BAL   R14,LOOKTIOT  HOW ABOUT ALTERNATE FORM?          GP98349 06070000
         LTR   R5,R1         HAVE THAT?                         GP98349 06080000
         BZ    NONEAPP       NO; NOTHING TO DO                  GP98349 06090000
         PUSH  USING                                            GP98349 06100000
DOAPPEND LR    R7,R6         GET DATASET COUNT                  GP98349 06110000
         MH    R7,=Y(L'DSN1)  CONVERT TO OFFSET                 GP98349 06120000
         LA    R7,DSN1(R7)   POINT TO FIRST UNUSED ENTRY        GP98349 06130000
         USING TIOENTRY,R5                                      GP98349 06140000
         SR    R4,R4         CLEAR FOR IC                       GP98349 06150000
DOAPTIOL ICM   R3,7,TIOEJFCB  GET JFCB TOKEN                    GP98349 06160000
         BZ    DOAPTION      HUH?                               GP98349 06170000
         BAL   R14,GETSWA    GET JFCB ADDRESS                   GP98349 06180000
         USING INFMJFCB,R3                                      GP98349 06190000
         LA    R2,DSN1       LOOP THROUGH NAMES FOR DUPLICATE   GP98349 06200000
DOAPLOOP CR    R2,R7         CURRENT?                           GP98349 06210000
         BE    DOAPMOVE      YES; NOT A DUPLICATE               GP98349 06220000
         CLC   DSN1-DSN1(L'DSN1,R2),JFCBDSNM  SAME DATA SET?    GP98349 06230000
         BE    DOAPTION      YES; IGNORE SECOND COMING          GP98349 06240000
         LA    R2,L'DSN1(,R2)                                   GP98349 06250000
         B     DOAPLOOP                                         GP98349 06260000
DOAPMOVE CH    R6,=Y(&MAXCAT)  HAD ENOUGH?                      GP98349 06270000
         BNL   DOAPFULL      YES; ISSUE WARNING                 GP98349 06280000
         MVC   DSN1-DSN1(L'DSN1,R7),JFCBDSNM  MOVE DATA SET NAME        06290000
         LA    R6,1(,R6)     SET NEW COUNT                      GP98349 06300000
         LA    R7,L'DSN1(,R7)  POINT TO NEXT ENTRY              GP98349 06310000
DOAPTION IC    R4,TIOELNGH   GET THIS LENGTH                    GP98349 06320000
         AR    R5,R4         SKIP TO NEXT ENTRY                 GP98349 06330000
         CLI   TIOELNGH,0    CONCATENATION?                     GP98349 06340000
         BE    DONEAPP       NO MORE; RETURN                    GP98349 06350000
         CLI   TIOEDDNM,C' '  CONCATENATION ?                   GP98349 06360000
         BE    DOAPTIOL      YES; APPEND IT                     GP98349 06370000
         B     DONEAPP       ELSE RETURN                        GP98349 06380000
         POP   USING                                            GP98349 06390000
DOAPFULL LA    R0,MSG2APP    SHOW OVERFLOW                      GP98349 06400000
         BAL   R14,MAKEMSG                                      GP98349 06410000
DONEAPP  STH   R6,NUMCT      UPDATE DATA SET COUNT              GP98349 06420000
         SPACE 2                                                GP98349 06430000
*********************************************************************** 06440000
*   IF THERE IS AN EXISTING STEPDYN ALLOCATION, THERE WAS A PRIOR     * 06450000
*   STEPLIB REQUEST THAT NEEDS TO BE FREED.                           * 06460000
*********************************************************************** 06470000
NONEAPP  LA    R1,BASDD      POINT TO COMMON DDNAME             GP98349 06480000
         BAL   R14,LOOKTIOT  LOOK FOR IT                        GP98345 06490000
         LTR   R1,R1         HAVE WE DONE THIS BEFORE?          GP98345 06500000
         BZ    LOADTCB             BR IF 'STEPDYN' NOT FOUND    GP98348 06510000
         SPACE 1                                                        06520000
TEMPRST  OI    PROCFLAG,PFCYCLE    INDICATE RERUN THIS MESS     GP98345 06530000
         B     RESET               AND GO DO RESET                      06540000
         SPACE 1                                                        06550000
*********************************************************************** 06560000
*   FOR EACH REQUESTED DATA SET, BUILD THE DDNAME (STEPDYNX), AND     * 06570000
*   ALLOCATE IT. VERIFY THAT IT OPENS AND IS A PDS OR PDSE.           * 06580000
*********************************************************************** 06590000
LOADTCB  L     R2,@JSTCB     GET JOB STEP TCB                   GP98345 06600000
         TM    EXITFLAG,XFMASTER   PRIOR SAVE OF MASTER TCBJLB? GP98345 06610000
         BNZ   LOADTCB2      YES                                GP98345 06620000
         OI    EXITFLAG,XFMASTER   SET IT                       GP98345 06630000
         MVC   MASTLIB,TCBJLB-TCB(R2)  SAVE PROC STEPLIB        GP98345 06640000
LOADTCB2 SR    R6,R6               SET A COUNTER                GP98345 06650000
         LA    R4,DSN1             POINT TO THE DSN(S)                  06660000
ALLOLOOP MVC   DSNAM,0(R4)         MOVE IN A DSNAME             GP98345 06670000
         STC   R6,DDNAM+7    MAKE DDN SUFFIX                    GP98348 06680000
         TR    DDNAM+7(1),SUFFIXES  MAKE PRINTABLE VERSION      GP98348 06690000
         LA    R1,STPRQ            POINT TO THE STEPLIB REQ             06700000
         SVC   99                  DO THE ALLOCATE                      06710000
         LA    R2,STPER            POINT TO ERROR CELL          GP98349 06720000
         CLC   STPER,=F'0'         TEST FOR ERRORS                      06730000
         BNZ   ERRALLOC            BR IF ERRORS                 GP98345 06740000
         SPACE 1                                                        06750000
         MVC   STEPDCB+DCBDDNAM-IHADCB(8),DDNAM  MOVE IN CURRENT DDNAME 06760000
         LA    R1,STEPDCB          ... POINT TO THE DCB         GP98345 06770000
         ST    R1,@DCB             ... SAVE THE DCB ADDRESS             06780000
         OI    @DCB,X'80'    SET END-OF-LIST BIT                GP98345 06790000
         OPEN  MF=(E,@DCB)   AND OPEN                           GP98345 06800000
         TM    STEPDCB+DCBOFLGS-IHADCB,DCBOFOPN  OPENED?        GP98345 06810000
         BNO   DSNOPBAD      NO; WRITE MESSAGE                  GP98345 06820000
         LA    R1,DDNAM      POINT TO DD NAME                   GP98345 06830000
         BAL   R14,LOOKTIOT  FIND IT                            GP98345 06840000
         LTR   R1,R1         FOUND?                             GP98345 06850000
         BZ    CLOSE         HUH?                               GP98345 06860000
         USING TIOENTRY,R1                                      GP98345 06870000
         ICM   R3,7,TIOEJFCB  GET JFCB POINTER OR TOKEN         GP98345 06880000
         DROP  R1                                               GP98345 06890000
         BAL   R14,GETSWA    GET ADDRESS FROM TOKEN                     06900000
         USING INFMJFCB,R3                                      GP98345 06910000
         TM    JFCDSORG,JFCORGPO   TEST FOR PDS AND PDSE        GP98350 06920000
         BO    CLOSE         HAVE PDS OR PDSE                   GP98345 06930000
         OI    PROCFLAG,PFERRNPO   ... INDICATE NOT P.O.        GP98345 06940000
         DROP  R3                                               GP98345 06950000
         SPACE 1                                                GP98345 06960000
CLOSE    CLOSE MF=(E,@DCB)         ... AND CLOSE THE DCB        GP98345 06970000
         TM    PROCFLAG,PFERRNPO   ... TEST FOR NOT P.O.        GP98345 06980000
         BO    DSNOTPO             IF NOT, DESERVES MESSAGE             06990000
         SPACE 1                                                GP98345 07000000
         LA    R6,1(,R6)     SET CURRENT DS COUNT               GP98348 07010000
         LA    R4,L'DSN1(,R4)      POINT TO THE NEXT DSNAME     GP98345 07020000
         CLI   0(R4),C' '          SEE IF IT EXISTS             GP98345 07030000
         BNH   DSNDONE             BR IF NO MORE DSN(S)         GP98345 07040000
         B     ALLOLOOP            THEN LOOP                            07050000
         SPACE 1                                                        07060000
*********************************************************************** 07070000
*   WHEN MORE THAN ONE DATA SET WAS REQUESTED, CONCATENATE THEM UNDER * 07080000
*   THE MAIN STEPDYN DD NAME.                                         * 07090000
*********************************************************************** 07100000
DSNDONE  MVC   STEPDCB+DCBDDNAM-IHADCB(8),BASDD   RESET THE DDNAME      07110000
         CH    R6,=H'1'            TEST FOR ONLY ONE DSNAME             07120000
         BE    NOCONCAT            IF ONLY ONE, NO CONCATENATION        07130000
         OI    EXITFLAG,XFCONCAT   INDICATE CONCATENATION       GP98345 07140000
         STH   R6,NUMCT            ELSE SAVE THE NUMBER                 07150000
         LA    R1,CONCT            POINT TO THE LIST                    07160000
         SVC   99                  AND CON-CAT-ENATE                    07170000
         LA    R2,CNCER            POINT TO ERROR CELL          GP98349 07180000
         CLC   CNCER,=F'0'         LOOK FOR ERROR                       07190000
         BNE   ERRALLOC            BR IF ERROR                  GP98345 07200000
         SPACE 1                                                GP98350 07210000
*********************************************************************** 07220000
*   NOW FIND THE SIOT ENTRIES FOR THE NEW DD ENTRIES, AND SET THE     * 07230000
*   CLOSE=FREE AND JOBLIB FLAGS (FWIW, JOBLIB IS EXTRANEOUS)          * 07240000
*********************************************************************** 07250000
NOCONCAT DS    0H                                                       07260000
         L     R2,@TCB             GET MY TCB ADDRESS                   07270000
         L     R2,TCBJSCB-TCB(,R2)        ... JSCB              GP98345 07280000
         L     R3,JSCSCT-IEZJSCB(,R2)     ... SCT HEADER        GP98345 07290000
         BAL   R14,GETSWA    GET ADDRESS FROM TOKEN                     07300000
         ICM   R3,7,SCTFSIOT-INSMSCT(R3)  ... 1ST SIOT          GP98345 07310000
SIOTLOOP BAL   R14,GETSWA    GET ADDRESS FROM TOKEN                     07320000
         USING INDMSIOT,R3                                      GP98345 07330000
         CLC   SCTDDNAM(7),BASDD     LOOK FOR MY DDNAMES        GP98345 07340000
         BNE   SIOTLP2                                          GP98345 07350000
         OI    SCTSBYT2,SIOCLUNL+SCTJOBLB   CLOSE=FREE+JOBLIB FLAGS     07360000
SIOTLP2  ICM   R3,7,SCTPSIOT       ... NEXT SIOT HEADER         GP98345 07370000
         BNZ   SIOTLOOP            ELSE LOOP 'TIL DONE          GP98345 07380000
         SPACE 1                                                        07390000
GETLOCK  STM   R0,R10,LOCKSAVE    SAVE REGISTERS OVER CALL      GP06327 07400000
         SETLOCK  OBTAIN,TYPE=LOCAL,MODE=UNCOND,RELATED=UNLOCK,        *07410000
               REGS=SAVE      GET LOCAL LOCK; PRESERVE R11-R14  GP06327 07420000
         LM    R0,R10,LOCKSAVE    RESTORE REGISTERS EXC. R15    GP06327 07430000
         SPACE 1                                                GP98345 07440000
         TM    PROCFLAG,PFAPF  AUTHORIZE?                       GP98349 07450000
         BZ    *+8           NO                                 GP98349 07460000
         OI    EXITFLAG,XFAPF  PROPAGATE REQUEST                GP98349 07470000
         SPACE 1                                                GP98345 07480000
*********************************************************************** 07490000
*  IN TSO, WE DO NOT USUALLY RUN UNDER THE JOBSTEP TCB. HOWEVER, TO     07500000
*    MAKE THE CODE WORK CORRECTLY, THE DCB MUST BE OWNED BY THE JSTCB.  07510000
*    WE SCHEDULE AN IRB TO RUN UNDER THE JSTCB TO OPEN THE DCB THERE.   07520000
*                                                               GP98345 07530000
*********************************************************************** 07540000
         L     R4,@JSTCB           LOAD THE JOB STEP TCB FOR CIRB       07550000
DOCIRB   CIRB  EP=(R12),WKAREA=50,RETRN=YES,RETIQE=NO,BRANCH=YES,      X07560000
               SVAREA=YES,KEY=SUPR,MODE=SUPR                            07570000
         USING RBSECT,R1           R1 POINTS TO THE IRB                 07580000
         ST    R1,IRBADDR          SAVE THE IRB ADDRESS                 07590000
         L     R3,RBNEXAV          R3 POINTS TO THE IQE                 07600000
         ST    R3,IQEADDR          SAVE THE IQE ADDRESS                 07610000
         USING IQESECT,R3                                               07620000
         ST    R1,IQEIRB                                                07630000
*********************************************************************** 07640000
*   RUN THROUGH THE TCBS, BEGINNING WITH THE JOB-STEP TCB, AND SET    * 07650000
*   OUR JOBLIB ADDRESS FOR EITHER A 0 OR OLD STEPLIB VALUE.           * 07660000
*********************************************************************** 07670000
         LA    R4,STEPDCB          GET ADDRESS                  GP98345 07680000
         L     R5,MASTLIB    LOAD THE MASTER STEPLIB ADDRESS    GP98345 07690000
         L     R2,@JSTCB           GET THE JOB STEP TCB                 07700000
         ST    R2,IQETCB           SAVE IN IQE                          07710000
         DROP  R1,R3                                                    07720000
         USING TCB,R2                                           GP98345 07730000
         NI    STEPFLAG,255-PF@SET-PF@FAIL  RESET CHANGE FLAGS  GP98345 07740000
SETLOOP  L     R0,TCBJLB     GET CURRENT STEPLIB ADDRESS        GP98345 07750000
         CLR   R5,R0         TEST FOR PREVIOUS STEPLIB          GP98345 07760000
         BE    SETMAST       MATCH; REPLACE AND FLAG            GP98345 07770000
         CLR   R4,R0         ALREADY SET OURS?                  GP98345 07780000
         BE    SETTNEXT      (BUT HOW?)                         GP98345 07790000
         OI    STEPFLAG,PF@FAIL  SHOW FOREIGN ADDRESS           GP98345 07800000
         B     SETTNEXT      NOW TRY ANOTHER                    GP98345 07810000
SETMAST  ST    R4,TCBJLB     SAVE STEPLIB DCB ADDR IN TCBJLB    GP98345 07820000
         OI    STEPFLAG,PF@SET   SHOW REPLACEMENT MADE          GP98345 07830000
SETTNEXT ICM   R2,15,TCBTCB  LOAD TCBTCB - NEXT TCB ON CHAIN    GP98345 07840000
         BNZ   SETLOOP             AND LOOP                     GP98345 07850000
         DROP  R2                                               GP98345 07860000
         SPACE 1                                                        07870000
*********************************************************************** 07880000
*   OPEN THE NEW STEPLIB DCB UNDER THE JOB-STEP TASK.                 * 07890000
*********************************************************************** 07900000
LASTCB2  BAL   R14,EFFEXIT   SCHEDULE THE IRB                   GP98344 07910000
         SPACE 1                                                        07920000
UNLOCK   STM   R0,R10,LOCKSAVE    SAVE REGISTERS 0-10           GP06327 07930000
         SETLOCK RELEASE,TYPE=LOCAL,RELATED=GETLOCK,REGS=SAVE   GP06327 07940000
         LM    R0,R10,LOCKSAVE    RESTORE REGISTERS EXC. R15    GP06327 07950000
         TM    STEPFLAG,PF@FAIL   ANY NOT CHANGED?              GP98345 07960000
         BZ    NEEDLIST      NOTHING FAILED; JUST EXIT          GP98345 07970000
         LA    R0,MSGNOCHG   PRESET FOR NO CHANGE               GP98345 07980000
         TM    STEPFLAG,PF@SET  ANY AT ALL CHANGED?             GP98345 07990000
         BZ    SETFAIL       NO; WITHDRAW AFTER MESSAGE         GP98345 08000000
         LA    R0,MSGSMCHG   SHOW ONE OR MORE NOT CHANGED       GP98345 08010000
         BAL   R14,MAKEMSG   AND WRITE IT OUT                   GP98345 08020000
NEEDLIST TM    STEPFLAG,PFLIST  DOES USER WANT A LIST?          GP98350 08030000
         BNZ   LISTER        YES; PRODUCE ONE                   GP98350 08040000
         B     COMEXIT                                          GP98344 08050000
SETFAIL  BAL   R14,MAKEMSG   WRITE IT OUT                       GP98345 08060000
         B     RESET         WITHDRAW CHANGES                   GP98344 08070000
         SPACE 1                                                        08080000
         DROP  R12                                              GP98344 08090000
         SPACE 1                                                GP98344 08100000
MSGEXIT  BAL   R14,MAKEMSG   WRITE THE ERROR MESSAGE            GP98344 08110000
ERREXIT  MVI   CONDCODE+L'CONDCODE-1,8  SET ERROR               GP98344 08120000
         SPACE 1                                                        08130000
COMEXIT  TM    PROCFLAG,PFSEQSUP+PFSEQZER  NEED TO RESET MODE?  GP98345 08140000
         BZ    QUICKOUT      NO                                 GP98345 08150000
         MODESET  KEY=NZERO,MODE=PROB                           GP98344 08160000
QUICKOUT LR    R1,R13                                           GP98344 08170000
         LH    R5,CONDCODE   GET RETURN CODE                    GP98344 08180000
         LA    R0,LDWSIZE                                               08190000
         SH    R13,=AL2(FAKESAVE-LDWDSECT)  TRUE SAVE AREA      GP06327 08200000
         L     R13,4(,R13)                                              08210000
         FREEMAIN R,LV=(0),A=(1)                                        08220000
         LR    R15,R5        SET RETURN CODE                    GP98344 08230000
         LM    R0,R12,20(R13)                                   GP98344 08240000
         L     R14,12(,R13)                                     GP98344 08250000
         BR    R14                                                      08260000
         EJECT ,                                                        08270000
*********************************************************************** 08280000
*                                                                     * 08290000
*    RESET:  RESTORES ALL MODIFIED TCBJLB FIELDS TO THEIR ORIGINAL    * 08300000
*            VALUE, AND CLOSES OUR STEPLIB (VIA IRB SCHEDULED AGAINST * 08310000
*            THE JOBSTEP TCB). IF THE PFCYCLE FLAG IS ON, RETURNS TO  * 08320000
*            CONTINUE THE ALLOCATION REQUEST.                         * 08330000
*                                                                     * 08340000
*********************************************************************** 08350000
RESET    L     R2,@TIOT      GET MY TIOT ADDRESS                GP98345 08360000
         LA    R2,TIOENTRY-TIOT1(,R2)    ... TIOT ENTRIES       GP98345 08370000
         USING TIOENTRY,R2   DECLARE IT                         GP98345 08380000
         SR    R3,R3                                            GP98345 08390000
RSETTIOT ICM   R3,1,TIOELNGH  ANY MORE?                         GP98345 08400000
         BZ    TRYRERUN      NO                                 GP98345 08410000
         CLC   4(7,R2),BASDD    LOOK FOR MY LOGO                GP98345 08420000
         BE    RSETFTIO             BR IF FOUND                 GP98345 08430000
         AR    R2,R3               POINT TO NEXT ENTRY          GP98345 08440000
         B     RSETTIOT            ... ELSE LOOP                GP98345 08450000
RSETFTIO S     R2,@TIOT            SUBTRACT TO GET TIOT OFFSET  GP98345 08460000
         STH   R2,TIOTOFST         SAVE THE TIOT OFFSET                 08470000
         DROP  R2                                               GP98345 08480000
         L     R2,@JSTCB     GET THE JOBSTEP TCB                GP98345 08490000
         USING TCB,R2                                           GP98345 08500000
RSETLOOK ICM   R3,15,TCBJLB  LOAD TASKLIB DCB ADDR              GP98345 08510000
         BNZ   RSETFDCB            BR IF ONE EXISTS             GP98345 08520000
         ICM   R2,15,TCBTCB  ELSE LOAD TCBTCB - NEXT TCB ADDR   GP98345 08530000
         BNZ   RSETLOOK            AND LOOP                     GP98345 08540000
         B     TRYRERUN            GET OUT IF PREVIOUSLY EXITED GP98345 08550000
RSETFDCB CLC   DCBTIOT-IHADCB(2,R3),TIOTOFST SEE IF THIS IS OUR STEPLIB 08560000
         BNE   NEEDLIST            BR IF PREVIOUSLY EXITED      GP98345 08570000
         DROP  R2                                               GP98345 08580000
         SPACE 1                                                        08590000
*********************************************************************** 08600000
*    IF WE GET HERE, THE MODULE HAS BEEN PREVIOUSLY ENTERED.          * 08610000
*    WE MUST FREE THE S.P. 254 AREA THAT WE JUST GOT AND REUSE        * 08620000
*    THE AREA GOTTEN ON THE PREVIOUS ENTRY.                           * 08630000
*********************************************************************** 08640000
         SPACE 1                                                        08650000
         L     R0,SP1              GET THE LGTH + S.P.                  08660000
         LR    R1,R12              POINT TO AREA TO BE FREED            08670000
         FREEMAIN R,LV=(0),A=(1)    FREE UP THE S.P. 254 AREA           08680000
         LA    R12,0(,R3)    GET CLEAN OLD ADDRESS              GP98349 08690000
         LA    R0,STEPDCB-STEPLIBX  OFFSET TO DCB               GP98350 08700000
         SR    R12,R0              R12 POINTS AT THE OLD EXIT   GP98350 08710000
         USING STEPLIBX,R12                                     GP98344 08720000
         OI    EXITFLAG,XFRESET    INDICATE RESET               GP98345 08730000
         L     R1,IRBADDR          GET THE IRB ADDRESS                  08740000
         USING RBSECT,R1           R1 POINTS TO THE IRB                 08750000
         L     R3,RBNEXAV          R3 POINTS TO THE IQE                 08760000
         ST    R3,IQEADDR          SAVE THE IQE ADDRESS                 08770000
         USING IQESECT,R3                                               08780000
         ST    R1,IQEIRB           SAVE IRB ADDRESS IN THE IQE          08790000
         LA    R4,STEPDCB          POINT TO OUR STEPLIB DCB     GP98345 08800000
         L     R5,MASTLIB    GET PROC STEPLIB BACK              GP98345 08810000
         L     R2,@JSTCB           GET THE JOB STEP TCB ADDR            08820000
         ST    R2,IQETCB           AND SAVE IN THE IQE                  08830000
         USING TCB,R2                                           GP98345 08840000
RSETLOOP CL    R4,TCBJLB     SEE IF THE STEPLIB IS OURS         GP98345 08850000
         BNE   RSETNTCB      AND LOOP A LITTLE                  GP98345 08860000
         ST    R5,TCBJLB     CLEAR OUT TCBJLB                   GP98345 08870000
RSETNTCB ICM   R2,15,TCBTCB  LOAD TCBTCB                        GP98345 08880000
         BNZ   RSETLOOP            ELSE LOOP 'TIL DONE          GP98345 08890000
         DROP  R1,R2,R3                                         GP98345 08900000
         SPACE 1                                                        08910000
LOCKLL2  STM   R0,R10,LOCKSAVE    SAVE REGISTERS 0-10           GP06327 08920000
         SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,RELATED=UNLOCK2,        *08930000
               REGS=SAVE      GET LOCAL LOCK                    GP06327 08940000
         LM    R0,R10,LOCKSAVE    RESTORE REGISTERS EXC. R15    GP06327 08950000
         SPACE 1                                                        08960000
         BAL   R14,EFFEXIT   SCHEDULE THE IRB                   GP98350 08970000
         SPACE 1                                                        08980000
UNLOCK2  STM   R0,R10,LOCKSAVE    SAVE REGISTERS 0-10           GP06327 08990000
         SETLOCK RELEASE,TYPE=LOCAL,RELATED=LOCKLL2,REGS=SAVE   GP06327 09000000
         LM    R0,R10,LOCKSAVE    RESTORE REGISTERS EXC. R15    GP06327 09010000
         SPACE 1                                                GP98345 09020000
TRYRERUN TM    PROCFLAG,PFCYCLE    SEE IF RERUN REQUESTED       GP98345 09030000
         BNO   NEEDLIST      NO; RETURN (VIA OPTIONAL LISTER)   GP98345 09040000
         SPACE 1                                                GP98345 09050000
         LA    R5,120        WAIT ONE MINUTE, MAX               GP98350 09060000
WAITLOOP STIMER  WAIT,BINTVL==A(50)   WAIT HALF SECOND          GP98350 09070000
         TM    EXITFLAG,XFRESET  CLOSE AND DECONCAT DONE?       GP98350 09080000
         BZ    WAITDONE      YES                                GP98350 09090000
         BCT   R5,WAITLOOP   ELSE WAIT A LITTLE LONGER          GP98350 09100000
WAITDONE NI    PROCFLAG,255-PFCYCLE  RESET IT                   GP98345 09110000
         B     LOADTCB             AND GO RERUN THE ALLOC CODE          09120000
         DROP  R12                                              GP98344 09130000
         EJECT ,                                                GP98349 09140000
*********************************************************************** 09150000
*                                                                     * 09160000
*   LISTER:  DISPLAYS CURRENT STEPLIB ALLOCATION AND ORIGINAL.        * 09170000
*      WRITTEN MOSTLY TO DEBUG SUBROUTINES IN UNAUTHORIZED STATE.     * 09180000
*                                                                     * 09190000
*********************************************************************** 09200000
         PUSH  USING                                            GP98349 09210000
LISTER   NI    STEPFLAG,255-PFLIST  RESET LIST REQUEST          GP98345 09220000
         L     R2,@TIOT      GET MY TIOT ADDRESS                GP98345 09230000
         LA    R0,MSGLIST    POINT TO LIST MESSAGE              GP98349 09240000
         BAL   R14,MAKEMSG   DISPLAY IT                         GP98349 09250000
         LA    R1,PATDDNAM   POINT TO OUR NAME                  GP98349 09260000
         BAL   R14,LOOKTIOT  IS IT ALLOCATED?                   GP98349 09270000
         LTR   R5,R1         TEST RETURN                        GP98349 09280000
         BZ    LISTERS       NO; LOOK FOR SYSTEM                GP98349 09290000
         BAL   R9,FORMTIOT   PROCESS TIOT ENTRIES               GP98349 09300000
LISTERS  LA    R1,=CL8'STEPLIB '  POINT TO STEPLIB NAME         GP98349 09310000
         BAL   R14,LOOKTIOT  LOOK FOR IT                        GP98349 09320000
         LTR   R5,R1         ANY FOUND?                         GP98349 09330000
         BZ    LISTERJ       DO WE CARE?                        GP98349 09340000
         BAL   R9,FORMTIOT   PROCESS TIOT ENTRIES               GP98349 09350000
LISTERJ  LA    R1,=CL8'JOBLIB '  IS THIS REALLY ALLOCATED?      GP98349 09360000
         BAL   R14,LOOKTIOT  LOOK FOR IT                        GP98349 09370000
         LTR   R5,R1         ANY FOUND?                         GP98349 09380000
         BZ    LISTERO       DO WE CARE?                        GP98349 09390000
         BAL   R9,FORMTIOT   PROCESS TIOT ENTRIES               GP98349 09400000
LISTERO  LA    R1,=CL8'YOURLIB'  IS THIS REALLY ALLOCATED?      GP98349 09410000
         BAL   R14,LOOKTIOT  LOOK FOR IT                        GP98349 09420000
         LTR   R5,R1         ANY FOUND?                         GP98349 09430000
         BZ    LISTERX       DO WE CARE?                        GP98349 09440000
         LA    R0,MSGLISTO   SHOW OLD LIBRARY IN USE            GP98349 09450000
         BAL   R14,MAKEMSG   AND WARN USER                      GP98349 09460000
         BAL   R9,FORMTIOT   PROCESS TIOT ENTRIES               GP98349 09470000
LISTERX  TM    STEPFLAG,PFLIST  DID WE LIST ONE?                GP98349 09480000
         BNZ   COMEXIT                                          GP98349 09490000
         LA    R0,MSGLIST0   SHOW NOTHING FOUND                 GP98349 09500000
         BAL   R14,MAKEMSG   AND WARN USER                      GP98349 09510000
         B     COMEXIT                                          GP98349 09520000
         SPACE 1                                                GP98349 09530000
         USING TIOENTRY,R5                                      GP98349 09540000
FORMTIOT MVC   NEWTXT(8+1+44+1+6),BLANKS  CLEAR LINE            GP98349 09550000
         SR    R4,R4         CLEAR FOR IC                       GP98349 09560000
         LA    R15,4+MSGPFX+8+1+44+1+6  TOTAL LENGTH            GP98349 09570000
         STH   R15,NEWWTO                                       GP98349 09580000
         LA    R1,NEWWTO(R15)                                   GP98349 09590000
         MVC   0(4,R1),MSGDESC  MOVE ROUT/DESC CODES            GP98349 09600000
FORMTIOL ICM   R3,7,TIOEJFCB  GET JFCB TOKEN                    GP98349 09610000
         BZ    FORMTION      HUH?                               GP98349 09620000
         MVC   NEWTXT(L'TIOEDDNM),TIOEDDNM  SHOW DD NAME        GP98349 09630000
         BAL   R14,GETSWA    CONVERT TO TRUE ADDRESS            GP98349 09640000
         MVC   NEWTXT+8+1(44),JFCBDSNM-INFMJFCB(R3)  MOVE DSN   GP98349 09650000
         MVC   NEWTXT+8+1+44+1(6),JFCBVOLS-INFMJFCB(R3)  AND 1ST SER    09660000
         MVI   NEWWFLAG,NEWWHAVE  PREFORMATTED MESSAGE          GP98349 09670000
         BAL   R14,MAKEMSG   DISPLAY ON DEFAULT                 GP98349 09680000
         OI    STEPFLAG,PFLIST  SHOW ENTRY LISTED               GP98349 09690000
FORMTION IC    R4,TIOELNGH   GET THIS LENGTH                    GP98349 09700000
         AR    R5,R4         SKIP TO NEXT ENTRY                 GP98349 09710000
         CLI   TIOELNGH,0    CONCATENATION?                     GP98349 09720000
         BER   R9            NO MORE; RETURN                    GP98349 09730000
         CLI   TIOEDDNM,C' '  CONCATENATION ?                   GP98349 09740000
         BE    FORMTIOL      YES; APPEND IT                     GP98349 09750000
         BR    R9            ELSE RETURN                        GP98349 09760000
         POP   USING                                            GP98349 09770000
         EJECT ,                                                        09780000
ERRALLOC LA    R0,MSGBDALL   GET BAD ALLOCATION MESSAGE         GP98344 09790000
         MVI   NEWWFLAG,NEWWONLY   FORMAT; DON'T WRITE          GP98344 09800000
         BAL   R14,MAKEMSG   PREPARE MESSAGE                    GP98344 09810000
         LR    R1,R0         GET MESSAGE ADDRESS                GP98344 09820000
         AH    R1,0(,R1)     GET MESSAGE END                    GP10311 09830000
         SH    R1,=H'17'     BACK-UP TO FIRST INSERTION         GP10311 09840000
         UNPK  DBLWD(5),0(3,R2)  OVERPACK                       GP98344 09850000
         TR    DBLWD(4),HEXTRTAB MAKE PRINTABLE HEX             GP98344 09860000
         MVC   0(4,R1),DBLWD  MOVE TO MESSAGE                   GP10311 09870000
         LH    R2,2(,R2)                                                09880000
         CVD   R2,DBLWD                                                 09890000
         OI    DBLWD+7,15                                               09900000
         UNPK  13(4,R1),DBLWD  GET SECOND INSERTION FIELD       GP10311 09910000
         MVI   NEWWFLAG,NEWWHAVE  PREFORMATTED                  GP98344 09920000
         BAL   R14,MAKEMSG   WRITE IT AS IS                     GP98344 09930000
         B     ERREXIT       SET ERROR RETURN                   GP98344 09940000
         SPACE 1                                                        09950000
ERRBDPRM LA    R0,MSGBDPRM   BAD PARM FIELD ENTRY               GP98348 09960000
         B     MSGEXIT       WRITE MESSAGE AND RETURN           GP98348 09970000
         SPACE 1                                                GP98349 09980000
ERR2MANY LA    R0,MSG2DSN    EXCESSIVE DATA SET NAME COUNT      GP98349 09990000
         B     MSGEXIT       WRITE MESSAGE AND RETURN           GP98349 10000000
         SPACE 1                                                GP98348 10010000
ERRBDDSN LA    R0,MSGBDDSN   INVALID DATA SET NAME              GP98344 10020000
         B     MSGEXIT       WRITE MESSAGE AND RETURN           GP98344 10030000
         SPACE 1                                                        10040000
ERRBDDEL LA    R0,MSGNODEL   NO/BAD DELIMITER                   GP98344 10050000
         B     MSGEXIT       WRITE MESSAGE AND RETURN           GP98344 10060000
         SPACE 1                                                        10070000
DSNOTPO  LA    R0,MSGNOTPO   DATASET NOT A PDS                  GP98344 10080000
         B     ERRDSNAM      ISSUE MESSAGE AND APPEND DSN       GP98345 10090000
         SPACE 1                                                GP98345 10100000
DSNOPBAD LA    R0,MSGNOTOP   SHOW OPEN FAILED                   GP98345 10110000
ERRDSNAM BAL   R14,MAKEMSG                                      GP98345 10120000
         MVC   NEWTXT(6),=C' DSN: '                             GP98345 10130000
         MVC   NEWTXT+6(L'DSNAM),DSNAM   SHOW CURRENT DSN       GP98345 10140000
         LA    R1,4+MSGPFX+6+L'DSNAM                            GP98349 10150000
         STH   R1,NEWWTO     SET LENGTH                         GP98345 10160000
         LA    R1,NEWWTO(R1)  POINT TO END                      GP98345 10170000
         MVC   0(4,R1),MSGDESC  UPDATE DESCRIPTORS              GP98345 10180000
         MVI   NEWWFLAG,NEWWHAVE  SHOW HAVE MESSAGE             GP98345 10190000
         B     MSGEXIT       SET ERROR AND ISSUE MESSAGE        GP98344 10200000
         TITLE 'S T E P L I B  ***  SUBROUTINES'                GP98345 10210000
         PUSH  PRINT                                            GP98345 10220000
         PRINT GEN                                              GP98345 10230000
         MSGCODE ,           EXPAND MESSAGE PROCESSING MODULE   GP98345 10240000
         POP   PRINT                                            GP98345 10250000
         SPACE 2                                                GP98344 10260000
********************************************************************    10270000
*   GETSWA:  OBTAIN THE (31-BIT) ADDRESS OF AN SQA ENTITY.         *    10280000
*          R3 CONTAINS THE TOKEN IN THE LOW THREE BYTES.           *    10290000
*      RETURNS 31-BIT ADDRESS IN R3                                *    10300000
********************************************************************    10310000
         SPACE 1                                                GP98344 10320000
GETSWA   ST    R14,DBLWD     SAVE RETURN REGISTER               GP98344 10330000
         AIF   (&MVSXA).SWAXA  NEWER SYSTEM                     GP06327 10340000
         LA    R3,0(,R3)     CLEAR HIGH BYTE                    GP06327 10350000
         LTR   R3,R3         ANY VALUE ?                        GP06327 10360000
         BNZ   LOOKSVA       YES; RETURN STORAGE ADDRESS        GP06327 10370000
         B     LOOKSWA8      NO; ERROR                          GP06327 10380000
         AGO   .NOSVA2                                          GP06327 10390000
.SWAXA   XC    LISTSWA(LISTSWAL),LISTSWA  CLEAR IT              GP98344 10400000
         ICM   R3,8,LISTSWA  FOR QUICK RETURN, CLEAR HIGH BYTE  GP98344 10410000
         STCM  R3,7,LISTSVA+4  PLACE IN SECOND WORD OF LIST     GP98344 10420000
         TM    LISTSVA+4+2,1   TOKEN OR ADDRESS ?               GP98344 10430000
         BZ    LOOKSVA       ADDRESS; JUST ADJUST AND RETURN    GP98344 10440000
         LA    R1,LISTSVA    GET SVA ADDRESS                    GP98344 10450000
         ST    R1,LISTEPA    STORE IN POINTER                   GP98344 10460000
         MVC   LISTSWA+8,=C'RL'  SET FUNCTION                   GP98344 10470000
         LA    R1,LISTSWA+8                                     GP98344 10480000
         ST    R1,LISTSWA+4  POINTER TO FUNCTION                GP98344 10490000
         SWAREQ EPA=LISTEPA,MF=(E,LISTSWA),UNAUTH=YES           GP98344 10500000
         SLR   R3,R3         SIGNAL ERROR                       GP98344 10510000
         BXH   R15,R15,LOOKSWA8  UNEXPECTED FAILURE             GP98344 10520000
         L     R3,LISTSVA    GET STORAGE ADDRESS OF JFCB        GP98344 10530000
         B     LOOKSWA8      COMMON RETURN                      GP98344 10540000
.NOSVA2  ANOP  ,                                                GP98344 10550000
LOOKSVA  LA    R3,16(,R3)    SPACE TO JFCB PROPER               GP98344 10560000
LOOKSWA8 L     R14,DBLWD     RESTORE RETURN ADDRESS             GP98344 10570000
         BR    R14           RETURN TO CALLER                   GP98344 10580000
         SPACE 2                                                GP98344 10590000
********************************************************************    10600000
*   EFFEXIT:  INVOKE THE SECOND LEVEL EXIT EFFECTOR TO SCHEDULE    *    10610000
*      OUR IRB.  NOTE THAT REGISTERS GET CLOBBERED.                *    10620000
*      REQUIRES KEY ZERO, SUPERVISOR MODE                          *    10630000
********************************************************************    10640000
         PUSH  USING                                            GP98350 10650000
         SPACE 1                                                GP98350 10660000
EFFEXIT  STM   R14,R12,12(R13)  SAVE EVERYTHING IN CURRENT SET  GP98350 10670000
         LR    R9,R13        PRESERVE SAVE AREA, JUST IN CASE   GP98350 10680000
         L     R6,CVTPTR           GET THE CVT                  GP98350 10690000
         USING CVT,R6                                           GP98350 10700000
         USING STEPLIBX,R12                                     GP98350 10710000
         L     R1,IQEADDR          GET THE IQE ADDRESS          GP98350 10720000
         LCR   R1,R1               GET TWO'S COMPLEMENT OF IQE ADR      10730000
         L     R15,CVT0EF00        STAGE 2 EXIT EFFECTOR ADDR   GP98350 10740000
         BALR  R14,R15             GO DO IT                     GP98350 10750000
         LM    R14,R12,12(R9)  RESTORE ALL REGISTERS            GP98350 10760000
         BR    R14           RETURN TO CALLER                   GP98350 10770000
         POP   USING                                            GP98350 10780000
         SPACE 2                                                GP98344 10790000
********************************************************************    10800000
*   LOOKTIOT:  LOCATE DDNAME PASSED IN R1                          *    10810000
*      RETURNS ENTRY IN R1 IF FOUND, ELSE 0                        *    10820000
********************************************************************    10830000
         PUSH  USING                                            GP98345 10840000
         SPACE 1                                                GP98345 10850000
LOOKTIOT STM   R2,R3,DBLWD   SAVE SOME REGISTERS                GP98345 10860000
         LR    R3,R1         MOVE USER'S REQUEST TO ANOTHER REG GP98345 10870000
         L     R1,@TIOT      GET TIOT ADDRESS                   GP98345 10880000
         SR    R2,R2         ENTRY LENGTH                       GP98345 10890000
         USING TIOT1,R1                                         GP98345 10900000
         LA    R1,TIOENTRY   POINT TO FIRST DD ENTRY            GP98345 10910000
         USING TIOENTRY,R1                                      GP98345 10920000
LOOKTIOL ICM   R2,1,TIOELNGH   GET THE LENGTH OF THIS ENTRY     GP98345 10930000
         BZ    LOOKTION      HUH?                               GP98345 10940000
         CLC   TIOEDDNM,0(R3)  MATCH USER'S REQUEST?            GP98345 10950000
         BE    LOOKTIOX      FOUND; RETURN IN R1                GP98345 10960000
         AR    R1,R2         POINT TO NEXT ENTRY                GP98345 10970000
         B     LOOKTIOL      TRY NEXT                           GP98345 10980000
LOOKTION SR    R1,R1         SIGNAL NOTHING FOUND               GP98345 10990000
LOOKTIOX LM    R2,R3,DBLWD   SAVE SOME REGISTERS                GP98345 11000000
         BR    R14           RETURN TO CALLER                   GP98345 11010000
         POP   USING                                            GP98345 11020000
         SPACE 2                                                GP98349 11030000
*********************************************************************** 11040000
*  PARSEDSN: THIS ROUTINE (INCORRECTLY) PARSES A DATA SET NAME IN THE * 11050000
*     EXTRACTED STRING 'PARMWORD'.  PARMWLEN HAS THE STRING LENGTH -1 * 11060000
*     UNQUOTED STRINGS ARE PREFIXED                                   * 11070000
*********************************************************************** 11080000
PARSEDSN STM   R0,R8,20(R13)  SAVE USER'S REGISTERS             GP98348 11090000
         SR    R4,R4                                            GP98348 11100000
         IC    R4,PARMWLEN   GET LENGTH-1 OF DSN                GP98348 11110000
         MVC   0(L'DSN1,R5),BLANKS  CLEAR                       GP98348 11120000
         LR    R7,R5         SET MOVE DESTINATION               GP98348 11130000
         SPACE 1                                                GP98348 11140000
         CLI   PARMWORD,C''''  LOOK FOR QUOTES                  GP98348 11150000
         BNE   PARSENQU            IF NOT, MUST ADD TSO ID      GP98348 11160000
         SH    R4,=H'2'      ALLOW FOR START AND END QUOTE      GP98348 11170000
         BM    ERRBDDEL        TOO SHORT; TOO BAD               GP98348 11180000
         LA    R2,PARMWORD+2(R4)  POINT TO END OF STRING        GP98348 11190000
         CLI   0(R2),C''''   MATCHING END QUOTE?                GP98348 11200000
         BNE   ERRBDDEL      NO; TOO BAD                        GP98348 11210000
         SR    R3,R3         SET PREFIX LENGTH                  GP98348 11220000
         LA    R8,PARMWORD+1 SAVE START MOVE ADDRESS            GP98348 11230000
         B     PARSECOM      JOIN COMMON CODE                   GP98348 11240000
         SPACE 1                                                GP98348 11250000
PARSENQU LA    R8,PARMWORD   SET MOVE SOURCE                    GP98348 11260000
PARSEDLP MVC   0(7,R7),PREFIX      MOVE IN THE USER'S PREFIX    GP98348 11270000
         SR    R3,R3                                            GP98348 11280000
         ICM   R3,1,PREFLEN  GET THE PREFIX LENGTH              GP98349 11290000
         BZ    PARSECOM      MOVE AS IS                         GP98349 11300000
         AR    R7,R3         SET MOVE DESTINATION               GP98348 11310000
         CLI   PARMWORD,C'.'  DID USER START WITH PERIOD?       GP98348 11320000
         BE    PARSECOM      ZOUNDS                             GP98348 11330000
         MVI   0(R7),C'.'          QUALIFY THE D.S. NAME        GP98348 11340000
         AR    R7,R0         SET NEW MOVE DESTINATION           GP98348 11350000
         AR    R3,R0         PREFIX LENGTH                      GP98348 11360000
PARSECOM AR    R3,R4         COMBINED LENGTH-1                  GP98348 11370000
         LA    R2,L'DSN1     AND COMPARE TO LEGAL MAXIMUM       GP98348 11380000
         CR    R3,R2                                            GP98348 11390000
         BNL   ERRBDDEL      TOO BAD                            GP98348 11400000
         EX    R4,EXMVCDSN        AND MOVE IN THE REST          GP98348 11410000
         LM    R0,R8,20(R13)  RESTORE USER'S REGISTERS          GP98348 11420000
         BR    R14                 AND GO BACK                  GP98348 11430000
EXMVCDSN MVC   0(0,R7),0(R8)  MOVE USER'S DSN OR FRAGMENT       GP98348 11440000
         SPACE 2                                                GP98348 11450000
*********************************************************************** 11460000
*  PARSEWRD: THIS ROUTINE EXTRACTS AND CONVERTS TO UPPER CASE ONE     * 11470000
*     'WORD' OF INPUT TEXT.  RECOGNIZES BLANK, COMMA, AND RIGHT PAR.  * 11480000
*********************************************************************** 11490000
PARSEWRD STM   R0,R8,20(R13)  SAVE USER'S REGISTERS             GP98348 11500000
         MVC   PARMWORD,BLANKS  CLEAR                           GP98348 11510000
         LR    R8,R1         PRESERVE STOP ADDRESS              GP98348 11520000
PARSEWLP LR    R7,R8         GET END ADDRESS                    GP98348 11530000
         SR    R7,R15        LESS START                         GP98348 11540000
         BM    PARSEWND      NOTHING                            GP98348 11550000
         LA    R0,256        SET SEGMENT LENGTH                 GP98348 11560000
PARSEWFB EX    R7,EXTRTNB    FIND A NON-BLANK                   GP98348 11570000
         BNZ   PARSESTR      HAVE ONE                           GP98348 11580000
         AR    R15,R0        POINT TO NEXT SEGMENT              GP98348 11590000
         SR    R7,R0         AND TRY AGAIN                      GP98348 11600000
         BNM   PARSEWFB      IN NEW SEGMENT                     GP98348 11610000
         B     PARSEWND      NO MORE TO DO                      GP98348 11620000
PARSESTR LR    R3,R1         PRESERVE START ADDRESS             GP98348 11630000
         LR    R7,R8         GET END ADDRESS                    GP98348 11640000
         SR    R7,R1         LESS START                         GP98348 11650000
         LR    R15,R1        SET START SCAN ADDRESS             GP98348 11660000
         LA    R2,L'PARMWORD  SET ANTICIPATED MAXIMUM LENGTH    GP98349 11670000
         CR    R7,R2         USE SHORTER OF THE TWO             GP98349 11680000
         BL    *+6                                              GP98349 11690000
         LR    R7,R2         TRUNCATE                           GP98349 11700000
         LA    R1,1(R7,R15)  POINT TO LAST LEGAL ADDRESS +1     GP98349 11710000
         SLR   R2,R2                                            GP98348 11720000
         TRT   0(1,R15),TRTNBLNK  SEE WHETHER THIS IS A SEPARATOR       11730000
         BZ    PARSERST      NO; DO NORMALLY                    GP98349 11740000
         LA    R1,1(,R15)    ENSURE LENGTH OF ONE               GP98349 11750000
         B     PARSESEP      AND SKIP FULL LOAD                 GP98349 11760000
PARSERST EX    R7,EXTRTBL    FIND A BLANK OR SEPARATOR          GP98348 11770000
PARSESEP LR    R15,R1        SET RETURN ADDRESS                 GP98348 11780000
         SR    R1,R3         LENGTH OF PARSED STRING            GP98348 11790000
         BNP   PARSEWLP      NIL - TRY AGAIN                    GP98348 11800000
         BCTR  R1,0                                             GP98348 11810000
         EX    R1,EXOCPRM    MOVE AND UPPER-CASE PARM WORD      GP98348 11820000
         STC   R1,PARMWLEN   SAVE LENGTH(-1)                    GP98348 11830000
         LM    R0,R8,20(R13)  RESTORE USER'S REGISTERS          GP98348 11840000
         B     4(,R14)              AND GO BACK WITH WORD       GP98348 11850000
PARSEWND LM    R0,R8,20(R13)  RESTORE USER'S REGISTERS          GP98348 11860000
         BR    R14                 AND GO BACK                  GP98348 11870000
EXTRTNB  TRT   0(0,R15),TRTBLANK  LOOK FOR A NON-BLANK          GP98348 11880000
EXTRTBL  TRT   0(0,R15),TRTNBLNK  LOOK FOR A BLANK OR SEPARATOR GP98348 11890000
EXOCPRM  OC    PARMWORD(0),0(R3)  MOVE INDENTIFIED WORD         GP98348 11900000
         TITLE 'S T E P L I B  ***  DATA'                       GP98345 11910000
********************************************************************    11920000
*        PROGRAM CONSTANTS                                         *    11930000
********************************************************************    11940000
         SPACE 1                                                        11950000
         DS    0D                                                       11960000
SP1      DC    AL1(254),AL3(512)                                        11970000
BLANKS   DC    CL80' '       SMALLER LOT OF BLANKS              GP98349 11980000
STEPAUTH DC    CL39'STEPAUTH' facility name to authorize        JW12095 11983000
SAFVID   DC    CL4'SAFV'     SAFV eye catcher                   JW12095 11986000
MSGNOCHG MSGDEF 13,'NO TCBS MODIFIED - ALREADY HAD A TASKLIB'   GP98345 11990000
MSGSMCHG MSGDEF 14,'AT LEAST ONE TCB NOT UPDATED - ALREADY HAS A TASKLI*12000000
               B',FLAG=W                                        GP98345 12010000
MSGBDPRM MSGDEF 17,'SYNTAX ERROR IN REQUEST - NO ACTION TAKEN.' GP98348 12020000
MSGNOTOP MSGDEF 19,'DATA SET DID NOT OPEN - NO ACTION.'         GP98345 12030000
MSGNOTPO MSGDEF 20,'DATA SET NOT PARTITIONED - NO ACTION.'      GP98345 12040000
MSGBDDSN MSGDEF 22,'INVALID DSNAME SPECIFICATION - NO ACTION TAKEN.'    12050000
MSGNODEL MSGDEF 40,'REQUIRED DELIMITER NOT FOUND - NO ACTION TAKEN.'    12060000
MSGUNAUT MSGDEF 47,'STEPLIB PROGRAM INVOKED UNAUTHORIZED. NO ACTION.'   12070000
MSG2DSN  MSGDEF 66,'MORE THAN &MAXCAT DATA SETS REQUESTED. NO ACTION.'  12080000
MSG2APP  MSGDEF 67,'&MAXCAT DATA SETS REACHED. APPEND TERMINATED.',FLAG*12090000
               =W            WARNING ONLY                       GP98349 12100000
MSGLIST  MSGDEF 88,'STEPLIB(S) IN USE:',FLAG=I                  GP98349 12110000
MSGLIST0 MSGDEF 89,'***** NO STEPLIB ENTRY FOUND',FLAG=I        GP98349 12120000
MSGLISTO MSGDEF 90,'***** OLD STEPLIB PROGRAM IN USE',FLAG=W    GP98349 12130000
MSGBDALL MSGDEF 99,'STEPLIB NOT ALLOCATED; E.C. = XXXX, R.C. = XXXX'    12140000
         PUSH  PRINT                                            GP98345 12150000
         PRINT GEN                                              GP98345 12160000
         MSGWORK PATTERN,MOD=STLB,MODNM=': '  PATTERN MESSAGE DATA      12170000
         POP   PRINT                                            GP98345 12180000
         SPACE 1                                                        12190000
HEXTRTAB EQU   *-C'0'                                           GP98345 12200000
         DC    C'0123456789ABCDEF'                                      12210000
         SPACE 1                                                        12220000
         LTORG ,                                                        12230000
         SPACE 1                                                GP98348 12240000
TRTNBLNK DC    256AL1(0)                                        GP98348 12250000
         ORG   TRTNBLNK+C' '                                    GP98348 12260000
         DC    X'04'                                            GP98348 12270000
         ORG   TRTNBLNK+C','                                    GP98348 12280000
         DC    X'04'                                            GP98348 12290000
         ORG   TRTNBLNK+C'('                                    GP98348 12300000
         DC    X'08'                                            GP98348 12310000
         ORG   TRTNBLNK+C')'                                    GP98348 12320000
         DC    X'0C'                                            GP98348 12330000
         ORG   ,                                                GP98348 12340000
TRTBLANK DC    256AL1(4)                                        GP98348 12350000
         ORG   TRTBLANK+C' '                                    GP98348 12360000
         DC    X'00'         SKIP BLANKS                        GP98348 12370000
         ORG   TRTBLANK+C','                                    GP98348 12380000
         DC    X'00'         SKIP COMMAS                        GP98348 12390000
         ORG   ,                                                GP98348 12400000
TRTDSNAM DC    256AL1(4)     QUICK AND DIRTY DSN CHECK          GP98349 12410000
         ORG   TRTDSNAM+C' '                                    GP98349 12420000
         DC    X'00'         ALLOW TRAILING BLANKS              GP98349 12430000
         ORG   TRTDSNAM+C'.'                                    GP98349 12440000
         DC    X'00'         ALLOW PERIODS                      GP98349 12450000
         ORG   TRTDSNAM+C'@'                                    GP98349 12460000
         DC    X'00'         ALLOW NATIONAL CHARACTERS          GP98349 12470000
         ORG   TRTDSNAM+C'#'                                    GP98349 12480000
         DC    X'00'         ALLOW NATIONAL CHARACTERS          GP98349 12490000
         ORG   TRTDSNAM+C'$'                                    GP98349 12500000
         DC    X'00'         ALLOW NATIONAL CHARACTERS          GP98349 12510000
         ORG   TRTDSNAM+C'A'                                    GP98349 12520000
         DC    9X'00'        ALLOW A-I                          GP98349 12530000
         ORG   TRTDSNAM+C'J'                                    GP98349 12540000
         DC    9X'00'        ALLOW J-R                          GP98349 12550000
         ORG   TRTDSNAM+C'S'                                    GP98349 12560000
         DC    8X'00'        ALLOW S-Z                          GP98349 12570000
         ORG   TRTDSNAM+C'0'                                    GP98349 12580000
         DC    10X'00'       ALLOW 0-9                          GP98349 12590000
         ORG   ,                                                GP98349 12600000
         TITLE 'S T E P L I B  ***  CIRB EXIT'                  GP98349 12610000
********************************************************************    12630000
* THIS IS THE IRB EXIT THAT IS SET UP AND SCHEDULED BY THE CIRB    *    12640000
* AND THE CALL TO CVT0EF00 WHICH IS THE STAGE 2 EXIT EFFECTOR.     *    12650000
*   R12 POINTS TO THIS AREA AFTER IT IS COPIED TO DYNAMIC STORAGE  *    12660000
********************************************************************    12670000
         PUSH  USING                                                    12680000
         SPACE 1                                                        12690000
STEPLIBX CSECT ,             MAINTAIN DISCRETE ADDRESSABILITY           12700000
         STM   R14,R12,12(R13)                                          12710000
         LR    R12,R15                                          GP98349 12720000
         USING STEPLIBX,R12                                     GP98349 12730000
         TM    EXITFLAG,XFRESET    TEST FOR RESET ENTRY         GP98345 12740000
         BO    DORESET             BR IF RESET                          12750000
         LA    R1,STEPDCB          POINT TO THE NEW DCB         GP98345 12760000
         ST    R1,DCBLIST          ... SAVE AS POINTER          GP98345 12770000
         OI    DCBLIST,X'80' SET END-OF-LIST BIT                GP98345 12780000
         OPEN  MF=(E,DCBLIST)  OPEN IT                          GP98345 12790000
         TM    EXITFLAG,XFAPF  AUTHORIZE?                       GP98349 12800000
         BZ    EXITRET       NO                                 GP98349 12810000
         TM    STEPDCB+DCBOFLGS-IHADCB,DCBOFOPN  DID IT OPEN?   GP98349 12820000
         BZ    EXITRET       NO; AVOID PROBLEMS                 GP98349 12830000
         SR    R1,R1         CLEAR HIGH BYTE                    GP99012 12840000
         ICM   R1,7,STEPDCB+DCBDEBA-IHADCB   DEB FOR STEPLIB    GP99012 12850000
         USING DEBBASIC,R1                                      GP98349 12860000
         OI    DEBFLGS1,DEBAPFIN  TURN ON APF LIBRARY BIT       GP98349 12870000
         L     R1,PSATOLD-PSA      GET MY TCB                   GP98351 12880000
         L     R1,TCBJSCB-TCB(,R1) GET JSCB                     GP98351 12890000
         USING IEZJSCB,R1                                       GP98351 12900000
         TM    JSCBSWT1,JSCBPASS  WISE GUY?                     GP98351 12910000
         BNZ   EXITRET       YES; NO CHANGE                     GP98351 12920000
         OI    JSCBSWT1,JSCBPASS  PERMIT LIBRARY ACCESS          87053  12930000
         OI    EXITFLAG,XFPASS   RECORD IT                      GP98351 12940000
         B     EXITRET             RETURN TO SYSTEM                     12950000
         DROP  R1                                               GP98351 12960000
         SPACE 1                                                        12970000
DORESET  OI    DCBLIST,X'20'       INDICATE CLOSE = FREE        GP98345 12980000
         CLOSE MF=(E,DCBLIST)  CLOSE IT                         GP98345 12990000
         MVI   DCONCATL+S99VERB-S99RB,S99VRBDC DECONCATENATE    GP98350 13000000
         MVI   DECONDD+7,C' '  RESET DD NAME                    GP98350 13010000
         LA    R2,DCONCATL         RESET THE                            13020000
         ST    R2,DCONCAT          ... DECONCATENATION          GP98345 13030000
         OI    DCONCAT,X'80'                                    GP98345 13040000
         LA    R2,DCATPTR          ... POINTERS                         13050000
         ST    R2,DCATERR+4        ...                                  13060000
         LA    R2,DECON            ...                                  13070000
         ST    R2,DCATPTR          ...                          GP98345 13080000
         OI    DCATPTR,X'80'                                    GP98345 13090000
         TM    EXITFLAG,XFCONCAT   TEST FOR DECONCAT NECESSARY  GP98345 13100000
         BNO   DORESETU            JUST SKIP DECONCAT IF NOT    GP98350 13110000
         LA    R1,DCONCAT          POINT TO THE MESS                    13120000
         SVC   99                  DE-CON-CAT                           13130000
********************************************************************    13140000
* DURING TESTING WITH ESA 4.3 IT WAS FOUND THAT THE CLOSE=FREE     *    13150000
* FLAGS WERE NOT BEING HONORED. THIS CODE DOES AN EXPLICIT UNALLOC *    13160000
* FOR EACH POSSIBLE ENTRY.                                         *    13170000
********************************************************************    13180000
DORESETU MVI   DCONCATL+S99VERB-S99RB,S99VRBUN UN-ALLOCATE      GP98350 13190000
         SR    R6,R6                                            GP98350 13200000
         LA    R7,&MAXCAT    MAXIMUM NUMBER                     GP98350 13210000
DORESETL STC   R6,DECONDD+7                                     GP98350 13220000
         TR    DECONDD+7(1),SUFFIXES  MAKE PRINTABLE SUFFIX     GP98350 13230000
         LA    R1,DCONCAT    (THIS WORKS BECAUSE DDCDDNAM=DUNDDNAM)     13240000
         SVC   99            GET RID OF IT                      GP98350 13250000
         LA    R6,1(,R6)     BUMP IT                            GP98350 13260000
         CR    R6,R7         DONE YET?                          GP98350 13270000
         BL    DORESETL      NO; DO ANOTHER                     GP98350 13280000
DORESETX NI    EXITFLAG,255-XFRESET  RESET DONE                 GP98345 13290000
         TM    EXITFLAG,XFPASS   DID I TURN IT ON?              GP98351 13300000
         BZ    EXITRET       NO; LEAVE IT                       GP98351 13310000
         L     R1,PSATOLD-PSA      GET MY TCB                   GP98351 13320000
         L     R1,TCBJSCB-TCB(,R1) GET JSCB                     GP98351 13330000
         USING IEZJSCB,R1                                       GP98351 13340000
         TM    JSCBSWT1,JSCBPASS  WISE GUY?                     GP98351 13350000
         BZ    EXITRET       YES; NO CHANGE                     GP98351 13360000
         NI    JSCBSWT1,255-JSCBPASS  YES; RESET IT             GP98351 13370000
EXITRET  LM    R14,R12,12(R13)                                          13380000
         BR    R14                                                      13390000
         SPACE 2                                                        13400000
STEPDCB  DCB   DSORG=PO,DDNAME=STEPDYN,MACRF=(R)                GP98345 13410000
         SPACE 2                                                        13420000
DCBLIST  DC    A(0)                                             GP98345 13430000
IRBADDR  DC    A(0)                                                     13440000
IQEADDR  DC    A(0)                                                     13450000
EXITFLAG DC    X'00'         COMBINED EXIT FLAGS                GP98345 13460000
XFMASTER EQU   X'80'           MASTER STEPLIB TESTED/STORED     GP98345 13470000
XFAPF    EQU   X'20'           AUTHORIZE DEB                    GP98349 13480000
XFPASS   EQU   X'10'           DS ACCESS                        GP98351 13490000
XFCONCAT EQU   X'02'           REQUEST IS CONCATENATION         GP98345 13500000
XFRESET  EQU   X'01'           REQUEST IS OPEN (0) OR CLOSE (1) GP98345 13510000
SUFFIXES DC    C' 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$'       GP98348 13520000
MASTLIB  DC    A(0)          ADDRESS OF THE ORIGINAL STEPLIB    GP98345 13530000
DCONCAT  DC    A(DCONCATL+X'80000000')                                  13540000
DCONCATL DC    AL1(S99RBEND-S99RB,S99VRBDC,S99NOMNT,0)          GP98349 13550000
DCATERR  DC    F'0'                                                     13560000
DCATADDR DC    A(DCATPTR)                                               13570000
         DC    2F'0'                                                    13580000
         SPACE 1                                                        13590000
DCATPTR  DC    A(DECON+X'80000000')                                     13600000
         SPACE 1                                                        13610000
DECON    DC    Y(DDCDDNAM,1,L'DECONDD)  DDNAM LIST              GP98349 13620000
DECONDD  DC    CL8'STEPDYN'                                     GP98348 13630000
         SPACE 1                                                GP98345 13640000
         LTORG ,             DON'T TRUST ANYONE                 GP98345 13650000
         SPACE 1                                                        13660000
EXITEND  DS    0H                                                       13670000
         POP   USING                                                    13680000
         AIF   (&MVSXA).SECTNEW                                 GP06333 13690000
STEPLIB  CSECT ,                                                GP06333 13700000
         AGO   .SECTCOM                                         GP06333 13710000
.SECTNEW ANOP  ,                                                GP06333 13720000
STEPLIB  RSECT ,             RESTORE MAIN PROGRAM SECTION               13730000
.SECTCOM SPACE 1                                                        13740000
********************************************************************    13750000
*              EXIT END                                            *    13760000
********************************************************************    13770000
         TITLE 'S T E P L I B  ***  DATA'                       GP98345 13780000
********************************************************************    13790000
*    PROGRAM DATA - RELOCATED TO DYNAMIC STORAGE                   *    13800000
********************************************************************    13810000
         PRINT GEN           WASTE A BUSH                       GP98344 13820000
PATTERN  WORKAREA ,          GENERATE DATA                      GP98344 13830000
         SPACE 2                                                        13840000
********************************************************************    13850000
*        PROGRAM VARIABLES  R13 POINTS TO THIS AREA                *    13860000
********************************************************************    13870000
         SPACE 1                                                        13880000
LDWDSECT DSECT ,             DYNAMIC STORAGE - SP 252                   13890000
         DS    18A           OS SAVE AREA (ORIGINAL)                    13900000
FAKESAVE DS    18A           OS SAVE AREA (FOR MVS SETLOCK)     GP06327 13910000
LOCKSAVE DS    11A           SAVE AREA OVER STLOCK FOR R0-R10   GP06327 13920000
DBLWD    DC    2D'0'                                            GP98349 13930000
@TCB     DC    A(0)                                                     13940000
@TIOT    DC    A(0)                                                     13950000
@JSTCB   DC    A(0)                                                     13960000
@DCB     DC    A(0)          OPEN/CLOSE LIST                            13970000
TIOTOFST DC    H'0'                                                     13980000
CONDCODE DC    H'0'          PROGRAM RETURN CODE                GP98344 13990000
PROCFLAG DC    X'00'         PROCESSING FLAG                    GP98345 14000000
PFSEQSUP EQU   X'80'           CURRENTLY IN SUPERVISOR MODE     GP98345 14010000
PFSEQZER EQU   X'40'           KEY ZERO - RESET BEFORE EXIT     GP98345 14020000
PFAPF    EQU   X'20'           DEBAPF REQUEST                   GP98349 14030000
PFRESET  EQU   X'08'           CALLED TO RESET PRIOR REQUEST    GP98348 14040000
PFAPPEND EQU   X'04'           APPEND PRIOR STEPLIB DATA SETS   GP98348 14050000
PFERRNPO EQU   X'02'           USER'S DS NOT PARTITIONED        GP98345 14060000
PFCYCLE  EQU   X'01'           FREE OLD REQUEST; ALLOCATE NEW   GP98345 14070000
STEPFLAG DC    X'00'         PROCESSING FLAG                    GP98345 14080000
PF@SET   EQU   X'20'           AT LEAST ONE TCBJLB CHANGED      GP98345 14090000
PF@FAIL  EQU   X'10'           AT LEAST ONE ALIEN TCBJLB FOUND  GP98345 14100000
PFLIST   EQU   X'01'           LISTER: FOUND ONE                GP98349 14110000
PREFLEN  DC    X'00'         LENGTH OF PREFIX (0-7)             GP98348 14120000
PREFIX   DC    CL7' '                                           GP98345 14130000
PARMWLEN DC    X'00'         LENGTH-1 OF PARSED WORD            GP98348 14140000
PARMWORD DC    CL63' ',C' '  UPPER-CASE RETURN FROM PARSER      GP98348 14150000
         DS    0D                                                       14160000
DSN1     DC    (&MAXCAT)CL44' ',C' '                            GP98348 14170000
         SPACE 1                                                        14180000
         AIF   (NOT &MVSXA).SKIPSWA                             GP06327 14190000
LISTSWA  SWAREQ FCODE=RL,EPA=LISTEPA,MF=L                       GP98344 14200000
LISTEPA  DS    A(LISTSVA)                                       GP98344 14210000
LISTSVA  DS    7A  REQUEST/RESPONSE LIST (IBM LIES; 4A ¬ ENOUGH)  98344 14220000
LISTSWAL EQU   *-LISTSWA     LENGTH TO CLEAR                    GP98344 14230000
LOOKSWA  DS    0H                                               GP98344 14240000
.SKIPSWA SPACE 1                                                GP98344 14250000
         MSGWORK DATA        EXPAND MESSAGE WORK AREA           GP98344 14260000
         SPACE 1                                                GP98345 14270000
DYNWORK  WORKAREA PFX=       EXPAND MAPPING                     GP98345 14280000
LDWSIZE  EQU   *-LDWDSECT                                       GP98345 14290000
         TITLE 'S T E P L I B  ***  DSECTS'                             14300000
CVTSAF   EQU   248 CVTSAF doesn't exist but is a reserved field in 3.8J 14303000
         ICHSAFV  DSECT=YES  map SAFV                           JW12095 14306000
         PRINT NOGEN         LOOK IT UP, LOOK IT UP                     14310000
         MSGWORK DSECT       DEFINE MSGDEF EXPANSION            GP98344 14320000
         SPACE 1                                                        14330000
IQESECT  DSECT                                                          14340000
IQELNK   DS    0AL4                                                     14350000
IQESTAT1 DS    B                                                        14360000
IEQLNKA  DS    AL3                                                      14370000
IQEPARAM DS    A                                                        14380000
IQEIRB   DS    0AL4                                                     14390000
IQEFLAGS DS    B                                                        14400000
IQUPURGE EQU   X'80'                                                    14410000
IQEIRBA  DS    AL3                                                      14420000
IQETCB   DS    0AL4                                                     14430000
IQESTAT2 DS    B                                                        14440000
IQETCBA  DS    AL3                                                      14450000
IQEDCB   DS    A                                                        14460000
IQEOUTLM DS    A                                                        14470000
IQEEND   DS    0C                                                       14480000
IQELEN   EQU   IQEEND-IQESECT                                           14490000
         SPACE 1                                                        14500000
         CVT   DSECT=YES                                                14510000
         SPACE 1                                                        14520000
         IHAASCB ,                                              GP98344 14530000
         SPACE 1                                                GP98344 14540000
         IEFJESCT ,                                             GP98344 14550000
         SPACE 1                                                GP98344 14560000
         IKJTCB ,                                               GP98344 14570000
         SPACE 1                                                        14580000
         IHAPSA ,                                                       14590000
         SPACE 1                                                        14600000
         DCBD  DEVD=DA,DSORG=PS                                         14610000
         SPACE 1                                                GP98349 14620000
         IEZDEB ,                                               GP98349 14630000
         SPACE 1                                                        14640000
DUMTIOT  DSECT ,             IN SOME VERSIONS OF OS, NO DSECT   GP98345 14650000
         IEFTIOT1 ,                                             GP98345 14660000
         SPACE 1                                                GP98345 14670000
DUMJFCB  DSECT ,             IN SOME VERSIONS OF OS, NO DSECT   GP98345 14680000
         IEFJFCBN ,                                             GP98345 14690000
         SPACE 1                                                GP98345 14700000
DUMSIOT  DSECT ,             IN SOME VERSIONS OF OS, NO DSECT   GP98345 14710000
         IEFASIOT ,                                             GP98345 14720000
         SPACE 1                                                GP98345 14730000
DUMSCT   DSECT ,             IN SOME VERSIONS OF OS, NO DSECT   GP98345 14740000
         IEFASCTB ,                                             GP98345 14750000
         SPACE 1                                                GP98345 14760000
DUMJSCB  DSECT ,             IN SOME VERSIONS OF OS, NO DSECT   GP98345 14770000
         IEZJSCB ,                                              GP98345 14780000
         SPACE 1                                                GP98345 14790000
         IHARB                                                          14800000
         SPACE 1                                                        14810000
         IEFZB4D0 .          EXPAND SVC 99 REQUESTS             GP98349 14820000
         SPACE 1                                                GP98349 14830000
         IEFZB4D2 .          EXPAND SVC 99 TU KEYS              GP98349 14840000
         SPACE 1                                                GP98349 14850000
         IKJPSCB ,                                              GP98349 14860000
         SPACE 1                                                GP98349 14870000
         IKJUPT ,                                               GP98349 14880000
         SPACE 1                                                GP98349 14890000
         YREGS ,                                                        14900000
         SPACE 1                                                        14910000
         END                                                            14920000
@@
//SYSUT2   DD  DISP=(,PASS),UNIT=VIO,
//             SPACE=(CYL,(1,1)),DCB=(LRECL=80,RECFM=FB,BLKSIZE=3120)
//SYSIN    DD  DUMMY
//SYSPRINT DD  SYSOUT=* 
//ASM     EXEC PGM=IFOX00,PARM='DECK,NOOBJECT,TERM,NOXREF'
//********************************************************************
//* You might have to change the DSNAMES in the next 2 DD statements *
//********************************************************************
//SYSIN    DD  DISP=(OLD,DELETE),DSN=*.RACUPDT.SYSUT2
//SYSLIB   DD  DISP=SHR,DSN=SYS2.MACLIB,DCB=BLKSIZE=32720
//         DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//SYSUT1   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT2   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSUT3   DD  UNIT=VIO,SPACE=(CYL,(1,1))
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSPUNCH DD  DISP=(,PASS),UNIT=VIO,SPACE=(CYL,(1,1))
//LINK    EXEC PGM=IEWL,
//             COND=(0,NE),
//             PARM='LIST,LET,MAP,RENT,REUS,REFR,AC=1'
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=VIO,SPACE=(TRK,(50,20))
//SYSLMOD  DD  DISP=SHR,DSN=SYS2.CMDLIB
//SYSLIN   DD  DISP=(OLD,DELETE),DSN=*.ASM.SYSPUNCH
//          DD *
 NAME STEPLIB(R)
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
 RX RDEFINE 'FACILITY STEPAUTH UACC(NONE)'
 RX PERMIT 'STEPAUTH CLASS(FACILITY) ID(ADMIN) ACCESS(READ)'
//STDOUT   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDERR   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDIN    DD   DUMMY                                         
