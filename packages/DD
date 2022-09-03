//DD  JOB (TSO),
//             'Install DD',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
//*  DD -                                                               00030000
//*    IS A TSO COMMAND THAT DISPLAYS DATASET ATTRIBUTES.               00040000
//*    IT DOESN'T REQUIRE THAT A DATASET BE CATALOGED, AND              00050000
//*    IT PROVIDES MORE INFORMATION FOR NONVSAM DATASETS                00060000
//*    (AND GENERATION DATA GROUPS) THAN LISTCAT DOES.                  00070000
//*                                                                     00080000
//*  DDX -                                                              00090000
//*    AN ALIAS FOR "DD".  IF THE THIRD CHARACTER OF THE TSO            00100000
//*    COMMAND NAME IS THE LETTER "X", THE COMMAND CLEARS               00110000
//*    THE 3270 SCREEN BEFORE DISPLAYING ANY INFORMATION.               00120000
//*    ALTHOUGH THIS SOUNDS LIKE A KLUGE, IT IS ACTUALLY A              00130000
//*    PRETTY USEFUL ONE.  I USE "DDX" ALL THE TIME.                    00140000
//*                                                                     00150000
//*  INSTALLATION -                                                     00160000
//*    MODIFY THE JOB CARD ABOVE AND THE PROCEDURE DEFAULTS             00170000
//*    BELOW TO SUIT YOUR TASTE.  IF YOU WANT TO CHANGE THE             00180000
//*    COMMAND NAME AND ALIAS YOU WILL HAVE TO DOCTOR THE               00190000
//*    JOBSTREAM.  THERE IS CODE IN THE PROGRAM THAT DEPENDS            00200000
//*    ON THE COMMAND NAME BEING "DD" (NEAR SEQUENCE # 00227000)        00210000
//*                                                                     00220000
//*  BUGS -                                                             00230000
//*    DD CALCULATES THE NUMBER OF RECORDS FOR FB/PS DATASETS.          00240000
//*    THE CALCULATION IS IN ERROR.  IF YOU FEEL LIKE FIXING            00250000
//*    THIS, BE MY GUEST, IT HAS IRRITATED ME FOR SOME TIME.            00260000
//*                                                                     00270000
//INSTALL PROC SOUT='*',             <=== SYSOUT CLASS                  00280000
//             LIB='SYS2.CMDLIB',    <=== TARGET LOAD LIBRARY           00290000
//             HELP='SYS2.HELP',     <=== HELP LIBRARY                  00300000
//             SYSTS=SYSDA,          <=== UNITNAME FOR WORK DATASETS    00310000
//             ASMBLR=IEUASM,        <=== NAME OF YOUR ASSEMBLER        00320000
//             ALIB='SYS1.LINKLIB',  <=== LOCATION OF YOUR ASSEMBLER    00330000
//             SMPMTS='SYS1.SMPMTS', <=== SMPMTS DATASET NAME           00340000
//             MACLIB='SYS1.MACLIB', <=== MACLIB DATASET NAME           00350000
//             AMODGEN='SYS1.AMODGEN', <=== AMODGEN DATASET NAME        00360000
//             MODGEN='SYS1.SMPMTS'  <=== MACRO LIBRARY NAME            00370000
//*                                       (USE SYS1.SMPMTS FOR MVS-370) 00380000
//*                                                                     00390000
//IHADVCT  EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      00400000
//SYSUT2   DD  DSN=SYS2.SOURCE.SYM61,DISP=(NEW,PASS,DELETE),            00410000
//             UNIT=SYSDA,VOL=SER=PUB001,                               00420000
//             SPACE=(TRK,(5,,1)),DCB=(SYS1.AMODGEN)                    00430000
//SYSPRINT DD  DUMMY                                                    00440000
//*                                                                     00450000
//IEBUPDTE EXEC PGM=IEBUPDTE,PARM=NEW                                   00460000
//SYSPRINT DD  SYSOUT=&SOUT                                             00470000
//SYSUT1   DD  DSN=&HELP,DISP=SHR                                       00480000
//SYSUT2   DD  DSN=&HELP,DISP=SHR                                       00490000
//*                                                                     00500000
//ASM     EXEC PGM=&ASMBLR,REGION=2048K,PARM='NOOBJECT,DECK,NOALIGN'    00510000
//STEPLIB  DD  DSN=&ALIB,DISP=SHR                                       00520000
//SYSTERM  DD  SYSOUT=&SOUT                                             00530000
//SYSPRINT DD  SYSOUT=&SOUT                                             00540000
//SYSLIB   DD  DSN=&MACLIB,DISP=SHR                                     00550000
//         DD  DSN=&MODGEN,DISP=SHR                                     00560000
//         DD  DSN=&SMPMTS,DISP=SHR                                     00570000
//         DD  DSN=&AMODGEN,DISP=SHR                                    00580000
//         DD  DSN=SYS2.SOURCE.SYM61,DISP=(OLD,DELETE)                  00590000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00600000
//SYSUT2   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00610000
//SYSUT3   DD  UNIT=&SYSTS,SPACE=(TRK,(30,15))                          00620000
//SYSPUNCH DD  DSN=&&SYSLIN,UNIT=&SYSTS,DISP=(,PASS,DELETE),            00630000
//             SPACE=(TRK,(5,1),RLSE)                                   00640000
//*                                                                     00650000
//LKED    EXEC PGM=HEWL,COND=(0,NE),PARM='LIST,MAP,XREF,RENT,REFR'      00660000
//SYSPRINT DD  SYSOUT=&SOUT                                             00670000
//SYSUT1   DD  UNIT=&SYSTS,SPACE=(TRK,5)                                00680000
//SYSLMOD  DD  DSN=&LIB,DISP=SHR                                        00690000
//SYSLIN   DD  DSN=&&SYSLIN,DISP=(OLD,DELETE)                           00700000
//         DD  DDNAME=SYSIN                                             00710000
//        PEND                                                          00720000
//*                                                                     00730000
//        EXEC INSTALL                                                  00740000
//IHADVCT.SYSIN DD *                                                    00750000
./ ADD NAME=IHADVCT                                                     00760000
*%IF DVCIBASE = ''                      /* IF NO DVCTI BASE SPECIFIED*/ 00770000
*%THEN %DVCIBASE = 'BASED(CVTZDTAB)';   /* USE CVTZDTAB              */ 00780000
*%IF DVCTBASE = ''                      /* IF NO DVCT BASE SPECIFIED */ 00790000
*                                       /* USE UCBTYP TO INDEX DVCTI */ 00800000
*                                       /* AND USE CVTZDTAB + OFFSET */ 00810000
*%THEN %DVCTBASE = 'BASED(CVTZDTAB+DVCTIOFF(UCBTYP & DVCTYPMK))';       00820000
*%;/*                                                                   00830000
         MACRO                                                          00840000
         IHADVCT   &DSECT=YES                                           00850000
.* /* START OF SPECIFICATIONS ****                                      00860000
.*                                                                      00870000
.*01  MODULE-NAME = IHADVCT                                             00880000
.*                                                                      00890000
.*01  COPYRIGHT = NONE                                                  00900000
.*                                                                      00910000
.*01  STATUS = OS/VS2 RELEASE 2, LEVEL 0                                00920000
.*                                                                      00930000
.*01  CHANGE-ACTIVITY = NONE                                            00940000
.*                                                                      00950000
.*01  DESCRIPTIVE-NAME = DEVICE CHARACTERISTICS TABLE MAPPING           00960000
.*                                                                      00970000
.*01  FUNCTION = THIS TABLE DESCRIBES PHYSICAL ATTRIBUTES OF EACH DASD  00980000
.*    DEVICE WHICH HAS BEEN SYSGENED                                    00990000
.*                                                                      01000000
.*01  NOTES = THE TABLE IS POINTED TO BY CVTZDTAB.                      01010000
.*                                                                      01020000
.*01  MODULE-TYPE = MACRO                                               01030000
.*02    PROCESSOR = ASSEMBLER-370R                                      01040000
.*                                                                      01050000
.*02    MACRO-SIZE = 200 STATEMENTS                                     01060000
.*                                                                      01070000
.**** END OF SPECIFICATIONS ***/                                        01080000
* /* MACCOMP Y-2 SC1D0/SJD48                                         */ 01090000
* /* MACSTAT Y-2 73226/021160                                        */ 01100000
*/********************************************************************/ 01110000
*/*                 DEVICE CHARACTERISTICS TABLE                     */ 01120000
*/********************************************************************/ 01130000
*/*                                                                  */ 01140000
*/*         THIS TABLE MAY BE USED TO FIND THE CHARACTERISTICS       */ 01150000
*/*         OF DIRECT ACCESS DEVICES.  THE APPLICABLE DEVICES ARE    */ 01160000
*/*         THOSE CONTAINING UCBDACC IN UCBTBYT3 (SEE IEFUCBOB).     */ 01170000
*/*                                                                  */ 01180000
*/*         NOTE: DEVTYPE MAY BE USED TO EXTRACT INFORMATION         */ 01190000
*/*         FROM THIS TABLE.  ITS OUTPUT AREA IS IN A SLIGHTLY       */ 01200000
*/*         DIFFERENT FORMAT, AND IS MAPPED BY IHADVA.               */ 01210000
*/*                                                                  */ 01220000
*/*         THE TABLE IS COMPOSED OF AN INDEX FOLLOWED BY ONE        */ 01230000
*/*         ENTRY FOR EACH DASD DEVICE WHICH HAS BEEN SYSGENED       */ 01240000
*/*                                                                  */ 01250000
*/*         FOR ASSEMBLER USE, TWO SEPARATE DSECTS ARE PROVIDED.     */ 01260000
*/*         A USING ON DVCTI GIVES ADDRESSIBILITY TO THE INDEX,      */ 01270000
*/*         AND A USING ON DVCT GIVES ADDRESSIBILITY TO AN ENTRY.    */ 01280000
*/*         SPECIFYING DSECT=NO SUPPRESSES THE INDEX AND PROVIDES    */ 01290000
*/*         AN ENTRY DESCRIPTION WITHOUT A DSECT STATEMENT           */ 01300000
*/*                                                                  */ 01310000
*/*         FOR PLS USE, TWO STRUCTURES ARE PROVIDED. THEIR STORAGE  */ 01320000
*/*         ATTRIBUTES ARE CONTROLLED BY SETTING STRING MACRO        */ 01330000
*/*         VARIABLES AS FOLLOWS:                                    */ 01340000
*/*         STRUCTURE  MACRO-VAR       DEFAULT SETTING               */ 01350000
*/*         DVCTI      %DVCIBASE  'BASED(CVTZDTAB)'                  */ 01360000
*/*         DVCT       %DVCTBASE  'BASED(CVTZDTAB                    */ 01370000
*/*                                +DVCTIOFF(UCBTYP&DVCTYPMK))'      */ 01380000
*/*                                                                  */ 01390000
*/*         THE DEFAULT SETTINGS WILL PROVIDE ADDRESSIBILITY TO      */ 01400000
*/*         ALL FIELDS, BUT DEPEND ON CVT AND UCB ADDRESSIBILITY.    */ 01410000
*/*                                                                  */ 01420000
*/*               FORMAT OF EACH ENTRY                               */ 01430000
*/*         _____________________________________________            */ 01440000
*/*   0(00)                                                          */ 01450000
*/*                 DVCCYL                DVCTRK                     */ 01460000
*/*          _____________________ _____________________             */ 01470000
*/*   4(04)                               DVCOVHD                    */ 01480000
*/*                 DVCTRKLN        DVCOVNLB   DVCOVLB               */ 01490000
*/*          _____________________ __________ __________             */ 01500000
*/*   8(08)                                                          */ 01510000
*/*           DVCOVNK    DVCFLAGS         DVCTOL                     */ 01520000
*/*          __________ __________ _____________________             */ 01530000
*/*  12(0C)                        ______________________            */ 01540000
*/*                 DVCALT                                           */ 01550000
*/*          _____________________        DVCOVR0          RPS       */ 01560000
*/*         ______________________ _____________________   ONLY      */ 01570000
*/*  16(10)                                                SECTION   */ 01580000
*/*           DVCSECT    DVCSECTD                                    */ 01590000
*/*          __________ __________                                   */ 01600000
*/********************************************************************/ 01610000
*%/*                                                                    01620000
         AIF   ('&DSECT' EQ 'NO').NODSECT                               01630000
DVCTI    DSECT ,              INDEX TO DVCT                             01640000
*              THIS INDEX IS LOCATED FROM CVTZDTAB.                     01650000
*              THE PROPER ENTRY IS FOUND BY ADDING THE LOW ORDER        01660000
*              4 BITS OF UCBTYP TO THE ADDRESS IN CVTZDTAB.             01670000
DVCTYPMK EQU   X'0000000F'              TYPICAL USAGE:                  01680000
*              LA    RWRK,DVCTYPMK      MASK FOR UNIT TYPE NUMBER       01690000
*              N     RWRK,UCBTYP        PICK UP UNIT TYPE NUMBER        01700000
*              IC    RWRK,DVCTIOFF(RWRK)  PICK UP OFFSET                01710000
DVCTIOFF DS    AL1                      OFFSET TO DVCT ENTRY            01720000
*********************************************************************** 01730000
         SPACE 3                                                        01740000
DVCT     DSECT ,                        FORMAT OF DVCT ENTRY            01750000
*              THE ENTRY IS LOCATED BY ADDING DVCTIOFF TO CVTZDTAB      01760000
         AGO   .ENTRY                                                   01770000
.NODSECT ANOP                                                           01780000
DVCT     DS    0H                       FORMAT OF DVCT ENTRY            01790000
.ENTRY   ANOP                                                           01800000
*                                                                       01810000
DVCCYL   DS    H                        PHYS NO. CYL PER VOLUME         01820000
DVCTRK   DS    H                        NO. TRACKS PER CYLINDER         01830000
DVCTRKLN DS    H                        NO. OF BYTES PER TRACK          01840000
*                                                                       01850000
DVCOVHD  DS    0H                       BLOCK OVERHEAD IF DVC2BOV=1     01860000
*              USE FOLLOWING TWO CONSTANTS IF DVC2BOV=0                 01870000
DVCOVNLB DS    XL1                      OVERHEAD NOT LAST BLOCK         01880000
DVCOVLB  DS    XL1                      OVERHEAD LAST BLOCK             01890000
*                                                                       01900000
DVCOVNK  DS    XL1                      OVERHEAD DECREMENT NOT KEYED    01910000
*                                                                       01920000
DVCFLAGS DS    BL1                                                      01930000
DVC2BOV  EQU   X'08'                    IF 1, USE DVCOVHD               01940000
*                                       IF 0, USE DVCOVNLB,DVCOVLB      01950000
DVCFTOL  EQU   X'01'                    IF 1, APPLY TOLERANCE FACTOR    01960000
*                                                                       01970000
DVCTOL   DS    H                        TOLERANCE FACTOR                01980000
*              APPLY TOLERANCE FACTOR AS FOLLOWS:                       01990000
*              1. ADD BLOCKSIZE AND KEYLENGTH                           02000000
*              2. MULTIPLY BY DVCTOL                                    02010000
*              3. SHIFT RIGHT DVCTSHFT BITS                             02020000
*              4. ADD APPROPRIATE OVERHEADS                             02030000
DVCTSHFT EQU   9                        SHIFT AMT TO DIVIDE BY 512      02040000
*                                                                       02050000
DVCALT   DS    H                        NUMBER ALTERNATE TRKS/VOLUME    02060000
*                                                                       02070000
DVCENTLG EQU   *-DVCT                   BASIC SIZE OF DEVICE TABLE      02080000
*                                       ENTRY, NOT INCLUDING ADD'L      02090000
*                                       CHARACTERISTICS FOR RPS         02100000
**********************************************************************  02110000
*              THE FOLLOWING SECTION OF THE TABLE IS PRESENT         *  02120000
*              ONLY FOR RPS DEVICES--TEST UCBTBYT2 FOR UCB2OPT3      *  02130000
**********************************************************************  02140000
DVCRPS   DS    0CL4                     RPS SECTION                     02150000
DVCOVR0  DS    H                        OVERHEAD BYTES FOR RECORD 0     02160000
DVCSECT  DS    XL1                      NUMBER SECTORS IN FULL TRACK    02170000
DVCSECTD DS    XL1                      NUMBER DATA SECTORS             02180000
*                                                                       02190000
*              END OF DVCT                                              02200000
         MEND                                                           02210000
**/;                                                                    02220000
*                                                                       02230000
* DCL  1 DVCTI DVCIBASE,                /* INDEX TO DVCT             */ 02240000
*        2 *   PTR(8),                  /* OFFSET TO ENTRY 0         */ 02250000
*        2 DVCTIOFF (*) PTR(8);         /* OFFSETS TO ENTRIES 1 TO N */ 02260000
*/*                                                                  */ 02270000
*/*         USE THE LAST 4 BITS OF UCBTYP TO INDEX DVCTIOFF.         */ 02280000
*/*         DVCTYPMK MAY BE USED AS A MASK TO 'AND' WITH UCBTYP.     */ 02290000
*/*         THE INDEX ENTRIES ARE OFFSETS RELATIVE TO CVTZDTAB.      */ 02300000
*/********************************************************************/ 02310000
*                                                                       02320000
* DCL  1 DVCT DVCTBASE,                 /* FORMAT OF DVCT ENTRY      */ 02330000
*        2 DVCCYL FIXED(15) UNSIGNED,   /* PHYS NO. CYL PER VOLUME   */ 02340000
*        2 DVCTRK FIXED(15) UNSIGNED,   /* NO. TRACKS PER CYLINDER   */ 02350000
*        2 DVCTRKLN FIXED(15) UNSIGNED, /* NO. BYTES PER TRACK       */ 02360000
*                                                                       02370000
*        2 DVCOVHD FIXED(15),           /* BLOCK OVERHD IF DVC2BOV=1 */ 02380000
*          /* USE THE FOLLOWING TWO CONSTANTS IF DVC2BOV=0           */ 02390000
*          3 DVCOVNLB FIXED(8),         /* OVERHEAD NOT LAST BLOCK   */ 02400000
*          3 DVCOVLB FIXED(8),          /* OVERHEAD FOR LAST BLOCK   */ 02410000
*                                                                       02420000
*        2 DVCOVNK FIXED(8),            /* OVERHD DECREMENT NOT KEYED*/ 02430000
*                                                                       02440000
*        2 DVCFLAGS BIT(8),                                             02450000
*          3 * BIT(4),                  /* RESERVED                  */ 02460000
*          3 DVC2BOV BIT(1),            /* IF 1, USE DVCOVHD         */ 02470000
*                                       /* IF 0, USE DVCOVNLB,OVLB   */ 02480000
*          3 * BIT(2),                  /* RESERVED                  */ 02490000
*          3 DVCFTOL BIT(1),            /* IF 1, APPLY TOLER FACTOR  */ 02500000
*                                                                       02510000
*        2 DVCTOL FIXED(15) UNSIGNED,   /* TOLERANCE FACTOR          */ 02520000
*/*         APPLY TOLERANCE AS FOLLOWS:                              */ 02530000
*/*         (BLOCKSIZE+KEYLENGTH) * DVCTOL / DVCTSHFT + OVERHEADS    */ 02540000
*                                                                       02550000
*        2 DVCALT FIXED(15),            /* NO. OF ALTERNATE TRACKS   */ 02560000
*/********************************************************************/ 02570000
*/*         THE FOLLOWING SETION OF THE TABLE IS PRESENT             */ 02580000
*/*         ONLY FOR RPS DEVICES (UCB2OPT3=1)                        */ 02590000
*/********************************************************************/ 02600000
*        2 DVCRPS,                      /*RPS SECTION                */ 02610000
*          3 DVCOVR0 FIXED(15),         /* OVERHD BYTES FOR RECORD 0 */ 02620000
*          3 DVCSECT FIXED(8),          /* NO. SECTORS IN FULL TRACK */ 02630000
*          3 DVCSECTD FIXED(8);         /* NO. OF DATA SECTORS       */ 02640000
*/********************************************************************/ 02650000
*                                                                       02660000
* DCL    DVCTSHFT FIXED(15) CONSTANT(512); /* DENOMINATOR FOR DVCTOL */ 02670000
*/*         THE FOLLOWING CONSTANT CAN BE USED TO MASK OUT ALL BUT   */ 02680000
*/*         THE DVCTIOFF SUBSCRIPT FROM THE UCB WORD UCBTYP          */ 02690000
* DCL    DVCTYPMK BIT(32) CONSTANT('0000000F'X); /* UCBTYP MASK      */ 02700000
*                                                                       02710000
*/*               END OF DVCT                                        */ 02720000
./ ENDUP                                                                02730000
/*                                                                      02740000
//IEBUPDTE.SYSIN DD *                                                   02750000
./         ADD    NAME=DD,LIST=ALL                                      02760000
./         NUMBER NEW1=1000,INCR=1000                                   02770000
./         ALIAS  NAME=DDX                                              02780000
)F FUNCTION -                                                           02790000
         -   THE DD COMMAND DISPLAYS INFORMATION ON AN OS               02800000
             DATASET.  AT A MINIMUM THE CATALOG INFORMATION IS          02810000
             DISPLAYED (VOLSER AND DEVICE TYPES) AND IF THE DATASET     02820000
             IS ON A MOUNTED DASD, DEVICE INFORMATION FROM THE          02830000
             DATASET'S DSCB IS DISPLAYED.                               02840000
                                                                        02850000
             AUTHOR -   DAVE PHILLIPS                                   02860000
                        A. DUDA AND SONS, INC.                          02870000
                        P.O. BOX 257                                    02880000
                        OVIEDO, FL  32765                               02890000
                                                                        02900000
)X SYNTAX -                                                             02910000
         DD 'DATASET-NAME'  PARTIAL OR                                  02920000
                            GENERATION(number) OR                       02930000
                            VOLUME(name)                                02940000
                                                                        02950000
  DEFAULTS -  DATASET-NAME DEFAULTS TO TSO USERID PREFIX                02960000
  ALIAS    -  DDX  (DOES CLEAR COMMAND BEFORE DISPLAY)                  02970000
  ABBREVIATIONS -  PARTIAL    ===> P                                    02980000
                   GENERATION ===> G                                    02990000
                   VOLUME     ===> V                                    03000000
                                                                        03010000
)O OPERAND -                                                            03020000
  'dataset-name'                                                        03030000
         - SPECIFIES THE COMPLETE OR PARTIAL NAME OF THE DATA SET       03040000
           WHICH IS TO BE SAERCHED FOR.                                 03050000
                                                                        03060000
))PARTIAL - SPECIFIES A PARTIAL DATA SET NAME SEARCH                    03070000
))GENERATION(number)  - LISTS DATA SET' DSCB INFORMATION FOR THE        03080000
                        GENERATION LEVEL SPECIFIED.                     03090000
))VOLUME(name) - SPECIFIES THE VOLUME NAME ON WHICH THE DATA SET        03100000
                 RESIDES.  IF A VOLUME NAME IS ENTERED, SYSALLDA IS     03110000
                 ASSUMED AS THE UNIT NAME; OTHERWISE, THE UNIT NAME     03120000
                 FROM THE CATALOG IS USED.                              03130000
                                                                        03140000
                 NOTE:  THIS PARAMETER SHOULD BE USED IF THE DATA SET   03150000
                 IS NOT CATALOGED OR IF THE CATALOG ENTRY IS NOT TO BE  03160000
                 USED.                                                  03170000
                                                                        03180000
                                                                        03190000
  EXAMPLES:                                                             03200000
                                                                        03210000
dd 'men.dp.pub.lib'                                                     03220000
                                                                        03230000
DSN = MEN.DP.PUB.LIB                                                    03240000
3380 - INTLIB                                                           03250000
DSORG PO  RECFM U  BLKSIZE 11,476  LRECL 11,476                         03260000
C- 7/25/88  KEYLEN 0  RKP 0  #EXTS 1                                    03270000
U- 8/19/88  CHANGED                                                     03280000
ALLOC IN CYL  SEC 1  TOTAL 3 CYL  USED 1 CYL 4 TRK                      03290000
EXTENTS  #   CCC HH     CCC HH                                          03300000
         1.  209 00  -  211 14    3 CYL                                 03310000
                                                                        03320000
dd 'x.file22' p                                                         03330000
                                                                        03340000
CATALOG: CATALOG.USER                                                   03350000
NON-VSAM ------ X.FILE225.DATA                                          03360000
NON-VSAM ------ X.FILE226.DATA                                          03370000
NON-VSAM ------ X.FILE227.PDS                                           03380000
NON-VSAM ------ X.FILE229.DATA                                          03390000
                                                                        03400000
dd 'od.abrcat'                                                          03410000
                                                                        03420000
CATALOG: CATALOG.USER                                                   03430000
NON-VSAM ------ OD.ABRCAT.G0631V00                                      03440000
NON-VSAM ------ OD.ABRCAT.G0632V00                                      03450000
NON-VSAM ------ OD.ABRCAT.G0633V00                                      03460000
                                                                        03470000
dd 'od.abrcat' g(-1)                                                    03480000
                                                                        03490000
DSN = OD.ABRCAT.G0632V00                                                03500000
3380 - VSAM01                                                           03510000
DSORG PS  RECFM VBS  BLKSIZE 23,476  LRECL 32,404                       03520000
C- 8/18/88  KEYLEN 0  RKP 0  #EXTS 1                                    03530000
U- 8/18/88                                                              03540000
ALLOC IN CYL  SEC 1  TOTAL 1 CYL  USED 3 TRK                            03550000
EXTENTS  #   CCC HH     CCC HH                                          03560000
         1.  146 00  -  146 14    1 CYL                                 03570000
                                                                        03580000
dd 'sys1.logrec' v(altres)                                              03590000
                                                                        03600000
DSN = SYS1.LOGREC                                                       03610000
3380 - ALTRES                                                           03620000
DSORG PS  RECFM U  BLKSIZE 1,944  LRECL 0                               03630000
C-11/09/86  KEYLEN 0  RKP 0  #EXTS 1                                    03640000
U- 6/08/88                                                              03650000
ALLOC IN CYL  SEC 0  TOTAL 5 CYL  USED 5 CYL                            03660000
EXTENTS  #   CCC HH     CCC HH                                          03670000
         1.  448 00  -  452 14    5 CYL                                 03680000
./        ENDUP                                                         03690000
/*                                                                      03700000
//ASM.SYSIN DD *                                                        03710000
DSINFO   TITLE 'DATASET INQUIRY FUNCTION                HOUSEKEEPING AN*03720000
               D PARSE COMMAND BUFFER'                                  03730000
         SPACE 2                                                        03740000
*                                                                       03750000
* FUNCTION -                                                            03760000
*            THIS PROGRAM DISPLAYS INFORMATION ON A OS DATASET. AT A    03770000
*            MINIMUM THE CATALOG INFORMATION IS DISPLAYED (VOLSER AND   03780000
*            DEVICE TYPES) AND IF THE DATASET IS ON A MOUNTED DASD      03790000
*            DEVICE INFORMATION FROM THE DATASET'S DSCB IS DISPLAYED.   03800000
*                                                                       03810000
* AUTHOR -                                                              03820000
*            DAVE PHILLIPS                                              03830000
*              A. DUDA AND SONS, INC.                                   03840000
*              P.O. BOX 257                                             03850000
*              OVIEDO, FL  32765                                        03860000
*                                                                       03870000
* CHANGES OCTOBER 1990 (DAVID ANDREWS) -                                03880000
*            PROGRAM MODIFIED TO USE THE MVS UNIT VERIFICATION          03890000
*            SERVICE (IEFEB4UV) IN THE EVENT THAT THE DEVICE NAME       03900000
*            TABLE CANNOT BE LOCATED (DEVNAMET DISAPPEARED IN           03910000
*            MVS/XA).  ALSO REPLACED SOME CODE THAT SCANNED THE         03920000
*            UCB LOOKUP TABLE WITH OTHER CODE THAT USES IOSVSUCB        03930000
*            (THE UCB LOOKUP TABLE ALSO DISAPPEARED IN MVS/XA).         03940000
*                                                                       03950000
         SPACE                                                          03960000
DSINFO   CSECT                                                          03970000
         SPACE                                                          03980000
         SAVE  (14,12),,DSINFO-&SYSDATE-&SYSTIME                        03990000
         SPACE                                                          04000000
         LR    R11,R15            LOAD FIRST BASE                       04010000
         LA    R12,X'FFF'(,R11)   COMPUTE                               04020000
         LA    R12,1(,R12)        SECOND BASE                           04030000
         USING DSINFO,R11,R12                                           04040000
         LR    R6,R1              SAVE PARM                             04050000
         SPACE                                                          04060000
         LA    R0,CORESIZE                                              04070000
         GETMAIN R,LV=(0)         GET A WORK AREA                       04080000
         ST    R13,4(,R1)         )                                     04090000
         ST    R1,8(,R13)         ) CHAIN SAVE AREAS                    04100000
         LR    R13,R1             )                                     04110000
         USING CORE,R13                                                 04120000
         SPACE 2                                                        04130000
* BUILD ALL SERVICE ROUTINE PARAMETER BLOCKS & LISTS                    04140000
         SPACE                                                          04150000
         MVI   FLAGS,0                                                  04160000
         USING CPPL,R6                                                  04170000
         LA    R4,PPLSECT         -> PPL                                04180000
         USING PPL,R4                                                   04190000
         LA    R5,IOPLSECT        -> IOPL                               04200000
         USING IOPL,R5                                                  04210000
         L     R1,CPPLUPT         -> UPT                                04220000
         L     R2,CPPLECT         -> ECT                                04230000
         SR    R3,R3                                                    04240000
         ST    R3,ECB             ZERO ECB                              04250000
         LA    R3,ECB             -> ECB                                04260000
         STM   R1,R3,PPLSECT      PUT ADDR OF UPT,ECT,ECB IN PPL        04270000
         STM   R1,R3,IOPLSECT     PUT ADDR OF UPT,ECT,ECB IN IOPL       04280000
         LA    R3,IOPB            -> PUTLINE PARM BLOCK                 04290000
         ST    R3,IOPLIOPB        PUT IN IOPL                           04300000
         MVC   PPLPCL,PCLADDR     MOVE PTR TO PCL TO PPL                04310000
         LA    R2,PARSBACK        -> PARSE ANSWER AREA                  04320000
         ST    R2,PPLANS          IN PPL                                04330000
         L     R1,CPPLCBUF        CBUF PTR IN PPL                       04340000
         ST    R1,PPLCBUF                                               04350000
         CLI   6(R1),C' '         3RD CHAR OF CMD NAME BLANK ?          04360000
         BE    *+8                YES - MUST BE 'DD'                    04370000
         OI    FLAGS,CLRSCN       NO  - MUST BE 'DDX'                   04380000
         SPACE                                                          04390000
         LH    R0,0(R1)           LENGTH OF CBUF                        04400000
         LH    R1,2(R1)           OFFSET TO PARMS IN CBUF               04410000
         LA    R1,4(,R1)          ADJUST TO SAME SCALE AS LENGTH        04420000
         CR    R1,R0              PARAMETERS ???                        04430000
         BL    DOPARSE            YES - PARSE THEM                     *04440000
                                  NO - SET UP DEFAULT DSNAME            04450000
         L     R1,CPPLUPT         -> UPT                                04460000
         SR    R0,R0              CLEAR FOR INSERT                      04470000
         ICM   R0,B'0001',23(R1)  LENGTH OF PREFIX                      04480000
         BZ    DOPARSE            NO PREFIX - PROMPT FOR DSNAME         04490000
         STH   R0,DSNLEN          LENGTH OF PREFIX IS LEN OF DSNAME     04500000
         MVI   DSNAME,C' '        BLANK OUT DSNAME FIELD                04510000
         MVC   DSNAME+1(L'DSNAME-1),DSNAME                              04520000
         MVC   DSNAME(7),16(R1)   GET PREFIX FROM UPT                   04530000
         MVI   VOLSER,C' '        BLANK OUT VOLSER FIELD                04540000
         MVC   VOLSER+1(L'VOLSER-1),VOLSER                              04550000
         OI    FLAGS,NOVOL        NO VOLSER SUPPLIED                    04560000
         B     PARMDONE           ALL DONE                              04570000
         SPACE                                                          04580000
DOPARSE  SR    R1,R1                                                    04590000
         ST    R1,PPLUWA          ZERO UWA PTR                          04600000
         SPACE                                                          04610000
         DROP  R4,R5,R6                                                 04620000
         SPACE 2                                                        04630000
* CALL PARSE SERVICE ROUTINE TO GET PARAMETERS                          04640000
         SPACE                                                          04650000
         CALLTSSR EP=IKJPARS,MF=(E,PPLSECT)                             04660000
         SPACE                                                          04670000
         LTR   R15,R15            PARSE OK ?                            04680000
         BZ    PARSEOK            YES                                   04690000
         SPACE                                                          04700000
         CH    R15,=H'4'          USER PROFILE NOPROMPT ?               04710000
         BE    RETURN             YES - GO AWAY                         04720000
         SPACE                                                          04730000
         LA    R1,GFPARSE         ROUTINE ID FOR GENERAL FAIL           04740000
         B     GNRLFAIL           GET GENERAL FAIL TO DIAGNOSE ERROR    04750000
         TITLE 'DATASET INQUIRY FUNCTION                PROCESS PARMS R*04760000
               ETURNED BY PARSE'                                        04770000
         SPACE 2                                                        04780000
PARSEOK  L     R5,PARSBACK        -> PDL AREA                           04790000
         USING IKJPARMD,R5                                              04800000
         SPACE                                                          04810000
         MVI   DSNAME,C' '        BLANK OUT DSNAME FIELD                04820000
         MVC   DSNAME+1(L'DSNAME-1),DSNAME                              04830000
         SPACE                                                          04840000
         TM    DSNIN+6,X'80'      DSNAME SPECIFIED ?                    04850000
         BO    GETDSN             YES                                   04860000
         SPACE                                                          04870000
         OI    FLAGS,FILETYPE     REMEMBER WE HAVE FILE TYPE PARMS      04880000
         MVI   DDNAME,C' '        BLANK OUT DDNAME FIELD                04890000
         MVC   DDNAME+1(L'DDNAME-1),DDNAME                              04900000
         L     R1,DSNIN+8         -> MEMBER                             04910000
         LH    R2,DSNIN+12        MEMBER LENGTH                         04920000
         LA    R3,DDNAME          -> AREA TO MOVE TO                    04930000
         BCTR  R2,0               MINUS ONE FOR EX MVC                  04940000
         EX    R2,MOVEPARM        GET DSN                               04950000
*        MVC   0(*-*,R3),0(R1)                                          04960000
         B     PARMSET            DONE WITH PARMS                       04970000
         SPACE                                                          04980000
GETDSN   L     R1,DSNIN           -> DSNAME                             04990000
         LH    R2,DSNIN+4         DSNAME LENGTH                         05000000
         STH   R2,DSNLEN          SAVE IT                               05010000
         LA    R3,DSNAME          -> AREA TO MOVE TO                    05020000
         BCTR  R2,0               MINUS ONE FOR EX MVC                  05030000
         EX    R2,MOVEPARM        GET DSN                               05040000
*        MVC   0(*-*,R3),0(R1)                                          05050000
         SPACE                                                          05060000
         CLI   PARTLKY+1,0        PARTIAL KEYWORD SPECIFIED ?           05070000
         BE    *+8                NOPE                                  05080000
         OI    FLAGS,PARTIAL      YES - REMEMBER THAT                   05090000
         SPACE                                                          05100000
         MVI   VOLSER,C' '        BLANK OUT VOLSER FIELD                05110000
         MVC   VOLSER+1(L'VOLSER-1),VOLSER                              05120000
         SPACE                                                          05130000
         OI    FLAGS,NOVOL        ASSUME NO VOLSER SPECIFIED            05140000
         SPACE                                                          05150000
         TM    VOLIN+6,X'80'      IS A VOLSER PRESENT                   05160000
         BZ    CHK4GEN            NOPE                                  05170000
         SPACE                                                          05180000
         NI    FLAGS,X'FF'-NOVOL  HAVE A VOLSER - RESET FLAG            05190000
         SPACE                                                          05200000
         L     R1,VOLIN           -> VOLSER                             05210000
         LH    R2,VOLIN+4         LENGTH                                05220000
         LA    R3,VOLSER          -> AREA FOR VOLSER                    05230000
         BCTR  R2,0               MINUS ONE FOR EX                      05240000
         EX    R2,MOVEPARM                                              05250000
*        MVC   0(*-*,R3),0(R1)                                          05260000
         SPACE                                                          05270000
         B     PARMSET            DONE WITH PARMS                       05280000
         SPACE                                                          05290000
CHK4GEN  TM    GENIN+6,X'80'      IS A GENERATION NUMBER PRESENT        05300000
         BZ    PARMSET            NOPE                                  05310000
         OI    FLAGS,GDGDSN       REMEMBER THAT                         05320000
         LH    R1,DSNLEN          LENGTH OF DSNAME                      05330000
         LA    R1,9(,R1)          PLUS LENGTH OF .G0000V00              05340000
         STH   R1,DSNLEN                                                05350000
         L     R1,GENIN           -> GENERATION NUMBER                  05360000
         LH    R2,GENIN+4         LENGTH                                05370000
         CLI   0(R1),C'-'         RELATIVE GENERATION NUMBER ?          05380000
         BNE   ABSGEN             NO - ABSOLUTE GENERATION NUMBER       05390000
         LA    R3,DSNAME+1        -> DSNAME + 1                         05400000
         AH    R3,DSNIN+4         -> AFTER DSNAME + 1                   05410000
         BCTR  R2,0               MINUS ONE FOR EX                      05420000
         EX    R2,MOVEPARM                                              05430000
*        MVC   0(*-*,R3),0(R1)                                          05440000
         SPACE                                                          05450000
         LA    R3,DSNAME          -> DSNAME                             05460000
         AH    R3,DSNIN+4         -> AFTER DSNAME                       05470000
         MVI   0(R3),C'('         LEFT PAREN                            05480000
         AH    R3,GENIN+4         -> END OF GENERATION NUMBER           05490000
         LA    R3,1(,R3)          -> AFTER GENERATION NUMBER            05500000
         MVI   0(R3),C')'         CLOSE PAREN                           05510000
         B     PARMSET                                                  05520000
         SPACE                                                          05530000
ABSGEN   LA    R3,DSNAME          -> DSNAME                             05540000
         AH    R3,DSNIN+4         -> PAST DSNAME                        05550000
         MVC   0(9,R3),=C'.G0000V00'   MOVE IN GENERATION PATTERN       05560000
         LA    R4,4               COMPUTE OFFSET                        05570000
         SR    R4,R2              TO MOVE GEN TO                        05580000
         LA    R3,2(R4,R3)        -> SPOT TO MOVE GEN TO                05590000
         BCTR  R2,0               MINUS ONE FOR EX                      05600000
         EX    R2,MOVEPARM                                              05610000
*        MVC   0(*-*,R3),0(R1)                                          05620000
         SPACE                                                          05630000
         LA    R3,DSNAME          -> DSNAME                             05640000
         AH    R3,DSNIN+4         -> PAST DSNAME                        05650000
         CLC   0(9,R3),=C'.G0000V00'   WAS GENERATION NUMBER ZERO ?     05660000
         BNE   PARMSET            NO - OK                               05670000
         MVI   0(R3),C' '         BLANK OUT .G0000V00                   05680000
         MVC   1(8,R3),0(R3)                                            05690000
         MVC   0(3,R3),=C'(0)'    CHANGE TO RELATIVE GENERATION         05700000
         SPACE 2                                                        05710000
         DROP  R5                 FINISHED WITH PDL                     05720000
         SPACE 2                                                        05730000
PARMSET  IKJRLSA PARSBACK          RELEASE PARSE STORAGE                05740000
         SPACE 2                                                        05750000
PARMDONE BAL   LINK,SETBUFR       SETUP I/O BUFFER                      05760000
         USING OUTRECD,OUTPTR     ESTABLISH OUTPUT BUFFER               05770000
         TITLE 'DATASET INQUIRY FUNCTION                EXTRACT VOLUME *05780000
               AND UNIT FOR UNCATALOGED DATASETS'                       05790000
         TM    FLAGS,GDGDSN       GENERATION NUMBER USED ???            05800000
         BO    DOLOCATE           USE LOCATE TO DECODE GDG NUMBER       05810000
         SPACE                                                          05820000
         TM    FLAGS,NOVOL        NO VOLSER ?                           05830000
         BO    GENLOC             DO GENERIC LOCATE FIRST               05840000
         SPACE                                                          05850000
         TM    FLAGS,FILETYPE     FILE TYPE INPUT ?                     05860000
         BO    SRCHFILE           LOOK UP FILE IN TIOT                  05870000
         SPACE 2                                                        05880000
* LOOK UP UCB FOR GIVEN VOLSER TO GET DEVICE TYPE                       05890000
         SPACE                                                          05900000
         MVC   LINE(6),=C'DSN = '          LABEL                        05910000
         MVC   LINE+6(L'DSNAME),DSNAME     DISPLAY DSNAME               05920000
         LH    R1,DSNLEN                   LENGTH OF DSNAME             05930000
         LA    OUTPTR,LINE+6(R1)           -> END OF LINE               05940000
         BAL   LINK,NEWLINE                NEXT LINE                    05950000
         SPACE                                                          05960000
         L     R3,CVTPTR          -> CVT                                05970000
         USING CVTMAP,R3                                                05980000
         XC    IOSVSWRK,IOSVSWRK  INITIALIZE SCAN WORKAREA     10/16/90 05990000
         LA    R1,IOSVSWRK        BUILD IOSVSUCB PARM LIST     10/16/90 06000000
         ST    R1,IOSVSPRM             "                       10/16/90 06010000
         LA    R1,IOSVSDEV             "                       10/16/90 06020000
         ST    R1,IOSVSPRM+4           "                       10/16/90 06030000
         LA    R1,IOSVSUCB             "                       10/16/90 06040000
         ST    R1,IOSVSPRM+8           "                       10/16/90 06050000
         OI    IOSVSPRM+8,X'80'        "                       10/16/90 06060000
         MVI   IOSVSDEV,UCB3DACC  DIRECT ACCESS ONLY           10/16/90 06070000
         AGO   .SKIP38A                                        * JLM *  06080000
UCBLOOP  L     R15,CVTUCBSC       -> IOSVSUCB UCB SCAN RTN     10/16/90 06090000
         LA    R1,IOSVSPRM        -> IOSVSUCB PARAMETER LIST   10/16/90 06100000
         BALR  R14,R15            GET POINTER TO NEXT UCB      10/16/90 06110000
         LTR   R15,R15            ANYMORE UCBS?                10/16/90 06120000
         BNZ   OBTN4              NO, VOLUME NOT MOUNTED       10/16/90 06130000
         L     R2,IOSVSUCB        POINT TO UCB                 10/16/90 06140000
         USING UCB,R2                  "                       10/16/90 06150000
         AGO   .BYPASS1                                        10/16/90 06160000
.SKIP38A ANOP  ,                                               * JLM *  06170000
*        L     R3,CVTILK2         -> UCB ADDR TABLE            * JLM *  06180000
         L     R3,CVT+X'028'      ADDR (UCB VECTOR TABLE)      * JLM *  06190000
*        DROP  R3                                              * JLM *  06200000
         SR    R2,R2                                                    06210000
         USING UCB,R2                                                   06220000
UCBLOOP  ICM   R2,B'0011',0(R3)   -> UCB                                06230000
         LA    R3,2(,R3)          -> NEXT UCB PTR                       06240000
         BZ    UCBLOOP            NEXT UCB IF NULL                      06250000
         CL    R2,=X'0000FFFF'    END OF TABLE ?                        06260000
         BE    OBTN4              YES - VOLUME NOT MOUNTED              06270000
         TM    UCBTBYT3,UCB3DACC  DASD ?                                06280000
         BNO   UCBLOOP            NO                                    06290000
.BYPASS1 ANOP  ,                                               10/16/90 06300000
         CLC   VOLSER,UCBVOLI     THIS THE VOLUME ?                     06310000
         BNE   UCBLOOP            NO                                    06320000
         DROP  R3                 FORGET ABOUT CVT             10/16/90 06330000
         SPACE                                                          06340000
GOTUCB   MVC   DEVTYPE,UCBTYP+3   SAVE DEVICE TYPE                      06350000
         L     R0,UCBTYP          DEVICE CODE                           06360000
         LR    R1,OUTPTR          -> OUTPUT AREA                        06370000
         BAL   LINK,UCBTRAN       GET DEVICE NAME                       06380000
         MVI   1(R1),C'-'         JUST FOR LOOKS                        06390000
         MVC   3(6,R1),VOLSER     WRITE OUT VOLSER                      06400000
         LA    OUTPTR,9(R1)       -> PAST                               06410000
         BAL   LINK,NEWLINE       WRITE OUT LINE                        06420000
         B     OBTAIN             AND GO FETCH DSCB                     06430000
         EJECT                                                          06440000
* LOOK UP DDNAME IN TIOT TO GET DSNAME AND VOLSER                       06450000
         SPACE                                                          06460000
SRCHFILE L     R1,CVTPTR          -> CVT                                06470000
         USING CVTMAP,R1                                                06480000
         L     R1,CVTTCBP         -> TCB WORDS                          06490000
         L     R1,4(,R1)          -> TCB                                06500000
         USING TCB,R1                                                   06510000
         L     R1,TCBTIO          -> TIOT                               06520000
         USING TIOT,R1                                                  06530000
         LA    R3,TIOENTRY        -> FIRST DD ENTRY                     06540000
         DROP  R1                                                       06550000
         USING TIOENTRY,R3                                              06560000
         SR    R2,R2              CLEAR TIOT LENGTH REGISTER            06570000
         SPACE                                                          06580000
DDLOOP   AR    R3,R2              -> NEXT DD ENTRY                      06590000
         ICM   R2,1,TIOELNGH      LENGTH OF DD ENTRY                    06600000
         BZ    NOFILE             YES - END OF TIOT                     06610000
         CLC   DDNAME,TIOEDDNM    THIS OUR DDNAME ?                     06620000
         BNE   DDLOOP             NOPE - LOOP                           06630000
GOTDD    ICM   R2,B'0111',TIOEJFCB   -> JFCB                            06640000
         BZ    BADFILE            NO JFCB                               06650000
         MVC   DSNAME,16(R2)      GET DSNAME OUT OF JFCB                06660000
         LA    R1,L'DSNAME        -> STRING                             06670000
         LA    R0,DSNAME          STRING LENGTH                         06680000
         BAL   R14,CHOPSTR        DETERMINE LENGTH OF DSNAME            06690000
         STH   R0,DSNLEN          SAVE IT                               06700000
         SPACE                                                          06710000
         ICM   R2,B'0111',TIOEFSRT   -> UCB                             06720000
         BZ    BADFILE            NO UCB - ERROR                        06730000
         USING UCB,R2                                                   06740000
         SPACE                                                          06750000
         TM    UCBTBYT3,UCB3DACC  DASD ?                                06760000
         BNO   BADFILE            NO - INVALID                          06770000
         SPACE                                                          06780000
         MVC   VOLSER,UCBVOLI     GET VOLSER                            06790000
         MVC   DEVTYPE,UCBTYP+3   SAVE DEVICE TYPE                      06800000
         SPACE                                                          06810000
         MVC   LINE(7),=C'FILE = '         LABEL                        06820000
         MVC   LINE+7(8),DDNAME            DISPLAY FILE NAME            06830000
         MVC   LINE+21(6),=C'DSN = '       LABEL                        06840000
         MVC   LINE+27(L'DSNAME),DSNAME    DISPLAY DSNAME               06850000
         LA    OUTPTR,LINE+27+L'DSNAME     -> END OF LINE               06860000
         BAL   LINK,NEWLINE                NEXT LINE                    06870000
         SPACE                                                          06880000
         L     R0,UCBTYP          DEVICE CODE                           06890000
         LR    R1,OUTPTR          -> OUTPUT AREA                        06900000
         BAL   LINK,UCBTRAN       GET DEVICE NAME                       06910000
         MVI   1(R1),C'-'         JUST FOR LOOKS                        06920000
         MVC   3(6,R1),VOLSER     WRITE OUT VOLSER                      06930000
         TM    UCBJBNR,UCBVRDEV   VIO UCB ?                             06940000
         BNO   *+10               NO                                    06950000
         MVC   3(6,R1),=C'(VIO) ' YES - FLAG VOLUME AS VIO VOLUME       06960000
         LA    OUTPTR,9(R1)       -> PAST                               06970000
         BAL   LINK,NEWLINE       WRITE OUT LINE                        06980000
         B     OBTAIN             AND GO FETCH DSCB                     06990000
         TITLE 'DATASET INQUIRY FUNCTION                GET CATALOG INF*07000000
               ORMATION'                                                07010000
* DO A GENERIC LOCATE TO FULLY QUALIFY DATASET                          07020000
         SPACE                                                          07030000
GENLOC   L     R0,GLWKSIZE        SIZE OF GENERIC LOCATE WORKAREA       07040000
         GETMAIN R,LV=(0)         GET WORK AREA FOR GENERIC LOCATE      07050000
         ST    R1,GENWKA          WORK AREA PTR                         07060000
         ST    R1,@GENWKA         KEEP OUR OWN COPY                     07070000
         MVC   0(4,R1),GLWKHDR    HEADER FOR WORKAREA                   07080000
         SPACE                                                          07090000
         MVC   GENLPARM(4),GENLFLAG   FLAGS INDICATING FUNCTION         07100000
         LA    R2,DSNLENC         -> GENERIC KEY                        07110000
         ST    R2,GENDSN                                                07120000
         XC    GENCAT,GENCAT      NO CATALOG DSNAME SUPPLIED            07130000
         XC    GENX1,GENX1        ZERO UNUSED WORDS                     07140000
         SPACE                                                          07150000
         LH    R3,DSNLEN          LENGTH OF DSNAME                      07160000
         TM    FLAGS,PARTIAL      PARTIAL DSNAME SUPPLIED ?             07170000
         BO    PARTDSN            YES - DON'T ADD PERIOD TO END         07180000
         LA    R2,DSNAME(R3)      -> PAST END OF DSNAME                 07190000
         MVI   0(R2),C'.'         ADD TRAILING PERIOD                   07200000
         LA    R3,1(R3)           BUMP LENGTH                           07210000
PARTDSN  STC   R3,DSNLENC         STORE IT FOR GENLOC                   07220000
         SPACE                                                          07230000
         LOCATE GENLPARM          DO GENERIC LOCATE                     07240000
         SPACE                                                          07250000
         OI    FLAGS,SINGDSN      ASSUME SINGLE DSNAME RETURNED         07260000
         L     R8,@GENWKA         -> GENLOC WORK AREA                   07270000
         LTR   R15,R15            GENERIC LOCATE OK ?                   07280000
         BZ    GENLOCOK           YES - USE IT                          07290000
         SPACE                                                          07300000
         CH    R15,=Y(X'2C')      WORK AREA FILLED UP ?                 07310000
         BNE   BADGENL            NO                                    07320000
         SPACE                                                          07330000
         MVC   LINE(L'FILLMSG),FILLMSG    OUTPUT                        07340000
         LA    OUTPTR,LINE+L'FILLMSG      ERROR                         07350000
         BAL   LINK,NEWLINE               MESSAGE                       07360000
         LH    R2,GLWKMAXR        SET LENGTH USED SINCE IT'S NOT        07370000
         STH   R2,2(,R8)          RETURNED BY THIS PARTICULAR RC        07380000
         SPACE                                                          07390000
GENLOCOK LH    R2,2(,R8)          NUMBER OF BYTES USED OF WORKAREA      07400000
         CH    R2,=Y(4+(45*2))    MORE THAN ONE DSN RETURNED ??         07410000
         BNH   ONEDSN             NOPE                                  07420000
         SPACE                                                          07430000
* MORE THAN ONE DATASET RETURNED - SO JUST LIST THEM OUT                07440000
         SPACE                                                          07450000
         NI    FLAGS,X'FF'-SINGDSN   RESET FLAG                         07460000
         LA    R3,4(R8)           -> START OF DSNAMES                   07470000
         LA    R4,45              INCREMENT                             07480000
         LA    R5,0(R2,R8)        -> END OF DSNAMES                     07490000
         BCTR  R5,0               -1 SO DO NOT TRIP ON EQUAL            07500000
         SPACE                                                          07510000
DSNLOOP  DS    0H                 LOOP WRITING OUT DSNAMES              07520000
         SPACE                                                          07530000
         CLI   0(R3),C'0'         CATALOG ENTRY ?                       07540000
         BNE   GETTYP             NO - OTHER                            07550000
         MVC   LINE(9),=C'CATALOG: '     YES - LABEL IT                 07560000
         MVC   LINE+9(44),1(R3)          MOVE IN CATALOG NAME           07570000
         LA    OUTPTR,LINE+53            -> PAST                        07580000
         LA    LINK,NXTDSN               RETURN ADDR                    07590000
         B     NEWLINE                   WRITE IT OUT                   07600000
         SPACE                                                          07610000
GETTYP   LA    R1,TYPTABLE        -> TYPES OF ENTRIES FROM GENLOC       07620000
         LA    R2,TYPCNT          NUMBER OF TYPES IN TABLE              07630000
TYPLOOP  CLC   0(1,R3),0(R1)      IS THIS IT ?                          07640000
         BE    GOTTYPE            YES                                   07650000
         LA    R1,1(,R1)          BUMP TO NEXT                          07660000
         BCT   R2,TYPLOOP         KEEP TRYING                           07670000
         SPACE                                                          07680000
         MVC   LINE(15),=CL15'  -  UNKNOWN'   TELL'EM DON'T KNOW        07690000
         MVC   LINE(1),0(R3)      GET TYPE CHARACTER                    07700000
         B     TYPDONE                                                  07710000
         SPACE                                                          07720000
GOTTYPE  LA    R1,TYPCNT          NUMBER OF ENTRIES IN TABLE            07730000
         SR    R1,R2              COMPUTE OFFSET                        07740000
         MH    R1,=H'15'          TO DESCRIPTION                        07750000
         LA    R1,TYPDESC(R1)     -> DESCRIPTION                        07760000
         MVC   LINE(15),0(R1)     GET IT                                07770000
         MVI   LINE+15,C' '       ADD BLANK TO SEPARATE FROM DSN        07780000
         SPACE                                                          07790000
TYPDONE  MVC   LINE+16(44),1(R3)  MOVE IN DSNAME                        07800000
         LA    R1,LINE+16         -> DSNAME                             07810000
         LA    R0,44              LENGTH                                07820000
         BAL   R14,CHOPSTR        TRIM TRAILING BLANKS                  07830000
         LR    OUTPTR,R1          -> PAST                               07840000
         BAL   LINK,NEWLINE       WRITE IT OUT                          07850000
NXTDSN   BXLE  R3,R4,DSNLOOP      LOOP                                  07860000
         SPACE                                                          07870000
         BAL   LINK,ENDLINE       FORCE A WRITE                         07880000
         B     GENLDONE           ALL DONE                              07890000
         SPACE 2                                                        07900000
* JUST ONE DATASET (OR NONE) RETURNED - SET UP TO DO A NORMAL LOCATE    07910000
         SPACE                                                          07920000
ONEDSN   CLI   49(R8),C' '        ANY DSNAME RETURNED ?                 07930000
         BH    GOTADSN            YES - GET IT                          07940000
         SPACE                                                          07950000
BADGENL  LH    R3,DSNLEN          LENGTH OF DSNAME                      07960000
         LA    R2,DSNAME(R3)      -> PAST END OF DSNAME                 07970000
         MVI   0(R2),C' '         BLANK TRAILING PERIOD                 07980000
         B     GENLDONE           DSN ALL SET NOW FOR NORMAL LOCATE     07990000
         SPACE                                                          08000000
GOTADSN  MVC   DSNAME,50(R8)      YES - GET THE DSNAME                  08010000
         SPACE 2                                                        08020000
GENLDONE L     R0,GLWKSIZE        SIZE OF GENLOC WORKAREA               08030000
         FREEMAIN R,LV=(0),A=(8)  FREE IT                               08040000
         SPACE                                                          08050000
         TM    FLAGS,SINGDSN      DO WE HAVE JUST ONE DSNAME ?          08060000
         BNO   RETURN             NO - ALL DONE                         08070000
         EJECT                                                          08080000
* DO NORMAL LOCATE TO GET VOLUME AND DEVICE INFORMATION FOR DATASET     08090000
         SPACE                                                          08100000
DOLOCATE LA    R3,DSNAME          -> DSNAME                             08110000
         ST    R3,CAMLSTP2                                              08120000
         SR    R3,R3              NO CVOL PARM                          08130000
         ST    R3,CAMLSTP3                                              08140000
         LA    R3,CAMAREA         -> CAMLST WORK AREA                   08150000
         ST    R3,CAMLSTP4                                              08160000
         MVC   CAMLST,NAMECAM     CAMLST NAME FLAGS                     08170000
         LOCATE CAMLST            GET THE VOLSER FOR DSN                08180000
         LR    R2,R15             SAVE LOCATE RC                        08190000
         LR    R3,R0              SAVE POSSIBLE INDEX LEVEL             08200000
         SPACE                                                          08210000
         LA    R1,DSNAME          -> STRING                             08220000
         LA    R0,L'DSNAME        LENGTH OF STRING                      08230000
         BAL   R14,CHOPSTR        DETERMINE LENGTH OF DSNAME            08240000
         STH   R0,DSNLEN          SAVE IT                               08250000
         SPACE                                                          08260000
         MVC   LINE(6),=C'DSN = '          LABEL                        08270000
         MVC   LINE+6(L'DSNAME),DSNAME     DISPLAY DSNAME REGARDLESS    08280000
         LH    R1,DSNLEN                   LENGTH OF DSNAME             08290000
         LA    OUTPTR,LINE+6(R1)           -> END OF LINE               08300000
         BAL   LINK,NEWLINE                NEXT LINE                    08310000
         SPACE                                                          08320000
         LR    R0,R3              RESTORE POSSIBLE INDEX LEVEL          08330000
         LTR   R15,R2             LOCATE OK ??                          08340000
         BNZ   BADLOC             NOPE                                  08350000
         EJECT                                                          08360000
* WRITE OUT CATALOG INFORMATION - VOLSERS AND DEVICE TYPES              08370000
         SPACE                                                          08380000
         LH    R2,VOLCNT          NUMBER OF VOLUMES DS IS ON            08390000
         CH    R2,=H'20'          OVER 20 ?                             08400000
         BNE   *+8                NOPE - USE IT                         08410000
         LH    R2,=H'20'          YES - USE MAX                         08420000
         SPACE                                                          08430000
         LA    R3,VOLENT          -> FIRST VOLUME ENTRY                 08440000
         USING CATRECD,R3                                               08450000
         B     PUTDEVCD           PUT OUT DEVICE NAME FIRST             08460000
         SPACE                                                          08470000
CATLOOP  C     R5,DEVCODE         DEVICE CODE CHANGED ???               08480000
         BE    DEVCSAME           NOPE - JUST PUT OUT VOLSER            08490000
         SPACE                                                          08500000
PUTDEVCD L     R0,DEVCODE         PICK UP DEVICE CODE                   08510000
         LR    R5,R0              SAVE FOR COMPARE                      08520000
         LR    R1,OUTPTR          -> OUTPUT AREA                        08530000
         BAL   LINK,UCBTRAN       GO GET DEVICE NAME                    08540000
         MVI   1(R1),C'-'         LOOKS NICE                            08550000
         LA    OUTPTR,3(R1)       -> NEXT FREE SPOT                     08560000
         SPACE                                                          08570000
DEVCSAME MVC   0(6,OUTPTR),CATVOL PUT OUT VOLSER                        08580000
         LA    R0,6               LENGTH OF VOLSER                      08590000
         LR    R1,OUTPTR          -> VOLSER                             08600000
         BAL   R14,CHOPSTR        TRIM EXTRA BLANKS OFF VOLSER          08610000
         LR    OUTPTR,R1          UPDATE PTR                            08620000
         LH    R0,DSEQNUM         DATASET SEQUENCE NUMBER               08630000
         CH    R0,=H'1'           OVER 1 ?                              08640000
         BNH   NODSEQ             NOPE                                  08650000
         SPACE                                                          08660000
         MVI   0(OUTPTR),C'('     PAREN TO INDICATE DS SEQ NUM          08670000
         LA    R1,1(OUTPTR)       -> POSTION FOR IT                     08680000
         BAL   LINK,INSRTNUM      GO OUTPUT NUMBER                      08690000
         MVC   0(2,R1),=C'),'     CLOSE PAREN & ADD TRAILING COMMA      08700000
         LA    OUTPTR,2(,R1)      -> NEXT FREE SPOT                     08710000
         B     WASDSEQ            SKIP ADDING COMMA & PTR UPDATE        08720000
         SPACE                                                          08730000
NODSEQ   MVI   0(OUTPTR),C','     AND A COMMA                           08740000
         LA    OUTPTR,1(OUTPTR)   -> PAST                               08750000
WASDSEQ  LA    R3,NEXTVOL         -> NEXT VOLUME ENTRY                  08760000
         BCT   R2,CATLOOP         GET NEXT VOLUME                       08770000
         SPACE                                                          08780000
         BCTR  OUTPTR,0           BACK UP OVER LAST COMMA               08790000
         BAL   LINK,NEWLINE       NEXT LINE                             08800000
         SPACE 2                                                        08810000
         TITLE 'DATASET INQUIRY FUNCTION                GET DSCBS FOR D*08820000
               ATASET'                                                  08830000
         CLI   CAMAREA+4,UCB3DACC DASD DEVICE ???                       08840000
         BNE   TPUT               NOPE                                  08850000
         SPACE                                                          08860000
         MVC   VOLSER,CAMAREA+6   GET THE VOLSER FROM CATALOG           08870000
         MVC   DEVTYPE,CAMAREA+5  GET THE DEVICE TYPE FROM CATALOG      08880000
         SPACE                                                          08890000
* OBTAIN ALL DSCBS FOR DATASET                                          08900000
         SPACE                                                          08910000
OBTAIN   LA    R3,DSNAME          -> DSNAME                             08920000
         ST    R3,CAMLSTP2                                              08930000
         LA    R3,VOLSER          -> VOLUME ID                          08940000
         ST    R3,CAMLSTP3                                              08950000
         LA    R3,CAMAREA         -> AREA FOR F1 DSCB                   08960000
         ST    R3,CAMLSTP4                                              08970000
         MVC   CAMLST,SRCHCAM     CAMLST SEARCH FLAGS                   08980000
         OBTAIN CAMLST            GET THE DSCB FOR DATASET              08990000
         SPACE                                                          09000000
         LTR   R15,R15            OBTAIN OK ???                         09010000
         BNZ   BADOBTN            NOPE                                  09020000
         SPACE 2                                                        09030000
         TM    DS1DSORG,X'80'     ISAM DATASET ??                       09040000
         BNO   NOF2DSCB           NOPE                                  09050000
         SPACE                                                          09060000
         LA    R3,DS1PTRDS        -> CCHHR OF FORMAT 2 DSCB             09070000
         ST    R3,CAMLSTP2                                              09080000
         LA    R3,VOLSER          -> VOLUME ID                          09090000
         ST    R3,CAMLSTP3                                              09100000
         LA    R3,IECSDSF2        -> AREA FOR FORMAT 2 DSCB             09110000
         ST    R3,CAMLSTP4                                              09120000
         MVC   CAMLST,SEEKCAM     CAMLST SEEK FLAGS                     09130000
         OBTAIN CAMLST            GET THE EXTENTS DSCB                  09140000
         SPACE                                                          09150000
         LTR   R15,R15            OBTAIN OK ???                         09160000
         BNZ   BADOBTN            NOPE                                  09170000
         SPACE 2                                                        09180000
NOF2DSCB CLI   DS1NOEPV,3         DATASET OVER 3 EXTENTS ??             09190000
         BNH   NOF3DSCB           NOPE - DON'T NEED F3 DSCB             09200000
         SPACE 2                                                        09210000
         LA    R3,DS1PTRDS        -> CCHHR OF FORMAT 3 DSCB             09220000
         TM    DS1DSORG,X'80'     ISAM DATASET ??                       09230000
         BNO   *+8                NOPE                                  09240000
         LA    R3,DS2PTRDS        -> CCHHR OF FORMAT 3 DSCB             09250000
         ST    R3,CAMLSTP2                                              09260000
         LA    R3,VOLSER          -> VOLUME ID                          09270000
         ST    R3,CAMLSTP3                                              09280000
         LA    R3,IECSDSF3        -> AREA FOR FORMAT 3 DSCB             09290000
         ST    R3,CAMLSTP4                                              09300000
         MVC   CAMLST,SEEKCAM     CAMLST SEEK FLAGS                     09310000
         OBTAIN CAMLST            GET THE EXTENTS DSCB                  09320000
         SPACE                                                          09330000
         LTR   R15,R15            OBTAIN OK ???                         09340000
         BNZ   BADOBTN            NOPE                                  09350000
         SPACE                                                          09360000
         OI    FLAGS,HAVEF3       INDICATE F3 DSCB READ                 09370000
         SPACE                                                          09380000
NOF3DSCB EQU   *                  BYPASS OBTAIN FOR F3 DSCB             09390000
         SPACE 2                                                        09400000
         L     @DVCT,CVTPTR       -> CVT                                09410000
         USING CVTMAP,@DVCT                                             09420000
         L     @DVCT,CVTZDTAB     -> DEVICE CHAR TABLE                  09430000
         IC    R4,DEVTYPE         PICK UP LAST BYTE UCBTYPE FIELD       09440000
         LA    R0,X'0F'           MASK                                  09450000
         NR    R4,R0              BOTTOM 4 BITS UCBTYPE DEVICE FIELD    09460000
         IC    R4,0(R4,@DVCT)     PICK UP OFFSET TO DEV ENTRY           09470000
         AR    @DVCT,R4           -> DEVICE CHAR TABLE ENTRY            09480000
         USING DVCT,@DVCT                                               09490000
         LH    R1,DVCTRK          TRACKS PER CYLINDER                   09500000
         ST    R1,TRKCYL          FOR SPACE CALCULATION                 09510000
         ST    @DVCT,DVCTPTR      SAVE ADDRESS OF DVCT ENTRY            09520000
         DROP  @DVCT                                                    09530000
         TITLE 'DATASET INQUIRY FUNCTION                GET DSORG,RECFM*09540000
               ,BLKSIZE,LRECL,OPTCD AND PSWD'                           09550000
         SPACE                                                          09560000
* DSORG                                                                 09570000
         SPACE                                                          09580000
         MVC   0(8,OUTPTR),=C'DSORG **' LABEL                           09590000
         LR    R5,OUTPTR          SAVE OUTPTR                           09600000
         LA    OUTPTR,8(OUTPTR)   -> PAST                               09610000
         SR    R1,R1              CLEAR FOR INSERT                      09620000
         ICM   R1,B'1100',DS1DSORG   PICK UP DSORG                      09630000
         BZ    NODSORG               NONE                               09640000
         LA    OUTPTR,6(R5)       BACK UP PTR TO DSORG VALUE AREA       09650000
         LA    R2,DSORGTBL        -> DSORGS                             09660000
DSORGLP  LTR   R1,R1              TEST THAT TOP BIT                     09670000
         BZ    NODSORG            ALL DONE                              09680000
         BNM   NXTDSORG           BIT NOT SET                           09690000
         MVC   0(2,OUTPTR),0(R2)  MOVE IN DSORG                         09700000
         LA    OUTPTR,2(OUTPTR)   -> NEXT SPOT                          09710000
NXTDSORG LA    R2,2(R2)           -> NEXT DSORG                         09720000
         SLL   R1,1               MOVE NEXT BIT INTO SIGN BIT           09730000
         B     DSORGLP            AND TEST IT                           09740000
NODSORG  LA    OUTPTR,2(OUTPTR)   -> NEXT FREE SPOT                     09750000
         SPACE                                                          09760000
* RECFM                                                                 09770000
         SPACE                                                          09780000
         MVC   0(8,OUTPTR),=C'RECFM **' LABEL                           09790000
         LR    R5,OUTPTR          SAVE OUTPTR                           09800000
         LA    OUTPTR,8(OUTPTR)   -> PAST RECFM VALUE                   09810000
         SR    R1,R1              CLEAR FOR INSERT                      09820000
         ICM   R1,B'0001',DS1RECFM   PICK UP RECFM                      09830000
         BZ    NORECFM               NONE                               09840000
         LA    OUTPTR,6(R5)       BACK UP PTR TO RECFM VALUE AREA       09850000
         MVI   1(OUTPTR),C' '     IN CASE OF ONE-CHAR RECFM VALUE       09860000
         LA    R2,B'11000000'     MASK                                  09870000
         NR    R2,R1              GET THE RECFM F,V,U BITS              09880000
         BZ    DORECFM            NOT ANY                               09890000
         SRL   R2,6               PUT AT BOTTOM                         09900000
         IC    R0,RECFMTBL-1(R2)  PICK UP THE CORRECT RECFM             09910000
         STC   R0,0(,OUTPTR)      PUT IN OUTPUT LINE                    09920000
         LA    OUTPTR,1(OUTPTR)   -> PAST                               09930000
DORECFM  LA    R0,B'00111110'     MASK                                  09940000
         NR    R1,R0              ZERO F,V,U RECFM BITS                 09950000
         LA    R2,RECFMTBL+3      -> REST OF RECFMS                     09960000
         SLL   R1,26              PUT IN TOP BYTE REG                   09970000
RECFMLP  LTR   R1,R1              TEST THAT TOP BIT                     09980000
         BZ    NORECFM            ALL DONE                              09990000
         BNM   NXTRECFM           BIT NOT SET                           10000000
         MVC   0(1,OUTPTR),0(R2)  MOVE IN RECFM                         10010000
         LA    OUTPTR,1(OUTPTR)   -> NEXT SPOT                          10020000
NXTRECFM LA    R2,1(R2)           -> NEXT RECFM                         10030000
         SLL   R1,1               MOVE NEXT BIT INTO SIGN BIT           10040000
         B     RECFMLP            AND TEST IT                           10050000
NORECFM  LA    OUTPTR,2(OUTPTR)   -> NEXT AREA                          10060000
         EJECT                                                          10070000
         SPACE                                                          10080000
* BLKSIZE                                                               10090000
         SPACE                                                          10100000
         MVC   0(8,OUTPTR),=C'BLKSIZE ' LABEL                           10110000
         LA    R1,8(OUTPTR)       -> OUTPUT POSTION FOR BLKSIZE         10120000
         SLR   R0,R0              BLKSIZE                               10130000
         ICM   R0,3,DS1BLKL            "                                10140000
         BAL   LINK,INSRTNUM      OUTPUT IT                             10150000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          10160000
         SPACE                                                          10170000
* LRECL                                                                 10180000
         SPACE                                                          10190000
         MVC   0(6,OUTPTR),=C'LRECL ' LABEL                             10200000
         LA    R1,6(OUTPTR)       -> OUTPUT POSITION FOR LRECL          10210000
         SLR   R0,R0              LRECL                                 10220000
         ICM   R0,3,DS1LRECL           "                                10230000
         BAL   LINK,INSRTNUM      OUTPUT IT                             10240000
         LA    OUTPTR,2(,R1)      -> PAST LRECL                         10250000
         SPACE 2                                                        10260000
         TM    DS1DSORG+1,X'08'   A VSAM DATASET ???  (DSORG = AM)      10270000
         BO    TPUT               YES - ALL DONE                        10280000
         SPACE 2                                                        10290000
* OPTCD                                                                 10300000
         SPACE                                                          10310000
         CLI   DS1OPTCD,0         ANY OPTCD ??                          10320000
         BZ    NOOPTCD            NOPE                                  10330000
         SPACE                                                          10340000
         MVC   0(6,OUTPTR),=C'OPTCD ' LABEL                             10350000
         LA    OUTPTR,6(OUTPTR)   -> PAST                               10360000
         SPACE                                                          10370000
         TM    DS1DSORG,X'80'     ISAM ?                                10380000
         BZ    *+12               NO                                    10390000
         LA    R2,ISAMOPT         -> ISAM OPTCD TABLE                   10400000
         B     GOTAM                                                    10410000
         TM    DS1DSORG,X'42'     BSAM OR BPAM ?                        10420000
         BZ    *+12               NO                                    10430000
         LA    R2,BSAMOPT         -> BSAM OPTCD TABLE                   10440000
         B     GOTAM                                                    10450000
         TM    DS1DSORG,X'20'     BDAM ?                                10460000
         BZ    NOAM               NO                                    10470000
         LA    R2,BDAMOPT         -> BDAM OPTCD TABLE                   10480000
         B     GOTAM                                                    10490000
         SPACE                                                          10500000
NOAM     MVI   0(OUTPTR),C'?'     INDICATE OPTCD UNKNOWN                10510000
         LA    OUTPTR,1(,OUTPTR)  BUMP PAST                             10520000
         SR    R4,R4                                                    10530000
         ICM   R4,B'1000',DS1OPTCD    PICK UP OPTCD                     10540000
         B     OPTCDHEX               PUT OUT OPTCD IN HEX              10550000
         SPACE                                                          10560000
GOTAM    SR    R1,R1                                                    10570000
         ICM   R1,B'1000',DS1OPTCD    PICK UP OPTCD                     10580000
         LR    R4,R1              SAVE OPTCD FOR LATER CHECK            10590000
         ICM   R0,B'1000',0(R2)   SIGNIFICANT BIT MASK                  10600000
         NR    R1,R0              KEEP JUST BITS THAT WE KNOW ABOUT     10610000
OPTCDLP  LTR   R1,R1              TEST THAT TOP BIT                     10620000
         BZ    OPTCDCHK           ALL DONE                              10630000
         BNM   NXTOPTCD           BIT NOT SET                           10640000
         MVC   0(1,OUTPTR),1(R2)  MOVE IN OPTCD                         10650000
         LA    OUTPTR,1(OUTPTR)   -> NEXT SPOT                          10660000
NXTOPTCD LA    R2,1(R2)           -> NEXT OPTCD                         10670000
         SLL   R1,1               MOVE NEXT BIT INTO SIGN BIT           10680000
         B     OPTCDLP            AND TEST IT                           10690000
         SPACE 2                                                        10700000
OPTCDCHK SR    R3,R3              LOAD R3 WITH                          10710000
         ICM   R3,B'1000',=X'FF'  A ONE'S COMPLEMENT MASK               10720000
         XR    R3,R0              ONE'S COMPLEMENT OF OPTCD BIT MASK    10730000
         NR    R3,R4              LEAVE JUST UN-INTERPETED BITS         10740000
         BZ    OPTCDONE           ALL BITS INTERPETED                   10750000
OPTCDHEX LA    OUTPTR,1(OUTPTR)   SKIP A SPACE                          10760000
         MVC   0(3,OUTPTR),=C'(X'''      INDICATE HEX CONSTANT          10770000
         STCM  R4,B'1000',4(OUTPTR)      PUT DOWN OPTCD                 10780000
         UNPK  3(1,OUTPTR),4(1,OUTPTR)   REVERSE BYTE ON LEFT           10790000
         NC    3(2,OUTPTR),HEXMASK       PUT WITHIN RANGE OF TR         10800000
         TR    3(2,OUTPTR),HEXTABLE      CONVERT TO HEX PRINTABLE       10810000
         MVC   5(2,OUTPTR),=C''')'       CLOSE QUOTE AND PAREN          10820000
         LA    OUTPTR,7(OUTPTR)   -> PAST                               10830000
         SPACE                                                          10840000
OPTCDONE LA    OUTPTR,2(OUTPTR)                                         10850000
         SPACE                                                          10860000
NOOPTCD  DS    0H                 DONE WITH OPTCD                       10870000
         SPACE                                                          10880000
* PSWD                                                                  10890000
         SPACE                                                          10900000
         TM    DS1DSIND,DS1IND10  DATASET PROTECTED ?                   10910000
         BZ    TESTRACF           NOPE - CHECK FOR RACF DEFINED         10920000
         SPACE                                                          10930000
         MVC   0(4,OUTPTR),=C'PSWD'                                     10940000
         TM    DS1DSIND,DS1IND04  R/W OR JUST WRITE ?                   10950000
         BZ    PSWDRW             ITS R/W                               10960000
         MVI   5(OUTPTR),C'W'                                           10970000
         LA    OUTPTR,6(OUTPTR)                                         10980000
         B     TESTRACF                                                 10990000
PSWDRW   MVC   5(3,OUTPTR),=C'R/W'                                      11000000
         LA    OUTPTR,8(OUTPTR)                                         11010000
         SPACE                                                          11020000
TESTRACF TM    DS1DSIND,DS1IND40  DATASET DEFINED TO RACF ?             11030000
         BZ    PSWDONE            NOPE                                  11040000
         TM    DS1DSIND,DS1IND10  DID DATASET HAVE A PSWD ?             11050000
         BO    PSWDLABL           YES - HAVE A LABEL                    11060000
         MVC   0(9,OUTPTR),=C'PSWD RACF'                                11070000
         LA    OUTPTR,9(OUTPTR)   -> NEXT AREA                          11080000
         B     PSWDONE                                                  11090000
PSWDLABL MVC   0(5,OUTPTR),=C',RACF' ADD RACF INDICATOR                 11100000
         LA    OUTPTR,5(OUTPTR)   -> NEXT AREA                          11110000
         SPACE 2                                                        11120000
PSWDONE  BAL   LINK,NEWLINE       NEXT LINE                             11130000
         TITLE 'DATASET INQUIRY FUNCTION                GET CREATE DATE*11140000
               ,EXPIRE DATE,KEYLEN,RKP AND # EXTENTS'                   11150000
         SPACE                                                          11160000
* CREATION DATE                                                         11170000
         SPACE                                                          11180000
         MVC  0(2,OUTPTR),=C'C-'  LABEL                                 11190000
         LA   R0,2(OUTPTR)        -> CREATE DATE VALUE POSTION          11200000
         LA   OUTPTR,12(OUTPTR)   -> PAST CREATE DATE POSITION          11210000
         LA   R1,DS1CREDT         -> CREATE DATE                        11220000
         BAL  LINK,CNVTDATE       CONVERT DATE TO MM/DD/YY FORMAT       11230000
         SPACE                                                          11240000
* EXPIRATION DATE                                                       11250000
         SPACE                                                          11260000
         OC   DS1EXPDT,DS1EXPDT   ANY EXPIRATION DATE ?                 11270000
         BZ   NOEXPDT             NO - SKIP IT                          11280000
         SPACE                                                          11290000
         MVC  0(2,OUTPTR),=C'E-'  LABEL                                 11300000
         LA   R0,2(OUTPTR)        -> EXPIRATION DATE VALUE POSTION      11310000
         LA   OUTPTR,12(OUTPTR)   -> PAST EXPIRATION DATE POSITION      11320000
         LA   R1,DS1EXPDT         -> EXPIRATION DATE                    11330000
         BAL  LINK,CNVTDATE       CONVERT DATE TO MM/DD/YY FORMAT       11340000
NOEXPDT  EQU  *                                                         11350000
         SPACE                                                          11360000
* KEYLEN                                                                11370000
         SPACE                                                          11380000
         MVC   0(7,OUTPTR),=C'KEYLEN ' LABEL                            11390000
         LA    R1,7(OUTPTR)       -> OUTPUT POSTION FOR KEYLEN          11400000
         SR    R0,R0              CLEAR FOR INSERT                      11410000
         IC    R0,DS1KEYL         KEYLEN                                11420000
         BAL   LINK,INSRTNUM      OUTPUT IT                             11430000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          11440000
         SPACE                                                          11450000
* RKP                                                                   11460000
         SPACE                                                          11470000
         MVC   0(4,OUTPTR),=C'RKP ' LABEL                               11480000
         LA    R1,4(OUTPTR)       -> OUTPUT POSITION FOR RKP            11490000
         SR    R0,R0              CLEAR FOR INSERT                      11500000
         ICM   R0,B'0011',DS1RKP  RKP                                   11510000
         BAL   LINK,INSRTNUM      OUTPUT IT                             11520000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          11530000
         SPACE                                                          11540000
* NUMBER OF EXTENTS                                                     11550000
         SPACE                                                          11560000
         MVC   0(6,OUTPTR),=C'#EXTS ' LABEL                             11570000
         LA    R1,6(OUTPTR)       -> OUTPUT POSTION FOR #EXTENTS        11580000
         SR    R0,R0              CLEAR FOR INSERT                      11590000
         IC    R0,DS1NOEPV        #EXTENTS                              11600000
         BAL   LINK,INSRTNUM      OUTPUT IT                             11610000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          11620000
         SPACE                                                          11630000
         CLI   DS1NOEPV,0         ZERO EXTENTS ???                      11640000
         BE    TPUT               YES - ALL DONE                        11650000
         SPACE                                                          11660000
         TITLE 'DATASET INQUIRY FUNCTION                CALCULATE RECOR*11670000
               D COUNT'                                                 11680000
         SPACE                                                          11690000
* RECORD COUNT                                                          11700000
         SPACE                                                          11710000
         TM    DS1DSORG,X'40'     DATASET PHYSICAL SEQUENTIAL ?         11720000
         BNO   NRECDCNT           NO GO                                 11730000
         IC    R1,DS1RECFM        RECFM                                 11740000
         LA    R0,B'11000000'     MASK                                  11750000
         NR    R1,R0              GET RECFM F,V,U BITS                  11760000
         LA    R0,B'10000000'     VALUE OF RECFM = F                    11770000
         CR    R1,R0              DATASET RECFM = F ?                   11780000
         BNE   NRECDCNT           CAN'T DO IT                           11790000
         ICM   R0,B'0011',DS1BLKL BLKSIZE ZERO ?                        11800000
         BZ    NRECDCNT           YES - BYPASS # RECDS CALCULATION      11810000
         SPACE                                                          11820000
         SLR   R4,R4              ASSUME # RECDS IS ZERO                11830000
         ICM   R0,B'0111',DS1LSTAR   LAST USED TTR ZERO ?               11840000
         BZ    NOTBLKED              YES - GO OUTPUT "RECDS 0"          11850000
         SPACE                                                          11860000
         L     @DVCT,DVCTPTR      PICK ADDR OF DVCT ENTRY               11870000
         USING DVCT,@DVCT         ADDRESSABILITY                        11880000
         SR    R4,R4                                                    11890000
         IC    R4,DS1KEYL         KEYLENGTH                             11900000
         SLR   R1,R1              BLKSIZE                               11910000
         ICM   R1,3,DS1BLKL            "                                11920000
         AR    R4,R1              + BLKSIZE = LOGICAL BLOCK SIZE        11930000
         LR    R1,R4              SAVE                                  11940000
         SPACE                                                          11950000
         TM    DVCFLAGS,DVCFTOL   APPLY TOLERANCE FACTOR ??             11960000
         BNO   NOTOL1             NOPE                                  11970000
         SPACE                                                          11980000
         MH    R4,DVCTOL          * TOLERANCE                           11990000
         SRA   R4,DVCTSHFT        / 512 = BLOCKLEN (EXCLUDING OVERHEAD) 12000000
         SPACE                                                          12010000
NOTOL1   TM    DVCFLAGS,DVC2BOV   SPLIT OVERHEAD VALUE ?                12020000
         BNO   SPLTOVHD           YES                                   12030000
         SPACE                                                          12040000
         LH    R5,DVCOVHD         OVERHEAD PER BLOCK                    12050000
         AR    R5,R4              PHYSICAL BLOCK LENGTH                 12060000
         CLI   DS1KEYL,0          DATASET KEYED ?                       12070000
         BNZ   KEYOVHD            YES, DON'T ADJUST BLOCK LENGTH        12080000
         SR    R0,R0              NEED TO TAKE OUT KEY OVERHEAD         12090000
         IC    R0,DVCOVNK         OVERHEAD REDUCTION IF NOT KEYED       12100000
         SR    R5,R0              PHYSICAL LENGTH OF NON-KEYED BLOCK    12110000
KEYOVHD  SLR   R3,R3              BYTES PER TRACK                       12120000
         ICM   R3,3,DVCTRKLN           "                                12130000
         SR    R2,R2              CLEAR FOR DIVIDE                      12140000
         DR    R2,R5              R3 = BLOCKS / TRACK                   12150000
         B     BLKSTRK            JOIN MAINLINE CODE AGAIN              12160000
         SPACE                                                          12170000
SPLTOVHD EQU   *                  OVERHEAD DIFFERENT FOR LAST BLOCK     12180000
         SR    R5,R5                                                    12190000
         IC    R5,DVCOVNLB        OVERHEAD IF NOT LAST BLOCK            12200000
         AR    R5,R4              PHYSICAL BLOCK LEN IF NOT LAST BLOCK  12210000
         SR    R6,R6                                                    12220000
         IC    R6,DVCOVLB         OVERHEAD IF LAST BLOCK                12230000
         AR    R6,R1              PHYSICAL BLOCK LENGTH IF LAST BLOCK   12240000
         CLI   DS1KEYL,0          DATASET KEYED ??                      12250000
         BNZ   KEYOVRHD           YES, DON'T ADJUST BLOCK LENGTHS       12260000
         SR    R0,R0              NEED TO TAKE OUT KEY OVERHEAD         12270000
         IC    R0,DVCOVNK         OVERHEAD REDUCTION IF NOT KEYED       12280000
         SR    R5,R0              PHYSICAL BLOCK LEN IF NOT LAST BLOCK  12290000
         SR    R6,R0              PHYSICAL BLOCK LENGTH IF LAST BLOCK   12300000
KEYOVRHD SLR   R3,R3              BYTES PER TRACK                       12310000
         ICM   R3,3,DVCTRKLN           "                                12320000
         SR    R3,R6              SUBTRACT LENGTH OF LAST BLOCK         12330000
         SR    R2,R2              CLEAR FOR DIVIDE                      12340000
         DR    R2,R5              (BYTES/TRK-LENLASTBLK)  /  BLKLEN     12350000
         LA    R1,1               ADD ONE TO                            12360000
         AR    R3,R1              ACCOUNT FOR LAST BLOCK                12370000
         SPACE                                                          12380000
BLKSTRK  EQU   *                  R3 = BLKS/TRK, R5 = PHYSICAL BLKLEN   12390000
         LR    R6,R3              SAVE BLKS/TRK FOR MAX CHECK           12400000
         SR    R1,R1                                                    12410000
         ICM   R1,B'0011',DS1LSTAR   NUMBER OF TRACKS IN DATASET        12420000
         MR    R2,R1              #TRKS  *  BLKS/TRK = #BLKS - LAST TRK 12430000
         SR    R0,R0                                                    12440000
         IC    R0,DS1LSTAR+2      NUMBER OF BLOCKS ON LAST TRACK        12450000
         CR    R0,R6              MORE THAN CAN FIT ON A TRACK ?        12460000
         BNH   RECNUMOK           NOPE - AS IS                          12470000
         LR    R0,R6              NO SET TO MAX                         12480000
         STC   R6,DS1LSTAR+2      AND FIX TTR IN DSCB TOO               12490000
RECNUMOK AR    R3,R0              R3 = NUMBER OF BLOCKS IN DATASET      12500000
         LR    R4,R3              IN CASE DS IS NOT BLOCKED             12510000
         SPACE                                                          12520000
         OC    DS1LRECL,DS1LRECL  LRECL ZERO ?                          12530000
         BZ    NOTBLKED           YES - DS NOT BLOCKED                  12540000
         CLC   DS1LRECL,DS1BLKL   OR EQUAL TO BLOCKSIZE ?               12550000
         BE    NOTBLKED           YES - DS NOT BLOCKED                  12560000
         SPACE                                                          12570000
         BCTR  R3,0               EXCLUDE LAST BLOCK (PROBABLY SHORT)   12580000
         SLR   R1,R1              BLKSIZE                               12590000
         ICM   R1,3,DS1BLKL            "                                12600000
         SLR   R2,R2              LRECL                                 12610000
         ICM   R2,3,DS1LRECL           "                                12620000
         SR    R0,R0              CLEAR FOR DIVIDE                      12630000
         DR    R0,R2              NUMBER OF LOGICAL RECORDS PER BLOCK   12640000
         MR    R0,R3              * (#BLOCKS IN DS -1) = # RECDS IN DS  12650000
         LR    R4,R1                                  (EXCEPT LAST BLK) 12660000
         SPACE                                                          12670000
         SR    R1,R1                                                    12680000
         IC    R1,DS1LSTAR+2      NUMBER OF BLOCKS ON LAST TRACK        12690000
         BCTR  R1,0               EXCLUDE LAST BLOCK                    12700000
         MR    R0,R5              BYTES USED LAST TRK EXCEPT LAST BLK   12710000
         SLR   R3,R3              BYTES ON A TRACK                      12720000
         ICM   R3,3,DVCTRKLN           "                                12730000
         SR    R3,R1              - BYTES USED EXCEPT LAST BLOCK        12740000
         SH    R3,DS1TRBAL        - FREE BYTES ON LAST TRACK           *12750000
                                  = PHYSICAL LENGTH OF LAST BLOCK       12760000
         SPACE                                                          12770000
         TM    DVCFLAGS,DVC2BOV   SPLIT OVERHEAD VALUE ??               12780000
         BO    SNGLOVHD           NOPE                                  12790000
         SPACE                                                          12800000
         SR    R0,R0                                                    12810000
         IC    R0,DVCOVNLB        OVERHEAD NOT LAST BLOCK               12820000
         SR    R3,R0              ACCOUNT FOR OVERHEAD                  12830000
         B     GOTOVHD                                                  12840000
         SPACE                                                          12850000
SNGLOVHD SH    R3,DVCOVHD         ACCOUNT FOR OVERHEAD PER BLOCK        12860000
         SPACE                                                          12870000
GOTOVHD  CLI   DS1KEYL,0          DATASET KEYED ?                       12880000
         BNZ   NKEYOVHD           YES - DON'T ADJUST                    12890000
         SR    R0,R0                                                    12900000
         IC    R0,DVCOVNK         NOT KEYED CONSTANT                    12910000
         AR    R3,R0              ACCOUNT FOR DS NOT KEYED              12920000
NKEYOVHD TM    DVCFLAGS,DVCFTOL   APPLY TOLERANCE FACTOR ??             12930000
         BNO   NOTOL2             NOPE                                  12940000
         SPACE                                                          12950000
         SLA   R3,DVCTSHFT        * 512                                 12960000
         LH    R1,DVCTOL          TOLERANCE                             12970000
         SR    R2,R2              SET UP FOR DIVIDE                     12980000
         DR    R2,R1              / TOLERANCE = LAST BLOCK LOG. LENGTH  12990000
         SPACE                                                          13000000
NOTOL2   SLR   R1,R1              LRECL                                 13010000
         ICM   R1,3,DS1LRECL           "                                13020000
         SR    R2,R2                                                    13030000
         DR    R2,R1              LOGICAL RECORDS IN LAST BLOCK         13040000
         AR    R4,R3              ADD TO TOTAL                          13050000
         SPACE                                                          13060000
         DROP  @DVCT                                                    13070000
         SPACE                                                          13080000
NOTBLKED MVC   0(7,OUTPTR),=C'#RECDS '   LABEL                          13090000
         LA    R1,7(,OUTPTR)         -> OUTPUT AREA FOR #RECDS          13100000
         LR    R0,R4                 NUMBER OF RECORDS IN DATASET       13110000
         BAL   LINK,INSRTNUM         OUTPUT IT                          13120000
         LA    OUTPTR,2(R1)          -> NEXT AREA                       13130000
         SPACE 2                                                        13140000
NRECDCNT BAL   LINK,NEWLINE       NEXT LINE                             13150000
         TITLE 'DATASET INQUIRY FUNCTION                GET ISAM STATIS*13160000
               TICS'                                                    13170000
         SPACE                                                          13180000
         TM    DS1DSORG,X'80'     ISAM DATASET ?                        13190000
         BNO   NOTISAM            NOPE                                  13200000
         SPACE                                                          13210000
* PRIME RECORDS                                                         13220000
         SPACE                                                          13230000
         MVC   0(11,OUTPTR),=C'PRIME RECDS'                             13240000
         LA    R1,12(,OUTPTR)     -> OUTPUT AREA                        13250000
         L     R0,DS2PRCTR        NUM OF PRIME RECORDS                  13260000
         BAL   LINK,INSRTNUM      WRITE IT OUT                          13270000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          13280000
         SPACE                                                          13290000
* DELETED RECORDS                                                       13300000
         SPACE                                                          13310000
         MVC   0(9,OUTPTR),=C'DEL RECDS'                                13320000
         LA    R1,10(,OUTPTR)     -> OUTPUT AREA                        13330000
         LH    R0,DS2TAGDT        NUM OF RECORDS TAGGED FOR DELETION    13340000
         BAL   LINK,INSRTNUM      WRITE IT OUT                          13350000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          13360000
         SPACE                                                          13370000
* OVERFLOW RECORDS                                                      13380000
         SPACE                                                          13390000
         MVC   0(9,OUTPTR),=C'OFL RECDS'                                13400000
         LA    R1,10(,OUTPTR)     -> OUTPUT AREA                        13410000
         LH    R0,DS2OVRCT        NUM OF RECORDS IN OVERFLOW AREA       13420000
         BAL   LINK,INSRTNUM      WRITE IT OUT                          13430000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          13440000
         SPACE                                                          13450000
* OVERFLOW REFERENCE COUNT                                              13460000
         SPACE                                                          13470000
         MVC   0(7,OUTPTR),=C'OFL REF'                                  13480000
         LA    R1,8(,OUTPTR)      -> OUTPUT AREA                        13490000
         SR    R0,R0                                                    13500000
         ICM   R0,B'0111',DS2RORG3   NON-FIRST OVERFLOW REFERNCE COUNT  13510000
         BAL   LINK,INSRTNUM      WRITE IT OUT                          13520000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          13530000
         SPACE                                                          13540000
         BAL   LINK,NEWLINE       NEXT LINE                             13550000
         SPACE                                                          13560000
* INDEX SIZE IN BYTES                                                   13570000
         SPACE                                                          13580000
         MVC   0(11,OUTPTR),=C'INDEX BYTES'                             13590000
         LA    R1,12(,OUTPTR)     -> OUTPUT AREA                        13600000
         LH    R0,DS2NOBYT        NUM OF BYTES NEEDED FOR HIGH INDEX    13610000
         BAL   LINK,INSRTNUM      WRITE IT OUT                          13620000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          13630000
         EJECT                                                          13640000
         SPACE                                                          13650000
* OVERFLOW AREA REMAINING                                               13660000
         SPACE                                                          13670000
         MVC   0(13,OUTPTR),=C'OFL AREA LEFT'                           13680000
         LA    R1,14(,OUTPTR)     -> OUTPUT AREA                        13690000
         LH    R0,DS2RORG2        NUM OF TRACKS REMAINING IN OFL AREA   13700000
         BAL   LINK,INSRTNUM      WRITE IT OUT                          13710000
         MVC   1(3,R1),=C'TRK'    LABEL                                 13720000
         LA    R1,5(R1)           -> OUTPUT AREA                        13730000
         LH    R0,DS2BYOVL        BYTES REMAINING ON OFL TRACK          13740000
         BAL   LINK,INSRTNUM      WRITE IT OUT                          13750000
         MVC   1(5,R1),=C'BYTES'  LABEL                                 13760000
         LA    OUTPTR,8(R1)       -> NEXT AREA                          13770000
         SPACE                                                          13780000
* FULL CYLINDER OVERFLOW AREAS                                          13790000
         SPACE                                                          13800000
         MVC   0(11,OUTPTR),=C'CYLOFL FULL'                             13810000
         LA    R1,12(,OUTPTR)     -> OUTPUT AREA                        13820000
         LH    R0,DS2RORG1        NUM OF FULL CYLINDER OVERFLOW AREAS   13830000
         BAL   LINK,INSRTNUM      WRITE IT OUT                          13840000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          13850000
         SPACE                                                          13860000
         BAL   LINK,NEWLINE       NEXT LINE                             13870000
         SPACE 2                                                        13880000
NOTISAM  DS    0H                 SKIP ISAM STATISTICS                  13890000
         TITLE 'DATASET INQUIRY FUNCTION                GET ASM2/SU60 U*13900000
               SAGE INFORMATION'                                        13910000
         SPACE                                                          13920000
* LAST USE DATE                                                         13930000
         SPACE                                                          13940000
         CLC  ASM2UCNT,=F'0'      SHOULD BE A USE CNT OR LAST USE DATE  13950000
         BE   NOASM2              NO USAGE STATS AVAILABLE              13960000
         SPACE                                                          13970000
         MVC  0(2,OUTPTR),=C'U-'  LABEL                                 13980000
         LA   R0,2(OUTPTR)        -> LAST USE DATE VALUE POSTION        13990000
         LA   OUTPTR,12(OUTPTR)   -> PAST LAST USE DATE POSITION        14000000
         LA   R1,ASM2UDAT         ASSUME OLD ASM2 (2.3) FORMAT          14010000
         CLI  DS1REFD,0           IS IT ?                               14020000
         BE   *+8                 YEP                                   14030000
         LA   R1,DS1REFD          NO, POINT TO ASM2 2.4 USE DATE LOC    14040000
         BAL  LINK,CNVTDATE       CONVERT DATE TO MM/DD/YY FORMAT       14050000
         SPACE                                                          14060000
* MVS SU 60 CHANGE BIT                                                  14070000
         SPACE                                                          14080000
         CLC   DS1DSSN,VOLSER     IS VOLSER FIELD IN DSCB UNTOUCHED ?   14090000
         BNE   ISASM2             NO - WE SHOULD HAVE ASM2 STATS THEN   14100000
         TM    DS1DSIND,DS1IND02  HAS DATASET BEEN MODIFIED ?           14110000
         BZ    ASM2DONE           NOPE - ALL DONE                       14120000
         MVC   0(7,OUTPTR),=C'CHANGED'   INDICATE DS HAS BEEN CHANGED   14130000
         LA    OUTPTR,9(,OUTPTR)  -> PAST                               14140000
         B     ASM2DONE                                                 14150000
ISASM2   DS    0H                 DSCB HAS ASM2 STATS                   14160000
         SPACE                                                          14170000
* LAST MODIFY DATE                                                      14180000
         SPACE                                                          14190000
         TM   ASM2MDAT+1,X'FE'    MODIFY DATE VALID ?                   14200000
         BNZ  NOMDATE             NO - SKIP IT                          14210000
         SPACE                                                          14220000
         MVC  0(2,OUTPTR),=C'M-'  LABEL                                 14230000
         LA   R0,2(OUTPTR)        -> MOD DATE VALUE POSTION             14240000
         LA   OUTPTR,12(OUTPTR)   -> PAST MOD DATE POSITION             14250000
         LA   R1,ASM2MDAT         -> MOD DATE                           14260000
         BAL  LINK,CNVTDATE       CONVERT DATE TO MM/DD/YY FORMAT       14270000
NOMDATE  EQU  *                                                         14280000
         SPACE                                                          14290000
* USE COUNT                                                             14300000
         SPACE                                                          14310000
         MVC   0(4,OUTPTR),=C'USE ' LABEL                               14320000
         LA    R1,4(OUTPTR)       -> OUTPUT POSTION FOR USE COUNT       14330000
         L     R0,ASM2UCNT        ASSUME OLD ASM2 (2.3) FORMAT          14340000
         CLI   DS1REFD,0          IS IT ?                               14350000
         BE    *+10               YES                                   14360000
         SR    R0,R0              NO, PICK UP NEW ASM2 2.4 USE COUNT    14370000
         ICM   R0,B'0111',ASM2USEC                                      14380000
         BAL   LINK,INSRTNUM      OUTPUT IT                             14390000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          14400000
         SPACE                                                          14410000
* MODIFYING JOBNAME                                                     14420000
         SPACE                                                          14430000
         TM   ASM2MDAT+1,X'FE'    MODIFY DATE VALID ?                   14440000
         BNZ  ASM2DONE            INVALID - NO MODIFYING JOB            14450000
         SPACE                                                          14460000
         MVC   0(5,OUTPTR),=C'USER '  LABEL                             14470000
         MVC   5(8,OUTPTR),ASM2MJOB   MODIFYING JOBNAME                 14480000
         LA    OUTPTR,15(OUTPTR)  -> NEXT AREA                          14490000
         SPACE 2                                                        14500000
ASM2DONE BAL   LINK,NEWLINE       NEXT LINE                             14510000
         SPACE                                                          14520000
NOASM2   DS    0H                 BYPASS ASM2 STATS                     14530000
         TITLE 'DATASET INQUIRY FUNCTION                GET ALLOCATION *14540000
               TYPE AND SECONDARY AMOUNT'                               14550000
         SPACE                                                          14560000
* ALLOCATION TYPE                                                       14570000
         SPACE                                                          14580000
         MVC   0(9,OUTPTR),=C'ALLOC IN ' LABEL                          14590000
         LA    OUTPTR,9(OUTPTR)   -> ALLOCATION TYPE VALUE POSITION     14600000
         TM    DS1SCALO,B'11000000'   TEST ALLOCATION TYPE              14610000
         BM    CHKALOC1               IN TRACKS OR BLOCKS               14620000
         BZ    INABSTR                IN ABSOLUTE TRACK                 14630000
         MVC   0(3,OUTPTR),=C'CYL'    IN CYLINDERS                      14640000
         LA    OUTPTR,3(OUTPTR)       BUMP POINTER                      14650000
         B     CHKALOC2               CHECK THE BOTTOM 4 BITS           14660000
INABSTR  MVC   0(5,OUTPTR),=C'ABSTR'  IN ABSOLUTE TRACK                 14670000
         LA    OUTPTR,5(OUTPTR)       BUMP POINTER                      14680000
         B     CHKALOC2               CHECK NEXT SET OF BITS            14690000
CHKALOC1 TM    DS1SCALO,B'10000000'   CHECK IF ALLOCATED IN TRACKS      14700000
         BNO   INBLKS                 NO - IN BLOCKS                    14710000
         MVC   0(3,OUTPTR),=C'TRK'    IN TRACKS                         14720000
         LA    OUTPTR,3(OUTPTR)       BUMP POINTER                      14730000
         B     CHKALOC2               CHECK NEXT BUNCHA BITS            14740000
INBLKS   MVC   0(3,OUTPTR),=C'BLK'    IN BLOCKS                         14750000
         LA    OUTPTR,3(OUTPTR)       BUMP POINTER                      14760000
         SPACE                                                          14770000
CHKALOC2 TM    DS1SCALO,B'00001000'   CONTIG ?                          14780000
         BNO   NOCONTIG               NOPE                              14790000
         MVC   0(7,OUTPTR),=C',CONTIG' SAY SO                           14800000
         LA    OUTPTR,7(OUTPTR)       BUMP POINTER                      14810000
NOCONTIG TM    DS1SCALO,B'00000100'   MAXIMUM CONTIG EXTENT ON VOL ?    14820000
         BNO   NOMXIG                 NOPE                              14830000
         MVC   0(5,OUTPTR),=C',MXIG'  WHAT A PIG                        14840000
         LA    OUTPTR,5(OUTPTR)       BUMP POINTER                      14850000
NOMXIG   TM    DS1SCALO,B'00000010'   5 BIG ONES ??                     14860000
         BNO   NO5BIGYS               NOPE                              14870000
         MVC   0(4,OUTPTR),=C',ALX'   ANOTHER HOG                       14880000
         LA    OUTPTR,4(OUTPTR)       BUMP POINTER                      14890000
NO5BIGYS TM    DS1SCALO,B'00000001'   ROUND ?                           14900000
         BNO   NOROUND                NOPE                              14910000
         MVC   0(6,OUTPTR),=C',ROUND' YEP                               14920000
         LA    OUTPTR,6(OUTPTR)       BUMP POINTER                      14930000
NOROUND  LA    OUTPTR,2(OUTPTR)   END OF ALLOCATION TYPE BITS           14940000
         SPACE 2                                                        14950000
* SECONDARY ALLOCATION                                                  14960000
         SPACE                                                          14970000
         MVC   0(4,OUTPTR),=C'SEC ' LABEL                               14980000
         LA    R1,4(OUTPTR)       -> OUTPUT POSITION FOR SEC ALLOC AMT  14990000
         SR    R0,R0              CLEAR FOR INSERT                      15000000
         ICM   R0,B'0111',DS1SCALO+1    SEC ALLOC AMT                   15010000
         BAL   LINK,INSRTNUM      OUTPUT IT                             15020000
         LA    OUTPTR,2(R1)       -> NEXT AREA                          15030000
         TITLE 'DATASET INQUIRY FUNCTION                GET TOTAL SPACE*15040000
                ALLOCATED FOR DATASET'                                  15050000
         SPACE                                                          15060000
* TOTAL ALLOCATION                                                      15070000
         SPACE                                                          15080000
         MVC   0(6,OUTPTR),=C'TOTAL ' LABEL                             15090000
         LA    OUTPTR,6(OUTPTR)   -> PAST                               15100000
         SR    R2,R2              CLEAR TRK COUNTER                     15110000
         SR    R3,R3              CLEAR CYL COUNTER                     15120000
         SPACE                                                          15130000
         CLI   DS1NOEPV,0         ANY EXTENTS ?                         15140000
         BE    EXTDONE            NOPE                                  15150000
         SPACE                                                          15160000
         LA    R4,DS1EXT1         -> 1ST 3 EXTENTS IN F1 DSCB           15170000
         LA    R5,3               3 EXTENTS                             15180000
         BAL   LINK,ADDEXT        ADD IT UP                             15190000
         TM    FLAGS,HAVEF3       DO WE HAVE A F3 DSCB ?                15200000
         BNO   EXTDONE            NOPE                                  15210000
         SPACE                                                          15220000
         LA    R4,DS3EXTNT        -> 1ST 4 EXTENTS IN F3 DSCB           15230000
         LA    R5,4               4 EXTENTS                             15240000
         BAL   LINK,ADDEXT        ADD THEM IN                           15250000
         LA    R4,DS3ADEXT        -> LAST 9 EXTENTS IN F3 DSCB          15260000
         LA    R5,9               9 EXTENTS                             15270000
         LA    LINK,EXTDONE       FALL THRU TO ADDEXT                   15280000
         SPACE                                                          15290000
ADDEXT   EQU   *                  LOCAL SUBROUTINE TO ADD UP EXTENTS    15300000
         SR    R1,R1              CLEAR FOR                             15310000
         SR    R0,R0              INSERTS                               15320000
         CLI   0(R4),X'00'        EMPTY EXTENT ??                       15330000
         BE    EXTDONE            YES, FINISHED                         15340000
         ICM   R1,B'0011',6(R4)   HIGH CC                               15350000
         ICM   R0,B'0011',2(R4)   LOW  CC                               15360000
         SR    R1,R0              TOTAL CYL FOR THIS EXTENT             15370000
         AR    R3,R1              ADD TO TOTAL                          15380000
         ICM   R1,B'0011',8(R4)   HIGH HH                               15390000
         LA    R1,1(R1)           BUMP BY ONE                           15400000
         ICM   R0,B'0011',4(R4)   LOW  HH                               15410000
         SR    R1,R0              TOTAL TRK FOR THIS EXTENT             15420000
         AR    R2,R1              ADD TO TOTAL                          15430000
         LA    R4,10(R4)          -> NEXT EXTENT                        15440000
         BCT   R5,ADDEXT          GET REST IN THIS GROUP                15450000
         BR    LINK               RETURN                                15460000
         SPACE 2                                                        15470000
EXTDONE  EQU   *                  EXIT WHEN ALL EXTENTS ADDED UP        15480000
         LR    R1,R2              SAVE NUMBER OF TRACKS                 15490000
         M     R2,TRKCYL          CONVERT CYLINDER COUNT TO TRACKS      15500000
         AR    R3,R1              ADD IN TRACKS FOR TOTAL SPACE IN TRKS 15510000
         D     R2,TRKCYL          GET CYLINDERS AND TRACKS              15520000
         SPACE                                                          15530000
         BAL   R5,CYLTRK          WRITE OUT AMT IN CYLS AND TRKS        15540000
         TITLE 'DATASET INQUIRY FUNCTION                GET SPACE USED *15550000
               IN DATASET'                                              15560000
         SPACE                                                          15570000
* SPACE USED                                                            15580000
         SPACE                                                          15590000
         TM    DS1DSORG,X'80'     ISAM DATASET ??                       15600000
         BO    SPCDONE            YES - SKIP                            15610000
         SPACE                                                          15620000
         MVC   0(5,OUTPTR),=C'USED ' LABEL                              15630000
         LA    OUTPTR,5(OUTPTR)   -> PAST                               15640000
         SR    R2,R2              CLEAR FOR INSERT                      15650000
         ICM   R2,B'0011',DS1LSTAR  TT OF LAST BLOCK WRITTEN TTR        15660000
         CLI   DS1LSTAR+2,0       USED PART OF A TRACK ?                15670000
         BE    EVENTRK            NO                                    15680000
         LA    R2,1(R2)           YES - BUMP TRACK COUNT                15690000
EVENTRK  SR    R3,R3              ZERO CYL AMT IN CASE TRKS ONLY        15700000
         C     R2,TRKCYL          OVER A CYL ?                          15710000
         BL    TRKOK              NO - SKIP CYLINDER AMT                15720000
         LR    R3,R2              BUILD 64-BIT                          15730000
         SR    R2,R2              DIVIDEND FOR DIVIDE                   15740000
         D     R2,TRKCYL          GET CYLINDERS AND TRACKS              15750000
TRKOK    LA    R5,SPCDONE         FALL THRU TO CYLTRK SUBROUTINE        15760000
         SPACE 2                                                        15770000
CYLTRK   EQU   *                  LOCAL SUBR TO OUTPUT IN CYL AND TRKS  15780000
         LTR   R3,R3              ANY CYLINDERS ?                       15790000
         BZ    TRKONLY            NOPE                                  15800000
         SPACE                                                          15810000
         LR    R0,R3              NUMBER OF CYLINDERS                   15820000
         LR    R1,OUTPTR          -> OUTPUT POSITION FOR CYLINDER AMT   15830000
         BAL   LINK,INSRTNUM      OUTPUT IT                             15840000
         MVC   1(3,R1),=C'CYL'    ADD LABEL                             15850000
         LA    OUTPTR,5(R1)       -> NEXT AREA                          15860000
         SPACE                                                          15870000
         LTR   R2,R2              DO WE NEED TO OUTPUT TRKS ?           15880000
         BNZ   TRKONLY            YES                                   15890000
         LA    OUTPTR,1(OUTPTR)   NO - PUT 2 SPACES AFTER CYL AMT       15900000
         BR    R5                 RETURN                                15910000
         SPACE                                                          15920000
TRKONLY  LR    R0,R2              NUMBER OF TRACKS                      15930000
         LR    R1,OUTPTR          -> OUTPUT POSITION FOR TRACK AMT      15940000
         BAL   LINK,INSRTNUM      OUTPUT IT                             15950000
         MVC   1(3,R1),=C'TRK'    ADD LABEL                             15960000
         LA    OUTPTR,6(R1)       -> NEXT AREA                          15970000
         BR    R5                 RETURN                                15980000
         SPACE                                                          15990000
SPCDONE  EQU   *                                                        16000000
         TITLE 'DATASET INQUIRY FUNCTION                DESCRIBE EXTENT*16010000
               S OF DATASET'                                            16020000
         SPACE                                                          16030000
* EXTENT DESCRIPTIONS                                                   16040000
         SPACE                                                          16050000
         BAL   LINK,NEWLINE       ADVANCE TO NEXT LINE                  16060000
         SPACE                                                          16070000
         MVC   LINE(L'XTENTHDR),XTENTHDR  MOVE IN HEADER LINE           16080000
         LA    OUTPTR,LINE+L'XTENTHDR     -> END OF LINE                16090000
         BAL   LINK,NEWLINE               NEXT LINE                     16100000
         SPACE                                                          16110000
         LA    R4,DS1EXT1         -> 1ST 3 EXTENTS IN F1 DSCB           16120000
         LA    R5,3               3 EXTENTS                             16130000
         BAL   R6,WRTEXT          WRITE THEM OUT                        16140000
         TM    FLAGS,HAVEF3       DO WE HAVE A F3 DSCB ?                16150000
         BNO   DESCDONE           NOPE                                  16160000
         SPACE                                                          16170000
         LA    R4,DS3EXTNT        -> 1ST 4 EXTENTS IN F3 DSCB           16180000
         LA    R5,4               4 EXTENTS                             16190000
         BAL   R6,WRTEXT          OUTPUT THEM                           16200000
         LA    R4,DS3ADEXT        -> LAST 9 EXTENTS IN F3 DSCB          16210000
         LA    R5,9               9 EXTENTS                             16220000
         LA    R6,DESCDONE        FALL THRU TO WRTEXT                   16230000
         SPACE                                                          16240000
WRTEXT   EQU   *                  LOCAL SUBR TO WRITE OUT EXTENTS       16250000
DESCLOOP CLI   0(R4),X'00'        EMPTY EXTENT ??                       16260000
         BE    DESCDONE           YES, FINISHED                         16270000
         SPACE                                                          16280000
         SR    R2,R2              CLEAR FOR INSERTS                     16290000
         SPACE                                                          16300000
         IC    R2,1(R4)           EXTENT NUMBER                         16310000
         LA    R2,1(R2)           PUT IN RANGE 1-16                     16320000
         CVD   R2,DBLWRD          DECIMAL                               16330000
         MVC   EXTNUM,EXTMASK     EDIT MASK                             16340000
         ED    EXTNUM,DBLWRD+6    CHARACTER                             16350000
         SPACE                                                          16360000
         ICM   R2,B'0011',2(R4)   LOW CC                                16370000
         CVD   R2,DBLWRD          DECIMAL                               16380000
         MVC   LOWCC,EXTMASK      EDIT MASK                             16390000
         ED    LOWCC,DBLWRD+5     CHARACTER                             16400000
         SPACE                                                          16410000
         ICM   R2,B'0011',4(R4)   LOW HH                                16420000
         CVD   R2,DBLWRD          DECIMAL                               16430000
         OI    DBLWRD+7,X'0F'     FIX SIGN FOR UNPK                     16440000
         UNPK  LOWHH,DBLWRD+6(2)  CHARACTER                             16450000
         MVI   LOWHH,C' '         KILL LEADING ZERO                     16460000
         SPACE                                                          16470000
         ICM   R2,B'0011',6(R4)   HIGH CC                               16480000
         CVD   R2,DBLWRD          DECIMAL                               16490000
         MVC   HICC,EXTMASK       EDIT MASK                             16500000
         ED    HICC,DBLWRD+5      CHARACTER                             16510000
         SPACE                                                          16520000
         ICM   R2,B'0011',8(R4)   HIGH HH                               16530000
         CVD   R2,DBLWRD          DECIMAL                               16540000
         OI    DBLWRD+7,X'0F'     FIX SIGN FOR UNPK                     16550000
         UNPK  HIHH,DBLWRD+6(2)   CHARACTER                             16560000
         MVI   HIHH,C' '          KILL LEADING ZERO                     16570000
         SPACE                                                          16580000
         MVI   LOWCC,C'.'         LOOKS                                 16590000
         MVI   HICC,C'-'          NICE                                  16600000
         SPACE                                                          16610000
         SR    R1,R1              CLEAR FOR                             16620000
         SR    R0,R0              INSERTS                               16630000
         ICM   R1,B'0011',6(R4)   HIGH CC                               16640000
         ICM   R0,B'0011',2(R4)   LOW  CC                               16650000
         SR    R1,R0              TOTAL CYL FOR THIS EXTENT             16660000
         LR    R3,R1              GET SET FOR MULTIPLY                  16670000
         M     R2,TRKCYL          CONVERT CYLINDER EXTENT TO TRACKS     16680000
         SPACE                                                          16690000
         ICM   R1,B'0011',8(R4)   HIGH HH                               16700000
         LA    R1,1(R1)           BUMP BY ONE                           16710000
         ICM   R0,B'0011',4(R4)   LOW  HH                               16720000
         SR    R1,R0              TOTAL TRK FOR THIS EXTENT             16730000
         AR    R3,R1              ADD TO CYLINDER EXTENT SPACE          16740000
         D     R2,TRKCYL          GET CYLINDERS AND TRACKS              16750000
         SPACE                                                          16760000
         LA    R1,EXTSPC          -> OUTPUT POSITION FOR SPACE DESC     16770000
         SPACE                                                          16780000
         LTR   R3,R3              ANY CYLINDERS ?                       16790000
         BZ    TRKONLY2           NOPE                                  16800000
         SPACE                                                          16810000
         LR    R0,R3              NUMBER OF CYLINDERS                   16820000
         BAL   LINK,INSRTNUM      OUTPUT IT                             16830000
         MVC   1(3,R1),=C'CYL'    ADD LABEL                             16840000
         LA    R1,5(R1)           -> NEXT AREA                          16850000
         SPACE                                                          16860000
         LTR   R2,R2              DO WE NEED TO OUTPUT TRKS ?           16870000
         BZ    NXTDESC            NO                                    16880000
         SPACE                                                          16890000
TRKONLY2 LR    R0,R2              NUMBER OF TRACKS                      16900000
         BAL   LINK,INSRTNUM      OUTPUT IT                             16910000
         MVC   1(3,R1),=C'TRK'    ADD LABEL                             16920000
         LA    R1,4(R1)           -> NEXT AREA                          16930000
         SPACE 2                                                        16940000
NXTDESC  LR    OUTPTR,R1          -> END OF LINE                        16950000
         BAL   LINK,NEWLINE       NEXT LINE                             16960000
         LA    R4,10(R4)          -> NEXT EXTENT                        16970000
         BCT   R5,DESCLOOP        GET REST IN THIS GROUP                16980000
         BR    R6                 RETURN                                16990000
         SPACE 3                                                        17000000
DESCDONE EQU   *                  EXIT WHEN ALL EXTENTS WRITTEN OUT     17010000
         TITLE 'DATASET INQUIRY FUNCTION                NORMAL EXIT'    17020000
TPUT     L     R1,@CLINE          -> CURRENT LINE                       17030000
         LA    R1,8(,R1)          -> PAST PREFIX                        17040000
         CR    R1,OUTPTR          ANYTHING ON CURRENT LINE ?            17050000
         BE    EMPTYLIN           NOPE                                  17060000
         SPACE                                                          17070000
         BAL   LINK,NEWLINE       WRITE THE LAST LINE                   17080000
         SPACE                                                          17090000
EMPTYLIN BAL   LINK,ENDLINE       FLUSH OUT BUFFER                      17100000
         SPACE                                                          17110000
RETURN   LA    R1,CORE            -> WORKAREA                           17120000
         LA    R0,CORESIZE        LENGTH                                17130000
         L     R13,4(,R13)        GET OLD SAVE AREA                     17140000
         SVC   10                 FREE WORKAREA                         17150000
         SPACE                                                          17160000
         SR    R15,R15            RETURN CODE ZERO                      17170000
         RETURN (14,12),,RC=(15)  RETURN TO CALLER                      17180000
         TITLE 'DATASET INQUIRY FUNCTION                ERROR EXITS'    17190000
         SPACE                                                          17200000
BADLOC   CH    R15,=H'8'                                                17210000
         BL    LOCRC                                                    17220000
         CH    R15,=H'20'                                               17230000
         BH    LOCRC                                                    17240000
         B     *-4(R15)           DETERMINE RETURN CODE                 17250000
         B     LOC8                                                     17260000
         B     LOC12                                                    17270000
         B     LOC16                                                    17280000
         B     LOC20                                                    17290000
         SPACE 2                                                        17300000
LOC8     MVC   LINE(L'NOQUAL),NOQUAL    "QUALIFICATION DOESN'T EXIST"   17310000
         LA    OUTPTR,L'NOQUAL+2(OUTPTR)                                17320000
         B     INDXLVL                                                  17330000
         SPACE                                                          17340000
LOC12    MVC   LINE(L'NEEDQUAL),NEEDQUAL  "INSUFFICIENT QUALIFICATION"  17350000
         LA    OUTPTR,L'NEEDQUAL+2(OUTPTR)                              17360000
         B     INDXLVL                                                  17370000
         SPACE                                                          17380000
LOC16    MVC   LINE(L'OVERQUAL),OVERQUAL  "TOO MUCH QUALIFICATION"      17390000
         LA    OUTPTR,L'OVERQUAL+2(OUTPTR)                              17400000
         B     INDXLVL                                                  17410000
         SPACE                                                          17420000
INDXLVL  LR    R2,R0                      SAVE R0                       17430000
         BAL   LINK,NEWLINE               NEXT LINE                     17440000
         MVC   LINE(L'INDXMSG),INDXMSG    "INDEX LEVEL "                17450000
         LA    R1,LINE+L'INDXMSG          -> AREA FOR LEVEL NUM         17460000
         LR    R0,R2                      INDEX LEVEL NUMBER            17470000
         BAL   LINK,INSRTNUM                                            17480000
         LA    OUTPTR,2(R1)               -> PAST                       17490000
         B     TPUT                                                     17500000
         SPACE                                                          17510000
LOC20    MVC   LINE(L'BADDSN),BADDSN    "INVALID DSNAME"                17520000
         LA    OUTPTR,L'BADDSN+2(OUTPTR)                                17530000
         B     TPUT                                                     17540000
         SPACE                                                          17550000
LOCRC    MVC   LINE(L'LOCHDR),LOCHDR    "LOCATE RETURN CODE XX"         17560000
         MVC   LINE+L'LOCHDR(L'RCMSG),RCMSG                             17570000
         LR    R0,R15                     RETURN CODE                   17580000
         LA    R1,LINE+L'LOCHDR+L'RCMSG   -> OUTPUT AREA                17590000
         BAL   LINK,INSRTNUM                                            17600000
         LA    OUTPTR,2(R1)               -> PAST                       17610000
         B     TPUT                                                     17620000
         EJECT                                                          17630000
BADOBTN  CH    R15,=H'4'                                                17640000
         BL    OBTNRC                                                   17650000
         CH    R15,=H'12'                                               17660000
         BH    OBTNRC                                                   17670000
         B     *(R15)             DETERMINE RETURN CODE                 17680000
         B     OBTN4                                                    17690000
         B     OBTN8                                                    17700000
         B     OBTN12                                                   17710000
         SPACE 2                                                        17720000
OBTN4    MVC   LINE(L'BADVOL1),BADVOL1  "VOLUME "                       17730000
         MVC   LINE+L'BADVOL1(L'VOLSER),VOLSER                          17740000
         LA    R1,LINE+L'BADVOL1        -> VOLSER STRING                17750000
         LA    R0,L'VOLSER              LENGTH                          17760000
         BAL   R14,CHOPSTR              TRIM TRAILING BLANKS            17770000
         LR    OUTPTR,R1                -> PAST VOLSER                  17780000
         MVC   LINE(L'BADVOL2),BADVOL2  " NOT MOUNTED"                  17790000
         LA    OUTPTR,LINE+L'BADVOL2    -> PAST                         17800000
         B     TPUT                                                     17810000
         SPACE                                                          17820000
OBTN8    MVC   LINE(L'BADDS),BADDS      "DATASET DOES NOT EXIST"        17830000
         LA    OUTPTR,L'BADDS+2(OUTPTR)                                 17840000
         B     TPUT                                                     17850000
         SPACE                                                          17860000
OBTN12   MVC   LINE(L'VTOCERR),VTOCERR  "I/O ERROR ON VTOC"             17870000
         LA    OUTPTR,L'VTOCERR+2(OUTPTR)                               17880000
         B     TPUT                                                     17890000
         SPACE                                                          17900000
OBTNRC   MVC   LINE(L'OBTNHDR),OBTNHDR  "OBTAIN RETURN CODE XX"         17910000
         MVC   LINE+L'OBTNHDR(L'RCMSG),RCMSG                            17920000
         LR    R0,R15                      RETURN CODE                  17930000
         LA    R1,LINE+L'OBTNHDR+L'RCMSG   -> OUTPUT AREA               17940000
         BAL   LINK,INSRTNUM                                            17950000
         LA    OUTPTR,2(R1)                -> PAST                      17960000
         B     TPUT                                                     17970000
         EJECT                                                          17980000
NOFILE   MVC   LINE(L'NOFILE1),NOFILE1  "FILE "                         17990000
         MVC   LINE+L'NOFILE1(L'DDNAME),DDNAME                          18000000
         LA    R1,LINE+L'NOFILE1        -> DDNAME STRING                18010000
         LA    R0,L'DDNAME              LENGTH                          18020000
         BAL   R14,CHOPSTR              TRIM TRAILING BLANKS            18030000
         LR    OUTPTR,R1                -> PAST DDNAME                  18040000
         MVC   LINE(L'NOFILE2),NOFILE2  " NOT FOUND"                    18050000
         LA    OUTPTR,LINE+L'NOFILE2    -> PAST                         18060000
         B     TPUT                                                     18070000
         SPACE 2                                                        18080000
BADFILE  MVC   LINE(L'BADFILE1),BADFILE1 "FILE "                        18090000
         MVC   LINE+L'BADFILE1(L'DDNAME),DDNAME                         18100000
         LA    R1,LINE+L'BADFILE1       -> DDNAME STRING                18110000
         LA    R0,L'DDNAME              LENGTH                          18120000
         BAL   R14,CHOPSTR              TRIM TRAILING BLANKS            18130000
         LR    OUTPTR,R1                -> PAST DDNAME                  18140000
         MVC   LINE(L'BADFILE2),BADFILE2 " IS NOT VALID FOR DD"         18150000
         LA    OUTPTR,LINE+L'BADFILE2   -> PAST                         18160000
         B     TPUT                                                     18170000
         EJECT                                                          18180000
GNRLFAIL DS    0H                 CALL GENERAL FAIL                     18190000
         SPACE                                                          18200000
         XC    GFPARMS(GFLENGF),GFPARMS    INIT GENERAL FAIL PARM LIST  18210000
         SPACE                                                          18220000
         ST    R15,GFRCODE        RETURN CODE IN PARM LIST              18230000
         STH   R1,GFCALLID        FAILING ROUTINE ID                    18240000
         L     R1,4(,R13)         -> PREV SAVE AREA                     18250000
         L     R1,24(,R1)         REG 1 IN PREV SAVE AREA (-> CPPL)     18260000
         ST    R1,GFCPPLP         -> CPPL FOR GENRL FAIL                18270000
         LA    R1,DSINFO+5        -> PROGRAM NAME                       18280000
         ST    R1,GFPGMNP         FOR ERROR MSG                         18290000
         LA    R1,6               LENGTH OF NAME                        18300000
         STH   R1,GFPGMNL         FOR GENERAL FAIL                      18310000
         SPACE                                                          18320000
         LA    R1,ECB             -> DUMMY ECB                          18330000
         ST    R1,GFECBP          FOR GENERAL FAIL TO GIVE TO PUTLINE   18340000
         SR    R1,R1                                                    18350000
         ST    R1,ECB             CLEAR ECB                             18360000
         SPACE                                                          18370000
         LA    R1,GFPARMS         -> GENERAL FAIL PARMS                 18380000
         ST    R1,GFPARMP         ADDR LIST FOR LINK                    18390000
         SPACE                                                          18400000
         LINK  EP=IKJEFF19,MF=(E,GFPARMP)  DIAGNOSE RETURN CODE         18410000
         SPACE                                                          18420000
         LTR   R15,R15            GENERAL FAIL EXECUTE OK ?             18430000
         BZ    RETURN                                                   18440000
         SPACE 2                                                        18450000
         ABEND 100,DUMP           WHY ?                                 18460000
         TITLE 'DATASET INQUIRY FUNCTION                SUBROUTINES'    18470000
         SPACE                                                          18480000
*                                                                       18490000
*  INPUT -  R0 = BINARY NUMBER                                          18500000
*           R1 -> OUTPUT AREA                                           18510000
*  OUTPUT - NUMBER IS OUTPUT LEFT JUSTIFIED WITH COMMA(S) AND LEADING   18520000
*           MINUS SIGN (IF NEGATIVE).  R0 WILL CONTAIN LENGTH OF CHAR   18530000
*           NUMBER AND R1 WILL POINT PAST THE NUMBER UPON RETURN.       18540000
*  NOTE   - A DECIMAL NUMBER MAY BE OUTPUT BY PLACING THE NUMBER IN     18550000
*           THE AREA "DBLWRD" AND USING "INSRTPAK" ENTRY.               18560000
*                                                                       18570000
INSRTNUM CVD   R0,DBLWRD          GET DECIMAL                           18580000
INSRTPAK STM   R2,R3,28(R13)      SAVE WORK REGISTERS                   18590000
         LR    R3,R1              PTR TO OUTPUT AREA                    18600000
         SPACE                                                          18610000
         MVC   EDITWK,EDITPAT     P'ZZZ,ZZZ,ZZZ,ZZZ,ZZ9'                18620000
         LA    R1,EDITWK+L'EDITWK-1   -> SIGNIFICANT DIGIT              18630000
         EDMK  EDITWK,DBLWRD      FORMAT NUMBER                         18640000
         BNM   INSRTNTM           NO NEED TO ADD MINUS SIGN             18650000
         BCTR  R1,0               CHAR BEFORE SIG DIGIT                 18660000
         MVI   0(R1),C'-'         LEADING MINUS SIGN                    18670000
         SPACE                                                          18680000
INSRTNTM LA    R2,EDITWK+L'EDITWK-1   -> LAST DIGIT                     18690000
         SR    R2,R1              LENGTH TO MOVE - 1                    18700000
         EX    R2,INSRTMVC        MOVE NUMBER TO OUTPUT AREA            18710000
         SPACE                                                          18720000
         LA    R0,1(R2)           LENGTH OF NUMBER                      18730000
         LA    R1,1(R2,R3)        -> PAST NUMBER                        18740000
         SPACE                                                          18750000
         LM    R2,R3,28(R13)      RESTORE REGISTERS                     18760000
         BR    LINK               RETURN                                18770000
         SPACE 2                                                        18780000
INSRTMVC MVC   0(*-*,R3),0(R1)    LEFT JUSTIFY OUTPUT NUMBER            18790000
EDITPAT  DC    4X'2020206B',X'202120'  EDIT PATTERN                     18800000
         EJECT                                                          18810000
         SPACE                                                          18820000
*                                                                       18830000
*  INPUT -  OUTPTR -> END OF CURRENT OUTPUT LINE                        18840000
*                                                                       18850000
*  OUTPUT - OUTPTR IS UPDATED TO POINT TO NEXT FREE OUTPUT AREA.        18860000
*           POINTERS ARE SET UP FOR THE PUTLINE SERVICE ROUTINE.        18870000
*           IF IOAREA IS FULL THEN IT IS WRITTEN OUT.                   18880000
*                                                                       18890000
*  ENTRY  - ENDLINE ENTRY POINT FLUSHES IO BUFFER WITHOUT               18900000
*           WRITING A LINE.                                             18910000
*                                                                       18920000
* USES   - @CLINE  ->  START OF CURRENT LINE                            18930000
*          @PLINE  ->  START OF PREVIOUS LINE                           18940000
*                                                                       18950000
         SPACE                                                          18960000
NEWLINE  DS    0H                 ADVANCE TO NEXT OUTPUT LINE           18970000
         SPACE                                                          18980000
         STM   R2,R3,SAVEREG      SAVE REGISTERS                        18990000
         SPACE                                                          19000000
         LA    OUTPTR,0(,OUTPTR)  CLEAR HIGH BYTE                       19010000
         LR    R3,OUTPTR          -> START OF NEXT LINE                 19020000
         L     R2,@CLINE          -> START OF CURRENT LINE              19030000
         ST    R3,0(,R2)          CHAIN TO NEXT LINE                    19040000
         SPACE                                                          19050000
         SR    R3,R2              LENGTH OF CURRENT LINE                19060000
         SH    R3,=H'4'           MINUS LENGTH OF CHAIN WORD            19070000
         STH   R3,4(,R2)          STORE LENGTH FOR PUTLINE              19080000
         SR    R3,R3              CLEAR                                 19090000
         STH   R3,6(,R2)          ZERO OFFSET VALUE                     19100000
         SPACE                                                          19110000
         ST    OUTPTR,@CLINE      SAVE PTR TO CURRENT LINE              19120000
         ST    R2,@PLINE          SAVE PTR TO PREVIOUS LINE             19130000
         LA    OUTPTR,8(,OUTPTR)  -> PAST PREFIX                        19140000
         SPACE                                                          19150000
         LA    R2,IOEND           -> END OF IOAREA                      19160000
         CR    OUTPTR,R2          END OF AREA ?                         19170000
         BL    NLRTN              NOPE                                  19180000
         B     BUFFLUSH           TIME TO WRITE I/O BUFFER              19190000
         SPACE                                                          19200000
ENDLINE  STM   R2,R3,SAVEREG      ENTRY POINT TO FLUSH BUFFER           19210000
         SPACE                                                          19220000
BUFFLUSH TM   FLAGS,CLRSCN        CLEAR SCREEN ???                      19230000
         BNO  NOCLR               NOPE                                  19240000
         SPACE                                                          19250000
         TPUT  CLR,L'CLR,FULLSCR,,HOLD CLEAR SCREEN         * JLM *     19260000
         STLINENO LINE=1                                                19270000
         NI    FLAGS,X'FF'-CLRSCN     RESET FLAG                        19280000
         SPACE                                                          19290000
NOCLR    LA    R1,IOAREA+8        -> START OF I/O AREA DATA             19300000
         CR    R1,OUTPTR          ANYTHING TO WRITE ?                   19310000
         BE    NLRTN              NOPE - SKIP IT                        19320000
         SPACE                                                          19330000
         L     R1,@PLINE          -> LAST LINE                          19340000
         SR    R0,R0              CLEAR REGISTER                        19350000
         ST    R0,0(,R1)          ZERO LAST CHAIN WORD TO TERMINATE     19360000
         SPACE                                                          19370000
         PUTLINE OUTPUT=(IOAREA,MULTLIN,DATA),MF=(E,IOPLSECT)           19380000
         SPACE                                                          19390000
         LTR   R15,R15            PUTLINE OK ?                          19400000
         BZ    SETBUF2            YES - DONE                            19410000
         LA    R1,GFPUTL          PUTLINE ERROR                         19420000
         B     GNRLFAIL           GET GENERAL FAIL TO DIAGNOSE ERROR    19430000
         SPACE                                                          19440000
SETBUFR  STM   R2,R3,SAVEREG      SETUP I/O BUFFER                      19450000
SETBUF2  SR    R3,R3              ZERO SOURCE LENGTH                    19460000
         ICM   R3,B'1000',=C' '   BLANK PAD CHAR                        19470000
         LA    R1,LIOAREA         LENGTH OF IOAREA                      19480000
         LA    R0,IOAREA          -> IO BUFFER                          19490000
         MVCL  R0,R2              INITIALIZE IT TO BLANKS               19500000
         SPACE                                                          19510000
         LA    R1,IOAREA          -> OUTPUT AREA                        19520000
         ST    R1,@CLINE          SAVE ADDR FOR PUTLINE                 19530000
         LA    OUTPTR,8(,R1)      -> FIRST FREE DATA BYTE               19540000
         SPACE                                                          19550000
NLRTN    LM    R2,R3,SAVEREG      RESTORE REGISTERS                     19560000
         BR    LINK               RETURN                                19570000
         EJECT                                                          19580000
         SPACE                                                          19590000
*                                                                       19600000
* INPUT  -  R0 ->  OUTPUT AREA TO PLACE DATE IN FORM MM/DD/YY           19610000
*           R1 ->  JULIAN DATE IN FORM YDD  (BINARY)                    19620000
* OUTPUT -  DATE IS CONVERTED TO GREGORIAN FORMAT.                      19630000
*                                                                       19640000
CNVTDATE STM   R2,R5,28(R13)      SAVE WORK REGISTERS                   19650000
         LR    R5,R0              -> OUTPUT AREA                        19660000
         LR    R4,R1              -> YDD  (BINARY)                      19670000
         LH    R2,1(R4)           NUM DAYS SINCE JAN 1                  19680000
         TM    0(R4),X'03'        YEAR DIVISIBLE BY 4 ?                 19690000
         BNZ   LEAPBYP            NOPE                                  19700000
         CH    R2,=H'60'          DOES LEAP YEAR AFFECT CUR DATE ?      19710000
         BL    LEAPBYP            NO - BEFORE FEB 29                   C19720000
                                  YES - PAST FEB 29 NEEDS ADJUST        19730000
         BCTR  R2,0               FIX FOR EXTRA DAY                     19740000
         SPACE                                                          19750000
LEAPBYP  LA    R1,DAYSTBLE        -> JULIAN DATE TABLE                  19760000
         LA    R3,12              NUMBER OF MONTHS                      19770000
         SPACE                                                          19780000
DAYSLOOP CH    R2,0(R1)           LESS THAN THIS MONTH ??               19790000
         BH    GOTMON             NO - WE HAVE A MONTH                  19800000
         SH    R1,=H'2'           BACK UP A MONTH                       19810000
         BCT   R3,DAYSLOOP                                              19820000
         SPACE                                                          19830000
         MVC   0(8,R5),=CL8'??/??/??'   SHOULD NEVER FALL THRU          19840000
         B     CVTDATRT                 RETURN                          19850000
         SPACE                                                          19860000
GOTMON   SH    R2,0(R1)           CURRENT DAY OF MONTH                  19870000
         SR    R1,R1              CLEAR FOR INSERT                      19880000
         IC    R1,0(,R4)          PICK UP YEAR                          19890000
         SPACE                                                          19900000
*  R1 = YEAR   R2 = DAY   R3 = MONTH                                    19910000
         SPACE                                                          19920000
         CVD   R1,DBLWRD          YEAR IN DECIMAL                       19930000
         ZAP   DATEWK,DBLWRD      PUT IN YEAR                           19940000
         CVD   R2,DBLWRD          DAY IN DECIMAL                        19950000
         NI    DBLWRD+7,X'F0'     ZERO SIGN NIBBLE                      19960000
         OC    DATEWK+1(2),DBLWRD+6    PUT IN DAY                       19970000
         CVD   R3,DBLWRD          MONTH IN DECIMAL                      19980000
         NI    DBLWRD+7,X'F0'     ZERO SIGN NIBBLE                      19990000
         OC    DATEWK(2),DBLWRD+6 PUT IN MONTH                          20000000
         MVC   EDITWK(10),DATEPAT MM/DD/YY                              20010000
         ED    EDITWK(10),DATEWK  FORMAT DATE                           20020000
         MVC   0(8,R5),EDITWK+2   MOVE TO OUTPUT AREA                   20030000
         SPACE                                                          20040000
CVTDATRT LM    R2,R5,28(R13)      RESTORE REGISTERS                     20050000
         BR    LINK               RETURN                                20060000
         SPACE 2                                                        20070000
DATEPAT  DC    X'40202120612020612020'  DATE PATTERN  MM/DD/YY          20080000
         SPACE                                                          20090000
         EJECT                                                          20100000
*  FUNCTION -                                                           20110000
*        THIS SUBROUTINE TRANSLATES THE UCBTYP FIELD INTO A             20120000
*        CHARACTER EBCDIC DEVICE NAME.  UPON ENTRY REGISTER ZERO        20130000
*        SHOULD CONTAIN THE UCBTYP FIELD. THE UNIT TYPE IS PLACED       20140000
*        AT THE ADDRESS POINTED TO BY REGISTER 1.                       20150000
*  INPUT -                                                              20160000
*        R0 =  UCBTYP FIELD                                             20170000
*        R1 -> OUTPUT AREA TO PLACE DEVICE NAME                         20180000
*  OUTPUT -                                                             20190000
*        R1 -> PAST END OF EBCDIC DEVICE NAME.                          20200000
*                                                                       20210000
*  NOTES -                                                              20220000
*        THE TRANSLATION IS MADE BY LOOKING UP THE UCBTYP IN THE        20230000
*        SYSTEM DEVNAMET TABLE, WHICH IS A LOAD MODULE KEPT IN THE      20240000
*        LINK PACK AREA.  THE ADDRESS OF THE DEVNAMET MODULE IS SAVED   20250000
*        TO AVOID A LINK-PACK SEARCH NEXT TIME THIS MODULE IS CALLED.   20260000
*                                                                       20270000
*        IN XA SYSTEMS, THE DEVICE NAME TABLE NO LONGER        10/16/90 20280000
*        EXISTS.  WHEN DEVNAMET CANNOT BE FOUND, TRANSLATION   10/16/90 20290000
*        IS PERFORMED VIA A CALL TO IEFEB4UV (MVS UNIT         10/16/90 20300000
*        VERIFICATION SERVICE).                                10/16/90 20310000
*                                                                       20320000
         SPACE 2                                                        20330000
UCBTRAN  STM   R14,R12,12(R13)    SAVE REGISTERS                        20340000
         LR    R2,R0              GET PARM                              20350000
         N     R2,MASKTYPE        MASK OUT IOS-USED BITS IN UCBTYPE     20360000
         LR    R4,R1              SAVE OUTPUT AREA POINTER              20370000
         SPACE                                                          20380000
         ICM   R5,B'1111',@DEVNAME DEVNAMET PREVIOUSLY FOUND?  10/16/90 20390000
         BP    HAVNAMET           YES - SKIP LPA SEARCH        10/16/90 20400000
         BM    UCBTRANX           PRIOR SEARCH FAILED, BRANCH  10/16/90 20410000
         SPACE                                                          20420000
         L     R3,CVTPTR          GET ADDRESS OF CVT                    20430000
         L     R15,CVTLPDSR-CVT(,R3) GET ADDR OF LPDE SEARCH ROUTINE    20440000
         LM    R0,R1,DEVNAMET     GET 'DEVNAMET' IN R0 & R1             20450000
         BALR  R14,R15            CALL IEAVVMSR - DESTROYS R6, R8 & R9  20460000
         B     GOTNAMET           +0 - DEVNAMET FOUND                   20470000
         MVI   3(R4),C'1'         +4 - DEVNAMET NOT FOUND               20480000
         MVI   @DEVNAME,X'80'     REMEMBER DEVNAMET NOT THERE  10/16/90 20490000
         B     UCBTRANX           ERROR EXIT                   10/16/90 20500000
GOTNAMET DS    0H                 DEVNAMET LPDE FOUND                   20510000
         LR    R1,R0              GET ADDRESS OF DEVNAMET LPDE          20520000
         L     R5,LPDENTP-LPDE(,R1) GET EP ADDRESS OF DEVNAMET          20530000
         LA    R5,0(,R5)          CLEAR BIT0 JUST IN CASE      10/16/90 20540000
         ST    R5,@DEVNAME        SAVE FOR FUTURE USE                   20550000
HAVNAMET L     R15,0(,R5)         GET NUMBER OF ENTRIES                 20560000
         LA    R5,4(,R5)          GET ADDRESS OF 1ST ENTRY              20570000
         SPACE                                                          20580000
UCBTDEV  C     R2,8(,R5)          UCBTYP MATCH ?                        20590000
         BE    UCBTGOT            YES - UCBTYP FOUND                    20600000
         LA    R5,12(,R5)         NO - POINT TO NEXT ENTRY              20610000
         BCT   R15,UCBTDEV        LOOP UNTIL END                        20620000
         SPACE                                                          20630000
         MVI   3(R4),C'2'         INDICATE ERROR TYPE                   20640000
         B     NONAMET            UCBTYP NOT FOUND                      20650000
         SPACE                                                          20660000
UCBTGOT  DS    0H                 UCBTYP FOUND IN DEVNAMET              20670000
         LA    R0,8               LENGTH OF DEVICE NAME                 20680000
         LR    R1,R5              -> DEVICE NAME                        20690000
         BAL   R14,CHOPSTR        CHOP OFF TRAILING BLANKS              20700000
         BNP   BLNKDEVC           ERROR - NAME IS ALL BLANKS            20710000
         SPACE                                                          20720000
         LR    R1,R0              COPY LENGTH                           20730000
         BCTR  R1,0               ADJUST LENGTH FOR EX                  20740000
         EX    R1,MOVEDEVN        MOVE DEVICE NAME TO OUTPUT AREA       20750000
         LA    R4,1(R1,R4)        -> PAST                               20760000
         SPACE                                                          20770000
UCBTRETN LR    R1,R4              PASS POINTER BACK TO CALLER           20780000
         LM    R2,R12,28(R13)     RESTORE REGISTERS                     20790000
         BR    LINK               RETURN                                20800000
         SPACE ,                                               10/16/90 20810000
*                                                              10/16/90 20820000
*  DEVNAMET WAS NOT FOUND, WE MUST BE RUNNING ON AN XA         10/16/90 20830000
*  SYSTEM.  USE IEFEB4UV TO PROCESS WHATEVER PASSES FOR        10/16/90 20840000
*  THE DEVICE NAME TABLE THESE DAYS.                           10/16/90 20850000
*                                                              10/16/90 20860000
UCBTRANX XC    UNITABLE,UNITABLE  CLEAR UNIT TABLE             10/16/90 20870000
         ST    R2,UNITABLE+8      STORE UCBTYP FIELD           10/16/90 20880000
         LA    R5,UNITABLE        POINT TO UNIT TABLE          10/16/90 20890000
         ST    R5,IEFEBPRM        (WILL SOON BE UNIT NAME)     10/16/90 20900000
         LA    R1,=AL1(BIT2+BIT7,0)  POINT TO FLAGS            10/16/90 20910000
         ST    R1,IEFEBPRM+4           "                       10/16/90 20920000
         OI    IEFEBPRM+4,X'80'   SET VL BIT                   10/16/90 20930000
         LA    R1,IEFEBPRM        POINT TO PARAMETER LIST      10/16/90 20940000
         ST    R13,IEFEBSAV+4     PROVIDE A SAVE AREA          10/16/90 20950000
         LA    R13,IEFEBSAV            "                       10/16/90 20960000
         LINK  EP=IEFEB4UV        MUSH                         10/16/90 20970000
         L     R13,4(,R13)        RESTORE OLD SAVE AREA        10/16/90 20980000
         LTR   R15,R15            EVERYTHING OKAY?             10/16/90 20990000
         BZ    UCBTGOT            YES, GO MOVE AND TRUNCATE    10/16/90 21000000
         MVI   3(R4),C'4'         INDICATE TRANSLATION ERROR   10/16/90 21010000
         B     NONAMET            AND EXIT                     10/16/90 21020000
         SPACE ,                                                        21030000
BLNKDEVC MVI   3(R4),C'3'         ERROR TYPE - DEVICE ALL BLANKS        21040000
         SPACE                                                          21050000
NONAMET  MVC   0(3,R4),=C'ERR'    INDICATE UCBTRAN TRANSLATION ERROR    21060000
         MVC   4(3,R4),=C'(X'''                                         21070000
         UNPK  7(9,R4),20(5,R13)  CONVERT DEVTYPE TO HEX                21080000
         NC    7(8,R4),HEXMASK    PUT WITHIN RANGE OF TR                21090000
         TR    7(8,R4),HEXTABLE   CONVERT TO HEX PRINTABLE              21100000
         MVC   15(3,R4),=C''') '                                        21110000
         LA    R4,17(R4)          -> PAST                               21120000
         B     UCBTRETN           RETURN                                21130000
         SPACE                                                          21140000
MOVEDEVN MVC   0(*-*,R4),0(R5)    MOVE DEVICE NAME TO OUTPUT AREA       21150000
         EJECT                                                          21160000
         SPACE                                                          21170000
*                                                                       21180000
*  INPUT -  R1 -> STRING                                                21190000
*           R0 = LENGTH OF STRING                                       21200000
*  OUTPUT - TRAILING BLANKS ARE CHOPPED OFF END OF STRING.              21210000
*           R1 -> FIRST BLANK AFTER STRING                              21220000
*           R0 = NEW LENGTH OF STRING                                   21230000
*           CONDITION CODE IS SET ACCORDING TO VALUE OF R0.             21240000
*                                                                       21250000
*  NOTES:  THIS SUBROUTINE USES R14 INSTEAD OF LINK TO ALLOW            21260000
*          IT TO BE CALLED FROM OTHER SUBROUTINES.                      21270000
*                                                                       21280000
CHOPSTR  LR    R15,R0             LENGTH OF STRING                      21290000
         LA    R1,0(R15,R1)       -> PAST STRING                        21300000
         BCTR  R1,0               -> LAST CHAR IN STRING                21310000
         SPACE                                                          21320000
CHOPLOOP CLI   0(R1),C' '         BLANK ?                               21330000
         BH    CHOPPED            NO - HIT SIGNIFICANT CHARACTER        21340000
         BCTR  R1,0               YES - CHOP IT                         21350000
         BCT   R15,CHOPLOOP       LOOP TILL END OF STRING               21360000
         SPACE                                                          21370000
CHOPPED  LTR   R0,R15             NEW LENGTH OF STRING   (ALSO SET CC)  21380000
         LA    R1,1(,R1)          -> FIRST BLANK AFTER STRING           21390000
         BR    R14                RETURN                                21400000
         TITLE 'DATASET INQUIRY FUNCTION                CONSTANTS'      21410000
MOVEPARM MVC   0(*-*,R3),0(R1)    MOVE PARAMETERS FROM PDL              21420000
         SPACE                                                          21430000
NAMECAM  CAMLST NAME,0,,0         FOR LOCATE MACRO                      21440000
         ORG   NAMECAM+4          BACK UP OVER ADCONS                   21450000
SRCHCAM  CAMLST SEARCH,0,0,0      FOR OBTAIN MACRO                      21460000
         ORG   SRCHCAM+4          BACKUP OVER ADCONS                    21470000
SEEKCAM  CAMLST SEEK,0,0,0        FOR OBTAIN MACRO                      21480000
         ORG   SEEKCAM+4          BACK UP OVER ADCONS                   21490000
         SPACE                                                          21500000
         DC    Y(0,31,59,90,120,151,181,212,243,273,304,334) JAN-DEC    21510000
DAYSTBLE EQU   *-2                -> DECEMBER                           21520000
         SPACE                                                          21530000
PCLADDR  DC    A(PARMTAB)         -> PCL                                21540000
         SPACE                                                          21550000
         DS    0F                                                       21560000
MASKTYPE DC    AL1(255,255-(UCBRR+UCBRVDEV+UCBVLPWR+UCBDVPWR),255,255) *21570000
                                  MASK OUT BITS USED BY IOS IN UCBTYP   21580000
DEVNAMET DC    CL8'DEVNAMET'      MODULE NAME FOR LPDE SEARCH ROUTINE   21590000
         SPACE                                                          21600000
GLWKSIZE DC    A(X'7FFC')         SIZE OF WORKAREA PASSED TO GENLOC     21610000
         SPACE                                                          21620000
GLWKMAXR DC    Y((X'7FFC'-4)/45*45) LEN OF MAX POSSIBLE USED WORKAREA   21630000
         SPACE                                                          21640000
GLWKHDR  DC    X'7FFC0004'        HEADER OF GENLOC WORKAREA             21650000
         SPACE                                                          21660000
GENLFLAG DC    X'05'              CTGNAME+CTGGENLD = DSNAME GEN LOC     21670000
         DC    X'20'              CTGRCATN = RETURN CATALOG NAME        21680000
         DC    X'11'              CTGSUPLT+CTGAM0 = SUPER LOC AND       21690000
         DC    X'00'                                NOT A CAMLST        21700000
         EJECT                                                          21710000
         PRINT NOGEN                                                    21720000
PARMTAB  IKJPARM                                                        21730000
DSNIN    IKJPOSIT DSNAME,USID,PROMPT='DSNAME'                           21740000
VOLGENKY IKJKEYWD                                                       21750000
         IKJNAME 'VOLUME',SUBFLD=VOLSUBF,ALIAS='VOLSER'                 21760000
         IKJNAME 'GENERATION',SUBFLD=GENSUBF                            21770000
PARTLKY  IKJKEYWD                                                       21780000
         IKJNAME 'PARTIAL'                                              21790000
VOLSUBF  IKJSUBF                                                        21800000
VOLIN    IKJIDENT 'VOLUME',MAXLNTH=6,PROMPT='VOLUME',                  *21810000
               FIRST=ALPHA,OTHER=ALPHANUM                               21820000
GENSUBF  IKJSUBF                                                        21830000
GENIN    IKJIDENT 'GENERATION NUMBER',MAXLNTH=5,                       *21840000
               FIRST=ANY,OTHER=NUMERIC,                                *21850000
               VALIDCK=GENCHK,                                         *21860000
               DEFAULT='0'                                              21870000
         IKJENDP                                                        21880000
         SPACE                                                          21890000
         PRINT GEN                                                      21900000
         SPACE 2                                                        21910000
GENCHK   DS    0H                                                       21920000
         SAVE  (14,5),,GENCHK-&SYSDATE-&SYSTIME                         21930000
         SPACE                                                          21940000
         LR    R5,R15                                                   21950000
         USING GENCHK,R5                                                21960000
         SPACE                                                          21970000
         L     R2,0(,R1)          -> PDE FOR GENERATION NUMBER          21980000
         L     R3,0(,R2)          -> GENERATION NUMBER                  21990000
         SPACE                                                          22000000
         CLI   0(R3),C'0'         NUMERIC ?                             22010000
         BL    TESTSIGN           NO, CHECK FOR MINUS                   22020000
         CLI   0(R3),C'9'         NUMERIC ?                             22030000
         BH    BADGEN             NO, INVALID GENERATION NUMBER         22040000
         LH    R0,4(,R2)          LENGTH OF PARAMETER                   22050000
         CH    R0,=H'4'           OVER 4 DIGITS ?                       22060000
         BH    BADGEN             YES - REJECT                          22070000
         B     OKGEN              YES, OK                               22080000
         SPACE                                                          22090000
TESTSIGN CLI   0(R3),C'-'         MINUS SIGN ?                          22100000
         BNE   BADGEN             NO, INVALID GENERATION NUMBER         22110000
         SPACE                                                          22120000
OKGEN    SR    R15,R15                                                  22130000
         B     GENRTN                                                   22140000
BADGEN   LA    R15,4              TELL PARSE TO ISSUE ERROR MESSAGE     22150000
         SPACE                                                          22160000
GENRTN   RETURN (14,5),,RC=(15)                                         22170000
         SPACE                                                          22180000
         DROP  R5                                                       22190000
         SPACE                                                          22200000
DSORGTBL DC    C'ISPSDACX....POU GSTXTQ..AMTR....'  DSORG TABLE         22210000
RECFMTBL DC    C'VFUTBSAM'                          RECFM TABLE         22220000
BSAMOPT  DC    B'11111111',C'WUCHQZTJ'              OPTCD TABLES        22230000
BDAMOPT  DC    B'11111001',C'WTEFA..R'                                  22240000
ISAMOPT  DC    B'11111011',C'WUMIY.LR'                                  22250000
HEXTABLE DC    C'0123456789ABCDEF'                  HEX CONVERSION      22260000
HEXMASK  DC    8X'0F'                               HEX CONVERSION      22270000
         SPACE                                                          22280000
EXTMASK  DC    X'402020202120'    EDIT MASK FOR EXTENT DESCRIPTIONS     22290000
XTENTHDR DC    C'EXTENTS  #   CCC HH     CCC HH'                        22300000
         SPACE                                                          22310000
TYPTABLE DC    C'ACDIPUXBGV'      TABLE OF TYPES RETURNED BY GENLOC     22320000
TYPCNT   EQU   *-TYPTABLE         NUM OF ENTRIES                        22330000
         SPACE                                                          22340000
TYPDESC  DC    CL15'NON-VSAM ------'                                    22350000
         DC    CL15'CLUSTER -------'                                    22360000
         DC    CL15'DATA ----------'                                    22370000
         DC    CL15'INDEX ---------'                                    22380000
         DC    CL15'PAGE SPACE ----'                                    22390000
         DC    CL15'USER CATALOG --'                                    22400000
         DC    CL15'ALIAS ---------'                                    22410000
         DC    CL15'GDG ENTRY------'                                    22420000
         DC    CL15'ALT INDEX -----'                                    22430000
         DC    CL15'VOLUME --------'                                    22440000
         SPACE                                                          22450000
BADDSN   DC    C'*** INVALID DSNAME ***'                                22460000
FILLMSG  DC    C'32K RETURN AREA FILLED - ONLY 728 DATASETS LISTED'     22470000
NOQUAL   DC    C'QUALIFICATION DOES NOT EXIST'                          22480000
NEEDQUAL DC    C'INSUFFICIENT QUALIFICATION'                            22490000
OVERQUAL DC    C'TOO MUCH QUALIFICATION'                                22500000
INDXMSG  DC    C'INDEX LEVEL '                                          22510000
BADVOL1  DC    C'VOLUME '                                               22520000
BADVOL2  DC    C' NOT MOUNTED'                                          22530000
BADDS    DC    C'DATASET NOT ON VOLUME'                                 22540000
VTOCERR  DC    C'*** I/O ERROR WHILE READING VTOC, NOTIFY SYSTEMS ***'  22550000
LOCHDR   DC    C'LOCATE'                                                22560000
OBTNHDR  DC    C'OBTAIN'                                                22570000
RCMSG    DC    C' RETURN CODE '                                         22580000
NOFILE1  DC    C'FILE '                                                 22590000
NOFILE2  DC    C' NOT FOUND'                                            22600000
BADFILE1 EQU   NOFILE1,L'NOFILE1                                        22610000
BADFILE2 DC    C' IS INVALID FOR DD'                                    22620000
CLR      DC    X'401140403C40400013' CLEAR SCREEN           * JLM *     22630000
         SPACE 2                                                        22640000
         LTORG                                                          22650000
         SPACE 2                                                        22660000
         TITLE 'DATASET INQUIRY FUNCTION                EQUATES AND DSE*22670000
               CTS'                                                     22680000
         SPACE 3                                                        22690000
R0       EQU   0                  WORK REGISTER                         22700000
R1       EQU   1                  WORK REGISTER                         22710000
R2       EQU   2                  WORK REGISTER                         22720000
R3       EQU   3                  WORK REGISTER                         22730000
R4       EQU   4                  WORK REGISTER                         22740000
R5       EQU   5                  WORK REGISTER                         22750000
R6       EQU   6                  WORK REGISTER                         22760000
R7       EQU   7   ) DUAL         WORK REGISTER                         22770000
@DVCT    EQU   7   ) DEFINITION   BASE REGISTER FOR DVCT DSECT          22780000
R8       EQU   8                  WORK REGISTER                         22790000
R9       EQU   9   ) DUAL         WORK REGISTER                         22800000
LINK     EQU   9   ) DEFINITION   LINKAGE REGISTER FOR SUBROUTINES      22810000
OUTPTR   EQU   10                 POINTER TO POSITION IN OUTPUT BUFFER  22820000
R11      EQU   11                 FIRST  BASE REGISTER FOR DSINFO CSECT 22830000
R12      EQU   12                 SECOND BASE REGISTER FOR DSINFO CSECT 22840000
R13      EQU   13                 BASE REGISTER FOR CORE DSECT          22850000
R14      EQU   14                 WORK REGISTER                         22860000
R15      EQU   15                 WORK REGISTER                         22870000
         EJECT                                                          22880000
         IKJCPPL                                                        22890000
CPPLLEN  EQU   *-CPPL                                                   22900000
         SPACE 2                                                        22910000
         IKJPPL                                                         22920000
PPLLEN   EQU   *-PPL                                                    22930000
         SPACE 2                                                        22940000
         IKJIOPL                                                        22950000
IOPLLEN  EQU   *-IOPL                                                   22960000
         SPACE 2                                                        22970000
         EJECT                                                          22980000
         SPACE                                                          22990000
         CVT   DSECT=YES,LIST=NO                                        23000000
         SPACE                                                          23010000
         EJECT                                                          23020000
         SPACE                                                          23030000
         IHALPDE                                                        23040000
         SPACE                                                          23050000
         EJECT                                                          23060000
         SPACE                                                          23070000
         IKJTCB DSECT=YES,LIST=NO                                       23080000
         SPACE                                                          23090000
         EJECT                                                          23100000
         SPACE                                                          23110000
TIOT     DSECT                                                          23120000
         IEFTIOT1                                                       23130000
         SPACE                                                          23140000
         EJECT                                                          23150000
         SPACE                                                          23160000
UCB      DSECT                                                          23170000
         IEFUCBOB LIST=NO                                               23180000
         EJECT                                                          23190000
         SPACE 2                                                        23200000
CORE     DSECT                                                          23210000
         DS    18F                                                      23220000
         SPACE                                                          23230000
DBLWRD   DS    D                                                        23240000
SAVEREG  DS    4F                 FOR SUBROUTINES                       23250000
@CLINE   DS    F                  -> CURRENT OUTPUT LINE                23260000
@PLINE   DS    F                  -> PREVIOUS OUTPUT LINE               23270000
DVCTPTR  DS    F                  -> DVCT ENTRY                         23280000
TRKCYL   DS    F                  NUMBER OF TRACKS PER CYLINDER         23290000
@DEVNAME DS    A                  -> DEVNAMET MODULE IN LPA             23300000
@GENWKA  DS    A                  -> ADDRESS OF GENLOC WORK AREA        23310000
DSNLEN   DS    H                  LENGTH OF DSNAME                      23320000
         DS    H                                                        23330000
         SPACE                                                          23340000
IOPB     PUTLINE MF=L                                                   23350000
PPLSECT  DS    0F                 PPL AREA                              23360000
         DS    CL(PPLLEN)                                               23370000
IOPLSECT DS    0F                 IOPL AREA                             23380000
         DS    CL(IOPLLEN)                                              23390000
ECB      DS    F                  ECB FOR PARSE                         23400000
PARSBACK DS    F                  -> PARSE PDL                          23410000
GFPARMP  DS    F                  -> GENERAL FAIL PARM BLOCK            23420000
         IKJEFFGF                 GENERAL FAIL PARM BLOCK               23430000
         EJECT                                                          23440000
CAMLST   DS    F                  FLAG BYTES INDICATING FUNC OF CAMLST  23450000
CAMLSTP2 DS    A                  PARAMETER TWO OF CAMLST               23460000
CAMLSTP3 DS    A                  PARAMETER THREE OF CAMLST             23470000
CAMLSTP4 DS    A                  PARAMETER FOUR OF CAMLST              23480000
         SPACE                                                          23490000
         ORG   CAMLST             REUSE SOME AREA                       23500000
GENLPARM DS    F                  FLAGS INDICATING FUNCTION             23510000
GENDSN   DS    A                  -> GENERIC KEY                        23520000
GENCAT   DS    A                  -> CATALOG DSNAME                     23530000
GENWKA   DS    A                  -> WORK AREA                          23540000
GENX1    DS    XL12               UNUSED PTRS                           23550000
         ORG                                                            23560000
         SPACE                                                          23570000
         DS    0D                                                       23580000
CAMAREA  DS    CL265              CAMLST WORK AREA                      23590000
         ORG   CAMAREA                                                  23600000
         DS    3CL140             RESERVE SPACE FOR 3 DSCB'S            23610000
         ORG   CAMAREA            LABELS FOR LOCATE INFO                23620000
VOLCNT   DS    H                  VOLUME SERIAL COUNT                   23630000
VOLENT   EQU   *                  START OF VOLUME ENTRIES               23640000
         SPACE 2                                                        23650000
         ORG   CAMAREA-44         BACK UP TO ELIMINATE 44 CHAR DSN      23660000
         IECSDSL1 (1,2,3)         GEN FMT 1,2 AND 3 DSCB LABELS         23670000
         SPACE 2                                                        23680000
         ORG   IECSDSL1           START OF F1 DSCB                      23690000
         DS    CL45                                                     23700000
ASM2USEC DS    0XL3               NEW ASM2 (2.4) USE COUNT LOCATION     23710000
ASM2UDAT DS    XL3                LAST USE DATE (YDD)                   23720000
ASM2MDAT DS    XL3                LAST MOD DATE (YDD)                   23730000
         DS    CL11                                                     23740000
ASM2MJOB DS    CL8                JOBNAME/USERID LAST MODIFIED DS       23750000
         DS    CL6                                                      23760000
ASM2UCNT DS    XL4                USE COUNT                             23770000
ASM2MTOD DS    XL2                TIME WHEN DS MODIFIED (HHMM)          23780000
         SPACE 3                                                        23790000
         ORG                                                            23800000
DSNLENC  DS    X                  LENGTH OF DSNAME FOR GENERIC LOCATE   23810000
DSNAME   DS    CL44               DSNAME                                23820000
DDNAME   DS    CL8                DDNAME (IF FILE TYPE CAT)             23830000
VOLSER   DS    CL6                VOLSER OF DATASET                     23840000
EDITWK   DS    CL19               FOR INSRTNUM SUBROUTINE               23850000
DATEWK   DS    CL4                FOR CNVTDATE SUBROUTINE               23860000
DEVTYPE  DS    X                  LAST BYTE OF UCBTYPE FIELD            23870000
FLAGS    DS    X                                                        23880000
NOVOL    EQU   X'80'              NO VOLUME SERIAL SUPPLIED             23890000
GDGDSN   EQU   X'40'              GENERATION NUMBER SUPPLIED            23900000
HAVEF3   EQU   X'20'              FORMAT 3 DSCB HAS BEEN READ           23910000
CLRSCN   EQU   X'10'              CLEAR SCREEN REQUESTED                23920000
SINGDSN  EQU   X'08'              ONE DSNAME FROM GENERIC LOCATE        23930000
PARTIAL  EQU   X'04'              PARTIAL DSNAME SUPPLIED               23940000
FILETYPE EQU   X'02'              FILE TYPE INPUT                       23950000
         SPACE                                                          23960000
IOAREA   DS    25CL80                                                   23970000
IOEND    EQU   *                                                        23980000
LIOAREA  EQU   *-IOAREA           LENGTH OF IOAREA                      23990000
         SPACE                                                          24000000
         DS    CL80               BUFFER JUST IN CASE                   24010000
         ORG   ,                                               10/16/90 24020000
IEFEBPRM DS    2F                 IEFEB4UV PARAMETER LIST      10/16/90 24030000
UNITABLE DS    CL12               IEFEB4UV UNIT TABLE          10/16/90 24040000
IEFEBSAV DS    18F                SAVE AREA FOR IEFEB4UV       10/16/90 24050000
         SPACE ,                                               10/16/90 24060000
IOSVSWRK DC    100X'00'           IOSVSUCB WORKAREA            10/16/90 24070000
IOSVSPRM DC    A(IOSVSWRK)         )                           10/16/90 24080000
         DC    A(IOSVSDEV)         ) MUST BE KEPT TOGETHER     10/16/90 24090000
         DC    X'80',AL3(IOSVSUCB) )                           10/16/90 24100000
IOSVSUCB DS    A                  UCB ADDRESS                  10/16/90 24110000
IOSVSDEV DC    AL1(UCB3DACC)      DASD DEVICE CLASS            10/16/90 24120000
         DS    0D                 ROUND UP TO DOUBLE WORD BOUNDRY       24130000
         SPACE                                                          24140000
CORESIZE EQU   *-CORE                                                   24150000
         SPACE 3                                                        24160000
CATRECD  DSECT                    FORMAT FOR A VOLUME ENTRY ON CATALOG  24170000
DEVCODE  DS    CL4                UCB DEVICE TYPE FIELD                 24180000
CATVOL   DS    CL6                SERIAL OF A VOLUME                    24190000
DSEQNUM  DS    H                  DATASET SEQUENCE NUMBER               24200000
NEXTVOL  EQU   *                  NEXT VOLUME ENTRY                     24210000
         SPACE 3                                                        24220000
OUTRECD  DSECT                    OUTPUT FORMAT                         24230000
LINE     DS    0C                 GEN PURPOSE LABEL                     24240000
         SPACE                                                          24250000
         DS    CL6                START OF EXTENT DESC OUTPUT LABELS    24260000
EXTNUM   DS    CL4                EXTENT NUMBER                         24270000
LOWCC    DS    CL6                LOW CYL ADDR                          24280000
LOWHH    DS    CL3                LOW HEAD ADDR                         24290000
         DS    CL2                                                      24300000
HICC     DS    CL6                HIGH CYL ADDR                         24310000
HIHH     DS    CL3                HIGH HEAD ADDR                        24320000
         DS    CL4                                                      24330000
EXTSPC   DS    0C                 AREA FOR EXTENT SPACE DESC            24340000
         SPACE 2                                                        24350000
         IHADVCT ,                DEVICE CHARACTERISTICS TABLE          24360000
*DVCT     DSECT                    DEVICE CHARACTERISTICS TABLE         24370000
*DVCCYL   DS    H                  NUMBER OF CYLINDERS PER VOLUME       24380000
*DVCTRK   DS    H                  NO OF TRACKS PER CYLINDER            24390000
*DVCTRKLN DS    H                  NO OF BYTES PER TRACK                24400000
*DVCOVHD  DS    0H                 OVERHEAD PER BLOCK                   24410000
*DVCOVNLB DS    X                  OVERHEAD NOT LAST BLOCK              24420000
*DVCOVLB  DS    X                  OVERHEAD LAST BLOCK                  24430000
*DVCOVNK  DS    X                  OVERHEAD DECREMENT NOT KEYED         24440000
*DVCFLAGS DS    X                  FLAG BYTE                            24450000
*DVC2BOV  EQU   X'08'              SINGLE VALUE BLOCK OVERHEAD          24460000
*DVCFTOL  EQU   X'01'              APPLY TOLERANCE FACTOR               24470000
*DVCTOL   DS    H                  TOLERANCE FACTOR                     24480000
*DVCTSHFT EQU   9                  SHIFT AMOUNT FOR TOLERANCE FACTOR    24490000
*DVCALT   DS    H                  NUMBER OF ALT TRACKS PER VOLUME      24500000
         SPACE ,                                               10/16/90 24510000
         IEZBITS ,                                             10/16/90 24520000
         SPACE 3                                                        24530000
         END   DSINFO                                                   24540000
/*                                                                      24550000
//LKED.SYSIN DD *                                                       24560000
  ALIAS DDX                                                             24570000
  NAME  DD(R)                                                           24580000
/*                                                                      24590000
//                                                                      24600000
