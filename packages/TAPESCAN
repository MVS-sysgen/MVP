//TAPESCAN  JOB (TSO),
//             'Install TAPESCAN',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//******************************************************************//  00020000
//*       CBT FILE #102 - TAPESCAN                                 *//  00030000
//******************************************************************//  00040000
//TAPESCAN EXEC ASMFCL,MAC1='SYS1.AMODGEN',                             00050000
//             PARM.LKED='XREF,MAP,NCAL,AC=1'                           00060000
//ASM.SYSIN DD *                                                        00070000
TSCN     TITLE 'TAPESCAN 5.2 - TAPE SCAN AND COPY UTILITY'              00010000
*                                                                       00020000
*---------------------------------------------------------------------- 00030000
*                                                                       00040000
* ABSTRACT - A PROGRAM TO PROVIDE AN OVERVIEW OF THE DATA SETS ON A     00050000
*            TAPE, COPY FILES AND RECOVER DATA PAST THE FIRST END OF    00060000
*            VOLUME INDICATOR.  INFORMATION PRESENTED INCLUDES RECORD   00070000
*            AND BYTE COUNT, LENGTH ESTIMATE, DISPLAY OF THE FIRST 100  00080000
*            BYTES OF THE FIRST FOUR RECORDS OF EACH DATA SET, AND THE  00090000
*            PHYSICAL TAPE FILE NUMBER.                                 00100000
*                                                                       00110000
* WARNINGS - WHEN DATA IS RECOVERED PAST THE END OF VOLUME INDICATOR,   00120000
*            THE FIRST RECORD MAY HAVE BEEN TRUNCATED.  IF ACCEPTED, IT 00130000
*            MAY LEAD TO PROBLEMS EVEN AFTER IT IS COPIED.  WHEN        00140000
*            COPYING DATA SETS FROM A STANDARD LABEL TAPE, THE DATA SET 00150000
*            SEQUENCE NUMBER STORED IN THE HEADER RECORD ISN'T CHANGED. 00160000
*            THIS HAS CAUSED NO PROBLEMS SO FAR, BUT IS NOT SUPPORTED   00170000
*            BY IBM.  SOME OPERATIONS OF THIS PROGRAM ARE BASED ON THE  00180000
*            NUMBER OF TAPE MARKS ENCOUNTERED.                          00190000
*                                                                       00200000
* RESTRICTIONS - FOR USE WITH TAPE DATA SETS ONLY.                      00210000
*                                                                       00220000
* DDNAMES - SYSPRINT - A FILE WHICH CONTAINS THE TAPE OVERVIEW REPORT   00230000
*                      AND ANY I/O ERROR MESSAGES.                      00240000
*                                                                       00250000
*           INPUT    - THE FILE WHICH REFERENCES THE TAPE TO BE SCANNED 00260000
*                      OR COPIED.                                       00270000
*                                                                       00280000
*           OUTPUT   - IF REQUIRED, WILL CONTAIN THE COPY OF THE DATA   00290000
*                      SETS REQUESTED FROM INPUT.                       00300000
*                                                                       00310000
* PARAMETERS - ALL THE PARAMETERS ARE KEYWORDS.  THEIR MEANINGS,DEFAULT 00320000
*              VALUES AND KEY WORDS ARE:                                00330000
*                                                                       00340000
*     COPY      - CREATE A COPY OF THE INPUT DATA SETS, LABELS AND      00350000
*                 TAPE MARKS REQUESTED ON OUTPUT.  THE OUTPUT DD        00360000
*                 STATEMENT MUST BE INCLUDED.  NOCOUNT MAY NOT BE       00370000
*                 SPECIFIED.  BY DEFAULT THE PARAMETER IS ASSUMED       00380000
*                 NOT TO BE IN EFFECT.                                  00390000
*                                                                       00400000
*     EOVMOD    - PLACES THE INPUT DATA TO BE COPIED AFTER THE LAST     00410000
*                 DATA SET ON OUTPUT.  THE OUTPUT DD STATEMENT MUST     00420000
*                 BE INCLUDED.  THIS PARAMETER IMPLIES COPY.  NOCOUNT   00430000
*                 MAY NOT BE SPECIFIED.  BY DEFAULT THE PARAMETER       00440000
*                 IS ASSUMED NOT TO BE IN EFFECT.                       00450000
*                                                                       00460000
*     ERRLIMN   - THE MAXIMUM NUMBER OF I/O ERRORS ALLOWED BEFORE       00470000
*                 PROCESSING IS TERMINATED IS SPECIFIED BY "N".  THE    00480000
*                 DEFAULT IS ERRLIM50.                                  00490000
*                                                                       00500000
*     LISTN     - THE NUMBER OF BLOCKS WHICH HAVE ONE LINE PRODUCED     00510000
*                 SYSPRINT IS SPECIFIED BY "N".  THE DEFAULT IS LIST    00520000
*                                                                       00530000
*     MAXEOVN   - PROCESSING WILL CONTINUE UNTIL "N" END OF VOLUME      00540000
*                 INDICATORS HAVE BEEN FOUND.  PROCESSING WILL STOP     00550000
*                 WHEN EITHER MAXTMN OR MAXEOVN IS EXCEEDED.  THE       00560000
*                 DEFAULT VALUE IS MAXEOV1 AND IF SKIPEOVN IS USED      00570000
*                 THEN MAXEOVN MUST BE ONE GREATER THAN SKIPEOVN.       00580000
*                                                                       00590000
*     MAXTMN    - STOP PROCESSING AFTER "N" TAPE MARKS HAVE BEEN        00600000
*                 ENCOUNTERED.  THIS WILL NOT GO PAST THE END OF VOLUME 00610000
*                 INDICATOR.  THE DEFAULT IS TO PROCESS TO THE END OF   00620000
*                 THE VOLUME.                                           00630000
*                                                                       00640000
*     NOCOUNT   - TO SAVE I/O TIME THE COUNTING FEATURE MAY BE TURNED   00650000
*                 OFF BY THIS PARAMETER.  THE NUMBER OF PHYSICAL BLOCKS 00660000
*                 THE MAXIMUM, AVERAGE AND MINIMUM BLOCK SIZES, THE     00670000
*                 LENGTH AND THE TOTAL NUMBER OF BYTES PROCESSED WILL   00680000
*                 BE REPORTED.  THIS PARAMETER MAY NOT BE USED WITHCOPY 00690000
*                 OR EOVMOD.  BY DEFAULT THE PARAMETER IS ASSUMED       00700000
*                 NOT TO BE IN EFFECT.                                  00710000
*                                                                       00720000
*     NOHEX     - DO NOT PRODUCE THE HEXADECIMAL EQUIVALENT OF THEFIRST 00730000
*                 FEW LINES ON SYSPRINT.  BY DEFAULT THE PARAMETER      00740000
*                 IS ASSUMED TO BE OFF.                                 00750000
*                                                                       00760000
*     NOLIST    - FUNCTIONS THE SAME AS LIST0.  BY DEFAULT THE          00770000
*                 PARAMETER IS ASSUMED NOT TO BE IN EFFECT.             00780000
*                                                                       00790000
*     NOSUMMARY - DO NOT PRODUCE THE SUMMARY OF DATA SETS FOR STANDARD  00800000
*                 LABEL TAPE ON SYSPRINT.  BY DEFAULT THE PARAMETER     00810000
*                 IS ASSUMED NOT TO BE IN EFFECT.                       00820000
*                                                                       00830000
*     NOVOLSER  - DURING A COPY OPERATION BETWEEM TWO STANDARD LABEL    00840000
*                 TAPES, THE VOLUME HEADER LABEL WILL NOT BE COPIED.    00850000
*                 BY DEFAULT THE PARAMETER IS ASSUMED NOT TO BE IN      00860000
*                 EFFECT.                                               00870000
*                                                                       00880000
*     SKIPEOVN  - PROCESSING WILL START AFTER "N" NUMBER OF END OF VOL  00890000
*                 INDICATORS HAVE BEEN ENCOUNTERED.  INSURE THAT MAXEOV 00900000
*                 IS AT LEAST ONE GREATER THAN SKIPEOVN. THE DEFAULT IS 00910000
*                 SKIPEOV0.                                             00920000
*                                                                       00930000
*     SKIPTMN   - START PROCESSING AFTER "N" TAPE MARKS HAVE BEEN       00940000
*                 ENCOUNTERED.  PROCESSING WILL NOT START PAST THE END  00950000
*                 OF VOLUME INDICATOR.  WHEN BOTH SKIPTMN AND SKIPEOVN  00960000
*                 ARE SPECIFIED THE MAXIMUM AMOUNT OF SKIPPING IS DONE. 00970000
*                 THE DEFAULT IS SKIPTM0.                               00980000
*                                                                       00990000
*     VTOC      - FOR A STANDARD LABEL TAPE, PRODUCE ONLY THE VTOC      01000000
*                 REPORT, AND NOT THE SAMPLE DUMP OF EACH FILE.         01010000
*                                                                       01020000
* DEFAULT PARAMETERS - LIST4,SKIPTM0,MAXEOV1,SKIPEOV0,ERRLIM50          01030000
*                                                                       01040000
* RETURN CODES - THE JOB STEP RETURN CODE IS SET IF TAPESCAN ABNORMALLY 01050000
*                TERMINATES.  THIS MAY BE TESTED BY LATER JOB STEPS AND 01060000
*                THEIR EXECUTION BASED ON TAPESCAN'S SUCCESS.  THE      01070000
*                RETURN CODE IS:                                        01080000
*                                                                       01090000
*                8 - TAPESCAN HAS TERMINATED BEFORE PROCESSING HAS BEEN 01100000
*                    COMPLETED.  THIS IS USUALLY CAUSED BY I/O ERRORS O 01110000
*                    INPUT.                                             01120000
*                                                                       01130000
* EXAMPLE -                                                             01140000
*               COPY THE FOURTH AND FIFTH FILES OF A STANDARD LABEL SEV 01150000
*               TRACK 556 BPI EVEN PARITY TAPE NEEDING TRANSLATION AND  01160000
*               CONVERSION, TO THE SECOND AND THIRD FILES OF A TAPE WHI 01170000
*               HAD ONLY ONE FILE.  DO NOT COPY THE VOLUME HEADER RECOR 01180000
*               OR PRODUCE THE HEXADECIMAL OUTPUT ON SYSPRINT.  LIST TH 01190000
*               FIRST 20 RECORDS OF THE INPUT DATA SETS.  STOP PROCESSI 01200000
*               AFTER 5 I/O ERRORS OCCUR.                               01210000
*                                                                       01220000
*               //COPYTAPE EXEC PGM=TAPESCAN,PARM=('COPY,NOVOLSER',     01230000
*               //            'EOVMOD,SKIPTM10,MAXTM16,LIST20,NOHEX')   01240000
*               //SYSPRINT DD  SYSOUT=A                                 01250000
*               //INPUT    DD  UNIT=2400-7,DISP=OLD,DCB=(TRTCH=ET,DEN=1 01260000
*               //             EROPT=ACC),VOL=SER=TRACK7                01270000
*               //OUTPUT   DD  UNIT=2400-4,LABEL=2,VOL=SER=OTPTTP       01280000
*                                                                       01290000
* ASM/LINK JCL - -                                                      01300000
*    //TAPESCAN     EXEC ASMHCL  (NO SPECIAL PARMS NEEDED)              01310000
*    //ASM.SYSIN     DD  DISP=SHR,DSN=<SOURCE>(TAPESCAN)                01320000
*    //LKED.SYSLMOD  DD  DISP=SHR,DSN=<LOADLIB>(TAPESCAN)               01330000
*                                                                       01340000
*--------------------------------------------------------------------   01350000
*                                                                       01360000
* THIS PROGRAM, CALLED TAPESCAN, WAS WRITTEN BY WILL DALAND, SOCIAL     01370000
* SCIENCE STATISTICAL LABORATORY, INSTITUTE FOR RESEARCH IN SOCIAL      01380000
* SCIENCE, UNIVERSITY OF NORTH CAROLINA AT CHAPEL HILL, MARCH 1974.     01390000
* SEE IRSS MEMO SSSL-7-3 FOR FURTHER DOCUMENTATION.                     01400000
*                                                                       01410000
* SINCE PROGRAM FOR TAPESCAN WAS PUBLICALLY FUNDED, PERMISSION IS       01420000
* GRANTED FOR ANYONE TO USE THIS PROGRAM IN WHOLE OR IN PART.  IT       01430000
* IS REQUESTED THAT WHEN DOING SO YOU GIVE CREDIT (PREFERABLY BOTH      01440000
* IN SOURCE AND DOCUMENTATION) TO WILL DALAND, IRSS, UNC@CH.            01450000
*                                                                       01460000
* VERSION 3.1 INCLUDES EXPIRATION DATE, AVERAGE BLOCK SIZES, AND VTOC   01470000
* LISTING AND WAS PRODUCED BY C. WRANDLE BARTH, GODDARD SPACE FLIGHT    01480000
* CENTER, JANUARY 1975.                                                 01490000
*                                                                       01500000
* VERSION 3.2 CORRECTED VARIOUS BUGS.                                   01510000
*                                                                       01520000
* VERSION 3.3 CHANGED OUTPUT TAPE HANDLING TO USE ONLY EXCP.            01530000
*                                                                       01540000
* VERSION 3.4 ADDS 6250 BPI TAPE HANDLING.                              01550000
* PLACEMENT OF PROGRAM SECTIONS: INIT CODE, INIT SUBS, INIT VARS,       01560000
* INIT LITS, COMMON ROUTINES, COMMON VARS, COMMON LITS, MAIN CODE,      01570000
* MAIN SUBS, MAIN VARS, MAIN LITS.                                      01580000
*                                                                       01590000
* VERSION 4.0 MAJOR PROGRAM REORGANIZATION AND MODIFICATION BY          01600000
* E. LONG, OCT 78                                                       01610000
*                                                                       01620000
*   17FEB79, FRANK PAJERSKI, MEMOREX                                    01630000
*      . DEFAULT LINE-COUNT SET TO 58                                   01640000
*      . MAX TAPE BLOCKSIZE REDUCED FROM 32767 TO 32760 BYTES           01650000
*        IN ORDER TO KEEP MVS HAPPY                                     01660000
*                                                                       01670000
*   21AUG81, JOHN J. JACOBY, MEMOREX (VERS. 4.1)                        01680000
*      . INTRODUCE THE ABILITY TO PRINT VTOC REPORT ONLY                01690000
*        BY SPECIFYING PARM='VTOC'                                      01700000
*      . ALL OTHER TAPESCAN FUNCTIONS ARE STILL SUPPORTED IF 'VTOC'     01710000
*        IS SPECIFIED, BUT ONLY THE VTOC, INFORMATIONAL AND ERROR       01720000
*        MESSAGES WILL BE PRINTED.                                      01730000
*      . INCLUDE 'DATE' CSECT SO EVERYTHING IS IN ONE CSECT.            01740000
*                                                                       01750000
*   31AUG81, JOHN J. JACOBY, MEMOREX (VERS. 4.2)                        01760000
*      . PRINT THE DD CARD VOLSER IN THE VTOC REPORT IF NL TAPE,        01770000
*        PRINT THE TAPE VOLSER IF SL, AND, IF THE DD CARD AND TAPE      01780000
*        DON'T MATCH, THEN ISSUE A WARNING MESSAGE.                     01790000
*                                                                       01800000
*   09DEC81, FRANK PAJERSKI, ATARI (VERSION 4.2)                        01810000
*      . MINOR ESTHETICS ....                                           01820000
*                                                                       01830000
*   12OCT87, SCOTT BARTH, SIGNETICS (VERSION 4.3)                       01840000
*      . ADDED OPTION "INOUT" TO OPEN MACRO ON INPUT, THIS WILL         01850000
*        SUPPRESS BLOCK COUNT CHECKING ON D/T 3480'S WHICH WILL         01860000
*        PREVENT S237 RC=C ABEND.                                       01870000
*                                                                       01880000
*   24OCT88, FRANK PAJERSKI, SYNTELLIGENCE (VERSION 4.4)                01890000
*      . MINOR CLEANUP/DELETION OF CIRCA S/360 COMMENTS ... THESE       01900000
*        WERE SO OLD THAT I WAS GETTING TIRED OF EXPLAINING THEM        01910000
*        TO THE YOUNGER FOLKS HERE, AS WELL AS BEING EMBARRASSED.       01920000
*      . THIS STILL RUNS OK UNDER MVS/XA 2.1.7 AND DFP 1.2.  THE        01930000
*        ADDITION OF "INOUT" TO HANDLE THE S237 PROBLEM MEANS MESSAGE   01940000
*        IEC510D IF NO TAPE RING IS PRESENT, BUT I CAN LIVE WITH THAT.  01950000
*                                                                       01960000
*                                                                       01970000
* THIS PROGRAM WAS MODIFIED FOR MVS, JUNE 1978 BY:                      01980000
*  STEVE R. HAGGERTY                                                    01990000
*  GTE DATA SERVICES                                                    02000000
*  MARINA DEL REY, CA 90291                                             02010000
*  (213) 821-0511 EXT. 285                                              02020000
*                                                                       02030000
*  INSTALLED AT UNIONBANC COMPUTER CORPORATION ON 08/25/78              02040000
*  BY HOWARD DEAN (TECHNICAL SERVICES). IF ANY PROBLEMS WITH            02050000
*  THIS PROGRAM ARE ENCOUNTERED, PLEASE CONTACT:                        02060000
*                                                                       02070000
*   HOWARD M. DEAN                                                      02080000
*   UNION BANK COMPUTER CORPORATION                                     02090000
*   TECHNICAL SERVICES (8TH FLOOR)                                      02100000
*   605 W. OLYMPIC BLVD.                                                02110000
*   LOS ANGELES, CA. 90015                                              02120000
*   PHONE - (213) 687-5719                                              02130000
*                                                                       02140000
*   WILLIAM J. SMITH                                                    02150000
*   SYNTEX (USA), INC.                                                  02160000
*   3401 HILLVIEW AVENUE                                                02170000
*   MS A4-CIS                                                           02180000
*   PALO ALTO, CA.  94304                                               02190000
*   PHONE - (415) 852-1638                                              02200000
*                                                                       02210000
*   LAST ONE TO WORK ON THIS CODE:                             HD DEC86 02220000
*                                                              HD DEC86 02230000
*   HOWARD M. DEAN                                             HD DEC86 02240000
*   SR. SYSTEMS SPECIALIST                                     HD DEC86 02250000
*   AMERICAN PRESIDENT LINES                                   HD DEC86 02260000
*   #3 WATERS PARK DRIVE                                       HD DEC86 02270000
*   SUITE 115                                                  HD DEC86 02280000
*   SAN MATEO, CA 94403                                        HD DEC86 02290000
*   (415) 570-2331                                             HD DEC86 02300000
*                                                              HD DEC86 02310000
* VERSION 3.2 CORRECTED VARIOUS BUGS.                                   02320000
* VERSION 3.3 CHANGED OUTPUT TAPE HANDLING TO USE ONLY EXCP.            02330000
* VERSION 3.4 CORRECTED FOR USE UNDER MVS REL. 3.7F      *GTEDS LA*SRH* 02340000
* VERSION 3.5 CORRECT BUGS AND ADD LINECNT PARAMETER     *GTEDS LA*HMD* 02350000
* VERSION 4.0 REMOVE 7-TRK CODE AND SUPPORT FOR 3480 CARTS     HD DEC86 02360000
* VERSION 5.0 COMPLETE SUPPORT FOR 3480 CARTRIDGE TAPES        HD JAN89 02370000
         EJECT                                                          02380000
***** H I S T O R Y    L O G **********                                 02390000
*  08/29/78 - HOWARD M. DEAN                                            02400000
*  A.  INSTALLED AT UNIONBANC COMPUTER                                  02410000
*  B.  FINISH SUPPORT FOR 556 BPI TAPE (REMOVED BY S. HAGGERTY)         02420000
*      SEE COMMENT AROUND LABEL TRY556. IF DENSITY IS NOT               02430000
*      FOUND, THIS IS THE DEFAULT                                       02440000
*                                                                       02450000
*  09/15/78 - HOWARD M. DEAN                                            02460000
*   A.  DENSITY PRINTOUT WRONG ON FIRST PAGE OF LISTING. CHANGED        02470000
*       COPY ROUTINE TO ISSUE SENSE CHANNEL COMMAND BEFORE PRINTING     02480000
*       FIRST HEADING SO DENSITY WILL BE CORRECT.                       02490000
*                                                                       02500000
*   B.  ADD ROUTINE TO CHECK FOR MAGNETIC TAPE DEVICES ON BOTH          02510000
*       INPUT AND OUTPUT, FOR IDIOTS THAT LIKE TO PLAY AROUND           02520000
*       WITH OTHER PEOPLES OVERSIGHTS.                                  02530000
*                                                                       02540000
*   C.  ADD ROUTINE TO CHECK FOR PRINTABLE OUTPUT VOLSER. IF THE        02550000
*       VOLSER FROM THE JFCB IS PRINTABLE, IT IS PRINTED. IF NOT,       02560000
*       THE UCB IS CHECKED FOR A VOLSER. IF THAT IS PRINTABLE IT        02570000
*       IS PRINTED. IF NOT, THE NAME OUTVOL IS USED. THE DCB VOLSER     02580000
*       OR JFCB VOLSER, WHICHEVER IS VALID IS COMPARED TO THE OUTPUT    02590000
*       TAPE LABEL TO DETERMINE IF THE LABEL IS VALID.                  02600000
*       ** H. DEAN  09/78 **                                            02610000
*                                                                       02620000
*  10/30/78 - HOWARD DEAN                                               02630000
*   A.  ADD ROUTINE TO PRINT ERROR MESSAGE IF EXPDT OR                  02640000
*       CREDT IN HDR1 OR EOF1 LABEL IS INVALID.                         02650000
*                                                                       02660000
*   B.  INDICATE STD TRTCH ON VTOC INSTEAD OF BLANKS.                   02670000
*                                                                       02680000
*   C.  FIX UNPK INSTRUCTION SO TOTAL COUNT OF BLOCKS READ              02690000
*       WOULD NOT BE TRUNCATED. ADDED 3 DIGITS TO LABEL BYTES.          02700000
*                                                                       02710000
*  11/22/78 - HOWARD DEAN                                               02720000
*   A. MOVE DENSITY SENSE ROUTINE TO AVOID ERROR IN SENSE               02730000
*      PROCESSING FOR 6250 BPI TAPES.                                   02740000
*                                                                       02750000
*   B. SET SENSEBYTS INITIALLY TO 3XL8'00'                              02760000
*                                                                       02770000
*  01/23/79 - HOWARD DEAN                                               02780000
*   A. FIX DENSITY PRINT ON FIRST PAGE OF COPY LISTING BY ISSUING       02790000
*      READ COMMAND TO SET MODE BEFORE OUTPUT PROCESSING                02800000
*  04/09/79 - HOWARD DEAN                                               02810000
*   A. MAKE LINE COUNT A SYMBOLIC PARAMETER                             02820000
*   B. MAKE "LINE" AN EXEC PARAMETER                                    02830000
*                                                                       02840000
*  01/09/84 - WILLIAM SMITH, SYNTEX (USA), INC., PALO ALTO, CA. 94304   02850000
*   A. CHANGED NAME TO "SYNTEX" FOR OBVIOUS REASONS                     02860000
*   B. EXECUTED UNDER MVS/SP 1.3.3 PUT 8308 WITH UCC/1 4.7 AND          02870000
*      CGA'S "TOP SECRET" SECURITY SYSTEM                               02880000
*   C. CREDIT IS HEREBY GIVEN TO A PERSONAL FRIEND, C. WRANDLE BARTH,   02890000
*      FORMERLY OF NASA GODDARD SPACE FLIGHT CENTER, FOR THE WORK HE    02900000
*      DID TO ENHANCE TAPESCAN IN THE MID 1970'S UNDER OS/MVT;  ALL     02910000
*      CHANGES IMPLEMENTED BY RANDY ARE FLAGGED -CWB-                   02920000
*                                                                       02930000
*  08/23/86 - HOWARD M. DEAN, SYNTEX (USA), INC. PALO ALTO, CA 94304    02940000
*   A. FIX PAGE EJECT ROUTINE IN 'VTOC' LISTING TO CORRECTLY            02950000
*      SPACE WHEN A MULTIPLE PAGE VTOC IS PRINTED.                      02960000
*                                                                       02970000
*  12/24/86 - HOWARD M. DEAN, AMER PRES LINES, SAN MATEO, CA 94403      02980000
*   A. REMOVE ALL VESTIGES OF SEVEN TRACK TAPE SUPPORT                  02990000
*   B. ADD MINIMAL SUPPORT FOR 3480 TAPE CARDTRIDGES.                   03000000
*   C. USE SYSTEM MACROS WHERE POSSIBLE INSTEAD OF HARDCODED VALUES     03010000
*                                                                       03020000
*  11/04/88 - HOWARD M. DEAN, AMER PRES LINES, SAN MATEO, CA 94403      03030000
*   A. ADD SUPPORT FOR 3480 TAPE CARTRIDGES - MAKE IT WORK              03040000
*                                                                       03050000
*  01/09/89 - HOWARD M. DEAN, AMER PRES LINES, SAN MATEO, CA 94403      03060000
*   A. FIX 3480 SUPPORT.                                                03070000
*   B. ADD OPTIONS LIST AT BEGINNING OF INVOCATION             HD JAN89 03080000
*                                                                       03090000
*  04/28/89 - HOWARD M. DEAN, AMER PRES LINES, SAN MATEO, CA 94403      03100000
*   A. FIX BUG IN OPTIONS LIST WITH "NOVOLSER" OPTION                   03110000
*                                                                       03120000
**** NOTE: THIS PROGRAM MUST BE LINKED AUTHORIZED FOR MVS               03130000
****       OPERATION. THIS IS NOT A REQUIREMENT IN ANOTHER              03140000
****       OPERATING SYSTEM. ** HMD 05/79 **                            03150000
*                                                                       03160000
*  FPAJ/18JAN99 --- MERGED TOGETHER TWO VERSIONS OF TAPESCAN            03170000
*                   AND CALLED IT 5.1.  ASM/LINK'ED UNDER MVS 5.2.2.    03180000
*  FPAJ/29OCT99 --- ASM/LINK'ED UNDER OS390R5                           03190000
*               --- CHG SYSPRINT BLKSIZE FROM 3458 TO 133               03200000
*                                                                       03210000
*  SAM GOLOB    --- ADDED 64K BLOCK SUPPORT FROM MY VERSION 4.6.        03220000
*    06/21/00       CALLED THIS VERSION 5.2.                            03230000
*                                                                       03240000
*********************************************************************** 03250000
         EJECT                                                          03260000
*********************************************************************** 03270000
*                                                                     * 03280000
*                     OPTIONS IN EFFECT MACRO                         * 03290000
*                                                                     * 03300000
*********************************************************************** 03310000
         MACRO                                                          03320000
         OPTN  &C,&CONST,&VALUE                                         03330000
         LCLA  &N,&P,&P1                                                03340000
         LCLC  &X,&Y,&Z                                        HD JAN89 03350000
         LCLC  &SANSNO                                         HD JAN89 03360000
         LCLC  &W                                                 *JLM* 03370000
&N       SETA  K'&CONST+1                                               03380000
&P       SETA  &N-2                                            HD JAN89 03390000
&W       SETC  'W'.'&SYSNDX'                                   HD JAN89 03400000
&X       SETC  'X'.'&SYSNDX'                                   HD JAN89 03410000
&Y       SETC  'Y'.'&SYSNDX'                                   HD JAN89 03420000
&Z       SETC  'Z'.'&SYSNDX'                                   HD JAN89 03430000
         AIF   (K'&C EQ 0).SKIP                                         03440000
.* ASSUME &CONST BEGINS WITH 'NO' IF &C IS NOT EQUAL TO NULLS  HD JAN89 03450000
&SANSNO  SETC  '&CONST'(3,&P)                                  HD JAN89 03460000
         B&C   &X                       NEGATIVE BRANCH        HD JAN89 03470000
         B     &Y                       POSITIVE BRANCH        HD JAN89 03480000
         AGO   .NONUM                                          HD JAN89 03490000
.SKIP    ANOP                                                  HD JAN89 03500000
         AIF   (K'&VALUE EQ 0).ERR01                                    03510000
         MVC   0(&N,R2),=C',&CONST'                                     03520000
         L     R1,&VALUE                                                03530000
         L     R15,=A(DBLWORK)          GET WORK AREA ADDRESS  HD JAN89 03540000
         CVD   R1,0(R15)                CONVERT IN WORK AREA   HD JAN89 03550000
         OI    7(R15),X'0F'             FIX SIGN                        03560000
         UNPK  &N.(3,R2),6(2,R15)       UNPACK                          03570000
&N       SETA  &N+3                     INCREMENT PAST NUMBER  HD JAN89 03580000
         LA    R2,&N.(R2)               INCREMENT POINTER      HD JAN89 03590000
&W       DS    0H                                              HD JAN89 03600000
         AGO   .MEND                                           HD JAN89 03610000
.NONUM   ANOP                                                  HD JAN89 03620000
&X       DS    0H                                                       03630000
         MVC   0(&N,R2),=C',&CONST'     MOVE NEGATIVE PARM     HD JAN89 03640000
         LA    R2,&N.(R2)               INCREMENT POINTER      HD JAN89 03650000
         B     &Z                       EXIT                   HD JAN89 03660000
&Y       DS    0H                                              HD JAN89 03670000
         MVC   0(&P,R2),=C',&SANSNO'    MOVE POSITIVE PARM     HD JAN89 03680000
         LA    R2,&P.(R2)               INCREMENT POINTER      HD JAN89 03690000
         B     &Z                       EXIT                   HD JAN89 03700000
&Z       DS    0H                                              HD JAN89 03710000
         AGO   .MEND                                           HD JAN89 03720000
.ERR01   MNOTE 12,'VALUE MUST BE SPEIFIED FOR NUMERIC VARIABLE'         03730000
         MEXIT                                                 HD JAN89 03740000
.MEND    ANOP                                                  HD JAN89 03750000
         MEND                                                           03760000
         EJECT                                                          03770000
*********************************************************************** 03780000
*                                                                     * 03790000
*                     INITIALIZATION ROUTINES                         * 03800000
*                                                                     * 03810000
*********************************************************************** 03820000
         LCLA  &LINECNT                                    *HMD 04/79*  03830000
&LINECNT SETA  57                                          *HMD 04/79*  03840000
MSECT    DSECT                                                          03850000
TRT1     DS    32D                                                      03860000
BLANKBUF DS    CL136              BLANKS FOR BLANKING MSGBUF FAST       03870000
TRT2     DS    32D                                                      03880000
MSGBUF   DS    CL136                                                    03890000
RECBUF   DS    CL136              MINIMUM ALLOC, MAX ALLOC = 64K        03900000
HOLDBUF  DS    CL136                                           HD AUG86 03910000
         EJECT                                                          03920000
TAPESCAN CSECT                                                          03930000
R0       EQU   0   WORK REG + SYSTEM USES                               03940000
R1       EQU   1   WORK REG + SYSTEM USES                               03950000
R2       EQU   2   MOSTLY FOR LINKAGE TO PUTLINE & GETNUM               03960000
R3       EQU   3   USED IN PARM SCANNER + VTOC ENTRY POINTER.     -CWB- 03970000
R4       EQU   4   BELOW LABEL 'PROCESS' COUNTS BLKS READ FOLLOWING A  +03980000
                   TAPEMARK OR THE LOAD POINT.                          03990000
R5       EQU   5   WORK, INTERNAL SUBROUTINE LINKAGE                    04000000
R6       EQU   6   WORK REG                                             04010000
R7       EQU   7   WORK REG + LENGTH OF LAST BLK READ                   04020000
R8       EQU   8   PARM FIELD LENGTH CTR, TOTAL BYTES ON TAPE CTR       04030000
R9       EQU   9   BASE REG FOR DSECT MSECT                             04040000
R10      EQU   10  FREE UNUSED REG                                      04050000
R11      EQU   11  2ND BASE REG                                         04060000
R12      EQU   12  1ST BASE REG                                         04070000
R13      EQU   13                                                       04080000
R14      EQU   14  WORK REG + SYSTEM USES                               04090000
R15      EQU   15  WORK REG + SYSTEM USES                               04100000
IOBECBPT EQU   4                                                        04110000
IOBSTART EQU   16                                                       04120000
DEBUCBAD EQU   32                                                       04130000
         EJECT                                                          04140000
TAPESCAN CSECT                                              -HMD-       04150000
         USING *,R12              BASE FOR INITIALIZATION ONLY          04160000
         STM   14,12,12(13)                                             04170000
         LR    R12,R15            R12 = TEMP BASE FOR INITIALIZATION    04180000
         L     R11,=A(EXIT)       PERMANENT BASE FOR MAIN AND COMMON    04190000
         USING EXIT,R11                                                 04200000
         LA    R9,SAVE                                                  04210000
         ST    R9,8(R13)                                                04220000
         ST    13,SAVE+4                                                04230000
         LR    R13,R9                                                   04240000
         L     R1,0(R1)           GET PTR TO PARM FIELD                 04250000
         LH    R8,0(R1)           LOAD PARM FIELD LENGTH                04260000
         LA    R3,2(R1)           SET PTR TO PARM FIELD CHAR STRING     04270000
         LA    R9,SRCHPRM         LOAD TEMPORARY BASE FOR MSECT DSECT   04280000
         USING MSECT,R9           PERMANENT DECLARATION FOR MSECT BASE  04290000
         OPEN  (SYSPRINT,OUTPUT)                                        04300000
         TM    SYSPRINT+(DCBOFLGS-IHADCB),DCBOFOPN                      04310000
         BZ    EXITRC8                                                  04320000
         GETMAIN VU,LA=GMCTRL,A=GMLOCS                                  04330000
         L     R9,GMLOCS          THERE IS AT LEAST THE MINIMUM CORE    04340000
*              ABOVE STMT SETS UP PERMANENT BASE FOR MSECT DSECT        04350000
*        MVI   BLANKBUF,C' '           (POSTPONE TIL AFTER CLEAR) -CWB- 04360000
*        MVC   BLANKBUF+1(135),BLANKBUF                           -CWB- 04370000
         LR    R6,R9              START CLEARING GOTTEN MAIN TO        +04380000
                                  SHORTEN POSSIBLE DUMPS.               04390000
         L     R4,GMLOCS+4        ACTUAL LENGTH OF GOTTEN MAIN          04400000
         LA    R5,256             OFT-USED CONSTANT FOR CLEARING MAIN   04410000
CLOOP    CR    R4,R5              R5 CONTAINS F'256'                    04420000
         BNH   LE256              BIF ONLY 256 OR FEWER BYTES LEFT      04430000
         XC    0(256,R6),0(R6)    CLEAR 256 BYTES                       04440000
         SR    R4,R5              R5 CONTAINS F'256'                    04450000
         AR    R6,R5              R5 CONTAINS F'256'                    04460000
         B     CLOOP                                                    04470000
LE256    BCTR  R4,0               SET TO MACHINE LENGTH                 04480000
         EX    R4,XCLEAR          CLEAR LAST 1 TO 256 BYTES             04490000
XCLEAR   XC    0(0,R6),0(R6)      EXECUTED IN STMT ABOVE                04500000
         MVI   BLANKBUF,C' '           CLEAR BLANK AREA.          -CWB- 04510000
         MVC   BLANKBUF+1(135),BLANKBUF                           -CWB- 04520000
         LA    R1,MSGBUF+46       FOR ADDRESS CONSTANT IN DYNAMIC -CWB- 04530000
         ST    R1,AMSGBP46        STORAGE.                        -CWB- 04540000
         LA    R1,MSGBUF+72       SECOND ADCON.                   -CWB- 04550000
         ST    R1,AMSGBP72                                        -CWB- 04560000
         LA    R15,RECBUF         POINT THE WRITE                       04570000
         O     R15,WRTCMND        CCW TO DYNAMICALLY                    04580000
         ST    R15,WRTCMND        ALLOCATED RECORD BUFFER.              04590000
         LA    R15,RECBUF         SET FILE SEARCH CCW TO                04600000
         O     R15,READ81CM       POINT TO                              04610000
         ST    R15,READ81CM                                             04620000
         LA    R15,RECBUF         INSERT POINTER IN               -CWB- 04630000
         O     R15,READCMND            READ COMMAND.              -CWB- 04640000
         ST    R15,READCMND                                       -CWB- 04650000
         LR    R5,R9              PTR TO TRT1                           04660000
         LA    R6,16                                                    04670000
         LA    R7,CTABLE                                                04680000
SETUPTR1 MVC   0(1,5),0(7)        MOVE SEED CHAR INTO TRT1              04690000
         MVC   1(15,5),0(5)       AND PROPAGATE IT                      04700000
         LA    5,16(5)                                                  04710000
         LA    7,1(7)                                                   04720000
         BCT   6,SETUPTR1                                               04730000
         MVC   TRT2(16),CTABLE    MOVE 16 CHAR SEED INTO TRT2           04740000
         MVC   TRT2+16(240),TRT2  AND PROPAGATE IT 15 TIMES             04750000
         TIME  DEC                GET DATE IN R1 IN FORM 00YYJJJF       04760000
         ST    R0,BADLNGTH        SAVE TIME WHILE YOU'RE AT IT          04770000
         LR    R2,R1              PUT IN R2 TO PASS TO DATE SUBROUTINE  04780000
         LA    R1,=A(DATERSLT)    PTR TO PTR TO DATE'S RESULT FIELD     04790000
         L     R15,=V(DATE)       IRSS DATE SUBROUTINE (DATECONV)       04800000
         BALR  R14,R15            CONVERT 00YYJJJF TO MM/DD/YY          04810000
         ED    TIMERSLT,BADLNGTH  EDIT INTO PAGE HEADER LINE BUFFER     04820000
*        SR    R0,R0              SET TO AVOID RESERVING ANY LINES      04830000
*        BAL   R2,PAGECHK         PRINT PAGE HEADER                     04840000
         DEVTYPE INPUT+40,DTYPE   CHECK TO SEE IF TAPE DEVICE     -HMD- 04850000
         LTR   R15,R15            DID WE FIND DD STATEMENT?       -HMD- 04860000
         BNZ   NOINPUT            NOPE                            -HMD- 04870000
         CLI   DTYPE+2,X'80'      IS THIS A TAPE DEVICE?          -HMD- 04880000
         BNE   NOTTAPEI           NOPE                            -HMD- 04890000
         RDJFCB (INPUT)           READ INPUT TAPE'S JFCB                04900000
         LTR   R15,R15            SEE IF DD STATEMENT THERE             04910000
         BNE   NOINPUT            EXIT WITH ERR MSG IF NOT              04920000
         MVC   DDVOL,JFCBINX+(JFCBVOLS-JFCB) PUT VOL INTO PAGE HEAD.    04930000
         MVC   JFCLTSV,JFCBINX+(JFCBLTYP-JFCB) SAVE INPUT LABEL TYPE    04940000
         TM    JFCLTSV,X'01'      SEE IF NL OR LTM SPECIFIED            04950000
         BO    *+8                IF IT WAS THEN LEAVE IT AS IT IS      04960000
         MVI   JFCBINX+(JFCBLTYP-JFCB),JFCBLP ELSE SET TO BLP           04970000
         MVC   JFCBINX+JFCBFLSQ-JFCB(2),=H'1' SET FILE SEQ. NO. TO = 1  04980000
         OPEN  (INPUT,INOUT),TYPE=J                            HD OCT88 04990000
         TM    INPUT+(DCBOFLGS-IHADCB),DCBOFOPN                         05000000
         BZ    NOINPUT            TERMINAL ERROR                        05010000
         L     R1,INPUT+(DCBEODAD-IHADCB) GET CORRECT EODAD       -HMD- 05020000
         ST    R1,EODADDR         SAVE FOR EXCP ROUTINE           -HMD- 05030000
         L     R1,GMCTRL+4        LOAD MAXIMUM REQUESTED GETMAIN LENGTH 05040000
         S     R1,GMLOCS+4        SUBTRACT LENGTH ACTUALLY GOTTEN       05050000
         BZ    SENSLDPT           BIF GOT ALL CORE REQUESTED            05060000
* ---- CUT OUT THE SHENANIGANS AND DON'T RUN CRIPPLED ------------ *    05070000
         CVD   R1,BADLNGTH        CONVERT DIFERENCE TO PACKED DECIMAL   05080000
         UNPK  MORECORE+33(5),BADLNGTH                                  05090000
         OI    MORECORE+37,C'0'                                         05100000
*        LH    R2,INPUT+(DCBBLKSI-IHADCB) TO PREVENT DATA 'OVERRUNS'    05110000
*        SR    R2,R1              SUBTRACT DIFFERENCE OF MORE CORE      05120000
*        STH   R2,INPUT+(DCBBLKSI-IHADCB) AND PUT BACK IN DCB           05130000
* ---------------------------------------------------------------- *    05140000
         BAL   R2,PUTLINE                                               05150000
         MVC   MSGBUF(L'MORECORE),MORECORE                              05160000
         B     EXITRC8                                                  05170000
SENSLDPT LA    R1,=AL3(SENSCMND)  SET UP PTR FOR EXECEXCP CALL          05180000
         LA    R2,INPUT           PTR TO DCB  FOR EXCP CALL             05190000
         BAL   R4,EXECEXCP        CALL EXCP SUBROUTINE                  05200000
         TM    SENSBYTS+1,X'08'   SEE IF LOAD POINT SENSED              05210000
         BO    SENSTYP            BIF LOAD POINT SENSED           -HMD- 05220000
         LA    R1,=AL3(RWNDCMND)  REWIND AND RE-SENSE COMMAND CHAIN     05230000
         LA    R2,INPUT           PTR TO DCB  FOR EXCP CALL             05240000
         BAL   R4,EXECEXCP                                              05250000
         TM    SENSBYTS+1,X'08'   SEE IF NOW AT LOAD POINT              05260000
         BO    SENSTYP            BIF AT LOAD POINT               -HMD- 05270000
LDPTERR  BAL   R2,PUTLINE         PRINT ERROR MESSAGE             -HMD- 05280000
         MVC   MSGBUF(66),=C'0UNABLE TO REWIND INPUT TAPE TO LOAD POINT+05290000
                - TERMINATING EXECUTION'                                05300000
         B     EXITRC8                                                  05310000
SENSTYP  DS    0H                 SENSE DENSITY OF TAPE           -HMD- 05320000
         LA    R1,HDEOD           FAKE EOD ADDRESS                -HMD- 05330000
         ST    R1,EODADDR         SAVE EOD ADDRESS FOR EXECEXCP   -HMD- 05340000
         LA    R1,=AL3(READCMND)  READ TO SET UP SENSE BYTES      -HMD- 05350000
         LA    R2,INPUT           POINT TO DCB                    -HMD- 05360000
         BAL   R4,EXECEXCP        GO DO EXCP                      -HMD- 05370000
HDEOD    LA    R1,=AL3(SENSCMND)  REWIND TO LOAD POINT            -HMD- 05380000
         LA    R2,INPUT           POINT TO INPUT DCB              -HMD- 05390000
         XC    SENSBYTS(24),SENSBYTS                              -HMD- 05400000
         BAL   R4,EXECEXCP        ISSUE EXCP                      -HMD- 05410000
RESTREOD L     R1,INPUT+(DCBEODAD-IHADCB) GET EOD ADDRESS         -HMD- 05420000
         ST    R1,EODADDR         SAVE FOR FUTURE USE             -HMD- 05430000
         L     R2,(DCBDEBAD-IHADCB)+INPUT GET "DEB" ADDRESS    HD NOV86 05440000
         L     R2,DEBUCBAD(R2)    GET "UCB" ADDRESS            HD NOV86 05450000
*        TM    UCBTBYT4-UCBOB(R2),UCB3480                      HD NOV86 05460000
         TM    UCBTBYT4-UCBOB(R2),X'80'                           *JLM* 05470000
         BO    HD3480             FLAG HEADER IF 3480 DEVICE   HD NOV86 05480000
         TM    SENSBYTS+3,4       P.E.= 1600 B.P.I?               -HMD- 05490000
         BZ    HD6250             NO, TRY 6250                    -HMD- 05500000
         MVI   TAPEDENS,C'3'      SET DENS FOR 1600 BPI           -HMD- 05510000
         MVC   PRTDENS(4),=CL4'1600'   SET PRINT DENS FOR 1600    -HMD- 05520000
         B     HDEND              HEADING END                     -HMD- 05530000
HD6250   L     R2,(DCBDEBAD-IHADCB)+INPUT GET INPUT DEB ADDR.     -HMD- 05540000
         L     R2,DEBUCBAD(R2)    GET UCB ADDRESS                 -HMD- 05550000
         TM    16(R2),2           UCBTYP = 6250?                  -HMD- 05560000
         BNO   HD800              NOPE, TRY 800 BPI               -HMD- 05570000
         MVI   TAPEDENS,C'4'      SET DEN FOR 6250 BPI            -HMD- 05580000
         MVC   PRTDENS(4),=CL4'6250'  SET PRINT FOR 6250          -HMD- 05590000
         B     HDEND                                              -HMD- 05600000
HD3480   MVI   TAPEDENS,C'5'      SELECT DENSITY=5             HD JAN89 05610000
         MVC   PRTDENS(4),=CL4' 38K'                           HD JAN89 05620000
         L     R1,=A(FLAG3480)    LOAD ADDRESS OF FLAG         HD NOV86 05630000
         MVI   0(R1),C'Y'         INDICATE 3480 DEVICE         HD NOV86 05640000
         B     HDEND              CONTINUE WITH PROGRAM        HD NOV86 05650000
HD800    TM    INPUT+(DCBDEN-IHADCB),B'10000011' 800 BPI?         -HMD- 05660000
         MVI   TAPEDENS,C'2'      SET DEN FOR 800 BPI             -HMD- 05670000
         MVC   PRTDENS(4),=CL4' 800'  SET PRINT FOR 800 BPI       -HMD- 05680000
         B     HDEND              END OF ROUTINE                  -HMD- 05690000
HDEND    DS    0H                 ENOUGH OF ALL THIS NONSENSE     -HMD- 05700000
         LA    R1,=AL3(RWNDCMND)  REWIND TO LOAD POINT            -HMD- 05710000
         LA    R2,INPUT           GET INPUT ADDRESS               -HMD- 05720000
         BAL   R4,EXECEXCP        GO DO IT                        -HMD- 05730000
REWOUND  TM    SENSBYTS+1,X'08'   ARE WE AT LOAD POINT NOW?       -HMD- 05740000
         BZ    LDPTERR            NO, INDICATE LOAD ERROR         -HMD- 05750000
SRCHPRM  LTR   R8,R8              LOAD AND TEST REMAINING PARM LENGTH   05760000
         BNH   ENDPARMS                                                 05770000
         SR    R5,R5              ZERO CURRENT PARM LENGTH COUNTER      05780000
         LR    R4,R3              SAVE PTR TO START OF PARM             05790000
SRCHCOMA CLI   0(R3),C','                                               05800000
         LA    R3,1(R3)           BUMP PTR TO NEXT CHAR                 05810000
         BE    GOTCOMMA                                                 05820000
         LA    5,1(5)             COUNTS LENGTH OF CURRENT PARM         05830000
         BCT   R8,SRCHCOMA                                              05840000
GOTCOMMA CLC   0(6,R4),=C'NOLIST' GET HERE IF COMMA OR END OF PARM LIST 05850000
         BE    NOLIST                                                   05860000
         CLC   0(4,4),=C'LIST'                                          05870000
         BE    LIST                                                     05880000
         CLC   0(5,4),=C'MAXTM'   INITIALLY 32760           FJP/20FEB79 05890000
         BE    MAXTM                                                    05900000
         CLC   0(6,4),=C'SKIPTM'                                        05910000
         BE    SKIPTM                                                   05920000
         CLC   0(5,4),=C'NOHEX'                                         05930000
         BE    NOHEX                                                    05940000
         CLC   0(6,4),=C'MAXEOV'                               HD JUN94 05950000
         BE    MAXEOV                                                   05960000
         CLC   0(7,4),=C'SKIPEOV'                                       05970000
         BE    SKIPEOV                                                  05980000
         CLC   0(9,4),=C'NOSUMMARY'                                     05990000
         BE    NOSUMARY                                                 06000000
         CLC   0(5,4),=C'COUNT'                                         06010000
         BE    COUNT                                                    06020000
         CLC   0(7,4),=C'NOCOUNT'                                       06030000
         BE    NOCOUNT                                                  06040000
         CLC   0(6,4),=C'ERRLIM'                                        06050000
         BE    ERRLIM                                                   06060000
         CLC   0(4,4),=C'COPY'                                          06070000
         BE    COPY               TAPE COPYING OPTION                   06080000
         CLC   0(6,4),=C'EOVMOD'  MOD OPTION IMPLIES COPY & COUNT OPTNS 06090000
         BE    EOVMOD                                                   06100000
         CLC   0(8,4),=C'NOVOLSER'                                      06110000
         BE    NOVOLSER                                                 06120000
         CLC   0(3,4),=C'OPT'                                           06130000
         BE    OPT                MISCELLANEOUS OPTIONS                 06140000
         CLC   0(4,4),=C'LINE'    LINE COUNT OPTION        *HMD 04/79*  06150000
         BE    LINE                                        *HMD 04/79*  06160000
         CLC   0(4,4),=C'VTOC'                                  MRX-JJJ 06170000
         BE    VTOCONLY                                         MRX-JJJ 06180000
UNRECOG  BAL   R2,PUTLINE                                               06190000
         MVC   MSGBUF(33),=C'0ERROR - UNRECOGNIZABLE PARAMETER'         06200000
PRLENERR BAL   2,PUTLINE                                                06210000
         MVC   MSGBUF(32),=C'0WARNING - INVALID PARM IGNORED.'          06220000
SRCHPARM BCTR  R8,0                                                     06230000
         B     SRCHPRM                                                  06240000
GETNUM   DS    0H                 CHAR STRNG INTGR TO BIN INTGR CONV SB 06250000
         SR    6,6                CLEAR ACCUMULATOR                     06260000
         CLI   0(4),C'9'          * R4=PTR TO 1ST CHAR OF NUM         * 06270000
         BH    NUMERR             * R5=ACTUAL LENGTH OF NUM           * 06280000
         CLI   0(4),C'0'          * RESULT RETURNED IN R6             * 06290000
         BL    NUMERR             * R2,R4,R5,R6, AND R7 MODIFIED BY   * 06300000
         MH    R6,=H'10'          * USING GETNUM SUBROUTINE.          * 06310000
         IC    R7,0(4)            PICK UP DECIMAL CHARACTER             06320000
         SLL   7,28               CHOP OFF LEFT 4 BITS                  06330000
         SRL   7,28               AND SHIFT BACK                        06340000
         AR    6,7                ADD DIGIT INTO RESULT                 06350000
         LA    4,1(4)             BUMP PTR TO NEXT CHAR                 06360000
         BCT   5,GETNUM+2         GO TO TOP OF LOOP                     06370000
         BR    2                  RETURN FROM GETNUM SUBROUTINE         06380000
NUMERR   BAL   R2,PUTLINE         ERROR DESCRIPTOR SUBROUTINE           06390000
         MVC   MSGBUF(50),=C'0ERROR - PARAMETER HAS INVALID NUMERICAL C+06400000
               OMPONENT'                                                06410000
         B     PRLENERR                                                 06420000
NOCOUNT  CH    R5,=H'7'                                                 06430000
         BNE   PRLENERR           *** WARNING *** MODIFIED IN COPY/MOD +06440000
                                                  OPTION ROUTINES.      06450000
         MVI   COUNTFLG,C'N'                                            06460000
         B     SRCHPARM                                                 06470000
COUNT    CH    R5,=H'5'           FINAL PROCESSING OF COUNT OPTION      06480000
         BNE   UNRECOG                                                  06490000
         MVI   COUNTFLG,C'Y'                                            06500000
         B     SRCHPARM                                                 06510000
NOVOLSER CH    R5,=H'8'                                                 06520000
         BNE   UNRECOG                                                  06530000
         MVI   SVOUTFLG,C'Y'      SET 'NOVOLSER SPECIFIED' FLAG         06540000
         TM    JFCLTSV,X'31'      SEE IF INPUT HAD LABEL TYPE OF NL,    06550000
         BNZ   SRCHPARM           BLP, OR LTM, AND BIF SO.              06560000
         OI    WRTFLG,X'04'       OR IN 'DO NOT COPY INPUT VOL LABEL'   06570000
         B     SRCHPARM                                                 06580000
COPY     CH    R5,=H'4'                                                 06590000
         BNE   UNRECOG                                                  06600000
         OI    COPYFLG,X'01'      INDICATE COPY OPTION SPECIFIED        06610000
         OI    WRTFLG,X'01'       INDICATE COPY   REQUESTED (THAT BIT)  06620000
         OI    NOCOUNT+5,X'F0'    NOP OUT NOCOUNT & SET FOR WARN MSG    06630000
         B     COUNT+8            COPY OPTION INVOKES COUNT AUTOMATICLY 06640000
EOVMOD   CH    R5,=H'6'           ADD DATASETS ONTO EOV                 06650000
         BNE   UNRECOG                                                  06660000
         OI    COPYFLG,X'03'      BITS = 'COPY OPT + MOD OPT REQUESTED' 06670000
         B     COPY+8             MOD OPTION IMPLIES COPY OPTION        06680000
OPT      CH    R5,=H'3'                                                 06690000
         BNH   PRLENERR                                                 06700000
         LA    R4,4(R4)                                                 06710000
         SH    R5,=H'4'                                                 06720000
         BAL   R2,GETNUM                                                06730000
         ST    R6,OPTNO                                                 06740000
         B     SRCHPARM                                                 06750000
ERRLIM   CH    R5,=H'5'           CHANGE SYNAD ERROR COUNT LIMIT        06760000
         BNH   PRLENERR                                                 06770000
         LA    R4,6(R4)                                                 06780000
         SH    R5,=H'6'                                                 06790000
         BAL   R2,GETNUM                                                06800000
         ST    R6,SYNADNO                                               06810000
         B     SRCHPARM                                                 06820000
NOLIST   CH    R5,=H'6'           FINAL PROCESSING OF NOLIST PARM       06830000
         BNE   UNRECOG                                                  06840000
         SR    R0,R0                                                    06850000
         ST    R0,LISTNO                                                06860000
         B     SRCHPARM                                                 06870000
LIST     CH    R5,=H'4'                                                 06880000
         BNH   PRLENERR                                                 06890000
         LA    R4,4(R4)                                                 06900000
         SH    R5,=H'4'           GET ACTUAL LENGTH OF PRESUMED NUMBER  06910000
         BAL   R2,GETNUM          GET PRESUMED NUMBER INTO BINARY FORM  06920000
         ST    R6,LISTNO          STORE NONNEGATIVE BINARY INTEGER      06930000
         B     SRCHPARM                                                 06940000
NOHEX    CH    R5,=H'5'                                                 06950000
         BNE   UNRECOG                                                  06960000
         MVI   HEXFLG,C'N'                                              06970000
         B     SRCHPARM                                                 06980000
VTOCONLY CH    R5,=H'4'                                         MRX-JJJ 06990000
         BNE   UNRECOG                                          MRX-JJJ 07000000
         MVI   VTOCFLAG,C'Y'      SET VTOC ONLY FLAG           HD OCT88 07010000
         B     SRCHPARM                                         MRX-JJJ 07020000
NOSUMARY CH    R5,=H'9'                                                 07030000
         BNE   UNRECOG                                                  07040000
         MVI   SUMFLG,C'N'        SET SUMMARY FLAG TO 'NOSUMMARY'       07050000
         LA    R0,1               READ MINIMUM OF 1 BLK AFTER A TAPEMRK 07060000
         ST    R0,READNO          SET # OF BLKS TO READ FOR SUMMARY=0   07070000
         B     SRCHPARM                                                 07080000
SKIPTM   CH    R5,=H'6'                                                 07090000
         BNH   PRLENERR                                                 07100000
         LA    R4,6(R4)                                                 07110000
         SH    R5,=H'6'                                                 07120000
         BAL   R2,GETNUM                                                07130000
         ST    R6,SKIPTMNO                                              07140000
         B     SRCHPARM                                                 07150000
LINE     CH    R5,=H'4'            IS THIS PARM 'LINE'?    *HMD 04/79*  07160000
         BNH   PRLENERR            NOPE, LENGTH ERROR      *HMD 04/79*  07170000
         LA    R4,4(R4)            BUMP POINTER            *HMD 04/79*  07180000
         SH    R5,=H'4'            DECREMENT COUNTER       *HMD 04/79*  07190000
         BAL   R2,GETNUM           GET NUMERIC VALUE       *HMD 04/79*  07200000
         CH    R6,=H'30'           TOO LOW?                *HMD 04/79*  07210000
         BL    NUMERR              YES, FORGET IT          *HMD 04/79*  07220000
         CH    R6,=H'99'           TOO HIGH?               *HMD 04/79*  07230000
         BH    NUMERR              YES, FORGET IT          *HMD 04/79*  07240000
         ST    R6,LINECNT          SAVE LINE COUNT PARM    *HMD 04/79*  07250000
         B     SRCHPARM            GET SOME MORE PARMS     *HMD 04/79*  07260000
MAXTM    CH    R5,=H'5'                                                 07270000
         BNH   PRLENERR                                                 07280000
         LA    R4,5(4)                                                  07290000
         SH    R5,=H'5'                                                 07300000
         BAL   R2,GETNUM                                                07310000
         LTR   R6,R6              MAKE SURE MAXTM IS NOT =0             07320000
         BZ    NUMERR             ERROR - INVALID NUMERICAL PARM        07330000
         ST    R6,MAXTMNO                                               07340000
         B     SRCHPARM                                                 07350000
SKIPEOV  CH    R5,=H'7'                                                 07360000
         BNH   PRLENERR                                                 07370000
         LA    R4,7(R4)                                                 07380000
         SH    R5,=H'7'                                                 07390000
         BAL   R2,GETNUM                                                07400000
         ST    R6,SKPEOVNO                                              07410000
         B     SRCHPARM                                                 07420000
MAXEOV   CH    R5,=H'6'           CHECK LENGTH OF PARM                  07430000
         BNH   PRLENERR           LENGTH MUST BE GREATER THAN 6         07440000
         LA    R4,6(R4)           BUMP PTR TO START OF PARM'S NUMBER    07450000
         SH    R5,=H'6'           GET ACTUAL LENGTH OF PRESUMED NUMBER  07460000
         BAL   R2,GETNUM          CONVERT NUMBER FOLLOWING 'MAXEOV' PRM 07470000
         LTR   R6,R6              MAKE SURE MAXEOV IS NOT=0             07480000
         BZ    NUMERR             INVALID NUMERICAL COMPONENT           07490000
         ST    R6,MAXEOVNO        STORE RESULT                          07500000
         B     SRCHPARM                                                 07510000
ENDPARMS DS    0H                                                       07520000
         CLC   SKPEOVNO,MAXEOVNO  PARM VALIDITY CHECKING                07530000
         BL    *+26               SKIP IF NO ERROR                      07540000
         BAL   R2,PUTLINE                                               07550000
         MVC   MSGBUF(48),=C'0SKIPEOV PARM GE MAXEOV; SKIPEOV SET TO MA+07560000
               XEOV-1'                                                  07570000
         L     R2,MAXEOVNO                                              07580000
         SH    R2,=H'1'                                                 07590000
         ST    R2,SKPEOVNO        STORE IT SET TO MAXEOV-1              07600000
         CLC   SKIPTMNO,MAXTMNO                                         07610000
         BL    *+26               SKIP IF SKIPTMNO & MAXTMNO CONSISTENT 07620000
         BAL   R2,PUTLINE         PRINT ERROR MESSAGE                   07630000
         MVC   MSGBUF(44),=C'0SKIPTM PARM GE MAXTM; SKIPTM SET TO MAXTM+07640000
               -1'                                                      07650000
         L     R2,MAXTMNO         MAXIMUM ON TAPEMARKS TO BE READ       07660000
         SH    R2,=H'1'                                                 07670000
         ST    R2,SKIPTMNO        STORE AS MAXTM-1                      07680000
         CLC   SKPEOVNO,=F'0'     SPECIFIED?                            07690000
         BE    ENDP010            NO, ITS OK                            07700000
         CLC   SKIPTMNO,=F'0'     SPECIFIED?                            07710000
         BE    ENDP010             NO, ITS OK                           07720000
         BAL   R2,PUTLINE         BOTH SPECIFIED, ERROR                 07730000
         MVC   MSGBUF(52),=C'0BOTH SKIPTM AND SKIPEOV SPECIFIED; SKIPEO+07740000
               V IGNORED.'        PRINT ERROR MESSAGE                   07750000
         MVC   SKPEOVNO,=F'0'                                           07760000
         EJECT                                                          07770000
*********************************************************************** 07780000
*                                                                     * 07790000
*                       PRINT OPTIONS IN EFFECT                       * 07800000
*                                                                     * 07810000
*********************************************************************** 07820000
ENDP010  DS    0H                                              HD OCT88 07830000
         MVC   MSGBUF,BLANKBUF    PRINT A BLANK LINE           HD JAN89 07840000
         BAL   R2,PUTLINE3                                     HD JAN89 07850000
         DC    C'OPT111'                                       HD JAN89 07860000
         SPACE 2                                               HD JAN89 07870000
         MVC   MSGBUF(19),=C' OPTIONS IN EFFECT:'                       07880000
         LA    R2,MSGBUF+19       R2=NEXT POSITION TO RECEIVE PARM      07890000
         OPTN  ,LIST,LISTNO                                    HD JAN89 07900000
         CLI   HEXFLG,C'N'                                     HD JAN89 07910000
         OPTN  E,NOHEX                                         HD JAN89 07920000
         CLI   COUNTFLG,C'N'                                   HD JAN89 07930000
         OPTN  E,NOCOUNT                                       HD JAN89 07940000
         CLC   READNO,=F'1'                                    HD JAN89 07950000
         OPTN  E,NOSUMMARY                                     HD JAN89 07960000
         CLI   COPYFLG,0                                       HD JAN89 07970000
         OPTN  E,NOCOPY                                        HD JAN89 07980000
         TM    COPYFLG,X'02'                                   HD JAN89 07990000
         OPTN  NO,NOEOVMOD                                     HD JAN89 08000000
         CLI   SVOUTFLG,C'Y'                                   HD APR89 08010000
         OPTN  E,NOVOLSER                                      HD JAN89 08020000
         MVI   MSGBUF+19,C' '    BLANK OUT FIRST COMMA         HD JAN89 08030000
         BAL   R2,PUTLINE3       PRINT FIRST OPTION SET        HD JAN89 08040000
         NOP   FWY101            POINT TO                      HD JAN89 08050000
         NOPR  0                    BRANCH AROUND              HD JAN89 08060000
         SPACE 2                                               HD JAN89 08070000
         MVC   MSGBUF,BLANKBUF   CLEAR BUFFER FAST             HD JAN89 08080000
         MVC   MSGBUF(19),=C' OPTIONS IN EFFECT:'                       08090000
         LA    R2,MSGBUF+19       R2=NEXT POSITION TO RECEIVE PARM      08100000
         OPTN  ,SKIPEOV,SKPEOVNO                               HD JAN89 08110000
         OPTN  ,SKIPTM,SKIPTMNO                                HD JAN89 08120000
         OPTN  ,MAXEOV,MAXEOVNO                                HD JAN89 08130000
         OPTN  ,MAXTM,MAXTMNO                                  HD JAN89 08140000
         OPTN  ,OPT,OPTNO                                      HD JAN89 08150000
         CLI   VTOCFLAG,C'N'                                   HD JAN89 08160000
         OPTN  E,NOVTOCONLY                                    HD JAN89 08170000
         MVI   MSGBUF+19,C' '     BLANK OUT FIRST COMMA                 08180000
         BAL   R2,PUTLINE3                                              08190000
         DC    C'OPT222'                                       HD JAN89 08200000
         SPACE 2                                               HD JAN89 08210000
FWY101   DS    0H                                              HD JAN89 08220000
         TM    COPYFLG,X'01'      SEE IF COPY AND/OR MOD SPECIFIED      08230000
         BZ    NOTBOTH            BIF COPY OPTION NOT SPECIFIEDHD JAN89 08240000
         MVI   COUNTFLG,C'Y'      TURN COUNT OPTION ON         HD OCT88 08250000
         MVI   SVOUTFLG,C'N'      TURN "NOVOLSER" OFF          HD OCT88 08260000
***************************************************************HD OCT88 08270000
**                                                             HD OCT88 08280000
**   CHECK OUTPUT TAPE, IF ANY FOR VALID DEVICE TYPE, ETC..    HD OCT88 08290000
**                                                             HD OCT88 08300000
***************************************************************HD OCT88 08310000
OUTCHECK DS    0H                                              HD OCT88 08320000
         DEVTYPE OUTPUT+40,DTYPE  CHECK FOR MAG TAPE DEVICE       -HMD- 08330000
         LTR   R15,R15            IS DD STMT THERE?               -HMD- 08340000
         BNZ   NOOUTPUT           NOPE                            -HMD- 08350000
         CLI   DTYPE+2,X'80'      IS THIS A TAPE DEVICE?          -HMD- 08360000
         BNE   NOTTAPEO           NOPE                            -HMD- 08370000
         RDJFCB (OUTPUT)          ELSE PROCESS COPY AND/OR MOD OPTIONS  08380000
         LTR   R15,R15            SEE IF RDJFCB WENT ALLRIGHT           08390000
         BNE   NOOUTPUT           'MISSING OR INVALID DD' ERROR MSG     08400000
         MVC   SAVEVOL,JFCBOUT+(JFCBVOLS-JFCB) SAVE VOLSER        -HMD- 08410000
         NI    VOLSW,255-VOLSWNV          TURN OFF BIT            -HMD- 08420000
         L     R3,=A(EBCDTBL)             GET TABLE ADDR          -HMD- 08430000
         TRT   SAVEVOL(6),0(R3)           CHECK PRINTABLES        -HMD- 08440000
         BZ    @VALID                     BIF OK                  -HMD- 08450000
         OI    VOLSW,VOLSWNV              TURN ON SWITCH          -HMD- 08460000
         B     *+10                       DONT MOVE IN VOLSER     -HMD  08470000
@VALID   MVC   POSMSG+22(6),SAVEVOL       MOVE VOLSER TO HEADING  -HMD- 08480000
         MVC   OUTLTYP,JFCBOUT+(JFCBLTYP-JFCB)                          08490000
         TM    OUTLTYP,X'01'      SEE IF LABEL IS NL OR LTM             08500000
         BO    *+8                BIF YES - IT IS NL OR LTM             08510000
         MVI   JFCBOUT+(JFCBLTYP-JFCB),JFCBLP ELSE SET TO BLP           08520000
         LA    R0,1                                                     08530000
         LH    R2,JOUTFLSQ        SAVE ORIGINAL FILE SEQ. NO.           08540000
         LTR   R2,R2              BUT IF IT'S                           08550000
         BNZ   *+6                EQUAL TO ZERO THEN                    08560000
         LR    R2,R0              SET IT TO = 1.                        08570000
         STH   R0,JOUTFLSQ        TEMPORARILY SET FLSQ IN JFCB TO = 1   08580000
         ST    R2,SVR2            SAVE R2 OVER TRT                -HMD- 08590000
         OPEN  (OUTPUT,INOUT),TYPE=J                           HD OCT88 08600000
         TM    OUTPUT+(DCBOFLGS-IHADCB),DCBOFOPN                        08610000
         BZ    NOOUTPUT                                                 08620000
         TM    VOLSW,VOLSWNV      IS VOLSER PRINTABLE?            -HMD- 08630000
         BNO   @VOLOK             YES, WE TOOK CARE OF IT EARLIER -HMD- 08640000
         L     R2,(DCBDEBAD-IHADCB)+OUTPUT GET DEB ADDR           -HMD- 08650000
         L     R2,DEBUCBAD(R2)    GET UCB ADDR                    -HMD- 08660000
         MVC   SAVEVOL(6),28(R2)  SAVE VOLSER                     -HMD- 08670000
         L     R3,=A(EBCDTBL)     GET TABLE ADDR                  -HMD- 08680000
         TRT   SAVEVOL(6),0(R3)   SEE IF CHARS ARE PRINTABLE      -HMD- 08690000
         BNZ   @VOLOK             NOPE, THEY ARE NOT..LEAVE CONST.-HMD- 08700000
         MVC   POSMSG+22(6),SAVEVOL   MOVE IN MESSAGE             -HMD- 08710000
@VOLOK   DS    0H                 END OF PRINTABLE CHECK          -HMD- 08720000
         L     R2,SVR2            RESTORE R2 FROM SAVE AREA       -HMD- 08730000
         LA    R1,EXCPIOBP        POINT TO IOB BSAM-TYPE PREFIX.  -CWB- 08740000
         IC    R0,OUTPUT+(DCBIOBA-IHADCB) CHANGE DCB POINTER FROM EXCP- 08750000
         ST    R1,OUTPUT+(DCBIOBA-IHADCB) TYPE TO BSAM-TYPE POINTER     08760000
         STC   R0,OUTPUT+(DCBIOBA-IHADCB) FOR "EXECEXCP" CONSISTANCY    08770000
         STH   R2,JOUTFLSQ        RESTORE ORIGINAL FLSQ IN JFCB         08780000
         TM    OUTLTYP,X'42'      SEE IF AL, AUL, SL, OR SUL            08790000
         BZ    COPYOUT            BIF NOT ONE OF ABOVE                  08800000
         LA    R1,=AL3(READCMND)  USE EXCP TO READ PRESUMED VOL   -CWB- 08810000
         LA    R2,OUTPUT               LABEL (CHANGED FROM BSAM   -CWB- 08820000
         BAL   R4,EXECEXCP             READING).                  -CWB- 08830000
*        LH    R7,READLENG        GET LENGTH OF READ COMMAND.     -CWB- 08840000
*  -----    CHANGE HALFWORD ARITHMETIC TO FULLWORD.                     08850000
         XC    FULLWRK1,FULLWRK1  CLEAR FULLWORK WORKAREA               08860000
         MVC   HALFWRK1(2),READLENG   MOVE ENTIRE HALFWORD              08870000
         L     R7,FULLWRK1        LOAD REGISTER PROPERLY      SBG 04/00 08880000
*        LH    R6,RESIDL          GET RESIDUAL LENGTH AFTER READ. -CWB- 08890000
*  -----    CHANGE HALFWORD ARITHMETIC TO FULLWORD.                     08900000
         XC    FULLWRK2,FULLWRK2  CLEAR FULLWORK WORKAREA               08910000
         MVC   HALFWRK2(2),RESIDL     MOVE ENTIRE HALFWORD              08920000
         L     R6,FULLWRK2        LOAD REGISTER PROPERLY                08930000
         SR    R7,R6                                                    08940000
         BNH   BADREC             VERY BAD IF RECORD LENGTH LT 1        08950000
         TM    OUTLTYP,X'02'      SEE IF SL OR SUL, OR AL OR AUL        08960000
         BO    COPYSL             BIF SL OR SUL                         08970000
*COPYAL  CLC   RECBUF(4),=X'B6AFAC51'  SEE IF ANSI 'VOL1'               08980000
         BNE   BADOUTVL           AL OR AUL IN JCL, BUT LABEL NOT ANSI  08990000
         C     R7,=F'80'                                                09000000
         BL    BADOUTVL                                                 09010000
         B     REWINDCK                                                 09020000
COPYSL   C     R7,=F'80'          SEE IF PRESUMED IBM SL IS 80 BYTES    09030000
         BNE   BADOUTVL           BIF PRESUMED IBM LABEL NOT 80 BYTES   09040000
         CLC   RECBUF(4),=C'VOL1'                                       09050000
         BNE   BADOUTVL                                                 09060000
         CLC   SAVEVOL,RECBUF+4             COMPARE VOL SERS     -HMD-  09070000
         BNE   BADOUTVL                                                 09080000
REWINDCK CLC   JOUTFLSQ(2),=H'1'  DO NOT REWIND IF FILE SEQ. NO. GT 1   09090000
         BH    COPYOUT            DO NOT REWIND IF FILE SEQ. NO. GT 1   09100000
         TM    COPYFLG,X'02'      DO NOT REWIND IF EOVMOD SPECIFIED     09110000
         BO    COPYOUT            DO NOT REWIND IF EOVMOD SPECIFIED     09120000
         CLI   SVOUTFLG,C'Y'      DO NOT REWIND IF NOVOLSER SPECIFIED   09130000
         BE    COPYOUT            DO NOT REWIND IF NOVOLSER SPECIFIED   09140000
         LA    R1,=AL3(RWNDCMND)  REWIND COMMAND CHAINED TO SENSE CMND  09150000
         LA    R2,OUTPUT          OUTPUT DCB                            09160000
         BAL   R4,EXECEXCP        REWIND AND SENSE                      09170000
         TM    SENSBYTS+1,X'08'   SEE IF LOAD POINT SENSED              09180000
         BO    COPYOUT            BIF SENSED LOADPOINT                  09190000
         BAL   R2,PUTLINE                                               09200000
         MVC   MSGBUF(67),=C'0UNABLE TO REWIND OUTPUT TAPE TO LOAD POIN+09210000
               T - TERMINATING EXECUTION'                               09220000
         B     EXITRC8                                                  09230000
BADOUTVL BAL   R2,PUTLINE                                               09240000
         MVC   MSGBUF(32),=C'0OUTPUT VOLUME LABEL IS INVALID:'          09250000
         LR    R3,R12             PROVIDE PROPER ADDRESSABILITY.        09260000
         L     R12,=A(EXIT+4096)                                        09270000
         DROP  R12                                                      09280000
         USING TAPESCAN,R3                                              09290000
         BAL   R5,LISTON                                                09300000
         B     EXITRC8                                                  09310000
         LR    R12,R3             RESTORE ADDRESSABILITY.               09320000
         DROP  R3                                                       09330000
         USING TAPESCAN,R12                                             09340000
COPYOUT  DS    0H                 IF NECESS., MOD PROCESSING DONE HERE  09350000
         LA    R0,EODADOUT        ADDRESS OF NEW OUTPUT TAPE EODAD RTN  09360000
         ST    R0,OUTPUT+(DCBEODAD-IHADCB) AND SET IT UP IN DCB.        09370000
FILELOOP TM    COPYFLG,X'02'      SEE IF MOD SPECIFIED                  09380000
         BO    FILEFSM            IF SO THEN DO SRCH FWRD FOR TPMK      09390000
         CLC   COUTFILE,JOUTFLSQ  ARE WE IN RIGHT FILE YET?             09400000
         BNL   FILEMSG            BIF YES                               09410000
FILEFSM  LA    R1,=AL3(FSMCMND)   FWRD SPACE JUST PAST NEXT TAPEMARK    09420000
         LA    R2,OUTPUT                                                09430000
         BAL   R4,EXECEXCP                                              09440000
         LA    R0,1               MAINTAIN                              09450000
         AH    R0,COUTFILE        CURRENT FILE                          09460000
         STH   R0,COUTFILE        NUMBER.                               09470000
         CH    R0,JOUTFLSQ        BIF NOT YET UP TO SPECIFIED FILE      09480000
         BL    *+12               BIF NOT YET UP TO SPECIFIED FILE      09490000
         TM    COPYFLG,X'02'      SEE IF EOVMOD SPECIFIED               09500000
         BZ    FILEMSG            BIF   EOVMOD NOT SPECIFIED            09510000
         LA    R1,=AL3(READCMND)  PREPARE TO READ WHAT IMMEDIATELY-CWB- 09520000
         LA    R2,OUTPUT               FOLLOWS THE TAPE MARK.     -CWB- 09530000
         BAL   R4,EXECEXCP        (CHANGED TO EXCP FROM BSAM.)    -CWB- 09540000
         B     FILELOOP           ELSE IT'S A BLOCK, SO JUST GO ON      09550000
FILEPOS  DS    0H                 BACKSPACE PAST 2ND TAPEMARK OF EOV    09560000
         LA    R2,OUTPUT          PTR TO OUTPUT TAPE'S DCB              09570000
         LA    R1,=AL3(BSFCMND)   BACKSPACE PAST ONE TAPEMARK           09580000
         BAL   R4,EXECEXCP                                              09590000
         LH    R0,COUTFILE        AND                                   09600000
         BCTR  R0,0               REDUCE CURRENT FILE NO. APPROPRIATELY 09610000
         STH   R0,COUTFILE                                              09620000
FILEMSG  DS    0H                 PRINT INITIAL OUTPUT POSITION MSG     09630000
         LH    R1,COUTFILE                                              09640000
         CVD   R1,BADLNGTH                                              09650000
         OI    BADLNGTH+7,X'0F'                                         09660000
         UNPK  POSMSG+32(4),BADLNGTH TELL WHERE 1ST FILE OUTPUT GO-CWB- 09670000
         MVC   POSMSG+47(4),POSMSG+32 MOVE INTO EXPLANATORY COMMEN-CWB- 09680000
         BAL   R2,PUTLINE                                               09690000
         MVC   MSGBUF(L'POSMSG),POSMSG                                  09700000
         B     NOTBOTH                                                  09710000
EODADOUT LA    R0,1               COUNT                                 09720000
         AH    R0,COUTFILE        THIS                                  09730000
         STH   R0,COUTFILE        TAPEMARK (THE 2ND ONE OF AN EOV).     09740000
         CH    R0,JOUTFLSQ        SEE IF UP TO SPECIFIED TAPEMARK       09750000
         BL    TOPT456            BIF NOT UP TO SPECIFIED TAPEMARK      09760000
         TM    COPYFLG,X'02'      SEE IF EOVMOD SPECIFIED               09770000
         BO    FILEPOS            BIF EOVMOD SPECIFIED                  09780000
         TM    OPTNO,X'10'        SEE IF 'IGNORE EOV'S UNTIL FLSQ SATIS 09790000
         BO    FILEPOS            BIF IT IS SPECIFIED                   09800000
         B     BADEOV             GIVE 'EM HELL, HARRY!                 09810000
*                                                                       09820000
NOTTAPEO MVC   IDNTAPE+1(6),=C'OUTPUT'   MOVE 'OUTPUT' TO MSG     -HMD- 09830000
NOTTAPEI BAL   R2,PUTLINE         PUT OUT LINE.                   -HMD- 09840000
         MVC   MSGBUF(L'IDNTAPE),IDNTAPE MOVE MSG TO BUFFER       -HMD- 09850000
         B     EXITRC8            RETURN WITH BAD CODE            -HMD- 09860000
*                                                                       09870000
TOPT456  TM    OPTNO,X'10'        TEST FOR OPT268435456                 09880000
         BO    FILELOOP           AND IF SPECIFIED IGNORE DOUBLE TPMK   09890000
BADEOV   BAL   R2,PUTLINE                                               09900000
         MVC   MSGBUF(83),=C'0ERROR - EOV INDICATION ENCOUNTERED BEFORE+09910000
                OUTPUT TAPE FINISHED INITIAL POSITIONING'               09920000
         B     EXITRC8                                                  09930000
EODADBAD BAL   R2,PUTLINE                                               09940000
         MVC   MSGBUF(092),=C'0ERROR - OUTPUT TAPE HAD TAPEMARK FOLLOWI+09950000
               NG LOAD POINT, BUT NEITHER LTM OR BLP WAS SPECIFIED'     09960000
*                                 (LENGTH FIELD CORRECTED.)       -CWB- 09970000
         B     EXITRC8                                                  09980000
NOTBOTH  DS    0H                                                       09990000
         L     R12,=A(EXIT+4096)  LOAD 2ND BASE REG FOR MAIN CODE       10000000
         DROP  R12                DROP R12 FOR COMMON STUFF, ONLY R11   10010000
         B     SKIPEOVP           INITIALIZATION ENDS HERE              10020000
         USING TAPESCAN,R12                                             10030000
NOOUTPUT MVC   BADINPUT+1(6),=C'OUTPUT'                                 10040000
NOINPUT  BAL   R2,PUTLINE         BAD OR MISSING DD ROUTINE             10050000
         MVC   MSGBUF(L'BADINPUT),BADINPUT                              10060000
         B     EXITRC8                                                  10070000
         DROP  R12                                                      10080000
BSFCMND  CCW   X'2F',0,X'70',1    BSF, CC,SLI,SKIP                      10090000
         CCW   X'04',SENSBYTS,X'20',24 SENSE SLI UP TO 24 BYTES         10100000
READ81CM CCW   2,0,X'60',81       SET TO POINT TO RECBUF DYNAMICALLY    10110000
         CCW   4,SENSBYTS,X'20',24 AND SENSE FOR DEBUG                  10120000
JFCBOUT  DS    22D                                                      10130000
* JOUTFLSQ EQU   JFCBOUT+(JFCBFLSQ-JFCB)                          *JLM* 10140000
*  RELOCATE ABOVE STATEMENT TO FOLLOW JFCB (IFOX00)               *JLM* 10150000
EXLSTOUT DC    0F'0',X'87',AL3(JFCBOUT)                                 10160000
GMCTRL   DC    A(RECBUF+136-TRT1)  MINIMUM LENGTH FOR THE GETMAIN       10170000
         DC    A(RECBUF-TRT1+65536) MAXIMUM LENGTH FOR THE GETMAIN      10180000
GMLOCS   DC    2F'0'                                                    10190000
DTYPE    DC    2F'0'              TO HOLD DEVTYPE INFO            -HMD- 10200000
SVR2     DC    F'0'               SAVE AREA FOR R2 OVER TRT       -HMD- 10210000
EXITLIST DS    0F                 INPUT DCB EXIT LIST FOR RDJFCB        10220000
         DC    X'87'              LAST ENTRY AND RDJFCB                 10230000
         DC    AL3(JFCBINX)       BUFFER FOR INPUT TAPE'S JFCB          10240000
BADINPUT DC    C'0INPUT  DD STATEMENT MISSING OR INVALID'               10250000
IDNTAPE  DC    C'0INPUT DEVICE IS NOT MAGNETIC TAPE - EXECUTION TERMINAX10260000
               TED '                                              -HMD- 10270000
POSMSG   DC    C'0FIRST OUTPUT FILE ON SCRTCH IS 0000 -- LABEL=(0000,BL+10280000
               P)'                                                -CWB- 10290000
MORECORE DC    C'0WARNING:  TAPESCAN SHOULD HAVE 00000 MORE BYTES OF CO+10300000
               RE FOR RELIABLE OPERATION; PROCESSING WILL BE ATTEMPTED +10310000
               ANYWAY.'                                                 10320000
CTABLE   DC    C'0123456789ABCDEF'                                      10330000
SAVEVOL  DC    CL6' '             SAVE AREA FOR OUTPUT VOLSER     -HMD- 10340000
SVOUTFLG DC    C'N'               C'Y' = 'SAVE OUTPUT VOL LABEL '       10350000
VERIFLG  DC    C'N'                                                     10360000
OUTLTYP  DC    C'0'               FOR SAVING OUTPUT TAPE'S LABEL TYPE   10370000
COPYFLG  DC    X'00'              COPY AND MOD OPTION REQUEST BITS      10380000
VOLSW    DC    X'00'              SWITCH FOR ALPHANUMERIC TEST    -HMD- 10390000
VOLSWNV  EQU   X'80'              VOLSER NOT ALPHANUMERIC         -HMD- 10400000
         LTORG                                                          10410000
         EJECT                                                          10420000
* COMMON ROUTINES FOR BOTH INITIALIZATION AND MAIN ARE HERE.            10430000
EXIT     MVI   RCINSTR+3,X'00'    NORMAL END, SET RET CODE = 0          10440000
EXITRC8  DS    0H                                                 -CWB- 10450000
         TM    OUTPUT+(DCBOFLGS-IHADCB),DCBOFOPN                        10460000
         BZ    CLOSEIN            IF NOT, SKIP CLOSE.             -CWB- 10470000
         LA    R1,EXCPIOB         RESTORE IOB POINTER IN DCB      -CWB- 10480000
         IC    R0,OUTPUT+(DCBIOBA-IHADCB) TO POINT TO TRUE IOB, NOT     10490000
         ST    R1,OUTPUT+(DCBIOBA-IHADCB) BSAM-TYPE IOB PREFIX.         10500000
         STC   R0,OUTPUT+(DCBIOBA-IHADCB)                               10510000
         NI    OUTPUT+(DCBOFLGS-IHADCB),X'7F' PRETEND LAST I/O WAS A    10520000
         OI    OUTPUT+(DCBOFLGS-IHADCB),X'04' READ TO AVOID WRITING A   10530000
*                                      TAPE MARK.                 -CWB- 10540000
         CLOSE  OUTPUT            CLOSE OUTPUT TAPE.              -CWB- 10550000
CLOSEIN  TM    INPUT+(DCBOFLGS-IHADCB),DCBOFOPN                         10560000
         BZ    NOTOPEN            PROGRAM DIDN'T GET VERY FAR,    -CWB- 10570000
*                                      DID IT.                    -CWB- 10580000
         CLOSE INPUT              CLOSE INPUT TAPE.               -CWB- 10590000
NOTOPEN  DS    0H                                                 -CWB- 10600000
         L     R13,SAVE+4         MAY BYPASS UNRELEASED SYNAD AREA      10610000
         LM    14,12,12(13)                                             10620000
RCINSTR  LA    15,8                                                     10630000
         BR    R14                FINAL EXIT FROM TAPESCAN IN ALL CASES 10640000
PUTLINE  DS    0H                 GENERAL PRINTING SUBROUTINE, ENTRY 1  10650000
         MVC   MSGBUF,BLANKBUF    CLEAR BUFFER FAST                     10660000
PUTLINE2 EX    0,0(R2)            GENERAL PRINTING SUBROUTINE, ENTRY 2  10670000
PUTLINE3 CLI   MSGBUF,C' '        GENERAL PRINTING SUBROUTINE, ENTRY 3  10680000
         BE    LNCOUNT-4          BIF CARR. CTRL  CHAR IS A BLANK       10690000
         CLI   MSGBUF,C'0'        SEE IF CARRIAGE CONTROL CHAR IS ZERO  10700000
         BE    C0                 BIF IS A ZERO                         10710000
         LA    R0,3               NO BLANK OR ZERO, MUST BE A MINUS     10720000
         B     LNCOUNT                                                  10730000
C0       LA    R0,2               COUNT TWO LINES                       10740000
         B     LNCOUNT                                                  10750000
         LA    R0,1               COUNT ONE LINE                        10760000
LNCOUNT  A     R0,LINENO                                                10770000
         ST    R0,LINENO                                                10780000
         C     R0,LINECNT         COMPARE WITH MAX LINES          -HMD- 10790000
         BNH   SAMEPAGE                                                 10800000
         MVC   PAGECHAR,=X'40202120' EDIT PATTERN                       10810000
         L     R1,PAGECNT         INCREMENT THE PAGE COUNT.       -CWB- 10820000
         LA    R1,1(R1)                (DECIMAL INSTRUCTIONS      -CWB- 10830000
         ST    R1,PAGECNT              REPLACED BY BINARY.)       -CWB- 10840000
         CVD   R1,DBLPAGE         MAKE IT PRINTABLE.              -CWB- 10850000
         ED    PAGECHAR,DBLPAGE+6                                 -CWB- 10860000
         MVI   LINENO+3,X'00'                                           10870000
         PUT   SYSPRINT,PAGEHDR                                         10880000
         SPACE 1                                               HD AUG86 10890000
         CLC   =C'VTOC--',0(R2)   ARE WE IN VTOC ROUTINE?      HD AUG86 10900000
         BNE   SAMEPAGE           NO, CONTINUE                 HD AUG86 10910000
         MVC   HOLDBUF,BLANKBUF   CLEAR BUFFER FAST            HD AUG86 10920000
         L     R1,=A(VTOCHED2)    GET HEADER TEXT AND MOVE     HD AUG86 10930000
         MVC   HOLDBUF(L'VTOCHED2),0(R1)                       HD AUG86 10940000
         MVI   LINENO+3,X'04'     INDICATE 3 LINES             HD AUG86 10950000
         PUT   SYSPRINT,HOLDBUF   PUT THE LINE OUT             HD AUG86 10960000
         MVI   MSGBUF,C'0'        INDICATE SKIP 1 LINE         HD DEC86 10970000
         SPACE 1                                               HD AUG86 10980000
SAMEPAGE PUT   SYSPRINT,MSGBUF                                          10990000
         B     6(R2)              RETURN FROM PUTLINE SUBROUTINE        11000000
PAGECHK  DS    0H                 LINE RESERVATION SUBROUTINE           11010000
         A     R0,LINENO          ADD LINES TO BE RESERVED TO LINE NO   11020000
         C     R0,LINECNT         COMPARE TO MAXIMUM LINE  NUMBER -HMD- 11030000
         BCR   13,R2              RETURN IF CURRENT PAGE HAS ENUF ROOM  11040000
         MVI   LINENO+3,0         ZERO LINE COUNTER                     11050000
*        AP    PAGEPACK,=P'1'     COUNT NEW PAGE                  -CWB- 11060000
         MVC   PAGECHAR,=X'40202120'                                    11070000
*        ED    PAGECHAR,PAGEPACK                                  -CWB- 11080000
         L     R1,PAGECNT         INCREMENT THE PAGE COUNT.       -CWB- 11090000
         LA    R1,1(R1)                (DECIMAL INSTRUCTIONS      -CWB- 11100000
         ST    R1,PAGECNT              REPLACED BY BINARY.)       -CWB- 11110000
         CVD   R1,DBLPAGE         MAKE IT PRINTABLE.              -CWB- 11120000
         ED    PAGECHAR,DBLPAGE+6                                 -CWB- 11130000
         PUT   SYSPRINT,PAGEHDR                                         11140000
         BR    R2                                                       11150000
*BADREC  WTL   'BAD (0) BLOCK SIZE' (OFTEN MEANS TAPE OFF END OF REEL)  11160000
BADREC   DS    0H                                               -CWB-   11170000
         BAL   R2,PUTLINE                                               11180000
         MVC   MSGBUF(L'BADRECM),BADRECM                                11190000
         B     EXITRC8                                                  11200000
SYNERR   SYNADAF ACSMETH=BSAM                                           11210000
SYNERR2  ST    R14,SVR14                                                11220000
         CH    R0,=H'4'           CHECK SYNADAF'S RETURN CODE           11230000
         BNE   DIRECT                                                   11240000
*        LH    R14,12(R1)         LOAD NO. OF BYTES READ                11250000
*  -----    CHANGE HALFWORD ARITHMETIC TO FULLWORD.                     11260000
         XC    FULLWRK1,FULLWRK1  CLEAR FULLWORK WORKAREA               11270000
         MVC   HALFWRK1(2),12(R1)    MOVE FULL HALFWORD                 11280000
         L     R14,FULLWRK1       FULLWORD LOAD                         11290000
         CVD   R14,BADLNGTH                                             11300000
         OI    BADLNGTH+7,X'0F'                                         11310000
         UNPK  32(5,R1),BADLNGTH                                        11320000
         MVC   8(24,R1),=C'0I/O ERROR - BYTES READ='                    11330000
DIRECT   BAL   R2,PUTLINE         PRINT SYNAD ERROR MESSAGE             11340000
         MVC   MSGBUF(120),8(R1)                                        11350000
         LA    R14,1                                                    11360000
         A     R14,ERRCOUNT       INCREMENT ERROR COUNT                 11370000
         ST    R14,ERRCOUNT                                             11380000
         C     R14,SYNADNO        COMPARE ERROR COUNT WITH ERROR LIMIT  11390000
         BH    TOOMANY            PRINT MSG & EXIT IF ERR LIM EXCEEDED  11400000
         SYNADRLS                                                       11410000
         L     R14,SVR14                                                11420000
         BR    R14                                                      11430000
TOOMANY  BAL   R2,PUTLINE         YOU SHOULD PROBABLY SET ERRLIM TO 0 - 11440000
         MVC   MSGBUF(55),=C'0TAPESCAN TERMINATING DUE TO EXCESSIVE I/O+11450000
                ERROR COUNT.'                                           11460000
         B     EXITRC8            FOR TAPE COPYING OPERATIONS.          11470000
EXECEXCP DS    0H                 EXCP SUBROUTINE, R2=PTR TO DCB, AND  +11480000
                                  R1=PTR TO 3-BYTE ADCON WHICH POINTS  +11490000
                                  TO CHANNEL PROGRAM.                   11500000
         ST    R2,EXCPDCB         SAVE DCB POINTER.               -CWB- 11510000
         L     R2,DCBIOBA-IHADCB(R2) GET PTR TO A BSAM IOB PREFIX       11520000
         MVC   STARTSAV(3),IOBSTART+9(R2) SAVE PTR TO BSAM'S CCW        11530000
         MVC   IOBSTART+9(3,R2),0(R1) MOVE IN PTR TO CHAN PROG          11540000
         LA    R1,4(R2)           LOAD PTR TO ECB IN PREFIX             11550000
         ST    R1,IOBECBPT+8(R2)  STORE PTR TO ECB                      11560000
         XC    0(4,R1),0(R1)      CLEAR ECB IN IOB PREFIX               11570000
         EXCP  8(R2)              EXCP USING BSAM'S IOB AND ECB         11580000
         WAIT  ECB=4(R2)                                                11590000
         L     R1,IOBSTART+8(R2)  RESTORE CCW POINTER.            -CWB- 11600000
         MVC   IOBSTART+9(3,R2),STARTSAV RESTORE PTR TO BSAM'S CCW      11610000
         CLI   4(R2),X'7F'        SEE IF EXCP WORKED PERFECTLY          11620000
         BCR   8,R4               RETURN IF IT DID                      11630000
         CLI   4(R2),X'41'        DID PERMANENT ERROR OCCUR?      -CWB- 11640000
         BNE   DOSYN              OTHER ERRORS BRANCH.            -CWB- 11650000
         CLI   0(R1),X'02'        WAS COMMAND CODE A READ?        -CWB- 11660000
         BNE   DOSYN              IF NOT, DO ERROR.               -CWB- 11670000
*        LH    R1,6(R1)           GET COMMAND LENGTH.             -CWB- 11680000
*  -----    CHANGE HALFWORD ARITHMETIC TO FULLWORD.                     11690000
         XC    FULLWRK1,FULLWRK1  CLEAR FULLWORK WORKAREA               11700000
         MVC   HALFWRK1(2),6(R1)      GET COMMAND LENGTH                11710000
         L     R1,FULLWRK1        FULLWORD LOAD                         11720000
*        CH    R1,22(R2)          IS RESIDUAL LENGTH THE SAME?    -CWB- 11730000
*  -----    CHANGE HALFWORD ARITHMETIC TO FULLWORD.                     11740000
         XC    FULLWRK2,FULLWRK2  CLEAR FULLWORK WORKAREA               11750000
         MVC   HALFWRK2(2),22(R2)    MOVE RESIDUAL LENGTH               11760000
         C     R1,FULLWRK2        FULLWORD COMPARE                      11770000
         BNE   DOSYN              IF NOT, BRANCH FOR ERROR.       -CWB- 11780000
         L     R1,EXCPDCB         ELSE, MUST BE TAPE MARK READ.   -CWB- 11790000
         C     R1,=A(INPUT)       IS THIS THE INPUT DCB?          -HMD- 11800000
         BNE   LOADEOD            NO, LOAD THE EODAD FROM THE DCB -HMD- 11810000
         L     R1,EODADDR         GET PREDEFINED EOD ADDRESS      -HMD- 11820000
         BR    R1                 GO TO IT                        -HMD- 11830000
LOADEOD  L     R1,DCBEODAD-IHADCB(R1) GET EOD FROM DCB            -HMD- 11840000
         BR    R1                 GO TO IT                        -CWB- 11850000
DOSYN    DS    0H                                                 -CWB- 11860000
         LA    R1,8(R2)           GET PTR TO IOB FOR SYNADAF MACRO      11870000
         SYNADAF ACSMETH=EXCP     WORKS OK EVEN THO IOBSTART RESTORED   11880000
         BAL   R14,SYNERR2        NOW DO REST OF ANALYSIS WITH SYNERR   11890000
         BR    R4                                                       11900000
         EJECT                                                          11910000
* CONSTANTS AND STORAGE FOR THIS SECTION                                11920000
SAVE     DS    9D                                                       11930000
JFCBINX  DS    22D                176 BYTES                             11940000
BADLNGTH DC    D'0'                                                     11950000
DBLPAGE  DC    D'0'               CVD WORK AREA FOR PAGE NUMBER.  -CWB- 11960000
FSMCMND  CCW   X'3F',0,X'30',1    FWRD SPACE FILE, SKIP/SLI             11970000
RWNDCMND CCW   7,0,X'60',1        REWIND TAPE, CHAIN CMND, SLI          11980000
SENSCMND CCW   4,SENSBYTS,X'20',24 SENSE UP TO 24 BYTES, SLI            11990000
READCMND CCW   X'02',0,X'20',65535  READ COMMAND.                 -CWB- 12000000
READLENG EQU   READCMND+6           READ LENGTH FIELD.            -CWB- 12010000
WRTCMND  CCW   1,0,X'20',0        PTR TO RECBUF UPDATED AFTER GM. -CWB- 12020000
WTMCMND  CCW   X'1F',0,X'60',1    WRITE TM, SLI, CC TO SENSE.     -CWB- 12030000
         CCW   X'04',SENSBYTS,X'20',1  SENSE CHAINED FOR DEV END. -CWB- 12040000
*                                 ABOVE FIELDS MOVED HERE FROM    -CWB- 12050000
*                                      LATTER PART OF PROGRAM TO  -CWB- 12060000
*                                      CORRECT ADDRESSABILITY.    -CWB- 12070000
SENSBYTS DC    3XL8'00'           BUFFER FOR SENSE BYTES                12080000
AMSGBP46 DC    A(0)               A(MSGBUF+46) SET UP DURING INIT -CWB- 12090000
AMSGBP72 DC    A(0)               A(MSGBUF+72) AS ABOVE.          -CWB- 12100000
ERRCOUNT DC    F'0'               NUMBER OF SYNAD EXITS TAKEN (I/O ERRS 12110000
OPTNO    DC    F'0'               BIT ORIENTED OPTIONS - MISC/DEBUG     12120000
LINENO   DC    F'90'                                              -CWB- 12130000
EODADDR  DC    F'0'               FAKE END OF DATA ADDRESS        -HMD- 12140000
LISTNO   DC    F'4'               NUMBER OF BLKS TO LIST PER DATASET    12150000
SKIPTMNO DC    F'0'                                                     12160000
MAXTMNO  DC    F'32767'                                                 12170000
SKPEOVNO DC    F'0'                                                     12180000
MAXEOVNO DC    F'1'                                                     12190000
SYNADNO  DC    F'50'              MAX SYNAD EXITS BEFORE TERMINATION    12200000
READNO   DC    F'3'               SET TO ONE  FOR NOSUMMARY OPTION      12210000
PAGECNT  DC    F'0'               PAGE COUNTER.                   -CWB- 12220000
LINECNT  DC    F'&LINECNT'        LINES/PAGE                      -HMD- 12230000
SVR14    DC    F'0'                                               -CWB- 12240000
COUTFILE DC    H'1'               CURRENT OUTPUT FILE SEQ. NO.          12250000
*PAGEPACK DC   PL2'0'             (REPL'D BY PAGECNT)             -CWB- 12260000
BADRECM  DC    C'0BAD (0) BLOCK SIZE ENCOUNTERED'                       12270000
ERRSUMSG DC    C'0NUMBER OF I/O ERRORS=XXXXX'                           12280000
PAGEHDR  DC    CL60'1TAPESCAN 5.2  --------  TAPE ANALYSIS AND COPYING +12290000
               PROGRAM  '                                   -CWB-       12300000
DATERSLT DC    CL8'MM/DD/YY'      DATE WILL BE PLACED HERE              12310000
         DC    CL2'  '                                                  12320000
TIMERSLT DC    XL11'4021207A20207A20204B20'                             12330000
         DC    C'   INPUT VOL='                                   -CWB- 12340000
DDVOL    DC    CL6'VVVVVV'                                        -CWB- 12350000
         DC    CL12'    DENSITY='                                 -SRH- 12360000
TAPEDENS DC    CL1'X'                                                   12370000
         DC    CL2' ('                                                  12380000
PRTDENS  DC    CL4' XXX'                                                12390000
         DC    CL4'BPI)'                                                12400000
         DC    CL6'  PAGE'                                              12410000
PAGECHAR DC    X'40202120'        EXAMPLE EDIT PATTERN FOR PAGE NUMBER  12420000
         DC    CL12'           '  PAGE TRAILING BLANKS                  12430000
JFCLTSV  DC    X'FF'              FOR SAVING JFCBLTYP BYTE              12440000
SENSW    DC    X'00'              SENS INFO SWITCH                *SRH* 12450000
EXCPIOBP DC    0D'0',A(*)         IOB BSAM-TYPE PREFIX.           -CWB- 12460000
EXCPECB  DC    F'0'                                               -CWB- 12470000
EXCPIOB  DC    B'01000010',XL3'0',A(EXCPECB,0),H'0'  IOB PROPER.  -CWB- 12480000
RESIDL   DC    H'0'               RESIDUAL COUNT.                 -CWB- 12490000
EXCPCCW  DC    A(0)               ADDRESS OF CHANNEL PROGRAM.     -CWB- 12500000
         DC    A(OUTPUT,0,0)                                      -CWB- 12510000
EXCPDCB  DC    A(0)               DCB ADDRESS SAVE AREA.          -CWB- 12520000
STARTSAV DC    C'SAV'             FOR SAVING BSAM'S IOBSTART FIELD      12530000
WRTFLG   DC    X'00'              TWO BIT COPY FLAG, X'03'=WRITE BLOCK  12540000
COUNTFLG DC    C'Y'               COUNT OPTION DEFAULT VALUE            12550000
HEXFLG   DC    C'Y'                                                     12560000
SUMFLG   DC    C'Y'                                                     12570000
VTOCFLAG DC    C'N'               VTOC ONLY FLAG               HD OCT88 12580000
* ----------------------------------------------------------  SBG 04/00 12590000
         DS    0D               START ON DWORD BOUNDARY                 12600000
FULLWRK1 DS    0F               FULLWORD WORK AREA                      12610000
         DC    H'0'                                                     12620000
HALFWRK1 DC    H'0'             HALFWORD WORK AREA                      12630000
FULLWRK2 DS    0F               FULLWORD WORK AREA                      12640000
         DC    H'0'                                                     12650000
HALFWRK2 DC    H'0'             HALFWORD WORK AREA                      12660000
* ----------------------------------------------------------  SBG 04/00 12670000
         EJECT                                                          12680000
SYSPRINT DCB   DDNAME=SYSPRINT,MACRF=PM,DSORG=PS,RECFM=FBA,            +12690000
               LRECL=133,BLKSIZE=133                         FPAJ MAY99 12700000
         EJECT                                                          12710000
* MAXIMUM BLKSIZE IS 32760                                     HD DEC86 12720000
INPUT    DCB   DDNAME=INPUT,MACRF=RC,DSORG=PS,RECFM=U,DEVD=TA,         +12730000
               BLKSIZE=32760,EODAD=EODS,SYNAD=SYNERR,EXLST=EXITLIST     12740000
         EJECT                                                          12750000
OUTPUT   DCB   DDNAME=OUTPUT,MACRF=(E),EODAD=EODADBAD,DSORG=PS,   -CWB-+12760000
               IOBAD=EXCPIOB,DEVD=TA,EXLST=EXLSTOUT               -CWB- 12770000
         EJECT                                                          12780000
         LTORG                                                          12790000
         EJECT                                                          12800000
* MAIN LOOP AND MAIN LINE CODE STARTS HERE                              12810000
SKIPEOVP DS    0H                 SKIPEOV OPTION CONTROL ROUTINE        12820000
         USING EXIT+4096,R12                                            12830000
         SR    R3,R3              CLEAR R3 TILL WE GET A VTOC     -CWB- 12840000
*                                      BLOCK ENTRY.               -CWB- 12850000
         CLC   CEOVNO,SKPEOVNO                                          12860000
         BNL   SKIPTMPR                                                 12870000
         SR    R4,R4              ZERO TO GET GOOD BLK CNT EVEN IF     +12880000
                                  SKIPTM OR SKIPEOV USED.               12890000
         SR    R8,R8              (RE)-ZERO BYTE COUNTER                12900000
         ST    R8,BLKCNT          (RE)-ZERO BLKCNT                      12910000
         BAL   R5,READER          CHECK FOR DOUBLE TAPEMARK             12920000
         C     R7,=F'80'          SEE IF BLKSIZE=80 (LIKE ALL LABELS)   12930000
         BNE   CONTROL            BIF BLKLNGTH NE 80 (I.E., IT'S NOT A +12940000
                                                    LABEL).             12950000
         C     R4,=F'1'           SEE IF THIS IS THE 1ST BLK AFTER A   +12960000
                                  TAPEMARK OR THE LOAD POINT.           12970000
         BNE   CONTROL            BIF IT ISN'T THE FIRST                12980000
         CLC   RECBUF(4),=C'EOV1'                                       12990000
         BNE   CONTROL                                                  13000000
         MVI   EOV1FLG,C'Y'       SET 'EOV PENDING' FLAG TO 'YES'       13010000
         B     CONTROL            POSITION PAST TAPEMARK AND GOTO EODS  13020000
SKIPTMPR CLC   CTPMKNO,SKIPTMNO   SKIPTM OPTION CONTROL ROUTINE         13030000
         BL    SKIPEOVP+12                                              13040000
         SR    R8,R8              CLEAR CTR FOR TOTAL BYTES ON TAPE     13050000
         ST    R8,BLKCNT          (RE)-ZERO BLKCNT                      13060000
         OI    WRTFLG,X'02'       OR IN  'ALL TM & EOV SKIPPING DONE'   13070000
         L     R1,CTPMKNO         GET TAPEMARK COUNT.             -CWB- 13080000
         SR    R0,R0              CALCULATE NUMBER OF SL FILES    -CWB- 13090000
         D     R0,=F'3'                WE HAVE SKIPPED.           -CWB- 13100000
         ST    R1,TRUESEQN        SAVE AS LABEL= VALUE.           -CWB- 13110000
         SR    R3,R3              CLEAR R3 TILL WE GET A VTOC     -CWB- 13120000
*                                      BLOCK ENTRY.               -CWB- 13130000
PROCESS  DS    0H                 TOP OF OUTER MAIN LOOP                13140000
         SR    R4,R4              ZERO BLK COUNTER                      13150000
         ST    R4,FILEBYTS        CLEAR FILE BYTE COUNT.          -CWB- 13160000
         ST    R4,MAX             RESET MAX BLKSIZE WATCHER             13170000
         MVC   MIN,=F'65535'      RESET MIN BLKSIZE WATCHER             13180000
         MVC   PREVHDR1,HDR1FLAG     SAVE PREV LABEL INDICATION.  -CWB- 13190000
         MVI   HDR1FLAG,C'N'             ASSUME NO HDR1 LABEL.    -CWB- 13200000
         MVI   LABLFLAG,C'N'      ASSUME THIS FILE IS NOT A LABEL.-CWB- 13210000
PROCESS2 DS    0H                 TOP OF MAIN INNER LOOP                13220000
         C     R4,READNO          SEE IF BLK IS TO BE READ FOR SUMMARY  13230000
         BNL   OTHRCHKS                                                 13240000
         BAL   R5,READON          READ BLK, CALC LENGTH, DO MIN/MAX     13250000
         C     R7,=F'80'          SEE IF BLKSIZE=80 (LIKE ALL LABELS)   13260000
         BNE   NOLABEL            BIF BLKLNGTH NE 80 (I.E., IT'S NOT A +13270000
                                                    LABEL).             13280000
         C     R4,=F'1'           SEE IF THIS IS THE 1ST BLK AFTER A   +13290000
                                  TAPEMARK OR THE LOAD POINT.           13300000
         BNE   *+18               BIF IT ISN'T THE FIRST                13310000
         CLC   RECBUF(4),=C'EOV1'                                       13320000
         BNE   *+8                                                      13330000
         MVI   EOV1FLG,C'Y'       SET 'EOV PENDING' FLAG TO 'YES'       13340000
         CLI   SUMFLG,C'Y'        SEE IF SUMMARY OPTION IS YES          13350000
         BE    CHKLABEL           DO SUMMARY PROCESSING IF SO           13360000
NOLABEL  C     R4,LISTNO                                                13370000
         BNH   LISTER             BRANCH TO LIST BLOCK                  13380000
         B     PROCESS2                                                 13390000
OTHRCHKS C     R4,LISTNO                                                13400000
         BNL   CHKCNT             BRANCH IF EVERYTHING ALREADY LISTED   13410000
         BAL   R5,READON                                                13420000
LISTER   BAL   R5,LISTON                                                13430000
         B     PROCESS2                                                 13440000
CHKCNT   CLI   COUNTFLG,C'N'                                            13450000
         BE    CONTROL                                                  13460000
         BAL   R5,READER          TOP    OF MAIN COUNTBLK OPTION LOOP   13470000
         B     *-4                BOTTOM OF MAIN COUNTBLK OPTION LOOP   13480000
CONTROL  CNTRL INPUT,FSM          FORWARD SPACE TO NEXT TAPEMARK,      +13490000
                                  THEN BACKSPACE OVER IT.               13500000
         SR    R4,R4              RE-ZERO CURRENT BLK COUNT             13510000
         ST    R4,MAX             ZERO MAX TO INHIBIT BLK COUNT MSG     13520000
         SR    R8,R8              IS THIS NECESSARY?                    13530000
         BAL   R5,READON          NOW READ THE TAPEMARK                 13540000
         BAL   R5,LISTON          RETURN HERE IF NO TAPEMARK - ERROR!   13550000
         BAL   R2,PUTLINE         PRINT ERR MSG                         13560000
         MVC   MSGBUF(37),=C'0ERROR - EXPECTED TAPEMARK NOT FOUND.'     13570000
         B     EXITRC8                                                  13580000
READON   DS    0H                                                       13590000
         MVC   RECBUF,BLANKBUF    CLEAR FIRST PART OF BUFFER FAST       13600000
READER   XC    TAPE(4),TAPE       CLEAR ECB                             13610000
         LA    R2,RECBUF                                                13620000
         READ  TAPE,SF,INPUT,(R2),65535                                 13630000
*                                     READ A BLK OR TAPEMARK (WE HOPE)  13640000
         CHECK TAPE                                                     13650000
         MVI   MARK,X'00'         SET TO X'FF' WHEN TAPEMARK READ (EOD) 13660000
         LA    R4,1(R4)           COUNT THE BLK JUST READ FOR DATASET   13670000
         L     R6,TAPE+16         GET PTR TO IOB                        13680000
*        LH    R6,14(R6)          GET RESIDUAL COUNT                    13690000
*  -----    CHANGE HALFWORD ARITHMETIC TO FULLWORD.                     13700000
         XC    FULLWRK1,FULLWRK1  CLEAR FULLWORK WORKAREA               13710000
         MVC   HALFWRK1(2),14(R6)     FULL HALFWORD OF RESIDUAL COUNT   13720000
         L     R6,FULLWRK1        FULLWORD LOAD                         13730000
*        LH    R7,INPUT+(DCBBLKSI-IHADCB)                               13740000
*  -----    CHANGE HALFWORD ARITHMETIC TO FULLWORD.                     13750000
         XC    FULLWRK2,FULLWRK2  CLEAR FULLWORK WORKAREA               13760000
*        MVC   HALFWRK2(2),INPUT+(DCBBLKSI-IHADCB)                      13770000
         MVC   HALFWRK2(2),=X'FFFF'    ASSUME BLOCKSIZE IS 65535        13780000
         L     R7,FULLWRK2        FULLWORD LOAD                         13790000
         SR    R7,R6              COMPUTE BLOCK'S LENGTH                13800000
         BNH   BADREC             BIF ZERO OR NEG RECORD LENGTH         13810000
         L     R1,FILEBYTS        ADD BLOCK LENGTH TO TOTAL       -CWB- 13820000
         AR    R1,R7                   BYTE COUNT FOR THIS        -CWB- 13830000
         ST    R1,FILEBYTS             FILE.                      -CWB- 13840000
         AR    R8,R7              COUNT ALL BYTES READ DURING RUN       13850000
         C     R7,MAX             COMPARE CURRENT BLK'S SIZE WITH MAX   13860000
         BNH   *+8                SKIP IF OLD MAX IS BIGGER             13870000
         ST    R7,MAX             STORE NEW MAX BLK LENGTH              13880000
         C     R7,MIN             COMPARE BLK'S SIZE WITH PREVIOUS MIN  13890000
         BNL   *+8                SKIP IF OLD MIN IS SMALLER            13900000
         ST    R7,MIN             STORE NEW MINIMUM BLOCK LENGTH        13910000
         SPACE 1                                               HD DEC86 13920000
         TM    SENSW,1        DID WE GET THE SENS INFO?                 13930000
         BO    DENEND         YEP,GO ON                                 13940000
         LA    R1,=AL3(SENSCMND) POINT TO SENS CMDS                     13950000
         LA    R2,INPUT       POINT TO INPUT DATASET                    13960000
         LR    R6,R4          SAVE R4                                   13970000
         BAL   R4,EXECEXCP    DO SENS EXCP                              13980000
         LR    R4,R6          RESTORE R4                                13990000
         OI    SENSW,1        TELL'M WE BEEN HERE BEFORE                14000000
         SPACE 1                                               HD DEC86 14010000
RD3480A  CLI   FLAG3480,C'Y'  ARE WE PROCESSING A 3480?        HD DEC86 14020000
         BNE   RD3480Z        NO, CHECK 1600/6250              HD DEC86 14030000
         MVI   TAPEDENS,C'5'  SELECT DENSITY=5                 HD NOV86 14040000
         MVC   PRTDENS(4),=CL4' 38K'                           HD JAN89 14050000
         B     DENEND         MOVE TEXT AND END DENSITY CHK    HD NOV86 14060000
RD3480Z  DS    0H                                              HD DEC86 14070000
         SPACE 1                                               HD DEC86 14080000
         TM    SENSBYTS+3,4   P.E. = 1600 BPI                           14090000
         BNO   TRY6250        NOPE, TRY 6250BPI                         14100000
         MVI   TAPEDENS,C'3'      SET DENS FOR 1600 BPI                 14110000
         MVC   PRTDENS(4),=CL4'1600'                                    14120000
         B     DENEND                                                   14130000
TRY6250  L     R2,(DCBDEBAD-IHADCB)+INPUT GET DEB PTR                   14140000
         L     R2,DEBUCBAD(R2)    GET UCB PTR                           14150000
         TM    16(R2),2           UCBTYP = 6250BPI?                     14160000
         BNO   TRY800             NOPE, TRY 800 BPI                     14170000
         MVI   TAPEDENS,C'4'      SET DEN FOR 6250BPI                   14180000
         MVC   PRTDENS(4),=CL4'6250'                                    14190000
         B     DENEND                                                   14200000
TRY800   TM    INPUT+(DCBDEN-IHADCB),B'10000011' 800BPI?                14210000
         BNO   TRY556             TRY 556BPI                            14220000
         MVI   TAPEDENS,C'2'      SET DENS                              14230000
         MVC   PRTDENS(4),=CL4' 800'  TO 800 BPI                        14240000
         B     DENEND             BRANCH AROUND 556 CODE         -HMD-  14250000
TRY556   DS    0H ANYONE STILL USING THESE TURKEYS? DO YOUR OWN THING   14260000
* I AM, AND WHOSE CALLING THEM TURKEYS, SUCKER                   -HMD-  14270000
         MVI   TAPEDENS,C'1'      DEN=1 FOR 556 BPI              -HMD-  14280000
         MVC   PRTDENS(4),=CL4' 556'  MOVE DEN TO PRINT          -HMD-  14290000
DENEND   DS    0H                                                       14300000
         CLI   WRTFLG,X'03'       SEE IF SHOULD WRITE TO OUTPUT TAPE    14310000
         BNE   READEREX                                                 14320000
         STH   R7,WRTCMND+6       STORE BLK LENGTH IN WRITE CCW         14330000
         LA    R1,=AL3(WRTCMND)                                         14340000
         LA    R2,OUTPUT                                                14350000
         LR    R6,R4              SAVE R4 (CURRENT FILE BLK CNT)        14360000
         BAL   R4,EXECEXCP                                              14370000
         LR    R4,R6              RESTORE R4 (CURRENT FILE BLK COUNT)   14380000
READEREX NI    WRTFLG,X'03'       ZERO OFF POSSIBLE 'NOVOLSER' BIT      14390000
         BR    R5                 RETURN FROM READON SUBROUTINE         14400000
LISTON   DS    0H                                              HD JAN89 14410000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 14420000
         BE    VTOCBYPD           YES..BYPASS PRINT            HD JAN89 14430000
         MVI   MSGBUF,C'0'                                              14440000
         LA    R0,4               PREPARE TO RESERVE 4 LINES            14450000
         BAL   R2,PAGECHK         RESERVE 4 LINES                       14460000
         BAL   R2,PUTLINE2        LIST THE FIRST PART OF THE BLOCK      14470000
         MVC   MSGBUF+1(132),RECBUF                                     14480000
HEXON    DS    0H                                              HD JAN89 14490000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 14500000
         BE    VTOCBYPD           YES, BYPASS PRINT            HD JAN89 14510000
         CLI   HEXFLG,C'N'        SEE IF HEXLIST SPECIFIED              14520000
         BCR   8,R5               BER R5 RETURN IF HEXLIST NOT SPECIFID 14530000
         MVC   MSGBUF+1(132),RECBUF                                     14540000
         MVI   TRINT1+1,131       SET UP DEFAULT  OF MOVE    =132 BYTES 14550000
         MVI   TRINT2+1,131       SET UP DEFAULT  OF MOVE    =132 BYTES 14560000
         C     R7,=F'132'         SEE IF LENGTH OF BLK EXCEEDS 1 LINE   14570000
         BH    TRINT1             BRANCH IF LE 132 BYTES                14580000
         BCTR  R7,0               CONVERT BLK LENGTH TO MACHINE LENGTH  14590000
         STC   R7,TRINT1+1        AND STORE IN 1ST TR INSTRUCTION       14600000
         STC   R7,TRINT2+1        AND IN THE SECOND ONE                 14610000
TRINT1   TR    MSGBUF+1(132),TRT1 MODIFIED INSTRUCTION (LENGTH)         14620000
         MVI   MSGBUF,C' '        SET CARRIAGE CONTROL                  14630000
         BAL   R2,PUTLINE3        PRINT FIRST LINE OF HEX               14640000
         DC    C'TRT111'                                                14650000
         MVC   MSGBUF+1(132),RECBUF                                     14660000
TRINT2   TR    MSGBUF+1(132),TRT2 MODIFIED INSTRUCTION (LENGTH)         14670000
         BAL   R2,PUTLINE3        PRINT SECOND LINE OF HEX              14680000
         DC    C'TRT222'          FILLER                                14690000
VTOCBYPD BR    R5                 RETURN FROM LISTON OR HEXON SUBR      14700000
CHKLABEL DS    0H                                                       14710000
         MVI   LABLFLAG,C'Y'      NOTE WE HAVE A LABEL.           -CWB- 14720000
         MVI   MSGBUF,C'0'                                              14730000
         MVC   MSGBUF+1(132),RECBUF                                     14740000
         CLC   RECBUF(4),=C'HDR1'                                       14750000
         BE    HDR1                                                     14760000
         CLC   RECBUF(4),=C'EOF1'                                       14770000
         BE    EOF1EOV1                                                 14780000
         CLC   RECBUF(4),=C'HDR2'                                       14790000
         BE    HDR2                                                     14800000
         CLC   RECBUF(4),=C'EOF2'                                       14810000
         BE    EOF2EOV2                                                 14820000
         CLC   RECBUF(4),=C'VOL1'                                       14830000
         BE    VOL1                                                     14840000
         CLC   RECBUF(4),=C'EOV1'                                       14850000
         BE    EOF1EOV1                                                 14860000
         CLC   RECBUF(4),=C'EOV2'                                       14870000
         BE    EOF2EOV2                                                 14880000
         MVI   LABLFLAG,C'N'      OOPS, NO LABEL.                 -CWB- 14890000
         B     NOLABEL            IT'S NOT A LABEL AFTER ALL            14900000
HDR1     DS    0H                                                       14910000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 14920000
         BE    VTOCBYP1           YES, BYPASS THIS MESS        HD JAN89 14930000
         MVI   HDR1FLAG,C'Y'      NOTE HEADER LABEL FILE.         -CWB- 14940000
         LA    R0,14              PREPARE TO RESERVE 14 LINES           14950000
         BAL   R2,PAGECHK                                               14960000
         MVC   MSGBUF,BLANKBUF    CLEAR PRINT BUFFER FAST               14970000
         MVI   MSGBUF+43,C'*'     MOVE IN SEED FOR FILL                 14980000
         MVC   MSGBUF+44(47),MSGBUF+43 FILL IN REST OF ASTERISKS        14990000
         MVI   MSGBUF,C'-'                                              15000000
         BAL   R2,PUTLINE3        PRINT LINE OF ASTERISKS               15010000
         DC    C'HDR222'                                                15020000
         MVI   MSGBUF,C' '        SET CARRIAGE CONTROL TO BLANK         15030000
         BAL   R2,PUTLINE3        PRINT 2ND LINE OF ASTERISKS           15040000
         DC    C'HDR333'                                                15050000
         MVC   MSGBUF+45(44),=C' DATASET SEQUENCE NUMBER 0000  (LABEL=0+15060000
               000) '                                                   15070000
         MVC   MSGBUF+70(4),RECBUF+31  MOVE IN DATASET SEQUENCE NUMBER  15080000
*        MVC   MSGBUF+83(4),RECBUF+31  AND MOVE IT IN AGAIN       -CWB- 15090000
VTOCBYP1 DS    0H                                              HD JAN89 15100000
         L     R5,CURRVTOC        POINT TO CURRENT VTOC BLOCK.    -CWB- 15110000
         CLI   0(R5),VTOCEPB      IS THIS BLOCK FULL?             -CWB- 15120000
         BL    NXTENTRY           IF NOT, BRANCH.                 -CWB- 15130000
         GETMAIN  R,LV=VTOCBLSZ   ELSE, GET CORE FOR ANOTHER BLOCK-CWB- 15140000
         ST    R1,0(R5)           SAVE FOREWARD POINTER.          -CWB- 15150000
         MVI   0(R5),VTOCEPB      REINSERT THE ENTRY COUNT.       -CWB- 15160000
         LR    R5,R1              MAKE NEW BLOCK CURRENT.         -CWB- 15170000
         ST    R1,CURRVTOC                                        -CWB- 15180000
         LA    R3,8(R5)           STEP OVER INITIAL DOUBLEWORD.   -CWB- 15190000
*                                      R3 POINTS AT CURRENT ENTRY.-CWB- 15200000
         SR    R0,R0              CLEAR ENTRY COUNT AND FORWARD   -CWB- 15210000
         ST    R0,0(R5)                POINTER IN NEW BLOCK.      -CWB- 15220000
         MVI   0(R5),1            CHANGE ENTRY COUNT TO 1.        -CWB- 15230000
         B     CLEARVEN           GO CLEAR THE FIRST ENTRY.       -CWB- 15240000
NXTENTRY LA    R3,VTOCSIZE(R3)    ADVANCE TO NEXT VTOC ENTRY.     -CWB- 15250000
         SR    R1,R1              INCREMENT ENTRY COUNT.          -CWB- 15260000
         IC    R1,0(R5)                                           -CWB- 15270000
         LA    R1,1(R1)                                           -CWB- 15280000
         STC   R1,0(R5)                                           -CWB- 15290000
         USING VTOC,R3            R3 WILL ALWAYS POINT TO ENTRY.  -CWB- 15300000
CLEARVEN MVC   VTOC(VTOCSIZE),BLANKBUF  CLEAR OUT VTOC ENTRY.     -CWB- 15310000
         L     R1,TRUESEQN        ADVANCE THE TRUE DATA SET       -CWB- 15320000
         LA    R1,1(R1)                SEQUENCE NUMBER COUNT.     -CWB- 15330000
         ST    R1,TRUESEQN                                        -CWB- 15340000
         L     R1,TRUESEQN        USE TRUE SEQUENCE NUMBER (AS    -CWB- 15350000
         CVD   R1,DBLWORK              OPPOSED TO WHAT THE LABEL  -CWB- 15360000
         OI    DBLWORK+7,X'0F'         SAYS) IN THE LABEL=XXXX    -CWB- 15370000
         UNPK  MSGBUF+83(4),DBLWORK    PART OF THE MESSAGE.       -CWB- 15380000
         MVC   VTOCSEQN,MSGBUF+83 ALSO USE IT IN VTOC.            -CWB- 15390000
         CLI   VTOCFLAG,C'Y'           VTOC ONLY?              HD JAN89 15400000
         BE    VTOCBYP2                YES, FILL IN LABEL REC  HD JAN89 15410000
         BAL   R2,PUTLINE3        PRINT MSG BETWEEN TWO LINES OF STARS  15420000
         DC    C'HDR444'                                                15430000
         MVC   MSGBUF+44(47),MSGBUF+43 REFILL WITH ASTERISKS            15440000
         BAL   R2,PUTLINE3                                              15450000
         DC    C'HDR555'                                                15460000
         BAL   R2,PUTLINE3                                              15470000
         DC    C'HDR666'                                                15480000
         MVC   MSGBUF+40(56),BLANKBUF CLEAR ASTERISKS TO BLANKS         15490000
         MVC   MSGBUF+1(80),RECBUF                                      15500000
         MVC   MSGBUF+82(35),=C'1ST HEADER LABEL RECORD,  FILE NO. '    15510000
VTOCBYP2 DS    0H                                              HD JAN89 15520000
         MVC   MSGBUF+116(4),RECBUF+31 MOVE IN DATASET SEQUENCE #       15530000
         MVC   DATASEQ,RECBUF+31  SAVE DATASET SEQUENCE #               15540000
         MVI   MSGBUF,C'-'        CCTRL FOR 2 BLANK LINES, THEN PRINT   15550000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 15560000
         BE    VTOCBYP3           YES, BYPASS PRINT            HD JAN89 15570000
         BAL   R2,PUTLINE3                                              15580000
         DC    C'HDR111'                                                15590000
         BAL   R5,HEXON           PRINT HEX IF OPTION IS YES            15600000
VTOCBYP3 DS    0H                                              HD JAN89 15610000
         CLC   RECBUF+54(6),=C'000000' SEE IF BLK CNT = 0               15620000
         BNE   DOBLKCNT                                                 15630000
         MVC   MSGBUF,BLANKBUF                                          15640000
         MVI   SWT2,X'01'               MOVE IN HEADR CODE        -HMD- 15650000
LBL1     DS    0H                                                       15660000
         MVC   MSGBUF+6(7),=C'DSNAME='                                  15670000
         MVC   MSGBUF+13(17),RECBUF+4                                   15680000
         LTR   R3,R3                  DO WE HAVE A VTOC ENTRY?    -CWB- 15690000
         BZ    *+10                   IF NOT, SKIP DSN SAVE.      -CWB- 15700000
         MVC   VTOCDSN,RECBUF+4   SAVE DSN FOR VTOC.              -CWB- 15710000
         MVC   MSGBUF+32(14),=C'CREATION DATE=' (MOVED OVER)      -CWB- 15720000
CHKDATE  DS    0H                 CHECK FOR VALID DATA            -HMD- 15730000
         L     R6,=A(NUBTABL)     LOAD TABLE ADDRESS              -HMD- 15740000
         CLC   =C'00000',RECBUF+42 CHECK FOR ZERO CREATION DATE   -HMD- 15750000
         BE    BADCREDT           BAD CREATION DATE               -HMD- 15760000
         TRT   RECBUF+42(5),0(R6) CHECK FOR NUMERICS              -HMD- 15770000
         BZ    DATEOK1            DATE IS OK                      -HMD- 15780000
BADCREDT DS    0H                 CREATION DATE IS BAD            -HMD- 15790000
         L     R1,AMSGBP46        POINT TO MESSAGE BUFFER         -HMD- 15800000
         MVC   0(8,R1),=C'INVALID ' MOVE INVALID TO MSG           -HMD- 15810000
         OI    SWT2,C'0'          INDICATE BAD DATE               -HMD- 15820000
         B     CDATBAD            SKIP DATE CONVERSION         HD JAN89 15830000
DATEOK1  DS    0H                                                 -HMD- 15840000
         PACK  BADLNGTH+4(4),RECBUF+42(5)   CONVERT FOR DATE SUBROUTINE 15850000
         L     R2,BADLNGTH+4                                            15860000
         LA    R1,AMSGBP46        PTR TO PTR TO DATE SUBR'S RSULT -CWB- 15870000
         L     R15,=V(DATE)       IRSS DATECONV SUBROUTINE              15880000
         BALR  R14,R15            DATE SUBROUTINE                       15890000
CDATBAD  DS    0H                                              HD JAN89 15900000
         LTR   R3,R3              DO WE HAVE A VTOC ENTRY?        -CWB- 15910000
         BZ    *+10               IF NOT, SKIP SAVE.              -CWB- 15920000
         MVC   VTOCCREA,MSGBUF+46 SAVE DATE FOR VTOC.             -CWB- 15930000
         CLC   RECBUF+48(5),=C'00000'  DOES TAPE HAVE EXP DATE?   -HMD- 15940000
         BE    EXPIRED            IF NOT, BRANCH.                 -CWB- 15950000
         MVC   MSGBUF+56(16),=C'EXPIRATION DATE='  ELSE, LIST IT. -CWB- 15960000
         TRT   RECBUF+48(5),0(R6)    SEE IF VALID EXPDT           -HMD- 15970000
         BZ    DATEOK2            YES, IS VALID                   -HMD- 15980000
BADEXPDT L     R1,AMSGBP72        GET POINTER TO MSG BUFFER       -HMD- 15990000
         MVC   0(8,R1),=C'INVALID ' MOVE INVALID TO MSG BUFFER    -HMD- 16000000
         OI    SWT2,C'0'          INDICATE ERROR                  -HMD- 16010000
         B     EDATBAD            FORGET DATE CONVERSION          -HMD  16020000
DATEOK2  PACK  BADLNGTH+4(4),RECBUF+48(5)                         -CWB- 16030000
         L     R2,BADLNGTH+4      DO CONVERSION AS BEFORE.        -CWB- 16040000
         LA    R1,AMSGBP72                                        -CWB- 16050000
         L     R15,=V(DATE)                                       -CWB- 16060000
         BALR  R14,R15                                            -CWB- 16070000
EDATBAD  DS    0H                                              HD JAN89 16080000
         LTR   R3,R3              DO WE HAVE A VTOC ENTRY?        -CWB- 16090000
         BZ    *+10               IF NOT, SKIP SAVE.              -CWB- 16100000
         MVC   VTOCEXPR,MSGBUF+72 SAVE EXP DATE FOR VTOC.         -CWB- 16110000
EXPIRED  EQU   *                                                  -CWB- 16120000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 16130000
         BE    VTOCBYP4           CONTINUE PROCESSING          HD JAN89 16140000
         MVI   MSGBUF,C'0'                                              16150000
         BAL   R2,PUTLINE3                                              16160000
         DC    CL6'LBL111'                                              16170000
         BAL   R5,PUTERROR        SEND WARNING MESSAGE            -HMD- 16180000
VTOCBYP4 B     LABELEND           END OF THIS LABEL           HD  JAN89 16190000
PUTERROR DS    0H                 ERROR MESSAGE ROUTINE           -HMD- 16200000
         CLI   SWT2,X'03'         SEE IF ERROR                    -HMD- 16210000
         BLR   R5                 BRANCH IF NO ERROR              -HMD- 16220000
         CLI   SWT2,C'1'          IS THIS A HDR ERROR?            -HMD- 16230000
         BNE   TRLRERR            NO, MUST BE A TRAILER ERROR     -HMD- 16240000
         MVC   ERR1MSG(4),=C'HDR1'                                -HMD- 16250000
         B     *+10                                               -HMD- 16260000
TRLRERR  MVC   ERR1MSG(4),=C'EOF1'                                -HMD- 16270000
         MVC   MSGBUF,BLANKBUF    CLEAR OUTPUT BUFFER             -HMD- 16280000
         BAL   R2,PUTLINE2                                        -HMD- 16290000
         MVC   MSGBUF(LMSG),ERR0MSG                               -HMD  16300000
         BR    R5                 RETURN TO CALLER                -HMD- 16310000
EOF1EOV1 DS    0H                                                       16320000
         CLC   DATASEQ,RECBUF+31  COMPARE OLD DATASET SEQ. NO. TO  THE +16330000
                                  CURRENT ONE.                          16340000
         BE    NEWSEQNO           SKIP ERROR MSG IF EQUAL               16350000
         CLI   DATASEQ,C'N'       SEE IF 'NONE' STILL IN DATASEQ        16360000
         BE    NEWSEQNO           SKIP ERR MSG IF NO PREVIOUS HDR1      16370000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 16380000
         BE    VTOCBYP5           YES, BYPASS THIS MESS        HD JAN89 16390000
         BAL   R2,PUTLINE2                                              16400000
         MVC   MSGBUF(100),=C'0ERROR - THE DATASET SEQ. NO. ON THE FOLL+16410000
               OWING LABEL DOES NOT MATCH THAT ON THE PRECEDING HDR1 LA+16420000
               BEL'                                                     16430000
         LA    R0,6               PREPARE TO RESERVE 6 LINES            16440000
         BAL   R2,PAGECHK         RESERVE 6 LINES OF PRINTOUT           16450000
VTOCBYP5 DS    0H                                              HD JAN89 16460000
         MVC   MSGBUF+1(132),RECBUF RESTORE MSGBUF                      16470000
NEWSEQNO MVC   DATASEQ,RECBUF+31  GET NEW DATASET SEQUENCE NUMBER       16480000
         MVC   MSGBUF+82(35),=C'1ST TRAILER LABEL RECORD, FILE NO. '    16490000
         MVC   MSGBUF+116(4),RECBUF+31                                  16500000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 16510000
         BE    DOBLKCNT           YES, BYPASS PRINT            HD JAN89 16520000
         BAL   R2,PUTLINE3                                              16530000
         DC    C'EOF1EV'                                                16540000
         BAL   R5,HEXON                                                 16550000
DOBLKCNT MVC   MSGBUF,BLANKBUF                                          16560000
         MVI   SWT2,X'02'         INDICATE IN TRAILER ROUTINE     -HMD- 16570000
         MVC   MSGBUF+82(12),=C'BLOCK COUNT=' (MOVED OVER.)       -CWB- 16580000
         MVC   MSGBUF+94(6),RECBUF+54                             -CWB- 16590000
         LTR   R3,R3              DO WE HAVE A VTOC ENTRY?        -CWB- 16600000
         BZ    LBL1               IF NOT, SKIP SAVE.              -CWB- 16610000
         CLI   VTOCOUNT,C' '      HAS THE TRUE BLOCK COUNT BEEN   -CWB- 16620000
         BNE   LBL1                    FILLED IN?  IF SO, BRANCH. -CWB- 16630000
         MVC   VTOCOUNT,RECBUF+54 ELSE, FILL IT IN FROM THE LABEL.-CWB- 16640000
         B     LBL1                                                     16650000
EOF2EOV2 DS    0H                                                       16660000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 16670000
         BE    VTOCBYP6           YES, BYPASS THIS MESS        HD JAN89 16680000
         LA    R0,6               PREPARE TO RESERVE 6 LINES OF PRINT   16690000
         BAL   R2,PAGECHK         RESERVE 6 LINES OF PRINT              16700000
         MVC   MSGBUF+82(35),=C'2ND TRAILER LABEL RECORD, FILE NO. '    16710000
         B     LBL2                                                     16720000
HDR2     DS    0H                                                       16730000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 16740000
         BE    VTOCBYP6           YES, BYPASS THIS MESS        HD JAN89 16750000
         LA    R0,8               PREPARE TO RESERVE 8 LINES  FOR PRINT 16760000
         BAL   R2,PAGECHK         RESERVE PRINT                         16770000
         MVC   MSGBUF+82(35),=C'2ND HEADER LABEL RECORD,  FILE NO. '    16780000
LBL2     DS    0H                                                       16790000
         MVC   MSGBUF+116(4),DATASEQ                                    16800000
         BAL   R2,PUTLINE3                                              16810000
         DC    C'HDR222'                                                16820000
         BAL   R5,HEXON                                                 16830000
VTOCBYP6 DS    0H                                              HD JAN89 16840000
         MVC   BLKSIZE(5),RECBUF+5                                      16850000
         MVC   LRECL(5),RECBUF+10                                       16860000
         MVC   RECFM(1),RECBUF+4  MOVE F, U, OR V PART OF RECFM DESC.   16870000
         SR    R1,R1                                                    16880000
         CLI   RECBUF+38,C'R'     SEE IF BLOCKING ATTRIBUTE IS 'BS'     16890000
         BNE   *+18               BIF NOT BS (BS AS IN 'VBS')           16900000
         LA    R1,2                                                     16910000
         MVC   RECFM+1(2),=C'BS'                                        16920000
         B     CTRLCHAR                                                 16930000
         CLI   RECBUF+38,C' '     SEE IF BLOCKING ATTRIBUTE IS UNBLKED  16940000
         BE    CTRLCHAR           BIF UNBLOCKED                         16950000
         MVC   RECFM+1(1),RECBUF+38   MOVE B OR S BLOCK ATTRIBUTE CHAR  16960000
         LA    R1,1               BUMP PTR PAST THE B OR S              16970000
CTRLCHAR LA    R1,RECFM+1(R1)                                           16980000
         MVC   0(1,R1),RECBUF+36                                        16990000
         MVC   TRTCH(2),RECBUF+34                                       17000000
         CLC   TRTCH(2),LBL2MSG+1 SEE IF IT'S 2 BLANKS                  17010000
         BNE   *+10                                                     17020000
         MVC   TRTCH(8),=C'STANDARD' 9-TRK                              17030000
         CLI   VTOCFLAG,C'Y'      CHECK FOR VTOC ONLY          HD JAN89 17040000
         BE    VTOCBYP7           BYPASS PRINT                 HD JAN89 17050000
         BAL   R2,PUTLINE                                               17060000
VTOCBYP7 DS    0H                                              HD JAN89 17070000
         MVC   MSGBUF(109),LBL2MSG                                      17080000
         CLI   RECBUF,C'H'        SEE IF IS 'HDR2'                      17090000
         BNE   LABELEND           BIF IT WAS EOF2 OR EOV2      HD JAN89 17100000
         LTR   R3,R3              DO WE HAVE A VTOC ENTRY?        -CWB- 17110000
         BZ    NOVSTUFF           IF NOT, SKIP SAVES.             -CWB- 17120000
         MVC   VTOCRECF,RECFM     COPY DATA FOR VTOC.             -CWB- 17130000
         MVC   VTOCLREC,LRECL                                     -CWB- 17140000
         MVC   VTOCBLKS,BLKSIZE                                   -CWB- 17150000
         MVC   VTOCDEN,PRTDENS                                    *SRH* 17160000
         MVC   VTOCTRTC,RECBUF+34                                 -CWB- 17170000
         MVC   VTOCJOBN,RECBUF+17                                 -CWB- 17180000
         MVC   VTOCSTEP,RECBUF+26                                 -CWB- 17190000
NOVSTUFF EQU   *                                                  -CWB- 17200000
         MVC   MSGBUF+2(120),MSGBUF+1    CLEAR MOST OF MSGBUF           17210000
         MVC   CRMSG+15(8),RECBUF+17                                    17220000
         MVC   CRMSG+32(8),RECBUF+26                                    17230000
         CLI   VTOCFLAG,C'Y'                                   HD JAN89 17240000
         BE    VTOCBYP8                                        HD JAN89 17250000
         BAL   R2,PUTLINE2                                              17260000
         MVC   MSGBUF+6(L'CRMSG),CRMSG                                  17270000
VTOCBYP8 B     LABELEND                                        HD JAN89 17280000
         EJECT                                                 HD JAN89 17290000
************************************************************** HD JAN89 17300000
**                                                             HD JAN89 17310000
**        ROUTINE TO HANDLE VOLSER FILE AND PRINT VOLSER       HD JAN89 17320000
**                                                             HD JAN89 17330000
************************************************************** HD JAN89 17340000
VOL1     DS    0H                                                       17350000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 17360000
         BE    VTOCBYP9           YES, BYPASS THIS MESS        HD JAN89 17370000
         LA    R0,6               PREPARE TO RESERVE 6 LINES OF PRINT   17380000
         BAL   R2,PAGECHK         AND RESERVE THEM                      17390000
         MVC   MSGBUF+82(19),=C'VOLUME LABEL RECORD'                    17400000
         MVI   MSGBUF,C'0'                                              17410000
         BAL   R2,PUTLINE3                                              17420000
         DC    C'VOL111'                                                17430000
         BAL   R5,HEXON                                                 17440000
VTOCBYP9 DS    0H                                              HD JAN89 17450000
         MVC   MSGBUF,BLANKBUF                                          17460000
         MVC   MSGBUF+6(21),=C'VOLUME SERIAL NUMBER='                   17470000
         MVC   MSGBUF+27(6),RECBUF+4                                    17480000
         MVC   MSGBUF+40(19),=C'OWNER INFORMATION='''                   17490000
         MVC   MSGBUF+59(10),RECBUF+41                                  17500000
         MVI   MSGBUF+69,C''''                                          17510000
         MVC   VSNSAVE,RECBUF+4   SAVE VSN AND OWNER FOR USE      -CWB- 17520000
         MVC   OWNERSAV,RECBUF+41      IN VTOC LISTING.           -CWB- 17530000
         MVI   MSGBUF,C'0'                                              17540000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 17550000
         BE    VTOCBYPA           YES, BYPASS PRINT            HD JAN89 17560000
         BAL   R2,PUTLINE3                                              17570000
         DC    C'VOL222'                                                17580000
         CLC   (JFCBINX+JFCBVOLS-JFCB)(6),RECBUF+4 SEE IF VOLUME MATCH  17590000
         BE    LABELEND           BIF SAME                     HD JAN89 17600000
         MVC   NOMATCH+46(6),RECBUF+4               VOL ON VOL1         17610000
         MVC   NOMATCH+84(6),JFCBINX+JFCBVOLS-JFCB  VOL ON INPUT DD     17620000
         BAL   R2,PUTLINE                                               17630000
         MVC   MSGBUF(90),NOMATCH                                       17640000
VTOCBYPA DS    0H                                              HD JAN89 17650000
         TM    WRTFLG,X'01'       ARE WE MAKING A COPY?        -CWB-    17660000
         BZ    LABELEND           IF NOT, CONTINUE PROCESSING. -CWB-    17670000
         L     R0,CTPMKNO         GET CURRENT TAPEMARK NUMBER           17680000
         LTR   R0,R0              SEE IF IT'S ZERO (NO TAPEMARKS YET)   17690000
         BNZ   LABELEND           BIF A TAPEMARK HAS ALREADY BEEN READ  17700000
         TM    JFCLTSV,X'02'      SEE IF 'SL' (OR SUL) BIT IS ON        17710000
         BZ    LABELEND           IF IT'S NOT THEN 'NO VERIFY' (BLP OR +17720000
                                  AL) WAS SPECIFIED IN JCL; SO KEEP ON. 17730000
         BAL   R2,PUTLINE         PRINT OPERATOR ERROR TERMINATION MSG  17740000
         MVC   MSGBUF(38),=C'0TERMINATION DUE TO WRONG TAPE MOUNTED'    17750000
         B     EXITRC8                                                  17760000
LABELEND B     PROCESS2                                        HD JAN89 17770000
         EJECT                                                 HD JAN89 17780000
************************************************************** HD JAN89 17790000
**                                                             HD JAN89 17800000
**               EOD PROCESSING                                HD JAN89 17810000
**                                                             HD JAN89 17820000
************************************************************** HD JAN89 17830000
EODS     DS    0H                                                       17840000
*        AP    MARKNO,=P'1'       COUNT THE TAPEMARK              -CWB- 17850000
*        UNPK  MARKNUM,MARKNO                                     -CWB- 17860000
         L     R1,MARKNO          COUNT THE TAPEMARK (COUNT       -CWB- 17870000
         LA    R1,1(R1)                STORED IN BINARY FORM).    -CWB- 17880000
         ST    R1,MARKNO                                          -CWB- 17890000
         CVD   R1,DBLWORK         MAKE IT PRINTABLE.              -CWB- 17900000
         UNPK  MARKNUM,DBLWORK                                    -CWB- 17910000
         OI    MARKNUM+3,X'F0'                                          17920000
         LA    R1,1                                                     17930000
         A     R1,CTPMKNO         CURRENT NUMBER OF TAPEMARKS PASSED    17940000
         ST    R1,CTPMKNO         IS NOW UPDATED TO ACTUAL VALUE        17950000
         L     R1,BLKCNT          BLKS IN PREVIOUS DATASETS             17960000
         AR    R1,R4              ADD NO. OF BLKS IN DATASET JUST READ  17970000
         ST    R1,BLKCNT          TO GET TOTAL BLKS READ SO FAR (EXCEPT+17980000
                                  FOR SKIPPING - SKIPEOV OR SKIPTM).    17990000
         CLI   WRTFLG,X'03'       SEE IF COPY BEING DONE NOW            18000000
         BNE   *+24               SKIP WRITING TAPEMARK IF NOT          18010000
         LA    R1,=AL3(WTMCMND)   PTR TO PTR TO WTM CCW FOR OUTPUT TAPE 18020000
         LA    R2,OUTPUT          DCB FOR WTM EXCP OUTPUT               18030000
         ST    R4,SVR4            SAVE R4 FOR EXEC ESCP CALL            18040000
         BAL   R4,EXECEXCP        CAL SUBROUTINE TO ISSUE EXCP          18050000
         L     R4,SVR4            RESTORE R4                            18060000
         CLI   COUNTFLG,C'N'                                            18070000
         BE    SIMPLETM           BIF COUNT OPTION NOT SPECIFIED        18080000
         L     R0,MAX                                                   18090000
         LTR   R0,R0                                                    18100000
         BZ    SIMPLETM           BIF NO COUNTING WORK HAS BEEN DONE    18110000
         LTR   R4,R4              BYPASS MAX/MIN/AVG IF           -CWB- 18120000
         BZ    SIMPLETM                NO RECORDS.                -CWB- 18130000
         L     R1,MIN                                                   18140000
         CVD   R1,BADLNGTH                                              18150000
         OI    BADLNGTH+7,X'0F'                                         18160000
         UNPK  MINMSG,BADLNGTH                                          18170000
         L     R1,MAX                                                   18180000
         CVD   R1,BADLNGTH                                              18190000
         OI    BADLNGTH+7,X'0F'                                         18200000
         UNPK  MAXMSG,BADLNGTH                                          18210000
         LR    R1,R4              COPY COUNT OF RECORDS AND DIVIDE-CWB- 18220000
         SRA   R1,1                    BY 2 TO ROUND THE AVERAGE. -CWB- 18230000
         A     R1,FILEBYTS        ADD NUMBER OF BYTES IN FILE.    -CWB- 18240000
         SR    R0,R0              CLEAR R0 FOR DIVIDE.            -CWB- 18250000
         ST    R0,FILEBYTS        ALSO CLEAR FILEBYTS FOR NEXT    -CWB- 18260000
*                                      FILE.                      -CWB- 18270000
         DR    R0,R4              DIVIDE FOR AVERAGE BLOCK SIZE.  -CWB- 18280000
         CVD   R1,BADLNGTH        MAKE AVERAGE SIZE PRINTABLE.    -CWB- 18290000
         OI    BADLNGTH+7,X'0F'                                   -CWB- 18300000
         UNPK  AVGMSG,BADLNGTH                                    -CWB- 18310000
         CVD   R4,BADLNGTH                                              18320000
         OI    BADLNGTH+7,X'0F'                                         18330000
         UNPK  COUNTMSG,BADLNGTH                                        18340000
         LTR   R3,R3              DO WE HAVE A VTOC ENTRY?        -CWB- 18350000
         BZ    NOSAVE             IF NOT, BRANCH.                 -CWB- 18360000
         CLI   LABLFLAG,C'Y'      IS THIS A LABEL FILE?           -CWB- 18370000
         BE    NOSAVE             IF SO, DON'T SAVE STAT'S.       -CWB- 18380000
         MVC   VTOCMINB,MINMSG    SAVE MIN, MAX, AVG, AND COUNT   -CWB- 18390000
         MVC   VTOCMAXB,MAXMSG         FOR VTOC LISTING.          -CWB- 18400000
         MVC   VTOCAVGB,AVGMSG                                    -CWB- 18410000
         MVC   VTOCOUNT,COUNTMSG                                  -CWB- 18420000
NOSAVE   EQU   *                                                  -CWB- 18430000
*        MVI   EOVM+1,120         SET LENGTH FOR POSSIBLE LONG EOV-CWB- 18440000
         MVI   EOVM+1,CNTSEND-TPMKMSG-1  SET LENGTH FOR MVC.      -CWB- 18450000
*        MVI   MSGBUF+100,C' '    PREPARE TO CLEAR PART OF MSG BUF-CWB- 18460000
*        MVC   MSGBUF+101(32),MSGBUF+100 CLEAR LAST PART OF  BUFFE-CWB- 18470000
         MVC   MSGBUF,BLANKBUF    CLEAR BUFFER COMPLETELY.        -CWB- 18480000
*        MVC   MSGBUF+18(87),EOVNUM+3                             -CWB- 18490000
         MVC   MSGBUF+18(CNTSEND-EOVNUM-3),EOVNUM+3 COPY MESG.    -CWB- 18500000
         LA    R5,PUTLINE2        SPECIFY LONG TYPE OF TAPEMARK MSG     18510000
         B     TMSGDONE                                                 18520000
SIMPLETM MVI   EOVM+1,33          SET LENGTH OF POSSIBLE EOV MSG        18530000
         LA    R5,PUTLINE         SPECIFY SHORT TPMK FOUND MSG IF ANY   18540000
*************************************************************  HD AUG86 18550000
** LABEL TMSGDONE REPLACED BELOW                           **  HD AUG86 18560000
*************************************************************  HD AUG86 18570000
*TMSGDONE  CLI   MARK,X'FF'                                             18580000
*          BE    EOVPROC                                                18590000
*          MVI   MARK,X'FF'                                             18600000
*************************************************************  HD AUG86 18610000
** END OF REPLACED CODE SEGMENT                            **  HD AUG86 18620000
*************************************************************  HD AUG86 18630000
TMSGDONE DS    0H                                                 -CWB- 18640000
         CLI   EOV1FLG,C'Y'                                             18650000
         BE    EOVPROC            BIF 'EOV PENDING' FLAG SET            18660000
         CLI   MARK,X'FF'         TWO CONSECUTIVE MARKS?          -CWB- 18670000
         BNE   SKPCHK             IF NOT, BRANCH.                 -CWB- 18680000
         CLI   PREVHDR1,C'N'      PREV FILE A HDR1 LABEL?         -CWB- 18690000
         BE    EOVPROC            IF NOT, GO DO EOV.              -CWB- 18700000
SKPCHK   MVI   MARK,X'FF'         NOTE THIS MARK FOUND.           -CWB- 18710000
         CLC   SKPEOVNO,CEOVNO    SKIP SKIPTM PROCESSSING IF            18720000
         BH    SKIPEOVP           SKIPEOV PROCESSING IS BEING DONE      18730000
         CLC   CTPMKNO,SKIPTMNO   COMPARE CTPMKNO WITH SKIPTMNO         18740000
         BL    SKIPTMPR           BRANCH TO DO POSSIBLE SKIPTM PROCESS  18750000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 18760000
         BE    VTOCBYPB           BYPASS PRINT IF YES.         HD JAN89 18770000
         BALR  R2,R5              TO PUTLINE OR PUTLINE2                18780000
VTOCBYPB DS    0H                                              HD JAN89 18790000
         MVC   MSGBUF(18),TPMKMSG                                       18800000
         OI    WRTFLG,X'02'       SET 'SKIPPING DONE' BIT FOR COPY      18810000
         CLC   CTPMKNO,MAXTMNO                                          18820000
         BL    PROCESS                                                  18830000
         CLI   WRTFLG,X'03'       CHECK IF COPY BEING DONE              18840000
         BNE   *+16               SKIP IF IT'S NOT BEING DONE           18850000
         LA    R1,=AL3(WTMCMND)   OTHERWISE WRITE AN EXTRA TAPEMARK,    18860000
         LA    R2,OUTPUT          JUST TO MAKE SURE.  THIS COULD        18870000
         BAL   R4,EXECEXCP        RESULT  IN 3 TAPEMARKS IN LAST EOV.   18880000
         B     PRENDMSG                                                 18890000
EOVPROC  DS    0H                 HANDLES END-OF-VOLUME INDICATIONS     18900000
         MVI   EOV1FLG,C'N'       CLEAR 'EOV PENDING' FLAG              18910000
*        AP    EOVNO,=P'1'        COUNT EOV'S                     -CWB- 18920000
*        UNPK  EOVNUM,EOVNO                                       -CWB- 18930000
         L     R1,EOVNO           COUNT EOV'S (COUNT STORED       -CWB- 18940000
         LA    R1,1(R1)                IN BINARY FORM).           -CWB- 18950000
         ST    R1,EOVNO                                           -CWB- 18960000
         CVD   R1,DBLWORK         MAKE IT PRINTABLE.              -CWB- 18970000
         UNPK  EOVNUM,DBLWORK                                     -CWB- 18980000
         OI    EOVNUM+2,X'F0'                                           18990000
*        MVC   WRITELOG+11(3),EOVNUM                              -CWB- 19000000
*WRITELOG WTL  'EOV000 ENCOUNTERED'                               -CWB- 19010000
         CLI   VTOCFLAG,C'Y'      VTOC ONLY?                   HD JAN89 19020000
         BE    VTOCBYPC           YES..BYPASS PRINT            HD JAN89 19030000
         BAL   R2,PUTLINE                                               19040000
VTOCBYPC DS    0H                                              HD JAN89 19050000
EOVM     MVC   MSGBUF(00),TPMKMSG LENGTH SPECIFIED IN EODS EXIT         19060000
         LA    R1,1                                                     19070000
         A     R1,CEOVNO                                                19080000
         ST    R1,CEOVNO                                                19090000
         C     R1,SKPEOVNO                                              19100000
         BL    SKIPEOVP           BIF SKIPEOV BEING DONE                19110000
         BNE   NOTJUST            BIF SKIPEOV PROCESSING NOT JUST DONE  19120000
         CLC   CTPMKNO,SKIPTMNO   SEE IF SKIPTM PROCESSING COMPLETED    19130000
         BL    NOTJUST            BIF NOT, SKIPTMNO SHOULD BECOME      +19140000
                                  ACTUAL NUMBER OF TAPEMARKS SKIPPED.   19150000
         MVC   SKIPTMNO,CTPMKNO   MORE THAN SKIPTMNO WERE SKIPPED DUE  +19160000
                                  SKIPEOV; RESET SKIPTMNO TO INDICATE  +19170000
                                  ACTUAL NUMBER SKIPPED (FOR LNGTH EST) 19180000
NOTJUST  DS    0H                                                       19190000
         OI    WRTFLG,X'02'       SET 'SKIPPING DONE' BIT FOR COPY      19200000
         CLC   CEOVNO,MAXEOVNO                                          19210000
         BL    PROCESS                                                  19220000
         SPACE 2                                               HD JAN89 19230000
PRENDMSG DS    0H                                              HD JAN89 19240000
         CLI   COUNTFLG,C'N'                                            19250000
         BE    SHORT              SKIP TO NOT PRINT COUNTS              19260000
         LA    R0,6               PREPARE TO RESERVE 6 LINES FOR PRINT  19270000
         BAL   R2,PAGECHK         RESERVE THEM                          19280000
         LA    R1,=AL3(SENSCMND)  RE-SENSE AT END OF PROCESSING         19290000
         LA    R2,INPUT           PTR TO DCB FOR EXCP CALL              19300000
         BAL   R4,EXECEXCP                                              19310000
         CLI   FLAG3480,C'Y'      IS THIS A 3480 DEVICE        HD JAN89 19320000
         BE    ADDDEN3            YES, CONTINUE WITH PROCESS   HD JAN89 19330000
         MVI   INDEX+3,8          SINCE 9-TRK, LOAD OFFSET     HD JAN89 19340000
         TM    SENSBYTS+3,X'04'   CHECK IF PE, 1 = PE = 1600 BPI        19350000
         BNO   CK6250             CK FOR 6250 BPI 9TRK                  19360000
         MVI   (DCBDEN-IHADCB)+INPUT,X'C3' SET DEN=1600 BPI INDICATOR   19370000
         MVI   LNGTHEST+50,C'3'   SET DENSITY = 3              HD JAN89 19380000
         B     ADDDEN3            CONTINUE WITH LENGTH CHECK   HD JAN89 19390000
         SPACE 1                                                        19400000
EIGHTBPI DS    0H                                                       19410000
         MVI   INDEX+3,0          SET OFFSET IN BPI TABLE      HD JAN89 19420000
         MVI   INPUT+(DCBDEN-IHADCB),X'83' SET DEN=800 BPI INDICATOR    19430000
         MVI   LNGTHEST+50,C'2'   SET DENSITY = 2              HD JAN89 19440000
         B     ADDDEN3            CONTINUE WITH LENGTH CHECK   HD JAN89 19450000
         SPACE 1                                                        19460000
CK6250   L     R2,(DCBDEBAD-IHADCB)+INPUT GET DEB ADDR                  19470000
         L     R2,DEBUCBAD(R2)    GET UCB ADDR                          19480000
         TM    16(R2),2           CK UCBTYP FOR 6250 BPI                19490000
         BNO   EIGHTBPI           NOPE, ASSUME 800 BPI         HD JAN89 19500000
         MVI   (DCBDEN-IHADCB)+INPUT,X'D3' SET DENS=4 FOR 6250 BPI      19510000
         MVI   LNGTHEST+50,C'4'   SAY DENS=4                            19520000
         MVI   INDEX+3,12         SET INDEX FOR 6250 BPI       HD JAN89 19530000
         B     ADDDEN3            BYPASS SOME CODE                      19540000
         SPACE 1                                                        19550000
*                                                                       19560000
*         FORMULA FOR LENGTH IN INCHES FOLLOWS:                         19570000
*         INCHES=R8/BPI+(IBG*BLKCNT+CTPMKNO*TMLENGTH)/1000              19580000
         SPACE 1                                                        19590000
ADDDEN3  L     R15,CTPMKNO        LOAD NUMBER OF TAPEMARKS READ         19600000
         S     R15,SKIPTMNO       SUBTRACT NUMBER OF TAPEMARKS SKIPPED  19610000
         MH    R15,TMLENGTH       MULTIPLY BY (TAPEMARK LENGTH*1000)    19620000
         CLI   FLAG3480,C'N'      3480?                        HD JAN89 19630000
         BE    ADDEN3A            YES, BYPASS THIS             HD JAN89 19640000
         MVI   LNGTHEST+50,C'5'   INDICATE DENSITY=5           HD JAN89 19650000
         L     R1,BLKCNT          LOAD BLOCK COUNT             HD JAN89 19660000
         MH    R1,GAP3480         MULT TO GET GAP LEN*1000     HD JAN89 19670000
         AR    R1,R15             GET TOTAL GAP + TM LGTH*1000 HD JAN89 19680000
         SR    R0,R0              CLEAR FOR DIVIDE             HD JAN89 19690000
         D     R0,=F'1000'        GET TOTAL TAPEMARK+GAP IN.   HD JAN89 19700000
         LR    R15,R1             SAVE FOR LATER               HD JAN89 19710000
         L     R2,BPI3480         LOAD DENSITY                 HD JAN89 19720000
         B     ADDEN3B            CONTINUE WITH CALCULATION    HD JAN89 19730000
ADDEN3A  DS    0H                                              HD JAN89 19740000
         L     R2,INDEX           LOAD INDEX INTO BPI/IBG TABLE         19750000
         L     R1,BLKCNT          LOAD BLOCK COUNT                      19760000
         MH    R1,BPIBGTBL(R2)    MULTIPLY TO GET TOTAL GAP LENGTH*1000 19770000
*                                                              HD JAN89 19780000
         AR    R1,R15             GET TOTAL GAP + TAPEMARK LENGTH*1000  19790000
         SR    R0,R0                                                    19800000
         D     R0,=F'1000'        GET TOTAL TAPEMARK+ GAP LENGTH INCHES 19810000
         LR    R15,R1             SAVE THIS FOR LATER                   19820000
         LH    R2,BPIBGTBL+2(R2)  LOAD PHYSICAL BPI (NOT LOGICAL BPI)   19830000
*                                                              HD JAN89 19840000
ADDEN3B  DS    0H                                              HD JAN89 19850000
         LR    R1,R8              LOAD TOTAL BYTE COUNT                 19860000
         SR    R0,R0                                                    19870000
         DR    R0,R2              DIVIDE BYTE COUNT BY PHYSICAL BPI     19880000
         AR    R1,R15             GET TOTAL LENGTH IN INCHES            19890000
         SR    R0,R0                                                    19900000
         D     R0,=F'12'          GET FEET IN R1, INCHES IN R0          19910000
         CVD   R1,BADLNGTH                                              19920000
         OI    BADLNGTH+7,X'0F'                                         19930000
         UNPK  LNGTHEST+17(4),BADLNGTH                                  19940000
         CVD   R0,BADLNGTH                                              19950000
         OI    BADLNGTH+7,X'0F'                                         19960000
         UNPK  LNGTHEST+27(2),BADLNGTH                                  19970000
         BAL   R2,PUTLINE                                               19980000
         MVC   MSGBUF(81),LNGTHEST                                      19990000
         BAL   R2,PUTLINE                                               20000000
         MVC   MSGBUF(106),LNGTHACC                                     20010000
         MVI   SHORTNOW+1,LNGTHEST-ENDMSG-1  SET LENGTH OF MESSAGE-HMD- 20020000
         CVD   R8,BADLNGTH        TOTAL BYTES READ (FOR COUNT OPT ONLY) 20030000
         OI    BADLNGTH+7,X'0F'   SET SIGN NIBBLE                       20040000
         UNPK  BYTES,BADLNGTH                                           20050000
         L     R8,BLKCNT          TOTAL BLKS READ ON TAPE, EXCLUDING   +20060000
                                  THOSE READ DURING SKIP PROC%SSING.    20070000
         CVD   R8,BADLNGTH                                              20080000
         OI    BADLNGTH+7,X'0F'                                         20090000
         UNPK  NBLKS,BADLNGTH                                           20100000
SHORT    BAL   R2,PUTLINE         PRINT 'SUCCESSFUL END' MSG            20110000
SHORTNOW MVC   MSGBUF(L'ENDMSG),ENDMSG (LENGTH MODIFIED FOR COUNTBLKS)  20120000
         L     R2,ERRCOUNT        COUNT OF SYNAD EXITS TAKEN            20130000
         LTR   R2,R2              SEE IF ZERO                           20140000
*        BZ    EXIT               EXIT IF SO                      -CWB- 20150000
         BZ    LISTVTOC           GO DO VTOC IF SO.               -CWB- 20160000
         CVD   R2,BADLNGTH                                              20170000
         OI    BADLNGTH+7,X'0F'                                         20180000
         UNPK  ERRSUMSG+22(5),BADLNGTH                                  20190000
         BAL   R2,PUTLINE                                               20200000
         MVC   MSGBUF(L'ERRSUMSG),ERRSUMSG                              20210000
*        B     EXIT                                               -CWB- 20220000
         SPACE 3                                                        20230000
LISTVTOC CLC   FRSTVTOC+1(3),=AL3(0)  DID WE MAKE A VTOC?         -CWB- 20240000
         BE    EXIT               IF NOT, SKIP IT.                -CWB- 20250000
         LA    R0,100             RESERVE 100 LINES (FORCE EJECT).-CWB- 20260000
         BAL   R2,PAGECHK                                         -CWB- 20270000
         BAL   R2,PUTLINE         OUTPUT VTOC HEADING.            -CWB- 20280000
         MVC   MSGBUF(VHEADLEN),VTOCHEAD                          -CWB- 20290000
         BAL   R2,PUTLINE         OUTPUT COLUMN HEADINGS.         -CWB- 20300000
         MVC   MSGBUF(L'VTOCHED2),VTOCHED2                        -CWB- 20310000
         L     R4,FRSTVTOC        GET ADDRESS OF FIRST VTOC BLOCK.-CWB- 20320000
         SR    R8,R8              CLEAR R8 FOR 1-BYTE COUNTS.     -CWB- 20330000
NEXTVBLK LA    R3,8(R4)           GET ADDRESS OF FIRST ENTRY IN   -CWB- 20340000
*                                      THE VTOC BLOCK.            -CWB- 20350000
         IC    R8,0(R4)           GET THE NUMBER OF ENTRIES IN    -CWB- 20360000
*                                      THIS VTOC BLOCK.           -CWB- 20370000
NEXTLINE MVC   MSGBUF,BLANKBUF    BUILD THE VTOC ENTRY LINE:      -CWB- 20380000
         MVC   MSGBUF+1(4),VTOCSEQN    DATA SET SEQUENCE NUMBER.  -CWB- 20390000
         MVC   MSGBUF+7(17),VTOCDSN    DATA SET NAME.             -CWB- 20400000
         MVC   MSGBUF+26(4),VTOCRECF   RECFM.                     -CWB- 20410000
         MVC   MSGBUF+32(5),VTOCLREC   LRECL.                     -CWB- 20420000
         MVC   MSGBUF+40(5),VTOCBLKS   BLKSIZE.                   -CWB- 20430000
         MVC   MSGBUF+47(4),VTOCDEN    DENSITY.                   -CWB- 20440000
         MVC   MSGBUF+53(2),VTOCTRTC   TRTCH.                     -CWB- 20450000
         CLC   VTOCTRTC,LBL2MSG+1    Q. TRTCH IS BLANKS.          -HMD- 20460000
         BNE   *+10                  A. NO, LEAVE AS IT IS        -HMD- 20470000
         MVC   MSGBUF+53(3),=C'STD'     INDICATE STD. TRTCH       -HMD- 20480000
         MVC   MSGBUF+59(5),VTOCMAXB   MAXIMUM BLOCK SIZE.        -CWB- 20490000
         MVC   MSGBUF+68(5),VTOCMINB   MINIMUM BLOCK SIZE.        -CWB- 20500000
         MVC   MSGBUF+77(5),VTOCAVGB   AVERAGE BLOCK SIZE.        -CWB- 20510000
         MVC   MSGBUF+86(6),VTOCOUNT   NUMBER OF BLOCKS.          -CWB- 20520000
         MVC   MSGBUF+95(8),VTOCCREA   CREATION DATE.             -CWB- 20530000
         MVC   MSGBUF+105(8),VTOCJOBN  JOB NAME.                  -CWB- 20540000
         MVC   MSGBUF+115(8),VTOCSTEP  STEP NAME.                 -CWB- 20550000
         MVC   MSGBUF+125(8),VTOCEXPR  EXPIRATION DATE.           -CWB- 20560000
         BAL   R2,PUTLINE3        OUTPUT THE LINE.                -CWB- 20570000
         DC    CL6'VTOC--'        REQUIRED DEAD SPACE.            -CWB- 20580000
         LA    R3,VTOCSIZE(R3)    ADVANCE TO NEXT ENTRY.          -CWB- 20590000
         BCT   R8,NEXTLINE        LOOP IF MORE IN THIS BLOCK.     -CWB- 20600000
         LR    R1,R4              POINT TO VTOC BLOCK.            -CWB- 20610000
         L     R4,0(R4)           GET ADDRESS OF NEXT BLOCK.      -CWB- 20620000
         LA    R0,VTOCBLSZ        GET VTOC BLOCK SIZE.            -CWB- 20630000
         FREEMAIN  R,LV=(0),A=(1) FREE UP VTOC BLOCK'S CORE.      -CWB- 20640000
         LA    R4,0(R4)           CLEAR HIGH-ORDER BYTE.          -CWB- 20650000
         LTR   R4,R4              IS FOREWARD POINTER ZERO?       -CWB- 20660000
         BZ    EXIT               IF SO, WE'RE DONE.              -CWB- 20670000
         B     NEXTVBLK           ELSE, GO DO NEXT BLOCK.         -CWB- 20680000
         EJECT                                                          20690000
CTPMKNO  DC    F'0'               # OF TAPEMARKS ALREADY ENCOUNTERED    20700000
CEOVNO   DC    F'0'               # OF DOUBLE TAPEMARKS PASSED          20710000
SVR4     DC    F'0'               FOR SAVING R4 TEMPORARILY             20720000
BLKCNT   DC    F'0'               KEEPS TRACK OF TOTAL BLKS READ ON TAPE20730000
                                  EXCLUDING THOSE READ DURING SKIPPING. 20740000
MAX      DC    F'0'               KEEPS TRACK OF MAX BLK LEN   HD DEC86 20750000
MIN      DC    F'65535'           KEEPS TRACK OF MIN BLK LEN   HD DEC86 20760000
TMLENGTH DC    H'3750'            DEFAULT TAPEMARK LENGTH*1000 (9-TRK)  20770000
INDEX    DC    F'0'               NOCONV=+4, +DEN*8            HD DEC86 20780000
         EJECT                                                          20790000
*                                                                       20800000
*  -.-.-.- DO NOT CHANGE THE ORDER OF THE FOLLOWING TABLE -.-.-.-.      20810000
*                                                                       20820000
*TBLORG EQU BPIBGTBL-20           THEORETICAL ORIGIN OF BPIBGTBL        20830000
*                                 WHICH IS LIKE A 3-D ARRAY:            20840000
*                                 NOCONV=+4, +DEN*8                     20850000
BPIBGTBL DC    H'601,800'    800 BPI NOCONV 9-TRK                       20860000
         DC    H'1,1'       1600 BPI CONV   9-TRK (NOT USED)            20870000
         DC    H'651,1600'  1600 BPI NOCONV 9-TRK                       20880000
         DC    H'300,6250'  6250 BPI NOCONV 9-TRK                 *SRH* 20890000
*  TABLE END                                                      *SRH* 20900000
GAP3480  DC    H'100'       3480 GAP                           HD JAN89 20910000
BPI3480  DC    F'38000'     3480 DENSITY                       HD JAN89 20920000
MARKNO   DC    F'0'               NUMBER OF TAPE MARKS READ.      -CWB- 20930000
EOVNO    DC    F'0'               NUMBER OF EOV'S PROCESSED.      -CWB- 20940000
TPMKMSG  DC    C'0TAPEMARK NO. '                                        20950000
MARKNUM  DC    C'    '                                                  20960000
         DC    C' -- EOV NO. '                                          20970000
EOVNUM   DC    CL3'000'                                                 20980000
         DC    C'    BLOCK LENGTHS:  MIN='                        -CWB- 20990000
MINMSG   DC    C'00000'                                                 21000000
         DC    C'  MAX='                                          -CWB- 21010000
MAXMSG   DC    C'00000'                                                 21020000
         DC    C'  AVG='                                          -CWB- 21030000
AVGMSG   DC    C'00000'                                           -CWB- 21040000
         DC    C'    NUMBER OF BLOCKS='                                 21050000
COUNTMSG DC    C'000000'                                                21060000
CNTSEND  EQU   *                                                  -CWB- 21070000
DATASEQ  DC    CL4'NONE'                                                21080000
ENDMSG   DC    C'0SUCCESSFUL PROCESSING OF THIS TAPE COMPLETED'         21090000
         DC    C':    TOTAL BYTES READ='                                21100000
BYTES    DC    C'XXXXXXXXXXX'    NUMBER OF BYTES READ             -HMD- 21110000
         DC    C'    NUMBER OF DATA BLOCKS READ='                       21120000
NBLKS    DC    C'XXXXXX'                                                21130000
LNGTHEST DC    C'0LENGTH ESTIMATE=XXXX FEET YY INCHES ASSUMING DEN=X AN+21140000
               D TRTCH=STANDARD           '                             21150000
LNGTHACC DC    C'0(LENGTH ESTIMATE USUALLY ACCURATE WITHIN PLUS OR MINU+21160000
               S TEN PERCENT;  ALMOST ALWAYS WITHIN TWENTY PERCENT)'    21170000
LNGT3480 DC    C'0LENGTH TEST BYPASSED FOR 3480 CARTRIDGE DEVICE' -HMD- 21180000
LNGT348A DC    C'0LENGTH MEANINGLESS FOR CARTRIDGE TAPE'       HD DEC86 21190000
LBL2MSG  DC    CL12'0     RECFM='                                       21200000
RECFM    DC    CL18'          BLKSIZE='                                 21210000
BLKSIZE  DC    CL18'XXXXX       LRECL='                                 21220000
LRECL    DC    CL20'XXXXX               '                               21230000
DENSITY  DC    CL6'TRTCH='                                              21240000
TRTCH    DC    CL35' '                                                  21250000
CRMSG    DC    C'CREATED BY JOB          IN STEP         '              21260000
MARK     DC    X'00'              'TAPEMARK JUST READ' FLAG (00 = NOT)  21270000
EOV1FLG  DC    C'N'               SET EOV1FLG =C'Y' WHENEVER 1ST BLK   +21280000
                                  AFTER A TAPEMARK IS 80 BYTES LONG &  +21290000
                                  STARTS WITH 'EOV1'.                   21300000
SWT2     DC    X'00'              SET FOR INVALID CREDT OR EXPDT  -HMD- 21310000
LABLFLAG DC    C'N'               INDICATES WHETHER WE ARE        -CWB- 21320000
*                                      PROCESSING A LABEL FILE.   -CWB- 21330000
PREVHDR1 DC    C'N'               C'Y' IF PREV FILE A HDR1 LABEL. -CWB- 21340000
HDR1FLAG DC    C'N'               C'Y' IF CURRENT FILE A HDR1.    -CWB- 21350000
FLAG3480 DC    C'N'               C'Y' IF TAPE IS A 3480       HD NOV86 21360000
DBLWORK  DC    D'0'               CVB/CVD WORK AREA.              -CWB- 21370000
CURRVTOC DC    A(FRSTVTOC)        CURRENT VTOC BLOCK.             -CWB- 21380000
FRSTVTOC DC    AL1(VTOCEPB),AL3(0)  ADDRESS OF FIRST VTOC BLOCK;  -CWB- 21390000
*                                      HIGH-ORDER BYTE SET TO     -CWB- 21400000
*                                      FORCE FIRST GETMAIN.       -CWB- 21410000
FILEBYTS DC    F'0'               NUMBER OF BYTES IN THIS FILE.   -CWB- 21420000
TRUESEQN DC    F'0'               LABEL= VALUE SEQUENCE NUMBER.   -CWB- 21430000
VTOCHEAD DC    C'-VOLUME TABLE OF CONTENTS FOR '                  -CWB- 21440000
VSNSAVE  DC    C'VSNVSN',C'    '                                  -CWB- 21450000
SAVETRK  DC    C'9 TRACK    '                                     -CWB- 21460000
OWNERSAV DC    CL10'          '                                   -CWB- 21470000
VHEADLEN EQU   *-VTOCHEAD                                         -CWB- 21480000
VTOCHED2 DC    C'-SEQ.  DATA SET NAME     RECFM  LRECL  BLKSIZE DEN TRT+21490000
               CH  MAX BLK  MIN BLK  AVG BLK  BLK COUNT  CREATED  JOB N+21500000
               AME  STEP      EXPIRES'                            -CWB- 21510000
ERR0MSG  DC    C'0WARNING - THE CREATION DATE AND/OR EXPIRATION DATE  OX21520000
               N THE ABOVE '                                            21530000
ERR1MSG  DC    C'HDR1'                                                  21540000
ERR2MSG  DC    C' LABEL IS INVALID '                                    21550000
LMSG     EQU   *-ERR0MSG                                                21560000
         EJECT                                                          21570000
         LTORG                                                          21580000
NOMATCH  DC    C'0WARNING - VOLUME SERIAL NUMBER IN VOL1 LABEL XXXXXX D+21590000
               OES NOT MATCH INPUT DD VOLSER XXXXXX'                    21600000
         EJECT                                                          21610000
*****************************************************************-HMD-  21620000
**  THIS TABLE IS USED TO DETERMINE IF THE VOLSER IN THE JFCB  **-HMD-  21630000
**  OR IN THE UCB IS PRINTABLE EBCDIC. IF SO IT IS DISPLAYED;  **-HMD-  21640000
**  IF NOT, THE DEFAULT CONSTANT IS DISPLAYED..SEE POSMSG.     **-HMD-  21650000
*****************************************************************-HMD-  21660000
EBCDTBL  DS    0C                                                       21670000
         DC    256XL1'FF'                FOR NON-PRINTABLES     -HMD-   21680000
         ORG   EBCDTBL+C' '              SPACES ARE OK          -HMD-   21690000
         DC    X'00'                     MAKE IT OK             -HMD-   21700000
         ORG   EBCDTBL+C'$'              DOLLAR-SIGN            -HMD    21710000
         DC    X'00'                     MAKE IT OK             -HMD-   21720000
         ORG   EBCDTBL+C'#'              POUND-SIGN AND AT-SIGN -HMD-   21730000
         DC    X'0000'                   BOTH ARE OK            -HMD-   21740000
         ORG   EBCDTBL+C'A'              ALPHABET (UPPER-CASE)  -HMD-   21750000
         DC    9X'00'                    OK                     -HMD-   21760000
         ORG   EBCDTBL+C'J'                                     -HMD-   21770000
         DC    9X'00'                                           -HMD-   21780000
         ORG   EBCDTBL+C'S'                                     -HMD-   21790000
         DC    8X'00'                                           -HMD-   21800000
         ORG   EBCDTBL+C'0'              NUMBERS                -HMD-   21810000
         DC    10X'00'                                          -HMD-   21820000
         ORG   ,                         BACK TO REALITY        -HMD-   21830000
         EJECT                                                          21840000
****************************************************************-HMD-   21850000
** THIS TABLE IS USED TO DETERMINE WHETHER THE CREATION DATE  **-HMD-   21860000
** OR THE EXPIRATION DATE IN THE HDR1 OR EOF1 FIELD OF A      **-HMD-   21870000
** STANDARD LABELED TAPE IS NUMERIC. IF NOT, AN ERROR MSG IS  **-HMD-   21880000
** PRINTED AND TAPESCAN CONTINUES.                            **-HMD-   21890000
****************************************************************-HMD-   21900000
NUBTABL  DS    0C                 NUMERIC CHECK TABLE                   21910000
         DC    256XL1'FD'         TO DISTINGUISH FROM EBCDTBL           21920000
         ORG   NUBTABL+C'0'       NUMBERS ONLY                          21930000
         DC    10X'00'                                                  21940000
         ORG   ,                  BACK TO REALITY                       21950000
         EJECT                                                          21960000
DATE     CSECT                                                          21970000
         SAVE  (14,12),T,*                                              21980000
*      R1 = ADDR FOR OUTPUT (DS CL8'MM/DD/YY')                          21990000
*      R2 = R1 FROM THE TIME MACRO                                      22000000
         LR    R12,R15                                                  22010000
         USING DATE,R12                                                 22020000
         LA    R3,DATESAVE                                              22030000
         ST    R13,4(,R3)                                               22040000
         ST    R3,8(,R13)                                               22050000
         LR    R13,R3                                                   22060000
         USING PARMAREA,R1                                              22070000
         L     R1,0(,R1)   GET ADDR FOR OUTPUT            *LACCD*       22080000
         ST    R2,W2                    STORE DATE (00YYDDDF)           22090000
         TM    W2+1,01                  IF ODD NOT LEAP.                22100000
         BO    NOLEAP                    NOT LEAP                       22110000
         TM    W2+1,X'12'               TEST FOR LEAP (VALID TILL 1999) 22120000
         BM    NOLEAP                   1NOT LEAP                       22130000
         MVI   MONTHTBL+5,29            SETUP FEB LEAP YEAR             22140000
NOLEAP   UNPK  MMDDYY+6(3),W2+1(2)           UNPK YR                    22150000
         XC    W1(6),W1                  CLEAR YR FOR DAY RTN.          22160000
         CVB   R4,W1                    GET DAY                         22170000
         LA    R5,MONTHTBL-4            SET BACK PTR                    22180000
MONLUPE  LA    R5,4(R5)                 INCR THRU MON TBL               22190000
         SH    R4,0(R5)                 DROP DOWN THRU MONTBL           22200000
         BH    MONLUPE                  NOT YET, TRY AGAIN              22210000
         AH    R4,0(R5)                 ADD BACK THE DAY                22220000
         CVD   R4,W1                    MAKE                            22230000
         OI    W2+3,X'0F'                  THE DAY                      22240000
         UNPK  MMDDYY+2(3),W2+2(2)            PRINTABLE                 22250000
         MVI   MMDDYY+5,C'/'            RESTORE SLASH                   22260000
         MVC   MMDDYY(2),2(R5)          MOVE THE MONTH                  22270000
         MVI   MMDDYY+2,C'/'  RESTORE SLASH                             22280000
         MVC   THEDATE,MMDDYY         MOVE DATE TO USER                 22290000
RETURN   DS    0H                                                       22300000
         L     R13,4(,R13)                                              22310000
         RETURN (14,12),,RC=0 RESTORE REGS AND RETURN                   22320000
         EJECT                                                          22330000
         SPACE 2                                                        22340000
MONTHTBL DS    0CL48                                                    22350000
MTK      DC    H'31',C'01' JAN                                          22360000
         DC    H'28',C'02' FEB                                          22370000
         DC    H'31',C'03' MAR                                          22380000
         DC    H'30',C'04' APR                                          22390000
         DC    H'31',C'05' MAY                                          22400000
         DC    H'30',C'06' JUN                                          22410000
         DC    H'31',C'07' JLY                                          22420000
         DC    H'31',C'08' AUG                                          22430000
         DC    H'30',C'09' SEP                                          22440000
         DC    H'31',C'10' OCT                                          22450000
         DC    H'30',C'11' NOV                                          22460000
         DC    H'255',C'12' DEC                                         22470000
         SPACE 2                                                        22480000
DATESAVE     DS    9D                                                   22490000
W1       DS    F              WORKARE1                                  22500000
W2       DS    F              WORKAREA2                                 22510000
MMDDYY   DS    CL9                                                      22520000
PARMAREA DSECT                                                          22530000
THEDATE  DS    CL8                                                      22540000
         SPACE 5                                                        22550000
VTOC     DSECT                                                          22560000
VTOCSEQN DS    CL4                SEQUENCE NUMBER                 -CWB- 22570000
VTOCDSN  DS    CL17               DATA SET NAME.                  -CWB- 22580000
VTOCRECF DS    CL4                RECFM.                          -CWB- 22590000
VTOCLREC DS    CL5                LRECL.                          -CWB- 22600000
VTOCBLKS DS    CL5                BLKSIZE.                        -CWB- 22610000
VTOCDEN  DS    CL4                DENSITY.                        -CWB- 22620000
VTOCTRTC DS    CL2                TRTCH.                          -CWB- 22630000
VTOCMAXB DS    CL5                MAXIMUM BLOCK SIZE.             -CWB- 22640000
VTOCMINB DS    CL5                MINIMUM BLOCK SIZE.             -CWB- 22650000
VTOCAVGB DS    CL5                AVERAGE BLOCK SIZE.             -CWB- 22660000
VTOCOUNT DS    CL6                BLOCK COUNT.                    -CWB- 22670000
VTOCCREA DS    CL8                CREATION DATE.                  -CWB- 22680000
VTOCJOBN DS    CL8                JOB NAME.                       -CWB- 22690000
VTOCSTEP DS    CL8                STEP NAME.                      -CWB- 22700000
VTOCEXPR DS    CL8                EXPIRATION DATE.                -CWB- 22710000
         DS    0D                 ADVANCE TO DOUBLE WORD BNDRY.   -CWB- 22720000
VTOCSIZE EQU   *-VTOC             SIZE OF VTOC ENTRY.             -CWB- 22730000
VTOCEPB  EQU   10                 NUMBER OF ENTRIES PER BLOCK.    -CWB- 22740000
VTOCBLSZ EQU   VTOCEPB*VTOCSIZE+8 SIZE OF VTOC BLOCK.             -CWB- 22750000
         EJECT                                                          22760000
         IEFUCBOB LIST=YES                                        -HMD- 22770000
         EJECT                                                 HD DEC86 22780000
         DCBD  DSORG=PS,DEVD=TA                                HD DEC86 22790000
         EJECT                                                 HD DEC86 22800000
JFCB     DSECT                                                 HD DEC86 22810000
         IEFJFCBN LIST=YES                                     HD DEC86 22820000
JOUTFLSQ EQU   JFCBOUT+(JFCBFLSQ-JFCB)                            *JLM* 22830000
         END                                                            22840000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             00090000
//SYSIN    DD  *                                                        00100000
  NAME TAPESCAN(R)                                                      00110000
//                                                                      00120000
