//DISKMAP JOB (JOB),
//             'INSTALL DISKMAP',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*                                                                     00030000
// EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,STMT,NUM,BUF(MAX)',           00040000
//             MAC1='SYS1.AMODGEN'                                      00050000
//*                                                                     00060000
//ASM.SYSIN DD *                                                        00070000
MAP      TITLE 'MAP DIRECT ACCESS DEVICE ALLOCATION'                    00030000
* *$DOC@* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00040000
*        PRODUCED AT TRIANGLE UNIVERSITIES COMPUTATION CENTER, RESEARCH 00050000
*              TRIANGLE PARK, NORTH CAROLINA, 27709                     00060000
*              SYSTEMS PROGRAMMER - H. L. JACKSON                       00070000
         SPACE                                                          00080000
*        EXTENSIONS AT GODDARD SPACE FLIGHT CENTER, GREENBELT,          00090000
*              MARYLAND, 20771                                          00100000
*              HANK HOOVER                                              00110000
         SPACE                                                          00120000
*        MODIFIED AT USAF COMMAND POST, PENTAGON, WASHINGTON, DC, 20330 00130000
*              SYSTEMS PROGRAMMER - V. R. RODRIGUEZ                     00140000
*                                                                       00150000
*        MODIFIED AT AFDSC/SFI, PENTAGON, WASHINGTON,D.C. 20330         00160000
*                  SYSTEMS PROGRAMMER - P E SCHAFER  JUN 1978 & FEB82   00170000
*                  TOOK OUT 2321 SUPPORT AND PARM OF ALL WHICH DID      00180000
*                  NOT WORK.  ADDED CHECK FOR RACF BIT AND CHECK TO     00190000
*                  SEE IF AUTHORIZED TO LIST MSS VOLUMES.               00200000
*                  RUNS ON MVS/SP 1.1.0                                 00210000
         SPACE 2                                                        00220000
* FUNCTION:                                                             00230000
*  TO MAP THE ALLOCATION OF DATA SETS ON DASD'S                         00240000
* INPUT: NONE                                                           00250000
* UTILITY: SYSUT1 - A FULLY DD DESCRIBED DATA SET                       00260000
* VOLUMES TO BE MAPPED: ALL VOLUMES TO BE MAPPED MUST HAVE A DD CONTROL 00270000
*                  CARD, THE DDNAME MAY BE ANYTHING, IT IS PREFFERED    00280000
*                  IF THEY WERE DD1, DD2, ETC.  DISP=OLD,VOLUME=SER AND 00290000
*                  UNIT=  MUST BE GIVEN. NO OTHER PARAMETERS ARE NEEDED 00300000
*                  THESE DD CARDS MUST FOLLOW THE SYSUT1 DD CARD.       00310000
         SPACE 3                                                        00320000
* OUTPUT: SYSPRINT                                                      00330000
* METHOD:                                                               00340000
*   1.  FIND TIOT BY USING EXTRACT                                      00350000
*   2.  READ TIOT TO FIND NEW DDNAMES                                   00360000
*   3.  PUT DDNAME IN DCB OF 'TEST' AND 'UTILITY'                       00370000
*   4.  READ JFCB FOR 'TEST'  THIS GIVES VOLUME NUMBER OF DISK          00380000
*       DESIRED TO BE MAPPED.                                           00390000
*   5.  FETCH VOLUME NUMBER FROM JFCB.                                  00400000
*   6.  SEARCH UCB'S FOR THIS DISK.                                     00410000
*   7.  LOCATE POSITION OF VTOC.                                        00420000
*   8.  READ VTOC USING OBTAIN ABSOLUTE TRACK.                          00430000
*   9.  CONVERT DATA TO READABLE FORMAT FOR PRINT LINE.                 00440000
*  10.  ACCUMULATE ALL EXTENTS AND SORT IN ORDER OF FIRST TRACK.        00450000
*  11.  IF THE DATA SET IS A PARTITIONED DATA SET THEN THE FOLLOWING    00460000
*       TASKS ARE PERFORMED TO CHECK THE DIRECTORY SIZE.  IF NOT        00470000
*       THEN THE NEXT DATA SET IS FETCHED AND CONVERTED.                00480000
*  12.  READ THE JFCB FOR THE 'UTILITY' DCB.  IT HAS THE DDNAME         00490000
*       INSERTED FROM ITEM 3.                                           00500000
*  13.  CHANGE THE NAME OF THE DATA SET TO CORRESPOND TO THE PARTITION- 00510000
*       ED ONE DESIRED.                                                 00520000
*  14.  OPEN TYPE J THE DATA SET USING THE MODIFIED JFCB.               00530000
*  15.  GET BLOCKS FROM THE DIRECTORY UNTIL END OF FILE.  RECORD THE    00540000
*       NUMBER OF TOTAL DIRECTORY BLOCKS AND THE NUMBER USED.           00550000
*  16.  AFTER ALL THE DATA SETS ARE LISTED, THEN A DISK STORAGE SPACE   00560000
*       MAP IS MADE.  THE MAP SHOWS ALL EXTENTS IN ASCENDING TRACK      00570000
*       ORDER.  A CROSS REFERENCE IS MADE TO THE DSNAME.                00580000
*$DOC$****************************************************************  00590000
MAPDISK  CSECT                                                          00600000
R0       EQU   0                  SYSTEMS                               00610000
R1       EQU   1                  SYSTEMS                               00620000
R2       EQU   2                  WORK & TEMP                           00630000
R3       EQU   3                  WORK & TEMP                           00640000
R4       EQU   4                  WORK & TEMP                           00650000
R5       EQU   5                  WORK & TEMP                           00660000
R6       EQU   6                  BASE REGISTER FOR PROGRAM             00670000
R7       EQU   7                  BASE TABLE                            00680000
R8       EQU   8                  BASE THIS CSECT                       00690000
R9       EQU   9                  BASE UCB DSECT                        00700000
R10      EQU   10                 BASE DSCB DSECT                       00710000
R11      EQU   11                 BASE UCB TABLE DSECT                  00720000
R12      EQU   12                 BASE CVT DSECT                        00730000
R13      EQU   13                 SAVE & WORKSCTN DSECT                 00740000
R14      EQU   14                 RETURN                                00750000
R15      EQU   15                 REGISTER ASSIGNMENTS                  00760000
         SAVE (14,12)                                           PES0678 00770000
         LR    R8,R15                                           PES0678 00780000
         LA    R6,2048(R8)                                      PES0678 00790000
         LA    R6,2048(R6)                                      PES0678 00800000
         USING MAPDISK,R8,R6                                    PES0678 00810000
         GETMAIN R,LV=WRKLNGTH         GET WORKING STORAGE AREA PES0678 00820000
         USING WORKSCTN,R1                                      PES0678 00830000
         ST    R1,8(R13)                                        PES0678 00840000
         ST    R13,SAVEAREA+4                                   PES0678 00850000
         LR    R13,R1                                           PES0678 00860000
         USING DSCB4,R10          SETTING UP ADDRESSES                  00870000
         USING UCB,R11                                                  00880000
         USING CVT,R12                                                  00890000
         USING WORKSCTN,R13                                     PES0678 00900000
         OPEN  (PRINT,(OUTPUT))                                         00910000
*                           RELOCATED CODE FOLLOWING FROM        JLM    00911000
*                           HEADING ROUTINE TO PREVENT           JLM    00911100
*                           UNNECESSARY REPEATED EXECUTION       JLM    00911200
         TIME  DEC   SECURE DATE/TIME FROM THE INTEGRAL TIMER           00912000
         SRL   0,12   SHIFT TO SECONDS/DECADE DIGIT                     00913000
         ST    0,WORD   SAVE THE DECIMAL TIME IN (HHMMS)                00914000
         OI    WORD+3,X'0F'   CHANGE SECONDS/DECADE TO (NO-SIGN)        00915000
         UNPK  TIME,WORD   DISPLAY THE TIME OF DAY                      00916000
         ST    1,WORD   SAVE THE DECIMAL DATE IN JULIAN                 00917000
         AP    WORD,=PL4'1900000'  ADD 19 TO CENTURY BYTE        JLM    00918000
         UNPK  EIGHT,WORD  CONVERT THE DATE TO DISPLAY FORM      JLM    00919000
         MVC   YEAR,EIGHT+1 DISPLAY THE YEAR                     JLM    00919100
         ZAP   DOUBLE,=P'0'   CLEAR THE YEAR CONVERSION AREA            00919200
         MVO   DOUBLE,WORD(2)   LOAD THE CCYY AS PACKED DECIMAL  JLM    00919300
         CVB   1,DOUBLE   LOAD YEAR AS THE DIVIDEND                     00919400
         SR    0,0   & CLEAR THE DIVIDEND HIGH-ORDER REGISTER           00919500
         D     0,=F'4'   YEAR / 4                                       00919600
         LTR   0,0   IF REMAINDER NOT ZERO                       JLM    00919700
         BNZ   LOOKSET          BYPASS LEAP YEAR ADJUSTMENT             00919800
         CVB   1,DOUBLE   LOAD YEAR AS THE DIVIDEND              JLM    00919900
         SR    0,0   & CLEAR THE DIVIDEND HIGH-ORDER REGISTER    JLM    00920000
         D     0,=F'100' YEAR / 100                              JLM    00920100
         LTR   0,0   IF REMAINDER NOT ZERO                       JLM    00920200
         BNZ   ISLEAP          PERFORM LEAP YEAR ADJUSTMENT      JLM    00920300
         CVB   1,DOUBLE   LOAD YEAR AS THE DIVIDEND              JLM    00920400
         SR    0,0   & CLEAR THE DIVIDEND HIGH-ORDER REGISTER    JLM    00920500
         D     0,=F'400' YEAR / 400                              JLM    00920600
         LTR   0,0   IF REMAINDER NOT ZERO                       JLM    00920700
         BNZ   LOOKSET          BYPASS LEAP YEAR ADJUSTMENT      JLM    00920800
ISLEAP   AP    MONTHS+5(2),=P'1'   SET FEBRUARY DAYS TO '29', LEAP YEAR 00920900
LOOKSET  LA    1,MONTHS   SET THE MONTH TABLE BASE                      00921000
LOOKAT   SP    WORD+2(2),0(2,1)   IS THE DAY WITHIN THIS MONTH   JLM    00921100
         BNP   GETMONTH   IF YES, GO TO EXTRACT THE MONTH ABBREVIATION  00921200
         LA    1,5(1)   SET THE TABLE BASE TO THE NEXT MONTH            00921300
         B     LOOKAT   & GO TO CHECK THE NEXT MONTH                    00921400
GETMONTH AP    WORD+2(2),0(2,1)   RESET THE DAY OF THE MONTH     JLM    00921500
         MVC   MONTH,2(1)   EXTRACT THE PROPER MONTH ABBREVIATION       00921600
         OI    WORD+3,X'0F'   UN-SIGN THE DAY OF THE MONTH              00921700
         UNPK  DAY,WORD+2(2) DISPLAY THE DAY OF THE MONTH        JLM    00921800
*                           END OF RELOCATED CODE                JLM    00921900
         MVC   APPEN,CVTXAPG+1    MOVE ADDRESS ONCE                     00922000
         EXTRACT TIOT,'S',FIELDS=(TIOT)   GET ADDRESS OF TIOT IN WORK   00930000
         L     R2,TIOT            BRING ADDRESS OF TIOT IN              00940000
         MVC   JOBNAME,0(R2)      SAVE JOBNAME FOR LATER CHECK          00950000
         LA    R2,24(R2)          BUILD UP TO FIRST DDNAME              00960000
         SR    R3,R3              CLEAR A REGISTER                      00970000
FNDSYSUT CLC   4(7,R2),NOUTIL+4                                         00980000
         BE    SVSYSUTA                                                 00990000
         IC    R3,0(R2)                                                 01000000
         LTR   R3,R3                   CHECK FOR END LIST               01010000
         BZ    ERROR                                                    01020000
         IC    R3,0(R2)                                                 01030000
         LA    R2,0(R2,R3)                                              01040000
         B     FNDSYSUT                                                 01050000
ERROR    BAL   R14,HEADING                                              01060000
         MVC   EXTOUT+3(29),NOUTIL                                      01070000
         BAL   R14,LINE0OUT            PRINT ABOVE                      01080000
         B     GOBCK                   LEAVE                            01090000
SVSYSUTA EQU   *                                                        01100000
         MVC   SVUCBADR,17(R2)  SAVE UCB ADDRESS FROM SYSUT1 TIOT ENTRY 01110000
         LA    R2,0(R2,R3)         READY FOR NEXT                       01120000
         ST    R2,TIOT             STORE VALUE FOR NEXT TIME AROUND     01130000
*        INITIAL HEADING                                                01140000
         SPACE                                                          01150000
DISKLOOP LA    R7,F356TBL    ADDRESS OF 3-5-6 DSCB POINTERS     PES0678 01160000
         LA    R10,WORK           ADDRESS FOR DSCB FORMAT 4             01170000
         L     R12,CVTPTR              ADDRESS FOR CVT                  01180000
         MVC   NTRKNT,=F'1'       START DSCB'S AT BOTTOM OF TABLE       01190000
         MVC   SRTKNT,=F'1'       START SORT COUNT AT BOTTOM            01200000
         OC    BIN(2),BIN              CLEAR BIN FOR CNVT               01210000
         MVC   SUBCEL,SAVESER+6        BLANK SUB CELL IN HEADING        01220000
         L     R11,CVTILK2   ADDRESS OF UCB POINTER                     01230000
SWT1     NOP   MANY               SWITCH FOR MORE THAN ONE VOL IN DD    01240000
         SR    R3,R3              CLEAR COUNT REGISTER                  01250000
         L     R2,TIOT            CHECK FOR END OF WORK                 01260000
         IC    R3,0(R2)                                                 01270000
         LTR   R3,R3                                                    01280000
         BZ    ALLDONE                                                  01290000
         CLC   4(8,R2),=CL8'SYSPRINT'                           PES0678 01300000
         BE    SKIPDD                                                   01310000
         CLC   4(8,R2),=CL8'SYSABEND'                                   01320000
         BE    SKIPDD                                                   01330000
         CLC   4(8,R2),=CL8'SYSIN'                                      01340000
         BE    SKIPDD                                                   01350000
         MVC   DCBDDNM(8),4(R2)   MOVE THIS NAME TO DCB                 01360000
         MVC   DCBDDNM2(8),4(R2)  MOVE NAME TO UTILITY DATA BLOCK       01370000
         LA    R2,0(R2,R3)        READY FOR NEXT DDNAME                 01380000
         ST    R2,TIOT            REPLACE                               01390000
         RDJFCB    (TEST)         GET VOLUME NO. FOR THIS DD            01400000
MANY     SR    R3,R3                                                    01410000
         IC    R3,JFCBNVOL        CHECK NUMBER OF VOLS LISTED           01420000
         BCT   R3,OVER            LAST ONE DOES NOT JUMP                01430000
         NI    SWT1+1,X'0F'       TURN OFF SWITCH                       01440000
SLIPIN   LA    R3,JFCBVOLS(R3)    ADDRESS OF VOLUME NUMBER              01450000
         MVC   SAVESER,0(R3)      MOVE IT TO TITLE                      01460000
UCBLOOP  ST    R11,UCBPTRAD   SAVE R11                                  01470000
         LH    R11,0(R11)              LOAD UCB ADDR                    01480000
         LTR   R11,R11                                                  01490000
         BZ    NEXTADDR                B IF DUMMY ENTRY                 01500000
         BM    NOTHERE                 B END OF LIST                    01510000
         CLI   UCBTYP+2,X'20'          CHECK FOR DIRECT ACCESS          01520000
         BE    ONOFF              DA JUMPS                              01530000
NEXTADDR L     11,UCBPTRAD             RESTORE 11                       01540000
         LA    11,2(11)                NEXT ENTRY IN TABLE              01550000
         B     UCBLOOP            NEXT UCB ADDRESS                      01560000
ONOFF    TM    UCBSTAT,X'80'   CHECK FOR ON-LINE                PES0678 01570000
         BZ    NEXTADDR           UNIT IS OFFLINE                       01580000
         CLC   UCBVOLI,=6X'0'         TEST FOR NO VOL SER       PES0678 01590000
         BE    NEXTADDR                                                 01600000
         CLC   JOBNAME+1(3),=C'SMI'                             PES0282 01610000
         BE    PASSVIRT            LET UP MAP VIRTUAL VOLS      PES0282 01620000
         TM    UCBTBYT2,UCBRVDEV   CHECK FOR VIRTUAL VOL        PES0282 01630000
         BO    NEXTADDR           NO MAP                        PES0282 01640000
PASSVIRT EQU   *                                                        01650000
         CLC   SAVESER,UCBVOLI    CHECK FOR VOLUME HERE         PES0678 01660000
         BNE   NEXTADDR           IF NOT HERE, TRY NEXT ONE             01670000
         MVC   TTRVTOC,UCBVTOC    SAVE TTR OF VTOC              PES0678 01680000
         MVC   UNIT,UCBNAME            PUT IN UNIT NAME                 01690000
         BAL   R14,HEADING   PUT OUT TITLE                              01700000
         MVC   EXTOUT+20(42),HEAD2  PUT IN HEADING                      01710000
         SPACE 2                                                        01720000
*        HEADER 2                                                       01730000
         SPACE                                                          01740000
         BAL   R14,LINE0OUT                                             01750000
         SPACE 2                                                        01760000
*        HEADER 3                                                       01770000
         SPACE                                                          01780000
         LA    R0,HEAD3   LOAD ADDRESS OF OUTPUT AREA                   01790000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      01800000
         SPACE 2                                                        01810000
*        HEADER 4                                                       01820000
         SPACE                                                          01830000
         LA    R0,HEAD4   LOAD ADDRESS OF OUTPUT AREA                   01840000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      01850000
         AP    LINEKNT,=P'3'      BOOST LINE COUNT                      01860000
         MVI   SEEK,0                                                   01870000
         MVC   SEEK+1(7),SEEK     CLEAR CONVERT BIN                     01880000
         STH   R11,UCBXREF+2      STORE UCB ADDRESS INDEX               01890000
         L     R15,CVTPCNVT       LOAD ADDRESS OF CONVERT ROUTINE       01900000
         L     R0,TTRVTOC              TTR OF VTOC                      01910000
         LA    R1,DEB             ADDRESS OF PSEUDO DEB                 01920000
         LA    R2,SEEK            ADDRESS TO STORE                      01930000
         STM   R9,R13,HOLD        SAVE REGS                             01940000
         BALR  R14,R15                                                  01950000
         LM    R9,R13,HOLD        RETURN REGS                           01960000
         SPACE 2                                                        01970000
*        READ FORMAT 4 DSCB                                             01980000
         SPACE                                                          01990000
GETDSCB  OBTAIN LIST              LOOK FOR DSCB'S                       02000000
         LTR   R15,R15            SEE IF OK                             02010000
         BE    NEXTCHK            OK                                    02020000
         CH    R15,=H'8'   MIDDLE OF TEST AREA                          02030000
         BL    GETDSCB            VOLUME NOT MOUNTED                    02040000
         BE    FETCHNXT           NOT THERE ANY MORE, GET NEXT DSCB     02050000
IOER     MVC   SERMSG,SAVESER     PUT SERIAL NUMBER IN PRINT            02060000
IOER3    LA    R0,MSSAGE+4   LOAD ADDRESS OF OUTPUT AREA                02070000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      02080000
         MVC   DATANAME(132),OUTBIN    CLEAR LINE                       02090000
         B     DISKLOOP           GO GET NEXT VTOC                      02100000
IOER1    MVC   EXTOUT(60),=C'0FORMAT 5 DSCB NOT FOUND BUT POINTED TO BY*02110000
                THE FORMAT 4 DSCB'                                      02120000
         BAL   R14,LINE0OUT                                             02130000
IOER4    MVC   SERMSG,SAVESER     PUT SER NUMBER IN PRINT OUT           02140000
         B     IOER3              NEXT LINE ON PRINT NOT CONSOLE        02150000
IOER2    MVC   EXTOUT(55),=C'0SEARCH FOR A FORMAT 3 OR FORMAT 5 DSCB IS*02160000
                UNSUCCESSFUL'                                           02170000
         BAL   R14,LINE0OUT                                             02180000
         B     IOER4                                                    02190000
SKIPDD   LA    R2,0(R2,R3)   BUMP TO NEXT ENTRY                         02200000
         ST    R2,TIOT                                                  02210000
         B     SWT1                                                     02220000
OVER     STC   R3,JFCBNVOL        STORE COUNT BACK                      02230000
         OI    SWT1+1,X'F0'       TURN SWITCH ON                        02240000
         MH    R3,=H'6'           FOR NUMBER OF VOL SERIALS             02250000
         B     SLIPIN             PICK UP VOL SERIAL                    02260000
CHECKIT1 CH    R15,=H'8'   MIDDLE OF TEST AREA                          02270000
         BL    F1DSCBS                                                  02280000
         B     IOER-4                                                   02290000
NOTHERE  EQU   *                                                        02300000
         MVC   EXTOUT+20(42),HEAD2  PUT IN HEADING                      02310000
         BAL   R14,LINE1OUT                                             02320000
         MVC   EXTOUT+1(31),WARNING   VOLUME NOT MOUNTED                02330000
         BAL   R14,LINE1OUT                                             02340000
         B     DISKLOOP           GET NEXT VOLUME                       02350000
HEADING  ST    R14,SV14HOLD       SAVE RETURN                           02360000
         AP    PAGEKNT,=P'1'      ADD ONE TO PAGE COUNT                 02630000
         MVC   PAGEPLC,PATTERN    PUT IN EDIT MASK                      02640000
         ED    PAGEPLC,PAGEKNT    NUMBER FOR PAGE                       02650000
         LA    R0,HEAD1   LOAD ADDRESS OF OUTPUT AREA                   02660000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      02670000
         MVI   EXTOUT,C'0'             SKIP 1                           02680000
         LA    R0,EXTOUT   LOAD ADDRESS OF OUTPUT AREA                  02690000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      02700000
         MVI   EXTOUT,C' '                                              02710000
         ZAP   LINEKNT,=P'3'      THREE LINES PRINTED                   02720000
         L     R14,SV14HOLD       RESTORE                               02730000
         BR    R14                CONTINUE                              02740000
NEWPAGE  ST    14,SV14                                                  02750000
         MVC   EXTOUT+20(42),HEAD2  PUT IN HEADING                      02760000
         LA    R0,EXTOUT   LOAD ADDRESS OF OUTPUT AREA                  02770000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      02780000
         MVC   EXTOUT+1(61),EXTOUT     CLEAR LINE                       02790000
         LA    R0,HEAD3   LOAD ADDRESS OF OUTPUT AREA                   02800000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      02810000
         LA    R0,HEAD4   LOAD ADDRESS OF OUTPUT AREA                   02820000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      02830000
         MVC   CRDATE(85),CRDATE-2  CLEAR REST OF LINE                  02840000
         BAL   R14,PRINTSUB-4   & GO TO PRINT A LINE                    02850000
         AP    LINEKNT,=P'3'                                            02860000
         L     14,SV14                                                  02870000
         BR    14                                                       02880000
NEXTCHK  CLI   DS4IDFMT,C'4'   CHECK FOR FORMAT 4                       02890000
         BNE   IOER               FIRST BLOCK MUST BE                   02900000
         MVC   SAVEF4,DSCB4       SAVE ALL OF FORMAT 4                  02910000
         MVC   SVCOUNT,DS4DEVDT   SAVE NUMBER OF DSCBS PER TRACK        02920000
         MVC   TRKPRCYL(2),DS4DEVSZ+2  SAVE NUMBER OF TRACKS PER CYL    02930000
         SPACE 2                                                        02940000
*        READ ALL DSCB'S EXCEPT FORMAT 4                                02950000
         SPACE                                                          02960000
FETCHNXT SR    R2,R2              CLEAR WORKING REG                     02970000
         LA    R10,SAVEF4         SET UP ADDRESS FOR IT                 02980000
         IC    R2,ADDRS+4         RECORD COUNT IN                       02990000
         LA    R2,1(R2)           BOOST BY ONE                          03000000
         CH    R2,TRKCNT          CHECK NUMBER OF RECORDS ON THE TRACK  03010000
         BNH   FETCHSQ            GO FETCH NEXT                         03020000
         MVC   WORK(4),ADDRS      MOVE TO TAKE CARE OF BOUNDRY          03030000
         LH    R2,WORK+2          LOAD ABSOLUTE TRACK ADDRESS           03040000
         LA    R2,1(R2)   BOOST BY ONE                                  03050000
         STH   R2,WORK+2          RESTORE ALL THE WAY                   03060000
* CHECK FOR CYLINDER OVERFLOW                                           03070000
         CH    R2,TRKPRCYL                                              03080000
         BL    RTORCCHH                                                 03090000
         SR    R2,R2                   RESET HEAD CNT                   03100000
         STH   R2,WORK+2                  AND STORE                     03110000
         LH    R2,WORK                    THEN                          03120000
         LA    R2,1(R2)                INCR CYL CNT                     03130000
         STH   R2,WORK                    AND STORE IT                  03140000
RTORCCHH MVC   ADDRS(4),WORK   PLACE BACK (CYL + 1)                     03150000
         LA    R2,1               RECORD = 1                            03160000
FETCHSQ  STC   R2,ADDRS+4         STORE RECORD COUNT                    03170000
FETCHRND CLC   DS4HPCHR(5),ADDRS       SEE IF VTOC FINISHED             03180000
         BL    CK356TBL                SEE IF ANY DSCB'S AFTER LAST F1  03190000
F1DSCBS  OBTAIN    LIST           NEXT DSCB                             03200000
         LTR   R15,R15            CHECK FOR SUCCESS                     03210000
         BNE   CHECKIT1           JUMP IF BAD                           03220000
         LA    R10,WORK           RE INSTATE FOR NEXT TWO CHECKS        03230000
         CLI   DS4IDFMT,X'0'      CHECK FOR FORMAT 3-5-6                03240000
         BE    FETCHNXT           MUST BE ZERO SO GO GET NEXT           03250000
         CLC   DS1PTRDS(5),=6X'0'   CK CCHHR PTR                        03260000
         BE    NOPTR                                                    03270000
         LA    R1,30                   SET COUNTER                      03280000
         LA    R2,EXTNTBL    LOAD ADDR OF EXTENT TABLE          PES0678 03290000
         LA    R2,2(R2)                BUMP PAST TABLE COUNT            03300000
FINDHOLE CLC   0(5,R2),=6X'0'   IS THIS ENTRY USED                      03310000
         BE    SAVE356P                NO - USE IT                      03320000
         LA    R2,5(R2)                BUMP TO NEXT ENTRY               03330000
         BCT   R1,FINDHOLE                                              03340000
         MVC   EXTOUT(50),=C'0EXTNTBL OVERFLOW, UNPREDICTABLE RESULTS M*03350000
               AY OCCUR'                                                03360000
         BAL   R14,LINE1OUT                                             03370000
         B     CK356TBL                FINISH THIS VOLUME               03380000
SAVE356P MVC   0(5,R2),DS1PTRDS        SAVE PTR                         03390000
         LA    R2,EXTNTBL    GET BEG AGAIN                      PES0678 03400000
         LH    R1,0(R2)                LOAD COUNT                       03410000
         LA    R1,1(R1)                  INCR                           03420000
         STH   R1,0(R2)                  AND SAVE                       03430000
NOPTR    CLI   DS4IDFMT,C'1'   CHECK FOR FORMAT 1                       03440000
         BE    FORMAT1            GO PROCESS                            03450000
         MVC   0(8,R7),SEEK       PUT CCHHR IN POINTER TABLE            03460000
         GETMAIN   R,LV=160       FETCH 160 BYTES TO STORE DSCB         03470000
         ST    R1,8(R7)           STORE ADDRESS OF WHERE DSCB STORED    03480000
         LA    R7,12(R7)          BOOST                                 03490000
         MVC   0(140,R1),WORK     MOVE DSCB                             03500000
         LA    R2,EXTNTBL                                       PES0678 03510000
         LH    R1,0(R2)                LOAD COUNT                       03520000
         LTR   R1,R1                   TEST ZERO ENTRIES                03530000
         BZ    FETCHNXT                IF NONE GO READ NEXT DSCB        03540000
         LA    R1,30                   SET COUNT                        03550000
         LA    R2,2(R2)                BUMP PAST COUNT                  03560000
FIND356P CLC   ADDRS(5),0(R2)          IS THIS DSCB IN TBL              03570000
         BE    CLEAR356                YES - GO CLEAR TBL ENTRY         03580000
         LA    R2,5(R2)                NO - INCR TO NEXT ENTRY          03590000
         BCT   R1,FIND356P             SEARCH ENTIRE TABLE              03600000
         B     FETCHNXT           GO GET NEXT DSCB                      03610000
CLEAR356 XC    0(5,R2),0(R2)           CLEAR ENTRY FOR THIS DSCB        03620000
         LA    R2,EXTNTBL                                       PES0678 03630000
         LH    R1,0(R2)                LOAD COUNTER                     03640000
         BCTR  R1,0                      DECR                           03650000
         STH   R1,0(R2)                  AND STORE                      03660000
         B     FETCHNXT                READ NEXT DSCB                   03670000
CK356TBL MVI   FETCHRND+7,X'40'   RESET BRANCH                          03680000
         LA    R2,EXTNTBL    LOAD TBL ADDR                      PES0678 03690000
         LH    R1,0(R2)                LOAD TABLE COUNT                 03700000
         LTR   R1,R1                   TEST EMPTY TBL                   03710000
         BZ    PRMAP                   NOTHING PAST LAST F1             03720000
         MVI   FETCHRND+7,X'F0'        MAKE UNCONDITIONAL BRANCH        03730000
         LA    R1,30                   SET COUNT                        03740000
         LA    R2,2(R2)                BUMP TO CCHHR ENTRIES            03750000
SRCHF356 CLC   0(5,R2),=6X'0'   IS THIS SLOT EMPTY                      03760000
         BNE   FOUND356                NO - SET UP TO READ IT           03770000
         LA    R2,5(R2)                YES - BUMP TO NEXT ENTRY         03780000
         BCT   R1,SRCHF356                                              03790000
         B     PRMAP                   END OF TBL GO PRINT MAP          03800000
FOUND356 MVC   ADDRS(5),0(R2)          MOVE IN ADDRESS OF DSCB          03810000
         B     F1DSCBS                 GO READ IT                       03820000
FORMAT1  GETMAIN   R,LV=160       FETCH 160 BYTES TO STORE DSCB         03830000
         MVC   0(140,R1),WORK     STORE DSCB                            03840000
         MVC   140(8,R1),SEEK     MBBCCHHR STORED ALSO                  03850000
         LA    R3,DSTBL-4         FETCH ADDRESS OF STARTING POINT       03860000
         L     R4,NTRKNT          NO. OF ENTRIES                        03870000
SORTF12  BCT   R4,SORTF11                                               03880000
         ST    R1,4(R3)           PUT ADDRESS AWAY                      03890000
SORTF14  L     R4,NTRKNT          BRING IN COUNT                        03900000
         LA    R4,1(R4)           TO BOOST BY ONE                       03910000
         ST    R4,NTRKNT                                                03920000
         B     FETCHNXT           GO GET NEXT DSCB                      03930000
SORTF11  LA    R3,4(R3)           BOOST TABLE ADDRESS TO NEXT           03940000
         L     R5,0(R3)           POINTER TO DSCB                       03950000
         CLC   WORK(44),0(R5)     LOOK AT DSNAME                        03960000
         BH    SORTF12            JUMP AROUND IF IT DOES NOT FIT        03970000
SORTF13  ST    R1,0(R3)           PUT ADDRESS IN TABLE                  03980000
         LR    R1,R5              LAST ADDRESS MOVED AROUND             03990000
         L     5,4(3)                                                   04000000
         LA    R3,4(R3)           BOOST TO NEXT                         04010000
         BCT   R4,SORTF13         MOVE ALL UP                           04020000
         ST    R1,0(R3)           PUT OUT LAST ADDRESS                  04030000
         B     SORTF14            FINISH UP THIS DSCB                   04040000
         SPACE 2                                                        04050000
*        PRINT 'VTOC'                                                   04060000
         SPACE                                                          04070000
PRMAP    MVC   DATANAME(4),=C'VTOC'                                     04080000
         LA    R1,DS4VTOCE        PUT OUT EXTENT SIZE                   04090000
         BAL   R14,ABTOREL        CONVERT TO RELATIVE TRACKS R2 BACK    04100000
         LA    R3,1               PSEUDO EXTENT NO. 1                   04110000
         BAL   R14,STORMAP        STORE EXTENT FOR MAP R1,R2,DATANAME   04120000
         BAL   R14,LENGTH         FIND LENGTH OF DATA SET R2-R3 BACK    04130000
         LA    R1,SPALLOC                                               04140000
         BAL   R14,EDIT           PREPARE TO PRINT R3,R1                04150000
         LR    R9,R2              SAVE TOTAL FOR AWHILE                 04160000
         MVC   HOLD+4(4),DS4HPCHR   GET END MARKER                      04170000
         SRL   R2,16                                                    04180000
         BAL   R14,ABTOREL1       DO ONLY USED                          04190000
         BAL   R14,LENGTH                                               04200000
         LA    R1,SPUSED                                                04210000
         BAL   R14,EDIT                                                 04220000
         SPACE 2                                                        04230000
*        PRINT VTOC VALUES                                              04240000
         SPACE                                                          04250000
         BAL   R14,PRINTSUB-4   & GO TO PRINT A LINE                    04260000
         MVC   EXTOUT+11(24),HEAD5  EXT-FIRST--LAST-LENGTH              04270000
         SPACE 2                                                        04280000
*        HEAD 5                                                         04290000
         SPACE                                                          04300000
         BAL   R14,LINE0OUT                                             04310000
         AP    LINEKNT,=P'2'      LINE COUNT UP ONE                     04320000
         LR    R2,R9              OUT OF ICE                            04330000
         LA    R3,1               EXTENT NUMBER                         04340000
         SPACE 2                                                        04350000
*        COMPUTE FREE SPACE VALUES                                      04360000
         SPACE                                                          04370000
         BAL   R14,FULLEDIT       DO FULL EDIT                          04380000
         MVC   EXTOUT+12(23),ZEXT   VALUES OF EXT-FIRST--LAST-LENGTH    04390000
         SPACE 2                                                        04400000
*        PRINT 'FREE SPACE'                                             04410000
         SPACE                                                          04420000
         BAL   R14,LINE1OUT                                             04430000
         TM    DS4VTOCI,X'80'     TEST FOR DSCB 5                       04440000
         BO    DOF1S              NO DSCB 5 JUMPS                       04450000
         MVC   DATANAME(10),=CL10'FREE SPACE'                           04460000
         MVC   SPUSED,SPUSED-1    CLEAR USED SPACE                      04470000
         LA    R10,F356TBL   FETCH ADDRESS OF FIRST FORMAT      PES0678 04480000
         L     R10,8(R10)                FIVE DSCB                      04490000
         CLI   DS5FMTID,C'5'      CHECK FOR 5                           04500000
         BNE   IOER1              BAD, GO TO NEXT VOL                   04510000
         LA    R9,ACCTABLE        LOAD ADDRESS OF ACCUMULATION TABLE    04520000
         SR    R1,R1              CLEAR ACCUM REG.                      04530000
NEXTDS   LA    R5,8                    8 FIELDS FIRST                   04540000
         LA    R4,DS5AVEXT        AREA OF EXTENTS                       04550000
         BAL   R14,NOTHER         DO FIRST 8 EXTENTS                    04560000
         LA    R5,18              LAST IN DSCB                          04570000
         LA    R4,DS5MAVET                                              04580000
         BAL   R14,NOTHER         DO LAST 18 EXTENTS                    04590000
         C     R5,DS5PTRDS+1      CHECK IF ONE MORE DSCB5 AROUND        04600000
         BE    ENDDSCB5           NO, QUIT                              04610000
         MVC   FIVE,DS1PTRDS   SET F5 KEY                               04620000
         BAL   R14,SRCHDSCB       GO SEARCH FOR DSCB 5                  04630000
         B     NEXTDS             KEEP GOING TILL END                   04640000
ENDDSCB5 MVC   0(4,R9),=F'0'      INDICATE END OF LIST BY ZERO          04650000
         LR    R3,R1              COUNT IN PARM AREA                    04660000
         LA    R1,SPALLOC         OUTPUT AREA                           04670000
         BAL   R14,EDIT           READY FOR PRINT                       04680000
         SPACE 2                                                        04690000
*        PRINT FREE SPACE VALUES                                        04700000
         SPACE                                                          04710000
         BAL   R14,PRINTSUB-4   & GO TO PRINT A LINE                    04720000
         AP    LINEKNT,=P'1'      COUNT IS CORRECT                      04730000
         MVC   DATANAME+11(5),=C'* * *'  SIGNAL AFTER FREE SPACE        04740000
         BAL   R14,PRNTEXTS       PRINT AND SORT ALL EXTENTS            04750000
         SPACE 2                                                        04760000
* START REAL DATA SET PRINT OUTS HERE, THRU WITH 4 AND 5                04770000
         SPACE                                                          04780000
DOF1S    L     R4,NTRKNT   CHECK FOR NO FORMAT 1'S                      04790000
         BCT   R4,FINEST                                                04800000
         B     MAPZ               NO F1'S SO DO MAP                     04810000
FINEST   ST    R4,NTRKNT          STORE CORRECT VALUE FOR COUNT         04820000
         LA    R12,DSTBL          PLACE OF FIRST DSCB F1                04830000
DONXTF1  L     R10,0(R12)         ADDRESS OF DSCB IS IN                 04840000
         NI    SPUSEDSW+1,X'00'   RESET 'SPACED USED' BYPASS SWITCH     04850000
         OI    SWPDS+1,X'F0'   SET 'DIRECTORY LOOK-UP' BYPASS SWITCH    04860000
         MVC   DATANAME,DS1DSNAM  BRING IN NAME OF DATA SET             04870000
         LA    R1,DS1CREDT        LOAD ADDRESS OF DATE                  04880000
         LA    R2,CRDATE          PLACE TO STORE                        04890000
         BAL   R14,CALEN          GO DO CALENDER                        04900000
         LA    R1,DS1EXPDT        PICKUP                                04910000
         LA    R2,PURGDATE        STORE                                 04920000
         BAL   R14,CALEN                                                04930000
         TM    DS1DSORG,X'80'          CHECK FOR ISAM                   04940000
         BZ    TYPX04                                                   04950000
         MVC   TYPESET,=C'INDX'                                         04960000
         OI    SPUSEDSW+1,X'F0'   SET 'SPACE USED' BYPASS SWITCH        04970000
         B     TYPON                                                    04980000
TYPX04   TM    DS1DSORG,X'20'          CHECK FOR DIRECT                 04990000
         BZ    TYPX06                                                   05000000
         MVC   TYPESET,=C'DIR.'                                         05010000
         B     TYPON                                                    05020000
TYPX06   TM    DS1DSORG,X'40'   CHECK FOR PHYSICAL SEQUENTIAL           05030000
         BZ    TYPX1                                                    05040000
         MVC   TYPESET,SEQ        SEQUENTIAL DATA SET                   05050000
         B     TYPON                                                    05060000
TYPX2    NI    SWPDS+1,X'0F'      SET TO GO THRU PDS                    05070000
         MVC   TYPESET,=C'PART'   PARTITIONED                           05080000
         B     TYPON                                                    05090000
TYPX1    TM    DS1DSORG,X'02'     CHECK FOR PARTITIONED                 05100000
         BO    TYPX2                                                    05110000
         MVC   TYPESET,WARNING+7   NOT DEFINED                          05120000
TYPON    SR    R3,R3                                                    05130000
         IC    R3,DS1NOEPV        GET EXTENT NO.                        05140000
         ST    R3,EXTKNT          STORE EXTENT VALUE FOR REST OF CALCUL 05150000
         CVD   R3,WORK                                                  05160000
         UNPK  EXTENTS,WORK+6(2)  TAKE OUT EXTENTS                      05170000
         OI    EXTENTS+1,X'F0'    MAKE GOOD ZONE                        05180000
         MVC   SERIALNO,DS1DSSN   MOVE SERIAL NUMBER                    05190000
         MVC   WORK(2),DS1VOLSQ   SEQUENCE NO IN HALF WORD              05200000
         LH    R3,WORK                                                  05210000
         CVD   R3,WORK                                                  05220000
         UNPK  SEQNO,WORK+6(2)    VOLUME SEQUENCE NUMBER                05230000
         OI    SEQNO+1,X'F0'      PUT IN SEQ WITH ZONE                  05240000
*                                 CHECK SECURITY                        05250000
         MVC   SECURITY(3),=C'NO '    PUT NO IN           PES0282       05260000
         TM    DS1DSIND,DS1IND40   TEST FOR RACF          PES0282       05270000
         BZ    RACFOFF                                    PES0282       05280000
         MVC   SECURITY(3),=C'YES'                        PES0282       05290000
RACFOFF  EQU   *                                          PES0282       05300000
         LA    R1,DS1EXT1   LOAD 1ST EXTENT ADDRESS                     05310000
         BAL   R4,CONV   & GO TO CONVERT TO TRACKS                      05320000
         STH   R3,EXTS   SAVE 1ST EXTENT BASE                           05330000
         LA    R1,10(R1)   LOAD 2ND EXTENT ADDRESS                      05340000
         BAL   R4,CONV   & GO TO CONVERT TO TRACKS                      05350000
         STH   R3,EXTS+2   SAVE 2ND EXTENT BASE                         05360000
         CLI   0(R1),X'00'   IS THERE ONLY 1 EXTENT                     05370000
         BNE   *+10   IF NO, BYPASS DUMMYING OVERFLOW TO 1 CYLINDER     05380000
         MVC   DS1EXT3+2(2),DS1EXT1+6   SET LAST PRIME CYLINDER AS THE X05390000
               OVERFLOW AREA BASE                                       05400000
         LA    R1,10(R1)   LOAD 3RD EXTENT ADDRESS                      05410000
         BAL   R4,CONV   & GO TO CONVERT TO TRACKS                      05420000
         STH   R3,EXTS+4   SAVE 3RD EXTENT BASE                         05430000
         MVC   FIVE,DS1PTRDS   SET F2 OR F3 KEY                         05440000
         LA    R9,ACCTABLE        NOW READY TO FIND PRIMARY ALLOCATION  05450000
         MVC   SPUSED,BLANK23                                           05460000
SPUSEDSW NOP   SEC3                                                     05470000
         CLC   DS1LSTAR(5),=6X'0'   ARE FIELDS VALID                    05480000
         BE    SEC3                    NO                               05490000
         LH    R3,DS1LSTAR        RELATIVE NUMBER OF TRACKS USED        05500000
         CLI   DS1LSTAR+2,0            ANY RECORDS USED ON TRK          05510000
         BE    *+8                     NO                               05520000
         LA    R3,1(R3)                YES - UP TRK COUNT               05530000
         LA    R1,SPUSED          OUT AREA                              05540000
         BAL   R14,EDIT           PUT OUT USED VALUE                    05550000
SEC3     SR    R11,R11   CLEAR ACCUM REG                                05560000
         LA    R1,DS1EXT1         F1 FIRST EXTENT                       05570000
         L     R5,EXTKNT          NUMBER OF EXTENTS                     05580000
         LTR   R5,R5              TEST FOR AN EMPTY DATA SET            05590000
         BZ    EXTSS3             JUMP IF EMPTY                         05600000
         C     R5,=F'4'           CHECK FOR ALL EXTENTS IN F1           05610000
         BL    EXTSS1                                                   05620000
         LA    R5,3               SET FOR LAST 3                        05630000
EXTSS1   BAL   R14,ALLSUM         SUM UP ALLOCATION                     05640000
         CLI   DS1NOEPV,4         CHECK FOR F3 TO BE CHECKED            05650000
         BL    EXTSS3             THRU                                  05660000
SRCHF3   BAL   R14,SRCHDSCB   GET NEW FORMAT 3 DSCB                     05670000
         CLI   DS3FMTID,C'3'           IS THIS A FORMAT 3 DSCB          05680000
         BE    FOUNDF3                 YES - GO COMPUTE EXTENTS         05690000
         MVC   FIVE,DS3PTRDS   SET F3 KEY                               05700000
         B     SRCHF3                                                   05710000
CALC     LTR   R0,R0   IS THE ENTRY BLANK                               05720000
         BCR   8,R4   IF YES, EXIT                                      05730000
         LA    R3,EXTS+4   LOAD EXTENTS END ADDRESS                     05740000
CALC1    CH    R0,0(R3)   IS THE ENTRY IN THIS EXTENT                   05750000
         BNL   CALC2   IF NO, GO TO CALCULATE USAGE IN TRACKS           05760000
         SH    R3,=H'2'   SHIFT TO LOWER EXTENT                         05770000
         B     CALC1   & RETURN TO THE CHECK                            05780000
CALC2    SH    R0,0(R3)   ENTRY - BASE = USAGE                          05790000
         A     R0,WORK   ACCUMULATE                                     05800000
         ST    R0,WORK   & STORE                                        05810000
         BR    R4   EXIT                                                05820000
FOUNDF3  LA    R1,DS3EXTNT   START OF GROUP                             05830000
         L     R5,EXTKNT          EXTENT COUNT BACK                     05840000
         SH    R5,=H'3'   REDUCE BY THOSE ALREADY DONE                  05850000
         CH    R5,=H'5'   FOUR MORE FIELDS                              05860000
         BL    EXTSS2                                                   05870000
         LA    R5,4               SET TO 4                              05880000
EXTSS2   BAL   R14,ALLSUM         SUM UP SOME MORE                      05890000
         L     R5,EXTKNT          LOAD IN COUNT                         05900000
         SH    R5,=H'7'   SET TO DO LAST NINE                           05910000
         BNP   EXTSS3             THRU                                  05920000
         LA    R1,DS3ADEXT                                              05930000
         BAL   R14,ALLSUM         DO LAST OF THEM                       05940000
EXTSS3   LR    R3,R11             PUT IN OUTPUT AREA                    05950000
         LA    R1,SPALLOC         TOTAL ALLOCATION                      05960000
         BAL   R14,EDIT           PUT OUT TOTAL                         05970000
         MVC   0(4,R9),=F'0'      INDICATE END OF LIST BY ZERO          05980000
         L     R10,0(R12)         POINT BACK TO F1                      05990000
         CLI   TYPESET,C'I'   IS THIS AN ISAM FILE                      06000000
         BNE   SWPDS   IF NO, BYPASS USED SPACE CALCULATION             06010000
         MVI   EXTS+6,X'00'   CLEAR ISAM                                06020000
         MVC   EXTS+7(5),EXTS+6   CALCULATION AREA                      06030000
SHIFT    CLC   EXTS(2),EXTS+2   IS 2ND EXTENT OUT OF SEQUENCE           06040000
         BH    SHIFT1   IF YES, GO TO RE-SEQUENCE                       06050000
         CLC   EXTS+2(2),EXTS+4   IS 3RD EXTENT OUT OF SEQUENCE         06060000
         BH    SHIFT2   IF YES, GO TO RE-SEQUENCE                       06070000
         MVC   FIVE,DS1PTRDS   SET F2 KEY                               06080000
         CLC   FIVE(4),FIVE+1   IS THE KEY BLANK                        06090000
         BE    *+16   IS YES, GO TO SET THE NO FORMAT 2 INDICATORS      06100000
         BAL   R14,SRCHDSCB   GET NEW FORMAT 2 DSCB                     06110000
         CLI   DS3FMTID,C'2'   IS THIS A FORMAT 2 DSCB                  06120000
         BE    FOUND2   IF YES, GO TO LOCATE LAST RECORDS               06130000
         SR    R3,R3   CLEAR SPACE USED COUNT                           06140000
         B     SRCHF2N   & BYPASS                                       06150000
SHIFT1   MVC   EXTS+6(2),EXTS   SAVE A                                  06160000
         MVC   EXTS(2),EXTS+2   SHIFT B                                 06170000
         MVC   EXTS+2(2),EXTS+6   SHIFT A                               06180000
         B     SHIFT   & RETURN TO SEQUENCE CHECK                       06190000
SHIFT2   MVC   EXTS+6(2),EXTS+2   SAVE B                                06200000
         MVC   EXTS+2(2),EXTS+4   SHIFT C                               06210000
         MVC   EXTS+4(2),EXTS+6   SHIFT B                               06220000
         B     SHIFT   & RETURN TO SEQUENCE CHECK                       06230000
FOUND2   LA    R1,DS3FMTID+50   LOAD ADDRESS OF LAST PRIME RECORD       06240000
         BAL   R4,CONV   & GO TO CONVERT TO TRACKS                      06250000
         STH   R3,EXTS+6   SAVE LAST PRIME                              06260000
         LA    R1,DS3FMTID+60   LOAD ADDRESS OF LAST INDEX RECORD       06270000
         BAL   R4,CONV   & GO TO CONVERT TO TRACKS                      06280000
         STH   R3,EXTS+8   SAVE LAST INDEX                              06290000
         LA    R1,DS3FMTID+73   LOAD ADDRESS OF LAST OVERFLOW RECORD    06300000
         BAL   R4,CONV   & GO TO CONVERT TO TRACKS                      06310000
         STH   R3,EXTS+10   SAVE LAST OVERFLOW                          06320000
         SR    R0,R0   CLEAR USED SPACE CALCULATOR                      06330000
         CLI   DS3FMTID+56,X'00'   IS THIS THE PRIME TRACK BASE         06340000
         BE    *+8   IF YES, GO TO INDEX CHECK                          06350000
         A     R0,=F'1'   AUGMENT NUMBER OF TRACKS USED                 06360000
         CLI   DS3FMTID+71,X'00'   IS THIS THE INDEX TRACK BASE         06370000
         BE    *+8   IF YES, GO TO OVERFLOW CHECK                       06380000
         A     R0,=F'1'   AUGMENT NUMBER OF TRACKS USED                 06390000
         CLI   DS3FMTID+79,X'00'   IS THIS THE OVERFLOW TRACK BASE      06400000
         BE    *+8   IF YES, GO TO SAVE TRACKS USED                     06410000
         A     R0,=F'1'   AUGMENT NUMBER OF TRACKS USED                 06420000
         ST    R0,WORK   SAVE NUMBER OF TRACKS USED                     06430000
         LH    R0,EXTS+6   LOAD LAST PRIME TTTT                         06440000
         BAL   R4,CALC   & GO TO FIGURE USAGE                           06450000
         LH    R0,EXTS+8   LOAD LAST INDEX TTTT                         06460000
         BAL   R4,CALC   & GO TO FIGURE USAGE                           06470000
         LH    R0,EXTS+10   LOAD LAST OVERFLOW TTTT                     06480000
         BAL   R4,CALC   & GO TO FIGURE USAGE                           06490000
         LR    R3,R0   SET-UP EDIT NUMBER                               06500000
SRCHF2N  LA    R1,SPUSED   & OUTPUT AREA                                06510000
         BAL   R14,EDIT   THEN GO TO DISPLAY SPACE USED                 06520000
         L     R10,0(R12)   POINT BACK TO F1                            06530000
SWPDS    B     NOPDS   SWITCH FOR DIRECTORY SEARCH                      06540000
         SPACE 2                                                        06550000
*        READ PDS DIRECTORY                                             06560000
         SPACE                                                          06570000
         RDJFCB    (UTILITY)      READ IN PARMS                         06580000
         MVC   JFCBDSNM(44),DS1DSNAM                                    06590000
         MVC   JFCBCRDT(6),DS1CREDT  MOVE IN BOTH DATES                 06600000
         MVI   JFCBNV0L,1         ONE VOLUME                            06610000
         MVC   JFCBV0LS(6),SAVESER   PUT IN VOLUME SERIAL NUMBER        06620000
         OI    JFCBTSDM,X'08'   SET AS PER INSTRUCTIONS IN SYS PROG     06630000
*                          GUIDE TO NOT UPDATE LAST REFERENCE DATE      06640000
         L     R5,SV5                                                   06650000
         OPEN  (UTILITY,(INPUT)),TYPE=J                                 06660000
         SR    R3,R3              CLEAR NO. USED REG                    06670000
         SR    R4,R4              CLEAR TOTAL SIZE REG                  06680000
         SR    R2,R2              JUST ZERO FOR TEST                    06690000
PDSLOOP  GET   UTILITY,WORK                                             06700000
         CH    R2,WORK            SEE IF BLOCK IS USED                  06710000
         BZ    PDSLOP             IF ZERO, AVAILABLE                    06720000
         LA    R3,1(R3)   BOOST BY ONE                                  06730000
PDSLOP   LA    R4,1(R4)   BOOST BY ONE                                  06740000
         B     PDSLOOP            MERRY GO ROUND TILL EOV (DS END)      06750000
PDSEND   LA    R1,DRBLUSED                                              06760000
         BAL   R14,EDIT           BLOCKS USED                           06770000
         LR    R3,R4                                                    06780000
         LA    R1,DRBLOCKS                                              06790000
         BAL   R14,EDIT           TOTAL NUMBER OF BLOCKS                06800000
         CLOSE (UTILITY)          READY FOR NEXT ROUND                  06810000
NOPDS    LA    R12,4(R12)         READY FOR NEXT F1                     06820000
         BAL   R14,PRINTSUB-4   & GO TO PRINT A LINE                    06830000
         MVC   DRBLOCKS(11),DRBLOCKS-2  CLEAR LAST PART OF LINE         06840000
         AP    LINEKNT,=P'1'      CHECK PAGE LENGTH                     06850000
         CP    LINEKNT,=P'57'     56 LINES PER PAGE                     06860000
         BL    TOE                                                      06870000
         BAL   R14,HEADING        NEW PAGE                              06880000
         BAL   14,NEWPAGE              PRINT HEAD3 AND HEAD4            06890000
TOE      LA    R5,DSORG                                                 06900000
         MVC   0(5,R5),BLANK23   CLEAR AREA FOR 'DSORG'                 06910000
         TM    DS1DSORG,X'80'          TEST ISAM                        06920000
         BZ    CHBIT0                                                   06930000
         MVC   0(2,R5),=C'IS'                                           06940000
         LA    R5,2(R5)                                                 06950000
CHBIT0   TM    DS1DSORG,X'40'   SEE IF BIT IS THERE                     06960000
         BZ    CHBIT1                                                   06970000
         MVC   0(2,R5),=C'PS'     PUT IN TYPE                           06980000
         LA    R5,2(R5)                                                 06990000
CHBIT1   TM    DS1DSORG,X'20'     PUT CHECK ON NEXT BIT                 07000000
         BZ    CHBIT2                                                   07010000
         MVC   0(2,R5),DA         DIRECT ACCESS                         07020000
         LA    R5,2(R5)                                                 07030000
CHBIT2   TM    DS1DSORG,X'02'     CHECK MORE BITS                       07040000
         BZ    CHBIT3                                                   07050000
         MVC   0(2,R5),=C'PO'     PARTITIONED                           07060000
         LA    R5,2(R5)                                                 07070000
CHBIT3   TM    DS1DSORG,X'01'                                           07080000
         BZ    CHBIT4                                                   07090000
         MVI   0(R5),C'U'         UNMOVABLE                             07100000
CHBIT4   LA    R5,RECFM                                                 07110000
         MVC   0(5,R5),BLANK23   CLEAR AREA FOR 'RECFM'                 07120000
         TM    DS1RECFM,X'C0'                                           07130000
         BZ    CHBIT8             NOTHING AT ALL - GO OUT               07140000
         BM    CHBIT5                                                   07150000
         MVI   0(R5),C'U'         PUT IN UNDEFINED                      07160000
CHBIT6   LA    R5,1(R5)                                                 07170000
         B     CHBIT8                                                   07180000
CHBIT5   TM    DS1RECFM,X'80'     CHECK FIXED                           07190000
         BZ    CHBIT7                                                   07200000
         MVI   0(R5),C'F'         FIXED FORMAT                          07210000
         B     CHBIT6                                                   07220000
CHBIT7   MVI   0(R5),C'V'         VARIABLE FORMAT                       07230000
         B     CHBIT6                                                   07240000
CHBIT8   TM    DS1RECFM,X'20'     CHECK TRACK OVERFLOW                  07250000
         BZ    CHBIT9                                                   07260000
         MVI   0(R5),C'T'         TRACK OVERFLOW                        07270000
         LA    R5,1(R5)                                                 07280000
CHBIT9   TM    DS1RECFM,X'10'     CHECK                                 07290000
         BZ    CHBIT10                                                  07300000
         MVI   0(R5),C'B'         BLOCKED                               07310000
         LA    R5,1(R5)                                                 07320000
CHBIT10  TM    DS1RECFM,X'08'     CHECK NEXT BIT                        07330000
         BZ    CHBIT11                                                  07340000
         MVI   0(R5),C'S'         STANDARD BLOCKS                       07350000
         LA    R5,1(R5)                                                 07360000
CHBIT11  TM    DS1RECFM,X'04'                                           07370000
         BZ    CHBIT12                                                  07380000
         MVI   0(R5),C'A'         ASA CONTROL CHARACTER                 07390000
         B     CHBIT13                                                  07400000
CHBIT14  LH    R3,DS1SCALO+2   GET 2ND ALLOCATION                       07410000
         MH    R3,TRKPRCYL             MAKE TRACKS                      07420000
         B     CHBIT15            GO IN                                 07430000
CHBIT12  TM    DS1RECFM,X'02'                                           07440000
         BZ    CHBIT13                                                  07450000
         MVI   0(R5),C'M'         MACHINE CONTROL CHARACTER             07460000
CHBIT13  LH    R3,DS1LRECL        BRING IN RECORD LENGTH                07470000
         LA    R2,LRECL           ADDRESS TO STORE                      07480000
         BAL   R14,LFTJXT         PUT IN SYMBOLS LEFT JUSTIFIED         07490000
         MVC   EXTOUT+6(33),TYPESORG  DSORG OF F1                       07500000
         BAL   R14,LINE1OUT                                             07510000
         LH    R3,DS1BLKL         BLOCK SIZE                            07520000
         LA    R2,BLKSIZE                                               07530000
         BAL   R14,LFTJXT         SYMBOLS IN                            07540000
         TM    DS1SCALO,X'C0'     CHECK FOR CYLINDER                    07550000
         BO    CHBIT14            IT IS IF JUMPS                        07560000
         LH    R3,DS1SCALO+2      GET 2ND ALLOCATION                    07570000
CHBIT15  LA    R2,ALL2ND                                                07580000
         BAL   R14,LFTJXT                                               07590000
         MVC   EXTOUT+6(33),TYPESIZE  SIZES OF RECORDS OF F1            07600000
         BAL   R14,LINE1OUT                                             07610000
         LR    R1,R10             LOCATION IN PARM                      07620000
         FREEMAIN  R,LV=160,A=(1)   FREE UP SPACE USED                  07630000
         BAL   R14,PRNTEXTS       PRINT AND SORT ALL EXTEXTS            07640000
         L     R4,NTRKNT          LOAD COUNT OF F1'S                    07650000
         S     R4,=F'1'                                                 07660000
         ST    R4,NTRKNT                                                07670000
         BP    DONXTF1            GO BACK FOR NEXT F1                   07680000
         SPACE 2                                                        07690000
* START OF ALLOCATION MAP                                               07700000
         SPACE                                                          07710000
MAPZ     MVC   DATANAME(132),OUTBIN   CLEAR FULL LINE                   07720000
         L     R5,SRTKNT          LOAD UP NO OF EXTENTS                 07730000
         BCTR  R5,0               CORRECT COUNT                         07740000
         LA    R9,SRTBL      START OF SORT BIN                  PES0678 07750000
MAP3     BAL   R14,HEADING        TITLE                                 07760000
         MVC   EXTOUT+20(42),HEAD2  PUT IN HEADING                      07770000
         BAL   R14,LINE0OUT                                             07780000
         LA    R0,TEXTT   LOAD ADDRESS OF OUTPUT AREA                   07790000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      07800000
         AP    LINEKNT,=P'2'                                            07810000
MAP2     L     R10,0(R9)          BRING IN ADDRESS STORED IN TABLE      07820000
         L     R2,44(R10)         BRING IN SIZE                         07830000
         BAL   R14,LENGTH         LENGTH                                07840000
         CVD   R3,WORK                                                  07850000
         UNPK  LNGTH,WORK+5(3)    PUT OUT LENGTH                        07860000
         LH    R3,HOLD            FIRST TRACK                           07870000
         CVD   R3,WORK                                                  07880000
         UNPK  STRTT,WORK+5(3)    START TRACK                           07890000
         LH    R3,HOLD+2          LAST TRACK                            07900000
         CVD   R3,WORK            CONVERT TO DECIMAL                    07910000
         UNPK  LSTT,WORK+5(3)     LAST TRACK                            07920000
         SR    R3,R3                                                    07930000
         IC    R3,48(R10)         EXTENT VALUE                          07940000
         CVD   R3,WORK                                                  07950000
         UNPK  EXTT,WORK+6(2)     PUT OUT EXTENT VALUE                  07960000
         OI    LNGTH+4,X'F0'      MAKE TO PROPER ZONE                   07970000
         OI    STRTT+4,X'F0'      MAKE TO PROPER ZONE                   07980000
         OI    LSTT+4,X'F0'       ZONE IT                               07990000
         OI    EXTT+1,X'F0'                                             08000000
         MVC   SHLINE,0(R10)      BRING IN DATA SET NAME                08010000
         LA    R0,MAPS   LOAD ADDRESS OF OUTPUT AREA                    08020000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      08030000
         LR    R1,R10             LOCATION IN PARM REG                  08040000
         FREEMAIN  R,LV=56,A=(1)   FREE SORT STORE                      08050000
         AP    LINEKNT,=P'1'                                            08060000
         LA    R9,4(R9)           READY FOR NEXT LINE                   08070000
         BCT   R5,MAP1                                                  08080000
         SPACE 2                                                        08090000
*  THROUGH WITH THIS VOLUME, PREPARE TO GO TO NEXT                      08100000
         SPACE                                                          08110000
         LA    R5,F356TBL    LOAD ADDRESS OF TABLE              PES0678 08120000
FREE2356 L     R1,8(R5)                ADDR OF 1ST DSCB                 08130000
         FREEMAIN R,LV=160,A=(1)       FREE IT                          08140000
         LA    R5,12(R5)               BUMP TO NEXT TBL ENTRY           08150000
         CR    R5,R7                   TEST TABLE END                   08160000
         BL    FREE2356                NOT YET                          08170000
         B     DISKLOOP           GO LOOK FOR NEXT VOL                  08180000
MAP1     CP    LINEKNT,=P'57'     CHECK FOR END OF PAGE                 08190000
         BL    MAP2               GO DO NEXT LINE                       08200000
         B     MAP3               DO HEADING                            08210000
         SPACE 2                                                        08220000
*        PRINT TERMINATION MESSAGE                                      08230000
         SPACE                                                          08240000
ALLDONE  CLOSE (PRINT)                                                  08250000
         SR    15,15                                                    08260000
GOBCK    EQU   *                                                        08270000
         L     R5,SV5                                                   08280000
         LR    R2,R13                                           PES0678 08290000
         L     R13,SAVEAREA+4                                   PES0678 08300000
         FREEMAIN R,LV=WRKLNGTH,A=(R2)                          PES0678 08310000
         RETURN (14,12),RC=(15)                                 PES0678 08320000
         SPACE 2                                                        08330000
PDSERR   MVC   ERRNAME(44),DS1DSNAM                                     08340000
         LA    R0,IOERROR   LOAD ADDRESS OF OUTPUT AREA                 08350000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      08360000
         MVC   EXTOUT(57),=C'0THEREFORE THE NUMBER OF BLOCKS SHOWN WILL*08370000
                NOT BE CORRECT'                                         08380000
         BAL   R14,LINE0OUT                                             08390000
         B     PDSEND                  GO FINISH MAP                    08400000
ABTOREL  MVC   HOLD(8),2(R1)      MOVE FOR BOUNDRIES                    08410000
         LH    R2,HOLD            CC                                    08420000
         MH    R2,TRKPRCYL                                              08430000
         AH    R2,HOLD+2          HH                                    08440000
ABTOREL1 LH    R3,HOLD+4          CC                                    08450000
         MH    R3,TRKPRCYL                                              08460000
         AH    R3,HOLD+6          HH                                    08470000
         SLL   R2,16              MOVE TO TOP                           08480000
         ALR   R2,R3              PUT TOGETHER                          08490000
         BR    R14                EXIT                                  08500000
ALLSUM   ST    R14,HOLD+8         TAKE CARE OF FETCHING ALLOCATIONS     08510000
ALLSUM1  EQU   *                                                        08520000
         BAL   R14,ABTOREL                                              08530000
         ST    R2,0(R9)           PUT IN TABLE                          08540000
         LA    R9,4(R9)           NEXT READY                            08550000
         BAL   R14,LENGTH         GET LENGTH IN R3                      08560000
         AR    R11,R3             TOTAL UP ALL LENGTHS FOR DS           08570000
         LA    R1,10(R1)          SET FOR NEXT                          08580000
         BCT   R5,ALLSUM1         LOOP                                  08590000
         L     R14,HOLD+8         RESTORE                               08600000
         BR    R14                BACK OUT                              08610000
CALEN    SR    R3,R3              COMPUTE YEARS AND DAYS                08620000
         IC    R3,0(R1)           PICK UP YEAR                          08630000
         CVD   R3,WORK                                                  08640000
         UNPK  0(2,R2),WORK+6(2)  YEAR                                  08650000
         OI    1(R2),X'F0'        ZONE                                  08660000
         MVC   WORK(2),1(R1)      FETCH LAST TWO BYTES                  08670000
         LH    R3,WORK                                                  08680000
         CVD   R3,WORK            TO DECIMAL                            08690000
         UNPK  2(3,R2),WORK+6(2)  DAYS                                  08700000
         OI    4(R2),X'F0'        ZONE                                  08710000
         BR    R14                RETURN                                08720000
EDIT     CVD   R3,WORK            MAKE IT DECIMAL                       08730000
         SH    R1,=H'2'   SET-UP CORRECT ADDRESS                        08740000
         MVC   0(L'PAT,R1),PAT    MOVE PATTERN                          08750000
         ED    0(L'PAT,R1),WORK+5 PUT IT IN SHAPE                       08760000
         BR    R14                EXIT                                  08770000
FULLEDIT CVD   R3,WORK            CONVERT TO DECIMAL                    08780000
         UNPK  ZEXT,WORK+6(2)     EXTENT NUMBER                         08790000
         ST    R14,HOLD+4         SAVE RETURN ADDRESS                   08800000
         BAL   R14,LENGTH         DETERMINE LENGTH                      08810000
         CVD   R3,WORK                                                  08820000
         UNPK  ZLNGTH,WORK+5(3)   ENTER TRACK LENGTH OR SIZE            08830000
         LH    R3,HOLD            FIRST TRACK                           08840000
         CVD   R3,WORK                                                  08850000
         UNPK  ZFIRST,WORK+5(3)   FIRST TRACK VALUE                     08860000
         LH    R3,HOLD+2          LAST                                  08870000
         CVD   R3,WORK                                                  08880000
         UNPK  ZLAST,WORK+5(3)    ENTER LAST TRACK VALUE                08890000
         OI    ZEXT+1,X'F0'       PUT IN PROPER ZONE                    08900000
         OI    ZLNGTH+4,X'F0'                                           08910000
         OI    ZFIRST+4,X'F0'     ZONE IN ALL                           08920000
         OI    ZLAST+4,X'F0'                                            08930000
         L     R14,HOLD+4         RESTORE RETURN ADDRESS                08940000
         BR    R14                                                      08950000
LENGTH   ST    R2,HOLD            SAVE VALUE FOR TEMP                   08960000
         LH    R3,HOLD+2          BRING IN LAST POSITION                08970000
         LA    R3,1(R3)   UPDATE                                        08980000
         SH    R3,HOLD            TAKE OFF START TRACK VALUE            08990000
         BR    R14                GO ON BACK                            09000000
LFTJXT   ST    R14,HOLD           SAVE EXIT                             09010000
         LA    R1,CALCU+2         TEMP WORK AREA                        09020000
         BAL   R14,EDIT           EDIT INTO TEMP                        09030000
LOOPBIT  CLI   0(R1),C' '         FIND FIRST NONSPACE                   09040000
         BNE   FOUNDX                                                   09050000
         LA    R1,1(R1)           CHECK NEXT CHARACTER                  09060000
         B     LOOPBIT                                                  09070000
FOUNDX   MVC   0(5,R2),0(R1)      LEFT JUSTIFY NUMERALS                 09080000
         L     R14,HOLD           EXIT BACK                             09090000
         BR    R14                                                      09100000
LINE0OUT MVI   SWIT,0             SET SWITCH TO NOT CHECK END OF PAGE   09110000
LINE1OUT ST    R14,HOLD+16        STORE RETURN ADDRESS                  09120000
         LA    R0,EXTOUT   LOAD ADDRESS OF OUTPUT AREA                  09130000
         BAL   R14,PRINTSUB   & GO TO PRINT A LINE                      09140000
         MVI   EXTOUT,C' '                                              09150000
         MVC   EXTOUT+1(70),EXTOUT   CLEAR LINE                         09160000
         TS    SWIT                                                     09170000
         BC    8,LINEOUT                                                09180000
         AP    LINEKNT,=P'1'                                            09190000
         CP    LINEKNT,=P'57'                                           09200000
         BL    LINEOUT                                                  09210000
         BAL   R14,HEADING        PUT OUT HEADING                       09220000
         BAL   14,NEWPAGE              PRINT HEAD3 AND HEAD4            09230000
LINEOUT  L     R14,HOLD+16   RESTORE                                    09240000
         BR    R14                                                      09250000
NOTHER   MVC   WORK(5),0(R4)      LOAD UP EXTENT VALUE                  09260000
         LH    R3,WORK+2                                                09270000
         MH    R3,TRKPRCYL                                              09280000
         SR    R2,R2                                                    09290000
         IC    R2,WORK+4                                                09300000
         AR    R3,R2              FIRST TRACK PLUS CYLINDER             09310000
         BZ    ENDDSCB5           DONE IF NOTHING THERE                 09320000
         AR    R1,R3              ACCUMULATE TOTAL                      09330000
         LH    R2,WORK                                                  09340000
         AR    R3,R2              STOP ADDRESS PLUS ONE                 09350000
         SLL   R2,16              MOVE TO LEFT                          09360000
         ALR   R2,R3              ALL TOGETHER                          09370000
         BCTR  R2,0               LESS ONE                              09380000
         ST    R2,0(R9)           PUT IT AWAY IN ACCUM                  09390000
         LA    R9,4(R9)                                                 09400000
         LA    R4,5(R4)           GO TO NEXT ONE                        09410000
         BCT   R5,NOTHER          GO DO FOR NEXT                        09420000
         BR    R14                                                      09430000
PRNTEXTS LA    R9,ACCTABLE        SET UP TABLE OF STORED EXTENTS        09440000
         MVC   EXTKNT,=F'0'       SET EXTENT COUNT TO 1                 09450000
         ST    R14,HOLD+8                                               09460000
         MVC   EXTOUT+11(24),HEAD5  EXT-FIRST----                       09470000
         BAL   R14,LINE0OUT                                             09480000
         AP    LINEKNT,=P'1'      LINE COUNT UP ONE                     09490000
PRNTXT1  L     R2,0(R9)           LOAD IN EXTENT SIZE                   09500000
         LTR   R2,R2              CHEKC FOR END OF LIST                 09510000
         BZ    JUMPS1             NO MORE LINES OUT                     09520000
         L     R3,EXTKNT          BRING EXTENT COUNT IN                 09530000
         LA    R3,1(R3)           BOOST                                 09540000
         ST    R3,EXTKNT          RESTORE                               09550000
         BAL   R14,STORMAP        STORE EXTENT FOR MAP OUTPUT           09560000
         L     R3,EXTKNT          COUNT BACK IN                         09570000
         BAL   R14,FULLEDIT       CONVERT AND DISPLAY IN EBCDIC         09580000
         LA    R9,4(R9)           SET UP TABLE FOR NEXT LINE            09590000
         MVC   EXTOUT+12(23),ZEXT   VALUES OF EXT-FIRST--LAST-LENGTH    09600000
         BAL   R14,LINE0OUT                                             09610000
         AP    LINEKNT,=P'1'      BOOST LINE COUNT BY ONE               09620000
         CP    LINEKNT,=P'57'     PRINT 56 LINES A PAGE                 09630000
         BL    PRNTXT1            JUMPT TO NEXT LINE                    09640000
         L     R2,0(R9)           LOAD NEXT POINTER                     09650000
         LTR   R2,R2              SEE IF NO MORE                        09660000
         BNE   JUMPS0             IF NO MORE THEN CLEAR TITLE           09670000
         MVC   DATANAME,OUTBIN    CLEAR LINE                            09680000
JUMPS0   BAL   R14,HEADING        JUMP TO NEW PAGE                      09690000
         BAL   14,NEWPAGE                                               09700000
         B     PRNTXT1                                                  09710000
JUMPS1   L     R14,HOLD+8         RESTORE RETURN ADDRESS                09720000
         BR    R14                                                      09730000
SRCHDSCB LA    R5,F356TBL    BRING IN START OF TABLE            PES0678 09740000
         LA    R5,12(R5)                 AFTER F5 DSCB                  09750000
NEXTETRY CLC   3(5,R5),FIVE   COMPARE CCHHR TO TABLE                    09760000
         BE    SETITUP            THERE                                 09770000
         LA    R5,12(R5)          READY FOR NEXT ONE                    09780000
         CR    R5,R7              TOP                                   09790000
         BL    NEXTETRY           CHECK FOR END                         09800000
         B     IOER2   NOT THERE, SO, ERROR                             09810000
SETITUP  L     R10,8(R5)          LOAD DSCB ADDRESS                     09820000
         BR    R14                EXIT                                  09830000
STORMAP  MVC   WORK(44),DATANAME  FETCH NAME                            09840000
         ST    R2,WORK+44         STORE TRACKS                          09850000
         STC   R3,WORK+48         STORE EXTENT NUMBER                   09860000
         GETMAIN   R,LV=56        FETCH STORE FOR EXTENT                09870000
         MVC   0(49,R1),WORK      STORE EXTENT TRACKS AND NAME          09880000
         LA    R3,SRTBL-4    ADDRESS OF TABLE                   PES0678 09890000
         L     R4,SRTKNT          NO. OF ENTRIES                        09900000
SORTE12  BCT   R4,SORTE11                                               09910000
         ST    R1,4(R3)           PUT ADDRESS AWAY                      09920000
SORTE14  L     R4,SRTKNT                                                09930000
         LA    R4,1(R4)           BOOST FOR NEXT ENTRY                  09940000
         ST    R4,SRTKNT                                                09950000
         BR    R14                EXIT                                  09960000
SORTE11  LA    R3,4(R3)           BOOST TO NEXT                         09970000
         L     R5,0(R3)           POINTER TP SORT MATERIAL              09980000
         CL    R2,44(R5)          LOOK AT REL TRACK                     09990000
         BH    SORTE12            JUMP AROUND IF TOO BIG                10000000
SORTE13  ST    R1,0(R3)           STORE ADDRESS IN TABLE                10010000
         LR    R1,R5              LAST ADDRESS                          10020000
         L     R5,4(R3)           NEXT ADDRESS                          10030000
         LA    R3,4(R3)           BOOST                                 10040000
         BCT   R4,SORTE13         MOVE ALL REST POINTERS UP             10050000
         ST    R1,0(R3)           CATCH LAST ONE                        10060000
         B     SORTE14            GO FINISH                             10070000
* SUB-ROUTINE FOR CONVERTING CCHH TO TTTT.                              10080000
CONV     MVC   WORK(4),2(R1)   SHIFT CCHH TO REGISTER BOUNDARY          10090000
         LH    R3,WORK   COMPUTE NUMBER OF CYLINDERS                    10100000
         MH    R3,=H'20'   X TRACKS PER CYLINDER                        10110000
         AH    R3,WORK+2   & ADD REMAINING TRACKS                       10120000
         BR    R4   EXIT                                                10130000
* SUB-ROUTINE FOR PRINTING ALL TEXTUAL DATA.                            10140000
         LA    R0,OUTBIN   LOAD ADDRESS OF OUTPUT AREA                  10150000
PRINTSUB ST    R14,WORD   SAVE THE RETURN ADDRESS                       10160000
         LA    R1,PRINT   LOAD THE AREA DCB ADDRESS                     10170000
         L     R15,48(0,1)   LOAD THE PUT ROUTINE ADDRESS               10180000
         BALR  R14,R15   & LINK TO THE PUT ROUTINE                      10190000
         L     R14,WORD   RESET THE RETURN ADDRESS                      10200000
         BR    R14   & RETURN TO THE MAIN ROUTINE                       10210000
PRNTEXIT CLC   62(2,R1),=6X'0'   IS BLKSIZE FILLED IN                   10220000
         BCR   7,R14   IF YES, RETURN                                   10230000
         MVI   63(R1),133              NO - SET TO 133                  10240000
         BR    R14                                                      10250000
         EJECT                                                  PES0678 10260000
         LTORG                                                          10270000
         PRINT NOGEN                                                    10280000
PRINT    DCB   DSORG=PS,MACRF=PMC,DDNAME=SYSPRINT,BFTEK=S,RECFM=FBA,   *10290000
               LRECL=133,EXLST=PRINTLST                                 10300000
         SPACE                                                          10310000
UTILITY  DCB   DSORG=PS,MACRF=GM,DDNAME=SYSUT1,BFTEK=S,RECFM=F,        110320000
               SYNAD=PDSERR,                                           *10330000
               BLKSIZE=256,LRECL=256,BUFNO=4,EODAD=PDSEND,EXLST=PDSJFCB 10340000
DCBDDNM2 EQU   UTILITY+40         WHERE DDNAME GOES                     10350000
         SPACE                                                          10360000
TEST     DCB   DSORG=PS,MACRF=GM,RECFM=F,BLKSIZE=140,BFTEK=S,          110370000
               EXLST=STRJFCB                                            10380000
TRKCNT   DC    AL1(0)             HALF WORD CHECK FOR NUMBER OF         10390000
SVCOUNT  DC    AL1(0)             DSCB'S PER TRACK                      10400000
LINEKNT  DC    PL2'0'             KEEP TRACK OF LINES PER PAGE          10410000
PAGEKNT  DC    PL2'0'                                                   10420000
PAT      DC    X'402020202120'    EDITING PATTERN - FOUR COLUMNS        10430000
PATTERN  DC    X'40202120'                                              10440000
SWIT     DC    X'FF'                                                    10450000
         DS    0D                                                       10460000
SAVEF4   DS    CL140              STORE F4 DSCB FOR MORE REFS           10470000
SV5      DS    F                                                        10480000
SV14     DS    F                                                        10490000
SV14HOLD DS    F                                                        10500000
UCBPTRAD DS    F                                                        10510000
PRINTLST DC    X'85'                   DCB EXIT                         10520000
         DC    AL3(PRNTEXIT)                                            10530000
EXTKNT   DC    F'0'               SAVE EXTENT COUNT OF F1               10540000
TTRVTOC  DC    1F'0'              SAVE TTR0 OF VTOC                     10550000
HOLD     DS    5F                 SAVE RETURN ADDRESS                   10560000
SRTKNT   DC    F'1'               ENTRIES IN SORT TABLE                 10570000
NTRKNT   DC    F'1'               NEXT ENTRY POSITION FOR DSCB F1       10580000
TIOT     DS    1F  SAVE TIOT   ADDRESS                                  10590000
TRKPRCYL DS    H                                                        10600000
SVUCBADR DS    XL3                                                      10610000
         DS    0F                                                       10620000
PDSJFCB  DC    X'87'              READ JFCB                             10630000
         DC    AL3(PDSAREA)                                             10640000
PDSAREA  DS    CL176                                                    10650000
JFCBDSNM EQU   PDSAREA                                                  10660000
JFCBTSDM EQU   PDSAREA+52                                    PES0282    10670000
JFCBCRDT EQU   PDSAREA+80                                               10680000
JFCBNV0L EQU   PDSAREA+117                                              10690000
JFCBV0LS EQU   PDSAREA+118                                              10700000
STRJFCB  DC    X'87'              ONE ENTRY ONLY                        10710000
         DC    AL3(JFCBAREA)                                            10720000
JFCBAREA DS    CL176                                                    10730000
JFCBNVOL EQU   JFCBAREA+117                                             10740000
JFCBVOLS EQU   JFCBAREA+118                                             10750000
LIST     CAMLST    SEEK,ADDRS,SAVESER,WORK                              10760000
         DS    0D                                                       10770000
SEEK     DC    AL3(0)             DUMMY PART                            10780000
ADDRS    DC    XL5'0'             ADDRESS PART                          10790000
DEB      DC    4F'0'              FAKE UP A DEB FOR TRACK CONVERSION    10800000
         DC    AL1(1)             ONE EXTENT                            10810000
         DC    AL3(0)                                                   10820000
         DC    1F'0'              NO PURGE                              10830000
         DC    X'0F'              ID                                    10840000
         DC    AL3(0)                                                   10850000
         DC    X'04'              EXTENT SCALE                          10860000
APPEN    DC    AL3(0)   APPENDAGE TABLE ADDRESS                         10870000
UCBXREF  DC    XL4'C0000000'      UCB ADDRESS - PLACED BY PROGRAM       10880000
BIN      EQU   *                       BIN NO. STORED BY PROG           10890000
         DC    3AL2(0)            STARTING ADDRESS                      10900000
         DC    X'FFFFFFFF7FFF'    LAST CCHH NUMBER POSSIBLE             10910000
WORK     DS    CL350              WORKING STORAGE FOR OBTAIN            10920000
         DS    0F                 ALIGN FOR SPS                         10930000
MSSAGE   DC    AL2(MSEND-*)                                             10940000
         DC    AL2(0)                                                   10950000
         DC    C' SER='                                                 10960000
SERMSG   DC    CL6' '                                                   10970000
         DC    C' PERMANENT I/O ERROR ON DASD. NO RECOVERY.'            10980000
MSEND    DC    CL80' '                                                  10990000
         DS    0C                                                       11000000
         DC    C'0'               ONE SPACE PLUS LINE                   11010000
OUTBIN   DC    C' '               OUT PUT BIN - NO SPACE                11020000
DATANAME DC    CL44'WHO'          DATASET NAME                          11030000
         DC    CL3' '             BLANK                                 11040000
CRDATE   DC    CL5' '             CREATED DATE                          11050000
         DC    CL3' '                                                   11060000
PURGDATE DC    CL5' '             PURGE DATE                            11070000
         DC    CL4' '                                                   11080000
TYPESET  DC    CL4' '             TYPE OF DATA SET (NOT,PART,SEQ)       11090000
         DC    CL5' '                                                   11100000
EXTENTS  DC    CL2' '             NUMBER OF EXTENTS                     11110000
         DC    CL5' '                                                   11120000
SERIALNO DC    CL6' '             SERIAL NUMBER                         11130000
         DC    CL5' '                                                   11140000
SEQNO    DC    CL2' '             SEQUENCE NUMBER                       11150000
         DC    CL5' '                                                   11160000
SECURITY DC    CL3' '             RACF BIT                PES0282       11170000
         DC    CL4' '                                                   11180000
SPALLOC  DC    CL4'2000'          SPACE ALLOCATED TRACKS TOTAL          11190000
         DC    CL4' '                                                   11200000
SPUSED   DC    CL4'1000'          SPACE USED - TOTAL TRACKS             11210000
         DC    CL4' '                                                   11220000
DRBLOCKS DC    CL4' '             BLOCKS IN DIRECTORY                   11230000
         DC    CL3' '                                                   11240000
DRBLUSED DC    CL4' '             NUMBER OF DIRECTORY BLOCKS USED       11250000
TYPESORG DC    CL6'DSORG='                                              11260000
DSORG    DC    CL11'     RECFM='                                        11270000
RECFM    DC    CL11'     LRECL='                                        11280000
LRECL    DC    CL5' '                                                   11290000
TYPESIZE DC    CL8'BLKSIZE='                                            11300000
BLKSIZE  DC    CL21'      2ND ALLOCATION='                              11310000
ALL2ND   DC    CL4' '                                                   11320000
CALCU    DC    CL10' '                                                  11330000
EXTOUT   DC    CL133' '           SPACE FOR OUTPUT LINES                11340000
ZEXT     DC    CL2' '             EXTENT NUMBER                         11350000
         DC    CL2' '                                                   11360000
ZFIRST   DC    CL5' '                  FIRST TRACK HERE                 11370000
         DC    CL2' '                                                   11380000
ZLAST    DC    CL5' '                  LAST TRACK NEXT                  11390000
         DC    CL2' '                                                   11400000
ZLNGTH   DC    CL5' '                  LENGTH OF EXTENT                 11410000
MAPS     DC    CL3' '                                                   11420000
STRTT    DC    CL5' '                  START TRACK                      11430000
         DC    CL9' '                                                   11440000
LSTT     DC    CL5' '                  LAST TRACK                       11450000
         DC    CL9' '                                                   11460000
LNGTH    DC    CL5' '                  LENGTH OF EXTENT                 11470000
         DC    CL7' '                                                   11480000
EXTT     DC    CL2' '             EXTENT NUMBER                         11490000
         DC    CL7' '                                                   11500000
SHLINE   DC    CL44' '            DATA SET NAME                         11510000
         DC    CL37' '            REST ARE BLANK                        11520000
HEAD1    DC    C'1'   SKIP TO TOP OF PAGE                               11530000
DAY      DS    CL2   AS OF DAY                                          11540000
         DC    C' '                                                     11550000
MONTH    DS    CL3   AS OF MONTH                                        11560000
         DC    C' '                                                     11570000
YEAR     DS    CL4   AS OF YEAR                                  JLM    11580000
         DC    C' / '                                                   11590000
TIME     DS    CL4   AS OF TIME OF DAY                                  11600000
         DC    CL7' '                                                   11610000
         DC    CL52'AFDSC/CMI UTILITY - DASD ALLOCATION MAP'            11620000
         DC    CL16'UPDATED MAR 2004'                            JLM    11630000
BLANK23  DC    CL23' '                                                  11640000
         DC    CL5'PAGE'                                                11650000
PAGEPLC  DC    CL4' '             PAGE COUNT HERE                       11660000
         DC    CL9' '                                                   11670000
HEAD2    DC    CL23'CONTENTS ON VOLUME=SER='                            11680000
SAVESER  DC    CL6'SYSRES'                                              11690000
         DC    C'   UNIT='                                              11700000
UNIT     DC    CL3' '                                                   11710000
SUBCEL   DC    CL2' '                                                   11720000
HEAD3    DC    CL49' '                                          PES0678 11730000
DA       DC    CL7'DATE'                                        PES0678 11740000
         DC    CL9'DATE'                                                11750000
         DC    CL17'FILE'                                               11760000
         DC    CL9'FILE'                                                11770000
         DC    CL15'VOL.'                                               11780000
         DC    CL7'TOTAL'                                               11790000
         DC    CL8'TRACKS'                                              11800000
         DC    CL8'DIREC.'                                              11810000
         DC    CL4'BLKS'                                                11820000
HEAD4    DC    CL16' '                                                  11830000
         DC    CL31'DATA SET NAME'                                      11840000
         DC    CL9'CREATED'                                             11850000
         DC    CL9'PURGE'                                               11860000
         DC    CL7'TYPE'                                                11870000
         DC    CL9'EXTENTS'                                             11880000
         DC    CL10'SERIAL'                                             11890000
SEQ      DC    CL5'SEQ.'                                                11900000
         DC    CL10'SECURITY'                                           11910000
         DC    CL8'ALLOC'                                               11920000
         DC    CL7'USED'                                                11930000
         DC    CL8'BLOCKS'                                              11940000
         DC    CL4'USED'                                                11950000
HEAD5    DC    CL24'EXT--FIRST---LAST-LENGTH'                           11960000
WARNING  DC    CL31'VOLUME NOT MOUNTED OR AVAILABLE'                    11970000
TEXTT    DC    CL133' FIRST TRACK   LAST TRACK      LENGTH    EXTENT   111980000
                     DATA SET NAME'                                     11990000
IOERROR  DC    CL133' I/O ERROR ENCOUNTERED IN READING DIRECTORY FOR'   12000000
ERRNAME  EQU   IOERROR+48                                               12010000
DCBDDNM  EQU   TEST+40                                                  12020000
WORD     DS    F   DATE/TIME WORK AREA                                  12030000
DOUBLE   DS    D   DATE/TIME WORK AREA                                  12040000
EXTS     DS    6H   ISAM EXTENT COMPUTATION AREA                        12050000
FIVE     DS    CL5    DATE/TIME WORK AREA                               12060000
EIGHT    DS    CL8    DATE WORK AREA                             JLM    12061000
NOUTIL   DC    C'-NO SYSUT1 DD, JOB TERMINATED'                         12070000
MONTHS   DC    P'31',C'JAN',P'28',C'FEB',P'31',C'MAR',P'30',C'APR'      12080000
         DC    P'31',C'MAY',P'30',C'JUN',P'31',C'JUL',P'31',C'AUG'      12090000
         DC    P'30',C'SEP',P'31',C'OCT',P'30',C'NOV',P'31',C'DEC'      12100000
EXTNTBL  DC    H'0'                    TOTAL OF 30 CCHHR ALLOWED        12110000
         DC    30X'0000000000'                                          12120000
JOBNAME  DS    F                                                        12130000
ACCTABLE DS    200F          SAVE EXTENTS FOR ACCUMULATION      PES0282 12140000
         SPACE 3                                                        12150000
WORKSCTN DSECT                                                  PES0678 12160000
SAVEAREA DS    18F                                              PES0678 12170000
DSTBL    DS    756F               TOTAL OF 756 DSCB F1 ALLOWED  PES0678 12180000
F356TBL  DS    249F     TOTAL OF 83 DSCB'S F3-F5-F6 ALLOWED     PES0282 12190000
SRTBL    DS    1023F             1023 ENTRIES ON ONE DISK       PES0282 12200000
         DS    0D                                                       12210000
WRKLNGTH EQU   *-WORKSCTN                                               12220000
         SPACE 3                                                PES0678 12230000
DSCB4    DSECT                                                          12240000
         DC    44X'04'                                          PES0678 12250000
         IECSDSL1 (4)                                           PES0678 12260000
         ORG   DSCB4                                            PES0678 12270000
         IECSDSL1 (1)                                           PES0678 12280000
         ORG   DSCB4                                            PES0678 12290000
         IECSDSL1 (3)                                           PES0678 12300000
         ORG   DSCB4                                            PES0678 12310000
         IECSDSL1 (5)                                           PES0678 12320000
         SPACE 3                                                        12330000
UCB      DSECT                                                          12340000
         IEFUCBOB LIST=YES                                      PES0678 12350000
         SPACE 3                                                        12360000
CVT      DSECT                                                          12370000
         CVT LIST=YES                                           PES0678 12380000
       END                                                              12390000
/*                                                                      00090000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR  <== TARGET                 00100000
//LKED.SYSIN DD *                                                       00110000
  SETCODE AC(1)                                                         00120000
  NAME DISKMAP(R)                                                       00130000
/*                                                                      00140000
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=DISKMAP
//DISKMAP  JOB  (SETUP),
//             'Run DISKMAP',
//             CLASS=A,
//             MSGCLASS=H,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=0K
//********************************************************************
//*
//* Name: DISKMAP
//*
//* Desc: Run the DISKMAP program
//*
//********************************************************************
//DISKMAP EXEC PGM=DISKMAP,REGION=1024K                                 00030000
//SYSPRINT DD  SYSOUT=*                                                 00040000
//SYSUT1   DD  DSN=SYS1.PROCLIB,DISP=SHR                                00050000
//DD1      DD  DISP=OLD,UNIT=3350,VOL=SER=PUB001                        00060000
//                    