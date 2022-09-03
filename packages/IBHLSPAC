//IBHLSPAC  JOB (TSO),
//             'Install IBHLSPAC',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//IBHLSPAC EXEC ASMFCL,PARM='LIST,NODECK,LOAD,NOTERM'                   00020000
//ASM.SYSIN DD *                                                        00030000
         TITLE 'I B H L S P A C    DIRECT ACCESS VOLUME SPACE INFORMATIX00040000
               ON'                                                      00050000
IBHLSPAC CSECT                                                          00060000
         SPACE 1                                                        00070000
*********************************************************************** 00080000
*                                                                     * 00090000
*        THIS ROUTINE USES THE SMF LSPACE ROUTINE (SVC 78)            * 00100000
*        TO COMPUTE VOLUME SPACE STATISTICS FOR ALL ONLINE            * 00110000
*        DA DEVICES.                                                  * 00120000
*                                                                     * 00130000
*        DDNAMES:  SYSPRINT - TO SYSOUT                               * 00140000
*                  VATLST - ALLOCATE TO SYS1.PARMLIB(VATLSTXX)        * 00150000
*                YOUR VATLST SHOULD HAVE COMMENTS STARTING COL        * 00160000
*                30 WHICH ARE THEN DISPLAYED IN THE REPORT.           * 00170000
*                                                                     * 00180000
* CBT File #021             WRITTEN BY G M SCHLESAK / BELL & HOWELL   * 00190000
*********************************************************************** 00200000
         SPACE 1                                                        00210000
*                                                                       00220000
*-------------------------------------------------------------------*   00230000
*                                                                   *   00240000
*                      D I S C L A I M E R                          *   00250000
*                                                                   *   00260000
*  BELL AND HOWELL COMPANY MAKES NO WARRANTY EXPRESSED OR IMPLIED   *   00270000
*  AS TO THE FITNESS OF THIS CODE UNDER YOUR ENVIRONMENT.  THIS     *   00280000
*  EXIT SHOULD BE COMPREHENSIVELY TESTED BEFORE IT IS PLACED INTO   *   00290000
*  YOUR PRODUCTION ENVIRONMENT.  IF YOU HAVE ANY QUESTIONS CONTACT: *   00300000
*                                                                   *   00310000
*           ROBERT M. SIRKIS                                        *   00320000
*           TECHNICAL SUPPORT SPECIALIST                            *   00330000
*           BELL AND HOWELL COMPANY                                 *   00340000
*           2231 WEST HOWARD STREET                                 *   00350000
*           EVANSTON, IL 60202                                      *   00360000
*           (312) 570-4687                                          *   00370000
*                                                                   *   00380000
*-------------------------------------------------------------------*   00390000
*                                                                       00400000
         SPACE 1                                                        00410000
R0       EQU   0                                                        00420000
R1       EQU   1                                                        00430000
R2       EQU   2                                                        00440000
R3       EQU   3                                                        00450000
R4       EQU   4                                                        00460000
R5       EQU   5                                                        00470000
R6       EQU   6                                                        00480000
R7       EQU   7                                                        00490000
R8       EQU   8                                                        00500000
R9       EQU   9                                                        00510000
R10      EQU   10                                                       00520000
R11      EQU   11                                                       00530000
R12      EQU   12                                                       00540000
R13      EQU   13                                                       00550000
R14      EQU   14                                                       00560000
R15      EQU   15                                                       00570000
         EJECT                                                          00580000
***  P E R F O R M   L I N K A G E   A N D   O P E N   F I L E S        00590000
         SAVE (14,12),,*                                                00600000
         LR    R12,R15                                                  00610000
         USING IBHLSPAC,R12                                             00620000
         ST    R13,SAVEAREA+4                                           00630000
         LA    R11,SAVEAREA                                             00640000
         ST    R11,8(R13)                                               00650000
         LR    R13,R11                                                  00660000
         SPACE 1                                                        00670000
         OPEN  (SYSPRINT,(OUTPUT))                                      00680000
         LTR   R15,R15             DID FILE OPEN?                       00690000
         BNZ   RETURN              NO                                   00700000
         OPEN  (VATLST,(INPUT))                                         00710000
         LTR   R15,R15             DID FILE OPEN?                       00720000
         BNZ   GETCVT              NO                                   00730000
         SPACE 2                                                        00740000
***  FILL IN COMMENT TABLE FROM VATLST FILE                             00750000
         SLR   R4,R4               ZERO TABLE ENTRY COUNTER             00760000
         LA    R5,CMNTABLE         POINT AT BEGINNING OF COMMENT TABLE  00770000
         USING CMNTENTY,R5         ADDRESS TABLE ENTRY SYMBOLICALLY     00780000
GETCMNTS GET   VATLST,VATRCRD                                           00790000
         MVC   CMNTVOLS,VATRVOLS   PUT VOLSER INTO TABLE                00800000
         MVC   CMNTCMNT,VATRCMNT   PUT COMMENT FILED INTO TABLE         00810000
         LA    R4,1(R4)            BUMP TABLE ENTRY COUNTER             00820000
         LA    R5,CMNTENTL(R5)     POINT TO NEXT TABLE ENTRY            00830000
         B     GETCMNTS            GO GET NEXT RECORD                   00840000
         DROP  R5                                                       00850000
         SPACE 1                                                        00860000
EOFCMNTS CLOSE (VATLST)                                                 00870000
         S     R5,=A(CMNTENTL)     POINT BACK TO THE LAST ENTRY         00880000
         ST    R5,TOTENTP          SAVE LAST ENTRY POINTER              00890000
         ST    R4,TOTENTN          SAVE TOTAL NUMBER OF ENTRIES         00900000
         SPACE 2                                                        00910000
***  F I L L   I N   &   P R I N T   P A G E   H E A D I N G            00920000
GETCVT   L     R2,CVTPTR           GET ADDR OF CVT                      00930000
         L     R3,CVTSMCA(R2)      GET ADDR OF SMCA                     00940000
         LTR   R3,R3               IS SMF IN SYSTEM?                    00950000
         BZ    *+10                NO, DO NOT MOVE SYSID                00960000
         MVC   HEADING1,SMCASID(R3) MOVE SYSID TO HEADING               00970000
         ED    HEADING2,CVTDATE+1(R2) PUT DATE IN HEADING               00980000
         TIME  DEC                 GET TIME OF DAY                      00990000
         ST    R0,DECTIME                                               01000000
         ED    HEADING3,DECTIME    PUT TIME IN HEADING                  01010000
         SPACE 1                                                        01020000
* PRINT PAGE HEADINGS                                                   01030000
         PUT   SYSPRINT,HEADINGA                                        01040000
         PUT   SYSPRINT,HEADINGB                                        01050000
         PUT   SYSPRINT,HEADINGC                                        01060000
         PUT   SYSPRINT,OUTLINE                                         01070000
         SPACE 2                                                        01080000
***  S E A R C H   U C B S   F O R   D A   D E V I C E S                01090000
         L     R2,CVTPTR           GET ADDR OF CVT                      01100000
         L     R2,CVTILK2(R2)      GET ADDR OF UCB ADDR LIST            01110000
CHECKUCB SR    R3,R3                                                    01120000
         CH    R3,0(R2)            IS THIS A ZERO ENTRY                 01130000
         BE    NEXTUCB             YES, GOTO NEXT ENTRY                 01140000
         LH    R3,0(R2)            GET UCB ADDR                         01150000
         CH    R3,XFFFF            END OF LIST?                         01160000
         BE    SPACETOT            YES, GO PRINT TOTAL LINE             01170000
         BAL   R11,VOLINFO         PROCESS THIS UCB                     01180000
         SPACE 1                                                        01190000
NEXTUCB  LA    R2,2(R2)            GET NEXT UCB ADDR                    01200000
         B     CHECKUCB                                                 01210000
         SPACE 2                                                        01220000
***  G E T   V O L U M E   I N F O R M A T I O N                        01230000
VOLINFO  EQU   *                                                        01240000
         USING UCBDSECT,R3                                              01250000
         CLI   UCBTYP+2,TYPDA      IS IT DA?                            01260000
         BNER  R11                 NO, GOTO NEXT UCB                    01270000
         CLI   UCBTYP+3,TYP2321    IS IT 2321 DATA CELL?                01280000
         BER   R11                 YES, GOTO NEXT UCB                   01290000
         TM    UCBSTAT,UCBONLI     IS DEVICE ONLINE                     01300000
         BNOR  R11                 NO, GOTO NEXT UCB                    01310000
         LR    R0,R3               SET R0 TO UCB IN QUESTION.           01320000
         L     R15,=V(OPTCHK)      GET OPTCHK ADDRESS.                  01330000
         BALR  R14,R15             CALL OPTCHK ROUTINE.                 01340000
         LTR   R15,R15             HAS UCB BEEN PROCESSED VIA OPTCHAN?  01350000
         BNER  R11                 YES-GOTO NEXT UCB.                   01360000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE  CLEAR PRINT LINE         01370000
         MVC   OUTLINE1,UCBVOLI    MOVE VOLSER                          01380000
         MVC   OUTLINE4,UCBNAME    MOVE UNIT ADDRESS                    01390000
         SPACE 2                                                        01400000
***  PUT COMMENT FROM VATLST INTO PRINT LINE                            01410000
         ICM   R4,15,TOTENTN       GET TOTAL NUMBER OF ENTRIES          01420000
         BZ    DOLSPACE            NO TABLE ENTRIES                     01430000
         L     R5,TOTENTP          POINT TO LAST TABLE ENTRY            01440000
         USING CMNTENTY,R5         ADDRESS TABLE ENTRY SYMBOLICALLY     01450000
FNDCLUP1 CLC   UCBVOLI,CMNTVOLS    IS THIS THE RIGHT VOLSER             01460000
         BE    FNDCLUP2            YES, WE GOT IT                       01470000
         S     R5,=A(CMNTENTL)     BUMP DOWN TO NEXT ENTRY              01480000
         BCT   R4,FNDCLUP1         GO CHECK IT                          01490000
         B     DOLSPACE            NO MATCHING ENTRY                    01500000
FNDCLUP2 MVC   OUTLINE8,CMNTCMNT   PUT COMMENT INTO OUTPUT              01510000
         DROP  R5                                                       01520000
         SPACE 2                                                        01530000
***  G O   T O   L S P A C E   R O U T I N E                            01540000
DOLSPACE LR    R0,R3               SET UP UCB ADDRESS                   01550000
         LA    R1,PARMAREA         30 BYTE MESSAGE AREA                 01560000
         SVC   78                  ISSUE LSPACE SVC                     01570000
         LTR   R15,R15             TEST RETURN CODE                     01580000
         BNZ   SETERR                                                   01590000
         SPACE 1                                                        01600000
* PACK VALUES RETURNED BY LSPACE                                        01610000
         PACK  PRMFSCTP,PRMFSCTC   PACK MAX CONTIG TRKS                 01620000
         PACK  PRMFSCCP,PRMFSCCC   PACK MAX CONTIG CYLS                 01630000
         PACK  PRMFSEXP,PRMFSEXC   PACK NUMBER OF EXTENTS               01640000
         PACK  PRMFSTTP,PRMFSTTC   PACK TOTAL FREE TRKS                 01650000
         PACK  PRMFSTCP,PRMFSTCC   PACK TOTAL FREE CYLS                 01660000
         SPACE 1                                                        01670000
* PUT VALUES IN PRINT LINE                                              01680000
         MVC   OUTLINE2,PATTERN1                                        01690000
         ED    OUTLINE2,FREESPAC                                        01700000
         CP    PRMFSEXP,=P'50'     MORE THAN 50 EXTENTS                 01710000
         BNH   *+8                                                      01720000
         MVI   OUTLINE2+20,C'*'    HI-LIGHT                             01730000
         CP    PRMFSTTP,=P'200'    MORE THAN 200 FREE TRACKS            01740000
         BNH   *+8                                                      01750000
         MVI   OUTLINE2+13,C'*'    HI-LIGHT                             01760000
         B     COMPDEV             GO COMPUTE DEVICE TYPE               01770000
         SPACE 1                                                        01780000
* MOVE ERROR MSG TO OUTPUT LINE                                         01790000
SETERR   EQU   *                                                        01800000
         SLA   R15,2                                                    01810000
         AL    R15,=A(MSGTBL-16)   ADDRESS MSG TABLE                    01820000
         MVC   OUTLINE3,0(R15)     MOVE MSG INTO LINE                   01830000
         OI    ERRINDIC,LSPACERR   INDICATE ERROR RETURNED              01840000
         SPACE 2                                                        01850000
***  C O M P U T E   D E V   T Y P E   A N D   U P D A T E              01860000
***  D E V   T Y P E   S P A C E   T O T A L S                          01870000
COMPDEV  LA    R4,DEVTBL           GET ADDR OF DEVICE TABLE             01880000
         USING TBLDSECT,R4                                              01890000
CHKDEV   CLI   TBLDEV,ENDTBL       IS THIS THE LAST ENTRY?              01900000
         BE    MVDVTYP             YES, WE MUST USE THIS ENTRY          01910000
         CLC   UCBTYP+3(1),TBLDEV  UCB MATCH TABLE ENTRY?               01920000
         BNE   NXTNTRY             NO, GO TO NEXT DEVICE ENTRY          01930000
MVDVTYP  MVC   OUTLINE5,TBLNAME    MOVE DEV NAME TO OUTPUT LINE         01940000
         MVI   TBLUSE,TBLUSED      INDICATE DEVICE HAS BEEN FOUND       01950000
         SPACE 1                                                        01960000
* CHECK FOR VOL CATAEGORY                                               01970000
CHKVPVT  TM    ERRINDIC,LSPACERR   LSPACE ERROR?                        01980000
         BO    VOLCAT              YES                                  01990000
         CLC   UCBVOLI(3),VOLVPVT  IS IT A PVT VOLUME?                  02000000
         BNE   CHKVSTG             NO                                   02010000
         AP    TOTCVPVT,PRMFSTCP   ADD FREE CYLS TOTAL                  02020000
         AP    TOTTVPVT,PRMFSTTP   ADD FREE TRKS TOTAL                  02030000
         B     VOLCAT              GO GET VOLUME CATEGORY               02040000
         SPACE 1                                                        02050000
CHKVSTG  CLC   UCBVOLI(3),VOLVSTG  IS IT A PVT VOLUME?                  02060000
         BNE   CHKVSCP             NO                                   02070000
         AP    TOTCVSTG,PRMFSTCP   ADD FREE CYLS TOTAL                  02080000
         AP    TOTTVSTG,PRMFSTTP   ADD FREE TRKS TOTAL                  02090000
         B     VOLCAT              GO GET VOLUME CATEGORY               02100000
         SPACE 1                                                        02110000
CHKVSCP  CLC   UCBVOLI(3),VOLVSCP  IS IT A SCP VOLUME?                  02120000
         BNE   CHKVTSO             NO                                   02130000
         AP    TOTCVSCP,PRMFSTCP   ADD FREE CYLS TOTAL                  02140000
         AP    TOTTVSCP,PRMFSTTP   ADD FREE TRKS TOTAL                  02150000
         B     VOLCAT              GO GET VOLUME CATEGORY               02160000
         SPACE 1                                                        02170000
CHKVTSO  CLC   UCBVOLI(3),VOLVTSO  IS IT A TSO VOLUME?                  02180000
         BNE   CHKVPUB             NO                                   02190000
         AP    TOTCVTSO,PRMFSTCP   ADD FREE CYLS TOTAL                  02200000
         AP    TOTTVTSO,PRMFSTTP   ADD FREE TRKS TOTAL                  02210000
         B     VOLCAT              GO GET VOLUME CATEGORY               02220000
         SPACE 1                                                        02230000
CHKVPUB  CLC   UCBVOLI(3),VOLVPUB  IS IT A PUB VOLUME?                  02240000
         BNE   CHKVOTH             NO                                   02250000
         AP    TOTCVPUB,PRMFSTCP   ADD FREE CYLS TOTAL                  02260000
         AP    TOTTVPUB,PRMFSTTP   ADD FREE TRKS TOTAL                  02270000
         B     VOLCAT              GO GET VOLUME CATEGORY               02280000
         SPACE 1                                                        02290000
CHKVOTH  AP    TOTCVOTH,PRMFSTCP   ADD FREE CYLS TOTAL                  02300000
         AP    TOTTVOTH,PRMFSTTP   ADD FREE TRKS TOTAL                  02310000
         B     VOLCAT              GO GET VOLUME CATEGORY               02320000
         SPACE 1                                                        02330000
* CHECK FOR VOL TYPE                                                    02340000
VOLCAT   TM    UCBSTAB,UCBBPRV     IS IT A PRV VOLUME?                  02350000
         BNO   CHKPUB              NO                                   02360000
         MVC   OUTLINE6,VOLTPRV    MOVE VOL TYPE TO OUTPUT LINE         02370000
         TM    ERRINDIC,LSPACERR   DO WEE HAVE NUMBERS?                 02380000
         BO    VOLSTAT             NO                                   02390000
         AP    TBLCPRV,PRMFSTCP    ADD FREE CYLS                        02400000
         AP    TBLTPRV,PRMFSTTP    ADD FREE TRKS                        02410000
         AP    TOTCPRV,PRMFSTCP    ADD FREE CYLS TOTAL                  02420000
         AP    TOTTPRV,PRMFSTTP    ADD FREE TRKS TOTAL                  02430000
         B     VOLSTAT             GO GET VOLUME STATUS                 02440000
         SPACE 1                                                        02450000
CHKPUB   TM    UCBSTAB,UCBBPUB     IS IT A PUB VOLUME?                  02460000
         BNO   CHKSTR              NO                                   02470000
         TM    ERRINDIC,LSPACERR   DO WEE HAVE NUMBERS?                 02480000
         BO    VOLSTAT             NO                                   02490000
         MVC   OUTLINE6,VOLTPUB    MOVE VOL TYPE TO OUTPUT LINE         02500000
         AP    TBLCPUB,PRMFSTCP    ADD FREE CYLS                        02510000
         AP    TBLTPUB,PRMFSTTP    ADD FREE TRKS                        02520000
         AP    TOTCPUB,PRMFSTCP    ADD FREE CYLS TOTAL                  02530000
         AP    TOTTPUB,PRMFSTTP    ADD FREE TRKS TOTAL                  02540000
         B     VOLSTAT             GO GET VOLUME STATUS                 02550000
         SPACE 1                                                        02560000
CHKSTR   TM    UCBSTAB,UCBBSTR     IS IT A STR VOLUME?                  02570000
         BNO   VOLSTAT             GO GET VOLUME STATUS                 02580000
         TM    ERRINDIC,LSPACERR   DO WEE HAVE NUMBERS?                 02590000
         BO    VOLSTAT             NO                                   02600000
         MVC   OUTLINE6,VOLTSTR    MOVE VOL TYPE TO OUTPUT LINE         02610000
         AP    TBLCSTR,PRMFSTCP    ADD FREE CYLS                        02620000
         AP    TBLTSTR,PRMFSTTP    ADD FREE TRKS                        02630000
         AP    TOTCSTR,PRMFSTCP    ADD FREE CYLS TOTAL                  02640000
         AP    TOTTSTR,PRMFSTTP    ADD FREE TRKS TOTAL                  02650000
         B     VOLSTAT             GO GET VOLUME STATUS                 02660000
         SPACE 1                                                        02670000
NXTNTRY  CLI   TBLDEV,ENDTBL       IS THIS END OF TABLE                 02680000
         BE    VOLSTAT             YES, GO COMPUTE VOL STATUS           02690000
         LA    R4,NXTDEV           POINT TO NEXT DEVICE ENTRY           02700000
         B     CHKDEV              GO SEE IF MATCH                      02710000
         DROP  R4                                                       02720000
         SPACE 2                                                        02730000
***  C O M P U T E   V O L U M E   S T A T U S                          02740000
VOLSTAT  MVC   OUTLINE7,STATRMOV   DEFAULT STATUS IS REMOVABLE          02750000
         TM    UCBSTAT,UCBRESV     IS IT RESERVED?                      02760000
         BNO   *+10                                                     02770000
         MVC   OUTLINE7,STATRESV   MOVE VOL STAT TO OUTPUT LINE         02780000
         TM    UCBSTAT,UCBPRES     IS IT PERM RESIDENT?                 02790000
         BNO   *+10                                                     02800000
         MVC   OUTLINE7,STATPRES   MOVE VOL STAT TO OUTPUT LINE         02810000
         DROP  R3                                                       02820000
         SPACE 2                                                        02830000
***  P R I N T   O U T P U T    L I N E                                 02840000
PUTLINE  PUT   SYSPRINT,OUTLINE                                         02850000
         NI    ERRINDIC,X'FF'-LSPACERR                                  02860000
         BR    R11                 GO TO NEXT UCB                       02870000
         SPACE 2                                                        02880000
***  P R I N T   S P A C E   T O T A L S   B Y                          02890000
***  D E V   T Y P E   &   V O L   T Y P E                              02900000
SPACETOT EQU   *                                                        02910000
         SPACE 1                                                        02920000
* PRINT VOL TYPE HEADINGS                                               02930000
         PUT   SYSPRINT,HEADINGH                                        02940000
         PUT   SYSPRINT,HEADINGF                                        02950000
         PUT   SYSPRINT,HEADINGG                                        02960000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE CLEAR PRINT LINE          02970000
         PUT   SYSPRINT,OUTLINE                                         02980000
         SPACE 1                                                        02990000
         MVC   OUTLINEA,VOLVPVT    MOVE VOL TYPE TO OUTPUT LINE         03000000
         MVC   OUTLINEB,PATTERN3   MOVE EDIT PATTERN TO OUTPUT LINE     03010000
         ED    OUTLINEB,TOTCVPVT   EDIT FREE SPACE VALUES               03020000
         PUT   SYSPRINT,OUTLINE                                         03030000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE CLEAR PRINT LINE          03040000
         SPACE 1                                                        03050000
         MVC   OUTLINEA,VOLVSCP    MOVE VOL TYPE TO OUTPUT LINE         03060000
         MVC   OUTLINEB,PATTERN3   MOVE EDIT PATTERN TO OUTPUT LINE     03070000
         ED    OUTLINEB,TOTCVSCP   EDIT FREE SPACE VALUES               03080000
         PUT   SYSPRINT,OUTLINE                                         03090000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE CLEAR PRINT LINE          03100000
         SPACE 1                                                        03110000
         MVC   OUTLINEA,VOLVTSO    MOVE VOL TYPE TO OUTPUT LINE         03120000
         MVC   OUTLINEB,PATTERN3   MOVE EDIT PATTERN TO OUTPUT LINE     03130000
         ED    OUTLINEB,TOTCVTSO   EDIT FREE SPACE VALUES               03140000
         PUT   SYSPRINT,OUTLINE                                         03150000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE CLEAR PRINT LINE          03160000
         SPACE 1                                                        03170000
         MVC   OUTLINEA,VOLVSTG    MOVE VOL TYPE TO OUTPUT LINE         03180000
         MVC   OUTLINEB,PATTERN3   MOVE EDIT PATTERN TO OUTPUT LINE     03190000
         ED    OUTLINEB,TOTCVSTG   EDIT FREE SPACE VALUES               03200000
         PUT   SYSPRINT,OUTLINE                                         03210000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE CLEAR PRINT LINE          03220000
         SPACE 1                                                        03230000
         MVC   OUTLINEA,VOLVPUB    MOVE VOL TYPE TO OUTPUT LINE         03240000
         MVC   OUTLINEB,PATTERN3   MOVE EDIT PATTERN TO OUTPUT LINE     03250000
         ED    OUTLINEB,TOTCVPUB   EDIT FREE SPACE VALUES               03260000
         PUT   SYSPRINT,OUTLINE                                         03270000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE CLEAR PRINT LINE          03280000
         SPACE 1                                                        03290000
         MVC   OUTLINEA,VOLVOTH    MOVE VOL TYPE TO OUTPUT LINE         03300000
         MVC   OUTLINEB,PATTERN3   MOVE EDIT PATTERN TO OUTPUT LINE     03310000
         ED    OUTLINEB,TOTCVOTH   EDIT FREE SPACE VALUES               03320000
         PUT   SYSPRINT,OUTLINE                                         03330000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE CLEAR PRINT LINE          03340000
         SPACE 1                                                        03350000
* PRINT DEV TYPE HEADINGS                                               03360000
         PUT   SYSPRINT,HEADINGH                                        03370000
         PUT   SYSPRINT,HEADINGD                                        03380000
         PUT   SYSPRINT,HEADINGE                                        03390000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE CLEAR PRINT LINE          03400000
         PUT   SYSPRINT,OUTLINE                                         03410000
         SPACE 1                                                        03420000
* PRINT THE TOTALS                                                      03430000
         LA    R4,DEVTBL                                                03440000
         USING TBLDSECT,R4                                              03450000
NEWNTRY  CLI   TBLUSE,TBLUSED      WAS THIS ENTRY USED?                 03460000
         BNE   UPDTPTR             NO, GO TO NEXT DEVICE ENTRY          03470000
         MVC   OUTLINEA,TBLNAME    MOVE DEVICE NAME TO OUTPUT LINE      03480000
         MVC   OUTLINEB,PATTERN2   MOVE EDIT PATTERN TO OUTPUT LINE     03490000
         ED    OUTLINEB,DEVTOTS    EDIT FREE SPACE VALUES               03500000
         PUT   SYSPRINT,OUTLINE                                         03510000
         MVC   OUTLINE+1(L'OUTLINE-1),OUTLINE CLEAR PRINT LINE          03520000
UPDTPTR  CLI   TBLDEV,ENDTBL       IS THIS THE LAST ENTRY?              03530000
         LA    R4,NXTDEV           POINT TO NEXT DEVICE ENTRY           03540000
         BNE   NEWNTRY             THERE ARE MORE ENTRIES               03550000
         MVC   OUTLINE(L'TOTAL),TOTAL                                   03560000
         MVC   OUTLINEB,PATTERN2   MOVE EDIT PATTERN TO OUTPUT LINE     03570000
         ED    OUTLINEB,FREETOTS   EDIT FREE SPACE TOTALS               03580000
         PUT   SYSPRINT,OUTLINE                                         03590000
         SPACE 2                                                        03600000
         CLOSE (SYSPRINT)                                               03610000
         SPACE 2                                                        03620000
***  R E T U R N   T O    S Y S T E M                                   03630000
RETURN   EQU   *                                                        03640000
         L     R13,4(R13)                                               03650000
         RETURN (14,12),RC=0                                            03660000
         SPACE 2                                                        03670000
***  D A T A   A R E A S   A N D   C O N S T A N T S                    03680000
CVTPTR   EQU   16                  ADDR OF PTR TO CVT                   03690000
CVTSMCA  EQU   196                 ADDR OF SMCA                         03700000
CVTDATE  EQU   56                  DATE                                 03710000
CVTILK2  EQU   40                  ADDR OF UCB ADDR LIST                03720000
SMCASID  EQU   16                  SYSTEM ID                            03730000
ENDTBL   EQU   X'FF'               END OF TABLE INDICATOR               03740000
TBLUSED  EQU   X'FF'               ENTRY HAS BEEN USED INDICATOR        03750000
ERRINDIC DC    X'00'               ERROR INDICATOR BYTE                 03760000
LSPACERR EQU   X'80'               ERROR RETURNED FROM LSPACE FOR VOL   03770000
         SPACE 1                                                        03780000
VOLTSTR  DC    CL4'STRG'                                                03790000
VOLTPRV  DC    CL4'PRIV'                                                03800000
VOLTPUB  DC    CL4'PUB '                                                03810000
VOLVPVT  DC    CL7'PVT*** '                                             03820000
VOLVTSO  DC    CL7'TSO*** '                                             03830000
VOLVSCP  DC    CL7'SCP*** '                                             03840000
VOLVSTG  DC    CL7'STG*** '                                             03850000
VOLVPUB  DC    CL7'PUB*** '                                             03860000
VOLVOTH  DC    CL7'OTHER  '                                             03870000
STATRMOV DC    CL5'REMOV'                                               03880000
STATRESV DC    CL5'RESRV'                                               03890000
STATPRES DC    CL5'RSDNT'                                               03900000
         SPACE 1                                                        03910000
DECTIME  DC    F'0'                                                     03920000
TOTENTP  DC    A(0)               POINTER TO LAST COMMENT ENTRY         03930000
TOTENTN  DC    F'0'               TOTAL NUMBER OF COMMENT ENTRIES       03940000
XFFFF    DC    X'FFFF'                                                  03950000
TOTAL    DC    C'0TOTALS'                                               03960000
SAVEAREA DS    18F                                                      03970000
         SPACE 1                                                        03980000
LSPACPRM EQU   *                                                        03990000
         DS    10F                                                      04000000
         ORG   LSPACPRM+8                                               04010000
PARMAREA EQU   *                   LSPACE INFO RETURN FIELD             04020000
         ORG   PARMAREA+6          BEGINNNING OF CHAR FILEDS            04030000
PRMFSTCC DS    CL4,CL1             FREE TOTAL CYL (CHAR)                04040000
PRMFSTTC DS    CL4,CL1             FREE TOTAL TRK (CHAR)                04050000
PRMFSEXC DS    CL4,CL1             FREE EXTENTS   (CHAR)                04060000
PRMFSCCC DS    CL4,CL1             LARGEST CONTIG CYL (CHAR)            04070000
PRMFSCTC DS    CL4                 LARGEST CONTIG TRK (CHAR)            04080000
         ORG   PARMAREA+15         BEGINNING OF PACKED FIELDS           04090000
FREESPAC EQU   *                                                        04100000
PRMFSTCP DS    CL3                 FREE TOT CYL (PACKED)                04110000
PRMFSTTP DS    CL3                 FREE TOT TRK (PACKED)                04120000
PRMFSEXP DS    CL3                 FREE EXTENTS   (PACKED)              04130000
PRMFSCCP DS    CL3                 LARGEST CONTIG CYL (PACKED)          04140000
PRMFSCTP DS    CL3                 LARGEST CONTIG TRK (PACKED)          04150000
         ORG                                                            04160000
         SPACE 1                                                        04170000
VATRCRD  DS    0CL80               VATLST RECORD LAYOUT                 04180000
VATRVOLS DS    CL6                 VOLSER                               04190000
         DS    CL23                FILLER                               04200000
VATRCMNT DS    CL30                COMMENT FIELD                        04210000
         DS    CL21                FILLER                               04220000
         SPACE 1                                                        04230000
MSGTBL   EQU   *                                                        04240000
         DC    CL16'PERM I/O ERROR' RC=4                                04250000
         DC    CL16'NON-STANDARD VOL' RC=8                              04260000
         DC    CL16'UCB NOT READY' RC=12                                04270000
         DC    CL16'LSPACE RC=16'  RC=16                                04280000
         SPACE 1                                                        04290000
DEVTBL   EQU   *                                                        04300000
DEV2311  DC    X'01',X'00',CL6' 2311 ',6PL4'0'                          04310000
DEV23051 DC    X'06',X'00',CL6'2305-1',6PL4'0'                          04320000
DEV23052 DC    X'07',X'00',CL6'2305-2',6PL4'0'                          04330000
DEV2314  DC    X'08',X'00',CL6' 2314 ',6PL4'0'                          04340000
DEV3330  DC    X'09',X'00',CL6' 3330 ',6PL4'0'                          04350000
DEV3340  DC    X'0A',X'00',CL6' 3340 ',6PL4'0'                          04360000
DEV3350  DC    X'0B',X'00',CL6' 3350 ',6PL4'0'                          04370000
DEV3375  DC    X'0C',X'00',CL6' 3375 ',6PL4'0'                          04380000
DEV33301 DC    X'0D',X'00',CL6'3330-1',6PL4'0'                          04390000
DEV3380  DC    X'0E',X'00',CL6' 3380 ',6PL4'0'                          04400000
DEV3390  DC    X'0F',X'00',CL6' 3390 ',6PL4'0'                          04410000
DEVUNKWN DC    X'FF',X'00',CL6'UNKNWN',6PL4'0'                          04420000
         SPACE 1                                                        04430000
FREETOTS EQU   *                   TOTAL FREE SPACE COUNTERS            04440000
TOTCPRV  DC    PL4'0'              PRV CYL TOTAL                        04450000
TOTTPRV  DC    PL4'0'              PRV TRK TOTAL                        04460000
TOTCPUB  DC    PL4'0'              PUB CYL TOTAL                        04470000
TOTTPUB  DC    PL4'0'              PUB TRK TOTAL                        04480000
TOTCSTR  DC    PL4'0'              STR CYL TOTAL                        04490000
TOTTSTR  DC    PL4'0'              STR TRK TOTAL                        04500000
TOTCVPVT DC    PL4'0'              VOL=PVT CYL TOTAL                    04510000
TOTTVPVT DC    PL4'0'              VOL=PVT TRK TOTAL                    04520000
TOTCVSCP DC    PL4'0'              VOL=SCP CYL TOTAL                    04530000
TOTTVSCP DC    PL4'0'              VOL=SCP TRK TOTAL                    04540000
TOTCVTSO DC    PL4'0'              VOL=TSO CYL TOTAL                    04550000
TOTTVTSO DC    PL4'0'              VOL=TSO TRK TOTAL                    04560000
TOTCVSTG DC    PL4'0'              VOL=STG CYL TOTAL                    04570000
TOTTVSTG DC    PL4'0'              VOL=STG TRK TOTAL                    04580000
TOTCVPUB DC    PL4'0'              VOL=PUB CYL TOTAL                    04590000
TOTTVPUB DC    PL4'0'              VOL=PUB TRK TOTAL                    04600000
TOTCVOTH DC    PL4'0'              VOL=OTHER CYL TOTAL                  04610000
TOTTVOTH DC    PL4'0'              VOL=OTHER TRK TOTAL                  04620000
         SPACE 1                                                        04630000
PATTERN1 DC    X'402020202120222020202120224020202021202240404040404040X04640000
               202020212022404040402020202120'                          04650000
PATTERN2 DC    X'402020202020212022402020202020212022404040202020202021X04660000
               20222020202020212022404040202020202021202240202020202021X04670000
               20'                                                      04680000
PATTERN3 DC    X'4020202020202120224020202020202120'                    04690000
         SPACE 1                                                        04700000
HEADINGA DC    CL121' '                                                 04710000
         ORG   HEADINGA                                                 04720000
         DC    C'1IBHLSPAC  V2L4      D I R E C T    A C C E S S    S PX04730000
                A C E      '                                            04740000
HEADING1 DC    CL4' ',CL3' '                                            04750000
HEADING2 DC    X'4020204B202020',CL3' '                                 04760000
HEADING3 DC    X'4021204B20204B2020'                                    04770000
         ORG                                                            04780000
HEADINGB DC    CL121'-VOLSER  FREE  FREE   FREE    LARGEST CONTIG FREE X04790000
               AREA  UNIT    DEVICE    ---STATUS--    ----COMMENTS----' 04800000
HEADINGC DC    CL121'         CYLS  TRKS  EXTENTS     CYLINDERS  TRACKSX04810000
                              TYPE'                                     04820000
HEADINGD DC    CL121'-DEVICE     ---PRIVATE---       ---PUBLIC---      X04830000
                ---STORAGE---'                                          04840000
HEADINGE DC    CL121'  TYPE      CYLS     TRKS       CYLS    TRKS      X04850000
                CYLS     TRKS'                                          04860000
HEADINGF DC    CL121'-VOLUME     FREE     FREE '                        04870000
HEADINGG DC    CL121'  TYPE      CYLS     TRKS '                        04880000
HEADINGH DC    CL121'0'                                                 04890000
         ORG   HEADINGH+1                                               04900000
         DC    91C'-'                                                   04910000
         ORG                                                            04920000
OUTLINE  DC    CL121' '                                                 04930000
         ORG   OUTLINE+1                                                04940000
OUTLINE1 DS    CL6' '              VOL SER                              04950000
OUTLINE2 DS    CL(L'PATTERN1)' '   FREE SPACE VALUES                    04960000
         ORG   OUTLINE2+4                                               04970000
OUTLINE3 DS    CL16' '             ERROR MSG                            04980000
         DS    CL29' '                                                  04990000
OUTLINE4 DS    CL3' '              UNIT ADDRESS                         05000000
         DS    CL5' '                                                   05010000
OUTLINE5 DS    CL6' '              DEVICE TYPE                          05020000
         DS    CL4' '                                                   05030000
OUTLINE6 DS    CL4' '              VOL TYPE                             05040000
         DS    CL2' '                                                   05050000
OUTLINE7 DS    CL5' '              VOL STATUS                           05060000
         DS    CL5' '                                                   05070000
OUTLINE8 DS    CL30' '             VOL COMMENTS                         05080000
         ORG   OUTLINE+1                                                05090000
OUTLINEA DS    CL6' '              DEVICE TYPE                          05100000
         DS    CL1' '                                                   05110000
OUTLINEB DS    CL(L'PATTERN2)' '   FREE SPACE TOTALS                    05120000
         ORG                                                            05130000
         SPACE 2                                                        05140000
         PRINT NOGEN                                                    05150000
SYSPRINT DCB   DDNAME=SYSPRINT,MACRF=PM,LRECL=121,DSORG=PS,            X05160000
               BLKSIZE=4840,RECFM=FBA                                   05170000
VATLST   DCB   DDNAME=VATLST,MACRF=GM,LRECL=80,DSORG=PS,               X05180000
               EODAD=EOFCMNTS                                           05190000
         SPACE 2                                                        05200000
         LTORG                                                          05210000
         SPACE 2                                                        05220000
CMNTABLE DS    200XL36     VATLST VOLSER COMMENT TABLE                  05230000
         SPACE 2                                                        05240000
         PRINT GEN                                                      05250000
         EJECT                                                          05260000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05270000
*  OPTCHAN - OPTIONAL CHANNEL ROUTINE                                 * 05280000
*                                                                     * 05290000
*  THIS ROUTINE IS TO DETERMINE IF A UCB HAS ALREADY BEEN PROCESSED   * 05300000
*  BY THE PROGRAM.  OPTCHAN WILL MAINTAIN A TABLE OF SERIAL NUMBERS   * 05310000
*  AND BASED ON THIS INFORMATION DECIDE IF UCB HAS ALREADY BEEN       * 05320000
*  PROCESSED.                                                         * 05330000
*                                                                     * 05340000
*  ENTRY - REGISTER 0 CONTAINS ADDRESS OF UCB UNDER CONSIDERATION.    * 05350000
*                                                                     * 05360000
*  EXIT  - REGISTER 15 IS RETURN CODE:                                * 05370000
*          0 - UCB HAS NOT BEEN PROCESSED.                            * 05380000
*          4 - UCB HAS BEEN PROCESSED.                                * 05390000
*                                                                     * 05400000
*  ABNORMAL CONDITIONS:                                               * 05410000
*  U100 - NUMBER OF UNIQUE UCBS HAS EXCEEDED 100.                     * 05420000
*  U200 - REGISTER 0 WAS INVALID.                                     * 05430000
*                           ADDED R. PIEPENBRINK / WALGREENS 04/77    * 05440000
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05450000
         SPACE 2                                                        05460000
OPTCHK   CSECT                                                          05470000
         SAVE (14,12),,*                                                05480000
         LR    R12,R15        BASE FOR ROUTINE                          05490000
         USING OPTCHK,R12                                               05500000
         ST    R13,OPTSAVE+4                                            05510000
         LA    R11,OPTSAVE                                              05520000
         ST    R11,8(R13)                                               05530000
         LR    R13,R11             LINKAGE COMPLETE                     05540000
         SPACE 2                                                        05550000
* IS VOLSER IN TABLE?                                                   05560000
         LR    R9,R0               R9 IS BASE FOR UCB                   05570000
         USING UCBDSECT,R9                                              05580000
         CLI   UCBID,X'FF'         IS IT A UCB?                         05590000
         BNE   OPTU200             NO - ABEND U200.                     05600000
         LA    R4,OPTABLE          ADDRESS TABLE                        05610000
         USING OPDSECT,R4                                               05620000
         LA    R6,OPEND-OPDSECT    SET INDEXING VALUE FOR BXH           05630000
         LA    R7,OPTLAST          LAST ENTRY IN TABLE                  05640000
         SPACE                                                          05650000
OPLOOP1  EQU   *                                                        05660000
         CLI   OPDSECT,X'FF'       END OF ENTRIES USED?                 05670000
         BE    OPNOTIN             YES - VOLSER NOT USED.               05680000
         CLC   OPVOLI,UCBVOLI      HAS IT BENN PROCESSED?               05690000
         BE    OPVOLIN             YES - VOLER WAS PROCESSED.           05700000
         BXLE  R4,R6,OPLOOP1       FINISH TABLE.                        05710000
         B     OPTU100                                                  05720000
         SPACE 2                                                        05730000
* VOLSER WAS PROCESSED.                                                 05740000
OPVOLIN  DS    0H                                                       05750000
         LA    R15,4               SET RETURN CODE. UCB PROCESSED       05760000
         B     OPOUT               GO TO EXIT.                          05770000
         SPACE 2                                                        05780000
* VOLSER WAS NOT IN TABLE.                                              05790000
OPNOTIN  DS    0H                                                       05800000
         CR    R4,R7               IS TABLE FULL?                       05810000
         BL    OPSKIP1             NO - BRANCH AROUND ABEND.            05820000
         SPACE                                                          05830000
* THE TABLE IS FULL                                                     05840000
OPTU100  EQU   *                                                        05850000
         ABEND 100,DUMP            TABLE FULL. ABEND U100.              05860000
         SPACE 1                                                        05870000
*  REGISTER 0 DOES NOT REFERENCE A VALID UCB.                           05880000
OPTU200  DS    0H                                                       05890000
         ABEND 200,DUMP                                                 05900000
         SPACE                                                          05910000
OPSKIP1  DS    0H                                                       05920000
         MVC   OPVOLI,UCBVOLI      MOVE IN VOLSER                       05930000
         MVI   OPEND,X'FF'         INDICATE LAST ENTRY USED             05940000
         SPACE 2                                                        05950000
* VOLSER WAS NOT PROCESSED                                              05960000
         SR    R15,R15             SET RETURN CODE. UCB NOT PROCESSED.  05970000
         SPACE 2                                                        05980000
* EXIT FROM OPTCHK                                                      05990000
OPOUT    EQU   *                                                        06000000
         L R13,4(R13)              RESTORE R13                          06010000
         RETURN (14,12),RC=(15)                                         06020000
         BR    R14                 EXIT.                                06030000
         SPACE 2                                                        06040000
         LTORG                                                          06050000
         SPACE 2                                                        06060000
* THIS IS THE TABLE OF VOLUME SERIAL NUMBERS. THE LAST ENTRY IS         06070000
* INDICATED BY THE FIRST BYTE BEING X'FF'  EACH ENTRY IS 6 BYTES.       06080000
OPTABLE  DC    X'FF',CL5' '        FIRST ENTRY INDICATED AS LAST.       06090000
         DC    98CL6' '            OTHER ENTRIES.                       06100000
OPTLAST  DC    CL6' '              LAST ENTRY                           06110000
OPTSAVE  DS    18F                 REGISTER SAVEAREA                    06120000
         EJECT                                                          06130000
UCBDSECT DSECT                                                          06140000
         DS    2X                                                       06150000
UCBID    DS    X                   UCB ID. SHOULD BE X'FF'              06160000
UCBSTAT  DS    XL1                 DEVICE STATUS                        06170000
UCBONLI  EQU   X'80'               DEVICE IS ONLINE                     06180000
UCBRESV  EQU   X'20'               VOL IS RESERVED                      06190000
UCBPRES  EQU   X'04'               VOL IS PERM RESIDENT                 06200000
         DS    9X                                                       06210000
UCBNAME  DS    CL3                 UNIT NAME                            06220000
UCBTYP   DS    XL4                 DEVICE TYPE                          06230000
TYPDA    EQU   X'20'               DIRECT ACCESS                        06240000
TYP2321  EQU   X'05'               2321 DATA CELL                       06250000
         DS    8X                                                       06260000
UCBVOLI  DS    CL6                 DEVICE VOLSER                        06270000
UCBSTAB  DS    XL1                 VOLUME STATUS                        06280000
UCBBPRV  EQU   X'10'               PRIVATE VOLUME                       06290000
UCBBPUB  EQU   X'08'               PUBLIC VOLUME                        06300000
UCBBSTR  EQU   X'04'               STORAGE VOLUME                       06310000
         SPACE 1                                                        06320000
CMNTENTY DSECT                                                          06330000
CMNTVOLS DS    CL6                 VOLSER                               06340000
CMNTCMNT DS    CL30                COMMENT FIELD                        06350000
CMNTENTL EQU   *-CMNTENTY                                               06360000
         SPACE 1                                                        06370000
TBLDSECT DSECT                                                          06380000
TBLDEV   DS    XL1                 DEVICE TYPE ENTRY                    06390000
TBLUSE   DS    XL1                 USE INDICATOR                        06400000
TBLNAME  DS    CL6                 DEVICE NAME                          06410000
DEVTOTS  EQU   *                                                        06420000
TBLCPRV  DS    PL4                 PRV CYL COUNTER                      06430000
TBLTPRV  DS    PL4                 PRV TRK COUNTER                      06440000
TBLCPUB  DS    PL4                 PUB CYL COUNTER                      06450000
TBLTPUB  DS    PL4                 PUB TRK COUNTER                      06460000
TBLCSTR  DS    PL4                 STR CYL COUNTER                      06470000
TBLTSTR  DS    PL4                 STR TRK COUNTER                      06480000
NXTDEV   EQU   *                                                        06490000
         SPACE 1                                                        06500000
OPDSECT  DSECT                                                          06510000
OPVOLI   DS    CL6                 VOLUME SERIAL ENTRY                  06520000
OPEND    DS    0C                                                       06530000
         END   IBHLSPAC                                                 06540000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR   <-- TARGET LOAD LIBRARY   06550000
//LKED.SYSIN DD *                                                       06560000
  NAME IBHLSPAC(R)                                                      06570000
//*
//SAMPLIB  EXEC PGM=PDSLOAD
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DISP=SHR,DSN=SYS2.SAMPLIB
//SYSUT1   DD DATA,DLM=@@
./ ADD NAME=IBHLSPAC
//IBHLSPAC JOB (SYS),'IBHLSPAC',CLASS=A,MSGCLASS=H,NOTIFY=&SYSUID       00010000
//IBHLSPAC EXEC PGM=IBHLSPAC                                            00020000
//VATLST   DD DISP=SHR,DSN=SYS1.PARMLIB(VATLST00)                       00030000
//SYSPRINT DD SYSOUT=*                                                  00040000
//                                                                      00050000

