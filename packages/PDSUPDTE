//PDSUPDTE  JOB (TSO),
//             'Install PDSUPDTE',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00030000
//* ***************************************************************** * 00040000
//* ASSEMBLE PDSUPDTE                                                 * 00050000
//* ***************************************************************** * 00060000
//ASM     EXEC ASMFCL PARM='LIST,OBJECT,NODECK'                         00070000
//SYSIN    DD  *                                                        00080000
         TITLE 'CSECT PDSUPDTE OF LOAD MODULE PDSUPDTE'                 00090000
*********************************************************************** 00100000
*                                                                     * 00110000
*        THIS PROGRAM IS CSECT PDSUPDTE OF LOAD MODULE PDSUPDTE.  IT  * 00120000
*        WAS CREATED BY THE DECOMPILER PROGRAM, DCOMPILE.             * 00130000
*                                                                     * 00140000
*        THIS PROGRAM CAN BE USED TO SCAN A PDS THAT HAS FIXED, 80    * 00150000
*        BYTE RECORDS AND MAKE CHANGES ACCORDING TO PARAMETER INPUT.  * 00160000
*        THE FORMAT OF THE PARM RECORD INPUT (SYSIN) IS AS FOLLOWS:   * 00170000
*                                                                     * 00180000
*                S1<S2<S3<   (STARTS IN POSITION 1)                   * 00190000
*        WHERE:                                                       * 00200000
*                S1 = STRING TO SEARCH FOR                            * 00210000
*                S2 = STRING TO REPLACE S1 WITH                       * 00220000
*                S3 = OPTIONAL 3RD STRING WHICH MUST ALSO EXIST       * 00230000
*                     BEFORE S2 WILL REPLACE S1.                      * 00240000
*                                                                     * 00250000
*        THE STRINGS 'END ' OR '</*' ARE RECOGNIZED AS END OF FILE    * 00260000
*        ON DDNAME=SYSIN.                                             * 00270000
*                                                                     * 00280000
*        RECORDS BEING SCANNED MAY CONTAIN THE STRINGS '.NU.' AND/OR  * 00290000
*        '.RU.'.  THEY STAND FOR NO UPDATE AND RESUME UPDATE.  THEY   * 00300000
*        CAN BE USED TO BRACKET STRINGS WHICH MAY MATCH THE S1 STRING * 00310000
*        BUT ARE NOT TO BE CHANGED IF THEY MATCH OR TO BRACKET        * 00320000
*        MULTIPLE RECORDS WHICH ARE TO BE BYPASSED.                   * 00330000
*                                                                     * 00340000
*        THE DDNAME FOR EACH PDS TO SCAN MUST START WITH @.           * 00350000
*                                                                     * 00360000
*        THE PARM ON THE EXECUTE STATEMENT CAN BE:                    * 00370000
*            CHECK -- PRINT CHANGES TO BE MADE, ONLY (DEFAULT).       * 00380000
*            UPDATE -- DO UPDATES AND PRINT CHANGES                   * 00390000
*            INSTALL -- IPO INSTALL (SEE IPO DOCUMENTATION)           * 00400000
*                                                                     * 00410000
*********************************************************************** 00420000
         SPACE 2                                                        00430000
PDSUPDTE CSECT                                                          00440000
         USING *,R15          ESTABLISH TEMP. BASE                      00450000
R0       EQU   0                                                        00460000
R1       EQU   1                                                        00470000
R2       EQU   2                                                        00480000
R3       EQU   3                                                        00490000
R4       EQU   4                                                        00500000
R5       EQU   5                                                        00510000
R6       EQU   6                                                        00520000
R7       EQU   7                                                        00530000
R8       EQU   8                                                        00540000
R9       EQU   9                                                        00550000
R10      EQU   10                                                       00560000
R11      EQU   11                                                       00570000
R12      EQU   12                                                       00580000
R13      EQU   13                                                       00590000
R14      EQU   14                                                       00600000
R15      EQU   15                                                       00610000
         B     RTN0F04A                                                 00620000
         SPACE 2                                                        00630000
         DC    C'PDSUPDTE &SYSDATE &SYSTIME'                            00640000
DAT0C01C EQU   *                                                        00650000
         DC    CL46'** USE TO UPDATE FIXED 80 LRECL PO DATASETS **'     00660000
         TITLE 'INITIALIZATION AND PARM EDIT'                           00670000
RTN0F04A DS    0H                                                       00680000
         DROP  R15            DROP TEMPORARY BASE                       00690000
         USING PDSUPDTE,R12,R10                                         00700000
         USING WORKAREA,R11                                             00710000
         STM   R14,R12,12(R13)                                          00720000
         LR    R12,R15                                                  00730000
         LA    R10,2048(R12)                                            00740000
         LA    R10,2048(R10)                                            00750000
         L     R8,0(R1)                                                 00760000
         L     R0,WORKSIZE              GET LENGTH FOR GETMAIN          00770000
         BAL   R1,SUB0C064                                              00780000
SUB0C064 EQU   *                                                        00790000
*   SVC -- G/FMAINR                                                     00800000
         SVC   10                                                       00810000
         LR    R11,R1                                                   00820000
         LR    R14,R11                                                  00830000
         L     R15,WORKSIZE                                             00840000
         LR    R0,R14                                                   00850000
         SLR   R1,R1                                                    00860000
         MVCL  R14,R0                   ZERO THE WORK AREA              00870000
         SPACE                                                          00880000
         LA    R14,SAVEAREA                                             00890000
         ST    R14,8(R13)                                               00900000
         ST    R13,4(R14)                                               00910000
         LR    R13,R14                                                  00920000
         LR    R1,R11                                                   00930000
         A     R1,BUFADMEM              GET OFFSET TO BUFFER IN WORK    00940000
         ST    R1,BUFADMEM              SAVE BUFFER ADDRESS             00950000
         LA    R1,DST0B904                                              00960000
         ST    R1,DST0B0C0                                              00970000
         ZAP   PACKED60(2),DAT0CD78(2)                                  00980000
         MVI   DST0B4DB,X'01'                                           00990000
         MVI   TRTAB,X'01'               INIT. TRTAB TO                 01000000
         MVC   TRTAB+1(255),TRTAB         ALL X'01'                     01010000
         LA    R1,21                     GET NO. SPECIAL CHARACTERS     01020000
         SLR   R3,R3                                                    01030000
RTN0C0AE EQU   *                                                        01040000
         IC    R3,SPECHARS-1(R1)         LOOP TO                        01050000
         LA    R15,TRTAB(R3)              CHANGE SPECIAL                01060000
         MVI   0(R15),X'00'               CHARACTERS TO                 01070000
         BCT   R1,RTN0C0AE               X'00' IN TRTAB                 01080000
         LA    R14,DST0B168                                             01090000
         LA    R15,550                                                  01100000
         LR    R0,R14                                                   01110000
         ICM   R1,8,DAT0CEAE                                            01120000
         MVCL  R14,R0                                                   01130000
         MVI   DST0B24B,X'F1'           CC = TOP OF PAGE                01140000
         MVC   DST0B24C(24),DAT0CE54    GET TITLE = PDS SCANNING AID    01150000
         MVC   DST0B27B(25),DAT0CE6C    SUB-TITLE = CONTROL STATEMENTS  01160000
         MVC   DST0B2AB(5),DAT0CE85      INIT. 'MODE='                  01170000
         MVC   DST0B2BA(5),DAT0CE8A      INIT. 'PAGE:'                  01180000
         MVC   DST0B2B0(7),DAT0CEA5      INIT. 'CHECK  '                01190000
         LA    R1,DST0B790              A(WORK AREA FOR RDJFCB)         01200000
         ST    R1,DCBXLIST              INIT. DCB EXIT LIST             01210000
         MVI   DCBXLIST,X'87'             FOR RDJFCB                    01220000
         L     R1,16                    GET A(CVT)                      01230000
         L     R1,0(R1)                 GET A(NEW/OLD TCB)              01240000
         L     R1,4(R1)                 GET A(OUR TCB)                  01250000
         L     R1,12(R1)                GET A(TIOT)                     01260000
         LA    R1,24(,R1)               POINT TO 1ST DD ENTRY           01270000
         ST    R1,DST0B0C4              SAVE POINTER                    01280000
         LA    R1,DST0B169                                              01290000
         ST    R1,DST0B06C                                              01300000
         MVI   DST0B06C,X'47'                                           01310000
         MVI   DST0B0D0,X'C1'           INIT. OBTAIN CAMLST FOR DSN     01320000
         LA    R1,DST0B844              A(DSN)                          01330000
         ST    R1,DST0B0D4                                              01340000
         LA    R1,DST0B806              A(VOLSER)                       01350000
         ST    R1,DST0B0D8                                              01360000
         LA    R1,DST0B870              A(OBTAIN WORK AREA)             01370000
         ST    R1,DST0B0DC                                              01380000
         BAL   R1,SUB0C144                                              01390000
         DC    X'8F'                               C'.'                 01400000
         DC    AL3(SYSPRINT)            A(SYSPRINT DCB)                 01410000
SUB0C144 EQU   *                                                        01420000
*   SVC -- OPEN  (SYSPRINT,(OUTPUT))                                    01430000
         SVC   19                                                       01440000
         TM    SYSPRINT+48,X'10'        OPEN OK?                        01450000
         BO    RTN0C15C                 YES--CONTINUE                   01460000
         OI    DAT0CD82,X'08'           NO--ISSUE WTO ERR. MSG          01470000
         LA    R1,DAT0CF48                                              01480000
*   SVC -- WTO/WTOR                                                     01490000
         SVC   35                                                       01500000
         B     RTN0C936                                                 01510000
         SPACE 2                                                        01520000
* EDIT THE PARM -- UPDATE, INSTALL, CHECK                               01530000
RTN0C15C EQU   *                                                        01540000
         OI    FLAGS2,X'20'             SHOW SYSPRINT OPEN              01550000
         CLC   0(2,R8),DWORK            PARM SPECIFIED?                 01560000
         BNH   RTN0C1C2                 NO--OK--ASSUME CHECK MODE       01570000
         CLC   0(2,R8),HALFW7           PARM TOO LONG?                  01580000
         BH    RTN0C18C                 YES--ERROR                      01590000
         BE    RTN0C19C                 NO--MUST BE INSTALL MODE        01600000
         CLC   DAT0CE99(7),0(R8)        UPDATE?                         01610000
         BE    RTN0C1B8                 YES--PROCESS UPDATES            01620000
         CLC   DAT0CEA3(7),0(R8)        CHECK MODE?                     01630000
         BE    RTN0C1C2                 YES--PROCESS CHECKS             01640000
RTN0C18C EQU   *                                                        01650000
         OI    DAT0CD82,X'08'           INVALID PARM OR ENVIRONMENT ERR 01660000
         LA    R1,DAT0CF70              INVALID PARM FIELD MSG          01670000
         BAL   R14,SUB0CC66             ISSUE ERROR MSG                 01680000
         B     RTN0C936                 GOTO END OF JOB                 01690000
         SPACE 2                                                        01700000
RTN0C19C EQU   *                                                        01710000
         CLC   HALFW7,0(R8)             PARM LENGTH=7 FOR INSTALL?      01720000
         BNE   RTN0C18C                 NO                              01730000
         OI    FLAGS2,X'10'             FLAG INSTALL MODE/CONSOLE INPUT 01740000
         OI    FLAGS,X'80'              FLAG UPDATE MODE                01750000
         MVC   DST0B2B0(7),DAT0CE91                                     01760000
         B     RTN0C1E2                                                 01770000
         SPACE 2                                                        01780000
RTN0C1B8 EQU   *                                                        01790000
         OI    FLAGS,X'80'              FLAG UPDATE MODE                01800000
         MVC   DST0B2B0(7),DAT0CE9B                                     01810000
RTN0C1C2 OPEN  (SYSIN)                                                  01820000
         TM    SYSIN+48,X'10'               SYSIN OPEN?                 01830000
         BZ    RTN0C1DE                     NO--USE CONSOLE             01840000
         OI    FLAGS2,X'80'                 YES--SHOW SYSIN OPENED      01850000
         B     RTN0C1E2                     CONTINUE                    01860000
         SPACE 2                                                        01870000
RTN0C1DE EQU   *                                                        01880000
         OI    FLAGS2,X'40'                 CONSOLE INPUT--SYSIN ERROR  01890000
RTN0C1E2 EQU   *                                                        01900000
         MVI   DST0B38D,X'FE'                                           01910000
         TITLE 'CONTROL RECORD SCAN/EDIT AND TABLE BUILD'               01920000
*                                                                       01930000
* THIS SECTION WILL LOOP OBTAINING S1<S2<S3< SCAN RECORDS, SAVING       01940000
* THE DATA IN A TABLE AND REPEATING UNTIL EOF (OR THE TABLE IS FULL).   01950000
* THESE RECORDS MAY SPAN MULTIPLE PHYSICAL RECORDS IF - OR + TRAILS THE 01960000
* DELIMITER (<)!                                                        01970000
*                                                                       01980000
* THE CONTROL RECORD TABLE ENTRIES BUILT WILL BE FORMATTED AS FOLLOWS:  01990000
*                                                                       02000000
*     DC   A(NEXT TABLE ENTRY OR 0)                                     02010000
*     DC   AL1(L'S1,L'S2,L'S3)   0=STRING NOT FOUND                     02020000
*     DC   X'00'=FLAGS X'C0'=S1, S2 AND S3 DELIMITERS FOUND; X'01'=     02030000
*                      S1 = S2 (NO UPDATE EFFECTIVELY)                  02040000
*     DC   CL?'S1',CL?'S2',CL?'S3' -- 1 OR MORE MAY BE MISSING--SEE     02050000
*                                     LENGTHS                           02060000
*                                                                       02070000
RTN0C1E6 EQU   *                                                        02080000
         L     R6,DST0B0C0                  GET A(INPUT BUFFER)         02090000
         MVC   0(8,R6),DWORK                RESET FLAGS AND L'FIELDS    02100000
         LA    R9,8(,R6)                    GET A(INPUT RECORD AREA)    02110000
         LA    R4,4(,R6)                    A(TABLE OF FIELD LENGTHS)   02120000
         BAL   R14,READPARM                 GET INPUT RECORD            02130000
*  INPUT FROM CONSOLE VIA WTOR OR SYSIN--R2 POINTS TO INPUT RECORD      02140000
         L     R0,DAT0CD6C                  GET NNAAAAAA WHERE:         02150000
*                                           N= NUMBER OF HIT CHARS = 1  02160000
*                                           A=A(STOP CHAR FOR HIT=<)    02170000
         LA    R1,0(R2)                     GET SCAN START ADDRESS      02180000
         LR    R14,R3                       GET L'RECORD FOR SCAN = 72  02190000
         SLL   R14,24                       SHIFT LENGTH TO HIGH ORDER  02200000
         OR    R1,R14                       COMBINE WITH ADDRESS        02210000
*                                                                       02220000
*  SCAN WHOLE RECORD (IF NECESSARY) FOR < AND DO THE FOLLOWING:         02230000
*     1. IF NONE FOUND AND NOT 'END ', ERROR FOR BAD PARM.              02240000
*     2. IF NONE FOUND AND 'END ', GOTO EOF ON INPUT (SYSIN)            02250000
*     3. EDIT CONTROL RECORD FIELDS (S1<S2<S3<).  FIELDS ARE ISOLATED,  02260000
*        ENTERED IN A TABLE AND FIELD LENGTH SAVED.  NOTE, FIELDS MAY   02270000
*        OPTIONALLY BE ENTERED ON SEPARATE RECORDS WITH A '-' OR '+'    02280000
*        FOLLOWING THE <.                                               02290000
*                                                                       02300000
         BAL   R14,TRTSUBR                  GO DO TRT (RETURN=0 OR +4)  02310000
*                                                                       02320000
* UPON RETURN, THE REGISTERS ARE AS FOLLOWS:                            02330000
*    R0 = OFFSET TO "HIT" OR LENGTH OF FIELD                            02340000
*    R1 = "HIT" ADDRESS     (S/B LESS THAN SYMBOL!)                     02350000
*    R2 = ADDRESS WHERE SCAN CAN CONTINUE ( ADDR OF < + 1)              02360000
*    R15= # BYTES REMAINING TO BE SCANNED                               02370000
* RETURN TO R14 + 0 IF NO HIT AND END OF SCAN                           02380000
* RETURN TO R14 + 4 IF HIT                                              02390000
*                                                                       02400000
         B     PARMERR                      ERROR IF NO HITS            02410000
         SPACE 4                                                        02420000
*                                                                       02430000
*  AT LEAST 1 < FOUND, BEGIN SCAN FOR S1, S2 AND S3                     02440000
*                                                                       02450000
*  FIND END OF STRING 1 -- STRING1<                                     02460000
*                                                                       02470000
RTN0C226 EQU   *                                                        02480000
         BAL   R7,FINDLTS                   SCAN FOR < (LESS THAN SYMB) 02490000
         B     PARMERR                      ERROR IF NOT FOUND          02500000
*  RETURN HERE IF < FOUND!!!!!!!                                        02510000
RTN0C22E EQU   *                                                        02520000
         CLI   4(R6),X'00'                  S1 FIELD FOUND?             02530000
         BE    PARMERR                      NO--ERROR                   02540000
         CLI   0(R2),C'-'                   CONTINUATION AFTER '<' ?    02550000
         BE    RTN0C25E                     YES--CONTINUE               02560000
         CLI   0(R2),C'+'                   CONTINUATION W/LEADING BLK? 02570000
         BNE   RTN0C262                     NO--CONTINUE                02580000
         BAL   R14,READCONT                 YES--READ CONTINUATION REC  02590000
RTN0C24A EQU   *                             AND REMOVE LEADING BLANKS  02600000
         CLI   0(R2),C' '                   BEGIN WITH BLANK?           02610000
         BNE   RTN0C262                     NO--FIND END STRING2        02620000
         LA    R2,1(R2)                     YES--SKIP BLANK             02630000
         BCT   R3,RTN0C24A                  FIND NEXT NON-BLANK         02640000
         B     PARMERR                      ERROR IF NO STRING2         02650000
         SPACE 2                                                        02660000
* CONTINUATION RECORD BUT NO LEADING BLANKS!                            02670000
RTN0C25E EQU   *                                                        02680000
         BAL   R14,READCONT                 READ NEXT RECORD            02690000
*                                                                       02700000
*  FIND END OF STRING 2 -- STRING1<STRING2<                             02710000
*                                                                       02720000
RTN0C262 EQU   *                                                        02730000
         BAL   R7,FINDLTS                   DO TRT FOR <                02740000
         B     PARMERR                      ERROR IF NOT FOUND          02750000
* RETURN HERE IF < FOUND!!!!!                                           02760000
RTN0C26A EQU   *                                                        02770000
         CLI   0(R2),C'-'                   CONTINUATION AT POSITION 1? 02780000
         BE    RTN0C292                     YES                         02790000
         CLI   0(R2),C'+'                   CONTINUATION AFTER BLANKS?  02800000
         BNE   RTN0C296                     NO--FIND STRING3            02810000
         BAL   R14,READCONT                 YES--READ CONTINUATION REC  02820000
RTN0C27E EQU   *                             AND REMOVE LEADING BLANKS  02830000
         CLI   0(R2),X'40'                  BEGIN WITH BLANK?           02840000
         BNE   RTN0C296                     NO--FIND END STRING3        02850000
         LA    R2,1(R2)                     YES--SKIP BLANK             02860000
         BCT   R3,RTN0C27E                  FIND NEXT NON-BLANK         02870000
         B     PARMERR                      ERROR IF NO STRING3         02880000
         SPACE 2                                                        02890000
* READ CONTINUATION RECORD BUT NO LEADING BLANKS!!                      02900000
RTN0C292 EQU   *                                                        02910000
         BAL   R14,READCONT                 READ CONTINUATION REC       02920000
*                                                                       02930000
*  FIND END OF STRING 3 -- STRING1<STRING2<STRING3<                     02940000
*                                                                       02950000
RTN0C296 EQU   *                                                        02960000
         BAL   R7,FINDLTS                   DO TRT FOR <                02970000
         B     RTN0C2B6                     MAY BE ERROR IF NOT FOUND   02980000
* RETURN HERE IF < FOUND!!!!!                                           02990000
RTN0C29E EQU   *                                                        03000000
         CLI   0(R2),C'-'                   CONTINUATION AFTER '<' ?    03010000
         BE    RTN0C2AE                     YES                         03020000
         CLI   0(R2),C'+'                   CONTINUATION W/LEADING BLK? 03030000
         BNE   RTN0C2B6                     NO--                        03040000
RTN0C2AE EQU   *                            YES--READ CONTINUATION REC  03050000
         BAL   R14,READCONT                  WITH/WITHOUT LEADING BLANK 03060000
         B     RTN0C2BE                                                 03070000
         SPACE 2                                                        03080000
RTN0C2B6 EQU   *                                                        03090000
         CLI   0(R2),X'40'                  CONTINUATION AFTER 3RD < ?  03100000
         BH    PARMERR                      YES--ERROR                  03110000
RTN0C2BE EQU   *                                                        03120000
         OI    7(R6),X'C0'                  SHOW S1, S2 AND S3 FOUND    03130000
         CLC   4(1,R6),5(R6)                L'S1 = L'S2?                03140000
         BNE   RTN0C2E8                     NO--S1 CANNOT = S2          03150000
* S1 LENGTH = S2 LENGTH -- NO SPECIAL FIELD HANDLING REQUIRED           03160000
         LA    R15,8(,R6)                   POINT TO S1 IN TABLE        03170000
         LR    R14,R15                      COPY A(S1 FIELD IN TABLE)   03180000
         SLR   R1,R1                        CLEAR FOR IC                03190000
         IC    R1,5(,R6)                    GET L'S2                    03200000
         ALR   R15,R1                       POINT TO S2 STRING IN T.E.  03210000
         BCTR  R1,0                         GET LENGTH FOR EXECUTE      03220000
         EX    R1,EXC0CD30                  S1 = S2?                    03230000
         BNE   RTN0C2E8                     NO--SPECIAL HANDLING REQ'D  03240000
         OI    7(R6),X'01'                  YES--FLAG S1 = S2           03250000
RTN0C2E8 EQU   *                                                        03260000
         ST    R9,DST0B0C0                  SAVE A(NEXT CNTL REC T.E.)  03270000
         LA    R15,DST0B904                 GET A(CNTL REC. TABLE)      03280000
         LA    R15,4092(,R15)               GET A(END OF CNTL. REC TAB) 03290000
         CR    R9,R15                       TABLE FULL?                 03300000
         BH    RTN0C33A                     YES--BEGIN PROCESSING       03310000
         SLR   R7,R7                        CLEAR FOR IC                03320000
         IC    R7,8(,R6)                    GET 1ST CHAR OF S1          03330000
         LA    R7,TRTAB2(R7)                 AS INDEX INTO TRT TABLE    03340000
         MVI   0(R7),X'01'                    AND SET 'HIT' ON CHAR     03350000
         LA    R3,BUFADTAB                  GET A(CNTL REC T.E. ADDR)   03360000
RTN0C30C EQU   *                                                        03370000
         ICM   R1,15,0(R3)                  IS THIS ENTRY FREE?         03380000
         BZ    RTN0C31A                     YES--USE IT                 03390000
         LR    R3,R1                        NO--NEXT TABLE ENTRY        03400000
         B     RTN0C30C                     LOOP UNTIL END OF ADDRESSES 03410000
         SPACE 2                                                        03420000
RTN0C31A EQU   *                                                        03430000
*  THIS ESSENTIALLY COMPRESSES INPUT BUFFERS CONTAINING S1<S2<S3<       03440000
*  PARAMETERS INTO MUCH SMALLER AREA AND THEN THE AREA SAVED IS USED    03450000
*  TO BEGIN THE NEXT INPUT AREA!!!                                      03460000
         STCM  R6,15,0(R3)                  STORE A(CNTL REC T.E.)      03470000
         B     RTN0C1E6                     READ ANOTHER SYSIN RECORD   03480000
         SPACE 2                                                        03490000
*                                                                       03500000
* ERROR DETECTED IN PARM RECORD -- ISSUE ERROR MSG                      03510000
*                                                                       03520000
RTN0C322 EQU   *                                                        03530000
PARMERR  EQU   *                                                        03540000
         LA    R1,DAT0CFD8                  INVALID SYNTAX MSG          03550000
         BAL   R14,SUB0CC66                 ISSUE ERROR MESSAGE         03560000
         TM    FLAGS2,X'40'                 SYSIN ERROR--USE CONSOLE?   03570000
         BO    RTN0C1E6                     YES--SKIP SYSIN PARM ERROR  03580000
         OI    DAT0CD82,X'08'               NO--FLAG SYSIN PARM ERROR   03590000
         B     RTN0C1E6                     PROCESS NEXT INPUT PARM     03600000
         SPACE 2                                                        03610000
RTN0C33A EQU   *                                                        03620000
         LA    R1,DAT0A0E8                  TOO MANY CONTROL RECS. MSG  03630000
         BAL   R14,SUB0CC66                 ISSUE ERROR MSG             03640000
         OI    DAT0CD82,X'08'               FLAG SYSIN PARM ERROR       03650000
*                                                                       03660000
* END OF CONTROL CARD SCAN.  END OF JOB IF ERRORS, CLOSE SYSIN IF OPEN, 03670000
* BEGIN PDS PROCESSING IF AT LEAST ONE ACCEPTABLE CONTROL RECORD FOUND. 03680000
*                                                                       03690000
RTN0C346 EQU   *                                                        03700000
EOFSYSIN EQU   *                                                        03710000
         TM    FLAGS2,X'80'                 SYSIN OPEN?                 03720000
         BZ    RTN0C35A                     NO--SKIP CLOSE              03730000
         CLOSE (SYSIN)                      YES--CLOSE IT               03740000
RTN0C35A EQU   *                                                        03750000
         L     R1,BUFADTAB                  GET A(BUFFER ADDR TABLE)    03760000
         TM    DAT0CD82,X'08'               SYSIN PARM/ENVIRONMENT ERR? 03770000
         BO    RTN0C936                     YES--GOTO END OF JOB        03780000
         LTR   R1,R1                        NO--ANY ACCEPTED PARMS?     03790000
         BNZ   RTN0C3BE                     YES--GO PROCESS THEM        03800000
         OI    DAT0CD82,X'01'               NO--SET FLAG / RC FOR ERROR 03810000
         B     RTN0C936                     GOTO END OF JOB             03820000
         EJECT                                                          03830000
*                                                                       03840000
*  THIS SUBROUTINE WILL SCAN THE PARM/CONTROL RECORDS (S1<S2<S3<) AND   03850000
*  ISOLATE EACH FIELD.  THE ROUTINE IS REENTERED WHERE IT LEFT OFF      03860000
*  UNTIL THE WHOLE RECORD HAS BEEN SCANNED.                             03870000
*                                                                       03880000
*  ON ENTRY, THE REGISTERS ARE AS FOLLOWS:                              03890000
*                                                                       03900000
*     R2 = ADDRESS TO START/CONTINUE SCAN AT                            03910000
*     R3 = REMAINING LENGTH FOR SCAN                                    03920000
*     R4 = A(4 BYTES TO CONTAIN L'S1, L'S2, L'S3, AND FLAG BYTE)        03930000
*     R7 = RETURN ADDRESS (+0 IF ERROR, +4 IF SCAN OK)                  03940000
*     R9 = A(VARIABLE LENGTH AREA TO CONTAIN S1,S2,S3)                  03950000
*                                                                       03960000
*  NOTE--R4 AND R9 POINT TO A CNTL. RECORD TABLE ENTRY WHICH IS IN      03970000
*        THE PROCESS OF BEING FORMATTED.  SINCE THIS ROUTINE IS         03980000
*        ENTERED MULTIPLE TIMES THE ABOVE COMMENTS ARE NOT REALLY       03990000
*        CORRECT.  R9 AND R4 POINT TO THE START OF THE AREA TO          04000000
*        CONTAIN THE APPROPRIATE STRING AND ITS LENGTH.  ON EXIT        04010000
*        EACH REGISTER IS BUMPED SO IT WILL POINT TO THE AREA FOR       04020000
*        THE NEXT STRING!                                               04030000
*                                                                       04040000
FINDLTS  EQU   *                                                        04050000
         L     R0,DAT0CD6C                  R0=NNAAAAAA                 04060000
*                                             N=# HIT CHAR FOR TRT=1    04070000
*                                             A=A(HIT CHAR STRING (<))  04080000
         LA    R1,0(R2)                     GET A(START OF SCAN)        04090000
         LR    R14,R3                       GET REMAINING SCAN LENGTH   04100000
         SLL   R14,24                       CONBINE LENGTH AND ADDR     04110000
         OR    R1,R14                        FOR SCAN ROUTINE           04120000
*                                           R1=LLAAAAAA                 04130000
*                                           L=LENGTH OF SCAN            04140000
*                                           A=A(START OF FIELD TO SCAN) 04150000
         BAL   R14,TRTSUBR                  DO TRT FOR <                04160000
         B     0(R7)                        RETURN IF NO "HIT"          04170000
* RETURN HERE IF < FOUND!!!!!                                           04180000
RTN0C38C EQU   *                                                        04190000
         LA    R14,72                       FIELD LENGTH MAX.= 71       04200000
         CLR   R0,R14                       FIELD LENGTH OK?            04210000
         BNL   0(R7)                        NO--ERROR RETURN = +0       04220000
         LTR   R5,R0                        YES--FIELD LENGTH OK?       04230000
         BNP   RTN0C3AA                     NO--END SCAN                04240000
         STC   R5,0(R4)                     SAVE STRING LENGTH IN T.E.  04250000
         BCTR  R5,0                         GET LENGTH FOR EX MVC       04260000
         EX    R5,SAVSTRNG                  MOVE STRING TO TABLE ENTRY  04270000
         LA    R9,1(R9,R5)                  ADDR. FOR NEXT STRING IN TE 04280000
RTN0C3AA EQU   *                                                        04290000
         LA    R2,1(R1)                     GET SCAN CONTINUE ADDRESS   04300000
         LR    R3,R15                       GET REMIAINING SCAN LENGTH  04310000
         LA    R4,1(R4)                     NEXT STRING LENGTH IN T.E.  04320000
         B     4(R7)                        END OF SCAN--SCAN OK RETURN 04330000
         SPACE 2                                                        04340000
SAVSTRNG MVC   0(1,R9),0(R2)                ** MOVES STRING TO TABLE ** 04350000
         TITLE 'TIOT SCAN FOR @DDNAMES AND DATASET VALIDATION'          04360000
*                                                                       04370000
*  BEGIN PROCESSING AFTER ALL PARMS HAVE BEEN READ, VALIDATED AND       04380000
*  STORED IN TABLES.                                                    04390000
*                                                                       04400000
RTN0C3BE EQU   *                                                        04410000
         LA    R1,DAT0A004            GET A(END OF SYSIN PARMS MSG)     04420000
         BAL   R14,SUB0CC66           DISPLAY OR PRINT IT               04430000
         L     R1,16                  A(CVT)                            04440000
         L     R1,0(R1)               A(NEW/OLD TCB)                    04450000
         L     R1,4(R1)               A(OUR TCB)                        04460000
         L     R1,12(R1)              A(TIOT)                           04470000
         LA    R1,24(,R1)             A(1ST DD ENTRY IN TIOT)           04480000
         ST    R1,DST0B0C4            SAVE DD ENTRY ADDR                04490000
         TM    FLAGS,X'80'            UPDATE MODE?                      04500000
         BO    RTN0C3EC               YES--LEAVE OPEN OUTPUT            04510000
         MVC   DAT0CD58(4),DAT0CD5C   CHANGE TO OPEN INPUT              04520000
*                                                                       04530000
* PROCESS A (OR NEXT) @XXX DD ENTRY                                     04540000
*                                                                       04550000
RTN0C3EC EQU   *                                                        04560000
         NI    FLAGS,X'80'            LEAVE ONLY UPDATE FLAG (IF ON)    04570000
         SLR   R15,R15                                                  04580000
         L     R14,DST0B0C4           GET A(1ST/NEXT DD ENTRY)          04590000
RTN0C3F6 EQU   *                                                        04600000
         ICM   R15,1,0(R14)           GET L'DD ENTRY                    04610000
         BZ    RTN0C936               BRANCH IF END OF TIOT             04620000
         CLI   4(R14),C'@'            DD ENTRY FOR PDS TO BE UPDATED?   04630000
         BE    RTN0C40C               YES--PROCESS IT IF IT IS OK       04640000
         AR    R14,R15                NO--NEXT DD ENTRY                 04650000
         B     RTN0C3F6               CONTINUE TIOT SCAN                04660000
         SPACE 2                                                        04670000
RTN0C40C EQU   *                                                        04680000
         AR    R15,R14                POINT TO NEXT DD ENTRY IN TIOT    04690000
         ST    R15,DST0B0C4           SAVE A(ENTRY)                     04700000
         ZAP   PACKED60(2),DAT0CD78(2)                                  04710000
         LR    R3,R14                                                   04720000
         ICM   R1,7,17(R14)           GET A(UCB)                        04730000
         BZ    RTN0C4BC               BRANCH IF NOT ALLOCATED           04740000
         CLI   18(R1),X'20'           DASD?                             04750000
         BNE   RTN0C4BC               NO--ERROR                         04760000
         MVC   PDSDATA+40(8),4(R14)   YES--DDNAME TO                    04770000
         MVC   PDSDIR+40(8),4(R14)       DCB'S                          04780000
         LA    R1,DAT0CD58                                              04790000
*   SVC -- RDJFCB FOR PDS                                               04800000
         SVC   64                                                       04810000
         MVC   DST0B27A(7),DAT0A1E4                                     04820000
         MVC   DST0B281(40),DST0B790  SAVE DSNAME                       04830000
         MVC   DST0B26F(4),DAT0A1DC                                     04840000
         MVC   DST0B273(6),DST0B806   SAVE VOLSER                       04850000
         MVC   DST0B844(44),DST0B790  SAVE DSNAME                       04860000
         MVI   DST0B7C4,X'08'         FLAG JFCB NOT TO BE REWRITTEN     04870000
         OI    DST0B7DC,X'80'                                           04880000
         LA    R1,DST0B0D0                                              04890000
*   SVC -- OBTAIN FORMAT 1 DSCB FOR PDS                                 04900000
         SVC   27                                                       04910000
         LTR   R15,R15                                                  04920000
         BNZ   RTN0C4BC                                                 04930000
         TM    DST0B898,X'C0'         RECFM SPECIFIED?                  04940000
         BNM   RTN0C4BC               NO--ERROR                         04950000
         TM    DST0B898,X'80'         RECFM = F?                        04960000
         BNO   RTN0C4BC               NO--ERROR                         04970000
         LA    R15,80                                                   04980000
         CH    R15,DST0B89C           LRECL=80?                         04990000
         BNE   RTN0C4BC               NO--ERROR                         05000000
         TM    DST0B896,X'02'         DSORG=PO?                         05010000
         BNO   RTN0C4BC               NO--ERROR                         05020000
         MVI   DST0B158,X'00'                                           05030000
         MVC   MEMNAME,DST0B7BC                                         05040000
         MVC   DST0B2C6(8),DST0B7BC                                     05050000
         TM    DST0B7E6,X'01'         MEMBER OF PDS SPECIFIED IN JCL?   05060000
         BZ    PROCDIR                NO--CONTINUE                      05070000
         OI    FLAGS,MEMBONLY         YES--SCAN ONLY 1 MEMBER           05080000
         MVC   DST0B7BC(8),DST0B1BB   SAVE THE MEMBER NAME              05090000
         NI    DST0B7E6,X'FE'         CHANGE JFCB FOR WHOLE DATASET     05100000
         B     PROCDIR                                                  05110000
         SPACE 2                                                        05120000
RTN0C4BC EQU   *                                                        05130000
         OI    DAT0CD82,X'01'                                           05140000
         MVC   DAT0A040(8),4(R3)                                        05150000
         LA    R1,DAT0A030                                              05160000
         BAL   R14,SUB0CC66                                             05170000
         B     RTN0C3EC                                                 05180000
         TITLE 'PROCESS THE PDS DIRECTORY'                              05190000
         DC    C'PROCPDS '                                              05200000
*   SVC -- OPENJ PDSDATA,XX,MF=(E,DAT0CD58) XX=UPDAT/INPUT (SEE PARM)   05210000
PROCDIR  LA    R1,DAT0CD58        GET OPEN LIST (UPDAT/INPUT MODE)      05220000
         SVC   22                 DO OPENJ                              05230000
         BAL   R1,SUB0C4E8                                              05240000
         DC    X'80'                               C'.'                 05250000
         DC    AL3(PDSDIR)                                              05260000
SUB0C4E8 EQU   *                                                        05270000
*   SVC -- OPENJ PDSDIR,INPUT                                           05280000
         SVC   22                                                       05290000
         LA    R1,PDSDATA                                               05300000
         LA    R0,DAT0C01C              COMMENT WAS $$$$$IBM            05310000
         LCR   R1,R1                                                    05320000
*   SVC -- FIND MEMBER OF PDS (NOP'D--WAS IBM CK FOR MEMBER $$$$$IBM)   05330000
         SVC   18                                                       05340000
         LTR   R15,R15                                                  05350000
*        BZ    RTN0C508                 NOP'D--SEE COMMENT ABOVE        05360000
         B     RTN0C508                 DISREGARD THE RETURN CODE       05370000
         LA    R1,DAT0A164                                              05380000
         BAL   R14,SUB0CC66                                             05390000
         B     RTN0C5B6                                                 05400000
         SPACE 2                                                        05410000
RTN0C508 EQU   *                                                        05420000
         LA    R6,DIRRECAD              GET BUFFER ADDRESS FOR DIR.     05430000
*        READ  DIRDECB,SF,*,(6),'S'     READ A DIRECTORY RECORD         05440000
         LA    R1,DIRDECB               GET A(DECB FOR READING DIR.)    05450000
         MVI   5(R1),X'80'              SET FOR 'S' LENGTH ON READ      05460000
         ST    R6,12(R1)                INIT BUFFER ADDRESS IN DECB     05470000
         L     R15,8(R1)                GET A(DCB)                      05480000
         L     R15,48(,R15)             GET A(BSAM ACCESS METHOD)       05490000
         BALR  R14,R15                  READ DIRECTORY                  05500000
*        CHECK DIRDECB                  CHECK DIRECTORY READ            05510000
         LA    R1,DIRDECB               GET A(DIR. DECB)                05520000
         L     R14,8(,R1)               GET A(DIR. DCB)                 05530000
         L     R15,52(,R14)             GET A(CHECK ROUTINE)            05540000
         BALR  R14,R15                  CHECK I/O                       05550000
         LA    R1,2                                                     05560000
         STH   R1,DEOFFSET              INIT. MIN. DIR. BYTES USED      05570000
RTN0C538 EQU   *                                                        05580000
         LH    R4,DEOFFSET                                              05590000
         CH    R4,DIRRECAD              ANY MEMBERS IN THIS DIR. ENTRY? 05600000
         BNL   RTN0C508                 NO--SKIP DIRECTORY ENTRY        05610000
         LA    R5,DST4B690(R4)          POINT TO MEMBER NAME            05620000
         CLI   0(R5),X'FF'              EOF ON DIRECTORY?               05630000
         BE    RTN0C5B6                 YES--CLOSE DIR AND DATA DCB'S   05640000
         IC    R1,11(,R5)               GET # HALFWORDS IN THIS ENTRY   05650000
         LA    R0,31                    GET MASK FOR ANDING             05660000
         NR    R1,R0                    REMOVE EXTRANEOUS BITS          05670000
         AR    R1,R1                    GET # BYTES (2 * #HALFWORDS)    05680000
         LA    R1,12(R1,R4)             GET OFFSET TO NEXT DIR. ENTRY   05690000
         STH   R1,DEOFFSET              SAVE OFFSET                     05700000
         OI    DST0B158,X'01'                                           05710000
         TM    FLAGS,MEMBONLY           SINGLE MEMBER SCAN?             05720000
         BZ    RTN0C582                 NO--CONTINUE                    05730000
         CLC   0(8,R5),MEMNAME          IS THIS THE MEMBER SPECIFIED?   05740000
         BL    RTN0C538                 NO--CONTINUE DIR. SCAN          05750000
         BE    RTN0C58A                 YES--PROCESS IT                 05760000
         B     RTN0C5B6                 NOT FOUND--CLOSE DCB'S          05770000
         SPACE 2                                                        05780000
*                                                                       05790000
*  BEGIN PROCESSING OF MEMBERS FROM DIRECTORY--SKIP ALIAS MEMBERS, THO! 05800000
*                                                                       05810000
RTN0C582 EQU   *                                                        05820000
         TM    11(R5),X'80'             ALIAS?                          05830000
         BO    RTN0C538                 YES--SKIP IT                    05840000
RTN0C58A EQU   *                                                        05850000
         MVI   CC,X'F0'                 SET FOR DOUBLE SPACING          05860000
         MVC   MEMNAME,0(R5)            INIT. MEMBERNAME FOR POINT      05870000
         MVC   TTR(3),8(R5)             SAVE TTR OF MEMBER              05880000
         LA    R1,PDSDATA                                               05890000
         LA    R0,TTR                                                   05900000
         L     R15,84(,R1)              A(POINT ROUTINE)                05910000
         BAL   R14,4(R15)               DO POINT                        05920000
         BAL   R14,PROCDATA             GO PROCESS THIS MEMBER          05930000
         TM    FLAGS,X'10'                                              05940000
         BZ    RTN0C538                                                 05950000
RTN0C5B6 CLOSE (PDSDIR,,PDSDATA)        CLOSE PDS DIRECTORY AND DATA    05960000
         TM    FLAGS,X'40'                                              05970000
         BO    RTN0C3EC                 GET NEXT DD ENTRY               05980000
         MVC   DAT0A128(8),DAT0A2D8     SAVE DDNAME JUST PROCESSED      05990000
         LA    R1,DAT0A118                                              06000000
         BAL   R14,SUB0CC66                                             06010000
         OI    DAT0CD82,X'01'                                           06020000
         B     RTN0C3EC                                                 06030000
         TITLE 'PROCESS THE MEMBER OF THE PDS--READ AND SCAN'           06040000
         DC    C'PROCDATA'                                              06050000
PROCDATA EQU   *                                                        06060000
         STM   R14,R12,12(R13)                                          06070000
         LA    R14,NEWSAVE                                              06080000
         ST    R13,4(R14)                                               06090000
         ST    R14,8(R13)                                               06100000
         LR    R13,R14                                                  06110000
RTN0C5FE EQU   *                                                        06120000
NEXTBLOK L     R9,BUFADMEM              GET A(MEMBER BUFFER)            06130000
         LA    R7,MEMBDECB              GET A(DECB FOR MEMBER)          06140000
         TM    FLAGS,X'20'              WAS PREVIOUS RECORD CHANGED?    06150000
         BZ    RTN0C636                 NO--SKIP WRITE                  06160000
         NI    FLAGS,X'DF'              YES--TURN OFF WRITE FLAG        06170000
         TM    FLAGS,X'80'              UPDATE MODE PROCESSING?         06180000
         BZ    RTN0C636                 NO--SKIP WRITE                  06190000
*        WRITE (7),SF,*,*,'S'           REWRITE RECORD "IN-PLACE"       06200000
         LR    R1,R7                    GET A(DECB)                     06210000
         MVI   5(R1),X'20'              SET DECB FOR WRITE SF           06220000
         L     R15,8(R1)                GET A(DCB)                      06230000
         L     R15,48(,R15)             GET A(WRITE ROUTINE)            06240000
         BALR  R14,R15                  DO WRITE                        06250000
         LR    R1,R7                    GET A(DECB)                     06260000
         L     R14,8(,R1)               GET A(DCB)                      06270000
         L     R15,52(,R14)             GET A(CHECK ROUTINE)            06280000
         BALR  R14,R15                  DO CHECK                        06290000
RTN0C636 EQU   *                                                        06300000
*        READ  (7),SF,PDSDATA,(9),'S'   DO SEQ. BSAM READ               06310000
         LR    R1,R7                    GET A(DECB)                     06320000
         MVI   5(R1),X'80'              SET DECB FOR READ SF            06330000
         LA    R14,PDSDATA              GET A(DCB)                      06340000
         ST    R14,8(R1)                INIT. DCB ADDR IN DECB          06350000
         ST    R9,12(R1)                INIT. BUFFER ADDR IN DECB       06360000
         MVI   4(R1),X'80'              INIT. READ LENGTH TO 'S'        06370000
         L     R15,8(R1)                GET A(DCB)                      06380000
         L     R15,48(,R15)             GET A(BSAM READ LOGIC)          06390000
         BALR  R14,R15                  DO READ                         06400000
*        CHECK (7)                      CHECK THE READ                  06410000
         LR    R1,R7                    GET A(DECB)                     06420000
         L     R14,8(,R1)               GET A(DCB)                      06430000
         L     R15,52(,R14)             GET A(CHECK LOGIC)              06440000
         BALR  R14,R15                  DO CHECK                        06450000
         LH    R5,PDSDATA+DCBBLKSI-IHADCB GET BLKSIZE FROM DCB          06460000
         L     R4,16(R7)                GET A(IOB) FROM DECB            06470000
         SH    R5,14(R4)                SUBTRACT RESIDUAL COUNT         06480000
         ST    R5,ACTBLKSZ              SAVE ACTUAL BLKSIZE READ        06490000
         XC    RECOFSET(4),RECOFSET     INITIAL RECORD OFFSET           06500000
RTN0C678 EQU   *                                                        06510000
GETREC   L     R6,RECOFSET              GET OFFSET TO NEXT RECORD       06520000
         C     R6,ACTBLKSZ              ANOTHER RECORD IN THIS BLOCK?   06530000
         BNL   NEXTBLOK                 NO--READ NEXT BLOCK             06540000
* LRECL=80 ASSUMED IN NEXT INSTRUCTION                                  06550000
         LA    R5,80(R6)                GET OFFSET TO NEXT RECORD       06560000
         ST    R5,RECOFSET              SAVE FOR NEXT PASS              06570000
         A     R6,BUFADMEM              GET A(CURRENT RECORD)           06580000
         ST    R6,CURRECAD              SAVE IT                         06590000
         MVI   CURRECAD,X'50'           RECORD LENGTH = 80              06600000
* TRT ASSUMES SCAN LENGTH = 71                                          06610000
         TRT   0(71,R6),TRTAB2          SCAN FOR 1ST CHAR. OF STRING    06620000
         BZ    GETREC                   BRANCH IF NO HIT DURING TRT     06630000
         MVC   DST0B169(71),0(R6)       SAVE BEFORE IMAGE (DATA)        06640000
         MVC   DST0B1B2(9),71(R6)        AND CONTINUATION PLUS SEQ #    06650000
         MVI   DST0B15A,X'00'                                           06660000
         TM    FLAGS,X'02'              ON IF .RU. (RESUME UPD) FOUND   06670000
         BO    RTN0C6EE                 BRANCH IF UPDATES ALLOWED       06680000
         TM    FLAGS,X'01'              .NU. (NO UPDATE) IN EFFECT?     06690000
         BO    RTN0C6D6                 YES--SCAN FOR .RU. (RESUME UPD) 06700000
*                                                                       06710000
* SCAN FOR .NU. (NO UPDATE) STRING IN RECORD                            06720000
*                                                                       06730000
         L     R0,DAT0CD70              R0=NNAAAAAA                     06740000
*                                        N = # HIT CHARS. FOR TRT       06750000
*                                        A = A(STRING OF HIT CHARS)     06760000
         L     R1,DST0B06C              R1=LLAAAAAA                     06770000
*                                        L = LENGTH FOR TRT SCAN        06780000
*                                        A = A(START OF FIELD TO SCAN)  06790000
         BAL   R14,TRTSUBR              DO TRT SCAN                     06800000
         B     RTN0C6EE                 .NU. NOT FOUND -- DO UPDATES    06810000
         SPACE 2                                                        06820000
RTN0C6D2 EQU   *                                                        06830000
         OI    FLAGS,X'01'              SHOW .NU. NO UPDATES IN EFFECT  06840000
*                                                                       06850000
* SCAN FOR .RU. (RESUME UPDATE) STRING IN RECORD                        06860000
*                                                                       06870000
RTN0C6D6 EQU   *                                                        06880000
         L     R0,DAT0CD74              R0=NNAAAAAA                     06890000
*                                        N=# HIT CHAR FOR TRT           06900000
*                                        A=A(STRING OF HIT CHAR)        06910000
         L     R1,DST0B06C              R1=LLAAAAAA                     06920000
*                                        L=LENGTH OF SCAN               06930000
*                                        A=A(START OF FIELD TO SCAN)    06940000
         BAL   R14,TRTSUBR              DO TRT                          06950000
         B     GETREC                   .RU. NOT FOUND--SKIP RECORD     06960000
         SPACE 2                                                        06970000
RTN0C6E6 EQU   *                                                        06980000
         NI    FLAGS,X'FE'              .RU. FOUND--.NU. FLAG OFF       06990000
         B     GETREC                   GET NEXT RECORD                 07000000
         SPACE 2                                                        07010000
*                                                                       07020000
* NO .NU. OR .RU. FOUND -- UPDATES ALLOWED AT THIS POINT                07030000
*                                                                       07040000
RTN0C6EE EQU   *                                                        07050000
         LA    R9,BUFADTAB              GET A(CNTL. REC. TABLE)         07060000
RTN0C6F2 EQU   *                                                        07070000
         ICM   R9,15,0(R9)              GET 1ST/NEXT CNTL. REC. T.E.    07080000
         BZ    RTN0C8A0                 BRANCH IF DONE                  07090000
         MVC   DST0B060,DST0B06C                                        07100000
         XC    DST0B064,DST0B064        ZERO NEXT FULLWORD              07110000
RTN0C706 EQU   *                                                        07120000
         L     R1,DST0B064                                              07130000
         LA    R0,8                                                     07140000
         CR    R1,R0                    IS THERE AN S1 STRING?          07150000
         BH    RTN0C6F2                 NO--NEXT BUFFER                 07160000
*                                                                       07170000
*  CNTL. REC. TABLE -- ENTRIES FORMATTED AS FOLLOWS:                    07180000
*     R9===> A(NEXT ENTRY OR 0)                                         07190000
*            4AL1(L'S1,L'S2,L'S3,FLAGS)--LENGTH=0 MEANS NO STRING       07200000
*                                        FOLLOWS                        07210000
*            CL?'S1',CL?'S2',CL?'S3' <== MAY BE NO EXISTENT!!!          07220000
*                                                                       07230000
         LA    R4,8(,R9)                GET A(STRING TO SCAN FOR)       07240000
         SLR   R5,R5                    CLEAR FOR IC                    07250000
         IC    R5,4(,R9)                GET LENGTH OF STRING # 1        07260000
         LA    R0,0(R4)                 GET A(S1)                       07270000
         LR    R14,R5                   GET LENGTH OF S1 STRING ...     07280000
         SLL   R14,24                     SHIFT LENGTH TO HIGH ORDER    07290000
         OR    R0,R14                   R0=NNAAAAAA                     07300000
*                                        N = # HIT CHARS FOR TRT        07310000
*                                        A = A(STRING OF HIT CHARS)     07320000
         L     R1,DST0B060              R1=LLAAAAAA                     07330000
*                                        L = LENGTH OF FIELD TO SCAN    07340000
*                                        A = A(START OF FIELD TO SCAN)  07350000
         BAL   R14,TRTSUBR              TRT SCAN FOR S1                 07360000
         B     RTN0C6F2                 NOT FOUND--NEXT CNTL. REC. T.E. 07370000
         SPACE 2                                                        07380000
*  S1 STRING FOUND                                                      07390000
RTN0C736 LR    R8,R1                    SAVE "HIT" ADDRESS FOR S1       07400000
         LA    R14,1(R1)                GET CONTINUATION ADDRESS        07410000
         ST    R14,DST0B060             SAVE FOR LATER                  07420000
         STC   R15,DST0B060             SAVE REMAINING SCAN BYTES       07430000
*                                                                       07440000
* I DON'T KNOW WHAT THIS IS THAT FOLLOWS!!  IT APPEARS TO BE SOME KIND  07450000
* OF SPECIAL STRING SCAN BUT IT SCREWS UP THE NORMAL FUNCTIONS SO WILL  07460000
* BE BYPASSED (FOR NOW ANYWAY!).                                        07470000
*                                                                       07480000
         B     RTN0C780                 IGNORE THIS ROUTINE!!!!         07490000
         BCTR  R1,0                     BACK UP 1 BYTE                  07500000
         TM    7(R9),X'80'              S2 STRING?                      07510000
         BZ    RTN0C76C                 NO                              07520000
         TRT   0(1,R1),TRTAB            PRECEEDED BY NON-SPEC. CHAR?    07530000
         BZ    RTN0C76C                 NO--CONTINUE                    07540000
         CLI   0(R1),X'E5'              PRECEEDED BY V?                 07550000
         BNE   RTN0C706                 NO--SKIP--CANNOT BE MATCH       07560000
         LR    R14,R1                   PREPARE TO BACKUP AGAIN         07570000
         BCTR  R14,0                    BACKUP 1 MORE BYTE              07580000
         CLI   0(R14),X'4B'             PRECEEDED BY '.V'????           07590000
         BNE   RTN0C706                 NO--SKIP--CANNOT BE MATCH       07600000
RTN0C76C EQU   *                                                        07610000
         TM    7(R9),X'40'              S3 STRING?                      07620000
         BZ    RTN0C780                 NO                              07630000
         AR    R1,R5                    POINT TO END OF S1 + 1          07640000
         TRT   1(1,R1),TRTAB            FOLLOWED BY SPECIAL CHARACTER?  07650000
         BNZ   RTN0C706                 NO--SKIP--CANNOT BE MATCH       07660000
         SPACE 4                                                        07670000
RTN0C780 EQU   *                                                        07680000
         CLI   6(R9),X'00'              S3 SPECIFIED?                   07690000
         BE    RTN0C7AC                 NO--START UPDATE CHAECKING      07700000
         AR    R4,R5                    YES--POINT TO S2                07710000
         IC    R5,5(,R9)                GET L'S2                        07720000
         AR    R4,R5                    POINT PAST S2 TO S3             07730000
         IC    R5,6(,R9)                GET L'S3                        07740000
         LA    R0,0(R4)                 R0 = AL1(0),AL3(S3)             07750000
         LR    R14,R5                   COPY L'S3 ...                   07760000
         SLL   R14,24                    AND SHIFT IT TO LOW ORDER BYTE 07770000
         OR    R0,R14                   R0=NNAAAAAA                     07780000
*                                        N = # HIT CHARS FOR TRT=L'S3   07790000
*                                        A = A(STRING OF HIT CHARS=S3)  07800000
         L     R1,CURRECAD              R1=LLAAAAAA                     07810000
*                                        L = LENGTH OF MEMBER RECORD    07820000
*                                        A = A(START OF MEMBER RECORD)  07830000
         BAL   R14,TRTSUBR              DOES S3 EXIST?                  07840000
         B     RTN0C706                 NO--CANNOT BE MATCH--SKIP       07850000
         SPACE 2                                                        07860000
RTN0C7AC EQU   *                                                        07870000
         TM    7(R9),X'01'              UPDATE REQUIRED (S1=S2)?        07880000
         BO    RTN0C7BC                 NOT REALLY IF S1=S2             07890000
         OI    DST0B15A,X'80'           YES--FLAG REAL UPDATE REQ'D     07900000
         B     RTN0C7C0                 CONTINUE                        07910000
         SPACE 2                                                        07920000
RTN0C7BC EQU   *                                                        07930000
         OI    DST0B15A,X'20'           MATCH BUT NO UPDATE REQ'D       07940000
RTN0C7C0 EQU   *                                                        07950000
         SLR   R3,R3                    CLEAR FOR IC                    07960000
         IC    R3,4(,R9)                GET LENGTH OF S1                07970000
         LA    R5,0(R8,R3)              POINT PAST S1                   07980000
         LA    R6,DST0B1B0              POINT TO END OF RECORD          07990000
         SR    R6,R5                    ROOM TO INSERT S2?              08000000
         BM    RTN0C920                 NO--CANNOT DO UPDATE            08010000
         SLR   R4,R4                    CLEAR FOR IC                    08020000
         IC    R4,5(,R9)                GET L'S2                        08030000
         SR    R3,R4                    WILL S2 FIT?                    08040000
         BZ    RTN0C862                 YES--CONTINUE                   08050000
         BP    RTN0C82C                 MAYBE--ONLY IF THERE ARE BLANKS 08060000
         LPR   R3,R3                    MAYBE--ONLY IF THERE ARE BLANKS 08070000
*                                                                       08080000
* NEED TO SHIFT LEFT AFTER UPDATE--CHECK FOR TRAILING BLANKS            08090000
*                                                                       08100000
         LA    R7,1(R6)                 GET # BYTES LEFT IN RECORD      08110000
RTN0C7EA EQU   *                                                        08120000
         L     R0,DAT0CD68              R0=NNAAAAAA                     08130000
*                                        N = # HIT CHARS FOR TRT=2      08140000
*                                        A = A(STRING OF HIT CHARS=  )  08150000
         LA    R1,0(R5)                 A(1ST BYTE BEYOND S1 IN RECORD) 08160000
         LR    R14,R7                   GET # REMAINING BYTES IN RECORD 08170000
         SLL   R14,24                   SHIFT LENGTH TO HIGH ORDER BYTE 08180000
         OR    R1,R14                   R1=LLAAAAAA                     08190000
*                                        L = LENGTH OF FIELD TO SCAN    08200000
*                                        A = A(START OF FIELD TO SCAN)  08210000
         BAL   R14,TRTSUBR              SCAN FOR CONTIGUOUS BLANKS      08220000
         B     RTN0C920                 NONE FOUND--CANNOT UPDATE       08230000
         SPACE 2                                                        08240000
RTN0C802 EQU   *                                                        08250000
         LTR   R2,R0                    BLANKS AT END?                  08260000
         BNP   RTN0C812                 NO--CONTINUE                    08270000
         BCTR  R2,0                                                     08280000
         EX    R2,EXC0C820              SAVE TRAILING CHARACTERS        08290000
         EX    R2,EXC0C826              INSERT BLANKS                   08300000
RTN0C812 EQU   *                                                        08310000
         LA    R5,1(,R5)                NEXT BYTE TO SHIFT              08320000
         BCTR  R7,0                     REMAINING BYTES IN RECORD       08330000
         BCT   R3,RTN0C7EA              BRANCH IF MORE TO SHIFT         08340000
         B     RTN0C862                 ALL DONE                        08350000
         SPACE 2                                                        08360000
EXC0C820 EQU   *                                                        08370000
         MVC   DST0B203(1),0(R5)        SAVE TRAILING DATA              08380000
EXC0C826 EQU   *                                                        08390000
         MVC   1(1,R5),DST0B203         OVERLAY 1 BLANK                 08400000
         SPACE 2                                                        08410000
*                                                                       08420000
* S1 LONGER THAN S2--CAN DROP ANY TRAILING BLANKS ON 1-1 BASIS          08430000
*                                                                       08440000
RTN0C82C EQU   *                                                        08450000
         LA    R7,2(R6)                                                 08460000
         L     R0,DAT0CD68              R0=NNAAAAAA                     08470000
*                                        N = # HIT CHARS FOR TRT=2      08480000
*                                        A = A(STRING OF HIT CHARS=  )  08490000
         LA    R1,0(R5)                                                 08500000
         LR    R14,R7                                                   08510000
         SLL   R14,24                                                   08520000
         OR    R1,R14                   R1=LLAAAAAA                     08530000
*                                        L = LENGTH OF FIELD TO SCAN    08540000
*                                        A = A(START OF FIELD TO SCAN)  08550000
         BAL   R14,TRTSUBR              AT LEAST 2 TRAILING BLANKS?     08560000
         B     RTN0C920                 NO--CANNOT DO UPDATE!!          08570000
         SPACE 2                                                        08580000
RTN0C848 EQU   *                                                        08590000
         LR    R6,R5                    GET L'S2                        08600000
         SR    R5,R3                    # BYTES TO DROP                 08610000
         LTR   R2,R0                    REMAINING BYTES IN RECORD       08620000
         LA    R7,0(R5,R2)              POINT PAST WHERE S2 WILL GO     08630000
         BNP   RTN0C85C                                                 08640000
         BCTR  R2,0                                                     08650000
         EX    R2,EXC0C88E                                              08660000
RTN0C85C EQU   *                                                        08670000
         BCTR  R3,0                                                     08680000
         EX    R3,EXC0C89A                                              08690000
RTN0C862 EQU   *                                                        08700000
         CLC   DST0B1B0(2),DST0B1BB     TWO TRAILING BLANKS?            08710000
         MVC   DST0B1B0(2),DST0B1BB     ... INSERT 2 TRAILING BLANKS    08720000
         BNE   RTN0C920                 NO--CANNOT DO UPDATE            08730000
         SLR   R3,R3                    CLEAR FOR IC                    08740000
         IC    R3,4(,R9)                GET L'S1                        08750000
         LA    R4,8(R3,R9)              POINT PAST S1 TO S2             08760000
         ICM   R3,1,5(R9)               GET L'S2                        08770000
         BZ    RTN0C706                 NO S2--SKIP UPDATE              08780000
         BCTR  R3,0                     DECRMENT FOR EXECUTE            08790000
         EX    R3,EXC0C894              INSERT S2 OVER S1               08800000
         B     RTN0C706                 DO NEXT CNTL REC.               08810000
         SPACE 2                                                        08820000
RTN0C88E EQU   *                                                        08830000
EXC0C88E EQU   *                                                        08840000
         MVC   0(1,R5),0(R6)                                            08850000
EXC0C894 EQU   *                                                        08860000
         MVC   0(1,R8),0(R4)                                            08870000
EXC0C89A EQU   *                                                        08880000
         MVC   0(1,R7),DST0B1BB                                         08890000
RTN0C8A0 EQU   *                                                        08900000
         TM    DST0B15A,X'A0'                RECORD CHANGED/NEED WRITE? 08910000
         BZ    GETREC                        NO--SKIP PRINT--NEXT REC.  08920000
         MVC   DST0B2C6(8),MEMNAME           YES--GET MEMBER NAME       08930000
         L     R1,CURRECAD                   POINT TO THE BEFORE IMAGE  08940000
         MVC   DST0B2DE(80),0(R1)            BEFORE IMAGE TO PRINT      08950000
         TM    DST0B15A,X'40'                UPDATE ERROR?              08960000
         BO    RTN0C908                      YES--ISSUE ERROR MSG       08970000
         MVC   DST0B2CF(8),DAT0CDA7          NO--INIT. REPLACED MSG     08980000
         TM    DST0B15A,X'80'                RECORD CHANGED?            08990000
         BO    RTN0C8E0                      YES--PRINT BEFORE/AFTER    09000000
         MVC   DST0B2CF(12),DAT0CDBA         NO--INIT. LINE FOUND MSG   09010000
         BAL   R14,PRINTIT                   PRINT RECORD--NO CHANGE    09020000
         OI    FLAGS,X'40'                   SHOW RECORD PRINTED???     09030000
         B     GETREC                        GET NEXT RECORD            09040000
         SPACE 2                                                        09050000
RTN0C8E0 EQU   *                                                        09060000
         BAL   R14,PRINTIT                   PRINT BEFORE IMAGE         09070000
         MVC   DST0B2DE(71),DST0B169          AND                       09080000
         MVC   DST0B325(9),DST0B1B2            SET                      09090000
         MVC   0(80,R1),DST0B2DE                UP AFTER IMAGE          09100000
         MVC   DST0B2CF(11),DAT0CDAF         INIT. REPLACEMENT MSG      09110000
         BAL   R14,PRINTIT                   PRINT AFTER IMAGE          09120000
         OI    FLAGS,X'60'                   RECORD PRINTED/DO REWRITE  09130000
         B     GETREC                        GET NEXT RECORD            09140000
         SPACE 2                                                        09150000
RTN0C908 EQU   *                                                        09160000
         MVC   DST0B2CF(14),DAT0CDC6         UPDATE ERROR MESSAGE INIT  09170000
         MVC   DST0B332(4),DAT0A1E0           **** TO HIGHLIGHT         09180000
         BAL   R14,PRINTIT                   PRINT ERROR RECORD         09190000
         OI    DAT0CD82,X'02'                                           09200000
         B     GETREC                                                   09210000
         SPACE 2                                                        09220000
RTN0C920 EQU   *                                                        09230000
         OI    DST0B15A,X'40'                TURN ON UPDATE FAILED FLAG 09240000
         B     RTN0C8A0                      CHECK FOR "HIT" PRINTOUT   09250000
         SPACE 2                                                        09260000
RTN0C928 EQU   *                                                        09270000
MEMBEOF  NI    FLAGS,X'FE'                   TURN OFF S1=S2 FLAG        09280000
         L     R13,4(R13)                    RESTORE SAVE ADDR.         09290000
         LM    R14,R12,12(R13)               RESTORE REGS ...           09300000
         BR    R14                            AND EXIT                  09310000
         EJECT                                                          09320000
RTN0C936 EQU   *                                                        09330000
         TR    DAT0CD82(1),DAT0CD83          CONVERT FLAGS TO RC        09340000
         SLR   R8,R8                         ZERO FOR IC                09350000
         IC    R8,DAT0CD82                   GET RC IN HEX              09360000
         CVD   R8,DWORK                      CONVERT TO PACKED          09370000
         UNPK  DAT0A160(2),DST0B04E(2)       CONVERT TO EBCDIC          09380000
         OI    DAT0A161,X'F0'                 INCLUDING LAST DIGIT      09390000
         LA    R1,DAT0A140                   GET END OF JOB MESSAGE     09400000
         BAL   R14,SUB0CC66                  ISSUE END OF JOB MSG       09410000
         TM    SYSPRINT+48,X'10'             SYSPRINT OPEN?             09420000
         BZ    RTN0C96A                      NO--SKIP CLOSE             09430000
         CLOSE (SYSPRINT)                    YES--CLOSE IT              09440000
RTN0C96A EQU   *                                                        09450000
         LR    R1,R11                        GET WORK AREA ADDRESS      09460000
         L     R13,4(R13)                    RESTORE PREVIOUS SAVE ADDR 09470000
         L     R0,WORKSIZE                   GET L'WORK AREA OBTAINED   09480000
         LA    R1,0(,R1)                     GET A(WORK AREA)           09490000
*   SVC -- G/FMAINR                                                     09500000
         SVC   10                            DO FREEMAIN                09510000
         LR    R15,R8                        GET RC                     09520000
         RETURN (14,12),RC=(15)              RESTORE REGS, EXIT WITH RC 09530000
         EJECT                                                          09540000
SUB0C986 EQU   *                                                        09550000
READPARM EQU   *                                                        09560000
         STM   R14,R12,DST0B0F8                                         09570000
RTN0C98A EQU   *                                                        09580000
         MVI   CC,X'F0'                     CC SET FOR DOUBLE SPACING   09590000
         L     R2,BUFADMEM                  GET BUFFER ADDRESS FOR READ 09600000
         B     RTN0C99A                     START READ                  09610000
         SPACE 2                                                        09620000
SUB0C996 EQU   *                                                        09630000
READCONT EQU   *                                                        09640000
         STM   R14,R12,DST0B0F8                                         09650000
RTN0C99A EQU   *                                                        09660000
         TM    FLAGS2,X'10'                 MODE = INSTALL/CONSOLE?     09670000
         BO    RTN0C9C2                     YES--BRANCH                 09680000
         TM    FLAGS2,X'40'                 MODE = CONSOLE (SYSIN ERR)? 09690000
         BO    RTN0CAE8                     YES--DO WTOR                09700000
         GET   SYSIN,DST0B2DE               READ A RECORD FROM SYSIN    09710000
         MVC   DST0B2CF(5),DAT0CD94         SHOW SYSIN WAS USED         09720000
         B     RTN0CB1C                     DO READ FOLLOW-UP           09730000
         SPACE 2                                                        09740000
RTN0C9C2 EQU   *                                                        09750000
         MVC   DST0B2CF(7),DAT0CDA0          SHOW INSTALL               09760000
         CLI   DST0B38D,X'FF'                                           09770000
         BNE   RTN0C9DE                                                 09780000
         MVC   DST0B2DE(80),DST0B33D                                    09790000
         MVI   DST0B38D,X'00'                                           09800000
         B     RTN0CB1C                                                 09810000
         SPACE 2                                                        09820000
RTN0C9DE EQU   *                                                        09830000
         SLR   R15,R15                                                  09840000
         L     R4,DST0B0C4                                              09850000
RTN0C9E4 EQU   *                                                        09860000
         ICM   R15,1,0(R4)                                              09870000
         BZ    RTN0CABE                                                 09880000
         CLI   4(R4),X'E9'                                              09890000
         BE    RTN0C9FA                                                 09900000
         AR    R4,R15                                                   09910000
         B     RTN0C9E4                                                 09920000
         SPACE 2                                                        09930000
RTN0C9FA EQU   *                                                        09940000
         AR    R15,R4                                                   09950000
         ST    R15,DST0B0C4                                             09960000
         CLI   11(R4),X'40'                                             09970000
         BNE   RTN0CAC4                                                 09980000
         CLI   5(R4),X'40'                                              09990000
         BE    RTN0CAC4                                                 10000000
         LR    R3,R4                                                    10010000
         ICM   R9,7,17(R4)                                              10020000
         BZ    RTN0CAC4                                                 10030000
         CLI   18(R9),X'20'                  DASD DEVICE?               10040000
         BNE   RTN0CAC4                      NO--ERROR                  10050000
         LA    R6,DAT0CED1                   YES--GET A(DEV. TYPE TAB)  10060000
RTN0CA26 EQU   *                                                        10070000
         CLI   0(R6),X'00'                   NULL TABLE ENTRY?          10080000
         BE    RTN0CA40                      YES--SKIP IT               10090000
         CLC   0(1,R6),19(R9)                  SUPPORTED DISK TYPE?     10100000
         BE    RTN0CA48                      YES--OK TO PROCESS         10110000
RTN0CA40 EQU   *                                                        10120000
         LA    R6,9(,R6)                     NO--NEXT TABLE ENTRY       10130000
         CLI   0(R6),X'FF'                   END OF TABLE?              10140000
         BE    RTN0CAC4                      YES--DEVICE NOT FOUND      10150000
         B     RTN0CA26                      NO--CONTINUE SCAN          10160000
         SPACE 2                                                        10170000
RTN0CA48 EQU   *                                                        10180000
         MVI   DST0B2DE,X'40'                                           10190000
         MVC   DST0B2DF(79),DST0B2DE                                    10200000
         MVC   DST0B2DE(6),5(R4)                                        10210000
         LA    R7,DST0B2DF                                              10220000
         BAL   R14,SUB0CADA                                             10230000
         MVI   0(R7),X'4C'                                              10240000
         MVC   1(6,R7),28(R9)                                           10250000
         LA    R7,2(R7)                                                 10260000
         BAL   R14,SUB0CADA                                             10270000
         MVI   0(R7),X'4C'                                              10280000
         MVI   DST0B33D,X'40'                                           10290000
         MVC   DST0B33E(79),DST0B33D                                    10300000
         MVC   DST0B33D(8),DAT0A1D0                                     10310000
         LA    R7,DST0B33E                                              10320000
         BAL   R14,SUB0CADA                                             10330000
         MVI   0(R7),X'4C'                                              10340000
         MVC   1(8,R7),1(R6)                                            10350000
         LA    R7,2(R7)                                                 10360000
         BAL   R14,SUB0CADA                                             10370000
         MVI   0(R7),X'4C'                                              10380000
         MVC   1(6,R7),5(R4)                                            10390000
         LA    R7,2(R7)                                                 10400000
         BAL   R14,SUB0CADA                                             10410000
         MVI   0(R7),X'4C'                                              10420000
         MVI   DST0B38D,X'FF'                                           10430000
         B     RTN0CB1C                                                 10440000
         SPACE 2                                                        10450000
RTN0CABE EQU   *                                                        10460000
         SLR   R15,R15                                                  10470000
         B     EOFSYSIN                                                 10480000
         SPACE 2                                                        10490000
RTN0CAC4 EQU   *                                                        10500000
         MVC   DAT0A1A2(8),4(R4)                                        10510000
         LA    R1,DAT0A194                                              10520000
         BAL   R14,SUB0CC66                                             10530000
         OI    DAT0CD82,X'08'                                           10540000
         B     RTN0C936                                                 10550000
         SPACE 2                                                        10560000
RTN0CADA EQU   *                                                        10570000
SUB0CADA EQU   *                                                        10580000
         CLI   0(R7),X'40'                                              10590000
         BER   R14                                                      10600000
         LA    R7,1(R7)                                                 10610000
         B     RTN0CADA                                                 10620000
         SPACE 2                                                        10630000
RTN0CAE8 EQU   *                                                        10640000
         XC    DST0B0F4(4),DST0B0F4                                     10650000
         LA    R1,DAT0CF9C                                              10660000
         IC    R14,0(R1)                                                10670000
         LA    R15,DST0B2DE                                             10680000
         ST    R15,0(R1)                                                10690000
         STC   R14,0(R1)                                                10700000
         LA    R14,DST0B0F4                                             10710000
         ST    R14,4(R1)                                                10720000
*   SVC -- WTO/WTOR                                                     10730000
         SVC   35                                                       10740000
         LA    R1,DST0B0F4                                              10750000
         LA    R0,1                                                     10760000
*   SVC -- WAIT                                                         10770000
         SVC   1                                                        10780000
         MVC   DST0B2CF(7),DAT0CD99                                     10790000
RTN0CB1C EQU   *                                                        10800000
         OC    DST0B2DE(72),DST0B1BB       LOWER CASE TO UPPER CASE     10810000
         MVC   0(72,R2),DST0B2DE          MOVE RECORD TO WORK BUFFER    10820000
         MVC   72(8,R2),DST0B1BB          BLANK LAST 8 CHARACTERS       10830000
         ST    R2,DST0B108                 SAVE BUFFER ADDRESS          10840000
         BAL   R14,PRINTIT                 PRINT RECORD JUST READ       10850000
         CLC   EOF1,0(R2)                  END OF PARM (END )?          10860000
         BE    EOFSYSIN                    YES--FORCED EOF ON SYSIN     10870000
         CLC   EOF2,0(R2)                  END OF PARM (</*)?           10880000
         BE    EOFSYSIN                    YES--FORCE EOF ON SYSIN      10890000
         LM    R14,R12,DST0B0F8                                         10900000
         LA    R3,72                                                    10910000
         BR    R14                                                      10920000
         EJECT                                                          10930000
*                                                                       10940000
* SUBROUTINE TO SCAN FIELDS/RECORDS VIA TRT                             10950000
*                                                                       10960000
*   REGISTER CONTENTS ON ENTRY:                                         10970000
*     R0  = NNAAAAAA WHERE N = # "HIT" CHARS AND A = A(STRING OF CHARS) 10980000
*     R1  = LLAAAAAA WHERE L = FIELD/REC LENGTH AND A = A(SCAN START)   10990000
*   REGISTER CONTENTS ON EXIT:                                          11000000
*     R0  =                                                             11010000
*                                                                       11020000
*                                                                       11030000
*                                                                       11040000
*                                                                       11050000
*                                                                       11060000
TRTSUBR  STM   R14,R12,12(R13)                                          11070000
         LR    R3,R1                       GET L'SOURCE IN HIGH ORDER   11080000
         LA    R4,0(R1)                    GET A(SOURCE START)          11090000
         SRL   R3,24                       SHIFT LENGTH TO LOW ORDER    11100000
         LR    R15,R3                      COPY LENGTH                  11110000
         LR    R5,R0                       GET # ARGUMENTS/ADDRESS      11120000
         LA    R6,0(R5)                    GET A(HIT CHARS)             11130000
         SRL   R5,24                       SHIFT COUNT TO LOW ORDER     11140000
         LTR   R3,R3                       SOURCE FIELD EXHAUSTED?      11150000
         BNP   RTN0CBBC                    YES--EXIT                    11160000
         LTR   R5,R5                       END OF TRT TABLE?            11170000
         BNP   RTN0CBBC                    YES--EXIT                    11180000
         CR    R3,R5                       END OF SOURCE FIELD?         11190000
         BL    RTN0CBBC                    YES--EXIT                    11200000
         SLR   R9,R9                       CLEAR FOR IC                 11210000
         IC    R9,0(R6)                    GET TABLE ARGUMENT (STOP CH) 11220000
         LA    R9,DST9B590(R9)             POINT TO POSITION IN TABLE   11230000
         MVI   0(R9),X'01'                 FORCE HIT ON ARGUMENT        11240000
         LA    R8,0(R3,R4)                 POINT TO END OF FIELD/REC    11250000
         SR    R8,R5                       CALC. L'SCAN                 11260000
         BCTR  R5,0                        DECREMENT                    11270000
         LR    R1,R4                       GET START OF SOURCE ...      11280000
         BCTR  R1,0                         - 1 IN CASE NO HIT          11290000
RTN0CB90 EQU   *                                                        11300000
         BCTR  R3,0                                                     11310000
         EX    R3,EXC0CBE0                 DO TRT                       11320000
         BZ    RTN0CBB8                    BRANCH IF NO HIT             11330000
         LTR   R5,R5                       TRT TABLE EXHAUSTED?         11340000
         BZ    RTN0CBC2                    YES--SCAN DONE               11350000
         CLR   R1,R8                       ????????                     11360000
         BH    RTN0CBB8                    ????????                     11370000
         EX    R5,EXC0CBDA                                              11380000
         BE    RTN0CBC2                                                 11390000
         LA    R3,0(R8,R5)                                              11400000
         SR    R3,R1                                                    11410000
         BP    RTN0CB90                                                 11420000
RTN0CBB8 EQU   *                                                        11430000
         MVI   0(R9),X'00'                 RESET TRT TABLE POSITION     11440000
RTN0CBBC EQU   *                                                        11450000
         LM    R14,R12,12(R13)                                          11460000
         BR    R14                                                      11470000
         EJECT                                                          11480000
RTN0CBC2 EQU   *                                                        11490000
         MVI   0(R9),X'00'                 RESET TRT TABLE POSITION     11500000
         LR    R0,R1                       GET A(HIT)                   11510000
         SR    R0,R4                       CALC. OFFSET TO HIT          11520000
         SR    R15,R0                      CALC. REMAINING              11530000
         BCTR  R15,0                        SOURCE BYTES                11540000
         L     R14,12(R13)                 GET RETURN ADDRESS           11550000
         LM    R2,R12,28(R13)              RESTORE OTHER REGISTERS      11560000
         B     4(R14)                      RETURN + 4 IS EXIT           11570000
         SPACE 2                                                        11580000
EXC0CBDA CLC   0(1,R6),0(R1)                                            11590000
         SPACE                                                          11600000
EXC0CBE0 TRT   1(1,R1),DST0B590                                         11610000
         SPACE 2                                                        11620000
PRINTIT  STM   R14,R1,PARMSAVE                                          11630000
         TM    SYSPRINT+48,X'10'            SYSPRINT OPEN?              11640000
         BZ    RTN0CC60                     NO                          11650000
         AP    PACKED60(2),DAT0CD7C(1)      YES--COUNT THIS LINE        11660000
         CLI   CC,X'F0'                     DOUBLE SPACED?              11670000
         BNE   RTN0CC06                     NO                          11680000
         AP    PACKED60(2),DAT0CD7C(1)      YES--ACCOUNT FOR EXTRA LINE 11690000
RTN0CC06 EQU   *                                                        11700000
         CP    PACKED60(2),DAT0CD78(2)      PAGE FULL?                  11710000
         BNH   RTN0CC48                     NO--CONTINUE                11720000
         AP    DAT0CD7A(2),DAT0CD7C(1)      YES--UPDATE PAGE COUNT      11730000
         MVC   DST0B2BF(4),DAT0CD7E                                     11740000
         ED    DST0B2BF(4),DAT0CD7A                                     11750000
         PUT   SYSPRINT,DST0B24B            PRINT HEADING               11760000
         PUT   SYSPRINT,DAT0CDDB            PRINT HEADING               11770000
         ZAP   PACKED60(2),DAT0CD7D(1)      RESET LINE COUNT            11780000
         MVI   CC,X'F0'                                                 11790000
RTN0CC48 PUT   SYSPRINT,CC                  PRINT THE PARM RECORD       11800000
         MVI   CC,X'40'                     RESET SINGLE SPACING        11810000
         MVC   PRINTLIN,CC                   AND BLANK PRINT AREA       11820000
RTN0CC60 EQU   *                                                        11830000
         LM    R14,R1,PARMSAVE                                          11840000
         BR    R14                                                      11850000
         EJECT                                                          11860000
SUB0CC66 EQU   *                                                        11870000
         STM   R14,R3,DST0B140                                          11880000
         LH    R2,0(R1)                                                 11890000
         LA    R0,5                                                     11900000
         SR    R2,R0                                                    11910000
         BM    RTN0CCA8                                                 11920000
         LR    R3,R1                                                    11930000
         TM    FLAGS2,X'40'                 CONSOLE USED/SYSIN ERROR?   11940000
         BO    RTN0CC8A                     YES--DO WTOR                11950000
         TM    FLAGS2,X'20'                 SYSIN OPEN?                 11960000
         BO    RTN0CC8C                     YES--DO GET FROM SYSIN      11970000
RTN0CC8A EQU   *                                                        11980000
*   SVC -- WTO/WTOR                                                     11990000
         SVC   35                                                       12000000
RTN0CC8C EQU   *                                                        12010000
         EX    R2,EXC0CCAE                                              12020000
         MVI   CC,X'F0'                     CC SET FOR DOUBLE SPACING   12030000
         MVC   DST0B2CF(7),DAT0CDD4                                     12040000
         MVC   DST0B332(4),DAT0A1E0                                     12050000
         BAL   R14,PRINTIT                                              12060000
         MVI   CC,X'F0'                     CC SET FOR DOUBLE SPACING   12070000
RTN0CCA8 EQU   *                                                        12080000
         LM    R14,R3,DST0B140                                          12090000
         BR    R14                                                      12100000
         SPACE 2                                                        12110000
EXC0CCAE EQU   *                                                        12120000
         MVC   DST0B2DE(1),4(R3)                                        12130000
         EJECT                                                          12140000
RTN0CCB4 EQU   *                                                        12150000
         LA    R2,121                                                   12160000
         CH    R2,62(,R1)                                               12170000
         BNHR  R14                                                      12180000
         STH   R2,62(,R1)                                               12190000
         BR    R14                                                      12200000
         EJECT                                                          12210000
RTN0CCC4 EQU   *                                                        12220000
         LR    R7,R1                                                    12230000
         LA    R15,0(,R15)                                              12240000
         CNOP  0,4                                                      12250000
         O     R15,DAT0CCD4                                             12260000
         B     RTN0CCD8                                                 12270000
         SPACE 2                                                        12280000
DAT0CCD4 EQU   *                                                        12290000
         DC    X'01000000'                         C'....'              12300000
RTN0CCD8 EQU   *                                                        12310000
*   SVC -- SYNADAF                                                      12320000
         SVC   68                                                       12330000
         MVC   DAT0A075(60),68(R1)                                      12340000
         MVC   DAT0A0DE(8),75(R1)                                       12350000
         LA    R1,DAT0A060                                              12360000
         BAL   R14,SUB0CC66                                             12370000
         LA    R15,0(,R15)                                              12380000
         CNOP  0,4                                                      12390000
         O     R15,DAT0CCFC                                             12400000
         B     RTN0CD00                                                 12410000
         SPACE 2                                                        12420000
DAT0CCFC EQU   *                                                        12430000
         DC    X'FF000000'                         C'....'              12440000
RTN0CD00 EQU   *                                                        12450000
*   SVC -- SYNADAF                                                      12460000
         SVC   68                                                       12470000
         OI    DAT0CD82,X'04'                                           12480000
         OI    FLAGS,X'10'                                              12490000
         LA    R1,DAT0A0B4                                              12500000
         BAL   R14,SUB0CC66                                             12510000
         L     R7,32(,R7)                                               12520000
         BR    R7                                                       12530000
         EJECT                                                          12540000
         DC    X'00000000000000000000000000000000' C'................'  12550000
         DC    X'0000000000000000'                 C'........'          12560000
EXC0CD30 EQU   *                                                        12570000
         CLC   0(1,R14),0(R15)                                          12580000
         DC    X'0000'                             C'..'                12590000
WORKSIZE EQU   *                                                        12600000
*        DC    X'00006380'                         C'....'              12610000
         DC    X'000098F8'                         C'....'              12620000
BUFADMEM EQU   *                                                        12630000
         DC    X'00001900'                         C'....'              12640000
DIRDECB EQU    *                                                        12650000
         DC    X'0000000000800100'                 C'........'          12660000
         DC    A(PDSDIR)                                                12670000
         DC    X'0000000000000000'                 C'........'          12680000
DCBXLIST DC    F'0'                                                     12690000
DAT0CD58 EQU   *                                                        12700000
         DC    X'84'                                                    12710000
         DC    AL3(PDSDATA)                                             12720000
DAT0CD5C EQU   *                                                        12730000
         DC    X'80'                                                    12740000
         DC    AL3(PDSDATA)                                             12750000
DAT0CD60 EQU   *                                                        12760000
         DC    X'85'                                                    12770000
         DC    AL3(RTN0CCB4)                                            12780000
         DC    X'01'                                                    12790000
         DC    AL3(DAT0CEAD)                                            12800000
DAT0CD68 EQU   *                                                        12810000
         DC    X'02'                                                    12820000
         DC    AL3(DAT0CEAE)                                            12830000
DAT0CD6C EQU   *                                                        12840000
         DC    X'01'                               C'.'                 12850000
         DC    AL3(DAT0CEB8)                                            12860000
DAT0CD70 EQU   *                                                        12870000
         DC    X'04'                               C'.'                 12880000
         DC    AL3(DAT0CEB0)                                            12890000
DAT0CD74 EQU   *                                                        12900000
         DC    X'04'                               C'.'                 12910000
         DC    AL3(DAT0CEB4)                                            12920000
DAT0CD78 EQU   *                                                        12930000
         DC    X'060C'                             C'..'                12940000
DAT0CD7A EQU   *                                                        12950000
         DC    X'000C'                             C'..'                12960000
DAT0CD7C EQU   *                                                        12970000
         DC    X'1C'                               C'.'                 12980000
DAT0CD7D EQU   *                                                        12990000
         DC    X'5C'                               C'.'                 13000000
DAT0CD7E EQU   *                                                        13010000
         DC    X'40202021'                         C' ...'              13020000
DAT0CD82 EQU   *                                                        13030000
         DC    X'00'                               C'.'                 13040000
DAT0CD83 EQU   *                                                        13050000
         DC    X'000408080C0C0C0C1010101010101010' C'................'  13060000
         DC    X'FF'                                                    13070000
DAT0CD94 EQU   *                                                        13080000
         DC    C'SYSIN'                                                 13090000
DAT0CD99 EQU   *                                                        13100000
         DC    C'CONSOLE'                                               13110000
DAT0CDA0 EQU   *                                                        13120000
         DC    C'INSTALL'                                               13130000
DAT0CDA7 EQU   *                                                        13140000
         DC    C'REPLACED'                                              13150000
DAT0CDAF EQU   *                                                        13160000
         DC    C'REPLACEMENT'                                           13170000
DAT0CDBA EQU   *                                                        13180000
         DC    C'-LINE FOUND-'                                          13190000
DAT0CDC6 EQU   *                                                        13200000
         DC    C'*UPDATE ERROR*'                                        13210000
DAT0CDD4 EQU   *                                                        13220000
         DC    C'MESSAGE'                                               13230000
DAT0CDDB EQU   *                                                        13240000
         DC    C'0'                                                     13250000
         DC    CL120' MEMBER   COMMENTS       .... ....1.... ....2.... +13260000
               ....3.... ....4.... ....5.... ....6.... ....7.C.. ....8' 13270000
DAT0CE54 EQU   *                                                        13280000
         DC    C'PDS STRING SCANNING AID '                              13290000
DAT0CE6C EQU   *                                                        13300000
         DC    C'UPDATE CONTROL STATEMENTS'                             13310000
DAT0CE85 EQU   *                                                        13320000
         DC    C'MODE='                                                 13330000
DAT0CE8A EQU   *                                                        13340000
         DC    C'PAGE:'                                                 13350000
HALFW7   DC    X'0007'                                                  13360000
DAT0CE91 EQU   *                                                        13370000
         DC    CL8'INSTALL'                                             13380000
DAT0CE99 EQU   *                                                        13390000
         DC    X'0006'                             C'..'                13400000
DAT0CE9B EQU   *                                                        13410000
         DC    CL8'UPDATE'                                              13420000
DAT0CEA3 EQU   *                                                        13430000
         DC    X'0005'                             C'..'                13440000
DAT0CEA5 EQU   *                                                        13450000
         DC    CL8'CHECK'                                               13460000
DAT0CEAD EQU   *                                                        13470000
         DC    X'00'                               C'.'                 13480000
DAT0CEAE EQU   *                                                        13490000
         DC    X'4040'                             C'  '                13500000
DAT0CEB0 EQU   *                                                        13510000
         DC    C'.NU.'                                                  13520000
DAT0CEB4 EQU   *                                                        13530000
         DC    C'.RU.'                                                  13540000
DAT0CEB8 EQU   *                                                        13550000
         DC    C'<END'                                                  13560000
SPECHARS EQU   *                                                        13570000
         DC    X'616B4B4D5D404C6E5E7A6F7F5F5A4E7E' C'...... .........'  13580000
         DC    X'6D5C4F6C7D'                       C'.....'             13590000
DAT0CED1 EQU   *                                                        13600000
         DC    X'01',C'2311    ' REMOVABLE DISK STORAGE      JLM2006325 13610000
         DC    X'02',C'2301    ' PARALLEL DRUM STORAGE       JLM2006325 13620000
         DC    X'03',C'2303    ' SERIAL DRUM STORAGE         JLM2006325 13630000
         DC    X'04',C'2302    ' DISK STORAGE                JLM2006325 13640000
         DC    X'05',C'2321    ' DATA CELL STORAGE           JLM2006325 13650000
         DC    X'06',C'2305-1  ' FIXED HEAD STORAGE          JLM2006325 13660000
         DC    X'07',C'2305-2  ' FIXED HEAD STORAGE          JLM2006325 13670000
         DC    X'08',C'2314    ' REMOVABLE DISK STORAGE      JLM2006325 13680000
         DC    X'09',C'3330    ' REMOVABLE DISK STORAGE      JLM2006325 13690000
         DC    X'0A',C'3340    ' REMOVABLE DISK STORAGE      JLM2006325 13700000
         DC    X'0B',C'3350    ' NON-REMOVABLE DISK STORAGE  JLM2006325 13710000
         DC    X'0C',C'3375    ' NON-REMOVABLE DISK STORAGE  JLM2006325 13720000
         DC    X'0D',C'3330-2  ' REMOVABLE DISK STORAGE      JLM2006325 13730000
         DC    X'0E',C'3380    ' NON-REMOVABLE DISK STORAGE  JLM2006325 13740000
         DC    X'0F',C'3390    ' NON-REMOVABLE DISK STORAGE  JLM2006325 13750000
         DC    X'FF',C'LIST END'                             JLM2006325 13760000
DAT0CF48 EQU   *                                                        13770000
         DC    X'00280000D7C4E2F0F0F1405C40C5D9D9' C'....PDS001 . ERR'  13780000
         DC    X'D6D9405C40E2E8E2D7D9C9D5E340C4C4' C'OR . SYSPRINT DD'  13790000
         DC    X'40D4C9E2E2C9D5C7'                 C' MISSING'          13800000
DAT0CF70 EQU   *                                                        13810000
         DC    X'002C0000D7C4E2F0F0F2405C40C5D9D9' C'....PDS002 . ERR'  13820000
         DC    X'D6D9405C40C9D5E5C1D3C9C440D7C1D9' C'OR . INVALID PAR'  13830000
         DC    X'C1D4C5E3C5D940C6C9C5D3C4'         C'AMETER FIELD'      13840000
DAT0CF9C EQU   *                                                        13850000
         DC    X'480000000000000000340000D7C4E2F0' C'............PDS0'  13860000
         DC    X'F0F340C5D5E3C5D940D7C4E2E4D7C4E3' C'03 ENTER PDSUPDT'  13870000
         DC    X'C540C3D6D5E3D9D6D340E2E3C1E3C5D4' C'E CONTROL STATEM'  13880000
         DC    X'C5D5E340D6D9407DC5D5C47D'         C'ENT OR .END.'      13890000
DAT0CFD8 EQU   *                                                        13900000
         DC    X'002B0000D7C4E2F0F0F440C9D5E5C1D3' C'....PDS004 INVAL'  13910000
         DC    X'C9C440C3D6D5E3D9D6D340E2E3C1E3C5' C'ID CONTROL STATE'  13920000
         DC    X'D4C5D5E340E2E8D5E3C1E700'         C'MENT SYNTAX.'      13930000
DAT0A004 EQU   *                                                        13940000
         DC    X'002A0000D7C4E2F0F0F540C5D5C440D6' C'....PDS005 END O'  13950000
         DC    X'C640C9D5D7E4E340C3D6D5E3D9D6D340' C'F INPUT CONTROL '  13960000
         DC    X'E2E3C1E3C5D4C5D5E3E20000'         C'STATEMENTS..'      13970000
DAT0A030 EQU   *                                                        13980000
         DC    X'002D0000D7C4E2F0F0F640C6C9D3C540' C'....PDS006 FILE '  13990000
DAT0A040 EQU   *                                                        14000000
         DC    X'E7E7E7E7E7E7E7E740C9D5E5C1D3C9C4' C'XXXXXXXX INVALID'  14010000
         DC    X'40C6D6D940D7C4E2E4D7C4E3C5000000' C' FOR PDSUPDTE...'  14020000
DAT0A060 EQU   *                                                        14030000
         DC    X'00510000D7C4E2F0F0F740C961D640C5' C'....PDS007 I.O E'  14040000
         DC    X'D9D9D6D940'                       C'RROR '             14050000
DAT0A075 EQU   *                                                        14060000
         DC    X'F1F2F3F4F5F6F7F8F9F0F1F2F3F4F5F6' C'1234567890123456'  14070000
         DC    X'F7F8F9F0F1F2F3F4F5F6F7F8F9F0F1F2' C'7890123456789012'  14080000
         DC    X'F3F4F5F6F7F8F9F0F1F2F3F4F5F6F7F8' C'3456789012345678'  14090000
         DC    X'F9F0F1F2F3F4F5F6F7F8F9F0000000'   C'901234567890...'   14100000
DAT0A0B4 EQU   *                                                        14110000
         DC    X'00320000D7C4E2F0F0F840D7D9D6C3C5' C'....PDS008 PROCE'  14120000
         DC    X'E2E2C9D5C740E3C5D9D4C9D5C1E3C5C4' C'SSING TERMINATED'  14130000
         DC    X'40C6D6D940C6C9D3C540'             C' FOR FILE '        14140000
DAT0A0DE EQU   *                                                        14150000
         DC    X'E7E7E7E7E7E7E7E70000'             C'XXXXXXXX..'        14160000
DAT0A0E8 EQU   *                                                        14170000
         DC    X'00300000D7C4E2F0F0F9405C40C5D9D9' C'....PDS009 . ERR'  14180000
         DC    X'D6D9405C40E3D6D640D4C1D5E840C3D6' C'OR . TOO MANY CO'  14190000
         DC    X'D5E3D9D6D340E2E3C1E3C5D4C5D5E3E2' C'NTROL STATEMENTS'  14200000
DAT0A118 EQU   *                                                        14210000
         DC    X'00270000D7C4E2F0F1F040C6C9D3C540' C'....PDS010 FILE '  14220000
DAT0A128 EQU   *                                                        14230000
         DC    X'E7E7E7E7E7E7E7E740C8C1C440D5D640' C'XXXXXXXX HAD NO '  14240000
         DC    X'E4D7C4C1E3C5E200'                 C'UPDATES.'          14250000
DAT0A140 EQU   *                                                        14260000
         DC    X'00220000D7C4E2F0F1F140D7C4E2E4D7' C'....PDS011 PDSUP'  14270000
         DC    X'C4E3C540C5D5C4C5C44B40C3D6C4C57E' C'DTE ENDED. CODE.'  14280000
DAT0A160 EQU   *                                                        14290000
         DC    X'E7'                               C'X'                 14300000
DAT0A161 EQU   *                                                        14310000
         DC    X'E70000'                           C'X..'               14320000
DAT0A164 EQU   *                                                        14330000
         DC    X'002D0000D7C4E2F0F1F240C4C1E3C1E2' C'....PDS012 DATAS'  14340000
         DC    X'C5E340D5D6E340C1D7D7D3C9C3C1C2D3' C'ET NOT APPLICABL'  14350000
         DC    X'C540E3D640D7C4E2E4D7C4E3C5000000' C'E TO PDSUPDTE...'  14360000
DAT0A194 EQU   *                                                        14370000
         DC    X'00350000D7C4E2F0F1F340C4C440'     C'....PDS013 DD '    14380000
DAT0A1A2 EQU   *                                                        14390000
         DC    X'E7E7E7E7E7E7E7E740C9D5E5C1D3C9C4' C'XXXXXXXX INVALID'  14400000
         DC    X'40D6D940E4D5D2D5D6E6D540C4C5E5C9' C' OR UNKNOWN DEVI'  14410000
         DC    X'C3C540E3E8D7C500000000000000'     C'CE TYPE.......'    14420000
DAT0A1D0 EQU   *                                                        14430000
         DC    CL8'3330.1  '                                            14440000
DAT0A1D8 EQU   *                                                        14450000
EOF1     DC    CL4'END'                                                 14460000
DAT0A1DC EQU   *                                                        14470000
         DC    CL4'VOL='                                                14480000
DAT0A1E0 EQU   *                                                        14490000
         DC    CL4'****'                                                14500000
DAT0A1E4 EQU   *                                                        14510000
         DC    CL7'DSNAME='                                             14520000
EOF2     DC    CL3'</*'                                                 14530000
         DC    X'0000'                             C'..'                14540000
* SYSPRINT DCB   DSORG=PS,DDNAME=SYSPRINT,LRECL=121,RECFM=FBA           14550000
SYSPRINT DS    0D                                                       14560000
DAT0A1F0 EQU   *                                                        14570000
         DC    X'00000000000000000000000000000000' C'................'  14580000
         DC    X'00000000000000010000400000000001' C'.......... .....'  14590000
         DC    X'0000000194'                       C'.....'             14600000
         DC    AL3(DAT0CD60)                                            14610000
         DC    X'E2E8E2D7D9C9D5E3'                 C'SYSPRINT'          14620000
DAT0A220 EQU   *                                                        14630000
         DC    X'02000050000000010000000100000000' C'................'  14640000
         DC    X'00000000000000010000000100000001' C'................'  14650000
         DC    X'00000079000000010000000000000001' C'................'  14660000
* SYSIN    DCB   DSORG=PS,DDNAME=SYSIN                                  14670000
SYSIN    DS    0D                                                       14680000
DAT0A250 EQU   *                                                        14690000
         DC    X'00000000000000000000000000000000' C'................'  14700000
         DC    X'00000000000000010000400000000001' C'.......... .....'  14710000
         DC    X'00'                               C'.'                 14720000
         DC    AL3(EOFSYSIN)                                            14730000
         DC    X'90000000E2E8E2C9D5404040'         C'....SYSIN   '      14740000
DAT0A280 EQU   *                                                        14750000
         DC    X'02005000000000010000000100000000' C'................'  14760000
         DC    X'00000000000000010000000100000001' C'................'  14770000
         DC    X'00000050000000010000000000000001' C'................'  14780000
* PDSDATA  DCB   DSORG=PS,DDNAME=@???????,EODAD=RTN0C928,               14790000
*                EXLST=DCBXLIST,                                        14800000
PDSDATA  DS    0D                                                       14810000
TEMPDATA EQU   *                                                        14820000
         DC    X'00000000000000000000000000000000' C'................'  14830000
         DC    X'00000000000000010000020000000001' C'................'  14840000
         DC    X'00'                               C'.'                 14850000
         DC    AL3(RTN0C928)                                            14860000
         DC    X'00'                               C'.'                 14870000
         DC    AL3(DCBXLIST)                                            14880000
DAT0A2D8 EQU   *                                                        14890000
         DC    X'E3C5D4D7C4C1E3C10200242400000001' C'TEMPDATA........'  14900000
         DC    A(RTN0CCC4)                                              14910000
         DC    X'0000'                             C'..'                14920000
DAT0A2EE EQU   *                                                        14930000
         DC    X'00000000000000000001000000010000' C'................'  14940000
         DC    X'00010000000000000001'             C'..........'        14950000
* PDSDIR   DCB   DSORG=PS,DDNAME=@???????,EODAD=RTN0C5B6,               14960000
*                LRECL=256,BLKSIZE=256,RECFM=F,EXLST=DCBXLIST           14970000
PDSDIR   DS    0D                                                       14980000
TEMPDIR  EQU   *                                                        14990000
         DC    X'00000000000000000000000000000000' C'................'  15000000
         DC    X'00000000000000010000400000000001' C'.......... .....'  15010000
         DC    X'00'                               C'.'                 15020000
         DC    AL3(RTN0C5B6)                                            15030000
         DC    X'C0'                               C'.'                 15040000
         DC    AL3(DCBXLIST)                                            15050000
DAT0A330 EQU   *                                                        15060000
         DC    X'E3C5D4D7C4C9D9400200200000000001' C'TEMPDIR ........'  15070000
         DC    A(RTN0CCC4)                                              15080000
         DC    X'00000100000000000000000100000001' C'................'  15090000
         DC    X'000000010000000000000001'         C'............'      15100000
         EJECT                                                          15110000
WORKAREA DSECT                                                          15120000
SAVEAREA DS    18F                                                      15130000
DWORK    DS    D         +048                                           15140000
PARMSAVE DS    4F        +050                                           15150000
DST0B060 DS    F                                                        15160000
DST0B064 DS    F                                                        15170000
TTR      EQU   WORKAREA+X'068'                                          15180000
DST0B06C EQU   WORKAREA+X'06C'                                          15190000
NEWSAVE  EQU   WORKAREA+X'070'                                          15200000
CURRECAD EQU   WORKAREA+X'0B8'                                          15210000
BUFADTAB EQU   WORKAREA+X'0BC'                                          15220000
ACTBLKSZ EQU   WORKAREA+X'0C8'                                          15230000
RECOFSET EQU   WORKAREA+X'0CC'                                          15240000
DST0B0C0 EQU   WORKAREA+X'0C0'                                          15250000
DST0B0C4 EQU   WORKAREA+X'0C4'                                          15260000
DST0B0D0 EQU   WORKAREA+X'0D0'                                          15270000
DST0B0D4 EQU   WORKAREA+X'0D4'                                          15280000
DST0B0D8 EQU   WORKAREA+X'0D8'                                          15290000
DST0B0DC EQU   WORKAREA+X'0DC'                                          15300000
MEMBDECB EQU   WORKAREA+X'0E0'                                          15310000
DST0B04E EQU   WORKAREA+X'04E'                                          15320000
DST0B0F4 EQU   WORKAREA+X'0F4'                                          15330000
DST0B0F8 EQU   WORKAREA+X'0F8'                                          15340000
DST0B108 EQU   WORKAREA+X'108'                                          15350000
DST0B140 EQU   WORKAREA+X'140'                                          15360000
DST0B158 EQU   WORKAREA+X'158'                                          15370000
FLAGS2   EQU   WORKAREA+X'159'                                          15380000
INSTALL  EQU   X'10'                    INSTALL MODE/CONSOLE INPUT      15390000
SYSPROPN EQU   X'20'                    SYSPRINT OPEN                   15400000
SYSINERR EQU   X'40'                    SYSIN OPEN ERROR--CONSOLE INPUT 15410000
SYSINOPN EQU   X'80'                    SYSIN OPEN                      15420000
DST0B15A EQU   WORKAREA+X'15A'                                          15430000
FLAGS    EQU   WORKAREA+X'15B'                                          15440000
MEMBONLY EQU   X'02'                    SINGLE MEMBER SCAN              15450000
MEMNAME  EQU   WORKAREA+X'15C',8                                        15460000
PACKED60 EQU   WORKAREA+X'164'                                          15470000
DEOFFSET EQU   WORKAREA+X'166'                                          15480000
DST0B168 EQU   WORKAREA+X'168'                                          15490000
DST0B169 EQU   WORKAREA+X'169'                                          15500000
DST0B1B2 EQU   WORKAREA+X'1B2'                                          15510000
DST0B1B0 EQU   WORKAREA+X'1B0'                                          15520000
DST0B1BB EQU   WORKAREA+X'1BB'                                          15530000
DST0B203 EQU   WORKAREA+X'203'                                          15540000
DST0B24B EQU   WORKAREA+X'24B'                                          15550000
DST0B24C EQU   WORKAREA+X'24C'                                          15560000
DST0B26F EQU   WORKAREA+X'26F'                                          15570000
DST0B273 EQU   WORKAREA+X'273'                                          15580000
DST0B27A EQU   WORKAREA+X'27A'                                          15590000
DST0B27B EQU   WORKAREA+X'27B'                                          15600000
DST0B281 EQU   WORKAREA+X'281'                                          15610000
DST0B2AB EQU   WORKAREA+X'2AB'                                          15620000
DST0B2BA EQU   WORKAREA+X'2BA'                                          15630000
DST0B2B0 EQU   WORKAREA+X'2B0'                                          15640000
DST0B2BF EQU   WORKAREA+X'2BF'                                          15650000
DST0B2DF EQU   WORKAREA+X'2DF'                                          15660000
CC       EQU   WORKAREA+X'2C4',1                                        15670000
PRINTLIN EQU   WORKAREA+X'2C5',120                                      15680000
DST0B2C6 EQU   WORKAREA+X'2C6'                                          15690000
DST0B2CF EQU   WORKAREA+X'2CF'                                          15700000
DST0B2DE EQU   WORKAREA+X'2DE'                                          15710000
DST0B325 EQU   WORKAREA+X'325'                                          15720000
DST0B332 EQU   WORKAREA+X'332'                                          15730000
DST0B33D EQU   WORKAREA+X'33D'                                          15740000
DST0B33E EQU   WORKAREA+X'33E'                                          15750000
DST0B38D EQU   WORKAREA+X'38D'                                          15760000
TRTAB    EQU   WORKAREA+X'390'                                          15770000
DST0B391 EQU   WORKAREA+X'391'                                          15780000
TRTAB2   EQU   WORKAREA+X'490'                                          15790000
DST0B4DB EQU   WORKAREA+X'4DB'                                          15800000
DST9B590 EQU   WORKAREA+X'590'                                          15810000
DST0B590 EQU   WORKAREA+X'590'                                          15820000
DST4B690 EQU   WORKAREA+X'690'                                          15830000
DIRRECAD EQU   WORKAREA+X'690'                                          15840000
DST0B790 EQU   WORKAREA+X'790'                                          15850000
DST0B7BC EQU   WORKAREA+X'7BC'                                          15860000
DST0B7C4 EQU   WORKAREA+X'7C4'                                          15870000
DST0B7DC EQU   WORKAREA+X'7DC'                                          15880000
DST0B7E6 EQU   WORKAREA+X'7E6'                                          15890000
DST0B806 EQU   WORKAREA+X'806'                                          15900000
DST0B844 EQU   WORKAREA+X'844'                                          15910000
DST0B870 EQU   WORKAREA+X'870'                                          15920000
DST0B898 EQU   WORKAREA+X'898'                                          15930000
DST0B89C EQU   WORKAREA+X'89C'                                          15940000
DST0B896 EQU   WORKAREA+X'896'                                          15950000
DST0B904 EQU   WORKAREA+X'904'                                          15960000
         SPACE 2                                                        15970000
         PRINT NOGEN                                                    15980000
         IHADCB DEVD=(DA),DSORG=PS                                      15990000
         END                                                            16000000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB                                      16010000
//SYSIN    DD  *                                                        16020000
  NAME PDSUPDTE(R)                                                      16030000
//                                                                      16040000
