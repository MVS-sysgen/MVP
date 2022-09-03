//PDSPROGM  JOB (TSO),
//             'Install PDSPROGM',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//* ********************************************************            00020000
//* *  INSTALL THE 'PDSPROGM' PROGRAM                      *            00030000
//* ********************************************************            00040000
//PDSPROGM EXEC ASMFCL,COND=(0,NE)                                      00050000
//ASM.SYSIN DD *                                                        00060000
         TITLE '   P D S P R O G M '                                    00010000
*$DOC@***************************************************************** 00020000
*                                                                     * 00030000
*  PDSPROGM: PDS SCRATCH/RENAME UTILITY PROGRAM  -  USERS GUIDE       * 00040000
*                                                                     * 00050000
*  'PDSPROGM' IS A UTILITY PROGRAM USED FOR DELETING OR RENAMING      * 00060000
*  ONE OR MORE MEMBERS OF A PARTITIONED DATA SET (PDS).               * 00070000
*                                                                     * 00080000
*  THE IBM UTILITY 'IEHPROGM' CAN DELETE AND RENAME MEMBERS,          * 00090000
*  BUT 'PDSPROGM' HAS THESE ADVANTAGES OVER IT:                       * 00100000
*    .  THE DATA SET NAME IS SPECIFIED ONLY IN A DD STATEMENT,        * 00110000
*    .  IF THE DATA SET IS CATALOGED, THE VOLUME AND                  * 00120000
*       UNIT TYPE NEED NOT BE SPECIFIED ANYWHERE,                     * 00130000
*    .  CONTROL STATEMENTS ARE SHORT AND SIMPLE,                      * 00140000
*    .  AN 'ALIAS' CAN BE CREATED FOR A MEMBER.                       * 00150000
*                                                                     * 00160000
*  'PDSPROGM' REQUIRES THE FOLLOWING DDNAMES:                         * 00170000
*    .  SYSPRINT - THE LOG AND MESSAGE OUTPUT,                        * 00180000
*    .  SYSIN    - THE CONTROL STATEMENT INPUT,                       * 00190000
*    .  SYSLIB   - THE PARTITIONED DATA SET CONTAINING                * 00200000
*                  THE MEMBER(S) TO BE PROCESSED.                     * 00210000
*                                                                     * 00220000
*  CONTROL STATEMENTS ARE 80-CHARACTER RECORDS, IN WHICH              * 00230000
*  ONLY THE FIRST 72 CHARACTERS ARE SCANNED.  THE LAST                * 00240000
*  8 POSITIONS ARE IGNORED, NOT LISTED ON THE LOG, AND                * 00250000
*  MAY BE USED FOR ANY PURPOSE, SUCH AS SEQUENCE NUMBERS.             * 00260000
*                                                                     * 00270000
*  A CONTROL STATEMENT CONSISTS OF AN OPERATION AND ONE               * 00280000
*  OR MORE OPERANDS.  THE OPERATION MAY BEGIN IN COLUMN 1 OR          * 00290000
*  MAY BE PRECEDED BY ONE OR MORE BLANKS.                             * 00300000
*                                                                     * 00310000
*  ONE OR MORE OPERANDS FOLLOW THE OPERATION, SEPARATED               * 00320000
*  FROM IT AND FROM EACH OTHER BY ONE OR MORE BLANKS.                 * 00330000
*  ANY COMMAS FOUND WILL BE TREATED AS BLANKS.                        * 00340000
*                                                                     * 00350000
*  STATEMENTS MAY NOT BE CONTINUED.                                   * 00360000
*                                                                     * 00370000
*  A STATEMENT CONTAINING ALL BLANKS WILL BE TREATED AS A             * 00380000
*  COMMENT AND MERELY LOGGED.  IF THE FIRST NONBLANK                  * 00390000
*  CHARACTER ON A STATEMENT IS AN ASTERISK (*), THE                   * 00400000
*  STATEMENT WILL BE TREATED AS A COMMENT AND MERELY LOGGED.          * 00410000
*                                                                     * 00420000
*  THE 'DELETE' STATEMENT CONSISTS OF THE OPERATION 'DELETE'          * 00430000
*  (OR 'DEL', 'D', 'SCRATCH', OR 'SCR') FOLLOWED BY ONE OPERAND,      * 00440000
*  THE NAME OF THE MEMBER TO BE DELETED.                              * 00450000
*                                                                     * 00460000
*  THE 'RENAME' STATEMENT CONTAINS THE OPERATION 'RENAME'             * 00470000
*  (OR 'REN' OR 'R') FOLLOWED BY TWO OPERANDS.  THE FIRST             * 00480000
*  OPERAND IS THE CURRENT NAME OF THE MEMBER, AND THE                 * 00490000
*  SECOND OPERAND IS THE NEW NAME OF THE MEMBER.                      * 00500000
*                                                                     * 00510000
*********************************************************************** 00520000
         EJECT                                                          00530000
*********************************************************************** 00540000
*                                                                     * 00550000
*  THE 'ALIAS' STATEMENT CONTAINS THE OPERATION 'ALIAS'               * 00560000
*  (OR 'ALI' OR 'A') FOLLOWED BY TWO OPERANDS.  THE FIRST             * 00570000
*  OPERAND IS THE NAME OF AN EXISTING MEMBER, AND THE                 * 00580000
*  SECOND OPERAND IS THE ALIAS TO BE ASSIGNED TO IT.                  * 00590000
*                                                                     * 00600000
*  THE 'OPTION' STATEMENT CONTAINS THE OPERATION 'OPTION'             * 00610000
*  (OR 'OPT' OR 'O') FOLLOWED BY ONE TO THREE OPERANDS. THE           * 00620000
*  OPERANDS MAY BE ANY OF THE FOLLOWING KEYWORDS, IN ANY ORDER:       * 00630000
*     LOG, NOLOG, MSG, NOMSG, FLUSH, NOFLUSH.                         * 00640000
*  THESE KEYWORDS CONTROL LOGGING OF CONTROL STATEMENTS,              * 00650000
*  APPEARANCE OF NORMAL MESSAGES, AND ACTION TO BE TAKEN              * 00660000
*  IN THE EVENT OF AN ERROR.  INITIAL SETTING IS LOG, MSG, FLUSH.     * 00670000
*                                                                     * 00680000
*  THE 'FILE' STATEMENT CONTAINS THE OPERATION 'FILE'                 * 00690000
*  (OR 'FIL' OR 'F') FOLLOWED BY ONE OPERAND, WHICH IS                * 00700000
*  A DDNAME.  THIS STATEMENT CAUSES ALL SUBSEQUENT                    * 00710000
*  'RENAME', 'DELETE', AND 'ALIAS' ACTIVITY TO BE                     * 00720000
*  DIRECTED AGAINST THE PDS ASSOCIATED WITH THE                       * 00730000
*  DDNAME SPECIFIED, INSTEAD OF DDNAME 'SYSLIB'.                      * 00740000
*  THIS ALLOWS MORE THAN ONE PDS TO BE PROCESSED                      * 00750000
*  IN THE SAME JOB STEP.                                              * 00760000
*                                                                     * 00770000
*  AN EXAMPLE OF SOME JCL THAT WOULD BE USED TO RUN 'PDSPROGM'.       * 00780000
*                                                                     * 00790000
*     //STEP1   EXEC PGM=PDSPROGM                                     * 00800000
*     //SYSPRINT DD  SYSOUT=A                                         * 00810000
*     //SYSLIB   DD  DSN=DSN1,DISP=OLD                                * 00820000
*     //MYLIB    DD  DSN=DSN2,DISP=OLD                                * 00830000
*     //SYSIN    DD  *                                                * 00840000
*     DELETE BACKUP                                                   * 00850000
*     DELETE CALIAS                                                   * 00860000
*     RENAME CURRENT BACKUP                                           * 00870000
*     RENAME TESTED CURRENT                                           * 00880000
*     ALIAS CURRENT CALIAS                                            * 00890000
*     FILE MYLIB                                                      * 00900000
*     DELETE BACKUP                                                   * 00910000
*                                                                     * 00920000
*  A NOTE OF WARNING ABOUT LOAD MODULES.                              * 00930000
*  'PDSPROGM' CAN BE USED TO DELETE OR RENAME A LOAD MODULE           * 00940000
*  BUT IS NOT DESIGNED TO ASSIGN AN ALIAS TO ONE.  ONLY THE           * 00950000
*  LINKAGE EDITOR SHOULD BE USED TO ACCOMPLISH THAT PROPERLY.         * 00960000
*  EVEN RENAMING A LOAD MODULE WITH 'PDSPROGM' (OR ANYTHING           * 00970000
*  OTHER THAN THE LINKAGE EDITOR) COULD CAUSE PROBLEMS, BECAUSE       * 00980000
*  IF THE MODULE HAS AN ALIAS, THE ALIAS ENTRY CONTAINS THE           * 00990000
*  ORIGINAL NAME OF IT'S ASSOCIATED MEMBER.  RENAMING A LOAD          * 01000000
*  MODULE ALIAS COULD CAUSE PROBLEMS IF THE MODULE IS LATER           * 01010000
*  RE-LINKED, BECAUSE LOAD MODULE ALIASES ARE CLOSELY TIED            * 01020000
*  TO ENTRY-POINT NAMES CODED IN THE PROGRAMS.                        * 01030000
*                                                                     * 01040000
*$DOC$***************************************************************** 01050000
         EJECT                                                          01060000
*********************************************************************** 01070000
*                                                                     * 01080000
*        'PDSPROGM'                                                   * 01090000
*                                                                     * 01100000
*********************************************************************** 01110000
         SPACE                                                          01120000
*        WRITTEN BY. BILL GODFREY,  PLANNING RESEARCH CORPORATION.      01130000
*        INSTALLATION. PRC COMPUTER CENTER INC, MCLEAN VA.              01140000
*        DATE WRITTEN. MAY 24 1979.                                     01150000
*        DATE UPDATED. JUNE 16 1980.                                    01160000
*        ATTRIBUTES. RE-ENTRANT.                                        01170000
*        DESCRIPTION.                                                   01180000
*            THIS PROGRAM SCRATCHES AND RENAMES MEMBERS OF A            01190000
*            PARTITIONED DATA SET (PDS).                                01200000
*                                                                       01210000
*            OPERATIONS ARE SPECIFIED ON CONTROL STATEMENTS (SYSIN)     01220000
*            AND ARE PERFORMED AGAINST THE DIRECTORY OF THE             01230000
*            DATA SET ALLOCATED TO DDNAME SYSLIB.                       01240000
*            VALID OPERATIONS ARE:                                      01250000
*             'DELETE' (OR 'SCRATCH'), 'RENAME', AND 'ALIAS'            01260000
*            THE OPERATION MUST BE FOLLOWED BY ONE OR MORE SPACES       01270000
*            AND THEN ONE OPERAND (FOR DELETE) OR TWO OPERANDS          01280000
*            (FOR RENAME OR ALIAS). THE FIRST OPERAND MUST BE AN        01290000
*            EXISTING MEMBER NAME.  THE SECOND OPERAND MUST BE A        01300000
*            NON-EXISTING MEMBER NAME.  THE SECOND OPERAND IS           01310000
*            SEPARATED FROM THE FIRST BY ONE OR MORE SPACES/COMMAS.     01320000
*            COLUMNS 73-80 ARE IGNORED, MAY BE USED FOR SEQUENCING.     01330000
*            STATEMENTS BEGINNING WITH AN ASTERISK ARE IGNORED.         01340000
*            AN 'OPTION' CONTROL STATEMENT ALLOWS OPTIONS TO            01350000
*            BE SET RELATING TO LOGGING, MESSAGES, AND ERRORS.          01360000
*            A 'FILE' CONTROL STATEMENT ALLOWS OPERATIONS TO            01370000
*            BE DIRECTED TO ANOTHER DDNAME. THE ONLY OPERAND            01380000
*            IS A DDNAME. 'SYSLIB' IS ALWAYS OPENED INITIALLY,          01390000
*            EVEN IF THE FIRST CONTROL STATEMENT IS A 'FILE'            01400000
*            STATEMENT.                                                 01410000
*                                                                       01420000
*            THE PARM FIELD MAY CONTAIN THE FOLLOWING KEYWORDS:         01430000
*            NL  - CONTROL STATEMENTS WILL NOT BE LOGGED                01440000
*            NM  - NORMAL MESSAGES WILL NOT BE LOGGED, ONLY ERRORS.     01450000
*            NF  - ERRORS WILL NOT CAUSE FLUSHING OF INPUT.             01460000
*            NR  - NO RESTRICTIONS ON CHARACTERS IN A NEW MEMBER.       01470000
*            NC  - NO CAPITALIZATION WILL BE DONE.                      01480000
*                                                                       01490000
*            CHANGES ...                                                01500000
*            RENAME AND ALIAS FEATURES ADDED 24MAY79.                   01510000
*            OPTIONS, PARMS AND SPECIAL CARDS ADDED 24MAY79.            01520000
*            REPORT WRITER ADDED 14DEC79.                               01530000
*            SCR, DEL, REN, ALI ADDED 14DEC79.                          01540000
*            STATUS BITS ADDED 14DEC79.                                 01550000
*            SYSPRINT OPEN EXIT ADDED 14DEC79.                          01560000
*            FILE STATEMENT ADDED. 14DEC79.                             01570000
*            COMMENT CARD ADDED. 11APR80.                               01580000
*            MESSAGE ID'S ADDED. ALIAS LMOD CHECKS ADDED. 16APR80.      01590000
*            SYSIN PARSE REWRITTEN. VOLSER IN HEADER. 18APR80.          01600000
*            OPTION STATEMENT ADDED. 29APR80.                           01610000
*            NO CODE CHANGE. USERS GUIDE ADDED AT TOP. 16JUN80.         01620000
         SPACE                                                          01630000
         MACRO                                                          01640000
&NAME    PARMC &C,&B                                                    01650000
&NAME    CLI   0(R1),C'&C'         IS CHARACTER PRESENT?                01660000
         BNE   *+20                NO, SKIP REMAINING INSTRUCTIONS      01670000
         OI    OPTIONS,&B          YES, SET BIT ON                      01680000
         CLI   1(R2),C'N'          IS CHARACTER PRECEDED BY 'N'?        01690000
         BNE   *+8                 NO, SKIP NEXT INSTRUCTION            01700000
         NI    OPTIONS,255-&B      YES, SET BIT OFF                     01710000
         MEND                                                           01720000
         SPACE                                                          01730000
PDSPROGM START                                                          01740000
         USING *,R10,R11                                                01750000
         B     @PROLOG-*(,15)                                           01760000
         DC    AL1(11),CL11'PDSPROGM'                                   01770000
         DC    CL16' &SYSDATE &SYSTIME '                                01780000
@SIZE    DC    0F'0',AL1(1),AL3(@DATAL)                                 01790000
@PROLOG  STM   14,12,12(13)                                             01800000
         LR    R10,15              BASE REGISTER                        01810000
         LA    R15,4095                                                 01820000
         LA    R11,1(R15,R10)      BASE REGISTER                        01830000
         LR    R2,R1                                                    01840000
         L     R0,@SIZE                                                 01850000
         GETMAIN R,LV=(0)                                               01860000
         ST    13,4(,1)                                                 01870000
         ST    1,8(,13)                                                 01880000
         LR    13,1                                                     01890000
         USING @DATA,R13                                                01900000
         SPACE 1                                                        01910000
         SLR   R15,R15                                                  01920000
         ST    R15,COUNTIN                                              01930000
         STH   R15,RC                                                   01940000
         MVI   OPTIONS,X'BC'       CAPS,,RES,FLUSH,LOG,MSG              01950000
         L     R2,0(,R2)           PARM                                 01960000
         LH    R0,0(,R2)           PARM LENGTH                          01970000
         LTR   R0,R0               IS PARM PRESENT                      01980000
         BZ    NOPARM              NO, BRANCH                           01990000
         LA    R1,2(,R2)           POINT TO PARM DATA                   02000000
PARMLOOP EQU   *                                                        02010000
         PARMC C,#C                CAPS                                 02020000
         SPACE                                                          02030000
         PARMC L,#L                LOG CONTROL STATEMENTS               02040000
         SPACE                                                          02050000
         PARMC M,#M                NORMAL MESSAGES                      02060000
         SPACE                                                          02070000
         PARMC R,#R                RESTRICTED MEMBER NAMES              02080000
         SPACE                                                          02090000
         PARMC F,#F                FLUSH AFTER FIRST ERROR              02100000
         SPACE                                                          02110000
         LA    R1,1(,R1)                                                02120000
         LA    R2,1(,R2)                                                02130000
         BCT   R0,PARMLOOP         CONTINUE SCAN                        02140000
NOPARM   EQU   *                                                        02150000
         MVI   UPPER,C' '                                               02160000
         MVC   UPPER+1(71),UPPER                                        02170000
         SPACE                                                          02180000
         ZAP   REPORTPG,=P'0'      INITIAL PAGE COUNTER                 02190000
         ZAP   REPORTLN,=P'0'      INITIAL LINE COUNTER                 02200000
         ZAP   REPORTMX,=P'50'     INITIAL LINES PER PAGE               02210000
         MVI   REPORTO-1,X'40'     BLANK PROPOGATOR                     02220000
         MVI   LINE-1,X'40'        BLANK PROPOGATOR                     02230000
         MVI   STATUS,0                                                 02240000
         EJECT                                                          02250000
************************************************************            02260000
*                                                          *            02270000
*        OPEN THE DCB'S                                    *            02280000
*                                                          *            02290000
************************************************************            02300000
         SPACE                                                          02310000
         MVC   PRTDCBW(PRTDCBL),PRTDCB                                  02320000
         MVC   CTLDCBW(CTLDCBL),CTLDCB                                  02330000
         MVC   LIBDCBW(LIBDCBL),LIBDCB                                  02340000
         LA    R3,PRTDCBW                                               02350000
         LA    R4,CTLDCBW                                               02360000
         LA    R5,LIBDCBW                                               02370000
         SPACE                                                          02380000
         IC    R0,EXLST(,R3)                                            02390000
         LA    R14,PRTEXLST                                             02400000
         ST    R14,EXLST(,R3)                                           02410000
         STC   R0,EXLST(,R3)       SYSPRINT EXLST=PRTEXLST              02420000
         SPACE                                                          02430000
         LA    R0,PRTEXO                                                02440000
         ST    R0,0(,R14)                                               02450000
         MVI   0(R14),X'85'        DCB EXIT                             02460000
         SPACE                                                          02470000
         IC    R0,EODAD(,R4)                                            02480000
         LA    R14,EODCTL                                               02490000
         ST    R14,EODAD(,R4)                                           02500000
         STC   R0,EODAD(,R4)       SYSIN EODAD=EODCTL                   02510000
         SPACE                                                          02520000
         IC    R0,EXLST(,R5)                                            02530000
         LA    R14,LIBEXLST                                             02540000
         ST    R14,EXLST(,R5)                                           02550000
         STC   R0,EXLST(,R5)       SYSLIB EXLST=LIBEXLST                02560000
         SPACE                                                          02570000
         LA    R0,JFCB                                                  02580000
         ST    R0,0(,R14)                                               02590000
         MVI   0(R14),X'87'        JFCB EXIT                            02600000
         SPACE                                                          02610000
         LA    R6,OPEN                                                  02620000
         MVI   0(R6),X'80'                                              02630000
         SPACE                                                          02640000
         OPEN  ((R3),OUTPUT),MF=(E,(R6))                                02650000
         SPACE                                                          02660000
         TM    OFLGS(R3),X'10'                                          02670000
         BO    OKPRT                                                    02680000
         LA    R15,16                                                   02690000
         B     EXIT                                                     02700000
PRTEXO   CLC   BLKSI(2,R1),=H'0'                                        02710000
         BNER  R14                                                      02720000
         MVC   BLKSI(2,R1),LRECL(R1)                                    02730000
         NI    RECFM(R1),255-X'10' CHANGE RECFM TO FA                   02740000
         BR    R14                                                      02750000
OKPRT    OI    STATUS,X'80'        SYSPRINT IS OPEN                     02760000
         SPACE                                                          02770000
         OPEN  ((R4),INPUT),MF=(E,(R6))                                 02780000
         SPACE                                                          02790000
         TM    OFLGS(R4),X'10'                                          02800000
         BO    OKCTL                                                    02810000
         LA    R15,16                                                   02820000
         B     EXIT                                                     02830000
OKCTL    OI    STATUS,X'40'        SYSIN IS OPEN                        02840000
         SPACE                                                          02850000
         OPEN  ((R5),UPDAT),MF=(E,(R6))                                 02860000
         SPACE                                                          02870000
         TM    OFLGS(R5),X'10'                                          02880000
         BO    OKLIB                                                    02890000
         LA    R15,16                                                   02900000
         B     EXIT                                                     02910000
OKLIB    OI    STATUS,X'20'        SYSLIB IS OPEN                       02920000
         SPACE                                                          02930000
         RDJFCB ((R5)),MF=(E,(R6))                                      02940000
         SPACE                                                          02950000
         MVC   LINE,LINE-1                                              02960000
         BAL   R14,REPORT                                               02970000
         EJECT                                                          02980000
************************************************************            02990000
*                                                          *            03000000
*        READ A CONTROL STATEMENT                          *            03010000
*                                                          *            03020000
************************************************************            03030000
         SPACE                                                          03040000
READ     EQU   *                                                        03050000
         CLI   RC+1,0                                                   03060000
         BE    READGET                                                  03070000
         TM    OPTIONS,#F          FLUSH                                03080000
         BO    FLUSH               YES, QUIT AT FIRST ERROR             03090000
READGET  EQU   *                                                        03100000
         GET   (R4),INREC                                               03110000
         SPACE                                                          03120000
         L     R1,COUNTIN                                               03130000
         A     R1,=F'1'                                                 03140000
         ST    R1,COUNTIN                                               03150000
         TM    OPTIONS,#C          CAPS                                 03160000
         BZ    *+10                                                     03170000
         OC    INREC(72),UPPER                                          03180000
         MVC   LINE,LINE-1                                              03190000
         TM    OPTIONS,#L          LOG                                  03200000
         BZ    NOLOG               BRANCH IF NOLOG                      03210000
         MVC   MID(6),=C'MSG000'                                        03220000
         MVC   TXT(72),INREC                                            03230000
         BAL   R14,REPORT                                               03240000
NOLOG    EQU   *                                                        03250000
         SPACE                                                          03260000
************************************************************            03270000
*                                                          *            03280000
*         PARSE THE CONTROL STATEMENT                      *            03290000
*                                                          *            03300000
************************************************************            03310000
         SPACE                                                          03320000
         LA    R15,INREC           POINT TO COLUMN 1                    03330000
         LA    R3,72-1             LENGTH CODE OF CONTROL STATEMENT     03340000
         LA    R6,ODL              POINT TO OPERAND DESCRIPTOR LIST     03350000
         XC    0(ODLL,R6),0(R6)    ZERO THE ODL                         03360000
         SR    R1,R1               INSURE HI ORDER BYTE ZERO            03370000
         LA    R0,ODLL/8-1         NUMBER OF ENTRIES IN O.D.L.          03380000
*                                  MINUS 1 (LAST ODE WILL REMAIN ZERO)  03390000
         B     LOOP                                                     03400000
TRTNONBL TRT   0(0,R15),TABNONBL   (EXECUTED)                           03410000
TRTBLANK TRT   0(0,R15),TABBLANK   (EXECUTED)                           03420000
LOOP     XC    0(8,R6),0(R6)       ZERO THE OPERAND DESCRIPTOR ENTRY    03430000
         EX    R3,TRTNONBL         FIND A NONBLANK                      03440000
         BZ    DONE                BRANCH IF ALL BLANKS                 03450000
         LR    R14,R1              GET ADDRESS OF STRING                03460000
         SR    R14,R15             GET LENGTH OF PRECEDING BLANKS       03470000
         SR    R3,R14              GET LENGTH OF REMAINING TEXT         03480000
         LR    R15,R1              GET ADDRESS OF NONBLANK              03490000
         EX    R3,TRTBLANK         FIND A BLANK                         03500000
         BZ    LAST                BRANCH IF NOT FOUND                  03510000
         LR    R14,R1              GET ADDRESS OF BLANK                 03520000
         SR    R14,R15             GET LENGTH OF FIELD                  03530000
         OI    6(R6),X'80'         OPERAND PRESENT                      03540000
         ST    R15,0(,R6)          ADDRESS OF OPERAND                   03550000
         STH   R14,4(,R6)          LENGTH OF OPERAND                    03560000
         SR    R3,R14              GET LENGTH CODE OF REMAINING TEXT    03570000
         BZ    DONE                BRANCH IF ONE TRAILING BLANK         03580000
         LA    R6,8(,R6)           POINT TO NEXT O.D.E.                 03590000
         LR    R15,R1              POINT TO BLANK                       03600000
         BCT   R0,LOOP                                                  03610000
         B     DONE                                                     03620000
LAST     LA    R14,1(,R3)          GET LENGTH                           03630000
         OI    6(R6),X'80'         OPERAND PRESENT                      03640000
         ST    R15,0(,R6)          ADDRESS OF OPERAND                   03650000
         STH   R14,4(,R6)          LENGTH OF OPERAND                    03660000
DONE     EQU   *                                                        03670000
         SPACE                                                          03680000
************************************************************            03690000
*                                                          *            03700000
*         DETERMINE TYPE OF CONTROL STATEMENT              *            03710000
*                                                          *            03720000
************************************************************            03730000
         SPACE                                                          03740000
         LA    R6,ODE1             POINT TO FIRST O.D.E.                03750000
         TM    6(R6),X'80'         ANYTHING PRESENT                     03760000
         BZ    READ                BRANCH IF WHOLE STATEMENT BLANK      03770000
         L     R1,0(,R6)           POINT TO FIRST NONBLANK IN STATEMENT 03780000
         CLI   0(R1),C'*'          COMMENT CARD                         03790000
         BE    READ                YES, BRANCH                          03800000
         SPACE                                                          03810000
         LA    R6,ODE2             POINT TO SECOND O.D.E.               03820000
         TM    6(R6),X'80'         IS OPERAND PRESENT ?                 03830000
         BZ    NOOPND              BRANCH IF MISSING OPERAND            03840000
         MVC   MEMBER,=CL8' '                                           03850000
         L     R1,0(,R6)           POINT TO STRING                      03860000
         LH    R15,4(,R6)          GET LENGTH OF STRING                 03870000
         LA    R0,8                MAX VALID LENGTH                     03880000
         CR    R15,R0              IS LENGTH VALID ?                    03890000
         BH    NOOPND              BRANCH IF OPERAND TOO LONG           03900000
         BCTR  R15,0               LENGTH MINUS 1 FOR EX                03910000
         EX    R15,MVCMEM          MOVE STRING TO MEMBER                03920000
         OI    6(R6),X'40'         MARK OPERAND MOVED                   03930000
NOOPND   EQU   *                                                        03940000
         SPACE                                                          03950000
         LA    R6,ODE1             RESTORE POINTER TO FIRST O.D.E.      03960000
         L     R1,0(,R6)           POINT TO FIRST STRING AGAIN          03970000
         LH    R15,4(,R6)          GET LENGTH OF STRING                 03980000
         MVC   TYPE(1),0(R1)       SAVE FIRST CHARACTER                 03990000
         LA    R0,7                MAX VALID LENGTH                     04000000
         CR    R15,R0              IS STRING TOO LONG ?                 04010000
         BH    ILLCARD             BRANCH IF TOO LONG                   04020000
         SLL   R15,2               MULTIPLY LENGTH BY 4                 04030000
         B     *(R15)              BRANCH TO ONE OF NEXT 7              04040000
         B     LEN1                1 CHAR                               04050000
         B     ILLCARD             2 CHAR                               04060000
         B     LEN3                3 CHAR                               04070000
         B     LEN4                                                     04080000
         B     LEN5                                                     04090000
         B     LEN6                                                     04100000
         B     LEN7                                                     04110000
LEN1     CLI   0(R1),C'S'                                               04120000
         BE    SCRATCH                                                  04130000
         CLI   0(R1),C'D'                                               04140000
         BE    SCRATCH                                                  04150000
         CLI   0(R1),C'R'                                               04160000
         BE    RENAME                                                   04170000
         CLI   0(R1),C'A'                                               04180000
         BE    RENAME                                                   04190000
         CLI   0(R1),C'F'                                               04200000
         BE    FILE                                                     04210000
         CLI   0(R1),C'E'                                               04220000
         BE    EODCTL                                                   04230000
         CLI   0(R1),C'O'                                               04240000
         BE    OPTION                                                   04250000
TYPEI    MVI   TYPE,C'I'                                                04260000
         B     ILLCARD                                                  04270000
LEN3     EQU   *                                                        04280000
         CLC   0(3,R1),=C'SCRATCH' SCR                                  04290000
         BE    SCRATCH                                                  04300000
         CLC   0(3,R1),=C'DELETE'  DEL                                  04310000
         BE    SCRATCH                                                  04320000
         CLC   0(3,R1),=C'RENAME'  REN                                  04330000
         BE    RENAME                                                   04340000
         CLC   0(3,R1),=C'ALIAS'   ALI                                  04350000
         BE    RENAME                                                   04360000
         CLC   0(3,R1),=C'END'     END                                  04370000
         BE    EODCTL                                                   04380000
         CLC   0(3,R1),=C'FILE'    FIL                                  04390000
         BE    FILE                                                     04400000
         CLC   0(3,R1),=C'OPTION'                                       04410000
         BE    OPTION                                                   04420000
         B     TYPEI                                                    04430000
LEN7     CLC   0(7,R1),=C'SCRATCH'                                      04440000
         BE    SCRATCH                                                  04450000
         B     TYPEI                                                    04460000
LEN6     CLC   0(6,R1),=C'DELETE'                                       04470000
         BE    SCRATCH                                                  04480000
         CLC   0(6,R1),=C'RENAME'                                       04490000
         BE    RENAME                                                   04500000
         CLC   0(6,R1),=C'OPTION'                                       04510000
         BE    OPTION                                                   04520000
         B     TYPEI                                                    04530000
LEN5     CLC   0(5,R1),=C'ALIAS'                                        04540000
         BE    RENAME                                                   04550000
         B     TYPEI                                                    04560000
LEN4     CLC   0(4,R1),=C'FILE'                                         04570000
         BE    FILE                                                     04580000
         B     TYPEI                                                    04590000
         SPACE                                                          04600000
ILLCARD  EQU   *                                                        04610000
         CLI   TYPE,C'F'                                                04620000
         MVI   TYPE,C'I'                                                04630000
         BE    FILE                                                     04640000
         MVC   LINE,LINE-1                                              04650000
         TM    OPTIONS,#L          LOG                                  04660000
         BNZ   LOGGED              YES, BRANCH                          04670000
         MVC   MID(6),=C'MSG000'                                        04680000
         MVC   TXT(72),INREC                                            04690000
         BAL   R14,REPORT                                               04700000
         MVC   LINE,LINE-1                                              04710000
LOGGED   MVC   MID(33),=C'MSG001  *** ILLEGAL STATEMENT ***'            04720000
ILLRC4   BAL   R14,REPORT                                               04730000
         MVI   RC+1,4                                                   04740000
         B     READ                                                     04750000
         SPACE                                                          04760000
ILLPREV  MVC   MID(29),=C'MSG012  *** FILE NOT OPEN ***'                04770000
         B     ILLRC4                                                   04780000
         SPACE                                                          04790000
************************************************************            04800000
*                                                          *            04810000
*         CHANGE DDNAME OF LIBRARY                         *            04820000
*                                                          *            04830000
************************************************************            04840000
         SPACE                                                          04850000
FILE     EQU   *                                                        04860000
         ZAP   REPORTLN,=P'0'      FORCE NEW PAGE                       04870000
         LA    R6,CLOSE                                                 04880000
         MVI   0(R6),X'80'                                              04890000
         TM    STATUS,X'20'                                             04900000
         BZ    FILEC5X                                                  04910000
         CLOSE ((R5)),MF=(E,(R6))                                       04920000
         NI    STATUS,255-X'20'                                         04930000
FILEC5X  EQU   *                                                        04940000
         CLI   TYPE,C'I'                                                04950000
         BE    FILERR                                                   04960000
         LA    R6,ODE2             POINT TO SECOND O.D.E.               04970000
         TM    6(R6),X'C0'         IS OPERAND PRESENT AND MOVED ?       04980000
         BNO   FILERR              BRANCH IF MISSING OR INVALID OPERAND 04990000
         MVC   DDNAM(8,R5),MEMBER                                       05000000
         LA    R6,CLOSE                                                 05010000
         OPEN  ((R5),UPDAT),MF=(E,(R6))                                 05020000
         TM    OFLGS(R5),X'10'                                          05030000
         BO    OKFILE                                                   05040000
         MVC   MID(35),=C'MSG011  FILE XXXXXXXX NOT AVAILABLE'          05050000
         MVC   MID+13(8),MEMBER                                         05060000
FILERC4  MVI   JFCB,C' '                                                05070000
         MVC   JFCB+1(43),JFCB                                          05080000
         BAL   R14,REPORT                                               05090000
         MVI   RC+1,4                                                   05100000
         B     READ                                                     05110000
OKFILE   EQU   *                                                        05120000
         OI    STATUS,X'20'        SYSLIB FILE IS OPEN                  05130000
         RDJFCB ((R5)),MF=(E,(R6))                                      05140000
         MVC   LINE,LINE-1                                              05150000
         MVC   MID(19),=C'MSG006  NEW FILE IS'                          05160000
         MVC   MID+20(8),MEMBER    DDNAME                               05170000
         BAL   R14,REPORT                                               05180000
         B     READ                                                     05190000
         SPACE                                                          05200000
FILERR   MVC   MID(38),=C'MSG010  *** INVALID FILE STATEMENT ***'       05210000
         B     FILERC4                                                  05220000
         EJECT                                                          05230000
************************************************************            05240000
*                                                          *            05250000
*        DELETE A MEMBER                                   *            05260000
*                                                          *            05270000
************************************************************            05280000
         SPACE                                                          05290000
SCRATCH  EQU   *                                                        05300000
         TM    STATUS,X'20'        IS SYSLIB OPEN?                      05310000
         BZ    ILLPREV             NO, ERROR                            05320000
         SPACE                                                          05330000
         LA    R6,ODE2             POINT TO SECOND O.D.E.               05340000
         TM    6(R6),X'C0'         IS OPERAND PRESENT AND MOVED ?       05350000
         BNO   ILLCARD             BRANCH IF MISSING OR INVALID OPERAND 05360000
         SPACE                                                          05370000
         MVC   LINE,LINE-1                                              05380000
         MVC   MID(14),=C'MSGXXX  MEMBER'                               05390000
         MVC   MBR(8),MEMBER                                            05400000
         LA    R6,MEMBER                                                05410000
         SPACE                                                          05420000
         STOW  (R5),(R6),D                                              05430000
         SPACE                                                          05440000
         B     STOWDR(R15)                                              05450000
STOWDR   B     STOWDR00                                                 05460000
         B     STOWDR04                                                 05470000
         B     STOWDR08                                                 05480000
         B     STOWDR0C                                                 05490000
         B     STOWDR10                                                 05500000
         B     STOWDR14                                                 05510000
         B     STOWDR18                                                 05520000
         SPACE                                                          05530000
STOWDR00 TRT   MBR(9),TABBLANK                                          05540000
         MVC   1(7,R1),=CL7'DELETED'                                    05550000
         CLI   TYPE,C'S'                                                05560000
         BNE   *+10                                                     05570000
         MVC   1(9,R1),=C'SCRATCHED'                                    05580000
         MVC   MID+3(3),=C'003'                                         05590000
         B     STOWCOMP                                                 05600000
STOWDR04 MVC   RESULT(23),=CL23'STOW RC=04 (IMPOSSIBLE)'                05610000
         B     STOWCOM                                                  05620000
STOWDR08 MVC   RESULT(09),=CL09'NOT FOUND'                              05630000
         B     STOWCOM                                                  05640000
STOWDR0C MVC   RESULT(23),=CL23'STOW RC=12 (IMPOSSIBLE)'                05650000
         B     STOWCOM                                                  05660000
STOWDR10 MVC   RESULT(17),=CL17'*** I/O ERROR ***'                      05670000
         B     STOWCOM                                                  05680000
STOWDR14 MVC   RESULT(17),=CL17'*** DCB ERROR ***'                      05690000
         B     STOWCOM                                                  05700000
STOWDR18 MVC   RESULT(28),=CL28'*** INSUFFICIENT STORAGE ***'           05710000
         B     STOWCOM                                                  05720000
         SPACE                                                          05730000
STOWCOM  EQU   *                                                        05740000
         MVI   RC+1,4                                                   05750000
         MVC   MID+3(3),=C'007'                                         05760000
         B     STOWCOMW                                                 05770000
STOWCOMP EQU   *                                                        05780000
         TM    OPTIONS,#M          MSG                                  05790000
         BZ    READ                                                     05800000
STOWCOMW EQU   *                                                        05810000
         BAL   R14,REPORT                                               05820000
         B     READ                                                     05830000
         SPACE                                                          05840000
MVCMEM   MVC   MEMBER(0),0(R1)     (EXECUTED)                           05850000
MVCNEW   MVC   NEWMEM(0),0(R1)     (EXECUTED)                           05860000
         EJECT                                                          05870000
************************************************************            05880000
*                                                          *            05890000
*        RENAME A MEMBER                                   *            05900000
*                                                          *            05910000
************************************************************            05920000
         SPACE                                                          05930000
RENAME   EQU   *                                                        05940000
         TM    STATUS,X'20'        IS SYSLIB OPEN?                      05950000
         BZ    ILLPREV             NO, ERROR                            05960000
         SPACE                                                          05970000
         LA    R6,ODE2             POINT TO SECOND O.D.E.               05980000
         TM    6(R6),X'80'         IS OPERAND PRESENT AND MOVED ?       05990000
         BNO   ILLCARD             BRANCH IF MISSING OR INVALID OPERAND 06000000
         MVC   LINE,LINE-1                                              06010000
         MVC   MID(14),=C'MSGXXX  MEMBER'                               06020000
         MVC   MBR(8),MEMBER                                            06030000
         SPACE                                                          06040000
         LA    R6,ODE3             POINT TO THIRD O.D.E.                06050000
         TM    6(R6),X'80'         IS OPERAND PRESENT ?                 06060000
         BZ    ILLCARD             BRANCH IF MISSING OPERAND            06070000
         MVC   NEWMEM,=CL8' '                                           06080000
         L     R1,0(,R6)           POINT TO STRING                      06090000
         LH    R15,4(,R6)          GET LENGTH OF STRING                 06100000
         LA    R0,8                MAX VALID LENGTH                     06110000
         CR    R15,R0              IS LENGTH VALID ?                    06120000
         BH    ILLCARD             BRANCH IF OPERAND TOO LONG           06130000
         BCTR  R15,0               LENGTH MINUS 1 FOR EX                06140000
         EX    R15,MVCNEW          MOVE STRING TO NEWMEM                06150000
         SPACE                                                          06160000
         TM    OPTIONS,#R          RESTRICTING CHARACTERS               06170000
         BZ    NORES               NO, BRANCH                           06180000
         TRT   NEWMEM,ALPHANUM     ARE CHARACTERS VALID                 06190000
         BNZ   ILLCHAR             NO, BRANCH                           06200000
         CLI   NEWMEM,C'0'         IS FIRST CHAR NUMERIC                06210000
         BNL   ILLCHAR             YES, BRANCH                          06220000
         CLI   NEWMEM,C'-'         IS FIRST CHAR HYPHEN                 06230000
         BE    ILLCHAR             YES, BRANCH                          06240000
         CLI   NEWMEM,X'C0'        IS FIRST CHAR X'C0'                  06250000
         BE    ILLCHAR             YES, BRANCH                          06260000
NORES    EQU   *                                                        06270000
         CLC   MEMBER,NEWMEM       NAMES IDENTICAL?                     06280000
         BE    ILLCARD             YES, ERROR.                          06290000
         CLI   TYPE,C'R'                                                06300000
         BNE   ALIAS                                                    06310000
         LA    R6,MEMBER                                                06320000
         SPACE                                                          06330000
         STOW  (R5),(R6),C                                              06340000
         SPACE                                                          06350000
         B     STOWCR(R15)                                              06360000
STOWCR   B     STOWCR00                                                 06370000
         B     STOWCR04                                                 06380000
         B     STOWCR08                                                 06390000
         B     STOWCR0C                                                 06400000
         B     STOWCR10                                                 06410000
         B     STOWCR14                                                 06420000
         B     STOWCR18                                                 06430000
         SPACE                                                          06440000
STOWCR00 TRT   MBR(9),TABBLANK                                          06450000
         MVC   1(7,R1),=CL7'RENAMED'                                    06460000
         MVC   9(8,R1),NEWMEM                                           06470000
         MVC   MID+3(3),=C'004'                                         06480000
         B     STOWCOMP                                                 06490000
STOWCR04 MVC   RESULT(35),=CL35'NOT RENAMED XXXXXXXX ALREADY EXISTS'    06500000
         MVC   RESULT+12(8),NEWMEM                                      06510000
         B     STOWCOM                                                  06520000
STOWCR08 MVC   RESULT(09),=CL09'NOT FOUND'                              06530000
         B     STOWCOM                                                  06540000
STOWCR0C MVC   RESULT(34),=CL34'NOT RENAMED, NO SPACE IN DIRECTORY'     06550000
         B     STOWCOM                                                  06560000
STOWCR10 MVC   RESULT(17),=CL17'*** I/O ERROR ***'                      06570000
         B     STOWCOM                                                  06580000
STOWCR14 MVC   RESULT(17),=CL17'*** DCB ERROR ***'                      06590000
         B     STOWCOM                                                  06600000
STOWCR18 MVC   RESULT(28),=CL28'*** INSUFFICIENT STORAGE ***'           06610000
         B     STOWCOM                                                  06620000
ILLCHAR  MVC   RESULT(32),=CL32'NEW MEMBER NAME XXXXXXXX INVALID'       06630000
         MVC   RESULT+16(8),NEWMEM                                      06640000
         B     STOWCOM                                                  06650000
         SPACE                                                          06660000
************************************************************            06670000
*                                                          *            06680000
*        ADD AN ALIAS TO A MEMBER                          *            06690000
*                                                          *            06700000
************************************************************            06710000
         SPACE                                                          06720000
ALIAS    EQU   *                                                        06730000
         LA    R6,BLDL                                                  06740000
         MVC   0(4,R6),=AL2(1,80)                                       06750000
         MVC   4(8,R6),MEMBER                                           06760000
         SPACE                                                          06770000
         BLDL  (R5),(R6)                                                06780000
         SPACE                                                          06790000
         B     BLDLRC(R15)                                              06800000
BLDLRC   B     BLDLRC00                                                 06810000
         B     BLDLRC04                                                 06820000
         B     BLDLRC08                                                 06830000
         B     BLDLRC0C                                                 06840000
         SPACE                                                          06850000
BLDLRC04 MVC   RESULT(09),=CL09'NOT FOUND'                              06860000
         B     STOWCOM                                                  06870000
BLDLRC08 MVC   RESULT(17),=CL17'*** I/O ERROR ***'                      06880000
         B     STOWCOM                                                  06890000
BLDLRC0C MVC   RESULT(17),=CL17'*** DCB ERROR ***'                      06900000
         B     STOWCOM                                                  06910000
BLDLRC00 LA    R6,4(,R6)                                                06920000
         CLI   11(R6),0            CONCATENATION ZERO                   06930000
         BNE   BLDLRC04            NO, TREAT AS NOT FOUND               06940000
         TM    36(R5),X'C0'        IS RECFM=U                           06950000
         BO    LMOD                YES, ISSUE LOAD MODULE MESSAGE       06960000
ALIGO    MVC   0(8,R6),NEWMEM                                           06970000
         MVC   11(74-11,R6),13(R6) REMOVE 2 BLDL BYTES                  06980000
         OI    11(R6),X'80'        SET ALIAS BIT                        06990000
         SPACE                                                          07000000
         STOW  (R5),(R6),A                                              07010000
         SPACE                                                          07020000
         B     STOWAR(R15)                                              07030000
STOWAR   B     STOWAR00                                                 07040000
         B     STOWAR04                                                 07050000
         B     STOWAR08                                                 07060000
         B     STOWAR0C                                                 07070000
         B     STOWAR10                                                 07080000
         B     STOWAR14                                                 07090000
         B     STOWAR18                                                 07100000
         SPACE                                                          07110000
STOWAR00 TRT   MBR(9),TABBLANK                                          07120000
         MVC   1(7,R1),=CL7'ALIASED'                                    07130000
         MVC   9(8,R1),NEWMEM                                           07140000
         MVC   MID+3(3),=C'005'                                         07150000
         B     STOWCOMP                                                 07160000
STOWAR04 MVC   RESULT(35),=CL35'NOT ALIASED XXXXXXXX ALREADY EXISTS'    07170000
         MVC   RESULT+12(8),NEWMEM                                      07180000
         B     STOWCOM                                                  07190000
STOWAR08 MVC   RESULT(23),=CL23'STOW RC=08 (IMPOSSIBLE)'                07200000
         B     STOWCOM                                                  07210000
STOWAR0C MVC   RESULT(34),=CL34'NOT ALIASED, NO SPACE IN DIRECTORY'     07220000
         B     STOWCOM                                                  07230000
STOWAR10 MVC   RESULT(17),=CL17'*** I/O ERROR ***'                      07240000
         B     STOWCOM                                                  07250000
STOWAR14 MVC   RESULT(17),=CL17'*** DCB ERROR ***'                      07260000
         B     STOWCOM                                                  07270000
STOWAR18 MVC   RESULT(28),=CL28'*** INSUFFICIENT STORAGE ***'           07280000
         B     STOWCOM                                                  07290000
LMOD     EQU   *                                                        07300000
         TM    13(R6),X'60'        ANY TTRS IN USER DATA                07310000
         BZ    ALIGO               NO, NOT A LMOD                       07320000
         LA    R6,ODE4             POINT TO FOURTH O.D.E.               07330000
         TM    6(R6),X'80'                                              07340000
         BZ    LMODERR                                                  07350000
         LH    R15,4(,R6)          GET LENGTH                           07360000
         L     R1,0(,R6)           POINT TO STRING                      07370000
         LA    R0,5                                                     07380000
         CR    R15,R0                                                   07390000
         BNE   LMODERR                                                  07400000
         CLC   0(5,R1),=C'FORCE'                                        07410000
         BNE   LMODERR                                                  07420000
         LA    R6,BLDL+4                                                07430000
         B     ALIGO                                                    07440000
LMODERR  MVC   RESULT(L'LMODMSG),LMODMSG                                07450000
         MVI   RC+1,4                                                   07460000
         MVC   MID+3(3),=C'007'                                         07470000
         B     STOWCOMW                                                 07480000
         EJECT                                                          07490000
************************************************************            07500000
*                                                          *            07510000
*         SET OPTIONS BY CONTROL STATEMENT                 *            07520000
*                                                          *            07530000
************************************************************            07540000
         SPACE                                                          07550000
OPTION   EQU   *                                                        07560000
         LA    R6,ODE2                                                  07570000
         TM    6(R6),X'C0'                                              07580000
         BNO   ILLCARD                                                  07590000
CTLSPEC  L     R1,0(,R6)           POINT TO OPERAND                     07600000
         LH    R15,4(,R6)          GET LENGTH OF KEYWORD                07610000
         CH    R15,=H'7'                                                07620000
         BE    CTL7                                                     07630000
         CH    R15,=H'5'                                                07640000
         BE    CTL5                                                     07650000
         CH    R15,=H'3'                                                07660000
         BNE   ILLCARD                                                  07670000
CTL3     CLC   0(3,R1),OPMSG+2                                          07680000
         BNE   *+12                                                     07690000
         OI    OPTIONS,#M          MSG                                  07700000
         B     CTLNEXT                                                  07710000
         CLC   0(3,R1),OPLOG+2                                          07720000
         BNE   *+12                                                     07730000
         OI    OPTIONS,#L          LOG                                  07740000
         B     CTLNEXT                                                  07750000
         B     ILLCARD                                                  07760000
CTL5     CLC   0(5,R1),OPLOG                                            07770000
         BNE   *+12                                                     07780000
         NI    OPTIONS,255-#L      NOLOG                                07790000
         B     CTLNEXT                                                  07800000
         CLC   0(5,R1),OPMSG                                            07810000
         BNE   *+12                                                     07820000
         NI    OPTIONS,255-#M      NOMSG                                07830000
         B     CTLNEXT                                                  07840000
         CLC   0(5,R1),OPFLUSH+2                                        07850000
         BNE   *+12                                                     07860000
         OI    OPTIONS,#F          FLUSH                                07870000
         B     CTLNEXT                                                  07880000
         B     ILLCARD                                                  07890000
CTL7     CLC   0(7,R1),OPFLUSH                                          07900000
         BNE   *+12                                                     07910000
         NI    OPTIONS,255-#F      NOFLUSH                              07920000
         B     CTLNEXT                                                  07930000
         B     ILLCARD                                                  07940000
CTLNEXT  LA    R6,8(,R6)           POINT TO NEXT O.D.E.                 07950000
         TM    6(R6),X'80'         OPERAND PRESENT?                     07960000
         BO    CTLSPEC             YES, BRANCH                          07970000
         B     READ                                                     07980000
         EJECT                                                          07990000
************************************************************            08000000
*                                                          *            08010000
*         REPORT WRITER                                    *            08020000
*                                                          *            08030000
************************************************************            08040000
         SPACE                                                          08050000
REPORT   LA    R1,LINE                                                  08060000
         LA    R0,121                                                   08070000
REPORTW  STM   R14,R2,REPORTS                                           08080000
         LA    R3,PRTDCBW          POINT R3 TO DCB                      08090000
         CP    REPORTLN,REPORTMX   IS LINECOUNT LIMIT REACHED           08100000
         BNH   *+10                NO                                   08110000
         ZAP   REPORTLN,=P'0'      YES, FORCE NEW PAGE                  08120000
         CP    REPORTLN,=P'0'      IS NEW PAGE REQUESTED?               08130000
         BE    REPORTH             YES, GO PRINT HEADING                08140000
REPORTD  CH    R0,=H'121'          IS OUTPUT LINE LENGTH OK?            08150000
         BNL   REPORTP             YES, BRANCH                          08160000
         MVC   REPORTO,REPORTO-1   BLANK THE WORK AREA                  08170000
         LTR   R14,R0              COPY LENGTH                          08180000
         BNP   REPORTB             BLANK LINE IF < 1                    08190000
         BCTR  R14,0               LENGTH MINUS 1                       08200000
         B     *+10                                                     08210000
         MVC   REPORTO(0),0(R1)    COPY OUTPUT LINE                     08220000
         EX    R14,*-6             EXECUTE MVC                          08230000
REPORTB  LA    R1,REPORTO          POINT TO NEW OUTPUT LINE             08240000
REPORTP  LR    R2,R1               POINT R2 TO OUTPUT LINE              08250000
         PUT   (R3),(R2)           WRITE OUTPUT LINE                    08260000
         CLI   0(R2),X'40'         CC = SINGLE SPACING                  08270000
         BE    REPORTC1            YES, BRANCH                          08280000
         CLI   0(R2),C'0'          CC = DOUBLE SPACING                  08290000
         BE    REPORTC2            YES, BRANCH                          08300000
         CLI   0(R2),C'-'          CC = TRIPLE SPACING                  08310000
         BNE   REPORTX             NO, BRANCH                           08320000
         AP    REPORTLN,=P'3'      INCREMENT LINE COUNTER               08330000
         B     REPORTX             EXIT                                 08340000
REPORTC2 AP    REPORTLN,=P'2'      INCREMENT LINE COUNTER               08350000
         B     REPORTX             EXIT                                 08360000
REPORTC1 AP    REPORTLN,=P'1'      INCREMENT LINE COUNTER               08370000
REPORTX  LM    R14,R2,REPORTS      RESTORE REGS                         08380000
         BR    R14                 RETURN                               08390000
REPORTH  AP    REPORTPG,=P'1'      INCREMENT PAGE COUNTER               08400000
         MVC   REPORTO,REPORTO-1   BLANK HEADING                        08410000
         MVI   REPORTO,C'1'        CC = NEW PAGE                        08420000
         MVC   REPORTO+1(L'HEAD1),HEAD1                                 08430000
         LA    R15,REPORTO+1+L'HEAD1                                    08440000
         MVC   0(44,R15),JFCB                                           08450000
         LA    R15,44(,R15)                                             08460000
APPENDV  CLI   0(R15),C' '                                              08470000
         BNE   *+8                                                      08480000
         BCT   R15,APPENDV                                              08490000
         MVC   3(6,R15),JFCB+118  VOLUME                                08500000
         LA    R1,REPORTO+100-9    RIGHT EDGE PAGE NO                   08510000
         MVC   3(6,R1),=X'402020202020' EDIT MASK                       08520000
         ED    3(6,R1),REPORTPG    UNPACK PAGE NO                       08530000
         MVC   0(4,R1),=C'PAGE'    INSERT 'PAGE'                        08540000
         PUT   (R3),REPORTO        PUT HEADING LINE 1                   08550000
         MVC   REPORTO,REPORTO-1   BLANK LINE                           08560000
         PUT   (R3),REPORTO        PUT HEADING LINE 2                   08570000
         LM    R0,R1,REPORTS+8     RESTORE R0 AND R1                    08580000
         B     REPORTD             GO PRINT DETAIL LINE                 08590000
         EJECT                                                          08600000
FLUSH    MVC   LINE,LINE-1                                              08610000
         MVC   MID(L'MSG002),MSG002                                     08620000
         BAL   R14,REPORT                                               08630000
         SPACE                                                          08640000
EODCTL   EQU   *                                                        08650000
EXITRC   LH    R15,RC                                                   08660000
EXIT     LR    R2,R15              SAVE RETURN CODE                     08670000
         LA    R6,CLOSE                                                 08680000
         MVI   0(R6),X'80'                                              08690000
         TM    STATUS,X'20'        IS SYSLIB OPEN?                      08700000
         BZ    EXITC5X             NO, SKIP CLOSE                       08710000
         CLOSE ((R5)),MF=(E,(R6))                                       08720000
EXITC5X  EQU   *                                                        08730000
         TM    STATUS,X'40'        IS SYSIN OPEN?                       08740000
         BZ    EXITC4X             NO, SKIP CLOSE                       08750000
         CLOSE ((R4)),MF=(E,(R6))                                       08760000
EXITC4X  EQU   *                                                        08770000
         TM    STATUS,X'80'        IS SYSPRINT OPEN?                    08780000
         BZ    EXITC3X             NO, SKIP CLOSE                       08790000
         LA    R3,PRTDCBW          POINT R3 TO DCB                      08800000
         CLOSE ((R3)),MF=(E,(R6))                                       08810000
EXITC3X  EQU   *                                                        08820000
         SPACE 1                                                        08830000
         LR    1,13                                                     08840000
         L     R0,@SIZE                                                 08850000
         L     13,4(,13)                                                08860000
         FREEMAIN R,A=(1),LV=(0)                                        08870000
         LR    R15,R2              RESTORE RETURN CODE                  08880000
         LM    0,12,20(13)                                              08890000
         L     14,12(,13)                                               08900000
         BR    14                                                       08910000
         EJECT                                                          08920000
************************************************************            08930000
*                                                          *            08940000
*        CONSTANTS                                         *            08950000
*                                                          *            08960000
************************************************************            08970000
         SPACE                                                          08980000
         LTORG                                                          08990000
OPLOG    DC    C'NOLOG'                                                 09000000
OPMSG    DC    C'NOMSG'                                                 09010000
OPFLUSH  DC    C'NOFLUSH'                                               09020000
         SPACE                                                          09030000
HEAD1    DC    C'PDSPROGM SCRATCH/RENAME UTILITY  -  '                  09040000
         PRINT NOGEN                                                    09050000
         SPACE                                                          09060000
PRTDCB   DCB   DDNAME=SYSPRINT,MACRF=(PM),DSORG=PS,                    +09070000
               RECFM=FBA,LRECL=121                                      09080000
PRTDCBL  EQU   *-PRTDCB                                                 09090000
         SPACE                                                          09100000
CTLDCB   DCB   DDNAME=SYSIN,MACRF=(GM),DSORG=PS,LRECL=80                09110000
CTLDCBL  EQU   *-CTLDCB                                                 09120000
         SPACE                                                          09130000
LIBDCB   DCB   DDNAME=SYSLIB,MACRF=(R,W),DSORG=PO                       09140000
LIBDCBL  EQU   *-LIBDCB                                                 09150000
         SPACE                                                          09160000
         PRINT GEN                                                      09170000
         SPACE                                                          09180000
SRCCAM   CAMLST SEARCH,2,3,4                                            09190000
         SPACE                                                          09200000
MSG002   DC    C'MSG002  *** ALL REMAINING STATEMENTS FLUSHED ***'      09210000
LMODMSG  DC    C'NOT ALIASED. LINKAGE EDITOR SHOULD BE USED TO ALIAS.'  09220000
         SPACE                                                          09230000
ALPHANUM DC    0D'0',64X'FF',X'00',26X'FF'                              09240000
*                            SPACE                                      09250000
         DC    X'00',4X'FF',X'00',26X'FF',2X'00',67X'FF',10X'00'        09260000
*                $            -              #@          X'C0' A-I      09270000
         DC    7X'FF',9X'00',8X'FF',8X'00',6X'FF',10X'00',6X'FF'        09280000
*                       J-R            S-Z            0-9               09290000
         SPACE                                                          09300000
*              THE ABOVE TABLE CAN BE USED TO TEST FOR ALPHANUMERIC     09310000
*              (PLUS NATIONAL AND BLANK AND HYPHEN AND X'C0')           09320000
*              WITH A 'TRT' INSTRUCTION.                                09330000
*              IF THE FIELD IS VALID, CONDITION CODE IS 0.              09340000
*              WARNING: 'TRT' CAN CHANGE THE LOW ORDER 8 BITS           09350000
*              OF REGISTER 2 AND LOW ORDER 24 BITS OF REG 1.            09360000
         SPACE                                                          09370000
TABNONBL DC    64X'FF'                                                  09380000
         DC    X'00'               BLANK                                09390000
         DC    42X'FF'                                                  09400000
         DC    X'00'               COMMA                                09410000
         DC    148X'FF'                                                 09420000
TABBLANK DC    64X'00'                                                  09430000
         DC    X'40'               BLANK                                09440000
         DC    42X'00'                                                  09450000
         DC    X'6B'               COMMA                                09460000
         DC    148X'00'                                                 09470000
         SPACE                                                          09480000
EODAD    EQU   32                  OFFSET INTO DCB                      09490000
RECFM    EQU   36                  OFFSET INTO DCB                      09500000
EXLST    EQU   36                  OFFSET INTO DCB                      09510000
OFLGS    EQU   48                  OFFSET INTO DCB                      09520000
DDNAM    EQU   40                  OFFSET INTO DCB                      09530000
BLKSI    EQU   62                  OFFSET INTO DCB                      09540000
LRECL    EQU   82                  OFFSET INTO DCB                      09550000
         SPACE                                                          09560000
#C       EQU   X'80'                                                    09570000
#R       EQU   X'20'                                                    09580000
#F       EQU   X'10'                                                    09590000
#L       EQU   X'08'                                                    09600000
#M       EQU   X'04'                                                    09610000
#DEFAULT EQU   #C+#R+#F+#L+#M      CAPS,NOEND,RES,FLUSH,LOG,MSG         09620000
         SPACE                                                          09630000
************************************************************            09640000
*                                                          *            09650000
*        DSECTS                                            *            09660000
*                                                          *            09670000
************************************************************            09680000
         SPACE                                                          09690000
@DATA    DSECT                                                          09700000
         DS    18F                 REGISTER SAVEAREA                    09710000
WORK     DS    0F                                                       09720000
DOUBLE   DS    D                                                        09730000
OPEN     DS    F                                                        09740000
CLOSE    EQU   OPEN                                                     09750000
COUNTIN  DS    F                                                        09760000
OPTIONS  DS    H                                                        09770000
RC       DS    H                                                        09780000
PRTDCBW  DS    0F,(PRTDCBL)X                                            09790000
CTLDCBW  DS    0F,(CTLDCBL)X                                            09800000
LIBDCBW  DS    0F,(LIBDCBL)X                                            09810000
LIBEXLST DS    F                                                        09820000
PRTEXLST DS    F                                                        09830000
JFCB     DS    196X                                                     09840000
INREC    DS    0D,CL80                                                  09850000
ODL      DS    0F                  OPERAND DESCRIPTOR LIST              09860000
ODE1     DS    2F                  OPERAND DESCRIPTOR ENTRY 1           09870000
ODE2     DS    2F                  OPERAND DESCRIPTOR ENTRY 2           09880000
ODE3     DS    2F                  OPERAND DESCRIPTOR ENTRY 3           09890000
ODE4     DS    2F                  OPERAND DESCRIPTOR ENTRY 4           09900000
ODE5     DS    2F                  OPERAND DESCRIPTOR ENTRY 5           09910000
ODE6     DS    2F                  OPERAND DESCRIPTOR ENTRY 6           09920000
ODLL     EQU   *-ODL                                                    09930000
TYPE     DS    C                                                        09940000
STATUS   DS    C                                                        09950000
         DS    C                   LINE-1                               09960000
LINE     DS    CL133                                                    09970000
LINEH1   DS    CL133                                                    09980000
MID      EQU   LINE+1,6                                                 09990000
TXT      EQU   LINE+9                                                   10000000
MBR      EQU   LINE+16,8                                                10010000
RESULT   EQU   LINE+25                                                  10020000
OBTAIN14 DS    F                                                        10030000
CAMLST   DS    4F                                                       10040000
MEMBER   DS    D                                                        10050000
NEWMEM   DS    D                                                        10060000
DSNAME   DS    CL44                                                     10070000
VOLUME   DS    CL6                                                      10080000
BLDL     DS    0D,CL80                                                  10090000
UPPER    DS    CL72                                                     10100000
REPORTS  DS    6F                  REGISTER SAVE AREA                   10110000
REPORTPG DS    PL3                 PAGE COUNT, INIT P'0'                10120000
REPORTLN DS    PL2                 LINE COUNT, INIT P'0'                10130000
REPORTMX DS    PL2                 LINES/PAGE, INIT P'50'               10140000
REPORTOB DS    CL1                 REPORTO-1 (INIT BLANK)               10150000
REPORTO  DS    CL133               OUTPUT AREA                          10160000
@DATAL   EQU   *-@DATA                                                  10170000
         SPACE                                                          10180000
R0       EQU   0                                                        10190000
R1       EQU   1                                                        10200000
R2       EQU   2                                                        10210000
R3       EQU   3                                                        10220000
R4       EQU   4                                                        10230000
R5       EQU   5                                                        10240000
R6       EQU   6                                                        10250000
R7       EQU   7                                                        10260000
R8       EQU   8                                                        10270000
R9       EQU   9                                                        10280000
R10      EQU   10                                                       10290000
R11      EQU   11                                                       10300000
R12      EQU   12                                                       10310000
R13      EQU   13                                                       10320000
R14      EQU   14                                                       10330000
R15      EQU   15                                                       10340000
         END                                                            10350000
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             00080000
//LKED.SYSIN DD *                                                       00090000
 NAME PDSPROGM(R)                                                       00100000
//                                                                      00110000
