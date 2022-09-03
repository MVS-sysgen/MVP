//INSTREAM  JOB (TSO),
//             'Install INSTREAM',
//             CLASS=A,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1),
//             USER=IBMUSER,
//             PASSWORD=SYS1
//*
//*                                                                     00020000
//* ********************************************************            00030000
//* *  INSTALL THE 'INSTREAM' TSO COMMAND                  *            00040000
//* ********************************************************            00050000
//*                                                                     00060000
//INSTREAM EXEC ASMFCL,COND=(0,NE),                                     00070000
//         PARM.ASM='LOAD,NODECK,LIST',                                 00080000
//         PARM.LKED='LIST,MAP,RENT,REUS,REFR'                          00090000
//ASM.SYSIN DD *                                                        00100000
         TITLE '   I N S T R E A M '                                    00010000
************************************************************            00020000
*                                                          *            00030000
*        'INSTREAM'                                        *            00040000
*                                                          *            00050000
************************************************************            00060000
         SPACE                                                          00070000
*        WRITTEN BY. BILL GODFREY, PLANNING RESEARCH CORPORATION.       00080000
*        INSTALLATION. PRC COMPUTER CENTER INC, MCLEAN VA.              00090000
*        DATE WRITTEN. JULY 22 1976.                                    00100000
*        DATE UPDATED. JUNE 17 1980.                                    00110001
*        ATTRIBUTES. RE-ENTRANT.                                        00120000
*        COMMENTS.                                                      00130000
*            THIS TSO COMMAND COPIES THE INPUT STREAM FOLLOWING IT      00140000
*            INTO A DATASET. THE USER IS PLACED IN AN 'INSTREAM'        00150000
*            MODE DURING WHICH EVERYTHING ENTERED IS COPIED TO A        00160000
*            DATASET, UNTIL 'END' (OR A SPECIFIED SUBSTITUTE) IS        00170000
*            ENCOUNTERED.                                               00180000
*                                                                       00190000
*            NOTE: ALLOCATION UNITNAME 'SYSDA' IS HARD-CODED            00200000
*            IN THIS COMMAND FOR THE TEMPORARY DATA SET.  THIS MAY      00210000
*            BE CHANGED TO A VIO UNITNAME AT ANOTHER INSTALLATION.      00220000
*                                                                       00230000
*            SYNTAX -                                                   00240000
*                 INSTREAM FILENAME('DDNAME')                           00250000
*                          MODE('MODE') END('END')  INSERT  CONTIN      00260001
*                                                                       00270000
*            DEFAULTS -                                                 00280000
*                    NOINSERT                                           00290001
*                    MODE  -  'INSTREAM'                                00300000
*                    END   -  'END'                                     00310000
*                                                                       00320000
*            THIS TSO COMMAND MAKES IT EASY TO PUT CONTROL CARDS        00330000
*            INTO CLISTS. FOR EXAMPLE, A CLIST THAT USES THE            00340000
*            IEBPTPCH UTILITY MIGHT LOOK LIKE THIS:                     00350000
*                                                                       00360000
*                 PROC 1 DSNAME PREFORM(A)                              00370000
*                 FREE FI(SYSIN SYSUT1 SYSUT2)                          00380000
*                 ALLOC FI(SYSUT1) DA(&DSNAME) SHR                      00390000
*                 ALLOC FI(SYSUT2) SYSOUT(A)                            00400000
*                 INSTREAM                                              00410000
*                 LABEL PRINT PREFORM=&PREFORM                          00420000
*                 END                                                   00430000
*                 CALL 'SYS1.LINKLIB(IEBPTPCH)'                         00440000
*                                                                       00450000
*            NOTICE THAT THE CONTENTS OF THE CONTROL STATEMENT          00460001
*            CAN BE SET BY VARIABLES, SUCH AS &PREFORM IN THE           00470001
*            EXAMPLE ABOVE.                                             00480000
*                                                                       00490001
*            SINCE CLIST STATEMENTS ARE LEFT JUSTIFIED AT THE           00500001
*            TIME THEY ARE EXECUTED, GETTING LEADING BLANKS             00510001
*            INTO A STATEMENT CAN BE A PROBLEM, BUT NOT IF YOU          00520001
*            USE THE 'INSERT' KEYWORD.  THIS KEYWORD TELLS THE          00530001
*            COMMAND THAT IF ANY INPUT STATEMENT BEGINS WITH            00540001
*            THE CHARACTERS 'I' OR 'IN' OR 'INSERT' FOLLOWED            00550001
*            BY A BLANK, THEN THE CHARACTERS AND THE BLANK WILL         00560001
*            BE IGNORED AND THE REMAINDER OF THE STATEMENT WILL         00570001
*            BE TREATED AS THE DATA.  THIS GIVES YOU THE FREEDOM        00580001
*            TO INDENT YOUR CLIST STATEMENTS.                           00590001
*                                                                       00600001
*            THE 'CONTIN' KEYWORD CAUSES THE FIRST BYTE OF DATA         00610001
*            TO BE PUT IN COLUMN 72, AND THE REST IN COLUMNS 1-71.      00620001
*            THIS LETS YOU CODE CONTINUATION CARDS WITHOUT HAVING       00630001
*            TO COUNT COLUMNS.                                          00640001
*                                                                       00650000
*            LOG OF CHANGES.                                            00660000
*            25AUG76 - DAIRFAIL ROUTINE ADDED.                          00670000
*            10DEC76 - DA08PSWD SET TO BLANKS. BINARY ZEROES CAUSE      00680000
*                      PASSWORD DATASET SEARCH DURING DAIR.             00690000
*            10DEC76 - 'SPACE' KEYWORD ADDED.                           00700000
*                      IN CLISTS UNDER VS2, LEADING BLANKS ARE          00710000
*                      REMOVED (UNLESS IN A 'DATA-ENDDATA' AREA)        00720000
*                      SO THIS KEYWORD WILL DEFINE A CHARACTER          00730000
*                      THAT WILL BE CHANGED TO LEADING BLANKS.          00740000
*            11MAR77 - 'CONTIN', 'BLKSIZE', AND 'ASIS' KEYWORDS.        00750000
*            20SEP79 - GBLB ADDED FOR NON MVS SITES.                    00760000
*                    - 'REUSE' KEYWORD ADDED.                           00770000
*            24MAR80 - TEST FOR ASIS FIXED. DONT CONVERT 1ST 4 BYTES.   00780000
*            23MAY80 - VIO CHANGED TO SYSDA.                            00790000
*            17JUN80 - SPACE KW REPLACED WITH INSERT KW                 00800001
         SPACE                                                          00810000
         GBLB  &MVS                                                     00820000
&MVS     SETB  1                   1 - MVS   0 - SVS,MVT                00830000
         SPACE                                                          00840000
INSTREAM START                                                          00850000
         USING *,R12                                                    00860000
         B     @PROLOG-*(,15)                                           00870000
         DC    AL1(11),CL11'INSTREAM'                                   00880000
         DC    CL16' &SYSDATE &SYSTIME '                                00890000
@SIZE    DC    0F'0',AL1(1),AL3(@DATAL)                                 00900000
@PROLOG  STM   14,12,12(13)                                             00910000
         LR    R12,R15                                                  00920000
         LR    R2,R1                                                    00930000
         USING CPPL,R2                                                  00940000
         L     R0,@SIZE                                                 00950000
         GETMAIN R,LV=(0)                                               00960000
         ST    13,4(,1)                                                 00970000
         ST    1,8(,13)                                                 00980000
         LR    13,1                                                     00990000
         USING @DATA,13                                                 01000000
         SPACE                                                          01010000
         SLR   R15,R15                                                  01020000
         ST    R15,MYECB                                                01030000
         ST    R15,OPTIONS                                              01040000
         ST    R15,LINKAREA+4                                           01050000
         STH   R15,CC                                                   01060000
         MVI   SPACES,C' '                                              01070000
         MVC   SPACES+1(L'SPACES-1),SPACES                              01080000
         LA    R0,400              DEFAULT BLOCK SIZE                   01090000
         STH   R0,BLOKSIZE                                              01100000
         SPACE                                                          01110000
************************************************************            01120000
*                                                          *            01130000
*        SET UP IOPL FOR SERVICE ROUTINES                  *            01140000
*                                                          *            01150000
************************************************************            01160000
         SPACE                                                          01170000
         LA    R15,MYIOPL          IOPL FOR 'PUTLINE' AND 'PUTGET'      01180000
         USING IOPL,R15                                                 01190000
*        MVC   IOPLUPT(4),CPPLUPT                                       01200000
         L     R14,CPPLUPT                                              01210000
         MVC   MYUPT(24),0(R14) FAKE UPT                                01220000
         LA    R14,MYUPT                                                01230000
         USING UPT,R14                                                  01240000
         OI    UPTSWS,UPTMODE    SET FAKE PROFILE MODE                  01250000
         DROP  R14                                                      01260000
         ST    R14,IOPLUPT                                              01270000
         MVC   IOPLECT(4),CPPLECT                                       01280000
         LA    R0,MYATTN                                                01290000
         ST    R0,IOPLECB                                               01300000
         LA    R0,MYPGPB                                                01310000
         ST    R0,IOPLIOPB                                              01320000
         DROP  R15                                                      01330000
         EJECT                                                          01340000
************************************************************            01350000
*                                                          *            01360000
*        CALL THE PARSE SERVICE ROUTINE                    *            01370000
*                                                          *            01380000
************************************************************            01390000
         SPACE                                                          01400000
         LA    R15,MYPPL                                                01410000
         USING PPL,R15                                                  01420000
         MVC   PPLUPT(4),CPPLUPT                                        01430000
         MVC   PPLECT(4),CPPLECT                                        01440000
         LA    R0,MYECB                                                 01450000
         ST    R0,PPLECB                                                01460000
         XC    MYECB,MYECB                                              01470000
         L     R0,=A(INSPCL)                                            01480000
         ST    R0,PPLPCL                                                01490000
         LA    R0,MYANS                                                 01500000
         ST    R0,PPLANS                                                01510000
         MVC   PPLCBUF(4),CPPLCBUF                                      01520000
         LA    R0,MYUWA                                                 01530000
         ST    R0,PPLUWA                                                01540000
         DROP  R15                                                      01550000
         SPACE 1                                                        01560000
************************************************************            01570000
*                                                          *            01580000
*        CALL THE PARSE SERVICE ROUTINE                    *            01590000
*                                                          *            01600000
************************************************************            01610000
         SPACE 1                                                        01620000
         LR    R1,R15              POINT TO PPL                         01630000
         AIF   (NOT &MVS).SKIP1                                         01640000
         L     R15,16              CVTPTR                               01650000
         TM    X'020C'(R15),X'80'  IF HI ORDER BIT NOT ON               01660000
         BNO   PARSELNK               THEN DO LINK, NOT CALL            01670000
         L     R15,X'020C'(,R15)   CVTPARS                              01680000
         BALR  R14,R15             CALL IKJPARS                         01690000
         B     PARSEEXT            SKIP AROUND LINK                     01700000
PARSELNK EQU   *                                                        01710000
.SKIP1   ANOP                                                           01720000
         LINK  EP=IKJPARS,SF=(E,LINKAREA)                               01730000
PARSEEXT EQU   *                                                        01740000
         SPACE 1                                                        01750000
         LTR   R15,R15                                                  01760000
         BNZ   PARSERR                                                  01770000
         SPACE                                                          01780000
         L     R9,MYANS                                                 01790000
         USING IKJPARMD,R9                                              01800000
         SPACE                                                          01810000
         CLI   CONTKW+1,0          'CONTINUE' SPECIFIED?                01820000
         BE    *+8                                                      01830000
         OI    OPTIONS,X'80'       YES - SET OPTION BIT                 01840000
         SPACE                                                          01850000
         CLI   ASISKW+1,0          'ASIS' SPECIFIED?                    01860000
         BE    *+8                 NO, BRANCH                           01870000
         OI    OPTIONS,X'40'       YES - SET ASIS BIT                   01880000
         SPACE                                                          01890000
         TM    BLOC+6,X'80'        'BLKSIZE' SPECIFIED?                 01900000
         BZ    NOBLOC              NO - BRANCH                          01910000
         LH    R14,BLOC+4          GET LENGTH                           01920000
         LTR   R14,R14                                                  01930000
         BZ    NOBLOC                                                   01940000
         L     R1,BLOC                                                  01950000
         BCTR  R14,0                                                    01960000
         B     *+10                                                     01970000
         PACK  DOUBLE(8),0(0,R1)                                        01980000
         EX    R14,*-6                                                  01990000
         CVB   R1,DOUBLE                                                02000000
         SLR   R0,R0               CLEAR FOR DIVIDE                     02010000
         LTR   R1,R1               'BLKSIZE(0)' SPECIFIED?              02020000
         BZ    NOBLOC              YES - IGNORE IT                      02030000
         LR    R15,R1              HOLD IN R15                          02040000
         LA    R14,80                                                   02050000
         DR    R0,R14              DIVIDE BY 80                         02060000
         LTR   R0,R0               ANY REMAINDER?                       02070000
         BNZ   NOBLOC              YES - IGNORE                         02080000
         STH   R15,BLOKSIZE                                             02090000
NOBLOC   EQU   *                                                        02100000
         MVI   INSERTSW,0                                               02110001
         CLI   INSERTKW+1,1        INSERT                               02120001
         BNE   NOIN                NO - BRANCH                          02130004
         MVI   INSERTSW,C'I'       SET INSERTSW                         02140001
NOIN     EQU   *                                                        02150001
         SPACE                                                          02160000
         MVC   ENDSTMT+1(8),=CL8'END'                                   02170000
         MVI   ENDSTMT,3-1                                              02180000
         TM    END+6,X'80'      END OVERRIDE?                           02190000
         BZ    NOEND            NO - BRANCH                             02200000
         L     R1,END           R1 --> END STMT                         02210000
         LH    R14,END+4        R14 =  LENGTH                           02220000
         BCTR  R14,0                                                    02230000
         MVC   ENDSTMT+1(8),BLANKS                                      02240000
         B     *+10                                                     02250000
         MVC   ENDSTMT+1(0),0(R1) MOVE VALUE IN                         02260000
         EX    R14,*-6                                                  02270000
         STC   R14,ENDSTMT                                              02280000
NOEND    EQU   *                                                        02290000
         SPACE                                                          02300000
         MVC   MYFILEN(8),=CL8'SYSIN'                                   02310000
         CLI   FILEKW+1,1          FILENAME SPECIFIED?                  02320000
         BNE   NOFILE                                                   02330000
         TM    FILES+6,X'80'       FILENAME VALUE SPEC?                 02340000
         BZ    NOFILE                                                   02350000
         L     R14,FILES                                                02360000
         MVC   MYFILEN(8),BLANKS                                        02370000
         LH    R1,FILES+4          GET LENGTH                           02380000
         BCTR  R1,0                                                     02390000
         B     *+10                                                     02400000
         MVC   MYFILEN(0),0(R14)                                        02410000
         EX    R1,*-6                                                   02420000
NOFILE   EQU   *                                                        02430000
         MVC   DSNAME(9),=CL9'&&INSTREAM'                               02440000
         LA    R14,9                                                    02450000
         STH   R14,DSNAMEL                                              02460000
         EJECT                                                          02470000
************************************************************            02480000
*                                                          *            02490000
*        ALLOCATE THE OUTPUT DATASET                       *            02500000
*                                                          *            02510000
************************************************************            02520000
         SPACE                                                          02530000
         LA    R1,MYDAPL                                                02540000
         USING DAPL,R1                                                  02550000
         MVC   DAPLUPT(4),CPPLUPT                                       02560000
         MVC   DAPLECT(4),CPPLECT                                       02570000
         LA    R0,MYECB                                                 02580000
         ST    R0,DAPLECB                                               02590000
         MVC   DAPLPSCB(4),CPPLPSCB                                     02600000
         LA    R15,MYDAPB                                               02610000
         ST    R15,DAPLDAPB                                             02620000
         SPACE                                                          02630000
         CLI   REUSKW+1,1          'REUSE' KW SPECIFIED?                02640000
         BNE   NOFREE              NO, BRANCH                           02650000
         USING DAPB18,R15                                               02660000
         XC    0(40,R15),0(R15)                                         02670000
         MVI   DA18CD+1,X'18'                                           02680000
         MVC   DA18DDN,MYFILEN                                          02690000
         MVC   DA18MNM(10),BLANKS                                       02700000
         OI    DA18CTL,X'10'       FREE EVEN IF PERM                    02710000
         DROP  R15                 DAPB18                               02720000
         BAL   R14,CALLDAIR        UNALLOCATE THE DDNAME                02730000
NOFREE   EQU   *                                                        02740000
         SPACE                                                          02750000
         LA    R15,MYDAPB                                               02760000
         USING DAPB08,R15                                               02770000
         XC    0(84,R15),0(R15)                                         02780000
         MVI   DA08CD+1,X'08'                                           02790000
         LA    R0,DSNAMEL                                               02800000
         ST    R0,DA08PDSN                                              02810000
         MVC   DA08DDN(8),MYFILEN                                       02820000
         MVC   DA08UNIT,@UNITVIO                                        02830000
         MVC   DA08SER,BLANKS                                           02840000
         LA    R0,1                1 TRACK                              02850000
         ST    R0,DA08PQTY                                              02860000
         MVC   DA08MNM,BLANKS                                           02870000
         MVC   DA08PSWD,BLANKS                                          02880000
         MVI   DA08DSP1,DA08NEW                                         02890000
         MVI   DA08DPS2,DA08DEL                                         02900000
         MVI   DA08DPS3,DA08DELE                                        02910000
         MVI   DA08CTL,DA08TRKS+DA08PERM                                02920000
         DROP  R15                 DAPB08                               02930000
         SPACE                                                          02940000
         BAL   R14,CALLDAIR                                             02950000
         LTR   R15,R15                                                  02960000
         BNZ   DAIRERR                                                  02970000
         SPACE                                                          02980000
************************************************************            02990000
*                                                          *            03000000
*        SET UP THE DCB                                    *            03010000
*                                                          *            03020000
************************************************************            03030000
         SPACE                                                          03040000
OPENIT   MVC   DCB(DCBLEN),DCBMODEL                                     03050000
         LA    R15,MYDAPB                                               03060000
         USING DAPB08,R15                                               03070000
         LA    R3,DCB                                                   03080000
         USING IHADCB,R3                                                03090000
         MVC   DCBDDNAM(8),DA08DDN                                      03100000
         MVC   DCBBLKSI,BLOKSIZE                                        03110000
         MVC   OPEND(4),OPEN                                            03120000
         DROP  R15                                                      03130000
        SPACE                                                           03140000
         OPEN  ((R3),OUTPUT),MF=(E,OPEND)                               03150000
        SPACE                                                           03160000
         TM    DCBOFLGS,X'10'                                           03170000
         BZ    OPENERR                                                  03180000
         EJECT                                                          03190000
************************************************************            03200000
*                                                          *            03210000
*        SET UP MODE MESSAGE                               *            03220000
*                                                          *            03230000
************************************************************            03240000
         SPACE                                                          03250000
         LA    R14,1                                                    03260000
         ST    R14,MYMODEMG                                             03270000
         LA    R14,MYMODEMT                                             03280000
         ST    R14,MYMODEMG+4    O.L.D.                                 03290000
         TM    RDY+6,X'80'   READY OVERRIDE?                            03300000
         BO    MODEMOD                                                  03310000
         MVC   MYMODEMT(16),READYMSG                                    03320000
         B     MODEMGX                                                  03330000
MODEMOD  XC    MYMODEMT(16),MYMODEMT                                    03340000
         MVI   MYMODEMT+4,X'40'                                         03350000
         MVC   MYMODEMT+5(11),MYMODEMT+4                                03360000
         LH    R1,RDY+4                                                 03370000
         LA    R1,4+1(,R1)         ADD PREFIX AND 1 BLANK               03380000
         STH   R1,MYMODEMT                                              03390000
         SH    R1,=H'6'                                                 03400000
         L     R14,RDY                                                  03410000
         B     *+10                                                     03420000
         MVC   MYMODEMT+5(0),0(R14)                                     03430000
         EX    R1,*-6                                                   03440000
MODEMGX  EQU   *                                                        03450000
         SPACE                                                          03460000
************************************************************            03470000
*                                                          *            03480000
*        ISSUE MODE MESSAGE AND WAIT FOR INPUT             *            03490000
*                                                          *            03500000
************************************************************            03510000
         SPACE                                                          03520000
MODE     EQU   *                                                        03530000
         BAL   R14,PUTGET                                               03540000
         SPACE                                                          03550000
*        PUTGET WILL EITHER PUT THE ADDRESS OF THE NEXT                 03560000
*        COMMAND INPUT BUFFER IN REG 1 OR, IF THE USER HIT              03570000
*        ATTENTION, PUTGET WILL SET A RETURN CODE OF 8 IN               03580000
*        REGISTER 15 AND THE ATTENTION EXIT WILL PLACE THE              03590000
*        ADDRESS OF THE COMMAND INPUT BUFFER IN 'MYNEXBUF'.             03600000
*        PUTGET WILL SET RC 8 ON ATTENTIONS ONLY IF THE                 03610000
*        ECB IN ITS IOPL IS THE SAME ONE POSTED BY THE                  03620000
*        ATTENTION EXIT.                                                03630000
         SPACE                                                          03640000
         CH    R15,=H'8'           ATTENTION HIT?                       03650000
         BE    POSTMODE            YES - BRANCH                         03660000
         ST    R1,MYNEXBUF                                              03670000
         SPACE                                                          03680000
POSTMODE L     R1,MYNEXBUF                                              03690000
         SPACE                                                          03700000
         LH    R0,0(R1)                                                 03710000
         SH    R0,=H'4'                                                 03720000
         LTR   R0,R0               IF BUFFER LENGTH 0, RETRY            03730000
         BNP   MODEF                                                    03740000
         SPACE                                                          03750000
************************************************************            03760000
*                                                          *            03770000
*        IF NOT 'ASIS', TRANSLATE TO UPPER CASE            *            03780000
*                                                          *            03790000
************************************************************            03800000
         SPACE                                                          03810000
         TM    OPTIONS,X'40'       WAS 'ASIS' SPECIFIED                 03820000
         BO    NOCAPS              YES, LEAVE ASIS                      03830000
         LR    R14,R0                                                   03840000
         BCTR  R14,0                                                    03850000
         B     *+10                                                     03860000
         OC    4(0,R1),SPACES                                           03870000
         EX    R14,*-6                                                  03880000
NOCAPS   EQU   *                                                        03890000
         EJECT                                                          03900000
************************************************************            03910000
*                                                          *            03920000
*        CHECK FOR 'END'                                   *            03930000
*                                                          *            03940000
************************************************************            03950000
         SPACE                                                          03960000
         BCTR  R0,0                                                     03970000
         SLR   R14,R14                                                  03980000
         IC    R14,ENDSTMT                                              03990000
         CR    R0,R14              LINE LONG ENOUGH TO CONTAIN 'END'?   04000000
         BL    NOTEND              NO - NOT END                         04010000
         B     *+10                                                     04020000
         CLC   4(0,R1),ENDSTMT+1                                        04030000
         EX    R14,*-6                                                  04040000
         BE    EXITC                                                    04050000
NOTEND   EQU   *                                                        04060000
         SPACE                                                          04070000
************************************************************            04080000
*                                                          *            04090000
*        MOVE THE GETLINE BUFFER TO OUTPUT AREA            *            04100000
*                                                          *            04110000
************************************************************            04120000
         SPACE                                                          04130000
         LR    R14,R0                                                   04140000
         CH    R14,=H'71'                                               04150000
         BNH   *+8                                                      04160000
         LA    R14,71                                                   04170000
         MVI   CARD,C' '                                                04180000
         MVC   CARD+1(79),CARD                                          04190000
         B     *+10                                                     04200000
         MVC   CARD(0),4(R1)                                            04210000
         EX    R14,*-6                                                  04220000
         SPACE                                                          04230000
************************************************************            04240000
*                                                          *            04250000
*        CHECK FOR INSERT AND REMOVE IT                    *            04260001
*                                                          *            04270000
************************************************************            04280000
         SPACE                                                          04290000
         CLI   INSERTSW,0          IS INSERT ACTIVE?                    04300001
         BE    INSERTX             NO - BRANCH                          04310001
         CLC   CARD(2),=C'I '      IS THIS AN INSERT                    04320001
         BNE   INSERT2             NO - BRANCH                          04330001
         MVC   CARD(78),CARD+2                                          04340001
         MVC   CARD+78(2),SPACES                                        04350001
         B     INSERTX                                                  04360001
INSERT2  CLC   CARD(3),=C'IN '     IS THIS AN INSERT                    04370001
         BNE   INSERT6                                                  04380001
         MVC   CARD(77),CARD+3                                          04390001
         MVC   CARD+77(3),SPACES                                        04400001
         B     INSERTX                                                  04410001
INSERT6  CLC   CARD(7),=C'INSERT ' IS THIS AN INSERT                    04420001
         BNE   INSERTX                                                  04430002
         MVC   CARD(73),CARD+7                                          04440001
         MVC   CARD+73(7),SPACES                                        04450001
INSERTX  EQU   *                                                        04460001
         SPACE                                                          04470000
************************************************************            04480000
*                                                          *            04490000
*        IF 'CONTIN' SPECIFIED, MOVE COL 1 TO COL 72       *            04500000
*                                                          *            04510000
************************************************************            04520000
         SPACE                                                          04530000
         TM    OPTIONS,X'80'                                            04540000
         BZ    NOCONT                                                   04550000
         IC    R14,CARD                                                 04560000
         MVC   CARD(71),CARD+1                                          04570000
         STC   R14,CARD+71                                              04580000
NOCONT   EQU   *                                                        04590000
         SPACE                                                          04600000
************************************************************            04610000
*                                                          *            04620000
*        WRITE THE RECORD                                  *            04630000
*                                                          *            04640000
************************************************************            04650000
         SPACE                                                          04660000
         PUT   (R3),CARD                                                04670000
         SPACE                                                          04680000
MODEF    L     R1,MYNEXBUF  POINT TO INPUT BUFFER                       04690000
         LH    R0,0(,R1)                                                04700000
         O     R0,=X'01000000' SUBPOOL 1                                04710000
         FREEMAIN R,LV=(0),A=(1)                                        04720000
         SPACE                                                          04730000
         XC    MYNEXBUF,MYNEXBUF                                        04740000
         B     MODE                                                     04750000
         EJECT                                                          04760000
************************************************************            04770000
*                                                          *            04780000
*        ERROR MESSAGES                                    *            04790000
*                                                          *            04800000
************************************************************            04810000
PARSERR  LA    R1,PARSERRM                                              04820000
         LA    R0,L'PARSERRM                                            04830000
         BAL   R14,PUTLINE                                              04840000
         B     EXITR                                                    04850000
DAIRERR  EQU   *                                                        04860000
*        CH    R15,=H'20'          DDNAME ALREADY IN USE?               04870000
*        BE    DAIRRC20            YES - WE'LL HANDLE THIS ONE          04880000
         BAL   R14,DAIRFAIL        ISSUE MESSAGE                        04890000
         LA    R15,12              RETURN CODE 12                       04900000
         STH   R15,CC                                                   04910000
         B     EXITR                                                    04920000
OPENERR  LA    R1,OPENERRM                                              04930000
         LA    R0,L'OPENERRM                                            04940000
         BAL   R14,PUTLINE                                              04950000
         B     EXITC                                                    04960000
PARSERRM DC    C'PARSE ERROR'                                           04970000
DAIRERRM DC    C'UNABLE TO ALLOCATE DATASET'                            04980000
OPENERRM DC    C'UNABLE TO OPEN DATASET'                                04990000
READYMSG DC    AL2(4+1+8,0),CL12' INSTREAM'                             05000000
         DC    0H'0'                                                    05010000
         EJECT                                                          05020000
************************************************************            05030000
*                                                          *            05040000
*        PUTLINE                                           *            05050000
*                                                          *            05060000
************************************************************            05070000
         SPACE                                                          05080000
PUTLINE  ST    R14,PUTLINS                                              05090000
         XC    MYOLD(8),MYOLD                                           05100000
         XC    MYSEG1(4),MYSEG1                                         05110000
         MVC   MYPTPB(12),MODLPTPB                                      05120000
         LA    R14,1               NO. OF LEVEL 1 SEGMENTS              05130000
         ST    R14,MYOLD                                                05140000
         LA    R14,MYSEG1          POINT TO LEV 1 SEGMENT               05150000
         ST    R14,MYOLD+4                                              05160000
         LR    R14,0               LENGTH                               05170000
         LA    R14,5(,R14)         ADD 4 PLUS 1 BLANK                   05180000
         STH   R14,MYSEG1                                               05190000
         MVI   MYSEG1+4,C' '                                            05200000
         LR    R14,0                                                    05210000
         BCTR  R14,0                                                    05220000
         B     *+10                                                     05230000
         MVC   MYSEG1+5(0),0(R1) MOVE MESSAGE IN                        05240000
         EX    R14,*-6                                                  05250000
         SPACE                                                          05260000
         PUTLINE PARM=MYPTPB,OUTPUT=(MYOLD),MF=(E,MYIOPL)               05270000
         SPACE                                                          05280000
         L     R14,PUTLINS                                              05290000
         BR    R14                                                      05300000
         EJECT                                                          05310000
************************************************************            05320000
*                                                          *            05330000
*        PUTGET                                            *            05340000
*                                                          *            05350000
************************************************************            05360000
PUTGET   ST    R14,PUTLINS                                              05370000
         MVC   MYPGPB(16),MODLPGPB                                      05380000
         SPACE                                                          05390000
         PUTGET PARM=MYPGPB,OUTPUT=(MYMODEMG,SINGLE,MODE),MF=(E,MYIOPL) 05400000
         SPACE                                                          05410000
         L     R1,MYPGPB+12       R1 --> INPUT BUFFER                   05420000
         L     R14,PUTLINS                                              05430000
         BR    R14                                                      05440000
         EJECT                                                          05450000
************************************************************            05460000
*                                                          *            05470000
*        CALL IKJDAIR SERVICE ROUTINE                      *            05480000
*                                                          *            05490000
************************************************************            05500000
         SPACE                                                          05510000
CALLDAIR ST    R14,DAIRREGS                                             05520000
         AIF   (NOT &MVS).SKIP2                                         05530000
         L     R15,16                                                   05540000
         TM    X'2DC'(R15),X'80'   CVTDAIR                              05550000
         BNO   DAIRLINK                                                 05560000
         L     R15,X'2DC'(,R15)                                         05570000
         BALR  R14,R15                                                  05580000
         B     DAIRFINI                                                 05590000
DAIRLINK EQU   *                                                        05600000
.SKIP2   ANOP                                                           05610000
         LINK  EP=IKJDAIR,SF=(E,LINKAREA)                               05620000
DAIRFINI L     R14,DAIRREGS                                             05630000
         BR    R14                                                      05640000
         SPACE                                                          05650000
************************************************************            05660000
*                                                          *            05670000
*        DYNAMIC ALLOCATION FAILURE ROUTINE                *            05680000
*                                                          *            05690000
************************************************************            05700000
         SPACE                                                          05710000
DAIRFAIL ST    R14,MYDFREGS                                             05720000
         AIF   (NOT &MVS).SKIP3                                         05730000
         LA    R1,MYDFPARM                                              05740000
         USING DFDSECTD,R1                                              05750000
         ST    R15,MYDFRC                                               05760000
         LA    R15,MYDFRC                                               05770000
         ST    R15,DFRCP                                                05780000
         LA    R15,MYDAPL                                               05790000
         ST    R15,DFDAPLP                                              05800000
         SLR   R15,R15                                                  05810000
         ST    R15,MYJEFF02                                             05820000
         LA    R15,MYJEFF02                                             05830000
         ST    R15,DFJEFF02                                             05840000
         LA    R15,DFDAIR                                               05850000
         STH   R15,MYDFID                                               05860000
         LA    R15,MYDFID                                               05870000
         ST    R15,DFIDP                                                05880000
         SLR   R15,R15                                                  05890000
         ST    R15,DFCPPLP                                              05900000
         LINK  EP=IKJEFF18,SF=(E,LINKAREA)                              05910000
         L     R15,MYDFRC                                               05920000
         DROP  R1                                                       05930000
.SKIP3   ANOP                                                           05940000
         AIF   (&MVS).SKIP4                                             05950000
         LA    R1,MSGDAIR                                               05960000
         LA    R0,L'MSGDAIR                                             05970000
         SVC   93                 TPUT                                  05980000
.SKIP4   ANOP                                                           05990000
         L     R14,MYDFREGS                                             06000000
         BR    R14                                                      06010000
         SPACE                                                          06020000
************************************************************            06030000
*                                                          *            06040000
*        CLOSE THE DATASET                                 *            06050000
*                                                          *            06060000
************************************************************            06070000
         SPACE                                                          06080000
EXITC    MVC   CLOSED,CLOSE                                             06090000
         CLOSE ((R3)),MF=(E,CLOSED)                                     06100000
         SPACE                                                          06110000
************************************************************            06120000
*                                                          *            06130000
*        FINAL EXIT FROM PROGRAM                           *            06140000
*                                                          *            06150000
************************************************************            06160000
         SPACE                                                          06170000
EXITR    EQU   *                                                        06180000
         IKJRLSA MYANS                                                  06190000
         SPACE                                                          06200000
EXIT0    LH    R15,CC                                                   06210000
EXIT     LR    1,13                                                     06220000
         L     R0,@SIZE                                                 06230000
         L     13,4(,13)                                                06240000
         ST    15,16(,13)                                               06250000
         FREEMAIN R,LV=(0),A=(1)                                        06260000
         LM    14,12,12(13)                                             06270000
         BR    14                                                       06280000
         SPACE                                                          06290000
************************************************************            06300000
*                                                          *            06310000
*        CONSTANTS                                         *            06320000
*                                                          *            06330000
************************************************************            06340000
         SPACE                                                          06350000
@UNITVIO DC    CL8'SYSDA'                                               06360000
BLANKS   DC    CL10' '                                                  06370000
         LTORG                                                          06380000
         SPACE                                                          06390000
         PRINT NOGEN                                                    06400000
         SPACE                                                          06410000
DCBMODEL DCB   DDNAME=DYNAM,DSORG=PS,MACRF=(PM),                       X06420000
               RECFM=FB,LRECL=80,BLKSIZE=800                            06430000
DCBLEN   EQU   *-DCBMODEL                                               06440000
         SPACE                                                          06450000
         PRINT GEN                                                      06460000
         SPACE                                                          06470000
OPEN     OPEN  0,MF=L                                                   06480000
         SPACE                                                          06490000
CLOSE    CLOSE 0,MF=L                                                   06500000
         SPACE                                                          06510000
MODLPTPB PUTLINE OUTPUT=(1,TERM,SINGLE,INFOR),                         X06520000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),MF=L                  06530000
         SPACE                                                          06540000
MODLPGPB PUTGET OUTPUT=(1,SINGLE,MODE),                                X06550000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),                     X06560000
               TERMGET=(EDIT,WAIT),MF=L                                 06570000
         SPACE                                                          06580000
         AIF   (&MVS).SKIP5                                             06590000
MSGDAIR  DC    C'UNABLE TO ALLOCATE UTILITY DATA SET'                   06600000
.SKIP5   ANOP                                                           06610000
         PRINT NOGEN                                                    06620000
         SPACE                                                          06630000
INSPCL   IKJPARM                                                        06640000
PROMPT   IKJKEYWD                                                       06650000
         IKJNAME 'MODE',SUBFLD=RDYSUB                                   06660000
ENDKW    IKJKEYWD                                                       06670000
         IKJNAME 'END',SUBFLD=ENDSUB                                    06680000
INSERTKW IKJKEYWD                                                       06690001
         IKJNAME 'INSERT'                                               06700001
FILEKW   IKJKEYWD                                                       06710000
         IKJNAME 'FILENAME',SUBFLD=FILESF                               06720000
CONTKW   IKJKEYWD                                                       06730000
         IKJNAME 'CONTINUE'                                             06740000
BLOCKW   IKJKEYWD                                                       06750000
         IKJNAME 'BLKSIZE',SUBFLD=BLOCSF,ALIAS=('B','BL','BLOCKSIZE')   06760000
ASISKW   IKJKEYWD                                                       06770000
         IKJNAME 'ASIS'                                                 06780000
REUSKW   IKJKEYWD                                                       06790000
         IKJNAME 'REUSE'                                                06800000
RDYSUB   IKJSUBF                                                        06810000
RDY      IKJIDENT 'MODE STATEMENT',FIRST=ALPHANUM,OTHER=ALPHANUM,      X06820000
               MAXLNTH=8,PROMPT='MODE STATEMENT'                        06830000
ENDSUB   IKJSUBF                                                        06840000
END      IKJIDENT 'END STATEMENT',FIRST=ALPHA,OTHER=ALPHANUM,          X06850000
               MAXLNTH=8,PROMPT='END STATEMENT'                         06860000
FILESF   IKJSUBF                                                        06870000
FILES    IKJIDENT 'FILENAME ALLOCATED FOR USE BY INSTREAM COMMAND',    X06880000
               FIRST=ALPHANUM,OTHER=ALPHANUM,MAXLNTH=8                  06890000
BLOCSF   IKJSUBF                                                        06900000
BLOC     IKJIDENT 'BLOCK SIZE',FIRST=NUMERIC,OTHER=NUMERIC,            X06910000
               MAXLNTH=5,PROMPT='BLOCK SIZE'                            06920000
         IKJENDP                                                        06930000
         SPACE                                                          06940000
         PRINT GEN                                                      06950000
         SPACE                                                          06960000
R0       EQU   0                                                        06970000
R1       EQU   1                                                        06980000
R2       EQU   2                                                        06990000
R3       EQU   3                                                        07000000
R4       EQU   4                                                        07010000
R5       EQU   5                                                        07020000
R6       EQU   6                                                        07030000
R7       EQU   7                                                        07040000
R8       EQU   8                                                        07050000
R9       EQU   9                                                        07060000
R10      EQU   10                                                       07070000
R11      EQU   11                                                       07080000
R12      EQU   12                                                       07090000
R13      EQU   13                                                       07100000
R14      EQU   14                                                       07110000
R15      EQU   15                                                       07120000
         SPACE                                                          07130000
@DATA    DSECT                                                          07140000
         DS    18F                                                      07150000
*                                                                       07160000
*        BEGIN AREA REFERENCED BY STAX ATTN EXIT                        07170000
*                                                                       07180000
MYIOPL   DS    4F                                                       07190000
MYATTN   DS    A                                                        07200000
MYNEXBUF DS    A                                                        07210000
MYMODEMG DS    2A                                                       07220000
*                                                                       07230000
*        END OF AREA REFERENCED BY STAX ATTN EXIT                       07240000
*                                                                       07250000
DOUBLE   DS    D                                                        07260000
LINKAREA DS    2F                                                       07270000
OPTIONS  DS    F                                                        07280000
BLOKSIZE DS    H                                                        07290000
MYMODEMT DS    A,XL12                                                   07300000
MYDAPL   DS    5F                                                       07310000
MYDAPB   DS    21F                                                      07320000
MYECB    DS    F                                                        07330000
MYPPL    DS    7F                                                       07340000
MYANS    DS    F                                                        07350000
MYUWA    DS    F                                                        07360000
MYPTPB   DS    3F                                                       07370000
MYPGPB   DS    4F                                                       07380000
MYOLD    DS    2F                                                       07390000
MYSEG1   DS    2H,CL256                                                 07400000
DSNAMEL  DS    H                                                        07410000
DSNAME   DS    CL44                                                     07420000
MYFILEN  DS    CL8                                                      07430000
MYCPPL   DS    4F                                                       07440000
MYECBL   DS    2A                                                       07450000
MYUPT    DS    CL24                                                     07460000
MYDFPARM DS    5F  USED BY DAIRFAIL                                     07470000
MYDFREGS DS    F   USED BY DAIRFAIL                                     07480000
MYDFRC   DS    F   USED BY DAIRFAIL                                     07490000
MYJEFF02 DS    F   USED BY DAIRFAIL                                     07500000
MYDFID   DS    H   USED BY DAIRFAIL                                     07510000
OPEND    DS    F                                                        07520000
CLOSED   DS    F                                                        07530000
PUTLINS  DS    F                                                        07540000
ENDSTMT  DS    CL9                                                      07550000
INSERTSW DS    CL1                                                      07560002
CC       DS    H                                                        07570000
DCB      DS    0D,XL(DCBLEN)                                            07580000
DAIRREGS DS    F                                                        07590000
CARD     DS    CL80                                                     07600000
SPACES   DS    CL80                                                     07610000
@DATAL   EQU   *-@DATA                                                  07620000
         SPACE                                                          07630000
IHADCB   DSECT                                                          07640000
         DS    36X                                                      07650000
DCBRECFM DS    X                                                        07660000
DCBEXLST DS    AL3                                                      07670000
DCBDDNAM DS    CL8                                                      07680000
DCBOFLGS DS    X                                                        07690000
         DS    XL7                                                      07700000
DCBSYNAD DS    A                                                        07710000
         DS    XL2                                                      07720000
DCBBLKSI DS    H                                                        07730000
         SPACE                                                          07740000
CVT      DSECT                                                          07750000
CVTPTR   EQU   16                                                       07760000
CVTMAP   EQU   *                                                        07770000
         ORG   CVT+X'1E0'                                               07780000
CVTSCAN  DS    F                                                        07790000
         ORG   CVT+X'20C'                                               07800000
CVTPARS  DS    F                                                        07810000
         ORG   CVT+X'2DC'                                               07820000
CVTDAIR  DS    F                                                        07830000
         SPACE                                                          07840000
         IKJCPPL                                                        07850000
         SPACE                                                          07860000
         IKJPPL                                                         07870000
         SPACE                                                          07880000
         IKJUPT                                                         07890000
         SPACE                                                          07900000
         IKJIOPL                                                        07910000
         SPACE                                                          07920000
         IKJDAPL                                                        07930000
         SPACE                                                          07940000
         IKJDAP08                                                       07950000
         SPACE                                                          07960000
         IKJDAP18                                                       07970000
         SPACE                                                          07980000
         IKJPSCB                                                        07990000
         SPACE                                                          08000000
         AIF   (NOT &MVS).SKIP6                                         08010000
         IKJEFFDF DFDSECT=YES                                           08020000
.SKIP6   ANOP                                                           08030000
         END                                                            08040000
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR        <= TARGET LIBRARY     00120000
//LKED.SYSIN DD *                                                       00130000
 NAME INSTREAM(R)                                                       00140000
//*                                                                     00150000
//HELP    EXEC PGM=IEBUPDTE,PARM=NEW,COND=(0,NE)                        00160000
//SYSPRINT DD  SYSOUT=*                                                 00170000
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP             <= TARGET LIBRARY     00180000
//SYSIN    DD  *                                                        00190000
./ ADD NAME=INSTREAM                                                    00200000
)F FUNCTION -                                                           00010000
  THE INSTREAM COMMAND ALLOCATES A TEMPORARY DATA                       00020000
  SET FOR UTILITY CONTROL STATEMENTS, AND COPIES                        00030000
  SUBSEQUENT CLIST OR TERMINAL INPUT TO THAT DATA SET.                  00040000
                                                                        00050000
  THE COPYING CONTINUES UNTIL AN 'END' STATEMENT                        00060000
  IS ENCOUNTERED.  THIS IS USEFUL IN CLISTS THAT                        00070000
  CALL UTILITY PROGRAMS THAT REQUIRE 80-CHARACTER                       00080000
  CONTROL STATEMENT INPUT.  SINCE THE CONTROL STATEMENTS                00090000
  ARE PART OF THE CLIST, THEIR CONTENTS CAN BE SET BY                   00100000
  VARIABLES IN THE CLIST.                                               00110000
                                                                        00120000
  AN EXAMPLE OF A CLIST THAT USES INSTREAM                              00130000
     PROC 0 CC(A)                                                       00140000
     FREE FI(SYSIN)                                                     00150000
     INSTREAM                                                           00160000
     X PRINT PREFORM=&CC.                                               00170000
     END                                                                00180000
     CALL 'SYS1.LINKLIB(IEBPTPCH)'                                      00190000
                                                                        00200000
  NOTE - CLIST STATEMENTS THAT ARE INDENTED ARE                         00210000
  LEFT-JUSTIFIED AT THE TIME THEY ARE EXECUTED, SO                      00220000
  CONTROL STATEMENTS THAT REQUIRE LEADING BLANKS                        00230000
  REQUIRE SPECIAL HANDLING.  SEE OPERAND(NOTES).                        00240000
)X SYNTAX  -                                                            00250000
         INSTREAM   FILE('DDNAME')   END('STRING')  BLKSIZE('SIZE')     00260000
                    REUSE   CONTIN   INSERT                             00270000
  REQUIRED - NONE                                                       00280000
  DEFAULTS - FILE(SYSIN) END(END) BLKSIZE(400)                          00290000
  ALIAS    - NONE                                                       00300000
)O OPERANDS -                                                           00310000
))FILE('DDNAME') - THE DDNAME TO WHICH THE NEW TEMPORARY                00320000
             DATA SET IS TO BE ALLOCATED.  IF NOT SPECIFIED,            00330000
             FILE(SYSIN) IS USED.  THE DDNAME MUST NOT BE               00340000
             CURRENTLY ALLOCATED, UNLESS 'REUSE' IS SPECIFIED.          00350000
))REUSE    - THIS KEYWORD INDICATES THAT IF THE DDNAME IS               00360000
             CURRENTLY ALLOCATED, IT IS TO BE FREED AND THEN            00370000
             RE-ALLOCATED TO THE NEW TEMPORARY DATA SET.                00380000
))BLOCK('SIZE') - THE BLOCKSIZE OF THE TEMPORARY DATA SET.              00390000
             IF NOT SPECIFIED, 400 IS USED.                             00400000
))END('STRING') - THE STRING THAT WILL REPLACE 'END' AS THE             00410000
             DELIMITER OF THE INPUT DATA.                               00420000
))INSERT   - THIS KEYWORD SPECIFIES THAT IF AN INPUT LINE               00430000
             STARTS WITH THE CHARACTERS 'I' OR 'IN' OR 'INSERT'         00440000
             FOLLOWED BY A BLANK, ONLY THE DATA FOLLOWING THE           00450000
             BLANK WILL BE COPIED.  THIS ALLOWS YOU TO INDENT           00460000
             YOUR CLIST AND STILL CONTROL LEADING BLANKS IN YOUR        00470000
             INPUT DATA.                                                00480000
))CONTIN   - THIS KEYWORD INDICATES THAT THE FIRST BYTE OF DATA         00490000
             IN EACH INPUT LINE IS TO BE PUT IN COLUMN 72, AND          00500000
             BYTES 2 THRU 72 SHIFTED OVER TO 1 THRI 71.                 00510000
             USEFUL IF THE UTILITY PROGRAM USES COLUMN 72               00520000
             FOR CONTINUATION.                                          00530000
))NOTES    - THIS IS NOT AN OPERAND, BUT SOME AIDS TO USING             00540000
             THE INSTREAM COMMAND.                                      00550000
             ... LEADING BLANKS ...                                     00560000
             LEADING BLANKS ARE NORMALLY REMOVED FROM A                 00570000
             CLIST STATEMENT AT THE TIME IT IS EXECUTED.                00580000
             THIS CAN BE PREVENTED BY PLACING THE STATEMENT             00590000
             BETWEEN A 'DATA' AND 'ENDDATA' STATEMENT (SEE              00600000
             THE TSO COMMAND LANGUAGE REFERENCE MANUAL).                00610000
             THESE TWO STATEMENTS WILL BE INVISIBLE TO INSTREAM         00620000
             AND NOT COPIED TO THE TEMPORARY DATA SET.                  00630000
             ANOTHER WAY TO COPY A LEADING BLANK IS TO USE THE          00640000
             'INSERT' KEYWORD AND BEGIN EACH INPUT LINE WITH            00650000
             THE WORD 'INSERT' FOLLOWED BY A BLANK FOLLOWED BY          00660000
             YOUR LEADING BLANK(S).  THIS GIVES YOU THE FREEDOM         00670000
             TO CODE YOR CLIST WITH INDENTATION FOR READABILITY.        00680000
./ ENDUP                                                                00220000
//                                                                      00230000
