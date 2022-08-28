//SHOWSS JOB (JOB),
//             'INSTALL SHOWSS',
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
         TITLE '   S H O W S S     '                                    00010000
*********************************************************************** 00020000
*                                                                     * 00030000
*        'SHOWSS' TSO COMMAND                                         * 00040000
*                                                                     * 00050000
*********************************************************************** 00060000
         SPACE                                                          00070000
*  ATTRIBUTES. RE-ENTRANT.                                              00080000
*   IF 'ZERO' OPERAND IS TO BE USED, MUST BE AUTHORIZED.                00090000
*  DESCRIPTION.                                                         00100000
*   THIS TSO COMMAND DISPLAYS THE NAMES OF ALL SUBSYSTEMS IN THE        00110000
*   SYSTEM AND THE ADDRESSES OF EACH SUBSYSTEM'S SSCT ENTRY AND SSVT.   00120000
*   MAKE SURE YOU CHANGE TABLE AT LABEL "SYSP"                          00130000
*   OPTIONALLY, THE COMMAND WILL ZERO THE SSVT ADDRESS IN THE SSCT      00140000
*   FOR A SPECIFIED SUBSYSTEM, THUS DISCONNECTING THE SUBSYSTEM         00150000
*   FROM THE ACTIVE SUBSYSTEM INTERFACE.  THIS CAN BE USED TO GET       00160000
*   AROUND THE JES2 'SUBSYSTEM INTERFACE NOT DORMANT' MESSAGE.          00170000
*   THE SSVT IS NOT FREEMAINED.                                         00180000
*  LOG OF CHANGES.                                                      00190000
*   15JUN82 - PARSE ADDED.  ZERO KEYWORD ADDED.                         00200000
*   14APR12 - Authorization through RAC added.                  JW12105 00202000
*    On systems with resource access control active the SHOWSS  JW12105 00204000
*    command is executed only if the user has read access to    JW12105 00206000
*    profile SHOWAUTH in the FACILITY class.                    JW12105 00208000
         SPACE                                                          00210000
SHOWSS   START                                                          00220000
         USING *,R12                                                    00230000
         B     @PROLOG-*(,15)                                           00240000
         DC    AL1(11),CL11'SHOWSS '                                    00250000
         DC    CL16' &SYSDATE &SYSTIME '                                00260000
@SIZE    DC    0F'0',AL1(0),AL3(@DATAL)                                 00270000
@PROLOG  STM   14,12,12(R13)                                            00280000
         LR    R12,R15                                                  00290000
         LR    R2,R1                                                    00300000
         USING CPPL,R2                                                  00310000
         L     R0,@SIZE                                                 00320000
         GETMAIN R,LV=(0)                                               00330000
         LR    R9,R1                                                    00340000
         USING @DATA,R9                                                 00350000
         LR    R0,R1               AREA TO BE CLEARED                   00360000
         L     R1,@SIZE            LENGTH TO BE CLEARED                 00370000
         SR    R14,R14             ZERO FROM ADDRESS                    00380000
         SR    R15,R15             ZERO FROM LENGTH                     00390000
         MVCL  R0,R14              ZERO IT ALL                          00400000
         SPACE                                                          00410000
         ST    R13,4(,R9)                                               00420000
         ST    R9,8(,R13)                                               00430000
         LR    R13,R9                                                   00440000
         SPACE                                                          00450000
************************************************************            00460000
*                                                          *            00470000
*        CHECK USERID                                      *            00480000
*                                                          *            00490000
************************************************************            00500000
         SPACE                                                          00510000
         L     R1,548              PSAAOLD                              00520000
         L     R15,172(,R1)        ASCBJBNI                             00530000
         LTR   R15,R15             IS THIS A JOB                        00540000
         BNZ   IMPLEXEC            YES, QUIT                            00550000
         L     R15,60(,R1)         ASCBTSB                              00560000
         LTR   R15,R15             IS THIS A TSO SESSION                00570000
         BZ    PROCEED             NO, BRANCH IF STARTED TASK           00580000
AUTHID   L     R1,16               CVTPTR                               00590000
         L     R15,0(,R1)          TCB WORDS CVTTCBP                    00600000
         L     R15,4(,R15)         CURRENT TCB                          00610000
         L     R1,180(,R15)        TCBJSCB                              00620000
         L     R1,264(,R1)         JSCBPSCB                             00630000
         LTR   R3,R1               ANY PSCB? (save R1)          JW12105 00640000
         BZ    IMPLEXEC            NO - NOT A TSO SESSION               00650000
         L     R1,CVTPTR     get CVT address                    JW12105 00650700
         ICM   R1,B'1111',CVTSAF(R1) SAFV defined ?             JW12105 00651400
         BZ    NORAC         no RAC, try standard authorization JW12105 00652100
         USING SAFV,R1       addressability of SAFV             JW12105 00652800
         CLC   SAFVIDEN(4),SAFVID SAFV initialized ?            JW12105 00653500
         BNE   NORAC         no RAC, try standard authorization JW12105 00654200
         DROP  R1            SAFV no longer needed              JW12105 00654900
         RACHECK ENTITY=SHOWAUTH,CLASS='FACILITY',ATTR=READ     JW12105 00655600
         LTR   R15,R15       RAC authorization granted?         JW12105 00656300
         BNZ   IMPLEXEC      no, exit                           JW12105 00657000
         B     PROCEED       yes, go execute                    JW12105 00657700
NORAC    LR    R1,R3         restore R1                         JW12105 00658400
         USING PSCB,R1                                                  00660000
*        TM    PSCBATR1,PSCBCTRL   OPERATOR                             00670000
*        BZ    IMPLEXEC            NO - BRANCH                          00680000
         CLC   PSCBUSER(2),SYSP    SYSTEMS SUPPORT USERID               00690000
         BE    PROCEED             YES, BRANCH                          00700000
         LA    R15,USERIDS                                              00710000
AUTHLOOP CLI   0(R15),0            END OF LIST?                         00720000
         BE    IMPLEXEC            YES, NOT AUTHORIZED                  00730000
         CLC   PSCBUSER(7),0(R15)                                       00740000
         BE    PROCEED                                                  00750000
         LA    R15,8(,R15)         POINT TO NEXT USERID                 00760000
         B     AUTHLOOP            GO CHECK IT                          00770000
         DROP  R1                  PSCB                                 00780000
SYSP     DC    C'SY'               1ST 2 OF SYSTEMS TSOIDS              00790000
USERIDS  DC    0D'0'               ALIGN FOR EASY ZAPS                  00800000
         DC    8D'0'               ROOM FOR 8 USERIDS                   00810000
         DC    H'0'                END OF USERID LIST                   00820000
         SPACE                                                          00830000
IMPLEXEC EQU   *                                                        00840000
         L     R1,CPPLCBUF                                              00850000
         XC    2(2,R1),2(R1)       SET CBUF TO IMPLICIT EXEC            00860000
         L     R1,CPPLECT          GET ECT ADDRESS                      00870000
         USING ECT,R1                                                   00880000
         CLI   ECTSCMD,C' '        IS THIS A SUBCOMMAND                 00890000
         BNE   *+10                YES - SAY SUBCOMMAND NOT FOUND       00900000
         MVC   ECTPCMD,=CL8'EXEC'  NO  - SAY COMMAND NOT FOUND          00910000
         DROP  R1                  ECT                                  00920000
         LR    R1,R13                                                   00930000
         L     R0,@SIZE                                                 00940000
         L     R13,4(,R13)                                              00950000
         FREEMAIN R,A=(1),LV=(0)                                        00960000
         L     R1,24(,R13)         RESTORE CPPL POINTER                 00970000
         LA    R15,12(,R13)        POINT TO 2-WORD XCTL PARM            00980000
         XC    0(8,R15),0(R15)     CLEAR IT                             00990000
         XCTL  (2,12),EP=EXEC,SF=(E,(15))                               01000000
PROCEED  EQU   *                                                        01010000
         SPACE 1                                                        01020000
************************************************************            01030000
*                                                          *            01040000
*        SET UP IOPL FOR PUTLINE                           *            01050000
*                                                          *            01060000
************************************************************            01070000
         SPACE                                                          01080000
         LA    R15,MYIOPL                                               01090000
         USING IOPL,R15                                                 01100000
         MVC   IOPLUPT(4),CPPLUPT                                       01110000
         MVC   IOPLECT(4),CPPLECT                                       01120000
         LA    R0,MYECB                                                 01130000
         ST    R0,IOPLECB                                               01140000
         XC    MYECB,MYECB                                              01150000
         LA    R0,MYPTPB                                                01160000
         ST    R0,IOPLIOPB                                              01170000
         DROP  R15                 IOPL                                 01180000
         SPACE                                                          01190000
         L     R15,16              LOAD CVT POINTER                     01200000
         TM    444(R15),X'80'      IS PUTLINE LOADED? (VS2)             01210000
         BNO   PUTLOAD             NO - BRANCH TO LOAD                  01220000
         L     R15,444(,R15)       YES - USE CVTPUTL                    01230000
         B     PUTLODED            BRANCH AROUND LOAD                   01240000
PUTLOAD  LA    R0,=CL8'IKJPUTL '                                        01250000
         LOAD  EPLOC=(0)                                                01260000
         LR    R15,R0              GET ENTRY ADDRESS                    01270000
         LA    R15,0(,R15)         CLEAR HI BYTE FOR DELETE ROUTINE     01280000
PUTLODED ST    R15,MYPUTLEP        SAVE PUTLINE ENTRY ADDRESS           01290000
         SPACE                                                          01300000
************************************************************            01310000
*                                                          *            01320000
*        SET UP PPL FOR PARSE                              *            01330000
*                                                          *            01340000
************************************************************            01350000
         SPACE                                                          01360000
         LA    R15,MYPPL                                                01370000
         USING PPL,R15                                                  01380000
         MVC   PPLUPT(4),CPPLUPT                                        01390000
         MVC   PPLECT(4),CPPLECT                                        01400000
         LA    R0,MYECB                                                 01410000
         ST    R0,PPLECB                                                01420000
         XC    MYECB,MYECB                                              01430000
         L     R0,=A(SSPCL)                                             01440000
         ST    R0,PPLPCL                                                01450000
         LA    R0,MYANS                                                 01460000
         ST    R0,PPLANS                                                01470000
         MVC   PPLCBUF(4),CPPLCBUF                                      01480000
         ST    R9,PPLUWA                                                01490000
         DROP  R15                 PPL                                  01500000
         SPACE 1                                                        01510000
************************************************************            01520000
*                                                          *            01530000
*        CALL THE PARSE SERVICE ROUTINE                    *            01540000
*                                                          *            01550000
************************************************************            01560000
         SPACE 1                                                        01570000
         LR    R1,R15              POINT TO PPL                         01580000
         L     R15,16              CVTPTR                               01590000
         TM    524(R15),X'80'      IF HI ORDER BIT NOT ON               01600000
         BNO   PARSELNK               THEN DO LINK, NOT CALL            01610000
         L     R15,524(,R15)       CVTPARS                              01620000
         BALR  R14,R15             CALL IKJPARS                         01630000
         B     PARSEEXT            SKIP AROUND LINK                     01640000
PARSELNK EQU   *                                                        01650000
         LINK  EP=IKJPARS,SF=(E,LINKAREA)                               01660000
PARSEEXT EQU   *                                                        01670000
         SPACE 1                                                        01680000
         LTR   R15,R15             PARSE SUCCESSFUL?                    01690000
         BZ    PARSEOK             YES, BRANCH                          01700000
         LA    R1,MSG01                                                 01710000
         LA    R0,L'MSG01                                               01720000
         BAL   R14,PUTMSG                                               01730000
         B     EXIT12                                                   01740000
PARSEOK  EQU   *                                                        01750000
         L     R3,MYANS                                                 01760000
         USING IKJPARMD,R3                                              01770000
         SPACE 1                                                        01780000
************************************************************            01790000
*                                                          *            01800000
*        IF ZERO(NAME) IS SPECIFIED, GET THE NAME          *            01810000
*                                                          *            01820000
************************************************************            01830000
         SPACE                                                          01840000
         MVC   ZNAM,=CL4' '                                             01850000
         TM    ZERO+6,X'80'                                             01860000
         BZ    NAMEX                                                    01870000
         L     R15,ZERO                                                 01880000
         LH    R1,ZERO+4                                                01890000
         BCTR  R1,0                                                     01900000
         B     *+10                                                     01910000
         MVC   ZNAM(0),0(R15)                                           01920000
         EX    R1,*-6                                                   01930000
NAMEX    EQU   *                                                        01940000
         SPACE 1                                                        01950000
************************************************************            01960000
*                                                          *            01970000
*        DISPLAY THE SUBSYSTEMS                            *            01980000
*                                                          *            01990000
************************************************************            02000000
         SPACE                                                          02010000
         L     R1,16               CVTPTR                               02020000
         L     R4,X'128'(,R1)      CVTJESCT                             02030000
         USING JESCT,R4                                                 02040000
         LA    R4,JESSSCT-(SSCTSCTA-SSCT) POINT TO SSCT HEAD            02050000
         USING SSCT,R4                                                  02060000
NEXTSSCT EQU   *                                                        02070000
         ICM   R4,15,SSCTSCTA      POINT TO NEXT SSCT                   02080000
         BZ    DONESSCT            IF END, EXIT                         02090000
         MVI   LINE,C' '                                                02100000
         MVC   LINE+1(L'LINE-1),LINE                                    02110000
         MVC   LINE(4),SSCTSNAM    SUBSYSTEM NAME                       02120000
         MVC   LINE+4(26),=C'  SSCT XXXXXX  SSVT XXXXXX'                02130000
         ST    R4,FULL             SAVE SSCT ADDRESS                    02140000
         SPACE                                                          02150000
         LA    R1,FULL+1                                                02160000
         LA    R15,LINE+11                                              02170000
         LA    R0,3                                                     02180000
         BAL   R14,UNPACK                                               02190000
         SPACE                                                          02200000
         LA    R1,SSCTSSVT+1                                            02210000
         LA    R15,LINE+24                                              02220000
         LA    R0,3                                                     02230000
         BAL   R14,UNPACK                                               02240000
         SPACE                                                          02250000
         L     R1,SSCTSSVT                                              02260000
         LTR   R1,R1                                                    02270000
         BNZ   *+10                                                     02280000
         MVC   LINE+32(9),=C'(DORMANT)'                                 02290000
         SPACE                                                          02300000
         LA    R1,LINE                                                  02310000
         LA    R0,50                                                    02320000
         BAL   R14,PUTLINE                                              02330000
         SPACE                                                          02340000
         CLI   ZNAM,C' '           IS ANY SUBSYSTEM TO BE UNPLUGGED     02350000
         BE    ZERO9               NO                                   02360000
         CLC   SSCTSNAM,ZNAM       IS THIS SUBSYSTEM TO BE UNPLUGGED    02370000
         BNE   ZERO9               NO                                   02380000
         SPACE                                                          02390000
************************************************************            02400000
*                                                          *            02410000
*        ZERO THE SSVT ADDRESS IF REQUESTED                *            02420000
*                                                          *            02430000
************************************************************            02440000
         SPACE                                                          02450000
         L     R15,SSCTSSVT        GET ADDRESS TO BE ZEROED             02460000
         LTR   R15,R15             IS IT ALREADY ZERO                   02470000
         BZ    ZALREADY            YES, BRANCH                          02480000
         SPACE                                                          02490000
         TESTAUTH FCTN=1                                                02500000
         LTR   R15,R15                                                  02510000
         BNZ   ZERO9                                                    02520000
         SPACE                                                          02530000
         CLI   WTORKW+1,2                                               02540000
         BE    WTORX                                                    02550000
         MVC   WTORW(CONFIRML),CONFIRM                                  02560000
         MVC   WTORW+12+30,SSCTSNAM                                     02570000
         SPACE                                                          02580000
ZWTOR    XC    ECB,ECB                                                  02590000
         MVI   REPLY,C' '                                               02600000
         WTOR  ,REPLY,,ECB,MF=(E,WTORW)                                 02610000
         SPACE                                                          02620000
         WAIT  ECB=ECB                                                  02630000
         SPACE                                                          02640000
         OI    REPLY,X'40'         CAPS                                 02650000
         CLI   REPLY,C'N'                                               02660000
         BE    ZERO9                                                    02670000
         CLI   REPLY,C'Y'                                               02680000
         BNE   ZWTOR                                                    02690000
WTORX    EQU   *                                                        02700000
         SPACE                                                          02710000
         LA R0,0                                                        02720000
         LA R1,1                                                        02730000
         SVC 244                                                        02740000
         MODESET KEY=ZERO                                               02750000
         SPACE                                                          02760000
         XC    SSCTSSVT,SSCTSSVT   UNPLUG THE SUBSYSTEM                 02770000
         SPACE                                                          02780000
         MODESET KEY=NZERO                                              02790000
         LA R1,0                                                        02800000
         SVC 244                                                        02810000
         SPACE                                                          02820000
ZERO9    B     NEXTSSCT                                                 02830000
         SPACE                                                          02840000
ZALREADY LA    R1,ALREADY                                               02850000
         LA    R0,L'ALREADY                                             02860000
         BAL   R14,PUTLINE                                              02870000
         B     ZERO9                                                    02880000
         SPACE                                                          02890000
************************************************************            02900000
*                                                          *            02910000
*        SUBROUTINES                                       *            02920000
*                                                          *            02930000
************************************************************            02940000
         SPACE                                                          02950000
UNPACK   MVC   1(1,R15),0(R1)      MOVE BYTE                            02960000
         UNPK  0(3,R15),1(2,R15)   UNPACK                               02970000
         TR    0(2,R15),UNPACKT-240                                     02980000
         LA    R15,2(,R15)         INCREMENT OUTPUT PTR                 02990000
         LA    R1,1(,R1)           INCREMENT INPUT PTR                  03000000
         BCT   R0,UNPACK           DECREMENT LENGTH, THEN LOOP          03010000
         MVI   0(R15),C' '         BLANK THE TRAILING BYTE              03020000
         BR    R14                 RETURN TO CALLER                     03030000
         SPACE                                                          03040000
************************************************************            03050000
*                                                          *            03060000
*        PUTMSG ROUTINE                                    *            03070000
*                                                          *            03080000
************************************************************            03090000
         SPACE                                                          03100000
PUTMSG   STM   R14,R1,PUTLINS                                           03110000
         XC    MYOLD(8),MYOLD                                           03120000
         XC    MYSEG1(4),MYSEG1                                         03130000
         MVC   MYPTPB(12),MODLPTPM                                      03140000
         LA    R14,1               NO. OF MESSAGE SEGMENTS              03150000
         ST    R14,MYOLD                                                03160000
         LA    R14,MYSEG1          POINT TO 1ST SEGMENT                 03170000
         ST    R14,MYOLD+4                                              03180000
         LR    R14,R0              LENGTH IN R0                         03190000
         LA    R14,4(,R14)         ADD 4                                03200000
         LA    R15,MYSEG1+4                                             03210000
         CLC   0(3,R1),=C'IKJ'     IS DATA PRECEEDED BY MESSAGE ID?     03220000
         BE    *+16                YES - BRANCH                         03230000
         LA    R14,1(,R14)         ADD 1 TO LENGTH                      03240000
         MVI   0(R15),C' '         INSERT LEADING BLANK                 03250000
         LA    R15,1(,R15)         BUMP POINTER                         03260000
         STH   R14,MYSEG1                                               03270000
         LR    R14,R0                                                   03280000
         BCTR  R14,0                                                    03290000
         B     *+10                                                     03300000
         MVC   0(0,R15),0(R1)      MOVE MESSAGE IN                      03310000
         EX    R14,*-6                                                  03320000
         LA    R1,MYIOPL                                                03330000
         L     R15,MYPUTLEP                                             03340000
         SPACE                                                          03350000
         PUTLINE PARM=MYPTPB,OUTPUT=(MYOLD),ENTRY=(15),MF=(E,(1))       03360000
         SPACE                                                          03370000
         LM    R14,R1,PUTLINS                                           03380000
         BR    R14                                                      03390000
         SPACE                                                          03400000
************************************************************            03410000
*                                                          *            03420000
*        PUTLINE ROUTINE                                   *            03430000
*                                                          *            03440000
************************************************************            03450000
         SPACE                                                          03460000
PUTLINE  STM   R14,R1,PUTLINS                                           03470000
         XC    MYSEG1(4),MYSEG1                                         03480000
         MVC   MYPTPB(12),MODLPTPB                                      03490000
         LR    R14,R0              LENGTH IN R0                         03500000
         LA    R14,4(,R14)         ADD 4                                03510000
         STH   R14,MYSEG1                                               03520000
         LR    R14,R0                                                   03530000
         BCTR  R14,0                                                    03540000
         B     *+10                                                     03550000
         MVC   MYSEG1+4(0),0(R1)   MOVE TEXT IN                         03560000
         EX    R14,*-6                                                  03570000
         LA    R1,MYIOPL                                                03580000
         L     R15,MYPUTLEP                                             03590000
         SPACE                                                          03600000
         PUTLINE PARM=MYPTPB,OUTPUT=(MYSEG1,DATA),ENTRY=(15),MF=(E,(1)) 03610000
         SPACE                                                          03620000
         LM    R14,R1,PUTLINS                                           03630000
         BR    R14                                                      03640000
         SPACE                                                          03650000
************************************************************            03660000
*                                                          *            03670000
*        TERMINATION                                       *            03680000
*                                                          *            03690000
************************************************************            03700000
         SPACE                                                          03710000
DONESSCT EQU   *                                                        03720000
         SPACE 1                                                        03730000
EXIT0    SR    15,15                                                    03740000
         B     EXIT                                                     03750000
EXIT12   LA    R15,12                                                   03760000
EXIT     LR    R1,R13                                                   03770000
         L     R0,@SIZE                                                 03780000
         L     R13,4(,R13)                                              03790000
         LR    R2,R15                                                   03800000
         FREEMAIN R,A=(1),LV=(0)                                        03810000
         LR    R15,R2                                                   03820000
         LM    0,12,20(R13)                                             03830000
         L     R14,12(,R13)                                             03840000
         BR    R14                                                      03850000
         SPACE                                                          03860000
************************************************************            03870000
*                                                          *            03880000
*        CONSTANTS                                         *            03890000
*                                                          *            03900000
************************************************************            03910000
         SPACE                                                          03920000
         LTORG                                                          03930000
         SPACE                                                          03940000
MODLPTPM PUTLINE OUTPUT=(1,TERM,SINGLE,INFOR),                         X03950000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),MF=L                  03960000
         SPACE                                                          03970000
MODLPTPB PUTLINE OUTPUT=(1,TERM,SINGLE,DATA),                          X03980000
               TERMPUT=(EDIT,WAIT,NOHOLD,NOBREAK),MF=L                  03990000
         SPACE                                                          04000000
UNPACKT  DC    C'0123456789ABCDEF' TRANSLATE TABLE                      04010000
         SPACE                                                          04020000
CONFIRM  WTOR  'CONFIRM REQUEST TO DISCONNECT XXXX SUBSYSTEM (Y OR N)',X04030000
               VREPLY,3,VECB,ROUTCDE=(2),MF=L                           04040000
CONFIRML EQU   *-CONFIRM                                                04050000
VREPLY   EQU   0                                                        04060000
VECB     EQU   0                                                        04070000
         SPACE                                                          04080000
MSG01    DC    C'ERROR IN PARSE'                                        04090000
AUTHMSG  DC    C'ENVIRONMENT IS NOT APF AUTHORIZED - ZERO IGNORED'      04100000
ALREADY  DC    C'SSCTSSVT IS ALREADY ZERO'                              04110000
         DC    0D'0'                                                    04120000
SHOWAUTH DC    CL39'SHOWAUTH' facility name to authorize        JW12105 04123000
SAFVID   DC    CL4'SAFV'     SAFV eye catcher                   JW12105 04126000
         SPACE                                                          04130000
************************************************************            04140000
*                                                          *            04150000
*        PARSE PCL                                         *            04160000
*                                                          *            04170000
************************************************************            04180000
         SPACE                                                          04190000
         PRINT NOGEN                                                    04200000
SSPCL    IKJPARM                                                        04210000
ZEROKW   IKJKEYWD                                                       04220000
         IKJNAME 'ZERO',SUBFLD=ZEROSF                                   04230000
WTORKW   IKJKEYWD                                                       04240000
         IKJNAME 'WTOR'                                                 04250000
         IKJNAME 'NOWTOR'                                               04260000
ZEROSF   IKJSUBF                                                        04270000
ZERO     IKJIDENT 'SUBSYSTEM NAME',                                    X04280000
               FIRST=ALPHA,OTHER=ALPHANUM,MAXLNTH=4,                   X04290000
               PROMPT='NAME OF SUBSYSTEM TO BE DISCONNECTED'            04300000
         IKJENDP                                                        04310000
         PRINT GEN                                                      04320000
         SPACE                                                          04330000
************************************************************            04340000
*                                                          *            04350000
*        DSECTS                                            *            04360000
*                                                          *            04370000
************************************************************            04380000
         SPACE                                                          04390000
@DATA    DSECT                                                          04400000
         DS    18F                 REGISTER SAVEAREA                    04410000
DOUBLE   DS    D                                                        04420000
LINKAREA DS    2F                                                       04430000
FULL     DS    F                                                        04440000
LINE     DS    CL50                                                     04450000
MYECB    DS    F                  USED BY PUTLINE AND PARSE             04460000
MYIOPL   DS    4F                 USED BY PUTLINE ROUTINE               04470000
MYPPL    DS    7F                 USED BY PARSE                         04480000
MYANS    DS    F                  USED BY PARSE                         04490000
MYPTPB   DS    3F                 USED BY PUTLINE ROUTINE               04500000
MYPUTLEP DS    F                  USED BY PUTLINE ROUTINE               04510000
MYOLD    DS    2F                 USED BY PUTLINE ROUTINE               04520000
MYSEG1   DS    2H,CL100           USED BY PUTLINE ROUTINE               04530000
PUTLINS  DS    4F                 USED BY PUTLINE ROUTINE               04540000
ZNAM     DS    CL4                                                      04550000
ECB      DS    F                                                        04560000
REPLY    DS    F                                                        04570000
WTORW    DS    0F,CL80                                                  04580000
         DS    0D                                                       04590000
@DATAL   EQU   *-@DATA                                                  04600000
         SPACE ,                                                JW12105 04601000
         CVT   DSECT=YES                                        JW12105 04602000
         SPACE ,                                                JW12105 04603000
         IHAPSA ,                                               JW12105 04604000
         SPACE ,                                                JW12105 04605000
CVTSAF   EQU   248 CVTSAF doesn't exist but is a reserved field in 3.8J 04606000
         ICHSAFV  DSECT=YES  map SAFV                           JW12105 04607000
         SPACE                                                          04610000
         IKJCPPL                                                        04620000
         SPACE                                                          04630000
         IKJIOPL                                                        04640000
         SPACE                                                          04650000
         IKJPPL                                                         04660000
         SPACE                                                          04670000
         IKJPSCB                                                        04680000
         SPACE                                                          04690000
         IKJECT                                                         04700000
         SPACE                                                          04710000
         IEFJESCT                                                       04720000
         SPACE                                                          04730000
         IEFJSCVT                                                       04740000
         SPACE                                                          04750000
         IEFJSSVT                                                       04760000
         SPACE                                                          04770000
R0       EQU   0                                                        04780000
R1       EQU   1                                                        04790000
R2       EQU   2                                                        04800000
R3       EQU   3                                                        04810000
R4       EQU   4                                                        04820000
R5       EQU   5                                                        04830000
R6       EQU   6                                                        04840000
R7       EQU   7                                                        04850000
R8       EQU   8                                                        04860000
R9       EQU   9                                                        04870000
R10      EQU   10                                                       04880000
R11      EQU   11                                                       04890000
R12      EQU   12                                                       04900000
R13      EQU   13                                                       04910000
R14      EQU   14                                                       04920000
R15      EQU   15                                                       04930000
         END                                                            04940000
@@
//SYSUT2   DD  DISP=(,PASS),UNIT=VIO,
//             SPACE=(CYL,(1,1)),DCB=(LRECL=80,RECFM=FB,BLKSIZE=3120)
//SYSIN    DD  DUMMY
//SYSPRINT DD  SYSOUT=*   
//ASMCL   EXEC ASMFCL,
//             PARM.ASM=(OBJ,NODECK),
//             PARM.LKED='LIST,MAP,RENT,REUS,REFR,AC=1',
//        MAC1='SYS1.AMODGEN',
//        MAC2='SYS2.MACLIB'
//ASM.SYSIN    DD DISP=(OLD,DELETE),DSN=*.RACUPDT.SYSUT2
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB(SHOWSS),DISP=SHR
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
 RX RDEFINE 'FACILITY SHOWAUTH UACC(NONE)'
 RX PERMIT 'SHOWAUTH CLASS(FACILITY) ID(ADMIN) ACCESS(READ)'
//STDOUT   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDERR   DD   SYSOUT=*,DCB=(RECFM=FB,LRECL=140,BLKSIZE=5600)
//STDIN    DD   DUMMY   
